MODULE ob;

(* Hosted Oberon build command                                                *)
(*                                                                            *)
(* Given a single module to build, works out what to compile to satisfy its   *)
(* imports, compiles them, and combines all objects into a single executable. *)


IMPORT
  SYSTEM, H := Winshim, K := Kernel, Files, Texts, Oberon, ORS, X64, ORG, ORP, WinPE, WinArgs;

TYPE
  ModuleName = ARRAY 1024 OF CHAR;
  PathName   = ARRAY H.MaxPath OF CHAR;

  Module     = POINTER TO ModuleDesc;
  Dependency = POINTER TO DependencyDesc;

  ModuleDesc = RECORD
    next:         Module;
    modname:      ModuleName;
    filename:     PathName;
    codename:     PathName;
    text:         Texts.Text;
    dependencies: Dependency;
    scanned:      BOOLEAN
  END;

  DependencyDesc = RECORD
    next: Dependency;
    mod:  Module
  END;

VAR
  Modulename:      ModuleName;
  SourcePath:      PathName;
  BuildPath:       PathName;
  Verbose:         BOOLEAN;
  Modules:         Module;
  LongestModname:  INTEGER;
  LongestFilename: INTEGER;
  LoadFlags:       SET;



(* -------------------------------------------------------------------------- *)
(* ---------------- File open with ';' delimited searchpath ----------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE FindFile(    name:       ARRAY OF CHAR;
                       searchpath: ARRAY OF CHAR;
                   VAR file:       Files.File;
                   VAR filename:   PathName);
VAR
  path: PathName;
  i, j: INTEGER;
BEGIN
  i := 0;  file := NIL;
  WHILE (file = NIL) & (searchpath[i] # 0X) DO
    j := 0;
    WHILE (i < LEN(searchpath)) & (searchpath[i] # 0X) & (searchpath[i] # ";") DO
      path[j] := searchpath[i];  INC(i);  INC(j);
    END;
    IF (path[j-1] # "/") & (path[j-1] # 5CX) THEN path[j] := 5CX; INC(j) END;  (* Switch / to \ *)
    path[j] := 0X;  H.Append(name, path);
    file := Files.Old(path);
    WHILE searchpath[i] = ";" DO INC(i) END;
  END;
  IF file # NIL THEN
    filename := path;
    (*H.ws("Found "); H.ws(name); H.ws(" at "); H.ws(path); H.wsn(".")*)
  END
END FindFile;


(* -------------------------------------------------------------------------- *)
(* ------------------ Scan module recording dependencies -------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE AddModule(modname: ARRAY OF CHAR);
VAR mod: Module;  file: Files.File;  filename: ModuleName;
BEGIN NEW(mod);
  mod.modname := modname;  mod.scanned := FALSE;
  filename := modname;  H.Append(".mod", filename);
  FindFile(filename, SourcePath, file, mod.filename);
  IF file = NIL THEN
    H.ws("Could not find source file "); H.ws(filename); H.ws(" for module '"); H.ws(modname); H.wsn("'.");
    H.ExitProcess(99)
  END;
  mod.codename := "";  H.Append(modname, mod.codename);  H.Append(".code", mod.codename);
  IF H.Length(modname)      > LongestModname  THEN LongestModname  := H.Length(modname) END;
  IF H.Length(mod.filename) > LongestFilename THEN LongestFilename := H.Length(mod.filename) END;
  mod.next := Modules;  Modules := mod
END AddModule;

PROCEDURE AddImport(from: Module; modname: ARRAY OF CHAR);
VAR dependentModule: Module;  dependency: Dependency;
BEGIN
  dependentModule := Modules;
  WHILE (dependentModule # NIL) & (dependentModule.modname # modname) DO
    dependentModule := dependentModule.next
  END;
  IF dependentModule = NIL THEN AddModule(modname);  dependentModule := Modules END;
  NEW(dependency);  dependency.mod := dependentModule;
  dependency.next := from.dependencies;  from.dependencies := dependency;
END AddImport;


PROCEDURE WriteFilepos;
BEGIN
  H.wc("["); H.wi(ORS.linenum); H.wc(":"); H.wi(ORS.Pos() - ORS.linebeg); H.wc("]")
END WriteFilepos;

PROCEDURE Expected(filename, message: ARRAY OF CHAR);
BEGIN
  H.ws("File '"); H.ws(filename); WriteFilepos; H.ws("': "); H.wsn(message); H.ExitProcess(99)
END Expected;


PROCEDURE ScanModuleFileImports(module: Module);
VAR
  sym:     INTEGER;
  impname: ORS.Ident;
BEGIN
  NEW(module.text);
  Texts.Open(module.text, module.filename);
  ORS.Init(module.text, 0);  ORS.Get(sym);
  IF sym # ORS.module THEN Expected(module.filename, "does not start with MODULE.") END;
  ORS.Get(sym);
  IF sym # ORS.ident THEN Expected(module.filename, "expected module id."); END;
  IF ORS.id # module.modname THEN
    H.ws("File "); H.ws(module.filename); WriteFilepos; H.ws(" module id '");
    H.ws(ORS.id); H.ws("' does not match expected id '");
    H.ws(module.modname); H.wsn("'.");
    H.ExitProcess(99)
  END;
  (*B.Init(S.id);*)
  ORS.Get(sym);
  IF sym # ORS.semicolon THEN Expected(module.filename, "expected semicolon from module id.") END;
  ORS.Get(sym);
  IF sym = ORS.import THEN
    REPEAT
      ORS.Get(sym);
      IF sym # ORS.ident THEN Expected(module.filename, "expected id (1).") END;
      impname := ORS.id;  ORS.Get(sym);
      IF sym = ORS.becomes THEN
        ORS.Get(sym);
        IF sym # ORS.ident THEN Expected(module.filename, "expected id (2).") END;
        impname := ORS.id;  ORS.Get(sym)
      END;
      IF (impname # "SYSTEM") & (impname # "Winshim") & (impname # "Kernel") THEN
        AddImport(module, impname)
      END
    UNTIL sym # ORS.comma
  END;

  (* All modules need Winshim, excepting Winshim itself *)
  IF module.modname # "Winshim" THEN
    AddImport(module, "Winshim");
    IF module.modname # "Kernel" THEN
      AddImport(module, "Kernel")
    END
  END;

  module.scanned := TRUE
END ScanModuleFileImports;


(* -------------------------------------------------------------------------- *)
(* ----------------------------- Compile module ----------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE WriteHuman(n, w: INTEGER);
BEGIN
  IF n < 1000 THEN
    H.wir(n, w)
  ELSE
    WriteHuman(n DIV 1000, w-4);  H.wc(",");  H.wiz(n MOD 1000, 3)
  END
END WriteHuman;

PROCEDURE Compile(module: Module);
VAR startTime, endTime: INTEGER;
BEGIN
  H.wsl(module.modname,  LongestModname  + 2);
  H.wsl(module.filename, LongestFilename);
  (*
  B.SetSourcePath(SourcePath);
  B.SetBuildPath(BuildPath);
  *)

  startTime := H.Time();
  ORS.Init(module.text, 0);  ORP.Module(module.filename);

  IF ORS.errcnt = 0 THEN
    endTime := H.Time();
    WriteHuman(X64.PC, 12);   WriteHuman(ORG.Varsize, 12);
    WriteHuman(endTime - startTime, 6);    H.ws(" ms");

    (*H.ws("  allocated: "); WriteHuman(K.Allocated, 1);*)

    H.wn
  END
END Compile;


(* -------------------------------------------------------------------------- *)
(* ------------------------ Build all needed modules ------------------------ *)
(* -------------------------------------------------------------------------- *)

PROCEDURE RemoveDependencies(remove: Module);
(* Remove module 'remove' from all dependency lists in 'Modules'. *)
VAR mod: Module;  dep, prev: Dependency;
BEGIN
  (*H.ws("RemoveDependencies("); H.ws(remove.modname); H.wsn(")");*)
  mod := Modules;
  WHILE mod # NIL DO
    dep := mod.dependencies;  prev := NIL;
    WHILE dep # NIL DO
      IF dep.mod = remove THEN
        IF prev = NIL THEN
          mod.dependencies := dep.next;  dep := dep.next
        ELSE
          prev.next := dep.next;  dep := dep.next
        END
      ELSE
        prev := dep;  dep := dep.next
      END
    END;
    mod := mod.next
  END
END RemoveDependencies;


PROCEDURE ReportDependencies;
VAR mod: Module;  dep: Dependency;
BEGIN
  mod := Modules;
  WHILE mod # NIL DO
    H.ws("Module "); H.wsl(mod.modname, LongestModname);
    dep := mod.dependencies;
    IF dep = NIL THEN
      H.wsn(" (none).")
    ELSE
      WHILE dep # NIL DO H.wc(" ");  H.ws(dep.mod.modname);  dep := dep.next END;
      H.wsn(".")
    END;
    mod := mod.next
  END
END ReportDependencies;


(*
PROCEDURE Build();
VAR
  mod, prev:  Module;
  allscanned: BOOLEAN;
  PEname:     PathName;
  codesize:   INTEGER;
  varsize:    INTEGER;
  start, end: INTEGER;  (* Times *)

BEGIN
  AddModule(Modulename);

  IF SourcePath # "./" THEN H.ws(", source path: '");  H.ws(SourcePath); H.wsn("'") END;
  IF BuildPath  # ""   THEN H.ws(", build path: '");   H.ws(BuildPath);  H.wsn("'") END;

  (* Keep scanning and adding modules until all dependencies have been scanned *)
  REPEAT
    mod := Modules;  allscanned := TRUE;
    WHILE mod # NIL DO
      IF ~mod.scanned THEN
        allscanned := FALSE;  ScanModuleFileImports(mod)
      END;
      mod := mod.next
    END;
  UNTIL allscanned;

  IF Verbose THEN H.wsn("Modules and dependencies:");  ReportDependencies END;

  (* Compile dependentless modules until all modules compiled. *)
  H.wsl("Module", LongestModname + 2);  H.wsl("File", LongestFilename);
  H.wsn("        code         VAR     time");
  codesize := 0;  varsize := 0;
  start := H.Time();

  REPEAT
    mod := Modules;  prev := NIL;
    WHILE (mod # NIL) & (mod.dependencies # NIL) DO
      prev := mod;  mod := mod.next
    END;
    IF mod # NIL THEN
      Compile(mod);
      IF ORS.errcnt # 0 THEN H.ExitProcess(99) END;

      IF mod.modname # "Winshim" THEN WinPE.AddModule(mod.codename) END;

      INC(codesize, X64.PC);  INC(varsize, ORG.Varsize);
      RemoveDependencies(mod);
      IF prev = NIL THEN Modules := mod.next ELSE prev.next := mod.next END
    ELSE
      H.wsn("Cannot resolve circular dependency order in:");
      ReportDependencies; H.ExitProcess(99)
    END
  UNTIL Modules = NIL;

  PEname := ""; H.Append(Modulename, PEname);  H.Append(".exe", PEname);
  WinPE.Generate(PEname, LoadFlags);

  end := H.Time();
  H.wsl("Total", LongestModname + LongestFilename + 2);
  WriteHuman(codesize, 12);     WriteHuman(varsize,  12);
  WriteHuman(end - start, 6);   H.wsn(" ms")
END Build;
*)


PROCEDURE SortModulesIntoBuildOrder;
VAR
  mod, prev:  Module;
  allscanned: BOOLEAN;
  buildfirst: Module;
  buildlast:  Module;
BEGIN
  AddModule(Modulename);

  IF SourcePath # "./" THEN H.ws(", source path: '");  H.ws(SourcePath); H.wsn("'") END;
  IF BuildPath  # ""   THEN H.ws(", build path: '");   H.ws(BuildPath);  H.wsn("'") END;

  (* Keep scanning and adding modules until all dependencies have been scanned *)
  REPEAT
    mod := Modules;  allscanned := TRUE;
    WHILE mod # NIL DO
      IF ~mod.scanned THEN
        allscanned := FALSE;  ScanModuleFileImports(mod)
      END;
      mod := mod.next
    END;
  UNTIL allscanned;

  IF Verbose THEN H.wsn("Modules and dependencies:");  ReportDependencies END;

  (* Find and move dependentless modules to build list until full build order determined. *)
  buildfirst := NIL;  buildlast := NIL;
  WHILE Modules # NIL DO
    prev := NIL;  mod := Modules;
    (* Find first module with no dependencies *)
    WHILE (mod # NIL) & (mod.dependencies # NIL) DO
      prev := mod; mod := mod.next
    END;
    IF mod = NIL THEN
      H.wsn("Cannot resolve circular dependency order in:");
      ReportDependencies; H.ExitProcess(99)
    END;
    (* Move mod from Modules list to build list *)
    (*H.ws("Moving module "); H.ws(mod.modname); H.wsn(" to build list.");*)
    IF buildlast = NIL THEN buildfirst := mod ELSE buildlast.next := mod END;
    buildlast := mod;
    mod := mod.next;
    buildlast.next := NIL;
    IF prev = NIL THEN Modules := mod ELSE prev.next := mod END;
    RemoveDependencies(buildlast);
  END;
  Modules := buildfirst;
END SortModulesIntoBuildOrder;


PROCEDURE Build;
VAR
  PEname:     PathName;
  codesize:   INTEGER;
  varsize:    INTEGER;
  start, end: INTEGER;  (* Times *)
  maxalloc:   INTEGER;
BEGIN
  SortModulesIntoBuildOrder;

  H.wsl("Module", LongestModname + 2);  H.wsl("File", LongestFilename);
  H.wsn("        code         VAR     time");
  codesize := 0;
  varsize  := 0;
  maxalloc := 0;
  start    := H.Time();

  WHILE Modules # NIL DO
    Compile(Modules);
    IF ORS.errcnt # 0 THEN H.ExitProcess(99) END;
    IF Modules.modname # "Winshim" THEN WinPE.AddModule(Modules.codename) END;
    Modules := Modules.next;
    IF K.Allocated > maxalloc THEN maxalloc := K.Allocated END;
    Oberon.GC;
    INC(codesize, X64.PC);  INC(varsize, ORG.Varsize)
  END;

  PEname := ""; H.Append(Modulename, PEname);  H.Append(".exe", PEname);
  WinPE.Generate(PEname, LoadFlags);

  end := H.Time();
  H.wsl("Total", LongestModname + LongestFilename + 2);
  WriteHuman(codesize, 12);     WriteHuman(varsize,  12);
  WriteHuman(end - start, 6);   H.wsn(" ms");
  IF K.Allocated > maxalloc THEN maxalloc := K.Allocated END;
  H.ws("Max heap size : "); WriteHuman(maxalloc, 1); H.wsn(".");
END Build;





(*
PROCEDURE AddExecutableDirToSourceSearchpath;
VAR i, j, k: INTEGER;
BEGIN
  (*
  H.ws("Initial directory: '"); H.ws(K.InitialDirectory); H.wsn("'.");
  H.ws("Executable file:   '"); H.ws(K.ExecutablePath);   H.wsn("'.");
  *)
  (* See if executable path starts with current working directory and omit if so *)
  i := 0;
  WHILE (i < LEN(K.InitialDirectory))
      & (i < LEN(K.ExecutablePath))
      & (K.InitialDirectory[i] # 0X)
      & (K.InitialDirectory[i] = K.ExecutablePath[i]) DO INC(i) END;
  IF (i > 0) & (K.InitialDirectory[i] = 0X)
   & ((K.ExecutablePath[i] = '/') OR (K.ExecutablePath[i] = '\')) THEN INC(i) ELSE i := 0 END;

  (* 'i' is start of executable directory *)
  (* Find end of path componenet of executable filename *)
  j := -1;  k := i;
  WHILE (k < LEN(K.ExecutablePath)) & (K.ExecutablePath[k] # 0X) DO
    IF (K.ExecutablePath[k] = "/") OR (K.ExecutablePath[k] = '\') THEN j := k END;
    INC(k)
  END;

  IF j >= 0 THEN
    (* 'i'and 'j' are first and limit character of executable directory *)
    (* Find end of source searchpath *)
    k := 0;
    WHILE (k < LEN(SourcePath)) & (SourcePath[k] # 0X) DO INC(k) END;

    (* Add path separator to source searchpath if none already present *)
    IF k < LEN(SourcePath) - 1 THEN
      IF (k > 0) & (SourcePath[k-1] # "/") & (SourcePath[k-1] # '\') THEN
        SourcePath[k] := "/";  INC(k);
      END
    END;

    (* Add executable path to source search path *)
    IF k + j-i + 3 < LEN(SourcePath) THEN
      SourcePath[k] := ";";  INC(k);
      WHILE i < j DO SourcePath[k] := K.ExecutablePath[i];  INC(i);  INC(k) END;
      SourcePath[k] := '/';  INC(k);
      SourcePath[k] := 0X;
    END
  END
END AddExecutableDirToSourceSearchpath;
*)


(* -------------------------------------------------------------------------- *)
(* ---------------------------- Argument parsing ---------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE ArgError(n: INTEGER; arg, msg: ARRAY OF CHAR);
BEGIN
  H.ws("Argument "); H.wi(n); H.ws(" '"); H.ws(arg); H.ws("': "); H.wsn(msg);
  H.ExitProcess(99);
END ArgError;

PROCEDURE ScanArguments;
VAR i: INTEGER;  arg: ARRAY 1024 OF CHAR;
BEGIN
  SourcePath := "./";
  BuildPath  := "";
  Modulename := "";

  i := 1;
  WHILE i < WinArgs.Argcount DO
    WinArgs.GetArg(i, arg);
    IF arg[0] = "/" THEN
      IF    arg = "/source" THEN INC(i);  WinArgs.GetArg(i, SourcePath)
      ELSIF arg = "/s"      THEN INC(i);  WinArgs.GetArg(i, SourcePath)
      ELSIF arg = "/build"  THEN INC(i);  WinArgs.GetArg(i, BuildPath)
      ELSIF arg = "/b"      THEN INC(i);  WinArgs.GetArg(i, BuildPath)
      ELSIF arg = "/v"      THEN Verbose := TRUE; INCL(LoadFlags, H.Verbose)
      ELSE
        ArgError(i, arg, "unrecognised option.")
      END
    ELSE
      IF Modulename = "" THEN
        Modulename := arg
      ELSE
        ArgError(i, arg, "filename already specified.")
      END
    END;
    INC(i)
  END;

  H.ws("OB - Oberon command line builder.");
  IF Modulename # "" THEN
    H.ws(" MODULE "); H.ws(Modulename); H.wsn(".");
  ELSE
    H.wn;
    H.wsn("No parameters. Expected name of module to build.");
    H.ExitProcess(99);
  END
END ScanArguments;


BEGIN
  Verbose         := FALSE;
  LoadFlags       := {};
  LongestModname  := 0;
  LongestFilename := 0;
  ScanArguments;

  (*AddExecutableDirToSourceSearchpath;*)
  Build;

  Oberon.GC
END ob.
