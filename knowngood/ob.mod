MODULE ob;

(* Hosted Oberon build command                                                *)
(*                                                                            *)
(* Given a single module to build, works out what to compile to satisfy its   *)
(* imports, compiles them, and combines all objects into a single executable. *)


IMPORT
  SYSTEM, H := Winshim, Files, Texts, w := Writer, ORS, X64, ORG, ORP, WinPE, WinArgs;

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
    file:         Files.File;
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
    (*w.s("Found "); w.s(name); w.s(" at "); w.s(path); w.sl(".")*)
  END
END FindFile;


(* -------------------------------------------------------------------------- *)
(* ------------------ Scan module recording dependencies -------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE AddModule(modname: ARRAY OF CHAR);
VAR mod: Module;  filename: ModuleName;
BEGIN NEW(mod);
  mod.modname := modname;  mod.scanned := FALSE;
  filename := modname;  H.Append(".mod", filename);
  FindFile(filename, SourcePath, mod.file, mod.filename);
  IF mod.file = NIL THEN
    w.s("Could not find source file "); w.s(filename); w.s(" for module '"); w.s(modname); w.sl("'.");
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
  w.c("["); w.i(ORS.linenum); w.c(":"); w.i(ORS.Pos() - ORS.linebeg); w.c("]")
END WriteFilepos;

PROCEDURE Expected(filename, message: ARRAY OF CHAR);
BEGIN
  w.s("File '"); w.s(filename); WriteFilepos; w.s("': "); w.sl(message); H.ExitProcess(99)
END Expected;


PROCEDURE ScanModuleFileImports(module: Module);
VAR
  T:       Texts.Text;
  sym:     INTEGER;
  impname: ORS.Ident;
BEGIN
  NEW(T);  Texts.Open(T, module.filename);  ORS.Init(T, 0);  ORS.Get(sym);
  IF sym # ORS.module THEN Expected(module.filename, "does not start with MODULE.") END;
  ORS.Get(sym);
  IF sym # ORS.ident THEN Expected(module.filename, "expected module id."); END;
  IF ORS.id # module.modname THEN
    w.s("File "); w.s(module.filename); WriteFilepos; w.s(" module id '");
    w.s(ORS.id); w.s("' does not match expected id '");
    w.s(module.modname); w.sl("'.");
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

(*PROCEDURE humanInt(i, n: INTEGER);
BEGIN
   IF n < 0 THEN n := 0 END;
   IF i < 1000 THEN w.in(i, -n)
   ELSE
      humanInt(i DIV 1000, n-4);  w.c(",");
      i := i MOD 1000;
      w.c(CHR(ORD("0") + i DIV 100));  i := i MOD 100;
      w.c(CHR(ORD("0") + i DIV 10));   i := i MOD 10;
      w.c(CHR(ORD("0") + i));
   END
END humanInt;

PROCEDURE Compile(module: Module);
VAR sym, startTime, endTime: INTEGER;  modinit: B.Node;
BEGIN
  w.sn(module.modname, LongestModname+1); w.sn(module.filename, LongestFilename+1);
  B.SetSourcePath(SourcePath);
  B.SetBuildPath(BuildPath);
  S.Init(module.file);  ORS.Get(sym);
  startTime := K.Time();
  IF sym = S.module THEN modinit := P.Module() ELSE S.Mark("Expected 'MODULE'") END;
  IF S.errcnt = 0 THEN
    B.WriteSymfile;
    G.Generate(modinit);
    B.Cleanup;
    G.Cleanup;  endTime := K.Time();
    humanInt(G.pc,      10);   humanInt(G.staticSize,  10);
    humanInt(ORG.Varsize, 10);   humanInt(endTime - startTime, 5);
    w.s("ms");  w.l
  END
END Compile;
*)


(* -------------------------------------------------------------------------- *)
(* ------------------------ Build all needed modules ------------------------ *)
(* -------------------------------------------------------------------------- *)

PROCEDURE RemoveDependencies(remove: Module);
VAR mod: Module;  dep, prev: Dependency;
BEGIN
  (*w.s("RemoveDependencies("); w.s(remove.modname); w.sl(")");*)
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
    w.s("Module "); w.sn(mod.modname, LongestModname);
    dep := mod.dependencies;
    IF dep = NIL THEN
      w.sl(" (none).")
    ELSE
      WHILE dep # NIL DO w.c(" ");  w.s(dep.mod.modname);  dep := dep.next END;
      w.sl(".")
    END;
    mod := mod.next
  END
END ReportDependencies;


PROCEDURE Build();
VAR
  mod, prev:  Module;
  allscanned: BOOLEAN;
  PEname:     PathName;
  codesize:   INTEGER;
  varsize:    INTEGER;
  (*
  start, end: INTEGER;  (* Times *)
  *)

BEGIN
  w.s("OB - Oberon Windows EXE builder: building "); w.s(Modulename); w.s(" at "); w.LongClock(H.LongClock()); w.sl(".");
  AddModule(Modulename);

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

  IF Verbose THEN w.sl("Modules and dependencies:");  ReportDependencies END;

  (* Compile dependentless modules until all modules compiled. *)
  w.sn("Module", LongestModname+1);  w.sn("File", LongestFilename+1);
  w.sl("      code    static       VAR   time");
  codesize := 0;  varsize := 0;  (*start := K.Time();*)
  REPEAT
    mod := Modules;  prev := NIL;
    WHILE (mod # NIL) & (mod.dependencies # NIL) DO
      prev := mod;  mod := mod.next
    END;
    IF mod # NIL THEN
      (*Compile(mod);*)
      ORP.CompileFile(mod.filename);
      IF ORS.errcnt # 0 THEN H.ExitProcess(99) END;

      IF mod.modname # "Winshim" THEN WinPE.AddModule(mod.codename) END;

      INC(codesize, X64.PC);  INC(varsize, ORG.Varsize);
      RemoveDependencies(mod);
      IF prev = NIL THEN Modules := mod.next ELSE prev.next := mod.next END
    ELSE
      w.sl("Cannot resolve circular dependency order in:");
      ReportDependencies; H.ExitProcess(99)
    END
  UNTIL Modules = NIL;

  PEname := ""; H.Append(Modulename, PEname);  H.Append(".exe", PEname);
  WinPE.Generate(PEname(*, LoadFlags*));

  (*
  end := K.Time();
  w.sn("Total", LongestModname + LongestFilename + 2);
  humanInt(codesize, 10);   humanInt(staticsize,  10);
  humanInt(varsize,  10);   humanInt(end - start,  5);
  w.sl("ms")
  *)
END Build;


(*
PROCEDURE AddExecutableDirToSourceSearchpath;
VAR i, j, k: INTEGER;
BEGIN
  (*
  w.s("Initial directory: '"); w.s(K.InitialDirectory); w.sl("'.");
  w.s("Executable file:   '"); w.s(K.ExecutablePath);   w.sl("'.");
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
  w.s("Argument "); w.i(n); w.s(" '"); w.s(arg); w.s("': "); w.sl(msg);
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

  IF Modulename = "" THEN
    w.sl("Build - Oberon recursive builder.");
    w.sl("No parameters. Expected name of module to build.");
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
  w.s("SourcePath: '");    w.s(SourcePath);
  w.s("', BuildPath: '");  w.s(BuildPath);  w.sl("'.");
  Build
END ob.
