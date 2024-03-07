MODULE obuild;

(* Hosted Oberon build command                                                *)
(*                                                                            *)
(* Given a single module to build, works out what to compile to satisfy its   *)
(* imports, compiles them, and combines all objects into a single executable. *)

IMPORT
  SYSTEM, H := WinHost, K := Kernel, Files, Texts, Oberon, ORS, X64, ORG, ORP, WinPE, WinArgs;

TYPE
  ModuleName = ARRAY 1024 OF CHAR;
  PathName   = ARRAY H.MaxPath OF CHAR;

  Module     = POINTER TO ModuleDesc;
  Dependency = POINTER TO DependencyDesc;

  ModuleDesc = RECORD
    next:         Module;
    modname:      ModuleName;  (* Just the module name: no directory, no extension *)
    filenamex:     PathName;
    codename:     PathName;
    file:         Files.File;
    codefile:     Files.File;
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
  SourcePrefix:    PathName;
  BinariesPrefix:  PathName;
  OutputPrefix:    PathName;
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
  path:   PathName;
  i, j:   INTEGER;
BEGIN
  i := 0;  file := NIL;
  IF searchpath = "" THEN
    file := Files.Old(name)
  ELSE
    WHILE (file = NIL) & (searchpath[i] # 0X) DO
      j := 0;
      WHILE (i < LEN(searchpath)) & (searchpath[i] # 0X) & (searchpath[i] # ";") DO
        path[j] := searchpath[i];  INC(i);  INC(j);
      END;
      IF (path[j-1] # "/") & (path[j-1] # 5CX) THEN path[j] := 5CX; INC(j) END;  (* Switch / to \ *)
      path[j] := 0X;  H.Append(name, path);
      file := Files.Old(path);
      WHILE searchpath[i] = ";" DO INC(i) END;
    END
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
BEGIN
  NEW(mod);
  mod.modname := modname;  mod.scanned := FALSE;
  filename := modname;  H.Append(".mod", filename);
  FindFile(filename,   SourcePrefix, file, mod.filenamex);
  IF file = NIL THEN
    H.ws("Could not find source file "); H.ws(filename); H.ws(" for module '"); H.ws(modname); H.wsn("'.");
    H.ExitProcess(99)
  END;
  mod.file := file;
  mod.codename := "";  H.Append(modname, mod.codename);  H.Append(".code", mod.codename);
  IF H.Length(modname)       > LongestModname  THEN LongestModname  := H.Length(modname)       END;
  IF H.Length(mod.filenamex) > LongestFilename THEN LongestFilename := H.Length(mod.filenamex) END;
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
  Texts.OpenFile(module.text, module.file);
  ORS.Init(module.text, 0);  ORS.Get(sym);
  IF sym # ORS.module THEN Expected(module.filenamex, "does not start with MODULE.") END;
  ORS.Get(sym);
  IF sym # ORS.ident THEN Expected(module.filenamex, "expected module id."); END;
  IF ORS.id # module.modname THEN
    H.ws("File "); H.ws(module.filenamex); WriteFilepos; H.ws(" module id '");
    H.ws(ORS.id); H.ws("' does not match expected id '");
    H.ws(module.modname); H.wsn("'.");
    H.ExitProcess(99)
  END;
  (*B.Init(S.id);*)
  ORS.Get(sym);
  IF sym # ORS.semicolon THEN Expected(module.filenamex, "expected semicolon from module id.") END;
  ORS.Get(sym);
  IF sym = ORS.import THEN
    REPEAT
      ORS.Get(sym);
      IF sym # ORS.ident THEN Expected(module.filenamex, "expected id (1).") END;
      impname := ORS.id;  ORS.Get(sym);
      IF sym = ORS.becomes THEN
        ORS.Get(sym);
        IF sym # ORS.ident THEN Expected(module.filenamex, "expected id (2).") END;
        impname := ORS.id;  ORS.Get(sym)
      END;
      IF (impname # "SYSTEM") & (impname # "WinHost") & (impname # "Kernel") THEN
        AddImport(module, impname)
      END
    UNTIL sym # ORS.comma
  END;

  IF module.modname # "WinHost" THEN  (* All modules need WinHost, excepting WinHost itself *)
    AddImport(module, "WinHost");
    IF module.modname # "Kernel" THEN (* All modules need Kernel, excepting WinHost and Kernel *)
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

PROCEDURE Compile(module: Module; VAR newsymbols: BOOLEAN);
VAR startTime, endTime: INTEGER;
BEGIN
  H.wsl(module.modname,   LongestModname  + 2);
  H.wsl(module.filenamex, LongestFilename);
  (*
  B.SetSourcePath(SourcePrefix);
  B.SetBuildPath(BinariesPrefix);
  *)

  startTime := H.Time();
  ORS.Init(module.text, 0);  ORP.Module(module.file, newsymbols, module.codefile);

  IF ORS.errcnt = 0 THEN
    endTime := H.Time();
    WriteHuman(X64.PC, 12);
    WriteHuman(ORG.Varsize, 12);
    WriteHuman((endTime - startTime) DIV 10000, 6);
    WriteHuman(ORG.Hdr.commands - ORG.Hdr.pointers, 11);
    WriteHuman(ORG.Hdr.lines    - ORG.Hdr.commands, 6);
    WriteHuman(ORG.Hdr.exports  - ORG.Hdr.lines,    7);
    WriteHuman(ORG.Hdr.imports  - ORG.Hdr.exports,  8);
    WriteHuman(ORG.Hdr.length   - ORG.Hdr.imports,  8);
    WriteHuman(K.Allocated, 10);
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


PROCEDURE SortModulesIntoBuildOrder;
VAR
  mod, prev:  Module;
  allscanned: BOOLEAN;
  buildfirst: Module;
  buildlast:  Module;
BEGIN
  AddModule(Modulename);

  IF SourcePrefix   # "" THEN H.ws("Sources path:  '");  H.ws(SourcePrefix);   H.wsn("'") END;
  IF BinariesPrefix # "" THEN H.ws("Binaries path: '");  H.ws(BinariesPrefix); H.wsn("'") END;

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
  maxptrs:    INTEGER;
  maxcmd:     INTEGER;
  maxannot:   INTEGER;
  maxexport:  INTEGER;
  maximport:  INTEGER;
  i:          INTEGER;
  newsymbols: BOOLEAN;

BEGIN
  SortModulesIntoBuildOrder;

  H.wsl("Module", LongestModname + 2);  H.wsl("File", LongestFilename);
  H.wsn("        code         VAR    ms       ptrs   cmd  annot  export  import      heap");
  codesize  := 0;
  varsize   := 0;
  maxalloc  := 0;
  maxptrs   := 0;
  maxcmd    := 0;
  maxannot  := 0;
  maxexport := 0;
  maximport := 0;
  start     := H.Time();

  WHILE Modules # NIL DO
    newsymbols := TRUE;
    Compile(Modules, newsymbols);
    INC(codesize, X64.PC);
    INC(varsize, ORG.Varsize);
    maxalloc  := H.Max(maxalloc,  K.Allocated);
    maxptrs   := H.Max(maxptrs,   ORG.Hdr.commands - ORG.Hdr.pointers);
    maxcmd    := H.Max(maxcmd,    ORG.Hdr.lines    - ORG.Hdr.commands);
    maxannot  := H.Max(maxannot,  ORG.Hdr.exports  - ORG.Hdr.lines);
    maxexport := H.Max(maxexport, ORG.Hdr.imports  - ORG.Hdr.exports);
    maximport := H.Max(maximport, ORG.Hdr.length   - ORG.Hdr.imports);

    IF ORS.errcnt # 0 THEN H.ExitProcess(99) END;
    (*IF Modules.modname # "WinHost" THEN WinPE.AddModule(Modules.codefile) END;*)
    WinPE.AddModule(Modules.codefile);
    Modules := Modules.next;
    Oberon.GC
  END;

  PEname := "";
  IF OutputPrefix # "" THEN H.Append(OutputPrefix, PEname) END;
  H.Append(Modulename, PEname);  H.Append(".exe", PEname);
  WinPE.Generate(PEname, LoadFlags);

  end := H.Time();

  H.wb(LongestModname + LongestFilename);
  H.wsn("    ==========  ==========  ====      =====  ====  =====  ======  ======  ========");

  H.wsl("Total:", LongestModname + LongestFilename + 2);

  WriteHuman(codesize, 12);     WriteHuman(varsize,  12);
  WriteHuman((end - start) DIV 10000, 6);   H.ws("  Max:");

  IF K.Allocated > maxalloc THEN maxalloc := K.Allocated END;
  WriteHuman(maxptrs,   5);
  WriteHuman(maxcmd,    6);
  WriteHuman(maxannot,  7);
  WriteHuman(maxexport, 8);
  WriteHuman(maximport, 8);
  WriteHuman(maxalloc,  10);  H.wn
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
    WHILE (k < LEN(SourcePrefix)) & (SourcePrefix[k] # 0X) DO BinariesPrefix  (* Add path separator to source searchpath if none already present *)
    IF k < LEN(SourcePrefix)   - 1 THEN
      IF (k > 0BinariesPrefixix[k  -1] # "  /") & (So  urcePrefix[k-1] # '\') THEN
        SourcePrefixBinariesPrefixsPrefixiesPrefix  END;

    (* Add executable path to source search path *)
    IF k + j-i + 3 < LEN(SourcePrefix)   THEN
      SourcePrefix[k] := BinariesP  refix     BinariesPrefixSourcePrefix[k] := K.ExecutablePath[i];  INC(i);  INC(k) END;
      SourcePrefix[  k]BinariesPrefix)  ;
      BinariesPrefix :=BinariesPrefixEND
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
  SourcePrefix   := "";
  BinariesPrefix := "";
  OutputPrefix   := "";
  Modulename     := "";

  i := 1;
  WHILE i < WinArgs.Argcount DO
    WinArgs.GetArg(i, arg);
    IF arg[0] = "/" THEN
      IF    (arg = "/sources")  OR (arg = "/s") THEN INC(i);  WinArgs.GetArg(i, SourcePrefix)
      ELSIF (arg = "/binaries") OR (arg = "/b") THEN INC(i);  WinArgs.GetArg(i, BinariesPrefix)
      ELSIF (arg = "/output")   OR (arg = "/o") THEN INC(i);  WinArgs.GetArg(i, OutputPrefix)
      ELSIF (arg = "/verbose")  OR (arg = "/v") THEN Verbose := TRUE; INCL(LoadFlags, H.Verbose)
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

  H.ws("obuild - Oberon command line builder");
  IF Modulename # "" THEN
    H.ws(", building MODULE "); H.ws(Modulename); H.wsn(".");
  ELSE
    H.wsn(".");
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
END obuild.
