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

  (* ---- *)

  Prefix= POINTER TO PrefixDesc;
  PrefixDesc = RECORD
    next:   Prefix;
    prefix: PathName
  END;

  Module2 = POINTER TO Module2Desc;
  Module2Desc = RECORD
    next:     Module2;
    modname:  ModuleName;  (* Just the module name: no directory, no extension *)

    sourcefn:   PathName;  sourcefile: Files.File;  sourceprefix: Prefix;
    sourcetime: INTEGER;
    symfn:      PathName;  symfile:    Files.File;  symprefix:  Prefix;
    symtime:    INTEGER;   symkey:     INTEGER;
    codefn:     PathName;  codefile:   Files.File;  codeprefix: Prefix;
    codetime:   INTEGER;   codekey:    INTEGER;
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

  SourcePrefices: Prefix;
  BinaryPrefices: Prefix;

  Modules2:       Module2;


(* ------------- File open with alternative file name prefix -------------- *)

PROCEDURE Open(
  name:       ARRAY OF CHAR;
  prefices:   Prefix;
  VAR file:   Files.File;
  VAR prefix: Prefix);
VAR
  fn: PathName;
BEGIN
  file := NIL;  prefix := prefices;
  IF prefix = NIL THEN
    file := Files.Old(name)
  ELSE
    WHILE (file = NIL) & (prefix # NIL) DO
      fn := prefix.prefix;
      H.Append(name, fn);
      file := Files.Old(fn);
      IF file = NIL THEN prefix := prefix.next END
    END
  END
END Open;

PROCEDURE Copy(source: ARRAY OF CHAR; VAR target: ARRAY OF CHAR);
VAR i, lim: INTEGER;
BEGIN
  i := 0;
  IF LEN(source) < LEN(target) THEN
    lim := LEN(source) - 1
  ELSE
    lim := LEN(target) - 1
  END;
  WHILE i < lim DO target[i] := source[i]; INC(i) END;
  target[i] := 0X
END Copy;

PROCEDURE CopySlice(
  source:     ARRAY OF CHAR;
  sourcepos:  INTEGER;
  len:        INTEGER;
  VAR target: ARRAY OF CHAR;
  targetpos:  INTEGER);
VAR
  i, j, lim: INTEGER;
BEGIN
  i := sourcepos;  j := targetpos;  lim := i + len;
  WHILE i < lim DO target[j] := source[i]; INC(i); INC(j) END;
  target[j] := 0X
END CopySlice;

PROCEDURE ParsePrefices(str: ARRAY OF CHAR; VAR prefix: Prefix);
VAR lastprefix: Prefix;  i, j: INTEGER;
BEGIN
  prefix := NIL;  i := 0;
  WHILE str[i] # 0X DO
    WHILE str[i] = ";" DO INC(i) END;
    j := i;
    WHILE (str[j] # 0X) & (str[j] # ";") DO INC(j) END;
    IF prefix = NIL THEN
      NEW(prefix);  lastprefix := prefix
    ELSE
      NEW(lastprefix.next);  lastprefix := lastprefix.next
    END;
    CopySlice(str, i, j-i, lastprefix.prefix, 0);
    i := j
  END
END ParsePrefices;

PROCEDURE InitModule(modname: ARRAY OF CHAR; VAR module: Module2);
VAR
  fn:      PathName;
  r:       Files.Rider;
  symkey:  INTEGER;
  codekey: INTEGER;
  last:    Module2;
BEGIN
  module := Modules2;  last := NIL;
  WHILE (module # NIL) & (modname # module.modname) DO
    last := module; module := module.next
  END;

  IF module = NIL THEN
    NEW(module);
    IF Modules2 = NIL THEN Modules2 := module ELSE last.next := module END;
    Copy(modname, module.modname);
    module.symkey := 1;  module.codekey := 2;
    module.sourcetime := 0; module.symtime := 0; module.codetime := 0;

    Copy(modname, module.sourcefn);  H.Append(".mod", module.sourcefn);
    Open(module.sourcefn, SourcePrefices, module.sourcefile, module.sourceprefix);
    IF module.sourcefile # NIL THEN
      module.sourcetime := Files.Date(module.sourcefile)
    END;

    Copy(modname, module.symfn);  H.Append(".smb", module.symfn);
    Open(module.symfn, BinaryPrefices, module.symfile, module.symprefix);
    IF module.symfile # NIL THEN
      module.symtime := Files.Date(module.symfile);
      Files.Set(r, module.symfile, 8);  Files.ReadInt(r, module.symkey)
    END;

    Copy(modname, module.codefn);  H.Append(".code", module.codefn);
    Open(module.codefn, BinaryPrefices, module.codefile, module.codeprefix);
    IF module.codefile # NIL THEN
      module.codetime := Files.Date(module.codefile);
      Files.Set(r, module.codefile, 20H); Files.ReadInt(r, module.codekey);
    END;

    H.ws("InitModule("); H.ws(modname); H.wsn("):");
    IF module.sourcefile # NIL THEN
      H.ws("  source: fn ");  H.ws(module.sourcefn);
      H.ws(",  prefix ");     IF module.sourceprefix = NIL THEN H.ws("NIL") ELSE H.ws(module.sourceprefix.prefix) END;
      H.ws(", date ");        H.WriteTime(module.sourcetime);
      H.wsn(".");
    END;
    IF module.symfile # NIL THEN
      H.ws("  sym:    fn ");  H.ws(module.symfn);
      H.ws(",  prefix ");     IF module.symprefix = NIL THEN H.ws("NIL") ELSE H.ws(module.symprefix.prefix) END;
      H.ws(", date ");        H.WriteTime(module.symtime);
      H.ws(", key ");         H.wh(module.symkey);
      H.wsn(".");
    END;
    IF module.codefile # NIL THEN
      H.ws("  code:   fn ");  H.ws(module.codefn);
      H.ws(", prefix ");      IF module.codeprefix = NIL THEN H.ws("NIL") ELSE H.ws(module.codeprefix.prefix) END;
      H.ws(", date ");        H.WriteTime(module.codetime);
      H.ws(", key ");         H.wh(module.codekey);
      H.wsn(".");
    END;
  END;

  IF (module.sourcefile # NIL)
   & (module.symfile    # NIL)
   & (module.codefile   # NIL) THEN
    IF (module.sourcetime > module.symtime) OR (module.sourcetime > module.codetime) THEN
      H.wsn("  Use source files as newer.")
    ELSE
      H.wsn("  Attempt using pre-compiled binaries as source not modified.")
    END
  ELSE
    IF module.sourcefile # NIL THEN
      H.wsn("  Use source files as no pre-compiled binaries.")
    ELSIF (module.symfile # NIL) & (module.codefile # NIL) THEN
      IF module.symkey = module.codekey THEN
        H.wsn("  Attempt using pre-compiled binary as no source file.")
      ELSE
        H.wsn("  Cannot build as no source file, and pre-compiled binary symbols and code do not match.")
      END
    ELSE
      H.wsn("  Cannot build as neither source files nor suitabe pre-compiled binaries are available.");
    END
  END;
END InitModule;




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
  dummy:   Module2;
BEGIN
  (*---*)InitModule(modname, dummy);(*---*)

  NEW(mod);
  mod.modname := modname;  mod.scanned := FALSE;
  filename := modname;  H.Append(".mod", filename);
  FindFile(filename,   SourcePrefix, file, mod.filename);
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
  H.wsl(module.modname,  LongestModname  + 2);
  H.wsl(module.filename, LongestFilename);
  (*
  B.SetSourcePath(SourcePrefix);
  B.SetBuildPath(BinariesPrefix);
  *)

  startTime := H.Time();
  ORS.Init(module.text, 0);  ORP.Module(module.filename, newsymbols);

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
    maxalloc  := ORG.Max(maxalloc,  K.Allocated);
    maxptrs   := ORG.Max(maxptrs,   ORG.Hdr.commands - ORG.Hdr.pointers);
    maxcmd    := ORG.Max(maxcmd,    ORG.Hdr.lines    - ORG.Hdr.commands);
    maxannot  := ORG.Max(maxannot,  ORG.Hdr.exports  - ORG.Hdr.lines);
    maxexport := ORG.Max(maxexport, ORG.Hdr.imports  - ORG.Hdr.exports);
    maximport := ORG.Max(maximport, ORG.Hdr.length   - ORG.Hdr.imports);

    IF ORS.errcnt # 0 THEN H.ExitProcess(99) END;
    IF Modules.modname # "WinHost" THEN WinPE.AddModule(Modules.codename) END;
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

  (* --- prototyping --- *)
  ParsePrefices(SourcePrefix,   SourcePrefices);
  ParsePrefices(BinariesPrefix, BinaryPrefices);
  (* --- prototyping --- *)

  (*AddExecutableDirToSourceSearchpath;*)
  Build;

  Oberon.GC
END obuild.
