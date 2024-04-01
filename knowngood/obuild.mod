MODULE obuild;

(* Hosted Oberon build command                                                *)
(*                                                                            *)
(* Given a single module to build, works out what to compile to satisfy its   *)
(* imports, compiles them, and combines all objects into a single executable. *)

IMPORT
  SYSTEM, H := WinHost, K := Kernel, Files, Texts, ORS, ORB, X64, ORG, ORP, WinPE, WinArgs;

TYPE
  PathName   = ARRAY H.MaxPath OF CHAR;

  Prefix = POINTER TO PrefixDesc;  (* File name prefix *)
  PrefixDesc = RECORD
    next:   Prefix;
    prefix: PathName
  END;

  ModuleFile = POINTER TO ModuleFileDesc;
  Import     = POINTER TO ImportDesc;

  FileDesc = RECORD
    fn:     PathName;
    prefix: Prefix;
    file:   Files.File;
    time:   INTEGER
  END;

  ImportDesc = RECORD
    name:   ORS.Ident;
    module: ModuleFile;
    key:    INTEGER;  (* Used for code file imports, not source file imports *)
    next:   Import
  END;

  ModuleFileDesc = RECORD
    next:       ModuleFile;  (* List of all modules *)
    name:       ORS.Ident;
    source:     FileDesc;
    symbols:    FileDesc;
    code:       FileDesc;
    key:        INTEGER;
    text:       Texts.Text;
    imports:    Import;
    compiled:   BOOLEAN;
    srcscanned: BOOLEAN;
    addedToPE:  BOOLEAN;
  END;

VAR
  (* Command line arguments *)
  Modulename:     ORS.Ident;
  SourcePrefix:   PathName;
  BinariesPrefix: PathName;
  OutputPrefix:   PathName;
  LoadFlags:      SET;

  SourcePrefices: Prefix;
  BinaryPrefices: Prefix;

  (* Log output control *)
  LongestModname:  INTEGER;
  LongestFilename: INTEGER;
  FirstCompile:    BOOLEAN;

  Modules:         ModuleFile;

  BuildStart:      INTEGER;
  TotalCode:       INTEGER;
  TotalGlobals:    INTEGER;


(* ------------- File find with alternative file name prefix -------------- *)

PROCEDURE Find(
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
END Find;


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


PROCEDURE WritePrefices(p: Prefix);
BEGIN
  IF p # NIL THEN H.wc("'");  H.ws(p.prefix);  H.wc("'") END;
  p := p.next;
  WHILE p # NIL DO H.ws(", '");  H.ws(p.prefix);  H.wc("'");  p := p.next END
END WritePrefices;


PROCEDURE WriteHuman(n, w: INTEGER);
BEGIN
  IF n < 1000 THEN
    H.wir(n, w)
  ELSE
    WriteHuman(n DIV 1000, w-4);  H.wc(",");  H.wiz(n MOD 1000, 3)
  END
END WriteHuman;


PROCEDURE ClearFileDesc(VAR d: FileDesc);
BEGIN
  d.fn := "";  d.prefix := NIL;  d.file := NIL;  d.time := 0
END ClearFileDesc;


PROCEDURE Warn(mod, str: ARRAY OF CHAR);
BEGIN
  H.ws("** ModuleFile ");  H.ws(mod); H.ws(": "); H.ws(str);  H.wsn(" **")
END Warn;


PROCEDURE Fail(mod, str: ARRAY OF CHAR);
BEGIN Warn(mod, str);  H.ExitProcess(1) END Fail;


PROCEDURE WriteFile(d: FileDesc);
BEGIN
  IF d.file = NIL THEN
    H.ws("none")
  ELSE
    H.ws("'");       H.ws(d.fn);
    (*
    IF d.prefix # NIL THEN
      H.ws("' in '");  H.ws(d.prefix.prefix)
    END;
    *)
    H.ws("' "); H.WriteTime(d.time)
  END
END WriteFile;


PROCEDURE WriteModule(mod: ModuleFile);
BEGIN
  H.ws("ModuleFile "); H.ws(mod.name); H.wsn(":");
  IF mod.source.file = NIL THEN
    H.wsn("  no source file.")
  ELSE
    H.ws("  source file: "); WriteFile(mod.source);  H.wsn(".");
    H.ws("  symbol file: "); WriteFile(mod.symbols); H.wsn(".");
    H.ws("  code file:   "); WriteFile(mod.code);    H.wsn(".")
  END
END WriteModule;


PROCEDURE MakeFileDesc(name, ext: ARRAY OF CHAR; prefices: Prefix; VAR desc: FileDesc);
BEGIN
  Copy(name, desc.fn); H.Append(".", desc.fn); H.Append(ext, desc.fn);
  Find(desc.fn, prefices, desc.file, desc.prefix);
  IF desc.file = NIL THEN
    ClearFileDesc(desc)
  ELSE
    desc.time := Files.Date(desc.file)
  END
END MakeFileDesc;


PROCEDURE GetSymbolFileKey(f: Files.File; VAR key: INTEGER);
VAR r: Files.Rider;
BEGIN Files.Set(r, f, 8);  Files.ReadInt(r, key) END GetSymbolFileKey;


PROCEDURE ScanCodeFileKeyAndImports(f: Files.File; VAR key: INTEGER; VAR firstimport: Import);
VAR
  r:      Files.Rider;
  hdr:    H.ModuleDesc;
  import: Import;
  last:   Import;
BEGIN
  firstimport := NIL;
  LongestFilename := H.Max(LongestFilename, H.Length(f.name));
  Files.Set(r, f, 0);
  Files.ReadBytes(r, hdr, SYSTEM.SIZE(H.ModuleDesc));
  key := hdr.key;
  Files.Set(r, f, hdr.nimports);
  NEW(import);
  last := NIL;
  Files.ReadString(r, import.name);
  WHILE (f # NIL) & (import.name[0] # 0X) DO
    Files.ReadInt(r, import.key);
    IF last = NIL THEN firstimport := import ELSE last.next := import END;
    last := import;
    (*H.ws("  code scan for "); H.ws(f.name); H.ws(" added import "); H.ws(import.name); H.wsn(".");*)
    NEW(import);
    Files.ReadString(r, import.name)
  END
END ScanCodeFileKeyAndImports;


PROCEDURE ScanSourceFileImports(mod: ModuleFile; VAR firstimport: Import);
VAR
  sym:    INTEGER;
  import: Import;
  last:   Import;
BEGIN
  (*H.wsn("- ScanSourceFileImports -");*)
  firstimport := NIL;
  NEW(mod.text);  Texts.OpenFile(mod.text, mod.source.file);  ORS.Init(mod.text, 0);
  ORS.Get(sym);
  IF sym # ORS.module THEN ORS.Mark("ModuleFile does not start with MODULE.") END;
  ORS.Get(sym);
  IF sym # ORS.ident THEN ORS.Mark("expected module id."); END;
  IF ORS.id # mod.name THEN
    H.ws("File "); H.ws(mod.source.fn);
    H.wc("["); H.wi(ORS.linenum); H.wc(":"); H.wi(ORS.Pos() - ORS.linebeg); H.wc("]");
    H.ws(" module id '");                   H.ws(ORS.id);
    H.ws("' does not match expected id '"); H.ws(mod.name); H.wsn("'.");
    H.ExitProcess(99)
  END;
  ORS.Get(sym);
  IF sym # ORS.semicolon THEN ORS.Mark("expected semicolon following module id.") END;
  ORS.Get(sym);
  IF sym = ORS.import THEN
    NEW(import);  last := NIL;
    REPEAT
      ORS.Get(sym);
      IF sym # ORS.ident THEN ORS.Mark("expected import module id (1).") END;
      import.name := ORS.id;  ORS.Get(sym);
      IF sym = ORS.becomes THEN
        ORS.Get(sym);
        IF sym # ORS.ident THEN ORS.Mark("expected id (2).") END;
        import.name := ORS.id;  ORS.Get(sym)
      END;
      IF (import.name # "SYSTEM") & (import.name # "Kernel") & (import.name # "WinHost") THEN
        import.key := 0;
        IF last = NIL THEN firstimport := import ELSE last.next := import END;
        last := import;
        (*H.ws("  source scan for "); H.ws(mod.name); H.ws(" added import "); H.ws(import.name);  H.wsn(".");*)
        NEW(import)
      END;
    UNTIL sym # ORS.comma
  END;
  mod.srcscanned := TRUE;
END ScanSourceFileImports;


PROCEDURE GetModule(name: ARRAY OF CHAR): ModuleFile;
VAR mod: ModuleFile;  key: INTEGER;
BEGIN
  LongestModname := H.Max(LongestModname, H.Length(name));
  mod := Modules;
  WHILE (mod # NIL) & (mod.name # name) DO mod := mod.next END;
  IF mod = NIL THEN
    (*H.ws("GetModule("); H.ws(name); H.wsn(") creating new module desc.");*)
    NEW(mod);  mod.name := name;
    mod.compiled := FALSE;  mod.srcscanned := FALSE;  mod.addedToPE := FALSE;
    MakeFileDesc(name, "mod", SourcePrefices, mod.source);
    MakeFileDesc(name, "smb", BinaryPrefices, mod.symbols);
    MakeFileDesc(name, "x64", BinaryPrefices, mod.code);

    (* Check for symbol and code file present, matching and up to date *)
    IF (mod.symbols.file # NIL) OR (mod.code.file # NIL) THEN
      IF mod.symbols.file = NIL THEN
        ClearFileDesc(mod.code);
        Warn(name, ".smb without .code")
      ELSIF mod.code.file    = NIL THEN
        ClearFileDesc(mod.symbols);
        Warn(name, ".code without .smb")
      ELSIF mod.source.time >= mod.code.time THEN
        ClearFileDesc(mod.symbols);  ClearFileDesc(mod.code);
        Warn(name, "Ignoring .code as source is newer")
      ELSE
        (* Both code & symbol present, do they match? *)
        GetSymbolFileKey(mod.symbols.file, mod.key);
        ScanCodeFileKeyAndImports(mod.code.file, key, mod.imports);
        IF key # mod.key THEN
          ClearFileDesc(mod.symbols);  ClearFileDesc(mod.code);
          mod.imports := NIL;
          Warn(name, ".smb and .code files have differing keys")
        END
      END
    END;
    mod.next := Modules; Modules := mod;
  END
RETURN mod END GetModule;


PROCEDURE DetermineNextCompilation(mod: ModuleFile): ModuleFile;
VAR result: ModuleFile;  import: Import;
BEGIN
  (*H.ws("- DetermineNextCompilation("); H.ws(mod.name); H.wsn(") -");*)
  result := NIL;
  IF ~mod.compiled THEN
    (* Walk existing code file imports, if any *)
    IF mod.code.file # NIL THEN
      (*H.ws("- Walk MODULE "); H.ws(mod.name); H.wsn(" code file imports -");*)
      import := mod.imports;
      WHILE (result = NIL) & (import # NIL) DO
        IF import.module = NIL THEN import.module := GetModule(import.name) END;
        (*H.ws("Code file for "); H.ws(mod.name); H.ws(" importing "); WriteModule(import.module);*)
        result := DetermineNextCompilation(import.module);
        IF result = NIL THEN
          IF import.key # import.module.key THEN
            ClearFileDesc(mod.symbols);  ClearFileDesc(mod.code);
            Warn(import.name, "cannot use precompiled code owing to import key mismatch");
            (*
            H.ws("  ");  H.ws(import.name); H.ws(" expected key "); H.wh(import.key); H.wsn("H.");
            H.ws(" imported module "); H.ws(import.module.name); H.ws(" has key "); H.wh(import.module.key); H.wsn("H.");
            *)
            mod.imports := NIL;
            import      := NIL  (* Abort walking code module imports *)
          ELSE
            import := import.next
          END
        END
      END
    END;
    IF (result = NIL) & (mod.code.file = NIL) THEN
      (* There was no code file, or we cannot use it due to key mismatch *)
      IF ~mod.srcscanned THEN
        (*H.ws("- Walk MODULE "); H.ws(mod.name); H.ws(" source file imports -");*)
        IF mod.source.file = NIL THEN Fail(mod.name, "Missing source file") END;
        ScanSourceFileImports(mod, mod.imports);
      END;
      import := mod.imports;
      WHILE (result = NIL) & (import # NIL) DO
        IF import.module = NIL THEN import.module := GetModule(import.name) END;
        (*H.ws("Source file for "); H.ws(mod.name); H.ws(" importing "); WriteModule(import.module);*)
        result := DetermineNextCompilation(import.module);
        IF result = NIL THEN
          import := import.next
        END
      END;
      IF result = NIL THEN  (* Ready to compile this module *)
        result := mod
      END
    END;
    IF result = NIL THEN
      (*H.ws("MODULE "); H.ws(mod.name); H.wsn(" compiled.");*)
      mod.compiled := TRUE
    END
  END
RETURN result END DetermineNextCompilation;


PROCEDURE CompileModule(mod: ModuleFile);
VAR startTime, endTime, key: INTEGER;  newsymbols: BOOLEAN;
BEGIN
  IF FirstCompile THEN H.wn; FirstCompile := FALSE END;
  H.ws("Compiling "); H.wsl(mod.name, LongestModname);

  ASSERT(mod.text # NIL);
  startTime := H.Time();
  ORS.Init(mod.text, 0);  (* Rewind text to beginning for full compilation *)
  newsymbols := TRUE;
  ORP.Module(mod.source.file, newsymbols);

  IF ORS.errcnt = 0 THEN
    endTime := H.Time();
    WriteHuman((endTime - startTime) DIV 10000, 5);
    H.ws("ms "); WriteHuman(K.Allocated, 10); H.wsn("B heap used.");
    MakeFileDesc(mod.name, "smb", NIL, mod.symbols);  ASSERT(mod.symbols.file # NIL);
    MakeFileDesc(mod.name, "x64", NIL, mod.code);     ASSERT(mod.code.file    # NIL);
    GetSymbolFileKey(mod.symbols.file, mod.key);
    ScanCodeFileKeyAndImports(mod.code.file, key, mod.imports);
    IF key # mod.key THEN
      Fail(mod.name, "Following compilation .smb and .code files have differing keys")
    END
  ELSE
    H.wsn(".");  Fail(mod.name, "Compilation failed.")
  END
END CompileModule;


PROCEDURE CollectGarbage;
VAR hdr: H.Module;
BEGIN
  hdr := H.Root;
  WHILE hdr # NIL DO
    IF hdr.name[0] # 0X THEN K.Mark(hdr.ptr) END;
    hdr := hdr.next
  END;
  Files.CloseCollectableFiles;
  K.Scan;
END CollectGarbage;


PROCEDURE Build;
VAR compile, target: ModuleFile;
BEGIN
  compile := DetermineNextCompilation(GetModule("WinHost"));
  IF compile # NIL THEN CompileModule(compile) END;
  compile := DetermineNextCompilation(GetModule("Kernel"));
  IF compile # NIL THEN CompileModule(compile) END;
  target  := GetModule(Modulename);
  compile := DetermineNextCompilation(target);
  WHILE compile # NIL DO
    CompileModule(compile);
    CollectGarbage;
    compile := DetermineNextCompilation(target);
  END;
END Build;


PROCEDURE AddModule(mod: ModuleFile);
VAR
  r:   Files.Rider;
  hdr: H.ModuleDesc;
BEGIN
  Files.Set(r, mod.code.file, 0);
  Files.ReadBytes(r, hdr, SYSTEM.SIZE(H.ModuleDesc));
  H.wsl(mod.name, LongestModname + 1);
  WriteHuman(hdr.ptr, 12);  INC(TotalCode,    hdr.ptr);
  WriteHuman(hdr.nvarsize, 12);   INC(TotalGlobals, hdr.nvarsize);
  WriteHuman(hdr.cmd      - hdr.ptr,      8);
  WriteHuman(hdr.nlines   - hdr.cmd,      6);
  WriteHuman(hdr.nexports - hdr.nlines,   8);
  WriteHuman(hdr.nimports - hdr.nexports, 8);
  WriteHuman(hdr.size     - hdr.nimports, 8);
  H.wn;
  WinPE.AddModule(mod.code.file);
  mod.addedToPE := TRUE
END AddModule;


PROCEDURE AddImports(mod: ModuleFile);  (* Recursively add modules imports and module *)
VAR import: Import;
BEGIN
  import := mod.imports;
  WHILE import # NIL DO AddImports(import.module);  import := import.next END;
  IF ~mod.addedToPE THEN AddModule(mod) END
END AddImports;


PROCEDURE Generate;
VAR mod: ModuleFile;  PEname: PathName;  i: INTEGER;
BEGIN
  PEname := "";  Copy(OutputPrefix, PEname);
  H.Append(Modulename, PEname);  H.Append(".exe", PEname);

  H.wn;
  H.wsl("Module", LongestModname);
  H.wsn("  static-code global-vars recptrs  cmds lineadr  export  import");

  FOR i := 1 TO LongestModname DO H.wc("-") END;
  H.wsn(" ------------ ----------- ------- ----- ------- ------- -------");

  mod := GetModule("WinHost");  AddModule(mod);
  mod := GetModule("Kernel");   AddModule(mod);
  AddImports(GetModule(Modulename));

  H.wb(LongestModname);  H.wsn(" ============ ===========");
  H.wsl("Total", LongestModname + 1);
  WriteHuman(TotalCode, 12);
  WriteHuman(TotalGlobals, 12);
  H.wn;

  WinPE.Generate(PEname, LoadFlags);

  H.wn;
  H.ws("Generated "); H.ws(PEname);
  H.ws(" in "); WriteHuman((H.Time() - BuildStart) DIV 10000, 1); H.wsn("ms.");
END Generate;


PROCEDURE FindSymbolFile(name: ORS.Ident): Files.File;
VAR mod: ModuleFile; result: Files.File;
BEGIN mod := Modules;
  WHILE (mod # NIL) & (mod.name # name) DO mod := mod.next END;
  IF mod # NIL THEN result := mod.symbols.file END;
RETURN result END FindSymbolFile;


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
      ELSIF (arg = "/verbose")  OR (arg = "/v") THEN INCL(LoadFlags, H.Verbose)
      ELSE
        ArgError(i, arg, "unrecognised option.")
      END
    ELSE
      IF Modulename = "" THEN
        Copy(arg, Modulename)
      ELSE
        ArgError(i, arg, "filename already specified.")
      END
    END;
    INC(i)
  END;

  H.ws("");
  IF Modulename = "" THEN
    H.wsn("obuild - Oberon command line builder.");
    H.wsn("No parameters. Expected name of module to build.");
    H.wn;
    H.wsn("obuild options module");
    H.wn;
    H.wsn("  options:");
    H.wsn("    /sources sourceprefices  - specify ';' separated list of source file prefixes to try.");
    H.wsn("    /binaries binaryprefices - specify ';' separated list of binary file prefixes to try.");
    H.wsn("    /output outputprefix     - prefix to add before module name in output file name.");
    H.wsn("    /verbose                 - build and exe loading will be verbose.");
    H.wn;
    H.wsn("    Note - options can be abbereviated to a single letter.");
    H.wn;
    H.wsn("  module: name of module to be built.");
    H.ExitProcess(99);
  END
END ScanArguments;


BEGIN
  BuildStart      := H.Time();
  LoadFlags       := {};
  LongestModname  := 0;
  LongestFilename := 0;
  FirstCompile    := TRUE;
  Modules         := NIL;
  TotalCode       := 0;
  TotalGlobals    := 0;

  ScanArguments;

  ParsePrefices(SourcePrefix,   SourcePrefices);
  IF (H.Verbose IN LoadFlags) & (SourcePrefices # NIL) THEN
    H.ws("Source prefixes: ");  WritePrefices(SourcePrefices); H.wsn(".")
  END;
  ParsePrefices(BinariesPrefix, BinaryPrefices);
  IF (H.Verbose IN LoadFlags) & (BinaryPrefices # NIL) THEN
    H.ws("Binary prefixes: ");  WritePrefices(BinaryPrefices); H.wsn(".")
  END;

  ORB.SetSymbolFileFinder(FindSymbolFile);
  Build;
  Generate;
END obuild.
