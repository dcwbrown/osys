MODULE obuild2;

(* Hosted Oberon build command                                                *)
(*                                                                            *)
(* Given a single module to build, works out what to compile to satisfy its   *)
(* imports, compiles them, and combines all objects into a single executable. *)

IMPORT
  SYSTEM, H := WinHost, K := Kernel, Files, Texts, Oberon, ORS, X64, ORG, ORP, WinPE, WinArgs;

TYPE
  ModuleName = ARRAY 1024 OF CHAR;
  PathName   = ARRAY H.MaxPath OF CHAR;

  Prefix = POINTER TO PrefixDesc;
  PrefixDesc = RECORD
    next:   Prefix;
    prefix: PathName
  END;

  Module = POINTER TO Module2Desc;
  Module2Desc = RECORD
    next:     Module;
    modname:  ModuleName;  (* Just the module name: no directory, no extension *)
    text:     Texts.Text;

    sourcefn:   PathName;  sourcefile: Files.File;  sourceprefix: Prefix;
    sourcetime: INTEGER;
    symfn:      PathName;  symfile:    Files.File;  symprefix:  Prefix;
    symtime:    INTEGER;   symkey:     INTEGER;
    codefn:     PathName;  codefile:   Files.File;  codeprefix: Prefix;
    codetime:   INTEGER;   codekey:    INTEGER;
  END;

  ModuleMaker = PROCEDURE (modname: ARRAY OF CHAR): Module;

  Import = POINTER TO Importdesc;
  Importdesc = RECORD name: ORS.Ident; next: Import END;

VAR
  Modulename:     ModuleName;
  SourcePrefix:   PathName;
  BinariesPrefix: PathName;
  OutputPrefix:   PathName;
  LoadFlags:      SET;
  SourcePrefices: Prefix;
  BinaryPrefices: Prefix;

  (* Log output control *)
  Verbose:         BOOLEAN;
  LongestModname:  INTEGER;
  LongestFilename: INTEGER;
  TitlePending:    BOOLEAN;

  Modules:        Module;
  Lastmodule:     Module;
  MakeModule:     ModuleMaker;


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

PROCEDURE WritePrefices(p: Prefix);
BEGIN
  WHILE p # NIL DO
    H.ws("  '");  H.ws(p.prefix);  H.wsn("'.");
    p := p.next
  END
END WritePrefices;

PROCEDURE WriteHuman(n, w: INTEGER);
BEGIN
  IF n < 1000 THEN
    H.wir(n, w)
  ELSE
    WriteHuman(n DIV 1000, w-4);  H.wc(",");  H.wiz(n MOD 1000, 3)
  END
END WriteHuman;

PROCEDURE Compile(module: Module);
VAR
  startTime:  INTEGER;
  endTime:    INTEGER;
  newsymbols: BOOLEAN;
  sym:        INTEGER;
  impname:    ORS.Ident;
  impmodule:  Module;
  import:     Import;
  imports:    Import;
  lastimport: Import;
BEGIN
  IF module.sourcefile = NIL THEN
    H.ws("Compile - missing source file for '"); H.ws(module.sourcefn); H.wsn("'.");
  (*
  ELSE
    H.ws("Compiling "); H.ws(module.sourcefile.name); H.wsn(".");
  *)
  END;
  ASSERT(module.sourcefile # NIL);

  (* Extract list of imported modules *)
  NEW(module.text);
  Texts.OpenFile(module.text, module.sourcefile);
  ORS.Init(module.text, 0);
  ORS.Get(sym);
  IF sym # ORS.module THEN ORS.Mark("does not start with MODULE.") END;
  ORS.Get(sym);
  IF sym # ORS.ident THEN ORS.Mark("expected module id."); END;
  IF ORS.id # module.modname THEN
    H.ws("File "); H.ws(module.sourcefn);
    H.wc("["); H.wi(ORS.linenum); H.wc(":"); H.wi(ORS.Pos() - ORS.linebeg); H.wc("]");
    H.ws(" module id '");                   H.ws(ORS.id);
    H.ws("' does not match expected id '"); H.ws(module.modname); H.wsn("'.");
    H.ExitProcess(99)
  END;
  ORS.Get(sym);
  IF sym # ORS.semicolon THEN ORS.Mark("expected semicolon following module id.") END;
  ORS.Get(sym);
  IF sym = ORS.import THEN
    REPEAT
      ORS.Get(sym);
      IF sym # ORS.ident THEN ORS.Mark("expected import module id (1).") END;
      impname := ORS.id;  ORS.Get(sym);
      IF sym = ORS.becomes THEN
        ORS.Get(sym);
        IF sym # ORS.ident THEN ORS.Mark("expected id (2).") END;
        impname := ORS.id;  ORS.Get(sym)
      END;
      IF impname # "SYSTEM" THEN
        NEW(import);  import.name := impname;
        IF imports = NIL THEN imports := import ELSE lastimport.next := import END;
        lastimport := import
      END;
    UNTIL sym # ORS.comma
  END;

  (* Make imported modules *)
  import := imports;
  WHILE import # NIL DO impmodule := MakeModule(import.name); import := import.next END;

  IF TitlePending THEN  (* Cannot write title until longest module and file names have been determined *)
    H.wsl("Module", LongestModname + 2);  H.wsl("File", LongestFilename + 2);
    H.wsn("        code         VAR    ms       ptrs   cmd  annot  export  import      heap");
    TitlePending := FALSE
  END;

  H.wsl(module.modname,  LongestModname + 2);
  H.ws(module.sourceprefix.prefix);  H.ws(module.sourcefn);
  H.wb(LongestFilename - (H.Length(module.sourceprefix.prefix) + H.Length(module.sourcefn)) + 2);
  startTime := H.Time();
  ORS.Init(module.text, 0);  (* Rewind text to beginning for full compilation *)
  newsymbols := TRUE;
  ORP.Module(module.sourcefile, newsymbols, module.codefile);

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


PROCEDURE MakeModuleProc(modname: ARRAY OF CHAR): Module;
(*  Returns an existing module record, or creates a new one and looks for both
    its source and its compiled binary files.
    Compiled binary files are only returned if they are up to date and their
    keys match.
*)
VAR
  module: Module;
  hdr:    H.CodeHeader;
  r:      Files.Rider;
  impmod: ARRAY 32 OF CHAR;
  key:    INTEGER;
  import: Module;
  prefix: Prefix;
BEGIN
  module := Modules;
  WHILE (module # NIL) & (modname # module.modname) DO module := module.next END;

  IF module = NIL THEN
    (*H.ws("Making module "); H.ws(modname); H.wsn(".");*)
    LongestModname := H.Max(LongestModname, H.Length(modname));
    NEW(module);
    Copy(modname, module.modname);
    module.symkey := 1;  module.codekey := 2;
    module.sourcetime := 0; module.symtime := 0; module.codetime := 0;

    Copy(modname, module.sourcefn);  H.Append(".mod", module.sourcefn);
    Open(module.sourcefn, SourcePrefices, module.sourcefile, module.sourceprefix);
    LongestFilename := H.Max(LongestFilename, H.Length(module.sourcefn) + H.Length(module.sourceprefix));

    IF module.sourcefile = NIL THEN
      H.ws("(couldn't open ");  H.ws(module.sourcefn);
      IF SourcePrefices # NIL THEN
        H.ws(" using prefices ");
        prefix := SourcePrefices;
        WHILE prefix # NIL DO
          H.ws("'"); H.ws(prefix.prefix); H.ws("' ");
          prefix := prefix.next
        END
      END;
      H.wsn(".)");
    END;

    IF module.sourcefile # NIL THEN
      module.sourcetime := Files.Date(module.sourcefile)
    END;

    Copy(modname, module.symfn);   H.Append(".smb",  module.symfn);
    Open(module.symfn, BinaryPrefices, module.symfile, module.symprefix);
    Copy(modname, module.codefn);  H.Append(".code", module.codefn);
    Open(module.codefn, BinaryPrefices, module.codefile, module.codeprefix);

    (*  Reject symbol and code file if any of:
        -  either is missing
        -  either is out of date
        -  the keys don't match
    *)
    IF module.symfile = NIL THEN module.codefile := NIL END;
    IF module.codefile # NIL THEN  (* Both symbol and code file exist *)
      module.symtime  := Files.Date(module.symfile);
      module.codetime := Files.Date(module.codefile);
      IF (module.symtime  < module.sourcetime)
      OR (module.codetime < module.sourcetime) THEN
        module.symfile := NIL;  module.codefile := NIL
      ELSE (* symbol and code file are up to date with source, or there is no source *)
        Files.Set(r, module.symfile,  8);   Files.ReadInt(r, module.symkey);
        Files.Set(r, module.codefile, 20H); Files.ReadInt(r, module.codekey);
        IF module.symkey # module.codekey THEN
          module.symfile := NIL;  module.codefile := NIL
        END
      END
    END;

    IF module.codefile # NIL THEN
      (*  Walk and make the code file imports and reject symbol and code file
          if any made key differs from the required key
      *)
      Files.Set(r, module.codefile, 0);  Files.ReadBytes(r, hdr, SYSTEM.SIZE(H.CodeHeader));
      Files.Set(r, module.codefile, hdr.imports);  Files.ReadString(r, impmod);
      WHILE (module.codefile # NIL) & (impmod[0] # 0X) DO
        Files.ReadInt(r, key);
        import := MakeModuleProc(impmod);
        IF import.codekey # key THEN
          (* This import has changed since module was compiled so we cannot use *)
          (* the pre-compiled symbol and code files.                            *)
          module.symfile := NIL;  module.codefile := NIL
        ELSE
          Files.ReadString(r, impmod)
        END;
      END
    END;

    (*  Compile source if we have been unable to use code or symbol files *)
    IF module.codefile = NIL THEN
      Compile(module)
    END;

    (* Link made module into list *)
    (*H.ws("Made ");  H.ws(module.modname); H.wsn(".");*)
    IF Modules = NIL THEN Modules := module ELSE Lastmodule.next := module END;
    Lastmodule := module;
  END
RETURN module END MakeModuleProc;


PROCEDURE Build;
VAR
  mod:    Module;
  PEname: PathName;
  start:  INTEGER;  (* Times *)
  end:    INTEGER;
BEGIN
  start := H.Time();
  mod := MakeModule("WinHost");
  mod := MakeModule("Kernel");
  mod := MakeModule(Modulename);
  mod := Modules;
  WHILE mod # NIL DO WinPE.AddModule(mod.codefile); mod := mod.next END;
  PEname := "";
  IF OutputPrefix # "" THEN H.Append(OutputPrefix, PEname) END;
  H.Append(Modulename, PEname);  H.Append(".exe", PEname);
  WinPE.Generate(PEname, LoadFlags);
  end := H.Time();
  H.ws("Build complete, "); H.wi((end - start) DIV 10000); H.wsn("ms.");
END Build;


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

  H.ws("obuild2 - Oberon command line builder");
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
  TitlePending    := TRUE;
  MakeModule      := MakeModuleProc;

  ScanArguments;

  ParsePrefices(SourcePrefix,   SourcePrefices);
  IF Verbose & (SourcePrefices # NIL) THEN
    H.wsn("Source prefixes:");  WritePrefices(SourcePrefices)
  END;
  ParsePrefices(BinariesPrefix, BinaryPrefices);
  IF Verbose & (BinaryPrefices # NIL) THEN
    H.wsn("Binary prefixes:");  WritePrefices(BinaryPrefices)
  END;

  Build;

  Oberon.GC
END obuild2.
