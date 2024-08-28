MODULE Link;  (* Create binary for a given module + optional command *)
(* DCWB 2023-04-14 .. 2024-05-25 *)

IMPORT SYSTEM, H := Host, Kernel, Files, Texts, Modules;

CONST
  Win = H.Win;
  Lin = H.Lin;

  MemoryAlignment = 1000H;   (* Sections are a multiple of this size in memory *)
  FileAlignment   =  200H;   (* Sections are a multiple of this size on file *)

  OberonCoreBase = 100000000H;  (* All linking is arranged to place the Oberon core at 1,0000,0000H *)

  Kernel32ImportCount = 41;
  Gdi32ImportCount    = 18;
  User32ImportCount   = 35;

TYPE
  EmbeddedModule = POINTER TO EmbeddedModuleDesc;
  EmbeddedModuleDesc = RECORD
    next: EmbeddedModule;
    mod:  Modules.ModDesc;
    file: Files.File;
    size: INTEGER
  END;

  Zeroes58 = ARRAY 3AH OF BYTE;

  U8  = BYTE;         U16 = SYSTEM.CARD16;  U32 = SYSTEM.CARD32;
  I8  = SYSTEM.INT8;  I16 = SYSTEM.INT16;   I32 = SYSTEM.INT32;   I64 = INTEGER;

  PEheader = RECORD
    eMagic:     U16;  (* 5AD4 *)
    zeroes:     Zeroes58;
    eLfanew:    U32;
    dosProgram: ARRAY 40H OF CHAR;
    signature:  U32;

    (* COFF file header*)
    machine:              U16;
    numberOfSections:     U16;
    timeDateStamp:        U32;
    pointerToSymbolTable: U32;
    numberOfSymbols:      U32;
    sizeOfOptionalHeader: U16;
    characteristics:      U16;

    (* PE32+ optional header *)
    pe32magic:               U16;
    majorLinkerVersion:      U8;  minorLinkerVersion:  U8;
    sizeOfCode:              U32;
    sizeOfInitializedData:   U32;
    sizeOfUninitializedData: U32;
    addressOfEntryPoint:     U32;
    baseOfCode:              U32;

    (* Windows specific PE32+ fields *)
    imageBase:             I64;
    MemoryAlignment:       U32;  fileAlignment:         U32;
    majorOSVersion:        U16;  minorOSVersion:        U16;
    majorImageVersion:     U16;  minorImageVersion:     U16;
    majorSubsystemVersion: U16;  minorSubsystemVersion: U16;
    win32VersionValue:     U32;
    sizeOfImage:           U32;  sizeOfHeaders:         U32;
    checksum:              U32;
    subsystem:             U16;
    dllCharacteristics:    U16;
    sizeOfStackReserve:    I64;  sizeOfStackCommit:     I64;
    sizeOfHeapReserve:     I64;  sizeOfHeapCommit:      I64;
    loaderFlags:           U32;
    numberOfRvaAndSizes:   U32;

    (* Optional header data directories *)
    exportTableRVA:           U32;  exportTableSize:           U32;
    importTableRVA:           U32;  importTableSize:           U32;
    resourceTableRVA:         U32;  resourceTableSize:         U32;
    exceptionTableRVA:        U32;  exceptionTableSize:        U32;
    certificateTableRVA:      U32;  certificateTableSize:      U32;
    baseRelocationTableRVA:   U32;  baseRelocationTableSize:   U32;
    debugRVA:                 U32;  debugSize:                 U32;
    architectureRVA:          U32;  architectureSize:          U32;
    globalPtrRVA:             U32;  globalPtrSize:             U32;
    tlsTableRVA:              U32;  tlsTableSize:              U32;
    loadConfigTableRVA:       U32;  loadConfigTableSize:       U32;
    boundImportRVA:           U32;  boundImportSize:           U32;
    IATRVA:                   U32;  IATSize:                   U32;
    delayImportDescriptorRVA: U32;  delayImportDescriptorSize: U32;
    CLRRuntimeHeaderRVA:      U32;  CLRRuntimeHeaderSize:      U32;
    reservedZeroRVA:          U32;  reservedZeroSize:          U32
  END;

  SectionHeader = RECORD
    Name:                 ARRAY 8 OF CHAR;
    VirtualSize:          U32;
    VirtualAddress:       U32;
    SizeOfRawData:        U32;
    PointerToRawData:     U32;
    PointerToRelocations: U32;
    PointerToLinenumbers: U32;
    NumberOfRelocations:  U32;
    Characteristics:      U32;
  END;

  ImportDirectoryTable = RECORD
    (* Import directory table entry for Kernel32 *)
    Kernel32LookupTable:  U32;   (*  0: RVA of table of import hint RVAs  *)
    Kernel32Datestamp:    U32;   (*  4: 0                                 *)
    Kernel32FwdChain:     U32;   (*  8: 0                                 *)
    Kernel32Dllnameadr:   U32;   (* 12: RVA of dll name                   *)
    Kernel32Target:       U32;   (* 16: Where to write imported addresses *)

    (* Import directory table entry for Gdi32 *)
    Gdi32LookupTable:     U32;   (*  0: RVA of table of import hint RVAs  *)
    Gdi32Datestamp:       U32;   (*  4: 0                                 *)
    Gdi32FwdChain:        U32;   (*  8: 0                                 *)
    Gdi32Dllnameadr:      U32;   (* 12: RVA of dll name                   *)
    Gdi32Target:          U32;   (* 16: Where to write imported addresses *)

    (* Import directory table entry for User32 *)
    User32LookupTable:    U32;   (*  0: RVA of table of import hint RVAs  *)
    User32Datestamp:      U32;   (*  4: 0                                 *)
    User32FwdChain:       U32;   (*  8: 0                                 *)
    User32Dllnameadr:     U32;   (* 12: RVA of dll name                   *)
    User32Target:         U32;   (* 16: Where to write imported addresses *)

    DirectoryEnd: ARRAY 5 OF U32;  (* Sentinel 0 filled directory table entry *)

    Kernel32Lookups: ARRAY Kernel32ImportCount + 1 OF I64; (* RVAs of Hints[] entry below *)
    Gdi32Lookups:    ARRAY Gdi32ImportCount    + 1 OF I64; (* RVAs of Hints[] entry below *)
    User32Lookups:   ARRAY User32ImportCount   + 1 OF I64; (* RVAs of Hints[] entry below *)

    Kernel32Dllname: ARRAY 14 OF CHAR;  (*  "kernel32.dll" *)
    Gdi32Dllname:    ARRAY 12 OF CHAR;  (*  "gdi32.dll"    *)
    User32Dllname:   ARRAY 12 OF CHAR;  (*  "user32.dll"   *)
  END;

  IHDR = RECORD  (* PNG file header *)
    width:       SYSTEM.CARD32;
    height:      SYSTEM.CARD32;
    bitdepth:    BYTE;
    coltype:     BYTE;
    compression: BYTE;
    filter:      BYTE;
    interlace:   BYTE
  END;

  BootstrapBuffer = RECORD
    Header:  Modules.ModDesc;
    Content: ARRAY 10000H OF BYTE
  END;


VAR
  W:            Texts.Writer;
  FileName:     ARRAY 512 OF CHAR;
  BinFile:      Files.File;
  Binary:       Files.Rider;

  Verbose:      BOOLEAN;

  FirstPreload: EmbeddedModule;
  LastPreload:  EmbeddedModule;
  PreloadSize:  INTEGER;

  (* Section ".idata" - imports from Windows OS *)
  Idt:            ImportDirectoryTable;
  ImportHints:    ARRAY 2000 OF BYTE;
  ImportHintSize: INTEGER;
  ImportSize:     INTEGER;  (* Directory table + hints rounded up to multiple of 16 *)
  HandlersOffset: INTEGER;  (* Offset of handlers table from start of Host.Mod vars *)

  (* Section "Oberon" - Inner core *)
  Core: RECORD
    Mem:    ARRAY 10000H OF BYTE;  (* Will be loaded at OberonCoreBase *)
    ModAdr: ARRAY 16 OF RECORD     (* Module name to base address map *)
      name: Modules.ModuleName;
      key:  INTEGER;
      adr:  INTEGER
    END;
    ModCount:   INTEGER;
    EntryPoint: INTEGER;  (* Relative to OberonCoreBase *)
    Size:       INTEGER;
    HostVars:   INTEGER;  (* Loaded address of vars relative to OberonCoreBase *)
  END;

  IconPng: Files.File;
  IconHdr: IHDR;



(* Convenience functions *)
PROCEDURE wn; BEGIN Texts.WriteLn(W); Texts.Append(Texts.Log, W.buf) END wn;

PROCEDURE wc (c: CHAR);          BEGIN Texts.Write(W, c)           END wc;
PROCEDURE ws (s: ARRAY OF CHAR); BEGIN Texts.WriteString(W, s)     END ws;
PROCEDURE wsn(s: ARRAY OF CHAR); BEGIN Texts.WriteString(W, s); wn END wsn;
PROCEDURE wh (i: INTEGER);       BEGIN Texts.WriteHex(W, i)        END wh;
PROCEDURE wi (i: INTEGER);       BEGIN Texts.WriteInt(W, i, 1)     END wi;
PROCEDURE wir(i, n: INTEGER);    BEGIN Texts.WriteInt(W, i, n)     END wir;



(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Align(a: INTEGER;  align: INTEGER): INTEGER;
BEGIN IF a > 0 THEN INC(a, align - 1) END;
RETURN a DIV align * align END Align;

PROCEDURE MemAlign(a: INTEGER): INTEGER;
RETURN Align(a, MemoryAlignment) END MemAlign;

PROCEDURE PageAlign(a: INTEGER): INTEGER;
RETURN Align(a, 10000H) END PageAlign;

PROCEDURE FileAlign(a: INTEGER): INTEGER;
RETURN Align(a, FileAlignment) END FileAlign;

PROCEDURE IsInnerCore(name: ARRAY OF CHAR): BOOLEAN;
RETURN (name = "Host")  OR (name = "Kernel")
    OR (name = "Files") OR (name = "Modules")
END IsInnerCore;

PROCEDURE SetFilePos(VAR r: Files.Rider; f: Files.File; pos: INTEGER);
(* If pos beyond end of file fills gap with zeroes *)
VAR l: INTEGER;
BEGIN
  l := Files.Length(f);
  IF pos <= l THEN
    Files.Set(r, f, pos)
  ELSE
    Files.Set(r, f, l);
    l := pos - l;
    WHILE l > 0 DO Files.WriteByte(r, 0);  DEC(l) END
  END
END SetFilePos;


(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* PE Header *)

PROCEDURE WriteSectionHeader(name:         ARRAY OF CHAR;
                             vsize, fsize: INTEGER;
                             rva,   fadr:  INTEGER;
                             flags:        INTEGER);
VAR
  shdr: SectionHeader;

BEGIN
  H.ZeroFill(shdr);
  shdr.Name             := name;
  shdr.VirtualSize      := vsize;
  shdr.VirtualAddress   := rva;
  shdr.SizeOfRawData    := fsize;
  shdr.PointerToRawData := fadr;
  shdr.Characteristics  := flags;
  Files.WriteRaw(Binary, shdr);
END WriteSectionHeader;


PROCEDURE WritePEHeader(ImageBase,   HeaderSize,  numsections,
                        FadrPreload, RvaPreload,
                        FadrIcon,    RvaIcon,
                        FadrImport,  RvaImport,
                        FadrModules, RvaModules: INTEGER);
CONST
  (* Section flags *)
  SWriteable     = 80000000H;
  SReadable      = 40000000H;
  SExecutable    = 20000000H;
  SUninitialised =       80H;
  SInitialised   =       40H;
  SCode          =       20H;

VAR
  hdr: PEheader;

BEGIN
  H.ZeroFill(hdr);

  (* MSDOS stub *)
  hdr.eMagic               := 5A4DH;
  hdr.eLfanew              := 128;
  hdr.dosProgram           := $ 0E 1F BA 0E 00 B4 09 CD  21 B8 01 4C CD 21 54 68
                                69 73 20 70 72 6F 67 72  61 6D 20 63 61 6E 6E 6F
                                74 20 20 72 75 6E 20 69  6E 20 44 4F 53 20 6D 6F
                                64 65 2E 0D 0A 24 $;
  hdr.signature            := 4550H;

  (* COFF file header*)
  hdr.machine              := 8664H;  (* AMD64/Intel 64 *)
  hdr.numberOfSections     := numsections;
  hdr.sizeOfOptionalHeader := 240;
  hdr.characteristics      := 200H  (* Windows debug information stripped *)
                            + 20H   (* Large address aware                *)
                            + 8     (* Coff symbol tables removed         *)
                            + 4     (* Coff linenumbers removed           *)
                            + 2     (* Executable image                   *)
                            + 1;    (* Relocs stripped *)

  (* PE32+ optional header *)
  hdr.pe32magic               := 20BH;  (* PE32+ *)
  hdr.majorLinkerVersion      := 1;
  hdr.minorLinkerVersion      := 49H;
  hdr.sizeOfCode              := PageAlign(Core.Size);
  hdr.sizeOfInitializedData   := RvaModules - RvaPreload;
  hdr.sizeOfUninitializedData := 0;
  hdr.addressOfEntryPoint     := RvaModules + Core.EntryPoint;
  hdr.baseOfCode              := RvaModules;

  (* Windows specific PE32+ fields *)
  hdr.imageBase               := ImageBase;
  hdr.MemoryAlignment         := MemoryAlignment;
  hdr.fileAlignment           := FileAlignment;
  hdr.majorOSVersion          := 1;
  hdr.majorSubsystemVersion   := 5;
  hdr.sizeOfImage             := RvaModules + PageAlign(Core.Size); (* + 1000H;*) (*Align(ModuleSize, MemoryAlignment);*)
  hdr.sizeOfHeaders           := HeaderSize;
  hdr.subsystem               := 3;      (* Console *)
(*hdr.subsystem               := 2;*)    (* Windows *)
  hdr.dllCharacteristics      := 400H;   (* No SEH *)
  hdr.sizeOfStackReserve      := 1000H;
  hdr.sizeOfStackCommit       := 1000H;
  hdr.sizeOfHeapReserve       := 1000H;  (* Minimal heap - Windows may use it, we don't *)
  hdr.numberOfRvaAndSizes     := 16;

  (* Optional header data directories *)
  IF IconPng # NIL THEN
    hdr.resourceTableRVA      := RvaIcon;
    hdr.resourceTableSize     := RvaImport - RvaIcon;
  END;
  hdr.importTableRVA          := RvaImport;
  hdr.importTableSize         := RvaModules - RvaImport;

  SetFilePos(Binary, BinFile, 0);
  Files.WriteRaw(Binary, hdr);

  (* Write section headers *)
  IF FadrIcon > FadrPreload THEN
    WriteSectionHeader("Preload",
                     RvaIcon  - RvaPreload,    (* Size in memory *)
                     FadrIcon - FadrPreload,   (* Size on disk   *)
                     RvaPreload,
                     FadrPreload,
                     SReadable + SInitialised);
  END;

  IF FadrImport > FadrIcon THEN
    WriteSectionHeader("Icon",
                     RvaImport  - RvaIcon,    (* Size in memory *)
                     FadrImport - FadrIcon,   (* Size on disk   *)
                     RvaIcon,
                     FadrIcon,
                     SReadable + SInitialised);
  END;

  WriteSectionHeader("Imports",
                     RvaModules - RvaImport,    (* Section size in memory *)
                     FileAlign(ImportSize),     (* Size on disk *)
                     RvaImport,
                     FadrImport,
                     SReadable + SWriteable + SInitialised);

  WriteSectionHeader("Core",
                     PageAlign(Core.Size),      (* Reserved size in memory *)
                     FileAlign(Core.Size),      (* Size on disk *)
                     RvaModules,
                     FadrModules,
                     SReadable + SWriteable + SExecutable + SCode);

  ASSERT(Files.Pos(Binary) <= HeaderSize);

END WritePEHeader;


(* --------------------------- Inner core linking --------------------------- *)

PROCEDURE ExportedOffset(modhdr: INTEGER; index: INTEGER): INTEGER;
VAR exportoffset: SYSTEM.CARD32;  mod: Modules.Module;
BEGIN
  mod := SYSTEM.VAL(Modules.Module, SYSTEM.ADR(Core.Mem[modhdr]));
  SYSTEM.GET(ORD(mod) + mod.exports + index * 4, exportoffset);
RETURN exportoffset END ExportedOffset;


PROCEDURE LinkImport*(
  modofs:       INTEGER;  (* Module offset of importing module relative to Core.Mem/OberonCoreBase *)
  offset:       INTEGER;  (* Offset within importing module of disp/abs value being updated *)
  impno, modno: INTEGER;
  modules:      ARRAY OF INTEGER
);
VAR
  disp:           SYSTEM.INT32;
  Coremodadr:     INTEGER;
  expoffset:      INTEGER;
  absreloc:       INTEGER;
  handlersoffset: INTEGER;
  winhost:        Modules.Module;
BEGIN
  winhost := SYSTEM.VAL(Modules.Module, SYSTEM.ADR(Core.Mem));
  Coremodadr  := SYSTEM.ADR(Core.Mem[modofs]);
  IF modno = 0 THEN  (* system function *)
    SYSTEM.GET(Coremodadr + offset, disp);
    ASSERT((impno >= 0) & (impno <= H.HandlerCount));

    handlersoffset := winhost.vars - OberonCoreBase  (* relative to start of modules *)
                    + HandlersOffset;

    INC(disp, handlersoffset + 8 * impno - modofs);
    SYSTEM.PUT(Coremodadr + offset, disp)
  ELSIF modno = 0FFFFH THEN (* 64 bit absolute address relocation *)
    (* qword at offset contains 32/0,32/module offset or 32/1,16/mod,16/imp *)
    SYSTEM.GET(Coremodadr + offset, absreloc);
    IF absreloc DIV 100000000H = 0 THEN  (* offset in this module *)
      SYSTEM.PUT(Coremodadr + offset, OberonCoreBase + modofs + absreloc)
    ELSE  (* import reference from another module *)
      modno := absreloc DIV 10000H MOD 10000H;  ASSERT(modno > 0);
      impno := absreloc MOD 10000H;
      ASSERT(impno > 0);
      SYSTEM.PUT(Coremodadr + offset, OberonCoreBase + modules[modno-1]
                                  + ExportedOffset(modules[modno-1], impno-1))
    END
  ELSE
    ASSERT(modno > 0);
    SYSTEM.GET(Coremodadr + offset, disp);
    expoffset := ExportedOffset(modules[modno-1], impno-1);
    INC(disp, modules[modno-1] - modofs + expoffset);
    SYSTEM.PUT(Coremodadr + offset, disp)
  END
END LinkImport;


PROCEDURE LoadModule(fn: ARRAY OF CHAR): Modules.Module;
VAR
  f:       Files.File;
  mod:     Modules.Module;
  R:       Files.Rider;
  i, m:    INTEGER;
  modules: ARRAY 32 OF INTEGER;  (* Offset into Core.Mem of start of module *)
  impcnt:  SYSTEM.CARD32;
  impname: Modules.ModuleName;
  impkey:  INTEGER;
  modnum:  INTEGER;
  offset:  SYSTEM.CARD32;
  impno:   SYSTEM.CARD16;
  modno:   SYSTEM.CARD16;
  ptradr:  INTEGER;
  ptroff:  INTEGER;
  (* debug logging only *)
  mod2:    Modules.Module;
BEGIN
  f := Files.Old(fn);
  IF f = NIL THEN ws("Cannot load ");  ws(fn);  wn; ASSERT(FALSE) END;
  SetFilePos(R, f, 0);
  mod := SYSTEM.VAL(Modules.Module, SYSTEM.ADR(Core.Mem[Core.Size]));
  Files.ReadRaw(R, mod^);
  Core.ModAdr[Core.ModCount].name := mod.name;
  Core.ModAdr[Core.ModCount].key  := mod.key;
  Core.ModAdr[Core.ModCount].adr  := Core.Size;  (* Offset into Core.Mem = relative to OberonCoreBase *)
  m := Core.Size + SYSTEM.SIZE(Modules.ModDesc);
  FOR i := SYSTEM.SIZE(Modules.ModDesc) + 1 TO mod.vars DO
    Files.ReadByte(R, Core.Mem[m]);  INC(m)
  END;
  FOR i := 1 TO mod.varsize DO Core.Mem[m] := 0;  INC(m) END;

  (* Extract list of imported modules from loaded code *)
  m := Core.Size + SYSTEM.SIZE(Modules.ModDesc);
  impcnt := 0;
  REPEAT
    i := 0;
    REPEAT impname[i] := CHR(Core.Mem[m]); INC(i); INC(m) UNTIL impname[i-1] = 0X;
    IF impname[0] # 0X THEN
      SYSTEM.GET(SYSTEM.ADR(Core.Mem[m]), impkey); INC(m, 8);
      (* Find loaded module number for this import *)
      modnum := 0;
      WHILE (modnum < Core.ModCount) & (Core.ModAdr[modnum].name # impname) DO INC(modnum) END;
      ASSERT(modnum < Core.ModCount);
      ASSERT(Core.ModAdr[modnum].key = impkey);
      modules[impcnt] := Core.ModAdr[modnum].adr
    END;
    INC(impcnt)
  UNTIL impname[0] = 0X;

(* TODO - Implement module ref counts *)

  (* Link imports *)
  SetFilePos(R, f, mod.imprefs);
  Files.ReadRaw(R, impcnt);
  FOR i := 1 TO impcnt DO
    Files.ReadRaw(R, offset);
    Files.ReadRaw(R, impno);
    Files.ReadRaw(R, modno);
    LinkImport(Core.Size, offset, impno, modno, modules)
  END;

  (* Relocate pointer addresses *)
  ptradr := ORD(mod) + mod.ptr;  (* Address witihin Core.Mem *)
  SYSTEM.GET(ptradr, ptroff);
  WHILE ptroff >= 0 DO
    SYSTEM.PUT(ptradr, OberonCoreBase + Core.Size + mod.vars + ptroff);
    INC(ptradr, 8);
    SYSTEM.GET(ptradr, ptroff)
  END;

  mod.refcnt := 1;
  INC(mod.cmd, OberonCoreBase + Core.Size);
  mod.size := (mod.vars + mod.varsize + 15) DIV 16 * 16;
  INC(mod.vars, OberonCoreBase + Core.Size);
  INC(Core.Size, mod.size);
  INC(Core.ModCount)
RETURN mod END LoadModule;


PROCEDURE LinkInnerCore;
VAR
  HostHdr:    Modules.Module;
  KernelHdr:  Modules.Module;
  FilesHdr:   Modules.Module;
  ModulesHdr: Modules.Module;
BEGIN
  Core.ModCount   := 0;
  Core.Size       := 0;
  Core.EntryPoint := 0;

  HostHdr    := LoadModule("Host.x64");
  KernelHdr  := LoadModule("Kernel.x64");
  FilesHdr   := LoadModule("Files.x64");
  ModulesHdr := LoadModule("Modules.x64");

  HostHdr.next := SYSTEM.VAL(Modules.Module, OberonCoreBase + ORD(KernelHdr)  - SYSTEM.ADR(Core.Mem));
  KernelHdr.next  := SYSTEM.VAL(Modules.Module, OberonCoreBase + ORD(FilesHdr)   - SYSTEM.ADR(Core.Mem));
  FilesHdr.next   := SYSTEM.VAL(Modules.Module, OberonCoreBase + ORD(ModulesHdr) - SYSTEM.ADR(Core.Mem));
  ModulesHdr.next := NIL;

  Core.EntryPoint := ORD(ModulesHdr) - SYSTEM.ADR(Core.Mem) + ModulesHdr.init;
  Core.HostVars   := HostHdr.vars - OberonCoreBase;

END LinkInnerCore;


(* ---------------- Windows .idata import table construction ---------------- *)

PROCEDURE BuildImportTables(RvaImport: INTEGER);
TYPE State = RECORD n, i, RvaImport, dll: INTEGER END;
VAR s: State;

  PROCEDURE FieldRVA(VAR field: ARRAY OF BYTE): U32;
  BEGIN RETURN SYSTEM.ADR(field) - SYSTEM.ADR(Idt) END FieldRVA;

  PROCEDURE AddProc(VAR hints: ARRAY OF BYTE; VAR i: INTEGER; name: ARRAY OF CHAR);
  VAR n: INTEGER;
  BEGIN
    n := 0; REPEAT hints[i] := ORD(name[n]);  INC(i);  INC(n) UNTIL name[n-1] = 0X;
  END AddProc;

  PROCEDURE AddImport(VAR lookups: ARRAY OF I64; VAR s: State;
                      name: ARRAY OF CHAR);
  BEGIN
    lookups[s.n] := s.RvaImport + SYSTEM.SIZE(ImportDirectoryTable) + s.i;
    INC(s.n);
    ASSERT(s.dll < 256);
    ASSERT(s.i + H.Length(name) + 3 < LEN(ImportHints));
    ImportHints[s.i] := s.dll;  ImportHints[s.i+1] := 0;  INC(s.i, 2);
    AddProc(ImportHints, s.i, name);
  END AddImport;

BEGIN
  H.ZeroFill(Idt);

  (* **NOTE** these imports must be in exactly the same order as the *)
  (* corresponding procedure variable declarations in Host.Mod.       *)

  Idt.Kernel32LookupTable := RvaImport + FieldRVA(Idt.Kernel32Lookups);
  Idt.Kernel32Dllnameadr  := RvaImport + FieldRVA(Idt.Kernel32Dllname);
  Idt.Kernel32Dllname     := "KERNEL32.DLL";
  s.n := 0;  s.i := 0;  s.dll := 0;  s.RvaImport := RvaImport;
  AddImport(Idt.Kernel32Lookups, s, "AddVectoredExceptionHandler");
  AddImport(Idt.Kernel32Lookups, s, "CloseHandle");
  AddImport(Idt.Kernel32Lookups, s, "CreateFileW");
  AddImport(Idt.Kernel32Lookups, s, "CreateProcessW");
  AddImport(Idt.Kernel32Lookups, s, "DeleteFileW");
  AddImport(Idt.Kernel32Lookups, s, "ExitProcess");
  AddImport(Idt.Kernel32Lookups, s, "FileTimeToLocalFileTime");
  AddImport(Idt.Kernel32Lookups, s, "FileTimeToSystemTime");
  AddImport(Idt.Kernel32Lookups, s, "FindClose");
  AddImport(Idt.Kernel32Lookups, s, "FindFirstFileExW");
  AddImport(Idt.Kernel32Lookups, s, "FindNextFileW");
  AddImport(Idt.Kernel32Lookups, s, "FlushFileBuffers");
  AddImport(Idt.Kernel32Lookups, s, "FormatMessageW");
  AddImport(Idt.Kernel32Lookups, s, "GetCommandLineW");
  AddImport(Idt.Kernel32Lookups, s, "GetCurrentDirectoryW");
  AddImport(Idt.Kernel32Lookups, s, "GetCurrentProcessId");
  AddImport(Idt.Kernel32Lookups, s, "GetEnvironmentVariableW");
  AddImport(Idt.Kernel32Lookups, s, "GetFinalPathNameByHandleW");
  AddImport(Idt.Kernel32Lookups, s, "GetFileAttributesExW");
  AddImport(Idt.Kernel32Lookups, s, "GetFileAttributesW");
  AddImport(Idt.Kernel32Lookups, s, "GetFileInformationByHandleEx");
  AddImport(Idt.Kernel32Lookups, s, "GetFileSizeEx");
  AddImport(Idt.Kernel32Lookups, s, "GetLastError");
  AddImport(Idt.Kernel32Lookups, s, "GetModuleFileNameW");
  AddImport(Idt.Kernel32Lookups, s, "GetProcAddress");
  AddImport(Idt.Kernel32Lookups, s, "GetStdHandle");
  AddImport(Idt.Kernel32Lookups, s, "GetSystemTimePreciseAsFileTime");
  AddImport(Idt.Kernel32Lookups, s, "GetTempFileNameA");
  AddImport(Idt.Kernel32Lookups, s, "GetTempPathA");
  AddImport(Idt.Kernel32Lookups, s, "LoadLibraryA");
  AddImport(Idt.Kernel32Lookups, s, "MoveFileExW");
  AddImport(Idt.Kernel32Lookups, s, "ReadFile");
  AddImport(Idt.Kernel32Lookups, s, "SetConsoleOutputCP");
  AddImport(Idt.Kernel32Lookups, s, "SetEndOfFile");
  AddImport(Idt.Kernel32Lookups, s, "SetFileInformationByHandle");
  AddImport(Idt.Kernel32Lookups, s, "SetFilePointerEx");
  AddImport(Idt.Kernel32Lookups, s, "Sleep");
  AddImport(Idt.Kernel32Lookups, s, "UnmapViewOfFile");
  AddImport(Idt.Kernel32Lookups, s, "VirtualAlloc");
  AddImport(Idt.Kernel32Lookups, s, "VirtualQuery");
  AddImport(Idt.Kernel32Lookups, s, "WriteFile");
  ASSERT(s.n = Kernel32ImportCount);

  Idt.Gdi32LookupTable := RvaImport + FieldRVA(Idt.Gdi32Lookups);
  Idt.Gdi32Dllnameadr  := RvaImport + FieldRVA(Idt.Gdi32Dllname);
  Idt.Gdi32Dllname     := "GDI32.DLL";
  s.n := 0;  INC(s.dll);
  AddImport(Idt.Gdi32Lookups, s, "BitBlt");
  AddImport(Idt.Gdi32Lookups, s, "CreateBitmap");
  AddImport(Idt.Gdi32Lookups, s, "CreateCompatibleDC");
  AddImport(Idt.Gdi32Lookups, s, "CreateDCA");
  AddImport(Idt.Gdi32Lookups, s, "CreateDIBSection");
  AddImport(Idt.Gdi32Lookups, s, "CreateFontA");
  AddImport(Idt.Gdi32Lookups, s, "CreatePen");
  AddImport(Idt.Gdi32Lookups, s, "CreateRectRgn");
  AddImport(Idt.Gdi32Lookups, s, "DeleteObject");
  AddImport(Idt.Gdi32Lookups, s, "GetGlyphOutlineW");
  AddImport(Idt.Gdi32Lookups, s, "GetCharABCWidthsW");
  AddImport(Idt.Gdi32Lookups, s, "GetDeviceCaps");
  AddImport(Idt.Gdi32Lookups, s, "GetOutlineTextMetricsW");
  AddImport(Idt.Gdi32Lookups, s, "GetStockObject");
  AddImport(Idt.Gdi32Lookups, s, "Rectangle");
  AddImport(Idt.Gdi32Lookups, s, "SelectObject");
  AddImport(Idt.Gdi32Lookups, s, "SetDCPenColor");
  AddImport(Idt.Gdi32Lookups, s, "SetDCBrushColor");
  ASSERT(s.n = Gdi32ImportCount);

  Idt.User32LookupTable := RvaImport + FieldRVA(Idt.User32Lookups);
  Idt.User32Dllnameadr  := RvaImport + FieldRVA(Idt.User32Dllname);
  Idt.User32Dllname     := "USER32.DLL";
  s.n := 0;  INC(s.dll);
  AddImport(Idt.User32Lookups, s, "AdjustWindowRectEx");
  AddImport(Idt.User32Lookups, s, "BeginPaint");
  AddImport(Idt.User32Lookups, s, "CreateIconFromResourceEx");
  AddImport(Idt.User32Lookups, s, "CreateWindowExW");
  AddImport(Idt.User32Lookups, s, "DefWindowProcW");
  AddImport(Idt.User32Lookups, s, "DispatchMessageW");
  AddImport(Idt.User32Lookups, s, "EndPaint");
  AddImport(Idt.User32Lookups, s, "GetClientRect");
  AddImport(Idt.User32Lookups, s, "GetClipboardFormatNameW");
  AddImport(Idt.User32Lookups, s, "GetCursorPos");
  AddImport(Idt.User32Lookups, s, "GetDCEx");
  AddImport(Idt.User32Lookups, s, "GetDpiForWindow");
  AddImport(Idt.User32Lookups, s, "GetMessageW");
  AddImport(Idt.User32Lookups, s, "GetQueueStatus");
  AddImport(Idt.User32Lookups, s, "GetWindowDC");
  AddImport(Idt.User32Lookups, s, "GetWindowRect");
  AddImport(Idt.User32Lookups, s, "LoadCursorW");
  AddImport(Idt.User32Lookups, s, "MessageBoxA");
  AddImport(Idt.User32Lookups, s, "MessageBoxW");
  AddImport(Idt.User32Lookups, s, "MoveWindow");
  AddImport(Idt.User32Lookups, s, "MsgWaitForMultipleObjects");
  AddImport(Idt.User32Lookups, s, "PeekMessageW");
  AddImport(Idt.User32Lookups, s, "PostMessageW");
  AddImport(Idt.User32Lookups, s, "PostQuitMessage");
  AddImport(Idt.User32Lookups, s, "RegisterClassExW");
  AddImport(Idt.User32Lookups, s, "ReleaseCapture");
  AddImport(Idt.User32Lookups, s, "ReleaseDC");
  AddImport(Idt.User32Lookups, s, "SetCapture");
  AddImport(Idt.User32Lookups, s, "SetProcessDpiAwarenessContext");
  AddImport(Idt.User32Lookups, s, "SetWindowRgn");
  AddImport(Idt.User32Lookups, s, "ShowCursor");
  AddImport(Idt.User32Lookups, s, "ShowWindow");
  AddImport(Idt.User32Lookups, s, "TrackMouseEvent");
  AddImport(Idt.User32Lookups, s, "TranslateMessage");
  AddImport(Idt.User32Lookups, s, "InvalidateRect");
  ASSERT(s.n = User32ImportCount);
  ImportHintSize := s.i;

  ImportSize := Align(SYSTEM.SIZE(ImportDirectoryTable) + ImportHintSize, 16);
END BuildImportTables;


PROCEDURE PresetWinHost(FadrModules, ImageBase, MadrPreload, IconSize, RvaIcon: INTEGER);
VAR
  varpos:      INTEGER;
  preloadvars: H.PreLoadVars;
BEGIN
  varpos := Core.HostVars;

  H.ZeroFill(preloadvars);
  preloadvars.Exeadr      := ImageBase;
  preloadvars.CoreAdr     := OberonCoreBase;
  preloadvars.CoreSize    := Core.Size;
  preloadvars.MadrPreload := MadrPreload;
  IF IconSize > 0 THEN
    preloadvars.MadrIcon  := ImageBase + RvaIcon + 0B4H;  (* See MakeIconResource for 0B4H *)
    preloadvars.IconSize  := IconSize
  END;

  SYSTEM.COPY(SYSTEM.ADR(preloadvars), SYSTEM.ADR(Core.Mem[varpos]), SYSTEM.SIZE(H.PreLoadVars));
  INC(varpos, SYSTEM.SIZE(H.PreLoadVars));
  ASSERT(varpos = Core.HostVars + SYSTEM.SIZE(H.PreLoadVars));

  SYSTEM.COPY(SYSTEM.ADR(Idt.Kernel32Lookups), SYSTEM.ADR(Core.Mem[varpos]), Kernel32ImportCount * 8);
  INC(varpos, Kernel32ImportCount * 8);

  SYSTEM.COPY(SYSTEM.ADR(Idt.User32Lookups), SYSTEM.ADR(Core.Mem[varpos]), User32ImportCount * 8);
  INC(varpos, User32ImportCount * 8);

  SYSTEM.COPY(SYSTEM.ADR(Idt.Gdi32Lookups), SYSTEM.ADR(Core.Mem[varpos]), Gdi32ImportCount * 8);
END PresetWinHost;


PROCEDURE CopyFile(f: Files.File; VAR r2: Files.Rider; size: INTEGER);
VAR
  buf: ARRAY 1024 OF BYTE;
  i:   INTEGER;
  r1:  Files.Rider;
BEGIN
  SetFilePos(r1, f, 0);
  i := size DIV LEN(buf);
  WHILE i > 0 DO
    Files.ReadRaw(r1, buf);
    Files.WriteRaw(r2, buf);
    DEC(i)
  END;
  i := size MOD LEN(buf);
  IF i > 0 THEN
    Files.ReadMem(r1, SYSTEM.ADR(buf), i);
    Files.WriteMem(r2, SYSTEM.ADR(buf), i)
  END
END CopyFile;


(* ---------------- Windows PE resource section construction ---------------- *)

PROCEDURE WriteWord(i: INTEGER);
BEGIN Files.WriteMem(Binary, SYSTEM.ADR(i), 2) END WriteWord;

PROCEDURE WriteDWord(i: INTEGER);
BEGIN Files.WriteMem(Binary, SYSTEM.ADR(i), 4) END WriteDWord;

PROCEDURE WriteResourceDirectoryTable(numids: INTEGER);  (* 16 bytes *)
VAR i: INTEGER;
BEGIN
  Files.WriteInt(Binary, 0);  (* characteristics and timestamp *)
  WriteDWord(0);           (* version *)
  WriteWord(0);            (* # name entries *)
  WriteWord(numids);       (* # id entries *)
END WriteResourceDirectoryTable;

PROCEDURE WriteResourceDirectoryEntry(id, offset: INTEGER); (* 8 bytes *)
BEGIN
  WriteDWord(id);
  WriteDWord(offset)
END WriteResourceDirectoryEntry;

PROCEDURE WriteResourceDataEntry(rva, size: INTEGER);  (* 16 bytes *)
BEGIN
  WriteDWord(rva);
  WriteDWord(size);
  Files.WriteInt(Binary, 1033);  (* codepage and 0 (reserved) *)
END WriteResourceDataEntry;

PROCEDURE WriteICONDIR(count: INTEGER); (* 6 bytes *)
VAR i: INTEGER;
BEGIN
  WriteWord(0);     (* reserved *)
  WriteWord(1);     (* type     *)
  WriteWord(count); (* # icons  *)
END WriteICONDIR;

PROCEDURE WriteIconDirEntry(width, height, size, index: INTEGER); (* 16 bytes *)
VAR i: INTEGER;
BEGIN
  Files.WriteByte(Binary, width  MOD 256);
  Files.WriteByte(Binary, height MOD 256);
  WriteWord(0);  (* no colour palette, reserved *)
  WriteWord(1);  (* colour planes *)
  WriteWord(8);  (* bits per pixel *)
  WriteDWord(size);
  WriteDWord(index)
END WriteIconDirEntry;

PROCEDURE WriteRESDIR(width, height, size, ordinal: INTEGER); (* 14 bytes *)
VAR i: INTEGER;
BEGIN
  Files.WriteByte(Binary, width  MOD 256);
  Files.WriteByte(Binary, height MOD 256);
  Files.WriteByte(Binary, 0);  (* no colour palette *)
  Files.WriteByte(Binary, 0);  (* reserved *)
  WriteWord(1);  (* colour planes *)
  WriteWord(8);  (* bits per pixel *)
  WriteDWord(size);
  WriteWord(ordinal)
END WriteRESDIR;

PROCEDURE MakeIconResource(fadr, rva: INTEGER): INTEGER;
VAR size: INTEGER;
BEGIN
  size := Files.Length(IconPng);
  SetFilePos(Binary, BinFile, fadr);
  (*00*) WriteResourceDirectoryTable(2);
  (*10*) WriteResourceDirectoryEntry(3,    80000020H); (* -> 2d lev for icons *)
  (*18*) WriteResourceDirectoryEntry(14,   80000038H); (* -> 2d lev for icon group *)
  (*20*) WriteResourceDirectoryTable(1);
  (*30*) WriteResourceDirectoryEntry(1,    80000050H); (* -> lang table for icon *)
  (*38*) WriteResourceDirectoryTable(1);
  (*48*) WriteResourceDirectoryEntry(1,    80000068H); (* -> lang table for icon group *)
  (*50*) WriteResourceDirectoryTable(1);
  (*60*) WriteResourceDirectoryEntry(1033,       80H); (* -> resource data entry for icon *)
  (*68*) WriteResourceDirectoryTable(1);
  (*78*) WriteResourceDirectoryEntry(1,          90H); (* -> resource data entry for icon group *)
  (*80*) WriteResourceDataEntry(rva + 0B4H, size + 0B4H);  (* Icon data at 0B4H *)
  (*90*) WriteResourceDataEntry(rva + 0A0H, 20);           (* Icon group at 0A0H *)
  (*A0*) WriteICONDIR(1);  (* 1 icon *)
  (*A6*) WriteRESDIR(256, 256, size, 1);
  (*B4*) CopyFile(IconPng, Binary, size);
RETURN Files.Pos(Binary) - fadr END MakeIconResource;


(* -------------------- Check for icon file <target>.png -------------------- *)

PROCEDURE SwapDW(x: INTEGER): INTEGER;
RETURN
    x              MOD 100H * 1000000H
  + x DIV 100H     MOD 100H * 10000H
  + x DIV 10000H   MOD 100H * 100H
  + x DIV 1000000H MOD 100H
END SwapDW;

PROCEDURE LoadPngHeader*(f: Files.File; VAR hdr: IHDR);
VAR
  r:   Files.Rider;
  i:   INTEGER;
  len: SYSTEM.CARD32;
  typ: ARRAY 5 OF CHAR;
  end: BOOLEAN;
BEGIN
  H.ZeroFill(hdr);  hdr.width := 0FFFFFFFFH;  (* Mark as not found *)
  SetFilePos(r, f, 0);
  Files.ReadInt(r, i);  end := r.res # 0;
  IF ~end THEN end := i # 0A1A0A0D474E5089H END;
  IF ~end THEN
    REPEAT
      Files.ReadRaw(r, len);  end := r.res # 0;
      IF ~end THEN len := SwapDW(len);
      Files.ReadMem(r, SYSTEM.ADR(typ), 4);  typ[4] := 0X;  end := r.res # 0 END;
      IF ~end THEN end := typ = "IEND" END;
      IF ~end THEN
        IF (typ = "IHDR") & (len = SYSTEM.SIZE(IHDR)) THEN
          Files.ReadRaw(r, hdr);
          IF r.res = 0 THEN
            hdr.width  := SwapDW(hdr.width);  hdr.height := SwapDW(hdr.height)
          ELSE
            hdr.width := 0FFFFFFFFH   (* Mark as not found *)
          END;
          end := TRUE
        ELSE
          SetFilePos(r, f, Files.Pos(r) + len + 4);  (* skip content and checksum *)
        END
      END
    UNTIL end
  END
END LoadPngHeader;


PROCEDURE CheckIcon(target: ARRAY OF CHAR);
VAR fn: ARRAY 64 OF CHAR;
BEGIN
  fn := target;  H.Append(".png", fn);  IconPng := Files.Old(fn);
  IF IconPng # NIL THEN
    LoadPngHeader(IconPng, IconHdr);
    IF (IconHdr.width       # 256)  OR (IconHdr.height  # 256)
    OR (IconHdr.bitdepth    # 8)    OR (IconHdr.coltype # 6)
    OR (IconHdr.compression # 0) THEN
      IconPng := NIL;
      ws("(icon png '"); ws(fn); wsn("' unsuitable.)")
    END
  END
END CheckIcon;


(* ------------------------ Write preload to binary ------------------------- *)

PROCEDURE WritePreload(fadr: INTEGER);
VAR emb: EmbeddedModule;
BEGIN
  PreloadSize := 0;
  IF FirstPreload # NIL THEN
    emb := FirstPreload;
    SetFilePos(Binary, BinFile, fadr);
    WHILE emb # NIL DO
      IF ~IsInnerCore(emb.mod.name) THEN
        CopyFile(emb.file, Binary, emb.size);
        INC(PreloadSize, emb.size)
      END;
      emb := emb.next
    END;
    Files.WriteInt(Binary, 0);  (* Terminate preload modules with empty name *)
    INC(PreloadSize, 16);
  END;
END WritePreload;


(* ---------------------- Generate EXE for one target ----------------------- *)

PROCEDURE GeneratePE*(name: ARRAY OF CHAR);
VAR
  PEname:      ARRAY 64 OF CHAR;
  prev1:       H.FileName;
  prev2:       H.FileName;
  prev3:       H.FileName;
  res:         INTEGER;
  HeaderSize:  INTEGER;
  FadrPreload: INTEGER;    RvaPreload:  INTEGER;
  FadrIcon:    INTEGER;    RvaIcon:     INTEGER;    IconSize:    INTEGER;
  FadrImport:  INTEGER;    RvaImport:   INTEGER;
  FadrModules: INTEGER;    RvaModules:  INTEGER;
  ImageBase:   INTEGER;    MadrPreload: INTEGER;
  numsections: INTEGER;
BEGIN
  CheckIcon(name);

  PEname[0] := 0X;  H.Append(name, PEname);  H.Append(".exe", PEname);
  BinFile := Files.New(PEname);
  SetFilePos(Binary, BinFile, 0);

  HandlersOffset := SYSTEM.SIZE(H.PreLoadVars)
                  + 8 * (Kernel32ImportCount + Gdi32ImportCount + User32ImportCount);
  LinkInnerCore;

  (* Section order:                   *)
  (*   o  Preload binaries (optional) *)
  (*   o  Icon resource    (optional) *)
  (*   o  Import table                *)
  (*   o  Inner core                  *)

  numsections := 2;
  IF FirstPreload # NIL THEN INC(numsections) END;
  IF IconPng      # NIL THEN INC(numsections) END;

  HeaderSize := FileAlign(SYSTEM.SIZE(PEheader)
              + numsections * SYSTEM.SIZE(SectionHeader));

  FadrPreload := FileAlign(HeaderSize);
  RvaPreload  := MemAlign(HeaderSize);

  WritePreload(FadrPreload);


  (* Copy icon section (as PE resources) *)
  IconSize := 0;
  FadrIcon := FileAlign(FadrPreload + PreloadSize);
  RvaIcon  := MemAlign(RvaPreload + PreloadSize);
  IF IconPng # NIL THEN IconSize := MakeIconResource(FadrIcon, RvaIcon) END;

  FadrImport := FileAlign(FadrIcon + IconSize);
  RvaImport  := MemAlign(RvaIcon + IconSize);
  BuildImportTables(RvaImport);

  (* Determine file and virtual addresses *)

  FadrModules := FileAlign(FadrImport + ImportSize);
  RvaModules  := Align(RvaImport + ImportSize, 10000H);
  ImageBase   := OberonCoreBase - RvaModules;

  ASSERT(RvaImport  + ImportSize <= RvaModules);  (* If fails increase RvaModules  *)

  (* Fill in target addresses *)
  Idt.Kernel32Target := RvaModules + Core.HostVars + SYSTEM.SIZE(H.PreLoadVars);
  Idt.Gdi32Target    := Idt.Kernel32Target + 8 * Kernel32ImportCount;
  Idt.User32Target   := Idt.Gdi32Target    + 8 * Gdi32ImportCount;

  (* Initialisation is at Modules body - it takes care *)
  (* of initialising Files, Kernel and Host.           *)
  MadrPreload := 0;
  IF PreloadSize > 0 THEN MadrPreload := ImageBase + RvaPreload END;
  PresetWinHost(FadrModules, ImageBase, MadrPreload, IconSize, RvaIcon);

  WHILE FadrImport > Files.Length(BinFile) DO Files.WriteByte(Binary, 0) END;
  SetFilePos(Binary, BinFile, FadrImport);
  Files.WriteRaw(Binary, Idt);
  Files.WriteMem(Binary, SYSTEM.ADR(ImportHints), ImportHintSize);
  ASSERT(ImportSize = Align(Files.Pos(Binary), 16) - FadrImport);

  WritePEHeader(ImageBase,   HeaderSize,  numsections,
                FadrPreload, RvaPreload,
                FadrIcon,    RvaIcon,
                FadrImport,  RvaImport,
                FadrModules, RvaModules);

  SetFilePos(Binary, BinFile, FadrModules);
  Files.WriteMem(Binary, SYSTEM.ADR(Core.Mem), Core.Size);

  IF Files.Pos(Binary) MOD FileAlignment # 0 THEN
    SetFilePos(Binary, BinFile, FileAlign(Files.Pos(Binary))-1);
    Files.WriteByte(Binary, 0);
  END;

  (* Shuffle previous versions *)
  prev1 := BinFile.name; H.Append(".-1", prev1);
  prev2 := BinFile.name; H.Append(".-2", prev2);
  prev3 := BinFile.name; H.Append(".-3", prev3);

  res   := H.FileMove(prev2, prev3);
  res   := H.FileMove(prev1, prev2);
  res   := H.FileMove(BinFile.name, prev1);

  Files.Register(BinFile);

  FirstPreload := NIL;
  LastPreload  := NIL
END GeneratePE;


(* ---------------------- Generate ELF for one target ----------------------- *)

PROCEDURE CompressIconImage(name: ARRAY OF CHAR;  VAR cmpbuf: ARRAY OF BYTE; VAR imglen: INTEGER);
VAR
  fn:    ARRAY 64 OF CHAR;
  bmp:   Files.File;
  rdr:   Files.Rider;
  x, y:  INTEGER;
  pos:   INTEGER;
  len:   SYSTEM.CARD32;
  type:  ARRAY 4 OF CHAR;
  magic: SYSTEM.CARD16;
  hdr:   RECORD
           size:   SYSTEM.CARD32;
           rsvd:   SYSTEM.CARD32;
           imgadr: SYSTEM.CARD32;
          END;
  img:   ARRAY 256*256 OF SYSTEM.CARD32;
  argb:  SYSTEM.CARD32;
  i:     INTEGER;
  a:     INTEGER;
  r:     INTEGER;
  g:     INTEGER;
  b:     INTEGER;
  kind,
  runkind:   INTEGER;  (* 0: transparent, 1: black, 2: white, 3: greylevel *)
  runlength: INTEGER;
  cmpi:      INTEGER;
  cmpx:      INTEGER;

  PROCEDURE addrun(kind, length, cmpx: INTEGER; VAR cmpi: INTEGER; VAR buf: ARRAY OF BYTE);
  BEGIN
    IF kind < 3 THEN
      WHILE length >= 64 DO buf[cmpi] := kind * 64;  INC(cmpi);  DEC(length, 64) END;
      IF length > 0 THEN   buf[cmpi] := kind * 64 + length;  INC(cmpi) END;
    ELSE
      ASSERT(length # 0); ASSERT(length < 64);
      buf[cmpx] := 0C0H + length
    END
  END addrun;

  PROCEDURE EncodeGrey(grey: INTEGER): INTEGER;
  VAR result: INTEGER;
  BEGIN
    IF grey MOD 1000000H = 0 THEN       (* edge between black and background *)
      result := grey DIV 2000000H + 80H
    ELSE                                (* edge between black and white *)
      result := grey MOD 100H DIV 2
    END;
  RETURN result END EncodeGrey;

BEGIN
  fn := name;  H.Append(".bmp", fn);  bmp := Files.Old(fn);
  IF bmp # NIL THEN
    Files.Set(rdr, bmp, 0);  Files.ReadRaw(rdr, magic);
    IF magic # 4D42H THEN bmp := NIL END
  END;
  IF bmp # NIL THEN
    Files.ReadRaw(rdr, hdr);
    IF hdr.imgadr + 256*256*4 > Files.Length(bmp) THEN
      bmp := NIL
    ELSE
      Files.Set(rdr, bmp, hdr.imgadr);
      FOR y := 0 TO 255 DO  (* Read image upside down *)
        Files.ReadMem(rdr, SYSTEM.ADR(img[256 * (255 - y)]), 256*4);
      END;
    END
  END;
  IF bmp # NIL THEN
    runkind := -1;  runlength := 0;  cmpi := 0;
    FOR i := 0 TO 65535 DO
      argb := img[i];
      IF    argb = 000000000H THEN kind := 0  (* Transparent *)
      ELSIF argb = 0FF000000H THEN kind := 1  (* Black       *)
      ELSIF argb = 0FFFFFFFFH THEN kind := 2  (* White       *)
      ELSE                         kind := 3  (* Intermediate grey or transparency level *)
      END;
      IF kind = runkind THEN INC(runlength);
      ELSE
        addrun(runkind, runlength, cmpx, cmpi, cmpbuf);
        IF kind = 3 THEN cmpx := cmpi; INC(cmpi) END;
        runkind := kind;  runlength := 1;
      END;
      IF kind = 3 THEN cmpbuf[cmpi] := EncodeGrey(argb);  INC(cmpi) END
    END;
    addrun(runkind, runlength, cmpx, cmpi, cmpbuf);
    cmpbuf[cmpi] := 0C0H;  (* Terminator *)
    (*H.ws("  Compressed length ");  H.wi(cmpi);  H.wsn(" bytes.");*)
    imglen := cmpi+1
  END
END CompressIconImage;


PROCEDURE PresetLinHost(name: ARRAY OF CHAR; MadrPreload: INTEGER);
VAR
  preloadvars: H.PreLoadVars;
  iconofs:     INTEGER;
  iconimg:     ARRAY H.IconImgSize OF BYTE;
  len:         INTEGER;
BEGIN
  H.ZeroFill(preloadvars);
  preloadvars.Exeadr      := 0;
  preloadvars.CoreAdr     := OberonCoreBase;
  preloadvars.CoreSize    := Core.Size;
  preloadvars.MadrPreload := MadrPreload;
  SYSTEM.COPY(SYSTEM.ADR(preloadvars), SYSTEM.ADR(Core.Mem[Core.HostVars]), SYSTEM.SIZE(H.PreLoadVars));
  CompressIconImage(name, iconimg, len);
  iconofs := Core.HostVars + SYSTEM.SIZE(H.PreLoadVars) + H.HandlerCount * 8;
  SYSTEM.COPY(SYSTEM.ADR(iconimg), SYSTEM.ADR(Core.Mem[iconofs]), len);
END PresetLinHost;


PROCEDURE GenerateELF*(name: ARRAY OF CHAR);
VAR
  HostHdr:     Modules.Module;
  KernelHdr:   Modules.Module;
  FilesHdr:    Modules.Module;
  ModulesHdr:  Modules.Module;
  entry:       INTEGER;
  prev1:       H.FileName;
  prev2:       H.FileName;
  prev3:       H.FileName;
  res:         INTEGER;
  fadrcore:    INTEGER;
  fadrpreload: INTEGER;
  madrpreload: INTEGER;
  numsegments: INTEGER;
BEGIN
  Core.ModCount   := 0;
  Core.Size       := 0;
  Core.EntryPoint := 0;
  HandlersOffset  := SYSTEM.SIZE(H.PreLoadVars);

  LinkInnerCore;

  BinFile := Files.New(name);

  (*H.ws("PreloadSize: "); H.wh(PreloadSize); H.wsn("H.");*)
  numsegments := 1;
  fadrcore    := 1000H;
  IF FirstPreload # NIL THEN
    INC(numsegments);
    fadrpreload := 1000H;
    WritePreload(fadrpreload);
    fadrcore    := Align(fadrpreload + PreloadSize, 1000H);
    madrpreload := 4000000H;
  END;

  PresetLinHost(name, madrpreload);

  entry := OberonCoreBase + Core.EntryPoint;

  SetFilePos(Binary, BinFile, 0);
  Files.WriteByte(Binary, 7FH);  Files.WriteMem(Binary, SYSTEM.ADR("ELF"), 3);
  Files.WriteByte(Binary, 2);     (* 64 bit                                 *)
  Files.WriteByte(Binary, 1);     (* Little endian                          *)
  Files.WriteByte(Binary, 1);     (* ELF v1                                 *)
  Files.WriteByte(Binary, 3);     (* Linux                                  *)
  Files.WriteInt (Binary, 0);     (* e_ident unused by Linux >= 2.6         *)
  WriteWord(2);                   (* Executable file                        *)
  WriteWord(3EH);                 (* AMD x86-64                             *)
  WriteDWord(1);                  (* Original version of ELF                *)
  Files.WriteInt(Binary, entry);  (* Entry point                            *)
  Files.WriteInt(Binary, 40H);    (* Address of program header table        *)
  Files.WriteInt(Binary, 0);      (* Address of section header table        *)
  WriteDWord(0);                  (* Flags                                  *)
  WriteWord(40H);                 (* Size of this header                    *)
  WriteWord(38H);                 (* Program header table entry size        *)
  WriteWord(numsegments);         (* Number of program header table entries *)
  WriteWord(40H);                 (* Section header table entry size        *)
  WriteWord(0);                   (* Number of section header table entries *)
  WriteWord(0);                   (* Section index containing names         *)
  ASSERT(Files.Pos(Binary) = 40H);

  IF FirstPreload # NIL THEN
    (* Preload segment header *)
    WriteDWord(1);                        (* Loadable segment                      *)
    WriteDWord(4);                        (* Readable                              *)
    Files.WriteInt(Binary, fadrpreload);  (* Offset of segment content in ELF file *)
    Files.WriteInt(Binary, madrpreload);   (* Virtual address of section in memory *)
    Files.WriteInt(Binary, 0);            (* Physical address (not used)           *)
    Files.WriteInt(Binary, PreloadSize);  (* Size of section in ELF file           *)
    Files.WriteInt(Binary, PreloadSize);  (* Size of section in memory             *)
    Files.WriteInt(Binary, 1000H);        (* Alignment                             *)
  END;

  (* Core segment program header *)
  WriteDWord(1);                     (* Loadable segment                      *)
  WriteDWord(7);                     (* Readable, writeable, executable       *)
  Files.WriteInt(Binary, fadrcore);  (* Offset of segment content in ELF file *)
  Files.WriteInt(Binary, OberonCoreBase);  (* Virtual address of section in memory *)
  Files.WriteInt(Binary, 0);         (* Physical address (not used)           *)
  Files.WriteInt(Binary, Core.Size); (* Size of section in ELF file           *)
  Files.WriteInt(Binary, Core.Size); (* Size of section in memory             *)
  Files.WriteInt(Binary, 1000H);     (* Alignment                             *)

  ASSERT(Files.Pos(Binary) = 40H + 38H * numsegments);

  (* Core *)
  SetFilePos(Binary, BinFile, fadrcore);
  Files.WriteMem(Binary, SYSTEM.ADR(Core.Mem), Core.Size);

  (* Shuffle previous versions *)
  prev1 := BinFile.name; H.Append(".-1", prev1);
  prev2 := BinFile.name; H.Append(".-2", prev2);
  prev3 := BinFile.name; H.Append(".-3", prev3);

  res   := H.FileMove(prev2, prev3);
  res   := H.FileMove(prev1, prev2);
  res   := H.FileMove(BinFile.name, prev1);

  Files.Register(BinFile);
  H.FileChangeMode(BinFile^, 5*64 + 5*8 + 7);  (* 0557 -rwxr-xr-x *)

  FirstPreload := NIL;
  LastPreload  := NIL
END GenerateELF;


(* --------------------------- Preloaded modules ---------------------------- *)

PROCEDURE IsAlreadyPreloaded(name: ARRAY OF CHAR): BOOLEAN;
VAR emb: EmbeddedModule;
BEGIN emb := FirstPreload;
  WHILE (emb # NIL) & (emb.mod.name # name) DO emb := emb.next END
RETURN emb # NIL END IsAlreadyPreloaded;


PROCEDURE AddPreloadModule(name: ARRAY OF CHAR; reqkey: INTEGER);
VAR
  fn:   H.FileName;
  f:    Files.File;
  size: INTEGER;
  r:    Files.Rider;
  hdr:  Modules.ModDesc;
  imp:  Modules.ModuleName;
  key:  INTEGER;
  emb:  EmbeddedModule;

BEGIN
  IF ~(IsInnerCore(name) OR IsAlreadyPreloaded(name)) THEN
    (*H.ws("Adding preload module "); H.ws(name); H.wsn(".");*)
    fn := name;  H.Append(".x64", fn);  f := Files.Old(fn);
    IF f = NIL THEN
      ws("Link.Mod cannot find "); ws(fn);
      wsn(".");  ASSERT(FALSE)
    ELSE
      SetFilePos(r, f, 0);  Files.ReadRaw(r, hdr);
      IF hdr.magic # "Oberon5" THEN
        ws("Link.Mod: "); ws(fn); wsn(" corrupt header.");
        ASSERT(FALSE)
      ELSIF hdr.name # name THEN
        ws("Link.Mod: "); ws(fn); wsn(" wrong module name.");
        ASSERT(FALSE)
      ELSIF (reqkey # 0) & (reqkey # hdr.key) THEN
        ws("Link.Mod: "); ws(fn); ws(" key ");  wh(hdr.key);
        ws("H not requested key ");  wh(reqkey);  wsn("H.");
        ASSERT(FALSE)
      ELSE
        size := Files.Length(f);
        IF (size <= 0) OR (size MOD 16 # 0) THEN
          ws("Link.Mod: "); ws(fn); ws(" length "); wi(size);
          wsn(" is not a whole multiple of 16.");
          ASSERT(FALSE)
        ELSE
          SetFilePos(r, f, (SYSTEM.SIZE(Modules.ModDesc) + 15) DIV 16 * 16);
          Files.ReadString(r, imp);
          WHILE imp[0] # 0X DO
            Files.ReadInt(r, key);
            AddPreloadModule(imp, key);
            Files.ReadString(r, imp);
          END;
          NEW(emb);
          emb.size := size;
          emb.next := NIL;
          emb.mod  := hdr;
          emb.file := f;
          IF FirstPreload = NIL THEN FirstPreload := emb ELSE LastPreload.next := emb END;
          LastPreload := emb;
          INC(PreloadSize, emb.size)
        END
      END
    END
  END
END AddPreloadModule;


PROCEDURE ParseTarget(
  VAR r: Texts.Reader;
  VAR ch: CHAR;
  VAR target, modname: ARRAY OF CHAR
);
VAR
  i, j, k: INTEGER;
BEGIN
  WHILE ch = " " DO Texts.Read(r, ch) END;
  i := 0;  j := 0;  k := -1;
  WHILE (ch > " ") & (ch # "+") DO
    target[i] := ch;
    IF ch = "." THEN k := i END;
    INC(i);
    IF (ch = "/") OR (ch = 5CX) THEN j := i END;
    Texts.Read(r, ch)
  END;
  target[i] := 0X;

  (* Extract modname from modname[.commandname] *)
  IF k < 0 THEN k := i END;
  (* Module name from j to k, command name from k+1 to i *)
  i := 0;
  WHILE j < k DO modname[i] := target[j];  INC(i);  INC(j) END;
  modname[i] := 0X
END ParseTarget;


PROCEDURE Link(tgthost: INTEGER);
VAR
  start:   INTEGER;
  r:       Texts.Reader;
  ch:      CHAR;
  target:  ARRAY 64 OF CHAR;
  modname: ARRAY 64 OF CHAR;
  i:       INTEGER;
BEGIN
  start := H.Time();

  (* Parse argument modname[.commandname] from parameter as arg *)
  Texts.OpenReader(r, Texts.Par.text, Texts.Par.pos);
  Texts.Read(r, ch);

  ParseTarget(r, ch, target, modname);
  WHILE (target[0] # 0X) & (target # "~") DO
    IF tgthost = Win THEN ws(" linking PE ") ELSE ws(" linking ELF ") END;
    ws(target);
    IF tgthost = Win THEN ws(".exe  ") ELSE ws("  ") END;

    AddPreloadModule(modname, 0);

    (* Add any further requested preload modules *)
    WHILE ch = " " DO Texts.Read(r, ch) END;
    WHILE ch = "+" DO
      Texts.Read(r, ch);  WHILE ch = " " DO Texts.Read(r,ch) END;
      i := 0;
      WHILE (ch > " ") & (ch # "+") DO
        modname[i] := ch; INC(i); Texts.Read(r,ch)
      END;
      modname[i] := 0X;
      AddPreloadModule(modname, 0);
      WHILE ch = " " DO Texts.Read(r, ch) END
    END;

    IF tgthost = Win THEN GeneratePE(target) ELSE GenerateELF(target) END;

    wi((H.Time() - start) DIV 10000);  wsn("ms");

    ParseTarget(r, ch, target, modname)
  END
END Link;


PROCEDURE PE*;   BEGIN Link(Win) END PE;
PROCEDURE ELF*;  BEGIN Link(Lin) END ELF;


BEGIN
  Texts.OpenWriter(W); wsn("OX Linker  DCWB 2024-05-10");
  FirstPreload := NIL;
  LastPreload  := NIL;
  PreloadSize  := 0;
END Link.
