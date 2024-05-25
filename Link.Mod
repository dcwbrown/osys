MODULE Link;  (* Create exe for a given module + optional command *)
(* DCWB 2023/04/14 .. 2024/05/25 *)

IMPORT SYSTEM, H := WinHost, Kernel, Files, Texts, Modules;

CONST
  MemoryAlignment = 1000H;   (* Sections are a multiple of this size in memory *)
  FileAlignment   =  200H;   (* Sections are a multiple of this size on file *)

  OberonCoreBase = 100000000H;  (* All linking is arranged to place the Oberon core at 1,0000,0000H *)

  BootstrapVarBytes   = 64;  (* size of WinHost PreLoadVars *)
  Kernel32ImportCount = 40;
  Gdi32ImportCount    = 18;
  User32ImportCount   = 35;

TYPE
  ModuleDesc = H.ModuleDesc;

  EmbeddedModule = POINTER TO EmbeddedModuleDesc;
  EmbeddedModuleDesc = RECORD
    next: EmbeddedModule;
    name: Modules.ModuleName;
    key:  INTEGER;
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

  BootstrapBuffer = RECORD
    Header:  ModuleDesc;
    Content: ARRAY 10000H OF BYTE
  END;


VAR
  W:            Texts.Writer;
  FileName:     ARRAY 512 OF CHAR;
  ExeFile:      Files.File;
  Exe:          Files.Rider;

  Verbose:      BOOLEAN;

  FirstPreload: EmbeddedModule;
  LastPreload:  EmbeddedModule;
  PreloadSize:  INTEGER;

  (* Section ".idata" - imports from Windows OS *)
  Idt:            ImportDirectoryTable;
  ImportHints:    ARRAY 2000 OF BYTE;
  ImportHintSize: INTEGER;
  ImportSize:     INTEGER;  (* Directory table + hints rounded up to multiple of 16 *)

  (* Section "Oberon" - Inner core *)
  CoreMem:    ARRAY 10000H OF BYTE;  (* Will be loaded at ImageBase + RvaModules *)
  CoreModAdr: ARRAY 16 OF RECORD  (* Module name to base address map *)
    name: Modules.ModuleName;
    key:  INTEGER;
    adr:  INTEGER
  END;
  CoreModCount:   INTEGER;
  CoreEntryPoint: INTEGER;  (* Relative to OberonCoreBase *)
  CoreSize:       INTEGER;
  CoreHostVars:   INTEGER;  (* Loaded address of vars relative to OberonCoreBase *)



(* Convenience functions *)
PROCEDURE spos(p: INTEGER);  BEGIN Files.Set(Exe, ExeFile, p) END spos;

PROCEDURE wn; BEGIN Texts.WriteLn(W); Texts.Append(Texts.Log, W.buf) END wn;

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
RETURN (name = "WinHost") OR (name = "Kernel")
    OR (name = "Files")   OR (name = "Modules")
END IsInnerCore;


(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)



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
  Files.WriteBytes(Exe, shdr, 0, SYSTEM.SIZE(SectionHeader));
END WriteSectionHeader;

(*
  i := 0; WHILE i < LEN(name) DO Files.Write(Exe, name[i]) INC(i) END;
  WHILE i < 8 DO Files.Write(Exe, 0); INC(i) END;
  Files.WriteBytes(Exe,
*)

PROCEDURE WritePEHeader(ImageBase,   HeaderSize,
                        FadrPreload, RvaPreload,
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
  IF FadrImport > FadrPreload THEN
    hdr.numberOfSections   := 3
  ELSE
    hdr.numberOfSections   := 2
  END;
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
  hdr.sizeOfCode              := PageAlign(CoreSize);
  hdr.sizeOfInitializedData   := RvaModules - RvaPreload;
  hdr.sizeOfUninitializedData := 0;
  hdr.addressOfEntryPoint     := RvaModules + CoreEntryPoint;
  hdr.baseOfCode              := RvaModules;

  (* Windows specific PE32+ fields *)
  hdr.imageBase               := ImageBase;
  hdr.MemoryAlignment         := MemoryAlignment;
  hdr.fileAlignment           := FileAlignment;
  hdr.majorOSVersion          := 1;
  hdr.majorSubsystemVersion   := 5;
  hdr.sizeOfImage             := RvaModules + PageAlign(CoreSize); (* + 1000H;*) (*Align(ModuleSize, MemoryAlignment);*)
  hdr.sizeOfHeaders           := HeaderSize;
  hdr.subsystem               := 3;      (* Console *)
(*hdr.subsystem               := 2;*)    (* Windows *)
  hdr.dllCharacteristics      := 400H;   (* No SEH *)
  hdr.sizeOfStackReserve      := 1000H;
  hdr.sizeOfStackCommit       := 1000H;
  hdr.sizeOfHeapReserve       := 1000H;  (* Minimal heap - Windows may use it, we don't *)
  hdr.numberOfRvaAndSizes     := 16;

  (* Optional header data directories *)
  hdr.importTableRVA          := RvaImport;
  hdr.importTableSize         := RvaModules - RvaImport;

  spos(0);
  Files.WriteBytes(Exe, hdr, 0, SYSTEM.SIZE(PEheader));

  (* Write section headers *)
  IF FadrImport > FadrPreload THEN
    WriteSectionHeader("Preload",
                     RvaImport  - RvaPreload,    (* Size in memory *)
                     FadrImport - FadrPreload,   (* Size on disk   *)
                     RvaPreload,
                     FadrPreload,
                     SReadable + SInitialised);
  END;

  WriteSectionHeader("Imports",
                     RvaModules - RvaImport,    (* Section size in memory *)
                     FileAlign(ImportSize),     (* Size on disk *)
                     RvaImport,
                     FadrImport,
                     SReadable + SWriteable + SInitialised);

  WriteSectionHeader("Core",
                     PageAlign(CoreSize),      (* Reserved size in memory *)
                     FileAlign(CoreSize),      (* Size on disk *)
                     RvaModules,
                     FadrModules,
                     SReadable + SWriteable + SExecutable + SCode);

  ASSERT(Files.Pos(Exe) <= HeaderSize);

END WritePEHeader;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE ReadVar(VAR r: Files.Rider;  VAR var: ARRAY OF BYTE);
BEGIN Files.ReadBytes(r, var, LEN(var))
END ReadVar;

(* --------------------------- Inner core linking --------------------------- *)

PROCEDURE ExportedOffset(modhdr: INTEGER; index: INTEGER): INTEGER;
VAR exportoffset: SYSTEM.CARD32;  mod: Modules.Module;
BEGIN
  mod := SYSTEM.VAL(Modules.Module, SYSTEM.ADR(CoreMem[modhdr]));
  SYSTEM.GET(ORD(mod) + mod.exports + index * 4, exportoffset);
RETURN exportoffset END ExportedOffset;


PROCEDURE LinkImport*(
  modofs:       INTEGER;  (* Module offset of importing module relative to CoreMem/OberonCoreBase *)
  offset:       INTEGER;  (* Offset within importing module of disp/abs value being updated *)
  impno, modno: INTEGER;
  modules:      ARRAY OF INTEGER
);
VAR
  disp:           SYSTEM.INT32;
  Coremodadr:         INTEGER;
  expoffset:      INTEGER;
  absreloc:       INTEGER;
  handlersoffset: INTEGER;
  winhost:        Modules.Module;
BEGIN
  winhost := SYSTEM.VAL(Modules.Module, SYSTEM.ADR(CoreMem));
  Coremodadr  := SYSTEM.ADR(CoreMem[modofs]);
  IF modno = 0 THEN  (* system function *)
    SYSTEM.GET(Coremodadr + offset, disp);
    ASSERT((impno >= 0) & (impno <= H.HandlerCount));

    handlersoffset := winhost.vars - OberonCoreBase  (* relative to start of modules *)
                    + BootstrapVarBytes
                    + 8 * (Kernel32ImportCount + Gdi32ImportCount + User32ImportCount);

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


PROCEDURE LoadModule(f: Files.File): Modules.Module;
VAR
  mod:     Modules.Module;
  R:       Files.Rider;
  i, m:    INTEGER;
  modules: ARRAY 32 OF INTEGER;  (* Offset into CoreMem of start of module *)
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
  ASSERT(f # NIL);
  Files.Set(R, f, 0);
  mod := SYSTEM.VAL(Modules.Module, SYSTEM.ADR(CoreMem[CoreSize]));
  ReadVar(R, mod^);
  CoreModAdr[CoreModCount].name := mod.name;
  CoreModAdr[CoreModCount].key  := mod.key;
  CoreModAdr[CoreModCount].adr  := CoreSize;  (* Offset into CoreMem = relative to OberonCoreBase *)
  m := CoreSize + SYSTEM.SIZE(Modules.ModDesc);
  FOR i := SYSTEM.SIZE(Modules.ModDesc) + 1 TO mod.vars DO
    Files.ReadByte(R, CoreMem[m]);  INC(m)
  END;
  FOR i := 1 TO mod.varsize DO CoreMem[m] := 0;  INC(m) END;

  (* Extract list of imported modules from loaded code *)
  m := CoreSize + SYSTEM.SIZE(Modules.ModDesc);
  impcnt := 0;
  REPEAT
    i := 0;
    REPEAT impname[i] := CHR(CoreMem[m]); INC(i); INC(m) UNTIL impname[i-1] = 0X;
    IF impname[0] # 0X THEN
      SYSTEM.GET(SYSTEM.ADR(CoreMem[m]), impkey); INC(m, 8);
      (* Find loaded module number for this import *)
      modnum := 0;
      WHILE (modnum < CoreModCount) & (CoreModAdr[modnum].name # impname) DO INC(modnum) END;
      ASSERT(modnum < CoreModCount);
      ASSERT(CoreModAdr[modnum].key = impkey);
      modules[impcnt] := CoreModAdr[modnum].adr
    END;
    INC(impcnt)
  UNTIL impname[0] = 0X;

(* TODO - Implement module ref counts *)

  (* Link imports *)
  Files.Set(R, f, mod.imprefs);
  Files.ReadBytes(R, impcnt, 4);
  FOR i := 1 TO impcnt DO
    ReadVar(R, offset);  ReadVar(R, impno);  ReadVar(R, modno);
    LinkImport(CoreSize, offset, impno, modno, modules)
  END;

  (* Relocate pointer addresses *)
  ptradr := ORD(mod) + mod.ptr;  (* Address witihin CoreMem *)
  SYSTEM.GET(ptradr, ptroff);
  WHILE ptroff >= 0 DO
    SYSTEM.PUT(ptradr, OberonCoreBase + CoreSize + mod.vars + ptroff);
    INC(ptradr, 8);
    SYSTEM.GET(ptradr, ptroff)
  END;

  INC(mod.cmd, OberonCoreBase + CoreSize);
  mod.size := (mod.vars + mod.varsize + 16) DIV 16 * 16;
  INC(mod.vars, OberonCoreBase + CoreSize);
  INC(CoreSize, mod.size);
  INC(CoreModCount)
RETURN mod END LoadModule;


PROCEDURE BuildInnerCore;
VAR
  WinHostHdr: Modules.Module;
  KernelHdr:  Modules.Module;
  FilesHdr:   Modules.Module;
  ModulesHdr: Modules.Module;
BEGIN
  CoreModCount   := 0;
  CoreSize       := 0;
  CoreEntryPoint := 0;

  WinHostHdr := LoadModule(Files.Old("WinHost.x64"));
  KernelHdr  := LoadModule(Files.Old("Kernel.x64"));
  FilesHdr   := LoadModule(Files.Old("Files.x64"));
  ModulesHdr := LoadModule(Files.Old("Modules.x64"));

  WinHostHdr.next := SYSTEM.VAL(Modules.Module, OberonCoreBase + ORD(KernelHdr)  - SYSTEM.ADR(CoreMem));
  KernelHdr.next  := SYSTEM.VAL(Modules.Module, OberonCoreBase + ORD(FilesHdr)   - SYSTEM.ADR(CoreMem));
  FilesHdr.next   := SYSTEM.VAL(Modules.Module, OberonCoreBase + ORD(ModulesHdr) - SYSTEM.ADR(CoreMem));
  ModulesHdr.next := NIL;

  CoreEntryPoint := ORD(ModulesHdr) - SYSTEM.ADR(CoreMem) + ModulesHdr.init;
  CoreHostVars   := WinHostHdr.vars - OberonCoreBase;

END BuildInnerCore;


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
  (* corresponding procedure variable declarations in WinHost.mode   *)

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
  AddImport(Idt.User32Lookups, s, "CreateIconIndirect");
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


PROCEDURE PresetWinHost(FadrModules, ImageBase: INTEGER);
VAR
  mod:         Modules.Module;
  varpos:      INTEGER;
  i:           INTEGER;
  preloadvars: H.PreLoadVars;
BEGIN
  varpos := CoreHostVars;

  H.ZeroFill(preloadvars);
  preloadvars.Exeadr    := ImageBase;
  preloadvars.ImgHeader := SYSTEM.VAL(H.Module, OberonCoreBase);
  IF FirstPreload # NIL THEN
    preloadvars.FileOfs := FadrModules + FileAlign(CoreSize)
  END;

  SYSTEM.COPY(SYSTEM.ADR(preloadvars), SYSTEM.ADR(CoreMem[varpos]), SYSTEM.SIZE(H.PreLoadVars));
  INC(varpos, SYSTEM.SIZE(H.PreLoadVars));
  ASSERT(varpos = CoreHostVars + BootstrapVarBytes);

  SYSTEM.COPY(SYSTEM.ADR(Idt.Kernel32Lookups), SYSTEM.ADR(CoreMem[varpos]), Kernel32ImportCount * 8);
  INC(varpos, Kernel32ImportCount * 8);

  SYSTEM.COPY(SYSTEM.ADR(Idt.User32Lookups), SYSTEM.ADR(CoreMem[varpos]), User32ImportCount * 8);
  INC(varpos, User32ImportCount * 8);

  SYSTEM.COPY(SYSTEM.ADR(Idt.Gdi32Lookups), SYSTEM.ADR(CoreMem[varpos]), Gdi32ImportCount * 8);
END PresetWinHost;


(* -------------------------- Generate one target --------------------------- *)

PROCEDURE CopyFile(f: Files.File; VAR r2: Files.Rider; size: INTEGER);
VAR
  buf: ARRAY 1024 OF BYTE;
  i:   INTEGER;
  r1:  Files.Rider;
BEGIN
  Files.Set(r1, f, 0);
  i := size DIV LEN(buf);
  WHILE i > 0 DO
    Files.ReadBytes(r1, buf, LEN(buf));
    Files.WriteBytes(r2, buf, 0, LEN(buf));
    DEC(i)
  END;
  i := size MOD LEN(buf);
  IF i > 0 THEN
    Files.ReadBytes(r1, buf, i);
    Files.WriteBytes(r2, buf, 0, i)
  END
END CopyFile;


PROCEDURE Generate*(PEname: ARRAY OF CHAR);
CONST NumSections = 2;
VAR
  emb:         EmbeddedModule;
  prev1:       Files.FileName;
  prev2:       Files.FileName;
  prev3:       Files.FileName;
  res:         INTEGER;
  HeaderSize:  INTEGER;    PreloadSize: INTEGER;
  FadrImport:  INTEGER;    RvaImport:   INTEGER;
  FadrPreload: INTEGER;    RvaPreload:  INTEGER;
  FadrModules: INTEGER;    RvaModules:  INTEGER;
  ImageBase:   INTEGER;
BEGIN
  ExeFile := Files.New(PEname);
  Files.Set(Exe, ExeFile, 0);

  BuildInnerCore;

  HeaderSize := FileAlign(SYSTEM.SIZE(PEheader)
              + NumSections * SYSTEM.SIZE(SectionHeader));

  FadrPreload := FileAlign(HeaderSize);
  RvaPreload  := MemAlign(HeaderSize);

  (* Copy preload modules to Preload section *)
  PreloadSize := 0;
  emb := FirstPreload;
  spos(FadrPreload);
  WHILE emb # NIL DO
    IF ~IsInnerCore(emb.name) THEN
      CopyFile(emb.file, Exe, emb.size);
      INC(PreloadSize, emb.size)
    END;
    emb := emb.next
  END;
  Files.WriteInt(Exe, 0);  (* Terminate preload modules with empty name *)
  INC(PreloadSize, 16);



  FadrImport := FileAlign(FadrPreload + PreloadSize);
  RvaImport  := MemAlign(RvaPreload + PreloadSize);
  BuildImportTables(RvaImport);

  (* Determine file and virtual addresses *)

  FadrModules := FileAlign(FadrImport + ImportSize);
  RvaModules  := Align(RvaImport + ImportSize, 10000H);
  ImageBase   := OberonCoreBase - RvaModules;

  ASSERT(RvaImport  + ImportSize <= RvaModules);  (* If fails increase RvaModules  *)

  (* Fill in target addresses *)
  Idt.Kernel32Target := RvaModules + CoreHostVars + BootstrapVarBytes;
  Idt.Gdi32Target    := Idt.Kernel32Target + 8 * Kernel32ImportCount;
  Idt.User32Target   := Idt.Gdi32Target    + 8 * Gdi32ImportCount;

  (* Initialisation is at Modules body - it takes care *)
  (* of initialising Files, Kernel and WinHost.        *)
  PresetWinHost(FadrModules, ImageBase);


  (* Write .exe file *)

  spos(FadrImport);
  Files.WriteBytes(Exe, Idt, 0, SYSTEM.SIZE(ImportDirectoryTable));
  Files.WriteBytes(Exe, ImportHints, 0, ImportHintSize);
  ASSERT(ImportSize = Align(Files.Pos(Exe), 16) - FadrImport);

  WritePEHeader(ImageBase,   HeaderSize,
                FadrPreload, RvaPreload,
                FadrImport,  RvaImport,
                FadrModules, RvaModules);

  spos(FadrModules);
  Files.WriteBytes(Exe, CoreMem, 0, CoreSize);

  IF Files.Pos(Exe) MOD FileAlignment # 0 THEN
    spos(FileAlign(Files.Pos(Exe))-1);
    Files.WriteByte(Exe, 0);
  END;

  (* Copy embedded modules *)
  emb := FirstPreload;
  WHILE emb # NIL DO
    IF ~IsInnerCore(emb.name) THEN CopyFile(emb.file, Exe, emb.size) END;
    emb := emb.next
  END;

  (* Shuffle previous versions *)
  prev1 := ExeFile.name; H.Append(".-1", prev1);
  prev2 := ExeFile.name; H.Append(".-2", prev2);
  prev3 := ExeFile.name; H.Append(".-3", prev3);

  res   := H.MoveFile(prev2, prev3);
  res   := H.MoveFile(prev1, prev2);
  res   := H.MoveFile(ExeFile.name, prev1);

  Files.Register(ExeFile);

  FirstPreload := NIL;
  LastPreload  := NIL
END Generate;


(* ---------------------------- Embedded modules ---------------------------- *)

PROCEDURE IsPreloaded(name: ARRAY OF CHAR): BOOLEAN;
VAR emb: EmbeddedModule;
BEGIN emb := FirstPreload;
  WHILE (emb # NIL) & (emb.name # name) DO emb := emb.next END;
RETURN emb # NIL END IsPreloaded;


PROCEDURE AddPreloadModule(name: ARRAY OF CHAR; reqkey: INTEGER);
VAR
  fn:   Files.FileName;
  f:    Files.File;
  size: INTEGER;
  r:    Files.Rider;
  hdr:  Modules.ModDesc;
  imp:  Modules.ModuleName;
  key:  INTEGER;
  emb:  EmbeddedModule;

BEGIN
  IF ~IsPreloaded(name) THEN
    fn := name;  H.Append(".x64", fn);  f := Files.Old(fn);
    IF f = NIL THEN
      ws("Link.Mod cannot find "); ws(fn);
      wsn(".");  ASSERT(FALSE)
    ELSE
      Files.Set(r, f, 0);  ReadVar(r, hdr);
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
          Files.Set(r, f, (SYSTEM.SIZE(Modules.ModDesc) + 15) DIV 16 * 16);
          Files.ReadString(r, imp);
          WHILE imp[0] # 0X DO
            Files.ReadInt(r, key);
            AddPreloadModule(imp, key);
            Files.ReadString(r, imp);
          END;
          NEW(emb);
          emb.size := size;
          emb.next := NIL;
          emb.name := hdr.name;
          emb.key  := hdr.key;
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
  WHILE ch > " " DO
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


PROCEDURE Link*;
VAR
  r:       Texts.Reader;
  ch:      CHAR;
  target:  ARRAY 64 OF CHAR;
  modname: ARRAY 64 OF CHAR;
BEGIN
  (* Parse argument modname[.commandname] from parameter as arg *)
  Texts.OpenReader(r, Texts.Par.text, Texts.Par.pos);
  Texts.Read(r, ch);

  ParseTarget(r, ch, target, modname);
  WHILE (target[0] # 0X) & (target # "~") DO
    ws("  linking ");  wsn(target);

    AddPreloadModule(modname, 0);
    (* Special knowledge: Oberon.Mod loads System.Mod *)
    IF IsPreloaded("Oberon") THEN AddPreloadModule("System", 0) END;

    H.Append(".exe", target);
    Generate(target);

    ParseTarget(r, ch, target, modname)
  END
END Link;


BEGIN
  Texts.OpenWriter(W); wsn("Linker DCWB 2024-05-10");
  FirstPreload := NIL;
  LastPreload  := NIL;
  PreloadSize  := 0;
END Link.
