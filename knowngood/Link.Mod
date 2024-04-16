MODULE Link;  (* Create exe from a list of compiled Oberon modules *)
(* DCWB 14.04.2023..10.02.2024 *)

IMPORT SYSTEM, H := WinHost, Kernel, X64, Files, Modules;

CONST
  HeaderSize      =  200H;
  MemoryAlignment = 1000H;   (* Sections are a multiple of this size in memory *)
  FileAlignment   =  200H;   (* Sections are a multiple of this size on file *)

  (* Import section *)
  FadrImport  = HeaderSize;  (* Import directory table *)
  RvaImport   = 1000H;
  ImportVSize = 0F000H;      (* Size reserved for import table in memory *)

  (* Note: Import table is sized large to place module base at 1,0000,0000H *)

  (* Modules section *)
  FadrModules = 0E00H;
  RvaModules  = RvaImport + ImportVSize; (* 2000H; *)
  ImageBase   = 0FFFF0000H;  (* Image base must be multiple of 64k / 10000H  *)
  ModuleBase  = ImageBase + RvaModules;

  BootstrapVarBytes   = 24;  (* preloaded bootstrap VAR size preceeding imported proc addresses *)
  Kernel32ImportCount = 39;
  Gdi32ImportCount    = 12;
  User32ImportCount   = 26;

TYPE
  ModuleDesc = H.ModuleDesc;

  ObjectFile = POINTER TO ObjectFileDesc;
  ObjectFileDesc = RECORD
    next: ObjectFile;
    file: Files.File
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
  FileName:     ARRAY 512 OF CHAR;
  ExeFile:      Files.File;
  Exe:          Files.Rider;
  ModuleSize:   INTEGER;
  ModuleVSize:  INTEGER;
  ImportMSize:  INTEGER;  (* Import section size in memory (multiple of 16 bytes) *)
  Objects:      ObjectFile;
  LastObject:   ObjectFile;
  Verbose:      BOOLEAN;
  EntryPoint:   INTEGER;

  (*Bootstrap:  BootstrapBuffer;*)

  Idt:        ImportDirectoryTable;

  Mem:        ARRAY 10000H OF BYTE;  (* Will be loaded at ImageBase + RvaModules *)
  ModAdr:     ARRAY 16 OF RECORD  (* Module name to base address map *)
    name: Modules.ModuleName;
    key:  INTEGER;
    adr:  INTEGER
  END;
  ModCount:   INTEGER;


  (* Section layout - generates 2 sections:
     1. Imports section requesting standard system functions
     2. Oberon section containing pre-linked modules in requested sequence
  *)


(* Convenience functions *)
PROCEDURE spos(p: INTEGER);  BEGIN Files.Set(Exe, ExeFile, p) END spos;


(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Align(a: INTEGER;  align: INTEGER): INTEGER;
BEGIN IF a > 0 THEN INC(a, align - 1) END;
RETURN a DIV align * align END Align;

PROCEDURE FileAlign(VAR r: Files.Rider; alignment: INTEGER);
BEGIN
  IF Files.Pos(r) MOD alignment # 0 THEN
    spos(Align(Files.Pos(r), alignment)-1);
    Files.WriteByte(r, 0);
  END
END FileAlign;


(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE WriteImports;
VAR
  i, dll, n:   INTEGER;
  importhints: ARRAY 1500 OF BYTE;
  hintsize:    INTEGER;
  target:      INTEGER;
  mod:         Modules.Module;

  PROCEDURE FieldRVA(VAR field: ARRAY OF BYTE): U32;
  BEGIN RETURN RvaImport + SYSTEM.ADR(field) - SYSTEM.ADR(Idt) END FieldRVA;

  PROCEDURE AddProc(VAR hints: ARRAY OF BYTE; VAR i: INTEGER; name: ARRAY OF CHAR);
  VAR n: INTEGER;
  BEGIN
    n := 0; REPEAT hints[i] := ORD(name[n]);  INC(i);  INC(n) UNTIL name[n-1] = 0X;
  END AddProc;

  PROCEDURE AddImport(VAR lookups: ARRAY OF I64; VAR n, i: INTEGER;
                      dll: INTEGER; VAR hints: ARRAY OF BYTE; name: ARRAY OF CHAR);
  BEGIN
    lookups[n] := RvaImport + SYSTEM.SIZE(ImportDirectoryTable) + i;
    INC(n);
    ASSERT(dll < 256);
    ASSERT(i + H.Length(name) + 3 < LEN(hints));
    hints[i] := dll;  hints[i+1] := 0;  INC(i, 2);
    AddProc(hints, i, name);
  END AddImport;

BEGIN
  H.ZeroFill(Idt);
  mod := SYSTEM.VAL(Modules.Module, SYSTEM.ADR(Mem));
  target := RvaModules + mod.vars + BootstrapVarBytes;

  (* **NOTE** these imports must be in exactly the same order as the *)
  (* corresponding procedure variable declarations in WinHost.mode   *)

  Idt.Kernel32LookupTable := FieldRVA(Idt.Kernel32Lookups);
  Idt.Kernel32Dllnameadr  := FieldRVA(Idt.Kernel32Dllname);
  Idt.Kernel32Dllname     := "KERNEL32.DLL";
  Idt.Kernel32Target      := target;
  n := 0;  i := 0;  dll := 0;
  AddImport(Idt.Kernel32Lookups, n, i, dll, importhints, "AddVectoredExceptionHandler");
  AddImport(Idt.Kernel32Lookups, n, i, dll, importhints, "CloseHandle");
  AddImport(Idt.Kernel32Lookups, n, i, dll, importhints, "CreateFileW");
  AddImport(Idt.Kernel32Lookups, n, i, dll, importhints, "DeleteFileW");
  AddImport(Idt.Kernel32Lookups, n, i, dll, importhints, "ExitProcess");
  AddImport(Idt.Kernel32Lookups, n, i, dll, importhints, "FileTimeToLocalFileTime");
  AddImport(Idt.Kernel32Lookups, n, i, dll, importhints, "FileTimeToSystemTime");
  AddImport(Idt.Kernel32Lookups, n, i, dll, importhints, "FindClose");
  AddImport(Idt.Kernel32Lookups, n, i, dll, importhints, "FindFirstFileExW");
  AddImport(Idt.Kernel32Lookups, n, i, dll, importhints, "FindNextFileW");
  AddImport(Idt.Kernel32Lookups, n, i, dll, importhints, "FlushFileBuffers");
  AddImport(Idt.Kernel32Lookups, n, i, dll, importhints, "FormatMessageW");
  AddImport(Idt.Kernel32Lookups, n, i, dll, importhints, "GetCommandLineW");
  AddImport(Idt.Kernel32Lookups, n, i, dll, importhints, "GetCurrentDirectoryW");
  AddImport(Idt.Kernel32Lookups, n, i, dll, importhints, "GetCurrentProcessId");
  AddImport(Idt.Kernel32Lookups, n, i, dll, importhints, "GetEnvironmentVariableW");
  AddImport(Idt.Kernel32Lookups, n, i, dll, importhints, "GetFileAttributesExW");
  AddImport(Idt.Kernel32Lookups, n, i, dll, importhints, "GetFileAttributesW");
  AddImport(Idt.Kernel32Lookups, n, i, dll, importhints, "GetFileInformationByHandleEx");
  AddImport(Idt.Kernel32Lookups, n, i, dll, importhints, "GetFileSizeEx");
  AddImport(Idt.Kernel32Lookups, n, i, dll, importhints, "GetLastError");
  AddImport(Idt.Kernel32Lookups, n, i, dll, importhints, "GetModuleFileNameW");
  AddImport(Idt.Kernel32Lookups, n, i, dll, importhints, "GetProcAddress");
  AddImport(Idt.Kernel32Lookups, n, i, dll, importhints, "GetStdHandle");
  AddImport(Idt.Kernel32Lookups, n, i, dll, importhints, "GetSystemTimePreciseAsFileTime");
  AddImport(Idt.Kernel32Lookups, n, i, dll, importhints, "GetTempFileNameA");
  AddImport(Idt.Kernel32Lookups, n, i, dll, importhints, "GetTempPathA");
  AddImport(Idt.Kernel32Lookups, n, i, dll, importhints, "LoadLibraryA");
  AddImport(Idt.Kernel32Lookups, n, i, dll, importhints, "MoveFileExW");
  AddImport(Idt.Kernel32Lookups, n, i, dll, importhints, "ReadFile");
  AddImport(Idt.Kernel32Lookups, n, i, dll, importhints, "SetConsoleOutputCP");
  AddImport(Idt.Kernel32Lookups, n, i, dll, importhints, "SetEndOfFile");
  AddImport(Idt.Kernel32Lookups, n, i, dll, importhints, "SetFileInformationByHandle");
  AddImport(Idt.Kernel32Lookups, n, i, dll, importhints, "SetFilePointerEx");
  AddImport(Idt.Kernel32Lookups, n, i, dll, importhints, "Sleep");
  AddImport(Idt.Kernel32Lookups, n, i, dll, importhints, "UnmapViewOfFile");
  AddImport(Idt.Kernel32Lookups, n, i, dll, importhints, "VirtualAlloc");
  AddImport(Idt.Kernel32Lookups, n, i, dll, importhints, "VirtualQuery");
  AddImport(Idt.Kernel32Lookups, n, i, dll, importhints, "WriteFile");
  ASSERT(n = Kernel32ImportCount);
  INC(target, 8 * n);

  Idt.Gdi32LookupTable := FieldRVA(Idt.Gdi32Lookups);
  Idt.Gdi32Dllnameadr  := FieldRVA(Idt.Gdi32Dllname);
  Idt.Gdi32Dllname     := "GDI32.DLL";
  Idt.Gdi32Target      := target;
  n := 0;  INC(dll);
  AddImport(Idt.Gdi32Lookups, n, i, dll, importhints, "BitBlt");
  AddImport(Idt.Gdi32Lookups, n, i, dll, importhints, "CreateBitmap");
  AddImport(Idt.Gdi32Lookups, n, i, dll, importhints, "CreateCompatibleDC");
  AddImport(Idt.Gdi32Lookups, n, i, dll, importhints, "CreateDCA");
  AddImport(Idt.Gdi32Lookups, n, i, dll, importhints, "CreateDIBSection");
  AddImport(Idt.Gdi32Lookups, n, i, dll, importhints, "CreateFontA");
  AddImport(Idt.Gdi32Lookups, n, i, dll, importhints, "DeleteObject");
  AddImport(Idt.Gdi32Lookups, n, i, dll, importhints, "GetGlyphOutlineW");
  AddImport(Idt.Gdi32Lookups, n, i, dll, importhints, "GetCharABCWidthsW");
  AddImport(Idt.Gdi32Lookups, n, i, dll, importhints, "GetDeviceCaps");
  AddImport(Idt.Gdi32Lookups, n, i, dll, importhints, "GetOutlineTextMetricsW");
  AddImport(Idt.Gdi32Lookups, n, i, dll, importhints, "SelectObject");
  ASSERT(n = Gdi32ImportCount);
  INC(target, 8 * n);

  Idt.User32LookupTable := FieldRVA(Idt.User32Lookups);
  Idt.User32Dllnameadr  := FieldRVA(Idt.User32Dllname);
  Idt.User32Dllname     := "USER32.DLL";
  Idt.User32Target      := target;
  n := 0;  INC(dll);
  AddImport(Idt.User32Lookups, n, i, dll, importhints, "BeginPaint");
  AddImport(Idt.User32Lookups, n, i, dll, importhints, "CreateIconIndirect");
  AddImport(Idt.User32Lookups, n, i, dll, importhints, "CreateWindowExW");
  AddImport(Idt.User32Lookups, n, i, dll, importhints, "DefWindowProcW");
  AddImport(Idt.User32Lookups, n, i, dll, importhints, "DispatchMessageW");
  AddImport(Idt.User32Lookups, n, i, dll, importhints, "EndPaint");
  AddImport(Idt.User32Lookups, n, i, dll, importhints, "GetClipboardFormatNameW");
  AddImport(Idt.User32Lookups, n, i, dll, importhints, "GetDpiForWindow");
  AddImport(Idt.User32Lookups, n, i, dll, importhints, "GetMessageW");
  AddImport(Idt.User32Lookups, n, i, dll, importhints, "GetQueueStatus");
  AddImport(Idt.User32Lookups, n, i, dll, importhints, "LoadCursorW");
  AddImport(Idt.User32Lookups, n, i, dll, importhints, "MessageBoxA");
  AddImport(Idt.User32Lookups, n, i, dll, importhints, "MessageBoxW");
  AddImport(Idt.User32Lookups, n, i, dll, importhints, "MoveWindow");
  AddImport(Idt.User32Lookups, n, i, dll, importhints, "MsgWaitForMultipleObjects");
  AddImport(Idt.User32Lookups, n, i, dll, importhints, "PeekMessageW");
  AddImport(Idt.User32Lookups, n, i, dll, importhints, "PostMessageW");
  AddImport(Idt.User32Lookups, n, i, dll, importhints, "PostQuitMessage");
  AddImport(Idt.User32Lookups, n, i, dll, importhints, "RegisterClassExW");
  AddImport(Idt.User32Lookups, n, i, dll, importhints, "ReleaseCapture");
  AddImport(Idt.User32Lookups, n, i, dll, importhints, "SetCapture");
  AddImport(Idt.User32Lookups, n, i, dll, importhints, "SetProcessDpiAwarenessContext");
  AddImport(Idt.User32Lookups, n, i, dll, importhints, "ShowCursor");
  AddImport(Idt.User32Lookups, n, i, dll, importhints, "ShowWindow");
  AddImport(Idt.User32Lookups, n, i, dll, importhints, "TranslateMessage");
  AddImport(Idt.User32Lookups, n, i, dll, importhints, "InvalidateRect");
  ASSERT(n = User32ImportCount);
  INC(target, 8 * n);

  hintsize := i;

  spos(FadrImport);
  Files.WriteBytes(Exe, Idt, 0, SYSTEM.SIZE(ImportDirectoryTable));
  Files.WriteBytes(Exe, importhints, 0, hintsize);

  ImportMSize := Align(Files.Pos(Exe), 16) - FadrImport;
  ASSERT(ImportMSize <= ImportVSize);


  (*H.ws("IDT size "); H.wh(ImportMSize); H.wsn("H.");*)
  IF FadrImport + ImportMSize >= FadrModules THEN
    H.ws("FadrImport + ImportMSize: "); H.wh(FadrImport + ImportMSize); H.ws("H - increase FadrModules");
    ASSERT(FALSE)
  END;
  IF RvaImport  + ImportMSize >= RvaModules THEN
    H.ws("RvaImport  + ImportMSize: "); H.wh(RvaImport  + ImportMSize); H.ws("H - increase RvaModules");
    ASSERT(FALSE)
  END;

  (*
  H.wsn("Idt: ");
  H.DumpMem(2, SYSTEM.ADR(Idt), SYSTEM.ADR(Idt), target - (RvaModules + mod.vars));
  *)
END WriteImports;


(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* PE Header *)

PROCEDURE WriteSectionHeader(name:         ARRAY OF CHAR;
                             vsize, fsize: INTEGER;
                             rva,   fadr:  INTEGER;
                             flags:        INTEGER);
TYPE
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


PROCEDURE WritePEHeader;
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
  (*
  H.ws("Size of eMagic:     "); H.wh(SYSTEM.ADR(hdr.zeroes)     - SYSTEM.ADR(hdr.eMagic));     H.wsn("H.");
  H.ws("Size of zeroes:     "); H.wh(SYSTEM.ADR(hdr.eLfanew)    - SYSTEM.ADR(hdr.zeroes));     H.wsn("H.");
  H.ws("Size of eLfanew:    "); H.wh(SYSTEM.ADR(hdr.dosProgram) - SYSTEM.ADR(hdr.eLfanew));    H.wsn("H.");
  H.ws("Size of dosProgram: "); H.wh(SYSTEM.ADR(hdr.signature)  - SYSTEM.ADR(hdr.dosProgram)); H.wsn("H.");
  H.ws("Size of signature:  "); H.wh(SYSTEM.ADR(hdr.machine)    - SYSTEM.ADR(hdr.signature));  H.wsn("H.");
  H.ws("Size of Zeroes58:   "); H.wh(SYSTEM.SIZE(Zeroes58));                                   H.wsn("H.");
  H.ws("Size of PEheader:   "); H.wh(SYSTEM.SIZE(PEheader));                                   H.wsn("H.");
  *)
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
  hdr.numberOfSections     := 2;
  hdr.sizeOfOptionalHeader := 240;
  hdr.characteristics      := 200H  (* Windows debug information stripped               *)
                            + 20H   (* Large address aware                              *)
                            + 8     (* Coff symbol tables removed (should really be 0?) *)
                            + 4     (* Coff linenumbers removed   (should really be 0?) *)
                            + 2     (* Executable image                                 *)
                            + 1;    (* Relocs stripped *)

  (* PE32+ optional header *)
  hdr.pe32magic               := 20BH;  (* PE32+ *)
  hdr.majorLinkerVersion      := 1;
  hdr.minorLinkerVersion      := 49H;
  hdr.sizeOfCode              := ModuleVSize; (*Align(ModuleSize, FileAlignment);*)
  hdr.sizeOfInitializedData   := ImportVSize;  (* ModuleSize already accounted for by hdr.sizeOfCode *)
  hdr.sizeOfUninitializedData := 0;
  hdr.addressOfEntryPoint     := EntryPoint;
  hdr.baseOfCode              := RvaModules;

  (* Windows specific PE32+ fields *)
  hdr.imageBase               := ImageBase;
  hdr.MemoryAlignment         := MemoryAlignment;
  hdr.fileAlignment           := FileAlignment;
  hdr.majorOSVersion          := 1;
  hdr.majorSubsystemVersion   := 5;
  hdr.sizeOfImage             := RvaModules + ModuleVSize; (*Align(ModuleSize, MemoryAlignment);*)
                               (*Align(HeaderSize, MemoryAlignment)
                               + Align(ModuleSize, MemoryAlignment)
                               + 4096;   (* import section *)
                               *)
  hdr.sizeOfHeaders           := HeaderSize;
  hdr.subsystem               := 3;    (* Console *)
(*hdr.subsystem               := 2;*)  (* Windows *)
  hdr.dllCharacteristics      := 400H;   (* No SEH *)
  hdr.sizeOfStackReserve      := 1000H;
  hdr.sizeOfStackCommit       := 1000H;
  hdr.sizeOfHeapReserve       := 1000H;  (* Minimal heap - Windows may use it, we don't *)
  hdr.numberOfRvaAndSizes     := 16;

  (* Optional header data directories *)
  hdr.importTableRVA          := RvaImport;
  hdr.importTableSize         := ImportVSize;

  spos(0);
  Files.WriteBytes(Exe, hdr, 0, SYSTEM.SIZE(PEheader));
  (*w.DumpMem(2, SYSTEM.ADR(hdr), 0, SYSTEM.SIZE(PEheader));*)

  (* Write section headers *)
  WriteSectionHeader(".idata",
                     ImportVSize,                           (* Section size in memory *)
                     Align(ImportMSize, FileAlignment),     (* Size on disk *)
                     RvaImport, FadrImport,
                     SReadable + SWriteable + SInitialised);

  WriteSectionHeader("Oberon",
                   (*Align(ModuleSize, MemoryAlignment),    (* Size in memory *)*)
                     ModuleVSize,                           (* Reserved size in memory *)
                     Align(ModuleSize, FileAlignment),      (* Size on disk *)
                     RvaModules, FadrModules,
                     SReadable + SWriteable + SExecutable + SCode);

  H.ws("After PE header, Pos(Exe) ");                H.wh(Files.Pos(Exe));
  H.ws("H, Align(Files.Pos(Exe), FileAlignment) ");  H.wh(Align(Files.Pos(Exe), FileAlignment));
  H.ws("H, HeaderSize "); H.wh(HeaderSize); H.wsn("H.");

  ASSERT(Align(Files.Pos(Exe), FileAlignment) = HeaderSize);

END WritePEHeader;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(*
PROCEDURE AddModule*(f: Files.File);
VAR obj: ObjectFile;
BEGIN
  ASSERT(f # NIL);
  NEW(obj); obj.file := f;
  IF Objects = NIL THEN Objects := obj ELSE LastObject.next := obj END;
  LastObject := obj
END AddModule;
*)


PROCEDURE ReadVar(VAR r: Files.Rider;  VAR var: ARRAY OF BYTE);
BEGIN Files.ReadBytes(r, var, LEN(var)) END ReadVar;


PROCEDURE ExportedOffset(modhdr: INTEGER; index: INTEGER): INTEGER;
VAR exportoffset: SYSTEM.CARD32;  mod: Modules.Module;
BEGIN
  mod := SYSTEM.VAL(Modules.Module, SYSTEM.ADR(Mem[modhdr]));
  SYSTEM.GET(ORD(mod) + mod.exports + index * 4, exportoffset);
RETURN exportoffset END ExportedOffset;


PROCEDURE LinkImport*(
  modofs:       INTEGER;  (* Module offset of importing module relative to Mem/ModuleBase *)
  offset:       INTEGER;  (* Offset within importing module of disp/abs value being updated *)
  impno, modno: INTEGER;
  modules:      ARRAY OF INTEGER
);
VAR
  disp: SYSTEM.INT32;
  modadr, expoffset, absreloc: INTEGER;
  handlersoffset: INTEGER;
BEGIN
  modadr := SYSTEM.ADR(Mem[modofs]);
  IF modno = 0 THEN  (* system function *)
    SYSTEM.GET(modadr + offset, disp);
    ASSERT((impno >= 0) & (impno <= H.HandlerCount));
    handlersoffset := SYSTEM.ADR(H.Handlers) - 100000000H;  (* Handler table offset from start of modules *)
    INC(disp, handlersoffset + 8 * impno - modofs);
    SYSTEM.PUT(modadr + offset, disp)
  ELSIF modno = 0FFFFH THEN (* 64 bit absolute address relocation *)
    (* qword at offset contains 32/0,32/module offset or 32/1,16/mod,16/imp *)
    SYSTEM.GET(modadr + offset, absreloc);
    IF absreloc DIV 100000000H = 0 THEN  (* offset in this module *)
      SYSTEM.PUT(modadr + offset, modofs + absreloc)
    ELSE  (* import reference from another module *)
      modno := absreloc DIV 10000H MOD 10000H;  ASSERT(modno > 0);
      impno := absreloc MOD 10000H;
      ASSERT(impno > 0);
      SYSTEM.PUT(modadr + offset, ModuleBase + modules[modno-1]
                                  + ExportedOffset(modules[modno-1], impno-1))
    END
  ELSE
    ASSERT(modno > 0);
    SYSTEM.GET(modadr + offset, disp);
    expoffset := ExportedOffset(modules[modno-1], impno-1);
    INC(disp, modules[modno-1] - modofs + expoffset);
    SYSTEM.PUT(modadr + offset, disp)
  END
END LinkImport;


PROCEDURE RelocatePointerAddresses*(ptradr, varadr: INTEGER);
VAR ptroff: INTEGER;
BEGIN
  SYSTEM.GET(ptradr, ptroff);
  WHILE ptroff >= 0 DO
    SYSTEM.PUT(ptradr, varadr + ptroff);
    INC(ptradr, 8);
    SYSTEM.GET(ptradr, ptroff)
  END
END RelocatePointerAddresses;


PROCEDURE LoadModule(fn: ARRAY OF CHAR): Modules.Module;
VAR
  f:       Files.File;
  mod:     Modules.Module;
  R:       Files.Rider;
  i, m:    INTEGER;
  modules: ARRAY 32 OF INTEGER;  (* Offset into Mem of start of module *)
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
  Files.Set(R, f, 0);
  mod := SYSTEM.VAL(Modules.Module, SYSTEM.ADR(Mem[ModuleSize]));
  ReadVar(R, mod^);
  ModAdr[ModCount].name := mod.name;
  ModAdr[ModCount].key  := mod.key;
  ModAdr[ModCount].adr  := ModuleSize;  (* Offset into Mem = relative to ModuleBase *)
  m := ModuleSize + SYSTEM.SIZE(Modules.ModDesc);
  FOR i := SYSTEM.SIZE(Modules.ModDesc) + 1 TO mod.vars DO
    Files.ReadByte(R, Mem[m]);  INC(m)
  END;
  FOR i := 1 TO mod.varsize DO Mem[m] := 0;  INC(m) END;

  (* Extract list of imported modules from loaded code *)
  m := ModuleSize + (SYSTEM.SIZE(Modules.ModDesc) + 15) DIV 16 * 16;
  REPEAT
    i := 0;
    REPEAT impname[i] := CHR(Mem[m]); INC(i); INC(m) UNTIL impname[i-1] = 0X;
    IF impname[0] # 0X THEN
      SYSTEM.GET(SYSTEM.ADR(Mem[m]), impkey); INC(m, 8);
      H.ws(fn); H.ws(" importing "); H.ws(impname); H.ws(" key "); H.wh(impkey); H.wsn("H.");
    END;
  UNTIL impname[0] = 0X;


(* TODO
   Build Modules ARRAY in import name loader above
   Add Imports field to header pointing beyond names to actual imports
   Link imports using imports field to address them
   Update Modules and Winhost to link modules the same way
   Remove redundant import names at end of code
   Implement module ref counts
*)

  (* Build list of imported module addresses *)
  Files.ReadString(R, impname);
  impcnt := 0;
  WHILE impname[0] # 0X DO
    modnum := 0;
    WHILE (modnum < ModCount) & (ModAdr[modnum].name # impname) DO INC(modnum) END;
    ASSERT(modnum < ModCount);
    Files.ReadInt(R, impkey);
    ASSERT(ModAdr[modnum].key = impkey);
    modules[impcnt] := ModAdr[modnum].adr;
    INC(impcnt);
    Files.ReadString(R, impname)
  END;
  Files.Set(R, f, (Files.Pos(R) + 15) DIV 16 * 16);

  IF impcnt > 0 THEN
    H.ws("* "); H.ws(mod.name); H.ws(" importing from");
    FOR i := 0 TO impcnt-1 DO
      H.ws("  ");
      mod2 := SYSTEM.VAL(Modules.Module, SYSTEM.ADR(Mem[modules[i]]));
      H.ws(mod2.name)
    END;
    H.wsn(".");
  END;

  (* Link imports *)
  Files.ReadBytes(R, impcnt, 4);
  IF impcnt > 0 THEN
    H.ws("Linking "); H.wi(impcnt);
    H.ws(" imports to module "); H.ws(mod.name); H.wsn(".");
  END;
  FOR i := 1 TO impcnt DO
    ReadVar(R, offset);  ReadVar(R, impno);  ReadVar(R, modno);
    LinkImport(ModuleSize, offset, impno, modno, modules)
  END;

  (* Relocate pointer addresses *)
  ptradr := ORD(mod) + mod.ptr;
  SYSTEM.GET(ptradr, ptroff);
  WHILE ptroff >= 0 DO
    SYSTEM.PUT(ptradr, ptroff + ModuleBase + ORD(mod) + mod.vars);
    INC(ptradr, 8);
    SYSTEM.GET(ptradr, ptroff)
  END;

  INC(mod.ptr, ModuleBase + ModuleSize);
  INC(mod.cmd, ModuleBase + ModuleSize);

  H.ws("Loaded module "); H.wi(ModCount);
  H.ws(" '");             H.ws(ModAdr[ModCount].name);
  H.ws("' at ");          H.wh(ModAdr[ModCount].adr);
  H.ws("H nimports ");    H.wh(mod.vars);
  H.ws("H varsize ");     H.wh(mod.varsize);  H.wsn("H.");

  mod.size := (mod.vars + mod.varsize + 16) DIV 16 * 16;

  INC(ModCount);
  INC(ModuleSize, mod.size)
RETURN mod END LoadModule;


PROCEDURE PresetWinHost;
VAR mod: Modules.Module;  varpos, i: INTEGER;
BEGIN
  mod    := SYSTEM.VAL(Modules.Module, SYSTEM.ADR(Mem));
  H.ws("PresetWinHost. mod.name "); H.ws(mod.name); H.ws(", nimports "); H.wh(mod.vars); H.wsn("H.");
  varpos := mod.vars;
  SYSTEM.PUT(SYSTEM.ADR(Mem[varpos]), ImageBase);       INC(varpos, 8);
  SYSTEM.PUT(SYSTEM.ADR(Mem[varpos]), ModuleBase);      INC(varpos, 8);
  SYSTEM.PUT(SYSTEM.ADR(Mem[varpos]), {63, H.Verbose}); INC(varpos, 8);
  ASSERT(varpos - mod.vars = BootstrapVarBytes);

  H.ws("Before IDT lookups write, varpos "); H.wh(varpos); H.wsn("H.");

  H.ws("SYSTEM.ADR(Idt.Kernel32Lookups) "); H.wh(SYSTEM.ADR(Idt.Kernel32Lookups)); H.wsn("H.");
  H.ws("SYSTEM.ADR(Mem[varpos])         "); H.wh(SYSTEM.ADR(Mem[varpos]));         H.wsn("H.");
  H.ws("Kernel32ImportCount * 8         "); H.wh(Kernel32ImportCount * 8);         H.wsn("H.");

  SYSTEM.COPY(SYSTEM.ADR(Idt.Kernel32Lookups), SYSTEM.ADR(Mem[varpos]), Kernel32ImportCount * 8);
  INC(varpos, Kernel32ImportCount * 8);

  SYSTEM.COPY(SYSTEM.ADR(Idt.User32Lookups), SYSTEM.ADR(Mem[varpos]), User32ImportCount * 8);
  INC(varpos, User32ImportCount * 8);

  SYSTEM.COPY(SYSTEM.ADR(Idt.Gdi32Lookups), SYSTEM.ADR(Mem[varpos]), Gdi32ImportCount * 8);
  INC(varpos, Gdi32ImportCount * 8);
END PresetWinHost;


PROCEDURE Init*;
BEGIN Objects := NIL;  LastObject := NIL  END Init;


PROCEDURE CallJumpIndirect(opcode: INTEGER; mod: Modules.Module);
VAR target: INTEGER;  disp: SYSTEM.INT32;
BEGIN
  IF mod.init # 0 THEN
    Mem[ModuleSize] := opcode;  INC(ModuleSize);
    target := mod.init + ORD(mod) - SYSTEM.ADR(Mem);
    disp := target - (ModuleSize + 4);
    SYSTEM.PUT(SYSTEM.ADR(Mem[ModuleSize]), disp);
    INC(ModuleSize, 4)
  END
END CallJumpIndirect;


PROCEDURE do*;
VAR
  WinHostHdr: Modules.Module;
  KernelHdr:  Modules.Module;
  FilesHdr:   Modules.Module;
  ModulesHdr: Modules.Module;
BEGIN
  H.wsn("Linking.");
  ModuleSize := 0;
  WinHostHdr := LoadModule("WinHost.x64");
  KernelHdr  := LoadModule("Kernel.x64");
  FilesHdr   := LoadModule("Files.x64");
  ModulesHdr := LoadModule("Modules.x64");

  WinHostHdr.next := SYSTEM.VAL(Modules.Module, ModuleBase + ORD(KernelHdr)  - SYSTEM.ADR(Mem));
  KernelHdr.next  := SYSTEM.VAL(Modules.Module, ModuleBase + ORD(FilesHdr)   - SYSTEM.ADR(Mem));
  FilesHdr.next   := SYSTEM.VAL(Modules.Module, ModuleBase + ORD(ModulesHdr) - SYSTEM.ADR(Mem));
  ModulesHdr.next := NIL;

  (* Set up initialisation *)
  EntryPoint := RvaModules (*ModuleBase*) + ModuleSize;

  CallJumpIndirect(0E8H, WinHostHdr);  (* Call *)
  CallJumpIndirect(0E8H, KernelHdr);   (* Call *)
  CallJumpIndirect(0E8H, FilesHdr);    (* Call *)
  CallJumpIndirect(0E9H, ModulesHdr);  (* Jump *)

  ModuleVSize := Align(ModuleSize, 10000H);  (* Tell loader to allocate whole multiple of alloc size *)

  H.ws("ModuleSize ");    H.wh(ModuleSize);
  H.ws("H ModuleVSize "); H.wh(ModuleVSize);
  H.ws("H entry point "); H.wh(EntryPoint); H.wsn("H.");

  ExeFile := Files.New("innercore.exe");

  Files.Set(Exe, ExeFile, 0);
  WriteImports;
  PresetWinHost;

  spos(FadrModules);
  Files.WriteBytes(Exe, Mem, 0, ModuleSize);
  FileAlign(Exe, FileAlignment);
  ModuleSize := Files.Pos(Exe) - FadrModules;

  WritePEHeader;
  Files.Register(ExeFile);

  H.wsn("Link generated innercore.exe.")
END do;

BEGIN Init; do
END Link.
