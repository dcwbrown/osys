MODULE WinPE;  (* Create exe from a list of compiled Oberon modules *)
(* DCWB 14.04.2023..10.02.2024 *)

IMPORT SYSTEM, H := WinHost, K := Kernel, X64, Files;

CONST
  HeaderSize      =  400H;
  MemoryAlignment = 1000H;  (* Sections are a multiple of this size in memory *)
  FileAlignment   =  200H;  (* Sections are a multiple of this size on file *)

  ImageBase   = 400000H;
  FadrImport  = 0400H;  RvaImport  = 1000H;  (* Import directory table *)
  FadrModules = 0E00H;  RvaModules = 2000H;  (* Oberon modules *)

  BootstrapVarBytes   = 24;  (* preloaded bootstrap VAR size preceeding imported proc addresses *)
  Kernel32ImportCount = 35;
  Gdi32ImportCount    = 12;
  User32ImportCount   = 26;

TYPE
  CodeHeader = H.CodeHeader;

  (* Temporary new code header
  CodeHeader* = RECORD-
    length*:   SYSTEM.CARD32;  (* File length *)
    initcode*: SYSTEM.CARD32;
    pointers*: SYSTEM.CARD32;
    commands*: SYSTEM.CARD32;
    exports*:  SYSTEM.CARD32;
    imports*:  SYSTEM.CARD32;  (* VARs start here following import resolution *)
    varsize*:  SYSTEM.CARD32;
    key*:      INTEGER;
  END;
  *)


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
    Header:  CodeHeader;
    Content: ARRAY 10000H OF BYTE
  END;


VAR
  FileName:   ARRAY 512 OF CHAR;
  ExeFile:    Files.File;
  Exe:        Files.Rider;
  OberonSize: INTEGER;
  ImportSize: INTEGER;
  Objects:    ObjectFile;
  LastObject: ObjectFile;
  Verbose:    BOOLEAN;

  Bootstrap:  BootstrapBuffer;

  Idt:        ImportDirectoryTable;


  (* Section layout - generates 2 sections:
     1. Imports section requesting standard system functions
     2. Oberon section containing concatenated modules in link sequence
  *)


(* Convenience functions *)
PROCEDURE spos(p: INTEGER);
BEGIN Files.Set(Exe, ExeFile, p) END spos;


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
  target:      INTEGER;
  importhints: ARRAY 1500 OF BYTE;
  hintsize:    INTEGER;

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
  target := RvaModules + Bootstrap.Header.imports + BootstrapVarBytes;

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

  ImportSize := Align(Files.Pos(Exe), 16) - FadrImport;
  (*H.ws("IDT size "); H.wh(ImportSize); H.wsn("H.");*)
  IF FadrImport + ImportSize >= FadrModules THEN
    H.ws("FadrImport + ImportSize: "); H.wh(FadrImport + ImportSize); H.ws("H - increase FadrModules");
    ASSERT(FALSE)
  END;
  IF RvaImport  + ImportSize >= RvaModules THEN
    H.ws("RvaImport  + ImportSize: "); H.wh(RvaImport  + ImportSize); H.ws("H - increase RvaModules");
    ASSERT(FALSE)
  END;
END WriteImports;


(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE CopyFile(f: Files.File);
VAR  r: Files.Rider;  buf: ARRAY 1000H OF BYTE;
BEGIN
  IF Verbose THEN
    H.ws("Adding "); H.ws(f.name); H.ws(" at file offset "); H.wh(Files.Pos(Exe));
    H.ws("H, FadrModules + "); H.wh(Files.Pos(Exe) - FadrModules); H.wsn("H.")
  END;
  ASSERT(Files.Pos(Exe) MOD 16 = 0);
  Files.Set(r, f, 0);
  WHILE ~r.eof DO
    Files.ReadBytes(r, buf, LEN(buf));
    Files.WriteBytes(Exe, buf, 0, LEN(buf) - r.res);
  END;
  FileAlign(Exe, 16);
END CopyFile;


PROCEDURE WriteModules;
VAR object: ObjectFile;
BEGIN
  object := Objects;
  WHILE object # NIL DO
    IF object.file.name # "WinHost.code" THEN CopyFile(object.file) END;
    object := object.next
  END;

  Files.WriteInt(Exe, 0);  (* Mark end of modules - appears as header.length = 0 *)

  (* Fill Oberon section to a whole multiple of section alignment *)
  (* by writing 0 to its last byte *)
  FileAlign(Exe, FileAlignment);

  OberonSize := Files.Pos(Exe) - FadrModules;
END WriteModules;


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
  hdr.sizeOfCode              := Align(OberonSize, FileAlignment);
  hdr.sizeOfInitializedData   := Align(ImportSize, MemoryAlignment);
  hdr.sizeOfUninitializedData := 0;
  hdr.addressOfEntryPoint     := RvaModules + Bootstrap.Header.initcode;
  hdr.baseOfCode              := RvaModules;

  (* Windows specific PE32+ fields *)
  hdr.imageBase               := ImageBase;
  hdr.MemoryAlignment         := MemoryAlignment;
  hdr.fileAlignment           := FileAlignment;
  hdr.majorOSVersion          := 1;
  hdr.majorSubsystemVersion   := 5;
  hdr.sizeOfImage             := Align(HeaderSize, MemoryAlignment)
                               + Align(OberonSize, MemoryAlignment)
                               + 4096;   (* import section *)
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
  hdr.importTableSize         := ImportSize;

  spos(0);
  Files.WriteBytes(Exe, hdr, 0, SYSTEM.SIZE(PEheader));
  (*w.DumpMem(2, SYSTEM.ADR(hdr), 0, SYSTEM.SIZE(PEheader));*)

  (* Write section headers *)
  WriteSectionHeader(".idata",
                     Align(ImportSize, MemoryAlignment),   (* Size in memory *)
                     Align(ImportSize, FileAlignment),     (* Size on disk *)
                     RvaImport, FadrImport,
                     SReadable + SWriteable + SInitialised);
  WriteSectionHeader("Oberon",
                     Align(OberonSize, MemoryAlignment),    (* Size in memory *)
                     Align(OberonSize, FileAlignment),      (* Size on disk *)
                     RvaModules, FadrModules,
                     SReadable + SWriteable + SExecutable + SCode);
END WritePEHeader;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE AddModule*(f: Files.File);
VAR obj: ObjectFile;
BEGIN
  ASSERT(f # NIL);
  NEW(obj); obj.file := f;
  IF Objects = NIL THEN Objects := obj ELSE LastObject.next := obj END;
  LastObject := obj
END AddModule;


PROCEDURE GetBootstrap;
VAR
  f: Files.File;
  r: Files.Rider;
BEGIN
  f := Objects.file;  Objects := Objects.next;  (* First object is the bootstrap *)
  Files.Set(r, f, 0);
  Files.ReadBytes(r, Bootstrap,  SYSTEM.SIZE(BootstrapBuffer));
  (*H.ws("Bootstrap bytes read: "); w.i(Files.Pos(r)); H.wsn(".");*)
  ASSERT(r.res >= 0);
  Files.Close(f)
END GetBootstrap;

PROCEDURE WriteZeroes(n: INTEGER);
VAR i: INTEGER;
BEGIN FOR i := 1 TO n DO Files.WriteByte(Exe, 0) END END WriteZeroes;

PROCEDURE WriteBootstrap(LoadFlags: SET);
BEGIN
  spos(FadrModules);
  Files.WriteBytes(Exe, Bootstrap, 0, Bootstrap.Header.imports);  (* Code and tables   *)

  (* Preset bootstrap modules global VARs *)
  Files.WriteInt(Exe, ImageBase);                                 (* EXE load address  *)
  Files.WriteInt(Exe, ImageBase + RvaModules);                    (* Header address    *)
  Files.WriteSet(Exe, LoadFlags);
  ASSERT(Files.Pos(Exe) -  (FadrModules + Bootstrap.Header.imports) = BootstrapVarBytes);

  (* Preset bootstrap VARs with WIndows proc addresses *)
  Files.WriteBytes(Exe, Idt.Kernel32Lookups, 0, Kernel32ImportCount * 8);
  Files.WriteBytes(Exe, Idt.User32Lookups,   0, User32ImportCount   * 8);
  Files.WriteBytes(Exe, Idt.Gdi32Lookups,  0, Gdi32ImportCount  * 8);

  WriteZeroes(Bootstrap.Header.varsize
            - ((Kernel32ImportCount + User32ImportCount + Gdi32ImportCount) * 8 + BootstrapVarBytes));
  FileAlign(Exe, 16)
END WriteBootstrap;


PROCEDURE Generate*(filename: ARRAY OF CHAR; LoadFlags: SET);
VAR fpos: INTEGER;
BEGIN
  Verbose := H.Verbose IN LoadFlags;

  IF Verbose THEN
    H.ws("WinPE.Generate. SIZE(CodeHeader) "); H.wh(SYSTEM.SIZE(CodeHeader));
    H.ws("H, SIZE(PEheader) "); H.wh(SYSTEM.SIZE(PEheader)); H.wsn("H.")
  END;

  ExeFile := Files.New(filename);

  GetBootstrap;
  WriteImports;
  WriteBootstrap(LoadFlags);
  WriteModules;
  WritePEHeader;
  Files.Register(ExeFile);

  IF Verbose THEN H.ws("WinPE generated "); H.ws(filename); H.wsn(".") END
END Generate;

PROCEDURE Init*;
BEGIN Objects := NIL;  LastObject := NIL  END Init;

BEGIN Init
END WinPE.
