MODULE Winshim;  IMPORT SYSTEM;

CONST
  (* Platform independent file open kinds *)
  OpenRO*      = 0;  (* Open r/o, fail if doesn't exist *)
  OpenRW*      = 1;  (* Open r/w, fail if doesn't exist *)

  (* System library procedure indices *)
  NewProc                = 0;
  (* error traps *)
  AssertionFailureProc   = 1;
  ArraySizeMismatchProc  = 2;
  UnterminatedStringProc = 3;

  MaxPath* = 780;  (* Enough UTF-8 bytes for for 260 wide chars *)


TYPE
  CodeHeaderPtr = POINTER TO CodeHeader;
  CodeHeader* = RECORD
    length*:   SYSTEM.CARD32;  (* File length *)
    initcode*: SYSTEM.CARD32;
    pointers*: SYSTEM.CARD32;
    commands*: SYSTEM.CARD32;
    exports*:  SYSTEM.CARD32;
    imports*:  SYSTEM.CARD32;  (* VARs start here following import resolution *)
    varsize*:  SYSTEM.CARD32;
    key*:      INTEGER;
  END;

  (* -------------------- Windows exception structures -------------------- *)

  Exception = POINTER- TO ExceptionRecord;
  ExceptionRecord = RECORD-
    ExceptionCode:        SYSTEM.CARD32;
    ExceptionFlags:       SYSTEM.CARD32;
    ExceptionRecord:      Exception;  (* Chained (nested) exception *)
    ExceptionAddress:     INTEGER;
    NumberParameters:     SYSTEM.CARD32;
    ExceptionInformation: ARRAY 15 OF INTEGER;  (* Addresses *)
  END;

  Context = POINTER- TO ContextRecord;
  ContextRecord = RECORD-
    P1Home, P2Home, P3Home, P4Home, P5Home, P6Home: INTEGER;             (*   0 *)
    ContextFlags, MxCsr:                            SYSTEM.CARD32;       (* 030 *)
    SegCs, SegDs, SegEs, SegFs, SegGs, SegSs:       SYSTEM.CARD16;       (* 038 *)
    EFlags:                                         SYSTEM.CARD32;       (* 044 *)
    Dr0, Dr1, Dr2, Dr3, Dr6, Dr7:                   INTEGER;             (* 048 *)
    rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi:         INTEGER;             (* 078 *)
    r8,  r9,  r10, r11, r12, r13, r14, r15, rip:    INTEGER;             (* 0B8 *)
    FloatingPointContext:                           ARRAY 64 OF INTEGER; (* 100 *)
    VectorRegisters:  (* 26 x M128A *)              ARRAY 52 OF INTEGER; (* 300 *)
    VectorControl:                                  INTEGER;             (* 4A0 *)
    DebugControl:                                   INTEGER;             (* 4A8 *)
    LastBranchToRip:                                INTEGER;             (* 4B0 *)
    LastBranchFromRip:                              INTEGER;             (* 4B8 *)
    LastExceptionToRip:                             INTEGER;             (* 4C0 *)
    LastExceptionFromRip:                           INTEGER;             (* 4C8 *)
  END;

  ExceptionPointers = POINTER- TO ExceptionPointersDesc;
  ExceptionPointersDesc = RECORD-
    exception: Exception;
    context:   Context
  END;


VAR
  (* WinPE.mod builds the executable with the following Winshim variables pre-loaded *)
  Exeadr: INTEGER;
  Header: CodeHeaderPtr;
  Dummy:  INTEGER;

  (* Pre-loaded Kernel32 imports *)
  AddVectoredExceptionHandler*:    PROCEDURE-(first, filter: INTEGER): INTEGER;
  CloseHandle*:                    PROCEDURE-(hObject: INTEGER): INTEGER;
  CreateFileW*:                    PROCEDURE-(lpFileName, dwDesiredAccess, dwShareMode, lpSecurityAttributes, dwCreationDisposition, dwFlagsAndAttributes, hTemplateFile: INTEGER): INTEGER;
  DeleteFileW*:                    PROCEDURE-(lpFilename: INTEGER): INTEGER;
  ExitProcess*:                    PROCEDURE-(exitcode: INTEGER);
  FileTimeToLocalFileTime*:        PROCEDURE-(filetime, localfiletime: INTEGER): INTEGER;
  FileTimeToSystemTime*:           PROCEDURE-(filetime, systemtime: INTEGER): INTEGER;
  FlushFileBuffers*:               PROCEDURE-(hFile: INTEGER): INTEGER;
  FormatMessageW:                  PROCEDURE-(flags, source, mid, lid, buf, size, args: INTEGER): INTEGER;
  GetCommandLineW*:                PROCEDURE-(): INTEGER;
  GetCurrentDirectoryW*:           PROCEDURE-(nsize, pbuffer: INTEGER): INTEGER;
  GetCurrentProcessId*:            PROCEDURE-(): INTEGER;
  GetEnvironmentVariableW*:        PROCEDURE-(lpName, lpBuffer, nSize: INTEGER): INTEGER;
  GetFileAttributesExW*:           PROCEDURE-(lpName, fInfoLevelId, lpFileInformation: INTEGER): INTEGER;  (* fInfoLevelId Must be 0 (GetFileExInfoStandard) *)
  GetFileAttributesW*:             PROCEDURE-(lpFileName: INTEGER): INTEGER;
  GetFileSizeEx*:                  PROCEDURE-(hFile, lpFileSize: INTEGER): INTEGER;
  GetLastError*:                   PROCEDURE-(): INTEGER;
  GetModuleFileNameW*:             PROCEDURE-(hModule, lpFilename, nSize: INTEGER): INTEGER;
  GetProcAddress*:                 PROCEDURE-(hmodule, procname: INTEGER): INTEGER;
  GetStdHandle*:                   PROCEDURE-(nStdHandle: SYSTEM.INT32): INTEGER;
  GetSystemTimePreciseAsFileTime*: PROCEDURE-(tickAdr: INTEGER);
  GetTempFileNameA*:               PROCEDURE-(pathadr, prefixadr, unique, tempfilenameadr: INTEGER): INTEGER;
  GetTempPathA*:                   PROCEDURE-(buflen, bufadr: INTEGER): INTEGER;
  LoadLibraryA*:                   PROCEDURE-(libname: INTEGER): INTEGER;
  MoveFileExW*:                    PROCEDURE-(lpExistingFileName, lpNewFileName, dwFlags: INTEGER): INTEGER;
  ReadFile*:                       PROCEDURE-(hFile, lpBuffer, nNumberOfBytesToRead, lpNumberOfBytesRead, lpOverlapped: INTEGER): INTEGER;
  SetConsoleOutputCP*:             PROCEDURE-(codepage: INTEGER) (* : INTEGER *);
  SetEndOfFile*:                   PROCEDURE-(hFile: INTEGER): INTEGER;
  SetFileInformationByHandle*:     PROCEDURE-(hFile, infoClass, info, bufsize: INTEGER): INTEGER;
  SetFilePointerEx*:               PROCEDURE-(hFile, liDistanceToMove, lpNewFilePointer, dwMoveMethod: INTEGER): INTEGER;
  UnmapViewOfFile:                 PROCEDURE-(adr: INTEGER): INTEGER;
  VirtualAlloc*:                   PROCEDURE-(address, size, type, protection: INTEGER): INTEGER;
  WriteFile*:                      PROCEDURE-(hFile, lpBuffer, nNumberOfBytesToWrite, lpNumberOfBytesWritten, lpOverlapped: INTEGER): INTEGER;

  (* Pre-loaded User32 imports *)
  MessageBoxA: PROCEDURE-(hWnd, lpText, lpCaption, uType: INTEGER)(*: INTEGER*);
  MessageBoxW: PROCEDURE-(hWnd, lpText, lpCaption, uType: INTEGER): INTEGER;

  (* Pre-loaded Shell32 imports *)
  CommandLineToArgvW: PROCEDURE-(lpCmdLine, pNumArgs: INTEGER): INTEGER;

  (* End of pre-loaded variables *)

  Stdin:      INTEGER;
  Stdout:     INTEGER;
  crlf*:      ARRAY 3 OF CHAR;
  Log*:       PROCEDURE(s: ARRAY OF BYTE);
  OberonAdr:  INTEGER;   (* Address of first module (Winshim.mod) *)
  LoadAdr:    INTEGER;   (* Where to load next module *)
  HWnd:       INTEGER;   (* Set if a window has been created *)

  (* System functions *)
  NewPointer*:        PROCEDURE(ptr, tag: INTEGER);
  AssertionFailure:   PROCEDURE;
  ArraySizeMismatch:  PROCEDURE;
  UnterminatedString: PROCEDURE;
  PostMortemDump:     PROCEDURE(modadr, offset, excpcode: INTEGER);

  ExceptionDepth: INTEGER;


PROCEDURE NoLog(s: ARRAY OF BYTE); BEGIN END NoLog;


(* -------------------------------------------------------------------------- *)
(* ---------------------- Very basic string functions ----------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Length*(s: ARRAY OF BYTE): INTEGER;
VAR l: INTEGER;
BEGIN  l := 0;  WHILE (l < LEN(s)) & (s[l] # 0) DO INC(l) END
RETURN l END Length;

PROCEDURE Append*(s: ARRAY OF CHAR; VAR d: ARRAY OF CHAR);
VAR i, j: INTEGER;
BEGIN
  j := Length(d);
  i := 0; WHILE (i < LEN(s)) & (j < LEN(d)) & (s[i] # 0X) DO
    d[j] := s[i];  INC(i);  INC(j)
  END;
  IF j >= LEN( d) THEN DEC(j) END;  d[j] := 0X
END Append;

PROCEDURE IntToHex*(n: INTEGER; VAR s: ARRAY OF CHAR);
VAR d, i, j: INTEGER;  ch: CHAR;
BEGIN
  i := 0;  j := 0;
  REPEAT
    d := n MOD 16;  n := n DIV 16 MOD 1000000000000000H;
    IF d <= 9 THEN s[j] := CHR(d + 48) ELSE s[j] := CHR(d + 55) END;
    INC(j)
  UNTIL n = 0;
  s[j] := 0X;  DEC(j);
  WHILE i < j DO ch:=s[i]; s[i]:=s[j]; s[j]:=ch; INC(i); DEC(j) END;
END IntToHex;

PROCEDURE IntToDecimal*(n: INTEGER; VAR s: ARRAY OF CHAR);
VAR i, j: INTEGER;  ch: CHAR;
BEGIN
  IF n = 8000000000000000H THEN s := "-9223372036854775808"
  ELSE i := 0;
    IF n < 0 THEN s[0] := "-";  i := 1;  n := -n END;
    j := i;
    REPEAT s[j] := CHR(n MOD 10 + 48);  INC(j);  n := n DIV 10 UNTIL n = 0;
    s[j] := 0X;  DEC(j);
    WHILE i < j DO ch:=s[i]; s[i]:=s[j]; s[j]:=ch; INC(i); DEC(j) END
  END
END IntToDecimal;


(* -------------------------------------------------------------------------- *)
(* ---------------- Simple logging/debugging console output ----------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE ws*(s: ARRAY OF CHAR); BEGIN Log(s) END ws;

PROCEDURE wc*(c: CHAR); BEGIN Log(c) END wc;

PROCEDURE wl*; BEGIN Log(crlf) END wl;

PROCEDURE wsl*(s: ARRAY OF CHAR); BEGIN Log(s);  Log(crlf) END wsl;

PROCEDURE wh*(n: INTEGER);
VAR hex: ARRAY 32 OF CHAR;
BEGIN IntToHex(n, hex);  Log(hex) END wh;

PROCEDURE whw*(n, w: INTEGER);
VAR hex: ARRAY 32 OF CHAR;  i: INTEGER;
BEGIN
  IntToHex(n, hex);  i := w - Length(hex);
  WHILE i > 0 DO wc("0"); DEC(i) END;
  Log(hex)
END whw;

PROCEDURE wb(n: INTEGER);
BEGIN WHILE n > 0 DO wc(" "); DEC(n) END END wb;


(* -------------------------------------------------------------------------- *)
(* ----------------------------- Time functions ----------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Nanotime*(): INTEGER;  (* In 100 nanosecond ticks since 1601 UTC *)
VAR tick: INTEGER;
BEGIN GetSystemTimePreciseAsFileTime(SYSTEM.ADR(tick));
RETURN tick END Nanotime;

PROCEDURE Time*(): INTEGER;  (* In millisecond ticks since 1601 UTC *)
VAR tick: INTEGER;
BEGIN GetSystemTimePreciseAsFileTime(SYSTEM.ADR(tick));
RETURN tick DIV 10000 END Time;

PROCEDURE Clock*(): INTEGER;
(* Returns 6/year,4/month,5/day,5/hour,6/minute,6/second local time*)
VAR
  tick, local, res: INTEGER;
  st: RECORD  (* Windows system time format *)
    year:         SYSTEM.CARD16;
    month:        SYSTEM.CARD16;
    dayofweek:    SYSTEM.CARD16;
    day:          SYSTEM.CARD16;
    hour:         SYSTEM.CARD16;
    minute:       SYSTEM.CARD16;
    second:       SYSTEM.CARD16;
    milliseconds: SYSTEM.CARD16;
  END;
  clock: INTEGER;
BEGIN
  clock := 0;
  GetSystemTimePreciseAsFileTime(SYSTEM.ADR(tick));
  res := FileTimeToLocalFileTime(SYSTEM.ADR(tick), SYSTEM.ADR(local));
  IF FileTimeToSystemTime(SYSTEM.ADR(local), SYSTEM.ADR(st)) # 0 THEN
    clock := ((  (((st.year MOD 100)*16 + st.month)*32 + st.day)*32
               + st.hour)*64 + st.minute)*64 + st.second;
  END
RETURN clock END Clock;

PROCEDURE LongClock*(): INTEGER;
(* Returns 13/0,15/year,4/month,5/day,5/hour,6/minute,6/second,10/millisecond *)
VAR
  tick, local, res: INTEGER;
  st: RECORD  (* Windows system time format *)
    year:         SYSTEM.CARD16;
    month:        SYSTEM.CARD16;
    dayofweek:    SYSTEM.CARD16;
    day:          SYSTEM.CARD16;
    hour:         SYSTEM.CARD16;
    minute:       SYSTEM.CARD16;
    second:       SYSTEM.CARD16;
    milliseconds: SYSTEM.CARD16;
  END;
  clock: INTEGER;
BEGIN
  clock := 0;
  GetSystemTimePreciseAsFileTime(SYSTEM.ADR(tick));
  res := FileTimeToLocalFileTime(SYSTEM.ADR(tick), SYSTEM.ADR(local));
  IF FileTimeToSystemTime(SYSTEM.ADR(local), SYSTEM.ADR(st)) # 0 THEN
    clock := (((  ((st.year*16 + st.month)*32 + st.day)*32
                + st.hour)*64 + st.minute)*64 + st.second)*1024 + st.milliseconds;
  END
RETURN clock END LongClock;


(* -------------------------------------------------------------------------- *)

PROCEDURE DumpMem*(indent, adr, start, len: INTEGER);
VAR
  rowstart:  INTEGER;
  dumplimit: INTEGER;
  i:         INTEGER;
  byte:      BYTE;
  bytes:     ARRAY 16 OF INTEGER;
BEGIN
  rowstart  := (       start       DIV 16) * 16;
  dumplimit := ((start + len + 15) DIV 16) * 16;
  WHILE rowstart < dumplimit DO
    wb(indent); whw(rowstart, 12); ws("  ");
    i := 0;
    WHILE i < 16 DO  (* Load a row of bytes *)
      IF (rowstart+i >= start) & (rowstart+i < start+len) THEN
        SYSTEM.GET(rowstart-start+adr+i, byte);  bytes[i] := byte
      ELSE
        bytes[i] := -1
      END;
      INC(i)
    END;
    i := 0;
    WHILE i < 16 DO  (* One row of hex Dump *)
      IF i MOD 8 = 0 THEN wc(" ") END;
      IF bytes[i] >= 0 THEN whw(bytes[i], 2);  wc(" ") ELSE ws("   ") END;
      INC(i)
    END;
    ws("  ");
    i := 0;
    WHILE i < 16 DO  (* One row of character Dump *)
      IF bytes[i] >= 0 THEN
        IF (bytes[i] < 32) OR (bytes[i] >= 127) THEN wc(".") ELSE wc(CHR(bytes[i])) END
      ELSE
        wc(" ")
      END;
      INC(i)
    END;
    wl;  INC(rowstart, 16);
  END
END DumpMem;


(* -------------------------------------------------------------------------- *)
(* ------------------------ Bootstrap initialisation ------------------------ *)
(* -------------------------------------------------------------------------- *)

(* -------------------------------------------------------------------------- *)
(* This code runs in-place before the Oberon 4GB memory is reserved           *)
(* System functions have not been set up, meaning                             *)
(*   The code may not use system functions such as NEW, ASSERT etc.           *)
(*   Faults like Array size mismatch or unterminated string will crash        *)
(* Any global variables set to code addresses must be reset after Winshim is   *)
(*   moved to Oberon memory.                                                  *)
(* -------------------------------------------------------------------------- *)

PROCEDURE PrepareOberonMachine;
CONST
  MEMRESERVE           = 2000H;
  MEMCOMMIT            = 1000H;
  PAGEEXECUTEREADWRITE = 40H;
VAR
  reserveadr:   INTEGER;
  moduleadr:    INTEGER;
  modulesize:   INTEGER;        (* loaded length including global vars *)
  modulelength: SYSTEM.CARD32;  (* image length from code file         *)
  bootsize:     INTEGER;
  res:          INTEGER;
  hdr:          CodeHeaderPtr;
BEGIN
  (* Reserve 2GB memory for the Oberon machine + 2GB for Jcc trap targets *)
  reserveadr := VirtualAlloc(100000000H, 100000000H, MEMRESERVE, PAGEEXECUTEREADWRITE);
  IF reserveadr = 0 THEN
    wsl("Could not reserve Oberon machine memory.");  ExitProcess(9);
  ELSE
    ws("Reserved 4GB Oberon machine memory at ");  wh(reserveadr);  wsl("H.")
  END;

  (* Determine loaded size of all modules *)
  bootsize   := Header.imports + Header.varsize;
  (*ws("bootsize "); wh(bootsize); wsl("H.");*)
  modulesize := bootsize;
  (*ws("modulesize "); wh(modulesize); wsl("H.");*)
  moduleadr  := (SYSTEM.VAL(INTEGER, Header) + bootsize + 15) DIV 16 * 16;  (* Address of first module for Oberon machine *)
  (*
  ws("Looking for modules to load starting at "); wh(moduleadr); wsl("H.");
  WriteModuleHeader(moduleadr);
  *)
  hdr        := SYSTEM.VAL(CodeHeaderPtr, moduleadr);
  (*ws("Potential first module length "); wh(hdr.length); wsl("H.");*)
  WHILE hdr.length > 0 DO
    (*
    ws("Module at "); wh(moduleadr); ws("H, length "); wh(hdr.length); wsl("H.");
    WriteModuleHeader(moduleadr);
    *)
    INC(modulesize, (hdr.imports + hdr.varsize + 15) DIV 16 * 16);
    INC(moduleadr, hdr.length);
    hdr := SYSTEM.VAL(CodeHeaderPtr, moduleadr);
  END;

  (* Commit enough for the modules being loaded plus a sentinel. *)
  INC(modulesize, 16);  (* Allow 16 bytes ofr a sentinel *)
  OberonAdr := VirtualAlloc(reserveadr, modulesize, MEMCOMMIT, PAGEEXECUTEREADWRITE);
  ws("Committed ");  wh(modulesize);  ws("H bytes at ");  wh(OberonAdr);  wsl("H.");
END PrepareOberonMachine;


(* -------------------------------------------------------------------------- *)

PROCEDURE WriteModuleName(adr: INTEGER);
VAR hdr: CodeHeaderPtr;  ch: CHAR;
BEGIN
  hdr := SYSTEM.VAL(CodeHeaderPtr, adr);
  INC(adr, SYSTEM.SIZE(CodeHeader));
  SYSTEM.GET(adr, ch);
  WHILE ch # 0X DO wc(ch);  INC(adr);  SYSTEM.GET(adr, ch) END;
END WriteModuleName;

PROCEDURE WriteModuleHeader(adr: INTEGER);
VAR hdr: CodeHeaderPtr;  ch: CHAR;
BEGIN
  hdr := SYSTEM.VAL(CodeHeaderPtr, adr);
  INC(adr, SYSTEM.SIZE(CodeHeader));
  SYSTEM.GET(adr, ch);
  WHILE ch # 0X DO wc(ch);  INC(adr);  SYSTEM.GET(adr, ch) END;
  ws(" header at ");  wh(SYSTEM.VAL(INTEGER, hdr));  wsl("H:");
  ws("  length:   ");  wh(hdr.length);    wsl("H.");
  ws("  initcode: ");  wh(hdr.initcode);  wsl("H.");
  ws("  pointers: ");  wh(hdr.pointers);  wsl("H.");
  ws("  commands: ");  wh(hdr.commands);  wsl("H.");
  ws("  exports:  ");  wh(hdr.exports);   wsl("H.");
  ws("  imports:  ");  wh(hdr.imports);   wsl("H.");
  ws("  varsize:  ");  wh(hdr.varsize);   wsl("H.");
  ws("  key:      ");  wh(hdr.key);       wsl("H.");
END WriteModuleHeader;

PROCEDURE WriteExports(modadr: INTEGER);
VAR hdr: CodeHeaderPtr; adr: INTEGER; export: SYSTEM.CARD32;
BEGIN
  ws("Export table at "); wh(modadr); wsl("H.");
  adr := modadr;
  SYSTEM.GET(adr, export);  INC(adr, 4);
  WHILE export # 0FFFFFFFFH DO
    ws("  export "); wh(export); wsl("H.");
    SYSTEM.GET(adr, export);  INC(adr, 4);
  END
END WriteExports;


(* ----------------------- Windows exception handling ----------------------- *)

PROCEDURE LocateModule(adr: INTEGER): INTEGER;
VAR  modadr: INTEGER;  hdr: CodeHeaderPtr;
BEGIN
  modadr := OberonAdr;  hdr := SYSTEM.VAL(CodeHeaderPtr, modadr);
  WHILE (hdr.length # 0) & (modadr + hdr.imports < adr) DO
    modadr := (modadr + hdr.imports + hdr.varsize + 15) DIV 16 * 16;
    hdr    := SYSTEM.VAL(CodeHeaderPtr, modadr);
  END;
  IF (adr < modadr) OR (adr > modadr + hdr.imports) THEN modadr := 0 END;
RETURN modadr END LocateModule;

PROCEDURE WriteModuleOffset*(adr: INTEGER);
VAR modadr: INTEGER;
BEGIN
  modadr := LocateModule(adr);
  IF modadr # 0  THEN
    ws(" in module "); WriteModuleName(modadr);
    ws(" at offset "); wh(adr - modadr); wc("H")
  END
END WriteModuleOffset;

PROCEDURE- ExceptionHandler(p: ExceptionPointers);  (* Called by Windows *)
VAR modadr, excpadr, excpcode: INTEGER;
BEGIN
  excpcode := p.exception.ExceptionCode;
  excpadr  := p.exception.ExceptionAddress;
  wl;

  IF    excpcode = 080000003H THEN ws("** Breakpoint (INT 3)");
  ELSIF excpcode = 080000004H THEN ws("** Single step (0F1H instr)");
  ELSIF excpcode = 0C0000005H THEN ws("** Access violation");
  ELSIF excpcode = 0C0000006H THEN ws("** In-page error");
  ELSIF excpcode = 0C000001DH THEN ws("** Illegal instruction");
  ELSIF excpcode = 0C000008EH THEN ws("** Divide by zero");
  ELSIF excpcode = 0C0000094H THEN ws("** Integer divide by zero");
  ELSE ws("** Exception ");  wh(excpcode);  wc("H")
  END;
  ws(" at address ");  wh(excpadr); wc("H");
  modadr := LocateModule(excpadr);
  IF modadr # 0  THEN
    ws(" in module "); WriteModuleName(modadr);
    ws(" at offset "); wh(excpadr - modadr); wc("H")
  END;
  wsl(". **");

  IF (modadr # 0) & (PostMortemDump # NIL) & (ExceptionDepth < 2) THEN
    INC(ExceptionDepth);
    PostMortemDump(modadr, excpadr - modadr, excpcode)
  ELSE
    ws("  rax "); whw(p.context.rax, 16);  ws("  rbx "); whw(p.context.rbx, 16);
    ws("  rcx "); whw(p.context.rcx, 16);  ws("  rdx "); whw(p.context.rdx, 16);  wl;
    ws("  rsp "); whw(p.context.rsp, 16);  ws("  rbp "); whw(p.context.rbp, 16);
    ws("  rsi "); whw(p.context.rsi, 16);  ws("  rdi "); whw(p.context.rdi, 16);  wl;
    ws("  r8  "); whw(p.context.r8,  16);  ws("  r9  "); whw(p.context.r9,  16);
    ws("  r10 "); whw(p.context.r10, 16);  ws("  r11 "); whw(p.context.r11, 16);  wl;
    ws("  r12 "); whw(p.context.r12, 16);  ws("  r13 "); whw(p.context.r13, 16);
    ws("  r14 "); whw(p.context.r14, 16);  ws("  r15 "); whw(p.context.r15, 16);  wl;
    (* Dump top of stack (i.e. lowest addresses) *)
    DumpMem(2, p.context.rsp, p.context.rsp, 128)
  END;

  ExitProcess(99)
END ExceptionHandler;


(* ----------------------------- Trap handlers ------------------------------ *)

PROCEDURE Trap(desc: ARRAY OF CHAR);
VAR adr, modadr: INTEGER;
BEGIN
  wsl(desc);
  SYSTEM.GET(SYSTEM.ADR(LEN(desc)) + 8, adr);  (* Get caller address of trap caller *)
  ws("At address ");  wh(adr);  ws("H");
  modadr := LocateModule(adr);
  IF modadr # 0  THEN
    ws(" in module "); WriteModuleName(modadr);
    ws(" at offset "); wh(adr - modadr); wc("H")
  END;
  wsl(".");
  IF (modadr # 0) & (PostMortemDump # NIL) & (ExceptionDepth < 2) THEN
    INC(ExceptionDepth);
    PostMortemDump(modadr, adr - modadr, -1)
  END;
  ExitProcess(99)
END Trap;

PROCEDURE AssertionFailureHandler();
BEGIN wl; Trap("** Assertion failure **") END AssertionFailureHandler;

PROCEDURE ArraySizeMismatchHandler();
BEGIN wl; Trap("** Array size mismatch **") END ArraySizeMismatchHandler;

PROCEDURE UnterminatedStringHandler();
BEGIN wl; Trap("** Unterminated string **") END UnterminatedStringHandler;

PROCEDURE NewPointerHandler(ptr, len: INTEGER);
BEGIN
  Log("NewPointer called.");  Log(crlf);
  ExitProcess(99)
END NewPointerHandler;


(* ------------------ Winshim internal assertion handlers ------------------- *)

PROCEDURE assertmsg(expectation: BOOLEAN; msg: ARRAY OF CHAR);
VAR res: INTEGER;
BEGIN
  IF ~expectation THEN
    ws(" * "); ws(msg); wsl(" *");
    res := CloseHandle(Stdout);
    ExitProcess(99)
  END
END assertmsg;

PROCEDURE assert(expectation: BOOLEAN);
BEGIN IF ~expectation THEN Trap("** Winshim assertion failure **") END
END assert;


(* -------------------------------------------------------------------------- *)
(* ------------ Unicode Transformation Formats UTF-8 and UTF-16 ------------- *)
(* -------------------------------------------------------------------------- *)

(* UTF-8:                                                                                           *)
(* -------------- codepoint --------------    ----------------------- bytes ----------------------- *)
(* 0000 0000 0000 0000 0000 0000 0zzz zzzz    0zzzzzzz                                              *)
(* 0000 0000 0000 0000 0000 0yyy yyzz zzzz    110yyyyy 10zzzzzz                                     *)
(* 0000 0000 0000 0000 xxxx yyyy yyzz zzzz    1110xxxx 10yyyyyy 10zzzzzz                            *)
(* 0000 0000 000w wwxx xxxx yyyy yyzz zzzz    11110www 10xxxxxx 10yyyyyy 10zzzzzz                   *)
(* The below are beyond the range of valid Unicode codepoints                                       *)
(* 0000 00vv wwww wwxx xxxx yyyy yyzz zzzz    111110vv 10wwwwww 10xxxxxx 10yyyyyy 10zzzzzz          *)
(* 0uvv vvvv wwww wwxx xxxx yyyy yyzz zzzz    1111110u 10vvvvvv 10wwwwww 10xxxxxx 10yyyyyy 10zzzzzz *)

PROCEDURE GetUtf8*(src: ARRAY OF CHAR; VAR i: INTEGER): INTEGER;
VAR n, result: INTEGER;
BEGIN assert(i < LEN(src)); result := ORD(src[i]);  INC(i);
  IF result >= 0C0H THEN
    IF    result >= 0FCH THEN result := result MOD 2;  n := 5
    ELSIF result >= 0F8H THEN result := result MOD 4;  n := 4
    ELSIF result >= 0F0H THEN result := result MOD 8;  n := 3
    ELSIF result >= 0E0H THEN result := result MOD 16; n := 2
    ELSE                      result := result MOD 32; n := 1
    END;
    WHILE n > 0 DO
      result := LSL(result,6);  DEC(n);
      IF (i < LEN(src)) & (ORD(src[i]) DIV 40H = 2) THEN
        INC(result, ORD(src[i]) MOD 40H);  INC(i)
      END
    END
  END;
RETURN result END GetUtf8;

PROCEDURE PutUtf8*(c: INTEGER; VAR dst: ARRAY OF CHAR; VAR i: INTEGER);
VAR n: INTEGER;
BEGIN
  assert(i < LEN(dst));
  assert(c > 0);  assert(c < 80000000H);
  IF i < LEN(dst) THEN
    IF c < 80H THEN dst[i] := CHR(c);  INC(i)
    ELSE
      IF    c < 800H     THEN  dst[i] := CHR(0C0H + ASR(c, 6));    n := 1;
      ELSIF c < 10000H   THEN  dst[i] := CHR(0E0H + ASR(c, 12));   n := 2;
      ELSIF c < 200000H  THEN  dst[i] := CHR(0F0H + ASR(c, 18));   n := 3;
      ELSIF c < 4000000H THEN  dst[i] := CHR(0F8H + ASR(c, 24));   n := 4;
      ELSE                     dst[i] := CHR(0FCH + ASR(c, 30));   n := 5;
      END;
      INC(i);
      WHILE (n > 0) & (i < LEN(dst)) DO
        DEC(n);  dst[i] := CHR(80H + ASR(c, n*6) MOD 40H);  INC(i)
      END;
    END
  END
END PutUtf8;


(* UTF-16:                                                                      *)
(* -------------- codepoint --------------    ------------- words ------------- *)
(* 0000 0000 0000 0000 zzzz zzzz zzzz zzzz    zzzzzzzzzzzzzzzz                  *)
(* 0000 0000 000x xxxx yyyy yyzz zzzz zzzz    110110wwwwyyyyyy 110111zzzzzzzzzz *)
(* Where xxxxx is 1-16, and wwww is xxxxx-1 (0-15).                             *)

PROCEDURE GetUtf16*(src: ARRAY OF SYSTEM.CARD16; VAR i: INTEGER): INTEGER;
VAR result: INTEGER;
BEGIN
  assert(i < LEN(src));
  result := src[i];  INC(i);
  IF result DIV 400H = 36H THEN    (* High surrogate *)
    result := LSL(result MOD 400H, 10) + 10000H;
    IF (i < LEN(src)) & (src[i] DIV 400H = 37H) THEN  (* Low surrogate *)
      INC(result, src[i] MOD 400H);  INC(i)
    END
  END
RETURN result END GetUtf16;

PROCEDURE PutUtf16*(ch: INTEGER; VAR dst: ARRAY OF SYSTEM.CARD16; VAR i: INTEGER);
BEGIN
  assert(i < LEN(dst));
  IF (ch < 10000H) & (i < LEN(dst)) THEN
    dst[i] := ch;  INC(i)
  ELSIF i+1 < LEN(dst) THEN
    DEC(ch, 10000H);
    dst[i] := 0D800H + ch DIV 400H;  INC(i);
    dst[i] := 0DC00H + ch MOD 400H;  INC(i);
  END
END PutUtf16;


PROCEDURE Utf8ToUtf16*(src: ARRAY OF CHAR;  VAR dst: ARRAY OF SYSTEM.CARD16): INTEGER;
VAR i, j: INTEGER;
BEGIN  i := 0;  j := 0;
  WHILE (i < LEN(src)) & (src[i] # 0X) DO PutUtf16(GetUtf8(src, i), dst, j) END;
  IF j < LEN(dst) THEN dst[j] := 0;  INC(j) END
RETURN j END Utf8ToUtf16;

PROCEDURE Utf16ToUtf8*(src: ARRAY OF SYSTEM.CARD16;  VAR dst: ARRAY OF CHAR): INTEGER;
VAR i, j: INTEGER;
BEGIN  i := 0;  j := 0;
  WHILE (i < LEN(src)) & (src[i] # 0) DO PutUtf8(GetUtf16(src, i), dst, j) END;
  IF j < LEN(dst) THEN dst[j] := 0X;  INC(j) END
RETURN j END Utf16ToUtf8;


(* -------------------------------------------------------------------------- *)

PROCEDURE WriteWindowsErrorMessage(err: INTEGER);
VAR
  msgW:    ARRAY 1024 OF SYSTEM.CARD16;
  msg:     ARRAY 1024 OF CHAR;
  i, res:  INTEGER;
BEGIN
  res := FormatMessageW(1000H, 0, err, 0, SYSTEM.ADR(msgW), 1024, 0); (* 1000H: FORMAT_MESSAGE_FROM_SYSTEM *)
  res := Utf16ToUtf8(msgW, msg);
  i := Length(msg) - 1;
  WHILE (i >= 0) & (msg[i] <= " ") DO DEC(i) END;  (* Drop trailing newline(s) *)
  msg[i + 1] := 0X;
  ws(msg)
END WriteWindowsErrorMessage;

PROCEDURE AssertWinErr*(err: INTEGER);
BEGIN
  IF err # 0 THEN
    wl; ws("** "); WriteWindowsErrorMessage(err); Trap(" **")
  END
END AssertWinErr;


(* -------------------------------------------------------------------------- *)
(* ---------------- Last resort error reporting - MessageBox ---------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE MessageBox*(title, msg: ARRAY OF CHAR);
VAR
  res:     INTEGER;
  title16: ARRAY 256 OF SYSTEM.CARD16;
  msg16:   ARRAY 256 OF SYSTEM.CARD16;
BEGIN
  res := Utf8ToUtf16(title, title16);
  res := Utf8ToUtf16(msg,   msg16);
  res := MessageBoxW(HWnd, SYSTEM.ADR(msg16), SYSTEM.ADR(title16), 0)
END MessageBox;


(* -------------------------------------------------------------------------- *)
(* ------------- Platform independent low level file operations ------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE FileOpen*(name: ARRAY OF CHAR; openkind: INTEGER; VAR handle: INTEGER): INTEGER;
VAR
  name16:      ARRAY 261 OF SYSTEM.CARD16;
  disposition: INTEGER;
  flags:       INTEGER;
  access:      INTEGER;
  mode:        INTEGER;
  res:         INTEGER;
BEGIN
  disposition := 3;         (* Open existing *)
  flags       := 0;
  IF openkind = OpenRO THEN
    access := 080000000H;  (* GenericRead *)
    mode   := 1;           (* FileShareRead *)
  ELSE
    access := 010000000H;  (* GenericAll (includes delete access) *)
    mode   := 7;           (* FileShareRead, FileShareWrite, FileShareDelete *)
  END;
  res := Utf8ToUtf16(name, name16);
  res := CreateFileW(SYSTEM.ADR(name16), access, mode, 0, disposition, flags, 0);
  IF res < 0 THEN
    handle := 0;  res := GetLastError()
  ELSE
    handle := res;  res := 0
  END
RETURN res END FileOpen;

PROCEDURE MoveFile*(source, dest: ARRAY OF CHAR): INTEGER;
VAR
  sourcew: ARRAY MaxPath OF SYSTEM.CARD16;
  destw:   ARRAY MaxPath OF SYSTEM.CARD16;
  res:     INTEGER;
BEGIN
  res := Utf8ToUtf16(source, sourcew);  assert(res > 0);
  res := Utf8ToUtf16(dest,   destw);    assert(res > 0);
  res := MoveFileExW(SYSTEM.ADR(sourcew), SYSTEM.ADR(destw), 3);  (* 1 => replace existing, 2 => copy allowed *)
  IF res = 0 THEN res := GetLastError() ELSE res := 0 END
RETURN res END MoveFile;


(* -------------------------------------------------------------------------- *)

PROCEDURE WriteStdout(s: ARRAY OF BYTE);
VAR written, result: INTEGER;
BEGIN
  result := WriteFile(Stdout, SYSTEM.ADR(s), Length(s), SYSTEM.ADR(written), 0);
END WriteStdout;


(* -------------------------------------------------------------------------- *)

PROCEDURE GetString(adr: INTEGER; VAR s: ARRAY OF CHAR; VAR len: INTEGER);
VAR i: INTEGER;
BEGIN i := 0;
  (*ws("GetString -> '");*)
  REPEAT SYSTEM.GET(adr, s[i]); INC(adr); INC(i) UNTIL s[i-1] = 0X;
  len := i;
  (*ws(s); wsl("'.")*)
END GetString;


(* -------------------------------------------------------------------------- *)

PROCEDURE ExportedAddress(modhdr: CodeHeaderPtr; index: INTEGER): INTEGER;
VAR exportoffset: SYSTEM.CARD32;
BEGIN
  SYSTEM.GET(SYSTEM.VAL(INTEGER, modhdr) + modhdr.exports + index * 4, exportoffset);
RETURN SYSTEM.VAL(INTEGER, modhdr) + exportoffset END ExportedAddress;

PROCEDURE FindModule(name: ARRAY OF CHAR; key: INTEGER): INTEGER;
VAR
  hdr:     CodeHeaderPtr;
  modadr:  INTEGER;
  modname: ARRAY 32 OF CHAR;
  len:     INTEGER;
BEGIN
  (*ws("Findmodule "); ws(name); wsl(".");*)
  modadr := OberonAdr;
  hdr := SYSTEM.VAL(CodeHeaderPtr, modadr);
  GetString(modadr + SYSTEM.SIZE(CodeHeader), modname, len);
  WHILE (hdr.length # 0) & (modname # name) DO
    modadr := (modadr + hdr.imports + hdr.varsize + 15) DIV 16 * 16;
    (*ws(".. considering ");  WriteModuleName(modadr); wl;*)
    hdr := SYSTEM.VAL(CodeHeaderPtr, modadr);
    IF hdr.length > 0 THEN
      GetString(modadr + SYSTEM.SIZE(CodeHeader), modname, len)
    END
  END;
  assert(hdr.length # 0); (*, "FindModule hdr.length is 0.");*)
  (*ws("Requested key "); wh(hdr.key); ws("H, found key "); wh(hdr.key); wsl("H.");*)
RETURN modadr END FindModule;


PROCEDURE LoadModule(modadr: INTEGER; VAR bodyadr: INTEGER);
(* Load module whose code image is at modadr *)
(* Module is loaded at LoadAdr which is then updated *)
VAR
  adr:         INTEGER;
  loadedsize:  SYSTEM.CARD32;
  hdr:         CodeHeaderPtr;
  impmod:      ARRAY 32 OF CHAR;
  modules:     ARRAY 64 OF INTEGER; (* Import from up to 64 modules *)
  i, len:      INTEGER;
  key:         INTEGER;
  importcount: SYSTEM.CARD32;
  offset:      SYSTEM.CARD32;
  disp:        SYSTEM.INT32;
  modno:       SYSTEM.CARD16;
  impno:       SYSTEM.CARD16;
  impmodadr:   INTEGER;  (* Module being imported from base address *)
  expadr:      INTEGER;  (* Address relative to imported module of an export *)
  modulebody:  PROCEDURE;
BEGIN
  (*
  ws("Loading ");  WriteModuleName(modadr);
  ws(" from ");    wh(modadr);
  ws("H to ");     wh(LoadAdr);  wsl("H.");
  WriteModuleHeader(modadr);
  *)

  hdr := SYSTEM.VAL(CodeHeaderPtr, modadr);
  SYSTEM.COPY(modadr, LoadAdr, hdr.imports);  (* Copy up to but excluding import table *)

  loadedsize := (hdr.imports + hdr.varsize + 15) DIV 16 * 16;
  (*ws("Loaded size "); wh(loadedsize); wsl("H.");*)
  SYSTEM.PUT(LoadAdr, loadedsize);      (* Update length in header to loaded size *)
  (*ws("Writing sentinel at "); wh(LoadAdr + loadedsize); wsl("H.");*)
  SYSTEM.PUT(LoadAdr + loadedsize, 0);  (* Add sentinel zero length module *)

  ws("Loaded ");          WriteModuleName(modadr);
  ws(" at ");             wh(LoadAdr);
  ws("H, code ");         wh(hdr.imports);
  ws("H bytes, data ");   wh(hdr.varsize);
  ws("H bytes, limit ");  wh(LoadAdr + loadedsize);  wsl("H.");

  (* Build list of imported module header addresses *)
  i := 0;
  adr := modadr + hdr.imports;
  GetString(adr, impmod, len);  INC(adr, len);
  WHILE impmod[0] # 0X DO
    SYSTEM.GET(adr, key);  INC(adr, 8);
    modules[i] := FindModule(impmod, key);
    INC(i);
    GetString(adr, impmod, len);  INC(adr, len)
  END;
  modules[i] := 0;

  (*wsl("Built list of imported module header addresses.");*)

  adr := (adr + 15) DIV 16 * 16;
  SYSTEM.GET(adr, importcount);  INC(adr, 4);
  (*ws("Import count "); wh(importcount); wsl("H.");*)
  i := 0;
  WHILE i < importcount DO
    SYSTEM.GET(adr, offset); INC(adr, 4);
    SYSTEM.GET(adr, impno);  INC(adr, 2);
    SYSTEM.GET(adr, modno);  INC(adr, 2);
    (*
    ws("  import from module "); wh(modno);
    ws("H, impno "); wh(impno);
    ws("H, to offset "); wh(offset); wsl("H.");
    *)
    IF modno = 0 THEN  (* system function *)
      SYSTEM.GET(LoadAdr + offset, disp);
      (*ws("disp    -"); wh(-disp); wsl("H.");*)
      IF    impno = NewProc                THEN disp := SYSTEM.ADR(NewPointer)         + disp - LoadAdr
      ELSIF impno = AssertionFailureProc   THEN disp := SYSTEM.ADR(AssertionFailure)   + disp - LoadAdr
      ELSIF impno = ArraySizeMismatchProc  THEN disp := SYSTEM.ADR(ArraySizeMismatch)  + disp - LoadAdr
      ELSIF impno = UnterminatedStringProc THEN disp := SYSTEM.ADR(UnterminatedString) + disp - LoadAdr
      ELSE  assert(FALSE) (*, "LoadModule: Unexpected system function import number.")*)
      END;
      (*ws("disp'   -"); wh(-disp); wsl("H.");*)
      SYSTEM.PUT(LoadAdr + offset, disp)
    ELSE
      assert(modno > 0); (*, "LoadModule: modno is < 0.");*)
      impmodadr := modules[modno-1];
      expadr := ExportedAddress(SYSTEM.VAL(CodeHeaderPtr, impmodadr), impno-1);
      (*
      ws("expadr  "); wh(expadr); wsl("H.");
      ws("LoadAdr "); wh(LoadAdr); wsl("H.");
      ws("offset  "); wh(offset); wsl("H.");
      *)
      SYSTEM.GET(LoadAdr + offset, disp);
      (*ws("disp    -"); wh(-disp); wsl("H.");*)
      disp := expadr + disp - LoadAdr;
      (*ws("disp'   -"); wh(-disp); wsl("H.");*)
      SYSTEM.PUT(LoadAdr + offset, disp)
    END;
    INC(i)
  END;

  bodyadr := LoadAdr + hdr.initcode;
  INC(LoadAdr, loadedsize)
END LoadModule;

PROCEDURE IncPC(increment: INTEGER);  (* Update return address by increment *)
VAR pc: INTEGER;
BEGIN
  SYSTEM.GET(SYSTEM.ADR(pc) + 8, pc);
  SYSTEM.PUT(SYSTEM.ADR(pc) + 8, pc + increment);
END IncPC;

PROCEDURE GetPC(): INTEGER;
VAR pc: INTEGER;
BEGIN SYSTEM.GET(SYSTEM.ADR(pc) + 8, pc);
RETURN pc END GetPC;

(* -------------------------------------------------------------------------- *)

PROCEDURE LoadRemainingModules;
VAR
  moduleadr:    INTEGER;
  modulelength: SYSTEM.CARD32;
  bodyadr:      INTEGER;
  body:         PROCEDURE;
  res:          INTEGER;
BEGIN
  (* Load and link remaining code modules from EXE file image *)
  moduleadr := SYSTEM.VAL(INTEGER, Header) + Header.imports + Header.varsize;  (* Address of first module for Oberon machine *)
  moduleadr := (moduleadr + 15) DIV 16 * 16;

  ws("Load remaining modules starting from "); wh(moduleadr);
  ws("H, RvaModules + "); wh(moduleadr - SYSTEM.VAL(INTEGER, Header)); wsl("H.");
  ws("First remaining module: '"); WriteModuleName(moduleadr); wsl("'.");

  SYSTEM.GET(moduleadr, modulelength);
  WHILE modulelength # 0 DO
    LoadModule(moduleadr, bodyadr);
    SYSTEM.PUT(SYSTEM.ADR(body), bodyadr);
    moduleadr := (moduleadr + modulelength + 15) DIV 16 * 16;
    SYSTEM.GET(moduleadr, modulelength);
    IF modulelength = 0 THEN
      (* This is the last module. We have no further need for the executable *)
      (* image so unmap it before running the module body.                   *)
      res := UnmapViewOfFile(Exeadr)
    END;
    body
  END;

  SYSTEM.PUT(LoadAdr, 0);  (* Mark end of loaded modules *)
  (*wsl("LoadRemainingModules complete.")*)
END LoadRemainingModules;

(* -------------------------------------------------------------------------- *)


BEGIN
  HWnd               := 0;
  Log                := NoLog;
  ExceptionDepth     := 0;
  NewPointer         := NIL;
  AssertionFailure   := NIL;
  ArraySizeMismatch  := NIL;
  UnterminatedString := NIL;
  PostMortemDump     := NIL;

  (* Initialise console input/output *)
  Stdin  := GetStdHandle(-10);  (* -10:   StdInputHandle *)
  Stdout := GetStdHandle(-11);  (* -11:   StdOutputHandle *)
  SetConsoleOutputCP(65001);    (* 65001: UTF8            *)
  crlf := $0D 0A 00$;

  Log := WriteStdout;

  ws("Winshim starting, Header at "); wh(SYSTEM.VAL(INTEGER, Header)); wsl("H.");
  (*
  ws("Stdout handle "); wh(Stdout);            wsl("H.");
  ws("NoLog at ");      wh(SYSTEM.ADR(NoLog)); wsl("H.");
  ws("Log at ");        wh(SYSTEM.ADR(Log));   wsl("H.");
  WriteModuleHeader(SYSTEM.VAL(INTEGER, Header));
  *)

  PrepareOberonMachine;

  (* Copy boot module into newly committed memory and switch PC to the new code. *)
  SYSTEM.COPY(SYSTEM.VAL(INTEGER, Header), OberonAdr, Header.imports + Header.varsize);
  (*ws("Transferring PC from original load at ");  wh(GetPC());  wsl("H.");*)
  IncPC(OberonAdr - SYSTEM.VAL(INTEGER, Header));  (* Transfer to copied code *)
  Log := WriteStdout;  (* Correct Log fn address following move *)
  ws("Transferred PC to code copied to Oberon memory at ");  wh(GetPC());  wsl("H.");

  (* Initialise system fuction handlers *)
  NewPointer         := NewPointerHandler;
  AssertionFailure   := AssertionFailureHandler;
  ArraySizeMismatch  := ArraySizeMismatchHandler;
  UnterminatedString := UnterminatedStringHandler;

  (* Trap OS exceptions *)
  (*res := AddVectoredExceptionHandler(1, SYSTEM.ADR(ExceptionHandler));*)
  (*IF res = 0 THEN AssertWinErr(GetLastError()) END;*)

  LoadAdr := (OberonAdr + Header.imports + Header.varsize + 15) DIV 16 * 16;

  (*ws("crlf at "); wh(SYSTEM.ADR(crlf)); wsl("H.");*)

  LoadRemainingModules;

  (*MessageBoxA(0, SYSTEM.ADR("Complete."), SYSTEM.ADR("Winshim"), 0);*)
  (*wsl("Winshim complete.");*)
  (*MessageBox("Winshim", "Complete");*)

  ExitProcess(0);
END Winshim.
