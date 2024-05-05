MODULE WinHost;  IMPORT SYSTEM;
CONST
  (* Platform independent file open kinds *)
  OpenRO* = 0;  (* Open r/o, fail if doesn't exist *)
  OpenRW* = 1;  (* Open r/w, fail if doesn't exist *)

  (* System library procedure indices *)
  NewProc*                   = 0;
  (* error traps *)
  AssertionFailureProc*      = 1;
  ArraySizeMismatchProc*     = 2;
  UnterminatedStringProc*    = 3;
  IndexOutOfRangeProc*       = 4;
  NilPointerDereferenceProc* = 5;
  TypeGuardFailureProc*      = 6;
  HandlerCount*              = 7;

  MaxPath* = 780;  (* Enough UTF-8 bytes for for 260 wide chars *)

  (* LoadFlags *)
  Verbose*    = 0;
  LoadOberon* = 62;
  NewLoad*    = 63;

  (* Windows VirtualAlloc flags *)
  MEMRESERVE           = 2000H;
  MEMCOMMIT            = 1000H;
  PAGEEXECUTEREADWRITE = 40H;

TYPE
  Module*     = POINTER- TO ModuleDesc;
  ModuleName* = ARRAY 32 OF CHAR;

  ModuleDesc* = RECORD-
    name*:    ModuleName;  (* 00H *)
    next*:    Module;      (* 20H Set on load *)
    key*:     INTEGER;     (* 28H *)
    num*:     INTEGER;     (* 30H Module num, set on load *)
    size*:    INTEGER;     (* 38H Allocated memory, set on load (image size in file) *)
    refcnt*:  INTEGER;     (* 40H Managed by module loader *)
    vars*:    INTEGER;     (* 48H Start of global VARs / in code file start of import ref table *)
    init*:    INTEGER;     (* 50H Module initialisation entry point *)
    imprefs*: INTEGER;     (* 58H Imports references *)
    cmd*:     INTEGER;     (* 60H PO2013 Commands: address of seq of command strings and code offsets *)
    exports*: INTEGER;     (* 68H PO2013 Entries: address of array of exported offsets *)
    ptr*:     INTEGER;     (* 70H PO2013 Pointers: address of array of pointer var addresses *)
    lines*:   INTEGER;     (* 78H Line numbers etc. to address mapping *)
    varsize*: INTEGER;     (* 80H Size of module VARs *)
    reserve1: INTEGER;     (* 88H *)
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

  PreLoadVars = RECORD-
    Exeadr:     INTEGER;    (* Image PE header loaded address *)
    ImgHeader*: Module;     (* Image Oberon section loaded address *)
    LoadFlags*: SET;
    FileOfs*:   INTEGER;    (* Offset into exe file of first embedded file *)
    LoadMod*:   ModuleName; (* Module for Oberon.Mod to load at startup *)
  END;


(* Start of pre-loaded variables (preset by WinPE.mod or Link.mod) *)

VAR
  Preload*: PreLoadVars;

  (* Pre-loaded Kernel32 imports *)
  AddVectoredExceptionHandler*:    PROCEDURE-(first, filter: INTEGER): INTEGER;
  CloseHandle*:                    PROCEDURE-(hObject: INTEGER): INTEGER;
  CreateFileW*:                    PROCEDURE-(lpFileName, dwDesiredAccess, dwShareMode, lpSecurityAttributes, dwCreationDisposition, dwFlagsAndAttributes, hTemplateFile: INTEGER): INTEGER;
  DeleteFileW*:                    PROCEDURE-(lpFilename: INTEGER): INTEGER;
  ExitProcess*:                    PROCEDURE-(exitcode: INTEGER);
  FileTimeToLocalFileTime*:        PROCEDURE-(filetime, localfiletime: INTEGER): INTEGER;
  FileTimeToSystemTime*:           PROCEDURE-(filetime, systemtime: INTEGER): INTEGER;
  FindClose*:                      PROCEDURE-(hfind: INTEGER): INTEGER;
  FindFirstFileExW*:               PROCEDURE-(name, levels, data, op, filter, flags: INTEGER): INTEGER;
  FindNextFileW*:                  PROCEDURE-(hfind, data: INTEGER): INTEGER;
  FlushFileBuffers*:               PROCEDURE-(hFile: INTEGER): INTEGER;
  FormatMessageW:                  PROCEDURE-(flags, source, mid, lid, buf, size, args: INTEGER): INTEGER;
  GetCommandLineW*:                PROCEDURE-(): INTEGER;
  GetCurrentDirectoryW*:           PROCEDURE-(nsize, pbuffer: INTEGER): INTEGER;
  GetCurrentProcessId*:            PROCEDURE-(): INTEGER;
  GetEnvironmentVariableW*:        PROCEDURE-(lpName, lpBuffer, nSize: INTEGER): INTEGER;
  GetFileAttributesExW*:           PROCEDURE-(lpName, fInfoLevelId, lpFileInformation: INTEGER): INTEGER;  (* fInfoLevelId Must be 0 (GetFileExInfoStandard) *)
  GetFileAttributesW*:             PROCEDURE-(lpFileName: INTEGER): INTEGER;
  GetFileInformationByHandleEx:    PROCEDURE-(hfile, infoclass, infoptr, bufsize: INTEGER): INTEGER;
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
  Sleep*:                          PROCEDURE-(ms: INTEGER);
  UnmapViewOfFile:                 PROCEDURE-(adr: INTEGER): INTEGER;
  VirtualAlloc*:                   PROCEDURE-(address, size, type, protection: INTEGER): INTEGER;
  VirtualQuery*:                   PROCEDURE-(address, buffer, length: INTEGER): INTEGER;
  WriteFile*:                      PROCEDURE-(hFile, lpBuffer, nNumberOfBytesToWrite, lpNumberOfBytesWritten, lpOverlapped: INTEGER): INTEGER;

  (* Pre-loaded Gdi32 imports *)
  BitBlt*:                         PROCEDURE-(dc, x, y, cx, cy, sc, x1, y1, op: INTEGER): INTEGER;
  CreateBitmap*:                   PROCEDURE-(w, h, pl, de, bs: INTEGER): INTEGER;
  CreateCompatibleDC*:             PROCEDURE-(dc: INTEGER): INTEGER;
  CreateDCA*:                      PROCEDURE-(drive, device, port, devmode: INTEGER): INTEGER;
  CreateDIBSection*:               PROCEDURE-(dc, bi, us, bs, sc, of: INTEGER): INTEGER;
  CreateFontA*:                    PROCEDURE-(cHeight,  cWidth,        cEscapement,    cOrientation,
                                             cWeight,  bItalic,       bUnderline,     bStrikeOut,
                                             iCharSet, iOutPrecision, iClipPrecision, iQuality,
                                             iPitchAndFamily,         pszFaceName: INTEGER): INTEGER;
  DeleteObject*:                   PROCEDURE-(ob: INTEGER): INTEGER;
  GetGlyphOutlineW*:               PROCEDURE-(hdc, uChar, fuFormat, lpgm, cjBuffer, pvBuffer, lpmat2: INTEGER): INTEGER;
  GetCharABCWidthsW*:              PROCEDURE-(hdc, first, last, abcadr: INTEGER): INTEGER;
  GetDeviceCaps*:                  PROCEDURE-(hdc, index: INTEGER): INTEGER;
  GetOutlineTextMetricsW*:         PROCEDURE-(hdc, bufsize, buffer: INTEGER): INTEGER;
  SelectObject*:                   PROCEDURE-(dc, ob: INTEGER): INTEGER;


  (* Pre-loaded User32 imports *)
  BeginPaint*:                     PROCEDURE-(wn, ps: INTEGER): INTEGER;
  CreateIconIndirect*:             PROCEDURE-(ic: INTEGER): INTEGER;
  CreateWindowExW*:                PROCEDURE-(es, cn, wn, st, x, y, w, h, pa, me, in, lp: INTEGER): INTEGER;
  DefWindowProcW*:                 PROCEDURE-(wn, ms, wp, lp: INTEGER): INTEGER;
  DispatchMessageW*:               PROCEDURE-(ms: INTEGER): INTEGER;
  EndPaint*:                       PROCEDURE-(wn, ps: INTEGER): INTEGER;
  GetClipboardFormatNameW*:        PROCEDURE-(format, name, maxcount: INTEGER): INTEGER;
  GetDpiForWindow*:                PROCEDURE-(wn: INTEGER): INTEGER;
  GetMessageW*:                    PROCEDURE-(lm, wn, mn, mx: INTEGER): INTEGER;
  GetQueueStatus*:                 PROCEDURE-(fl: INTEGER): INTEGER;
  LoadCursorW*:                    PROCEDURE-(in, cn: INTEGER): INTEGER;
  MessageBoxA*:                    PROCEDURE-(w, t, c, u: INTEGER)(*: INTEGER*);
  MessageBoxW*:                    PROCEDURE-(w, t, c, u: INTEGER): INTEGER;
  MoveWindow*:                     PROCEDURE-(wn, x, y, w, h, repaint: INTEGER);
  MsgWaitForMultipleObjects*:      PROCEDURE-(cn, hs, wa, ms, wm: INTEGER);
  PeekMessageW*:                   PROCEDURE-(lm, wn, mn, mx, rm: INTEGER): INTEGER;
  PostMessageW*:                   PROCEDURE-(hwnd, msg, wp, lp: INTEGER): INTEGER;
  PostQuitMessage*:                PROCEDURE-(rc: INTEGER);
  RegisterClassExW*:               PROCEDURE-(wc: INTEGER): INTEGER;
  ReleaseCapture*:                 PROCEDURE-;
  SetCapture*:                     PROCEDURE-(wn: INTEGER);
  SetProcessDpiAwarenessContext*:  PROCEDURE-(cx: INTEGER): INTEGER;
  ShowCursor*:                     PROCEDURE-(sh: INTEGER);
  ShowWindow*:                     PROCEDURE-(wn, cm: INTEGER);
  TranslateMessage*:               PROCEDURE-(ms: INTEGER): INTEGER;
  InvalidateRect*:                 PROCEDURE-(wn, rc, er: INTEGER): INTEGER;

(* End of pre-loaded variables *)

  (* Linker expects handlers to follow immediately after pre-loaded vars *)
  Handlers*: ARRAY HandlerCount OF PROCEDURE;

  Stdin:  INTEGER;
  Stdout: INTEGER;
  crlf*:  ARRAY 3 OF CHAR;
  Log:    PROCEDURE(s: ARRAY OF BYTE);
  Sol:    BOOLEAN;   (* True only at start of line *)
  HWnd:   INTEGER;   (* Set if a window has been created *)

  ModuleSpace*: INTEGER;   (* Start of module space *)
  AllocPtr*:    INTEGER;   (* Start of remaining free module space *)
  CommitLen*:   INTEGER;   (* Committed module space memory *)
  Root*:        Module;    (* List of loaded and free'd modules *)

  ExceptionDepth: INTEGER;

  (* Partially parsed command line *)
  CommandLine*: ARRAY 1024 OF CHAR;
  CmdModule*:   ARRAY 32 OF CHAR;
  CmdCommand*:  ARRAY 32 OF CHAR;
  ArgStart*:    INTEGER;


PROCEDURE NoLog(s: ARRAY OF BYTE); BEGIN END NoLog;

PROCEDURE Min*(a, b: INTEGER): INTEGER;
BEGIN IF a < b THEN b := a END;  RETURN b END Min;

PROCEDURE Max*(a, b: INTEGER): INTEGER;
BEGIN IF a > b THEN b := a END;  RETURN b END Max;


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

PROCEDURE ZeroFill*(VAR buf: ARRAY OF BYTE);  VAR i: INTEGER;
BEGIN FOR i := 0 TO LEN(buf)-1 DO buf[i] := 0 END END ZeroFill;


(* -------------------------------------------------------------------------- *)
(* ---------------- Simple logging/debugging console output ----------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE wc*(c: CHAR); BEGIN Log(c) END wc;

PROCEDURE wn*; BEGIN Log(crlf) END wn;

PROCEDURE wcn*; BEGIN IF ~Sol THEN wn END END wcn;  (* Conditional newline *)

PROCEDURE ws*(s: ARRAY OF CHAR); BEGIN Log(s) END ws;

PROCEDURE wsr*(s: ARRAY OF CHAR; w: INTEGER);  (* Right justified with leading spaces *)
BEGIN DEC(w, Length(s));  WHILE w > 0 DO wc(" "); DEC(w) END; Log(s) END wsr;

PROCEDURE wsz*(s: ARRAY OF CHAR; w: INTEGER);  (* Right justified with leading zeroes *)
BEGIN DEC(w, Length(s));  WHILE w > 0 DO wc("0"); DEC(w) END; Log(s) END wsz;

PROCEDURE wsn*(s: ARRAY OF CHAR); BEGIN Log(s); Log(crlf) END wsn;


PROCEDURE wh*(n: INTEGER);
VAR hex: ARRAY 32 OF CHAR;
BEGIN IntToHex(n, hex); Log(hex) END wh;

PROCEDURE whr*(n, w: INTEGER);  (* Right justified with leading spaces *)
VAR hex: ARRAY 32 OF CHAR;
BEGIN IntToHex(n, hex); wsr(hex, w) END whr;

PROCEDURE whz*(n, w: INTEGER);  (* Right justified with leading zeroes *)
VAR hex: ARRAY 32 OF CHAR;
BEGIN IntToHex(n, hex); wsz(hex, w) END whz;


PROCEDURE wi*(n: INTEGER);
VAR dec: ARRAY 32 OF CHAR;
BEGIN IntToDecimal(n, dec); Log(dec) END wi;

PROCEDURE wir*(n, w: INTEGER);  (* Right justified with leading spaces *)
VAR dec: ARRAY 32 OF CHAR;
BEGIN IntToDecimal(n, dec); wsr(dec, w) END wir;

PROCEDURE wiz*(n, w: INTEGER);  (* Right justified with leading zeroes *)
VAR dec: ARRAY 32 OF CHAR;
BEGIN IntToDecimal(n, dec); wsz(dec, w) END wiz;


PROCEDURE wb*(n: INTEGER);
BEGIN WHILE n > 0 DO wc(" "); DEC(n) END END wb;


(* -------------------------------------------------------------------------- *)
(* ----------------------------- Time functions ----------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Time*(): INTEGER;  (* In 100 nanosecond ticks since 1601 UTC *)
VAR tick: INTEGER;
BEGIN GetSystemTimePreciseAsFileTime(SYSTEM.ADR(tick));
RETURN tick END Time;

PROCEDURE TimeAsClock*(filetime: INTEGER): INTEGER;
(* Returns local time in PO2013 format:               *)
(* 23/0,15/year-2000,4/month,5/day,5/hour,6/min,6/sec *)
VAR
  local, res: INTEGER;
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
  res := FileTimeToLocalFileTime(SYSTEM.ADR(filetime), SYSTEM.ADR(local));
  clock := 0;
  IF FileTimeToSystemTime(SYSTEM.ADR(local), SYSTEM.ADR(st)) # 0 THEN
    DEC(st.year, 2000);
    clock := ((((  st.year    * 10H
                 + st.month ) * 20H
                 + st.day   ) * 20H
                 + st.hour  ) * 40H
                 + st.minute) * 40H
                 + st.second;
  END
RETURN clock END TimeAsClock;

PROCEDURE TimeAsExtClock*(filetime: INTEGER): INTEGER;
(* Returns local time as:                                             *)
(* 13/0,15/year,4/month,5/day,5/hour,6/minute,6/second,10/millisecond *)
VAR
  local, res: INTEGER;
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
  res := FileTimeToLocalFileTime(SYSTEM.ADR(filetime), SYSTEM.ADR(local));
  clock := 0;
  IF FileTimeToSystemTime(SYSTEM.ADR(local), SYSTEM.ADR(st)) # 0 THEN
    clock := (((((  st.year    * 10H
                  + st.month ) * 20H
                  + st.day   ) * 20H
                  + st.hour  ) * 40H
                  + st.minute) * 40H
                  + st.second) * 400H
                  + st.milliseconds;
  END
RETURN clock END TimeAsExtClock;

PROCEDURE Clock*(): INTEGER;
(* Returns local time as                                              *)
(* 30/0,8/year-2000,4/month,5/day,5/hour,6/min,6/sec                  *)
BEGIN RETURN TimeAsClock(Time()) END Clock;

PROCEDURE ExtClock*(): INTEGER;
(* Returns local time as                                              *)
(* 13/0,15/year,4/month,5/day,5/hour,6/minute,6/second,10/millisecond *)
BEGIN RETURN TimeAsExtClock(Time()) END ExtClock;


(*
PROCEDURE WriteClock*;
VAR clock: INTEGER;
BEGIN
  clock := LongClock();
  wiz(clock DIV 1000000000H MOD 8000H, 4);  (*year*)     wc("-");
  wiz(clock DIV 100000000H MOD 10H,    2);  (*month*)    wc("-");
  wiz(clock DIV 8000000H MOD 20H,      2);  (*day*)      wc(" ");
  wiz(clock DIV 400000H MOD 20H,       2);  (*hour*)     wc(":");
  wiz(clock DIV 10000H MOD 40H,        2);  (*min*)      wc(":");
  wiz(clock DIV 400H MOD 40H,          2);  (*sec*)
END WriteClock;
*)

PROCEDURE WriteTime*(time: INTEGER);
VAR clock: INTEGER;
BEGIN
  clock := TimeAsClock(time);
  wiz(clock DIV 1000000000H MOD 8000H, 4);  (*year*)     wc("-");
  wiz(clock DIV 100000000H MOD 10H,    2);  (*month*)    wc("-");
  wiz(clock DIV 8000000H MOD 20H,      2);  (*day*)      wc(" ");
  wiz(clock DIV 400000H MOD 20H,       2);  (*hour*)     wc(":");
  wiz(clock DIV 10000H MOD 40H,        2);  (*min*)      wc(":");
  wiz(clock DIV 400H MOD 40H,          2);  (*sec*)      wc(".");
  wiz(clock MOD 400H,                  3)
END WriteTime;


(* -------------------------------------------------------------------------- *)
(* --------------------------- Trivial Stdin read --------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE GetChar*(): CHAR;
VAR ch: CHAR;  res, bytesread: INTEGER;
BEGIN
  ch := 0X;
  res := ReadFile(Stdin, SYSTEM.ADR(ch), 1, bytesread, 0);
RETURN ch END GetChar;


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
    wb(indent); whz(rowstart, 12); ws("  ");
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
      IF bytes[i] >= 0 THEN whz(bytes[i], 2);  wc(" ") ELSE ws("   ") END;
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
    wn;  INC(rowstart, 16);
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
(* Any global variables set to code addresses must be reset after WinHost is  *)
(*   moved to Oberon memory.                                                  *)
(* -------------------------------------------------------------------------- *)

PROCEDURE PrepareOberonMachine;
VAR
  reserveadr:   INTEGER;
  size:         INTEGER;        (* loaded length including global vars *)
  hdr:          Module;
BEGIN
  (* Reserve 2GB memory for the Oberon module space (code, tables and global vars) *)
  reserveadr := VirtualAlloc(100000000H, 100000000H, MEMRESERVE, PAGEEXECUTEREADWRITE);
  IF reserveadr = 0 THEN
    wsn("** Could not reserve Oberon machine memory **");  ExitProcess(9);
  ELSIF Verbose IN Preload.LoadFlags THEN
    ws("* Reserved 2GB Oberon machine memory at ");  wh(reserveadr);  wsn("H.")
  END;

  (* Determine loaded size of all modules *)
  hdr  := Preload.ImgHeader;
  size := 0;
  WHILE hdr.size # 0 DO
    INC(size, (hdr.vars + hdr.varsize + 15) DIV 16 * 16);
    hdr := SYSTEM.VAL(Module, ORD(hdr) + hdr.size)
  END;

  (* Commit enough for the modules being loaded plus a sentinel ModuleDesc. *)
  CommitLen := (size + SYSTEM.SIZE(ModuleDesc) + 4095) DIV 4096 * 4096;
  ModuleSpace := VirtualAlloc(reserveadr, CommitLen, MEMCOMMIT, PAGEEXECUTEREADWRITE);
  IF Verbose IN Preload.LoadFlags THEN
    ws("* Committed ");  wh(CommitLen);  ws("H bytes at ");  wh(ModuleSpace);  wsn("H.")
  END
END PrepareOberonMachine;


(* -------------------- Pulling variables out of memory --------------------- *)

PROCEDURE GetString(VAR adr: INTEGER; VAR str: ARRAY OF CHAR);
VAR i: INTEGER;
BEGIN
  i := -1;
  REPEAT INC(i); SYSTEM.GET(adr, str[i]); INC(adr) UNTIL str[i] = 0X
END GetString;

PROCEDURE GetUnsigned(VAR adr, n: INTEGER);
VAR i: BYTE; s: INTEGER;
BEGIN
  n := 0; s := 0;
  REPEAT
    SYSTEM.GET(adr, i);  INC(adr);
    INC(n, LSL(i MOD 128, s));  INC(s, 7)
  UNTIL i < 128
END GetUnsigned;


(* ----------------------- Windows exception handling ----------------------- *)

PROCEDURE LocateModule(adr: INTEGER): Module;
VAR  (*modadr: INTEGER;*)  hdr: Module;
BEGIN
  hdr := Root;
  WHILE (hdr # NIL) & ((adr < ORD(hdr)) OR (adr >= (*ORD(hdr)+*)hdr.vars)) DO
    hdr := hdr.next
  END;
RETURN hdr END LocateModule;

PROCEDURE LocateLine(hdr: Module; offset: INTEGER);
VAR
  adr, line, pc, i: INTEGER;
  name: ARRAY 32 OF CHAR;
BEGIN
  IF hdr.lines # 0 THEN
    adr := ORD(hdr) + hdr.lines;
    GetString(adr, name);
    WHILE name[0] # 0X DO
      SYSTEM.GET(adr, line);  INC(adr, 8);
      SYSTEM.GET(adr, pc);    INC(adr, 8);
      GetUnsigned(adr, i);
      WHILE (i # 0) & (offset > pc + i) DO
        INC(pc, i);  GetUnsigned(adr, i);  INC(line, i);
        GetUnsigned(adr, i)
      END;
      IF (offset > pc) & (offset <= pc + i) THEN
        ws("** on line "); wi(line); ws(" in "); ws(name); wsn(" **");
        name[0] := 0X
      ELSE
        GetString(adr, name)
      END;
    END
  END
END LocateLine;

PROCEDURE LocateAddress(adr: INTEGER; p: ExceptionPointers);  (* Writes location info about address, if any *)
VAR hdr: Module;  offset: INTEGER;
BEGIN
  ws(" at address ");  wh(adr); wc("H");
  hdr := LocateModule(adr);
  IF hdr # NIL  THEN
    offset := adr - ORD(hdr);
    ws(" in module "); ws(hdr.name);
    ws(" at offset "); wh(offset); wc("H")
  END;
  wsn(". **");

  IF hdr # NIL THEN LocateLine(hdr, offset) END;

  IF p # NIL THEN
    ws("  rax "); whz(p.context.rax, 16);  ws("  rbx "); whz(p.context.rbx, 16);
    ws("  rcx "); whz(p.context.rcx, 16);  ws("  rdx "); whz(p.context.rdx, 16);  wn;
    ws("  rsp "); whz(p.context.rsp, 16);  ws("  rbp "); whz(p.context.rbp, 16);
    ws("  rsi "); whz(p.context.rsi, 16);  ws("  rdi "); whz(p.context.rdi, 16);  wn;
    ws("  r8  "); whz(p.context.r8,  16);  ws("  r9  "); whz(p.context.r9,  16);
    ws("  r10 "); whz(p.context.r10, 16);  ws("  r11 "); whz(p.context.r11, 16);  wn;
    ws("  r12 "); whz(p.context.r12, 16);  ws("  r13 "); whz(p.context.r13, 16);
    ws("  r14 "); whz(p.context.r14, 16);  ws("  r15 "); whz(p.context.r15, 16);  wn;
    (* Dump top of stack (i.e. lowest addresses) *)
    DumpMem(2, p.context.rsp, p.context.rsp, 128)
  END
END LocateAddress;

PROCEDURE- ExceptionHandler(p: ExceptionPointers);  (* Called by Windows *)
VAR modadr, excpadr, excpcode: INTEGER;
BEGIN
  INC(ExceptionDepth);
  excpcode := p.exception.ExceptionCode;
  excpadr  := p.exception.ExceptionAddress;
  wn;

  IF    excpcode = 080000003H THEN ws("** Breakpoint (INT 3)");
  ELSIF excpcode = 080000004H THEN ws("** Single step (0F1H instr)");
  ELSIF excpcode = 0C0000005H THEN ws("** Access violation");
  ELSIF excpcode = 0C0000006H THEN ws("** In-page error");
  ELSIF excpcode = 0C000001DH THEN ws("** Illegal instruction");
  ELSIF excpcode = 0C000008EH THEN ws("** Divide by zero");
  ELSIF excpcode = 0C0000094H THEN ws("** Integer divide by zero");
  ELSIF excpcode = 0C0000096H THEN ws("** Privileged instruction");
  ELSIF excpcode = 0C00000FDH THEN ws("** Stack Overflow");
  ELSE ws("** Exception ");  wh(excpcode);  wc("H")
  END;

  IF ExceptionDepth < 2 THEN
    LocateAddress(excpadr, p)
  ELSE
    wsn(": nested exception **")
  END;
  ExitProcess(99)
END ExceptionHandler;


(* ----------------------------- Trap handlers ------------------------------ *)

PROCEDURE Trap*(retoffset: INTEGER; desc: ARRAY OF CHAR);
VAR adr, modadr: INTEGER;
BEGIN  (* retoffset is callers local var size *)
  ws(desc);
  SYSTEM.GET(SYSTEM.ADR(LEN(desc)) + 16 + retoffset, adr);
  LocateAddress(adr, NIL);
  ExitProcess(99)
END Trap;

PROCEDURE NewPointerHandler();
BEGIN wcn; Trap(0, "** New pointer handler not istalled") END NewPointerHandler;

PROCEDURE AssertionFailureHandler();
BEGIN wcn; Trap(0, "** Assertion failure")      END AssertionFailureHandler;

PROCEDURE ArraySizeMismatchHandler();
BEGIN wcn; Trap(0, "** Array size mismatch")     END ArraySizeMismatchHandler;

PROCEDURE UnterminatedStringHandler();
BEGIN wcn; Trap(0, "** Unterminated string")     END UnterminatedStringHandler;

PROCEDURE IndexOutOfRangeHandler();
BEGIN wcn; Trap(0, "** Index out of range")      END IndexOutOfRangeHandler;

PROCEDURE NilPointerDereferenceHandler();
BEGIN wcn; Trap(0, "** NIL pointer dereference") END NilPointerDereferenceHandler;

PROCEDURE TypeGuardFailureHandler();
BEGIN wcn; Trap(0, "** Type guard failure")      END TypeGuardFailureHandler;

PROCEDURE InitSysHandlers;
BEGIN
  Handlers[NewProc]                   := NewPointerHandler;
  Handlers[AssertionFailureProc]      := AssertionFailureHandler;
  Handlers[ArraySizeMismatchProc]     := ArraySizeMismatchHandler;
  Handlers[UnterminatedStringProc]    := UnterminatedStringHandler;
  Handlers[IndexOutOfRangeProc]       := IndexOutOfRangeHandler;
  Handlers[NilPointerDereferenceProc] := NilPointerDereferenceHandler;
  Handlers[TypeGuardFailureProc]      := TypeGuardFailureHandler
END InitSysHandlers;



(* ------------------ WinHost internal assertion handlers ------------------- *)

PROCEDURE assertmsg(expectation: BOOLEAN; msg: ARRAY OF CHAR);
VAR res: INTEGER;
BEGIN
  IF ~expectation THEN
    ws(" * "); ws(msg); wsn(" *");
    res := CloseHandle(Stdout);
    ExitProcess(99)
  END
END assertmsg;

PROCEDURE assert(expectation: BOOLEAN);
BEGIN IF ~expectation THEN Trap(0, "** WinHost assertion failure **") END
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

PROCEDURE WriteWindowsErrorMessage*(err: INTEGER);
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
    wn; ws("** "); WriteWindowsErrorMessage(err); Trap(0, " **")
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
  disposition := 3;        (* Open existing *)
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
  IF (res < 0) & (openkind # OpenRO) THEN  (* Try again readonly *)
    access := 080000000H;  (* GenericRead *)
    mode   := 1;           (* FileShareRead *)
    res    := CreateFileW(SYSTEM.ADR(name16), access, mode, 0, disposition, flags, 0);
  END;
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


PROCEDURE FileTime*(hfile: INTEGER): INTEGER;
TYPE infodesc = RECORD-
    creation: INTEGER;
    access:   INTEGER;
    write:    INTEGER;
    change:   INTEGER;
    attribs:  SYSTEM.CARD32
  END;
VAR
  res:  INTEGER;
  info: infodesc;
BEGIN
  res := GetFileInformationByHandleEx(hfile, 0, SYSTEM.ADR(info), SYSTEM.SIZE(infodesc));
  (*
  wsn("* GetFileInformationByHandleEx ->");
  ws("  creation "); WriteTime(info.creation); wsn(".");
  ws("  access   "); WriteTime(info.access);   wsn(".");
  ws("  write    "); WriteTime(info.write);    wsn(".");
  ws("  change   "); WriteTime(info.change);   wsn(".");
  ws("  attribs  "); whz(info.attribs, 4);     wsn("H.");
  *)
  IF res = 0 THEN AssertWinErr(GetLastError()) END;
RETURN info.write END FileTime;


(* -------------------------------------------------------------------------- *)

PROCEDURE WriteStdout(s: ARRAY OF BYTE);
VAR len, written, result: INTEGER;
BEGIN
  len := Length(s);
  IF len > 0 THEN Sol := s[len-1] = 0AH END;
  result := WriteFile(Stdout, SYSTEM.ADR(s), len, SYSTEM.ADR(written), 0);
END WriteStdout;


(* -------------------------------------------------------------------------- *)

PROCEDURE FindModule(name: ARRAY OF CHAR; key: INTEGER): Module;
VAR
  hdr:     Module;
BEGIN
  hdr := Root;
  WHILE (hdr # NIL) & (hdr.name # name) DO hdr := hdr.next END;
  assert(hdr.size # 0); (*, "FindModule hdr.size is 0.");*)
RETURN hdr END FindModule;


PROCEDURE ExportedAddress(modhdr: Module; index: INTEGER): INTEGER;
VAR exportoffset: SYSTEM.CARD32;
BEGIN
  SYSTEM.GET(ORD(modhdr) + modhdr.exports + index * 4, exportoffset);
RETURN ORD(modhdr) + exportoffset END ExportedAddress;


PROCEDURE LinkImport*(modadr, offset, impno, modno: INTEGER; modules: ARRAY OF Module);
VAR disp, absreloc: INTEGER;
BEGIN
  IF modno = 0 THEN  (* system function *)
    SYSTEM.GET(modadr + offset, disp);
    ASSERT((impno >= 0) & (impno <= HandlerCount));
    disp := SYSTEM.ADR(Handlers) + 8 * impno + disp - modadr;
    SYSTEM.PUT(modadr + offset, disp)
  ELSIF modno = 0FFFFH THEN (* 64 bit absolute address relocation *)
    (* qword at offset contains 32/0,32/module offset or 32/1,16/mod,16/imp *)
    SYSTEM.GET(modadr + offset, absreloc);
    IF absreloc DIV 100000000H = 0 THEN  (* offset in this module *)
      SYSTEM.PUT(modadr + offset, modadr + absreloc)
    ELSE  (* import reference from another module *)
      modno := absreloc DIV 10000H MOD 10000H;  assert(modno > 0);
      impno := absreloc MOD 10000H;
      assert(impno > 0);
      SYSTEM.PUT(modadr + offset, ExportedAddress(modules[modno-1], impno-1))
    END
  ELSE
    assert(modno > 0);
    SYSTEM.GET(modadr + offset, disp);
    INC(disp, ExportedAddress(modules[modno-1], impno-1) - modadr);
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


PROCEDURE WriteModuleHeader*(mod: Module);
BEGIN
  ws("  Module "); ws(mod.name); ws(" at "); wh(ORD(mod)); wsn("H:");
  ws("    key:     "); whr(mod.key,      16);  wsn("H  28H.");
  ws("    num:     "); whr(mod.num,      16);  wsn("H  30H Module num, set on load.");
  ws("    size:    "); whr(mod.size,     16);  wsn("H  38H Allocated memory, set on load (image size in file).");
  ws("    refcnt:  "); whr(mod.refcnt,   16);  wsn("H  40H Managed by module loader.");
  ws("    vars:    "); whr(mod.vars,     16);  wsn("H  48H Start of global VARs / in code file start of import ref table.");
  ws("    init:    "); whr(mod.init,     16);  wsn("H  50H Module initialisation entry point.");
  ws("    imprefs: "); whr(mod.imprefs,  16);  wsn("H  58H Imports references.");
  ws("    cmd:     "); whr(mod.cmd,      16);  wsn("H  60H PO2013 Commands: address of seq of command strings and code offsets.");
  ws("    exports: "); whr(mod.exports,  16);  wsn("H  68H PO2013 Entries: address of array of exported offsets.");
  ws("    ptr:     "); whr(mod.ptr,      16);  wsn("H  70H PO2013 Pointers: address of array of pointer var addresses.");
  ws("    lines:   "); whr(mod.lines,    16);  wsn("H  78H Line numbers etc. to address mapping.");
  ws("    varsize: "); whr(mod.varsize,  16);  wsn("H  80H Size of module VARs.");
END WriteModuleHeader;


PROCEDURE WriteModuleHeaders*;
VAR mod: Module;
BEGIN
  wsn("Module headers:");
  mod := Root;
  WHILE mod # NIL DO
    IF mod.name[0] = 0X THEN
      ws("Free block at ");  wh(ORD(mod)); ws("H size "); wh(mod.size); wsn("H:");
    ELSE
      WriteModuleHeader(mod)
    END;
    mod := mod.next
  END
END WriteModuleHeaders;


PROCEDURE LoadModule(imagehdr: Module);
(* Load module whose code image is at modadr *)
(* Module is loaded at AllocPtr which is then updated *)
(* Returns address of body code *)
VAR
  adr:         INTEGER;
  loadedhdr:   Module;
  impmod:      ARRAY 32 OF CHAR;
  modules:     ARRAY 64 OF Module; (* Import from up to 64 modules *)
  i:           INTEGER;
  key:         INTEGER;
  importcount: SYSTEM.CARD32;
  offset:      SYSTEM.CARD32;
  impno:       SYSTEM.CARD16;
  modno:       SYSTEM.CARD16;
  modulebody:  PROCEDURE;
BEGIN
  SYSTEM.COPY(ORD(imagehdr), AllocPtr, imagehdr.vars);  (* Copy up to but excluding import table *)
  loadedhdr := SYSTEM.VAL(Module, AllocPtr);

  (* Update length in header from object binary size to loaded size *)
  loadedhdr.size   := (loadedhdr.vars + loadedhdr.varsize + 15) DIV 16 * 16;

  IF Verbose IN Preload.LoadFlags THEN
    ws("* Loaded ");              ws(loadedhdr.name);
    ws(" at ");                   wh(AllocPtr);
    ws("H, code ");               wh(loadedhdr.vars);
    ws("H bytes, data ");         wh(loadedhdr.varsize);
    ws("H bytes, loaded size ");  wh(loadedhdr.size);
    ws("H bytes, limit ");        wh(AllocPtr + loadedhdr.size);
    ws("H, lines at ");           wh(loadedhdr.lines);
    wsn("H.")
  END;

  (* Build list of imported module header addresses *)
  i := 0;
  adr := ORD(imagehdr) + loadedhdr.vars;
  GetString(adr, impmod);
  WHILE impmod[0] # 0X DO
    SYSTEM.GET(adr, key);  INC(adr, 8);
    modules[i] := FindModule(impmod, key);
    INC(i);
    GetString(adr, impmod)
  END;
  modules[i] := NIL;

  (* Link imports from other modules *)
  adr := (adr + 15) DIV 16 * 16;
  SYSTEM.GET(adr, importcount);  INC(adr, 4);
  i := 0;
  WHILE i < importcount DO
    SYSTEM.GET(adr, offset); INC(adr, 4);
    SYSTEM.GET(adr, impno);  INC(adr, 2);
    SYSTEM.GET(adr, modno);  INC(adr, 2);
    LinkImport(AllocPtr, offset, impno, modno, modules);
    INC(i)
  END;

  (* Relocate pointer addresses *)
  assert(loadedhdr.ptr # 0);  assert(ORD(loadedhdr) = AllocPtr);
  INC(loadedhdr.vars, AllocPtr);
  INC(loadedhdr.ptr, AllocPtr);
  RelocatePointerAddresses(loadedhdr.ptr, loadedhdr.vars);

  INC(loadedhdr.cmd, AllocPtr);

  (* Link module into loaded module list *)
  loadedhdr.next := Root;
  Root := loadedhdr;

  (* Execute module body *)
  SYSTEM.PUT(SYSTEM.ADR(modulebody), AllocPtr + loadedhdr.init);
  INC(AllocPtr, loadedhdr.size);

  modulebody
END LoadModule;


PROCEDURE SetRoot*(r: Module); BEGIN
  (*
  ws("* Change Root from "); wh(ORD(Root));
  ws("H (");                 ws(Root.name);
  ws(") to ");               wh(ORD(r)); wsn("H.");
  *)
  Root := r
END SetRoot;


PROCEDURE Allocate*(size: INTEGER; VAR p, alloc: INTEGER);
VAR adr, newcommitlen: INTEGER;
BEGIN
  (*
  ws("WinHost.Allocate(size "); wh(size);        wsn("H).");
  ws("  ModuleSpace: ");        wh(ModuleSpace); wsn("H.");
  ws("  AllocPtr:    ");        wh(AllocPtr);    wsn("H.");
  ws("  CommitLen:   ");        wh(CommitLen);   wsn("H.");
  *)
  p     := 0;
  alloc := 0;
  size  := (size + 15) DIV 16 * 16;
  IF AllocPtr - ModuleSpace + size < 80000000H THEN  (* Hard limit on reserved size 2GB due to relative addressing *)
    IF AllocPtr + size > ModuleSpace + CommitLen THEN

      (*ws("* CommitLen increasing from "); wh(CommitLen);*)

      (* Round up to a multiple of 256K *)
      newcommitlen := (AllocPtr + size + 40000H - 1) DIV 40000H * 40000H - ModuleSpace;

      (*
      ws("H to "); wh(newcommitlen); wsn("H *");

      ws("Calling VirtualAlloc(addr "); wh(ModuleSpace+CommitLen);
      ws("H, size "); wh(newcommitlen-CommitLen);
      wsn("H, MEMCOMMIT, PAGEEXECUTEREADWRITE).");
      *)

      adr := VirtualAlloc(ModuleSpace+CommitLen, newcommitlen-CommitLen, MEMCOMMIT, PAGEEXECUTEREADWRITE);
      IF adr = 0 THEN
        ws(": "); WriteWindowsErrorMessage(GetLastError()); wsn(".");
        ExitProcess(0);
      END;
      ASSERT(adr = ModuleSpace + CommitLen);
      CommitLen := newcommitlen
    END;
    IF AllocPtr + size <= ModuleSpace + CommitLen THEN
      p     := AllocPtr;
      alloc := size;
      INC(AllocPtr, size);
      (* ws("  AllocPtr ->: ");        wh(AllocPtr);    wsn("H.");*)
    END
  END
END Allocate;


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

PROCEDURE LoadRemainingModules(imagehdr: Module);
VAR
  res:       INTEGER;
  loadedhdr: Module;
BEGIN
  (* Load and link remaining code modules from EXE file image *)
  IF Verbose IN Preload.LoadFlags THEN
    ws("* Load remaining modules starting from "); wh(ORD(Preload.ImgHeader)); wsn("H.");
  END;
  WHILE imagehdr.size # 0 DO
    LoadModule(imagehdr);
    imagehdr := SYSTEM.VAL(Module, ORD(imagehdr) + imagehdr.size);
  END
END LoadRemainingModules;


(* -------------------------------------------------------------------------- *)

PROCEDURE SetHWnd*(h: INTEGER);
BEGIN HWnd := h END SetHWnd;


(* -------------------------------------------------------------------------- *)

PROCEDURE GetRSP*(): INTEGER;
VAR result: INTEGER;
BEGIN result := SYSTEM.ADR(result) + 8
RETURN result END GetRSP;

PROCEDURE PressKeyToContinue;
VAR ch: CHAR;
BEGIN wcn; ws("Press any key to continue "); ch := GetChar(); wn
END PressKeyToContinue;

PROCEDURE QueryMem(adr: INTEGER): INTEGER;
TYPE
  mbidesc = RECORD-
    BaseAddress:       INTEGER;
    AllocationBase:    INTEGER;
    AllocationProtect: SYSTEM.CARD32;
    PartitionId:       SYSTEM.CARD16;
    RegionSize:        INTEGER;
    State:             SYSTEM.CARD32;
    Protect:           SYSTEM.CARD32;
    Type:              SYSTEM.CARD32
  END;
VAR mbi: mbidesc;  res, size: INTEGER;
BEGIN
  size := 0;
  res := VirtualQuery(adr, SYSTEM.ADR(mbi), SYSTEM.SIZE(mbidesc));
  IF res >= SYSTEM.SIZE(mbidesc) THEN
    ws("VirtualQuery adr "); wh(adr);
    ws("H, result "); wh(res); wsn("H.");
    ws("  BaseAddress:       "); wh(mbi.BaseAddress);       wsn("H.");
    ws("  AllocationBase:    "); wh(mbi.AllocationBase);    wsn("H.");
    ws("  AllocationProtect: "); wh(mbi.AllocationProtect); wsn("H.");
    ws("  PartitionId:       "); wh(mbi.PartitionId);       wsn("H.");
    ws("  RegionSize:        "); wh(mbi.RegionSize);        wsn("H.");
    ws("  State:             "); wh(mbi.State);             wsn("H.");
    ws("  Protect:           "); wh(mbi.Protect);           wsn("H.");
    ws("  Type:              "); wh(mbi.Type);              wsn("H.");
    DumpMem(0, SYSTEM.ADR(mbi), 0, res);
    IF adr < ORD(Preload.ImgHeader) + 180000000H THEN
      size := mbi.RegionSize
    END
  END
RETURN size END QueryMem;


(* -------------------------- Command line parsing -------------------------- *)

PROCEDURE Getch32*(VAR adr, ch32: INTEGER);  (* Pull 32 bit Unicode from command line *)
VAR ch16: SYSTEM.CARD16;
BEGIN
  SYSTEM.GET(adr, ch16);  INC(adr, 2);
  IF ch16 DIV 400H # 36H THEN
    ch32 := ch16 (* Not a high surrogate *)
  ELSE
    ch32 := LSL(ch16 MOD 400H, 10) + 10000H;
    SYSTEM.GET(adr, ch16);
    IF ch16 DIV 400H = 37H THEN
      INC(adr, 2);  (* Add low surrogate part *)
      INC(ch32, ch16 MOD 400H)
    END
  END
END Getch32;

PROCEDURE ParseCommandLine;
VAR
  i, j, k:  INTEGER;
  adr:      INTEGER;
  ch:       INTEGER;
  quoted:   BOOLEAN;
  modstart: INTEGER;
  modlimit: INTEGER;
BEGIN
  (* Parse command line *)
  adr           := GetCommandLineW();
  quoted        := FALSE;
  i             := 0;
  modstart      := -1;
  modlimit      := -1;
  CmdModule[0]  := 0X;
  CmdCommand[0] := 0X;

  (* Parse program name, if any. (CreateProcess can miss program name.) *)
  Getch32(adr, ch);
  IF ch # 32 THEN
    modstart := 0;
    WHILE (i < LEN(CommandLine) - 3) & (ch # 0) & ((ch # 32) OR quoted) DO  (* Skip to first unquoted space *)
      IF    ch = ORD(".")                 THEN modlimit := i
      ELSIF (ch = ORD("/")) OR (ch = 5CH) THEN modlimit := -1;  modstart := i + 1
      END;
      PutUtf8(ch, CommandLine, i);
      IF ch = 22H THEN quoted  := ~quoted END;
      Getch32(adr, ch)
    END;

    IF modlimit < 0 THEN modlimit := i END;
    CommandLine[i] := 0X;
    (*
    ws("Extract module and command names from '"); ws(CommandLine); wsn("'");
    ws("  modstart: "); wi(modstart); wsn(".");
    ws("  modlimit: "); wi(modlimit); wsn(".");
    *)

    (* Extract module and command names from program name, if any *)

    j := modstart;
    k := 0;
    WHILE j < modlimit DO
      CmdModule[k] := CommandLine[j];
      INC(k);  INC(j)
    END;
    CmdModule[k] := 0X;
    IF (k > 0) & (CommandLine[j] = ".") THEN
      INC(j); k := 0;
      WHILE j < i DO CmdCommand[k] := CommandLine[j];  INC(j);  INC(k) END;
      CmdCommand[k] := 0X
    END
  END;

  (* Skip to arguments *)
  WHILE (i < LEN(CommandLine) - 3) & (ch = 32) DO
    CommandLine[i] := " ";  INC(i);
    Getch32(adr, ch)
  END;
  ArgStart := i;  (* Note: may be end of command line *)

  (* Copy remainder of command line *)
  WHILE (i < LEN(CommandLine) - 3) & (ch # 0) DO PutUtf8(ch, CommandLine, i);  Getch32(adr, ch) END;
  CommandLine[i] := 0X;

  (*
  wsn("Winhost command line parsing:");
  ws("  CommandLine: '"); ws(CommandLine); wsn("'.");
  ws("  CmdModule:   '"); ws(CmdModule);   wsn("'.");
  ws("  CmdCommand:  '"); ws(CmdCommand);  wsn("'.");
  ws("  ArgStart:    ");  wi(ArgStart);    wsn(".");
  *)
END ParseCommandLine;

PROCEDURE NewStartup;
VAR
  res:        INTEGER;
  loadedlen:  INTEGER;
  reserveadr: INTEGER;
  reservelen: INTEGER;
  lastbyte:   INTEGER;
  adr:        INTEGER;
  size:       INTEGER;
  mod:        Module;
BEGIN
  ModuleSpace := ORD(Preload.ImgHeader);
  Root        := Preload.ImgHeader;
  InitSysHandlers;

  (*
  ws("* Exeadr:       "); wh(Preload.Exeadr);         wsn("H *");
  ws("* ImgHeader at: "); wh(ORD(Preload.ImgHeader)); wsn("H *");
  ws("* LoadFlags:    "); wh(ORD(Preload.LoadFlags)); wsn("H *");
  *)

  (* Show loaded modules *)
  (*wsn("* Modules: *");*)
  mod := Preload.ImgHeader;
  WHILE mod # NIL DO
    (*
    ws("  "); ws(mod.name);
    ws(" at "); wh(ORD(mod));
    ws("H size "); wh(mod.size);
    wsn("H.");
    *)
    lastbyte := ORD(mod) + mod.size - 1;
    mod := mod.next;
  END;

  (*WriteModuleHeaders;*)

  AllocPtr    := lastbyte + 1;
  CommitLen   := (AllocPtr + 0FFFFH) DIV 10000H * 10000H - ModuleSpace;
  (*
  wsn("Derived values: ");
  ws("  ModuleSpace: "); wh(ModuleSpace); wsn("H.");
  ws("  AllocPtr:    "); wh(AllocPtr);    wsn("H.");
  ws("  CommitLen:   "); wh(CommitLen);   wsn("H.");

  ws("* lastbyte "); wh(lastbyte);
  ws("H => loaded length "); wh(lastbyte + 1 - ORD(Preload.ImgHeader));
  *)
  loadedlen := (lastbyte + 1 - ORD(Preload.ImgHeader) + 65535) DIV 65536 * 65536;
  (*ws("H, rounded up to allocation granularity "); wh(loadedlen); wsn("H.");*)

  (* Extend memory to 2GB reserve *)
  reserveadr := ORD(Preload.ImgHeader) + loadedlen;
  reservelen := 80000000H - loadedlen;
  (*
  ws("Reserving memory from "); wh(reserveadr);
  ws("H up to "); wh(reserveadr + reservelen); wsn("H.");
  *)
  res := VirtualAlloc(reserveadr, reservelen, MEMRESERVE, PAGEEXECUTEREADWRITE);
  (*ws("* VirtualAlloc result "); wh(res); wsn("H *");*)
  IF res = 0 THEN ws(": "); WriteWindowsErrorMessage(GetLastError()); wsn(".")  END;

  IF AddVectoredExceptionHandler(1, SYSTEM.ADR(ExceptionHandler)) = 0 THEN
    AssertWinErr(GetLastError())
  END
END NewStartup;


BEGIN
  HWnd                  := 0;
  Log                   := NoLog;
  Sol                   := FALSE;
  ExceptionDepth        := 0;

  (* Initialise console input/output *)
  Stdin  := GetStdHandle(-10);  (* -10:   StdInputHandle *)
  Stdout := GetStdHandle(-11);  (* -11:   StdOutputHandle *)
  SetConsoleOutputCP(65001);    (* 65001: UTF8            *)
  crlf := $0D 0A 00$;

  Log := WriteStdout;

  IF NewLoad IN Preload.LoadFlags THEN  (* New non-copying startup variant *)

    (*wsn("* WinHost starting, new style binary.");*)
    NewStartup

  ELSE

    IF Verbose IN Preload.LoadFlags THEN
      ws("* WinHost starting, old style binary, Preload.ImgHeader at "); wh(ORD(Preload.ImgHeader)); wsn("H.");
      ws("* Initial RSP "); wh(GetRSP()); wsn("H.");
    END;

    PrepareOberonMachine;

    IF Preload.ImgHeader.size # (Preload.ImgHeader.vars + Preload.ImgHeader.varsize + 15) DIV 16 * 16 THEN
      assertmsg(FALSE, "Invalid image size in bootstrap image (WinHost) header")
    END;

    (* Copy boot module into newly committed memory and switch PC to the new code. *)
    SYSTEM.COPY(ORD(Preload.ImgHeader), ModuleSpace, Preload.ImgHeader.size);

    (***** Transfer program counter to copied code *****)
    IncPC(ModuleSpace - ORD(Preload.ImgHeader));

    Log := WriteStdout;  (* Correct Log fn address following move *)
    IF Verbose IN Preload.LoadFlags THEN
      ws("* Transferred PC to code copied to Oberon memory at ");  wh(GetPC());  wsn("H.")
    END;

    SYSTEM.PUT(SYSTEM.ADR(Root), ModuleSpace);
    INC(Root.cmd, ModuleSpace);
    INC(Root.ptr, ModuleSpace);
    RelocatePointerAddresses(Root.ptr, ModuleSpace + Root.vars);

    Root.next := NIL;
    AllocPtr := ModuleSpace + Preload.ImgHeader.size;

    InitSysHandlers;

    (* Trap OS exceptions *)
    IF AddVectoredExceptionHandler(1, SYSTEM.ADR(ExceptionHandler)) = 0 THEN
      AssertWinErr(GetLastError())
    END;

    LoadRemainingModules(SYSTEM.VAL(Module, ORD(Preload.ImgHeader) + Preload.ImgHeader.size));
    ExitProcess(0)
  END;

  ParseCommandLine

END WinHost.
