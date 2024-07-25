MODULE Host;  (* DCWB 2024-01 Windows host interface *)

IMPORT SYSTEM;

CONST
  (* Host definition *)
  Win*  = 1;
  Lin*  = 2;
  Host* = Win;

  (* Platform independent file open kinds *)
  OpenRO* = 0;  (* Open r/o, fail if doesn't exist *)
  OpenRW* = 1;  (* Open r/w, fail if doesn't exist *)

  (* Oberon system library procedure indices *)
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

  (* Windows VirtualAlloc flags *)
  MEMRESERVE           = 2000H;
  MEMCOMMIT            = 1000H;
  PAGEEXECUTEREADWRITE = 40H;


TYPE
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
    P1Home, P2Home, P3Home, P4Home, P5Home, P6Home: INTEGER;             (*   0H *)
    ContextFlags, MxCsr:                            SYSTEM.CARD32;       (* 030H *)
    SegCs, SegDs, SegEs, SegFs, SegGs, SegSs:       SYSTEM.CARD16;       (* 038H *)
    EFlags:                                         SYSTEM.CARD32;       (* 044H *)
    Dr0, Dr1, Dr2, Dr3, Dr6, Dr7:                   INTEGER;             (* 048H *)
    rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi:         INTEGER;             (* 078H *)
    r8,  r9,  r10, r11, r12, r13, r14, r15, rip:    INTEGER;             (* 0B8H *)
    FloatingPointContext:                           ARRAY 64 OF INTEGER; (* 100H *)
    VectorRegisters:  (* 26 x M128A *)              ARRAY 52 OF INTEGER; (* 300H *)
    VectorControl:                                  INTEGER;             (* 4A0H *)
    DebugControl:                                   INTEGER;             (* 4A8H *)
    LastBranchToRip:                                INTEGER;             (* 4B0H *)
    LastBranchFromRip:                              INTEGER;             (* 4B8H *)
    LastExceptionToRip:                             INTEGER;             (* 4C0H *)
    LastExceptionFromRip:                           INTEGER;             (* 4C8H *)
  END;

  ExceptionPointers = POINTER- TO ExceptionPointersDesc;
  ExceptionPointersDesc = RECORD-
    exception: Exception;
    context:   Context
  END;

  PreLoadVars* = RECORD-
    Exeadr*:      INTEGER;  (* Image PE header address (Windows only)  *)
    dummy2:       INTEGER;
    CoreAdr*:     INTEGER;  (* Oberon core address, usually 100000000H *)
    CoreSize*:    INTEGER;  (* Length of inner core                    *)
    MadrPreload*: INTEGER;  (* Start of preload section                *)
    MadrIcon*:    INTEGER;
    IconSize*:    INTEGER;
    dummy7:       INTEGER;
  END;

  FileName*    = ARRAY MaxPath OF CHAR;
  WideFileName = ARRAY 260 OF SYSTEM.CARD16;

  HostFile* = RECORD
    name*:       FileName;
    registered*: BOOLEAN;
    handle:      INTEGER;
    unregname:   FileName;
  END;

  FileInfo* = RECORD
    name*:   FileName;
    date*:   INTEGER;
    length*: INTEGER;
  END;

  FileEntryHandler* = PROCEDURE(info: FileInfo; VAR continue: BOOLEAN);


(* Start of pre-loaded variables (preset by Link.mod/Windows loader) *)

VAR
  Preload*: PreLoadVars;

  (* Pre-loaded Kernel32 imports *)
  AddVectoredExceptionHandler*:    PROCEDURE\(first, filter: INTEGER): INTEGER;
  CloseHandle*:                    PROCEDURE\(hObject: INTEGER): INTEGER;
  CreateFileW*:                    PROCEDURE\(lpFileName, dwDesiredAccess, dwShareMode, lpSecurityAttributes, dwCreationDisposition, dwFlagsAndAttributes, hTemplateFile: INTEGER): INTEGER;
  CreateProcessW*:                 PROCEDURE\(appname, cmdline, procattr, threadattr,
                                              handles, flags, env, dir,
                                              startupinfo, processinfo: INTEGER): INTEGER;
  DeleteFileW*:                    PROCEDURE\(lpFilename: INTEGER): INTEGER;
  ExitProcess:                     PROCEDURE\(exitcode: INTEGER);
  FileTimeToLocalFileTime*:        PROCEDURE\(filetime, localfiletime: INTEGER): INTEGER;
  FileTimeToSystemTime*:           PROCEDURE\(filetime, systemtime: INTEGER): INTEGER;
  FindClose*:                      PROCEDURE\(hfind: INTEGER): INTEGER;
  FindFirstFileExW*:               PROCEDURE\(name, levels, data, op, filter, flags: INTEGER): INTEGER;
  FindNextFileW*:                  PROCEDURE\(hfind, data: INTEGER): INTEGER;
  FlushFileBuffers*:               PROCEDURE\(hFile: INTEGER): INTEGER;
  FormatMessageW:                  PROCEDURE\(flags, source, mid, lid, buf, size, args: INTEGER): INTEGER;
  GetCommandLineW*:                PROCEDURE\(): INTEGER;
  GetCurrentDirectoryW*:           PROCEDURE\(nsize, pbuffer: INTEGER): INTEGER;
  GetCurrentProcessId*:            PROCEDURE\(): INTEGER;
  GetEnvironmentVariableW*:        PROCEDURE\(lpName, lpBuffer, nSize: INTEGER): INTEGER;
  GetFinalPathNameByHandleW*:      PROCEDURE\(hfile, name, len, flags: INTEGER): INTEGER;
  GetFileAttributesExW*:           PROCEDURE\(lpName, fInfoLevelId, lpFileInformation: INTEGER): INTEGER;  (* fInfoLevelId Must be 0 (GetFileExInfoStandard) *)
  GetFileAttributesW*:             PROCEDURE\(lpFileName: INTEGER): INTEGER;
  GetFileInformationByHandleEx:    PROCEDURE\(hfile, infoclass, infoptr, bufsize: INTEGER): INTEGER;
  GetFileSizeEx*:                  PROCEDURE\(hFile, lpFileSize: INTEGER): INTEGER;
  GetLastError*:                   PROCEDURE\(): INTEGER;
  GetModuleFileNameW*:             PROCEDURE\(hModule, lpFilename, nSize: INTEGER): INTEGER;
  GetProcAddress*:                 PROCEDURE\(hmodule, procname: INTEGER): INTEGER;
  GetStdHandle*:                   PROCEDURE\(nStdHandle: SYSTEM.INT32): INTEGER;
  GetSystemTimePreciseAsFileTime*: PROCEDURE\(tickAdr: INTEGER);
  GetTempFileNameA*:               PROCEDURE\(pathadr, prefixadr, unique, tempfilenameadr: INTEGER): INTEGER;
  GetTempPathA*:                   PROCEDURE\(buflen, bufadr: INTEGER): INTEGER;
  LoadLibraryA*:                   PROCEDURE\(libname: INTEGER): INTEGER;
  MoveFileExW*:                    PROCEDURE\(lpExistingFileName, lpNewFileName, dwFlags: INTEGER): INTEGER;
  ReadFile:                        PROCEDURE\(hFile, lpBuffer, nNumberOfBytesToRead, lpNumberOfBytesRead, lpOverlapped: INTEGER): INTEGER;
  SetConsoleOutputCP*:             PROCEDURE\(codepage: INTEGER) (* : INTEGER *);
  SetEndOfFile*:                   PROCEDURE\(hFile: INTEGER): INTEGER;
  SetFileInformationByHandle*:     PROCEDURE\(hFile, infoClass, info, bufsize: INTEGER): INTEGER;
  SetFilePointerEx*:               PROCEDURE\(hFile, liDistanceToMove, lpNewFilePointer, dwMoveMethod: INTEGER): INTEGER;
  Sleep*:                          PROCEDURE\(ms: INTEGER);
  UnmapViewOfFile:                 PROCEDURE\(adr: INTEGER): INTEGER;
  VirtualAlloc*:                   PROCEDURE\(address, size, type, protection: INTEGER): INTEGER;
  VirtualQuery*:                   PROCEDURE\(address, buffer, length: INTEGER): INTEGER;
  WriteFile:                       PROCEDURE\(hFile, lpBuffer, nNumberOfBytesToWrite, lpNumberOfBytesWritten, lpOverlapped: INTEGER): INTEGER;

  (* Pre-loaded Gdi32 imports *)
  BitBlt*:                         PROCEDURE\(dc, x, y, cx, cy, sc, x1, y1, op: INTEGER): INTEGER;
  CreateBitmap*:                   PROCEDURE\(w, h, pl, de, bs: INTEGER): INTEGER;
  CreateCompatibleDC*:             PROCEDURE\(dc: INTEGER): INTEGER;
  CreateDCA*:                      PROCEDURE\(drive, device, port, devmode: INTEGER): INTEGER;
  CreateDIBSection*:               PROCEDURE\(dc, bi, us, bs, sc, of: INTEGER): INTEGER;
  CreateFontA*:                    PROCEDURE\(cHeight,  cWidth,        cEscapement,    cOrientation,
                                              cWeight,  bItalic,       bUnderline,     bStrikeOut,
                                              iCharSet, iOutPrecision, iClipPrecision, iQuality,
                                              iPitchAndFamily,         pszFaceName: INTEGER): INTEGER;
  CreatePen*:                      PROCEDURE\(style, width, colour: INTEGER): INTEGER;
  CreateRectRgn*:                  PROCEDURE\(x1, y1, x2, y2: INTEGER): INTEGER;
  DeleteObject*:                   PROCEDURE\(ob: INTEGER): INTEGER;
  GetGlyphOutlineW*:               PROCEDURE\(hdc, uChar, fuFormat, lpgm, cjBuffer, pvBuffer, lpmat2: INTEGER): INTEGER;
  GetCharABCWidthsW*:              PROCEDURE\(hdc, first, last, abcadr: INTEGER): INTEGER;
  GetDeviceCaps*:                  PROCEDURE\(hdc, index: INTEGER): INTEGER;
  GetOutlineTextMetricsW*:         PROCEDURE\(hdc, bufsize, buffer: INTEGER): INTEGER;
  GetStockObject*:                 PROCEDURE\(typ: INTEGER): INTEGER;
  Rectangle*:                      PROCEDURE\(dc, l, t, r, b: INTEGER): INTEGER;
  SelectObject*:                   PROCEDURE\(dc, ob: INTEGER): INTEGER;
  SetDCPenColor*:                  PROCEDURE\(dc, col: INTEGER): INTEGER;
  SetDCBrushColor*:                PROCEDURE\(dc, col: INTEGER): INTEGER;


  (* Pre-loaded User32 imports *)
  AdjustWindowRectEx*:             PROCEDURE\(rect, style, menu, exstyle: INTEGER): INTEGER;
  BeginPaint*:                     PROCEDURE\(wn, ps: INTEGER): INTEGER;
  CreateIconFromResourceEx*:       PROCEDURE\(adr, size, ficon, ver, cx, cy, fl: INTEGER): INTEGER;
  CreateWindowExW*:                PROCEDURE\(es, cn, wn, st, x, y, w, h, pa, me, in, lp: INTEGER): INTEGER;
  DefWindowProcW*:                 PROCEDURE\(wn, ms, wp, lp: INTEGER): INTEGER;
  DispatchMessageW*:               PROCEDURE\(ms: INTEGER): INTEGER;
  EndPaint*:                       PROCEDURE\(wn, ps: INTEGER): INTEGER;
  GetClientRect*:                  PROCEDURE\(wn, rec: INTEGER): INTEGER;
  GetClipboardFormatNameW*:        PROCEDURE\(format, name, maxcount: INTEGER): INTEGER;
  GetCursorPos*:                   PROCEDURE\(pos: INTEGER);
  GetDCEx*:                        PROCEDURE\(wn, clip, flags: INTEGER): INTEGER;
  GetDpiForWindow*:                PROCEDURE\(wn: INTEGER): INTEGER;
  GetMessageW*:                    PROCEDURE\(lm, wn, mn, mx: INTEGER): INTEGER;
  GetQueueStatus*:                 PROCEDURE\(fl: INTEGER): INTEGER;
  GetWindowDC*:                    PROCEDURE\(wn: INTEGER): INTEGER;
  GetWindowRect*:                  PROCEDURE\(wn, rec: INTEGER): INTEGER;
  LoadCursorW*:                    PROCEDURE\(in, cn: INTEGER): INTEGER;
  MessageBoxA*:                    PROCEDURE\(w, t, c, u: INTEGER)(*: INTEGER*);
  MessageBoxW*:                    PROCEDURE\(w, t, c, u: INTEGER): INTEGER;
  MoveWindow*:                     PROCEDURE\(wn, x, y, w, h, repaint: INTEGER);
  MsgWaitForMultipleObjects*:      PROCEDURE\(cn, hs, wa, ms, wm: INTEGER);
  PeekMessageW*:                   PROCEDURE\(lm, wn, mn, mx, rm: INTEGER): INTEGER;
  PostMessageW*:                   PROCEDURE\(hwnd, msg, wp, lp: INTEGER): INTEGER;
  PostQuitMessage*:                PROCEDURE\(rc: INTEGER);
  RegisterClassExW*:               PROCEDURE\(wc: INTEGER): INTEGER;
  ReleaseCapture*:                 PROCEDURE\;
  ReleaseDC*:                      PROCEDURE\(wn, dc: INTEGER): INTEGER;
  SetCapture*:                     PROCEDURE\(wn: INTEGER);
  SetProcessDpiAwarenessContext*:  PROCEDURE\(cx: INTEGER): INTEGER;
  SetWindowRgn*:                   PROCEDURE\(wn, rgn, redraw: INTEGER): INTEGER;
  ShowCursor*:                     PROCEDURE\(sh: INTEGER);
  ShowWindow*:                     PROCEDURE\(wn, cm: INTEGER);
  TrackMouseEvent*:                PROCEDURE\(evtrk: INTEGER): INTEGER;
  TranslateMessage*:               PROCEDURE\(ms: INTEGER): INTEGER;
  InvalidateRect*:                 PROCEDURE\(wn, rc, er: INTEGER): INTEGER;

(* End of pre-loaded variables *)

  (* Linker expects handlers to follow immediately after pre-loaded vars *)
  Handlers*: ARRAY HandlerCount OF PROCEDURE;

  Stdin:    INTEGER;
  Stdout:   INTEGER;
  Newline*: ARRAY 3 OF CHAR;
  HWnd:     INTEGER;        (* Set when a window has been created *)

  AllocPtr*:    INTEGER;   (* Start of remaining free module space *)
  CommitLen*:   INTEGER;   (* Committed module space memory *)


  (* Partially parsed command line *)
  CommandLine*: ARRAY 1024 OF CHAR;
  CmdModule*:   ARRAY 32 OF CHAR;
  CmdCommand*:  ARRAY 32 OF CHAR;
  ArgStart*:    INTEGER;

  ExitCode:    INTEGER;
  TrapDepth:   INTEGER;
  TrapHandler: PROCEDURE(adr: INTEGER; desc: ARRAY OF CHAR);

  DeleteUnregisteredFilesAtExit: PROCEDURE;

  MonthLen:    ARRAY 11 OF INTEGER;


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
  (*IF d > 9 THEN s[j] := "0"; INC(j) END;*)
  s[j] := 0X;  DEC(j);
  WHILE i < j DO ch:=s[i];  s[i]:=s[j];  s[j]:=ch;  INC(i);  DEC(j) END;
END IntToHex;

PROCEDURE IntToDecimal*(n: INTEGER; VAR s: ARRAY OF CHAR);
VAR i, j: INTEGER;  ch: CHAR;
BEGIN
  IF n = 8000000000000000H THEN s[0] := 0X; Append("-9223372036854775808", s)
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

PROCEDURE Log*(s: ARRAY OF BYTE);
VAR written, res: INTEGER;
BEGIN
  res := WriteFile(Stdout, SYSTEM.ADR(s), Length(s), SYSTEM.ADR(written), 0);
END Log;

PROCEDURE wc*(c: CHAR); BEGIN Log(c) END wc;

PROCEDURE wn*; BEGIN Log(Newline) END wn;

PROCEDURE ws*(s: ARRAY OF CHAR); BEGIN Log(s) END ws;

PROCEDURE wsr*(s: ARRAY OF CHAR; w: INTEGER; c: CHAR);  (* Right justified with leading c *)
BEGIN DEC(w, Length(s));  WHILE w > 0 DO wc(c); DEC(w) END; Log(s) END wsr;

PROCEDURE wsn*(s: ARRAY OF CHAR); BEGIN Log(s); Log(Newline) END wsn;

PROCEDURE wh*(n: INTEGER);
VAR hex: ARRAY 32 OF CHAR;
BEGIN IntToHex(n, hex); Log(hex) END wh;

PROCEDURE whr*(n, w: INTEGER; c: CHAR);  (* Right justified with leading zeroes *)
VAR hex: ARRAY 32 OF CHAR;
BEGIN IntToHex(n, hex); wsr(hex, w, c) END whr;

PROCEDURE wi*(n: INTEGER);
VAR dec: ARRAY 32 OF CHAR;
BEGIN IntToDecimal(n, dec); Log(dec) END wi;

PROCEDURE wir*(n, w: INTEGER; c: CHAR);
VAR dec: ARRAY 32 OF CHAR;
BEGIN IntToDecimal(n, dec); wsr(dec, w, c) END wir;

PROCEDURE wb*(n: INTEGER);
BEGIN WHILE n > 0 DO wc(" "); DEC(n) END END wb;


(* -------------------------------------------------------------------------- *)
(* ----------------------------- Time functions ----------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Time*(): INTEGER;  (* In 100 nanosecond ticks since 2000-01-01 UTC *)
VAR tick: INTEGER;
BEGIN GetSystemTimePreciseAsFileTime(SYSTEM.ADR(tick));
RETURN tick - 10000000 * 24 * 60 * 60 * (399 * 365 + 96)  (* Bias to 2000-01-01 UTC *)
END Time;


PROCEDURE TimeAsClock*(time: INTEGER): INTEGER;
(* Returns UTC time in PO2013 format:            *)
(* 38/year-2000,4/month,5/day,5/hour,6/min,6/sec *)
CONST
  DaysperYear     = 365;
  Daysper4years   = DaysperYear     * 4  + 1;
  Daysper100years = Daysper4years   * 25 - 1;
  Daysper400years = Daysper100years * 4  + 1;
VAR year, month, day, sec, qccycles, ccycles, qcycles: INTEGER;
BEGIN
  sec      := time DIV 10000000 - 86400 * (31+29);  (* seconds relative to 1st March 2000 (leap century) *)
  day      := sec DIV 86400;            sec  := sec MOD 86400;
  qccycles := day DIV Daysper400years;  day  := day MOD Daysper400years;
  ccycles  := day DIV Daysper100years;  IF ccycles =  4 THEN DEC(ccycles)  END;
  DEC(day, ccycles * Daysper100years);
  qcycles  := day DIV Daysper4years;    IF qcycles = 25 THEN DEC(qcycles)  END;
  DEC(day, qcycles * Daysper4years);
  year := day DIV DaysperYear;          IF year = 4 THEN DEC(year) END;
  DEC(day, year * DaysperYear);
  year := year + 4 * qcycles + 100 * ccycles + 400 * qccycles;
  month := 0;
  WHILE (month < 11) & (day >= MonthLen[month]) DO
    DEC(day, MonthLen[month]);  INC(month)
  END;
  IF month >= 10 THEN DEC(month, 12);  INC(year) END;
  RETURN (((((  year  + 2000)      * 10H
              + month + 3)         * 20H
              + day   + 1)         * 20H
              + sec DIV 3600)      * 40H
              + sec DIV 60 MOD 60) * 40H
              + sec MOD 60
END TimeAsClock;

(*
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
*)


PROCEDURE Clock*(): INTEGER;
(* Returns local time as                         *)
(* 38/year-2000,4/month,5/day,5/hour,6/min,6/sec *)
BEGIN RETURN TimeAsClock(Time()) END Clock;

PROCEDURE WriteDate(clock: INTEGER);
VAR year: INTEGER;
BEGIN year := clock DIV 4000000H;
  IF year < 0 THEN wir(-year, 2, "0"); ws("BC-") ELSE wir(year, 4, "0"); wc("-") END;
  wir(clock DIV 400000H MOD 10H, 2, "0");  (*month*)    wc("-");
  wir(clock DIV 20000H  MOD 20H, 2, "0");  (*day*)
END WriteDate;

PROCEDURE WriteTime(clock: INTEGER);
BEGIN
  wir(clock DIV 1000H MOD 20H, 2, "0");  (*hour*)     wc(":");
  wir(clock DIV 40H MOD 40H,   2, "0");  (*min*)      wc(":");
  wir(clock MOD 40H,           2, "0");  (*sec*)
END WriteTime;

PROCEDURE WriteClock(clock: INTEGER);
BEGIN WriteDate(clock); wc(" "); WriteTime(clock)
END WriteClock;



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
  rowstart  := ( start             DIV 16) * 16;
  dumplimit := ((start + len + 15) DIV 16) * 16;
  WHILE rowstart < dumplimit DO
    wb(indent); whr(rowstart, 12, "0"); ws("  ");
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
      IF bytes[i] >= 0 THEN whr(bytes[i], 2, "0");  wc(" ") ELSE ws("   ") END;
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


(* -------------------- Pulling variables out of memory --------------------- *)

PROCEDURE GetString*(VAR p: INTEGER; VAR s: ARRAY OF CHAR);
VAR i: INTEGER; ch: CHAR;
BEGIN i := 0;
  REPEAT SYSTEM.GET(p, ch);
    IF i < LEN(s) THEN s[i] := ch END;
    INC(p);  INC(i)
  UNTIL ch = 0X
END GetString;


PROCEDURE GetUnsigned*(VAR adr, n: INTEGER);
VAR i: BYTE; s: INTEGER;
BEGIN
  n := 0; s := 0;
  REPEAT
    SYSTEM.GET(adr, i);  INC(adr);
    INC(n, LSL(i MOD 128, s));  INC(s, 7)
  UNTIL i < 128
END GetUnsigned;


(* ----------------------------- Exit handling ------------------------------ *)

PROCEDURE SetExitCode*(code: INTEGER);
BEGIN ExitCode := code END SetExitCode;

PROCEDURE Exit*;
BEGIN
  IF DeleteUnregisteredFilesAtExit # NIL THEN DeleteUnregisteredFilesAtExit END;
  ExitProcess(ExitCode)
END Exit;

(* ---------------------- Exception and trap handling ----------------------- *)

PROCEDURE\ ExceptionHandler(p: ExceptionPointers);  (* Called by Windows *)
VAR excpcode: INTEGER;  desc, hex: ARRAY 32 OF CHAR;
BEGIN
  INC(TrapDepth);
  IF TrapDepth > 1 THEN
    wsn("  Nested exception.")
  ELSE
    excpcode := p.exception.ExceptionCode;
    IF    excpcode = 080000003H THEN desc := "Breakpoint (INT 3)";
    ELSIF excpcode = 080000004H THEN desc := "Single step (0F1H instr)";
    ELSIF excpcode = 0C0000005H THEN desc := "Access violation";
    ELSIF excpcode = 0C0000006H THEN desc := "In-page error";
    ELSIF excpcode = 0C000001DH THEN desc := "Illegal instruction";
    ELSIF excpcode = 0C000008EH THEN desc := "Divide by zero";
    ELSIF excpcode = 0C0000094H THEN desc := "Integer divide by zero";
    ELSIF excpcode = 0C0000096H THEN desc := "Privileged instruction";
    ELSIF excpcode = 0C00000FDH THEN desc := "Stack Overflow";
    ELSE
      desc := "Exception "; IntToHex(excpcode, hex);
      Append(hex, desc); Append("H", desc)
    END;
    IF TrapHandler = NIL THEN
      ws("  Trap: "); ws(desc);
      ws(" at address "); wh(p.exception.ExceptionAddress); wsn("H.")
    ELSE
      TrapHandler(p.exception.ExceptionAddress, desc)
    END;
    IF p # NIL THEN
      wsn("Dump:");
      ws("  rax "); whr(p.context.rax, 16, "0");  ws("  rbx "); whr(p.context.rbx, 16, "0");
      ws("  rcx "); whr(p.context.rcx, 16, "0");  ws("  rdx "); whr(p.context.rdx, 16, "0");  wn;
      ws("  rsp "); whr(p.context.rsp, 16, "0");  ws("  rbp "); whr(p.context.rbp, 16, "0");
      ws("  rsi "); whr(p.context.rsi, 16, "0");  ws("  rdi "); whr(p.context.rdi, 16, "0");  wn;
      ws("  r8  "); whr(p.context.r8,  16, "0");  ws("  r9  "); whr(p.context.r9,  16, "0");
      ws("  r10 "); whr(p.context.r10, 16, "0");  ws("  r11 "); whr(p.context.r11, 16, "0");  wn;
      ws("  r12 "); whr(p.context.r12, 16, "0");  ws("  r13 "); whr(p.context.r13, 16, "0");
      ws("  r14 "); whr(p.context.r14, 16, "0");  ws("  r15 "); whr(p.context.r15, 16, "0");  wn;
      wsn("  Top of stack:");
      DumpMem(2, p.context.rsp, p.context.rsp, 128)
    END
  END;
  ExitProcess(99)
END ExceptionHandler;


(* ----------------------------- Trap handlers ------------------------------ *)

PROCEDURE SetTrapHandler*(handler: PROCEDURE(adr: INTEGER; desc: ARRAY OF CHAR));
BEGIN TrapHandler := handler END SetTrapHandler;

PROCEDURE ResetTrap*; BEGIN TrapDepth := 0 END ResetTrap;


PROCEDURE Trap*(retoffset: INTEGER; desc: ARRAY OF CHAR);
VAR adr, modadr: INTEGER;
BEGIN  (* retoffset is callers local var size *)
  IF retoffset < 0 THEN retoffset := -32 END;  (* Address our own return address *)
  SYSTEM.GET(SYSTEM.ADR(LEN(desc)) + 16 + retoffset, adr);
  INC(TrapDepth);
  IF (TrapHandler = NIL) OR (TrapDepth > 1) THEN
    wn;  ws("  Trap: ");  ws(desc);  ws(" at address "); wh(adr); wsn("H.")
  ELSE
    TrapHandler(adr, desc);
  END;
  ExitProcess(99)
END Trap;

PROCEDURE NewPointerHandler();
BEGIN Trap(0, "New pointer handler not istalled") END NewPointerHandler;

PROCEDURE AssertionFailureHandler();
BEGIN Trap(0, "Assertion failure")       END AssertionFailureHandler;

PROCEDURE ArraySizeMismatchHandler();
BEGIN Trap(0, "Array size mismatch")     END ArraySizeMismatchHandler;

PROCEDURE UnterminatedStringHandler();
BEGIN Trap(0, "Unterminated string")     END UnterminatedStringHandler;

PROCEDURE IndexOutOfRangeHandler();
BEGIN Trap(0, "Index out of range")      END IndexOutOfRangeHandler;

PROCEDURE NilPointerDereferenceHandler();
BEGIN Trap(0, "NIL pointer dereference") END NilPointerDereferenceHandler;

PROCEDURE TypeGuardFailureHandler();
BEGIN Trap(0, "Type guard failure")      END TypeGuardFailureHandler;


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
BEGIN ASSERT(i < LEN(src)); result := ORD(src[i]);  INC(i);
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
  ASSERT(i < LEN(dst));
  ASSERT(c >= 0);  ASSERT(c < 80000000H);
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

PROCEDURE GetUtf16Mem*(VAR adr, ch32: INTEGER);  (* Pull 32 bit Unicode from command line *)
VAR ch16: SYSTEM.CARD16;
BEGIN
  SYSTEM.GET(adr, ch16);  INC(adr, 2);
  IF ch16 DIV 400H # 36H THEN ch32 := ch16 (* Not a high surrogate *)
  ELSE
    ch32 := LSL(ch16 MOD 400H, 10) + 10000H;
    SYSTEM.GET(adr, ch16);
    IF ch16 DIV 400H = 37H THEN
      INC(adr, 2);  (* Add low surrogate part *)
      INC(ch32, ch16 MOD 400H)
    END
  END
END GetUtf16Mem;

(*
PROCEDURE GetUtf16*(src: ARRAY OF SYSTEM.CARD16; VAR i: INTEGER): INTEGER;
VAR adr, result: INTEGER;
BEGIN
  result := src[i];  INC(i);
  IF result DIV 400H = 36H THEN    (* High surrogate *)
    result := LSL(result MOD 400H, 10) + 10000H;
    IF (i < LEN(src)) & (src[i] DIV 400H = 37H) THEN  (* Low surrogate *)
      INC(result, src[i] MOD 400H);  INC(i)
    END
  END
RETURN result END GetUtf16;
*)

PROCEDURE PutUtf16*(ch: INTEGER; VAR dst: ARRAY OF SYSTEM.CARD16; VAR i: INTEGER);
BEGIN
  ASSERT(i < LEN(dst));
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
VAR adr, lim, ch32, j: INTEGER;
BEGIN
  adr := SYSTEM.ADR(src);  lim := adr + 2 * LEN(src);  j := 0;
  REPEAT
    GetUtf16Mem(adr, ch32);
    IF ch32 # 0 THEN PutUtf8(ch32, dst, j) END
  UNTIL (ch32 = 0) OR (adr >= lim) OR (j >= LEN(dst));
  IF j >= LEN(dst) THEN j := LEN(dst)-1 END;
  dst[j] := 0X;  INC(j)
RETURN j END Utf16ToUtf8;


(* -------------------------------------------------------------------------- *)

PROCEDURE AssertWinError*(err: INTEGER);
VAR
  msgW:    ARRAY 1024 OF SYSTEM.CARD16;
  msg:     ARRAY 1024 OF CHAR;
  i, res:  INTEGER;
BEGIN
  IF err # 0 THEN
    res := FormatMessageW(1000H, 0, err, 0, SYSTEM.ADR(msgW), 1024, 0); (* 1000H: FORMAT_MESSAGE_FROM_SYSTEM *)
    res := Utf16ToUtf8(msgW, msg);
    i := Length(msg) - 1;
    WHILE (i >= 0) & (msg[i] <= " ") DO DEC(i) END;  (* Drop trailing space/Newline(s) *)
    WHILE (i >= 0) & (msg[i] =  ".") DO DEC(i) END;  (* Drop trailing periods *)
    msg[i + 1] := 0X;
    Trap(LEN(msgW)*2 + LEN(msg) + 16, msg);
  END
END AssertWinError;


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

PROCEDURE MakeUnregName(VAR file: HostFile);
VAR stamp: ARRAY 20 OF CHAR;
BEGIN
  file.unregname := file.name;
  Append(".unreg-", file.unregname);
  IntToDecimal(Time() MOD 20000000H, stamp);
  Append(stamp, file.unregname)
END MakeUnregName;


PROCEDURE FileOld*(VAR file: HostFile): BOOLEAN;
VAR
  name16:  ARRAY 261 OF SYSTEM.CARD16;
  rv:      INTEGER;
  success: BOOLEAN;
BEGIN
  rv := Utf8ToUtf16(file.name, name16);
  file.handle := CreateFileW(SYSTEM.ADR(name16), 010000000H (* access: GenericAll *),
                             7 (* mode: FileShareRead, FileShareWrite, FileShareDelete *), 0,
                             3 (* disposition: open exisiting *), 0 (* flags: none *), 0);
  IF file.handle < 0 THEN  (* Try again read only *)
    file.handle := CreateFileW(SYSTEM.ADR(name16), 080000000H (* access: GenericRead *),
                               1 (* mode: FileShareRead *), 0,
                               3 (* disposition: open exisiting *), 0 (* flags: none *), 0);
  END;
  success := file.handle >= 0;
  IF success THEN
    file.registered := TRUE;
    MakeUnregName(file);
  END
RETURN success END FileOld;


PROCEDURE FileLength*(file: HostFile): INTEGER;
VAR length: INTEGER;
BEGIN
  ASSERT(file.handle >= 0);
  ASSERT(GetFileSizeEx(file.handle, SYSTEM.ADR(length)) # 0);
RETURN length END FileLength;


PROCEDURE FileSetDisposition(handle, disposition: INTEGER);
BEGIN ASSERT(SetFileInformationByHandle(handle, 4, SYSTEM.ADR(disposition), 8) # 0);
END FileSetDisposition;


PROCEDURE FileNew*(VAR file: HostFile);
BEGIN file.registered := FALSE;  file.handle := -1 END FileNew;


PROCEDURE EnsureHandle(VAR file: HostFile);
VAR name16: ARRAY 261 OF SYSTEM.CARD16;  rv, retries: INTEGER;
BEGIN
  IF file.handle < 0 THEN ASSERT(~file.registered);
    retries := 0;
    REPEAT
      MakeUnregName(file);
      rv := Utf8ToUtf16(file.unregname, name16);
      file.handle := CreateFileW(SYSTEM.ADR(name16), 010000000H (* access: GenericAll *),
                                 7 (* mode: FileShareRead, FileShareWrite, FileShareDelete *), 0,
                                 1 (* disposition: create new *), 0 (* flags: none *), 0);
      rv := GetLastError();  INC(retries)
    UNTIL (file.handle >= 0) OR (rv # 80 (* file exists *)) OR (retries > 100);
    IF retries > 100 THEN Trap(-1, "Couldn't find unused temporary file name") END;
    IF file.handle < 0 THEN AssertWinError(rv) END;
    FileSetDisposition(file.handle, 1);  (* 1 = DeleteFile on close *)
  END
END EnsureHandle;


PROCEDURE FileFlush*(file: HostFile);
VAR rv: INTEGER;
BEGIN IF file.handle >= 0 THEN rv := FlushFileBuffers(file.handle) END END FileFlush;


PROCEDURE FileClose*(VAR file: HostFile);
VAR
  name16: ARRAY 261 OF SYSTEM.CARD16;
  rv:     INTEGER;
BEGIN
  IF file.handle >= 0 THEN
    ASSERT(CloseHandle(file.handle) # 0);
    IF ~file.registered THEN
      rv := Utf8ToUtf16(file.unregname, name16);
      rv := DeleteFileW(SYSTEM.ADR(name16));
    END
  END;
  file.handle := -1
END FileClose;


PROCEDURE FileSetPos*(VAR file: HostFile; pos: INTEGER);
BEGIN
  EnsureHandle(file);
  IF SetFilePointerEx(file.handle, pos, 0, 0) = 0 THEN
    Trap(0, "Host.FileSetPos: SetFilePointerEx failed")
  END
END FileSetPos;


PROCEDURE FileMove*(source, dest: ARRAY OF CHAR): INTEGER;
VAR
  sourcew: ARRAY MaxPath OF SYSTEM.CARD16;
  destw:   ARRAY MaxPath OF SYSTEM.CARD16;
  res:     INTEGER;
BEGIN
  res := Utf8ToUtf16(source, sourcew);  ASSERT(res > 0);
  res := Utf8ToUtf16(dest,   destw);    ASSERT(res > 0);
  res := MoveFileExW(SYSTEM.ADR(sourcew), SYSTEM.ADR(destw), 3);  (* 1 => replace existing, 2 => copy allowed *)
  IF res = 0 THEN res := GetLastError() ELSE res := 0 END
RETURN res END FileMove;


PROCEDURE FileSetName(handle: INTEGER;  name: ARRAY OF CHAR);
TYPE renamedesc = RECORD-
    ReplaceIfExists: BYTE;
    dummyhandle:     INTEGER;
    length:          SYSTEM.CARD32;
    name16:          ARRAY MaxPath OF SYSTEM.CARD16;
  END;
VAR rename: renamedesc;  rv: INTEGER;
BEGIN
  ASSERT(handle >= 0);
  ZeroFill(rename);
  rename.ReplaceIfExists := 1;
  rename.length := Utf8ToUtf16(name, rename.name16) - 1;
  ASSERT(SetFileInformationByHandle(
    handle, 3, SYSTEM.ADR(rename), SYSTEM.SIZE(renamedesc)) # 0);
END FileSetName;


PROCEDURE FileRegister*(VAR file: HostFile);
BEGIN
  ASSERT(~file.registered);  ASSERT(file.handle >= 0);
  FileSetDisposition(file.handle, 0);  (* Remove delete on close flag *)
  FileSetName(file.handle, file.name);
  file.registered := TRUE
END FileRegister;


PROCEDURE FileUnregister*(VAR file: HostFile);
BEGIN
  FileSetName(file.handle, file.unregname);
  FileSetDisposition(file.handle, 1);  (* 1 = DeleteFile on close *)
  file.registered := FALSE;
END FileUnregister;


PROCEDURE FileTime*(file: HostFile): INTEGER;
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
  ASSERT(file.handle >= 0);
  res := GetFileInformationByHandleEx(file.handle, 0, SYSTEM.ADR(info), SYSTEM.SIZE(infodesc));
  IF res = 0 THEN AssertWinError(GetLastError()) END;
RETURN info.write - 10000000 * 24 * 60 * 60 * (399 * 365 + 96)  (* Bias to 2000-01-01 UTC *)
END FileTime;


PROCEDURE FileRead*(VAR file: HostFile; adr, len: INTEGER): INTEGER;
VAR bytesread: INTEGER;
BEGIN ASSERT(len > 0);  ASSERT(file.handle >= 0);
  IF ReadFile(file.handle, adr, len, SYSTEM.ADR(bytesread), 0) = 0 THEN
    bytesread := -1
  END;
RETURN bytesread END FileRead;


PROCEDURE FileWrite*(VAR file: HostFile; adr, len: INTEGER);
VAR byteswritten: INTEGER;
BEGIN
  IF len > 0 THEN
    EnsureHandle(file);
    ASSERT(WriteFile(file.handle, adr, len, SYSTEM.ADR(byteswritten), 0) # 0);
    ASSERT(byteswritten = len);
  END
END FileWrite;

PROCEDURE FileChangeMode*(file: HostFile; mode: INTEGER);
BEGIN (* No action on Windows *) END FileChangeMode;

PROCEDURE SetDeleteUnregisteredFilesAtExit*(delete: PROCEDURE);
BEGIN DeleteUnregisteredFilesAtExit := delete END SetDeleteUnregisteredFilesAtExit;


PROCEDURE IsSelfOrParent(dir: ARRAY OF SYSTEM.CARD16): BOOLEAN;
(* TRUE iff /^(.*[/\]|)\.\.?$/ - if ., .. or path ending in . or .. *)
VAR i, j: INTEGER;  result: BOOLEAN;
BEGIN
  result := FALSE;
  i := 0;  j := 0;
  WHILE dir[i] # 0 DO
    IF (dir[i] = ORD("/")) OR (dir[i] = 5CH) THEN j := i + 1 END;
    INC(i)
  END;
  IF dir[j] = ORD(".") THEN
    INC(j);
    IF (dir[j] = 0) OR (dir[j] = ORD(".")) & (dir[j+1] = 0) THEN
      result := TRUE
    END
  END
RETURN result END IsSelfOrParent;


PROCEDURE FileEnum(name: WideFileName; proc: FileEntryHandler; VAR cont: BOOLEAN);
VAR
  search: WideFileName;
  i, j:   INTEGER;
  len:    INTEGER;
  hfind:  INTEGER;
  inf:    FileInfo;
  data:   RECORD-
    attribs:   SYSTEM.CARD32;
    creationl: SYSTEM.CARD32;
    creationh: SYSTEM.CARD32;
    accessl:   SYSTEM.CARD32;
    accessh:   SYSTEM.CARD32;
    writel:    SYSTEM.CARD32;
    writeh:    SYSTEM.CARD32;
    sizeh:     SYSTEM.CARD32;
    sizel:     SYSTEM.CARD32;
    res0:      SYSTEM.CARD32;
    res1:      SYSTEM.CARD32;
    name:      ARRAY 260 OF SYSTEM.CARD16;
    dum1:      ARRAY  14 OF SYSTEM.CARD16;  (* alternate file name unused *)
    dum2:      SYSTEM.CARD32;
    dum3:      SYSTEM.CARD32;
    dum4:      SYSTEM.CARD16;
    dum5:      SYSTEM.CARD32   (* Not set by windows - round up to 260H byes *)
  END;
BEGIN
  IF ~IsSelfOrParent(name) THEN
    len := 0;
    WHILE name[len] # 0 DO search[len] := name[len]; INC(len) END;
    IF len > 0 THEN search[len] := ORD("/"); INC(len) END;
    i := len;
    search[i] := ORD("*");  search[i+1] := 0;
    hfind := FindFirstFileExW(SYSTEM.ADR(search), 1, SYSTEM.ADR(data), 0, 0, 0);
    cont := TRUE;
    WHILE cont & (hfind # 0) DO
      IF 4 IN SYSTEM.VAL(SET, ORD(data.attribs)) THEN  (* recurse into directory *)
        i := 0;  j := len;
        WHILE data.name[i] # 0 DO
          search[j] := data.name[i];  INC(i);  INC(j)
        END;
        search[j] := 0;
        FileEnum(search, proc, cont)
      ELSE
        (* Copy full name including directory into inf *)
        i := 0; j := 0;
        WHILE i < len DO PutUtf8(search[i], inf.name, j);  INC(i) END;
        i := 0;
        WHILE data.name[i] # 0 DO PutUtf8(data.name[i], inf.name, j);  INC(i) END;
        inf.name[j] := 0X;
        inf.date   := TimeAsClock(LSL(data.writeh, 32) + data.writel);
        inf.length := LSL(data.sizeh,  32) + data.sizel;
        proc(inf, cont)
      END;
      IF FindNextFileW(hfind, SYSTEM.ADR(data)) = 0 THEN
        i := FindClose(hfind);
        hfind := 0
      END;
    END
  END
END FileEnum;

PROCEDURE FileEnumerate*(prefix: ARRAY OF CHAR; proc: FileEntryHandler);
VAR
  len:  INTEGER;
  wfn:  WideFileName;
  cont: BOOLEAN;
BEGIN
  len  := Utf8ToUtf16("", wfn);
  FileEnum(wfn, proc, cont)
END FileEnumerate;


PROCEDURE FileDelete*(name: ARRAY OF CHAR): BOOLEAN;
VAR name16: ARRAY MaxPath OF SYSTEM.CARD16;  rv: INTEGER;
BEGIN
  rv := Utf8ToUtf16(name, name16);
  RETURN DeleteFileW(SYSTEM.ADR(name16)) # 0
END FileDelete;


(* -------------------------------------------------------------------------- *)
(* ----------------------- Virtual memory management ------------------------ *)
(* -------------------------------------------------------------------------- *)

PROCEDURE HeapInit*(reserve, commit: INTEGER): INTEGER;
CONST MEMRESERVE = 2000H;  MEMCOMMIT = 1000H;  PAGEREADWRITE = 4;
VAR heapOrg: INTEGER;
BEGIN
  (* Initialize heap *)
  heapOrg := VirtualAlloc(200000000H, reserve, MEMRESERVE, PAGEREADWRITE);
  ASSERT(heapOrg # 0);
  heapOrg := VirtualAlloc(heapOrg, commit, MEMCOMMIT, PAGEREADWRITE);
  ASSERT(heapOrg # 0)
RETURN heapOrg END HeapInit;


PROCEDURE Allocate*(size: INTEGER; VAR p, alloc: INTEGER);
VAR adr, newcommitlen: INTEGER;
BEGIN
  p     := 0;
  alloc := 0;
  size  := (size + 15) DIV 16 * 16;
  IF AllocPtr - Preload.CoreAdr + size < 80000000H THEN  (* Hard limit on reserved size 2GB due to relative addressing *)
    IF AllocPtr + size > Preload.CoreAdr + CommitLen THEN

      (* Round up to a multiple of 256K *)
      newcommitlen := (AllocPtr + size + 40000H - 1) DIV 40000H * 40000H - Preload.CoreAdr;

      (*
      ws("* CommitLen increasing from "); wh(CommitLen);
      ws("H to "); wh(newcommitlen); wsn("H *");
      *)

      (*
      ws("Calling VirtualAlloc(addr "); wh(Preload.CoreAdr+CommitLen);
      ws("H, size "); wh(newcommitlen-CommitLen);
      wsn("H, MEMCOMMIT, PAGEEXECUTEREADWRITE).");
      *)

      adr := VirtualAlloc(Preload.CoreAdr+CommitLen, newcommitlen-CommitLen, MEMCOMMIT, PAGEEXECUTEREADWRITE);
      IF adr = 0 THEN AssertWinError(GetLastError()) END;
      ASSERT(adr = Preload.CoreAdr + CommitLen);
      CommitLen := newcommitlen
    END;
    IF AllocPtr + size <= Preload.CoreAdr + CommitLen THEN
      p     := AllocPtr;
      alloc := size;
      INC(AllocPtr, size);
      (* ws("  AllocPtr ->: ");        wh(AllocPtr);    wsn("H.");*)
    END
  END
END Allocate;



(* -------------------------------------------------------------------------- *)

PROCEDURE SetHWnd*(h: INTEGER);
BEGIN HWnd := h END SetHWnd;


(* -------------------------------------------------------------------------- *)

PROCEDURE PressKeyToContinue;
VAR ch: CHAR;
BEGIN wn; ws("Press any key to continue "); ch := GetChar(); wn
END PressKeyToContinue;


(* -------------------------- Command line parsing -------------------------- *)

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

  (* Parse program name, if any. (CreateProcess can omit program name.) *)
  GetUtf16Mem(adr, ch);
  IF ch # 32 THEN
    modstart := 0;
    WHILE (i < LEN(CommandLine) - 3) & (ch # 0) & ((ch # 32) OR quoted) DO  (* Skip to first unquoted space *)
      IF     ch = ORD(".")                THEN modlimit := i
      ELSIF (ch = ORD("/")) OR (ch = 5CH) THEN modlimit := -1;  modstart := i + 1
      END;
      PutUtf8(ch, CommandLine, i);
      IF ch = 22H THEN quoted  := ~quoted END;
      GetUtf16Mem(adr, ch)
    END;

    IF modlimit < 0 THEN modlimit := i END;
    CommandLine[i] := 0X;

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
    GetUtf16Mem(adr, ch)
  END;
  ArgStart := i;  (* Note: may be end of command line *)

  (* Copy remainder of command line *)
  WHILE (i < LEN(CommandLine) - 3) & (ch # 0) DO
    PutUtf8(ch, CommandLine, i);  GetUtf16Mem(adr, ch)
  END;
  CommandLine[i] := 0X
END ParseCommandLine;


PROCEDURE Init*(stackorg: INTEGER);  (* Called from Kernel.init *)
VAR
  res:        INTEGER;
  reserveadr: INTEGER;
  reservelen: INTEGER;
BEGIN
  HWnd      := 0;
  TrapDepth := 0;
  ExitCode  := 0;

  (* Initialise console input/output *)
  Stdin  := GetStdHandle(-10);  (* -10:   StdInputHandle *)
  Stdout := GetStdHandle(-11);  (* -11:   StdOutputHandle *)
  SetConsoleOutputCP(65001);    (* 65001: UTF8            *)
  Newline := $0D 0A 00$;

  (* End of each month based on year starting 1st March *)
  MonthLen[ 0] := 31;  (* March     *)
  MonthLen[ 1] := 30;  (* April     *)
  MonthLen[ 2] := 31;  (* May       *)
  MonthLen[ 3] := 30;  (* June      *)
  MonthLen[ 4] := 31;  (* July      *)
  MonthLen[ 5] := 31;  (* August    *)
  MonthLen[ 6] := 30;  (* September *)
  MonthLen[ 7] := 31;  (* October   *)
  MonthLen[ 8] := 30;  (* November  *)
  MonthLen[ 9] := 31;  (* December  *)
  MonthLen[10] := 31;  (* January   *)

  (*
  ws("Oberon Host.Mod starting at "); WriteClock(TimeAsClock(Time())); wsn(" UTC.");
  ws("Time 0: "); WriteClock(TimeAsClock(0)); wsn(".");
  *)

  Handlers[NewProc]                   := NewPointerHandler;
  Handlers[AssertionFailureProc]      := AssertionFailureHandler;
  Handlers[ArraySizeMismatchProc]     := ArraySizeMismatchHandler;
  Handlers[UnterminatedStringProc]    := UnterminatedStringHandler;
  Handlers[IndexOutOfRangeProc]       := IndexOutOfRangeHandler;
  Handlers[NilPointerDereferenceProc] := NilPointerDereferenceHandler;
  Handlers[TypeGuardFailureProc]      := TypeGuardFailureHandler;

  AllocPtr  := Preload.CoreAdr + Preload.CoreSize;
  CommitLen := (Preload.CoreSize + 0FFFFH) DIV 10000H * 10000H;

  (* Reserve memroy beyond committed up to 2GB *)
  reserveadr := Preload.CoreAdr + CommitLen;
  reservelen := 80000000H - CommitLen;
  res := VirtualAlloc(reserveadr, reservelen, MEMRESERVE, PAGEEXECUTEREADWRITE);
  IF res = 0 THEN AssertWinError(GetLastError()) END;

  IF AddVectoredExceptionHandler(1, SYSTEM.ADR(ExceptionHandler)) = 0 THEN
    AssertWinError(GetLastError())
  END;

  ParseCommandLine;
END Init;


END Host.
