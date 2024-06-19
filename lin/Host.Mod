MODULE Host;  (* DCWB 2024-06 Linux host interface *)

IMPORT SYSTEM;

CONST
  (* Host definition *)
  Win*  = 1;
  Lin*  = 2;
  Host* = Lin;

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

  Stdin*  = 0;
  Stdout* = 1;
  Stderr* = 2;

  MaxPath* = 780;  (* Enough UTF-8 bytes for for 260 wide chars *)

  (* Linux system call function constants *)
  PROTNONE    = 0;
  PROTRWX     = 7;
  MAPPRIVATE  = 2;
  MAPFIXED    = 10H;
  MAPANON     = 20H;
  ORDONLY     = 0;
  ORDWR       = 2;
  OCREAT      = 64;
  OTMPFILE    = 20200000H;
  ATFDCWD     = -100;
  ATEMPTYPATH = 1000H;

TYPE
  PreLoadVars* = RECORD-
    Exeadr*:      INTEGER;  (* Unused on Linux                         *)
    dummy2:       INTEGER;
    CoreAdr*:     INTEGER;  (* Oberon core address, usually 100000000H *)
    CoreSize*:    INTEGER;  (* Length of inner core                    *)
    MadrPreload*: INTEGER;  (* Start of preload section                *)
    MadrIcon*:    INTEGER;
    IconSize*:    INTEGER;
    dummy7:       INTEGER;
  END;

VAR
  Preload*: PreLoadVars;

  (* Linker expects handlers to follow immediately after pre-loaded vars *)
  Handlers*: ARRAY HandlerCount OF PROCEDURE;

  Newline*:  ARRAY 3 OF CHAR;

  AllocPtr*:    INTEGER;   (* Start of remaining free module space *)
  CommitLen*:   INTEGER;   (* Committed module space memory *)


  (*
  (* Partially parsed command line *)
  CommandLine*: ARRAY 1024 OF CHAR;
  CmdModule*:   ARRAY 32 OF CHAR;
  CmdCommand*:  ARRAY 32 OF CHAR;
  ArgStart*:    INTEGER;
  *)

  ExitCode:    INTEGER;
  TrapDepth:   INTEGER;
  TrapHandler: PROCEDURE(adr: INTEGER; desc: ARRAY OF CHAR);

  MonthLen:    ARRAY 11 OF INTEGER;


(* ----------------------- Linux syscall system APIs ------------------------ *)

PROCEDURE #0   sysread(file, adr, len: INTEGER): INTEGER;
PROCEDURE #1   syswrite(handle, address, length: INTEGER): INTEGER;
PROCEDURE #2   sysopen(name, flags, mode: INTEGER): INTEGER;
PROCEDURE #3   sysclose(handle: INTEGER): INTEGER;
PROCEDURE #4   sysstat(name, adr: INTEGER): INTEGER;
PROCEDURE #5   sysfstat(handle, adr: INTEGER): INTEGER;
PROCEDURE #9   sysmmap(adr, len, prot, flags, file, offset: INTEGER): INTEGER;
PROCEDURE #10  sysmprotect(adr, len, prot: INTEGER): INTEGER;
PROCEDURE #39  sysgetpid(): INTEGER;
PROCEDURE #60  sysexit(i: INTEGER);
PROCEDURE #82  sysrename(old, new: INTEGER): INTEGER;
PROCEDURE #87  sysunlink(name: INTEGER): INTEGER;
PROCEDURE #228 sysclockgettime(id, adr: INTEGER): INTEGER;
PROCEDURE #265 syslinkat(olddirfd, oldpath, newdirfd, newpath, flags: INTEGER): INTEGER;



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
VAR res: INTEGER;
BEGIN
  res := syswrite(Stdout, SYSTEM.ADR(s), Length(s));
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
BEGIN sysexit(ExitCode) END Exit;


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
  sysexit(99)
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
(* --------------------------- Trivial Stdin read --------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE GetChar*(): CHAR;
VAR ch: CHAR;  res, bytesread: INTEGER;
BEGIN
  ch := 0X;
  res := sysread(Stdin, SYSTEM.ADR(ch), 1);
RETURN ch END GetChar;


(* -------------------------------------------------------------------------- *)
(* ----------------------------- Time functions ----------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Time*(): INTEGER;
(* In 100 nanosecond ticks since 1970-01-01 00:00 UTC *)
(* Sufficient to represent times between approx 27,250 BC and 31,000 AD. *)
VAR tv: RECORD- sec, nsec: INTEGER END;
BEGIN
  ASSERT(sysclockgettime(0, SYSTEM.ADR(tv)) = 0);
  ASSERT(tv.nsec < 40000000H);
RETURN tv.sec * 10000000 + tv.nsec DIV 100       (* Scale to 100ns ticks   *)
     - 10000000 * 24 * 60 * 60 * (30 * 365 + 7)  (* Bias to 2000-01-01 UTC *)
END Time;

PROCEDURE TimeAsClock*(time: INTEGER): INTEGER;
(* Returns UTC time in PO2013 format:               *)
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


PROCEDURE Clock*(): INTEGER;
(* Returns local time as                         *)
(* 38/year-2000,4/month,5/day,5/hour,6/min,6/sec *)
BEGIN RETURN TimeAsClock(Time()) END Clock;

PROCEDURE WriteDate*(clock: INTEGER);
VAR year: INTEGER;
BEGIN year := clock DIV 4000000H;
  IF year < 0 THEN wir(-year, 2, "0"); ws("BC-") ELSE wir(year, 4, "0"); wc("-") END;
  wir(clock DIV 400000H MOD 10H, 2, "0");  (*month*)    wc("-");
  wir(clock DIV 20000H  MOD 20H, 2, "0");  (*day*)
END WriteDate;

PROCEDURE WriteTime*(clock: INTEGER);
BEGIN
  wir(clock DIV 1000H MOD 20H, 2, "0");  (*hour*)     wc(":");
  wir(clock DIV 40H MOD 40H,   2, "0");  (*min*)      wc(":");
  wir(clock MOD 40H,           2, "0");  (*sec*)
END WriteTime;

PROCEDURE WriteClock*(clock: INTEGER);
BEGIN WriteDate(clock); wc(" "); WriteTime(clock)
END WriteClock;


(* -------------------------------------------------------------------------- *)
(* ------------- Platform independent low level file operations ------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE FileOld*(name: ARRAY OF CHAR): INTEGER;
VAR handle: INTEGER;
BEGIN
  handle := sysopen(SYSTEM.ADR(name), ORDWR, 0);
  IF handle < 0 THEN  (* Try again readonly *)
    handle := sysopen(SYSTEM.ADR(name), ORDONLY, 0)
  END;
  (* Consider mapping Linux error code to dtandard error code *)
RETURN handle END FileOld;


PROCEDURE FileNew(): INTEGER;  (* Create an anonymous file *)
VAR dir: ARRAY 2 OF CHAR;
BEGIN dir := ".";
RETURN sysopen(SYSTEM.ADR(dir), OTMPFILE + ORDWR, 1A0H)  (* 0640: WR- *)
END FileNew;


PROCEDURE FileRegister(handle: INTEGER; name: ARRAY OF CHAR);
CONST dir = "";
VAR rv: INTEGER;
BEGIN
  rv := sysunlink(SYSTEM.ADR(name));  (* Overwrite existing file if any *)
  ASSERT(syslinkat(handle, SYSTEM.ADR(dir), ATFDCWD, SYSTEM.ADR(name), ATEMPTYPATH) = 0)
END FileRegister;


PROCEDURE FileUnregister(name: ARRAY OF CHAR): INTEGER;
BEGIN RETURN sysunlink(SYSTEM.ADR(name))
END FileUnregister;


PROCEDURE FileMove*(source, dest: ARRAY OF CHAR): INTEGER;
BEGIN RETURN sysrename(SYSTEM.ADR(source), SYSTEM.ADR(dest))
END FileMove;


PROCEDURE FileTime*(hfile: INTEGER): INTEGER;
(* Returns Universal time in 100ns ticks since 1970/01/01 00:00:00 *)
VAR stat: RECORD-
    dev:      INTEGER;       (* ID of device containing file     *)
    ino:      INTEGER;       (* Inode number                     *)
    mode:     SYSTEM.CARD32; (* File type and mode               *)
    nlink:    SYSTEM.CARD32; (* Number of hard links             *)
    uid:      SYSTEM.CARD32; (* User ID of owner                 *)
    gid:      SYSTEM.CARD32; (* Group ID of owner                *)
    rdev:     INTEGER;       (* Device ID (if special file)      *)
    size:     INTEGER;       (* Total size, in bytes             *)
    blksize:  INTEGER;       (* Block size for filesystem I/O    *)
    blocks:   INTEGER;       (* Number of 512 B blocks allocated *)
    atimsec:  INTEGER;       (* Time of last access              *)
    atimnsec: INTEGER;       (* Time of last access              *)
    mtimsec:  INTEGER;       (* Time of last modification        *)
    mtimnsec: INTEGER;       (* Time of last modification        *)
    ctimsec:  INTEGER;       (* Time of last status change       *)
    ctimnsec: INTEGER;       (* Time of last status change       *)
  END;
BEGIN
  IF sysfstat(hfile, SYSTEM.ADR(stat)) < 0 THEN
    stat.mtimsec := 0;  stat.mtimnsec := 0;
  END;
RETURN stat.mtimsec * 10000000 + stat.mtimnsec DIV 100  (* Scale to 100ns ticks   *)
     - 10000000 * 24 * 60 * 60 * (30 * 365 + 7)         (* Bias to 2000-01-01 UTC *)
END FileTime;


(* -------------------------------------------------------------------------- *)
(* ----------------------- Virtual memory management ------------------------ *)
(* -------------------------------------------------------------------------- *)

PROCEDURE HeapInit*(reserve, commit: INTEGER): INTEGER;
VAR heaporg, res: INTEGER;
BEGIN
  heaporg := sysmmap(200000000H, reserve, PROTNONE, MAPPRIVATE + MAPFIXED + MAPANON, -1, 0);
  ASSERT(heaporg # 0);
  res := sysmprotect(heaporg, commit, PROTRWX);
  ASSERT(res >= 0);
RETURN heaporg END HeapInit;


PROCEDURE Allocate*(size: INTEGER; VAR p, alloc: INTEGER);
VAR adr, newcommitlen: INTEGER;
BEGIN
  ws("Host.Allocate(size "); wh(size);            wsn("H).");
  ws("  Preload.CoreAdr: "); wh(Preload.CoreAdr); wsn("H.");
  ws("  AllocPtr:    ");     wh(AllocPtr);        wsn("H.");
  ws("  CommitLen:   ");     wh(CommitLen);       wsn("H.");
  p     := 0;
  alloc := 0;
  size  := (size + 15) DIV 16 * 16;
  IF AllocPtr - Preload.CoreAdr + size < 80000000H THEN  (* Hard limit on reserved size 2GB due to relative addressing *)
    IF AllocPtr + size > Preload.CoreAdr + CommitLen THEN

      (* Round up to a multiple of 256K *)
      newcommitlen := (AllocPtr + size + 40000H - 1) DIV 40000H * 40000H - Preload.CoreAdr;
      ws("* CommitLen increasing from "); wh(CommitLen);
      ws("H to "); wh(newcommitlen); wsn("H *");

      adr := sysmprotect(Preload.CoreAdr+CommitLen, newcommitlen-CommitLen, PROTRWX);
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

PROCEDURE Init*;
VAR
  res:        INTEGER;
  reserveadr: INTEGER;
  reservelen: INTEGER;
BEGIN
  Newline := $0A 00$;
  wsn("Host starting.");

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

  Handlers[NewProc]                   := NewPointerHandler;
  Handlers[AssertionFailureProc]      := AssertionFailureHandler;
  Handlers[ArraySizeMismatchProc]     := ArraySizeMismatchHandler;
  Handlers[UnterminatedStringProc]    := UnterminatedStringHandler;
  Handlers[IndexOutOfRangeProc]       := IndexOutOfRangeHandler;
  Handlers[NilPointerDereferenceProc] := NilPointerDereferenceHandler;
  Handlers[TypeGuardFailureProc]      := TypeGuardFailureHandler;

  AllocPtr   := Preload.CoreAdr + Preload.CoreSize;
  CommitLen  := (Preload.CoreSize + 0FFFH) DIV 1000H * 1000H;
  reserveadr := Preload.CoreAdr + CommitLen;
  reservelen := 80000000H - CommitLen;
  res := sysmmap(reserveadr, reservelen, PROTNONE, MAPPRIVATE + MAPFIXED + MAPANON, -1, 0);
  ASSERT(res = reserveadr)
END Init;


PROCEDURE TestClock(time: INTEGER);
CONST day = 24 * 60 * 60 * 10000000;
VAR i, clock, year: INTEGER;
BEGIN
  clock := TimeAsClock(time);  i := 0;
  wn; ws("Testing starting from "); wh(time); ws("H - "); WriteClock(clock); ws(":");
  year := clock DIV 4000000H;
  WHILE clock DIV 4000000H < year + 1 DO
    IF (clock DIV 20000H MOD 20H = 1) THEN wn END;
    IF clock DIV 20000H MOD 20H >= 28 THEN WriteDate(clock);  ws("  ") END;
    INC(time, day);  clock := TimeAsClock(time);
    INC(i)
  END;
  wn; wn
END TestClock;

PROCEDURE #229 sysclockgetres(id, adr: INTEGER): INTEGER;

PROCEDURE TimeTest;
CONST
  day      = 24 * 60 * 60 * 10000000;
  year     = 365 * day;
  leapyear = 366 * day;
VAR
  time, clock, i: INTEGER;
  t1, t2, t3, tres: RECORD- sec, nsec: INTEGER END;
BEGIN
  ASSERT(sysclockgetres(0, SYSTEM.ADR(tres)) = 0);

  ws("Timer resolution "); wi(tres.sec);
  ws(" seconds and "); wi(tres.nsec); wsn(" nanoseconds.");

  WriteClock(TimeAsClock(Time()));  wn;


  FOR i := 1 TO 5 DO
    ASSERT(sysclockgettime(0, SYSTEM.ADR(t1))
         + sysclockgettime(0, SYSTEM.ADR(t2))
         + sysclockgettime(0, SYSTEM.ADR(t3)) = 0);
    ws(" gettime deltas: "); wi(t3.nsec - t2.nsec);
    ws(", "); wi(t2.nsec - t1.nsec); wsn(".")
  END;


  TestClock(0);                    (* 1970 *)
  (*
  TestClock(1 * year);             (* 1971 *)
  TestClock(2 * year);             (* 1972 *)
  TestClock(30 * year + 7 * day);  (* 2000 *)

  TestClock(-year);                (* 1969 *)
  TestClock(-2 * year - day);      (* 1968 *)

  TestClock(-(year *  100 + day * 24));  (* 1870AD *)
  TestClock(-(year *   70 + day * 17));  (* 1900AD *)
  TestClock(-(year *  170 + day * 41));  (* 1800AD *)
  TestClock(-(year *  270 + day * 65));  (* 1700AD *)
  TestClock(-(year *  369 + day * 89));  (* 1601AD *)
  TestClock(-(year *  370 + day * 90));  (* 1600AD *)
  TestClock(-(year * 1970 + day * 478)); (*    0AD *)
  TestClock(-(year * 1971 + day * 478)); (*    1BC *)
  TestClock(-(year * 2470 + day * 599)); (*  500BC *)
  TestClock(year   * 2530 + day * 614);  (* 4500AD *)
  *)

END TimeTest;




BEGIN Init;
  TimeTest;
  wsn("Host complete.");
  sysexit(0)
END Host.
