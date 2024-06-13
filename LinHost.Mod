MODULE LinHost;  IMPORT SYSTEM;

CONST
  (* Host definition *)
  Win*  = 1;
  Lin*  = 2;
  Host* = Lin;

  Stdin*  = 0;
  Stdout* = 1;
  Stderr* = 2;

TYPE
  PreLoadVars* = RECORD-
    Exeadr*:      INTEGER;  (* Unused on Linux                         *)
    dummy2:       INTEGER;
    CoreAdr*:     INTEGER;  (* Oberon core address, usually 100000000H *)
    CoreSize*:    INTEGER;  (* End of core allocation                  *)
    MadrPreload*: INTEGER;  (* Start of preload section                *)
    MadrIcon*:    INTEGER;
    IconSize*:    INTEGER;
    dummy7:       INTEGER;
  END;

VAR
  Preload*: PreLoadVars;
  crlf*:    ARRAY 3 OF CHAR;

  AllocPtr*:    INTEGER;   (* Start of remaining free module space *)
  CommitLen*:   INTEGER;   (* Committed module space memory *)


  (*
  (* Partially parsed command line *)
  CommandLine*: ARRAY 1024 OF CHAR;
  CmdModule*:   ARRAY 32 OF CHAR;
  CmdCommand*:  ARRAY 32 OF CHAR;
  ArgStart*:    INTEGER;

  ExitCode:    INTEGER;
  TrapDepth:   INTEGER;
  TrapHandler: PROCEDURE(adr: INTEGER; desc: ARRAY OF CHAR);
  *)


(* ----------------------- Linux syscall system APIs ------------------------ *)

PROCEDURE #1  syswrite(handle, address, length: INTEGER): INTEGER;
PROCEDURE #60 sysexit(i: INTEGER);



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

PROCEDURE wn*; BEGIN Log(crlf) END wn;

PROCEDURE ws*(s: ARRAY OF CHAR); BEGIN Log(s) END ws;

PROCEDURE wsz*(s: ARRAY OF CHAR; w: INTEGER);  (* Right justified with leading zeroes *)
BEGIN DEC(w, Length(s));  WHILE w > 0 DO wc("0"); DEC(w) END; Log(s) END wsz;

PROCEDURE wsn*(s: ARRAY OF CHAR); BEGIN Log(s); Log(crlf) END wsn;

PROCEDURE wh*(n: INTEGER);
VAR hex: ARRAY 32 OF CHAR;
BEGIN IntToHex(n, hex); Log(hex) END wh;

PROCEDURE whz*(n, w: INTEGER);  (* Right justified with leading zeroes *)
VAR hex: ARRAY 32 OF CHAR;
BEGIN IntToHex(n, hex); wsz(hex, w) END whz;

PROCEDURE wi*(n: INTEGER);
VAR dec: ARRAY 32 OF CHAR;
BEGIN IntToDecimal(n, dec); Log(dec) END wi;

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
(* --------------------------- Trivial Stdin read --------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE #0 sysread(file, adr, len: INTEGER): INTEGER;

PROCEDURE GetChar*(): CHAR;
VAR ch: CHAR;  res, bytesread: INTEGER;
BEGIN
  ch := 0X;
  res := sysread(Stdin, SYSTEM.ADR(ch), 1);
RETURN ch END GetChar;


(* -------------------------------------------------------------------------- *)
(* ----------------------------- Time functions ----------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE #228 sysclockgettime(id, adr: INTEGER): INTEGER;

PROCEDURE Time*(): INTEGER;  (* In 100 nanosecond ticks since 1970-01-01 00:00 UTC *)
VAR
  tv: RECORD- sec, nsec: INTEGER(*SYSTEM.CARD32*) END;
  res: INTEGER;
BEGIN res := sysclockgettime(0, SYSTEM.ADR(tv));
RETURN tv.sec * 10000000 + tv.nsec DIV 100 END Time;

(*

TODO - time to yy/mm/dd/hh/mm/ss
TODO - time zone
-- See musl implementations.

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



(* -------------------------------------------------------------------------- *)
(* ----------------------- Virtual memory management ------------------------ *)
(* -------------------------------------------------------------------------- *)

(*
PROCEDURE Allocate*(size: INTEGER; VAR p, alloc: INTEGER);
VAR adr, newcommitlen: INTEGER;
BEGIN
  ws("LinHost.Allocate(size "); wh(size);        wsn("H).");
  ws("  ModuleSpace: ");        wh(ModuleSpace); wsn("H.");
  ws("  AllocPtr:    ");        wh(AllocPtr);    wsn("H.");
  ws("  CommitLen:   ");        wh(CommitLen);   wsn("H.");
  p     := 0;
  alloc := 0;
  size  := (size + 15) DIV 16 * 16;
  IF AllocPtr - ModuleSpace + size < 80000000H THEN  (* Hard limit on reserved size 2GB due to relative addressing *)
    IF AllocPtr + size > ModuleSpace + CommitLen THEN

      (* Round up to a multiple of 256K *)
      newcommitlen := (AllocPtr + size + 40000H - 1) DIV 40000H * 40000H - ModuleSpace;
      ws("* CommitLen increasing from "); wh(CommitLen);
      ws("H to "); wh(newcommitlen); wsn("H *");

      (*
      ws("Calling VirtualAlloc(addr "); wh(ModuleSpace+CommitLen);
      ws("H, size "); wh(newcommitlen-CommitLen);
      wsn("H, MEMCOMMIT, PAGEEXECUTEREADWRITE).");
      *)
      adr := VirtualAlloc(ModuleSpace+CommitLen, newcommitlen-CommitLen, MEMCOMMIT, PAGEEXECUTEREADWRITE);
      IF adr = 0 THEN AssertWinError(GetLastError()) END;


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
*)


(* -------------------------------------------------------------------------- *)

PROCEDURE #9  sysmmap(adr, len, prot, flags, file, offset: INTEGER): INTEGER;
PROCEDURE #10 sysmprotect(adr, len, prot: INTEGER): INTEGER;
PROCEDURE #39 sysgetpid(): INTEGER;

PROCEDURE Init*;
CONST
  PROTNONE   = 0;
  PROTRWX    = 7;
  MAPPRIVATE = 2;
  MAPFIXED   = 10H;
  MAPANON    = 20H;
VAR
  res:        INTEGER;
  reserveadr: INTEGER;
  reservelen: INTEGER;
  ch:         CHAR;
  rsp:        INTEGER;
  i, v:       INTEGER;
BEGIN
  crlf := $0A 00$;
  wsn("LinHost starting.");
  ws("Time of day: "); wi(Time()); wsn(".");

  ws("rsp               "); wh(SYSTEM.REG(4));     wsn("H.");
  ws("Preload.CoreAdr   "); wh(Preload.CoreAdr);   wsn("H.");
  ws("Preload.CoreSize  "); wh(Preload.CoreSize);  wsn("H.");

  AllocPtr  := Preload.CoreAdr + Preload.CoreSize;
  CommitLen := (Preload.CoreSize + 0FFFH) DIV 1000H * 1000H;

  ws("AllocPtr   "); wh(AllocPtr);  wsn("H.");
  ws("CommitLen  "); wh(CommitLen); wsn("H.");

  (* Reserve memory beyond committed up to 2GB *)
  reserveadr := Preload.CoreAdr + CommitLen;
  reservelen := 80000000H - CommitLen;

  ws("reserveadr "); wh(reserveadr); wsn("H.");
  ws("reservelen "); wh(reservelen); wsn("H.");

  wsn("Memory after AllocPtr:");
  DumpMem(2, AllocPtr, AllocPtr, 256);
  i := AllocPtr;
  REPEAT SYSTEM.GET(i, v);  INC(i, 8) UNTIL (v # 0) OR (i>= 100002000H);
  IF v # 0 THEN  DEC(i, 8);
    ws("Found non-zero memory at ");  wh(i);  wsn("H:");
    DumpMem(2, i, i, 256);
  END;

  ws("Process "); wi(sysgetpid()); ws(" before sysmmap call."); ch := GetChar();

  ws("Calling sysmmap(addr "); wh(reserveadr);
  ws("H, size "); wh(reservelen);
  wsn("H, PROTNONE, MAPPRIVATE + MAPFIXED + MAPANON, -1, 0).");

  res := sysmmap(reserveadr, reservelen, PROTNONE, MAPPRIVATE + MAPFIXED + MAPANON, -1, 0);
  IF res # reserveadr THEN
    IF res < 0 THEN
      ws("-res "); wi(-res); wsn(".");
      wsn("Couldn't reserve Oberon memory.")
    ELSE
      ws("mmap reserved memory at "); wh(res); wsn("H.")
    END
  END;

  ws("Process "); wi(sysgetpid()); ws(" after sysmmap call."); ch := GetChar();


  (* Now try updateing protection of first 10000H after the loaded code to rwx *)
  res := sysmprotect(100010000H, 10000H, PROTRWX);
  IF res < 0 THEN
    ws("-res "); wi(-res); wsn(".");
    wsn("Couldn't commit Oberon memory.")
  END;
  ws("Process "); wi(sysgetpid()); ws(" after first sysmprotect call."); ch := GetChar();


  (* Add another 10000H *)
  res := sysmprotect(100020000H, 10000H, PROTRWX);
  IF res < 0 THEN
    ws("-res "); wi(-res); wsn(".");
    wsn("Couldn't commit Oberon memory.")
  END;
  ws("Process "); wi(sysgetpid()); ws(" after second sysmprotect call."); ch := GetChar();


  wsn("LinHost complete.");

  sysexit(0)
END Init;




BEGIN Init
END LinHost.
