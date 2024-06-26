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

  (* Signals for x86-64 *)
  SIGHUP    = 1;
  SIGINT    = 2;
  SIGQUIT   = 3;
  SIGILL    = 4;
  SIGTRAP   = 5;
  SIGABRT   = 6;
  SIGBUS    = 7;
  SIGFPE    = 8;
  SIGKILL   = 9;
  SIGUSR1   = 10;
  SIGSEGV   = 11;
  SIGUSR2   = 12;
  SIGPIPE   = 13;
  SIGALRM   = 14;
  SIGTERM   = 15;
  SIGSTKFLT = 16;
  SIGCHLD   = 17;
  SIGCONT   = 18;
  SIGSTOP   = 19;
  SIGTSTP   = 20;
  SIGTTIN   = 21;
  SIGTTOU   = 22;
  SIGURG    = 23;
  SIGXCPU   = 24;
  SIGXFSZ   = 25;
  SIGVTALRM = 26;
  SIGPROF   = 27;
  SIGWINCH  = 28;
  SIGIO     = 29;
  SIGPOLL   = 29;
  SIGPWR    = 30;
  SIGSYS    = 31;
  NSIG      = 65;

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
  SIRUSR      = 100H;
  SIWUSR      = 80H;
  SIXUSR      = 40H;
  SIRGRP      = 20H;
  SIWGRP      = 10H;
  SIXGRP      = 8;
  SIROTH      = 4;
  SIWOTH      = 2;
  SIXOTH      = 1;

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

  StatDesc = RECORD-
    dev:      INTEGER;       (* ID of device containing file     *)
    ino:      INTEGER;       (* Inode number                     *)
    nlink:    SYSTEM.CARD32; (* Number of hard links             *)
    mode:     SYSTEM.CARD32; (* File type and mode               *)
    uid:      SYSTEM.CARD32; (* User ID of owner                 *)
    gid:      SYSTEM.CARD32; (* Group ID of owner                *)
    pad0:     SYSTEM.CARD32;
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
    unused:   ARRAY 3 OF INTEGER;
  END;
(*
struct stat {
  dev_t st_dev;
  ino_t st_ino;
  nlink_t st_nlink;

  mode_t st_mode;
  uid_t st_uid;
  gid_t st_gid;
  unsigned int    __pad0;
  dev_t st_rdev;
  off_t st_size;
  blksize_t st_blksize;
  blkcnt_t st_blocks;

  struct timespec st_atim;
  struct timespec st_mtim;
  struct timespec st_ctim;
  long __unused[3];
};
*)

  SignalAction = RECORD-  (* Corresponds to MUSL's struct k_sigaction *)
    action:   PROCEDURE/(signal, info, context: INTEGER);
    flags:    INTEGER;
    restorer: INTEGER;
    mask:     INTEGER;
  END;

  FpStackEnt = RECORD-
    significand: ARRAY 4 OF SYSTEM.CARD16;
    exponent:    SYSTEM.CARD16;
    padding:     ARRAY 3 OF SYSTEM.CARD16;
  END;

  XmmReg = RECORD-
    element: ARRAY 4 OF SYSTEM.CARD32;
  END;

  FpRegset = RECORD-  (* MUSL fpregset_t *)
    cwd, swd, ftw, fop: SYSTEM.CARD16;
    rip, rdp:           INTEGER;
    mxcsr, mxcsrmask:   SYSTEM.CARD32;
    st:                 ARRAY 8 OF FpStackEnt;
    xmm:                ARRAY 16 OF XmmReg;
    padding:            ARRAY 24 OF SYSTEM.CARD32;
  END;

  MachineContext = RECORD-  (* MUSL mcontext_t *)
    gpregs: ARRAY 23 OF INTEGER;
    fpregs: FpRegset;
  END;

  AltStack = RECORD-  (* MUSL stack_t *)
    sp:    INTEGER;
    flags: SYSTEM.CARD32;
    size:  INTEGER;
  END;

  UserContext = RECORD-  (* MUSL ucontext_t *)
    flags:   INTEGER;
    link:    POINTER- TO UserContext;
    altst:   AltStack;
    machine: MachineContext;
    sigmask: ARRAY 2 OF INTEGER;
    fpregs:  ARRAY 64 OF INTEGER;
  END;

  SignalInfo = RECORD-  (* MUSL siginfo_t selected for SIGILL/FPE/SEGV/BUS/TRAP *)
    signo:  SYSTEM.INT32;
    errno:  SYSTEM.INT32;
    code:   SYSTEM.INT32;
    adr:    INTEGER;    (* Only for SIGILL/FPE/SEGV/BUS/TRAP *)
  END;

VAR
  Preload*: PreLoadVars;

  (* Linker expects handlers to follow immediately after pre-loaded vars *)
  Handlers*: ARRAY HandlerCount OF PROCEDURE;

  Newline*:  ARRAY 3 OF CHAR;

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

  MonthLen:    ARRAY 11 OF INTEGER;
  Anonfnum:    INTEGER;


(* ----------------------- Linux syscall system APIs ------------------------ *)

PROCEDURE #0   sysread(handle, adr, len: INTEGER): INTEGER;
PROCEDURE #1   syswrite(handle, adr, len: INTEGER): INTEGER;
PROCEDURE #2   sysopen(name, flags, mode: INTEGER): INTEGER;
PROCEDURE #3   sysclose(handle: INTEGER): INTEGER;
PROCEDURE #4   sysstat(name, adr: INTEGER): INTEGER;
PROCEDURE #5   sysfstat(handle, adr: INTEGER): INTEGER;
PROCEDURE #8   syslseek(handle, offset, whence: INTEGER): INTEGER;
PROCEDURE #9   sysmmap(adr, len, prot, flags, file, offset: INTEGER): INTEGER;
PROCEDURE #10  sysmprotect(adr, len, prot: INTEGER): INTEGER;
PROCEDURE #13  sysrtsigaction(signum, act, oldact, nsig: INTEGER): INTEGER;
PROCEDURE #39  sysgetpid(): INTEGER;
PROCEDURE #60  sysexit(i: INTEGER);
PROCEDURE #74  sysfsync(handle: INTEGER): INTEGER;
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
BEGIN res := syswrite(Stdout, SYSTEM.ADR(s), Length(s));
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


(* ---------------------------- Signal handling ----------------------------- *)

PROCEDURE/ SignalHandler(signal, infoadr, contextadr: INTEGER);
CONST
  REGR8      = 0;
  REGR9      = 1;
  REGR10     = 2;
  REGR11     = 3;
  REGR12     = 4;
  REGR13     = 5;
  REGR14     = 6;
  REGR15     = 7;
  REGRDI     = 8;
  REGRSI     = 9;
  REGRBP     = 10;
  REGRBX     = 11;
  REGRDX     = 12;
  REGRAX     = 13;
  REGRCX     = 14;
  REGRSP     = 15;
  REGRIP     = 16;
  REGEFL     = 17;
  REGCSGSFS  = 18;
  REGERR     = 19;
  REGTRAPNO  = 20;
  REGOLDMASK = 21;
  REGCR2     = 22;

  (* REGERR bit numbers from linux/arch/x86/include/asm/trap_pf.h *)
  PFPROT  = 0;   (* 0: no page found  1: protection fault      *)
  PFWRITE = 1;   (* 0: read access    1: write access          *)
  PFUSER  = 2;   (* 0: kernel-mode access  1: user-mode access *)
  PFRSVD  = 3;   (* 1: use of reserved bit detected            *)
  PFINSTR = 4;   (* 1: fault was an instruction fetch          *)
  PFPK    = 5;   (* 1: protection keys block access            *)
  PFSHSTK = 6;   (* 1: shadow stack access fault               *)
  PFSGX   = 15;  (* 1: SGX MMU page-fault                      *)
  PFRMP   = 31;  (* 1: fault was due to RMP violation          *)

VAR
  context: POINTER- TO UserContext;
  info:    POINTER- TO SignalInfo;
  err: SET;
BEGIN
  IF signal    = SIGILL  THEN ws("Illegal instruction")
  ELSIF signal = SIGBUS  THEN ws("Bus fault")
  ELSIF signal = SIGSEGV THEN ws("Segmentation fault")
  ELSIF signal = SIGFPE  THEN ws("Floating point error")
  ELSE  wh(signal); wc("H")
  END;
  wsn(", context:");
  wsn("Signal info:");
  SYSTEM.PUT(SYSTEM.ADR(info), infoadr);
  DumpMem(0, infoadr, 0, 128);
  ws("  signo "); wh(info.signo); wsn("H.");
  ws("  errno "); wh(info.errno); wsn("H.");
  ws("  code  "); wh(info.code);  wsn("H.");
  ws("  adr   "); wh(info.adr);   wsn("H.");

  wsn("User context:");
  SYSTEM.PUT(SYSTEM.ADR(context), contextadr);
  (*DumpMem(0, contextadr, 0, SYSTEM.SIZE(UserContext));*)
  ws("  flags:                 "); wh(context.flags); wsn("H.");
  ws("  link:                  "); wh(SYSTEM.VAL(INTEGER, context.link)); wsn("H.");
  ws("  altst.sp               "); wh(context.altst.sp); wsn("H.");
  ws("  altst.flags            "); wh(context.altst.flags); wsn("H.");
  ws("  altst.size             "); wh(context.altst.size); wsn("H.");
  ws("  machine.gpregs[REGRIP] "); wh(context.machine.gpregs[REGRIP]); wsn("H.");
  ws("  machine.gpregs[REGRSP] "); wh(context.machine.gpregs[REGRSP]); wsn("H.");
  ws("  machine.gpregs[REGRAX] "); wh(context.machine.gpregs[REGRAX]); wsn("H.");
  ws("  machine.gpregs[REGRCX] "); wh(context.machine.gpregs[REGRCX]); wsn("H.");
  ws("  machine.gpregs[REGRDX] "); wh(context.machine.gpregs[REGRDX]); wsn("H.");
  ws("  machine.gpregs[REGRBX] "); wh(context.machine.gpregs[REGRBX]); wsn("H.");
  ws("  machine.gpregs[REGERR] "); wh(context.machine.gpregs[REGERR]); wsn("H.");
  (*DumpMem(0, SYSTEM.ADR(context.machine.gpregs), 0, 23*8);*)
  err := SYSTEM.VAL(SET, context.machine.gpregs[REGERR]);
  (* Decode REGERR using logic from linux/tree/arch/x86/mm/fault.c[544] in show_fault_oops() *)
  IF    PFINSTR IN err THEN wsn("Instruction fetch")
  ELSIF PFWRITE IN err THEN wsn("Write access")
  ELSE                      wsn("Read access")
  END;
  IF  ~(PFPROT IN err) THEN wsn("not-present page")
  ELSIF PFRSVD IN err  THEN wsn("reserved bit violation")
  ELSIF PFPK   IN err  THEN wsn("protection keys violation")
  ELSIF PFRMP  IN err  THEN wsn("RMP violation")
  ELSE                      wsn("permissions violation")
  END;
  sysexit(99)
END SignalHandler;


PROCEDURE SetSignalHandlers;
CONST
  SASIGINFO  = 4;
  SANODEFER  = 40000000H;
  SARESTORER = 4000000H;
VAR
  rv:  INTEGER;
  act: SignalAction;
  ap:  POINTER- TO SignalAction;
BEGIN
  act.action   := SignalHandler;
  act.mask     := 0;
  act.flags    := SASIGINFO + SANODEFER + SARESTORER;  (* MUSL always sets SA_RESTORER *)
  act.restorer := 0;  (* We never return to signal cause so don't need a restorer *)
  ASSERT(sysrtsigaction(SIGILL,  SYSTEM.ADR(act), 0, NSIG DIV 8) = 0);
  ASSERT(sysrtsigaction(SIGBUS,  SYSTEM.ADR(act), 0, NSIG DIV 8) = 0);
  ASSERT(sysrtsigaction(SIGSEGV, SYSTEM.ADR(act), 0, NSIG DIV 8) = 0);
  ASSERT(sysrtsigaction(SIGFPE,  SYSTEM.ADR(act), 0, NSIG DIV 8) = 0);
  (*rv := 0;  rv := rv DIV 0;*)
END SetSignalHandlers;

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
  (* ws("Host.FileOld("); ws(name); ws(") -> "); wh(handle); wsn("H."); *)
  (* Consider mapping Linux error code to some standard error code *)
  ws("Host.FileOld('"); ws(name); ws("') -> "); wh(handle); wsn("H.");
RETURN handle END FileOld;


PROCEDURE FileNew*(): INTEGER;  (* Create an anonymous file *)
VAR fn: ARRAY 40 OF CHAR;  h, rv: INTEGER;
BEGIN  (* Would like to use O_TMPFILE but doesn't work on wsl2. *)
  INC(Anonfnum);  IntToDecimal(Anonfnum, fn);  Append("anonymous", fn);
  h := sysopen(SYSTEM.ADR(fn), OCREAT+ORDWR, SIRUSR+SIWUSR+SIRGRP);
  (* ws("Host.FileNew:open() -> "); wh(h); wsn("H."); *)
  IF h >= 0 THEN
    rv := sysunlink(SYSTEM.ADR(fn));
    (* ws("Host.FileNew:unlink('"); ws(fn); ws("') -> "); wh(rv); wsn("H.")*)
  END;
  ws("Host.FileNew() -> "); wh(h); wsn("H.");
RETURN h END FileNew;


PROCEDURE FileFlush*(handle: INTEGER);
VAR rv: INTEGER;
BEGIN rv := sysfsync(handle) END FileFlush;


PROCEDURE FileClose*(VAR handle: INTEGER);
BEGIN ASSERT(sysclose(handle) # 0);  handle := -1 END FileClose;


PROCEDURE FileLength*(handle: INTEGER): INTEGER;
VAR stat: StatDesc;
BEGIN
  IF sysfstat(handle, SYSTEM.ADR(stat)) < 0 THEN stat.size := 0 END;
  ws("Host.FileLength("); wh(handle); ws("H) -> "); wh(stat.size); wsn("H.");
  (*DumpMem(2, SYSTEM.ADR(stat), 0, SYSTEM.SIZE(StatDesc))*)
RETURN stat.size END FileLength;


PROCEDURE FileSetPos*(handle, pos: INTEGER);
BEGIN ASSERT(syslseek(handle, pos, 0) = pos)
END FileSetPos;


PROCEDURE FileMove*(source, dest: ARRAY OF CHAR): INTEGER;
BEGIN RETURN sysrename(SYSTEM.ADR(source), SYSTEM.ADR(dest))
END FileMove;


PROCEDURE FileRegister*(handle: INTEGER; name: ARRAY OF CHAR);
CONST dir = "";
VAR rv: INTEGER;
BEGIN
  (*ws("Host.FileRegister(h "); wh(handle); ws("H, name "); ws(name); wsn(")");*)
  rv := sysunlink(SYSTEM.ADR(name));  (* Overwrite existing file if any *)
(*rv := syslinkat(handle, SYSTEM.ADR(dir), ATFDCWD, SYSTEM.ADR(name), ATEMPTYPATH);*)
  rv := syslinkat(handle, 0, ATFDCWD, SYSTEM.ADR(name), ATEMPTYPATH);
  IF rv # 0 THEN
    ws("syslinkat(handle "); wh(handle);
    ws("H, '', ATFDCWD, '"); ws(name);
    ws("', ATEMPTYPATH) -> -"); wi(-rv); wsn(".")
  END;
  ASSERT(rv = 0);
END FileRegister;


PROCEDURE FileUnregister*(handle: INTEGER; name: ARRAY OF CHAR);
BEGIN
  (*ws("Host.FileUnregister(h "); wh(handle); ws("H, name "); ws(name); wsn(")");*)
  ASSERT(sysunlink(SYSTEM.ADR(name)) = 0)
END FileUnregister;


PROCEDURE FileTime*(hfile: INTEGER): INTEGER;
(* Returns Universal time in 100ns ticks since 1970/01/01 00:00:00 *)
VAR stat: StatDesc;
BEGIN
  IF sysfstat(hfile, SYSTEM.ADR(stat)) < 0 THEN
    stat.mtimsec := 0;  stat.mtimnsec := 0;
  END;
RETURN stat.mtimsec * 10000000 + stat.mtimnsec DIV 100  (* Scale to 100ns ticks   *)
     - 10000000 * 24 * 60 * 60 * (30 * 365 + 7)         (* Bias to 2000-01-01 UTC *)
END FileTime;


PROCEDURE FileRead*(h, adr, len: INTEGER): INTEGER;
VAR rv: INTEGER;
BEGIN ASSERT(len > 0);
  rv := sysread(h, adr, len);
  (*
  ws("Host.FileRead(h "); wh(h); ws("H, adr "); wh(adr);
  ws("H, len "); wh(len); ws("H, -> "); wh(rv); wsn("H).");
  *)
RETURN rv END FileRead;


PROCEDURE FileWrite*(h, adr, len: INTEGER);
VAR byteswritten: INTEGER;
BEGIN
  IF len > 0 THEN
    byteswritten := syswrite(h, adr, len);
    (*
    ws("Host.FileWrite(h "); wh(h);
    ws("H, adr "); wh(adr);
    ws("H, len "); wh(len);
    ws("H) -> "); wh(byteswritten); wsn("H.");
    (*DumpMem(2, adr, adr, len);*)
    *)
    IF len # byteswritten THEN
      Trap(8, "syswrite failed to write all bytes.");
    END
  END
END FileWrite;


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
VAR rv, newcommitlen: INTEGER;
BEGIN
  (*
  ws("Host.Allocate(size "); wh(size);            wsn("H).");
  ws("  Preload.CoreAdr: "); wh(Preload.CoreAdr); wsn("H.");
  ws("  AllocPtr:    ");     wh(AllocPtr);        wsn("H.");
  ws("  CommitLen:   ");     wh(CommitLen);       wsn("H.");
  *)
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
      rv := sysmprotect(Preload.CoreAdr+CommitLen, newcommitlen-CommitLen, PROTRWX);

      ASSERT(rv = 0);
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


(* -------------------------- Command line parsing -------------------------- *)

PROCEDURE ParseCommandLine(pargs: INTEGER);
VAR
  i, j, k:  INTEGER;
  argc:     INTEGER;
  modstart: INTEGER;
  modlimit: INTEGER;
  p:        INTEGER;
  ch:       CHAR;
  quote:    BOOLEAN;
BEGIN
  SYSTEM.GET(pargs, argc);  INC(pargs, 8);

  (* Parse program name *)
  SYSTEM.GET(pargs, p);  INC(pargs, 8);
  i := 0;  modstart := 0;  modlimit := -1;
  SYSTEM.GET(p, ch);  INC(p);
  WHILE ch # 0X DO
    IF    ch = "." THEN modlimit := i
    ELSIF ch = "/" THEN modlimit := -1;  modstart := i+1
    END;
    CommandLine[i] := ch;  INC(i);
    SYSTEM.GET(p, ch);  INC(p);
  END;

  ArgStart := i;
  IF modlimit < 0 THEN modlimit := i END;

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
  END;

  (* Add arguments *)
  SYSTEM.GET(pargs, p);  INC(pargs, 8);
  WHILE p # 0 DO
    CommandLine[i] := " ";  INC(i);

    (* Check for spaces in argument *)
    j := p;
    SYSTEM.GET(j, ch);  INC(j);
    WHILE (ch # 0X) & (ch # " ") DO SYSTEM.GET(j, ch);  INC(j) END;
    quote := ch = " ";
    IF quote THEN CommandLine[i] := 22X;  INC(i) END;
    SYSTEM.GET(p, ch);  INC(p);
    WHILE ch # 0X DO
      CommandLine[i] := ch;  INC(i);
      SYSTEM.GET(p, ch);  INC(p)
    END;
    IF quote THEN CommandLine[i] := 22X;  INC(i) END;

    SYSTEM.GET(pargs, p);  INC(pargs, 8)
  END;

  CommandLine[i] := 0X;
  IF CommandLine[ArgStart] = " " THEN INC(ArgStart) END;

  (*
  ws("CommandLine: '"); ws(CommandLine); wsn("'.");
  ws("CmdModule:   '"); ws(CmdModule);   wsn("'.");
  ws("CmdCommand:  '"); ws(CmdCommand);  wsn("'.");
  ws("ArgStart:    ");  wi(ArgStart);    wsn(".");
  *)
END ParseCommandLine;


PROCEDURE Init*(stackorg: INTEGER);
VAR
  res:        INTEGER;
  reserveadr: INTEGER;
  reservelen: INTEGER;
BEGIN
  Newline := $0A 00$;
  (*wsn("Linux host starting.");*)

  (* Length of each month based on year starting 1st March *)
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

  SetSignalHandlers;

  AllocPtr   := Preload.CoreAdr + Preload.CoreSize;
  CommitLen  := (Preload.CoreSize + 0FFFH) DIV 1000H * 1000H;
  reserveadr := Preload.CoreAdr + CommitLen;
  reservelen := 80000000H - CommitLen;
  res := sysmmap(reserveadr, reservelen, PROTNONE, MAPPRIVATE + MAPFIXED + MAPANON, -1, 0);
  ASSERT(res = reserveadr);

  Anonfnum := Time();

  ParseCommandLine(stackorg)
END Init;


END Host.




(* -------------------------------- obsolete -------------------------------- *)

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




BEGIN Init(SYSTEM.REG(4));
  TimeTest;
  wsn("Host complete.");
  sysexit(0)