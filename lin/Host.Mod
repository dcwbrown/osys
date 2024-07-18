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
  ORDONLY     = 00000000H;
  ORDWR       = 00000002H;
  OCREAT      = 00000040H;
  OEXCL       = 00000080H;
  ODIRECTORY  = 00010000H;
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

  FileName* = ARRAY MaxPath OF CHAR;

  HostFile* = RECORD
    name*:       FileName;
    registered*: BOOLEAN;
    fd:          INTEGER;
    unregname:   FileName;
  END;

  FileInfo* = RECORD
    name*:   FileName;
    date*:   INTEGER;
    length*: INTEGER;
  END;

  FileEntryHandler* = PROCEDURE(info: FileInfo; VAR continue: BOOLEAN);

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

  DirEntry64 = RECORD-
    ino:    INTEGER;
    off:    INTEGER;
    reclen: SYSTEM.CARD16;
    type:   BYTE;
    name:   FileName;
  END;

  Statistic = RECORD time, count, lastentry: INTEGER END;


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

  DeleteUnregisteredFilesAtExit: PROCEDURE;

  MonthLen:    ARRAY 11 OF INTEGER;
  Anonfnum:    INTEGER;

  OldStat:        Statistic;
  NewStat:        Statistic;
  FlushStat:      Statistic;
  CloseStat:      Statistic;
  SetStat:        Statistic;
  MoveStat:       Statistic;
  RegisterStat:   Statistic;
  UnregisterStat: Statistic;
  UnlinkStat:     Statistic;
  ReadStat:       Statistic;  ReadBytes:    INTEGER;
  WriteStat:      Statistic;  WrittenBytes: INTEGER;
  StatReport:     PROCEDURE;
  StartTime:      INTEGER;
  AtNL:           BOOLEAN;
  TimeStamp:      PROCEDURE;


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
PROCEDURE #217 sysgetdents64(fd, adr, len: INTEGER): INTEGER;
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
  IF AtNL & (TimeStamp # NIL) THEN AtNL := FALSE; TimeStamp END;
  res := syswrite(Stdout, SYSTEM.ADR(s), Length(s));
END Log;

PROCEDURE wc*(c: CHAR); BEGIN Log(c) END wc;

PROCEDURE wn*; BEGIN Log(Newline); AtNL := TRUE END wn;

PROCEDURE ws*(s: ARRAY OF CHAR); BEGIN Log(s) END ws;

PROCEDURE wsr*(s: ARRAY OF CHAR; w: INTEGER; c: CHAR);  (* Right justified with leading c *)
BEGIN DEC(w, Length(s));  WHILE w > 0 DO wc(c); DEC(w) END; Log(s) END wsr;

PROCEDURE wsn*(s: ARRAY OF CHAR); BEGIN Log(s); Log(Newline); AtNL := TRUE END wsn;

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
BEGIN
  IF DeleteUnregisteredFilesAtExit # NIL THEN DeleteUnregisteredFilesAtExit END;
  IF StatReport # NIL THEN StatReport END;
  sysexit(ExitCode)
END Exit;


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
  ExitCode := 99; Exit
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
  ExitCode := 99; Exit
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

PROCEDURE WriteTimestamp;
VAR now, clock, ms: INTEGER;
BEGIN
  now   := Time();
  clock := TimeAsClock(now);
  ms    := now DIV 10000 MOD 1000;
  WriteTime(clock);  wc(".");  wir(ms, 3, "0"); ws(". ");
END WriteTimestamp;


PROCEDURE InitStatistic(VAR s: Statistic);
BEGIN ZeroFill(s) END InitStatistic;

PROCEDURE Enter(VAR s: Statistic);
BEGIN s.lastentry := Time() END Enter;

PROCEDURE Leave(VAR s: Statistic);
BEGIN INC(s.time, Time() - s.lastentry);  INC(s.count) END Leave;

PROCEDURE WriteStatistic(s: Statistic);
BEGIN
  wi(s.time DIV 10000);  ws("ms: count ");  wi(s.count);
  IF s.count > 0 THEN
    ws(", each ");  wi((s.time DIV s.count) DIV 10000);  ws("ms")
  END
END WriteStatistic;

PROCEDURE ReportStats;
BEGIN
  wsn("Exit statistics:");

  ws("  Old:        ");  WriteStatistic(OldStat);        wsn(".");
  ws("  New:        ");  WriteStatistic(NewStat);        wsn(".");
  ws("  Flush:      ");  WriteStatistic(FlushStat);      wsn(".");
  ws("  Close:      ");  WriteStatistic(CloseStat);      wsn(".");
  ws("  Set:        ");  WriteStatistic(SetStat);        wsn(".");
  ws("  Move:       ");  WriteStatistic(MoveStat);       wsn(".");
  ws("  Register:   ");  WriteStatistic(RegisterStat);   wsn(".");
  ws("  Unregister: ");  WriteStatistic(UnregisterStat); wsn(".");
  ws("  Unlink:     ");  WriteStatistic(UnlinkStat);     wsn(".");

  ws("  Read:       ");  WriteStatistic(ReadStat);
  ws(", bytes "); wi(ReadBytes);
  IF ReadBytes > 0 THEN ws(", each "); wi(ReadBytes DIV ReadStat.count) END;
  wsn(".");

  ws("  Write:      ");  WriteStatistic(WriteStat);
  ws(", bytes "); wi(WrittenBytes);
  IF WrittenBytes > 0 THEN ws(", each "); wi(WrittenBytes DIV WriteStat.count) END;
  wsn(".");
  ws("  Total:      ");
  wi((OldStat.time    + NewStat.time  + FlushStat.time    + CloseStat.time
    + SetStat.time    + MoveStat.time + RegisterStat.time + UnregisterStat.time
    + UnlinkStat.time + ReadStat.time + WriteStat.time) DIV 10000); wsn("ms.");
  ws("Run time ");  wi((Time() - StartTime) DIV 10000); wsn("ms.");
END ReportStats;


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
VAR success: BOOLEAN;
BEGIN
  Enter(OldStat);
  file.fd := sysopen(SYSTEM.ADR(file.name), ORDWR, 0);
  IF file.fd < 0 THEN  (* Try again readonly *)
    file.fd := sysopen(SYSTEM.ADR(file.name), ORDONLY, 0)
  END;
  success := file.fd >= 0;
  IF success THEN
    MakeUnregName(file);
    file.registered := TRUE;
  END;
  Leave(OldStat);
RETURN success END FileOld;


PROCEDURE FileNew*(VAR file: HostFile);
VAR retries: INTEGER;
BEGIN
  Enter(NewStat);
  file.registered := FALSE;  retries := 0;
  REPEAT
    MakeUnregName(file);
    file.fd := sysopen(SYSTEM.ADR(file.unregname), OCREAT+OEXCL+ORDWR, SIRUSR+SIWUSR+SIRGRP);
    INC(retries);
  UNTIL (file.fd >= 0) OR (file.fd # -17 (* EEXIST *)) OR (retries > 100);
  IF retries > 100 THEN Trap(-1, "Couldn't find unused temporary file name") END;
  IF file.fd < 0 THEN Trap(-1, "Couldn't create temporary file") END;
  Leave(NewStat);
END FileNew;


PROCEDURE FileFlush*(file: HostFile);
VAR rv: INTEGER;
BEGIN Enter(FlushStat); rv := sysfsync(file.fd); Leave(FlushStat); END FileFlush;


PROCEDURE FileClose*(VAR file: HostFile);
VAR rv: INTEGER;
BEGIN
  IF file.fd >= 0 THEN
    Enter(CloseStat);
    ASSERT(sysclose(file.fd) = 0); file.fd := -1;
    Leave(CloseStat);
    IF ~file.registered THEN
      Enter(UnlinkStat);
      rv := sysunlink(SYSTEM.ADR(file.unregname));
      Leave(UnlinkStat);
    END;
    Leave(UnlinkStat);
  END
END FileClose;


PROCEDURE FileLength*(file: HostFile): INTEGER;
VAR stat: StatDesc;
BEGIN
  IF sysfstat(file.fd, SYSTEM.ADR(stat)) < 0 THEN stat.size := 0 END;
RETURN stat.size END FileLength;


PROCEDURE FileSetPos*(file: HostFile; pos: INTEGER);
BEGIN Enter(SetStat); ASSERT(syslseek(file.fd, pos, 0) = pos); Leave(SetStat);
END FileSetPos;


PROCEDURE FileMove*(source, dest: ARRAY OF CHAR): INTEGER;
VAR res: INTEGER;
BEGIN
  Enter(MoveStat);
  res := sysrename(SYSTEM.ADR(source), SYSTEM.ADR(dest));
  Leave(MoveStat);
RETURN res END FileMove;


PROCEDURE FileRegister*(VAR file: HostFile);
VAR rv: INTEGER;
BEGIN
  Enter(RegisterStat);
  ASSERT(~file.registered);
  rv := sysunlink(SYSTEM.ADR(file.name));  (* Overwrite existing file if any *)
  rv := sysrename(SYSTEM.ADR(file.unregname), SYSTEM.ADR(file.name));
  IF rv # 0 THEN
    ws("sysrename('"); ws(file.unregname);
    ws("', '"); ws(file.name);
    ws("' -> -"); wi(-rv); wsn(".")
  END;
  ASSERT(rv = 0);
  file.registered := TRUE;
  Leave(RegisterStat);
END FileRegister;


PROCEDURE FileUnregister*(VAR file: HostFile);
VAR rv: INTEGER;
BEGIN
  Enter(UnregisterStat);
  ASSERT(file.registered);
  (*ws("Host.FileUnregister(h "); wh(handle); ws("H, name "); ws(name); wsn(")");*)
  rv := sysrename(SYSTEM.ADR(file.name), SYSTEM.ADR(file.unregname));
  IF rv # 0 THEN
    ws("sysrename('"); ws(file.name);
    ws("', '"); ws(file.unregname);
    ws("' -> -"); wi(-rv); wsn(".")
  END;
  ASSERT(rv = 0);
  file.registered := FALSE;
  Leave(UnregisterStat);
END FileUnregister;


PROCEDURE FileTime*(file: HostFile): INTEGER;
(* Returns Universal time in 100ns ticks since 1970/01/01 00:00:00 *)
VAR stat: StatDesc;
BEGIN
  IF sysfstat(file.fd, SYSTEM.ADR(stat)) < 0 THEN
    stat.mtimsec := 0;  stat.mtimnsec := 0;
  END;
RETURN stat.mtimsec * 10000000 + stat.mtimnsec DIV 100  (* Scale to 100ns ticks   *)
     - 10000000 * 24 * 60 * 60 * (30 * 365 + 7)         (* Bias to 2000-01-01 UTC *)
END FileTime;


PROCEDURE FileRead*(file: HostFile; adr, len: INTEGER): INTEGER;
VAR res: INTEGER;
BEGIN ASSERT(len > 0);
  Enter(ReadStat);
  res := sysread(file.fd, adr, len);
  Leave(ReadStat);  INC(ReadBytes, len);
RETURN res END FileRead;


PROCEDURE FileWrite*(file: HostFile; adr, len: INTEGER);
VAR byteswritten: INTEGER;
BEGIN
  IF len > 0 THEN
    Enter(WriteStat);
    byteswritten := syswrite(file.fd, adr, len);
    Leave(WriteStat);  INC(WrittenBytes, byteswritten);
    IF len # byteswritten THEN
      Trap(8, "syswrite failed to write all bytes.");
    END
  END
END FileWrite;

PROCEDURE SetDeleteUnregisteredFilesAtExit*(delete: PROCEDURE);
BEGIN DeleteUnregisteredFilesAtExit := delete END SetDeleteUnregisteredFilesAtExit;


PROCEDURE IsSelfOrParent(dir: FileName): BOOLEAN;
(* TRUE iff /^(.*\/|)\.\.?$/ - if ., .. or path ending in . or .. *)
VAR i, j: INTEGER;  result: BOOLEAN;
BEGIN
  result := FALSE;
  i := 0;  j := 0;
  WHILE dir[i] # 0X DO IF dir[i] = "/" THEN j := i + 1 END; INC(i) END;
  IF dir[j] = "." THEN
    INC(j);
    IF (dir[j] = 0X) OR (dir[j] = ".") & (dir[j+1] = 0X) THEN
      result := TRUE
    END
  END
RETURN result END IsSelfOrParent;


PROCEDURE FileEnum(name: FileName; proc: FileEntryHandler; VAR cont: BOOLEAN);
CONST DTDIR = 4;  DTREG = 8;
VAR
  stat:    StatDesc;
  search:  FileName;
  fd:      INTEGER;
  buf:     ARRAY 1024 OF BYTE;
  pos:     INTEGER;
  p:       POINTER- TO DirEntry64;
  i, len:  INTEGER;
  inf:     FileInfo;
  bytesrd: INTEGER;
BEGIN
  IF ~IsSelfOrParent(name) THEN
    len := 0; WHILE name[len] # 0X DO search[len] := name[len]; INC(len) END;
    IF (len > 0) & (search[len-1] # "/") THEN search[len] := "/"; INC(len) END;
    i := len;
    search[i] := ".";  search[i+1] := 0X;
    fd := sysopen(SYSTEM.ADR(search), ORDONLY + ODIRECTORY, 0);
    IF fd >= 0 THEN
      cont := TRUE;
      search[len] := 0X;
      bytesrd := sysgetdents64(fd, SYSTEM.ADR(buf), LEN(buf));
      WHILE cont & (bytesrd > 0) DO
        pos := 0;
        WHILE cont & (pos < bytesrd) DO
          SYSTEM.PUT(SYSTEM.ADR(p), SYSTEM.ADR(buf[pos]));
          inf.name := search;  Append(p.name, inf.name);
          IF p.type = DTREG THEN     (* Regular file *)
            inf.date   := 0;
            inf.length := 0;
            IF sysstat(SYSTEM.ADR(inf.name), SYSTEM.ADR(stat)) = 0 THEN
              inf.length := stat.size;
              inf.date := TimeAsClock(stat.mtimsec * 10000000 + stat.mtimnsec DIV 100  (* Scale to 100ns ticks   *)
                                    - 10000000 * 24 * 60 * 60 * (30 * 365 + 7))        (* Bias to 2000-01-01 UTC *)
            END;
            proc(inf, cont)
          ELSIF p.type = DTDIR THEN  (* subdirectory *)
            FileEnum(inf.name, proc, cont)
          END;
          INC(pos, p.reclen)
        END;
        bytesrd := sysgetdents64(fd, SYSTEM.ADR(buf), LEN(buf));
      END;
      ASSERT(sysclose(fd) = 0)
    END
  END
END FileEnum;

PROCEDURE FileEnumerate*(prefix: ARRAY OF CHAR; proc: FileEntryHandler);
VAR cont: BOOLEAN; fn: FileName;
BEGIN
  fn := prefix;
  FileEnum(fn, proc, cont)
END FileEnumerate;


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
  AtNL := TRUE;
  TimeStamp := WriteTimestamp;
  wsn("Host starting.");
  StartTime := Time();

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

  InitStatistic(OldStat);
  InitStatistic(NewStat);
  InitStatistic(FlushStat);
  InitStatistic(CloseStat);
  InitStatistic(SetStat);
  InitStatistic(MoveStat);
  InitStatistic(RegisterStat);
  InitStatistic(UnregisterStat);
  InitStatistic(UnlinkStat);
  InitStatistic(ReadStat);   ReadBytes    := 0;
  InitStatistic(WriteStat);  WrittenBytes := 0;
  StatReport := ReportStats;

  ParseCommandLine(stackorg)
END Init;


END Host.
