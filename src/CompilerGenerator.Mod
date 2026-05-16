MODULE CompilerGenerator; (* N.Wirth, 16.4.2016 / 4.4.2017 / 31.5.2019  Oberon compiler; code generator for RISC*)

IMPORT SYSTEM, Files, CS := CompilerScanner, CB := CompilerBase;

(*Code generator for Oberon compiler for RISC processor.
   Procedural interface to Parser OSAP; result in array "code".
   Procedure Close writes code-files*)

CONST
  maxOps  = 200;    (*max intermediate operations per procedure*)
  maxStrx = 2400;   (*max string bytes*)
  maxTD   = 160;    (*max number of type descriptors*)
  Reg = 10; RegI = 11; Cond = 12;  (*internal item modes*)

  (*conditions*)
  EQ = 0;
  NE = 1;  (* CS.neq - CS.eql *)
  LT = 2;  (* CS.lss - CS.eql *)
  LE = 3;  (* CS.leq - CS.eql *)
  GT = 4;  (* CS.gtr - CS.eql *)
  GE = 5;  (* CS.geq - CS.eql *)
  PL = 6;  (* CS.plus        *)
  MI = 7;  (* CS.minus       *)


TYPE
  Item* = RECORD
    mode*: INTEGER;
    type*: CB.Type;
    a*:    INTEGER;
    b*:    INTEGER;
    r:     INTEGER;
    rdo*:  BOOLEAN  (*read only*)
  END;

  (*item forms and meaning of fields:
  mode    r      a       b
  --------------------------------
  Const   -      value   (proc adr)  (immediate value)
  Var     base   off     -           (direct adr)
  Par     -      off0    off1        (indirect adr)
  Reg     regno
  RegI    regno  off     -
  Cond    cond   Fchain  Tchain      (cond = op-CS.eq) *)

  Operation* = RECORD
    op: ARRAY 10 OF CHAR;
    l:  INTEGER;   (*left argument: -1(none) or index of a previous operation*)
    r:  Item;      (*sole or right argument*)
  END;

  condname = ARRAY 3 OF CHAR;

  (*operation forms and meaning of fields:
  op       l           r
  -----------------------------
  monadic  -1          arg
  dyadic   opsindex    rightarg
  branch   chain/dest  NIL              *)

VAR
  pc*:     INTEGER;   (* Operation index *)
  varsize: INTEGER;   (*data index*)
  tdx,
  strx:    INTEGER;
  entry:   INTEGER;   (*main entry point*)
  RH:      INTEGER;   (*available registers R[0] ... R[H-1]*)
  frame:   INTEGER;   (*frame offset changed in SaveRegs and RestoreRegs*)
  fixorgP,
  fixorgD,
  fixorgT: INTEGER;   (*origins of lists of locations to be fixed up by loader*)
  check:   BOOLEAN;   (*emit run-time checks*)
  version: INTEGER;   (* 0 = RISC-0, 1 = RISC-5 *)
    
  relname: ARRAY 6 OF condname;
  (* code:    ARRAY maxCode OF INTEGER; *)
  ops:     ARRAY maxOps OF Operation;
  data:    ARRAY maxTD OF INTEGER;  (*type descriptors*)
  str:     ARRAY maxStrx OF CHAR;


(* -------------------------------- Tracing --------------------------------- *)

PROCEDURE wa;                     BEGIN CS.wa       END wa;
PROCEDURE wn;                     BEGIN CS.wn       END wn;
PROCEDURE wc  (c: CHAR);          BEGIN CS.wc(c)    END wc;
PROCEDURE ws  (s: ARRAY OF CHAR); BEGIN CS.ws(s)    END ws;
PROCEDURE wsn (s: ARRAY OF CHAR); BEGIN CS.wsn(s)   END wsn;
PROCEDURE wi  (i: INTEGER);       BEGIN CS.wi(i)    END wi;
PROCEDURE wir (i, n: INTEGER);    BEGIN CS.wir(i,n) END wir;
PROCEDURE wh  (n: INTEGER);       BEGIN CS.wh(n)    END wh;
PROCEDURE wtab(n: INTEGER);       BEGIN CS.wtab(n)  END wtab;

PROCEDURE wsl*(s: ARRAY OF CHAR; w: INTEGER; c: CHAR);  (* Left justified with trailing c *)
BEGIN ws(s);  DEC(w, Host.Length(s));  WHILE w > 0 DO wc(c); DEC(w) END END wsl;

PROCEDURE wsr*(s: ARRAY OF CHAR; w: INTEGER; c: CHAR);  (* Right justified with leading c *)
BEGIN DEC(w, Host.Length(s));  WHILE w > 0 DO wc(c); DEC(w) END;  ws(s) END wsr;

PROCEDURE whr*(n, w: INTEGER; c: CHAR);  (* Right justified with leading c *)
VAR hex: ARRAY 32 OF CHAR;
BEGIN Host.IntToHex(n, hex); wsr(hex, w, c) END whr;

PROCEDURE wil(n, w: INTEGER);
VAR num: ARRAY 32 OF CHAR;
BEGIN Host.IntToDecimal(n, num); wsl(num, w, " ") END wil;

PROCEDURE wSpace(n: INTEGER);
BEGIN WHILE n > 0 DO wc(" ");  DEC(n) END
END wSpace;

PROCEDURE wBool(b: BOOLEAN);
BEGIN IF b THEN ws("true") ELSE ws("false") END
END wBool;

PROCEDURE wCond(cond: INTEGER);
BEGIN
  IF    cond = MI THEN ws("MI")
  ELSIF cond = PL THEN ws("PL")
  ELSIF cond = EQ THEN ws("EQ")
  ELSIF cond = NE THEN ws("NE")
  ELSIF cond = LT THEN ws("LT")
  ELSIF cond = GE THEN ws("GE")
  ELSIF cond = LE THEN ws("LE")
  ELSIF cond = GT THEN ws("GT")
  ELSE ws("cond<"); wi(cond); ws(">")
  END
END wCond;

PROCEDURE wMode(mode: INTEGER);
BEGIN
  IF    mode = CB.Const THEN ws("Const")
  ELSIF mode = CB.Var   THEN ws("Var")
  ELSIF mode = CB.Par   THEN ws("Par")
  ELSIF mode = CB.Fld   THEN ws("Fld")
  ELSIF mode = CB.Typ   THEN ws("Typ")
  ELSIF mode = CB.SProc THEN ws("SProc")
  ELSIF mode = CB.SFunc THEN ws("SFunc")
  ELSIF mode = CB.Mod   THEN ws("Mod")
  ELSIF mode = Reg     THEN ws("Reg")
  ELSIF mode = RegI    THEN ws("RegI")
  ELSIF mode = Cond    THEN ws("Cond")
  ELSE ws("mode<");  wi(mode);  ws(">")
  END
END wMode;

PROCEDURE wForm*(form: INTEGER);
BEGIN
  IF    form = CB.Byte    THEN ws("Byte")     ELSIF form = CB.Bool    THEN ws("Bool")
  ELSIF form = CB.Char    THEN ws("Char")     ELSIF form = CB.Int     THEN ws("Int")
  ELSIF form = CB.Real    THEN ws("Real")     ELSIF form = CB.Set     THEN ws("Set")
  ELSIF form = CB.Pointer THEN ws("Pointer")  ELSIF form = CB.NilTyp  THEN ws("NilTyp")
  ELSIF form = CB.NoTyp   THEN ws("NoTyp")    ELSIF form = CB.Proc    THEN ws("Proc")
  ELSIF form = CB.String  THEN ws("String")   ELSIF form = CB.Array   THEN ws("Array")
  ELSIF form = CB.Record  THEN ws("Record")
  ELSE ws("form<"); wi(form); ws(">")
  END
END wForm;

PROCEDURE wType(type: CB.Type);
BEGIN
  IF (type.typobj # NIL) & (type.typobj.name # "") THEN
    ws(type.typobj.name);  ws(":");
  END;
  wForm(type.form)
END wType;

PROCEDURE wItem(x: Item);
BEGIN
  ws("(mode ");   wMode(x.mode);
  ws(", type ");  wType(x.type);
  ws(", a ");     wi(x.a);
  ws(", b ");     wi(x.b);
  ws(", r ");     wi(x.r);
  ws(", rdo ");   wBool(x.rdo);
  ws(")")
END wItem;

(*
PROCEDURE wObject(o: CB.Object);
BEGIN
  ws("<class ");  wMode(o.class);
  ws(", exno ");  wi(o.exno);
  ws(", expo ");  wBool(o.expo);
  ws(", rdo ");   wBool(o.rdo);
  ws(", lev ");   wi(o.lev);
  IF o.type # NIL THEN ws(", type ");  wType(o.type) END;
  IF o.name[0] # 0X THEN ws(", name ");  ws(o.name) END;
  ws(", val ");   wi(o.val);
  ws(">")
END wObject;
*)

(*
**(*instruction assemblers according to formats*)
**
**PROCEDURE Put0(op, a, b, c: INTEGER);
**BEGIN (*emit format-0 instruction*)
**  code[pc] := ((a*10H + b) * 10H + op) * 10000H + c;  INC(pc)
**END Put0;
**
**PROCEDURE Put1(op, a, b, im: INTEGER);
**BEGIN (*emit format-1 instruction,  -10000H <= im < 10000H*)
**  IF im < 0 THEN INC(op, V) END;
**  code[pc] := (((a+40H) * 10H + b) * 10H + op) * 10000H + (im MOD 10000H);  INC(pc)
**END Put1;
**
**PROCEDURE Put1a(op, a, b, im: INTEGER);
**BEGIN (*same as Put1, but with range test  -10000H <= im < 10000H*)
**  IF (im >= -10000H) & (im <= 0FFFFH) THEN Put1(op, a, b, im)
**  ELSE Put1(Mov+U, RH, 0, im DIV 10000H);
**    IF im MOD 10000H # 0 THEN Put1(Ior, RH, RH, im MOD 10000H) END;
**    Put0(op, a, b, RH)
**  END
**END Put1a;
**
**PROCEDURE Put2(op, a, b, off: INTEGER);
**BEGIN (*emit load/store instruction*)
**  code[pc] := ((op * 10H + a) * 10H + b) * 100000H + (off MOD 100000H);  INC(pc)
**END Put2;
**
**PROCEDURE Put3(op, cond, off: INTEGER);
**BEGIN (*emit branch instruction*)
**  code[pc] := ((op+12) * 10H + cond) * 1000000H + (off MOD 1000000H);  INC(pc)
**END Put3;
**
**PROCEDURE incR;
**BEGIN
**  IF RH < MT-1 THEN INC(RH) ELSE CS.Mark("register stack overflow") END
**END incR;
**
**PROCEDURE CheckRegs*;
**BEGIN
**  IF RH # 0 THEN CS.Mark("Reg Stack");  RH := 0 END;
**  IF pc >= maxCode - 40 THEN CS.Mark("program too long") END;
**  IF frame # 0 THEN CS.Mark("frame error");  frame := 0 END
**END CheckRegs;
*)

PROCEDURE SetCC(VAR x: Item;  n: INTEGER);
BEGIN x.mode := Cond;  x.a := 0;  x.b := 0;  x.r := n
END SetCC;

PROCEDURE Trap(cond, num: INTEGER);
BEGIN
  (*Put3(BLR, cond, CS.Pos()*100H + num*10H + MT)*)
  ws("Trap: cond ");  wCond(cond);  ws(", num ");  wi(num);  wsn(".")
END Trap;

(*handling of forward reference, fixups of branch addresses and constant tables*)

PROCEDURE negated(cond: INTEGER): INTEGER;
BEGIN
  IF    cond = EQ THEN cond := NE  ELSIF cond = NE THEN cond := EQ
  ELSIF cond = LT THEN cond := GE  ELSIF cond = GE THEN cond := LT
  ELSIF cond = GT THEN cond := LE  ELSIF cond = LE THEN cond := GT
  ELSIF cond = PL THEN cond := MI  ELSIF cond = MI THEN cond := PL
  END
RETURN cond END negated;

PROCEDURE FixChain(i, dest: INTEGER);
VAR j: INTEGER;
BEGIN
  WHILE i >= 0 DO
    j := ops[i].l;
    ops[i].l := dest-1;
    i := j
  END
END FixChain;

PROCEDURE MergeChains(i, j: INTEGER): INTEGER;
VAR k: INTEGER;
BEGIN
  IF i >= 0 THEN
    k := i; WHILE ops[k].l >= 0 DO k := ops[k].l END;
    ops[k].l := j;
    k := i
  ELSE
    j := j
  END
RETURN k END MergeChains;

(*
PROCEDURE fix(at, with: INTEGER);
BEGIN code[at] := code[at] DIV C24 * C24 + (with MOD C24)
END fix;

PROCEDURE FixOne*(at: INTEGER);
BEGIN fix(at, pc-at-1)
END FixOne;

PROCEDURE FixLink*(L: INTEGER);
VAR L1: INTEGER;
BEGIN
  WHILE L # 0 DO L1 := code[L] MOD 40000H;  fix(L, pc-L-1);  L := L1 END
END FixLink;

PROCEDURE FixLinkWith(L0, dst: INTEGER);
VAR L1: INTEGER;
BEGIN
  WHILE L0 # 0 DO
    L1 := code[L0] MOD C24;
    code[L0] := code[L0] DIV C24 * C24 + ((dst - L0 - 1) MOD C24);  L0 := L1
  END
END FixLinkWith;

PROCEDURE merged(L0, L1: INTEGER): INTEGER;
VAR L2, L3: INTEGER;
BEGIN
  IF L0 # 0 THEN L3 := L0;
    REPEAT L2 := L3;  L3 := code[L2] MOD 40000H UNTIL L3 = 0;
    code[L2] := code[L2] + L1;  L1 := L0
  END;
  RETURN L1
END merged;
*)


(* loading of operands and addresses into registers *)

(*
PROCEDURE GetSB(base: INTEGER);
BEGIN
  IF version = 0 THEN Put1(Mov, RH, 0, VarOrg0)
  ELSE Put2(Ldr, RH, -base, pc-fixorgD);  fixorgD := pc-1
  END
END GetSB;
*)

PROCEDURE NilCheck;
BEGIN IF check THEN Trap(EQ, 4) END
END NilCheck;

PROCEDURE wOp(pc: INTEGER;  op: Operation);
BEGIN
  wa;  ws("l"); wi(pc);  wtab(10);
  ws(op.op);   wtab(20);
  IF op.r.mode = 0 THEN  (*branch*)
    ws("L");  wi(op.l)
  ELSE
    IF op.l >= 0 THEN ws("l"); wi(op.l); ws(", ") END;
    wItem(op.r)
  END;
  wn
END wOp;

PROCEDURE Monadic(op: ARRAY OF CHAR;  VAR i: Item);
BEGIN
  ops[pc].op := op;
  ops[pc].l  := -1;
  ops[pc].r  := i;
  wOp(pc, ops[pc]);
  i.mode := Reg;  i.r := pc;  INC(pc)
END Monadic;

PROCEDURE Dyadic(op: ARRAY OF CHAR;  VAR l: INTEGER;  VAR r: Item);
BEGIN
  ops[pc].op := op;
  ops[pc].l  := l;
  ops[pc].r  := r;
  wOp(pc, ops[pc]);
  l := pc;  INC(pc)
END Dyadic;

PROCEDURE Branch(op: ARRAY OF CHAR; dest: INTEGER);
BEGIN
  ops[pc].op     := op;
  ops[pc].l      := dest;
  ops[pc].r.mode := 0;
  wOp(pc, ops[pc]);
  INC(pc)
END Branch;

PROCEDURE load(VAR x: Item);
VAR op: INTEGER;
BEGIN
  (* IF x.type.size = 1 THEN op := Ldr+1 ELSE op := Ldr END; *)
  IF x.mode # Reg THEN
    (*
    IF x.mode = CB.Const THEN
      IF x.type.form = CB.Proc THEN
        IF x.r > 0 THEN CS.Mark("not allowed")
        ELSIF x.r = 0 THEN Put3(BL, 7, 0);  Put1a(Sub, RH, LNK, pc*4 - x.a)
        ELSE GetSB(x.r);  Put1(Add, RH, RH, x.a + 100H) (*mark as progbase-relative*)
        END
      ELSIF (x.a <= 0FFFFH) & (x.a >= -10000H) THEN Put1(Mov, RH, 0, x.a)
      ELSE Put1(Mov+U, RH, 0, x.a DIV 10000H MOD 10000H);
        IF x.a MOD 10000H # 0 THEN Put1(Ior, RH, RH, x.a MOD 10000H) END
      END;
      x.r := RH;  incR
    ELSIF x.mode = CB.Var THEN
      IF x.r > 0 THEN (*local*) Put2(op, RH, SP, x.a + frame)
      ELSE GetSB(x.r);  Put2(op, RH, RH, x.a)
      END;
      x.r := RH;  incR
    ELSIF x.mode = CB.Par THEN Put2(Ldr, RH, SP, x.a + frame);  Put2(op, RH, RH, x.b);  x.r := RH;  incR
    ELSIF x.mode = RegI THEN Put2(op, x.r, x.r, x.a)
    ELSIF x.mode = Cond THEN
      Put3(BC, negated(x.r), 2);
      FixLink(x.b);  Put1(Mov, RH, 0, 1);  Put3(BC, 7, 1);
      FixLink(x.a);  Put1(Mov, RH, 0, 0);  x.r := RH;  incR
    END;
    *)
    Monadic("Load", x)
  END
END load;

PROCEDURE loadAdr(VAR x: Item);
BEGIN
  (*
  IF x.mode = CB.Var THEN
    IF x.r > 0 THEN (*local*) Put1a(Add, RH, SP, x.a + frame)
    ELSE GetSB(x.r);  Put1a(Add, RH, RH, x.a)
    END;
    x.r := RH;  incR
  ELSIF x.mode = CB.Par THEN Put2(Ldr, RH, SP, x.a + frame);
    IF x.b # 0 THEN Put1a(Add, RH, RH, x.b) END;
    x.r := RH;  incR
  ELSIF x.mode = RegI THEN
    IF x.a # 0 THEN Put1a(Add, x.r, x.r, x.a) END
  ELSE CS.Mark("address error")
  END;
  *)
  ws("LoadAdr: item ");  wItem(x);  wsn(".");
  x.mode := Reg
END loadAdr;

PROCEDURE loadCond(VAR x: Item);
BEGIN
  ws("* loadCond: x ");  wItem(x);  wn;
  x.mode := Cond;  x.a := 0;  x.b := 0;  x.r := NE
  (*
  IF x.type.form = CB.Bool THEN
    IF x.mode = CB.Const THEN x.r := 15 - x.a*8
    ELSE load(x);
      IF code[pc-1] DIV 40000000H # -2 THEN
        (*Put1(Cmp, x.r, x.r, 0) *)
        ws("Cmp r");  wi(x.r);  wsn(", 0.");
      END;
      x.r := NE;  DEC(RH)
    END;
    x.mode := Cond;  x.a := 0;  x.b := 0
  ELSE CS.Mark("not Boolean?")
  END
  *)
END loadCond;

PROCEDURE loadTypTagAdr(T: CB.Type);
VAR x: Item;
BEGIN x.mode := CB.Var;  x.a := T.len;  x.r := -T.mno;  loadAdr(x)
END loadTypTagAdr;

PROCEDURE loadStringAdr(VAR x: Item);
BEGIN
  (*
  GetSB(0);  Put1a(Add, RH, RH, varsize+x.a);  x.mode := Reg;  x.r := RH;  incR
  *)
  Monadic("LdStrAdr", x);
END loadStringAdr;

(* Items: Conversion from constants or from Objects on the Heap to Items on the Stack*)

PROCEDURE MakeConstItem*(VAR x: Item;  typ: CB.Type;  val: INTEGER);
BEGIN x.mode := CB.Const;  x.type := typ;  x.a := val
END MakeConstItem;

PROCEDURE MakeRealItem*(VAR x: Item;  val: REAL);
BEGIN x.mode := CB.Const;  x.type := CB.realType;  x.a := SYSTEM.VAL(INTEGER, val)
END MakeRealItem;

PROCEDURE MakeStringItem*(VAR x: Item;  len: INTEGER);  (*copies string from S-buffer to OGenerator-string array*)
VAR i: INTEGER;
BEGIN x.mode := CB.Const;  x.type := CB.strType;  x.a := strx;  x.b := len;  i := 0;
  IF strx + len + 4 < maxStrx THEN
    WHILE len > 0 DO str[strx] := CS.str[i];  INC(strx);  INC(i);  DEC(len) END;
    WHILE strx MOD 4 # 0 DO str[strx] := 0X;  INC(strx) END
  ELSE CS.Mark("too many strings")
  END
END MakeStringItem;

PROCEDURE MakeItem*(VAR x: Item;  y: CB.Object;  curlev: INTEGER);
BEGIN x.mode := y.class;  x.type := y.type;  x.a := y.val;  x.rdo := y.rdo;
  IF y.class = CB.Par THEN x.b := 0
  ELSIF (y.class = CB.Const) & (y.type.form = CB.String) THEN x.b := y.lev  (*len*) ;
  ELSE x.r := y.lev
  END;
  IF (y.lev > 0) & (y.lev # curlev) & (y.class # CB.Const) THEN CS.Mark("not accessible ") END
END MakeItem;

(* Code generation for Selectors, Variables, Constants *)

PROCEDURE Field*(VAR x: Item;  y: CB.Object);   (* x := x.y *)
BEGIN;
  IF x.mode = CB.Var THEN
    IF x.r >= 0 THEN x.a := x.a + y.val
    ELSE loadAdr(x);  x.mode := RegI;  x.a := y.val
    END
  ELSIF x.mode = RegI THEN x.a := x.a + y.val
  ELSIF x.mode = CB.Par THEN x.b := x.b + y.val
  END
END Field;

PROCEDURE Index*(VAR x, y: Item);   (* x := x[y] *)
VAR s, lim: INTEGER;
BEGIN
  (*
  s := x.type.base.size;  lim := x.type.len;
  IF (y.mode = CB.Const) & (lim >= 0) THEN
    IF (y.a < 0) OR (y.a >= lim) THEN CS.Mark("bad index") END;
    IF x.mode IN {CB.Var, RegI} THEN x.a := y.a * s + x.a
    ELSIF x.mode = CB.Par THEN x.b := y.a * s + x.b
    END
  ELSE load(y);
    IF check THEN  (*check array bounds*)
      IF lim >= 0 THEN Put1a(Cmp, RH, y.r, lim)
      ELSE (*open array*)
        IF x.mode IN {CB.Var, CB.Par} THEN Put2(Ldr, RH, SP, x.a+4+frame);  Put0(Cmp, RH, y.r, RH)
        ELSE CS.Mark("error in Index")
        END
      END;
      Trap(10, 1)  (*BCC*)
    END;
    IF s = 4 THEN Put1(Lsl, y.r, y.r, 2) ELSIF s > 1 THEN Put1a(Mul, y.r, y.r, s) END;
    IF x.mode = CB.Var THEN
      IF x.r > 0 THEN Put0(Add, y.r, SP, y.r);  INC(x.a, frame)
      ELSE GetSB(x.r);
        IF x.r = 0 THEN Put0(Add, y.r, RH, y.r)
        ELSE Put1a(Add, RH, RH, x.a);  Put0(Add, y.r, RH, y.r);  x.a := 0
        END
      END;
      x.r := y.r;  x.mode := RegI
    ELSIF x.mode = CB.Par THEN
      Put2(Ldr, RH, SP, x.a + frame);
      Put0(Add, y.r, RH, y.r);  x.mode := RegI;  x.r := y.r;  x.a := x.b
    ELSIF x.mode = RegI THEN Put0(Add, x.r, x.r, y.r);  DEC(RH)
    END
  END
  *)
  ws("* Index: x ");  wItem(x);  ws(", [y] ");  wItem(y);  wsn(".")
END Index;

PROCEDURE DeRef*(VAR x: Item);
BEGIN
  (*
  IF x.mode = CB.Var THEN
    IF x.r > 0 THEN (*local*) Put2(Ldr, RH, SP, x.a + frame) ELSE GetSB(x.r);  Put2(Ldr, RH, RH, x.a) END;
    NilCheck;  x.r := RH;  incR
  ELSIF x.mode = CB.Par THEN
    Put2(Ldr, RH, SP, x.a + frame);  Put2(Ldr, RH, RH, x.b);  NilCheck;  x.r := RH;  incR
  ELSIF x.mode = RegI THEN Put2(Ldr, x.r, x.r, x.a);  NilCheck
  ELSIF x.mode # Reg THEN CS.Mark("bad mode in DeRef")
  END;
  *)
  x.mode := RegI;  x.a := 0;  x.b := 0;
  ws("* Deref: x: ");  wItem(x);  wsn(".")
END DeRef;

PROCEDURE Q(T: CB.Type;  VAR dcw: INTEGER);
BEGIN (*one entry of type descriptor extension table*)
  IF T.base # NIL THEN
    Q(T.base, dcw);  data[dcw] := (T.mno*1000H + T.len) * 1000H + dcw - fixorgT;
    fixorgT := dcw;  INC(dcw)
  END
END Q;

PROCEDURE FindPtrFlds(typ: CB.Type;  off: INTEGER;  VAR dcw: INTEGER);
VAR fld: CB.Object;  i, s: INTEGER;
BEGIN
  IF (typ.form = CB.Pointer) OR (typ.form = CB.NilTyp) THEN data[dcw] := off;  INC(dcw)
  ELSIF typ.form = CB.Record THEN
    fld := typ.dsc;
    WHILE fld # NIL DO FindPtrFlds(fld.type, fld.val + off, dcw);  fld := fld.next END
  ELSIF typ.form = CB.Array THEN
    s := typ.base.size;
    FOR i := 0 TO typ.len-1 DO FindPtrFlds(typ.base, i*s + off, dcw) END
  END
END FindPtrFlds;

PROCEDURE BuildTD*(T: CB.Type;  VAR dc: INTEGER);
VAR dcw, k, s: INTEGER;  (*dcw = word address*)
BEGIN dcw := dc DIV 4;  s := T.size;  (*convert size for heap allocation*)
  IF s <= 24 THEN s := 32 ELSIF s <= 56 THEN s := 64 ELSIF s <= 120 THEN s := 128
  ELSE s := (s+263) DIV 256 * 256
  END;
  T.len := dc;  data[dcw] := s;  INC(dcw);  (*len used as address*)
  k := T.nofpar;   (*extension level!*)
  IF k > 3 THEN CS.Mark("ext level too large")
  ELSE Q(T, dcw);
    WHILE k < 3 DO data[dcw] := -1;  INC(dcw);  INC(k) END
  END;
  FindPtrFlds(T, 0, dcw);  data[dcw] := -1;  INC(dcw);  tdx := dcw;  dc := dcw*4;
  IF tdx >= maxTD THEN CS.Mark("too many record types");  tdx := 0 END
END BuildTD;

PROCEDURE TypeTest*(VAR x: Item;  T: CB.Type;  varpar, isguard: BOOLEAN);
VAR pc0: INTEGER;
BEGIN
  IF T = NIL THEN
    IF x.mode >= Reg THEN DEC(RH) END;
    SetCC(x, MI)
  ELSE (*fetch tag into RH*)
    (*
    IF varpar THEN Put2(Ldr, RH, SP, x.a+4+frame)
    ELSE load(x);
      pc0 := pc;  Put3(BC, EQ, 0);  (*NIL belongs to every pointer type*)
      Put2(Ldr, RH, x.r, -8)
    END;
    Put2(Ldr, RH, RH, T.nofpar*4);  incR;
    loadTypTagAdr(T);  (*tag of T*)
    Put0(Cmp, RH-1, RH-1, RH-2);  DEC(RH, 2);
    IF ~varpar THEN fix(pc0, pc - pc0 - 1) END;
    IF isguard THEN
      IF check THEN Trap(NE, 2) END
    ELSE SetCC(x, EQ);
      IF ~varpar THEN DEC(RH) END
    END
    *)
    ws("* TypeTest: x ");  wItem(x);  ws(", T ");  wType(T);
    ws(", varpar ");  wBool(varpar);  ws(", isguard ");  wBool(isguard);
    wsn(".")
  END
END TypeTest;

(* Code generation for Boolean operators *)

PROCEDURE Not*(VAR x: Item);   (* x := ~x *)
VAR t: INTEGER;
BEGIN
  IF x.mode # Cond THEN loadCond(x) END;
  x.r := negated(x.r);  t := x.a;  x.a := x.b;  x.b := t
END Not;

PROCEDURE And1*(VAR x: Item);   (* x := x & *)
BEGIN
  IF x.mode # Cond THEN loadCond(x) END;
  Branch(relname[negated(x.r)], x.a);
  x.a := pc-1;  FixChain(x.b, pc);  x.b := -1
END And1;

PROCEDURE And2*(VAR x, y: Item);
BEGIN
  IF y.mode # Cond THEN loadCond(y) END;
  x.a := MergeChains(y.a, x.a);  x.b := y.b;  x.r := y.r
END And2;

PROCEDURE Or1*(VAR x: Item);   (* x := x OR *)
BEGIN
  IF x.mode # Cond THEN loadCond(x) END;
  Branch(relname[x.r], x.a);
  x.b := pc-1;  FixChain(x.a, pc);  x.a := -1
END Or1;

PROCEDURE Or2*(VAR x, y: Item);
BEGIN
  IF y.mode # Cond THEN loadCond(y) END;
  x.a := y.a;  x.b := MergeChains(y.b, x.b);  x.r := y.r
END Or2;

(* Code generation for arithmetic operators *)

PROCEDURE Neg*(VAR x: Item);   (* x := -x *)
BEGIN
  Monadic("Neg", x)
  (*
  IF x.type.form = CB.Int THEN
    IF x.mode = CB.Const THEN x.a := -x.a
    ELSE load(x);  Put1(Mov, RH, 0, 0);  Put0(Sub, x.r, RH, x.r)
    END
  ELSIF x.type.form = CB.Real THEN
    IF x.mode = CB.Const THEN x.a := x.a + 7FFFFFFFH + 1
    ELSE load(x);  Put1(Mov, RH, 0, 0);  Put0(Fsb, x.r, RH, x.r)
    END
  ELSE (*form = Set*)
    IF x.mode = CB.Const THEN x.a := -x.a-1
    ELSE load(x);  Put1(Xor, x.r, x.r, -1)
    END
  END
  *)
END Neg;

PROCEDURE AddOp*(op: INTEGER;  VAR x, y: Item);   (* x := x +- y *)
BEGIN
  IF op = CS.plus THEN
    IF (x.mode = CB.Const) & (y.mode = CB.Const) THEN x.a := x.a + y.a
    (*
    ELSIF y.mode = CB.Const THEN load(x);
      IF y.a # 0 THEN Put1a(Add, x.r, x.r, y.a) END
    *)
    ELSE
      load(x);
      Dyadic("add", x.r, y)
      (*
      load(y); Put0(Add, RH-2, x.r, y.r);  DEC(RH);  x.r := RH-1
      *)
    END
  ELSE (*op = CS.minus*)
    IF (x.mode = CB.Const) & (y.mode = CB.Const) THEN x.a := x.a - y.a
    (*
    ELSIF y.mode = CB.Const THEN load(x);
      IF y.a # 0 THEN Put1a(Sub, x.r, x.r, y.a) END
    *)
    ELSE
      load(x);
      Dyadic("sub", x.r, y)
      (*
      load(y);  Put0(Sub, RH-2, x.r, y.r);  DEC(RH);  x.r := RH-1
      *)
    END
  END
END AddOp;

PROCEDURE log2(m: INTEGER;  VAR e: INTEGER): INTEGER;
BEGIN e := 0;
  WHILE ~ODD(m) DO m := m DIV 2;  INC(e) END;
  RETURN m
END log2;

PROCEDURE MulOp*(VAR x, y: Item);   (* x := x * y *)
BEGIN
  IF (x.mode = CB.Const) & (y.mode = CB.Const) THEN
    x.a := x.a * y.a
  ELSIF x.mode = CB.Const THEN
    load(y);
    Dyadic("imul", y.r, x);
    x.mode := Reg;  x.r := y.r
  ELSE
    load(x);
    Dyadic("imul", x.r, y)
  END
END MulOp;

PROCEDURE DivOp*(op: INTEGER;  VAR x, y: Item);   (* x := x op y *)
VAR e: INTEGER;
BEGIN
  IF op = CS.div THEN
    IF    (y.mode = CB.Const) & (y.a <= 0) THEN         CS.Mark("bad divisor")
    ELSIF (x.mode = CB.Const) & (y.mode = CB.Const) THEN x.a := x.a DIV y.a
    ELSE
      load(x);
      Dyadic("idiv", x.r, y);  (* idiv should check for div by zero *)
    END
  ELSE (*op = CS.mod*)
    IF    (y.mode = CB.Const) & (y.a <= 0)         THEN CS.Mark("bad modulus")
    ELSIF (x.mode = CB.Const) & (y.mode = CB.Const) THEN x.a := x.a MOD y.a
    ELSE
      load(x);
      Dyadic("imod", x.r, y)
    END
  END
END DivOp;

(* Code generation for REAL operators *)

PROCEDURE RealOp*(op: INTEGER;  VAR x, y: Item);   (* x := x op y *)
BEGIN load(x);
  IF    op = CS.plus  THEN Dyadic("fadd", x.r, y)
  ELSIF op = CS.minus THEN Dyadic("fsub", x.r, y)
  ELSIF op = CS.times THEN Dyadic("fmul", x.r, y)
  ELSIF op = CS.rdiv  THEN Dyadic("fdiv", x.r, y)
  END
END RealOp;

(* Code generation for set operators *)

PROCEDURE Singleton*(VAR x: Item);  (* x := {x} *)
VAR one: Item;
BEGIN
  IF x.mode = CB.Const THEN x.a := LSL(1, x.a)
  ELSE
    Monadic("exp2", x);
  END
END Singleton;

PROCEDURE Set*(VAR x, y: Item);   (* x := {x .. y} *)
BEGIN
  IF (x.mode = CB.Const) & ( y.mode = CB.Const) THEN
    IF x.a <= y.a THEN x.a := LSL(2, y.a) - LSL(1, x.a) ELSE x.a := 0 END
  ELSE
    load(x);  Dyadic("set", x.r, y)
    (*
    IF (x.mode = CB.Const) & (x.a <= 16) THEN x.a := LSL(-1, x.a)
    ELSE load(x);  Put1(Mov, RH, 0, -1);  Put0(Lsl, x.r, RH, x.r)
    END;
    IF (y.mode = CB.Const) & (y.a < 16) THEN Put1(Mov, RH, 0, LSL(-2, y.a));  y.mode := Reg;  y.r := RH;  incR
    ELSE load(y);  Put1(Mov, RH, 0, -2);  Put0(Lsl, y.r, RH, y.r)
    END;
    IF x.mode = CB.Const THEN
      IF x.a # 0 THEN Put1(Xor, y.r, y.r, -1);  Put1a(And, RH-1, y.r, x.a) END;
      x.mode := Reg;  x.r := RH-1
    ELSE DEC(RH);  Put0(Ann, RH-1, x.r, y.r)
    END
    *)
  END
END Set;

PROCEDURE In*(VAR x, y: Item);  (* x := x IN y *)
BEGIN
  load(x);  Dyadic("in", x.r, y);
  (*
  load(y);
  IF x.mode = CB.Const THEN Put1(Ror, y.r, y.r, (x.a + 1) MOD 20H);  DEC(RH)
  ELSE load(x);  Put1(Add, x.r, x.r, 1);  Put0(Ror, y.r, y.r, x.r);  DEC(RH, 2)
  END;
  *)
  SetCC(x, MI)
END In;

PROCEDURE SetOp*(op: INTEGER;  VAR x, y: Item);   (* x := x op y *)
VAR xset, yset: SET;  (*x.type.form = Set*)
BEGIN
  IF (x.mode = CB.Const) & (y.mode = CB.Const) THEN
    xset := SYSTEM.VAL(SET, x.a);  yset := SYSTEM.VAL(SET, y.a);
    IF    op = CS.plus  THEN xset := xset + yset
    ELSIF op = CS.minus THEN xset := xset - yset
    ELSIF op = CS.times THEN xset := xset * yset
    ELSIF op = CS.rdiv  THEN xset := xset / yset
    END;
    x.a := SYSTEM.VAL(INTEGER, xset)
  ELSE
    load(x);
    IF    op = CS.plus  THEN Dyadic("or",     x.r, y)
    ELSIF op = CS.minus THEN Dyadic("andnot", x.r, y)
    ELSIF op = CS.times THEN Dyadic("and",    x.r, y)
    ELSIF op = CS.rdiv  THEN Dyadic("xor",    x.r, y)
    END
  (*
  ELSIF y.mode = CB.Const THEN
    load(x);
    IF op = CS.plus THEN Put1a(Ior, x.r, x.r, y.a)
    ELSIF op = CS.minus THEN Put1a(Ann, x.r, x.r, y.a)
    ELSIF op = CS.times THEN Put1a(And, x.r, x.r, y.a)
    ELSIF op = CS.rdiv THEN Put1a(Xor, x.r, x.r, y.a)
    END;
  ELSE load(x);  load(y);
    IF op = CS.plus THEN Put0(Ior, RH-2, x.r, y.r)
    ELSIF op = CS.minus THEN Put0(Ann, RH-2, x.r, y.r)
    ELSIF op = CS.times THEN Put0(And, RH-2, x.r, y.r)
    ELSIF op = CS.rdiv THEN Put0(Xor, RH-2, x.r, y.r)
    END;
    DEC(RH);  x.r := RH-1
  *)
  END
END SetOp;

(* Code generation for relations *)

PROCEDURE IntRelation*(op: INTEGER;  VAR x, y: Item);   (* x := x < y *)
BEGIN
  load(x);  Dyadic(relname[op - CS.eql], x.r, y);
  SetCC(x, op - CS.eql)
  (*
  load(x);
  IF (y.mode = CB.Const) & (y.type.form # CB.Proc) THEN
    load(x);
    IF (y.a # 0) OR ~(op IN {CS.eql, CS.neq}) OR (code[pc-1] DIV 40000000H # -2) THEN Put1a(Cmp, x.r, x.r, y.a) END;
    DEC(RH)
  ELSE
    IF (x.mode = Cond) OR (y.mode = Cond) THEN CS.Mark("not implemented") END;
    load(x);  load(y);  Put0(Cmp, x.r, x.r, y.r);  DEC(RH, 2)
  END;
  SetCC(x, relmap[op - CS.eql])
  *)
END IntRelation;

PROCEDURE RealRelation*(op: INTEGER;  VAR x, y: Item);   (* x := x < y *)
BEGIN
  load(x);  Dyadic(relname[op - CS.eql], x.r, y);
  SetCC(x, op - CS.eql)
  (*
  load(x);
  IF (y.mode = CB.Const) & (y.a = 0) THEN DEC(RH)
  ELSE load(y);  Put0(Fsb, x.r, x.r, y.r);  DEC(RH, 2)
  END;
  SetCC(x, relmap[op - CS.eql])
  *)
END RealRelation;

PROCEDURE StringRelation*(op: INTEGER;  VAR x, y: Item);   (* x := x < y *)
  (*x, y are char arrays or strings*)
BEGIN
  IF x.type.form = CB.String THEN loadStringAdr(x) ELSE loadAdr(x) END;
  IF y.type.form = CB.String THEN loadStringAdr(y) ELSE loadAdr(y) END;
  Dyadic("strcmp", x.r, y);
  SetCC(x, op - CS.eql)
  (*
  Put2(Ldr+1, RH, x.r, 0);     (*load byte from x str*)
  Put1(Add, x.r, x.r, 1);      (*advance address*)
  Put2(Ldr+1, RH+1, y.r, 0);   (*load byte from y str*)
  Put1(Add, y.r, y.r, 1);      (*advance address*)
  Put0(Cmp, RH+2, RH, RH+1);   (*compare bytes*)
  Put3(BC, NE, 2);             (*exit loop if NE*)
  Put1(Cmp, RH+2, RH, 0);      (*test first str for zero terminator*)
  Put3(BC, NE, -8);            (*if not reach terminator, loop back*)
  DEC(RH, 2);
  SetCC(x, relmap[op - CS.eql])
  *)
END StringRelation;

(* Code generation of Assignments *)

PROCEDURE StrToChar*(VAR x: Item);
BEGIN x.type := CB.charType;  DEC(strx, 4);  x.a := ORD(str[x.a])
END StrToChar;

PROCEDURE Store*(VAR x, y: Item);  (* x := y *)
VAR op: INTEGER;
BEGIN  load(y);
  Dyadic("store", y.r, x)
  (*
  IF x.type.size = 1 THEN op := Str+1 ELSE op := Str END;
  IF x.mode = CB.Var THEN
    IF x.r > 0 THEN (*local*) Put2(op, y.r, SP, x.a + frame)
    ELSE GetSB(x.r);  Put2(op, y.r, RH, x.a)
    END
  ELSIF x.mode = CB.Par THEN Put2(Ldr, RH, SP, x.a + frame);  Put2(op, y.r, RH, x.b);
  ELSIF x.mode = RegI THEN Put2(op, y.r, x.r, x.a);  DEC(RH);
  ELSE CS.Mark("bad mode in Store")
  END;
  DEC(RH)
  *)
END Store;

PROCEDURE StoreStruct*(VAR x, y: Item);  (* x := y, frame = 0 *)
VAR s, pc0: INTEGER;
BEGIN
  IF y.type.size # 0 THEN
    loadStruc(x);  loadStruc(y);
    Monadic("source", y);  Monadic("Copy", x);
    (*
    IF (x.type.form = CB.Array) &  (x.type.len > 0) THEN
      IF y.type.len >= 0 THEN
        IF x.type.size = y.type.size THEN Put1a(Mov, RH, 0, (y.type.size+3) DIV 4)
        ELSE CS.Mark("different length/size, not implemented")
        END
      ELSE (*y  open array*) Put2(Ldr, RH, SP, y.a+4);  s := y.type.base.size;  (*element size*)
        pc0 := pc;  Put3(BC, EQ, 0);
        IF s = 1 THEN Put1(Add, RH, RH, 3);  Put1(Asr, RH, RH, 2)
        ELSIF s # 4 THEN Put1a(Mul, RH, RH, s DIV 4)
        END;
        IF check THEN
          Put1a(Mov, RH+1, 0, (x.type.size+3) DIV 4);  Put0(Cmp, RH+1, RH, RH+1);  Trap(GT, 3)
        END;
        fix(pc0, pc + 5 - pc0)
      END
    ELSIF x.type.form = CB.Record THEN Put1a(Mov, RH, 0, x.type.size DIV 4)
    ELSE CS.Mark("inadmissible assignment")
    END;
    Put2(Ldr, RH+1, y.r, 0);  Put1(Add, y.r, y.r, 4);
    Put2(Str, RH+1, x.r, 0);  Put1(Add, x.r, x.r, 4);
    Put1(Sub, RH, RH, 1);  Put3(BC, NE, -6)
    *)
  END;
  (*
  RH := 0
  *)
END StoreStruct;

PROCEDURE CopyString*(VAR x, y: Item);  (* x := y *)
   VAR len: INTEGER;
 BEGIN loadAdr(x);  len := x.type.len;
  IF len >= 0 THEN
    IF len <  y.b THEN CS.Mark("string too long") END
  ELSIF check THEN Put2(Ldr, RH, SP, x.a+4);  (*open array len, frame = 0*)
    Put1(Cmp,RH, RH, y.b);  Trap(LT, 3)
  END;
  loadStringAdr(y);
  Put2(Ldr, RH, y.r, 0);  Put1(Add, y.r, y.r, 4);
  Put2(Str, RH, x.r, 0);  Put1(Add, x.r, x.r, 4);
  Put1(Asr, RH, RH, 24);  Put3(BC, NE,  -6);  RH := 0
 END CopyString;
  
(* Code generation for parameters *)
  
PROCEDURE OpenArrayParam*(VAR x: Item);
BEGIN loadAdr(x);
  IF x.type.len >= 0 THEN Put1a(Mov, RH, 0, x.type.len) ELSE Put2(Ldr, RH, SP, x.a+4+frame) END;
  incR
END OpenArrayParam;

PROCEDURE VarParam*(VAR x: Item;  ftype: CB.Type);
VAR xmd: INTEGER;
BEGIN xmd := x.mode;  loadAdr(x);
  IF (ftype.form = CB.Array) & (ftype.len < 0) THEN (*open array*)
    IF x.type.len >= 0 THEN Put1a(Mov, RH, 0, x.type.len) ELSE  Put2(Ldr, RH, SP, x.a+4+frame) END;
    incR
  ELSIF ftype.form = CB.Record THEN
    IF xmd = CB.Par THEN Put2(Ldr, RH, SP, x.a+4+frame);  incR ELSE loadTypTagAdr(x.type) END
  END
END VarParam;

PROCEDURE ValueParam*(VAR x: Item);
BEGIN load(x)
END ValueParam;

PROCEDURE StringParam*(VAR x: Item);
BEGIN loadStringAdr(x);  Put1(Mov, RH, 0, x.b);  incR  (*len*)
END StringParam;

(*For Statements*)

PROCEDURE For0*(VAR x, y: Item);
BEGIN load(y)
END For0;

PROCEDURE For1*(VAR x, y, z, w: Item;  VAR L: INTEGER);
BEGIN
  IF z.mode = CB.Const THEN Put1a(Cmp, RH, y.r, z.a)
  ELSE load(z);  Put0(Cmp, RH-1, y.r, z.r);  DEC(RH)
  END;
  L := pc;
  IF w.a > 0 THEN Put3(BC, GT, 0)
  ELSIF w.a < 0 THEN Put3(BC, LT, 0)
  ELSE CS.Mark("zero increment");  Put3(BC, MI, 0)
  END;
  Store(x, y)
END For1;

PROCEDURE For2*(VAR x, y, w: Item);
BEGIN load(x);  DEC(RH);  Put1a(Add, x.r, x.r, w.a)
END For2;

(* Branches, procedure calls, procedure prolog and epilog *)

PROCEDURE Here*(): INTEGER;
BEGIN RETURN pc
END Here;

PROCEDURE FJump*(VAR L: INTEGER);
BEGIN Put3(BC, 7, L);  L := pc-1
END FJump;

PROCEDURE CFJump*(VAR x: Item);
BEGIN
  IF x.mode # Cond THEN loadCond(x) END;
  Put3(BC, negated(x.r), x.a);  FixLink(x.b);  x.a := pc-1
END CFJump;

PROCEDURE BJump*(L: INTEGER);
BEGIN Put3(BC, 7, L-pc-1)
END BJump;

PROCEDURE CBJump*(VAR x: Item;  L: INTEGER);
BEGIN
  IF x.mode # Cond THEN loadCond(x) END;
  Put3(BC, negated(x.r), L-pc-1);  FixLink(x.b);  FixLinkWith(x.a, L)
END CBJump;

PROCEDURE Fixup*(VAR x: Item);
BEGIN FixLink(x.a)
END Fixup;

PROCEDURE SaveRegs(r: INTEGER);  (* R[0 .. r-1]*)
VAR r0: INTEGER;
BEGIN (*r > 0*) r0 := 0;
  Put1(Sub, SP, SP, r*4);  INC(frame, 4*r);
  REPEAT Put2(Str, r0, SP, (r-r0-1)*4);  INC(r0) UNTIL r0 = r
END SaveRegs;

PROCEDURE RestoreRegs(r: INTEGER);  (*R[0 .. r-1]*)
VAR r0: INTEGER;
BEGIN (*r > 0*) r0 := r;
  REPEAT DEC(r0);  Put2(Ldr, r0, SP, (r-r0-1)*4) UNTIL r0 = 0;
  Put1(Add, SP, SP, r*4);  DEC(frame, 4*r)
END RestoreRegs;

PROCEDURE PrepCall*(VAR x: Item;  VAR r: INTEGER);
BEGIN (*x.type.form = CB.Proc*)
  IF x.mode > CB.Par THEN load(x) END;
  r := RH;
  IF RH > 0 THEN SaveRegs(RH);  RH := 0 END
END PrepCall;

PROCEDURE Call*(VAR x: Item;  r: INTEGER);
BEGIN (*x.type.form = CB.Proc*)
  IF x.mode = CB.Const THEN
    IF x.r >= 0 THEN Put3(BL, 7, (x.a DIV 4)-pc-1)
    ELSE (*imported*)
      IF pc - fixorgP < 1000H THEN
        Put3(BL, 7, ((-x.r) * 100H + x.a) * 1000H + pc-fixorgP);  fixorgP := pc-1
      ELSE CS.Mark("fixup impossible")
      END
    END
  ELSE
    IF x.mode <= CB.Par THEN load(x);  DEC(RH)
    ELSE Put2(Ldr, RH, SP, 0);  Put1(Add, SP, SP, 4);  DEC(r);  DEC(frame, 4)
    END;
    IF check THEN Trap(EQ, 5) END;
    Put3(BLR, 7, RH)
  END;
  IF x.type.base.form = CB.NoTyp THEN (*procedure*) RH := 0
  ELSE (*function*)
    IF r > 0 THEN Put0(Mov, r, 0, 0);  RestoreRegs(r) END;
    x.mode := Reg;  x.r := r;  RH := r+1
  END
END Call;

PROCEDURE Enter*(parblksize, locblksize: INTEGER;  int: BOOLEAN);
VAR a, r: INTEGER;
BEGIN frame := 0;
  IF ~int THEN (*procedure prolog*)
    IF locblksize >= 10000H THEN CS.Mark("too many locals") END;
    a := 4;  r := 0;
    Put1(Sub, SP, SP, locblksize);  Put2(Str, LNK, SP, 0);
    WHILE a < parblksize DO Put2(Str, r, SP, a);  INC(r);  INC(a, 4) END
  ELSE (*interrupt procedure*)
    Put1(Sub, SP, SP, locblksize);  Put2(Str, 0, SP, 0);  Put2(Str, 1, SP, 4);  Put2(Str, 2, SP, 8)
    (*R0, R1, R2 saved on stack*)
  END
END Enter;

PROCEDURE Return*(form: INTEGER;  VAR x: Item;  size: INTEGER;  int: BOOLEAN);
BEGIN
  IF form # CB.NoTyp THEN load(x) END;
  IF ~int THEN (*procedure epilog*)
    Put2(Ldr, LNK, SP, 0);  Put1(Add, SP, SP, size);  Put3(BR, 7, LNK)
  ELSE (*interrupt return, restore R2, R1, R0*)
    Put2(Ldr, 2, SP, 8);  Put2(Ldr, 1, SP, 4);  Put2(Ldr, 0, SP, 0);  Put1(Add, SP, SP, size);
    Put3(BR, 7, 10H) (*RTI*)
  END;
  RH := 0
END Return;

(* In-line code procedures*)

PROCEDURE Increment*(upordown: INTEGER;  VAR x, y: Item);
VAR op, zr, v: INTEGER;
BEGIN (*frame = 0*)
  IF upordown = 0 THEN op := Add ELSE op := Sub END;
  IF x.type = CB.byteType THEN v := 1 ELSE v := 0 END;
  IF y.type.form = CB.NoTyp THEN y.mode := CB.Const;  y.a := 1 END;
  IF (x.mode = CB.Var) & (x.r > 0) THEN
    zr := RH;  Put2(Ldr+v, zr, SP, x.a);  incR;
    IF y.mode = CB.Const THEN Put1a(op, zr, zr, y.a) ELSE load(y);  Put0(op, zr, zr, y.r);  DEC(RH) END;
    Put2(Str+v, zr, SP, x.a);  DEC(RH)
  ELSE loadAdr(x);  zr := RH;  Put2(Ldr+v, RH, x.r, 0);  incR;
    IF y.mode = CB.Const THEN Put1a(op, zr, zr, y.a) ELSE load(y);  Put0(op, zr, zr, y.r);  DEC(RH) END;
    Put2(Str+v, zr, x.r, 0);  DEC(RH, 2)
  END
END Increment;

PROCEDURE Include*(inorex: INTEGER;  VAR x, y: Item);
VAR op, zr: INTEGER;
BEGIN loadAdr(x);  zr := RH;  Put2(Ldr, RH, x.r, 0);  incR;
  IF inorex = 0 THEN op := Ior ELSE op := Ann END;
  IF y.mode = CB.Const THEN Put1a(op, zr, zr, LSL(1, y.a))
  ELSE load(y);  Put1(Mov, RH, 0, 1);  Put0(Lsl, y.r, RH, y.r);  Put0(op, zr, zr, y.r);  DEC(RH)
  END;
  Put2(Str, zr, x.r, 0);  DEC(RH, 2)
END Include;

PROCEDURE Assert*(VAR x: Item);
VAR cond: INTEGER;
BEGIN
  IF x.mode # Cond THEN loadCond(x) END;
  IF x.a = 0 THEN cond := negated(x.r)
  ELSE Put3(BC, x.r, x.b);  FixLink(x.a);  x.b := pc-1;  cond := 7
  END;
  Trap(cond, 7);  FixLink(x.b)
END Assert;

PROCEDURE New*(VAR x: Item);
BEGIN loadAdr(x);  loadTypTagAdr(x.type.base);  Trap(7, 0);  RH := 0
END New;

PROCEDURE Pack*(VAR x, y: Item);
VAR z: Item;
BEGIN z := x;  load(x);  load(y);
  Put1(Lsl, y.r, y.r, 23);  Put0(Add, x.r, x.r, y.r);  DEC(RH);  Store(z, x)
END Pack;

PROCEDURE Unpk*(VAR x, y: Item);
VAR z, e0: Item;
BEGIN  z := x;  load(x);  e0.mode := Reg;  e0.r := RH;  e0.type := CB.intType;
  Put1(Asr, RH, x.r, 23);  Put1(Sub, RH, RH, 127);  Store(y, e0);  incR;
  Put1(Lsl, RH, RH, 23);  Put0(Sub, x.r, x.r, RH);  Store(z, x)
END Unpk;

PROCEDURE Led*(VAR x: Item);
BEGIN load(x);  Put1(Mov, RH, 0, -60);  Put2(Str, x.r, RH, 0);  DEC(RH)
END Led;

PROCEDURE Get*(VAR x, y: Item);
BEGIN load(x);  x.type := y.type;  x.mode := RegI;  x.a := 0;  Store(y, x)
END Get;

PROCEDURE Put*(VAR x, y: Item);
BEGIN load(x);  x.type := y.type;  x.mode := RegI;  x.a := 0;  Store(x, y)
END Put;

PROCEDURE Copy*(VAR x, y, z: Item);
BEGIN load(x);  load(y);
  IF z.mode = CB.Const THEN
    IF z.a > 0 THEN load(z) ELSE CS.Mark("bad count") END
  ELSE load(z);
    IF check THEN Trap(LT, 3) END;
    Put3(BC, EQ, 6)
  END;
  Put2(Ldr, RH, x.r, 0);  Put1(Add, x.r, x.r, 4);
  Put2(Str, RH, y.r, 0);  Put1(Add, y.r, y.r, 4);
  Put1(Sub, z.r, z.r, 1);  Put3(BC, NE, -6);  DEC(RH, 3)
END Copy;

PROCEDURE LDPSR*(VAR x: Item);
BEGIN (*x.mode = Const*)  Put3(0, 15, x.a + 20H)
END LDPSR;

PROCEDURE LDREG*(VAR x, y: Item);
BEGIN
  IF y.mode = CB.Const THEN Put1a(Mov, x.a, 0, y.a)
  ELSE load(y);  Put0(Mov, x.a, 0, y.r);  DEC(RH)
  END
END LDREG;

(*In-line code functions*)

PROCEDURE Abs*(VAR x: Item);
BEGIN
  IF x.mode = CB.Const THEN x.a := ABS(x.a)
  ELSE load(x);
    IF x.type.form = CB.Real THEN Put1(Lsl, x.r, x.r, 1);  Put1(Ror, x.r, x.r, 1)
    ELSE Put1(Cmp, x.r, x.r, 0);  Put3(BC, GE, 2);  Put1(Mov, RH, 0, 0);  Put0(Sub, x.r, RH, x.r)
    END
  END
END Abs;

PROCEDURE Odd*(VAR x: Item);
BEGIN load(x);  Put1(And, x.r, x.r, 1);  SetCC(x, NE);  DEC(RH)
END Odd;

PROCEDURE Floor*(VAR x: Item);
BEGIN load(x);  Put1(Mov+U, RH, 0, 4B00H);  Put0(Fad+V, x.r, x.r, RH)
END Floor;

PROCEDURE Float*(VAR x: Item);
BEGIN load(x);  Put1(Mov+U, RH, 0, 4B00H);  Put0(Fad+U, x.r, x.r, RH)
END Float;

PROCEDURE Ord*(VAR x: Item);
BEGIN
  IF x.mode IN {CB.Var, CB.Par, RegI, Cond} THEN load(x) END
END Ord;

PROCEDURE Len*(VAR x: Item);
BEGIN
  IF x.type.len >= 0 THEN
    IF x.mode = RegI THEN DEC(RH) END;
    x.mode := CB.Const;  x.a := x.type.len
  ELSE (*open array*) Put2(Ldr, RH, SP, x.a + 4 + frame);  x.mode := Reg;  x.r := RH;  incR
  END
END Len;

PROCEDURE Shift*(fct: INTEGER;  VAR x, y: Item);
VAR op: INTEGER;
BEGIN load(x);
  IF fct = 0 THEN op := Lsl ELSIF fct = 1 THEN op := Asr ELSE op := Ror END;
  IF y.mode = CB.Const THEN Put1(op, x.r, x.r, y.a MOD 20H)
  ELSE load(y);  Put0(op, RH-2, x.r, y.r);  DEC(RH);  x.r := RH-1
  END
END Shift;

PROCEDURE ADC*(VAR x, y: Item);
BEGIN load(x);  load(y);  Put0(Add+2000H, x.r, x.r, y.r);  DEC(RH)
END ADC;

PROCEDURE SBC*(VAR x, y: Item);
BEGIN load(x);  load(y);  Put0(Sub+2000H, x.r, x.r, y.r);  DEC(RH)
END SBC;

PROCEDURE UML*(VAR x, y: Item);
BEGIN load(x);  load(y);  Put0(Mul+2000H, x.r, x.r, y.r);  DEC(RH)
END UML;

PROCEDURE Bit*(VAR x, y: Item);
BEGIN load(x);  Put2(Ldr, x.r, x.r, 0);
  IF y.mode = CB.Const THEN Put1(Ror, x.r, x.r, y.a+1);  DEC(RH)
  ELSE load(y);  Put1(Add, y.r, y.r, 1);  Put0(Ror, x.r, x.r, y.r);  DEC(RH, 2)
  END;
  SetCC(x, MI)
END Bit;

PROCEDURE Register*(VAR x: Item);
BEGIN (*x.mode = Const*)
  Put0(Mov, RH, 0, x.a MOD 10H);  x.mode := Reg;  x.r := RH;  incR
END Register;

PROCEDURE H*(VAR x: Item);
BEGIN (*x.mode = Const*)
  Put0(Mov + U + x.a MOD 2 * V, RH, 0, 0);  x.mode := Reg;  x.r := RH;  incR
END H;

PROCEDURE Adr*(VAR x: Item);
BEGIN
  IF x.mode IN {CB.Var, CB.Par, RegI} THEN loadAdr(x)
  ELSIF (x.mode = CB.Const) & (x.type.form = CB.Proc) THEN load(x)
  ELSIF (x.mode = CB.Const) & (x.type.form = CB.String) THEN loadStringAdr(x)
  ELSE CS.Mark("not addressable")
  END
END Adr;

PROCEDURE Condition*(VAR x: Item);
BEGIN (*x.mode = Const*) SetCC(x, x.a)
END Condition;

PROCEDURE Open*(v: INTEGER);
BEGIN pc := 0;  tdx := 0;  strx := 0;  RH := 0;  fixorgP := 0;  fixorgD := 0;  fixorgT := 0;  check := v # 0;  version := v;
  IF v = 0 THEN pc := 1;
    REPEAT code[pc] := 0;  INC(pc) UNTIL pc = 8
  END
END Open;

PROCEDURE SetDataSize*(dc: INTEGER);
BEGIN varsize := dc
END SetDataSize;

PROCEDURE Header*;
BEGIN entry := pc*4;
  IF version = 0 THEN code[0] := 0E7000000H-1 + pc;  Put1a(Mov, SP, 0, StkOrg0)  (*RISC-0*)
  ELSE Put1(Sub, SP, SP, 4);  Put2(Str, LNK, SP, 0)
  END
END Header;

PROCEDURE NofPtrs(typ: CB.Type): INTEGER;
VAR fld: CB.Object;  n: INTEGER;
BEGIN
  IF (typ.form = CB.Pointer) OR (typ.form = CB.NilTyp) THEN n := 1
  ELSIF typ.form = CB.Record THEN
    fld := typ.dsc;  n := 0;
    WHILE fld # NIL DO n := NofPtrs(fld.type) + n;  fld := fld.next END
  ELSIF typ.form = CB.Array THEN n := NofPtrs(typ.base) * typ.len
  ELSE n := 0
  END;
  RETURN n
END NofPtrs;

PROCEDURE FindPtrs(VAR R: Files.Rider;  typ: CB.Type;  adr: INTEGER);
VAR fld: CB.Object;  i, s: INTEGER;
BEGIN
  IF (typ.form = CB.Pointer) OR (typ.form = CB.NilTyp) THEN Files.WriteInt(R, adr)
  ELSIF typ.form = CB.Record THEN
    fld := typ.dsc;
    WHILE fld # NIL DO FindPtrs(R, fld.type, fld.val + adr);  fld := fld.next END
  ELSIF typ.form = CB.Array THEN
    s := typ.base.size;
    FOR i := 0 TO typ.len-1 DO FindPtrs(R, typ.base, i*s + adr) END
  END
END FindPtrs;

PROCEDURE Close*(VAR modid: CS.Ident;  key, nofent: INTEGER);
VAR obj: CB.Object;
    i, comsize, nofimps, nofptrs, size: INTEGER;
    name: CS.Ident;
    F: Files.File;  R: Files.Rider;
BEGIN  (*exit code*)
  IF version = 0 THEN Put1(Mov, 0, 0, 0);  Put3(BR, 7, 0)  (*RISC-0*)
  ELSE Put2(Ldr, LNK, SP, 0);  Put1(Add, SP, SP, 4);  Put3(BR, 7, LNK)
  END;
  obj := CB.topScope.next;  nofimps := 0;  comsize := 4;  nofptrs := 0;
  WHILE obj # NIL DO
    IF (obj.class = CB.Mod) & (obj.dsc # CB.system) THEN INC(nofimps) (*count imports*)
    ELSIF (obj.exno # 0) & (obj.class = CB.Const) & (obj.type.form = CB.Proc)
        & (obj.type.nofpar = 0) & (obj.type.base = CB.noType) THEN i := 0;  (*count commands*)
      WHILE obj.name[i] # 0X DO INC(i) END;
      i := (i+4) DIV 4 * 4;  INC(comsize, i+4)
    ELSIF obj.class = CB.Var THEN INC(nofptrs, NofPtrs(obj.type))  (*count pointers*)
    END;
    obj := obj.next
  END;
  size := varsize + strx + comsize + (pc + nofimps + nofent + nofptrs + 1)*4;  (*varsize includes type descriptors*)

  CB.MakeFileName(name, modid, ".rsc");  (*write code file*)
  F := Files.New(name);  Files.Set(R, F, 0);  Files.WriteString(R, modid);  Files.WriteInt(R, key);  Files.Write(R, CHR(version));
  Files.WriteInt(R, size);
  obj := CB.topScope.next;
  WHILE (obj # NIL) & (obj.class = CB.Mod) DO  (*imports*)
    IF obj.dsc # CB.system THEN Files.WriteString(R, obj(CB.Module).orgname);  Files.WriteInt(R, obj.val) END;
    obj := obj.next
  END;
  Files.Write(R, 0X);
  Files.WriteInt(R, tdx*4);
  i := 0;
  WHILE i < tdx DO Files.WriteInt(R, data[i]);  INC(i) END;  (*type descriptors*)
  Files.WriteInt(R, varsize - tdx*4);  (*data*)
  Files.WriteInt(R, strx);
  FOR i := 0 TO strx-1 DO Files.Write(R, str[i]) END;  (*strings*)
  Files.WriteInt(R, pc);  (*code len*)
  FOR i := 0 TO pc-1 DO Files.WriteInt(R, code[i]) END;  (*program*)
  obj := CB.topScope.next;
  WHILE obj # NIL DO  (*commands*)
    IF (obj.exno # 0) & (obj.class = CB.Const) & (obj.type.form = CB.Proc) &
        (obj.type.nofpar = 0) & (obj.type.base = CB.noType) THEN
      Files.WriteString(R, obj.name);  Files.WriteInt(R, obj.val)
    END;
    obj := obj.next
  END;
  Files.Write(R, 0X);
  Files.WriteInt(R, nofent);  Files.WriteInt(R, entry);
  obj := CB.topScope.next;
  WHILE obj # NIL DO  (*entries*)
    IF obj.exno # 0 THEN
      IF (obj.class = CB.Const) & (obj.type.form = CB.Proc) OR (obj.class = CB.Var) THEN
        Files.WriteInt(R, obj.val);
      ELSIF obj.class = CB.Typ THEN
        IF obj.type.form = CB.Record THEN Files.WriteInt(R,  obj.type.len MOD 10000H)
        ELSIF (obj.type.form = CB.Pointer) & ((obj.type.base.typobj = NIL) OR (obj.type.base.typobj.exno = 0)) THEN
          Files.WriteInt(R,  obj.type.base.len MOD 10000H)
        END
      END
    END;
    obj := obj.next
  END;
  obj := CB.topScope.next;
  WHILE obj # NIL DO  (*pointer variables*)
    IF obj.class = CB.Var THEN FindPtrs(R, obj.type, obj.val) END;
    obj := obj.next
  END;
  Files.WriteInt(R, -1);
  Files.WriteInt(R, fixorgP);  Files.WriteInt(R, fixorgD);  Files.WriteInt(R, fixorgT);  Files.WriteInt(R, entry);
  Files.Write(R, "O");  Files.Register(F)
END Close;

BEGIN
  relname[0]  := "eq";  relname[1] := "ne";
  relname[2]  := "lt";  relname[3] := "le";
  relname[4]  := "gt";  relname[5] := "ge";
END CompilerGenerator.
