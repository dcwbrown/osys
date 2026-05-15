MODULE CompilerParser;  (*N. Wirth 1.7.97 / 8.3.2020  Oberon compiler for RISC in Oberon-07*)

IMPORT Texts, Oberon, CS := CompilerScanner, CB := CompilerBase, CG := CompilerGenerator;

(*Author: Niklaus Wirth, 2014.
  Parser of Oberon-RISC compiler. Uses Scanner CS to obtain symbols (tokens),
  CB for definition of data structures and for handling import and export, and
  CG to produce binary code. CompilerParser performs type checking and data allocation.
  Parser is target-independent, except for part of the handling of allocations.*)

TYPE 
  PtrBase     = POINTER TO PtrBaseDesc;
  PtrBaseDesc = RECORD  (*list of names of pointer base types*)
    name: CS.Ident;  
    type: CB.Type;  
    next: PtrBase
  END;
  
VAR 
  sym:        INTEGER;   (*last symbol read*)
  dc:         INTEGER;    (*data counter*)
  level:      INTEGER; 
  exno:       INTEGER;
  version:    INTEGER;
  newSF:      BOOLEAN;  (*option flag*)
  expression: PROCEDURE (VAR x: CG.Item);  (*to avoid forward reference*)
  Type:       PROCEDURE (VAR type: CB.Type);
  FormalType: PROCEDURE (VAR typ: CB.Type;  dim: INTEGER);
  modid:      CS.Ident;
  pbsList:    PtrBase;   (*list of names of pointer base types*)
  dummy:      CB.Object;
  W:          Texts.Writer;

PROCEDURE Check(s: INTEGER;  msg: ARRAY OF CHAR);
BEGIN
  IF sym = s THEN CS.Get(sym) ELSE CS.Mark(msg) END
END Check;

PROCEDURE qualident(VAR obj: CB.Object);
BEGIN obj := CB.thisObj();  CS.Get(sym);
  IF obj = NIL THEN CS.Mark("undef");  obj := dummy END;
  IF (sym = CS.period) & (obj.class = CB.Mod) THEN
    CS.Get(sym);
    IF sym = CS.ident THEN obj := CB.thisimport(obj);  CS.Get(sym);
      IF obj = NIL THEN CS.Mark("undef");  obj := dummy END
    ELSE CS.Mark("identifier expected");  obj := dummy
    END
  END
END qualident;

PROCEDURE CheckBool(VAR x: CG.Item);
BEGIN
  IF x.type.form # CB.Bool THEN CS.Mark("not Boolean");  x.type := CB.boolType END
END CheckBool;

PROCEDURE CheckInt(VAR x: CG.Item);
BEGIN
  IF x.type.form # CB.Int THEN CS.Mark("not Integer");  x.type := CB.intType END
END CheckInt;

PROCEDURE CheckReal(VAR x: CG.Item);
BEGIN
  IF x.type.form # CB.Real THEN CS.Mark("not Real");  x.type := CB.realType END
END CheckReal;

PROCEDURE CheckSet(VAR x: CG.Item);
BEGIN
  IF x.type.form # CB.Set THEN CS.Mark("not Set");  x.type := CB.setType END
END CheckSet;

PROCEDURE CheckSetVal(VAR x: CG.Item);
BEGIN
  IF x.type.form # CB.Int THEN CS.Mark("not Int");  x.type := CB.setType
  ELSIF x.mode = CB.Const THEN
    IF (x.a < 0) OR (x.a >= 32) THEN CS.Mark("invalid set") END
  END
END CheckSetVal;

PROCEDURE CheckConst(VAR x: CG.Item);
BEGIN
  IF x.mode # CB.Const THEN CS.Mark("not a constant");  x.mode := CB.Const END
END CheckConst;

PROCEDURE CheckReadOnly(VAR x: CG.Item);
BEGIN
  IF x.rdo THEN CS.Mark("read-only") END
END CheckReadOnly;

PROCEDURE CheckExport(VAR expo: BOOLEAN);
BEGIN
  IF sym = CS.times THEN
    expo := TRUE;  CS.Get(sym);
    IF level # 0 THEN CS.Mark("remove asterisk") END
  ELSE expo := FALSE
  END
END CheckExport;

PROCEDURE IsExtension(t0, t1: CB.Type): BOOLEAN;
BEGIN (*t1 is an extension of t0*)
  RETURN (t0 = t1) OR (t1 # NIL) & IsExtension(t0, t1.base)
END IsExtension;

(* expressions *)

PROCEDURE TypeTest(VAR x: CG.Item;  T: CB.Type;  guard: BOOLEAN);
VAR xt: CB.Type;
BEGIN xt := x.type;
  IF (T.form = xt.form ) & ((T.form = CB.Pointer) OR (T.form = CB.Record) & (x.mode = CB.Par)) THEN
    WHILE (xt # T) & (xt # NIL) DO xt := xt.base END;
    IF xt # T THEN xt := x.type;
      IF xt.form = CB.Pointer THEN
        IF IsExtension(xt.base, T.base) THEN CG.TypeTest(x, T.base, FALSE, guard);  x.type := T
        ELSE CS.Mark("not an extension")
        END
      ELSIF (xt.form = CB.Record) & (x.mode = CB.Par) THEN
        IF IsExtension(xt, T) THEN  CG.TypeTest(x, T, TRUE, guard);  x.type := T
        ELSE CS.Mark("not an extension")
        END
      ELSE CS.Mark("incompatible types")
      END
    ELSIF ~guard THEN CG.TypeTest(x, NIL, FALSE, FALSE)
    END
  ELSE CS.Mark("type mismatch")
  END;
  IF ~guard THEN x.type := CB.boolType END
END TypeTest;

PROCEDURE selector(VAR x: CG.Item);
VAR y: CG.Item;  obj: CB.Object;
BEGIN
  WHILE (sym = CS.lbrak) OR (sym = CS.period) OR (sym = CS.arrow)
      OR (sym = CS.lparen) & (x.type.form IN {CB.Record, CB.Pointer}) DO
    IF sym = CS.lbrak THEN
      REPEAT CS.Get(sym);  expression(y);
        IF x.type.form = CB.Array THEN
          CheckInt(y);  CG.Index(x, y);  x.type := x.type.base
        ELSE CS.Mark("not an array")
        END
      UNTIL sym # CS.comma;
      Check(CS.rbrak, "no ]")
    ELSIF sym = CS.period THEN CS.Get(sym);
      IF sym = CS.ident THEN
        IF x.type.form = CB.Pointer THEN CG.DeRef(x);  x.type := x.type.base END;
        IF x.type.form = CB.Record THEN
          obj := CB.thisfield(x.type);  CS.Get(sym);
          IF obj # NIL THEN CG.Field(x, obj);  x.type := obj.type
          ELSE CS.Mark("undef")
          END
        ELSE CS.Mark("not a record")
        END
      ELSE CS.Mark("ident?")
      END
    ELSIF sym = CS.arrow THEN
      CS.Get(sym);
      IF x.type.form = CB.Pointer THEN CG.DeRef(x);  x.type := x.type.base
      ELSE CS.Mark("not a pointer")
      END
    ELSIF (sym = CS.lparen) & (x.type.form IN {CB.Record, CB.Pointer}) THEN (*type guard*)
      CS.Get(sym);
      IF sym = CS.ident THEN
        qualident(obj);
        IF obj.class = CB.Typ THEN TypeTest(x, obj.type, TRUE)
        ELSE CS.Mark("guard type expected")
        END
      ELSE CS.Mark("not an identifier")
      END;
      Check(CS.rparen, " ) missing")
    END
  END
END selector;

PROCEDURE EqualSignatures(t0, t1: CB.Type): BOOLEAN;
VAR p0, p1: CB.Object;  com: BOOLEAN;
BEGIN com := TRUE;
  IF (t0.base = t1.base) & (t0.nofpar = t1.nofpar) THEN
    p0 := t0.dsc;  p1 := t1.dsc;
    WHILE p0 # NIL DO
      IF (p0.class = p1.class) &  (p0.rdo = p1.rdo) &
        ((p0.type = p1.type) OR
        (p0.type.form = CB.Array) & (p1.type.form = CB.Array) & (p0.type.len = p1.type.len) & (p0.type.base = p1.type.base) OR
        (p0.type.form = CB.Proc) & (p1.type.form = CB.Proc) & EqualSignatures(p0.type, p1.type))
      THEN p0 := p0.next;  p1 := p1.next
      ELSE p0 := NIL;  com := FALSE
      END
    END
  ELSE com := FALSE
  END;
  RETURN com
END EqualSignatures;

PROCEDURE CompTypes(t0, t1: CB.Type;  varpar: BOOLEAN): BOOLEAN;
BEGIN (*check for assignment compatibility*)
  RETURN (t0 = t1)    (*openarray assignment disallowed in CG*)
    OR (t0.form = CB.Array) & (t1.form = CB.Array) & (t0.base =  t1.base) & (t0.len = t1.len)
    OR (t0.form = CB.Record) & (t1.form = CB.Record)  & IsExtension(t0, t1)
    OR ~varpar &
      ((t0.form = CB.Pointer) & (t1.form = CB.Pointer)  & IsExtension(t0.base, t1.base)
      OR (t0.form = CB.Proc) & (t1.form = CB.Proc) & EqualSignatures(t0, t1)
      OR (t0.form IN {CB.Pointer, CB.Proc}) & (t1.form = CB.NilTyp))
END CompTypes;

PROCEDURE Parameter(par: CB.Object);
VAR x: CG.Item;  varpar: BOOLEAN;
BEGIN expression(x);
  IF par # NIL THEN
    varpar := par.class = CB.Par;
    IF CompTypes(par.type, x.type, varpar) THEN
      IF ~varpar THEN CG.ValueParam(x)
      ELSE (*par.class = Par*)
        IF ~par.rdo THEN CheckReadOnly(x) END;
        CG.VarParam(x, par.type)
      END
    ELSIF (x.type.form = CB.Array) & (par.type.form = CB.Array) &
        (x.type.base = par.type.base) & (par.type.len < 0) THEN
      IF ~par.rdo THEN CheckReadOnly(x) END;
      CG.OpenArrayParam(x)
    ELSIF (x.type.form = CB.String) & varpar & par.rdo & (par.type.form = CB.Array) &
        (par.type.base.form = CB.Char) & (par.type.len < 0) THEN CG.StringParam(x)
    ELSIF ~varpar & (par.type.form = CB.Int) & (x.type.form = CB.Int) THEN CG.ValueParam(x)  (*BYTE*)
    ELSIF (x.type.form = CB.String) & (x.b = 2) & (par.class = CB.Var) & (par.type.form = CB.Char) THEN
      CG.StrToChar(x);  CG.ValueParam(x)
    ELSIF (par.type.form = CB.Array) & (par.type.base = CB.byteType) &
        (par.type.len >= 0) & (par.type.size = x.type.size) THEN
      CG.VarParam(x, par.type)
    ELSE CS.Mark("incompatible parameters")
    END
  END
END Parameter;

PROCEDURE ParamList(VAR x: CG.Item);
VAR n: INTEGER;  par: CB.Object;
BEGIN par := x.type.dsc;  n := 0;
  IF sym # CS.rparen THEN
    Parameter(par);  n := 1;
    WHILE sym <= CS.comma DO
      Check(CS.comma, "comma?");
      IF par # NIL THEN par := par.next END;
      INC(n);  Parameter(par)
    END;
    Check(CS.rparen, ") missing")
  ELSE CS.Get(sym);
  END;
  IF n < x.type.nofpar THEN CS.Mark("too few params")
  ELSIF n > x.type.nofpar THEN CS.Mark("too many params")
  END
END ParamList;

PROCEDURE StandFunc(VAR x: CG.Item;  fct: INTEGER;  restyp: CB.Type);
VAR y: CG.Item;  n, npar: INTEGER;
BEGIN Check(CS.lparen, "no (");
  npar := fct MOD 10;  fct := fct DIV 10;  expression(x);  n := 1;
  WHILE sym = CS.comma DO CS.Get(sym);  expression(y);  INC(n) END;
  Check(CS.rparen, "no )");
  IF n = npar THEN
    IF fct = 0 THEN (*ABS*)
      IF x.type.form IN {CB.Int, CB.Real} THEN CG.Abs(x);  restyp := x.type ELSE CS.Mark("bad type") END
    ELSIF fct = 1 THEN (*ODD*) CheckInt(x);  CG.Odd(x)
    ELSIF fct = 2 THEN (*FLOOR*) CheckReal(x);  CG.Floor(x)
    ELSIF fct = 3 THEN (*FLT*) CheckInt(x);  CG.Float(x)
    ELSIF fct = 4 THEN (*ORD*)
      IF x.type.form <= CB.Proc THEN CG.Ord(x)
      ELSIF (x.type.form = CB.String) & (x.b = 2) THEN CG.StrToChar(x)
      ELSE CS.Mark("bad type")
      END
    ELSIF fct = 5 THEN (*CHR*) CheckInt(x);  CG.Ord(x)
    ELSIF fct = 6 THEN (*LEN*)
        IF x.type.form = CB.Array THEN CG.Len(x) ELSE CS.Mark("not an array") END
    ELSIF fct IN {7, 8, 9} THEN (*LSL, ASR, ROR*) CheckInt(y);
      IF x.type.form IN {CB.Int, CB.Set} THEN CG.Shift(fct-7, x, y);  restyp := x.type ELSE CS.Mark("bad type") END
    ELSIF fct = 11 THEN (*ADC*) CG.ADC(x, y)
    ELSIF fct = 12 THEN (*SBC*) CG.SBC(x, y)
    ELSIF fct = 13 THEN (*UML*) CG.UML(x, y)
    ELSIF fct = 14 THEN (*BIT*) CheckInt(x);  CheckInt(y);  CG.Bit(x, y)
    ELSIF fct = 15 THEN (*REG*) CheckConst(x);  CheckInt(x);  CG.Register(x)
    ELSIF fct = 16 THEN (*VAL*)
      IF (x.mode= CB.Typ) & (x.type.size <= y.type.size) THEN restyp := x.type;  x := y
      ELSE CS.Mark("casting not allowed")
      END
    ELSIF fct = 17 THEN (*ADR*) CG.Adr(x)
    ELSIF fct = 18 THEN (*SIZE*)
      IF x.mode = CB.Typ THEN CG.MakeConstItem(x, CB.intType, x.type.size)
      ELSE CS.Mark("must be a type")
      END
    ELSIF fct = 19 THEN (*COND*) CheckConst(x);  CheckInt(x);  CG.Condition(x)
    ELSIF fct = 20 THEN (*H*) CheckConst(x);  CheckInt(x);  CG.H(x)
    END;
    x.type := restyp
  ELSE CS.Mark("wrong nof params")
  END
END StandFunc;

PROCEDURE element(VAR x: CG.Item);
VAR y: CG.Item;
BEGIN expression(x);  CheckSetVal(x);
  IF sym = CS.upto THEN CS.Get(sym);  expression(y);  CheckSetVal(y);  CG.Set(x, y)
  ELSE CG.Singleton(x)
  END;
  x.type := CB.setType
END element;

PROCEDURE set(VAR x: CG.Item);
VAR y: CG.Item;
BEGIN
  IF sym >= CS.if THEN
    IF sym # CS.rbrace THEN CS.Mark(" } missing") END;
    CG.MakeConstItem(x, CB.setType, 0) (*empty set*)
  ELSE element(x);
    WHILE (sym < CS.rparen) OR (sym > CS.rbrace) DO
      IF sym = CS.comma THEN CS.Get(sym)
      ELSIF sym # CS.rbrace THEN CS.Mark("missing comma")
      END;
      element(y);  CG.SetOp(CS.plus, x, y)
    END
  END
END set;

PROCEDURE factor(VAR x: CG.Item);
VAR obj: CB.Object;  rx: INTEGER;
BEGIN (*sync*)
  IF (sym < CS.char) OR (sym > CS.ident) THEN CS.Mark("expression expected");
    REPEAT CS.Get(sym) UNTIL (sym >= CS.char) & (sym <= CS.for) OR (sym >= CS.then)
  END;
  IF sym = CS.ident THEN
    qualident(obj);
    IF obj.class = CB.SFunc THEN StandFunc(x, obj.val, obj.type)
    ELSE CG.MakeItem(x, obj, level);  selector(x);
      IF sym = CS.lparen THEN
        CS.Get(sym);
        IF (x.type.form = CB.Proc) & (x.type.base.form # CB.NoTyp) THEN
          CG.PrepCall(x, rx);  ParamList(x);  CG.Call(x, rx);  x.type := x.type.base
        ELSE CS.Mark("not a function");  ParamList(x)
        END
      END
    END
  ELSIF sym = CS.int THEN CG.MakeConstItem(x, CB.intType, CS.ival);  CS.Get(sym)
  ELSIF sym = CS.real THEN CG.MakeRealItem(x, CS.rval);  CS.Get(sym)
  ELSIF sym = CS.char THEN CG.MakeConstItem(x, CB.charType, CS.ival);  CS.Get(sym)
  ELSIF sym = CS.nil THEN CS.Get(sym);  CG.MakeConstItem(x, CB.nilType, 0)
  ELSIF sym = CS.string THEN CG.MakeStringItem(x, CS.slen);  CS.Get(sym)
  ELSIF sym = CS.lparen THEN CS.Get(sym);  expression(x);  Check(CS.rparen, "no )")
  ELSIF sym = CS.lbrace THEN CS.Get(sym);  set(x);  Check(CS.rbrace, "no }")
  ELSIF sym = CS.not THEN CS.Get(sym);  factor(x);  CheckBool(x);  CG.Not(x)
  ELSIF sym = CS.false THEN CS.Get(sym);  CG.MakeConstItem(x, CB.boolType, 0)
  ELSIF sym = CS.true THEN CS.Get(sym);  CG.MakeConstItem(x, CB.boolType, 1)
  ELSE CS.Mark("not a factor");  CG.MakeConstItem(x, CB.intType, 0)
  END
END factor;

PROCEDURE term(VAR x: CG.Item);
VAR y: CG.Item;  op, f: INTEGER;
BEGIN factor(x);  f := x.type.form;
  WHILE (sym >= CS.times) & (sym <= CS.and) DO
    op := sym;  CS.Get(sym);
    IF op = CS.times THEN
      IF f = CB.Int THEN factor(y);  CheckInt(y);  CG.MulOp(x, y)
      ELSIF f = CB.Real THEN factor(y);  CheckReal(y);  CG.RealOp(op, x, y)
      ELSIF f = CB.Set THEN factor(y);  CheckSet(y);  CG.SetOp(op, x, y)
      ELSE CS.Mark("bad type")
      END
    ELSIF (op = CS.div) OR (op = CS.mod) THEN
      CheckInt(x);  factor(y);  CheckInt(y);  CG.DivOp(op, x, y)
    ELSIF op = CS.rdiv THEN
      IF f = CB.Real THEN factor(y);  CheckReal(y);  CG.RealOp(op, x, y)
      ELSIF f = CB.Set THEN factor(y);  CheckSet(y);  CG.SetOp(op, x, y)
      ELSE CS.Mark("bad type")
      END
    ELSE (*op = and*) CheckBool(x);  CG.And1(x);  factor(y);  CheckBool(y);  CG.And2(x, y)
    END
  END
END term;

PROCEDURE SimpleExpression(VAR x: CG.Item);
VAR y: CG.Item;  op: INTEGER;
BEGIN
  IF sym = CS.minus THEN CS.Get(sym);  term(x);
    IF x.type.form IN {CB.Int, CB.Real, CB.Set} THEN CG.Neg(x) ELSE CheckInt(x) END
  ELSIF sym = CS.plus THEN CS.Get(sym);  term(x);
  ELSE term(x)
  END;
  WHILE (sym >= CS.plus) & (sym <= CS.or) DO
    op := sym;  CS.Get(sym);
    IF op = CS.or THEN CG.Or1(x);  CheckBool(x);  term(y);  CheckBool(y);  CG.Or2(x, y)
    ELSIF x.type.form = CB.Int THEN term(y);  CheckInt(y);  CG.AddOp(op, x, y)
    ELSIF x.type.form = CB.Real THEN term(y);  CheckReal(y);  CG.RealOp(op, x, y)
    ELSE CheckSet(x);  term(y);  CheckSet(y);  CG.SetOp(op, x, y)
    END
  END
END SimpleExpression;

PROCEDURE expression0(VAR x: CG.Item);
VAR y: CG.Item;  obj: CB.Object;  rel, xf, yf: INTEGER;
BEGIN SimpleExpression(x);
  IF (sym >= CS.eql) & (sym <= CS.geq) THEN
    rel := sym;  CS.Get(sym);  SimpleExpression(y);  xf := x.type.form;  yf := y.type.form;
    IF x.type = y.type THEN
      IF (xf IN {CB.Char, CB.Int}) THEN CG.IntRelation(rel, x, y)
      ELSIF xf = CB.Real THEN CG.RealRelation(rel, x, y)
      ELSIF (xf IN {CB.Set, CB.Pointer, CB.Proc, CB.NilTyp, CB.Bool}) THEN
        IF rel <= CS.neq THEN CG.IntRelation(rel, x, y) ELSE CS.Mark("only = or #") END
      ELSIF (xf = CB.Array) & (x.type.base.form = CB.Char) OR (xf = CB.String) THEN
        CG.StringRelation(rel, x, y)
      ELSE CS.Mark("illegal comparison")
      END
    ELSIF (xf IN {CB.Pointer, CB.Proc}) & (yf = CB.NilTyp)
        OR (yf IN {CB.Pointer, CB.Proc}) & (xf = CB.NilTyp) THEN
      IF rel <= CS.neq THEN CG.IntRelation(rel, x,  y) ELSE CS.Mark("only = or #") END
    ELSIF (xf = CB.Pointer) & (yf = CB.Pointer) &
        (IsExtension(x.type.base, y.type.base) OR IsExtension(y.type.base, x.type.base))
        OR (xf = CB.Proc) & (yf = CB.Proc) & EqualSignatures(x.type, y.type) THEN
      IF rel <= CS.neq THEN CG.IntRelation(rel,  x, y) ELSE CS.Mark("only = or #") END
    ELSIF (xf = CB.Array) & (x.type.base.form = CB.Char) &
          ((yf = CB.String) OR (yf = CB.Array) & (y.type.base.form = CB.Char))
        OR (yf = CB.Array) & (y.type.base.form = CB.Char) & (xf = CB.String) THEN
      CG.StringRelation(rel, x, y)
    ELSIF (xf = CB.Char) & (yf = CB.String) & (y.b = 2) THEN
      CG.StrToChar(y);  CG.IntRelation(rel, x, y)
    ELSIF (yf = CB.Char) & (xf = CB.String) & (x.b = 2) THEN
      CG.StrToChar(x);  CG.IntRelation(rel, x, y)
    ELSIF (xf = CB.Int) & (yf = CB.Int) THEN CG.IntRelation(rel,  x, y)  (*BYTE*)
    ELSE CS.Mark("illegal comparison")
    END;
    x.type := CB.boolType
  ELSIF sym = CS.in THEN
    CS.Get(sym);  CheckInt(x);  SimpleExpression(y);  CheckSet(y);  CG.In(x, y) ;
    x.type := CB.boolType
  ELSIF sym = CS.is THEN
    CS.Get(sym);  qualident(obj);  TypeTest(x, obj.type, FALSE) ;
    x.type := CB.boolType
  END
END expression0;

(* statements *)

PROCEDURE StandProc(pno: INTEGER);
VAR nap, npar: INTEGER;  (*nof actual/formal parameters*)
    x, y, z: CG.Item;
BEGIN Check(CS.lparen, "no (");
  npar := pno MOD 10;  pno := pno DIV 10;  expression(x);  nap := 1;
  IF sym = CS.comma THEN
    CS.Get(sym);  expression(y);  nap := 2;  z.type := CB.noType;
    WHILE sym = CS.comma DO CS.Get(sym);  expression(z);  INC(nap) END
  ELSE y.type := CB.noType
  END;
  Check(CS.rparen, "no )");
  IF (npar = nap) OR (pno IN {0, 1}) THEN
    IF pno IN {0, 1} THEN (*INC, DEC*)
      CheckInt(x);  CheckReadOnly(x);
      IF y.type # CB.noType THEN CheckInt(y) END;
      CG.Increment(pno, x, y)
    ELSIF pno IN {2, 3} THEN (*INCL, EXCL*)
      CheckSet(x);  CheckReadOnly(x);  CheckInt(y);  CG.Include(pno-2, x, y)
    ELSIF pno = 4 THEN CheckBool(x);  CG.Assert(x)
    ELSIF pno = 5 THEN(*NEW*) CheckReadOnly(x);
       IF (x.type.form = CB.Pointer) & (x.type.base.form = CB.Record) THEN CG.New(x)
       ELSE CS.Mark("not a pointer to record")
       END
    ELSIF pno = 6 THEN CheckReal(x);  CheckInt(y);  CheckReadOnly(x);  CG.Pack(x, y)
    ELSIF pno = 7 THEN CheckReal(x);  CheckInt(y);  CheckReadOnly(x);  CG.Unpk(x, y)
    ELSIF pno = 8 THEN
      IF x.type.form <= CB.Set THEN CG.Led(x) ELSE CS.Mark("bad type") END
    ELSIF pno = 10 THEN CheckInt(x);  CG.Get(x, y)
    ELSIF pno = 11 THEN CheckInt(x);  CG.Put(x, y)
    ELSIF pno = 12 THEN CheckInt(x);  CheckInt(y);  CheckInt(z);  CG.Copy(x, y, z)
    ELSIF pno = 13 THEN CheckConst(x);  CheckInt(x);  CG.LDPSR(x)
    ELSIF pno = 14 THEN CheckInt(x);  CG.LDREG(x, y)
    END
  ELSE CS.Mark("wrong nof parameters")
  END
END StandProc;

PROCEDURE StatSequence;
VAR obj: CB.Object;
    orgtype: CB.Type;  (*original type of case var*)
    x, y, z, w: CG.Item;
    L0, L1, rx: INTEGER;

  PROCEDURE TypeCase(obj: CB.Object;  VAR x: CG.Item);
  VAR typobj: CB.Object;
  BEGIN
    IF sym = CS.ident THEN
      qualident(typobj);  CG.MakeItem(x, obj, level);
      IF typobj.class # CB.Typ THEN CS.Mark("not a type") END;
      TypeTest(x, typobj.type, FALSE);  obj.type := typobj.type;
      CG.CFJump(x);  Check(CS.colon, ": expected");  StatSequence
    ELSE CG.CFJump(x);  CS.Mark("type id expected")
    END
   END TypeCase;

  PROCEDURE SkipCase;
  BEGIN
    WHILE sym # CS.colon DO CS.Get(sym) END;
    CS.Get(sym);  StatSequence
  END SkipCase;

BEGIN (* StatSequence *)
  REPEAT (*sync*) obj := NIL;
    IF ~((sym >= CS.ident)  & (sym <= CS.for) OR (sym >= CS.semicolon)) THEN
      CS.Mark("statement expected");
      REPEAT CS.Get(sym) UNTIL (sym >= CS.ident)
    END;
    IF sym = CS.ident THEN
      qualident(obj);  CG.MakeItem(x, obj, level);
      IF x.mode = CB.SProc THEN StandProc(obj.val)
      ELSE selector(x);
        IF sym = CS.becomes THEN (*assignment*)
          CS.Get(sym);  CheckReadOnly(x);  expression(y);
          IF CompTypes(x.type, y.type, FALSE) THEN
            IF (x.type.form <= CB.Pointer) OR (x.type.form = CB.Proc) THEN CG.Store(x, y)
            ELSE CG.StoreStruct(x, y)
            END
          ELSIF (x.type.form = CB.Array) & (y.type.form = CB.Array) & (x.type.base = y.type.base) & (y.type.len < 0) THEN
            CG.StoreStruct(x, y)
          ELSIF (x.type.form = CB.Array) & (x.type.base.form = CB.Char) & (y.type.form = CB.String) THEN
            CG.CopyString(x, y)
          ELSIF (x.type.form = CB.Int) & (y.type.form = CB.Int) THEN CG.Store(x, y)  (*BYTE*)
          ELSIF (x.type.form = CB.Char) & (y.type.form = CB.String) & (y.b = 2) THEN
            CG.StrToChar(y);  CG.Store(x, y)
          ELSE CS.Mark("illegal assignment")
          END
        ELSIF sym = CS.eql THEN CS.Mark("should be :=");  CS.Get(sym);  expression(y)
        ELSIF sym = CS.lparen THEN (*procedure call*)
          CS.Get(sym);
          IF (x.type.form = CB.Proc) & (x.type.base.form = CB.NoTyp) THEN
            CG.PrepCall(x, rx);  ParamList(x);  CG.Call(x, rx)
          ELSE CS.Mark("not a procedure");  ParamList(x)
          END
        ELSIF x.type.form = CB.Proc THEN (*procedure call without parameters*)
          IF x.type.nofpar > 0 THEN CS.Mark("missing parameters") END;
          IF x.type.base.form = CB.NoTyp THEN CG.PrepCall(x, rx);  CG.Call(x, rx) ELSE CS.Mark("not a procedure") END
        ELSIF x.mode = CB.Typ THEN CS.Mark("illegal assignment")
        ELSE CS.Mark("not a procedure")
        END
      END
    ELSIF sym = CS.if THEN
      CS.Get(sym);  expression(x);  CheckBool(x);  CG.CFJump(x);
      Check(CS.then, "no THEN");
      StatSequence;  L0 := 0;
      WHILE sym = CS.elsif DO
        CS.Get(sym);  CG.FJump(L0);  CG.Fixup(x);  expression(x);  CheckBool(x);
        CG.CFJump(x);  Check(CS.then, "no THEN");  StatSequence
      END;
      IF sym = CS.else THEN CS.Get(sym);  CG.FJump(L0);  CG.Fixup(x);  StatSequence
      ELSE CG.Fixup(x)
      END;
      CG.FixLink(L0);  Check(CS.end, "no END")
    ELSIF sym = CS.while THEN
      CS.Get(sym);  L0 := CG.Here();  expression(x);  CheckBool(x);  CG.CFJump(x);
      Check(CS.do, "no DO");  StatSequence;  CG.BJump(L0);
      WHILE sym = CS.elsif DO
        CS.Get(sym);  CG.Fixup(x);  expression(x);  CheckBool(x);  CG.CFJump(x);
        Check(CS.do, "no DO");  StatSequence;  CG.BJump(L0)
      END;
      CG.Fixup(x);  Check(CS.end, "no END")
    ELSIF sym = CS.repeat THEN
      CS.Get(sym);  L0 := CG.Here();  StatSequence;
      IF sym = CS.until THEN
        CS.Get(sym);  expression(x);  CheckBool(x);  CG.CBJump(x, L0)
      ELSE CS.Mark("missing UNTIL")
      END
    ELSIF sym = CS.for THEN
      CS.Get(sym);
      IF sym = CS.ident THEN
        qualident(obj);  CG.MakeItem(x, obj, level);  CheckInt(x);  CheckReadOnly(x);
        IF sym = CS.becomes THEN
          CS.Get(sym);  expression(y);  CheckInt(y);  CG.For0(x, y);  L0 := CG.Here();
          Check(CS.to, "no TO");  expression(z);  CheckInt(z);  obj.rdo := TRUE;
          IF sym = CS.by THEN CS.Get(sym);  expression(w);  CheckConst(w);  CheckInt(w)
          ELSE CG.MakeConstItem(w, CB.intType, 1)
          END;
          Check(CS.do, "no DO");  CG.For1(x, y, z, w, L1);
          StatSequence;  Check(CS.end, "no END");
          CG.For2(x, y, w);  CG.BJump(L0);  CG.FixLink(L1);  obj.rdo := FALSE
        ELSE CS.Mark(":= expected")
        END
      ELSE CS.Mark("identifier expected")
      END
    ELSIF sym = CS.case THEN
      CS.Get(sym);
      IF sym = CS.ident THEN
        qualident(obj);  orgtype := obj.type;
        IF (orgtype.form = CB.Pointer) OR (orgtype.form = CB.Record) & (obj.class = CB.Par) THEN
          Check(CS.of, "OF expected");  TypeCase(obj, x);  L0 := 0;
          WHILE sym = CS.bar DO
            CS.Get(sym);  CG.FJump(L0);  CG.Fixup(x);  obj.type := orgtype;  TypeCase(obj, x)
          END;
          CG.Fixup(x);  CG.FixLink(L0);  obj.type := orgtype
        ELSE CS.Mark("numeric case not implemented");
          Check(CS.of, "OF expected");  SkipCase;
          WHILE sym = CS.bar DO SkipCase END
        END
      ELSE CS.Mark("ident expected")
      END;
      Check(CS.end, "no END")
    END;
    CG.CheckRegs;
    IF sym = CS.semicolon THEN CS.Get(sym)
    ELSIF sym < CS.semicolon THEN CS.Mark("missing semicolon?")
    END
  UNTIL sym > CS.semicolon
END StatSequence;

(* Types and declarations *)

PROCEDURE IdentList(class: INTEGER;  VAR first: CB.Object);
VAR obj: CB.Object;
BEGIN
  IF sym = CS.ident THEN
    CB.NewObj(first, CS.id, class);  CS.Get(sym);  CheckExport(first.expo);
    WHILE sym = CS.comma DO
      CS.Get(sym);
      IF sym = CS.ident THEN CB.NewObj(obj, CS.id, class);  CS.Get(sym);  CheckExport(obj.expo)
      ELSE CS.Mark("ident?")
      END
    END;
    IF sym = CS.colon THEN CS.Get(sym) ELSE CS.Mark(":?") END
  ELSE first := NIL
  END
END IdentList;

PROCEDURE ArrayType(VAR type: CB.Type);
VAR x: CG.Item;  typ: CB.Type;  len: INTEGER;
BEGIN NEW(typ);  typ.form := CB.NoTyp;
  expression(x);
  IF (x.mode = CB.Const) & (x.type.form = CB.Int) & (x.a >= 0) THEN len := x.a
  ELSE len := 1;  CS.Mark("not a valid length")
  END;
  IF sym = CS.of THEN CS.Get(sym);  Type(typ.base);
    IF (typ.base.form = CB.Array) & (typ.base.len < 0) THEN CS.Mark("dyn array not allowed") END
  ELSIF sym = CS.comma THEN CS.Get(sym);  ArrayType(typ.base)
  ELSE CS.Mark("missing OF");  typ.base := CB.intType
  END;
  typ.size := (len * typ.base.size + 3) DIV 4 * 4;
  typ.form := CB.Array;  typ.len := len;  type := typ
END ArrayType;

PROCEDURE RecordType(VAR type: CB.Type);
VAR obj, obj0, new, bot, base: CB.Object;
    typ, tp: CB.Type;
    offset, off, n: INTEGER;
BEGIN NEW(typ);  typ.form := CB.NoTyp;  typ.base := NIL;  typ.mno := -level;  typ.nofpar := 0;  offset := 0;  bot := NIL;
  IF sym = CS.lparen THEN
    CS.Get(sym);  (*record extension*)
    IF level # 0 THEN CS.Mark("extension of local types not implemented") END;
    IF sym = CS.ident THEN
      qualident(base);
      IF base.class = CB.Typ THEN
        IF base.type.form = CB.Record THEN typ.base := base.type
        ELSE typ.base := CB.intType;  CS.Mark("invalid extension")
        END;
        typ.nofpar := typ.base.nofpar + 1;  (*"nofpar" here abused for extension level*)
        bot := typ.base.dsc;  offset := typ.base.size
      ELSE CS.Mark("type expected")
      END
    ELSE CS.Mark("ident expected")
    END;
    Check(CS.rparen, "no )")
  END;
  WHILE sym = CS.ident DO  (*fields*)
    n := 0;  obj := bot;
    WHILE sym = CS.ident DO
      obj0 := obj;
      WHILE (obj0 # NIL) & (obj0.name # CS.id) DO obj0 := obj0.next END;
      IF obj0 # NIL THEN CS.Mark("mult def") END;
      NEW(new);  CS.CopyId(new.name);  new.class := CB.Fld;  new.next := obj;  obj := new;  INC(n);
      CS.Get(sym);  CheckExport(new.expo);
      IF (sym # CS.comma) & (sym # CS.colon) THEN CS.Mark("comma expected")
      ELSIF sym = CS.comma THEN CS.Get(sym)
      END
    END;
    Check(CS.colon, "colon expected");  Type(tp);
    IF (tp.form = CB.Array) & (tp.len < 0) THEN CS.Mark("dyn array not allowed") END;
    IF tp.size > 1 THEN offset := (offset+3) DIV 4 * 4 END;
    offset := offset + n * tp.size;  off := offset;  obj0 := obj;
    WHILE obj0 # bot DO obj0.type := tp;  obj0.lev := 0;  off := off - tp.size;  obj0.val := off;  obj0 := obj0.next END;
    bot := obj;
    IF sym = CS.semicolon THEN CS.Get(sym) ELSIF sym # CS.end THEN CS.Mark(" ;  or END") END
  END;
  typ.form := CB.Record;  typ.dsc := bot;  typ.size := (offset + 3) DIV 4 * 4;  type := typ
END RecordType;

PROCEDURE FPSection(VAR adr: INTEGER;  VAR nofpar: INTEGER);
VAR obj, first: CB.Object;  tp: CB.Type;
    parsize: INTEGER;  cl: INTEGER;  rdo: BOOLEAN;
BEGIN
  IF sym = CS.var THEN CS.Get(sym);  cl := CB.Par ELSE cl := CB.Var END;
  IdentList(cl, first);  FormalType(tp, 0);  rdo := FALSE;
  IF (cl = CB.Var) & (tp.form >= CB.Array) THEN cl := CB.Par;  rdo := TRUE END;
  IF (tp.form = CB.Array) & (tp.len < 0) OR (tp.form = CB.Record) THEN
    parsize := 2*CG.WordSize  (*open array or record, needs second word for length or type tag*)
  ELSE parsize := CG.WordSize
  END;
  obj := first;
  WHILE obj # NIL DO
    INC(nofpar);  obj.class := cl;  obj.type := tp;  obj.rdo := rdo;  obj.lev := level;  obj.val := adr;
    adr := adr + parsize;  obj := obj.next
  END;
  IF adr >= 52 THEN CS.Mark("too many parameters") END
END FPSection;

PROCEDURE ProcedureType(ptype: CB.Type;  VAR parblksize: INTEGER);
VAR obj: CB.Object;  size: INTEGER;  nofpar: INTEGER;
BEGIN ptype.base := CB.noType;  size := parblksize;  nofpar := 0;  ptype.dsc := NIL;
  IF sym = CS.lparen THEN
    CS.Get(sym);
    IF sym = CS.rparen THEN CS.Get(sym)
    ELSE FPSection(size, nofpar);
      WHILE sym = CS.semicolon DO CS.Get(sym);  FPSection(size, nofpar) END;
      Check(CS.rparen, "no )")
    END;
    IF sym = CS.colon THEN  (*function*)
      CS.Get(sym);
      IF sym = CS.ident THEN
        qualident(obj);  ptype.base := obj.type;
        IF ~((obj.class = CB.Typ) & (obj.type.form IN {CB.Byte .. CB.Pointer, CB.Proc})) THEN
          CS.Mark("illegal function type")
        END
      ELSE CS.Mark("type identifier expected")
      END
    END
  END;
  ptype.nofpar := nofpar;  parblksize := size
END ProcedureType;

PROCEDURE FormalType0(VAR typ: CB.Type;  dim: INTEGER);
VAR obj: CB.Object;  dmy: INTEGER;
BEGIN
  IF sym = CS.ident THEN
    qualident(obj);
    IF obj.class = CB.Typ THEN typ := obj.type ELSE CS.Mark("not a type");  typ := CB.intType END
  ELSIF sym = CS.array THEN
    CS.Get(sym);  Check(CS.of, "OF ?");
    IF dim >= 1 THEN CS.Mark("multi-dimensional open arrays not implemented") END;
    NEW(typ);  typ.form := CB.Array;  typ.len := -1;  typ.size := 2*CG.WordSize;
    FormalType(typ.base, dim+1)
  ELSIF sym = CS.procedure THEN
    CS.Get(sym);  CB.OpenScope;
    NEW(typ);  typ.form := CB.Proc;  typ.size := CG.WordSize;  dmy := 0;  ProcedureType(typ, dmy);
    typ.dsc := CB.topScope.next;  CB.CloseScope
  ELSE CS.Mark("identifier expected");  typ := CB.noType
  END
END FormalType0;

PROCEDURE CheckRecLevel(lev: INTEGER);
BEGIN
  IF lev # 0 THEN CS.Mark("ptr base must be global") END
END CheckRecLevel;

PROCEDURE Type0(VAR type: CB.Type);
VAR dmy: INTEGER;  obj: CB.Object;  ptbase: PtrBase;
BEGIN type := CB.intType;  (*sync*)
  IF (sym # CS.ident) & (sym < CS.array) THEN CS.Mark("not a type");
    REPEAT CS.Get(sym) UNTIL (sym = CS.ident) OR (sym >= CS.array)
  END;
  IF sym = CS.ident THEN
    qualident(obj);
    IF obj.class = CB.Typ THEN
      IF (obj.type # NIL) & (obj.type.form # CB.NoTyp) THEN type := obj.type END
    ELSE CS.Mark("not a type or undefined")
    END
  ELSIF sym = CS.array THEN CS.Get(sym);  ArrayType(type)
  ELSIF sym = CS.record THEN
    CS.Get(sym);  RecordType(type);  Check(CS.end, "no END")
  ELSIF sym = CS.pointer THEN
    CS.Get(sym);  Check(CS.to, "no TO");
    NEW(type);  type.form := CB.Pointer;  type.size := CG.WordSize;  type.base := CB.intType;
    IF sym = CS.ident THEN
      obj := CB.thisObj();
      IF obj # NIL THEN
        IF (obj.class = CB.Typ) & (obj.type.form IN {CB.Record, CB.NoTyp}) THEN
          CheckRecLevel(obj.lev);  type.base := obj.type
        ELSIF obj.class = CB.Mod THEN CS.Mark("external base type not implemented")
        ELSE CS.Mark("no valid base type")
        END
      ELSE CheckRecLevel(level);  (*enter into list of forward references to be fixed in Declarations*)
        NEW(ptbase);  CS.CopyId(ptbase.name);  ptbase.type := type;  ptbase.next := pbsList;  pbsList := ptbase
      END;
      CS.Get(sym)
    ELSE Type(type.base);
      IF (type.base.form # CB.Record) OR (type.base.typobj = NIL) THEN CS.Mark("must point to named record") END;
      CheckRecLevel(level)
    END
  ELSIF sym = CS.procedure THEN
    CS.Get(sym);  CB.OpenScope;
    NEW(type);  type.form := CB.Proc;  type.size := CG.WordSize;  dmy := 0;
    ProcedureType(type, dmy);  type.dsc := CB.topScope.next;  CB.CloseScope
  ELSE CS.Mark("illegal type")
  END
END Type0;

PROCEDURE Declarations(VAR varsize: INTEGER);
VAR obj, first: CB.Object;
    x: CG.Item;  tp: CB.Type;  ptbase: PtrBase;
    expo: BOOLEAN;  id: CS.Ident;
BEGIN (*sync*) pbsList := NIL;
  IF (sym < CS.const) & (sym # CS.end) & (sym # CS.return) THEN CS.Mark("declaration?");
    REPEAT CS.Get(sym) UNTIL (sym >= CS.const) OR (sym = CS.end) OR (sym = CS.return)
  END;
  IF sym = CS.const THEN
    CS.Get(sym);
    WHILE sym = CS.ident DO
      CS.CopyId(id);  CS.Get(sym);  CheckExport(expo);
      IF sym = CS.eql THEN CS.Get(sym) ELSE CS.Mark("= ?") END;
      expression(x);
      IF (x.type.form = CB.String) & (x.b = 2) THEN CG.StrToChar(x) END;
      CB.NewObj(obj, id, CB.Const);  obj.expo := expo;
      IF x.mode = CB.Const THEN obj.val := x.a;  obj.lev := x.b;  obj.type := x.type
      ELSE CS.Mark("expression not constant");  obj.type := CB.intType
      END;
      Check(CS.semicolon, ";  missing")
    END
  END;
  IF sym = CS.type THEN
    CS.Get(sym);
    WHILE sym = CS.ident DO
      CS.CopyId(id);  CS.Get(sym);  CheckExport(expo);
      IF sym = CS.eql THEN CS.Get(sym) ELSE CS.Mark("=?") END;
      Type(tp);
      CB.NewObj(obj, id, CB.Typ);  obj.type := tp;  obj.expo := expo;  obj.lev := level;
      IF tp.typobj = NIL THEN tp.typobj := obj END;
      IF expo & (obj.type.form = CB.Record) THEN obj.exno := exno;  INC(exno) ELSE obj.exno := 0 END;
      IF tp.form = CB.Record THEN
        ptbase := pbsList;  (*check whether this is base of a pointer type;  search and fixup*)
        WHILE ptbase # NIL DO
          IF obj.name = ptbase.name THEN ptbase.type.base := obj.type END;
          ptbase := ptbase.next
        END;
        IF level = 0 THEN CG.BuildTD(tp, dc) END    (*type descriptor;  len used as its address*)
      END;
      Check(CS.semicolon, ";  missing")
    END
  END;
  IF sym = CS.var THEN
    CS.Get(sym);
    WHILE sym = CS.ident DO
      IdentList(CB.Var, first);  Type(tp);
      obj := first;
      WHILE obj # NIL DO
        obj.type := tp;  obj.lev := level;
        IF tp.size > 1 THEN varsize := (varsize + 3) DIV 4 * 4 (*align*) END;
        obj.val := varsize;  varsize := varsize + obj.type.size;
        IF obj.expo THEN obj.exno := exno;  INC(exno) END;
        obj := obj.next
      END;
      Check(CS.semicolon, ";  missing")
    END
  END;
  varsize := (varsize + 3) DIV 4 * 4;
  ptbase := pbsList;
  WHILE ptbase # NIL DO
    IF ptbase.type.base.form = CB.Int THEN CS.Mark("undefined pointer base of") END;
    ptbase := ptbase.next
  END;
  IF (sym >= CS.const) & (sym <= CS.var) THEN CS.Mark("declaration in bad order") END
END Declarations;

PROCEDURE ProcedureDecl;
VAR proc: CB.Object;
    type: CB.Type;
    procid: CS.Ident;
    x: CG.Item;
    locblksize, parblksize, L: INTEGER;
    int: BOOLEAN;
BEGIN (* ProcedureDecl *) int := FALSE;  CS.Get(sym);
  IF sym = CS.times THEN CS.Get(sym);  int := TRUE END;
  IF sym = CS.ident THEN
    CS.CopyId(procid);  CS.Get(sym);
    CB.NewObj(proc, CS.id, CB.Const);
    IF int THEN parblksize := 12 ELSE parblksize := 4 END;
    NEW(type);  type.form := CB.Proc;  type.size := CG.WordSize;
    proc.type := type;  proc.val := -1;  proc.lev := level;
    CheckExport(proc.expo);
    IF proc.expo THEN proc.exno := exno;  INC(exno) END;
    CB.OpenScope;  INC(level);  type.base := CB.noType;
    ProcedureType(type, parblksize);  (*formal parameter list*)
    Check(CS.semicolon, "no ;");  locblksize := parblksize;
    Declarations(locblksize);
    proc.val := CG.Here() * 4;  proc.type.dsc := CB.topScope.next;
    IF sym = CS.procedure THEN
      L := 0;  CG.FJump(L);
      REPEAT ProcedureDecl;  Check(CS.semicolon, "no ;") UNTIL sym # CS.procedure;
      CG.FixOne(L);  proc.val := CG.Here() * 4;  proc.type.dsc := CB.topScope.next
    END;
    CG.Enter(parblksize, locblksize, int);
    IF sym = CS.begin THEN CS.Get(sym);  StatSequence END;
    IF sym = CS.return THEN
      CS.Get(sym);  expression(x);
      IF type.base = CB.noType THEN CS.Mark("this is not a function")
      ELSIF ~CompTypes(type.base, x.type, FALSE) THEN CS.Mark("wrong result type")
      END
    ELSIF type.base.form # CB.NoTyp THEN
      CS.Mark("function without result");  type.base := CB.noType
    END;
    CG.Return(type.base.form, x, locblksize, int);
    CB.CloseScope;  DEC(level);  Check(CS.end, "no END");
    IF sym = CS.ident THEN
      IF CS.id # procid THEN CS.Mark("no match") END;
      CS.Get(sym)
    ELSE CS.Mark("no proc id")
    END
  ELSE CS.Mark("proc id expected")
  END
END ProcedureDecl;

PROCEDURE Import;
VAR impid, impid1: CS.Ident;
BEGIN
  IF sym = CS.ident THEN
    CS.CopyId(impid);  CS.Get(sym);
    IF sym = CS.becomes THEN
      CS.Get(sym);
      IF sym = CS.ident THEN CS.CopyId(impid1);  CS.Get(sym)
      ELSE CS.Mark("id expected");  impid1 := impid
      END
    ELSE impid1 := impid
    END;
    CB.Import(impid, impid1)
  ELSE CS.Mark("id expected")
  END
END Import;

PROCEDURE Module;
VAR key: INTEGER;
BEGIN Texts.WriteString(W, "  compiling ");  CS.Get(sym);
  IF sym = CS.module THEN
    CS.Get(sym);
    IF sym = CS.times THEN version := 0;  dc := 8;  Texts.Write(W, "*");  CS.Get(sym) ELSE dc := 0;  version := 1 END;
    CB.Init;  CB.OpenScope;
    IF sym = CS.ident THEN
      CS.CopyId(modid);  CS.Get(sym);
      Texts.WriteString(W, modid);  Texts.Append(Oberon.Log, W.buf)
    ELSE CS.Mark("identifier expected")
    END;
    Check(CS.semicolon, "no ;");  level := 0;  exno := 1;  key := 0;
    IF sym = CS.import THEN
      CS.Get(sym);  Import;
      WHILE sym = CS.comma DO CS.Get(sym);  Import END;
      Check(CS.semicolon, ";  missing")
    END;
    CG.Open(version);  Declarations(dc);  CG.SetDataSize((dc + 3) DIV 4 * 4);
    WHILE sym = CS.procedure DO ProcedureDecl;  Check(CS.semicolon, "no ;") END;
    CG.Header;
    IF sym = CS.begin THEN CS.Get(sym);  StatSequence END;
    Check(CS.end, "no END");
    IF sym = CS.ident THEN
      IF CS.id # modid THEN CS.Mark("no match") END;
      CS.Get(sym)
    ELSE CS.Mark("identifier missing")
    END;
    IF sym # CS.period THEN CS.Mark("period missing") END;
    IF (CS.errcnt = 0) & (version # 0) THEN
      CB.Export(modid, newSF, key);
      IF newSF THEN Texts.WriteString(W, " new symbol file") END
    END;
    IF CS.errcnt = 0 THEN
      CG.Close(modid, key, exno);
      Texts.WriteInt(W, CG.pc, 6);  Texts.WriteInt(W, dc, 6);  Texts.WriteHex(W, key)
    ELSE Texts.WriteLn(W);  Texts.WriteString(W, "compilation FAILED")
    END;
    Texts.WriteLn(W);  Texts.Append(Oberon.Log, W.buf);
    CB.CloseScope;  pbsList := NIL
  ELSE CS.Mark("must start with MODULE")
  END
END Module;

PROCEDURE Option(VAR S: Texts.Scanner);
BEGIN newSF := FALSE;
  IF S.nextCh = "/" THEN
    Texts.Scan(S);  Texts.Scan(S);
    IF (S.class = Texts.Name) & (S.s[0] = "s") THEN newSF := TRUE END
  END
END Option;

PROCEDURE Compile*;
VAR beg, end, time: INTEGER;
    T: Texts.Text;
    S: Texts.Scanner;
BEGIN Texts.OpenScanner(S, Oberon.Par.text, Oberon.Par.pos);  Texts.Scan(S);
  IF S.class = Texts.Char THEN
    IF S.c = "@" THEN
      Option(S);  Oberon.GetSelection(T, beg, end, time);
      IF time >= 0 THEN CS.Init(T, beg);  Module END
    ELSIF S.c = "^" THEN
      Option(S);  Oberon.GetSelection(T, beg, end, time);
      IF time >= 0 THEN
        Texts.OpenScanner(S, T, beg);  Texts.Scan(S);
        IF S.class = Texts.Name THEN
          Texts.WriteString(W, S.s);  NEW(T);  Texts.Open(T, S.s);
          IF T.len > 0 THEN CS.Init(T, 0);  Module END
        END
      END
    END
  ELSE
    WHILE S.class = Texts.Name DO
      NEW(T);  Texts.Open(T, S.s);
      IF T.len > 0 THEN Option(S);  CS.Init(T, 0);  Module
      ELSE Texts.WriteString(W, S.s);  Texts.WriteString(W, " not found");
        Texts.WriteLn(W);  Texts.Append(Oberon.Log, W.buf)
      END;
      IF (T.len # 0) & (CS.errcnt = 0) THEN Texts.Scan(S) ELSE S.class := 0 END
    END
  END;
  Oberon.Collect(0)
END Compile;

BEGIN Texts.OpenWriter(W);  Texts.WriteString(W, "OR Compiler  8.3.2020");
  Texts.WriteLn(W);  Texts.Append(Oberon.Log, W.buf);
  NEW(dummy);  dummy.class := CB.Var;  dummy.type := CB.intType;
  expression := expression0;  Type := Type0;  FormalType := FormalType0
END CompilerParser.
