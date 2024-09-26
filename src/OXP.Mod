MODULE OXP;  (* DCWB 2024-08-10 *)
(* Modified from N. Wirth 1.7.97 / 8.3.2020  Oberon compiler for RISC in Oberon-07*)

IMPORT SYSTEM, H := Host, Kernel, Files, Modules, Texts, ORS, OXB, OXG;

  (*Original author: Niklaus Wirth, 2014.
    Parser of Oberon-RISC compiler. Uses Scanner ORS to obtain symbols (tokens),
    OXB for definition of data structures and for handling import and export, and
    ORG to produce binary code. OXP performs type checking and data allocation.
    Parser is target-independent, except for part of the handling of allocations.*)

TYPE
  PtrBase = POINTER TO PtrBaseDesc;
  PtrBaseDesc = RECORD  (*list of names of pointer base types*)
    name: ORS.Ident; type: OXB.Type; next: PtrBase
  END;

VAR
  sym:         INTEGER;    (* last symbol read *)
  VarSize:     INTEGER;    (* Global VAR size *)
  Level:       INTEGER;    (* Procedure nesting level *)
  newSF:       BOOLEAN;    (* option flag *)
  expression:  PROCEDURE(VAR x: OXG.Argument);  (* to avoid forward reference *)
  Type:        PROCEDURE(VAR type: OXB.Type);
  FormalType:  PROCEDURE(VAR typ: OXB.Type;  dim: INTEGER);
  modid:       ORS.Ident;
  pbsList:     PtrBase;    (* list of names of pointer base types *)
  dummy:       OXB.Object;
  W:           Texts.Writer;


PROCEDURE wn; BEGIN Texts.WriteLn(W); Texts.Append(Texts.Log, W.buf) END wn;
PROCEDURE ws (s: ARRAY OF CHAR); BEGIN Texts.WriteString(W, s)     END ws;
PROCEDURE wsn(s: ARRAY OF CHAR); BEGIN Texts.WriteString(W, s); wn END wsn;
PROCEDURE wi (i: INTEGER);       BEGIN Texts.WriteInt(W, i, 1)     END wi;


PROCEDURE Check(s: INTEGER;  msg: ARRAY OF CHAR);
BEGIN
  IF sym = s THEN ORS.Get(sym) ELSE ORS.Mark(msg) END
END Check;

PROCEDURE qualident(VAR obj: OXB.Object);
BEGIN obj := OXB.thisObj();  ORS.Get(sym);
  IF obj = NIL THEN ORS.Mark("undef");  obj := dummy END;
  IF (sym = ORS.period) & (obj.class = OXB.Mod) THEN
    ORS.Get(sym);
    IF sym = ORS.ident THEN obj := OXB.thisimport(obj);  ORS.Get(sym);
      IF obj = NIL THEN ORS.Mark("undef");  obj := dummy END
    ELSE ORS.Mark("identifier expected");  obj := dummy
    END
  END
END qualident;

PROCEDURE CheckBool(VAR x: OXG.Argument);
BEGIN
  IF ~((x IS OXG.ConditionArg) OR (x.type.form = OXB.Bool)) THEN
    ORS.Mark("not Boolean");  x.type := OXB.boolType
  END
END CheckBool;

PROCEDURE CheckInt(VAR x: OXG.Argument);
BEGIN
  IF x.type.form # OXB.Int64 THEN ORS.Mark("not Integer");  x.type := OXB.intType END
END CheckInt;

PROCEDURE CheckReal(VAR x: OXG.Argument);
BEGIN
  IF x.type.form # OXB.Real THEN ORS.Mark("not Real");  x.type := OXB.realType END
END CheckReal;

PROCEDURE CheckSet(VAR x: OXG.Argument);
BEGIN
  IF x.type.form # OXB.Set THEN ORS.Mark("not Set");  x.type := OXB.setType END
END CheckSet;

PROCEDURE CheckSetVal(VAR x: OXG.Argument);
BEGIN
  IF x.type.form # OXB.Int64 THEN
    ORS.Mark("not Int");  x.type := OXB.setType
  ELSIF x.class = OXB.Const THEN
    IF (x.val < 0) OR (x.val >= 64) THEN ORS.Mark("invalid set") END
  END
END CheckSetVal;

PROCEDURE CheckConst(VAR x: OXG.Argument);
BEGIN
  IF ~((x.class = OXB.Const) OR (x.type.form IN {OXB.ProcAdr, OXB.StrAdr})) THEN
    ORS.Mark("not a constant");  x := OXG.NewArg(OXB.Const, 0, 0, 0, OXB.intType);
  END
END CheckConst;

PROCEDURE CheckReadOnly(VAR x: OXG.Argument);
BEGIN
  IF x.rdo THEN ORS.Mark("read-only") END
END CheckReadOnly;

PROCEDURE CheckExport(VAR expo: BOOLEAN);
BEGIN
  IF sym = ORS.times THEN
    expo := TRUE;  ORS.Get(sym);
    IF Level # 0 THEN ORS.Mark("export must be global") END
  ELSE expo := FALSE
  END
END CheckExport;

PROCEDURE CheckHosted(VAR hosted: BYTE; allowed: SET);
BEGIN
  hosted := OXB.Nohost;
  IF ~OXB.SystemImported() THEN allowed := {} END;
  IF sym IN {ORS.rdiv, ORS.bslash, ORS.neq, ORS.minus} THEN
    IF    sym = ORS.rdiv   THEN hosted := OXB.LinABI
    ELSIF sym = ORS.neq    THEN hosted := OXB.Syscall
    ELSIF sym = ORS.bslash THEN hosted := OXB.WinABI
    ELSE                        hosted := OXB.Nogc
    END;
    ORS.Get(sym);
    IF ~(hosted IN allowed) THEN
      IF    hosted = OXB.WinABI  THEN ORS.Mark("'\' Windows procedure mark not allowed here")
      ELSIF hosted = OXB.LinABI  THEN ORS.Mark("'/' Linux procedure mark not allowed here")
      ELSIF hosted = OXB.Syscall THEN ORS.Mark("'#' Linux syscall procedure mark not allowed here")
      ELSE                            ORS.Mark("'-' No-garbage-collection mark not allowed here")
      END
    END;
    IF ~OXB.SystemImported() THEN ORS.Mark("Mark requires importing SYSTEM.") END
  END
END CheckHosted;


PROCEDURE IsExtension(t0, t1: OXB.Type): BOOLEAN;
BEGIN (*t1 is an extension of t0*)
  RETURN (t0 = t1) OR (t1 # NIL) & IsExtension(t0, t1.base)
END IsExtension;

(* expressions *)

PROCEDURE TypeTest(VAR x: OXG.Argument;  T: OXB.Type;  guard: BOOLEAN);
VAR xt: OXB.Type;
BEGIN xt := x.type;
  IF (T.form = xt.form )
   & (   (T.form = OXB.Pointer)
      OR (T.form = OXB.Record) & (x.class = OXB.RefPar)) THEN
    WHILE (xt # T) & (xt # NIL) DO xt := xt.base END;
    IF xt # T THEN xt := x.type;
      IF xt.form = OXB.Pointer THEN
        IF IsExtension(xt.base, T.base) THEN OXG.TypeTest(x, T.base, FALSE, guard); x.type := T
        ELSE ORS.Mark("not an extension")
        END
      ELSIF (xt.form = OXB.Record) & (x.class = OXB.RefPar) THEN
        IF IsExtension(xt, T) THEN OXG.TypeTest(x, T, TRUE, guard);  x.type := T
        ELSE ORS.Mark("not an extension")
        END
      ELSE ORS.Mark("incompatible types")
      END
    ELSIF ~guard THEN OXG.TypeTest(x, NIL, FALSE, FALSE)
    END
  ELSE ORS.Mark("type mismatch")
  END;
  IF ~guard THEN x.type := OXB.boolType END
END TypeTest;

PROCEDURE selector(VAR x: OXG.Argument);
VAR y: OXG.Argument;  obj: OXB.Object;
BEGIN
  WHILE (sym = ORS.lbrak) OR (sym = ORS.period) OR (sym = ORS.arrow)
      OR (sym = ORS.lparen) & (x.type.form IN {OXB.Record, OXB.Pointer}) DO
    IF sym = ORS.lbrak THEN
      REPEAT ORS.Get(sym);  expression(y);
        IF x.type.form = OXB.Array THEN
          CheckInt(y);  OXG.Dyadic(OXG.OpIndex, x, y);  x.type := x.type.base
        ELSE ORS.Mark("not an array")
        END
      UNTIL sym # ORS.comma;
      Check(ORS.rbrak, "no ]")
    ELSIF sym = ORS.period THEN ORS.Get(sym);
      IF sym = ORS.ident THEN
        IF x.type.form = OXB.Pointer THEN OXG.DeRef(x);  x.type := x.type.base END;
        IF x.type.form = OXB.Record THEN
          obj := OXB.thisfield(x.type);  ORS.Get(sym);
          IF obj # NIL THEN OXG.Dyadic(OXG.OpField, x, OXG.MakeArgument(obj, Level));  x.type := obj.type
          ELSE ORS.Mark("undef")
          END
        ELSE ORS.Mark("not a record")
        END
      ELSE ORS.Mark("ident?")
      END
    ELSIF sym = ORS.arrow THEN
      ORS.Get(sym);
      IF x.type.form = OXB.Pointer THEN OXG.DeRef(x);  x.type := x.type.base
      ELSE ORS.Mark("not a pointer")
      END
    ELSIF (sym = ORS.lparen) & (x.type.form IN {OXB.Record, OXB.Pointer}) THEN (*type guard*)
      ORS.Get(sym);
      IF sym = ORS.ident THEN
        qualident(obj);
        IF obj.class = OXB.Typ THEN TypeTest(x, obj.type, TRUE)
        ELSE ORS.Mark("guard type expected")
        END
      ELSE ORS.Mark("not an identifier")
      END;
      Check(ORS.rparen, " ) missing")
    END
  END
END selector;

PROCEDURE EqualSignatures(t0, t1: OXB.Type): BOOLEAN;
VAR p0, p1: OXB.Object;  com: BOOLEAN;
BEGIN com := TRUE;
  IF (t0.base = t1.base) & (t0.nofpar = t1.nofpar) THEN
    p0 := t0.dsc;  p1 := t1.dsc;
    WHILE p0 # NIL DO
      IF (p0.class = p1.class) & (p0.rdo = p1.rdo) &
        ((p0.type = p1.type) OR
        (p0.type.form = OXB.Array) & (p1.type.form = OXB.Array) & (p0.type.len = p1.type.len) & (p0.type.base = p1.type.base) OR
        (p0.type.form = OXB.ProcAdr) & (p1.type.form = OXB.ProcAdr) & EqualSignatures(p0.type, p1.type))
      THEN p0 := p0.next;  p1 := p1.next
      ELSE p0 := NIL;  com := FALSE
      END
    END
  ELSE com := FALSE
  END;
  RETURN com
END EqualSignatures;

PROCEDURE CompTypes(t0, t1: OXB.Type;  varpar: BOOLEAN): BOOLEAN;
BEGIN (*check for assignment compatibility*)
  RETURN (t0 = t1)    (*openarray assignment disallowed in ORG*)
    OR (t0.form = OXB.Array) & (t1.form = OXB.Array) & (t0.base =  t1.base) & (t0.len = t1.len)
    OR (t0.form = OXB.Record) & (t1.form = OXB.Record)  & IsExtension(t0, t1)
    OR ~varpar &
      ((t0.form = OXB.Pointer) & (t1.form = OXB.Pointer)  & IsExtension(t0.base, t1.base)
      OR (t0.form = OXB.ProcAdr) & (t1.form = OXB.ProcAdr) & EqualSignatures(t0, t1)
      OR (t0.form IN {OXB.Pointer, OXB.ProcAdr}) & (t1.form = OXB.NilTyp))
      OR (t0.form = OXB.Int64) & (t1.form = OXB.Int64)  (* All int types are value compatible *)
END CompTypes;

PROCEDURE Parameter(par: OXB.Object;  hosted: BYTE;  n: INTEGER);
VAR x, y: OXG.Argument;  varpar: BOOLEAN;
BEGIN
  expression(x);  y := OXG.MakeArgument(par, Level);
  IF par # NIL THEN
    varpar := par.class = OXB.RefPar;
    IF CompTypes(par.type, x.type, varpar) THEN
      IF ~varpar THEN
        OXG.Dyadic(OXG.OpValueParam, x, y)
      ELSE (*par.class = RefPar*)
        IF ~par.rdo THEN CheckReadOnly(x) END;
        OXG.Dyadic(OXG.OpVarParam, x, y);
      END
    ELSIF (x.type.form = OXB.Array)     & (par.type.form = OXB.Array)
        & (x.type.base = par.type.base) & (par.type.len < 0) THEN
      IF ~par.rdo THEN CheckReadOnly(x) END;
      OXG.Dyadic(OXG.OpOpenArrParam, x, y)
    ELSIF (x.type.form = OXB.StrAdr) & varpar & par.rdo
        & (par.type.form = OXB.Array) & (par.type.base.form = OXB.Char)
        & (par.type.len < 0) THEN
      OXG.Dyadic(OXG.OpStringParam, x, y)
    ELSIF ~varpar & (par.type.form = OXB.Int64) & (x.type.form = OXB.Int64) THEN
      OXG.Dyadic(OXG.OpValueParam, x, y)  (* Integer types interop *)
    ELSIF OXG.IsSingleCharacterString(x)
        & (par.class = OXB.ValPar) & (par.type.form = OXB.Char) THEN
      OXG.StrToChar(x);  OXG.Dyadic(OXG.OpValueParam, x, y)
    ELSIF (par.type.form = OXB.Array) & (par.type.base = OXB.byteType) & (par.type.len < 0) THEN
      IF ~par.rdo THEN CheckReadOnly(x) END;
      OXG.Dyadic(OXG.OpByteArrParam, x, y)  (* Can pass anything to a dynamic array of byte *)
    ELSE
      ORS.Mark("incompatible parameters");
    END
  END
END Parameter;

PROCEDURE ParamList(VAR x: OXG.Argument);
VAR n: INTEGER;  par: OXB.Object;
BEGIN
  ASSERT(x.type # NIL);
  par := x.type.dsc;  n := 0;
  IF sym # ORS.rparen THEN
    Parameter(par, x.type.hosted, n);  n := 1;
    WHILE sym <= ORS.comma DO
      Check(ORS.comma, "comma?");
      IF par # NIL THEN par := par.next END;
      Parameter(par, x.type.hosted, n);  INC(n);
      IF FALSE THEN END;  (* Crashes without this. Haven't found out why.
                             May be to do with true chain, false chain,
                             and jump generation? *)
    END;
    Check(ORS.rparen, ") missing")
  ELSE ORS.Get(sym);
  END;
  IF n < x.type.nofpar THEN    ORS.Mark("too few params")
  ELSIF n > x.type.nofpar THEN ORS.Mark("too many params")
  END
END ParamList;

PROCEDURE StandFunc(VAR x: OXG.Argument;  fct: INTEGER;  restyp: OXB.Type);
VAR y: OXG.Argument;  n, npar: INTEGER;
BEGIN Check(ORS.lparen, "no (");
  npar := fct MOD 10;  fct := fct DIV 10;  expression(x);  n := 1;
  WHILE sym = ORS.comma DO ORS.Get(sym);  expression(y);  INC(n) END;
  Check(ORS.rparen, "no )");
  IF n = npar THEN
    IF    fct = 0 THEN (*ABS*)   IF x.type.form IN {OXB.Int64, OXB.Real} THEN
                                   OXG.Monadic(OXG.OpAbs, x);  restyp := x.type
                                 ELSE ORS.Mark("bad type") END
    ELSIF fct = 1 THEN (*ODD*)   CheckInt(x);   OXG.Monadic(OXG.OpOdd,   x)
    ELSIF fct = 2 THEN (*FLOOR*) CheckReal(x);  OXG.Monadic(OXG.OpFloor, x)
    ELSIF fct = 3 THEN (*FLT*)   CheckInt(x);   OXG.Monadic(OXG.OpFloat, x)
    ELSIF fct = 4 THEN (*ORD*)   IF x.type.form <= OXB.ProcAdr THEN OXG.Monadic(OXG.OpOrd, x)
                                 ELSIF OXG.IsSingleCharacterString(x) THEN OXG.StrToChar(x)
                                 ELSE ORS.Mark("bad type")
                                 END
    ELSIF fct = 5 THEN (*CHR*)   CheckInt(x);  OXG.Monadic(OXG.OpOrd, x)
    ELSIF fct = 6 THEN (*LEN*)   IF x.type.form = OXB.Array THEN OXG.Monadic(OXG.OpLen, x)
                                 ELSE ORS.Mark("not an array") END
    ELSIF fct IN {7, 8, 9} THEN (*LSL, ASR, ROR*)
                                 CheckInt(y);
                                 IF x.type.form IN {OXB.Int64, OXB.Set} THEN
                                   OXG.Dyadic(OXG.OpLSL + fct - 7, x, y);  restyp := x.type
                                 ELSE ORS.Mark("bad type") END
    ELSIF fct = 11 THEN (*ADC*)  OXG.Dyadic(OXG.OpADC, x, y)
    ELSIF fct = 12 THEN (*SBC*)  OXG.Dyadic(OXG.OpSBC, x, y)
    ELSIF fct = 13 THEN (*UML*)  OXG.Dyadic(OXG.OpUML, x, y)
    ELSIF fct = 14 THEN (*BIT*)  CheckInt(x);    CheckInt(y);  OXG.Dyadic(OXG.OpBit, x, y)
    ELSIF fct = 15 THEN (*REG*)  CheckConst(x);  CheckInt(x);  OXG.Monadic(OXG.OpREG, x)
    ELSIF fct = 16 THEN (*VAL*)  IF x.type.size <= y.type.size THEN restyp := x.type;  x := y
                                 ELSE ORS.Mark("casting not allowed") END
    ELSIF fct = 17 THEN (*ADR*)  OXG.Monadic(OXG.OpAdr, x)
    ELSIF fct = 18 THEN (*SIZE*) IF x.class = OXB.Typ THEN x := OXG.NewArg(OXB.Const, x.type.size, 0, 0, OXB.intType)
                                 ELSE ORS.Mark("must be a type") END
    ELSIF fct = 19 THEN (*COND*) CheckConst(x);  CheckInt(x);  OXG.Monadic(OXG.OpCond, x)
    END;
    x.type := restyp
  ELSE ORS.Mark("wrong nof params")
  END
END StandFunc;

PROCEDURE element(VAR x: OXG.Argument);
VAR y: OXG.Argument;
BEGIN expression(x);  CheckSetVal(x);
  IF sym = ORS.upto THEN
    ORS.Get(sym);  expression(y);  CheckSetVal(y);  OXG.Dyadic(OXG.OpSetRange, x, y)
  ELSE
    OXG.Monadic(OXG.OpSetSingleton, x)
  END;
  x.type := OXB.setType
END element;

PROCEDURE set(VAR x: OXG.Argument);
VAR y: OXG.Argument;
BEGIN
  IF sym >= ORS.if THEN
    IF sym # ORS.rbrace THEN ORS.Mark(" } missing") END;
    x := OXG.NewArg(OXB.Const, 0, 0, 0, OXB.setType) (*empty set*)
  ELSE element(x);
    WHILE (sym < ORS.rparen) OR (sym > ORS.rbrace) DO
      IF    sym = ORS.comma  THEN ORS.Get(sym)
      ELSIF sym # ORS.rbrace THEN ORS.Mark("missing comma")
      END;
      element(y);  OXG.Dyadic(OXG.OpSetOr, x, y)
    END
  END
END set;

PROCEDURE factor(VAR x: OXG.Argument);
VAR obj: OXB.Object;  saved: SET;
BEGIN (*sync*)
  IF (sym < ORS.char) OR (sym > ORS.ident) THEN
    ORS.Mark("expression expected");
    REPEAT ORS.Get(sym) UNTIL (sym >= ORS.char) & (sym <= ORS.for) OR (sym >= ORS.then)
  END;
  IF sym = ORS.ident THEN
    qualident(obj);
    IF obj.class = OXB.StdFunc THEN StandFunc(x, obj.val, obj.type)
    ELSE x := OXG.MakeArgument(obj, Level);  selector(x);
      IF sym = ORS.lparen THEN
        ORS.Get(sym);
        IF (x.type.form = OXB.ProcAdr) & (x.type.base.form # OXB.NoTyp) THEN
          OXG.PrepCall(x, saved);
          ParamList(x);
          OXG.Call(x, saved);
          x.type := x.type.base
        ELSE ORS.Mark("not a function");  ParamList(x)
        END
      END
    END
  ELSIF sym = ORS.int    THEN x := OXG.NewArg(OXB.Const, ORS.ival, 0, 0, OXB.intType);  ORS.Get(sym)
  ELSIF sym = ORS.char   THEN x := OXG.NewArg(OXB.Const, ORS.ival, 0, 0, OXB.charType); ORS.Get(sym)
  ELSIF sym = ORS.nil    THEN x := OXG.NewArg(OXB.Const, 0, 0, 0, OXB.nilType);         ORS.Get(sym)
  ELSIF sym = ORS.false  THEN x := OXG.NewArg(OXB.Const, 0, 0, 0, OXB.boolType);        ORS.Get(sym)
  ELSIF sym = ORS.true   THEN x := OXG.NewArg(OXB.Const, 1, 0, 0, OXB.boolType);        ORS.Get(sym)
  ELSIF sym = ORS.real   THEN x := OXG.MakeRealArgument(ORS.rval);                  ORS.Get(sym)
  ELSIF sym = ORS.string THEN x := OXG.MakeStringArgument(ORS.slen);                ORS.Get(sym)
  ELSIF sym = ORS.lparen THEN ORS.Get(sym);  expression(x);  Check(ORS.rparen, "no )")
  ELSIF sym = ORS.lbrace THEN ORS.Get(sym);  set(x);         Check(ORS.rbrace, "no }")
  ELSIF sym = ORS.not    THEN ORS.Get(sym);  factor(x);  CheckBool(x);  OXG.Not(x)
  ELSE ORS.Mark("not a factor");  x := OXG.NewArg(OXB.Const, 0, 0, 0, OXB.intType)
  END
END factor;

PROCEDURE term(VAR x: OXG.Argument);
VAR y: OXG.Argument;  op, f: INTEGER;
BEGIN factor(x); f := x.type.form;
  WHILE (sym >= ORS.times) & (sym <= ORS.and) DO
    op := sym;  ORS.Get(sym);
    IF op = ORS.times THEN
      IF    f = OXB.Int64 THEN factor(y);  CheckInt(y);   OXG.Dyadic(OXG.OpIntMultiply, x, y)
      ELSIF f = OXB.Real  THEN factor(y);  CheckReal(y);  OXG.Dyadic(OXG.OpRealMultiply, x, y)
      ELSIF f = OXB.Set   THEN factor(y);  CheckSet(y);   OXG.Dyadic(OXG.OpSetAnd, x, y)
      ELSE ORS.Mark("bad type")
      END
    ELSIF (op = ORS.div) OR (op = ORS.mod) THEN
      CheckInt(x);  factor(y);  CheckInt(y);  IF op = ORS.div THEN OXG.Dyadic(OXG.OpIntDivide,  x, y)
                                              ELSE                 OXG.Dyadic(OXG.OpIntModulus, x, y)
                                              END
    ELSIF op = ORS.rdiv THEN
      IF    f = OXB.Real THEN factor(y);  CheckReal(y);  OXG.Dyadic(OXG.OpRealDivide, x, y)
      ELSIF f = OXB.Set  THEN factor(y);  CheckSet(y);   OXG.Dyadic(OXG.OpSetXor, x, y)
      ELSE ORS.Mark("bad type")
      END
    ELSE (*op = and*) CheckBool(x);  OXG.And1(x);  factor(y);  CheckBool(y);  OXG.And2(x, y)
    END
  END
END term;

PROCEDURE SimpleExpression(VAR x: OXG.Argument);
VAR y: OXG.Argument;  op: INTEGER;
BEGIN
  IF sym = ORS.minus THEN ORS.Get(sym); term(x);
    IF    x.type.form = OXB.Int64 THEN OXG.Monadic(OXG.OpNegate,     x)
    ELSIF x.type.form = OXB.Real  THEN OXG.Monadic(OXG.OpRealNegate, x)
    ELSIF x.type.form = OXB.Set   THEN OXG.Monadic(OXG.OpInvert,     x)
    ELSE CheckInt(x) END
  ELSIF sym = ORS.plus THEN ORS.Get(sym); term(x);
  ELSE term(x)
  END;
  WHILE (sym >= ORS.plus) & (sym <= ORS.or) DO
    op := sym;  ORS.Get(sym);
    IF op = ORS.or THEN CheckBool(x);  term(y);  CheckBool(y);
      OXG.Or1(x);  OXG.Or2(x, y)
    ELSIF x.type.form = OXB.Int64 THEN term(y);  CheckInt(y);
      OXG.Dyadic(OXG.OpIntPlus + op - ORS.plus, x, y)
    ELSIF x.type.form = OXB.Real THEN term(y);  CheckReal(y);
      OXG.Dyadic(OXG.OpRealPlus + op - ORS.plus, x, y)
    ELSE CheckSet(x);  term(y);  CheckSet(y);
      OXG.Dyadic(OXG.OpSetOr + op - ORS.plus, x, y)
    END
  END
END SimpleExpression;

PROCEDURE expression0(VAR x: OXG.Argument);
VAR y: OXG.Argument;  obj: OXB.Object;  rel, xf, yf: INTEGER;
BEGIN SimpleExpression(x);
  IF (sym >= ORS.eql) & (sym <= ORS.geq) THEN
    rel := sym;  ORS.Get(sym);  SimpleExpression(y);  xf := x.type.form;  yf := y.type.form;
    IF x.type = y.type THEN
      IF (xf IN {OXB.Char, OXB.Int64}) THEN
        OXG.Dyadic(OXG.OpIntEql + rel - ORS.eql, x, y)
      ELSIF xf = OXB.Real THEN
        OXG.Dyadic(OXG.OpRealEql + rel - ORS.eql, x, y)
      ELSIF (xf IN {OXB.Set, OXB.Pointer, OXB.ProcAdr, OXB.NilTyp, OXB.Bool}) THEN
        IF rel <= ORS.neq THEN
          OXG.Dyadic(OXG.OpIntEql + rel - ORS.eql, x, y)
        ELSE
          ORS.Mark("only = or #")
        END
      ELSIF (xf = OXB.Array) & (x.type.base.form = OXB.Char) OR (xf = OXB.StrAdr) THEN
        OXG.Dyadic(OXG.OpStringEql + rel - ORS.eql, x, y)
      ELSE
        ORS.Mark("illegal comparison")
      END
    ELSIF (xf IN {OXB.Pointer, OXB.ProcAdr}) & (yf = OXB.NilTyp)
       OR (yf IN {OXB.Pointer, OXB.ProcAdr}) & (xf = OXB.NilTyp) THEN
      IF rel <= ORS.neq
        THEN
          OXG.Dyadic(OXG.OpIntEql + rel - ORS.eql, x, y)
        ELSE
          ORS.Mark("only = or #")
        END
    ELSIF (xf = OXB.Pointer) & (yf = OXB.Pointer) & (IsExtension(x.type.base, y.type.base)
       OR IsExtension(y.type.base, x.type.base))
       OR (xf = OXB.ProcAdr) & (yf = OXB.ProcAdr) & EqualSignatures(x.type, y.type) THEN
      IF rel <= ORS.neq THEN
        OXG.Dyadic(OXG.OpIntEql + rel - ORS.eql, x, y)
      ELSE
        ORS.Mark("only = or #")
      END
    ELSIF (xf = OXB.Array) & (x.type.base.form = OXB.Char)
        & ((yf = OXB.StrAdr) OR (yf = OXB.Array) & (y.type.base.form = OXB.Char))
        OR (yf = OXB.Array) & (y.type.base.form = OXB.Char) & (xf = OXB.StrAdr) THEN
      OXG.Dyadic(OXG.OpStringEql + rel - ORS.eql, x, y)
    ELSIF (xf = OXB.Char) & OXG.IsSingleCharacterString(y) THEN
      OXG.StrToChar(y);  OXG.Dyadic(OXG.OpIntEql + rel - ORS.eql, x, y)
    ELSIF (yf = OXB.Char) & OXG.IsSingleCharacterString(x) THEN
      OXG.StrToChar(x);  OXG.Dyadic(OXG.OpIntEql + rel - ORS.eql, x, y)
    ELSIF (xf = OXB.Int64) & (yf = OXB.Int64) THEN
      OXG.Dyadic(OXG.OpIntEql + rel - ORS.eql, x, y)  (*BYTE*)
    ELSE
      ORS.Mark("illegal comparison")
    END;
    (*x.type := OXB.boolType*)
  ELSIF sym = ORS.in THEN
    ORS.Get(sym);  CheckInt(x);  SimpleExpression(y);  CheckSet(y);
    OXG.Dyadic(OXG.OpIn, x, y);  x.type := OXB.boolType
  ELSIF sym = ORS.is THEN
    ORS.Get(sym);  qualident(obj);
    TypeTest(x, obj.type, FALSE);  x.type := OXB.boolType
  END
END expression0;

(* statements *)

PROCEDURE StandProc(pno: INTEGER);
VAR nap, npar: INTEGER;  (*nof actual/formal parameters*)
    x, y, z: OXG.Argument;
BEGIN
  Check(ORS.lparen, "no (");
  npar := pno MOD 10;  pno := pno DIV 10;  expression(x);  nap := 1;
  IF sym = ORS.comma THEN
    ORS.Get(sym);
    expression(y);
    nap := 2;
    (*z.type := OXB.noType;*)
    WHILE sym = ORS.comma DO ORS.Get(sym);  expression(z);  INC(nap) END
  ELSE y := NIL
  END;
  Check(ORS.rparen, "no )");
  IF (npar = nap) OR (pno IN {0, 1}) THEN
    IF pno IN {0, 1} THEN (*INC, DEC*)
      CheckInt(x);  CheckReadOnly(x);  IF y # NIL THEN CheckInt(y) END;
      OXG.Dyadic(OXG.OpInc + pno, x, y)
    ELSIF pno IN {2, 3} THEN (*INCL, EXCL*)
      CheckSet(x);  CheckReadOnly(x);  CheckInt(y);
      OXG.Dyadic(OXG.OpSetIncl + pno-2, x, y)
    ELSIF pno = 4 THEN CheckBool(x);  OXG.Assert(x)
    ELSIF pno = 5 THEN(*NEW*) CheckReadOnly(x);
       IF (x.type.form = OXB.Pointer) & (x.type.base.form = OXB.Record) THEN
         OXG.Monadic(OXG.OpNew, x)
       ELSE
         ORS.Mark("not a pointer to record")
       END
    ELSIF pno = 6  THEN CheckReal(x);  CheckInt(y);  CheckReadOnly(x);  OXG.Dyadic(OXG.OpPack, x, y)
    ELSIF pno = 7  THEN CheckReal(x);  CheckInt(y);  CheckReadOnly(x);  OXG.Dyadic(OXG.OpUnpack, x, y)
    ELSIF pno = 8  THEN IF x.type.form <= OXB.Set THEN OXG.Monadic(OXG.OpLed, x) ELSE ORS.Mark("bad type") END
    ELSIF pno = 10 THEN CheckInt(x);  OXG.Dyadic(OXG.OpGet, x, y)
    ELSIF pno = 11 THEN CheckInt(x);  OXG.Dyadic(OXG.OpPut, x, y)
    ELSIF pno = 12 THEN CheckInt(x);  CheckInt(y);  CheckInt(z);  OXG.Copy(x, y, z)
    (*ELSIF pno = 13 THEN CheckConst(x);  CheckInt(x);  OXG.LDPSR(x)*)
    ELSIF pno = 14 THEN CheckInt(x);  OXG.Dyadic(OXG.OpLoadReg, x, y)
    ELSE ORS.Mark("Standard procedure not implement")
    END
  ELSE ORS.Mark("wrong number of parameters")
  END
END StandProc;

PROCEDURE StatSequence;
VAR obj:        OXB.Object;
    orgtype:    OXB.Type;  (*original type of case var*)
    x, y, z, w: OXG.Argument;
    lbl:        OXG.Label;
    jmp:        OXG.Jump;
    saved:      SET;

  PROCEDURE TypeCase(obj: OXB.Object;  VAR x: OXG.Argument);
    VAR typobj: OXB.Object;
  BEGIN
    IF sym = ORS.ident THEN
      qualident(typobj);  x := OXG.MakeArgument(obj, Level);
      IF typobj.class # OXB.Typ THEN ORS.Mark("not a type") END;
      TypeTest(x, typobj.type, FALSE); obj.type := typobj.type;
      OXG.CFJump(x);  Check(ORS.colon, ": expected");  StatSequence
    ELSE OXG.CFJump(x);  ORS.Mark("type id expected")
    END
   END TypeCase;

  PROCEDURE SkipCase;
  BEGIN
    WHILE sym # ORS.colon DO ORS.Get(sym) END;
    ORS.Get(sym);  StatSequence
  END SkipCase;

BEGIN (* StatSequence *)
  REPEAT (*sync*) obj := NIL;
    IF ~((sym >= ORS.ident)  & (sym <= ORS.for) OR (sym >= ORS.semicolon)) THEN
      ORS.Mark("statement expected");
      REPEAT ORS.Get(sym) UNTIL (sym >= ORS.ident)
    END;
    OXG.StartStatement;
    IF sym = ORS.ident THEN
      qualident(obj);
      IF obj.class = OXB.StdProc THEN StandProc(obj.val)
      ELSE x := OXG.MakeArgument(obj, Level);
        selector(x);
        IF sym = ORS.becomes THEN (*assignment*)
          ORS.Get(sym);  CheckReadOnly(x);  expression(y);
          IF CompTypes(x.type, y.type, FALSE) THEN
            IF (x.type.form <= OXB.Pointer) OR (x.type.form = OXB.ProcAdr) THEN OXG.Store(x, y)
            ELSE OXG.StoreStruct(x, y)
            END
          ELSIF (x.type.form = OXB.Array) & (y.type.form = OXB.Array) & (x.type.base = y.type.base) & (y.type.len < 0) THEN
            OXG.StoreStruct(x, y)
          ELSIF (x.type.form = OXB.Array) & (x.type.base.form = OXB.Char) & (y.type.form = OXB.StrAdr) THEN
            OXG.CopyString(x, y)
          ELSIF (x.type.form = OXB.Int64) & (y.type.form = OXB.Int64) THEN OXG.Store(x, y)  (*BYTE*)
          ELSIF (x.type.form = OXB.Char) & OXG.IsSingleCharacterString(y) THEN
            OXG.StrToChar(y);  OXG.Store(x, y)
          ELSE
            ORS.Mark("illegal assignment")
          END
        ELSIF sym = ORS.eql THEN ORS.Mark("should be :=");  ORS.Get(sym);  expression(y)
        ELSIF sym = ORS.lparen THEN (*procedure call*)
          ORS.Get(sym);
          IF (x.type.form = OXB.ProcAdr) & (x.type.base.form = OXB.NoTyp) THEN
            OXG.PrepCall(x, saved);  ParamList(x);  OXG.Call(x, saved)
          ELSE ORS.Mark("not a procedure");  ParamList(x)
          END
        ELSIF x.type.form = OXB.ProcAdr THEN (*procedure call without parameters*)
          IF x.type.nofpar > 0 THEN ORS.Mark("missing parameters") END;
          IF x.type.base.form = OXB.NoTyp THEN
            OXG.PrepCall(x, saved);  OXG.Call(x, saved)
          ELSE
            ORS.Mark("not a procedure")
          END
        ELSIF x.class = OXB.Typ THEN ORS.Mark("illegal assignment")
        ELSE ORS.Mark("not a procedure")
        END
      END
    ELSIF sym = ORS.if THEN
      ORS.Get(sym); expression(x); CheckBool(x); OXG.CFJump(x);
      Check(ORS.then, "no THEN");
      StatSequence; jmp := NIL;
      WHILE sym = ORS.elsif DO
        ORS.Get(sym);
        OXG.FJump(jmp);
        OXG.FixLink(x(OXG.ConditionArg).falsechain);
        expression(x);  CheckBool(x);
        OXG.CFJump(x);
        Check(ORS.then, "no THEN");
        StatSequence
      END;
      IF sym = ORS.else THEN
        ORS.Get(sym);  OXG.FJump(jmp);  OXG.FixLink(x(OXG.ConditionArg).falsechain);
        StatSequence
      ELSE
        OXG.FixLink(x(OXG.ConditionArg).falsechain)
      END;
      OXG.FixLink(jmp); Check(ORS.end, "no END")
    ELSIF sym = ORS.while THEN
      lbl := OXG.MarkLoopStart();
      ORS.Get(sym); expression(x); CheckBool(x); OXG.CFJump(x);
      Check(ORS.do, "no DO");  StatSequence;
      OXG.BJump(lbl);
      WHILE sym = ORS.elsif DO
        ORS.Get(sym);  OXG.FixLink(x(OXG.ConditionArg).falsechain);  expression(x);  CheckBool(x);  OXG.CFJump(x);
        Check(ORS.do, "no DO");  StatSequence;  OXG.BJump(lbl)
      END;
      OXG.FixLink(x(OXG.ConditionArg).falsechain);
      Check(ORS.end, "no END")
    ELSIF sym = ORS.repeat THEN
      lbl := OXG.MarkLoopStart();
      ORS.Get(sym);  StatSequence;
      IF sym = ORS.until THEN
        ORS.Get(sym);  expression(x);  CheckBool(x);  OXG.CBJump(x, lbl)
      ELSE ORS.Mark("missing UNTIL")
      END
    ELSIF sym = ORS.for THEN  (* FOR val(x) := ini(y) TO lim(z) BY step(w) *)
      ORS.Get(sym);
      IF sym # ORS.ident THEN ORS.Mark("identifier expected")
      ELSE
        qualident(obj);  x := OXG.MakeArgument(obj, Level);  CheckInt(x);  CheckReadOnly(x);
        IF sym # ORS.becomes THEN ORS.Mark(":= expected")
        ELSE
          ORS.Get(sym);  expression(y);  CheckInt(y); OXG.For0(x, y);
          lbl := OXG.MarkLoopStart();
          Check(ORS.to, "TO expected");
          expression(z);  CheckInt(z);  obj.rdo := TRUE;
          IF sym = ORS.by THEN ORS.Get(sym); expression(w); CheckConst(w); CheckInt(w)
          ELSE w := OXG.NewArg(OXB.Const, 1, 0, 0, OXB.intType)
          END;
          Check(ORS.do, "DO expected");
          OXG.For1(x, y, z, w, jmp);  (* val, ini, lim, step *)
          StatSequence; Check(ORS.end, "END expected");
          OXG.For2(x, y, w);
          OXG.BJump(lbl);
          OXG.FixLink(jmp);
          obj.rdo := FALSE
        END
      END
    ELSIF sym = ORS.case THEN
      ORS.Get(sym);
      IF sym = ORS.ident THEN
        qualident(obj);  orgtype := obj.type;
        IF (orgtype.form = OXB.Pointer) OR (orgtype.form = OXB.Record) & (obj.class = OXB.RefPar) THEN
          Check(ORS.of, "OF expected");  TypeCase(obj, x);
          jmp := NIL;
          WHILE sym = ORS.bar DO
            ORS.Get(sym);  OXG.FJump(jmp);  OXG.FixLink(x(OXG.ConditionArg).falsechain);
            obj.type := orgtype;  TypeCase(obj, x)
          END;
          OXG.FixLink(x(OXG.ConditionArg).falsechain);  OXG.FixLink(jmp);  obj.type := orgtype
        ELSE ORS.Mark("numeric case not implemented");
          Check(ORS.of, "OF expected");  SkipCase;
          WHILE sym = ORS.bar DO SkipCase END
        END
      ELSE ORS.Mark("ident expected")
      END;
      Check(ORS.end, "no END")
    END;
    OXG.CheckRegs;
    IF sym = ORS.semicolon THEN ORS.Get(sym)
    ELSIF sym < ORS.semicolon THEN ORS.Mark("missing semicolon?")
    END
  UNTIL sym > ORS.semicolon;
END StatSequence;

(* Types and declarations *)

PROCEDURE IdentList(class: INTEGER;  VAR first: OXB.Object);
VAR obj: OXB.Object;
BEGIN
  IF sym = ORS.ident THEN
    OXB.NewObj(first, ORS.id, class);  ORS.Get(sym);  CheckExport(first.expo);
    WHILE sym = ORS.comma DO
      ORS.Get(sym);
      IF sym = ORS.ident THEN OXB.NewObj(obj, ORS.id, class);  ORS.Get(sym);  CheckExport(obj.expo)
      ELSE ORS.Mark("ident?")
      END
    END;
    IF sym = ORS.colon THEN ORS.Get(sym) ELSE ORS.Mark(":?") END
  ELSE first := NIL
  END
END IdentList;

PROCEDURE ArrayType(VAR type: OXB.Type);
VAR x: OXG.Argument;  typ: OXB.Type;  len: INTEGER;
BEGIN NEW(typ); typ.form := OXB.NoTyp;
  expression(x);
  IF (x.class = OXB.Const) & (x.type.form = OXB.Int64)
   & (x.val >= 0) THEN len := x.val
  ELSE len := 1;  ORS.Mark("not a valid length")
  END;
  IF sym = ORS.of THEN ORS.Get(sym); Type(typ.base);
    IF (typ.base.form = OXB.Array) & (typ.base.len < 0) THEN ORS.Mark("dyn array not allowed") END
  ELSIF sym = ORS.comma THEN ORS.Get(sym); ArrayType(typ.base)
  ELSE ORS.Mark("missing OF"); typ.base := OXB.intType
  END;
  typ.size := len * typ.base.size;  (* Was: (len * typ.base.size + 7) DIV 8 * 8 *)
  typ.form := OXB.Array; typ.len := len; type := typ
END ArrayType;

PROCEDURE Alignment(type: OXB.Type): INTEGER;
VAR result, align: INTEGER;  field: OXB.Object;
BEGIN
  IF type.form = OXB.Array THEN
    result := Alignment(type.base)
  ELSIF type.form = OXB.Record THEN
    result := 0;  field := type.dsc;
    WHILE field # NIL DO
      align := Alignment(field.type);
      IF align > result THEN result := align END;
      field := field.next
    END
  ELSE
    result := type.size
  END
RETURN result END Alignment;

PROCEDURE Align(VAR adr: INTEGER; size: INTEGER);
(* Advance adr to be aligned suitably for a variable of given size *)
BEGIN
  IF    size > 4 THEN adr := (adr + 7) DIV 8 * 8
  ELSIF size > 2 THEN adr := (adr + 3) DIV 4 * 4
  ELSIF size > 1 THEN adr := (adr + 1) DIV 2 * 2
  END
END Align;

PROCEDURE RecordType(VAR type: OXB.Type; hosted: BYTE);
VAR obj, obj0, new, bot, base: OXB.Object;
    typ, tp: OXB.Type;
    offset, off, align, n: INTEGER;
BEGIN NEW(typ);
  typ.form   := OXB.NoTyp;
  typ.base   := NIL;
  typ.mno    := -Level;
  typ.nofpar := 0;
  typ.hosted := hosted;
  offset     := 0;
  bot        := NIL;
  IF sym = ORS.lparen THEN
    ORS.Get(sym);  (*record extension*)
    IF Level # 0 THEN ORS.Mark("extension of local types not implemented") END;
    IF sym = ORS.ident THEN
      qualident(base);
      IF base.class = OXB.Typ THEN
        IF base.type.form = OXB.Record THEN typ.base := base.type
        ELSE typ.base := OXB.intType;  ORS.Mark("invalid extension")
        END;
        typ.nofpar := typ.base.nofpar + 1;  (*"nofpar" here abused for extension level*)
        bot := typ.base.dsc; offset := typ.base.size
      ELSE ORS.Mark("type expected")
      END
    ELSE ORS.Mark("ident expected")
    END;
    Check(ORS.rparen, "no )")
  END;
  WHILE sym = ORS.ident DO  (*fields*)
    n := 0;  obj := bot;
    WHILE sym = ORS.ident DO
      obj0 := obj;
      WHILE (obj0 # NIL) & (obj0.name # ORS.id) DO obj0 := obj0.next END;
      IF obj0 # NIL THEN ORS.Mark("mult def") END;
      NEW(new);  ORS.CopyId(new.name);  new.class := OXB.Field;  new.next := obj;  obj := new;  INC(n);
      ORS.Get(sym);  CheckExport(new.expo);
      IF (sym # ORS.comma) & (sym # ORS.colon) THEN ORS.Mark("comma expected")
      ELSIF sym = ORS.comma THEN ORS.Get(sym)
      END
    END;
    Check(ORS.colon, "colon expected");
    Type(tp);
    IF (tp.form = OXB.Array) & (tp.len < 0) THEN ORS.Mark("dyn array not allowed") END;
    Align(offset, Alignment(tp));
    offset := offset + n * tp.size;  off := offset;  obj0 := obj;
    WHILE obj0 # bot DO
      obj0.type := tp;   obj0.modno := 0;  off := off - tp.size;
      obj0.val  := off;  obj0 := obj0.next
    END;
    bot := obj;
    IF sym = ORS.semicolon THEN ORS.Get(sym) ELSIF sym # ORS.end THEN ORS.Mark(" ;  or END") END
  END;
  typ.form := OXB.Record;  typ.dsc := bot;  typ.size := offset;
  type := typ
END RecordType;

(* Formal parameter section - one or more comma separated vars of one type *)
(* Each parameter takes either one or two 64 bit cells on the stack        *)
(* Parameters are numbered in obj.val starting at 0                        *)
PROCEDURE FPSection(VAR nofpar: INTEGER);
VAR obj, first: OXB.Object;  tp: OXB.Type;  cl: INTEGER;  rdo: BOOLEAN;
BEGIN
  IF sym = ORS.var THEN ORS.Get(sym); cl := OXB.RefPar ELSE cl := OXB.ValPar END;
  IdentList(cl, first);  FormalType(tp, 0);  rdo := FALSE;
  IF (cl = OXB.ValPar) & (tp.form >= OXB.Array) THEN
    cl := OXB.RefPar;  rdo := TRUE
  END;
  obj := first;
  WHILE obj # NIL DO
    obj.val   := nofpar;  INC(nofpar);
    obj.class := cl;      obj.type  := tp;
    obj.rdo   := rdo;     obj.modno := -Level;
    obj := obj.next
  END
END FPSection;

PROCEDURE ProcedureType(ptype: OXB.Type);
VAR obj: OXB.Object;  nofpar: INTEGER;
BEGIN ptype.base := OXB.noType;  nofpar := 0;  ptype.dsc := NIL;
  IF sym = ORS.lparen THEN                     (* Parse formal parameters *)
    ORS.Get(sym);
    IF sym = ORS.rparen THEN ORS.Get(sym)
    ELSE
      FPSection(nofpar);
      WHILE sym = ORS.semicolon DO ORS.Get(sym); FPSection(nofpar) END;
      Check(ORS.rparen, "no )")
    END;
    IF sym = ORS.colon THEN                    (* Parse function result type *)
      ORS.Get(sym);
      IF sym = ORS.ident THEN
        qualident(obj);  ptype.base := obj.type;
        IF ~((obj.class = OXB.Typ) & (obj.type.form IN {OXB.Byte .. OXB.Pointer, OXB.ProcAdr})) THEN
          ORS.Mark("illegal function type")
        END
      ELSE ORS.Mark("type identifier expected")
      END
    END
  END;
  ptype.nofpar := nofpar;
END ProcedureType;

PROCEDURE FormalType0(VAR typ: OXB.Type;  dim: INTEGER);
VAR obj: OXB.Object;  dmy: INTEGER;
BEGIN
  IF sym = ORS.ident THEN
    qualident(obj);
    IF obj.class = OXB.Typ THEN typ := obj.type ELSE ORS.Mark("not a type");  typ := OXB.intType END
  ELSIF sym = ORS.array THEN
    ORS.Get(sym);  Check(ORS.of, "OF ?");
    IF dim >= 1 THEN ORS.Mark("multi-dimensional open arrays not implemented") END;
    NEW(typ);  typ.form := OXB.Array;  typ.len := -1;  typ.size := 2*OXG.WordSize;
    FormalType(typ.base, dim+1)
  ELSIF sym = ORS.procedure THEN
    ORS.Get(sym);  OXB.OpenScope;
    NEW(typ);  typ.form := OXB.ProcAdr;  typ.size := OXG.WordSize;
    typ.hosted := OXB.Nohost;  dmy := 0;
    ProcedureType(typ);
    typ.dsc := OXB.topScope.next;  OXB.CloseScope
  ELSE ORS.Mark("identifier expected");  typ := OXB.noType
  END
END FormalType0;

PROCEDURE CheckRecLevel(lev: INTEGER);
BEGIN
  IF lev # 0 THEN ORS.Mark("ptr base must be global") END
END CheckRecLevel;

PROCEDURE Type0(VAR type: OXB.Type);
VAR obj: OXB.Object;  ptbase: PtrBase;  hosted: BYTE;
BEGIN type := OXB.intType;  (*sync*)
  IF (sym # ORS.ident) & (sym < ORS.array) THEN ORS.Mark("not a type");
    REPEAT ORS.Get(sym) UNTIL (sym = ORS.ident) OR (sym >= ORS.array)
  END;
  IF sym = ORS.ident THEN
    qualident(obj);
    IF obj.class = OXB.Typ THEN
      IF (obj.type # NIL) & (obj.type.form # OXB.NoTyp) THEN type := obj.type END
    ELSE ORS.Mark("not a type or undefined")
    END
  ELSIF sym = ORS.array THEN ORS.Get(sym);  ArrayType(type)
  ELSIF sym = ORS.record THEN
    ORS.Get(sym);  CheckHosted(hosted, {OXB.Nogc});  RecordType(type, hosted);  Check(ORS.end, "no END")
  ELSIF sym = ORS.pointer THEN
    ORS.Get(sym);  CheckHosted(hosted, {OXB.Nogc});  Check(ORS.to, "no TO");
    NEW(type); type.form   := OXB.Pointer; type.size   := OXG.WordSize; type.base   := OXB.intType;
    type.hosted := hosted;
    IF sym = ORS.ident THEN
      obj := OXB.thisObj();
      IF obj # NIL THEN
        IF (obj.class = OXB.Typ) & (obj.type.form IN {OXB.Record, OXB.NoTyp}) THEN
          CheckRecLevel(-obj.modno);  type.base := obj.type;
          IF (type.hosted # OXB.Nogc) & (obj.type.hosted = OXB.Nogc) THEN
            ORS.Mark("Garbage collected pointer may not point to uncollected record")
          END;
        ELSIF obj.class = OXB.Mod THEN ORS.Mark("external base type not implemented")
        ELSE ORS.Mark("no valid base type")
        END
      ELSE CheckRecLevel(Level);  (*enter into list of forward references to be fixed in Declarations*)
        NEW(ptbase);  ORS.CopyId(ptbase.name);  ptbase.type := type;
        ptbase.next := pbsList;  pbsList := ptbase
      END;
      ORS.Get(sym)
    ELSE Type(type.base);
      IF (type.base.form # OXB.Record) OR (type.base.typobj = NIL) THEN ORS.Mark("must point to named record") END;
      IF (type.hosted # OXB.Nogc) & (type.base.hosted = OXB.Nogc) THEN
        ORS.Mark("Garbage collected pointer may not point to uncollected record")
      END;
      CheckRecLevel(Level)
    END
  ELSIF sym = ORS.procedure THEN
    ORS.Get(sym);  OXB.OpenScope;
    NEW(type);  type.form := OXB.ProcAdr;  type.size := OXG.WordSize;
    CheckHosted(type.hosted, {OXB.WinABI, OXB.LinABI});
    ProcedureType(type);  type.dsc := OXB.topScope.next;  OXB.CloseScope
  ELSE ORS.Mark("illegal type")
  END
END Type0;


PROCEDURE Declarations(varclass: INTEGER);
VAR obj, first: OXB.Object;
  x: OXG.Argument; tp: OXB.Type; ptbase: PtrBase;
  expo: BOOLEAN; id: ORS.Ident;
  nofvars: INTEGER;
BEGIN (*sync*) pbsList := NIL;
  IF (sym < ORS.const) & (sym # ORS.end) & (sym # ORS.return) THEN ORS.Mark("declaration?");
    REPEAT ORS.Get(sym) UNTIL (sym >= ORS.const) OR (sym = ORS.end) OR (sym = ORS.return)
  END;
  IF sym = ORS.const THEN
    ORS.Get(sym);
    WHILE sym = ORS.ident DO
      ORS.CopyId(id);  ORS.Get(sym);  CheckExport(expo);
      IF sym = ORS.eql THEN ORS.Get(sym) ELSE ORS.Mark("= ?") END;
      expression(x);
      IF OXG.IsSingleCharacterString(x) THEN OXG.StrToChar(x) END;
      OXB.NewObj(obj, id, OXB.Const);  obj.expo := expo;
      IF x.class IN {OXB.Const, OXB.String} THEN
        obj.class := x.class;  obj.type := x.type;
        obj.val   := x.val;    obj.ext  := x.ext;
        obj.modno := x.modno
      ELSE ORS.Mark("expression not constant");  obj.type := OXB.intType
      END;
      Check(ORS.semicolon, "; missing")
    END
  END;
  IF sym = ORS.type THEN
    ORS.Get(sym);
    WHILE sym = ORS.ident DO
      ORS.CopyId(id);  ORS.Get(sym);  CheckExport(expo);
      IF sym = ORS.eql THEN ORS.Get(sym) ELSE ORS.Mark("=?") END;
      Type(tp);
      OXB.NewObj(obj, id, OXB.Typ);  obj.type := tp;  obj.expo := expo;  obj.modno := -Level;
      IF tp.typobj = NIL THEN tp.typobj := obj END;
      IF expo & (obj.type.form = OXB.Record) THEN obj.exno := 1 END;
      IF tp.form = OXB.Record THEN
        ptbase := pbsList;  (*check whether this is base of a pointer type;  search and fixup*)
        WHILE ptbase # NIL DO
          IF obj.name = ptbase.name THEN
            ptbase.type.base := obj.type;
            IF (ptbase.type.base.hosted = OXB.Nogc) & (ptbase.type.hosted # OXB.Nogc) THEN
              ORS.Mark("Garbage collected pointer may not point to uncollected record")
            END
          END;
          ptbase := ptbase.next
        END;
        IF Level = 0 THEN OXG.BuildTD(tp, VarSize) END (* type descriptor; len used as its address *)
      END;
      Check(ORS.semicolon, "; missing")
    END
  END;
  IF sym = ORS.var THEN
    ORS.Get(sym);  nofvars := 0;
    WHILE sym = ORS.ident DO
      IdentList(varclass, first); Type(tp);
      (* Assign type and index to var list *)
      obj := first;  WHILE obj # NIL DO
        obj.type := tp;  obj.modno := -Level;  obj.val := nofvars;
        IF obj.expo THEN obj.exno := 1 END;
        obj := obj.next;    INC(nofvars)
      END;
      Check(ORS.semicolon, "; missing")
    END
  END;
  ptbase  := pbsList;
  WHILE ptbase # NIL DO
    IF ptbase.type.base.form = OXB.Int64 THEN
      ORS.Mark("undefined pointer base of "); ws(ptbase.name); wn
    END;
    ptbase := ptbase.next
  END;
  IF (sym >= ORS.const) & (sym <= ORS.var) THEN ORS.Mark("declaration in bad order") END
END Declarations;


PROCEDURE ProcedureDecl;
VAR proc:        OXB.Object;
    obj:         OXB.Object;
    type:        OXB.Type;
    procid:      ORS.Ident;
    x, y:        OXG.Argument;
    jmp:         OXG.Jump;
    noflocals:   INTEGER;
    i:           INTEGER;
    longestname: INTEGER;
    hosted:      BYTE;
BEGIN ORS.Get(sym);
  CheckHosted(hosted, {OXB.LinABI, OXB.Syscall, OXB.WinABI});

  IF hosted = OXB.Syscall THEN
    IF sym = ORS.int THEN
      i := ORS.ival;  ORS.Get(sym)
    ELSE
      i := 0;  ORS.Mark("Missing syscall number");
    END;
  END;

  IF sym = ORS.ident THEN
    ORS.CopyId(procid);  ORS.Get(sym);
    OXB.NewObj(proc, ORS.id, OXB.Proc);
    NEW(type);  type.form := OXB.ProcAdr;  type.size := OXG.WordSize;
    type.hosted := hosted;
    proc.type := type;  proc.val := -1;  proc.modno := -Level;
    CheckExport(proc.expo);
    IF proc.expo THEN
      IF hosted # OXB.Syscall THEN
        proc.exno := 1
      ELSE
        ORS.Mark("Cannot export Linux syscall procedure.");
        proc.expo := FALSE
      END
    END;
    OXB.OpenScope;  INC(Level);  type.base := OXB.noType;
    ProcedureType(type);  (*formal parameter list*)

    (*IF hosted > 0 THEN proc.modno := hosted END;*)

    IF hosted = OXB.Syscall THEN  (* no body *)
      type.dsc := OXB.topScope.next;
      proc.val := i
    ELSE
      Check(ORS.semicolon, "no ;");
      Declarations(OXB.ProcVar);

      type.dsc := OXB.topScope.next;

      (* Parse nested procedures *)
      proc.val := OXG.Here();
      IF sym = ORS.procedure THEN
        jmp := NIL;  OXG.FJump(jmp);
        REPEAT ProcedureDecl;  Check(ORS.semicolon, "no ;") UNTIL sym # ORS.procedure;
        OXG.FixLink(jmp);
        proc.val := OXG.Here();
        proc.type.dsc := OXB.topScope.next
      END;

      (* Compile procedure body *)
      (*wn; ws("["); wi(ORS.linenum); ws("] PROCEDURE "); ws(procid); wsn("."); wn;*)
      x := OXG.MakeArgument(proc, Level);
      OXG.Monadic(OXG.OpEnter, x);
      IF sym = ORS.begin THEN ORS.Get(sym);  StatSequence END;
      IF sym = ORS.return THEN
        ORS.Get(sym);  expression(y);
        IF type.base = OXB.noType THEN ORS.Mark("this is not a function")
        ELSIF ~CompTypes(type.base, y.type, FALSE) THEN ORS.Mark("wrong result type")
        END;
      ELSIF type.base.form # OXB.NoTyp THEN
        ORS.Mark("function without result");  type.base := OXB.noType
      END;
      Check(ORS.end, "no END");
      IF sym = ORS.ident THEN
        IF ORS.id # procid THEN ORS.Mark("no match") END;
        ORS.Get(sym)
      ELSE ORS.Mark("no proc id")
      END;
      OXG.Dyadic(OXG.OpReturn, x, y);

      (* Having generated pseudode we can now determine storage of *)
      (* local variables, intermediate values and parameters.      *)
      (*OXG.AllocProc;*)

      OXG.Generate(proc);
    END;
    OXB.CloseScope;  DEC(Level)
  END
END ProcedureDecl;


PROCEDURE Import;
VAR impid, impid1: ORS.Ident;
BEGIN
  IF sym = ORS.ident THEN
    ORS.CopyId(impid); ORS.Get(sym);
    IF sym = ORS.becomes THEN
      ORS.Get(sym);
      IF sym = ORS.ident THEN ORS.CopyId(impid1); ORS.Get(sym)
      ELSE ORS.Mark("id expected"); impid1 := impid
      END
    ELSE impid1 := impid
    END;
    OXB.Import(impid, impid1)
  ELSE ORS.Mark("id expected")
  END
END Import;


PROCEDURE Module*(VAR newsymbols: BOOLEAN);
VAR
  key:    INTEGER;
  exno:   INTEGER;
  obj:    OXB.Object;
  start:  INTEGER;
  x:      OXG.Argument;
BEGIN
  start := H.Time();
  Texts.WriteString(W, "  compiling ");
  ORS.Get(sym);
  IF sym # ORS.module THEN
    ORS.Mark("must start with MODULE");
    (*H.wn; H.ws("Sym: "); H.wi(sym); H.ws(", '"); H.ws(ORS.id); H.wsn("'.");*)
  ELSE
    ORS.Get(sym);
    OXB.Init;  OXB.OpenScope;
    IF sym = ORS.ident THEN
      ORS.CopyId(modid);  ORS.Get(sym);
      Texts.WriteString(W, modid);  Texts.Append(Texts.Log, W.buf)
    ELSE ORS.Mark("identifier expected")
    END;
    Check(ORS.semicolon, "no ;");  Level := 0;  key := 0;

    IF sym = ORS.import THEN
      ORS.Get(sym);  Import;
      WHILE sym = ORS.comma DO ORS.Get(sym); Import END;
      Check(ORS.semicolon, "; missing")
    END;
    OXG.Open(modid);
    Declarations(OXB.ModVar);

    (* Allocate global variable addresses *)
    VarSize := 0;
    obj := OXB.topScope.next;
    WHILE obj # NIL DO
      IF obj.class = OXB.ModVar THEN
        Align(VarSize, obj.type.size);  obj.val := VarSize;  INC(VarSize, obj.type.size)
      END;
      obj := obj.next
    END;

    OXG.SetDataSize((VarSize + 7) DIV 8 * 8);
    WHILE sym = ORS.procedure DO ProcedureDecl; Check(ORS.semicolon, "no ;") END;

    x := NIL;
    OXG.Monadic(OXG.OpEnter, x);

    IF sym = ORS.begin THEN ORS.Get(sym);  StatSequence END;
    Check(ORS.end, "no END");
    IF sym = ORS.ident THEN
      IF ORS.id # modid THEN ORS.Mark("no match") END;
      ORS.Get(sym)
    ELSE ORS.Mark("Module end identifier missing")
    END;
    IF sym # ORS.period THEN ORS.Mark("period missing") END;

    (* Identify record descriptors that need exporting because *)
    (* they are referenced by exported pointer types.          *)
    obj := OXB.topScope.next;
    WHILE obj # NIL DO
      IF (obj.class = OXB.Typ)
       & (obj.type.form = OXB.Pointer)
       & (obj.type.base.typobj # NIL) THEN
        obj.type.base.typobj.exno := 1
      END;
      obj := obj.next
    END;
    (* Allocate export numbers *)
    exno := 1;
    obj := OXB.topScope.next;
    WHILE obj # NIL DO
      IF obj.exno # 0 THEN obj.exno := exno; INC(exno) END;
      obj := obj.next
    END;
    IF ORS.errcnt = 0 THEN
      OXB.Export(modid, newsymbols, key);
      IF newsymbols THEN Texts.WriteString(W, " new symbol file") END;
      OXG.Close(modid, key, exno);
      (*Texts.WriteInt(W, X64.PC, 6);*)  Texts.WriteInt(W, VarSize, 6);  Texts.WriteHex(W, key);
      Texts.WriteInt(W, (H.Time() - start) DIV 10000, 4); Texts.WriteString(W, "ms")
    ELSE
      Texts.WriteLn(W);  Texts.WriteString(W, "compilation FAILED");
      H.SetExitCode(1)
    END;
    Texts.WriteLn(W);  Texts.Append(Texts.Log, W.buf);
    OXB.CloseScope;  pbsList := NIL
  END
END Module;

PROCEDURE Option(VAR S: Texts.Scanner);
BEGIN newSF := FALSE;
  IF S.nextCh = "/" THEN
    Texts.Scan(S); Texts.Scan(S);
    IF (S.class = Texts.Name) & (S.s[0] = "s") THEN newSF := TRUE END
  END
END Option;

PROCEDURE Compile*;
VAR
  beg, end, time: INTEGER;
  T: Texts.Text;
  S: Texts.Scanner;
BEGIN Texts.OpenScanner(S, Texts.Par.text, Texts.Par.pos); Texts.Scan(S);
  IF S.class = Texts.Char THEN
    IF S.c = "@" THEN
      Option(S); Texts.GetSelection(T, beg, end, time);
      IF time >= 0 THEN ORS.Init(T, beg); Module(newSF) END
    ELSIF S.c = "^" THEN
      Option(S); Texts.GetSelection(T, beg, end, time);
      IF time >= 0 THEN
        Texts.OpenScanner(S, T, beg); Texts.Scan(S);
        IF S.class = Texts.Name THEN
          Texts.WriteString(W, S.s); NEW(T); Texts.Open(T, S.s);
          IF T.len > 0 THEN ORS.Init(T, 0); Module(newSF) END
        END
      END
    END
  ELSE
    WHILE S.class = Texts.Name DO
      NEW(T); Texts.Open(T, S.s);
      IF T.len > 0 THEN Option(S); ORS.Init(T, 0); Module(newSF)
      ELSE Texts.WriteString(W, S.s); Texts.WriteString(W, " not found");
        Texts.WriteLn(W); Texts.Append(Texts.Log, W.buf)
      END;
      IF (T.len # 0) & (ORS.errcnt = 0) THEN Texts.Scan(S) ELSE S.class := 0 END
    END
  END;
  Modules.Collect(0)  (* Trigger immediate garbage collection when OXP exits *)
END Compile;


BEGIN
  Texts.OpenWriter(W); Texts.WriteString(W, "OX Compiler  DCWB 2024-08-10, NW 8.3.2020 ");
  Texts.WriteLn(W); Texts.Append(Texts.Log, W.buf);
  NEW(dummy);  dummy.class := OXB.ModVar;  dummy.type := OXB.intType;
  expression := expression0;  Type := Type0;  FormalType := FormalType0;
END OXP.
