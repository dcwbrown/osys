MODULE OCP;  (* DCWB 2024-08-10 Oberon common target parser *)
(* Modified from N. Wirth 1.7.97 / 8.3.2020  Oberon compiler for RISC in Oberon-07*)

IMPORT SYSTEM, H := Host, Kernel, Files, Modules, Texts, ORS, OCB, OCG;

  (*Original author: Niklaus Wirth, 2014.
    Parser of Oberon-RISC compiler. Uses Scanner ORS to obtain symbols (tokens),
    OCB for definition of data structures and for handling import and export, and
    OCG to produce binary code. OCP performs type checking and data allocation.
    Parser is target-independent, except for part of the handling of allocations.*)


TYPE
  PtrBase = POINTER TO PtrBaseDesc;
  PtrBaseDesc = RECORD  (*list of names of pointer base types*)
    name: ORS.Ident; type: OCB.Type; next: PtrBase
  END;

VAR
  sym:         INTEGER;    (* last symbol read *)
  VarSize:     INTEGER;    (* Global VAR size *)
  Level:       INTEGER;    (* Procedure nesting level *)
  newSF:       BOOLEAN;    (* option flag *)
  expression:  PROCEDURE(): OCB.Object;  (* to avoid forward reference *)
  Type:        PROCEDURE(VAR type: OCB.Type);
  FormalType:  PROCEDURE(VAR typ: OCB.Type;  dim: INTEGER);
  modid:       ORS.Ident;
  pbsList:     PtrBase;    (* list of names of pointer base types *)
  dummy:       OCB.Object;
  W:           Texts.Writer;
  CodeSeq:     OCB.Object;


PROCEDURE wa;                    BEGIN Texts.Append(Texts.Log, W.buf) END wa;
PROCEDURE wn;                    BEGIN Texts.WriteLn(W); wa           END wn;
PROCEDURE ws (s: ARRAY OF CHAR); BEGIN Texts.WriteString(W, s)        END ws;
PROCEDURE wsn(s: ARRAY OF CHAR); BEGIN Texts.WriteString(W, s); wn    END wsn;
PROCEDURE wi (i: INTEGER);       BEGIN Texts.WriteInt(W, i, 1)        END wi;
PROCEDURE wir(i, n: INTEGER);    BEGIN Texts.WriteInt(W, i, n)        END wir;


PROCEDURE GenExpr(sym: INTEGER; x, y: OCB.Object): OCB.Object;
RETURN OCG.NewItem(OCB.Expression, sym, -1, x.type, x, y)
END GenExpr;


PROCEDURE GenStmt(val: INTEGER; obj: OCB.Object);
BEGIN
  CodeSeq := OCG.NewItem(OCB.Statement, val, 0, NIL, CodeSeq, obj)
END GenStmt;


PROCEDURE Check(s: INTEGER;  msg: ARRAY OF CHAR);
BEGIN IF sym = s THEN ORS.Get(sym) ELSE ORS.Mark(msg) END
END Check;

PROCEDURE qualident(VAR obj: OCB.Object);
BEGIN obj := OCB.thisObj();  ORS.Get(sym);
  IF obj = NIL THEN ORS.Mark("undef");  obj := dummy END;
  IF (sym = ORS.period) & (obj.class = OCB.Mod) THEN
    ORS.Get(sym);
    IF sym = ORS.ident THEN obj := OCB.thisimport(obj);  ORS.Get(sym);
      IF obj = NIL THEN ORS.Mark("undef");  obj := dummy END
    ELSE ORS.Mark("identifier expected");  obj := dummy
    END
  END
END qualident;

PROCEDURE IsBool(x: OCB.Object): BOOLEAN;
VAR res: BOOLEAN;
BEGIN
  IF    x.type.form = OCB.Bool   THEN res := TRUE
  ELSIF x.class # OCB.Expression THEN res := FALSE
  ELSE
    res := x.val IN {ORS.and, ORS.or, ORS.not,
                     ORS.eql .. ORS.geq,
                     ORS.in,  ORS.is}
  END
RETURN res END IsBool;

PROCEDURE CheckBool(VAR x: OCB.Object);
BEGIN
  IF ~IsBool(x) THEN
    ORS.Mark("not Boolean");  x.type := OCB.boolType
  END
END CheckBool;

PROCEDURE CheckInt(VAR x: OCB.Object);
BEGIN
  IF x.type.form # OCB.Int64 THEN ORS.Mark("not Integer");  x.type := OCB.intType END
END CheckInt;

PROCEDURE CheckReal(VAR x: OCB.Object);
BEGIN
  IF x.type.form # OCB.Real THEN ORS.Mark("not Real");  x.type := OCB.realType END
END CheckReal;

PROCEDURE CheckSet(VAR x: OCB.Object);
BEGIN
  IF x.type.form # OCB.Set THEN ORS.Mark("not Set");  x.type := OCB.setType END
END CheckSet;

PROCEDURE CheckSetVal(VAR x: OCB.Object);
BEGIN
  IF x.type.form # OCB.Int64 THEN
    ORS.Mark("not Int");  x.type := OCB.setType
  ELSIF x.class = OCB.Const THEN
    IF (x.val < 0) OR (x.val >= 64) THEN ORS.Mark("invalid set") END
  END
END CheckSetVal;

PROCEDURE CheckConst(VAR x: OCB.Object);
BEGIN
  IF ~((x.class = OCB.Const) OR (x.type.form IN {OCB.ProcAdr, OCB.StrAdr})) THEN
    ORS.Mark("not a constant");
    x := OCG.NewItem(OCB.Const, 0, 0, OCB.intType, NIL, NIL)
  END
END CheckConst;

PROCEDURE CheckReadOnly(VAR x: OCB.Object);
BEGIN
  IF x.rdo THEN ORS.Mark("read-only") END
END CheckReadOnly;

PROCEDURE CheckAccess(x: OCB.Object; level: INTEGER);
BEGIN
  ASSERT(x.class IN {OCB.Const,   OCB.Typ,
                     OCB.GlobVar, OCB.ProcVar,
                     OCB.ValPar,  OCB.RefPar,
                     OCB.Proc,    OCB.StdProc});
  IF (x.modno < 0) & (x.modno # -level) & (x.class # OCB.Const) THEN
    ORS.Mark("not accessible ")
  END;
END CheckAccess;

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
  hosted := OCB.Nohost;
  IF ~OCB.SystemImported() THEN allowed := {} END;
  IF sym IN {ORS.rdiv, ORS.bslash, ORS.neq, ORS.minus} THEN
    IF    sym = ORS.rdiv   THEN hosted := OCB.LinABI
    ELSIF sym = ORS.neq    THEN hosted := OCB.Syscall
    ELSIF sym = ORS.bslash THEN hosted := OCB.WinABI
    ELSE                        hosted := OCB.Nogc
    END;
    ORS.Get(sym);
    IF ~(hosted IN allowed) THEN
      IF    hosted = OCB.WinABI  THEN ORS.Mark("'\' Windows procedure mark not allowed here")
      ELSIF hosted = OCB.LinABI  THEN ORS.Mark("'/' Linux procedure mark not allowed here")
      ELSIF hosted = OCB.Syscall THEN ORS.Mark("'#' Linux syscall procedure mark not allowed here")
      ELSE                            ORS.Mark("'-' No-garbage-collection mark not allowed here")
      END
    END;
    IF ~OCB.SystemImported() THEN ORS.Mark("Mark requires importing SYSTEM.") END
  END
END CheckHosted;


PROCEDURE IsExtension(t0, t1: OCB.Type): BOOLEAN;
BEGIN (*t1 is an extension of t0*)
  RETURN (t0 = t1) OR (t1 # NIL) & IsExtension(t0, t1.base)
END IsExtension;


(* expressions *)

PROCEDURE TypeTest(VAR x: OCB.Object;  T: OCB.Type;  guard: BOOLEAN): OCB.Object;
VAR xt: OCB.Type;  res: OCB.Object;
BEGIN xt := x.type;
  IF (T.form = xt.form )
   & (   (T.form = OCB.Pointer)
      OR (T.form = OCB.Record) & (x.class = OCB.RefPar)) THEN
    WHILE (xt # T) & (xt # NIL) DO xt := xt.base END;
    IF xt # T THEN xt := x.type;
      IF xt.form = OCB.Pointer THEN
        IF IsExtension(xt.base, T.base)
        THEN res := OCG.GenTypeTest(x, T.base, FALSE, guard); x.type := T
        ELSE ORS.Mark("not an extension")
        END
      ELSIF (xt.form = OCB.Record) & (x.class = OCB.RefPar) THEN
        IF IsExtension(xt, T) THEN res := OCG.GenTypeTest(x, T, TRUE, guard);  x.type := T
        ELSE ORS.Mark("not an extension")
        END
      ELSE ORS.Mark("incompatible types")
      END
    ELSIF ~guard THEN res := OCG.GenTypeTest(x, NIL, FALSE, FALSE)
    END
  ELSE ORS.Mark("type mismatch")
  END;
  IF ~guard THEN x.type := OCB.boolType END
RETURN res END TypeTest;

PROCEDURE selector(x: OCB.Object): OCB.Object;
VAR y: OCB.Object;  obj: OCB.Object;
BEGIN
  WHILE (sym = ORS.lbrak) OR (sym = ORS.period) OR (sym = ORS.arrow)
     OR (sym = ORS.lparen) & (x.type.form IN {OCB.Record, OCB.Pointer}) DO
    IF sym = ORS.lbrak THEN
      REPEAT ORS.Get(sym);  y := expression();
        IF x.type.form = OCB.Array THEN
          CheckInt(y);  x := GenExpr(ORS.lbrak, x, y);  x.type := x.type.base
        ELSE ORS.Mark("not an array")
        END
      UNTIL sym # ORS.comma;
      Check(ORS.rbrak, "missing ]")
    ELSIF sym = ORS.period THEN ORS.Get(sym);
      IF sym = ORS.ident THEN
        IF x.type.form = OCB.Pointer THEN
          x := GenExpr(ORS.arrow, x, NIL);  x.type := x.type.base
        END;
        IF x.type.form = OCB.Record THEN
          obj := OCB.thisfield(x.type);  ORS.Get(sym);
          IF obj # NIL THEN
            x := GenExpr(ORS.period, x, obj);  x.type := obj.type
          ELSE ORS.Mark("undef")
          END
        ELSE ORS.Mark("not a record")
        END
      ELSE ORS.Mark("ident?")
      END
    ELSIF sym = ORS.arrow THEN
      ORS.Get(sym);
      IF x.type.form = OCB.Pointer THEN x := GenExpr(ORS.arrow, x, NIL);  x.type := x.type.base
      ELSE ORS.Mark("not a pointer")
      END
    ELSIF (sym = ORS.lparen) & (x.type.form IN {OCB.Record, OCB.Pointer}) THEN (*type guard*)
      ORS.Get(sym);
      IF sym = ORS.ident THEN
        qualident(obj);
        IF obj.class = OCB.Typ THEN x := TypeTest(x, obj.type, TRUE)
        ELSE ORS.Mark("guard type expected")
        END
      ELSE ORS.Mark("not an identifier")
      END;
      Check(ORS.rparen, " ) missing")
    END
  END
RETURN x END selector;

PROCEDURE EqualSignatures(t0, t1: OCB.Type): BOOLEAN;
VAR p0, p1: OCB.Object;  com: BOOLEAN;
BEGIN com := TRUE;
  IF (t0.base = t1.base) & (t0.nofpar = t1.nofpar) THEN
    p0 := t0.dsc;  p1 := t1.dsc;
    WHILE p0 # NIL DO
      IF (p0.class = p1.class) & (p0.rdo = p1.rdo) &
        ((p0.type = p1.type) OR
        (p0.type.form = OCB.Array) & (p1.type.form = OCB.Array) & (p0.type.len = p1.type.len) & (p0.type.base = p1.type.base) OR
        (p0.type.form = OCB.ProcAdr) & (p1.type.form = OCB.ProcAdr) & EqualSignatures(p0.type, p1.type))
      THEN p0 := p0.next;  p1 := p1.next
      ELSE p0 := NIL;  com := FALSE
      END
    END
  ELSE com := FALSE
  END;
  RETURN com
END EqualSignatures;

PROCEDURE CompTypes(t0, t1: OCB.Type;  varpar: BOOLEAN): BOOLEAN;
BEGIN (*check for assignment compatibility*)
  RETURN (t0 = t1)    (*openarray assignment disallowed in ORG*)
    OR (t0.form = OCB.Array) & (t1.form = OCB.Array) & (t0.base =  t1.base) & (t0.len = t1.len)
    OR (t0.form = OCB.Record) & (t1.form = OCB.Record)  & IsExtension(t0, t1)
    OR ~varpar &
      (  (t0.form = OCB.Pointer) & (t1.form = OCB.Pointer) & IsExtension(t0.base, t1.base)
      OR (t0.form = OCB.ProcAdr) & (t1.form = OCB.ProcAdr) & EqualSignatures(t0, t1)
      OR (t0.form IN {OCB.Pointer, OCB.ProcAdr}) & (t1.form = OCB.NilTyp))
      OR (t0.form = OCB.Int64) & (t1.form = OCB.Int64)  (* All int types are value compatible *)
END CompTypes;


(*
PROCEDURE NewParam(formal, actual: OCB.Object): OCB.Object;
RETURN OCG.NewItem(OCB.Param, 0, 0, NIL, formal, actual) END NewParam;
*)

PROCEDURE Parameter(prep, par: OCB.Object);
VAR x: OCB.Object;  varpar: BOOLEAN;
BEGIN
  x := expression();
  IF par # NIL THEN
    varpar := par.class = OCB.RefPar;
    IF CompTypes(par.type, x.type, varpar) THEN

      IF varpar & ~par.rdo THEN CheckReadOnly(x) END;
      OCG.GenParam(prep, par, x);

    ELSIF (par.type.form = OCB.Array) & (par.type.len < 0)
        & (   (x.type.form = OCB.Array)  & (x.type.base = par.type.base)
           OR (x.type.form = OCB.StrAdr) & varpar & par.rdo & (par.type.base.form = OCB.Char)
           OR (par.type.base = OCB.byteType)) THEN

      IF ~par.rdo THEN CheckReadOnly(x) END;
      OCG.GenParam(prep, par, x);

    ELSIF ~varpar & (par.type.form = OCB.Int64) & (x.type.form = OCB.Int64) THEN

      ASSERT(FALSE); (* should not reach here because it Looks like *)
                     (* this case is already handled in comptypes   *)
      OCG.GenParam(prep, par, x);

    ELSIF OCG.IsSingleCharacterString(x)
        & (par.class = OCB.ValPar) & (par.type.form = OCB.Char) THEN

      OCG.StrToChar(x);  OCG.GenParam(prep, par, x);

    ELSE
      ORS.Mark("incompatible parameters");
    END
  END
END Parameter;

PROCEDURE ParamList(prep: OCB.Object);
VAR par: OCB.Object;
BEGIN
  ASSERT(prep.type # NIL);  ASSERT(prep.class = OCB.CallSite);
  par := prep.type.dsc;
  IF sym # ORS.rparen THEN
    Parameter(prep, par);
    WHILE sym <= ORS.comma DO
      Check(ORS.comma, "comma?");
      IF par # NIL THEN par := par.next END;
      Parameter(prep, par);

IF FALSE THEN END  (* Crashes without this. Haven't found out why. May be to *)
                   (* do with true chain, false chain, and jump generation?  *)
                   (* TODO remove with new code generator                    *)

    END;
    Check(ORS.rparen, ") missing")
  ELSE ORS.Get(sym);
  END;
  IF    prep.val < prep.type.nofpar THEN ORS.Mark("too few params")
  ELSIF prep.val > prep.type.nofpar THEN ORS.Mark("too many params")
  END
END ParamList;

PROCEDURE StandFunc(fct: INTEGER;  restyp: OCB.Type): OCB.Object;
VAR x, y: OCB.Object;  n, npar: INTEGER;
BEGIN Check(ORS.lparen, "missing (");
  npar := fct MOD 10;  fct := fct DIV 10;  x := expression();  n := 1;
  WHILE sym = ORS.comma DO ORS.Get(sym);  y := expression();  INC(n) END;
  Check(ORS.rparen, "missing )");
  IF n = npar THEN
    IF    fct = 0 THEN (*ABS*)   IF x.type.form IN {OCB.Int64, OCB.Real} THEN
                                   x := OCG.GenStdFunc(fct, x, NIL);  restyp := x.type
                                 ELSE ORS.Mark("bad type") END
    ELSIF fct = 1 THEN (*ODD*)   CheckInt(x);   x := OCG.GenStdFunc(fct, x, NIL)
    ELSIF fct = 2 THEN (*FLOOR*) CheckReal(x);  x := OCG.GenStdFunc(fct, x, NIL)
    ELSIF fct = 3 THEN (*FLT*)   CheckInt(x);   x := OCG.GenStdFunc(fct, x, NIL)
    ELSIF fct = 4 THEN (*ORD*)   IF x.type.form <= OCB.ProcAdr THEN x := OCG.GenStdFunc(fct, x, NIL)
                                 ELSIF OCG.IsSingleCharacterString(x) THEN OCG.StrToChar(x)
                                 ELSE ORS.Mark("bad type")
                                 END
    ELSIF fct = 5 THEN (*CHR*)   CheckInt(x);  x := OCG.GenStdFunc(fct, x, NIL)
    ELSIF fct = 6 THEN (*LEN*)   IF x.type.form = OCB.Array THEN x := OCG.GenStdFunc(fct, x, NIL)
                                 ELSE ORS.Mark("not an array") END
    ELSIF fct IN {7, 8, 9} THEN (*LSL, ASR, ROR*)
                                 CheckInt(y);
                                 IF x.type.form IN {OCB.Int64, OCB.Set} THEN
                                   x := OCG.GenStdFunc(fct, x, y);  restyp := x.type
                                 ELSE ORS.Mark("bad type") END
    ELSIF fct = 11 THEN (*ADC*)  x := OCG.GenStdFunc(fct, x, y)
    ELSIF fct = 12 THEN (*SBC*)  x := OCG.GenStdFunc(fct, x, y)
    ELSIF fct = 13 THEN (*UML*)  x := OCG.GenStdFunc(fct, x, y)
    ELSIF fct = 14 THEN (*BIT*)  CheckInt(x);    CheckInt(y);  x := OCG.GenStdFunc(fct, x, y)
    ELSIF fct = 15 THEN (*REG*)  CheckConst(x);  CheckInt(x);  x := OCG.GenStdFunc(fct, x, NIL)
    ELSIF fct = 16 THEN (*VAL*)  IF x.type.size <= y.type.size THEN restyp := x.type;  x := y
                                 ELSE ORS.Mark("casting not allowed") END
    ELSIF fct = 17 THEN (*ADR*)  x := OCG.GenStdFunc(fct, x, NIL)
    ELSIF fct = 18 THEN (*SIZE*) IF x.class = OCB.Typ THEN x := OCG.NewItem(OCB.Const, x.type.size, 0, OCB.intType, x, NIL)
                                 ELSE ORS.Mark("must be a type") END
    ELSIF fct = 19 THEN (*COND*) CheckConst(x);  CheckInt(x);  x := OCG.GenStdFunc(fct, x, NIL)
    END;
    x.type := restyp
  ELSE ORS.Mark("wrong nof params")
  END
RETURN x END StandFunc;

PROCEDURE element(): OCB.Object;
VAR x, y: OCB.Object;
BEGIN x := expression();  CheckSetVal(x);
  IF sym = ORS.upto THEN
    ORS.Get(sym);  y := expression();  CheckSetVal(y);
    x := GenExpr(ORS.upto, x, y)
  ELSE
    x := GenExpr(ORS.rbrace, x, NIL)
  END;
  x.type := OCB.setType
RETURN x END element;

PROCEDURE set(): OCB.Object;
VAR x: OCB.Object;
BEGIN
  IF sym >= ORS.if THEN
    IF sym # ORS.rbrace THEN ORS.Mark(" } missing") END;
    x := OCG.NewItem(OCB.Const, 0, 0, OCB.setType, NIL, NIL) (*empty set*)
  ELSE x := element();
    WHILE (sym < ORS.rparen) OR (sym > ORS.rbrace) DO
      IF    sym = ORS.comma  THEN ORS.Get(sym)
      ELSIF sym # ORS.rbrace THEN ORS.Mark("missing comma")
      END;
      x := GenExpr(ORS.comma, x, element())
    END
  END
RETURN x END set;

PROCEDURE factor(): OCB.Object;
VAR x: OCB.Object;
BEGIN (*sync*)
  IF (sym < ORS.char) OR (sym > ORS.ident) THEN
    ORS.Mark("expression expected");
    REPEAT ORS.Get(sym) UNTIL (sym >= ORS.char) & (sym <= ORS.for) OR (sym >= ORS.then)
  END;
  IF sym = ORS.ident THEN
    qualident(x);
    IF x.class = OCB.StdFunc THEN x := StandFunc(x.val, x.type)
    ELSE CheckAccess(x, Level);
      x := selector(x);
      IF sym = ORS.lparen THEN
        ORS.Get(sym);
        IF (x.type.form = OCB.ProcAdr) & (x.type.base.form # OCB.NoTyp) THEN
          OCG.GenCallSite(x);  ParamList(x);
          x.type := x.type.base
        ELSE ORS.Mark("not a function");  ParamList(x)
        END
      END
    END
  ELSIF sym = ORS.int    THEN x := OCG.NewItem(OCB.Const, ORS.ival, 0, OCB.intType,  NIL, NIL);  ORS.Get(sym)
  ELSIF sym = ORS.char   THEN x := OCG.NewItem(OCB.Const, ORS.ival, 0, OCB.charType, NIL, NIL);  ORS.Get(sym)
  ELSIF sym = ORS.nil    THEN x := OCG.NewItem(OCB.Const, 0,        0, OCB.nilType,  NIL, NIL);  ORS.Get(sym)
  ELSIF sym = ORS.false  THEN x := OCG.NewItem(OCB.Const, 0,        0, OCB.boolType, NIL, NIL);  ORS.Get(sym)
  ELSIF sym = ORS.true   THEN x := OCG.NewItem(OCB.Const, 1,        0, OCB.boolType, NIL, NIL);  ORS.Get(sym)
  ELSIF sym = ORS.real   THEN x := OCG.MakeRealItem(ORS.rval);                                  ORS.Get(sym)
  ELSIF sym = ORS.string THEN x := OCG.MakeStringItem(ORS.slen);                                ORS.Get(sym)
  ELSIF sym = ORS.lparen THEN ORS.Get(sym);  x := expression();  Check(ORS.rparen, "missing )")
  ELSIF sym = ORS.lbrace THEN ORS.Get(sym);  x := set();     Check(ORS.rbrace, "missing }")
  ELSIF sym = ORS.not    THEN ORS.Get(sym);  x := factor();  CheckBool(x);
                              x := GenExpr(ORS.not, x, NIL)
  ELSE ORS.Mark("not a factor");  x := OCG.NewItem(OCB.Const, 0, 0, OCB.intType, NIL, NIL)
  END
RETURN x END factor;

PROCEDURE term(): OCB.Object;
VAR x, y: OCB.Object;  op, f: INTEGER;
BEGIN x := factor(); f := x.type.form;
  WHILE (sym >= ORS.times) & (sym <= ORS.and) DO
    op := sym;  ORS.Get(sym);
    IF op = ORS.times THEN
      IF    f = OCB.Int64 THEN y := factor();  CheckInt(y);   x := GenExpr(op, x, y)
      ELSIF f = OCB.Real  THEN y := factor();  CheckReal(y);  x := GenExpr(op, x, y)
      ELSIF f = OCB.Set   THEN y := factor();  CheckSet(y);   x := GenExpr(op, x, y)
      ELSE ORS.Mark("bad type")
      END
    ELSIF (op = ORS.div) OR (op = ORS.mod) THEN
      CheckInt(x);  y := factor();  CheckInt(y);  x := GenExpr(op, x, y)
    ELSIF op = ORS.rdiv THEN
      IF    f = OCB.Real THEN y := factor();  CheckReal(y);  x := GenExpr(op, x, y)
      ELSIF f = OCB.Set  THEN y := factor();  CheckSet(y);   x := GenExpr(op, x, y)
      ELSE ORS.Mark("bad type")
      END
    ELSE (*op = and*) CheckBool(x);
       (*OXG.And1(x); y := factor();  CheckBool(y);  OXG.And2(x, y)*)
       y := factor();  CheckBool(y);  x := GenExpr(op, x, y)
    END
  END
RETURN x END term;

PROCEDURE SimpleExpression(): OCB.Object;
VAR x, y: OCB.Object;  op: INTEGER;
BEGIN
  IF sym = ORS.minus THEN ORS.Get(sym); x := term();
    IF x.type.form IN {OCB.Int64, OCB.Real, OCB.Set} THEN x := GenExpr(ORS.minus, x, NIL)
    ELSE CheckInt(x) END
  ELSIF sym = ORS.plus THEN ORS.Get(sym); x := term();
  ELSE x := term()
  END;
  WHILE (sym >= ORS.plus) & (sym <= ORS.or) DO
    op := sym;  ORS.Get(sym);
    IF op = ORS.or THEN CheckBool(x);  y := term();  CheckBool(y);  x := GenExpr(op, x, y);
      (*OXG.Or1(x);  OXG.Or2(x, y)*)
    ELSIF x.type.form = OCB.Int64 THEN y := term();  CheckInt(y);   x := GenExpr(op, x, y)
    ELSIF x.type.form = OCB.Real  THEN y := term();  CheckReal(y);  x := GenExpr(op, x, y)
    ELSE                 CheckSet(x);  y := term();  CheckSet(y);   x := GenExpr(op, x, y)
    END
  END
RETURN x END SimpleExpression;

PROCEDURE expression0(): OCB.Object;
VAR x, y: OCB.Object;  obj: OCB.Object;  rel, xf, yf: INTEGER;
BEGIN x := SimpleExpression();
  IF (sym >= ORS.eql) & (sym <= ORS.geq) THEN
    rel := sym;  ORS.Get(sym);  y := SimpleExpression();  xf := x.type.form;  yf := y.type.form;
    IF x.type = y.type THEN
      IF (xf IN {OCB.Char, OCB.Int64}) THEN x := GenExpr(rel, x, y)
      ELSIF xf = OCB.Real              THEN x := GenExpr(rel, x, y)
      ELSIF (xf IN {OCB.Set, OCB.Pointer, OCB.ProcAdr, OCB.NilTyp, OCB.Bool}) THEN
        IF rel <= ORS.neq              THEN x := GenExpr(rel, x, y)
        ELSE ORS.Mark("only = or #")
        END
      ELSIF (xf = OCB.Array) & (x.type.base.form = OCB.Char) OR (xf = OCB.StrAdr) THEN
        x := GenExpr(rel, x, y)
      ELSE ORS.Mark("illegal comparison")
      END
    ELSIF (xf IN {OCB.Pointer, OCB.ProcAdr}) & (yf = OCB.NilTyp)
       OR (yf IN {OCB.Pointer, OCB.ProcAdr}) & (xf = OCB.NilTyp) THEN
      IF rel <= ORS.neq THEN x := GenExpr(rel, x, y)
      ELSE ORS.Mark("only = or #")
      END
    ELSIF (xf = OCB.Pointer) & (yf = OCB.Pointer) & (IsExtension(x.type.base, y.type.base)
       OR IsExtension(y.type.base, x.type.base))
       OR (xf = OCB.ProcAdr) & (yf = OCB.ProcAdr) & EqualSignatures(x.type, y.type) THEN
      IF rel <= ORS.neq THEN x := GenExpr(rel, x, y)
      ELSE ORS.Mark("only = or #")
      END
    ELSIF (xf = OCB.Array) & (x.type.base.form = OCB.Char)
        & ((yf = OCB.StrAdr) OR (yf = OCB.Array) & (y.type.base.form = OCB.Char))
        OR (yf = OCB.Array) & (y.type.base.form = OCB.Char) & (xf = OCB.StrAdr) THEN
      x := GenExpr(rel, x, y)
    ELSIF (xf = OCB.Char) & OCG.IsSingleCharacterString(y) THEN
      OCG.StrToChar(y);  x := GenExpr(rel, x, y)
    ELSIF (yf = OCB.Char) & OCG.IsSingleCharacterString(x) THEN
      OCG.StrToChar(x);  x := GenExpr(rel, x, y)
    ELSIF (xf = OCB.Int64) & (yf = OCB.Int64) THEN
      x := GenExpr(rel, x, y)  (*BYTE*)
    ELSE
      ORS.Mark("illegal comparison")
    END;
    (*x.type := OCB.boolType*)
  ELSIF sym = ORS.in THEN
    ORS.Get(sym);  CheckInt(x);  y := SimpleExpression();  CheckSet(y);
    x := GenExpr(ORS.in, x, y);  x.type := OCB.boolType
  ELSIF sym = ORS.is THEN
    ORS.Get(sym);  qualident(obj);
    x := TypeTest(x, obj.type, FALSE);  x.type := OCB.boolType
  END
RETURN x END expression0;

(* statements *)

PROCEDURE StandProc(VAR obj: OCB.Object);
VAR
    pno:       INTEGER;
    nap, npar: INTEGER;  (*nof actual/formal parameters*)
    x, y, z:   OCB.Object;
BEGIN
  (*
  ws("StandProc("); wa;  OCG.wObject(obj);  ws(")");
  IF obj.next # NIL THEN ws(", next ");  wa;  OCG.wObject(obj.next) END;
  IF obj.dsc  # NIL THEN ws(", dsc ");   wa;  OCG.wObject(obj.dsc)  END;
  wn;
  *)

  Check(ORS.lparen, "missing (");
  pno  := obj.val;  npar := pno MOD 10;  pno := pno DIV 10;
  x := expression();  nap := 1;
  IF sym = ORS.comma THEN
    ORS.Get(sym);
    y := expression();
    nap := 2;
    (*z.type := OCB.noType;*)
    WHILE sym = ORS.comma DO ORS.Get(sym);  z := expression();  INC(nap) END
  ELSE y := NIL
  END;
  Check(ORS.rparen, "missing )");
  IF (npar = nap) OR (pno IN {0, 1}) THEN
    IF pno IN {0, 1} THEN (*INC, DEC*)
      CheckInt(x);  CheckReadOnly(x);  IF y # NIL THEN CheckInt(y) END;
      OCG.GenStdProc(pno, x, y)
    ELSIF pno IN {2, 3} THEN (*INCL, EXCL*)
      CheckSet(x);  CheckReadOnly(x);  CheckInt(y);
      OCG.GenStdProc(pno, x, y)
    ELSIF pno = 4 THEN CheckBool(x);  OCG.GenAssert(x)
    ELSIF pno = 5 THEN(*NEW*) CheckReadOnly(x);
       IF (x.type.form = OCB.Pointer) & (x.type.base.form = OCB.Record) THEN
         OCG.GenStdProc(pno, x, NIL)
       ELSE
         ORS.Mark("not a pointer to record")
       END
    ELSIF pno = 6  THEN CheckReal(x);  CheckInt(y);  CheckReadOnly(x);  OCG.GenStdProc(pno, x, y)
    ELSIF pno = 7  THEN CheckReal(x);  CheckInt(y);  CheckReadOnly(x);  OCG.GenStdProc(pno, x, y)
    ELSIF pno = 8  THEN IF x.type.form <= OCB.Set THEN OCG.GenStdProc(pno, x, NIL) ELSE ORS.Mark("bad type") END
    ELSIF pno = 10 THEN CheckInt(x);  OCG.GenStdProc(pno, x, y)
    ELSIF pno = 11 THEN CheckInt(x);  OCG.GenStdProc(pno, x, y)
    ELSIF pno = 12 THEN CheckInt(x);  CheckInt(y);  CheckInt(z);  OCG.GenCopy(x, y, z)
    (*ELSIF pno = 13 THEN CheckConst(x);  CheckInt(x);  OXG.LDPSR(x)*)
    ELSIF pno = 14 THEN CheckInt(x);  OCG.GenStdProc(pno, x, y)
    ELSE ORS.Mark("Standard procedure not implement")
    END
  ELSE ORS.Mark("wrong number of parameters")
  END
END StandProc;

PROCEDURE StatSequence(): OCB.Object;
VAR
  obj:       OCB.Object;
  orgtype:   OCB.Type;  (*original type of case var*)
  y, z, w:   OCB.Object;
  jmp, lbl:  INTEGER;
  save, res: OCB.Object;

  PROCEDURE TypeCase(VAR obj: OCB.Object);
  VAR typobj: OCB.Object;
  BEGIN
    IF sym = ORS.ident THEN
      CheckAccess(obj, Level);
      qualident(typobj);
      IF typobj.class # OCB.Typ THEN ORS.Mark("not a type") END;
      GenStmt(0, OCG.NewItem(OCB.Caseval, 0, 0, NIL, TypeTest(obj, typobj.type, FALSE), StatSequence()));
      obj.type := typobj.type;
      (* OXG.CFJump(x);  TODO jumps *)
      Check(ORS.colon, ": expected");
    ELSE
      (* OXG.CFJump(x);  TODO jumps *)
      ORS.Mark("type id expected")
    END
   END TypeCase;

  PROCEDURE SkipCase;
  BEGIN
    WHILE sym # ORS.colon DO ORS.Get(sym) END;
    ORS.Get(sym)
  END SkipCase;

BEGIN (* StatSequence *)
  save := CodeSeq;  CodeSeq := NIL;
  REPEAT (*sync*) obj := NIL;
    IF ~((sym >= ORS.ident)  & (sym <= ORS.for) OR (sym >= ORS.semicolon)) THEN
      ORS.Mark("statement expected");
      REPEAT ORS.Get(sym) UNTIL (sym >= ORS.ident)
    END;
    IF sym = ORS.ident THEN
      qualident(obj);
      IF obj.class = OCB.StdProc THEN StandProc(obj)
      ELSE
        CheckAccess(obj, Level);
        obj := selector(obj);

        IF sym IN {ORS.becomes, ORS.eql} THEN (* assignment *)

          IF sym = ORS.eql THEN ORS.Mark("should be :=") END;
          ORS.Get(sym);  CheckReadOnly(obj);  y := expression();
          IF (obj.type.form = OCB.Char) & OCG.IsSingleCharacterString(y) THEN
            OCG.StrToChar(y)
          END;
          IF CompTypes(obj.type, y.type, FALSE)
          OR (obj.type.form = OCB.Array) & (y.type.form = OCB.Array) & (obj.type.base = y.type.base) & (y.type.len < 0)
          OR (obj.type.form = OCB.Array) & (obj.type.base.form = OCB.Char) & (y.type.form = OCB.StrAdr)
          OR (obj.type.form = OCB.Int64) & (y.type.form = OCB.Int64) THEN
            GenStmt(0, OCG.GenAssign(obj, y))
          ELSE
            ORS.Mark("illegal assignment")
          END

        ELSIF sym = ORS.lparen THEN (*procedure call*)

          ORS.Get(sym);
          IF (obj.type.form = OCB.ProcAdr) & (obj.type.base.form = OCB.NoTyp) THEN
            OCG.GenCallSite(obj);  ParamList(obj);  GenStmt(0, obj)
          ELSE
            ORS.Mark("not a procedure");  ParamList(obj)
          END

        ELSIF obj.type.form = OCB.ProcAdr THEN (*procedure call without parameters*)

          IF obj.type.nofpar > 0 THEN ORS.Mark("missing parameters") END;
          IF obj.type.base.form = OCB.NoTyp THEN
            OCG.GenCallSite(obj);  GenStmt(0, obj)
          ELSE
            ORS.Mark("not a procedure")
          END

        ELSIF obj.class = OCB.Typ THEN ORS.Mark("illegal assignment")
        ELSE ORS.Mark("not a procedure")
        END
      END
    ELSIF sym = ORS.if THEN
      ORS.Get(sym); obj := expression(); CheckBool(obj);
      (* OXG.CFJump(x);  TODO jumps *)
      Check(ORS.then, "missing THEN");
      y := StatSequence();  jmp := -1;
      GenStmt(0, OCG.NewItem(OCB.If, 0,0,NIL, obj, y));
      WHILE sym = ORS.elsif DO
        ORS.Get(sym);
        (* OXG.FJump(x); OXG.FixLink(x(OXG.ConditionArg).falsechain);  TODO jumps *)
        obj := expression();  CheckBool(obj);
        (* OXG.CFJump(x);  TODO jumps *)
        Check(ORS.then, "missing THEN");
        y := StatSequence();
        GenStmt(0, OCG.NewItem(OCB.Elsif, 0,0,NIL, obj, y))
      END;
      IF sym = ORS.else THEN
        ORS.Get(sym);
        (* OXG.FJump(jmp); OXG.FixLink(x(OXG.ConditionArg).falsechain);  TODO jumps *)
        y := StatSequence();
        GenStmt(0, OCG.NewItem(OCB.Else, 0,0,NIL, NIL, y))
      ELSE
        (* OXG.FixLink(x(OXG.ConditionArg).falsechain)  TODO jumps *)
      END;
      (* OXG.FixLink(jmp);  TODO jumps *)
      Check(ORS.end, "missing END")
    ELSIF sym = ORS.while THEN
      (* lbl := OXG.MarkLoopStart();  TODO jumps *)
      ORS.Get(sym); obj := expression(); CheckBool(obj);
      (* OXG.CFJump(x);  TODO jumps *)
      Check(ORS.do, "missing DO");  y := StatSequence();
      (* OXG.BJump(lbl);  TODO jumps *)
      WHILE sym = ORS.elsif DO
        ORS.Get(sym);
        (* OXG.FixLink(x(OXG.ConditionArg).falsechain);    TODO jumps *)
        obj := expression();  CheckBool(obj);
        (* OXG.CFJump(x);  TODO jumps *)
        Check(ORS.do, "missing DO");  y := StatSequence();
        (* OXG.BJump(lbl)  TODO jumps *)
      END;
      (* OXG.FixLink(x(OXG.ConditionArg).falsechain);  TODO jumps *)
      Check(ORS.end, "missing END")
    ELSIF sym = ORS.repeat THEN
      (* lbl := OXG.MarkLoopStart(); *)
      ORS.Get(sym);  y := StatSequence();
      IF sym = ORS.until THEN
        ORS.Get(sym);  obj := expression();  CheckBool(obj);
        (* OXG.CBJump(x, lbl)  TODO jumps *)
      ELSE ORS.Mark("missing UNTIL")
      END
    ELSIF sym = ORS.for THEN  (* FOR val(x) := ini(y) TO lim(z) BY step(w) *)
      ORS.Get(sym);
      IF sym # ORS.ident THEN ORS.Mark("identifier expected")
      ELSE
        qualident(obj);  CheckAccess(obj, Level);  CheckInt(obj);  CheckReadOnly(obj);
        IF sym # ORS.becomes THEN ORS.Mark(":= expected")
        ELSE
          ORS.Get(sym);  y := expression();  CheckInt(y);
          OCG.GenFor0(obj, y);
          (* lbl := OXG.MarkLoopStart(); *)
          Check(ORS.to, "TO expected");
          z := expression();  CheckInt(z);  obj.rdo := TRUE;
          IF sym = ORS.by THEN ORS.Get(sym); w := expression(); CheckConst(w); CheckInt(w)
          ELSE w := OCG.NewItem(OCB.Const, 1, 0, OCB.intType, NIL, NIL)
          END;
          Check(ORS.do, "DO expected");
          OCG.GenFor1(obj, y, z, w, jmp);  (* val, ini, lim, step *)
          y := StatSequence(); Check(ORS.end, "END expected");
          OCG.GenFor2(obj, y, w);
          (* OXG.BJump(lbl); OXG.FixLink(jmp);  TODO jumps *)
          obj.rdo := FALSE
        END
      END
    ELSIF sym = ORS.case THEN
      ORS.Get(sym);
      IF sym = ORS.ident THEN
        qualident(obj);  orgtype := obj.type;
        IF (orgtype.form = OCB.Pointer) OR (orgtype.form = OCB.Record) & (obj.class = OCB.RefPar) THEN
          Check(ORS.of, "OF expected");  TypeCase(obj);
          jmp := -1;
          WHILE sym = ORS.bar DO
            ORS.Get(sym);
            (* OXG.FJump(jmp);  OXG.FixLink(x(OXG.ConditionArg).falsechain);  TODO jumps *)
            obj.type := orgtype;  TypeCase(obj)
          END;
          (* OXG.FixLink(x(OXG.ConditionArg).falsechain);  OXG.FixLink(jmp);  TODO jumps *)
          obj.type := orgtype
        ELSE ORS.Mark("numeric case not implemented");
          Check(ORS.of, "OF expected");  SkipCase;
          WHILE sym = ORS.bar DO SkipCase END
        END
      ELSE ORS.Mark("ident expected")
      END;
      Check(ORS.end, "missing END")
    END;
    IF sym = ORS.semicolon THEN ORS.Get(sym)
    ELSIF sym < ORS.semicolon THEN ORS.Mark("missing semicolon?")
    END
  UNTIL sym > ORS.semicolon;
  res := CodeSeq;  CodeSeq := save
RETURN res END StatSequence;

(* Types and declarations *)

PROCEDURE IdentList(class: INTEGER;  VAR first: OCB.Object);
VAR obj: OCB.Object;
BEGIN
  IF sym = ORS.ident THEN
    OCB.NewObj(first, ORS.id, class);  ORS.Get(sym);  CheckExport(first.expo);
    WHILE sym = ORS.comma DO
      ORS.Get(sym);
      IF sym = ORS.ident THEN OCB.NewObj(obj, ORS.id, class);  ORS.Get(sym);  CheckExport(obj.expo)
      ELSE ORS.Mark("ident?")
      END
    END;
    IF sym = ORS.colon THEN ORS.Get(sym) ELSE ORS.Mark(":?") END
  ELSE first := NIL
  END
END IdentList;

PROCEDURE ArrayType(VAR type: OCB.Type);
VAR x: OCB.Object;  typ: OCB.Type;  len: INTEGER;
BEGIN NEW(typ); typ.form := OCB.NoTyp;
  x := expression();
  IF (x.class = OCB.Const) & (x.type.form = OCB.Int64)
   & (x.val >= 0) THEN len := x.val
  ELSE len := 1;  ORS.Mark("not a valid length")
  END;
  IF sym = ORS.of THEN ORS.Get(sym); Type(typ.base);
    IF (typ.base.form = OCB.Array) & (typ.base.len < 0) THEN ORS.Mark("dyn array not allowed") END
  ELSIF sym = ORS.comma THEN ORS.Get(sym); ArrayType(typ.base)
  ELSE ORS.Mark("missing OF"); typ.base := OCB.intType
  END;
  typ.size := len * typ.base.size;  (* Was: (len * typ.base.size + 7) DIV 8 * 8 *)
  typ.form := OCB.Array; typ.len := len; type := typ
END ArrayType;

PROCEDURE Alignment(type: OCB.Type): INTEGER;
VAR result, align: INTEGER;  field: OCB.Object;
BEGIN
  IF type.form = OCB.Array THEN
    result := Alignment(type.base)
  ELSIF type.form = OCB.Record THEN
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

PROCEDURE RecordType(VAR type: OCB.Type; hosted: BYTE);
VAR obj, obj0, new, bot, base: OCB.Object;
    typ, tp: OCB.Type;
    offset, off, align, n: INTEGER;
BEGIN NEW(typ);
  typ.form   := OCB.NoTyp;
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
      IF base.class = OCB.Typ THEN
        IF base.type.form = OCB.Record THEN typ.base := base.type
        ELSE typ.base := OCB.intType;  ORS.Mark("invalid extension")
        END;
        typ.nofpar := typ.base.nofpar + 1;  (*"nofpar" here abused for extension level*)
        bot := typ.base.dsc; offset := typ.base.size
      ELSE ORS.Mark("type expected")
      END
    ELSE ORS.Mark("ident expected")
    END;
    Check(ORS.rparen, "missing )")
  END;
  WHILE sym = ORS.ident DO  (*fields*)
    n := 0;  obj := bot;
    WHILE sym = ORS.ident DO
      obj0 := obj;
      WHILE (obj0 # NIL) & (obj0.name # ORS.id) DO obj0 := obj0.next END;
      IF obj0 # NIL THEN ORS.Mark("mult def") END;
      NEW(new);  new.name := ORS.id;  new.class := OCB.Field;  new.next := obj;  obj := new;  INC(n);
      ORS.Get(sym);  CheckExport(new.expo);
      IF (sym # ORS.comma) & (sym # ORS.colon) THEN ORS.Mark("comma expected")
      ELSIF sym = ORS.comma THEN ORS.Get(sym)
      END
    END;
    Check(ORS.colon, "colon expected");
    Type(tp);
    IF (tp.form = OCB.Array) & (tp.len < 0) THEN ORS.Mark("dyn array not allowed") END;
    Align(offset, Alignment(tp));
    offset := offset + n * tp.size;  off := offset;  obj0 := obj;
    WHILE obj0 # bot DO
      obj0.type := tp;   obj0.modno := 0;  off := off - tp.size;
      obj0.val  := off;  obj0 := obj0.next
    END;
    bot := obj;
    IF sym = ORS.semicolon THEN ORS.Get(sym) ELSIF sym # ORS.end THEN ORS.Mark(" ;  or END") END
  END;
  typ.form := OCB.Record;  typ.dsc := bot;  typ.size := offset;
  type := typ
END RecordType;

(* Formal parameter section - one or more comma separated vars of one type *)
(* Each parameter takes either one or two 64 bit cells on the stack        *)
(* Parameters are numbered in obj.val starting at 0                        *)
PROCEDURE FPSection(VAR nofpar: INTEGER);
VAR obj, first: OCB.Object;  tp: OCB.Type;  cl: INTEGER;  rdo: BOOLEAN;
BEGIN
  IF sym = ORS.var THEN ORS.Get(sym); cl := OCB.RefPar ELSE cl := OCB.ValPar END;
  IdentList(cl, first);  FormalType(tp, 0);  rdo := FALSE;
  IF (cl = OCB.ValPar) & (tp.form >= OCB.Array) THEN
    cl := OCB.RefPar;  rdo := TRUE
  END;
  obj := first;
  WHILE obj # NIL DO
    obj.val   := nofpar;  INC(nofpar);
    obj.class := cl;      obj.type  := tp;
    obj.rdo   := rdo;     obj.modno := -Level;
    obj := obj.next
  END
END FPSection;

PROCEDURE ProcedureType(ptype: OCB.Type);
VAR obj: OCB.Object;  nofpar: INTEGER;
BEGIN ptype.base := OCB.noType;  nofpar := 0;  ptype.dsc := NIL;
  IF sym = ORS.lparen THEN                     (* Parse formal parameters *)
    ORS.Get(sym);
    IF sym = ORS.rparen THEN ORS.Get(sym)
    ELSE
      FPSection(nofpar);
      WHILE sym = ORS.semicolon DO ORS.Get(sym); FPSection(nofpar) END;
      Check(ORS.rparen, "missing )")
    END;
    IF sym = ORS.colon THEN                    (* Parse function result type *)
      ORS.Get(sym);
      IF sym = ORS.ident THEN
        qualident(obj);  ptype.base := obj.type;
        IF ~((obj.class = OCB.Typ) & (obj.type.form IN {OCB.Byte .. OCB.Pointer, OCB.ProcAdr})) THEN
          ORS.Mark("illegal function type")
        END
      ELSE ORS.Mark("type identifier expected")
      END
    END
  END;
  ptype.nofpar := nofpar;
END ProcedureType;

PROCEDURE FormalType0(VAR typ: OCB.Type;  dim: INTEGER);
VAR obj: OCB.Object;  dmy: INTEGER;
BEGIN
  IF sym = ORS.ident THEN
    qualident(obj);
    IF obj.class = OCB.Typ THEN typ := obj.type ELSE ORS.Mark("not a type");  typ := OCB.intType END
  ELSIF sym = ORS.array THEN
    ORS.Get(sym);  Check(ORS.of, "OF ?");
    IF dim >= 1 THEN ORS.Mark("multi-dimensional open arrays not implemented") END;
    NEW(typ);  typ.form := OCB.Array;  typ.len := -1;  typ.size := 2*OCB.WordSize;
    FormalType(typ.base, dim+1)
  ELSIF sym = ORS.procedure THEN
    ORS.Get(sym);  OCB.OpenScope;
    NEW(typ);  typ.form := OCB.ProcAdr;  typ.size := OCB.WordSize;
    typ.hosted := OCB.Nohost;  dmy := 0;
    ProcedureType(typ);
    typ.dsc := OCB.topScope.next;  OCB.CloseScope
  ELSE ORS.Mark("identifier expected");  typ := OCB.noType
  END
END FormalType0;

PROCEDURE CheckRecLevel(lev: INTEGER);
BEGIN
  IF lev # 0 THEN ORS.Mark("ptr base must be global") END
END CheckRecLevel;

PROCEDURE Type0(VAR type: OCB.Type);
VAR obj: OCB.Object;  ptbase: PtrBase;  hosted: BYTE;
BEGIN type := OCB.intType;  (*sync*)
  IF (sym # ORS.ident) & (sym < ORS.array) THEN ORS.Mark("not a type");
    REPEAT ORS.Get(sym) UNTIL (sym = ORS.ident) OR (sym >= ORS.array)
  END;
  IF sym = ORS.ident THEN
    qualident(obj);
    IF obj.class = OCB.Typ THEN
      IF (obj.type # NIL) & (obj.type.form # OCB.NoTyp) THEN type := obj.type END
    ELSE ORS.Mark("not a type or undefined")
    END
  ELSIF sym = ORS.array THEN ORS.Get(sym);  ArrayType(type)
  ELSIF sym = ORS.record THEN
    ORS.Get(sym);  CheckHosted(hosted, {OCB.Nogc});  RecordType(type, hosted);  Check(ORS.end, "missing END")
  ELSIF sym = ORS.pointer THEN
    ORS.Get(sym);  CheckHosted(hosted, {OCB.Nogc});  Check(ORS.to, "missing TO");
    NEW(type); type.form   := OCB.Pointer; type.size   := OCB.WordSize; type.base   := OCB.intType;
    type.hosted := hosted;
    IF sym = ORS.ident THEN
      obj := OCB.thisObj();
      IF obj # NIL THEN
        IF (obj.class = OCB.Typ) & (obj.type.form IN {OCB.Record, OCB.NoTyp}) THEN
          CheckRecLevel(-obj.modno);  type.base := obj.type;
          IF (type.hosted # OCB.Nogc) & (obj.type.hosted = OCB.Nogc) THEN
            ORS.Mark("Garbage collected pointer may not point to uncollected record")
          END;
        ELSIF obj.class = OCB.Mod THEN ORS.Mark("external base type not implemented")
        ELSE ORS.Mark("missing valid base type")
        END
      ELSE CheckRecLevel(Level);  (*enter into list of forward references to be fixed in Declarations*)
        NEW(ptbase);  ptbase.name := ORS.id;  ptbase.type := type;
        ptbase.next := pbsList;  pbsList := ptbase
      END;
      ORS.Get(sym)
    ELSE Type(type.base);
      IF (type.base.form # OCB.Record) OR (type.base.typobj = NIL) THEN ORS.Mark("must point to named record") END;
      IF (type.hosted # OCB.Nogc) & (type.base.hosted = OCB.Nogc) THEN
        ORS.Mark("Garbage collected pointer may not point to uncollected record")
      END;
      CheckRecLevel(Level)
    END
  ELSIF sym = ORS.procedure THEN
    ORS.Get(sym);  OCB.OpenScope;
    NEW(type);  type.form := OCB.ProcAdr;  type.size := OCB.WordSize;
    CheckHosted(type.hosted, {OCB.WinABI, OCB.LinABI});
    ProcedureType(type);  type.dsc := OCB.topScope.next;  OCB.CloseScope
  ELSE ORS.Mark("illegal type")
  END
END Type0;


PROCEDURE Declarations(varclass: INTEGER);
VAR obj, first, seq: OCB.Object;
  x: OCB.Object; tp: OCB.Type; ptbase: PtrBase;
  expo: BOOLEAN; id: ORS.Ident;
  nofvars: INTEGER;
BEGIN (*sync*) pbsList := NIL;
  IF (sym < ORS.const) & (sym # ORS.end) & (sym # ORS.return) THEN ORS.Mark("declaration?");
    REPEAT ORS.Get(sym) UNTIL (sym >= ORS.const) OR (sym = ORS.end) OR (sym = ORS.return)
  END;
  IF sym = ORS.const THEN
    ORS.Get(sym);
    WHILE sym = ORS.ident DO
      id := ORS.id;  ORS.Get(sym);  CheckExport(expo);
      IF sym = ORS.eql THEN ORS.Get(sym) ELSE ORS.Mark("= ?") END;
      x := expression();
      IF OCG.IsSingleCharacterString(x) THEN OCG.StrToChar(x) END;
      OCB.NewObj(obj, id, OCB.Const);  obj.expo := expo;
      IF x.class IN {OCB.Const, OCB.String} THEN
        obj.class := x.class;  obj.type := x.type;
        obj.val   := x.val;    obj.ext  := x.ext;
        obj.modno := x.modno
      ELSE ORS.Mark("expression not constant");  obj.type := OCB.intType
      END;
      Check(ORS.semicolon, "; missing")
    END
  END;
  IF sym = ORS.type THEN
    ORS.Get(sym);
    WHILE sym = ORS.ident DO
      id := ORS.id;  ORS.Get(sym);  CheckExport(expo);
      IF sym = ORS.eql THEN ORS.Get(sym) ELSE ORS.Mark("=?") END;
      Type(tp);
      OCB.NewObj(obj, id, OCB.Typ);  obj.type := tp;  obj.expo := expo;  obj.modno := -Level;
      IF tp.typobj = NIL THEN tp.typobj := obj END;
      IF expo & (obj.type.form = OCB.Record) THEN obj.exno := 1 END;
      IF tp.form = OCB.Record THEN
        ptbase := pbsList;  (*check whether this is base of a pointer type;  search and fixup*)
        WHILE ptbase # NIL DO
          IF obj.name = ptbase.name THEN
            ptbase.type.base := obj.type;
            IF (ptbase.type.base.hosted = OCB.Nogc) & (ptbase.type.hosted # OCB.Nogc) THEN
              ORS.Mark("Garbage collected pointer may not point to uncollected record")
            END
          END;
          ptbase := ptbase.next
        END;
        IF Level = 0 THEN OCG.BuildTD(tp, VarSize) END (* type descriptor; len used as its address *)
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
    IF ptbase.type.base.form = OCB.Int64 THEN
      ORS.Mark("undefined pointer base of "); ws(ptbase.name); wn
    END;
    ptbase := ptbase.next
  END;
  IF (sym >= ORS.const) & (sym <= ORS.var) THEN ORS.Mark("declaration in bad order") END
END Declarations;


PROCEDURE ProcedureDecl;
VAR proc:        OCB.Object;
    seq:         OCB.Object;
    type:        OCB.Type;
    procid:      ORS.Ident;
    y:           OCB.Object;
    jmp:         INTEGER;
    noflocals:   INTEGER;
    i:           INTEGER;
    longestname: INTEGER;
    hosted:      BYTE;
BEGIN ORS.Get(sym);
  CheckHosted(hosted, {OCB.LinABI, OCB.Syscall, OCB.WinABI});

  IF hosted = OCB.Syscall THEN
    IF sym = ORS.int THEN
      i := ORS.ival;  ORS.Get(sym)
    ELSE
      i := 0;  ORS.Mark("Missing syscall number");
    END;
  END;

  IF sym = ORS.ident THEN
    procid := ORS.id;  ORS.Get(sym);
    OCB.NewObj(proc, ORS.id, OCB.Proc);
    NEW(type);  type.form := OCB.ProcAdr;  type.size := OCB.WordSize;
    type.hosted := hosted;
    proc.type := type;  proc.val := -1;  proc.modno := -Level;
    CheckExport(proc.expo);
    IF proc.expo THEN
      IF hosted # OCB.Syscall THEN
        proc.exno := 1
      ELSE
        ORS.Mark("Cannot export Linux syscall procedure.");
        proc.expo := FALSE
      END
    END;
    OCB.OpenScope;  INC(Level);  type.base := OCB.noType;
    ProcedureType(type);  (*formal parameter list*)

    (*IF hosted > 0 THEN proc.modno := hosted END;*)

    IF hosted = OCB.Syscall THEN  (* no body *)
      type.dsc := OCB.topScope.next;
      proc.val := i
    ELSE
      Check(ORS.semicolon, "missing ;");
      Declarations(OCB.ProcVar);

      type.dsc := OCB.topScope.next;

      (* Parse nested procedures *)
      (* proc.val := OXG.Here();  TODO jumps *)
      IF sym = ORS.procedure THEN
        (* jmp := -1;  OXG.FJump(jmp);  TODO jumps *)
        REPEAT ProcedureDecl;  Check(ORS.semicolon, "missing ;") UNTIL sym # ORS.procedure;
        (* OXG.FixLink(jmp);  proc.val := OXG.Here();  TODO jumps *)
        proc.type.dsc := OCB.topScope.next
      END;

      (* Compile procedure body *)
      CheckAccess(proc, Level);
      OCG.GenEnter(proc);
      IF sym = ORS.begin THEN
        ORS.Get(sym);  seq := StatSequence();
        OCG.GenBody(proc, seq)
      END;
      IF sym = ORS.return THEN
        ORS.Get(sym);  y := expression();
        IF type.base = OCB.noType THEN ORS.Mark("this is not a function")
        ELSIF ~CompTypes(type.base, y.type, FALSE) THEN ORS.Mark("wrong result type")
        END;
      ELSIF type.base.form # OCB.NoTyp THEN
        ORS.Mark("function without result");  type.base := OCB.noType
      END;
      Check(ORS.end, "missing END");
      IF sym = ORS.ident THEN
        IF ORS.id # procid THEN ORS.Mark("mismatch") END;
        ORS.Get(sym)
      ELSE ORS.Mark("missing proc id")
      END;
      OCG.GenReturn(y)
    END;
    OCB.CloseScope;  DEC(Level)
  END
END ProcedureDecl;


PROCEDURE Import;
VAR impid, impid1: ORS.Ident;
BEGIN
  IF sym = ORS.ident THEN
    impid := ORS.id; ORS.Get(sym);
    IF sym = ORS.becomes THEN
      ORS.Get(sym);
      IF sym = ORS.ident THEN impid1 := ORS.id; ORS.Get(sym)
      ELSE ORS.Mark("id expected"); impid1 := impid
      END
    ELSE impid1 := impid
    END;
    OCB.Import(impid, impid1)
  ELSE ORS.Mark("id expected")
  END
END Import;


PROCEDURE Module*(VAR newsymbols: BOOLEAN);
VAR
  key:    INTEGER;
  exno:   INTEGER;
  obj:    OCB.Object;
  start:  INTEGER;
BEGIN
  start := H.Ticks();
  ws(" compiling ");
  ORS.Get(sym);
  IF sym # ORS.module THEN
    ORS.Mark("must start with MODULE");
  ELSE
    ORS.Get(sym);
    OCB.Init;  OCB.OpenScope;
    IF sym = ORS.ident THEN modid := ORS.id;  ORS.Get(sym);  ws(modid);  wa
    ELSE ORS.Mark("identifier expected")
    END;
    Check(ORS.semicolon, "missing ;");  Level := 0;  key := 0;

    IF sym = ORS.import THEN
      ORS.Get(sym);  Import;
      WHILE sym = ORS.comma DO ORS.Get(sym); Import END;
      Check(ORS.semicolon, "; missing")
    END;
    OCG.GenOpenModule(modid);
    Declarations(OCB.GlobVar);

    (* Allocate global variable addresses *)
    VarSize := 0;
    obj := OCB.topScope.next;
    WHILE obj # NIL DO
      IF obj.class = OCB.GlobVar THEN
        Align(VarSize, obj.type.size);  obj.val := VarSize;  INC(VarSize, obj.type.size)
      END;
      obj := obj.next
    END;

    OCG.GenSetDataSize((VarSize + 7) DIV 8 * 8);
    WHILE sym = ORS.procedure DO ProcedureDecl; Check(ORS.semicolon, "missing ;") END;

    OCG.GenEnter(NIL);

    IF sym = ORS.begin THEN
      ORS.Get(sym);  obj := StatSequence();
      OCG.GenBody(NIL, obj);
    END;
    Check(ORS.end, "missing END");
    IF sym = ORS.ident THEN
      IF ORS.id # modid THEN ORS.Mark("no match") END;
      ORS.Get(sym)
    ELSE ORS.Mark("Module end identifier missing")
    END;
    IF sym # ORS.period THEN ORS.Mark("period missing") END;

    (* Identify record descriptors that need exporting because *)
    (* they are referenced by exported pointer types.          *)
    obj := OCB.topScope.next;
    WHILE obj # NIL DO
      IF (obj.class = OCB.Typ)
       & (obj.type.form = OCB.Pointer)
       & (obj.type.base.typobj # NIL) THEN
        obj.type.base.typobj.exno := 1
      END;
      obj := obj.next
    END;

    (* Allocate export numbers *)
    exno := 1;
    obj := OCB.topScope.next;
    WHILE obj # NIL DO
      IF obj.exno # 0 THEN obj.exno := exno; INC(exno) END;
      obj := obj.next
    END;
    IF ORS.errcnt = 0 THEN
      OCB.Export(modid, newsymbols, key);
      IF newsymbols THEN ws(" new symbol file") END;
      OCG.GenCloseModule(modid, key, exno);
      (*Texts.WriteInt(W, X64.PC, 6);*)  Texts.WriteInt(W, VarSize, 6);  Texts.WriteHex(W, key);
      wir((H.Ticks() - start) DIV 10000, 4); ws("ms")
    ELSE
      wn;  ws("compilation FAILED");  wa;  H.SetExitCode(1)
    END;
    wn;
    OCB.CloseScope;  pbsList := NIL
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
          ws(S.s); NEW(T); Texts.Open(T, S.s);
          IF T.len > 0 THEN ORS.Init(T, 0); Module(newSF) END
        END
      END
    END
  ELSE
    WHILE S.class = Texts.Name DO
      NEW(T); Texts.Open(T, S.s);
      IF T.len > 0 THEN Option(S); ORS.Init(T, 0); Module(newSF)
      ELSE ws(S.s); wsn(" not found");
      END;
      IF (T.len # 0) & (ORS.errcnt = 0) THEN Texts.Scan(S) ELSE S.class := 0 END
    END
  END;
  Modules.Collect(0)  (* Trigger immediate garbage collection when OCP exits *)
END Compile;


BEGIN
  Texts.OpenWriter(W); wsn("Oberon compiler. NW 2020-08-03, DCWB 2025-02-12.");
  NEW(dummy);  dummy.class := OCB.GlobVar;  dummy.type := OCB.intType;
  expression := expression0;  Type := Type0;  FormalType := FormalType0;
END OCP.