MODULE CompilerIntermediate;  (*D.Brown 2026-11-03*)

IMPORT SYSTEM, Host,
  CS := CompilerScanner,
  CB := CompilerBase;

CONST
  MaxString = 2400;
  MaxInst   = 1000;
  WordSize* = 8;
  Op        = 10;  (*result of prior operation*)
  Type      = 11;  (*used by varparam*)

  (*instruction opcodes*)
  opstore  = 1;
  opadd    = 2;
  opsub    = 3;
  opdiv    = 4;
  opmod    = 5;
  opret    = 7;
  opcfjump = 8;
  opfjump  = 9;
  opbjump  = 10;
  opcbjump = 11;
  opprep   = 12;
  opvalpar = 13;
  opvarpar = 14;
  opoarpar = 15;
  opstrpar = 16;
  opcall   = 17;


TYPE
  Item* = RECORD
    mode*:   BYTE;
    rdo*:    BOOLEAN;  (*read only*)
    global:  BOOLEAN;  (*module global variable*)
    type*:   CB.Type;
    val:     INTEGER;  (*from object.val*)
    len:     INTEGER;  (*string length, when mode const and type.form string*)
  END;

  Instruction = RECORD
    op:   INTEGER;
    x, y: POINTER TO Item
  END;

VAR
  strings:  ARRAY MaxString OF CHAR;
  sc:       INTEGER;  (*string counter*)

  instructions: ARRAY MaxInst OF Instruction;
  ic*:          INTEGER;  (*instruction counter*)

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

PROCEDURE wsl*(s: ARRAY OF CHAR; w: INTEGER; c: CHAR);  (*Left justified with trailing c*)
BEGIN ws(s);  DEC(w, Host.Length(s));  WHILE w > 0 DO wc(c); DEC(w) END END wsl;

PROCEDURE wsr*(s: ARRAY OF CHAR; w: INTEGER; c: CHAR);  (*Right justified with leading c*)
BEGIN DEC(w, Host.Length(s));  WHILE w > 0 DO wc(c); DEC(w) END;  ws(s) END wsr;

PROCEDURE whr*(n, w: INTEGER; c: CHAR);  (*Right justified with leading c*)
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

PROCEDURE wsym*(s: INTEGER);
BEGIN
  IF    s = 0             THEN ws("<null>")
  ELSIF s = CS.times      THEN ws("times")
  ELSIF s = CS.rdiv       THEN ws("rdiv")
  ELSIF s = CS.div        THEN ws("div")
  ELSIF s = CS.mod        THEN ws("mod")
  ELSIF s = CS.and        THEN ws("and")
  ELSIF s = CS.plus       THEN ws("plus")
  ELSIF s = CS.minus      THEN ws("minus")
  ELSIF s = CS.or         THEN ws("or")
  ELSIF s = CS.eql        THEN ws("eql")
  ELSIF s = CS.neq        THEN ws("neq")
  ELSIF s = CS.lss        THEN ws("lss")
  ELSIF s = CS.leq        THEN ws("leq")
  ELSIF s = CS.gtr        THEN ws("gtr")
  ELSIF s = CS.geq        THEN ws("geq")
  ELSIF s = CS.in         THEN ws("in")
  ELSIF s = CS.is         THEN ws("is")
  ELSIF s = CS.arrow      THEN ws("arrow")
  ELSIF s = CS.period     THEN ws("period")
  ELSIF s = CS.bslash     THEN ws("bslash")
  ELSIF s = CS.char       THEN ws("char")
  ELSIF s = CS.int        THEN ws("int")
  ELSIF s = CS.real       THEN ws("real")
  ELSIF s = CS.false      THEN ws("false")
  ELSIF s = CS.true       THEN ws("true")
  ELSIF s = CS.nil        THEN ws("nil")
  ELSIF s = CS.not        THEN ws("not")
  ELSIF s = CS.lparen     THEN ws("lparen")
  ELSIF s = CS.lbrak      THEN ws("lbrak")
  ELSIF s = CS.lbrace     THEN ws("lbrace")
  ELSIF s = CS.if         THEN ws("if")
  ELSIF s = CS.while      THEN ws("while")
  ELSIF s = CS.repeat     THEN ws("repeat")
  ELSIF s = CS.case       THEN ws("case")
  ELSIF s = CS.for        THEN ws("for")
  ELSIF s = CS.comma      THEN ws("comma")
  ELSIF s = CS.colon      THEN ws("colon")
  ELSIF s = CS.becomes    THEN ws("becomes")
  ELSIF s = CS.upto       THEN ws("upto")
  ELSIF s = CS.rparen     THEN ws("rparen")
  ELSIF s = CS.rbrak      THEN ws("rbrak")
  ELSIF s = CS.rbrace     THEN ws("rbrace")
  ELSIF s = CS.then       THEN ws("then")
  ELSIF s = CS.of         THEN ws("of")
  ELSIF s = CS.do         THEN ws("do")
  ELSIF s = CS.to         THEN ws("to")
  ELSIF s = CS.by         THEN ws("by")
  ELSIF s = CS.semicolon  THEN ws("semicolon")
  ELSIF s = CS.end        THEN ws("end")
  ELSIF s = CS.bar        THEN ws("bar")
  ELSIF s = CS.else       THEN ws("else")
  ELSIF s = CS.elsif      THEN ws("elsif")
  ELSIF s = CS.until      THEN ws("until")
  ELSIF s = CS.return     THEN ws("return")
  ELSIF s = CS.array      THEN ws("array")
  ELSIF s = CS.record     THEN ws("record")
  ELSIF s = CS.pointer    THEN ws("pointer")
  ELSIF s = CS.const      THEN ws("const")
  ELSIF s = CS.type       THEN ws("type")
  ELSIF s = CS.var        THEN ws("var")
  ELSIF s = CS.procedure  THEN ws("procedure")
  ELSIF s = CS.begin      THEN ws("begin")
  ELSIF s = CS.import     THEN ws("import")
  ELSIF s = CS.module     THEN ws("module")
  ELSIF s = CS.string     THEN
    ws("'"); ws(CS.str); ws("'");
  ELSIF s = CS.ident      THEN
    ws("<"); ws(CS.id); ws(">");
  ELSE
    wc("#"); wi(s); wc("#")
  END
END wsym;


PROCEDURE wmode(mode: INTEGER);
BEGIN
  IF    mode = CB.Const THEN ws("Const")
  ELSIF mode = CB.Var   THEN ws("Var")
  ELSIF mode = CB.Par   THEN ws("Par")
  ELSIF mode = CB.Fld   THEN ws("Fld")
  ELSIF mode = CB.Typ   THEN ws("Typ")
  ELSIF mode = CB.SProc THEN ws("SProc")
  ELSIF mode = CB.SFunc THEN ws("SFunc")
  ELSIF mode = CB.Mod   THEN ws("Mod")
  ELSE ws("mode<");  wi(mode);  ws(">")
  END
END wmode;

PROCEDURE wform*(form: INTEGER);
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
END wform;

PROCEDURE wtype(type: CB.Type);
BEGIN
  ASSERT(type # NIL);
  IF (type.typobj # NIL) & (type.typobj.name # "") THEN
    ws(type.typobj.name)
  ELSE
    wform(type.form)
  END;
END wtype;

PROCEDURE witem(x: Item);
VAR i: INTEGER;
BEGIN
  IF x.mode = CB.Const THEN
    IF x.type.form <= CB.Set THEN
      wi(x.val)
    ELSIF x.type.form = CB.String THEN
      wc(22X);  FOR i := 0 TO x.len-1 DO wc(strings[x.val+i]) END;  wc(22X)
    (*
    ELSIF x.type.form = CB.Proc THEN
      ws("proc ");  ws(x.type.typobj.name)
    *)
    ELSE
      ws("const "); wform(x.type.form); ws("?")
    END
  ELSIF x.mode = CB.Var THEN
    wform(x.type.form);  wc("[");
    IF x.global THEN ws("g") ELSE ws("s") END;
    wi(x.val);  wc("]")
  ELSIF x.mode = CB.Par THEN
    ASSERT(~x.global);
    wform(x.type.form);  ws("[[s");  wi(x.val);  ws("]]")
  ELSIF x.mode = Op THEN
    wc("<");  wi(x.val);  wc(">")
  ELSE
    ws("(");         wmode(x.mode);
    IF x.type # NIL THEN ws(": ");  wtype(x.type) END;
    ws(", val ");    wi(x.val);
    ws(", len ");    wi(x.len);
    IF x.global THEN ws(" global") END;
    IF x.rdo    THEN ws(" readonly") END;
    ws(")")
  END
END witem;

PROCEDURE wopx;
BEGIN wc(" ");  wil(ic, 10)
END wopx;

PROCEDURE wop(op: INTEGER);
BEGIN
  IF    op = opstore  THEN ws("store  ")
  ELSIF op = opadd    THEN ws("add    ")
  ELSIF op = opsub    THEN ws("sub    ")
  ELSIF op = opdiv    THEN ws("div    ")
  ELSIF op = opmod    THEN ws("mod    ")
  ELSIF op = opret    THEN ws("ret    ")
  ELSIF op = opcfjump THEN ws("cfjump ")
  ELSIF op = opfjump  THEN ws("fjump  ")
  ELSIF op = opbjump  THEN ws("bjump  ")
  ELSIF op = opcbjump THEN ws("cbjump ")
  ELSIF op = opprep   THEN ws("prep   ")
  ELSIF op = opvalpar THEN ws("valpar ")
  ELSIF op = opvarpar THEN ws("varpar ")
  ELSIF op = opoarpar THEN ws("oarpar ")
  ELSIF op = opstrpar THEN ws("strpar ")
  ELSIF op = opcall   THEN ws("call   ")
  ELSE wc("?"); wi(op); wsn("?");  ASSERT(FALSE)
  END
END wop;

PROCEDURE winst(inst: Instruction);
BEGIN
  wop(inst.op);
  IF inst.x # NIL THEN witem(inst.x^) END;
  IF inst.y # NIL THEN ws(", ");  witem(inst.y^) END
END winst;

PROCEDURE writeinstructions();
VAR i: INTEGER;
BEGIN
  wsn("Post fixup:");
  FOR i := 0 TO ic-1 DO wc(" ");  wil(i, 10);  winst(instructions[i]);  wn END
END writeinstructions;


(* -------------------------------------------------------------------------- *)

PROCEDURE ConstVal*(x: Item): INTEGER;
VAR val: INTEGER;
BEGIN ASSERT(x.mode = CB.Const);
  IF x.type.form = CB.String THEN
    ASSERT(x.len = 2);  (*single character string*)
    val := ORD(strings[x.val])
  ELSE
    ASSERT(x.type.form <= CB.Pointer);
    val := x.val
  END
RETURN val END ConstVal;

PROCEDURE IsOneCharString*(x: Item): BOOLEAN;
RETURN (x.mode = CB.Const)  &  (x.type.form = CB.String)  &  (x.len = 2)
END IsOneCharString;

PROCEDURE MakeObjectFromConstItem*(VAR x: Item;  id: CS.Ident;  expo: BOOLEAN;  curlev: INTEGER): CB.Object;
VAR obj: CB.Object;
BEGIN
  CB.NewObj(obj, id, CB.Const);
  obj.type := x.type;  obj.expo := expo;  obj.val  := x.val;
  IF x.type.form = CB.String THEN
    obj.lev := x.len
  ELSIF x.global THEN
    obj.lev := 0
  ELSE
    obj.lev := curlev
  END;
  IF x.mode # CB.Const THEN
    CS.Mark("expression not constant");
    obj.type := CB.intType
  END
RETURN obj END MakeObjectFromConstItem;

PROCEDURE MakeItem*(VAR x: Item;  y: CB.Object;  curlev: INTEGER);
BEGIN
  x.mode := y.class;  x.type := y.type;  x.val    := y.val;
  x.rdo  := y.rdo;    x.len  := 0;       x.global := FALSE;
  IF y.class = CB.Const THEN
    IF y.type.form = CB.String THEN x.len := y.lev END
  ELSIF y.lev = 0 THEN
    x.global := TRUE
  ELSIF y.lev # curlev
    THEN CS.Mark("not accessible ")
  END
END MakeItem;

PROCEDURE MakeConstItem*(VAR x: Item;  typ: CB.Type;  val: INTEGER);
BEGIN
  x.mode := CB.Const;  x.type := typ;  x.val    := val;
  x.rdo  := FALSE;     x.len  := 0;    x.global := FALSE;
END MakeConstItem;

PROCEDURE MakeRealItem*(VAR x: Item;  val: REAL);
BEGIN MakeConstItem(x, CB.realType, SYSTEM.VAL(INTEGER, val))
END MakeRealItem;

PROCEDURE MakeStringItem*(VAR x: Item;  len: INTEGER);
(*copy string from CS.str to strings*)
VAR i: INTEGER;
BEGIN
  x.mode := CB.Const;  x.type := CB.strType;  x.val    := sc;
  x.len  := len;       x.rdo  := FALSE;       x.global := FALSE;
  IF sc + len > MaxString THEN
    CS.Mark("too many strings")
  ELSE
    FOR i := 0 TO len-1 DO strings[sc] := CS.str[i];  INC(sc) END
  END
  (*TODO reuse existing strings*)
END MakeStringItem;

PROCEDURE MakeTypeItem(VAR x: Item;  t: CB.Type);
BEGIN
  x.mode := Type;  x.type := t;      x.val    := 0;
  x.len  := 0;     x.rdo  := FALSE;  x.global := FALSE;
END MakeTypeItem;

PROCEDURE Niladic(op: INTEGER);
BEGIN
  IF ic < MaxInst THEN
    instructions[ic].op := op;
    wc(" ");  wil(ic, 10);  winst(instructions[ic]);  wn;
    INC(ic)
  ELSE
    CS.Mark("too many instructions")
  END
END Niladic;

PROCEDURE Monadic(op: INTEGER;  x: Item);
BEGIN
  IF ic < MaxInst THEN
    instructions[ic].op := op;
    NEW(instructions[ic].x);  instructions[ic].x^ := x;
    wc(" ");  wil(ic, 10);  winst(instructions[ic]);  wn;
    INC(ic)
  ELSE
    CS.Mark("too many instructions")
  END
END Monadic;

PROCEDURE Dyadic(op: INTEGER;  x, y: Item);
BEGIN
  IF ic < MaxInst THEN
    instructions[ic].op := op;
    NEW(instructions[ic].x);  instructions[ic].x^ := x;
    NEW(instructions[ic].y);  instructions[ic].y^ := y;
    wc(" ");  wil(ic, 10);  winst(instructions[ic]);  wn;
    INC(ic)
  ELSE
    CS.Mark("too many instructions")
  END
END Dyadic;

PROCEDURE BuildTD* (T: CB.Type;  VAR dc: INTEGER);                         END BuildTD;
PROCEDURE TypeTest*(VAR x: Item;  T: CB.Type;  varpar, isguard: BOOLEAN);  END TypeTest;

PROCEDURE Index*(VAR x, y: Item);             END Index;  (*x := x[y]*)
PROCEDURE DeRef*(VAR x: Item);                END DeRef;  (*x := x^  *)
PROCEDURE Field*(VAR x: Item;  y: CB.Object); END Field;  (*x := x.y *)

PROCEDURE Not*      (VAR x: Item);  END Not;
PROCEDURE Neg*      (VAR x: Item);  END Neg;
PROCEDURE Abs*      (VAR x: Item);  END Abs;
PROCEDURE Odd*      (VAR x: Item);  END Odd;
PROCEDURE Floor*    (VAR x: Item);  END Floor;
PROCEDURE Float*    (VAR x: Item);  END Float;
PROCEDURE Ord*      (VAR x: Item);  END Ord;
PROCEDURE Len*      (VAR x: Item);  END Len;
PROCEDURE Adr*      (VAR x: Item);  END Adr;
PROCEDURE Register* (VAR x: Item);  END Register;
PROCEDURE Condition*(VAR x: Item);  END Condition;
PROCEDURE H*        (VAR x: Item);  END H;
PROCEDURE Singleton*(VAR x: Item);  END Singleton;
PROCEDURE Assert*   (VAR x: Item);  END Assert;
PROCEDURE New*      (VAR x: Item);  END New;
PROCEDURE Led*      (VAR x: Item);  END Led;
PROCEDURE LDPSR*    (VAR x: Item);  END LDPSR;

PROCEDURE CFJump*(VAR x: Item);
BEGIN Monadic(opcfjump, x) END CFJump;

PROCEDURE FJump*(VAR L: INTEGER);
BEGIN Niladic(opfjump);  L := ic-1 END FJump;

PROCEDURE BJump*(VAR L: INTEGER);
BEGIN Niladic(opbjump);  L := ic-1 END BJump;

PROCEDURE CBJump*(VAR x: Item;  L: INTEGER);
BEGIN Monadic(opcbjump, x);  L := ic-1 END CBJump;

PROCEDURE Fixup*(VAR x: Item);
BEGIN
  ws(" fixup  ");  witem(x);  wn;
END Fixup;

PROCEDURE FixLink*(L: INTEGER);
BEGIN
  ws(" fixlnk <");  wi(L);  wsn(">");
END FixLink;

PROCEDURE FixOne*(at: INTEGER);  (*set jump target at <at> to current ic*)
BEGIN
  ws(" fixone <");  wi(at);  ws(">: fixing '"); winst(instructions[at]);  wsn("'");
  ASSERT(instructions[3].op IN {opcfjump, opfjump, opbjump, opcbjump});
  ASSERT(instructions[3].x = NIL);
  NEW(instructions[3].x);
  MakeConstItem(instructions[3].x^, CB.intType, ic)
END FixOne;


PROCEDURE For0*(VAR x, y: Item);                         END For0;
PROCEDURE For1*(VAR x, y, z, w: Item;  VAR L: INTEGER);  END For1;
PROCEDURE For2*(VAR x, y, w: Item);                      END For2;

PROCEDURE ADC*  (VAR x, y: Item);  END ADC;
PROCEDURE SBC*  (VAR x, y: Item);  END SBC;
PROCEDURE UML*  (VAR x, y: Item);  END UML;
PROCEDURE Bit*  (VAR x, y: Item);  END Bit;
PROCEDURE Set*  (VAR x, y: Item);  END Set;
PROCEDURE In*   (VAR x, y: Item);  END In;
PROCEDURE MulOp*(VAR x, y: Item);  END MulOp;
PROCEDURE Pack* (VAR x, y: Item);  END Pack;
PROCEDURE Unpk* (VAR x, y: Item);  END Unpk;
PROCEDURE Get*  (VAR x, y: Item);  END Get;
PROCEDURE Put*  (VAR x, y: Item);  END Put;
PROCEDURE LDREG*(VAR x, y: Item);  END LDREG;

PROCEDURE Store*(VAR x, y: Item);
BEGIN Dyadic(opstore, x, y) END Store;

PROCEDURE StoreStruct*(VAR x, y: Item);
END StoreStruct;

PROCEDURE CopyString* (VAR x, y: Item);
END CopyString;

PROCEDURE Copy*(VAR x, y, z: Item);  END Copy;

PROCEDURE AddOp*(op: INTEGER;  VAR x, y: Item);  (*x := x +- y*)
BEGIN
  ASSERT(op IN {CS.plus, CS.minus});
  IF (x.mode = CB.Const) & (y.mode = CB.Const) THEN
    IF op = CS.plus THEN INC(x.val, y.val) ELSE DEC(x.val, y.val) END
  ELSE
    IF op = CS.plus THEN Dyadic(opadd, x, y) ELSE Dyadic(opsub, x, y) END;
    x.mode := Op;  x.val := ic-1
  END
END AddOp;

PROCEDURE DivOp*(op: INTEGER;  VAR x, y: Item);  (*x := x op y*)
BEGIN
  ASSERT(op IN {CS.div, CS.mod});
  IF (y.mode = CB.Const) & (y.val <= 0) THEN
    IF op = CS.div THEN CS.Mark("bad divisor") ELSE CS.Mark("bad modulus") END
  ELSE
    IF (x.mode = CB.Const) & (y.mode = CB.Const) THEN
      IF op = CS.div THEN x.val := x.val DIV y.val ELSE x.val := x.val MOD y.val END
    ELSE
      IF op = CS.div THEN Dyadic(opdiv, x, y) ELSE Dyadic(opmod, x, y) END;
      x.mode := Op;  x.val := ic-1
    END
  END
END DivOp;


PROCEDURE RealOp*   (op: INTEGER;        VAR x, y: Item);  END RealOp;  (*x := x op y*)
PROCEDURE SetOp*    (op: INTEGER;        VAR x, y: Item);  END SetOp;   (*x := x op y*)
PROCEDURE Shift*    (fct: INTEGER;       VAR x, y: Item);  END Shift;
PROCEDURE Increment*(upordown: INTEGER;  VAR x, y: Item);  END Increment;
PROCEDURE Include*  (inorex: INTEGER;    VAR x, y: Item);  END Include;  (*x := x | or &~ 1<<y*)

PROCEDURE And1*(VAR x: Item);     END And1;  (*x := x &*)
PROCEDURE And2*(VAR x, y: Item);  END And2;
PROCEDURE Or1* (VAR x: Item);     END Or1;   (*x := x OR*)
PROCEDURE Or2* (VAR x, y: Item);  END Or2;


PROCEDURE IntRelation*   (op: INTEGER;  VAR x, y: Item);  END IntRelation;    (*x := x < y*)
PROCEDURE RealRelation*  (op: INTEGER;  VAR x, y: Item);  END RealRelation;   (*x := x < y*)
PROCEDURE StringRelation*(op: INTEGER;  VAR x, y: Item);  END StringRelation; (*x := x < y*)

PROCEDURE CheckRegs*;  END CheckRegs;

(* ---------------------------- Procedure calls ----------------------------- *)

PROCEDURE PrepCall*(VAR x: Item;  VAR r: INTEGER);
BEGIN Monadic(opprep, x) END PrepCall;

PROCEDURE ValueParam*(VAR x: Item);
BEGIN Monadic(opvalpar, x)END ValueParam;

PROCEDURE VarParam*(VAR x: Item;  ftype: CB.Type);
VAR ty: Item;
BEGIN
  ty.mode := Type;  ty.type := ftype;
  Dyadic(opvarpar, x, ty)
END VarParam;

PROCEDURE OpenArrayParam*(VAR x: Item);
BEGIN Monadic(opoarpar, x) END OpenArrayParam;

PROCEDURE StringParam*(VAR x: Item);
BEGIN Monadic(opstrpar, x) END StringParam;

PROCEDURE Call*(VAR x: Item;  r: INTEGER);
BEGIN Monadic(opcall, x) END Call;


(* ------------------------- Procedure declaration -------------------------- *)

PROCEDURE Enter* (procid: CS.Ident;  parblksize, locblksize: INTEGER);
BEGIN wn;
  ws(" Begin ");      ws(procid);
  ws(", ic ");        wi(ic);
  ws(", par size ");  wi(parblksize);
  ws(", loc size ");  wi(locblksize);  wsn(".");
END Enter;

PROCEDURE Return*(procid: CS.Ident;  form: INTEGER;  VAR x: Item;  size: INTEGER);
BEGIN
  Monadic(opret, x);
  ws(" End ");        ws(procid);
  ws(", ic ");        wi(ic);
  ws(", loc size ");  wi(size);  wsn(".");
END Return;


(* --------------------------- Module declaration --------------------------- *)

PROCEDURE Open*;
VAR i: INTEGER;
BEGIN (*initialise module compilation*)
  sc := 0;  ic := 0;
  Host.ZeroFill(instructions)
END Open;

PROCEDURE Header*(dc: INTEGER);
BEGIN wn;
  ws(" Begin module, dc ");  wi(dc);  wn;
END Header;

PROCEDURE Close*(VAR modid: CS.Ident;  dc, key, nofent: INTEGER);
BEGIN
  writeinstructions;
  wopx;  wsn("end");
  ws(" Success, ic ");  wi(ic);
  ws(", dc ");          wi(dc);
  ws(", key ");         wh(key);  wsn(".")
END Close;

BEGIN
END CompilerIntermediate.
