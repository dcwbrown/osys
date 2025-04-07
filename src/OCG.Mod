MODULE OCG;  (* DCWB 2024-08-10 Oberon common code generator *)

IMPORT SYSTEM, H := Host, Kernel, Files, Modules, Texts, ORS, OCB;

CONST
  TextSize   = 20000H;   (* Code, string and type descriptor buffer size (128KB) *)

  (* Named registers *)
  RAX = 0;  RCX = 1;  RDX = 2;  RBX = 3;
  RSP = 4;  RBP = 5;  RSI = 6;  RDI = 7;

VAR
  W: Texts.Writer;

  Text:  ARRAY TextSize OF BYTE;
  PC:    INTEGER;

  LoadReg:  PROCEDURE(x: OCB.Object; reg: INTEGER; keep: SET);
  LoadCond: PROCEDURE(x: OCB.Object; keep: SET;  VAR t, f, cond: INTEGER);



(* -------------------------------- Tracing --------------------------------- *)

PROCEDURE wa;                    BEGIN Texts.Append(Texts.Log, W.buf) END wa;
PROCEDURE wn*;                   BEGIN Texts.WriteLn(W);  wa          END wn;
PROCEDURE wc (c: CHAR);          BEGIN Texts.Write(W, c)              END wc;
PROCEDURE ws (s: ARRAY OF CHAR); BEGIN Texts.WriteString(W, s)        END ws;
PROCEDURE wsn(s: ARRAY OF CHAR); BEGIN Texts.WriteString(W, s); wn    END wsn;
PROCEDURE wi (i: INTEGER);       BEGIN Texts.WriteInt(W, i, 1)        END wi;
PROCEDURE wir(i, n: INTEGER);    BEGIN Texts.WriteInt(W, i, n)        END wir;

PROCEDURE wsl*(s: ARRAY OF CHAR; w: INTEGER; c: CHAR);  (* Left justified with trailing c *)
BEGIN ws(s);  DEC(w, H.Length(s));  WHILE w > 0 DO wc(c); DEC(w) END END wsl;

PROCEDURE wsr*(s: ARRAY OF CHAR; w: INTEGER; c: CHAR);  (* Right justified with leading c *)
BEGIN DEC(w, H.Length(s));  WHILE w > 0 DO wc(c); DEC(w) END;  ws(s) END wsr;

PROCEDURE wh*(n: INTEGER);
VAR hex: ARRAY 32 OF CHAR;
BEGIN H.IntToHex(n, hex); ws(hex) END wh;

PROCEDURE whr*(n, w: INTEGER; c: CHAR);  (* Right justified with leading c *)
VAR hex: ARRAY 32 OF CHAR;
BEGIN H.IntToHex(n, hex); wsr(hex, w, c) END whr;

PROCEDURE wil(n, w: INTEGER);
VAR num: ARRAY 32 OF CHAR;
BEGIN H.IntToDecimal(n, num); wsl(num, w, " ") END wil;

PROCEDURE wtab(n: INTEGER);
VAR l: INTEGER;
BEGIN
  l := W.buf.len;  WHILE l < n DO wc(" ");  INC(l) END;
  Texts.Append(Texts.Log, W.buf)
END wtab;

PROCEDURE wSpace(n: INTEGER);
BEGIN WHILE n > 0 DO wc(" ");  DEC(n) END
END wSpace;


PROCEDURE wForm(form, w: INTEGER);
BEGIN
  IF    form = OCB.Byte    THEN wsl("Byte",    w, " ")
  ELSIF form = OCB.Bool    THEN wsl("Bool",    w, " ")
  ELSIF form = OCB.Char    THEN wsl("Char",    w, " ")
  ELSIF form = OCB.Int8    THEN wsl("Int8",    w, " ")
  ELSIF form = OCB.Int16   THEN wsl("Int16",   w, " ")
  ELSIF form = OCB.Int32   THEN wsl("Int32",   w, " ")
  ELSIF form = OCB.Int64   THEN wsl("Int64",   w, " ")
  ELSIF form = OCB.Card16  THEN wsl("Card16",  w, " ")
  ELSIF form = OCB.Card32  THEN wsl("Card32",  w, " ")
  ELSIF form = OCB.Real    THEN wsl("Real",    w, " ")
  ELSIF form = OCB.Set     THEN wsl("Set",     w, " ")
  ELSIF form = OCB.Pointer THEN wsl("Pointer", w, " ")
  ELSIF form = OCB.NilTyp  THEN wsl("NilTyp",  w, " ")
  ELSIF form = OCB.NoTyp   THEN wsl("NoTyp",   w, " ")
  ELSIF form = OCB.ProcAdr THEN wsl("ProcAdr", w, " ")
  ELSIF form = OCB.StrAdr  THEN wsl("StrAdr",  w, " ")
  ELSIF form = OCB.Array   THEN wsl("Array",   w, " ")
  ELSIF form = OCB.Record  THEN wsl("Record",  w, " ")
  ELSE ws("<"); wi(form); ws(">")
  END
END wForm;

PROCEDURE wReg(r: INTEGER);
BEGIN
  IF    r < 0 THEN ws("r?")
  ELSIF r = 0 THEN ws("rax")
  ELSIF r = 1 THEN ws("rcx")
  ELSIF r = 2 THEN ws("rdx")
  ELSIF r = 3 THEN ws("rbx")
  ELSIF r = 4 THEN ws("rsp")
  ELSIF r = 5 THEN ws("rbp")
  ELSIF r = 6 THEN ws("rsi")
  ELSIF r = 7 THEN ws("rdi")
  ELSE ws("r");  wi(r)
  END
END wReg;

PROCEDURE wAdr(x: OCB.Object);
BEGIN
  ASSERT(x.class IN {OCB.GlobVar, OCB.Proc, OCB.String});
  IF  x.modno < 0 THEN wi(x.modno); wc("."); wi(x.val)
  ELSIF x.val < 0 THEN wi(x.val) ELSE wh(x.val) END
END wAdr;

PROCEDURE wConst(x: OCB.Object);
VAR i, l: INTEGER;
BEGIN
  ASSERT(x.class IN {OCB.Const, OCB.Proc, OCB.String});
  IF    x.class = OCB.Proc   THEN wAdr(x)
  ELSIF x.class = OCB.String THEN wAdr(x);
                                  ws(" '"); i := x.val;  l := i + x.ext - 1;
                                  WHILE i < l DO wc(CHR(Text[i]));  INC(i) END;
                                  wc("'")
  ELSIF x.class = OCB.Const  THEN IF (x.val >= 0) & (x.val <= 9)
                                  THEN wi(x.val)
                                  ELSE wh(x.val); ws("H ") END  END
END wConst;


PROCEDURE wRegSet(reg: SET);
VAR i, s, e: INTEGER; sep: BOOLEAN;
  PROCEDURE wRange(VAR sep: BOOLEAN; s, e: INTEGER);
  BEGIN
    IF s >= 0 THEN
      IF sep THEN wc(",") END;  sep := TRUE;
      wReg(s);
      IF e > s THEN
        IF e = s+1 THEN ws(",") ELSE ws("..") END;
        wReg(e)
      END
    END
  END wRange;
BEGIN ws("{");  sep:= FALSE;  s := -1;
  FOR i := 0 TO 15 DO IF i IN reg THEN
    IF s < 0 THEN s := i;  e := i
    ELSIF i = e + 1 THEN e := i
    ELSE wRange(sep, s, e);  s := i;  e := i
    END
  END END;
  wRange(sep, s, e);
  ws("}")
END wRegSet;

PROCEDURE wClass(c: INTEGER);
BEGIN
  IF    c = OCB.Head       THEN ws("Head ")
  ELSIF c = OCB.Const      THEN ws("Const ")
  ELSIF c = OCB.String     THEN ws("String ")
  ELSIF c = OCB.Typ        THEN ws("Typ ")
  ELSIF c = OCB.Proc       THEN ws("Proc ")
  ELSIF c = OCB.GlobVar    THEN ws("glob")
  ELSIF c = OCB.ProcVar    THEN ws("local")
  ELSIF c = OCB.ValPar     THEN ws("ValPar ")
  ELSIF c = OCB.RefPar     THEN ws("RefPar ")
  ELSIF c = OCB.Field      THEN ws("Field ")
  ELSIF c = OCB.StdProc    THEN ws("StdProc ")
  ELSIF c = OCB.StdFunc    THEN ws("StdFunc ")
  ELSIF c = OCB.Mod        THEN ws("Module ")
  ELSIF c = OCB.Expression THEN ws("Expr ")
  ELSE                     ws("<"); wi(c); ws(">")
  END;
END wClass;

PROCEDURE wType(tp: OCB.Type);
BEGIN
  IF (tp.typobj # NIL) & (tp.typobj.name # "") THEN
    ws(tp.typobj.name);  ws(": ");
  END;
  wForm(tp.form, 1)
END wType;

PROCEDURE wSym*(sym: INTEGER);
BEGIN
  IF    sym = ORS.times     THEN ws("*")
  ELSIF sym = ORS.rdiv      THEN ws("/")
  ELSIF sym = ORS.div       THEN ws("DIV")
  ELSIF sym = ORS.mod       THEN ws("MOD")
  ELSIF sym = ORS.and       THEN ws("&")
  ELSIF sym = ORS.plus      THEN ws("+")
  ELSIF sym = ORS.minus     THEN ws("-")
  ELSIF sym = ORS.or        THEN ws("OR")
  ELSIF sym = ORS.eql       THEN ws("=")
  ELSIF sym = ORS.neq       THEN ws("#")
  ELSIF sym = ORS.lss       THEN ws("<")
  ELSIF sym = ORS.leq       THEN ws("<=")
  ELSIF sym = ORS.gtr       THEN ws(">")
  ELSIF sym = ORS.geq       THEN ws(">=")
  ELSIF sym = ORS.in        THEN ws("IN")
  ELSIF sym = ORS.is        THEN ws("IS")
  ELSIF sym = ORS.arrow     THEN ws("^")
  ELSIF sym = ORS.period    THEN ws(".")
  ELSIF sym = ORS.char      THEN ws("CHAR")
  ELSIF sym = ORS.int       THEN ws("int")
  ELSIF sym = ORS.real      THEN ws("real")
  ELSIF sym = ORS.false     THEN ws("FALSE")
  ELSIF sym = ORS.true      THEN ws("TRUE")
  ELSIF sym = ORS.nil       THEN ws("NIL")
  ELSIF sym = ORS.string    THEN wc(22X)
  ELSIF sym = ORS.not       THEN ws("~")
  ELSIF sym = ORS.lparen    THEN ws("(")
  ELSIF sym = ORS.lbrak     THEN ws("[")
  ELSIF sym = ORS.lbrace    THEN ws("{")
  ELSIF sym = ORS.ident     THEN ws("ident")
  ELSIF sym = ORS.if        THEN ws("IF")
  ELSIF sym = ORS.while     THEN ws("WHILE")
  ELSIF sym = ORS.repeat    THEN ws("REPEAT")
  ELSIF sym = ORS.case      THEN ws("CASE")
  ELSIF sym = ORS.for       THEN ws("FOR")
  ELSIF sym = ORS.comma     THEN ws(",")
  ELSIF sym = ORS.colon     THEN ws(":")
  ELSIF sym = ORS.becomes   THEN ws(":=")
  ELSIF sym = ORS.upto      THEN ws("..")
  ELSIF sym = ORS.rparen    THEN ws(")")
  ELSIF sym = ORS.rbrak     THEN ws("]")
  ELSIF sym = ORS.rbrace    THEN ws("}")
  ELSIF sym = ORS.then      THEN ws("THEN")
  ELSIF sym = ORS.of        THEN ws("OF")
  ELSIF sym = ORS.do        THEN ws("DO")
  ELSIF sym = ORS.to        THEN ws("TO")
  ELSIF sym = ORS.by        THEN ws("BY")
  ELSIF sym = ORS.semicolon THEN ws(";")
  ELSIF sym = ORS.end       THEN ws("END")
  ELSIF sym = ORS.bar       THEN ws("|")
  ELSIF sym = ORS.else      THEN ws("ELSE")
  ELSIF sym = ORS.elsif     THEN ws("ELSIF")
  ELSIF sym = ORS.until     THEN ws("UNTIL")
  ELSIF sym = ORS.return    THEN ws("RETURN")
  ELSIF sym = ORS.array     THEN ws("ARRAY")
  ELSIF sym = ORS.record    THEN ws("RECORD")
  ELSIF sym = ORS.pointer   THEN ws("POINTER")
  ELSIF sym = ORS.const     THEN ws("CONST")
  ELSIF sym = ORS.type      THEN ws("TYPE")
  ELSIF sym = ORS.var       THEN ws("VAR")
  ELSIF sym = ORS.procedure THEN ws("PROCEDURE")
  ELSIF sym = ORS.begin     THEN ws("BEGIN")
  ELSIF sym = ORS.import    THEN ws("IMPORT")
  ELSIF sym = ORS.module    THEN ws("MODULE")
  ELSE ws("<sym ");  wi(sym);  wsn(">");  ASSERT(FALSE)
  END
END wSym;

PROCEDURE wObject*(x: OCB.Object);
BEGIN
  IF x = NIL THEN H.Trap(0, "x NIL in wObject") END;
  (*IF x.name # "" THEN ws(x.name);  ws("(") END;*)
  IF    x.class = OCB.Head       THEN ws("Head ")
  ELSIF x.class = OCB.Const      THEN (*s("Const ");*)   wConst(x)
  ELSIF x.class = OCB.String     THEN ws("String ");     wAdr(x)
  ELSIF x.class = OCB.Typ        THEN ws("Typ ");        ws(x.type.typobj.name)
  ELSIF x.class = OCB.Proc       THEN ws("Proc ");       wAdr(x)
  ELSIF x.class = OCB.GlobVar    THEN ws("M");           wAdr(x)
  ELSIF x.class = OCB.ProcVar    THEN ws("L");           wi(x.val)
  ELSIF x.class = OCB.ValPar     THEN ws("ValPar ");     wi(x.val)
  ELSIF x.class = OCB.RefPar     THEN ws("RefPar ");     wi(x.val)
  ELSIF x.class = OCB.Field      THEN ws("Field ");      wi(x.val)
  ELSIF x.class = OCB.StdProc    THEN ws("StdProc ");    wi(x.val)
  ELSIF x.class = OCB.StdFunc    THEN ws("StdFunc ");    wi(x.val)
  ELSIF x.class = OCB.Mod        THEN ws("Mod ");        ws(x.name)
  ELSIF x.class = OCB.Expression THEN ws("Expression "); wSym(x.val)
  ELSIF x.class = OCB.Statement  THEN ws("Statement ");
  ELSIF x.class = OCB.Assign     THEN ws("Assign ");  (*          "); *)
  ELSIF x.class = OCB.Param      THEN ws("Param ");  (*           "); *)
  ELSIF x.class = OCB.If         THEN ws("If ");  (*              "); *)
  ELSIF x.class = OCB.Elsif      THEN ws("Elsif ");  (*           "); *)
  ELSIF x.class = OCB.Else       THEN ws("Else ");  (*            "); *)
  ELSIF x.class = OCB.While      THEN ws("While ");  (*           "); *)
  ELSIF x.class = OCB.Repeat     THEN ws("Repeat ");  (*          "); *)
  ELSIF x.class = OCB.Case       THEN ws("Case ");  (*            "); *)
  ELSIF x.class = OCB.Caseval    THEN ws("Caseval ");  (*         "); *)
  ELSE
    ws("<"); wi(x.class); (*ws(", op "); wOp(x.op, 1);*) ws(">")
  END;
  (*IF x.name # "" THEN ws(")") END;*)
  (*IF x.op # 0 THEN wc("(");  wOp(x.op, 1);  ws(")") END;*)
  wa
END wObject;

PROCEDURE wExpr(x: OCB.Object);
BEGIN
  IF ~(x.class = OCB.Expression) THEN wObject(x)
  ELSE
    ws("[");
    IF x.dsc = NIL THEN
      wSym(x.val);  ws(" ");  wExpr(x.next)
    ELSE
      wExpr(x.next);  ws(" ");  wSym(x.val);  ws(" ");  wExpr(x.dsc)
    END;
    ws("]")
  END
END wExpr;

PROCEDURE wStdProc(n: INTEGER);
BEGIN
  IF    n =  0 THEN ws("INC")
  ELSIF n =  1 THEN ws("DEC")
  ELSIF n =  2 THEN ws("INCL")
  ELSIF n =  3 THEN ws("EXCL")
  ELSIF n =  4 THEN ws("ASSERT")
  ELSIF n =  5 THEN ws("NEW")
  ELSIF n =  6 THEN ws("PACK")
  ELSIF n =  7 THEN ws("UNPACK")
  ELSIF n =  8 THEN ws("LED")
  ELSIF n = 10 THEN ws("GET")
  ELSIF n = 11 THEN ws("PUT")
  ELSIF n = 12 THEN ws("COPY")
  ELSIF n = 13 THEN ws("LDPSR")
  ELSIF n = 14 THEN ws("LDREG")
  ELSE ws("<");  wi(n);  ws(">")
  END
END wStdProc;

PROCEDURE wStdFunc(n: INTEGER);
BEGIN
  IF    n =  0 THEN ws("ABS")
  ELSIF n =  1 THEN ws("ODD")
  ELSIF n =  2 THEN ws("FLOOR")
  ELSIF n =  3 THEN ws("FLOAT")
  ELSIF n =  4 THEN ws("ORD")
  ELSIF n =  5 THEN ws("CHR")
  ELSIF n =  6 THEN ws("LEN")
  ELSIF n =  7 THEN ws("LSL")
  ELSIF n =  8 THEN ws("ASR")
  ELSIF n =  9 THEN ws("ROR")
  ELSIF n = 11 THEN ws("ADC")
  ELSIF n = 12 THEN ws("SBC")
  ELSIF n = 13 THEN ws("UML")
  ELSIF n = 14 THEN ws("BIT")
  ELSIF n = 15 THEN ws("REG")
  ELSIF n = 16 THEN ws("VAL")
  ELSIF n = 17 THEN ws("ADR")
  ELSIF n = 18 THEN ws("SIZE")
  ELSIF n = 19 THEN ws("COND")
  ELSE ws("<");  wi(n);  ws(">")
  END
END wStdFunc;

PROCEDURE wOp(op: ARRAY OF CHAR);
BEGIN whr(PC, 5`, "0");  wtab(10);  ws(op);  wtab(6) END wOp;


(* ------------------------------------------------------------------------ *)

PROCEDURE NewItem*(class, val, ext: INTEGER;
                   type:            OCB.Type;
                   next, dsc:       OCB.Object): OCB.Object;
VAR obj: OCB.Object;
BEGIN
  NEW(obj);
  obj.class := class;  obj.rdo   := FALSE;
  obj.val   := val;    obj.ext   := ext;
  obj.exno  := 0;      obj.type  := type;
  obj.name  := "";     obj.expo  := FALSE;
  obj.next  := next;   obj.dsc   := dsc;
  obj.uses  := {};     obj.modno := ORS.linenum;
  (*ws("NewItem         "); wObject(obj); wn;*)
RETURN obj END NewItem;

PROCEDURE MakeRealItem*(val: REAL): OCB.Object;
VAR x: OCB.Object;
BEGIN wsn("MakeRealItem.")
RETURN x END MakeRealItem;

PROCEDURE MakeStringItem*(len: INTEGER): OCB.Object;
VAR x: OCB.Object;
BEGIN ws("MakeStringItem  '");  ws(ORS.str);
  ws("', len ");  wi(len);  wn;
  x := NewItem(OCB.String, 0, len, OCB.strType, NIL, NIL);
RETURN x END MakeStringItem;

PROCEDURE IsSingleCharacterString*(x: OCB.Object): BOOLEAN;
RETURN (x.class = OCB.String) & (x.ext = 2) END IsSingleCharacterString;

PROCEDURE StrToChar*(VAR x: OCB.Object);
BEGIN
  ASSERT(x.class = OCB.String);
  ASSERT(x.rdo);
  x := NewItem(OCB.Const, Text[x.val], 0, OCB.charType, NIL, NIL)
END StrToChar;


PROCEDURE GenTypeTest*(VAR x: OCB.Object;  t: OCB.Type;  varpar, isguard: BOOLEAN): OCB.Object;
BEGIN ASSERT(FALSE);
RETURN NIL END GenTypeTest;

PROCEDURE GenIndex*(x, y: OCB.Object): OCB.Object;
BEGIN ASSERT(FALSE);
RETURN x END GenIndex;

PROCEDURE GenDeref*(x: OCB.Object): OCB.Object;
BEGIN ASSERT(FALSE);
RETURN x END GenDeref;

PROCEDURE GenField*(x, y: OCB.Object; level: INTEGER): OCB.Object;
BEGIN ASSERT(FALSE);
RETURN x END GenField;

PROCEDURE GenParam*(x, y: OCB.Object): OCB.Object;
BEGIN
  (*ws("GenParam        value ");  wObject(x);  ws(", formal: ");  wObject(y);  wn*)
RETURN x END GenParam;

PROCEDURE GenStdFunc*(fct: INTEGER; x, y: OCB.Object): OCB.Object;
BEGIN
  (*
  ws("GenStdFunc      ");  wStdFunc(fct);
  IF x # NIL THEN ws(", x ");  wObject(x) END;
  IF y # NIL THEN ws(", y ");  wObject(y) END;
  wn;
  *)
RETURN x END GenStdFunc;

PROCEDURE GenStdProc*(pno: INTEGER; x, y: OCB.Object);
BEGIN
  (*
  ws("GenStdProc      ");  wStdProc(pno);
  IF x # NIL THEN ws(", x ");  wObject(x) END;
  IF y # NIL THEN ws(", y ");  wObject(y) END;
  wn;
  *)
END GenStdProc;

(* Use of OCB.Object fields for operations:

   class: BYTE     OCB.Expression
   type:  Type     Result type
   val:   INTEGER  Operation symbol (e.g. ORS.plus, ORS.div etc)
   ext:   INTEGER  Result register
   next:  Object   first param subexpression or variable
   dsc:   Object   second param subexpression or variable (optional)
   modno: INTEGER  source linenumber of operation
   name:  Ident    unused
   exno:  BYTE     unused
*)

PROCEDURE GenOperation*(sym: INTEGER; x, y: OCB.Object): OCB.Object;
BEGIN
  x := NewItem(OCB.Expression, sym, -1, x.type, x, y);
  IF x.next.class = OCB.Expression THEN x.uses := x.next.uses END;
  IF (x.dsc # NIL) & (x.dsc.class = OCB.Expression) THEN
    x.uses := x.uses + x.dsc.uses
  END;
  IF (x.type.form = OCB.Int64) & (x.val IN {ORS.div, ORS.mod, ORS.times}) THEN
    x.uses := x.uses + {RAX, RDX}
  ELSIF (x.val IN {ORS.eql .. ORS.geq})
      & (x.next.type.form IN {OCB.StrAdr, OCB.Array}) THEN
    x.uses := x.uses + {RCX, RSI, RDI}
  END;
RETURN x END GenOperation;

PROCEDURE GenPrepCall*(VAR x: OCB.Object);
BEGIN (*ws("GenPrepCall     ");  wObject(x);  wn*)
END GenPrepCall;

PROCEDURE GenCall*(VAR x: OCB.Object);
BEGIN (*ws("GenCall         ");  wObject(x);  wn*)
END GenCall;

PROCEDURE GenAssert*(x: OCB.Object);
BEGIN ASSERT(FALSE);
END GenAssert;

PROCEDURE GenCopy*(x, y, z: OCB.Object);
BEGIN ASSERT(FALSE);
END GenCopy;

PROCEDURE StartStatement*; END StartStatement;

PROCEDURE GenAssign*(x, y: OCB.Object): OCB.Object;
(* Encapsulates Store, StoreStruct and CopyString *)
BEGIN (*ws("GenStore x ");  wObject(x);  ws(" := y ");  wExpr(y);  wn;*)
RETURN NewItem(OCB.Assign, 0, 0, NIL, x, y) END GenAssign;

PROCEDURE GenFor0*(x, y: OCB.Object);
BEGIN ASSERT(FALSE)
END GenFor0;

PROCEDURE GenFor1*(x, y, z, w: OCB.Object; jmp: INTEGER);
BEGIN ASSERT(FALSE)
END GenFor1;

PROCEDURE GenFor2*(x, y, w: OCB.Object);
BEGIN ASSERT(FALSE)
END GenFor2;

PROCEDURE BuildTD*(tp: OCB.Type; varsize: INTEGER);
BEGIN ws("BuildTD for ");  wType(tp);  ws(", varsize ");  wi(varsize);  wn
END BuildTD;

PROCEDURE GenEnter*(x: OCB.Object);
BEGIN wn; ws("GenEnter ");
  IF x = NIL THEN ws("<MODULE>") ELSE wObject(x) END;
  wn
END GenEnter;


(* ----------------------- Expression code generation ----------------------- *)

PROCEDURE wSet(s: SET);
VAR i: INTEGER;
BEGIN
  i := 0;  WHILE (i < 64) & ~(i IN s) DO INC(i) END;
  IF i = 64 THEN ws("{}")
  ELSE
    wc("{");  wi(i);  INC(i);
    WHILE i < 63 DO
      INC(i);  IF i IN s THEN wc(",");  wi(i) END
    END;
    wc("}")
  END
END wSet;

(*
PROCEDURE ChooseReg(request: INTEGER; avoid: SET): INTEGER;
BEGIN INCL(avoid, RSP);
  IF request < 0 THEN request := 0 END;
  WHILE request IN avoid DO INC(request) END
RETURN request END ChooseReg;
*)

PROCEDURE wExprObj(x: OCB.Object);
BEGIN
  IF x.class = OCB.Expression THEN wReg(x.ext) ELSE wObject(x) END
END wExprObj;

PROCEDURE wInst(x: OCB.Object; reg: INTEGER);
BEGIN
  IF (x.class = OCB.Expression)
  &  (x.type.form = OCB.Int64)
  &  (x.val IN {ORS.div, ORS.mod, ORS.times}) THEN
    IF x.val = ORS.mod THEN ASSERT(reg = RDX) ELSE ASSERT(reg = RAX) END
  END;
  whr(PC, 5, "0");  wtab(10);
  IF    x.type.form = OCB.Int64 THEN wc("i")
  ELSIF x.type.form = OCB.Real  THEN wc("r")
  ELSIF x.type.form = OCB.Set   THEN wc("b")
  ELSE                               wc("?")
  END;
  wSym(x.val);  wtab(6);
  wReg(reg);
  IF x.dsc # NIL THEN ws(", ");  wExprObj(x.dsc) END;
  wn;
  INC(PC)
END wInst;

(*
PROCEDURE SaveReg(reg: INTEGER;  VAR keep: SET;  extra: SET;  VAR save: INTEGER);
BEGIN
  IF reg IN keep THEN
    save := ChooseReg(-1, keep + extra);
    EXCL(keep, reg);  INCL(keep, save);
    wOp("mov");  wReg(save);  ws(", ");  wReg(reg);  wsn("  ; save");  INC(PC)
 ELSE
    save := -1
  END
END SaveReg;

PROCEDURE GenExpression(x: OCB.Object; request: INTEGER; maintain: SET);
VAR reg: INTEGER;  type: CHAR;  avoid: SET;

  PROCEDURE LoadExpr(x: OCB.Object; request: INTEGER; maintain: SET);
  BEGIN
    IF x.class = OCB.Expression THEN
      GenExpression(x, request, maintain)
    ELSE
      ASSERT(request >= 0);
      wOp("mov");  wReg(request);  ws(", ");  wObject(x);  wn;  INC(PC)
    END
  END LoadExpr;

  PROCEDURE GenDivMod(x: OCB.Object; request: INTEGER; maintain: SET);
  (* Special case register handling of Intel DIV and MOD *)
  VAR saveax, savedx: INTEGER;
  BEGIN
    ASSERT((request < 0) OR ~(request IN maintain));

    IF request < 0 THEN
      IF x.val = ORS.div THEN request := RAX ELSE request := RDX END;
      IF request IN maintain THEN request := ChooseReg(-1, maintain) END
    END;

    IF (request >= 0) & (request # RAX) & (request # RDX) THEN
      INCL(maintain, request)
    END;

    SaveReg(RAX, maintain, {RDX}, saveax);
    SaveReg(RDX, maintain, {},    savedx);

    IF x.next.class = OCB.Expression THEN
      GenExpression(x.next, RAX, maintain)
    ELSE
      LoadExpr(x.next, RAX, maintain)
    END;

    IF x.dsc.class = OCB.Expression THEN GenExpression(x.dsc, -1, maintain + {RAX}) END;

    wOp("xor   edx, edx");  wn;  INC(PC);
    wOp("div");  wObject(x.dsc);  wn;  INC(PC);

    (* Set result register *)

    IF (x.val = ORS.div) & (request # RAX) THEN
      wOp("mov");  wReg(request);  wsn(", rax");  INC(PC)
    ELSIF (x.val = ORS.mod) & (request # RDX) THEN
      wOp("mov");  wReg(request);  wsn(", rdx");  INC(PC)
    END;

    IF saveax >= 0 THEN wOp("mov   rax, ");  wReg(saveax);  wsn("  ; restore");  INC(PC) END;
    IF savedx >= 0 THEN wOp("mov   rdx, ");  wReg(savedx);  wsn("  ; restore");  INC(PC) END;

    x.ext := request
  END GenDivMod;

BEGIN
  ASSERT(x.class = OCB.Expression);
  ASSERT(~(request IN maintain));
  ASSERT(x.ext = -1);  (* Register not yet allocated *)

  IF (x.type.form = OCB.Int64) & (x.val IN {ORS.div, ORS.mod}) THEN
    GenDivMod(x, request, maintain)
  ELSE
    reg  := ChooseReg(request, maintain + x.uses);

    IF x.next.class = OCB.Expression THEN
      GenExpression(x.next, reg, maintain);  ASSERT(x.next.ext = reg)
    ELSE
      LoadExpr(x.next, reg, maintain)
    END;

    ASSERT(reg >= 0);
    ASSERT(~(reg IN maintain));
    INCL(maintain, reg);

    IF (x.dsc # NIL) & (x.dsc.class = OCB.Expression) THEN
      GenExpression(x.dsc, -1, maintain)
    END;

    wInst(x, reg);

    IF (request < 0) OR (request = reg) THEN
      x.ext := reg
    ELSE
      wOp("mov");  wReg(request);  ws(", ");  wReg(reg);  wn;  INC(PC);
      x.ext := request
    END
  END
END GenExpression;
*)


(* --------------------------------------------------------------------------
*
*         Register allocation strategy
*         ----------------------------
*
*         OCP calls GenOperation which records the alu operations required
*         to evaluate and expression as a tree.
*         GenOperation recognises operations that have processor specific
*         register demands, and records and propagates required registers
*         in the Objct.uses SET.
*
*         LoadReg takes the following parameters:
*           x:      Object   - the expression of variable to be loaded
*           resreg: INTEGER  - register to be loaded
*           keep:   SET      - registers whose values are to be unchanged
*                              following evaluation
*
* Note    It is the job of the LoadReg caller to determine an appropriate
*         register.
*
*         Code generation for a dyadic operations
*         ---------------------------------------
*
*         The evaluation of a dyadic operation first requires the evaluation
*         of its left and right arguments.
*
*         Some X64 operators require specific registers for their left
*         arguments:
*
*           DIV/IDIV  rax := rdx:rax div r/m64,  rdx := rdx:rax rem r/m64
*           MUL/IMUL  rdx:rax := rax * r/m64
*                     (IMUL r/rm64 * constant32 has no reg limit)
*
*         Where possible, when asking LoadReg to code for DIV or MUL, try to
*         request rax (or rdx for MOD) as the result register.
*
*         The keep set passed to LoadReg should never include a register
*         required by the evaluation .uses, nor the result register.
*
*         Since the left parameter to DIV, MUL must be RAX, it makes sense
*         to evaluate the right arg first, generating a register other than
*         rax or rdx, then evaluate the left arg to rax, then (for DIV) clear
*         rdx.
*
*         Function calls
*         --------------
*
*         Function evaluate may use any register. Functions are not required
*         to restore any registers.
*
*         Therefore when encountering a function call in mid expresssion, any
*         registers recorded in keep must be saved first and restored afterwards.
*
*
* --------------------------------------------------------------------------- *)


(*
PROCEDURE GenOp(x: OCB.Object;  VAR resreg: INTEGER;  keep: SET);
VAR reg, p1, p2: INTEGER;
BEGIN
  ASSERT(x.class = OCB.Expression);
  ASSERT(x.ext = -1);  (* Register not yet allocated *)

  p1 := resreg;
  IF (x.type.form = OCB.Int64)
   & (x.val IN {ORS.div, ORS.mod})
   & ~(RAX IN x.dsc.uses) THEN
    p1 := RAX
  END;

  LoadReg(x.next, p1, keep + x.uses);

  IF (x.dsc # NIL) & (x.dsc.class = OCB.Expression) THEN
    p2 := -1;
    LoadReg(x.dsc, p2, keep + x.uses + {p1})
  END;

  IF (x.type.form = OCB.Int64) & (x.val IN {ORS.div, ORS.mod}) THEN
    wOp("xor   edx, edx");        wn;  INC(PC);
    IF p1 # RAX THEN
      wOp("mov");  ws("rax, ");  wReg(p1);  wn;  INC(PC)
    END;
    wOp("div");  wObject(x.dsc);  wn;  INC(PC);
    IF x.val = ORS.div THEN p1 := RAX ELSE p1 := RDX END;
  ELSE
    wInst(x, p1)
  END;

  IF resreg < 0 THEN resreg := p1
  ELSIF resreg # p1 THEN
    wOp("mov");  wReg(resreg);  ws(", ");  wReg(p1);  wn;  INC(PC)
  END;

  x.ext := resreg


(*
  ASSERT(~(request IN maintain));

  IF (x.type.form = OCB.Int64) & (x.val IN {ORS.div, ORS.mod}) THEN
    GenDivMod(x, request, maintain)
  ELSE
    reg  := ChooseReg(request, maintain + x.uses);

    IF x.next.class = OCB.Expression THEN
      GenExpression(x.next, reg, maintain);  ASSERT(x.next.ext = reg)
    ELSE
      LoadExpr(x.next, reg, maintain)
    END;

    ASSERT(reg >= 0);
    ASSERT(~(reg IN maintain));
    INCL(maintain, reg);

    IF (x.dsc # NIL) & (x.dsc.class = OCB.Expression) THEN
      GenExpression(x.dsc, -1, maintain)
    END;

    wInst(x, reg);

    IF (request < 0) OR (request = reg) THEN
      x.ext := reg
    ELSE
      wOp("mov");  wReg(request);  ws(", ");  wReg(reg);  wn;  INC(PC);
      x.ext := request
    END
  END
*)
END GenOp;
*)

PROCEDURE ImplicitLeftReg(x: OCB.Object): INTEGER;
VAR reg: INTEGER;
BEGIN reg := -1;
  IF (x # NIL)
  &  (x.class = OCB.Expression)
  &  (x.type.form = OCB.Int64)
  &  (x.val IN {ORS.div, ORS.mod, ORS.times}) THEN
    IF x.val = ORS.mod THEN reg := RDX ELSE reg := RAX END
  END
RETURN reg END ImplicitLeftReg;

PROCEDURE SelectRegister(x: OCB.Object;  keep: SET): INTEGER;
VAR r: INTEGER;
BEGIN
  r := ImplicitLeftReg(x);
  IF (r < 0) OR (r IN keep) THEN
    r := 0;  WHILE r IN keep DO INC(r) END;
    ASSERT(r < 16)  (* Out of registers *)
  END
RETURN r END SelectRegister;

PROCEDURE Evaluate(x: OCB.Object;  reg: INTEGER;  keep: SET);
VAR lp, rp: INTEGER;  (* Lft and right parameter registers *)
BEGIN
  ASSERT(x.class = OCB.Expression);
  IF (x.dsc # NIL) & (x.dsc.class = OCB.Expression) THEN  (* Evaluate right argument *)
    rp := SelectRegister(x.dsc, keep + {reg});
    LoadReg(x.dsc, rp, keep);
    INCL(keep, rp)
  END;
  lp := ImplicitLeftReg(x);  IF lp < 0 THEN lp := reg END;
  LoadReg(x.next, lp, keep);
  IF (x.class = OCB.Expression) &  (x.type.form = OCB.Int64) &  (x.val IN {ORS.div, ORS.mod}) THEN
    wOp("xor");  ws("edx, edx");  wn;  INC(PC)`
  END;
  wInst(x, lp);
  IF lp # reg THEN
    wOp("mov");  wReg(reg);  ws(", ");  wReg(lp);  wn;  INC(PC);
  END;
  x.ext := reg
END Evaluate;

PROCEDURE LoadReg0(x: OCB.Object; reg: INTEGER; keep: SET);
VAR lp, rp: INTEGER;  (* Left and right parameter registers *)
BEGIN
  ASSERT(reg >= 0);
  IF reg IN keep THEN
    ws("LoadReg(");  wObject(x);  ws("), reg ");  wReg(reg);
    ws(", keep ");  wRegSet(keep);  wn
  END;
  ASSERT(~(reg IN keep));
  IF x.class # OCB.Expression THEN
    wOp("mov");  wReg(reg);  ws(", ");  wObject(x);  wn;  INC(PC)
  ELSE  (* x is an expression *)
    ASSERT(x.uses * keep = {});
    Evaluate(x, reg, keep)
  END
END LoadReg0;

PROCEDURE LoadCond0(x: OCB.Object; keep: SET;  VAR t, f, cond: INTEGER);
BEGIN
  IF x.class # OCB.Expression THEN ASSERT(x.type.form = OCB.Bool);
    wOp("cmp");  wObject(x);  ws(", 0");  wn;  INC(PC);
    cond := ORS.neq
  END
END LoadCond0;


PROCEDURE InvertComparison(cond: INTEGER): INTEGER;
BEGIN
  IF    cond = ORS.eql THEN cond := ORS.neq
  ELSIF cond = ORS.neq THEN cond := ORS.eql
  ELSIF cond = ORS.lss THEN cond := ORS.geq
  ELSIF cond = ORS.leq THEN cond := ORS.gtr
  ELSIF cond = ORS.gtr THEN cond := ORS.leq
  ELSIF cond = ORS.geq THEN cond := ORS.lss
  ELSE ASSERT(FALSE)
  END
RETURN cond END InvertComparison;

PROCEDURE GenComparison(x: OCB.Object; maintain: SET; VAR t, f, cond: INTEGER);
BEGIN
  (*dup of GenExpression ...*)
END GenComparison;

PROCEDURE GenJumpForward(cond, chain: INTEGER): INTEGER;
BEGIN
  whr(PC, 5, "0");  wtab(10);
  wc("j");  wSym(cond);  wtab(6);
  IF chain > 0 THEN ws("chain ");  wi(chain) ELSE ws("fwd") END;
  wn;
  chain := PC;  INC(PC)
RETURN chain END GenJumpForward;

PROCEDURE GenCondition(x: OCB.Object; maintain: SET; VAR t, f, cond: INTEGER);
VAR i: INTEGER;
BEGIN
  IF x.class # OCB.Expression THEN
    ASSERT(x.type.form = OCB.Bool);
  ELSE
    IF x.val = ORS.not THEN
      i := t;  t := f;  f := i;  cond := InvertComparison(cond)
    ELSIF x.val IN {ORS.and, ORS.or} THEN
      GenCondition(x.next, maintain, t, f, cond);
      IF x.val = ORS.and THEN
        f := GenJumpForward(InvertComparison(cond), f)
      ELSE (* x.val = ORS.or *)
        t := GenJumpForward(cond, t)
      END;
      GenCondition(x.dsc, maintain, t, f, cond)
    ELSIF x.val IN {ORS.eql..ORS.geq} THEN
      GenComparison(x, maintain, t, f, cond)
    ELSIF x.val = ORS.in THEN ASSERT(FALSE)
    ELSIF x.val = ORS.is THEN ASSERT(FALSE)
    ELSE ASSERT(FALSE)
    END
  END;
END GenCondition;

PROCEDURE wStmtSeq(nest: INTEGER;  seq: OCB.Object);
  PROCEDURE wStmt(nest: INTEGER;  stmt: OCB.Object);
  VAR parm: OCB.Object;  n: INTEGER;
  BEGIN
    IF stmt # NIL THEN
      wc("[");  wi(stmt.modno);  ws("]  ");  wtab(10);  (* line number *)
      wObject(stmt);

      IF stmt.class = OCB.Assign  THEN (* dest, expr  *)
        wObject(stmt.next);  ws(" := ");  wExpr(stmt.dsc);  wn;
        (*
        IF stmt.dsc.class = OCB.Expression THEN
          GenExpression(stmt.dsc, -1, {RSP})
        END;
        wOp("mov");  wObject(stmt.next);  ws(", ");  wExprObj(stmt.dsc);  INC(PC)
        *)
        LoadReg(stmt.dsc, RAX, {});
        wOp("mov");  wObject(stmt.next);  ws(", rax");  INC(PC)

      ELSIF stmt.class = OCB.Param   THEN (* dest, expr  *) wObject(stmt.next);  ws(" := ");  wExpr(stmt.dsc)

      ELSIF stmt.class = OCB.If      THEN (* cond, body  *) wExpr(stmt.next);  ws(" THEN")

      ELSIF stmt.class = OCB.Elsif   THEN (* cond, body  *) wExpr(stmt.next);  ws(" THEN")

      ELSIF stmt.class = OCB.Else    THEN (* /   , body  *) ws("...")

      ELSIF stmt.class = OCB.While   THEN (* cond, body  *) wExpr(stmt.next);  ws(" DO")

      ELSIF stmt.class = OCB.Repeat  THEN (* cond, body  *) ws("... UNTIL ");  wExpr(stmt.dsc)

      ELSIF stmt.class = OCB.Case    THEN (* expr, /     *) wExpr(stmt.next)

      ELSIF stmt.class = OCB.Caseval THEN (* val , body  *) wExpr(stmt.next);  ws(":")

      ELSIF stmt.class = OCB.Proc    THEN (* /   , parms *) ws(" ");  wi(stmt.type.nofpar);  ws(" parms")
      END;
      wn;
      IF stmt.class IN {OCB.If,    OCB.Elsif,  OCB.Else,
                        OCB.While, OCB.Repeat, OCB.Caseval} THEN
        wStmtSeq(nest+1, stmt.dsc)
      ELSIF stmt.class = OCB.Proc THEN
        parm := stmt.type.dsc;  n := stmt.type.nofpar;
        WHILE (parm # NIL) & (n > 0) DO
          ws(" ");  wObject(parm);  wn;
          parm := parm.next;  DEC(n);
        END;
      END
    END
  END wStmt;
BEGIN IF seq # NIL THEN
  wStmtSeq(nest, seq.next);  wStmt(nest, seq.dsc)
END END wStmtSeq;

PROCEDURE GenBody*(proc, seq: OCB.Object);
BEGIN ws("GenBody ");
  IF proc = NIL THEN ws("<MAIN>") ELSE wObject(proc) END;  wn;
  wStmtSeq(1, seq)
END GenBody;

PROCEDURE GenReturn*(x: OCB.Object);
BEGIN wsn("GenReturn");
  IF x # NIL THEN
    ws("  Return ");  wExpr(x);  wn;
    (*GenExpression(x, RAX, {RSP});*)
    LoadReg(x, RAX, {});
    wOp("ret");  wn;  INC(PC);
  END
END GenReturn;

PROCEDURE GenOpenModule*(modid: ORS.Ident);
BEGIN wn;  ws("GenOpenModule   '");  ws(modid);  wsn("'")
END GenOpenModule;

PROCEDURE GenSetDataSize*(size: INTEGER);
BEGIN ws("GenSetDataSize  (");  wi(size);  wsn(")");
END GenSetDataSize;

PROCEDURE GenCloseModule*(modid: ORS.Ident; key, exno: INTEGER);
BEGIN
  ws("GenCloseModule  '");  ws(modid);
  ws("', key ");            wh(key);
  ws(", exno ");            wi(exno);  wn
END GenCloseModule;

BEGIN Texts.OpenWriter(W);
  PC       := 0;
  LoadReg  := LoadReg0;
  LoadCond := LoadCond0;
END OCG.