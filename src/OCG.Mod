MODULE OCG;  (* DCWB 2024-08-10 Oberon common code generator *)

IMPORT SYSTEM, H := Host, Kernel, Files, Modules, Texts, ORS, OCB;

CONST
  TextSize   = 20000H;   (* Code, string and type descriptor buffer size (128KB) *)

  (* Additional object classes               *)
  (*                   next         dsc      *)
  Condition*  = 20;  (* 1st arg      2nd arg  *)
  Expression* = 21;  (* 1st arg      2nd arg  *)
  Statement*  = 22;  (* prev stmt    detail   *)
  Assign*     = 23;  (* dest         expr     *)
  If*         = 24;  (* cond         body     *)
  Elsif*      = 25;  (* cond         body     *)
  Else*       = 26;  (* /            body     *)
  While*      = 27;  (* cond         body     *)
  Repeat*     = 28;  (* cond         body     *)
  Case*       = 29;  (* expr         /        *)
  Caseval*    = 30;  (* val          body     *)


VAR
  W: Texts.Writer;

  Text:        ARRAY TextSize OF BYTE;
  TextAdr:     INTEGER;



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

PROCEDURE whr*(n, w: INTEGER; c: CHAR);  (* Right justified with leading zeroes *)
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
  IF x.modno > 0 THEN
    ws("["); wi(x.modno); ws("]."); wi(x.val)
  ELSE
    wh(x.val); ws("H")
  END
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
      wi(s);
      IF e > s THEN
        IF e = s+1 THEN wc(",") ELSE ws("..") END;
        wi(e);
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
  IF    c = OCB.Head     THEN ws("Head ")
  ELSIF c = OCB.Const    THEN ws("Const ")
  ELSIF c = OCB.String   THEN ws("String ")
  ELSIF c = OCB.Typ      THEN ws("Typ ")
  ELSIF c = OCB.Proc     THEN ws("Proc ")
  ELSIF c = OCB.GlobVar  THEN ws("glob")
  ELSIF c = OCB.ProcVar  THEN ws("local")
  ELSIF c = OCB.ValPar   THEN ws("ValPar ")
  ELSIF c = OCB.RefPar   THEN ws("RefPar ")
  ELSIF c = OCB.Field    THEN ws("Field ")
  ELSIF c = OCB.StdProc  THEN ws("StdProc ")
  ELSIF c = OCB.StdFunc  THEN ws("StdFunc ")
  ELSIF c = OCB.Mod      THEN ws("Module ")
  ELSIF c = Condition    THEN ws("Cond ")
  ELSIF c = Expression   THEN ws("Expr ")
  (*
  ELSIF c = OCB.RegVal   THEN ws("RegVal")
  ELSIF c = OCB.RegRef   THEN ws("RegRef")
  ELSIF c = OCB.Expr     THEN ws("Expr ")
  ELSIF c = OCB.Jump     THEN ws("Jump ")
  *)
  ELSE                    ws("<"); wi(c); ws(">")
  END;
END wClass;

PROCEDURE wObject*(x: OCB.Object);
BEGIN
  IF x = NIL THEN H.Trap(0, "x NIL in wObject") END;
  IF x.name # "" THEN ws(x.name);  ws("(") END;
  IF    x.class = OCB.Head     THEN ws("Head ")
  ELSIF x.class = OCB.Const    THEN ws("Const ");      wConst(x)
  ELSIF x.class = OCB.String   THEN ws("String ");     wAdr(x)
  ELSIF x.class = OCB.Typ      THEN ws("Typ ");        ws(x.type.typobj.name)
  ELSIF x.class = OCB.Proc     THEN ws("Proc ");       wAdr(x)
  ELSIF x.class = OCB.GlobVar  THEN ws("g");           wAdr(x)
  ELSIF x.class = OCB.ProcVar  THEN ws("l");           wi(x.val)
  ELSIF x.class = OCB.ValPar   THEN ws("ValPar ");     wi(x.val)
  ELSIF x.class = OCB.RefPar   THEN ws("RefPar ");     wi(x.val)
  ELSIF x.class = OCB.Field    THEN ws("Field ");      wi(x.val)
  ELSIF x.class = OCB.StdProc  THEN ws("StdProc ");    wi(x.val)
  ELSIF x.class = OCB.StdFunc  THEN ws("StdFunc ");    wi(x.val)
  ELSIF x.class = OCB.Mod      THEN ws("Mod ");        ws(x.name)

  ELSIF x.class = Condition    THEN ws("Cond ");       wi(x.val)
  ELSIF x.class = Expression   THEN ws("Expression "); wi(x.val)
  ELSIF x.class = Statement    THEN ws("Statement ");
  ELSIF x.class = Assign       THEN ws("Assign ");
  ELSIF x.class = If           THEN ws("If ");
  ELSIF x.class = Elsif        THEN ws("Elsif ");
  ELSIF x.class = Else         THEN ws("Else ");
  ELSIF x.class = While        THEN ws("While ");
  ELSIF x.class = Repeat       THEN ws("Repeat ");
  ELSIF x.class = Case         THEN ws("Case ");
  ELSIF x.class = Caseval      THEN ws("Caseval ");
  ELSE
    ws("<"); wi(x.class); (*ws(", op "); wOp(x.op, 1);*) ws(">")
  END;
  IF x.name # "" THEN ws(")") END;
  (*IF x.op # 0 THEN wc("(");  wOp(x.op, 1);  ws(")") END;*)
  wa
END wObject;

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

PROCEDURE wExpr(x: OCB.Object);
BEGIN
  IF x.class # Expression THEN wObject(x)
  ELSE
    ws("['");  wSym(x.val);  ws("', ");
    wExpr(x.next);
    IF x.dsc # NIL THEN ws(", ");  wExpr(x.dsc) END;
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



(* ------------------------------------------------------------------------ *)

PROCEDURE NewItem*(class, val,
                   ext, modno: INTEGER;
                   type:       OCB.Type;
                   next, dsc:  OCB.Object): OCB.Object;
VAR obj: OCB.Object;
BEGIN
  NEW(obj);
  obj.class := class;  obj.rdo  := FALSE;
  obj.val   := val;    obj.ext  := ext;
  obj.modno := 0;      obj.type := type;
  obj.name  := "";     obj.expo := FALSE;
  obj.exno  := 0;
  obj.next  := next;   obj.dsc  := dsc;
  ws("NewItem         "); wObject(obj); wn;
RETURN obj END NewItem;

PROCEDURE MakeRealItem*(val: REAL): OCB.Object;
VAR x: OCB.Object;
BEGIN wsn("MakeRealItem.")
RETURN x END MakeRealItem;

PROCEDURE MakeStringItem*(len: INTEGER): OCB.Object;
VAR x: OCB.Object;
BEGIN ws("MakeStringItem  '");  ws(ORS.str);
  ws("', len ");  wi(len);  wn;
  x := NewItem(OCB.String, 0, len, 0, OCB.strType, NIL, NIL);
RETURN x END MakeStringItem;

PROCEDURE IsSingleCharacterString*(x: OCB.Object): BOOLEAN;
RETURN (x.class = OCB.String) & (x.ext = 2) END IsSingleCharacterString;

PROCEDURE StrToChar*(VAR x: OCB.Object);
BEGIN
  ASSERT(x.class = OCB.String);
  ASSERT(x.rdo);
  x := NewItem(OCB.Const, Text[x.val], 0, 0, OCB.charType, NIL, NIL)
END StrToChar;


PROCEDURE GenTypeTest*(VAR X: OCB.Object; T: OCB.Type; varpar, isguard: BOOLEAN);
BEGIN ASSERT(FALSE);
END GenTypeTest;

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
BEGIN ws("GenParam        value ");  wObject(x);  ws(", formal: ");  wObject(y);  wn
RETURN x END GenParam;

PROCEDURE GenStdFunc*(fct: INTEGER; x, y: OCB.Object): OCB.Object;
BEGIN ws("GenStdFunc      ");  wStdFunc(fct);
  IF x # NIL THEN ws(", x ");  wObject(x) END;
  IF y # NIL THEN ws(", y ");  wObject(y) END;
  wn;
RETURN x END GenStdFunc;

PROCEDURE GenStdProc*(pno: INTEGER; x, y: OCB.Object);
BEGIN ws("GenStdProc      ");  wStdProc(pno);
  IF x # NIL THEN ws(", x ");  wObject(x) END;
  IF y # NIL THEN ws(", y ");  wObject(y) END;
  wn;
END GenStdProc;

PROCEDURE GenOperation*(sym: INTEGER; VAR x: OCB.Object; y: OCB.Object);
BEGIN ws("GenOperation    ");  wSym(sym);
  IF x # NIL THEN ws(", x ");  wObject(x) END;
  IF y # NIL THEN ws(", y ");  wObject(y) END;
  wn;
  IF sym IN {ORS.and, ORS.or, ORS.not,
             ORS.eql, ORS.neq,
             ORS.lss, ORS.leq, ORS.gtr, ORS.geq,
             ORS.in,  ORS.is} THEN
    x := NewItem(Condition, sym, 0, 0, x.type, x, y)
  ELSE
    x := NewItem(Expression, sym, 0, 0, x.type, x, y)
  END
END GenOperation;

PROCEDURE GenPrepCall*(VAR x: OCB.Object);
BEGIN ws("GenPrepCall     ");  wObject(x);  wn
END GenPrepCall;

PROCEDURE GenCall*(VAR x: OCB.Object);
BEGIN ws("GenCall         ");  wObject(x);  wn
END GenCall;

PROCEDURE GenAssert*(x: OCB.Object);
BEGIN ASSERT(FALSE);
END GenAssert;

PROCEDURE GenCopy*(x, y, z: OCB.Object);
BEGIN ASSERT(FALSE);
END GenCopy;

PROCEDURE StartStatement*; END StartStatement;

PROCEDURE GenStore*(x, y: OCB.Object);
(* Encapsulates Store, StoreStruct and CopyString *)
BEGIN ws("GenStore        x ");  wObject(x);  ws(" := y ");  wExpr(y);  wn;
END GenStore;

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
BEGIN ws("BuildTD for     ");  wType(tp);  ws(", varsize ");  wi(varsize);  wn
END BuildTD;

PROCEDURE GenEnter*(x: OCB.Object);
BEGIN ws("GenEnter        ");
  IF x = NIL THEN ws("<MODULE>") ELSE wObject(x) END;
  wn
END GenEnter;

PROCEDURE GenReturn*(x: OCB.Object);
BEGIN ws("GenReturn");
  IF x # NIL THEN ws("(");  wObject(x);  ws(")") END;
  wn
END GenReturn;

PROCEDURE Generate*(x: OCB.Object);
BEGIN wsn("Generate.");  wn
END Generate;

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

BEGIN Texts.OpenWriter(W)
END OCG.