MODULE OCG;  (* DCWB 2024-08-10 Oberon common code generator *)

IMPORT SYSTEM, H := Host, Kernel, Files, Modules, Texts, ORS, OCB;

CONST
  TextSize   = 20000H;   (* Code, string and type descriptor buffer size (128KB) *)

  (* Named registers *)
  RAX = 0;  RCX = 1;  RDX = 2;  RBX = 3;
  RSP = 4;  RBP = 5;  RSI = 6;  RDI = 7;

TYPE
  Operand = RECORD  (* Representation of components of a general X64 reg/mem parameter *)
    direct: BOOLEAN;
    x:      INTEGER;
    r:      INTEGER;
    v:      OCB.Object;  (* String, Typ, Proc, GlobVar *)
    i:      INTEGER;
    s:      INTEGER;  (* 1, 2, 4 or 8, plus, if reg = -1: 3, 5, 9 *)
  END;

(*  Operand - represents possible kinds of X64 operation right hand side

    Direct:
      r < 0 - constant value in x
      else  - register value in r END

    Indirect:
      v - object providing address of String, Typ, Proc or GlobVar
      r - base register  (none if r < 0)
      x - offset in bytes
      i - index register (none if i < 0)
      s - scale 1, 2, 4 or 8
    END

  E.g., for ProcVar, ValPar and RefPar variables:
    direct - false
    x      - stack offset
    r      - RSP
    v      - NIL
    i, s   - optional scaled index
*)


VAR
  Text:  ARRAY TextSize OF BYTE;
  PC:    INTEGER;

  LoadReg:  PROCEDURE(x: OCB.Object; reg: INTEGER; keep: SET);
  LoadCond: PROCEDURE(x: OCB.Object; keep: SET;  VAR t, f, cond: INTEGER);



(* -------------------------------- Tracing --------------------------------- *)

PROCEDURE wa;                     BEGIN OCB.wa       END wa;
PROCEDURE wn;                     BEGIN OCB.wn       END wn;
PROCEDURE wc  (c: CHAR);          BEGIN OCB.wc(c)    END wc;
PROCEDURE ws  (s: ARRAY OF CHAR); BEGIN OCB.ws(s)    END ws;
PROCEDURE wsn (s: ARRAY OF CHAR); BEGIN OCB.wsn(s)   END wsn;
PROCEDURE wi  (i: INTEGER);       BEGIN OCB.wi(i)    END wi;
PROCEDURE wir (i, n: INTEGER);    BEGIN OCB.wir(i,n) END wir;

PROCEDURE wh  (n: INTEGER);       BEGIN OCB.wh(n)    END wh;
PROCEDURE wtab(n: INTEGER);       BEGIN OCB.wtab(n)  END wtab;

PROCEDURE wsl*(s: ARRAY OF CHAR; w: INTEGER; c: CHAR);  (* Left justified with trailing c *)
BEGIN ws(s);  DEC(w, H.Length(s));  WHILE w > 0 DO wc(c); DEC(w) END END wsl;

PROCEDURE wsr*(s: ARRAY OF CHAR; w: INTEGER; c: CHAR);  (* Right justified with leading c *)
BEGIN DEC(w, H.Length(s));  WHILE w > 0 DO wc(c); DEC(w) END;  ws(s) END wsr;

PROCEDURE whr*(n, w: INTEGER; c: CHAR);  (* Right justified with leading c *)
VAR hex: ARRAY 32 OF CHAR;
BEGIN H.IntToHex(n, hex); wsr(hex, w, c) END whr;

PROCEDURE wil(n, w: INTEGER);
VAR num: ARRAY 32 OF CHAR;
BEGIN H.IntToDecimal(n, num); wsl(num, w, " ") END wil;

PROCEDURE wSpace(n: INTEGER);
BEGIN WHILE n > 0 DO wc(" ");  DEC(n) END
END wSpace;


PROCEDURE wForm(form: INTEGER);
BEGIN
  IF    form = OCB.Byte    THEN ws("Byte")     ELSIF form = OCB.Bool    THEN ws("Bool")
  ELSIF form = OCB.Char    THEN ws("Char")     ELSIF form = OCB.Int8    THEN ws("Int8")
  ELSIF form = OCB.Int16   THEN ws("Int16")    ELSIF form = OCB.Int32   THEN ws("Int32")
  ELSIF form = OCB.Int64   THEN ws("Int64")    ELSIF form = OCB.Card16  THEN ws("Card16")
  ELSIF form = OCB.Card32  THEN ws("Card32")   ELSIF form = OCB.Real    THEN ws("Real")
  ELSIF form = OCB.Set     THEN ws("Set")      ELSIF form = OCB.Pointer THEN ws("Pointer")
  ELSIF form = OCB.NilTyp  THEN ws("NilTyp")   ELSIF form = OCB.NoTyp   THEN ws("NoTyp")
  ELSIF form = OCB.ProcAdr THEN ws("ProcAdr")  ELSIF form = OCB.StrAdr  THEN ws("StrAdr")
  ELSIF form = OCB.Array   THEN ws("Array")    ELSIF form = OCB.Record  THEN ws("Record")
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
  IF    c = OCB.Head       THEN ws("Head ")     ELSIF c = OCB.Const      THEN ws("Const ")
  ELSIF c = OCB.String     THEN ws("String ")   ELSIF c = OCB.Typ        THEN ws("Typ ")
  ELSIF c = OCB.Proc       THEN ws("Proc ")     ELSIF c = OCB.GlobVar    THEN ws("glob")
  ELSIF c = OCB.ProcVar    THEN ws("local")     ELSIF c = OCB.ValPar     THEN ws("ValPar ")
  ELSIF c = OCB.RefPar     THEN ws("RefPar ")   ELSIF c = OCB.Field      THEN ws("Field ")
  ELSIF c = OCB.StdProc    THEN ws("StdProc ")  ELSIF c = OCB.StdFunc    THEN ws("StdFunc ")
  ELSIF c = OCB.Mod        THEN ws("Module ")   ELSIF c = OCB.Expression THEN ws("Expr ")
  ELSE                     ws("<"); wi(c); ws(">")
  END;
END wClass;

PROCEDURE wType*(tp: OCB.Type);
BEGIN
  IF (tp.typobj # NIL) & (tp.typobj.name # "") THEN
    ws(tp.typobj.name);  ws(": ");
  END;
  wForm(tp.form)
END wType;

PROCEDURE wSym*(sym: INTEGER);
BEGIN
  IF    sym = ORS.times     THEN ws("*")          ELSIF sym = ORS.rdiv      THEN ws("/")
  ELSIF sym = ORS.div       THEN ws("DIV")        ELSIF sym = ORS.mod       THEN ws("MOD")
  ELSIF sym = ORS.and       THEN ws("&")          ELSIF sym = ORS.plus      THEN ws("+")
  ELSIF sym = ORS.minus     THEN ws("-")          ELSIF sym = ORS.or        THEN ws("OR")
  ELSIF sym = ORS.eql       THEN ws("=")          ELSIF sym = ORS.neq       THEN ws("#")
  ELSIF sym = ORS.lss       THEN ws("<")          ELSIF sym = ORS.leq       THEN ws("<=")
  ELSIF sym = ORS.gtr       THEN ws(">")          ELSIF sym = ORS.geq       THEN ws(">=")
  ELSIF sym = ORS.in        THEN ws("IN")         ELSIF sym = ORS.is        THEN ws("IS")
  ELSIF sym = ORS.arrow     THEN ws("^")          ELSIF sym = ORS.period    THEN ws(".")
  ELSIF sym = ORS.char      THEN ws("CHAR")       ELSIF sym = ORS.int       THEN ws("int")
  ELSIF sym = ORS.real      THEN ws("real")       ELSIF sym = ORS.false     THEN ws("FALSE")
  ELSIF sym = ORS.true      THEN ws("TRUE")       ELSIF sym = ORS.nil       THEN ws("NIL")
  ELSIF sym = ORS.string    THEN wc(22X)          ELSIF sym = ORS.not       THEN ws("~")
  ELSIF sym = ORS.lparen    THEN ws("(")          ELSIF sym = ORS.lbrak     THEN ws("[")
  ELSIF sym = ORS.lbrace    THEN ws("{")          ELSIF sym = ORS.ident     THEN ws("ident")
  ELSIF sym = ORS.if        THEN ws("IF")         ELSIF sym = ORS.while     THEN ws("WHILE")
  ELSIF sym = ORS.repeat    THEN ws("REPEAT")     ELSIF sym = ORS.case      THEN ws("CASE")
  ELSIF sym = ORS.for       THEN ws("FOR")        ELSIF sym = ORS.comma     THEN ws(",")
  ELSIF sym = ORS.colon     THEN ws(":")          ELSIF sym = ORS.becomes   THEN ws(":=")
  ELSIF sym = ORS.upto      THEN ws("..")         ELSIF sym = ORS.rparen    THEN ws(")")
  ELSIF sym = ORS.rbrak     THEN ws("]")          ELSIF sym = ORS.rbrace    THEN ws("}")
  ELSIF sym = ORS.then      THEN ws("THEN")       ELSIF sym = ORS.of        THEN ws("OF")
  ELSIF sym = ORS.do        THEN ws("DO")         ELSIF sym = ORS.to        THEN ws("TO")
  ELSIF sym = ORS.by        THEN ws("BY")         ELSIF sym = ORS.semicolon THEN ws(";")
  ELSIF sym = ORS.end       THEN ws("END")        ELSIF sym = ORS.bar       THEN ws("|")
  ELSIF sym = ORS.else      THEN ws("ELSE")       ELSIF sym = ORS.elsif     THEN ws("ELSIF")
  ELSIF sym = ORS.until     THEN ws("UNTIL")      ELSIF sym = ORS.return    THEN ws("RETURN")
  ELSIF sym = ORS.array     THEN ws("ARRAY")      ELSIF sym = ORS.record    THEN ws("RECORD")
  ELSIF sym = ORS.pointer   THEN ws("POINTER")    ELSIF sym = ORS.const     THEN ws("CONST")
  ELSIF sym = ORS.type      THEN ws("TYPE")       ELSIF sym = ORS.var       THEN ws("VAR")
  ELSIF sym = ORS.procedure THEN ws("PROCEDURE")  ELSIF sym = ORS.begin     THEN ws("BEGIN")
  ELSIF sym = ORS.import    THEN ws("IMPORT")     ELSIF sym = ORS.module    THEN ws("MODULE")
  ELSE ws("<sym ");  wi(sym);  wsn(">");  ASSERT(FALSE)
  END
END wSym;

PROCEDURE wObject*(x: OCB.Object);
BEGIN
  IF x = NIL THEN H.Trap(0, "x NIL in wObject") END;
  (*IF x.name # "" THEN ws(x.name);  ws("(") END;*)
  IF    x.class = OCB.Head       THEN ws("Head ")
  ELSIF x.class = OCB.Const      THEN (*ws("Const ");*)  wConst(x)
  ELSIF x.class = OCB.String     THEN ws("String ");     wAdr(x)
  ELSIF x.class = OCB.Typ        THEN ws("Typ ");        ws(x.type.typobj.name)
  ELSIF x.class = OCB.Proc       THEN ws("Proc ");       wAdr(x)
  ELSIF x.class = OCB.GlobVar    THEN ws("M");           wAdr(x)
  ELSIF x.class = OCB.ProcVar    THEN ws("L");           wi(x.val)
  ELSIF x.class = OCB.ValPar     THEN ws("ValPar ");     wi(x.ext)
  ELSIF x.class = OCB.RefPar     THEN ws("RefPar ");     wi(x.ext)
  ELSIF x.class = OCB.Field      THEN ws("Field ");      wi(x.val)
  ELSIF x.class = OCB.StdProc    THEN ws("StdProc ");    wi(x.val)
  ELSIF x.class = OCB.StdFunc    THEN ws("StdFunc ");    wi(x.val)
  ELSIF x.class = OCB.Mod        THEN ws("Mod ");        ws(x.name)
  ELSIF x.class = OCB.Expression THEN ws("Expression "); wSym(x.val)
  ELSIF x.class = OCB.Statement  THEN ws("Statement ");
  ELSIF x.class = OCB.Assign     THEN ws("Assign ");
  ELSIF x.class = OCB.Param      THEN ws("Param ");
  ELSIF x.class = OCB.If         THEN ws("If ");
  ELSIF x.class = OCB.Elsif      THEN ws("Elsif ");
  ELSIF x.class = OCB.Else       THEN ws("Else ");
  ELSIF x.class = OCB.While      THEN ws("While ");
  ELSIF x.class = OCB.Repeat     THEN ws("Repeat ");
  ELSIF x.class = OCB.Case       THEN ws("Case ");
  ELSIF x.class = OCB.Caseval    THEN ws("Caseval ");
(*ELSIF x.class = OCB.CallSite   THEN ws("CallSite ");*)
  ELSIF x.class = OCB.PassParam  THEN ws("PassParam ");
  ELSE
    ws("<"); wi(x.class); (*ws(", op "); wOp(x.op, 1);*) ws(">")
  END
  (*IF x.name # "" THEN ws(")") END;*)
  (*IF x.op # 0 THEN wc("(");  wOp(x.op, 1);  ws(")") END;*)
END wObject;

PROCEDURE wExpr(x: OCB.Object);
BEGIN
  IF x.class = OCB.Expression THEN
    ws("(");
    IF x.dsc = NIL THEN
      wSym(x.val);  ws(" ");  wExpr(x.next)
    ELSE
      wExpr(x.next);  ws(" ");  wSym(x.val);  ws(" ");  wExpr(x.dsc)
    END;
    ws(")")
  ELSE
    wObject(x)
  END;


END wExpr;

PROCEDURE wStdProc(n: INTEGER);
BEGIN
  IF    n =  0 THEN ws("INC")     ELSIF n =  1 THEN ws("DEC")
  ELSIF n =  2 THEN ws("INCL")    ELSIF n =  3 THEN ws("EXCL")
  ELSIF n =  4 THEN ws("ASSERT")  ELSIF n =  5 THEN ws("NEW")
  ELSIF n =  6 THEN ws("PACK")    ELSIF n =  7 THEN ws("UNPACK")
  ELSIF n =  8 THEN ws("LED")     ELSIF n = 10 THEN ws("GET")
  ELSIF n = 11 THEN ws("PUT")     ELSIF n = 12 THEN ws("COPY")
  ELSIF n = 13 THEN ws("LDPSR")   ELSIF n = 14 THEN ws("LDREG")
  ELSE ws("<");  wi(n);  ws(">")
  END
END wStdProc;

PROCEDURE wStdFunc(n: INTEGER);
BEGIN
  IF    n =  0 THEN ws("ABS")    ELSIF n =  1 THEN ws("ODD")
  ELSIF n =  2 THEN ws("FLOOR")  ELSIF n =  3 THEN ws("FLOAT")
  ELSIF n =  4 THEN ws("ORD")    ELSIF n =  5 THEN ws("CHR")
  ELSIF n =  6 THEN ws("LEN")    ELSIF n =  7 THEN ws("LSL")
  ELSIF n =  8 THEN ws("ASR")    ELSIF n =  9 THEN ws("ROR")
  ELSIF n = 11 THEN ws("ADC")    ELSIF n = 12 THEN ws("SBC")
  ELSIF n = 13 THEN ws("UML")    ELSIF n = 14 THEN ws("BIT")
  ELSIF n = 15 THEN ws("REG")    ELSIF n = 16 THEN ws("VAL")
  ELSIF n = 17 THEN ws("ADR")    ELSIF n = 18 THEN ws("SIZE")
  ELSIF n = 19 THEN ws("COND")
  ELSE ws("<");  wi(n);  ws(">")
  END
END wStdFunc;

PROCEDURE wOp(op: ARRAY OF CHAR);
BEGIN whr(PC, 5, "0");  wtab(10);  ws(op);  wtab(6) END wOp;


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
  obj.modno := ORS.linenum;
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

(* Use of OCB.Object fields for OCB.Expression:

   class: BYTE     OCB.Expression
   type:  Type     Result type
   val:   INTEGER  Operation symbol (e.g. ORS.plus, ORS.div etc)
   ext:   INTEGER  Result register
   next:  Object   first param subexpression or variable
   dsc:   Object   second param subexpression or variable (optional)
   modno: INTEGER  source linenumber of operation
   name:  Ident    unused
   exno:  BYTE     unused
   rdo:   BOOLEAN
   expo:  BOOLAN
*)

PROCEDURE GenAssert*(x: OCB.Object);
BEGIN ASSERT(FALSE);
END GenAssert;

PROCEDURE GenCopy*(x, y, z: OCB.Object);
BEGIN ASSERT(FALSE);
END GenCopy;

PROCEDURE StartStatement*; END StartStatement;

PROCEDURE GenAssign*(x, y: OCB.Object): OCB.Object;
(* Encapsulates Store, StoreStruct and CopyString *)
BEGIN ws("GenAssign x ");  wObject(x);  ws(" := y ");  wExpr(y);  wn;
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

PROCEDURE wMonadic(x: OCB.Object;  reg: INTEGER);
BEGIN
  whr(PC, 5, "0");  wtab(10);
  IF    x.type.form = OCB.Int64 THEN wc("i")
  ELSIF x.type.form = OCB.Real  THEN wc("r")
  ELSIF x.type.form = OCB.Set   THEN wc("b")
  ELSE                               wc("?")
  END;
  wSym(x.val);  wtab(6);
  wReg(reg);
  wn;
  INC(PC)
END wMonadic;

PROCEDURE wrelnum(n: INTEGER);
BEGIN IF n > 0 THEN wc("+");  wi(n) END END wrelnum;

PROCEDURE wOperand(o: Operand);
BEGIN
  IF o.direct THEN
    IF o.r >= 0 THEN wReg(o.r) ELSE wi(o.x) END
  ELSE
    wc("[");
    IF o.v # NIL THEN  (* Relative to global/module var *)
      wObject(o.v);
      IF o.x # 0 THEN wrelnum(o.x) END;
      ASSERT(o.r < 0);
    ELSE
      IF o.r < 0 THEN
        wsn("Indirect operand relative to neither variable nor register");
      END;
      ASSERT(o.r >= 0);  (* Relative to register *)
      wReg(o.r);
      IF o.x # 0 THEN wrelnum(o.x) END
    END;
    IF o.i >= 0 THEN
      wc("+");  wReg(o.i);
      IF o.s > 1 THEN wc("*");  wi(o.s) END
    END;
    wc("]")
  END
END wOperand;

PROCEDURE wDyadic(x: OCB.Object;  reg: INTEGER;  rhs: Operand);
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
  wSym(x.val);  wtab(6);  wReg(reg);  ws(", ");  wOperand(rhs);  wn;
  INC(PC)
END wDyadic;


(* --------------------------------------------------------------------------
*
*         Register allocation strategy
*         ----------------------------
*
*         OCP calls GenOperation which records the alu operations required
*         to evaluate and expression as a tree.
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


PROCEDURE ImplicitResult(x: OCB.Object): INTEGER;
VAR result: INTEGER;
BEGIN result := -1;
  IF (x # NIL)
  &  (x.class = OCB.Expression)
  &  (x.type.form = OCB.Int64)
  &  (x.val IN {ORS.div, ORS.mod, ORS.times}) THEN
    IF x.val = ORS.mod THEN result := RDX ELSE result := RAX END
  END
RETURN result END ImplicitResult;

PROCEDURE ImplicitlyUsed(x: OCB.Object): SET;
VAR uses: SET;
BEGIN uses := {};
  IF (x # NIL)
  &  (x.class = OCB.Expression)
  &  (x.type.form = OCB.Int64)
  &  (x.val IN {ORS.div, ORS.mod, ORS.times}) THEN
    uses := {RAX, RDX};
  END
RETURN uses END ImplicitlyUsed;

PROCEDURE FirstFreeReg(avoid: SET): INTEGER;
VAR free: INTEGER;
BEGIN
  INCL(avoid, RSP);
  free := 0;  WHILE free IN avoid DO INC(free) END;
  IF free >= 16 THEN ORS.Mark("Out of registers") END
RETURN free END FirstFreeReg;

PROCEDURE SelReg(x: OCB.Object;  reg: INTEGER;  keep: SET): INTEGER;
VAR res: INTEGER;
BEGIN
  res := ImplicitResult(x);
  IF res < 0 THEN res := reg END;
  IF res < 0 THEN res := FirstFreeReg(keep) END
RETURN res END SelReg;

PROCEDURE InSet(r: INTEGER; s: SET): BOOLEAN;
RETURN (r >= 0) & (r < 64) & (r IN s) END InSet;

PROCEDURE DodgeRegisters(VAR r: INTEGER;  VAR avoid: SET);
VAR newr: INTEGER;
BEGIN
  IF InSet(r, avoid) THEN
    newr := FirstFreeReg(avoid);
    wOp("mov");  wReg(newr);  ws(", ");  wReg(r);
    ws("  ");  wtab(16); wsn("; dodge");  INC(PC);
    r := newr;  INCL(avoid, r)
  END
END DodgeRegisters;

PROCEDURE DodgeOperand(avoid: SET;  VAR o: Operand);
BEGIN
  (*ws("DodgeOperand avoid ");  wRegSet(avoid);  ws(" in ");  wOperand(o);  wn;*)
  DodgeRegisters(o.r, avoid);
  DodgeRegisters(o.i, avoid)
END DodgeOperand;

PROCEDURE ResolveOperand(x: OCB.Object;  reg: INTEGER;  keep: SET;  VAR o: Operand);
(* Returns object x in form suitable for AMD64 instruction (right) operand). *)
(* Generates code to prepare the result where necessary.                     *)
(* reg only used for expression evaluation: if >= 0 gives preferred          *)
(* (not required) register to use for expression result.                     *)
VAR dsco: Operand;  avoid: SET;
BEGIN
  ASSERT(x # NIL);
  o.r := -1;  o.x := 0;  o.v := NIL;  o.i := -1;
  ws("ResolveOperand, x.class = ");  wClass(x.class);  wn;
  IF x.class = OCB.Const THEN
    o.direct := TRUE;  o.x := x.val
  ELSIF x.class IN {OCB.String, OCB.Typ, OCB.Proc, OCB.GlobVar} THEN
    o.direct := FALSE;  o.v := x
  ELSIF x.class IN {OCB.ProcVar, OCB.ValPar} THEN
    o.direct := FALSE;  o.r := RSP;  o.x := x.val
  ELSIF x.class = OCB.RefPar THEN
    o.direct := FALSE;  o.r := SelReg(x.dsc, reg, keep);
    LoadReg(x.dsc, o.r, keep)
  (*
  ELSIF x.class = OCB.CallSite THEN
    o.direct := FALSE;  o.r := RSP;  o.x := 111;  (* function result *)
  *)
  ELSIF x.class = OCB.Expression THEN
    wObject(x);  wn;
    IF x.val = ORS.period THEN (* field *)
      ResolveOperand(x.next, reg, keep, o);
      IF o.direct THEN ws("o: ");  wOperand(o);  wn;  ASSERT(FALSE) END;
      ASSERT(x.dsc # NIL);
      ASSERT(x.dsc.class = OCB.Field);
      INC(o.x, x.dsc.val)
    ELSIF x.val = ORS.lbrak THEN (* index *)
      wsn("Index:");
      ResolveOperand(x.dsc, -1, keep, dsco);
      ResolveOperand(x.next, reg, keep, o);
      IF (dsco.direct) & (dsco.r < 0) THEN
        INC(o.x, dsco.x * x.next.type.size)
      ELSE
        ASSERT(o.i < 0);
        IF dsco.direct THEN
          o.i := dsco.r
        ELSE
          o.i := FirstFreeReg(keep);
          wOp("mov");  wReg(o.i);  ws(", ");  wOperand(dsco);  wn;  INC(PC)
        END;
        o.s := x.next.type.size;
      END
    ELSIF x.val = ORS.arrow THEN (* deref *)
      wsn("Deref:");
    ELSE  (* arithmetic or boolean instruction *)
      o.direct := TRUE;
      o.r := ImplicitResult(x);
      IF o.r < 0 THEN o.r := ImplicitResult(x.next) END;
      IF o.r < 0 THEN o.r := reg END;
      IF o.r < 0 THEN o.r := FirstFreeReg(keep) END;

      IF x.dsc # NIL THEN
        avoid := ImplicitlyUsed(x) + ImplicitlyUsed(x.next);
        ResolveOperand(x.dsc, -1, keep + avoid + {o.r}, dsco);
        DodgeOperand(avoid, dsco);
        IF dsco.r >= 0 THEN INCL(keep, dsco.r) END;
        IF dsco.i >= 0 THEN INCL(keep, dsco.i) END
      END;

      LoadReg(x.next, o.r, keep);

      IF (x.class = OCB.Expression) &  (x.type.form = OCB.Int64) &  (x.val IN {ORS.div, ORS.mod}) THEN
        wOp("xor");  ws("edx, edx");  wn;  INC(PC)
      END;

      IF x.dsc = NIL THEN wMonadic(x, o.r) ELSE wDyadic(x, o.r, dsco) END
    END
  ELSE
    wsn("x.class # expression:");
    ws("  x:      ");  wObject(x);       wn;
    IF x.next # NIL THEN ws("  x.next: ");  wObject(x.next);  wn END;
    IF x.dsc  # NIL THEN ws("  x.dsc:  ");  wObject(x.dsc);   wn END;
    ASSERT(FALSE);
  END
END ResolveOperand;

PROCEDURE LoadReg0(x: OCB.Object; reg: INTEGER; keep: SET);
VAR o: Operand;
BEGIN
  ASSERT(reg >= 0);
  ResolveOperand(x, reg, keep, o);
  IF (~o.direct) OR (o.r # reg) THEN
      wOp("mov");  wReg(reg);  ws(", ");  wOperand(o);  wn;  INC(PC)
  END
END LoadReg0;

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

PROCEDURE GenJumpForward(cond, chain: INTEGER): INTEGER;
BEGIN
  whr(PC, 5, "0");  wtab(10);
  wc("j");  wSym(cond);  wtab(6);
  IF chain > 0 THEN ws("chain ");  wh(chain) ELSE ws("fwd") END;
  wn;
  chain := PC;  INC(PC)
RETURN chain END GenJumpForward;

PROCEDURE LoadCond0(x: OCB.Object; keep: SET;  VAR t, f, cond: INTEGER);
VAR i, lp, rp: INTEGER;
BEGIN
  IF x.class = OCB.Expression THEN

    IF x.val IN {ORS.eql..ORS.geq} THEN  ASSERT(x.dsc # NIL);
      IF x.dsc.class = OCB.Expression THEN  (* Evaluate right argument *)
        rp := SelReg(x.dsc, -1, keep);
        LoadReg(x.dsc, rp, keep);
        INCL(keep, rp)
      END;
      lp := SelReg(x.next, -1, keep);
      LoadReg(x.next, lp, keep);
      wOp("cmp");  wReg(lp);  ws(", ");  wObject(x.dsc);  wn;  INC(PC);
      cond := x.val

    ELSIF x.val = ORS.not THEN
      i := t;  t := f;  f := i;  cond := InvertComparison(cond)

    ELSIF x.val IN {ORS.and, ORS.or} THEN
      LoadCond(x.next, keep, t, f, cond);
      IF x.val = ORS.and THEN
        f := GenJumpForward(InvertComparison(cond), f)
      ELSE (* x.val = ORS.or *)
        t := GenJumpForward(cond, t)
      END;
      LoadCond(x.dsc, keep, t, f, cond)

    ELSIF x.val = ORS.in THEN  ASSERT(FALSE)
    ELSIF x.val = ORS.is THEN  ASSERT(FALSE)
    ELSE ORS.Mark("Expression is not boolean")
    END
  ELSE ASSERT(x.type.form = OCB.Bool);
    wOp("cmp");  wObject(x);  ws(", 0");  wn;  INC(PC);
    cond := ORS.neq
  END
END LoadCond0;

PROCEDURE ResolveChain(chain: INTEGER);
BEGIN
  IF chain # 0 THEN
    ws("Resolve jump chain ending at ");  wh(chain);  wn
  END
END ResolveChain;

PROCEDURE PreprocessFunctionCalls(arg: OCB.Object);
BEGIN
  wsn("Preprocess function calls.");
  IF arg # NIL THEN
    IF arg.class = OCB.Expression THEN
      PreprocessFunctionCalls(arg.next);  PreprocessFunctionCalls(arg.dsc);
    ELSIF arg.class = OCB.Proc THEN
      wsn("*Preprocess function call*");
    END
  END
END PreprocessFunctionCalls;

PROCEDURE ProcessStmtSeq(nest, tempcount: INTEGER;  seq: OCB.Object;  VAR maxtemp: INTEGER);
VAR stmttemp: INTEGER;

  PROCEDURE ProcessStmt(nest, tempcount: INTEGER;  stmt: OCB.Object;  VAR maxtemp: INTEGER);
  VAR parm: OCB.Object;  t, f, n, cond: INTEGER;  o: Operand;  keep: SET;
  BEGIN
    keep := {};
    IF stmt # NIL THEN
      wc("[");  wi(stmt.modno);  ws("]  ");  wtab(10);  (* line number *)
      wObject(stmt);

      IF stmt.class = OCB.Assign  THEN (* dest, expr  *)
        wObject(stmt.next);  ws(" := ");  wExpr(stmt.dsc);  wn;
        PreprocessFunctionCalls(stmt.dsc);
        ResolveOperand(stmt.dsc, RAX, keep, o);
        wOp("mov");  wObject(stmt.next);  ws(", ");  wOperand(o);  INC(PC)

      ELSIF stmt.class = OCB.Param   THEN (* dest, expr  *)
        wObject(stmt.next);  ws(" := ");  wExpr(stmt.dsc);  wn;
        ResolveOperand(stmt.dsc, RAX, keep, o);
        wOp("mov");  ws("L111");  ws(", ");  wOperand(o);  INC(PC)

      ELSIF stmt.class = OCB.If      THEN (* cond, body  *)
        wExpr(stmt.next);  wn;
        t := 0;  f := 0;
        LoadCond(stmt.next, {}, t, f, cond);
        f := GenJumpForward(InvertComparison(cond), f);
        wsn("THEN");
        ResolveChain(t);
        ProcessStmtSeq(nest+1, maxtemp, stmt.dsc, maxtemp);
        ResolveChain(f)

      ELSIF stmt.class = OCB.Elsif   THEN (* cond, body  *) wExpr(stmt.next);  ws(" THEN")
      ELSIF stmt.class = OCB.Else    THEN (* /   , body  *) ws(" ELSE")
      ELSIF stmt.class = OCB.While   THEN (* cond, body  *) wExpr(stmt.next);  ws(" DO")
      ELSIF stmt.class = OCB.Repeat  THEN (* cond, body  *) ws("... UNTIL ");  wExpr(stmt.dsc)
      ELSIF stmt.class = OCB.Case    THEN (* expr, /     *) wExpr(stmt.next)
      ELSIF stmt.class = OCB.Caseval THEN (* val , body  *) wExpr(stmt.next);  ws(":")

      ELSIF stmt.class = OCB.Proc    THEN
        wn;
        ResolveOperand(stmt, RAX, keep, o);
        wOp("call");  wObject(stmt);  INC(PC);
        IF stmt.type # NIL THEN  (* It's a function *)
          wn;  wOp("mov");  ws("[rsp+111), rax");
          ws("  ");  wtab(16);  ws("; save function result");  INC(PC)
        END
      END;

      wn;
      IF stmt.class IN {OCB.If,    OCB.Elsif,  OCB.Else,
                        OCB.While, OCB.Repeat, OCB.Caseval} THEN
        ProcessStmtSeq(nest+1, maxtemp, stmt.dsc, maxtemp)
      END
    END
  END ProcessStmt;

BEGIN
  IF seq # NIL THEN
    ASSERT(seq.class = OCB.Statement);
    stmttemp := tempcount;

    ProcessStmtSeq(nest, tempcount, seq.next, stmttemp);
    IF stmttemp > maxtemp THEN maxtemp := stmttemp END;

    ProcessStmt(nest, tempcount, seq.dsc, maxtemp)
  END
END ProcessStmtSeq;


PROCEDURE MeasureExpressionLocals(x: OCB.Object;  VAR fnparams, fnresults: INTEGER);
BEGIN
  ws("MeasureExpressionLocals, x: ");  wObject(x);  wn;
  IF x # NIL THEN
    IF x.type.form = OCB.ProcAdr THEN
      ASSERT(x.type.len MOD 8 = 0);
      IF x.type.len > fnparams THEN fnparams := x.type.len END;
      IF x.type.base # NIL THEN INC(fnresults) END
    ELSIF x.class = OCB.Expression THEN
      MeasureExpressionLocals(x.next, fnparams, fnresults);
      MeasureExpressionLocals(x.dsc,  fnparams, fnresults)
    END
  END
END MeasureExpressionLocals;

PROCEDURE MeasureBlockLocals(x: OCB.Object): INTEGER;  (* x is first statement of block *)
VAR blockparams, blockresults, fnparams, fnresults, i: INTEGER;
BEGIN blockparams := 0;  blockresults := 0;
  IF x # NIL THEN
    ASSERT(x.class = OCB.Statement);
    WHILE x # NIL DO
      IF x.dsc.class = OCB.Assign THEN
        fnparams := 0;  fnresults := 0;
        MeasureExpressionLocals(x.dsc.dsc, fnparams, fnresults);
        IF fnparams  > blockparams  THEN blockparams  := fnparams  END;
        IF fnresults > blockresults THEN blockresults := fnresults END
      END;
      x := x.next;
    END;
    ws("Block local slots: params: ");  wi(blockparams);
    ws(", results: ");  wi(blockresults);  wn
  END
RETURN blockparams + blockresults END MeasureBlockLocals;

PROCEDURE GenBody*(proc, seq: OCB.Object);
VAR tempdepth: INTEGER;  localslots: INTEGER;
BEGIN ws("GenBody ");
  IF proc = NIL THEN ws("<MAIN>") ELSE wObject(proc) END;  wn;
  localslots := MeasureBlockLocals(seq);

  tempdepth := 0;
  ProcessStmtSeq(1, 0, seq, tempdepth);
  IF tempdepth # 0 THEN
    ws("Body requires ");  wi(tempdepth);  wsn(" temporary function results.")
  END;
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

PROCEDURE GenModuleOpen*(modid: ORS.Ident);
BEGIN wn;  ws("GenModuleOpen   '");  ws(modid);  wsn("'")
END GenModuleOpen;

PROCEDURE GenSetDataSize*(size: INTEGER);
BEGIN ws("GenSetDataSize  (");  wi(size);  wsn(")");
END GenSetDataSize;

PROCEDURE GenCloseModule*(modid: ORS.Ident; key, exno: INTEGER);
BEGIN
  ws("GenCloseModule  '");  ws(modid);
  ws("', key ");            wh(key);
  ws(", exno ");            wi(exno);  wn
END GenCloseModule;

BEGIN
  PC := 0;  LoadReg := LoadReg0;  LoadCond := LoadCond0
END OCG.