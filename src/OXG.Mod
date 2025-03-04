MODULE OXG;  (* DCWB 2023-06 to 2024-05; Code generator for X64 *)

(* Originally: N.Wirth, 16.4.2016 / 4.4.2017 / 31.5.2019 *)
(*             Oberon compiler; code generator for RISC  *)

IMPORT SYSTEM, H := Host, K := Kernel, Files, Modules, Texts, ORS, OXB;

CONST
  WordSize* = OXB.WordSize;
  MaxImpMod = 32;         (* Max no. imported modules *)
  TextSize  = 20000H;     (* Code, string and type descriptor buffer size (128KB) *)


  (* Classes from OXB *)
  Head    = OXB.Head;
  Const   = OXB.Const;
  String  = OXB.String;   (* String constant *)
  Typ     = OXB.Typ;
  Proc    = OXB.Proc;     (* Procedure body address constant *)
  GlobVar = OXB.GlobVar;  (* Module global / imported VAR *)
  ProcVar = OXB.ProcVar;  (* Procedure local variable *)
  ValPar  = OXB.ValPar;   (* Value parameter *)
  RefPar  = OXB.RefPar;   (* VAR parameter or structured value parameter *)
  Field   = OXB.Field;
  StdProc = OXB.StdProc;
  StdFunc = OXB.StdFunc;
  Mod     = OXB.Mod;

  (* Additional classes *)
  Cond    = 20;
  RegVal  = 21;
  RegRef  = 22;
  Expr    = 23;
  Jump    = 24;


  OpNone = 0;

  (* Niladic operations *)
  OpEnter* = 1;

  (* Monadic non-result-returning operations *)
  OpReturn*       = 12;
  OpNew*          = 14;
  OpLed*          = 15;
  OpValueParam*   = 18;
  OpOpenArrParam* = 19;
  OpByteArrParam* = 20;
  OpVarParam*     = 21;
  OpStringParam*  = 22;
  OpTestNZ        = 23;  (* Sets condition code flags *)
  OpCallProc      = 24;


  (* Dyadic non-result-returning operations *)
  OpStore        = 41;
  OpStoreStruc   = 42;
  OpCopyString   = 43;
  OpInc*         = 44;
  OpDec*         = 45;
  OpPack*        = 46;
  OpUnpack*      = 47;
  OpGet*         = 48;
  OpPut*         = 49;
  OpSetIncl*     = 50;  (* Op conditions must match ORS symbol conditions in order and contiguity *)
  OpSetExcl*     = 51;
  OpIntEql*      = 52;
  OpIntNeq*      = 53;
  OpIntLss*      = 54;
  OpIntLeq*      = 55;
  OpIntGtr*      = 56;
  OpIntGeq*      = 57;
  OpRealEql*     = 58;
  OpRealNeq*     = 59;
  OpRealLss*     = 60;
  OpRealLeq*     = 61;
  OpRealGtr*     = 62;
  OpRealGeq*     = 63;
  OpStringEql*   = 64;
  OpStringNeq*   = 65;
  OpStringLss*   = 66;
  OpStringLeq*   = 67;
  OpStringGtr*   = 68;
  OpStringGeq*   = 69;
  OpAnd*         = 70;
  OpOr*          = 71;
  OpNot*         = 72;

  (* Result-returning operations *)

  (* Monadic result-returning operations *)
  OpAbs*          = 91;
  OpSetSingleton* = 92;
  OpNegate*       = 93;
  OpRealNegate*   = 94;
  OpInvert*       = 95;  (* Set not *)
  OpOdd*          = 96;
  OpFloor*        = 97;
  OpFloat*        = 98;
  OpOrd*          = 99;
  OpLen*          = 100;
  OpLoad          = 101;
  OpAdr*          = 102;
  OpREG*          = 103;
  OpCond*         = 104;
  OpSetCC         = 105;
  OpCallFn        = 106;

  (* Dyadic expression result-returning operations *)
  OpLSL*          = 121;  (* LSL,ASR, ROR must be in same oder as stdfunc numbers *)
  OpASR*          = 122;
  OpROR*          = 123;
  OpADC*          = 124;
  OpSBC*          = 125;
  OpUML*          = 126;
  OpBit*          = 127;
  OpSetRange*     = 128;
  OpSetOr*        = 129;  (* '+' *)
  OpSetAndNot*    = 130;  (* '-' *)
  OpSetAnd*       = 131;  (* '*' *)
  OpSetXor*       = 132;  (* '/' *)
  OpIntPlus*      = 133;
  OpIntMinus*     = 134;
  OpIntMultiply*  = 135;
  OpIntDivide*    = 136;
  OpIntModulus*   = 137;
  OpRealPlus*     = 138;
  OpRealMinus*    = 139;
  OpRealMultiply* = 140;
  OpRealDivide*   = 141;
  OpIn*           = 142;
  OpLoadReg*      = 143;
  OpIndex*        = 144;
  OpField*        = 145;

  (* X64 Conditions *)
  CO*  = 00H;   (* Overflow                                            *)
  CNO* = 01H;   (* No overflow                                         *)
  CC*  = 02H;   (* Carry, Below                         (unsigned lss) *)
  CNC* = 03H;   (* No carry, Above or equal             (unsigned geq) *)
  CZ*  = 04H;   (* Zero, Equal                                   (eql) *)
  CNZ* = 05H;   (* Nonzero, Not equal                            (neq) *)
  CNA* = 06H;   (* Not above, Below or equal            (unsigned leq) *)
  CA*  = 07H;   (* Above, Not below or equal            (unsigned gtr) *)
  CS*  = 08H;   (* Sign, Negative                                      *)
  CNS* = 09H;   (* No sign, Positive or zero                           *)
  CP*  = 0AH;   (* Parity                                              *)
  CNP* = 0BH;   (* No parity                                           *)
  CL*  = 0CH;   (* Less than, Not greater or equal        (signed lss) *)
  CNL* = 0DH;   (* Not less than, Greater or equal        (signed geq) *)
  CNG* = 0EH;   (* Not greater than, Less than or equal   (signed leq) *)
  CG*  = 0FH;   (* Greater than, Not less than or equal   (signed gtr) *)

  (* Named registers *)
  RAX = 0;
  RCX = 1;
  RDX = 2;
  RSP = 4;
  RBP = 5;
  RSI = 6;
  RDI = 7;

TYPE
  Argument*     = POINTER TO ArgumentDesc;
  ConditionArg* = POINTER TO ConditionArgDesc;
(*
  JumpArg*      = POINTER TO JumpArgDesc;

  Pseudocode*   = POINTER TO PseudoDesc;
  Label*        = POINTER TO LabelDesc;
  Jump*         = POINTER TO JumpDesc;
  Expression*   = POINTER TO ExprDesc;
*)

  (* Object classes                                             *)
  (*                                                            *)
  (*  class    val                            ext        modno  *)
  (*  -------  -----------------------------  ---------  -----  *)
  (*  Const    value                                            *)
  (*  String   code address                   len               *)
  (*  Typ      type descriptor address/impno             modno  *)
  (*  Proc     code address/impno                        modno  *)
  (*  GlobVar  data address/impno                        modno  *)
  (*  ProcVar  index                                            *)
  (*  ValPar   index                                            *)
  (*  RefPar   index                                            *)
  (*  Field    offset                                           *)
  (*  StdProc  inline code number                               *)
  (*  StdFunc  inline code number                               *)
  (*  Mod      module key                                       *)
  (*  Cond     condition code                                   *)
  (*  RegVal   regno                                            *)
  (*  RegRef   regno                          2nd-reg^          *)
  (*                                                            *)
  (* ^2nd-reg: for record and open array parameters             *)

  ArgumentDesc* = RECORD (OXB.ObjCoreDesc)
    op:    INTEGER;  (* op num that generated this argument, if any *)
    x:     Argument; (* first operand to op, if any *)
    y:     Argument; (* second operand to op, if any *)
    needs: SET;      (* registers that are unavoidably affected by evaluation *)
  END;

  ConditionArgDesc = RECORD (ArgumentDesc)  (* Condition code in .val *)
    falsechain*: INTEGER;  (* Jump *)
    truechain*:  INTEGER;  (* Jump *)
(*
  END;

  JumpArgDesc = RECORD (ArgumentDesc)
*)
    condition:   INTEGER;  (* -1 iff unconditional *)
    target:      INTEGER;
    also:        INTEGER;  (* Previous jump to be assigned the same target as this one *)
  END;


VAR
  W:     Texts.Writer;
  Hdr*:  Modules.ModDesc;
  Modid: ORS.Ident;

  ArgConst0: Argument;
  ArgConst1: Argument;

  Strings:        ARRAY 500 OF INTEGER;  (* Saved string addresses for duplicate avoidance *)
  StrCnt:         INTEGER;
  SavedStringDup: INTEGER;
  DupStrings:     INTEGER;
  StringBytes:    INTEGER;

  Varsize*: INTEGER;    (* Module global VAR size *)
  VarFix:   ARRAY 1024 OF SYSTEM.CARD32;
  VarFInx:  INTEGER;

  Text:     ARRAY TextSize OF BYTE;
  TextAdr:  INTEGER;

  ImpFix:   ARRAY 2048 OF RECORD offset, fix: SYSTEM.CARD32 END;
  ImpFInx:  INTEGER;

  Operations: ARRAY 100 OF Argument;
  OpCount:    INTEGER;


(* -------------------------------- Tracing --------------------------------- *)

PROCEDURE wn*; BEGIN Texts.WriteLn(W); Texts.Append(Texts.Log, W.buf) END wn;
PROCEDURE wc (c: CHAR);          BEGIN Texts.Write(W, c)           END wc;
PROCEDURE ws (s: ARRAY OF CHAR); BEGIN Texts.WriteString(W, s)     END ws;
PROCEDURE wsn(s: ARRAY OF CHAR); BEGIN Texts.WriteString(W, s); wn END wsn;
PROCEDURE wi (i: INTEGER);       BEGIN Texts.WriteInt(W, i, 1)     END wi;
PROCEDURE wir(i, n: INTEGER);    BEGIN Texts.WriteInt(W, i, n)     END wir;

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
  IF    form = OXB.Byte    THEN wsl("Byte",    w, " ")
  ELSIF form = OXB.Bool    THEN wsl("Bool",    w, " ")
  ELSIF form = OXB.Char    THEN wsl("Char",    w, " ")
  ELSIF form = OXB.Int8    THEN wsl("Int8",    w, " ")
  ELSIF form = OXB.Int16   THEN wsl("Int16",   w, " ")
  ELSIF form = OXB.Int32   THEN wsl("Int32",   w, " ")
  ELSIF form = OXB.Int64   THEN wsl("Int64",   w, " ")
  ELSIF form = OXB.Card16  THEN wsl("Card16",  w, " ")
  ELSIF form = OXB.Card32  THEN wsl("Card32",  w, " ")
  ELSIF form = OXB.Real    THEN wsl("Real",    w, " ")
  ELSIF form = OXB.Set     THEN wsl("Set",     w, " ")
  ELSIF form = OXB.Pointer THEN wsl("Pointer", w, " ")
  ELSIF form = OXB.NilTyp  THEN wsl("NilTyp",  w, " ")
  ELSIF form = OXB.NoTyp   THEN wsl("NoTyp",   w, " ")
  ELSIF form = OXB.ProcAdr THEN wsl("ProcAdr", w, " ")
  ELSIF form = OXB.StrAdr  THEN wsl("StrAdr",  w, " ")
  ELSIF form = OXB.Array   THEN wsl("Array",   w, " ")
  ELSIF form = OXB.Record  THEN wsl("Record",  w, " ")
  ELSE ws("<"); wi(form); ws(">")
  END
END wForm;

PROCEDURE wCond(cond: INTEGER);
BEGIN
  IF    cond = CO  THEN ws("jo")
  ELSIF cond = CNO THEN ws("jno")
  ELSIF cond = CC  THEN ws("jb")
  ELSIF cond = CNC THEN ws("jae")
  ELSIF cond = CZ  THEN ws("je")
  ELSIF cond = CNZ THEN ws("jne")
  ELSIF cond = CNA THEN ws("jbe")
  ELSIF cond = CA  THEN ws("ja")
  ELSIF cond = CS  THEN ws("js")
  ELSIF cond = CNS THEN ws("jns")
  ELSIF cond = CP  THEN ws("jpe")
  ELSIF cond = CNP THEN ws("jpo")
  ELSIF cond = CL  THEN ws("jl")
  ELSIF cond = CNL THEN ws("jge")
  ELSIF cond = CNG THEN ws("jle")
  ELSIF cond = CG  THEN ws("jg")
  ELSE ws("<"); wi(cond); ws(">")
  END
END wCond;

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

PROCEDURE wAdr(x: Argument);
BEGIN
  ASSERT(x.class IN {GlobVar, Proc, String});
  IF x.modno > 0 THEN
    ws("["); wi(x.modno); ws("]."); wi(x.val)
  ELSE
    wh(x.val); ws("H")
  END
END wAdr;

PROCEDURE wConst(x: Argument);
VAR i, l: INTEGER;
BEGIN
  ASSERT(x.class IN {Const, Proc, String});
  IF    x.class = Proc   THEN wAdr(x)
  ELSIF x.class = String THEN wAdr(x);
                              ws(" '"); i := x.val;  l := i + x.ext - 1;
                              WHILE i < l DO wc(CHR(Text[i]));  INC(i) END;
                              wc("'")
  ELSIF x.class = Const  THEN IF (x.val >= 0) & (x.val <= 9)
                              THEN wi(x.val)
                              ELSE wh(x.val); ws("H ") END  END
END wConst;

PROCEDURE wOp(op, w: INTEGER);
BEGIN
  IF    op = OpEnter            THEN wsl("Enter",        w, " ")
  ELSIF op = OpCallProc         THEN wsl("CallProc",     w, " ")
  ELSIF op = OpCallFn           THEN wsl("CallFn",       w, " ")
  ELSIF op = OpReturn           THEN wsl("Return",       w, " ")
  ELSIF op = OpAbs              THEN wsl("Abs",          w, " ")
  ELSIF op = OpOdd              THEN wsl("Odd",          w, " ")
  ELSIF op = OpFloor            THEN wsl("Floor",        w, " ")
  ELSIF op = OpFloat            THEN wsl("Float",        w, " ")
  ELSIF op = OpOrd              THEN wsl("Ord",          w, " ")
  ELSIF op = OpLen              THEN wsl("Len",          w, " ")
  ELSIF op = OpREG              THEN wsl("REG",          w, " ")
  ELSIF op = OpAdr              THEN wsl("Adr",          w, " ")
  ELSIF op = OpCond             THEN wsl("Cond",         w, " ")
  ELSIF op = OpNew              THEN wsl("New",          w, " ")
  ELSIF op = OpLed              THEN wsl("Led",          w, " ")
  ELSIF op = OpSetSingleton     THEN wsl("SetSingleton", w, " ")
  ELSIF op = OpInvert           THEN wsl("Invert",       w, " ")
  ELSIF op = OpNegate           THEN wsl("Negate",       w, " ")
  ELSIF op = OpStore            THEN wsl("Store",        w, " ")
  ELSIF op = OpStoreStruc       THEN wsl("StoreStruc",   w, " ")
  ELSIF op = OpCopyString       THEN wsl("CopyString",   w, " ")
  ELSIF op = OpLSL              THEN wsl("LSL",          w, " ")
  ELSIF op = OpASR              THEN wsl("ASR",          w, " ")
  ELSIF op = OpROR              THEN wsl("ROR",          w, " ")
  ELSIF op = OpADC              THEN wsl("ADC",          w, " ")
  ELSIF op = OpSBC              THEN wsl("SBC",          w, " ")
  ELSIF op = OpUML              THEN wsl("UML",          w, " ")
  ELSIF op = OpBit              THEN wsl("Bit",          w, " ")
  ELSIF op = OpInc              THEN wsl("Inc",          w, " ")
  ELSIF op = OpDec              THEN wsl("Dec",          w, " ")
  ELSIF op = OpPack             THEN wsl("Pack",         w, " ")
  ELSIF op = OpUnpack           THEN wsl("Unpack",       w, " ")
  ELSIF op = OpGet              THEN wsl("Get",          w, " ")
  ELSIF op = OpPut              THEN wsl("Put",          w, " ")
  ELSIF op = OpLoadReg          THEN wsl("LoadReg",      w, " ")
  ELSIF op = OpSetRange         THEN wsl("MakeSetRange", w, " ")
  ELSIF op = OpIn               THEN wsl("In",           w, " ")
  ELSIF op = OpSetOr            THEN wsl("SetOr",        w, " ")
  ELSIF op = OpSetAndNot        THEN wsl("SetAndNot",    w, " ")
  ELSIF op = OpSetAnd           THEN wsl("SetAnd",       w, " ")
  ELSIF op = OpSetXor           THEN wsl("SetXor",       w, " ")
  ELSIF op = OpSetIncl          THEN wsl("SetIncl",      w, " ")
  ELSIF op = OpSetExcl          THEN wsl("SetExcl",      w, " ")
  ELSIF op = OpIntEql           THEN wsl("IntEql",       w, " ")
  ELSIF op = OpIntNeq           THEN wsl("IntNeq",       w, " ")
  ELSIF op = OpIntLss           THEN wsl("IntLss",       w, " ")
  ELSIF op = OpIntLeq           THEN wsl("IntLeq",       w, " ")
  ELSIF op = OpIntGtr           THEN wsl("IntGtr",       w, " ")
  ELSIF op = OpIntGeq           THEN wsl("IntGeq",       w, " ")
  ELSIF op = OpRealEql          THEN wsl("RealEql",      w, " ")
  ELSIF op = OpRealNeq          THEN wsl("RealNeq",      w, " ")
  ELSIF op = OpRealLss          THEN wsl("RealLss",      w, " ")
  ELSIF op = OpRealLeq          THEN wsl("RealLeq",      w, " ")
  ELSIF op = OpRealGtr          THEN wsl("RealGtr",      w, " ")
  ELSIF op = OpRealGeq          THEN wsl("RealGeq",      w, " ")
  ELSIF op = OpStringEql        THEN wsl("StringEql",    w, " ")
  ELSIF op = OpStringNeq        THEN wsl("StringNeq",    w, " ")
  ELSIF op = OpStringLss        THEN wsl("StringLss",    w, " ")
  ELSIF op = OpStringLeq        THEN wsl("StringLeq",    w, " ")
  ELSIF op = OpStringGtr        THEN wsl("StringGtr",    w, " ")
  ELSIF op = OpStringGeq        THEN wsl("StringGeq",    w, " ")
  ELSIF op = OpSetCC            THEN wsl("SetCC",        w, " ")
  ELSIF op = OpIntPlus          THEN wsl("IntPlus",      w, " ")
  ELSIF op = OpIntMinus         THEN wsl("IntMinus",     w, " ")
  ELSIF op = OpIntMultiply      THEN wsl("IntMultiply",  w, " ")
  ELSIF op = OpIntDivide        THEN wsl("IntDivide",    w, " ")
  ELSIF op = OpIntModulus       THEN wsl("IntModulus",   w, " ")
  ELSIF op = OpRealPlus         THEN wsl("RealPlus",     w, " ")
  ELSIF op = OpRealMinus        THEN wsl("RealMinus",    w, " ")
  ELSIF op = OpRealMultiply     THEN wsl("RealMultiply", w, " ")
  ELSIF op = OpRealDivide       THEN wsl("RealDivide",   w, " ")
  ELSIF op = OpIndex            THEN wsl("Index",        w, " ")
  ELSIF op = OpField            THEN wsl("Field",        w, " ")
  ELSIF op = OpValueParam       THEN wsl("ValueParam",   w, " ")
  ELSIF op = OpOpenArrParam     THEN wsl("OpenArrParam", w, " ")
  ELSIF op = OpByteArrParam     THEN wsl("ByteArrParam", w, " ")
  ELSIF op = OpVarParam         THEN wsl("VarParam",     w, " ")
  ELSIF op = OpStringParam      THEN wsl("StringParam",  w, " ")
  ELSIF op = OpTestNZ           THEN wsl("TestNZ",       w, " ")
  ELSIF op = OpLoad             THEN wsl("Load",         w, " ")

  ELSE ws("<"); wi(op); ws(">")
  END
END wOp;

PROCEDURE wOpSym(op: INTEGER);
BEGIN
  IF    op = OpAbs              THEN ws("ABS")
  ELSIF op = OpOdd              THEN ws("ODD")
  ELSIF op = OpFloor            THEN ws("FLOOR")
  ELSIF op = OpFloat            THEN ws("FLOAT")
  ELSIF op = OpOrd              THEN ws("ORD")
  ELSIF op = OpLen              THEN ws("LEN")
  ELSIF op = OpREG              THEN ws("REG")
  ELSIF op = OpAdr              THEN ws("ADR")
  ELSIF op = OpCond             THEN ws("COND")
  ELSIF op = OpSetSingleton     THEN ws("SetSingleton")
  ELSIF op = OpInvert           THEN ws("Invert")
  ELSIF op = OpNegate           THEN ws("Negate")
  ELSIF op = OpLSL              THEN ws("LSL")
  ELSIF op = OpASR              THEN ws("ASR")
  ELSIF op = OpROR              THEN ws("ROR")
  ELSIF op = OpADC              THEN ws("ADC")
  ELSIF op = OpSBC              THEN ws("SBC")
  ELSIF op = OpUML              THEN ws("UML")
  ELSIF op = OpBit              THEN ws("Bit")
  ELSIF op = OpPack             THEN ws("Pack")
  ELSIF op = OpUnpack           THEN ws("Unpack")
  ELSIF op = OpGet              THEN ws("Get")
  ELSIF op = OpPut              THEN ws("Put")
  ELSIF op = OpLoadReg          THEN ws("LoadReg")
  ELSIF op = OpSetRange         THEN ws("MakeSetRange")
  ELSIF op = OpIn               THEN ws("IN")
  ELSIF op = OpSetOr            THEN ws("OR")
  ELSIF op = OpSetAndNot        THEN ws("ANDNOT")
  ELSIF op = OpSetAnd           THEN ws("AND")
  ELSIF op = OpSetXor           THEN ws("XOR")
  ELSIF op = OpSetIncl          THEN ws("INCL")
  ELSIF op = OpSetExcl          THEN ws("EXCL")
  ELSIF op = OpIntEql           THEN ws("=")
  ELSIF op = OpIntNeq           THEN ws("#")
  ELSIF op = OpIntLss           THEN ws("<")
  ELSIF op = OpIntLeq           THEN ws("<=")
  ELSIF op = OpIntGtr           THEN ws(">")
  ELSIF op = OpIntGeq           THEN ws(">=")
  ELSIF op = OpRealEql          THEN ws("R=")
  ELSIF op = OpRealNeq          THEN ws("R#")
  ELSIF op = OpRealLss          THEN ws("R<")
  ELSIF op = OpRealLeq          THEN ws("R<=")
  ELSIF op = OpRealGtr          THEN ws("R>")
  ELSIF op = OpRealGeq          THEN ws("R>=")
  ELSIF op = OpStringEql        THEN ws("S=")
  ELSIF op = OpStringNeq        THEN ws("S#")
  ELSIF op = OpStringLss        THEN ws("S<")
  ELSIF op = OpStringLeq        THEN ws("S<=")
  ELSIF op = OpStringGtr        THEN ws("S>")
  ELSIF op = OpStringGeq        THEN ws("S>=")
  ELSIF op = OpIntPlus          THEN ws("+")
  ELSIF op = OpIntMinus         THEN ws("-")
  ELSIF op = OpIntMultiply      THEN ws("*")
  ELSIF op = OpIntDivide        THEN ws("DIV")
  ELSIF op = OpIntModulus       THEN ws("MOD")
  ELSIF op = OpRealPlus         THEN ws("R+")
  ELSIF op = OpRealMinus        THEN ws("R-")
  ELSIF op = OpRealMultiply     THEN ws("R*")
  ELSIF op = OpRealDivide       THEN ws("R/")
  ELSIF op = OpIndex            THEN ws("Index")
  ELSIF op = OpField            THEN ws("Field")
  ELSIF op = OpTestNZ           THEN ws("TestNZ")

  ELSIF op = OpStore            THEN ws("<Store>")
  ELSIF op = OpAnd              THEN ws("<And>")
  ELSIF op = OpOr               THEN ws("<Or>")
  ELSIF op = OpNot              THEN ws("<Not>")
  ELSIF op = OpSetCC            THEN ws("<SetCC>")


  ELSE ws("<"); wi(op); ws(">")
  END
END wOpSym;


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
  IF    c = Head     THEN ws("Head ")
  ELSIF c = Const    THEN ws("Const ")
  ELSIF c = String   THEN ws("String ")
  ELSIF c = Typ      THEN ws("Typ ")
  ELSIF c = Proc     THEN ws("Proc ")
  ELSIF c = GlobVar  THEN ws("glob")
  ELSIF c = ProcVar  THEN ws("local")
  ELSIF c = ValPar   THEN ws("ValPar ")
  ELSIF c = RefPar   THEN ws("RefPar ")
  ELSIF c = Field    THEN ws("Field ")
  ELSIF c = StdProc  THEN ws("StdProc ")
  ELSIF c = StdFunc  THEN ws("StdFunc ")
  ELSIF c = Mod      THEN ws("Module ")
  ELSIF c = Cond     THEN ws("Cond ")
  ELSIF c = RegVal   THEN ws("RegVal")
  ELSIF c = RegRef   THEN ws("RegRef")
  ELSIF c = Expr     THEN ws("Expr ")
  ELSIF c = Jump     THEN ws("Jump ")
  ELSE                    ws("<"); wi(c); ws(">")
  END;
END wClass;

PROCEDURE wArgument*(x: Argument);
BEGIN
  IF x.name # "" THEN ws(x.name);  ws("(") END;
  IF    x.class = Head     THEN ws("Head ")
  ELSIF x.class = Const    THEN                   wConst(x)
  ELSIF x.class = String   THEN ws("String ");    wAdr(x)
  ELSIF x.class = Proc     THEN ws("Proc ");      wAdr(x)
  ELSIF x.class = GlobVar  THEN ws("g");          wAdr(x)
  ELSIF x.class = ProcVar  THEN ws("l");          wi(x.val)
  ELSIF x.class = ValPar   THEN ws("ValPar ");    wi(x.val)
  ELSIF x.class = RefPar   THEN ws("RefPar ");    wi(x.val)
  ELSIF x.class = Field    THEN ws("Field ");     wi(x.val)
  ELSIF x.class = Typ      THEN ws("Typ ");       ws(x.type.typobj.name)
  ELSIF x.class = StdProc  THEN ws("StdProc ");   wi(x.val)
  ELSIF x.class = StdFunc  THEN ws("StdFunc ");   wi(x.val)
  ELSIF x.class = Mod      THEN ws("Mod ");       ws(x.name)
  ELSIF x.class = Cond     THEN ws("Cond ");      wCond(x.val)
  ELSIF x.class = RegVal   THEN                   wReg(x.val)
  ELSIF x.class = RegRef   THEN ws("@");          wReg(x.val)
  ELSIF x.class = Expr     THEN ws("Expr ");      wOp(x.op, 1)
  ELSE
    ws("<"); wi(x.class); ws(", op "); wOp(x.op, 1); ws(">")
  END;
  IF x.name # "" THEN ws(")") END;
  (*IF x.op # 0 THEN wc("(");  wOp(x.op, 1);  ws(")") END;*)
END wArgument;


(* ------------------- Body analysis and code generation -------------------- *)

PROCEDURE Here*(): INTEGER;
BEGIN
  wsn("Here.");  (* TODO - return procedure entry address *)
RETURN TextAdr END Here;

PROCEDURE IsSingleCharacterString*(x: Argument): BOOLEAN;
RETURN (x.class = String) & (x.ext = 2) END IsSingleCharacterString;

PROCEDURE DeRef*(VAR x: Argument);
BEGIN ASSERT(FALSE)
END DeRef;


(* -------------------- Operation argument construction --------------------- *)

PROCEDURE OpnumToIntCondition(opnum: INTEGER; type: OXB.Type): INTEGER;
VAR result: INTEGER;  signed: BOOLEAN;
BEGIN
  signed := type.ref IN {OXB.Int8, OXB.Int16, OXB.Int32, OXB.Int64};
  IF (opnum < OpIntEql) OR (opnum > OpStringGeq) THEN
    H.ws("opnum "); H.wi(opnum); H.wn;
    H.Trap(16, "opnum out of range in OpnumToIntCondition")
  END;
  ASSERT((opnum >= OpIntEql) & (opnum <= OpStringGeq));
  opnum := OpIntEql + (opnum - OpIntEql) MOD 6;
  IF    opnum = OpIntEql THEN result := CZ
  ELSIF opnum = OpIntNeq THEN result := CNZ
  ELSIF opnum = OpIntLss THEN IF signed THEN result := CL  ELSE result := CC  END
  ELSIF opnum = OpIntGeq THEN IF signed THEN result := CNL ELSE result := CNC END
  ELSIF opnum = OpIntLeq THEN IF signed THEN result := CNG ELSE result := CNA END
  ELSIF opnum = OpIntGtr THEN IF signed THEN result := CG  ELSE result := CA  END
  ELSE ASSERT(FALSE)
  END
RETURN result END OpnumToIntCondition;

PROCEDURE NewArg*(class, val, ext, modno: INTEGER; type: OXB.Type): Argument;
VAR  arg: Argument;  obj: OXB.ObjCore;
BEGIN
  NEW(arg);
  arg.class := class;  arg.val   := val;   arg.ext := ext;
  arg.modno := modno;  arg.type  := type;
  arg.name  := "";     arg.rdo   := FALSE;
  arg.op    := 0;      arg.needs := {};
RETURN arg END NewArg;

PROCEDURE NewOpArg(op, class: INTEGER;  x, y: Argument;  type: OXB.Type): Argument;
VAR arg: Argument;
BEGIN
  ws("NewOpArg "); wOpSym(op);  ws(", class: ");  wClass(class);
  IF x # NIL THEN ws(", x: ");  wArgument(x) END;
  IF y # NIL THEN ws(", y: ");  wArgument(y) END;
  wn;

  arg := NewArg(class, -1, 0, 0, type);
  arg.op := op;  arg.x := x;  arg.y := y;  arg.needs := {};
  IF (x # NIL) & (x.op # 0) THEN arg.needs := arg.needs + x.needs END;
  IF (y # NIL) & (y.op # 0) THEN arg.needs := arg.needs + y.needs END;
RETURN arg END NewOpArg;

PROCEDURE NewConditionArg(op: INTEGER; x, y: Argument): Argument;
VAR arg: ConditionArg;
BEGIN
  ws("NewConditionArg "); wOpSym(op);
  IF x # NIL THEN ws(", x: ");  wArgument(x) END;
  IF y # NIL THEN ws(", y: ");  wArgument(y) END;
  wn;

  NEW(arg);  arg.class := RegVal;  arg.type := OXB.boolType;
  arg.op := op;   arg.x := x;  arg.y := y;  arg.needs := {};
  arg.val := -1;
  IF (op >= OpIntEql) & (op <= OpStringGeq) THEN
    arg.ext := OpnumToIntCondition(op, x.type)
  ELSE
    arg.ext := -1;
  END;
  IF (x # NIL) & (x.op # 0) THEN arg.needs := arg.needs + x.needs END;
  IF (y # NIL) & (y.op # 0) THEN arg.needs := arg.needs + y.needs END;
  arg.falsechain := -1;  arg.truechain := -1
RETURN arg END NewConditionArg;

PROCEDURE MakeArgument*(x: OXB.Object; curlev: INTEGER): Argument;
VAR modno: INTEGER;  arg: Argument;
BEGIN
  ASSERT(x.class IN {Const, String, Proc, GlobVar, ProcVar, ValPar, RefPar, Typ, Field});
  arg := NewArg(x.class, x.val, x.ext, x.modno, x.type);  arg.name := x.name;
  IF ~(x.class IN {Const, String, Proc, ValPar, RefPar}) THEN
    IF (x.modno < 0) & (x.modno # -curlev) THEN ORS.Mark("not accessible") END
  END;
  IF x.class = String THEN ASSERT(x.rdo) END;
RETURN arg END MakeArgument;

PROCEDURE MakeRealArgument*(val: REAL): Argument;
RETURN NewArg(Const, ORD(val), 0, 0, OXB.realType) END MakeRealArgument;

PROCEDURE MakeStringArgument*(len: INTEGER): Argument; (* copies string from ORS-buffer *)
VAR i, j, adr: INTEGER; c1, c2: BYTE;  arg: Argument;
BEGIN
  (* Check whether string already made *)
  i := 0;  j := 0;
  WHILE i < StrCnt DO
    adr := Strings[i];  j := 0;
    WHILE (j < len) & (Text[adr + j] = ORD(ORS.str[j])) DO INC(j) END;
    IF j = len THEN i := StrCnt ELSE INC(i) END
  END;
  IF j = len THEN
    INC(SavedStringDup, len);
    INC(DupStrings);
  ELSE  (* Copy string into text section *)
    adr := TextAdr;
    FOR i := 0 TO len - 1 DO
      Text[TextAdr] := ORD(ORS.str[i]);  INC(TextAdr)
    END;
    IF StrCnt >= LEN(Strings) THEN
      ORS.Mark("Too many strings");
      wn;
      wsn("String table overflow. Strings:");
      FOR i := 0 TO LEN(Strings) - 1 DO
        wir(i, 4); ws(": '");
        j := Strings[i];
        WHILE Text[j] # 0 DO wc(CHR(Text[j]));  INC(j) END;
        wsn("'");
      END
    ELSE
      Strings[StrCnt] := adr;  INC(StrCnt)
    END;
    INC(StringBytes, len)
  END;
  arg := NewArg(String, adr, len, 0, OXB.strType);  arg.rdo := TRUE
RETURN arg END MakeStringArgument;


(* ---- Item conversion ---- *)

PROCEDURE StrToChar*(VAR x: Argument);
BEGIN
  (*ws("StrToChar(");  wi(x.unique); wsn(")");*)
  ASSERT(x.class = String);
  ASSERT(x.rdo);
  x := NewArg(Const, Text[x.val], 0, 0, OXB.charType)
END StrToChar;


(* ------------------------------- Operations ------------------------------- *)


(* ---------------------------- Labels and jumps ---------------------------- *)

PROCEDURE JumpForward(jump: ConditionArg; VAR chain: INTEGER; cond: INTEGER);
BEGIN
  Operations[OpCount] := jump;
  jump.class     := Jump;
  jump.condition := cond;
  jump.target    := -1;
  jump.also      := chain;
  chain          := OpCount;
  INC(OpCount)
END JumpForward;

PROCEDURE JumpBack(cond, target: INTEGER);
VAR jump: ConditionArg;
BEGIN
  NEW(jump);  Operations[OpCount] := jump;
  jump.condition := cond;
  jump.target    := target;
  jump.also      := -1;
  INC(OpCount)
END JumpBack;


PROCEDURE FixChain(VAR l, target: INTEGER); (*: Jump; target: Label);*)
VAR j: ConditionArg;
BEGIN
  (*
  WHILE l # NIL DO l.target := target; l := l.also END;
  l := NIL
  *)
  WHILE l >= 0 DO j := Operations[l](ConditionArg);  j.target := target;  l := j.also END;
  l := -1
END FixChain;

PROCEDURE FixLink*(VAR l: INTEGER);  (*Jump);*)
BEGIN
  (*
  IF l # NIL THEN
    NEW(target);
    Ops.next := target;  target.prev := Ops;  Ops := target;
    target.addr := OpAddr;
    FixChain(l, target)
  END
  *)
  IF l >= 0 THEN
    Operations[OpCount] := NIL;
    FixChain(l, OpCount);
    INC(OpCount)
  END
END FixLink;


PROCEDURE LoadFromCC(VAR x: Argument);
(*            jxx    tchain
    fchain:   xor    eax,eax
              jmp    done
    tchain:   mov    eax,1
    done:                      *)
VAR
  done: INTEGER; (*Jump;*)
  cx:   ConditionArg;
BEGIN
  ASSERT(x # NIL);
  done := -1;
  cx   := x(ConditionArg);
  IF (cx.truechain < 0) & (cx.falsechain < 0) THEN
    (* Can use X86 SetCC instruction *)
    x := NewOpArg(OpSetCC, Expr, x, NIL, OXB.boolType)
  ELSE
    JumpForward(cx, cx.truechain, cx.val);

    FixLink(cx.falsechain);
    (*
    MakeOp(OpLoad, ArgConst0, NIL);
    *)
    JumpForward(cx, done, -1);

    FixLink(cx.truechain);
    (*
    MakeOp(OpLoad, ArgConst1, NIL);
    *)
    FixLink(done);

(* TODO - needs thinking though with OpArgs - not yet done *)

    x := NewArg(Head, 0(*op.result*), 0, 0, OXB.intType)
  END



  (*
  cx := x(ConditionArg);
  IF (cx.truechain = NIL) & (cx.falsechain = NIL) THEN
    (* Can use X86 SetCC instruction *)
    x := NewOpArg(OpSetCC, x, NIL, OXB.boolType)
  ELSE
    JumpForward(cx.truechain, cx.val);

    FixLink(cx.falsechain);
    (*
    MakeOp(OpLoad, ArgConst0, NIL);
    *)
    JumpForward(done, -1);

    FixLink(cx.truechain);
    (*
    MakeOp(OpLoad, ArgConst1, NIL);
    *)
    FixLink(done);

(* TODO - needs thinking though with OpArgs - not yet done *)

    x := NewArg(Head, 0(*op.result*), 0, 0, OXB.intType)
  END
  *)
END LoadFromCC;


PROCEDURE Enter*(x: Argument);
BEGIN
  ws("Enter '");  IF x # NIL THEN ws(x.name) ELSE ws("module body") END;  wsn("'.");
  (*
  MakeOp(OpEnter, x, NIL);
  *)
  OpCount  := 0
END Enter;


PROCEDURE Operation*(opnum: INTEGER; x, y: Argument): Argument;
VAR
  type:       OXB.Type;
  xval, yval: INTEGER;
  result:     Argument;
BEGIN
  (*
  ws("Operation "); wOpSym(opnum);
  IF x # NIL THEN ws(", x: ");  wArgument(x) END;
  IF y # NIL THEN ws(", y: ");  wArgument(y) END;
  wn;
  *)

  ASSERT(opnum # OpEnter);        (* call OXG.Enter directly *)
  ASSERT(opnum # OpStore);        (* call OXG.Store directly *)
  ASSERT(opnum # OpReturn);       (* call OXG.Return directly *)
  ASSERT(opnum # OpValueParam);   (* call OXG.ValueParam directly *)
  ASSERT(opnum # OpCallProc);     (* call OXG.Call directly *)
  ASSERT(opnum # OpCallFn);       (* call OXG.Call directly *)

  IF (x # NIL) & (y = NIL) & (x.class = Const) & (opnum >= OpAbs) & (opnum <= OpInvert) THEN
    (* Monadic with contant parameter *)
    IF    opnum = OpAbs          THEN x.val := ABS(x.val)
    ELSIF opnum = OpSetSingleton THEN x.val := LSL(1, x.val)
    ELSIF opnum = OpNegate       THEN x.val := - x.val
    ELSIF opnum = OpRealNegate   THEN ASSERT(FALSE) (*x.val := ORD(-SYSTEM.VAL(REAL, x.val))*)
    ELSIF opnum = OpInvert       THEN x.val := ORD(-SYSTEM.VAL(SET, x.val))
    END
  ELSIF (x # NIL) & (y # NIL)
      & (x.class = Const) & (y.class = Const)
      & (   (opnum >= OpLSL)    & (opnum <= OpRealDivide)
         OR (opnum >= OpIntEql) & (opnum <= OpRealGeq)) THEN
    type := x.type;  xval := x.val;  yval := y.val;
    IF    opnum = OpLSL          THEN x := NewArg(Const, LSL(xval, yval), 0, 0, type)
    ELSIF opnum = OpASR          THEN x := NewArg(Const, ASR(xval, yval), 0, 0, type)
    ELSIF opnum = OpROR          THEN x := NewArg(Const, ROR(xval, yval), 0, 0, type)
    ELSIF opnum = OpADC          THEN ASSERT(FALSE) (*x := MakeArgConst(type, xval yval)*)
    ELSIF opnum = OpSBC          THEN ASSERT(FALSE) (*x := MakeArgConst(type, xval yval)*)
    ELSIF opnum = OpUML          THEN ASSERT(FALSE) (*x := MakeArgConst(type, xval yval)*)
    ELSIF opnum = OpBit          THEN ASSERT(FALSE) (*x := MakeArgConst(type, SYSTEM.BIT(xval, yval))*)
    ELSIF opnum = OpSetRange     THEN x := NewArg(Const, ORD({xval..yval}), 0, 0, type)
    ELSIF opnum = OpSetOr        THEN x := NewArg(Const, ORD(SYSTEM.VAL(SET,xval) + SYSTEM.VAL(SET,yval)), 0, 0, type)
    ELSIF opnum = OpSetAndNot    THEN x := NewArg(Const, ORD(SYSTEM.VAL(SET,xval) - SYSTEM.VAL(SET,yval)), 0, 0, type)
    ELSIF opnum = OpSetAnd       THEN x := NewArg(Const, ORD(SYSTEM.VAL(SET,xval) * SYSTEM.VAL(SET,yval)), 0, 0, type)
    ELSIF opnum = OpSetXor       THEN x := NewArg(Const, ORD(SYSTEM.VAL(SET,xval) / SYSTEM.VAL(SET,yval)), 0, 0, type)
    ELSIF opnum = OpIntPlus      THEN x := NewArg(Const, xval + yval, 0, 0, type)
    ELSIF opnum = OpIntMinus     THEN x := NewArg(Const, xval - yval, 0, 0, type)
    ELSIF opnum = OpIntMultiply  THEN x := NewArg(Const, xval * yval, 0, 0, type)
    ELSIF opnum = OpIntDivide    THEN x := NewArg(Const, xval DIV yval, 0, 0, type)
    ELSIF opnum = OpIntModulus   THEN x := NewArg(Const, xval MOD yval, 0, 0, type)
    ELSIF opnum = OpRealPlus     THEN ASSERT(FALSE) (*x := MakeArgConst(type, xval yval)*)
    ELSIF opnum = OpRealMinus    THEN ASSERT(FALSE) (*x := MakeArgConst(type, xval yval)*)
    ELSIF opnum = OpRealMultiply THEN ASSERT(FALSE) (*x := MakeArgConst(type, xval yval)*)
    ELSIF opnum = OpRealDivide   THEN ASSERT(FALSE) (*x := MakeArgConst(type, xval yval)*)
    ELSIF opnum = OpIntEql       THEN x := NewArg(Const, ORD(xval =  yval), 0, 0, OXB.boolType)
    ELSIF opnum = OpIntNeq       THEN x := NewArg(Const, ORD(xval #  yval), 0, 0, OXB.boolType)
    ELSIF opnum = OpIntLss       THEN x := NewArg(Const, ORD(xval <  yval), 0, 0, OXB.boolType)
    ELSIF opnum = OpIntLeq       THEN x := NewArg(Const, ORD(xval <= yval), 0, 0, OXB.boolType)
    ELSIF opnum = OpIntGtr       THEN x := NewArg(Const, ORD(xval >  yval), 0, 0, OXB.boolType)
    ELSIF opnum = OpIntGeq       THEN x := NewArg(Const, ORD(xval >= yval), 0, 0, OXB.boolType)
    (*
    ELSIF opnum = OpRealEql      THEN x := NewArg(Const, ORD(SYSTEM.VAL(REAL, xval) =  SYSTEM.VAL(REAL, yval)), 0, 0, OXB.boolType)
    ELSIF opnum = OpRealNeq      THEN x := NewArg(Const, ORD(SYSTEM.VAL(REAL, xval) #  SYSTEM.VAL(REAL, yval)), 0, 0, OXB.boolType)
    ELSIF opnum = OpRealLss      THEN x := NewArg(Const, ORD(SYSTEM.VAL(REAL, xval) <  SYSTEM.VAL(REAL, yval)), 0, 0, OXB.boolType)
    ELSIF opnum = OpRealLeq      THEN x := NewArg(Const, ORD(SYSTEM.VAL(REAL, xval) <= SYSTEM.VAL(REAL, yval)), 0, 0, OXB.boolType)
    ELSIF opnum = OpRealGtr      THEN x := NewArg(Const, ORD(SYSTEM.VAL(REAL, xval) >  SYSTEM.VAL(REAL, yval)), 0, 0, OXB.boolType)
    ELSIF opnum = OpRealGeq      THEN x := NewArg(Const, ORD(SYSTEM.VAL(REAL, xval) >= SYSTEM.VAL(REAL, yval)), 0, 0, OXB.boolType)
    *)
    END
  ELSE
    (* At least one parameter is not constant *)
    IF opnum IN {OpValueParam .. OpStringParam} THEN
      ASSERT(y # NIL);
      IF opnum = OpValueParam THEN ASSERT(y.class = ValPar) ELSE ASSERT(y.class = RefPar) END
    END;

    IF (x # NIL)
     & (x IS ConditionArg)
     & ((opnum < OpIntEql) OR (opnum > OpStringGeq)) THEN
      LoadFromCC(x)
    END;

    IF (opnum >= OpAbs) & (opnum <= OpField) THEN
      result := NewOpArg(opnum, RegVal, x, y, x.type);

      (* Numeric monadic and dyadic operators *)

      IF (opnum = OpIntDivide) OR (opnum = OpIntModulus) THEN
        result.needs := result.needs + {RAX, RDX};
        IF opnum = OpIntDivide THEN result.val := RAX ELSE result.val := RDX END;
      END

    ELSIF (opnum >= OpAnd) & (opnum <= OpOr)
       OR (opnum >= OpIntEql) & (opnum <= OpStringGeq) THEN
      result := NewConditionArg(opnum, x, y)

    ELSIF opnum = OpTestNZ THEN
      ASSERT(FALSE) (* result := NewConditionArg(CNZ) *)

    ELSE
      ws("Operation unrecognised: ");  wOp(opnum,1);  wn;
      ASSERT(FALSE)
    END
  END
RETURN result END Operation;


PROCEDURE wArg(x: Argument);
BEGIN
  IF    x.name  # ""     THEN ws(x.name)
  ELSIF x.class = RegVal THEN wReg(x.val)
  ELSIF x.class = RegRef THEN ws("(");  wReg(x.val);  ws(")")
  ELSE wArgument(x)
  END
END wArg;


PROCEDURE WriteExpr(expr: Argument);
BEGIN wtab(5);

  IF expr.class IN {RegVal, RegRef, Jump} THEN
    IF expr.class = Jump THEN
      ws("test");  wtab(7);
    ELSE
      IF expr.class = RegRef THEN ws("(") END;
      wReg(expr.val);
      IF expr.class = RegRef THEN ws(")") END;
      wtab(7);  ws(":=");  wtab(3)
    END;
    ASSERT(expr.x # NIL);
    ASSERT(expr.op # 0);
    IF expr.y = NIL THEN (* Monadic *)
      wOpSym(expr.op);  ws(" ");  wArg(expr.x)
    ELSE
      wArg(expr.x);
      ws(" "); wOpSym(expr.op); ws(" ");
      wArg(expr.y)
    END
  ELSIF expr.op = OpStore THEN
    ASSERT(expr.x # NIL);
    ASSERT(expr.y # NIL);
    wArg(expr.x);
    wtab(7);  ws(":=");  wtab(3);
    wArg(expr.y)
  ELSE
    ws("?? ");
    wOp(expr.op, 1);  wtab(16);
    IF expr.class IN {RegVal, RegRef} THEN
      wReg(expr.val);  wtab(4);  ws(":=");  wtab(3)
    ELSE
      wtab(7)
    END;
    IF expr.x # NIL THEN
      wArgument(expr.x);
      IF expr.y # NIL THEN ws(", ");  (*wtab(26);*)  wArgument(expr.y) END
    END
  END;
  wn
END WriteExpr;


PROCEDURE MoveReg(x: Argument;  reg: INTEGER);
BEGIN
  ASSERT(x.class IN {RegVal, RegRef});
  ASSERT(x.val >= 0);
  IF x.val # reg THEN
    wtab(5);  wReg(reg);
    wtab(7);  ws(":=");
    wtab(3);  wReg(x.val);  wn;
    x.val := reg
  END
END MoveReg;


PROCEDURE NextFreeReg(prefer: INTEGER;  avoid: SET): INTEGER;
VAR r: INTEGER;
BEGIN
  INCL(avoid, RSP);
  IF (prefer < 0) OR (prefer IN avoid) THEN
    r := 0;  WHILE r IN avoid DO INC(r) END;
    IF r > 15 THEN r := -1 END
  ELSE
    r := prefer
  END
RETURN r END NextFreeReg;

PROCEDURE GenerateTerm(expr: Argument;  req: INTEGER;  avoid: SET);
VAR xavoid: SET;  yreq: INTEGER;
BEGIN
  ASSERT(req >= 0);
  ASSERT(expr # NIL);
  ASSERT(expr.op # 0);
  ASSERT(expr.x # NIL);
  ASSERT({req} * avoid = {});
  ASSERT(expr.x.needs * avoid = {});

  (*
  ws("GenerateTerm req: "); wi(req); ws(", avoid: "); wRegSet(avoid);
  ws(", expr: "); wArgument(expr); wn;
  *)

  xavoid := avoid;
  yreq   := -1;

  IF (expr.y # NIL) & (expr.y.op # 0) THEN
    ASSERT(expr.y.needs * avoid = {});
    yreq := NextFreeReg(expr.y.val, avoid + {req});
    xavoid := xavoid + expr.y.needs + {yreq}
  END;

  IF (expr.x # NIL) & (expr.x.op # 0) THEN
    GenerateTerm(expr.x, req, xavoid);
    IF expr.x.val # req THEN
      MoveReg(expr.x, req)
    END
  END;

  IF (expr.y # NIL) & (expr.y.op # 0) THEN
    IF (expr.y.op = OpIntModulus) OR (expr.y.op = OpIntDivide) THEN
      MoveReg(expr.y, RAX)
    END;
    GenerateTerm(expr.y, yreq, avoid)
  END;

  IF    expr.op = OpIntDivide  THEN expr.val := RAX
  ELSIF expr.op = OpIntModulus THEN expr.val := RDX
  ELSE                              expr.val := req
  END;

  WriteExpr(expr)
END GenerateTerm;


PROCEDURE GenerateExpression(addr: INTEGER;  oparg: Argument;  VAR avoid: SET);
VAR n: INTEGER;  xreq, yreq: INTEGER;  xavoid: SET;
BEGIN
  IF oparg = NIL THEN H.Trap(32, "oparg = NIL in GenerateExpression") END;
  ASSERT(oparg # NIL);

  (*
  ws("GenerateExpression addr: "); wi(addr); ws(", avoid: "); wRegSet(avoid);
  ws(", oparg: "); wArgument(oparg); wn;
  *)

  n := 1;
  xavoid := avoid;

  IF (oparg.y # NIL) & (oparg.y.op # 0) THEN
    yreq   := NextFreeReg(oparg.y.val, avoid + oparg.y.needs);
    xavoid := xavoid + oparg.y.needs + {yreq}
  END;

  IF (oparg.x # NIL) & (oparg.x.op # 0) THEN
    IF oparg.op = OpReturn THEN
      xreq := RAX
    ELSE
      xreq := NextFreeReg(oparg.x.val, xavoid)
    END;
    GenerateTerm(oparg.x, xreq, xavoid)
  END;

  IF (oparg.y # NIL) & (oparg.y.op # 0) THEN
    GenerateTerm(oparg.y, yreq, avoid)
  END;

  IF oparg.op = OpStore THEN
    wi(addr);  WriteExpr(oparg)

  ELSIF oparg.op = OpReturn THEN
    IF oparg.x = NIL THEN
      wi(addr);  wtab(5);  wsn("ret")
    ELSE
      wi(addr);  wtab(5);  ws("ret");  wtab(7);
      IF oparg.x.op # 0 THEN ASSERT(oparg.x.class = RegVal);
        wReg(oparg.x.val)
      ELSE
        wArgument(oparg.x)
      END;
      wn
    END

  ELSIF oparg.op = OpValueParam THEN  ASSERT(oparg.x # NIL);
    wi(addr);  wtab(5);  ws("push");  wtab(7);
      IF oparg.x.op # 0 THEN ASSERT(oparg.x.class = RegVal);
        wReg(oparg.x.val)
      ELSE
        wArgument(oparg.x)
      END;
      wn

  ELSIF (oparg.op = 0) & (oparg.class = Proc) THEN
    wi(addr);  wtab(5);  ws("call");  wtab(7);  wArgument(oparg);  wn

  ELSIF (oparg.op >= OpIntEql) & (oparg.op <= OpIntGeq) THEN
    wi(addr);  WriteExpr(oparg)

  ELSE
    ws("Unrecognised expression, op: ");  wOp(oparg.op,1);
    ws(", arg: ");  wArgument(oparg);  wsn(".");
    ASSERT(FALSE)
  END
END GenerateExpression;


PROCEDURE Generate*(proc: OXB.Object);  (* Analyse and generate code starting at Body *)
TYPE
  codeitem = RECORD
    obj:    OXB.Object;
    stkoff: INTEGER;
    reg:    INTEGER;
  END;
VAR
  inst:  Argument;
  addr:  INTEGER;
  obj:   OXB.Object;
  parms: ARRAY 20 OF codeitem;
  frame: ARRAY 20 OF codeitem;
  i:     INTEGER;
  avoid: SET;
BEGIN
  IF proc # NIL THEN
    ws("PROCEDURE ");  ws(proc.name);  wsn(".");
    obj := proc.type.dsc;
  ELSE
    ws("MODULE ");  ws(Modid);  wsn(".");
    obj := OXB.topScope.next;
  END;

  WHILE obj # NIL DO
    IF    obj.class = OXB.ProcVar               THEN frame[obj.val].obj := obj
    ELSIF obj.class IN {OXB.ValPar, OXB.RefPar} THEN parms[obj.val].obj := obj
    END;
    obj := obj.next
  END;

  IF parms[0].obj # NIL THEN wsn("Parameters:") END;
  FOR i := 0 TO LEN(parms) - 1 DO
    IF parms[i].obj # NIL THEN obj := parms[i].obj;  ASSERT(i = obj.val);
      ws("  ["); wi(i);  ws("] ");
      IF    obj.class = OXB.ValPar THEN ws("Value ")
      ELSIF obj.rdo THEN ws("RefRO ") ELSE ws("Ref   ")
      END;
      ws(obj.name);  ws(", size ");  wi(obj.type.size);
      IF    obj.type.form = OXB.Record THEN ASSERT(obj.class = OXB.RefPar);
        ws(", record")
      ELSIF obj.type.form = OXB.Array  THEN ASSERT(obj.class = OXB.RefPar);
        IF obj.type.len < 0 THEN ws(", open-array") ELSE ws(", array len ");  wi(obj.type.len) END
      END;
      IF obj.type.base # NIL THEN
        ws(", base size ");  wi(obj.type.base.size)
      END;
      wn
    END
  END;

  IF frame[0].obj # NIL THEN wsn("Variables:") END;
  FOR i := 0 TO LEN(frame) - 1 DO
    IF frame[i].obj # NIL THEN obj := frame[i].obj;  ASSERT(i = obj.val);
      ws("  ["); wi(i);  ws("] ");
      ws(obj.name);  ws(", size ");  wi(obj.type.size);
      IF obj.type.base # NIL THEN
        ws(", base size ");  wi(obj.type.base.size)
      END;
      wn
    END
  END;

  IF OpCount > 0 THEN wsn("Operations:") END;
  addr  := 0;
  avoid := {RSP};
  WHILE addr < OpCount DO
    inst := Operations[addr];
    IF inst = NIL THEN  wi(addr);  ws(":");  wtab(5);  wsn("Label")
    ELSE
      IF inst.class # 0 THEN GenerateExpression(addr, inst, avoid) END;
      CASE inst OF ConditionArg:
        wi(addr);  wtab(5);
        IF inst.condition >= 0 THEN wCond(inst.condition) ELSE ws("Jmp") END;
        wtab(7);
        IF inst.target < 0 THEN ws("??")
        ELSE
          ASSERT(inst.target >= 0);
          wi(inst.target)
        END;
        wn
      END
    END;
    INC(addr)
  END;
  INC(TextAdr, 16)
END Generate;


PROCEDURE InvertedCondition(cond: INTEGER): INTEGER;
BEGIN
  IF (cond < 0) OR (cond > 15) THEN H.Trap(0, "InvertedCondition: cond not in {0..15}") END;
  ASSERT(cond IN {0..15});
RETURN ORD(SYSTEM.VAL(SET, cond)/{0}) END InvertedCondition;

PROCEDURE CFJump*(VAR x: Argument);
VAR avoid: SET;
BEGIN
  ws("CFJump x: "); wArgument(x); wn;
  IF ~(x IS ConditionArg) THEN x := Operation(OpTestNZ, x, NIL) END;
  ASSERT(x IS ConditionArg);
  CASE x OF ConditionArg:
    ws("CFJump.2 x: "); wArgument(x); wn;
    IF x = NIL THEN H.Trap(8, "x = NIL in CFJump") END;
    IF (x.ext < 0) OR (x.ext > 15) THEN H.Trap(8, "CFJump: x.ext not in {0..15}") END;
    JumpForward(x, x.falsechain, InvertedCondition(x.ext));
    FixLink(x.truechain)
  END
END CFJump;

PROCEDURE FJump*(VAR p: INTEGER); (*Jump);*)
VAR jump: ConditionArg;
BEGIN
  NEW(jump);
  jump.class      := Jump;
  jump.condition  := -1;
  jump.target     := -1;
  jump.also       := -1;
  jump.op         := 0;
  jump.needs      := {};
  jump.val        := -1;
  jump.ext        := -1;
  jump.falsechain := -1;
  jump.truechain  := -1;
  Operations[OpCount] := jump;
  INC(OpCount)
END FJump;

PROCEDURE BJump*(p: INTEGER); (*Pseudocode);*)
BEGIN ASSERT(p >= 0);  JumpBack(-1, p) END BJump;

PROCEDURE CBJump*(VAR x: Argument; p: INTEGER); (*Label);*)
BEGIN ASSERT(p >= 0);  ASSERT(Operations[p] = NIL);
  IF ~(x IS ConditionArg) THEN x := Operation(OpTestNZ, x, NIL) END;
  ASSERT(x IS ConditionArg);
  CASE x OF ConditionArg:
    JumpBack(InvertedCondition(x.ext), p);
    FixLink(x.truechain);
    FixChain(x.falsechain, p)
  END;
END CBJump;




PROCEDURE StartStatement*;
END StartStatement;


PROCEDURE AddOperation(x: Argument);
BEGIN
  IF x = NIL THEN H.Trap(8, "AddOperation(NIL)") END;
  Operations[OpCount] := x;  INC(OpCount);
END AddOperation;


(* ---- Assignment ---- *)

PROCEDURE Store*(VAR dest, src: Argument); (* dest := src *)
BEGIN
  ASSERT(dest # NIL);
  ASSERT(src # NIL);
  AddOperation(NewOpArg(OpStore, Expr, dest, src, dest.type));
END Store;


PROCEDURE StoreStruct*(VAR x, y: Argument);   (* x := y *)
BEGIN AddOperation(Operation(OpStoreStruc, x, y)) END StoreStruct;


PROCEDURE CopyString*(VAR x, y: Argument);  (* x := y *)
BEGIN AddOperation(Operation(OpCopyString, x, y)) END CopyString;


(* ---- Procedure / function calls ---- *)

PROCEDURE PrepCall*(VAR x: Argument);
BEGIN
  ws("PrepCall "); wArgument(x); wsn(".");
END PrepCall;


PROCEDURE Call*(VAR x: Argument);
BEGIN
  ws("Call ");  wArgument(x);  wsn(".");
  AddOperation(x)
  (*
  IF x.type.base.form = OXB.NoTyp THEN
    AddOperation(NewOpArg(OpCallProc, x, NIL))
  ELSE
    AddOperation(NewOpArg(OpCallFn, x, NIL))
  END
  *)
END Call;

PROCEDURE Return*(res: Argument);
BEGIN
  IF res = NIL THEN
    AddOperation(NewOpArg(OpReturn, Expr, NIL, NIL, NIL))
  ELSE
    AddOperation(NewOpArg(OpReturn, Expr, res, NIL, res.type))
  END
END Return;

PROCEDURE ValueParam*(x, y: Argument);
BEGIN
  ASSERT(y # NIL);
  ASSERT(y.class = ValPar);
  AddOperation(NewOpArg(OpValueParam, Expr, x, y, x.type))
END ValueParam;



(* ---- Standard procedures --- *)

PROCEDURE Assert*(VAR x: Argument);
VAR call: Argument;  assertproc: Argument;
BEGIN
  IF ~(x IS ConditionArg) THEN x := Operation(OpTestNZ, x, NIL) END;
  ASSERT(x IS ConditionArg);
  CASE x OF ConditionArg:
    JumpForward(x, x.truechain, x.val);
    FixLink(x.falsechain);

    assertproc := NewArg(Proc, H.AssertionFailureProc, 0, 1, OXB.procType);
    assertproc.name := "<ASSERT>";
    AddOperation(Operation(OpCallProc, assertproc, NIL));

    FixLink(x.truechain)
  END
END Assert;

PROCEDURE Copy*(VAR x, y, z: Argument);
BEGIN
  wsn("copy")
END Copy;


(* ---- Boolean operators ---- *)

PROCEDURE Not*(VAR x: Argument);   (* x := ~x *)
VAR t: INTEGER;
BEGIN
  IF ~(x IS ConditionArg) THEN x := Operation(OpTestNZ, x, NIL) END;
  ASSERT(x IS ConditionArg);
  CASE x OF ConditionArg:
    x.ext := InvertedCondition(x.ext);
    t := x.falsechain;  x.falsechain := x.truechain;  x.truechain := t
  END
END Not;

PROCEDURE MergeChain(chain1, chain2: INTEGER): INTEGER;
(* Returns chain1 + chain2 *)
VAR p: ConditionArg;
BEGIN
  IF chain1 < 0 THEN chain1 := chain2
  ELSIF chain2 >= 0 THEN
    p := Operations[chain1](ConditionArg);
    WHILE p.also >= 0 DO p := Operations[p.also](ConditionArg) END;
    p.also := chain2
  END
RETURN chain1 END MergeChain;

(*
*PROCEDURE And1*(VAR x: Argument);   (* x := x & *)
*BEGIN
*  (*ws("And1("); wi(x.unique); wsn(").");*)
*  IF ~(x IS ConditionArg) THEN x := Operation(OpTestNZ, x, NIL) END;
*  ASSERT(x IS ConditionArg);
*  CASE x OF ConditionArg:
*    JumpForward(x, x.falsechain, InvertedCondition(x.ext));
*    FixLink(x.truechain)
*  END
*END And1;
*
*PROCEDURE And2*(VAR x, y: Argument);   (* x := x & y *)
*BEGIN
*  (*ws("And2("); wi(x.unique); ws(","); wi(y.unique); wsn(").");*)
*  IF ~(y IS ConditionArg) THEN y := Operation(OpTestNZ, y, NIL) END;
*  ASSERT(x IS ConditionArg);  ASSERT(y IS ConditionArg);
*  CASE x OF ConditionArg:  CASE y OF ConditionArg:
*    x.falsechain := MergeChain(y.falsechain, x.falsechain);
*    x.truechain  := y.truechain;
*    x.ext        := y.ext
*  END END
*END And2;
*
*PROCEDURE Or1*(VAR x: Argument);   (* x := x OR *)
*BEGIN
*  (*ws("Or1("); wi(x.unique); wsn(").");*)
*  IF ~(x IS ConditionArg) THEN x := Operation(OpTestNZ, x, NIL) END;
*  ASSERT(x IS ConditionArg);
*  CASE x OF ConditionArg:
*    JumpForward(x, x.truechain, x.ext);
*    FixLink(x.falsechain)
*  END
*END Or1;
*
*PROCEDURE Or2*(VAR x, y: Argument);   (* x := x OR y *)
*BEGIN
*  (*ws("Or2("); wi(x.unique); ws(","); wi(y.unique); wsn(").");*)
*  IF ~(y IS ConditionArg) THEN x := Operation(OpTestNZ, y, NIL) END;
*  ASSERT(x IS ConditionArg);  ASSERT(y IS ConditionArg);
*  CASE x OF ConditionArg:  CASE y OF ConditionArg:
*    x.falsechain := y.falsechain;
*    x.truechain  := MergeChain(y.truechain, x.truechain);
*    x.ext        := y.ext
*  END END
*END Or2;
*)


(* ---- Control flow ---- *)

PROCEDURE MarkLoopStart*(): INTEGER;
BEGIN Operations[OpCount] := NIL;  INC(OpCount)
RETURN OpCount-1 END MarkLoopStart;

PROCEDURE CheckRegs*;
BEGIN
  (*wsn("CheckRegs.");*)
END CheckRegs;


PROCEDURE For0*(VAR var, ctl: Argument); (* ctl is initial value *)
BEGIN
  ws("For0(var ");  wArgument(var);
  ws(", ctl ");     wArgument(ctl);  wsn(")");
END For0;


PROCEDURE For1*(VAR var, ctl, lim, inc: Argument; VAR lbl: INTEGER);
BEGIN
  ws("For1(var ");  wArgument(var);
  ws(", ctl ");     wArgument(ctl);
  ws(", lim ");     wArgument(lim);
  ws(", inc ");     wArgument(inc);  wsn(")");
END For1;


PROCEDURE For2*(VAR var, ctl, inc: Argument);
BEGIN
  ws("For2(var ");  wArgument(var);
  ws(", ctl ");     wArgument(ctl);
  ws(", inc ");     wArgument(inc); wsn(")");
END For2;


(* ---- Type descriptor construction and testing ---- *)

PROCEDURE BuildTD*(T: OXB.Type; VAR adr: INTEGER);
BEGIN
  wsn("BuildTD.");
END BuildTD;

PROCEDURE TypeTest*(VAR x: Argument; T: OXB.Type; varpar, isguard: BOOLEAN);
BEGIN
  ws("TypeTest(");  wArgument(x); wsn(")");
END TypeTest;


PROCEDURE Open*(modid: ARRAY OF CHAR);
VAR i: INTEGER;  obj: OXB.Object;
BEGIN
  Modid := modid;
  H.wn; wsn("Open");
  H.ZeroFill(Hdr);
  (*
    Strx            := 0;
    StrFInx         := 0;
  *)
  Varsize := 0;
  VarFInx := 0;
  ImpFInx := 0;
END Open;


PROCEDURE SetDataSize*(dc: INTEGER);
BEGIN Varsize := dc END SetDataSize;


PROCEDURE Close*(modid: ORS.Ident; key, exno: INTEGER);
(* Note: export numbers are 1 .. exno-1 *)
VAR i: INTEGER;
BEGIN
  AddOperation(NewOpArg(OpReturn, Expr, NIL, NIL, NIL));
  Generate(NIL);
  IF StringBytes > 0 THEN
    ws("String storage used "); wi(StringBytes);
    ws(" bytes in ");    wi(StrCnt);
    ws(" unique string(s).");
    IF DupStrings > 0 THEN
      ws(" ");                 wi(SavedStringDup);
      ws(" bytes saved in ");  wi(DupStrings);
      ws(" duplicate string(s).")
    END;
    wn;
  END;

  OpCount := 0;
END Close;


(* ---- Initialisation ---- *)

BEGIN
  Texts.OpenWriter(W);
  wsn("OX Generator DCWB 2024-08-11");
  TextAdr        := 0;
  StrCnt         := 0;
  OpCount        := 0;
  SavedStringDup := 0;
  StringBytes    := 0;
  DupStrings     := 0;
  ArgConst0      := NewArg(Const, 0, 0, 0, OXB.intType);
  ArgConst1      := NewArg(Const, 1, 0, 0, OXB.intType);
END OXG.
