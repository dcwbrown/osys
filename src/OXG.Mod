MODULE OXG;  (* DCWB 2023-06 to 2024-05; Code generator for X64 *)

(* Originally: N.Wirth, 16.4.2016 / 4.4.2017 / 31.5.2019 *)
(*             Oberon compiler; code generator for RISC  *)

IMPORT SYSTEM, H := Host, K := Kernel, Files, Modules, Texts, ORS, OXB;

CONST
  WordSize* = OXB.WordSize;
  MaxImpMod = 32;      (* Max no. imported modules *)
  TextSize  = 20000H;  (* Code, string and type descriptor buffer size (128KB) *)


  (* Classes from OXB *)
  Head    = OXB.Head;
  Const   = OXB.Const;
  String  = OXB.String;  (* String constant *)
  Proc    = OXB.Proc;    (* Procedure body address constant *)
  ProcVar = OXB.ProcVar; (* Procedure local variable *)
  ModVar  = OXB.ModVar;  (* Module global / imported VAR *)
  ValPar  = OXB.ValPar;  (* Value parameter *)
  RefPar  = OXB.RefPar;  (* VAR parameter or structured value parameter *)
  Field   = OXB.Field;
  Typ     = OXB.Typ;
  StdProc = OXB.StdProc;
  StdFunc = OXB.StdFunc;
  Mod     = OXB.Mod;

  (* Additional classes *)
  Cond    = 20;
  ItmdVal = 21;
  ItmdRef = 22;


  (* Niladic operations *)
  OpEnter* = 1;

  (* Monadic non-result-returning operations *)
  OpPrepCall      = 11;
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


TYPE
  Pseudocode*    = POINTER TO PseudoDesc;
  Op             = POINTER TO OpDesc;
  Label*         = POINTER TO LabelDesc;
  Jump*          = POINTER TO JumpDesc;

  Argument*     = POINTER TO ArgumentDesc;
  ConditionArg* = POINTER TO ConditionArgDesc;

  PseudoDesc = RECORD
    next: Pseudocode
  END;

  OpDesc = RECORD (PseudoDesc)
    opnum:  INTEGER;
    x:      Argument;      (* operand and result (if any)      *)
    y:      Argument;      (* second operand (dyadic ops only) *)
    result: INTEGER;       (* result local number              *)
  END;

  LabelDesc = RECORD (PseudoDesc)
    label: INTEGER
  END;

  JumpDesc = RECORD (PseudoDesc)
    condition: INTEGER;     (* -1 iff unconditional *)
    target:    Pseudocode;
    prev:      Jump;        (* Previous jump to be assigned the same target as this one *)
  END;

  ArgumentDesc* = RECORD (OXB.ObjCore) unique: INTEGER END;

  ConditionArgDesc = RECORD (ArgumentDesc)  (* val = condition code *)
    falsechain*, truechain*: Jump
  END;

(*
  ObjArgDesc = RECORD (ArgumentDesc) obj: OXB.ObjCore END;

  OpArgDesc = RECORD (ArgumentDesc) op: Op END;
*)


VAR
  W:     Texts.Writer;
  Hdr*:  Modules.ModDesc;
  Modid: ORS.Ident;

  ConstArg0: Argument;
  ConstArg1: Argument;

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

  Unique:   INTEGER;
  Internum: INTEGER;
  Labelnum: INTEGER;

  Ops:      Pseudocode;     (* List of operations implementing procedure *)
  Body:     Pseudocode;     (* Start of most recently compiled body *)


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
  IF    cond = CO  THEN ws("JO ")
  ELSIF cond = CNO THEN ws("JNO")
  ELSIF cond = CC  THEN ws("JB ")
  ELSIF cond = CNC THEN ws("JAE")
  ELSIF cond = CZ  THEN ws("JE ")
  ELSIF cond = CNZ THEN ws("JNE")
  ELSIF cond = CNA THEN ws("JBE")
  ELSIF cond = CA  THEN ws("JA ")
  ELSIF cond = CS  THEN ws("JS ")
  ELSIF cond = CNS THEN ws("JNS")
  ELSIF cond = CP  THEN ws("JPE")
  ELSIF cond = CNP THEN ws("JPO")
  ELSIF cond = CL  THEN ws("JL ")
  ELSIF cond = CNL THEN ws("JGE")
  ELSIF cond = CNG THEN ws("JLE")
  ELSIF cond = CG  THEN ws("JG ")
  ELSE ws("<"); wi(cond); ws(">")
  END
END wCond;

PROCEDURE wAdr(x: Argument);
BEGIN
  ASSERT(x.class IN {ModVar, Proc, String});
  IF x.modno > 0 THEN
    ws("["); wi(x.modno); ws("]."); wi(x.val)
  ELSE
    ws("@"); wh(x.val); ws("H")
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

PROCEDURE wArgument*(x: Argument);
BEGIN
  wi(x.unique);
  IF x.name # "" THEN wc(" ");  ws(x.name) END;
  ws(": ");
  IF    x.class = Head     THEN ws("Head ")
  ELSIF x.class = Const    THEN ws("Const ");     wConst(x)
  ELSIF x.class = String   THEN ws("String ");    wAdr(x)
  ELSIF x.class = Proc     THEN ws("Proc ");      wAdr(x)
  ELSIF x.class = ModVar   THEN ws("ModVar ");    wAdr(x)
  ELSIF x.class = ProcVar  THEN ws("ProcVar ");   wi(x.val)
  ELSIF x.class = ValPar   THEN ws("ValPar ");    wi(x.val)
  ELSIF x.class = RefPar   THEN ws("RefPar ");    wi(x.val)
  ELSIF x.class = Field    THEN ws("Field ");     wi(x.val)
  ELSIF x.class = Typ      THEN ws("Typ ");       ws(x.type.typobj.name)
  ELSIF x.class = StdProc  THEN ws("StdProc ");   wi(x.val)
  ELSIF x.class = StdFunc  THEN ws("StdFunc ");   wi(x.val)
  ELSIF x.class = Mod      THEN ws("Mod ");       ws(x.name)
  ELSIF x.class = Cond     THEN ws("Cond ");      wCond(x.val)
  ELSIF x.class = ItmdVal  THEN ws("ItmdVal ");   wi(x.val)
  ELSIF x.class = ItmdRef  THEN ws("ItmdRef ");   wi(x.val)
  END
END wArgument;

PROCEDURE wOp(op, w: INTEGER);
BEGIN
  IF    op = OpEnter            THEN wsl("Enter",        w, " ")
  ELSIF op = OpPrepCall         THEN wsl("PrepCall",     w, " ")
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


(* ------------------- Body analysis and code generation -------------------- *)

PROCEDURE Here*(): INTEGER;
BEGIN
  wsn("Here.");  (* TODO - return procedure entry address *)
RETURN TextAdr END Here;

PROCEDURE IsSingleCharacterString*(x: Argument): BOOLEAN;
RETURN (x.class = String) & (x.ext = 2) END IsSingleCharacterString;

PROCEDURE DeRef*(VAR x: Argument);
BEGIN
END DeRef;


(* -------------------- Operation argument construction --------------------- *)

PROCEDURE NewArg*(class, val, ext, modno: INTEGER; type: OXB.Type): Argument;
VAR arg: Argument;  carg: ConditionArg;
BEGIN
  IF class = Cond THEN NEW(carg);  arg := carg ELSE NEW(arg) END;
  arg.class := class;  arg.val  := val;  arg.ext := ext;
  arg.modno := modno;  arg.type := type;
  arg.name  := "";     arg.rdo  := FALSE;
  arg.unique := Unique;   INC(Unique)
RETURN arg END NewArg;

PROCEDURE MakeArgument*(y: OXB.Object; curlev: INTEGER): Argument;
VAR modno: INTEGER;  arg: Argument;
BEGIN
  ASSERT(y.class IN {Const, String, Proc, ModVar, ProcVar, ValPar, RefPar, Typ, Field});
  arg := NewArg(y.class, y.val, y.ext, y.modno, y.type);
  arg.name := y.name;
  IF ~(y.class IN {OXB.Const, OXB.String, OXB.Proc, OXB.ValPar, OXB.RefPar}) THEN
    IF (y.modno < 0) & (y.modno # -curlev) THEN ORS.Mark("not accessible") END
  END;
  IF y.class = String THEN ASSERT(y.rdo) END;
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

PROCEDURE JumpForward(VAR chain: Jump; cond: INTEGER);
VAR jump: Jump;
BEGIN
  NEW(jump);  Ops.next := jump;  Ops := jump;
  jump.condition := cond;  jump.target := NIL;  jump.prev := chain;
  chain := jump
END JumpForward;

PROCEDURE JumpBack(cond: INTEGER; target: Pseudocode);
VAR jump: Jump;
BEGIN
  NEW(jump);  Ops.next := jump;  Ops := jump;
  jump.condition := cond;  jump.target := target;  jump.prev := NIL;
END JumpBack;


PROCEDURE FixChain(VAR l: Jump; target: Label);
BEGIN
  WHILE l # NIL DO l.target := target; l := l.prev END;
  l := NIL
END FixChain;

PROCEDURE FixLink*(VAR l: Jump);
VAR target: Label;
BEGIN
  IF l # NIL THEN
    NEW(target);
    Ops.next := target;  Ops := target;
    target.label := Labelnum;  INC(Labelnum);
    FixChain(l, target)
  END
END FixLink;

PROCEDURE LoadFromCC(VAR x: Argument);
(*            jxx    tchain
    fchain:   xor    eax,eax
              jmp    done
    tchain:   mov    eax,1
    done:                      *)
VAR op: POINTER TO OpDesc;  done: Jump;
BEGIN
  INC(Internum);
  ASSERT(x # NIL);
  ASSERT(x IS ConditionArg);
  IF (x(ConditionArg).truechain = NIL) & (x(ConditionArg).falsechain = NIL) THEN
    (* Can use X86 SetCC instruction *)
    NEW(op);  Ops.next := op;  Ops := op;  op.opnum := OpSetCC;
    op.x := x;  op.result := Internum;
  ELSE
    JumpForward(x(ConditionArg).truechain, x.val);

    FixLink(x(ConditionArg).falsechain);
    NEW(op);  Ops.next := op;  Ops := op;  op.opnum := OpLoad;
    op.x      := ConstArg0;
    op.result := Internum;
    JumpForward(done, -1);

    FixLink(x(ConditionArg).truechain);
    NEW(op);  Ops.next := op;  Ops := op;  op.opnum := OpLoad;
    op.x      := ConstArg1;
    op.result := Internum;
    FixLink(done);

    x := NewArg(ItmdVal, op.result, 0, 0, OXB.intType)
  END;
  INC(Internum)
END LoadFromCC;

PROCEDURE OpnumToIntCondition(opnum: INTEGER; type: OXB.Type): INTEGER;
VAR result: INTEGER;  signed: BOOLEAN;
BEGIN
  signed := type.ref IN {OXB.Int8, OXB.Int16, OXB.Int32, OXB.Int64};
  IF (opnum < OpIntEql) OR (opnum > OpStringGeq) THEN
    H.ws("opnum "); H.wi(opnum); H.wn;
    H.Trap(8, "opnum out of range in OpnumToIntCondition")
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


PROCEDURE Operation(opnum: INTEGER; VAR x: Argument; y: Argument);
CONST TraceOperation = FALSE;
VAR op: POINTER TO OpDesc;
BEGIN
  IF TraceOperation THEN
    wOp(opnum, 1);
    IF x # NIL THEN
      ws("("); wi(x.unique);
      IF y # NIL THEN ws(", "); wi(y.unique) END;
      ws(")");
      ASSERT((opnum = OpSetCC) OR ~(x IS ConditionArg))
    END
  END;

  IF opnum = OpEnter THEN Internum := 0;  Labelnum := 1  END;

  IF (x # NIL)
   & (x IS ConditionArg)
   & ((opnum < OpIntEql) OR (opnum > OpStringGeq)) THEN
    LoadFromCC(x)
  END;

  NEW(op);  op.opnum := opnum;  op.x := x;  op.y := y;  op.result := -1;

  IF (opnum >= OpAbs) & (opnum <= OpField) THEN
    op.result := Internum;  INC(Internum);
    x := NewArg(ItmdVal, op.result, 0, 0, x.type);
  ELSIF (opnum >= OpIntEql) & (opnum <= OpStringGeq) THEN
    x := NewArg(Cond, OpnumToIntCondition(opnum, x.type), 0, 0, OXB.boolType);
    IF TraceOperation THEN ws(" -> ["); wi(x.unique); ws("]") END
  ELSIF opnum = OpTestNZ THEN
    x := NewArg(Cond, CNZ, 0, 0, OXB.boolType);
    IF TraceOperation THEN ws(" -> ["); wi(x.unique); ws("]") END
  END;
  IF Ops # NIL THEN Ops.next := op END;  Ops := op;

  IF opnum = OpEnter THEN Body := Ops END;

  IF TraceOperation & (op.result >= 0) THEN ws(" -> "); wi(op.result) END;
  IF TraceOperation THEN wn END
END Operation;


PROCEDURE Monadic*(op: INTEGER; VAR x: Argument);
BEGIN
  IF (x # NIL) & (x.class = Const) & (op >= OpAbs) & (op <= OpInvert) THEN
    IF    op = OpAbs          THEN x.val := ABS(x.val)
    ELSIF op = OpSetSingleton THEN x.val := LSL(1, x.val)
    ELSIF op = OpNegate       THEN x.val := - x.val
    ELSIF op = OpRealNegate   THEN ASSERT(FALSE) (*x.val := ORD(-SYSTEM.VAL(REAL, x.val))*)
    ELSIF op = OpInvert       THEN x.val := ORD(-SYSTEM.VAL(SET, x.val))
    END
  ELSE
    Operation(op, x, NIL)
  END
END Monadic;

PROCEDURE Dyadic*(op: INTEGER; VAR x: Argument; y: Argument);
VAR type: OXB.Type;  xval, yval: INTEGER;
BEGIN
  IF x = NIL THEN H.Trap(24, "x NIL in Dyadic") END;
  IF (x # NIL) & (y # NIL)
   & (x.class = Const) & (y.class = Const)
   & (   (op >= OpLSL)    & (op <= OpRealDivide)
      OR (op >= OpIntEql) & (op <= OpRealGeq)) THEN
    type := x.type;
    xval := x.val;
    yval := y.val;
    IF    op = OpLSL          THEN x := NewArg(Const, LSL(xval, yval), 0, 0, type)
    ELSIF op = OpASR          THEN x := NewArg(Const, ASR(xval, yval), 0, 0, type)
    ELSIF op = OpROR          THEN x := NewArg(Const, ROR(xval, yval), 0, 0, type)
    ELSIF op = OpADC          THEN ASSERT(FALSE) (*x := MakeConstArg(type, xval yval)*)
    ELSIF op = OpSBC          THEN ASSERT(FALSE) (*x := MakeConstArg(type, xval yval)*)
    ELSIF op = OpUML          THEN ASSERT(FALSE) (*x := MakeConstArg(type, xval yval)*)
    ELSIF op = OpBit          THEN ASSERT(FALSE) (*x := MakeConstArg(type, SYSTEM.BIT(xval, yval))*)
    ELSIF op = OpSetRange     THEN x := NewArg(Const, ORD({xval..yval}), 0, 0, type)
    ELSIF op = OpSetOr        THEN x := NewArg(Const, ORD(SYSTEM.VAL(SET,xval) + SYSTEM.VAL(SET,yval)), 0, 0, type)
    ELSIF op = OpSetAndNot    THEN x := NewArg(Const, ORD(SYSTEM.VAL(SET,xval) - SYSTEM.VAL(SET,yval)), 0, 0, type)
    ELSIF op = OpSetAnd       THEN x := NewArg(Const, ORD(SYSTEM.VAL(SET,xval) * SYSTEM.VAL(SET,yval)), 0, 0, type)
    ELSIF op = OpSetXor       THEN x := NewArg(Const, ORD(SYSTEM.VAL(SET,xval) / SYSTEM.VAL(SET,yval)), 0, 0, type)
    ELSIF op = OpIntPlus      THEN x := NewArg(Const, xval + yval, 0, 0, type)
    ELSIF op = OpIntMinus     THEN x := NewArg(Const, xval - yval, 0, 0, type)
    ELSIF op = OpIntMultiply  THEN x := NewArg(Const, xval * yval, 0, 0, type)
    ELSIF op = OpIntDivide    THEN x := NewArg(Const, xval DIV yval, 0, 0, type)
    ELSIF op = OpIntModulus   THEN x := NewArg(Const, xval MOD yval, 0, 0, type)
    ELSIF op = OpRealPlus     THEN ASSERT(FALSE) (*x := MakeConstArg(type, xval yval)*)
    ELSIF op = OpRealMinus    THEN ASSERT(FALSE) (*x := MakeConstArg(type, xval yval)*)
    ELSIF op = OpRealMultiply THEN ASSERT(FALSE) (*x := MakeConstArg(type, xval yval)*)
    ELSIF op = OpRealDivide   THEN ASSERT(FALSE) (*x := MakeConstArg(type, xval yval)*)
    ELSIF op = OpIntEql       THEN x := NewArg(Const, ORD(xval =  yval), 0, 0, OXB.boolType)
    ELSIF op = OpIntNeq       THEN x := NewArg(Const, ORD(xval #  yval), 0, 0, OXB.boolType)
    ELSIF op = OpIntLss       THEN x := NewArg(Const, ORD(xval <  yval), 0, 0, OXB.boolType)
    ELSIF op = OpIntLeq       THEN x := NewArg(Const, ORD(xval <= yval), 0, 0, OXB.boolType)
    ELSIF op = OpIntGtr       THEN x := NewArg(Const, ORD(xval >  yval), 0, 0, OXB.boolType)
    ELSIF op = OpIntGeq       THEN x := NewArg(Const, ORD(xval >= yval), 0, 0, OXB.boolType)
    (*
    ELSIF op = OpRealEql      THEN x := NewArg(Const, ORD(SYSTEM.VAL(REAL, xval) =  SYSTEM.VAL(REAL, yval)), 0, 0, OXB.boolType)
    ELSIF op = OpRealNeq      THEN x := NewArg(Const, ORD(SYSTEM.VAL(REAL, xval) #  SYSTEM.VAL(REAL, yval)), 0, 0, OXB.boolType)
    ELSIF op = OpRealLss      THEN x := NewArg(Const, ORD(SYSTEM.VAL(REAL, xval) <  SYSTEM.VAL(REAL, yval)), 0, 0, OXB.boolType)
    ELSIF op = OpRealLeq      THEN x := NewArg(Const, ORD(SYSTEM.VAL(REAL, xval) <= SYSTEM.VAL(REAL, yval)), 0, 0, OXB.boolType)
    ELSIF op = OpRealGtr      THEN x := NewArg(Const, ORD(SYSTEM.VAL(REAL, xval) >  SYSTEM.VAL(REAL, yval)), 0, 0, OXB.boolType)
    ELSIF op = OpRealGeq      THEN x := NewArg(Const, ORD(SYSTEM.VAL(REAL, xval) >= SYSTEM.VAL(REAL, yval)), 0, 0, OXB.boolType)
    *)
    END
  ELSE
    Operation(op, x, y)
  END
END Dyadic;



PROCEDURE Generate*(proc: OXB.Object);  (* Analyse and generate code starting at Body *)
TYPE
  codeitem = RECORD
    obj:    OXB.Object;
    stkoff: INTEGER;
    reg:    INTEGER;
  END;
VAR
  inst:  Pseudocode;
  obj:   OXB.Object;
  parms: ARRAY 20 OF codeitem;
  frame: ARRAY 20 OF codeitem;
  i:     INTEGER;
BEGIN
  IF proc # NIL THEN
    ws("PROCEDURE ");  ws(proc.name);  wsn(".");
    obj := proc.type.dsc;
  ELSE
    ws("MODULE ");  ws(Modid);  wsn(".");
    obj := OXB.topScope.next;
  END;

  WHILE obj # NIL DO
    IF    obj.class = OXB.ProcVar              THEN frame[obj.val].obj := obj
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
      IF obj.type.base # NIL THEN
        ws(", base size ");  wi(obj.type.base.size)
      END;
      IF ((obj.class = OXB.RefPar) & (obj.type.form = OXB.Array)) THEN
        IF obj.type.len < 0 THEN ws(", open-array") ELSE ws(", len ");  wi(obj.type.len) END
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

  IF Body # NIL THEN wsn("Operations:") END;
  inst := Body;
  WHILE inst # NIL DO
    CASE inst OF
      Op:    wtab(10); wOp(inst.opnum, 1); wtab(15);
             IF inst.x # NIL THEN
               wArgument(inst.x);
               IF inst.y # NIL THEN ws(", ");  wtab(40);  wArgument(inst.y) END;
               ws(" ")
             END;
             IF inst.result >= 0 THEN ws("-> "); wi(inst.result) END;
             wn
    | Label: ws("    l");  wil(inst.label, 5);  wsn("Label")
    | Jump:  wtab(10);
             IF inst.condition >= 0 THEN wCond(inst.condition) ELSE ws("Jmp") END;
             wtab(15); wc("l");
             IF inst.target = NIL THEN ws("??")
             ELSE
               ASSERT(inst.target # NIL);
               wi(inst.target(Label).label)
             END;
             wn
    END;
    inst := inst.next
  END;
  INC(TextAdr, 16)
END Generate;




PROCEDURE InvertedCondition(cond: INTEGER): INTEGER;
BEGIN ASSERT(cond IN {0..15});
RETURN ORD(SYSTEM.VAL(SET, cond)/{0}) END InvertedCondition;

PROCEDURE CFJump*(VAR x: Argument);
BEGIN
  IF ~(x IS ConditionArg) THEN Dyadic(OpTestNZ, x, NIL) END;
  ASSERT(x IS ConditionArg);
  CASE x OF ConditionArg:
    JumpForward(x.falsechain, InvertedCondition(x.val));
    FixLink(x.truechain)
  END
END CFJump;

PROCEDURE FJump*(VAR p: Jump);
BEGIN JumpForward(p, -1) END FJump;

PROCEDURE BJump*(p: Pseudocode);
BEGIN ASSERT(p # NIL);  JumpBack(-1, p) END BJump;

PROCEDURE CBJump*(VAR x: Argument; p: Label);
BEGIN ASSERT(p # NIL);  ASSERT(p IS Label);
  IF ~(x IS ConditionArg) THEN Dyadic(OpTestNZ, x, NIL) END;
  ASSERT(x IS ConditionArg);
  CASE x OF ConditionArg:
    JumpBack(InvertedCondition(x.val), p);
    FixLink(x.truechain);
    FixChain(x.falsechain, p)
  END;
END CBJump;




PROCEDURE StartStatement*;
END StartStatement;


(* ---- Assignment ---- *)

PROCEDURE Store*(VAR dest, src: Argument); (* dest := src *)
BEGIN
  ASSERT(dest # NIL);
  ASSERT(src # NIL);
  Dyadic(OpStore, dest, src)
END Store;


PROCEDURE StoreStruct*(VAR x, y: Argument);   (* x := y *)
BEGIN Dyadic(OpStoreStruc, x, y) END StoreStruct;


PROCEDURE CopyString*(VAR x, y: Argument);  (* x := y *)
BEGIN Dyadic(OpCopyString, x, y) END CopyString;


(* ---- Procedure / function calls ---- *)

PROCEDURE PrepCall*(VAR x: Argument; VAR saved: SET);
BEGIN Monadic(OpPrepCall, x) END PrepCall;


PROCEDURE Call*(VAR x: Argument; saved: SET);
BEGIN
  IF x.type.base.form = OXB.NoTyp THEN
    Monadic(OpCallProc, x)
  ELSE
    Monadic(OpCallFn, x)
  END
END Call;


(* ---- Standard procedures --- *)

PROCEDURE Assert*(VAR x: Argument);
VAR call: Argument;  assertproc: Argument;
BEGIN
  IF ~(x IS ConditionArg) THEN Dyadic(OpTestNZ, x, NIL) END;
  ASSERT(x IS ConditionArg);
  CASE x OF ConditionArg:
    JumpForward(x.truechain, x.val);
    FixLink(x.falsechain);

    assertproc := NewArg(Proc, H.AssertionFailureProc, 0, 1, OXB.procType);
    assertproc.name := "<ASSERT>";
    Dyadic(OpCallProc, assertproc, NIL);

    FixLink(x.truechain)
  END
END Assert;

PROCEDURE Copy*(VAR x, y, z: Argument);
BEGIN
  ws(",");  wi(y.unique);  ws(",");  wi(z.unique); wsn(")");
END Copy;


(* ---- Boolean operators ---- *)

PROCEDURE Not*(VAR x: Argument);   (* x := ~x *)
VAR t: Jump;
BEGIN
  IF ~(x IS ConditionArg) THEN Dyadic(OpTestNZ, x, NIL) END;
  ASSERT(x IS ConditionArg);
  CASE x OF ConditionArg:
    x.val := InvertedCondition(x.val);
    t := x.falsechain;  x.falsechain := x.truechain;  x.truechain := t
  END
END Not;

PROCEDURE MergeChain(falsechain, truechain: Jump): Jump;
(* Returns falsechain + truechain *)
VAR p: Jump;
BEGIN
  IF falsechain = NIL THEN falsechain := truechain
  ELSIF truechain # NIL THEN
    p := falsechain; WHILE p.next # NIL DO p := p.next(Jump) END;
    p.next := truechain
  END
RETURN falsechain END MergeChain;

PROCEDURE And1*(VAR x: Argument);   (* x := x & *)
BEGIN
  (*ws("And1("); wi(x.unique); wsn(").");*)
  IF ~(x IS ConditionArg) THEN Dyadic(OpTestNZ, x, NIL) END;
  ASSERT(x IS ConditionArg);
  CASE x OF ConditionArg:
    JumpForward(x.falsechain, InvertedCondition(x.val));
    FixLink(x.truechain)
  END
END And1;

PROCEDURE And2*(VAR x, y: Argument);   (* x := x & y *)
BEGIN
  (*ws("And2("); wi(x.unique); ws(","); wi(y.unique); wsn(").");*)
  IF ~(y IS ConditionArg) THEN Dyadic(OpTestNZ, y, NIL) END;
  ASSERT(x IS ConditionArg);  ASSERT(y IS ConditionArg);
  CASE x OF ConditionArg:  CASE y OF ConditionArg:
    x.falsechain := MergeChain(y.falsechain, x.falsechain);
    x.truechain  := y.truechain;
    x.val        := y.val
  END END
END And2;

PROCEDURE Or1*(VAR x: Argument);   (* x := x OR *)
BEGIN
  (*ws("Or1("); wi(x.unique); wsn(").");*)
  IF ~(x IS ConditionArg) THEN Dyadic(OpTestNZ, x, NIL) END;
  ASSERT(x IS ConditionArg);
  CASE x OF ConditionArg:
    JumpForward(x.truechain, x.val);
    FixLink(x.falsechain)
  END
END Or1;

PROCEDURE Or2*(VAR x, y: Argument);   (* x := x OR y *)
BEGIN
  (*ws("Or2("); wi(x.unique); ws(","); wi(y.unique); wsn(").");*)
  IF ~(y IS ConditionArg) THEN Dyadic(OpTestNZ, y, NIL) END;
  ASSERT(x IS ConditionArg);  ASSERT(y IS ConditionArg);
  CASE x OF ConditionArg:  CASE y OF ConditionArg:
    x.falsechain := y.falsechain;
    x.truechain  := MergeChain(y.truechain, x.truechain);
    x.val        := y.val
  END END
END Or2;


(* ---- Control flow ---- *)

PROCEDURE MarkLoopStart*(): Label;
VAR target: Label;
BEGIN
  NEW(target);  target.label := Labelnum;  INC(Labelnum);
  Ops.next := target;  Ops := target;
RETURN target END MarkLoopStart;

PROCEDURE CheckRegs*;
BEGIN
  (*wsn("CheckRegs.");*)
END CheckRegs;


PROCEDURE For0*(VAR var, ctl: Argument); (* ctl is initial value *)
BEGIN
  ws("For0(var ");  wi(var.unique);
  ws(", ctl ");     wi(ctl.unique);  wsn(")");
END For0;


PROCEDURE For1*(VAR var, ctl, lim, inc: Argument; VAR l: Jump);
BEGIN
  ws("For1(var ");  wi(var.unique);
  ws(", ctl ");     wi(ctl.unique);
  ws(", lim ");     wi(lim.unique);
  ws(", inc ");     wi(inc.unique);  wsn(")");
END For1;


PROCEDURE For2*(VAR var, ctl, inc: Argument);
BEGIN
  ws("For2(var ");  wi(var.unique);
  ws(", ctl ");     wi(ctl.unique);
  ws(", inc ");     wi(inc.unique); wsn(")");
END For2;


(* ---- Type descriptor construction and testing ---- *)

PROCEDURE BuildTD*(T: OXB.Type; VAR adr: INTEGER);
BEGIN
  wsn("BuildTD.");
END BuildTD;

PROCEDURE TypeTest*(VAR x: Argument; T: OXB.Type; varpar, isguard: BOOLEAN);
BEGIN
  ws("TypeTest(");  wi(x.unique); wsn(")");
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
VAR i: INTEGER;  x: Argument;
BEGIN
  Operation(OpReturn, x, NIL);
  Generate(NIL);
  ws("String de-duplication saved "); wi(SavedStringDup);
  ws(" bytes in ");                   wi(DupStrings);
  ws(" strings leaving ");            wi(StrCnt);
  ws(" strings in ");                 wi(StringBytes); wsn(" bytes.");
  Ops := NIL;
  Body := NIL;
END Close;


(* ---- Initialisation ---- *)

BEGIN
  Texts.OpenWriter(W);
  wsn("OX Generator DCWB 2024-08-11");
  Unique   := 0;
  Internum := 0;
  Labelnum := 0;
  TextAdr  := 0;
  StrCnt   := 0;
  Ops      := NIL;
  SavedStringDup := 0;
  StringBytes    := 0;
  DupStrings := 0;
  ConstArg0 := NewArg(Const, 0, 0, 0, OXB.intType);
  ConstArg1 := NewArg(Const, 1, 0, 0, OXB.intType);
END OXG.
