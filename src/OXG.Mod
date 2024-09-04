MODULE OXG;  (* DCWB 2023-06 to 2024-05; Code generator for X64 *)

(* Originally: N.Wirth, 16.4.2016 / 4.4.2017 / 31.5.2019  Oberon compiler; code generator for RISC*)

(*
    It may make sense to have a separate 'local' mode, independent of Eadr:
    Currently there are a lot of places which do:
        IF (mode = Eadr) & (n = RSP) THEN INC(offset, X64.SPO * 8) END;
    This allows mode Eadr to be used both for local variable access, and for
    direct access to the stack when manipulating it at a lower level.
    With a local mode x.n would be a stack offset, the same as Strucp, and
    would always have SPO applied, while Eadr, n = RSP would never have SPO
    applied.
*)

IMPORT SYSTEM, H := Host, K := Kernel, Files, Modules, Texts, ORS, OXB;

CONST
  WordSize* = OXB.WordSize;
  MaxImpMod = 32;      (* Max no. imported modules *)
  TextSize  = 20000H;  (* Code, string and type descriptor buffer size (128KB) *)

  (* Classes from OXB *)
  Head   = OXB.Head;
  Const  = OXB.Const;
  Local  = OXB.Local;  (* Procedure local variable or evaluation temporary *)
  Var    = OXB.Var;    (* Module global / imported VAR *)
  Par    = OXB.Par;    (* Value parameter *)
  ParInd = OXB.ParInd; (* VAR parameter or structured value parameter *)
  Fld    = OXB.Fld;
  Typ    = OXB.Typ;
  SProc  = OXB.SProc;
  SFunc  = OXB.SFunc;
  Mod    = OXB.Mod;

  (* Extended classes *)
  Cond    = 11;
  Reg     = 14;
  RegInd  = 15;
  Global  = 16;  (* non-stack Var *)


  (* Niladic operations *)
  OpEnter        = 1;

  (* Monadic non-result-returning operations *)
  OpPrepCall     = 11;
  OpReturn       = 12;
  OpNew*         = 14;
  OpLed*         = 15;
  OpValueParam   = 18;
  OpOpenArrParam = 19;
  OpByteArrParam = 20;
  OpVarParam     = 21;
  OpStringParam  = 22;
  OpTestNZ       = 23;  (* Sets condition code flags *)
  OpCallProc     = 24;


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
  OpLSL*          = 121;
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
  Pseudocode* = POINTER TO PseudoDesc;
  ItemRef*    = POINTER TO Item;

  PseudoDesc = RECORD
    next: Pseudocode
  END;

  Op = POINTER TO OpDesc;
  OpDesc = RECORD (PseudoDesc)
    opnum:  INTEGER;
    x:      ItemRef;       (* operand and result (if any)      *)
    y:      ItemRef;       (* second operand (dyadic ops only) *)
    result: INTEGER;       (* result local number              *)
  END;

  Label* = POINTER TO LabelDesc;
  LabelDesc = RECORD (PseudoDesc)
    label: INTEGER
  END;

  Jump* = POINTER TO JumpDesc;
  JumpDesc = RECORD (PseudoDesc)
    condition: INTEGER;     (* -1 iff unconditional *)
    target:    Pseudocode;
    prev:      Jump;        (* Previous jump to be assigned the same target as this one *)
  END;



  ArgumentDesc*     = RECORD END;
  ConditionArgDesc* = RECORD (ArgumentDesc) code: INTEGER; falsechain, truechain: Jump END;
  StringArgDesc*    = RECORD (ArgumentDesc) adr, len: INTEGER END;
  ProcArgDesc*      = RECORD (ArgumentDesc) modno, adr: INTEGER END;

  TypedArgDesc*     = RECORD (ArgumentDesc) type: OXB.Type END;
  ConstArgDesc*     = RECORD (TypedArgDesc) val: INTEGER END;
  GlobalArgDesc*    = RECORD (TypedArgDesc) modno, adr: INTEGER END;
  LocalArgDesc*     = RECORD (TypedArgDesc) index: INTEGER END;
  ParamArgDesc*     = RECORD (TypedArgDesc) index: INTEGER END;
  ParIndArgDesc*    = RECORD (TypedArgDesc) index: INTEGER END;
(*
  RegArgDesc*       = RECORD (TypedArgDesc) regno: INTEGER END;
  RegIndArgDesc*    = RECORD (TypedArgDesc) regno, offset, offsetreg, offsetscale: INTEGER END;
  StkArgDesc*       = RECORD (TypedArgDesc) index: INTEGER END;
  StkIndArgDesc*    = RECORD (TypedArgDesc) index, offset, offsetreg, offsetscale: INTEGER END;
*)

  Argument*     = POINTER TO ArgumentDesc;
  ConditionArg* = POINTER TO ConditionArgDesc;
  ConstArg*     = POINTER TO ConstArgDesc;
  StringArg*    = POINTER TO StringArgDesc;
  ProcArg*      = POINTER TO ProcArgDesc;
  GlobalArg*    = POINTER TO GlobalArgDesc;
  LocalArg      = POINTER TO LocalArgDesc;
  ParamArg      = POINTER TO ParamArgDesc;
  ParIndArg     = POINTER TO ParIndArgDesc;
(*
  RegArg        = POINTER TO RegArgDesc;
  RegIndArg     = POINTER TO RegIndArgDesc;
  StkArg        = POINTER TO StkArgDesc;
  StkIndArg     = POINTER TO StkIndArgDesc;
*)

  Item* = RECORD
    unique:        INTEGER;
    extclass*:     INTEGER;
    type*:         OXB.Type;
    modno:         INTEGER;     (* 0 = local, 1 = 1st imported module, ... *)
    val*:          INTEGER;
    offset:        INTEGER;     (* For indirect items: Var, Par, RegInd *)
    size*:         INTEGER;     (* = string length *)
    readonly*:     BOOLEAN;
    condition:     INTEGER;     (* X64 Condition code: see constants Cxx above *)
    falsechain*:   Jump;
    truechain*:    Jump;
  END;

  LocalDsc = RECORD
    obj:   OXB.Object;  (* Only for named local variables *)
    reg:   INTEGER;     (* Register allocated, -1 if none *)
    stack: INTEGER;     (* Stack position, -1 if none *)
  END;


VAR
  W:     Texts.Writer;
  Hdr*:  Modules.ModDesc;
  Modid: ORS.Ident;

  ConstItem0: ItemRef;
  ConstItem1: ItemRef;
  NilItem:    Item;

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

  Locals:   ARRAY 100 OF LocalDsc;
  LocalCnt: INTEGER;


(* ---------------- Experimentation - to see what OXP calls ----------------- *)

PROCEDURE ifexperiment;
VAR a, b, c: BOOLEAN; i, j: INTEGER;
BEGIN
  IF a & b THEN i := 1 ELSE j := 1 END;
END ifexperiment;

PROCEDURE func(a, b: INTEGER): INTEGER;
RETURN a * b END func;

PROCEDURE expressionevaluationexperiment;
VAR a, b, c, d: INTEGER;
BEGIN
  a := a * b + c * d;
  a := func(func(a, b), func(c, d));
  REPEAT INC(a) UNTIL a >= 5;
END expressionevaluationexperiment;

PROCEDURE BoolTestAssign;
VAR a, b, c: BOOLEAN;
BEGIN
  a := b = c;
END BoolTestAssign;

(* -------------------------------- Tracing --------------------------------- *)

PROCEDURE wn; BEGIN Texts.WriteLn(W); Texts.Append(Texts.Log, W.buf) END wn;
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


PROCEDURE wClass(class, w: INTEGER);
BEGIN
  IF    class = Head   THEN wsl("Head",   w, " ")
  ELSIF class = Const  THEN wsl("Const",  w, " ")
  ELSIF class = Local  THEN wsl("Local",  w, " ")
  ELSIF class = Var    THEN wsl("Var",    w, " ")
  ELSIF class = Par    THEN wsl("Par",    w, " ")
  ELSIF class = ParInd THEN wsl("ParInd", w, " ")
  ELSIF class = Fld    THEN wsl("Fld",    w, " ")
  ELSIF class = Typ    THEN wsl("Typ",    w, " ")
  ELSIF class = SProc  THEN wsl("SProc",  w, " ")
  ELSIF class = SFunc  THEN wsl("SFunc",  w, " ")
  ELSIF class = Mod    THEN wsl("Mod",    w, " ")
  ELSIF class = Cond   THEN wsl("Cond",   w, " ")
  ELSIF class = Reg    THEN wsl("Reg",    w, " ")
  ELSIF class = RegInd THEN wsl("RegInd", w, " ")
  ELSIF class = Global THEN wsl("Global", w, " ")
  ELSE                      wsl("?",      w, " ")
  END
END wClass;

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
  ELSIF form = OXB.Proc    THEN wsl("Proc",    w, " ")
  ELSIF form = OXB.String  THEN wsl("String",  w, " ")
  ELSIF form = OXB.Array   THEN wsl("Array",   w, " ")
  ELSIF form = OXB.Record  THEN wsl("Record",  w, " ")
  ELSE ws("<"); wi(form); ws(">")
  END
END wForm;

(*
  PROCEDURE wRef(ref: Reference);
  BEGIN
    IF ref.section < 16 THEN wSection(ref.section, 1)
    ELSE ASSERT(ref.section >= 16); ws("Imp["); wi(ref.section-16); ws("]")
    END;
    ws(" "); wh(ref.val); ws("H");
  END wRef;
*)

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

PROCEDURE wAdr(x: Item);
BEGIN
  IF x.modno # 0 THEN
    ws("["); wi(x.modno); ws("]."); wi(x.val)
  ELSE
    ws("@"); wh(x.val); ws("H")
  END
END wAdr;

PROCEDURE wConst(x: Item);
VAR i, l: INTEGER;
BEGIN
  IF x.type.form IN {OXB.Proc, OXB.String} THEN wAdr(x);
    IF x.type.form = OXB.String THEN
      ws(" '"); i := x.val;  l := i + x.size - 1;
      WHILE i < l DO wc(CHR(Text[i]));  INC(i) END;
      wc("'")
    END;
  ELSIF (x.val >= 0) & (x.val <= 9) THEN wi(x.val)
  ELSE wh(x.val); ws("H ")
  END;
END wConst;

PROCEDURE wItem(x: Item);
BEGIN
  ws("["); wi(x.unique); ws("] ");
  wClass(x.extclass, 1); ws(" ");
  IF x.type # NIL THEN wForm(x.type.form, 1); ws(" ") END;
  IF    x.extclass = Cond   THEN wCond(x.condition);
  ELSIF x.extclass = Const  THEN wConst(x)
  ELSIF x.extclass = Reg    THEN ws("r"); wi(x.val)
  ELSIF x.extclass = RegInd THEN ws("r"); wi(x.val)
  ELSIF x.extclass = Global THEN wAdr(x)
  ELSIF x.extclass = Var    THEN ws("[rsp+"); wh(x.val); ws("H]")   (* VAR variable or atomic value parameter *)
  ELSIF x.extclass = Par    THEN ws("[rsp+"); wh(x.val); ws("H]")   (* VAR parameter or structured value parameter (indirect) *)
  ELSIF x.extclass = Fld    THEN ws("+"); wh(x.val); ws("H")
  ELSIF x.extclass = Typ    THEN ws(".")
  ELSIF x.extclass = SProc  THEN ws(".")
  ELSIF x.extclass = SFunc  THEN ws(".")
  ELSIF x.extclass = Mod    THEN ws(".")
  END;
  IF x.offset # 0 THEN ws("+"); wh(x.offset); ws("H") END;
END wItem;

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

PROCEDURE NextUnique(): INTEGER;
BEGIN INC(Unique);  RETURN Unique END NextUnique;

PROCEDURE NextIntermediate(): INTEGER;
BEGIN INC(Internum);  RETURN Internum END NextIntermediate;

PROCEDURE Here*(): INTEGER;
BEGIN
  wsn("Here.");  (* TODO - return procedure entry address *)
RETURN TextAdr END Here;



PROCEDURE IsSingleCharacterString*(x: Item): BOOLEAN;
RETURN (x.type.form = OXB.String) & (x.size = 2)
END IsSingleCharacterString;


PROCEDURE DeRef*(VAR x: Item);
BEGIN
END DeRef;


(* -------------------- Operation argument construction --------------------- *)

PROCEDURE MakeConditionArg(code: INTEGER): Argument;
VAR result: ConditionArg;
BEGIN NEW(result);  result.code := code
RETURN result END MakeConditionArg;

PROCEDURE MakeStringArg(adr, len: INTEGER): Argument;
VAR result: StringArg;
BEGIN NEW(result);  result.adr := adr; result.len := len
RETURN result END MakeStringArg;

PROCEDURE MakeProcArg(modno, adr: INTEGER): Argument;
VAR result: ProcArg;
BEGIN NEW(result);  result.modno := modno; result.adr := adr
RETURN result END MakeProcArg;

PROCEDURE MakeConstArg*(type: OXB.Type; val: INTEGER): Argument;
VAR result: ConstArg;
BEGIN NEW(result);  result.type := type; result.val := val
RETURN result END MakeConstArg;

PROCEDURE MakeGlobalArg(type: OXB.Type; modno, adr: INTEGER): Argument;
VAR result: GlobalArg;
BEGIN NEW(result);  result.type := type;  result.modno := modno;  result.adr := adr
RETURN result END MakeGlobalArg;

PROCEDURE MakeLocalArg(type: OXB.Type; index: INTEGER): Argument;
VAR result: LocalArg;
BEGIN NEW(result);  result.type := type;  result.index := index
RETURN result END MakeLocalArg;

PROCEDURE MakeParamArg(type: OXB.Type; index: INTEGER): Argument;
VAR result: ParamArg;
BEGIN NEW(result);  result.type := type;  result.index := index
RETURN result END MakeParamArg;

PROCEDURE MakeParIndArg(type: OXB.Type; index: INTEGER): Argument;
VAR result: ParIndArg;
BEGIN NEW(result);  result.type := type;  result.index := index
RETURN result END MakeParIndArg;


PROCEDURE MakeArgument*(y: OXB.Object; curlev: INTEGER): Argument;
VAR modno: INTEGER;  result: Argument;
BEGIN
  IF y.lev < 0 THEN modno := -y.lev ELSE modno := 0 END;
  IF y.class = Const THEN
    IF    y.type.form = OXB.String THEN result := MakeStringArg(y.val, y.lev)
    ELSIF y.type.form = OXB.Proc   THEN result := MakeProcArg(modno, y.val)
    ELSE                                result := MakeConstArg(y.type, y.val)
    END
  ELSIF y.class = Var    THEN result := MakeGlobalArg(y.type, modno, y.val)
  ELSIF y.class = Local  THEN result := MakeLocalArg (y.type, y.val)
  ELSIF y.class = Par    THEN result := MakeParamArg (y.type, y.val)
  ELSIF y.class = ParInd THEN result := MakeParIndArg(y.type, y.val)
  ELSE ASSERT(FALSE)
  END
RETURN result END MakeArgument;

PROCEDURE MakeRealArgument*(val: REAL): Argument;
RETURN MakeConstArg(OXB.realType, ORD(val)) END MakeRealArgument;

PROCEDURE MakeStringArgument*(len: INTEGER): Argument; (* copies string from ORS-buffer *)
VAR i, j, adr: INTEGER; c1, c2: BYTE;
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
  END
RETURN MakeStringArg(adr, len) END MakeStringArgument;




(* ---- Item construction ---- *)

PROCEDURE MakeItem*(VAR x: Item; y: OXB.Object; curlev: INTEGER);
VAR name: ARRAY 40 OF CHAR;
BEGIN
  H.ZeroFill(x);
  x.unique   := NextUnique();
  x.extclass := y.class;
  x.type     := y.type;
  x.val      := y.val;
  x.size     := y.type.size;
  x.readonly := y.rdo;
  x.modno    := 0;
  IF (y.class = Const) & (y.type.form = OXB.String) THEN (* String length *)
    x.size := y.lev
  ELSIF y.lev < 0 THEN
    x.modno := -y.lev
  END;
  IF (y.class = Var) & (y.lev <= 0) THEN x.extclass := Global END;
  IF (y.lev > 0) & (y.lev # curlev) & (y.class # Const) THEN
    ORS.Mark("not accessible ")
  END;


  (* Describe item *)
  wir(x.unique, 4);  ws(": ");  wClass(y.class, 6);
  IF y.type = NIL THEN ws("(NIL)           ")
  ELSE
    wForm(y.type.form, 8);
    IF y.type.ref = 0 THEN ws("        ");
    ELSE                   wForm(y.type.ref, 8)
    END
  END;
  name := "  '";  H.Append(y.name, name);  H.Append("'", name);  wsl(name, 20, " ");
  IF y.class = OXB.Const THEN
    IF y.type.form = OXB.Proc THEN
      IF y.lev >= 0 THEN ws(" module procedure at "); wh(y.val); wsn("H.");
      ELSE               ws(" imported procedure: modno "); wi(-y.lev);
                         ws(", impno "); wi(y.val); wsn(".");
      END
    ELSIF y.type.form = OXB.String THEN
      IF y.lev >= 0 THEN ws(" module string at "); wh(y.val); wsn("H.");
      ELSE               ws(" imported string: modno "); wi(-y.lev);
                         ws(", impno?/offset? "); wi(y.val); wsn(".");  (* Not yet implemented *)
      END
    ELSE
      ws(" constant value "); wh(y.val); wsn("H.")
    END
  ELSIF y.class = OXB.Local THEN
    ws(" local "); wi(y.val); wsn(".")
  ELSIF y.class = OXB.Var THEN
    ASSERT(y.lev <= 0);  (* Local variables are OXB.Local *)
    IF    y.lev = 0 THEN ws(" module var at "); wh(y.val); wsn("H.");
    ELSIF y.type.form = OXB.Proc THEN ws(" imported procedure var: modno "); wi(-y.lev);
                                      ws(", impno "); wi(y.val); wsn(".");
    ELSE                              ws(" imported var: modno "); wi(-y.lev);
                                      ws(", impno "); wi(y.val); wsn(".");
    END
  ELSIF y.class = OXB.Par THEN
    ASSERT((y.type.form # OXB.Record) & ((y.type.form # OXB.Array) OR (y.type.len >= 0)));
    ws(" value parameter "); wi(y.val); wsn(".")
  ELSIF y.class = OXB.ParInd THEN
    ws(" VAR parameter "); wi(y.val); wsn(".")
  ELSE
    IF    y.class = 0         THEN wsn(" class 0")
    ELSIF y.class = OXB.SProc THEN ws (" system procedure # "); wi(y.val); wsn(".")
    ELSIF y.class = OXB.Typ   THEN ws (" type # "); wi(y.val); wsn(".")
    END;
    ASSERT(y.class IN {0, OXB.SProc, OXB.Typ})
  END
END MakeItem;


PROCEDURE IsCodeSection(x: Item): BOOLEAN;  (* this module or imported module *)
RETURN (x.extclass = Const) & (x.type.form IN {OXB.Proc, OXB.String, OXB.Typ})
END IsCodeSection;


PROCEDURE MakeConstItem*(VAR x: Item;  t: OXB.Type;  v: INTEGER);
BEGIN
  H.ZeroFill(x);
  x.unique   := NextUnique();
  x.extclass := Const;
  x.type     := t;
  x.val      := v;
  x.size     := 8;
  wir(x.unique, 4); ws(": Constant, value "); wh(v);
  ws("H, type ");
  IF t = NIL THEN ws("(NIL)")
  ELSE
    ws("(form "); wForm(t.form, 1); ws(", Ref "); wForm(t.ref, 1); ws(")")
  END;
  wn;
END MakeConstItem;

PROCEDURE MakeRealItem*(VAR x: Item; v: REAL);
BEGIN
  H.ZeroFill(x);
  x.unique   := NextUnique();
  x.extclass := Const;
  x.type     := OXB.realType;
  x.val      := ORD(v);
  x.size     := 8;
  wir(x.unique, 4); ws(": Real constant, value "); wh(x.val); wsn("H.");
END MakeRealItem;

PROCEDURE MakeStringItem*(VAR x: Item; len: INTEGER); (* copies string from ORS-buffer *)
VAR i, j, a1: INTEGER; c1, c2: BYTE;
BEGIN
  H.ZeroFill(x);
  x.unique   := NextUnique();
  x.extclass := Const;
  x.type     := OXB.strType;
  x.val      := TextAdr;
  x.size     := len;
  wir(x.unique, 4); ws(": String constant: '");
  i := 0;  WHILE (i < len) & (ORS.str[i] # 0X) DO wc(ORS.str[i]);  INC(i)  END;
  wsn("'.");

  (* Check whether string already made *)
  i := 0;  j := 0;
  WHILE i < StrCnt DO
    a1 := Strings[i];  j := 0;
    WHILE (j < len) & (Text[a1 + j] = ORD(ORS.str[j])) DO INC(j) END;
    IF j = len THEN i := StrCnt ELSE INC(i) END
  END;
  IF j = len THEN
    x.val := a1;
    INC(SavedStringDup, len);
    INC(DupStrings);
  ELSE  (* Copy string into text section *)
    x.val := TextAdr;
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
      Strings[StrCnt] := x.val;  INC(StrCnt)
    END;
    INC(StringBytes, len)
  END
END MakeStringItem;



(* ---- Item conversion ---- *)

PROCEDURE StrToChar*(VAR x: Item);
BEGIN
  ws("StrToChar(");  wi(x.unique); wsn(")");
  x.type := OXB.charType;
  x.val := Text[x.val];
  x.size := 1;
END StrToChar;


(* ------------------------------- Operations ------------------------------- *)

PROCEDURE IsSigned*(x: OXB.Type): BOOLEAN;  (* returns whether x represents a signed integer value *)
BEGIN RETURN (x.ref >= OXB.Int8) & (x.ref <= OXB.Int64) END IsSigned;

PROCEDURE OpnumToIntCondition(opnum: INTEGER; signed: BOOLEAN): INTEGER;
VAR result: INTEGER;
BEGIN
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

PROCEDURE Operation(opnum: INTEGER; VAR x: Item; y: Item);
VAR op: POINTER TO OpDesc;
BEGIN
  wOp(opnum, 1);
  IF x.extclass # 0 THEN
    ws("("); wi(x.unique);
    IF y.extclass # 0 THEN ws(", "); wi(y.unique) END;
    ws(")");
    ASSERT((opnum = OpSetCC) OR (x.extclass # Cond))
  END;

  IF opnum = OpEnter THEN Internum := 0;  Labelnum := 1  END;

  NEW(op);  op.opnum := opnum;
  IF x.extclass # 0  THEN
    NEW(op.x);  op.x^ := x;
    IF y.extclass # 0  THEN
      NEW(op.y);  op.y^ := y
    END;
  END;
  op.result := -1;
  IF (opnum >= OpAbs) & (opnum <= OpIndex) THEN
    x.extclass := Reg;
    x.val      := NextIntermediate();
    x.size     := 8;
    op.result  := x.val;
  ELSIF (opnum >= OpIntEql) & (opnum <= OpStringGeq) THEN
    x.extclass  := Cond;
    x.condition := OpnumToIntCondition(opnum, IsSigned(x.type));
  ELSIF opnum = OpTestNZ THEN
    x.extclass  := Cond;
    x.condition := CNZ;
  END;
  IF Ops # NIL THEN Ops.next := op END;  Ops := op;
  IF op.result >= 0 THEN ws(" -> "); wi(op.result) END;
  wn
END Operation;

PROCEDURE Monadic*(op: INTEGER; VAR x: Item);
BEGIN
  IF (x.extclass = Const) & (op >= OpAbs) & (op <= OpInvert) THEN
    IF    op = OpAbs          THEN x.val := ABS(x.val)
    ELSIF op = OpSetSingleton THEN x.val := LSL(1, x.val)
    ELSIF op = OpNegate       THEN x.val := - x.val
    ELSIF op = OpRealNegate   THEN ASSERT(FALSE) (*x.val := ORD(-SYSTEM.VAL(REAL, x.val))*)
    ELSIF op = OpInvert       THEN x.val := ORD(-SYSTEM.VAL(SET, x.val))
    END
  ELSE
    Operation(op, x, NilItem)
  END
END Monadic;

PROCEDURE Dyadic*(op: INTEGER; VAR x: Item; y: Item);
BEGIN
  IF (x.extclass = Const) & (y.extclass = Const)
   & (op >= OpLSL) & (op <= OpRealDivide) THEN
    IF    op = OpLSL          THEN x.val := LSL(x.val, y.val)
    ELSIF op = OpASR          THEN x.val := ASR(x.val, y.val)
    ELSIF op = OpROR          THEN x.val := ROR(x.val, y.val)
    ELSIF op = OpADC          THEN ASSERT(FALSE) (*x.val := x.val y.val*)
    ELSIF op = OpSBC          THEN ASSERT(FALSE) (*x.val := x.val y.val*)
    ELSIF op = OpUML          THEN ASSERT(FALSE) (*x.val := x.val y.val*)
    ELSIF op = OpBit          THEN ASSERT(FALSE) (*x.val := SYSTEM.BIT(x.val, y.val)*)
    ELSIF op = OpSetRange     THEN x.val := ORD({x.val..y.val})
    ELSIF op = OpSetOr        THEN x.val := ORD(SYSTEM.VAL(SET,x.val) + SYSTEM.VAL(SET,y.val))
    ELSIF op = OpSetAndNot    THEN x.val := ORD(SYSTEM.VAL(SET,x.val) - SYSTEM.VAL(SET,y.val))
    ELSIF op = OpSetAnd       THEN x.val := ORD(SYSTEM.VAL(SET,x.val) * SYSTEM.VAL(SET,y.val))
    ELSIF op = OpSetXor       THEN x.val := ORD(SYSTEM.VAL(SET,x.val) / SYSTEM.VAL(SET,y.val))
    ELSIF op = OpIntPlus      THEN x.val := x.val + y.val
    ELSIF op = OpIntMinus     THEN x.val := x.val - y.val
    ELSIF op = OpIntMultiply  THEN x.val := x.val * y.val
    ELSIF op = OpIntDivide    THEN x.val := x.val DIV y.val
    ELSIF op = OpIntModulus   THEN x.val := x.val MOD y.val
    ELSIF op = OpRealPlus     THEN ASSERT(FALSE) (*x.val := x.val y.val*)
    ELSIF op = OpRealMinus    THEN ASSERT(FALSE) (*x.val := x.val y.val*)
    ELSIF op = OpRealMultiply THEN ASSERT(FALSE) (*x.val := x.val y.val*)
    ELSIF op = OpRealDivide   THEN ASSERT(FALSE) (*x.val := x.val y.val*)
    END
  ELSE
    Operation(op, x, y)
  END
END Dyadic;


PROCEDURE Header*;
BEGIN
  LocalCnt := 0;
  Dyadic(OpEnter, NilItem, NilItem);
  Body := Ops
END Header;

PROCEDURE Enter*(proc: OXB.Object);
VAR obj: OXB.Object;
BEGIN
  LocalCnt := 0;
  IF proc # NIL THEN  (* Initialise locals array from parameters and local vars *)
    obj := proc.type.dsc;
    WHILE obj # NIL DO
      IF obj.class IN {OXB.Local, OXB.Par, OXB.ParInd} THEN
        IF LocalCnt >= LEN(Locals) THEN ORS.Mark("Too many locals"); LocalCnt := 0 END;
        Locals[LocalCnt].obj   := obj;
        Locals[LocalCnt].reg   := -1;
        Locals[LocalCnt].stack := -1;
        INC(LocalCnt)
      END;
      obj := obj.next;
    END
  END;
  Dyadic(OpEnter, NilItem, NilItem);
  Body := Ops
 END Enter;


PROCEDURE Generate*(proc: OXB.Object);  (* Analyse and generate code starting at Body *)
VAR inst: Pseudocode;  obj: OXB.Object;
BEGIN
  IF proc # NIL THEN
    ws("Generate procedure '");  ws(proc.name);  wsn("' body.");
    obj := proc.type.dsc;
  ELSE
    ws("Generate module '");  ws(Modid);  wsn("' body.");
    obj := OXB.topScope.next;
  END;
  IF obj # NIL THEN
    wsn("Variables:");
    WHILE obj # NIL DO
      IF obj.class IN {OXB.Local, OXB.Var, OXB.Par, OXB.ParInd} THEN
        whr(obj.val, 3, " ");
        IF    obj.class = OXB.Local  THEN ws("H: Local VAR   ")
        ELSIF obj.class = OXB.Var    THEN ws("H: Module VAR  ")
        ELSIF obj.class = OXB.Par    THEN ws("H: Value parm  ")
        ELSIF obj.class = OXB.ParInd THEN
          IF obj.rdo                 THEN ws("H: ro-ref parm ")
                                     ELSE ws("H: rw-ref parm ")
          END
        END;
        ws(obj.name);  ws(", size ");  wi(obj.type.size);
        IF obj.type.base # NIL THEN
          ws(", base size ");  wi(obj.type.base.size)
        END;
        IF ((obj.class = OXB.ParInd) & (obj.type.form = OXB.Array)) THEN
          IF obj.type.len < 0 THEN ws(", open array") ELSE ws(", len ");  wi(obj.type.len) END
        END;
        wn
      END;
      obj := obj.next
    END
  END;

  (* Add intermediate locals to local array *)


  inst := Body;
  WHILE inst # NIL DO
    CASE inst OF
      Op:    ws("          ");
             wOp(inst.opnum, 13);
             IF inst.x # NIL THEN
               wItem(inst.x^);
               IF inst.y # NIL THEN ws(", ");  wItem(inst.y^) END;
               ws(" ")
             END;
             IF inst.result >= 0 THEN ws("-> "); wi(inst.result) END;
             wn
    | Label: ws("    l");  wil(inst.label, 5);  wsn("Label")
    | Jump:  ws("          ");
             IF inst.condition >= 0 THEN wCond(inst.condition) ELSE ws("Jmp") END;
             ws("          l");
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

PROCEDURE LoadFromCC(VAR x: Item);
(*            jxx    tchain
    fchain:   xor    eax,eax
              jmp    done
    tchain:   mov    eax,1
    done:                      *)
VAR op: POINTER TO OpDesc;  done: Jump;
BEGIN
  INC(Internum);
  IF (x.truechain = NIL) & (x.falsechain = NIL) THEN
    (* Can use X86 SetCC instruction *)
    Monadic(OpSetCC, x)
  ELSE
    JumpForward(x.truechain, x.condition);

    FixLink(x.falsechain);
    NEW(op);  Ops.next := op;  Ops := op;  op.opnum := OpLoad;
    op.x      := ConstItem0;
    op.result := Internum;
    JumpForward(done, -1);

    FixLink(x.truechain);
    NEW(op);  Ops.next := op;  Ops := op;  op.opnum := OpLoad;
    op.x      := ConstItem1;
    op.result := Internum;
    FixLink(done)
  END;

  x.extclass := Reg;
  x.val      := Internum;
  x.size     := 8
END LoadFromCC;

PROCEDURE InvertedCondition(cond: INTEGER): INTEGER;
BEGIN ASSERT(cond IN {0..15});
RETURN ORD(SYSTEM.VAL(SET, cond)/{0}) END InvertedCondition;

PROCEDURE CFJump*(VAR x: Item);
BEGIN
  ws("CFJump.");
  IF x.extclass # Cond THEN Dyadic(OpTestNZ, x, NilItem) END;
  JumpForward(x.falsechain, InvertedCondition(x.condition));
  FixLink(x.truechain);
END CFJump;

PROCEDURE FJump*(VAR p: Jump);
BEGIN JumpForward(p, -1) END FJump;

PROCEDURE BJump*(p: Pseudocode);
BEGIN ASSERT(p # NIL);
  wsn("BJump.");
  JumpBack(-1, p)
  (*
  NEW(jump);  Ops.next := jump;  Ops := jump;
  jump.condition := -1;  jump.target := p;
  *)
END BJump;

PROCEDURE CBJump*(VAR x: Item; p: Label);
BEGIN ASSERT(p # NIL);  ASSERT(p IS Label);
  ws("CBJump.");
  IF x.extclass # Cond THEN Dyadic(OpTestNZ, x, NilItem) END;
  JumpBack(InvertedCondition(x.condition), p);
  (*
  NEW(jump);  Ops.next := jump;  Ops := jump;
  jump.condition := InvertedCondition(x.n);  jump.target := p;
  *)
  FixLink(x.truechain);
  FixChain(x.falsechain, p);
END CBJump;



PROCEDURE StartStatement*;
BEGIN Internum := 0 END StartStatement;


(* ---- Assignment ---- *)

PROCEDURE Store*(VAR dest, src: Item); (* dest := src *)
BEGIN
  IF src.extclass = Cond THEN LoadFromCC(src) END;
  Dyadic(OpStore, dest, src)
END Store;


PROCEDURE StoreStruct*(VAR x, y: Item);   (* x := y *)
BEGIN Dyadic(OpStoreStruc, x, y) END StoreStruct;


PROCEDURE CopyString*(VAR x, y: Item);  (* x := y *)
BEGIN Dyadic(OpCopyString, x, y) END CopyString;


(* ---- Paramter passing ---- *)

PROCEDURE ValueParam*(VAR x: Item;  hosted: BYTE; n: INTEGER);
BEGIN
  IF x.extclass = Cond THEN LoadFromCC(x) END;
  Dyadic(OpValueParam, x, NilItem)
END ValueParam;


PROCEDURE OpenArrParam*(VAR x: Item; hosted: BYTE);
(* x being passed to open array formal parameter *)
BEGIN Dyadic(OpOpenArrParam, x, NilItem) END OpenArrParam;


PROCEDURE ByteArrParam*(VAR x: Item;  hosted: BYTE);
(* Any type x being passed to ARRAY OF BYTE formal parameter *)
BEGIN Dyadic(OpByteArrParam, x, NilItem) END ByteArrParam;


PROCEDURE VarParam*(VAR x: Item;  ftype: OXB.Type;  hosted: BYTE);
BEGIN Dyadic(OpVarParam, x, NilItem) END VarParam;


PROCEDURE StringParam*(VAR x: Item;  hosted: BYTE);
BEGIN Dyadic(OpStringParam, x, NilItem) END StringParam;


(* ---- Procedure / function calls ---- *)

PROCEDURE PrepCall*(VAR x: Item; VAR saved: SET);
BEGIN Dyadic(OpPrepCall, x, NilItem) END PrepCall;


PROCEDURE Call*(VAR x: Item; saved: SET);
BEGIN
  IF x.type.base.form = OXB.NoTyp THEN
    Dyadic(OpCallProc, x, NilItem)
  ELSE
    Dyadic(OpCallFn, x, NilItem)
  END
END Call;


PROCEDURE Return*(VAR x: Item);
  (*
  form:      INTEGER;
  VAR x:     Item;
  parmsize:  INTEGER;
  locsize:   INTEGER;
  hosted:    BYTE);
  *)
BEGIN
  IF x.extclass = Cond THEN LoadFromCC(x) END;
  Dyadic(OpReturn, x, NilItem)  (* Value returning only if x.extclass # 0 *)
END Return;


(* ---- Standard procedures --- *)

PROCEDURE Assert*(VAR x: Item);
VAR call: Item;
BEGIN
  IF x.extclass # Cond THEN Dyadic(OpTestNZ, x, NilItem) END;
  JumpForward(x.truechain, x.condition);
  FixLink(x.falsechain);

  (* call assertion failure trap *)
  x.extclass := OXB.Const;
  x.type     := OXB.procType;
  x.modno    := 1;
  x.val      := H.AssertionFailureProc;
  Dyadic(OpCallProc, x, NilItem);

  FixLink(x.truechain)
END Assert;

PROCEDURE Copy*(VAR x, y, z: Item);
BEGIN
  ws(",");  wi(y.unique);  ws(",");  wi(z.unique); wsn(")");
END Copy;


(* ---- Boolean operators ---- *)

PROCEDURE Not*(VAR x: Item);   (* x := ~x *)
VAR t: Jump;
BEGIN
  IF x.extclass # Cond THEN Dyadic(OpTestNZ, x, NilItem) END;
  x.condition := InvertedCondition(x.condition);
  t := x.falsechain;  x.falsechain := x.truechain;  x.truechain := t
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

PROCEDURE And1*(VAR x: Item);   (* x := x & *)
BEGIN
  wsn("And1.");
  IF x.extclass # Cond THEN Dyadic(OpTestNZ, x, NilItem) END;
  JumpForward(x.falsechain, InvertedCondition(x.condition));
  FixLink(x.truechain)
END And1;

PROCEDURE And2*(VAR x, y: Item);   (* x := x & y *)
BEGIN
  wsn("And2.");
  IF y.extclass # Cond THEN Dyadic(OpTestNZ, y, NilItem) END;
  x.falsechain := MergeChain(y.falsechain, x.falsechain);
  x.truechain  := y.truechain;
  x.condition  := y.condition;
END And2;

PROCEDURE Or1*(VAR x: Item);   (* x := x OR *)
BEGIN
  ws("Or1.");
  IF x.extclass # Cond THEN Dyadic(OpTestNZ, x, NilItem) END;
  JumpForward(x.truechain, x.condition);
  FixLink(x.falsechain)
END Or1;

PROCEDURE Or2*(VAR x, y: Item);   (* x := x OR y *)
BEGIN
  wsn("Or2.");
  IF y.extclass # Cond THEN Dyadic(OpTestNZ, y, NilItem) END;
  x.falsechain := y.falsechain;
  x.truechain  := MergeChain(y.truechain, x.truechain);
  x.condition  := y.condition;
END Or2;



(* ---- selection - fields, indices & pointers ---- *)

PROCEDURE Field*(VAR x: Item; y: OXB.Object);   (* x := x.y *)
BEGIN
  ASSERT(y.class = OXB.Fld);
  IF x.extclass = Par THEN x.extclass := Var END;
  INC(x.offset, y.val)
END Field;


(* ---- Control flow ---- *)

(* Use of Item.o fields for conditions:
*
*  o.n      - the condition code (uses symbol encodings ORS.eql .. ORS.geq)
*  o.offset - address of prev instr if any needing patching with curr target
*  o.index  - address of any forward jump list targeting this instruction
*)

PROCEDURE MarkLoopStart*(): Label;
VAR target: Label;
BEGIN
  wsn("MarkLoopStart");
  NEW(target);
  Ops.next := target;  Ops := target;
  target.label := Labelnum;  INC(Labelnum);
RETURN target END MarkLoopStart;

PROCEDURE CheckRegs*;
BEGIN
  wsn("CheckRegs.");
END CheckRegs;

(*
PROCEDURE FixOne*(at: INTEGER);
BEGIN
  wsn("FixOne.");
END FixOne;


PROCEDURE Last*(): Pseudocode;
RETURN Ops END Last;
*)


(*
FOR var := ctl TO lim BY inc DO ... END
For0:      w is immediate
           LoadReg(ctl)
           X64.Depar(var.o)

For1:  L0: Compare(ctl, lim);  FreeRegs(lim.o)
           ja/jb  L1
           StoreReg(var, ctl)  [Leaves any x & y reg usage unchanged]

           StatSeq

For2:      Dyadic(add, ctl, inc)

           jmp    L0
       L1:
*)



PROCEDURE For0*(VAR var, ctl: Item); (* ctl is initial value *)
BEGIN
  ws("For0(var ");  wi(var.unique);
  ws(", ctl ");     wi(ctl.unique);  wsn(")");
END For0;


PROCEDURE For1*(VAR var, ctl, lim, inc: Item; VAR l: Jump);
BEGIN
  ws("For1(var ");  wi(var.unique);
  ws(", ctl ");     wi(ctl.unique);
  ws(", lim ");     wi(lim.unique);
  ws(", inc ");     wi(inc.unique);  wsn(")");
END For1;


PROCEDURE For2*(VAR var, ctl, inc: Item);
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

PROCEDURE TypeTest*(VAR x: Item; T: OXB.Type; varpar, isguard: BOOLEAN);
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
  Varsize         := 0;
  VarFInx         := 0;
  ImpFInx         := 0;
END Open;


PROCEDURE SetDataSize*(dc: INTEGER);
BEGIN Varsize := dc END SetDataSize;


PROCEDURE Close*(modid: ORS.Ident; key, exno: INTEGER);
(* Note: export numbers are 1 .. exno-1 *)
VAR i: INTEGER;
BEGIN
  Dyadic(OpReturn, NilItem, NilItem);
  Generate(NIL);
  ws("String de-duplication saved "); wi(SavedStringDup);
  ws(" bytes in ");                   wi(DupStrings);
  ws(" strings leaving ");            wi(StrCnt);
  ws(" strings in ");                 wi(StringBytes); wsn(" bytes.");
  Ops := NIL;
  Body := NIL;
  FOR i := 0 TO LocalCnt-1 DO Locals[i].obj := NIL END;
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
  StringBytes    := `0;
  DupStrings := 0;
  NEW(ConstItem0);  MakeConstItem(ConstItem0^, OXB.intType, 0);
  NEW(ConstItem1);  MakeConstItem(ConstItem1^, OXB.intType, 1);
  NilItem.extclass := 0;
END OXG.
