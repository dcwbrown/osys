MODULE X64;  (* DCWB August 2023; X64 constants *)

IMPORT SYSTEM, H := Host, ORS, ORB;

CONST
  MaxPC* = 20000H;  (* Text generation buffer size (128KB) *)

  (* Named registers *)
  RAX* = 0;
  RCX* = 1;
  RDX* = 2;
  RBX* = 3;
  RSP* = 4;     (* X64 stack pointer *)
  RBP* = 5;
  RSI* = 6;
  RDI* = 7;

  (* Conditions *)
  CF*  = 0;     (* False, Never                         *)
  CT*  = 1;     (* True, always                         *)
  CO*  = 80H;   (* Overflow                             *)
  CNO* = 81H;   (* No overflow                          *)
  CC*  = 82H;   (* Carry, Below                         *)
  CNC* = 83H;   (* No carry, Above or equal             *)
  CZ*  = 84H;   (* Zero, Equal                          *)
  CNZ* = 85H;   (* Nonzero, Not equal                   *)
  CNA* = 86H;   (* Not above, Below or equal            *)
  CA*  = 87H;   (* Above, Not below or equal            *)
  CS*  = 88H;   (* Sign, Negative                       *)
  CNS* = 89H;   (* No sign, Positive or zero            *)
  CP*  = 8AH;   (* Parity                               *)
  CNP* = 8BH;   (* No parity                            *)
  CL*  = 8CH;   (* Less than, Not greater or equal      *)
  CNL* = 8DH;   (* Not less than, Greater or equal      *)
  CNG* = 8EH;   (* Not greater than, Less than or equal *)
  CG*  = 8FH;   (* Greater than, Not less than or equal *)

  (* Dyadic ALU operations, values correspond to x86 instruction set *)
  Plus*  = 00H;
  Or*    = 08H;
  And*   = 20H;
  Minus* = 28H;
  Xor*   = 30H;
  Cmp*   = 38H;

  (* X64 processor item representation *)

  (* Mode           Description         n                   offset                      *)
  (*                ------------------  ------------------  --------------------------- *)
  Cond*    = 20;  (* Condition          CF, CT, CO..CG      0                           *)
  Const*   = 21;  (* Constant           value               0                           *)
  Reg*     = 22;  (* Register           register (0-15)     0                           *)
  Strucp*  = 23;  (* Stack struc ptr    stack offset        0                           *)
  Stkind*  = 24;  (* Stack indirect     stack offset        0                           *)
  Eadr*    = 25;  (* Effective address  base register       relative to base            *)
  Code*    = 26;  (* Code offset        0                   code offset                 *)
  String*  = 27;  (* String offs & len  string length       current string block offset *)
  Global*  = 28;  (* VAR offset         0                   VAR block offset            *)
  Import*  = 29;  (* Import             16/modno, 16/impno  0                           *)
  Impcode* = 30;  (* Imported code var  16/modno, 16/impno  0                           *)

  (* Note re imported procedures: An imported procedure has Mode Import, while an *)
  (* imported procedure address variable has mode ImpCode.                        *)

TYPE
  Item* = RECORD
    mode*:     INTEGER;
    type*:     ORB.Type;
    readonly*: BOOLEAN;
    n*:        INTEGER;
    offset*:   INTEGER;
    index*:    INTEGER;
    scale*:    INTEGER
  END;

VAR
  PC*:     INTEGER;
  SPO*:    INTEGER;
  Text*:   ARRAY MaxPC OF BYTE;


(* -------------------------------- Assembly -------------------------------- *)

PROCEDURE ClearMode*(VAR x: Item);
BEGIN
  x.mode     := Const;
  x.n        := -1;
  x.offset   := 0;
  x.index    := -1;
  x.scale    := 0;
END ClearMode;

PROCEDURE ClearStack*; BEGIN SPO := 0 END ClearStack;

PROCEDURE AdjustStack*(delta: INTEGER);  (* delta is a count of quadwords *)
BEGIN INC(SPO, delta) END AdjustStack;

PROCEDURE Emit*(value: INTEGER);
BEGIN
  IF PC < LEN(Text) THEN
    Text[PC] := value;  INC(PC)
  ELSE
    ORS.Mark("Program too long")
  END
END Emit;

PROCEDURE SetPC*(pc: INTEGER); BEGIN PC := pc END SetPC;

PROCEDURE Align*(size: INTEGER);
BEGIN PC := (PC + size - 1) DIV size * size END Align;

PROCEDURE EmitBytes*(size, value: INTEGER);
BEGIN
  ASSERT(size IN {1, 2, 4, 8});
  IF PC + size <= LEN(Text) THEN
    SYSTEM.COPY(SYSTEM.ADR(value), SYSTEM.ADR(Text) + PC, size);
    INC(PC, size)
  ELSE
    ORS.Mark("Program too long")
  END
END EmitBytes;

PROCEDURE EmitString*(s: ARRAY OF CHAR);
VAR i: INTEGER;
BEGIN i := 0;
  WHILE (i < LEN(s)) & (s[i] # 0X) DO Emit(ORD(s[i])); INC(i) END;
  Emit(0)
END EmitString;

PROCEDURE IsSigned*(x: ORB.Type): BOOLEAN;  (* returns whether x represents a signed integer value *)
BEGIN RETURN (x.ref >= ORB.Int8) & (x.ref <= ORB.Int64) END IsSigned;

PROCEDURE Peek*(adr, size: INTEGER; signed: BOOLEAN): INTEGER;
VAR result: INTEGER;
BEGIN
  IF signed & (Text[adr+size-1] >= 128) THEN result := -1 ELSE result := 0 END;
  SYSTEM.COPY(SYSTEM.ADR(Text) + adr, SYSTEM.ADR(result), size)
RETURN result END Peek;

PROCEDURE Patch*(adr, size, value: INTEGER);
BEGIN
  ASSERT((adr >= 0) & (adr + size < MaxPC));
  ASSERT(size IN {1, 2, 4, 8});
  SYSTEM.COPY(SYSTEM.ADR(value), SYSTEM.ADR(Text) + adr, size)
END Patch;


PROCEDURE Init*;
BEGIN PC := 0;  SPO := 0
END Init;

BEGIN  Init
END X64.
