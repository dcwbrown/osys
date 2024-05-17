MODULE ORG;  (* DCWB Jun 2023 to Feb 2024; Code generator for X64 *)

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

IMPORT SYSTEM, H := WinHost, Files, Texts, ORS, ORB, X64, l := Listing, K := Kernel;

CONST
  WordSize* = ORB.WordSize;
  MaxImpMod = 32;     (* Max no. imported modules *)

     (* Trace flags *)
  TraceAny*      = 0;
  TraceMakeItem* = 1;

  (* Promote X64 conditions to ORG *)
  CF  = X64.CF;   CT  = X64.CT;
  CO  = X64.CO;   CNO = X64.CNO;  CC  = X64.CC;   CNC = X64.CNC;
  CZ  = X64.CZ;   CNZ = X64.CNZ;  CNA = X64.CNA;  CA  = X64.CA;
  CS  = X64.CS;   CNS = X64.CNS;  CP  = X64.CP;   CNP = X64.CNP;
  CL  = X64.CL;   CNL = X64.CNL;  CNG = X64.CNG;  CG  = X64.CG;

  (* Promote X64 reg names to ORG *)
  RAX = X64.RAX;  RCX = X64.RCX;  RDX = X64.RDX;  RBX = X64.RBX;
  RSP = X64.RSP;  RBP = X64.RBP;  RSI = X64.RSI;  RDI = X64.RDI;

  AllFree* = {0..3,5..15};  (* RSP (4) is reserved *)

  (* Promote X64 item modes to ORG *)
  (* Mode                     Description        n                   offset                      *)
  (*                          -----------------  ------------------  --------------------------- *)
  Cond     = X64.Cond;     (* Condition          CF, CT, CO..CG      0                           *)
  Const    = X64.Const;    (* Constant           value               0                           *)
  Reg      = X64.Reg;      (* Register           register (0-15)     0                           *)
  Strucp   = X64.Strucp;   (* Stack struc ptr    stack offset        0                           *)
  Stkind   = X64.Stkind;   (* Stack indirect     stack offset        0                           *)
  Eadr     = X64.Eadr;     (* Effective address  base register       relative to base            *)
  Code     = X64.Code;     (* Code offset        0                   code offset                 *)
  String   = X64.String;   (* String offset      string length       current string block offset *)
  Global   = X64.Global;   (* VAR offset         0                   VAR block offset            *)
  Import   = X64.Import;   (* Import             16/modno, 16/impno  0                           *)
  Impcode  = X64.Impcode;  (* Imported code var  16/modno, 16/impno  0                           *)

  (* Dyadic ALU operations, values correspond to x86 instruction set *)
  Plus*  = X64.Plus;
  Or*    = X64.Or;
  And*   = X64.And;
  Minus* = X64.Minus;
  Xor*   = X64.Xor;
  Cmp*   = X64.Cmp;

TYPE
  Item = X64.Item;

  LineAnnotation = POINTER TO LineAnnotationDesc;
  LineAnnotationDesc = RECORD
    next: LineAnnotation;
    line: INTEGER;
    pc:   INTEGER
  END;

  BodyAnnotation* = POINTER TO BodyAnnotationDesc;
  BodyAnnotationDesc = RECORD
    next:      BodyAnnotation;
    name:      ORS.Ident;
    firstline: LineAnnotation;
    lastline:  LineAnnotation
  END;

  ModuleDesc = H.ModuleDesc;

  (* Temporary new code header
  ModuleDesc* = RECORD-
    length*:   SYSTEM.CARD32;  (* File length *)
    initcode*: SYSTEM.CARD32;
    pointers*: SYSTEM.CARD32;
    commands*: SYSTEM.CARD32;
    exports*:  SYSTEM.CARD32;
    imports*:  SYSTEM.CARD32;  (* VARs start here following import resolution *)
    varsize*:  SYSTEM.CARD32;
    key*:      INTEGER;
  END;
  *)

VAR
  W:        Texts.Writer;
  Hdr*:     ModuleDesc;

  Str:      ARRAY 8192 OF CHAR;  (* Max bytes of string per module body *)
  Strx:     INTEGER;
  StrFix:   ARRAY 512 OF SYSTEM.CARD32;  (* Code offsets needing fixup *)
  StrFInx:  INTEGER;

  Varsize*: INTEGER;      (* Module global VAR size *)
  VarFix:   ARRAY 1024 OF SYSTEM.CARD32;
  VarFInx:  INTEGER;

  ImpFix:   ARRAY 2048 OF RECORD offset, fix: SYSTEM.CARD32 END;
  ImpFInx:  INTEGER;

  Free*:    SET;      (* Free registers *)

  BodyAnnotations: BodyAnnotation;


(* -------------------------------- Tracing --------------------------------- *)

  Trflags:  SET;      (* Listing trace options *)
  Tr*:      l.Buffer;
  Disasmpc: INTEGER;


PROCEDURE wn; BEGIN Texts.WriteLn(W); Texts.Append(Texts.Log, W.buf) END wn;

PROCEDURE ws (s: ARRAY OF CHAR); BEGIN Texts.WriteString(W, s)     END ws;
PROCEDURE wsn(s: ARRAY OF CHAR); BEGIN Texts.WriteString(W, s); wn END wsn;
PROCEDURE wh (i: INTEGER);       BEGIN Texts.WriteHex(W, i)        END wh;
PROCEDURE wi (i: INTEGER);       BEGIN Texts.WriteInt(W, i, 1)     END wi;
PROCEDURE wir(i, n: INTEGER);    BEGIN Texts.WriteInt(W, i, n)     END wir;


PROCEDURE Trace*(kind: INTEGER; str: ARRAY OF CHAR);
BEGIN
  IF ORS.ListCodegen & (kind IN Trflags) THEN
    l.StartRightCol;
    Tr.p := 0;  l.s(str, Tr)  (* Start trace line *)
  ELSE
    Tr.p := LEN(Tr.c) + 1  (* Suppress output on this line *)
  END
END Trace;

PROCEDURE Disassemble(mode: INTEGER; comment: ARRAY OF CHAR);
(* Disassemble up to X64.PC, or opcode 2EH, whichever comes first *)
BEGIN
  IF ORS.ListAssembly & (Disasmpc < X64.PC) & (X64.Text[Disasmpc] # 2EH) THEN
    IF mode = X64.Import THEN INC(mode, ImpFix[ImpFInx-1].fix * 256) END;
    l.DisassembleInstruction(Disasmpc, mode, comment);
    WHILE (Disasmpc < X64.PC) & (X64.Text[Disasmpc] # 2EH) DO
      l.DisassembleInstruction(Disasmpc, mode, "");
    END
  END;
  Disasmpc := X64.PC
END Disassemble;


(* ---------------------- Line number -> PC annotation ---------------------- *)

PROCEDURE NewBodyAnnotation*(name: ARRAY OF CHAR; VAR a: BodyAnnotation);
BEGIN
  IF BodyAnnotations = NIL THEN
    NEW(a);  BodyAnnotations := a
  ELSE
    a := BodyAnnotations;
    WHILE a.next # NIL DO a := a.next END;
    NEW(a.next);  a := a.next
  END;
  a.name := name;  a.firstline := NIL;  a.lastline := NIL;
END NewBodyAnnotation;

PROCEDURE AddLineAnnotation*(a: BodyAnnotation);
VAR la: LineAnnotation;
BEGIN
  IF a.firstline # NIL THEN ASSERT(a.lastline # NIL) END;
  IF (a.firstline = NIL)
  OR (a.lastline.line # ORS.linenum) & (a.lastline.pc # X64.PC) THEN
    NEW(la);  la.line := ORS.linenum;  la.pc := X64.PC;
    IF a.firstline = NIL THEN a.firstline := la ELSE a.lastline.next := la END;
    a.lastline := la
  END
END AddLineAnnotation;


(* --------------------------- Register tracking ---------------------------- *)

PROCEDURE ReserveReg*(r: INTEGER);
BEGIN ASSERT((r >= 0) & (r < 16) & (r # RSP));  EXCL(Free, r) END ReserveReg;

PROCEDURE Reserve(request: INTEGER; reject: SET): INTEGER;
BEGIN
  IF (request < 0) OR ~(request IN Free) THEN
    request := 0;  reject := reject + (-Free);
    WHILE (request < 16) & (request IN reject) DO INC(request) END;
  END;
  IF request < 16 THEN
    EXCL(Free, request)
  ELSE
    request := 15; ORS.Mark("out of registers")
  END
RETURN request END Reserve;

PROCEDURE Release(r: INTEGER);
BEGIN ASSERT((r >= 0) & (r # RSP)); INCL(Free, r) END Release;

PROCEDURE ClearRegs; BEGIN Free := AllFree END ClearRegs;

PROCEDURE CheckRegs*(msg: ARRAY OF CHAR);
BEGIN
  IF (Free # AllFree) OR (X64.SPO # 0) THEN
    l.s("CheckRegs(", Tr); l.s(msg, Tr);
    l.s("): Used ", Tr); l.Set(AllFree - Free, Tr);
    l.s(", Free: ", Tr); l.Set(Free, Tr);
    l.s(", SPO ",   Tr); l.i(X64.SPO, Tr); l.sn(".", Tr)
  END;
  IF Free # AllFree           THEN ORS.Mark("Reg Stack"); ClearRegs;      wn; H.Trap(0, "Free # AllFree")           END;
  IF X64.PC >= X64.MaxPC - 40 THEN ORS.Mark("program too long");          wn; H.Trap(0, "X64.PC >= X64.MaxPC - 40") END;
  IF X64.SPO # 0              THEN ORS.Mark("SPO error"); X64.ClearStack; wn; H.Trap(0, "X64.SPO # 0")              END
END CheckRegs;

PROCEDURE UsesReg(x: Item; reg: INTEGER): BOOLEAN;
VAR result: BOOLEAN;
BEGIN result := FALSE;
  IF x.mode IN {Reg, Eadr} THEN
    result := result OR (x.n = reg)
  END;
  IF x.mode IN {Stkind, Strucp, Eadr, String, Global, Import} THEN
    result := result OR (x.index = reg)
  END
RETURN result END UsesReg;

PROCEDURE FreeRegs(VAR x: Item);
BEGIN
  IF (x.mode IN {Reg, Eadr}) & (x.n # RSP) THEN Release(x.n) END;
  IF x.index >= 0 THEN Release(x.index) END;
  X64.ClearMode(x)
END FreeRegs;


(* ---------------------- Instruction prefix encoding ----------------------- *)

PROCEDURE Prefices*(mode, reg, size, n, index: INTEGER);
VAR rex: INTEGER;
BEGIN  rex := 0;
  IF ~(mode IN {Reg, Eadr}) THEN n := -1 END;  (* Cases where n is not a register *)
  (* Emit prefices *)
  IF size  =  2 THEN X64.Emit(66H) END;
  IF size  >  4 THEN INC(rex, 8)   END;
  IF reg   >= 8 THEN INC(rex, 4)   END;
  IF index >= 8 THEN INC(rex, 2)   END;
  IF n     >= 8 THEN INC(rex, 1)   END;

  IF (rex # 0) OR (size = 1) & (reg IN {4,5,6,7}) THEN X64.Emit(40H + rex) END;
END Prefices;


(* ----------------------- Effective address encoding ----------------------- *)

(*----  mod/reg/rm for effective addresses

  base  index  offset  mod  rm    s      i      b     disp
  ----  -----  ------  ---  ----  -----  -----  ----  -------
    N     N       N    0    5                         32/0
    N     N       Y    0    5                         32/offset
    N     Y       N    0    4     scale  index  5                  *1
    N     Y       Y    2    4     scale  index  base  32/offset    *1
    RSP   N       N    0    4     scale  4      4
    RBP   N       N    1    5                         8/0
    Y     N       N    0    base
    RSP   N      8/    1    4     0      4      4     8/offset
    RSP   N     32/    2    4     0      4      4     32/offset
    Y     N      8/    1    base                      8/offset
    Y     N     32/    2    base                      32/offset
    Y     Y       N    0    4     scale  index  base               *1
    Y     Y       Y    2    4     scale  index  base  32/offset    *1

*1 - there is no encoding for index = RSP.
*)

PROCEDURE ModRegRm*(mode, reg, n, index, scale, offset, immsize: INTEGER);
VAR
  needsib:  BOOLEAN;
  dispsize: INTEGER;
  base:     INTEGER;
  mod:      INTEGER;
  rm:       INTEGER;
  modno:    INTEGER;
BEGIN
  reg := reg MOD 8;

  IF mode = Reg THEN

    (* reg to reg *)
    ASSERT(n >= 0);  ASSERT(index < 0);  ASSERT(offset = 0);
    X64.Emit(0C0H + reg*8 + n MOD 8)

  ELSE

    (* reg from/to mem *)

    ASSERT(mode IN {Eadr, Code, String, Global, Import, Impcode});
    ASSERT(index # RSP);  (* Not supported by X64 *)

    IF mode IN {Import, Impcode} THEN
      ASSERT(n MOD 1000H > 0)  (* Import numbers start at 1 *)
    END;

    IF mode = Eadr THEN base := n MOD 8 ELSE base := -1 END;
    IF index >= 0 THEN index := index MOD 8 END;

    (* Determine presence and size of disp, ignoring special cases *)
    dispsize := 4;  (* Most cases including all fixup cases are 32 bit *)
    IF mode = Eadr THEN
      IF    offset = 0                        THEN dispsize := 0
      ELSIF (offset >= -80H) & (offset < 80H) THEN dispsize := 1
      END
    END;

    (* Determine mod based on disp size, ignoring special cases *)
    IF    dispsize = 0 THEN mod := 0
    ELSIF dispsize = 1 THEN mod := 1
    ELSE                    mod := 2
    END;

    (* Adjust for sib and special cases *)
    needsib := FALSE;
    rm := base;
    IF index < 0 THEN
      IF    base < 0                    THEN rm := 5;  dispsize := 4;  mod := 0
      ELSIF (base = RBP) & (offset = 0) THEN rm := 5;  dispsize := 1;  mod := 1
      ELSIF base = RSP                  THEN rm := 4;  needsib  := TRUE; index := 4;
      END;
    ELSE
      rm := 4;  needsib := TRUE;
      IF base < 0 THEN                   (* no base, with index and/or offset *)
        mod := 0;  dispsize := 4;  base := 5
      ELSIF (base = 5) & (mod = 0) THEN  (* Base is RBP/R13: must use mode with disp *)
        mod := 1;  dispsize := 1
      END
    END;

    X64.Emit(mod*64 + reg*8 + rm);          (* modregrm *)
    IF needsib THEN
      X64.Emit(scale*64 + index*8 + base);  (* sib *)
    END;

    (* Fixup Code/String/Global/Import offsets *)
    IF mode IN {Code, String, Global, Import, Impcode} THEN
      ASSERT(dispsize = 4);
      ASSERT(~needsib);  (* SIB addressing does not support rip relative disps *)
      DEC(offset, X64.PC + 4 + immsize)  (* Make offset relative to next instruction *)
    END;
    IF     mode = String   THEN StrFix[StrFInx] := X64.PC;  INC(StrFInx)
    ELSIF  mode = Global   THEN VarFix[VarFInx] := X64.PC;  INC(VarFInx)
    ELSIF (mode = Import)
       OR (mode = Impcode) THEN ImpFix[ImpFInx].offset := X64.PC;
                                ImpFix[ImpFInx].fix := n; INC(ImpFInx)
    END;

    IF dispsize = 1 THEN
      ASSERT((offset >= -80H) & (offset < 80H));
      X64.Emit(offset)
    ELSIF dispsize = 4 THEN
      ASSERT((offset >= -80000000H) & (offset < 80000000H));
      X64.EmitBytes(4, offset)
    ELSE
      ASSERT(dispsize = 0)
    END
  END
END ModRegRm;

PROCEDURE OpModRegRm(op, mode, reg, size, n, index, scale, offset, immsize: INTEGER);
VAR iprelreg: INTEGER;
BEGIN
  IF ~(mode IN {Reg, Eadr, Code, String, Global, Import, Impcode}) THEN
    ws("** mode "); wi(mode); wsn(" **");
    ORS.Mark("Unexpected x.mode")
  END;
  ASSERT(mode IN {Reg, Eadr, Code, String, Global, Import, Impcode});
  iprelreg := -1;
  IF (mode IN {Code, String, Global, Import, Impcode}) & (index >= 0) THEN
    (* Preload code (rip) relative address because SIB doesn't support rip relative *)
    iprelreg := Reserve(-1, {RBP});
    OpModRegRm(8DH, mode, iprelreg, 8, n, -1, 0, offset, 0); (* lea *)
    mode    := Eadr;
    n       := iprelreg;
    offset  := 0;
    immsize := 0;
  END;
  Prefices(mode, reg, size, n, index);
  IF op >= 100H THEN X64.Emit(op DIV 100H) END;
  X64.Emit(op MOD 100H);
  ModRegRm(mode, reg, n, index, scale, offset, immsize);
  IF iprelreg >= 0 THEN Release(iprelreg) END
END OpModRegRm;

PROCEDURE OpItem*(op, reg, size, immsize: INTEGER; x: Item);
VAR offset: INTEGER;
BEGIN
  offset := x.offset;
  IF (x.mode = Eadr) & (x.n = RSP) THEN INC(offset, X64.SPO * 8) END;
  OpModRegRm(op, x.mode, reg, size, x.n, x.index, x.scale, offset, immsize)
END OpItem;


(* ---------------------------- Code generation ----------------------------- *)

PROCEDURE SetCondition(VAR x: Item; cond: INTEGER);
BEGIN
  X64.ClearMode(x);
  ASSERT((cond >= CF) & (cond <= CG) & ((cond <= CT) OR (cond >= CO)));
  x.mode  := Cond;
  x.n     := cond;
  x.scale := 0;       (* prev instr needing patching to after this instruction *)
  x.index := 0;       (* prev instruction needing patch to this instruction *)
END SetCondition;

PROCEDURE DeparToReg(reg: INTEGER; VAR x: Item);
BEGIN
  ASSERT(x.mode IN {Stkind, Strucp});
  OpModRegRm(8BH, Eadr, reg, 8, RSP, -1, 0, x.n + 8 * X64.SPO, 0);  (* mov reg,[rsp+x] *)
  x.mode := Eadr;
  x.n    := reg;
  Disassemble(0, "Depar")
END DeparToReg;

PROCEDURE Depar(VAR x: Item);
BEGIN
  IF x.mode IN {Stkind, Strucp} THEN DeparToReg(Reserve(-1, {}), x) END
END Depar;


PROCEDURE LoadFlags(VAR x: Item);  (* Set Z condition flag based on x *)
BEGIN
  ASSERT(x.type.form = ORB.Bool);
  IF x.mode = Const THEN
    ASSERT((x.n = 0) OR (x.n = 1));
    SetCondition(x, x.n)
  ELSE
    Depar(x);
    IF x.mode = Reg THEN  (* use test reg,reg *)
      OpModRegRm(85H, Reg, x.n, 8, x.n, -1, 0, 0, 0)
    ELSE  (* use cmp mem,0 *)
      ASSERT(x.mode IN {Eadr, Code, String, Global, Import});
      ASSERT(x.type.size = 1);
      OpItem(80H, 7, x.type.size, 1, x);  X64.Emit(0)
    END;
    Disassemble(x.mode, "LoadFlags");
    FreeRegs(x);
    SetCondition(x, CNZ)
  END
END LoadFlags;


PROCEDURE InvertedCondition(cond: INTEGER): INTEGER;
BEGIN RETURN ORD(SYSTEM.VAL(SET, cond) / {0})
END InvertedCondition;


PROCEDURE LoadImmediate(reg, val: INTEGER);
BEGIN
  ASSERT(reg >= 0);
  IF val = 0 THEN
    OpModRegRm(31H, Reg, reg, 4, reg, -1, 0, 0, 0)  (* xor reg,reg *)
  ELSIF (val > 0) & (val < 100000000H) THEN   (* Load 32 bit positive value as 32 bit load with zero extension *)
    Prefices(Reg, -1, 4, reg, -1);
    X64.Emit(0B8H + reg MOD 8);
    X64.EmitBytes(4, val);
  ELSIF (val < 0) & (val >= -80000000H) THEN  (* Load 32 bit negative value with sign extended move *)
    OpModRegRm(0C7H, Reg, 0, 8, reg, -1, 0, 0, 4);
    X64.EmitBytes(4,val);
  ELSE                                        (* Need full 64 bit literal *)
    Prefices(Reg, reg, 8, -1, -1);
    X64.Emit(0B8H + reg MOD 8);
    X64.EmitBytes(8, val);
  END
END LoadImmediate;

PROCEDURE MoveReg(r1, r2: INTEGER);  (* r1 := r2 *)
BEGIN
  ASSERT(r1 >= 0);  ASSERT(r2 >= 0);  ASSERT(r1 # r2);
  OpModRegRm(89H, Reg, r2, 8, r1, -1, 0, 0, 0)
END MoveReg;

PROCEDURE LoadMem(reg, mode: INTEGER; signed: BOOLEAN; size, n, index, scale, offset: INTEGER);
BEGIN
  Trace(TraceAny, "LoadMem(");  l.Reg(8, reg, Tr);
  l.s(", ", Tr);         l.Mode(mode, Tr);
  l.s(", signed ", Tr);  l.Bool(signed, Tr);
  l.s(", size ", Tr);    l.i(size, Tr);
  l.s(", n ", Tr);       l.i(n, Tr);
  l.s(", index ", Tr);   l.i(index, Tr);
  l.s(", scale ", Tr);   l.i(scale, Tr);
  l.s(", offset ", Tr);  l.i(offset, Tr);  l.n(Tr);

  ASSERT(size IN {1, 2, 4, 8});
  IF signed THEN
    IF    size = 1 THEN OpModRegRm(0FBEH, mode, reg, 8, n, index, scale, offset, 0)  (* movsx  r64, r/m8  *)
    ELSIF size = 2 THEN OpModRegRm(0FBFH, mode, reg, 8, n, index, scale, offset, 0)  (* movsx  r64, r/m16 *)
    ELSIF size = 4 THEN OpModRegRm(63H,   mode, reg, 8, n, index, scale, offset, 0)  (* movsxd r64, r/m32 *)
    ELSIF size = 8 THEN OpModRegRm(8BH,   mode, reg, 8, n, index, scale, offset, 0)  (* mov    r64, r/m64 *)
    END
  ELSE (* unsigned *)
    IF    size = 1 THEN OpModRegRm(0FB6H, mode, reg, 8, n, index, scale, offset, 0)  (* movzx  r64, r/m8  *)
    ELSIF size = 2 THEN OpModRegRm(0FB7H, mode, reg, 8, n, index, scale, offset, 0)  (* movzx  r64, r/m16 *)
    ELSIF size = 4 THEN OpModRegRm(8BH,   mode, reg, 4, n, index, scale, offset, 0)  (* mov    r32, r/m32 *)
    ELSIF size = 8 THEN OpModRegRm(8BH,   mode, reg, 8, n, index, scale, offset, 0)  (* mov    r64, r/m64 *)
    END
  END
END LoadMem;


PROCEDURE FixLinkWith(L, target: INTEGER);
(*  Patch chain of 4 byte addresses starting at L.                         *)
(*  Chain continues at (L)                                                 *)
(*  Patch each address with offset (from byte following address) to target *)
VAR lprev, offset: INTEGER;
BEGIN
  Trace(TraceAny, "FixLinkWith: chain at $"); l.h(L, Tr);
  l.s(", target $", Tr); l.h(target, Tr); l.sn(".", Tr);
  WHILE L # 0 DO
    offset := target - (L+4);

    Trace(TraceAny, "-> Patch at $"); l.h(L, Tr);
    l.s(" currently $", Tr);         l.h(X64.Peek(L, 4, FALSE), Tr);
    l.s(", with offset $", Tr);      l.h(offset, Tr);
    l.s(", target: $", Tr);          l.h(target, Tr); l.sn(".", Tr);

    lprev := X64.Peek(L, 4, FALSE);
    X64.Patch(L, 4, offset);
    L := lprev
  END
END FixLinkWith;

PROCEDURE FixLink*(L: INTEGER);
BEGIN FixLinkWith(L, X64.PC) END FixLink;

PROCEDURE LoadCondition(reg: INTEGER; x: Item);  (* Set register to 0/1 based on condition flags *)
VAR s1, s2: INTEGER;
BEGIN s1 := 0; s2 := 0;
  ASSERT((reg >= 0) & (reg <= 15));
  (*
            setz   al
            jmp    skip
  fc:       mov    al,0        ; False chain points here
            jmp    c1
  tc:       mov    al,1        ; True chain points here
  skip:     and    eax,1
  *)
  OpModRegRm(0F90H + x.n MOD 16, Reg, 0, 4, reg, -1, 0, 0, 0);  (* setcc byte-reg *)
  IF x.offset # 0 THEN (* Terminate FALSE chain *)
    X64.Emit(0EBH);  X64.Emit(0);  s1 := X64.PC;  (* jmp skip *)
    FixLink(x.offset);
    IF reg >= 8 THEN X64.Emit(41H) END; X64.Emit(0B0H + reg MOD 8); X64.Emit(0) (* mov regb,0 *)
  END;
  IF x.index # 0 THEN (* Terminate True chain *)
    X64.Emit(0EBH);  X64.Emit(0);  s2 := X64.PC;  (* jmp skip *)
    FixLink(x.index);
    IF reg >= 8 THEN X64.Emit(41H) END; X64.Emit(0B0H + reg MOD 8); X64.Emit(1) (* mov regb,1 *)
  END;
  IF s1 # 0 THEN X64.Patch(s1 - 1, 1, X64.PC - s1) END;
  IF s2 # 0 THEN X64.Patch(s2 - 1, 1, X64.PC - s2) END;
  OpModRegRm(83H, Reg, And DIV 8, 4, reg, -1, 0, 0, 1);  X64.Emit(1);  (* and reg,1 *)
  Disassemble(0, "LoadCondition")
END LoadCondition;


PROCEDURE LoadRegUnreserved(reg: INTEGER; VAR x: Item);
VAR offset: INTEGER;
BEGIN
  IF x.mode IN {Stkind, Strucp} THEN DeparToReg(reg, x) END;
  IF x.mode = Cond THEN
    IF x.n < 2 THEN LoadImmediate(reg, x.n) ELSE LoadCondition(reg, x) END;
    x.index := -1;  x.offset := 0
  ELSIF x.mode = Const THEN
    LoadImmediate(reg, x.n)
  ELSIF x.mode = Reg THEN
    IF reg # x.n THEN MoveReg(reg, x.n); Release(x.n) END
  ELSIF x.mode IN {Eadr, Code, String, Global, Import} THEN
    offset := x.offset;
    IF (x.mode = Eadr) & (x.n = RSP) THEN INC(offset, X64.SPO * 8) END;
    LoadMem(reg, x.mode, X64.IsSigned(x.type), x.type.size, x.n, x.index, x.scale, offset)
  ELSE
    wn; ws("** LoadRegUnreserved, x.mode "); wi(x.mode); wsn(".");
    ASSERT(FALSE)
  END
END LoadRegUnreserved;


PROCEDURE LoadReg(reg: INTEGER; VAR x: Item);
BEGIN
  LoadRegUnreserved(reg, x);
  FreeRegs(x);  x.mode := Reg;  x.n := reg;  ReserveReg(x.n);
  Disassemble(x.mode, "Load")
END LoadReg;

PROCEDURE LoadAddressReg(reg: INTEGER; VAR x: Item);
VAR offset: INTEGER;
BEGIN
  IF x.mode IN {Stkind, Strucp} THEN DeparToReg(reg, x) END;
  offset := x.offset;
  IF (x.mode = Eadr) & (x.n = RSP) THEN INC(offset, 8 * X64.SPO) END;
  IF (x.mode = Eadr) & (x.index < 0) & (offset = 0) THEN
    IF reg # x.n THEN MoveReg(reg, x.n) END
  ELSIF (x.mode IN {Eadr, Code, String, Global, Import, Impcode}) THEN
    OpModRegRm(8DH, x.mode, reg, 8, x.n, x.index, x.scale, offset, 0);  (* lea *)
    Disassemble(x.mode, "LoadAddress")
  ELSE
    ASSERT(FALSE)
  END
END LoadAddressReg;

PROCEDURE LoadTypeAddress(reg: INTEGER; t: ORB.Type);
VAR x: Item;
BEGIN
  ASSERT(t.form = ORB.Record);
  IF t.mno > 0 THEN (* Import *)
    OpModRegRm(8DH, Import, reg, 8, t.mno * 10000H + t.len, -1, 0, 0, 0);  (* lea *)
    Disassemble(Import, "Load imported type address")
  ELSE (* Code relative *)
    OpModRegRm(8DH, Code, reg, 8, 0, -1, 0, t.len, 0);  (* lea *)
    Disassemble(Code, "Load local type address")
  END;
END LoadTypeAddress;

PROCEDURE LoadSize(reg: INTEGER; x: Item);  (* length in bytes *)
BEGIN
  IF x.mode = Strucp THEN
    ASSERT((x.type.form = ORB.Record) OR (x.type.form = ORB.Array) & (x.type.len < 0));
    (* Load length / type descriptor address *)
    LoadMem(reg, Eadr, FALSE, 8, RSP, -1, 0, x.n + 8 * X64.SPO + 8);
    IF x.type.form = ORB.Array THEN
      (* TODO multiply by element size *)
      IF x.type.base.size # 1 THEN ASSERT(FALSE) END;
    ELSIF x.type.form = ORB.Record THEN  (* Get record size from type descriptor *)
      LoadMem(reg, Eadr, FALSE, 8, reg, -1, 0, 0);   (* Load length from first qword of type descriptor *)
    END
  ELSIF x.type.form = ORB.Array THEN ASSERT(x.type.len >= 1);
    LoadImmediate(reg, x.type.len)
  ELSIF x.type.form = ORB.String THEN
    LoadImmediate(reg, x.n)
  ELSE
    LoadImmediate(reg, x.type.size)
  END
END LoadSize;


PROCEDURE Load(VAR x: Item);
VAR reg: INTEGER;
BEGIN
  IF x.mode # Reg THEN
    IF (x.mode = Eadr) & (x.n # RSP) THEN
      reg := x.n
    ELSE
      reg := Reserve(-1, {})
    END;
    IF (x.mode = Code) & (x.type.form = ORB.Proc) THEN
      LoadAddressReg(reg, x); FreeRegs(x);  x.mode := Reg;  x.n := reg;  ReserveReg(x.n);
    ELSE
      LoadReg(reg, x)
    END
  END
END Load;


PROCEDURE CallSysFn(sysfn: INTEGER);  (* Generates 6 bytes *)
BEGIN
  (*X64.Emit(0E8H);*)
  X64.Emit(0FFH);  X64.Emit(15H);  (* 00 010 101 *)  (* call [relative-address] *)
  ImpFix[ImpFInx].offset := X64.PC;
  ImpFix[ImpFInx].fix    := sysfn;  (* 16/module 0, 16/impno sysfn *)
  INC(ImpFInx);
  X64.EmitBytes(4, -(X64.PC + 4))  (* Offset from next instruction to module address 0 *)
END CallSysFn;


PROCEDURE DeRef*(VAR x: Item);
BEGIN Trace(TraceAny, "DeRef(x), x: "); l.Item(x, Tr); l.n(Tr);
  ASSERT(x.type.form = ORB.Pointer);
  IF ~(x.mode IN {Stkind, Eadr, Code, String, Global, Import}) THEN
    ORS.Mark("Variable expected");
  END;
  Load(x);  x.mode := Eadr;
  Disassemble(0, "DeRef");
  IF ~ORS.Hasty THEN
    OpModRegRm(85H, Reg, x.n, 8, x.n, -1, 0, 0, 0);  (* test reg,reg *)
    X64.Emit(75H);  X64.Emit(6);  (* jnz: if pointer not NIL *)
    CallSysFn(H.NilPointerDereferenceProc);
    Disassemble(0, "NIL pointer trap")
  END
END DeRef;


PROCEDURE LoadAddress(VAR x: Item);
VAR reg: INTEGER;
BEGIN
  IF x.mode = Reg THEN reg := x.n ELSE reg := Reserve(-1, {}) END;
  LoadAddressReg(reg, x);
  FreeRegs(x);  x.mode := Reg;  x.n := reg;  ReserveReg(x.n)
END LoadAddress;

PROCEDURE LoadStructure(rbase, rlength: INTEGER; VAR x: Item);  (* length returned as byte count *)
BEGIN
  ASSERT(x.type.form IN {ORB.Array, ORB.Record, ORB.String});
  ASSERT(x.mode IN {Stkind, Strucp, Eadr, Code, String, Global, Import});
  IF (x.type.form = ORB.Array) & (x.type.len < 0) THEN
    (* Open array parameter *)
    ASSERT(x.mode = Strucp);
    OpModRegRm(8BH, Eadr, rbase,   8, RSP, -1, 0, x.n + 8 * X64.SPO,     0);  (* mov reg,[rsp+x] *)
    OpModRegRm(8BH, Eadr, rlength, 8, RSP, -1, 0, x.n + 8 * X64.SPO + 8, 0);  (* mov reg,[rsp+x] *)
    ASSERT(x.type.base.size >= 0);
    IF x.type.base.size > 1 THEN  (* multiply length by type size to get byte count *)
      ASSERT(x.type.base.size < 80000000H);
      (* TODO - optimise powers of 2 with a shift *)
      ASSERT(FALSE);  (* Why does this code involve the value of x? Should just be rlen *= size *)
      IF x.type.base.size >= 80H THEN
        OpItem(69H, rlength, 8, 4, x);  X64.EmitBytes(4, x.type.base.size)
      ELSE
        OpItem(6BH, rlength, 8, 1, x);  X64.Emit(x.type.base.size)
      END;
    END
  ELSE
    (* Record or array with length known at compile time, or string *)
    LoadAddressReg(rbase, x);
    IF x.type.form = ORB.String THEN
      LoadImmediate(rlength, x.n)
    ELSIF x.type.form = ORB.Array THEN
      LoadImmediate(rlength, x.type.len)   (* array length in bytes *)
    ELSE
      ASSERT(x.mode # Strucp);  (* TODO - get runtime type length *)
      LoadImmediate(rlength, x.type.size)  (* record size in bytes *)
    END
  END;
  FreeRegs(x);
  x.mode := Eadr;  x.n := rbase;  x.index := rlength;
  ReserveReg(rbase);   ReserveReg(rlength);
  Disassemble(x.mode, "LoadStructure")
END LoadStructure;


PROCEDURE StoreRegToMem*(sreg, mode, size, n, index, scale, offset: INTEGER);
VAR op: INTEGER;
BEGIN
  IF size = 1 THEN op := 088H ELSE op := 089H END;
  OpModRegRm(op, mode, sreg, size, n, index, scale, offset, 0);
  (*Disassemble(mode, "StoreRegToMem")*)
END StoreRegToMem;

PROCEDURE StoreReg(reg: INTEGER; VAR x: Item);
BEGIN
  ASSERT((reg >= 0) & (reg <= 15));
  (*
  Trace(TraceAny, "StoreReg(reg: "); l.Reg(8, reg, Tr);
  l.s(", x: ", Tr);                 l.Item(x, Tr); l.sn(")", Tr);
  *)
  StoreRegToMem(reg, x.mode, x.type.size, x.n, x.index, x.scale, x.offset);
  Disassemble(x.mode, "StoreReg")
END StoreReg;

PROCEDURE StoreImmediateToMem(imm, mode, size, n, index, scale, offset: INTEGER);
VAR op, reg, immsize: INTEGER;
BEGIN
  IF (imm >= -80000000H) & (imm < 80000000H) THEN  (* imm fits 32 bits *)
    IF size = 1 THEN op := 0C6H ELSE op := 0C7H END;
    immsize := H.Min(size, 4);
    OpModRegRm(op, mode, 0, size, n, index, scale, offset, immsize);
    X64.EmitBytes(immsize, imm);
    Disassemble(mode, "StoreImmediate")
  ELSE  (* 64 bit immediate - load immediate then store reg *)
    reg := Reserve(-1, {});
    LoadImmediate(reg, imm);
    StoreRegToMem(reg, mode, size, n, index, scale, offset);
    Release(reg)
  END
END StoreImmediateToMem;

PROCEDURE StoreImmediate(imm: INTEGER; VAR x: Item);
VAR offset: INTEGER;
BEGIN
  offset := x.offset;
  IF (x.mode = Eadr) & (x.n = RSP) THEN INC(offset, X64.SPO * 8) END;
  StoreImmediateToMem(imm, x.mode, x.type.size, x.n, x.index, x.scale,  offset);
END StoreImmediate;

(*----*)

PROCEDURE PushReg*(r: INTEGER);
BEGIN Prefices(Reg, r, 4, -1, -1); X64.Emit(50H + r MOD 8) END PushReg;

PROCEDURE PopReg*(r: INTEGER);
BEGIN Prefices(Reg, r, 4, -1, -1); X64.Emit(58H + r MOD 8) END PopReg;

PROCEDURE PushImmediate*(i: INTEGER);
VAR reg: INTEGER;
BEGIN
  IF (i >= -80H) & (i < 80H) THEN
    X64.Emit(6AH);  X64.Emit(i)
  ELSIF (i >= -80000000H) & (i < 80000000H) THEN
    X64.Emit(68H);  X64.EmitBytes(4, i)
  ELSE
    reg := Reserve(-1, {});
    Prefices(Reg, reg, 8, -1, -1); X64.Emit(0B8H + reg MOD 8); X64.EmitBytes(8, i);  (* Load literal *)
    Prefices(Reg, reg, 4, -1, -1); X64.Emit(050H + reg MOD 8);                       (* Push reg *)
    Release(reg)
  END
END PushImmediate;

PROCEDURE PushMem*(mode: INTEGER; signed: BOOLEAN; size, n, index, scale, offset: INTEGER);
VAR reg: INTEGER;
BEGIN
  IF size < 8 THEN
    reg := Reserve(-1, {});
    LoadMem(reg, mode, signed, size, n, index, scale, offset);
    PushReg(reg);
    Release(reg)
  ELSE
    OpModRegRm(0FFH, mode, 6, 8, n, index, scale, offset, 0)
  END
END PushMem;


PROCEDURE PushAdr*(VAR x: Item);
VAR reg: INTEGER;
BEGIN
  IF (x.mode IN {Stkind, Strucp}) & (x.index < 0) & (x.offset = 0) THEN
    PushMem(Eadr, FALSE, 8, RSP, -1, 0, x.n + 8 * X64.SPO)
  ELSIF (x.mode = Eadr) & (x.index < 0) & (x.offset = 0) & ((x.n # RSP) OR (X64.SPO = 0)) THEN
    PushReg(x.n)
  ELSIF x.mode IN {Stkind, Strucp, Eadr, Code, String, Global, Import} THEN
    reg := Reserve(-1, {});
    LoadAddressReg(reg, x);  PushReg(reg);
    Release(reg);
  ELSE
    ASSERT(FALSE)
  END
END PushAdr;

PROCEDURE PushTypeAdr*(VAR x: Item);
VAR reg: INTEGER; type: ORB.Type;
BEGIN
  IF x.mode = Strucp THEN
    ASSERT(x.type.form = ORB.Record);
    PushMem(Eadr, FALSE, 8, RSP, -1, 0, x.n + 8 * X64.SPO + 8)  (* descriptor address *)
  ELSE
    IF x.type.form = ORB.Pointer THEN type := x.type.base ELSE type := x.type END;
    ASSERT(type.form = ORB.Record);
    reg := Reserve(-1, {});
    LoadTypeAddress(reg, type);
    PushReg(reg);  (* Pass record descriptor *)
    Release(reg)
  END
END PushTypeAdr;

PROCEDURE PushLength(x: Item);  (* Push element count of array *)
BEGIN ASSERT(x.type.form = ORB.Array);
  IF x.mode = Strucp THEN ASSERT(x.type.len < 0);
    PushMem(Eadr, FALSE, 8, RSP, -1, 0, x.n + 8 * X64.SPO + 8);
  ELSIF x.type.form = String THEN ASSERT(FALSE);
    PushImmediate(x.n)
  ELSE ASSERT(x.type.len >= 0);
    PushImmediate(x.type.len)
  END
END PushLength;

PROCEDURE PushSize(x: Item);  (* push size in bytes *)
VAR reg: INTEGER;
BEGIN
  IF (x.mode = Strucp) & (x.type.form = ORB.Array) THEN
    ASSERT(x.type.len < 0);
    IF x.type.base.size = 1 THEN
      PushMem(Eadr, FALSE, 8, RSP, -1, 0, x.n + 8 * X64.SPO + 8)
    ELSE (* element count from parameter *)
      reg := Reserve(-1, {});
      IF x.type.base.size < 80H THEN
        OpModRegRm(6BH, Eadr, reg, 8, RSP, -1, 0, x.n + 8 * X64.SPO + 8, 1);
        X64.Emit(x.type.base.size)
      ELSE
        OpModRegRm(69H, Eadr, reg, 8, RSP, -1, 0, x.n + 8 * X64.SPO + 8, 1);
        X64.EmitBytes(4, x.type.base.size)
      END;
      PushReg(reg);
      Release(reg)
    END
    (* Note, cannot determine dynamic accurate record size at run time:
       The type desciptor size field is the (larger) allocation size. *)
  ELSIF x.type.form = ORB.Array THEN ASSERT(x.type.len >= 1);
    PushImmediate(x.type.len * x.type.base.size)
  ELSIF x.type.form = String THEN
    PushImmediate(x.n)
  ELSE
    PushImmediate(x.type.size)
  END
END PushSize;


PROCEDURE Push*(VAR x: Item);
VAR reg, offset: INTEGER;
BEGIN
  IF    (x.mode = Code) & (x.type.form = ORB.Proc) THEN PushAdr(x)
  ELSIF x.mode = Const THEN PushImmediate(x.n)
  ELSIF x.mode = Reg   THEN PushReg(x.n)
  ELSIF x.mode = Cond THEN
    IF x.n < 2 THEN PushImmediate(x.n) ELSE Load(x);  PushReg(x.n) END
  ELSIF x.mode IN {Stkind, Strucp, Eadr, Code, String, Global, Import, Impcode} THEN
    Depar(x);
    offset := x.offset;
    IF (x.mode = Eadr) & (x.n = RSP) THEN INC(offset, X64.SPO * 8) END;
    PushMem(x.mode, X64.IsSigned(x.type), x.type.size, x.n, x.index, x.scale, offset)
  ELSE
    ASSERT(FALSE)
  END;
  FreeRegs(x)
END Push;

(*----*)


PROCEDURE AluOpFromImmediate(op, mode, imm, size, n, index, scale, offset: INTEGER); (* dest := dest op imm *)
VAR immsize, immreg, immlim, incdec: INTEGER;
BEGIN
  ASSERT(mode IN {Reg, Eadr, Code, String, Global, Import});
  IF mode = Reg THEN size := 8 END;
  IF ((imm = 1) OR (imm = -1)) & ((op = Plus) OR (op = Minus)) THEN
    incdec := 0;
    IF op = Minus THEN incdec := 1 END;
    IF imm = -1 THEN incdec := 1 - incdec END;
    OpModRegRm(0FFH, mode, incdec, size, n, index, scale, offset, 0)  (* inc/dec dest *)
  ELSE
    immsize := size; IF immsize > 4 THEN immsize := 4 END;
    immlim := LSL(1, immsize);
    IF (imm < -immlim) OR (imm >= immlim) THEN
      immreg := Reserve(-1, {});
      LoadImmediate(immreg, imm);                                                       (* mov immreg,imm64 *)
      OpModRegRm(op+1, mode, immreg, size, n, index, scale, offset, 0);                 (* op  dest,immreg  *)
      Release(immreg)
    ELSIF size = 1 THEN
      OpModRegRm(80H, mode, op DIV 8, size, n, index, scale, offset, 1);  X64.Emit(imm) (* op  dest8,imm8   *)
    ELSIF (imm >= -80H) & (imm < 80H) THEN
      OpModRegRm(83H, mode, op DIV 8, size, n, index, scale, offset, 1);  X64.Emit(imm) (* op  dest,imm8    *)
    ELSE
      OpModRegRm(81H, mode, op DIV 8, size, n, index, scale, offset, 4);                (* op  dest,imm<=32 *)
      IF size < 4 THEN X64.EmitBytes(size, imm) ELSE X64.EmitBytes(4, imm) END
    END
  END
END AluOpFromImmediate;

PROCEDURE AluOpToReg(  (* reg := reg op src *)
  op, mode, reg:                 INTEGER;
  signed:                        BOOLEAN;
  size, n, index, scale, offset: INTEGER);
VAR srcreg: INTEGER;
BEGIN
  ASSERT(mode IN {Reg, Eadr, Code, String, Global, Import});
  srcreg := -1;
  IF (mode # Reg) & (size < 8) THEN
    srcreg := Reserve(-1, {});
    LoadMem(srcreg, mode, signed, size, n, index, scale, offset);
    mode := Reg;  n := srcreg;  index := -1;  scale := 0;  offset := 0;
  END;
  OpModRegRm(op+3, mode, reg, 8, n, index, scale, offset, 0);
  IF srcreg >= 0 THEN Release(srcreg) END
END AluOpToReg;

PROCEDURE IsDyadicNoOp(op: INTEGER; src: Item): BOOLEAN;
VAR result: BOOLEAN;
BEGIN result := FALSE;
  IF src.mode = Const THEN
    IF src.n = 0 THEN
      result := (op = Plus) OR (op = Minus) OR (op = Or) OR (op = Xor)
    ELSIF src.n = -1 THEN
      result := op = And
    END
  END
RETURN result END IsDyadicNoOp;

PROCEDURE Dyadic(op: INTEGER;  VAR dest, src: Item);  (* dest := dest op src *)
(* Note: If result value needs to be a register, Load(dest) before calling Dyadic *)
VAR opname: ARRAY 6 OF CHAR;  offset: INTEGER;
BEGIN
  IF    op = Plus  THEN opname := "Plus"  ELSIF op = Minus THEN opname := "Minus"
  ELSIF op = Cmp   THEN opname := "Cmp"   ELSIF op = And   THEN opname := "And"
  ELSIF op = Or    THEN opname := "Or"    ELSIF op = Xor   THEN opname := "Xor"
  ELSE  ASSERT(FALSE)
  END;
  ASSERT(op IN {Plus, Minus, Cmp, And, Or, Xor});
  Trace(TraceAny, "Dyadic(");  l.Op(op, Tr);
  l.s("; dest: ", Tr);  l.Item(dest, Tr);
  l.s(", src: ",  Tr);  l.Item(src, Tr); l.n(Tr);

  IF (dest.mode = Reg) & (dest.n IN Free) THEN
    l.s("** dest register ", Tr); l.Reg(8, dest.n, Tr); l.sn(" not reserved.", Tr);
  END;
  IF (src.mode = Reg) & (src.n IN Free) THEN
    l.s("** src register ", Tr); l.Reg(8, src.n, Tr); l.sn(" not reserved.", Tr);
  END;

  IF (src.mode = Const) & (dest.mode = Const) THEN

    ASSERT(op # Cmp);

    IF    op = Plus  THEN INC(dest.n, src.n)
    ELSIF op = Minus THEN DEC(dest.n, src.n)
    ELSIF op = And   THEN dest.n := ORD(SYSTEM.VAL(SET,dest.n) * SYSTEM.VAL(SET,src.n));
    ELSIF op = Or    THEN dest.n := ORD(SYSTEM.VAL(SET,dest.n) + SYSTEM.VAL(SET,src.n));
    ELSIF op = Xor   THEN dest.n := ORD(SYSTEM.VAL(SET,dest.n) / SYSTEM.VAL(SET,src.n));
    END

  ELSIF ~IsDyadicNoOp(op, src) THEN

    IF src.mode = Const THEN
      Depar(dest);
      offset := dest.offset;
      IF (dest.mode = Eadr) & (dest.n = RSP) THEN INC(offset, X64.SPO * 8) END;
      AluOpFromImmediate(op, dest.mode, src.n, dest.type.size, dest.n, dest.index, dest.scale, offset);
      Disassemble(dest.mode, opname)
    ELSE
      Depar(src);
      ASSERT(src.mode IN {Reg, Eadr, Code, String, Global, Import});
      offset := src.offset;
      IF (src.mode = Eadr) & (src.n = RSP) THEN INC(offset, X64.SPO * 8) END;
      IF dest.mode # Reg THEN Load(dest) END;
      AluOpToReg(op, src.mode, dest.n, X64.IsSigned(src.type), src.type.size, src.n, src.index, src.scale, offset);
      Disassemble(src.mode, opname)
    END

  END;

  IF (dest.mode = Reg) & (dest.n IN Free) THEN
    l.s("** Warning, register dest.n (", Tr); l.Reg(8, dest.n, Tr); l.sn(") not reserved **", Tr)
  END
END Dyadic;


(* ---- Item construction ---- *)

PROCEDURE MakeItem*(VAR x: Item; y: ORB.Object; curlev: INTEGER);
VAR base: INTEGER;
BEGIN
  Trace(TraceMakeItem, "MakeItem(y: "); l.Object(y, Tr); l.s(", curlev: ", Tr);
  l.i(curlev, Tr); l.s(")", Tr);
  ASSERT(y.class IN {ORB.Const, ORB.Var, ORB.Par, ORB.SProc, ORB.Typ});
  (*TODO rather than ASSERTing if class is a Module, should throw error. *)
  H.ZeroFill(x);
  X64.ClearMode(x);
  x.mode     := y.class;
  x.type     := y.type;
  x.readonly := y.rdo;
  IF y.class = ORB.Const THEN
    IF y.type.form = ORB.Proc THEN
      IF y.lev >= 0 THEN x.mode := Code;    x.n := 0;    x.offset := y.val;
      ELSE               x.mode := Import;  x.n := LSL(-y.lev, 16) + y.val;
      END
    ELSIF y.type.form = ORB.String THEN
      x.mode := String;  x.readonly := TRUE;  x.n := y.lev;  x.offset := y.val;
    ELSE
      x.mode := Const;  x.n := y.val
    END
  ELSIF y.class = ORB.Var THEN
    IF    y.lev > 0 THEN x.mode := Eadr;    x.n := RSP;  x.offset := y.val;
    ELSIF y.lev = 0 THEN x.mode := Global;  x.n := 0;    x.offset := y.val;
    ELSE(*y.lev < 0 *)
      IF y.type.form = ORB.Proc THEN x.mode := Impcode ELSE x.mode := Import END;
      x.n := LSL(-y.lev, 16) + y.val;
    END
  ELSIF y.class = ORB.Par THEN
    IF (y.type.form = ORB.Array) & (y.type.len < 0) OR (y.type.form = ORB.Record) THEN
      x.mode := Strucp
    ELSE
      x.mode := Stkind
    END;
    x.n := y.val
  ELSE
    ASSERT(y.class IN {0, ORB.SProc, ORB.Typ})
  END;
  l.s(" -> ", Tr);  l.Item(x, Tr);  l.n(Tr);
END MakeItem;

PROCEDURE MakeConstItem*(VAR x: Item;  t: ORB.Type;  v: INTEGER);
BEGIN
  Trace(TraceMakeItem, "MakeConstItem, value: ");
  l.i(v, Tr); l.s(", type ", Tr); l.Type(t, Tr); l.n(Tr);
  H.ZeroFill(x);
  X64.ClearMode(x);
  x.mode := Const;  x.type  := t;  x.n := v;
END MakeConstItem;

PROCEDURE MakeRealItem*(VAR x: Item; v: REAL);
BEGIN
  Trace(TraceMakeItem, "MakeRealItem."); l.n(Tr);
  H.ZeroFill(x);  X64.ClearMode(x);
  x.mode := Const;  x.type  := ORB.realType;  x.n := ORD(v)
END MakeRealItem;

PROCEDURE MakeStringItem*(VAR x: Item; len: INTEGER); (* copies string from ORS-buffer *)
VAR i: INTEGER;
BEGIN
  Trace(TraceMakeItem, "MakeStringItem('");
  i := 0;  WHILE (i < len) & (ORS.str[i] # 0X) DO l.c(ORS.str[i], Tr); INC(i) END;
  l.sn("')", Tr);

  H.ZeroFill(x);
  X64.ClearMode(x);
  x.mode := String;  x.type   := ORB.strType;  x.readonly := TRUE;
  x.n    := len;     x.offset := Strx;

  IF Strx+len >= LEN(Str) THEN ORS.Mark("Too much string literal")
  ELSE
    FOR i := 0 TO len-1 DO Str[Strx+i] := ORS.str[i] END;
    INC(Strx, len);
    IF ORS.str[len-1] # 0X THEN Str[Strx] := 0X; INC(Strx) END
  END;
  Trace(TraceMakeItem, ".. StringItem ");  l.Item(x, Tr);  l.n(Tr);
END MakeStringItem;



(* ---- Item conversion ---- *)

PROCEDURE StrToChar*(VAR x: Item);
BEGIN Trace(TraceAny, "StrToChar(x: ");  l.Item(x, Tr);  l.n(Tr);
  ASSERT(x.type.form = ORB.String);
  ASSERT(x.mode      = String);
  ASSERT(x.n         = 2);  (* String length - a character and a zero terminator *)
  ASSERT(x.readonly);
  x.type := ORB.charType;
  x.mode := Const;  x.n := ORD(Str[x.offset]);  x.offset := 0;
END StrToChar;


(* ---- Assignment ---- *)

PROCEDURE Store*(VAR dest, src: Item); (* dest := src *)
BEGIN
  Trace(TraceAny, "Store(dest: ");  l.Item(dest, Tr);
  l.s(", src: ", Tr);  l.Item(src, Tr);  l.n(Tr);
  Depar(dest);
  IF src.mode = Const THEN
    StoreImmediate(src.n, dest)
  ELSE
    Load(src);
    StoreReg(src.n, dest);
  END;
  FreeRegs(src);  FreeRegs(dest)
END Store;


PROCEDURE SaveExchange(current, required: INTEGER; VAR save: INTEGER);
BEGIN
  IF current < 0 THEN
    IF required IN Free THEN current := required
                        ELSE current := Reserve(-1, {}) END
  END;
  IF current = required THEN
    save := -1
  ELSE
    save := current;
    OpModRegRm(87H, Reg, current, 8, required, -1, 0, 0, 0)  (* xchg *)
  END
END SaveExchange;

PROCEDURE RestoreReg(reg, exchanged: INTEGER);
BEGIN
  IF exchanged >= 0 THEN
    MoveReg(reg, exchanged)
  ELSE
    Release(reg)
  END
END RestoreReg;


PROCEDURE StoreStruct*(VAR x, y: Item);   (* x := y *)
VAR sadr, slen, dadr, dlen, xchgrsi, xchgrdi, xchgrcx: INTEGER;
BEGIN Trace(TraceAny, "StoreStruct(x: "); l.Item(x, Tr);  l.s(", y: ", Tr); l.Item(y, Tr); l.sn(")", Tr);

  IF y.type.size # 0 THEN

    (* Source structure *)
    sadr := Reserve(RSI, {RDI, RCX});
    slen := Reserve(RCX, {RDI});
    LoadStructure(sadr, slen, y);

    (* Destination structure *)
    dadr := Reserve(RDI, {});
    dlen := Reserve(-1, {});
    LoadStructure(dadr, dlen, x);

    (* For arrays need to check lengths *)
    IF (x.type.form = ORB.Array) & (y.type.form = ORB.Array) THEN
      IF (x.type.len >= 0) & (y.type.len >= 0) THEN
        (* Check lengths at compile time *)
        IF x.type.size # y.type.size THEN
          ORS.Mark("different array length/size, not implemented")
        END
      ELSE
        (* Generate code to fail on differing size *)
        OpModRegRm(Cmp+1, Reg, slen, 8, dlen, -1, 0, 0, 0);
        X64.Emit(73H);   X64.Emit(6);  (* jae: if source fits in target *)
        CallSysFn(H.ArraySizeMismatchProc)
      END
    END;

    (* Temporary register exchanges to use correct regs for rep movsb *)
    SaveExchange(sadr, RSI, xchgrsi);
    SaveExchange(dadr, RDI, xchgrdi);
    SaveExchange(slen, RCX, xchgrcx);

    X64.Emit(0F3H);  X64.Emit(0A4H);  (* rep movsb *)

    (* Restore temporarily exchanged registers *)
    RestoreReg(RSI, xchgrsi);
    RestoreReg(RDI, xchgrdi);
    RestoreReg(RCX, xchgrcx);

    FreeRegs(x);  FreeRegs(y);
    Disassemble(0, "StoreStruct");
  END
END StoreStruct;


PROCEDURE CopyString*(VAR x, y: Item);  (* x := y *)
VAR sadr, slen, dadr, dlen: INTEGER;
BEGIN Trace(TraceAny, "CopyString(x: "); l.Item(x, Tr);  l.s(", y: ", Tr); l.Item(y, Tr); l.sn(")", Tr);
  ASSERT(x.mode IN {Strucp, Stkind, Eadr, Global, Import});
  ASSERT(y.type.form = ORB.String);
  ASSERT(y.mode = String);

  IF (x.type.len >= 0) & (x.type.len < y.n) THEN
    ORS.Mark("string too long");
    y.n := x.type.len
  END;

  (* Source string *)
  sadr := Reserve(RSI, -{});  (* Must be RSI *)
  slen := Reserve(RCX, -{});  (* Must be RCX *)
  LoadStructure(sadr, slen, y);

  (* Destination string *)
  dadr := Reserve(RDI, -{});  (* Must be RDI *)
  IF x.type.len >= 0 THEN (* dest length known at compile time*)
    LoadAddressReg(dadr, x)
  ELSE
    dlen := Reserve(-1, {});
    LoadStructure(dadr, dlen, x);
    (* Check length at run time *)
    OpModRegRm(Cmp+1, Reg, slen, 8, dlen, -1, 0, 0, 0);
    X64.Emit(76H);   X64.Emit(6);  (* jbe: if source fits in dest skip failure *)
    CallSysFn(H.ArraySizeMismatchProc)
  END;

  (* Consider: stop copy at first 0, enforce 0 if none *)
  X64.Emit(0F3H);  X64.Emit(0A4H);  (* rep movsb *)
  Release(RSI);  Release(RDI);  Release(RCX);
  FreeRegs(x);  FreeRegs(y);
  Disassemble(0, "CopyString")
END CopyString;


(* ---- Paramter passing ---- *)

PROCEDURE ValueParam*(VAR x: Item;  stkoff: INTEGER);
VAR pos, reg: INTEGER;
BEGIN
  Trace(TraceAny, "ValueParam["); l.i(stkoff, Tr); l.s("](x: ", Tr); l.Item(x, Tr); l.sn(")", Tr);
  IF stkoff = 0 THEN  (* normal (simple) parameter passing *)
    Push(x); X64.AdjustStack(1)
  ELSE  (* Windows ABI parameter passing *)
    pos := -(stkoff DIV 8 + 1);
    IF    pos = 0 THEN reg := RCX
    ELSIF pos = 1 THEN reg := RDX
    ELSIF pos = 2 THEN reg := 8
    ELSIF pos = 3 THEN reg := 9
    ELSE reg := -1
    END;
    IF reg >= 0 THEN
      LoadReg(reg, x)
    ELSE
      IF x.mode = Const THEN
        StoreImmediateToMem(x.n, Eadr, 8, RBP, -1, 0, pos * 8)
      ELSE
        Load(x);  StoreRegToMem(x.n, Eadr, 8, RBP, -1, 0, pos * 8);  FreeRegs(x)
      END
    END
  END;
  Disassemble(x.mode, "ValueParam")
END ValueParam;


PROCEDURE OpenArrayParam*(VAR x: Item; stkoff: INTEGER);
(* x being passed to open array formal parameter *)
BEGIN
  Trace(TraceAny, "OpenArrayParam["); l.i(stkoff, Tr);
  l.s("](x: ", Tr);                   l.Item(x, Tr);
  l.s(") SPO ", Tr);                  l.i(X64.SPO, Tr);  l.n(Tr);
  ASSERT(x.mode IN {Stkind, Strucp, String, Eadr, Global, Import});
  IF stkoff # 0 THEN
    ORS.Mark("Open array parameters not supported for Windows ABI")
  ELSIF x.mode = Reg THEN
    ORS.Mark("Not compatible with open array parameter")
  ELSE
    PushLength(x); X64.AdjustStack(1);  (* Pass array length in elements *)
    PushAdr(x);    X64.AdjustStack(1);  (* Pass array address *)
  END;
  FreeRegs(x);
  Disassemble(x.mode, "OpenArrayParam");
  Trace(TraceAny, "OpenArrayParam exit: SPO "); l.i(X64.SPO, Tr);  l.n(Tr)
END OpenArrayParam;

PROCEDURE ArrayOfByteParam*(VAR x: Item; stkoff: INTEGER);
(* Any type x being passed to ARRAY OF BYTE formal parameter *)
BEGIN
  Trace(TraceAny, "ArrayOfByteParam["); l.i(stkoff, Tr);
  l.s("](x: ", Tr);                   l.Item(x, Tr);
  l.s(") SPO ", Tr);                  l.i(X64.SPO, Tr);  l.n(Tr);
  ASSERT(x.mode IN {Stkind, Strucp, String, Eadr, Global, Import});
  IF stkoff # 0 THEN
    ORS.Mark("ARRAY OF BYTE parameters not supported for Windows ABI")
  ELSIF x.mode = Reg THEN
    ORS.Mark("Not compatible with ARRAY OF BYTE parameter")
  ELSE
    PushSize(x);  X64.AdjustStack(1);  (* Pass var size in bytes *)
    PushAdr(x);   X64.AdjustStack(1);  (* Pass var address *)
  END;
  FreeRegs(x);
  Disassemble(x.mode, "OpenArrayParam");
  Trace(TraceAny, "OpenArrayParam exit: SPO "); l.i(X64.SPO, Tr);  l.n(Tr)
END ArrayOfByteParam;


PROCEDURE VarParam*(VAR x: Item;  ftype: ORB.Type;  stkoff: INTEGER);
VAR reg: INTEGER;
BEGIN
  Trace(TraceAny, "VarParam["); l.i(stkoff, Tr);
  l.s("](x: ", Tr);            l.Item(x, Tr);
  l.s(", ftype: ", Tr);        l.Type(ftype, Tr);
  l.s(") SPO ", Tr);           l.i(X64.SPO, Tr);  l.n(Tr);
  IF stkoff # 0 THEN
    ORS.Mark("VAR parameters not supported for Windows ABI")
  ELSIF x.mode IN {Reg, Const} THEN
    ORS.Mark("Not compatible with VAR parameter")
  ELSIF (ftype.form = ORB.Array) & (ftype.len < 0) THEN
    OpenArrayParam(x, stkoff)
  ELSE
    ASSERT(x.mode IN {Stkind, Strucp, Eadr, Global, Import});
    IF ftype.form = ORB.Record THEN PushTypeAdr(x); X64.AdjustStack(1) END;
    PushAdr(x);  X64.AdjustStack(1);
(* ----
    IF (x.mode = Strucp) & ((ftype.form = ORB.Array) & (ftype.len < 0) OR (ftype.form = ORB.Record)) THEN
      IF x.type.form = ftype.form THEN
        (* Passing open array to open array, or dynamic record to dynamic record *)
        PushMem(Eadr, FALSE, 8, RSP, -1, 0, x.n + 8 * X64.SPO + 8)
      ELSE ASSERT(x.type.form = ORB.Record);  (* Cannot pass open array to dynamic record *)
        (* Passing dynamic record to ARRAY OF BYTE *)
        reg := Reserve(-1, {});  (* Obtain length from type descriptor *)
        LoadMem(reg, Eadr, FALSE, 8, RSP, -1, 0, x.n + 8 * X64.SPO + 8);  (* Load address of record *)
        LoadMem(reg, Eadr, FALSE, 8, reg, -1, 0, -8);  (* Load address of type descriptor from qword preceeding record *)
        PushMem(Eadr, FALSE, 8, reg, -1, 0, 0);        (* Push record length from first qword of type descriptor *)
        Release(reg)
      END;
      X64.AdjustStack(1);
      PushMem(Eadr, FALSE, 8, RSP, -1, 0, x.n + 8 * X64.SPO);  (* Push array/record address *)
      X64.AdjustStack(1)
    ELSE
      Depar(x);
      (* First pass array length or type descriptor address, if any *)
      IF (ftype.form = ORB.Array) & (ftype.len < 0) THEN ASSERT(x.type.len >= 0);
        IF x.mode = String THEN
          PushImmediate(x.n)
        ELSIF x.type.form = ORB.Array THEN
          PushImmediate(x.type.len)
        ELSE
          PushImmediate(x.type.size) (* Passing non-array to ARRAY OF BYTE *)
        END;
        X64.AdjustStack(1) (* Pass array length *)
      ELSIF ftype.form = ORB.Record THEN
        reg := Reserve(-1, {});
        LoadTypeAddress(reg, ftype);  (* Pass record descriptor *)
        PushReg(reg);  X64.AdjustStack(1);
        Release(reg)
      END;
      (* Pass array or record address *)
      PushAdr(x);  X64.AdjustStack(1)
    END;
---- *)
    Disassemble(x.mode, "VarParam")
  END;
  FreeRegs(x)
END VarParam;

PROCEDURE StringParam*(VAR x: Item; stkoff: INTEGER);
BEGIN
  Trace(TraceAny, "StringParam["); l.i(stkoff, Tr);
  l.s("](x: ", Tr);               l.Item(x, Tr);
  l.s(") SPO ", Tr);              l.i(X64.SPO, Tr);  l.n(Tr);
  ASSERT(x.mode = String);
  ASSERT(stkoff = 0);  (* Not supported for Windows ABI calls *)
  PushImmediate(x.n);  X64.AdjustStack(1);  (* Pass string length *)
  PushAdr(x);          X64.AdjustStack(1);  (* Pass string address *)
  Disassemble(x.mode, "StringParam");
  FreeRegs(x)
END StringParam;


(* ---- Procedure / function calls ---- *)

PROCEDURE PrepCall*(VAR x: Item; VAR saved: SET);
VAR i : INTEGER;  stackspace: INTEGER;
BEGIN
  Trace(TraceAny, "PrepCall(x: "); l.Item(x, Tr);
  l.s("), SPO ", Tr);             l.i(X64.SPO, Tr); l.n(Tr);
  saved := AllFree - Free;
  Depar(x);
  IF x.mode = Eadr THEN EXCL(saved, x.n); IF x.index >= 0 THEN EXCL(saved, x.index) END END;
  IF saved # {} THEN  (* Save partially evaluated expression *)
    FOR i := 0 TO 15 DO IF i IN saved THEN PushReg(i);  X64.AdjustStack(1) END END;
    Disassemble(0, "Save registers");
    Free := AllFree
  END;
  ASSERT(x.type.form = ORB.Proc);
  IF ~(x.mode IN {Eadr, Global, Code, Import, Impcode}) THEN
    ws("** x.mode ");  wi(x.mode);  wsn(" **");
    ORS.Mark("Didn't expect such an x.mode.")
  END;
  ASSERT(x.mode IN {Eadr, Global, Code, Import, Impcode});

  stackspace := x.type.len;  (* Only used for Windows native ABI calls *)
  IF stackspace < 32 THEN stackspace := 32 END;

  IF x.mode = Eadr THEN  (* Push proc address to stack *)
    Push(x);  X64.AdjustStack(1);  FreeRegs(x);
    Disassemble(x.mode, "proc address to be called");
    ASSERT(x.type.len MOD 8 = 0);
    x.mode   := Eadr;
    IF ~x.type.native THEN  (* Normal, simple calling process *)
      x.n      := RSP;
      x.offset := -8 * (X64.SPO - (x.type.len DIV 8 + 1))
    ELSE  (* Windows ABI calling process *)
      x.n      := RBP;
      x.offset := -8 * (X64.SPO - (stackspace DIV 8 + 2))
    END
  END;
  IF x.type.native THEN  (* Prepare stack space for windows abi *)
    (* We need to place parameters aligned so that the first parameter (at the
       lowest address) is 16 byte aligned. This alignemnet will only be known at
       run time. However we need to keep SPO matching SP so that expressions
       referencing local vars and parameters work corectly. Therefore we use
       BP to place the parameters on the stack, and advance SP 8 bytes extra to
       allow for either alignment *)
    ReserveReg(RBP);  MoveReg(RBP, RSP);
    AluOpFromImmediate(Minus, Reg, stackspace,         8, RBP, -1, 0, 0);
    AluOpFromImmediate(And,   Reg, 0FFFFFFFFFFFFFFF0H, 8, RBP, -1, 0, 0);
    AluOpFromImmediate(Minus, Reg, stackspace + 8,     8, RSP, -1, 0, 0);
    X64.AdjustStack(stackspace DIV 8 + 1);
    Disassemble(0, "Reserve Windows ABI stack space")
  END
END PrepCall;


PROCEDURE Call*(VAR x: Item; saved: SET);
VAR i, reg, resultreg, stackspace: INTEGER;
BEGIN
  Trace(TraceAny, "Call(x: "); l.Item(x, Tr);
  l.s("), SPO ", Tr);         l.i(X64.SPO, Tr);
  l.s(", type.len ", Tr);  l.i(x.type.len, Tr);  l.n(Tr);
  ASSERT(x.type.form = ORB.Proc);
  ASSERT(x.mode IN {Eadr, Global, Code, Import, Impcode});

  IF x.type.native THEN  (* Adjust to 16 byte aligned pos *)
    OpModRegRm(87H, Reg, RBP, 8, RSP, -1, 0, 0, 0);  (* xchg rsp,rbp *)
    Disassemble(0, "Switch to Windows ABI aligned stack frame")
  END;

  IF x.type.form = ORB.Proc THEN
    IF x.mode IN {Eadr, Global, Impcode} THEN
      OpModRegRm(0FFH, x.mode, 2, 4, x.n, x.index, x.scale, x.offset, 0)  (* call [addr] *)
    ELSE
      X64.Emit(0E8H);
      IF x.mode = Code THEN
        X64.EmitBytes(4, x.offset - (X64.PC + 4))
      ELSIF x.mode = Import THEN
        ImpFix[ImpFInx].offset := X64.PC;
        ImpFix[ImpFInx].fix    := x.n;  INC(ImpFInx);
        X64.EmitBytes(4, -(X64.PC + 4))  (* Offset from next instruction to module address 0 *)
      ELSE
        l.s("** mode ", Tr);  l.Mode(x.mode, Tr);  l.sn(" **", Tr);
        ASSERT(FALSE)
      END
    END
  ELSE
    l.s("** form ", Tr);  l.Form(x.type.form, Tr);  l.sn(" **", Tr);
    ASSERT(FALSE)
  END;
  Disassemble(x.mode, "Call");

  IF ~x.type.native THEN   (* Normal (simple) stack handling *)
    X64.AdjustStack(-(x.type.len DIV 8));  (* matches stack adjustment by <ret n> *)
  ELSE (* Windows ABI stack handling *)
    stackspace := x.type.len;  IF stackspace < 32 THEN stackspace := 32 END;
    OpModRegRm(8DH, Eadr, RSP, 8, RBP, -1, 0, stackspace + 8, 0);  (* lea rsp, rbp + parmreserved *)
    Disassemble(0, "Drop Windows ABI parameter area from stack");
    X64.AdjustStack(-(stackspace DIV 8 + 1))  (* matches stack adjustment by <ret n> *)
  END;

  IF x.mode = Eadr THEN
    (* Drop called proc address from stack *)
    AluOpFromImmediate(Plus, Reg, 8, 8, RSP, -1, 0, 0);
    X64.AdjustStack(-1);
    Disassemble(0, "Drop called proc address")
  END;

  IF x.type.base.form = ORB.NoTyp THEN (* procedure *)
    ASSERT(saved = {});
    ClearRegs
  ELSE                                 (* function *)
    ClearRegs;
    FreeRegs(x);
    (* Move function result to another reg if RAX was in use before fn call *)
    Free := Free - saved;
    resultreg := Reserve(-1, {});
    IF resultreg # RAX THEN MoveReg(resultreg, RAX) END;
    x.mode := Reg;  x.n := resultreg;
    IF saved # {} THEN (* Restore saved registers *)
      i := 15;
      WHILE i >= 0 DO
        IF i IN saved THEN PopReg(i);  ReserveReg(i);  X64.AdjustStack(-1);  END;
        DEC(i)
      END;
      Disassemble(0, "Restore saved registers")
    END;
  END
END Call;

PROCEDURE Enter*(parmsize, locsize: INTEGER; native: BOOLEAN);
VAR i, count: INTEGER;
BEGIN
  Trace(TraceAny, "Enter(parmsize $"); l.h(parmsize, Tr);
  l.s(", locsize $", Tr);             l.h(locsize, Tr);   l.sn(")", Tr);

  IF native THEN
    (* For native procedures, deposit reg params on stack *)
    StoreRegToMem(RCX, Eadr, 8, RSP, -1, 0, 08H);
    StoreRegToMem(RDX, Eadr, 8, RSP, -1, 0, 10H);
    StoreRegToMem(8,   Eadr, 8, RSP, -1, 0, 18H);
    StoreRegToMem(9,   Eadr, 8, RSP, -1, 0, 20H);
    Disassemble(Eadr, "Save native procedure parameters to stack.");
    ASSERT(locsize >= 64);
    (* Save X64 ABI non-volatile registers *)
    PushReg(RBX);  PushReg(RSI);  PushReg(RDI);  PushReg(RBP);
    PushReg(12);   PushReg(13);   PushReg(14);   PushReg(15);
    DEC(locsize, 64)
  END;

  (* Clear local variables *)
  ASSERT(locsize MOD 8 = 0);
  count := locsize DIV 8;
  IF count > 0 THEN
    IF count < 3 THEN
      FOR i := 1 TO count DO PushImmediate(0) END
    ELSIF count < 8 THEN
      LoadImmediate(RAX, 0);          (*     xor    eax,eax   *)
      FOR i := 1 TO count DO PushReg(RAX) END
    ELSE
      LoadImmediate(RCX, count);      (*     mov    rcx,count *)
      PushImmediate(0);               (* l1: push   0         *)
      X64.Emit(0E2H); X64.Emit(0FCH)  (*     loop   l1        *)
    END;
    Disassemble(0, "Reserve and clear local variables")
  END;
  X64.ClearStack;  ClearRegs
END Enter;


PROCEDURE EmitStrings();
VAR i: INTEGER;
BEGIN
  IF ORS.ListAssembly & (Disasmpc < X64.PC) THEN Disassemble(0, "") END;
  IF Strx > 0 THEN
    (*l.n(Tr);*)
    i := 0;
    WHILE i < StrFInx DO  (* Relocate collected string addresses *)
      X64.Patch(StrFix[i], 4, X64.PC + X64.Peek(StrFix[i], 4, FALSE));  INC(i)
    END;
    i := 0;
    WHILE i < Strx DO  (* Emit 1 string  *)
      WHILE (Str[i] # 0X) & (i < Strx) DO X64.Emit(ORD(Str[i])); INC(i) END;
      IF i < Strx THEN ASSERT(Str[i] = 0X); INC(i) END;
      X64.Emit(0);
      IF ORS.ListAssembly THEN l.DisassembleString(Disasmpc) END;
    END
  END;
  Strx := 0;  StrFInx := 0;
  IF ~ORS.ListAssembly THEN Disasmpc := X64.PC END
END EmitStrings;

PROCEDURE Return*(
  form:      INTEGER;
  VAR x:     Item;
  parmsize:  INTEGER;
  locsize:   INTEGER;
  native:    BOOLEAN);
BEGIN
  Trace(TraceAny, "Return("); l.Form(form, Tr);
  l.s(", x: ", Tr);       l.Item(x, Tr);
  l.s(", locsize ", Tr);  l.i(locsize, Tr);
  l.s(", parmsize ", Tr); l.i(parmsize, Tr);
  l.s(", SPO ", Tr);      l.i(X64.SPO, Tr); l.n(Tr);

  IF form # ORB.NoTyp THEN
    Load(x);
    IF x.n # RAX THEN MoveReg(RAX, x.n) END;
    Disassemble(x.mode, "Load function result")
  END;

  IF native THEN
    ASSERT(locsize >= 64);
    IF locsize > 64 THEN
      AluOpFromImmediate(Plus, Reg, locsize - 64, 8, RSP, -1, 0, 0);
      Disassemble(0, "release local variable space")
    END;
    PopReg(15);   PopReg(14);   PopReg(13);   PopReg(12);
    PopReg(RBP);  PopReg(RDI);  PopReg(RSI);  PopReg(RBX);
    X64.Emit(0C3H)  (* ret *)
  ELSE
    IF locsize > 0 THEN
      AluOpFromImmediate(Plus, Reg, locsize, 8, RSP, -1, 0, 0);
      Disassemble(0, "release local variable space")
    END;
    IF parmsize = 0 THEN
      X64.Emit(0C3H)  (* ret *)
    ELSE
      X64.Emit(0C2H);  X64.EmitBytes(2, parmsize)  (* retn *)
    END
  END;
  Disassemble(0, "Return");

  X64.Emit(2EH);  (* Mark end of code with ignored instruction 'seg cs' *)
  Disasmpc := X64.PC;

  (*EmitStrings;*)
  X64.ClearStack;  ClearRegs
END Return;


(* ---- Standard functions ---- *)

PROCEDURE Abs*(VAR x: Item);
VAR reg: INTEGER;
BEGIN Trace(TraceAny, "Abs(x "); l.Item(x, Tr); l.sn(")", Tr);
  IF x.mode = Const THEN x.n := ABS(x.n)
  ELSIF x.type.form = ORB.Real THEN ASSERT(FALSE)
  ELSE
    Load(x);  reg := Reserve(-1, {});
    MoveReg(reg, x.n);                                 (* mov     reg,x.n *)
    OpModRegRm(0F7H,  Reg, 3,   8, x.n, -1, 0, 0, 0);  (* neg     x.n     *)
    OpModRegRm(0F4CH, Reg, x.n, 8, reg, -1, 0, 0, 0);  (* cmovl   x.n,reg *)
    Release(reg)
  END;
  Disassemble(x.mode, "Abs")
END Abs;

PROCEDURE Odd*(VAR x: Item);
BEGIN Trace(TraceAny, "Odd(x: "); l.Item(x, Tr); l.sn(")", Tr);
  Load(x);
  AluOpFromImmediate(And, Reg, 1, 8, x.n, -1, 0, 0);  (* and  x.n,1 *)
  FreeRegs(x);
  SetCondition(x, CNZ);  (* odd if x & 1 nonzero *)
  Disassemble(x.mode, "Odd")
END Odd;

PROCEDURE Floor*(VAR x: Item);
BEGIN Trace(TraceAny, "Floor(x: "); l.Item(x, Tr); l.sn(")", Tr);  ASSERT(FALSE);
  ASSERT(FALSE)
END Floor;

PROCEDURE Float*(VAR x: Item);
BEGIN Trace(TraceAny, "Float(x: "); l.Item(x, Tr); l.sn(")", Tr);  ASSERT(FALSE);
  ASSERT(FALSE)
END Float;

PROCEDURE Ord*(VAR x: Item);
BEGIN Trace(TraceAny, "Ord(x: "); l.Item(x, Tr); l.sn(")", Tr);
  IF x.mode # Const THEN Load(x) END;
  Disassemble(x.mode, "ORD")
END Ord;

PROCEDURE Len*(VAR x: Item);
BEGIN Trace(TraceAny, "Len(x: "); l.Item(x, Tr); l.sn(")", Tr);
  ASSERT(x.type.form IN {ORB.String, ORB.Array});
  ASSERT(x.mode IN {Stkind, Strucp, Eadr, Code, String, Global, Import});
  IF x.mode = Strucp THEN  (* Length is runtime parameter *)
    x.mode   := Eadr;
    x.offset := x.n + 8;
    x.n      := RSP;
  ELSE  (* Length known at compile time *)
    x.mode := Const;
    IF x.mode # String THEN (* Length not already in x.n *)
      ASSERT(x.type.form = ORB.Array);
      x.n := x.type.len
    END;
    x.offset := 0;
  END;
  ASSERT(x.index < 0);  ASSERT(x.scale = 0)
END Len;

PROCEDURE Shift*(fct: INTEGER; VAR x, y: Item);
VAR op, xchgrcx: INTEGER;  name: ARRAY 4 OF CHAR;
BEGIN
  IF    fct = 0 THEN name := "LSL";  op := 4  (* shl *)
  ELSIF fct = 1 THEN name := "ASR";  op := 7  (* sar *)
  ELSIF fct = 2 THEN name := "ROR";  op := 1  (* ror *)
  ELSE ASSERT(FALSE)
  END;
  Trace(TraceAny, "Shift("); l.s(name, Tr); l.s(", x: ", Tr); l.Item(x, Tr); l.s(", y: ", Tr); l.Item(y, Tr); l.n(Tr);
  IF y.mode = Const THEN
    Load(x);
    OpModRegRm(0C1H, Reg, op, 8, x.n, -1, 0, 0, 1);  X64.Emit(y.n);
  ELSE
    IF x.mode # Reg THEN LoadReg(Reserve(RAX, {RCX}), x) END;
    IF x.n = RCX THEN  (* Use a different register as we'll need cl for shift count *)
      x.n := Reserve(-1, {});  MoveReg(x.n, RCX);  Release(RCX)
    END;
    (* Get shift amount to rcx *)
    IF y.mode # Reg THEN LoadReg(Reserve(RCX, {}), y) END;
    SaveExchange(y.n, RCX, xchgrcx);
    OpModRegRm(0D3H, Reg, op, 8, x.n, -1, 0, 0, 0);  (* shiftop reg,cl *)
    RestoreReg(RCX, xchgrcx)
  END;
  FreeRegs(y);
  Disassemble(0, name)
END Shift;

PROCEDURE ADC*(VAR x, y: Item);
BEGIN Trace(TraceAny, "ADC(x: "); l.Item(x, Tr); l.s(", y: ", Tr); l.Item(y, Tr); l.sn(")", Tr);
  ASSERT(FALSE)
END ADC;

PROCEDURE SBC*(VAR x, y: Item);
BEGIN Trace(TraceAny, "SBC(x: "); l.Item(x, Tr); l.s(", y: ", Tr); l.Item(y, Tr); l.sn(")", Tr);
  ASSERT(FALSE)
END SBC;

PROCEDURE UML*(VAR x, y: Item);
BEGIN Trace(TraceAny, "UML(x: "); l.Item(x, Tr); l.s(", y: ", Tr); l.Item(y, Tr); l.sn(")", Tr);
  ASSERT(FALSE)
END UML;

PROCEDURE Bit*(VAR x, y: Item);
BEGIN Trace(TraceAny, "Bit(x: "); l.Item(x, Tr); l.s(", y: ", Tr); l.Item(y, Tr); l.sn(")", Tr);
  ASSERT(FALSE)
END Bit;

PROCEDURE REG*(VAR x: Item);
VAR r: INTEGER;
BEGIN Trace(TraceAny, "REG(x "); l.Item(x, Tr); l.sn(")", Tr);
  ASSERT(x.mode = Const);
  r := Reserve(x.n, {});
  MoveReg(r, x.n);
  x.n := r;  x.mode := Reg
END REG;

PROCEDURE Adr*(VAR x: Item);
BEGIN Trace(TraceAny, "Adr(x "); l.Item(x, Tr); l.sn(")", Tr);
  IF x.mode IN {Stkind, Strucp, Eadr, Code, String, Global, Import, Impcode} THEN
    LoadAddress(x);  x.mode := Reg;  ASSERT(~(x.n IN Free));
  ELSE
    ORS.Mark("not addressable")
  END;
  Disassemble(0, "ADR")
END Adr;

PROCEDURE COND*(VAR x: Item);
BEGIN Trace(TraceAny, "COND(x "); l.Item(x, Tr); l.sn(")", Tr);
  ASSERT(FALSE)
END COND;


(* ---- Standard procedures --- *)

PROCEDURE Increment*(upordown: INTEGER; VAR x, y: Item);  (* x := x +/- y *)
VAR op: INTEGER;
BEGIN Trace(TraceAny, "Increment(");
  IF upordown = 0 THEN
    op := Plus;   l.s("up", Tr)
  ELSE
    op := Minus;  l.s("down", Tr)
  END;
  l.s(", x: ", Tr); l.Item(x, Tr); l.s(", y: ", Tr); l.Item(y, Tr); l.n(Tr);

  ASSERT(x.mode IN {Stkind, Strucp, Eadr, Global, Import});
  Depar(x);
  IF y.type = ORB.noType THEN
    AluOpFromImmediate(op, x.mode, 1,   x.type.size, x.n, x.index, x.scale, x.offset)
  ELSIF y.mode = Const THEN
    AluOpFromImmediate(op, x.mode, y.n, x.type.size, x.n, x.index, x.scale, x.offset);
  ELSE
    Load(y);
    (*X64.AluOpRegToMem(op, y.n, x.size, x.n, x.index, x.scale, x.offset)*)
    (*X64.EmitRegMemOp(op+1, y.n, x.size, x.n, x.index, x.scale, x.offset)*)
    OpItem(op+1, y.n, 8, 0, x)
  END;

  Disassemble(x.mode, "INC/DEC");
  FreeRegs(x);  FreeRegs(y)
END Increment;


PROCEDURE Assert*(VAR x: Item);
BEGIN Trace(TraceAny, "Assert(x: ");  l.Item(x, Tr);  l.sn(")", Tr);
  IF x.mode # Cond THEN LoadFlags(x) END;
  IF x.n <= 1 THEN (* assertion is constant at compile time *)
    ASSERT(x.offset = 0);  ASSERT(x.index = 0);
    IF x.n = 0 THEN CallSysFn(H.AssertionFailureProc) END
  ELSE
    ASSERT(x.n DIV 16 = 8);
    X64.Emit(x.n - 10H);  X64.Emit(6);  (* Skip over call instruction if condition met *)
    FixLink(x.offset); (* FALSE chain jumps to assert trap *)
    CallSysFn(H.AssertionFailureProc);
    FixLink(x.index); (* TRUE chain bypasses asser trap *)
  END;
  Disassemble(x.mode, "ASSERT")

  (*
  IF x.n > 1 THEN
    ASSERT(x.n DIV 16 = 8);
    X64.Emit(x.n - 10H);  X64.Emit(6);  (* Skip over call instruction if condition met *)
  END;
  IF x.n # 1 THEN CallSysFn(H.AssertionFailureProc) END;  (* Jump to assert failure handler *)
  *)
END Assert;

PROCEDURE New*(VAR x: Item);
BEGIN
  Trace(TraceAny, "New(x: ");  l.Item(x, Tr);
  l.s("), base type ", Tr);   l.Type(x.type.base, Tr); l.n(Tr);
  ASSERT(x.mode IN {Stkind, Strucp, Eadr, Global, Import});
  PushAdr(x);      X64.AdjustStack(1);
  PushTypeAdr(x);  X64.AdjustStack(1);
  FreeRegs(x);
  CallSysFn(H.NewProc);
  X64.AdjustStack(-2);
  Disassemble(x.mode, "New")
END New;

PROCEDURE Led*(VAR x: Item);
BEGIN Trace(TraceAny, "Led(x: ");  l.Item(x, Tr);  l.sn(")", Tr);
  ASSERT(FALSE)
END Led;

PROCEDURE LDPSR*(VAR x: Item);
BEGIN Trace(TraceAny, "LDPSR(x: ");  l.Item(x, Tr);  l.sn(")", Tr);
  ASSERT(FALSE)
END LDPSR;

PROCEDURE Pack*(VAR x, y: Item);
BEGIN Trace(TraceAny, "Pack(x: "); l.Item(x, Tr); l.s(", y: ", Tr); l.Item(y, Tr); l.sn(")", Tr);
  ASSERT(FALSE)
END Pack;

PROCEDURE Unpk*(VAR x, y: Item);
BEGIN Trace(TraceAny, "Unpk(x: "); l.Item(x, Tr); l.s(", y: ", Tr); l.Item(y, Tr); l.sn(")", Tr);
  ASSERT(FALSE)
END Unpk;

PROCEDURE Get*(VAR x, y: Item);  (* y := [x] *)
BEGIN Trace(TraceAny, "Get(x: "); l.Item(x, Tr); l.s(", y: ", Tr); l.Item(y, Tr); l.n(Tr);
  ASSERT(y.type.size <= 8);
  Load(x);  x.type := y.type;  x.mode := Eadr;  Store(y, x);
  FreeRegs(x);  FreeRegs(y)
END Get;

PROCEDURE Put*(VAR x, y: Item);
BEGIN Trace(TraceAny, "Put(x: "); l.Item(x, Tr); l.s(", y: ", Tr); l.Item(y, Tr); l.n(Tr);
  ASSERT(y.type.size <= 8);
  Load(x);  x.type := y.type;  x.mode := Eadr;  Store(x, y);
  FreeRegs(x);  FreeRegs(y)
END Put;

PROCEDURE LDREG*(VAR x, y: Item);
BEGIN
  Trace(TraceAny, "LDREG(x: "); l.Item(x, Tr);
  l.s(", y: ", Tr);            l.Item(y, Tr); l.n(Tr);
  ASSERT(x.mode = Const);
  LoadRegUnreserved(x.n, y)
END LDREG;

PROCEDURE Copy*(VAR x, y, z: Item);
BEGIN Trace(TraceAny, "Copy(x: "); l.Item(x, Tr);
  l.s(", y: ", Tr);               l.Item(y, Tr);
  l.s(", z: ", Tr);               l.Item(z, Tr); l.n(Tr);
  IF (z.mode = Const) & (z.n <= 0) THEN ORS.Mark("bad count") END;

  LoadReg(RSI, x);
  LoadReg(RDI, y);
  LoadReg(RCX, z);

  (* TODO Check rcx >=0 & <= LEN(y) *)

  X64.Emit(0F3H);  X64.Emit(0A4H);  (* rep movsb *)
  FreeRegs(x);  FreeRegs(y);  FreeRegs(z);
  Disassemble(0, "Copy");
END Copy;


(* ---- Sets ---- *)

PROCEDURE Set*(VAR x, y: Item);   (* x := {x .. y} *)
VAR rx, ry, rcxsave: INTEGER;
BEGIN
  Trace(TraceAny, "Set(x: "); l.Item(x, Tr);
  l.s(", y: ", Tr);           l.Item(y, Tr);
  IF (x.mode = Const) & (y.mode = Const) THEN
    IF x.n <= y.n THEN
      x.n := LSL(2, y.n) - LSL(1, x.n) ELSE x.n := 0
    END
  ELSE
    IF RCX IN Free THEN rcxsave := -1 ELSE
      rcxsave := Reserve(-1, {});  MoveReg(rcxsave, RCX);
      IF    (x.mode IN {Reg, Eadr}) & (x.n     = RCX) THEN x.n     := rcxsave
      ELSIF (x.mode = Eadr)         & (x.index = RCX) THEN x.index := rcxsave
      ELSIF (y.mode IN {Reg, Eadr}) & (y.n     = RCX) THEN y.n     := rcxsave
      ELSIF (y.mode = Eadr)         & (y.index = RCX) THEN y.index := rcxsave
      END
    END;
    rx := Reserve(-1, {RCX});
    ry := Reserve(-1, {RCX});
    IF x.mode = Const THEN LoadImmediate(rx, LSL(-1, x.n)) ELSE
      LoadImmediate(rx, -1);  LoadReg(RCX, x);
      OpModRegRm(0D3H, Reg, 4, 8, rx, -1, 0, 0, 0);   (* shl rx,cl *)
    END;
    IF y.mode = Const THEN LoadImmediate(ry, LSL(-2, y.n)) ELSE
      LoadImmediate(ry, -2);  LoadReg(RCX, y);
      OpModRegRm(0D3H, Reg, 4, 8, ry, -1, 0, 0, 0);   (* shl ry,cl *)
    END;
    AluOpToReg(Xor, Reg, rx, FALSE, 8, ry, -1, 0, 0); (* and rx,ry *)
    FreeRegs(x);  FreeRegs(y);  Release(ry);  ReserveReg(rx);
    x.mode := Reg;  x.n := rx;
    IF rcxsave >= 0 THEN MoveReg(RCX, rcxsave);  Release(rcxsave) END;
    Disassemble(0, "{x .. y}");
  END;
  l.s(") -> ", Tr); l.h(x.n, Tr);  l.sn("H.", Tr);
END Set;

PROCEDURE Singleton*(VAR x: Item);  (* x := {x} *)
VAR reg: INTEGER;
BEGIN Trace(TraceAny, "Singleton(x: "); l.Item(x, Tr); l.sn(")", Tr);
  IF x.mode = Const THEN
    x.n := LSL(1, x.n)
  ELSE
    Load(x);
    reg := Reserve(-1, {});
    LoadImmediate(reg, 0);
    OpModRegRm(0FABH, Reg, x.n, 8, reg, -1, 0, 0, 0);  (* bts reg, x *)
    Disassemble(x.mode, "Singleton");
    FreeRegs(x);
    x.mode := Reg;  x.n := reg
  END
END Singleton;


PROCEDURE In*(VAR x, y: Item);  (* x := x IN y *)
BEGIN
  Trace(TraceAny, "In(x: "); l.Item(x, Tr);
  l.s(", y: ", Tr);          l.Item(y, Tr); l.sn(")", Tr);
  ASSERT(x.mode  >= Const);
  ASSERT(y.mode  >= Const);

  (* TODO - optimise immediate x cases where x<32 with AND 1<<x *)

  Depar(x);  Depar(y);
  IF y.mode = Const THEN Load(y) END;
  IF x.mode = Const THEN
    OpItem(0FBAH, 4, 8, 1, y);   (* bt y,imm *)
    X64.Emit(x.n)
  ELSE
    Load(x);
    OpItem(0FA3H, x.n, 8, 0, y)  (* bt y,x *)
  END;
  Disassemble(y.mode, "In");
  FreeRegs(x);  FreeRegs(y);
  SetCondition(x, CC)
END In;

PROCEDURE SetOp*(op: INTEGER; VAR x, y: Item);   (* x := x op y *)
VAR xs, ys: SET;
BEGIN
  Trace(TraceAny, "SetOp("); l.Sym(op, Tr);
  l.s("; x: ", Tr);         l.Item(x, Tr);
  l.s(", y: ", Tr);         l.Item(y, Tr); l.sn(")", Tr);
  IF (x.mode = Const) & (y.mode = Const) THEN
    xs := SYSTEM.VAL(SET, x.n);  ys := SYSTEM.VAL(SET, y.n);
    IF    op = ORS.plus  THEN xs := xs + ys
    ELSIF op = ORS.minus THEN xs := xs - ys
    ELSIF op = ORS.times THEN xs := xs * ys
    ELSIF op = ORS.rdiv  THEN xs := xs / ys
    END;
    x.n := ORD(xs)
  ELSE
    Load(x);
    (* Convert op to x86 opcode *)
    IF op = ORS.minus THEN  (* Invert y *)
      IF y.mode = Const THEN
        y.n := -y.n - 1  (* = invert *)
      ELSE
        Load(y);  OpItem(0F7H, 2, 8, 0, y);
        Disassemble(y.mode, "Invert for SET -")
      END
    END;
    IF    op = ORS.plus  THEN op := Or
    ELSIF op = ORS.minus THEN op := And
    ELSIF op = ORS.times THEN op := And
    ELSIF op = ORS.rdiv  THEN op := Xor
    ELSE l.s("** op = ", Tr); l.i(op, Tr); l.sn(" **", Tr); ASSERT(FALSE)
    END;
    Dyadic(op, x, y);  FreeRegs(y)
  END;
  Disassemble(0, "SetOp")
END SetOp;

PROCEDURE Include*(op: INTEGER; VAR x, y: Item);  (* INCL/EXCL(x, y): x := x +/- {y} *)
BEGIN
  Trace(TraceAny, "Include(");
  IF op = 0 THEN l.s("INCL", Tr) ELSE l.s("EXCL", Tr) END;
  l.s(", y: ", Tr); l.Item(x, Tr);
  l.s(", y: ", Tr); l.Item(y, Tr); l.sn(")", Tr);
  ASSERT(x.mode IN {Stkind, Strucp, Eadr, Global, Import});
  ASSERT(y.mode >= Const);
  Depar(x);  Depar(y);
  IF y.mode = Const THEN
    IF op = 0 THEN
      OpItem(0FBAH, 5, 8, 1, x)  (* bts x,imm *)
    ELSE
      OpItem(0FBAH, 6, 8, 1, x)  (* btr x,imm *)
    END;
    X64.Emit(y.n)
  ELSE
    Load(y);
    IF op = 0 THEN
      OpItem(0FABH, y.n, 8, 0, x)  (* bts x,y *)
    ELSE
      OpItem(0FB3H, y.n, 8, 0, x)  (* btr x,y *)
    END
  END;
  FreeRegs(x);  FreeRegs(y);
  IF op = 0 THEN Disassemble(x.mode, "INCL") ELSE Disassemble(x.mode, "EXCL") END;
END Include;


(* ---- Boolean operators ---- *)

PROCEDURE merged(L0, L1: INTEGER): INTEGER;  (* Insert L0 at the start of L1 *)
VAR L2, L3: INTEGER;
BEGIN
  Trace(TraceAny, "Merged(L0: $"); l.h(L0, Tr);
  l.s(", L1: $", Tr);              l.h(L1, Tr);  l.sn(")", Tr);
  IF L0 # 0 THEN
    L3 := L0;
    REPEAT
      L2 := L3;
      L3 := X64.Peek(L2, 4, FALSE)
    UNTIL L3 = 0;
    Trace(TraceAny, "-> Patch at $"); l.h(L2, Tr);
    l.s(" currently $", Tr);          l.h(X64.Peek(L2, 4, FALSE), Tr);
    l.s(", with offset $", Tr);       l.h(L1, Tr);  l.sn(".", Tr);
    ASSERT(X64.Peek(L2, 4, FALSE) = 0);
    X64.Patch(L2, 4, X64.Peek(L2, 4, FALSE) + L1);
    L1 := L0
  END;
  RETURN L1
END merged;

PROCEDURE Not*(VAR x: Item);   (* x := ~x *)
VAR t: INTEGER;
BEGIN Trace(TraceAny, "Not(x: "); l.Item(x, Tr); l.sn(")", Tr);
  IF x.mode # Cond THEN LoadFlags(x) END;
  x.n := InvertedCondition(x.n);
  t := x.offset;  x.offset := x.index;  x.index := t
END Not;

PROCEDURE Or1*(VAR x: Item);   (* x := x OR *)
BEGIN Trace(TraceAny, "Or1(x: "); l.Item(x, Tr); l.sn(")", Tr);
  IF x.mode # Cond THEN LoadFlags(x) END;
  ASSERT(x.n > 0);
  IF x.n > 0 THEN
    IF x.n = 1 THEN  (* condition TRUE *)
      X64.Emit(0E9H);
    ELSE
      X64.Emit(0FH); X64.Emit(x.n);
    END;
    X64.EmitBytes(4, x.index);
    x.index := X64.PC - 4;
    Disassemble(x.mode, "Or1");
    FixLink(x.offset);  x.offset := 0
  END;
END Or1;

PROCEDURE Or2*(VAR x, y: Item);   (* x := x OR y *)
BEGIN
  Trace(TraceAny, "Or2(x: "); l.Item(x, Tr);
  l.s(", y: ", Tr);           l.Item(y, Tr); l.sn(")", Tr);
  IF y.mode # Cond THEN LoadFlags(y) END;
  x.index  := merged(y.index, x.index);
  x.offset := y.offset;
  x.n      := y.n
END Or2;


PROCEDURE And1*(VAR x: Item);   (* x := x & *)
BEGIN Trace(TraceAny, "And1(x: "); l.Item(x, Tr); l.sn(")", Tr);
  IF x.mode # Cond THEN LoadFlags(x) END;
  ASSERT(x.n # 1);
  IF x.n # 1 THEN
    IF x.n = 0 THEN
      X64.Emit(0E9H)
    ELSE
      X64.Emit(0FH); X64.Emit(InvertedCondition(x.n));
    END;
    X64.EmitBytes(4, x.offset);
    x.offset := X64.PC - 4;
    Disassemble(x.mode, "And1");
    FixLink(x.index); x.index := 0
  END;
END And1;

PROCEDURE And2*(VAR x, y: Item);   (* x := x & y *)
BEGIN
  Trace(TraceAny, "And2(x: "); l.Item(x, Tr);
  l.s(", y: ", Tr);           l.Item(y, Tr); l.sn(")", Tr);
  IF y.mode # Cond THEN LoadFlags(y) END;
  x.offset := merged(y.offset, x.offset);
  x.index  := y.index;
  x.n      := y.n
END And2;


PROCEDURE OpToIntCondition(op: INTEGER; signed: BOOLEAN): INTEGER;
VAR result: INTEGER;
BEGIN
  IF    op = ORS.eql THEN result := CZ
  ELSIF op = ORS.neq THEN result := CNZ
  ELSIF op = ORS.lss THEN IF signed THEN result := CL  ELSE result := CC  END
  ELSIF op = ORS.leq THEN IF signed THEN result := CNG ELSE result := CNA END
  ELSIF op = ORS.gtr THEN IF signed THEN result := CG  ELSE result := CA  END
  ELSIF op = ORS.geq THEN IF signed THEN result := CNL ELSE result := CNC END
  ELSE ASSERT(FALSE)
  END
RETURN result END OpToIntCondition;


(*
      80   r/m8,  imm8
      81   r/m16, imm16
      81   r/m32, imm32
rex.w 81   r/m64, imm32
      83   r/m16, imm8
      83   r/m32, imm8
rex.w 83   r/m64, imm8
*)

PROCEDURE Compare(VAR x, y: Item);
VAR immsize: INTEGER;
BEGIN
  Depar(x);  Depar(y);
  ASSERT(x.mode >= Const);  ASSERT(y.mode >= Const);
  IF (y.mode = Const) & (y.n >= -80000000H) & (y.n < 80000000H) THEN
    ASSERT(x.mode # Const);
    IF x.type.size <= 4 THEN immsize := x.type.size ELSE immsize := 4 END;
    IF (y.n >= -80H) & (y.n < 80H) THEN immsize := 1 END;
    IF    x.type.size = 1 THEN OpItem(80H, 7, x.type.size, immsize, x)
    ELSIF immsize     = 1 THEN OpItem(83H, 7, x.type.size, immsize, x)
    ELSE                       OpItem(81H, 7, x.type.size, immsize, x)
    END;
    X64.EmitBytes(immsize, y.n);
  ELSE
    Dyadic(Cmp, x, y);
  END;
  Disassemble(x.mode, "Compare")
END Compare;


(* TODO support e.g.  'boolvar = (a = b)' *)
PROCEDURE IntRelation*(op: INTEGER; VAR x, y: Item);   (* x := x < y *)
VAR immrel: BOOLEAN;
BEGIN
  Trace(TraceAny, "IntRelation("); l.Sym(op, Tr);
  l.s(", x: ", Tr);               l.Item(x, Tr);
  l.s(", y: ", Tr);               l.Item(y, Tr); l.sn(")", Tr);
  ASSERT(x.mode >= Const);
  IF y.mode < Const THEN l.s("** y.mode < Const, is ", Tr); l.i(y.mode, Tr); l.sn(" **", Tr) END;
  ASSERT(y.mode >= Const);
  IF (x.mode = Const) & (y.mode = Const) THEN
    IF    op = ORS.lss THEN immrel := x.n <  y.n
    ELSIF op = ORS.leq THEN immrel := x.n <= y.n
    ELSIF op = ORS.eql THEN immrel := x.n =  y.n
    ELSIF op = ORS.geq THEN immrel := x.n >= y.n
    ELSIF op = ORS.gtr THEN immrel := x.n >  y.n
    ELSIF op = ORS.neq THEN immrel := x.n #  y.n
    ELSE ASSERT(FALSE); immrel := FALSE
    END;
    IF immrel THEN SetCondition(x, 1) ELSE SetCondition(x, 0) END
  ELSE
    Compare(x, y);
    FreeRegs(x);  FreeRegs(y);  (* Result is in flags *)
    SetCondition(x, OpToIntCondition(op, X64.IsSigned(x.type)))
  END
END IntRelation;

PROCEDURE RealRelation*(op: INTEGER; VAR x, y: Item);   (* x := x < y *)
BEGIN
  Trace(TraceAny, "RealRelation("); l.Sym(op, Tr);
  l.s(", x: ", Tr); l.Item(x, Tr);
  l.s(", y: ", Tr); l.Item(y, Tr); l.sn(")", Tr);
  ASSERT(FALSE)
END RealRelation;



PROCEDURE StringRelation*(op: INTEGER; VAR x, y: Item);   (* x := x < y *)
VAR xadr, xlen, yadr, ylen, saveax, xchgcx, xchgsi, lbl: INTEGER;
BEGIN
Trace(TraceAny, "StringRelation("); l.Sym(op, Tr);
  l.s(", x: ", Tr); l.Item(x, Tr);
  l.s(", y: ", Tr); l.Item(y, Tr); l.sn(")", Tr);

  xadr := Reserve(RSI, {RAX, RCX});
  xlen := Reserve(RCX, {RAX});
  LoadStructure(xadr, xlen, x);
  Disassemble(x.mode, "Load string x");

  yadr := Reserve(RDI, {RAX});
  ylen := Reserve(RDX, {RAX});
  LoadStructure(yadr, ylen, y);
  Disassemble(y.mode, "Load string y");

  SaveExchange(xadr, RSI, xchgsi);
  SaveExchange(xlen, RCX, xchgcx);
  SaveExchange(-1,   RAX, saveax);
  Disassemble(0, "Adjust registers for loop lodsb");

  OpModRegRm(003BH, Reg, RCX, 8, y.index, -1, 0, 0, 0); (* cmp    xlen,ylen *)
  OpModRegRm(0F47H, Reg, RCX, 8, y.index, -1, 0, 0, 0); (* cmova  xlen,ylen *)
  Disassemble(0, "Determine shorter string length");

  OpModRegRm(Minus+3, Reg, y.n, 8, RSI, -1, 0, 0, 0);   (* sub    y.n,rsi      *)
  AluOpFromImmediate(Minus, Reg, 1, 8, y.n, -1, 0, 0);  (* dec    y.n          *)
  Disassemble(0, "String offset y[n] from x[n+1]");
  lbl := X64.PC;                                        (* lbl:                     *)
  X64.Emit(0ACH);                                       (* lodsb                    *)
  OpModRegRm(Cmp+2, Eadr, RAX, 1, RSI, y.n, 0, 0, 0);   (* cmp    al,[rsi+y.n] *)
  Disassemble(0, "Compare one byte");

  X64.Emit(75H);  X64.Emit(0CH);                        (* jne    end               *)
  Disassemble(0, "If string mismatch");

  X64.Emit(8); X64.Emit(0C0H);                          (* or     al,al             *)
  X64.Emit(0E0H); X64.Emit(lbl-(X64.PC+1));             (* loopne lbl               *)
  Disassemble(0, "If not out of bytes, loop back");

  X64.Emit(74H);  X64.Emit(6);                          (* je     end               *)
  Disassemble(0, "If strings match");

  CallSysFn(H.UnterminatedStringProc);  (* error handler *)
  Disassemble(0, "Unterminated string error trap");

  RestoreReg(RSI, xchgsi);
  RestoreReg(RCX, xchgcx);
  RestoreReg(RAX, saveax);
  Disassemble(0, "Restore saved registers");

  FreeRegs(x);  FreeRegs(y);

  SetCondition(x, OpToIntCondition(op, FALSE))
END StringRelation;




(* ---- Arithmetic operators ---- *)

PROCEDURE AddOp*(op: INTEGER; VAR x, y: Item);   (* x := x +- y *)
BEGIN
  Trace(TraceAny, "AddOp("); l.Sym(op, Tr);
  l.s("; x: ", Tr);         l.Item(x, Tr);
  l.s(", y: ", Tr);         l.Item(y, Tr); l.sn(")", Tr);
  ASSERT(op IN {ORS.plus, ORS.minus});

  IF (x.mode = Reg) & (x.n IN Free) THEN
    l.s("** x register ", Tr); l.Reg(8, x.n, Tr); l.sn(" not reserved.", Tr);
  END;
  IF (y.mode = Reg) & (y.n IN Free) THEN
    l.s("** y register ", Tr); l.Reg(8, y.n, Tr); l.sn(" not reserved.", Tr);
  END;

  IF (x.mode = Const) & (y.mode = Const) THEN
    IF op = ORS.plus THEN INC(x.n, y.n) ELSE DEC(x.n, y.n) END
  ELSE
    (* Convert op to x86 opcode *)
    IF op = ORS.plus THEN op := Plus ELSE op := Minus END;
    ASSERT(x.type.form = ORB.Int64);
    ASSERT(x.type.ref IN {ORB.Byte,ORB.Int8,ORB.Int16,ORB.Card16,ORB.Int32,ORB.Card32,ORB.Int64});
    Load(x);  x.type := ORB.intType;  (* Load changes short integers to full 64 bit integers *)
    Dyadic(op, x, y);  FreeRegs(y);
    Disassemble(y.mode, "AddOp")
  END
END AddOp;


PROCEDURE log2(m: INTEGER; VAR exponent: INTEGER): INTEGER;
BEGIN exponent := 0;
  WHILE ~ODD(m) DO m := m DIV 2; INC(exponent) END;
  RETURN m
END log2;

PROCEDURE MulImmediate(VAR x: Item; y: INTEGER);  (* x := x * y *)
VAR reg: INTEGER;
BEGIN
  ASSERT(x.mode >= Const);
  IF (y < -80000000H) OR (y >= 80000000H) THEN
    reg := Reserve(-1, {});
    LoadImmediate(reg, y);
    OpModRegRm(0FAFH, Reg, x.n, 8, reg, -1, 0, 0, 0);
    Release(reg)
  ELSE
    Depar(x);
    IF (x.mode IN {Reg, Eadr}) & (x.n # RSP) THEN reg := x.n ELSE reg := Reserve(-1, {}) END;
    IF (y < -80H) OR (y >= 80H) THEN
      OpItem(69H, reg, 8, 4, x);  X64.EmitBytes(4, y)  (* imul reg,x,imm32 *)
    ELSE
      OpItem(6BH, reg, 8, 1, x);  X64.Emit(y)          (* imul reg,x,imm8 *)
    END;
    FreeRegs(x);  x.mode := Reg;  x.n := reg;  ReserveReg(reg)
  END
END MulImmediate;

PROCEDURE MulOp*(VAR x, y: Item);   (* x := x * y *)
VAR exponent: INTEGER;  ti: Item;
BEGIN Trace(TraceAny, "MulOp(x: "); l.Item(x, Tr); l.s(", y: ", Tr); l.Item(y, Tr); l.sn(")", Tr);
  ASSERT(x.mode >= Const);  ASSERT(y.mode >= Const);
  IF (x.mode = Const) & (y.mode = Const) THEN
    x.n := x.n * y.n
  ELSE
    IF x.mode = Const THEN ti := x;  x:= y;  y := ti END;
    IF y.mode = Const THEN
      (* TODO: if y.n = 2 use shorter shl r/m64,1 instruction REX D1 /4 *)
      IF (y.n >= 2) & (log2(y.n, exponent) = 1) THEN
        Load(x);
        OpModRegRm(0C1H, Reg, 4, 8, x.n, -1, 0, 0, 1);  X64.Emit(exponent);  (* shl *)
      ELSE
        MulImmediate(x, y.n)
      END
    ELSE
      Load(x); Depar(y);
      IF y.type.size # 8 THEN Load(y) END;
      OpItem(0FAFH, x.n, 8, 0, y)  (* imul x.n,y *)
    END;
    FreeRegs(y);
    Disassemble(y.mode, "MulOp")
  END
END MulOp;

PROCEDURE DivOp*(op: INTEGER; VAR x, y: Item);   (* x := x op y *)
VAR
  exponent: INTEGER;
  name:     ARRAY 4 OF CHAR;
  saveRAX:  BOOLEAN;
  saveRDX:  BOOLEAN;
  reg:      INTEGER;
  reg2:     INTEGER;
BEGIN
  IF op = ORS.div THEN name := "DIV" ELSE name := "MOD" END;
  Trace(TraceAny, "DivOp("); l.s(name, Tr);
  l.s(", x: ", Tr);         l.Item(x, Tr);
  l.s(", y: ", Tr);         l.Item(y, Tr);  l.sn(")", Tr);
  ASSERT(x.mode >= Const);  ASSERT(y.mode >= Const);
  saveRAX := FALSE;  saveRDX := FALSE;

  IF (x.mode = Const) & (y.mode = Const) THEN
    IF op = ORS.div THEN x.n := x.n DIV y.n ELSE x.n := x.n MOD y.n END
  ELSE
    IF (y.mode = Const) & (y.n >= 2) & (log2(y.n, exponent) = 1) THEN

      Load(x);
      IF op = ORS.div THEN (* DIV by power of 2 *)
        OpModRegRm(0C1H, Reg, 7, 8, x.n, -1, 0, 0, 1);  X64.Emit(exponent);  (* sar *)
      ELSE (* MOD power of 2 *)
        AluOpFromImmediate(And, Reg, y.n-1, 8, x.n, -1, 0, 0);
      END;
      reg := x.n;  (* Result in x.n *)
      Disassemble(0, "DivOp DIV/MOD by power of 2")

    ELSE (* DIV or MOD non-power of 2 *)

      IF (x.mode = Reg) & (x.n = RAX) THEN
        (* numerator is already in rax *)
      ELSE
        IF ~((RAX IN Free) OR UsesReg(x, RAX)) THEN
          (* Save rax temporarily *)
          saveRAX := TRUE;
          PushReg(RAX);  X64.AdjustStack(1);  Release(RAX);
          Disassemble(0, "DivOp save rax")
        END;
        LoadReg(RAX, x);  ReserveReg(RAX);
        Disassemble(0, "DivOp load numerator to rax")
      END;

      IF (y.mode IN {Reg, Eadr}) & (y.n = RDX) THEN
        reg2 := Reserve(-1, {});
        IF y.mode = Reg THEN
          MoveReg(reg2, RDX)
        ELSE
          LoadReg(reg2, y)
        END;
        y.n := reg2;
        Release(RDX)
      ELSIF ~(RDX IN Free) THEN
        Depar(y);
        saveRDX := TRUE;
        PushReg(RDX);  X64.AdjustStack(1);
        Release(RDX)
      END;
      (*LoadImmediate(RDX, 0);*)
      X64.Emit(48H); X64.Emit(99H);  (* cqo - sign extend rax into rdx *)
      ReserveReg(RDX);
      Disassemble(0, "DivOp prepare rdx");

      IF y.mode = Const THEN
        reg := Reserve(-1, {});  LoadImmediate(reg, y.n);
        OpModRegRm(0F7H, Reg, 7, 8, reg, -1, 0, 0, 0);   (* idiv reg *)
        Release(reg)
      ELSE
        Depar(y);
        IF y.type.size # 8 THEN Load(y) END;
        OpItem(0F7H, 7, 8, 0, y);                        (* idiv y *)
      END;
      Disassemble(y.mode, "DivOp divide");

      IF op = ORS.div THEN  (* DIV Result in RAX *)

        IF saveRDX THEN
          PopReg(RDX);  X64.AdjustStack(-1);  ReserveReg(RDX);
          Disassemble(0, "Divop DIV restore rdx")
        END;

        IF saveRAX THEN
          reg := Reserve(-1, {});  MoveReg(reg, RAX);
          Disassemble(0, "Move result out of rax");
          PopReg(RAX);  X64.AdjustStack(-1);  ReserveReg(RAX);
          Disassemble(0, "Divop DIV restore rax")
        ELSE
          reg := RAX
        END;

      ELSE (* MOD result in RDX *)

        IF saveRDX THEN
          reg := Reserve(-1, {});  MoveReg(reg, RDX);
          Disassemble(0, "Move result out of rdx");
          PopReg(RDX);  X64.AdjustStack(-1);
          Disassemble(0, "Divop MOD restore rdx")
        ELSE
          reg := RDX
        END;

        IF saveRAX THEN
          PopReg(RAX);  X64.AdjustStack(-1);
          Disassemble(0, "Divop MOD restore rax")
        END

      END;

      IF ~saveRAX THEN Release(RAX) END;
      IF ~saveRDX THEN Release(RDX) END

    END;

    FreeRegs(x);  FreeRegs(y);
    x.mode := Reg;
    x.n    := reg;
    ReserveReg(reg);
    IF saveRAX THEN ReserveReg(RAX) END;
    IF saveRDX THEN ReserveReg(RDX) END;
    Disassemble(0, name)
  END;
END DivOp;


PROCEDURE RealOp*(op: INTEGER; VAR x, y: Item);   (* x := x op y *)
BEGIN
  Trace(TraceAny, "RealOp("); l.Sym(op, Tr);
  l.s("; x: ", Tr);         l.Item(x, Tr);
  l.s(", y: ", Tr);         l.Item(y, Tr); l.sn(")", Tr);
  ASSERT(FALSE)
END RealOp;

PROCEDURE Neg*(VAR x: Item);   (* x := -x *)
VAR size, op: INTEGER;
BEGIN Trace(TraceAny, "Neg(x: "); l.Item(x, Tr); l.n(Tr);
  IF x.type.form = ORB.Int64 THEN
    IF x.mode = Const THEN
      x.n := -x.n
    ELSE
      Load(x); OpModRegRm(0F7H, Reg, 3, 8, x.n, -1, 0, 0, 0);
      Disassemble(x.mode, "Neg")
    END
  ELSIF x.type.form = ORB.Set THEN
    IF x.mode = Const THEN
      x.n := -x.n - 1
    ELSE
      Load(x); OpModRegRm(0F7H, Reg, 2, 8, x.n, -1, 0, 0, 0);
      Disassemble(x.mode, "Neg")
    END
  ELSIF x.type.form = ORB.Real THEN
    ASSERT(FALSE)
  ELSE
    ASSERT(FALSE)
  END
END Neg;


(* ---- selection - fields, indices & pointers ---- *)

PROCEDURE Field*(VAR x: Item; y: ORB.Object);   (* x := x.y *)
BEGIN
  Trace(TraceAny, "Field(x: "); l.Item(x, Tr);
  l.s(", y: ", Tr); l.Object(y, Tr); l.sn(")", Tr);
  ASSERT(x.mode IN {Stkind, Strucp, Eadr, Global, Import});
  ASSERT(y.class = ORB.Fld);
  IF x.mode IN {Stkind, Strucp} THEN x.mode := Stkind END;
  INC(x.offset, y.val)
END Field;

PROCEDURE Index*(VAR x, y: Item);   (* x := x[y] *)
VAR elemsize: INTEGER;  xlength: Item;
BEGIN Trace(TraceAny, "Index(x: "); l.Item(x, Tr); l.s(", y: ", Tr); l.Item(y, Tr); l.n(Tr);
  ASSERT(x.type.form = ORB.Array);
  ASSERT(x.mode IN {Stkind, Strucp, Eadr, String, Global, Import});
  elemsize := x.type.base.size;
  Trace(TraceAny, "  "); l.s("x base type: ", Tr); l.Type(x.type.base, Tr); l.n(Tr);
  Trace(TraceAny, "  "); l.s("elemsize: ", Tr); l.i(elemsize, Tr); l.n(Tr);

  (* Check index in range *)
  IF ~ORS.Hasty THEN
    IF (y.mode = Const) & ((y.n < 0) OR ((x.type.len >= 0) & (y.n >= x.type.len))) THEN
      ORS.Mark("Bad index")
    ELSIF (y.mode # Const) OR (x.type.len < 0) THEN
      IF x.type.len < 0 THEN  ASSERT(x.mode = Strucp);
        H.ZeroFill(xlength);
        xlength.type   := ORB.intType;
        xlength.mode   := Eadr;
        xlength.n      := RSP;
        xlength.offset := x.n + 8;
        xlength.index  := -1
      ELSE
        MakeConstItem(xlength, ORB.intType, x.type.len)
      END;
      Compare(y, xlength);
      X64.Emit(72H);   X64.Emit(6);  (* jb: if index within array *)
      CallSysFn(H.IndexOutOfRangeProc);
      Disassemble(0, "Array index trap");
      FreeRegs(xlength)
    END;
    Trace(TraceAny, "  After range check y: "); l.Item(y, Tr); l.n(Tr)
  END;

  (* Index into array *)
  IF y.mode = Const THEN
    INC(x.offset, y.n * elemsize)
  ELSE
    IF x.mode = Strucp THEN x.mode := Stkind END;
    IF (elemsize IN {1, 2, 4, 8})
     & (x.mode IN {Stkind, Strucp, Eadr, Code, String, Global, Import})
     & (x.index < 0) THEN
      (* Use X64 SIB index and scale *)
      x.scale := 0;
      IF    elemsize = 2 THEN x.scale := 1
      ELSIF elemsize = 4 THEN x.scale := 2
      ELSIF elemsize = 8 THEN x.scale := 3
      END;
      Load(y);
      x.index := y.n;
      FreeRegs(y);
      ReserveReg(x.index)
    ELSE
      (* Apply index to base address in x *)
      LoadAddress(x);  x.mode := Eadr;
    (*AluOpToReg(Plus, y.mode, x.n, X64.IsSigned(y.type), y.type.size, y.n, y.index, y.scale, y.offset);*)
      Trace(TraceAny, "  "); l.s("MulImmediate by ", Tr); l.i(elemsize, Tr); l.sn(".", Tr);
      MulImmediate(y, elemsize);
      Trace(TraceAny, "  "); l.s("MulImmediate result y.mode ", Tr); l.Mode(y.mode, Tr); l.sn(".", Tr);
      AluOpToReg(Plus, y.mode, x.n, X64.IsSigned(y.type), y.type.size, y.n, y.index, y.scale, y.offset);
      FreeRegs(y)
    END
  END;
  Disassemble(y.mode, "Index")
END Index;


(* ---- Control flow ---- *)

(* Use of Item.o fields for conditions:
*
*  o.n      - the condition code (uses symbol encodings ORS.eql .. ORS.geq)
*  o.offset - address of prev instr if any needing patching with curr target
*  o.index  - address of any forward jump list targeting this instruction
*)

PROCEDURE CFJump*(VAR x: X64.Item);
VAR cond: INTEGER;
BEGIN Trace(TraceAny, "CFJump(x: "); l.Item(x, Tr);  l.sn(")", Tr);
  IF x.mode # Cond THEN LoadFlags(x) END;
  cond := InvertedCondition(x.n);
  ASSERT(cond >= 0);
  IF cond > 0 THEN
    IF cond = 1 THEN  (* unconditional forward jump *)
      X64.Emit(0E9H)
    ELSE
      ASSERT(cond DIV 16 = 8);
      X64.Emit(0FH);  X64.Emit(cond)
    END;
    X64.EmitBytes(4, x.offset);  x.offset := X64.PC - 4;
    FixLink(x.index);  (* Fix any links that target following this instruction *)
    Disassemble(0, "CFJump")
  END
END CFJump;

PROCEDURE FJump*(VAR L: INTEGER);
BEGIN Trace(TraceAny, "FJump(L "); l.i(L, Tr); l.sn(")", Tr);
  X64.Emit(0E9H);  X64.EmitBytes(4, L);  L := X64.PC-4;
  Disassemble(0, "FJump")
END FJump;

PROCEDURE BJump*(L: INTEGER);
BEGIN Trace(TraceAny, "BJump(L "); l.i(L, Tr); l.sn(")", Tr);
  IF L - (X64.PC + 1) >= -80H THEN
    X64.Emit(0EBH);  X64.Emit(L - (X64.PC + 1))
  ELSE
    X64.Emit(0E9H);  X64.EmitBytes(4, L - (X64.PC + 4))
  END;
  Disassemble(0, "BJump")
END BJump;

PROCEDURE CBJump*(VAR x: X64.Item; L: INTEGER);
VAR cond, disp: INTEGER;
BEGIN
  Trace(TraceAny, "CBJump(x: "); l.Item(x, Tr);
  l.s(", L: ", Tr);             l.i(L, Tr); l.sn(")", Tr);
  IF x.mode # Cond THEN LoadFlags(x) END;
  cond := InvertedCondition(x.n);
  ASSERT(L < X64.PC);
  IF cond DIV 16 = 8 THEN
    IF L - (X64.PC + 1) >= -80H THEN
      X64.Emit(70H + cond MOD 16);    X64.Emit(L - (X64.PC + 1))
    ELSE
      X64.Emit(0FH);  X64.Emit(cond); X64.EmitBytes(4, L - (X64.PC + 4))
    END
  ELSE
    IF cond # 1 THEN ASSERT(cond = 0) ELSE
      (* Unconditional backward jump *)
      IF L - (X64.PC + 1) >= -80H THEN
        X64.Emit(0EBH);  X64.Emit(L - (X64.PC + 1))
      ELSE
        X64.Emit(0E9H);  X64.EmitBytes(4, L - (X64.PC + 4))
      END
    END
  END;
  Disassemble(0, "CBJump");
  FixLink(x.index);
  FixLinkWith(x.offset, L)
END CBJump;

PROCEDURE FixOne*(at: INTEGER);
BEGIN Trace(TraceAny, "FixOne(at "); l.i(at, Tr); l.sn(")", Tr);
  ASSERT(FALSE)
END FixOne;

PROCEDURE Here*(): INTEGER;  BEGIN RETURN X64.PC END Here;

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
VAR loopvar: Item;
BEGIN
  Trace(TraceAny, "For0(var: "); l.Item(var, Tr);
  l.s(", ctl: ", Tr);           l.Item(ctl, Tr); l.sn(")", Tr);
  (*
  Load(ctl);
  Depar(var);
  ASSERT(ctl.mode = Reg);
  ASSERT(var.mode IN {Eadr, Global, Import});
  *)
  (* Store ctl into var without changing var *)
  loopvar := var;
  IF loopvar.mode = Eadr THEN ASSERT(loopvar.n = RSP)
  ELSE ASSERT(loopvar.mode IN {Global, Import}) END;
  Store(loopvar, ctl);  FreeRegs(loopvar);  FreeRegs(ctl);
  Disassemble(0, "For0");
END For0;


PROCEDURE For1*(VAR var, ctl, lim, inc: Item; VAR L: INTEGER);
VAR loopvar: Item;
BEGIN
  Trace(TraceAny, "For1(var: "); l.Item(var, Tr);
  l.s(", ctl: ", Tr);           l.Item(ctl, Tr);
  l.s(", lim: ", Tr);           l.Item(lim, Tr);
  l.s(", inc: ", Tr);           l.Item(inc, Tr); l.sn(")", Tr);

  loopvar := var;
  IF loopvar.mode = Eadr THEN ASSERT(loopvar.n = RSP)
  ELSE ASSERT(loopvar.mode IN {Global, Import}) END;

  ASSERT(inc.mode = Const);
  IF inc.n = 0 THEN ORS.Mark("zero increment"); inc.n := 1 END;

  Dyadic(Cmp, loopvar, lim);  FreeRegs(lim);
  X64.Emit(0FH);
  IF inc.n > 0 THEN X64.Emit(8FH) ELSE X64.Emit(8CH) END; (* jg/jl *)
  L := X64.PC;
  X64.EmitBytes(4, 0);

  FreeRegs(loopvar);

  Disassemble(var.mode, "For1");
END For1;


PROCEDURE For2*(VAR var, ctl, inc: Item);
BEGIN
  Trace(TraceAny, "For2(var: "); l.Item(var, Tr);
  l.s(", ctl: ", Tr);           l.Item(ctl, Tr);
  l.s(", inc: ", Tr);           l.Item(inc, Tr); l.sn(")", Tr);

  IF var.mode = Eadr THEN ASSERT(var.n = RSP)
  ELSE ASSERT(var.mode IN {Global, Import}) END;

  Dyadic(Plus, var, inc);  FreeRegs(inc);
  FreeRegs(var);
  CheckRegs("For loop termination");
  Disassemble(0, "For2");
END For2;


(* ---- Type descriptor construction and testing ---- *)

PROCEDURE EmitInt(i: INTEGER;  comment: ARRAY OF CHAR);
BEGIN
  IF Disasmpc < X64.PC THEN Disassemble(0, "") END;
  ASSERT(Disasmpc = X64.PC);
  X64.EmitBytes(8, i);
  IF ORS.ListAssembly THEN
    l.DisassembleInt(Disasmpc, comment)
  ELSE Disasmpc := X64.PC
  END
END EmitInt;

PROCEDURE Q(T: ORB.Type);
BEGIN (*one entry of type descriptor extension table*)
  IF T.base # NIL THEN
    Q(T.base);
    ImpFix[ImpFInx].offset := X64.PC;
    ImpFix[ImpFInx].fix    := 0FFFF0000H;  INC(ImpFInx);
    IF T.mno = 0 THEN
      EmitInt(T.len, "type extension local module offset")
    ELSE
      EmitInt(100000000H + T.mno * 10000H + T.len,
            "type extension: 32/1, 16/mno, 16/import")
    END;
  END
END Q;

PROCEDURE FindPtrFlds(typ: ORB.Type; off: INTEGER);
VAR
  fld:  ORB.Object;
  i, s: INTEGER;
BEGIN
  IF (typ.form = ORB.Pointer) & ~typ.native OR (typ.form = ORB.NilTyp) THEN
    EmitInt(off, "pointer/niltype field offset")
  ELSIF typ.form = ORB.Record THEN
    fld := typ.dsc;
    WHILE fld # NIL DO
      FindPtrFlds(fld.type, fld.val + off);
      fld := fld.next
    END
  ELSIF typ.form = ORB.Array THEN
    s := typ.base.size;
    FOR i := 0 TO typ.len-1 DO FindPtrFlds(typ.base, i*s + off) END
  END
END FindPtrFlds;


PROCEDURE ShowFields(T: ORB.Type);
VAR f: ORB.Object;
BEGIN
  f := T.dsc;
  WHILE f # NIL DO
    Trace(TraceAny, "  '"); l.s(f.name, Tr);
    l.s("': ", Tr);        l.Type(f.type, Tr); l.n(Tr);
    f := f.next
  END
END ShowFields;

PROCEDURE BuildTD*(T: ORB.Type; VAR adr: INTEGER);
VAR k, s: INTEGER;
BEGIN
  Trace(TraceAny, "BuildTD(T "); l.Type(T, Tr);
  l.s(", adr ", Tr);            l.i(adr, Tr); l.sn(")", Tr);
  ASSERT(adr MOD 8 = 0);
  ShowFields(T);
  IF Disasmpc < X64.PC THEN Disassemble(0, "") END;
  X64.Align(8);  Disasmpc := X64.PC;

  s := T.size; (* Convert size for heap allocation *)
  IF    s <= 48  THEN s := 64
  ELSIF s <= 112 THEN s := 128
  ELSIF s <= 240 THEN s := 256
  ELSE                s := (s + 527) DIV 512 * 512
  END;

  T.len := X64.PC;  (* T.len overriden to be adddress of descriptor for record types *)

  EmitInt(s, "Type size including heap overhead");
  k  := T.nofpar;  (* extension level! *)
  IF k > 3 THEN ORS.Mark("ext level too large")
           ELSE Q(T); WHILE k < 3 DO EmitInt(-1, "unused extension level"); INC(k) END
  END;
  FindPtrFlds(T, 0);
  EmitInt(-1, "end of type");

  (* Debug annotation: Emit type name *)
  IF T.typobj = NIL THEN X64.Emit(0) ELSE X64.EmitString(T.typobj.name) END;
  IF ORS.ListAssembly THEN l.DisassembleString(Disasmpc) END;
END BuildTD;

PROCEDURE TypeTest*(VAR X: X64.Item; T: ORB.Type; varpar, isguard: BOOLEAN);
VAR r1, r2, pc: INTEGER;  x: Item;  b: l.Buffer;
BEGIN
  Trace(TraceAny, "TypeTest(x: "); l.Item(x, Tr);
  l.s(", T: ", Tr);                l.Type(T, Tr);
  IF varpar  THEN l.s(", varpar, ", Tr)  ELSE l.s("~varpar, ", Tr)  END;
  IF isguard THEN l.sn(", isguard)", Tr) ELSE l.sn(", ~isguard)", Tr) END;
(*
  ASSERT(varpar = (x.mode = Strucp));
*)
  IF T = NIL THEN
    FreeRegs(X);
    SetCondition(X, 1)  (* True *)
  ELSE (*fetch tag*)
    x := X;
    IF varpar THEN
      ASSERT(x.mode = Strucp);  ASSERT(x.type.form = ORB.Record)
    ELSE
      ASSERT(x.mode # Strucp);  ASSERT(x.type.form = ORB.Pointer)
    END;
    r1 := Reserve(-1, {});
    IF x.mode = Strucp THEN
      LoadMem(r1, Eadr, FALSE, 8, RSP, -1, 0, x.n + 8 * X64.SPO + 8);
    ELSE
      LoadRegUnreserved(r1, x);  (* Load pointer value *)
      OpModRegRm(85H, Reg, r1, 8, r1, -1, 0, 0, 0);  (* test r1,r1 *)
      X64.Emit(74H);  pc := X64.PC;  X64.Emit(0);   (* jz: if NIL - which matches all types *)
      LoadMem(r1, Eadr, FALSE, 8, r1, -1, 0, -10H);  (* Load type tag *)
    END;
    Disassemble(Eadr, "TypeTest load record type tag");
    LoadMem(r1, Eadr, FALSE, 8, r1, -1, 0, T.nofpar * 8);
    Disassemble(Eadr, "Extract base type tag");
    r2 := Reserve(-1, {});
    LoadTypeAddress(r2, T);
    AluOpToReg(Cmp, Reg, r1, FALSE, 8, r2, -1, 0, 0);  (* cmp r1,r2 *)
    Release(r1); Release(r2);
    X64.Patch(pc, 1, X64.PC - (pc + 1));
    IF isguard THEN
      X64.Emit(74H);  X64.Emit(6);  (* je: if types match *)
      CallSysFn(H.TypeGuardFailureProc);
      Disassemble(0, "Guard type test ");
    ELSE
      FreeRegs(X);
      SetCondition(X, CZ);
      Disassemble(0, "'IS' Type test ");
    END
  END
END TypeTest;

PROCEDURE Open*(modid: ARRAY OF CHAR);
VAR i: INTEGER;  obj: ORB.Object;
BEGIN
l.Init(ORS.SourceFile());
  Trflags := {TraceAny, TraceMakeItem};  (*{TraceAny, TraceMakeItem};*)
  (*Trace(TraceAny, "ORG Open('"); l.s(sourcefn, Tr); l.sn("')", Tr);*)
  X64.ClearStack;   ClearRegs;
  X64.Init;
  H.ZeroFill(Hdr);
  BodyAnnotations := NIL;
  Strx            := 0;
  StrFInx         := 0;
  Varsize         := 0;
  VarFInx         := 0;
  ImpFInx         := 0;
  Disasmpc        := 0;
  X64.SetPC(SYSTEM.SIZE(ModuleDesc));
  Disasmpc := X64.PC;

  (* Emit imported module names and keys *)
  obj := ORB.topScope.next;
  WHILE (obj # NIL) & (obj.class = ORB.Mod) DO  (* imports *)
    IF obj.dsc # ORB.system THEN
      X64.EmitString(obj(ORB.Module).orgname);  (* name *)
      X64.EmitBytes(8, obj.val)                 (* key  *)
    END;
    obj := obj.next
  END;
  X64.Emit(0);
  X64.Align(16)
END Open;

PROCEDURE SetDataSize*(dc: INTEGER);
BEGIN Varsize := dc END SetDataSize;

PROCEDURE Header*;
BEGIN
  Trace(TraceAny, "Header - module initialisation code entry point."); l.n(Tr);
  Hdr.init := X64.PC;  (* Set entry point address *)
END Header;

PROCEDURE wsize(size: INTEGER);
BEGIN
  l.h(size,  Tr);  l.c("H", Tr);
  l.s(", ", Tr);   l.i(size, Tr);
END wsize;

PROCEDURE EmitPtrs(typ: ORB.Type; adr: INTEGER);
VAR fld: ORB.Object;  i, s: INTEGER;
BEGIN
  IF (~typ.native) & ((typ.form = ORB.Pointer) OR (typ.form = ORB.NilTyp)) THEN
    (*l.s("  ptrec  ", Tr); l.h(adr, Tr); l.sn("H.", Tr);*)
    X64.EmitBytes(8, adr)
  ELSIF ~typ.native & (typ.form = ORB.Record) THEN
    fld := typ.dsc;
    WHILE fld # NIL DO EmitPtrs(fld.type, fld.val + adr); fld := fld.next END
  ELSIF typ.form = ORB.Array THEN
    s := typ.base.size;
    FOR i := 0 TO typ.len-1 DO EmitPtrs(typ.base, i*s + adr) END
  END
END EmitPtrs;

PROCEDURE EmitUnsigned(n: INTEGER);
BEGIN
  ASSERT(n >= 0);
  WHILE n >= 128 DO X64.Emit(n MOD 128 + 128); n := n DIV 128 END;
  X64.Emit(n)
END EmitUnsigned;

PROCEDURE Close*(modid: ORS.Ident; key, exno: INTEGER);
(* Note: export numbers are 1 .. exno-1 *)
VAR
  i:        INTEGER;
  reslen:   INTEGER;
  obj:      ORB.Object;
  fname:    ARRAY 40 OF CHAR;
  codefile: Files.File;
  R:        Files.Rider;
  bodyan:   BodyAnnotation;
  linean:   LineAnnotation;
  line:     INTEGER;
  pc:       INTEGER;
BEGIN
  X64.Emit(0C3H);  (* ret *)
  Disassemble(0, "Return from module body");
  X64.Emit(2EH);  (* Mark end of code with ignored instruction 'seg cs' *)
  EmitStrings;

  Trace(TraceAny, "Close:"); l.n(Tr);
  l.s("  modid:         ", Tr);  l.sn(modid,  Tr);
  l.s("  key:           ", Tr);  l.h(key,     Tr);  l.n(Tr);
  l.s("  exno:          ", Tr);  l.i(exno,    Tr);  l.n(Tr);
  l.s("  code size:     ", Tr);  wsize(X64.PC);     l.n(Tr);
  l.s("  VAR size:      ", Tr);  wsize(Varsize);    l.n(Tr);
  l.s("  VAR fixups:    ", Tr);  l.i(VarFInx, Tr);  l.n(Tr);
  l.s("  Import fixups: ", Tr);  l.i(ImpFInx, Tr);  l.n(Tr);
  l.n(Tr);

  Hdr.name    := modid;
  Hdr.key     := key;
  Hdr.varsize := Varsize;

  (* Prepare to patch header pointers directly into X64.Text *)


  (* -------------------- Emit global VAR pointer table --------------------- *)
  X64.Align(16);
  Hdr.ptr := X64.PC;  (* Set global VAR pointer table address *)
  obj := ORB.topScope.next;
  WHILE obj # NIL DO
    IF obj.class = ORB.Var THEN EmitPtrs(obj.type, obj.val) END;
    obj := obj.next
  END;
  X64.EmitBytes(8, -1);


  (* -------------------------- Emit command table -------------------------- *)
  X64.Align(16);
  Hdr.cmd := X64.PC;  (* Set command table address *)
  obj := ORB.topScope.next;
  WHILE obj # NIL DO  (* commands *)
    IF (obj.exno # 0) & (obj.class = ORB.Const) & (obj.type.form = ORB.Proc)
     & (obj.type.nofpar = 0) & (obj.type.base = ORB.noType) THEN
      X64.EmitString(obj.name); X64.EmitBytes(4, obj.val)
    END;
    obj := obj.next
  END;
  X64.Emit(0);


  (* ------------------------ Emit line annotations ------------------------- *)

  X64.Align(16);
  Hdr.lines := X64.PC;
  bodyan := BodyAnnotations;
  WHILE bodyan # NIL DO
    linean := bodyan.firstline;
    IF linean = NIL THEN
      wn; ws("** Warning: body with no lines: "); ws(bodyan.name); wsn(" **");
      ASSERT(FALSE);
    ELSE
      X64.EmitString(bodyan.name);
      line   := linean.line;
      pc     := linean.pc;
      X64.EmitBytes(8, line);
      X64.EmitBytes(8, pc);
      (*
      ws("Emit body annotation for '"); ws(bodyan.name);
      ws("', first line "); wi(line);
      ws(", first PC "); wh(pc); wsn("H.");
      *)
      linean := linean.next;
      WHILE linean # NIL DO
        (*
        ws("  line "); wi(linean.line);
        ws(", PC "); wh(linean.pc); wsn("H.");
        *)
        ASSERT(linean.pc > pc);
        EmitUnsigned(linean.pc   - pc);    pc   := linean.pc;
        EmitUnsigned(linean.line - line);  line := linean.line;
        linean := linean.next
      END;
      X64.Emit(0);
    END;
    bodyan := bodyan.next
  END;
  X64.Emit(0);
  (*
  ws("Annotations took "); wi(X64.PC - Hdr.lines); wsn(" bytes.");
  *)


  (* ---------- Determine export table offset and resident length ----------- *)
  X64.Align(16);
  Hdr.exports := X64.PC;     (* Set export table address *)
  reslen := (X64.PC + exno * 4 + 15) DIV 16 * 16;
  (* Note there are exno-1 export entries plus a -1 terminator *)


  (* -------------------------- Emit export table --------------------------- *)
  l.n(Tr);  l.sn("Object file exports:", Tr);
  obj := ORB.topScope.next;
  WHILE obj # NIL DO  (* entries *)
    IF obj.exno # 0 THEN
      i := -1;
      IF    obj.class = ORB.Const                  THEN i := obj.val;  ASSERT(obj.type.form = ORB.Proc)
      ELSIF obj.class = ORB.Var                    THEN i := obj.val + reslen
      ELSIF obj.class = ORB.Typ THEN
        IF obj.type.form = ORB.Record              THEN i := obj.type.len
        ELSIF (obj.type.form = ORB.Pointer)
            & (   (obj.type.base.typobj = NIL)
               OR (obj.type.base.typobj.exno = 0)) THEN i := obj.type.base.len; ASSERT(FALSE)
        END
      END;
      ASSERT(i >= 0);
      X64.EmitBytes(4, i);
      l.s("  ", Tr);  l.i(obj.exno, Tr); l.s(": ", Tr);
      l.h(i, Tr);  l.s("H. ", Tr);  l.Object(obj, Tr); l.n(Tr)
    END;
    obj := obj.next
  END;

  (*
  (* Types required by pointers *)
  obj := ORB.topScope.next;
  WHILE obj # NIL DO  (*pointer variables*)
    IF obj.class = ORB.Var THEN EmitPtrs(obj.type, obj.val) END;
    obj := obj.next
  END;
  *)

  (*l.s("Export count ", Tr); l.i((X64.PC - Hdr.exports) DIV 4, Tr); l.sn(".", Tr);*)
  X64.EmitBytes(4, -1);  (* Mark end of table *)

  (* --------------------- End of resident code/tables ---------------------- *)

  X64.Align(16);
  IF X64.PC # reslen THEN
    l.n(Tr); l.s("** PC ", Tr);   l.h(X64.PC, Tr);
    l.s("H, reslen ", Tr);        l.h(reslen, Tr);
    l.s("H, Hdr.exports ", Tr);   l.h(Hdr.exports, Tr);
    l.s("H, exno ", Tr);          l.h(exno, Tr);
    l.sn("H **", Tr)
  END;
  ASSERT(X64.PC = reslen);
  Hdr.vars := reslen; (* Set resident size / var offset *)

  (* Relocate code references to module global VARs *)
  (*l.n(Tr); l.i(VarFInx, Tr); l.sn(" VAR relocations.", Tr);*)
  FOR i := 0 TO VarFInx-1 DO
    (*l.s("  at ", Tr);  l.h(VarFix[i], Tr);  l.sn("H.", Tr);*)
    X64.Patch(VarFix[i], 4, X64.Peek(VarFix[i], 4, FALSE) + reslen)
  END;

  (* Emit imports *)
  X64.Align(16);
  Hdr.imprefs := X64.PC;
  X64.EmitBytes(4, ImpFInx);
  FOR i := 0 TO ImpFInx - 1 DO
    X64.EmitBytes(4, ImpFix[i].offset);
    X64.EmitBytes(4, ImpFix[i].fix);
  END;
  X64.EmitBytes(4, -1);
  WHILE X64.PC MOD 16 # 0 DO X64.Emit(0) END;
  Hdr.size := X64.PC;
  Hdr.magic := "Oberon5";

  (* Display header content *)
  l.sn("Header:", Tr);
  l.s("  Size:       ", Tr);  l.h(Hdr.size,    Tr);  l.sn("H.", Tr);
  l.s("  Init code:  ", Tr);  l.h(Hdr.init,    Tr);  l.sn("H.", Tr);
  l.s("  Pointers:   ", Tr);  l.h(Hdr.ptr,     Tr);  l.sn("H.", Tr);
  l.s("  Exports:    ", Tr);  l.h(Hdr.exports, Tr);  l.sn("H.", Tr);
  l.s("  VAR offset: ", Tr);  l.h(Hdr.vars,    Tr);  l.sn("H.", Tr);
  l.s("  VAR size:   ", Tr);  l.h(Hdr.varsize, Tr);  l.sn("H.", Tr);

  (* Write compiled module *)
  fname := "";  H.Append(modid, fname);  H.Append(".x64", fname);
  codefile := Files.New(fname);  Files.Set(R, codefile, 0);
  Files.WriteBytes(R, Hdr, 0, SYSTEM.SIZE(ModuleDesc));
  Files.WriteBytes(R, X64.Text, SYSTEM.SIZE(ModuleDesc), X64.PC - SYSTEM.SIZE(ModuleDesc));
  Files.Register(codefile);

  BodyAnnotations := NIL
END Close;


(* ---- Initialisation ---- *)

BEGIN Texts.OpenWriter(W);
END ORG.
