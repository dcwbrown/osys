MODULE Linktest2;

IMPORT SYSTEM, Linktest, P := Winshim, Files, Texts, Oberon, ORP;

TYPE r = RECORD i: INTEGER END;

VAR s1, s2: ARRAY 5 OF CHAR; b: BOOLEAN;

PROCEDURE TestNew;
VAR f: Files.File;
BEGIN
  NEW(f)
END TestNew;

PROCEDURE TestFiles;
VAR  f1, f2: Files.File;  r1, r2: Files.Rider;  buf: ARRAY 512 OF BYTE;
BEGIN
  f1 := Files.Old("Linktest2.mod");  Files.Set(r1, f1, 0);
  f2 := Files.New("TestFile.Txt");   Files.Set(r2, f2, 0);
  REPEAT
    Files.ReadBytes(r1, buf, LEN(buf));
    Files.WriteBytes(r2, buf, 0, LEN(buf) - r1.res)
  UNTIL r1.eof;
  Files.Register(f2);

  (*Files.DumpFiles;*)
END TestFiles;

PROCEDURE TestBadPointer;
VAR p: POINTER TO r;
BEGIN
  p.i := 1;
END TestBadPointer;

PROCEDURE code; BEGIN IF FALSE THEN ELSE END END code;

PROCEDURE TestInterrupt;
VAR b: BYTE;
BEGIN
  b := 0CDH;  SYSTEM.PUT(SYSTEM.ADR(code),   b);  (* int n    *)
  b := 010H;  SYSTEM.PUT(SYSTEM.ADR(code)+1, b);  (* n = 3    *)
  b := 0C3H;  SYSTEM.PUT(SYSTEM.ADR(code)+2, b);  (* ret      *)
  code
END TestInterrupt;

PROCEDURE TestJumpOutOfRange;
VAR b: BYTE; dw: SYSTEM.CARD32;
BEGIN
  b  := 0E9H;       SYSTEM.PUT(SYSTEM.ADR(code),   b);   (* jmp rel32 *)
  dw := 40000000H;  SYSTEM.PUT(SYSTEM.ADR(code)+1, dw);  (* ofset 1GB forwards *)
  code
END TestJumpOutOfRange;

PROCEDURE TestDivideByZero;
VAR i: INTEGER;
BEGIN
  i := 0;  i := 5 DIV i;
END TestDivideByZero;

PROCEDURE TestTexts;
VAR W: Texts.Writer;
BEGIN
  (*P.wsl("Calling Texts.OpenWriter(W)");*)
  Texts.OpenWriter(W);
  (*P.wsl("Returned from Texts.OpenWriter(W)");*)
  Texts.WriteString(W, "Texts testing.");
  Texts.WriteLn(W);
  (*
  P.ws("Calling Texts.Append, W at "); P.wh(SYSTEM.ADR(W));
  P.ws("H, W.buf at ");                P.wh(SYSTEM.VAL(INTEGER, W.buf));
  P.ws("H, W.buf.len ");               P.wh(W.buf.len);
  P.wsl("H.");
  *)
  Texts.Append(Oberon.Log, W.buf);
END TestTexts;

PROCEDURE TestCompilation();
BEGIN
  P.wsl("Testing compilation of LinkTest.mod.");
  ORP.CompileFile("LinkTest.mod");
END TestCompilation;

BEGIN
  Linktest.ltw("Linktest2 using Linktest.ltw.");
  P.wsl("LinkTest2 using P.wsl.");

  s1 := "abcd";
  s2 := "abcd";
  s1[4] := "e";
  (*s2[4] := "e";*)
  b := s1 = s2;
  IF b THEN P.wsl("s1 = s2") ELSE P.wsl("s1 # s2") END;
  TestNew;
  TestFiles;
  TestTexts;
  (*TestTexts;*)
  (*TestJumpOutOfRange;*)
  (*TestBadPointer;*)
  (*TestInterrupt;*)
  (*TestDivideByZero;*)
  TestCompilation;
END Linktest2.
