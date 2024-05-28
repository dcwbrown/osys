MODULE Test;  (* DCWB 27 Feb 2024 *)

IMPORT H := WinHost, Texts;

TYPE r = RECORD i: INTEGER END;

VAR W: Texts.Writer;

PROCEDURE ArrayIndexTest*;
VAR i: INTEGER; a: ARRAY 10 OF INTEGER;
BEGIN
  FOR i := 0 TO 20 DO
    Texts.Write(W, " ");  Texts.WriteInt(W, i, 1);  Texts.Append(Texts.Log, W.buf);
    a[i] := i
  END;
END ArrayIndexTest;

PROCEDURE NilDereferenceTest*;
VAR p: POINTER TO r;
BEGIN
  INC(p.i)
END NilDereferenceTest;

PROCEDURE DivideByZeroTest*;
VAR i: INTEGER;
BEGIN
  i := 0;
  i := 1 DIV i
END DivideByZeroTest;

PROCEDURE TrapAlpha*; BEGIN H.Trap(0, "Alpha") END TrapAlpha;
PROCEDURE TrapBeta*;  BEGIN H.Trap(-1, "Beta") END TrapBeta;

BEGIN
  Texts.OpenWriter(W);
  Texts.WriteString(W, "Test loaded.");
  Texts.Append(Texts.Log, W.buf)
END Test.

ORP.Compile Test.Mod/s ~
Test.RunArrayIndexTest
Test.RunNilDereferenceTest
Test.TrapAlpha
Test.TrapBeta

