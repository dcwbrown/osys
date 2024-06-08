MODULE Test;  (* DCWB 27 Feb 2024 *)

IMPORT SYSTEM, H := WinHost, Texts;

TYPE r = RECORD i: INTEGER END;

VAR
  W:    Texts.Writer;

(*
  p: PROCEDURE\(a, b: INTEGER);

PROCEDURE\ ab (a, b: INTEGER);
BEGIN H.ws("a "); H.wi(a); H.ws(", b "); H.wi(b); H.wsn(".")
END ab;

PROCEDURE testab*;
BEGIN
  p := ab;
  ab(1,2);
  p(1,2);
END testab;
*)

(*$la+lc+*)
PROCEDURE Obe(a, b, c, d, e, f, g, h: INTEGER): INTEGER;
VAR i: INTEGER;
BEGIN
  i := a + b + c + d + e + f + g + h;
  RETURN i
END Obe;

PROCEDURE/ Lin(a, b, c, d, e, f, g, h: INTEGER): INTEGER;
VAR i: INTEGER;
BEGIN
  i := a + b + c + d + e + f + g + h;
  RETURN i
END Lin;

PROCEDURE\ Win(a, b, c, d, e, f, g, h: INTEGER): INTEGER;
VAR i: INTEGER;
BEGIN
  i := a + b + c + d + e + f + g + h;
  RETURN i
END Win;

PROCEDURE TestHost*;
VAR i: INTEGER;
BEGIN
  H.wsn("Calling Obe.");
  i := Obe(1,2,3,4,5,6,7,8);
  H.ws("Obe returned "); H.wi(i); H.wsn(".");

  H.wsn("Calling Lin.");
  i := Lin(1,2,3,4,5,6,7,8);
  H.ws("Lin returned "); H.wi(i); H.wsn(".");

  H.wsn("Calling Win.");
  i := Win(1,2,3,4,5,6,7,8);
  H.ws("Win returned "); H.wi(i); H.wsn(".");
END TestHost;

(*$la-lc-*)

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

