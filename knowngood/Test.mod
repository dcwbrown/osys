MODULE Test;  (* DCWB 27 Feb 2024 *)

IMPORT H := WinHost;

TYPE r = RECORD i: INTEGER END;

PROCEDURE RunArrayIndexTest*;
VAR
  i: INTEGER;
  a: ARRAY 10 OF INTEGER;
BEGIN
  FOR i := 0 TO 20 DO
    H.wc(" ");  H.wi(i);
    a[i] := i
  END;
END RunArrayIndexTest;

PROCEDURE RunNilDereferenceTest*;
VAR p: POINTER TO r;
BEGIN
  H.wi(p.i)
END RunNilDereferenceTest;

BEGIN H.wsn("Test loaded.")
END Test.

ORP.Compile Test.Mod/s ~
Test.RunArrayIndexTest
Test.RunNilDereferenceTest
