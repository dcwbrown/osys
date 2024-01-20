MODULE Linktest2;

IMPORT Linktest, Winshim, Files;

TYPE r = RECORD i: INTEGER END;

VAR s1, s2: ARRAY 5 OF CHAR; b: BOOLEAN;

PROCEDURE TestFiles;
VAR f: Files.File;
BEGIN
  f := Files.Old("Linktest2.mod")
END TestFiles;

PROCEDURE TestNew;
VAR f: Files.File;
BEGIN
  NEW(f)
END TestNew;

PROCEDURE TestBadPointer;
VAR p: POINTER TO r;
BEGIN
  p.i := 1;
END TestBadPointer;

BEGIN
  Linktest.ltw("Linktest2 using Linktest.ltw.");
  Winshim.wsl("LinkTest2 using Winshim.wsl.");

  s1 := "abcd";
  s2 := "abcd";
  s1[4] := "e";
  (*s2[4] := "e";*)
  b := s1 = s2;
  IF b THEN Winshim.wsl("s1 = s2") ELSE Winshim.wsl("s1 # s2") END;
  TestFiles;
  TestNew;
  TestBadPointer;
END Linktest2.
