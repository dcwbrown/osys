MODULE Linktest2;

IMPORT Linktest, Winshim;

VAR s1, s2: ARRAY 5 OF CHAR; b: BOOLEAN;

BEGIN
  Linktest.ltw("Linktest2 using Linktest.ltw.");
  Winshim.wsl("LinkTest2 using Winshim.wsl.");

  s1 := "abcd";
  s2 := "abcd";
  s1[4] := "e";
  s2[4] := "e";
  b := s1 = s2;
  IF b THEN Winshim.wsl("s1 = s2") ELSE Winshim.wsl("s1 # s2") END;
END Linktest2.
