MODULE Writer;  (* Character output convenience functions *)

(* Temp version for OSYS *)

IMPORT SYSTEM, P := Winshim;

VAR skipline: BOOLEAN;

PROCEDURE SkipLine*; BEGIN skipline := TRUE  END SkipLine;
PROCEDURE Noskip*;   BEGIN skipline := FALSE END Noskip;

PROCEDURE l*();
BEGIN IF skipline THEN skipline := FALSE ELSE P.wl END END l;

PROCEDURE c*(c: CHAR);           BEGIN IF ~skipline THEN P.Log(c) END END c;
PROCEDURE s*(s: ARRAY OF CHAR);  BEGIN IF ~skipline THEN P.Log(s) END END s;
PROCEDURE sl*(t: ARRAY OF CHAR); BEGIN IF ~skipline THEN P.wsl(t) END END sl;

PROCEDURE b*(n: INTEGER);
BEGIN IF ~skipline THEN WHILE n > 0 DO P.wc(" "); DEC(n) END END END b;

PROCEDURE sn*(str: ARRAY OF CHAR; n: INTEGER);  (* n<0 spaces to left, >0 spaces to right *)
VAR l, w: INTEGER;
BEGIN l := 0;
  IF n < 0 THEN w := -n ELSE w := n END;
  WHILE (l < LEN(str)) & (l < w) & (str[l] # 0X) DO INC(l) END;
  IF n < 0 THEN b(w-l); s(str) ELSE s(str); b(w-l) END
END sn;

PROCEDURE zn*(str: ARRAY OF CHAR; n: INTEGER);  (* n<0 zeroes to left, >0 spaces to right *)
VAR l: INTEGER;
BEGIN
  IF n >= 0 THEN sn(str, n)
  ELSE
    l := 0;  n := -n;
    WHILE (l < LEN(str)) & (l < n) & (str[l] # 0X) DO INC(l) END;
    WHILE n > l DO c("0"); DEC(n) END;
    s(str)
  END
END zn;

PROCEDURE h1*(i: INTEGER);
BEGIN IF i<10 THEN c(CHR(i + 48)) ELSE c(CHR(i + 55)) END END h1;

PROCEDURE h*(i: INTEGER);
VAR num: ARRAY 20 OF CHAR;
BEGIN P.IntToHex(i, num);  s(num) END h;

PROCEDURE hn*(i, n: INTEGER);
VAR num: ARRAY 20 OF CHAR;
BEGIN P.IntToHex(i, num);  zn(num, n) END hn;

PROCEDURE hs*(i: INTEGER);
BEGIN IF i < 0 THEN c("-");  i := -i END;  h(i) END hs;

PROCEDURE i*(j: INTEGER);
VAR num: ARRAY 24 OF CHAR;
BEGIN P.IntToDecimal(j, num);  s(num) END i;

PROCEDURE in*(i, n: INTEGER);
VAR num: ARRAY 24 OF CHAR;
BEGIN P.IntToDecimal(i, num);  sn(num, n) END in;

PROCEDURE DumpMem*(indent, adr, start, len: INTEGER);
BEGIN P.DumpMem(indent, adr, start, len) END DumpMem;

BEGIN skipline := FALSE;
END Writer.
