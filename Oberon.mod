MODULE Oberon;  (*$la-*) (*$lc-*)

IMPORT SYSTEM, Shim := Winshim, Texts;

VAR
  Log*: Texts.Text;
  R:    Texts.Reader;

PROCEDURE NotifyLog(T: Texts.Text; op: INTEGER; beg, end: INTEGER);
VAR buf: ARRAY 256 OF CHAR;  pos, i: INTEGER;
BEGIN
  IF op = 1 (* Insert *) THEN
    Texts.OpenReader(R, Log, beg);  pos := beg;
    WHILE pos < end DO
      i := 0;
      WHILE (pos < end) & (i < LEN(buf)-1) DO
        Texts.Read(R, buf[i]);  INC(pos);  INC(i)
      END;
      buf[i] := 0X; Shim.Log(buf);
    END
  END
END NotifyLog;

BEGIN
  NEW(Log);  Log.notify := NotifyLog;  Texts.Open(Log, "");
END Oberon.
