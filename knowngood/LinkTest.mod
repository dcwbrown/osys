MODULE Linktest;

IMPORT SYSTEM, Winshim;

PROCEDURE ltw*(s: ARRAY OF CHAR);
BEGIN
  Winshim.wsn(s);
END ltw;

BEGIN
  Winshim.wsn("Linktest starting.");
  Winshim.ws("crlf at "); Winshim.wh(SYSTEM.ADR(Winshim.crlf)); Winshim.wsn("H.");
  Winshim.ws("crlf ...");
  Winshim.ws(Winshim.crlf);
  ltw("ltw called from LinkTest");
  Winshim.wsn("Linktest complete.")
END Linktest.
