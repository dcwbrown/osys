MODULE osys;
IMPORT SYSTEM, H := WinHost, Kernel, Display;


PROCEDURE DoCharacter(ch: INTEGER);
BEGIN
  IF ch = 1BH THEN (* ESC - TestOberon specific ... *)
    Display.Close
  END
END DoCharacter;

PROCEDURE DoMouse(x, y: INTEGER;  flags: SET);  (* {0} MR, {1} MM, {2} ML *)
BEGIN
  H.ws("DoMouse x "); H.wi(x);
  H.ws(", y "); H.wi(y);
  IF 2 IN flags THEN H.ws(" ML") END;
  IF 1 IN flags THEN H.ws(" MM") END;
  IF 0 IN flags THEN H.ws(" MR") END;
  H.wsn(".")
END DoMouse;

PROCEDURE PreDraw(x, y, width, height: INTEGER;  bitmap: Display.HostBitmap);
BEGIN END PreDraw;

PROCEDURE PostDraw(x, y, width, height: INTEGER;  bitmap: Display.HostBitmap);
BEGIN END PostDraw;


(*$la-lc-*)
BEGIN
  H.wsn("Oberon system starting.");
  Display.SetCharacterHandler(DoCharacter);
  Display.SetMouseHandler(DoMouse);
  Display.SetDrawHandlers(PreDraw, PostDraw);
  Display.Loop;
  H.wsn("Oberon system closing.");
END osys.
(*$la-lc-*)
