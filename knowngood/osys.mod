MODULE osys;
IMPORT SYSTEM, H := WinHost, Kernel, HostWindow := WinHostWindow, Display;


PROCEDURE DoCharacter(ch: INTEGER);
BEGIN
  IF ch = 1BH THEN (* ESC - TestOberon specific ... *)
    HostWindow.Quit
  END
END DoCharacter;

PROCEDURE DoMouse(x, y: INTEGER;  flags: SET);  (* {0} MR, {1} MM, {2} ML *)
BEGIN END DoMouse;

PROCEDURE PreDraw(x, y, width, height: INTEGER;  bitmap: HostWindow.Bitmap);
BEGIN END PreDraw;

PROCEDURE PostDraw(x, y, width, height: INTEGER;  bitmap: HostWindow.Bitmap);
BEGIN END PostDraw;


PROCEDURE Loop*;
VAR res: INTEGER;
BEGIN
  REPEAT
    res := HostWindow.ProcessOneMessage();
    IF res = 0 THEN  (* Empty queue *)
      HostWindow.WaitMsgOrTime(10000)
    END
  UNTIL res > 1  (* => WM_QUIT *)
END Loop;

(*$la-lc-*)
BEGIN
  H.wsn("Oberon system starting.");

  HostWindow.SetCharacterHandler(Display.Window, DoCharacter);
  HostWindow.SetMouseHandler(Display.Window, DoMouse);
  HostWindow.SetDrawHandlers(Display.Window, PreDraw, PostDraw);

  Loop;
  H.wsn("Oberon system closing.");
END osys.
(*$la-lc-*)
