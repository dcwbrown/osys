MODULE args;

IMPORT SYSTEM, H := Winshim, WinArgs;

TYPE

VAR
  Command:  INTEGER;
  adr:      INTEGER;
  ch32:     INTEGER;

PROCEDURE Getch32*(VAR adr, ch32: INTEGER);
VAR 
  ch16:   SYSTEM.CARD16;
BEGIN
  SYSTEM.GET(adr, ch16);  INC(adr, 2);
  IF ch16 DIV 400H # 36H THEN
    ch32 := ch16 (* Not a high surrogate *)
  ELSE
    ch32 := LSL(ch16 MOD 400H, 10) + 10000H;
    SYSTEM.GET(adr, ch16);
    IF ch16 DIV 400H = 37H THEN
      INC(adr, 2);  (* Add low surrogate part *)
      INC(ch32, ch16 MOD 400H)
    END
  END
END Getch32;

PROCEDURE Writech32(ch32: INTEGER);
VAR utf8: ARRAY 6 OF CHAR; i: INTEGER;
BEGIN
  i := 0;
  H.PutUtf8(ch32, utf8, i);  utf8[i] := 0X;
  H.ws(utf8)
END Writech32;

PROCEDURE WriteCommandline;
BEGIN
  Command := H.GetCommandLineW();
  adr := Command;
  Getch32(adr, ch32);
  WHILE ch32 # 0 DO
    Writech32(ch32);
    Getch32(adr, ch32)
  END;
  H.wl;
END WriteCommandline;

PROCEDURE WriteArgs;
VAR n: INTEGER; arg: ARRAY 100 OF CHAR;
BEGIN
  H.ws("Argcount "); H.wh(WinArgs.Argcount); H.wsl("H.");
  FOR n := 0 TO WinArgs.Argcount-1 DO
    WinArgs.GetArg(n, arg);  H.ws("  '"); H.ws(arg); H.wsl("'.");
  END;
END WriteArgs;

BEGIN
  (*
  H.wsl("Args.");
  WriteCommandline;
  *)
  WriteArgs
END args.
