MODULE WinArgs;  (* DCWB 14.02.2024 *)

IMPORT SYSTEM, H := WinHost;

VAR
  Commandline: INTEGER;  (* Address of UTF16 string obtained from Windows *)
  Argcount*:   INTEGER;
  Args:        ARRAY 256 OF SYSTEM.CARD32;  (* Byte offsets into Commandline *)


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


PROCEDURE ParseArgs;  (* Split UTF16 arg string at argument boundaries    *)

(*  Arguments are tokenised following bash conventions.                   *)
(*                                                                        *)
(*  o Arguments are delimited by whitespace (ch <= 32)                    *)
(*  o White space may be included by surrounding it with "..". The Quotes *)
(*    may surround the whole argument, or any part of it, so long as the  *)
(*    whitespace is between the quotes.                                   *)
(*  o The third special character is backslash - '\'. The character       *)
(*    following a '\' is always treated as if a plain character, even if  *)
(*    it is a '\', '"' or ' '.                                            *)
(*    .  Outside a quoted sequence, the leading '\' is always dropped.    *)
(*    .  Inside a quoted sequence, the leading '\' is only dropped when   *)
(*       followed by '\' or '"'.                                          *)

VAR
  i, j:      INTEGER;
  quoted:    BOOLEAN;
  ch:        SYSTEM.CARD16;
  backslash: SYSTEM.CARD16;
  zero:      SYSTEM.CARD16;
BEGIN
  backslash   := 5CH;  (* 2 byte - utf16 - backslash *)
  zero        := 0;    (* 2 byte - utf16 - zero      *)
  Commandline := H.GetCommandLineW();
  i           := Commandline;
  Argcount    := 0;
  SYSTEM.GET(i, ch);
  WHILE ch # 0 DO

    (* Skip whitespace *)
    WHILE (ch > 0) & (ch <= ORD(" ")) DO INC(i, 2); SYSTEM.GET(i, ch) END;

    (* Parse argument *)
    IF ch > 0 THEN
      Args[Argcount] := i - Commandline;
      quoted         := FALSE;
      j              := i;

      WHILE (ch > ORD(" ")) OR (quoted & (ch = ORD(" "))) DO

        IF ch = 5CH THEN  (* \ *)
          INC(i, 2);  SYSTEM.GET(i, ch);
          IF quoted & (ch # 5CH) & (ch # 22H) THEN
            SYSTEM.PUT(j, backslash);  INC(j, 2)
          END;
          SYSTEM.PUT(j, ch);  INC(j, 2)

        ELSIF ch = 22H THEN (* " *)
          quoted := ~quoted

        ELSE
          SYSTEM.PUT(j, ch);  INC(j, 2)
        END;

        INC(i, 2); SYSTEM.GET(i, ch)

      END;

      SYSTEM.PUT(j, zero); INC(j, 2);
      ASSERT( j <= i + 2);
      INC(Argcount)
    END
  END
END ParseArgs;


PROCEDURE GetArg*(n: INTEGER; VAR arg: ARRAY OF CHAR);
VAR adr, ch32, i: INTEGER;
BEGIN
  IF (n < 0) OR (n >= Argcount) THEN
    arg[0] := 0X
  ELSE
    adr := Commandline + Args[n];
    i   := 0;
    Getch32(adr, ch32);
    WHILE ch32 # 0 DO
      H.PutUtf8(ch32, arg, i);
      Getch32(adr, ch32)
    END;
    arg[i] := 0X
  END
END GetArg;


BEGIN ParseArgs
END WinArgs.
