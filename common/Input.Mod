MODULE Input; (*NW 5.10.86 / 15.11.90 Ceres-2; PDR 21.4.12 / NW 15.5.2013 Ceres-4*)
(*DCWB 2024-03-19 hosting on MS Win *)

IMPORT SYSTEM, H := Host, Gui;

VAR
  kbdCode: BYTE;     (*last keyboard code read*)
  Recd:    BOOLEAN;
  Up:      BOOLEAN;
  Shift:   BOOLEAN;
  Ctrl:    BOOLEAN;
  Ext:     BOOLEAN;
  KTabAdr: INTEGER;  (*keyboard code translation table*)
  MW, MH:  INTEGER;  (*mouse limits*)
  MX, MY:  INTEGER;  (*mouse coords*)
  MK:      SET;      (*mouse keys*)

PROCEDURE Peek();
BEGIN
  IF Gui.KeyReady() (*SYSTEM.BIT(msAdr, 28)*) THEN
    Gui.GetKey(kbdCode); (*SYSTEM.GET(kbdAdr, kbdCode)*)
    IF    kbdCode = 0F0H THEN Up  := TRUE
    ELSIF kbdCode = 0E0H THEN Ext := TRUE
    ELSE
      IF (kbdCode = 12H) OR (kbdCode = 59H) THEN (*shift*) Shift := ~Up
      ELSIF kbdCode = 14H THEN (*ctrl*) Ctrl := ~Up
      ELSIF ~Up THEN Recd := TRUE (*real key going down*)
      END;
      Up := FALSE; Ext := FALSE
    END
  END;
END Peek;

PROCEDURE Available*(): INTEGER;
BEGIN Peek();
  RETURN ORD(Recd)
END Available;

PROCEDURE Read*(VAR ch: CHAR);
BEGIN
  WHILE ~Recd DO Peek() END;
  IF Shift OR Ctrl THEN INC(kbdCode, 80H) END; (*ctrl implies shift*)
(* ch := kbdTab[kbdCode]; *)
  SYSTEM.GET(KTabAdr + kbdCode, ch);
  IF Ctrl THEN ch := CHR(ORD(ch) MOD 20H) END;
  Recd := FALSE
END Read;

PROCEDURE Mouse*(VAR keys: SET; VAR x, y: INTEGER);
VAR w: INTEGER;
BEGIN
  w := Gui.Mouse(); (*SYSTEM.GET(msAdr, w)*)
  keys := SYSTEM.VAL(SET, w DIV 1000000H MOD 8);
  x := w MOD 1000H;
  y := w DIV 1000H MOD 1000H;
  IF x >= MW THEN x := MW-1 END;
  IF y >= MH THEN y := MH-1 END
END Mouse;

PROCEDURE MouseInWindow*(): BOOLEAN;
VAR w: INTEGER;
BEGIN w := Gui.Mouse();
  RETURN (w DIV 8000000H) MOD 2 = 1
END MouseInWindow;

PROCEDURE SetMouseLimits*(w, h: INTEGER);
BEGIN MW := w; MH := h
END SetMouseLimits;

PROCEDURE Init*;
BEGIN Up := FALSE; Shift := FALSE; Ctrl := FALSE; Recd := FALSE;
  KTabAdr := SYSTEM.ADR($
    00 00 00 00 00 1A 00 00  00 00 00 00 00 09 60 00
    00 00 00 00 00 71 31 00  00 00 7A 73 61 77 32 00
    00 63 78 64 65 34 33 00  00 20 76 66 74 72 35 00
    00 6E 62 68 67 79 36 00  00 00 6D 6A 75 37 38 00
    00 2C 6B 69 6F 30 39 00  00 2E 2F 6C 3B 70 2D 00
    00 00 27 00 5B 3D 00 00  00 00 0D 5D 00 5C 00 00
    00 00 00 00 00 00 08 00  00 00 00 00 00 00 00 00
    00 7F 00 00 00 00 1B 00  00 00 00 00 00 00 00 00
    00 00 00 00 00 00 00 00  00 00 00 00 00 09 7E 00
    00 00 00 00 00 51 21 00  00 00 5A 53 41 57 40 00
    00 43 58 44 45 24 23 00  00 20 56 46 54 52 25 00
    00 4E 42 48 47 59 5E 00  00 00 4D 4A 55 26 2A 00
    00 3C 4B 49 4F 29 28 00  00 3E 3F 4C 3A 50 5F 00
    00 00 22 00 7B 2B 00 00  00 00 0D 7D 00 7C 00 00
    00 00 00 00 00 00 08 00  00 00 00 00 00 00 00 00
    00 7F 00 00 00 00 1B 00  00 00 00 00 00 00 00 00$);

  (* UK keyboard changes *)
  SYSTEM.PUT(KTabAdr + 09EH, 022X);  (* " was @ *)
  SYSTEM.PUT(KTabAdr + 061H, 05CX);  (* \ was unused *)
  SYSTEM.PUT(KTabAdr + 0E1H, 07CX);  (* | was unused *)
  SYSTEM.PUT(KTabAdr + 00EH, 023X);  (* # was ` *)
  SYSTEM.PUT(KTabAdr + 0D2H, 040X);  (* @ was " *)
  SYSTEM.PUT(KTabAdr + 05DH, 060X);  (* ` was \ *)
  SYSTEM.PUT(KTabAdr + 0DDH, 07EX);  (* ~ was | *)
  (* Note: does not generate UK pound sterling sign (U+00A3) or *)
  (* not sign (U+00AC) as these are outside 7 bit ASCII.        *)
END Init;

BEGIN Init
END Input.
