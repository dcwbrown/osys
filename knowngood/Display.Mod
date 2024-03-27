MODULE Display;  (*NW 5.11.2013 / 3.7.2016*)

(*  DCWB 2024/03/22                                                   *)
(*                                                                    *)
(*  Adapt from 32 bit to 64 bit INTEGERs and SETs                     *)
(*  Adapt to bitmaps with leftmost bit at the top                     *)
(*                                                                    *)
(*  Wirth's PO2013 hardware places the leftmost bit at the bottom,    *)
(*  meaning that a PO2013 SET corresponds to 32 pixel columns, with   *)
(*  {0} corresponding to a set pixel at the the leftmost column and   *)
(*  {31} corresponding to a set pixel at the rightmost column.        *)
(*                                                                    *)
(*  Because Windows is little endian (like PO2013), the poisition of  *)
(*  bytes in an integer correspond to the positions of groups of 8    *)
(*  pixels in the same way as in PO2013.                              *)
(*                                                                    *)
(*  However within each byte, a Windows device independent bitmap     *)
(*  (and presumably the underlying hardware) places the bits in       *)
(*  a byte in reverse order to pixels                                 *)
(*                                                                    *)
(*  Thus on Windows, {0} corresponds to a pixel in the 8th column     *)
(*  and {31} corresponds to a pixel in the 23rd column.               *)
(*                                                                    *)
(*  So -- how to adapt the PO2013 code for Windows 1bpp bitmaps?      *)
(*                                                                    *)
(*  Consider: adapt from 32 bit to 64 bit sets, retaining byte        *)
(*  addressing and reversing bits within a byte. This turns out       *)
(*  to be complex.                                                    *)
(*                                                                    *)
(*  For example, creating a SET of pixels from lowpixel to highpixel  *)
(*  goes from the PO2013 version:                                     *)
(*                                                                    *)
(*    pixels := {lowpixel .. highpixel}                               *)
(*                                                                    *)
(*  to this overly complex code:                                      *)
(*                                                                    *)
(*    pixels := {};                                                   *)
(*    IF highpixel >= lowpixel THEN                                   *)
(*      leftbyte := lowpixel DIV 8;  rightbyte := highpixel DIV 8;    *)
(*      lowpixel := lowpixel MOD 8;  highpixel := highpixel MOD 8;    *)
(*      IF leftbyte = rightbyte THEN                                  *)
(*        pixels := {   (leftbyte * 8 + 7 - lowpixel)                 *)
(*                   .. (leftbyte * 8 + 7 - highpixel)}               *)
(*      ELSE                                                          *)
(*        pixels := {   (leftbyte * 8 + 7 - lowpixel)                 *)
(*                   .. (leftbyte * 8 + 7)};                          *)
(*        pixels := {   (rightbyte * 8)                               *)
(*                   .. (rightbyte * 8 + 7 - highpixel)} + pixels;    *)
(*        IF rightbyte > leftbyte + 1 THEN                            *)
(*          pixels := {   (leftbyte * 8 + 8)                          *)
(*                     .. (rightbyte * 8 - 1)} + pixels               *)
(*        END                                                         *)
(*      END                                                           *)
(*    END                                                             *)
(*                                                                    *)
(*  Conclusion:                                                       *)
(*                                                                    *)
(*  Work in bytes, using SETs to handle bits within a byte rather     *)
(*  than a whole word.                                                *)

IMPORT SYSTEM, H := WinHost, WinGui;

CONST
  black*   = 0;  (*black = background*)
  white*   = 1;
  replace* = 0;  (*modes*)
  paint*   = 1;
  invert*  = 2;
  clear*   = 3;

(* Note: mode replace color white => mode paint, mode replace color black => clear. *)

TYPE
  Frame*     = POINTER TO FrameDesc;
  FrameMsg*  = RECORD END;
  Handler*   = PROCEDURE (F: Frame; VAR M: FrameMsg);
  FrameDesc* = RECORD
    next*, dsc*:    Frame;
    X*, Y*, W*, H*: INTEGER;
    handle*:        Handler
  END;

VAR
  Base*:   INTEGER;
  Width*:  INTEGER;
  Height*: INTEGER;
  Stride:  INTEGER;  (* Width in whole bytes *)

  Reverse4: ARRAY 16 OF BYTE;

  (* a pattern is an array of bytes; the first is its width (< 32), *)
  (* the second its height, the rest the raster                     *)
  arrow*,
  star*,
  hook*,
  updown*,
  block*,
  cross*,
  grey*: INTEGER;

PROCEDURE Handle*(F: Frame; VAR M: FrameMsg);
BEGIN
  IF (F # NIL) & (F.handle # NIL) THEN F.handle(F, M) END
END Handle;

(* raster ops *)

(* 24 SETs per row, each 8 bytes / 64 bits, making 192 bytes / 1536 bits per row *)

PROCEDURE UpdateByte(addr, mode: INTEGER; p: SET);
VAR b: BYTE;
BEGIN
  IF p * {0..7} # {} THEN
    SYSTEM.GET(addr, b);
    IF    mode = paint  THEN b := SYSTEM.VAL(INTEGER, SYSTEM.VAL(SET, b+0) + p)
    ELSIF mode = invert THEN b := SYSTEM.VAL(INTEGER, SYSTEM.VAL(SET, b+0) / p)
    ELSIF mode = clear  THEN b := SYSTEM.VAL(INTEGER, SYSTEM.VAL(SET, b+0) - p)
    END;
    SYSTEM.PUT(addr, b)
  END
END UpdateByte;

PROCEDURE Dot*(col, x, y, mode: INTEGER);
VAR a: INTEGER;  s: SET;
BEGIN
  a := Base + x DIV 8 + y * Stride;
  s := {7 - x MOD 8};
  IF mode = replace THEN
    IF col = black THEN mode := clear ELSE mode := paint END
  END;
  UpdateByte(a, mode, s)
END Dot;


PROCEDURE ReplConst*(col, x, y, w, h, mode: INTEGER);
VAR al, ar, a0, a1: INTEGER;  left, right, mid: SET;  b: BYTE;
BEGIN
  (*
  H.ws("ReplConst(");
  IF    mode = invert THEN H.ws("invert, ")
  ELSIF mode = paint  THEN H.ws("paint, ")
  ELSIF mode = clear  THEN H.ws("clear, ")
  ELSIF col = black   THEN H.ws("black, ")
                      ELSE H.ws("white, ")
  END;
  H.wi(x); H.wc(","); H.wi(y); H.wc(","); H.wi(w); H.wc(","); H.wi(h); H.ws(")");
  IF w = 1 THEN
    H.ws("  Vertical   line at x "); H.wi(x); H.ws(" from y "); H.wi(y); H.ws(" to "); H.wi(y+h-1); H.wsn(".")
  ELSIF h = 1 THEN
    H.ws("  Horizontal line at y "); H.wi(y); H.ws(" from x "); H.wi(x); H.ws(" to "); H.wi(x+w-1); H.wsn(".")
  ELSE
    H.wn
  END;
  *)
  IF mode = replace THEN
    IF col = black THEN mode := clear ELSE mode := paint END
  END;
  al := Base + y * Stride;  (* Byte address at start of first (bottommost) affected bitmap row *)
  ar := (x+w-1) DIV 8 + al;
  al := x DIV 8 + al;
  IF ar = al THEN
    mid := {(7 - (x+w-1) MOD 8) .. (7 - x MOD 8)};
    a1 := al;
    WHILE a1 < al + h * Stride DO
      UpdateByte(a1, mode, mid);  INC(a1, Stride)
    END
  ELSIF ar > al THEN
    left  := {0 .. 7 - x MOD 8};
    right := {7 - (x+w-1) MOD 8 .. 7};
    a0 := al;
    WHILE a0 < al + h * Stride DO  (* For each row *)
      UpdateByte(a0, mode, left);
      FOR a1 := a0+1 TO ar-1 DO UpdateByte(a1, mode, {0..7}) END;
      UpdateByte(ar, mode, right);
      INC(ar, Stride);
      INC(a0, Stride)
    END
  END
END ReplConst;

PROCEDURE ReverseByte(b: BYTE): BYTE;
BEGIN RETURN Reverse4[b MOD 10H] * 10H + Reverse4[b DIV 10H] END ReverseByte;

PROCEDURE CopyPattern*(col, patadr, x, y, mode: INTEGER);  (*only for modes = paint, invert*)
VAR
  w, h:  BYTE;
  a, a0: INTEGER;
  a1:    INTEGER;
  pat:   INTEGER; (* One (64 bit) row of pattern *)
  pbt:   BYTE;    (* One byte of pattern         *)
BEGIN
  SYSTEM.GET(patadr, w);    (* width (<32) *)
  SYSTEM.GET(patadr+1, h);  (* height      *)
  INC(patadr, 2);
  a := Base + x DIV 8 + y * Stride;
  IF mode = replace THEN
    IF col = black THEN mode := clear ELSE mode := paint END
  END;
  a0 := a;
  WHILE a0 < a + h * Stride DO
    (* Load pattern (up to 4 bytes) to top of pat *)
    SYSTEM.GET(patadr, pbt); INC(patadr); pat := LSL(ReverseByte(pbt), 56);
    IF w > 8 THEN      SYSTEM.GET(patadr, pbt); INC(patadr); INC(pat, LSL(ReverseByte(pbt), 48));
      IF w > 16 THEN   SYSTEM.GET(patadr, pbt); INC(patadr); INC(pat, LSL(ReverseByte(pbt), 40));
        IF w > 24 THEN SYSTEM.GET(patadr, pbt); INC(patadr); INC(pat, LSL(ReverseByte(pbt), 32)) END
      END
    END;
    IF x MOD 8 # 0 THEN pat := ROR(pat, x MOD 8) END;  (* Move right to align with first col *)
    a1 := a0;
    WHILE pat # 0 DO
      UpdateByte(a1, mode, SYSTEM.VAL(SET, ROR(pat, 56)));
      INC(a1);
      pat := LSL(pat, 8);
    END;
    INC(a0, Stride)
  END;
  WinGui.Invalidate
END CopyPattern;

PROCEDURE CopyBlock*(sx, sy, w, h, dx, dy, mode: INTEGER); (*only for mode = replace*)
VAR sa, da, sa0, sa1, d, len: INTEGER;
    u0, u1, u2, u3, v0, v1, v2, v3, n: INTEGER;
    end, step: INTEGER;
    src, dst, spill: SET;
    m0, m1, m2, m3: SET;
BEGIN
  ASSERT(FALSE);
  (*
  u0 := sx DIV 64;      (* First affected whole word of source & dest *)
  v0 := dx DIV 64;
  u1 := sx MOD 64;      (* Bit offset of start in source & dest *)
  v1 := dx MOD 64;
  u2 := (sx+w) DIV 64;  (* offset of limit word in source & dest *)
  v2 := (dx+w) DIV 64;
  u3 := (sx+w) MOD 64;  (* Bit offset of limit (last bit + 1) in source & dest *)
  v3 := (dx+w) MOD 64;
  sa := Base + u0 * 8 + sy * Stride;  (* Address of first affected words in source & dest *)
  da := Base + v0 * 8 + dy * Stride;
  d := da - sa;         (* Copy displacement of whole words as byte offset *)
  n := u1 - v1;         (* displacement in bits *)
  len := (u2 - u0) * 8; (* Byte length of whole words containing source *)

????????

  m0 := {v1 .. 31};     (* Mask of written bits in first word of dest *)
  m2 := {v3 .. 31};     (* Mask of final unaffected bits in dest *)
  m3 := m0 / m2;
  IF d >= 0 THEN sa0 := sa + (h-1)*Stride; end := sa-Stride;     step := -Stride (*copy up, scan down*)
            ELSE sa0 := sa;             end := sa + h*Stride; step := Stride  (*copy down, scan up*)
  END;
  WHILE sa0 # end DO
    IF n >= 0 THEN (*shift right*) m1 := {n .. 31};
      IF v1 + w >= 32 THEN
        SYSTEM.GET(sa0+len, src); src := ROR(src, n);
        SYSTEM.GET(sa0+len+d, dst);
        SYSTEM.PUT(sa0+len+d, (dst * m2) + (src - m2));
        spill := src - m1;
        FOR sa1 := sa0 + len-4 TO sa0+4  BY -4 DO
          SYSTEM.GET(sa1, src); src := ROR(src, n);
          SYSTEM.PUT(sa1+d, spill + (src * m1));
          spill := src - m1
        END;
        SYSTEM.GET(sa0, src); src := ROR(src, n);
        SYSTEM.GET(sa0+d, dst);
        SYSTEM.PUT(sa0+d, (src * m0) + (dst - m0))
      ELSE SYSTEM.GET(sa0, src); src := ROR(src, n);
        SYSTEM.GET(sa0+d, dst);
        SYSTEM.PUT(sa0+d, (src * m3) + (dst - m3))
      END
    ELSE (*shift left*) m1 := {-n .. 31};
      SYSTEM.GET(sa0, src); src := ROR(src, n);
      SYSTEM.GET(sa0+d, dst);
      IF v1 + w < 32 THEN
        SYSTEM.PUT(sa0+d, (dst - m3) + (src * m3))
      ELSE SYSTEM.PUT(sa0+d, (dst - m0) + (src * m0));
        spill := src - m1;
        FOR sa1 := sa0+4 TO sa0 + len-4 BY 4 DO
          SYSTEM.GET(sa1, src); src := ROR(src, n);
          SYSTEM.PUT(sa1+d, spill + (src * m1));
          spill := src - m1
        END;
        SYSTEM.GET(sa0+len, src); src := ROR(src, n);
        SYSTEM.GET(sa0+len+d, dst);
        SYSTEM.PUT(sa0+len+d, (src - m2) + (dst * m2))
      END
    END;
    INC(sa0, step)
  END
  *)
END CopyBlock;

PROCEDURE ReplPattern*(col, patadr, x, y, w, h, mode: INTEGER);
(* pattern width = 32, fixed; pattern starts at patadr+4, for mode = invert only *)
VAR al, ar, a0, a1: INTEGER;
    pta0, pta1: INTEGER;  (*pattern addresses*)
    ph: BYTE;
    left, right, mid, pix, pixl, pixr, ptw: SET;
BEGIN
  ASSERT(FALSE);
  al := Base + y*Stride; SYSTEM.GET(patadr+1, ph);
  pta0 := patadr+4; pta1 := ph*4 + pta0;
  ar := ((x+w-1) DIV 64) * 8 + al; al := (x DIV 64) * 8 + al;
  IF ar = al THEN
    mid := {(x MOD 64) .. ((x+w-1) MOD 64)};
    a1 := al;
    WHILE a1 < al + h * Stride DO
      SYSTEM.GET(a1, pix); SYSTEM.GET(pta0, ptw); SYSTEM.PUT(a1, (pix - mid) + (pix/ptw * mid)); INC(pta0, 4);
      IF pta0 = pta1 THEN pta0 := patadr+4 END;
      INC(a1, Stride)
    END
  ELSIF ar > al THEN
    left := {(x MOD 64) .. 31}; right := {0 .. ((x+w-1) MOD 64)};
    a0 := al;
    WHILE a0 < al + h * Stride DO
      SYSTEM.GET(a0, pixl); SYSTEM.GET(pta0, ptw); SYSTEM.PUT(a0, (pixl - left) + (pixl/ptw * left));
      FOR a1 := a0+4 TO ar-4 BY 4 DO SYSTEM.GET(a1, pix); SYSTEM.PUT(a1, pix/ptw) END;
      SYSTEM.GET(ar, pixr); SYSTEM.PUT(ar, (pixr - right) + (pixr/ptw * right));
      INC(pta0, 4); INC(ar, Stride);
      IF pta0 = pta1 THEN pta0 := patadr+4 END;
      INC(a0, Stride)
    END
  END
END ReplPattern;


BEGIN
  Reverse4[0]  := 0;  Reverse4[1]  := 8;   Reverse4[2]  := 4;  Reverse4[3]  := 12;
  Reverse4[4]  := 2;  Reverse4[5]  := 10;  Reverse4[6]  := 6;  Reverse4[7]  := 14;
  Reverse4[8]  := 1;  Reverse4[9]  := 9;   Reverse4[10] := 5;  Reverse4[11] := 13;
  Reverse4[12] := 3;  Reverse4[13] := 11;  Reverse4[14] := 7;  Reverse4[15] := 15;

  Base   := WinGui.Window.bmp.address;
  Width  := WinGui.Window.bmp.width;
  Height := WinGui.Window.bmp.height;
  Stride := (Width + 7) DIV 8;
  arrow  := SYSTEM.ADR($0F0F 0060 0070 0038 001C 000E 0007 8003 C101 E300 7700 3F00 1F00 3F00 7F00 FF00$);
  star   := SYSTEM.ADR($0F0F 8000 8220 8410 8808 9004 A002 C001 7F7F C001 A002 9004 8808 8410 8220 8000$);
  hook   := SYSTEM.ADR($0C0C 070F 8707 C703 E701 F700 7F00 3F00 1F00 0F00 0700 0300 01$);
  updown := SYSTEM.ADR($080E 183C 7EFF 1818 1818 1818 FF7E3C18$);
  block  := SYSTEM.ADR($0808 FFFF C3C3 C3C3 FFFF$);
  cross  := SYSTEM.ADR($0F0F 0140 0220 0410 0808 1004 2002 4001 0000 4001 2002 1004 0808 0410 0220 0140$);
  grey   := SYSTEM.ADR($2002 0000 5555 5555 AAAA AAAA$)
END Display.
