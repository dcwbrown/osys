MODULE Display;  (*NW 5.11.2013 / 3.7.2016*)
  IMPORT SYSTEM, Gui;

  CONST black* = 0; white* = 1;  (*black = background*)
    replace* = 0; paint* = 1; invert* = 2;  (*modes*)
    Stride = Gui.Stride;

  TYPE Frame* = POINTER TO FrameDesc;
    FrameMsg* = RECORD END ;
    Handler* = PROCEDURE (F: Frame; VAR M: FrameMsg);
    FrameDesc* = RECORD next*, dsc*: Frame;
        X*, Y*, W*, H*: INTEGER;
        handle*: Handler
      END ;

  VAR Base*, Width*, Height*: INTEGER;
    arrow*, star*, hook*, updown*, block*, cross*, grey*: INTEGER;
    (*a pattern is an array of bytes; the first is its width (< 32), the second its height, the rest the raster*)

  PROCEDURE Handle*(F: Frame; VAR M: FrameMsg);
  BEGIN
    IF (F # NIL) & (F.handle # NIL) THEN F.handle(F, M) END
  END Handle;


  (* Translation for 64 bit sets and Windows bit order in each byte *)

  PROCEDURE Get(a: INTEGER; VAR s: SET);
  VAR dw: SYSTEM.CARD32;  ls: SET;
  BEGIN
    SYSTEM.GET(a, dw);  ls := SYSTEM.VAL(SET, ORD(dw));
    ls := LSL(ls * SYSTEM.VAL(SET, 55555555H), 1) + ROR(ls * SYSTEM.VAL(SET, 0AAAAAAAAH), 1);
    ls := LSL(ls * SYSTEM.VAL(SET, 33333333H), 2) + ROR(ls * SYSTEM.VAL(SET, 0CCCCCCCCH), 2);
    s  := LSL(ls * SYSTEM.VAL(SET, 0F0F0F0FH), 4) + ROR(ls * SYSTEM.VAL(SET, 0F0F0F0F0H), 4);
  END Get;

  PROCEDURE Put(a: INTEGER; s: SET);
  BEGIN
    s := LSL(s * SYSTEM.VAL(SET, 55555555H), 1) + ROR(s * SYSTEM.VAL(SET, 0AAAAAAAAH), 1);
    s := LSL(s * SYSTEM.VAL(SET, 33333333H), 2) + ROR(s * SYSTEM.VAL(SET, 0CCCCCCCCH), 2);
    s := LSL(s * SYSTEM.VAL(SET, 0F0F0F0FH), 4) + ROR(s * SYSTEM.VAL(SET, 0F0F0F0F0H), 4);
    SYSTEM.PUT(a, SYSTEM.VAL(SYSTEM.CARD32, s));
  END Put;


  (* raster ops *)

  PROCEDURE Dot*(col, x, y, mode: INTEGER);
    VAR a: INTEGER; u, s: SET;
  BEGIN a := Base + (x DIV 32)*4 + y*Stride;
    s := {x MOD 32}; Get(a, u);
    IF mode = paint THEN Put(a, u + s)
    ELSIF mode = invert THEN Put(a, u / s)
    ELSE (*mode = replace*)
      IF col # black THEN Put(a, u + s) ELSE Put(a, u - s) END
    END;
    Gui.Dirty(x, y, 1, 1)
  END Dot;

  PROCEDURE ReplConst*(col, x, y, w, h, mode: INTEGER);
    VAR al, ar, a0, a1: INTEGER; left, right, mid, pix, pixl, pixr: SET;
  BEGIN al := Base + y*Stride;
    ar := ((x+w-1) DIV 32)*4 + al; al := (x DIV 32)*4 + al;
    IF ar = al THEN
      mid := {(x MOD 32) .. ((x+w-1) MOD 32)};
      FOR a1 := al TO al + (h-1)*Stride BY Stride DO
        Get(a1, pix);
        IF mode = invert THEN Put(a1, pix / mid)
        ELSIF (mode = replace) & (col = black) THEN (*erase*) Put(a1, pix - mid)
        ELSE (* (mode = paint) OR (mode = replace) & (col # black) *) Put(a1, pix + mid)
        END
      END
    ELSIF ar > al THEN
      left := {(x MOD 32) .. 31}; right := {0 .. ((x+w-1) MOD 32)};
      FOR a0 := al TO al + (h-1)*Stride BY Stride DO
        Get(a0, pixl); Get(ar, pixr);
        IF mode = invert THEN
          Put(a0, pixl / left);
          FOR a1 := a0+4 TO ar-4 BY 4 DO Get(a1, pix); Put(a1, -pix) END ;
          Put(ar, pixr / right)
        ELSIF (mode = replace) & (col = black) THEN (*erase*)
          Put(a0, pixl - left);
          FOR a1 := a0+4 TO ar-4 BY 4 DO Put(a1, {}) END ;
          Put(ar, pixr - right)
        ELSE (* (mode = paint) OR (mode = replace) & (col # black) *)
          Put(a0, pixl + left);
          FOR a1 := a0+4 TO ar-4 BY 4 DO Put(a1, {0 .. 31}) END ;
          Put(ar, pixr + right)
        END ;
        INC(ar, Stride)
      END
    END;
    Gui.Dirty(x, y, w, h)
  END ReplConst;

  PROCEDURE CopyPattern*(col, patadr, x, y, mode: INTEGER);  (*only for modes = paint, invert*)
    VAR a, a0, pwd: INTEGER;
      w, h, pbt: BYTE; pix, mask: SET;
  BEGIN
    SYSTEM.GET(patadr, w); SYSTEM.GET(patadr+1, h); INC(patadr, 2);
    Gui.Dirty(x, y, w, h);
    a := Base + (x DIV 32)*4 + y*Stride;  x := x MOD 32;  mask := SYSTEM.VAL(SET, ASR(7FFFFFFFH, 31-x));
    FOR a0 := a TO a + (h-1)*Stride BY Stride DO
      (*build pattern line; w <= 32*)
      SYSTEM.GET(patadr, pbt); INC(patadr); pwd := pbt;
      IF w > 8 THEN SYSTEM.GET(patadr, pbt); INC(patadr); pwd := pbt*100H + pwd;
        IF w > 16 THEN SYSTEM.GET(patadr, pbt); INC(patadr); pwd := pbt*10000H + pwd;
          IF w > 24 THEN SYSTEM.GET(patadr, pbt); INC(patadr); pwd := pbt*1000000H + pwd END
        END
      END ;
      Get(a0, pix);
      IF mode = invert THEN Put(a0, SYSTEM.VAL(SET, LSL(pwd, x)) / pix)
      ELSE Put(a0, SYSTEM.VAL(SET, LSL(pwd, x)) + pix)
      END ;
      IF x + w > 32 THEN (*spill over*)
        Get(a0+4, pix);
        IF mode = invert THEN Put(a0+4, SYSTEM.VAL(SET, ASR(pwd, 32-x)) * mask / pix)
        ELSE Put(a0+4, SYSTEM.VAL(SET, ASR(pwd, 32-x)) * mask + pix)
        END
      END
    END
  END CopyPattern;

  PROCEDURE CopyBlock*(sx, sy, w, h, dx, dy, mode: INTEGER); (*only for mode = replace*)
    VAR sa, da, sa0, sa1, d, len: INTEGER;
      u0, u1, u2, u3, v0, v1, v2, v3, n: INTEGER;
      end, step: INTEGER;
      src, dst, spill: SET;
      m0, m1, m2, m3: SET;
  BEGIN
    u0 := sx DIV 32; u1 := sx MOD 32; u2 := (sx+w) DIV 32; u3 := (sx+w) MOD 32;
    v0 := dx DIV 32; v1 := dx MOD 32; v2 := (dx+w) DIV 32; v3 := (dx+w) MOD 32;
    sa := Base + u0*4 + sy*Stride; da := Base + v0*4 + dy*Stride;
    d := da - sa; n := u1 - v1;   (*displacement in words and bits*)
    len := (u2 - u0) * 4;
    m0 := {v1 .. 31}; m2 := {v3 .. 31}; m3 := m0 / m2;
    IF d >= 0 THEN (*copy up, scan down*) sa0 := sa + (h-1)*Stride; end := sa-Stride; step := -Stride
    ELSE (*copy down, scan up*) sa0 := sa; end := sa + h*Stride; step := Stride
    END ;
    WHILE sa0 # end DO
      IF n >= 0 THEN (*shift right*) m1 := {n .. 31};
        IF v1 + w >= 32 THEN
          Get(sa0+len, src); src := ROR(src, n);
          Get(sa0+len+d, dst);
          Put(sa0+len+d, (dst * m2) + (src - m2));
          spill := src - m1;
          FOR sa1 := sa0 + len-4 TO sa0+4  BY -4 DO
            Get(sa1, src); src := ROR(src, n);
            Put(sa1+d, spill + (src * m1));
            spill := src - m1
          END ;
          Get(sa0, src); src := ROR(src, n);
          Get(sa0+d, dst);
          Put(sa0+d, (src * m0) + (dst - m0))
        ELSE Get(sa0, src); src := ROR(src, n);
          Get(sa0+d, dst);
          Put(sa0+d, (src * m3) + (dst - m3))
        END
      ELSE (*shift left*) m1 := {-n .. 31};
        Get(sa0, src); src := ROR(src, n);
        Get(sa0+d, dst);
        IF v1 + w < 32 THEN
          Put(sa0+d, (dst - m3) + (src * m3))
        ELSE Put(sa0+d, (dst - m0) + (src * m0));
          spill := src - m1;
          FOR sa1 := sa0+4 TO sa0 + len-4 BY 4 DO
            Get(sa1, src); src := ROR(src, n);
            Put(sa1+d, spill + (src * m1));
            spill := src - m1
          END ;
          Get(sa0+len, src); src := ROR(src, n);
          Get(sa0+len+d, dst);
          Put(sa0+len+d, (src - m2) + (dst * m2))
        END
      END ;
      INC(sa0, step)
    END;
    Gui.Dirty(dx, dy, w, h)
  END CopyBlock;

  PROCEDURE ReplPattern*(col, patadr, x, y, w, h, mode: INTEGER);
  (* pattern width = 32, fixed; pattern starts at patadr+4, for mode = invert only *)
    VAR al, ar, a0, a1: INTEGER;
      pta0, pta1: INTEGER;  (*pattern addresses*)
      ph: BYTE;
      left, right, mid, pix, pixl, pixr, ptw: SET;
  BEGIN al := Base + y*Stride; SYSTEM.GET(patadr+1, ph);
    pta0 := patadr+4; pta1 := ph*4 + pta0;
    ar := ((x+w-1) DIV 32)*4 + al; al := (x DIV 32)*4 + al;
    IF ar = al THEN
      mid := {(x MOD 32) .. ((x+w-1) MOD 32)};
      FOR a1 := al TO al + (h-1)*Stride BY Stride DO
        Get(a1, pix);
        SYSTEM.GET(pta0, ptw);
        Put(a1, (pix - mid) + (pix/ptw * mid));
        INC(pta0, 4);
        IF pta0 = pta1 THEN pta0 := patadr+4 END
      END
    ELSIF ar > al THEN
      left := {(x MOD 32) .. 31}; right := {0 .. ((x+w-1) MOD 32)};
      FOR a0 := al TO al + (h-1)*Stride BY Stride DO
        Get(a0, pixl); Get(pta0, ptw); Put(a0, (pixl - left) + (pixl/ptw * left));
        FOR a1 := a0+4 TO ar-4 BY 4 DO Get(a1, pix); Put(a1, pix/ptw) END ;
        Get(ar, pixr); Put(ar, (pixr - right) + (pixr/ptw * right));
        INC(pta0, 4); INC(ar, 128);
        IF pta0 = pta1 THEN pta0 := patadr+4 END
      END
    END;
    Gui.Dirty(x, y, w, h)
  END ReplPattern;

BEGIN
  Base   := Gui.Window.bmp.address;
  Width  := Gui.Width;
  Height := Gui.Height;
  arrow := SYSTEM.ADR($0F0F 0060 0070 0038 001C 000E 0007 8003 C101 E300 7700 3F00 1F00 3F00 7F00 FF00$);
  star := SYSTEM.ADR($0F0F 8000 8220 8410 8808 9004 A002 C001 7F7F C001 A002 9004 8808 8410 8220 8000$);
  hook := SYSTEM.ADR($0C0C 070F 8707 C703 E701 F700 7F00 3F00 1F00 0F00 0700 0300 01$);
  updown := SYSTEM.ADR($080E 183C 7EFF 1818 1818 1818 FF7E3C18$);
  block := SYSTEM.ADR($0808 FFFF C3C3 C3C3 FFFF$);
  cross := SYSTEM.ADR($0F0F 0140 0220 0410 0808 1004 2002 4001 0000 4001 2002 1004 0808 0410 0220 0140$);
  grey := SYSTEM.ADR($2002 0000 5555 5555 AAAA AAAA$)
END Display.
