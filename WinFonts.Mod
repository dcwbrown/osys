MODULE WinFonts;

IMPORT SYSTEM, K := Kernel, H := WinHost;

TYPE
  INT16 = SYSTEM.INT16;  UINT16 = SYSTEM.CARD16;
  INT32 = SYSTEM.INT32;  UINT32 = SYSTEM.CARD32;

  Glyph* = POINTER TO GlyphDesc;

  GlyphDesc* = RECORD
    map*:       INTEGER;  (* Alphamap index of start of glyph alpha map                         *)
    mapWidth*:  INTEGER;  (* Alpha width in 1/4 pixels                                          *)
    mapHeight*: INTEGER;  (* Alpha heght in whole pixles                                        *)
    originX*:   INTEGER;  (* Offset of left alignment edge rightwards from left of alpha map    *)
    baseline*:  INTEGER   (* Offset of baseline downwards from top of alpha map                 *)
   END;

   Face  = POINTER TO FaceDesc;
   Font* = POINTER TO FontDesc;

   FontDesc = RECORD
     hfont:    INTEGER;
     face:     Face;
     name*:    ARRAY 256 OF CHAR;
     em:       INTEGER;
     glyphs:   ARRAY 95 OF Glyph;
     ascent*:  INTEGER;  (* In whole pixels *)
     descent*: INTEGER;  (* In whole pixels *)
     lead*:    INTEGER;  (* In whole pixels *)
     next:     Font
   END;

   FaceDesc = RECORD
     name:    ARRAY 256 OF CHAR;
     sizes:   Font;
     emsize:  INTEGER;
     widths:  ARRAY 95 OF INTEGER; (* at emsize in 56.8 *)
     ascent:  INTEGER;  (* In whole pixels at emsize *)
     descent: INTEGER;  (* In whole pixels at emsize *)
     lead:    INTEGER;  (* In whole pixels at emsize *)
     next:    Face
   END;

VAR
  DisplayDC: INTEGER;

  AlphaMap:  ARRAY 100000 OF BYTE;
  MapLen:    INTEGER;

  Faces:     Face;

  Default*:  Font;


(* ---------------------------- Font constuction ---------------------------- *)

PROCEDURE GetFace(name: ARRAY OF CHAR): Face;
TYPE
  TextMetrics = RECORD
    height:              INT32;
    ascent:              INT32;
    descent:             INT32;
    internalLeading:     INT32;
    externalLeading:     INT32;
    aveCharWidth:        INT32;
    maxCharWidth:        INT32;
    weight:              INT32;
    overhang:            INT32;
    digitizedAspectX:    INT32;
    digitizedAspectY:    INT32;
    firstChar:           UINT16;
    lastChar:            UINT16;
    defaultChar:         UINT16;
    breakChar:           UINT16;
    italic:              BYTE;
    underlined:          BYTE;
    struckOut:           BYTE;
    pitchAndFamily:      BYTE;
    charSet:             BYTE
  END;
  OutlineTextMetrics = RECORD
    size:                  UINT32;
    textmetrics:           TextMetrics;
    filler:                BYTE;
    panoseFamilyType:      BYTE;
    panoseSerifStyle:      BYTE;
    panoseWeight:          BYTE;
    panoseProportion:      BYTE;
    panoseContrast:        BYTE;
    panoseStrokeVariation: BYTE;
    panoseArmStyle:        BYTE;
    panoseLetterform:      BYTE;
    panoseMidline:         BYTE;
    panoseXHeight:         BYTE;
    selection:             UINT32;
    type:                  UINT32;
    charSlopeRise:         INT32;
    charSlopeRun:          INT32;
    italicAngle:           INT32;
    emSquare:              UINT32;
    ascent:                INT32;
    descent:               INT32;
    lineGap:               UINT32;
    capEmHeight:           UINT32;
    xHeight:               UINT32;
    fontBoxLeft:           INT32;
    fontBoxTop:            INT32;
    fontBoxRight:          INT32;
    fontBoxBottom:         INT32;
    macAscent:             INT32;
    macDescent:            INT32;
    macLineGap:            UINT32;
    minimumPPEM:           UINT32;
    subscriptSizeX:        INT32;
    subscriptSizeY:        INT32;
    subscriptOffsetX:      INT32;
    subscriptOffsetY:      INT32;
    superscriptSizeX:      INT32;
    superscriptSizeY:      INT32;
    superscriptOffsetX:    INT32;
    superscriptOffsetY:    INT32;
    strikeoutSize:         UINT32;
    strikeoutPosition:     INT32;
    underscoreSize:        INT32;
    underscorePosition:    INT32;
    pFamilyName:           INTEGER;
    pFaceName:             INTEGER;
    pStyleName:            INTEGER;
    pFullName:             INTEGER;
    buffer:                ARRAY 4096 OF UINT16  (* Contains face names *)
  END;
  ABC = RECORD a: INT32; b: UINT32; c: INT32 END;

VAR
  face:     Face;
  hFont:    INTEGER;
  hOldFont: INTEGER;
  tm:       TextMetrics;
  otm:      OutlineTextMetrics;
  bufsize:  INTEGER;
  i:        INTEGER;
  abcs:     ARRAY 95 OF ABC;
BEGIN
  face := Faces;
  WHILE (face # NIL) & (face.name # name) DO face := face.next END;
  IF face = NIL THEN
    H.ws("Create new Fonts.Face '"); H.ws(name); H.wsn("'.");
    NEW(face);
    face.name := name;

    hFont     := H.CreateFontA(-2048, 0,0,0,0,0,0,0,0,7,0,0,0, SYSTEM.ADR(name));  (* 7: OUT_TT_ONLY_PRECIS *)
    ASSERT(hFont # 0);
    hOldFont  := H.SelectObject(DisplayDC, hFont);
    bufsize   := H.GetOutlineTextMetricsW(DisplayDC, 0, 0);
    H.ws("GetOutlineTextMetricsW buffer size required: "); H.wi(bufsize); H.wsn(".");
    ASSERT(H.GetOutlineTextMetricsW(DisplayDC, SYSTEM.SIZE(OutlineTextMetrics), SYSTEM.ADR(otm)) # 0);

    IF otm.emSquare # otm.textmetrics.height - otm.textmetrics.internalLeading THEN
      (* Reselect the font at its design em size. *)
      ASSERT(H.DeleteObject(H.SelectObject(DisplayDC, hOldFont)) # 0);
      hFont     := H.CreateFontA(-otm.emSquare, 0,0,0,0,0,0,0,0,7,0,0,0, SYSTEM.ADR(name));  (* 7: OUT_TT_ONLY_PRECIS *)
      ASSERT(hFont # 0);
      hOldFont  := H.SelectObject(DisplayDC, hFont);
      bufsize   := H.GetOutlineTextMetricsW(DisplayDC, 0, 0);
      H.ws("GetOutlineTextMetricsW buffer size required: "); H.wi(bufsize); H.wsn(".");
      ASSERT(H.GetOutlineTextMetricsW(DisplayDC, SYSTEM.SIZE(OutlineTextMetrics), SYSTEM.ADR(otm)) # 0);
    END;

    face.emsize  := otm.emSquare;
    face.ascent  := otm.ascent;
    face.descent := otm.descent;
    face.lead    := otm.lineGap;
    H.ws("Create face '");        H.ws(name);  H.wsn("'.");
    H.ws("  emSquare          "); H.wi(otm.emSquare);  H.wsn(".");
    H.ws("  tmHeight          "); H.wi(otm.textmetrics.height);  H.wsn(".");
    H.ws("  tmAscent          "); H.wi(otm.textmetrics.ascent);  H.wsn(".");
    H.ws("  tmDescent         "); H.wi(otm.textmetrics.descent);  H.wsn(".");
    H.ws("  tmInternalLeading "); H.wi(otm.textmetrics.internalLeading);  H.wsn(".");
    H.ws("  tm height-leading "); H.wi(otm.textmetrics.height - otm.textmetrics.internalLeading);  H.wsn(".");
    H.ws("  tm ascent+descent "); H.wi(otm.textmetrics.ascent + otm.textmetrics.descent);  H.wsn(".");
    H.ws("  otm.ascent        "); H.wi(otm.ascent);   H.wsn(".");
    H.ws("  otm.descent       "); H.wi(otm.descent);  H.wsn(".");
    H.ws("  otm.lineGap       "); H.wi(otm.lineGap);  H.wsn(".");
    H.ws("  otm.macAscent     "); H.wi(otm.macAscent);   H.wsn(".");
    H.ws("  otm.macDescent    "); H.wi(otm.macDescent);  H.wsn(".");
    H.ws("  otm.macLineGap    "); H.wi(otm.macLineGap);  H.wsn(".");
    (* fill in character width at design em square size *)
    ASSERT(H.GetCharABCWidthsW(DisplayDC, 32, 126, SYSTEM.ADR(abcs)) # 0);
    FOR i := 0 TO 94 DO
      face.widths[i] := (abcs[i].a + abcs[i].b + abcs[i].c) * otm.emSquare
                    DIV (otm.textmetrics.height - otm.textmetrics.internalLeading)
    END;
    face.next := Faces;  Faces := face;
    ASSERT(H.DeleteObject(H.SelectObject(DisplayDC, hOldFont)) # 0);
  END
RETURN face END GetFace;

PROCEDURE GetSize(face: Face; em: INTEGER): Font;
VAR font: Font;  sizename: ARRAY 32 OF CHAR;
BEGIN
  font := face.sizes;
  WHILE (font # NIL) & (font.em # em) DO font := font.next END;
  IF font = NIL THEN
    NEW(font);
    font.hfont   := H.CreateFontA(-em, 0,0,0,0,0,0,0,0,7,0,0,0, SYSTEM.ADR(face.name));
    font.face    := face;
    font.em      := em;  H.IntToDecimal(em, sizename);
    font.name    := face.name;  H.Append(" ", font.name);  H.Append(sizename, font.name);
    font.ascent  := (face.ascent   * em - (face.emsize - 1)) DIV face.emsize;  (* Whole pixels rounded up *)
    font.descent := (-face.descent * em + (face.emsize - 1)) DIV face.emsize;  (* Whole pixels rounded up *)
    font.lead    := (face.lead     * em + (face.emsize - 1)) DIV face.emsize;  (* Whole pixels rounded up *)
    font.next    := face.sizes;
    face.sizes   := font;

    H.ws("Fonts.GetSize '");     H.ws(face.name);
    H.ws("' at ");               H.wi(em);           H.wsn(":");
    H.ws("  face.emsize:   ");   H.wi(face.emsize);  H.wsn(".");
    H.ws("  face.ascent:   ");   H.wi(face.ascent);  H.wsn(".");
    H.ws("  face.descent:  ");   H.wi(face.descent); H.wsn(".");
    H.ws("  font.hfont:    ");   H.wh(font.hfont);   H.wsn("H.");
    H.ws("  font.sizename: '");  H.ws(font.name);    H.wsn("'.");
    H.ws("  font.ascent:   ");   H.wi(font.ascent);  H.wsn(".");
    H.ws("  font.descent:  ");   H.wi(font.descent); H.wsn(".");
    H.ws("  font.lead:     ");   H.wi(font.lead);    H.wsn(".");
  END
RETURN font END GetSize;

PROCEDURE GetFont*(name: ARRAY OF CHAR; em: INTEGER): Font;
RETURN
  GetSize(GetFace(name), em)
END GetFont;

PROCEDURE Load*(namesize: ARRAY OF CHAR): Font;  (* Oberon system compatible-ish - size is part of the name *)
VAR
  i:    INTEGER;
  name: ARRAY 128 OF CHAR;
  size: INTEGER;
  unit: INTEGER;
BEGIN
  i := 0;
  ASSERT(namesize[0] # 0X);
  WHILE (i < LEN(namesize)) & (namesize[i] # 0X) DO
    name[i] := namesize[i]; INC(i)
  END;
  DEC(i);
  unit := 1;
  size := 0;
  WHILE (i >= 0) & (name[i] >= "0") & (name[i] <= "9") DO
    size := size + unit * (ORD(name[i]) - ORD('0'));
    unit := unit * 10;
    DEC(i)
  END;
  WHILE (i >= 0) & ((name[i] = " ") OR (name[i] = ".")) DO DEC(i) END;
  ASSERT(i > 0);
  name[i+1] := 0X;
RETURN GetFont(name, size) END Load;

PROCEDURE WriteMap(alpha: BYTE; len: INTEGER);
BEGIN
  WHILE len > 3FH DO WriteMap(alpha, 3FH);  DEC(len, 3FH)  END;
  IF    alpha =  0   THEN AlphaMap[MapLen] :=  40H + len;  INC(MapLen)
  ELSIF alpha >= 40H THEN AlphaMap[MapLen] :=  80H + len;  INC(MapLen)
  ELSIF len   >= 3   THEN AlphaMap[MapLen] := 0C0H + len;  INC(MapLen);
                          AlphaMap[MapLen] := alpha;       INC(MapLen)
  ELSE
    ASSERT(alpha > 0);  ASSERT(alpha <= 3FH);
    WHILE len > 0 DO AlphaMap[MapLen] := alpha;  INC(MapLen);  DEC(len) END
  END
END WriteMap;


PROCEDURE MakeAlphaMap(width, height: INTEGER; buf: ARRAY OF BYTE): INTEGER;
VAR
  stride, len:   INTEGER;
  last,   alpha: INTEGER;
  y:             INTEGER;
  result:        INTEGER;
  pos,    lim:   INTEGER;
BEGIN
  result := MapLen;
  ASSERT(width < 10000H);
  ASSERT(height < 10000H);

  (*
  H.ws("MakeAlphaMap width "); H.wi(width);
  H.ws(", height "); H.wi(height);
  H.ws(", stride "); H.wi(stride);
  H.wsn(".");
  *)

  stride := (width + 3) DIV 4 * 4;
  last   := buf[0];
  pos    := 1;
  lim    := width;
  len    := 1;
  y      := 0;
  WHILE y < height DO
    WHILE pos < lim DO
      alpha := buf[pos];  INC(pos);
      IF alpha = last THEN INC(len)
      ELSE WriteMap(last, len);  last := alpha;  len := 1 END;
    END;
    INC(y);  pos := y * stride;  lim := pos + width;
  END;
  WriteMap(last, len);
  AlphaMap[MapLen] := 0H;
  INC(MapLen);

  (*
  H.ws("MakeAlphaMap. Original size "); H.wi(width * stride);
  H.ws(", compressed size ");  H.wi(MapLen - result);
  H.ws(", reduction to "); H.wi((100 * (MapLen - result)) DIV (width * stride));
  H.wsn("%.");
  w.DumpMem(2, SYSTEM.ADR(AlphaMap[result]), 0, MapLen - result);
  *)

RETURN result END MakeAlphaMap;


PROCEDURE GetAlphaMap*(font: Font; ch: INTEGER; VAR glyph: Glyph);
VAR
  oldfont: INTEGER;
  res:     INTEGER;
  buffer:  ARRAY 16384 OF BYTE;

  glyphmetrics: RECORD
    blackBoxX:    UINT32;
    blackBoxY:    UINT32;
    glyphOriginX: INT32;
    glyphOriginY: INT32;
    cellIncX:     INT16;
    cellIncY:     INT16
  END;

  matrix: RECORD
    em11, em12, em21, em22: UINT32  (* in 16.16 whole.fraction format *)
  END;
BEGIN
  ASSERT(font.hfont # 0);  ASSERT(DisplayDC # 0);

  matrix.em11 := 40000H;
  matrix.em12 := 0;
  matrix.em21 := 0;
  matrix.em22 := 10000H;

  oldfont := H.SelectObject(DisplayDC, font.hfont);
  res := H.GetGlyphOutlineW(
    DisplayDC,
    ch,
    6,                               (* GGO_GRAY8_BITMAP *)
    SYSTEM.ADR(glyphmetrics),
    LEN(buffer), SYSTEM.ADR(buffer),
    SYSTEM.ADR(matrix)
  );
  ASSERT(res > 0);
  ASSERT(H.SelectObject(DisplayDC, oldfont) # 0);

  glyph.mapWidth  := glyphmetrics.blackBoxX;
  glyph.mapHeight := glyphmetrics.blackBoxY;
  glyph.originX   := glyphmetrics.glyphOriginX;
  glyph.baseline  := glyphmetrics.glyphOriginY;

  (*
  H.wc("'");               H.wc(CHR(ch));
  H.ws("' bitmap size ");  H.wi(res); H.wsn(".");
  H.ws("Black box ");      H.wi(glyphmetrics.blackBoxX);     H.wc(",");  H.wi(glyphmetrics.blackBoxY);
  H.ws(", glyph origin "); H.wi(glyphmetrics.glyphOriginX);  H.wc(",");  H.wi(glyphmetrics.glyphOriginY);
  H.ws(", advance ");      H.wi(glyphmetrics.cellIncX);      H.wc(",");  H.wi(glyphmetrics.cellIncY);
  H.wsn(".");
  w.DumpMem(2, SYSTEM.ADR(buffer), 0, res);
  *)

  glyph.map := SYSTEM.ADR(AlphaMap[MakeAlphaMap(glyphmetrics.blackBoxX, glyphmetrics.blackBoxY, buffer)]);
  (*
  H.wsn("Generated alphampap:");
  w.DumpMem(2, glyph.map, 0, SYSTEM.ADR(AlphaMap[MapLen]) - glyph.map);
  *)
END GetAlphaMap;


PROCEDURE GetGlyph*(font: Font; ch: INTEGER): Glyph;
VAR glyph: Glyph;
BEGIN
  ASSERT((ch >= 32) & (ch <= 126));
  glyph := font.glyphs[ch-32];
  IF (glyph = NIL) OR (glyph.map = -1) THEN
    IF glyph = NIL THEN
      NEW(glyph);
      glyph.map := -1;
      font.glyphs[ch-32] := glyph
    END;
    IF ch = 32 THEN
      glyph.mapWidth  := 0;
      glyph.mapHeight := 0;
      glyph.originX   := 0;
      glyph.baseline  := 0;
      glyph.map       := 0
    ELSE
      GetAlphaMap(font, ch, glyph)
    END
  END
RETURN glyph END GetGlyph;


PROCEDURE ConvertOberonPattern*(obmp: INTEGER): Glyph;
VAR i, j:   INTEGER;
    stride: INTEGER;
    tbmp:   ARRAY 2048 OF BYTE;
    ti:     INTEGER;
    width:  INTEGER;
    height: INTEGER;
    x, y:   INTEGER;
    b:      BYTE;
    bit:    INTEGER;
    wb:     INTEGER;  (* Width of oberon bitmap in bytes *)
    ch:     CHAR;
    rowp:   INTEGER;
    alpha:  BYTE;
    glyph:  Glyph;
BEGIN
  SYSTEM.GET(obmp,   b);  width  := b;
  SYSTEM.GET(obmp+1, b);  height := b;
  ti := 0;
  wb := (width + 7) DIV 8;
  stride :=  wb * 8;
  ASSERT(width * height * 4 <= LEN(tbmp));
  H.ws("height "); H.wi(height); H.ws(" stride "); H.wi(stride); H.ws(" width "); H.wi(width); H.wsn(".");
  FOR y := 0 TO height - 1 DO
    rowp := obmp + 2 + (height - 1 - y) * wb;
    H.wil(rowp-obmp, 4); H.ws(": |");
    FOR x := 0 TO width-1 DO
      IF x MOD 8 = 0 THEN
        SYSTEM.GET(rowp, b);  INC(rowp);  H.ws(" <"); H.whz(b,2); H.ws("> ")
      END;
      IF ODD(b) THEN alpha := 64 ELSE alpha := 0 END;
      b := ASR(b, 1);
      IF alpha = 0 THEN H.wc(" ") ELSE H.wc("O") END;
      FOR i := 0 TO 3 DO
        tbmp[y * width * 4  +  x * 4  + i] := alpha
      END
    END;
    H.wsn("|");
  END;
  (* Dump tbmp to be sure *)
  j := 0;
  FOR y := 0 TO height - 1 DO
    H.ws("|");
    FOR x := 0 TO width - 1 DO
      FOR i := 0 TO 3 DO
        IF tbmp[j] = 0 THEN H.wc(" ") ELSE H.wc("O") END; INC(j)
      END
    END;
    H.wsn("|");
  END;

  NEW(glyph);
  glyph.mapWidth  := width*4;
  glyph.mapHeight := height;
  glyph.originX   := 0;
  glyph.baseline  := 0;

  glyph.map := SYSTEM.ADR(AlphaMap[MakeAlphaMap(width*4, height, tbmp)]);

  H.DumpMem(0, glyph.map, 0, SYSTEM.ADR(AlphaMap[MapLen]) - glyph.map);
RETURN glyph END ConvertOberonPattern;

PROCEDURE GetAdvance*(font: Font; ch: INTEGER): INTEGER;  (* Result in 1/256ths of a pixel *)
VAR result: INTEGER;
BEGIN
  IF (ch >= 32) & (ch <= 126) THEN
    result := font.face.widths[ch-32] * font.em * 256 DIV font.face.emsize
  ELSE
    result := 0
  END
RETURN result END GetAdvance;


BEGIN
  H.wsn("Fonts loaded.");
  MapLen := 0;
  DisplayDC := H.CreateDCA(SYSTEM.ADR("DISPLAY"),0,0,0);  ASSERT(DisplayDC # 0);
  (*Default := GetFont("Avrile Serif Medium", 13);*)
  (*Default := GetFont("Cambria", 16);*)
  (*Default := GetFont("Arial", 15);*)
  (*Default := GetFont("Calibri", 17);*)
  Default := GetFont("Consolas", 17);
END WinFonts.
