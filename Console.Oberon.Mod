MODULE Oberon; (* DCWB 06.04.2024  Minimal Oberon.Mod for console applications *)

IMPORT SYSTEM, H := WinHost, Files, Texts, Modules;

VAR
  Log*: Texts.Text;

  Par*: RECORD
    text*: Texts.Text;
    pos*:  INTEGER
  END;

  ActCnt: INTEGER; (*action count for GC*)
  Mod:    Modules.Module;


PROCEDURE GetSelection* (VAR text: Texts.Text; VAR beg, end, time: INTEGER);
BEGIN  beg := 0;  end := 0;  time := -1  END GetSelection;

PROCEDURE Collect* (count: INTEGER);
BEGIN ActCnt := count
END Collect;

PROCEDURE LogNotify(T: Texts.Text; op: INTEGER; beg, end: INTEGER);
VAR ch: CHAR;  r: Texts.Reader;  b: Texts.Buffer;
BEGIN
  Texts.OpenReader(r, Log, beg);
  WHILE ~r.eot & (beg < end) DO
    Texts.Read(r, ch);
    IF ch = 0DX THEN H.wn ELSE H.wc(ch) END;
    INC(beg)
  END;
  (*Texts.Delete(T, 0, end, b)*)
END LogNotify;

PROCEDURE Getch32*(VAR adr, ch32: INTEGER);  (* Pull 32 bit Unicode from command line *)
VAR
  ch16: SYSTEM.CARD16;
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

PROCEDURE GetCommandLineParameters;
VAR
  w:        Texts.Writer;
  adr:      INTEGER;
  quoted:   BOOLEAN;
  ch:       INTEGER;
  utf8:     ARRAY 5 OF CHAR;
  i:        INTEGER;
BEGIN
  Texts.OpenWriter(w);
  adr := H.GetCommandLineW();
  Getch32(adr, ch);
  quoted  := FALSE;
  WHILE (ch # 0) & ((ch # 32) OR quoted) DO  (* Skip to first unquoted space *)
    IF ch = 22H THEN quoted  := ~quoted END;  Getch32(adr, ch)
  END;
  WHILE ch = 32 DO Getch32(adr, ch) END;  (* Skip to first non-blank *)
  (* Copy remainder of command line to Par.text *)
  WHILE ch # 0 DO
    i := 0;  H.PutUtf8(ch, utf8, i);  utf8[i] := 0X;
    Texts.WriteString(w, utf8);
    Getch32(adr, ch);
  END;
  NEW(Par.text);  Texts.Open(Par.text, "");
  Texts.Append(Par.text, w.buf);
  Par.pos := 0
END GetCommandLineParameters;

BEGIN
  NEW(Log);
  Texts.Open(Log, "");
  Log.notify := LogNotify;
  GetCommandLineParameters;
  IF 63 IN H.Preload.LoadFlags THEN
    H.ws("Console.Oberon loading "); H.ws(H.Preload.LoadMod); H.wsn(".");
    Modules.Load(H.Preload.LoadMod, Mod);  Mod := NIL;
  END;
END Oberon.