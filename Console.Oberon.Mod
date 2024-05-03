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
  Cmd:    ARRAY 100 OF CHAR;
  res:    INTEGER;


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
  i := 0;
  WHILE (ch # 0) & ((ch # 32) OR quoted) DO  (* Skip to first unquoted space *)
    H.PutUtf8(ch, Cmd, i);
    IF ch = 22H THEN quoted  := ~quoted END;
    Getch32(adr, ch)
  END;
  Cmd[i] := 0X;
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

PROCEDURE CallCommand;
VAR S: Texts.Scanner;  P: Modules.Command;
BEGIN
  Texts.OpenScanner(S, Par.text, Par.pos);  Texts.Scan(S);
  IF S.class = Texts.Name THEN
    Par.pos := Texts.Pos(S);
    (*H.ws("Call command '"); H.ws(S.s); H.wsn("'.");*)
    P := Modules.ThisCommand(Mod, S.s);
    IF Modules.res = 0 THEN P END
  END;
END CallCommand;

BEGIN
  (*H.wsn("** Console.Oberon initialising.");*)
  NEW(Log);
  Texts.Open(Log, "");
  Log.notify := LogNotify;
  GetCommandLineParameters;
  IF H.NewLoad IN H.Preload.LoadFlags THEN
    (*H.ws("**** Console.Oberon loading "); H.ws(H.Preload.LoadMod); H.wsn(" ****");*)
    Modules.Load(H.Preload.LoadMod, Mod);
    res := Modules.res;
    IF res = 0 THEN
      (*
      H.ws("Console.Oberon. Cmd '"); H.ws(Cmd);
      H.ws("', H.Preload.LoadMod '"); H.ws(H.Preload.LoadMod); H.wsn("'.");
      *)
      IF Cmd = H.Preload.LoadMod THEN CallCommand; res := Modules.res END
    END;
    IF res # 0 THEN
      H.wsn("**** Load failed. ****");
      H.ws("**** Console.Oberon init load error: "); H.ws(Modules.importing);
      IF    res = 1 THEN H.wsn(" module not found")
      ELSIF res = 2 THEN H.wsn(" bad version")
      ELSIF res = 3 THEN H.ws(" imports ");
                                 H.ws(Modules.imported);
                                 H.wsn(" with bad key");
      ELSIF res = 4 THEN H.wsn(" corrupted obj file")
      ELSIF res = 5 THEN H.wsn(" command not found")
      ELSIF res = 7 THEN H.wsn(" insufficient space")
      END
    END;
    Mod := NIL;
  END;
END Oberon.