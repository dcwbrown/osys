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


PROCEDURE LoadExeObjects;
VAR
  fn16: ARRAY H.MaxPath OF SYSTEM.CARD16;
  fn8:  ARRAY H.MaxPath OF CHAR;
  res:  INTEGER;
  f:    Files.File;
  r:    Files.Rider;
  pos:  INTEGER;
  mod:  Modules.ModDesc;
BEGIN
  res := H.GetModuleFileNameW(0, SYSTEM.ADR(fn16), H.MaxPath);
  ASSERT(res # 0);
  res := H.Utf16ToUtf8(fn16, fn8);
  f := Files.Old(fn8);
  ASSERT(f # NIL);
  pos := H.ObjectOfs;
  Files.Set(r, f, pos);
  REPEAT
    Files.ReadBytes(r, mod, SYSTEM.SIZE(Modules.ModDesc));
    IF ~r.eof THEN
      H.ws(".. module '"); H.ws(mod.name); H.wsn("'.");
      INC(pos, mod.size);
      Files.Set(r, f, pos)
    END
  UNTIL r.eof
END LoadExeObjects;


BEGIN
  IF 63 IN H.LoadFlags THEN H.wsn("Reached Console.Oberon.") END;

  NEW(Log);
  Texts.Open(Log, "");
  Log.notify := LogNotify;

  IF H.ObjectOfs # 0 THEN LoadExeObjects END
END Oberon.