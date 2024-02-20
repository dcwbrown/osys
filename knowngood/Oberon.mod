MODULE Oberon;

IMPORT SYSTEM, H := Winshim, K := Kernel, Texts;

VAR
  Log*: Texts.Text;
  R:    Texts.Reader;

PROCEDURE NotifyLog(T: Texts.Text; op: INTEGER; beg, end: INTEGER);
VAR buf: ARRAY 256 OF CHAR;  pos, i: INTEGER;
BEGIN
  IF op = 1 (* Insert *) THEN
    Texts.OpenReader(R, Log, beg);  pos := beg;
    WHILE pos < end DO
      i := 0;
      WHILE (pos < end) & (i < LEN(buf)-1) DO
        Texts.Read(R, buf[i]);
        IF buf[i] = 0DX THEN INC(i); buf[i] := 0AX END;  (* Add LF after CR *)
        INC(pos);  INC(i)
      END;
      buf[i] := 0X; H.Log(buf);
    END
  END
END NotifyLog;


PROCEDURE GC*;
VAR
  modadr: INTEGER;
  hdr:    H.CodeHeaderPtr;
  tick1:  INTEGER;
  tick2:  INTEGER;
  alloc1: INTEGER;
BEGIN
  modadr := H.OberonAdr;
  hdr    := SYSTEM.VAL(H.CodeHeaderPtr, modadr);
  alloc1 := K.Allocated;
  tick1  := H.Nanotime();
  REPEAT
    (*
    H.ws("  "); H.WriteModuleName(modadr);
    H.ws(" first ptr ref at "); H.wh(modadr + hdr.pointers); H.wsn("H.");
    *)
    K.Mark(modadr + hdr.pointers);
    INC(modadr, hdr.length);
    hdr := SYSTEM.VAL(H.CodeHeaderPtr, modadr);
  UNTIL hdr.length = 0;
  tick2 := H.Nanotime();
  H.ws("GC mark completed in "); H.wi((tick2 - tick1) DIV 10); H.wsn(" microseconds.");

  (*Files.RestoreList;*)

  tick1 := H.Nanotime();
  K.Scan;
  tick2 := H.Nanotime();
  H.ws("GC scan completed in "); H.wi((tick2 - tick1) DIV 10); H.wsn(" microseconds.");
  H.ws("GC recycled "); H.wh(alloc1 - K.Allocated);
  H.ws("H bytes leaving "); H.wh(K.Allocated);
  H.wsn("H allocated.")
END GC;

BEGIN
  NEW(Log);  Log.notify := NotifyLog;  Texts.Open(Log, "");
END Oberon.
