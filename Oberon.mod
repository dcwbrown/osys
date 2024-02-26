MODULE Oberon;

IMPORT SYSTEM, H := WinHost, K := Kernel, Files, Texts;

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
  (*
  tick1:  INTEGER;
  tick2:  INTEGER;
  alloc1: INTEGER;
  *)
BEGIN
  modadr := H.OberonAdr;
  hdr    := SYSTEM.VAL(H.CodeHeaderPtr, modadr);
  (*
  alloc1 := K.Allocated;
  tick1  := H.Nanotime();
  *)
  REPEAT
    (*
    H.ws("  "); H.WriteModuleName(modadr);
    H.ws(" first ptr ref at "); H.wh(modadr + hdr.pointers); H.wsn("H.");
    *)
    K.Mark(modadr + hdr.pointers);
    INC(modadr, hdr.length);
    hdr := SYSTEM.VAL(H.CodeHeaderPtr, modadr);
  UNTIL hdr.length = 0;
  (*
  tick2 := H.Nanotime();
  H.ws("GC mark completed in "); H.wi((tick2 - tick1) DIV 10); H.wsn(" microseconds.");
  *)
  Files.CloseCollectableFiles;

  (*tick1 := H.Nanotime();*)
  K.Scan;
  (*
  tick2 := H.Nanotime();
  H.ws("GC scan completed in "); H.wi((tick2 - tick1) DIV 10); H.wsn(" microseconds.");
  H.ws("GC recycled "); H.wh(alloc1 - K.Allocated);
  H.ws("H bytes leaving "); H.wh(K.Allocated);
  H.wsn("H allocated.")
  *)
END GC;


(* ----------------------------- Heap debugging ----------------------------- *)

PROCEDURE WriteTypeName(adr: INTEGER);  (* Pass tag address *)
VAR ptr: INTEGER; ch: BYTE;
BEGIN
  INC(adr, 20H);  (* First ptr addr *)
  SYSTEM.GET(adr, ptr);
  WHILE ptr >= 0 DO INC(adr, 8); SYSTEM.GET(adr, ptr) END;
  INC(adr, 8);
  SYSTEM.GET(adr, ch);
  WHILE ch # 0 DO H.wc(CHR(ch)); INC(adr); SYSTEM.GET(adr, ch) END
END WriteTypeName;


PROCEDURE ShowType(indent, ptr: INTEGER);
VAR rec, tag, flag, len, adr: INTEGER;
BEGIN
  H.wb(indent); H.wh(ptr);
  SYSTEM.GET(ptr, rec);  H.ws("H -> ");  H.wh(rec);
  IF rec = 0 THEN
    H.wsn("H.")
  ELSE
    SYSTEM.GET(rec - 16, tag);  H.ws(", tag ");    H.wh(tag);
    SYSTEM.GET(rec - 8, flag);  H.ws("H, flag ");  H.wh(flag);
    H.ws("H '"); WriteTypeName(tag);
    IF flag = -1 THEN
      H.wsn("' (already scanned).")
    ELSE
      SYSTEM.PUT(rec - 8, -1);  SYSTEM.GET(tag, len);
      H.ws("', len ");  H.wh(len);  H.wsn("H.");
      adr := tag + 20H;  (* Address of first ptr *)
      SYSTEM.GET(adr, ptr);
      WHILE ptr >= 0 DO
        H.wb(indent); H.ws("ptr offset "); H.wh(ptr);
        H.ws("H at "); H.wh(rec + ptr); H.wsn("H.");
        ShowType(indent + 1, rec + ptr);
        INC(adr,8); SYSTEM.GET(adr, ptr)
      END
    END
  END
END ShowType;


PROCEDURE ShowHeap*;
VAR modadr, ptradr, ptr: INTEGER;  hdr: H.CodeHeaderPtr;  modname: ARRAY 32 OF CHAR;
BEGIN
  H.wsn("Modules:     LoadAdr        Code     Vars     Ptrs");
  modadr := H.OberonAdr;
  hdr := SYSTEM.VAL(H.CodeHeaderPtr, modadr);
  REPEAT
    H.GetModuleName(modadr, modname);
    H.ws("  "); H.wsl(modname, 10); H.whr(modadr, 10);
    H.ws("H");  H.whr(hdr.imports,  8);
    H.ws("H");  H.whr(hdr.varsize,  8);
    H.ws("H");  H.whr(hdr.pointers, 8);  H.wsn("H.");

    ptradr := modadr + hdr.pointers;
    SYSTEM.GET(ptradr, ptr);
    WHILE ptr >= 0 DO
      ShowType(0, ptr);
      INC(ptradr, 8);  SYSTEM.GET(ptradr, ptr)
    END;
    INC(modadr, hdr.length);
    hdr := SYSTEM.VAL(H.CodeHeaderPtr, modadr);
  UNTIL hdr.length = 0;
END ShowHeap;


(* -------------------------------------------------------------------------- *)

BEGIN
  NEW(Log);  Log.notify := NotifyLog;  Texts.Open(Log, "");
END Oberon.
