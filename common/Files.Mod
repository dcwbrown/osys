MODULE Files;  (* DCWB 2024-01-11..2024-05-11 *)

(* Meaning of result codes used by Wirth Files.Check/Delete/Rename:

   -1 - Empty filename ("")
    0 - OK
    2 - File not found
    3 - Starts with non-letter
    4 - Too long
    5 - Contains other than letter, digit or "."
*)

IMPORT SYSTEM, Kernel, H := Host;

CONST MaxPath = H.MaxPath;

TYPE
  FileName* = ARRAY MaxPath OF CHAR;

  File*    = POINTER TO FileDesc;
  FileDesc = RECORD (H.HostFile)
    next:    POINTER- TO FileDesc;  (* Non-collecting list of open files *)
    buf:     ARRAY 4096 OF BYTE;
    dirty:   BOOLEAN;
    bufpos:  INTEGER;  (* File position at start of buffer, -1 if not in use *)
    buflen:  INTEGER;  (* normally full, less at end of file *)
    filelen: INTEGER;  (* Current length on disk, excluding latest writes to buffer at eof *)
  END;

  Rider* = RECORD
    eof*:  BOOLEAN;
    res*:  INTEGER;
    pos:   INTEGER;
    file:  File;
  END;

VAR
  first:  POINTER- TO FileDesc;  (* uncollected list of open files *)


(* --------------------------------- Files ---------------------------------- *)

PROCEDURE Old*(name: ARRAY OF CHAR): File;
VAR handle: INTEGER;  f: File;
BEGIN
  IF name[0] = 0X THEN
    f := NIL  (* E.g. commonly when Texts.Open passed "" as filename *)
  ELSE
    f := first;
    WHILE (f # NIL) & ~(f.registered & (f.name = name)) DO f := f.next END;
    IF f = NIL THEN
      NEW(f);  H.ZeroFill(f^);  f.name := name;
      IF H.FileOld(f^) THEN
        f.next    := first;
        f.bufpos  := -1;
        f.filelen := H.FileLength(f^);
        first     := f;
      ELSE
        f := NIL
      END
    END
  END
RETURN f END Old;


PROCEDURE New*(name: ARRAY OF CHAR): File;
VAR f: File;
BEGIN
  NEW(f);  H.ZeroFill(f^);  f.name := name;
  H.FileNew(f^);
  f.next    := first;
  f.bufpos  := -1;
  f.filelen := 0;
  first     := f;
RETURN f END New;


PROCEDURE Unregister(f: File); BEGIN H.FileUnregister(f^) END Unregister;


PROCEDURE UnregisterByName(name: ARRAY OF CHAR);
VAR f: File;
BEGIN f := first;
  WHILE f # NIL DO
    IF f.registered & (f.name = name) THEN Unregister(f) END;
    f := f.next;
  END;
END UnregisterByName;


PROCEDURE UpdateLength(f: File);
BEGIN
  IF (f.bufpos >= 0) & (f.bufpos + f.buflen > f.filelen) THEN
    f.filelen := f.bufpos + f.buflen
  END
END UpdateLength;


PROCEDURE FlushBuffer(f: File);
BEGIN
  IF (f.bufpos >= 0) & f.dirty THEN
    H.FileSetPos(f^, f.bufpos);
    H.FileWrite(f^, SYSTEM.ADR(f.buf), f.buflen);
    f.dirty := FALSE;
    UpdateLength(f)
  END
END FlushBuffer;


PROCEDURE Register*(f: File);
BEGIN
  IF (f # NIL) & (f.name[0] # 0X) & ~f.registered THEN
    UnregisterByName(f.name);
    FlushBuffer(f);
    H.FileRegister(f^)
  END
END Register;


PROCEDURE Rename*(old, new: ARRAY OF CHAR; VAR res: INTEGER);
(* Returns
   0 - success
   2 - old name not found
*)
VAR f: File;
BEGIN
  UnregisterByName(new);
  res := H.FileMove(old, new);
  IF res # 0 THEN
    f := first;  WHILE (f # NIL) & (f.name # old) DO f := f.next END;
    IF (f # NIL) & f.registered THEN f.name := new END
  END;
END Rename;


PROCEDURE Length*(f: File): INTEGER;
BEGIN UpdateLength(f) RETURN f.filelen END Length;


PROCEDURE Date*(f: File): INTEGER;  (* In 100 nanosecond ticks since 2000-01-01 UTC *)
RETURN H.FileTime(f^) END Date;


(* --------------------------------- Riders --------------------------------- *)

PROCEDURE Set*(VAR r: Rider; f: File; pos: INTEGER);
BEGIN
  IF pos < 0 THEN pos := 0 END;
  r.eof   := FALSE;
  r.res   := 0;
  r.file  := f;
  r.pos   := pos;
END Set;

PROCEDURE Pos*(VAR r: Rider): INTEGER;
BEGIN RETURN r.pos END Pos;

PROCEDURE Base*(VAR r: Rider): File;
BEGIN RETURN r.file END Base;


(* ----------------------- Read/Write through buffer ------------------------ *)

PROCEDURE ZeroMem(adr, len: INTEGER);
VAR zero: INTEGER;
BEGIN
  WHILE len >= 8 DO SYSTEM.PUT(adr, 0);  INC(adr, 8);  DEC(len, 8) END;
  WHILE len > 0 DO SYSTEM.PUT(adr, 0X);  INC(adr);  DEC(len)  END;
END ZeroMem;


PROCEDURE LoadBuffer(f: File; pos: INTEGER);
BEGIN
  (*
  H.ws("LoadBuffer '"); H.ws(f.name);
  H.ws("', pos "); H.wh(pos);
  H.ws("H, f.bufpos "); H.wh(f.bufpos);
  H.wsn("H.");
  *)
  FlushBuffer(f);
  IF pos DIV 4096 * 4096 # f.bufpos THEN
    (*
    H.wsn("Load buffer from disk.");
    *)
    f.bufpos := pos DIV 4096 * 4096;
    H.FileSetPos(f^, f.bufpos);
    f.buflen := H.FileRead(f^, SYSTEM.ADR(f.buf), 4096);
    IF f.buflen < 0 THEN f.buflen := 0 END;
  END;
  (*
  H.ws("LoadBuffer complete, bufpos "); H.wh(f.bufpos);
  H.ws("H, buflen "); H.wh(f.buflen); H.wsn("H.");
  *)
END LoadBuffer;


PROCEDURE ReadMem*(VAR r: Rider; adr, len: INTEGER);
VAR f: File;  copyoffset, copylength: INTEGER;
BEGIN f := r.file;
  (*
    H.ws("ReadMem '"); H.ws(r.file.name);
    H.ws("' at "); H.wh(r.pos); H.ws("H, len "); H.wh(len); H.wsn("H.");
  *)
  WHILE len > 0 DO
    (* Satisfy as much as possible from buffer, or reload buffer *)
    IF (f.bufpos >= 0) & (r.pos >= f.bufpos) & (r.pos < f.bufpos + f.buflen) THEN
      (* Calculate affected part of buffer *)
      copyoffset := r.pos - f.bufpos;
      copylength := f.buflen - copyoffset;  ASSERT(copylength > 0);
      IF copylength > len THEN copylength := len END;
      SYSTEM.COPY(SYSTEM.ADR(f.buf[copyoffset]), adr, copylength);
      INC(adr, copylength);  INC(r.pos, copylength);  DEC(len, copylength);
    ELSE
      (* Load appropriate buffer *)
      LoadBuffer(f, r.pos);
      IF r.pos >= f.bufpos + f.buflen THEN  (* cannot satisfy any further reading *)
        r.eof := TRUE;   r.res := len;
        ZeroMem(adr, len);
        len := 0
      END
    END
  END;
  (*
  H.wsn("ReadMem complete.")
  *)
END ReadMem;


PROCEDURE WriteMem*(VAR r: Rider; adr, len: INTEGER);
VAR f: File;  copyoffset, copylength: INTEGER;
BEGIN f := r.file;
  (*
  H.ws("WriteMem '"); H.ws(r.file.name);
  H.ws("' at "); H.wh(r.pos); H.ws("H, len "); H.wh(len); H.wsn("H.");
  *)
  WHILE len > 0 DO
    (*
    H.ws("WriteMem loop. r.pos "); H.wh(r.pos);
    H.ws("H, f.bufpos "); H.wh(f.bufpos);
    H.ws("H, f.buflen "); H.wh(f.buflen);
    H.ws("H, len "); H.wh(len); H.wsn("H.");
    *)
    (* Write as much as possible to existing buffer, or advance buffer *)
    IF (f.bufpos >= 0) & (r.pos >= f.bufpos) & (r.pos < f.bufpos + 4096) THEN
      (* Write as much as possible to existing buffer *)
      copyoffset := r.pos - f.bufpos;
      copylength := 4096 - copyoffset;  IF copylength > len THEN copylength := len END;
      ASSERT(copylength > 0);  ASSERT(copylength <= 4096);
      (*
      H.ws("  copyoffset "); H.wh(copyoffset);
      H.ws("H, copylength "); H.wh(copylength); H.wsn("H.");
      *)
      IF copyoffset > f.buflen THEN ZeroMem(SYSTEM.ADR(f.buf[f.buflen]), copyoffset - f.buflen) END;

      SYSTEM.COPY(adr, SYSTEM.ADR(f.buf[copyoffset]), copylength);
      IF f.buflen < copyoffset + copylength THEN f.buflen := copyoffset + copylength END;
      INC(adr, copylength);  INC(r.pos, copylength);  DEC(len, copylength);
      f.dirty := TRUE;
    ELSE
      LoadBuffer(f, r.pos)
    END
  END;
  (*
  H.wsn("WriteMem complete.")
  *)
END WriteMem;


PROCEDURE Close*(f: File);
BEGIN IF f # NIL THEN FlushBuffer(f); H.FileFlush(f^) END END Close;


(* ---------------------------------- Read ---------------------------------- *)

PROCEDURE ReadRaw*(VAR r: Rider;  VAR var: ARRAY OF BYTE);
BEGIN ReadMem(r, SYSTEM.ADR(var), LEN(var)) END ReadRaw;

PROCEDURE ReadByte*(VAR r: Rider; VAR x: BYTE);
BEGIN ReadMem(r, SYSTEM.ADR(x), 1) END ReadByte;

PROCEDURE Read*(VAR r: Rider; VAR ch: CHAR);
BEGIN ReadMem(r, SYSTEM.ADR(ch), 1) END Read;

PROCEDURE ReadInt*(VAR r: Rider; VAR x: INTEGER);
BEGIN ReadMem(r, SYSTEM.ADR(x), 8) END ReadInt;

PROCEDURE ReadSet*(VAR r: Rider; VAR s: SET);
BEGIN ReadMem(r, SYSTEM.ADR(s), 8) END ReadSet;

PROCEDURE ReadReal*(VAR r: Rider; VAR x: REAL);
BEGIN ReadMem(r, SYSTEM.ADR(x), 8) END ReadReal;

PROCEDURE ReadString*(VAR r: Rider; VAR x: ARRAY OF CHAR);
VAR i: INTEGER;  ch: CHAR;
BEGIN i := 0;
  Read(r, ch);
  WHILE (i < LEN(x) - 1) & (ch # 0X) DO  x[i] := ch;  INC(i);  Read(r, ch)  END;
  WHILE ch # 0X DO Read(r, ch) END;
  x[i] := 0X
END ReadString;

PROCEDURE ReadNum*(VAR r: Rider; VAR x: INTEGER);
VAR n, y: INTEGER;  b: BYTE;
BEGIN
  n := 0;
  y := 0;
  ReadByte(r, b);
  WHILE b >= 80H DO
    INC(y, LSL(b - 80H, n));
    INC(n, 7);
    ReadByte(r, b);
  END;
  INC(y, LSL(b, n));
  IF n < 57 THEN
    y := ASR(LSL(y, 57-n), 57-n)  (* Sign extend *)
  END;
  x := y;
END ReadNum;


(* ---------------------------------- Write --------------------------------- *)

PROCEDURE WriteRaw*(VAR r: Rider; var: ARRAY OF BYTE);
BEGIN WriteMem(r, SYSTEM.ADR(var), LEN(var)) END WriteRaw;

PROCEDURE WriteByte*(VAR r: Rider; x: BYTE);
BEGIN WriteMem(r, SYSTEM.ADR(x), 1) END WriteByte;

PROCEDURE Write*(VAR r: Rider; ch: CHAR);
BEGIN WriteMem(r, SYSTEM.ADR(ch), 1) END Write;

PROCEDURE WriteInt*(VAR r: Rider; x: INTEGER);
BEGIN WriteMem(r, SYSTEM.ADR(x), 8) END WriteInt;

PROCEDURE WriteSet*(VAR r: Rider; s: SET);
BEGIN WriteMem(r, SYSTEM.ADR(s), 8) END WriteSet;

PROCEDURE WriteReal*(VAR r: Rider; x: REAL);
BEGIN WriteMem(r, SYSTEM.ADR(x), 8) END WriteReal;

PROCEDURE WriteString*(VAR r: Rider; x: ARRAY OF CHAR);
VAR l: INTEGER;
BEGIN
  l := 0;  WHILE (l < LEN(x)) & (x[l] # 0X) DO INC(l) END;
  IF l > 0 THEN WriteMem(r, SYSTEM.ADR(x), l) END;
  Write(r, 0X)
END WriteString;

PROCEDURE WriteNum*(VAR r: Rider; x: INTEGER); (* Write 7 bits a time least sig bit first *)
BEGIN
  WHILE (x < -40H) OR (x >= 40H) DO WriteByte(r, x MOD 80H + 80H); x := ASR(x, 7) END ;
  WriteByte(r, x MOD 80H)
END WriteNum;


(* -------------- Files list cleanup during garbage collection -------------- *)

PROCEDURE CloseCollectableFiles*;  (* Only for calling between GC mark and scan *)
VAR f, g: File;

  PROCEDURE getmark(f: File): INTEGER;
  VAR mark: INTEGER;
  BEGIN
    IF f = NIL THEN
      mark := -1
    ELSE
      SYSTEM.GET(ORD(f) - 8, mark)
    END
  RETURN mark END getmark;

  PROCEDURE finalise(f: File); BEGIN H.FileClose(f^) END finalise;

BEGIN
  (* Skip first to first still-live file *)
  WHILE getmark(first) = 0 DO finalise(first); first := first.next END;
  f := first;
  WHILE f # NIL DO
    g := f.next;
    (* skip g to next live file or end (NIL) *)
    WHILE getmark(g) = 0 DO finalise(g); g := g.next END;
    f.next := g;
    f := g
  END
END CloseCollectableFiles;


PROCEDURE DeleteUnregisteredFilesAtExit;
VAR f: File;
BEGIN
f := first;
  WHILE f # NIL DO
    IF ~f.registered THEN H.FileClose(f^) END;
    f := f.next
  END
END DeleteUnregisteredFilesAtExit;


(* Files.Mod is initialised by Modules.Init *)
PROCEDURE Init*(stackorg: INTEGER);
BEGIN
  first := NIL;
  H.SetDeleteUnregisteredFilesAtExit(DeleteUnregisteredFilesAtExit);
  Kernel.Init(stackorg);
END Init;

END Files.
