MODULE Files;  (* DCWB 2024-01-11..2024-05-11 *)

(* Meaning of result codes used by Wirth Files.Check/Delete/Rename:

   -1 - Empty filename ("")
    0 - OK
    2 - File not found
    3 - Starts with non-letter
    4 - Too long
    5 - Contains other than letter, digit or "."
*)

IMPORT SYSTEM, Kernel, H := WinHost;

CONST MaxPath = H.MaxPath;

TYPE
  FileName* = ARRAY MaxPath OF CHAR;

  File*    = POINTER TO FileDesc;
  FileDesc = RECORD
    next:       POINTER- TO FileDesc;  (* Non-collecting list of open files *)
    name*:      FileName;
    registered: BOOLEAN;
    pos:        INTEGER;
    tempname:   FileName;
    handle:     INTEGER
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
VAR res, hnd: INTEGER;  f: File;
BEGIN
  IF name[0] = 0X THEN
    f := NIL  (* E.g. commonly when Texts.Open passed "" as filename *)
  ELSE
    f := first;
    WHILE (f # NIL) & ~(f.registered & (f.name = name)) DO f := f.next END;
    IF f = NIL THEN
      IF H.FileOpen(name, H.OpenRW, hnd) = 0 THEN
        NEW(f);
        H.ZeroFill(f^);
        f.next       := first;
        f.registered := TRUE;
        f.name       := name;
        f.tempname   := "";
        f.handle     := hnd;
        first        := f;
      END
    END
  END
RETURN f END Old;


PROCEDURE MakeTempFile(VAR name: ARRAY OF CHAR);
VAR temppath: FileName;  res: INTEGER;
BEGIN
  res := H.GetTempPathA(LEN(temppath), SYSTEM.ADR(temppath));
  ASSERT((res > 0) & (res <= LEN(temppath)));
  res := H.GetTempFileNameA(SYSTEM.ADR(temppath), SYSTEM.ADR("OSY"), 0, SYSTEM.ADR(name));
  ASSERT(res # 0)
END MakeTempFile;


PROCEDURE New*(name: ARRAY OF CHAR): File;
VAR
  h:           INTEGER;
  f:           File;
  tempname:    FileName;
  disposition: INTEGER;
BEGIN
  MakeTempFile(tempname);
  ASSERT(H.FileOpen(tempname, H.OpenRW, h) = 0);
  disposition := 1;  (* 1 = DeleteFile on close *)
  ASSERT(H.SetFileInformationByHandle(h, 4, SYSTEM.ADR(disposition), 8) # 0);
  NEW(f);
  H.ZeroFill(f^);
  f.tempname   := tempname;
  f.registered := FALSE;
  f.name       := name;
  f.handle     := h;
  f.next       := first;  first := f;
RETURN f END New;


PROCEDURE Close*(f: File);
VAR res: INTEGER;
BEGIN IF f # NIL THEN res := H.FlushFileBuffers(f.handle) END
END Close;


PROCEDURE CloseHostHandle*(f: File);  (* Do not use the file after calling this *)
BEGIN ASSERT(H.CloseHandle(f.handle) # 0); f.handle := -1;
END CloseHostHandle;


PROCEDURE Unregister(f: File);
VAR disposition, res: INTEGER;
BEGIN
  CloseHostHandle(f);
  MakeTempFile(f.tempname);
  res := H.MoveFile(f.name, f.tempname);
  IF (res # 0) & (res # 2) THEN  (* any failure other than 'not found' *)
    H.ws("** Unregister: Could not move '"); H.ws(f.name); H.ws("' to '");
    H.ws(f.tempname); H.ws("':"); H.AssertWinError(res);
  END;
  ASSERT(H.FileOpen(f.tempname, H.OpenRW, f.handle) = 0);
  disposition := 1;  (* 1 = DeleteFile on close *)
  res := H.SetFileInformationByHandle(f.handle, 4, SYSTEM.ADR(disposition), 8);
  f.registered := FALSE
END Unregister;


PROCEDURE UnregisterByName(name: ARRAY OF CHAR);
VAR f: File;
BEGIN f := first;
  WHILE f # NIL DO
    IF f.registered & (f.name = name) THEN Unregister(f) END;
    f := f.next;
  END;
END UnregisterByName;


PROCEDURE Register*(f: File);
VAR
  nameW:       ARRAY MaxPath OF SYSTEM.CARD16;
  res:         INTEGER;
  err:         INTEGER;
  disposition: INTEGER;
BEGIN
  IF (f # NIL) & (f.name[0] # 0X) & ~f.registered THEN
    disposition := 0;  (* Remove delete on close flag *)
    ASSERT(H.SetFileInformationByHandle(f.handle, 4, SYSTEM.ADR(disposition), 8) # 0);
    CloseHostHandle(f);
    UnregisterByName(f.name);
    IF H.MoveFile(f.tempname, f.name) # 0 THEN
      err := H.GetLastError();
      res := H.Utf8ToUtf16(f.tempname, nameW);  (* Couldn't move temp file, so delete it *)
      res := H.DeleteFileW(SYSTEM.ADR(nameW));
      H.wn; H.ws("** Cannot register "); H.ws(f.name); H.ws(" as it is already in use: **");
      H.AssertWinError(err);
    END;
    ASSERT(H.FileOpen(f.name, H.OpenRW, f.handle) = 0);
    f.registered := TRUE;
  END
END Register;


PROCEDURE ListFiles*;
VAR f: File; h: INTEGER;
BEGIN f := first;
  WHILE f # NIL DO
    H.ws("  at "); H.wh(ORD(f));
    H.ws("H, name '");  H.ws(f.name);
    H.ws("', "); IF ~f.registered THEN H.ws("un") END;
    H.ws("registered, handle "); h := f.handle;
    IF h < 0 THEN H.wc("-"); h := -h END; H.wh(h); H.wsn("H.");
    f := f.next
  END
END ListFiles;


PROCEDURE Rename*(old, new: ARRAY OF CHAR; VAR res: INTEGER);
(* Returns
   0 - success
   2 - old name not found
*)
VAR f: File;
BEGIN
  UnregisterByName(new);
  f := first;  WHILE (f # NIL) & (f.name # old) DO f := f.next END;

  IF f # NIL THEN
    IF f.registered THEN CloseHostHandle(f)
    ELSE f := NIL END
  END;


  (*
  H.ws("Files.Rename("); H.ws(old); H.ws(", "); H.ws(new); H.wsn(", active registered files:");
  ListFiles;
  *)
  res := H.MoveFile(old, new);
  IF (res = 0) & (f # NIL) THEN
    f.registered := TRUE;
    f.name := new
  END;
END Rename;


PROCEDURE Length*(f: File): INTEGER;
VAR res, size: INTEGER;
BEGIN
  res := H.GetFileSizeEx(f.handle, SYSTEM.ADR(size));
  IF res = 0 THEN
    H.ws("Files.Length: GetFileSizeEx failed, f.name '"); H.ws(f.name);
    H.ws("', f.handle: "); H.wh(f.handle); H.wsn("H.");
    ASSERT(FALSE);
  END;
RETURN size END Length;


PROCEDURE Date*(f: File): INTEGER;  (* In 100 nanosecond ticks since 1601 UTC *)
RETURN H.FileTime(f.handle) END Date;


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

PROCEDURE SetFilePos(f: File; pos: INTEGER);
VAR res: INTEGER;
BEGIN
  IF pos # f.pos THEN
    res := H.SetFilePointerEx(f.handle, pos, 0, 0);
    IF res = 0 THEN
      H.ws("SetFilePos failed. File at ");
      H.wh(ORD(f)); H.ws("H, name ");
      H.ws(f.name); H.ws(", File handle "); H.wh(f.handle);
      H.ws("H, pos "); H.wi(pos); H.wsn(".");
      H.AssertWinError(H.GetLastError())
    END;
    ASSERT(res # 0);
    f.pos := pos
  END
END SetFilePos;

(* ---------------------------------- Read ---------------------------------- *)


PROCEDURE ReadRange(VAR r: Rider; n: INTEGER; VAR x: ARRAY OF BYTE; VAR bytesread: INTEGER);
VAR f: File; res, pos, len: INTEGER;
BEGIN
  f  := r.file;
  pos := r.pos;
  len := n;
  IF len > 0 THEN
    SetFilePos(f, pos);
    res := H.ReadFile(f.handle, SYSTEM.ADR(x), len, SYSTEM.ADR(bytesread), 0);
    INC(f.pos, bytesread)
  ELSE
    bytesread := 0;
  END;
  INC(r.pos, bytesread)
END ReadRange;


PROCEDURE ReadBytes*(VAR r: Rider; VAR x: ARRAY OF BYTE; n: INTEGER);
VAR bytesread: INTEGER;
BEGIN
  ASSERT(n <= LEN(x));
  ReadRange(r, n, x, bytesread);
  IF bytesread < n THEN
    r.eof := TRUE;  r.res := n - bytesread;
    REPEAT DEC(n); x[n] := 0 UNTIL n <= bytesread;
  ELSE
    ASSERT(bytesread = n)
  END;
END ReadBytes;


PROCEDURE ReadByte*(VAR r: Rider; VAR x: BYTE);
VAR bytesread: INTEGER;
BEGIN
  ReadRange(r, 1, x, bytesread);
  IF bytesread < 1 THEN
    r.eof := TRUE;  r.res := 1;
    x     := 0;
  ELSE
    ASSERT(bytesread = 1)
  END
END ReadByte;


PROCEDURE Read*(VAR r: Rider; VAR ch: CHAR);
VAR bytesread: INTEGER;
BEGIN
  ReadRange(r, 1, ch, bytesread);
  IF bytesread < 1 THEN
    r.eof := TRUE;  r.res := 1;
    ch    := 0X;
  ELSE
    ASSERT(bytesread = 1)
  END
END Read;

PROCEDURE ReadInt*(VAR r: Rider; VAR x: INTEGER);
BEGIN ReadBytes(r, x, 8) END ReadInt;

PROCEDURE ReadSet*(VAR r: Rider; VAR s: SET);
BEGIN ReadBytes(r, SYSTEM.VAL(INTEGER, s), 8) END ReadSet;

PROCEDURE ReadReal*(VAR r: Rider; VAR x: REAL);
BEGIN ReadBytes(r, SYSTEM.VAL(INTEGER, x), 8) END ReadReal;

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
  (*H.ws("Files.ReadNum ");*)
  n := 0;
  y := 0;
  ReadByte(r, b);  (*H.wh(b);  H.ws(" ");*)
  WHILE b >= 80H DO
    INC(y, LSL(b - 80H, n));
    INC(n, 7);
    ReadByte(r, b);  (*H.wh(b);  H.ws(" ");*)
  END;
  INC(y, LSL(b, n));
  IF n < 57 THEN
    y := ASR(LSL(y, 57-n), 57-n)  (* Sign extend *)
  END;
  (*H.ws(" -> ");  H.wh(y);  H.wsn("H.");  ASSERT(FALSE);*)
  x := y;
END ReadNum;

(* ---------------------------------- Write --------------------------------- *)

PROCEDURE WriteByte*(VAR r: Rider; x: BYTE);
VAR res, byteswritten: INTEGER;  f: File;
BEGIN
  f := r.file;
  SetFilePos(f, r.pos);
  res := H.WriteFile(f.handle, SYSTEM.ADR(x), 1, SYSTEM.ADR(byteswritten), 0);
  ASSERT(byteswritten = 1);
  INC(r.pos);  f.pos := r.pos
END WriteByte;

PROCEDURE WriteBytes*(VAR r: Rider; x: ARRAY OF BYTE;  offset, length: INTEGER);
VAR res, byteswritten: INTEGER;  f: File;
BEGIN
  IF length > 0 THEN
    ASSERT(offset + length <= LEN(x));
    f := r.file;
    SetFilePos(f, r.pos);
    res := H.WriteFile(f.handle, SYSTEM.ADR(x) + offset, length, SYSTEM.ADR(byteswritten), 0);
    ASSERT(byteswritten = length);
    INC(r.pos, length);  f.pos := r.pos
  END
END WriteBytes;

PROCEDURE Write*(VAR r: Rider; ch: CHAR);
VAR res, byteswritten: INTEGER;  f: File;
BEGIN
  f := r.file;
  SetFilePos(f, r.pos);
  res := H.WriteFile(f.handle, SYSTEM.ADR(ch), 1, SYSTEM.ADR(byteswritten), 0);
  ASSERT(byteswritten = 1);
  INC(r.pos);  f.pos := r.pos
END Write;

PROCEDURE WriteInt*(VAR r: Rider; x: INTEGER);
BEGIN WriteBytes(r, x, 0, 8) END WriteInt;

PROCEDURE WriteSet*(VAR r: Rider; s: SET);
BEGIN WriteInt(r, ORD(s)) END WriteSet;

PROCEDURE WriteReal*(VAR r: Rider; x: REAL);
BEGIN WriteInt(r, ORD(x)) END WriteReal;

PROCEDURE WriteString*(VAR r: Rider; x: ARRAY OF CHAR);
VAR l: INTEGER;
BEGIN
  l := 0;  WHILE (l < LEN(x)) & (x[l] # 0X) DO INC(l) END;
  IF l > 0 THEN WriteBytes(r, x, 0, l) END;
  Write(r, 0X)
END WriteString;

PROCEDURE WriteNum*(VAR r: Rider; x: INTEGER); (* Write 7 bits a time least sig bit first *)
BEGIN
  WHILE (x < -40H) OR (x >= 40H) DO WriteByte(r, x MOD 80H + 80H); x := ASR(x, 7) END ;
  WriteByte(r, x MOD 80H)
END WriteNum;


(* ------------------------------- Debugging -------------------------------- *)

PROCEDURE DumpFiles*;
VAR f: File;
BEGIN
  f:= first;
  WHILE f # NIL DO
    H.ws("File '"); H.ws(f.name);  H.ws("' ");
    H.wh(f.handle); H.ws("H ");
    IF ~f.registered THEN
      H.ws("temp name "); H.ws(f.tempname); H.wsn(".")
    ELSE
      H.wsn("registered.");
    END;
    f := f.next
  END;
END DumpFiles;


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

  PROCEDURE finalise(f: File);
  VAR res: INTEGER;
  BEGIN
    (*
    H.ws("* Finalising file at "); H.wh(ORD(f)); H.ws("H, name ");
    H.ws(f.name); H.ws(" ("); IF ~f.registered THEN H.ws("un") END;
    H.wsn("registered).");
    ASSERT(getmark(f) = 0);
    *)
    IF f.registered THEN CloseHostHandle(f) END
  END finalise;

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


(* Files.Mod is initialised by Modules.Init *)
PROCEDURE Init*;
BEGIN first := NIL;  Kernel.Init;
END Init;

END Files.
