MODULE Files;  (* DCWB 11.01.2024..10.02.2024 *)

IMPORT SYSTEM, Kernel, P := WinHost;

(* DCWB 10.02.2023 *)

CONST MaxPath = P.MaxPath;

TYPE
  FileName = ARRAY MaxPath OF CHAR;
  File*    = POINTER TO FileDesc;

  Rider* = RECORD
    eof*:  BOOLEAN;
    res*:  INTEGER;
    pos:   INTEGER;
    file:  File;
  END;

  FileDesc = RECORD
    next:       POINTER- TO FileDesc;  (* Non-collecting list of open files *)
    tempname:   FileName;
    registered: BOOLEAN;
    name:       FileName;
    handle:     INTEGER;
    pos:        INTEGER;
  END;

VAR
  first: POINTER- TO FileDesc;  (* uncollected list of open files *)


(* --------------------------------- Files ---------------------------------- *)

PROCEDURE Old*(name: ARRAY OF CHAR): File;
VAR
  h:    INTEGER;
  res:  INTEGER;
  f:    File;
BEGIN
  f := first;
  WHILE (f # NIL) & ~(f.registered & (f.name = name)) DO f := f.next END;
  IF f = NIL THEN
    res := P.FileOpen(name, P.OpenRW, h);
    IF res > 0 THEN (* error encountered *)
      f := NIL
    ELSE
      NEW(f);
      f.tempname   := "";
      f.registered := TRUE;
      f.name       := name;
      f.handle     := h;
      f.next       := first;  first := f;
    END
  END;
RETURN f END Old;

PROCEDURE MakeTempFile(VAR name: ARRAY OF CHAR);
VAR temppath: FileName;  res: INTEGER;
BEGIN
  res := P.GetTempPathA(LEN(temppath), SYSTEM.ADR(temppath));
  ASSERT((res > 0) & (res <= LEN(temppath)));
  res := P.GetTempFileNameA(SYSTEM.ADR(temppath), SYSTEM.ADR("OSY"), 0, SYSTEM.ADR(name));
  ASSERT(res # 0)
END MakeTempFile;

PROCEDURE New*(name: ARRAY OF CHAR): File;
VAR
  res:         INTEGER;
  h:           INTEGER;
  f:           File;
  tempname:    FileName;
  disposition: INTEGER;
BEGIN
  MakeTempFile(tempname);
  res := P.FileOpen(tempname, P.OpenRW, h);
  ASSERT(res = 0);
  disposition := 1;  (* 1 = DeleteFile on close *)
  res := P.SetFileInformationByHandle(h, 4, SYSTEM.ADR(disposition), 8);
  ASSERT(res # 0);
  NEW(f);
  f.tempname   := tempname;
  f.registered := FALSE;
  f.name       := name;
  f.handle     := h;
  f.next       := first;  first := f;
RETURN f END New;

PROCEDURE Close*(f: File);
VAR res: INTEGER;
BEGIN IF f # NIL THEN res := P.FlushFileBuffers(f.handle) END END Close;

PROCEDURE Unregister(f: File);
BEGIN
  ASSERT(P.CloseHandle(f.handle) # 0);
  MakeTempFile(f.tempname);
  ASSERT(P.MoveFile(f.name, f.tempname) = 0);
  ASSERT(P.FileOpen(f.tempname, P.OpenRW, f.handle) = 0);
  f.registered := FALSE;
END Unregister;

PROCEDURE UnregisterByName(name: ARRAY OF CHAR);
VAR f: File;
BEGIN f := first;
  WHILE f # NIL DO
    IF f.registered & (f.name = name) THEN Unregister(f) END;
    f := f.next;
  END
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
    res := P.SetFileInformationByHandle(f.handle, 4, SYSTEM.ADR(disposition), 8);
    ASSERT(res # 0);
    ASSERT(P.CloseHandle(f.handle) # 0);
    UnregisterByName(f.name);
    IF P.MoveFile(f.tempname, f.name) # 0 THEN
      err := P.GetLastError();
      res := P.Utf8ToUtf16(f.tempname, nameW);  (* Couldn't move temp file, so delete it *)
      res := P.DeleteFileW(SYSTEM.ADR(nameW));
      P.wn; P.ws("** Cannot register "); P.ws(f.name); P.ws(" as it is already in use: **");
      P.AssertWinErr(err);
    END;
    ASSERT(P.FileOpen(f.name, P.OpenRW, f.handle) = 0);
    f.registered := TRUE;
  END
END Register;

PROCEDURE Length*(f: File): INTEGER;
VAR res, size: INTEGER;
BEGIN
  res := P.GetFileSizeEx(f.handle, SYSTEM.ADR(size));
  ASSERT(res # 0);
RETURN size END Length;

(* --------------------------------- Riders --------------------------------- *)

PROCEDURE Set*(VAR r: Rider; f: File; pos: INTEGER);
VAR res: INTEGER;
BEGIN
  (*P.ws("Files.Set, r: Rider at "); P.wh(SYSTEM.ADR(r)); P.wsn("H.");*)
  r.eof   := FALSE;
  r.res   := 0;
  r.file  := f;
  r.pos   := pos;
END Set;

PROCEDURE Pos*(VAR r: Rider): INTEGER;
BEGIN RETURN r.pos END Pos;

PROCEDURE Base*(VAR r: Rider): File;
BEGIN RETURN r.file END Base;

PROCEDURE SyncFilePos(VAR r: Rider);
VAR res: INTEGER;
BEGIN
  IF r.pos # r.file.pos THEN
    res := P.SetFilePointerEx(r.file.handle, r.pos, 0, 0);
    ASSERT(res # 0);
    r.file.pos := r.pos
  END;
END SyncFilePos;


(* ---------------------------------- Read ---------------------------------- *)

PROCEDURE ReadByte*(VAR r: Rider; VAR x: BYTE);
VAR res, bytesread: INTEGER;
BEGIN
  SyncFilePos(r);
  res := P.ReadFile(r.file.handle, SYSTEM.ADR(x), 1, SYSTEM.ADR(bytesread), 0);
  IF bytesread < 1 THEN
    r.eof := TRUE;  r.res := 1;  x := 0
  ELSE
    ASSERT(bytesread = 1);
    INC(r.pos);  r.file.pos := r.pos
  END
END ReadByte;

PROCEDURE ReadBytes*(VAR r: Rider; VAR x: ARRAY OF BYTE; n: INTEGER);
VAR res, bytesread: INTEGER;
BEGIN
  ASSERT(n <= LEN(x));
  SyncFilePos(r);
  res := P.ReadFile(r.file.handle, SYSTEM.ADR(x), n, SYSTEM.ADR(bytesread), 0);
  IF bytesread < n THEN
    r.eof := TRUE;
    r.res := n - bytesread;
    REPEAT DEC(n); x[n] := 0 UNTIL n <= bytesread;
  ELSE
    ASSERT(bytesread = n)
  END;
  INC(r.pos, bytesread);  r.file.pos := r.pos
END ReadBytes;

PROCEDURE Read*(VAR r: Rider; VAR ch: CHAR);
VAR res, bytesread: INTEGER;
BEGIN
  SyncFilePos(r);
  res := P.ReadFile(r.file.handle, SYSTEM.ADR(ch), 1, SYSTEM.ADR(bytesread), 0);
  IF bytesread < 1 THEN
    r.eof := TRUE;  r.res := 1;  ch := 0X
  ELSE
    ASSERT(bytesread = 1);
    INC(r.pos);  r.file.pos := r.pos
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
  (*P.ws("Files.ReadNum ");*)
  n := 0;
  y := 0;
  ReadByte(r, b);  (*P.wh(b);  P.ws(" ");*)
  WHILE b >= 80H DO
    INC(y, LSL(b - 80H, n));
    INC(n, 7);
    ReadByte(r, b);  (*P.wh(b);  P.ws(" ");*)
  END;
  INC(y, LSL(b, n));
  IF n < 57 THEN
    y := ASR(LSL(y, 57-n), 57-n)  (* Sign extend *)
  END;
  (*P.ws(" -> ");  P.wh(y);  P.wsn("H.");  ASSERT(FALSE);*)
  x := y;
END ReadNum;

(* ---------------------------------- Write --------------------------------- *)

PROCEDURE WriteByte*(VAR r: Rider; x: BYTE);
VAR res, byteswritten: INTEGER;
BEGIN
  SyncFilePos(r);
  res := P.WriteFile(r.file.handle, SYSTEM.ADR(x), 1, SYSTEM.ADR(byteswritten), 0);
  ASSERT(byteswritten = 1);
  INC(r.pos);  r.file.pos := r.pos
END WriteByte;

PROCEDURE WriteBytes*(VAR r: Rider; x: ARRAY OF BYTE;  offset, length: INTEGER);
VAR res, byteswritten: INTEGER;
BEGIN
  IF length > 0 THEN
    ASSERT(offset + length <= LEN(x));
    SyncFilePos(r);
    (*P.ws("WriteBytes. offset "); P.wh(offset); P.ws("H, length "); P.wh(length); P.wsn("H.");*)
    res := P.WriteFile(r.file.handle, SYSTEM.ADR(x) + offset, length, SYSTEM.ADR(byteswritten), 0);
    (*P.ws("  byteswritten "); P.wh(byteswritten); P.wsn("H.");*)
    ASSERT(byteswritten = length);
    INC(r.pos, length);  r.file.pos := r.pos
  END
END WriteBytes;

PROCEDURE Write*(VAR r: Rider; ch: CHAR);
VAR res, byteswritten: INTEGER;
BEGIN
  (*P.ws("Files.Write('"); P.wc(ch); P.wsn("'.");*)
  SyncFilePos(r);
  res := P.WriteFile(r.file.handle, SYSTEM.ADR(ch), 1, SYSTEM.ADR(byteswritten), 0);
  ASSERT(byteswritten = 1);
  INC(r.pos);  r.file.pos := r.pos
END Write;

PROCEDURE WriteInt*(VAR r: Rider; x: INTEGER);
BEGIN WriteBytes(r, x, 0, 8) END WriteInt;

PROCEDURE WriteSet*(VAR r: Rider; s: SET);
BEGIN WriteInt(r, ORD(s)) END WriteSet;

PROCEDURE WriteReal*(VAR r: Rider; x: REAL);
BEGIN WriteInt(r, ORD(x)) END WriteReal;

PROCEDURE WriteString*(VAR r: Rider; x: ARRAY OF CHAR);
VAR l: INTEGER;
BEGIN (*l := P.Length(x);*)
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
    P.ws("File '"); P.ws(f.name);  P.ws("' ");  P.wh(f.handle);
    P.ws("H ");
    IF ~f.registered THEN
      P.ws("temp name "); P.ws(f.tempname); P.wsn(".")
    ELSE
      P.wsn("registered.");
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
      SYSTEM.GET(SYSTEM.VAL(INTEGER, f) - 8, mark)
    END
  RETURN mark END getmark;

  PROCEDURE finalise(f: File);
  VAR res: INTEGER;
  BEGIN
    ASSERT(getmark(f) = 0);
    IF f.registered THEN
      res := P.CloseHandle(f.handle);
      (*P.ws("Collection closed file "); P.ws(f.name); P.wn*)
    END;
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

BEGIN first := NIL
END Files.
