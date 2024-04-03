MODULE Files;  (* DCWB 2024-01-11..2024-02-10 *)

IMPORT SYSTEM, Kernel, H := WinHost;

(* DCWB 10.02.2023 *)

CONST MaxPath = H.MaxPath;

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
    name*:      FileName;
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
    res := H.FileOpen(name, H.OpenRW, h);
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
  res := H.GetTempPathA(LEN(temppath), SYSTEM.ADR(temppath));
  ASSERT((res > 0) & (res <= LEN(temppath)));
  res := H.GetTempFileNameA(SYSTEM.ADR(temppath), SYSTEM.ADR("OSY"), 0, SYSTEM.ADR(name));
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
  res := H.FileOpen(tempname, H.OpenRW, h);
  ASSERT(res = 0);
  disposition := 1;  (* 1 = DeleteFile on close *)
  res := H.SetFileInformationByHandle(h, 4, SYSTEM.ADR(disposition), 8);
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
BEGIN IF f # NIL THEN res := H.FlushFileBuffers(f.handle) END END Close;

PROCEDURE Unregister(f: File);
BEGIN
  ASSERT(H.CloseHandle(f.handle) # 0);
  MakeTempFile(f.tempname);
  ASSERT(H.MoveFile(f.name, f.tempname) = 0);
  ASSERT(H.FileOpen(f.tempname, H.OpenRW, f.handle) = 0);
  f.registered := FALSE;
END Unregister;

PROCEDURE UnregisterByName(name: ARRAY OF CHAR);
VAR f: File;
BEGIN f := first;
  (*H.wn; H.ws("* UnregisterByName('"); H.ws(name); H.ws("'");*)
  WHILE f # NIL DO
    (*H.ws(" consider '"); H.ws(f.name); H.wc("'");*)
    IF f.registered & (f.name = name) THEN (*H.ws(" unregister");*) Unregister(f) END;
    f := f.next;
  END;
  (*H.wsn(") *")*)
END UnregisterByName;

PROCEDURE Register*(f: File);
VAR
  nameW:       ARRAY MaxPath OF SYSTEM.CARD16;
  res:         INTEGER;
  err:         INTEGER;
  disposition: INTEGER;
BEGIN
  (*H.wn; H.ws("* Register('"); H.ws(f.name); H.wsn("') *");*)
  IF (f # NIL) & (f.name[0] # 0X) & ~f.registered THEN
    disposition := 0;  (* Remove delete on close flag *)
    res := H.SetFileInformationByHandle(f.handle, 4, SYSTEM.ADR(disposition), 8);
    ASSERT(res # 0);
    ASSERT(H.CloseHandle(f.handle) # 0);
    UnregisterByName(f.name);
    IF H.MoveFile(f.tempname, f.name) # 0 THEN
      err := H.GetLastError();
      res := H.Utf8ToUtf16(f.tempname, nameW);  (* Couldn't move temp file, so delete it *)
      res := H.DeleteFileW(SYSTEM.ADR(nameW));
      H.wn; H.ws("** Cannot register "); H.ws(f.name); H.ws(" as it is already in use: **");
      H.AssertWinErr(err);
    END;
    ASSERT(H.FileOpen(f.name, H.OpenRW, f.handle) = 0);
    f.registered := TRUE;
  END
END Register;


PROCEDURE Rename*(old, new: ARRAY OF CHAR; VAR res: INTEGER);
VAR
  wold, wnew: ARRAY 261 OF SYSTEM.CARD16;
  winresult:  INTEGER;
  f:          File;
BEGIN
  (*
  H.ws("** Files.Rename(old '"); H.ws(old);
  H.ws("', new '"); H.ws(new); H.wsn("') **");
  *)
  winresult := H.Utf8ToUtf16(old, wold);
  winresult := H.Utf8ToUtf16(new, wnew);
  winresult := H.MoveFileExW(SYSTEM.ADR(wold), SYSTEM.ADR(wnew), 1);
  IF winresult = 0 THEN
    (*
    H.ws("** Files.Rename(old '"); H.ws(old);
    H.ws("', new '"); H.ws(new); H.wsn("') failed **");
    *)
    res := 2
  ELSE
    f := first;
    WHILE f # NIL DO
      IF f.registered & (f.name = old) THEN f.name := new END;
      f := f.next
    END;
    res := 0
  END
END Rename;


PROCEDURE Length*(f: File): INTEGER;
VAR res, size: INTEGER;
BEGIN
  res := H.GetFileSizeEx(f.handle, SYSTEM.ADR(size));
  ASSERT(res # 0);
RETURN size END Length;

PROCEDURE Date*(f: File): INTEGER;  (* In 100 nanosecond ticks since 1601 UTC *)
BEGIN RETURN H.FileTime(f.handle) END Date;


(* --------------------------------- Riders --------------------------------- *)

PROCEDURE Set*(VAR r: Rider; f: File; pos: INTEGER);
BEGIN
  (*H.ws("Files.Set, r: Rider at "); H.wh(SYSTEM.ADR(r)); H.wsn("H.");*)
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

PROCEDURE SyncFilePos(VAR r: Rider);
VAR res: INTEGER;
BEGIN
  IF r.pos # r.file.pos THEN
    res := H.SetFilePointerEx(r.file.handle, r.pos, 0, 0);
    IF res = 0 THEN
      H.ws("SyncFilePos failed. File handle "); H.wh(r.file.handle);
      H.ws("H, pos "); H.wi(r.pos); H.wsn(".");
      H.AssertWinErr(H.GetLastError())
    END;
    ASSERT(res # 0);
    r.file.pos := r.pos
  END;
END SyncFilePos;


(* ---------------------------------- Read ---------------------------------- *)

PROCEDURE ReadByte*(VAR r: Rider; VAR x: BYTE);
VAR res, bytesread: INTEGER;
BEGIN
  SyncFilePos(r);
  res := H.ReadFile(r.file.handle, SYSTEM.ADR(x), 1, SYSTEM.ADR(bytesread), 0);
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
  res := H.ReadFile(r.file.handle, SYSTEM.ADR(x), n, SYSTEM.ADR(bytesread), 0);
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
  res := H.ReadFile(r.file.handle, SYSTEM.ADR(ch), 1, SYSTEM.ADR(bytesread), 0);
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
VAR res, byteswritten: INTEGER;
BEGIN
  SyncFilePos(r);
  res := H.WriteFile(r.file.handle, SYSTEM.ADR(x), 1, SYSTEM.ADR(byteswritten), 0);
  ASSERT(byteswritten = 1);
  INC(r.pos);  r.file.pos := r.pos
END WriteByte;

PROCEDURE WriteBytes*(VAR r: Rider; x: ARRAY OF BYTE;  offset, length: INTEGER);
VAR res, byteswritten: INTEGER;
BEGIN
  IF length > 0 THEN
    ASSERT(offset + length <= LEN(x));
    SyncFilePos(r);
    (*H.ws("WriteBytes. offset "); H.wh(offset); H.ws("H, length "); H.wh(length); H.wsn("H.");*)
    res := H.WriteFile(r.file.handle, SYSTEM.ADR(x) + offset, length, SYSTEM.ADR(byteswritten), 0);
    (*H.ws("  byteswritten "); H.wh(byteswritten); H.wsn("H.");*)
    ASSERT(byteswritten = length);
    INC(r.pos, length);  r.file.pos := r.pos
  END
END WriteBytes;

PROCEDURE Write*(VAR r: Rider; ch: CHAR);
VAR res, byteswritten: INTEGER;
BEGIN
  (*H.ws("Files.Write('"); H.wc(ch); H.wsn("'.");*)
  SyncFilePos(r);
  res := H.WriteFile(r.file.handle, SYSTEM.ADR(ch), 1, SYSTEM.ADR(byteswritten), 0);
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
BEGIN (*l := H.Length(x);*)
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
    H.ws("File '"); H.ws(f.name);  H.ws("' ");  H.wh(f.handle);
    H.ws("H ");
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
    ASSERT(getmark(f) = 0);
    IF f.registered THEN
      res := H.CloseHandle(f.handle);
      (*H.ws("Collection closed file "); H.ws(f.name); H.wn*)
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
