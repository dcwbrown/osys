MODULE Files;
IMPORT SYSTEM, Kernel, P := Winshim;

(* Adapted Oberon 07 by NW 11.1.86 / 22.9.93 / 25.5.95 / 25.12.95 / 15.8.2013 *)

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
    next:       File;       (* list of open files *)
    tempname:   FileName;
    registered: BOOLEAN;
    name:       FileName;
    norm:       FileName;   (* Normalised filename helps work with Windows case insensitivity *)
    handle:     INTEGER;
    pos:        INTEGER;
  END;

VAR
  first: File;  (* list of open files *)


(* --------------------------------- Files ---------------------------------- *)

PROCEDURE NormaliseFileName(name: ARRAY OF CHAR; VAR normalised: ARRAY OF CHAR);
VAR i, l: INTEGER; ch: CHAR;
BEGIN i := 0;
  l := LEN(name)-1;  IF l > LEN(normalised)-1 THEN l := LEN(normalised)-1 END;
  ch := name[0];
  WHILE (i < l) & (ch # 0X) DO
    IF (ch > "A") & (ch < "Z") THEN normalised[i] := CHR(ORD(ch) + ORD("a") - ORD("A"))
    ELSIF ch = "\" (*"*) THEN normalised[i] := "/"
    ELSE normalised[i] := ch
    END;
    INC(i); ch := name[i]
  END;
  ASSERT(ch = 0X);
  normalised[i] := 0X
END NormaliseFileName;

PROCEDURE Old*(name: ARRAY OF CHAR): File;
VAR
  h:    INTEGER;
  res:  INTEGER;
  f:    File;
  norm: FileName;
BEGIN
  f := first;
  NormaliseFileName(name, norm);
  WHILE (f # NIL) & ~(f.registered & (f.norm = norm)) DO f := f.next END;
  IF f = NIL THEN
    res := P.FileOpen(name, P.OpenRW, h);
    IF res > 0 THEN (* error encountered *)
      f := NIL
    ELSE
      NEW(f);
      (*P.ws("File allocated at "); P.wh(SYSTEM.VAL(INTEGER, f)); P.wsl("H.");*)
      f.tempname   := "";
      f.registered := TRUE;
      f.name       := name;
      f.norm       := norm;
      f.handle     := h;
      f.next       := first;  first := f;
    END
  END;
  (*P.wsl("Old complete.");*)
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
  res:      INTEGER;
  h:        INTEGER;
  f:        File;
  tempname: FileName;
BEGIN
  MakeTempFile(tempname);
  res := P.FileOpen(tempname, P.OpenRW, h);
  ASSERT(res = 0);
  NEW(f);
  f.tempname   := tempname;
  f.registered := FALSE;
  f.name       := name;
  NormaliseFileName(name, f.norm);
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
  P.MoveFile(f.name, f.tempname);
  ASSERT(P.FileOpen(f.tempname, P.OpenRW, f.handle) = 0);
  f.registered := FALSE;
END Unregister;

PROCEDURE UnregisterByNormalisedName(norm: ARRAY OF CHAR);
VAR f: File;
BEGIN f := first;
  WHILE f # NIL DO
    IF f.registered & (f.norm = norm) THEN Unregister(f) END;
    f := f.next;
  END
END UnregisterByNormalisedName;

PROCEDURE Register*(f: File);
VAR
  tmpnamew: ARRAY MaxPath OF SYSTEM.CARD16;
  regnamew: ARRAY MaxPath OF SYSTEM.CARD16;
  res:      INTEGER;
BEGIN
  IF (f # NIL) & (f.name[0] # 0X) & ~f.registered THEN
    ASSERT(P.CloseHandle(f.handle) # 0);
    UnregisterByNormalisedName(f.norm);
    P.MoveFile(f.tempname, f.name);
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
  (*P.ws("Files.Set, r: Rider at "); P.wh(SYSTEM.ADR(r)); P.wsl("H.");*)
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
    (*
    P.ws("Files.SyncFilePos: r.pos "); P.wh(r.pos);
    P.ws("H, r.file.pos ");            P.wh(r.file.pos);
    P.ws("H, r.file.handle ");         P.wh(r.file.handle);
    P.wsl("H.");
    *)
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
  n := 0;
  y := 0;
  ReadByte(r, b);
  WHILE b >= 80H DO INC(y, LSL(b - 80H, n));  INC(n, 7);  ReadByte(r, b) END;
  INC(y, LSL(b, n));
  IF n < 57 THEN y := ASR(LSL(y, 57-n), 57-n) END  (* Sign extend *)
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

PROCEDURE WriteBytes*(VAR r: Rider; x: ARRAY OF BYTE; n: INTEGER);
VAR res, byteswritten: INTEGER;
BEGIN
  IF n > 0 THEN
    SyncFilePos(r);
    res := P.WriteFile(r.file.handle, SYSTEM.ADR(x), n, SYSTEM.ADR(byteswritten), 0);
    ASSERT(byteswritten = n);
    INC(r.pos, n);  r.file.pos := r.pos
  END
END WriteBytes;

PROCEDURE Write*(VAR r: Rider; ch: CHAR);
VAR res, byteswritten: INTEGER;
BEGIN
  (*P.ws("Files.Write('"); P.wc(ch); P.wsl("'.");*)
  SyncFilePos(r);
  res := P.WriteFile(r.file.handle, SYSTEM.ADR(ch), 1, SYSTEM.ADR(byteswritten), 0);
  ASSERT(byteswritten = 1);
  INC(r.pos);  r.file.pos := r.pos
END Write;

PROCEDURE WriteInt*(VAR r: Rider; x: INTEGER);
BEGIN WriteBytes(r, x, 8) END WriteInt;

PROCEDURE WriteSet*(VAR r: Rider; s: SET);
BEGIN WriteInt(r, ORD(s)) END WriteSet;

PROCEDURE WriteReal*(VAR r: Rider; x: REAL);
BEGIN WriteInt(r, ORD(x)) END WriteReal;

PROCEDURE WriteString*(VAR r: Rider; x: ARRAY OF CHAR);
VAR l: INTEGER;
BEGIN (*l := P.Length(x);*)
  l := 0;  WHILE (l < LEN(x)) & (x[l] # 0X) DO INC(l) END;
  IF l > 0 THEN WriteBytes(r, x, l) END;
  Write(r, 0X)
END WriteString;

PROCEDURE WriteNum*(VAR r: Rider; x: INTEGER);
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
      P.ws("temp name "); P.ws(f.tempname); P.wsl(".")
    ELSE
      P.wsl("registered.");
    END;
    f := f.next
  END;
END DumpFiles;

BEGIN first := NIL
END Files.

--------------------------------------------------------------------------------

PROCEDURE Check(s: ARRAY OF CHAR; VAR name: FileDir.FileName; VAR res: INTEGER);
(* Enforces file name rules: /^[A-Za-z][A-Za-z0-9.]*$/ *)
VAR
  i:  INTEGER;
  ch: CHAR;
BEGIN
  ch := s[0]; i := 0;
  IF (ch >= "A") & (ch <= "Z") OR (ch >= "a") & (ch <= "z") THEN
    REPEAT
      name[i] := ch; INC(i); ch := s[i]
    UNTIL ~(   (ch >= "0") & (ch <= "9") OR (ch >= "A") & (ch <= "Z")
            OR (ch >= "a") & (ch <= "z") OR (ch = "."))
       OR  (i = FileDir.FnLength);
    IF i = FileDir.FnLength THEN res := 4
    ELSIF ch = 0X THEN res := 0;
      WHILE i < FileDir.FnLength DO name[i] := 0X; INC(i) END
    ELSE res := 5
    END
  ELSIF
    ch = 0X THEN name[0] := 0X; res := -1
  ELSE res := 3
  END
END Check;

PROCEDURE New*(name: ARRAY OF CHAR): File;
VAR
  i, res: INTEGER;
  f:       File;
  buf:     Buffer;
  F:       FileDir.FileHd;
  namebuf: FileDir.FileName;
BEGIN
  f := NIL;
  Check(name, namebuf, res);
  IF res <= 0 THEN
    NEW(buf); buf.apos := 0; buf.mod := TRUE; buf.lim := HS; buf.next := buf;
    F := SYSTEM.VAL(FileDir.FileHd, SYSTEM.ADR(buf.data));
    F.mark  := FileDir.HeaderMark;
    F.aleng := 0;
    F.bleng := HS;
    F.name  := namebuf;
    F.date  := Kernel.Clock();
    NEW(f);
    f.aleng      := 0;
    f.bleng      := HS;
    f.modH       := TRUE;
    f.registered := FALSE;
    f.date       := F.date;
    f.firstbuf   := buf;
    f.nofbufs    := 1;
    f.name       := namebuf;
    f.sechint    := 0;
    i := 0;
    REPEAT f.ext[i] := NIL; F.ext[i] := 0; INC(i) UNTIL i = FileDir.ExTabSize;
    i := 0;
    REPEAT f.sec[i] := 0; F.sec[i] := 0; INC(i) UNTIL i = STS
  END;
  RETURN f
END New;

PROCEDURE UpdateHeader(f: File; VAR F: FileDir.FileHeader);
  VAR k: INTEGER;
BEGIN F.aleng := f.aleng; F.bleng := f.bleng;
  F.sec := f.sec; k := (f.aleng + (XS-STS)) DIV XS;
  WHILE k > 0 DO DEC(k); F.ext[k] := f.ext[k].adr END
END UpdateHeader;

PROCEDURE ReadBuf(f: File; buf: Buffer; pos: INTEGER);
  VAR sec: DiskAdr;
BEGIN
  IF pos < STS THEN sec := f.sec[pos]
  ELSE sec := f.ext[(pos-STS) DIV XS].sec[(pos-STS) MOD XS]
  END;
  Kernel.GetSector(sec, buf.data);
  IF pos < f.aleng THEN buf.lim := SS ELSE buf.lim := f.bleng END;
  buf.apos := pos; buf.mod := FALSE
END ReadBuf;

PROCEDURE WriteBuf(f: File; buf: Buffer);
  VAR i, k: INTEGER;
    secadr: DiskAdr; inx: Index;
BEGIN
  IF buf.apos < STS THEN
    secadr := f.sec[buf.apos];
    IF secadr = 0 THEN
      Kernel.AllocSector(f.sechint, secadr);
      f.modH := TRUE; f.sec[buf.apos] := secadr; f.sechint := secadr
    END;
    IF buf.apos = 0 THEN
      UpdateHeader(f, SYSTEM.VAL(FileDir.FileHeader, buf.data)); f.modH := FALSE
    END
  ELSE i := (buf.apos - STS) DIV XS; inx := f.ext[i];
    IF inx = NIL THEN
      NEW(inx); inx.adr := 0; inx.sec[0] := 0; f.ext[i] := inx; f.modH := TRUE
    END;
    k := (buf.apos - STS) MOD XS; secadr := inx.sec[k];
    IF secadr = 0 THEN
      Kernel.AllocSector(f.sechint, secadr);
      f.modH := TRUE; inx.mod := TRUE; inx.sec[k] := secadr; f.sechint := secadr
    END
  END;
  Kernel.PutSector(secadr, buf.data); buf.mod := FALSE
END WriteBuf;

PROCEDURE Buf(f: File; pos: INTEGER): Buffer;
  VAR buf: Buffer;
BEGIN buf := f.firstbuf;
  WHILE (buf.apos # pos) & (buf.next # f.firstbuf) DO buf := buf.next END;
  IF buf.apos # pos THEN buf := NIL END;
  RETURN buf
END Buf;

PROCEDURE GetBuf(f: File; pos: INTEGER): Buffer;
  VAR buf: Buffer;
BEGIN buf := f.firstbuf;
  WHILE (buf.apos # pos) & (buf.next # f.firstbuf) DO buf := buf.next END;
  IF buf.apos # pos THEN
    IF f.nofbufs < MaxBufs THEN  (*allocate new buffer*)
      NEW(buf); buf.next := f.firstbuf.next; f.firstbuf.next := buf; INC(f.nofbufs)
    ELSE (*reuse a buffer*) f.firstbuf := buf;
      IF buf.mod THEN WriteBuf(f, buf) END
    END;
    IF pos <= f.aleng THEN ReadBuf(f, buf, pos) ELSE buf.apos := pos; buf.lim := 0; buf.mod := FALSE END
  END;
  RETURN buf
END GetBuf;

PROCEDURE Unbuffer(f: File);
  VAR i, k: INTEGER;
    buf: Buffer;
    inx: Index;
    head: FileDir.FileHeader;
BEGIN buf := f.firstbuf;
  REPEAT
    IF buf.mod THEN WriteBuf(f, buf) END;
    buf := buf.next
  UNTIL buf = f.firstbuf;
  k := (f.aleng + (XS-STS)) DIV XS; i := 0;
  WHILE i < k DO
    inx := f.ext[i]; INC(i);
    IF inx.mod THEN
      IF inx.adr = 0 THEN
        Kernel.AllocSector(f.sechint, inx.adr); f.sechint := inx.adr; f.modH := TRUE
      END;
      Kernel.PutSector(inx.adr, inx.sec); inx.mod := FALSE
    END
  END;
  IF f.modH THEN
    Kernel.GetSector(f.sec[0], head); UpdateHeader(f, head);
    Kernel.PutSector(f.sec[0], head); f.modH := FALSE
  END
END Unbuffer;

PROCEDURE Register*(f: File);
BEGIN
  IF (f # NIL) & (f.name[0] # 0X) THEN
    Unbuffer(f);
    IF ~f.registered THEN
      FileDir.Insert(f.name, f.sec[0]); f.registered := TRUE; f.next := root; root := SYSTEM.VAL(INTEGER, f)
    END
  END
END Register;

PROCEDURE Close*(f: File);
BEGIN
  IF f # NIL THEN Unbuffer(f) END
END Close;

PROCEDURE Purge*(f: File);
  VAR a, i, j, k: INTEGER;
    ind: FileDir.IndexSector;
BEGIN
  IF f # NIL THEN a := f.aleng + 1; f.aleng := 0; f.bleng := HS;
    IF a <= STS THEN i := a;
    ELSE i := STS; DEC(a, i); j := (a-1) MOD XS; k := (a-1) DIV XS;
      WHILE k >= 0 DO
        Kernel.GetSector(f.ext[k].adr, ind);
        REPEAT DEC(j); Kernel.FreeSector(ind[j]) UNTIL j = 0;
        Kernel.FreeSector(f.ext[k].adr); j := XS; DEC(k)
      END
    END;
    REPEAT DEC(i); Kernel.FreeSector(f.sec[i]) UNTIL i = 0
  END
END Purge;

PROCEDURE Delete*(name: ARRAY OF CHAR; VAR res: INTEGER);
  VAR adr: DiskAdr;
      namebuf: FileDir.FileName;
BEGIN Check(name, namebuf, res);
  IF res = 0 THEN
    FileDir.Delete(namebuf, adr);
    IF adr = 0 THEN res := 2 END
  END
END Delete;

PROCEDURE Rename*(old, new: ARRAY OF CHAR; VAR res: INTEGER);
  VAR adr: DiskAdr;
      oldbuf, newbuf: FileDir.FileName;
      head: FileDir.FileHeader;
BEGIN Check(old, oldbuf, res);
  IF res = 0 THEN
    Check(new, newbuf, res);
    IF res = 0 THEN
      FileDir.Delete(oldbuf, adr);
      IF adr # 0 THEN
        FileDir.Insert(newbuf, adr);
        Kernel.GetSector(adr, head); head.name := newbuf; Kernel.PutSector(adr, head)
      ELSE res := 2
      END
    END
  END
END Rename;

PROCEDURE Length*(f: File): INTEGER;
BEGIN RETURN f.aleng * SS + f.bleng - HS
END Length;

PROCEDURE Date*(f: File): INTEGER;
BEGIN RETURN f.date
END Date;

(* --------------------------Read-------------------------- *)

PROCEDURE Set*(VAR r: Rider; f: File; pos: INTEGER);
  VAR a, b: INTEGER;
BEGIN  r.eof := FALSE; r.res := 0;
  IF f # NIL THEN
    IF pos < 0 THEN a := 0; b := HS
    ELSIF pos < f.aleng * SS + f.bleng - HS THEN
      a := (pos + HS) DIV SS; b := (pos + HS) MOD SS;
    ELSE a := f.aleng; b := f.bleng
    END;
    r.file := f; r.apos := a; r.bpos := b; r.buf := f.firstbuf
  ELSE r.file:= NIL
  END
END Set;

PROCEDURE Pos*(VAR r: Rider): INTEGER;
BEGIN RETURN r.apos * SS + r.bpos - HS
END Pos;

PROCEDURE Base*(VAR r: Rider): File;
BEGIN RETURN r.file
END Base;

PROCEDURE ReadByte*(VAR r: Rider; VAR x: BYTE);
  VAR buf: Buffer;
BEGIN
  IF r.apos # r.buf.apos THEN r.buf := GetBuf(r.file, r.apos) END;
  IF r.bpos < r.buf.lim THEN x := r.buf.data[r.bpos]; INC(r.bpos)
  ELSIF r.apos < r.file.aleng THEN
    INC(r.apos); buf := Buf(r.file, r.apos);
    IF buf = NIL THEN
      IF r.buf.mod THEN WriteBuf(r.file, r.buf) END;
      ReadBuf(r.file, r.buf, r.apos)
    ELSE r.buf := buf
    END;
    x := r.buf.data[0]; r.bpos := 1
  ELSE x := 0; r.eof := TRUE
  END
END ReadByte;

PROCEDURE ReadBytes*(VAR r: Rider; VAR x: ARRAY OF BYTE; n: INTEGER);
  VAR i: INTEGER;
BEGIN i := 0;  (*this implementation is to be improved*)
  WHILE i < n DO ReadByte(r, x[i]); INC(i) END
END ReadBytes;

PROCEDURE Read*(VAR r: Rider; VAR ch: CHAR);
  VAR buf: Buffer;  (*same as ReadByte*)
BEGIN
  IF r.apos # r.buf.apos THEN r.buf := GetBuf(r.file, r.apos) END;
  IF r.bpos < r.buf.lim THEN ch := CHR(r.buf.data[r.bpos]); INC(r.bpos)
  ELSIF r.apos < r.file.aleng THEN
    INC(r.apos); buf := Buf(r.file, r.apos);
    IF buf = NIL THEN
      IF r.buf.mod THEN WriteBuf(r.file, r.buf) END;
      ReadBuf(r.file, r.buf, r.apos)
    ELSE r.buf := buf
    END;
    ch := CHR(r.buf.data[0]); r.bpos := 1
  ELSE ch := 0X; r.eof := TRUE
  END
END Read;

PROCEDURE ReadInt*(VAR R: Rider; VAR x: INTEGER);
  VAR x0, x1, x2, x3: BYTE;
BEGIN ReadByte(R, x0); ReadByte(R, x1); ReadByte(R, x2); ReadByte(R, x3);
  x := ((x3 * 100H + x2) * 100H + x1) * 100H + x0
END ReadInt;

PROCEDURE ReadSet*(VAR R: Rider; VAR s: SET);
  VAR n: INTEGER;
BEGIN ReadInt(R, SYSTEM.VAL(INTEGER, s))
END ReadSet;

PROCEDURE ReadReal*(VAR R: Rider; VAR x: REAL);
  VAR n: INTEGER;
BEGIN ReadInt(R, SYSTEM.VAL(INTEGER, x))
END ReadReal;

PROCEDURE ReadString*(VAR R: Rider; VAR x: ARRAY OF CHAR);
  VAR i: INTEGER; ch: CHAR;
BEGIN i := 0; Read(R, ch);
  WHILE ch # 0X DO
    IF i < LEN(x)-1 THEN x[i] := ch; INC(i) END;
    Read(R, ch)
  END;
  x[i] := 0X
END ReadString;

PROCEDURE ReadNum*(VAR R: Rider; VAR x: INTEGER);
  VAR n, y: INTEGER; b: BYTE;
BEGIN n := 32; y := 0; ReadByte(R, b);
  WHILE b >= 80H DO y := ROR(y + b-80H, 7); DEC(n, 7); ReadByte(R, b) END;
  IF n <= 4 THEN x := ROR(y + b MOD 10H, 4) ELSE x := ASR(ROR(y + b, 7), n-7) END
END ReadNum;

(* --------------------------Write-------------------------- *)

PROCEDURE NewExt(f: File);
  VAR i, k: INTEGER; ext: Index;
BEGIN k := (f.aleng - STS) DIV XS;
  NEW(ext); ext.adr := 0; ext.mod := TRUE; f.ext[k] := ext; i := XS;
  REPEAT DEC(i); ext.sec[i] := 0 UNTIL i = 0
END NewExt;

PROCEDURE WriteByte*(VAR r: Rider; x: BYTE);
  VAR f: File; buf: Buffer;
BEGIN
  IF r.apos # r.buf.apos THEN r.buf := GetBuf(r.file, r.apos); END;
  IF r.bpos >= r.buf.lim THEN
    IF r.bpos < SS THEN
      INC(r.buf.lim); INC(r.file.bleng); r.file.modH := TRUE
    ELSE f := r.file; WriteBuf(f, r.buf); INC(r.apos); buf := Buf(r.file, r.apos);
      IF buf = NIL THEN
        IF r.apos <= f.aleng THEN ReadBuf(f, r.buf, r.apos)
        ELSE r.buf.apos := r.apos; r.buf.lim := 1; f.aleng := f.aleng + 1; f.bleng := 1; f.modH := TRUE;
          IF (f.aleng - STS) MOD XS = 0 THEN NewExt(f) END
        END
      ELSE r.buf := buf
      END;
      r.bpos := 0
    END
  END;
  r.buf.data[r.bpos] := x; INC(r.bpos); r.buf.mod := TRUE
END WriteByte;

PROCEDURE WriteBytes*(VAR r: Rider; x: ARRAY OF BYTE; n: INTEGER);
  VAR i: INTEGER;
BEGIN i := 0; (*this implementation is to be improed*)
  WHILE i < n DO WriteByte(r, x[i]); INC(i) END
END WriteBytes;

PROCEDURE Write*(VAR r: Rider; ch: CHAR);
  VAR f: File; buf: Buffer;
BEGIN (*same as WriteByte*)
  IF r.apos # r.buf.apos THEN r.buf := GetBuf(r.file, r.apos); END;
  IF r.bpos >= r.buf.lim THEN
    IF r.bpos < SS THEN
      INC(r.buf.lim); INC(r.file.bleng); r.file.modH := TRUE
    ELSE f := r.file; WriteBuf(f, r.buf); INC(r.apos); buf := Buf(r.file, r.apos);
      IF buf = NIL THEN
        IF r.apos <= f.aleng THEN ReadBuf(f, r.buf, r.apos)
        ELSE r.buf.apos := r.apos; r.buf.lim := 1; f.aleng := f.aleng + 1; f.bleng := 1; f.modH := TRUE;
          IF (f.aleng - STS) MOD XS = 0 THEN NewExt(f) END
        END
      ELSE r.buf := buf
      END;
      r.bpos := 0
    END
  END;
  r.buf.data[r.bpos] := ORD(ch); INC(r.bpos); r.buf.mod := TRUE
END Write;

PROCEDURE WriteInt*(VAR R: Rider; x: INTEGER);
BEGIN WriteByte(R, x MOD 100H);
  WriteByte(R, x DIV 100H MOD 100H);
  WriteByte(R, x DIV 10000H MOD 100H);
  WriteByte(R, x DIV 1000000H MOD 100H)
END WriteInt;

PROCEDURE WriteSet*(VAR R: Rider; s: SET);
BEGIN WriteInt(R, ORD(s))
END WriteSet;

PROCEDURE WriteReal*(VAR R: Rider; x: REAL);
BEGIN  WriteInt(R, ORD(x))
END WriteReal;

PROCEDURE WriteString*(VAR R: Rider; x: ARRAY OF CHAR);
  VAR i: INTEGER; ch: CHAR;
BEGIN i := 0;
  REPEAT ch := x[i]; Write(R, ch); INC(i) UNTIL ch = 0X
END WriteString;

PROCEDURE WriteNum*(VAR R: Rider; x: INTEGER);
BEGIN
  WHILE (x < -40H) OR (x >= 40H) DO WriteByte(R, x MOD 80H + 80H); x := ASR(x, 7) END;
  WriteByte(R, x MOD 80H)
END WriteNum;

(* --------------------------System use-------------------------- *)

PROCEDURE Init*;
BEGIN root := 0; Kernel.Init; FileDir.Init
END Init;

PROCEDURE RestoreList*; (*after mark phase of garbage collection*)
  VAR f, f0: INTEGER;

  PROCEDURE mark(f: INTEGER): INTEGER;
    VAR m: INTEGER;
  BEGIN
    IF f = 0 THEN m := -1 ELSE SYSTEM.GET(f-4, m) END;
    RETURN m
  END mark;

BEGIN (*field "next" has offset 0*)
  WHILE mark(root) = 0 DO SYSTEM.GET(root, root) END;
  f := root;
  WHILE f # 0 DO
    f0 := f;
    REPEAT SYSTEM.GET(f0, f0) UNTIL mark(f0) # 0;
    SYSTEM.PUT(f, f0); f := f0
  END
END RestoreList;

BEGIN END Files.