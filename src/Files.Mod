MODULE Files;  (*NW 11.1.86 / 22.9.93 / 25.5.95 / 25.12.95 / 15.8.2013*)
               (*DB 2024-07-21 Replace direct disk access by host file system *)

IMPORT SYSTEM, H := Host, Kernel;

CONST MaxBufs = 4;  Bufsize = 4096;

TYPE
  File*  = POINTER TO FileDesc;
  Buffer = POINTER TO BufferRecord;

  Rider* = RECORD eof*: BOOLEAN;
    res*: INTEGER;
    file: File;
    pos:  INTEGER;
    buf:  Buffer
  END;

  FileDesc = RECORD (H.HostFile)
    next:     POINTER- TO FileDesc;  (* Non-collecting list of open files *)
    nofbufs:  INTEGER;
    firstbuf: Buffer;
  END;

  BufferRecord = RECORD
    page: INTEGER;
    lim:  INTEGER;
    mod:  BOOLEAN;
    next: Buffer;
    data: ARRAY Bufsize OF BYTE;
  END;

VAR
  first: POINTER- TO FileDesc;  (* uncollected list of open files *)


PROCEDURE Old*(name: ARRAY OF CHAR): File;
VAR f: File;  buf: Buffer;
BEGIN f := NIL;
  IF name[0] # 0X THEN
    f := first;
    WHILE (f # NIL) & ~(f.registered & (f.name = name)) DO f := f.next END;
    IF f = NIL THEN
      NEW(f);  H.ZeroFill(f^);  f.name := name;
      IF H.FileOld(f^) THEN
        NEW(buf);
        buf.page   := 0;
        buf.next   := buf;
        buf.mod    := FALSE;
        buf.lim    := H.FileRead(f^, SYSTEM.ADR(buf.data), Bufsize);  ASSERT(buf.lim >= 0);
        f.firstbuf := buf;
        f.nofbufs  := 1;
        f.next     := first;
        first      := f;
      ELSE
        f := NIL
      END
    END
  END
RETURN f END Old;


PROCEDURE New*(name: ARRAY OF CHAR): File;
VAR f: File;  buf: Buffer;
BEGIN
  NEW(buf);
  buf.page := 0;
  buf.mod  := TRUE;
  buf.lim  := 0;
  buf.next := buf;
  NEW(f);  H.ZeroFill(f^);
  f.name     := name;
  f.firstbuf := buf;
  f.nofbufs  := 1;
  f.next     := first;
  first      := f;
  H.FileNew(f^);
RETURN f END New;


PROCEDURE ReadBuf(f: File; buf: Buffer; page: INTEGER);  (* page = sector number *)
BEGIN
  H.FileSetPos(f^, page * Bufsize);
  buf.lim  := H.FileRead(f^, SYSTEM.ADR(buf.data), Bufsize);
  buf.page := page;
  buf.mod  := FALSE;
END ReadBuf;


PROCEDURE WriteBuf(f: File; buf: Buffer);
BEGIN
  H.FileSetPos(f^, buf.page * Bufsize);
  H.FileWrite(f^, SYSTEM.ADR(buf.data), buf.lim);
  buf.mod := FALSE
END WriteBuf;


PROCEDURE Buf(f: File; page: INTEGER): Buffer;
VAR buf: Buffer;
BEGIN buf := f.firstbuf;
  WHILE (buf.page # page) & (buf.next # f.firstbuf) DO buf := buf.next END;
  IF buf.page # page THEN buf := NIL END
RETURN buf END Buf;


PROCEDURE GetBuf(f: File; page: INTEGER): Buffer;
VAR buf: Buffer;
BEGIN buf := f.firstbuf;
  WHILE (buf.page # page) & (buf.next # f.firstbuf) DO buf := buf.next END;
  IF buf.page # page THEN
    IF f.nofbufs < MaxBufs THEN  (*allocate new buffer*)
      NEW(buf); buf.next := f.firstbuf.next; f.firstbuf.next := buf; INC(f.nofbufs)
    ELSE (*reuse a buffer*)
      f.firstbuf := buf;
      IF buf.mod THEN WriteBuf(f, buf) END
    END;
    IF page * Bufsize <= f.len THEN
      ReadBuf(f, buf, page)
    ELSE
      buf.page := page; buf.lim := 0; buf.mod := FALSE
    END
  END;
  RETURN buf
END GetBuf;


PROCEDURE Unbuffer(f: File);
VAR buf: Buffer;
BEGIN buf := f.firstbuf;
  REPEAT IF buf.mod THEN WriteBuf(f, buf) END;  buf := buf.next
  UNTIL buf = f.firstbuf
END Unbuffer;


PROCEDURE UnregisterByName(name: ARRAY OF CHAR);
VAR f: File;
BEGIN f := first;
  WHILE f # NIL DO
    IF f.registered & (f.name = name) THEN H.FileUnregister(f^) END;
    f := f.next;
  END;
END UnregisterByName;


PROCEDURE Register*(f: File);
BEGIN
  IF (f # NIL) & (f.name[0] # 0X) THEN
    Unbuffer(f);
    IF ~f.registered THEN UnregisterByName(f.name);  H.FileRegister(f^) END
  END
END Register;


PROCEDURE Close*(f: File);
BEGIN IF f # NIL THEN Unbuffer(f) END END Close;


PROCEDURE Delete*(name: ARRAY OF CHAR; VAR res: INTEGER);
BEGIN IF H.FileDelete(name) THEN res := 0 ELSE res := 2 END END Delete;


PROCEDURE Rename*(old, new: ARRAY OF CHAR; VAR res: INTEGER);
BEGIN res := H.FileMove(old, new);
  IF res # 0 THEN res := 2 END
END Rename;


PROCEDURE Length*(f: File): INTEGER; RETURN f.len END Length;


PROCEDURE Date*(f: File): INTEGER; (* In 100 nanosecond ticks since 2000-01-01 UTC *)
RETURN H.FileTime(f^) END Date;


PROCEDURE Set*(VAR r: Rider; f: File; pos: INTEGER);
BEGIN
  r.eof  := FALSE;
  r.res  := 0;
  r.file := f;
  IF f # NIL THEN
    r.buf := f.firstbuf;
    IF    pos < 0     THEN r.pos := 0
    ELSIF pos < f.len THEN r.pos := pos
    ELSE                   r.pos := f.len
    END
  END;
END Set;


PROCEDURE Pos* (VAR r: Rider): INTEGER; RETURN r.pos  END Pos;
PROCEDURE Base*(VAR r: Rider): File;    RETURN r.file END Base;


(*---------------------------Read---------------------------*)

PROCEDURE ReadMem*(VAR r: Rider; adr, len: INTEGER);
VAR buf: Buffer;  page, offs, copylen: INTEGER;
BEGIN
  offs := r.pos MOD Bufsize;
  IF (r.pos DIV Bufsize = r.buf.page) & (offs + len <= r.buf.lim) THEN
    (* Fast case - all data in current buffer *)
    SYSTEM.COPY(SYSTEM.ADR(r.buf.data[offs]), adr, len);
    INC(r.pos, len)
  ELSE
    WHILE len > 0 DO
      page := r.pos DIV Bufsize;
      offs := r.pos MOD Bufsize;
      IF page = r.buf.page THEN
        IF offs < r.buf.lim THEN  (* At least the first byte to be read is in the current buffer *)
          copylen := r.buf.lim - offs;
          IF copylen > len THEN copylen := len END;
          SYSTEM.COPY(SYSTEM.ADR(r.buf.data[offs]), adr, copylen);
          INC(r.pos, copylen);  INC(adr, copylen);  DEC(len, copylen)
        ELSE (* at end of file *)
          ASSERT(r.pos = r.file.len);
          WHILE len > 0 DO
            SYSTEM.PUT(adr, SYSTEM.VAL(BYTE, 0));
            INC(adr);  DEC(len)
          END;
          r.eof := TRUE;
        END
      ELSE
        r.buf := GetBuf(r.file, page)
      END
    END
  END
END ReadMem;

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
BEGIN n := 0;  y := 0;
  ReadByte(r, b);
  WHILE b >= 80H DO  INC(y, LSL(b - 80H, n));  INC(n, 7);  ReadByte(r, b)  END;
  INC(y, LSL(b, n));
  IF n < 57 THEN y := ASR(LSL(y, 57-n), 57-n)  (* Sign extend *) END;
  x := y;
END ReadNum;


(*---------------------------Write---------------------------*)

PROCEDURE WriteMem*(VAR r: Rider; adr, len: INTEGER);
VAR f: File;  buf: Buffer;  page, offs, copylen: INTEGER;
BEGIN
  offs := r.pos MOD Bufsize;
  buf  := r.buf;
  IF (r.pos DIV Bufsize = buf.page) & (offs + len <= Bufsize) THEN
    (* Fast case, all data will go into the current buffer *)
    SYSTEM.COPY(adr, SYSTEM.ADR(buf.data[offs]), len);
    INC(r.pos, len);
    IF r.pos > r.file.len THEN
      r.file.len := r.pos;
      ASSERT(offs + len > buf.lim);
      buf.lim := offs + len
    END;
    buf.mod := TRUE
  ELSE
    WHILE len > 0 DO
      page := r.pos DIV Bufsize;
      offs := r.pos MOD Bufsize;
      buf  := r.buf;
      IF page = buf.page THEN
        IF offs > buf.lim THEN
          H.wn;
          H.ws("fn '");         H.ws(r.file.name);
          H.ws("', page ");     H.wh(page);
          H.ws("H, offs ");     H.wh(offs);
          H.ws("H, buf.lim ");  H.wh(buf.lim);
          H.ws("H, f.len ");    H.wh(r.file.len);
          H.wsn("H.");
        END;
        ASSERT(offs <= buf.lim);  (* r.pos must be within or at end of file *)
        copylen := Bufsize - offs;
        IF copylen > len THEN copylen := len END;
        SYSTEM.COPY(adr, SYSTEM.ADR(buf.data[offs]), copylen);
        buf.mod := TRUE;
        INC(r.pos, copylen);  INC(adr, copylen);  DEC(len, copylen);
        IF offs + copylen > buf.lim THEN buf.lim := offs + copylen END;
        IF r.pos > r.file.len THEN r.file.len := r.pos END;
      ELSE
        r.buf := GetBuf(r.file, page)
      END
    END
  END
END WriteMem;

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
  WHILE (x < -40H) OR (x >= 40H) DO WriteByte(r, x MOD 80H + 80H); x := ASR(x, 7) END;
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


(* Files.Mod Init is called by Modules.Init *)
PROCEDURE Init*(stackorg: INTEGER);
BEGIN
  first := NIL;
  H.SetDeleteUnregisteredFilesAtExit(DeleteUnregisteredFilesAtExit);
  Kernel.Init(stackorg);
END Init;

END Files.