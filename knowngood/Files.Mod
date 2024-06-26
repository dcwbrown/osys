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
  FileDesc = RECORD
    next:       POINTER- TO FileDesc;  (* Non-collecting list of open files *)
    name*:      FileName;
    registered: BOOLEAN;
    pos:        INTEGER;
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
VAR handle: INTEGER;  f: File;
BEGIN
  IF name[0] = 0X THEN
    f := NIL  (* E.g. commonly when Texts.Open passed "" as filename *)
  ELSE
    f := first;
    WHILE (f # NIL) & ~(f.registered & (f.name = name)) DO f := f.next END;
    IF f = NIL THEN
      handle := H.FileOld(name);
      IF handle >= 0 THEN
        NEW(f);  H.ZeroFill(f^);
        f.next       := first;
        f.name       := name;
        f.registered := TRUE;
        f.pos        := 0;
        f.handle     := handle;
        first        := f;
      END
    END
  END
RETURN f END Old;


PROCEDURE New*(name: ARRAY OF CHAR): File;
VAR f: File;
BEGIN
  NEW(f);  H.ZeroFill(f^);
  f.next       := first;
  f.name       := name;
  f.registered := FALSE;
  f.pos        := 0;
  f.handle     := H.FileNew();
  first := f;
RETURN f END New;


PROCEDURE Close*(f: File);
BEGIN IF f # NIL THEN H.FileFlush(f.handle) END
END Close;


PROCEDURE Unregister(f: File);
BEGIN H.FileUnregister(f.handle, f.name);  f.registered := FALSE
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
BEGIN
  IF (f # NIL) & (f.name[0] # 0X) & ~f.registered THEN
    UnregisterByName(f.name);
    H.FileRegister(f.handle, f.name);
    f.registered := TRUE;
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
RETURN H.FileLength(f.handle) END Length;


PROCEDURE Date*(f: File): INTEGER;  (* In 100 nanosecond ticks since 2000-01-01 UTC *)
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
BEGIN IF pos # f.pos THEN H.FileSetPos(f.handle, pos);  f.pos := pos END
END SetFilePos;

(* ---------------------------------- Read ---------------------------------- *)

PROCEDURE ZeroMem(adr, len: INTEGER);
VAR zero: INTEGER;
BEGIN
  WHILE len >= 8 DO SYSTEM.PUT(adr, 0);  INC(adr, 8);  DEC(len, 8) END;
  WHILE len > 0 DO SYSTEM.PUT(adr, 0X);  INC(adr);  DEC(len)  END;
END ZeroMem;


PROCEDURE ReadMem*(VAR r: Rider; adr, len: INTEGER);
VAR f: File; bytesread: INTEGER;
BEGIN
  IF len > 0 THEN
    f := r.file;  SetFilePos(f, r.pos);
    bytesread := H.FileRead(f.handle, adr, len);
    ASSERT(bytesread >= 0);
    INC(f.pos, bytesread);  INC(r.pos, bytesread);
    IF bytesread < len THEN
      r.eof := TRUE;  r.res := len - bytesread;
      ZeroMem(adr + bytesread, r.res)
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

PROCEDURE WriteMem*(VAR r: Rider; adr, len: INTEGER);
VAR res, byteswritten: INTEGER;  f: File;
BEGIN
  f := r.file;  SetFilePos(f, r.pos);
  H.FileWrite(f.handle, adr, len);
  INC(r.pos, len);  f.pos := r.pos
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

  PROCEDURE finalise(f: File);
  BEGIN IF f.registered THEN H.FileClose(f.handle) END
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
PROCEDURE Init*(stackorg: INTEGER);
BEGIN first := NIL;  Kernel.Init(stackorg); END Init;

END Files.
