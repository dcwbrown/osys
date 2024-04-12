MODULE Kernel;  (*NW/PR  11.4.86 / 27.12.95 / 4.2.2014*)

IMPORT SYSTEM, H := WinHost;

VAR
  (* Every allocated block on the heap starts 64/tag, 64/unset  *)
  (* Every tag starts 64/size - rounded up to 64, 128, or 256*k *)
  (* Every free block starts 64/length, 64/-1, 64/nextfree      *)
  HeapOrg*:   INTEGER;
  HeapLimit*: INTEGER;
  List0:      INTEGER;  (* List of large free blocks (>= 512 bytes *)
  List1:      INTEGER;  (* List of 256 byte free blocks *)
  List2:      INTEGER;  (* List of 128 byte free blocks *)
  List3:      INTEGER;  (* List of  64 byte free blocks *)
  Allocated*: INTEGER;


(* -------------------------- New: heap allocation -------------------------- *)

PROCEDURE GetBlock(VAR p: INTEGER;  len: INTEGER);  (* len is multiple of 256 *)
VAR
  q0, q1, q2, size: INTEGER;
  done: BOOLEAN;
BEGIN
  ASSERT(len > 256);  ASSERT(len MOD 256 = 0);
  q0   := 0;
  q1   := List0;
  done := FALSE;
  WHILE ~done & (q1 # 0) DO
    SYSTEM.GET(q1, size);
    SYSTEM.GET(q1 + 16, q2);  (* next *)
    IF    size < len THEN (* no fit *) q0 := q1;  q1 := q2
    ELSIF size = len THEN (* extract -> p *)
      done := TRUE;
      p    := q1;
      IF q0 # 0 THEN SYSTEM.PUT(q0 + 16, q2) ELSE List0 := q2 END
    ELSE (*reduce size*)
      done := TRUE;
      p    := q1;
      q1   := q1 + len;
      SYSTEM.PUT(q1, size-len);
      SYSTEM.PUT(q1 + 8,  -1);
      SYSTEM.PUT(q1 + 16, q2);
      IF q0 # 0 THEN SYSTEM.PUT(q0 + 16, q1) ELSE List0 := q1 END
    END
  END;
  IF ~done THEN p := 0 END
END GetBlock;

PROCEDURE GetBlock256(VAR p: INTEGER);
VAR q: INTEGER;
BEGIN
  IF List1 # 0 THEN p := List1;  SYSTEM.GET(List1+16, List1)
  ELSE
    GetBlock(q, 512);
    IF q # 0 THEN
      SYSTEM.PUT(q + 256, 256);
      SYSTEM.PUT(q + 264, -1);
      SYSTEM.PUT(q + 272, List1);
      List1 := q + 256
    END;
    p := q
  END
END GetBlock256;

PROCEDURE GetBlock128(VAR p: INTEGER);
VAR q: INTEGER;
BEGIN
  IF List2 # 0 THEN p := List2;  SYSTEM.GET(List2 + 16, List2)
  ELSE
    GetBlock256(q);
    IF q # 0 THEN
      SYSTEM.PUT(q + 128, 128);
      SYSTEM.PUT(q + 136, -1);
      SYSTEM.PUT(q + 144, List2);
      List2 := q + 128
    END;
    p := q
  END
END GetBlock128;

PROCEDURE GetBlock64(VAR p: INTEGER);
VAR q: INTEGER;
BEGIN
  IF List3 # 0 THEN p := List3;  SYSTEM.GET(List3 + 16, List3)
  ELSE
    GetBlock128(q);
    IF q # 0 THEN
      SYSTEM.PUT(q + 64, 64);
      SYSTEM.PUT(q + 72, -1);
      SYSTEM.PUT(q + 80, List3);
      List3 := q + 64
    END;
    p := q
  END
END GetBlock64;

PROCEDURE New*(VAR ptr: INTEGER;  tag: INTEGER);
(* called by NEW via H.NewPointer;  ptr and tag are pointers*)
VAR p, size, lim: INTEGER;
BEGIN
  (*
  H.ws("NewPtr. ptr @");  H.wh(SYSTEM.ADR(ptr));
  H.ws("H, tag ");  H.wh(tag);  H.wc("H");
  H.WriteModuleOffset(tag);
  *)
  SYSTEM.GET(tag, size);  (* Size includes header *)
  (*
  H.ws(", tag.size "); H.wh(size);
  H.ws("H.");
  *)
  IF    size = 64  THEN GetBlock64(p)
  ELSIF size = 128 THEN GetBlock128(p)
  ELSIF size = 256 THEN GetBlock256(p)
  ELSE GetBlock(p, (size+255) DIV 256 * 256)
  END;
  IF p = 0 THEN ptr := 0
  ELSE
    ptr := p + 16;
    SYSTEM.PUT(p, tag);
    lim := p + size;
    INC(p, 8);
    INC(Allocated, size);
    WHILE p < lim DO SYSTEM.PUT(p, 0);  INC(p, 8) END
  END;
  (*H.ws(" Allocated at "); H.wh(ptr); H.wsn("H.");*)
END New;


(* --------------------------- Garbage collector ---------------------------- *)
(* ------------------------- Called from Oberon.GC -------------------------- *)

PROCEDURE Mark*(pref: INTEGER);
VAR pvadr, offadr, offset, tag, p, q, r: INTEGER;
BEGIN
  SYSTEM.GET(pref, pvadr);  (* pointers < HeapOrg considered NIL *)
  WHILE pvadr >= 0 DO
    SYSTEM.GET(pvadr, p);
    (*
    H.ws("    pref ");   H.wh(pref);
    H.ws("H -> pvadr "); H.wh(pvadr);
    H.ws("H -> p ");     H.wh(p);     H.wsn("H.");
    *)
    IF p >= HeapOrg THEN
      SYSTEM.GET(p - 8, offadr);
      IF offadr <= 0 THEN (* mark elements in data structure with root p *)
        q := p;
        REPEAT
          SYSTEM.GET(p - 8, offadr);
          (*H.ws("Visit "); H.wh(p); H.ws("H, p.offadr "); H.wh(offadr); H.wc("H");*)
          IF offadr <= 0 THEN
            SYSTEM.GET(p - 10H, tag);
            (*H.ws(", p.tag '"); WriteTypeName(tag); H.ws("'");*)
            offadr := tag + 20H
          ELSE
            INC(offadr, 8)
          END;
          SYSTEM.PUT(p - 8, offadr);
          (*H.ws("Mark "); H.wh(p); H.ws("H with "); H.wh(offadr); H.wsn("H.");*)
          SYSTEM.GET(offadr, offset);
          (*H.ws(", offadr' "); H.wh(offadr); H.ws("H -> offset "); H.wh(offset); H.wsn("H.");*)
          IF offset # -1 THEN (* down *)
            SYSTEM.GET(p + offset, r);
            IF r >= HeapOrg THEN
              SYSTEM.GET(r - 8, offadr);
              IF offadr <= 0 THEN
                SYSTEM.PUT(p + offset, q);  q := p;  p := r
              END
            END
          ELSE (*up*)
            SYSTEM.GET(q - 8, offadr);
            SYSTEM.GET(offadr, offset);
            IF p # q THEN
              SYSTEM.GET(q + offset, r);
              SYSTEM.PUT(q + offset, p);  p := q;  q := r
            END
          END
        UNTIL (p = q) & (offset = -1)
      END
    END;
    INC(pref, 8);  SYSTEM.GET(pref, pvadr)
  END
END Mark;


PROCEDURE Scan*;
VAR p, q, mark, tag, size: INTEGER;
BEGIN
  (*H.ws("Kernel.Scan starting, Allocated = "); H.wh(Allocated); H.wsn("H.");*)
  p := HeapOrg;
  REPEAT SYSTEM.GET(p+8, mark);  q := p;
    WHILE mark = 0 DO
      SYSTEM.GET(p, tag);  SYSTEM.GET(tag, size);  INC(p, size);  SYSTEM.GET(p+8, mark)
    END;
    size := p - q;  DEC(Allocated, size);  (*size of free block*)
    IF size > 0 THEN
      (*H.ws("Recycled "); H.wh(size); H.wsn("H bytes.");*)
      IF size MOD 128 # 0 THEN
        SYSTEM.PUT(q, 64);  SYSTEM.PUT(q+8, -1);  SYSTEM.PUT(q+16, List3);  List3 := q;  INC(q, 64);  DEC(size, 64)
      END;
      IF size MOD 256 # 0 THEN
        SYSTEM.PUT(q, 128);  SYSTEM.PUT(q+8, -1);  SYSTEM.PUT(q+16, List2);  List2 := q;  INC(q, 128);  DEC(size, 128)
      END;
      IF size MOD 512 # 0 THEN
        SYSTEM.PUT(q, 256);  SYSTEM.PUT(q+8, -1);  SYSTEM.PUT(q+16,  List1);  List1 := q;  INC(q, 256);  DEC(size, 256)
      END;
      IF size > 0 THEN
        SYSTEM.PUT(q, size);  SYSTEM.PUT(q+8, -1);  SYSTEM.PUT(q+16, List0);  List0 := q;  INC(q, size)
      END
    END;
    IF mark > 0 THEN
      SYSTEM.GET(p, tag);  SYSTEM.GET(tag, size);  SYSTEM.PUT(p+8, 0);  INC(p, size)
    ELSE (*free*)
      SYSTEM.GET(p, size);  INC(p, size)
    END
  UNTIL p >= HeapLimit;
  (*H.ws("Kernel.Scan complete, Allocated = "); H.wh(Allocated); H.wsn("H.");*)
END Scan;


PROCEDURE Halt*(exitCode: INTEGER);
BEGIN (*Heap.Finalise;*)  H.ExitProcess(exitCode) END Halt;


(* ------------------------ Miscellaneous procedures ------------------------ *)

PROCEDURE Time*(): INTEGER;   (* In milliseconds since system-dependent origin *)
BEGIN RETURN H.Time() DIV 10000 END Time;

PROCEDURE Clock*(): INTEGER;  (* 32 bit packed date and time *)
BEGIN RETURN H.Clock() END Clock;

PROCEDURE SetClock*(dt: INTEGER);
BEGIN END SetClock;


(* -------------------------------------------------------------------------- *)

PROCEDURE Init*;
CONST
  MEMRESERVE = 2000H;  MEMCOMMIT = 1000H;  PAGEREADWRITE = 4;
  reserve = 80000000H;  (* Reserve 2GB contiguous *)
  commit  = 2000000H;   (* Initially commit 32MB  *)
BEGIN
  (*
  H.wsn("Kernel initialising.");
  H.ws("H.VirtualAlloc at ");  H.wh(SYSTEM.VAL(INTEGER, H.VirtualAlloc));  H.wsn("H.");
  *)

  (* Initialize heap *)
  HeapOrg := H.VirtualAlloc(80000000H, reserve, MEMRESERVE, PAGEREADWRITE);
  (*H.ws("Virtual alloc reservation returned ");  H.wh(HeapOrg);  H.wsn("H.");*)
  ASSERT(HeapOrg # 0);
  HeapOrg := H.VirtualAlloc(HeapOrg, commit, MEMCOMMIT, PAGEREADWRITE);
  (*H.ws("Virtual alloc commit returned ");  H.wh(HeapOrg);  H.wsn("H.");*)
  ASSERT(HeapOrg # 0);

  HeapLimit := HeapOrg + commit;
  List1 := 0;
  List2 := 0;
  List3 := 0;
  List0 := HeapOrg;
  SYSTEM.PUT(List0, HeapLimit - HeapOrg);
  SYSTEM.PUT(List0 + 8, -1);
  SYSTEM.PUT(List0 + 16, 0);
  Allocated := 0;

  SYSTEM.PUT(SYSTEM.ADR(H.Handlers[H.NewProc]), SYSTEM.ADR(New));

  IF H.Verbose IN H.LoadFlags THEN
    H.ws("* Heap allocated at ");  H.wh(HeapOrg);
    H.ws("H, commited to ");       H.wh(HeapOrg + commit);
    H.ws("H, reserved to ");       H.wh(HeapOrg + reserve);  H.wsn("H.")
  END;
END Init;

BEGIN Init END Kernel.
