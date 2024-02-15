MODULE Kernel;  (*NW/PR  11.4.86 / 27.12.95 / 4.2.2014*)

IMPORT SYSTEM, P := Winshim;

VAR
  HeapOrg*:   INTEGER;
  HeapLimit*: INTEGER;
  List0:      INTEGER;  (* List of large free blocks (>= 512 bytes *)
  List1:      INTEGER;  (* List of 256 byte free blocks *)
  List2:      INTEGER;  (* List of 128 byte free blocks *)
  List3:      INTEGER;  (* List of  64 byte free blocks *)
  Allocated:  INTEGER;

  (*
  stackOrg*,  stackSize*, MemLim*: INTEGER;
  clock: INTEGER;
  *)


(* -------------------------- New: heap allocation -------------------------- *)


PROCEDURE GetBlock(VAR p: INTEGER;  len: INTEGER);  (* len is multiple of 256 *)
VAR
  q0, q1, q2, size: INTEGER;
  done: BOOLEAN;
BEGIN
  ASSERT(len > 256);
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
    SYSTEM.PUT(q + 256, 256);
    SYSTEM.PUT(q + 264, -1);
    SYSTEM.PUT(q + 272, List1);
    List1 := q + 256;  p := q
  END
END GetBlock256;

PROCEDURE GetBlock128(VAR p: INTEGER);
VAR q: INTEGER;
BEGIN
  IF List2 # 0 THEN p := List2;  SYSTEM.GET(List2 + 16, List2)
  ELSE
    GetBlock256(q);
    SYSTEM.PUT(q + 128, 128);
    SYSTEM.PUT(q + 136, -1);
    SYSTEM.PUT(q + 144, List2);
    List2 := q + 128;  p := q
  END
END GetBlock128;

PROCEDURE GetBlock64(VAR p: INTEGER);
VAR q: INTEGER;
BEGIN
  IF List3 # 0 THEN p := List3;  SYSTEM.GET(List3 + 16, List3)
  ELSE
    GetBlock128(q);
    SYSTEM.PUT(q + 64, 64);
    SYSTEM.PUT(q + 72, -1);
    SYSTEM.PUT(q + 80, List3);
    List3 := q + 64;  p := q
  END
END GetBlock64;

PROCEDURE New*(VAR ptr: INTEGER;  tag: INTEGER);
(* called by NEW via P.NewPointer;  ptr and tag are pointers*)
VAR p, size, lim: INTEGER;
BEGIN
  (*
  P.ws("NewPtr. ptr @");  P.wh(SYSTEM.ADR(ptr));
  P.ws("H, tag ");  P.wh(tag);  P.wc("H");
  P.WriteModuleOffset(tag);
  *)
  SYSTEM.GET(tag, size);  (* Size includes header *)
  (*
  P.ws(", tag.size "); P.wh(size);
  P.ws("H.");
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
  (*P.ws(" Allocated at "); P.wh(ptr); P.wsl("H.");*)
END New;


(* ---------------------------- Garbage collector ----------------------------*)

(*
PROCEDURE Mark*(pref: INTEGER);
VAR pvadr, offadr, offset, tag, p, q, r: INTEGER;
BEGIN
  SYSTEM.GET(pref, pvadr);  (* pointers < HeapOrg considered NIL *)
  WHILE pvadr # 0 DO
    SYSTEM.GET(pvadr, p);
    SYSTEM.GET(p-4, offadr);
    IF (p >= HeapOrg) & (offadr = 0) THEN (* mark elements in data structure with root p *)
      q := p;
      REPEAT
        SYSTEM.GET(p-4, offadr);
        IF offadr = 0 THEN SYSTEM.GET(p-8, tag);  offadr := tag + 16 ELSE INC(offadr, 4) END;
        SYSTEM.PUT(p-4, offadr);
        SYSTEM.GET(offadr, offset);
        IF offset # -1 THEN (* down *)
          SYSTEM.GET(p+offset, r);
          SYSTEM.GET(r-4, offadr);
          IF (r >= HeapOrg) & (offadr = 0) THEN SYSTEM.PUT(p+offset, q);  q := p;  p := r END
        ELSE (*up*)
          SYSTEM.GET(q-4, offadr);
          SYSTEM.GET(offadr, offset);
          IF p # q THEN SYSTEM.GET(q+offset, r);  SYSTEM.PUT(q+offset, p);  p := q;  q := r END
        END
      UNTIL (p = q) & (offset = -1)
    END;
    INC(pref, 4);  SYSTEM.GET(pref, pvadr)
  END
END Mark;

PROCEDURE Scan*;
VAR p, q, mark, tag, size: INTEGER;
BEGIN p := HeapOrg;
  REPEAT SYSTEM.GET(p+4, mark);  q := p;
    WHILE mark = 0 DO
      SYSTEM.GET(p, tag);  SYSTEM.GET(tag, size);  INC(p, size);  SYSTEM.GET(p+4, mark)
    END;
    size := p - q;  DEC(Allocated, size);  (*size of free block*)
    IF size > 0 THEN
      IF size MOD 64 # 0 THEN
        SYSTEM.PUT(q, 32);  SYSTEM.PUT(q+4, -1);  SYSTEM.PUT(q+8, List3);  List3 := q;  INC(q, 32);  DEC(size, 32)
      END;
      IF size MOD 128 # 0 THEN
        SYSTEM.PUT(q, 64);  SYSTEM.PUT(q+4, -1);  SYSTEM.PUT(q+8, List2);  List2 := q;  INC(q, 64);  DEC(size, 64)
      END;
      IF size MOD 256 # 0 THEN
        SYSTEM.PUT(q, 128);  SYSTEM.PUT(q+4, -1);  SYSTEM.PUT(q+8,  List1);  List1 := q;  INC(q, 128);  DEC(size, 128)
      END;
      IF size > 0 THEN
        SYSTEM.PUT(q, size);  SYSTEM.PUT(q+4, -1);  SYSTEM.PUT(q+8, List0);  List0 := q;  INC(q, size)
      END
    END;
    IF mark > 0 THEN SYSTEM.GET(p, tag);  SYSTEM.GET(tag, size);  SYSTEM.PUT(p+4, 0);  INC(p, size)
    ELSE (*free*) SYSTEM.GET(p, size);  INC(p, size)
    END
  UNTIL p >= HeapLimit
END Scan;
*)


(* ------------------------ Miscellaneous procedures ------------------------ *)

PROCEDURE Ticks*(): INTEGER;  (* In 100ns ticks *)
VAR res, tick: INTEGER;
BEGIN res := P.GetSystemTimePreciseAsFileTime(SYSTEM.ADR(tick));
RETURN tick END Ticks;

PROCEDURE Time*(): INTEGER;   (* In milliseconds *)
RETURN Ticks() DIV 10000 END Time;

(* ----- Temporarily expose functions that belong in the platform shim ------ *)

PROCEDURE Append*(s: ARRAY OF CHAR; VAR d: ARRAY OF CHAR);
BEGIN P.Append(s, d) END Append;

PROCEDURE Halt*(exitCode: INTEGER);
BEGIN (*Heap.Finalise;*)  P.ExitProcess(exitCode) END Halt;


(*
PROCEDURE Clock*(): INTEGER;  (* Packed date and time *)
BEGIN RETURN clock
END Clock;

PROCEDURE SetClock*(dt: INTEGER);
BEGIN clock := dt
END SetClock;
*)


(* -------------------------------------------------------------------------- *)

PROCEDURE Init*;
CONST
  MEMRESERVE = 2000H;  MEMCOMMIT = 1000H;  PAGEREADWRITE = 4;
  reserve = 80000000H;  (* Reserve 2GB contiguous *)
  commit  = 2000000H;   (* Initially commit 32MB  *)
BEGIN
  (*
  P.wsl("Kernel initialising.");
  P.ws("P.VirtualAlloc at ");  P.wh(SYSTEM.VAL(INTEGER, P.VirtualAlloc));  P.wsl("H.");
  *)

  (* Initialize heap *)
  HeapOrg := P.VirtualAlloc(80000000H, reserve, MEMRESERVE, PAGEREADWRITE);
  (*P.ws("Virtual alloc reservation returned ");  P.wh(HeapOrg);  P.wsl("H.");*)
  ASSERT(HeapOrg # 0);
  HeapOrg := P.VirtualAlloc(HeapOrg, commit, MEMCOMMIT, PAGEREADWRITE);
  (*P.ws("Virtual alloc commit returned ");  P.wh(HeapOrg);  P.wsl("H.");*)
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

  SYSTEM.PUT(SYSTEM.ADR(P.NewPointer), SYSTEM.ADR(New));

  P.ws("Heap allocated at "); P.wh(HeapOrg);
  P.ws("H, commited to ");    P.wh(HeapOrg + commit);
  P.ws("H, reserved to ");    P.wh(HeapOrg + reserve);  P.wsl("H.");
END Init;

BEGIN Init END Kernel.