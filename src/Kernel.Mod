MODULE Kernel;  (*NW/PR  11.4.86 / 27.12.95 / 4.2.2014*)

IMPORT SYSTEM, H := Host;

VAR allocated*: INTEGER;
  (* Every allocated block on the heap starts 64/tag, 64/unset  *)
  (* Every tag starts 64/size - rounded up to 64, 128, or 256*k *)
  (* Every free block starts 64/length, 64/-1, 64/nextfree      *)
  heapOrg*, heapLim*: INTEGER;
  list0, list1, list2, list3: INTEGER;  (*lists of free blocks of size n*512, 256, 128, 64 bytes*)


(* -------------------------- New: heap allocation -------------------------- *)

PROCEDURE GetBlock(VAR p: LONGINT;  len: LONGINT);
(* len is multiple of 256 *)
VAR q0, q1, q2, size: LONGINT; done: BOOLEAN;
BEGIN q0 := 0;  q1 := list0;  done := FALSE;
  ASSERT(len > 256);  ASSERT(len MOD 256 = 0);
    WHILE ~done & (q1 # 0) DO
    SYSTEM.GET(q1, size);
    SYSTEM.GET(q1 + 16, q2);  (* next *)
    IF    size < len THEN (* no fit *) q0 := q1;  q1 := q2
    ELSIF size = len THEN (* extract -> p *)
      done := TRUE;  p := q1;
      IF q0 # 0 THEN SYSTEM.PUT(q0 + 16, q2) ELSE list0 := q2 END
    ELSE (*reduce size*)
      done := TRUE;  p := q1;  q1 := q1 + len;
      SYSTEM.PUT(q1, size-len);  SYSTEM.PUT(q1 + 8,  -1);  SYSTEM.PUT(q1 + 16, q2);
      IF q0 # 0 THEN SYSTEM.PUT(q0 + 16, q1) ELSE list0 := q1 END
    END
  END;
  IF ~done THEN p := 0 END
END GetBlock;

PROCEDURE GetBlock256(VAR p: LONGINT);
VAR q: LONGINT;
BEGIN
  IF list1 # 0 THEN p := list1;  SYSTEM.GET(list1+16, list1)
  ELSE
    GetBlock(q, 512);
    IF q # 0 THEN
      SYSTEM.PUT(q + 256, 256);
      SYSTEM.PUT(q + 264, -1);
      SYSTEM.PUT(q + 272, list1);
      list1 := q + 256
    END;
    p := q
  END
END GetBlock256;

PROCEDURE GetBlock128(VAR p: LONGINT);
VAR q: LONGINT;
BEGIN
  IF list2 # 0 THEN p := list2;  SYSTEM.GET(list2 + 16, list2)
  ELSE
    GetBlock256(q);
    IF q # 0 THEN
      SYSTEM.PUT(q + 128, 128);
      SYSTEM.PUT(q + 136, -1);
      SYSTEM.PUT(q + 144, list2);
      list2 := q + 128
    END;
    p := q
  END
END GetBlock128;

PROCEDURE GetBlock64(VAR p: LONGINT);
VAR q: LONGINT;
BEGIN
  IF list3 # 0 THEN p := list3;  SYSTEM.GET(list3 + 16, list3)
  ELSE
    GetBlock128(q);
    IF q # 0 THEN
      SYSTEM.PUT(q + 64, 64);
      SYSTEM.PUT(q + 72, -1);
      SYSTEM.PUT(q + 80, list3);
      list3 := q + 64
    END;
    p := q
  END
END GetBlock64;

PROCEDURE New*(VAR ptr: LONGINT;  tag: LONGINT);
(* called by NEW via H.Handlers[H.NewProc];  ptr and tag are pointers*)
VAR p, size, lim: LONGINT;
BEGIN SYSTEM.GET(tag, size);
  (* Size includes header *)
  IF    size = 64  THEN GetBlock64(p)
  ELSIF size = 128 THEN GetBlock128(p)
  ELSIF size = 256 THEN GetBlock256(p)
  ELSE GetBlock(p, (size+511) DIV 512 * 512)
  END;
  IF p = 0 THEN ptr := 0
  ELSE
    ptr := p + 16;
    SYSTEM.PUT(p, tag);
    lim := p + size;
    INC(p, 8);
    INC(allocated, size);
    WHILE p < lim DO SYSTEM.PUT(p, 0);  INC(p, 8) END
  END;
END New;


(* --------------------------- Garbage collector ---------------------------- *)
(* ------------------------- Called from Modules.GC ------------------------- *)

PROCEDURE Mark*(pref: LONGINT);
VAR pvadr, offadr, offset, tag, p, q, r: LONGINT;
BEGIN
  SYSTEM.GET(pref, pvadr);  (* pointers < heapOrg considered NIL *)
  WHILE pvadr >= 0 DO
    IF pvadr < 100000000H THEN H.Trap(56, "Mark: pref -> too low address") END;
    SYSTEM.GET(pvadr, p);
    IF p >= heapOrg THEN
      SYSTEM.GET(p - 8, offadr);
      IF offadr <= 0 THEN q := p; (* mark elements in data structure with root p *)
        REPEAT SYSTEM.GET(p - 8, offadr);
          IF offadr <= 0 THEN
            SYSTEM.GET(p - 10H, tag);
            offadr := tag + 20H
          ELSE
            INC(offadr, 8)
          END;
          SYSTEM.PUT(p - 8, offadr);  SYSTEM.GET(offadr, offset);
          IF offset # -1 THEN (* down *)
            SYSTEM.GET(p + offset, r);
            IF r >= heapOrg THEN
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
VAR p, q, mark, tag, size: LONGINT;
BEGIN p := heapOrg;
  REPEAT SYSTEM.GET(p+8, mark);  q := p;
    WHILE mark = 0 DO
      SYSTEM.GET(p, tag);  SYSTEM.GET(tag, size);  INC(p, size);  SYSTEM.GET(p+8, mark)
    END;
    size := p - q;  DEC(allocated, size);  (*size of free block*)
    IF size > 0 THEN
      IF size MOD 128 # 0 THEN
        SYSTEM.PUT(q, 64);  SYSTEM.PUT(q+8, -1);  SYSTEM.PUT(q+16, list3);  list3 := q;  INC(q, 64);  DEC(size, 64)
      END;
      IF size MOD 256 # 0 THEN
        SYSTEM.PUT(q, 128);  SYSTEM.PUT(q+8, -1);  SYSTEM.PUT(q+16, list2);  list2 := q;  INC(q, 128);  DEC(size, 128)
      END;
      IF size MOD 512 # 0 THEN
        SYSTEM.PUT(q, 256);  SYSTEM.PUT(q+8, -1);  SYSTEM.PUT(q+16,  list1);  list1 := q;  INC(q, 256);  DEC(size, 256)
      END;
      IF size > 0 THEN
        SYSTEM.PUT(q, size);  SYSTEM.PUT(q+8, -1);  SYSTEM.PUT(q+16, list0);  list0 := q;  INC(q, size)
      END
    END;
    IF mark > 0 THEN
      SYSTEM.GET(p, tag);  SYSTEM.GET(tag, size);  SYSTEM.PUT(p+8, 0);  INC(p, size)
    ELSE (*free*) SYSTEM.GET(p, size);  INC(p, size)
    END
  UNTIL p >= heapLim
END Scan;


(*-------- Miscellaneous procedures----------*)

PROCEDURE Time*(): INTEGER;
(* In milliseconds since 2000-01-01 UTC *)
BEGIN RETURN H.Ticks() DIV 10000 END Time;


PROCEDURE Clock*(): INTEGER;
(* Returns local time as 38/year-2000,4/month,5/day,5/hour,6/min,6/sec *)
BEGIN RETURN H.Clock() END Clock;

(*
PROCEDURE SetClock*(dt: INTEGER);
BEGIN END SetClock;
*)

(* -------------------------------------------------------------------------- *)

PROCEDURE Init*(stackorg: INTEGER);  (* Called from Files.Init *)
CONST
  reserve = 80000000H;  (* Reserve 2GB contiguous *)
  commit  = 2000000H;   (* Initially commit 32MB  *)
BEGIN
  H.Init(stackorg);
  heapOrg := H.HeapInit(reserve, commit);

  heapLim := heapOrg + commit;
  list1 := 0;  list2 := 0;  list3 := 0;  list0 := heapOrg;
  SYSTEM.PUT(list0, heapLim - heapOrg);
  SYSTEM.PUT(list0 + 8, -1);
  SYSTEM.PUT(list0 + 16, 0);
  allocated := 0;

  SYSTEM.PUT(SYSTEM.ADR(H.Handlers[H.NewProc]), SYSTEM.ADR(New));
END Init;

END Kernel.
