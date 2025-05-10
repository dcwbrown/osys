MODULE Testregalloc;  (* DCWB 2024-10-06 *)

IMPORT OCB;

TYPE
  R = RECORD x: INTEGER;  a: ARRAY 16 OF INTEGER END;
  P = POINTER TO R;

VAR
  i, j: INTEGER;  r: R;
  proc: PROCEDURE(i: INTEGER);

PROCEDURE a(p: INTEGER);
VAR
  l: INTEGER;
  r: R;
  a: ARRAY 5 OF INTEGER;
BEGIN
  l := p + 1;
  l := r.x;
  l := a[l]
END a;

PROCEDURE t(i: INTEGER): INTEGER;
RETURN i DIV 3 END t;

PROCEDURE p(x, y: INTEGER;  r: R;  s: ARRAY OF CHAR);
VAR k, l: INTEGER;
BEGIN
  i := 1;
  j := i + 2;
  k := j + (-1);
  k := j * (-1);
  IF (i = 0) & (j = 1) THEN
    i := 2
  END;
  IF i = 1 THEN
    j := (i + j) - (k + l)
  END;
  IF (i = 1) & (j = 2) OR (j = 3) THEN i := 0 END;
  k := j DIV 3;
  l := i DIV j + k DIV l;
  a(i+1);
  a(1);
  a(i);
  a(i DIV j);
  INC(i);
  k := t(x);
  k := OCB.WordSize;
END p;

BEGIN
  p(1, 2, r, "Hi");
  proc(5);
END Testregalloc.

ORP.Compile Testregalloc.Mod/s ~

