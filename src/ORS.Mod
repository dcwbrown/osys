MODULE ORS; (* NW 19.9.93 / 15.3.2017  Scanner in Oberon-07*)

IMPORT SYSTEM, H := Host, Files, Texts;

(* Oberon Scanner does lexical analysis. Input is Oberon-Text, output is
  sequence of symbols, i.e identifiers, numbers, strings, and special symbols.
  Recognises all Oberon keywords and skips comments. The keywords are
  recorded in a table.
  Get(sym) delivers next symbol from input text with Reader R.
  Mark(msg) records error and delivers error message with Writer W.
  If Get delivers ident, then the identifier (a string) is in variable id, if int or char
  in ival, if real in rval, and if string in str (and slen) *)
  
CONST IdLen* = 32;
  NKW = 34;  (* nof keywords *)
  maxExp = 38;  stringBufSize = 4096;

  (* lexical symbols *)
  null    = 0;  times*     = 1;  rdiv*      = 2;  div*    = 3;  mod*    = 4;
  and*    = 5;  plus*      = 6;  minus*     = 7;  or*     = 8;  eql*    = 9;
  neq*    = 10; lss*       = 11; leq*       = 12; gtr*    = 13; geq*    = 14;
  in*     = 15; is*        = 16; arrow*     = 17; period* = 18; bslash* = 19;
  char*   = 20; int*       = 21; real*      = 22; false*  = 23; true*   = 24;
  nil*    = 25; string*    = 26; not*       = 27; lparen* = 28; lbrak*  = 29;
  lbrace* = 30; ident*     = 31;
  if*     = 32; while*     = 34; repeat*    = 35; case*   = 36; for*    = 37;
  comma*  = 40; colon*     = 41; becomes*   = 42; upto*   = 43; rparen* = 44;
  rbrak*  = 45; rbrace*    = 46; then*      = 47; of*     = 48; do*     = 49;
  to*     = 50; by*        = 51; semicolon* = 52; end*    = 53; bar*    = 54;
  else*   = 55; elsif*     = 56; until*     = 57; return* = 58;
  array*  = 60; record*    = 61; pointer*   = 62; const*  = 63; type*   = 64;
  var*    = 65; procedure* = 66; begin*     = 67; import* = 68; module* = 69;  eot = 70;

TYPE Ident* = ARRAY IdLen OF CHAR;

VAR
  ival*, slen*: LONGINT;  (* results of Get *)
  rval*:        REAL;
  id*:          Ident;    (* for identifiers *)
  str*:         ARRAY stringBufSize OF CHAR;
  errcnt*:      INTEGER;
  W:            Texts.Writer;

  ch:       CHAR;      (* last character read *)
  errpos:   LONGINT;
  linebeg*: INTEGER;   (* Position at start of current line *)
  linenum*: INTEGER;   (* Line number of current line *)
  R:        Texts.Reader;
  k:        INTEGER;
  KWX:      ARRAY 10 OF INTEGER;
  keyTab:   ARRAY NKW OF 
              RECORD sym: INTEGER; id: ARRAY 12 OF CHAR END;

  (* Compilation options *)
  ListCodegen*:  BOOLEAN;  (* 'lc+' *)
  ListAssembly*: BOOLEAN;  (* 'la+' *)
  Hasty*:        BOOLEAN;  (* 'h+': Disables index range and pointer nil checks *)


PROCEDURE Pos*(): INTEGER;
BEGIN RETURN Texts.Pos(R) - 1
END Pos;

PROCEDURE GetCh;
VAR next: CHAR;
BEGIN
  Texts.Read(R, next);
  IF (ch = 0DX) OR (ch = 0AX) THEN
    IF (ch = 0DX) & (next = 0AX) THEN Texts.Read(R, next) END;
    INC(linenum);
    linebeg := Pos()
  END;
  ch := next
END GetCh;

PROCEDURE Mark*(msg: ARRAY OF CHAR);
VAR p: LONGINT;
BEGIN p := Pos();
  IF (p > errpos) & (errcnt < 25) THEN
    Texts.WriteLn(W);
    Texts.WriteString(W, "  [");    Texts.WriteInt(W, linenum, 1);
    Texts.WriteString(W, ":");      Texts.WriteInt(W, p - linebeg, 1);
    Texts.WriteString(W, "] pos "); Texts.WriteInt(W, p, 1);
    Texts.Write(W, " ");            Texts.WriteString(W, msg);
    Texts.Append(Texts.Log, W.buf)
  END;
  INC(errcnt); errpos := p + 4
END Mark;

PROCEDURE Identifier(VAR sym: INTEGER);
VAR i, k: INTEGER;
BEGIN i := 0;
  REPEAT
    IF i < IdLen-1 THEN id[i] := ch; INC(i) END;  
    GetCh
  UNTIL (ch < "0") OR (ch > "9") & (ch < "A") OR (ch > "Z") & (ch < "a") OR (ch > "z");
  id[i] := 0X;
  IF i < 10 THEN k := KWX[i-1];  (*search for keyword*)
    WHILE (id # keyTab[k].id) & (k < KWX[i]) DO INC(k) END;
    IF k < KWX[i] THEN sym := keyTab[k].sym ELSE sym := ident END
  ELSE sym := ident
  END
END Identifier;

PROCEDURE String;
  VAR i: INTEGER;
BEGIN i := 0; GetCh;
  WHILE ~R.eot & (ch # 22X) DO
    IF ch >= " " THEN
      IF i < stringBufSize-1 THEN str[i] := ch; INC(i) ELSE Mark("string too long") END;
    END;
    GetCh
  END;
  str[i] := 0X; INC(i); GetCh; slen := i
END String;

PROCEDURE GetHexChar(): INTEGER;
VAR val: INTEGER;
BEGIN
  IF    (ch >= "0") & (ch <= "9") THEN val := ORD(ch) - 30H; GetCh
  ELSIF (ch >= "A") & (ch <= "F") THEN val := ORD(ch) - 37H; GetCh
  ELSE  val := -1
  END
RETURN val END GetHexChar;

PROCEDURE HexString;
  VAR i, m, n: INTEGER;
BEGIN i := 0; GetCh;
  WHILE ~R.eot  & (ch <= " ") DO GetCh END;  (* skip whitespace *)
  WHILE ~R.eot & (ch # "$") DO
    m := GetHexChar();  n := GetHexChar();
    IF n >= 0 THEN m := m * 16 + n END;
    IF m < 0 THEN
      Mark("hexdig expected"); IF ch # "$" THEN GetCh END
    ELSE
      IF i < stringBufSize THEN
        str[i] := CHR(m); INC(i)
      ELSE
        Mark("string too long")
      END
    END;
    WHILE ~R.eot  & (ch <= " ") DO GetCh END  (* skip whitespace *)
  END;
  GetCh; slen := i  (* No 0X appended! *)
END HexString;

(*---
PROCEDURE Ten(e: LONGINT): REAL;
  VAR x, t: REAL;
BEGIN x := 1.0; t := 10.0;
  WHILE e > 0 DO
    IF ODD(e) THEN x := t * x END;
    t := t * t; e := e DIV 2
  END;
  RETURN x
END Ten;
---*)

PROCEDURE Number(VAR sym: INTEGER);
  CONST max = 7FFFFFFFFFFFFFFFH  (* 2^63 - 1 *)  (* was 2147483647 (*2^31 - 1*)*);
  VAR i, k, e, n, s, h: LONGINT; (*--- x: REAL; ---*)
    d: ARRAY 32 OF INTEGER;
    negE: BOOLEAN;
BEGIN ival := 0; i := 0; n := 0; k := 0;
  REPEAT
    IF n < LEN(d) THEN d[n] := ORD(ch)-30H; INC(n) ELSE Mark("too many digits"); n := 0 END;
    GetCh
  UNTIL (ch < "0") OR (ch > "9") & (ch < "A") OR (ch > "F");
  IF (ch = "H") OR (ch = "R") OR (ch = "X") THEN  (*hex*)
    REPEAT h := d[i];
      IF h >= 10 THEN h := h-7 END;
      k := k*10H + h; INC(i) (*no overflow check*)
    UNTIL i = n;
    IF ch = "X" THEN sym := char;
      IF k < 100H THEN ival := k ELSE Mark("illegal value"); ival := 0 END
    ELSIF ch = "R" THEN sym := real; rval := SYSTEM.VAL(REAL, k)
    ELSE sym := int; ival := k
    END;
    GetCh
  ELSIF ch = "." THEN
    GetCh;
    IF ch = "." THEN (*double dot*) ch := 7FX;  (*decimal integer*)
      REPEAT
        IF d[i] < 10 THEN
          IF k <= (max-d[i]) DIV 10 THEN k := k *10 + d[i] ELSE Mark("too large"); k := 0 END
        ELSE Mark("bad integer")
        END;
        INC(i)
      UNTIL i = n;
      sym := int; ival := k
    ELSE
      Mark("real not implemented"); sym := int; ival := 0
      (*---
      (*real number*)
      x := 0.0; e := 0;
      REPEAT  (*integer part*) x := x * 10.0 + FLT(d[i]); INC(i) UNTIL i = n;
      WHILE (ch >= "0") & (ch <= "9") DO  (*fraction*)
        x := x * 10.0 + FLT(ORD(ch) - 30H); DEC(e); GetCh
      END;
      IF (ch = "E") OR (ch = "D") THEN  (*scale factor*)
        GetCh; s := 0;
        IF ch = "-" THEN negE := TRUE; GetCh
        ELSE negE := FALSE;
          IF ch = "+" THEN GetCh END
        END;
        IF (ch >= "0") & (ch <= "9") THEN
          REPEAT s := s*10 + ORD(ch)-30H; GetCh
          UNTIL (ch < "0") OR (ch >"9");
          IF negE THEN e := e-s ELSE e := e+s END
        ELSE Mark("digit?")
        END
      END;
      IF e < 0 THEN
        IF e >= -maxExp THEN x := x / Ten(-e) ELSE x := 0.0 END
      ELSIF e > 0 THEN
        IF e <= maxExp THEN x := Ten(e) * x ELSE x := 0.0; Mark("too large") END
      END;
      sym := real; rval := x
      ---*)
    END
  ELSE  (*decimal integer*)
    REPEAT
      IF d[i] < 10 THEN
        IF k <= (max-d[i]) DIV 10 THEN k := k*10 + d[i] ELSE Mark("too large"); k := 0 END
      ELSE Mark("bad integer")
      END;
      INC(i)
    UNTIL i = n;
    sym := int; ival := k
  END
END Number;

PROCEDURE comment;
BEGIN GetCh;
  IF ch = "$" THEN GetCh;
    WHILE (ch = "l") OR (ch = "h") DO
      IF ch = "l" THEN GetCh;
        IF ch = "a" THEN GetCh;
          IF (ch = "-") OR (ch = "+") THEN ListAssembly := ch = "+"; GetCh END
        ELSIF ch = "c" THEN GetCh;
          IF (ch = "-") OR (ch = "+") THEN ListCodegen := ch = "+"; GetCh END
        END
      ELSIF ch = "h" THEN GetCh;
        IF (ch = "-") OR (ch = "+") THEN Hasty := ch = "+"; GetCh END
      END
    END
  END;
  REPEAT
    WHILE ~R.eot & (ch # "*") DO
      IF ch = "(" THEN GetCh;
        IF ch = "*" THEN comment END
      ELSE GetCh
      END
    END;
    WHILE ch = "*" DO GetCh END
  UNTIL (ch = ")") OR R.eot;
  IF ~R.eot THEN GetCh ELSE Mark("unterminated comment") END
END comment;


(*       ASCII == first 128 characters of Unicode:       *)
(*                                                       *)
(*            0 1 2 3 4 5 6 7 8 9 a b c d e f            *)
(*          +---------------------------------+          *)
(*   32   20|   ! " # $ % & ' ( ) * + , - . / |20    32  *)
(*   48   30| 0 1 2 3 4 5 6 7 8 9 : ; < = > ? |30    48  *)
(*   64   40| @ A B C D E F G H I J K L M N O |40    64  *)
(*   80   50| P Q R S T U V W X Y Z [ \ ] ^ _ |50    80  *)
(*   96   60| ` a b c d e f g h i j k l m n o |60    96  *)
(*  112   70| p q r s t u v w x y z { | } ~   |70   112  *)
(*          +---------------------------------+          *)
(*            0 1 2 3 4 5 6 7 8 9 a b c d e f            *)

PROCEDURE Get*(VAR sym: INTEGER);
BEGIN
  REPEAT
    WHILE ~R.eot & (ch <= " ") DO GetCh END;
    IF R.eot THEN sym := eot
    ELSIF ch < "A" THEN
      IF ch < "0" THEN
        IF    ch = 22X THEN String;    sym := string
        ELSIF ch = "#" THEN GetCh;     sym := neq
        ELSIF ch = "$" THEN HexString; sym := string
        ELSIF ch = "&" THEN GetCh;     sym := and
        ELSIF ch = "(" THEN GetCh;     IF ch = "*" THEN sym := null; comment
                                                   ELSE sym := lparen END
        ELSIF ch = ")" THEN GetCh;     sym := rparen
        ELSIF ch = "*" THEN GetCh;     sym := times
        ELSIF ch = "+" THEN GetCh;     sym := plus
        ELSIF ch = "," THEN GetCh;     sym := comma
        ELSIF ch = "-" THEN GetCh;     sym := minus
        ELSIF ch = "." THEN GetCh;     IF ch = "." THEN GetCh; sym := upto
                                                   ELSE sym := period END
        ELSIF ch = "/" THEN GetCh;     sym := rdiv
                       ELSE GetCh;     sym := null (* ! % ' *)
        END
      ELSIF ch < ":" THEN Number(sym)
      ELSIF ch = ":" THEN GetCh;     IF ch = "=" THEN GetCh; sym := becomes
                                                 ELSE sym := colon END
      ELSIF ch = ";" THEN GetCh;     sym := semicolon
      ELSIF ch = "<" THEN GetCh;     IF ch = "=" THEN GetCh; sym := leq
                                                 ELSE sym := lss END
      ELSIF ch = "=" THEN GetCh;     sym := eql
      ELSIF ch = ">" THEN GetCh;     IF ch = "=" THEN GetCh; sym := geq
                                                 ELSE sym := gtr END
                     ELSE GetCh;     sym := null (* ? @ *)
      END
    ELSIF ch < "[" THEN Identifier(sym)
    ELSIF ch < "a" THEN
      IF    ch = "[" THEN sym := lbrak
      ELSIF ch = "]" THEN sym := rbrak
      ELSIF ch = "^" THEN sym := arrow
      ELSIF ch = 5CX THEN sym := bslash
                     ELSE sym := null  (* _ ` *)
      END;
      GetCh
    ELSIF ch < "{" THEN Identifier(sym) ELSE
      IF    ch = "{" THEN sym := lbrace
      ELSIF ch = "}" THEN sym := rbrace
      ELSIF ch = "|" THEN sym := bar
      ELSIF ch = "~" THEN sym := not
      ELSIF ch = 7FX THEN sym := upto
                     ELSE sym := null
      END;
      GetCh
    END
  UNTIL sym # null
END Get;


PROCEDURE Init*(T: Texts.Text; pos: LONGINT);
BEGIN
  ListCodegen  := FALSE;
  ListAssembly := FALSE;
  Hasty        := FALSE;
  errpos       := pos;
  errcnt       := 0;
  linebeg      := 0;
  linenum      := 1;
  Texts.OpenReader(R, T, pos);
  ch := 0X;
  GetCh
END Init;

PROCEDURE SourceFile*(): Files.File;
BEGIN RETURN Texts.Base(R) END SourceFile;

PROCEDURE EnterKW(sym: INTEGER; name: ARRAY OF CHAR);
BEGIN keyTab[k].id := name; keyTab[k].sym := sym; INC(k)
END EnterKW;

BEGIN Texts.OpenWriter(W);  k := 0;  KWX[0] := 0;  KWX[1] := 0;
  ch := 0X;
  EnterKW(if, "IF");
  EnterKW(do, "DO");
  EnterKW(of, "OF");
  EnterKW(or, "OR");
  EnterKW(to, "TO");
  EnterKW(in, "IN");
  EnterKW(is, "IS");
  EnterKW(by, "BY");
  KWX[2] := k;

  EnterKW(end, "END");
  EnterKW(nil, "NIL");
  EnterKW(var, "VAR");
  EnterKW(div, "DIV");
  EnterKW(mod, "MOD");
  EnterKW(for, "FOR");
  KWX[3] := k;

  EnterKW(else, "ELSE");
  EnterKW(then, "THEN");
  EnterKW(true, "TRUE");
  EnterKW(type, "TYPE");
  EnterKW(case, "CASE");
  KWX[4] := k;

  EnterKW(elsif, "ELSIF");
  EnterKW(false, "FALSE");
  EnterKW(array, "ARRAY");
  EnterKW(begin, "BEGIN");
  EnterKW(const, "CONST");
  EnterKW(until, "UNTIL");
  EnterKW(while, "WHILE");
  KWX[5] := k;

  EnterKW(record, "RECORD");
  EnterKW(repeat, "REPEAT");
  EnterKW(return, "RETURN");
  EnterKW(import, "IMPORT");
  EnterKW(module, "MODULE");
  KWX[6] := k;

  EnterKW(pointer, "POINTER");
  KWX[7] := k; KWX[8] := k;

  EnterKW(procedure, "PROCEDURE");
  KWX[9] := k
END ORS.
