MODULE OCB;   (*NW 25.6.2014  / AP 4.3.2020 / 8.3.2019  in Oberon-07*)

IMPORT SYSTEM, H := Host, Files, Texts, ORS;

(*  Definition of data types Object and Type, which together form the data
    structure called "symbol table".

    Contains procedures for creation of Objects, and for search:
      NewObj, this, thisimport, thisfield (and OpenScope, CloseScope).

    Handling of import and export, i.e. reading and writing of "symbol
    files"  is done by procedures Import and Export.

    This module contains the list of standard identifiers, with which the
    symbol table (universe), and that of the pseudo-module SYSTEM are
    initialized.
*)


CONST
  WordSize*   = 8;
  versionkey* = 1;
  maxTypTab   = 100;

  (* class values *)
  Head*    = 0;
  Const*   = 1;  (* Atomic constant value *)
  String*  = 2;  (* String constant *)
  Typ*     = 3;  (* Type descriptor *)
  Proc*    = 4;  (* Procedure body address constant *)
  GlobVar* = 5;  (* Module global / imported VAR *)
  ProcVar* = 6;  (* Procedure local variable *)
  ValPar*  = 7;  (* Atomic value parameter *)
  RefPar*  = 8;  (* VAR parameter or structured value parameter *)
  Field*   = 9;
  StdProc* = 10;
  StdFunc* = 11;
  Mod*     = 12;

  (* Generated classes *)
  (*                    next        dsc       val            expo  *)
  Expression* = 20;  (* 1st arg     2nd arg                        *)
  Param*      = 21;  (* formal      expr      1st slot used        *)
  Statement*  = 22;  (* prev stmt   detail                         *)
  Assign*     = 23;  (* dest        expr                           *)
  If*         = 24;  (* cond        body                           *)
  Elsif*      = 25;  (* cond        body                           *)
  Else*       = 26;  (* /           body                           *)
  While*      = 27;  (* cond        body                           *)
  Repeat*     = 28;  (* cond        body                           *)
  Case*       = 29;  (* expr        /                              *)
  Caseval*    = 30;  (* val         body                           *)
(*CallSite*   = 31;*)(* 1st par     function                       *)
  PassParam*  = 32;  (* type        val/expr                       *)


  (* form values *)
  Byte*    = 1;
  Bool*    = 2;
  Char*    = 3;
  Int8*    = 4;
  Int16*   = 5;
  Int32*   = 6;
  Int64*   = 7;
  Card16*  = 8;
  Card32*  = 9;
  Real*    = 10;
  Set*     = 11;
  Pointer* = 12;
  NilTyp*  = 13;
  NoTyp*   = 14;
  ProcAdr* = 15;
  StrAdr*  = 16;
  Array*   = 17;
  Record*  = 18;

  (* hosted values *)
  Nohost*  = 0;  (* Normal Oberon behaviour                      *)
  Nogc*    = 1;  (* Record or pointer with no garbage collection *)
  LinABI*  = 2;  (* Procedure with Linux X64 ABI                 *)
  WinABI*  = 3;  (* Procedure with Windows X64 ABI               *)
  Syscall* = 4;  (* Linux syscall procedure                      *)


TYPE
  Object*  = POINTER TO ObjDesc;
  Module*  = POINTER TO ModDesc;
  Type*    = POINTER TO TypeDesc;

  (* Object classes                                                           *)
  (*                                                                          *)
  (*  class       val                       ext  modno  next         dsc      *)
  (*  -------     ------------------------  ---  -----  -----------  -------- *)
  (*  Const       value                                                       *)
  (*  String      code address              len                               *)
  (*  Typ         descriptor address/impno       modno                        *)
  (*  Proc        code address/impno             modno                        *)
  (*  GlobVar     data address/impno             modno                        *)
  (*  ProcVar     index                                                       *)
  (*  ValPar      index                                                       *)
  (*  RefPar      index                                                       *)
  (*  Field       offset                                                      *)
  (*  StdProc     inline code number                                          *)
  (*  StdFunc     inline code number                                          *)
  (*  Mod         key                                                         *)
  (*                                                                          *)
  (*  Selector    field offset                          variable     index    *)
  (*  Expression  op symbol                 reg         1st arg      2nd arg  *)
  (*  Param                                             reg/stk      expr     *)
  (*  Statement                                         prev stmt    detail   *)
  (*  Assign                                            dest         expr     *)
  (*  If                                                cond         body     *)
  (*  Elsif                                             cond         body     *)
  (*  Else                                              /            body     *)
  (*  While                                             cond         body     *)
  (*  Repeat                                            cond         body     *)
  (*  Case                                              expr         /        *)
  (*  Caseval                                           val          body     *)
  (*                                                                          *)
  (*  For selector, expo means dereference before index/field offset          *)

  ObjDesc* = RECORD
    class*: BYTE;
    rdo*:   BOOLEAN;    (* read-only                               *)
    val*:   INTEGER;    (* see above                               *)
    ext*:   INTEGER;    (* see above                               *)
    modno*: INTEGER;    (* <0: local stack frame, = proc nesting   *)
                        (*  0: module global                       *)
                        (* >0: -imported module num (1 .. 16)      *)
    type*:  Type;
    name*:  ORS.Ident;
    expo*:  BOOLEAN;    (* whether exported as a symbol            *)
    exno*:  BYTE;       (* export number in object file            *)
    next*:  Object;
    dsc*:   Object
  END;

  ModDesc = RECORD (ObjDesc) orgname*: ORS.Ident END;


  (* Type forms and the meaning of "dsc" and "base" *)
  (*                                                *)
  (* form     dsc      base                         *)
  (* ---------------------------------------------  *)
  (* Pointer  -        type of dereferenced object  *)
  (* ProcAdr  params   result type                  *)
  (* Array    -        type of elements             *)
  (* Record   fields   extension                    *)

  TypeDesc* = RECORD
    form*:   INTEGER;
    mno*:    INTEGER;  (* <0: stack. 0: global. >0: import module num      *)
    ref*:    INTEGER;  (* Only used for import/export                      *)
    nofpar*: INTEGER;  (* For procedures: parameter count                  *)
                       (* For records:    extension level                  *)
    len*:    INTEGER;  (* For arrays:     element count, < 0 => open array *)
                       (* For records:    adr of descriptor                *)
                       (* For procedures: size of params                   *)
    dsc*:    Object;   (* For procedure:  params & local vars              *)
                       (* For records:    fields                           *)
    typobj*: Object;   (* For Typ:        object representing type         *)
    base*:   Type;     (* For arrays, records, pointers, fn procedures     *)
    size*:   INTEGER;  (* In bytes. For procedures: 8                      *)
    hosted*: BYTE      (* 1: no-gc rec, 2: Linux proc, 3: Windows proc     *)
  END;


VAR
  topScope*, universe, system*: Object;

  byteType*, boolType*, charType*: Type;
  intType*,  realType*, setType*:  Type;
  nilType*,  noType*,   strType*:  Type;

  int8Type:               Type;
  int16Type,  int32Type:  Type;
  card16Type, card32Type: Type;

  procType*: Type;

  nofmod, Ref: INTEGER;
  typtab: ARRAY maxTypTab OF Type;

  W*: Texts.Writer;


(* ------------------------------- Logging -------------------------------- *)

PROCEDURE wa*;                    BEGIN Texts.Append(Texts.Log, W.buf) END wa;
PROCEDURE wn*;                    BEGIN Texts.WriteLn(W); wa           END wn;
PROCEDURE wc* (c: CHAR);          BEGIN Texts.Write(W, c)              END wc;
PROCEDURE ws* (s: ARRAY OF CHAR); BEGIN Texts.WriteString(W, s)        END ws;
PROCEDURE wsn*(s: ARRAY OF CHAR); BEGIN Texts.WriteString(W, s); wn    END wsn;
PROCEDURE wi* (i: INTEGER);       BEGIN Texts.WriteInt(W, i, 1)        END wi;
PROCEDURE wir*(i, n: INTEGER);    BEGIN Texts.WriteInt(W, i, n)        END wir;

PROCEDURE wh*(n: INTEGER);
VAR hex: ARRAY 32 OF CHAR;
BEGIN H.IntToHex(n, hex);  ws(hex) END wh;

PROCEDURE wtab*(n: INTEGER);
BEGIN WHILE W.buf.len < n DO wc(" ") END;  wa END wtab;




PROCEDURE NewObj*(VAR obj: Object; id: ORS.Ident; class: INTEGER);
(* insert new Object with name id *)
VAR new, x: Object;
BEGIN x := topScope;
  WHILE (x.next # NIL) & (x.next.name # id) DO x := x.next END;
  IF x.next = NIL THEN NEW(new);
    new.name := id;     new.class := class;  new.next := NIL;
    new.rdo  := FALSE;  new.dsc   := NIL;    new.exno := 0;
    new.expo := FALSE;  new.modno := 0;      new.type := NIL;
    new.val  := 0;      new.ext   := 0;
    x.next := new;  obj := new
  ELSE obj := x.next; ORS.Mark("mult def")
  END
END NewObj;

PROCEDURE thisObj*(): Object;
VAR s, x: Object;
BEGIN s := topScope;
  REPEAT x := s.next;
    WHILE (x # NIL) & (x.name # ORS.id) DO x := x.next END;
    s := s.dsc
  UNTIL (x # NIL) OR (s = NIL);
  RETURN x
END thisObj;

PROCEDURE SystemImported*(): BOOLEAN;
VAR s, x: Object;
BEGIN s := topScope;
  REPEAT x := s.next;
    WHILE (x # NIL) & ~((x IS Module) & (x(Module).orgname = "SYSTEM")) DO
      x := x.next
    END;
    s := s.dsc
  UNTIL (x # NIL) OR (s = NIL);
RETURN x # NIL END SystemImported;

PROCEDURE thisimport*(mod: Object): Object;
VAR obj: Object;
BEGIN
  IF mod.rdo THEN
    IF mod.name[0] # 0X THEN
      obj := mod.dsc;
      WHILE (obj # NIL) & (obj.name # ORS.id) DO obj := obj.next END
    ELSE obj := NIL
    END
  ELSE obj := NIL
  END;
  RETURN obj
END thisimport;

PROCEDURE thisfield*(rec: Type): Object;
VAR fld: Object;
BEGIN fld := rec.dsc;
  WHILE (fld # NIL) & (fld.name # ORS.id) DO fld := fld.next END;
  RETURN fld
END thisfield;

PROCEDURE OpenScope*;
VAR s: Object;
BEGIN NEW(s); s.class := Head; s.dsc := topScope; s.next := NIL; topScope := s
END OpenScope;

PROCEDURE CloseScope*;
BEGIN topScope := topScope.dsc
END CloseScope;

(*------------------------------- Import ---------------------------------*)

PROCEDURE MakeFileName*(VAR FName: ORS.Ident; name, ext: ARRAY OF CHAR);
VAR i, j: INTEGER;
BEGIN i := 0; j := 0;  (*assume name suffix less than 4 characters*)
  WHILE (i < ORS.IdLen-5) & (name[i] > 0X) DO FName[i] := name[i]; INC(i) END;
  REPEAT FName[i]:= ext[j]; INC(i); INC(j) UNTIL ext[j] = 0X;
  FName[i] := 0X
END MakeFileName;

PROCEDURE ThisModule(name, orgname: ORS.Ident; decl: BOOLEAN; key: INTEGER): Object;
VAR mod: Module; obj, obj1: Object;
BEGIN obj1 := topScope; obj := obj1.next;  (*search for module*)
  WHILE (obj # NIL) & (obj(Module).orgname # orgname) DO obj1 := obj; obj := obj1.next END;
  IF obj = NIL THEN  (*new module, search for alias*)
    obj := topScope.next;
    WHILE (obj # NIL) & (obj.name # name) DO obj := obj.next END;
    IF obj = NIL THEN (*insert new module*)
      NEW(mod); mod.class := Mod; mod.rdo := FALSE;
      mod.name := name; mod.orgname := orgname; mod.val := key;
      mod.modno := nofmod; INC(nofmod); mod.dsc := NIL; mod.next := NIL;
      IF decl THEN mod.type := noType ELSE mod.type := nilType END;
      obj1.next := mod; obj := mod
    ELSIF decl THEN
      IF obj.type.form = NoTyp THEN ORS.Mark("mult def") ELSE ORS.Mark("invalid import order") END
    ELSE ORS.Mark("conflict with alias")
    END
  ELSIF decl THEN (*module already present, explicit import by declaration*)
    IF  obj.type.form = NoTyp THEN ORS.Mark("mult def") ELSE ORS.Mark("invalid import order") END
  END;
  RETURN obj
END ThisModule;

PROCEDURE Read(VAR R: Files.Rider; VAR x: INTEGER);  (* Read signed byte *)
VAR b: BYTE;
BEGIN Files.ReadByte(R, b);
  IF b < 80H THEN x := b ELSE x := b - 100H END
END Read;

PROCEDURE InType(VAR R: Files.Rider; thismod: Object; VAR T: Type);
VAR key: INTEGER;
    ref, class, form, np, readonly: INTEGER;
    fld, par, obj, mod, last: Object;
    t: Type;
    name, modname: ORS.Ident;
    b: BYTE;
BEGIN Read(R, ref);
  IF ref < 0 THEN T := typtab[-ref]  (*already read*)
  ELSE NEW(t); T := t; typtab[ref] := t; t.mno := thismod.modno;
    Read(R, form); t.form := form;
    IF form = Pointer THEN InType(R, thismod, t.base); t.size := WordSize
    ELSIF form = Array THEN
      InType(R, thismod, t.base); Files.ReadNum(R, t.len); Files.ReadNum(R, t.size)
    ELSIF form = Record THEN
      InType(R, thismod, t.base);
      IF t.base.form = NoTyp THEN t.base := NIL; obj := NIL ELSE obj := t.base.dsc END;
      Files.ReadNum(R, t.len);     (*TD adr/exno*)
      Files.ReadNum(R, t.nofpar);  (*ext level*)
      Files.ReadNum(R, t.size);
      Read(R, class);  last := NIL;
      WHILE class # 0 DO  (*fields*)
        NEW(fld); fld.class := class; Files.ReadString(R, fld.name);
        IF last = NIL THEN t.dsc := fld ELSE last.next := fld END;
        last := fld;
        IF fld.name[0] # 0X THEN fld.expo := TRUE; InType(R, thismod, fld.type) ELSE fld.expo := FALSE; fld.type := nilType END;
        Files.ReadNum(R, fld.val);  Read(R, class)
      END;
      IF last = NIL THEN t.dsc := obj ELSE last.next := obj END
    ELSIF form = ProcAdr THEN
      InType(R, thismod, t.base);
      Files.ReadNum(R, t.len);  Files.ReadByte(R, t.hosted);
      obj := NIL; np := 0; Read(R, class);
      WHILE class # 0 DO  (*parameters*)
        NEW(par); par.class := class; Read(R, readonly); par.rdo := readonly = 1;
        Files.ReadNum(R, par.val);
        InType(R, thismod, par.type); par.next := obj; obj := par; INC(np); Read(R, class)
      END;
      t.dsc := obj; t.nofpar := np; t.size := WordSize
    END;
    Files.ReadString(R, modname);
    IF modname[0] #  0X THEN  (*re-import ========*)
      Files.ReadInt(R, key); Files.ReadString(R, name);
      mod := ThisModule(modname, modname, FALSE, key);
      obj := mod.dsc;  (*search type*)
      WHILE (obj # NIL) & (obj.name # name) DO obj := obj.next END;
      IF obj # NIL THEN T := obj.type   (*type object found in object list of mod*)
      ELSE (*insert new type object in object list of mod*)
        NEW(obj); obj.name := name; obj.class := Typ; obj.next := mod.dsc; mod.dsc := obj; obj.type := t;
        t.mno := mod.modno; t.typobj := obj; T := t
      END;
      typtab[ref] := T
    END
  END
END InType;

PROCEDURE Import*(VAR modid, modid1: ORS.Ident);
VAR key: INTEGER; class, k: INTEGER;
    obj: Object;  t: Type;
    thismod: Object;
    modname, fname: ORS.Ident;
    F: Files.File; R: Files.Rider;
BEGIN
  IF modid1 = "SYSTEM" THEN
    thismod := ThisModule(modid, modid1, TRUE,  key); DEC(nofmod);
    thismod.modno := 0; thismod.dsc := system; thismod.rdo := TRUE
  ELSE MakeFileName(fname, modid1, ".smb"); F := Files.Old(fname);
    IF F # NIL THEN
      Files.Set(R, F, 0); Files.ReadInt(R, key); Files.ReadInt(R, key); Files.ReadString(R, modname);
      IF modid1 # modname THEN
        (* catch module name mismatch due to Windows filename case insensitivity *)
        ORS.Mark("Module name case differs from filename");  modid1 := modname;
      END;
      thismod := ThisModule(modid, modid1, TRUE, key); thismod.rdo := TRUE;
      Read(R, class); (*version key*)
      IF class # versionkey THEN ORS.Mark("wrong version") END;
      Read(R, class);
      WHILE class # 0 DO
        NEW(obj); obj.class := class; Files.ReadString(R, obj.name);
        InType(R, thismod, obj.type); obj.modno := -thismod.modno;
        IF class = Typ THEN
          t := obj.type; t.typobj := obj; Read(R, k);  (*fixup bases of previously declared pointer types*)
          WHILE k # 0 DO typtab[k].base := t; Read(R, k) END
        ELSE
          IF class = Const THEN
            IF obj.type.form = Real THEN Files.ReadInt(R, obj.val) ELSE Files.ReadNum(R, obj.val) END
          ELSIF class = GlobVar THEN Files.ReadNum(R, obj.val); obj.rdo := TRUE
          END
        END;
        obj.next := thismod.dsc; thismod.dsc := obj; Read(R, class)
      END;
    ELSE ORS.Mark("import not available")
    END
  END
END Import;

(*-------------------------------- Export ---------------------------------*)

PROCEDURE Write(VAR R: Files.Rider; x: INTEGER);  (* Write (signed) byte *)
BEGIN Files.WriteByte(R, x)
END Write;

PROCEDURE OutType(VAR R: Files.Rider; t: Type);
VAR obj, mod, fld, bot: Object;

  PROCEDURE OutPar(VAR R: Files.Rider; par: Object; n: INTEGER);
    VAR cl: INTEGER;
  BEGIN
    IF n > 0 THEN
      OutPar(R, par.next, n-1); cl := par.class;
      Write(R, cl);
      IF par.rdo THEN Write(R, 1) ELSE Write(R, 0) END;
      Files.WriteNum(R, par.val);
      OutType(R, par.type)
    END
  END OutPar;

  PROCEDURE FindHiddenPointers(VAR R: Files.Rider; typ: Type; offset: INTEGER);
    VAR fld: Object; i, n: INTEGER;
  BEGIN
    IF (typ.form = Pointer) OR (typ.form = NilTyp) THEN Write(R, Field); Write(R, 0); Files.WriteNum(R, offset)
    ELSIF typ.form = Record THEN fld := typ.dsc;
      WHILE fld # NIL DO FindHiddenPointers(R, fld.type, fld.val + offset); fld := fld.next END
    ELSIF typ.form = Array THEN i := 0; n := typ.len;
      WHILE i < n DO FindHiddenPointers(R, typ.base, typ.base.size * i + offset); INC(i) END
    END
  END FindHiddenPointers;

BEGIN
  IF t.ref > 0 THEN (*type was already output*) Write(R, -t.ref)
  ELSE obj := t.typobj;
    IF obj # NIL THEN Write(R, Ref); t.ref := Ref; INC(Ref) ELSE (*anonymous*) Write(R, 0) END;
    Write(R, t.form);
    IF t.form = Pointer THEN OutType(R, t.base)
    ELSIF t.form = Array THEN OutType(R, t.base); Files.WriteNum(R, t.len); Files.WriteNum(R, t.size)
    ELSIF t.form = Record THEN
      IF t.base # NIL THEN OutType(R, t.base); bot := t.base.dsc ELSE OutType(R, noType); bot := NIL END;
      IF obj # NIL THEN Files.WriteNum(R, obj.exno) ELSE Write(R, 0) END;
      Files.WriteNum(R, t.nofpar); Files.WriteNum(R, t.size);
      fld := t.dsc;
      WHILE fld # bot DO  (*fields*)
        IF fld.expo THEN
          Write(R, Field); Files.WriteString(R, fld.name); OutType(R, fld.type); Files.WriteNum(R, fld.val)  (*offset*)
        ELSE FindHiddenPointers(R, fld.type, fld.val)
        END;
        fld := fld.next
      END;
      Write(R, 0)
    ELSIF t.form = ProcAdr THEN OutType(R, t.base);
      Files.WriteNum(R, t.len);  Files.WriteByte(R, t.hosted);
      OutPar(R, t.dsc, t.nofpar); Write(R, 0)
    END;
    IF (t.mno > 0) & (obj # NIL) THEN  (*re-export, output name*)
      mod := topScope.next;
      WHILE (mod # NIL) & (mod.modno # t.mno) DO mod := mod.next END;
      IF mod # NIL THEN Files.WriteString(R, mod(Module).orgname); Files.WriteInt(R, mod.val); Files.WriteString(R, obj.name)
      ELSE ORS.Mark("re-export not found"); Write(R, 0)
      END
    ELSE Write(R, 0)
    END
  END
END OutType;

PROCEDURE Export*(VAR modid: ORS.Ident; VAR newSF: BOOLEAN; VAR key: INTEGER);
VAR x, sum, oldkey: INTEGER;
    obj, obj0: Object;
    filename: ORS.Ident;
    F, F1: Files.File; R, R1: Files.Rider;
BEGIN Ref := Record + 1; MakeFileName(filename, modid, ".smb");
  F := Files.New(filename); Files.Set(R, F, 0);
  Files.WriteInt(R, 0); (*placeholder*)
  Files.WriteInt(R, 0); (*placeholder for key to be inserted at the end*)
  Files.WriteString(R, modid); Write(R, versionkey);
  obj := topScope.next;
  WHILE obj # NIL DO
    IF obj.expo THEN
      Write(R, obj.class); Files.WriteString(R, obj.name);
      OutType(R, obj.type);
      IF obj.class = Typ THEN
        IF obj.type.form = Record THEN
          obj0 := topScope.next;  (*check whether this is base of previously declared pointer types*)
          WHILE obj0 # obj DO
            IF (obj0.type.form = Pointer) & (obj0.type.base = obj.type) & (obj0.type.ref > 0) THEN Write(R, obj0.type.ref) END;
            obj0 := obj0.next
          END
        END;
        Write(R, 0)
      ELSIF obj.class = Const THEN
        IF obj.type.form = ProcAdr THEN Files.WriteNum(R, obj.exno)
        ELSIF obj.type.form = Real THEN Files.WriteInt(R, obj.val)
        ELSE Files.WriteNum(R, obj.val)
        END
      ELSIF obj.class = GlobVar THEN Files.WriteNum(R, obj.exno)
      END
    END;
    obj := obj.next
  END;
  REPEAT Write(R, 0) UNTIL Files.Length(F) MOD 8 = 0; (* Zero fill to whole multiple of integer size *)

  FOR Ref := Record+1 TO maxTypTab-1 DO typtab[Ref] := NIL END;
  Files.Set(R, F, 0); sum := 0; Files.ReadInt(R, x);  (* compute key (checksum) *)
  WHILE ~R.eof DO sum := sum + x; Files.ReadInt(R, x) END;
  F1 := Files.Old(filename); (*sum is new key*)
  IF F1 # NIL THEN Files.Set(R1, F1, 8); Files.ReadInt(R1, oldkey) ELSE oldkey := sum+1 END;
  IF sum # oldkey THEN
    IF newSF OR (F1 = NIL) THEN
      key := sum; newSF := TRUE; Files.Set(R, F, 8); Files.WriteInt(R, sum);  (* insert checksum *)
      Files.Register(F)
    ELSE ORS.Mark("new symbol file inhibited")
    END
  ELSE newSF := FALSE; key := sum
  END
END Export;

PROCEDURE Init*;
BEGIN topScope := universe; nofmod := 1
END Init;

PROCEDURE type(ref, form: INTEGER; size: INTEGER): Type;
VAR tp: Type;
BEGIN NEW(tp); tp.form := form; tp.size := size; tp.ref := ref; tp.base := NIL;
  typtab[ref] := tp; RETURN tp
END type;

PROCEDURE enter(name: ARRAY OF CHAR; cl: INTEGER; type: Type; n: INTEGER);
VAR obj: Object;
BEGIN NEW(obj); obj.name := name; obj.class := cl; obj.type := type; obj.val := n; obj.dsc := NIL;
  IF cl = Typ THEN type.typobj := obj END;
  obj.next := system; system := obj
END enter;


BEGIN
  Texts.OpenWriter(W);

  byteType   := type(Byte,   Int64,   1);
  boolType   := type(Bool,   Bool,    1);
  charType   := type(Char,   Char,    1);
  int8Type   := type(Int8,   Int64,   1);
  int16Type  := type(Int16,  Int64,   2);
  card16Type := type(Card16, Int64,   2);
  int32Type  := type(Int32,  Int64,   4);
  card32Type := type(Card32, Int64,   4);
  intType    := type(Int64,  Int64,   8);
  realType   := type(Real,   Real,    8);
  setType    := type(Set,    Set,     8);
  nilType    := type(NilTyp, NilTyp,  8);
  noType     := type(NoTyp,  NoTyp,   8);
  strType    := type(StrAdr, StrAdr, 16);
  procType   := type(ProcAdr,   ProcAdr,    8);

(*initialize universe with data types and in-line procedures;
   LONGINT is synonym to INTEGER, LONGREAL to REAL.
   LED, ADC, SBC; LDPSR, LDREG, REG, COND are not in language definition*)
  system := NIL;  (*n = procno*10 + nofpar*)
  enter("UML",      StdFunc, intType,  132);  (*functions*)
  enter("SBC",      StdFunc, intType,  122);
  enter("ADC",      StdFunc, intType,  112);
  enter("ROR",      StdFunc, intType,  92);
  enter("ASR",      StdFunc, intType,  82);
  enter("LSL",      StdFunc, intType,  72);
  enter("LEN",      StdFunc, intType,  61);
  enter("CHR",      StdFunc, charType, 51);
  enter("ORD",      StdFunc, intType,  41);
  enter("FLT",      StdFunc, realType, 31);
  enter("FLOOR",    StdFunc, intType,  21);
  enter("ODD",      StdFunc, boolType, 11);
  enter("ABS",      StdFunc, intType,  1);
  enter("LED",      StdProc, noType,   81);   (*procedures*)
  enter("UNPK",     StdProc, noType,   72);
  enter("PACK",     StdProc, noType,   62);
  enter("NEW",      StdProc, noType,   51);
  enter("ASSERT",   StdProc, noType,   41);
  enter("EXCL",     StdProc, noType,   32);
  enter("INCL",     StdProc, noType,   22);
  enter("DEC",      StdProc, noType,   11);
  enter("INC",      StdProc, noType,   1);
  enter("SET",      Typ,     setType,  0);    (*types*)
  enter("BOOLEAN",  Typ,     boolType, 0);
  enter("BYTE",     Typ,     byteType, 0);
  enter("CHAR",     Typ,     charType, 0);
  enter("LONGREAL", Typ,     realType, 0);
  enter("REAL",     Typ,     realType, 0);
  enter("LONGINT",  Typ,     intType,  0);
  enter("INTEGER",  Typ,     intType,  0);
  topScope := NIL; OpenScope; topScope.next := system; universe := topScope;

  system   := NIL;  (* initialize "unsafe" pseudo-module SYSTEM*)
  enter("COND",   StdFunc, boolType,   191);  (*functions*)
  enter("SIZE",   StdFunc, intType,    181);
  enter("ADR",    StdFunc, intType,    171);
  enter("VAL",    StdFunc, intType,    162);
  enter("REG",    StdFunc, intType,    151);
  enter("BIT",    StdFunc, boolType,   142);
  enter("LDREG",  StdProc, noType,     142);  (*procedures*)
  enter("LDPSR",  StdProc, noType,     131);
  enter("COPY",   StdProc, noType,     123);
  enter("PUT",    StdProc, noType,     112);
  enter("GET",    StdProc, noType,     102);
  enter("INT8",   Typ,     int8Type,   0);    (*types*)
  enter("INT16",  Typ,     int16Type,  0);
  enter("INT32",  Typ,     int32Type,  0);
  enter("CARD16", Typ,     card16Type, 0);
  enter("CARD32", Typ,     card32Type, 0)
END OCB.
