MODULE Modules;  (*Link and load on RISC;  NW 20.10.2013 / 8.1.2019*)

IMPORT SYSTEM, H := WinHost, Files;

TYPE
  Module*     = POINTER- TO ModDesc;
  ModuleName* = ARRAY 32 OF CHAR;
  ModDesc* = RECORD-
    name*:    ModuleName;  (* 00H *)
    next*:    Module;      (* 20H Set on load *)
    key*:     INTEGER;     (* 28H *)
    num*:     INTEGER;     (* 30H Module num, set on load *)
    size*:    INTEGER;     (* 38H Allocated memory, set on load (image size in file) *)
    refcnt*:  INTEGER;     (* 40H Managed by module loader *)
    vars*:    INTEGER;     (* 48H Start of global VARs / in code file start of import ref table *)
    init*:    INTEGER;     (* 50H Module initialisation entry point *)
    imprefs*: INTEGER;     (* 58H Imports references *)
    cmd*:     INTEGER;     (* 60H PO2013 Commands: address of seq of command strings and code offsets *)
    exports*: INTEGER;     (* 68H PO2013 Entries: address of array of exported offsets *)
    ptr*:     INTEGER;     (* 70H PO2013 Pointers: address of array of pointer var addresses *)
    lines*:   INTEGER;     (* 78H Line numbers etc. to address mapping *)
    varsize*: INTEGER;     (* 80H Size of module VARs *)
    magic*:   ARRAY 8 OF CHAR; (* 88H *)
  END;
  Command*    = PROCEDURE;

VAR
  Root*:       Module;
  M:           Module;     (* Loaded module 'Oberon' *)
  P:           Command;
  res*:        INTEGER;
  Importing*:  ModuleName;
  Imported*:   ModuleName;
  ActCnt*:     INTEGER;    (* Action count for scheduling garbage collection *)
  StackOrg*:   INTEGER;
  PreloadNext: INTEGER;


PROCEDURE error(n: INTEGER;  name: ARRAY OF CHAR);
BEGIN res := n;  Importing := name
END error;


PROCEDURE Check(s: ARRAY OF CHAR);
VAR i: INTEGER;  ch: CHAR;
BEGIN ch := s[0];  res := 1;  i := 1;
  IF (ch >= "A") & (ch <= "Z") OR (ch >= "a") & (ch <= "z") THEN
    REPEAT ch := s[i];  INC(i)
    UNTIL ~(   (ch >= "0") & (ch <= "9") OR (ch >= "A") & (ch <= "Z")
            OR (ch >= "a") & (ch <= "z") OR (ch = ".")) OR (i = 32);
    IF (i < 32) & (ch = 0X) THEN res := 0 END
  END
END Check;


PROCEDURE FindModule(name: ARRAY OF CHAR): Module;
VAR mod: Module;
BEGIN mod := Root;
  WHILE (mod # NIL) & (name # mod.name) DO mod := mod.next END;
RETURN mod END FindModule;


PROCEDURE AllocateModule(VAR header: ModDesc): Module;
(* Allocate space for module & preset header *)
VAR mod: Module;  p, size, allocsize: INTEGER;
BEGIN
  size := (header.vars + header.varsize + 15) DIV 16 * 16;
  mod := Root;
  WHILE (mod # NIL) & ~((mod.name[0] = 0X) & (mod.size >= size)) DO
    mod := mod.next
  END;
  IF mod # NIL THEN (* Use existing hole *)
    p := ORD(mod)
  ELSE
    H.Allocate(size, p, allocsize);  mod := SYSTEM.VAL(Module, p);
    IF mod # NIL THEN
      H.ZeroFill(mod^);
      mod.size := allocsize;
      mod.num  := Root.num + 1;
      mod.next := Root;
      Root     := mod
    END
  END;
  IF mod # NIL THEN
    mod.refcnt  := 0;
    mod.name    := header.name;
    mod.key     := header.key;
    mod.vars    := p + header.vars;
    mod.init    := header.init;
    mod.imprefs := 0;
    mod.cmd     := p + header.cmd;
    mod.exports := header.exports;
    mod.ptr     := header.ptr;
    mod.lines   := header.lines;
    mod.varsize := header.varsize;
  END;
RETURN mod END AllocateModule;


PROCEDURE ExportedAddress(modhdr: Module; index: INTEGER): INTEGER;
VAR exportoffset: SYSTEM.CARD32;
BEGIN
  SYSTEM.GET(ORD(modhdr) + modhdr.exports + index * 4, exportoffset);
RETURN ORD(modhdr) + exportoffset END ExportedAddress;


PROCEDURE LinkImport*(modadr, offset, impno, modno: INTEGER; modules: ARRAY OF Module);
VAR disp, absreloc: INTEGER;
BEGIN
  IF modno = 0 THEN  (* system function *)
    SYSTEM.GET(modadr + offset, disp);
    ASSERT((impno >= 0) & (impno <= H.HandlerCount));
    disp := SYSTEM.ADR(H.Handlers) + 8 * impno + disp - modadr;
    SYSTEM.PUT(modadr + offset, disp)
  ELSIF modno = 0FFFFH THEN (* 64 bit absolute address relocation *)
    (* qword at offset contains 32/0,32/module offset or 32/1,16/mod,16/imp *)
    SYSTEM.GET(modadr + offset, absreloc);
    IF absreloc DIV 100000000H = 0 THEN  (* offset in this module *)
      SYSTEM.PUT(modadr + offset, modadr + absreloc)
    ELSE  (* import reference from another module *)
      modno := absreloc DIV 10000H MOD 10000H;  ASSERT(modno > 0);
      impno := absreloc MOD 10000H;
      ASSERT(impno > 0);
      SYSTEM.PUT(modadr + offset, ExportedAddress(modules[modno-1], impno-1))
    END
  ELSE
    ASSERT(modno > 0);
    SYSTEM.GET(modadr + offset, disp);
    INC(disp, ExportedAddress(modules[modno-1], impno-1) - modadr);
    SYSTEM.PUT(modadr + offset, disp)
  END
END LinkImport;


PROCEDURE Preload;  (* Load all modules from EXE preload section *)
VAR
  hdr, mod:  Module;
  i, p:      INTEGER;
  impname:   ModuleName;
  impkey:    INTEGER;
  impmod:    Module;
  impcount:  SYSTEM.CARD32;
  offset:    SYSTEM.CARD32;
  impno:     SYSTEM.CARD16;
  modno:     SYSTEM.CARD16;
  import:    ARRAY 16 OF Module;
  nimpmods:  INTEGER;
  ptroff:    INTEGER;
  body:      Command;
BEGIN
  WHILE PreloadNext # 0  DO
    hdr := SYSTEM.VAL(Module, PreloadNext);
    IF hdr.name[0] = 0X THEN PreloadNext := 0  (* End of preload modules reached *)
    ELSE
      ASSERT(hdr.magic = "Oberon5");
      mod := AllocateModule(hdr^);  ASSERT(mod # NIL);
      p := PreloadNext + SYSTEM.SIZE(ModDesc);
      SYSTEM.COPY(p, ORD(mod) + SYSTEM.SIZE(ModDesc), hdr.vars - SYSTEM.SIZE(ModDesc));

      (* Load Imported modules array - for preload Link guarantees all will be present *)
      nimpmods := 0;
      H.GetString(p, impname);
      WHILE impname[0] # 0X DO
        SYSTEM.GET(p, impkey);  INC(p, 8);
        impmod := FindModule(impname);  ASSERT(impmod # NIL);  ASSERT(impmod.key = impkey);
        import[nimpmods] := impmod;  INC(nimpmods);
        H.GetString(p, impname);
      END;

      (* Link imports *)
      p := PreloadNext + hdr.vars;  (* Address import table *)
      SYSTEM.GET(p, impcount);  INC(p, 4);
      FOR i := 1 TO impcount DO
        SYSTEM.GET(p, offset);  SYSTEM.GET(p+4, impno);  SYSTEM.GET(p+6, modno);
        INC(p, 8);
        LinkImport(ORD(mod), offset, impno, modno, import)
      END;

      (* Relocate pointer addresses *)
      p := ORD(mod) + mod.ptr;  SYSTEM.GET(p, ptroff);
      WHILE ptroff >= 0 DO
        SYSTEM.PUT(p, mod.vars + ptroff);  INC(p, 8);  SYSTEM.GET(p, ptroff)
      END;

      (* Increment Imported module refcounts *)
      FOR i := 0 TO nimpmods-1 DO INC(import[i].refcnt) END;

      INC(PreloadNext, hdr.size);

      (* Initialize module *)
      IF mod.init # 0 THEN
        body := SYSTEM.VAL(Command, ORD(mod) + mod.init);  body
      END
    END
  END
END Preload;


PROCEDURE Load*(name: ARRAY OF CHAR;  VAR newmod: Module);
  (*search module in list;  if not found, load module.
    res = 0: already present or loaded;
    res = 1: file not available;
    res = 2: file has wrong versionkey
    res = 3: key conflict;
    res = 4: bad file version;
    res = 5: corrupted file;
    res = 7: no space*)
VAR
  filename:  ARRAY 64 OF CHAR;
  F:         Files.File;
  nimpmods:  INTEGER;
  R:         Files.Rider;
  header:    ModDesc;
  name1:     ModuleName;
  key:       INTEGER;
  impname:   ModuleName;
  impkey:    INTEGER;
  impmod:    Module;
  import:    ARRAY 16 OF Module;
  mod:       Module;
  impcount:  SYSTEM.CARD32;
  offset:    SYSTEM.CARD32;
  impno:     SYSTEM.CARD16;
  modno:     SYSTEM.CARD16;
  p:         INTEGER;
  ptroff:    INTEGER;
  i:         INTEGER;
  body:      Command;
BEGIN
  res       := 0;
  Importing := name;
  newmod    := FindModule(name);

  IF (newmod = NIL) & (PreloadNext # 0) THEN
    Preload;  newmod := FindModule(name);
  END;

  IF newmod = NIL THEN
    Check(name);

    IF res = 0 THEN
      filename := name;  H.Append(".x64", filename);  F := Files.Old(filename);
      IF F = NIL THEN res := 1 END
    END;

    IF res = 0 THEN
      nimpmods := 0;
      Files.Set(R, F, 0);  Files.ReadVar(R, header);
      IF header.magic # "Oberon5" THEN res := 4 ELSE
        name1     := header.name;
        Importing := name1;
        key       := header.key;
        (* Load list of Imported modules (immediately follows module header) *)
        Files.ReadString(R, impname);
        WHILE (impname[0] # 0X) & (res = 0) DO
          Files.ReadInt(R, impkey);
          Load(impname, impmod);
          import[nimpmods] := impmod;
          Importing := name1;
          IF res = 0 THEN
            IF impmod.key = impkey THEN INC(impmod.refcnt);  INC(nimpmods)
            ELSE error(3, name1);  Imported := impname
            END
          END;
          Files.ReadString(R, impname)
        END
      END
    END;

    IF res = 0 THEN
      mod := AllocateModule(header);  IF mod = NIL THEN error(7, name1) END
    END;

    IF res = 0 THEN
      (* Read static memory, including code, type descriptors, strings and pointers *)
      Files.Set(R, F, SYSTEM.SIZE(ModDesc));
      Files.ReadMem(R, ORD(mod) + SYSTEM.SIZE(ModDesc), header.vars - SYSTEM.SIZE(ModDesc));

      (* Link imports *)

      Files.Set(R, F, header.imprefs);
      Files.ReadVar(R, impcount);
      FOR i := 1 TO impcount DO
        Files.ReadVar(R, offset);  Files.ReadVar(R, impno);  Files.ReadVar(R, modno);
        LinkImport(ORD(mod), offset, impno, modno, import)
      END;

      (* Relocate pointer addresses *)
      p := ORD(mod) + mod.ptr;  SYSTEM.GET(p, ptroff);
      WHILE ptroff >= 0 DO
        SYSTEM.PUT(p, mod.vars + ptroff);  INC(p, 8);  SYSTEM.GET(p, ptroff)
      END;

      (* Initialize module *)
      IF mod.init # 0 THEN
        body := SYSTEM.VAL(Command, ORD(mod) + mod.init);  body
      END
    ELSIF res >= 3 THEN Importing := name;
      WHILE nimpmods > 0 DO DEC(nimpmods); DEC(import[nimpmods].refcnt) END
    END;
    newmod := mod
  END
END Load;


PROCEDURE ThisCommand*(mod: Module;  name: ARRAY OF CHAR): Command;
VAR k, adr, w: INTEGER;  ch: CHAR;
    s: ARRAY 32 OF CHAR;
    offset: SYSTEM.CARD32;
BEGIN res := 5;  w := 0;
  IF mod # NIL THEN
    adr := mod.cmd;  SYSTEM.GET(adr, ch);
    WHILE (ch # 0X) & (res # 0) DO k := 0;  (*read command name*)
      REPEAT s[k] := ch;  INC(k);  INC(adr);  SYSTEM.GET(adr, ch) UNTIL ch = 0X;
      s[k] := 0X;
      INC(adr); (*REPEAT INC(adr) UNTIL adr MOD 4 = 0;*)
      SYSTEM.GET(adr, offset);  INC(adr, 4);
      IF s = name THEN res := 0; w := ORD(mod) + offset ELSE SYSTEM.GET(adr, ch) END
    END
  END
  RETURN SYSTEM.VAL(Command, w)
END ThisCommand;

PROCEDURE Free*(name: ARRAY OF CHAR);
VAR mod, imp: Module;  p, q: INTEGER;
BEGIN res := 0;
  mod := FindModule(name);
  IF mod # NIL THEN
    IF mod.refcnt = 0 THEN
      mod.name[0] := 0X;
      (*
      p := mod.imp;  q := mod.cmd;
      WHILE p < q DO SYSTEM.GET(p, imp);  DEC(imp.refcnt);  INC(p, 4) END;
      *)
    ELSE res := 1
    END
  END
END Free;


(* --------------------------- Garbage collection --------------------------- *)

(* Garbage collection trigger moved here from Oberon.Mod so that programs
   can be compiled without Obeorn.Mod and work both as command line tools
   and within the Oberon system. Note that the Oberon system only collects
   garbage within the GC task which only runs after command completion, so
   the hosted command line implementation never runs garbage collection,
   the effect of garbage collection provided by the hosted OS process exit.
*)
PROCEDURE Collect* (count: INTEGER);
BEGIN ActCnt := count
END Collect;


(* ----------------------------- Trap Handling ------------------------------ *)

PROCEDURE Locate*(adr: INTEGER; VAR mod: Module; VAR line: INTEGER; VAR proc: ModuleName);
VAR
  offset, p, l, pc, i: INTEGER;
  name: ModuleName;
BEGIN
  mod := Root;  line := -1;  proc[0] := 0X;
  WHILE (mod # NIL) & ((adr < ORD(mod)) OR (adr >= mod.vars)) DO mod := mod.next END;
  IF (mod # NIL) & (mod.lines > 0) THEN
    offset := adr - ORD(mod);
    p := ORD(mod) + mod.lines;
    H.GetString(p, name);
    WHILE name[0] # 0X DO
      SYSTEM.GET(p, l);  SYSTEM.GET(p+8, pc);  INC(p, 16);  H.GetUnsigned(p, i);
      WHILE (i # 0) & (offset > pc + i) DO
        INC(pc, i);  H.GetUnsigned(p, i);  INC(l, i);  H.GetUnsigned(p, i)
      END;
      IF (offset > pc) & (offset <= pc + i) THEN
        IF name = mod.name THEN proc := "<init>" ELSE proc := name END;
        line := l;
        name[0] := 0X;  (* Terminate loop *)
      ELSE
        H.GetString(p, name)
      END
    END
  END
END Locate;


(* Non-GUI trap handler *)
PROCEDURE HandleTrap(adr: INTEGER; desc: ARRAY OF CHAR);
VAR mod: Module; line: INTEGER; proc: ModuleName;
BEGIN
  Locate(adr, mod, line, proc);
  H.ws("  Trap: ");  H.ws(desc);
  IF mod = NIL THEN H.ws(" at address ");  H.wh(adr);  H.wsn("H.")
  ELSE H.ws(" in module "); H.ws(mod.name);
    IF line < 0 THEN H.ws(" at offset "); H.wh(adr - ORD(mod));  H.wsn("H.")
    ELSE H.ws(" on line "); H.wi(line); H.ws(" in "); H.ws(proc); H.wsn(".")
    END
  END
END HandleTrap;



(* ----------------------------- Initialisation ----------------------------- *)

PROCEDURE Init;
BEGIN
  Root := SYSTEM.VAL(Module, H.Preload.ImgHeader);
  Files.Init;
  ActCnt      := 0;
  PreloadNext := H.Preload.MadrPreload;
  H.SetTrapHandler(HandleTrap);
END Init;

BEGIN Init;
  StackOrg := SYSTEM.REG(4);
  Load(H.CmdModule, M);
  IF (res = 0) & (H.CmdCommand[0] # 0X) THEN
    P := ThisCommand(M, H.CmdCommand);
    IF res = 0 THEN P END
  END;

  (*LED(res);  REPEAT UNTIL FALSE*)  (*only if load fails*)
  IF res # 0 THEN
    H.ws("Load error: "); H.ws(Importing);
    IF    res = 1 THEN H.wsn(" module not found")
    ELSIF res = 2 THEN H.wsn(" bad version")
    ELSIF res = 3 THEN H.ws(" imports '"); H.ws(Imported); H.wsn(" with bad key");
    ELSIF res = 4 THEN H.wsn(" corrupted obj file")
    ELSIF res = 5 THEN H.ws(" command "); H.ws(H.CmdCommand); H.wsn(" not found")
    ELSIF res = 7 THEN H.wsn(" insufficient space")
    END;
    H.SetExitCode(res)
  END;
  H.Exit
END Modules.
