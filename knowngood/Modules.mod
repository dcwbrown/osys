MODULE Modules;  (*Link and load on RISC;  NW 20.10.2013 / 8.1.2019*)

IMPORT SYSTEM, H := WinHost, Files;

TYPE
  (*Module*     = POINTER TO ModDesc;*)
  Module*     = H.Module;
  Command*    = PROCEDURE;
  ModuleName* = ARRAY 32 OF CHAR;
  ModDesc*    = H.ModuleDesc;

VAR
  M:          Module;     (* Loaded module 'Oberon' *)
  res*:       INTEGER;
  importing*: ModuleName;
  imported*:  ModuleName;


PROCEDURE ThisFile(name: ARRAY OF CHAR): Files.File;
VAR i: INTEGER;  filename: ModuleName;
BEGIN i := 0;
  WHILE name[i] # 0X DO filename[i] := name[i];  INC(i) END;
  filename[i]   := ".";  filename[i+1] := "x";  filename[i+2] := "6";
  filename[i+3] := "4";  filename[i+4] := 0X;
  RETURN Files.Old(filename)
END ThisFile;


PROCEDURE error(n: INTEGER;  name: ARRAY OF CHAR);
BEGIN res := n;  importing := name
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


PROCEDURE ReadVar(VAR r: Files.Rider;  VAR var: ARRAY OF BYTE);
BEGIN Files.ReadBytes(r, var, LEN(var)) END ReadVar;


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
  mod:       Module;
  nofimps:   INTEGER;
  F:         Files.File;
  R:         Files.Rider;
  header:    ModDesc;
  name1:     ModuleName;
  key:       INTEGER;
  size:      INTEGER;
  impname:   ModuleName;
  import:    ARRAY 16 OF Module;
  impmod:    Module;
  impkey:    INTEGER;
  imptabpos: INTEGER;
  p:         INTEGER;
  allocsize: INTEGER;
  loadlen:   INTEGER;
  i:         INTEGER;
  impcount:  SYSTEM.CARD32;
  offset:    SYSTEM.CARD32;
  impno:     SYSTEM.CARD16;
  modno:     SYSTEM.CARD16;
  body:      Command;
(*
  i, n, key, impkey, mno, nofimps, size: INTEGER;
  p, u, v, w: INTEGER;  (*addresses*)
  ch: CHAR;
  body: Command;
  fixorgP, fixorgD, fixorgT: INTEGER;
  disp, adr, inst, pno, vno, dest, offset: INTEGER;
*)
BEGIN
  mod := H.Root;  error(0, name);  nofimps := 0;
  res := 0;
  (*nofimps := 0;*)
  WHILE (mod # NIL) & (name # mod.name) DO mod := mod.next END;

  IF mod = NIL THEN (*load*)
    Check(name);
    IF res = 0 THEN F := ThisFile(name) ELSE F := NIL END;
    IF F # NIL THEN
      (*
      H.ws("* Loading "); H.ws(name);
      H.ws(" from file "); H.ws(F.name);  H.wsn(" *");
      *)
      Files.Set(R, F, 0);
      Files.ReadBytes(R, header, SYSTEM.SIZE(ModDesc));
      name1     := header.name;
      key       := header.key;
      size      := (header.nimports + header.nvarsize + 15) DIV 16 * 16;
      importing := name1;
      (*
      H.wsn("File header:");
      H.ws("  name: "); H.ws(name1); H.wsn(".");
      H.ws("  key:  "); H.wh(key);   H.wsn("H.");
      H.ws("  size: "); H.wh(size);  H.wsn("H.");
      *)
      (* Load list of imported modules *)
      Files.Set(R, F, header.nimports);
      Files.ReadString(R, impname);
      WHILE (impname[0] # 0X) & (res = 0) DO
        Files.ReadInt(R, impkey);
        (*H.ws(name); H.ws(" importing "); H.ws(impname); H.wsn(".");*)
        Load(impname, impmod);
        import[nofimps] := impmod;
        importing := name1;
        IF res = 0 THEN
          IF impmod.key = impkey THEN INC(impmod.refcnt);  INC(nofimps)
          ELSE error(3, name1);  imported := impname
          END
        END;
        Files.ReadString(R, impname)
      END;
      imptabpos := (Files.Pos(R) + 15) DIV 16 * 16;
    ELSE
      H.ws("Couldn't find compiled binary .X64 file for module "); H.ws(name); H.wsn(".");
      error(1, name)
    END;

    IF res = 0 THEN (*search for a hole in the list allocate and link*)
      mod := H.Root;
      WHILE (mod # NIL) & ~((mod.name[0] = 0X) & (mod.size >= size)) DO mod := mod.next END;
      IF mod = NIL THEN (*no large enough hole was found*)
        H.Allocate(size, p, allocsize);
        (*
        H.ws("H.Allocate requested "); H.wh(size);
        H.ws("H, got "); H.wh(allocsize);
        H.ws("H bytes at "); H.wh(p); H.wsn("H.");
        *)
        IF allocsize # 0 THEN
          mod := SYSTEM.VAL(Module, p);
          H.ZeroFill(mod^);
          mod.size := allocsize;
          mod.num  := H.Root.num + 1;
          mod.next := H.Root;
          mod.nimports := 0;  (* Nothing loaded yet - not a candidate for LocateModule *)
          H.SetRoot(mod);
        ELSE error(7, name1)
        END
      ELSE (*fill hole*) p := ORD(mod)
      END
    END;

    IF res = 0 THEN
      (* Read static memory, including code, type descriptors, strings and pointers *)
      IF res = 0 THEN (*read file*)
        INC(p, SYSTEM.SIZE(ModDesc));  (* Skip header *)
        Files.Set(R, F, SYSTEM.SIZE(ModDesc));
        loadlen := header.nimports - SYSTEM.SIZE(ModDesc);
        ASSERT(loadlen MOD 8 = 0);
        WHILE loadlen > 0 DO
          Files.ReadInt(R, i);  DEC(loadlen, 8);
          SYSTEM.PUT(p, i);     INC(p, 8)
        END;
        mod.name := name;  mod.key := key;  mod.refcnt := 0;
      END;

      mod.vars     := ORD(mod) + header.nimports;
      mod.cmd      := ORD(mod) + header.cmd;
      mod.nexports := header.nexports;
      mod.nlines   := header.nlines;

      H.ws("* Loaded ");             H.ws(mod.name);
      H.ws(" at ");                  H.wh(ORD(mod));
      H.ws("H, code ");              H.wh(header.nimports);
      H.ws("H bytes, data ");        H.wh(header.nvarsize);
      H.ws("H bytes, loaded size "); H.wh(mod.size);
      H.wsn("H.");

      (* Link imports *)
      Files.Set(R, F, imptabpos);
      Files.ReadBytes(R, impcount, 4);
      (*H.ws("Import count: "); H.wi(impcount); H.wsn(".");*)
      FOR i := 1 TO impcount DO
        ReadVar(R, offset);  ReadVar(R, impno);  ReadVar(R, modno);
        H.LinkImport(ORD(mod), offset, impno, modno, import)
      END;

      (* Relocate pointer addresses *)
      mod.ptr  := ORD(mod) + header.ptr;
      H.RelocatePointerAddresses(mod.ptr, mod.vars);

      body := SYSTEM.VAL(Command, ORD(mod) + header.ninitcode);
      body   (*initialize module*)
(*
    ELSIF res >= 3 THEN importing := name;
      WHILE nofimps > 0 DO DEC(nofimps);  DEC(import[nofimps].refcnt) END
*)
    END
  END;
  newmod := mod
END Load;

PROCEDURE ThisCommand*(mod: Module;  name: ARRAY OF CHAR): Command;
VAR k, adr, w: INTEGER;  ch: CHAR; offset: SYSTEM.CARD32;
    s: ARRAY 32 OF CHAR;
BEGIN
  res := 5;  w := 0;
  IF mod # NIL THEN
    adr := mod.cmd;  SYSTEM.GET(adr, ch);
    WHILE (ch # 0X) & (res # 0) DO
      k := 0;  (*read command name*)
      REPEAT s[k] := ch;  INC(k);  INC(adr);  SYSTEM.GET(adr, ch) UNTIL ch = 0X;
      s[k] := 0X;
      INC(adr); (*REPEAT INC(adr) UNTIL adr MOD 4 = 0;*)
      SYSTEM.GET(adr, offset);  INC(adr, 4);
      IF s = name THEN
        res := 0;  w := ORD(mod) + offset
      ELSE
        SYSTEM.GET(adr, ch)
      END
    END
  END
  RETURN SYSTEM.VAL(Command, w)
END ThisCommand;

PROCEDURE Free*(name: ARRAY OF CHAR);
VAR mod, imp: Module;  p, q: INTEGER;
BEGIN mod := H.Root;  res := 0;
  WHILE (mod # NIL) & (mod.name # name) DO mod := mod.next END;
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

PROCEDURE Init*;
BEGIN
(*
  Files.Init;
  MTOrg := SYSTEM.REG(MT);
  SYSTEM.GET(16, AllocPtr);
  SYSTEM.GET(20, root);
  SYSTEM.GET(24, limit);
  DEC(limit, 8000H)
*)
END Init;

BEGIN Init;
  IF 63 IN H.LoadFlags THEN
    H.wsn("Reached Modules.");
    Load("Oberon", M);
  END;
(*
  Load("Oberon", M);
  LED(res);  REPEAT UNTIL FALSE  (*only if load fails*)
*)
END Modules.
