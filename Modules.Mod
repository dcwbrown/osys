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
  P:          Command;
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
BEGIN
  mod := H.Root;  error(0, name);  nofimps := 0;
  res := 0;
  (*nofimps := 0;*)
  WHILE (mod # NIL) & (name # mod.name) DO mod := mod.next END;

  IF mod = NIL THEN (*load*)
    (*H.ws("Modules.Load: loading '"); H.ws(name); H.wsn("'.");*)
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
      size      := (header.vars + header.varsize + 15) DIV 16 * 16;
      importing := name1;
      (*
      H.wsn("File header:");
      H.ws("  name: "); H.ws(name1); H.wsn(".");
      H.ws("  key:  "); H.wh(key);   H.wsn("H.");
      H.ws("  size: "); H.wh(size);  H.wsn("H.");
      *)
      (* Load list of imported modules *)
      Files.Set(R, F, header.vars);
      Files.ReadString(R, impname);
      WHILE (impname[0] # 0X) & (res = 0) DO
        Files.ReadInt(R, impkey);
        (*H.ws(name); H.ws(" importing "); H.ws(impname); H.wsn(".");*)
        Load(impname, impmod);
        import[nofimps] := impmod;
        importing := name1;
        IF res = 0 THEN
          IF impmod.key = impkey THEN INC(impmod.refcnt);  INC(nofimps)
          ELSE
            H.ws("Modules.Load recursive load of imported module ");
            H.ws(impname); H.wsn(" has mismatched key.");
            error(3, name1);  imported := impname
          END
        ELSE
          H.ws("Modules.Load recursive load of imported module ");
          H.ws(impname); H.wsn(" failed.");
        END;
        Files.ReadString(R, impname)
      END;
      imptabpos := (Files.Pos(R) + 15) DIV 16 * 16;
    ELSE
      H.ws("Couldn't find compiled binary .X64 file for module '"); H.ws(name); H.wsn("'.");
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
          mod.vars := 0;  (* Nothing loaded yet - not a candidate for LocateModule *)
          H.SetRoot(mod);
        ELSE error(7, name1)
        END
      ELSE (*fill hole*) p := ORD(mod)
      END
    END;

    IF res = 0 THEN
      (*H.ws("Modules.Load "); H.ws(name); H.wsn(" reading static memory.");*)
      (* Read static memory, including code, type descriptors, strings and pointers *)
      INC(p, SYSTEM.SIZE(ModDesc));  (* Skip header *)
      Files.Set(R, F, SYSTEM.SIZE(ModDesc));
      loadlen := header.vars - SYSTEM.SIZE(ModDesc);
      ASSERT(loadlen MOD 8 = 0);
      WHILE loadlen > 0 DO
        Files.ReadInt(R, i);  DEC(loadlen, 8);
        SYSTEM.PUT(p, i);     INC(p, 8)
      END;
      mod.refcnt  := 0;
      mod.name    := header.name;
      mod.key     := header.key;
      mod.init    := header.init;
      mod.exports := header.exports;
      mod.lines   := header.lines;
      mod.varsize := header.varsize;
      mod.vars    := ORD(mod) + header.vars;
      mod.cmd     := ORD(mod) + header.cmd;
      mod.ptr     := ORD(mod) + header.ptr;

      (* Link imports *)
      Files.Set(R, F, imptabpos);
      Files.ReadBytes(R, impcount, 4);
      (*H.ws("Import count: "); H.wi(impcount); H.wsn(".");*)
      FOR i := 1 TO impcount DO
        ReadVar(R, offset);  ReadVar(R, impno);  ReadVar(R, modno);
        H.LinkImport(ORD(mod), offset, impno, modno, import)
      END;

      (* Relocate pointer addresses *)
      H.RelocatePointerAddresses(mod.ptr, mod.vars);

      (*
      H.ws("* Loaded ");             H.ws(mod.name);
      H.ws(" at ");                  H.wh(ORD(mod));
      H.ws("H, code ");              H.wh(header.vars);
      H.ws("H bytes, data ");        H.wh(header.varsize);
      H.ws("H bytes, loaded size "); H.wh(mod.size);
      H.ws("H, lines at ");          H.wh(mod.lines);
      H.wsn("H.");
      *)

      body := SYSTEM.VAL(Command, ORD(mod) + mod.init);
      (*
      H.ws("Initialising "); H.ws(mod.name);
      H.ws(" entry point offset "); H.wh(mod.init);
      H.ws("H at "); H.wh(ORD(body)); H.wsn("H.");
      *)
      body;   (*initialize module*)
(*
    ELSIF res >= 3 THEN importing := name;
      WHILE nofimps > 0 DO DEC(nofimps);  DEC(import[nofimps].refcnt) END
*)
    END;
    (*
    H.ws("Modules.Load "); H.ws(name);
    IF mod = NIL THEN H.wsn(" failed.") ELSE H.wsn(" succeeded.") END;
    *)
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

BEGIN
  Init;
  (*H.ws("Modules initialisation, LoadFlags: "); H.wh(ORD(H.Preload.LoadFlags)); H.wsn("H.");*)
  IF H.CmdCommand[0] # 0X THEN
    (*
    H.ws("Modules initialisation. H.CmdModule '"); H.ws(H.CmdModule);
    H.ws("', H.CmdCommand '"); H.ws(H.CmdCommand); H.wsn("'.");
    *)
    Load(H.CmdModule, M);
    P := ThisCommand(M, H.CmdCommand);
    IF res = 0 THEN P END
  ELSE
    Load("Oberon", M)
  END;
  IF M = NIL THEN
    H.wsn("**** Load failed. ****");
    H.ws("**** Modules init load error: "); H.ws(importing);
    IF    res = 1 THEN H.wsn(" module not found")
    ELSIF res = 2 THEN H.wsn(" bad version")
    ELSIF res = 3 THEN H.wsn(" imports ");
      H.ws(imported); H.wsn(" with bad key");
    ELSIF res = 4 THEN H.wsn(" corrupted obj file")
    ELSIF res = 5 THEN H.wsn(" command not found")
    ELSIF res = 7 THEN H.wsn(" insufficient space")
    END
  END;
  (*H.wsn("**** Modules ExitProcess(0) ****");*)
  H.Exit
(*
  Load("Oberon", M);
  LED(res);  REPEAT UNTIL FALSE  (*only if load fails*)
*)
END Modules.
