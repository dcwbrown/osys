MODULE Modules;  (*Link and load on RISC;  NW 20.10.2013 / 8.1.2019*)

IMPORT SYSTEM, H := WinHost, Files;

TYPE
  (*Module*     = POINTER TO ModDesc;*)
  Module*     = H.Module;
  Command*    = PROCEDURE;
  ModuleName* = ARRAY 32 OF CHAR;
  ModDesc*    = H.ModuleDesc;

VAR
  M:             Module;     (* Loaded module 'Oberon' *)
  P:             Command;
  res*:          INTEGER;
  importing*:    ModuleName;
  imported*:     ModuleName;
  PreloadExe:    Files.File;
  PreloadOffset: INTEGER;
  Load*:         PROCEDURE(name: ARRAY OF CHAR;  VAR newmod: Module);


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


PROCEDURE ReadVar(VAR r: Files.Rider;  VAR var: ARRAY OF BYTE);
BEGIN Files.ReadBytes(r, var, LEN(var)) END ReadVar;


PROCEDURE LoadModule*(F: Files.File; VAR fileoffset: INTEGER; VAR newmod: Module);
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
  filepos:   INTEGER;
  nofimps:   INTEGER;
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
  ptroff:    INTEGER;
  impcount:  SYSTEM.CARD32;
  offset:    SYSTEM.CARD32;
  impno:     SYSTEM.CARD16;
  modno:     SYSTEM.CARD16;
  body:      Command;
BEGIN
  nofimps    := 0;
  filepos    := fileoffset;
  fileoffset := 0;

  Files.Set(R, F, filepos);
  H.ZeroFill(header);
  Files.ReadBytes(R, header, SYSTEM.SIZE(ModDesc));
  IF header.magic # "Oberon5" THEN
    H.wsn("Corrupt module header.");
    H.DumpMem(2, SYSTEM.ADR(header), 0, SYSTEM.SIZE(ModDesc));
    res := 4
  ELSE
    H.ws("LoadModule loading '"); H.ws(header.name);
    H.ws("' from offset ");  H.wh(filepos);  H.wsn("H.");
    name1      := header.name;
    importing  := name1;
    key        := header.key;
    size       := (header.vars + header.varsize + 15) DIV 16 * 16;
    fileoffset := filepos + header.size;

    (* Load list of imported modules *)
    Files.Set(R, F, filepos + header.vars);
    Files.ReadString(R, impname);
    WHILE (impname[0] # 0X) & (res = 0) DO
      Files.ReadInt(R, impkey);
      H.ws("  recursive load importing module "); H.ws(impname); H.wsn(".");
      Load(impname, impmod);
      IF res # 0 THEN
        H.ws("  recursive load importing module "); H.ws(impname);
        H.ws(" failed, res "); H.wi(res); H.wsn(".");
      ELSE
        H.ws("  recursive load importing module "); H.ws(impname); H.wsn(" succeeded.");
      END;
      import[nofimps] := impmod;
      importing := name1;
      IF res = 0 THEN
        IF impmod.key = impkey THEN INC(impmod.refcnt);  INC(nofimps)
        ELSE error(3, name1);  imported := impname;
        H.ws("  ... but key mismatch: expected "); H.wh(impkey);
        H.ws("H, got "); H.wh(impmod.key); H.wsn("H.");
        END
      END;
      Files.ReadString(R, impname)
    END;
    imptabpos := (Files.Pos(R) + 15) DIV 16 * 16;

    H.ws("LoadModule loading '"); H.ws(header.name);
    H.ws("' recursive import loading complete, res "); H.wi(res); H.wsn(".");
  END;

  H.ws("LoadModule loading '"); H.ws(header.name);
  H.ws("' line 171, res "); H.wi(res); H.wsn(".");

  IF res = 0 THEN (*search for a hole in the list allocate and link*)
    H.ws("LoadModule loading '"); H.ws(header.name); H.wsn("' allocating memory.");
    mod := H.Root;
    WHILE (mod # NIL) & ~((mod.name[0] = 0X) & (mod.size >= size)) DO mod := mod.next END;
    IF mod = NIL THEN (*no large enough hole was found*)
      H.Allocate(size, p, allocsize);
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
    (* Read static memory, including code, type descriptors, strings and pointers *)
    H.ws("LoadModule loading '"); H.ws(header.name); H.wsn("' reading static memory.");
    INC(p, SYSTEM.SIZE(ModDesc));  (* Skip header *)
    Files.Set(R, F, filepos + SYSTEM.SIZE(ModDesc));
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
    H.ws("LoadModule loading '"); H.ws(header.name); H.wsn("' linking imports.");
    H.ws("  imptabpos "); H.wh(imptabpos); H.wsn("H.");
    Files.Set(R, F, imptabpos);
    Files.ReadBytes(R, impcount, 4);
    H.ws("  "); H.wi(impcount); H.wsn(" imports.");
    FOR i := 1 TO impcount DO
      ReadVar(R, offset);  ReadVar(R, impno);  ReadVar(R, modno);
      LinkImport(ORD(mod), offset, impno, modno, import)
    END;

    (* Relocate pointer addresses *)
    H.ws("LoadModule loading '"); H.ws(header.name); H.wsn("' relocating pointer addresses.");
    p := mod.ptr;
    SYSTEM.GET(p, ptroff);
    WHILE ptroff >= 0 DO
      SYSTEM.PUT(p, mod.vars + ptroff);
      INC(p, 8);
      SYSTEM.GET(p, ptroff)
    END;

    H.ws("LoadModule loaded '"); H.ws(mod.name);
    H.ws("', about to call initialisation, ORD(mod) ");
    H.wh(ORD(mod)); H.ws("H, mod.init ");
    H.wh(mod.init); H.wsn("H.");

    (* Initialize module *)
    IF mod.init # 0 THEN
      body := SYSTEM.VAL(Command, ORD(mod) + mod.init);
      H.ws("  initialisation at "); H.wh(ORD(body)); H.wsn("h:");
      H.DumpMem(2, ORD(body), ORD(body), 64);
      body
    END;
    H.ws("LoadModule '"); H.ws(mod.name);
    H.wsn("' initialisation complete.");
(*
  ELSIF res >= 3 THEN importing := name;
    WHILE nofimps > 0 DO DEC(nofimps);  DEC(import[nofimps].refcnt) END
*)
  END;
  newmod := mod
END LoadModule;


PROCEDURE Preload;
VAR mod: Module;
BEGIN
  IF (res = 0) & (PreloadOffset < Files.Length(PreloadExe)) THEN
    WHILE (res = 0)
        & (PreloadOffset > 0)
        & (PreloadOffset < Files.Length(PreloadExe)) DO
      H.ws("* Preload from offset "); H.wh(PreloadOffset); H.wsn("H.");
      LoadModule(PreloadExe, PreloadOffset, mod);
      ASSERT(res = 0);
      H.ws("* Preload of offset "); H.wh(PreloadOffset);
      H.ws("H '"); H.ws(mod.name); H.wsn("' complete.");
    END;
    H.wsn("Preload finished. Modules loaded:");
    mod := H.Root;
    WHILE mod # NIL DO
      H.ws("  "); H.ws(mod.name); H.wn;
      mod := mod.next
    END;
    H.ws("  PreloadOffset "); H.wh(PreloadOffset);
    H.ws("H, exe length "); H.wh(Files.Length(PreloadExe)); H.wsn("H.");
    IF PreloadOffset # 0 THEN
      ASSERT(PreloadOffset = Files.Length(PreloadExe));
      PreloadOffset := 0  (* Preloading complete. *)
    END
  END
END Preload;


PROCEDURE LoadImpl(name: ARRAY OF CHAR;  VAR newmod: Module);
VAR mod: Module;  F: Files.File;  offset: INTEGER;
BEGIN
  res := 0;
  error(0, name);  (* No error yet *)
  mod := H.Root;
  WHILE (mod # NIL) & (name # mod.name) DO mod := mod.next END;
  IF (mod = NIL) & (PreloadOffset # 0) THEN
    H.ws("Load("); H.ws(name); H.wsn(") at entry, not already loaded, running preload.");
    Preload;
    mod := H.Root;
    WHILE (mod # NIL) & (name # mod.name) DO mod := mod.next END;
    IF mod = NIL THEN
      H.ws("Load("); H.ws(name); H.wsn(") following preload, still not loaded.")
    ELSE
      H.ws("Load("); H.ws(name); H.wsn(") following preload, now found loaded.")
    END
  END;
  IF mod = NIL THEN
    H.ws("Load("); H.ws(name); H.wsn("), loading from file.");
    Check(name);
    IF res = 0 THEN F := ThisFile(name) ELSE F := NIL END;
    IF F = NIL THEN
      error(1, name)
    ELSE
      offset := 0;  LoadModule(F, offset, mod)
    END;
    H.ws("Load("); H.ws(name); H.wsn("), complete.");
  END;
  newmod := mod
END LoadImpl;



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


PROCEDURE InitPreload;
VAR
  fn16:  ARRAY H.MaxPath OF SYSTEM.CARD16;
  fn8:   ARRAY H.MaxPath OF CHAR;
  res:   INTEGER;
BEGIN
  PreloadOffset := H.Preload.FileOfs;
  IF PreloadOffset # 0 THEN
    res := H.GetModuleFileNameW(0, SYSTEM.ADR(fn16), H.MaxPath);  ASSERT(res # 0);
    res := H.Utf16ToUtf8(fn16, fn8);
    PreloadExe := Files.Old(fn8);
    ASSERT(PreloadExe # NIL)
  ELSE
    PreloadExe := NIL
  END
END InitPreload;


BEGIN
  Load := LoadImpl;
  Files.Init;
  InitPreload;

  IF H.CmdModule = "Modules" THEN  (* Simulate default behaviour of PO2013 *)
    Load("Oberon", M);
  ELSE
    Load(H.CmdModule, M);
    IF (res = 0) & (H.CmdCommand[0] # 0X) THEN
      P := ThisCommand(M, H.CmdCommand);
      IF res = 0 THEN P END
    END
  END;

  IF res # 0 THEN
    H.ws("** Modules initialisation: load error: '"); H.ws(importing);
    IF    res = 1 THEN H.wsn("' module not found")
    ELSIF res = 2 THEN H.wsn("' bad version")
    ELSIF res = 3 THEN H.ws("' imports '"); H.ws(imported); H.wsn("' with bad key");
    ELSIF res = 4 THEN H.wsn("' corrupted obj file")
    ELSIF res = 5 THEN H.ws("' command '"); H.ws(H.CmdCommand); H.wsn("' not found")
    ELSIF res = 7 THEN H.wsn("' insufficient space")
    END
  END;

  H.Exit
(*
  Load("Oberon", M);
  LED(res);  REPEAT UNTIL FALSE  (*only if load fails*)
*)
END Modules.
