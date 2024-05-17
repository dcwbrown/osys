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
  importing*, imported*: ModuleName;
  PreloadExe:    Files.File;
  PreloadOffset: INTEGER;
  Load*:         PROCEDURE(name: ARRAY OF CHAR;  VAR newmod: Module);
  ActCnt*:       INTEGER;  (* Action count for scheduling garbage collection *)
  StackOrg*:     INTEGER;


PROCEDURE ThisFile(name: ARRAY OF CHAR): Files.File;
VAR i: INTEGER;  filename: ModuleName;
BEGIN i := 0;
  WHILE name[i] # 0X DO filename[i] := name[i];  INC(i) END;
    filename[i] := "."; filename[i+1] := "x"; filename[i+2] := "6"; filename[i+3] := "4"; filename[i+4] := 0X;
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


PROCEDURE LoadModule*(VAR F: Files.File; VAR fileoffset: INTEGER; VAR newmod: Module);
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

  Files.Set(R, F, filepos);
  H.ZeroFill(header);
  Files.ReadBytes(R, header, SYSTEM.SIZE(ModDesc));
  IF header.magic # "Oberon5" THEN
    res := 4
  ELSE
    name1     := header.name;
    importing := name1;
    key       := header.key;
    size      := (header.vars + header.varsize + 15) DIV 16 * 16;

    (* Load list of imported modules (immediately follows module header) *)
    Files.ReadString(R, impname);
    WHILE (impname[0] # 0X) & (res = 0) DO
      Files.ReadInt(R, impkey);
      Load(impname, impmod);
      import[nofimps] := impmod;
      importing := name1;
      IF res = 0 THEN
        IF impmod.key = impkey THEN INC(impmod.refcnt);  INC(nofimps)
        ELSE error(3, name1);  imported := impname
        END
      END;
      Files.ReadString(R, impname)
    END
  END;

  IF res = 0 THEN (*search for a hole in the list allocate and link*)
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
    mod.ptr     := header.ptr;
    mod.vars    := ORD(mod) + header.vars;
    mod.cmd     := ORD(mod) + header.cmd;

    (* Link imports *)

    Files.Set(R, F, filepos + header.imprefs);
    Files.ReadBytes(R, impcount, 4);
    FOR i := 1 TO impcount DO
      ReadVar(R, offset);  ReadVar(R, impno);  ReadVar(R, modno);
      LinkImport(ORD(mod), offset, impno, modno, import)
    END;

    (* Relocate pointer addresses *)
    p := ORD(mod) + mod.ptr;
    SYSTEM.GET(p, ptroff);
    WHILE ptroff >= 0 DO
      SYSTEM.PUT(p, mod.vars + ptroff);
      INC(p, 8);
      SYSTEM.GET(p, ptroff)
    END;

    (* Manage preloading *)
    IF fileoffset > 0 THEN  (* Advance preload state *)
      IF fileoffset + header.size < Files.Length(F) THEN
        INC(fileoffset, header.size)
      ELSE
        (* We've reached the end of the exe, close it before initialising the module. *)
        Files.CloseHostHandle(F);  F := NIL;
        fileoffset := 0;
      END
    END;

    (* Initialize module *)
    IF mod.init # 0 THEN
      body := SYSTEM.VAL(Command, ORD(mod) + mod.init);
      body
    END;
(*
  ELSIF res >= 3 THEN importing := name;
    WHILE nofimps > 0 DO DEC(nofimps);  DEC(import[nofimps].refcnt) END
*)
  END;
  newmod := mod
END LoadModule;


PROCEDURE LoadImpl(name: ARRAY OF CHAR;  VAR newmod: Module);
VAR mod: Module;  F: Files.File;  offset: INTEGER;
BEGIN
  res := 0;
  mod := H.Root;  WHILE (mod # NIL) & (name # mod.name) DO mod := mod.next END;
  IF (mod = NIL) & (PreloadOffset # 0) THEN
    WHILE (res = 0) & (PreloadExe # NIL) DO
      LoadModule(PreloadExe, PreloadOffset, mod);  ASSERT(res = 0)
    END;
    mod := H.Root;  WHILE (mod # NIL) & (name # mod.name) DO mod := mod.next END;
  END;
  IF mod = NIL THEN
    Check(name);
    IF res = 0 THEN F := ThisFile(name) ELSE F := NIL END;
    IF F = NIL THEN
      error(1, name)
    ELSE
      error(0, name);  (* No error yet *)
      offset := 0;
      LoadModule(F, offset, mod)
    END;
  END;
  newmod := mod
END LoadImpl;


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


(* ----------------------------- Initialisation ----------------------------- *)

PROCEDURE Init;
VAR
  fn16:  ARRAY H.MaxPath OF SYSTEM.CARD16;
  fn8:   ARRAY H.MaxPath OF CHAR;
BEGIN
  Files.Init;
  Load          := LoadImpl;
  PreloadExe    := NIL;
  PreloadOffset := 0;
  ActCnt        := 0;
  IF H.Preload.FileOfs # 0 THEN
    ASSERT(H.GetModuleFileNameW(0, SYSTEM.ADR(fn16), H.MaxPath) # 0);
    ASSERT(H.Utf16ToUtf8(fn16, fn8) # 0);
    PreloadExe    := Files.Old(fn8);
    PreloadOffset := H.Preload.FileOfs;
    ASSERT(PreloadExe # NIL)
  END
END Init;

BEGIN Init;
  StackOrg := SYSTEM.REG(4);
  Load(H.CmdModule, M);
  IF (res = 0) & (H.CmdCommand[0] # 0X) THEN
    P := ThisCommand(M, H.CmdCommand);
    IF res = 0 THEN P END
  END;

  ASSERT(res = 0);
  (*
  IF res # 0 THEN
    H.ws("Load error: "); H.ws(importing);
    IF    res = 1 THEN H.wsn(" module not found")
    ELSIF res = 2 THEN H.wsn(" bad version")
    ELSIF res = 3 THEN H.ws(" imports '"); H.ws(imported); H.wsn(" with bad key");
    ELSIF res = 4 THEN H.wsn(" corrupted obj file")
    ELSIF res = 5 THEN H.ws(" command "); H.ws(H.CmdCommand); H.wsn(" not found")
    ELSIF res = 7 THEN H.wsn(" insufficient space")
    END;
    H.SetExitCode(res)
  END;
  *)

  H.ExitToHost
  (*LED(res);  REPEAT UNTIL FALSE*)  (*only if load fails*)
END Modules.
