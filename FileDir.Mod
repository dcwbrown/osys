MODULE FileDir;   (* DCWB 2024-04-01 *)

IMPORT SYSTEM, H := WinHost, Kernel;

CONST
  FnLength* = H.MaxPath;

TYPE
  FileName* = ARRAY FnLength OF CHAR;
  WinFName  = ARRAY 260 OF SYSTEM.CARD16;
  FileInfo* = RECORD
    name*:   FileName;
    date*:   INTEGER;
    length*: INTEGER;
  END;
  EntryHandler* = PROCEDURE (info: FileInfo; VAR continue: BOOLEAN);

PROCEDURE wsu(s: ARRAY OF SYSTEM.CARD16);
VAR buf: ARRAY 100 OF CHAR;  len: INTEGER;
BEGIN len := H.Utf16ToUtf8(s, buf);  H.ws(buf) END wsu;

PROCEDURE IsSelfOrParent(dir: ARRAY OF SYSTEM.CARD16): BOOLEAN;
VAR i, j: INTEGER;  result: BOOLEAN;
BEGIN
  result := FALSE;
  i := 0;  j := 0;
  WHILE dir[i] # 0 DO
    IF (dir[i] = ORD("/")) OR (dir[i] = 5CH) THEN j := i + 1 END;
    INC(i)
  END;
  IF dir[j] = ORD(".") THEN
    INC(j);
    IF (dir[j] = 0) OR (dir[j] = ORD(".")) & (dir[j+1] = 0) THEN
      result := TRUE
    END
  END
RETURN result END IsSelfOrParent;

PROCEDURE WinEnum(name: WinFName; proc: EntryHandler; VAR cont: BOOLEAN);
VAR
  search: WinFName;
  i, j:   INTEGER;
  len:    INTEGER;
  hfind:  INTEGER;
  inf:    FileInfo;
  data:   RECORD-
    attribs:   SYSTEM.CARD32;
    creationl: SYSTEM.CARD32;
    creationh: SYSTEM.CARD32;
    accessl:   SYSTEM.CARD32;
    accessh:   SYSTEM.CARD32;
    writel:    SYSTEM.CARD32;
    writeh:    SYSTEM.CARD32;
    sizeh:     SYSTEM.CARD32;
    sizel:     SYSTEM.CARD32;
    res0:      SYSTEM.CARD32;
    res1:      SYSTEM.CARD32;
    name:      ARRAY 260 OF SYSTEM.CARD16;
    dum1:      ARRAY  14 OF SYSTEM.CARD16;  (* alternate file name unused *)
    dum2:      SYSTEM.CARD32;
    dum3:      SYSTEM.CARD32;
    dum4:      SYSTEM.CARD16;
    dum5:      SYSTEM.CARD32   (* Not set by windows - round up to 260H byes *)
  END;
BEGIN
  (*H.ws("WinEnum, name: '"); wsu(name); H.wsn("'.");*)
  IF ~IsSelfOrParent(name) THEN
    len := 0;
    WHILE name[len] # 0 DO search[len] := name[len]; INC(len) END;
    IF len > 0 THEN search[len] := ORD("/"); INC(len) END;
    i := len;
    search[i] := ORD("*");  search[i+1] := 0;
    (*H.ws("FindFirstFileExW('"); wsu(search); H.wsn("'.");*)
    hfind := H.FindFirstFileExW(SYSTEM.ADR(search), 1, SYSTEM.ADR(data), 0, 0, 0);
    cont := TRUE;
    WHILE hfind # 0 DO
      IF 4 IN SYSTEM.VAL(SET, ORD(data.attribs)) THEN  (* recurse into directory *)
        i := 0;  j := len;
        WHILE data.name[i] # 0 DO
          search[j] := data.name[i];  INC(i);  INC(j)
        END;
        search[j] := 0;
        (*H.ws("Recursively searching '"); wsu(search); H.wsn("'.");*)
        WinEnum(search, proc, cont)
      ELSE
        (* Copy full name including directory into inf *)
        i := 0; j := 0;
        WHILE i < len DO H.PutUtf8(search[i], inf.name, j);  INC(i) END;
        i := 0;
        WHILE data.name[i] # 0 DO H.PutUtf8(data.name[i], inf.name, j);  INC(i) END;
        inf.name[j] := 0X;
        (*H.ws("Found '"); H.ws(inf.name); H.wsn("'.");*)
        (*H.DumpMem(0, SYSTEM.ADR(data), SYSTEM.ADR(data), 260H);*)
        inf.date   := H.TimeAsClock(LSL(data.writeh, 32) + data.writel);
        inf.length := LSL(data.sizeh,  32) + data.sizel;
        proc(inf, cont)
      END;
      IF H.FindNextFileW(hfind, SYSTEM.ADR(data)) = 0 THEN
        i := H.FindClose(hfind);
        hfind := 0
      END;
    END
  END
END WinEnum;


PROCEDURE Enumerate*(prefix: ARRAY OF CHAR; proc: EntryHandler);
VAR
  len:  INTEGER;
  wfn:  WinFName;
  inf:  FileInfo;
  data: RECORD-
    attribs:   SYSTEM.CARD32;
    creationl: SYSTEM.CARD32;
    creationh: SYSTEM.CARD32;
    accessl:   SYSTEM.CARD32;
    accessh:   SYSTEM.CARD32;
    writel:    SYSTEM.CARD32;
    writeh:    SYSTEM.CARD32;
    sizeh:     SYSTEM.CARD32;
    sizel:     SYSTEM.CARD32;
    res0:      SYSTEM.CARD32;
    res1:      SYSTEM.CARD32;
    name:      WinFName;
    dum1:      ARRAY  14 OF SYSTEM.CARD16;  (* alternate file name unused *)
    dum2:      SYSTEM.CARD32;
    dum3:      SYSTEM.CARD32;
    dum4:      SYSTEM.CARD16;
    dum5:      SYSTEM.CARD32   (* Not set by windows - round up to 260H byes *)
  END;
  hfind: INTEGER;
  cont:  BOOLEAN;
BEGIN
  len  := H.Utf8ToUtf16("", wfn);
  WinEnum(wfn, proc, cont)
  (*
  hfind := H.FindFirstFileExW(SYSTEM.ADR(wfn), 1, SYSTEM.ADR(data), 0, 0, 0);
  cont := TRUE;
  WHILE cont & (hfind # 0) DO
    IF ~(4 IN SYSTEM.VAL(SET, ORD(data.attribs))) THEN  (* Skip directories *)
      len := H.Utf16ToUtf8(data.name, inf.name);
      H.ws("Found '"); H.ws(inf.name); H.wsn("'.");
      (*H.DumpMem(0, SYSTEM.ADR(data), SYSTEM.ADR(data), 260H);*)
      inf.date   := H.TimeAsClock(LSL(data.writeh, 32) + data.writel);
      inf.length := LSL(data.sizeh,  32) + data.sizel;
      proc(inf, cont)
    END;
    IF H.FindNextFileW(hfind, SYSTEM.ADR(data)) = 0 THEN
      ASSERT(H.FindClose(hfind) # 0);
      hfind := 0
    END;
  END
  *)
END Enumerate;


END FileDir.
