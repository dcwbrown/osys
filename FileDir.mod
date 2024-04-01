MODULE FileDir;   (* DCWB 2024-04-01 *)

IMPORT SYSTEM, H := WinHost, Kernel;

CONST
  FnLength* = H.MaxPath;

TYPE
  FileName* = ARRAY FnLength OF CHAR;
  FileInfo* = RECORD
    name*:   FileName;
    date*:   INTEGER;
    length*: INTEGER;
  END;
  EntryHandler* = PROCEDURE (info: FileInfo; VAR continue: BOOLEAN);

PROCEDURE Enumerate*(prefix: ARRAY OF CHAR; proc: EntryHandler);
BEGIN

END Enumerate;


END FileDir.
