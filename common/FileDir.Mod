MODULE FileDir;   (* DCWB 2024-04-01 *)

IMPORT SYSTEM, H := Host, Kernel;

CONST
  FnLength* = H.MaxPath;

TYPE
  FileName*     = H.FileName;
  WideFileName  = ARRAY 260 OF SYSTEM.CARD16;
  FileInfo*     = H.FileInfo;
  EntryHandler* = H.FileEntryHandler;

PROCEDURE Enumerate*(prefix: ARRAY OF CHAR; proc: EntryHandler);
BEGIN H.FileEnumerate(prefix, proc) END Enumerate;

END FileDir.
