MODULE Oberon; (* DCWB 06.04.2024  Minimal Oberon.Mod for console applications *)

IMPORT SYSTEM, H := WinHost, Kernel, Files, Texts, Modules;


VAR
  Log*: Texts.Text;

  Par*: Texts.Params;

  Mod:    Modules.Module;
  Cmd:    ARRAY 100 OF CHAR;
  res:    INTEGER;


PROCEDURE GetSelection* (VAR text: Texts.Text; VAR beg, end, time: INTEGER);
BEGIN  beg := 0;  end := 0;  time := -1  END GetSelection;

PROCEDURE Collect* (count: INTEGER);
BEGIN Kernel.Collect(count) END Collect;

PROCEDURE CallCommand;
VAR S: Texts.Scanner;  P: Modules.Command;
BEGIN
  Texts.OpenScanner(S, Par.text, Par.pos);  Texts.Scan(S);
  IF S.class = Texts.Name THEN
    Par.pos := Texts.Pos(S);
    (*H.ws("Call command '"); H.ws(S.s); H.wsn("'.");*)
    P := Modules.ThisCommand(Mod, S.s);
    IF Modules.res = 0 THEN P END
  END;
END CallCommand;

BEGIN
  (*H.wsn("** Console.Oberon initialising.");*)
  Log := Texts.Log;
  Par := Texts.Par;
  IF H.CmdCommand[0] = 0X THEN
    IF H.NewLoad IN H.Preload.LoadFlags THEN
      (*H.ws("**** Console.Oberon loading "); H.ws(H.Preload.LoadMod); H.wsn(" ****");*)
      Modules.Load(H.Preload.LoadMod, Mod);
      res := Modules.res;
      IF res = 0 THEN
        (*
        H.ws("Console.Oberon. Cmd '"); H.ws(Cmd);
        H.ws("', H.Preload.LoadMod '"); H.ws(H.Preload.LoadMod); H.wsn("'.");
        *)
        IF Cmd = H.Preload.LoadMod THEN CallCommand; res := Modules.res END
      END;
      IF res # 0 THEN
        H.wsn("**** Load failed. ****");
        H.ws("**** Console.Oberon init load error: "); H.ws(Modules.importing);
        IF    res = 1 THEN H.wsn(" module not found")
        ELSIF res = 2 THEN H.wsn(" bad version")
        ELSIF res = 3 THEN H.ws(" imports ");
                                   H.ws(Modules.imported);
                                   H.wsn(" with bad key");
        ELSIF res = 4 THEN H.wsn(" corrupted obj file")
        ELSIF res = 5 THEN H.wsn(" command not found")
        ELSIF res = 7 THEN H.wsn(" insufficient space")
        END
      END;
      Mod := NIL;
    END
  END
END Oberon.