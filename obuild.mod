MODULE obuild;  (* Command line driver for ORP *)  (*$CONSOLE*)

IMPORT Boot, ORP, ORS, WinPE;

BEGIN
  ORP.CompileFile("Winshim.mod");
  IF ORS.errcnt = 0 THEN ORP.CompileFile("Kernel.mod")    END;
  IF ORS.errcnt = 0 THEN ORP.CompileFile("Files.mod")     END;
  IF ORS.errcnt = 0 THEN ORP.CompileFile("Fonts.mod")     END;
  IF ORS.errcnt = 0 THEN ORP.CompileFile("Texts.mod")     END;
  IF ORS.errcnt = 0 THEN ORP.CompileFile("Oberon.mod")    END;
  IF ORS.errcnt = 0 THEN ORP.CompileFile("Writer.mod")    END;
  IF ORS.errcnt = 0 THEN ORP.CompileFile("ORS.mod")       END;
  IF ORS.errcnt = 0 THEN ORP.CompileFile("ORB.mod")       END;
  IF ORS.errcnt = 0 THEN ORP.CompileFile("X64.mod")       END;
  IF ORS.errcnt = 0 THEN ORP.CompileFile("Listing.mod")   END;
  IF ORS.errcnt = 0 THEN ORP.CompileFile("ORG.mod")       END;
  IF ORS.errcnt = 0 THEN ORP.CompileFile("ORP.mod")       END;
  IF ORS.errcnt = 0 THEN ORP.CompileFile("WinPE.mod")       END;
  IF ORS.errcnt = 0 THEN ORP.CompileFile("obuild.mod")    END;
  IF ORS.errcnt = 0 THEN
    WinPE.AddModule("Kernel.code");
    WinPE.AddModule("Files.code");
    WinPE.AddModule("Fonts.code");
    WinPE.AddModule("Texts.code");
    WinPE.AddModule("Writer.code");
    WinPE.AddModule("Oberon.code");
    WinPE.AddModule("ORS.code");
    WinPE.AddModule("ORB.code");
    WinPE.AddModule("X64.code");
    WinPE.AddModule("Listing.code");
    WinPE.AddModule("ORG.code");
    WinPE.AddModule("ORP.code");
    WinPE.AddModule("WinPE.code");
    WinPE.AddModule("obuild.code");
    WinPE.Generate("obuild.exe");
  ELSE
    Boot.ExitProcess(99)
  END
END obuild.
