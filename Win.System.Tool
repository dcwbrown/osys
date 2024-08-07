System.Open ^  System.Recall  System.Watch  System.Collect
Edit.Open ^  Edit.Recall
Edit.ChangeFont Oberon10i.Scn.Fnt
Edit.ChangeFont Oberon10b.Scn.Fnt

System.Directory ^
  *.Mod  *.Bak  *.Tool  *.Text  *.Scn.Fnt  *.smb  *.x64

ORP.Compile @  ORP.Compile @/s  ORP.Compile name~
System.Free ~
System.Open Draw.Tool
System.CopyFiles ~
System.RenameFiles ~
System.DeleteFiles ~

System.ShowModules  System.ShowCommands ^

PCLink1.Run
Hilbert.Draw  Sierpinski.Draw  Blink.Run  Stars.Open

ORP.Compile
  Win.Host.Mod/s   Kernel.Mod/s  Files.Mod/s        Modules.Mod/s
  Fonts.Mod/s      Texts.Mod/s   Link.Mod/s         ORS.Mod/s
  ORB.Mod/s        X64.Mod/s     Listing.Mod/s      ORG.Mod/s   ORP.Mod/s
  FileDir.Mod/s    Win.Gui.Mod/s Input.Mod/s        Display.Mod/s
  Viewers.Mod/s    Oberon.Mod/s  MenuViewers.Mod/s
  TextFrames.Mod/s Edit.Mod/s    System.Mod/s ~

Link.PE ORP.Compile Link.PE Oberon ~

ORP.Compile
  Graphics.Mod/s GraphicFrames.Mod/s Draw.Mod/s
  Curves.Mod/s   Rectangles.Mod/s ~

ORP.Compile Splines.Mod/s ~

ORP.Compile Test.Mod/s ~
Test.ArrayIndexTest  Test.NilDereferenceTest  Test.DivideByZeroTest
Test.TestHost  Test.testab
