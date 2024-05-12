System.Open ^  System.Recall  System.Watch  System.Collect  System.Quit
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

Tools.Inspect 0
Tools.Sector 1
Tools.ShowFile 
Tools.Recall  Tools.Clear

ORP.Compile WinHost.Mod/s Kernel.Mod/s Files.Mod/s Modules.Mod/s ~
ORP.Compile Fonts.Mod/s Texts.Mod/s Link.Mod/s ORS.Mod/s ~
ORP.Compile ORB.Mod/s X64.Mod/s Listing.Mod/s ORG.Mod/s ORP.Mod/s ~

Link.Link ORP.Compile Link.Link

ORP.Compile FileDir.Mod/s WinGui.Mod/s Input.Mod/s Display.Mod/s ~
ORP.Compile Viewers.Mod/s Oberon.Mod/s MenuViewers.Mod/s ~
ORP.Compile TextFrames.Mod/s Edit.Mod/s System.Mod/s ~

Link.Link Oberon
