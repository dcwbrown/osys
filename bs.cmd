@echo -------------------------- Build innercore.exe ---------------------------
::
::
@rd /s /q buildsys 2>NUL
@md buildsys >NUL 2>NUL
@cd buildsys
copy ..\*.mod >NUL
copy ..\*.Tool >NUL
copy ..\*.Fnt >NUL
copy ..\*.Lib >NUL
copy ..\buildorp\ORP.exe >NUL
copy ..\buildLink\Link.exe >NUL
::
ORP Compile WinHost.Mod/s Kernel.Mod/s FileDir.Mod/s Files.Mod/s Modules.Mod/s ~
ORP Compile WinGui.Mod/s Input.Mod/s Display.Mod/s Viewers.Mod/s ~
ORP Compile Fonts.Mod/s Texts.Mod/s Oberon.Mod/s ~
ORP Compile MenuViewers.Mod/s TextFrames.Mod/s Edit.Mod/s System.Mod/s ~
ORP Compile ORS.Mod/s ORB.Mod/s X64.Mod/s Listing.Mod/s ORG.Mod/s ORP.Mod/s ~
ORP Compile Link.Mod/s ~
Link Link
::
innercore
::
:end
@cd ..
