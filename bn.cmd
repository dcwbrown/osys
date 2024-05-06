@echo ------------------- Build Oberon compiler and system ---------------------
@echo.
::
::
@rd /s /q new 2>NUL
@md new >NUL 2>NUL
@cd new
::
::
@copy ..\*.mod >NUL
@copy ..\*.Tool >NUL
@copy ..\*.Fnt >NUL
@copy ..\*.Lib >NUL
@copy ..\knowngood\ORP.Compile.exe >NUL 2>NUL
@copy ..\knowngood\Link.Link.exe >NUL 2>NUL
::
@ORP.Compile WinHost.Mod/s Kernel.Mod/s FileDir.Mod/s Files.Mod/s Modules.Mod/s ~
@if errorlevel 1 goto end
@ORP.Compile WinGui.Mod/s Input.Mod/s Display.Mod/s Viewers.Mod/s ~
@if errorlevel 1 goto end
@ORP.Compile Fonts.Mod/s Texts.Mod/s Oberon.Mod/s ~
@if errorlevel 1 goto end
@ORP.Compile MenuViewers.Mod/s TextFrames.Mod/s Edit.Mod/s System.Mod/s ~
@if errorlevel 1 goto end
@ORP.Compile ORS.Mod/s ORB.Mod/s X64.Mod/s Listing.Mod/s ORG.Mod/s ORP.Mod/s ~
@if errorlevel 1 goto end
@ORP.Compile Link.Mod/s ~
@if errorlevel 1 goto end
@Link.Link ORP.Compile
@if errorlevel 1 goto end
@Link.Link Link
@if errorlevel 1 goto end
@move /Y Link.exe Link.Link.exe >NUL
@Link.Link Oberon
@if errorlevel 1 goto end
::
::
@echo.
@echo.
@echo ------------------- Rebuild with newly built compiler --------------------
@echo.
::
::
@ORP.Compile WinHost.Mod/s Kernel.Mod/s FileDir.Mod/s Files.Mod/s Modules.Mod/s ~
@if errorlevel 1 goto end
@ORP.Compile WinGui.Mod/s Input.Mod/s Display.Mod/s Viewers.Mod/s ~
@if errorlevel 1 goto end
@ORP.Compile Fonts.Mod/s Texts.Mod/s Oberon.Mod/s ~
@if errorlevel 1 goto end
@ORP.Compile MenuViewers.Mod/s TextFrames.Mod/s Edit.Mod/s System.Mod/s ~
@if errorlevel 1 goto end
@ORP.Compile ORS.Mod/s ORB.Mod/s X64.Mod/s Listing.Mod/s ORG.Mod/s ORP.Mod/s ~
@if errorlevel 1 goto end
@ORP.Compile Link.Mod/s ~
@if errorlevel 1 goto end
@Link.Link ORP.Compile
@if errorlevel 1 goto end
@Link.Link Link
@if errorlevel 1 goto end
@move /Y Link.exe Link.Link.exe >NUL
@Link.Link Oberon
@if errorlevel 1 goto end
::
::
:end
@cd ..