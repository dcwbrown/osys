@echo ------------------------- Build Oberon compiler --------------------------
@echo.
::
::
@rd /s /q build 2>NUL
@md build >NUL 2>NUL
@cd build
@copy ..\knowngood\ORP.Compile.exe >NUL 2>NUL
@copy ..\knowngood\Link.Link.exe >NUL 2>NUL
::
::
@fc ..\Link.Mod ..\knowngood\Link.Mod >NUL
@if not errorlevel 1 goto noprebuild
::goto noprebuild
::
::
@echo.
@echo ---------- Build prebuild linker with new link and old winhost -----------
@echo.
::
::
@copy ..\knowngood\*.mod >NUL
@copy /Y ..\Link.mod >NUL
@ORP.Compile WinHost.Mod/s Kernel.Mod/s Files.Mod/s Modules.Mod/s ~
@if errorlevel 1 goto end
@ORP.Compile Fonts.Mod/s Texts.Mod/s Link.Mod/s ~
@if errorlevel 1 goto end
@Link.Link Link
@if errorlevel 1 goto end
@move /Y Link.exe Link.Link.exe >NUL
::
::
@echo.
@echo ----------- Build with known good compiler and updated linker ------------
@echo.
:noprebuild
::
@copy /Y ..\*.mod >NUL
::
@ORP.Compile WinHost.Mod/s Kernel.Mod/s Files.Mod/s Modules.Mod/s ~
@if errorlevel 1 goto end
@ORP.Compile Fonts.Mod/s Texts.Mod/s Link.Mod/s ~
@if errorlevel 1 goto end
@ORP.Compile ORS.Mod/s ORB.Mod/s X64.Mod/s Listing.Mod/s ORG.Mod/s ORP.Mod/s ~
@if errorlevel 1 goto end
::
@Link.Link ORP.Compile Link
@if errorlevel 1 goto end
@move /Y Link.exe Link.Link.exe >NUL
::
::
@echo.
@echo --------------- Rebuild compiler with newly built compiler ---------------
@echo.
::
::
@ORP.Compile WinHost.Mod/s Kernel.Mod/s Files.Mod/s Modules.Mod/s ~
@if errorlevel 1 goto end
@ORP.Compile Fonts.Mod/s Texts.Mod/s Link.Mod/s ~
@if errorlevel 1 goto end
@ORP.Compile ORS.Mod/s ORB.Mod/s X64.Mod/s Listing.Mod/s ORG.Mod/s ORP.Mod/s ~
@if errorlevel 1 goto end
::
@Link.Link ORP.Compile Link
@if errorlevel 1 goto end
@move /Y Link.exe Link.Link.exe >NUL
::
::
@echo.
@echo -------------------------- Build Oberon system ---------------------------
@echo.
::
::
@ORP.Compile FileDir.Mod/s WinGui.Mod/s Input.Mod/s Display.Mod/s Viewers.Mod/s ~
@if errorlevel 1 goto end
@ORP.Compile Oberon.Mod/s MenuViewers.Mod/s TextFrames.Mod/s Edit.Mod/s System.Mod/s ~
@if errorlevel 1 goto end
::
@Link.Link Oberon
@if errorlevel 1 goto end
::
::
:end
@cd ..