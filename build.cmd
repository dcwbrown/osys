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
:: @fc ..\Link.Mod ..\knowngood\Link.Mod >NUL && goto noprebuild
goto noprebuild
::
::
@echo.
@echo ---------- Build prebuild linker with new link and old winhost -----------
@echo.
::
::
@copy ..\knowngood\*.mod >NUL
@copy /Y ..\Link.mod >NUL
@ORP.Compile WinHost.Mod/s Kernel.Mod/s Files.Mod/s Modules.Mod/s ~ || goto end
@ORP.Compile Fonts.Mod/s Texts.Mod/s Link.Mod/s ~                   || goto end
@Link.Link Link                                                     || goto end
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
@ORP.Compile WinHost.Mod/s Kernel.Mod/s Files.Mod/s Modules.Mod/s ~  || goto end
@ORP.Compile Fonts.Mod/s Texts.Mod/s Link.Mod/s ORS.Mod/s ~          || goto end
@ORP.Compile ORB.Mod/s X64.Mod/s Listing.Mod/s ORG.Mod/s ORP.Mod/s ~ || goto end
::
@Link.Link ORP.Compile Link || goto end
@move /Y Link.exe Link.Link.exe >NUL
::
::
@echo.
@echo --------------- Rebuild compiler with newly built compiler ---------------
@echo.
::
::
@ORP.Compile WinHost.Mod/s Kernel.Mod/s Files.Mod/s Modules.Mod/s ~  || goto end
@ORP.Compile Fonts.Mod/s Texts.Mod/s Link.Mod/s ORS.Mod/s ~          || goto end
@ORP.Compile ORB.Mod/s X64.Mod/s Listing.Mod/s ORG.Mod/s ORP.Mod/s ~ || goto end
::
@Link.Link ORP.Compile Link                                          || goto end
@move /Y Link.exe Link.Link.exe >NUL
::
::
@echo.
@echo -------------------------- Build Oberon system ---------------------------
@echo.
::
::
@ORP.Compile FileDir.Mod/s WinGui.Mod/s Input.Mod/s Display.Mod/s ~ || goto end
@ORP.Compile Viewers.Mod/s Oberon.Mod/s MenuViewers.Mod/s ~         || goto end
@ORP.Compile TextFrames.Mod/s Edit.Mod/s System.Mod/s ~             || goto end
::
@Link.Link Oberon                                                   || goto end
::
::
:end
@cd ..