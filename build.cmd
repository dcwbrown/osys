@echo off
echo ---------------- Build Windows Oberon compiler and system -----------------
echo.
::
::
rd /s /q build.win 2>NUL
md build.win >NUL 2>NUL
cd build.win
copy ..\knowngood\ORP.Compile.exe >NUL 2>NUL
copy ..\knowngood\Link.PE.exe >NUL 2>NUL
::
::
::  goto outercorebuild
::  fc ..\src\Link.Mod ..\knowngood\Link.Mod >NUL && goto noprebuild
goto noprebuild
::
::
echo.
echo ---------- Build prebuild linker with new link and old winhost -----------
echo.
::
::
copy /Y ..\knowngood\*.mod >NUL
copy /Y ..\src\Link.mod >NUL
ORP.Compile Win.Host.Mod/s Kernel.Mod/s Files.Mod/s Modules.Mod/s ~  || goto end
ORP.Compile Fonts.Mod/s Texts.Mod/s Link.Mod/s ~                     || goto end
Link.PE Link                                                         || goto end
move /Y Link.exe Link.PE.exe >NUL
::
::
:noprebuild
echo.
echo ----------- Build with known good compiler and updated linker ------------
echo.
::
copy /Y ..\src\*.mod >NUL
::copy /Y ..\win\*.mod >NUL
::
ORP.Compile Win.Host.Mod/s Kernel.Mod/s Files.Mod/s Modules.Mod/s ~  || goto end
ORP.Compile Fonts.Mod/s Texts.Mod/s Link.Mod/s ORS.Mod/s ~           || goto end
ORP.Compile ORB.Mod/s X64.Mod/s Listing.Mod/s ORG.Mod/s ORP.Mod/s ~  || goto end
Link.PE ORP.Compile Link.PE                                          || goto end
::
::
echo.
echo --------------- Rebuild compiler with newly built compiler ---------------
echo.
::
::
ORP.Compile Win.Host.Mod/s Kernel.Mod/s Files.Mod/s Modules.Mod/s ~  || goto end
ORP.Compile Fonts.Mod/s Texts.Mod/s Link.Mod/s ORS.Mod/s ~           || goto end
ORP.Compile ORB.Mod/s X64.Mod/s Listing.Mod/s ORG.Mod/s ORP.Mod/s ~  || goto end
Link.PE ORP.Compile Link.PE                                          || goto end
goto oberonbuild
::
::
:outercorebuild
echo.
echo --------------------------- Build outer core -----------------------------
echo.
::
copy /Y ..\src\*.mod >NUL
::copy /Y ..\win\*.mod >NUL
::
ORP.Compile Win.Host.Mod/s Kernel.Mod/s Files.Mod/s Modules.Mod/s ~  || goto end
ORP.Compile Fonts.Mod/s Texts.Mod/s ~                                || goto end
::
::
:oberonbuild
echo.
echo -------------------------- Build Oberon system ---------------------------
echo.
::
::
ORP.Compile FileDir.Mod/s Win.Gui.Mod/s Input.Mod/s Display.Mod/s ~ || goto end
ORP.Compile Viewers.Mod/s Oberon.Mod/s MenuViewers.Mod/s ~          || goto end
ORP.Compile TextFrames.Mod/s Edit.Mod/s System.Mod/s ~              || goto end
copy /Y ..\*.png >NUL 2>NUL
Link.PE Oberon+System                                               || goto end
::Link.PE Oberon+System+ORP+Link                                      || goto end

echo.
echo --------------------------- Run Oberon system ----------------------------
echo.
::
::
copy /Y ..\Win.System.Tool System.Tool>NUL 2>NUL
copy /Y ..\fnt\*.Fnt >NUL 2>NUL
Oberon
::
::
:end
cd ..
