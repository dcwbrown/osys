@echo off
echo ---------------- Build Windows Oberon compiler and system -----------------
echo.
::
::
rd /s /q build 2>NUL
md build >NUL 2>NUL
cd build
copy ..\knowngood\ORP.Compile.exe >NUL 2>NUL
copy ..\knowngood\Link.Link.exe >NUL 2>NUL
::
::
::  goto outercorebuild
::  fc ..\common\Link.Mod ..\knowngood\Link.Mod >NUL && goto noprebuild
goto noprebuild
::
::
echo.
echo ---------- Build prebuild linker with new link and old winhost -----------
echo.
::
::
copy /Y ..\knowngood\*.mod >NUL
copy /Y ..\common\Link.mod >NUL
ORP.Compile Host.Mod/s Kernel.Mod/s Files.Mod/s Modules.Mod/s ~    || goto end
ORP.Compile Fonts.Mod/s Texts.Mod/s Link.Mod/s ~                   || goto end
Link.Link Link                                                     || goto end
move /Y Link.exe Link.Link.exe >NUL
::
::
:noprebuild
echo.
echo ----------- Build with known good compiler and updated linker ------------
echo.
::
copy /Y ..\common\*.mod >NUL
copy /Y ..\win\*.mod >NUL
::
ORP.Compile Host.Mod/s Kernel.Mod/s Files.Mod/s Modules.Mod/s ~     || goto end
ORP.Compile Fonts.Mod/s Texts.Mod/s Link.Mod/s ORS.Mod/s ~          || goto end
ORP.Compile ORB.Mod/s X64.Mod/s Listing.Mod/s ORG.Mod/s ORP.Mod/s ~ || goto end
Link.Link ORP.Compile Link.Link                                     || goto end
::
::
echo.
echo --------------- Rebuild compiler with newly built compiler ---------------
echo.
::
::
ORP.Compile Host.Mod/s Kernel.Mod/s Files.Mod/s Modules.Mod/s ~     || goto end
ORP.Compile Fonts.Mod/s Texts.Mod/s Link.Mod/s ORS.Mod/s ~          || goto end
ORP.Compile ORB.Mod/s X64.Mod/s Listing.Mod/s ORG.Mod/s ORP.Mod/s ~ || goto end
Link.Link ORP.Compile Link.Link                                     || goto end
goto oberonbuild
::
::
echo.
echo --------------------------- Build outer core -----------------------------
echo.
:outercorebuild
::
copy /Y ..\common\*.mod >NUL
copy /Y ..\win\*.mod >NUL
::
ORP.Compile Host.Mod/s Kernel.Mod/s Files.Mod/s Modules.Mod/s ~  || goto end
ORP.Compile Fonts.Mod/s Texts.Mod/s ~                            || goto end
::
::
echo.
echo -------------------------- Build Oberon system ---------------------------
echo.
:oberonbuild
::
::
ORP.Compile FileDir.Mod/s Gui.Mod/s Input.Mod/s Display.Mod/s ~ || goto end
ORP.Compile Viewers.Mod/s Oberon.Mod/s MenuViewers.Mod/s ~      || goto end
ORP.Compile TextFrames.Mod/s Edit.Mod/s System.Mod/s ~          || goto end
copy /Y ..\*.png >NUL 2>NUL
Link.Link Oberon+System+ORP+Link                                || goto end

echo.
echo --------------------------- Run Oberon system ----------------------------
echo.
::
::
copy /Y ..\*.Tool >NUL 2>NUL
copy /Y ..\*.Fnt >NUL 2>NUL
Oberon
::
::
:end
cd ..
