@echo off
echo ----------------- Build Linux compiler on Windows system ------------------
echo.
::
::
rd /s /q build.lin 2>NUL
md build.lin >NUL 2>NUL
cd build.lin
copy ..\knowngood\ORP.Compile.exe >NUL 2>NUL
copy ..\knowngood\Link.PE.exe Link.ELF.exe >NUL 2>NUL
::
::
copy /Y ..\src\*.mod >NUL
::
ORP.Compile Lin.Host.Mod/s Kernel.Mod/s Files.Mod/s Modules.Mod/s ~  || goto end
ORP.Compile Fonts.Mod/s Texts.Mod/s Link.Mod/s ORS.Mod/s ~           || goto end
ORP.Compile ORB.Mod/s X64.Mod/s Listing.Mod/s ORG.Mod/s ORP.Mod/s ~  || goto end
Link.ELF ORP.Compile Link.ELF                                        || goto end
::
::
:end
cd ..
