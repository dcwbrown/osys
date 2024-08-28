@echo off
::
::
rd /s /q build.orx 2>NUL
md build.orx >NUL 2>NUL
cd build.orx >NUL
::
::
::  copy ..\knowngood\ORP.Compile.exe >NUL 2>NUL
::  copy ..\knowngood\Link.PE.exe >NUL 2>NUL
::  copy /Y ..\src\*.mod >NUL
::  ORP.Compile Win.Host.Mod/s Kernel.Mod/s Files.Mod/s Modules.Mod/s ~  || goto end
::  ORP.Compile Fonts.Mod/s Texts.Mod/s Link.Mod/s ORS.Mod/s ORB.Mod/s ~ || goto end
::
::
robocopy ..\build.win . *.smb *.x64 *.exe /xx /njh /njs /np /nfl /ndl >NUL
robocopy ..\src       . OXG.Mod OXP.Mod Link.Mod /xx /njh /njs /np /nfl /ndl   >NUL
::
::
ORP.Compile OXG.Mod/s OXP.Mod/s ~                                        || goto end
Link.PE OXP.Compile                                                      || goto end
::
::
robocopy ..\src . ORG.Mod /xx /njh /njs /np /nfl /ndl   >NUL
OXP.Compile Link.Mod/s ~
::
::
:end
cd ..
