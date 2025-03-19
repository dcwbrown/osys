@echo off
::
::
rd /s /q build.oc 2>NUL
md build.oc >NUL 2>NUL
cd build.oc >NUL
::
::
::  copy ..\knowngood\ORP.Compile.exe >NUL 2>NUL
::  copy ..\knowngood\Link.PE.exe >NUL 2>NUL
::  copy /Y ..\src\*.mod >NUL
::  ORP.Compile Win.Host.Mod/s Kernel.Mod/s Files.Mod/s Modules.Mod/s ~  || goto end
::  ORP.Compile Fonts.Mod/s Texts.Mod/s Link.Mod/s ORS.Mod/s ORB.Mod/s ~ || goto end
::
::
robocopy ..\build.win . *.smb *.x64 *.exe        /xx /njh /njs /np /nfl /ndl >NUL
robocopy ..\src       . OCB.Mod OCG.Mod OCP.Mod  /xx /njh /njs /np /nfl /ndl >NUL
::
::
ORP.Compile OCB.Mod/s OCG.Mod/s OCP.Mod/s ~ || goto end
Link.PE OCP.Compile                         || goto end
::
::
:: robocopy ..\src . ORG.Mod win.Host.Mod /xx /njh /njs /np /nfl /ndl   >NUL
:: OXP.Compile win.Host.Mod/s ~
::
::
robocopy ..\src . testregalloc.Mod /xx /njh /njs /np /nfl /ndl   >NUL
OCP.Compile testregalloc.Mod/s ~
::
::
:end
cd ..
