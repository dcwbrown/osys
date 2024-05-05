:: bld.cmd - build Oberon comiler and system
;;
@SET VERBOSE=
@SET KNOWNGOODBUILD=..\knowngood\obuild
::
:: Build compiler - obuild.exe
::
::   The first build uses the previous knowngood compiler, which will build the
::   first new .exe with the old WinPE code. Therefore the first build must
::   use the old WinHost code to guarantee that the WinHost global variable
::   layout matches that of the knowngood WinPE.
::
::   To add functionality that requires a new WinHost, it is therefore necessary
::   to build once with just WinHost updated, then again with functionality
::   dependent on WinHost.
::
@rd /s /q buildpre 2>NUL
@rd /s /q build1 2>NUL
@rd /s /q build2 2>NUL
@rd /s /q buildORP 2>NUL
@rd /s /q buildlink 2>NUL
@rd /s /q buildsys 2>NUL
::
:: If Link has changed a prebuild will be required
@fc knowngood\Link.mod Link.mod
@if not errorlevel 1 goto firstbuild
::
@echo.
@echo ------------- Build prebuild compiler from knowngood and new WinPE -------------
@md buildpre >NUL
@copy WinPE.mod buildpre >NUL
@copy Link.mod buildpre >NUL
@cd buildpre >NUL
%KNOWNGOODBUILD% %VERBOSE% /l /s ./;../knowngood/Console.;../knowngood/ obuild
@if errorlevel 1 goto end
@cd ..
::
::
:firstbuild
@echo.
@echo ----------------- Build new compiler using known good compiler -----------------
@del t1 t2 >NUL 2>NUL
@md build1 >NUL 2>NUL
@cd build1
if exist ..\buildpre\obuild.exe (
  ..\buildpre\obuild %VERBOSE% /l /s ../Console.;../ obuild
) else (
  %KNOWNGOODBUILD% %VERBOSE% /l /s ../Console.;../ obuild
)
@if errorlevel 1 goto end
@cd ..
::
::
@echo.
@echo ---------------- Build new compiler using newly built compiler -----------------
@md build2 >NUL 2>NUL
@cd build2
..\build1\obuild %VERBOSE% /l /s ../Console.;../ obuild
@if errorlevel 1 goto end
@if not exist obuild.exe goto end
@cd ..
::
::
@echo.
@echo ---------------------------------- Build ORP -----------------------------------
@md buildORP >NUL 2>NUL
@cd buildORP
..\build2\obuild %VERBOSE% /l /s ../Console.;../ /b ../build2/ ORP
@if errorlevel 1 goto end
@if not exist ORP.exe goto end
@cd ..
::
::
@echo.
@echo ---------------------------------- Build Link -----------------------------------
@md buildLink >NUL 2>NUL
@cd buildLink
..\build2\obuild %VERBOSE% /l /s ../Console.;../ /b ../build2/ Link
@if errorlevel 1 goto end
@if not exist Link.exe goto end
@cd ..
::
::
@echo.
@echo -------------------------- Build innercore.exe ---------------------------
::
::
@rd /s /q buildsys 2>NUL
@md buildsys >NUL 2>NUL
@cd buildsys
copy ..\*.mod >NUL 2>NUL
copy ..\*.Tool >NUL 2>NUL
copy ..\*.Fnt >NUL 2>NUL
copy ..\*.Lib >NUL 2>NUL
copy ..\buildorp\ORP.exe ORP.Compile.exe >NUL 2>NUL
copy ..\buildLink\Link.exe Link.Link.exe >NUL 2>NUL
::
ORP.Compile WinHost.Mod/s Kernel.Mod/s FileDir.Mod/s Files.Mod/s Modules.Mod/s ~
ORP.Compile WinGui.Mod/s Input.Mod/s Display.Mod/s Viewers.Mod/s ~
ORP.Compile Fonts.Mod/s Texts.Mod/s Oberon.Mod/s ~
ORP.Compile MenuViewers.Mod/s TextFrames.Mod/s Edit.Mod/s System.Mod/s ~
ORP.Compile ORS.Mod/s ORB.Mod/s X64.Mod/s Listing.Mod/s ORG.Mod/s ORP.Mod/s ~
ORP.Compile Link.Mod/s ~
Link.Link

:end
@cd ..
