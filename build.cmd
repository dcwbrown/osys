:: build.cmd - build Oberon comiler and system
;;
@SET VERBOSE=/v
::
:: The compiler is built if any files affecting the compiler have changed
::
@fc knowngood\WinHost.mod WinHost.mod >NUL
@if errorlevel 1 goto buildcompiler
@fc knowngood\Kernel.mod  Kernel.mod  >NUL
@if errorlevel 1 goto buildcompiler
@fc knowngood\Files.mod   Files.mod   >NUL
@if errorlevel 1 goto buildcompiler
@fc knowngood\Fonts.mod   Fonts.mod   >NUL
@if errorlevel 1 goto buildcompiler
@fc knowngood\Texts.mod   Texts.mod   >NUL
@if errorlevel 1 goto buildcompiler
@fc knowngood\ORS.mod     ORS.mod     >NUL
@if errorlevel 1 goto buildcompiler
@fc knowngood\ORB.mod     ORB.mod     >NUL
@if errorlevel 1 goto buildcompiler
@fc knowngood\X64.mod     X64.mod     >NUL
@if errorlevel 1 goto buildcompiler
@fc knowngood\Listing.mod Listing.mod >NUL
@if errorlevel 1 goto buildcompiler
@fc knowngood\ORG.mod     ORG.mod     >NUL
@if errorlevel 1 goto buildcompiler
@fc knowngood\ORP.mod     ORP.mod     >NUL
@if errorlevel 1 goto buildcompiler
@fc knowngood\WinPE.mod   WinPE.mod   >NUL
@if errorlevel 1 goto buildcompiler
@fc knowngood\WinArgs.mod WinArgs.mod >NUL
@if errorlevel 1 goto buildcompiler
@fc knowngood\obuild.mod  obuild.mod  >NUL
@if errorlevel 1 goto buildcompiler
::
@if not exist build2\obuild.exe goto buildcompiler
::
@goto skipcompiler
::
::
:buildcompiler
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
@rd /s /q buildtest 2>NUL
::
:: If WinPE has changed a prebuild will be required
@fc knowngood\WinPE.mod WinPE.mod
@if not errorlevel 1 goto firstbuild
::
@echo.
@echo ------------- Build prebuild compiler from knowngood and new WinPE -------------
@mkdir buildpre >NUL
@copy WinPE.mod buildpre >NUL
@cd buildpre >NUL
..\knowngood\obuild %VERBOSE% /s ./;../knowngood/Console.;../knowngood/ obuild
@if errorlevel 1 goto end
@cd ..
::
::
:firstbuild
@echo.
@echo ----------------- Build new compiler using known good compiler -----------------
@del t1 t2 >NUL 2>NUL
@mkdir build1 >NUL 2>NUL
@cd build1
if exist ..\buildpre\obuild.exe (
  ..\buildpre\obuild %VERBOSE% /s ../Console.;../ obuild
) else (
  ..\knowngood\obuild %VERBOSE% /s ../Console.;../ obuild
)
@if errorlevel 1 goto end
@cd ..
::
::
@echo.
@echo ---------------- Build new compiler using newly built compiler -----------------
@mkdir build2 >NUL 2>NUL
@cd build2
..\build1\obuild %VERBOSE% /s ../Console.;../ obuild
@if errorlevel 1 goto end
@if exist obuild.exe goto obexists
::
@echo.
@echo Build failed. obuild.exe not created.
@goto end
:obexists
@cd ..
::
::
@echo.
@echo --------------------------------- Build tests ----------------------------------
@mkdir buildtest >NUL 2>NUL
@cd buildtest
..\build2\obuild %VERBOSE% /s ../Console.;../ /b ../build2/ Test
@if errorlevel 1 goto testbuildfailed
@if exist Test.exe goto testexists
@echo.
@echo Compiler build failed. Test.exe not created.
@goto end
::
:testbuildfailed
@echo.
@echo Compiler build failed. Couldn't build Test.mod.
@goto end
::
:testexists
@echo.
@echo ---------------------------------- Run tests -----------------------------------
Test
@if errorlevel 1 goto testfailed
@cd ..
@echo Compiler build successful. Run snapgood.cmd to snapshot as known good.
::
::
:skipcompiler
::
::
@goto buildsystem
::@goto buildlinker
::
:testfailed
@echo.
@echo Compiler build failed. Tests.mod failed.
@goto end
::
::
::
::
:buildsystem
::
:: Build Oberon system - System.exe
::
::
@rd /s /q buildsys 2>NUL
@md buildsys
@cd buildsys
copy ..\*.mod >NUL
copy ..\*.Tool >NUL
copy ..\*.Fnt >NUL
copy ..\*.Lib >NUL
..\build2\obuild %VERBOSE% /s ./ ORP
..\build2\obuild %VERBOSE% /s ./ System
@if errorlevel 1 goto osysbuildfailed
::
System
@cd ..
@goto end
::
::
:osysbuildfailed
@cd ..
@echo.
@echo Oberon system build failed.
@goto end
::
::
::
::
:buildlinker
::
::  Build linker
::
@rd /s /q buildlink 2>nul
@md buildlink
@cd buildlink
@copy ..\build2\*.x64 >NUL
@copy ..\build2\*.smb >NUL
::@copy ..\buildsys\*.x64 >NUL
::@copy ..\buildsys\*.smb >NUL
@copy ..\*.mod >NUL
..\build2\obuild %VERBOSE% /s ./ Link
@if errorlevel 1 goto linkbuildfailed
::
Link
@if errorlevel 1 goto linkrunfailed
::
::
innercore
@cd ..
@goto end
::
::
:linkrunfailed
@cd ..
@echo.
@echo Link run failed.
@goto end
::
::
:linkbuildfailed
@cd ..
@echo.
@echo Linker build failed.
::
::
:end
