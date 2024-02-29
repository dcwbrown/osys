:: Build new compiler from known good compiler
::
:: The first build uses the previous knowngood compiler, which will build the
:: first new .exe with the old WinPE code. Therefore the first build must
:: use the old WinHost code to guarantee that the WinHost global variable
:: layout matches that of the knowngood WinPE.
::
:: To add functionality that requires a new WinHost, it is therefore necessary
:: to build once with just WinHost updated, then again with functionality
:: dependent on WinHost.
::
@rd /s /q buildpre 2>NUL
@rd /s /q build1 2>NUL
@rd /s /q build2 2>NUL
@rd /s /q buildtest 2>NUL
::
:: If WinPE has changed a prebuild will be required
@sed -n '/Start of/,/End of/p' knowngood\WinHost.mod >t1
@sed -n '/Start of/,/End of/p' WinHost.mod >t2
@fc t1 t2 >NUL
@if not errorlevel 1 goto firstbuild
::
@echo.
@echo ------------- Build prebuild compiler from knowngood and new WinPE -------------
@mkdir buildpre >NUL
@copy knowngood\*.mod buildpre >NUL
@copy WinPE.mod buildpre >NUL
@cd buildpre >NUL
..\knowngood\obuild obuild
@if errorlevel 1 goto end
@cd ..
::
:firstbuild
@echo.
@echo ----------------- Build new compiler using known good compiler -----------------
@del t1 t2 >NUL
@mkdir build1 >NUL 2>NUL
@copy *.mod build1 >NUL
@cd build1
if exist ..\buildpre\obuild.exe (..\buildpre\obuild obuild) else (..\knowngood\obuild obuild)
@if errorlevel 1 goto end
@cd ..
::
@echo.
@echo ---------------- Build new compiler using newly built compiler -----------------
@mkdir build2 >NUL 2>NUL
@copy *.mod build2 >NUL
@cd build2
..\build1\obuild obuild
@if errorlevel 1 goto end
@if exist obuild.exe goto obexists
::
@echo.
@echo Build failed. obuild.exe not created.
@goto end
:obexists
@cd ..
::
@echo.
@echo --------------------------------- Build tests ----------------------------------
@mkdir buildtest >NUL 2>NUL
@copy *.mod buildtest >NUL
@cd buildtest
..\build1\obuild Test
@if errorlevel 1 goto end
@if exist Test.exe goto Testexists
::
@echo.
@echo Build failed. Test.exe not created.
@goto end
:Testexists
@echo.
@echo ---------------------------------- Run tests -----------------------------------
Test
@if errorlevel 1 goto end
@cd ..
@echo Build successful. Run snapgood.cmd to snapshot as known good.

:end
