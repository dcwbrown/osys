:: Build new compiler from known good compiler
::
:: The first build uses the previous knowngood compiler, which will build the
:: first new .exe with the old WinPE code. Therefore the first build must
:: use the old winshim code to guarantee that the winshim global variable
:: layout matches that of the knowngoos WinPE.
::
:: To add functionality that requires a new winshim, it is therefore necessary
:: to build once with just winshim updated, then again with functionality
:: dependent on winshim.
::
@rd /s /q build1 build2 2>NUL
@mkdir build1 build2 >NUL
@copy *.mod build1 >NUL
@copy knowngood\winshim.mod build1 >NUL
@copy *.mod build2 >NUL
::
@echo ----------------- Build new compiler using known good compiler -----------------
@cd build1
..\knowngood\ob ob
@if errorlevel 1 goto end
@cd ..
::
@echo ---------------- Build new compiler using newly built compiler -----------------
@cd build2
..\build1\ob ob
@if errorlevel 1 goto end
@if exist ob.exe goto ok
@echo.
@echo Build failed. ob.exe not created.
@goto end
:ok
@cd ..
@echo.
@echo Build successful. Run snapgood.cmd to snapshot as known good.

:end
