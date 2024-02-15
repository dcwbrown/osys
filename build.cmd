:: Build new compiler from known good compiler
::
@rd /s /q build1 build2 2>NUL
@mkdir build1 build2 >NUL
@copy *.mod build1 >NUL
@copy *.mod build2 >NUL
::
@echo ----------------- Build new compiler using known good compiler -----------------
@cd build1
..\knowngood\hob hob
@if errorlevel 1 goto end
@cd ..
::
@echo ---------------- Build new compiler using newly built compiler -----------------
@cd build2
..\build1\hob hob
@if errorlevel 1 goto end
@cd ..
@echo.
@echo Build successful. Run snapgood.cmd to snapshot as known good.

:end
