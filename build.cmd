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

:: @cd bootstrap
:: @mkdir build >NUL 2>NUL
:: @..\smoc\Build /v /b build /s ./;../smoc;.. obuild
:: @if errorlevel 1 goto end
:: @cd ..
:: ::
:: :: use bootstrap compiler to build final compiler
:: ::
:: @del *.smb >NUL
:: @del *.code >NUL
:: ::
:: @echo ------------------------------------------------------------------------
:: bootstrap\build\obuild
:: @if errorlevel 1 goto end
:: @echo ------------------------------------------------------------------------
:: ::
:: ::  use final compiler to rebuild compiler
:: ::
:: @mkdir bootstrap\build2 >NUL 2>NUL
:: @cd bootstrap\build2
:: @del /q *.* >NUL
:: @copy ..\..\*.mod >NUL
:: :: ..\..\obuild obuild.mod
:: ..\..\hob hob
:: @cd ..\..
:: @if errorlevel 1 goto end
:: @echo ------------------------------------------------------------------------
:: ::
:: ::  use rebuilt compiler to build another
:: ::
:: @mkdir bootstrap\build3 >NUL 2>NUL
:: @cd bootstrap\build3
:: @del /q *.* >NUL
:: @copy ..\..\*.mod >NUL
:: :: ..\build2\obuild obuild.mod
:: ..\build2\hob hob
:: @cd ..\..
:: @if errorlevel 1 goto end
:: @echo ------------------------------------------------------------------------
:: ::
:: ::  use build3 compiler to build another with existing .smbs
:: ::
:: @mkdir bootstrap\build4 >NUL 2>NUL
:: @cd bootstrap\build4
:: @del /q *.* >NUL
:: @copy ..\..\*.mod >NUL
:: @copy ..\build3\*.smb >NUL
:: :: ..\build3\obuild obuild.mod
:: ..\build3\hob hob
:: @cd ..\..

:end
