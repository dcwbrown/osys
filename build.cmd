:: Build bootstrap compiler
::
@cd bootstrap
@mkdir build >NUL 2>NUL
@..\smoc\Build /v /b build /s ./;../smoc;.. obuild
@if errorlevel 1 goto end
@cd ..
::
:: use bootstrap compiler to build final compiler
::
@del *.smb >NUL
@del *.code >NUL
::
@echo ------------------------------------------------------------------------
bootstrap\build\obuild
@if errorlevel 1 goto end
::
::  use final compiler to rebuild compiler
::
@mkdir bootstrap\build2 >NUL 2>NUL
@cd bootstrap\build2
@del /q *.* >NUL
@copy ..\..\*.mod
@echo ------------------------------------------------------------------------
..\..\obuild obuild.mod
@cd ..\..
@if errorlevel 1 goto end
::
::  use rebuilt compiler to build another
::
@mkdir bootstrap\build3 >NUL 2>NUL
@cd bootstrap\build3
@del /q *.* >NUL
@copy ..\..\*.mod
@echo ------------------------------------------------------------------------
..\build2\obuild obuild.mod
@cd ..\..
@if errorlevel 1 goto end
::
::  use build3 compiler to build another with existing .smbs
::
@mkdir bootstrap\build4 >NUL 2>NUL
@cd bootstrap\build4
@del /q *.* >NUL
@copy ..\..\*.mod
@copy ..\build3\*.smb
@echo ------------------------------------------------------------------------
..\build3\obuild obuild.mod
@cd ..\..

:end
