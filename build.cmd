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
@..\..\obuild obuild.mod
@cd ..\..
:end
