:: Copy latest build to knowngood
@if exist build\Oberon.exe goto good
::
@echo Cannot snapshot good build: no file build\Oberon.exe.
@goto end
::
:good
::
@echo Backing up knowngood.prev2 as knowngood.prev3
@rd /s /q knowngood.prev3 2>NUL
@ren knowngood.prev2 knowngood.prev3
::
@echo Backing up knowngood.prev1 as knowngood.prev2
@rd /s /q knowngood.prev2 2>NUL
@ren knowngood.prev1 knowngood.prev2
::
@echo Backing up knowngood as knowngood.prev1
@rd /s /q knowngood.prev1 2>NUL
@ren knowngood knowngood.prev1
::
@echo Copying files from build2 to knowngood
@mkdir knowngood
@copy *.mod knowngood >NUL
@copy build\ORP.Compile.exe knowngood >NUL
@copy build\Link.Link.exe knowngood >NUL
@copy build\Oberon.exe knowngood >NUL
::
:end