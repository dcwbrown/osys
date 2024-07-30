:: Copy latest win build to knowngood
@if exist build.win\Oberon.exe goto good
::
@echo Cannot snapshot good build: no file build\Oberon.exe.
@goto end
::
:good
::
@echo Backing up knowngood-2 as knowngood-3
@rd /s /q knowngood-3 2>NUL
@ren knowngood-2 knowngood-3
::
@echo Backing up knowngood-1 as knowngood-2
@rd /s /q knowngood-2 2>NUL
@ren knowngood-1 knowngood-2
::
@echo Backing up knowngood as knowngood-1
@rd /s /q knowngood-1 2>NUL
@ren knowngood knowngood-1
::
@echo Copying files from build to knowngood
@mkdir knowngood
@copy src\*.mod knowngood >NUL
@copy build.win\*.exe knowngood >NUL
@copy build.lin\ORP.Compile knowngood >NUL
@copy build.lin\Link.ELF knowngood >NUL
@copy build.lin\Oberon knowngood >NUL
::
:end