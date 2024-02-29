:: Copy latest build to knowngood
@if exist build2\obuild.exe goto good
::
@echo Cannot snapshot good build: no file build2\obuild.exe
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
@copy build2\*.mod knowngood >NUL
@copy build2\*.smb knowngood >NUL
@copy build2\*.code knowngood >NUL
@copy build2\*.exe knowngood >NUL
::
:end