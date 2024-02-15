:: Copy latest build to knowngood
@if exist build2\ob.exe goto good
::
@echo Cannot snapshot good build: no file build2\ob.exe
@goto end
::
:good
@echo Backing up knowngood as knowngood.prev
@rd /s /q knowngood.prev
@ren knowngood knowngood.prev
@echo Copying files from build2 to knowngood
@mkdir knowngood
@copy build2\*.mod knowngood >NUL
@copy build2\*.smb knowngood >NUL
@copy build2\*.code knowngood >NUL
@copy build2\*.exe knowngood >NUL
::
:end