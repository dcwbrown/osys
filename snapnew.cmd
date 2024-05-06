:: Copy latest new style build to newgood
::
@echo Backing up newgood.prev2 as newgood.prev3
@rd /s /q newgood.prev3 2>NUL
@ren newgood.prev2 newgood.prev3
::
@echo Backing up newgood.prev1 as newgood.prev2
@rd /s /q newgood.prev2 2>NUL
@ren newgood.prev1 newgood.prev2
::
@echo Backing up newgood as newgood.prev1
@rd /s /q newgood.prev1 2>NUL
@ren newgood newgood.prev1
::
@echo Copying files from new to newgood
@mkdir newgood
@copy new\*.mod newgood >NUL
@copy new\*.exe newgood >NUL
::
:end