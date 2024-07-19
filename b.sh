#!/bin/bash
set -e
#
#
function cmd {
  /mnt/c/Windows/System32/cmd.exe /c $*
}
#
# cp ../lin/Host.Mod .
# cp Link.PE.exe Link.ELF.exe
# /mnt/c/Windows/System32/cmd.exe /c "ORP.Compile Host.Mod/s ~"
# /mnt/c/Windows/System32/cmd.exe /c "Link.ELF Host ~"
# ./Host fred
#
cd ~/projects/oberon/osys
rm -rf buildlin
mkdir buildlin
cd buildlin
#cp ../common/*.Mod .
cmd copy '..\common\*.Mod >NUL'
cp ../lin/*.Mod .
cp ../knowngood/*.exe .
#cp ../build/*.exe .
mv Link.PE.exe Link.ELF.exe
#
# Build Linux compiler and linker using Win compiler and linker
#
cmd ORP.Compile Host.Mod/s Kernel.Mod/s Files.Mod/s Modules.Mod/s
cmd ORP.Compile Fonts.Mod/s Texts.Mod/s Link.Mod/s ORS.Mod/s
cmd ORP.Compile ORB.Mod/s X64.Mod/s Listing.Mod/s ORG.Mod/s ORP.Mod/s
cmd Link.ELF ORP.Compile Link
mv Link Link.ELF

#
#
# Rebuild using Linux compiler
#
#
./ORP.Compile Host.Mod/s Kernel.Mod/s Files.Mod/s Modules.Mod/s
./ORP.Compile Fonts.Mod/s Texts.Mod/s Link.Mod/s ORS.Mod/s
./ORP.Compile ORB.Mod/s X64.Mod/s Listing.Mod/s ORG.Mod/s ORP.Mod/s
./Link.ELF ORP.Compile Link
mv Link Link.ELF
#
#
# Build Oberon system
#
#
./ORP.Compile FileDir.Mod/s Gui.Mod/s Input.Mod/s Display.Mod/s
./ORP.Compile Viewers.Mod/s Oberon.Mod/s MenuViewers.Mod/s
./ORP.Compile TextFrames.Mod/s Edit.Mod/s System.Mod/s
./Link.ELF Oberon+System+ORP+Link
#
#
# Run Oberon system
#
#
cp ../*.Tool ../*.Fnt .
./Oberon
