#!/bin/bash
set -e
#
# ------------------- Build Linux Oberon compiler and system -------------------
#
#
rm -rf build.lin
mkdir build.lin
cd build.lin
cp -p ../src/*.Mod .
cp -p ../knowngood/ORP.Compile ../knowngood/Link.ELF .
#
echo
echo ----------------------- Build Linux Oberon compiler -----------------------
echo
#
./ORP.Compile Lin.Host.Mod/s Kernel.Mod/s Files.Mod/s Modules.Mod/s
./ORP.Compile Fonts.Mod/s Texts.Mod/s Link.Mod/s ORS.Mod/s
./ORP.Compile ORB.Mod/s X64.Mod/s Listing.Mod/s ORG.Mod/s ORP.Mod/s
./Link.ELF ORP.Compile Link
mv Link Link.ELF
#
echo
echo ------------------------- Build LinuOberon system -------------------------
echo
#
./ORP.Compile FileDir.Mod/s Lin.Gui.Mod/s Input.Mod/s Display.Mod/s
./ORP.Compile Viewers.Mod/s Oberon.Mod/s MenuViewers.Mod/s
./ORP.Compile TextFrames.Mod/s Edit.Mod/s System.Mod/s
cp -p ../Oberon.bmp .
#./Link.ELF Oberon+System+ORP+Link
./Link.ELF Oberon+System
#
#
# Run Oberon system
#
#
cp -p ../fnt/*.Fnt .
cp -p ../Lin.System.Tool System.Tool
#
echo
echo --------------------------- Start Linux Oberon ----------------------------
echo
#
./Oberon
