#!/bin/bash
set -e
#
# ------------------- Build Linux Oberon compiler and system -------------------
#
#
rm -rf build.lin
mkdir build.lin
cd build.lin
cp ../src/*.Mod .
cp ../knowngood/ORP.Compile ../knowngood/Link.ELF .
#
#
# Build Linux compiler
#
#
./ORP.Compile Lin.Host.Mod/s Kernel.Mod/s Files.Mod/s Modules.Mod/s
./ORP.Compile Fonts.Mod/s Texts.Mod/s Link.Mod/s ORS.Mod/s
./ORP.Compile ORB.Mod/s X64.Mod/s Listing.Mod/s ORG.Mod/s ORP.Mod/s
./Link.ELF ORP.Compile Link
mv Link Link.ELF
#
#
# Build Oberon system
#
#
./ORP.Compile FileDir.Mod/s Lin.Gui.Mod/s Input.Mod/s Display.Mod/s
./ORP.Compile Viewers.Mod/s Oberon.Mod/s MenuViewers.Mod/s
./ORP.Compile TextFrames.Mod/s Edit.Mod/s System.Mod/s
./Link.ELF Oberon+System+ORP+Link
#
#
# Run Oberon system
#
#
cp ../fnt/*.Fnt .
cp ../Lin.System.Tool System.Tool
./Oberon
