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
#cp ../knowngood/*.exe .
cp ../build/*.exe .
cp Link.PE.exe Link.ELF.exe
#
#
cmd ORP.Compile Host.Mod/s Kernel.Mod/s Files.Mod/s Modules.Mod/s
cmd ORP.Compile Fonts.Mod/s Texts.Mod/s Link.Mod/s ORS.Mod/s
cmd ORP.Compile ORB.Mod/s X64.Mod/s Listing.Mod/s ORG.Mod/s ORP.Mod/s
cmd Link.ELF ORP.Compile
#
#
# Rebuild using Linux compiler
#
#
./ORP.Compile Host.Mod/s Kernel.Mod/s Files.Mod/s Modules.Mod/s
./ORP.Compile Fonts.Mod/s Texts.Mod/s Link.Mod/s ORS.Mod/s
./ORP.Compile ORB.Mod/s X64.Mod/s Listing.Mod/s ORG.Mod/s ORP.Mod/s

