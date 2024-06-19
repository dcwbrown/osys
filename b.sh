set -e
cp ../lin/Host.Mod .
/mnt/c/Windows/System32/cmd.exe /c "ORP.Compile Host.Mod/s ~"
/mnt/c/Windows/System32/cmd.exe /c "Link.Link Host /l ~"
./Host
