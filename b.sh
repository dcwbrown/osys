set -e
cp ../LinHost.Mod .
/mnt/c/Windows/System32/cmd.exe /c "ORP.Compile LinHost.Mod/s ~"
/mnt/c/Windows/System32/cmd.exe /c "Link.Link LinHost /l ~"
./LinHost
