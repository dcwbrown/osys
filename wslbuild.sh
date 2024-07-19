#!/bin/bash
# Build on linux from files hosted on windows:
# Copy latest osys from Windows using rsync
# Fix executable bits
# Start normal build
# This is a lot faster than over the wsl ntfs file mount
rsync -a --delete localhost::osys/ ~/osys
cd ~/osys
chmod +x knowngood/ORP.Compile knowngood/Link.ELF *.sh
. ./build.sh
