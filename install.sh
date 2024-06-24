#!/bin/bash

svn checkout --username anonymous --password anonymous svn://powhegbox.mib.infn.it/trunk/POWHEG-BOX-V2
cd POWHEG-BOX-V2
svn co --username anonymous --password anonymous svn://powhegbox.mib.infn.it/trunk/User-Processes-V2/Zj
mv Zj/ZjMiNNLO/Makefile Zj/ZjMiNNLO/Makefile.orig 
cp ../patches/powheg/ZjMiNNLO.Makefile Zj/ZjMiNNLO/Makefile
#for patch_file in ../patches/*.patch; do
#  if [ -e "$patch_file" ]; then
#    if [ "../patches/include.pwhg_bookhist-multi.h.patch" != "$patch_file" ]; then
#      echo "Applying patch: $patch_file"
#      patch -d $(pwd) -p1 < "$patch_file"
#    fi
#  fi
#done

exit 0
