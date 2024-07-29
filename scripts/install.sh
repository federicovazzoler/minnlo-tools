#!/bin/bash

environment() {
    SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
    PARENT_DIR="$(dirname "$SCRIPT_DIR")"
    source "$PARENT_DIR/env.sh"
}

main() {
    environment
    pushd ${MINNLO_TOOLS_PATH}
    svn checkout --username anonymous --password anonymous svn://powhegbox.mib.infn.it/trunk/POWHEG-BOX-V2
    pushd POWHEG-BOX-V2
    svn co --username anonymous --password anonymous svn://powhegbox.mib.infn.it/trunk/User-Processes-V2/Zj
    mv Zj/ZjMiNNLO/Makefile Zj/ZjMiNNLO/Makefile.orig 
    case "$(uname)" in
        "Linux")
            cp ../patches/powheg/ZjMiNNLO.Makefile Zj/ZjMiNNLO/Makefile
            ;;
        "Darwin")
            cp ../patches/powheg/ZjMiNNLO_MAC.Makefile Zj/ZjMiNNLO/Makefile
            ;;
        *)
            echo "unsupported operating system"
            exit 1
            ;;
    esac
    popd
    popd
    exit 0
}

main

#for patch_file in ../patches/*.patch; do
#  if [ -e "$patch_file" ]; then
#    if [ "../patches/include.pwhg_bookhist-multi.h.patch" != "$patch_file" ]; then
#      echo "Applying patch: $patch_file"
#      patch -d $(pwd) -p1 < "$patch_file"
#    fi
#  fi
#done

