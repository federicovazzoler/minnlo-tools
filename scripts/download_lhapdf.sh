#!/bin/bash

environment() {
    SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
    PARENT_DIR="$(dirname "$SCRIPT_DIR")"
    source "$PARENT_DIR/env.sh"
}

main() {
    LINK='http://lhapdfsets.web.cern.ch/lhapdfsets/current/'

    for pdf in $@; do
      wget ${LINK}'/'${pdf}'.tar.gz'
      olddir=`pwd`
      cd `lhapdf-config --datadir`
      tar xvzpf ${olddir}'/'${pdf}'.tar.gz'
      cd - > /dev/null
      rm ${pdf}'.tar.gz'
    done
  
    exit 0
}

main ${@}