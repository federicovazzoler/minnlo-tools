#!/bin/bash

version() {
    local cmd="${1}"
    echo "${cmd} version:"
    which ${cmd}
    echo ""
}

environment() {
    SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
    PARENT_DIR="$(dirname "$SCRIPT_DIR")"
    source "$PARENT_DIR/env.sh"
}

main() {
    environment

    version gcc
    version g++
    version c++
    version clang++
    version root
    version gfortran
}

main