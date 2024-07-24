#!/bin/bash

set -e

help() {
    echo "Usage: $0 [-c | -clean] [-a analysis_name | -analysis analysis_name]"
    echo "Compile the POWHEG-BOX-V2 program with the specified analysis and options."
    echo "Options:"
    echo "  -c, -clean        clean the program before compilation"
    echo "  -a, -analysis     specify the name of the analysis to run"
    echo "  -h, -help         display this help message"
    echo ""
    echo "Example usage:"
    echo "$0 -c -a my_analysis"
    echo ""
    echo "Available analyses:"
    ls analyses/
    exit 0
}

print_success() {
    if [ "$(uname)" == "Linux" ]; then
        echo -e "\e[32m$1\e[0m"
    else
        echo "Success: $1"
    fi
}

print_error() {
    if [ "$(uname)" == "Linux" ]; then
        echo -e "\e[31m$1\e[0m"
    else
        echo "Error: $1"
    fi
}

environment() {
    SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
    PARENT_DIR="$(dirname "$SCRIPT_DIR")"
    source "$PARENT_DIR/env.sh"
}

main() {
    CLEAN=""
    ANALYSIS=""
    POSITIONAL=()
    while [[ $# -gt 0 ]]; do
        key="$1"
        case $key in
            -h | -help)
            help
            ;;
            -c)
            CLEAN=YES
            shift
            ;;
            -clean)
            CLEAN=YES
            shift
            ;;
            -a)
            ANALYSIS=$2
            shift
            shift
            ;;
            -analysis)
            ANALYSIS=$2
            shift
            shift
            ;;
            *)# unknown option
            POSITIONAL+=("$1")
            shift
            ;;
        esac
    done
    set -- "${POSITIONAL[@]}" # restore positional parameters

    environment

    if [ ! -z ${ANALYSIS} ]; then
        ANALYSIS=${MINNLO_TOOLS_PATH}/analyses/${ANALYSIS}
        if [ ! -f ${ANALYSIS} ]; then
            echo "${ANALYSIS} not found"
            exit 1
        fi
    else
        ANALYSIS=${MINNLO_TOOLS_PATH}/analyses/pwhg_analysis_Zj.f
        echo "Using defaul analysis ${ANALYSIS}"
    fi

    PROCS=""
    PROCS=${PROCS}" ${MINNLO_TOOLS_PATH}/POWHEG-BOX-V2/Zj/ZjMiNNLO"

    for PROC in ${PROCS}; do
        pushd ${PROC}

        if [ ${CLEAN} ]; then
          make clean
        fi

        cp -v ${ANALYSIS} pwhg_analysis-custom.f

        # compile main executable and lhef_analysis
        MAKE_OPTS=""
        if [ "$(uname)" == "Linux" ]; then MAKE_OPTS=-j$(nproc); fi
        if make ${MAKE_OPTS} pwhg_main; then
          print_success "pwhg_main compilation successful."
        else
          print_error "pwhg_main compilation failed."
          exit 1
        fi

        if make ${MAKE_OPTS} lhef_analysis; then
          print_success "lhef_analysis compilation successful."
        else
          print_error "lhef_analysis compilation failed."
          exit 1
        fi

        popd

        echo "compiling merge_top.cpp"
        pushd ${MINNLO_TOOLS_PATH}/scripts
        g++ -o merge_top merge_top.cpp # -I$CONDA_PREFIX/include -L$CONDA_PREFIX/lib
        if [ $? -ne 0 ]; then
            echo "failed to compile merge_top.cpp"
            exit 1
        fi
        pop
    done

    exit 0
}

main ${@}