#!/bin/bash

# Function to check if conda is installed
check_conda() {
    if ! command -v conda &> /dev/null; then
        echo "conda could not be found. Please install Anaconda or Miniconda and try again."
        exit 1
    else
        case "$(uname)" in
            "Darwin")
                export CONDA_PATH="${CONDA_EXE}"
                export CONDA_PYTHON_PATH="${CONDA_PYTHON_EXE}"
                echo "Created env. variable CONDA_PATH=${CONDA_PATH}"
                echo "Created env. variable CONDA_PYTHON_PATH=${CONDA_PYTHON_PATH}"
                export LD_LIBRARY_PATH=$CONDA_PREFIX/lib:$LD_LIBRARY_PATH
                export DYLD_LIBRARY_PATH=$CONDA_PREFIX/lib:$DYLD_LIBRARY_PATH
                export LIBRARY_PATH=$CONDA_PREFIX/lib:$LIBRARY_PATH
                ;;
            "Linux")
                export CONDA_PATH="${CONDA_EXE}"
                export CONDA_PYTHON_PATH="${CONDA_PYTHON_EXE}"
                echo "Created env. variable CONDA_PATH=${CONDA_PATH}"
                echo "Created env. variable CONDA_PYTHON_PATH=${CONDA_PYTHON_PATH}"
                ;;
            *)
                echo "unsupported operating system"
                exit 1
                ;;
        esac
    fi
}

# Function to activate the conda environment
activate_environment() {
    local env_name=$1
    conda=${CONDA_PATH}
    source "$(conda info --base)/etc/profile.d/conda.sh"
    conda activate "$env_name"
    if [ $? -ne 0 ]; then
        echo "Failed to activate conda environment: $env_name"
        echo ""
        echo "Available envs:"
        conda info --envs
        exit 1
    fi
    echo "Conda environment '$env_name' activated successfully."
}

# Main script
main() {
    check_conda

    # Determine the directory where the script is located
    SOURCE="${BASH_SOURCE[0]}"
    while [ -L "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
        DIR=$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )
        SOURCE=$(readlink "$SOURCE")
        [[ $SOURCE != /* ]] && SOURCE=$DIR/$SOURCE # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
    done
    SCRIPT_DIR=$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )
    export MINNLO_TOOLS_PATH=${SCRIPT_DIR}
    echo "Created env. variable MINNLO_TOOLS_PATH=${SCRIPT_DIR}"

    # Export the pdf sets location
    export LHAPDF_DATA_PATH="/cvmfs/sft.cern.ch/lcg/external/lhapdfsets/current/"
    echo "Created env. variable LHAPDF_DATA_PATH=${LHAPDF_DATA_PATH}"
    
    activate_environment "minnlo-env"
}

main