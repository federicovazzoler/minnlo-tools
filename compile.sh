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

# Function to print a success message in green
print_success() {
  echo -e "\e[32m$1\e[0m"
}

# Function to print an error message in red
print_error() {
  echo -e "\e[31m$1\e[0m"
}
 
source environment.sh

if [ ! -z ${ANALYSIS} ]; then
  ANALYSIS=$(pwd)/analyses/${ANALYSIS}

  if [ ! -f ${ANALYSIS} ]; then
    echo "${ANALYSIS} not found"
    exit 1
  fi
else
  ANALYSIS=$(pwd)/analyses/pwhg_analysis_Zj.f
fi

PROCS=""
PROCS=${PROCS}" POWHEG-BOX-V2/Zj/ZjMiNNLO"

for PROC in ${PROCS}; do
  pushd ${PROC}
  
  if [ ${CLEAN} ]; then
    make clean
  fi
  
  cp -v ${ANALYSIS} pwhg_analysis-custom.f
  
 
  # compile main executable and lhef_analysis
  if make -j$(nproc) pwhg_main; then
    print_success "pwhg_main compilation successful."
  else
    print_error "pwhg_main compilation failed."
    exit 1
  fi
  
  if make -j$(nproc) lhef_analysis; then
    print_success "lhef_analysis compilation successful."
  else
    print_error "lhef_analysis compilation failed."
    exit 1
  fi
  
#  # compile the pythia interface
#  ## configure PHOTOS
#  pushd PHOTOS > /dev/null
#  chmod u+x configure
#  if ./configure --without-hepmc && make; then
#    print_success "PHOTOS configuration and compilation successful."
#  else
#    print_error "PHOTOS configuration or compilation failed."
#  #  exit 1
#  fi
#  
#  # Return to the original directory
#  popd 
#  
#  if make -j$(nproc) main-PYTHIA82-lhef; then
#    print_success "main-PYTHIA82-lhef compilation successful."
#  else
#    print_error "main-PYTHIA82-lhef compilation failed."
#  #  exit 1
#  fi
#  
#  if make -j$(nproc) main-PHOTOS-lhef; then
#    print_success "main-PHOTOS-lhef compilation successful."
#  else
#    print_error "main-PHOTOS-lhef compilation failed."
#  #  exit 1
#  fi
  
  # Return to the original directory
  popd
done

exit 0
