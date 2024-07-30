#!/bin/bash

# Exit immediately if a command exits with a non-zero status
set -e

# Print each command before executing it (useful for debugging)
#set -x

# Path to the Conda environment tarball and POWHEG main executable
CONDA_ENV_TARBALL="/nfs/dust/cms/user/vazzolef/minnlo-tools/minnlo-env-minimal.tar.gz"
POWHEG_MAIN="/nfs/dust/cms/user/vazzolef/minnlo-tools/POWHEG-BOX-V2/Zj/ZjMiNNLO/pwhg_main"

# Arguments passed to the script
GRIDPACK="${1}"
OUTFOLDER="${2}"
NEVTS="${3}"
SEED="${4}"

# Ensure all arguments are provided
if [[ -z "${GRIDPACK}" || -z "${OUTFOLDER}" || -z "${NEVTS}" || -z "${SEED}" ]]; then
    echo "Usage: $0 <gridpack> <outfolder> <nevts> <seed>"
    exit 1
fi

# Create a temporary directory and ensure it's cleaned up on exit
TEMP_DIR=$(mktemp -d)
#trap 'rm -rf "${TEMP_DIR}"' EXIT

# Unpack the Conda environment tarball in the temporary directory
echo "Unpacking environment..."
tar -xzf "${CONDA_ENV_TARBALL}" -C "${TEMP_DIR}"

# Activate the Conda environment
echo "Sourcing environment..."
source "${TEMP_DIR}/bin/activate"

# Set the LHAPDF data path
export LHAPDF_DATA_PATH="/cvmfs/sft.cern.ch/lcg/external/lhapdfsets/current/"

# Unpack the gridpack
echo "Unpacking gridpack..."
tar -xzf "${GRIDPACK}" -C "${TEMP_DIR}"

# Change to the directory containing the unpacked gridpack
echo "Running POWHEG..."
pushd "${TEMP_DIR}/gridpack"

# Modify the powheg.input file
sed -i "s;numevts.*;numevts ${NEVTS};g" powheg.input
sed -i "s;xgriditeration.*;#xgriditeration.*;g" powheg.input
sed -i "s;parallelstage.*;#parallelstage.*;g" powheg.input
sed -i "s;manyseeds.*;#manyseeds.*;g" powheg.input
sed -i "s;maxseeds.*;#maxseeds.*;g" powheg.input
echo "iseed ${SEED}" >> powheg.input

# Run the POWHEG main executable and log the output
time "${POWHEG_MAIN}" | tee -a powheg.log

# Create output folder with timestamp and UUID
timestamp=$(date +"%d_%m_%Y")
uuid=$(uuidgen)
output_dir="${OUTFOLDER}/${timestamp}/${uuid}/seed_${SEED}"
mkdir -p "${output_dir}"

# Copy relevant output files to the output directory
echo "Staging output from ${TEMP_DIR} to ${output_dir}"
cp -p pwgpwhgalone-output*.top "${output_dir}" 
cp -p pwgevents.lhe            "${output_dir}"
cp -p powheg.log               "${output_dir}"
cp -p powheg.input             "${output_dir}"

# Return to the previous directory
popd

echo "Job completed successfully."

exit 0