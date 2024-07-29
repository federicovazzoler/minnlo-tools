#!/bin/bash

set -e

# Create a temporary directory
TEMP_DIR=$(mktemp -d)

# Unpack the Conda environment tarball in the temporary directory
echo "Unpacking env"
tar -xzf /nfs/dust/cms/user/vazzolef/minnlo-tools/minnlo-env-minimal.tar.gz -C $TEMP_DIR

# Activate the Conda environment
echo "Sourcing env"
source $TEMP_DIR/bin/activate

export LHAPDF_DATA_PATH="/cvmfs/sft.cern.ch/lcg/external/lhapdfsets/current/"

echo "Unpacking gridpack"
tar -xzf /nfs/dust/cms/user/vazzolef/minnlo-tools/production/gridpacks/pilot2/gridpack.tar.gz -C $TEMP_DIR

# Generation step
echo "Running POWHEG"
pushd $TEMP_DIR/gridpack
sed -i "s;numevts.*;numevts 1000;g" powheg.input
time /nfs/dust/cms/user/vazzolef/minnlo-tools/POWHEG-BOX-V2/Zj/ZjMiNNLO/pwhg_main<<EOF
$1
EOF

# Staging output
# Define the base output folder
outfolder="/nfs/dust/cms/user/vazzolef/minnlo-tools/production/events/pilot2/"

# Generate the current date and time in the format YYYYMMDD_HHMM (without seconds)
#timestamp=$(date +"%d%m%Y_%H%M")
timestamp=$(date +"%d_%m_%Y")

# Generate a UUID
uuid=$(uuidgen)

# Create the full path for the new folder with the timestamp and UUID
newfolder="${outfolder}${timestamp}/${uuid}"

# Create the new folder
mkdir -p "$newfolder"

cp -v pwgpwhgalone-output*.top $newfolder 
cp -v pwgevents-*.lhe $newfolder
cp -v pwgcounters-st4-*.dat $newfolder

popd

# Clean up the environment
echo "Cleaning up env"
rm -rf $TEMP_DIR

exit 0