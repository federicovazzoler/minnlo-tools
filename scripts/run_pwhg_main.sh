#!/bin/bash

inputCard="${@}"

if [ ! -f ${inputCard} ]; then
  echo -e  "\033[0;31mNo input card given\033[0m"
  exit 1
fi

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PARENT_DIR="$(dirname "$SCRIPT_DIR")"
source "$PARENT_DIR/env.sh"

# Create the output directory
wrkFld="$(basename "$inputCard")"
wrkFld="${wrkFld%.input}"
mkdir -p "${wrkFld}" || exit 1
cp ${inputCard} ${wrkFld}/powheg.input || exit 1

# Execute the analysis script
pushd ${wrkFld}
if [ -f "powheg.log" ]; then rm "powheg.log"; fi
echo "powheg start: $(date +"%Y-%m-%d %H:%M:%S")" | tee -a powheg.log
$MINNLO_TOOLS_PATH/POWHEG-BOX-V2/Zj/ZjMiNNLO/pwhg_main < powheg.input | tee -a powheg.log
echo "powheg end: $(date +"%Y-%m-%d %H:%M:%S")" | tee -a powheg.log
popd

exit 0
