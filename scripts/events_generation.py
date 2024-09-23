import os
import sys
import subprocess
import argparse

def parser():
    parser = argparse.ArgumentParser(description="Prepare and submit jobs to generate events from a MiNNLO gridpack")
    parser.add_argument("input_gridpack", help="Path to the POWHEG input gridpack")
    parser.add_argument("output_folder", help="Path to the output folder where the events will be stored")
    parser.add_argument("-e", "--num_evts", type=int, default=10000, help="Number of events per job")
    parser.add_argument("-j", "--num_jobs", type=int, default=1000, help="Number of jobs")
    parser.add_argument("-i", "--initial_seed", type=int, default=1, help="Initial seed for the job submission")
    parser.add_argument("-r", "--runtime", type=int, default=3, help="Max. runtime for the jobs")
    parser.add_argument("-s", "--submit", action="store_true", help="Submit the workflow")
    args = parser.parse_args()

    return args

def check_env():
    MINNLO_TOOLS_PATH = os.environ.get("MINNLO_TOOLS_PATH")
    CONDA_PATH = os.environ.get("CONDA_PATH")
    LHAPDF_DATA_PATH = os.environ.get("LHAPDF_DATA_PATH")
    if (not MINNLO_TOOLS_PATH) or (not CONDA_PATH) or (not LHAPDF_DATA_PATH):
        sys.stderr.write("Source the environment before launching the script 'source <MINNLO_TOOLS_FOLDER>/env.sh'\n")
        sys.exit()
    else:
        sys.stdout.write("Environment found.\n")
        return MINNLO_TOOLS_PATH, CONDA_PATH, LHAPDF_DATA_PATH

def write_job(input_gridpack, num_evts, seed, dagman_folder, events_folder, runtime, MINNLO_TOOLS_PATH, LHAPDF_DATA_PATH):
    job_folder = os.path.join(dagman_folder, f"seed_{seed}")
    os.makedirs(job_folder, exist_ok=False)

    script_path = os.path.join(job_folder, f"job_seed_{seed}.sh")
    submit_path = os.path.join(job_folder, f"job_seed_{seed}.sub") 
    output_path = os.path.join(job_folder, f"job_seed_{seed}.out")
    error_path  = os.path.join(job_folder, f"job_seed_{seed}.err")
    log_path    = os.path.join(job_folder, f"job_seed_{seed}.log")

    # Script
    script_file = f"""#!/bin/bash

# Exit immediately if a command exits with a non-zero status
set -e

# Print each command before executing it (useful for debugging)
#set -x

# Path to the Conda environment tarball and POWHEG main executable
CONDA_ENV_TARBALL={MINNLO_TOOLS_PATH}/minnlo-env-minimal.tar.gz
POWHEG_MAIN={MINNLO_TOOLS_PATH}/POWHEG-BOX-V2/Zj/ZjMiNNLO/pwhg_main

# Arguments passed to the script
GRIDPACK={input_gridpack}
OUTFOLDER={events_folder}
NEVTS={num_evts}
SEED={seed}

# Ensure all arguments are provided
if [[ -z "$GRIDPACK" || -z "$OUTFOLDER" || -z "$NEVTS" || -z "$SEED" ]]; then
    echo "Usage: $0 <gridpack> <outfolder> <nevts> <seed>"
    exit 1
fi

# Create a temporary directory and ensure it's cleaned up on exit
TEMP_DIR=$(mktemp -d)
#trap 'rm -rf "$TEMP_DIR"' EXIT

# Unpack the Conda environment tarball in the temporary directory
echo "Unpacking environment..."
tar -xzf "$CONDA_ENV_TARBALL" -C "$TEMP_DIR"

# Activate the Conda environment
echo "Sourcing environment..."
source "$TEMP_DIR/bin/activate"

# Set the LHAPDF data path
export LHAPDF_DATA_PATH={LHAPDF_DATA_PATH}

# Unpack the gridpack
echo "Unpacking gridpack..."
tar -xzf "$GRIDPACK" -C "$TEMP_DIR"

# Change to the directory containing the unpacked gridpack
echo "Running POWHEG..."
pushd "$TEMP_DIR/gridpack"

# Modify the powheg.input file
sed -i "s;numevts.*;numevts $NEVTS;g" powheg.input
sed -i "s;xgriditeration.*;#xgriditeration.*;g" powheg.input
sed -i "s;parallelstage.*;#parallelstage.*;g" powheg.input
sed -i "s;manyseeds.*;#manyseeds.*;g" powheg.input
sed -i "s;maxseeds.*;#maxseeds.*;g" powheg.input
echo "iseed $SEED" >> powheg.input

# Run the POWHEG main executable and log the output
time "$POWHEG_MAIN" | tee -a powheg.log

# Create output folder with timestamp and UUID
timestamp=$(date +"%d_%m_%Y")
uuid=$(uuidgen)
output_dir="$OUTFOLDER/$timestamp/$uuid/seed_$SEED"
mkdir -p "$output_dir"

# Copy relevant output files to the output directory
echo "Staging output from $TEMP_DIR to $output_dir"
cp -p pwgpwhgalone-output*.top "$output_dir" 
cp -p pwgevents.lhe            "$output_dir"
cp -p powheg.log               "$output_dir"
cp -p powheg.input             "$output_dir"

# Return to the previous directory
popd

echo "Job completed successfully."

exit 0
"""
    with open(script_path, "w") as file:
        file.write(script_file)
    os.chmod(script_path, 0o755)

    # Submit
    submit_file = f"""
executable      = {script_path}
output          = {output_path}
error           = {error_path}
log             = {log_path}
+RequestRuntime = {runtime}
on_exit_remove  = (ExitBySignal == False) && (ExitCode == 0)\n
max_retries     = 3
requirements    = Machine =!= LastRemoteHost
+OpSysAndVer = "RedHat9"
queue
"""
    with open(submit_path, "w") as file:
        file.write(submit_file)

    return submit_path, job_folder

def write_cleanup_script(dagman_folder):
    script_path = os.path.join(dagman_folder, "cleanup.sh")

    # Script
    script_file = f"""#!/bin/bash

set -x

# PostScript arguments
job_return_value=$1  # Return value of the job
job_name=$2          # Name of the job (from the DAG file)

# Check if the job completed successfully
if [ "$job_return_value" -eq 0 ]; then
    if [ -d "$job_name" ]; then
        # Delete the files
        rm -r "$job_name" 
    fi
fi

# Exit with the job's return value
exit $job_return_value
"""
    with open(script_path, "w") as file:
        file.write(script_file)
    os.chmod(script_path, 0o755)

    return script_path

def create_folder(folder_path, **kwargs):
    os.makedirs(folder_path, exist_ok=kwargs.get('exist_ok', False))
    return folder_path

def get_full_path(relative_path):
    try:
        # Get the current working directory
        current_working_directory = os.getcwd()
        
        # Join the current working directory with the relative path
        full_path = os.path.join(current_working_directory, relative_path)
        
        return full_path
    except Exception as e:
        print(f"Error: {e}")
        sys.exit(1)

def generate_dagman_area(input_gridpack, output_folder, num_evts, num_jobs, initial_seed, runtime, submit):
    MINNLO_TOOLS_PATH, _, LHAPDF_DATA_PATH = check_env()

    # Check runtime value
    if runtime > 168:
        raise Exception(f"You asked a runtime of {runtime} h which exceedes 1 week.")

    # Convert runtime to seconds
    runtime *= 3600

    # transform output_folder to full path
    output_folder = get_full_path(output_folder)
   
    # Create the output_folder directory structure
    events_folder = create_folder(os.path.join(output_folder, "events"))
    dagman_folder = create_folder(os.path.join(output_folder, "dagman"))

    dagman_file = os.path.join(dagman_folder, "dagman.dag")

    cleanup_script_path = write_cleanup_script(dagman_folder=dagman_folder)

    with open(dagman_file, "w") as df:
        df.write("# DAGMan file\n\n")

        for seed in range(initial_seed, initial_seed + num_jobs):
            job_submit_path, job_folder = write_job(input_gridpack=input_gridpack, num_evts=num_evts, seed=seed, dagman_folder=dagman_folder, events_folder=events_folder, runtime=runtime, MINNLO_TOOLS_PATH=MINNLO_TOOLS_PATH, LHAPDF_DATA_PATH=LHAPDF_DATA_PATH)
            df.write(f"JOB job_seed_{seed} {job_submit_path}\n")       
            df.write(f"SCRIPT POST job_seed_{seed} {cleanup_script_path} $RETURN {job_folder}/job_seed_{seed}\n\n")

    if submit:
        submit_condor_job(dagman_file)

    # Define the strings to be printed
    str1 = "output folder: " + str(output_folder)
    str2 = "dagman folder: " + str(dagman_folder)
    str3 = "dagman file  : " + str(dagman_file)
    
    # Compute the length of each string
    len1 = len(str1)
    len2 = len(str2)
    len3 = len(str3)
    
    # Determine the maximum length
    max_length = max(len1, len2, len3)
    
    # Print a line of asterisks of the maximum length
    print('*' * max_length)
    
    # Print the strings
    print(str1)
    print(str2)
    print(str3)

    print('*' * max_length)

def submit_condor_job(dagman_file):
    subprocess.run(["condor_submit_dag", dagman_file], check=True) 
    
def main():
    args = parser()

    generate_dagman_area(input_gridpack=args.input_gridpack, output_folder=args.output_folder, num_evts=args.num_evts, num_jobs=args.num_jobs, initial_seed=args.initial_seed, runtime=args.runtime, submit=args.submit)

if __name__ == "__main__":
    main()