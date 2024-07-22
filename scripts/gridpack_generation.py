import os
import sys
import re
import subprocess
import argparse

def parser():
    parser = argparse.ArgumentParser(description="Prepare and submit jobs to generate a MiNNLO gridpack")
    parser.add_argument("input_card", help="Path to the POWHEG input card")
    parser.add_argument("output_folder", help="Path to the output folder where the gridpack will be stored")
    parser.add_argument("-e", "--num_evts", type=int, default=10000, help="Number of events per job")
    parser.add_argument("-j", "--num_jobs", type=int, default=1000, help="Number of jobs")
    parser.add_argument("-i", "--initial_seed", type=int, default=1, help="Initial seed for the job submission")
    parser.add_argument("-r", "--runtime", type=int, default=1, help="Max. runtime for each job")
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

def write_single_stage_job(stage, seed, dagman_folder, gridpack_folder, runtime, MINNLO_TOOLS_PATH, CONDA_PATH, LHAPDF_DATA_PATH):
    job_folder = os.path.join(dagman_folder, f"stage_{stage}", f"seed_{seed}")
    os.makedirs(job_folder, exist_ok=False)

    script_path = os.path.join(job_folder, f"job_stage_{stage}_seed_{seed}.sh")
    submit_path = os.path.join(job_folder, f"job_stage_{stage}_seed_{seed}.sub") 
    output_path = os.path.join(job_folder, f"job_stage_{stage}_seed_{seed}.out")
    error_path  = os.path.join(job_folder, f"job_stage_{stage}_seed_{seed}.err")
    log_path    = os.path.join(job_folder, f"job_stage_{stage}_seed_{seed}.log")

    # Script
    script_file = f"""#!/bin/bash

set -e

# Activate the conda environment
source "$({CONDA_PATH} info --base)/etc/profile.d/conda.sh"
conda activate minnlo-env
export LHAPDF_DATA_PATH="{LHAPDF_DATA_PATH}"

# Generation step
pushd {gridpack_folder}
time {MINNLO_TOOLS_PATH}/POWHEG-BOX-V2/Zj/ZjMiNNLO/pwhg_main<<EOF
{seed}
EOF

popd

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
max_retries     = 20
requirements    = Machine =!= LastRemoteHost
+OpSysAndVer = "RedHat9"
queue
"""
    with open(submit_path, "w") as file:
        file.write(submit_file)

    return submit_path

def write_prepare_job(stage, dagman_folder, gridpack_folder):
    job_folder = create_folder(os.path.join(dagman_folder, f"stage_{stage}"))

    script_path = os.path.join(job_folder, f"copy_stage_{stage}.sh")
    submit_path = os.path.join(job_folder, f"copy_stage_{stage}.sub") 
    output_path = os.path.join(job_folder, f"copy_stage_{stage}.out")
    error_path  = os.path.join(job_folder, f"copy_stage_{stage}.err")
    log_path    = os.path.join(job_folder, f"copy_stage_{stage}.log")

    # Script
    script_file = f"""#!/bin/bash

set -e

pushd {gridpack_folder}
cp -v stage_{stage}-powheg.input powheg.input
cp -v stage_{stage}-seeds.dat pwgseeds.dat
popd

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
on_exit_remove  = (ExitBySignal == False) && (ExitCode == 0)\n
max_retries     = 20
requirements    = Machine =!= LastRemoteHost
+OpSysAndVer = "RedHat9"
queue
"""
    with open(submit_path, "w") as file:
        file.write(submit_file)

    return submit_path

def prepare_powheg_card(input_card, initial_seed, num_jobs, stage, num_evts, gridpack_folder):
    with open(input_card, "r") as f:
        contents = f.read()
        contents = re.sub(r".*manyseeds.*", "manyseeds 1", contents)
        contents = re.sub(r".*maxseeds.*", f"maxseeds {num_jobs}", contents)
        if   stage == 0:
            contents = re.sub(r".*xgriditeration.*", "xgriditeration 1", contents)
            contents = re.sub(r".*parallelstage.*", "parallelstage 1", contents)
        elif stage == 1:
            contents = re.sub(r".*xgriditeration.*", "xgriditeration 2", contents)
            contents = re.sub(r".*parallelstage.*", "parallelstage 1", contents)
        elif stage == 2:
            contents = re.sub(r".*xgriditeration.*", "xgriditeration 2", contents)
            contents = re.sub(r".*parallelstage.*", "parallelstage 2", contents)
        elif stage == 3:
            contents = re.sub(r".*xgriditeration.*", "xgriditeration 2", contents)
            contents = re.sub(r".*parallelstage.*", "parallelstage 3", contents)
        else:
            sys.stderr.write("Unrecognised stage of generation\n")
            sys.stderr.write("Available stages are: 0, 1, 2, 3\n")
            sys.exit()
        # patch num events
        contents = re.sub(r".*numevts.*", f"numevts {num_evts}", contents)
        

    with open(os.path.join(gridpack_folder, f"stage_{stage}-powheg.input"), "w") as f:
        f.write(contents)

    # create pwgseeds.dat file
    with open(os.path.join(gridpack_folder, f"stage_{stage}-seeds.dat"), "w") as f:
        for i in range(initial_seed, initial_seed + num_jobs):
            f.write(f"{i}\n")

def write_finalise_job(dagman_folder, output_folder):
    script_path = os.path.join(dagman_folder, "compress.sh")
    submit_path = os.path.join(dagman_folder, "compress.sub") 
    output_path = os.path.join(dagman_folder, "compress.out")
    error_path  = os.path.join(dagman_folder, "compress.err")
    log_path    = os.path.join(dagman_folder, "compress.log")

    # Script
    script_file = f"""#!/bin/bash

set -e

pushd {output_folder}
tar -czf gridpack.tar.gz gridpack
popd

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
on_exit_remove  = (ExitBySignal == False) && (ExitCode == 0)\n
max_retries     = 20
requirements    = Machine =!= LastRemoteHost
+OpSysAndVer = "RedHat9"
queue
"""
    with open(submit_path, "w") as file:
        file.write(submit_file)

    return submit_path

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

def generate_dagman_area(input_card, output_folder, num_evts, num_jobs, initial_seed, runtime, submit):
    MINNLO_TOOLS_PATH, CONDA_PATH, LHAPDF_DATA_PATH = check_env()

    #convert runtime to seconds
    runtime *= 3600
   
    # transform output_folder to full path
    output_folder = get_full_path(output_folder)
   
    # Create the output_folder directory structure
    gridpack_folder = create_folder(os.path.join(output_folder, "gridpack"))
    dagman_folder = create_folder(os.path.join(output_folder, "dagman"))

    dagman_file = os.path.join(dagman_folder, "dagman.dag")

    with open(dagman_file, "w") as df:
        df.write("# DAGMan file\n")

        stages = [0, 1, 2, 3]
        for stage in stages:
            prepare_powheg_card(input_card=input_card, initial_seed=initial_seed, num_jobs=num_jobs, stage=stage, num_evts=num_evts, gridpack_folder=gridpack_folder) 

            job_prepare_path = write_prepare_job(stage=stage, dagman_folder=dagman_folder, gridpack_folder=gridpack_folder)

            df.write(f"\n")
            df.write(f"# Stage {stage}: PREPARE\n")
            df.write(f"JOB prepare_stage_{stage} {job_prepare_path}\n\n")
            
            df.write(f"# Stage {stage}: JOBS\n")
            for seed in range(initial_seed, initial_seed + num_jobs):
                job_submit_path = write_single_stage_job(stage=stage, seed=seed, dagman_folder=dagman_folder, gridpack_folder=gridpack_folder, runtime=runtime, MINNLO_TOOLS_PATH=MINNLO_TOOLS_PATH, CONDA_PATH=CONDA_PATH, LHAPDF_DATA_PATH=LHAPDF_DATA_PATH)
                df.write(f"JOB stage_{stage}_seed_{seed} {job_submit_path}\n")                

        finalise_job_path = write_finalise_job(dagman_folder=dagman_folder, output_folder=output_folder)
        df.write("\n")
        df.write(f"# Finalise job\n")
        df.write(f"JOB finalise {finalise_job_path}\n")

        # Write the PARENT CHILD syntax
        df.write("\n")
        grid_jobs_for_stage = []
        for stage in stages:
            jobs_string = ""
            for seed in range(initial_seed, initial_seed + num_jobs):
                jobs_string += f"stage_{stage}_seed_{seed} "
            grid_jobs_for_stage.append(jobs_string)

        df.write(f"PARENT prepare_stage_0 CHILD {grid_jobs_for_stage[0]}\n")
        for stage in stages[1:]:
            df.write(f"PARENT {grid_jobs_for_stage[stage - 1]} CHILD prepare_stage_{stage}\n")
            df.write(f"PARENT prepare_stage_{stage} CHILD {grid_jobs_for_stage[stage]}\n") 
        df.write(f"PARENT {grid_jobs_for_stage[-1]} CHILD finalise\n")

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

    generate_dagman_area(input_card=args.input_card, output_folder=args.output_folder, num_evts=args.num_evts, num_jobs=args.num_jobs, initial_seed=args.initial_seed, runtime=args.runtime, submit=args.submit)

if __name__ == "__main__":
    main()