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
    if (not MINNLO_TOOLS_PATH) or (not CONDA_PATH):
        sys.stderr.write("Source the environment before launching the script 'source <MINNLO_TOOLS_FOLDER>/env.sh'\n")
        sys.exit()
    else:
        sys.stdout.write("Environment found.")
        return MINNLO_TOOLS_PATH, CONDA_PATH

def write_single_stage_job(stage, seed, dagman_folder, gridpack_folder, runtime, MINNLO_TOOLS_PATH, CONDA_PATH):
    job_folder = os.path.join(dagman_folder, f"stage_{stage}", f"seed_{seed}")
    os.makedirs(job_folder, exist_ok=False)

    script_path = os.path.join(job_folder, "job.sh")
    submit_path = os.path.join(job_folder, "job.sub") 
    output_path = os.path.join(job_folder, "job.out")
    error_path = os.path.join(job_folder, "job.err")
    log_path = os.path.join(job_folder, "job.log")

    # Script
    script_file = f"""
#!/bin/bash

set -e

# Activate the conda environment
conda={CONDA_PATH}
source "$(conda info --base)/etc/profile.d/conda.sh"
conda activate minnlo-env

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
exectutable     = {script_path}
output          = {output_path}
error           = {error_path}
log             = {log_path}
+RequestRuntime = {runtime}
on_exit_remove  = (ExitBySignal == False) && (ExitCode == 0)\n
max_retries     = 3
requirements    = Machine =!= LastRemoteHost
"""
    with open(submit_path, "w") as file:
        file.write(submit_file)

    return submit_path

def write_prepare_job(stage, dagman_folder, gridpack_folder):
    job_folder = create_folder(os.path.join(dagman_folder, f"stage_{stage}"))

    script_path = os.path.join(job_folder, "copy.sh")
    submit_path = os.path.join(job_folder, "job.sub") 
    output_path = os.path.join(job_folder, "job.out")
    error_path = os.path.join(job_folder, "job.err")
    log_path = os.path.join(job_folder, "job.log")

    # Script
    script_file = f"""
#!/bin/bash

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
exectutable     = {script_path}
output          = {output_path}
error           = {error_path}
log             = {log_path}
on_exit_remove  = (ExitBySignal == False) && (ExitCode == 0)\n
max_retries     = 3
requirements    = Machine =!= LastRemoteHost
"""
    with open(submit_path, "w") as file:
        file.write(submit_file)

    return submit_path

def prepare_powheg_card(input_card, initial_seed, num_jobs, stage, gridpack_folder):
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

    with open(os.path.join(gridpack_folder, f"stage_{stage}-powheg.input"), "w") as f:
        f.write(contents)

    # create pwgseeds.dat file
    with open(os.path.join(gridpack_folder, f"stage_{stage}-seeds.dat"), "w") as f:
        for i in range(initial_seed, initial_seed + num_jobs):
            f.write(f"{i}\n")

def create_folder(folder_path, **kwargs):
    os.makedirs(folder_path, exist_ok=kwargs.get('exist_ok', False))
    return folder_path

def generate_dagman_area(input_card, output_folder, num_evts, num_jobs, initial_seed, runtime, submit):
    MINNLO_TOOLS_PATH, CONDA_PATH = check_env()

    #convert runtime to seconds
    runtime *= 3600
   
    # Create the output_folder directory structure
    gridpack_folder = create_folder(os.path.join(output_folder, "gridpack"))
    dagman_folder = create_folder(os.path.join(output_folder, "dagman"))

    dagman_file = os.path.join(dagman_folder, "dagman.dag")

    with open(dagman_file, "w") as df:
        df.write("# DAGMan file\n\n")

        stages = [0, 1, 2, 3]
        for stage in stages:
            prepare_powheg_card(input_card=input_card, initial_seed=initial_seed, num_jobs=num_jobs, stage=stage, gridpack_folder=gridpack_folder) 

            job_prepare_path = write_prepare_job(stage=stage, dagman_folder=dagman_folder, gridpack_folder=gridpack_folder)

            df.write(f"# Stage {stage}: PREPARE\n")
            df.write(f"JOB prepare_stage_{stage} {job_prepare_path}\n\n")
            
            df.write(f"# Stage {stage}: JOBS\n")
            for seed in range(initial_seed, initial_seed + num_jobs):
                job_submit_path = write_single_stage_job(stage=stage, seed=seed, dagman_folder=dagman_folder, gridpack_folder=gridpack_folder, runtime=runtime, MINNLO_TOOLS_PATH=MINNLO_TOOLS_PATH, CONDA_PATH=CONDA_PATH)
                df.write(f"JOB stage_{stage}_seed_{seed} {job_submit_path}\n")                

            child_string = ""
            for seed in range(initial_seed, initial_seed + num_jobs):
                child_string += f"stage_{stage}_seed_{seed} " 
            df.write(f"PARENT prepare_stage_{stage} CHILD {child_string}\n\n")

        # Write the PARENT CHILD syntax
        df.write("\n")
        for stage in stages[:-1]:
            parent_string = ""
            child_string = ""
            for seed in range(initial_seed, initial_seed + num_jobs):
                parent_string += f"stage_{stage}_seed_{seed} "
                child_string += f"stage_{stage + 1}_seed_{seed} "
            df.write(f"PARENT {parent_string} CHILD {child_string}\n")

def main():
    args = parser()

    generate_dagman_area(
        input_card=args.input_card,
        output_folder=args.output_folder,
        num_evts=args.num_evts,
        num_jobs=args.num_jobs,
        initial_seed=args.initial_seed,
        runtime=args.runtime,
        submit=args.submit
    )

if __name__ == "__main__":
    main()