# minnlo-tools

MiNNLO installation and related scripts

## Prepare the environment

Make sure you have a recent version of `conda` installed. In any case please run

```bash
conda update conda
```

Create the working environment by running

```bash
conda env create -f minnlo-env.yml
```

Pack the environment to be submitted in the condor jobs. Install `conda-pack`

```bash
conda install -c conda-forge conda-pack
```

and then create and pack a minimal environment

```bash
conda env create -f minnlo-env-minimal.yml
conda pack -n minnlo-env-minimal -o minnlo-env-minimal.tar.gz
```

Activate the conda environment with

```bash
conda activate minnlo-env
```

You are ready to go

## For MAC users

You need to install `svn` in order to be able to download POWHEG

Step 1: install `homebrew`

```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

Step 2: install `svn` and `wget`

```bash
brew install svn
brew install wget
```
