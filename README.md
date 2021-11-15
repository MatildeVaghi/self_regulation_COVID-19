# Self_Regulation_Ontology
![CircleCI](https://circleci.com/gh/poldrack/Self_Regulation_Ontology.svg?style=svg&circle-token=c2c503d9ef106e45769fa00ca689b3b10d882c9d)

This is the main repository for analyses and data for the project related to self-regulation during the early phases of the COVID-19 pandemic.

## Setting up the repository

In order to use the code, you first need to create a version of the settings file, using the following steps:

1. Copy the file "Self_Regulation_Settings_example.txt" to a new file called "Self_Regulation_Settings.txt"

2. Using your favorite text editor, edit the file to specify the location of the project directory on the line
starting with "base directory". 
Note: If you do not create a settings file, one will be created by the setup.py file (see below) with default values

## Organization of the repository

Data: contains all of the of derived data collected during the emergence of the pandemic in 2020
Data_crisis: data for training dataset,  Corona Health Survey and Impact Survey (Nikolaidis et al., 2021) 
Data_master: data for training dataset, self-regulation (Eisenberg et al., 2019)
Data_psych: data for training dataset, psychiatric symptoms (Rouault et al., 2018)
data_preparation: code for preparing derived data
utils: utilitis for loading/saving data and metadata
expanalysis_mv: tools needed for data preparation

other directories are specific to particular analyses 
those presented in the paper are stored within dimensional_structure"


## Setting up python environment

### for all analyses besides data_preparation
pip install -r requirements1.txt
pip install -r requirements2.txt
python setup.py install
rpy2 needs to be installed

rpy2 can be install using conda install rpy2
if errors occur when install R packages in the conda environment these commands may fix the issues:
conda install gxx_linux-64
conda install gfortran_linux-64

### R setup
install:

GPArotation
missForest
psych
lme4
qgraph
mpath
dynamicTreeCut

### Example sequence of installation steps using anaconda
* conda create -n SRO python=3.5.3
* source activate SRO
* pip install -r requirements1.txt
* pip install -r requirements2.txt
* conda install -c r rpy2
* conda install -c r r
* pip install git+https://github.com/IanEisenberg/expfactory-analysis

## Docker usage
to build run:
`docker build --rm -t sro .`

Mount the Data and Results directory from the host into the container at /SRO/Data and /SRO/Results respectively

To start bash in the docker container with the appropriate mounts run: 
docker run --rm --entrypoint /bin/bash --mount type=bind,src=/Users/Matilde/Documents/Experiments/Self_Regulation_Ontology-master_COVID,dst=/SRO --mount type=bind,src=/Users/Matilde/Documents/Experiments/Self_Regulation_Ontology-master_COVID/Data,dst=/Data  -ti -p 8888:8888 sro
