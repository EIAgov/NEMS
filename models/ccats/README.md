# CCATS Module
Carbon Capture, Allocation, Transport and Storage Module

## Documentation
CCATS documentation is written in Sphinx.
All associated files will be located in models/ccats/docs.

Two versions of the documentation are currently accessible outside of the repository:
- Stable: file://fs-f3/OEA/IIEA/CO2/CTUS/Development/6_Documentation/stable/html/index.html
- In development: file:///L:/mid/jab/git/NEMS/models/ccats/docs/build/html/index.html

Note: External web access is needed to render the math syntax, therefore the model formulation will not be readable on nem3.

## Running CCATS
CCATS can be run both from NEMS or standalone.

### Running CCATS in NEMS
- Set "EXQ" == 1 in the run scedes file.
- Modify relevant flags in the "setup" files within the CCATS "input" directories.
- Run NEMS using the "RunNEMS" bat file.

### Running CCATS Standalone
- Setup the restart file:
	- Select a restart file for the run. This can come from any NEMS run. Copy this restart file into the "nems\models\ccats" directory as "restart.npz".
	- Copy "pyfiler1", "keys.sed" and "FILELIST" from the same run as the restart file into the "nems\models\ccats" directory. 
	- Copy "dict.txt" and "varlist.txt" from the same run as the restart file into the "nems\models\ccats\input" directory. 
- Open the "nems\models\ccats" in an IDE and assign a python environment. The recommended environment for a standalone run is the default NEMS python environment (i.e. O:\python_environments\aeo2025_py311_d)
- Modify relevant flags in the "setup" files within the CCATS "input" directories.
- Run ccats from "ccats.py" 