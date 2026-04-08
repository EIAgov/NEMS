# User guide to files within the NEMS/scripts/restart folder in NEMS

This guide walks you through the process of using RestartResizeNPZ or compare_restart located in this folder.

---
# RestartResizeNPZ.py 
## Purpose:
To add/redimension/remove variables from NEMS, a tool to resize the restart.npz file of NEMS was needed. RestartResizeNPZ's purpose is to automate this process. This will be similiar to the existing WEPS HDF restart code.

## Usage:
The resize tool will be run when varaibles are being added, resized/redimensioned, or removed.
This code can be run simultaneously while regenerating a new .pyd file. For more on the .pyd file, see scripts/setup.

## Step 1: Update the `NEMS/input/dict.txt` file

NEMS has a file in input file named dict.txt for documentation and explanation of Global Data Structures for NEMS. This file explains when reading in the restartnpz file exactly what the variable is, it's dimensions, common blocks. When updating, you want to add any new variables to the end of
the common block.

---

## Step 2: Update the `NEMS/includes` file(s) associated with your variable.

The includes files are the building block for the fortan pieces of NEMS. This explains to NEMS what common block the variable is housed, what the definition of the variables is, its variable type and dimensions. When updating the includes files, make sure that you are adding any new variables 
to the end of the common block. Make sure if equivalence statements exist, that you update your equivalence statement to be correctly sized when adding/redimensioning/removing a variable.

## Step 3: Set up a run to generate a new .pyd

Edit the `NEMS/scripts/setup/input/init_configs.csv` file to have the key 'make_pyd_flag' set to zero.  The `init_configs.csv` file is the core configuration file for NEMS to understand if a new .pyd is to be built. Once that flag is changed, this signifies to NEMS that a new .pyd shall be built.
Now that we have told NEMS we're generating a new .pyd file, go into the `NEMS/source` folder and delete the builddir to make sure there is no lingering build files. Then, you can click from the NEMS folder `RunNEMS.bat` to launch the Graphic User Interface (GUI) to begin set up. Once the GUI launches, 
click the box for `Only create a run folder` so your NEMS folder will generate the new .pyd items. This compilation takes some time, so move on to Step 4 while this runs in the background.

## Step 4: Use RestartResizeNPZ.py to generate a new restart npz correctly sized

While Step 3 is running, you may now start to resize the restart.npz file. To do this, you will need:
1. a restart file from a recently completed run.
1. a NEMSVardf.csv from that same run.
1. The PyfilerWrapper code located in NEMS/Scripts/PyFiler
1. a dictionary from your new dict.txt file that has the items resized.
1. Changing the inputs of the user.file_map inside of RestartResizeNPZ.py

Once those items are in a folder together, simply run RestartResizeNPZ.py from your Python IDE of choice.


## Step 5: Change scedes file and test

Now comes the fun part! Update the restart file location in the scedes file and run a test.

#compare_restart.py
## Purpose:
Compare restart.npz files to find differences.

## Usage:
Compare restart files to see the differences between multiple restart files. The differences will be tested against a tolerance and see if run results are the same.