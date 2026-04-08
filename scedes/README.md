# ​The Scenario-Descriptor (SCEDES) File 

## What is it?

The settings for a particular run, or scenario of NEMS, are stored in a SCEDES. NEMS scedes files identify all settings for a NEMS run. NEMS runs have three types of settings: input file names, object file names, and runtime options, usually integers, which are interpreted as switches or numerical values for key input assumptions. A scedes file contains all the settings used every time a NEMS run is made. 

## SCEDES Headers
Key :

- Key used to designate the variable name to store the NEMS input file name/path, object file names, and runtime option.

Value:

- Value of the NEMS input file name/path, object file names, and runtime option.

ModuleOwner

- 3 letters Model Owner notation (e.g. EXW, EXM, EXK, EXR, EXQ, etc.) corresponding with the model flags in the SCEDES file and UsedInFilelist value. Example: If SCEDES flag EXI is set to 1, all corresponding EXI values set here under "ModuleOwner" AND "UsedInFilelist" set to 1 will populate the generation of FILELIST with EXI relevant files paths.

Description

- Paragraph or sentence description of the corresponding key and value pair.

UsedInMoreOpt

- Boolean value (1 = True; 0 = False) to indicate of the corresponding Key and Value pairs are used to generate an input in MOREOPT.

UsedInFilelist

- Boolean value (1 = True; 0 = False) to indicate of the corresponding Key and Value pairs are used to generate an input in FILELIST.

Filelist_format

- String of text used by Fortran models that needs to be appended at the end of every line in FILELIST.
- Example: READ SEQUENTIAL FORMATTED 80