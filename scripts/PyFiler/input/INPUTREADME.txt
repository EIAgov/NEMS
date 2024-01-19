This file explains the empty folder structure for PyFiler/Input:

For the reduction of duplicate files within NEMS, Integration has decided that duplicate files should not 
occur in this folder. However, it is important to note that PyFiler requires files to successfully compile. 

These files are:
dict.txt, as PyFiler.f90 requires dict.txt to parse through the restart file. 
FILELIST (in PyFiler folder), as PyFiler.f90 needs FILELIST to find necessary files

The key reason for why dict.txt is needed here is within FILELIST, the relative pathing of dict.txt 
(./input/dict.txt). Without this file in this location, it is not possible for pyfiler to compile and 
read restart files.

If you are using the write_filer() option from pyfiler.utils, then an additional file is needed in the input file,
varlist.txt. This comes from the NEMS/Input folder and is what FILER looks at to see if you can write out to
this file location or not. If you name the output restart file RESTART_1.unf, it will write to that location.
For module specific output, Integration highly recommends a meaningful restart file name, such as RESTART_HSM.unf.