

### TO RUN:

# Note: I'm using the aeo2024_py311_mc6 enviroment that Mike Cole has been working with because it has the
# pyarrow library

# Because PyFiler was not originally designed to be run in a very standalone way as a utility script like this, it requires
# a certain folder structure in the scripts/PyFiler folder of your git directory. The easiest way to populate this folder
# with the correct contents is to use some recently completed run.

# Below are are instructions to run this script.

# Find a recently completed run and do the following:
# 1)  From the completed run, copy over the contents of Pyfiler/input/ dict.txt and varlist.txt to
#     the scripts/Pyfiler/input folder of your git directory
# 2) From the completed run, copy  the FILELIST file from the Pyfiler folder and add it the Pyfiler folder
#    from your git directory
# 3) Find the restart file from which you want to extract the information from
#      - Copy the .unf file to the hmm/scripts folder, and give it a meaninful name (restart_initialize_GDS.unf)
#      - Make sure the name to the restart file is updated below for the restart_file variable
#      - Make another copy of the restart file, and copy it to scripts/PyFiler of your git directory.
#      - Rename the .unf file in scripts/Pyfiler to restart.unf
# 4) Run this script, there should now be an updated .txt file in the hmm/ToAIMMS folder
# 5) Be sure to not commit any changes in the PyFiler folder. In fact, it is probably a good idea to clear out any changes
#    you made in there to ensure it does not conflict with a future run

import numpy as np
import os
import pandas as pd
import pyarrow as pa
import sys
from pathlib import Path

# Paths to Fortran compilers
sys.path.append(r"C:/Program Files (x86)/Intel/oneAPI/compiler/2023.2.1/windows/redist/intel64_win/compiler")
os.add_dll_directory(r"C:/Program Files (x86)/Intel/oneAPI/compiler/2023.2.1/windows/redist/intel64_win/compiler")

# Input file location/names
# Because PyFiler will require us to change the working directory to the scripts/Pyfiler folder,
# it is best if we set these as absoulte paths now.
gds = Path(os.getcwd()).parent.absolute() / "toAIMMS" / "GlobalDataToHMM_Init.txt" # Name of output .txt file
f_in = Path(os.getcwd()).parent.absolute() / "input" / "h2putvars.txt" # Name of putvars txt file
restart_file = Path(os.getcwd()) / "RestartHMMAttempt.unf" # source restart file
# TODO Make it so that dict.txt is dynamic, may be limited by PyFiler, however.

# Add path to where PyFiler is so we can import the modules
sys.path.insert(1, '../../../scripts/PyFiler')
import pyfiler
import PyFilerWrapper as pfw

# PyFiler requires its directory to be in the PyFiler folder.
os.chdir("M:/ogs/syk/git/NEMS_Ash/scripts/PyFiler")

# Read in specified restart file data
pyfiler.utils.read_filer(restart_file)

# Ideally, this dict path would be outside of the PyFiler folder, but the
# read_filer function really wants dict.txt to be in Pyfiler/input,
# So I'm having it read it from there for consistency.
dict_path = Path(os.getcwd()).absolute()  / "input"/ "dict.txt"
# Read in dict.txt from the Pyfiler/input folder
df_in = pfw.ParseDict(dict_path)[0] \
    .set_index(['Common Block Name', 'Fortran Variable Name'])

# Parse the h2putvar file. (modified code that Mike Cole uses in main.py)
def parse_putget_file(f_in, pyfiler_dict):
    """
        pyfiler_dict should be:
            PyFiler.PyFilerWrapper.ParseDict('dict.txt')[0] \
                  .set_index(['Common Block Name', 'Fortran Variable Name'])
    """
    varlistfile = f_in
    z = pyfiler_dict

    df = pd.read_csv(varlistfile, header=None) \
        .rename(columns={0: 'common_block', 1: 'variable'})
    for i in df.columns:
        df[i] = df[i].apply(lambda x: x.split('=')[1].strip())

    df['Common Block Name'] = df['common_block'].str.upper()
    df['Fortran Variable Name'] = df['variable'].str.upper()
    df['my_name'] = df['common_block'] + '.' + df['variable']
    df = df.set_index(['Common Block Name', 'Fortran Variable Name'])

    my_dim = {}
    for i in df.index:
        temp = z.loc[i, 'Dimensions Parameters'].split(',')
        temp2 = []
        for j in temp:
            if str(j).isdigit():
                temp2.append(f"M{j}")
            else:
                temp2.append(j)
        my_dim[df.loc[i, 'my_name']] = temp2

    return my_dim

# Write out tables (modified code that Mike Cole uses in main.py)
def write_tables(pyfiler, my_var):

    my_tables = {}
    for k, v in my_var.items():
        x = np.copy(eval(f"pyfiler.{k.lower()}"))
        my_col = k.replace('.', '_')
        s = x.shape

        if len(s) == 0:
            # CYCLEINFO.CURIRUN, NCNTRL.CURIYR, NCNTRL.FCRL, NCNTRL.NCRL
            my_table = pa.table({'M1': [1], my_col: [int(x)]})

            #  TODO: needed?
            if k == 'CYCLEINFO.CURIRUN':
                pass

        elif len(s) == 1:
            z = np.arange(x.shape[-1]) + 1
            a = np.c_[z, x]
            temp = {v[0]: a[:, 0],
                    my_col: a[:, 1]}
            my_table = pa.table(temp)
            # z = my_table.to_pandas()
            pass

        elif len(s) == 2:
            z = np.arange(x.shape[-1]) + 1
            L = []
            for i in range(s[0]):
                a = np.c_[z, x[i]]
                temp = {v[0]: np.repeat(i + 1, s[len(s) - 1]),
                        v[len(s) - 1]: a[:, 0],
                        my_col: a[:, 1]}
                L.append(pa.table(temp))
            my_table = pa.concat_tables(L)

        elif len(s) == 3:
            z = np.arange(x.shape[-1]) + 1
            L = []
            for i in range(s[0]):
                for j in range(s[1]):
                    a = np.c_[z, x[i, j]]
                    temp = {v[0]: np.repeat(i + 1, s[len(s) - 1]),
                            v[1]: np.repeat(j + 1, s[len(s) - 1]),
                            v[len(s) - 1]: a[:, 0],
                            my_col: a[:, 1]}
                    L.append(pa.table(temp))
            my_table = pa.concat_tables(L)

        elif len(s) == 4:
            z = np.arange(x.shape[-1]) + 1
            L = []
            for i in range(s[0]):
                for j in range(s[1]):
                    for m in range(s[2]):
                        a = np.c_[z, x[i, j, m]]
                        temp = {v[0]: np.repeat(i + 1, s[len(s) - 1]),
                                v[1]: np.repeat(j + 1, s[len(s) - 1]),
                                v[2]: np.repeat(m + 1, s[len(s) - 1]),
                                v[len(s) - 1]: a[:, 0],
                                my_col: a[:, 1]}
                        L.append(pa.table(temp))
            my_table = pa.concat_tables(L)

        elif len(s) == 5:
            print("5 dimensions not yet handled: ", k, v, s)
            my_table = 'nil'

        if not (my_table == 'nil'):
            my_tables[f'{my_col}'] = my_table

            with open(gds, 'a', encoding='utf-8') as f:
                f.write('COMPOSITE TABLE:\n')
            df = my_table.to_pandas()
            df = df.fillna(0)  # TODO: need to investigate nan
            df.to_markdown(gds, index=False, tablefmt='plain', mode='a')
            with open(gds, 'a', encoding='utf-8') as f:
                f.write('\n;\n')

    return my_tables



# create a dict with commonblock.variablename as key, dimension list as value
my_var = parse_putget_file(f_in, df_in)

# Process and write out data
write_tables(pyfiler, my_var)
