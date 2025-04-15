import os
import sys
import pandas as pd
import shutil

import PyFiler.NEMSRestartIO as nemres

dll_dirs = ([r"C:\Program Files (x86)\Intel\oneAPI\compiler\2023.2.1\windows\redist\intel64_win\compiler",
             r"C:\Windows\System32"])

for i in dll_dirs:
    if i not in sys.path:
        sys.path.append(i)
        os.add_dll_directory(i)

for i in ["PyFiler"]:
    j = os.path.join(os.getcwd(), i)
    if j not in sys.path:
        sys.path.append(j)

import pyfiler1


def convert_unf_to_npz(my_file):
    """ Convert restart file from unf format to npz format

    Parameters
    ----------
    my_file : string
        name of UNF restart file to convert
    """
    tmp_file = "restart_temp.unf"

    shutil.copy('PyFiler/FILELIST', 'FILELIST')
    
    shutil.copy2(my_file, tmp_file)
    pyfiler1.utils.read_filer(tmp_file)

    NEMSVardf = pd.read_csv('NEMSVardf.csv', index_col = 'Fortran Variable Name')
    NEMSVardf.rename(columns={'Fortran Variable Name.1' : 'Fortran Variable Name'}, inplace=True)

    nemres.to_npz(pyfiler1, NEMSVardf)
    os.remove(tmp_file)


if __name__ == "__main__":
    n = "restart.unf"
    convert_unf_to_npz(n)