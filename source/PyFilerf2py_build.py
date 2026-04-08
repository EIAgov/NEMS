import numpy.f2py
import sys
import os
import multiprocessing
from numpy.f2py.auxfuncs import gentitle
sys.path.append(r"C:\Program Files (x86)\Intel\oneAPI\compiler\2023.2.1\windows\redist\intel64_win\compiler")
os.add_dll_directory(r"C:\Program Files (x86)\Intel\oneAPI\compiler\2023.2.1\windows\redist\intel64_win\compiler")

rundir=os.path.dirname(os.path.realpath(__file__))

def run_pyfiler1():
    """Create and build Python C module.c and .pyf files of the pyfiler1.
    """
    numpy.f2py.run_main(['-h',
                        'pyfiler1.pyf',
                        'PyFiler.f90',
                        'util_copyadjusted.f90',
                        'util_sumqas.f90','util_avepas_avepasa.f90','util_minit.f90',
                        'util_call_pyd1_module.f90',
                        '-m', 'pyfiler1',
                        # 'only:', 'init_filer', 'get_data',
                        '--include-paths', r'../includes',
                        '--overwrite-signature'])
        
def build_pyd_files():
    """The main flow control method to process pyfiler1 and have the module.c and .pyf files generated.
    """
    run_pyfiler1()
    print('exiting here &&&***&&&')
    return

if __name__ == "__main__":
    build_pyd_files()
