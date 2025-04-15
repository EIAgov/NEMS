# -*- coding: utf-8 -*-
"""
Created on Sept 27 2024

@author: Claire Su
"""
from datetime import datetime
import os, sys
import numpy as np

dll_dirs = ([r"C:\Program Files (x86)\Intel\oneAPI\compiler\2023.2.1\windows\redist\intel64_win\compiler",
            r"C:\Windows\System32"])

for i in dll_dirs:
    if i not in sys.path:
        sys.path.append(i)
        os.add_dll_directory(i)

j = os.path.join(os.getcwd(), "PyFiler")
if j not in sys.path:
    sys.path.append(j)

def main_check_nan_inf(pyfiler1):
    """The major method to run this helper module. It receives inanq setting, conducts the flow 
    and retunr a dictionary of restart variables with NaN, and/or infinity values. 

    Parameters
    ----------
    pyfiler1 : module 
        the restart file pyfiler object
    
    Returns
    -------
    None.
    """
    log_it2("We are in check_nan_inf.py, start to call check_for_nan_inf()")
    nan_inf_list = check_for_nan_inf(pyfiler1)

    # if there is NaN infinity values in the list, print message and populate the 2 debugging files
    if not nan_inf_list:
        log_it2("end of check_for_nan_inf(), no NaN/inf found.")
        return
    else:
        log_it2("end of check_for_nan_inf(), NaN/inf found!")
        log_it2("Start to print and generate 2 files: debug.nan_inf_dictionary.txt, debug.nan_inf_list.txt")
        # now assemble info for nohup.out print out
        print_dict = assemble_printout(pyfiler1, nan_inf_list)
        log_it2("end of the 2 file (debug.nan_inf_dictionary.txt, debug.nan_inf_list.txt) generation.")

        # TODO: generate a npz file before call write_stop_file (stop.txt). Place holder here
        #np.savetxt("userful.csv",pyfiler1.mpblk.pmgtr, delimiter=',')
        #to_npz(pyfiler1, vartable)

        # Temp comment out this feature (to generate a stop.txt file) since all model cases has NaN, inf exists
        #write_stop_file()

def check_for_nan_inf(pyfiler1):
    """Iterate through each variable in pyfiler and exam any year data value is NaN or inf 

    Parameters
    ----------
    pyfiler1 : module
        the restart file pyfiler object

    Returns
    -------
    nan_inf_list : list
        the list with the variables contains NaN and/or inf
    """
    # get the name list of common blocks
    t1 = [i for i in pyfiler1.__dict__.keys() if not i.startswith("_") and not i.endswith("_run")] #144
    t1 = [i for i in t1 if not isinstance(pyfiler1.__dict__[i], str)] # get a 144 element list
    
    # look for good pyfiler attributes. For example: pyfiler1.pqmatch is not the target (not good) but pyfiler1.other is what we want.
    # temp shall trimmed to 117 elements 
    temp=[]
    for i in t1:
        temp_attribute_dict_keys_list=list(vars(eval(f"pyfiler1.{i}")).keys())
        # if the attribute dict keys all starts with "_", we don't care about that attribute
        # otherwise, it's a good/targeted pyfiler attribute; append to our temp list
        tt=[i for i in temp_attribute_dict_keys_list if not i.startswith("_")]
        if tt: temp.append(i)

    cmn_blks = [i.lower() for i in temp if i.lower() not in ["nchar", "other", "utils"]]
    cmn_blks.sort()      #114

    nan_inf_list = []
    for i in cmn_blks:
        # convert common block's dict_keys to list: my_keys
        my_keys = []
        try:
            my_keys = list(eval(f"pyfiler1.{i}").__dict__.keys())
        except:
            print(f"{i}: unable to process common block")
        ignore_me = ["__doc__"]
        my_keys = [k for k in my_keys[:] if k not in ignore_me]
       
        # look for nan and inf
        for k in my_keys:
            try:
                v = eval(f"pyfiler1.{i}.{k}")
                if np.isnan(v).any() or np.isinf(v).any():
                    nan_inf_list.append(f"{i}.{k}")
            except:
                pass
                # the except is because string value, not numeric. Too many fields in restart file. Save I/O without output it.
                #print(f"{i}.{k}: unable to process. The value is a string, not numeric")
   
    return nan_inf_list

def assemble_printout(pyfiler1, nan_inf_list):
    """Iterate through the passed-in NaN/inf variable list and populate the index location of those NaN/inf variables.

    Parameters
    ----------
    pyfiler1 : module
        the restart file pyfiler object
    
    nan_inf_list : list
        the list with the variables contains NaN and/or inf

    Returns
    -------
    print_dict : dict
        the dictionary with variable name as key and NaN/inf index position as value
    """
    # if nan_inf_list is empty, do nothing
    if not nan_inf_list: return {}
    print_dict={}
    for i in nan_inf_list:
        tmp_nan=np.argwhere(np.isnan(eval(f"pyfiler1.{i}"))).tolist()
        tmp_inf=np.argwhere(np.isinf(eval(f"pyfiler1.{i}"))).tolist()
        tmp_nan.extend(tmp_inf)
        
        print_dict[i]=tmp_nan

    # generate logs for debug:
    write_nan_inf_list(nan_inf_list)
    write_nan_inf_dict(print_dict)
    return print_dict

def write_stop_file():
    """Write out a stop.txt to stop NEMS run in queue or local.
    """
    open("stop.txt", "w+").close()

def write_nan_inf_dict(print_dict):
    """write a debug log file debug.nan_inf_dictionary.txt to address the variable NaN/inf index position 

    Parameters
    ----------
    print_dict : dict
        the dictionary with variable name as key and NaN/inf index position as value
    
    Returns
    -------
    None.
    """
    result_list = list(print_dict.items())
    with open("debug.nan_inf_dictionary.txt", 'a+', encoding='utf-8') as f:
        for k,v in result_list:
            f.write(f'{k} has NaN or infinity at {v}\n')
        f.write('------------------------\n')

def write_nan_inf_list(nan_inf_list):
    """write a debug log file debug.nan_inf_list.txt to address the variables contains NaN and/or inf

    Parameters
    ----------
    nan_inf_list : list
        the list with the variables contains NaN and/or inf
    
    Returns
    -------
    None.
    """
    print("The list of restart variables contain NaN or infinity:\n")
    with open("debug.nan_inf_list.txt", 'a+', encoding='utf-8') as f:
        f.write('The list of restart variables contain NaN or infinity:\n')
        for i in nan_inf_list:
            print(f'{i}\n')
            f.write(f'{i}\n')
        f.write('------------------------\n')

# keep below for debug in local run testing----------
def log_it2(s):
    """Append a formatted message to LOGFILE (nohup.out).

    LOGGFILE is a module-level hardcoded parameter.

    sys.stdout.flush() and sys.stederr.flush() ensure that
    nothing is left in the buffer.

    Parameters
    ----------
    n : integer
        cycle number
    s : string
        message to write in logfile
    
    Returns
    -------
    None.
    """
    #LOGFILE="nohup_test_nan.out"
    LOGFILE="nohup.out"
    with open(LOGFILE, "a", encoding="utf-8") as f:
        f.write(f"{datetime.now()} :: {s}\n")
        sys.stdout.flush()
        sys.stderr.flush()


if __name__ == "__main__":
    import pyfiler1
    pyfiler1.utils.read_filer('restart_t0929a.unf')
    '''
    # for debug, mock testing NaN data:
    pyfiler1.mpblk.pmgtr[0,0] = np.nan
    pyfiler1.mpblk.pmgtr[0,1] = np.inf
    print(pyfiler1.mpblk.pmgtr[0])
    print(pyfiler1.rseff.ckconwt[11,1,6,0])  # pyfiler1.rseff.ckconwt.shape=(61, 3, 9, 3)
    '''
    # performance test:
    files =('nohup.out','debug.nan_inf_dictionary.txt','debug.nan_inf_list.txt')
    for f in files: open(f, "w").close()
    log_it2("Performance Test: Begin - start check_for_nan_inf()")
    for i in range(1000): main_check_nan_inf(pyfiler1)
    log_it2("Performance Test: End - end of check_for_nan_inf()")
    print("done!")
    os.sys.exit()

    main_check_nan_inf(pyfiler1)
    #if inanq==1:write_stop_file()

    #np.savetxt("userful.csv",pyfiler1.mpblk.pmgtr, delimiter=',')
    #to_npz(pyfiler1, vartable)
