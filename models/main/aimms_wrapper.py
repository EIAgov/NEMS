from datetime import datetime
import numpy as np
import os
import subprocess
import sys
import threading  # used in an eval statement
import time

import aimms_util

from logging_utilities import print_it

MODULE_NAME = "aimms_wrapper.py"

# note that this module has its own logfile, separate from nohup.out
LOGFILE = os.path.join(os.getcwd(), "aimms_wrapper.log")


def log_it(n, s):
    """Write to a logfle.

    Parameters
    ----------
    n : int
        cycle number
    s : string
        message
    """
    with open(LOGFILE, "a", encoding="utf-8") as f:
        f.write(f"{datetime.now()} :: cycle {n} :: {MODULE_NAME} :: {s}\n")
        f.flush()



def aimms_err(n, f):
    """Check aimmRunOnly.err file and print error message if warranted.

    Parameters
    ----------
    n : int
        cycle number
    f : string
        path to "aimmsRunOnly.err" file

    Returns
    -------
    bool
        True if found an error. False otherwise.
    """
    result = False

    # # force error
    # print_it(n, f"checking for suspicious AIMMS err file: {f}")
    # if not os.path.exists("ngas/log"):
    #     os.makedirs("ngas/log")
    # with open(f, "w") as f_out:
    #     f_out.write("FORCED ERRROR")

    if os.path.exists(f):
        with open(f, "r") as f_in:
            z = f_in.readlines()
        z2 = [i for i in z if (not(i.startswith("Error : The CPLEX")) and 
                               not(i.startswith("Error : Division by zero error with 0")) and
                               not("icense" in i))]
        if len(z2) > 0:
            result = True
            print_it(n, f"suspicious AIMMS error file: {f}", MODULE_NAME)  # also dump z or z2?

    return result


def run_module(my_module, pyfiler, my_var_put, AIMMS_path, base_year):
    """Run an AIMMS module.

    Called from models/main/nexec.py

    Parameters
    ----------
    my_module : string
        AIMMS module ("coal" or "ngas" or "hmm")
    pyfiler : _module
        pyfiler fortran module
    my_var_put : string
        file path for writing AIMMS input file
    AIMMS_path : string
        path to AIMMS exe
    base_year : int
        Depends on my_module, but passed as arg
    
    Returns
    -------
    string ("failure", "nan_found", "success", "stop", "suspicious_aimms_error_file")
    """
    CURIRUN = pyfiler.utils.curirun
    cwd = os.getcwd()

    s = f"in 'aimms_wrapper.run_module' for {my_module}"
    print_it(CURIRUN, s, MODULE_NAME)
    log_it(CURIRUN, s)

    year = int(pyfiler.ncntrl.curcalyr)
    iter = int(pyfiler.ncntrl.curitr)
    ncrl = int(pyfiler.ncntrl.ncrl)

    s = f"{my_module}: {year}:{iter}"
    print_it(CURIRUN, s, MODULE_NAME)
    log_it(CURIRUN, s)

    if not os.getcwd().endswith(my_module):
        os.chdir(my_module)

    s = f"write_aimms_input: {my_module}"
    print_it(CURIRUN, s, MODULE_NAME)
    log_it(CURIRUN, s)
    aimms_util.write_aimms_input(my_module, pyfiler, my_var_put, base_year)
    time.sleep(.25)
    
    aimms_attempt_max = 50
    aimms_attempt = 1
    success = False
    
    s = f"run_aimms: {my_module}"
    print_it(CURIRUN, s, MODULE_NAME)
    log_it(CURIRUN, s)
    my_result = "failure"  # default value
    stop_me = os.path.exists("stop.txt")
    nan_found = os.path.exists("nan_found.txt")
    suspicious_aimms_err = aimms_err(CURIRUN, os.path.join(my_module, "log", "aimmsRunOnly.err"))
    while ((aimms_attempt <= aimms_attempt_max) and (not success) and (not stop_me) and (not nan_found)) and (not suspicious_aimms_err):
        # On subsequent tries, re-run AIMMS input so that the monitor.in stays at "MainExeuction" instead of "Quit".
        if aimms_attempt > 1:
                aimms_util.write_aimms_input(my_module, pyfiler, my_var_put, base_year)
                time.sleep(3)  # TODO: still needed?
      
        s = f'attempt: {aimms_attempt}, max attempts: {aimms_attempt_max}'
        print_it(CURIRUN, s, MODULE_NAME)
        log_it(CURIRUN, s)
        AIMMS_thread = False
        success, AIMMS_thread = run_aimms(my_module, CURIRUN, year, iter, AIMMS_path, AIMMS_thread, my_command='Quit')
        stop_me = os.path.exists(os.path.join(cwd, "stop.txt"))
        nan_found = os.path.exists(os.path.join(cwd, "nan_found.txt"))
        suspicious_aimms_err = aimms_err(CURIRUN, os.path.join(my_module, "log", "aimmsRunOnly.err"))
        print_it(CURIRUN, f"Looking for {os.path.join(cwd, 'stop.txt')} : {stop_me=}", MODULE_NAME)
        print_it(CURIRUN, f"Also looking for {os.path.join(cwd, 'nan_found.txt')} : {nan_found=}", MODULE_NAME)
        print_it(CURIRUN, f"Also looking for suspicious log/aimmsRunOnly.err file: : {suspicious_aimms_err=}", MODULE_NAME)
        if stop_me:
            my_result = "stop"
            print_it(CURIRUN, f"{my_result=}", MODULE_NAME)
        elif nan_found:
            my_result = "nan_found"
            print_it(CURIRUN, f"{my_result=}", MODULE_NAME)
        elif suspicious_aimms_err:
            my_result = "suspicious_aimms_error_file"
            print_it(CURIRUN, f"{my_result=}", MODULE_NAME)
        elif success:
            my_result = "success"
            print_it(CURIRUN, f"{my_result=}", MODULE_NAME)
            time.sleep(0.5)
        else:
            s = f"attempt {aimms_attempt} was unsuccessful. Wait 5 seconds and try again."
            print_it(CURIRUN, s, MODULE_NAME)
            log_it(CURIRUN, s)
            time.sleep(5)
            aimms_attempt += 1

    s = f"Year: {year}; success: {success}, attempts: {aimms_attempt}"
    print_it(CURIRUN, s, MODULE_NAME)
    log_it(CURIRUN, s)

    if my_result == "success":
        s = f"process_aimms_results: {my_module}"
        print_it(CURIRUN, s, MODULE_NAME)
        log_it(CURIRUN, s)
        process_aimms_results(my_module, pyfiler, base_year)

    if os.getcwd().endswith(my_module):
        os.chdir('..')

    return my_result


def process_aimms_results(my_module, pyfiler, base_year):
    """Read and process AIMMS results.

    Parameters
    ----------
    my_module : string
        "coal", "hmm", or "ngas"
    pyfiler : module
        pyfiler fortran module
    base_year : int
        base year for my_module
    """
    CURIRUN = pyfiler.utils.curirun
    year = int(pyfiler.ncntrl.curiyr) + 1989
    iteration = int(pyfiler.ncntrl.curitr)

    # the following requires being the proper subdirectory (ngas, coal, etc.)
    s = f"process_aimms_results / read_aimms_output: {my_module}"
    print_it(CURIRUN, s, MODULE_NAME)
    log_it(CURIRUN, s)
    try:
        aimms_results = aimms_util.read_aimms_output(year, iteration)
    except:
        print_it(CURIRUN, f"Trying to read {my_module} AIMMS output for a second time...", MODULE_NAME)
        time.sleep(3)
        aimms_results = aimms_util.read_aimms_output(year, iteration)

    s = f"process_aimms_results / fill_pyfiler: {my_module}"
    print_it(CURIRUN, s, MODULE_NAME)
    log_it(CURIRUN, s)
    fill_pyfiler(pyfiler, aimms_results, base_year) 


# https://stackoverflow.com/questions/6904487/how-to-pass-arguments-to-a-thread
def thread_function_ngas(name, my_AIMMS_path=None, n=0):
    #my_AIMMS_path = r'C:\AIMMS_Installation_Free_Releases\4.96.4.6-x64-VS2017\Bin\aimms.exe'
    my_proc = r'NEMS_monitor'
    my_proj = os.path.join(os.getcwd(), 'natgas.aimms')
    s = f'my_proc, my_proj: {my_proc}, {my_proj}'
    print_it(n, s, MODULE_NAME)
    log_it(n, s)
    x = [my_AIMMS_path, '-R' + my_proc, my_proj, '--hidden']
    my_process = subprocess.Popen(x)


# https://stackoverflow.com/questions/6904487/how-to-pass-arguments-to-a-thread
def thread_function_coal(name, my_AIMMS_path=None, n=0):
    #my_AIMMS_path = r'C:\AIMMS_Installation_Free_Releases\4.96.4.6-x64-VS2017\Bin\aimms.exe'
    my_proc = r'NEMS_monitor'
    my_proj = os.path.join(os.getcwd(), 'Coal.aimms')
    s = f'my_proc, my_proj: {my_proc}, {my_proj}'
    print_it(n, s, MODULE_NAME)
    log_it(n, s)
    x = [my_AIMMS_path, '-R' + my_proc, my_proj, '--hidden']
    my_process = subprocess.Popen(x)


def thread_function_hmm(name, my_AIMMS_path=None, n=0):
    #my_AIMMS_path = r'C:\AIMMS_Installation_Free_Releases\4.96.4.6-x64-VS2017\Bin\aimms.exe'
    my_proc = r'NEMS_monitor'
    my_proj = os.path.join(os.getcwd(), 'hmm.aimms')
    s = f'my_proc, my_proj: {my_proc}, {my_proj}'
    print_it(n, s, MODULE_NAME)
    log_it(n, s)
    x = [my_AIMMS_path, '-R' + my_proc, my_proj, '--hidden']
    my_process = subprocess.Popen(x)


def run_aimms(my_module, cycle, year, iteration, AIMMS_path, x, my_command='Quit'):
    # AIMMS_path = r'C:\AIMMS_Installation_Free_Releases\4.96.4.6-x64-VS2017\Bin\aimms.exe'
    # x: a thread in which AIMMS is running, or False
    module_wait_time_limit = {"coal": 40, "hmm": 40, "ngas": 40}
    
    my_file = r'fromAIMMS\GlobalDataToNEMS_' + str(year) + '_' + str(iteration).zfill(2) + '.txt'
    
    s = "Looking for file " + my_file
    print_it(cycle, s, MODULE_NAME)
    log_it(cycle, s)

    # for offline testing, if the output file already exists, then exit the function
    success = os.path.isfile(my_file)
    if success:
        return success, None

    if my_module == 'coal':
        my_func = "threading.Thread(target=thread_function_coal, args=(1,), kwargs={'my_AIMMS_path':AIMMS_path, 'n':cycle})"  # no 'daemon'
    elif my_module == 'hmm':
        my_func = "threading.Thread(target=thread_function_hmm, args=(1,), kwargs={'my_AIMMS_path':AIMMS_path, 'n':cycle})"  # no 'daemon'
    elif my_module == 'ngas':
        my_func = "threading.Thread(target=thread_function_ngas, args=(1,), kwargs={'my_AIMMS_path':AIMMS_path, 'n':cycle})"  # no 'daemon'
    else:
        assert False, f"unknown AIMMS module: {my_module}"

    if not x:
        x = eval(my_func)
        x.start()
        if not x:
            time.sleep(5)
            x = eval(my_func)
            x.start()
            if not x:
                time.sleep(5)
                x = eval(my_func)
                x.start()

    s = f"AIMMS thread is alive: {x.is_alive()}"
    print_it(cycle, s, MODULE_NAME)
    log_it(cycle, s)

    if not x:
        s = 'no thread!?!'
        print_it(cycle, s, MODULE_NAME)
        log_it(cycle, s)

    # write "Quit" to monitor.in.txt after an appropriate delay
    # need to wait long enough for AIMMS to pickup the monitor.in.txt file
    # no more than 15 seconds, say.
    completed = False
    executing = False
    exited = False
    my_sleep = 0.25
    aimms_time_counter = 0
    aimms_time_limit = 15
    out_prev = " "
    waiting_prev = False
    while (not completed) and (not exited) and (aimms_time_counter <= aimms_time_limit):
        with open('monitor.out.txt', 'r') as f:
            z = f.readline()
        if z != out_prev:
            out_prev = z
            with open("monitor.out.debug.txt", "a") as f:
                f.write(f"{datetime.now()} :: {z}\n")
        if "omplete" in z:
            completed = True
        elif "executing" in z:
            executing = True
            s = 'sAction:="Wait";'
            with open('monitor.in.txt', 'w', encoding='utf-8') as f:
                f.write(f'{s}')
            if not waiting_prev:
                with open("monitor.in.debug.txt", "a") as f:
                    f.write(f"{datetime.now()} :: {s}\n")
            waiting_prev = True
            # sleep 5 seconds while incrementing aimms_time_counter only a bit
            time.sleep(5)
            aimms_time_counter += my_sleep
        elif 'Exited' in z:
            exited = True
        else:
            time.sleep(my_sleep)
            aimms_time_counter += my_sleep
    if completed:
        s = f"'omplete' found in monitor.out.txt: {completed}"
    elif executing:
        s = f"'executing' found in monitor.out.txt: {executing}"
    elif aimms_time_counter > aimms_time_limit:
        s = f"'aimms_time_counter' > {aimms_time_limit}: {aimms_time_counter}"
    else:
        s = f"unexpected situation: {completed=}, {aimms_time_counter=}"
    print_it(cycle, s, MODULE_NAME)
    log_it(cycle, s)
    s = f'sAction:="{my_command}";'
    with open('monitor.in.txt', 'w', encoding='utf-8') as f:
        f.write(s)
    with open("monitor.in.debug.txt", "a") as f:
        f.write(f"{datetime.now()} :: {s}\n")
    s = f'wrote command {my_command} to monitor.in.txt.'
    print_it(cycle, s, MODULE_NAME)
    log_it(cycle, s)

    # look for AIMMS output file 
    success = os.path.isfile(my_file)
    my_sleep = 0.25
        
    aimms_time_counter = 0
    while ((not success) and (aimms_time_counter <= module_wait_time_limit[my_module])):
        # print_it(f'aimms_time_counter: {aimms_time_counter}')
        time.sleep(my_sleep)
        aimms_time_counter += my_sleep
        success = os.path.isfile(my_file)

    s = f'aimms_time_counter: {aimms_time_counter}'
    print_it(cycle, s, MODULE_NAME)
    log_it(cycle, s)
    if success:
        s = f'Success! Found output file: {my_file}'
        print_it(cycle, s, MODULE_NAME)
        log_it(cycle, s)
 
    # x is the thread if aimms is open
    return success, x


def fill_pyfiler(pyfiler, dfd, base_year):
    """Write to PyFiler

    Parameters
    ----------
    pyfiler : module
        pyfiler fortran module
    dfd : dict
        dict of dataframes
    base_year : int
        base year relevant to dfd

    Returns
    -------
    bool
        True
    """
    CURIRUN = pyfiler.utils.curirun
    by = base_year - 1989
    y = int(pyfiler.ncntrl.curiyr)

    # pyfiler.ogsmout.ogrnagprd[:,:, by-1:] has base_year and following years

    # TODO: assume that if a variable is in the AIMMS output, then we should put it the restart file
    dfd2 = {k.split('(')[0]: v.rename(columns={'globalyr': 'MNUMYR', k: k.split('(')[0]}) for k, v in dfd.items()}
    temp = list(dfd.keys())
    my_var2 = {i.split('(')[0]: i.split('(')[1][:-1].replace('globalyr','MNUMYR').split(',') for i in temp}

    for k, v in my_var2.items():
        # replace FIRST '_' in k with '.'
        temp2 = k.find('_')
        if temp2 >= 0:
            k2 = k[0:k.find('_')] + '.' + k[k.find('_')+1:]
        else:
            k2 = k
        k2 = k2.lower()
        log_it(CURIRUN, k2)

        # determine proper string representation of the variable in pyfiler
        try:
            x = f"pyfiler.{k2}"
            z = np.copy(eval(x))
        except AttributeError:
            x = f"pyfiler.utils.{k2.split('.')[1]}"
            z = np.copy(eval(x))
        my_string = x

        if my_string == "pyfiler.coalemm.num_sc":
            pyfiler.coalemm.num_sc = 0

        elif my_string == "pyfiler.coalout.coalcode":
            pyfiler.coalout.coalcode = 0

        # TODO: is this correct ???
        elif my_string == "pyfiler.utils.h2step":
            pass

        elif "MNUMYR" not in v:
            eval(my_string)[:] = 0

        # recall that y=int(pyfiler.ncntrl.curiyr)
        elif y > by:
            z = v.index("MNUMYR")
            if z == 0:
                eval(x)[y-1] = 0
            elif z == 1:
                eval(x)[:, y-1] = 0
            elif z == 2:
                eval(x)[:, :, y-1] = 0
            elif z == 3:
                eval(x)[:, :, :, y-1] = 0
            elif z == 4:
                eval(x)[:, :, :, :, y-1] = 0
            elif z == 5:
                eval(x)[:, :, :, :, :, y-1] = 0
            else:
                assert False, "can't handle more then 5 dimensions"
            
        # make sure var has '_' rather than '.'
        var = k.replace('.', '_').upper()
        # ---------
        
        df = dfd2[var]
        # cols = [i for i in df.columns if "_" not in i]  # TODO what if i.startswith("MX_") ????

        # make columns into 'int' as appropriate
        for i in df.columns:
            if '_' not in i:
                df[i] = df[i].astype(int)

        z = np.copy(eval(my_string))
        s = z.shape

        # 'coalemm.num_sc' is a zero-dimensional array in pyfiler
        # so requires special code
        if k2 == 'coalemm.num_sc':
            pyfiler.coalemm.num_sc = 0
            for i in df.index:
                pyfiler.coalemm.num_sc = int(float(df.loc[i, var]))

        # the coalout.coalcode dataframe is empty unless there was an AIMMS problem
        elif k2 == 'coalout.coalcode':
            if df.empty:
                pyfiler.coalout.coalcode = 0
            else:
                print(df)
                assert False, "coalout.coalcode != 0 means CMM AIMMS did not find optimal solution (?)"

        # integer "index" variables
        elif k2 in ['coalemm.cmm_ldv_indx', 
                    'coalemm.cmm_sdv_indx', 
                    'coalemm.new_ldv_indx', 
                    'coalemm.new_sdv_indx']:
             for i in df.index:
                 j0 = int(df.loc[i, v[0]]) - 1
                 # assert j0 == i
                 eval(my_string)[j0] = int(float(df.loc[i, var]))

        elif k2 == 'hmmblk.h2step':
            pass
        
        elif len(s) == 1:          
            if not df.empty:  # TODO: ?should we raise a warning if the df is empty???
                a = list(df.columns)[:-1]
                df[a] = df[a].astype(int)
                df2 = df.set_index(a)
                for (i1) in df2.index:
                    eval(my_string)[i1-1] = float(df2.loc[(i1)].iloc[0])

        elif len(s) == 2:
            if not df.empty:  # TODO: is this condition necessary ????
                a = list(df.columns)[:-1]
                df[a] = df[a].astype(int)
                df2 = df.set_index(a)
                for (i1, i2) in df2.index:
                    eval(my_string)[i1-1][i2-1] = float(df2.loc[(i1,i2)].iloc[0])  # TODO: transform to int if 'coalemm.cmm_cont_indx' ???

        elif len(s) == 3:
            if not df.empty:
                a = list(df.columns)[:-1]
                df[a] = df[a].astype(int)
                df2 = df.set_index(a)
                for (i1, i2, i3) in df2.index:
                    eval(my_string)[i1-1][i2-1][i3-1] = float(df2.loc[(i1,i2,i3)].iloc[0])

        elif len(s) == 4:
            if not df.empty:
                a = list(df.columns)[:-1]
                df[a] = df[a].astype(int)
                df2 = df.set_index(a)
                for (i1, i2, i3, i4) in df2.index:
                    eval(my_string)[i1-1][i2-1][i3-1][i4-1] = float(df2.loc[(i1,i2,i3,i4)].iloc[0])
                    
        elif len(s) == 5:
            if not df.empty:
                a = list(df.columns)[:-1]
                df[a] = df[a].astype(int)
                df2 = df.set_index(a)
                for (i1, i2, i3, i4. i5) in df2.index:
                    eval(my_string)[i1-1][i2-1][i3-1][i4-1][i5-1] = float(df2.loc[(i1,i2,i3,i4,i5)].iloc[0])

        else:
            # TODO: are there any 6-dimensional restart variables?
            print(f'Unable to process: {k2}, {v} (???)')
    
    return True  # TODO: return something more meaningful
