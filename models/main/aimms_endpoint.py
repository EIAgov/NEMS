"""
Replacement for aimms_wrapper.py
"""
from datetime import datetime
import numpy as np
import os
import requests
import signal
import socket
import subprocess
import sys
import time


import aimms_util
from logging_utilities import print_it

MODULE_NAME = "aimms_endpoint.py"

# note that this module has its own logfile, separate from nohup.out
LOGFILE = os.path.join(os.getcwd(), "aimms_endpoint.log")

# Greg M. said: 49512-65535
midpoint = 57500
POTENTIAL_PORTS_P1 = range(49512, midpoint)
POTENTIAL_PORTS_P2 = range(midpoint+1, 65535+1)

s = requests.session()

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


def find_available_port():
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        print(f"{os.getcwd()=}")
        if os.getcwd().split("\\")[-2].endswith("p1"):
            POTENTIAL_PORTS = POTENTIAL_PORTS_P1
        else:
            POTENTIAL_PORTS = POTENTIAL_PORTS_P2
        print(f"{POTENTIAL_PORTS=}")
        for port in POTENTIAL_PORTS:
            if not (s.connect_ex(('localhost', port)) == 0):
                return port
        return False

# Different method where the OS seeks to try and find an available port
def find_available_portOS():
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

    # Binding to port 0 instructs the OS to find an available port.
    # The OS picks from the dynamic ports (49512 - 65535)
    sock.bind(("localhost", 0))
    port = sock.getsockname()[1]
    return(port)

def launch_aimms(aimms_path, p):
    """
    example p: 'main/aimms_frame/aimms_frame.aimms'
    """
    #p = "ngas/natgas.aimms"
    temp = p.split('/')
    my_dir = temp[0] + "/" + temp[1]
    my_aimms = temp[-1]
    cwd = os.getcwd()
    
    try:
        os.chdir(my_dir)
    except:
        assert False, "launch_aimms folder confusion"

    my_port = find_available_portOS()
    if my_port:
        with open("my_port.txt", "w") as f:
            f.write(f"my_port := {my_port} ;")
    else:
        assert False, "no available port"

    print(f"{p=}, {my_port=}")
    path_to_aimms_exe = os.path.join(aimms_path, "Bin", "Aimms.exe")
    process = subprocess.Popen([path_to_aimms_exe.replace(".exe", ""), my_aimms, '--hidden'])
    
        # Write AIMMS PID to folder in case needed for operations later
    with open('aimms_pid.txt', 'w') as file_aimms_pid:
        file_aimms_pid.write(str(process.pid))
    
    # Sleep to give AIMMS time to open and compile before sending call to start timeout timer
    time.sleep(35)

    # Run timeout procedure in AIMMS
    try:
        url_base = f"http://localhost:{my_port}/api/v2/tasks"
        r = s.post(f"{url_base}/call_timeout")
    except:
        print('Could not run AIMMS timeout procedure. Perhaps a longer wait is needed?')
    
    os.chdir(cwd)
    return process


# the following does not work, but leaving the code as a caution to future coders
def close_aimms(x):
    #x.terminate()  # Send SIGTERM signal
    os.kill(x, signal.SIGTERM)  # or SIGKILL ???

    x.wait()  # Wait for the process to terminate
    print(f"Process terminated with exit code: {x.returncode}")


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
        path to AIMMS exe. NOT used in this function
    base_year : int
        Depends on my_module, but passed as arg
    
    Returns
    -------
    string ("failure", "nan_found", "success", "stop", "suspicious_aimms_error_file")
    """
    CURIRUN = pyfiler.utils.curirun
    cwd = os.getcwd()

    s = f"in 'aimms_endpoint.run_module' for {my_module}"
    print_it(CURIRUN, s, MODULE_NAME)
    log_it(CURIRUN, s)

    s = f"path to AIMMS is {AIMMS_path}"
    print_it(CURIRUN, s, MODULE_NAME)
    log_it(CURIRUN, s)

    year = int(pyfiler.ncntrl.curcalyr)
    iter = int(pyfiler.ncntrl.curitr)
    ncrl = int(pyfiler.ncntrl.ncrl)

    s = f"{my_module}: {year}:{iter}, {ncrl=}"
    print_it(CURIRUN, s, MODULE_NAME)
    log_it(CURIRUN, s)

    if not os.getcwd().endswith(my_module):
        os.chdir(my_module)

    s = f"write_aimms_input: {my_module}"
    print_it(CURIRUN, s, MODULE_NAME)
    log_it(CURIRUN, s)
    aimms_util.write_aimms_input(my_module, pyfiler, my_var_put, base_year)
    time.sleep(.25)
    
    aimms_attempt_max = 3
    aimms_attempt = 1
    assert aimms_attempt <= aimms_attempt_max

    success = False

    s = f"run_aimms: {my_module}"
    print_it(CURIRUN, s, MODULE_NAME)
    log_it(CURIRUN, s)
    my_result = "failure"  # default value
    stop_me = os.path.exists("stop.txt")
    nan_found = os.path.exists("nan_found.txt")
    suspicious_aimms_err = aimms_err(CURIRUN, os.path.join(my_module, "log", "aimmsRunOnly.err"))
    while ((aimms_attempt <= aimms_attempt_max) and (not success) and (not stop_me) and (not nan_found)) and (not suspicious_aimms_err):
        # On subsequent tries, re-run AIMMS input so that the monitor.in stays at "MainExecution" instead of "Quit".
        if aimms_attempt > 1:
                aimms_util.write_aimms_input(my_module, pyfiler, my_var_put, base_year)
                time.sleep(3)  # TODO: still needed?
      
        s = f'attempt: {aimms_attempt}, max attempts: {aimms_attempt_max}'
        print_it(CURIRUN, s, MODULE_NAME)
        log_it(CURIRUN, s)

        success = run_api(year, iter, ncrl, my_module)
        stop_me = os.path.exists(os.path.join(cwd, "stop.txt"))
        nan_found = os.path.exists(os.path.join(cwd, "nan_found.txt"))
        suspicious_aimms_err = aimms_err(CURIRUN, os.path.join(my_module, "log", "aimmsRunOnly.err"))
        
        print_it(CURIRUN, f"Looking for {os.path.join(cwd, 'stop.txt')} : {stop_me=}", MODULE_NAME)
        print_it(CURIRUN, f"Also looking for {os.path.join(cwd, 'nan_found.txt')} : {nan_found=}", MODULE_NAME)
        print_it(CURIRUN, f"Also looking for suspicious log/aimmsRunOnly.err file: : {suspicious_aimms_err=}", MODULE_NAME)
        print_it(CURIRUN, f"Also looking for {os.path.join(cwd, f'{my_module}/done.txt')} : {success=}", MODULE_NAME)
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
    output_file = f"fromAIMMS/GlobalDataToNEMS_{str(year)}_{str(iteration).zfill(2)}.txt"
    if not os.path.exists(output_file):
        print_it(CURIRUN, f"sleep 5 seconds for {output_file}", MODULE_NAME)
        time.sleep(5)
    try:
        aimms_results = aimms_util.read_aimms_output(year, iteration)
    except:
        print_it(CURIRUN, f"Trying to read {my_module} AIMMS output for a second time (sleep 20 seconds)...", MODULE_NAME)
        time.sleep(20)
        aimms_results = aimms_util.read_aimms_output(year, iteration)

    s = f"process_aimms_results / fill_pyfiler: {my_module}"
    print_it(CURIRUN, s, MODULE_NAME)
    log_it(CURIRUN, s)
    fill_pyfiler(pyfiler, aimms_results, base_year) 


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
                    eval(my_string)[i1-1][i2-1] = float(df2.loc[(i1,i2)].iloc[0])

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
                for (i1, i2, i3, i4, i5) in df2.index:
                    eval(my_string)[i1-1][i2-1][i3-1][i4-1][i5-1] = float(df2.loc[(i1,i2,i3,i4,i5)].iloc[0])

        else:
            # TODO: are there any 6-dimensional restart variables?
            print(f'Unable to process: {k2}, {v} (???)')
    
    return True  # TODO: return something more meaningful


def run_api(year, iter, ncrl, my_module):
    MY_DEBUG = True
    SIGNAL_FILE = "done.txt"

    # TODO: check for ng_runval, ngsteo files

    if os.path.exists(SIGNAL_FILE):
        os.remove(SIGNAL_FILE)

    with open('my_modscendate.txt', 'r') as file:
        lines = file.readlines()
    my_modscendate = lines[0].split(":=")[1].replace(";", "").replace('"', "").strip()
    print(f"{my_modscendate=}")

    # my_port.txt should comprise a single line like:
    # my_port := 49512 ;
    with open('../main/aimms_endpoint/my_port.txt', 'r') as file:
        lines = file.readlines()
    my_port = int(lines[0].split(":=")[1].replace(";", "").strip())

    print("Sending API request to port " + str(my_port))

    url_base = f"http://localhost:{my_port}/api/v2/tasks"

    x = {"rows":[{"modscendate": my_modscendate, "year": year, "iteration": iter, "report": ncrl}]}
    print(f"{x=}")
    if MY_DEBUG:
        print(x)
    r = s.post(f"{url_base}/call_{my_module}",json=x)

    # NOTE: required files in ngas folder: ng_runval.txt, ngsteo.txt
    z = [i for i in r]
    z2 = eval((z[0] + z[1]).decode("utf-8"))["id"]  # example: '50618c3d-c207-4acf-b467-c5f7783d956b'
    if MY_DEBUG:
        print(f"requests.post: {r}")
        print(f"{z=}")
        print(f"{z2=}")

    # The while loop addresses this earlier concern:
    #   Might need to repeatedly have it to try and ping with a request until it receives a 200 status?
    #   If you request right away, it will return 404 since AIMMS wouldn't have finished running.
    max_sleep = 350  # note: HMM can take over a minute in year 2030+
    sleep_increment = 0.5
    my_sleep = 0
    while (my_sleep <= max_sleep) and (not os.path.exists(SIGNAL_FILE)):
        my_sleep += sleep_increment
        time.sleep(sleep_increment)

    if os.path.exists(SIGNAL_FILE):
        return True
    else:
        return False


if __name__ == "__main__":
    ret = launch_aimms()
    print(ret)