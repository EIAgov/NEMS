"""
Alternate version for aimms_endpoint.py  to be called from Fortran command call
"""
from datetime import datetime
import os
import requests,requests.utils
import signal
import socket
import subprocess
import sys
import time
import logging
import pickle
#from requests.adapters import HTTPAdapter

from logging_utilities import print_it



MODULE_NAME = "aimms_endpoint_p2"     #p2 aimms endpoint project name

# note that this module has its own logfile, separate from nohup.out
#LOGFILE = os.path.join(os.getcwd(), "aimms_endpoint.log")

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
def send_msg_to_quit(filename,msg):
    signal_file_name = f"..\\{filename}"
    signal_file = open(signal_file_name, "w")
    # Write content to the file
    signal_file.write(msg+"\n")
    # Close the file to save changes
    signal_file.close()

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
        with open(f, encoding='utf-8-sig') as f_in:
            z = f_in.readlines()
        # we decided to let it not faulter if cplex license is not available and attempt addtional trials
        # for now Division by zero error with 0 is only critical error in EFD and ECP AIMMS to stop the run
        # this critical error message list can be expanded as we find more post solve conditions in EFD, ECP, and RESTORE to stop run
        z2 = [i for i in z if (not(i.startswith("Error : The CPLEX")) and 
                               not(i.startswith("Warning")) and 
                               not("icense" in i))]
        if len(z2) > 0:
            result = True
            print_it(n, f"suspicious AIMMS error file: {f}", MODULE_NAME)  # also dump z or z2?

    return result

# the following does not work, but leaving the code as a caution to future coders
def close_aimms(x):
    #x.terminate()  # Send SIGTERM signal
    os.kill(x, signal.SIGTERM)  # or SIGKILL ???

    x.wait()  # Wait for the process to terminate
    print(f"Process terminated with exit code: {x.returncode}")

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
# Different method where the OS seeks to try and find an available port

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


def run_api(year, iter, ncrl, my_module):
    MY_DEBUG = True
    SIGNAL_FILE = "done.txt"


    # TODO: check for ng_runval, ngsteo files

    if os.path.exists(SIGNAL_FILE):
        os.remove(SIGNAL_FILE)

    # with open('my_modscendate.txt', 'r') as file:
    #     lines = file.readlines()
    # my_modscendate = lines[0].split(":=")[1].replace(";", "").replace('"', "").strip()
    # print(f"{my_modscendate=}")

    # my_port.txt should comprise a single line like:
    # my_port := 49512 ;
    #with open('./aimms_frame_p2/my_port.txt', 'r') as file:
    with open('../main/aimms_endpoint/my_port.txt', 'r') as file:
        lines = file.readlines()
    my_port = int(lines[0].split(":=")[1].replace(";", "").strip())

    print("Sending API request to port " + str(my_port))

    url_base = f"http://localhost:{my_port}/api/v2/tasks"
    
    x = {"rows":[{ "year": year, "iteration": iter}]}
    print(f"{x=}")
    if MY_DEBUG:
        print(x)
    #s.mount(f"{url_base}/call_{my_module}", adapter)  #added for testing HTTP connection pooling
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
    max_sleep = 180  # note: HMM can take over a minute in year 2030+
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
    """Run an AIMMS module.
    """
    """
    This function is to launch three AIMMS runs in parallel
    :param keylist: :type- object
    :param user: :type- object
    """
    LOG_DIR = '..'        
    LOG_FILE = 'p2_endpoint_run_api.log'
    LOG_FQFN = os.path.join(LOG_DIR, LOG_FILE)

    # logging config
    for handler in logging.root.handlers[:]:
        logging.root.removeHandler(handler)
    logging.basicConfig(filename=LOG_FQFN,
                        level=logging.DEBUG,
                        format='[%(asctime)s] [%(name)s] [%(threadName)s] '
                               '[%(funcName)s] [%(levelname)s] :: %(message)s')
   


    # P2_Session_PICKLE_FILE = "..\\P2_session_pickle.pickle"
    # if os.path.exists(P2_Session_PICKLE_FILE):
    #     start_time = datetime.now()
    #     with open(P2_Session_PICKLE_FILE,'rb') as f2:
    #         s=pickle.load(f2)
    #     end_time = datetime.now()
    #     execution_time = end_time-start_time
    #     log_entry_prefix = f"{datetime.now()} :: {MODULE_NAME} :: "
    #     s2 = f"seconds to load a session pickle file : {execution_time}; seconds"
    #     logging.info(log_entry_prefix + s2)
    # else:
    #     s = requests.session()
    #     log_entry_prefix = f"{datetime.now()} :: {MODULE_NAME} :: "
    #     s2 = f"a session pickle file is not found"
    #     #logging.info(log_entry_prefix + s2)   #added for testing HTTP connection pooling
    #     #adapter = HTTPAdapter(pool_connections=20, pool_maxsize=1, max_retries=3) #added for testing HTTP connection pooling



    # read endpoint_p2_caller.txt for parameters such CURIRUN, year, iter, ncrl, my_module
    # ------------------------------------
    #f = open(".\\main\\endpoint_p2_caller.txt", "r")  #<<<<<<<<<<< enable following two lines only for running locally (when not calling this module through call_p2_endpoint.bat)
    f = open("endpoint_p2_caller.txt", "r")    #<<<<<<<<<<< use this line for only batch command call
    readline = f.readline().strip()
    CURIRUN = int(readline[readline.find(":")+1:].strip())   #read CURIRUN
    readline = f.readline().strip()
    year = int(readline[readline.find(":")+1:].strip())   #read year
    readline = f.readline().strip()
    iter = int(readline[readline.find(":")+1:].strip())   #read iter
    readline = f.readline().strip()
    ncrl = int(readline[readline.find(":")+1:].strip())   #read ncrl
    readline = f.readline().strip()
    my_module = readline[readline.find(":")+1:].strip()   #read my_module 'ecp' 'efd', or 'rest'
    f.close()

 

  
    #<<<<<<<<<<<<<<<<< enable following two lines only for running locally (when not calling this module through call_p2_endpoint.bat)>>>>>>>>>>>>
    #aimms_process_frame2 = launch_aimms("C:\\AIMMS_Installation_Free_Releases\\25.3.1.0-x64-VS2022", "main/aimms_frame_p2/aimms_frame_p2.aimms")   
    #print_it(CURIRUN, f"{aimms_process_frame2=}", MODULE_NAME)

   
    log_entry_prefix = f"{MODULE_NAME} :: {my_module} :: cycle {CURIRUN} :: run year {year} :: iter {iter} :: "
    logging.info( log_entry_prefix + f"Starting a python script to submit a run_api request to {MODULE_NAME} to run {my_module} AIMMS." )

 
    cwd = os.getcwd()

    if not os.getcwd().endswith(my_module):
        os.chdir("..")          #<<<<<<<<<< enable this line only for calling this module through call_p2_endpoint.bat)
        os.chdir(my_module)   

    cwd = os.getcwd()

    s2 = f"** Switching to model folder .\\{my_module} to run the model."
    print(
        f"{log_entry_prefix} :: {s2}",
        end="\n\n"
    )
    logging.info(log_entry_prefix + s2)    

    aimms_attempt_max = 3
    aimms_attempt = 1
    assert aimms_attempt <= aimms_attempt_max

    success = False
    s2 = f"** run_module: {my_module}"
    print(
        f"{log_entry_prefix} :: {s2}",
        end="\n\n"
    )
    logging.info(log_entry_prefix + s2)

    my_result = "failure"  # default value
    stop_me = os.path.exists("stop.txt")
    nan_found = os.path.exists("nan_found.txt")
    #suspicious_aimms_err =  False  #<<<<<<<<<<<<<<<<< enable the line only for running locally (when not calling this module through call_p2_endpoint.bat)>>>>>>>>>>>>
    suspicious_aimms_err = aimms_err(CURIRUN, os.path.join(".", "log", "aimmsRunOnly.err"))   #<<<<<<<<<<<<<<<<< comment this line for running locally >>>>>>>>>>>>
    while ((aimms_attempt <= aimms_attempt_max) and (not success) and (not stop_me) and (not nan_found)) and (not suspicious_aimms_err):
        # On subsequent tries, re-run AIMMS input so that the monitor.in stays at "MainExecution" instead of "Quit".
            
        s2 = f'** attempt: {aimms_attempt}, max attempts: {aimms_attempt_max}'
        print(
        f"{log_entry_prefix} :: {s2}",
        end="\n\n"
        )
        logging.info(log_entry_prefix + s2)

        success = run_api(year, iter, ncrl, my_module)
        stop_me = os.path.exists(os.path.join(cwd, "stop.txt"))
        nan_found = os.path.exists(os.path.join(cwd, "nan_found.txt"))
        suspicious_aimms_err = aimms_err(CURIRUN, os.path.join(".", "log", "aimmsRunOnly.err"))
        
        print(log_entry_prefix, f"Looking for {os.path.join(cwd, 'stop.txt')} : {stop_me=}", MODULE_NAME)
        print(log_entry_prefix, f"Also looking for {os.path.join(cwd, 'nan_found.txt')} : {nan_found=}", MODULE_NAME)
        print(log_entry_prefix, f"Also looking for suspicious log/aimms.err file: : {suspicious_aimms_err=}", MODULE_NAME)
        print(log_entry_prefix, f"Also looking for {os.path.join(cwd, f'{my_module}/done.txt')} : {success=}", MODULE_NAME)
        if stop_me:
            my_result = "stop"
            print(log_entry_prefix, f"{my_result=}", MODULE_NAME)
            send_msg_to_quit("kill_run.txt",my_module + " in aimms_endpoint_p2 has stopped.")
        elif nan_found:
            my_result = "nan_found"
            print(log_entry_prefix, f"{my_result=}", MODULE_NAME)
            send_msg_to_quit("kill_run.txt",my_module + " in aimms_endpoint_p2 has nan_found.")
        elif suspicious_aimms_err:
            my_result = "suspicious_aimms_error_file"
            print(log_entry_prefix, f"{my_result=}", MODULE_NAME)
            send_msg_to_quit("kill_run.txt",my_module + " in aimms_endpoint_p2 ran into a critical AIMMS error.")
        elif success:
            my_result = "success"
            print(log_entry_prefix, f"{my_result=}", MODULE_NAME)
            time.sleep(0.5)
        else:
            s2 = f"** attempt {aimms_attempt} was unsuccessful. Wait 5 seconds and try again."
            print(log_entry_prefix, s2, MODULE_NAME)
            logging.info(log_entry_prefix + s2)
            time.sleep(5)
            aimms_attempt += 1

    s2 = f"** run success: {success}, attempts: {aimms_attempt}"
    print(log_entry_prefix, s2, my_module)
    logging.info(log_entry_prefix + s2)

    if my_result == "success":
        s2 = f"** The run was successful."

    else:
        s2 = f"** Run attempts have failed...Please check the error log found in {my_module}\\log\\. "

    print(log_entry_prefix, s2, MODULE_NAME)
    logging.info(log_entry_prefix + s2)
    # if not os.path.exists(P2_Session_PICKLE_FILE ):
    #     start_time = datetime.now()
    #     with open(P2_Session_PICKLE_FILE,'wb') as f2:
    #         pickle.dump(s,f2)
    #     end_time = datetime.now()
    #     execution_time = end_time-start_time
    #     log_entry_prefix = f"{datetime.now()} :: {MODULE_NAME} :: "
    #     s2 = f"seconds to save a session pickle file : {execution_time}; seconds"
    #     logging.info(log_entry_prefix + s2)
    
    #aimms_process_frame2.kill()   #<<<<<<<<<<<<<<<<< enable the line only for running locally (when not calling this module through call_p2_endpoint.bat)>>>>>>>>>>>>
    if os.getcwd().endswith(my_module):
        os.chdir('..')
    
    