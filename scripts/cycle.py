"""Cycle.py is the main entry-point for a NEMS run after the 
output folder has been set up.  It leads into nems_flow.py, which is
the main loop inside of NEMS.

The program is called with two arguments: dest_path and run_type.
dest_path is the output folder and run_string tells type of run 
(local/queue and sequential/parallel)

run_type: Queue versus Local
----------------------------
EIA NEMS runs are typically Queue runs that are managed by a RabbitMQ
queue server. The integration team sometime does local runs, which do
not invole RabbitMQ.

run_type: Sequential versus Parallel
-----------------------------------
Sequential runs all happen in a single process. Also known as jognems.
Parallel runs are split into two parallel processes (p1 and p2), which
are then followed by another process (p3). Also known as parnems.

Logging
-------
We tried using 'logging', but that did not work as desired. 
So, we use the simple 'log_it' function to write to nohup.out.
Note that the filename nohup.out is set in the queue celery 
code, so we cannot change the name in cycle.py

TODO items, in priority order (roughly)
* low priority: iterdump
"""
import cleanup
import cycle_helper as ch
from datetime import datetime
import glob
import multiprocessing
import os
import pandas as pd
import run_task
import shutil
import subprocess
import sys
import threading
import time
import main.partition_integrator as npz_join
import zipfile

j = os.path.join(os.getcwd(), "converge")
if j not in sys.path:
    sys.path.append(j)
import intercv_main

from multiprocessing import Queue

import main.parse_scedes as psf

temp = os.path.join(os.path.dirname(os.path.abspath(__file__)), "Validator")
if temp not in sys.path:
    sys.path.append(temp)
from Validator import validate

temp = os.path.join(os.path.dirname(os.path.abspath(__file__)), "reporter")
if temp not in sys.path:
    sys.path.append(temp)
from reporter import RW_reporter_main

# the following fails sometimes
#MODULE_NAME = os.path.basename(vars(sys.modules[__name__])['__file__'])
MODULE_NAME = "cycle.py"

LOCAL_DEBUG = False  # set to True when actively debugging LOCAL runs

LOGFILE = "nohup.out"  # filenameset by celery, required by validator.
LOGFILE_BEFORE_VALIDATOR = "nohup_before_validator.out"
LOGFILE_AFTER_VALIDATOR = LOGFILE
LOGFILE_BEFORE_STOP = "nohup_before_stop.out"

# CALL_P will be used later to hold the Python evironment + "scripts/python.exe". 
# It will be filled when we read in the scedes.all file.
PYENV = "nil"
CALL_P = "nil"

STOP_SIGNAL = "stop.txt"
TIME_FORMAT = "%Y-%m-%d %H:%M:%S"
MERGE_P1_P2 = True

NEMS_BANNER = ["N       N   E E E E   M       M   S S S S",
               "N N     N   E         M M   M M   S      ",
               "N   N   N   E E E E   M   M   M   S S S S",
               "N     N N   E         M       M         S",
               "N       N   E E E E   M       M   S S S S",
               ] 

REPORTER_SUCCESS_FILE = "reporter/ITG-reporter_success.txt"

TIME_LIMIT = 120  # max seconds per module


def log_it(n, s, fout=False):
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
    """
    if not fout:
        fout = LOGFILE

    with open(fout, "a", encoding="utf-8") as f:
        f.write(f"{datetime.now()} :: cycle {n} :: {MODULE_NAME} :: {s}\n")
        f.flush()


def check_stop_signal():
    """Check for the existence of a file with filename matching 
    STOP_SIGNAL ("stop.txt"). Need to check the root folder and 
    possible p1/p2/p3 sub-folders.

    STOP_SIGNAL is a module-level hardcode parameter.

    Returns
    -------
    bool
        True if STOP_SIGNAL file existe.
    """
    end_run = False
    cwd = os.getcwd()
    for i in ["", "p1", "p2", "p3"]:
        j = os.path.join(cwd, i, STOP_SIGNAL)
        if os.path.isfile(j):
            end_run = True
    return end_run


def stop_it(n, message="unknown failure"):
    """Append a formatted crash message to the LOGFILE (nohup.out)
    and exit Python.
    
    LOGGFILE is a module-level hardcoded parameter.
    Per NEMS tradition, the crash message includes "ha ha".

    Parameters
    ----------
    n : integer
        cycle number
    message : str, optional
        crash message , by default "unknown failure"
    """
    ha = ["H    H   AA    H    H   AA  ",
          "H    H  A  A   H    H  A  A ",
          "HHHHHH AAAAAA  HHHHHH AAAAAA",
          "H    H A    A  H    H A    A",
          "H    H A    A  H    H A    A",
          ] 
    
    log_it(n, message)
    with open(LOGFILE, "a") as f_out:
        f_out.write(3 * "\n")
        f_out.write("\n".join(ha))
        f_out.write(3 * "\n")

    log_it(n, "Stopped! Call 'cleanup.cleanup'")
    log_it(n, f"{os.getcwd()=}")
    try:
        cleanup.cleanup(n, os.getcwd())
    except:
        log_it(n, "unable to cleanup")

    with open('nems_run_status_log.txt', 'a') as ns:
        status = f'module:STOP, cycle: {n}, year: -1, iteration: -1\n'
        ns.write(status)    
    
    os.sys.exit(message)


def reporter_wrapper(n, SCEDES, nruns):
    log_it(n, f"In function 'reporter_wrapper'. Call 'generate_report' since int(SCEDES['NEMRWR']) >= 1: {int(SCEDES['NEMRWR'])}")
    log_it(n, f"In function 'reporter_wrapper'. Using configuration file str(SCEDES['RWCNFG']) : {str(SCEDES['RWCNFG'])}")
    nruns = nruns
    try:
        log_it(n, "call function 'generate_report'")
        generate_report(n, SCEDES, nruns)
    except:
        if os.getcwd().endswith("reporter"):
            os.chdir("..")
        log_it(n, "Unable to run function 'generate_report'")
    
    if os.getcwd().endswith("reporter"):
        os.chdir("..")
    if os.path.exists(REPORTER_SUCCESS_FILE):
        log_it(n, f"Found file {REPORTER_SUCCESS_FILE}")
    else:
        log_it(n, f"did NOT find file {REPORTER_SUCCESS_FILE}")


def generate_report(my_cycle, SCEDES, nruns):
    """Call NEMS Python reporter.

    Parameters
    ----------
    my_cycle : integer
        cycle number
    SCEDES : dict
        "SCEN"
        "DATE"
        "NEMRWR"
    """
    n = my_cycle
    log_it(n, "in 'generate_report'")   
    log_it(n, f"{os.getcwd()=}")

    scenario = SCEDES["SCEN"]
    datecode = SCEDES["DATE"]
    nruns = nruns
    
    if os.path.exists(REPORTER_SUCCESS_FILE):
        try:
            os.remove(REPORTER_SUCCESS_FILE)
        except:
            log_it(n, f"unable to remove {REPORTER_SUCCESS_FILE}") 

    log_it(n, "about to call function 'cycle_helper.generate_report'")
    try:
        ch.generate_report(SCEDES, n)
        log_it(n, f"Back from 'cycle.helper.generate_report'. Create {REPORTER_SUCCESS_FILE}")
        if os.getcwd().endswith("reporter"):
            os.chdir("..")
        with open(REPORTER_SUCCESS_FILE, "w") as temp:
            pass
    except:
        # TODO: does this code ever get activated if the "try" fails?
        if os.getcwd().endswith("reporter"):
            os.chdir("..")
        log_it(n, "something went wrong with 'cycle_helper.generate_report'")

    if os.getcwd().endswith("reporter"):
        os.chdir("..")

    if os.path.exists(REPORTER_SUCCESS_FILE):
        # move files from reporter/output folder to datecode folder
        #ex: reporter/output/ref2025_rep.d091024c.RAN"
        if n < nruns:
            my_infix = f".{n}."
        else:
            my_infix = "."

        s_from = f"reporter/output/{scenario}.{datecode}"
        s_to = f"{scenario}{my_infix}{datecode}"
        log_it(n, f"try to move {s_from} to {s_to} for XLSX, RAN")
        for i in ["xlsx", "ran"]:
            if os.path.exists(f"{s_from}.{i}"): 
                shutil.move(f"{s_from}.{i}", f"{s_to}.{i}")

        api_file = f"NEMS{SCEDES['SCEN']}.unif.api"
        s_from = f"reporter/output/{api_file}.csv"
        s_to = f"{api_file}{my_infix}csv"
        log_it(n, f"try to move {s_from} to {s_to}")
        if os.path.exists(s_from): 
            shutil.move(f"{s_from}", f"{s_to}")

        #TODO: remove after reporter quits writing to "all_tables_stacked.xlsx"
        s_from = "reporter/output/all_tables_stacked.xlsx"
        s_to = f"{scenario}{my_infix}{datecode}.xlsx"
        log_it(n, f"try to move {s_from} to {s_to}")
        if os.path.exists(f"{s_from}"): 
            shutil.move(f"{s_from}", f"{s_to}")


   
    return os.path.exists(REPORTER_SUCCESS_FILE)


def thread_function_multi(queue, my_path, my_call_p):
   
    cur_dir = os.getcwd()
    os.chdir(my_path)
    my_py = os.path.join(my_path, 'nems_flow_wrapper.py')
    my_process = subprocess.run([my_call_p, my_py])

    os.chdir(cur_dir)

    return my_process.returncode


def check_intercycle_convergence(minscore, nruns, n, doemall, doaminof, whencall=0):
    
    if nruns == 1:
        if os.path.exists("p2"):
            filenames = os.listdir("p2/input")
            matching_file = [filename for filename in filenames if filename.startswith("restarti")][0]
            restart_extension = matching_file.split(".")[1]
            conv_restart_prev = f"p2/input/restarti.{restart_extension}"
        else:
            filenames = os.listdir("input")
            matching_file = [filename for filename in filenames if filename.startswith("restarti")][0]
            restart_extension = matching_file.split(".")[1]
            conv_restart_prev = f"input/restarti.{restart_extension}"
    else:
        if os.path.exists("p2"):
            conv_restart_prev = "p2/restart.in"
        else:
            conv_restart_prev = "restart.in"
    
    conv_restart_cur = "restart.npz"


    iret = intercv_main.main(conv_restart_prev, conv_restart_cur, minscore)

    log_it(n, f"Intercycle return code = {iret}")
    
    df = pd.read_csv("converge/output/Iconv_AverageGPA.csv")
    usgpa       = df["US GPA"].iloc[-1]
    rgpa        = df["Regional GPA"].iloc[-1]
    last_line   = df["PASS/FAIL"].iloc[-1]

    s = f"GPA ({usgpa}; {rgpa}) on a 4 point scale (averaged over 3 worst years with {minscore} considered minimally acceptable."
    log_it(n, s)
    log_it(n, last_line)
    log_it(n, f"{usgpa=} {rgpa=}", "GPA.txt")
    
    ## TODO: what does "whencall==0" mean? Just that p1, p2, and p3 were successful?
    if whencall == 0:

        if iret == 1:
            if ((doemall == 0) & (n >= doaminof)):
                log_it(n, f"The convergence criteria of {minscore} was met.")
                nruns = n
            else:
                if n < nruns:
                    log_it(n, f"The convergence criteria of {minscore} was met in cycle {n} but I must cycle on due to user request.")
                    log_it(n, f"Note: the convergence criteria specified in the SCEDES -might- be higher than that specified in the Validator.")

    log_it(n, f"End intercycle convergence check for cycle {n}.")
      
    return(nruns)


def update_current_data_files(n):
    """
    copy files from one iteration to another
    combine some files
    remove unneeded files
    """
    s = "restart.in"
    for i in [f'{s}',
              f'p1/{s}',
              f'p2/{s}',
              f'p3/{s}',
              ]:
        try:
            j = f".{n}".join(os.path.splitext(i))
            shutil.copy2(i, j)
            log_it(n, f"copy {i} to {j}")
        except:
            pass

    # copy ending restart file to starting restart file
    # the code is structured so that restart.RLX takes "precedence"
    if os.path.exists("p2"): # parallel run
        
        log_it(n, f"copy restart.unf to p3/restart.unf to preserve intercv output in the p3 output restart file")

        shutil.copy2("restart.npz", "p3/restart.npz")

        for i in ["p1", "p2"]:
            file_to = f"{i}/RESTART.IN"
            for my_file in ["restart.npz", "restart.rlx"]:
                try:
                    shutil.copy(my_file, file_to)
                    log_it(n, f"Copied: {my_file} to: {file_to}")
                except:
                    log_it(n, f"Did not copy: {my_file} to {file_to}")
    else:
        # sequential run
        file_to = "RESTART.IN"
        for my_file in ["restart.npz", "restart.rlx"]:
            try:
                shutil.copy(my_file, file_to)
                log_it(n, f"Copied: {my_file} to: {file_to}")
            except:
                log_it(n, f"Did not copy: {my_file} to {file_to}")

    # TODO: most of these files do not exist...
    for i in ['EPM_epmout.txt',
              'restart.unf',
              'restart_out.unf',
              'restart.npz',
              'MAINDBUG',
              'MNPQIT.daf',
              'MC_COMMON.csv',  # TODO: what about MC_*.CSV ???
              'INTLDBG.txt',
              'MNEXPECT.txt',
              'p1/EPM_epmout.txt',
              'p1/restart.unf',
              'p1/restart.npz',
              'p1/restart_out.unf',
              'p1/MAINDBUG',
              'p1/MNPQIT.daf',
              'p1/MC_COMMON.csv',  # TODO: what about MC_*.CSV ???
              'p1/INTLDBG.txt',
              'p1/MNEXPECT.txt',
              'p2/EPM_epmout.txt',
              'p2/restart.unf',
              'p2/restart.npz',
              'p2/restart_out.unf',
              'p2/MAINDBUG',
              'p2/MNPQIT.daf',
              'p2/MC_COMMON.csv',  # TODO: what about MC_*.CSV ???
              'p2/mnexpect.txt',
              'p2/edbpgrp.txt',
              'p3/EPM_epmout.txt',
              'p3/restart.unf',
              'p3/restart.npz',
              'p3/restart_out.unf',
              ]:
                try:
                    j = f".{n}".join(os.path.splitext(i))
                    shutil.copy2(i, j)
                    log_it(n, f"copy {i} to {j}")
                except:
                    pass

    for i in ['p1/coal1CPS','p1/EDN0SLC','p2/coal1CPS','p2/EDN0SLC']:
        if os.path.isfile(i): 
            os.remove(i)

    if os.path.isfile('p2/INTLDBG.txt'): 
        shutil.copy('p2/INTLDBG.txt','p2/ACT_INTLDBG.txt')

    return 0




def check_for_intercycle_oscillation(n):
    """Check for intercycle oscillations.

    Summary results are written to nohup.out (seach for "osc").
    Detailed results are written to oscillate.csv.

    Args for oscillate.exe:
       3: makes RMSDpct the measure used (3:default)
      .99 with option 3, .99 is max ratio of Skipped to Adjacent 
          measure, meaning comparison between skipped cycles are 
          more similar (lower measure) than adjacent
      .5  with option 3, the RMSDpct for Adj cycles must be at 
         least this big (or else they are too close together to 
         be considered significant).
       3  min number of oscillations in 3 calculations for 5 last
          cycles (expects to see RAN files like: short.3.0906c.ran, 
          where the cycle number is between scenario and datecode

    Parameters
    ----------
    n : integer
        cycle number
    """
    log_it(n, "OSC Check for intercycle oscillations")
    with open(LOGFILE, "a") as log:
        return_code = subprocess.run(["oscillate.exe", "3 .99 .5 3"], stdout=log, stderr=log)

    log_it(n, f"oscillate.exe returned: {return_code}")

    # TODO: point to oscillate.csv file ???
    # TODO: return something


def do_end_of_run_tasks(SCEDES, minscore, nruns, n, doemall, doaminof):
    """check for intercycle oscillation,
    create EMM database (if EMM is 'on'), delete/zip some files,
    run validator, generate report of model runtime.

    Parameters
    ----------
    n : integer
        cycle number
    SCEDES : dict
        "TABREQN": tabreq filename
        "EXE": 1 if electricity model in ON, 0 if off
    """
    minscore = float(SCEDES["MINSCORE"])
    
    cwd = os.getcwd()
    if cwd.endswith("reporter"):
        os.chdir("..")
        cwd = os.getcwd()
    
    nruns = check_intercycle_convergence(minscore, nruns, n, doemall, doaminof, 0)
    
    df = pd.read_csv("converge/output/Iconv_AverageGPA.csv")
    usgpa       = df["US GPA"].iloc[-1]
    rgpa        = df["Regional GPA"].iloc[-1]
    last_line   = df["PASS/FAIL"].iloc[-1]
    
    #commenting out the old oscillation code, since it doesn't work.
    # log_it(n, "call 'check_for_intercycle_oscillation'")
    # try:
        # check_for_intercycle_oscillation(n)
    # except:
        # log_it(n, "unable to check for intercycle oscillation")

    log_it(n, f"{SCEDES['NEMRWR']=}")
    if int(SCEDES['NEMRWR']) >= 1:
        log_it(n, "try to run Python reporter one last time")
        reporter_wrapper(n, SCEDES, nruns)
        log_it(n, "finished running Python reporter")
        
        
    os.chdir(cwd)

    # TODO: bad things happen if...
    # IF we don't call validator as a subprocess:
    # in case the validator overwrites the existing nohup.out 
    # file, as happens in queue runs, let's make a copy
    # we'll process it later in the code
    #shutil.copy2(LOGFILE, LOGFILE_BEFORE_VALIDATOR)

    # run validator
    log_it(n, "call 'validate.main'")
    val_bat = f"run_validator.bat {PYENV} > validator.log 2>validatorDebug.log"
    val_path = os.path.join(cwd, "Validator")
    temp = os.path.join(val_path, "run_validator.bat")
    log_it(n, f"Validator path {temp} exists: {os.path.isfile(temp)}")
    os.chdir(val_path)
    p = subprocess.Popen(val_bat)
    stdout, stderr = p.communicate()
    os.chdir(cwd)
    log_it(n, "Done calling validator")
    log_it(n, "Waiting for validator results")
    # wait for validator to finish before cleanup.
    # 3 minutes total longest wait, in 20-second chunks
    time.sleep(20)
    for valwait in range(1, 17):
        if not (os.path.exists("validator_FAIL.xlsx") or os.path.exists("validator_PASS.xlsx")):
            time.sleep(20)

    if not (os.path.exists("validator_FAIL.xlsx") or os.path.exists("validator_PASS.xlsx")):
        log_it(n, f"Validator did not finish yet. Maybe increase wait time in cycle.py?")

    for f in ["validator_FAIL.xlsx", "validator_PASS.xlsx"]:
        if os.path.exists(f):
            log_it(n, f"see Validator results in file {f}")
    
    # delete/zip various files except when debugging a local run
    if not LOCAL_DEBUG:
        log_it(n, "call 'cleanup.cleanup'")
        try:
            cleanup.cleanup(n, cwd)
        except:
            log_it(n, "unable to cleanup")
    os.chdir(cwd)

    # generate model runtime & GPA report
    # might eventually include validator run time, so generate -after' validator

    # get regional GPA info
    # expect a file GPA.txt wiht lines line: 
    # 2024-12-10 16:02:50.774560 :: cycle 3 :: cycle.py :: usgpa='4.000' rgpa='4.000'
    gpa_txt = "GPA.txt"
    try:
        with open(gpa_txt, "r") as f_in:
            lines = f_in.readlines()
        
        rgpa = {i+1: float(j.split("=")[-1].strip().replace("'",""))
                 for i, j in enumerate(lines)}
        
    except:
        pass

    for i in ["", "p1", "p2", "p3"]:
        my_path = os.path.join(os.getcwd(), i)
        if os.path.exists(os.path.join(my_path, "nohup.out")):
            log_it(n, f"try to generate model runtime report based on nohup.out in {my_path}")
            try:
                ch.generate_model_runtime_report(n, my_path)
            except:
                pass

    if os.path.exists("p1"):
        try:
            df = pd.DataFrame(columns=["p1", "p2", "p3"])
            for i in ["p1", "p2", "p3"]:
                with open(os.path.join(i, "nohup.out"), "r") as f:
                    for line in f:
                        if "TOTAL MODEL TIME:" in line:
                            temp = line.split("::")
                            cycle = temp[1].strip()[-1]
                            t = float(temp[3].split(":")[-1].strip())
                            df.loc[cycle, i] = t
            df = df.sort_index()
            df["nems"] = df[["p1","p2"]].max(axis=1) + df["p3"]
            df = round((df.astype(float)/3600), 2)
            df.loc["tot"] = df.sum()
            df["| rgpa"] = ""
            for j in df.index:
                try:
                    df.loc[j, "| rgpa"] = rgpa[int(j)]
                except:
                    pass
            log_it(n, "p1/p2/p3 runtime summary")
            log_it(n, "\n" + df.to_string())
        except:
            log_it(n, "unable to generate p1/p2/p3 runtime summary")

    # TODO: return something


def run(dest_path, run_type):
    """
    dest_path: run folder. Ex: r"r:\\output\\mc6\\zef25\\d052324ax"

    run_type is one of:
        * 'local_sequential'
        * 'local_parallel'
        * 'queue_sequential'
        * 'queue_parallel'
    """
    start_time = datetime.now()

    # print the NEMS banner, plus blank lines in case
    # some module overwrites beginning of nohup.out
    with open(LOGFILE, "a") as f_out:
        f_out.write(16 * ".\n")
        f_out.write("\n".join(NEMS_BANNER))
        f_out.write(6 * "\n")

    log_it(0, f"in 'run': {dest_path=} , {run_type=}")

    with open("launched.from", "r") as f_in:
        z = f_in.readlines()
    log_it(0, "contents of launched.from")
    with open(LOGFILE, "a") as f_out:
        f_out.write("".join(z))
    
    try:
        log_it(0, f"{os.environ['COMPUTERNAME']=}")
    except:
        pass
    try:
        log_it(0, f"{os.environ['VIRTUAL_ENV']=}")
    except:
        pass

    log_it(0, "in function 'run'")
    log_it(0, f"{dest_path=}")
    log_it(0, f"{run_type=}")

    if "-" in dest_path:
        s = f"Hyphens are not allowed in NEMS output filepaths: {dest_path}\nendinding run"
        stop_it(0, s)

    if ("local" in run_type) and ("sequential" in run_type):
        pass
    elif ("local" in run_type) and ("parallel" in run_type):
        pass
    elif ("queue" in run_type) and ("sequential" in run_type):
        pass
    elif ("queue" in run_type) and ("parallel" in run_type):
        pass
    else:
        stop_it(0, f"Unknown run type: {run_type}\nending run")

    if check_stop_signal():
        shutil.copy2(LOGFILE, LOGFILE_BEFORE_STOP)
        stop_it(0, f"detected stop signal: {STOP_SIGNAL}")

    SCEDES = psf.parse_scedes_all(os.path.join(dest_path, "scedes.all"))

    # no! must run nems_flow.py first in order to create
    # restart.npz, NEMSvardf.csv, etc

    PYENV = SCEDES["NEMSPYENN"]
    CALL_P = os.path.join(PYENV, "scripts", "python.exe")
    nruns = int(SCEDES["NRUNS"])
    doaminof = int(SCEDES["DOAMINOF"])
    doemall = int(SCEDES["DOEMALL"])
    minscore = float(SCEDES["MINSCORE"])
    StopCode = 0

    log_it(0, f"{PYENV=}")
    
    # unzips the PSBASEUNF.zip file for RFM before it gets copied over to the output directory (so it can be removed in cleanup.py)
    # checks if p2 exists for parnems (in /p2/input), else unzip for jognems (in /input)
    # this 
    if SCEDES['EXN'] == "1":
        if os.path.isdir('p2'):
            #unzip PSBASEUNF.zip file
            with zipfile.ZipFile(SCEDES['PSBASEUNFN'].replace("$NEMS/input/rfm/",os.path.join(os.getcwd(),"p2/input/")), 'r') as z:
                z.extractall("p2/input")
        else:
            #unzip PSBASEUNF.zip file
            with zipfile.ZipFile(SCEDES['PSBASEUNFN'].replace("$NEMS/input/rfm/",os.path.join(os.getcwd(),"input/")), 'r') as z:
                z.extractall("input")

    # TODO: why do this as step zero ???
    # nruns = check_intercycle_convergence(minscore, nruns, n, doemall, doaminof, 0)

    if "sequential" in run_type:
        n = 1
        while n <= nruns:
            cwd = os.getcwd()
            log_it(n, f"os.getcwd(): {cwd}, n: {n}, nruns: {nruns}")

            # be extra careful to delete any "done.txt" file
            if os.path.isfile(f"done.txt"):
                os.remove("done.txt")

            with open(os.path.join(cwd, "curirun.txt"), "w") as f_irun:
                f_irun.write(str(n))

            # delete any old AIMMS results
            for my_folder in ["ngas/fromAIMMS", "coal/fromAIMMS", "hmm/fromAIMMS", "rest/fromAIMMS"]:
                if os.path.isdir(my_folder):
                    log_it(n, f"Deleting any old AIMMS 'toNEMS' files from {my_folder}")
                    files = glob.glob(f"{my_folder}/GlobalDatatoNEMS*")
                    for f in files:
                        os.remove(f)
            # Remove any folders containing parquet files for AIMMS modules
            for my_folder in ["ngas/ToAIMMS", "coal/ToAIMMS", "hmm/ToAIMMS", "rest/toAIMMS"]:
                ToAIMMSPath = os.path.join(os.getcwd(), my_folder)
                if os.path.isdir(ToAIMMSPath):
                    log_it(n, f"Deleting any old AIMMS 'toAIMMS' folders from {my_folder}")
                    for item in os.listdir(ToAIMMSPath):
                        item_path = os.path.join(ToAIMMSPath, item)
                        if os.path.isdir(ToAIMMSPath) and item.startswith("GlobalDataToAIMMS_"):
                            try:
                                shutil.rmtree(item_path)
                            except:
                                pass
            # Remove files from EFD and ECP
            for files in ["efd/PassBack*.txt", "efd/composite*.txt",
                          "ecp/OutToNEMS*.txt", "ecp/composite*.txt"]:
                log_it(n, f"Deleting any files that are {files}")
                z = glob.glob(os.path.join(os.getcwd(), files))
                z = [i for i in z[:] if os.path.isfile(i)]
                for f2 in z:
                    try:
                        os.remove(f2) 
                    except:
                        pass

            if "local" in run_type:
                my_py = os.path.join(cwd, 'nems_flow.py')
                log_it(n, f"calling 'subprocess.run([CALL_P, my_py])'")
                with open(LOGFILE, "a") as log:
                    my_process = subprocess.run([CALL_P, my_py], stdout=log, stderr=log)

                # only works with 'local' runs
                log_it(n, f"my_process.returncode: {my_process.returncode}")
                if my_process.returncode != 0:
                    s = "ERROR Stopping since my_process.returncode != 0"
                    stop_it(n, s)

            elif "queue" in run_type:
                log_it(n, f"calling run_task with {os.getcwd()}")
                my_process = run_task.run_task(os.getcwd(), [CALL_P, 'nems_flow.py'], os.environ['COMPUTERNAME'])
           
            log_it(n, "begin looking for 'done.txt' file")
            my_timer = 0
            stopped = check_stop_signal()
            done = (os.path.exists(os.path.join(cwd, "done.txt")) or os.path.exists(os.path.join(cwd, "no_modules_on.txt")))
            while (my_timer < TIME_LIMIT) and (not done) and (not stopped):
                log_it(n, f"my_timer: {my_timer/60} minutes")
                time.sleep(1 * 60)
                my_timer += (1 * 60) 
                done = os.path.exists(os.path.join(cwd, "done.txt"))
                stopped = check_stop_signal()

            if stopped:
                shutil.copy2(LOGFILE, LOGFILE_BEFORE_STOP)
                stop_it(n, f"detected stop signal: {STOP_SIGNAL}")
                
            else:
                log_it(n, f"done: {done}")
                if not done:
                    stop_it(n, "ERROR Not done by time limit")

            nruns = check_intercycle_convergence(minscore, nruns, n, doemall, doaminof, 0)

            if int(SCEDES["NEMRWR"]) >= 1:
                log_it(n, f"Call 'reporter_wrapper' since  int(SCEDES['NEMRWR']) == {int(SCEDES['NEMRWR'])}")
                reporter_wrapper(n, SCEDES, nruns)

            if n < nruns:
                log_it(n, "Call 'update_current_data_files' since n < nruns")
                update_current_data_files(n)

            log_it(n, "increment cycle counter")
            n += 1
       
        log_it(n, "call 'do_end_of_run_tasks'")
        do_end_of_run_tasks(SCEDES, minscore, nruns, n, doemall, doaminof)
        log_it(n, "after function 'do_end_of_run_tasks'")

    elif "parallel" in run_type:
        cwd = os.getcwd()

        for f in ["scedes.all"]:
            for j in ["p1", "p2", "p3"]:
                shutil.copy2(os.path.join(cwd, f), os.path.join(cwd, j, f))
        
        if LOCAL_DEBUG:
            # copy files that are often changed when actively developing code
            log_it(0, "local run: {LOCAL_DEBUG=}")
            log_it(0, "copying various files to p1, p2, and p3")
            for f in ["nems_flow.py", "nems_flow_wrapper.py", 
                      "aimms_util.py", "aimms_wrapper.py", "prenems_aimms.py",
                      "main/aimms_util.py", "main/aimms_wrapper.py", "main/prenems_aimms.py"]:
                for j in ["p1", "p2", "p3"]:
                    shutil.copy2(os.path.join(cwd, f), os.path.join(cwd, j, f))

        n = 1
        while n <= nruns:
            cwd = os.getcwd()
            log_it(n, f"os.getcwd(): {cwd}, n: {n}, nruns: {nruns}")
        
            # be extra careful that there are no spurious "done.txt" files
            for j in ["p1", "p2", "p3"]:
                if os.path.isfile(f"{j}/done.txt"):
                    os.remove(f"{j}/done.txt")

            # delete any old AIMMS results
            for my_folder in ["p1/ngas/fromAIMMS", "p2/ngas/fromAIMMS",
                              "p1/coal/fromAIMMS", "p2/coal/fromAIMMS",
                              "p1/hmm/fromAIMMS", "p2/hmm/fromAIMMS",
                              "p2/rest/fromAIMMS"]:
                log_it(n, f"Deleting any old AIMMS 'toNEMS' files from {my_folder}")
                if os.path.isdir(my_folder):
                    files = glob.glob(f"{my_folder}/GlobalDatatoNEMS*")
                    for f in files:
                        os.remove(f)

            # Remove any folders containing parquet files for AIMMS modules
            for my_folder in ["p1/ngas/toAIMMS", "p2/ngas/toAIMMS",
                              "p1/coal/toAIMMS", "p2/coal/toAIMMS",
                              "p1/hmm/toAIMMS", "p2/hmm/toAIMMS",
                              "p2/rest/toAIMMS"]:
                ToAIMMSPath = os.path.join(os.getcwd(), my_folder)
                if os.path.isdir(ToAIMMSPath):
                    log_it(n, f"Deleting any old AIMMS 'toAIMMS' folders from {my_folder}")
                    for item in os.listdir(ToAIMMSPath):
                        item_path = os.path.join(ToAIMMSPath, item)
                        if os.path.isdir(ToAIMMSPath) and item.startswith("GlobalDataToAIMMS_"):
                            try:
                                shutil.rmtree(item_path)
                            except:
                                pass
            
            # Remove files from EFD and ECP
            for files in ["p2/efd/PassBack*.txt", "p2/efd/composite*.txt",
                          "p2/ecp/OutToNEMS*.txt", "p2/ecp/composite*.txt"]:
                log_it(n, f"Deleting any files that are {files}")
                z = glob.glob(os.path.join(os.getcwd(), files))
                z = [i for i in z[:] if os.path.isfile(i)]
                for f2 in z:
                    try:
                        os.remove(f2) 
                    except:
                        pass
            
            for my_dir in ["p1", "p2"]:  # Note: "p3" is run AFTER "p1" and "p2" have BOTH completed
                with open(os.path.join(cwd, my_dir, "curirun.txt"), "w") as f_irun:
                    f_irun.write(str(n))

            if "local" in run_type:
                queue = Queue()

                processes = []
                for my_dir in ["p1", "p2"]:
                    my_path = os.path.join(cwd, my_dir)
                    processes.append(multiprocessing.Process(target=thread_function_multi, 
                                                             args=(queue,), 
                                                             kwargs={'my_path': my_path, 'my_call_p': CALL_P}))
                
                # start child processes
                log_it(n, "run 'process.start()'")
                for process in processes:
                    process.start()
                # wait for child processes to finish
                log_it(n, "run 'process.join()'")
                for process in processes:
                    process.join()
                # shutdown the queue correctly
                queue.put(None)

                os.chdir(cwd)

            elif "queue" in run_type:
                cwd = os.getcwd()

                log_it(n, "start 'threading.Thread'")
                # change dir to p1 and call run run_task in a thread
                os.chdir(os.path.join(cwd, "p1"))
                log_it(n, f"calling run_task with {os.getcwd()}")
                p1 = threading.Thread(target=run_task.run_task, args=(os.getcwd(), [CALL_P, 'nems_flow.py'], os.environ['COMPUTERNAME']))
                p1.start()

                # change dir to p2 and call run run_task in a thread
                os.chdir(os.path.join(cwd, "p2"))
                log_it(n, f"calling run_task with {os.getcwd()}")
                p2 = threading.Thread(target=run_task.run_task, args=(os.getcwd(), [CALL_P, 'nems_flow.py'], os.environ['COMPUTERNAME']))
                p2.start()

                # wait for p1 and p2 to finish
                log_it(n, "'join' p1 and p2 processes")
                p1.join()
                p2.join()

                os.chdir(cwd)

                # TODO: check p1.returncode, p2.returncode

            # check that each of "p1" and "p2" completed successfully
            done_par = {i: ((os.path.exists(os.path.join(cwd, i, "done.txt")) or os.path.exists(os.path.join(cwd, i, "no_modules_on.txt")))) for i in ["p1", "p2"]}

            log_it(n, "begin looking for 'done.txt' file in p1 and p2 folders")
            my_timer = 0
            stopped = check_stop_signal()
            while (my_timer < TIME_LIMIT) and (not done_par["p1"]) and (not done_par["p2"]) and (not stopped):
                log_it(n, f"my_timer: {my_timer/60} minutes")
                time.sleep(1 * 60)
                my_timer += (1 * 60) 
                done_par = {i: os.path.exists(os.path.join(cwd, i, "done.txt")) for i in ["p1", "p2"]}
                stopped = check_stop_signal()

            if stopped:
                shutil.copy2(LOGFILE, LOGFILE_BEFORE_STOP)
                stop_it(n, f"detected stop signal: {STOP_SIGNAL}")

            else:
                log_it(n, f"done: {done_par}")
                done_done = not [i for i in done_par.values() if not i]
                if not done_done:
                    stop_it(n, f"Not done by time limit")
                   
            # # "p3" is run AFTER "p1" and "p2" have BOTH completed
            # ---------
            log_it(n, "manage restart files from p1 and p2 via tfiler")
            log_it(n, f"MERGE_P1_P2: {MERGE_P1_P2}")

            tfiler_file = 'tfiler.files'
            with open(tfiler_file, 'w') as f:
                f.write("Dictionary input/dict.txt\n")
                f.write("Restarti   p1/restart.unf\n")
                f.write("In Format  1  Unformatted\n")
                f.write("Varlist    p1/input/varlist.txt\n")

            if not MERGE_P1_P2:
                p3_path = os.path.join(cwd, "p3")
                p3_files = glob.glob(os.path.join(p3_path, "input", "restarti*"))
                for i in p3_files:
                    os.remove(i)
                with open(tfiler_file, 'a') as f:
                    f.write("Restarto   p3/restarti.unf\n")
                with open(os.path.join(p3_path, "filelist")) as f:
                    x = f.readlines()
                for i, j in enumerate(x):
                    if j.strip().lower().startswith("restarti"):
                        x[i] = x[i].replace("rlx", "unf").replace('RLX', 'unf')
                with open(os.path.join(p3_path, "filelist"), "w") as f:
                    f.write(("").join(x))
            else:
                if SCEDES["NRUNS"] == '1':
                    with open(tfiler_file, 'a') as f:
                        f.write("Restarto   p3/input/restarti.unf\n")
                else:
                    with open(tfiler_file, 'a') as f:
                        f.write("Restarto   p3/RESTART.IN\n")

            with open(tfiler_file, 'a') as f:
                f.write("Out Format 1\n")
                f.write("Filer Obj  na\n")
                f.write("date-time-na\n")
                f.write("===FYearSubset=0==FYearStart=51==FYearEnd=51===\n")
                f.write("p2/restart.unf\n")
                f.write(" \n")
                # need extra blank line for where it tries to read a second, optional dictionary file.
                f.write(" \n")

            with open(LOGFILE, "a") as f_out:
                log_it(n, "Joining P1 and P2 restarts.  Here we go!")
                npz_join.main(npz_p1="p1/restart.npz", npz_p2="p2/restart.npz", varlist_file="p2//input//varlistrec.txt", nruns = SCEDES["NRUNS"])         
            # ---------

            log_it(n, "About to start p3")
            my_dir = "p3"
            my_path = os.path.join(cwd, my_dir) 
                
            with open(os.path.join(cwd, my_dir, "curirun.txt"), "w") as f_irun:
                f_irun.write(str(n))

            if "local" in run_type:
                cwd = os.getcwd()
                os.chdir(os.path.join(cwd, my_dir))
                log_it(n, f"calling 'subprocess.run' for p3")
                with open(LOGFILE, "a") as f_out:
                    my_process = subprocess.run([CALL_P, "nems_flow.py"], stdout=f_out, stderr=f_out)
                os.chdir(cwd)

                log_it(n, f"{my_process.returncode=}")

                # only works for local (non-queue runs)
                if my_process.returncode != 0:
                    s = f"ERROR my_process.returncode != 0"
                    stop_it(n, s)

            elif "queue" in run_type:
                cwd = os.getcwd()
                os.chdir(os.path.join(cwd, my_dir))
                log_it(n, f"calling run_task with {os.getcwd()}")
                my_process = run_task.run_task(os.getcwd(), [CALL_P, "nems_flow.py"], os.environ["COMPUTERNAME"])
                os.chdir(cwd)

                log_it(n, f"queue run. my_process = {str(my_process)}")

            done_par[my_dir] = os.path.exists(os.path.join(cwd, my_dir, "done.txt")) or os.path.exists(os.path.join(cwd, my_dir, "no_modules_on.txt"))

            # TODO: is following code block needed/useful ???
            my_timer = 0
            stopped = check_stop_signal()
            done_par[my_dir] = os.path.exists(os.path.join(cwd, my_dir, "done.txt")) or os.path.exists(os.path.join(cwd, my_dir, "no_modules_on.txt"))
            while (my_timer < TIME_LIMIT) and (not done_par[my_dir]) and (not stopped):
                log_it(n, f"my_timer {my_dir}: {my_timer/60} minutes")
                time.sleep(1 * 60)
                my_timer += (1 * 60) 
                done_par[my_dir] = os.path.exists(os.path.join(cwd, my_dir, "done.txt"))
                stopped = check_stop_signal()

            if stopped:
                shutil.copy2(LOGFILE, LOGFILE_BEFORE_STOP)
                stop_it(n, f"detected stop signal: {STOP_SIGNAL}")
            else:
                log_it(n, f"done: {done_par}")
                done_done = not [i for i in done_par.values() if not i]
                if not done_done:
                    stop_it(n, f"Not done by time limit")

            os.chdir(cwd)  # TODO: why is this necessary?

            # copy p3 ending restart-related files to parent folder
            shutil.copy2("p3/restart.npz", "restart.npz")
            try:
                shutil.copy2("p3/NEMSVardf.csv", "NEMSVardf.csv")
            except:
                log_it(n, "unable to copy NPZ, NEMSVardf.csv")
                pass

            log_it(n, "call 'nruns = check_intercycle_convergence'")
            nruns = check_intercycle_convergence(minscore, nruns, n, doemall, doaminof, 0)


            if int(SCEDES["NEMRWR"]) >= 1:
                log_it(n, f"Call 'reporter_wrapper' since  int(SCEDES['NEMRWR']) == {int(SCEDES['NEMRWR'])}")
                reporter_wrapper(n, SCEDES, nruns)

            if n < nruns:
                log_it(n, "Call 'update_current_data_files' since n < nruns")
                update_current_data_files(n)

            if StopCode == 1 and n >= doaminof:
                log_it(n, "Inter-Cycle Convergence met. Exiting Loop")
                break

            log_it(n, "increment cycle counter")
            n += 1

        log_it(n, "call 'do_end_of_run_tasks'")
        do_end_of_run_tasks(SCEDES, minscore, nruns, n, doemall, doaminof)
        log_it(n, "after function 'do_end_of_run_tasks'")

    else:
        s = f"Unknown run_type: {run_type}"
        stop_it(n, s)

    elapsed = datetime.now() - start_time
    s = f"Total time (d:hh:mm): {elapsed.days:01}:{(elapsed.seconds//3600):02}:{(elapsed.seconds//60 % 60):02}"
    log_it(n, s)

    shutil.copy2("nohup.out", "nems-log.txt")
    if 'parallel' in run_type:
        for i in ["p1", "p2", "p3"]:
            try:
                shutil.copy2(f"{i}/nohup.out", f"{i}/nems-log.txt")
            except Exception:
                log_it(n, "Unable to copy nohup.out to nems_log.txt in p1/p2/p3 (?)")
    else:
        log_it(n, "Sequential run, skip copying nohup.out to nems-log.txt in p1/p2/p3.")

    os.sys.exit(0)


if __name__ == '__main__':
    # for GUI runs and for runs from the IDE
    try:
        dest_path = sys.argv[1]
        run_string = sys.argv[2]

    except:
        log_it(0, "NOTICE: this is a LOCAL (non-queue) run")
        dest_path = os.getcwd()
        if os.path.exists("p1"):
            run_string = "local_parallel"
        else:
            run_string = "local_sequential"

        for i in ["intercv", 
                  "oscillate"]:
            if not os.path.isfile(os.path.join(dest_path, i + ".exe")):
                os.rename(os.path.join(dest_path, i + ".xxx"), os.path.join(dest_path, i + ".exe"))

        for i in ["main/parse_scedes.py", 
                  "run_task.py"]:
            if not os.path.isfile(os.path.join(dest_path, i)):
                s = f"ending. could not find {i}"
                stop_it(0, s)
    
    run(dest_path, run_string)
