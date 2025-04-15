import subprocess
import os
import sys
import warnings
import time

warnings.filterwarnings("ignore")


def run_ephrts(aimms_path, YEAR_STR, PASS_STR, debug, debug_path):
    """
    Python wrapper provided by the aimms documentation to call the ephrts aimms module

    https://how-to.aimms.com/Articles/257/257-run-batch-commands-in-aimms-command-line-tool.html

    Parameters
    ----------
    aimms_path - file path to the aimms module
    YEAR_STR - year string passed as an arugment flag
    PASS_STR - iteration passed as an argument flag

    Returns
    -------

    Empty : Runs the aimm module

    """

    if debug:
        os.chdir(debug_path)
    else:
        aimms_path = os.path.join(aimms_path, "Bin", "AimmsCmd.exe")

    # defined in scedes file
    aimms_flags = " --minimized --license-wait-seconds 30 "
    aimms_project = "ephrts\\ephrts.aimms"

    # point to ephrts file
    ephrts_command =  aimms_project+" < ephrts_debug\\ephrts_cmds_"+YEAR_STR+"_"+PASS_STR+".txt > ephrts_debug\\ephrts_error_log-"+YEAR_STR+"-"+PASS_STR+".txt"

    command = aimms_path + aimms_flags + ephrts_command

    # return up to n times (in case aimms license not found)
    ret = 999 # set to high number to let return of subprocess call to set to zero (ideally)

    for i in range(0,9):
        ret = subprocess.call(command, shell=True) # do not trust the return code from aimms, explicitly check the log file if "Return Value = 0" exists

        # explicitly search for the return value = 0 in the log file
        check = False
        with open("ephrts_debug\\ephrts_error_log-"+YEAR_STR+"-"+PASS_STR+".txt", 'r') as log_file:
            if 'Return value = 0' in log_file.read():
                check = True

        # check for aimms error file
        if os.path.exists("ephrts\\log\\aimms.err"):
            check = False

        if check:
            print('Ephrts exited properly on try (license attempt) number ', i)
            break
        else:
            print("warning : possible ephrts optimization failure or likely licence issue...trying again")
            time.sleep(10)

    if not check:
        os.chdir("../ephrts/")
        print("ephrts crashed in RunEphrts.py, likely due to an infeasibility")
        with open("crash_nems.txt") as f:
            f.write('ephrts crashed in RunEphrts.py') # there's a check in the fortran code that looks for this file and will stop the NEMS run


if __name__ == "__main__":

    run_ephrts(aimms_path, YEAR_STR, PASS_STR)
