from datetime import datetime
import sys
import os

def print_it(CURIRUN, s, MODULE_NAME):
    """Prints messages onto the terminal and flush them from buffer
    
    Messages from both python 'print' and fortran 'write(*,*)' will 
    be written out of order onto the terminal if buffer is not periodically
    flushed. This function prints the messages with timestamps, cycle number, 
    and module names before flushing the stdout and stderr from the buffer.

    Parameters
    ----------

    CURIRUN : str
        Current NEMS cycle number
    s : str
        String to include in the print statement
    MODULE_NAME : str
        name of the python file where this print is coming from (e.g. prenems.py)

    Returns
    -------
    
    None

    """
    
    print(f"{datetime.now()} :: cycle {CURIRUN} :: {MODULE_NAME} :: {s}\n")
    sys.stdout.flush()
    sys.stderr.flush()


def prenems_setup_logs(pyfiler, user, CURIRUN):
    """Prints setup essential message and logs onto terminal

    Message contents:
        1.  Current NEMS Runtype (Jognem / Parnem)
        2.  SCEDES file name (e.g. ref2025_solo)
        3.  Datekey (e.g. d120925e)
        4.  HOST PC Name
        5.  Current Cycle Number
        6.  EPHRTS and H2 model switches

    Parameters
    ----------
    pyfiler : module
        pyfiler fortran module
    
    user : SimpleNamespace
        Contains dictionary of NEMS runtime information
    
    CURIRUN : str
        Current NEMS cycle number

    Returns
    -------
    
    None.

    """
    
    print(f"####################################################")
    print(f"####################################################")
    
    # 1.  Check then print Current NEMS Runtype (Jognem / Parnem)
    if user.runtype == 's':
        print(f"NEMS Run Type: JOGNEMS")
    elif user.runtype in ["p1", "p2", "p3"]:
        print(f"NEMS Run Type: PARNEMS {user.runtype}")
    else:
        print(f"NEMS Run Type: UNKNOWN")
    
    
    print(f"SCEDES: {pyfiler.utils.scen}")                          # 2.  SCEDES file name
    print(f"Datekey: {pyfiler.nchar.date}")                         # 3.  Datekey
    print(f"Host PC Name: {os.environ['COMPUTERNAME']}")            # 4.  HOST PC Name
    print(f"Current Cycle: {CURIRUN} of {user.SCEDES['NRUNS']}")    # 5.  Current Cycle Number


    
    # 6.  EPHRTS and H2 model switches
    #! EDT (06/25/22): toggle electricty h2 submodule
    EPHRTS = int(user.SCEDES['EPHRTS']) and ('EMM' in user.module_order)
    #TURN_ON = False if EPHRTS==0 else True
    TURN_ON = EPHRTS
    msg = 'ON' if TURN_ON else 'OFF'
    s = f'THE ELECTRICITY H2 SUBMODULE IS TURNED {msg}, EPHRTS = {EPHRTS}'
    print(f"{datetime.now()} :: cycle {CURIRUN} :: {s}\n")
    
    print(f"####################################################")
    print(f"####################################################")


    sys.stdout.flush()
    sys.stderr.flush()