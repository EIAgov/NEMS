"""
Created August 8 2024
Revised April 24 2025

@author: ???

Perform some setup tasks then insert a NEMS 'task' into the RabbitMQ based queue system.

A 'task' is typically going to be either the process running cycle.py or nems_flow.py
depending on which part of NEMS is calling run_task()/run_task_with_timeout().

NOTE: Creation date was determined by the first time this file was seen in the main branch
of the NEMS gitlab repository.
"""

from celery import Celery
import os
import sys
import multiprocessing
import json
from pathlib import Path
from kombu import Exchange, Queue

MYHOST = os.environ['COMPUTERNAME']
SECRETS_FILE = "O:\\python_environments\\secrets.json"

def run_task_with_timeout(loc, comm, queue_name, priority = 0):
    """
    Create a subprocess to insert a task into the NEMS queue. Then wait up to
    the timeout for the task to insert.

    This function BLOCKS until the sub-process has completed through the
    the use of the multiprocessing.Queue.get() function.

    Parameters
    ----------
    loc : string
        The path to the run's output directory (nemsbase.path_datekey).
    comm : list
        A list containing the arguments for the task to execute. The contents of the list
        can vary with the task being called. However NEMS, currently, only has one function
        designed to be executed as a task (tasks.exec_at_loc()). For this the arguments are:
        arg 0 : Path to the Python executable
        arg 1 : Name of the Python script to execute.
        arg 2-n : Any arguments expected by the script specified in argument 1.
    queue_name : string
        Name of a queue for the RabbitMQ server.
    priority : int
        A priority for the task to use once when it is inserted into the queue.
    timeout : int
        A timeout value in seconds to use.
    """

    result_list = multiprocessing.Queue()
    result = -1

    # Create a separate process to run the long-running function 
    process = multiprocessing.Process(target=setup_queue_target, args = (result_list, loc, comm, queue_name, priority)) 
    process.start()

    # Wait for the process to complete within the timeout period 
    process.join(10)

    # Check if the process is still alive (i.e., the function didn't complete in time) 
    if process.is_alive():
        process.terminate() # Terminate the process if it exceeded the timeout 
        process.join() # Wait for process to finish termination 
        print(f"NEMS Job Submitted into Queue") 
    
    else:
        # If the process finished on time, get the result from the queue 
        result = result_list.get()
    return result

def setup_queue_target(result_list, loc, comm, queue_name, priority):
    """
    Wrapper function used by a child process to store setup the task's output
    for later retrieval through a multiprocessing queue.

    Parameters
    ----------
    loc : string
        The path to the run's output directory (nemsbase.path_datekey).
    comm : list
        A list containing the arguments for the task to execute. The contents of the list
        can vary with the task being called. However NEMS, currently, only has one function
        designed to be executed as a task (tasks.exec_at_loc()). For this the arguments are:
        arg 0 : Path to the Python executable
        arg 1 : Name of the Python script to execute.
        arg 2-n : Any arguments expected by the script specified in argument 1.
    queue_name : string
        Name of a queue for the RabbitMQ server.
    priority : int
        A priority for the task to use once when it is inserted into the queue.
    """
    result_list.put(run_task(loc, comm, queue_name, priority=priority))


def run_task(loc, comm, queue_name, priority = 0):
    """
    Wrapper function to be used by child processes to connect to RabbitMQ and send
    the task into the specified queue.

    Parameters
    ----------
    loc : string
        The path to the run's output directory (nemsbase.path_datekey).
    comm : list
        A list containing the arguments for the task to execute. The contents of the list
        can vary with the task being called. However NEMS, currently, only has one function
        designed to be executed as a task (tasks.exec_at_loc()). For this the arguments are:
        arg 0 : Path to the Python executable
        arg 1 : Name of the Python script to execute.
        arg 2-n : Any arguments expected by the script specified in argument 1.
    queue_name : string
        Name of a queue for the RabbitMQ server.
    priority : int
        A priority for the task to use once when it is inserted into the queue.

    Notes
    ----------
    We declare myqueue = ... to prevent Celery attempting to redefine the queue named in queue_name.
    An exception will be thrown due to a mis-match on the Celery parameter x-max-priority if we do
    not declare this.
    """
    ## Queue configuration
    # TODO: Migrate this code to a function
    mode = "TEST" if 'tstnem' in MYHOST.lower() else "NEMS"
    with open(SECRETS_FILE, encoding='UTF-8', mode='r') as s:
        t = json.load(s)
    host = t[mode]['RabbitMQ']['HOST']
    password = t[mode]['RabbitMQ']['SECRET']
    user = t[mode]['RabbitMQ']['USER']
    # ssl_pfx = t['SSL']['SSL_PFX']
    # nems_ssl = {
    #     'keyfile': f"{ssl_pfx}{t['SSL']['KEYFILE']}",
    #     'certfile': f"{ssl_pfx}{t['SSL']['CERTFILE']}",
    #     'ca_certs': f"{ssl_pfx}{t['SSL']['CA_CERTS']}",
    #     'cert_reqs': f"{t['SSL']['CERT_REQS']}"
    # }
    # for f in nems_ssl.values():
    #     if(type(f) is not type(str)):
    #         continue
    #     p = Path(f)
    #     if not p.is_file():
    #         raise FileNotFoundError(f"Missing SSL files: {p}")
    myqueue = Queue(queue_name, no_declare=True)
    ## end queue configuration

    UID, OUTDIR = get_run_args(loc)
    app = Celery("tasks", broker=f"pyamqp://{user}:{password}@{host}//", backend="rpc://") #, broker_use_ssl=nems_ssl)
    result = app.send_task("tasks.exec_at_loc", args=[UID, loc, OUTDIR, comm], queue=myqueue, priority = priority).get()
    return result

def get_run_args(loc):
    """
    Function to parse NEMS arguments from a string.

    Parameters
    ----------
    loc : string
        The path to the run's output directory (nemsbase.path_datekey).
    
    Returns
    ----------
    Returns the user-id and outdir for the run.
    """
    """
    Function to parse NEMS arguments from a string.

    Parameters
    ----------
    loc : string
        The path to the run's output directory (nemsbase.path_datekey).
    
    Returns
    ----------
    Returns the user-id and outdir for the run.
    """
    launch_file = 'launched.from'
    par = ['p1', 'p2', 'p3']
    dirs = os.path.split(loc)
    p = dirs[1]
    if p in par:
        launch_file = os.path.join(dirs[0], launch_file)
    else:
        launch_file = os.path.join(loc, launch_file)
    UID = ''
    OUTDIR = ''
    try: 
        lines = ''
        with open(launch_file, 'r') as f:
            lines = f.readlines()

        UID = lines[4].split()[1]
        OUTDIR = lines[-1].split('=')[1].strip()
    except:
        UID = os.getenv("USERNAME")
        OUTDIR = loc

    return UID, OUTDIR 


if __name__ == "__main__":
    sys.exit(run_task(sys.argv[1], sys.argv[2], sys.argv[3]))
