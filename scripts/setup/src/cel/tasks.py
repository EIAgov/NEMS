from celery import Celery
from celery.utils.log import get_task_logger
from datetime import datetime
import os
import subprocess
import shutil
import time
import glob
import json
from pathlib import Path
from kombu import Exchange, Queue

## Queue configuration
# TODO: Migrate this code to a function
MYHOST = os.environ['COMPUTERNAME']
SECRETS_FILE = "O:\\python_environments\\secrets.json"
MODE = "TEST" if 'tstnem' in MYHOST.lower() else "NEMS"
with open(SECRETS_FILE, encoding='UTF-8', mode='r') as s:
    t = json.load(s)
host =  t[MODE]['RabbitMQ']['HOST']
password = t[MODE]['RabbitMQ']['SECRET']
user = t[MODE]['RabbitMQ']['USER']
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
app = Celery("tasks",
            broker=f"pyamqp://{user}:{password}@{host}//",
            backend="rpc://",
            task_queues = [
                Queue('priority',
                Exchange('priority'),
                routing_key='priority',
                queue_arguments={'x-max-priority': 2})
            ]) #, broker_use_ssl=nems_ssl)
## end queue configuration

logger = get_task_logger(__name__)
@app.task
def exec_at_loc(uid, loc, outdir, comm):
    """
    Execute a NEMS job in a sub-process. Generally we expect this function will 
    be running cycle.py or nems_flow.py

    Also performs some 'cleanup' tasks for end-of-run.
    
    Parameters
    ----------
    uid : string 
        3-character string representing username (unused)
    loc : string
        The path to the run's workdir directory (nemsbase.path_datekey).
    output_dir : string
        The path to the run's output directory.
    comm : list
        A list containing the arguments for the NEMS job. The contents of the list can 
        vary with the task being called. However NEMS, currently, only has one function
        designed to be executed as a task (tasks.exec_at_loc()). For this the arguments are:
        arg 0 : Path to the Python executable
        arg 1 : Name of the Python script to execute.
        arg 2-n : Any arguments expected by the script specified in argument 1.

    Returns
    -------
    Returns the return code of the sub-process.
    """
    p_path = None
    if 'cycle' in comm:
        logger.info('run started in output directory: ' + loc)
    else:
        path_parts = loc.split(os.sep)
        p_path = path_parts[-1]
        logger.info(p_path + ' started at: ' + loc)

    outdir = None
    if not loc.startswith(os.path.join("D:", os.sep, "workdir")):
        outdir = loc
        path, datekey = os.path.split(os.path.normpath(loc))
        path, scen = os.path.split(path)
        workdir = os.path.join("D:", os.sep, "workdir", scen,datekey)
        if os.path.exists(workdir):
            shutil.rmtree(workdir)
        shutil.copytree(outdir,workdir)
        loc = workdir
    curr_dir = outdir
    if curr_dir is None:
        curr_dir = os.getcwd()
    with open(loc + '\\nohup.out', "a") as outfile:
        proc = subprocess.run(comm, text=True, stdout=outfile, stderr=outfile, cwd = loc, timeout=256200)
        if p_path:
            outfile.write(p_path + ' done')

    if not outdir is None:
        for p in range(1,4):
            pfold = os.path.join(loc,"p"+str(p))
            if os.path.exists(os.path.join(pfold, "input")):
                shutil.rmtree(os.path.join(pfold, "input"))
                for f in glob.glob(os.path.join(pfold, "*.dll")):
                    os.remove(f)
                for f in glob.glob(os.path.join(pfold, "*.exe")):
                    os.remove(f)
        if os.path.exists(os.path.join(loc, "input")):
            shutil.rmtree(os.path.join(loc, "input"))
            if os.path.exists(os.path.join(loc, "scedes.all")):
                os.remove(os.path.join(loc, "scedes.all"))
            if os.path.exists(os.path.join(loc, "ftab.exe")):
                shutil.copy(os.path.join(loc, "ftab.exe"), os.path.join(loc, "ftab.xxx"))
            for f in glob.glob(os.path.join(loc, "*.dll")):
                os.remove(f)
            for f in glob.glob(os.path.join(loc, "*.exe")):
                os.remove(f)
        if os.path.exists(os.path.join(outdir,"ftab.exe")):
            shutil.copy(os.path.join(outdir,"ftab.exe"),os.path.join(outdir,"ftab.xxx"))
        shutil.copytree(loc,outdir,dirs_exist_ok=True)
        for i in range(5):
            try:
                shutil.rmtree(loc)
                break
            except Exception as e:
                logger.info(e)
                if i == 4:
                    returncode = -1
                    logger.info("Run complete but cleanup failed.")
                    return returncode
                else:
                    time.sleep(120)
        try:
            shutil.copytree(loc, outdir, dirs_exist_ok=True)
            shutil.rmtree(loc)
        except FileNotFoundError as e:
            print(f"{loc} does not exist! Was this file removed by another process?")
    return proc.returncode
