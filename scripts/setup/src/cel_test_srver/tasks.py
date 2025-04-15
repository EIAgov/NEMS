from celery import Celery
from celery.utils.log import get_task_logger
import os
import subprocess
import shutil
import glob

host = "ASHTSTNEMVIR002"
user = "user"
password = "test"

app = Celery("tasks", broker=f"pyamqp://{user}:{password}@{host}//", backend="rpc://")
logger = get_task_logger(__name__)
@app.task
def exec_at_loc(userid, loc, output_dir, comm):
    p_path = None
    if 'cycle' in comm:
        logger.info('run started in output directory: ' + loc)
    else:
        path_parts = loc.split(os.sep)
        p_path = path_parts[-1]
        logger.info(p_path + ' started at: ' + loc)

    outdir = None
    if not loc.startswith(os.path.join("D:",os.sep,"workdir")):
        outdir = loc
        path, datekey = os.path.split(os.path.normpath(loc))
        path, scen = os.path.split(path)
        workdir = os.path.join("D:",os.sep,"workdir",scen,datekey)
        if os.path.exists(workdir):
            shutil.rmtree(workdir)
        shutil.copytree(outdir,workdir)
        loc = workdir
    curr_dir = outdir
    if curr_dir is None:
        curr_dir = os.getcwd()
    with open(loc + '\\nohup.out', "a") as outfile:
        proc = subprocess.run(comm, text=True, stdout=outfile, stderr=outfile, cwd = loc)
        if p_path:
            outfile.write(p_path + ' done')

  
    if not outdir is None:
        for p in range(1,4):
            pfold = os.path.join(loc,"p"+str(p))
            if os.path.exists(os.path.join(pfold,"input")):
                shutil.rmtree(os.path.join(pfold,"input"))
                for f in glob.glob(os.path.join(pfold,"*.dll")):
                    os.remove(f)
                for f in glob.glob(os.path.join(pfold,"*.exe")):
                    os.remove(f)
        if os.path.exists(os.path.join(loc,"input")):
            shutil.rmtree(os.path.join(loc,"input"))
            if os.path.exists(os.path.join(loc,"scedes.all")):
                os.remove(os.path.join(loc,"scedes.all"))
            if os.path.exists(os.path.join(loc,"ftab.exe")):
                shutil.copy(os.path.join(loc,"ftab.exe"),os.path.join(loc,"ftab.xxx"))
            for f in glob.glob(os.path.join(loc,"*.dll")):
                os.remove(f)
            for f in glob.glob(os.path.join(loc,"*.exe")):
                os.remove(f)
        if os.path.exists(os.path.join(outdir,"ftab.exe")):
            shutil.copy(os.path.join(outdir,"ftab.exe"),os.path.join(outdir,"ftab.xxx"))
        shutil.copytree(loc,outdir,dirs_exist_ok=True)
        shutil.rmtree(loc)
    return proc.returncode
