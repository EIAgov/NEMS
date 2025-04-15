from celery import Celery
import os
import sys
import multiprocessing
import time


def run_task_with_timeout(loc, comm, q):
    result_list = multiprocessing.Queue()
    
    # Create a separate process to run the long-running function 
    process = multiprocessing.Process(target=setup_queue_target, args = (result_list, loc, comm, q)) 
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
        result = result_queue.get()


def setup_queue_target(result_list, loc, comm, q):
    result_list.put(run_task(loc, comm, q))


def run_task(loc, comm, q):
    host = "XXXXXXXXXXXX"
    user = "XXXXXXXXXXXX"
    password = "XXXXXXXXXXXX"
    UID, OUTDIR = get_run_args(loc)
    app = Celery("tasks", broker=f"pyamqp://{user}:{password}@{host}//", backend="rpc://")
    return app.send_task("tasks.exec_at_loc", args=[UID, loc, OUTDIR, comm], queue=q).get()

def submit_to_queue():
    app.send_task("tasks.exec_at_loc", args=[UID, loc, OUTDIR, comm], queue=q).get()


def get_run_args(loc):
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
