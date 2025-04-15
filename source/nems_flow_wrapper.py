""" Wrapper to use instead of calling nems_flow.py directly, when multiprocessing """
from datetime import datetime
import os
import subprocess
import sys

LOGFILE = "nohup.out"


# the following fails sometimes
#MODULE_NAME = os.path.basename(vars(sys.modules[__name__])['__file__'])
MODULE_NAME = "nems_flow_wrapper.py"


def print_it(s):
    print(f"{datetime.now()} :: {MODULE_NAME} :: {s}\n")
    sys.stdout.flush()
    sys.stderr.flush()


def run():
    with open("scedes.all", "r") as f_in:
        z = f_in.readlines()
    z2 = [i for i in z if i.split("=")[0] == "NEMSPYENN"][0]
    call_p = os.path.join(z2.split("=")[1].strip(), "scripts", 'python.exe')
   
    with open(LOGFILE, "w") as f_out:
        my_process = subprocess.run([call_p, "nems_flow.py"], stdout=f_out, stderr=f_out)


if __name__ == '__main__':
    run()