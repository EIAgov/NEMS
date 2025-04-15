
"""
Rewrite the bat files as a python script that gets called from submit.py
Converted by: Nathan Agbemenyale
"""
import os
import subprocess
import shutil
import sys
import time
import pyf_duplicates_remover
import csv
import pandas as pd
import filecmp

print(os.getcwd())
sys.path.append('../models')
import main.parse_scedes as psf


# The function accepts an argument that decides a full build or partial build
def make_pyd_files(FULL_BUILD_FLAG, env, work_dir,uscedes):
    """The major meson process flow control method to generate and trim signature files (.pyf), 
    parse the wrapper and module.c file, and run meson build to generate .pyd files, based on the value of FULL_BUILD_FLAG parameter.

    Args:
        FULL_BUILD_FLAG (_int_): full build indicator flag.
            -when the flag = 0: start the process from PyFilerf2py_build.py execution. This mode takes the longest process time.
            -when the flag = 1: start the meson process from pyfiler1.pyf and pyfiler2.pyf execution. Also trim the duplicates
                                and parse wrapper and module.c files in desired format.
            -when the flag = 2: directly run meson build to generate pyfiler1.pyd and pyfiler2.pyd files.

        env (_str_): the Python virtrul environment path
        work_dir (_str_): the working directory where the Fortran files locate
    """
    if FULL_BUILD_FLAG not in (0, 1, 2):
        print("INVALID BUILD VALUE. Please provide 0 (fully build pyf and pyd) or 1 (partial build) or 2 (run meson build process):")
        return

    # Activate virtual environment where numpy and other modules are installed to avoid the "ModuleNotFoundError"
    # env=r'O:\python_environments\aeo2025_py311\Scripts'
    call_p= os.path.join(env, 'scripts\python.exe')
    current_directory = os.getcwd()
    os.chdir(work_dir)

    # Run F2py and generate a pyf, process the pyf via F2Py, format the secondary files, and run the meson build process
    if FULL_BUILD_FLAG == 0:

        # Build pyf
        pybuild = "PyFilerf2py_build.py"
        result=subprocess.run([call_p, pybuild])
        capture_error_and_exit(result,pybuild)

    # Processes the existing pyf via F2Py,format the secondary files and run the meson build process
    if FULL_BUILD_FLAG in (0, 1):
        pyfs=("pyfiler1.pyf","pyfiler2.pyf")
        for pyf in pyfs:
            result=subprocess.run([call_p, "-m", "numpy.f2py", pyf])
            capture_error_and_exit(result,pyf)

        # remove the duplicates in pyf:
        pyf_duplicates_remover.main()

        # now parse the wrapper and module.c file
        wpr = "meson_wrapper_parser.py"
        result=subprocess.run([call_p, wpr])
        capture_error_and_exit(result,wpr)

        mod = "meson_module_c_parser.py"
        result=subprocess.run([call_p, mod])
        capture_error_and_exit(result,mod)

        # sleep for 5 seconds
        time.sleep(5)

    # Run the meson build process 
    if FULL_BUILD_FLAG in (0, 1, 2):
        SCEDES = psf.parse_scedes_file(uscedes)
       # Call ifort and meson build system from virtual environment to setup builddir1
        NEMS_PATH=os.path.dirname(work_dir)
        a = get_ifort_executible_path(NEMS_PATH)

        load_meson_data(SCEDES,NEMS_PATH)

        b1=f'{a} meson setup builddir'
        result=subprocess.run(b1, shell=True)
        capture_error_and_exit(result,b1)

        c1=f'{a} meson.exe compile -C builddir'
        result=subprocess.run(c1, shell=True)
        capture_error_and_exit(result,c1)

    os.chdir(current_directory)

def capture_error_and_exit(result,msg):
    """check return code of the subprocess result, output the desired error message and exit the program

    Args:
        result (_obj_): the return object from the subprocess call
        msg (_str_): the desired output message
    """
    rc = result.returncode
    print(f'returncode={rc} in {msg}')
    if rc!=0:
        print(f'FAILED in {msg}. Exit the program now!')
        os.sys.exit()

def copy_pyd_to_pyfiler(work_dir):
    """The method to copy the pyd files to $NEMS\scripts\Pyfiler folder.

    Args:
        work_dir (_str_): the working directory where the Fortran files locate. i.e. $NEMS\source
    """
    # get the directory of the script
    current_directory = os.getcwd()
    os.chdir(work_dir)

    script_dir = os.path.dirname(os.path.realpath(__file__))
    # files to copy
    files_to_copy = [r"pyfiler1.cp311-win_amd64.pyd", r"pyfiler2.cp311-win_amd64.pyd"]
    folddir = [r"builddir", r"builddir"]
    for foldername, file_name in zip(folddir, files_to_copy):
        # source and destination path relative to NEMS folder or directory
        source_path = os.path.join("..", "source", foldername, file_name)
        destination_path = os.path.join("..", "scripts", "PyFiler", file_name)
        # check if source file exists
        if os.path.exists(source_path):
            shutil.copy(source_path, destination_path)
            print(file_name,":File copied successfully.")
        else:
            print(file_name,":File does not exist.")

    # files to copy
    files_to_copy = ["intercv.exe","tfiler.exe"]
    folddir = [r"builddir",r"builddir",r"builddir"]
    for foldername, file_name in zip(folddir, files_to_copy):
        # source and destination path relative to NEMS folder or directory
        source_path = os.path.join("..", "source", foldername, file_name)
        destination_path = os.path.join("..", "source", file_name)
        # check if source file exists
        if os.path.exists(source_path):
            shutil.copy(source_path, destination_path)
            print(file_name,"File copied successfully.")
        else:
            print(file_name,":File does not exist.")

    os.chdir(current_directory)


def get_ifort_executible_path(NEMS_PATH):
    """Read ifort command file path from $NEMS\scripts\setup\input\nems_config_xilink_nems_ftab.csv and return the parsed command string.

    Args:
        NEMS_PATH (_str_): the directory path of NEMS source code folder.
    
    Returns:
        ifort_executible (_str_): the command string to call ifort executible
    """
    file = os.path.join(NEMS_PATH,'scripts','setup','input','nems_config_xilink_nems_ftab.csv')
    ifortvars_path = ''
    with open(file,newline='') as csvfile:
        rd = csv.DictReader(csvfile)  
        for line in rd:
            if line['key'] == 'IFORTVARS_PATH':
                ifortvars_path = line['value']

    ifort_executible = f'"{ifortvars_path}"&&'
    return ifort_executible

def load_meson_data(SCEDES,NEMS_PATH):
    ''' 
    reads a csv and populates the meson template
    '''
    finalfile='meson.build'
    tempfile='meson.build2'
    shutil.copy('meson_template.build',tempfile)
    import pandas as pd
    #read the data in 
    files_df=pd.read_csv('../scripts/setup/input/meson_file_configuration.csv')
    #create three lists that will be used to populate meson
    pyfiler1_file_list=[]
    pyfiler2_file_list=[]
    tfiler_file_list=[]
    #if its a scedes key, look it up, and replace $nems with the nems_path.  Otherwise, use the path.
    for i in files_df.iterrows():
        a=i[1]['list']
        final_path=i[1]['name']
        if i[1]['scedes_key']==1:
            temp_file_name=SCEDES[final_path]
            #If its a real path, just use it.
            final_path=temp_file_name
            #if its a $nems path, fix it. 
            if temp_file_name[0]=='$':
                final_path=os.path.join(NEMS_PATH,"/".join(temp_file_name.split('/')[1:]))
        if a=='pyfiler1':
            pyfiler1_file_list.append(final_path)
        if a=='pyfiler2':
            pyfiler2_file_list.append(final_path)
        if a=='tfiler':
            tfiler_file_list.append(final_path)

    #write it into meson, looking for the three places these lists go.

    filein = open('meson_template.build', 'r')
    fileout= open(tempfile, 'w')
    Lines = filein.readlines()
    for line in Lines:
        fileout.write(line)
        if "py.extension_module('pyfiler1'," in line:
            for i in pyfiler1_file_list:
                fileout.write("'")
                fileout.write(i)
                fileout.write("',\n")
        if "py.extension_module('pyfiler2'," in line:
            for i in pyfiler2_file_list:
                fileout.write("'")
                fileout.write(i)
                fileout.write("',\n")
        if "executable('tfiler'," in line:
            for i in tfiler_file_list:
                fileout.write("'")
                fileout.write(i)
                fileout.write("',\n")
    filein.close()
    fileout.close()
    #only replace the meson.build file if its different
    print('Meson: Checking to see if build file is different')
    if (open(tempfile,'r').read() == open(finalfile,'r').read()):
        print('Build Files are the same, using old file')
    else:
        print('Meson: Build Files are not the same')
        shutil.copy2(tempfile,finalfile)