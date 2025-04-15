# -*- coding: utf-8 -*-
"""
Created on Apr 04 2023
Revised Dec 2024

@author: Claire Su

Launch NEMS run via GUI (most users) or command-line (cygwin or GitLab runner)

To run this script from a cygwin command line:
1. set your current $NEMS. For example: NEMS=M:/ogs/my_user_id/git/NEMS_test
2. export NEMS
3. . $NEMS/scripts/commands.sh

4. In the $NEMS/scedes folder, type the command to activate Python environment 
and pass the arguments (see below).

Three argument cases are supported:
    
    . $NEMS/scripts/commands.sh
    submitpy="C:/python_environments/aeo2025_py311_D/Scripts/activate&python $NEMS/scripts/setup/src/nems_submit.py"

    cmd.exe /c "$submitpy"
    cmd.exe /c "$submitpy jog|par"
    cmd.exe /c "$submitpy jog|par <scenario> <outdir>"
    example: cmd.exe /c "$submitpy par ref2023 u:/output/rcs"

For EMM proprocessor, use:
    cmd.exe /c "$submitpy"
    or:
    cmd.exe /c "$submitpy pre"
"""
import fnmatch
import os
import shutil
import sys
import subprocess
import csv
import pandas as pd
from datetime import datetime
from network_drive import NetworkDrive
from meson_exec import make_pyd_files
from meson_exec import copy_pyd_to_pyfiler


log_prefix = '[Setup Program]'

USER = ""
NEMS_PATH = ""
NEMS_SCPT_PATH = ""


def main(arg_list):
    """
    Control overall program flow and invoke $NEMS/scripts/setup/src/nems_setup.py

    Parameters
    ----------
    arg_list : list
        the argument string list passed in when invoke this nems_submit.py
        Users have 2 ways to invoke nems_submit.py. Pass either no args 
        (then this program will ask you to choose), or 3 args.
        arg 0: mode of the run. par (parnems), jog (jognems), or pre (EMM preprocessor)
        arg 1: user scenario
        arg 2: output directory path
        arg 3: job type and checkbox on/off: local or queue, checkbox 1 (only create run folder) or 0

    Returns
    -------
    None
    """
    nemsh = "nemsone.shell"
    wpwd = os.getcwd()

    # ----------
    # process arguments
    mode = arg_list[0]
    if mode == "pre": 
        PREPRO = "yes"
    else:
        PREPRO = "no"
    
    # OUTDIR and USCEN are in arg_list if len(arg_list) >= 3
    # but we don't need to extract them now. They'll be extracted
    # in functions "choose outdir" and "choose_scedes"
    #if len(arg_list) >= 3:
    #    OUTDIR = arg_list[1]
    #    USCEN = arg_list[2]
    
    jobtype = "local"  # TODO: why not "queue" by default?
    only_run_folder_checked = "0"  # default for the "only run folder" checkbox is "un-checked"
    if len(arg_list) == 4:
        jobtype = arg_list[3][:-1]
        only_run_folder_checked = arg_list[3][-1]
    # done with "process arguments" block
    # ----------

    print_header()
    print_instructions()

    my_vars = ['USER', 'NEMS', 'NEMSJOBLOG', 'OSTYPE', 'PREPRO']
    bashvars = {k: os.getenv(k, "").replace('/', '\\') for k in my_vars}
    
    global USER, NEMS_PATH, NEMS_SCPT_PATH  # TODO: better to avoid global ?
    USER = bashvars.get('USER')
    NEMS_PATH = bashvars.get('NEMS')
    NEMS_SCPT_PATH = os.path.join(NEMS_PATH, 'scripts')
    NEMSJOBLOG =  "Y:\RabbitMQ"


    check_current_directory(wpwd, NEMS_SCPT_PATH)
    
    # prepare actualfile and shellfile list for keys.sed later
    actualfile, shellfile = make_actualfile_list(PREPRO, nemsh)

    # select user scedes file
    USCEN = choose_scedes(arg_list)

    # calculate datecode; it will be the unique run identifier for this scenario
    # is_centralized_log = True: use centralized NEMSJOBLOG and runlog files
    # is_centralized_log = False: use HHMMSS as part of datecode
    is_centralized_log = True
    if is_centralized_log:
        DATE, verfile = set_datecode(NEMSJOBLOG, USCEN)
        OUTDIR = choose_outdir(arg_list, USCEN, DATE)
        make_runlog_entry(verfile, USCEN, DATE, OUTDIR, NEMSJOBLOG)
    else:
        DATE = set_datecode_hhmmss(USCEN)
        OUTDIR = choose_outdir(arg_list, USCEN, DATE)
    
    print(f"{log_prefix}")
    
    # copy shell files content over to actual files
    #print(shellfile)
    for i in range(0,len(shellfile)):
        if os.path.exists(shellfile[i]):
            shutil.copy(shellfile[i], actualfile[i])

    # Invoke string substitutions in shell files
    # The order of the substitutions establishes the precedence: 
    # first value is used
    # ???    3a) Set output directory first
    make_keys_sed([OUTDIR,USCEN,DATE], mode)
    uscedes = search_scedes_file(wpwd, USCEN, actualfile)

    # ------------------------------------------------------------------
    # Make the pyd files. A value of 0 or 1 or 2 is required for 
    # "Full Build" or "Partial Build" process.
    # Please provide 0 (fully build pyf and pyd) or 1 (partial build) 
    # or 2 (run meson build process) 
    # go up one folder from "L:/main/naa/git/NEMS/scedes", the location 
    # from which NEMS runs, to the source folder. 
    # assign source to work_dir to be used as the argument in the 
    # make_pyd_files() and copy_pyd_to_pyfiler()
    work_dir = os.path.join("..", "source")
    make_pyd_flag, pyver = get_init_configs()

    if PREPRO == 'yes':
        cur_workdir = os.getcwd()
        os.chdir('../source')
        print("[SETUP PREPRO] building .obj files")
        subprocess.run("buildprepro.bat", shell = True)
        print("[SETUP PREPRO] build .obj files complete")
        os.chdir(cur_workdir)
    else:
        make_pyd_files(make_pyd_flag, pyver, work_dir,uscedes)

        # check for existence of pyfiler1 and pyfiler2 pyd files
        # exit if they don't exist
        temp = os.listdir(os.path.join(work_dir, "builddir"))
        check_pyd_1 = (len(fnmatch.filter(temp, "pyfiler1.*.pyd")) == 1)
        check_pyd_2 = (len(fnmatch.filter(temp, "pyfiler2.*.pyd")) == 1)
        if not (check_pyd_1 and check_pyd_2):
            print("[SETUP ERROR]: Error building pyd files. Exiting Setup now.")
            os.sys.exit()
           
        # Copy pyd files from the builddir folder to the PyFiler folder
        copy_pyd_to_pyfiler(work_dir)

        # export the following for nems_setup.py to use
        prepare_setup_py([USCEN,DATE,OUTDIR,uscedes,arg_list[0],jobtype, only_run_folder_checked])
    # ----------
    
    #if arg_list[0] == 'pre':
    if PREPRO == "yes":
        # EMM preprocesser mode
        prepro_run(mode, USCEN, DATE, OUTDIR, uscedes)
    else:
        # jognems and parnems mode
        scpt = convert_to_py_path(pyver + '/scripts/activate&python ')
        scpt += os.path.join(NEMS_SCPT_PATH, 'setup', 'src', 'nems_setup.py')
        scpt = 'cmd.exe /c "' + scpt + '"'
        subprocess.run(scpt, shell=True)

def get_init_configs():
    """Read in $NEMS\scripts\setup\input\init_config.csv and return make_pyd_flag and Python verion path string.

    Args:
        NEMS_PATH (_str_): the directory path of NEMS source code folder.
    
    Returns:
        make_pyd_flag (_int_): the pyd make flag
        pyver (_str_): the Python verion path string
    """
    file = os.path.join(NEMS_PATH,'scripts','setup','input','init_configs.csv')
    make_pyd_flag = 2
    pyver = ''

    with open(file,newline='') as csvfile:
        rd = csv.DictReader(csvfile)
        for line in rd:
            match line['key']:
                case "pyver": pyver = line['value']
                case "make_pyd_flag": make_pyd_flag = int(line['value'].strip())
    
    return make_pyd_flag, pyver

def make_actualfile_list(PREPRO, nemsh):
    """
    Make the actualfile list. Prepare for keys.sed copy filemgr, jcl, nems.sh etc. file based on jog/par or pre mode.

    Parameters
    ----------
    PREPRO : string
        yes/no flag to indicate if it's EMM preprocessor mode
    nemsh : string
        filename; e.g. "nemsone.shell"

    Returns
    -------
    actualfile : list
        list of actual files filemgr, jcl, moreopt etc. to apply for keys.sed
    shellfile : list
        list of shell files filemgr, jcl, moreopt etc. in $NEMS\scripts directory to use as a source for keys.sed
    """
    # introduce a new variable emmprepro to identify normal NEMS run or EMM preprocessor switch
    emmprepro = ''

    # if PREPRO = yes, interact with the user to set to prepplt or preppet
    if PREPRO == 'yes':
        # interact with user to choose prepplt or prepett to process
        emmprepro = interact_prepro()
    
    # assemble shellfile and actualfile lists
    shellfile = []
    actualfile = []
    shell_list = ['filemgr.shell', 'moreopt.shell', nemsh, emmprepro+'.shell']
    actual_list = ['FILELIST','MOREOPT','nems.sh']

    # create shellfile and makefile lists up to regular NEMS run or EMM preprocessor
    # go execute parnems or jognems if PREPRO is empty or 'no'. Otherwise, run EMM preprocessor:
    if PREPRO == 'no':
        for i in range(0,3):
            shellfile.append(os.path.join(NEMS_SCPT_PATH,shell_list[i]))
            actualfile.append(actual_list[i])
    else:
        print(f'{log_prefix}This will create the NEMS script (nems.sh) from {os.path.join(NEMS_SCPT_PATH,shell_list[-1])}')
        # assign actualfile=["$NEMS/scripts/filemgr.shell","$NEMS/scripts/$prepro.shell"], and actualfile=["FILELIST","nems.sh"]
        shellfile.append(os.path.join(NEMS_SCPT_PATH,shell_list[0]))
        shellfile.append(os.path.join(NEMS_SCPT_PATH,shell_list[-1]))
        actualfile.append(actual_list[0])
        actualfile.append(actual_list[-1])
    
    return actualfile, shellfile


def prepro_run(mode, USCEN, DATE, OUTDIR, uscedes):
    """
    Generate a prepro nems.sh with shell command to user scenario, output directory et. info,
    and log info in a launch.from text file. Execute the nems.sh and save the excution result in nems.out.sh

    Parameters
    ----------
    mode : string
        the user chosen mode of the run. The value is either par (parnems), jog (jognems), or pre (EMM preprocessor).
    USCEN : string
        the user scenario. eg. ref2024
    DATE : string
        datekey string. eg. d041923a
    OUTDIR : string
        the output directory path.
    uscedes : string
        the chosen scedes file path

    Returns
    -------
    None
    """
    file = f"nems.sh.{USCEN}.{DATE}"
    file_out = file.replace('nems.sh', 'nems.out.sh')
    uscedes_text = uscedes.replace('\\', '/')
    with open(file, 'a', encoding='utf-8') as f:
        f.write(f'echo This run was set up using EMM prepro mode >> launched.from\n')
        f.write(f'echo The common scedes file or run used to set up this run: >> launched.from\n')
        f.write(f'echo The user scedes file to set up this run: {uscedes_text} >> launched.from\n')
        f.write(f'echo OUTDIR={OUTDIR}/{USCEN}/{DATE} >> launched.from\n')
    
    scpt=f'sh {file} | tee {file_out}'
    subprocess.run(scpt, shell=True)
    print(f'{log_prefix}')
    print(f'{log_prefix}')
    print(f'{log_prefix}A copy of the messages above was saved in {file_out}')


def prepare_setup_py(ls):
    """
    Export the nems_setup.py required info to environment variables.

    Parameters
    ----------
    ls : list
        setting info
        example: ['nemsnew_j', 
                  'd041423b', 
                  'Q:/output/rcs', 
                  'L:\\main\\rcs\\temp_testing\\scedes.nemsnew_j']

    Returns
    -------
    None
    """
    # for debug: ls=['nemsnew_j', 'd041423b', 'Q:/output/rcs', 'L:\\main\\rcs\\temp_testing\\scedes.nemsnew_j']
    os.environ['scenario'] = ls[0]
    os.environ['datekey'] = ls[1]
    os.environ['OUTDIR'] = ls[2]
    os.environ['launchedUSCEN'] = ls[3]
   
    if ls[4]=='jog':
        os.environ['PARNEMS'] = ""
    else:
        os.environ['PARNEMS'] = "par"

    os.environ['JOBTYPE'] = ls[5]

    # 1: "only_run_folder_checked"; 0: otherwise
    os.environ['CHKRUNDIR'] = ls[6]  

    # need to explicitly assign copyem and COPYDIR here since the info
    # is needed before keys.sed file generated in the output directory
    os.environ['copyem'] = "1" # copyem=?COPYINP@
    os.environ['COPYDIR'] = ".\input"


def make_keys_sed(ls, mode):
    """
    Write a key.sed file with OUTDIR, scenario, datekey inno. 
    Also, if it is a parnems run, write EXBUILD=0 and CUTITR=0 

    Parameters
    ----------
    ls : list
        the content list of the info to write to the keys.sed file.
    mode : string
        the user chosen mode of the run. The value is either par (parnems), jog (jognems), or pre (EMM preprocessor).

    Returns
    -------
    None
    """
    with open('keys.sed', 'w+') as f:
        f.write(f'OUTDIR={ls[0]}\n')
        f.write(f'SCEN={ls[1]}\n')
        f.write(f'DATE={ls[2]}\n')
        if mode != 'par':
            f.write('EXBUILD=0\n')
            f.write('CUTITR=0\n')


# TODO: use more descriptive function name rather than "search_scedes_file"
def search_scedes_file(wpwd,USCEN,actualfile):
    """
    The major function populates key.sed. It performs 4 tasks: 
    Search for the user provided scedes file. Exit the program if scedes not found.
    Run scenawak.exe and display the warnings if any.
    Then run keyssed.exe to generate the rest content of the keys.sed file.

    Parameters
    ----------
    wpwd : string
        the working directory
    USCEN : string
        the user scenario. eg. ref2024
    actualfile : list
        the list of actual files keyssed.exe shall apply

    Returns
    -------
    uscedes : string
        the user scenario which is going to use in a NEMS run
    """
    uscedes = os.path.join(wpwd,'scedes.'+USCEN)
    
    if os.path.exists(uscedes) and not os.path.isdir(uscedes):
        print(f'{log_prefix}The user scenario descriptor file, {uscedes}, will be used.')
        try:
            os.remove('warn')
        except OSError:
            pass        
        
        # run scenawak.exe
        scpt = os.path.join(NEMS_SCPT_PATH,'scenawk.exe') + ' ' + uscedes + ' >>keys.sed'
        subprocess.run(scpt, shell=True)
        
        if os.path.exists('warn'):
            with open('warn','r') as f:
                for line in f:
                    print(f'{log_prefix}{line.strip()}')
            try:
                os.remove('warn')
            except OSError:
                pass                     
    else:
        print(f'The optional user scenario descriptor file, {uscedes}, not found.')
        os.sys.exit()
    
    # apply string substitutions to the shell files to create the actual files
    scpt = os.path.join(NEMS_SCPT_PATH,'keyssed.exe')+' keys.sed '
    for i in range(0, len(actualfile)):
        subprocess.run(scpt+actualfile[i], shell=True)

    return uscedes


def make_runlog_entry(verfile, USCEN, DATE, OUTDIR, NEMSJOBLOG):
    """
    Write user name and output folder to the runlog file. 
    Example: nems_auto t:/output/aeo2024/ref2024/d111323a

    Parameters
    ----------
    verfile : string
        job queue version filename
    USCEN : string
        scenario. eg. ref2024
    DATE : string
        datekey string. eg. d041923a
    OUTDIR : string
        output directory
    NEMSJOBLOG: string
        job log directory

    Returns
    -------
    None
    """
    # append or new version file with the user, scedes, date, outdir info
    f = convert_to_py_path(verfile)
    s = convert_to_py_path(f'{USER} {OUTDIR}/{USCEN}/{DATE}\n')
    with open(f, 'a', encoding='utf-8') as file:
        file.write(s)
    # for debug:
    #m:\\NEMSJobLog\\@.verscomp.nemsnew_j.d041123
    #rcs Q:/output/rcs/nemsnew_j/d041123a
    
    # substitute UNC file name if outdir is a shared directory on current PC
    s = convert_to_py_path(f'{OUTDIR}/{USCEN}/{DATE}')
    s = s.replace('\\', '/')
    f = os.path.join(NEMS_SCPT_PATH, 'setup', 'input', 'submit_unc_lookup.csv')
    s = convert_unc_path(s,f)
    logentry=f'\n{USER} {s}'

    # write to the runlog file
    f = convert_to_py_path(NEMSJOBLOG + '/runlog')
    with open(f, 'a', encoding='utf-8') as file:
        file.write(logentry)

def convert_unc_path(output_path, unc_lookup_file):
    """Examinate the output path and replace the drive name with the corresponding UNC path listed in the configuration file setup/input/submit_unc_lookup.csv.

    Parameters
    ----------
    output_path : _str_
        the output path before UNC lookup replacement.
    unc_lookup_file : _str_
        the UNC configuration file

    Returns
    -------
    _str_
        The replaced UNC path string.
    """
    # the first character is the drive info
    s = output_path

    # Read the CSV file into a DataFrame
    df = pd.read_csv(unc_lookup_file)

    # Locate the row where 'Drive' is
    row = df[df['Drive'] == s[0].lower()]

    result = ''
    if not row.empty:
        # locate the first matched row UNC value and assemble with the SCEN and DATE info
        result = row['UNC'].values[0] + s[2:]
    else:
        # the output is in the local drive d: or any drive not listed in the submit_unc_lookup.csv file
        result = s
    return result

def choose_outdir(arg_list, USCEN, DATE):
    """
    Determine output directory. If not specified in the argument list,
    then ask the user to choose from a list.

    Parameters
    ----------
    arg_list : list
        the argument string list passed in when invoke this nems_submit.py
    USCEN : string
        the user scenario. eg. ref2024
    DATE : string
        datekey string. eg. d041923a

    Returns
    -------
    string
        the chosen output directory path
    """
    OUTDIR = ''
    if len(arg_list) >= 3:
        return arg_list[2]
    
    outchoice = 0
    OUTDIR = '/dodah'
    print(f'{log_prefix}\n{log_prefix}Available space on some NEMS drives')
    outmenu_file = os.path.join(NEMS_SCPT_PATH,'setup','input','submit_outmenu.csv')
    net_drive = NetworkDrive()
    suggested, all_drive_dict, num_lastmenu, drive_catg_custom, drive_catg_desired, drive_catg_testing = net_drive.get_catg_drive_list(outmenu_file,USER)
    display = net_drive.convert_drive_info_to_display(all_drive_dict, log_prefix, False)
    print(display)

    if OUTDIR != '/dodah': 
        print(f'{log_prefix}\n{log_prefix}    Directory {OUTDIR} does not exist.  Re-enter.')
    OUTDIR=f'{suggested}/output/{USER}'
    OUTDIR = display_outmenu_choices(suggested, num_lastmenu, outchoice, drive_catg_custom, drive_catg_desired, USCEN, DATE)
    return OUTDIR


def display_outmenu_choices(suggested, num_lastmenu, outchoice, drive_catg_custom, drive_catg_desired, USCEN, DATE):
    """
    Display the drive space and a suggested output location.

    Parameters
    ----------
    suggested : string
        suggested output directory path
    num_lastmenu : integer
        row count of the submit_outmenu.csv content
    outchoice : string
        user chosen output option number
    drive_catg_custom : list
        drive list in the custom categrory
    drive_catg_desired : list
        drive list in the important and testing categrories
    USCEN : string
        user scenario. eg. ref2024
    DATE : string
        datekey string. eg. d041923a

    Returns
    -------
    OUTDIR : string
        the output directory path
    """
    ls = drive_catg_custom + drive_catg_desired
    # replace 'zzz' string with the suggested drive name
    (ls[0])['output'] = (ls[0].get('output')).replace('zzz', suggested)

    print(f'{log_prefix}')
    print(f'{log_prefix}     Select an option for your output directory parent location')
    print(f'{log_prefix}')
    for i in range(0,num_lastmenu):
        s1 = (str(i+1)+')').rjust(3,' ')
        s2 = (ls[i].get("output")).ljust(18,' ')
        print(f'{log_prefix}{s1} {s2} {ls[i].get("note")}')
    outchoice = input(f'{log_prefix}         Enter option number [1] : ').strip()
    if outchoice == '': outchoice = '1'
    
    # default output drive to the suggested drive. Set the output drive based on the user choice.
    OUTDIR=f'{suggested}/output/{USER}'
    if outchoice == '2':
        outdir1=input(f'{log_prefix}           Enter parent directory in which /{USCEN}/{DATE}/ will reside [{OUTDIR}] :').strip()
        if outdir1 != '': OUTDIR = outdir1
    elif outchoice != '1':
        OUTDIR=ls[int(outchoice)-1].get('output')
        print(f'{log_prefix}you selected {int(outchoice)}) {OUTDIR}')
    print(f'{log_prefix}     This run will be stored in {OUTDIR}/{USCEN}/{DATE}')
    
    # now create the OUTDIR directory if it does not exist
    d=convert_to_py_path(OUTDIR)
    if not os.path.exists(d):
        os.mkdir(d)
    
    return OUTDIR


def set_datecode_hhmmss(USCEN):
    """
    Calculate currrent mmddyy.HHMMSS as datekey.

    Parameters
    ----------
    NEMSJOBLOG : string
        the job queue directory path
    USCEN : string
        the user scenario. eg. ref2024

    Returns
    -------
    DATE : string
        datekey string. eg. 041923.164627
    """
    DATE = datetime.now().strftime('%m%d%y.%H%M%S')
    print(f'{log_prefix}')
    print(f'{log_prefix}     The unique name for this run will be {USCEN}/{DATE}')
    return DATE


def set_datecode(NEMSJOBLOG, USCEN):
    """
    Calculate the current mmddyy and version character.

    Parameters
    ----------
    NEMSJOBLOG : string
        the job queue directory path.
    USCEN : string
        the user scenario. eg. ref2024

    Returns
    -------
    DATE : string
        datekey string. eg. d041923a
    verfile : string
        jOb queue version filename
    """
    #print(f'{log_prefix}'+set_datecode()', NEMSJOBLOG)
    vers = 'a'
    curr_date = datetime.now().strftime('%m%d%y')
    DATE = 'd' + curr_date
    
    verfile = f"{NEMSJOBLOG}\@.verscomp.{USCEN}.{DATE}"
    # for debug: verfile=r'M:\NEMSJobLog\@.verscomp.ref2023_solo.d040623'
    
    # check and read the verfile. Calculate and update the next version char
    if os.path.exists(verfile):
        num_lines = sum(1 for line in open(verfile))
        vers = get_version_char(num_lines+1)
    
    DATE += vers
    print(f'{log_prefix}')
    print(f'{log_prefix}     The unique name for this run will be {USCEN}/{DATE}')
    return DATE, verfile


def get_version_char(num):
    """
    Find the alphabetic character(s) corresponding to arg 'num'.

    Parameters
    ----------
    num : integer
        version numer

    Returns
    -------
    string
        version character(s). example: a, bh
    """
    file = os.path.join(NEMS_SCPT_PATH, 'version.seq')
    with open(file, 'r', encoding='utf-8') as f:
        lines = f.readlines()
    lookup = {s[0]: s[1].strip() for s in [i.split(" ") for i in lines]}
    # example "lookup" dict: {"1": "a", "2": "b", ...}

    # return the mapped char. return HAHAHA if no corresponding key value pair found
    return lookup.get(str(num), 'HAHAHA')


def choose_scedes(arg_list):
    """
    Determine scedes name. If not specified as an argument, then ask 
    the user to select from files in the "scedes" directory.

    Parameters
    ----------
    arg_list : list

    Returns
    -------
    USCEN : string
        the user scenario. eg., ref2024
    """
    #if len(arg_list) >= 3:
    if len(arg_list) >= 2:
        return arg_list[1]

    scedes_list = [i for i in os.listdir() if \
                   i.startswith("scedes") and not i.startswith("scedes.all.")]
    scedes_list.sort(key=os.path.getmtime, reverse=True)
    
    if len(scedes_list) > 0:
        # grab the most recent scedes file as the default. Only take the file extension.
        # for example, scedes.ref2023 yields USCEN='ref2023'
        USCEN = scedes_list[0].split('.')[1]
    else:
        msg = "No scedes file found. Please launch from the scedes folder. Exiting NEMS."
        print(f"{log_prefix}{msg}")
        os.sys.exit()
    
    uscen = input(f'{log_prefix}    Enter User Scenario [{USCEN}] :')
    if uscen != "": 
        USCEN = uscen
    
    return USCEN


def print_header():
    """
    Print the NEMS run header to the screen.

    Returns
    -------
    None
    """
    print('\n\n\n\n\n\n\n')
    print(f'{log_prefix}=============================================================')
    print(f'{log_prefix}                           NEMS RUN')
    print(f'{log_prefix}=============================================================')


def print_instructions():
    """
    Print NEMS run instructions to the screen.

    Returns
    -------
    None
    """
    print(f'{log_prefix}Press ENTER to accept the default answers shown in []')
    print(f'{log_prefix}Use command line arguments as follows:')
    print(f'{log_prefix}')
    print(f'{log_prefix}runnems [user-scedes [output-directory] ]')
    print(f'{log_prefix}')
    print(f'{log_prefix}')
    print(f'{log_prefix}   user-scedes is the name of your scenario descriptor file')
    print(f'{log_prefix}      if it is found in {os.getcwd()}.')
    print(f'{log_prefix}      It is also the scenario name used to identify this run')
    print(f'{log_prefix}')
    #print(f'{log_prefix} Command line examples:')
    #print(f'{log_prefix}   jognems myrun u:/output/dsa/aimefd')
    print(f'{log_prefix}=============================================================')


def interact_prepro():
    """
    Ask user input: either 1 for prepplt or 2 for prepett.

    Returns
    -------
    prepro_choice : string
        user choice (prepplt or prepett)
    """
    print(f'{log_prefix}')
    print(f'{log_prefix}   The PREPRO option is set.  Select the program:')
    print(f'{log_prefix}   1) prepplt   :  The plntdaf preprocessor')
    print(f'{log_prefix}   2) prepett   :  The ettdem  preprocessor')
    n=int(input('   Enter the program number: '))
    prepro_choice = ''
    if n == 1:
        prepro_choice = 'prepplt'
    elif n == 2:
        prepro_choice = 'prepett'
    else:
        print(f'{log_prefix}You must choose either 1 or 2 only. Exiting now...')
        os.sys.exit()

    return prepro_choice


def choose_mode_info():
    """
    Ask user for the mode: jog (jognems), par (parnems), or pre (EMM preprocessor)

    Returns
    -------
    mode : string
        'jog', 'par', or 'pre'
    """
    print(f'{log_prefix}Choose the process mode:')
    print(f'{log_prefix} 1) jognems')
    print(f'{log_prefix} 2) parnems')
    print(f'{log_prefix} 3) prepro')
    
    # ask user to choose the mode
    # pressing ENTER without a choice will select the first option
    mode_choice = input(f'{log_prefix} Enter option number [1] : ').strip()
    if (mode_choice == "1") or (mode_choice == ""): 
        mode = "jog"
    elif mode_choice  == "2":
        mode = "par"
    elif mode_choice  == "3":
        mode = "pre"

    return mode


def check_current_directory(curr_dir, scpt_dir):
    """
    Make sure that the current working directory is not $NEMS\scripts. 
    Otherwise, print message and exit the program.

    Parameters
    ----------
    curr_dir : string
        the current working directory path.
    scpt_dir : string
        the $NEMS\scripts directory path.

    Returns
    -------
    None
    """
    if curr_dir == scpt_dir:
        print(f'{log_prefix}     Sorry.  There is a slight problem.')
        print(f'{log_prefix}     Your current directory is {curr_dir}.')
        print(f'{log_prefix}     Please execute this from another directory.')
        os.sys.exit(1)


def convert_to_py_path(src):
    """
    Convert shell script forward slash (/) to Python backslash (\) format. 
    The Job queue runlog still uses /.

    Parameters
    ----------
    src : string
        the path in Shell forward slash (/) format

    Returns
    -------
    src : string
        the path in Python backslash (\) format
    """
    return src.replace('/', '\\')


if __name__ == '__main__':
    # Python passes the file name as the first argument. Since 
    # we do not need that info, remove it from the arument list.
    arg_list = sys.argv[1:]
    
    # if a mode is not sepecified, ask user to choose a mode
    if len(arg_list) == 0:
        arg_list.append(choose_mode_info())
    
    main(arg_list)
