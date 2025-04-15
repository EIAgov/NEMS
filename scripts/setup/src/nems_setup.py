# -*- coding: utf-8 -*-
"""
Created on Feb 24 2023

@author: Claire Su
"""

import os, shutil, subprocess, re, zipfile
import time
import socket

from pathlib import Path
from datetime import datetime

from nemsbase import NEMSBase
from nemsutil import util_debugging_dump_dict
from cel import run_task

def copy_filelist_etc(nemsbase):
    '''
    Copy 4 essential files ('keys.sed', 'FILELIST', 'MOREOPT', 'scentext') under the OUTDIR folder.

    Parameters
    ----------
    nemsbase : NEMSBase object
        the main NEMS class.

    Returns
    -------
    None.

    '''
    # no scentext.$scenario.$datekey exists in the output dir even produced from 2022/12 nemspar.shell. We may remove the file copy of scentext.
    ls = ('keys.sed', 'FILELIST', 'MOREOPT', 'scentext')
    for i in ls:
        src=f'{i}.{nemsbase.dt_scenario}'
        src = os.path.join(nemsbase.OLDDIR, src)
        
        #if (not os.path.exists(src)):
        if (os.path.exists(src)):
            shutil.copy(src, i)

def fix_filelist_path_bug(nemsbase, file):
    '''
    Fix directory path bug ("./input/psrain.txt" become ".\input/psrain.txt" due to Python instead of Shell environment) in FILELIST file.

    Parameters
    ----------
    nemsbase : NEMSBase object
        the main NEMS class.
    file : string
        filename with the path.

    Returns
    -------
    None.

    '''
    nemsbase.util_convert_py_path_to_ksh_path(file)

def build_coal_structure(nemsbase, p='p2'):
    '''
    Build AIMMS folder and file structure of coal model.

    Parameters
    ----------
    nemsbase : NEMSBase object
        the main NEMS class.
    p : string, optional
        the partition folder (p1, p2, or p3). The default is 'p2'. Jognems behaves same as p2.

    Returns
    -------
    None.

    '''
    # put coal units file in p2 for coal model at beginning of run so it will be there for project open/read
    f=nemsbase.scedvars.get('COALUNITSN')+nemsbase.scedvars.get('COALUNITSD')
    shutil.copy(f,'COALEMM_EMM_CL_UNITS.txt')

    access = os.path.join(nemsbase.dir_scripts, 'access.dsn')
    print(f'{nemsbase.log_prefix}'+f"AIMMSBIT option to use 64-bit odbc template: {nemsbase.AIMMSBIT}")
    if (nemsbase.AIMMSBIT == '1' and nemsbase.AIMMSVER == '4'): access = os.path.join(nemsbase.dir_scripts, 'access64.dsn')
    print(f'{nemsbase.log_prefix}'+f'aimms odbc template file: {access}')
    
    if (p == 'p2' or nemsbase.mode == 'jog'):
        print(f'{nemsbase.log_prefix}coal directory for coal AIMMS project')
    
        # We touch the .ams file after unzipping to update the time stamp to prevent possible errors from cygwin
        shutil.copytree(nemsbase.scedvars.get('COALN'), 'coal')
        Path('.\\coal\\mainproject\\coal.ams').touch()
        shutil.copy(access, 'coal\\coal_out.mdb.dsn')
        nemsbase.util_append_content('coal\\coal_out.mdb.dsn', 'DBQ=coal_out.mdb')
        
        # The following line sets up diagnostic messaging for the AIMMS SDK "aimmslink" process. for NGMM and COAL
        nemsbase.util_copy_extension(nemsbase.dir_scripts, os.getcwd(), '.logger.xml')
        #nemsbase.util_copy_pattern_files(nemsbase.dir_scripts, os.getcwd(), '.logger.xml')

    # these 3 small AIMMS database description files are sometimes needed after the run, so put them in all input folders, not just p2,
    # since only one copy of the input folder (p3) is saved after the run (when invoked via runit.exe)
    ls = ('cmm.mdb.dsn', 'cmm2.mdb.dsn', 'cps.mdb.dsn')
    for e in ls: shutil.copy(access, os.path.join(nemsbase.COPYDIR, e))

    # add extra "." ('DBQ=.') so it looks for the input (copydir) in the parent folder 
    # Basically, for AIMMS version 4 (AIMMVER), we want to look for the parent dir of the current dir.
    for e in ls:
        f = os.path.join(nemsbase.COPYDIR, e)
        nemsbase.util_append_content(f,'DBQ=.'+f)

def build_efd_ecp_rest_structure(nemsbase, p='p2'):
    '''
    Build AIMMS folder and file structures of EFD, ECP, and REST models.

    Parameters
    ----------
    nemsbase : NEMSBase object
        the main NEMS class.
    p : string, optional
        the partition folder (p1, p2, or p3). The default is 'p2'. Jognems behaves same as p2.

    Returns
    -------
    None.

    '''
    if (p == 'p2' or nemsbase.mode == 'jog'):
        # only add these AIMMS EFD files for the p2 run
        if (nemsbase.scedvars.get('AIMMSEFD') == '1'):
            print(f'{nemsbase.log_prefix}efd directory for efd AIMMS project')
            shutil.copytree(nemsbase.scedvars.get('EFDN'), '.\\efd')
            Path('.\\efd\\mainproject\\efd.ams').touch()

        if (nemsbase.scedvars.get('AIMMSECP') == '1'):
            print(f'{nemsbase.log_prefix}ecp directory for ecp AIMMS project')
            shutil.copytree(nemsbase.scedvars.get('ECPN'), '.\\ecp')
            if (nemsbase.scedvars.get('AIMECPPAR') == '0'):
                Path('.\\ecp\\mainproject\\ecp.ams').touch()
            else:
                # Path('.\\ecp\\ecp_ge\\mainproject\\ecp_ge.ams').touch()
                # Path('.\\ecp\\ecp_gs\\mainproject\\ecp_gs.ams').touch()
                # Path('.\\ecp\\ecp_gw\\mainproject\\ecp_gw.ams').touch()
                Path('.\\ecp\\ecp_ge\\mainproject\\ecp.ams').touch()
                Path('.\\ecp\\ecp_gsw\\mainproject\\ecp.ams').touch()
                #Path('.\\ecp\\ecp_gw\\mainproject\\ecp.ams').touch()

        if (nemsbase.scedvars.get('RUNSTORE') == '1'):
            print(f'{nemsbase.log_prefix}rest directory for wind, solar, and electricity storage project')
            shutil.copytree(nemsbase.scedvars.get('RESTOREN'), '.\\rest')
            Path('.\\rest\\mainproject\\EMM_renewable.ams').touch()

        if (nemsbase.scedvars.get('RUNEMMSQL') == '1'):
            print(f'{nemsbase.log_prefix}'+'emm_db directory for emm_db AIMMS project')
            shutil.copytree(nemsbase.scedvars.get('EMM_DBN'), '.\\emm_db')
def build_ephrts_structure(nemsbase, p='p2'):
    '''
    Build AIMMS folder and file structures of HMM (EPHRTS) model.

    Parameters
    ----------
    nemsbase : NEMSBase object
        the main NEMS class.
    p : string, optional
        the partition folder (p1, p2, or p3). The default is 'p2'. Jognems behaves same as p2.

    Returns
    -------
    None.

    '''
    # reserve the following for Ed EPHRTS project (nemspar.shell version cut-off date is 08/03/2022)
    # is H2 under all directory or only p2? temp add a directory check 'if (p == 'p2'):' -Cliare
    if (p == 'p2' or nemsbase.mode == 'jog'):
        if (nemsbase.scedvars.get('EPHRTS') == '1'):
            print(f'{nemsbase.log_prefix}'+'electricity hydrogen submodule (EPHRTS)')
            shutil.copytree(nemsbase.scedvars.get('HYDROGENN'), '.\\ephrts')
            Path('.\\ephrts\\mainproject\\ephrts.ams').touch()

def build_ngas_structure(nemsbase, p='p1'):
    '''
    Build AIMMS folder and file structures of NGMM model.

    Parameters
    ----------
    nemsbase : NEMSBase object
        the main NEMS class.
    p : string, optional
        the partition folder (p1, p2, or p3). The default is 'p2'. Jognems behaves same as p1.

    Returns
    -------
    None.

    '''
    if (p == 'p1' or nemsbase.mode == 'jog'):
        if (nemsbase.scedvars.get('AIMMSNG') == '1'):
            print(f'{nemsbase.log_prefix}'+'ngas directory for ngas AIMMS project')
            shutil.copytree(nemsbase.scedvars.get('NGAIMMSN'), '.\\ngas')
            Path('.\\ngas\\mainproject\\natgas.ams').touch()

def build_hsm_structure(nemsbase, p='p1'):
    '''
    Build Python folder and file structures of HSM model.

    Parameters
    ----------
    nemsbase : NEMSBase object
        the main NEMS class.
    p : string, optional
        the partition folder (p1, p2, or p3). The default is 'p2'. Jognems behaves same as p1.

    Returns
    -------
    None.

    '''
    if (p == 'p1' or nemsbase.mode == 'jog'):
        print(f'{nemsbase.log_prefix}'+'hsm directory for hsm python project')
        print(nemsbase.scedvars.get('HSMPYPTH'))
        shutil.copytree(nemsbase.scedvars.get('HSMPYPTH'), '.\\hsm')
        Path('.\\hsm\\hsm.py').touch()
        
def build_ngpl_structure(nemsbase, p='p1'):
    '''
    Build Python folder and file structures of NGPL model.

    Parameters
    ----------
    nemsbase : NEMSBase object
        the main NEMS class.
    p : string, optional
        the partition folder (p1, p2, or p3). The default is 'p1'. Jognems behaves same as p1.

    Returns
    -------
    None.

    '''
    print(f'{nemsbase.log_prefix}'+'ngpl directory for ngpl python project')
    print(nemsbase.scedvars.get('NGPLPRICEN'))
    shutil.copytree(nemsbase.scedvars.get('NGPLPRICEN'), '.\\ngpl')
    Path('.\\ngpl\\ngplprice.py').touch()

def build_reporter_structure(nemsbase, p='p1'):
    '''
    Build Python folder and file structures of NEMS Report Writer model.

    Parameters
    ----------
    nemsbase : NEMSBase object
        the main NEMS class.
    p : string, optional
        the partition folder (p1, p2, or p3). The default is 'p1'. Jognems behaves same as p1.

    Returns
    -------
    None.

    '''
    print(f'{nemsbase.log_prefix}'+'reporter directory for reporter python project')
    print(nemsbase.scedvars.get('REPORTRN'))
    shutil.copytree(nemsbase.scedvars.get('REPORTRN'), '.\\reporter')
    Path('.\\reporter\\reporter.py').touch()
    
def build_hmm_structure(nemsbase, p='p2'):
    '''
    Build AIMMS folder and file structures of HMM model.

    Parameters
    ----------
    nemsbase : NEMSBase object
        the main NEMS class.
    p : string, optional
        the partition folder (p1, p2, or p3). The default is 'p2'. Jognems behaves same as p1.

    Returns
    -------
    None.

    '''
    if (p == 'p2' or nemsbase.mode == 'jog'):
      print(f'{nemsbase.log_prefix}'+'hmm directory for hmm AIMMS project')
      shutil.copytree(nemsbase.scedvars.get('H2AIMMSN'), '.\\hmm')
      Path('.\\hmm\\mainproject\\hmm.ams').touch()

def build_ccats_structure(nemsbase, p='p1'):
    '''
    Build Python folder and file structures of CCATS model.

    Parameters
    ----------
    nemsbase : NEMSBase object
        the main NEMS class.
    p : string, optional
        the partition folder (p1, p2, or p3). The default is 'p1'. Jognems behaves same as p1.

    Returns
    -------
    None.

    '''
    if (p == 'p1' or nemsbase.mode == 'jog'):
        print(f'{nemsbase.log_prefix}'+'ccats directory for ccats python project')
        print(nemsbase.scedvars.get('CCPYPTH'))
        shutil.copytree(nemsbase.scedvars.get('CCPYPTH'), '.\\CCATS')
        Path('.\\ccats\\ccats.py').touch()
        
def build_idm_structure(nemsbase, p='p2'):
    
    '''
    Build Python folder and file structures of IDM model. Place the IDM folder in p2
    partition because the function is only required by HMM.

    Parameters
    ----------
    nemsbase : NEMSBase object
        the main NEMS class.
    p : string, optional
        the partition folder (p1, p2, or p3). The default is 'p1'. Jognems behaves same as p1.

    Returns
    -------
    None.

    '''
    if (p == 'p2' or nemsbase.mode == 'jog'):
        print(f'{nemsbase.log_prefix}'+'IDM directory for IDM python project')
        print(nemsbase.scedvars.get('IDMPYPTH'))
        shutil.copytree(nemsbase.scedvars.get('IDMPYPTH'), '.\\idm')
        Path('.\\idm\\idm_h2_accounting.py').touch()

def build_epm_structure(nemsbase):
    '''
    Build Python folder and file structures of EPM model.

    Parameters
    ----------
    nemsbase : NEMSBase object
        the main NEMS class.
    p : string, optional
        the partition folder (p1, p2, or p3). The default is 'p1'. Jognems behaves same as p1.

    Returns
    -------
    None.

    '''
    print(f'{nemsbase.log_prefix}'+'epm directory for epm python project')
    print(nemsbase.scedvars.get('EPMPY'))
    shutil.copytree(nemsbase.scedvars.get('EPMPY'), '.\\epm')
    Path('.\\epm\\run_epm.py').touch()


def copy_none_output_hswrk(nemsbase, file, display, is_copydir=True):
    '''
    Append a copy command for a specified file and destination to the hswrk.bat file.

    Parameters
    ----------
    nemsbase : NEMSBase object
        the main NEMS class.
    file : string
        the name of the copied file printed in the hswrk.bat.
    display : string
        the destination file name printed in the hswrk.bat.
    is_copydir : boolean, optional
        a boolean to indicate if include COPYDIR in the file path . The default is True.

    Returns
    -------
    None.

    '''
    dst=file
    if is_copydir:
        dst=os.path.join(nemsbase.COPYDIR,file)
    nemsbase.util_append_content('hswrk.bat',f'copy {dst} {display}\n')

def copy_inputfile_output_hswrk(nemsbase, src, file, display, input_in_child_dir=True):
    '''
    Copy a specified file into the current directory or its child directory. Also append the copy command to the hswrk.bat file.

    Parameters
    ----------
    nemsbase : NEMSBase object
        the main NEMS class.
    src : string
        the source, copied-from filename.
    file : string
        the name of the copied file printed in the hswrk.bat.
    display : string
        the destination file name printed in the hswrk.bat.
    input_in_child_dir : boolean, optional
        a boolean to indicate the input folder location shall be in the output root or the child p1,p2,p3 folder (jog or parnems model). The default is True.

    Returns
    -------
    None.

    '''
    # for debug testing:
    #src=nemsbase.scedvars.get('PLNTDAFN')+nemsbase.scedvars.get('PLNTDAFD')
    #file='plntdaf.daf'
    dst=os.path.join(nemsbase.COPYDIR,file)
    if (not input_in_child_dir):    #for copyem == 0
        dst=file

    shutil.copy(src,dst)
    nemsbase.util_append_content('hswrk.bat',f'copy {dst} {display}\n')

def nruns_switch_emm_group(nemsbase, ls, input_in_child_dir=True):
    '''
    A EMM method, based on scedes key NRUN setting and EXE=on/off, to copy or not copy a list of EMM files to a output folder perticular layer location.
    Also output the bash commands in hswrk.bat file.

    Parameters
    ----------
    nemsbase : NEMSBase object
        the main NEMS class.
    ls : dict
        the dict defines the file scource, filename, and expected printout message of a list of files to be handled.
    input_in_child_dir : boolean, optional
        a boolean to indicate the input folder location shall be in the output root or the child p1,p2,p3 folder (jog or parnems model). The default is True.

    Returns
    -------
    None.

    '''
    if (nemsbase.BoolE):
        if nemsbase.NRUNS == 1:
            nruns_switch_group(nemsbase, ls, 'E', True, True)
        else:
            for f in ls:
                copy_none_output_hswrk(nemsbase, f['file2'], f['display'], False)

def nruns_switch_group(nemsbase, ls, logic_model='', input_in_child_dir=True, is_copy=True):
    '''
    A generic method, based on model group (EXE, EXM, EXO) and it's on/off', to copy or not copy a list of that model files to a output folder perticular layer location.
    Also output the bash commands in hswrk.bat file.

    Parameters
    ----------
    nemsbase : NEMSBase object
        the main NEMS class.
    ls : dict
        the dict defines the file scource, filename, and expected printout message of a list of files to be handled.
    logic_model : string, optional
        model switch control string. 'E' for EXE, 'M' for EXM, 'O' for EXO. The default is ''.
    input_in_child_dir : boolean, optional
        a boolean to indicate this input folder location. It shall be in the output root or the child p1,p2,p3 folder (jog or parnems model). The default is True.
    is_copy : boolean, optional
        A control to decide if copy the list of files and output the command on screen and hswrk.bat. The default is True.

    Returns
    -------
    None.

    '''
    logic_model = logic_model.upper()
    check_logic = False
    if logic_model == 'E':
        check_logic = nemsbase.BoolE
    elif logic_model == 'M':
        check_logic = nemsbase.BoolM
    elif logic_model == 'O':
        check_logic = nemsbase.BoolO
    else:
        print(f'{nemsbase.log_prefix}'+'Developer - please define in code which model group you want to check. Exit now')
        os.sys.exit()
    
    if (check_logic):
        for f in ls:
            if (is_copy):
                copy_inputfile_output_hswrk(nemsbase, f['src'], f['file'], f['display'], input_in_child_dir)
            else:
                copy_none_output_hswrk(nemsbase, f['src'], f['display'])


def cycle_restart_emm(nemsbase):
    '''
    Cycle restart files if NRUNS > 1.

    Parameters
    ----------
    nemsbase : NEMSBase object
        the main NEMS class.

    Returns
    -------
    None.

    '''
    # cycle restart file  and EMM basis files if this is a multi-cycle run
    if nemsbase.NRUNS > 1:
        shutil.copy(nemsbase.scedvars.get('RESTARTN')+nemsbase.scedvars.get('RESTARTD'),'RESTART.IN')
        if nemsbase.BoolE:
            shutil.copy(nemsbase.scedvars.get('BASEMMIN')+nemsbase.scedvars.get('BASEMMID'),'basemmi')
            shutil.copy(nemsbase.scedvars.get('BASEFDIN')+nemsbase.scedvars.get('BASEFDID'),'basefdi')

def build_root_dir(nemsbase):
    '''
    Build the content of (intercvfiles.txt, RESTART.unf) files in the output root directory, and ('iccnvrg.txt','dict.txt','varlist.txt') in the input subdirectory.

    Parameters
    ----------
    nemsbase : NEMSBase object
        the main NEMS class.

    Returns
    -------
    None.

    '''
    logfile = 'intercvfiles.txt'
    f=('iccnvrg.txt','dict.txt','varlist.txt')
    sced=(('ICCNVRGN','ICCNVRGD'),('DICTN','DICTD'),('VARLISTN','VARLISTD'))
    log_only=('FILERN','FILERD')
    s_log_only=f'           {nemsbase.scedvars.get(log_only[0])+nemsbase.scedvars.get(log_only[1])}\n'

    if nemsbase.copyem:
        # set up the input folder for ftab inputs
        nemsbase.util_create_dir(nemsbase.COPYDIR)
        '''
        I intentionally disable the followning Python print-out command, because:
        it’s a bug in nemspar.shell Line #749. it uses >> intercvfiles.txt, but actually, the I/O entry is reductant.
        in nemspar.shell: echo "           ?COPYDIR@/iccnvrg.txt" >> intercvfiles.txt
        '''
        #s=f'           {nemsbase.COPYDIR}\iccnvrg.txt'+'\n'
        #nemsbase.util_append_content(logfile,s)

        if nemsbase.NRUNS == 1:
            shutil.copy(nemsbase.scedvars.get('RESTARTN')+nemsbase.scedvars.get('RESTARTD'),os.path.join(nemsbase.COPYDIR,'restarti.unf'))
            # be aware the following s string patching f'           {nemsbase.COPYDIR}\\restarti.unf' required double slashes - need to escape \r (line break) -Claire
            s='           RESTART.unf'+'\n'+'           1'+'\n'+f'           {nemsbase.COPYDIR}\\restarti.unf'+'\n'+'           1\n'
            nemsbase.util_append_content(logfile,s)
        else:
            shutil.copy(os.path.join(nemsbase.dir_scripts,logfile),logfile)

        for i,v in enumerate(f):
            shutil.copy(nemsbase.scedvars.get(sced[i][0])+nemsbase.scedvars.get(sced[i][1]),os.path.join(nemsbase.COPYDIR,f[i]))
            nemsbase.util_append_content(logfile,f'           {nemsbase.COPYDIR}\{f[i]}\n')

    else:
        if nemsbase.NRUNS == 1:
            # only log RESTART.unf. No copy over. why?
            s='           RESTART.unf'+'\n'+'           1'+'\n'+f'           {nemsbase.scedvars.get("RESTARTN")+nemsbase.scedvars.get("RESTARTD")}'+'\n'+'           1\n'
            nemsbase.util_append_content(logfile,s)
        else:
            shutil.copy(os.path.join(nemsbase.dir_scripts,logfile),logfile)

        for i,v in enumerate(f):
            # no copy over but log out only
            nemsbase.util_append_content(logfile,f'           {nemsbase.scedvars.get(sced[i][0])+nemsbase.scedvars.get(sced[i][1])}\n')

    # same code for either copyem !=0 or ==0. No copy for ?FILERN@?FILERD@. Only echo to the log
    nemsbase.util_append_content(logfile,s_log_only)

    # replace Python path '\' with Ksh format '/' for NEMS cycle_par.sh well pick up and continue:
    nemsbase.util_convert_py_path_to_ksh_path(logfile)

def grep_word_append_moreopt_file(nemsbase,pattern,replacement):
    '''
    In moreopt file, locate the specified pattern word and replace it with the replacement.

    Parameters
    ----------
    nemsbase : NEMSBase object
        the main NEMS class.
    pattern : string
        the search match pattern. For example, 'XPRESSSW' or 'iterdump'.
    replacement : string
        the new string to replace the matched pattern word. For example, 'XPRESSSW 0' or 'ITERDUMP 0'.

    Returns
    -------
    None.

    '''
    filename='MOREOPT'
    with open(filename, encoding='utf-8') as file:
        lines = file.readlines()
        lines = [line for line in lines if re.search(pattern,line,re.IGNORECASE) is None]

    #util_append_content(self, filename, content):
    filename = 'm'
    with open(filename, 'w', encoding='utf-8') as f:
        f.writelines(lines)
        f.write(replacement+'\n')

    shutil.move('m', 'MOREOPT')


def check_xpress(nemsbase):
    '''
    Check the scedes XPRESSSW value and output the message if XPRESS is not turned on.

    Parameters
    ----------
    nemsbase : NEMSBase object
        the main NEMS class.

    Returns
    -------
    None.

    '''
    # Check for xpress on hosts that don't have it and turn it off
    if nemsbase.scedvars.get('XPRESSSW') == "0":
        print(f'{nemsbase.log_prefix}'+'   XPRESS not turned on')

def just_do_it(nemsbase, jobtype):
    '''
    Now we are ready tokick off a NEMS run. Just do it!

    Parameters
    ----------
    nemsbase : NEMSBase object
        the main NEMS class.
    jobtype : string
        either 'local_sequential', 'local_parallel', 'queue_parallel' or 'queue_sequential'

    Returns
    -------
    None.

    '''
    # for debugging: (in default scedes ref202):
    #HOLD=0, RUNCOB=0, RELEASE=0
    hold   =nemsbase.scedvars.get('HOLD')
    runcob =nemsbase.scedvars.get('RUNCOB')
    release=nemsbase.scedvars.get('RELEASE')
    delay  =''
    if (hold == '1'): delay='hold'
    if (runcob == '1'): delay='18:00'
    if (release == '1'): delay='release'
    
    if 'local' in jobtype:

        print(f'{nemsbase.log_prefix}'+f'Submitting NEMS run in class debug on this computer {nemsbase.bashvars.get("COMPUTERNAME")}')

        # temporary copy run_task.py because cycle.py imports run_task.py even in jobtype=local
        shutil.copy(os.path.join(nemsbase.dir_scripts,"setup","src", "cel", "run_task.py"), "run_task.py")
        
        scpt = os.path.join(nemsbase.path_datekey,'cycle.py')
        scpt = scpt.replace('\\', '/') + f' {nemsbase.path_datekey} {jobtype}'
        scpt=f'"{scpt}"'
        s1 = 'start /separate /min "'+f'{nemsbase.dt_scenario}'+'"'
        #the origin command is: start -l -m -t "$scenario $datekey"
        scpt1 = os.path.join(nemsbase.dir_scripts,'sys.exe')
        final_scpt=s1+" "+scpt1+' python '+scpt
        subprocess.run(final_scpt, shell=True)
        print(f'{nemsbase.log_prefix}a {jobtype} build is cooking')
            
    else:
        
        print(f'Job type: {jobtype}')
        print(nemsbase.path_datekey)

        call_p = os.path.join(nemsbase.NEMSPYENV,'scripts', 'python.exe')
        comm = [call_p, 'cycle.py', nemsbase.path_datekey, jobtype]
        
        shutil.copy(os.path.join(nemsbase.dir_scripts,"setup","src", "cel", "run_task.py"), "run_task.py")
        print('now send to queue')
        run_task.run_task_with_timeout(nemsbase.path_datekey, comm, "shared")

def build_emm_exe_exm_exo_list(nemsbase):
    '''
    Based on the user scedes settings, build and return 4 dictionraies for OML to bind.

    Parameters
    ----------
    nemsbase : NEMSBase object
        the main NEMS class.

    Returns
    -------
    ls_emm : dict
        list files of EMM model.
    ls_exe : dict
        list files grouped for EXE on/off switch.
    ls_exm : dict
        list files grouped for EXM on/off switch.
    ls_exo : dict
        list files grouped for EXO on/off switch.

    '''
    ls_emm=[{'file':'basemmi.txt', 'file2':'basemmi',\
             'display': 'BASEMMI.dat','src':nemsbase.scedvars.get('BASEMMIN')+nemsbase.scedvars.get('BASEMMID')}]
    ls_emm.append({'file':'BASEFDi.txt', 'file2':'basefdi',\
                   'display':'BASEFDI.dat', 'src':nemsbase.scedvars.get('BASEFDIN')+nemsbase.scedvars.get('BASEFDID')})

    ls_exe=[{'file':'plntdaf.daf', 'display': 'PLNTTMP.daf','src':nemsbase.scedvars.get('PLNTDAFN')+nemsbase.scedvars.get('PLNTDAFD')}]
    ls_exe.append({'file':'ettdem.daf', 'display':'ETTTMP.daf', 'src':nemsbase.scedvars.get('ETTDEMN')+nemsbase.scedvars.get('ETTDEMD')})

    ls_exm=[{'file':'mchighlo.xls', 'display': 'mchighlo.xls','src':nemsbase.scedvars.get('MCHIGHLON')+nemsbase.scedvars.get('MCHIGHLOD')}]
    ls_exm.append({'file':'comfloor.xls', 'display':'comfloor.xls', 'src':nemsbase.scedvars.get('COMFLOORN')+nemsbase.scedvars.get('COMFLOORD')})
    ls_exm.append({'file':'eviews32.ini', 'display':'eviews32.ini', 'src':nemsbase.scedvars.get('EVIEWS32N')+nemsbase.scedvars.get('EVIEWS32D')})

    ls_exo=[{'file':'lf_nem.gms', 'display': 'lf_nem.gms','src':nemsbase.scedvars.get('LF_NEMN')+nemsbase.scedvars.get('LF_NEMD')}]
    #ls_exo.append({'file':'lfinput.gms', 'display':'lfinput.gms', 'src':nemsbase.scedvars.get('LFINPUTN')+nemsbase.scedvars.get('LFINPUTD')})
    ls_exo.append({'file':'lfminput.gdx', 'display':'lfminput.gdx', 'src':nemsbase.scedvars.get('LFMINPUTN')+nemsbase.scedvars.get('LFMINPUTD')})
    ls_exo.append({'file':'lfmodel.gms', 'display':'lfmodel.gms', 'src':nemsbase.scedvars.get('LFMODELN')+nemsbase.scedvars.get('LFMODELD')})
    ls_exo.append({'file':'lfprep.gms', 'display':'lfprep.gms', 'src':nemsbase.scedvars.get('LFPREPN')+nemsbase.scedvars.get('LFPREPD')})
    ls_exo.append({'file':'lfrepset.gms', 'display':'lfrepset.gms', 'src':nemsbase.scedvars.get('LFREPSETN')+nemsbase.scedvars.get('LFREPSETD')})
    ls_exo.append({'file':'lfreport.gms', 'display':'lfreport.gms', 'src':nemsbase.scedvars.get('LFREPORTN')+nemsbase.scedvars.get('LFREPORTD')})
    ls_exo.append({'file':'lfshell.gms', 'display':'lfshell.gms', 'src':nemsbase.scedvars.get('LFSHELLN')+nemsbase.scedvars.get('LFSHELLD')})
    ls_exo.append({'file':'mpsshell.gms', 'display':'mpsshell.gms', 'src':nemsbase.scedvars.get('MPSSHELLN')+nemsbase.scedvars.get('MPSSHELLD')})
    ls_exo.append({'file':'cre8mps.gms', 'display':'cre8mps.gms', 'src':nemsbase.scedvars.get('CRE8MPSN')+nemsbase.scedvars.get('CRE8MPSD')})

    return ls_emm, ls_exe, ls_exm, ls_exo

def parse_cpcmds_copy_file(nemsbase, path):
    src=[]
    dst=[]
    with open(path,'r') as f:
        lines = filter(None, (line.strip() for line in f.readlines()))
        for line in lines:
            # if it's not starting with echo command, parse the line
            if line[:4] !='echo':
                s = line.split('; ')
                # we only need to parse the middle element 
                l = len('then cp ')
                s[1]=s[1][l:]
                path_list=s[1].split(' ')
                src.append(path_list[0].strip().replace('$NEMS',nemsbase.NEMS).replace('/','\\'))
                dst.append(path_list[1].strip().replace('$NEMS',nemsbase.NEMS).replace('/','\\'))

    # now copy files
    for idx, item in enumerate(src):
        nemsbase.util_check_before_copy(src[idx], dst[idx])

def copy_models_main_folder_etc(nemsbase, p):
    '''
    Copy mainpy required main and PyFiler etc. folders.

    Parameters
    ----------
    nemsbase : NEMSBase object
        the main NEMS class.
    p : string
        the partition folder (p1, p2, or p3) or "root" for the output folder root
    
    Returns
    -------
    None.
    '''
    # copy conversion factor input files:
    src_input_dir = os.path.join(nemsbase.NEMS, 'input')    
    dst_dir = r'.\input'
    dst_mnfactorx = os.path.basename(nemsbase.scedvars.get('MNFACTORXN'))
    dst_mnrisk = os.path.basename(nemsbase.scedvars.get('MNRISKN'))
    dst_qsblk = os.path.basename(nemsbase.scedvars.get('QSBLKN'))
    files = ('mnfactorx_calc_api_list.txt', 'mnfactorx_calc_xlsx_eq_list.txt', dst_mnfactorx, dst_mnrisk, dst_qsblk)
    if not os.path.isdir(dst_dir): os.mkdir(dst_dir)
    for f in files:
        shutil.copy(os.path.join(src_input_dir,f), os.path.join(dst_dir,f))
    if p=="root": return

    # no need ('cycle.py','oscillate.exe','cleanup.py') in {p1,p2,p3} level

    src_source_dir = os.path.join(nemsbase.NEMS, 'source')
    files = ('nems_flow.py','nems_flow_wrapper.py')
    for f in files:
        shutil.copy(os.path.join(src_source_dir,f), f)

    f = os.path.join(nemsbase.NEMS, 'models', 'main')
    shutil.copytree(f,'main')
    
    # copy FILELIST to root datecode folder
    if p == "p1":
        src_dir = os.path.join(nemsbase.NEMS, 'scedes')
        temp = os.path.join(src_dir, "FILELIST")
        shutil.copy(os.path.join(src_dir, "FILELIST"), "../FILELIST")

    src_input_dir = os.path.join(nemsbase.NEMS, 'input')
    if p == 'p1' and nemsbase.BoolM:
        # MAC module specific. Copy its input files and eviews under output root:
        src_dir = os.path.join(src_input_dir,'macro')
        for i in os.listdir(src_dir):
            shutil.copy(os.path.join(src_dir,i), i)    
        f = 'eviews32.ini'
        shutil.copy(os.path.join(src_input_dir,f), f)
    
    if p == 'p2' and nemsbase.BoolO:
        # LFMM module specific. Copy its input files under output root:
        src_models_lfmm_dir = os.path.join(nemsbase.NEMS,'models','lfmm')
        src_dir = os.path.join(src_models_lfmm_dir,'input')
        for i in os.listdir(src_dir):
            shutil.copy(os.path.join(src_dir,i), i)
        src_dir = os.path.join(src_models_lfmm_dir,'source')
        for i in os.listdir(src_dir):
            shutil.copy(os.path.join(src_dir,i), i)

    # copy entire PyFiler folder from the root:
    f = os.path.join(os.path.dirname(os.getcwd()), 'PyFiler')
    shutil.copytree(f,'PyFiler')

def build_parnems(nemsbase, jobtype, is_only_run_folder_checked):
    '''
    The parnems main method.

    Parameters
    ----------
    nemsbase : NEMSBase object
        the main NEMS class.
    jobtype : string
        either 'local' or 'queue'

    Returns
    -------
    None.

    '''
    #os.sys.stdout.flush()
    passes =('p1','p2','p3')
    for p in passes:
        os.chdir(nemsbase.path_datekey)
        print('\n\n')
        print(f'{nemsbase.log_prefix}'+'=======================================================')
        print(f'{nemsbase.log_prefix}'+f'setting up part {p}')
        nemsbase.util_create_dir(p)
        
        os.chdir(f'{p}')
        copy_filelist_etc(nemsbase)

        nemsbase.util_remove_file('nems.objs')
        
        # Check for, and delete, existing .Z files in output directory.
        # These would be there if performing a re-run.
        nemsbase.util_remove_extension(os.getcwd(),'.gz')

        # COPYINP(=>copyem) is a parameter in each secedes file, to on/off(1/0) copy input files to subdirectory of run output directory.

        # replace optional "$NEMS" string in scedes file names with the value of environment variable $NEMS
        scpt = r'cat FILELIST | sed "s@\$NEMS@$NEMS@g" > filelist.tmp'
        subprocess.run([scpt], stdout=subprocess.PIPE, shell=True)
        nemsbase.util_move_file_dir('filelist.tmp','FILELIST')

        tmp_cmd = os.path.join(nemsbase.NEMS, 'scripts','cac_filelist.exe')
        if (nemsbase.copyem): #"$copyem" != "0"
            nemsbase.util_remove_file('filelist.changed')
            nemsbase.util_remove_file('cpcmds.sh')
            subprocess.run([tmp_cmd, 'FILELIST', nemsbase.COPYDIR], stdout=subprocess.PIPE)
            
            # At this moment, bad filelist.changed is generated. FILELIST is still good
            # but next step FILELIST will be removed and replaced with filelist.changed
            fix_filelist_path_bug(nemsbase, 'Filelist.changed')
            fix_filelist_path_bug(nemsbase, 'cpcmds.sh')

            nemsbase.util_remove_file('FILELIST')

            # trim (right space) the content of  the filelist.changed file and save to the FILIST file
            nemsbase.util_trim_right_space('filelist.changed','FILELIST')
            
            nemsbase.util_remove_file('filelist.changed')
            nemsbase.util_create_dir(nemsbase.COPYDIR)
            # so far so good. Ready to copy a list of files

            # cac_filelist creates uncmprss.sh and cpcmds.sh to uncompress files and then copy them
            parse_cpcmds_copy_file(nemsbase, 'cpcmds.sh')
            
            nemsbase.util_remove_file('cpcmds.sh')
        else:
            #  the u as the directory to copy input files to really means do an "uncompress only"
            subprocess.run([tmp_cmd, 'FILELIST', 'u'], stdout=subprocess.PIPE)
            # cac_filelist creates uncmprss.sh to uncompress files but we don't need to run uncmprss anymore since no .zip files

        nemsbase.util_remove_file('hswrk.bat')
        nemsbase.util_remove_file('nohup.out')
        if not nemsbase.isScedvarsParsed:
            nemsbase.scedvars = nemsbase.parse_sced_vars(nemsbase.mode,p)

        build_ngpl_structure(nemsbase, p)
        copy_models_main_folder_etc(nemsbase, p)

        #build_reporter_structure(nemsbase, p)

        # finding error string
        if (nemsbase.BoolO): shutil.copy(os.path.join(nemsbase.dir_scripts, 'lffindstr.bat'), 'lffindstr.bat')
        
        if (nemsbase.BoolN and p == 'p2'):
            prev = os.getcwd()
            os.chdir(nemsbase.COPYDIR)
            #unzip PSBASEUNF.zip file
            with zipfile.ZipFile(nemsbase.scedvars.get('PSBASEUNFN'), 'r') as z:
                z.extractall()

            os.chdir(prev)

        #  put coal units file in p2 for coal model at beginning of run so it will be there for project open/read
        if (nemsbase.BoolC):
            build_coal_structure(nemsbase, p)

        if (nemsbase.BoolE):
            build_efd_ecp_rest_structure(nemsbase, p)
            # reserve the following for Ed EPHRTS project (nemspar.shell version cut-off date is 12/28/2022)
            build_ephrts_structure(nemsbase, p)

        if (nemsbase.BoolG):
            build_ngas_structure(nemsbase, p)

        if (nemsbase.BoolL):
            build_hsm_structure(nemsbase, p)
            
        if (nemsbase.BoolH):
            build_hmm_structure(nemsbase, p)
            build_idm_structure(nemsbase, p)
            
        if (nemsbase.BoolCC):
            build_ccats_structure(nemsbase, p)
            

        # Build the Python EPM folder even when nemsbase.BoolEPM is False
        # The epm_read routine is called for all runs, even if RUNEPM == 0
        build_epm_structure(nemsbase)


        shutil.copy(os.path.join(nemsbase.dir_scripts,'deloml.bat'), 'hswrk.bat')
        runoml = int(nemsbase.scedvars.get('EXE')) + int(nemsbase.scedvars.get('EXC')) + int(nemsbase.scedvars.get('EXH'))
        runoml = 1 if (runoml > 0) else 0
        wd = '' if (runoml > 0) else 'not '

        ls_emm, ls_exe, ls_exm, ls_exo = build_emm_exe_exm_exo_list(nemsbase)
        print(f'{nemsbase.log_prefix}'+f'NRUNS set to {nemsbase.NRUNS}')
        if (nemsbase.copyem):
            nruns_switch_emm_group(nemsbase, ls_emm, True)

            if (nemsbase.BoolE):
                copy_none_output_hswrk(nemsbase, 'basemmo.txt', 'BASEMMO.dat')

            nruns_switch_group(nemsbase, ls_exe, 'E', True, True)

            src=nemsbase.scedvars.get('EMMDBN')+nemsbase.scedvars.get('EMMDBD')
            copy_inputfile_output_hswrk(nemsbase, src, 'emmdb.mdb', 'EMMDB.mdb')

            nruns_switch_group(nemsbase, ls_exm, 'M', True, True)
            nruns_switch_group(nemsbase, ls_exo, 'O', True, True)

        else:
            nruns_switch_emm_group(nemsbase, ls_emm, False)
            nruns_switch_group(nemsbase, ls_exe, 'E', False, False)
            nruns_switch_group(nemsbase, ls_exm, 'M', False, False)
            nruns_switch_group(nemsbase, ls_exo, 'O', False, False)


        dbwrite = int(nemsbase.scedvars.get('ORCLECP')) + int(nemsbase.scedvars.get('ORCLEFD')) + int(nemsbase.scedvars.get('ORCLEFP'))
        dbmodon = int(nemsbase.scedvars.get('EXE'))

        if (p == 'p2'):
            # the following 2 codes are specific for parnems only. Because:
            # iterdump.f90(63): ! if this is a p2 run, there will be a copy of the full varlist.txt file in varlistall.txt. use that so the concatenated restart file
            shutil.copy('input\\varlist.txt', 'input\\varlistall.txt')
            shutil.copy('input\\varlistrec.txt', 'input\\varlist.txt')
            
            # copy executable for converting plant group data from text file to access database
            if dbwrite > 0 and dbmodon > 0:
                shutil.copy(f"{nemsbase.scedvars.get('UDBPN')}{nemsbase.scedvars.get('UDBPD')}", 'udbp.exe')

        elif (p == 'p3'):
            # MOREOPT.shell doesn't have the output 'ITERDUMP 0' like parnems/p3 does. It seems 'ITERDUMP 0' already will naturally output to jognems and {p1,p2} parnems, but not p3'. Is this a nemsh.shell missing code or on purpose? -Claire
            grep_word_append_moreopt_file(nemsbase,'iterdump','ITERDUMP 0')

        # I still declare TMP even though SetupPy doesn't need it. The nemspar.shell export it for udat.f.
        # source\udat.f(58) and command.sh calls $TMP
        # set the directory for link files to the current directory
        # here TMP will point to output folder\{p1,p2,p3}
        os.environ['TMP'] = os.getcwd()

        cycle_restart_emm(nemsbase)

        # clean-up script
        shutil.copy(nemsbase.scedvars.get('CLEANUP'),'cleanup.sh')
        # end of loop for setting up parallel folders.

    # now we come back to the root dir
    os.chdir('..')

    # Remainder sets up the parent folder.
    build_root_dir(nemsbase)


    # create scedes.all
    # be aware that {p1,p2,p3}/keys.sed with nullstr info and sceds.all is on the output root.
    src = os.path.join(nemsbase.OLDDIR, f'scedes.all.{nemsbase.bashvars.get("scenario")}.{nemsbase.bashvars.get("datekey")}')
    nemsbase.util_check_before_copy(src,'scedes.all')

    # build Validator input files
    nemsbase.copy_validator()
    nemsbase.copy_reporter()
    nemsbase.copy_converge()

    check_xpress(nemsbase)

    if not is_only_run_folder_checked:
        just_do_it(nemsbase, jobtype)
    else:
        print("You checked the Only create run folder checkbox. The output folder is ready!")


def build_jognems(nemsbase, jobtype, is_only_run_folder_checked):
    '''
    The jognems main method.

    Parameters
    ----------
    nemsbase : NEMSBase object
        the main NEMS class.
    jobtype : string
        either 'local' or 'queue'

    Returns
    -------
    None.

    '''
    os.chdir(nemsbase.path_datekey)
    print('\n\n')
    print(f'{nemsbase.log_prefix}'+'=======================================================')
    copy_filelist_etc(nemsbase)

    nemsbase.util_remove_file('nems.objs')

    # Check for, and delete, existing .Z files in output directory.
    # These would be there if performing a re-run.
    nemsbase.util_remove_extension(os.getcwd(),'.gz')

    # COPYINP(=>copyem) is a parameter in each secedes file, to on/off(1/0) copy input files to subdirectory of run output directory.

    # replace optional "$NEMS" string in scedes file names with the value of environment variable $NEMS
    scpt = r'cat FILELIST | sed "s@\$NEMS@$NEMS@g" > filelist.tmp'
    subprocess.run([scpt], stdout=subprocess.PIPE, shell=True)
    nemsbase.util_move_file_dir('filelist.tmp','FILELIST')

    tmp_cmd = os.path.join(nemsbase.NEMS, 'scripts','cac_filelist.exe')
    if (nemsbase.copyem): #"$copyem" != "0"
        nemsbase.util_remove_file('filelist.changed')
        nemsbase.util_remove_file('cpcmds.sh')
        subprocess.run([tmp_cmd, 'FILELIST', nemsbase.COPYDIR], stdout=subprocess.PIPE)
        
        # At this moment, bad filelist.changed is generated. FILELIST is still good
        # but next step FILELIST will be removed and replaced with filelist.changed
        fix_filelist_path_bug(nemsbase, 'Filelist.changed')
        fix_filelist_path_bug(nemsbase, 'cpcmds.sh')

        nemsbase.util_remove_file('FILELIST')

        # trim (right space) the content of  the filelist.changed file and save to the FILIST file
        nemsbase.util_trim_right_space('filelist.changed','FILELIST')

        nemsbase.util_remove_file('filelist.changed')
        nemsbase.util_create_dir(nemsbase.COPYDIR)
        # so far so good. Ready to copy a list of files

        # cac_filelist creates uncmprss.sh and cpcmds.sh to uncompress files and then copy them
        parse_cpcmds_copy_file(nemsbase, 'cpcmds.sh')
        
        nemsbase.util_remove_file('cpcmds.sh')
    else:
        # the u as the directory to copy input files to really means do an "uncompress only"
        subprocess.run([tmp_cmd, 'FILELIST', 'u'], stdout=subprocess.PIPE)
        # cac_filelist creates uncmprss.sh to uncompress files but we don't need to run uncmprss anymore since no .zip files

    nemsbase.util_remove_file('hswrk.bat')
    nemsbase.util_remove_file('nohup.out')
    if not nemsbase.isScedvarsParsed:
        nemsbase.scedvars = nemsbase.parse_sced_vars(nemsbase.mode)

    copy_models_main_folder_etc(nemsbase, 'root')    

    # finding error string
    if (nemsbase.BoolO): shutil.copy(os.path.join(nemsbase.dir_scripts, 'lffindstr.bat'), 'lffindstr.bat')

    if (nemsbase.BoolN):
        prev = os.getcwd()
        os.chdir(nemsbase.COPYDIR)
        #unzip PSBASEUNF.zip file
        with zipfile.ZipFile(nemsbase.scedvars.get('PSBASEUNFN'), 'r') as z:
            z.extractall()

        os.chdir(prev)

    #  put coal units file in p2 for coal model at beginning of run so it will be there for project open/read
    if (nemsbase.BoolC):
        build_coal_structure(nemsbase)

    if (nemsbase.BoolE):
        build_efd_ecp_rest_structure(nemsbase)
        # reserve the following for Ed EPHRTS project (nemspar.shell version cut-off date is 08/03/2022)
        build_ephrts_structure(nemsbase)

    if (nemsbase.BoolG):
        build_ngas_structure(nemsbase)

    if (nemsbase.BoolL):
        build_hsm_structure(nemsbase)
    
    if (nemsbase.BoolH):
        build_hmm_structure(nemsbase)
        build_idm_structure(nemsbase)
        
    if (nemsbase.BoolCC):
        build_ccats_structure(nemsbase)
        
    # Build the Python EPM folder even when nemsbase.BoolEPM is False
    # The epm_read routine is called for all runs, even if RUNEPM == 0
    build_epm_structure(nemsbase)

    build_ngpl_structure(nemsbase)
    #build_reporter_structure(nemsbase)

    shutil.copy(os.path.join(nemsbase.dir_scripts,'deloml.bat'), 'hswrk.bat')

    ls_emm, ls_exe, ls_exm, ls_exo = build_emm_exe_exm_exo_list(nemsbase)
    print(f'{nemsbase.log_prefix}'+f'NRUNS set to {nemsbase.NRUNS}')
    if (nemsbase.copyem):
        nruns_switch_emm_group(nemsbase, ls_emm, True)

        if (nemsbase.BoolE):
            # Identically translate the code in nemspar.shell. Be aware. The following two lines are not in the origin else (i.e. not copyem) statement.
            copy_none_output_hswrk(nemsbase, 'basemmo.txt', 'BASEMMO.dat')

        nruns_switch_group(nemsbase, ls_exe, 'E', True, True)

        # Identically translate the code in nemspar.shell. Be aware. the following two lines are not in the origin else (i.e. not copyem) statement.
        src=nemsbase.scedvars.get('EMMDBN')+nemsbase.scedvars.get('EMMDBD')
        copy_inputfile_output_hswrk(nemsbase, src, 'emmdb.mdb', 'EMMDB.mdb')

        nruns_switch_group(nemsbase, ls_exm, 'M', True, True)
        nruns_switch_group(nemsbase, ls_exo, 'O', True, True)

    else:
        nruns_switch_emm_group(nemsbase, ls_emm, False)
        nruns_switch_group(nemsbase, ls_exe, 'E', False, False)
        nruns_switch_group(nemsbase, ls_exm, 'M', False, False)
        nruns_switch_group(nemsbase, ls_exo, 'O', False, False)
    
    # nemsh.shell has this code block in different position but in nemspar.shell is under p2 folder. I re-arrange it to here.
    dbwrite = int(nemsbase.scedvars.get('ORCLECP')) + int(nemsbase.scedvars.get('ORCLEFD')) + int(nemsbase.scedvars.get('ORCLEFP'))
    dbmodon = int(nemsbase.scedvars.get('EXE'))
    if dbwrite > 0 and dbmodon > 0:
        shutil.copy(f"{nemsbase.scedvars.get('UDBPN')}{nemsbase.scedvars.get('UDBPD')}", 'udbp.exe')

    # source\udat.f(58) and command.sh calls $TMP
    os.environ['TMP'] = os.getcwd()

    cycle_restart_emm(nemsbase)

    # clean-up script
    shutil.copy(nemsbase.scedvars.get('CLEANUP'),'cleanup.sh')
    # end of loop for setting up parallel folders.

    # for parnems, we need to come back to the root dir. But for jognems, we stay in the root always
    #os.chdir('..')

    # Remainder sets up the parent folder.
    build_root_dir(nemsbase)

    # create scedes.all so future run replication/archival will work
    src = os.path.join(nemsbase.OLDDIR, f'scedes.all.{nemsbase.bashvars.get("scenario")}.{nemsbase.bashvars.get("datekey")}')
    nemsbase.util_check_before_copy(src,'scedes.all')

    # build Validator input files
    nemsbase.copy_validator()
    nemsbase.copy_reporter()
    nemsbase.copy_converge()

    check_xpress(nemsbase)
    
    if not is_only_run_folder_checked:
        just_do_it(nemsbase, jobtype)
    else:
        print("You checked the Only create run folder checkbox. The output folder is ready!")


def compose_jobtype_for_cyclepy(jobtype, mode):
    '''
    Compose jobtype string to pass over to cycle.py.

    Parameters
    ----------
    jobtype : string
        either 'local' or 'queue'
    mode : string
        'par'(parnems) or 'jog'(jognems).

    Returns
    -------
    result: the composed jobtype string.
    '''    
    result = ''
    if jobtype == 'local':
        result = 'local_'
    else:
        result = 'queue_'
    
    if mode == 'par':
        result += 'parallel'
    else:
        result += 'sequential'
    return result

if __name__ == '__main__':
    mode = os.getenv('PARNEMS','')
    mode = 'jog' if mode == '' else 'par'
    
    jobtype = os.getenv('JOBTYPE','')
    jobtype = compose_jobtype_for_cyclepy(jobtype, mode)
    is_only_run_folder_checked = (os.getenv('CHKRUNDIR',0)=="1")

    nb = NEMSBase(mode)
    nb.main()
    
    if mode == 'jog':
        build_jognems(nb, jobtype, is_only_run_folder_checked)
    else:
        build_parnems(nb, jobtype, is_only_run_folder_checked)