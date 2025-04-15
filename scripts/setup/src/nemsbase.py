# -*- coding: utf-8 -*-
"""
Created on Feb 24 2023

@author: Claire Su
"""

import os, shutil
import subprocess
import glob
import getpass
from datetime import datetime, timedelta, date
import pandas as pd
import sys # reserve for debugging

class NEMSBase:

    from nemsutil import util_create_dir, util_move_file_dir, util_remove_file, util_remove_extension, util_copy_extension, util_append_content
    from nemsutil import util_check_before_copy, util_convert_py_path_to_ksh_path, util_debugging_dump_dict, util_trim_right_space

    def read_bash_scedes_variables(self):
        '''
        A preparation step for NEMS run. Build a dict of NEMS required settings and initate an empty dict for scedes.

        Returns
        -------
        bashvars : dict
            a dict to store the required NEMS settings passed by submit.sh.
        scedvars : dict
            a dict to store the parsed scedes settings.

        '''
        bashvars = {}
        ls=['launchedSCEN','launchedUSCEN','USER','NEMS','OUTDIR','scenario','datekey','copyem','COPYDIR','NEMSJOBLOG', 'COMPUTERNAME','OSTYPE']
        for e in ls:
            bashvars[e]=os.getenv(e,'')
            if (bashvars.get(e) is not None):
                bashvars[e]=bashvars[e].replace('/', '\\')

        scedvars = {}

        return bashvars, scedvars

    def set_sced_var(self, var_dict , key, value):
        '''
        Assign a scedes key with a value. If the value is a path, replace to Python path format.

        Parameters
        ----------
        var_dict : dict
            the dictionary (Ex. NEMSBase.scedvars) to set the scedes key value.
        key : string
            the key in the dictionary.
        value : string
            the value to be assigned.

        Returns
        -------
        var_dict : dict
            return the dictionary object with the new value.

        '''
        # bash has no export this. Set the value from varkeys or scedes files.
        var_dict[key] = value
        if (var_dict.get(key) is not None):
            var_dict[key]=var_dict[key].replace('/', '\\')
        return var_dict

    def parse_sced_vars(self, mode, p='p1'):
        '''
        Load and parse the output keys.sed file. Assign the scedes key and value into the dictionary (NEMSBase.scedvars)

        Parameters
        ----------
        mode : string
            'par'(parnems) or 'jog'(jognems).
        p : string, optional
            subfolder p1, p2, or p3. The default is 'p1'.

        Returns
        -------
        scedvars : dict
            the NEMSBase scedes key value dictionary.

        '''
        scedvars = self.scedvars
        file = os.path.join(self.path_datekey,p,'keys.sed')
        if mode != 'par':
            file = os.path.join(self.path_datekey,'keys.sed')

        with open(file, 'r', encoding='utf-8') as f:
            # filter out blank lines
            lines = filter(None, (line.strip() for line in f.readlines()))
            for line in lines:
                s = line.split('=')
                s[1] = s[1].replace('nullstr','')
                s[1] = s[1].replace('$NEMS',self.NEMS)
                scedvars = self.set_sced_var(scedvars, s[0], s[1])

        # explicitly set H (hydrogen) here. Due to H=nullstr is missing in keys.sed file
        # Remove the code once the hydrogen project is done.
        scedvars = self.set_sced_var(scedvars, 'H', '')

        # the following var names are customized in namespar.shell and nemsh.shell. Reflect the change:
        scedvars['scenario'] = scedvars.get('SCEN')
        scedvars['datekey'] = scedvars.get('DATE')
        scedvars = {key:val for key, val in scedvars.items() if key not in ('Mani','DATE')}

        # origin in nemsbase.__init_(), move to here to set:
        self.set_base_property(scedvars)

        self.isScedvarsParsed = True

        return scedvars

    def parse_sced_after_submitsh(self, file):
        '''
        Search scedes key RETAIN in a user defined file (a scedes file) for the NEMS run. Write the RETAIN value into NEMSBase.bashvars dict. 

        Parameters
        ----------
        file : string
            the path and name of the user specified scedes file to be parse

        Returns
        -------
        bashvars : dict
            a dict to store the required NEMS setting passed by submit.sh.

        '''
        bashvars = self.bashvars
        with open(file, 'r', encoding='utf-8') as f:
            # filter out blank lines
            lines = filter(None, (line.strip() for line in f.readlines()))
            for line in lines:
                s = line.split('=')
                s[1] = s[1].replace('nullstr','')
                s[1] = s[1].replace('$NEMS',self.NEMS)
                if s[0] in ('RETAIN'):
                    bashvars[s[0]] = s[1].replace('/', '\\')
                    #scedvars = self.set_sced_var(scedvars, s[0], s[1])
        return bashvars

    def copy_initial_files(self):
        '''
        Exam the existence of the files ('filelist.', 'keys.sed.') and print out messages. Create the output directory.

        Returns
        -------
        None.

        '''
        print("dt_scenario=", self.dt_scenario)
        # check the exsistence of files FILELIST.$scenario.$datekey. If the file doesn not exist, print message and exit.
        if(not os.path.exists('filelist.'+ self.dt_scenario)):
            print(f'{self.log_prefix}'+' NO '+ a[:-1] +' FILE !!!!! ')
            print(f'{self.log_prefix}'+' SCRIPT ENDING ')
            os.sys.exit() # the later shell script shall naturally exit/crash later since no good filelist etc.

        # check the exsistence of keys.sed.$scenario.$datekey. If the file doesn not exist, print message and copy key.sed over.
        f = 'keys.sed.' + self.dt_scenario
        if (not os.path.exists(f)):
            print(f'{self.log_prefix}'+f' NO keys.sed.{self.bashvars.get("scenario")}.{self.bashvars.get("datekey")} FILE !!!!! ')
            print(f'{self.log_prefix}'+' using keys.sed ')
            shutil.copy('keys.sed', f)
    
        print(f'{self.log_prefix}'+f'setting up NEMS scenario {self.bashvars.get("scenario")}')
    
        self.util_create_dir(self.path_scenario)
        os.chdir(self.path_scenario)
    
        print(f'{self.log_prefix}'+f'setting up datekey {self.bashvars.get("datekey")}')
        self.util_create_dir(self.path_datekey)

    def write_scedeshash_log_via_ls_command(self, file_patterns, metafile, scedfile, title=''):
        """
        Write scedeshash log by gathering file hashes using git log.

        Parameters
        ----------
        file_patterns : list
            Patterns to match files (e.g., ['*.f', '*.f90']).
        metafile : str
            Path to save matched file names.
        scedfile : str
            Path to append the git log results.
        title : str
            Title to prepend to the log.

        Returns
        -------
        None
        """
        # Find files matching the patterns
        matched_files = []
        for pattern in file_patterns:
            matched_files.extend(glob.glob(pattern))
        matched_files = sorted(set(matched_files))  # Remove duplicates, if any

        # Write matched files to metafile
        metafile_path = os.path.join(self.path_datekey, metafile)
        with open(metafile_path, 'w', encoding='utf-8') as meta:
            meta.write('\n'.join(matched_files) + '\n')

        print(f'{self.log_prefix} Getting hash codes for {metafile_path}')

        # Call git log in a single batch
        if matched_files:
            git_command = ['git', 'log', '-n1', '--oneline', '--'] + matched_files
            result = subprocess.run(git_command, stdout=subprocess.PIPE, text=True)
            git_logs = result.stdout.strip().split('\n')

            # Write results to scedfile
            with open(scedfile, 'a', encoding='utf-8') as sced:
                sced.write(title + '\n')
                # Generate all content in a batch
                content_lines = [f"{log.split(' ')[0]}" for log in git_logs]
                # Write all content at once, followed by a newline
                sced.write('\n'.join(content_lines) + '\n\n')


    def get_git_status(self):
        '''
        The main method to get git status on $NEMS\source and $NEMS\input folder by calling write_scedeshash_log_via_ls_command()

        Returns
        -------
        None.

        '''
        os.chdir(self.NEMS)

        # git updates
        cmitfile = os.path.join(self.path_datekey,'commit_head')
        with open(cmitfile, 'w', encoding='utf-8') as f:
            f.write("Last commit before this run was made:\n" )
            result = subprocess.run(['git','log','--name-only','-n','1'],stdout=subprocess.PIPE,shell=True)
            f.write(result.stdout.decode('utf-8'))

            f.write( "\nFiles modified but not committed:\n")
            result = subprocess.run(['git','ls-files','-m'],stdout=subprocess.PIPE,shell=True)
            f.write(result.stdout.decode('utf-8'))

            f.write( "\nAll commits from the last month:\n")
            result = subprocess.run(['git','log','--name-only','--since="1 month ago"'],stdout=subprocess.PIPE,shell=True)
            f.write(result.stdout.decode('utf-8'))

        # write out scedeshash.all
        scedfile = os.path.join(self.path_datekey, "scedeshash.all")
        with open(scedfile, 'w', encoding='utf-8') as f:
            # We may not need this scedeshash.all file. The info is dupilicated with the log file commit_head.
            f.write("Files modified but not committed:\n")
            result = subprocess.run(['git','ls-files','-m'],stdout=subprocess.PIPE,shell=True)
            f.write(result.stdout.decode('utf-8'))
            f.write('\n')

        # Move to $NEMS/source directory and log source files
        os.chdir(os.path.join(self.NEMS, 'source'))
        self.write_scedeshash_log_via_ls_command(['*.f', '*.f90'], 'sourcefiles', scedfile, 
                                                'The last committed hash of the source files:')

        # Move to $NEMS/input directory and log input files
        os.chdir(os.path.join(self.NEMS, 'input'))
        self.write_scedeshash_log_via_ls_command(['*'], 'inputfiles', scedfile, 
                                                'The last committed hash of the input files:')

    def retain_files(self):
        """
        calucate the output folder retain date based on the scedes file RETAIN setting. 
        RETAIN=0 means permanent keep. No retain file geerated. Otherwise, a remove_yyyymmdd.txt will be gererated.
        Default is 90 days.

        Returns
        -------
        None.

        """
        os.chdir(self.path_datekey)
        retainrun = self.bashvars.get('RETAIN')
        if not retainrun.isdigit():
            print(f'{self.log_prefix}'+"Bad retention period requested, resetting to 90 days.")
            retainrun = '90'
            self.bashvars['RETAIN'] = retainrun

        if retainrun > '0':
            # calculate the future deletion date

            new_date = date.today() + timedelta(days=int(retainrun))
            future = str(new_date.year) + str(new_date.month).zfill(2) + str(new_date.day).zfill(2)

            s = f"Retain this run for {int(retainrun)} days (automatically delete after {int(retainrun)} days)"
            print(f'{self.log_prefix}'+f"Retention period set to {int(retainrun)} days\n{s}")

            t = os.path.join(os.getcwd(), f'remove_{future}.txt')
            with open(t, 'w', encoding='utf-8') as f:
                f.write(s)


    def copy_pyfiler(self):
        '''
        Copy the entire PyFiler folder to the output directory.

        Returns
        -------
        None.

        '''
        shutil.copytree(os.path.join(self.dir_scripts, 'PyFiler'), 'PyFiler')

    def copy_validator(self):
        '''
        Copy the entire Validator folder to the output directory.

        Returns
        -------
        None.

        '''
        # build Validator input files
        print(f'{self.log_prefix}'+'start building Validator input files')
        shutil.copytree(os.path.join(self.dir_scripts, 'Validator'), 'Validator')

        os.chdir('Validator')
        print(f'{self.log_prefix}'+'creating Validator input folder')
        os.makedirs('.\\input', mode = 0o777, exist_ok = False)
        ls=[{'src':self.scedvars.get('STEOVARSN'), 'dst':r'.\input\steovars.csv'}]
        ls.append({'src':self.scedvars.get('VLDCTLRN'), 'dst':r'.\input\validator_controller.csv'})
        ls.append({'src':os.path.join(self.dir_input,'coal','steo_benchmark_cmm.csv'), 'dst':r'.\input\steo_benchmark_cmm.csv'})
        ls.append({'src':os.path.join(self.dir_input,'ngas','steo_benchmark_ngmm.csv'), 'dst':r'.\input\steo_benchmark_ngmm.csv'})
        ls.append({'src':os.path.join(self.dir_input,'emm','steo_benchmark_emm.csv'), 'dst':r'.\input\steo_benchmark_emm.csv'})
        ls.append({'src':os.path.join(self.dir_input,'bld','steo_benchmark_buildings.csv'), 'dst':r'.\input\steo_benchmark_buildings.csv'})
        ls.append({'src':os.path.join(self.dir_input,'idm','steo_benchmark_industrial.csv'), 'dst':r'.\input\steo_benchmark_industrial.csv'})
        ls.append({'src':os.path.join(self.dir_input,'itg','steo_benchmark_integration.csv'), 'dst':r'.\input\steo_benchmark_integration.csv'})
        ls.append({'src':os.path.join(self.dir_input,'lfmm','steo_benchmark_liquids.csv'), 'dst':r'.\input\steo_benchmark_liquids.csv'})
        ls.append({'src':os.path.join(self.dir_input,'hsm','steo_benchmark_hsm.csv'), 'dst':r'.\input\steo_benchmark_hsm.csv'})
        ls.append({'src':os.path.join(self.dir_input,'macro','steo_benchmark_macro.csv'), 'dst':r'.\input\steo_benchmark_macro.csv'})
        for f in ls:
            shutil.copy(f['src'], f['dst'])

        #now back to the output root
        os.chdir('..')
        
    def copy_reporter(self):
        '''
        Copy the entire reporter folder to the output directory.

        Returns
        -------
        None.

        '''
        shutil.copytree(os.path.join(self.dir_models, 'reporter'), 'reporter')
        
    def copy_converge(self):
        '''
        Copy the entire reporter folder to the output directory.

        Returns
        -------
        None.

        '''
        shutil.copytree(os.path.join(self.dir_models, 'converge'), 'converge')
        if os.path.exists('p1'): shutil.copytree(os.path.join(self.dir_models, 'converge'), 'p1/converge')
        if os.path.exists('p2'): shutil.copytree(os.path.join(self.dir_models, 'converge'), 'p2/converge')
        if os.path.exists('p3'): shutil.copytree(os.path.join(self.dir_models, 'converge'), 'p3/converge')

    def copy_main_flow_files(self):
        '''
        Copy mainpy nems_flow.py, cycle.py, the main folder and PyFiler essential oml64 DLL files.

        Returns
        -------
        None.
        '''
        src_script_dir = os.path.join(self.NEMS, 'scripts')
        files = ('cycle.py', 'cycle_helper.py','oscillate.exe','cleanup.py')
        for f in files:
            shutil.copy(os.path.join(src_script_dir,f), f)

        src_source_dir = os.path.join(self.NEMS, 'source')
        files = ('nems_flow.py','nems_flow_wrapper.py','intercv.exe','tfiler.exe')
        for f in files:
            shutil.copy(os.path.join(src_source_dir,f), f)

        f = os.path.join(self.NEMS, 'models', 'main')
        shutil.copytree(f,'main')

        # copy essentail files to one level up
        files = "unf_to_npz.py"
        shutil.copy(os.path.join(r'.\main',files), files)

        src_input_dir = os.path.join(self.NEMS, 'input')
        if (self.BoolM):
            # MAC module specific. Copy its input files and eviews under output root:
            src_dir = os.path.join(src_input_dir,'macro')
            for i in os.listdir(src_dir):
                shutil.copy(os.path.join(src_dir,i), i)
            f = 'eviews32.ini'
            shutil.copy(os.path.join(src_input_dir,f), f)
        
        if (self.BoolO):
            # LFMM module specific. Copy its input files under output root:
            src_models_lfmm_dir = os.path.join(self.NEMS,'models','lfmm')
            src_dir = os.path.join(src_models_lfmm_dir,'input')
            for i in os.listdir(src_dir):
                shutil.copy(os.path.join(src_dir,i), i)
            src_dir = os.path.join(src_models_lfmm_dir,'source')
            for i in os.listdir(src_dir):
                shutil.copy(os.path.join(src_dir,i), i)

    def write_launched_from(self):
        '''
        Output username, parnems or jognems, output folder location etc. info to a launched.from file

        Returns
        -------
        None.

        '''
        s1 = f"{self.bashvars.get('scenario')}/{self.bashvars.get('datekey')} launched from {self.OLDDIR} by {self.USER} on"
        s2 = datetime.now().astimezone().strftime("%a %B %d %Y %H:%M:%S %Z") +'\n'
        s3 = f"using NEMS platform stored here: {self.NEMS}\n"
        result = (((subprocess.getoutput(f'net user {getpass.getuser()} '+'/DOMAIN ')).split('Full Name')[1]).lstrip()).split(" ")
        name = result[0]+result[1].split('\n')[0]
        mode_str = 'parnems' if self.mode == 'par' else 'jognems'
        with open('launched.from', 'w', encoding='utf-8') as f:
            f.write(s1+'\n'+s2+s3)
            f.write(f'username: {name}\n')
            f.write(f'userid: {os.getenv("USERNAME")}\n')
            f.write(f'This run was set up using {mode_str}\n')
            f.write(f"The common scedes file or run used to set up this run: {self.bashvars.get('scenario')}\n")
            f.write(f"The user scedes file to set up this run:               {self.bashvars.get('launchedUSCEN')}\n")
            f.write(f'OUTDIR={self.path_datekey}\n')

    def set_base_property(self,scedvars):
        '''
        Set and build the essential scedes key value. This is revoked in parse_sced_vars() after parse all scedes from the keys.sed file. 

        Parameters
        ----------
        scedvars : dict
            the NEMSBase scedes key value dictionary.

        Returns
        -------
        None.

        '''
        self.COPYDIR = scedvars.get('COPYDIR') #.\input

        self.AIMMSBIT = scedvars.get('AIMMS64') #AIMMSBIT=?AIMMS64@
        self.AIMMSVER = scedvars.get('AIMMSVER')
        
        self.NRUNS = int(scedvars.get('NRUNS'))
        self.copyem = (scedvars.get('copyem') != '0')  #nemspar.shell: if ["$copyem" != "0" ]
        self.OMLVERS = scedvars.get('OMLVERS')
        self.lnkinems = scedvars.get('LNKINEMS')
        self.NEMSPYENV = scedvars.get('NEMSPYENV')

        # set bool values to replace ?C@,?M@,?R@ etc. We may remove this code block if we decide to phase out ?C@ etc. design.
        self.BoolW=(scedvars.get('EXW') == '1')
        self.BoolM=(scedvars.get('EXM') == '1')
        self.BoolR=(scedvars.get('EXR') == '1')
        self.BoolK=(scedvars.get('EXK') == '1')
        self.BoolT=(scedvars.get('EXT') == '1')
        self.BoolI=(scedvars.get('EXI') == '1')
        self.BoolC=(scedvars.get('EXC') == '1')
        self.BoolE=(scedvars.get('EXE') == '1')
        self.BoolG=(scedvars.get('EXG') == '1')
        self.BoolL=(scedvars.get('EXL') == '1')
        self.BoolO=(scedvars.get('EXO') == '1')
        self.BoolN=(scedvars.get('EXN') == '1')
        self.BoolH=(scedvars.get('EXH') == '1')
        self.BoolCC=(scedvars.get('EXQ') == '1')

    
    def __init__(self, mode='par'):
        '''
        The init method of NEMSBase class.

        Parameters
        ----------
        mode : string
            'par'(parnems) or 'jog'(jognems). The default is 'par'.

        Returns
        -------
        None.

        '''
        self.mode = mode
        self.log_prefix = '[Setup Program]'

        self.isScedvarsParsed = False

        self.bashvars, self.scedvars = self.read_bash_scedes_variables()

        self.USER    = self.bashvars.get('USER')
        self.NEMS    = self.bashvars.get('NEMS')
        self.OUTDIR  = self.bashvars.get('OUTDIR')
        self.copyem = (self.bashvars.get('copyem') != '0')  #nemspar.shell: if ["$copyem" != "0" ]
        self.COPYDIR = self.bashvars.get('COPYDIR') #.\input

        self.path_scenario = os.path.join(self.OUTDIR,self.bashvars.get('scenario'))
        self.path_datekey  = os.path.join(self.OUTDIR,self.bashvars.get('scenario'), self.bashvars.get('datekey'))
        self.dt_scenario   = self.bashvars.get('scenario') + '.' + self.bashvars.get('datekey')

        self.dir_scripts= os.path.join(self.NEMS, 'scripts')
        self.dir_seceds = os.path.join(self.NEMS, 'scedes')
        self.dir_input  = os.path.join(self.NEMS, 'input')
        self.dir_models = os.path.join(self.NEMS, 'models')

        # parse scedes file directly after submit.sh
        self.scedvars = self.parse_sced_after_submitsh(self.bashvars.get('launchedUSCEN'))

        # set bool values to replace ?C@,?M@,?R@ etc. Default all models turn on.
        self.BoolW=True #(self.scedvars.get('W') == '')
        self.BoolM=True #(self.scedvars.get('M') == '')
        self.BoolR=True #(self.scedvars.get('R') == '')
        self.BoolK=True #(self.scedvars.get('K') == '')
        self.BoolT=True #(self.scedvars.get('T') == '')
        self.BoolI=True #(self.scedvars.get('I') == '')
        self.BoolC=True #(self.scedvars.get('C') == '')
        self.BoolE=True #(self.scedvars.get('E') == '')
        self.BoolG=True #(self.scedvars.get('G') == '')
        self.BoolL=True #(self.scedvars.get('L') == '')
        self.BoolO=True #(self.scedvars.get('O') == '')
        self.BoolN=True #(self.scedvars.get('N') == '')
        self.BoolH=True #(self.scedvars.get('H') == '')

    def main(self):
        '''
        The main method of NEMSBase class

        Returns
        -------
        None.

        '''
        # We need to know where the runnems files were located,which is the present directory
        self.OLDDIR=os.getcwd() 

        self.copy_initial_files()
        self.get_git_status()
        self.retain_files()

        # Moving of PyFiler to Output Folder
        self.copy_pyfiler()

        self.copy_main_flow_files()
        self.write_launched_from()
        