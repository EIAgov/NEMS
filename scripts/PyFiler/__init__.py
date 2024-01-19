"""
Created on Wed Jun 1 17:00:45 2022

@author: JMW

A wrapper code to initialize PyFiler and convert restart files from 'restart.unf' to 'restart.hdf' and vice versa
"""


import os
import sys
import shutil
#import .pyfiler
#import .PyFilerWrapper

def PyFilerFileCheck (NEMSDIR, reqfile):
    """parses the relative to where this is launched from PyFiler/Input directory to see whether or not dict.txt and
     varlist.txt are in the PyFiler/Input folder and whether FILELIST is in the PyFiler directory or not.
    Parameters
    ----------
    dir- Current directory
    reqfile- required files to run PyFiler

    Returns
    -------
    Saves dict.txt, varlist.txt and FILELIST into the appropriate location.
    """
    if NEMSDIR[-4:] == 'NEMS':
        print('NEMS if PATH: ' + NEMSDIR)
        file_exists = os.path.exists(NEMSDIR + '\\scripts\\PyFiler\\input\\dict.txt'), os.path.exists(NEMSDIR + '\\scripts\\PyFiler\\input\\varlist.txt'), True
    else:
        print('Non-NEMS if path: ' + NEMSDIR)
        file_exists = os.path.exists(os.getcwd()+'\\PyFiler\\input\\dict.txt'), os.path.exists(os.getcwd()+'\\PyFiler\\input\\varlist.txt'), False
    for ii in range(len(file_exists)):
        if file_exists[ii] == False:
            if NEMSDIR[-4:] =='NEMS':
                print('NEMS Try If')
                shutil.copy(NEMSDIR+'\\input\\'+reqfile[ii],NEMSDIR+'\\scripts\\PyFiler\\input\\'+reqfile[ii])
                if reqfile[ii] == 'FILELIST':
                    break
            elif reqfile[ii] == 'FILELIST':
                print('FILELIST location: ' + os.getcwd())
                # Looking for FILELIST in PARNEMS Folder Structure
                if os.getcwd()[-2:] == 'p1':
                    shutil.copy(os.getcwd()+"\\" + reqfile[ii],os.path.abspath(os.path.join(os.getcwd(),".."))+'\\PyFiler\\'+reqfile[ii])
                # Looking for FILELIST in JOGNEMS Folder Structure
                elif os.getcwd()[-2] not in ('p2','p3'):
                    shutil.copy(os.getcwd() + "\\" + reqfile[ii], os.getcwd() + '\\PyFiler\\' + reqfile[ii])
                else:
                    print('Error, look at line 54')
            else: #DICT.TXT finder
                print('Dict/Varlist File location: ' + os.getcwd())
                #Looking for dict.txt in PARNEMS Folder Structure
                if os.getcwd()[-2:] == 'p1':
                    shutil.copy(os.getcwd()+'\\input\\'+reqfile[ii],os.path.join(os.getcwd(),"..")+'\\PyFiler\\input\\'+reqfile[ii])
                #Looking for dict.txt in JOGNEMS Folder Structure
                elif os.getcwd()[-2] not in ('p2','p3'):
                    shutil.copy(os.getcwd() + '\\input\\' + reqfile[ii],os.getcwd() + '\\PyFiler\\input\\' + reqfile[ii])
                else:
                    print('Error, look at line 61')

"""
Code for __init__.py. Gets the current working directory, and if it's not one of the output directories listed below,
checks to get the directory into the correct starting position of .../.../.../.../NEMS for corect location of the 
input file dict.txt needed.
"""
curdir = os.getcwd()
print('Current Directory is: ' + curdir)
print('Last 4 of CurDir is: ' + curdir[-4:])
print('Current Drive is : ' + curdir[0])
# Checking whether or not this is running a JOG/PARNEMS run or within the users local repository.
# TODO: Look into whether or not NEMS Python will need to check folder location dependency.
if curdir[0].upper() not in ('K', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V'):
    if curdir[-4:] != 'NEMS':
        curdir = os.path.abspath(os.path.join(curdir, '..'))
        if curdir[-4:] != 'NEMS':
            curdir = os.path.abspath(os.path.join(curdir, '..'))
            if curdir[-4:] != 'NEMS':
                curdir = os.path.abspath(os.path.join(curdir, '..'))
PyFilerReqFiles = ['dict.txt', 'varlist.txt', 'FILELIST']
PyFilerFileCheck(curdir, PyFilerReqFiles)