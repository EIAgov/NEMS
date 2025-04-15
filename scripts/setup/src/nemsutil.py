#!/usr/bin/env python3
"""
Created on Feb 24 2023

@author: Claire Su
"""

import os, shutil
from os.path import exists

log_prefix = '[Setup Program]'

def util_create_dir(self, p):
    '''
    Safely create a directory and print out the creating messages.

    Parameters
    ----------
    p : string
        the path of the directory.

    Returns
    -------
    None.

    '''
    if os.path.isdir(p):
        print(f'{log_prefix}'+f'{p} exists')
        print(f'{log_prefix}'+"continuing")
    else:
        print(f'{log_prefix}'+f'creating {p}')
        os.mkdir(p)


def util_move_file_dir(self, src, dst):
    '''
    A util method to safely move a file or directory to the destination.

    Parameters
    ----------
    src : string
       the source, moved-from filename or directory.
    dst : string
        the destination, moved-to filename or directory.

    Returns
    -------
    None.

    '''
    if os.path.exists(src):
        shutil.move(src, dst)

def util_remove_file(self, file):
    '''
    A util method to remove a file and surpress its OSError. 

    Parameters
    ----------
    file : string
        the name of the removed file.

    Returns
    -------
    None.

    '''
    try:
        os.remove(file)
    except OSError:
        pass

def util_remove_extension(self, dirname, extension):
    '''
    In the specific directory, remove the files with a particular file extension. For example, *.gz

    Parameters
    ----------
    dirname : string
        the directory which contains the files to be removed.
    extension : string
        the specific file extension group.

    Returns
    -------
    None.

    '''
    # dirname can be either absolute or relative.
    # ex. $rm -f *.gz
    ls = os.listdir(dirname)
    for i in ls:
        if i.endswith(extension):
            self.util_remove_file(os.path.join(dirname, i))

def util_copy_extension(self, src_dir, dst_dir, extension):
    '''
    In a directory, copy files with a perticular extension to a destination directory.

    Parameters
    ----------
    src_dir : string
       the source, copied-from directory.
    dst_dir : string
        the destination, copied-to directory
    extension : string
        the specific file extension group.

    Returns
    -------
    None.

    '''
    # src_dir can be either absolute or relative.
    # ex. $cp -f $NEMS/oml/$OMLVERS/*.dll .
    #ls = os.listdir(src_dir)
    for i in os.listdir(src_dir):
        if i.endswith(extension):
            shutil.copy(os.path.join(src_dir,i), os.path.join(dst_dir,i))


def util_append_content(self, file, content):
    '''
    Open a file and append content with UTF-8 encoding.

    Parameters
    ----------
    file : string
        the file to be writen in the content.
    content : string
        the content string with line break etc.

    Returns
    -------
    None.

    '''
    # append or new a file with the passed content
    with open(file, 'a', encoding='utf-8') as f:
        f.write(content)

def util_check_before_copy(self, src, dst):
    '''
    Safely copy. Simply compare the strings of destination and source not same, and ensure the source file exists, before copy the source to the destination.

    Parameters
    ----------
    src : string
       the source, copied-from file or directory .
    dst : string
        the destination, copied-to file or directory.

    Returns
    -------
    None.

    '''
    if src == dst:
        # shall not copy to itself
        # for debug:
        #print(f'Not copy - The source file or directory {src} and distination file or directory {dst} must differ')
        return

    if exists(src):
        if os.path.isdir(src):
            shutil.copytree(src,dst)
        else:
            shutil.copy(src,dst)
    else:
        return

def util_convert_py_path_to_ksh_path(self, file):
    '''
    Open a file and convert the file paths in the content from KornShell format to Python path format.

    Parameters
    ----------
    file : TYPE
        the name and path of the file to replace KornShell path format to Python path format.

    Returns
    -------
    None.

    '''
    with open(file, 'r',encoding='utf-8') as f :
      content = f.read()
    
    content = content.replace('\\', '/')

    with open(file, 'w', encoding='utf-8') as f:
      f.write(content)

def util_debugging_dump_dict(self, dict_obj, file):
    '''
    A debugger uitiliy to dump out a specified dictionary to a specified file for troubleshoot.

    Parameters
    ----------
    dict_obj : dict
        the dictionary you want to dump out
    file :string
        the file with an absoulte file path

    Returns
    -------
    None.

    '''
    if type(dict_obj) is not dict:
        print(f'{log_prefix}'+'not a dictionary. Exit out this debugger')
        return
    
    resultList = list(dict_obj.items())
    with open(file, 'w+', encoding='utf-8') as f:
        for k,v in resultList:
            f.write(f'{k}={v}\n')

def util_trim_right_space(self, src, dst):
    '''
    Read in a source file, right trim each line and write in the destination file.

    Parameters
    ----------
    src : string
       the source, read-in searched file.
    dst : string
        the destination, written-out trimmed result file.

    Returns
    -------
    None.

    '''    
    lines = []
    with open(src,'r') as f:
        lines = f.readlines()
    
    outlines = [line.rstrip() for line in lines]
    with open(dst,'w+') as f:
        for line in outlines:
            f.write(line+'\n')
