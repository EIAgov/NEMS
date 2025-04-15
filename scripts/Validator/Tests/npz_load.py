# -*- coding: utf-8 -*-
"""
Created on Jan 10 2025

@author: Claire Su
"""
import numpy as np
from DataModel.Model import Model

def check_negative(n_arr):
    """Check if the passed-in n-dimension array contains a negative value

    Parameters
    ----------
    n_arr : arrary
        The passed-in Numpy arrary

    Returns
    -------
    bool
        True if there is at least one negative value in the array. False if all are equal or larger than zero.
    """
    # for debug testing:
    #n_arr[10,0] = -1

    errors = False
    if np.any(n_arr<-0.001): errors = True
    return errors

def check_key_variables(npz_data):
    """Define a list of common block variable to be checked if they contain any negative value. Return a list of variables which contain negative value.
    Per modelers' feedback:
    -QCIIN is fine with negative values, but the other -IN variables are not.
    -QBLK/qeiel - This variable can be negative.
    -the import and ALLSEC Other (MPBLK/potas, QBLK/qotas) variables can be negative.
    -for QBLK/qh2in, QBLK/qh2inpf,QBLK/qotin, prior to 2018 data is not real; skip checking. Only examinate data year 2018 and after.

    Parameters
    ----------
    npz_data : array
        A N dimension Numpy array with npz data.

    Returns
    -------
    list
        A list of variables which contain negative value.
    """
    excludes = ('MPBLK/potas',
                'QBLK/qciin',
                'QBLK/qeiel',
                'QBLK/qotas')
    
    ls = [i for i in npz_data.keys() if (i.startswith("QBLK/") or i.startswith("MPBLK/")) and i not in excludes] 
    ls.sort()

    # set up teh custom list - nothing in IDM  prior to 2018 is real
    customs = ('QBLK/qh2in',
            'QBLK/qh2inpf',
            'QBLK/qotin')

    # iterate througth the key fuel variables and check
    error_list = []
    for i in ls:
        e = False
        if i in customs:
            # if restart year from 1990-2050 (=61), skip checking IDM prior to 2018 data. Only examinate data year 2018 and after.
            if npz_data[i].shape[1] == 61: e=check_negative(npz_data[i][:,28:])
        else:
            e=check_negative(npz_data[i])
        
        if e : error_list.append(i)
    return error_list

def load_npz_data(npz_file):
    """Read in the specified .npz file and return as an N-dimension array.

    Parameters
    ----------
    npz_file : string
        The .npz file path.

    Returns
    -------
    array
        A N dimension Numpy array with npz data.
    """
    npz_data=np.load(npz_file)
    #print(npz_data['QBLK/qmgtr'])
    return npz_data

def main():
    npz_data = load_npz_data(Model.getInstance().files.npz.path)
    error_list = check_key_variables(npz_data)
    return error_list

if __name__ == "__main__":
    main()