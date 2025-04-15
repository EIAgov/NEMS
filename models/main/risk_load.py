# -*- coding: utf-8 -*-
"""
Created on Feb 22 2024

@author: Claire Su
"""
import os
import sys
import pandas as pd

sys.path.append(os.getcwd() + '\\pyfiler1')

def main(user, pyfiler):
    """The main entry point to execute this risk_load module. It calls load_mmrisk() to load the risk file 'mnrisk_processed.csv'
    and write data to the specified pyfiler.

    Parameters
    ----------
    user : SimpleNamespace
        user class object, containing dict with NEMS runtime information
    pyfiler : module
        pyfiler fortran module

    Returns
    -------
    None.

    """    
    file=user.SCEDES['MNRISKN']
    df=load_mnrisk(file)
    write_to_restart(pyfiler,df)

def load_mnrisk(file):
    """Read in the specified risk file, parse into a dataframe.
    If the file does not exit, print out unable to open the file message and return.

    Parameters
    ----------
    file : str
        the 'risk' file path

    Returns
    -------
    DataFrame
        the dataframe parsed from the risk file
    
    """    
    if not os.path.isfile(file):
        print(' Unable to open the risk premiums data file,')
        print(' so the global data already read from the restart file will not be changed.')
        return

    df=pd.read_csv(file)
    df[df.columns[1:]]=df[df.columns[1:]].astype(float)
    df=df.fillna(0.0)    
    return df

def write_to_restart(pyfiler,df):
    """Retrieve the value list of EXTRARISK and write to the pyfiler.emssion.extrarisk

    Parameters
    ----------
    pyfiler : module
        pyfiler fortran module

    df : DataFrame
        the dataframe parsed from the SEDS file

    Returns
    -------
    module
        pyfiler fortran module

    """
    get_row=df.loc[df['VARNAME'] == 'EXTRARISK']
    ls=get_row.values.flatten().tolist()
    pyfiler.emission.extrarisk=ls[1:]
    return pyfiler

if __name__ == '__main__':
    print("Please load from the main program script. Exit now...")
    os.sys.exit()