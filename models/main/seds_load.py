# -*- coding: utf-8 -*-
"""
Created on Feb 20 2024

@author: Claire Su
"""
import os
import pandas as pd

def main(user, pyfiler):
    """The main entry point to execute this seds_load module. It calls load_qsblk() to load the SEDS file 'qsblk_processed.csv'
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
    file=user.SCEDES['QSBLKN']

    df,varname_list=load_qsblk(file)
    write_to_restart(pyfiler,df,varname_list)

def load_qsblk(file):
    """Read in the specified SEDS file, parse into a dataframe and compose a variable name list.
    If the file does not exit, print out unable to open the file message and return.

    Parameters
    ----------
    
    file : str
        the SEDS file path

    Returns
    -------
    DataFrame
        the dataframe parsed from the SEDS file
    list 
        the list of variable names listed in the SEDS file
    """
    if not os.path.isfile(file):
        print(' Unable to open the SEDS/Supplementary Global Data file,')
        print(' so the global data already read from the restart file will not be changed.')
        return

    df=pd.read_csv(file)
    varname_list=df['VARNAME'].unique()

    # set NaN as folat 0.0
    df[df.columns[3:]]=df[df.columns[3:]].astype(float)
    df=df.fillna(0.0)
    
    return df, varname_list

def write_to_restart(pyfiler,df,varname_list=[]):
    """Iterate through the variable name list, retrieve the corresponding values, and write to the pyfiler.

    Parameters
    ----------
    pyfiler : module
        pyfiler fortran module

    df : DataFrame
        the dataframe parsed from the SEDS file

    varname_list : list, optional
        the list of variable names listed in the SEDS file. Defaults to [].
        
    Returns
    -------
    None.

    """
    po=pyfiler.qsblk

    # for debug:
    #print("before, pyd 1990=",po.qselrs[0,:])

    for n in varname_list:
        l=n.lower()
        setattr(po,l,get_value(df,n))

def get_value(df,key_varname):
    """Retrieve data by variable name and iterate through 11 regions, and return a 2-dimentsion data array.

    Parameters
    ----------

    df : DataFrame
        the dataframe parsed from the SEDS file

    varname_list : str
        the variable name to look up
        
    Returns
    -------
    list
        2-dimension list of variable values

    """
    drop_columns=['VARNAME','DESCRIPTION','MNUMCR']

    # create a 2-dimension list with data appended
    arr=[]
    k=["%.2d" % i for i in range(1,12)]
    for e in k:
        d=get_value_per_mnumcr(df,key_varname,e)
        arr.append(d)
    
    return arr

def get_value_per_mnumcr(df,key_varname,key_mnumcr,drop_columns=['VARNAME','DESCRIPTION','MNUMCR']):
    """Query the SEDS dataframe by a specified variable name and MNUMCR, and get a list of the variable values. 

    Parameters
    ----------
    df : DataFrame
        the dataframe parsed from the SEDS file
    
    key_varname : str
        the variable name to look up
    
    key_mnumcr : str
        the search keyword at the 'MNUMCR' column
    
    drop_columns : list, optional
        the drop column list when do feature engeering on the dataframe. Defaults to ['VARNAME','DESCRIPTION','MNUMCR'].

    Returns
    -------
    list
        a list of the variable values
        
    """
    get_row=df.loc[(df['VARNAME'] == key_varname) & (df['MNUMCR'].str.startswith(key_mnumcr)) ]
    get_row=get_row.drop(drop_columns, axis=1)
    ls=get_row.values.flatten().tolist()

    return ls

if __name__ == '__main__':
    print("Please load from the main program script. Exit now...")
    os.sys.exit()