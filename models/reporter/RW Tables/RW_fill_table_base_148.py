# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""

def fill_table_base_148(dfd, table_spec, table_id):
    """Fill table for Aircraft Stock
   
    The function returns a dict in which each key is an integer
    references a table row, and each value is a dataframe indexed
    by region number. The integer keys are the same as "irow" in
    the old layin.xls / ftab.f.
 
    Parameters
    ----------
    dfd : dict of restart variables
        key = variable name
        value = pandas series of variable values
 
    Returns
    -------
    dict
        dict of dataframes, with integers as keys. The dict values
        are dataframes indexed by region number.
    
    
       
    """
    
    z = {}


    #   Aircraft Stock
    #   Total Stock
    j=0
    for i in range(194,262):
        j=j+1
        z[j]=dfd['AIROUT'].loc[i]
        
    #   Aircraft Active Stock
    for i in range(262,330):
        j=j+1
        z[j]=dfd['AIROUT'].loc[i]

    #   Aircraft Parked Stock                                       
    for i in range(330,398):
        j=j+1
        z[j]=dfd['AIROUT'].loc[i]

    #   Aircraft Cargo Stock                                        
    for i in range(398,415):
        j=j+1
        z[j]=dfd['AIROUT'].loc[i]
    
    return z
 
