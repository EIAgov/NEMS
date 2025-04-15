# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_146(dfd, table_spec, table_id):
    """Fill table for  Freight Technology Penetration

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

    # Table 146 - Freight Technology Penetration

    #   Class 2b-3:  Pickup, Van, and Vocational
    #     Diesel
    for i in range(1,44):
        z[i] = dfd["TECHSHARE"].loc[i].loc[1].loc[1] * 100.0
    
    for i in range(1,28):
        z[i+43] = dfd["TECHSHARE"].loc[i+56].loc[1].loc[1] * 100.0
    
    #     Gasoline
    for i in range(1,25):
        z[i+70] = dfd["TECHSHARE"].loc[i].loc[1].loc[2] * 100.0    

    for i in range(1,42):
        z[i+94] = dfd["TECHSHARE"].loc[i+42].loc[1].loc[2] * 100.0  
    
    j = 135
    
    #   Classes 4-6:  Vocational
    for i in range(1,44):
        z[i+j] = dfd["TECHSHARE"].loc[i].loc[2].loc[1] * 100.0
    
    for i in range(1,28):
        z[i+j+43] = dfd["TECHSHARE"].loc[i+56].loc[2].loc[1] * 100.0
    
    #     Gasoline
    for i in range(1,25):
        z[i+j+70] = dfd["TECHSHARE"].loc[i].loc[2].loc[2] * 100.0    

    for i in range(1,42):
        z[i+j+94] = dfd["TECHSHARE"].loc[i+42].loc[2].loc[2] * 100.0  
    
    j = 135 * 2
        
    #   Classes 7 and 8:  Vocational
    for i in range(1,44):
        z[i+j] = dfd["TECHSHARE"].loc[i].loc[3].loc[1] * 100.0
    
    for i in range(1,28):
        z[i+j+43] = dfd["TECHSHARE"].loc[i+56].loc[3].loc[1] * 100.0
    
    j = 340

    #   Classes 7 and 8:  Tractor Day Cab
    for i in range(1,44):
        z[i+j] = dfd["TECHSHARE"].loc[i].loc[4].loc[1] * 100.0
    
    for i in range(1,28):
        z[i+j+43] = dfd["TECHSHARE"].loc[i+56].loc[4].loc[1] * 100.0
    
    j = 411    
        
    #   Classes 7 and 8:  Tractor Sleeper Cab
    for i in range(1,44):
        z[i+j] = dfd["TECHSHARE"].loc[i].loc[5].loc[1] * 100.0
    
    for i in range(1,28):
        z[i+j+43] = dfd["TECHSHARE"].loc[i+56].loc[5].loc[1] * 100.0
        
        
    return z
