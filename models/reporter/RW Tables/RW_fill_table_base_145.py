# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""

def fill_table_base_145(dfd, table_spec, table_id):
    """Fill table for   GDP Composition and Other Assorted Macro Topics
   
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


    #   GDP Composition and Other Assorted Macro Topics
    #    Composition                                                
    #                                                               
    #    Composition of GDP                                         
    #    (billion chain-weighted m__m $)                            
    #T145(1:102,IY,IS)=MC_DETAIL(1:102,IY)
    for x in range (1,103):
       z[x] = dfd['MC_DETAIL'].loc[x]
    
    return z
 

