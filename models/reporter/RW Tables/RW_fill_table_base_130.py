# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""

def fill_table_base_130(dfd, table_spec, table_id):
    """Fill table for crude oil Export Volumes
   
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
   
    NUMREP=12    
    MNUMPR=dfd['MNUMPR_rwpre']
  
    for PADD in range (1,int(MNUMPR)+1):     
       z[(PADD-1)*NUMREP+ 1] = dfd['Q_CRUDE_EXPORTS'].loc[PADD,1]
       z[(PADD-1)*NUMREP+ 2] = dfd['Q_CRUDE_EXPORTS'].loc[PADD,2]
       z[(PADD-1)*NUMREP+ 3] = dfd['Q_CRUDE_EXPORTS'].loc[PADD,3]
       z[(PADD-1)*NUMREP+ 4] = dfd['Q_CRUDE_EXPORTS'].loc[PADD,4]
       z[(PADD-1)*NUMREP+ 5] = dfd['Q_CRUDE_EXPORTS'].loc[PADD,5]
       z[(PADD-1)*NUMREP+ 6] = dfd['Q_CRUDE_EXPORTS'].loc[PADD,6]
       z[(PADD-1)*NUMREP+ 7] = dfd['Q_CRUDE_EXPORTS'].loc[PADD,7]
       z[(PADD-1)*NUMREP+ 8] = dfd['Q_CRUDE_EXPORTS'].loc[PADD,8]
       z[(PADD-1)*NUMREP+ 9] = dfd['Q_CRUDE_EXPORTS'].loc[PADD,9]
       z[(PADD-1)*NUMREP+ 10] = dfd['Q_CRUDE_EXPORTS'].loc[PADD,10]
       z[(PADD-1)*NUMREP+ 11] = dfd['Q_CRUDE_EXPORTS'].loc[PADD,11]
       z[(PADD-1)*NUMREP+ 12] = dfd['Q_CRUDE_EXPORTS'].loc[PADD,1:11,:].sum()
       
# ! PADD Totals for II, III, and V:
    z[121] = dfd['Q_CRUDE_EXPORTS'].loc[2, 1] + dfd['Q_CRUDE_EXPORTS'].loc[3, 1]
    z[122] = dfd['Q_CRUDE_EXPORTS'].loc[2, 2] + dfd['Q_CRUDE_EXPORTS'].loc[3, 2]
    z[123] = dfd['Q_CRUDE_EXPORTS'].loc[2, 3] + dfd['Q_CRUDE_EXPORTS'].loc[3, 3]
    z[124] = dfd['Q_CRUDE_EXPORTS'].loc[2, 4] + dfd['Q_CRUDE_EXPORTS'].loc[3, 4]
    z[125] = dfd['Q_CRUDE_EXPORTS'].loc[2, 5] + dfd['Q_CRUDE_EXPORTS'].loc[3, 5]
    z[126] = dfd['Q_CRUDE_EXPORTS'].loc[2, 6] + dfd['Q_CRUDE_EXPORTS'].loc[3, 6]
    z[127] = dfd['Q_CRUDE_EXPORTS'].loc[2, 7] + dfd['Q_CRUDE_EXPORTS'].loc[3, 7]
    z[128] = dfd['Q_CRUDE_EXPORTS'].loc[2, 8] + dfd['Q_CRUDE_EXPORTS'].loc[3, 8]
    z[129] = dfd['Q_CRUDE_EXPORTS'].loc[2, 9] + dfd['Q_CRUDE_EXPORTS'].loc[3, 9]
    z[130] = dfd['Q_CRUDE_EXPORTS'].loc[2, 10] + dfd['Q_CRUDE_EXPORTS'].loc[3,10]
    z[131] = dfd['Q_CRUDE_EXPORTS'].loc[2, 11] + dfd['Q_CRUDE_EXPORTS'].loc[3,11]
    z[132] = z[121]+z[122]+z[123]+z[124]+z[125]+ z[126]+z[127]+z[128]+z[129]+z[130]+z[131]
    z[133] = dfd['Q_CRUDE_EXPORTS'].loc[4, 1] + dfd['Q_CRUDE_EXPORTS'].loc[5, 1]
    z[134] = dfd['Q_CRUDE_EXPORTS'].loc[4, 2] + dfd['Q_CRUDE_EXPORTS'].loc[5, 2]
    z[135] = dfd['Q_CRUDE_EXPORTS'].loc[4, 3] + dfd['Q_CRUDE_EXPORTS'].loc[5, 3]
    z[136] = dfd['Q_CRUDE_EXPORTS'].loc[4, 4] + dfd['Q_CRUDE_EXPORTS'].loc[5, 4]
    z[137] = dfd['Q_CRUDE_EXPORTS'].loc[4, 5] + dfd['Q_CRUDE_EXPORTS'].loc[5, 5]
    z[138] = dfd['Q_CRUDE_EXPORTS'].loc[4, 6] + dfd['Q_CRUDE_EXPORTS'].loc[5, 6]
    z[139] = dfd['Q_CRUDE_EXPORTS'].loc[4, 7] + dfd['Q_CRUDE_EXPORTS'].loc[5, 7]
    z[140] = dfd['Q_CRUDE_EXPORTS'].loc[4, 8] + dfd['Q_CRUDE_EXPORTS'].loc[5, 8]
    z[141] = dfd['Q_CRUDE_EXPORTS'].loc[4, 9] + dfd['Q_CRUDE_EXPORTS'].loc[5, 9]
    z[142] = dfd['Q_CRUDE_EXPORTS'].loc[4, 10] + dfd['Q_CRUDE_EXPORTS'].loc[5,10]
    z[143] = dfd['Q_CRUDE_EXPORTS'].loc[4, 11] + dfd['Q_CRUDE_EXPORTS'].loc[5,11]
    z[144] = z[133]+z[134]+z[135]+z[136]+z[137]+ z[138]+z[139]+z[140]+z[141]+z[142]+z[143]
    z[145] = dfd['Q_CRUDE_EXPORTS'].loc[7, 1] + dfd['Q_CRUDE_EXPORTS'].loc[8, 1]
    z[146] = dfd['Q_CRUDE_EXPORTS'].loc[7, 2] + dfd['Q_CRUDE_EXPORTS'].loc[8, 2]
    z[147] = dfd['Q_CRUDE_EXPORTS'].loc[7, 3] + dfd['Q_CRUDE_EXPORTS'].loc[8, 3]
    z[148] = dfd['Q_CRUDE_EXPORTS'].loc[7, 4] + dfd['Q_CRUDE_EXPORTS'].loc[8, 4]
    z[149] = dfd['Q_CRUDE_EXPORTS'].loc[7, 5] + dfd['Q_CRUDE_EXPORTS'].loc[8, 5]
    z[150] = dfd['Q_CRUDE_EXPORTS'].loc[7, 6] + dfd['Q_CRUDE_EXPORTS'].loc[8, 6]
    z[151] = dfd['Q_CRUDE_EXPORTS'].loc[7, 7] + dfd['Q_CRUDE_EXPORTS'].loc[8, 7]
    z[152] = dfd['Q_CRUDE_EXPORTS'].loc[7, 8] + dfd['Q_CRUDE_EXPORTS'].loc[8, 8]
    z[153] = dfd['Q_CRUDE_EXPORTS'].loc[7, 9] + dfd['Q_CRUDE_EXPORTS'].loc[8, 9]
    z[154] = dfd['Q_CRUDE_EXPORTS'].loc[7, 10] + dfd['Q_CRUDE_EXPORTS'].loc[8,10]
    z[155] = dfd['Q_CRUDE_EXPORTS'].loc[7, 11] + dfd['Q_CRUDE_EXPORTS'].loc[8,11]
    z[156] = z[145]+z[146]+z[147]+z[148]+z[149]+ z[150]+z[151]+z[152]+z[153]+z[154]+z[155] 
   
    return z
       