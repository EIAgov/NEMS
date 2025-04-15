# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""

def fill_table_base_128(dfd, table_spec, table_id):
    """Fill table for petroleum product Import Volumes
   
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
   
    NUMREP=18    
    MNUMPR=dfd['MNUMPR_rwpre']
  
    for PADD in range (1,int(MNUMPR)+1):     
       z[(PADD-1)*NUMREP+ 1] = dfd['RFIPQAS'].loc[PADD,2]
       z[(PADD-1)*NUMREP+ 2] = dfd['RFIPQAG'].loc[PADD,2]
       z[(PADD-1)*NUMREP+ 3] = dfd['RFIPQCG'].loc[PADD,2]
       z[(PADD-1)*NUMREP+ 4] = dfd['RFIPQMG'].loc[PADD,2] + dfd['RFIPQCBOB'].loc[PADD,2]
       z[(PADD-1)*NUMREP+ 5] = dfd['RFIPQCD'].loc[PADD,2]
       z[(PADD-1)*NUMREP+ 6] = dfd['RFIPQDL'].loc[PADD,2]
       z[(PADD-1)*NUMREP+ 7] = dfd['RFIPQDU'].loc[PADD,2]
       z[(PADD-1)*NUMREP+ 8] = dfd['RFIPQJF'].loc[PADD,2]
       z[(PADD-1)*NUMREP+ 9] = dfd['RFIPQPR'].loc[PADD,2]+dfd['RFIPQPY'].loc[PADD,2]+dfd['RFIPQPP'].loc[PADD,2]+dfd['RFIPQET'].loc[PADD,2]+dfd['RFIPQBU'].loc[PADD,2]+dfd['RFIPQIS'].loc[PADD,2]
       z[(PADD-1)*NUMREP+ 10] = dfd['RFIPQLU'].loc[PADD,2]
       z[(PADD-1)*NUMREP+ 11] = dfd['RFIPQDS'].loc[PADD,2]
       z[(PADD-1)*NUMREP+ 12] = dfd['RFIPQRL'].loc[PADD,2]
       z[(PADD-1)*NUMREP+ 13] = dfd['RFIPQRH'].loc[PADD,2]
       z[(PADD-1)*NUMREP+ 14] = dfd['RFIPQPF'].loc[PADD,2]
       z[(PADD-1)*NUMREP+ 15] = dfd['RFIPQPC'].loc[PADD,2]
       z[(PADD-1)*NUMREP+ 16] = dfd['RFIPQRG'].loc[PADD,2]+ dfd['RFIPQRBOB'].loc[PADD,2] 
       z[(PADD-1)*NUMREP+ 17] = dfd['RFPQUFC'].loc[PADD,2]*1000   
       z[(PADD-1)*NUMREP+ 18] =  z[(PADD-1)*NUMREP+ 1]+ z[(PADD-1)*NUMREP+ 2] + z[(PADD-1)*NUMREP+ 3]  + z[(PADD-1)*NUMREP+ 4]+ z[(PADD-1)*NUMREP+ 5]+ z[(PADD-1)*NUMREP+ 6]+ z[(PADD-1)*NUMREP+ 7]+ z[(PADD-1)*NUMREP+ 8]+ z[(PADD-1)*NUMREP+ 9]+ z[(PADD-1)*NUMREP+ 10] + z[(PADD-1)*NUMREP+ 11]  + z[(PADD-1)*NUMREP+ 12]+ z[(PADD-1)*NUMREP+ 13]+ z[(PADD-1)*NUMREP+ 14]+ z[(PADD-1)*NUMREP+ 15]+ z[(PADD-1)*NUMREP+ 16]+ z[(PADD-1)*NUMREP+ 17]
       #z[181:190] = dfd['RFQEXPRDT'].loc[1:10] * 1000.
       z[181] = dfd['RFQEXPRDT'].loc[1] * 1000.
       z[182] = dfd['RFQEXPRDT'].loc[2] * 1000.
       z[183] = dfd['RFQEXPRDT'].loc[3] * 1000.
       z[184] = dfd['RFQEXPRDT'].loc[4] * 1000.
       z[185] = dfd['RFQEXPRDT'].loc[5] * 1000.
       z[186] = dfd['RFQEXPRDT'].loc[6] * 1000.
       z[187] = dfd['RFQEXPRDT'].loc[7] * 1000.
       z[188] = dfd['RFQEXPRDT'].loc[8] * 1000.
       z[189] = dfd['RFQEXPRDT'].loc[9] * 1000.
       z[190] = dfd['RFQEXPRDT'].loc[10] * 1000.

       
   
    return z
       