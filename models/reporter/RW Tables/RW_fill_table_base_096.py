# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_096(dfd, table_spec, table_id):
    """Fill table World Steam Coal Flows By Importing Regions and Exporting Countries
   
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
    
    Note
    ----
        Several variables are assigned temporary values to get the prototype working.
        Conversions like TRIL_TO_QUAD are performed at make_SMK_fixed.
        TODO: Hard-coded lines in ftab.f will be implemented next stage.
        At some point, we will delete the commented rows with Fortran formulas.
    
    """
	
    
    z = {}

    #   World Steam Coal Flows By Importing Regions and Exporting Countries
    #   (million short tons)
    #    Import and Export Regions 1/                               
    #                                                               
    #   Steam Coal Exports to Europe/Other 2/                       
    #T96(III+33,IY,IS)=WSCF(4,III,IY)
    z[11] = dfd['WSCF'].loc[1].loc[11]
    #     Australia 
    z[1] = dfd['WSCF'].loc[1].loc[1]
    #     United States  
    z[2] = dfd['WSCF'].loc[1].loc[2]
    #     Southern Africa 3/ 
    z[3] = dfd['WSCF'].loc[1].loc[3]
    #     Eurasia 4/     
    z[4] = dfd['WSCF'].loc[1].loc[4]
    #     Poland      
    z[5] = dfd['WSCF'].loc[1].loc[5]
    #     Canada        
    z[6] = dfd['WSCF'].loc[1].loc[6]
    #     China      
    z[7] = dfd['WSCF'].loc[1].loc[7]
    #     South America
    z[8] = dfd['WSCF'].loc[1].loc[8]
    #     Vietnam and North Korea
    z[9] = dfd['WSCF'].loc[1].loc[9]
    #     Indonesia              
    z[10] = dfd['WSCF'].loc[1].loc[10]                             
    #   Steam Coal Exports to Asia 5/ 
    z[22] = dfd['WSCF'].loc[2].loc[11]
    #     Australia  
    z[12] = dfd['WSCF'].loc[2].loc[1]
    #     United States 
    z[13] = dfd['WSCF'].loc[2].loc[2]
    #     Southern Africa 3/ 
    z[14] = dfd['WSCF'].loc[2].loc[3]
    #     Eurasia 4/  
    z[15] = dfd['WSCF'].loc[2].loc[4]
    #     Poland  
    z[16] = dfd['WSCF'].loc[2].loc[5]
    #     Canada    
    z[17] = dfd['WSCF'].loc[2].loc[6]
    #     China 
    z[18] = dfd['WSCF'].loc[2].loc[7]
    #     South America 
    z[19] = dfd['WSCF'].loc[2].loc[8]
    #     Vietnam and North Korea 
    z[20] = dfd['WSCF'].loc[2].loc[9]
    #     Indonesia   
    z[21] = dfd['WSCF'].loc[2].loc[10]                              
    #   Steam Coal Exports to America
    z[33] = dfd['WSCF'].loc[3].loc[11]
    #     Australia
    z[23] = dfd['WSCF'].loc[3].loc[1]
    #     United States
    z[24] = dfd['WSCF'].loc[3].loc[2]
    #     Southern Africa
    z[25] = dfd['WSCF'].loc[3].loc[3] 
    #     Eurasia 
    z[26] =  dfd['WSCF'].loc[3].loc[4]
    #     Poland  
    z[27] = dfd['WSCF'].loc[3].loc[5] 
    #     Canada  
    z[28] = dfd['WSCF'].loc[3].loc[6]
    #     China  
    z[29] = dfd['WSCF'].loc[3].loc[7]
    #     South America 
    z[30] = dfd['WSCF'].loc[3].loc[8]
    #     Vietnam and North Korea
    z[31] = dfd['WSCF'].loc[3].loc[9]
    #     Indonesia      
    z[32] = dfd['WSCF'].loc[3].loc[10]                             
    #   Total Steam Coal Exports  
    z[44] = dfd['WSCF'].loc[4].loc[11]
    #     Australia 
    z[34] = dfd['WSCF'].loc[4].loc[1]
    #     United States 
    z[35] = dfd['WSCF'].loc[4].loc[2]
    #     Southern Africa 
    z[36] = dfd['WSCF'].loc[4].loc[3]
    #     Eurasia 
    z[37] = dfd['WSCF'].loc[4].loc[4]
    #     Poland   
    z[38] =dfd['WSCF'].loc[4].loc[5]
    #     Canada   
    z[39] = dfd['WSCF'].loc[4].loc[6]
    #     China   
    z[40] = dfd['WSCF'].loc[4].loc[7]
    #     South America  
    z[41] = dfd['WSCF'].loc[4].loc[8]
    #     Vietnam and North Korea
    z[42] = dfd['WSCF'].loc[4].loc[9]
    #     Indonesia  
    z[43] = dfd['WSCF'].loc[4].loc[10]

    return z