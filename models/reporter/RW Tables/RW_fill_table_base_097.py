# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_097(dfd, table_spec, table_id):
    """Fill table World Metallurgical Coal Flows By Importing Regions and Exporting Countries
   
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

    #   World Metallurgical Coal Flows By Importing Regions and Exporting Countries
    #   (million short tons)
    #    Import and Export Regions 1/                               
    #                                                               
    #   Metallurgical Coal Exports to Europe/Other 2/               
    #T97(III+33,IY,IS)=WMCF(4,III,IY)
    z[11] = dfd['WMCF'].loc[1].loc[11]
    #     Australia                                                 
    #T97(III+33,IY,IS)=WMCF(4,III,IY)
    z[1] = dfd['WMCF'].loc[1].loc[1]
    #     United States                                             
    #T97(III+33,IY,IS)=WMCF(4,III,IY)
    z[2] =  dfd['WMCF'].loc[1].loc[2]
    #     Southern Africa 3/                                        
    #T97(III+33,IY,IS)=WMCF(4,III,IY)
    z[3] =  dfd['WMCF'].loc[1].loc[3]
    #     Eurasia 4/                                                
    #T97(III+33,IY,IS)=WMCF(4,III,IY)
    z[4] =  dfd['WMCF'].loc[1].loc[4]
    #     Poland                                                    
    #T97(III+33,IY,IS)=WMCF(4,III,IY)
    z[5] =  dfd['WMCF'].loc[1].loc[5]
    #     Canada                                                    
    #T97(III+33,IY,IS)=WMCF(4,III,IY)
    z[6] =  dfd['WMCF'].loc[1].loc[6]
    #     China                                                     
    #T97(III+33,IY,IS)=WMCF(4,III,IY)
    z[7] =  dfd['WMCF'].loc[1].loc[7]
    #     South America                                             
    #T97(III+33,IY,IS)=WMCF(4,III,IY)
    z[8] =  dfd['WMCF'].loc[1].loc[8]
    #     Vietnam and North Korea                                   
    #T97(III+33,IY,IS)=WMCF(4,III,IY)
    z[9] =  dfd['WMCF'].loc[1].loc[9]
    #     Indonesia                                                 
    #T97(III+33,IY,IS)=WMCF(4,III,IY)
    z[10] =  dfd['WMCF'].loc[1].loc[10]                            
    #   Metallurgical Coal Exports to Asia 5/                       
    #T97(III+33,IY,IS)=WMCF(4,III,IY)
    z[22] =  dfd['WMCF'].loc[2].loc[11]
    #     Australia                                                 
    #T97(III+33,IY,IS)=WMCF(4,III,IY)
    z[12] = dfd['WMCF'].loc[2].loc[1]
    #     United States                                             
    #T97(III+33,IY,IS)=WMCF(4,III,IY)
    z[13] = dfd['WMCF'].loc[2].loc[2]
    #     Southern Africa 3/                                        
    #T97(III+33,IY,IS)=WMCF(4,III,IY)
    z[14] = dfd['WMCF'].loc[2].loc[3]
    #     Eurasia 4/                                                
    #T97(III+33,IY,IS)=WMCF(4,III,IY)
    z[15] = dfd['WMCF'].loc[2].loc[4]
    #     Poland                                                    
    #T97(III+33,IY,IS)=WMCF(4,III,IY)
    z[16] = dfd['WMCF'].loc[2].loc[5]
    #     Canada                                                    
    #T97(III+33,IY,IS)=WMCF(4,III,IY)
    z[17] = dfd['WMCF'].loc[2].loc[6]
    #     China                                                     
    #T97(III+33,IY,IS)=WMCF(4,III,IY)
    z[18] = dfd['WMCF'].loc[2].loc[7]
    #     South America                                             
    #T97(III+33,IY,IS)=WMCF(4,III,IY)
    z[19] = dfd['WMCF'].loc[2].loc[8]
    #     Vietnam and North Korea                                   
    #T97(III+33,IY,IS)=WMCF(4,III,IY)
    z[20] = dfd['WMCF'].loc[2].loc[9]
    #     Indonesia                                                 
    #T97(III+33,IY,IS)=WMCF(4,III,IY)                               
    z[21] = dfd['WMCF'].loc[2].loc[10]                             
    #   Metallurgical Coal Exports to America                       
    #T97(III+33,IY,IS)=WMCF(4,III,IY)
    z[33] = dfd['WMCF'].loc[3].loc[11]
    #     Australia                                                 
    #T97(III+33,IY,IS)=WMCF(4,III,IY)
    z[23] = dfd['WMCF'].loc[3].loc[1]
    #     United States                                             
    #T97(III+33,IY,IS)=WMCF(4,III,IY)
    z[24] = dfd['WMCF'].loc[3].loc[2]
    #     Southern Africa 3/                                        
    #T97(III+33,IY,IS)=WMCF(4,III,IY)
    z[25] = dfd['WMCF'].loc[3].loc[3]
    #     Eurasia 4/                                                
    #T97(III+33,IY,IS)=WMCF(4,III,IY)
    z[26] = dfd['WMCF'].loc[3].loc[4]
    #     Poland                                                    
    #T97(III+33,IY,IS)=WMCF(4,III,IY)
    z[27] = dfd['WMCF'].loc[3].loc[5]
    #     Canada                                                    
    #T97(III+33,IY,IS)=WMCF(4,III,IY)
    z[28] = dfd['WMCF'].loc[3].loc[6]
    #     China                                                     
    #T97(III+33,IY,IS)=WMCF(4,III,IY)
    z[29] = dfd['WMCF'].loc[3].loc[7]
    #     South America                                             
    #T97(III+33,IY,IS)=WMCF(4,III,IY)
    z[30] = dfd['WMCF'].loc[3].loc[8]
    #     Vietnam and North Korea                                   
    #T97(III+33,IY,IS)=WMCF(4,III,IY)
    z[31] = dfd['WMCF'].loc[3].loc[9]
    #     Indonesia                                                 
    #T97(III+33,IY,IS)=WMCF(4,III,IY)
    z[32] = dfd['WMCF'].loc[3].loc[10]                            
    #   Total Metallurgical Coal Exports                            
    #T97(III+33,IY,IS)=WMCF(4,III,IY)
    z[44] = dfd['WMCF'].loc[4].loc[11]
    #     Australia                                                 
    #T97(III+33,IY,IS)=WMCF(4,III,IY)
    z[34] = dfd['WMCF'].loc[4].loc[1]
    #     United States                                             
    #T97(III+33,IY,IS)=WMCF(4,III,IY)
    z[35] = dfd['WMCF'].loc[4].loc[2]
    #     Southern Africa 3/                                        
    #T97(III+33,IY,IS)=WMCF(4,III,IY)
    z[36] = dfd['WMCF'].loc[4].loc[3]
    #     Eurasia 4/                                                
    #T97(III+33,IY,IS)=WMCF(4,III,IY)
    z[37] = dfd['WMCF'].loc[4].loc[4]
    #     Poland                                                    
    #T97(III+33,IY,IS)=WMCF(4,III,IY)
    z[38] = dfd['WMCF'].loc[4].loc[5]
    #     Canada                                                    
    #T97(III+33,IY,IS)=WMCF(4,III,IY)
    z[39] = dfd['WMCF'].loc[4].loc[6]
    #     China                                                     
    #T97(III+33,IY,IS)=WMCF(4,III,IY)
    z[40] = dfd['WMCF'].loc[4].loc[7]
    #     South America                                             
    #T97(III+33,IY,IS)=WMCF(4,III,IY)
    z[41] = dfd['WMCF'].loc[4].loc[8]
    #     Vietnam and North Korea                                   
    #T97(III+33,IY,IS)=WMCF(4,III,IY)
    z[42] = dfd['WMCF'].loc[4].loc[9] 
    #     Indonesia                                                 
    #T97(III+33,IY,IS)=WMCF(4,III,IY)
    z[43] =  dfd['WMCF'].loc[4].loc[10]

    return z                                                               
