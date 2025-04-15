# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_098(dfd, table_spec, table_id):
    """Fill table World Total Coal Flows By Importing Regions and Exporting Countries
   
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

    #   World Total Coal Flows By Importing Regions and Exporting Countries
    #   (million short tons)
    #    Import and Export Regions 1/                               
    #                                                               
    #   Total Coal Exports to Europe/Other 2/                       
    #T98(III+33,IY,IS)=WTCF(4,III,IY)II=1,11
    z[11] = dfd['WTCF'].loc[1].loc[11]
    #     Australia                                                 
    #T98(III+33,IY,IS)=WTCF(4,III,IY)
    z[1] = dfd['WTCF'].loc[1].loc[1]
    #     United States                                             
    #T98(III+33,IY,IS)=WTCF(4,III,IY)
    z[2] =  dfd['WTCF'].loc[1].loc[2]
    #     Southern Africa 3/                                        
    #T98(III+33,IY,IS)=WTCF(4,III,IY)
    z[3] =  dfd['WTCF'].loc[1].loc[3]
    #     Eurasia 4/                                                
    #T98(III+33,IY,IS)=WTCF(4,III,IY)
    z[4] =  dfd['WTCF'].loc[1].loc[4]
    #     Poland                                                    
    #T98(III+33,IY,IS)=WTCF(4,III,IY)
    z[5] =  dfd['WTCF'].loc[1].loc[5]
    #     Canada                                                    
    #T98(III+33,IY,IS)=WTCF(4,III,IY)
    z[6] =  dfd['WTCF'].loc[1].loc[6]
    #     China                                                     
    #T98(III+33,IY,IS)=WTCF(4,III,IY)
    z[7] =  dfd['WTCF'].loc[1].loc[7]
    #     South America                                             
    #T98(III+33,IY,IS)=WTCF(4,III,IY)
    z[8] =  dfd['WTCF'].loc[1].loc[8]
    #     Vietnam and North Korea                                   
    #T98(III+33,IY,IS)=WTCF(4,III,IY)
    z[9] =  dfd['WTCF'].loc[1].loc[9]
    #     Indonesia                                                 
    #T98(III+33,IY,IS)=WTCF(4,III,IY)
    z[10] =  dfd['WTCF'].loc[1].loc[10]
    #                                                               
    #   Total Coal Exports to Asia 5/                               
    #T98(III+33,IY,IS)=WTCF(4,III,IY)
    z[22] =  dfd['WTCF'].loc[2].loc[11]
    #     Australia                                                 
    #T98(III+33,IY,IS)=WTCF(4,III,IY)
    z[12] = dfd['WTCF'].loc[2].loc[1]
    #     United States                                             
    #T98(III+33,IY,IS)=WTCF(4,III,IY)
    z[13] = dfd['WTCF'].loc[2].loc[2]
    #     Southern Africa 3/                                        
    #T98(III+33,IY,IS)=WTCF(4,III,IY)
    z[14] = dfd['WTCF'].loc[2].loc[3]
    #     Eurasia 4/                                                
    #T98(III+33,IY,IS)=WTCF(4,III,IY)
    z[15] = dfd['WTCF'].loc[2].loc[4]
    #     Poland                                                    
    #T98(III+33,IY,IS)=WTCF(4,III,IY)
    z[16] = dfd['WTCF'].loc[2].loc[5]
    #     Canada                                                    
    #T98(III+33,IY,IS)=WTCF(4,III,IY)
    z[17] = dfd['WTCF'].loc[2].loc[6]
    #     China                                                     
    #T98(III+33,IY,IS)=WTCF(4,III,IY)
    z[18] = dfd['WTCF'].loc[2].loc[7]
    #     South America                                             
    #T98(III+33,IY,IS)=WTCF(4,III,IY)
    z[19] = dfd['WTCF'].loc[2].loc[8]
    #     Vietnam and North Korea                                   
    #T98(III+33,IY,IS)=WTCF(4,III,IY)
    z[20] = dfd['WTCF'].loc[2].loc[9]
    #     Indonesia                                                 
    #T98(III+33,IY,IS)=WTCF(4,III,IY)
    z[21] = dfd['WTCF'].loc[2].loc[10]                             
    #   Total Coal Exports to America                               
    #T98(III+33,IY,IS)=WTCF(4,III,IY)
    z[33] = dfd['WTCF'].loc[3].loc[11]
    #     Australia                                                 
    #T98(III+33,IY,IS)=WTCF(4,III,IY)
    z[23] = dfd['WTCF'].loc[3].loc[1]
    #     United States                                             
    #T98(III+33,IY,IS)=WTCF(4,III,IY)
    z[24] = dfd['WTCF'].loc[3].loc[2]
    #     Southern Africa 3/                                        
    #T98(III+33,IY,IS)=WTCF(4,III,IY)
    z[25] = dfd['WTCF'].loc[3].loc[3]
    #     Eurasia 4/                                                
    #T98(III+33,IY,IS)=WTCF(4,III,IY)
    z[26] = dfd['WTCF'].loc[3].loc[4]
    #     Poland                                                    
    #T98(III+33,IY,IS)=WTCF(4,III,IY)
    z[27] = dfd['WTCF'].loc[3].loc[5]
    #     Canada                                                    
    #T98(III+33,IY,IS)=WTCF(4,III,IY)
    z[28] = dfd['WTCF'].loc[3].loc[6]
    #     China                                                     
    #T98(III+33,IY,IS)=WTCF(4,III,IY)
    z[29] = dfd['WTCF'].loc[3].loc[7]
    #     South America                                             
    #T98(III+33,IY,IS)=WTCF(4,III,IY)
    z[30] = dfd['WTCF'].loc[3].loc[8]
    #     Vietnam and North Korea                                   
    #T98(III+33,IY,IS)=WTCF(4,III,IY)
    z[31] = dfd['WTCF'].loc[3].loc[9]
    #     Indonesia                                                 
    #T98(III+33,IY,IS)=WTCF(4,III,IY)
    z[32] = dfd['WTCF'].loc[3].loc[10]
    #                                                               
    #   Total Coal Exports                                          
    #T98(III+33,IY,IS)=WTCF(4,III,IY)
    z[44] = dfd['WTCF'].loc[4].loc[11]
    #     Australia                                                 
    #T98(III+33,IY,IS)=WTCF(4,III,IY)
    z[34] = dfd['WTCF'].loc[4].loc[1]
    #     United States                                             
    #T98(III+33,IY,IS)=WTCF(4,III,IY)
    z[35] = dfd['WTCF'].loc[4].loc[2]
    #     Southern Africa 3/                                        
    #T98(III+33,IY,IS)=WTCF(4,III,IY)
    z[36] = dfd['WTCF'].loc[4].loc[3]
    #     Eurasia 4/                                                
    #T98(III+33,IY,IS)=WTCF(4,III,IY)
    z[37] = dfd['WTCF'].loc[4].loc[4]
    #     Poland                                                    
    #T98(III+33,IY,IS)=WTCF(4,III,IY)
    z[38] = dfd['WTCF'].loc[4].loc[5]
    #     Canada                                                    
    #T98(III+33,IY,IS)=WTCF(4,III,IY)
    z[39] = dfd['WTCF'].loc[4].loc[6]
    #     China                                                     
    #T98(III+33,IY,IS)=WTCF(4,III,IY)
    z[40] = dfd['WTCF'].loc[4].loc[7]
    #     South America                                             
    #T98(III+33,IY,IS)=WTCF(4,III,IY)
    z[41] = dfd['WTCF'].loc[4].loc[8]
    #     Vietnam and North Korea                                   
    #T98(III+33,IY,IS)=WTCF(4,III,IY)
    z[42] = dfd['WTCF'].loc[4].loc[9] 
    #     Indonesia                                                 
    #T98(III+33,IY,IS)=WTCF(4,III,IY)
    z[43] =  dfd['WTCF'].loc[4].loc[10]

    return z                                                               
