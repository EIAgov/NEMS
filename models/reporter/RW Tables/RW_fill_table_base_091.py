# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_091(dfd, table_spec, table_id):
    """Fill table Primary Natural Gas Capacity Entering Natural Gas Market Module Region from Neighboring Regions
   
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

    #   Primary Natural Gas Capacity Entering Natural Gas Market Module Region from Neighboring Regions
    #   (billion cubic feet per year)
    #   Regional Flow Capacity 1/                                   
    #                                                               
    #   Into New England from:                                      
    #     Mid-Atlantic and Ohio                                     
    #T91(1,IY,IS)=NGCAPS(1,1,IY)
    z[1] = dfd['NGCAPS'].loc[1].loc[1]
    #     Canada                                                    
    #T91(2,IY,IS)=NGCAPS(1,2,IY)
    z[2] = dfd['NGCAPS'].loc[1].loc[2]
    #                                                               
    #   Into Mid-Atlantic and Ohio from:                            
    #     Eastern Midwest                                           
    #T91(3,IY,IS)=NGCAPS(2,1,IY)
    z[3] = dfd['NGCAPS'].loc[2].loc[1]
    #     Southeast                                                 
    #T91(4,IY,IS)=NGCAPS(2,2,IY)
    z[4] = dfd['NGCAPS'].loc[2].loc[2]
    #     Canada                                                    
    #T91(5,IY,IS)=NGCAPS(2,3,IY)
    z[5] = dfd['NGCAPS'].loc[2].loc[3]
    #                                                               
    #   Into Eastern Midwest from:                                  
    #     Mid-Atlantic and Ohio                                     
    #T91(6,IY,IS)=NGCAPS(3,1,IY)
    z[6] = dfd['NGCAPS'].loc[3].loc[1]
    #     South Central                                             
    #T91(7,IY,IS)=NGCAPS(3,2,IY)
    z[7] = dfd['NGCAPS'].loc[3].loc[2]
    #     Rocky Mountains-Great Plains                              
    #T91(8,IY,IS)=NGCAPS(3,3,IY)
    z[8] = dfd['NGCAPS'].loc[3].loc[3]
    #     Northern Great Plains                                     
    #T91(9,IY,IS)=NGCAPS(3,4,IY)
    z[9] = dfd['NGCAPS'].loc[3].loc[4]
    #     Canada                                                    
    #T91(10,IY,IS)=NGCAPS(3,5,IY)
    z[10] = dfd['NGCAPS'].loc[3].loc[5]
    #                                                               
    #   Into Southeast from:                                        
    #     Mid-Atlantic and Ohio                                     
    #T91(11,IY,IS)=NGCAPS(4,1,IY)
    z[11] = dfd['NGCAPS'].loc[4].loc[1]

    #     Eastern Midwest                                           
    #T91(12,IY,IS)=NGCAPS(4,2,IY)
    z[12] = dfd['NGCAPS'].loc[4].loc[2]
    #     South Central                                             
    #T91(13,IY,IS)=NGCAPS(4,3,IY)
    z[13] = dfd['NGCAPS'].loc[4].loc[3]
    #                                                               
    #   Into Florida from:                                          
    #     Southeast                                                 
    #T91(14,IY,IS)=NGCAPS(5,1,IY)
    z[14] = dfd['NGCAPS'].loc[5].loc[1]
    #     South Central                                             
    #T91(15,IY,IS)=NGCAPS(5,2,IY)
    z[15] = dfd['NGCAPS'].loc[5].loc[2]
    #                                                               
    #   Into South Central from:                                    
    #     Eastern Midwest                                           
    #T91(16,IY,IS)=NGCAPS(6,1,IY)
    z[16] = dfd['NGCAPS'].loc[6].loc[1]
    #     Southeast                                                 
    #T91(17,IY,IS)=NGCAPS(6,2,IY)
    z[17] = dfd['NGCAPS'].loc[6].loc[2]
    #     Rocky Mountains-Great Plains                              
    #T91(18,IY,IS)=NGCAPS(6,3,IY)
    z[18] = dfd['NGCAPS'].loc[6].loc[3]
    #     Arizona and New Mexico                                    
    #T91(19,IY,IS)=NGCAPS(6,4,IY)
    z[19] = dfd['NGCAPS'].loc[6].loc[4]
    #                                                               
    #   Into Rocky Mountains-Great Plains from:                     
    #     Eastern Midwest                                           
    #T91(20,IY,IS)=NGCAPS(7,1,IY)
    z[20] = dfd['NGCAPS'].loc[7].loc[1]
    #     South Central                                             
    #T91(21,IY,IS)=NGCAPS(7,2,IY)
    z[21] = dfd['NGCAPS'].loc[7].loc[2]
    #     Northern Great Plains                                     
    #T91(22,IY,IS)=NGCAPS(7,3,IY)
    z[22] = dfd['NGCAPS'].loc[7].loc[3]
    #     Arizona and New Mexico                                    
    #T91(23,IY,IS)=NGCAPS(7,4,IY)
    z[23] = dfd['NGCAPS'].loc[7].loc[4]
    #     Oregon and Washington                                     
    #T91(24,IY,IS)=NGCAPS(7,5,IY)
    z[24] = dfd['NGCAPS'].loc[7].loc[5]
    #                                                               
    #   Into Northern Great Plains from:                            
    #     Eastern Midwest                                           
    #T91(25,IY,IS)=NGCAPS(8,1,IY)
    z[25] = dfd['NGCAPS'].loc[8].loc[1]
    #     Rocky Mountains-Great Plains                              
    #T91(26,IY,IS)=NGCAPS(8,2,IY)
    z[26] = dfd['NGCAPS'].loc[8].loc[2]
    #     Canada                                                    
    #T91(27,IY,IS)=NGCAPS(8,3,IY)
    z[27] = dfd['NGCAPS'].loc[8].loc[3]
    #                                                               
    #   Into Arizona and New Mexico from:                           
    #     South Central                                             
    #T91(28,IY,IS)=NGCAPS(9,1,IY)
    z[28] = dfd['NGCAPS'].loc[9].loc[1]
    #     Rocky Mountains-Great Plains                              
    #T91(29,IY,IS)=NGCAPS(9,2,IY)
    z[29] = dfd['NGCAPS'].loc[9].loc[2]
    #                                                               
    #   Into California from:                                       
    #     Oregon and Washington                                     
    #T91(30,IY,IS)=NGCAPS(10,1,IY)
    z[30] = dfd['NGCAPS'].loc[10].loc[1]
    #     Rocky Mountains-Great Plains                              
    #T91(31,IY,IS)=NGCAPS(10,2,IY)
    z[31] = dfd['NGCAPS'].loc[10].loc[2]
    #     Arizona and New Mexico                                    
    #T91(32,IY,IS)=NGCAPS(10,3,IY)
    z[32] = dfd['NGCAPS'].loc[10].loc[3]
    #                                                               
    #   Into Oregon and Washington from:                            
    #     Rocky Mountains-Great Plains                              
    #T91(33,IY,IS)=NGCAPS(11,1,IY)
    z[33] = dfd['NGCAPS'].loc[11].loc[1]
    #     Canada (into Washington)                                  
    #T91(34,IY,IS)=NGCAPS(11,2,IY)
    z[34] = dfd['NGCAPS'].loc[11].loc[2]
    #     Canada (through Idaho)                                    
    #T91(35,IY,IS)=NGCAPS(11,3,IY)
    z[35] = dfd['NGCAPS'].loc[11].loc[3]

    return z