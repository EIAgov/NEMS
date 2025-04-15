# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_090(dfd, table_spec, table_id):
    """Fill table Primary Natural Gas Flows Entering Natural Gas Market Module Region from Neighboring Regions
   
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



    #   Primary Natural Gas Flows Entering Natural Gas Market Module Region from Neighboring Regions
    #   (billion cubic feet per year)
    #   Regional Flows 1/                                           
    #                                                               
    #   Into New England from: 2/                                   
    #     Mid-Atlantic and Ohio                                     
    #T90(1,IY,IS)=NGFLOWS(1,1,IY)
    z[1] = dfd['NGFLOWS'].loc[1].loc[1]
    #     Canada                                                    
    #T90(2,IY,IS)=NGFLOWS(1,2,IY)
    z[2] = dfd['NGFLOWS'].loc[1].loc[2]
    #                                                               
    #   Into Mid-Atlantic and Ohio from:                            
    #     Eastern Midwest                                           
    #T90(3,IY,IS)=NGFLOWS(2,1,IY)
    z[3] = dfd['NGFLOWS'].loc[2].loc[1]
    #     Southeast                                                 
    #T90(4,IY,IS)=NGFLOWS(2,2,IY)
    z[4] = dfd['NGFLOWS'].loc[2].loc[2]
    #     Canada                                                    
    #T90(5,IY,IS)=NGFLOWS(2,3,IY)
    z[5] = dfd['NGFLOWS'].loc[2].loc[3]
    #                                                               
    #   Into Eastern Midwest from:                                  
    #     Mid-Atlantic and Ohio                                     
    #T90(6,IY,IS)=NGFLOWS(3,1,IY)
    z[6] = dfd['NGFLOWS'].loc[3].loc[1]
    #     South Central                                             
    #T90(7,IY,IS)=NGFLOWS(3,2,IY)
    z[7] = dfd['NGFLOWS'].loc[3].loc[2]
    #     Rocky Mountains-Great Plains                              
    #T90(8,IY,IS)=NGFLOWS(3,3,IY)
    z[8] = dfd['NGFLOWS'].loc[3].loc[3]
    #     Northern Great Plains                                     
    #T90(9,IY,IS)=NGFLOWS(3,4,IY)
    z[9] = dfd['NGFLOWS'].loc[3].loc[4]
    #     Canada                                                    
    #T90(10,IY,IS)=NGFLOWS(3,5,IY)
    z[10] = dfd['NGFLOWS'].loc[3].loc[5]
    #                                                               
    #   Into Southeast from:                                        
    #     Mid-Atlantic and Ohio                                     
    #T90(11,IY,IS)=NGFLOWS(4,1,IY)
    z[11] = dfd['NGFLOWS'].loc[4].loc[1]

    #     Eastern Midwest                                           
    #T90(12,IY,IS)=NGFLOWS(4,2,IY)
    z[12] = dfd['NGFLOWS'].loc[4].loc[2]
    #     South Central                                             
    #T90(13,IY,IS)=NGFLOWS(4,3,IY)
    z[13] = dfd['NGFLOWS'].loc[4].loc[3]
    #                                                               
    #   Into Florida from:                                          
    #     Southeast                                                 
    #T90(14,IY,IS)=NGFLOWS(5,1,IY)
    z[14] = dfd['NGFLOWS'].loc[5].loc[1]
    #     South Central                                             
    #T90(15,IY,IS)=NGFLOWS(5,2,IY)
    z[15] = dfd['NGFLOWS'].loc[5].loc[2]
    #                                                               
    #   Into South Central from:                                    
    #     Eastern Midwest                                           
    #T90(16,IY,IS)=NGFLOWS(6,1,IY)
    z[16] = dfd['NGFLOWS'].loc[6].loc[1]
    #     Southeast                                                 
    #T90(17,IY,IS)=NGFLOWS(6,2,IY)
    z[17] = dfd['NGFLOWS'].loc[6].loc[2]
    #     Rocky Mountains-Great Plains                              
    #T90(18,IY,IS)=NGFLOWS(6,3,IY)
    z[18] = dfd['NGFLOWS'].loc[6].loc[3]
    #     Arizona and New Mexico                                    
    #T90(19,IY,IS)=NGFLOWS(6,4,IY)
    z[19] = dfd['NGFLOWS'].loc[6].loc[4]
    #                                                               
    #   Into Rocky Mountains-Great Plains from:                     
    #     Eastern Midwest                                           
    #T90(20,IY,IS)=NGFLOWS(7,1,IY)
    z[20] = dfd['NGFLOWS'].loc[7].loc[1]
    #     South Central                                             
    #T90(21,IY,IS)=NGFLOWS(7,2,IY)
    z[21] = dfd['NGFLOWS'].loc[7].loc[2]
    #     Northern Great Plains                                     
    #T90(22,IY,IS)=NGFLOWS(7,3,IY)
    z[22] = dfd['NGFLOWS'].loc[7].loc[3]
    #     Arizona and New Mexico                                    
    #T90(23,IY,IS)=NGFLOWS(7,4,IY)
    z[23] = dfd['NGFLOWS'].loc[7].loc[4]
    #     Oregon and Washington                                     
    #T90(24,IY,IS)=NGFLOWS(7,5,IY)
    z[24] = dfd['NGFLOWS'].loc[7].loc[5]
    #                                                               
    #   Into Northern Great Plains from:                            
    #     Eastern Midwest                                           
    #T90(25,IY,IS)=NGFLOWS(8,1,IY)
    z[25] = dfd['NGFLOWS'].loc[8].loc[1]
    #     Rocky Mountains-Great Plains                              
    #T90(26,IY,IS)=NGFLOWS(8,2,IY)
    z[26] = dfd['NGFLOWS'].loc[8].loc[2]
    #     Canada                                                    
    #T90(27,IY,IS)=NGFLOWS(8,3,IY)
    z[27] = dfd['NGFLOWS'].loc[8].loc[3]
    #                                                               
    #   Into Arizona and New Mexico from:                           
    #     South Central                                             
    #T90(28,IY,IS)=NGFLOWS(9,1,IY)
    z[28] = dfd['NGFLOWS'].loc[9].loc[1]
    #     Rocky Mountains-Great Plains                              
    #T90(29,IY,IS)=NGFLOWS(9,2,IY)
    z[29] = dfd['NGFLOWS'].loc[9].loc[2]
    #                                                               
    #   Into California from:                                       
    #     Oregon and Washington                                     
    #T90(30,IY,IS)=NGFLOWS(10,1,IY)
    z[30] = dfd['NGFLOWS'].loc[10].loc[1]
    #     Rocky Mountains-Great Plains                              
    #T90(31,IY,IS)=NGFLOWS(10,2,IY)
    z[31] = dfd['NGFLOWS'].loc[10].loc[2]
    #     Arizona and New Mexico                                    
    #T90(32,IY,IS)=NGFLOWS(10,3,IY)
    z[32] = dfd['NGFLOWS'].loc[10].loc[3]
    #                                                               
    #   Into Oregon and Washington from:                            
    #     Rocky Mountains-Great Plains                              
    #T90(33,IY,IS)=NGFLOWS(11,1,IY)
    z[33] = dfd['NGFLOWS'].loc[11].loc[1]
    #     Canada (into Washington)                                  
    #T90(34,IY,IS)=NGFLOWS(11,2,IY)
    z[34] = dfd['NGFLOWS'].loc[11].loc[2]
    #     Canada (through Idaho)                                    
    #T90(35,IY,IS)=NGFLOWS(11,3,IY)
    z[35] = dfd['NGFLOWS'].loc[11].loc[3]
    
    return z