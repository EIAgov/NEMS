# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_033(dfd, table_spec, table_id):
    """Fill table Other Commercial Sector Consumption
   
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
   
    #   Other Commercial Sector Consumption
    #   (quadrillion Btu)
    #    Fuels                                                      
    #   Miscellaneous End-Use Services                              
    #     Electricity                                               
    # T33(1,IY,IS)=CMUSCONSUMPTION(10,1,IY)
    z[1] = dfd['CMUSCONSUMPTION'].loc[10].loc[1]
    #     Natural Gas                                               
    #T33(2,IY,IS)=CMUSCONSUMPTION(10,2,IY)
    z[2] = dfd['CMUSCONSUMPTION'].loc[10].loc[2]
    #     Distillate Fuel Oil                                       
    #T33(3,IY,IS)=CMUSCONSUMPTION(10,3,IY)
    z[3] = dfd['CMUSCONSUMPTION'].loc[10].loc[3]                    
    #   Combined Heat and Power                                     
    #     Steam Coal                                                
    #T33(4,IY,IS)=CMUSCOGEN(1,IY)
    z[4] = dfd['CMUSCOGEN'].loc[1]
    #     Distillate and Residual Fuel Oil                          
    #T33(5,IY,IS)=CMUSCOGEN(2,IY)
    z[5] = dfd['CMUSCOGEN'].loc[2]
    #     Natural Gas                                               
    #T33(6,IY,IS)=CMUSCOGEN(3,IY)
    z[6] = dfd['CMUSCOGEN'].loc[3]
    #     Biomass                                                   
    #T33(7,IY,IS)=CMUSCOGEN(4,IY)
    z[7] = dfd['CMUSCOGEN'].loc[4] 
    
    #   District Services                                            
    #     Space Heating                                             
    #        Electricity                                            
    #T33(8,IY,IS)=CMUSDISTSERV(1,1,IY)
    z[8] = dfd['CMUSDISTSERV'].loc[1].loc[1]
    #        Natural Gas                                            
    #T33(9,IY,IS)=CMUSDISTSERV(1,2,IY)
    z[9] = dfd['CMUSDISTSERV'].loc[1].loc[2]
    #        Distillate Fuel Oil                                    
    #T33(10,IY,IS)=CMUSDISTSERV(1,3,IY)
    z[10] = dfd['CMUSDISTSERV'].loc[1].loc[3]                      
    #     Space Cooling                                             
    #        Electricity                                            
    #T33(11,IY,IS)=CMUSDISTSERV(2,1,IY)
    z[11] = dfd['CMUSDISTSERV'].loc[2].loc[1]
    #        Natural Gas                                            
    #T33(12,IY,IS)=CMUSDISTSERV(2,2,IY)
    z[12] = dfd['CMUSDISTSERV'].loc[2].loc[2]                      
    #     Hot Water Heating                                         
    #        Electricity                                            
    #T33(13,IY,IS)=CMUSDISTSERV(3,1,IY)
    z[13] = dfd['CMUSDISTSERV'].loc[3].loc[1]
    #        Natural Gas                                            
    #T33(14,IY,IS)=CMUSDISTSERV(3,2,IY)
    z[14] = dfd['CMUSDISTSERV'].loc[3].loc[2]
    #        Distillate Fuel Oil                                    
    #T33(15,IY,IS)=CMUSDISTSERV(3,3,IY)
    z[15] = dfd['CMUSDISTSERV'].loc[3].loc[3]                   
    #   Average Efficiency - Purchased Equipment                    
    #   (Btu Out / Btu In, unless otherwise noted)                  
    #    Space Heating                                              
    #      Electricity                                              
    #T33(16,IY,IS)=CMUSPURCHEFF(1,1,IY)
    z[16] = dfd['CMUSPURCHEFF'].loc[1].loc[1]   
    #      Natural Gas                                              
    #T33(17,IY,IS)=CMUSPURCHEFF(1,2,IY)
    z[17] = dfd['CMUSPURCHEFF'].loc[1].loc[2]  
    #      Distillate Fuel Oil                                      
    #T33(18,IY,IS)=CMUSPURCHEFF(1,3,IY)
    z[18] = dfd['CMUSPURCHEFF'].loc[1].loc[3]                      
    #    Space Cooling                                              
    #      Electricity                                              
    #T33(19,IY,IS)=CMUSPURCHEFF(2,1,IY)
    z[19] = dfd['CMUSPURCHEFF'].loc[2].loc[1] 
    #      Natural Gas                                              
    #T33(20,IY,IS)=CMUSPURCHEFF(2,2,IY)
    z[20] = dfd['CMUSPURCHEFF'].loc[2].loc[2]                      
    #    Water Heating                                              
    #      Electricity                                              
    #T33(21,IY,IS)=CMUSPURCHEFF(3,1,IY)
    z[21] = dfd['CMUSPURCHEFF'].loc[3].loc[1]  
    #      Natural Gas                                              
    #T33(22,IY,IS)=CMUSPURCHEFF(3,2,IY)
    z[22] = dfd['CMUSPURCHEFF'].loc[3].loc[2] 
    #      Distillate Fuel Oil                                      
    #T33(23,IY,IS)=CMUSPURCHEFF(3,3,IY)
    z[23] = dfd['CMUSPURCHEFF'].loc[3].loc[3]                       
    #    Ventilation (cubic feet per minute per Btu) 1              
    #      Electricity                                              
    #T33(24,IY,IS)=CMUSPURCHEFF(4,1,IY)
    z[24] = dfd['CMUSPURCHEFF'].loc[4].loc[1]                      
    #    Cooking                                                    
    #      Electricity                                              
    #T33(25,IY,IS)=CMUSPURCHEFF(5,1,IY)
    z[25] = dfd['CMUSPURCHEFF'].loc[5].loc[1]  
    #      Natural Gas                                              
    #T33(26,IY,IS)=CMUSPURCHEFF(5,2,IY)
    z[26] = dfd['CMUSPURCHEFF'].loc[5].loc[2]                         
    #    Lighting Efficacy 2/                                       
    #      (efficacy in lumens per watt)                            
    #      Electricity                                              
    #T33(27,IY,IS)=CMUSPURCHEFF(6,1,IY)
    z[27] = dfd['CMUSPURCHEFF'].loc[6].loc[1]                       
    #    Refrigeration                                              
    #      Electricity                                              
    #T33(28,IY,IS)=CMUSPURCHEFF(7,1,IY)
    z[28] = dfd['CMUSPURCHEFF'].loc[7].loc[1]  

    return z    
