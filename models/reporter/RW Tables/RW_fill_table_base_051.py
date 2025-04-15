# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_051(dfd, table_spec, table_id):
    """Fill table Light-Duty Vehicle Miles Traveled by Technology Type
   
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

    #   Light-Duty Vehicle Miles Traveled by Technology Type
    #   (billion miles, unless otherwise noted)
    #    Technology Type                                            
    #   Conventional Vehicles 1/                                    
    #     Gasoline ICE Vehicles                                    
    #T51(1,IY,IS)=TRLDVMT(1,IY)
    z[1] =  dfd['TRLDVMT'].loc[1]
    #     TDI Diesel ICE                                            
    #T51(2,IY,IS)=TRLDVMT(2,IY)
    z[2] = dfd['TRLDVMT'].loc[2]                                                              
    #   Alternative-Fuel Vehicles 1/                                
    #     Ethanol-Flex Fuel ICE                                    
    #T51(5,IY,IS)=TRLDVMT(3,IY)
    z[5] = dfd['TRLDVMT'].loc[3]
    #     100-Mile Electric Vehicle                                
    #T51(6,IY,IS)=TRLDVMT(4,IY)
    z[6] = dfd['TRLDVMT'].loc[4]
    #     200-Mile Electric Vehicle                                 
    #T51(7,IY,IS)=TRLDVMT(7,IY)
    z[7] = dfd['TRLDVMT'].loc[7]
    #     300-Mile Electric Vehicle                                 
    #T51(15,IY,IS)=TRLDVMT(15,IY)
    z[15] = dfd['TRLDVMT'].loc[15]
    #     Plug-in 20 Gasoline Hybrid                                
    #T51(8,IY,IS)=TRLDVMT(5,IY)
    z[8] = dfd['TRLDVMT'].loc[5]
    #     Plug-in 50 Gasoline Hybrid                                
    #T51(4,IY,IS)=TRLDVMT(6,IY)
    z[4] = dfd['TRLDVMT'].loc[6]
    #     Electric-Diesel Hybrid                                    
    #T51(9,IY,IS)=TRLDVMT(8,IY)
    z[9] = dfd['TRLDVMT'].loc[8]
    #     Electric-Gasoline Hybrid                                  
    #T51(10,IY,IS)=TRLDVMT(16,IY)
    z[10] = dfd['TRLDVMT'].loc[16]
    #     Natural Gas ICE                                           
    #T51(11,IY,IS)=TRLDVMT(11,IY)
    z[11] = dfd['TRLDVMT'].loc[11]
    #     Natural Gas Bi-fuel                                       
    #T51(12,IY,IS)=TRLDVMT(9,IY)
    z[12] = dfd['TRLDVMT'].loc[9]
    #     Propane ICE                                               
    #T51(13,IY,IS)=TRLDVMT(12,IY)
    z[13] = dfd['TRLDVMT'].loc[12]
    #     Propane Bi-fuel                                           
    #T51(14,IY,IS)=TRLDVMT(10,IY)
    z[14] = dfd['TRLDVMT'].loc[10]
    #     Fuel Cell Methanol                                        
    #T51(16,IY,IS)=TRLDVMT(13,IY)
    z[16] = dfd['TRLDVMT'].loc[13]
    #     Fuel Cell Hydrogen                                        
    #T51(17,IY,IS)=TRLDVMT(14,IY)
    z[17] = dfd['TRLDVMT'].loc[14]                                
    #   VMT Equation Components                                     
    #     Total VMT (billion miles)                                 
    #T51(21,IY,IS)=TRLDVMTE(1,IY)
    z[21] = dfd['TRLDVMTE'].loc[1]
    #     VMT/Licensed Driver (thousand miles)                      
    #T51(22,IY,IS)=TRLDVMTE(2,IY)
    z[22] = dfd['TRLDVMTE'].loc[2]
    #     Licensed Drivers (million)                                
    #T51(23,IY,IS)=TRLDVMTE(3,IY)
    z[23] = dfd['TRLDVMTE'].loc[3]                                 
    #   Price Effects                                               
    #     Motor Gasoline Price (1987 $/million Btu)                 
    #T51(24,IY,IS)=TRLDVMTE(4,IY)
    z[24] = dfd['TRLDVMTE'].loc[4]
    #     Household Stock Miles per Gallon                          
    #T51(25,IY,IS)=TRLDVMTE(5,IY)
    z[25] = dfd['TRLDVMTE'].loc[5]
    #     Real Cost of Driving per Mile (1987 cents)                
    #T51(26,IY,IS)=TRLDVMTE(6,IY)
    z[26] = dfd['TRLDVMTE'].loc[6]
    #     Licensing Rate                                            
    #T51(27,IY,IS)=TRLDVMTE(7,IY)
    z[27] = dfd['TRLDVMTE'].loc[7]                                
    #   Income Effects                                              
    #     Disposable Income per Licensed Driver                     
    #T51(28,IY,IS)=TRLDVMTE(8,IY)
    z[28] = dfd['TRLDVMTE'].loc[8]
    #     Point Income Elasticity (ratio)                           
    #T51(29,IY,IS)=TRLDVMTE(9,IY)
    z[29] = dfd['TRLDVMTE'].loc[9]                                 
    #   Demographic Driving Population Effect                       
    #     Percent Female Driving Population                         
    #T51(30,IY,IS)=TRLDVMTE(10,IY)
    z[30] = dfd['TRLDVMTE'].loc[10]
    #     Vehicle Miles Traveled per Vehicle                        
    #T51(31,IY,IS)=TRLDVMTE(11,IY)
    z[31] = dfd['TRLDVMTE'].loc[11]

    return z                                                                 
