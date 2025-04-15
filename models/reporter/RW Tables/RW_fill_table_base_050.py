# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_050(dfd, table_spec, table_id):
    """Fill table Light-Duty Vehicle Miles per Gallon by Technology Type
   
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
       
    
    """
	
  
    z = {}

# issues writing out z 1,5,7,8,21,27,28

    #   Light-Duty Vehicle Miles per Gallon by Technology Type
    #   (miles per gallon gasoline equivalent)
    #    Technology Type                                            
    #                                                               
    #   New Car Miles per Gallon 1/                                 
    #    Conventional Cars                                          
    #      Gasoline ICE Vehicles                                    
    #T50(1,IY,IS)=TRLDMPGC(1,IY)
    z[1] = dfd['TRLDMPGC'].loc[1]

    #      TDI Diesel ICE                                           
    #T50(2,IY,IS)=TRLDMPGC(2,IY)
    z[2] = dfd['TRLDMPGC'].loc[2]

    #                                                               
    #    Alternative-Fuel Cars                                      
    #      Ethanol-Flex Fuel ICE                                    
    #T50(5,IY,IS)=TRLDMPGC(3,IY)
    z[5] = dfd['TRLDMPGC'].loc[3]

    #      100-Mile Electric Vehicle                                
    #T50(6,IY,IS)=TRLDMPGC(4,IY)
    z[6] = dfd['TRLDMPGC'].loc[4]

    #      200-Mile Electric Vehicle                                
    #T50(7,IY,IS)=TRLDMPGC(7,IY)
    z[7] = dfd['TRLDMPGC'].loc[7]

    #      300-Mile Electric Vehicle                                
    #T50(15,IY,IS)=TRLDMPGC(15,IY)
    z[15] = dfd['TRLDMPGC'].loc[15]

    #      Plug-in 20 Gasoline Hybrid                               
    #T50(8,IY,IS)=TRLDMPGC(5,IY)
    z[8] = dfd['TRLDMPGC'].loc[5]

    #      Plug-in 50 Gasoline Hybrid                               
    #T50(4,IY,IS)=TRLDMPGC(6,IY)
    z[4] = dfd['TRLDMPGC'].loc[6]

    #      Electric-Diesel Hybrid                                   
    #T50(9,IY,IS)=TRLDMPGC(8,IY)
    z[9] = dfd['TRLDMPGC'].loc[8]

    #      Electric-Gasoline Hybrid                                 
    #T50(10,IY,IS)=TRLDMPGC(16,IY)
    z[10] = dfd['TRLDMPGC'].loc[16]

    #      Natural Gas ICE                                          
    #T50(11,IY,IS)=TRLDMPGC(11,IY)
    z[11] = dfd['TRLDMPGC'].loc[11]

    #      Natural Gas Bi-fuel                                      
    #T50(12,IY,IS)=TRLDMPGC(9,IY)
    z[12] = dfd['TRLDMPGC'].loc[9]

    #      Propane ICE                                              
    #T50(13,IY,IS)=TRLDMPGC(12,IY)
    z[13] = dfd['TRLDMPGC'].loc[12]

    #      Propane Bi-fuel                                          
    #T50(14,IY,IS)=TRLDMPGC(10,IY)
    z[14] = dfd['TRLDMPGC'].loc[10]

    #      Fuel Cell Methanol                                       
    #T50(16,IY,IS)=TRLDMPGC(13,IY)
    z[16] = dfd['TRLDMPGC'].loc[13]

    #      Fuel Cell Hydrogen                                       
    #T50(17,IY,IS)=TRLDMPGC(14,IY)
    z[17] = dfd['TRLDMPGC'].loc[14]

    #                                                               
    #    Average New Cars Miles per Gallon                          
    #T50(20,IY,IS)=NEWMPG(1,IY)
    z[20] = dfd['NEWMPG'].loc[1]

    #                                                               
    #   New Light Truck Miles per Gallon 1/                         
    #    Conventional Light Trucks                                  
    #      Gasoline ICE Vehicles                                    
    #T50(21,IY,IS)=TRLDMPGT(1,IY)
    z[21] = dfd['TRLDMPGT'].loc[1]

    #      TDI Diesel ICE                                           
    #T50(22,IY,IS)=TRLDMPGT(2,IY)
    z[22] = dfd['TRLDMPGT'].loc[2]

    #                                                               
    #    Alternative-Fuel Light Trucks                              
    #      Ethanol-Flex Fuel ICE                                    
    #T50(25,IY,IS)=TRLDMPGT(3,IY)
    z[25] = dfd['TRLDMPGT'].loc[3]

    #      100-Mile Electric Vehicle                                
    #T50(26,IY,IS)=TRLDMPGT(4,IY)
    z[26] = dfd['TRLDMPGT'].loc[4]

    #      200-Mile Electric Vehicle                                
    #T50(27,IY,IS)=TRLDMPGT(7,IY)
    z[27] = dfd['TRLDMPGT'].loc[7]

    #      300-Mile Electric Vehicle                                
    #T50(35,IY,IS)=TRLDMPGT(15,IY)
    z[35] = dfd['TRLDMPGT'].loc[15]

    #      Plug-in 20 Gasoline Hybrid                               
    #T50(28,IY,IS)=TRLDMPGT(5,IY)
    z[28] = dfd['TRLDMPGT'].loc[5]

    #      Plug-in 50 Gasoline Hybrid                               
    #T50(24,IY,IS)=TRLDMPGT(6,IY)
    z[24] = dfd['TRLDMPGT'].loc[6]

    #      Electric-Diesel Hybrid                                   
    #T50(29,IY,IS)=TRLDMPGT(8,IY)
    z[29] = dfd['TRLDMPGT'].loc[8]

    #      Electric-Gasoline Hybrid                                 
    #T50(30,IY,IS)=TRLDMPGT(16,IY)
    z[30] = dfd['TRLDMPGT'].loc[16]

    #      Natural Gas ICE                                          
    #T50(31,IY,IS)=TRLDMPGT(11,IY)
    z[31] = dfd['TRLDMPGT'].loc[11]

    #      Natural Gas Bi-fuel                                      
    #T50(32,IY,IS)=TRLDMPGT(9,IY)
    z[32] = dfd['TRLDMPGT'].loc[9]

    #      Propane ICE                                              
    #T50(33,IY,IS)=TRLDMPGT(12,IY)
    z[33] = dfd['TRLDMPGT'].loc[12]

    #      Propane Bi-fuel                                          
    #T50(34,IY,IS)=TRLDMPGT(10,IY)
    z[34] = dfd['TRLDMPGT'].loc[10]

    #      Fuel Cell Methanol                                       
    #T50(36,IY,IS)=TRLDMPGT(13,IY)
    z[36] = dfd['TRLDMPGT'].loc[13]

    #      Fuel Cell Hydrogen                                       
    #T50(37,IY,IS)=TRLDMPGT(14,IY)
    z[37] = dfd['TRLDMPGT'].loc[14]

    #                                                               
    #    Average Light Truck Miles per Gallon                       
    #T50(40,IY,IS)=NEWMPG(2,IY)
    z[40] =  dfd['NEWMPG'].loc[2]

    #                                                               
    #   Average New Vehicle Miles per Gallon                        
    #T50(41,IY,IS)=NEWMPG(3,IY)
    z[41] = dfd['NEWMPG'].loc[3]

    #                                                               
    #   Average Car Stock Miles per Gallon 2/                       
    #T50(42,IY,IS)=TRLDMPGF(1,IY)
    z[42] =  dfd['TRLDMPGF'].loc[1]

    #   Average Light Truck Stock Miles per Gallon 2/               
    #T50(43,IY,IS)=TRLDMPGF(2,IY)
    z[43] =  dfd['TRLDMPGF'].loc[2]

    #                                                               
    #   Average Vehicle Stock Miles per Gallon 2/                   
    #T50(44,IY,IS)=TRLDMPGF(3,IY)
    z[44] =  dfd['TRLDMPGF'].loc[3]

    #                                                               
    #                                                               
    #   New Car, Light Truck, and Commercial                        
    #    Light Truck Miles per Gallon 2/                            
    #      Motor Gasoline, Internal Combustion or Other             
    #T50(45,IY,IS)=TECHMPG(1,IY)
    z[45] =  dfd['TECHMPG'].loc[1]

    #      Diesel, Internal Combustion or Other                     
    #T50(46,IY,IS)=TECHMPG(2,IY)
    z[46] =  dfd['TECHMPG'].loc[2]

    #      Natural Gas, Internal Combustion                         
    #T50(47,IY,IS)=TECHMPG(3,IY)
    z[47] =  dfd['TECHMPG'].loc[3]

    #      Propane, Internal Combustion                             
    #T50(48,IY,IS)=TECHMPG(4,IY)
    z[48] =  dfd['TECHMPG'].loc[4]

    #      Other, Internal Combustion or Hybrid                     
    #T50(49,IY,IS)=TECHMPG(5,IY)
    z[49] =  dfd['TECHMPG'].loc[5]

    #      Electric Vehicles                                        
    #T50(50,IY,IS)=TECHMPG(6,IY)
    z[50] =  dfd['TECHMPG'].loc[6]

    #      Hydrogen Fuel Cell                                       
    #T50(51,IY,IS)=TECHMPG(7,IY)
    z[51] =  dfd['TECHMPG'].loc[7]

    #      Plug-in Hybrids                                          
    #T50(52,IY,IS)=TECHMPG(8,IY)
    z[52] =  dfd['TECHMPG'].loc[8]

    #   Average New Vehicle Miles per Gallon +2B 2/           
    #T50(53,IY,IS)=LDV_MPG(2,IY)
    z[53] = dfd['LDV_MPG'].loc[2]

    #                                                               
    #   Stock Car, Light Truck, and Commercial                      
    #    Light Truck Miles per Gallon 2/                            
    #      Motor Gasoline, Internal Combustion or Other             
    #T50(54,IY,IS)=STKMPG(1,IY)
    z[54] = dfd['STKMPG'].loc[1]

    #      Diesel, Internal Combustion or Other                     
    #T50(55,IY,IS)=STKMPG(2,IY)
    z[55] = dfd['STKMPG'].loc[2]

    #      Natural Gas, Internal Combustion                         
    #T50(56,IY,IS)=STKMPG(3,IY)
    z[56] = dfd['STKMPG'].loc[3]

    #      Propane, Internal Combustion                             
    #T50(57,IY,IS)=STKMPG(4,IY)
    z[57] = dfd['STKMPG'].loc[4]

    #      Other, Internal Combustion or Hybrid                     
    #T50(58,IY,IS)=STKMPG(5,IY)
    z[58] = dfd['STKMPG'].loc[5]

    #      Electric Vehicles                                        
    #T50(59,IY,IS)=STKMPG(6,IY)
    z[59] = dfd['STKMPG'].loc[6]

    #      Hydrogen Fuel Cell                                       
    #T50(60,IY,IS)=STKMPG(7,IY)
    z[60] = dfd['STKMPG'].loc[7]

    #      Plug-in Hybrids                                          
    #T50(61,IY,IS)=STKMPG(8,IY)
    z[61] = dfd['STKMPG'].loc[8]

    #   Average Vehicle Stock Miles per Gallon +2B 2/               
    #T50(62,IY,IS)=LDV_MPG(3,IY)
    z[62] = dfd['LDV_MPG'].loc[3]

    return z                                                               
