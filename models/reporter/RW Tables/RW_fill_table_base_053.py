# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_053(dfd, table_spec, table_id):
    """Fill table Transportation Fleet Car and Truck Fuel Consumption by Type and Technology
   
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

    #   Transportation Fleet Car and Truck Fuel Consumption by Type and Technology
    #   (trillion Btu)
    #    Technology Type                                             
    #   Cars 1/                                                     
    #    Conventional Cars                                          
    #      Gasoline ICE Vehicles                                    
    #T53(1,IY,IS)=FLTFCLDVBTU(1,1,IY)
    z[1] = dfd['FLTFCLDVBTU'].loc[1].loc[1]
    #      TDI Diesel ICE                                           
    #T53(2,IY,IS)=FLTFCLDVBTU(1,2,IY)
    z[2] = dfd['FLTFCLDVBTU'].loc[1].loc[2]
    #        Total Conventional Cars                                
    #T53(3,IY,IS)=T53(1,IY,IS)+T53(2,IY,IS)
    z[3] = z[1]+z[2]                                              
    #    Alternative-Fuel Cars                                      
    #      Ethanol-Flex Fuel ICE                                    
    #T53(5,IY,IS)=FLTFCLDVBTU(1,3,IY)
    z[5] = dfd['FLTFCLDVBTU'].loc[1].loc[3]
    #      100-Mile Electric Vehicle                                
    #T53(6,IY,IS)=FLTFCLDVBTU(1,4,IY)
    z[6] = dfd['FLTFCLDVBTU'].loc[1].loc[4]
    #      200-Mile Electric Vehicle                                
    #T53(7,IY,IS)=FLTFCLDVBTU(1,7,IY)
    z[7] = dfd['FLTFCLDVBTU'].loc[1].loc[7]
    #      300-Mile Electric Vehicle                                
    #T53(15,IY,IS)=FLTFCLDVBTU(1,15,IY)
    z[15] = dfd['FLTFCLDVBTU'].loc[1].loc[15]
    #      Plug-in 20 Gasoline Hybrid                               
    #T53(8,IY,IS)=FLTFCLDVBTU(1,5,IY)
    z[8] = dfd['FLTFCLDVBTU'].loc[1].loc[5]
    #      Plug-in 50 Gasoline Hybrid                               
    #T53(4,IY,IS)=FLTFCLDVBTU(1,6,IY)
    z[4] = dfd['FLTFCLDVBTU'].loc[1].loc[6]
    #      Electric-Diesel Hybrid                                   
    #T53(9,IY,IS)=FLTFCLDVBTU(1,8,IY)
    z[9] = dfd['FLTFCLDVBTU'].loc[1].loc[8]
    #      Electric-Gasoline Hybrid                                 
    #T53(10,IY,IS)=FLTFCLDVBTU(1,16,IY)
    z[10] = dfd['FLTFCLDVBTU'].loc[1].loc[16]
    #      Natural Gas ICE                                          
    #T53(11,IY,IS)=FLTFCLDVBTU(1,11,IY)
    z[11] = dfd['FLTFCLDVBTU'].loc[1].loc[11]
    #      Natural Gas Bi-fuel                                      
    #T53(12,IY,IS)=FLTFCLDVBTU(1,9,IY)
    z[12] = dfd['FLTFCLDVBTU'].loc[1].loc[9]
    #      Propane ICE                                              
    #T53(13,IY,IS)=FLTFCLDVBTU(1,12,IY)
    z[13] = dfd['FLTFCLDVBTU'].loc[1].loc[12]
    #      Propane Bi-fuel                                          
    #T53(14,IY,IS)=FLTFCLDVBTU(1,10,IY)
    z[14] = dfd['FLTFCLDVBTU'].loc[1].loc[10]
    #      Fuel Cell Methanol                                       
    #T53(16,IY,IS)=FLTFCLDVBTU(1,13,IY)
    z[16] = dfd['FLTFCLDVBTU'].loc[1].loc[13]
    #      Fuel Cell Hydrogen                                       
    #T53(17,IY,IS)=FLTFCLDVBTU(1,14,IY)
    z[17] = dfd['FLTFCLDVBTU'].loc[1].loc[14]
    #        Total Alternative Cars                                 
    #T53(18,IY,IS)=FSUM(T53(4,IY,IS),14)
    z[18] = z[4]+z[5]+z[6]+z[7]+z[8]+z[9]+z[10]+z[11]+z[12]+z[13]+z[14]+z[15]+z[16]+z[17]
    #    Total Car Consumption                                      
    #T53(20,IY,IS)=T53(3,IY,IS)+T53(18,IY,IS)
    z[20] = z[3] +z[18]                                           
    #   Light Trucks 1/                                             
    #    Conventional Light Trucks                                  
    #      Gasoline ICE Vehicles                                    
    #T53(21,IY,IS)=FLTFCLDVBTU(2,1,IY)
    z[21] =  dfd['FLTFCLDVBTU'].loc[2].loc[1]
    #      TDI Diesel ICE                                           
    #T53(22,IY,IS)=FLTFCLDVBTU(2,2,IY)
    z[22] =  dfd['FLTFCLDVBTU'].loc[2].loc[2]
    #        Total Conventional Light Trucks                        
    #T53(23,IY,IS)=T53(21,IY,IS)+T53(22,IY,IS)
    z[23] = z[21] + z[22]                                       
    #    Alternative-Fuel Light Trucks                              
    #      Ethanol-Flex Fuel ICE                                    
    #T53(25,IY,IS)=FLTFCLDVBTU(2,3,IY)
    z[25] = dfd['FLTFCLDVBTU'].loc[2].loc[3]
    #      100-Mile Electric Vehicle                                
    #T53(26,IY,IS)=FLTFCLDVBTU(2,4,IY)
    z[26] = dfd['FLTFCLDVBTU'].loc[2].loc[4]
    #      200-Mile Electric Vehicle                                
    #T53(27,IY,IS)=FLTFCLDVBTU(2,7,IY)
    z[27] = dfd['FLTFCLDVBTU'].loc[2].loc[7]
    #      300-Mile Electric Vehicle                                
    #T53(35,IY,IS)=FLTFCLDVBTU(2,15,IY)
    z[35] = dfd['FLTFCLDVBTU'].loc[2].loc[15]
    #      Plug-in 20 Gasoline Hybrid                               
    #T53(28,IY,IS)=FLTFCLDVBTU(2,5,IY)
    z[28] = dfd['FLTFCLDVBTU'].loc[2].loc[5]
    #      Plug-in 50 Gasoline Hybrid                               
    #T53(24,IY,IS)=FLTFCLDVBTU(2,6,IY)
    z[24] = dfd['FLTFCLDVBTU'].loc[2].loc[6]
    #      Electric-Diesel Hybrid                                   
    #T53(29,IY,IS)=FLTFCLDVBTU(2,8,IY)
    z[29] = dfd['FLTFCLDVBTU'].loc[2].loc[8]
    #      Electric-Gasoline Hybrid                                 
    #T53(30,IY,IS)=FLTFCLDVBTU(2,16,IY)
    z[30] = dfd['FLTFCLDVBTU'].loc[2].loc[16]
    #      Natural Gas ICE                                          
    #T53(31,IY,IS)=FLTFCLDVBTU(2,11,IY)
    z[31] = dfd['FLTFCLDVBTU'].loc[2].loc[11]
    #      Natural Gas Bi-fuel                                      
    #T53(32,IY,IS)=FLTFCLDVBTU(2,9,IY)
    z[32] = dfd['FLTFCLDVBTU'].loc[2].loc[9]
    #      Propane ICE                                              
    #T53(33,IY,IS)=FLTFCLDVBTU(2,12,IY)
    z[33] = dfd['FLTFCLDVBTU'].loc[2].loc[12]
    #      Propane Bi-fuel                                          
    #T53(34,IY,IS)=FLTFCLDVBTU(2,10,IY)
    z[34] = dfd['FLTFCLDVBTU'].loc[2].loc[10]
    #      Fuel Cell Methanol                                       
    #T53(36,IY,IS)=FLTFCLDVBTU(2,13,IY)
    z[36] = dfd['FLTFCLDVBTU'].loc[2].loc[13]
    #      Fuel Cell Hydrogen                                       
    #T53(37,IY,IS)=FLTFCLDVBTU(2,14,IY)
    z[37] = dfd['FLTFCLDVBTU'].loc[2].loc[14]
    #        Total Alternative Light Trucks                         
    #T53(38,IY,IS)=FSUM(T53(24,IY,IS),14)
    z[38] = z[24]+z[25]+z[26]+z[27]+z[28]+z[29]+z[30]+z[31]+z[32]+z[33]+z[34]+z[35]+z[36]+z[37]
    #    Total Light Truck Consumption                              
    #T53(40,IY,IS)=T53(23,IY,IS)+T53(38,IY,IS)
    z[40] = z[23] + z[38]                                            
    #   Total Fleet Vehicles                                        
    #T53(41,IY,IS)=T53(20,IY,IS)+T53(40,IY,IS)
    z[41] = z[20]+z[40]                                             
    #   Commercial Light Trucks 2/            
    #T53(42:51,IY,IS)=BCLTBTUT(1:10,IY)
     #      Motor Gasoline 
    z[42] = dfd['BCLTBTUT'].loc[1]
    #      Diesel                                                   
    z[43] = dfd['BCLTBTUT'].loc[2]
    #      Propane 
    z[44] = dfd['BCLTBTUT'].loc[3]
    #      Compressed/Liquefied Natural Gas                         
    z[45] = dfd['BCLTBTUT'].loc[4]
    #      Ethanol-Flex Fuel                                        
    z[46] = dfd['BCLTBTUT'].loc[5]
    #      Electric  
    z[47] = dfd['BCLTBTUT'].loc[6]
    #      Plug-in Gasoline Hybrid 
    z[48] = dfd['BCLTBTUT'].loc[7]
    #      Plug-in Diesel Hybrid 
    z[49] = dfd['BCLTBTUT'].loc[8]
    #      Fuel Cell            
    z[50] = dfd['BCLTBTUT'].loc[9]
    #      Fuel Cell Battery Dominant          
    z[51] = dfd['BCLTBTUT'].loc[10]
    #      Gasoline HEV
    z[52] = dfd['BCLTBTUT'].loc[11]
    #      H2 ICE           
    z[53] = dfd['BCLTBTUT'].loc[12]
    #         Total Commercial Light Trucks 
    z[54] = z[42] + z[43] + z[44] + z[45] + z[46] + z[47] + z[48] + z[49] + z[50] + z[51] + z[52] + z[53]

    return z