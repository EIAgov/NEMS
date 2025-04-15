# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_054(dfd, table_spec, table_id):
    """Fill table Transportation Fleet Car and Truck Sales by Type and Technology
   
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
    # issues with z 19,39
    
    #   Transportation Fleet Car and Truck Sales by Type and Technology
    #   (thousands)
    #    Technology Type                                            
    #                                                               
    #   New Car Sales 1/                                            
    #    Conventional Cars                                          
    #      Gasoline ICE Vehicles                                    
    #T54(1,IY,IS)=FLTECHRPT(1,1,IY)
    z[1] = dfd['FLTECHRPT'].loc[1].loc[1]
    #      TDI Diesel ICE                                           
    #T54(2,IY,IS)=FLTECHRPT(1,2,IY)
    z[2] = dfd['FLTECHRPT'].loc[1].loc[2]
    #        Total Conventional Cars                                
    #T54(3,IY,IS)=T54(1,IY,IS)+T54(2,IY,IS)
    z[3] = z[1]+z[2]
    #                                                               
    #    Alternative-Fuel Cars                                      
    #      Ethanol-Flex Fuel ICE                                    
    #T54(5,IY,IS)=FLTECHRPT(1,3,IY)
    z[5] = dfd['FLTECHRPT'].loc[1].loc[3]
    #      100-Mile Electric Vehicle                                
    #T54(6,IY,IS)=FLTECHRPT(1,4,IY)
    z[6] = dfd['FLTECHRPT'].loc[1].loc[4]
    #      200-Mile Electric Vehicle                                
    #T54(7,IY,IS)=FLTECHRPT(1,7,IY)
    z[7] = dfd['FLTECHRPT'].loc[1].loc[7]
    #      300-Mile Electric Vehicle                                
    #T54(15,IY,IS)=FLTECHRPT(1,15,IY)
    z[15] = dfd['FLTECHRPT'].loc[1].loc[15]
    #      Plug-in 20 Gasoline Hybrid                               
    #T54(8,IY,IS)=FLTECHRPT(1,5,IY)
    z[8] = dfd['FLTECHRPT'].loc[1].loc[5]
    #      Plug-in 50 Gasoline Hybrid                               
    #T54(4,IY,IS)=FLTECHRPT(1,6,IY)
    z[4] = dfd['FLTECHRPT'].loc[1].loc[6]
    #      Electric-Diesel Hybrid                                   
    #T54(9,IY,IS)=FLTECHRPT(1,8,IY)
    z[9] = dfd['FLTECHRPT'].loc[1].loc[8]
    #      Electric-Gasoline Hybrid                                 
    #T54(10,IY,IS)=FLTECHRPT(1,16,IY)
    z[10] = dfd['FLTECHRPT'].loc[1].loc[16]
    #      Natural Gas ICE                                          
    #T54(11,IY,IS)=FLTECHRPT(1,11,IY)
    z[11] = dfd['FLTECHRPT'].loc[1].loc[11]
    #      Natural Gas Bi-fuel                                      
    #T54(12,IY,IS)=FLTECHRPT(1,9,IY)
    z[12] = dfd['FLTECHRPT'].loc[1].loc[9]
    #      Propane ICE                                              
    #T54(13,IY,IS)=FLTECHRPT(1,12,IY)
    z[13] = dfd['FLTECHRPT'].loc[1].loc[12]
    #      Propane Bi-fuel                                          
    #T54(14,IY,IS)=FLTECHRPT(1,10,IY)
    z[14] = dfd['FLTECHRPT'].loc[1].loc[10]
    #      Fuel Cell Methanol                                       
    #T54(16,IY,IS)=FLTECHRPT(1,13,IY)
    z[16] = dfd['FLTECHRPT'].loc[1].loc[13]
    #      Fuel Cell Hydrogen                                       
    #T54(17,IY,IS)=FLTECHRPT(1,14,IY)
    z[17] = dfd['FLTECHRPT'].loc[1].loc[14]
    #        Total Alternative Cars                                 
    #T54(18,IY,IS)=FSUM(T54(4,IY,IS),14)
    z[18] = z[4]+z[5]+z[6]+z[7]+z[8]+z[9]+z[10]+z[11]+z[12]+z[13]+z[14]+z[15]+z[16]+z[17]
    #    Total New Car Sales                                        
    #T54(20,IY,IS)=T54(3,IY,IS)+T54(18,IY,IS)
    z[20] = z[3]+z[18]
    #                                                               
    #    Percent Alternative Car Sales                              
    #T54(19,IY,IS)=T54(18,IY,IS)/T54(20,IY,IS)*100.
    z[19] = z[18]/ z[20]*100
    #                                                               
    #   New Light Truck Sales 1/                                    
    #    Conventional Light Trucks                                  
    #      Gasoline ICE Vehicles                                    
    #T54(21,IY,IS)=FLTECHRPT(2,1,IY)
    z[21] = dfd['FLTECHRPT'].loc[2].loc[1]
    #      TDI Diesel ICE                                           
    #T54(22,IY,IS)=FLTECHRPT(2,2,IY)
    z[22] = dfd['FLTECHRPT'].loc[2].loc[2]
    #        Total Conventional Light Trucks                        
    #T54(23,IY,IS)=T54(21,IY,IS)+T54(22,IY,IS)
    z[23] = z[21]+z[22]
    #                                                               
    #    Alternative-Fuel Light Trucks                              
    #      Ethanol-Flex Fuel ICE                                    
    #T54(25,IY,IS)=FLTECHRPT(2,3,IY)
    z[25] = dfd['FLTECHRPT'].loc[2].loc[3]
    #      100-Mile Electric Vehicle                                
    #T54(26,IY,IS)=FLTECHRPT(2,4,IY)
    z[26] = dfd['FLTECHRPT'].loc[2].loc[4]
    #      200-Mile Electric Vehicle                                
    #T54(27,IY,IS)=FLTECHRPT(2,7,IY)
    z[27] = dfd['FLTECHRPT'].loc[2].loc[7]
    #      300-Mile Electric Vehicle                                
    #T54(35,IY,IS)=FLTECHRPT(2,15,IY)
    z[35] = dfd['FLTECHRPT'].loc[2].loc[15]
    #      Plug-in 20 Gasoline Hybrid                               
    #T54(28,IY,IS)=FLTECHRPT(2,5,IY)
    z[28] = dfd['FLTECHRPT'].loc[2].loc[5]
    #      Plug-in 50 Gasoline Hybrid                               
    #T54(24,IY,IS)=FLTECHRPT(2,6,IY)
    z[24] = dfd['FLTECHRPT'].loc[2].loc[6]
    #      Electric-Diesel Hybrid                                   
    #T54(29,IY,IS)=FLTECHRPT(2,8,IY)
    z[29] = dfd['FLTECHRPT'].loc[2].loc[8]
    #      Electric-Gasoline Hybrid                                 
    #T54(30,IY,IS)=FLTECHRPT(2,16,IY)
    z[30] = dfd['FLTECHRPT'].loc[2].loc[16]
    #      Natural Gas ICE                                          
    #T54(31,IY,IS)=FLTECHRPT(2,11,IY)
    z[31] = dfd['FLTECHRPT'].loc[2].loc[11]
    #      Natural Gas Bi-fuel                                      
    #T54(32,IY,IS)=FLTECHRPT(2,9,IY)
    z[32] = dfd['FLTECHRPT'].loc[2].loc[9]
    #      Propane ICE                                              
    #T54(33,IY,IS)=FLTECHRPT(2,12,IY)
    z[33] = dfd['FLTECHRPT'].loc[2].loc[12]
    #      Propane Bi-fuel                                          
    #T54(34,IY,IS)=FLTECHRPT(2,10,IY)
    z[34] = dfd['FLTECHRPT'].loc[2].loc[10]
    #      Fuel Cell Methanol                                       
    #T54(36,IY,IS)=FLTECHRPT(2,13,IY)
    z[36] = dfd['FLTECHRPT'].loc[2].loc[13]
    #      Fuel Cell Hydrogen                                       
    #T54(37,IY,IS)=FLTECHRPT(2,14,IY)
    z[37] = dfd['FLTECHRPT'].loc[2].loc[14]
    #        Total Alternative Light Trucks                         
    #T54(38,IY,IS)=FSUM(T54(24,IY,IS),14)
    z[38] = z[24]+z[25]+z[26]+z[27]+z[28]+z[29]+z[30]+z[31]+z[32]+z[33]+z[34]+z[35]+z[36]+z[37]
    #    Total New Light Truck Sales                                
    #T54(40,IY,IS)=T54(23,IY,IS)+T54(38,IY,IS)
    z[40] = z[23]+z[38]
    #                                                               
    #    Percent Alternative Light Truck Sales                      
    #T54(39,IY,IS)=T54(38,IY,IS)/T54(40,IY,IS)*100.
    #
    z[39] = z[38]/z[40]*100
    #                                                               
    #   Total Fleet Vehicles                                        
    #T54(41,IY,IS)=T54(20,IY,IS)+T54(40,IY,IS)
    z[41] = z[20]+z[40]
    #                                                               
    #   Commercial Light Truck Sales 2/                             
    #      Motor Gasoline                                           
    #T54(42:51,IY,IS)=CLTSALT(1:10,IY)*.001
    z[42] = dfd['CLTSALT'].loc[1]*.001
    #      Diesel                                                   
    #T54(42:51,IY,IS)=CLTSALT(1:10,IY)*.001
    z[43] = dfd['CLTSALT'].loc[2]*.001
    #      Propane                                                  
    #T54(42:51,IY,IS)=CLTSALT(1:10,IY)*.001
    z[44] = dfd['CLTSALT'].loc[3]*.001
    #      Compressed/Liquefied Natural Gas                         
    #T54(42:51,IY,IS)=CLTSALT(1:10,IY)*.001
    z[45] = dfd['CLTSALT'].loc[4]*.001
    #      Ethanol-Flex Fuel                                        
    #T54(42:51,IY,IS)=CLTSALT(1:10,IY)*.001
    z[46] = dfd['CLTSALT'].loc[5]*.001
    #      Electric                                                 
    #T54(42:51,IY,IS)=CLTSALT(1:10,IY)*.001
    z[47] = dfd['CLTSALT'].loc[6]*.001
    #      Plug-in Gasoline Hybrid                                  
    #T54(42:51,IY,IS)=CLTSALT(1:10,IY)*.001
    z[48] = dfd['CLTSALT'].loc[7]*.001
    #      Plug-in Diesel Hybrid                                    
    #T54(42:51,IY,IS)=CLTSALT(1:10,IY)*.001
    z[49] = dfd['CLTSALT'].loc[8]*.001
    #      Fuel Cell                                                
    #T54(42:51,IY,IS)=CLTSALT(1:10,IY)*.001
    z[50] = dfd['CLTSALT'].loc[9]*.001
    #      Fuel Cell Battery Dominant          
    z[51] = dfd['CLTSALT'].loc[10]*.001
    #      Gasoline HEV
    z[52] = dfd['CLTSALT'].loc[11]*.001
    #      H2 ICE           
    z[53] = dfd['CLTSALT'].loc[12]*.001
    #         Total Commercial Light Trucks 
    z[54] = z[42] + z[43] + z[44] + z[45] + z[46] + z[47] + z[48] + z[49] + z[50] + z[52] + z[53]
    #         Total Commercial Light Truck Sales                    
    #T54(42:51,IY,IS)=CLTSALT(1:10,IY)*.001
#    z[51] = dfd['CLTSALT'].loc[10]*.001
    return z                                                               