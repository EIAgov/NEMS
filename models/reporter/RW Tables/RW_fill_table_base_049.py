# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_049(dfd, table_spec, table_id):
    """Fill table  Light-Duty Vehicle Stock by Technology Type
   
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
	
    import pandas as pd
    z = {}
    
    #   Light-Duty Vehicle Stock by Technology Type
    #   (millions)
    #    Technology Type                                            
    #                                                               
    #   Car Stock 1/                                                
    #    Conventional Cars                                          
    #      Gasoline ICE Vehicles                                    
    #T49(1,IY,IS)=TRLDSTKC(1,IY)
    z[1] = dfd['TRLDSTKC'].loc[1]
    #      TDI Diesel ICE                                           
    #T49(2,IY,IS)=TRLDSTKC(2,IY)
    z[2] = dfd['TRLDSTKC'].loc[2]
    #        Total Conventional Cars                                
    #T49(3,IY,IS)=T49(1,IY,IS)+T49(2,IY,IS)
    z[3] = z[1]+z[2]                                               
    #    Alternative-Fuel Cars                                      
    #      Ethanol-Flex Fuel ICE                                    
    #T49(5,IY,IS)=TRLDSTKC(3,IY)
    z[5] = dfd['TRLDSTKC'].loc[3]
    #      100-Mile Electric Vehicle                                
    #T49(6,IY,IS)=TRLDSTKC(4,IY)
    z[6] = dfd['TRLDSTKC'].loc[4]
    #      200-Mile Electric Vehicle                                
    #T49(7,IY,IS)=TRLDSTKC(7,IY)
    z[7] = dfd['TRLDSTKC'].loc[7]
    #      300-Mile Electric Vehicle                                
    #T49(15,IY,IS)=TRLDSTKC(15,IY)
    z[15] = dfd['TRLDSTKC'].loc[15]
    #      Plug-in 20 Gasoline Hybrid                               
    #T49(8,IY,IS)=TRLDSTKC(5,IY)
    z[8] = dfd['TRLDSTKC'].loc[5]
    #      Plug-in 50 Gasoline Hybrid                               
    #T49(4,IY,IS)=TRLDSTKC(6,IY)
    z[4] = dfd['TRLDSTKC'].loc[6]
    #      Electric-Diesel Hybrid                                   
    #T49(9,IY,IS)=TRLDSTKC(8,IY)
    z[9] = dfd['TRLDSTKC'].loc[8]
    #      Electric-Gasoline Hybrid                                 
    #T49(10,IY,IS)=TRLDSTKC(16,IY)
    z[10] = dfd['TRLDSTKC'].loc[16]
    #      Natural Gas ICE                                          
    #T49(11,IY,IS)=TRLDSTKC(11,IY)
    z[11] = dfd['TRLDSTKC'].loc[11]
    #      Natural Gas Bi-fuel                                      
    #T49(12,IY,IS)=TRLDSTKC(9,IY)
    z[12] = dfd['TRLDSTKC'].loc[9]
    #      Propane ICE                                              
    #T49(13,IY,IS)=TRLDSTKC(12,IY)
    z[13] = dfd['TRLDSTKC'].loc[12]
    #      Propane Bi-fuel                                          
    #T49(14,IY,IS)=TRLDSTKC(10,IY)
    z[14] = dfd['TRLDSTKC'].loc[10]
    #      Fuel Cell Methanol                                       
    #T49(16,IY,IS)=TRLDSTKC(13,IY)
    z[16] = dfd['TRLDSTKC'].loc[13]
    #      Fuel Cell Hydrogen                                       
    #T49(17,IY,IS)=TRLDSTKC(14,IY)
    z[17] = dfd['TRLDSTKC'].loc[14]
    #        Total Alternative Cars                                 
    #T49(18,IY,IS)=FSUM(T49(4,IY,IS),14)
    z[18] = z[4]+z[5]+z[6]+z[7]+z[8]+z[9]+z[10]+z[11]+z[12]+z[13]+z[14]+z[15]+z[16]+z[17]
    #   Total Car Stock                                             
    #T49(20,IY,IS)=T49(3,IY,IS)+T49(18,IY,IS)
    z[20] = z[3]+z[18]                                            
    #   Light Truck Stock 1/                                        
    #    Conventional Light Trucks                                  
    #      Gasoline ICE Vehicles                                    
    #T49(21,IY,IS)=TRLDSTKT(1,IY)
    z[21] = dfd['TRLDSTKT'].loc[1]
    #      TDI Diesel ICE                                           
    #T49(22,IY,IS)=TRLDSTKT(2,IY)
    z[22] = dfd['TRLDSTKT'].loc[2]
    #        Total Conventional Light Trucks                        
    #T49(23,IY,IS)=T49(21,IY,IS)+T49(22,IY,IS)
    z[23] = z[21]+z[22]                                            
    #    Alternative-Fuel Light Trucks                              
    #      Ethanol-Flex Fuel ICE                                    
    #T49(25,IY,IS)=TRLDSTKT(3,IY)
    z[25] = dfd['TRLDSTKT'].loc[3]
    #      100-Mile Electric Vehicle                                
    #T49(26,IY,IS)=TRLDSTKT(4,IY)
    z[26] = dfd['TRLDSTKT'].loc[4]
    #      200-Mile Electric Vehicle                                
    #T49(27,IY,IS)=TRLDSTKT(7,IY)
    z[27] = dfd['TRLDSTKT'].loc[7]
    #      300-Mile Electric Vehicle                                
    #T49(35,IY,IS)=TRLDSTKT(15,IY)
    z[35] = dfd['TRLDSTKT'].loc[15]
    #      Plug-in 20 Gasoline Hybrid                               
    #T49(28,IY,IS)=TRLDSTKT(5,IY)
    z[28] = dfd['TRLDSTKT'].loc[5]
    #      Plug-in 50 Gasoline Hybrid                               
    #T49(24,IY,IS)=TRLDSTKT(6,IY)
    z[24] = dfd['TRLDSTKT'].loc[6]
    #      Electric-Diesel Hybrid                                   
    #T49(29,IY,IS)=TRLDSTKT(8,IY)
    z[29] = dfd['TRLDSTKT'].loc[8]
    #      Electric-Gasoline Hybrid                                 
    #T49(30,IY,IS)=TRLDSTKT(16,IY)
    z[30] = dfd['TRLDSTKT'].loc[16]
    #      Natural Gas ICE                                          
    #T49(31,IY,IS)=TRLDSTKT(11,IY)
    z[31] = dfd['TRLDSTKT'].loc[11]
    #      Natural Gas Bi-fuel                                      
    #T49(32,IY,IS)=TRLDSTKT(9,IY)
    z[32] = dfd['TRLDSTKT'].loc[9]
    #      Propane ICE                                              
    #T49(33,IY,IS)=TRLDSTKT(12,IY)
    z[33] = dfd['TRLDSTKT'].loc[12]
    #      Propane Bi-fuel                                          
    #T49(34,IY,IS)=TRLDSTKT(10,IY)
    z[34] = dfd['TRLDSTKT'].loc[10]
    #      Fuel Cell Methanol                                       
    #T49(36,IY,IS)=TRLDSTKT(13,IY)
    z[36] = dfd['TRLDSTKT'].loc[13]
    #      Fuel Cell Hydrogen                                       
    #T49(37,IY,IS)=TRLDSTKT(14,IY)
    z[37] = dfd['TRLDSTKT'].loc[14]
    #        Total Alternative Light Trucks                         
    #T49(38,IY,IS)=FSUM(T49(24,IY,IS),14)
    z[38] = z[24]+z[25]+z[26]+z[27]+z[28]+z[29]+z[30]+z[31]+z[32]+z[33]+z[34]+z[35]+z[36]+z[37]
    #   Total Light Truck Stock                                     
    #T49(40,IY,IS)=T49(23,IY,IS)+T49(38,IY,IS)
    z[40] = z[23]+z[38]                                            
    #   Total Stock, Cars and Light Trucks                          
    #T49(41,IY,IS)=T49(20,IY,IS)+T49(40,IY,IS)
    z[41] = z[20]+z[40]
    #      Conventional Gasoline                                    
    #T49(42,IR,IY,IS) = TRLDSTKT( 1,IR,IY) + TRLDSTKC( 1,IR,IY)
    z[42] = dfd['TRLDSTKT'].loc[1]+dfd['TRLDSTKC'].loc[1]
    #      TDI Diesel                                               
    #T49(43,IR,IY,IS) = TRLDSTKT( 2,IR,IY) + TRLDSTKC( 2,IR,IY)
    z[43] = dfd['TRLDSTKT'].loc[2]+dfd['TRLDSTKC'].loc[2]
    #      Flex-Fuel                                                
    #T49(46,IR,IY,IS) = TRLDSTKT( 3,IR,IY) + TRLDSTKC( 3,IR,IY)
    z[46] = dfd['TRLDSTKT'].loc[3]+dfd['TRLDSTKC'].loc[3]
    #      Electric                                                 
    #T49(48,IR,IY,IS) = TRLDSTKT( 7,IR,IY) + TRLDSTKC( 7,IR,IY) + TRLDSTKT( 4,IR,IY) + TRLDSTKC( 4,IR,IY) + TRLDSTKT(15,IR,IY) + TRLDSTKC(15,IR,IY)
    z[48] = dfd['TRLDSTKT'].loc[7]+dfd['TRLDSTKC'].loc[7]+dfd['TRLDSTKT'].loc[4]+dfd['TRLDSTKC'].loc[4]+dfd['TRLDSTKT'].loc[15]+dfd['TRLDSTKC'].loc[15]
    #      Plug-in Electric Hybrid                                  
    #T49(45,IR,IY,IS) = TRLDSTKT( 5,IR,IY) + TRLDSTKC( 5,IR,IY) + TRLDSTKT( 6,IR,IY) + TRLDSTKC( 6,IR,IY)
    z[45] = dfd['TRLDSTKT'].loc[5]+dfd['TRLDSTKC'].loc[5]+dfd['TRLDSTKT'].loc[6]+dfd['TRLDSTKC'].loc[6]
    #      Electric Hybrid                                          
    #T49(44,IR,IY,IS) = TRLDSTKT( 8,IR,IY) + TRLDSTKC( 8,IR,IY) + TRLDSTKT(16,IR,IY) + TRLDSTKC(16,IR,IY)
    z[44] = dfd['TRLDSTKT'].loc[8]+dfd['TRLDSTKC'].loc[8]+dfd['TRLDSTKT'].loc[16]+dfd['TRLDSTKC'].loc[16]
    #      Gaseous (Propane and Natural Gas)                        
    #T49(47,IR,IY,IS) = TRLDSTKT( 9,IR,IY) + TRLDSTKC( 9,IR,IY) + TRLDSTKT(10,IR,IY) + TRLDSTKC(10,IR,IY) + TRLDSTKT(11,IR,IY) + TRLDSTKC(11,IR,IY) + TRLDSTKT(12,IR,IY) + TRLDSTKC(12,IR,IY)
    z[47] = dfd['TRLDSTKT'].loc[9]+dfd['TRLDSTKC'].loc[9]+dfd['TRLDSTKT'].loc[10]+dfd['TRLDSTKC'].loc[10]+dfd['TRLDSTKT'].loc[11]+dfd['TRLDSTKC'].loc[11]+dfd['TRLDSTKT'].loc[12]+dfd['TRLDSTKC'].loc[12]
    #      Fuel Cell                                                
    #T49(49,IR,IY,IS) = TRLDSTKT(13,IR,IY) + TRLDSTKC(13,IR,IY) + TRLDSTKT(14,IR,IY) + TRLDSTKC(14,IR,IY)
    z[49] = dfd['TRLDSTKT'].loc[13]+dfd['TRLDSTKC'].loc[13]+dfd['TRLDSTKT'].loc[14]+dfd['TRLDSTKC'].loc[14]
    return z                                                               