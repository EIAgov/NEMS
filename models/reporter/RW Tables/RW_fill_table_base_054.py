# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""

from RW_preprocessor import tdm_powertrain

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
    
    #   Transportation Fleet Car and Truck Sales by Type and Technology
    #   (thousands)
    #    Technology Type                                            
    #                                                               
    #   New Car Sales 1/                                            
    #    Conventional Cars                                          
    z[1] = dfd['FLTECHRPT'].loc[1].loc[tdm_powertrain['conv_gas']]
    z[2] = dfd['FLTECHRPT'].loc[1].loc[tdm_powertrain['diesel']]
    
    #        Total Conventional Cars                                
    z[3] = z[1]+z[2]
    #                                                               
    #    Alternative-Fuel Cars                                      
    z[5] = dfd['FLTECHRPT'].loc[1].loc[tdm_powertrain['E85']]
    z[6] = dfd['FLTECHRPT'].loc[1].loc[tdm_powertrain['EV100']]
    z[7] = dfd['FLTECHRPT'].loc[1].loc[tdm_powertrain['EV200']]
    z[15] = dfd['FLTECHRPT'].loc[1].loc[tdm_powertrain['EV300']]
    z[8] = dfd['FLTECHRPT'].loc[1].loc[tdm_powertrain['PHEV20']]
    z[4] = dfd['FLTECHRPT'].loc[1].loc[tdm_powertrain['PHEV50']]
    z[9] = dfd['FLTECHRPT'].loc[1].loc[tdm_powertrain['HEV_D']]
    z[10] = dfd['FLTECHRPT'].loc[1].loc[tdm_powertrain['HEV_G']]
    z[11] = dfd['FLTECHRPT'].loc[1].loc[tdm_powertrain['NG_dedicated']]
    z[12] = dfd['FLTECHRPT'].loc[1].loc[tdm_powertrain['NG_bifuel']]
    z[13] = dfd['FLTECHRPT'].loc[1].loc[tdm_powertrain['LPG_dedicated']]
    z[14] = dfd['FLTECHRPT'].loc[1].loc[tdm_powertrain['LPG_bifuel']]
    z[16] = dfd['FLTECHRPT'].loc[1].loc[tdm_powertrain['FC_methanol']]
    z[17] = dfd['FLTECHRPT'].loc[1].loc[tdm_powertrain['FC_hydrogen']]
    
    #        Total Alternative Cars                                 
    z[18] = z[4]+z[5]+z[6]+z[7]+z[8]+z[9]+z[10]+z[11]+z[12]+z[13]+z[14]+z[15]+z[16]+z[17]
    
    #    Total New Car Sales                                        
    z[20] = z[3]+z[18]

    #    Percent Alternative Car Sales                              
    z[19] = z[18]/ z[20]*100

    #   New Light Truck Sales 1/                                    
    #    Conventional Light Trucks                                  
    z[21] = dfd['FLTECHRPT'].loc[2].loc[tdm_powertrain['conv_gas']]
    z[22] = dfd['FLTECHRPT'].loc[2].loc[tdm_powertrain['diesel']]
    
    #        Total Conventional Light Trucks                        
    z[23] = z[21]+z[22]

    #    Alternative-Fuel Light Trucks                              
    z[25] = dfd['FLTECHRPT'].loc[2].loc[tdm_powertrain['E85']]
    z[26] = dfd['FLTECHRPT'].loc[2].loc[tdm_powertrain['EV100']]
    z[27] = dfd['FLTECHRPT'].loc[2].loc[tdm_powertrain['EV200']]
    z[35] = dfd['FLTECHRPT'].loc[2].loc[tdm_powertrain['EV300']]
    z[28] = dfd['FLTECHRPT'].loc[2].loc[tdm_powertrain['PHEV20']]
    z[24] = dfd['FLTECHRPT'].loc[2].loc[tdm_powertrain['PHEV50']]
    z[29] = dfd['FLTECHRPT'].loc[2].loc[tdm_powertrain['HEV_D']]
    z[30] = dfd['FLTECHRPT'].loc[2].loc[tdm_powertrain['HEV_G']]
    z[31] = dfd['FLTECHRPT'].loc[2].loc[tdm_powertrain['NG_dedicated']]
    z[32] = dfd['FLTECHRPT'].loc[2].loc[tdm_powertrain['NG_bifuel']]
    z[33] = dfd['FLTECHRPT'].loc[2].loc[tdm_powertrain['LPG_dedicated']]
    z[34] = dfd['FLTECHRPT'].loc[2].loc[tdm_powertrain['LPG_bifuel']]
    z[36] = dfd['FLTECHRPT'].loc[2].loc[tdm_powertrain['FC_methanol']]
    z[37] = dfd['FLTECHRPT'].loc[2].loc[tdm_powertrain['FC_hydrogen']]
    #        Total Alternative Light Trucks                         
    z[38] = z[24]+z[25]+z[26]+z[27]+z[28]+z[29]+z[30]+z[31]+z[32]+z[33]+z[34]+z[35]+z[36]+z[37]
    #    Total New Light Truck Sales                                
    z[40] = z[23]+z[38]

    #    Percent Alternative Light Truck Sales                      
    z[39] = z[38]/z[40]*100
    #                                                               
    #   Total Fleet Vehicles                                        
    z[41] = z[20]+z[40]
    #                                                               
    #   Commercial Light Truck Sales 2/                             
    #      Motor Gasoline                                           
    z[42] = dfd['CLTSALT'].loc[1]*.001
    #      Diesel                                                   
    z[43] = dfd['CLTSALT'].loc[2]*.001
    #      Propane                                                  
    z[44] = dfd['CLTSALT'].loc[3]*.001
    #      Compressed/Liquefied Natural Gas                         
    z[45] = dfd['CLTSALT'].loc[4]*.001
    #      Ethanol-Flex Fuel                                        
    z[46] = dfd['CLTSALT'].loc[5]*.001
    #      Electric                                                 
    z[47] = dfd['CLTSALT'].loc[6]*.001
    #      Plug-in Gasoline Hybrid                                  
    z[48] = dfd['CLTSALT'].loc[7]*.001
    #      Plug-in Diesel Hybrid                                    
    z[49] = dfd['CLTSALT'].loc[8]*.001
    #      Fuel Cell                                                
    z[50] = dfd['CLTSALT'].loc[9]*.001
    #      Fuel Cell Battery Dominant          
    z[51] = dfd['CLTSALT'].loc[10]*.001
    #      Gasoline HEV
    z[52] = dfd['CLTSALT'].loc[11]*.001
    #      H2 ICE           
    z[53] = dfd['CLTSALT'].loc[12]*.001
    #         Total Commercial Light Trucks 
    z[54] = z[42] + z[43] + z[44] + z[45] + z[46] + z[47] + z[48] + z[49] + z[50] + z[52] + z[53]

    return z                                                               