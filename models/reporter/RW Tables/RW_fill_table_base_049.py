# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""

from RW_preprocessor import tdm_powertrain

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
    
    # Light-Duty Vehicle Stock by Technology Type
    # (millions)

    # Car Stock 1/
    # Conventional Cars
    z[1] = dfd['TRLDSTKC'].loc[tdm_powertrain['conv_gas']]
    z[2] = dfd['TRLDSTKC'].loc[tdm_powertrain['diesel']]
    # Total Conventional Cars
    z[3] = z[1]+z[2]
    
    # Alternative-Fuel Cars
    z[5] = dfd['TRLDSTKC'].loc[tdm_powertrain['E85']]
    z[6] = dfd['TRLDSTKC'].loc[tdm_powertrain['EV100']]
    z[7] = dfd['TRLDSTKC'].loc[tdm_powertrain['EV200']]
    z[15] = dfd['TRLDSTKC'].loc[tdm_powertrain['EV300']]
    z[8] = dfd['TRLDSTKC'].loc[tdm_powertrain['PHEV20']]
    z[4] = dfd['TRLDSTKC'].loc[tdm_powertrain['PHEV50']]
    z[9] = dfd['TRLDSTKC'].loc[tdm_powertrain['HEV_D']]
    z[10] = dfd['TRLDSTKC'].loc[tdm_powertrain['HEV_G']]
    z[11] = dfd['TRLDSTKC'].loc[tdm_powertrain['NG_dedicated']]
    z[12] = dfd['TRLDSTKC'].loc[tdm_powertrain['NG_bifuel']]
    z[13] = dfd['TRLDSTKC'].loc[tdm_powertrain['LPG_dedicated']]
    z[14] = dfd['TRLDSTKC'].loc[tdm_powertrain['LPG_bifuel']]
    z[16] = dfd['TRLDSTKC'].loc[tdm_powertrain['FC_methanol']]
    z[17] = dfd['TRLDSTKC'].loc[tdm_powertrain['FC_hydrogen']]
    # Total Alternative Cars
    z[18] = z[4]+z[5]+z[6]+z[7]+z[8]+z[9]+z[10]+z[11]+z[12]+z[13]+z[14]+z[15]+z[16]+z[17]
    
    #   Total Car Stock
    z[20] = z[3]+z[18]
    
    #   Light Truck Stock 1/
    # Conventional Light Trucks
    z[21] = dfd['TRLDSTKT'].loc[tdm_powertrain['conv_gas']]
    z[22] = dfd['TRLDSTKT'].loc[tdm_powertrain['diesel']]
    # Total Conventional Light Trucks
    z[23] = z[21]+z[22]
    
    # Alternative-Fuel Light Trucks
    z[25] = dfd['TRLDSTKT'].loc[tdm_powertrain['E85']]
    z[26] = dfd['TRLDSTKT'].loc[tdm_powertrain['EV100']]
    z[27] = dfd['TRLDSTKT'].loc[tdm_powertrain['EV200']]
    z[35] = dfd['TRLDSTKT'].loc[tdm_powertrain['EV300']]
    z[28] = dfd['TRLDSTKT'].loc[tdm_powertrain['PHEV20']]
    z[24] = dfd['TRLDSTKT'].loc[tdm_powertrain['PHEV50']]
    z[29] = dfd['TRLDSTKT'].loc[tdm_powertrain['HEV_D']]
    z[30] = dfd['TRLDSTKT'].loc[tdm_powertrain['HEV_G']]
    z[31] = dfd['TRLDSTKT'].loc[tdm_powertrain['NG_dedicated']]
    z[32] = dfd['TRLDSTKT'].loc[tdm_powertrain['NG_bifuel']]
    z[33] = dfd['TRLDSTKT'].loc[tdm_powertrain['LPG_dedicated']]
    z[34] = dfd['TRLDSTKT'].loc[tdm_powertrain['LPG_bifuel']]
    z[36] = dfd['TRLDSTKT'].loc[tdm_powertrain['FC_methanol']]
    z[37] = dfd['TRLDSTKT'].loc[tdm_powertrain['FC_hydrogen']]
    # Total Alternative Light Trucks
    z[38] = z[24]+z[25]+z[26]+z[27]+z[28]+z[29]+z[30]+z[31]+z[32]+z[33]+z[34]+z[35]+z[36]+z[37]
    
    #   Total Light Truck Stock
    z[40] = z[23]+z[38]
    
    #   Total Stock, Cars and Light Trucks
    z[41] = z[20]+z[40]
    
    # Conventional Gasoline
    z[42] = dfd['TRLDSTKT'].loc[tdm_powertrain['conv_gas']]+dfd['TRLDSTKC'].loc[tdm_powertrain['conv_gas']]
    # TDI Diesel
    z[43] = dfd['TRLDSTKT'].loc[tdm_powertrain['diesel']]+dfd['TRLDSTKC'].loc[tdm_powertrain['diesel']]
    # Flex-Fuel
    z[46] = dfd['TRLDSTKT'].loc[tdm_powertrain['E85']]+dfd['TRLDSTKC'].loc[tdm_powertrain['E85']]
    # Electric
    z[48] = dfd['TRLDSTKT'].loc[tdm_powertrain['EV200']]+dfd['TRLDSTKC'].loc[tdm_powertrain['EV200']]+ \
            dfd['TRLDSTKT'].loc[tdm_powertrain['EV100']]+dfd['TRLDSTKC'].loc[tdm_powertrain['EV100']]+ \
            dfd['TRLDSTKT'].loc[tdm_powertrain['EV300']]+dfd['TRLDSTKC'].loc[tdm_powertrain['EV300']]
    # Plug-in Electric Hybrid
    z[45] = dfd['TRLDSTKT'].loc[tdm_powertrain['PHEV20']]+dfd['TRLDSTKC'].loc[tdm_powertrain['PHEV20']]+ \
            dfd['TRLDSTKT'].loc[tdm_powertrain['PHEV50']]+dfd['TRLDSTKC'].loc[tdm_powertrain['PHEV50']]
    # Electric Hybrid
    z[44] = dfd['TRLDSTKT'].loc[tdm_powertrain['HEV_D']]+dfd['TRLDSTKC'].loc[tdm_powertrain['HEV_D']]+ \
            dfd['TRLDSTKT'].loc[tdm_powertrain['HEV_G']]+dfd['TRLDSTKC'].loc[tdm_powertrain['HEV_G']]
    # Gaseous (Propane and Natural Gas)
    z[47] = dfd['TRLDSTKT'].loc[tdm_powertrain['NG_bifuel']]+dfd['TRLDSTKC'].loc[tdm_powertrain['NG_bifuel']]+ \
            dfd['TRLDSTKT'].loc[tdm_powertrain['LPG_bifuel']]+dfd['TRLDSTKC'].loc[tdm_powertrain['LPG_bifuel']]+ \
            dfd['TRLDSTKT'].loc[tdm_powertrain['NG_dedicated']]+dfd['TRLDSTKC'].loc[tdm_powertrain['NG_dedicated']]+ \
            dfd['TRLDSTKT'].loc[tdm_powertrain['LPG_dedicated']]+dfd['TRLDSTKC'].loc[tdm_powertrain['LPG_dedicated']]
    # Fuel Cell
    z[49] = dfd['TRLDSTKT'].loc[tdm_powertrain['FC_methanol']]+dfd['TRLDSTKC'].loc[tdm_powertrain['FC_methanol']]+ \
            dfd['TRLDSTKT'].loc[tdm_powertrain['FC_hydrogen']]+dfd['TRLDSTKC'].loc[tdm_powertrain['FC_hydrogen']]
    
    return z                                                               