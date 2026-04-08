# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""

from RW_preprocessor import tdm_powertrain

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

    #   Light-Duty Vehicle Miles per Gallon by Technology Type
    #   (miles per gallon gasoline equivalent)
    #    Technology Type                                            
    #                                                               
    #   New Car Miles per Gallon 1/
    z[1] = dfd['TRLDMPGC'].loc[tdm_powertrain['conv_gas']]
    z[2] = dfd['TRLDMPGC'].loc[tdm_powertrain['diesel']]
    z[5] = dfd['TRLDMPGC'].loc[tdm_powertrain['E85']]
    z[6] = dfd['TRLDMPGC'].loc[tdm_powertrain['EV100']]
    z[7] = dfd['TRLDMPGC'].loc[tdm_powertrain['EV200']]
    z[15] = dfd['TRLDMPGC'].loc[tdm_powertrain['EV300']]
    z[8] = dfd['TRLDMPGC'].loc[tdm_powertrain['PHEV20']]
    z[4] = dfd['TRLDMPGC'].loc[tdm_powertrain['PHEV50']]
    z[9] = dfd['TRLDMPGC'].loc[tdm_powertrain['HEV_D']]
    z[10] = dfd['TRLDMPGC'].loc[tdm_powertrain['HEV_G']]
    z[11] = dfd['TRLDMPGC'].loc[tdm_powertrain['NG_dedicated']]
    z[12] = dfd['TRLDMPGC'].loc[tdm_powertrain['NG_bifuel']]
    z[13] = dfd['TRLDMPGC'].loc[tdm_powertrain['LPG_dedicated']]
    z[14] = dfd['TRLDMPGC'].loc[tdm_powertrain['LPG_bifuel']]
    z[16] = dfd['TRLDMPGC'].loc[tdm_powertrain['FC_methanol']]
    z[17] = dfd['TRLDMPGC'].loc[tdm_powertrain['FC_hydrogen']]


    #    Average New Cars Miles per Gallon
    z[20] = dfd['NEWMPG'].loc[1]


    #   New Light Truck Miles per Gallon 1/
    z[21] = dfd['TRLDMPGT'].loc[tdm_powertrain['conv_gas']]
    z[22] = dfd['TRLDMPGT'].loc[tdm_powertrain['diesel']]
    z[25] = dfd['TRLDMPGT'].loc[tdm_powertrain['E85']]
    z[26] = dfd['TRLDMPGT'].loc[tdm_powertrain['EV100']]
    z[27] = dfd['TRLDMPGT'].loc[tdm_powertrain['EV200']]
    z[35] = dfd['TRLDMPGT'].loc[tdm_powertrain['EV300']]
    z[28] = dfd['TRLDMPGT'].loc[tdm_powertrain['PHEV20']]
    z[24] = dfd['TRLDMPGT'].loc[tdm_powertrain['PHEV50']]
    z[29] = dfd['TRLDMPGT'].loc[tdm_powertrain['HEV_D']]
    z[30] = dfd['TRLDMPGT'].loc[tdm_powertrain['HEV_G']]
    z[31] = dfd['TRLDMPGT'].loc[tdm_powertrain['NG_dedicated']]
    z[32] = dfd['TRLDMPGT'].loc[tdm_powertrain['NG_bifuel']]
    z[33] = dfd['TRLDMPGT'].loc[tdm_powertrain['LPG_dedicated']]
    z[34] = dfd['TRLDMPGT'].loc[tdm_powertrain['LPG_bifuel']]
    z[36] = dfd['TRLDMPGT'].loc[tdm_powertrain['FC_methanol']]
    z[37] = dfd['TRLDMPGT'].loc[tdm_powertrain['FC_hydrogen']]

    #    Average Light Truck Miles per Gallon
    z[40] =  dfd['NEWMPG'].loc[2]


    #   Average New Vehicle Miles per Gallon
    z[41] = dfd['NEWMPG'].loc[3]

    #   Average Car Stock Miles per Gallon 2/
    z[42] =  dfd['TRLDMPGF'].loc[1]

    #   Average Light Truck Stock Miles per Gallon 2/
    z[43] =  dfd['TRLDMPGF'].loc[2]


    #   Average Vehicle Stock Miles per Gallon 2/
    z[44] =  dfd['TRLDMPGF'].loc[3]


    #   New Car, Light Truck, and Commercial
    #    Light Truck Miles per Gallon 2/
    #      Motor Gasoline, Internal Combustion or Other
    z[45] =  dfd['TECHMPG'].loc[1]

    #      Diesel, Internal Combustion or Other
    z[46] =  dfd['TECHMPG'].loc[2]

    #      Natural Gas, Internal Combustion
    z[47] =  dfd['TECHMPG'].loc[3]

    #      Propane, Internal Combustion
    z[48] =  dfd['TECHMPG'].loc[4]

    #      Other, Internal Combustion or Hybrid
    z[49] =  dfd['TECHMPG'].loc[5]

    #      Electric Vehicles
    z[50] =  dfd['TECHMPG'].loc[6]

    #      Hydrogen Fuel Cell
    z[51] =  dfd['TECHMPG'].loc[7]

    #      Plug-in Hybrids
    z[52] =  dfd['TECHMPG'].loc[8]

    #   Average New Vehicle Miles per Gallon +2B 2/
    z[53] = dfd['LDV_MPG'].loc[2]


    #   Stock Car, Light Truck, and Commercial
    #    Light Truck Miles per Gallon 2/
    #      Motor Gasoline, Internal Combustion or Other
    z[54] = dfd['STKMPG'].loc[1]

    #      Diesel, Internal Combustion or Other
    z[55] = dfd['STKMPG'].loc[2]

    #      Natural Gas, Internal Combustion
    z[56] = dfd['STKMPG'].loc[3]

    #      Propane, Internal Combustion
    z[57] = dfd['STKMPG'].loc[4]

    #      Other, Internal Combustion or Hybrid
    z[58] = dfd['STKMPG'].loc[5]

    #      Electric Vehicles
    z[59] = dfd['STKMPG'].loc[6]

    #      Hydrogen Fuel Cell
    z[60] = dfd['STKMPG'].loc[7]

    #      Plug-in Hybrids
    z[61] = dfd['STKMPG'].loc[8]

    #   Average Vehicle Stock Miles per Gallon +2B 2/
    z[62] = dfd['LDV_MPG'].loc[3]

    return z                                                               
