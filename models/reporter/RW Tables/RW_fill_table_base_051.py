# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""

from RW_preprocessor import tdm_powertrain

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
    z[1] =  dfd['TRLDVMT'].loc[tdm_powertrain['conv_gas']]
    z[2] = dfd['TRLDVMT'].loc[tdm_powertrain['diesel']]
    z[5] = dfd['TRLDVMT'].loc[tdm_powertrain['E85']]
    z[6] = dfd['TRLDVMT'].loc[tdm_powertrain['EV100']]
    z[7] = dfd['TRLDVMT'].loc[tdm_powertrain['EV200']]
    z[15] = dfd['TRLDVMT'].loc[tdm_powertrain['EV300']]
    z[8] = dfd['TRLDVMT'].loc[tdm_powertrain['PHEV20']]
    z[4] = dfd['TRLDVMT'].loc[tdm_powertrain['PHEV50']]
    z[9] = dfd['TRLDVMT'].loc[tdm_powertrain['HEV_D']]
    z[10] = dfd['TRLDVMT'].loc[tdm_powertrain['HEV_G']]
    z[11] = dfd['TRLDVMT'].loc[tdm_powertrain['NG_dedicated']]
    z[12] = dfd['TRLDVMT'].loc[tdm_powertrain['NG_bifuel']]
    z[13] = dfd['TRLDVMT'].loc[tdm_powertrain['LPG_dedicated']]
    z[14] = dfd['TRLDVMT'].loc[tdm_powertrain['LPG_bifuel']]
    z[16] = dfd['TRLDVMT'].loc[tdm_powertrain['FC_methanol']]
    z[17] = dfd['TRLDVMT'].loc[tdm_powertrain['FC_hydrogen']]

    #   VMT Equation Components
    #     Total VMT (billion miles)
    z[21] = dfd['TRLDVMTE'].loc[1]
    #     VMT/Licensed Driver (thousand miles)
    z[22] = dfd['TRLDVMTE'].loc[2]
    #     Licensed Drivers (million)
    z[23] = dfd['TRLDVMTE'].loc[3]
    #   Price Effects
    #     Motor Gasoline Price (1987 $/million Btu)
    z[24] = dfd['TRLDVMTE'].loc[4]
    #     Household Stock Miles per Gallon
    z[25] = dfd['TRLDVMTE'].loc[5]
    #     Real Cost of Driving per Mile (1987 cents)
    z[26] = dfd['TRLDVMTE'].loc[6]
    #     Licensing Rate
    z[27] = dfd['TRLDVMTE'].loc[7]
    #   Income Effects
    #     Disposable Income per Licensed Driver
    z[28] = dfd['TRLDVMTE'].loc[8]
    #     Point Income Elasticity (ratio)
    z[29] = dfd['TRLDVMTE'].loc[9]
    #   Demographic Driving Population Effect
    #     Percent Female Driving Population
    z[30] = dfd['TRLDVMTE'].loc[10]
    #     Vehicle Miles Traveled per Vehicle
    z[31] = dfd['TRLDVMTE'].loc[11]

    return z                                                                 
