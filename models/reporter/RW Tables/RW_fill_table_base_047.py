# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM

SZO fixed TRQLDV TRANSP Lt Duty Vehicle Energy Use on 9/4/2024
"""

from RW_preprocessor import tdm_powertrain, tdm_fuel

def fill_table_base_047(dfd, table_spec, table_id):
    """Fill table Light-Duty Vehicle Energy Consumption by Technology Type and Fuel Type

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
    Move the fix for TRQLDV to preprocessor_base later
    """

    z = {}

    MNUMCR = dfd["MNUMCR_rwpre"]            

    #   Light-Duty Vehicle Energy Consumption by Technology Type and Fuel Type
    #   (trillion Btu)
    #    Technology Type
    #   Light-Duty Consumption by Technology Type 1/
    #   Conventional Vehicles
    z[1] = dfd["TRLDQTEK"].loc[tdm_powertrain['conv_gas']]
    z[2] = dfd["TRLDQTEK"].loc[tdm_powertrain['diesel']]
    #   Total Conventional
    z[18] = z[1] + z[2]
    
    #  Alternative-tdm_fuel Vehicles
    z[4] = dfd["TRLDQTEK"].loc[tdm_powertrain['E85']]
    z[5] = dfd["TRLDQTEK"].loc[tdm_powertrain['EV100']]
    z[11] = dfd["TRLDQTEK"].loc[tdm_powertrain['EV200']]
    z[15] = dfd["TRLDQTEK"].loc[tdm_powertrain['EV300']]
    z[12] = dfd["TRLDQTEK"].loc[tdm_powertrain['PHEV20']]
    z[3] = dfd["TRLDQTEK"].loc[tdm_powertrain['PHEV50']]
    z[13] = dfd["TRLDQTEK"].loc[tdm_powertrain['HEV_D']]
    z[14] = dfd["TRLDQTEK"].loc[tdm_powertrain['HEV_G']]
    z[7] = dfd["TRLDQTEK"].loc[tdm_powertrain['NG_dedicated']]
    z[8] = dfd["TRLDQTEK"].loc[tdm_powertrain['NG_bifuel']]
    z[9] = dfd["TRLDQTEK"].loc[tdm_powertrain['LPG_dedicated']]
    z[10] = dfd["TRLDQTEK"].loc[tdm_powertrain['LPG_bifuel']]
    z[16] = dfd["TRLDQTEK"].loc[tdm_powertrain['FC_methanol']]
    z[17] = dfd["TRLDQTEK"].loc[tdm_powertrain['FC_hydrogen']]

    # Total Alternative
    z[19] = (
        z[3]
        + z[4]
        + z[5]
        + z[7]
        + z[8]
        + z[9]
        + z[10]
        + z[11]
        + z[12]
        + z[13]
        + z[14]
        + z[15]
        + z[16]
        + z[17]
    )

    #   Total
    z[20] = z[18] + z[19]

    # Light-Duty Consumption by tdm_fuel Type
    z[21] = dfd["TRQLDV"].loc[tdm_fuel['gasoline']].loc[MNUMCR]
    z[22] = dfd["TRQLDV"].loc[tdm_fuel['diesel']].loc[MNUMCR]
    z[23] = dfd["TRQLDV"].loc[tdm_fuel['M85']].loc[MNUMCR]
    z[24] = dfd["TRQLDV"].loc[tdm_fuel['E85']].loc[MNUMCR]
    z[25] = dfd["TRQLDV"].loc[tdm_fuel['NG']].loc[MNUMCR]
    z[26] = dfd["TRQLDV"].loc[tdm_fuel['LPG']].loc[MNUMCR]
    z[27] = dfd["TRQLDV"].loc[tdm_fuel['electricity']].loc[MNUMCR]
    z[28] = dfd["TRQLDV"].loc[tdm_fuel['hydrogen']].loc[MNUMCR]

    return z
