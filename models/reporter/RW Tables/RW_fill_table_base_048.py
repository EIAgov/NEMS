
# -*- coding: utf-8 -*-
"""
Created on Mon Jan 3 08:30:06 2024

@author: SZO
"""

from RW_preprocessor import tdm_powertrain, tdm_fuel


def fill_table_base_048(dfd, table_spec, table_id):
    """Fill table for Light-Duty Vehicle Sales by Technology Type.
   
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
    
    """
    
    z = {}

    # New Car Sales 1/
    #  Conventional Cars-----------------
    z[1] = dfd['TRLDSALC'].loc[tdm_powertrain['conv_gas']] * 1000.
    z[2] = dfd['TRLDSALC'].loc[tdm_powertrain['diesel']] * 1000.
    z[3] = z[1] + z[2]
    
    #  Alternative-Fuel Cars----------------
    z[5] = dfd['TRLDSALC'].loc[tdm_powertrain['E85']] * 1000.
    z[6] = dfd['TRLDSALC'].loc[tdm_powertrain['EV100']] * 1000.
    z[7] = dfd['TRLDSALC'].loc[tdm_powertrain['EV200']] * 1000.
    z[15] = dfd['TRLDSALC'].loc[tdm_powertrain['EV300']] * 1000.
    z[8] = dfd['TRLDSALC'].loc[tdm_powertrain['PHEV20']] * 1000.
    z[4] = dfd['TRLDSALC'].loc[tdm_powertrain['PHEV50']] * 1000.
    z[9] = dfd['TRLDSALC'].loc[tdm_powertrain['HEV_D']] * 1000.
    z[10] = dfd['TRLDSALC'].loc[tdm_powertrain['HEV_G']] * 1000.
    z[11] = dfd['TRLDSALC'].loc[tdm_powertrain['NG_dedicated']] * 1000.
    z[12] = dfd['TRLDSALC'].loc[tdm_powertrain['NG_bifuel']] * 1000.
    z[13] = dfd['TRLDSALC'].loc[tdm_powertrain['LPG_dedicated']] * 1000.
    z[14] = dfd['TRLDSALC'].loc[tdm_powertrain['LPG_bifuel']] * 1000.
    z[16] = dfd['TRLDSALC'].loc[tdm_powertrain['FC_methanol']] * 1000.
    z[17] = dfd['TRLDSALC'].loc[tdm_powertrain['FC_hydrogen']] * 1000.
    z[18] = z[4] + z[5] + z[6] + z[7] + z[8] + z[9] + z[10] + z[11] + z[12] + z[13] + z[14] + z[15] + z[16] + z[17]

    # ----------------
    z[20] = z[3] + z[18]
    z[19] = z[18] / z[20] * 100.


    # New Light Truck Sales 2/--------------
    z[21] = dfd['TRLDSALT'].loc[tdm_powertrain['conv_gas']] * 1000.
    z[22] = dfd['TRLDSALT'].loc[tdm_powertrain['diesel']] * 1000.
    z[23] =  z[21] + z[22]
    
    #  Alternative-Fuel Light Trucks----------------
    z[25] = dfd['TRLDSALT'].loc[tdm_powertrain['E85']] * 1000.
    z[26] = dfd['TRLDSALT'].loc[tdm_powertrain['EV100']] * 1000.
    z[27] = dfd['TRLDSALT'].loc[tdm_powertrain['EV200']] * 1000.
    z[35] = dfd['TRLDSALT'].loc[tdm_powertrain['EV300']] * 1000.
    z[28] = dfd['TRLDSALT'].loc[tdm_powertrain['PHEV20']] * 1000.
    z[24] = dfd['TRLDSALT'].loc[tdm_powertrain['PHEV50']] * 1000.
    z[29] = dfd['TRLDSALT'].loc[tdm_powertrain['HEV_D']] * 1000.
    z[30] = dfd['TRLDSALT'].loc[tdm_powertrain['HEV_G']] * 1000.
    z[31] = dfd['TRLDSALT'].loc[tdm_powertrain['NG_dedicated']] * 1000.
    z[32] = dfd['TRLDSALT'].loc[tdm_powertrain['NG_bifuel']] * 1000.
    z[33] = dfd['TRLDSALT'].loc[tdm_powertrain['LPG_dedicated']] * 1000.
    z[34] = dfd['TRLDSALT'].loc[tdm_powertrain['LPG_bifuel']] * 1000.
    z[36] = dfd['TRLDSALT'].loc[tdm_powertrain['FC_methanol']] * 1000.
    z[37] = dfd['TRLDSALT'].loc[tdm_powertrain['FC_hydrogen']] * 1000.
    z[38] = z[24] + z[25] + z[26] + z[27] + z[28] + z[29] + z[30] + z[31] + z[32] + z[33] + z[34] + z[35] + z[36] + z[37]

# -----------------
    z[40] = z[23] + z[38]
    z[39] = z[38] / z[40] * 100.

# ---------------
    z[41] = dfd['LEGALTSAL'].loc[1] * 100.
    z[42] = dfd['LEGALTSAL'].loc[2] * 1000.
    z[43] = dfd['LEGALTSAL'].loc[3] * 1000.

# Total Sales, Cars and Light Trucks--------------
    #   Conventional Gasoline
    z[51] = (dfd['TRLDSALC'].loc[1] + dfd['TRLDSALT'].loc[1] )* 1000.

    #   TDI Diesel
    z[45] = (dfd['TRLDSALC'].loc[2] + dfd['TRLDSALT'].loc[2]) * 1000.

    #   Flex-Fuel
    z[47] = (dfd['TRLDSALC'].loc[3] + dfd['TRLDSALT'].loc[3]) * 1000.

    # Ethanol (not reported)
    z[52] = z[47] *0.0

    #   Electric
    z[49] = (dfd['TRLDSALC'].loc[7] + dfd['TRLDSALT'].loc[7] + dfd['TRLDSALC'].loc[4]
            + dfd['TRLDSALT'].loc[4] + dfd['TRLDSALC'].loc[15] + dfd['TRLDSALT'].loc[15]) * 1000.

    #   Plug-in Electric Hybrid
    z[53] = (dfd['TRLDSALC'].loc[5] + dfd['TRLDSALT'].loc[5] + dfd['TRLDSALC'].loc[6] + dfd['TRLDSALT'].loc[6]) * 1000.

    #   Electric Hybrid
    z[46] = (dfd['TRLDSALC'].loc[8] + dfd['TRLDSALT'].loc[8] + dfd['TRLDSALC'].loc[16] + dfd['TRLDSALT'].loc[16]) * 1000.

    #Gaseous
    z[48] = (dfd['TRLDSALC'].loc[9] + dfd['TRLDSALT'].loc[9] + dfd['TRLDSALC'].loc[10] + dfd['TRLDSALT'].loc[10]
            + dfd['TRLDSALC'].loc[11] + dfd['TRLDSALT'].loc[11] + dfd['TRLDSALC'].loc[12] + dfd['TRLDSALT'].loc[12]) * 1000.

    #Fuel Cell
    z[50] = (dfd['TRLDSALC'].loc[13] + dfd['TRLDSALT'].loc[13] + dfd['TRLDSALC'].loc[14] + dfd['TRLDSALT'].loc[14]) * 1000.

    #Total Vehicles Sales
    z[44] = z[20] + z[40]

# Sales of Microhybrids (engine off at idle)------------------
    #Conventional Gasoline Microhybrids
    z[54] = (dfd['TRMICROS'].loc[(1,1), :] + dfd['TRMICROS'].loc[(2,1), :]) * 1000.

    # TDI Diesel Microhybrids
    z[55] = (dfd['TRMICROS'].loc[(1,2), :] + dfd['TRMICROS'].loc[(2,2), :]) * 1000.

    #    Flex-Fuel
    z[59] = (dfd['TRMICROS'].loc[(1,3), :] + dfd['TRMICROS'].loc[(2,3), :]) * 1000.

    #Ethanol (not reported)
    z[56] = z[59] * 0.0

    #   Electric
    z[61] = (dfd['TRMICROS'].loc[(1,7), :] + dfd['TRMICROS'].loc[(2,7), :] + dfd['TRMICROS'].loc[1,4]
            + dfd['TRMICROS'].loc[(2,4), :] + dfd['TRMICROS'].loc[(1,15), :] + dfd['TRMICROS'].loc[(2,15), :]) * 1000.

    #   Plug-in Electric Hybrid
    z[58] = (dfd['TRMICROS'].loc[(1,5), :] + dfd['TRMICROS'].loc[(2,5), :]
            + dfd['TRMICROS'].loc[(1,6), :] + dfd['TRMICROS'].loc[(2,6), :]) * 1000.

    #   Electric Hybrid
    z[57] = (dfd['TRMICROS'].loc[(1,8), :] + dfd['TRMICROS'].loc[(2,8), :]
            + dfd['TRMICROS'].loc[(1,16), :] + dfd['TRMICROS'].loc[(2,16), :]) * 1000.

    #Gaseous
    z[60] = (dfd['TRMICROS'].loc[(1,9), :] + dfd['TRMICROS'].loc[(2,9), :] + dfd['TRMICROS'].loc[(1,10), :]
            + dfd['TRMICROS'].loc[(2,10), :] + dfd['TRMICROS'].loc[(1,11), :] + dfd['TRMICROS'].loc[(2,11), :]
            + dfd['TRMICROS'].loc[(1,12), :] + dfd['TRMICROS'].loc[(2,12), :]) * 1000.

    #   Gaseous
    z[62] = (dfd['TRMICROS'].loc[(1,13), :] + dfd['TRMICROS'].loc[(2,13), :]
            + dfd['TRMICROS'].loc[(1,14), :] + dfd['TRMICROS'].loc[(2,14), :]) * 1000.

    #      Total Microhybrids
    z[63] = z[54] + z[55] + z[57] + z[58] + z[59] + z[60] + z[61] + z[62] +z[56]


# Total Alternative-Fueled Vehicle Sales------------ 
    z[64] = z[46] + z[47] + z[48] + z[49] + z[50] + z[52] + z[53]


    # Credit Bank---------------------
    # T48(65,IR,IY,IS)=ZEV_CREDIT_BANK(IR,1,IY)
    # MNUMC2	MAXZEV 9 3
    # '''Below variables have only 9 regions, make padding by filling
    #  region 10 with 0
    #  region 11 with sum of region 1 to 9'''
    # TODO: check should be sum or average?
    # 
    df = dfd['ZEV_CREDIT_BANK'].loc[(slice(None), 1), :]
    df = df.reset_index(level=1, drop=True)
    df.loc[10] = 0
    df.loc[11] = df.sum()
    z[65] = df #* 1000.


    # T48(66,IR,IY,IS)=ZEV_CREDIT_BANK(IR,2,IY)
    df = dfd['ZEV_CREDIT_BANK'].loc[(slice(None), 2), :]
    df = df.reset_index(level=1, drop=True)
    df.loc[10] = 0
    df.loc[11] = df.sum()
    z[66] = df #* 1000.

    # T48(67,IR,IY,IS)=ZEV_CREDIT_BANK(IR,3,IY)
    df = dfd['ZEV_CREDIT_BANK'].loc[(slice(None), 3), :]
    df = df.reset_index(level=1, drop=True)
    df.loc[10] = 0
    df.loc[11] = df.sum()
    z[67] = df #* 1000.


    return z













