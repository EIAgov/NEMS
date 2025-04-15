#-*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_060(dfd, table_spec, table_id):
    """Fill table  Technology Market Penetration in Light-Duty Vehicles
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
    
    SZO: 
        1. Need check with EIA to see if MKT_D_P is available in the restart. 
           If available, code could be simple using 
              def calc_mkt_peneration(M3, TRTECH)
        2. Why coded again for z[199] to z[214] at the bottom?
    
    """
	
    z = {}
    
    CAL_YR=1 #IY+BASEYR-1

    def calc_mkt_peneration(M3, TRTECH):
        df = dfd['MKT_D_P'].loc[M3].loc[TRTECH]
        # Convert the DataFrame into a dictionary with index as keys and rows as DataFrames
        df_d = {idx: df.loc[[idx]] for idx in df.index}
        return df_d
    
    i = 1
    j = 71
    TRTECH = range(i, j+1)
    
    # # Car [1, 1:71]
    M3 = 1
    z_car = calc_mkt_peneration(M3, TRTECH)

    # # Light Truck [2, 1:71]
    M3 = 2
    z_lt = calc_mkt_peneration(M3, TRTECH)
    # Update dictionary with each key increased by 86
    z_lt = {key + 71: value for key, value in z_lt.items()}

    # #  Light-Duty Vehicle Total [3, 1:71]
    M3 = 3
    z_ldv = calc_mkt_peneration(M3, TRTECH)
    # Update dictionary with each key increased by 2*86
    z_ldv = {key + 2*71: value for key, value in z_ldv.items()}

    # Combine Car, Light Truck, and Light-Duty Vehicle
    z.update(z_car)
    z.update(z_lt)
    z.update(z_ldv)

    
    return z
 
    
