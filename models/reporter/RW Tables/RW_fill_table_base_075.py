# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_075(dfd, table_spec, table_id):
    """Fill table for Average Technology Cost for Light-Duty Vehicles

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

    def calc_avg_cost(M3, TRTECH):
        # TODO: Remove Hard-coded years
        df = (
            dfd["AVGCOST"].loc[M3].loc[TRTECH]
            / dfd["MC_JPGDP"].loc[table_spec["first_year"]]
            * dfd["MC_JPGDP"].loc[table_spec["growth_rate_start"]]
        )
        # Convert the DataFrame into a dictionary with index as keys and rows as DataFrames
        df_d = {idx: df.loc[[idx]] for idx in df.index}
        return df_d

    i = 1
    j = 71
    TRTECH = range(i, j + 1)

    # # Car [1, 1:71]
    M3 = 1
    z_car = calc_avg_cost(M3, TRTECH)

    # # Light Truck [2, 1:71]
    M3 = 2
    z_lt = calc_avg_cost(M3, TRTECH)
    # Update dictionary with each key increased by 71
    z_lt = {key + j: value for key, value in z_lt.items()}

    # #  Light-Duty Vehicle Total [3, 1:71]
    M3 = 3
    z_ldv = calc_avg_cost(M3, TRTECH)
    # Update dictionary with each key increased by 2*71
    z_ldv = {key + 2 * j: value for key, value in z_ldv.items()}

    # Combine Car, Light Truck, and Light-Duty Vehicle
    z.update(z_car)
    z.update(z_lt)
    z.update(z_ldv)

    ## need to put AVGCOST in the preprocessor ###

    return z
