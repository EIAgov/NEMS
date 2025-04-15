# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""
import pandas as pd


def fill_table_base_079(dfd, table_spec, table_id):
    """Fill table Lower 48 Onshore Crude Oil Production by Region and Type

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

    #   Lower 48 Onshore Crude Oil Production by Region and Type
    #   (million barrels per day)
    #    Region and Type
    #
    #   Regional Production
    #     Non-Enhanced Oil Recovery
    #       East
    # T79(16:22,IY,IS)=OGREGPRD(1:7,1,IY)
    z[1] = dfd["OGREGPRD"].loc[1].loc[1]
    #       Gulf Coast
    # T79(16:22,IY,IS)=OGREGPRD(1:7,1,IY)
    z[2] = dfd["OGREGPRD"].loc[2].loc[1]
    #       Midcontinent
    # T79(16:22,IY,IS)=OGREGPRD(1:7,1,IY)
    z[3] = dfd["OGREGPRD"].loc[3].loc[1]
    #       Southwest
    # T79(16:22,IY,IS)=OGREGPRD(1:7,1,IY)
    z[4] = dfd["OGREGPRD"].loc[4].loc[1]
    #       Rocky Mountain
    # T79(16:22,IY,IS)=OGREGPRD(1:7,1,IY)
    z[5] = dfd["OGREGPRD"].loc[5].loc[1]
    #       Northern Great Plains
    # T79(16:22,IY,IS)=OGREGPRD(1:7,1,IY)
    z[6] = dfd["OGREGPRD"].loc[6].loc[1]
    #       West Coast
    # T79(16:22,IY,IS)=OGREGPRD(1:7,1,IY)
    z[7] = dfd["OGREGPRD"].loc[7].loc[1]
    # T79(37,IY,IS)=FSUM(T79(16,IY,IS),7)
    z[22] = z[1] + z[2] + z[3] + z[4] + z[5] + z[6] + z[7]
    #     Enhanced Oil Recovery
    #       East
    # T79(23:29,IY,IS)=OGREGPRD(1:7,2,IY)
    z[8] = dfd["OGREGPRD"].loc[1].loc[2]
    #       Gulf Coast
    z[9] = dfd["OGREGPRD"].loc[2].loc[2]
    #       Midcontinent
    z[10] = dfd["OGREGPRD"].loc[3].loc[2]
    #       Southwest
    z[11] = dfd["OGREGPRD"].loc[4].loc[2]
    #       Rocky Mountain
    z[12] = dfd["OGREGPRD"].loc[5].loc[2]
    #       Northern Great Plains
    z[13] = dfd["OGREGPRD"].loc[6].loc[2]
    #       West Coast
    z[14] = dfd["OGREGPRD"].loc[7].loc[2]
    # T79(38,IY,IS)=FSUM(T79(23,IY,IS),7)
    z[23] = z[8] + z[9] + z[10] + z[11] + z[12] + z[13] + z[14]
    #     Total Onshore
    #       East
    # T79(30:36,IY,IS)=T79(16:22,IY,IS)+T79(23:29,IY,IS)
    z[15] = z[1] + z[8]
    #       Gulf Coast
    z[16] = z[2] + z[9]
    #       Midcontinent
    z[17] = z[3] + z[10]
    #       Southwest
    z[18] = z[4] + z[11]
    #       Rocky Mountain
    z[19] = z[5] + z[12]
    #       Northern Great Plains
    z[20] = z[6] + z[13]
    #       West Coast
    z[21] = z[7] + z[14]
    # T79(39,IY,IS)=FSUM(T79(30,IY,IS),7)
    z[24] = z[15] + z[16] + z[17] + z[18] + z[19] + z[20] + z[21]

    return z
