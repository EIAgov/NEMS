# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""
import pandas as pd


def fill_table_base_080(dfd, table_spec, table_id):
    """Fill table  Lower 48 Onshore Natural Gas Production by Region and Type

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

    #   Lower 48 Onshore Natural Gas Production by Region and Type
    #   (trillion cubic feet)
    #    Region and Type
    #
    #   Regional Production
    #     Conventional
    # T80(16:22,IY,IS)=OGREGPRD(1:7,3,IY)+OGREGPRD(1:7,4,IY)
    #       East
    z[1] = dfd["OGREGPRD"].loc[1].loc[3] + dfd["OGREGPRD"].loc[1].loc[4]
    #       Gulf Coast
    z[2] = dfd["OGREGPRD"].loc[2].loc[3] + dfd["OGREGPRD"].loc[2].loc[4]
    #       Midcontinent
    z[3] = dfd["OGREGPRD"].loc[3].loc[3] + dfd["OGREGPRD"].loc[3].loc[4]
    #       Southwest
    z[4] = dfd["OGREGPRD"].loc[4].loc[3] + dfd["OGREGPRD"].loc[4].loc[4]
    #       Rocky Mountain
    z[5] = dfd["OGREGPRD"].loc[5].loc[3] + dfd["OGREGPRD"].loc[5].loc[4]
    #       Northern Great Plains
    z[6] = dfd["OGREGPRD"].loc[6].loc[3] + dfd["OGREGPRD"].loc[6].loc[4]
    #       West Coast
    z[7] = dfd["OGREGPRD"].loc[7].loc[3] + dfd["OGREGPRD"].loc[7].loc[4]
    #
    #   Regional Production
    #     Tight
    # T80(23:29,IY,IS)=OGREGPRD(1:7,5,IY)
    #       East
    z[8] = dfd["OGREGPRD"].loc[1].loc[5]
    #       Gulf Coast
    z[9] = dfd["OGREGPRD"].loc[2].loc[5]
    #       Midcontinent
    z[10] = dfd["OGREGPRD"].loc[3].loc[5]
    #       Southwest
    z[11] = dfd["OGREGPRD"].loc[4].loc[5]
    #       Rocky Mountain
    z[12] = dfd["OGREGPRD"].loc[5].loc[5]
    #       Northern Great Plains
    z[13] = dfd["OGREGPRD"].loc[6].loc[5]
    #       West Coast
    z[14] = dfd["OGREGPRD"].loc[7].loc[5]
    #   Regional Production
    #     Tight
    # T80(30:36,IY,IS)=OGREGPRD(1:7,6,IY)
    #       East
    z[15] = dfd["OGREGPRD"].loc[1].loc[6]
    #       Gulf Coast
    z[16] = dfd["OGREGPRD"].loc[2].loc[6]
    #       Midcontinent
    z[17] = dfd["OGREGPRD"].loc[3].loc[6]
    #       Southwest
    z[18] = dfd["OGREGPRD"].loc[4].loc[6]
    #       Rocky Mountain
    z[19] = dfd["OGREGPRD"].loc[5].loc[6]
    #       Northern Great Plains
    z[20] = dfd["OGREGPRD"].loc[6].loc[6]
    #       West Coast
    z[21] = dfd["OGREGPRD"].loc[7].loc[6]
    #   Regional Production
    #     Coalbed Methane
    #       East
    # T80(37:43,IY,IS)=OGREGPRD(1:7,7,IY)
    z[22] = dfd["OGREGPRD"].loc[1].loc[7]
    #       Gulf Coast
    # T80(37:43,IY,IS)=OGREGPRD(1:7,7,IY)
    z[23] = dfd["OGREGPRD"].loc[2].loc[7]
    #       Midcontinent
    # T80(37:43,IY,IS)=OGREGPRD(1:7,7,IY)
    z[24] = dfd["OGREGPRD"].loc[3].loc[7]
    #       Southwest
    # T80(37:43,IY,IS)=OGREGPRD(1:7,7,IY)
    z[25] = dfd["OGREGPRD"].loc[4].loc[7]
    #       Rocky Mountain
    # T80(37:43,IY,IS)=OGREGPRD(1:7,7,IY)
    z[26] = dfd["OGREGPRD"].loc[5].loc[7]
    #       Northern Great Plains
    # T80(37:43,IY,IS)=OGREGPRD(1:7,7,IY)
    z[27] = dfd["OGREGPRD"].loc[6].loc[7]
    #       West Coast
    # T80(37:43,IY,IS)=OGREGPRD(1:7,7,IY)
    z[28] = dfd["OGREGPRD"].loc[7].loc[7]
    #   Regional Production
    #     Total Onshore
    # T80(44:50,IY,IS)=T80(16:22,IY,IS)+T80(23:29,IY,IS)+T80(30:36,IY,IS)+T80(37:43,IY,IS)
    #       East
    z[29] = z[1] + z[8] + z[15] + z[22]
    #       Gulf Coast
    z[30] = z[2] + z[9] + z[16] + z[23]
    #       Midcontinent
    z[31] = z[3] + z[10] + z[17] + z[24]
    #       Southwest
    z[32] = z[4] + z[11] + z[18] + z[25]
    #       Rocky Mountain
    z[33] = z[5] + z[12] + z[19] + z[26]
    #       Northern Great Plains
    z[34] = z[6] + z[13] + z[20] + z[27]
    #       West Coast
    z[35] = z[7] + z[14] + z[21] + z[28]
    # T80(51,IY,IS)=FSUM(T80(16,IY,IS),7)
    z[36] = z[1] + z[2] + z[3] + z[4] + z[5] + z[6] + z[7]
    #     Tight Gas
    # T80(52,IY,IS)=FSUM(T80(23,IY,IS),7)
    z[37] = z[8] + z[9] + z[10] + z[11] + z[12] + z[13] + z[14]
    #
    #     Shale Gas
    # T80(53,IY,IS)=FSUM(T80(30,IY,IS),7)
    z[38] = z[15] + z[16] + z[17] + z[18] + z[19] + z[20] + z[21]
    #
    #     Coalbed Methane
    # T80(54,IY,IS)=FSUM(T80(37,IY,IS),7)
    z[39] = z[22] + z[23] + z[24] + z[25] + z[26] + z[27] + z[28]
    #
    #     Total Onshore
    # T80(55,IY,IS)=FSUM(T80(44,IY,IS),7)
    z[40] = z[29] + z[30] + z[31] + z[32] + z[33] + z[34] + z[35]

    return z
