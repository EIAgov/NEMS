# -*- coding: utf-8 -*-
"""
Created on Wed Dec 4 10:54:06 2024

@author: ADS
"""
import pandas as pd


def fill_table_base_081(dfd, table_spec, table_id):
    """Fill table Shale Gas & Tight Oil Production by Play

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

    #   Shale Gas & Tight Oil Production by Play
    #   (trillion cubic feet/million barrels per day)
    #    Region and Type
    #
    #   Shale Gas Plays
    # T80(1:15,IY,IS)=OGQSHLGAS(1:15,IY)

    #     Barnett
    z[1] = dfd["OGQSHLGAS"].loc[1]

    #     Haynesville/Bossier
    z[2] = dfd["OGQSHLGAS"].loc[2]

    #     Fayetteville
    z[3] = dfd["OGQSHLGAS"].loc[3]

    #     Woodford
    z[4] = dfd["OGQSHLGAS"].loc[4]

    #     Eagle Ford
    z[5] = dfd["OGQSHLGAS"].loc[5]

    #     Antrim
    z[6] = dfd["OGQSHLGAS"].loc[6]

    #     Marcellus
    z[7] = dfd["OGQSHLGAS"].loc[7]

    #     Bakken
    z[8] = dfd["OGQSHLGAS"].loc[8]

    #     Utica
    z[9] = dfd["OGQSHLGAS"].loc[9]

    #     Permian
    z[10] = dfd["OGQSHLGAS"].loc[10]

    #     Other
    z[11] = dfd["OGQSHLGAS"].loc[15]

    #     ?
    z[12] = dfd["OGQSHLGAS"].loc[12]

    #     ?
    z[13] = dfd["OGQSHLGAS"].loc[13]

    #     ?
    z[14] = dfd["OGQSHLGAS"].loc[14]

    #     ?
    z[15] = dfd["OGQSHLGAS"].loc[15]

    #       Total
    # T80(56,IY,IS)=SUM(OGQSHLGAS(1:15,IY))
    z[16] = pd.concat([z[i].to_frame().T for i in range(1, 12)]).sum()

    #   Tight Oil Plays
    # T79(1:15,IY,IS)=OGQSHLOIL(1:15,IY)
    #     Bakken
    z[17] = dfd["OGQSHLOIL"].loc[1]

    #     Eagle Ford
    z[18] = dfd["OGQSHLOIL"].loc[2]

    #     Woodford
    z[19] = dfd["OGQSHLOIL"].loc[3]

    #     Austin Chalk
    z[20] = dfd["OGQSHLOIL"].loc[4]

    #     Spraberry
    z[21] = dfd["OGQSHLOIL"].loc[5]

    #     Niobrara
    z[22] = dfd["OGQSHLOIL"].loc[6]

    #     Avalon/Bone Springs
    z[23] = dfd["OGQSHLOIL"].loc[7]

    #     Monterey
    z[24] = dfd["OGQSHLOIL"].loc[8]

    #     Wolfcamp
    z[25] = dfd["OGQSHLOIL"].loc[9]

    #     Utica
    z[26] = dfd["OGQSHLOIL"].loc[10]

    #     Other
    z[27] = dfd["OGQSHLOIL"].loc[11:15].sum()

    #     Monterey
    z[28] = dfd["OGQSHLOIL"].loc[12]

    #     Monterey
    z[29] = dfd["OGQSHLOIL"].loc[13]

    #     Monterey
    z[30] = dfd["OGQSHLOIL"].loc[14]

    #     Monterey
    z[31] = dfd["OGQSHLOIL"].loc[15]

    #       Total
    # T79(40,IY,IS)=SUM(OGQSHLOIL(1:15,IY))
    z[32] = pd.concat([z[i].to_frame().T for i in range(17, 28)]).sum()


    return z
