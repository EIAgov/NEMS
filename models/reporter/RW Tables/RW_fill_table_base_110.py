# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_110(dfd, table_spec, table_id):
    """Fill table for Net Negative Revenue Results
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

    FYRPRC = int(table_spec["settings"]["year_index_real_dol_priceyr"])
    # Get inflation index from 1990->2050
    MC_JPGDP = dfd["MC_JPGDP"].loc[1990:]

    # Get inflation index of FYRPRC (2022)
    SCALPR2 = MC_JPGDP.loc[FYRPRC]  # 2022 = column 33

    NUMREP = 10
    MNUMNR = dfd["MNUMNR_rwpre"]

    #  Results are for EMM  Regions in Electricity Model
    # UNRGNS = dfd['UNRGNS'].max()
    UNRGNS = dfd["UNRGNS"].max()
    for IR in range(1, int(UNRGNS) + 1):
        z[(IR - 1) * NUMREP + 1] = dfd["UNRVCOL"].loc[IR] * SCALPR2
        z[(IR - 1) * NUMREP + 2] = dfd["UNRVCCY"].loc[IR] * SCALPR2
        z[(IR - 1) * NUMREP + 3] = dfd["UNRVSTM"].loc[IR] * SCALPR2
        z[(IR - 1) * NUMREP + 4] = dfd["UNRVNUC"].loc[IR] * SCALPR2
        z[(IR - 1) * NUMREP + 5] = (
            z[(IR - 1) * NUMREP + 1]
            + z[(IR - 1) * NUMREP + 2]
            + z[(IR - 1) * NUMREP + 3]
            + z[(IR - 1) * NUMREP + 4]
        )
        z[(IR - 1) * NUMREP + 6] = dfd["UNCPCOL"].loc[IR] / 1000.0
        z[(IR - 1) * NUMREP + 7] = dfd["UNCPCCY"].loc[IR] / 1000.0
        z[(IR - 1) * NUMREP + 8] = dfd["UNCPSTM"].loc[IR] / 1000.0
        z[(IR - 1) * NUMREP + 9] = dfd["UNCPNUC"].loc[IR] / 1000.0
        z[(IR - 1) * NUMREP + 10] = (
            z[(IR - 1) * NUMREP + 6]
            + z[(IR - 1) * NUMREP + 7]
            + z[(IR - 1) * NUMREP + 8]
            + z[(IR - 1) * NUMREP + 9]
        )
    #  National Results
    z[(MNUMNR - 3) * NUMREP + 1] = dfd["UNRVCOL"].loc[MNUMNR] * SCALPR2
    z[(MNUMNR - 3) * NUMREP + 2] = dfd["UNRVCCY"].loc[MNUMNR] * SCALPR2
    z[(MNUMNR - 3) * NUMREP + 3] = dfd["UNRVSTM"].loc[MNUMNR] * SCALPR2
    z[(MNUMNR - 3) * NUMREP + 4] = dfd["UNRVNUC"].loc[MNUMNR] * SCALPR2
    z[(MNUMNR - 3) * NUMREP + 5] = (
        z[(MNUMNR - 3) * NUMREP + 1]
        + z[(MNUMNR - 3) * NUMREP + 2]
        + z[(MNUMNR - 3) * NUMREP + 3]
        + z[(MNUMNR - 3) * NUMREP + 4]
    )
    z[(MNUMNR - 3) * NUMREP + 6] = dfd["UNCPCOL"].loc[MNUMNR] / 1000.0
    z[(MNUMNR - 3) * NUMREP + 7] = dfd["UNCPCCY"].loc[MNUMNR] / 1000.0
    z[(MNUMNR - 3) * NUMREP + 8] = dfd["UNCPSTM"].loc[MNUMNR] / 1000.0
    z[(MNUMNR - 3) * NUMREP + 9] = dfd["UNCPNUC"].loc[MNUMNR] / 1000.0
    z[(MNUMNR - 3) * NUMREP + 10] = (
        z[(MNUMNR - 3) * NUMREP + 6]
        + z[(MNUMNR - 3) * NUMREP + 7]
        + z[(MNUMNR - 3) * NUMREP + 8]
        + z[(MNUMNR - 3) * NUMREP + 9]
    )

    return z
