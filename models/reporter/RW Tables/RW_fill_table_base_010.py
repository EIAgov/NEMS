# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_010(dfd, table_spec, table_id):
    """Fill table Electricity Trade.

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

    import pandas as pd

    z = {}

    FYRPRC = int(table_spec["settings"]["year_index_real_dol_priceyr"])
    # Get inflation index from 1990->2050
    MC_JPGDP = dfd["MC_JPGDP"].loc[1990:]

    # Get inflation index of FYRPRC (2022)
    SCALPR2 = MC_JPGDP.loc[FYRPRC]  # 2022 = column 33

    # Set dimensional parameters
    # locals().update(table_spec['d_par'])

    MNUMNR = dfd["MNUMNR_rwpre"]
    TRIL_TO_QUAD = dfd["TRIL_TO_QUAD_rwpre"]

    # Electricity Trade
    # (billion kilowatthours, unless otherwise noted)
    # Electricity Trade
    #
    # Interregional Electricity Trade
    #
    # Gross Domestic Sales
    # Firm Power
    # T10(1,IY,IS)=UTEXMF(MNUMNR,IY)*.001
    z[1] = dfd["UTEXMF"].loc[MNUMNR] * TRIL_TO_QUAD
    # Economy
    # T10(2,IY,IS)=UTEXME(MNUMNR,IY)*.001
    z[2] = dfd["UTEXME"].loc[MNUMNR] * TRIL_TO_QUAD
    # Total Gross Domestic Sales
    # T10(3,IY,IS)=(UTEXMF(MNUMNR,IY)+UTEXME(MNUMNR,IY))*.001
    z[3] = (dfd["UTEXMF"].loc[MNUMNR] + dfd["UTEXME"].loc[MNUMNR]) * TRIL_TO_QUAD

    # Gross Domestic Sales (million #### dollars)
    # Firm Power
    # T10(4,IY,IS)=UTEXDF(MNUMNR,IY)
    z[4] = dfd["UTEXDF"].loc[MNUMNR] * SCALPR2
    # Economy
    # T10(5,IY,IS)=UTEXDE(MNUMNR,IY)
    z[5] = dfd["UTEXDE"].loc[MNUMNR] * SCALPR2
    # Total Gross Domestic Sales
    # T10(6,IY,IS)=UTEXDF(MNUMNR,IY)+UTEXDE(MNUMNR,IY)
    z[6] = z[4] + z[5]

    # International Electricity Trade
    # Imports from Canada and Mexico
    # Firm Power
    # T10(7,IY,IS)=UTIMPF(MNUMNR,IY)*.001
    z[7] = dfd["UTIMPF"].loc[MNUMNR] * TRIL_TO_QUAD
    # Economy
    # T10(8,IY,IS)=UTIMPE(MNUMNR,IY)*.001
    z[8] = dfd["UTIMPE"].loc[MNUMNR] * TRIL_TO_QUAD
    # Total Imports from Canada and Mexico
    # T10(9,IY,IS)=(UTIMPF(MNUMNR,IY)+UTIMPE(MNUMNR,IY))*.001
    z[9] = (dfd["UTIMPF"].loc[MNUMNR] + dfd["UTIMPE"].loc[MNUMNR]) * TRIL_TO_QUAD
    # Exports to Canada and Mexico
    # Firm Power
    # T10(10,IY,IS)=UTEXPF(MNUMNR,IY)*.001
    z[10] = dfd["UTEXPF"].loc[MNUMNR] * TRIL_TO_QUAD
    # Economy
    # T10(11,IY,IS)=UTEXPE(MNUMNR,IY)*.001
    z[11] = dfd["UTEXPE"].loc[MNUMNR] * TRIL_TO_QUAD
    # Total Exports to Canada and Mexico
    # T10(12,IY,IS)=(UTEXPF(MNUMNR,IY)+UTEXPE(MNUMNR,IY))*.001
    z[12] = (dfd["UTEXPF"].loc[MNUMNR] + dfd["UTEXPE"].loc[MNUMNR]) * TRIL_TO_QUAD

    return z
