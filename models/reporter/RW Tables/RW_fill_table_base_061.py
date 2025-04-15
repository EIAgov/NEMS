# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_061(dfd, table_spec, table_id):
    """Fill table Competitive Electricity Prices
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

    #   Competitive Electricity Prices
    #   (#### mills per kilowatthour)
    #    Competitive Sector Price

    #    Competitive Sector Price
    #
    #   Residential Competitive Price

    #     Reliability Component
    # T61(1,IR,IY,IS)=PECRSRLN(IR,IY)
    z[1] = dfd["PECRSRLN"] * SCALPR2

    #     Energy Component
    # T61(2,IR,IY,IS)=PECRSMEN(IR,IY)
    z[2] = dfd["PECRSMEN"] * SCALPR2

    #     Tax Component
    # T61(3,IR,IY,IS)=PECRSTXN(IR,IY)
    z[3] = dfd["PECRSTXN"] * SCALPR2

    #     Transmission and Distribution Component
    # T61(4,IR,IY,IS)=PECRSTDN(IR,IY)
    z[4] = dfd["PECRSTDN"] * SCALPR2

    #       Total Residential
    # T61(5,IR,IY,IS)=FSUM(T61(1,IR,IY,IS),4)
    z[5] = z[1] + z[2] + z[3] + z[4]

    #

    #   Commercial Competitive Price

    #     Reliability Component
    # T61(6,IR,IY,IS)=PECCMRLN(IR,IY)
    z[6] = dfd["PECCMRLN"] * SCALPR2

    #     Energy Component
    # T61(7,IR,IY,IS)=PECCMMEN(IR,IY)
    z[7] = dfd["PECCMMEN"] * SCALPR2

    #     Tax Component
    # T61(8,IR,IY,IS)=PECCMTXN(IR,IY)
    z[8] = dfd["PECCMTXN"] * SCALPR2

    #     Transmission and Distribution Component
    # T61(9,IR,IY,IS)=PECCMTDN(IR,IY)
    z[9] = dfd["PECCMTDN"] * SCALPR2

    #       Total Commercial
    # T61(10,IR,IY,IS)=FSUM(T61(6,IR,IY,IS),4)
    z[10] = z[6] + z[7] + z[8] + z[9]

    #

    #   Industrial Competitive Price

    #     Reliability Component
    # T61(11,IR,IY,IS)=PECINRLN(IR,IY)
    z[11] = dfd["PECINRLN"] * SCALPR2

    #     Energy Component
    # T61(12,IR,IY,IS)=PECINMEN(IR,IY)
    z[12] = dfd["PECINMEN"] * SCALPR2

    #     Tax Component
    # T61(13,IR,IY,IS)=PECINTXN(IR,IY)
    z[13] = dfd["PECINTXN"] * SCALPR2

    #     Transmission and Distribution Component
    # T61(14,IR,IY,IS)=PECINTDN(IR,IY)
    z[14] = dfd["PECINTDN"] * SCALPR2

    #       Total Industrial
    # T61(15,IR,IY,IS)=FSUM(T61(11,IR,IY,IS),4)
    z[15] = z[11] + z[12] + z[13] + z[14]

    #

    #   All Sectors Competitive Price

    #     Reliability Component
    # T61(16,IR,IY,IS)=PECASRLN(IR,IY)
    z[16] = dfd["PECASRLN"] * SCALPR2

    #     Energy Component
    # T61(17,IR,IY,IS)=PECASMEN(IR,IY)
    z[17] = dfd["PECASMEN"] * SCALPR2

    #     Tax Component
    # T61(18,IR,IY,IS)=PECASTXN(IR,IY)
    z[18] = dfd["PECASTXN"] * SCALPR2

    #     Transmission and Distribution Component
    # T61(19,IR,IY,IS)=PECASTDN(IR,IY)
    z[19] = dfd["PECASTDN"] * SCALPR2

    #       Total All Sectors
    # T61(20,IR,IY,IS)=FSUM(T61(16,IR,IY,IS),4)
    z[20] = z[16] + z[17] + z[18] + z[19]

    return z
