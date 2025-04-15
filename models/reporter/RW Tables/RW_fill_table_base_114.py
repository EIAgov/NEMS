# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_114(dfd, table_spec, table_id):
    """Fill table for New Light-Duty Vehicle Prices
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

    FYRPRC = int(table_spec["settings"]["year_index_real_dol_priceyr"])
    # Get inflation index from 1990->2050
    MC_JPGDP = dfd["MC_JPGDP"].loc[1990:]

    # Get inflation index of FYRPRC (2022)
    SCALPR2 = MC_JPGDP.loc[FYRPRC]  # 2022 = column 33
    SCALPR90 = MC_JPGDP.iloc[0]  # tran prices are in $90 instead of $87

    TNUMCLASS = dfd["TNUMCLASS_rwpre"]
    NUMREP = TNUMCLASS - 1

    # Note: Transportation prices are in $1990 in the restart file
    for II in range(1, 8 + 1):
        #        --- GASOLINE
        z[0 * NUMREP + II] = dfd["LDVPRI"].loc[1, 1, II] / 1000.0 / SCALPR90 * SCALPR2
        z[1 * NUMREP + II] = dfd["LDVPRI"].loc[2, 1, II] / 1000.0 / SCALPR90 * SCALPR2
        #         --- TDI DIESEL
        z[2 * NUMREP + II] = dfd["LDVPRI"].loc[1, 2, II] / 1000.0 / SCALPR90 * SCALPR2
        z[3 * NUMREP + II] = dfd["LDVPRI"].loc[2, 2, II] / 1000.0 / SCALPR90 * SCALPR2
        #         --- PLUG-IN 10 GASOLINE HYBRID
        z[4 * NUMREP + II] = dfd["LDVPRI"].loc[1, 5, II] / 1000.0 / SCALPR90 * SCALPR2
        z[5 * NUMREP + II] = dfd["LDVPRI"].loc[2, 5, II] / 1000.0 / SCALPR90 * SCALPR2
        #         --- PLUG-IN 40 GASOLINE HYBRID
        z[6 * NUMREP + II] = dfd["LDVPRI"].loc[1, 6, II] / 1000.0 / SCALPR90 * SCALPR2
        z[7 * NUMREP + II] = dfd["LDVPRI"].loc[2, 6, II] / 1000.0 / SCALPR90 * SCALPR2
        #         --- ETHANOL FLEX
        z[8 * NUMREP + II] = dfd["LDVPRI"].loc[1, 3, II] / 1000.0 / SCALPR90 * SCALPR2
        z[9 * NUMREP + II] = dfd["LDVPRI"].loc[2, 3, II] / 1000.0 / SCALPR90 * SCALPR2
        #         --- CNG
        z[10 * NUMREP + II] = dfd["LDVPRI"].loc[1, 11, II] / 1000.0 / SCALPR90 * SCALPR2
        z[11 * NUMREP + II] = dfd["LDVPRI"].loc[2, 11, II] / 1000.0 / SCALPR90 * SCALPR2
        #         --- CNG BI-FUEL
        z[12 * NUMREP + II] = dfd["LDVPRI"].loc[1, 9, II] / 1000.0 / SCALPR90 * SCALPR2
        z[13 * NUMREP + II] = dfd["LDVPRI"].loc[2, 9, II] / 1000.0 / SCALPR90 * SCALPR2
        #         --- LPG
        z[14 * NUMREP + II] = dfd["LDVPRI"].loc[1, 12, II] / 1000.0 / SCALPR90 * SCALPR2
        z[15 * NUMREP + II] = dfd["LDVPRI"].loc[2, 12, II] / 1000.0 / SCALPR90 * SCALPR2
        #         --- LPG BI-FUEL
        z[16 * NUMREP + II] = dfd["LDVPRI"].loc[1, 10, II] / 1000.0 / SCALPR90 * SCALPR2
        z[17 * NUMREP + II] = dfd["LDVPRI"].loc[2, 10, II] / 1000.0 / SCALPR90 * SCALPR2
        #         --- 100 MILE ELECTRIC
        z[18 * NUMREP + II] = dfd["LDVPRI"].loc[1, 4, II] / 1000.0 / SCALPR90 * SCALPR2
        z[19 * NUMREP + II] = dfd["LDVPRI"].loc[2, 4, II] / 1000.0 / SCALPR90 * SCALPR2
        #         --- 200 MILE ELECTRIC
        z[20 * NUMREP + II] = dfd["LDVPRI"].loc[1, 7, II] / 1000.0 / SCALPR90 * SCALPR2
        z[21 * NUMREP + II] = dfd["LDVPRI"].loc[2, 7, II] / 1000.0 / SCALPR90 * SCALPR2
        #         --- 300 MILE ELECTRIC
        z[22 * NUMREP + II] = dfd["LDVPRI"].loc[1, 15, II] / 1000.0 / SCALPR90 * SCALPR2
        z[23 * NUMREP + II] = dfd["LDVPRI"].loc[2, 15, II] / 1000.0 / SCALPR90 * SCALPR2
        #         --- DIESEL-ELECTRIC HYBRID
        z[24 * NUMREP + II] = dfd["LDVPRI"].loc[1, 8, II] / 1000.0 / SCALPR90 * SCALPR2
        z[25 * NUMREP + II] = dfd["LDVPRI"].loc[2, 8, II] / 1000.0 / SCALPR90 * SCALPR2
        #         --- GASOLINE-ELECTRIC HYBRID
        z[26 * NUMREP + II] = dfd["LDVPRI"].loc[1, 16, II] / 1000.0 / SCALPR90 * SCALPR2
        z[27 * NUMREP + II] = dfd["LDVPRI"].loc[2, 16, II] / 1000.0 / SCALPR90 * SCALPR2
        #         --- FUEL CELL METHANOL
        z[28 * NUMREP + II] = dfd["LDVPRI"].loc[1, 13, II] / 1000.0 / SCALPR90 * SCALPR2
        z[29 * NUMREP + II] = dfd["LDVPRI"].loc[2, 13, II] / 1000.0 / SCALPR90 * SCALPR2
        #         --- FUEL CELL HYDROGEN
        z[30 * NUMREP + II] = dfd["LDVPRI"].loc[1, 14, II] / 1000.0 / SCALPR90 * SCALPR2
        z[31 * NUMREP + II] = dfd["LDVPRI"].loc[2, 14, II] / 1000.0 / SCALPR90 * SCALPR2

    #        --- AVERAGE VEHICLE PRICES - CARS, TRUCKS, AND TOTAL LIGHT DUTY VEHICLES

    z[257] = dfd["AVG_PRC_CAR"] / SCALPR90 * SCALPR2
    z[258] = dfd["AVG_PRC_TRK"] / SCALPR90 * SCALPR2
    z[259] = dfd["AVG_PRC_VEH"] / SCALPR90 * SCALPR2
    z[260] = dfd["FULL_PRC_CAR"] / SCALPR90 * SCALPR2
    z[261] = dfd["FULL_PRC_TRK"] / SCALPR90 * SCALPR2
    z[262] = dfd["FULL_PRC_VEH"] / SCALPR90 * SCALPR2
    z[263] = dfd["LI_ION_COST"].loc[15] / SCALPR90 * SCALPR2
    z[264] = dfd["LI_ION_COST"].loc[5] / SCALPR90 * SCALPR2
    z[265] = dfd["LI_ION_COST"].loc[16] / SCALPR90 * SCALPR2

    return z
