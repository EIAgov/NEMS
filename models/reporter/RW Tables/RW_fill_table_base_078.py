# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_078(dfd, table_spec, table_id):
    """Fill table  Natural Gas Delivered Prices by End-Use Sector and Census Division

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
    TRIL_TO_QUAD = dfd["TRIL_TO_QUAD_rwpre"]
    MNUMCR = dfd["MNUMCR_rwpre"]

    #   Natural Gas Delivered Prices by End-Use Sector and Census Division
    #   (#### dollars per thousand cubic feet)
    #    Sector and Region
    #
    #   Residential
    # T78(10,IY,IS)=PNGRS(11,IY)*CFNGN(IY)
    z[10] = dfd["AMPBLK/PNGRS"].loc[MNUMCR] * dfd["CFNGN"] * SCALPR2

    #     New England
    # T78(1:9,IY,IS)=PNGRS(1:9,IY)*CFNGN(IY)
    z[1] = dfd["AMPBLK/PNGRS"].loc[1] * dfd["CFNGN"] * SCALPR2

    #     Middle Atlantic
    # T78(1:9,IY,IS)=PNGRS(1:9,IY)*CFNGN(IY)
    z[2] = dfd["AMPBLK/PNGRS"].loc[2] * dfd["CFNGN"] * SCALPR2

    #     East North Central
    # T78(1:9,IY,IS)=PNGRS(1:9,IY)*CFNGN(IY)
    z[3] = dfd["AMPBLK/PNGRS"].loc[3] * dfd["CFNGN"] * SCALPR2

    #     West North Central
    # T78(1:9,IY,IS)=PNGRS(1:9,IY)*CFNGN(IY)
    z[4] = dfd["AMPBLK/PNGRS"].loc[4] * dfd["CFNGN"] * SCALPR2

    #     South Atlantic
    # T78(1:9,IY,IS)=PNGRS(1:9,IY)*CFNGN(IY)
    z[5] = dfd["AMPBLK/PNGRS"].loc[5] * dfd["CFNGN"] * SCALPR2

    #     East South Central
    # T78(1:9,IY,IS)=PNGRS(1:9,IY)*CFNGN(IY)
    z[6] = dfd["AMPBLK/PNGRS"].loc[6] * dfd["CFNGN"] * SCALPR2

    #     West South Central
    # T78(1:9,IY,IS)=PNGRS(1:9,IY)*CFNGN(IY)
    z[7] = dfd["AMPBLK/PNGRS"].loc[7] * dfd["CFNGN"] * SCALPR2

    #     Mountain
    # T78(1:9,IY,IS)=PNGRS(1:9,IY)*CFNGN(IY)
    z[8] = dfd["AMPBLK/PNGRS"].loc[8] * dfd["CFNGN"] * SCALPR2

    #     Pacific
    # T78(1:9,IY,IS)=PNGRS(1:9,IY)*CFNGN(IY)
    z[9] = dfd["AMPBLK/PNGRS"].loc[9] * dfd["CFNGN"] * SCALPR2

    #   Commercial
    # T78(20,IY,IS)=PNGCM(11,IY)*CFNGN(IY)
    z[20] = dfd["AMPBLK/PNGCM"].loc[MNUMCR] * dfd["CFNGN"] * SCALPR2

    #     New England
    # T78(11:19,IY,IS)=PNGCM(1:9,IY)*CFNGN(IY)
    z[11] = dfd["AMPBLK/PNGCM"].loc[1] * dfd["CFNGN"] * SCALPR2

    #     Middle Atlantic
    # T78(11:19,IY,IS)=PNGCM(1:9,IY)*CFNGN(IY)
    z[12] = dfd["AMPBLK/PNGCM"].loc[2] * dfd["CFNGN"] * SCALPR2

    #     East North Central
    # T78(11:19,IY,IS)=PNGCM(1:9,IY)*CFNGN(IY)
    z[13] = dfd["AMPBLK/PNGCM"].loc[3] * dfd["CFNGN"] * SCALPR2

    #     West North Central
    # T78(11:19,IY,IS)=PNGCM(1:9,IY)*CFNGN(IY)
    z[14] = dfd["AMPBLK/PNGCM"].loc[4] * dfd["CFNGN"] * SCALPR2

    #     South Atlantic
    # T78(11:19,IY,IS)=PNGCM(1:9,IY)*CFNGN(IY)
    z[15] = dfd["AMPBLK/PNGCM"].loc[5] * dfd["CFNGN"] * SCALPR2

    #     East South Central
    # T78(11:19,IY,IS)=PNGCM(1:9,IY)*CFNGN(IY)
    z[16] = dfd["AMPBLK/PNGCM"].loc[6] * dfd["CFNGN"] * SCALPR2

    #     West South Central
    # T78(11:19,IY,IS)=PNGCM(1:9,IY)*CFNGN(IY)
    z[17] = dfd["AMPBLK/PNGCM"].loc[7] * dfd["CFNGN"] * SCALPR2

    #     Mountain
    # T78(11:19,IY,IS)=PNGCM(1:9,IY)*CFNGN(IY)
    z[18] = dfd["AMPBLK/PNGCM"].loc[8] * dfd["CFNGN"] * SCALPR2

    #     Pacific
    # T78(11:19,IY,IS)=PNGCM(1:9,IY)*CFNGN(IY)
    z[19] = dfd["AMPBLK/PNGCM"].loc[9] * dfd["CFNGN"] * SCALPR2

    #   Industrial 1/
    # T78(30,IY,IS)=PNGIN(11,IY)*CFNGN(IY)
    z[30] = dfd["AMPBLK/PNGIN"].loc[MNUMCR] * dfd["CFNGN"] * SCALPR2

    #     New England
    # T78(21:29,IY,IS)=PNGIN(1:9,IY)*CFNGN(IY)
    z[21] = dfd["AMPBLK/PNGIN"].loc[1] * dfd["CFNGN"] * SCALPR2

    #     Middle Atlantic
    # T78(21:29,IY,IS)=PNGIN(1:9,IY)*CFNGN(IY)
    z[22] = dfd["AMPBLK/PNGIN"].loc[2] * dfd["CFNGN"] * SCALPR2

    #     East North Central
    # T78(21:29,IY,IS)=PNGIN(1:9,IY)*CFNGN(IY)
    z[23] = dfd["AMPBLK/PNGIN"].loc[3] * dfd["CFNGN"] * SCALPR2

    #     West North Central
    # T78(21:29,IY,IS)=PNGIN(1:9,IY)*CFNGN(IY)
    z[24] = dfd["AMPBLK/PNGIN"].loc[4] * dfd["CFNGN"] * SCALPR2

    #     South Atlantic
    # T78(21:29,IY,IS)=PNGIN(1:9,IY)*CFNGN(IY)
    z[25] = dfd["AMPBLK/PNGIN"].loc[5] * dfd["CFNGN"] * SCALPR2

    #     East South Central
    # T78(21:29,IY,IS)=PNGIN(1:9,IY)*CFNGN(IY)
    z[26] = dfd["AMPBLK/PNGIN"].loc[6] * dfd["CFNGN"] * SCALPR2

    #     West South Central
    # T78(21:29,IY,IS)=PNGIN(1:9,IY)*CFNGN(IY)
    z[27] = dfd["AMPBLK/PNGIN"].loc[7] * dfd["CFNGN"] * SCALPR2

    #     Mountain
    # T78(21:29,IY,IS)=PNGIN(1:9,IY)*CFNGN(IY)
    z[28] = dfd["AMPBLK/PNGIN"].loc[8] * dfd["CFNGN"] * SCALPR2

    #     Pacific
    # T78(21:29,IY,IS)=PNGIN(1:9,IY)*CFNGN(IY)
    z[29] = dfd["AMPBLK/PNGIN"].loc[9] * dfd["CFNGN"] * SCALPR2

    #   Electric Power 2/
    # T78(40,IY,IS)=PNGEL(11,IY)*CFNGU(IY)
    z[40] = dfd["AMPBLK/PNGEL"].loc[MNUMCR] * dfd["CFNGU"] * SCALPR2

    #     New England
    # T78(31:39,IY,IS)=PNGEL(1:9,IY)*CFNGU(IY)
    z[31] = dfd["AMPBLK/PNGEL"].loc[1] * dfd["CFNGU"] * SCALPR2

    #     Middle Atlantic
    # T78(31:39,IY,IS)=PNGEL(1:9,IY)*CFNGU(IY)
    z[32] = dfd["AMPBLK/PNGEL"].loc[2] * dfd["CFNGU"] * SCALPR2

    #     East North Central
    # T78(31:39,IY,IS)=PNGEL(1:9,IY)*CFNGU(IY)
    z[33] = dfd["AMPBLK/PNGEL"].loc[3] * dfd["CFNGU"] * SCALPR2

    #     West North Central
    # T78(31:39,IY,IS)=PNGEL(1:9,IY)*CFNGU(IY)
    z[34] = dfd["AMPBLK/PNGEL"].loc[4] * dfd["CFNGU"] * SCALPR2

    #     South Atlantic
    # T78(31:39,IY,IS)=PNGEL(1:9,IY)*CFNGU(IY)
    z[35] = dfd["AMPBLK/PNGEL"].loc[5] * dfd["CFNGU"] * SCALPR2

    #     East South Central
    # T78(31:39,IY,IS)=PNGEL(1:9,IY)*CFNGU(IY)
    z[36] = dfd["AMPBLK/PNGEL"].loc[6] * dfd["CFNGU"] * SCALPR2

    #     West South Central
    # T78(31:39,IY,IS)=PNGEL(1:9,IY)*CFNGU(IY)
    z[37] = dfd["AMPBLK/PNGEL"].loc[7] * dfd["CFNGU"] * SCALPR2

    #     Mountain
    # T78(31:39,IY,IS)=PNGEL(1:9,IY)*CFNGU(IY)
    z[38] = dfd["AMPBLK/PNGEL"].loc[8] * dfd["CFNGU"] * SCALPR2

    #     Pacific
    # T78(31:39,IY,IS)=PNGEL(1:9,IY)*CFNGU(IY)
    z[39] = dfd["AMPBLK/PNGEL"].loc[9] * dfd["CFNGU"] * SCALPR2

    #   Transportation 3/
    # T78(50,IY,IS)=PNGTR(11,IY)*CFNGN(IY)
    z[50] = dfd["AMPBLK/PNGTR"].loc[MNUMCR] * dfd["CFNGN"] * SCALPR2

    #     New England
    # T78(41:49,IY,IS)=PNGTR(1:9,IY)*CFNGN(IY)
    z[41] = dfd["AMPBLK/PNGTR"].loc[1] * dfd["CFNGN"] * SCALPR2

    #     Middle Atlantic
    # T78(41:49,IY,IS)=PNGTR(1:9,IY)*CFNGN(IY)
    z[42] = dfd["AMPBLK/PNGTR"].loc[2] * dfd["CFNGN"] * SCALPR2

    #     East North Central
    # T78(41:49,IY,IS)=PNGTR(1:9,IY)*CFNGN(IY)
    z[43] = dfd["AMPBLK/PNGTR"].loc[3] * dfd["CFNGN"] * SCALPR2

    #     West North Central
    # T78(41:49,IY,IS)=PNGTR(1:9,IY)*CFNGN(IY)
    z[44] = dfd["AMPBLK/PNGTR"].loc[4] * dfd["CFNGN"] * SCALPR2

    #     South Atlantic
    # T78(41:49,IY,IS)=PNGTR(1:9,IY)*CFNGN(IY)
    z[45] = dfd["AMPBLK/PNGTR"].loc[5] * dfd["CFNGN"] * SCALPR2

    #     East South Central
    # T78(41:49,IY,IS)=PNGTR(1:9,IY)*CFNGN(IY)
    z[46] = dfd["AMPBLK/PNGTR"].loc[6] * dfd["CFNGN"] * SCALPR2

    #     West South Central
    # T78(41:49,IY,IS)=PNGTR(1:9,IY)*CFNGN(IY)
    z[47] = dfd["AMPBLK/PNGTR"].loc[7] * dfd["CFNGN"] * SCALPR2

    #     Mountain
    # T78(41:49,IY,IS)=PNGTR(1:9,IY)*CFNGN(IY)
    z[48] = dfd["AMPBLK/PNGTR"].loc[8] * dfd["CFNGN"] * SCALPR2

    #     Pacific
    # T78(41:49,IY,IS)=PNGTR(1:9,IY)*CFNGN(IY)
    z[49] = dfd["AMPBLK/PNGTR"].loc[9] * dfd["CFNGN"] * SCALPR2

    #   All Sectors 4/
    # T78(60,IY,IS)=PNGAS(11,IY)*CFNGC(IY)
    z[60] = dfd["AMPBLK/PNGAS"].loc[MNUMCR] * dfd["CFNGC"] * SCALPR2

    #     New England
    # T78(51:59,IY,IS)=PNGAS(1:9,IY)*CFNGC(IY)
    z[51] = dfd["AMPBLK/PNGAS"].loc[1] * dfd["CFNGC"] * SCALPR2

    #     Middle Atlantic
    # T78(51:59,IY,IS)=PNGAS(1:9,IY)*CFNGC(IY)
    z[52] = dfd["AMPBLK/PNGAS"].loc[2] * dfd["CFNGC"] * SCALPR2

    #     East North Central
    # T78(51:59,IY,IS)=PNGAS(1:9,IY)*CFNGC(IY)
    z[53] = dfd["AMPBLK/PNGAS"].loc[3] * dfd["CFNGC"] * SCALPR2

    #     West North Central
    # T78(51:59,IY,IS)=PNGAS(1:9,IY)*CFNGC(IY)
    z[54] = dfd["AMPBLK/PNGAS"].loc[4] * dfd["CFNGC"] * SCALPR2

    #     South Atlantic
    # T78(51:59,IY,IS)=PNGAS(1:9,IY)*CFNGC(IY)
    z[55] = dfd["AMPBLK/PNGAS"].loc[5] * dfd["CFNGC"] * SCALPR2

    #     East South Central
    # T78(51:59,IY,IS)=PNGAS(1:9,IY)*CFNGC(IY)
    z[56] = dfd["AMPBLK/PNGAS"].loc[6] * dfd["CFNGC"] * SCALPR2

    #     West South Central
    # T78(51:59,IY,IS)=PNGAS(1:9,IY)*CFNGC(IY)
    z[57] = dfd["AMPBLK/PNGAS"].loc[7] * dfd["CFNGC"] * SCALPR2
    #     Mountain
    # T78(51:59,IY,IS)=PNGAS(1:9,IY)*CFNGC(IY)
    z[58] = dfd["AMPBLK/PNGAS"].loc[8] * dfd["CFNGC"] * SCALPR2

    #     Pacific
    # T78(51:59,IY,IS)=PNGAS(1:9,IY)*CFNGC(IY)
    z[59] = dfd["AMPBLK/PNGAS"].loc[9] * dfd["CFNGC"] * SCALPR2

    return z
