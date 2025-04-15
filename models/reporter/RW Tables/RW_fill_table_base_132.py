# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_132(dfd, table_spec, table_id):
    """Fill table  Crude Oil Refinery Input Price by Refinery Region

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

    #  Long summations cross-reference to other tables -- those lines need fixing

    #   Crude Oil Refinery Input Price by Refinery Region
    #   (#### dollars per barrel)
    #    Crude Oil Price to Refineries
    #
    #   Region 1 - PADD I
    # T132(12,IY,IS)=(T106(01,IY,IS)*T132(01,IY,IS)+T106(02,IY,IS)*T132(02,IY,IS)+T106(03,IY,IS)*T132(03,IY,IS)+T106(04,IY,IS)*T132(04,IY,IS)+T106(05,IY,IS)*T132(05,IY,IS)+T106(06,IY,IS)*T132(06,IY,IS)+T106(07,IY,IS)*T132(07,IY,IS)+T106(08,IY,IS)*T132(08,IY,IS)+T106(09,IY,IS)*T132(09,IY,IS)+T106(10,IY,IS)*T132(10,IY,IS)+T106(11,IY,IS)*T132(11,IY,IS))/SUM(T106(01:11,IY,IS))
    # z[12] = (T106(z[01])*z[01])+T106(z[02])*z[02])+T106(z[03])*z[03])+T106(z[04])*z[04])+T106(z[05])*z[05])+T106(z[06])*z[06])+T106(z[07])*z[07])+T106(z[08])*z[08])+T106(z[09])*z[09])+T106(z[10])*z[10])+ T106(z[11])*z[11])) / SUM(T106(z[01:11]))
    #     API 35-40 Sweet
    # T132(01:11,IY,IS)=P_RFCRUDEINP(1,1:11,IY)
    z[1] = dfd["P_RFCRUDEINP"].loc[1].loc[1] * SCALPR2

    #     API 35+ Sour
    # T132(01:11,IY,IS)=P_RFCRUDEINP(1,1:11,IY)
    z[2] = dfd["P_RFCRUDEINP"].loc[1].loc[2] * SCALPR2

    #     API 27-35 MediSour
    # T132(01:11,IY,IS)=P_RFCRUDEINP(1,1:11,IY)
    z[3] = dfd["P_RFCRUDEINP"].loc[1].loc[3] * SCALPR2

    #     API 27-35 Sour
    # T132(01:11,IY,IS)=P_RFCRUDEINP(1,1:11,IY)
    z[4] = dfd["P_RFCRUDEINP"].loc[1].loc[4] * SCALPR2

    #     API less than 27 Sweet
    # T132(01:11,IY,IS)=P_RFCRUDEINP(1,1:11,IY)
    z[5] = dfd["P_RFCRUDEINP"].loc[1].loc[5] * SCALPR2

    #     API less than 27 Sour
    # T132(01:11,IY,IS)=P_RFCRUDEINP(1,1:11,IY)
    z[6] = dfd["P_RFCRUDEINP"].loc[1].loc[6] * SCALPR2

    #     California
    # T132(01:11,IY,IS)=P_RFCRUDEINP(1,1:11,IY)
    z[7] = dfd["P_RFCRUDEINP"].loc[1].loc[7] * SCALPR2

    #     Syncrude
    # T132(01:11,IY,IS)=P_RFCRUDEINP(1,1:11,IY)
    z[8] = dfd["P_RFCRUDEINP"].loc[1].loc[8] * SCALPR2

    #     Dilbit/Synbit
    # T132(01:11,IY,IS)=P_RFCRUDEINP(1,1:11,IY)
    z[9] = dfd["P_RFCRUDEINP"].loc[1].loc[9] * SCALPR2

    #     API 40-50 Sweet
    # T132(01:11,IY,IS)=P_RFCRUDEINP(1,1:11,IY)
    z[10] = dfd["P_RFCRUDEINP"].loc[1].loc[10] * SCALPR2

    #     API 50+ Sweet
    # T132(01:11,IY,IS)=P_RFCRUDEINP(1,1:11,IY)
    z[11] = dfd["P_RFCRUDEINP"].loc[1].loc[11] * SCALPR2

    z[12] = (
        dfd["RFCRUDEINP"].loc[1].loc[1] * z[1]
        + dfd["RFCRUDEINP"].loc[1].loc[2] * z[2]
        + dfd["RFCRUDEINP"].loc[1].loc[3] * z[3]
        + dfd["RFCRUDEINP"].loc[1].loc[4] * z[4]
        + dfd["RFCRUDEINP"].loc[1].loc[5] * z[5]
        + dfd["RFCRUDEINP"].loc[1].loc[6] * z[6]
        + dfd["RFCRUDEINP"].loc[1].loc[7] * z[7]
        + dfd["RFCRUDEINP"].loc[1].loc[8] * z[8]
        + dfd["RFCRUDEINP"].loc[1].loc[9] * z[9]
        + dfd["RFCRUDEINP"].loc[1].loc[10] * z[10]
        + dfd["RFCRUDEINP"].loc[1].loc[11] * z[11]
    ) / dfd["RFCRUDEINP"].loc[1, 1:11, :].sum().replace(0, 1)

    #   Region 2 - PADD II Inland

    #     API 35-40 Sweet
    # T132(13:23,IY,IS)=P_RFCRUDEINP(2,1:11,IY)
    z[13] = dfd["P_RFCRUDEINP"].loc[2].loc[1] * SCALPR2

    #     API 35+ Sour
    # T132(13:23,IY,IS)=P_RFCRUDEINP(2,1:11,IY)
    z[14] = dfd["P_RFCRUDEINP"].loc[2].loc[2] * SCALPR2

    #     API 27-35 MediSour
    # T132(13:23,IY,IS)=P_RFCRUDEINP(2,1:11,IY)
    z[15] = dfd["P_RFCRUDEINP"].loc[2].loc[3] * SCALPR2

    #     API 27-35 Sour
    # T132(13:23,IY,IS)=P_RFCRUDEINP(2,1:11,IY)
    z[16] = dfd["P_RFCRUDEINP"].loc[2].loc[4] * SCALPR2

    #     API less than 27 Sweet
    # T132(13:23,IY,IS)=P_RFCRUDEINP(2,1:11,IY)
    z[17] = dfd["P_RFCRUDEINP"].loc[2].loc[5] * SCALPR2

    #     API less than 27 Sour
    # T132(13:23,IY,IS)=P_RFCRUDEINP(2,1:11,IY)
    z[18] = dfd["P_RFCRUDEINP"].loc[2].loc[6] * SCALPR2

    #     California
    # T132(13:23,IY,IS)=P_RFCRUDEINP(2,1:11,IY)
    z[19] = dfd["P_RFCRUDEINP"].loc[2].loc[7] * SCALPR2

    #     Syncrude
    # T132(13:23,IY,IS)=P_RFCRUDEINP(2,1:11,IY)
    z[20] = dfd["P_RFCRUDEINP"].loc[2].loc[8] * SCALPR2

    #     Dilbit/Synbit
    # T132(13:23,IY,IS)=P_RFCRUDEINP(2,1:11,IY)
    z[21] = dfd["P_RFCRUDEINP"].loc[2].loc[9] * SCALPR2

    #     API 40-50 Sweet
    # T132(13:23,IY,IS)=P_RFCRUDEINP(2,1:11,IY)
    z[22] = dfd["P_RFCRUDEINP"].loc[2].loc[10] * SCALPR2

    #     API 50+ Sweet
    # T132(13:23,IY,IS)=P_RFCRUDEINP(2,1:11,IY)
    z[23] = dfd["P_RFCRUDEINP"].loc[2].loc[11] * SCALPR2

    #

    #   Region 3 - PADD II Lakes

    #     API 35-40 Sweet
    # T132(25:35,IY,IS)=P_RFCRUDEINP(3,1:11,IY)
    z[25] = dfd["P_RFCRUDEINP"].loc[3].loc[1] * SCALPR2

    #     API 35+ Sour
    # T132(25:35,IY,IS)=P_RFCRUDEINP(3,1:11,IY)
    z[26] = dfd["P_RFCRUDEINP"].loc[3].loc[2] * SCALPR2

    #     API 27-35 MediSour
    # T132(25:35,IY,IS)=P_RFCRUDEINP(3,1:11,IY)
    z[27] = dfd["P_RFCRUDEINP"].loc[3].loc[3] * SCALPR2

    #     API 27-35 Sour
    # T132(25:35,IY,IS)=P_RFCRUDEINP(3,1:11,IY)
    z[28] = dfd["P_RFCRUDEINP"].loc[3].loc[4] * SCALPR2

    #     API less than 27 Sweet
    # T132(25:35,IY,IS)=P_RFCRUDEINP(3,1:11,IY)
    z[29] = dfd["P_RFCRUDEINP"].loc[3].loc[5] * SCALPR2

    #     API less than 27 Sour
    # T132(25:35,IY,IS)=P_RFCRUDEINP(3,1:11,IY)
    z[30] = dfd["P_RFCRUDEINP"].loc[3].loc[6] * SCALPR2

    #     California
    # T132(25:35,IY,IS)=P_RFCRUDEINP(3,1:11,IY)
    z[31] = dfd["P_RFCRUDEINP"].loc[3].loc[7] * SCALPR2

    #     Syncrude
    # T132(25:35,IY,IS)=P_RFCRUDEINP(3,1:11,IY)
    z[32] = dfd["P_RFCRUDEINP"].loc[3].loc[8] * SCALPR2

    #     Dilbit/Synbit
    # T132(25:35,IY,IS)=P_RFCRUDEINP(3,1:11,IY)
    z[33] = dfd["P_RFCRUDEINP"].loc[3].loc[9] * SCALPR2

    #     API 40-50 Sweet
    # T132(25:35,IY,IS)=P_RFCRUDEINP(3,1:11,IY)
    z[34] = dfd["P_RFCRUDEINP"].loc[3].loc[10] * SCALPR2

    #     API 50+ Sweet
    # T132(25:35,IY,IS)=P_RFCRUDEINP(3,1:11,IY)
    z[35] = dfd["P_RFCRUDEINP"].loc[3].loc[11] * SCALPR2
    #

    #   Region 4 - PADD III Gulf

    #     API 35-40 Sweet
    # T132(37:47,IY,IS)=P_RFCRUDEINP(4,1:11,IY)
    z[37] = dfd["P_RFCRUDEINP"].loc[4].loc[1] * SCALPR2

    #     API 35+ Sour
    # T132(37:47,IY,IS)=P_RFCRUDEINP(4,1:11,IY)
    z[38] = dfd["P_RFCRUDEINP"].loc[4].loc[2] * SCALPR2

    #     API 27-35 MediSour
    # T132(37:47,IY,IS)=P_RFCRUDEINP(4,1:11,IY)
    z[39] = dfd["P_RFCRUDEINP"].loc[4].loc[3] * SCALPR2

    #     API 27-35 Sour
    # T132(37:47,IY,IS)=P_RFCRUDEINP(4,1:11,IY)
    z[40] = dfd["P_RFCRUDEINP"].loc[4].loc[4] * SCALPR2

    #     API less than 27 Sweet
    # T132(37:47,IY,IS)=P_RFCRUDEINP(4,1:11,IY)
    z[41] = dfd["P_RFCRUDEINP"].loc[4].loc[5] * SCALPR2

    #     API less than 27 Sour
    # T132(37:47,IY,IS)=P_RFCRUDEINP(4,1:11,IY)
    z[42] = dfd["P_RFCRUDEINP"].loc[4].loc[6] * SCALPR2

    #     California
    # T132(37:47,IY,IS)=P_RFCRUDEINP(4,1:11,IY)
    z[43] = dfd["P_RFCRUDEINP"].loc[4].loc[7] * SCALPR2
    #     Syncrude
    # T132(37:47,IY,IS)=P_RFCRUDEINP(4,1:11,IY)
    z[44] = dfd["P_RFCRUDEINP"].loc[4].loc[8] * SCALPR2

    #     Dilbit/Synbit
    # T132(37:47,IY,IS)=P_RFCRUDEINP(4,1:11,IY)
    z[45] = dfd["P_RFCRUDEINP"].loc[4].loc[9] * SCALPR2

    #     API 40-50 Sweet
    # T132(37:47,IY,IS)=P_RFCRUDEINP(4,1:11,IY)
    z[46] = dfd["P_RFCRUDEINP"].loc[4].loc[10] * SCALPR2

    #     API 50+ Sweet
    # T132(37:47,IY,IS)=P_RFCRUDEINP(4,1:11,IY)
    z[47] = dfd["P_RFCRUDEINP"].loc[4].loc[11] * SCALPR2

    #

    #   Region 5 - PADD III Inland

    #     API 35-40 Sweet
    # T132(49:59,IY,IS)=P_RFCRUDEINP(5,1:11,IY)
    z[49] = dfd["P_RFCRUDEINP"].loc[5].loc[1] * SCALPR2

    #     API 35+ Sour
    # T132(49:59,IY,IS)=P_RFCRUDEINP(5,1:11,IY)
    z[50] = dfd["P_RFCRUDEINP"].loc[5].loc[2] * SCALPR2

    #     API 27-35 MediSour
    # T132(49:59,IY,IS)=P_RFCRUDEINP(5,1:11,IY)
    z[51] = dfd["P_RFCRUDEINP"].loc[5].loc[3] * SCALPR2

    #     API 27-35 Sour
    # T132(49:59,IY,IS)=P_RFCRUDEINP(5,1:11,IY)
    z[52] = dfd["P_RFCRUDEINP"].loc[5].loc[4] * SCALPR2

    #     API less than 27 Sweet
    # T132(49:59,IY,IS)=P_RFCRUDEINP(5,1:11,IY)
    z[53] = dfd["P_RFCRUDEINP"].loc[5].loc[5] * SCALPR2

    #     API less than 27 Sour
    # T132(49:59,IY,IS)=P_RFCRUDEINP(5,1:11,IY)
    z[54] = dfd["P_RFCRUDEINP"].loc[5].loc[6] * SCALPR2

    #     California
    # T132(49:59,IY,IS)=P_RFCRUDEINP(5,1:11,IY)
    z[55] = dfd["P_RFCRUDEINP"].loc[5].loc[7] * SCALPR2

    #     Syncrude
    # T132(49:59,IY,IS)=P_RFCRUDEINP(5,1:11,IY)
    z[56] = dfd["P_RFCRUDEINP"].loc[5].loc[8] * SCALPR2

    #     Dilbit/Synbit
    # T132(49:59,IY,IS)=P_RFCRUDEINP(5,1:11,IY)
    z[57] = dfd["P_RFCRUDEINP"].loc[5].loc[9] * SCALPR2

    #     API 40-50 Sweet
    # T132(49:59,IY,IS)=P_RFCRUDEINP(5,1:11,IY)
    z[58] = dfd["P_RFCRUDEINP"].loc[5].loc[10] * SCALPR2

    #     API 50+ Sweet
    # T132(49:59,IY,IS)=P_RFCRUDEINP(5,1:11,IY)
    z[59] = dfd["P_RFCRUDEINP"].loc[5].loc[11] * SCALPR2

    #

    #   Region 6 - PADD IV

    #     API 35-40 Sweet
    # T132(61:71,IY,IS)=P_RFCRUDEINP(6,1:11,IY)
    z[61] = dfd["P_RFCRUDEINP"].loc[6].loc[1] * SCALPR2

    #     API 35+ Sour
    # T132(61:71,IY,IS)=P_RFCRUDEINP(6,1:11,IY)
    z[62] = dfd["P_RFCRUDEINP"].loc[6].loc[2] * SCALPR2

    #     API 27-35 MediSour
    # T132(61:71,IY,IS)=P_RFCRUDEINP(6,1:11,IY)
    z[63] = dfd["P_RFCRUDEINP"].loc[6].loc[3] * SCALPR2

    #     API 27-35 Sour
    # T132(61:71,IY,IS)=P_RFCRUDEINP(6,1:11,IY)
    z[64] = dfd["P_RFCRUDEINP"].loc[6].loc[4] * SCALPR2

    #     API less than 27 Sweet
    # T132(61:71,IY,IS)=P_RFCRUDEINP(6,1:11,IY)
    z[65] = dfd["P_RFCRUDEINP"].loc[6].loc[5] * SCALPR2

    #     API less than 27 Sour
    # T132(61:71,IY,IS)=P_RFCRUDEINP(6,1:11,IY)
    z[66] = dfd["P_RFCRUDEINP"].loc[6].loc[6] * SCALPR2

    #     California
    # T132(61:71,IY,IS)=P_RFCRUDEINP(6,1:11,IY)
    z[67] = dfd["P_RFCRUDEINP"].loc[6].loc[7] * SCALPR2

    #     Syncrude
    # T132(61:71,IY,IS)=P_RFCRUDEINP(6,1:11,IY)
    z[68] = dfd["P_RFCRUDEINP"].loc[6].loc[8] * SCALPR2

    #     Dilbit/Synbit
    # T132(61:71,IY,IS)=P_RFCRUDEINP(6,1:11,IY)
    z[69] = dfd["P_RFCRUDEINP"].loc[6].loc[9] * SCALPR2

    #     API 40-50 Sweet
    # T132(61:71,IY,IS)=P_RFCRUDEINP(6,1:11,IY)
    z[70] = dfd["P_RFCRUDEINP"].loc[6].loc[10] * SCALPR2

    #     API 50+ Sweet
    # T132(61:71,IY,IS)=P_RFCRUDEINP(6,1:11,IY)
    z[71] = dfd["P_RFCRUDEINP"].loc[6].loc[11] * SCALPR2

    #

    #   Region 7 - PADD V California

    #     API 35-40 Sweet
    # T132(73:83,IY,IS)=P_RFCRUDEINP(7,1:11,IY)
    z[73] = dfd["P_RFCRUDEINP"].loc[7].loc[1] * SCALPR2

    #     API 35+ Sour
    # T132(73:83,IY,IS)=P_RFCRUDEINP(7,1:11,IY)
    z[74] = dfd["P_RFCRUDEINP"].loc[7].loc[2] * SCALPR2

    #     API 27-35 MediSour
    # T132(73:83,IY,IS)=P_RFCRUDEINP(7,1:11,IY)
    z[75] = dfd["P_RFCRUDEINP"].loc[7].loc[3] * SCALPR2

    #     API 27-35 Sour
    # T132(73:83,IY,IS)=P_RFCRUDEINP(7,1:11,IY)
    z[76] = dfd["P_RFCRUDEINP"].loc[7].loc[4] * SCALPR2

    #     API less than 27 Sweet
    # T132(73:83,IY,IS)=P_RFCRUDEINP(7,1:11,IY)
    z[77] = dfd["P_RFCRUDEINP"].loc[7].loc[5] * SCALPR2

    #     API less than 27 Sour
    # T132(73:83,IY,IS)=P_RFCRUDEINP(7,1:11,IY)
    z[78] = dfd["P_RFCRUDEINP"].loc[7].loc[6] * SCALPR2

    #     California
    # T132(73:83,IY,IS)=P_RFCRUDEINP(7,1:11,IY)
    z[79] = dfd["P_RFCRUDEINP"].loc[7].loc[7] * SCALPR2

    #     Syncrude
    # T132(73:83,IY,IS)=P_RFCRUDEINP(7,1:11,IY)
    z[80] = dfd["P_RFCRUDEINP"].loc[7].loc[8] * SCALPR2

    #     Dilbit/Synbit
    # T132(73:83,IY,IS)=P_RFCRUDEINP(7,1:11,IY)
    z[81] = dfd["P_RFCRUDEINP"].loc[7].loc[9] * SCALPR2

    #     API 40-50 Sweet
    # T132(73:83,IY,IS)=P_RFCRUDEINP(7,1:11,IY)
    z[82] = dfd["P_RFCRUDEINP"].loc[7].loc[10] * SCALPR2

    #     API 50+ Sweet
    # T132(73:83,IY,IS)=P_RFCRUDEINP(7,1:11,IY)
    z[83] = dfd["P_RFCRUDEINP"].loc[7].loc[11] * SCALPR2

    #

    #   Region 8 - PADD V Other

    #     API 35-40 Sweet
    # T132(85:95,IY,IS)=P_RFCRUDEINP(8,1:11,IY)
    z[85] = dfd["P_RFCRUDEINP"].loc[8].loc[1] * SCALPR2

    #     API 35+ Sour
    # T132(85:95,IY,IS)=P_RFCRUDEINP(8,1:11,IY)
    z[86] = dfd["P_RFCRUDEINP"].loc[8].loc[2] * SCALPR2

    #     API 27-35 MediSour
    # T132(85:95,IY,IS)=P_RFCRUDEINP(8,1:11,IY)
    z[87] = dfd["P_RFCRUDEINP"].loc[8].loc[3] * SCALPR2

    #     API 27-35 Sour
    # T132(85:95,IY,IS)=P_RFCRUDEINP(8,1:11,IY)
    z[88] = dfd["P_RFCRUDEINP"].loc[8].loc[4] * SCALPR2

    #     API less than 27 Sweet
    # T132(85:95,IY,IS)=P_RFCRUDEINP(8,1:11,IY)
    z[89] = dfd["P_RFCRUDEINP"].loc[8].loc[5] * SCALPR2

    #     API less than 27 Sour
    # T132(85:95,IY,IS)=P_RFCRUDEINP(8,1:11,IY)
    z[90] = dfd["P_RFCRUDEINP"].loc[8].loc[6] * SCALPR2

    #     California
    # T132(85:95,IY,IS)=P_RFCRUDEINP(8,1:11,IY)
    z[91] = dfd["P_RFCRUDEINP"].loc[8].loc[7] * SCALPR2

    #     Syncrude
    # T132(85:95,IY,IS)=P_RFCRUDEINP(8,1:11,IY)
    z[92] = dfd["P_RFCRUDEINP"].loc[8].loc[8] * SCALPR2

    #     Dilbit/Synbit
    # T132(85:95,IY,IS)=P_RFCRUDEINP(8,1:11,IY)
    z[93] = dfd["P_RFCRUDEINP"].loc[8].loc[9] * SCALPR2

    #     API 40-50 Sweet
    # T132(85:95,IY,IS)=P_RFCRUDEINP(8,1:11,IY)
    z[94] = dfd["P_RFCRUDEINP"].loc[8].loc[10] * SCALPR2

    #     API 50+ Sweet
    # T132(85:95,IY,IS)=P_RFCRUDEINP(8,1:11,IY)
    z[95] = dfd["P_RFCRUDEINP"].loc[8].loc[11] * SCALPR2

    z[24] = (
        dfd["RFCRUDEINP"].loc[2].loc[1] * z[13]
        + dfd["RFCRUDEINP"].loc[2].loc[2] * z[14]
        + dfd["RFCRUDEINP"].loc[2].loc[3] * z[15]
        + dfd["RFCRUDEINP"].loc[2].loc[4] * z[16]
        + dfd["RFCRUDEINP"].loc[2].loc[5] * z[17]
        + dfd["RFCRUDEINP"].loc[2].loc[6] * z[18]
        + dfd["RFCRUDEINP"].loc[2].loc[7] * z[19]
        + dfd["RFCRUDEINP"].loc[2].loc[8] * z[20]
        + dfd["RFCRUDEINP"].loc[2].loc[9] * z[21]
        + dfd["RFCRUDEINP"].loc[2].loc[10] * z[22]
        + dfd["RFCRUDEINP"].loc[2].loc[11] * z[23]
    ) / dfd["RFCRUDEINP"].loc[2, 1:11, :].sum().replace(0, 1)

    z[36] = (
        dfd["RFCRUDEINP"].loc[3].loc[1] * z[25]
        + dfd["RFCRUDEINP"].loc[3].loc[2] * z[26]
        + dfd["RFCRUDEINP"].loc[3].loc[3] * z[27]
        + dfd["RFCRUDEINP"].loc[3].loc[4] * z[28]
        + dfd["RFCRUDEINP"].loc[3].loc[5] * z[29]
        + dfd["RFCRUDEINP"].loc[3].loc[6] * z[30]
        + dfd["RFCRUDEINP"].loc[3].loc[7] * z[31]
        + dfd["RFCRUDEINP"].loc[3].loc[8] * z[32]
        + dfd["RFCRUDEINP"].loc[3].loc[9] * z[33]
        + dfd["RFCRUDEINP"].loc[3].loc[10] * z[34]
        + dfd["RFCRUDEINP"].loc[3].loc[11] * z[35]
    ) / dfd["RFCRUDEINP"].loc[3, 1:11, :].sum().replace(0, 1)

    z[48] = (
        dfd["RFCRUDEINP"].loc[4].loc[1] * z[37]
        + dfd["RFCRUDEINP"].loc[4].loc[2] * z[38]
        + dfd["RFCRUDEINP"].loc[4].loc[3] * z[39]
        + dfd["RFCRUDEINP"].loc[4].loc[4] * z[40]
        + dfd["RFCRUDEINP"].loc[4].loc[5] * z[41]
        + dfd["RFCRUDEINP"].loc[4].loc[6] * z[42]
        + dfd["RFCRUDEINP"].loc[4].loc[7] * z[43]
        + dfd["RFCRUDEINP"].loc[4].loc[8] * z[44]
        + dfd["RFCRUDEINP"].loc[4].loc[9] * z[45]
        + dfd["RFCRUDEINP"].loc[4].loc[10] * z[46]
        + dfd["RFCRUDEINP"].loc[4].loc[11] * z[47]
    ) / dfd["RFCRUDEINP"].loc[4, 1:11, :].sum().replace(0, 1)

    z[60] = (
        dfd["RFCRUDEINP"].loc[5].loc[1] * z[49]
        + dfd["RFCRUDEINP"].loc[5].loc[2] * z[50]
        + dfd["RFCRUDEINP"].loc[5].loc[3] * z[51]
        + dfd["RFCRUDEINP"].loc[5].loc[4] * z[52]
        + dfd["RFCRUDEINP"].loc[5].loc[5] * z[53]
        + dfd["RFCRUDEINP"].loc[5].loc[6] * z[54]
        + dfd["RFCRUDEINP"].loc[5].loc[7] * z[55]
        + dfd["RFCRUDEINP"].loc[5].loc[8] * z[56]
        + dfd["RFCRUDEINP"].loc[5].loc[9] * z[57]
        + dfd["RFCRUDEINP"].loc[5].loc[10] * z[58]
        + dfd["RFCRUDEINP"].loc[5].loc[11] * z[59]
    ) / dfd["RFCRUDEINP"].loc[5, 1:11, :].sum().replace(0, 1)

    z[72] = (
        dfd["RFCRUDEINP"].loc[6].loc[1] * z[61]
        + dfd["RFCRUDEINP"].loc[6].loc[2] * z[62]
        + dfd["RFCRUDEINP"].loc[6].loc[3] * z[63]
        + dfd["RFCRUDEINP"].loc[6].loc[4] * z[64]
        + dfd["RFCRUDEINP"].loc[6].loc[5] * z[65]
        + dfd["RFCRUDEINP"].loc[6].loc[6] * z[66]
        + dfd["RFCRUDEINP"].loc[6].loc[7] * z[67]
        + dfd["RFCRUDEINP"].loc[6].loc[8] * z[68]
        + dfd["RFCRUDEINP"].loc[6].loc[9] * z[69]
        + dfd["RFCRUDEINP"].loc[6].loc[10] * z[70]
        + dfd["RFCRUDEINP"].loc[6].loc[11] * z[71]
    ) / dfd["RFCRUDEINP"].loc[6, 1:11, :].sum().replace(0, 1)

    z[84] = (
        dfd["RFCRUDEINP"].loc[7].loc[1] * z[73]
        + dfd["RFCRUDEINP"].loc[7].loc[2] * z[74]
        + dfd["RFCRUDEINP"].loc[7].loc[3] * z[75]
        + dfd["RFCRUDEINP"].loc[7].loc[4] * z[76]
        + dfd["RFCRUDEINP"].loc[7].loc[5] * z[77]
        + dfd["RFCRUDEINP"].loc[7].loc[6] * z[78]
        + dfd["RFCRUDEINP"].loc[7].loc[7] * z[79]
        + dfd["RFCRUDEINP"].loc[7].loc[8] * z[80]
        + dfd["RFCRUDEINP"].loc[7].loc[9] * z[81]
        + dfd["RFCRUDEINP"].loc[7].loc[10] * z[82]
        + dfd["RFCRUDEINP"].loc[7].loc[11] * z[83]
    ) / dfd["RFCRUDEINP"].loc[7, 1:11, :].sum().replace(0, 1)

    #   United States

    #     API 35-40 Sweet

    # T132(96,IY,IS)=(T106(85,IY,IS)*T132(85,IY,IS)+T106(86,IY,IS)*T132(86,IY,IS)+T106(87,IY,IS)*T132(87,IY,IS)+T106(88,IY,IS)*T132(88,IY,IS)+T106(89,IY,IS)*T132(89,IY,IS)+T106(90,IY,IS)*T132(90,IY,IS)+T106(91,IY,IS)*T132(91,IY,IS)+T106(92,IY,IS)*T132(92,IY,IS)+T106(93,IY,IS)*T132(93,IY,IS)+T106(94,IY,IS)*T132(94,IY,IS)+T106(95,IY,IS)*T132(95,IY,IS))/SUM(T106(85:95,IY,IS))
    z[96] = (
        dfd["RFCRUDEINP"].loc[8].loc[1] * z[85]
        + dfd["RFCRUDEINP"].loc[8].loc[2] * z[86]
        + dfd["RFCRUDEINP"].loc[8].loc[3] * z[87]
        + dfd["RFCRUDEINP"].loc[8].loc[4] * z[88]
        + dfd["RFCRUDEINP"].loc[8].loc[5] * z[89]
        + dfd["RFCRUDEINP"].loc[8].loc[6] * z[90]
        + dfd["RFCRUDEINP"].loc[8].loc[7] * z[91]
        + dfd["RFCRUDEINP"].loc[8].loc[8] * z[92]
        + dfd["RFCRUDEINP"].loc[8].loc[9] * z[93]
        + dfd["RFCRUDEINP"].loc[8].loc[10] * z[94]
        + dfd["RFCRUDEINP"].loc[8].loc[11] * z[95]
    ) / dfd["RFCRUDEINP"].loc[8, 1:11, :].sum().replace(0, 1)

    #  average for United States:

    for CrTy in range(1, 12):  #  crude types
        # IF (sum(RFCRUDEINP(1:8,CrTy,IY)) .NE. 0.0) &
        z[96 + CrTy] = (
            (
                dfd["P_RFCRUDEINP"].loc[1, CrTy] * dfd["RFCRUDEINP"].loc[1, CrTy]
                + dfd["P_RFCRUDEINP"].loc[2, CrTy] * dfd["RFCRUDEINP"].loc[2, CrTy]
                + dfd["P_RFCRUDEINP"].loc[3, CrTy] * dfd["RFCRUDEINP"].loc[3, CrTy]
                + dfd["P_RFCRUDEINP"].loc[4, CrTy] * dfd["RFCRUDEINP"].loc[4, CrTy]
                + dfd["P_RFCRUDEINP"].loc[5, CrTy] * dfd["RFCRUDEINP"].loc[5, CrTy]
                + dfd["P_RFCRUDEINP"].loc[6, CrTy] * dfd["RFCRUDEINP"].loc[6, CrTy]
                + dfd["P_RFCRUDEINP"].loc[7, CrTy] * dfd["RFCRUDEINP"].loc[7, CrTy]
                + dfd["P_RFCRUDEINP"].loc[8, CrTy] * dfd["RFCRUDEINP"].loc[8, CrTy]
            )
            / dfd["RFCRUDEINP"].loc[1:8, CrTy, :].sum().replace(0, 1)
            * SCALPR2
        ).fillna(0)

    # !  total United States:
    # DO PADD=1,11             ! not PADDS but crude types, but PADD exists
    # T106(96+PADD,IY,IS) = sum(RFCRUDEINP(1:8,PADD,IY))
    # ENDDO
    # T106(108,IY,IS) = FSUM(T106(97,IY,IS),11)
    # T132(108,IY,IS)=(T106(97,IY,IS)*T132(97,IY,IS)+T106(98,IY,IS)*T132(98,IY,IS)+T106(99,IY,IS)*T132(99,IY,IS)+T106(100,IY,IS)*T132(100,IY,IS)+T106(101,IY,IS)*T132(101,IY,IS)+T106(102,IY,IS)*T132(102,IY,IS)+T106(103,IY,IS)*T132(103,IY,IS)+T106(104,IY,IS)*T132(104,IY,IS)+T106(105,IY,IS)*T132(105,IY,IS)+T106(106,IY,IS)*T132(106,IY,IS)+T106(107,IY,IS)*T132(107,IY,IS))/SUM(T106(97:107,IY,IS))
    z[108] = (
        dfd["RFCRUDEINP"].loc[1:8, 1, :].sum() * z[97]
        + dfd["RFCRUDEINP"].loc[1:8, 2, :].sum() * z[98]
        + dfd["RFCRUDEINP"].loc[1:8, 3, :].sum() * z[99]
        + dfd["RFCRUDEINP"].loc[1:8, 4, :].sum() * z[100]
        + dfd["RFCRUDEINP"].loc[1:8, 5, :].sum() * z[101]
        + dfd["RFCRUDEINP"].loc[1:8, 6, :].sum() * z[102]
        + dfd["RFCRUDEINP"].loc[1:8, 7, :].sum() * z[103]
        + dfd["RFCRUDEINP"].loc[1:8, 8, :].sum() * z[104]
        + dfd["RFCRUDEINP"].loc[1:8, 9, :].sum() * z[105]
        + dfd["RFCRUDEINP"].loc[1:8, 10, :].sum() * z[106]
        + dfd["RFCRUDEINP"].loc[1:8, 11, :].sum() * z[107]
    ) / (
        dfd["RFCRUDEINP"].loc[1, 1:11, :].sum()
        + dfd["RFCRUDEINP"].loc[2, 1:11, :].sum()
        + dfd["RFCRUDEINP"].loc[3, 1:11, :].sum()
        + dfd["RFCRUDEINP"].loc[4, 1:11, :].sum()
        + dfd["RFCRUDEINP"].loc[5, 1:11, :].sum()
        + dfd["RFCRUDEINP"].loc[6, 1:11, :].sum()
        + dfd["RFCRUDEINP"].loc[7, 1:11, :].sum()
        + dfd["RFCRUDEINP"].loc[8, 1:11, :].sum()
    ).replace(
        0, 1
    )
    #   Region 9 - Maritime Canada and Caribbean
    # T132(120,IY,IS)=(T106(109,IY,IS)*T132(109,IY,IS)+T106(110,IY,IS)*T132(110,IY,IS)+T106(111,IY,IS)*T132(111,IY,IS)+T106(112,IY,IS)*T132(112,IY,IS)+T106(113,IY,IS)*T132(113,IY,IS)+T106(114,IY,IS)*T132(114,IY,IS)+T106(115,IY,IS)*T132(115,IY,IS)+T106(116,IY,IS)*T132(116,IY,IS)+T106(117,IY,IS)*T132(117,IY,IS)+T106(118,IY,IS)*T132(118,IY,IS)+T106(119,IY,IS)*T132(119,IY,IS))/SUM(T106(109:119,IY,IS))
    # z[120] = (T106(z[109])*z[109])+T106(z[110])*z[110])+T106(z[111])*z[111])+T106(z[112])*z[112])+T106(z[113])*z[113])+T106(z[114])*z[114])+T106(z[115])*z[115])+T106(z[116])*z[116])+T106(z[117])*z[117])+T106(z[118])*z[118])+T106(z[119])*z[119]))/SUM(T106(z[109:119]))

    #     API 35-40 Sweet
    # T132(109:119,IY,IS)=P_RFCRUDEINP(9,1:11,IY)
    z[109] = dfd["P_RFCRUDEINP"].loc[9].loc[1] * SCALPR2

    #     API 35+ Sour
    # T132(109:119,IY,IS)=P_RFCRUDEINP(9,1:11,IY)
    z[110] = dfd["P_RFCRUDEINP"].loc[9].loc[2] * SCALPR2

    #     API 27-35 MediSour
    # T132(109:119,IY,IS)=P_RFCRUDEINP(9,1:11,IY)
    z[111] = dfd["P_RFCRUDEINP"].loc[9].loc[3] * SCALPR2

    #     API 27-35 Sour
    # T132(109:119,IY,IS)=P_RFCRUDEINP(9,1:11,IY)
    z[112] = dfd["P_RFCRUDEINP"].loc[9].loc[4] * SCALPR2

    #     API less than 27 Sweet
    # T132(109:119,IY,IS)=P_RFCRUDEINP(9,1:11,IY)
    z[113] = dfd["P_RFCRUDEINP"].loc[9].loc[5] * SCALPR2

    #     API less than 27 Sour
    # T132(109:119,IY,IS)=P_RFCRUDEINP(9,1:11,IY)
    z[114] = dfd["P_RFCRUDEINP"].loc[9].loc[6] * SCALPR2

    #     California
    # T132(109:119,IY,IS)=P_RFCRUDEINP(9,1:11,IY)
    z[115] = dfd["P_RFCRUDEINP"].loc[9].loc[7] * SCALPR2

    #     Syncrude
    # T132(109:119,IY,IS)=P_RFCRUDEINP(9,1:11,IY)
    z[116] = dfd["P_RFCRUDEINP"].loc[9].loc[8] * SCALPR2

    #     Dilbit/Synbit
    # T132(109:119,IY,IS)=P_RFCRUDEINP(9,1:11,IY)
    z[117] = dfd["P_RFCRUDEINP"].loc[9].loc[9] * SCALPR2

    #     API 40-50 Sweet
    # T132(109:119,IY,IS)=P_RFCRUDEINP(9,1:11,IY)
    z[118] = dfd["P_RFCRUDEINP"].loc[9].loc[10] * SCALPR2

    #     API 50+ Sweet
    # T132(109:119,IY,IS)=P_RFCRUDEINP(9,1:11,IY)
    z[119] = dfd["P_RFCRUDEINP"].loc[9].loc[11] * SCALPR2
    z[120] = (
        dfd["RFCRUDEINP"].loc[9].loc[1] * z[109]
        + dfd["RFCRUDEINP"].loc[9].loc[2] * z[110]
        + dfd["RFCRUDEINP"].loc[9].loc[3] * z[111]
        + dfd["RFCRUDEINP"].loc[9].loc[4] * z[112]
        + dfd["RFCRUDEINP"].loc[9].loc[5] * z[113]
        + dfd["RFCRUDEINP"].loc[9].loc[6] * z[114]
        + dfd["RFCRUDEINP"].loc[9].loc[7] * z[115]
        + dfd["RFCRUDEINP"].loc[9].loc[8] * z[116]
        + dfd["RFCRUDEINP"].loc[9].loc[9] * z[117]
        + dfd["RFCRUDEINP"].loc[9].loc[10] * z[118]
        + dfd["RFCRUDEINP"].loc[9].loc[11] * z[119]
    ) / dfd["RFCRUDEINP"].loc[9, 1:11, :].sum().replace(0, 1)

    return z
