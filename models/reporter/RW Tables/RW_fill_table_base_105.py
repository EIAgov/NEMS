# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
SZO fixed "Other Crude Oil Not Listed Above"
Fixed not printing on 8/9/2024
"""


def fill_table_base_105(dfd, table_spec, table_id):
    """Fill table Domestic Crude Oil Production and Prices by Refinery Region

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

    RDAYS = dfd["RDAYS_rwpre"]
    MNCRUD = dfd["MNCRUD_rwpre"]

    #   Domestic Crude Oil Production and Prices by Refinery Region
    #    Production and Prices
    #
    #   Domestic Crude Oil Production
    #   (thousand barrels per day)
    #
    #   Region 1 - PADD I

    #     API 35-40 Sweet
    # T105(001:006,IY,IS)=OGCRUDEREF(1,1:6,IY)/RDAYS*1000.
    z[1] = dfd["OGCRUDEREF"].loc[1].loc[1] / RDAYS * 1000.0

    #     API 35+ Sour
    # T105(001:006,IY,IS)=OGCRUDEREF(1,1:6,IY)/RDAYS*1000.
    z[2] = dfd["OGCRUDEREF"].loc[1].loc[2] / RDAYS * 1000.0

    #     API 27-35 MediSour
    # T105(001:006,IY,IS)=OGCRUDEREF(1,1:6,IY)/RDAYS*1000.
    z[3] = dfd["OGCRUDEREF"].loc[1].loc[3] / RDAYS * 1000.0

    #     API 27-35 Sour
    # T105(001:006,IY,IS)=OGCRUDEREF(1,1:6,IY)/RDAYS*1000.
    z[4] = dfd["OGCRUDEREF"].loc[1].loc[4] / RDAYS * 1000.0

    #     API less than 27 Sweet
    # T105(001:006,IY,IS)=OGCRUDEREF(1,1:6,IY)/RDAYS*1000.
    z[5] = dfd["OGCRUDEREF"].loc[1].loc[5] / RDAYS * 1000.0

    #     API less than 27 Sour
    # T105(001:006,IY,IS)=OGCRUDEREF(1,1:6,IY)/RDAYS*1000.
    z[6] = dfd["OGCRUDEREF"].loc[1].loc[6] / RDAYS * 1000.0

    #     API 40-50 Sweet
    # T105(121:122,IY,IS)=OGCRUDEREF(1,10:11,IY)/RDAYS*1000.
    z[121] = dfd["OGCRUDEREF"].loc[1].loc[10] / RDAYS * 1000.0

    #     API 50+ Sweet
    # T105(121:122,IY,IS)=OGCRUDEREF(1,10:11,IY)/RDAYS*1000.
    z[122] = dfd["OGCRUDEREF"].loc[1].loc[11] / RDAYS * 1000.0
    # T105(007,IY,IS)=FSUM(T105(001,IY,IS),6)+FSUM(T105(121,IY,IS),2)
    z[7] = z[1] + z[2] + z[3] + z[4] + z[5] + z[6] + z[121] + z[122]
    #

    #   Region 2 - PADD II Inland

    #     API 35-40 Sweet
    # T105(008:013,IY,IS)=OGCRUDEREF(2,1:6,IY)/RDAYS*1000.
    z[8] = dfd["OGCRUDEREF"].loc[2].loc[1] / RDAYS * 1000.0

    #     API 35+ Sour
    # T105(008:013,IY,IS)=OGCRUDEREF(2,1:6,IY)/RDAYS*1000.
    z[9] = dfd["OGCRUDEREF"].loc[2].loc[2] / RDAYS * 1000.0

    #     API 27-35 MediSour
    # T105(008:013,IY,IS)=OGCRUDEREF(2,1:6,IY)/RDAYS*1000.
    z[10] = dfd["OGCRUDEREF"].loc[2].loc[3] / RDAYS * 1000.0

    #     API 27-35 Sour
    # T105(008:013,IY,IS)=OGCRUDEREF(2,1:6,IY)/RDAYS*1000.
    z[11] = dfd["OGCRUDEREF"].loc[2].loc[4] / RDAYS * 1000.0

    #     API less than 27 Sweet
    # T105(008:013,IY,IS)=OGCRUDEREF(2,1:6,IY)/RDAYS*1000.
    z[12] = dfd["OGCRUDEREF"].loc[2].loc[5] / RDAYS * 1000.0

    #     API less than 27 Sour
    # T105(008:013,IY,IS)=OGCRUDEREF(2,1:6,IY)/RDAYS*1000.
    z[13] = dfd["OGCRUDEREF"].loc[2].loc[6] / RDAYS * 1000.0

    #     API 40-50 Sweet
    # T105(123:124,IY,IS)=OGCRUDEREF(2,10:11,IY)/RDAYS*1000.
    z[123] = dfd["OGCRUDEREF"].loc[2].loc[10] / RDAYS * 1000.0

    #     API 50+ Sweet
    # T105(123:124,IY,IS)=OGCRUDEREF(2,10:11,IY)/RDAYS*1000.
    z[124] = dfd["OGCRUDEREF"].loc[2].loc[11] / RDAYS * 1000.0

    # T105(014,IY,IS)=FSUM(T105(008,IY,IS),6)+FSUM(T105(123,IY,IS),2)
    z[14] = z[8] + z[9] + z[10] + z[11] + z[12] + z[13] + z[123] + z[124]
    #   Region 3 - PADD II Lakes

    #     API 35-40 Sweet
    # T105(015:020,IY,IS)=OGCRUDEREF(3,1:6,IY)/RDAYS*1000.
    z[15] = dfd["OGCRUDEREF"].loc[3].loc[1] / RDAYS * 1000.0

    #     API 35+ Sour
    # T105(015:020,IY,IS)=OGCRUDEREF(3,1:6,IY)/RDAYS*1000.
    z[16] = dfd["OGCRUDEREF"].loc[3].loc[2] / RDAYS * 1000.0

    #     API 27-35 MediSour
    # T105(015:020,IY,IS)=OGCRUDEREF(3,1:6,IY)/RDAYS*1000.
    z[17] = dfd["OGCRUDEREF"].loc[3].loc[3] / RDAYS * 1000.0

    #     API 27-35 Sour
    # T105(015:020,IY,IS)=OGCRUDEREF(3,1:6,IY)/RDAYS*1000.
    z[18] = dfd["OGCRUDEREF"].loc[3].loc[4] / RDAYS * 1000.0

    #     API less than 27 Sweet
    # T105(015:020,IY,IS)=OGCRUDEREF(3,1:6,IY)/RDAYS*1000.
    z[19] = dfd["OGCRUDEREF"].loc[3].loc[5] / RDAYS * 1000.0

    #     API less than 27 Sour
    # T105(015:020,IY,IS)=OGCRUDEREF(3,1:6,IY)/RDAYS*1000.
    z[20] = dfd["OGCRUDEREF"].loc[3].loc[6] / RDAYS * 1000.0

    #     API 40-50 Sweet
    # T105(125:126,IY,IS)=OGCRUDEREF(3,10:11,IY)/RDAYS*1000.
    z[125] = dfd["OGCRUDEREF"].loc[3].loc[10] / RDAYS * 1000.0

    #     API 50+ Sweet
    # T105(125:126,IY,IS)=OGCRUDEREF(3,10:11,IY)/RDAYS*1000.
    z[126] = dfd["OGCRUDEREF"].loc[3].loc[11] / RDAYS * 1000.0

    # T105(021,IY,IS)=FSUM(T105(015,IY,IS),6)+FSUM(T105(125,IY,IS),2)
    z[21] = z[15] + z[16] + z[17] + z[18] + z[19] + z[20] + z[125] + z[126]

    #   Region 4 - PADD III Gulf

    #     API 35-40 Sweet
    # T105(022:027,IY,IS)=OGCRUDEREF(4,1:6,IY)/RDAYS*1000.
    z[22] = dfd["OGCRUDEREF"].loc[4].loc[1] / RDAYS * 1000.0

    #     API 35+ Sour
    # T105(022:027,IY,IS)=OGCRUDEREF(4,1:6,IY)/RDAYS*1000.
    z[23] = dfd["OGCRUDEREF"].loc[4].loc[2] / RDAYS * 1000.0

    #     API 27-35 MediSour
    # T105(022:027,IY,IS)=OGCRUDEREF(4,1:6,IY)/RDAYS*1000.
    z[24] = dfd["OGCRUDEREF"].loc[4].loc[3] / RDAYS * 1000.0

    #     API 27-35 Sour
    # T105(022:027,IY,IS)=OGCRUDEREF(4,1:6,IY)/RDAYS*1000.
    z[25] = dfd["OGCRUDEREF"].loc[4].loc[4] / RDAYS * 1000.0

    #     API less than 27 Sweet
    # T105(022:027,IY,IS)=OGCRUDEREF(4,1:6,IY)/RDAYS*1000.
    z[26] = dfd["OGCRUDEREF"].loc[4].loc[5] / RDAYS * 1000.0

    #     API less than 27 Sour
    # T105(022:027,IY,IS)=OGCRUDEREF(4,1:6,IY)/RDAYS*1000.
    z[27] = dfd["OGCRUDEREF"].loc[4].loc[6] / RDAYS * 1000.0

    #     API 40-50 Sweet
    # T105(127:128,IY,IS)=OGCRUDEREF(4,10:11,IY)/RDAYS*1000.
    z[127] = dfd["OGCRUDEREF"].loc[4].loc[10] / RDAYS * 1000.0

    #     API 50+ Sweet
    # T105(127:128,IY,IS)=OGCRUDEREF(4,10:11,IY)/RDAYS*1000.
    z[128] = dfd["OGCRUDEREF"].loc[4].loc[11] / RDAYS * 1000.0
    # T105(028,IY,IS)=FSUM(T105(022,IY,IS),6)+FSUM(T105(127,IY,IS),2)
    z[28] = z[22] + z[23] + z[24] + z[25] + z[26] + z[27] + z[127] + z[128]

    #

    #   Region 5 - PADD III Inland

    #     API 35-40 Sweet
    # T105(029:034,IY,IS)=OGCRUDEREF(5,1:6,IY)/RDAYS*1000.
    z[29] = dfd["OGCRUDEREF"].loc[5].loc[1] / RDAYS * 1000.0

    #     API 35+ Sour
    # T105(029:034,IY,IS)=OGCRUDEREF(5,1:6,IY)/RDAYS*1000.
    z[30] = dfd["OGCRUDEREF"].loc[5].loc[2] / RDAYS * 1000.0

    #     API 27-35 MediSour
    # T105(029:034,IY,IS)=OGCRUDEREF(5,1:6,IY)/RDAYS*1000.
    z[31] = dfd["OGCRUDEREF"].loc[5].loc[3] / RDAYS * 1000.0

    #     API 27-35 Sour
    # T105(029:034,IY,IS)=OGCRUDEREF(5,1:6,IY)/RDAYS*1000.
    z[32] = dfd["OGCRUDEREF"].loc[5].loc[4] / RDAYS * 1000.0

    #     API less than 27 Sweet
    # T105(029:034,IY,IS)=OGCRUDEREF(5,1:6,IY)/RDAYS*1000.
    z[33] = dfd["OGCRUDEREF"].loc[5].loc[5] / RDAYS * 1000.0

    #     API less than 27 Sour
    # T105(029:034,IY,IS)=OGCRUDEREF(5,1:6,IY)/RDAYS*1000.
    z[34] = dfd["OGCRUDEREF"].loc[5].loc[6] / RDAYS * 1000.0

    #     API 40-50 Sweet
    # T105(129:130,IY,IS)=OGCRUDEREF(5,10:11,IY)/RDAYS*1000.
    z[129] = dfd["OGCRUDEREF"].loc[5].loc[10] / RDAYS * 1000.0

    #     API 50+ Sweet
    # T105(129:130,IY,IS)=OGCRUDEREF(5,10:11,IY)/RDAYS*1000.
    z[130] = dfd["OGCRUDEREF"].loc[5].loc[11] / RDAYS * 1000.0

    # T105(035,IY,IS)=FSUM(T105(029,IY,IS),6)+FSUM(T105(129,IY,IS),2)
    z[35] = (
        z[29].fillna(0)
        + z[30].fillna(0)
        + z[31].fillna(0)
        + z[32].fillna(0)
        + z[33].fillna(0)
        + z[34].fillna(0)
        + z[129].fillna(0)
        + z[130].fillna(0)
    )

    #   Region 6 - PADD IV

    #     API 35-40 Sweet
    # T105(036:041,IY,IS)=OGCRUDEREF(6,1:6,IY)/RDAYS*1000.
    z[36] = dfd["OGCRUDEREF"].loc[6].loc[1] / RDAYS * 1000.0

    #     API 35+ Sour
    # T105(036:041,IY,IS)=OGCRUDEREF(6,1:6,IY)/RDAYS*1000.
    z[37] = dfd["OGCRUDEREF"].loc[6].loc[2] / RDAYS * 1000.0

    #     API 27-35 MediSour
    # T105(036:041,IY,IS)=OGCRUDEREF(6,1:6,IY)/RDAYS*1000.
    z[38] = dfd["OGCRUDEREF"].loc[6].loc[3] / RDAYS * 1000.0

    #     API 27-35 Sour
    # T105(036:041,IY,IS)=OGCRUDEREF(6,1:6,IY)/RDAYS*1000.
    z[39] = dfd["OGCRUDEREF"].loc[6].loc[4] / RDAYS * 1000.0

    #     API less than 27 Sweet
    # T105(036:041,IY,IS)=OGCRUDEREF(6,1:6,IY)/RDAYS*1000.
    z[40] = dfd["OGCRUDEREF"].loc[6].loc[5] / RDAYS * 1000.0

    #     API less than 27 Sour
    # T105(036:041,IY,IS)=OGCRUDEREF(6,1:6,IY)/RDAYS*1000.
    z[41] = dfd["OGCRUDEREF"].loc[6].loc[6] / RDAYS * 1000.0

    #     API 40-50 Sweet
    # T105(131:132,IY,IS)=OGCRUDEREF(6,10:11,IY)/RDAYS*1000.
    z[131] = dfd["OGCRUDEREF"].loc[6].loc[10] / RDAYS * 1000.0

    #     API 50+ Sweet
    # T105(131:132,IY,IS)=OGCRUDEREF(6,10:11,IY)/RDAYS*1000.
    z[132] = dfd["OGCRUDEREF"].loc[6].loc[11] / RDAYS * 1000.0

    # T105(042,IY,IS)=FSUM(T105(036,IY,IS),6)+FSUM(T105(131,IY,IS),2)
    z[42] = (
        z[36].fillna(0)
        + z[37].fillna(0)
        + z[38].fillna(0)
        + z[39].fillna(0)
        + z[40].fillna(0)
        + z[41].fillna(0)
        + z[131].fillna(0)
        + z[132].fillna(0)
    )

    #   Region 7 - PADD V California

    #     California
    # T105(043:043,IY,IS)=OGCRUDEREF(7,7:7,IY)/RDAYS*1000.
    z[43] = dfd["OGCRUDEREF"].loc[7, 7] / RDAYS * 1000.0

    # T105(044,IY,IS)=FSUM(T105(043,IY,IS),1)
    z[44] = z[43]

    #   Region 8 - PADD V Other

    #     API 35-40 Sweet
    # T105(045:050,IY,IS)=OGCRUDEREF(8,1:6,IY)/RDAYS*1000.
    z[45] = dfd["OGCRUDEREF"].loc[8].loc[1] / RDAYS * 1000.0

    #     API 35+ Sour
    # T105(045:050,IY,IS)=OGCRUDEREF(8,1:6,IY)/RDAYS*1000.
    z[46] = dfd["OGCRUDEREF"].loc[8].loc[2] / RDAYS * 1000.0

    #     API 27-35 MediSour
    # T105(045:050,IY,IS)=OGCRUDEREF(8,1:6,IY)/RDAYS*1000.
    z[47] = dfd["OGCRUDEREF"].loc[8].loc[3] / RDAYS * 1000.0

    #     API 27-35 Sour
    # T105(045:050,IY,IS)=OGCRUDEREF(8,1:6,IY)/RDAYS*1000.
    z[48] = dfd["OGCRUDEREF"].loc[8].loc[4] / RDAYS * 1000.0

    #     API less than 27 Sweet
    # T105(045:050,IY,IS)=OGCRUDEREF(8,1:6,IY)/RDAYS*1000.
    z[49] = dfd["OGCRUDEREF"].loc[8].loc[5] / RDAYS * 1000.0

    #     API less than 27 Sour
    # T105(045:050,IY,IS)=OGCRUDEREF(8,1:6,IY)/RDAYS*1000.
    z[50] = dfd["OGCRUDEREF"].loc[8].loc[6] / RDAYS * 1000.0

    #     API 40-50 Sweet
    # T105(133:134,IY,IS)=OGCRUDEREF(8,10:11,IY)/RDAYS*1000.
    z[133] = dfd["OGCRUDEREF"].loc[8].loc[10] / RDAYS * 1000.0

    #     API 50+ Sweet
    # T105(133:134,IY,IS)=OGCRUDEREF(8,10:11,IY)/RDAYS*1000.
    z[134] = dfd["OGCRUDEREF"].loc[8].loc[11] / RDAYS * 1000.0

    # T105(051,IY,IS)=FSUM(T105(045,IY,IS),6)+FSUM(T105(133,IY,IS),2)
    z[51] = z[45] + z[46] + z[47] + z[48] + z[49] + z[50] + z[133] + z[134]

    #   United States

    #     API 35-40 Sweet
    # T105(052,IY,IS)=(SUM(OGCRUDEREF(1:6,1,IY))+OGCRUDEREF(8,1,IY))/RDAYS*1000.
    z[52] = (
        (
            dfd["OGCRUDEREF"].loc[((1, 2, 3, 4, 5, 6), 1), :].sum()
            + dfd["OGCRUDEREF"].loc[8].loc[1]
        )
        / RDAYS
        * 1000.0
    )

    #     API 35+ Sour
    # T105(053,IY,IS)=(SUM(OGCRUDEREF(1:6,2,IY))+OGCRUDEREF(8,2,IY))/RDAYS*1000.
    z[53] = (
        (
            dfd["OGCRUDEREF"].loc[((1, 2, 3, 4, 5, 6), 2), :].sum()
            + dfd["OGCRUDEREF"].loc[8].loc[2]
        )
        / RDAYS
        * 1000.0
    )

    #     API 27-35 MediSour
    # T105(054,IY,IS)=(SUM(OGCRUDEREF(1:6,3,IY))+OGCRUDEREF(8,3,IY))/RDAYS*1000.
    z[54] = (
        (
            dfd["OGCRUDEREF"].loc[((1, 2, 3, 4, 5, 6), 3), :].sum()
            + dfd["OGCRUDEREF"].loc[8].loc[3]
        )
        / RDAYS
        * 1000.0
    )

    #     API 27-35 Sour
    # T105(055,IY,IS)=(SUM(OGCRUDEREF(1:6,4,IY))+OGCRUDEREF(8,4,IY))/RDAYS*1000.
    z[55] = (
        (
            dfd["OGCRUDEREF"].loc[((1, 2, 3, 4, 5, 6), 4), :].sum()
            + dfd["OGCRUDEREF"].loc[8].loc[4]
        )
        / RDAYS
        * 1000.0
    )

    #     API less than 27 Sweet
    # T105(056,IY,IS)=(SUM(OGCRUDEREF(1:6,5,IY))+OGCRUDEREF(8,5,IY))/RDAYS*1000.
    z[56] = (
        (
            dfd["OGCRUDEREF"].loc[((1, 2, 3, 4, 5, 6), 5), :].sum()
            + dfd["OGCRUDEREF"].loc[8].loc[5]
        )
        / RDAYS
        * 1000.0
    )

    #     API less than 27 Sour
    # T105(057,IY,IS)=(SUM(OGCRUDEREF(1:6,6,IY))+OGCRUDEREF(8,6,IY))/RDAYS*1000.
    z[57] = (
        (
            dfd["OGCRUDEREF"].loc[((1, 2, 3, 4, 5, 6), 6), :].sum()
            + dfd["OGCRUDEREF"].loc[8].loc[6]
        )
        / RDAYS
        * 1000.0
    )

    #     California
    # T105(058,IY,IS)=OGCRUDEREF(7,7,IY)/RDAYS*1000.
    z[58] = dfd["OGCRUDEREF"].loc[7, 7] / RDAYS * 1000.0

    #     API 40-50 Sweet
    # T105(135,IY,IS)=(SUM(OGCRUDEREF(1:6,10,IY))+OGCRUDEREF(8,10,IY))/RDAYS*1000.
    z[135] = (
        (
            dfd["OGCRUDEREF"].loc[((1, 2, 3, 4, 5, 6), 10), :].sum()
            + dfd["OGCRUDEREF"].loc[8].loc[10]
        )
        / RDAYS
        * 1000.0
    )

    #     API 50+ Sweet
    # T105(136,IY,IS)=(SUM(OGCRUDEREF(1:6,11,IY))+OGCRUDEREF(8,11,IY))/RDAYS*1000.
    z[136] = (
        (
            dfd["OGCRUDEREF"].loc[((1, 2, 3, 4, 5, 6), 11), :].sum()
            + dfd["OGCRUDEREF"].loc[8].loc[11]
        )
        / RDAYS
        * 1000.0
    )

    # T105(059,IY,IS)=FSUM(T105(052,IY,IS),7)+FSUM(T105(135,IY,IS),2)
    z[59] = z[52] + z[53] + z[54] + z[55] + z[56] + z[57] + z[58] + z[135] + z[136]

    #   Other Crude Oil Not Listed Above
    # T105(060,IY,IS)=SUM(OGCRUDEREF(1:8,1:MNCRUD,IY))/RDAYS*1000.-T105(059,IY,IS)
    z[60] = (
        dfd["OGCRUDEREF"].loc[1:8, 1 : int(MNCRUD) + 1, :].sum() / 365 * 1000 - z[59]
    )

    #   Domestic Crude Oil Prices

    #   (#### dollars per barrel)

    #

    #   Region 1 - PADD I

    #     API 35-40 Sweet
    # T105(061:066,IY,IS)=RFCRUDEWHP(1,1:6,IY)
    z[61] = dfd["RFCRUDEWHP"].loc[1].loc[1] * SCALPR2

    #     API 35+ Sour
    # T105(061:066,IY,IS)=RFCRUDEWHP(1,1:6,IY)
    z[62] = dfd["RFCRUDEWHP"].loc[1].loc[2] * SCALPR2

    #     API 27-35 MediSour
    # T105(061:066,IY,IS)=RFCRUDEWHP(1,1:6,IY)
    z[63] = dfd["RFCRUDEWHP"].loc[1].loc[3] * SCALPR2

    #     API 27-35 Sour
    # T105(061:066,IY,IS)=RFCRUDEWHP(1,1:6,IY)
    z[64] = dfd["RFCRUDEWHP"].loc[1].loc[4] * SCALPR2

    #     API less than 27 Sweet
    # T105(061:066,IY,IS)=RFCRUDEWHP(1,1:6,IY)
    z[65] = dfd["RFCRUDEWHP"].loc[1].loc[5] * SCALPR2

    #     API less than 27 Sour
    # T105(061:066,IY,IS)=RFCRUDEWHP(1,1:6,IY)
    z[66] = dfd["RFCRUDEWHP"].loc[1].loc[6] * SCALPR2

    #     API 40-50 Sweet
    # T105(137:138,IY,IS)=RFCRUDEWHP(1,10:11,IY)
    z[137] = dfd["RFCRUDEWHP"].loc[1].loc[10] * SCALPR2

    #     API 50+ Sweet
    # T105(137:138,IY,IS)=RFCRUDEWHP(1,10:11,IY)
    z[138] = dfd["RFCRUDEWHP"].loc[1].loc[11] * SCALPR2

    z[67] = z[61] * 0.0

    #   Region 2 - PADD II Inland

    #     API 35-40 Sweet
    # T105(068:073,IY,IS)=RFCRUDEWHP(2,1:6,IY)
    z[68] = dfd["RFCRUDEWHP"].loc[2].loc[1] * SCALPR2

    #     API 35+ Sour
    # T105(068:073,IY,IS)=RFCRUDEWHP(2,1:6,IY)
    z[69] = dfd["RFCRUDEWHP"].loc[2].loc[2] * SCALPR2

    #     API 27-35 MediSour
    # T105(068:073,IY,IS)=RFCRUDEWHP(2,1:6,IY)
    z[70] = dfd["RFCRUDEWHP"].loc[2].loc[3] * SCALPR2

    #     API 27-35 Sour
    # T105(068:073,IY,IS)=RFCRUDEWHP(2,1:6,IY)
    z[71] = dfd["RFCRUDEWHP"].loc[2].loc[4] * SCALPR2

    #     API less than 27 Sweet
    # T105(068:073,IY,IS)=RFCRUDEWHP(2,1:6,IY)
    z[72] = dfd["RFCRUDEWHP"].loc[2].loc[5] * SCALPR2

    #     API less than 27 Sour
    # T105(068:073,IY,IS)=RFCRUDEWHP(2,1:6,IY)
    z[73] = dfd["RFCRUDEWHP"].loc[2].loc[6] * SCALPR2
    # T105(068:073,IY,IS)=RFCRUDEWHP(2,1:6,IY)
    # z[74] = dfd['RFCRUDEWHP'].loc[2,<check>]
    z[74] = z[68] * 0.0
    #     API 40-50 Sweet
    # T105(139:140,IY,IS)=RFCRUDEWHP(2,10:11,IY)
    z[139] = dfd["RFCRUDEWHP"].loc[2].loc[10] * SCALPR2

    #     API 50+ Sweet
    # T105(139:140,IY,IS)=RFCRUDEWHP(2,10:11,IY)
    z[140] = dfd["RFCRUDEWHP"].loc[2].loc[11] * SCALPR2

    #   Region 3 - PADD II Lakes
    # T105(075:080,IY,IS)=RFCRUDEWHP(3,1:6,IY)
    z[81] = z[140] * 0.0

    #     API 35-40 Sweet
    # T105(075:080,IY,IS)=RFCRUDEWHP(3,1:6,IY)
    z[75] = dfd["RFCRUDEWHP"].loc[3].loc[1] * SCALPR2

    #     API 35+ Sour
    # T105(075:080,IY,IS)=RFCRUDEWHP(3,1:6,IY)
    z[76] = dfd["RFCRUDEWHP"].loc[3].loc[2] * SCALPR2

    #     API 27-35 MediSour
    # T105(075:080,IY,IS)=RFCRUDEWHP(3,1:6,IY)
    z[77] = dfd["RFCRUDEWHP"].loc[3].loc[3] * SCALPR2

    #     API 27-35 Sour
    # T105(075:080,IY,IS)=RFCRUDEWHP(3,1:6,IY)
    z[78] = dfd["RFCRUDEWHP"].loc[3].loc[4] * SCALPR2

    #     API less than 27 Sweet
    # T105(075:080,IY,IS)=RFCRUDEWHP(3,1:6,IY)
    z[79] = dfd["RFCRUDEWHP"].loc[3].loc[5] * SCALPR2

    #     API less than 27 Sour
    # T105(075:080,IY,IS)=RFCRUDEWHP(3,1:6,IY)
    z[80] = dfd["RFCRUDEWHP"].loc[3].loc[6] * SCALPR2

    #     API 40-50 Sweet
    # T105(141:142,IY,IS)=RFCRUDEWHP(3,10:11,IY)
    z[141] = dfd["RFCRUDEWHP"].loc[3].loc[10] * SCALPR2

    #     API 50+ Sweet
    # T105(141:142,IY,IS)=RFCRUDEWHP(3,10:11,IY)
    z[142] = dfd["RFCRUDEWHP"].loc[3].loc[11] * SCALPR2

    #

    #   Region 4 - PADD III Gulf

    #     API 35-40 Sweet
    # T105(082:087,IY,IS)=RFCRUDEWHP(4,1:6,IY)
    z[82] = dfd["RFCRUDEWHP"].loc[4].loc[1] * SCALPR2

    #     API 35+ Sour
    # T105(082:087,IY,IS)=RFCRUDEWHP(4,1:6,IY)
    z[83] = dfd["RFCRUDEWHP"].loc[4].loc[2] * SCALPR2

    #     API 27-35 MediSour
    # T105(082:087,IY,IS)=RFCRUDEWHP(4,1:6,IY)
    z[84] = dfd["RFCRUDEWHP"].loc[4].loc[3] * SCALPR2

    #     API 27-35 Sour
    # T105(082:087,IY,IS)=RFCRUDEWHP(4,1:6,IY)
    z[85] = dfd["RFCRUDEWHP"].loc[4].loc[4] * SCALPR2

    #     API less than 27 Sweet
    # T105(082:087,IY,IS)=RFCRUDEWHP(4,1:6,IY)
    z[86] = dfd["RFCRUDEWHP"].loc[4].loc[5] * SCALPR2

    #     API less than 27 Sour
    # T105(082:087,IY,IS)=RFCRUDEWHP(4,1:6,IY)
    z[87] = dfd["RFCRUDEWHP"].loc[4].loc[6] * SCALPR2
    # T105(082:087,IY,IS)=RFCRUDEWHP(4,1:6,IY)
    z[88] = z[82] * 0.0
    #     API 40-50 Sweet
    # T105(143:144,IY,IS)=RFCRUDEWHP(4,10:11,IY)
    z[143] = dfd["RFCRUDEWHP"].loc[4].loc[10] * SCALPR2

    #     API 50+ Sweet
    # T105(143:144,IY,IS)=RFCRUDEWHP(4,10:11,IY)
    z[144] = dfd["RFCRUDEWHP"].loc[4].loc[11] * SCALPR2

    #

    #

    #   Region 5 - PADD III Inland
    # T105(089:094,IY,IS)=RFCRUDEWHP(5,1:6,IY)
    # z[95] = dfd['RFCRUDEWHP'].loc[5,<check>]

    #     API 35-40 Sweet
    # T105(089:094,IY,IS)=RFCRUDEWHP(5,1:6,IY)
    z[89] = dfd["RFCRUDEWHP"].loc[5].loc[1] * SCALPR2

    #     API 35+ Sour
    # T105(089:094,IY,IS)=RFCRUDEWHP(5,1:6,IY)
    z[90] = dfd["RFCRUDEWHP"].loc[5].loc[2] * SCALPR2

    #     API 27-35 MediSour
    # T105(089:094,IY,IS)=RFCRUDEWHP(5,1:6,IY)
    z[91] = dfd["RFCRUDEWHP"].loc[5].loc[3] * SCALPR2

    #     API 27-35 Sour
    # T105(089:094,IY,IS)=RFCRUDEWHP(5,1:6,IY)
    z[92] = dfd["RFCRUDEWHP"].loc[5].loc[4] * SCALPR2

    #     API less than 27 Sweet
    # T105(089:094,IY,IS)=RFCRUDEWHP(5,1:6,IY)
    z[93] = dfd["RFCRUDEWHP"].loc[5].loc[5] * SCALPR2

    #     API less than 27 Sour
    # T105(089:094,IY,IS)=RFCRUDEWHP(5,1:6,IY)
    z[94] = dfd["RFCRUDEWHP"].loc[5].loc[6] * SCALPR2

    #     API 40-50 Sweet
    # T105(145:146,IY,IS)=RFCRUDEWHP(5,10:11,IY)
    z[145] = dfd["RFCRUDEWHP"].loc[5].loc[10] * SCALPR2

    #     API 50+ Sweet
    # T105(145:146,IY,IS)=RFCRUDEWHP(5,10:11,IY)
    z[146] = dfd["RFCRUDEWHP"].loc[5].loc[11] * SCALPR2

    # z[95] = z[89] + z[90] + z[91] + z[92] + z[93] + z[94] + z[145] + z[146]
    z[95] = 0 * z[146]

    #   Region 6 - PADD IV
    # T105(096:101,IY,IS)=RFCRUDEWHP(6,1:6,IY)
    # z[102] = dfd['RFCRUDEWHP'].loc[6,<check>]

    #     API 35-40 Sweet
    # T105(096:101,IY,IS)=RFCRUDEWHP(6,1:6,IY)
    z[96] = dfd["RFCRUDEWHP"].loc[6].loc[1] * SCALPR2

    #     API 35+ Sour
    # T105(096:101,IY,IS)=RFCRUDEWHP(6,1:6,IY)
    z[97] = dfd["RFCRUDEWHP"].loc[6].loc[2] * SCALPR2

    #     API 27-35 MediSour
    # T105(096:101,IY,IS)=RFCRUDEWHP(6,1:6,IY)
    z[98] = dfd["RFCRUDEWHP"].loc[6].loc[3] * SCALPR2

    #     API 27-35 Sour
    # T105(096:101,IY,IS)=RFCRUDEWHP(6,1:6,IY)
    z[99] = dfd["RFCRUDEWHP"].loc[6].loc[4] * SCALPR2

    #     API less than 27 Sweet
    # T105(096:101,IY,IS)=RFCRUDEWHP(6,1:6,IY)
    z[100] = dfd["RFCRUDEWHP"].loc[6].loc[5] * SCALPR2

    #     API less than 27 Sour
    # T105(096:101,IY,IS)=RFCRUDEWHP(6,1:6,IY)
    z[101] = dfd["RFCRUDEWHP"].loc[6].loc[6] * SCALPR2

    #     API 40-50 Sweet
    # T105(147:148,IY,IS)=RFCRUDEWHP(6,10:11,IY)
    z[147] = dfd["RFCRUDEWHP"].loc[6].loc[10] * SCALPR2

    #     API 50+ Sweet
    # T105(147:148,IY,IS)=RFCRUDEWHP(6,10:11,IY)
    z[148] = dfd["RFCRUDEWHP"].loc[6].loc[11] * SCALPR2

    #
    # z[102] = z[96] + z[97] + z[98] + z[99] + z[100] + z[101] + z[147] + z[148]
    z[102] = 0 * z[148]

    #   Region 7 - PADD V California
    # T105(103:103,IY,IS)=RFCRUDEWHP(7,7:7,IY)
    z[104] = z[148] * 0.0

    #     California
    # T105(103:103,IY,IS)=RFCRUDEWHP(7,7:7,IY)
    z[103] = dfd["RFCRUDEWHP"].loc[7].loc[7] * SCALPR2

    #

    #   Region 8 - PADD V Other
    # T105(105:110,IY,IS)=RFCRUDEWHP(8,1:6,IY)
    z[111] = z[103] * 0.0

    #     API 35-40 Sweet
    # T105(105:110,IY,IS)=RFCRUDEWHP(8,1:6,IY)
    z[105] = dfd["RFCRUDEWHP"].loc[8].loc[1] * SCALPR2

    #     API 35+ Sour
    # T105(105:110,IY,IS)=RFCRUDEWHP(8,1:6,IY)
    z[106] = dfd["RFCRUDEWHP"].loc[8].loc[2] * SCALPR2

    #     API 27-35 MediSour
    # T105(105:110,IY,IS)=RFCRUDEWHP(8,1:6,IY)
    z[107] = dfd["RFCRUDEWHP"].loc[8].loc[3] * SCALPR2

    #     API 27-35 Sour
    # T105(105:110,IY,IS)=RFCRUDEWHP(8,1:6,IY)
    z[108] = dfd["RFCRUDEWHP"].loc[8].loc[4] * SCALPR2

    #     API less than 27 Sweet
    # T105(105:110,IY,IS)=RFCRUDEWHP(8,1:6,IY)
    z[109] = dfd["RFCRUDEWHP"].loc[8].loc[5] * SCALPR2

    #     API less than 27 Sour
    # T105(105:110,IY,IS)=RFCRUDEWHP(8,1:6,IY)
    z[110] = dfd["RFCRUDEWHP"].loc[8].loc[6] * SCALPR2

    #     API 40-50 Sweet
    # T105(149:150,IY,IS)=RFCRUDEWHP(8,10:11,IY)
    z[149] = dfd["RFCRUDEWHP"].loc[8].loc[10] * SCALPR2

    #     API 50+ Sweet
    # T105(149:150,IY,IS)=RFCRUDEWHP(8,10:11,IY)
    z[150] = dfd["RFCRUDEWHP"].loc[8].loc[11] * SCALPR2

    #

    #   United States

    #     API 35-40 Sweet
    # T105(112,IY,IS)=(OGCRUDEREF(1,1,IY)*RFCRUDEWHP(1,1,IY)+OGCRUDEREF(2,1,IY)*RFCRUDEWHP(2,1,IY)+OGCRUDEREF(3,1,IY)*RFCRUDEWHP(3,1,IY)+OGCRUDEREF(4,1,IY)*RFCRUDEWHP(4,1,IY)+OGCRUDEREF(5,1,IY)*RFCRUDEWHP(5,1,IY)+OGCRUDEREF(6,1,IY)*RFCRUDEWHP(6,1,IY)+OGCRUDEREF(8,1,IY)*RFCRUDEWHP(8,1,IY))/(OGCRUDEREF(1,1,IY)+OGCRUDEREF(2,1,IY)+OGCRUDEREF(3,1,IY)+OGCRUDEREF(4,1,IY)+OGCRUDEREF(5,1,IY)+OGCRUDEREF(6,1,IY)+OGCRUDEREF(8,1,IY))
    z[112] = (
        (
            dfd["OGCRUDEREF"].loc[1].loc[1] * dfd["RFCRUDEWHP"].loc[1].loc[1]
            + dfd["OGCRUDEREF"].loc[2].loc[1] * dfd["RFCRUDEWHP"].loc[2].loc[1]
            + dfd["OGCRUDEREF"].loc[3].loc[1] * dfd["RFCRUDEWHP"].loc[3].loc[1]
            + dfd["OGCRUDEREF"].loc[4].loc[1] * dfd["RFCRUDEWHP"].loc[4].loc[1]
            + dfd["OGCRUDEREF"].loc[5].loc[1] * dfd["RFCRUDEWHP"].loc[5].loc[1]
            + dfd["OGCRUDEREF"].loc[6].loc[1] * dfd["RFCRUDEWHP"].loc[6].loc[1]
            + dfd["OGCRUDEREF"].loc[8].loc[1] * dfd["RFCRUDEWHP"].loc[8].loc[1]
        )
        / (
            dfd["OGCRUDEREF"].loc[1].loc[1]
            + dfd["OGCRUDEREF"].loc[2].loc[1]
            + dfd["OGCRUDEREF"].loc[3].loc[1]
            + dfd["OGCRUDEREF"].loc[4].loc[1]
            + dfd["OGCRUDEREF"].loc[5].loc[1]
            + dfd["OGCRUDEREF"].loc[6].loc[1]
            + dfd["OGCRUDEREF"].loc[8].loc[1]
        )
    ) * SCALPR2

    #     API 35+ Sour
    # T105(113,IY,IS)=(OGCRUDEREF(1,2,IY)*RFCRUDEWHP(1,2,IY)+OGCRUDEREF(2,2,IY)*RFCRUDEWHP(2,2,IY)+OGCRUDEREF(3,2,IY)*RFCRUDEWHP(3,2,IY)+OGCRUDEREF(4,2,IY)*RFCRUDEWHP(4,2,IY)+OGCRUDEREF(5,2,IY)*RFCRUDEWHP(5,2,IY)+OGCRUDEREF(6,2,IY)*RFCRUDEWHP(6,2,IY)+OGCRUDEREF(8,2,IY)*RFCRUDEWHP(8,2,IY))/(OGCRUDEREF(1,2,IY)+OGCRUDEREF(2,2,IY)+OGCRUDEREF(3,2,IY)+OGCRUDEREF(4,2,IY)+OGCRUDEREF(5,2,IY)+OGCRUDEREF(6,2,IY)+OGCRUDEREF(8,2,IY))
    z[113] = (
        (
            dfd["OGCRUDEREF"].loc[1, 2] * dfd["RFCRUDEWHP"].loc[1, 2]
            + dfd["OGCRUDEREF"].loc[2, 2] * dfd["RFCRUDEWHP"].loc[2, 2]
            + dfd["OGCRUDEREF"].loc[3, 2] * dfd["RFCRUDEWHP"].loc[3, 2]
            + dfd["OGCRUDEREF"].loc[4, 2] * dfd["RFCRUDEWHP"].loc[4, 2]
            + dfd["OGCRUDEREF"].loc[5, 2] * dfd["RFCRUDEWHP"].loc[5, 2]
            + dfd["OGCRUDEREF"].loc[6, 2] * dfd["RFCRUDEWHP"].loc[6, 2]
            + dfd["OGCRUDEREF"].loc[8, 2] * dfd["RFCRUDEWHP"].loc[8, 2]
        )
        / (
            dfd["OGCRUDEREF"].loc[1, 2]
            + dfd["OGCRUDEREF"].loc[2, 2]
            + dfd["OGCRUDEREF"].loc[3, 2]
            + dfd["OGCRUDEREF"].loc[4, 2]
            + dfd["OGCRUDEREF"].loc[5, 2]
            + dfd["OGCRUDEREF"].loc[6, 2]
            + dfd["OGCRUDEREF"].loc[8, 2]
        )
    ) * SCALPR2

    #     #API 27-35 MediSour
    # T105(114,IY,IS)=(OGCRUDEREF(1,3,IY)*RFCRUDEWHP(1,3,IY)+OGCRUDEREF(2,3,IY)*RFCRUDEWHP(2,3,IY)+OGCRUDEREF(3,3,IY)*RFCRUDEWHP(3,3,IY)+OGCRUDEREF(4,3,IY)*RFCRUDEWHP(4,3,IY)+OGCRUDEREF(5,3,IY)*RFCRUDEWHP(5,3,IY)+OGCRUDEREF(6,3,IY)*RFCRUDEWHP(6,3,IY)+OGCRUDEREF(8,3,IY)*RFCRUDEWHP(8,3,IY))/(OGCRUDEREF(1,3,IY)+OGCRUDEREF(2,3,IY)+OGCRUDEREF(3,3,IY)+OGCRUDEREF(4,3,IY)+OGCRUDEREF(5,3,IY)+OGCRUDEREF(6,3,IY)+OGCRUDEREF(8,3,IY))
    z[114] = (
        (
            dfd["OGCRUDEREF"].loc[1].loc[3] * dfd["RFCRUDEWHP"].loc[1].loc[3]
            + dfd["OGCRUDEREF"].loc[2].loc[3] * dfd["RFCRUDEWHP"].loc[2].loc[3]
            + dfd["OGCRUDEREF"].loc[3].loc[3] * dfd["RFCRUDEWHP"].loc[3].loc[3]
            + dfd["OGCRUDEREF"].loc[4].loc[3] * dfd["RFCRUDEWHP"].loc[4].loc[3]
            + dfd["OGCRUDEREF"].loc[5].loc[3] * dfd["RFCRUDEWHP"].loc[5].loc[3]
            + dfd["OGCRUDEREF"].loc[6].loc[3] * dfd["RFCRUDEWHP"].loc[6].loc[3]
            + dfd["OGCRUDEREF"].loc[8].loc[3] * dfd["RFCRUDEWHP"].loc[8].loc[3]
        )
        / (
            dfd["OGCRUDEREF"].loc[1].loc[3]
            + dfd["OGCRUDEREF"].loc[2].loc[3]
            + dfd["OGCRUDEREF"].loc[3].loc[3]
            + dfd["OGCRUDEREF"].loc[4].loc[3]
            + dfd["OGCRUDEREF"].loc[5].loc[3]
            + dfd["OGCRUDEREF"].loc[6].loc[3]
            + dfd["OGCRUDEREF"].loc[8].loc[3]
        )
    ) * SCALPR2

    #     API 27-35 Sour
    # T105(115,IY,IS)=(OGCRUDEREF(1,4,IY)*RFCRUDEWHP(1,4,IY)+OGCRUDEREF(2,4,IY)*RFCRUDEWHP(2,4,IY)+OGCRUDEREF(3,4,IY)*RFCRUDEWHP(3,4,IY)+OGCRUDEREF(4,4,IY)*RFCRUDEWHP(4,4,IY)+OGCRUDEREF(5,4,IY)*RFCRUDEWHP(5,4,IY)+OGCRUDEREF(6,4,IY)*RFCRUDEWHP(6,4,IY)+OGCRUDEREF(8,4,IY)*RFCRUDEWHP(8,4,IY))/(OGCRUDEREF(1,4,IY)+OGCRUDEREF(2,4,IY)+OGCRUDEREF(3,4,IY)+OGCRUDEREF(4,4,IY)+OGCRUDEREF(5,4,IY)+OGCRUDEREF(6,4,IY)+OGCRUDEREF(8,4,IY))
    z[115] = (
        (
            dfd["OGCRUDEREF"].loc[1].loc[4] * dfd["RFCRUDEWHP"].loc[1].loc[4]
            + dfd["OGCRUDEREF"].loc[2].loc[4] * dfd["RFCRUDEWHP"].loc[2].loc[4]
            + dfd["OGCRUDEREF"].loc[3].loc[4] * dfd["RFCRUDEWHP"].loc[3].loc[4]
            + dfd["OGCRUDEREF"].loc[4].loc[4] * dfd["RFCRUDEWHP"].loc[4].loc[4]
            + dfd["OGCRUDEREF"].loc[5].loc[4] * dfd["RFCRUDEWHP"].loc[5].loc[4]
            + dfd["OGCRUDEREF"].loc[6].loc[4] * dfd["RFCRUDEWHP"].loc[6].loc[4]
            + dfd["OGCRUDEREF"].loc[8].loc[4] * dfd["RFCRUDEWHP"].loc[8].loc[4]
        )
        / (
            dfd["OGCRUDEREF"].loc[1].loc[4]
            + dfd["OGCRUDEREF"].loc[2].loc[4]
            + dfd["OGCRUDEREF"].loc[3].loc[4]
            + dfd["OGCRUDEREF"].loc[4].loc[4]
            + dfd["OGCRUDEREF"].loc[5].loc[4]
            + dfd["OGCRUDEREF"].loc[6].loc[4]
            + dfd["OGCRUDEREF"].loc[8].loc[4]
        )
    ) * SCALPR2

    #     API less than 27 Sweet
    # T105(116,IY,IS)=(OGCRUDEREF(1,5,IY)*RFCRUDEWHP(1,5,IY)+OGCRUDEREF(2,5,IY)*RFCRUDEWHP(2,5,IY)+OGCRUDEREF(3,5,IY)*RFCRUDEWHP(3,5,IY)+OGCRUDEREF(4,5,IY)*RFCRUDEWHP(4,5,IY)+OGCRUDEREF(5,5,IY)*RFCRUDEWHP(5,5,IY)+OGCRUDEREF(6,5,IY)*RFCRUDEWHP(6,5,IY)+OGCRUDEREF(8,5,IY)*RFCRUDEWHP(8,5,IY))/(OGCRUDEREF(1,5,IY)+OGCRUDEREF(2,5,IY)+OGCRUDEREF(3,5,IY)+OGCRUDEREF(4,5,IY)+OGCRUDEREF(5,5,IY)+OGCRUDEREF(6,5,IY)+OGCRUDEREF(8,5,IY))
    z[116] = (
        (
            dfd["OGCRUDEREF"].loc[1].loc[5] * dfd["RFCRUDEWHP"].loc[1].loc[5]
            + dfd["OGCRUDEREF"].loc[2].loc[5] * dfd["RFCRUDEWHP"].loc[2].loc[5]
            + dfd["OGCRUDEREF"].loc[3].loc[5] * dfd["RFCRUDEWHP"].loc[3].loc[5]
            + dfd["OGCRUDEREF"].loc[4].loc[5] * dfd["RFCRUDEWHP"].loc[4].loc[5]
            + dfd["OGCRUDEREF"].loc[5].loc[5] * dfd["RFCRUDEWHP"].loc[5].loc[5]
            + dfd["OGCRUDEREF"].loc[6].loc[5] * dfd["RFCRUDEWHP"].loc[6].loc[5]
            + dfd["OGCRUDEREF"].loc[8].loc[5] * dfd["RFCRUDEWHP"].loc[8].loc[5]
        )
        / (
            dfd["OGCRUDEREF"].loc[1].loc[5]
            + dfd["OGCRUDEREF"].loc[2].loc[5]
            + dfd["OGCRUDEREF"].loc[3].loc[5]
            + dfd["OGCRUDEREF"].loc[4].loc[5]
            + dfd["OGCRUDEREF"].loc[5].loc[5]
            + dfd["OGCRUDEREF"].loc[6].loc[5]
            + dfd["OGCRUDEREF"].loc[8].loc[5]
        )
    ) * SCALPR2

    #     API less than 27 Sour
    # T105(117,IY,IS)=(OGCRUDEREF(1,6,IY)*RFCRUDEWHP(1,6,IY)+OGCRUDEREF(2,6,IY)*RFCRUDEWHP(2,6,IY)+OGCRUDEREF(3,6,IY)*RFCRUDEWHP(3,6,IY)+OGCRUDEREF(4,6,IY)*RFCRUDEWHP(4,6,IY)+OGCRUDEREF(5,6,IY)*RFCRUDEWHP(5,6,IY)+OGCRUDEREF(6,6,IY)*RFCRUDEWHP(6,6,IY)+OGCRUDEREF(8,6,IY)*RFCRUDEWHP(8,6,IY))/(OGCRUDEREF(1,6,IY)+OGCRUDEREF(2,6,IY)+OGCRUDEREF(3,6,IY)+OGCRUDEREF(4,6,IY)+OGCRUDEREF(5,6,IY)+OGCRUDEREF(6,6,IY)+OGCRUDEREF(8,6,IY))
    z[117] = (
        (
            dfd["OGCRUDEREF"].loc[1, 6] * dfd["RFCRUDEWHP"].loc[1, 6]
            + dfd["OGCRUDEREF"].loc[2, 6] * dfd["RFCRUDEWHP"].loc[2, 6]
            + dfd["OGCRUDEREF"].loc[3, 6] * dfd["RFCRUDEWHP"].loc[3, 6]
            + dfd["OGCRUDEREF"].loc[4, 6] * dfd["RFCRUDEWHP"].loc[4, 6]
            + dfd["OGCRUDEREF"].loc[5, 6] * dfd["RFCRUDEWHP"].loc[5, 6]
            + dfd["OGCRUDEREF"].loc[6, 6] * dfd["RFCRUDEWHP"].loc[6, 6]
            + dfd["OGCRUDEREF"].loc[8, 6] * dfd["RFCRUDEWHP"].loc[8, 6]
        )
        / (
            dfd["OGCRUDEREF"].loc[1, 6]
            + dfd["OGCRUDEREF"].loc[2, 6]
            + dfd["OGCRUDEREF"].loc[3, 6]
            + dfd["OGCRUDEREF"].loc[4, 6]
            + dfd["OGCRUDEREF"].loc[5, 6]
            + dfd["OGCRUDEREF"].loc[6, 6]
            + dfd["OGCRUDEREF"].loc[8, 6]
        )
    ) * SCALPR2

    #     #California
    # T105(118,IY,IS)=RFCRUDEWHP(7,7,IY)
    z[118] = dfd["RFCRUDEWHP"].loc[7].loc[7] * SCALPR2

    #     API 40-50 Sweet
    # T105(151,IY,IS)=(OGCRUDEREF(1,10,IY)*RFCRUDEWHP(1,10,IY)+OGCRUDEREF(2,10,IY)*RFCRUDEWHP(2,10,IY)+OGCRUDEREF(3,10,IY)*RFCRUDEWHP(3,10,IY)+OGCRUDEREF(4,10,IY)*RFCRUDEWHP(4,10,IY)+OGCRUDEREF(5,10,IY)*RFCRUDEWHP(5,10,IY)+OGCRUDEREF(6,10,IY)*RFCRUDEWHP(6,10,IY)+OGCRUDEREF(8,10,IY)*RFCRUDEWHP(8,10,IY))/(OGCRUDEREF(1,10,IY)+OGCRUDEREF(2,10,IY)+OGCRUDEREF(3,10,IY)+OGCRUDEREF(4,10,IY)+OGCRUDEREF(5,10,IY)+OGCRUDEREF(6,10,IY)+OGCRUDEREF(8,10,IY))
    z[151] = (
        (
            dfd["OGCRUDEREF"].loc[1].loc[10] * dfd["RFCRUDEWHP"].loc[1].loc[10]
            + dfd["OGCRUDEREF"].loc[2].loc[10] * dfd["RFCRUDEWHP"].loc[2].loc[10]
            + dfd["OGCRUDEREF"].loc[3].loc[10] * dfd["RFCRUDEWHP"].loc[3].loc[10]
            + dfd["OGCRUDEREF"].loc[4].loc[10] * dfd["RFCRUDEWHP"].loc[4].loc[10]
            + dfd["OGCRUDEREF"].loc[5].loc[10] * dfd["RFCRUDEWHP"].loc[5].loc[10]
            + dfd["OGCRUDEREF"].loc[6].loc[10] * dfd["RFCRUDEWHP"].loc[6].loc[10]
            + dfd["OGCRUDEREF"].loc[8].loc[10] * dfd["RFCRUDEWHP"].loc[8].loc[10]
        )
        / (
            dfd["OGCRUDEREF"].loc[1].loc[10]
            + dfd["OGCRUDEREF"].loc[2].loc[10]
            + dfd["OGCRUDEREF"].loc[3].loc[10]
            + dfd["OGCRUDEREF"].loc[4].loc[10]
            + dfd["OGCRUDEREF"].loc[5].loc[10]
            + dfd["OGCRUDEREF"].loc[6].loc[10]
            + dfd["OGCRUDEREF"].loc[8].loc[10]
        )
    ) * SCALPR2

    #     API 50+ Sweet
    # T105(152,IY,IS)=(OGCRUDEREF(1,11,IY)*RFCRUDEWHP(1,11,IY)+OGCRUDEREF(2,11,IY)*RFCRUDEWHP(2,11,IY)+OGCRUDEREF(3,11,IY)*RFCRUDEWHP(3,11,IY)+OGCRUDEREF(4,11,IY)*RFCRUDEWHP(4,11,IY)+OGCRUDEREF(5,11,IY)*RFCRUDEWHP(5,11,IY)+OGCRUDEREF(6,11,IY)*RFCRUDEWHP(6,11,IY)+OGCRUDEREF(8,11,IY)*RFCRUDEWHP(8,11,IY))/(OGCRUDEREF(1,11,IY)+OGCRUDEREF(2,11,IY)+OGCRUDEREF(3,11,IY)+OGCRUDEREF(4,11,IY)+OGCRUDEREF(5,11,IY)+OGCRUDEREF(6,11,IY)+OGCRUDEREF(8,11,IY))
    z[152] = (
        (
            dfd["OGCRUDEREF"].loc[1].loc[11] * dfd["RFCRUDEWHP"].loc[1].loc[11]
            + dfd["OGCRUDEREF"].loc[2].loc[11] * dfd["RFCRUDEWHP"].loc[2].loc[11]
            + dfd["OGCRUDEREF"].loc[3].loc[11] * dfd["RFCRUDEWHP"].loc[3].loc[11]
            + dfd["OGCRUDEREF"].loc[4].loc[11] * dfd["RFCRUDEWHP"].loc[4].loc[11]
            + dfd["OGCRUDEREF"].loc[5].loc[11] * dfd["RFCRUDEWHP"].loc[5].loc[11]
            + dfd["OGCRUDEREF"].loc[6].loc[11] * dfd["RFCRUDEWHP"].loc[6].loc[11]
            + dfd["OGCRUDEREF"].loc[8].loc[11] * dfd["RFCRUDEWHP"].loc[8].loc[11]
        )
        / (
            dfd["OGCRUDEREF"].loc[1].loc[11]
            + dfd["OGCRUDEREF"].loc[2].loc[11]
            + dfd["OGCRUDEREF"].loc[3].loc[11]
            + dfd["OGCRUDEREF"].loc[4].loc[11]
            + dfd["OGCRUDEREF"].loc[5].loc[11]
            + dfd["OGCRUDEREF"].loc[6].loc[11]
            + dfd["OGCRUDEREF"].loc[8].loc[11]
        )
    ) * SCALPR2

    # T105(119,IY,IS)=(T105(112,IY,IS)*T105(052,IY,IS)+T105(113,IY,IS)*T105(053,IY,IS)+T105(114,IY,IS)*T105(054,IY,IS)+T105(115,IY,IS)*T105(055,IY,IS)+T105(116,IY,IS)*T105(056,IY,IS)+T105(117,IY,IS)*T105(057,IY,IS)+T105(118,IY,IS)*T105(058,IY,IS)+T105(151,IY,IS)*T105(135,IY,IS)+T105(152,IY,IS)*T105(136,IY,IS))/(FSUM(T105(52,IY,IS),7)+FSUM(T105(135,IY,IS),2))
    z[119] = (
        z[112] * z[52]
        + z[113] * z[53]
        + z[114] * z[54]
        + z[115] * z[55]
        + z[116] * z[56]
        + z[117] * z[57]
        + z[118] * z[58]
        + z[151] * z[135]
        + z[152] * z[136]
    ) / (z[52] + z[53] + z[54] + z[55] + z[56] + z[57] + z[58] + z[135] + z[136])

    return z
