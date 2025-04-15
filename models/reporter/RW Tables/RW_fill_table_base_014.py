# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""

import pandas as pd


def fill_table_base_014(dfd, table_spec, table_id):
    """Fill table   Oil and Gas Supply

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


    """

    z = {}

    FYRPRC = int(table_spec["settings"]["year_index_real_dol_priceyr"])
    # Get inflation index from 1990->2050
    MC_JPGDP = dfd["MC_JPGDP"].loc[1990:]

    # Get inflation index of FYRPRC (2022)
    SCALPR2 = MC_JPGDP.loc[FYRPRC]  # 2022 = column 33

    OILTYPES = dfd["OILTYPES_rwpre"]
    RDAYS = dfd["RDAYS_rwpre"]
    OGDIST = dfd["OGDIST_rwpre"]
    MNL48N = dfd["MNL48N_rwpre"]
    MNUMOR = dfd["MNUMOR_rwpre"]
    GASTYPES = dfd["GASTYPES_rwpre"]

    #   Oil and Gas Supply
    #    Production and Supply
    #
    #   Crude Oil
    #
    #   Lower 48 Average Wellhead Price 1/

    #   (#### dollars per barrel)
    # T14(1,IY,IS)=OGPCRWHP(IY)
    z[1] = dfd["OGPCRWHP"] * SCALPR2

    #

    #   Production (million barrels per day) 2/

    #      United States Total
    # T14(2,IY,IS)=(OGQCRREP(1,IY)+OGQCRREP(2,IY)+OGQCRREP(3,IY)+OGQCRREP(4,IY))/RDAYS
    z[2] = dfd["OGQCRREP"].loc[1:4].sum() / RDAYS

    #         Lower 48 Onshore
    # T14(3,IY,IS)=(OGQCRREP(1,IY)+OGQCRREP(2,IY))/RDAYS
    z[3] = (dfd["OGQCRREP"].loc[1] + dfd["OGQCRREP"].loc[2]) / RDAYS

    #            Tight Oil 3/
    # T14(44,IY,IS)=SUM(OGQSHLOIL(:,IY))
    z[44] = dfd["OGQSHLOIL"].loc[:, :].sum()

    #            Carbon Dioxide Enhanced Oil Recovery
    # T14(45,IY,IS)=SUM(OGEORPRD(8,1:13,IY))/1000./RDAYS
    z[45] = dfd["OGEORPRD"].loc[8].loc[1:13].sum() / 1000.0 / (RDAYS)

    #            Other
    # T14(46,IY,IS)=T14(3,IY,IS)-T14(44,IY,IS)-T14(45,IY,IS)
    z[46] = z[3] - z[44] - z[45]

    #         Lower 48 Offshore
    # T14(6,IY,IS)=OGQCRREP(3,IY)/RDAYS
    z[6] = dfd["OGQCRREP"].loc[3] / (RDAYS)

    #            State
    # T14(55,IY,IS)=(OGPRDOFF(1,1,IY)+OGPRDOFF(2,1,IY)+OGPRDOFF(3,1,IY))/RDAYS
    z[55] = (
        dfd["OGPRDOFF"].loc[1].loc[1]
        + dfd["OGPRDOFF"].loc[2].loc[1]
        + dfd["OGPRDOFF"].loc[3].loc[1]
    ) / (RDAYS)

    #            Federal
    # T14(56,IY,IS)=T14(6,IY,IS)-T14(55,IY,IS)
    z[56] = z[6] - z[55]

    #         Alaska
    # T14(7,IY,IS)=OGQCRREP(4,IY)/RDAYS
    z[7] = dfd["OGQCRREP"].loc[4] / (RDAYS)

    #            State Offshore
    # T14(58,IY,IS)=OGPRDOFF(5,1,IY)/RDAYS
    z[58] = dfd["OGPRDOFF"].loc[5].loc[1] / (RDAYS)

    #            Federal Offshore
    # T14(59,IY,IS)=OGPRDOFF(4,1,IY)/RDAYS
    z[59] = dfd["OGPRDOFF"].loc[6].loc[1] / (RDAYS)

    #            Onshore
    # T14(57,IY,IS)=T14(7,IY,IS)-T14(58,IY,IS)-T14(59,IY,IS)
    z[57] = z[7] - z[58] - z[59]

    #   Federal Land Crude Oil Production
    # No FTAB equivalent, new rows
    z[76] = dfd["OGCOPRD_FED"].loc[14]

    #   Non-Federal Land Crude Oil Production
    z[77] = dfd["OGCOPRD_NONFED"].loc[14]

    #   Lower 48 End of Year Reserves 2/

    #   (billion barrels)
    # T14(8,IY,IS)=(OGQCRRSV(IY)*.001)
    z[8] = dfd["OGQCRRSV"] * 0.001

    #

    #   Natural Gas Plant Liquids Production

    #   (million barrels per day)

    #         Lower 48 Onshore
    # T14(48,IY,IS)=SUM(OGNGPLPRD(4:66,IY))+OGNGPLPRD(1,IY)+OGNGPLPRD(2,IY)
    z[48] = (
        dfd["OGNGPLPRD"].loc[4:66, :].sum()
        + dfd["OGNGPLPRD"].loc[1]
        + dfd["OGNGPLPRD"].loc[2]
    )

    #         Lower 48 Offshore
    # T14(49,IY,IS)=SUM(OGNGPLPRD(67:74,IY))+SUM(OGNGPLPRD(76:83,IY))
    z[49] = dfd["OGNGPLPRD"].loc[67:74, :].sum() + dfd["OGNGPLPRD"].loc[76:83, :].sum()

    #         Alaska
    # T14(50,IY,IS)=OGNGPLPRD(3,IY)+OGNGPLPRD(75,IY)+OGNGPLPRD(84,IY)
    z[50] = (
        dfd["OGNGPLPRD"].loc[3] + dfd["OGNGPLPRD"].loc[75] + dfd["OGNGPLPRD"].loc[84]
    )

    #      United States Total
    # T14(47,IY,IS)=FSUM(T14(48,IY,IS),3)
    z[47] = z[48] + z[49] + z[50]

    #   Natural Gas

    #

    #   Prices (#### dollars per million Btu)

    #      Henry Hub Spot Price
    # T14(31,IY,IS)=OGHHPRNG(IY)
    z[31] = dfd["OGHHPRNG"] * SCALPR2

    #

    #   Dry Production (trillion cubic feet) 4/

    #      United States Total
    # T14(10,IY,IS)=(OGQNGREP(1,IY)+OGQNGREP(2,IY)+OGQNGREP(3,IY)+OGQNGREP(4,IY)+OGQNGREP(5,IY)+OGQNGREP(6,IY)+OGQNGREP(7,IY)+OGQNGREP(8,IY)+OGSHALENG(IY))*.001
    z[10] = (
        dfd["OGQNGREP"].loc[1]
        + dfd["OGQNGREP"].loc[2]
        + dfd["OGQNGREP"].loc[3]
        + dfd["OGQNGREP"].loc[4]
        + dfd["OGQNGREP"].loc[5]
        + dfd["OGQNGREP"].loc[6]
        + dfd["OGQNGREP"].loc[7]
        + dfd["OGQNGREP"].loc[8]
        + dfd["OGSHALENG"]
    ) * 0.001

    #         Lower 48 Onshore
    # T14(11,IY,IS)=(OGQNGREP(1,IY)+OGQNGREP(2,IY)+OGQNGREP(3,IY)+OGQNGREP(4,IY)+OGQNGREP(5,IY)+OGSHALENG(IY))*.001
    z[11] = (
        dfd["OGQNGREP"].loc[1]
        + dfd["OGQNGREP"].loc[2]
        + dfd["OGQNGREP"].loc[3]
        + dfd["OGQNGREP"].loc[4]
        + dfd["OGQNGREP"].loc[5]
        + dfd["OGSHALENG"]
    ) * 0.001

    #            Associated-Dissolved 5/
    # T14(12,IY,IS)=(SUM(OGADGPRD(1:66,OILTYPES,IY))-OGADGPRD(3,OILTYPES,IY))/1000.
    z[12] = (
        dfd["OGADGPRD"].loc[1:66, OILTYPES, :].sum()
        - dfd["OGADGPRD"].loc[3].loc[OILTYPES]
    ) / 1000.0

    #            Non-Associated
    # T14(13,IY,IS)=(OGQNGREP(1,IY)+OGQNGREP(2,IY)+OGQNGREP(3,IY)+OGQNGREP(4,IY))*.001
    z[13] = (
        dfd["OGQNGREP"].loc[1]
        + dfd["OGQNGREP"].loc[2]
        + dfd["OGQNGREP"].loc[3]
        + dfd["OGQNGREP"].loc[4]
    ) * 0.001

    #            Tight Gas
    # T14(15,IY,IS)=OGQNGREP(3,IY)/1000.
    z[15] = dfd["OGQNGREP"].loc[3] / 1000.0

    #            Shale Gas and Tight Oil Plays 3/
    # T14(26,IY,IS)=OGQNGREP(1,IY)/1000.
    z[26] = dfd["OGQNGREP"].loc[1] / 1000.0

    #            Coalbed Methane
    # T14(27,IY,IS)=OGQNGREP(2,IY)/1000.
    z[27] = dfd["OGQNGREP"].loc[2] / 1000.0

    #            Other
    # T14(14,IY,IS)=(OGQNGREP(4,IY)+OGQNGREP(5,IY)+OGSHALENG(IY))*.001
    z[14] = (dfd["OGQNGREP"].loc[4] + dfd["OGQNGREP"].loc[5] + dfd["OGSHALENG"]) * 0.001

    #         Lower 48 Offshore
    # T14(16,IY,IS)=(OGQNGREP(6,IY)+OGQNGREP(7,IY))*.001
    z[16] = (dfd["OGQNGREP"].loc[6] + dfd["OGQNGREP"].loc[7]) * 0.001

    #            State
    # T14(60,IY,IS)=(OGPRDOFF(1,2,IY)+OGPRDOFF(2,2,IY)+OGPRDOFF(3,2,IY))/1000.
    z[60] = (
        dfd["OGPRDOFF"].loc[1].loc[2]
        + dfd["OGPRDOFF"].loc[2].loc[2]
        + dfd["OGPRDOFF"].loc[3].loc[2]
    ) / 1000.0

    #            Federal
    # T14(61,IY,IS)=T14(16,IY,IS)-T14(60,IY,IS)
    z[61] = z[16] - z[60]

    #            Associated-Dissolved 5/
    # T14(17,IY,IS)=(SUM(OGADGPRD(67:OGDIST,OILTYPES,IY))-OGADGPRD(75,OILTYPES,IY)-OGADGPRD(84,OILTYPES,IY))/1000.
    z[17] = (
        dfd["OGADGPRD"].loc[67:OGDIST, OILTYPES, :].sum()
        - dfd["OGADGPRD"].loc[75].loc[OILTYPES]
        - dfd["OGADGPRD"].loc[84].loc[OILTYPES]
    ) / 1000.0

    #            Non-Associated
    # T14(18,IY,IS)=OGQNGREP(6,IY)*.001
    z[18] = dfd["OGQNGREP"].loc[6] * 0.001

    #         Alaska
    # T14(19,IY,IS)=OGQNGREP(8,IY)*.001
    z[19] = dfd["OGQNGREP"].loc[8] * 0.001

    #            State Offshore
    # T14(63,IY,IS)=OGPRDOFF(5,2,IY)/1000.
    z[63] = dfd["OGPRDOFF"].loc[5].loc[2] / 1000.0

    #            Federal Offshore
    # T14(64,IY,IS)=OGPRDOFF(4,2,IY)/1000.
    z[64] = dfd["OGPRDOFF"].loc[6].loc[2] / 1000.0

    #            Onshore
    # T14(62,IY,IS)=T14(19,IY,IS)-T14(63,IY,IS)-T14(64,IY,IS)
    z[62] = z[19] - z[63] - z[64]

    #   Federal Land Natural Gas Production
    z[78] = dfd["OGNGPRD_FED"].loc[14]

    #   Non-Federal Land Natural Gas Production
    z[79] = dfd["OGNGPRD_NONFED"].loc[14]

    #   Lower 48 End of Year Dry Reserves (tcf) 4/
    # T14(20,IY,IS)=OGQNGRSV(IY)*.001
    z[20] = dfd["OGQNGRSV"] * 0.001

    #

    #   Supplemental Gas Supplies (tcf) 5/
    # T14(21,IY,IS)=OGQNGREP(10,IY)+OGQNGREP(11,IY)+OGQNGREP(12,IY)
    z[21] = dfd["OGQNGREP"].loc[10] + dfd["OGQNGREP"].loc[11] + dfd["OGQNGREP"].loc[12]

    #

    #

    #   Total Lower 48 Wells Drilled (thousands)
    # T14(22,IY,IS)=OGNOWELL(IY)*.001
    z[22] = dfd["OGNOWELL"] * 0.001

    #

    #   -------------------------

    #   exclude the following when creating AEO Table

    #   -------------------------

    #      Enhanced Oil Recovery
    # T14(35,IY,IS)=SUM(RFQTDCRD(1:MNL48N,IY))-SUM(RFQDCRD(1:MNL48N,IY))-OGQCRREP(1,IY)/RDAYS
    z[35] = (
        dfd["RFQTDCRD"].loc[1:MNL48N, :].sum()
        - dfd["RFQDCRD"].loc[1:MNL48N, :].sum()
        - dfd["OGQCRREP"].loc[1] / (RDAYS)
    )

    #         Carbon Dioxide
    # T14(33,IY,IS)=SUM(OGEORPRD(8,1:13,IY))/1000./RDAYS
    z[33] = dfd["OGEORPRD"].loc[8, 1:13, :].sum() / 1000.0 / (RDAYS)

    #         Other
    # T14(34,IY,IS)=T14(35,IY,IS)-T14(33,IY,IS)
    z[34] = z[35] - z[33]

    #      from Oil Shale
    # T14(29,IY,IS)=OGQCRREP(1,IY)/RDAYS
    z[29] = dfd["OGQCRREP"].loc[1] / (RDAYS)

    #      from Arctic National Wildlife Refuge
    # T14(30,IY,IS)=OGQCRREP(5,IY)/RDAYS
    z[30] = dfd["OGQCRREP"].loc[5] / (RDAYS)

    #

    #   Associated-Dissolved Natural Gas Production
    # T14(28,IY,IS)=T14(12,IY,IS)+T14(17,IY,IS)
    z[28] = z[12] + z[17]

    #      Lower 48 Onshore
    # T14(12,IY,IS)=(SUM(OGADGPRD(1:66,OILTYPES,IY))-OGADGPRD(3,OILTYPES,IY))/1000.
    # z[12] = (dfd['OGADGPRD'].loc[1:66,OILTYPES,:].sum - dfd['OGADGPRD'].loc[3].loc[OILTYPES]) / 1000.

    #         East
    # T14(65,IY,IS)=(OGADGPRD(8,OILTYPES,IY)+OGADGPRD(9,OILTYPES,IY)+OGADGPRD(10,OILTYPES,IY)+OGADGPRD(12,OILTYPES,IY)+OGADGPRD(15,OILTYPES,IY)+OGADGPRD(16,OILTYPES,IY)+OGADGPRD(19,OILTYPES,IY)+OGADGPRD(22,OILTYPES,IY)+OGADGPRD(23,OILTYPES,IY)+OGADGPRD(24,OILTYPES,IY)+OGADGPRD(25,OILTYPES,IY)+OGADGPRD(33,OILTYPES,IY)+OGADGPRD(34,OILTYPES,IY)+OGADGPRD(37,OILTYPES,IY)+OGADGPRD(38,OILTYPES,IY)+OGADGPRD(40,OILTYPES,IY)+OGADGPRD(43,OILTYPES,IY)+OGADGPRD(44,OILTYPES,IY)+OGADGPRD(45,OILTYPES,IY)+OGADGPRD(47,OILTYPES,IY)+OGADGPRD(61,OILTYPES,IY)+OGADGPRD(62,OILTYPES,IY)+OGADGPRD(64,OILTYPES,IY)+OGADGPRD(65,OILTYPES,IY))/1000.
    z[65] = (
        dfd["OGADGPRD"].loc[8].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[9].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[10].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[12].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[15].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[16].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[19].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[22].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[23].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[24].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[25].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[33].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[34].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[37].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[38].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[40].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[43].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[44].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[45].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[47].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[61].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[62].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[64].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[65].loc[OILTYPES]
    ) / 1000.0

    #         Gulf Coast
    # T14(66,IY,IS)=(OGADGPRD(1,OILTYPES,IY)+OGADGPRD(2,OILTYPES,IY)+OGADGPRD(11,OILTYPES,IY)+OGADGPRD(20,OILTYPES,IY)+OGADGPRD(21,OILTYPES,IY)+OGADGPRD(27,OILTYPES,IY)+OGADGPRD(28,OILTYPES,IY)+OGADGPRD(48,OILTYPES,IY)+OGADGPRD(49,OILTYPES,IY)+OGADGPRD(50,OILTYPES,IY)+OGADGPRD(51,OILTYPES,IY)+OGADGPRD(53,OILTYPES,IY))/1000.
    z[66] = (
        dfd["OGADGPRD"].loc[1].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[2].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[11].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[20].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[21].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[27].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[28].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[48].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[49].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[50].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[51].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[53].loc[OILTYPES]
    ) / 1000.0

    #         Midcontinent
    # T14(67,IY,IS)=(OGADGPRD(5,OILTYPES,IY)+OGADGPRD(17,OILTYPES,IY)+OGADGPRD(18,OILTYPES,IY)+OGADGPRD(26,OILTYPES,IY)+OGADGPRD(29,OILTYPES,IY)+OGADGPRD(31,OILTYPES,IY)+OGADGPRD(41,OILTYPES,IY)+OGADGPRD(59,OILTYPES,IY))/1000.
    z[67] = (
        dfd["OGADGPRD"].loc[5].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[17].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[18].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[26].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[29].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[31].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[41].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[59].loc[OILTYPES]
    ) / 1000.0

    #         Southwest
    # T14(68,IY,IS)=(OGADGPRD(35,OILTYPES,IY)+OGADGPRD(52,OILTYPES,IY)+OGADGPRD(54,OILTYPES,IY)+OGADGPRD(55,OILTYPES,IY)+OGADGPRD(56,OILTYPES,IY)+OGADGPRD(57,OILTYPES,IY)+OGADGPRD(58,OILTYPES,IY))/1000.
    z[68] = (
        dfd["OGADGPRD"].loc[35].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[52].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[54].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[55].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[56].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[57].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[58].loc[OILTYPES]
    ) / 1000.0

    #         Rocky Mountain
    # T14(69,IY,IS)=(OGADGPRD(4,OILTYPES,IY)+OGADGPRD(7,OILTYPES,IY)+OGADGPRD(14,OILTYPES,IY)+OGADGPRD(32,OILTYPES,IY)+OGADGPRD(36,OILTYPES,IY)+OGADGPRD(60,OILTYPES,IY)+OGADGPRD(66,OILTYPES,IY))/1000.
    z[69] = (
        dfd["OGADGPRD"].loc[4].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[7].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[14].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[32].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[36].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[60].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[66].loc[OILTYPES]
    ) / 1000.0

    #         Northern Great Plains
    # T14(70,IY,IS)=(OGADGPRD(30,OILTYPES,IY)+OGADGPRD(39,OILTYPES,IY)+OGADGPRD(46,OILTYPES,IY))/1000.
    z[70] = (
        dfd["OGADGPRD"].loc[30].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[39].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[46].loc[OILTYPES]
    ) / 1000.0

    #         West Coast
    # T14(71,IY,IS)=(OGADGPRD(6,OILTYPES,IY)+OGADGPRD(42,OILTYPES,IY)+OGADGPRD(63,OILTYPES,IY))/1000.
    z[71] = (
        dfd["OGADGPRD"].loc[6].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[42].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[63].loc[OILTYPES]
    ) / 1000.0

    # #      Lower 48 Offshore
    # #T14(17,IY,IS)=(SUM(OGADGPRD(67:OGDIST,OILTYPES,IY))-OGADGPRD(75,OILTYPES,IY)-OGADGPRD(84,OILTYPES,IY))/1000.
    # z[17] = (dfd['OGADGPRD'].loc[67:OGDIST,OILTYPES,:].sum() - dfd['OGADGPRD'].loc[75].loc[OILTYPES] - dfd['OGADGPRD'].loc[84].loc[OILTYPES]) / 1000.

    #         Atlantic
    # T14(72,IY,IS)=(OGADGPRD(67,OILTYPES,IY)+OGADGPRD(68,OILTYPES,IY)+OGADGPRD(69,OILTYPES,IY)+OGADGPRD(76,OILTYPES,IY)+OGADGPRD(77,OILTYPES,IY)+OGADGPRD(78,OILTYPES,IY))/1000.
    z[72] = (
        dfd["OGADGPRD"].loc[67].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[68].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[69].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[76].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[77].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[78].loc[OILTYPES]
    ) / 1000.0

    #         Pacific
    # T14(73,IY,IS)=(OGADGPRD(73,OILTYPES,IY)+OGADGPRD(74,OILTYPES,IY)+OGADGPRD(82,OILTYPES,IY)+OGADGPRD(83,OILTYPES,IY))/1000.
    z[73] = (
        dfd["OGADGPRD"].loc[73].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[74].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[82].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[83].loc[OILTYPES]
    ) / 1000.0

    #         Gulf of Mexico
    # T14(74,IY,IS)=(OGADGPRD(70,OILTYPES,IY)+OGADGPRD(71,OILTYPES,IY)+OGADGPRD(72,OILTYPES,IY)+OGADGPRD(79,OILTYPES,IY)+OGADGPRD(80,OILTYPES,IY)+OGADGPRD(81,OILTYPES,IY))/1000.
    z[74] = (
        dfd["OGADGPRD"].loc[70].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[71].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[72].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[79].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[80].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[81].loc[OILTYPES]
    ) / 1000.0

    #      Alaska
    # T14(75,IY,IS)=(OGADGPRD(3,OILTYPES,IY)+OGADGPRD(75,OILTYPES,IY)+OGADGPRD(84,OILTYPES,IY))/1000.
    z[75] = (
        dfd["OGADGPRD"].loc[3].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[75].loc[OILTYPES]
        + dfd["OGADGPRD"].loc[84].loc[OILTYPES]
    ) / 1000.0


    #

    #   Crude Oil and Natural Gas Plant Liquid

    #   Production (million barrels per day)

    #      United States Total
    # T14(51,IY,IS)=T14(2,IY,IS)+T14(47,IY,IS)
    z[51] = z[2] + z[47]

    #         Lower 48 Onshore
    # T14(52,IY,IS)=T14(3,IY,IS)+T14(48,IY,IS)
    z[52] = z[3] + z[48]

    #         Lower 48 Offshore
    # T14(53,IY,IS)=T14(6,IY,IS)+T14(49,IY,IS)
    z[53] = z[6] + z[49]

    #         Alaska
    # T14(54,IY,IS)=T14(7,IY,IS)+T14(50,IY,IS)
    z[54] = z[7] + z[50]

    #

    #   Oil Shale World Oil Price
    # T14(37,IY,IS)=OS_WOP(IY)
    z[37] = dfd["OS_WOP"] * SCALPR2

    #   Average Natural Gas "Wellhead" Price
    # T14(9,IY,IS)=OGWPRNG(MNUMOR,IY)
    z[9] = dfd["OGWPRNG"].loc[MNUMOR] * SCALPR2

    #

    #   Cumulative Production

    #     Crude Oil (billion barrels)
    # T14(23,IY,IS)=T14(23,IY-1,IS)+T14(2,IY,IS)*365./1000.
    years = table_spec["years"]
    year_cum_oilgas_prod_begin = int(
        table_spec["settings"]["year_cum_oilgas_prod_begin"]
    )
    year_index = years.index(year_cum_oilgas_prod_begin)
    df2 = z[2].copy().to_frame().T if isinstance(z[2], pd.Series) else z[2].copy()
    df2.iloc[:, :year_index] = 0
    z[23] = df2.cumsum(axis=1) * 365.0 / 1000.0

    #     Natural Gas (trillion cubic feet)

    #       Lower 48 Dry Gas
    # T14(24,IY,IS)=T14(24,IY-1,IS)+T14(11,IY,IS)+T14(16,IY,IS)
    df11 = z[11].copy().to_frame().T if isinstance(z[11], pd.Series) else z[11].copy()
    df11.iloc[:, :year_index] = 0
    df16 = z[16].copy().to_frame().T if isinstance(z[16], pd.Series) else z[16].copy()
    df16.iloc[:, :year_index] = 0
    z[24] = pd.concat([df11.cumsum(axis=1), df16.cumsum(axis=1)], keys=None).sum()

    #       Lower 48 Non-Associated Gas
    # T14(25,IY,IS)=T14(25,IY-1,IS)+T14(13,IY,IS)+T14(18,IY,IS)
    df13 = z[13].copy().to_frame().T if isinstance(z[13], pd.Series) else z[13].copy()
    df13.iloc[:, :year_index] = 0
    df18 = z[18].copy().to_frame().T if isinstance(z[18], pd.Series) else z[18].copy()
    df18.iloc[:, :year_index] = 0
    z[25] = pd.concat([df13.cumsum(axis=1), df18.cumsum(axis=1)], keys=None).sum()

    #       Coalbed Methane and Shale Gas
    # T14(42,IY,IS)=T14(42,IY-1,IS)+T14(26,IY,IS)+T14(27,IY,IS)
    df26 = z[26].copy().to_frame().T if isinstance(z[26], pd.Series) else z[26].copy()
    df26.iloc[:, :year_index] = 0
    df27 = z[27].copy().to_frame().T if isinstance(z[27], pd.Series) else z[27].copy()
    df27.iloc[:, :year_index] = 0
    z[42] = pd.concat([df26.cumsum(axis=1), df27.cumsum(axis=1)], keys=None).sum()

    #       Coalbed Methane, Shale Gas, and Tight Gas
    # T14(43,IY,IS)=T14(43,IY-1,IS)+T14(26,IY,IS)+T14(27,IY,IS)+T14(15,IY,IS)
    # z[43] = z[43].(lag) +z[26]+z[27]+z[15]
    df26 = z[26].copy().to_frame().T if isinstance(z[26], pd.Series) else z[26].copy()
    df26.iloc[:, :year_index] = 0
    df27 = z[27].copy().to_frame().T if isinstance(z[27], pd.Series) else z[27].copy()
    df27.iloc[:, :year_index] = 0
    df15 = z[15].copy().to_frame().T if isinstance(z[15], pd.Series) else z[15].copy()
    df15.iloc[:, :year_index] = 0
    z[43] = pd.concat(
        [df26.cumsum(axis=1), df27.cumsum(axis=1), df15.cumsum(axis=1)], keys=None
    ).sum()

    #

    #   Crude Oil Wellhead Price

    #     (nominal dollars per barrel)

    #      Lower 48 Crude Oil Average Wellhead Price 1/
    # T14(36,IY,IS)=T14(1,IY,IS)/SCALPR*MC_JPGDP(IY)
    # z[36] = z[1]/ dfd['SCALPR'] * dfd['MC_JPGDP']
    z[36] = z[1] / SCALPR2 * MC_JPGDP.values

    #   Coalbed Methane and Shale Gas Production
    # T14(40,IY,IS)=(OGQNGREP(1,IY)+OGQNGREP(2,IY))/1000.
    z[40] = (dfd["OGQNGREP"].loc[1] + dfd["OGQNGREP"].loc[2]) / 1000.0

    #   Coalbed Methane, Shale Gas, Tight Gas Production
    # T14(41,IY,IS)=(OGQNGREP(3,IY)+OGQNGREP(1,IY)+OGQNGREP(2,IY))/1000.
    z[41] = (
        dfd["OGQNGREP"].loc[3] + dfd["OGQNGREP"].loc[1] + dfd["OGQNGREP"].loc[2]
    ) / 1000.0

    #

    #   Realized Non-associated Natural Gas Production
    # T14(38,IY,IS)=(SUM(OGRNAGPRD(1:OGDIST,GASTYPES,IY))-OGRNAGPRD(3,GASTYPES,IY)-OGRNAGPRD(75,GASTYPES,IY)-OGRNAGPRD(84,GASTYPES,IY))/1000.
    z[38] = (
        dfd["OGRNAGPRD"].loc[1:OGDIST, GASTYPES, :].sum()
        - dfd["OGRNAGPRD"].loc[3].loc[GASTYPES]
        - dfd["OGRNAGPRD"].loc[75].loc[GASTYPES]
        - dfd["OGRNAGPRD"].loc[84].loc[GASTYPES]
    ) / 1000.0

    #   Expected Non-associated Natural Gas Production
    # T14(39,IY,IS)=(SUM(OGENAGPRD(1:OGDIST,GASTYPES,IY))-OGENAGPRD(3,GASTYPES,IY)-OGENAGPRD(75,GASTYPES,IY)-OGENAGPRD(84,GASTYPES,IY))/1000.
    z[39] = (
        dfd["OGENAGPRD"].loc[1:OGDIST, (GASTYPES), :].sum()
        - dfd["OGENAGPRD"].loc[3].loc[GASTYPES]
        - dfd["OGENAGPRD"].loc[75].loc[GASTYPES]
        - dfd["OGENAGPRD"].loc[84].loc[GASTYPES]
    ) / 1000.0



    # Alternatively we could use this more compact code?
    # columns = ["OGCOPRD_FED", "OGCOPRD_NONFED", "OGNGPRD_FED", "OGNGPRD_NONFED"]
    # index = 76
    #
    # for col in columns:
    #     for loc in range(1, 15):
    #         z[index] = dfd[col].loc[loc]
    #         index += 1

    return z
