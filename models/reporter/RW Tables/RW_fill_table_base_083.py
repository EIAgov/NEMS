# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM

SZO fixed all issues on 8/23/2024.

"""


def fill_table_base_083(dfd, table_spec, table_id):
    """Fill table  Components of Selected Petroleum Product Prices

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

    MNUMOR = dfd["MNUMOR_rwpre"]
    NNGEM = dfd["NNGEM_rwpre"]

    #   Trace Report for Gas Price to the Electric Power Sector
    #   (#### dollars per thousand cubic feet)
    #    Price and Region
    #
    #   Import Price
    # T83(1,IY,IS)=NGIMPPRC(4,IY)
    z[1] = dfd["NGIMPPRC"].loc[4] * SCALPR2

    #   Wellhead Price
    # T83(2,IY,IS)=OGWPRNG(MNUMOR,IY)
    z[2] = dfd["OGWPRNG"].loc[MNUMOR] * SCALPR2

    #   Supply Price
    # T83(3,IY,IS)=(NGIMPPRC(4,IY)*NGIMPVOL(4,IY)+OGWPRNG(MNUMOR,IY)*(OGQNGREP(1,IY)+OGQNGREP(2,IY)+OGQNGREP(3,IY)+OGQNGREP(4,IY)+OGQNGREP(5,IY)+OGQNGREP(6,IY)+OGQNGREP(7,IY)+OGQNGREP(8,IY)+OGSHALENG(IY)))/FFF
    z[3] = (
        (
            dfd["NGIMPPRC"].loc[4] * dfd["NGIMPVOL"].loc[4]
            + dfd["OGWPRNG"].loc[MNUMOR]
            * (dfd["OGQNGREP"].loc[1:8].sum() + dfd["OGSHALENG"])
        )
        / (dfd["NGIMPVOL"].loc[4] + dfd["OGQNGREP"].loc[1:8].sum() + dfd["OGSHALENG"])
    ) * SCALPR2

    #   Regional Spot Prices

    #     New England
    # T83(4:19,IY,IS)=NGSPOT_EMM(1:16,IY)
    z[4] = dfd["NGSPOT_EMM"].loc[1] * SCALPR2

    #     Middle Atlantic
    # T83(4:19,IY,IS)=NGSPOT_EMM(1:16,IY)
    z[5] = dfd["NGSPOT_EMM"].loc[2] * SCALPR2

    #     Ohio
    # T83(4:19,IY,IS)=NGSPOT_EMM(1:16,IY)
    z[6] = dfd["NGSPOT_EMM"].loc[3] * SCALPR2

    #     Indiana, Illinois, and Michigan
    # T83(4:19,IY,IS)=NGSPOT_EMM(1:16,IY)
    z[7] = dfd["NGSPOT_EMM"].loc[4] * SCALPR2

    #     North Central
    # T83(4:19,IY,IS)=NGSPOT_EMM(1:16,IY)
    z[8] = dfd["NGSPOT_EMM"].loc[5] * SCALPR2

    #     Kansas and Missouri
    # T83(4:19,IY,IS)=NGSPOT_EMM(1:16,IY)
    z[9] = dfd["NGSPOT_EMM"].loc[6] * SCALPR2

    #     Delaware, Maryland, Virginia, West Virginia
    # T83(4:19,IY,IS)=NGSPOT_EMM(1:16,IY)
    z[10] = dfd["NGSPOT_EMM"].loc[7] * SCALPR2

    #     North Carolina and South Carolina
    # T83(4:19,IY,IS)=NGSPOT_EMM(1:16,IY)
    z[11] = dfd["NGSPOT_EMM"].loc[8] * SCALPR2

    #     Alabama, Georgia, and Mississippi
    # T83(4:19,IY,IS)=NGSPOT_EMM(1:16,IY)
    z[12] = dfd["NGSPOT_EMM"].loc[9] * SCALPR2

    #     Florida
    # T83(4:19,IY,IS)=NGSPOT_EMM(1:16,IY)
    z[13] = dfd["NGSPOT_EMM"].loc[10] * SCALPR2

    #     Kentucky and Tennessee
    # T83(4:19,IY,IS)=NGSPOT_EMM(1:16,IY)
    z[14] = dfd["NGSPOT_EMM"].loc[11] * SCALPR2

    #     West South Central
    # T83(4:19,IY,IS)=NGSPOT_EMM(1:16,IY)
    z[15] = dfd["NGSPOT_EMM"].loc[12] * SCALPR2

    #     Rocky Mountain and Nevada
    # T83(4:19,IY,IS)=NGSPOT_EMM(1:16,IY)
    z[16] = dfd["NGSPOT_EMM"].loc[13] * SCALPR2

    #     Arizona & New Mexico
    # T83(4:19,IY,IS)=NGSPOT_EMM(1:16,IY)
    z[17] = dfd["NGSPOT_EMM"].loc[14] * SCALPR2

    #     Oregon & Washington
    # T83(4:19,IY,IS)=NGSPOT_EMM(1:16,IY)
    z[18] = dfd["NGSPOT_EMM"].loc[15] * SCALPR2

    #     California & Hawaii
    # T83(4:19,IY,IS)=NGSPOT_EMM(1:16,IY)
    z[19] = dfd["NGSPOT_EMM"].loc[16] * SCALPR2

    #       Average (weighted by electric power use)
    z[100] = z[19] * 0.0
    # T83(100,IY,IS)=FFF/SUM(SQNGELGR(1:NNGEM-1,IY,1:3))
    # DO III=1,NNGEM-1
    # FFF = FFF + NGSPOT_EMM(III,IY) * sum(SQNGELGR(III,IY,1:3))
    for III in range(1, NNGEM):
        z[100] += dfd["NGSPOT_EMM"].loc[III] * dfd["SQNGELGR"].loc[III, 1:3, :].sum()
    z[100] = (
        z[100]
        / (
            dfd["SQNGELGR"].loc[1 : NNGEM - 1, 1, :].sum()
            + dfd["SQNGELGR"].loc[1 : NNGEM - 1, 2, :].sum()
            + dfd["SQNGELGR"].loc[1 : NNGEM - 1, 3, :].sum()
        )
    ).fillna(0) * SCALPR2

    #   Regional Spot Price minus Supply Price

    #     New England

    # IF (NGSPOT_EMM(III,IY) .NE. 0.00) &
    # T83(19+III,IY,IS) = NGSPOT_EMM(III,IY) -  T83(3,IY,IS)      ! spot markup

    # T83(19+III,IY,IS) = NGSPOT_EMM(III,IY) -  T83(3,IY,IS)      ! spot markup

    # z[20] = (dfd['NGSPOT_EMM'].loc[1] * SCALPR2 - z[3]).where(dfd['NGSPOT_EMM'].loc[1] > 0., dfd['NGSPOT_EMM'].loc[1] * SCALPR2 - z[3])
    z[20] = dfd["NGSPOT_EMM"].loc[1] * SCALPR2
    z[20].loc[z[20] != 0] -= z[3]

    #     Middle Atlantic
    # T83(4:19,IY,IS)=NGSPOT_EMM(1:16,IY) - T83(3,IY,IS)
    # z[21] = (dfd['NGSPOT_EMM'].loc[2] * SCALPR2 - z[3]).where(dfd['NGSPOT_EMM'].loc[2]  > 0., dfd['NGSPOT_EMM'].loc[2] * SCALPR2 - z[3])
    # z[21] = z[21].where(z[21] > 0, 0)
    z[21] = dfd["NGSPOT_EMM"].loc[2] * SCALPR2
    z[21].loc[z[21] != 0] -= z[3]

    #     Ohio
    # T83(4:19,IY,IS)=NGSPOT_EMM(1:16,IY) - T83(3,IY,IS)
    # z[22] = (dfd['NGSPOT_EMM'].loc[3] * SCALPR2 - z[3]).where(dfd['NGSPOT_EMM'].loc[3]  > 0.,dfd['NGSPOT_EMM'].loc[3] * SCALPR2 - z[3])
    z[22] = dfd["NGSPOT_EMM"].loc[3] * SCALPR2
    z[22].loc[z[22] != 0] -= z[3]

    #     Indiana, Illinois, and Michigan
    # T83(4:19,IY,IS)=NGSPOT_EMM(1:16,IY) - T83(3,IY,IS)
    # z[23] = (dfd['NGSPOT_EMM'].loc[4] * SCALPR2 - z[3]).where(dfd['NGSPOT_EMM'].loc[4]  > 0., dfd['NGSPOT_EMM'].loc[4] * SCALPR2 - z[3])
    z[23] = dfd["NGSPOT_EMM"].loc[4] * SCALPR2
    z[23].loc[z[23] != 0] -= z[3]

    #     North Central
    # T83(4:19,IY,IS)=NGSPOT_EMM(1:16,IY) - T83(3,IY,IS)
    # z[24] = (dfd['NGSPOT_EMM'].loc[5] * SCALPR2 - z[3]).where(dfd['NGSPOT_EMM'].loc[5]  > 0., dfd['NGSPOT_EMM'].loc[5] * SCALPR2 - z[3])
    z[24] = dfd["NGSPOT_EMM"].loc[5] * SCALPR2
    z[24].loc[z[24] != 0] -= z[3]

    #     Kansas and Missouri
    # T83(4:19,IY,IS)=NGSPOT_EMM(1:16,IY) - T83(3,IY,IS)
    # z[25] = (dfd['NGSPOT_EMM'].loc[6] * SCALPR2 - z[3]).where(dfd['NGSPOT_EMM'].loc[6]  > 0.,dfd['NGSPOT_EMM'].loc[6] * SCALPR2 - z[3])
    z[25] = dfd["NGSPOT_EMM"].loc[6] * SCALPR2
    z[25].loc[z[25] != 0] -= z[3]

    #     Delaware, Maryland, Virginia, West Virginia
    # T83(4:19,IY,IS)=NGSPOT_EMM(1:16,IY) - T83(3,IY,IS)
    # z[26] = dfd['NGSPOT_EMM'].loc[7] * SCALPR2 - z[3]
    z[26] = dfd["NGSPOT_EMM"].loc[7] * SCALPR2
    z[26].loc[z[26] != 0] -= z[3]

    #     North Carolina and South Carolina
    # T83(4:19,IY,IS)=NGSPOT_EMM(1:16,IY) - T83(3,IY,IS)
    # z[27] =  dfd['NGSPOT_EMM'].loc[8] * SCALPR2 - z[3]
    z[27] = dfd["NGSPOT_EMM"].loc[8] * SCALPR2
    z[27].loc[z[27] != 0] -= z[3]

    #     Alabama, Georgia, and Mississippi
    # T83(4:19,IY,IS)=NGSPOT_EMM(1:16,IY) - T83(3,IY,IS)
    # z[28] = dfd['NGSPOT_EMM'].loc[9] * SCALPR2 - z[3]
    z[28] = dfd["NGSPOT_EMM"].loc[9] * SCALPR2
    z[28].loc[z[28] != 0] -= z[3]

    #     Florida
    # T83(4:19,IY,IS)=NGSPOT_EMM(1:16,IY) - T83(3,IY,IS)
    # z[29] = dfd['NGSPOT_EMM'].loc[10] * SCALPR2 - z[3]
    z[29] = dfd["NGSPOT_EMM"].loc[10] * SCALPR2
    z[29].loc[z[23] != 0] -= z[3]

    #     Kentucky and Tennessee
    # T83(4:19,IY,IS)=NGSPOT_EMM(1:16,IY) - T83(3,IY,IS)
    # z[30] = dfd['NGSPOT_EMM'].loc[11] * SCALPR2 - z[3]
    z[30] = dfd["NGSPOT_EMM"].loc[11] * SCALPR2
    z[30].loc[z[30] != 0] -= z[3]

    #     West South Central
    # T83(4:19,IY,IS)=NGSPOT_EMM(1:16,IY) - T83(3,IY,IS)
    # z[31] = dfd['NGSPOT_EMM'].loc[12] * SCALPR2 - z[3]
    z[31] = dfd["NGSPOT_EMM"].loc[12] * SCALPR2
    z[31].loc[z[31] != 0] -= z[3]

    #     Rocky Mountain and Nevada
    # T83(4:19,IY,IS)=NGSPOT_EMM(1:16,IY) - T83(3,IY,IS)
    # z[32] = dfd['NGSPOT_EMM'].loc[13] * SCALPR2 - z[3]

    z[32] = dfd["NGSPOT_EMM"].loc[13] * SCALPR2
    z[32].loc[z[32] != 0] -= z[3]

    #     Arizona & New Mexico
    # T83(4:19,IY,IS)=NGSPOT_EMM(1:16,IY) - T83(3,IY,IS)
    # z[33] = (dfd['NGSPOT_EMM'].loc[14] * SCALPR2 - z[3])
    z[33] = dfd["NGSPOT_EMM"].loc[14] * SCALPR2
    z[33].loc[z[33] != 0] -= z[3]

    #     Oregon & Washington
    # T83(4:19,IY,IS)=NGSPOT_EMM(1:16,IY) - T83(3,IY,IS)
    # z[34] = (dfd['NGSPOT_EMM'].loc[15] * SCALPR2 - z[3])

    z[34] = dfd["NGSPOT_EMM"].loc[15] * SCALPR2
    z[34].loc[z[34] != 0] -= z[3]

    #     California & Hawaii
    # T83(4:19,IY,IS)=NGSPOT_EMM(1:16,IY) - T83(3,IY,IS)
    # z[35] = (dfd['NGSPOT_EMM'].loc[16]* SCALPR2 - z[3])

    z[35] = dfd["NGSPOT_EMM"].loc[16] * SCALPR2
    z[35].loc[z[35] != 0] -= z[3]

    #       Average Spot minus Supply Price
    # #T83(101,IY,IS)=T83(100,IY,IS)-T83(3,IY,IS)
    # z[101] = z[100]-z[3]
    # #z[101] = z[101].where(z[101] > 0, 0)

    # #       Average Spot minus Wellhead Price
    # #T83(102,IY,IS)=T83(100,IY,IS)-T83(2,IY,IS)
    # z[102] = z[100]-z[2]
    # #z[102] = z[102].where(z[102] > 0, 0)

    # !  average spot markups
    #        IF (T83(100,IY,IS) .NE. 0.00) THEN
    #           T83(101,IY,IS) = T83(100,IY,IS) - T83(3,IY,IS)
    #           T83(102,IY,IS) = T83(100,IY,IS) - T83(2,IY,IS)
    #        ENDIF
    z[101] = z[100].copy()
    z[101].loc[z[101] != 0] = z[100] - z[3]

    z[102] = z[100].copy()
    z[102].loc[z[102] != 0] = z[100] - z[2]
    #

    #   Distributor Tariff, Electric Power Sector

    #     New England
    # T83(36:51,IY,IS)=UDTAR(1:16,IY)
    z[36] = dfd["UDTAR"].loc[1] * SCALPR2

    #     Middle Atlantic
    # T83(36:51,IY,IS)=UDTAR(1:16,IY)
    z[37] = dfd["UDTAR"].loc[2] * SCALPR2

    #     Ohio
    # T83(36:51,IY,IS)=UDTAR(1:16,IY)
    z[38] = dfd["UDTAR"].loc[3] * SCALPR2

    #     Indiana, Illinois, and Michigan
    # T83(36:51,IY,IS)=UDTAR(1:16,IY)
    z[39] = dfd["UDTAR"].loc[4] * SCALPR2

    #     North Central
    # T83(36:51,IY,IS)=UDTAR(1:16,IY)
    z[40] = dfd["UDTAR"].loc[5] * SCALPR2

    #     Kansas and Missouri
    # T83(36:51,IY,IS)=UDTAR(1:16,IY)
    z[41] = dfd["UDTAR"].loc[6] * SCALPR2

    #     Delaware, Maryland, Virginia, West Virginia
    # T83(36:51,IY,IS)=UDTAR(1:16,IY)
    z[42] = dfd["UDTAR"].loc[7] * SCALPR2

    #     North Carolina and South Carolina
    # T83(36:51,IY,IS)=UDTAR(1:16,IY)
    z[43] = dfd["UDTAR"].loc[8] * SCALPR2

    #     Alabama, Georgia, and Mississippi
    # T83(36:51,IY,IS)=UDTAR(1:16,IY)
    z[44] = dfd["UDTAR"].loc[9] * SCALPR2

    #     Florida
    # T83(36:51,IY,IS)=UDTAR(1:16,IY)
    z[45] = dfd["UDTAR"].loc[10] * SCALPR2

    #     Kentucky and Tennessee
    # T83(36:51,IY,IS)=UDTAR(1:16,IY)
    z[46] = dfd["UDTAR"].loc[11] * SCALPR2

    #     West South Central
    # T83(36:51,IY,IS)=UDTAR(1:16,IY)
    z[47] = dfd["UDTAR"].loc[12] * SCALPR2

    #     Rocky Mountain and Nevada
    # T83(36:51,IY,IS)=UDTAR(1:16,IY)
    z[48] = dfd["UDTAR"].loc[13] * SCALPR2

    #     Arizona & New Mexico
    # T83(36:51,IY,IS)=UDTAR(1:16,IY)
    z[49] = dfd["UDTAR"].loc[14] * SCALPR2

    #     Oregon & Washington
    # T83(36:51,IY,IS)=UDTAR(1:16,IY)
    z[50] = dfd["UDTAR"].loc[15] * SCALPR2

    #     California & Hawaii
    # T83(36:51,IY,IS)=UDTAR(1:16,IY)
    z[51] = dfd["UDTAR"].loc[16] * SCALPR2

    #       Average (weighted by consumption)
    # T83(103,IY,IS)=FFF/SUM(SQNGELGR(1:NNGEM-1,IY,1:3))
    #   DO III=1,NNGEM-1
    # FFF = FFF + T83(III+35,IY,IS) * sum(SQNGELGR(III,IY,1:3))
    # ENDDO

    # z[103]=z[19]*0.0
    # for III in range(1,NNGEM):
    z[103] = (
        z[36] * dfd["SQNGELGR"].loc[1, 1:3, :].sum()
        + z[37] * dfd["SQNGELGR"].loc[2, 1:3, :].sum()
        + z[38] * dfd["SQNGELGR"].loc[3, 1:3, :].sum()
        + z[39] * dfd["SQNGELGR"].loc[4, 1:3, :].sum()
        + z[40] * dfd["SQNGELGR"].loc[5, 1:3, :].sum()
        + z[41] * dfd["SQNGELGR"].loc[6, 1:3, :].sum()
        + z[42] * dfd["SQNGELGR"].loc[7, 1:3, :].sum()
        + z[43] * dfd["SQNGELGR"].loc[8, 1:3, :].sum()
        + z[44] * dfd["SQNGELGR"].loc[9, 1:3, :].sum()
        + z[45] * dfd["SQNGELGR"].loc[10, 1:3, :].sum()
        + z[46] * dfd["SQNGELGR"].loc[11, 1:3, :].sum()
        + z[47] * dfd["SQNGELGR"].loc[12, 1:3, :].sum()
        + z[48] * dfd["SQNGELGR"].loc[13, 1:3, :].sum()
        + z[49] * dfd["SQNGELGR"].loc[14, 1:3, :].sum()
        + z[50] * dfd["SQNGELGR"].loc[15, 1:3, :].sum()
        + z[51] * dfd["SQNGELGR"].loc[16, 1:3, :].sum()
    )

    z[103] = (
        z[103]
        / (
            dfd["SQNGELGR"].loc[1 : NNGEM - 1, 1, :].sum()
            + dfd["SQNGELGR"].loc[1 : NNGEM - 1, 2, :].sum()
            + dfd["SQNGELGR"].loc[1 : NNGEM - 1, 3, :].sum()
        )
    ).fillna(0)

    #

    #   Consumption, Electric Power Sector

    #   (billion cubic feet)

    #     New England
    # T83(52,IY,IS)=SUM(SQNGELGR(1,IY,1:3))/CFNGU(IY)
    z[52] = dfd["SQNGELGR"].loc[1].loc[1:3].sum() / dfd["CFNGU"]

    #     Middle Atlantic
    # T83(53,IY,IS)=SUM(SQNGELGR(2,IY,1:3))/CFNGU(IY)
    z[53] = dfd["SQNGELGR"].loc[2].loc[1:3].sum() / dfd["CFNGU"]

    #     Ohio
    # T83(54,IY,IS)=SUM(SQNGELGR(3,IY,1:3))/CFNGU(IY)
    z[54] = dfd["SQNGELGR"].loc[3].loc[1:3].sum() / dfd["CFNGU"]

    #     Indiana, Illinois, and Michigan
    # T83(55,IY,IS)=SUM(SQNGELGR(4,IY,1:3))/CFNGU(IY)
    z[55] = dfd["SQNGELGR"].loc[4].loc[1:3].sum() / dfd["CFNGU"]

    #     North Central
    # T83(56,IY,IS)=SUM(SQNGELGR(5,IY,1:3))/CFNGU(IY)
    z[56] = dfd["SQNGELGR"].loc[5].loc[1:3].sum() / dfd["CFNGU"]

    #     Kansas and Missouri
    # T83(57,IY,IS)=SUM(SQNGELGR(6,IY,1:3))/CFNGU(IY)
    z[57] = dfd["SQNGELGR"].loc[6].loc[1:3].sum() / dfd["CFNGU"]
    # T83(58,IY,IS) = sum(SQNGELGR( 7,IY,1:3)) / CFNGU(IY)           ! electric power quantities
    # T83(59,IY,IS) = sum(SQNGELGR( 8,IY,1:3)) / CFNGU(IY)           ! electric power quantities
    # T83(60,IY,IS) = sum(SQNGELGR( 9,IY,1:3)) / CFNGU(IY)           ! electric power quantities
    # T83(61,IY,IS) = sum(SQNGELGR(10,IY,1:3)) / CFNGU(IY)           ! electric power quantities
    # T83(62,IY,IS) = sum(SQNGELGR(11,IY,1:3)) / CFNGU(IY)           ! electric power quantities
    # T83(63,IY,IS) = sum(SQNGELGR(12,IY,1:3)) / CFNGU(IY)           ! electric power quantities
    # T83(64,IY,IS) = sum(SQNGELGR(13,IY,1:3)) / CFNGU(IY)           ! electric power quantities
    # T83(65,IY,IS) = sum(SQNGELGR(14,IY,1:3)) / CFNGU(IY)           ! electric power quantities
    # T83(66,IY,IS) = sum(SQNGELGR(15,IY,1:3)) / CFNGU(IY)           ! electric power quantities
    # T83(67,IY,IS) = sum(SQNGELGR(16,IY,1:3)) / CFNGU(IY)           ! electric power quantities
    # T83(68:83,IY,IS) = PNGELGR(1:16,IY) * CFNGU(IY)                ! electric power prices
    # T83(84:99,IY,IS) = PNGELGR(1:16,IY) * CFNGU(IY) - T83(3,IY,IS) ! electric power markup
    z[58] = dfd["SQNGELGR"].loc[7].loc[1:3].sum() / dfd["CFNGU"]
    z[59] = dfd["SQNGELGR"].loc[8].loc[1:3].sum() / dfd["CFNGU"]
    z[60] = dfd["SQNGELGR"].loc[9].loc[1:3].sum() / dfd["CFNGU"]
    z[61] = dfd["SQNGELGR"].loc[10].loc[1:3].sum() / dfd["CFNGU"]
    z[62] = dfd["SQNGELGR"].loc[11].loc[1:3].sum() / dfd["CFNGU"]
    z[63] = dfd["SQNGELGR"].loc[12].loc[1:3].sum() / dfd["CFNGU"]
    z[64] = dfd["SQNGELGR"].loc[13].loc[1:3].sum() / dfd["CFNGU"]
    z[65] = dfd["SQNGELGR"].loc[14].loc[1:3].sum() / dfd["CFNGU"]
    z[66] = dfd["SQNGELGR"].loc[15].loc[1:3].sum() / dfd["CFNGU"]
    z[67] = dfd["SQNGELGR"].loc[16].loc[1:3].sum() / dfd["CFNGU"]

    z[68] = dfd["PNGELGR"].loc[1] * dfd["CFNGU"] * SCALPR2
    z[69] = dfd["PNGELGR"].loc[2] * dfd["CFNGU"] * SCALPR2
    z[70] = dfd["PNGELGR"].loc[3] * dfd["CFNGU"] * SCALPR2
    z[71] = dfd["PNGELGR"].loc[4] * dfd["CFNGU"] * SCALPR2
    z[72] = dfd["PNGELGR"].loc[5] * dfd["CFNGU"] * SCALPR2
    z[73] = dfd["PNGELGR"].loc[6] * dfd["CFNGU"] * SCALPR2
    z[74] = dfd["PNGELGR"].loc[7] * dfd["CFNGU"] * SCALPR2
    z[75] = dfd["PNGELGR"].loc[8] * dfd["CFNGU"] * SCALPR2
    z[76] = dfd["PNGELGR"].loc[9] * dfd["CFNGU"] * SCALPR2
    z[77] = dfd["PNGELGR"].loc[10] * dfd["CFNGU"] * SCALPR2
    z[78] = dfd["PNGELGR"].loc[11] * dfd["CFNGU"] * SCALPR2
    z[79] = dfd["PNGELGR"].loc[12] * dfd["CFNGU"] * SCALPR2
    z[80] = dfd["PNGELGR"].loc[13] * dfd["CFNGU"] * SCALPR2
    z[81] = dfd["PNGELGR"].loc[14] * dfd["CFNGU"] * SCALPR2
    z[82] = dfd["PNGELGR"].loc[15] * dfd["CFNGU"] * SCALPR2
    z[83] = dfd["PNGELGR"].loc[16] * dfd["CFNGU"] * SCALPR2
    z[84] = z[68] - z[3]
    z[85] = z[69] - z[3]
    z[86] = z[70] - z[3]
    z[87] = z[71] - z[3]
    z[88] = z[72] - z[3]
    z[89] = z[73] - z[3]
    z[90] = z[74] - z[3]
    z[91] = z[75] - z[3]
    z[92] = z[76] - z[3]
    z[93] = z[77] - z[3]
    z[94] = z[78] - z[3]
    z[95] = z[79] - z[3]
    z[96] = z[80] - z[3]
    z[97] = z[81] - z[3]
    z[98] = z[82] - z[3]
    z[99] = z[83] - z[3]

    z[104] = (
        z[52]
        + z[53]
        + z[54]
        + z[55]
        + z[56]
        + z[57]
        + z[58]
        + z[59]
        + z[60]
        + z[61]
        + z[62]
        + z[63]
        + z[64]
        + z[65]
        + z[66]
        + z[67]
    )
    # Price, Electric Power Sector average
    # FFF=0.0
    # DO III=1,NNGEM-1
    # FFF = FFF + SPNGELGR(III,IY,1) * SQNGELGR(III,IY,1) + &
    # SPNGELGR(III,IY,2) * SQNGELGR(III,IY,2) + &
    # SPNGELGR(III,IY,3) * SQNGELGR(III,IY,3)
    # ENDDO
    # IF ((T83(104,IY,IS) * CFNGU(IY)) .NE. 0.0) THEN
    # T83(105,IY,IS) = FFF / (T83(104,IY,IS) * CFNGU(IY))
    # ELSE
    # T83(105,IY,IS) = 0.0
    # ENDIF

    z[105] = 0.0
    for III in range(1, NNGEM):
        z[105] += (
            dfd["ANGTDM/SPNGELGR"].loc[III, 1] * dfd["SQNGELGR"].loc[III, 1]
            + dfd["ANGTDM/SPNGELGR"].loc[III, 2] * dfd["SQNGELGR"].loc[III, 2]
            + dfd["ANGTDM/SPNGELGR"].loc[III, 3] * dfd["SQNGELGR"].loc[III, 3]
        ) * SCALPR2

    z[105] = (z[105] / (z[104] * dfd["CFNGU"])).fillna(0)

    #          electric power markup average
    # IF (T83(105,IY,IS) .NE. 0.00) THEN
    # T83(107,IY,IS) = T83(105,IY,IS) - T83(3,IY,IS)
    # T83(108,IY,IS) = T83(105,IY,IS) - T83(2,IY,IS)
    # ENDIF
    # ENDDO

    # Markup over Supply Price, Electric Power Sector
    #     Average weighted by consumption
    z[107] = z[105].copy()
    z[107].loc[z[107] != 0] -= z[3]

    # Markup over Wellhead price, ELectric Power Sect
    #    Average weighted by consumption
    z[108] = z[105].copy()
    z[108].loc[z[108] > 0] -= z[2]

    # !        --- Avg Gas Prices to EL Gen weighted by 2015 elec cons
    # !  do in separate loop because pre-2015 averages need to be calculated using a number that wouldn't be calculated yet if done in the same loop
    #          DO IY = 1,LASTYR
    #            FFF=0.0
    #            DO III=1,NNGEM-1
    #              FFF = FFF + SPNGELGR(III,IY,1) * SQNGELGR(III,26,1) + &
    #                          SPNGELGR(III,IY,2) * SQNGELGR(III,26,2) + &
    #                          SPNGELGR(III,IY,3) * SQNGELGR(III,26,3)
    #            ENDDO
    #            IF ((T83(104,26,IS) * CFNGU(IY)) .NE. 0.0) THEN
    #                T83(106,IY,IS) = FFF /  (T83(104,26,IS) * CFNGU(IY))
    #            ELSE
    #                T83(106,IY,IS) = 0.0
    #            ENDIF
    #            T83(109,IY,IS) = T83(106,IY,IS) - T83(2,IY,IS)
    #          ENDDO
    # NNGEM_rwpre	17	int	Natural Gas Electricity regions
    # SPNGELGR         R    4 3 (NNGEMM,MNUMYR,     3)              PRICE  87$BTU NG     ELECTR Natural gas price to electric power sector, 3 seasons
    # SQNGELGR         R    4 3 (NNGEMM,MNUMYR,     3)              QUNTY  tBTU   NG     ELECTR Natural gas use in electric power sector, 3 seasons
    # CFNGU            R    4 1 (MNUMYR)                            CONFAC BTUCF  NG     Natural Gas - Utility consumption

    idx_2015 = 2015

    df = dfd["SQNGELGR"].loc[1:16].copy()

    # Replace all columns with column 2015
    df.loc[:, df.columns != idx_2015] = df[idx_2015].to_numpy()[:, None]

    # Calculate value in 2015 NG price
    FFF = (dfd["NGTDMOUT/SPNGELGR"].loc[1:16] * df).sum(axis=0)

    # z[104]: Consumption, Electric Power Sector
    z[106] = FFF / (z[104].loc[idx_2015] * dfd["CFNGU"]) * SCALPR2
    z[106].fillna(0)

    z[109] = z[106] - z[2].values

    return z
