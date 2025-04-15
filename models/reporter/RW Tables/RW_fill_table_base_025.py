# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM

SZO rewrote on 8/30/2024

"""


def fill_table_base_025(dfd, table_spec, table_id):
    """Fill table for  Carbon Dioxide Enhanced Oil Recovery Operations by Source

    The function returns a dict in which each key is an integer references a table row, and each
    value is a dataframe indexed by region number. The integer keys are the same as "IROWS" in the
    layin.

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
        Similar to CAPFLATE to adjusted biomass prices, the SUBROUTINE FINFLATE adjusts INFLATION
        FACTOR for Oil & Gas Module like:

            OGCO2PRCs = OGCO2PRCs * SCALPR / MC_JPGDP(2008-1989)
    """

    z = {}

    FYRPRC = int(table_spec["settings"]["year_index_real_dol_priceyr"])
    # Get inflation index from 1990->2050
    MC_JPGDP = dfd["MC_JPGDP"].loc[1990:]

    # Get inflation index of FYRPRC (2022)
    SCALPR2 = MC_JPGDP.loc[FYRPRC]  # 2022 = column 33

    RDAYS = dfd["RDAYS_rwpre"]
    TCF_to_MMT = dfd["TCF_to_MMT_rwpre"]

    # ftab.f ---
    # SUBROUTINE FINFLATE
    # ...
    #   OGCO2PRCs = OGCO2PRCs * SCALPR / MC_JPGDP(2008-1989)
    #
    OGCO2PRCS = dfd["OGCO2PRCS"] * SCALPR2 / MC_JPGDP.iloc[2008 - 1990]

    #   Carbon Dioxide Enhanced Oil Recovery Operations by Source
    #    Source
    #
    #   Available Carbon Dioxide
    #   (million metric tons)

    # ftab.f ---
    # ! available.  18000 converts from thousand cubic feet to million metric tons
    #     T25( 1:13,IY,IS) = OGCO2AVLs(8,1:13,IY) / 18000.
    #     T25(14,IY,IS) = sum(OGCO2AVLs(8,1:13,IY)) / 18000.
    #     T25(64,IY,IS) = T25(14,IY,IS) - T25( 4,IY,IS)

    #      Hydrogen
    z[1] = dfd["OGCO2AVLS"].loc[8].loc[1] / TCF_to_MMT

    #      Ammonia
    z[2] = dfd["OGCO2AVLS"].loc[8].loc[2] / TCF_to_MMT

    #      Ethanol
    z[3] = dfd["OGCO2AVLS"].loc[8].loc[3] / TCF_to_MMT

    #      Cement
    z[5] = dfd["OGCO2AVLS"].loc[8].loc[5] / TCF_to_MMT

    #      Refineries, Hydrogen
    z[6] = dfd["OGCO2AVLS"].loc[8].loc[6] / TCF_to_MMT

    #      Electric Power Plants
    z[7] = dfd["OGCO2AVLS"].loc[8].loc[7] / TCF_to_MMT

    #      Natural Gas Processing
    z[9] = dfd["OGCO2AVLS"].loc[8].loc[9] / TCF_to_MMT

    #      Coal-to-Liquids Plants
    z[10] = dfd["OGCO2AVLS"].loc[8].loc[10] / TCF_to_MMT

    #      New Source 1
    z[12] = dfd["OGCO2AVLS"].loc[8].loc[12] / TCF_to_MMT

    #      New Source 2
    z[13] = dfd["OGCO2AVLS"].loc[8].loc[13] / TCF_to_MMT

    #         Total Anthropogenic Available
    #      Natural

    z[4] = dfd["OGCO2AVLS"].loc[8].loc[4] / TCF_to_MMT

    #         Total Available
    # T25(14,IY,IS)=SUM(OGCO2AVLS(8,1:13,IY))/18000.
    z[14] = dfd["OGCO2AVLS"].loc[8].loc[1:13].sum() / TCF_to_MMT

    # T25(64,IY,IS)=T25(14,IY,IS)-T25(4,IY,IS)
    z[64] = z[14] - z[4]

    #   Purchased Carbon Dioxide

    #   (million metric tons)
    # ftab.f ---
    # ! purchased.  18000 converts from thousand cubic feet to million metric tons
    #         T25(15:27,IY,IS) = OGCO2PUR(8,1:13,IY) / 18000.
    #         T25(28,IY,IS) = sum(OGCO2PUR(8,1:13,IY)) / 18000.

    #      Hydrogen
    z[15] = dfd["OGCO2PUR"].loc[8].loc[1] / TCF_to_MMT

    #      Ammonia
    z[16] = dfd["OGCO2PUR"].loc[8].loc[2] / TCF_to_MMT

    #      Ethanol
    z[17] = dfd["OGCO2PUR"].loc[8].loc[3] / TCF_to_MMT

    #      Cement
    z[19] = dfd["OGCO2PUR"].loc[8].loc[5] / TCF_to_MMT

    #      Refineries, Hydrogen
    z[20] = dfd["OGCO2PUR"].loc[8].loc[6] / TCF_to_MMT

    #      Electric Power Plants
    z[21] = dfd["OGCO2PUR"].loc[8].loc[7] / TCF_to_MMT

    #      Natural Gas Processing
    z[23] = dfd["OGCO2PUR"].loc[8].loc[9] / TCF_to_MMT

    #      Coal-to-Liquids Plants
    z[24] = dfd["OGCO2PUR"].loc[8].loc[10] / TCF_to_MMT

    #      New Source 1
    z[26] = dfd["OGCO2PUR"].loc[8].loc[12] / TCF_to_MMT

    #      New Source 2
    z[27] = dfd["OGCO2PUR"].loc[8].loc[13] / TCF_to_MMT

    #      Total Purchased by All Industrial Sources
    # T25(90,IY,IS) = T25(15,IY,IS) + T25(16,IY,IS) + T25(17,IY,IS) + T25(19,IY,IS) + T25(20,IY,IS) + T25(23,IY,IS) + T25(26,IY,IS) + T25(27,IY,IS)
    z[90] = (z[15] + z[16] + z[17] + z[19] + z[20] + z[23] + z[26] + z[27]).replace(
        0, 1
    )

    #      Natural
    z[18] = dfd["OGCO2PUR"].loc[8].loc[4] / TCF_to_MMT

    #         Total Purchased
    # T25(28,IY,IS)=SUM(OGCO2PUR(8,1:13,IY))/18000.
    z[28] = dfd["OGCO2PUR"].loc[8].loc[1:13].sum() / TCF_to_MMT

    #         Total Anthropogenic Purchased
    # T25(67,IY,IS)=T25(28,IY,IS)-T25(18,IY,IS)
    z[67] = z[28] - z[18]

    #   Recycled Carbon Dioxide
    #   (million metric tons)
    # T25(93,IY,IS) = (OGCO2REC(8,1,IY) + OGCO2REC(8,2,IY) + OGCO2REC(8,3,IY) + OGCO2REC(8,5,IY) + OGCO2REC(8,6,IY) + OGCO2REC(8,9,IY) + OGCO2REC(8,12,IY) + OGCO2REC(8,13,IY)) / 18000.
    z[93] = (
        dfd["OGCO2REC"].loc[8].loc[1]
        + dfd["OGCO2REC"].loc[8].loc[2]
        + dfd["OGCO2REC"].loc[8].loc[3]
        + dfd["OGCO2REC"].loc[8].loc[5]
        + dfd["OGCO2REC"].loc[8].loc[6]
        + dfd["OGCO2REC"].loc[8].loc[9]
        + dfd["OGCO2REC"].loc[8].loc[12]
        + dfd["OGCO2REC"].loc[8].loc[13]
    ) / TCF_to_MMT

    #      Hydrogen  need to check for divide by zero
    # T25(36,IY,IS)=T25(93,IY,IS)*(T25(15,IY,IS)/T25(90,IY,IS))
    z[36] = z[93] * (z[15] / z[90])

    #      Ammonia
    # T25(37,IY,IS)=T25(93,IY,IS)*(T25(16,IY,IS)/T25(90,IY,IS))
    z[37] = z[93] * (z[16] / z[90])

    #      Ethanol
    # T25(38,IY,IS)=T25(93,IY,IS)*(T25(17,IY,IS)/T25(90,IY,IS))
    z[38] = z[93] * (z[17] / z[90])

    #      Cement
    # T25(40,IY,IS)=T25(93,IY,IS)*(T25(19,IY,IS)/T25(90,IY,IS))
    z[40] = z[93] * (z[19] / z[90])

    #      Refineries, Hydrogen
    # T25(41,IY,IS)=T25(93,IY,IS)*(T25(20,IY,IS)/T25(90,IY,IS))
    z[41] = z[93] * (z[20] / z[90])

    #      Electric Power Plants
    # T25(42,IY,IS)=OGCO2REC(8,7,IY)/18000.
    z[42] = dfd["OGCO2REC"].loc[8].loc[7] / TCF_to_MMT

    #      Natural Gas Processing
    # T25(44,IY,IS)=T25(93,IY,IS)*(T25(23,IY,IS)/T25(90,IY,IS))
    z[44] = z[93] * (z[23] / z[90])

    #      Coal-to-Liquids Plants
    # T25(45,IY,IS)=OGCO2REC(8,10,IY)/18000.
    z[45] = dfd["OGCO2REC"].loc[8].loc[10] / TCF_to_MMT

    #      New Source 1
    # T25(47,IY,IS)=T25(93,IY,IS)*(T25(26,IY,IS)/T25(90,IY,IS))
    z[47] = z[93] * (z[26] / z[90])

    #      New Source 2
    # T25(48,IY,IS)=T25(93,IY,IS)*(T25(27,IY,IS)/T25(90,IY,IS))
    z[48] = z[93] * (z[27] / z[90])

    #      Natural
    # T25(39,IY,IS)=OGCO2REC(8,4,IY)/18000.
    z[39] = dfd["OGCO2REC"].loc[8].loc[4] / TCF_to_MMT

    #         Total Recycled
    # T25(49,IY,IS)=SUM(OGCO2REC(8,1:13,IY))/18000.
    z[49] = dfd["OGCO2REC"].loc[8].loc[1:13].sum() / TCF_to_MMT

    #         Total Anthropogenic Recycled
    # T25(65,IY,IS)=T25(49,IY,IS)-T25(39,IY,IS)
    z[65] = z[49] - z[39]

    #   Carbon Dioxide Used

    #   (million metric tons)
    # T25(94,IY,IS) = (OGCO2INJ(8,1,IY) + OGCO2INJ(8,2,IY) + OGCO2INJ(8,3,IY) + OGCO2INJ(8,5,IY) + OGCO2INJ(8,6,IY) + OGCO2INJ(8,9,IY) + OGCO2INJ(8,12,IY) + OGCO2INJ(8,13,IY)) / 18000.
    z[94] = (
        dfd["OGCO2INJ"].loc[8].loc[1]
        + dfd["OGCO2INJ"].loc[8].loc[2]
        + dfd["OGCO2INJ"].loc[8].loc[3]
        + dfd["OGCO2INJ"].loc[8].loc[5]
        + dfd["OGCO2INJ"].loc[8].loc[6]
        + dfd["OGCO2INJ"].loc[8].loc[9]
        + dfd["OGCO2INJ"].loc[8].loc[12]
        + dfd["OGCO2INJ"].loc[8].loc[13]
    ) / TCF_to_MMT

    #      Hydrogen
    # T25(50,IY,IS)=T25(94,IY,IS)*(T25(15,IY,IS)/T25(90,IY,IS))
    z[50] = z[94] * (z[15] / z[90])

    #      Ammonia
    # T25(51,IY,IS)=T25(94,IY,IS)*(T25(16,IY,IS)/T25(90,IY,IS))
    z[51] = z[94] * (z[16] / z[90])

    #      Ethanol
    # T25(52,IY,IS)=T25(94,IY,IS)*(T25(17,IY,IS)/T25(90,IY,IS))
    z[52] = z[94] * (z[17] / z[90])

    #      Cement
    # T25(54,IY,IS)=T25(94,IY,IS)*(T25(19,IY,IS)/T25(90,IY,IS))
    z[54] = z[94] * (z[19] / z[90])

    #      Refineries, Hydrogen
    # T25(55,IY,IS)=T25(94,IY,IS)*(T25(20,IY,IS)/T25(90,IY,IS))
    z[55] = z[94] * (z[20] / z[90])

    #      Electric Power Plants
    # T25(56,IY,IS)=OGCO2INJ(8,7,IY)/18000.
    z[56] = dfd["OGCO2INJ"].loc[8].loc[7] / TCF_to_MMT

    #      Natural Gas Processing
    # T25(58,IY,IS)=T25(94,IY,IS)*(T25(23,IY,IS)/T25(90,IY,IS))
    z[58] = z[94] * (z[23] / z[90])

    #      Coal-to-Liquids Plants
    # T25(59,IY,IS)=OGCO2INJ(8,10,IY)/18000.
    z[59] = dfd["OGCO2INJ"].loc[8].loc[10] / TCF_to_MMT

    #      New Source 1
    # T25(61,IY,IS)=T25(94,IY,IS)*(T25(26,IY,IS)/T25(90,IY,IS))
    z[61] = z[94] * (z[26] / z[90])

    #      New Source 2
    # T25(62,IY,IS)=T25(94,IY,IS)*(T25(27,IY,IS)/T25(90,IY,IS))
    z[62] = z[94] * (z[27] / z[90])

    #      Natural
    # T25(53,IY,IS)=OGCO2INJ(8,4,IY)/18000.
    z[53] = dfd["OGCO2INJ"].loc[8].loc[4] / TCF_to_MMT

    #         Total Used
    # T25(63,IY,IS)=SUM(OGCO2INJ(8,1:13,IY))/18000.
    z[63] = dfd["OGCO2INJ"].loc[8].loc[1:13].sum() / TCF_to_MMT

    #         Total Anthropogenic Used
    # T25(66,IY,IS)=T25(63,IY,IS)-T25(53,IY,IS)
    z[66] = z[63] - z[53]

    #   Oil Produced

    #   (million barrels per day)

    #      East Coast
    # T25(29,IY,IS)=SUM(OGEORPRD(1,1:13,IY))/365000.
    z[29] = dfd["OGEORPRD"].loc[1].loc[1:13].sum() / RDAYS * 0.001

    #      Gulf Coast
    # T25(30,IY,IS)=SUM(OGEORPRD(2,1:13,IY))/365000.
    z[30] = dfd["OGEORPRD"].loc[2].loc[1:13].sum() / RDAYS * 0.001

    #      Midcontinent
    # T25(31,IY,IS)=SUM(OGEORPRD(3,1:13,IY))/365000.
    z[31] = dfd["OGEORPRD"].loc[3].loc[1:13].sum() / RDAYS * 0.001

    #      Southwest
    # T25(32,IY,IS)=SUM(OGEORPRD(4,1:13,IY))/365000.
    z[32] = dfd["OGEORPRD"].loc[4].loc[1:13].sum() / RDAYS * 0.001

    #      Rocky Mountains
    # T25(33,IY,IS)=SUM(OGEORPRD(5,1:13,IY))/365000.+SUM(OGEORPRD(7,1:13,IY))/365000.
    z[33] = (
        dfd["OGEORPRD"].loc[5].loc[1:13].sum() / RDAYS * 0.001
        + dfd["OGEORPRD"].loc[7].loc[1:13].sum() / RDAYS * 0.001
    )

    #      West Coast
    # T25(34,IY,IS)=SUM(OGEORPRD(6,1:13,IY))/365000.
    z[34] = dfd["OGEORPRD"].loc[6].loc[1:13].sum() / RDAYS * 0.001

    #         Total Oil Production
    # T25(35,IY,IS)=SUM(OGEORPRD(8,1:13,IY))/365000.
    z[35] = dfd["OGEORPRD"].loc[8].loc[1:13].sum() / RDAYS * 0.001

    #   Oil Produced (by source)  Note: check for divide by zero

    #   (million barrels per day)
    # T25(95,IY,IS) = (OGEORPRD(8,1,IY) + OGEORPRD(8,2,IY) + OGEORPRD(8,3,IY) + OGEORPRD(8,5,IY) + OGEORPRD(8,6,IY) + OGEORPRD(8,9,IY) + OGEORPRD(8,12,IY) + OGEORPRD(8,13,IY)) / 365000.
    z[95] = (
        (
            dfd["OGEORPRD"].loc[8].loc[1]
            + dfd["OGEORPRD"].loc[8].loc[2]
            + dfd["OGEORPRD"].loc[8].loc[3]
            + dfd["OGEORPRD"].loc[8].loc[5]
            + dfd["OGEORPRD"].loc[8].loc[6]
            + dfd["OGEORPRD"].loc[8].loc[9]
            + dfd["OGEORPRD"].loc[8].loc[12]
            + dfd["OGEORPRD"].loc[8].loc[13]
        )
        / RDAYS
        * 0.001
    )

    #      Hydrogen
    # T25(68,IY,IS)=T25(95,IY,IS)*(T25(15,IY,IS)/T25(90,IY,IS))
    z[68] = z[95] * (z[15] / z[90])

    #      Ammonia
    # T25(69,IY,IS)=T25(95,IY,IS)*(T25(16,IY,IS)/T25(90,IY,IS))
    z[69] = z[95] * (z[16] / z[90])

    #      Ethanol
    # T25(70,IY,IS)=T25(95,IY,IS)*(T25(17,IY,IS)/T25(90,IY,IS))
    z[70] = z[95] * (z[17] / z[90])

    #      Cement
    # T25(72,IY,IS)=T25(95,IY,IS)*(T25(19,IY,IS)/T25(90,IY,IS))
    z[72] = z[95] * (z[19] / z[90])

    #      Refineries, Hydrogen
    # T25(73,IY,IS)=T25(95,IY,IS)*(T25(20,IY,IS)/T25(90,IY,IS))
    z[73] = z[95] * (z[20] / z[90])

    #      Electric Power Plants
    # T25(74,IY,IS)=OGEORPRD(8,7,IY)/365000.
    z[74] = dfd["OGEORPRD"].loc[8, 7] / RDAYS * 0.001

    #      Natural Gas Processing
    # T25(76,IY,IS)=T25(95,IY,IS)*(T25(23,IY,IS)/T25(90,IY,IS))
    z[76] = z[95] * (z[23] / z[90])

    #      Coal-to-Liquids Plants
    # T25(77,IY,IS)=OGEORPRD(8,10,IY)/365000.
    z[77] = dfd["OGEORPRD"].loc[8, 10] / RDAYS * 0.001

    #      New Source 1
    # T25(79,IY,IS)=T25(95,IY,IS)*(T25(26,IY,IS)/T25(90,IY,IS))
    z[79] = z[95] * (z[26] / z[90])

    #      New Source 2
    # T25(80,IY,IS)=T25(95,IY,IS)*(T25(27,IY,IS)/T25(90,IY,IS))
    z[80] = z[95] * (z[27] / z[90])

    #      Natural
    # T25(71,IY,IS)=OGEORPRD(8,4,IY)/365000.
    z[71] = dfd["OGEORPRD"].loc[8, 4] / RDAYS * 0.001

    #         Total Oil Production
    # T25(81,IY,IS)=SUM(OGEORPRD(8,1:13,IY))/365000.
    z[81] = dfd["OGEORPRD"].loc[8].loc[1:13].sum() / RDAYS * 0.001

    #         Total Anthropogenic
    # T25(82,IY,IS)=T25(81,IY,IS)-T25(71,IY,IS)
    z[82] = z[81] - z[71]

    #   Carbon Dioxide Price by Region

    #   (#### dollars per thousand cubic feet)

    #      weighted by carbon dioxide available

    #         Hydrogen
    # ftab.f ---
    # IF (sum(OGCO2AVLs(1:7, 1,IY)) .NE. 0.0) &
    #         T25(83,IY,IS) =(OGCO2AVLs(1, 1,IY)*OGCO2PRCs(1, 1,IY) + OGCO2AVLs(2, 1,IY)*OGCO2PRCs(2, 1,IY) + &
    #                         OGCO2AVLs(3, 1,IY)*OGCO2PRCs(3, 1,IY) + OGCO2AVLs(4, 1,IY)*OGCO2PRCs(4, 1,IY) + &
    #                         OGCO2AVLs(5, 1,IY)*OGCO2PRCs(5, 1,IY) + OGCO2AVLs(6, 1,IY)*OGCO2PRCs(6, 1,IY) + &
    #                         OGCO2AVLs(7, 1,IY)*OGCO2PRCs(7, 1,IY)) / (OGCO2AVLs(1, 1,IY) + &
    #                         OGCO2AVLs(2, 1,IY) + OGCO2AVLs(3, 1,IY) + OGCO2AVLs(4, 1,IY) + &
    #                         OGCO2AVLs(5, 1,IY) + OGCO2AVLs(6, 1,IY) + OGCO2AVLs(7, 1,IY))
    # dict.txt ---
    # OGSMOUT   OGCO2AVLs   R    4 3 (8, 13,MNUMYR) QUNTY MMCF CO2 available (mmcf) by source
    # OGSMOUT   OGCO2PRCs   R    4 3 (8,13,MNUMYR) PRICE 87$MCF CO2 price ($/mmcf) by source
    z[83] = (
        (
            dfd["OGCO2AVLS"].loc[1].loc[1] * OGCO2PRCS.loc[1].loc[1]
            + dfd["OGCO2AVLS"].loc[2].loc[1] * OGCO2PRCS.loc[2].loc[1]
            + dfd["OGCO2AVLS"].loc[3].loc[1] * OGCO2PRCS.loc[3].loc[1]
            + dfd["OGCO2AVLS"].loc[4].loc[1] * OGCO2PRCS.loc[4].loc[1]
            + dfd["OGCO2AVLS"].loc[5].loc[1] * OGCO2PRCS.loc[5].loc[1]
            + dfd["OGCO2AVLS"].loc[6].loc[1] * OGCO2PRCS.loc[6].loc[1]
            + dfd["OGCO2AVLS"].loc[7].loc[1] * OGCO2PRCS.loc[7].loc[1]
        )
        / (
            dfd["OGCO2AVLS"].loc[1].loc[1]
            + dfd["OGCO2AVLS"].loc[2].loc[1]
            + dfd["OGCO2AVLS"].loc[3].loc[1]
            + dfd["OGCO2AVLS"].loc[4].loc[1]
            + dfd["OGCO2AVLS"].loc[5].loc[1]
            + dfd["OGCO2AVLS"].loc[6].loc[1]
            + dfd["OGCO2AVLS"].loc[7].loc[1]
        )
    ).fillna(0)

    #         Ammonia
    # IF (sum(OGCO2AVLs(1:7, 2,IY)) .NE. 0.0) &
    #         T25(84,IY,IS) =(OGCO2AVLs(1, 2,IY)*OGCO2PRCs(1, 2,IY) + OGCO2AVLs(2, 2,IY)*OGCO2PRCs(2, 2,IY) + &
    #                         OGCO2AVLs(3, 2,IY)*OGCO2PRCs(3, 2,IY) + OGCO2AVLs(4, 2,IY)*OGCO2PRCs(4, 2,IY) + &
    #                         OGCO2AVLs(5, 2,IY)*OGCO2PRCs(5, 2,IY) + OGCO2AVLs(6, 2,IY)*OGCO2PRCs(6, 2,IY) + &
    #                         OGCO2AVLs(7, 2,IY)*OGCO2PRCs(7, 2,IY)) / (OGCO2AVLs(1, 2,IY) + &
    #                         OGCO2AVLs(2, 2,IY) + OGCO2AVLs(3, 2,IY) + OGCO2AVLs(4, 2,IY) + &
    #                         OGCO2AVLs(5, 2,IY) + OGCO2AVLs(6, 2,IY) + OGCO2AVLs(7, 2,IY))
    z[84] = (
        (
            dfd["OGCO2AVLS"].loc[1].loc[2] * OGCO2PRCS.loc[1].loc[2]
            + dfd["OGCO2AVLS"].loc[2].loc[2] * OGCO2PRCS.loc[2].loc[2]
            + dfd["OGCO2AVLS"].loc[3].loc[2] * OGCO2PRCS.loc[3].loc[2]
            + dfd["OGCO2AVLS"].loc[4].loc[2] * OGCO2PRCS.loc[4].loc[2]
            + dfd["OGCO2AVLS"].loc[5].loc[2] * OGCO2PRCS.loc[5].loc[2]
            + dfd["OGCO2AVLS"].loc[6].loc[2] * OGCO2PRCS.loc[6].loc[2]
            + dfd["OGCO2AVLS"].loc[7].loc[2] * OGCO2PRCS.loc[7].loc[2]
        )
        / (
            dfd["OGCO2AVLS"].loc[1].loc[2]
            + dfd["OGCO2AVLS"].loc[2].loc[2]
            + dfd["OGCO2AVLS"].loc[3].loc[2]
            + dfd["OGCO2AVLS"].loc[4].loc[2]
            + dfd["OGCO2AVLS"].loc[5].loc[2]
            + dfd["OGCO2AVLS"].loc[6].loc[2]
            + dfd["OGCO2AVLS"].loc[7].loc[2]
        )
    ).fillna(0)

    #         Ethanol
    # IF (sum(OGCO2AVLs(1:7, 3,IY)) .NE. 0.0) &
    #         T25(85,IY,IS) =(OGCO2AVLs(1, 3,IY)*OGCO2PRCs(1, 3,IY) + OGCO2AVLs(2, 3,IY)*OGCO2PRCs(2, 3,IY) + &
    #                         OGCO2AVLs(3, 3,IY)*OGCO2PRCs(3, 3,IY) + OGCO2AVLs(4, 3,IY)*OGCO2PRCs(4, 3,IY) + &
    #                         OGCO2AVLs(5, 3,IY)*OGCO2PRCs(5, 3,IY) + OGCO2AVLs(6, 3,IY)*OGCO2PRCs(6, 3,IY) + &
    #                         OGCO2AVLs(7, 3,IY)*OGCO2PRCs(7, 3,IY)) / (OGCO2AVLs(1, 3,IY) + &
    #                         OGCO2AVLs(2, 3,IY) + OGCO2AVLs(3, 3,IY) + OGCO2AVLs(4, 3,IY) + &
    #                         OGCO2AVLs(5, 3,IY) + OGCO2AVLs(6, 3,IY) + OGCO2AVLs(7, 3,IY))
    z[85] = (
        (
            dfd["OGCO2AVLS"].loc[1].loc[3] * OGCO2PRCS.loc[1].loc[3]
            + dfd["OGCO2AVLS"].loc[2].loc[3] * OGCO2PRCS.loc[2].loc[3]
            + dfd["OGCO2AVLS"].loc[3].loc[3] * OGCO2PRCS.loc[3].loc[3]
            + dfd["OGCO2AVLS"].loc[4].loc[3] * OGCO2PRCS.loc[4].loc[3]
            + dfd["OGCO2AVLS"].loc[5].loc[3] * OGCO2PRCS.loc[5].loc[3]
            + dfd["OGCO2AVLS"].loc[6].loc[3] * OGCO2PRCS.loc[6].loc[3]
            + dfd["OGCO2AVLS"].loc[7].loc[3] * OGCO2PRCS.loc[7].loc[3]
        )
        / (
            dfd["OGCO2AVLS"].loc[1].loc[3]
            + dfd["OGCO2AVLS"].loc[2].loc[3]
            + dfd["OGCO2AVLS"].loc[3].loc[3]
            + dfd["OGCO2AVLS"].loc[4].loc[3]
            + dfd["OGCO2AVLS"].loc[5].loc[3]
            + dfd["OGCO2AVLS"].loc[6].loc[3]
            + dfd["OGCO2AVLS"].loc[7].loc[3]
        )
    ).fillna(0)

    #         Cement
    # IF (sum(OGCO2AVLs(1:7, 5,IY)) .NE. 0.0) &
    #         T25(87,IY,IS) =(OGCO2AVLs(1, 5,IY)*OGCO2PRCs(1, 5,IY) + OGCO2AVLs(2, 5,IY)*OGCO2PRCs(2, 5,IY) + &
    #                         OGCO2AVLs(3, 5,IY)*OGCO2PRCs(3, 5,IY) + OGCO2AVLs(4, 5,IY)*OGCO2PRCs(4, 5,IY) + &
    #                         OGCO2AVLs(5, 5,IY)*OGCO2PRCs(5, 5,IY) + OGCO2AVLs(6, 5,IY)*OGCO2PRCs(6, 5,IY) + &
    #                         OGCO2AVLs(7, 5,IY)*OGCO2PRCs(7, 5,IY)) / (OGCO2AVLs(1, 5,IY) + &
    #                         OGCO2AVLs(2, 5,IY) + OGCO2AVLs(3, 5,IY) + OGCO2AVLs(4, 5,IY) + &
    #                         OGCO2AVLs(5, 5,IY) + OGCO2AVLs(6, 5,IY) + OGCO2AVLs(7, 5,IY))
    z[87] = (
        (
            dfd["OGCO2AVLS"].loc[1].loc[5] * OGCO2PRCS.loc[1].loc[5]
            + dfd["OGCO2AVLS"].loc[2].loc[5] * OGCO2PRCS.loc[2].loc[5]
            + dfd["OGCO2AVLS"].loc[3].loc[5] * OGCO2PRCS.loc[3].loc[5]
            + dfd["OGCO2AVLS"].loc[4].loc[5] * OGCO2PRCS.loc[4].loc[5]
            + dfd["OGCO2AVLS"].loc[5].loc[5] * OGCO2PRCS.loc[5].loc[5]
            + dfd["OGCO2AVLS"].loc[6].loc[5] * OGCO2PRCS.loc[6].loc[5]
            + dfd["OGCO2AVLS"].loc[7].loc[5] * OGCO2PRCS.loc[7].loc[5]
        )
        / (
            dfd["OGCO2AVLS"].loc[1].loc[5]
            + dfd["OGCO2AVLS"].loc[2].loc[5]
            + dfd["OGCO2AVLS"].loc[3].loc[5]
            + dfd["OGCO2AVLS"].loc[4].loc[5]
            + dfd["OGCO2AVLS"].loc[5].loc[5]
            + dfd["OGCO2AVLS"].loc[6].loc[5]
            + dfd["OGCO2AVLS"].loc[7].loc[5]
        )
    ).fillna(0)

    #         Refineries, Hydrogen
    # IF (sum(OGCO2AVLs(1:7, 6,IY)) .NE. 0.0) &
    #         T25(88,IY,IS) =(OGCO2AVLs(1, 6,IY)*OGCO2PRCs(1, 6,IY) + OGCO2AVLs(2, 6,IY)*OGCO2PRCs(2, 6,IY) + &
    #                         OGCO2AVLs(3, 6,IY)*OGCO2PRCs(3, 6,IY) + OGCO2AVLs(4, 6,IY)*OGCO2PRCs(4, 6,IY) + &
    #                         OGCO2AVLs(5, 6,IY)*OGCO2PRCs(5, 6,IY) + OGCO2AVLs(6, 6,IY)*OGCO2PRCs(6, 6,IY) + &
    #                         OGCO2AVLs(7, 6,IY)*OGCO2PRCs(7, 6,IY)) / (OGCO2AVLs(1, 6,IY) + &
    #                         OGCO2AVLs(2, 6,IY) + OGCO2AVLs(3, 6,IY) + OGCO2AVLs(4, 6,IY) + &
    #                         OGCO2AVLs(5, 6,IY) + OGCO2AVLs(6, 6,IY) + OGCO2AVLs(7, 6,IY))
    z[88] = (
        (
            dfd["OGCO2AVLS"].loc[1].loc[6] * OGCO2PRCS.loc[1].loc[6]
            + dfd["OGCO2AVLS"].loc[2].loc[6] * OGCO2PRCS.loc[2].loc[6]
            + dfd["OGCO2AVLS"].loc[3].loc[6] * OGCO2PRCS.loc[3].loc[6]
            + dfd["OGCO2AVLS"].loc[4].loc[6] * OGCO2PRCS.loc[4].loc[6]
            + dfd["OGCO2AVLS"].loc[5].loc[6] * OGCO2PRCS.loc[5].loc[6]
            + dfd["OGCO2AVLS"].loc[6].loc[6] * OGCO2PRCS.loc[6].loc[6]
            + dfd["OGCO2AVLS"].loc[7].loc[6] * OGCO2PRCS.loc[7].loc[6]
        )
        / (
            dfd["OGCO2AVLS"].loc[1].loc[6]
            + dfd["OGCO2AVLS"].loc[2].loc[6]
            + dfd["OGCO2AVLS"].loc[3].loc[6]
            + dfd["OGCO2AVLS"].loc[4].loc[6]
            + dfd["OGCO2AVLS"].loc[5].loc[6]
            + dfd["OGCO2AVLS"].loc[6].loc[6]
            + dfd["OGCO2AVLS"].loc[7].loc[6]
        )
    ).fillna(0)

    #         Electric Power Plants
    # IF (sum(OGCO2AVLs(1:7, 7,IY)) .NE. 0.0) &
    #         T25(89,IY,IS) =(OGCO2AVLs(1, 7,IY)*OGCO2PRCs(1, 7,IY) + OGCO2AVLs(2, 7,IY)*OGCO2PRCs(2, 7,IY) + &
    #                         OGCO2AVLs(3, 7,IY)*OGCO2PRCs(3, 7,IY) + OGCO2AVLs(4, 7,IY)*OGCO2PRCs(4, 7,IY) + &
    #                         OGCO2AVLs(5, 7,IY)*OGCO2PRCs(5, 7,IY) + OGCO2AVLs(6, 7,IY)*OGCO2PRCs(6, 7,IY) + &
    #                         OGCO2AVLs(7, 7,IY)*OGCO2PRCs(7, 7,IY)) / (OGCO2AVLs(1, 7,IY) + &
    #                         OGCO2AVLs(2, 7,IY) + OGCO2AVLs(3, 7,IY) + OGCO2AVLs(4, 7,IY) + &
    #                         OGCO2AVLs(5, 7,IY) + OGCO2AVLs(6, 7,IY) + OGCO2AVLs(7, 7,IY))
    z[89] = (
        (
            dfd["OGCO2AVLS"].loc[1].loc[7] * OGCO2PRCS.loc[1].loc[7]
            + dfd["OGCO2AVLS"].loc[2].loc[7] * OGCO2PRCS.loc[2].loc[7]
            + dfd["OGCO2AVLS"].loc[3].loc[7] * OGCO2PRCS.loc[3].loc[7]
            + dfd["OGCO2AVLS"].loc[4].loc[7] * OGCO2PRCS.loc[4].loc[7]
            + dfd["OGCO2AVLS"].loc[5].loc[7] * OGCO2PRCS.loc[5].loc[7]
            + dfd["OGCO2AVLS"].loc[6].loc[7] * OGCO2PRCS.loc[6].loc[7]
            + dfd["OGCO2AVLS"].loc[7].loc[7] * OGCO2PRCS.loc[7].loc[7]
        )
        / (
            dfd["OGCO2AVLS"].loc[1].loc[7]
            + dfd["OGCO2AVLS"].loc[2].loc[7]
            + dfd["OGCO2AVLS"].loc[3].loc[7]
            + dfd["OGCO2AVLS"].loc[4].loc[7]
            + dfd["OGCO2AVLS"].loc[5].loc[7]
            + dfd["OGCO2AVLS"].loc[6].loc[7]
            + dfd["OGCO2AVLS"].loc[7].loc[7]
        )
    ).fillna(0)

    #         Natural Gas Processing
    # IF (sum(OGCO2AVLs(1:7, 9,IY)) .NE. 0.0) &
    #         T25(91,IY,IS) =(OGCO2AVLs(1, 9,IY)*OGCO2PRCs(1, 9,IY) + OGCO2AVLs(2, 9,IY)*OGCO2PRCs(2, 9,IY) + &
    #                         OGCO2AVLs(3, 9,IY)*OGCO2PRCs(3, 9,IY) + OGCO2AVLs(4, 9,IY)*OGCO2PRCs(4, 9,IY) + &
    #                         OGCO2AVLs(5, 9,IY)*OGCO2PRCs(5, 9,IY) + OGCO2AVLs(6, 9,IY)*OGCO2PRCs(6, 9,IY) + &
    #                         OGCO2AVLs(7, 9,IY)*OGCO2PRCs(7, 9,IY)) / (OGCO2AVLs(1, 9,IY) + &
    #                         OGCO2AVLs(2, 9,IY) + OGCO2AVLs(3, 9,IY) + OGCO2AVLs(4, 9,IY) + &
    #                         OGCO2AVLs(5, 9,IY) + OGCO2AVLs(6, 9,IY) + OGCO2AVLs(7, 9,IY))
    z[91] = (
        (
            dfd["OGCO2AVLS"].loc[1].loc[9] * OGCO2PRCS.loc[1].loc[9]
            + dfd["OGCO2AVLS"].loc[2].loc[9] * OGCO2PRCS.loc[2].loc[9]
            + dfd["OGCO2AVLS"].loc[3].loc[9] * OGCO2PRCS.loc[3].loc[9]
            + dfd["OGCO2AVLS"].loc[4].loc[9] * OGCO2PRCS.loc[4].loc[9]
            + dfd["OGCO2AVLS"].loc[5].loc[9] * OGCO2PRCS.loc[5].loc[9]
            + dfd["OGCO2AVLS"].loc[6].loc[9] * OGCO2PRCS.loc[6].loc[9]
            + dfd["OGCO2AVLS"].loc[7].loc[9] * OGCO2PRCS.loc[7].loc[9]
        )
        / (
            dfd["OGCO2AVLS"].loc[1].loc[9]
            + dfd["OGCO2AVLS"].loc[2].loc[9]
            + dfd["OGCO2AVLS"].loc[3].loc[9]
            + dfd["OGCO2AVLS"].loc[4].loc[9]
            + dfd["OGCO2AVLS"].loc[5].loc[9]
            + dfd["OGCO2AVLS"].loc[6].loc[9]
            + dfd["OGCO2AVLS"].loc[7].loc[9]
        )
    ).fillna(0)

    #         Coal-to-Liquids Plants
    # IF (sum(OGCO2AVLs(1:7,10,IY)) .NE. 0.0) &
    #         T25(92,IY,IS) =(OGCO2AVLs(1,10,IY)*OGCO2PRCs(1,10,IY) + OGCO2AVLs(2,10,IY)*OGCO2PRCs(2,10,IY) + &
    #                         OGCO2AVLs(3,10,IY)*OGCO2PRCs(3,10,IY) + OGCO2AVLs(4,10,IY)*OGCO2PRCs(4,10,IY) + &
    #                         OGCO2AVLs(5,10,IY)*OGCO2PRCs(5,10,IY) + OGCO2AVLs(6,10,IY)*OGCO2PRCs(6,10,IY) + &
    #                         OGCO2AVLs(7,10,IY)*OGCO2PRCs(7,10,IY)) / (OGCO2AVLs(1,10,IY) + &
    #                         OGCO2AVLs(2,10,IY) + OGCO2AVLs(3,10,IY) + OGCO2AVLs(4,10,IY) + &
    #                         OGCO2AVLs(5,10,IY) + OGCO2AVLs(6,10,IY) + OGCO2AVLs(7,10,IY))
    z[92] = (
        (
            dfd["OGCO2AVLS"].loc[1].loc[10] * OGCO2PRCS.loc[1].loc[10]
            + dfd["OGCO2AVLS"].loc[2].loc[10] * OGCO2PRCS.loc[2].loc[10]
            + dfd["OGCO2AVLS"].loc[3].loc[10] * OGCO2PRCS.loc[3].loc[10]
            + dfd["OGCO2AVLS"].loc[4].loc[10] * OGCO2PRCS.loc[4].loc[10]
            + dfd["OGCO2AVLS"].loc[5].loc[10] * OGCO2PRCS.loc[5].loc[10]
            + dfd["OGCO2AVLS"].loc[6].loc[10] * OGCO2PRCS.loc[6].loc[10]
            + dfd["OGCO2AVLS"].loc[7].loc[10] * OGCO2PRCS.loc[3].loc[10]
        )
        / (
            dfd["OGCO2AVLS"].loc[1].loc[10]
            + dfd["OGCO2AVLS"].loc[2].loc[10]
            + dfd["OGCO2AVLS"].loc[3].loc[10]
            + dfd["OGCO2AVLS"].loc[4].loc[10]
            + dfd["OGCO2AVLS"].loc[5].loc[10]
            + dfd["OGCO2AVLS"].loc[6].loc[10]
            + dfd["OGCO2AVLS"].loc[7].loc[10]
        )
    ).fillna(0)

    #      Average, Anthropogenic Sources
    # ftab.f
    # IF (sum(OGCO2AVLs(1:7,1:3,IY))+sum(OGCO2AVLs(1:7,5:7,IY))+sum(OGCO2AVLs(1:7,9:10,IY)) .NE. 0.0) &
    #         T25(96,IY,IS) =(sum(OGCO2AVLs(1:7, 1,IY))*T25(83,IY,IS) + &
    #                         sum(OGCO2AVLs(1:7, 2,IY))*T25(84,IY,IS) + &
    #                         sum(OGCO2AVLs(1:7, 3,IY))*T25(85,IY,IS) + &
    #                         sum(OGCO2AVLs(1:7, 5,IY))*T25(87,IY,IS) + &
    #                         sum(OGCO2AVLs(1:7, 6,IY))*T25(88,IY,IS) + &
    #                         sum(OGCO2AVLs(1:7, 7,IY))*T25(89,IY,IS) + &
    #                         sum(OGCO2AVLs(1:7, 9,IY))*T25(91,IY,IS) + &
    #                         sum(OGCO2AVLs(1:7,10,IY))*T25(92,IY,IS))/ &
    #                        (sum(OGCO2AVLs(1:7,1:3,IY))+sum(OGCO2AVLs(1:7,5:7,IY))+sum(OGCO2AVLs(1:7,9:10,IY)))
    z[96] = (
        (
            dfd["OGCO2AVLS"].loc[1:7, 1, :].sum() * z[83].fillna(0)
            + dfd["OGCO2AVLS"].loc[1:7, 2, :].sum() * z[84].fillna(0)
            + dfd["OGCO2AVLS"].loc[1:7, 3, :].sum() * z[85].fillna(0)
            + dfd["OGCO2AVLS"].loc[1:7, 5, :].sum() * z[87].fillna(0)
            + dfd["OGCO2AVLS"].loc[1:7, 6, :].sum() * z[88].fillna(0)
            + dfd["OGCO2AVLS"].loc[1:7, 7, :].sum() * z[89].fillna(0)
            + dfd["OGCO2AVLS"].loc[1:7, 9, :].sum() * z[91].fillna(0)
            + dfd["OGCO2AVLS"].loc[1:7, 10, :].sum() * z[92].fillna(0)
        )
        / (
            dfd["OGCO2AVLS"].loc[1:7, 1:3, :].sum()
            + dfd["OGCO2AVLS"].loc[1:7, 5:7, :].sum()
            + dfd["OGCO2AVLS"].loc[1:7, 9:10, :].sum()
        )
    ).fillna(0)
    # z[96] = z[96] / dfd['C_CO2_FACTOR_rwpre']

    #         Natural
    # T25(86,IY,IS)=(OGCO2AVLS(1,4,IY)*OGCO2PRCS(1,4,IY)+OGCO2AVLS(2,4,IY)*OGCO2PRCS(2,4,IY)+OGCO2AVLS(3,4,IY)*OGCO2PRCS(3,4,IY)+OGCO2AVLS(4,4,IY)*OGCO2PRCS(4,4,IY)+OGCO2AVLS(5,4,IY)*OGCO2PRCS(5,4,IY)+OGCO2AVLS(6,4,IY)*OGCO2PRCS(6,4,IY)+OGCO2AVLS(7,4,IY)*OGCO2PRCS(7,4,IY))/(OGCO2AVLS(1,4,IY)+OGCO2AVLS(2,4,IY)+OGCO2AVLS(3,4,IY)+OGCO2AVLS(4,4,IY)+OGCO2AVLS(5,4,IY)+OGCO2AVLS(6,4,IY)+OGCO2AVLS(7,4,IY))
    # z[86] = SCALPR2*((dfd['OGCO2AVLS'].loc[1].loc[4] * dfd['OGCO2PRCS'].loc[1].loc[4] + dfd['OGCO2AVLS'].loc[2].loc[4] * dfd['OGCO2PRCS'].loc[2].loc[4] + dfd['OGCO2AVLS'].loc[3].loc[4] * dfd['OGCO2PRCS'].loc[3].loc[4] + dfd['OGCO2AVLS'].loc[4].loc[4] * dfd['OGCO2PRCS'].loc[4].loc[4] + dfd['OGCO2AVLS'].loc[5].loc[4] * dfd['OGCO2PRCS'].loc[5].loc[4] + dfd['OGCO2AVLS'].loc[6].loc[4] * dfd['OGCO2PRCS'].loc[6].loc[4] + dfd['OGCO2AVLS'].loc[7].loc[4] * dfd['OGCO2PRCS'].loc[7].loc[4])/ (dfd['OGCO2AVLS'].loc[1].loc[4] + dfd['OGCO2AVLS'].loc[2].loc[4] + dfd['OGCO2AVLS'].loc[3].loc[4] + dfd['OGCO2AVLS'].loc[4].loc[4] + dfd['OGCO2AVLS'].loc[5].loc[4] + dfd['OGCO2AVLS'].loc[6].loc[4] + dfd['OGCO2AVLS'].loc[7].loc[4]))
    #  IF (sum(OGCO2AVLs(1:7, 4,IY)) .NE. 0.0) &
    #         T25(86,IY,IS) =(OGCO2AVLs(1, 4,IY)*OGCO2PRCs(1, 4,IY) + OGCO2AVLs(2, 4,IY)*OGCO2PRCs(2, 4,IY) + &
    #                         OGCO2AVLs(3, 4,IY)*OGCO2PRCs(3, 4,IY) + OGCO2AVLs(4, 4,IY)*OGCO2PRCs(4, 4,IY) + &
    #                         OGCO2AVLs(5, 4,IY)*OGCO2PRCs(5, 4,IY) + OGCO2AVLs(6, 4,IY)*OGCO2PRCs(6, 4,IY) + &
    #                         OGCO2AVLs(7, 4,IY)*OGCO2PRCs(7, 4,IY)) / (OGCO2AVLs(1, 4,IY) + &
    #                         OGCO2AVLs(2, 4,IY) + OGCO2AVLs(3, 4,IY) + OGCO2AVLs(4, 4,IY) + &
    #                         OGCO2AVLs(5, 4,IY) + OGCO2AVLs(6, 4,IY) + OGCO2AVLs(7, 4,IY))
    z[86] = (
        (
            dfd["OGCO2AVLS"].loc[1].loc[4] * OGCO2PRCS.loc[1].loc[4]
            + dfd["OGCO2AVLS"].loc[2].loc[4] * OGCO2PRCS.loc[2].loc[4]
            + dfd["OGCO2AVLS"].loc[3].loc[4] * OGCO2PRCS.loc[3].loc[4]
            + dfd["OGCO2AVLS"].loc[4].loc[4] * OGCO2PRCS.loc[4].loc[4]
            + dfd["OGCO2AVLS"].loc[5].loc[4] * OGCO2PRCS.loc[5].loc[4]
            + dfd["OGCO2AVLS"].loc[6].loc[4] * OGCO2PRCS.loc[6].loc[4]
            + dfd["OGCO2AVLS"].loc[7].loc[4] * OGCO2PRCS.loc[7].loc[4]
        )
        / (
            dfd["OGCO2AVLS"].loc[1].loc[4]
            + dfd["OGCO2AVLS"].loc[2].loc[4]
            + dfd["OGCO2AVLS"].loc[3].loc[4]
            + dfd["OGCO2AVLS"].loc[4].loc[4]
            + dfd["OGCO2AVLS"].loc[5].loc[4]
            + dfd["OGCO2AVLS"].loc[6].loc[4]
            + dfd["OGCO2AVLS"].loc[7].loc[4]
        )
    ).fillna(0)
    # z[86] = z[86] / 3.67 # dfd['C_CO2_FACTOR_rwpre']

    #      Average, All Sources
    # T25(97,IY,IS)=(SUM(OGCO2AVLS(1:7,1,IY))*T25(83,IY,IS)+SUM(OGCO2AVLS(1:7,2,IY))*T25(84,IY,IS)+SUM(OGCO2AVLS(1:7,3,IY))*T25(85,IY,IS)+SUM(OGCO2AVLS(1:7,4,IY))*T25(86,IY,IS)+SUM(OGCO2AVLS(1:7,5,IY))*T25(87,IY,IS)+SUM(OGCO2AVLS(1:7,6,IY))*T25(88,IY,IS)+SUM(OGCO2AVLS(1:7,7,IY))*T25(89,IY,IS)+SUM(OGCO2AVLS(1:7,9,IY))*T25(91,IY,IS)+SUM(OGCO2AVLS(1:7,10,IY))*T25(92,IY,IS))/(SUM(OGCO2AVLS(1:7,1:7,IY))+SUM(OGCO2AVLS(1:7,9:10,IY)))
    # z[97] =  SCALPR2*((dfd['OGCO2AVLS'].loc[1:7,1,:].sum()*z[83]+ dfd['OGCO2AVLS'].loc[1:7,2,:].sum()*z[84]+ dfd['OGCO2AVLS'].loc[1:7,3,:].sum()*z[85]+ dfd['OGCO2AVLS'].loc[1:7,4,:].sum()*z[86]+ dfd['OGCO2AVLS'].loc[1:7,5,:].sum()*z[87]+ dfd['OGCO2AVLS'].loc[1:7,6,:].sum()*z[88]+ dfd['OGCO2AVLS'].loc[1:7,7,:].sum()*z[89]+ dfd['OGCO2AVLS'].loc[1:7,9,:].sum()*z[91]+ dfd['OGCO2AVLS'].loc[1:7,10,:].sum()*z[92] )/ (dfd['OGCO2AVLS'].loc[1:7,1:7,:].sum()+ dfd['OGCO2AVLS'].loc[1:7,9:10,:].sum()))
    # IF (sum(OGCO2AVLs(1:7,1:7,IY))+sum(OGCO2AVLs(1:7,9:10,IY)) .NE. 0.0) &
    #         T25(97,IY,IS) =(sum(OGCO2AVLs(1:7, 1,IY))*T25(83,IY,IS) + &
    #                         sum(OGCO2AVLs(1:7, 2,IY))*T25(84,IY,IS) + &
    #                         sum(OGCO2AVLs(1:7, 3,IY))*T25(85,IY,IS) + &
    #                         sum(OGCO2AVLs(1:7, 4,IY))*T25(86,IY,IS) + &
    #                         sum(OGCO2AVLs(1:7, 5,IY))*T25(87,IY,IS) + &
    #                         sum(OGCO2AVLs(1:7, 6,IY))*T25(88,IY,IS) + &
    #                         sum(OGCO2AVLs(1:7, 7,IY))*T25(89,IY,IS) + &
    #                         sum(OGCO2AVLs(1:7, 9,IY))*T25(91,IY,IS) + &
    #                         sum(OGCO2AVLs(1:7,10,IY))*T25(92,IY,IS))/ &
    #                        (sum(OGCO2AVLs(1:7,1:7,IY))+sum(OGCO2AVLs(1:7,9:10,IY)))
    z[97] = (
        (
            dfd["OGCO2AVLS"].loc[1:7, 1, :].sum() * z[83]
            + dfd["OGCO2AVLS"].loc[1:7, 2, :].sum() * z[84]
            + dfd["OGCO2AVLS"].loc[1:7, 3, :].sum() * z[85]
            + dfd["OGCO2AVLS"].loc[1:7, 4, :].sum() * z[86]
            + dfd["OGCO2AVLS"].loc[1:7, 5, :].sum() * z[87]
            + dfd["OGCO2AVLS"].loc[1:7, 6, :].sum() * z[88]
            + dfd["OGCO2AVLS"].loc[1:7, 7, :].sum() * z[89]
            + dfd["OGCO2AVLS"].loc[1:7, 9, :].sum() * z[91]
            + dfd["OGCO2AVLS"].loc[1:7, 10, :].sum() * z[92]
        )
        / (
            dfd["OGCO2AVLS"].loc[1:7, 1:7, :].sum()
            + dfd["OGCO2AVLS"].loc[1:7, 9:10, :].sum()
        )
    ).fillna(0)
    # z[97] = z[97] / 3.67 # dfd['C_CO2_FACTOR_rwpre']

    #   Carbon Dioxide Price by Region
    #   (#### dollars per thousand cubic feet)
    #      weighted by carbon dioxide purchased

    #         Hydrogen
    # T25(98,IY,IS)=(OGCO2PUR(1,1,IY)*OGCO2PRCS(1,1,IY)+OGCO2PUR(2,1,IY)*OGCO2PRCS(2,1,IY)+OGCO2PUR(3,1,IY)*OGCO2PRCS(3,1,IY)+OGCO2PUR(4,1,IY)*OGCO2PRCS(4,1,IY)+OGCO2PUR(5,1,IY)*OGCO2PRCS(5,1,IY)+OGCO2PUR(6,1,IY)*OGCO2PRCS(6,1,IY)+OGCO2PUR(7,1,IY)*OGCO2PRCS(7,1,IY))/(OGCO2PUR(1,1,IY)+OGCO2PUR(2,1,IY)+OGCO2PUR(3,1,IY)+OGCO2PUR(4,1,IY)+OGCO2PUR(5,1,IY)+OGCO2PUR(6,1,IY)+OGCO2PUR(7,1,IY))
    # z[98] = SCALPR2*((dfd['OGCO2PUR'].loc[1].loc[1] * dfd['OGCO2PRCS'].loc[1].loc[1] + dfd['OGCO2PUR'].loc[2].loc[1] * dfd['OGCO2PRCS'].loc[2].loc[1] + dfd['OGCO2PUR'].loc[3].loc[1] * dfd['OGCO2PRCS'].loc[3].loc[1] + dfd['OGCO2PUR'].loc[4].loc[1] * dfd['OGCO2PRCS'].loc[4].loc[1] + dfd['OGCO2PUR'].loc[5].loc[1] * dfd['OGCO2PRCS'].loc[5].loc[1] + dfd['OGCO2PUR'].loc[6].loc[1] * dfd['OGCO2PRCS'].loc[6].loc[1] + dfd['OGCO2PUR'].loc[7].loc[1] * dfd['OGCO2PRCS'].loc[7].loc[1])/ (dfd['OGCO2PUR'].loc[1].loc[1] + dfd['OGCO2PUR'].loc[2].loc[1] + dfd['OGCO2PUR'].loc[3].loc[1] + dfd['OGCO2PUR'].loc[4].loc[1] + dfd['OGCO2PUR'].loc[5].loc[1] + dfd['OGCO2PUR'].loc[6].loc[1] + dfd['OGCO2PUR'].loc[7].loc[1]))
    z[98] = (
        (
            dfd["OGCO2PUR"].loc[1].loc[1] * OGCO2PRCS.loc[1].loc[1]
            + dfd["OGCO2PUR"].loc[2].loc[1] * OGCO2PRCS.loc[2].loc[1]
            + dfd["OGCO2PUR"].loc[3].loc[1] * OGCO2PRCS.loc[3].loc[1]
            + dfd["OGCO2PUR"].loc[4].loc[1] * OGCO2PRCS.loc[4].loc[1]
            + dfd["OGCO2PUR"].loc[5].loc[1] * OGCO2PRCS.loc[5].loc[1]
            + dfd["OGCO2PUR"].loc[6].loc[1] * OGCO2PRCS.loc[6].loc[1]
            + dfd["OGCO2PUR"].loc[7].loc[1] * OGCO2PRCS.loc[7].loc[1]
        )
        / (
            dfd["OGCO2PUR"].loc[1].loc[1]
            + dfd["OGCO2PUR"].loc[2].loc[1]
            + dfd["OGCO2PUR"].loc[3].loc[1]
            + dfd["OGCO2PUR"].loc[4].loc[1]
            + dfd["OGCO2PUR"].loc[5].loc[1]
            + dfd["OGCO2PUR"].loc[6].loc[1]
            + dfd["OGCO2PUR"].loc[7].loc[1]
        )
    ).fillna(0)

    #         Ammonia
    # T25(99,IY,IS)=(OGCO2PUR(1,2,IY)*OGCO2PRCS(1,2,IY)+OGCO2PUR(2,2,IY)*OGCO2PRCS(2,2,IY)+OGCO2PUR(3,2,IY)*OGCO2PRCS(3,2,IY)+OGCO2PUR(4,2,IY)*OGCO2PRCS(4,2,IY)+OGCO2PUR(5,2,IY)*OGCO2PRCS(5,2,IY)+OGCO2PUR(6,2,IY)*OGCO2PRCS(6,2,IY)+OGCO2PUR(7,2,IY)*OGCO2PRCS(7,2,IY))/(OGCO2PUR(1,2,IY)+OGCO2PUR(2,2,IY)+OGCO2PUR(3,2,IY)+OGCO2PUR(4,2,IY)+OGCO2PUR(5,2,IY)+OGCO2PUR(6,2,IY)+OGCO2PUR(7,2,IY))
    # z[99] = SCALPR2*((dfd['OGCO2PUR'].loc[1].loc[2] * dfd['OGCO2PRCS'].loc[1].loc[2] + dfd['OGCO2PUR'].loc[2].loc[2] * dfd['OGCO2PRCS'].loc[2].loc[2] + dfd['OGCO2PUR'].loc[3].loc[2] * dfd['OGCO2PRCS'].loc[3].loc[2] + dfd['OGCO2PUR'].loc[4].loc[2] * dfd['OGCO2PRCS'].loc[4].loc[2] + dfd['OGCO2PUR'].loc[5].loc[2] * dfd['OGCO2PRCS'].loc[5].loc[2] + dfd['OGCO2PUR'].loc[6].loc[2] * dfd['OGCO2PRCS'].loc[6].loc[2] + dfd['OGCO2PUR'].loc[7].loc[2] * dfd['OGCO2PRCS'].loc[7].loc[2])/ (dfd['OGCO2PUR'].loc[1].loc[2] + dfd['OGCO2PUR'].loc[2].loc[2] + dfd['OGCO2PUR'].loc[3].loc[2] + dfd['OGCO2PUR'].loc[4].loc[2] + dfd['OGCO2PUR'].loc[5].loc[2] + dfd['OGCO2PUR'].loc[6].loc[2] + dfd['OGCO2PUR'].loc[7].loc[2]))
    z[99] = (
        (
            dfd["OGCO2PUR"].loc[1].loc[2] * OGCO2PRCS.loc[1].loc[2]
            + dfd["OGCO2PUR"].loc[2].loc[2] * OGCO2PRCS.loc[2].loc[2]
            + dfd["OGCO2PUR"].loc[3].loc[2] * OGCO2PRCS.loc[3].loc[2]
            + dfd["OGCO2PUR"].loc[4].loc[2] * OGCO2PRCS.loc[4].loc[2]
            + dfd["OGCO2PUR"].loc[5].loc[2] * OGCO2PRCS.loc[5].loc[2]
            + dfd["OGCO2PUR"].loc[6].loc[2] * OGCO2PRCS.loc[6].loc[2]
            + dfd["OGCO2PUR"].loc[7].loc[2] * OGCO2PRCS.loc[7].loc[2]
        )
        / (
            dfd["OGCO2PUR"].loc[1].loc[2]
            + dfd["OGCO2PUR"].loc[2].loc[2]
            + dfd["OGCO2PUR"].loc[3].loc[2]
            + dfd["OGCO2PUR"].loc[4].loc[2]
            + dfd["OGCO2PUR"].loc[5].loc[2]
            + dfd["OGCO2PUR"].loc[6].loc[2]
            + dfd["OGCO2PUR"].loc[7].loc[2]
        )
    ).fillna(0)

    #         Ethanol
    # T25(100,IY,IS)=(OGCO2PUR(1,3,IY)*OGCO2PRCS(1,3,IY)+OGCO2PUR(2,3,IY)*OGCO2PRCS(2,3,IY)+OGCO2PUR(3,3,IY)*OGCO2PRCS(3,3,IY)+OGCO2PUR(4,3,IY)*OGCO2PRCS(4,3,IY)+OGCO2PUR(5,3,IY)*OGCO2PRCS(5,3,IY)+OGCO2PUR(6,3,IY)*OGCO2PRCS(6,3,IY)+OGCO2PUR(7,3,IY)*OGCO2PRCS(7,3,IY))/(OGCO2PUR(1,3,IY)+OGCO2PUR(2,3,IY)+OGCO2PUR(3,3,IY)+OGCO2PUR(4,3,IY)+OGCO2PUR(5,3,IY)+OGCO2PUR(6,3,IY)+OGCO2PUR(7,3,IY))
    # z[100] = SCALPR2*((dfd['OGCO2PUR'].loc[1].loc[3] * dfd['OGCO2PRCS'].loc[1].loc[3] + dfd['OGCO2PUR'].loc[2].loc[3] * dfd['OGCO2PRCS'].loc[2].loc[3] + dfd['OGCO2PUR'].loc[3].loc[3] * dfd['OGCO2PRCS'].loc[3].loc[3] + dfd['OGCO2PUR'].loc[4].loc[3] * dfd['OGCO2PRCS'].loc[4].loc[3] + dfd['OGCO2PUR'].loc[5].loc[3] * dfd['OGCO2PRCS'].loc[5].loc[3] + dfd['OGCO2PUR'].loc[6].loc[3] * dfd['OGCO2PRCS'].loc[6].loc[3] + dfd['OGCO2PUR'].loc[7].loc[3] * dfd['OGCO2PRCS'].loc[7].loc[3])/ (dfd['OGCO2PUR'].loc[1].loc[3] + dfd['OGCO2PUR'].loc[2].loc[3] + dfd['OGCO2PUR'].loc[3].loc[3] + dfd['OGCO2PUR'].loc[4].loc[3] + dfd['OGCO2PUR'].loc[5].loc[3] + dfd['OGCO2PUR'].loc[6].loc[3] + dfd['OGCO2PUR'].loc[7].loc[3]))
    z[100] = (
        (
            dfd["OGCO2PUR"].loc[1].loc[3] * OGCO2PRCS.loc[1].loc[3]
            + dfd["OGCO2PUR"].loc[2].loc[3] * OGCO2PRCS.loc[2].loc[3]
            + dfd["OGCO2PUR"].loc[3].loc[3] * OGCO2PRCS.loc[3].loc[3]
            + dfd["OGCO2PUR"].loc[4].loc[3] * OGCO2PRCS.loc[4].loc[3]
            + dfd["OGCO2PUR"].loc[5].loc[3] * OGCO2PRCS.loc[5].loc[3]
            + dfd["OGCO2PUR"].loc[6].loc[3] * OGCO2PRCS.loc[6].loc[3]
            + dfd["OGCO2PUR"].loc[7].loc[3] * OGCO2PRCS.loc[7].loc[3]
        )
        / (
            dfd["OGCO2PUR"].loc[1].loc[3]
            + dfd["OGCO2PUR"].loc[2].loc[3]
            + dfd["OGCO2PUR"].loc[3].loc[3]
            + dfd["OGCO2PUR"].loc[4].loc[3]
            + dfd["OGCO2PUR"].loc[5].loc[3]
            + dfd["OGCO2PUR"].loc[6].loc[3]
            + dfd["OGCO2PUR"].loc[7].loc[3]
        )
    ).fillna(0)

    #         Cement
    # T25(102,IY,IS)=(OGCO2PUR(1,5,IY)*OGCO2PRCS(1,5,IY)+OGCO2PUR(2,5,IY)*OGCO2PRCS(2,5,IY)+OGCO2PUR(3,5,IY)*OGCO2PRCS(3,5,IY)+OGCO2PUR(4,5,IY)*OGCO2PRCS(4,5,IY)+OGCO2PUR(5,5,IY)*OGCO2PRCS(5,5,IY)+OGCO2PUR(6,5,IY)*OGCO2PRCS(6,5,IY)+OGCO2PUR(7,5,IY)*OGCO2PRCS(7,5,IY))/(OGCO2PUR(1,5,IY)+OGCO2PUR(2,5,IY)+OGCO2PUR(3,5,IY)+OGCO2PUR(4,5,IY)+OGCO2PUR(5,5,IY)+OGCO2PUR(6,5,IY)+OGCO2PUR(7,5,IY))
    # z[102] = SCALPR2*((dfd['OGCO2PUR'].loc[1].loc[5] * dfd['OGCO2PRCS'].loc[1].loc[5] + dfd['OGCO2PUR'].loc[2].loc[5] * dfd['OGCO2PRCS'].loc[2].loc[5] + dfd['OGCO2PUR'].loc[3].loc[5] * dfd['OGCO2PRCS'].loc[3].loc[5] + dfd['OGCO2PUR'].loc[4].loc[5] * dfd['OGCO2PRCS'].loc[4].loc[5] + dfd['OGCO2PUR'].loc[5].loc[5] * dfd['OGCO2PRCS'].loc[5].loc[5] + dfd['OGCO2PUR'].loc[6].loc[5] * dfd['OGCO2PRCS'].loc[6].loc[5] + dfd['OGCO2PUR'].loc[7].loc[5] * dfd['OGCO2PRCS'].loc[7].loc[5])/ (dfd['OGCO2PUR'].loc[1].loc[5] + dfd['OGCO2PUR'].loc[2].loc[5] + dfd['OGCO2PUR'].loc[3].loc[5] + dfd['OGCO2PUR'].loc[4].loc[5] + dfd['OGCO2PUR'].loc[5].loc[5] + dfd['OGCO2PUR'].loc[6].loc[5] + dfd['OGCO2PUR'].loc[7].loc[5]))
    z[102] = (
        (
            dfd["OGCO2PUR"].loc[1].loc[5] * OGCO2PRCS.loc[1].loc[5]
            + dfd["OGCO2PUR"].loc[2].loc[5] * OGCO2PRCS.loc[2].loc[5]
            + dfd["OGCO2PUR"].loc[3].loc[5] * OGCO2PRCS.loc[3].loc[5]
            + dfd["OGCO2PUR"].loc[4].loc[5] * OGCO2PRCS.loc[4].loc[5]
            + dfd["OGCO2PUR"].loc[5].loc[5] * OGCO2PRCS.loc[5].loc[5]
            + dfd["OGCO2PUR"].loc[6].loc[5] * OGCO2PRCS.loc[6].loc[5]
            + dfd["OGCO2PUR"].loc[7].loc[5] * OGCO2PRCS.loc[7].loc[5]
        )
        / (
            dfd["OGCO2PUR"].loc[1].loc[5]
            + dfd["OGCO2PUR"].loc[2].loc[5]
            + dfd["OGCO2PUR"].loc[3].loc[5]
            + dfd["OGCO2PUR"].loc[4].loc[5]
            + dfd["OGCO2PUR"].loc[5].loc[5]
            + dfd["OGCO2PUR"].loc[6].loc[5]
            + dfd["OGCO2PUR"].loc[7].loc[5]
        )
    ).fillna(0)

    #         Refineries, Hydrogen
    # T25(103,IY,IS)=(OGCO2PUR(1,6,IY)*OGCO2PRCS(1,6,IY)+OGCO2PUR(2,6,IY)*OGCO2PRCS(2,6,IY)+OGCO2PUR(3,6,IY)*OGCO2PRCS(3,6,IY)+OGCO2PUR(4,6,IY)*OGCO2PRCS(4,6,IY)+OGCO2PUR(5,6,IY)*OGCO2PRCS(5,6,IY)+OGCO2PUR(6,6,IY)*OGCO2PRCS(6,6,IY)+OGCO2PUR(7,6,IY)*OGCO2PRCS(7,6,IY))/(OGCO2PUR(1,6,IY)+OGCO2PUR(2,6,IY)+OGCO2PUR(3,6,IY)+OGCO2PUR(4,6,IY)+OGCO2PUR(5,6,IY)+OGCO2PUR(6,6,IY)+OGCO2PUR(7,6,IY))
    # z[103] = SCALPR2*((dfd['OGCO2PUR'].loc[1].loc[6] * dfd['OGCO2PRCS'].loc[1].loc[6] + dfd['OGCO2PUR'].loc[2].loc[6] * dfd['OGCO2PRCS'].loc[2].loc[6] + dfd['OGCO2PUR'].loc[3].loc[6] * dfd['OGCO2PRCS'].loc[3].loc[6] + dfd['OGCO2PUR'].loc[4].loc[6] * dfd['OGCO2PRCS'].loc[4].loc[6] + dfd['OGCO2PUR'].loc[5].loc[6] * dfd['OGCO2PRCS'].loc[5].loc[6] + dfd['OGCO2PUR'].loc[6].loc[6] * dfd['OGCO2PRCS'].loc[6].loc[6] + dfd['OGCO2PUR'].loc[7].loc[6] * dfd['OGCO2PRCS'].loc[7].loc[6])/ (dfd['OGCO2PUR'].loc[1].loc[6] + dfd['OGCO2PUR'].loc[2].loc[6] + dfd['OGCO2PUR'].loc[3].loc[6] + dfd['OGCO2PUR'].loc[4].loc[6] + dfd['OGCO2PUR'].loc[5].loc[6] + dfd['OGCO2PUR'].loc[6].loc[6] + dfd['OGCO2PUR'].loc[7].loc[6]))
    z[103] = (
        (
            dfd["OGCO2PUR"].loc[1].loc[6] * OGCO2PRCS.loc[1].loc[6]
            + dfd["OGCO2PUR"].loc[2].loc[6] * OGCO2PRCS.loc[2].loc[6]
            + dfd["OGCO2PUR"].loc[3].loc[6] * OGCO2PRCS.loc[3].loc[6]
            + dfd["OGCO2PUR"].loc[4].loc[6] * OGCO2PRCS.loc[4].loc[6]
            + dfd["OGCO2PUR"].loc[5].loc[6] * OGCO2PRCS.loc[5].loc[6]
            + dfd["OGCO2PUR"].loc[6].loc[6] * OGCO2PRCS.loc[6].loc[6]
            + dfd["OGCO2PUR"].loc[7].loc[6] * OGCO2PRCS.loc[7].loc[6]
        )
        / (
            dfd["OGCO2PUR"].loc[1].loc[6]
            + dfd["OGCO2PUR"].loc[2].loc[6]
            + dfd["OGCO2PUR"].loc[3].loc[6]
            + dfd["OGCO2PUR"].loc[4].loc[6]
            + dfd["OGCO2PUR"].loc[5].loc[6]
            + dfd["OGCO2PUR"].loc[6].loc[6]
            + dfd["OGCO2PUR"].loc[7].loc[6]
        )
    ).fillna(0)

    #         Electric Power Plants
    # T25(104,IY,IS)=(OGCO2PUR(1,7,IY)*OGCO2PRCS(1,7,IY)+OGCO2PUR(2,7,IY)*OGCO2PRCS(2,7,IY)+OGCO2PUR(3,7,IY)*OGCO2PRCS(3,7,IY)+OGCO2PUR(4,7,IY)*OGCO2PRCS(4,7,IY)+OGCO2PUR(5,7,IY)*OGCO2PRCS(5,7,IY)+OGCO2PUR(6,7,IY)*OGCO2PRCS(6,7,IY)+OGCO2PUR(7,7,IY)*OGCO2PRCS(7,7,IY))/(OGCO2PUR(1,7,IY)+OGCO2PUR(2,7,IY)+OGCO2PUR(3,7,IY)+OGCO2PUR(4,7,IY)+OGCO2PUR(5,7,IY)+OGCO2PUR(6,7,IY)+OGCO2PUR(7,7,IY))
    # z[104] = SCALPR2*((dfd['OGCO2PUR'].loc[1].loc[7] * dfd['OGCO2PRCS'].loc[1].loc[7] + dfd['OGCO2PUR'].loc[2].loc[7] * dfd['OGCO2PRCS'].loc[2].loc[7] + dfd['OGCO2PUR'].loc[3].loc[7] * dfd['OGCO2PRCS'].loc[3].loc[7] + dfd['OGCO2PUR'].loc[4].loc[7] * dfd['OGCO2PRCS'].loc[4].loc[7] + dfd['OGCO2PUR'].loc[5].loc[7] * dfd['OGCO2PRCS'].loc[5].loc[7] + dfd['OGCO2PUR'].loc[6].loc[7] * dfd['OGCO2PRCS'].loc[6].loc[7] + dfd['OGCO2PUR'].loc[7].loc[7] * dfd['OGCO2PRCS'].loc[7].loc[7])/ (dfd['OGCO2PUR'].loc[1].loc[7] + dfd['OGCO2PUR'].loc[2].loc[7] + dfd['OGCO2PUR'].loc[3].loc[7] + dfd['OGCO2PUR'].loc[4].loc[7] + dfd['OGCO2PUR'].loc[5].loc[7] + dfd['OGCO2PUR'].loc[6].loc[7] + dfd['OGCO2PUR'].loc[7].loc[7]))
    z[104] = (
        (
            dfd["OGCO2PUR"].loc[1].loc[7] * OGCO2PRCS.loc[1].loc[7]
            + dfd["OGCO2PUR"].loc[2].loc[7] * OGCO2PRCS.loc[2].loc[7]
            + dfd["OGCO2PUR"].loc[3].loc[7] * OGCO2PRCS.loc[3].loc[7]
            + dfd["OGCO2PUR"].loc[4].loc[7] * OGCO2PRCS.loc[4].loc[7]
            + dfd["OGCO2PUR"].loc[5].loc[7] * OGCO2PRCS.loc[5].loc[7]
            + dfd["OGCO2PUR"].loc[6].loc[7] * OGCO2PRCS.loc[6].loc[7]
            + dfd["OGCO2PUR"].loc[7].loc[7] * OGCO2PRCS.loc[7].loc[7]
        )
        / (
            dfd["OGCO2PUR"].loc[1].loc[7]
            + dfd["OGCO2PUR"].loc[2].loc[7]
            + dfd["OGCO2PUR"].loc[3].loc[7]
            + dfd["OGCO2PUR"].loc[4].loc[7]
            + dfd["OGCO2PUR"].loc[5].loc[7]
            + dfd["OGCO2PUR"].loc[6].loc[7]
            + dfd["OGCO2PUR"].loc[7].loc[7]
        )
    ).fillna(0)

    #         Natural Gas Processing
    # T25(106,IY,IS)=(OGCO2PUR(1,9,IY)*OGCO2PRCS(1,9,IY)+OGCO2PUR(2,9,IY)*OGCO2PRCS(2,9,IY)+OGCO2PUR(3,9,IY)*OGCO2PRCS(3,9,IY)+OGCO2PUR(4,9,IY)*OGCO2PRCS(4,9,IY)+OGCO2PUR(5,9,IY)*OGCO2PRCS(5,9,IY)+OGCO2PUR(6,9,IY)*OGCO2PRCS(6,9,IY)+OGCO2PUR(7,9,IY)*OGCO2PRCS(7,9,IY))/(OGCO2PUR(1,9,IY)+OGCO2PUR(2,9,IY)+OGCO2PUR(3,9,IY)+OGCO2PUR(4,9,IY)+OGCO2PUR(5,9,IY)+OGCO2PUR(6,9,IY)+OGCO2PUR(7,9,IY))
    # z[106] = SCALPR2*((dfd['OGCO2PUR'].loc[1].loc[9] * dfd['OGCO2PRCS'].loc[1].loc[9] + dfd['OGCO2PUR'].loc[2].loc[9] * dfd['OGCO2PRCS'].loc[2].loc[9] + dfd['OGCO2PUR'].loc[3].loc[9] * dfd['OGCO2PRCS'].loc[3].loc[9] + dfd['OGCO2PUR'].loc[4].loc[9] * dfd['OGCO2PRCS'].loc[4].loc[9] + dfd['OGCO2PUR'].loc[5].loc[9] * dfd['OGCO2PRCS'].loc[5].loc[9] + dfd['OGCO2PUR'].loc[6].loc[9] * dfd['OGCO2PRCS'].loc[6].loc[9] + dfd['OGCO2PUR'].loc[7].loc[9] * dfd['OGCO2PRCS'].loc[7].loc[9])/ (dfd['OGCO2PUR'].loc[1].loc[9] + dfd['OGCO2PUR'].loc[2].loc[9] + dfd['OGCO2PUR'].loc[3].loc[9] + dfd['OGCO2PUR'].loc[4].loc[9] + dfd['OGCO2PUR'].loc[5].loc[9] + dfd['OGCO2PUR'].loc[6].loc[9] + dfd['OGCO2PUR'].loc[7].loc[9]))
    z[106] = (
        (
            dfd["OGCO2PUR"].loc[1].loc[9] * OGCO2PRCS.loc[1].loc[9]
            + dfd["OGCO2PUR"].loc[2].loc[9] * OGCO2PRCS.loc[2].loc[9]
            + dfd["OGCO2PUR"].loc[3].loc[9] * OGCO2PRCS.loc[3].loc[9]
            + dfd["OGCO2PUR"].loc[4].loc[9] * OGCO2PRCS.loc[4].loc[9]
            + dfd["OGCO2PUR"].loc[5].loc[9] * OGCO2PRCS.loc[5].loc[9]
            + dfd["OGCO2PUR"].loc[6].loc[9] * OGCO2PRCS.loc[6].loc[9]
            + dfd["OGCO2PUR"].loc[7].loc[9] * OGCO2PRCS.loc[7].loc[9]
        )
        / (
            dfd["OGCO2PUR"].loc[1].loc[9]
            + dfd["OGCO2PUR"].loc[2].loc[9]
            + dfd["OGCO2PUR"].loc[3].loc[9]
            + dfd["OGCO2PUR"].loc[4].loc[9]
            + dfd["OGCO2PUR"].loc[5].loc[9]
            + dfd["OGCO2PUR"].loc[6].loc[9]
            + dfd["OGCO2PUR"].loc[7].loc[9]
        )
    ).fillna(0)

    #         Coal-to-Liquids Plants
    # T25(107,IY,IS)=(OGCO2PUR(1,10,IY)*OGCO2PRCS(1,10,IY)+OGCO2PUR(2,10,IY)*OGCO2PRCS(2,10,IY)+OGCO2PUR(3,10,IY)*OGCO2PRCS(3,10,IY)+OGCO2PUR(4,10,IY)*OGCO2PRCS(4,10,IY)+OGCO2PUR(5,10,IY)*OGCO2PRCS(5,10,IY)+OGCO2PUR(6,10,IY)*OGCO2PRCS(6,10,IY)+OGCO2PUR(7,10,IY)*OGCO2PRCS(7,10,IY))/(OGCO2PUR(1,10,IY)+OGCO2PUR(2,10,IY)+OGCO2PUR(3,10,IY)+OGCO2PUR(4,10,IY)+OGCO2PUR(5,10,IY)+OGCO2PUR(6,10,IY)+OGCO2PUR(7,10,IY))
    # z[107] = SCALPR2*((dfd['OGCO2PUR'].loc[1].loc[10] * dfd['OGCO2PRCS'].loc[1].loc[10] + dfd['OGCO2PUR'].loc[2].loc[10] * dfd['OGCO2PRCS'].loc[2].loc[10] + dfd['OGCO2PUR'].loc[3].loc[10] * dfd['OGCO2PRCS'].loc[3].loc[10] + dfd['OGCO2PUR'].loc[4].loc[10] * dfd['OGCO2PRCS'].loc[4].loc[10] + dfd['OGCO2PUR'].loc[5].loc[10] * dfd['OGCO2PRCS'].loc[5].loc[10] + dfd['OGCO2PUR'].loc[6].loc[10] * dfd['OGCO2PRCS'].loc[6].loc[10] + dfd['OGCO2PUR'].loc[7].loc[10] * dfd['OGCO2PRCS'].loc[7].loc[10])/ (dfd['OGCO2PUR'].loc[1].loc[10] + dfd['OGCO2PUR'].loc[2].loc[10] + dfd['OGCO2PUR'].loc[3].loc[10] + dfd['OGCO2PUR'].loc[4].loc[10] + dfd['OGCO2PUR'].loc[5].loc[10] + dfd['OGCO2PUR'].loc[6].loc[10] + dfd['OGCO2PUR'].loc[7].loc[10]))
    z[107] = (
        (
            dfd["OGCO2PUR"].loc[1].loc[10] * OGCO2PRCS.loc[1].loc[10]
            + dfd["OGCO2PUR"].loc[2].loc[10] * OGCO2PRCS.loc[2].loc[10]
            + dfd["OGCO2PUR"].loc[3].loc[10] * OGCO2PRCS.loc[3].loc[10]
            + dfd["OGCO2PUR"].loc[4].loc[10] * OGCO2PRCS.loc[4].loc[10]
            + dfd["OGCO2PUR"].loc[5].loc[10] * OGCO2PRCS.loc[5].loc[10]
            + dfd["OGCO2PUR"].loc[6].loc[10] * OGCO2PRCS.loc[6].loc[10]
            + dfd["OGCO2PUR"].loc[7].loc[10] * OGCO2PRCS.loc[7].loc[10]
        )
        / (
            dfd["OGCO2PUR"].loc[1].loc[10]
            + dfd["OGCO2PUR"].loc[2].loc[10]
            + dfd["OGCO2PUR"].loc[3].loc[10]
            + dfd["OGCO2PUR"].loc[4].loc[10]
            + dfd["OGCO2PUR"].loc[5].loc[10]
            + dfd["OGCO2PUR"].loc[6].loc[10]
            + dfd["OGCO2PUR"].loc[7].loc[10]
        )
    ).fillna(0)

    #      Average, Anthropogenic Sources
    # T25(111,IY,IS)=(SUM(OGCO2PUR(1:7,1,IY))*T25(98,IY,IS)+SUM(OGCO2PUR(1:7,2,IY))*T25(99,IY,IS)+SUM(OGCO2PUR(1:7,3,IY))*T25(100,IY,IS)+SUM(OGCO2PUR(1:7,5,IY))*T25(102,IY,IS)+SUM(OGCO2PUR(1:7,6,IY))*T25(103,IY,IS)+SUM(OGCO2PUR(1:7,7,IY))*T25(104,IY,IS)+SUM(OGCO2PUR(1:7,9,IY))*T25(106,IY,IS)+SUM(OGCO2PUR(1:7,10,IY))*T25(107,IY,IS))/(SUM(OGCO2PUR(1:7,1:3,IY))+SUM(OGCO2PUR(1:7,5:7,IY))+SUM(OGCO2PUR(1:7,9:10,IY)))
    # z[111] = SCALPR2*(((dfd['OGCO2PUR'].loc[1:7].loc[1].sum()*z[98] + dfd['OGCO2PUR'].loc[((1:7),2)),:].sum()*z[99] + dfd['OGCO2PUR'].loc[((1:7),3)),:].sum()*z[100] + dfd['OGCO2PUR'].loc[((1:7),5)),:].sum()*z[102] + dfd['OGCO2PUR'].loc[((1:7),6)),:].sum()*z[103] + dfd['OGCO2PUR'].loc[((1:7),7)),:].sum()*z[104] + dfd['OGCO2PUR'].loc[((1:7),9)),:].sum()*z[106] + dfd['OGCO2PUR'].loc[((1:7),10)),:].sum()*z[107])/(dfd['OGCO2PUR'].loc[((1:7),(1:3)),:].sum() + dfd['OGCO2PUR'].loc[((1:7),(5:7)),:].sum() + dfd['OGCO2PUR'].loc[((1:7),(9:10)),:].sum()))
    # z[111] = SCALPR2*((dfd['OGCO2PUR'].loc[1:7,1,:].sum()*z[98] + dfd['OGCO2PUR'].loc[1:7,2,:].sum()*z[99] + dfd['OGCO2PUR'].loc[1:7,3,:].sum()*z[100] + dfd['OGCO2PUR'].loc[1:7,5,:].sum()*z[102] + dfd['OGCO2PUR'].loc[1:7,6,:].sum()*z[103] + dfd['OGCO2PUR'].loc[1:7,7,:].sum()*z[104] + dfd['OGCO2PUR'].loc[1:7,9,:].sum()*z[106] + dfd['OGCO2PUR'].loc[1:7,10,:].sum()*z[107])/(dfd['OGCO2PUR'].loc[1:7,1:3,:].sum() + dfd['OGCO2PUR'].loc[1:7,5:7,:].sum() + dfd['OGCO2PUR'].loc[1:7,9:10,:].sum()))
    z[111] = (
        (
            dfd["OGCO2PUR"].loc[1:7, 1, :].sum() * z[98]
            + dfd["OGCO2PUR"].loc[1:7, 2, :].sum() * z[99]
            + dfd["OGCO2PUR"].loc[1:7, 3, :].sum() * z[100]
            + dfd["OGCO2PUR"].loc[1:7, 5, :].sum() * z[102]
            + dfd["OGCO2PUR"].loc[1:7, 6, :].sum() * z[103]
            + dfd["OGCO2PUR"].loc[1:7, 7, :].sum() * z[104]
            + dfd["OGCO2PUR"].loc[1:7, 9, :].sum() * z[106]
            + dfd["OGCO2PUR"].loc[1:7, 10, :].sum() * z[107]
        )
        / (
            dfd["OGCO2PUR"].loc[1:7, 1:3, :].sum()
            + dfd["OGCO2PUR"].loc[1:7, 5:7, :].sum()
            + dfd["OGCO2PUR"].loc[1:7, 9:10, :].sum()
        )
    ).fillna(0)

    #         Natural
    # T25(101,IY,IS)=(OGCO2PUR(1,4,IY)*OGCO2PRCS(1,4,IY)+OGCO2PUR(2,4,IY)*OGCO2PRCS(2,4,IY)+OGCO2PUR(3,4,IY)*OGCO2PRCS(3,4,IY)+OGCO2PUR(4,4,IY)*OGCO2PRCS(4,4,IY)+OGCO2PUR(5,4,IY)*OGCO2PRCS(5,4,IY)+OGCO2PUR(6,4,IY)*OGCO2PRCS(6,4,IY)+OGCO2PUR(7,4,IY)*OGCO2PRCS(7,4,IY))/(OGCO2PUR(1,4,IY)+OGCO2PUR(2,4,IY)+OGCO2PUR(3,4,IY)+OGCO2PUR(4,4,IY)+OGCO2PUR(5,4,IY)+OGCO2PUR(6,4,IY)+OGCO2PUR(7,4,IY))
    # z[101] = SCALPR2*((dfd['OGCO2PUR'].loc[1].loc[4] * dfd['OGCO2PRCS'].loc[1].loc[4] + dfd['OGCO2PUR'].loc[2].loc[4] * dfd['OGCO2PRCS'].loc[2].loc[4] + dfd['OGCO2PUR'].loc[3].loc[4] * dfd['OGCO2PRCS'].loc[3].loc[4] + dfd['OGCO2PUR'].loc[4].loc[4] * dfd['OGCO2PRCS'].loc[4].loc[4] + dfd['OGCO2PUR'].loc[5].loc[4] * dfd['OGCO2PRCS'].loc[5].loc[4] + dfd['OGCO2PUR'].loc[6].loc[4] * dfd['OGCO2PRCS'].loc[6].loc[4] + dfd['OGCO2PUR'].loc[7].loc[4] * dfd['OGCO2PRCS'].loc[7].loc[4])/ (dfd['OGCO2PUR'].loc[1].loc[4] + dfd['OGCO2PUR'].loc[2].loc[4] + dfd['OGCO2PUR'].loc[3].loc[4] + dfd['OGCO2PUR'].loc[4].loc[4] + dfd['OGCO2PUR'].loc[5].loc[4] + dfd['OGCO2PUR'].loc[6].loc[4] + dfd['OGCO2PUR'].loc[7].loc[4]))
    z[101] = (
        (
            dfd["OGCO2PUR"].loc[1].loc[4] * OGCO2PRCS.loc[1].loc[4]
            + dfd["OGCO2PUR"].loc[2].loc[4] * OGCO2PRCS.loc[2].loc[4]
            + dfd["OGCO2PUR"].loc[3].loc[4] * OGCO2PRCS.loc[3].loc[4]
            + dfd["OGCO2PUR"].loc[4].loc[4] * OGCO2PRCS.loc[4].loc[4]
            + dfd["OGCO2PUR"].loc[5].loc[4] * OGCO2PRCS.loc[5].loc[4]
            + dfd["OGCO2PUR"].loc[6].loc[4] * OGCO2PRCS.loc[6].loc[4]
            + dfd["OGCO2PUR"].loc[7].loc[4] * OGCO2PRCS.loc[7].loc[4]
        )
        / (
            dfd["OGCO2PUR"].loc[1].loc[4]
            + dfd["OGCO2PUR"].loc[2].loc[4]
            + dfd["OGCO2PUR"].loc[3].loc[4]
            + dfd["OGCO2PUR"].loc[4].loc[4]
            + dfd["OGCO2PUR"].loc[5].loc[4]
            + dfd["OGCO2PUR"].loc[6].loc[4]
            + dfd["OGCO2PUR"].loc[7].loc[4]
        )
    ).fillna(0)

    #      Average, All Sources
    # T25(112,IY,IS)=(SUM(OGCO2PUR(1:7,1,IY))*T25(98,IY,IS)+SUM(OGCO2PUR(1:7,2,IY))*T25(99,IY,IS)+SUM(OGCO2PUR(1:7,3,IY))*T25(100,IY,IS)+SUM(OGCO2PUR(1:7,4,IY))*T25(101,IY,IS)+SUM(OGCO2PUR(1:7,5,IY))*T25(102,IY,IS)+SUM(OGCO2PUR(1:7,6,IY))*T25(103,IY,IS)+SUM(OGCO2PUR(1:7,7,IY))*T25(104,IY,IS)+SUM(OGCO2PUR(1:7,9,IY))*T25(106,IY,IS)+SUM(OGCO2PUR(1:7,10,IY))*T25(107,IY,IS))/(SUM(OGCO2PUR(1:7,1:7,IY))+SUM(OGCO2PUR(1:7,9:10,IY)))
    # z[112] = SCALPR2*((dfd['OGCO2PUR'].loc[1:7,1,:].sum()*z[98] + dfd['OGCO2PUR'].loc[1:7,2,:].sum()*z[99] + dfd['OGCO2PUR'].loc[1:7,3,:].sum()*z[100] + dfd['OGCO2PUR'].loc[1:7,4,:].sum()*z[101] + dfd['OGCO2PUR'].loc[1:7,5,:].sum()*z[102] + dfd['OGCO2PUR'].loc[1:7,6,:].sum()*z[103] + dfd['OGCO2PUR'].loc[1:7,7,:].sum()*z[104] + dfd['OGCO2PUR'].loc[1:7,9,:].sum()*z[106] + dfd['OGCO2PUR'].loc[1:7,10,:].sum()*z[107])/(dfd['OGCO2PUR'].loc[1:7,1:7,:].sum() + dfd['OGCO2PUR'].loc[1:7,9:10,:].sum()))
    z[112] = (
        (
            dfd["OGCO2PUR"].loc[1:7, 1, :].sum() * z[98]
            + dfd["OGCO2PUR"].loc[1:7, 2, :].sum() * z[99]
            + dfd["OGCO2PUR"].loc[1:7, 3, :].sum() * z[100]
            + dfd["OGCO2PUR"].loc[1:7, 4, :].sum() * z[101]
            + dfd["OGCO2PUR"].loc[1:7, 5, :].sum() * z[102]
            + dfd["OGCO2PUR"].loc[1:7, 6, :].sum() * z[103]
            + dfd["OGCO2PUR"].loc[1:7, 7, :].sum() * z[104]
            + dfd["OGCO2PUR"].loc[1:7, 9, :].sum() * z[106]
            + dfd["OGCO2PUR"].loc[1:7, 10, :].sum() * z[107]
        )
        / (
            dfd["OGCO2PUR"].loc[1:7, 1:7, :].sum()
            + dfd["OGCO2PUR"].loc[1:7, 9:10, :].sum()
        )
    ).fillna(0)

    return z
