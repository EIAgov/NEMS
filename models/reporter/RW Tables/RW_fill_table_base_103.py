# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
SZO fixed offset proces on 9/4/2024
"""


def fill_table_base_103(dfd, table_spec, table_id):
    """Fill table for  Renewable and Low Carbon Fuel Standard Items

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

    #   Renewable and Low Carbon Fuel Standard Items
    #    Credits, Credit Prices, and LCFS Information
    #   Renewable Fuels Standard Prices
    #   (#### dollars per gallon)
    #
    #   RFS Mandates by Category (billion credits)

    #     Total
    # T103(9,IY,IS)=RFSMANDATES(1,IY)
    z[9] = dfd["RFSMANDATES"].loc[1]

    #     Advanced
    # T103(10,IY,IS)=RFSMANDATES(2,IY)
    z[10] = dfd["RFSMANDATES"].loc[2]

    #     Cellulosic
    # T103(11,IY,IS)=RFSMANDATES(3,IY)
    z[11] = dfd["RFSMANDATES"].loc[3]

    #     Biodiesel
    # T103(12,IY,IS)=RFSMANDATES(4,IY)
    z[12] = dfd["RFSMANDATES"].loc[4]

    #

    #   Credits by Category (billion credits)

    #     Total Credits

    #       Corn Ethanol
    # T103(13,IY,IS)=RFSCREDITS(1,1,IY)
    z[13] = dfd["RFSCREDITS"].loc[1, 1]

    #       Advanced Ethanol
    # T103(14,IY,IS)=RFSCREDITS(1,2,IY)
    z[14] = dfd["RFSCREDITS"].loc[1, 2]

    #       Cellulosic Ethanol
    # T103(15,IY,IS)=RFSCREDITS(1,3,IY)
    z[15] = dfd["RFSCREDITS"].loc[1, 3]

    #       Imported Ethanol
    # T103(16,IY,IS)=RFSCREDITS(1,4,IY)
    z[16] = dfd["RFSCREDITS"].loc[1, 4]

    #       Biodiesel + Imports
    # T103(17,IY,IS)=RFSCREDITS(1,5,IY)
    z[17] = dfd["RFSCREDITS"].loc[1, 5]

    #       Renewable Diesel + Imports
    # T103(18,IY,IS)=RFSCREDITS(1,6,IY)
    z[18] = dfd["RFSCREDITS"].loc[1, 6]

    #       Renewable Gasoline
    # T103(19,IY,IS)=RFSCREDITS(1,7,IY)
    z[19] = dfd["RFSCREDITS"].loc[1, 7]

    #       BTL-FT
    # T103(20,IY,IS)=RFSCREDITS(1,8,IY)
    z[20] = dfd["RFSCREDITS"].loc[1, 8]

    #       BTL-Pyrolysis
    # T103(21,IY,IS)=RFSCREDITS(1,9,IY)
    z[21] = dfd["RFSCREDITS"].loc[1, 9]

    #       CBTL
    # T103(22,IY,IS)=RFSCREDITS(1,10,IY)
    z[22] = dfd["RFSCREDITS"].loc[1, 10]

    #       Biobutanol
    # T103(23,IY,IS)=RFSCREDITS(1,12,IY)
    z[23] = dfd["RFSCREDITS"].loc[1, 12]

    #       Shortfall
    # T103(24,IY,IS)=RFSCREDITS(1,11,IY)
    z[24] = dfd["RFSCREDITS"].loc[1, 11]

    #         Total
    # T103(25,IY,IS)=RFSCREDITS(1,13,IY)
    z[25] = dfd["RFSCREDITS"].loc[1, 13]

    #       Expected Shortfall
    # T103(26,IY,IS)=RFSSAFETY(1,IY)
    z[26] = dfd["RFSSAFETY"].loc[1]

    #

    #     Advanced Credits

    #       Advanced Ethanol
    # T103(27,IY,IS)=RFSCREDITS(2,2,IY)
    z[27] = dfd["RFSCREDITS"].loc[2, 2]

    #       Cellulosic Ethanol
    # T103(28,IY,IS)=RFSCREDITS(2,3,IY)
    z[28] = dfd["RFSCREDITS"].loc[2, 3]

    #       Imported Ethanol
    # T103(29,IY,IS)=RFSCREDITS(2,4,IY)
    z[29] = dfd["RFSCREDITS"].loc[2, 4]

    #       Biodiesel + Imports
    # T103(30,IY,IS)=RFSCREDITS(2,5,IY)
    z[30] = dfd["RFSCREDITS"].loc[2, 5]

    #       Renewable Diesel + Imports
    # T103(31,IY,IS)=RFSCREDITS(2,6,IY)
    z[31] = dfd["RFSCREDITS"].loc[2, 6]

    #       Renewable Gasoline
    # T103(32,IY,IS)=RFSCREDITS(2,7,IY)
    z[32] = dfd["RFSCREDITS"].loc[2, 7]

    #       BTL-FT
    # T103(33,IY,IS)=RFSCREDITS(2,8,IY)
    z[33] = dfd["RFSCREDITS"].loc[2, 8]

    #       BTL-Pyrolysis
    # T103(34,IY,IS)=RFSCREDITS(2,9,IY)
    z[34] = dfd["RFSCREDITS"].loc[2, 9]

    #       CBTL
    # T103(35,IY,IS)=RFSCREDITS(2,10,IY)
    z[35] = dfd["RFSCREDITS"].loc[2, 10]

    #       Shortfall
    # T103(37,IY,IS)=RFSCREDITS(2,11,IY)
    z[37] = dfd["RFSCREDITS"].loc[2, 11]

    #         Total
    # T103(38,IY,IS)=RFSCREDITS(2,13,IY)
    z[38] = dfd["RFSCREDITS"].loc[2, 13]

    #

    #     Cellulosic Credits

    #       Cellulosic Ethanol
    # T103(40,IY,IS)=RFSCREDITS(3,3,IY)
    z[40] = dfd["RFSCREDITS"].loc[3, 3]

    #       BTL-FT
    # T103(41,IY,IS)=RFSCREDITS(3,8,IY)
    z[41] = dfd["RFSCREDITS"].loc[3, 8]

    #       BTL-Pyrolysis
    # T103(42,IY,IS)=RFSCREDITS(3,9,IY)
    z[42] = dfd["RFSCREDITS"].loc[3, 9]

    #       CBTL
    # T103(43,IY,IS)=RFSCREDITS(3,10,IY)
    z[43] = dfd["RFSCREDITS"].loc[3, 10]

    #       Shortfall
    # T103(44,IY,IS)=RFSCREDITS(3,11,IY)
    z[44] = dfd["RFSCREDITS"].loc[3, 11]

    #         Total
    # T103(45,IY,IS)=RFSCREDITS(3,13,IY)
    z[45] = dfd["RFSCREDITS"].loc[3, 13]

    #

    #     Nonadvanced Credits
    # T103(47,IY,IS)=RFSCREDITS(1,13,IY)-RFSCREDITS(2,13,IY)
    z[47] = dfd["RFSCREDITS"].loc[1, 13] - dfd["RFSCREDITS"].loc[2, 13]

    #

    #     Biodiesel Credits

    #       Biodiesel + Imports
    # T103(48,IY,IS)=RFSCREDITS(4,5,IY)
    z[48] = dfd["RFSCREDITS"].loc[4, 5]

    #       Renewable Diesel + Imports
    # T103(49,IY,IS)=RFSCREDITS(4,6,IY)
    z[49] = dfd["RFSCREDITS"].loc[4, 6]

    #       Shortfall
    # T103(50,IY,IS)=RFSCREDITS(4,11,IY)
    z[50] = dfd["RFSCREDITS"].loc[4, 11]

    #         Total
    # T103(51,IY,IS)=RFSCREDITS(4,13,IY)
    z[51] = dfd["RFSCREDITS"].loc[4, 13]

    #

    #   LCFS Results - Motor Gas

    #     Baseline Carbon Factor (Mton C02/trill)
    # T103(53,IY,IS)=LCFS_BASELINE(1,IY)
    z[53] = dfd["LCFS_BASELINE"].loc[1]

    #     Actual Carbon Factor (Mton C02/trill)
    # T103(54,IY,IS)=LCFS_ACTUAL(1,IY)
    z[54] = dfd["LCFS_ACTUAL"].loc[1]

    #     Safety Valve (Mtons C02)
    # T103(55,IY,IS)=LCFS_WAIVER(1,IY)
    z[55] = dfd["LCFS_WAIVER"].loc[1]

    #     Offset Price (thousand #### dollars / ton C02)
    # T103(56,IY,IS)=LCFS_OFFSET_PRC(1,IY)
    z[56] = dfd["LCFS_OFFSET_PRC"].loc[1] * SCALPR2 / 1000.0

    #     Carbon Offset by Biofuels (MMton C02)
    # T103(57,IY,IS)=LCFS_CARB_OFFSET(1,IY)
    z[57] = dfd["LCFS_CARB_OFFSET"].loc[1]

    #     Transportation CO2 Emissions (MMton C02)
    # T103(58,IY,IS)=LCFS_PETOTRILLS(1,IY)*LCFS_ACTUAL(1,IY)/1000.
    z[58] = dfd["LCFS_PETOTRILLS"].loc[1] * dfd["LCFS_ACTUAL"].loc[1] / 1000.0

    #     Transportation Consumption (quadrillion Btu)
    # T103(59,IY,IS)=LCFS_PETOTRILLS(1,IY)/1000.
    z[59] = dfd["LCFS_PETOTRILLS"].loc[1] / 1000.0

    #

    #   LCFS Results - Diesel

    #     Baseline Carbon Factor (Mton C02/trill)
    # T103(60,IY,IS)=LCFS_BASELINE(2,IY)
    z[60] = dfd["LCFS_BASELINE"].loc[2]

    #     Actual Carbon Factor (Mton C02/trill)
    # T103(61,IY,IS)=LCFS_ACTUAL(2,IY)
    z[61] = dfd["LCFS_ACTUAL"].loc[2]

    #     Safety Valve (Mtons C02)
    # T103(62,IY,IS)=LCFS_WAIVER(2,IY)
    z[62] = dfd["LCFS_WAIVER"].loc[2]

    #     Offset Price (thousand #### dollars / ton C02)
    # T103(63,IY,IS)=LCFS_OFFSET_PRC(2,IY)
    # LCFS_Offset_Prc = LCFS_Offset_Prc * SCALPR / 1000.
    z[63] = dfd["LCFS_OFFSET_PRC"].loc[2] * SCALPR2 / 1000.0

    #     Carbon Offset by Biofuels (MMton C02)
    # T103(64,IY,IS)=LCFS_CARB_OFFSET(2,IY)
    z[64] = dfd["LCFS_CARB_OFFSET"].loc[2]

    #     Transportation CO2 Emissions (MMton C02)
    # T103(65,IY,IS)=LCFS_PETOTRILLS(2,IY)*LCFS_ACTUAL(2,IY)/1000.
    z[65] = dfd["LCFS_PETOTRILLS"].loc[2] * dfd["LCFS_ACTUAL"].loc[2] / 1000.0

    #     Transportation Consumption (quadrillion Btu)
    # T103(66,IY,IS)=LCFS_PETOTRILLS(2,IY)/1000.
    z[66] = dfd["LCFS_PETOTRILLS"].loc[2] / 1000.0

    #
    #   CFP Results - Motor Gas

    #     Baseline Carbon Factor (Mton C02/trill)
    # T103(67,IY,IS)=CFP_BASELINE(1,IY)
    z[67] = dfd["CFP_BASELINE"].loc[1]

    #     Actual Carbon Factor (Mton C02/trill)
    # T103(68,IY,IS)=CFP_ACTUAL(1,IY)
    z[68] = dfd["CFP_ACTUAL"].loc[1]

    #     Safety Valve (Mtons C02)
    # T103(69,IY,IS)=CFP_WAIVER(1,IY)
    z[69] = dfd["CFP_WAIVER"].loc[1]

    #     Offset Price (thousand #### dollars / ton C02)
    # T103(70,IY,IS)=CFP_OFFSET_PRC(1,IY)
    z[70] = dfd["CFP_OFFSET_PRC"].loc[1]  # * SCALPR2

    #     Carbon Offset by Biofuels (MMton C02)
    # T103(71,IY,IS)=CFP_CARB_OFFSET(1,IY)
    z[71] = dfd["CFP_CARB_OFFSET"].loc[1]

    #     Transportation CO2 Emissions (MMton C02)
    # T103(72,IY,IS)=CFP_PETOTRILLS(1,IY)*CFP_ACTUAL(1,IY)/1000.
    z[72] = dfd["CFP_PETOTRILLS"].loc[1] * dfd["CFP_ACTUAL"].loc[1] / 1000.0

    #     Transportation Consumption (quadrillion Btu)
    # T103(73,IY,IS)=CFP_PETOTRILLS(1,IY)/1000.
    z[73] = dfd["CFP_PETOTRILLS"].loc[1] / 1000.0

    #

    #   CFP Results - Diesel

    #     Baseline Carbon Factor (Mton C02/trill)
    # T103(74,IY,IS)=CFP_BASELINE(2,IY)
    z[74] = dfd["CFP_BASELINE"].loc[2]

    #     Actual Carbon Factor (Mton C02/trill)
    # T103(75,IY,IS)=CFP_ACTUAL(2,IY)
    z[75] = dfd["CFP_ACTUAL"].loc[2]

    #     Safety Valve (Mtons C02)
    # T103(76,IY,IS)=CFP_WAIVER(2,IY)
    z[76] = dfd["CFP_WAIVER"].loc[2]

    #     Offset Price (thousand #### dollars / ton C02)
    # T103(77,IY,IS)=CFP_OFFSET_PRC(2,IY)
    z[77] = dfd["CFP_OFFSET_PRC"].loc[2]  # * SCALPR2

    #     Carbon Offset by Biofuels (MMton C02)
    # T103(78,IY,IS)=CFP_CARB_OFFSET(2,IY)
    z[78] = dfd["CFP_CARB_OFFSET"].loc[2]

    #     Transportation CO2 Emissions (MMton C02)
    # T103(79,IY,IS)=CFP_PETOTRILLS(2,IY)*CFP_ACTUAL(2,IY)/1000.
    z[79] = dfd["CFP_PETOTRILLS"].loc[2] * dfd["CFP_ACTUAL"].loc[2] / 1000.0

    #     Transportation Consumption (quadrillion Btu)
    # T103(80,IY,IS)=CFP_PETOTRILLS(2,IY)/1000.
    z[80] = dfd["CFP_PETOTRILLS"].loc[2] / 1000.0

    return z
