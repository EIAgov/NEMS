# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
SZO fixed on 8/9/2024
Fixed Energy Consumption (trillion Btu)/Manufacturing Sector/Petroleum Refineries on 8/202024
"""

import numpy as np
import pandas as pd


def fill_table_base_034(dfd, table_spec, table_id):
    """Fill table   Industrial Sector Macroeconomic Indicators

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

    RDAYS = dfd["RDAYS_rwpre"]
    MNUMCR = dfd["MNUMCR_rwpre"]
    MNUMPR = dfd["MNUMPR_rwpre"]

    #  Note: CRfromCD(11) / 1, 1, 2, 2, 3, 3, 3, 4, 4, 5, 5/ maps census divisions to regions

    #   Industrial Sector Macroeconomic Indicators
    #    Indicators

    #   Nonfarm Employment (millions)
    # T34(2,IR,IY,IS)=SUM(MC_EMPNA(IR,1:19,IY))+SUM(MC_EMPNA(IR,22:39,IY))
    # z[2] = dfd['MC_EMPNA'].loc[1:19,:].sum() + dfd['MC_EMPNA'].loc[22:39,:].sum()
    z[2] = (
        dfd["MC_EMPNA"].loc[(slice(None), range(1, 19 + 1)), :].groupby(level=0).sum()
        + dfd["MC_EMPNA"]
        .loc[(slice(None), range(22, 39 + 1)), :]
        .groupby(level=0)
        .sum()
    )

    #   Gross Domestic Product (billion m__m dollars)
    # T34( 1,IR,IY,IS) =  MC_GDPR(IY)
    z1 = z[2].copy()
    z1.iloc[:] = 1
    z[1] = z1 * dfd["MC_GDPR"].values

    #   Value of Shipments (billion m==m dollars)
    #   Nonmanufacturing Sector

    #     Agriculture/Forestry/Fishing/Hunting

    """ Swap index of dfd['MC_REVIND'] to make it easier to code the following equations.
        
        Before swap:
            index_level = 0: MNUMCR: 1:11	
            index_level = 1: MNSICM: 1:60
        
        Swap:
            dfd['MC_REVIND'] = dfd['MC_REVIND'].swaplevel('MNUMCR', 'MNSICM')

        After swap:
            index_level = 0: MNSICM: 1:60	
            index_level = 1: MNUMCR: 1:11   
    """

    dfd["MC_REVIND"] = dfd["MC_REVIND"].swaplevel("MNUMCR", "MNSICM")

    # T34(3,IR,IY,IS)=MC_REVIND(IR,42,IY)+MC_REVIND(IR,43,IY)+MC_REVIND(IR,44,IY)
    z[3] = (
        dfd["MC_REVIND"].loc[42] + dfd["MC_REVIND"].loc[43] + dfd["MC_REVIND"].loc[44]
    )

    #     Mining
    # T34(4,IR,IY,IS)=MC_REVIND(IR,45,IY)+MC_REVIND(IR,46,IY)+MC_REVIND(IR,47,IY)
    z[4] = (
        dfd["MC_REVIND"].loc[45] + dfd["MC_REVIND"].loc[46] + dfd["MC_REVIND"].loc[47]
    )

    #       Coal Mining
    # T34(98,IR,IY,IS)=MC_REVIND(IR,45,IY)
    z[98] = dfd["MC_REVIND"].loc[45]

    #       Oil and Gas Extraction
    # T34(99,IR,IY,IS)=MC_REVIND(IR,46,IY)
    z[99] = dfd["MC_REVIND"].loc[46]

    #       Other Mining
    # T34(100,IR,IY,IS)=MC_REVIND(IR,47,IY)
    z[100] = dfd["MC_REVIND"].loc[47]

    #     Construction
    # T34(5,IR,IY,IS)=MC_REVIND(IR,48,IY)
    z[5] = dfd["MC_REVIND"].loc[48]

    #

    #   Manufacturing Sector

    #     Food Products
    # T34(6,IR,IY,IS)=MC_REVIND(IR,1,IY)
    z[6] = dfd["MC_REVIND"].loc[1]

    #       Grain and Oil Seed Milling
    # T34(108,IR,IY,IS)=MC_REVIND(IR,2,IY)
    z[108] = dfd["MC_REVIND"].loc[2]

    #       Dairy Products
    # T34(64,IR,IY,IS)=MC_REVIND(IR,3,IY)
    z[64] = dfd["MC_REVIND"].loc[3]

    #       Animal Slaughter and Seafood Products
    # T34(65,IR,IY,IS)=MC_REVIND(IR,4,IY)
    z[65] = dfd["MC_REVIND"].loc[4]

    #       Other
    # T34(66,IR,IY,IS)=MC_REVIND(IR,5,IY)
    z[66] = dfd["MC_REVIND"].loc[5]

    #     Beverages and Tobacco Products
    # T34(7,IR,IY,IS)=MC_REVIND(IR,6,IY)
    z[7] = dfd["MC_REVIND"].loc[6]

    #     Textile Mills and Products
    # T34(8,IR,IY,IS)=MC_REVIND(IR,7,IY)
    z[8] = dfd["MC_REVIND"].loc[7]

    #     Wood Products
    # T34(9,IR,IY,IS)=MC_REVIND(IR,8,IY)
    z[9] = dfd["MC_REVIND"].loc[8]

    #     Furniture and Related Products
    # T34(10,IR,IY,IS)=MC_REVIND(IR,9,IY)
    z[10] = dfd["MC_REVIND"].loc[9]

    #     Paper Products
    # T34(11,IR,IY,IS)=MC_REVIND(IR,10,IY)
    z[11] = dfd["MC_REVIND"].loc[10]

    #       Pulp and Paper Mills
    # T34(95,IR,IY,IS)=MC_REVIND(IR,11,IY)
    z[95] = dfd["MC_REVIND"].loc[11]

    #       Paperboard Container
    # T34(96,IR,IY,IS)=MC_REVIND(IR,12,IY)
    z[96] = dfd["MC_REVIND"].loc[12]

    #       Other Paper
    # T34(97,IR,IY,IS)=MC_REVIND(IR,13,IY)
    z[97] = dfd["MC_REVIND"].loc[13]

    #     Printing
    # T34(12,IR,IY,IS)=MC_REVIND(IR,14,IY)
    z[12] = dfd["MC_REVIND"].loc[14]

    #     Chemical Manufacturing
    # T34(13,IR,IY,IS)=MC_REVIND(IR,15,IY)+MC_REVIND(IR,16,IY)+MC_REVIND(IR,18,IY)+MC_REVIND(IR,19,IY)+MC_REVIND(IR,20,IY)
    z[13] = (
        dfd["MC_REVIND"].loc[15]
        + dfd["MC_REVIND"].loc[16]
        + dfd["MC_REVIND"].loc[18]
        + dfd["MC_REVIND"].loc[19]
        + dfd["MC_REVIND"].loc[20]
    )

    #       Bulk Chemicals
    # T34(24,IR,IY,IS)=MC_REVIND(IR,15,IY)+MC_REVIND(IR,16,IY)+MC_REVIND(IR,18,IY)+MC_REVIND(IR,19,IY)
    z[24] = (
        dfd["MC_REVIND"].loc[15]
        + dfd["MC_REVIND"].loc[16]
        + dfd["MC_REVIND"].loc[18]
        + dfd["MC_REVIND"].loc[19]
    )

    #         Inorganic
    # T34(37,IR,IY,IS)=MC_REVIND(IR,15,IY)
    z[37] = dfd["MC_REVIND"].loc[15]

    #           Industrial Gas Manufacturing
    # T34(109,IY,IS)=MC_REVIND(11,51,IY)
    # z[109] = dfd['MC_REVIND'].loc[11].loc[51]
    z[109] = dfd["MC_REVIND"].loc[51].loc[11]

    #           Synthetic Dye and Pigment/Other
    # T34(110,IY,IS)=MC_REVIND(11,52,IY)
    # z[110] = dfd['MC_REVIND'].loc[11].loc[52]
    z[110] = dfd["MC_REVIND"].loc[52].loc[11]

    #         Organic
    # T34(38,IR,IY,IS)=MC_REVIND(IR,16,IY)
    z[38] = dfd["MC_REVIND"].loc[16]

    #           Petrochemical Manufacturing
    # T34(111,IY,IS)=MC_REVIND(11,49,IY)
    # z[111] = dfd['MC_REVIND'].loc[11].loc[49]
    z[111] = dfd["MC_REVIND"].loc[49].loc[11]

    #           Ethanol
    # T34(34,IR,IY,IS)=MC_REVIND(IR,17,IY)
    z[34] = dfd["MC_REVIND"].loc[17]

    #           All Other Basic Organic Chem Man
    # T34(112,IY,IS)=MC_REVIND(11,50,IY)
    # z[112] = dfd['MC_REVIND'].loc[11].loc[50]
    z[112] = dfd["MC_REVIND"].loc[50].loc[11]

    #         Resin, Synthetic Rubber, and Fibers
    # T34(39,IR,IY,IS)=MC_REVIND(IR,18,IY)
    z[39] = dfd["MC_REVIND"].loc[18]

    #         Agricultural Chemicals
    # T34(40,IR,IY,IS)=MC_REVIND(IR,19,IY)
    z[40] = dfd["MC_REVIND"].loc[19]

    #       Other Chemical Products
    # T34(25,IR,IY,IS)=MC_REVIND(IR,20,IY)
    z[25] = dfd["MC_REVIND"].loc[20]

    #         Pharma Products
    # T34(67,IR,IY,IS)=MC_REVIND(IR,21,IY)
    z[67] = dfd["MC_REVIND"].loc[21]

    #         Paint Products
    # T34(68,IR,IY,IS)=MC_REVIND(IR,22,IY)
    z[68] = dfd["MC_REVIND"].loc[22]

    #         Soaps and Cleaning Products
    # T34(69,IR,IY,IS)=MC_REVIND(IR,23,IY)
    z[69] = dfd["MC_REVIND"].loc[23]

    #         Other
    # T34(70,IR,IY,IS)=MC_REVIND(IR,24,IY)
    z[70] = dfd["MC_REVIND"].loc[24]

    #     Petroleum and Coal Products
    #  T34(14,IR,IY,IS) = MC_REVIND(IR,25,IY) + MC_REVIND(IR,26,IY) * 0.91
    z[14] = dfd["MC_REVIND"].loc[25] + dfd["MC_REVIND"].loc[26] * 0.91

    #       Petroleum Refineries
    # T34(26,IR,IY,IS)=MC_REVIND(IR,25,IY)
    z[26] = dfd["MC_REVIND"].loc[25]

    #       Other Petroleum and Coal Products
    # T34(27,IR,IY,IS) = MC_REVIND(IR,26,IY)*0.91
    z[27] = dfd["MC_REVIND"].loc[26] * 0.91

    #     Plastics and Rubber Products
    # T34(15,IR,IY,IS)=MC_REVIND(IR,27,IY)
    z[15] = dfd["MC_REVIND"].loc[27]

    #     Non-metallic minerals

    # T34(16,IR,IY,IS) = MC_REVIND(IR,28,IY) + MC_REVIND(IR,30,IY) + MC_REVIND(IR,31,IY)+ MC_REVIND(IR,32,IY)
    z[16] = (
        dfd["MC_REVIND"].loc[28]
        + dfd["MC_REVIND"].loc[30]
        + dfd["MC_REVIND"].loc[31]
        + dfd["MC_REVIND"].loc[32]
    )

    #       Glass and Glass Products
    # T34(28,IR,IY,IS)=MC_REVIND(IR,28,IY)
    z[28] = dfd["MC_REVIND"].loc[28]

    #         Flat Glass
    # T34(35,IR,IY,IS)=MC_REVIND(IR,29,IY)
    z[35] = dfd["MC_REVIND"].loc[29]

    #         Container Glass
    # T34(113,IY,IS)=MC_REVIND(11,53,IY)
    # z[113] = dfd['MC_REVIND'].loc[11].loc[53]
    z[113] = dfd["MC_REVIND"].loc[53].loc[11]

    #         Manufac Excl. Flat and Contain Glass
    # T34(114,IY,IS)=MC_REVIND(11,54,IY)
    # z[114] = dfd['MC_REVIND'].loc[11].loc[54]
    z[114] = dfd["MC_REVIND"].loc[54].loc[11]

    #         Cement
    # T34(106,IR,IY,IS)=MC_REVIND(IR,30,IY)
    z[106] = dfd["MC_REVIND"].loc[30]

    #         Lime
    # T34(107,IR,IY,IS) = MC_REVIND(IR,31,IY)
    z[107] = dfd["MC_REVIND"].loc[31]

    #       Cement and Lime
    z[29] = z[106] + z[107]

    #       Other Nonmetallic Mineral Products
    # T34(30,IR,IY,IS)=MC_REVIND(IR,32,IY)
    z[30] = dfd["MC_REVIND"].loc[32]

    #     Primary Metals Industry
    # T34(17,IR,IY,IS) = MC_REVIND(IR,33,IY) + MC_REVIND(IR,34,IY) + MC_REVIND(IR,35,IY) + MC_REVIND(IR,26,IY)*(1-0.91)
    z[17] = (
        dfd["MC_REVIND"].loc[33]
        + dfd["MC_REVIND"].loc[34]
        + dfd["MC_REVIND"].loc[35]
        + dfd["MC_REVIND"].loc[26] * (1 - 0.91)
    )

    #       Iron and Steel Mills and Products
    # T34(31,IR,IY,IS)=MC_REVIND(IR,33,IY)+MC_REVIND(IR,26,IY)*(1-0.91)
    z[31] = dfd["MC_REVIND"].loc[33] + dfd["MC_REVIND"].loc[26] * (1 - 0.91)

    #       Alumina and Aluminum Products
    # T34(32,IR,IY,IS)=MC_REVIND(IR,34,IY)
    z[32] = dfd["MC_REVIND"].loc[34]

    #       Other Primary Metal Products
    # T34(33,IR,IY,IS)=MC_REVIND(IR,35,IY)
    z[33] = dfd["MC_REVIND"].loc[35]

    #     Fabricated Metal Products
    # T34(18,IR,IY,IS)=MC_REVIND(IR,36,IY)
    z[18] = dfd["MC_REVIND"].loc[36]

    #     Machinery
    # T34(19,IR,IY,IS)=MC_REVIND(IR,37,IY)
    z[19] = dfd["MC_REVIND"].loc[37]

    #     Computers and Electronics
    # T34(20,IR,IY,IS)=MC_REVIND(IR,38,IY)
    z[20] = dfd["MC_REVIND"].loc[38]

    #     Transportation Equipment
    # T34(21,IR,IY,IS)=MC_REVIND(IR,39,IY)
    z[21] = dfd["MC_REVIND"].loc[39]

    #     Electrical Equipment
    # T34(22,IR,IY,IS)=MC_REVIND(IR,40,IY)
    z[22] = dfd["MC_REVIND"].loc[40]

    #     Miscellaneous Manufacturing
    # T34(23,IR,IY,IS)=MC_REVIND(IR,41,IY)
    z[23] = dfd["MC_REVIND"].loc[41]

    #

    #   Total Industrial Value of Shipments
    # T34(36,IR,IY,IS)=FSUM(T34(3,IR,IY,IS),21)
    z[36] = (
        z[3]
        + z[4]
        + z[5]
        + z[6]
        + z[7]
        + z[8]
        + z[9]
        + z[10]
        + z[11]
        + z[12]
        + z[13]
        + z[14]
        + z[15]
        + z[16]
        + z[17]
        + z[18]
        + z[19]
        + z[20]
        + z[21]
        + z[22]
        + z[23]
    )

    #   -------------------------

    #   exclude the following when creating AEO Table

    #   -------------------------

    #   Grouped as in Table 18

    #     Non-Industrial and Service Sectors
    # T34(71,IR,IY,IS)=SUM(MC_REVSER(IR,1:10,IY))
    z[71] = (
        dfd["MC_REVSER"].loc[(slice(None), range(1, 10 + 1)), :].groupby(level=0).sum()
    )
    z[72] = z[71] * 0.0
    z[73] = z[71] * 0.0
    z[74] = z[71] * 0.0
    z[75] = z[71] * 0.0
    z[76] = z[71] * 0.0

    total_numbers = [
        1,
        6,
        7,
        8,
        9,
        10,
        14,
        15,
        16,
        18,
        19,
        20,
        25,
        26,
        27,
        28,
        30,
        31,
        32,
        33,
        34,
        35,
        36,
        37,
        38,
        39,
        40,
        41,
        42,
        43,
        44,
        45,
        46,
        47,
        48,
    ]
    mfg_numbers = [
        1,
        6,
        7,
        8,
        9,
        10,
        14,
        15,
        16,
        18,
        19,
        20,
        25,
        26,
        27,
        28,
        30,
        31,
        32,
        33,
        34,
        35,
        36,
        37,
        38,
        39,
        40,
        41,
    ]
    ei_numbers = [1, 10, 15, 16, 18, 19, 25, 28, 30, 31, 33, 34]
    nei_numbers = [6, 7, 8, 9, 14, 20, 26, 27, 32, 35, 36, 37, 38, 39, 40, 41]
    agmncn_numbers = [42, 43, 44, 45, 46, 47, 48]

    for III in total_numbers:
        z[72] = z[72] + dfd["MC_REVIND"].loc[III]  # Total Industrial
    for III in mfg_numbers:
        z[74] = z[74] + dfd["MC_REVIND"].loc[III]  #       Manufacturing
    for III in ei_numbers:
        z[75] = z[75] + dfd["MC_REVIND"].loc[III]  #         Energy-Intensive
    for III in nei_numbers:
        z[76] = z[76] + dfd["MC_REVIND"].loc[III]  #         Non-Energy-Intensive
    for III in agmncn_numbers:
        z[73] = (
            z[73] + dfd["MC_REVIND"].loc[III]
        )  #       Agriculture, Mining, and Construction

    #   Total Shipments
    # T34(77,IR,IY,IS)=T34(72,IR,IY,IS)+T34(71,IR,IY,IS)
    z[77] = z[72] + z[71]

    #

    #   Grouped as in Freight Transportation

    """ dfd['MC_REVIND'] has already index swaped !!! 
        index_level = 0: MNSICM: 1:60	
        index_level = 1: MNUMCR: 1:11
    """
    #      Basic Chemicals
    # T34(78,IR,IY,IS)=SUM(MC_REVIND(IR,15:20,IY))-MC_REVIND(IR,17,IY)-MC_REVIND(IR,19,IY)-MC_REVIND(IR,21,IY)
    # z[78] = dfd['MC_REVIND'].loc[15:20,:].sum() - dfd['MC_REVIND'].loc[17] - dfd['MC_REVIND'].loc[19] - dfd['MC_REVIND'].loc[21]
    z[78] = (
        dfd["MC_REVIND"].loc[range(15, 20 + 1)].groupby(level=1).sum()
        - dfd["MC_REVIND"].loc[17]
        - dfd["MC_REVIND"].loc[19]
        - dfd["MC_REVIND"].loc[21]
    )

    #      Primary Metals
    # T34(79,IR,IY,IS)=SUM(MC_REVIND(IR,33:35,IY))
    # z[79] = dfd['MC_REVIND'].loc[33:35,:].sum()
    z[79] = dfd["MC_REVIND"].loc[range(33, 35 + 1)].groupby(level=1).sum()

    #      Processed Food
    # T34(80,IR,IY,IS)=SUM(MC_REVIND(IR,2:5,IY))
    # z[80] = dfd['MC_REVIND'].loc[2:5,:].sum()
    z[80] = dfd["MC_REVIND"].loc[range(2, 5 + 1)].groupby(level=1).sum()

    #      Paper Products
    # T34(81,IR,IY,IS)=SUM(MC_REVIND(IR,11:13,IY))
    z[81] = dfd["MC_REVIND"].loc[range(11, 13 + 1)].groupby(level=1).sum()

    #      Petroleum Products
    # T34(82,IR,IY,IS)=MC_REVIND(IR,25,IY)+MC_REVIND(IR,26,IY)
    z[82] = dfd["MC_REVIND"].loc[25] + dfd["MC_REVIND"].loc[26]

    #      Stone, Clay, Glass, & Concrete

    # T34(83,IR,IY,IS) = MC_REVIND(IR,28,IY) + MC_REVIND(IR,30,IY) + MC_REVIND(IR,31,IY) + MC_REVIND(IR,32,IY)
    z[83] = (
        dfd["MC_REVIND"].loc[28]
        + dfd["MC_REVIND"].loc[30]
        + dfd["MC_REVIND"].loc[31]
        + dfd["MC_REVIND"].loc[32]
    )

    #      Metal Durables Excluding Computers
    # T34(84,IR,IY,IS)=SUM(MC_REVIND(IR,36:40,IY))-MC_REVIND(IR,38,IY)
    z[84] = (
        dfd["MC_REVIND"].loc[range(36, 40 + 1)].groupby(level=1).sum()
        - dfd["MC_REVIND"].loc[38]
    )

    #      Other Manufacturing
    # T34(85,IR,IY,IS)=SUM(MC_REVIND(IR,7:8,IY))+MC_REVIND(IR,14,IY)+MC_REVIND(IR,41,IY)
    z[85] = (
        dfd["MC_REVIND"].loc[range(7, 8 + 1)].groupby(level=1).sum()
        + dfd["MC_REVIND"].loc[14]
        + dfd["MC_REVIND"].loc[41]
    )

    #      Agriculture
    # T34(86,IR,IY,IS)=SUM(MC_REVIND(IR,42:44,IY))
    z[86] = dfd["MC_REVIND"].loc[range(42, 44 + 1)].groupby(level=1).sum()

    #      Mining
    # T34(87,IR,IY,IS)=SUM(MC_REVIND(IR,45:47,IY))
    z[87] = dfd["MC_REVIND"].loc[range(45, 47 + 1)].groupby(level=1).sum()

    #      Alcohol and Tobacco
    # T34(88,IR,IY,IS)=MC_REVIND(IR,6,IY)
    z[88] = dfd["MC_REVIND"].loc[6]

    #      Pharma
    # T34(89,IR,IY,IS)=MC_REVIND(IR,21,IY)
    z[89] = dfd["MC_REVIND"].loc[21]

    #      Fertilizers
    # T34(102,IR,IY,IS)=MC_REVIND(IR,19,IY)
    z[102] = dfd["MC_REVIND"].loc[19]

    #      Rubber and Plastics
    # T34(103,IR,IY,IS)=MC_REVIND(IR,27,IY)
    z[103] = dfd["MC_REVIND"].loc[27]

    #      Computers
    # T34(104,IR,IY,IS)=MC_REVIND(IR,38,IY)
    z[104] = dfd["MC_REVIND"].loc[38]

    #      Furniture
    # T34(105,IR,IY,IS)=MC_REVIND(IR,9,IY)
    z[105] = dfd["MC_REVIND"].loc[9]

    #   Grouped as in Commercial Light Trucks

    #      Agriculture
    # T34(90,IR,IY,IS)=MC_REVIND(IR,42,IY)
    z[90] = dfd["MC_REVIND"].loc[42]

    #      Mining
    # T34(91,IR,IY,IS)=SUM(MC_REVIND(IR,45:47,IY))
    z[91] = dfd["MC_REVIND"].loc[range(45, 47 + 1)].groupby(level=1).sum()

    #      Construction
    # T34(92,IR,IY,IS)=MC_REVIND(IR,48,IY)
    z[92] = dfd["MC_REVIND"].loc[48]

    #      Trade (Total Manufacturing)
    # T34(93,IR,IY,IS)=SUM(MC_REVIND(IR,1:41,IY))-SUM(MC_REVIND(IR,2:5,IY))-SUM(MC_REVIND(IR,11:13,IY))-MC_REVIND(IR,17,IY)-SUM(MC_REVIND(IR,21:24,IY))-MC_REVIND(IR,29,IY)
    # z[93] = dfd['MC_REVIND'].loc[1:41,:].sum()-dfd['MC_REVIND'].loc[2:5,:].sum()-dfd['MC_REVIND'].loc[11:13,:].sum()-dfd['MC_REVIND'].loc[17] - dfd['MC_REVIND'].loc[21:24].sum()-dfd['MC_REVIND'].loc[29]
    z[93] = (
        dfd["MC_REVIND"].loc[range(1, 41 + 1)].groupby(level=1).sum()
        - dfd["MC_REVIND"].loc[range(2, 5 + 1)].groupby(level=1).sum()
        - dfd["MC_REVIND"].loc[range(11, 13 + 1)].groupby(level=1).sum()
        - dfd["MC_REVIND"].loc[17]
        - dfd["MC_REVIND"].loc[range(21, 24 + 1)].groupby(level=1).sum()
        - dfd["MC_REVIND"].loc[29]
    )

    #      Utility
    # T34(94,IR,IY,IS)=SUM(MC_REVSER(IR,3:4,IY))
    # z[94] = dfd['MC_REVSER'].loc[3:4,:].sum()
    z[94] = (
        dfd["MC_REVSER"].loc[(slice(None), range(3, 4 + 1)), :].groupby(level=0).sum()
    )

    #   Energy Consumption (trillion Btu)

    #   Nonmanufacturing Sector

    #    Regional indices CRFROMCD are listed at the beginning of the file

    CRFROMCD = {1: 1, 2: 1, 3: 2, 4: 2, 5: 3, 6: 3, 7: 3, 8: 4, 9: 4, 10: 5, 11: 5}

    def make_11region_df(df, mapping_dict):
        """Create a new df of 11 regions from the input df of 5 regions based on mapping_dict

        Parameters
        ----------
        df : dataframe of multi-index
        mapping_dict : dictionary
            region mapping specification from input df to the new df
        Returns
        -------
        _type_
            a new df with the same indexes and columns of the input df but comes with 11 regions

        """
        # Convert df to a 3D array
        arr = df.values.reshape(
            len(df.index.levels[0]), len(df.index.levels[1]), len(df.columns)
        )
        #  Create a new 3D arr
        # new_arr = np.zeros((18, 0, 61)) #np.zeros((18, 11, 61))
        new_arr = np.zeros((20, 0, table_spec["year_len"]))  # np.zeros((18, 11, 61))

        # Insert items along the second dimension based on the mapping
        for i2, i1 in mapping_dict.items():
            new_arr = np.insert(new_arr, i2 - 1, arr[:, i1 - 1, :], axis=1)

        # Flatten the first two dimensions and leave the third as it is
        flattened_array = new_arr.reshape(-1, new_arr.shape[2])

        # Create a MultiIndex from the first two dimensions, adjusting indices to start from 1
        indices = np.indices((new_arr.shape[0], new_arr.shape[1])) + 1  # Adding 1 here

        # Flatten the indices arrays and pair them together to form a multi-index
        multi_index = pd.MultiIndex.from_arrays(
            indices.reshape(2, -1), names=df.index.names
        )

        # Create a new df
        df_11region = pd.DataFrame(
            flattened_array, index=multi_index, columns=df.columns
        )

        return df_11region

    # AGCON(M18=18,	MINDCR=5)

    #     Agriculture/Forestry/Fishing/Hunting
    # T34(41,IR,IY,IS)=SUM(AGCON(:,CRFROMCD(IR),IY))
    df = make_11region_df(dfd["AGCON"], CRFROMCD)
    z[41] = df.groupby(level=1).sum()

    #     Mining
    # T34(42,IR,IY,IS)=SUM(MINECON(:,CRFROMCD(IR),IY))+QLPIN(IR,IY)*1000.
    df = make_11region_df(dfd["MINECON"], CRFROMCD)
    z[42] = df.groupby(level=1).sum() + dfd["QLPIN"]

    #     Construction
    # T34(43,IR,IY,IS)=SUM(CONSTCON(:,CRFROMCD(IR),IY))
    df = make_11region_df(dfd["CONSTCON"], CRFROMCD)
    z[43] = df.groupby(level=1).sum()

    #

    #   Manufacturing Sector

    #     Food Products
    # T34(44,IR,IY,IS)=SUM(FOODCON(:,CRFROMCD(IR),IY))
    # z[44] = dfd['FOODCON'].loc[((:),CRfromCD(IR)),:].sum()
    df = make_11region_df(dfd["FOODCON"], CRFROMCD)
    z[44] = df.groupby(level=1).sum()

    #     Paper Products
    # T34(45,IR,IY,IS)=SUM(PAPERCON(:,CRFROMCD(IR),IY))
    # z[45] = dfd['PAPERCON'].loc[((:),CRfromCD(IR)),:].sum()
    df = make_11region_df(dfd["PAPERCON"], CRFROMCD)
    z[45] = df.groupby(level=1).sum()

    #     Bulk Chemicals
    # T34(46,IR,IY,IS)=SUM(CHEMCON(:,CRFROMCD(IR),IY))
    df = make_11region_df(dfd["CHEMCON"], CRFROMCD)
    z[46] = df.groupby(level=1).sum()

    #     Petroleum Refineries
    # ftab.f ------------------
    # !  Industrial consumptions, which are in Census Regions rather than Census Divisions
    # !            so assign a region to each division it contains:
    #    T34(47,IR,IY,IS) = sum(      REFCON(1:17,5,IY))
    #    T34(47,IR,IY,IS) = T34(47,IR,IY,IS) + 1000. * &
    #              ((CORNCD(3,11,IY) * CFCORN / 1000. -0.9751*(CRNETHCD(11,IY)+OTHETHCD(11,IY)) * 365. * CFPET - &
    #                RFBIOBUTECD(MNUMCR,IY)*365*CFBIOBUTE(IY)) / 1000000. + &
    #                sum(BIMQTYCD(1:4,11,IY))/1000000.*RDAYS*(CFVEGGIE(IY)-CFBIOD(IY)) + &
    #                - RDAYS / 1000000. * &
    #               (sum(BTLFRAC(1:4,MNUMPR,IY)) * CFBTLLIQ(IY) + &
    #                sum(CBTLFRAC(2,1:4,MNUMPR,IY)) * CFCBTLLIQ(2,IY) + &
    #                UBAVOL(MNUMPR,IY) * 5.763))
    #    IF (CONEFF(IY) .NE. 0.0) T34(47,IR,IY,IS) = T34(47,IR,IY,IS) + 1000. *  &
    #         (0.9751*CLLETHCD(11,IY) * RDAYS * (CFBMQ(IY) * 42. / CONEFF(IY) - CFPET)) / 1000000.
    #
    z47_us = dfd["INDREP/REFCON"].loc[1:17, 5, :].sum()
    z47_us = (
        z47_us
        + 1000.0
        * (
            (
                dfd["CORNCD"].loc[3, 11] * dfd["CFCORN"].iloc[0] / 1000.0
                - dfd["ETHANOL_PARAM_rwpre"]
                * (dfd["CRNETHCD"].loc[11] + dfd["OTHETHCD"].loc[11])
                * RDAYS
                * dfd["CFPET"].iloc[0]
                - dfd["RFBIOBUTECD"].loc[MNUMCR] * RDAYS * dfd["CFBIOBUTE"]
            )
            / 1000000
            + dfd["BIMQTYCD"].loc[1:4, 11, :].sum()
            / 1000000
            * RDAYS
            * (dfd["CFVEGGIE"] - dfd["CFBIOD"])
            - RDAYS
            / 1000000
            * (
                dfd["BTLFRAC"].loc[1:4, MNUMPR, :].sum() * dfd["CFBTLLIQ"]
                + dfd["CBTLFRAC"].loc[2, 1:4, MNUMPR, :].sum() * dfd["CFCBTLLIQ"].loc[2]
                + dfd["UBAVOL"].loc[MNUMPR] * dfd["CFUBA_rwpre"]
            )
        )
        + 1000
        * (
            dfd["ETHANOL_PARAM_rwpre"]
            * dfd["CLLETHCD"].loc[11]
            * RDAYS
            * (
                dfd["CFBMQ"] * dfd["GAL_PER_BBL_rwpre"] / dfd["CONEFF"]
                - dfd["CFPET"].iloc[0]
            ).fillna(0)
        )
        / 1000000
    )
    # Construct 11 regions with the same values
    z[47] = pd.DataFrame([[0] * table_spec["year_len"]] * 11, columns=z47_us.index)
    z[47] = z[47].reset_index(drop=True)
    z[47].index = z[47].index + 1
    #
    # TODO: check with John and EIA to see "assign a region to each division it contains" is OK?
    # Some statements in EAI IDM report:
    # https://www.eia.gov/outlooks/aeo/nems/documentation/industrial/pdf/IDM_2022.pdf
    #   "The IDM projects energy consumption at the census region level; energy consumption at the
    # census division level is allocated by using data from the State Energy Data System (SEDS) for 2019.
    #   "Because the module runs at the census region level, regional results are shared equally among
    # the census divisions using a factor, 𝐷𝐷𝑆𝑆𝑃𝑃𝑃𝑃, where 𝐷𝐷𝑆𝑆𝑃𝑃𝑃𝑃 is either one half or one-third"
    #   Table 19. Oil and Gas Supply Module and census region mapping
    # 		Northeast 	Gulf Coast 	Midcontinent 	Southwest 	Rocky Mountains West Coast
    # Census region 	1 		    2 		    3 		    4 		    5 		    6
    #           1 		22.9% 		0.0% 		0.0% 		0.0% 		0.0% 		0.0%
    #           2 		38.0% 		0.0% 		71.3% 		0.0% 		15.5% 		0.0%
    #           3 		39.1% 		100.0% 		28.7% 		57.0% 		0.0% 		0.0%
    #           4 		0.0% 		0.0% 		0.0% 		43.0% 		84.5% 		100.0%
    #
    # Assign a region to each division it contains
    df = make_11region_df(dfd["INDREP/REFCON"], CRFROMCD)
    z[47] = df.groupby(level=1).sum()  # + z[47]
    # Replace US with the origional data for groupping by 5 (10+11) is a little bigger
    z[47].loc[11] = z47_us.values

    #     Glass and Glass Products
    # T34(48,IR,IY,IS)=SUM(GLASSCON(:,CRFROMCD(IR),IY))
    df = make_11region_df(dfd["GLASSCON"], CRFROMCD)
    z[48] = df.groupby(level=1).sum()

    #     Cement and Lime
    # T34(49,IR,IY,IS)=SUM(CEMENTCON(:,CRFROMCD(IR),IY))
    df = make_11region_df(dfd["CEMENTCON"], CRFROMCD)
    z[49] = df.groupby(level=1).sum()

    #     Iron and Steel Mills and Products
    # T34(50,IR,IY,IS)=SUM(STEELCON(:,CRFROMCD(IR),IY))
    df = make_11region_df(dfd["STEELCON"], CRFROMCD)
    z[50] = df.groupby(level=1).sum()

    #     Alumina and Aluminum Products
    # T34(51,IR,IY,IS)=SUM(ALUMCON(:,CRFROMCD(IR),IY))
    # z[51] = dfd['ALUMCON'].loc[((:),CRfromCD(IR)),:].sum()
    df = make_11region_df(dfd["ALUMCON"], CRFROMCD)
    z[51] = df.groupby(level=1).sum()

    #     Fabricated Metal Products
    # T34(52,IR,IY,IS)=SUM(FABMETALCON(:,CRFROMCD(IR),IY))
    # z[52] = dfd['FABMETALCON'].loc[((:),CRfromCD(IR)),:].sum()
    df = make_11region_df(dfd["FABMETALCON"], CRFROMCD)
    z[52] = df.groupby(level=1).sum()

    #     Machinery
    # T34(53,IR,IY,IS)=SUM(MACHINECON(:,CRFROMCD(IR),IY))
    # z[53] = dfd['MACHINECON'].loc[((:),CRfromCD(IR)),:].sum()
    df = make_11region_df(dfd["MACHINECON"], CRFROMCD)
    z[53] = df.groupby(level=1).sum()

    #     Computers
    # T34(54,IR,IY,IS)=SUM(COMPUTECON(:,CRFROMCD(IR),IY))
    df = make_11region_df(dfd["COMPUTECON"], CRFROMCD)
    z[54] = df.groupby(level=1).sum()

    #     Transportation Equipment
    # T34(55,IR,IY,IS)=SUM(TRANEQUIPCON(:,CRFROMCD(IR),IY))
    df = make_11region_df(dfd["TRANEQUIPCON"], CRFROMCD)
    z[55] = df.groupby(level=1).sum()

    #     Electronic Equipment
    # T34(56,IR,IY,IS)=SUM(ELECEQUIPON(:,CRFROMCD(IR),IY))
    # z[56] = dfd['ELECEQUIPON'].loc[((:),CRfromCD(IR)),:].sum()
    df = make_11region_df(dfd["ELECEQUIPON"], CRFROMCD)
    z[56] = df.groupby(level=1).sum()

    #     Wood Products
    # T34(57,IR,IY,IS)=SUM(WOODPRODCON(:,CRFROMCD(IR),IY))
    # z[57] = dfd['WOODPRODCON'].loc[((:),CRfromCD(IR)),:].sum()
    df = make_11region_df(dfd["WOODPRODCON"], CRFROMCD)
    z[57] = df.groupby(level=1).sum()

    #     Plastics
    # T34(58,IR,IY,IS)=SUM(PLASTICCON(:,CRFROMCD(IR),IY))
    df = make_11region_df(dfd["PLASTICCON"], CRFROMCD)
    z[58] = df.groupby(level=1).sum()

    #     Light Chemicals
    # T34(59,IR,IY,IS)=sum(LTCHEMCON(:,CRfromCD(IR),IY))
    df = make_11region_df(dfd["LTCHEMCON"], CRFROMCD)
    z[59] = df.groupby(level=1).sum()

    #     Other Non-Metallic Minerals
    # T34(60,IR,IY,IS)=sum(OTHRNMMCON(:,CRfromCD(IR),IY))
    df = make_11region_df(dfd["OTHRNMMCON"], CRFROMCD)
    z[60] = df.groupby(level=1).sum()

    #     Other Primary Metals
    # T34(61,IR,IY,IS)=sum(OTHRPRIMCON(:,CRfromCD(IR),IY))
    df = make_11region_df(dfd["OTHRPRIMCON"], CRFROMCD)
    z[61] = df.groupby(level=1).sum()

    #     Miscellaneous Finished Goods
    # T34(62,IR,IY,IS)=sum(MISCFINCON(:,CRfromCD(IR),IY))
    df = make_11region_df(dfd["MISCFINCON"], CRFROMCD)
    z[62] = df.groupby(level=1).sum()

    #
    #   Total Industrial Fuel Consumption
    # T34(63,IR,IY,IS) = FSUM(T34(41,IR,IY,IS),19)
    z[63] = (
        z[41]
        + z[42]
        + z[43]
        + z[44]
        + z[45]
        + z[46]
        + z[47]
        + z[48]
        + z[49]
        + z[50]
        + z[51]
        + z[52]
        + z[53]
        + z[54]
        + z[55]
        + z[56]
        + z[57]
        + z[58]
        + z[59]
    )

    return z
