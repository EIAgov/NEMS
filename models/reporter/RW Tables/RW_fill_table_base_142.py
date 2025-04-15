# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM

SZO Fixed the percentage calculations on 7/12/2024

"""


def fill_table_base_142(dfd, table_spec, table_id):
    """Fill table for    Real Composition of Gross Domestic Product

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

    #   Real Composition of Gross Domestic Product
    #   (billion m__m chain-weighted dollars)
    #   Component

    #   Gross Domestic Product
    # T142(1,IY,IS)=MC_GDPR(IY)
    z[1] = dfd["MC_GDPR"]

    #   Gross Domestic Product (percent change)
    z[2] = dfd["MC_GDPR"].pct_change() * 100

    #   Personal Consumption Expenditures
    # T142(3,IY,IS)=MC_CONSR(IY)
    z[3] = dfd["MC_CONSR"]

    #     Goods
    # T142(4,IY,IS)=MC_CGOODSR(IY)
    z[4] = dfd["MC_CGOODSR"]

    #       Durable Goods
    # T142(5,IY,IS)=MC_CDR(IY)
    z[5] = dfd["MC_CDR"]

    #         Motor Vehicles & Parts
    # T142(6,IY,IS)=MC_CDMVR(IY)
    z[6] = dfd["MC_CDMVR"]

    #         Furnishings & Durable Household Equipment
    # T142(7,IY,IS)=MC_CDFHER(IY)
    z[7] = dfd["MC_CDFHER"]

    #         Recreational Goods & Vehicles
    # T142(8,IY,IS)=MC_CDRECR(IY)
    z[8] = dfd["MC_CDRECR"]

    #         Other Durables
    # T142(9,IY,IS)=MC_CDOR(IY)
    z[9] = dfd["MC_CDOR"]

    #       Nondurable Goods
    # T142(10,IY,IS)=MC_CNR(IY)
    z[10] = dfd["MC_CNR"]

    #         Food & Beverages (Off-Premises)
    # T142(11,IY,IS)=MC_CNFR(IY)
    z[11] = dfd["MC_CNFR"]

    #         Clothing & Footwear
    # T142(12,IY,IS)=MC_CNCSR(IY)
    z[12] = dfd["MC_CNCSR"]

    #         Motor Vehicle Fuels, Lubricants & Fluids
    # T142(13,IY,IS)=MC_CNEGAOR(IY)
    z[13] = dfd["MC_CNEGAOR"]

    #         Fuel Oil & Other Fuels
    # T142(14,IY,IS)=MC_CNEFAOR(IY)
    z[14] = dfd["MC_CNEFAOR"]

    #         Pharmacological & Other Medical Products
    # T142(15,IY,IS)=MC_CNOPMPR(IY)
    z[15] = dfd["MC_CNOPMPR"]

    #         Tobacco
    # T142(16,IY,IS)=MC_CNOTOBR(IY)
    z[16] = dfd["MC_CNOTOBR"]

    #         Other
    # T142(17,IY,IS)=MC_CNOOR(IY)
    z[17] = dfd["MC_CNOOR"]

    #     Services
    # T142(18,IY,IS)=MC_CSVR(IY)
    z[18] = dfd["MC_CSVR"]

    #       Household Consumption Expenditures
    # T142(19,IY,IS)=MC_CSVHHR(IY)
    z[19] = dfd["MC_CSVHHR"]

    #         Housing
    # T142(20,IY,IS)=MC_CSVHR(IY)
    z[20] = dfd["MC_CSVHR"]

    #         Utilities
    # T142(21,IY,IS)=MC_CSVUR(IY)
    z[21] = dfd["MC_CSVUR"]

    #         Health Care
    # T142(22,IY,IS)=MC_CSVHCR(IY)
    z[22] = dfd["MC_CSVHCR"]

    #         Transportation Services
    # T142(23,IY,IS)=MC_CSVTSR(IY)
    z[23] = dfd["MC_CSVTSR"]

    #         Recreation Services
    # T142(24,IY,IS)=MC_CSVRECR(IY)
    z[24] = dfd["MC_CSVRECR"]

    #         Food Services
    # T142(25,IY,IS)=MC_CSVFR(IY)
    z[25] = dfd["MC_CSVFR"]

    #         Accommodations
    # T142(26,IY,IS)=MC_CSVACR(IY)
    z[26] = dfd["MC_CSVACR"]

    #         Financial Services
    # T142(27,IY,IS)=MC_CSVFINR(IY)
    z[27] = dfd["MC_CSVFINR"]

    #         Insurance
    # T142(28,IY,IS)=MC_CSVINSR(IY)
    z[28] = dfd["MC_CSVINSR"]

    #         Other Services
    # T142(29,IY,IS)=MC_CSVOR(IY)
    z[29] = dfd["MC_CSVOR"]

    #       Nonprofits Serving Households, Final
    # T142(30,IY,IS)=MC_CSVNPISHR(IY)
    z[30] = dfd["MC_CSVNPISHR"]

    #

    #   Investment
    # T142(31,IY,IS)=MC_IR(IY)
    z[31] = dfd["MC_IR"]

    #     Nonresidential Fixed
    # T142(32,IY,IS)=MC_IFNRER(IY)
    z[32] = dfd["MC_IFNRER"]

    #       Equipment & Software
    # T142(33,IY,IS)=MC_IFNREER(IY)
    z[33] = dfd["MC_IFNREER"]

    #         Information Processing
    # T142(34,IY,IS)=MC_IFNREEIPR(IY)
    z[34] = dfd["MC_IFNREEIPR"]

    #         Industrial
    # T142(35,IY,IS)=MC_IFNREEINDR(IY)
    z[35] = dfd["MC_IFNREEINDR"]

    #         Light Vehicles
    # T142(36,IY,IS)=MC_IFNREETLVR(IY)
    z[36] = dfd["MC_IFNREETLVR"]

    #         Aircraft
    # T142(37,IY,IS)=MC_IFNREETACR(IY)
    z[37] = dfd["MC_IFNREETACR"]

    #         Other Transportation
    # T142(38,IY,IS)=MC_IFNREETOR(IY)
    z[38] = dfd["MC_IFNREETOR"]

    #         Other Equipment
    # T142(39,IY,IS)=MC_IFNREEOR(IY)
    z[39] = dfd["MC_IFNREEOR"]

    #       Structures
    # T142(40,IY,IS)=MC_IFNRESR(IY)
    z[40] = dfd["MC_IFNRESR"]

    #         Commercial & Health Care
    # T142(41,IY,IS)=MC_IFNRESCR(IY)
    z[41] = dfd["MC_IFNRESCR"]

    #         Manufacturing
    # T142(42,IY,IS)=MC_IFNRESMFGR(IY)
    z[42] = dfd["MC_IFNRESMFGR"]

    #         Power & Communications
    # T142(43,IY,IS)=MC_IFNRESPR(IY)
    z[43] = dfd["MC_IFNRESPR"]

    #           Power
    # T142(44,IY,IS)=MC_IFNRESPPR(IY)
    z[44] = dfd["MC_IFNRESPPR"]

    #           Communications
    # T142(45,IY,IS)=MC_IFNRESPCR(IY)
    z[45] = dfd["MC_IFNRESPCR"]

    #         Mining & Petroleum
    # T142(46,IY,IS)=MC_IFNRESMIR(IY)
    z[46] = dfd["MC_IFNRESMIR"]

    #         Other
    # T142(47,IY,IS)=MC_IFNRESOR(IY)
    z[47] = dfd["MC_IFNRESOR"]

    #     Residential Fixed
    # T142(48,IY,IS)=MC_IFRER(IY)
    z[48] = dfd["MC_IFRER"]

    #       Structures
    # T142(49,IY,IS)=MC_IFRESR(IY)
    z[49] = dfd["MC_IFRESR"]

    #       Equipment
    # T142(50,IY,IS)=MC_IFREER(IY)
    z[50] = dfd["MC_IFREER"]

    #     Change in Inventories
    # T142(51,IY,IS)=MC_IIR(IY)
    z[51] = dfd["MC_IIR"]

    #       Nonfarm
    # T142(52,IY,IS)=MC_IINFR(IY)
    z[52] = dfd["MC_IINFR"]

    #         Manufacturing
    # T142(53,IY,IS)=MC_IIMR(IY)
    z[53] = dfd["MC_IIMR"]

    #         Wholesale
    # T142(54,IY,IS)=MC_IIWR(IY)
    z[54] = dfd["MC_IIWR"]

    #         Retail
    # T142(55,IY,IS)=MC_IIRTR(IY)
    z[55] = dfd["MC_IIRTR"]

    #         Construction, Mining & Public Utilities
    # T142(56,IY,IS)=MC_IICMIUR(IY)
    z[56] = dfd["MC_IICMIUR"]

    #       Farm
    # T142(57,IY,IS)=MC_IIFR(IY)
    z[57] = dfd["MC_IIFR"]

    #   Exports
    # T142(58,IY,IS)=MC_XR(IY)
    z[58] = dfd["MC_XR"]

    #     Goods
    # T142(59,IY,IS)=MC_XGR(IY)
    z[59] = dfd["MC_XGR"]

    #       Food, Feed, and Beverage
    # T142(69,IY,IS)=MC_XGFFBR(IY)
    z[69] = dfd["MC_XGFFBR"]

    #       Industrial Supplies
    # T142(70,IY,IS)=MC_XGINR(IY)
    z[70] = dfd["MC_XGINR"]

    #       Consumer Goods
    # T142(71,IY,IS)=MC_XGCR(IY)
    z[71] = dfd["MC_XGCR"]

    #       Capital Goods
    # T142(72,IY,IS)=MC_XGKR(IY)
    z[72] = dfd["MC_XGKR"]

    #       Motor Vehicles and Parts
    # T142(73,IY,IS)=MC_XGAUTOR(IY)
    z[73] = dfd["MC_XGAUTOR"]

    #     Services
    # T142(60,IY,IS)=MC_XSVTOTR(IY)
    z[60] = dfd["MC_XSVTOTR"]

    #   Imports
    # T142(61,IY,IS)=MC_MR(IY)
    z[61] = dfd["MC_MR"]

    #     Goods
    # T142(62,IY,IS)=MC_MGR(IY)
    z[62] = dfd["MC_MGR"]

    #       Food, Feed, and Beverage
    # T142(74,IY,IS)=MC_MGFFBR(IY)
    z[74] = dfd["MC_MGFFBR"]

    #       Industrial Supplies
    # T142(75,IY,IS)=MC_MGINAPETR(IY)
    z[75] = dfd["MC_MGINAPETR"]

    #       Consumer Goods
    # T142(76,IY,IS)=MC_MGCR(IY)
    z[76] = dfd["MC_MGCR"]

    #       Capital Goods
    # T142(77,IY,IS)=MC_MGKR(IY)
    z[77] = dfd["MC_MGKR"]

    #       Motor Vehicles and Parts
    # T142(78,IY,IS)=MC_MGAUTOR(IY)
    z[78] = dfd["MC_MGAUTOR"]

    #     Services
    # T142(63,IY,IS)=MC_MSVTOTR(IY)
    z[63] = dfd["MC_MSVTOTR"]

    #   Government Purchases
    # T142(64,IY,IS)=MC_GR(IY)
    z[64] = dfd["MC_GR"]

    #     Federal
    # T142(65,IY,IS)=MC_GFR(IY)
    z[65] = dfd["MC_GFR"]

    #       Defense
    # T142(66,IY,IS)=MC_GFMLR(IY)
    z[66] = dfd["MC_GFMLR"]

    #       Nondefense
    # T142(67,IY,IS)=MC_GFOR(IY)
    z[67] = dfd["MC_GFOR"]

    #     State & Local
    # T142(68,IY,IS)=MC_GSLR(IY)
    z[68] = dfd["MC_GSLR"]

    return z
