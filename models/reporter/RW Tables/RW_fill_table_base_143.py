# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
SZO Fixed the percentage calculations on 7/12/2024
"""


def fill_table_base_143(dfd, table_spec, table_id):
    """Fill table Nominal Composition of Gross Domestic Product

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

    #   Nominal Composition of Gross Domestic Product
    #   (billion nominal chain-weighted dollars)
    #   Component

    #   Gross Domestic Product
    # T143(1,IY,IS)=MC_GDP(IY)
    z[1] = dfd["MC_GDP"]

    #     (percent change)
    # T143(2,IY,IS)=((MC_GDP(IY)/MC_GDP(IY-1))-1)*100
    z[2] = dfd["MC_GDP"].pct_change() * 100

    #   Personal Consumption Expenditures
    # T143(3,IY,IS)=MC_CONS(IY)
    z[3] = dfd["MC_CONS"]

    #     Goods
    # T143(4,IY,IS)=MC_CGOODS(IY)
    z[4] = dfd["MC_CGOODS"]

    #       Durable Goods
    # T143(5,IY,IS)=MC_CD(IY)
    z[5] = dfd["MC_CD"]

    #         Motor Vehicles & Parts
    # T143(6,IY,IS)=MC_CDMV(IY)
    z[6] = dfd["MC_CDMV"]

    #         Furnishings & Durable Household Equipment
    # T143(7,IY,IS)=MC_CDFHE(IY)
    z[7] = dfd["MC_CDFHE"]

    #         Recreational Goods & Vehicles
    # T143(8,IY,IS)=MC_CDREC(IY)
    z[8] = dfd["MC_CDREC"]

    #         Other Durables
    # T143(9,IY,IS)=MC_CDO(IY)
    z[9] = dfd["MC_CDO"]

    #       Nondurable Goods
    # T143(10,IY,IS)=MC_CN(IY)
    z[10] = dfd["MC_CN"]

    #         Food & Beverages (Off-Premises)
    # T143(11,IY,IS)=MC_CNF(IY)
    z[11] = dfd["MC_CNF"]

    #         Clothing & Footwear
    # T143(12,IY,IS)=MC_CNCS(IY)
    z[12] = dfd["MC_CNCS"]

    #         Motor Vehicle Fuels, Lubricants & Fluids
    # T143(13,IY,IS)=MC_CNEGAO(IY)
    z[13] = dfd["MC_CNEGAO"]

    #         Fuel Oil & Other Fuels
    # T143(14,IY,IS)=MC_CNEFAO(IY)
    z[14] = dfd["MC_CNEFAO"]

    #         Pharmacological & Other Medical Products
    # T143(15,IY,IS)=MC_CNOPMP(IY)
    z[15] = dfd["MC_CNOPMP"]

    #         Tobacco
    # T143(16,IY,IS)=MC_CNOTOB(IY)
    z[16] = dfd["MC_CNOTOB"]

    #         Other
    # T143(17,IY,IS)=MC_CNOO(IY)
    z[17] = dfd["MC_CNOO"]

    #     Services
    # T143(18,IY,IS)=MC_CSV(IY)
    z[18] = dfd["MC_CSV"]

    #       Household Consumption Expenditures
    # T143(19,IY,IS)=MC_CSVHH(IY)
    z[19] = dfd["MC_CSVHH"]

    #         Housing
    # T143(20,IY,IS)=MC_CSVH(IY)
    z[20] = dfd["MC_CSVH"]

    #         Utilities
    # T143(21,IY,IS)=MC_CSVU(IY)
    z[21] = dfd["MC_CSVU"]

    #         Health Care
    # T143(22,IY,IS)=MC_CSVHC(IY)
    z[22] = dfd["MC_CSVHC"]

    #         Transportation Services
    # T143(23,IY,IS)=MC_CSVTS(IY)
    z[23] = dfd["MC_CSVTS"]

    #         Recreation Services
    # T143(24,IY,IS)=MC_CSVREC(IY)
    z[24] = dfd["MC_CSVREC"]

    #         Food Services
    # T143(25,IY,IS)=MC_CSVF(IY)
    z[25] = dfd["MC_CSVF"]

    #         Accommodations
    # T143(26,IY,IS)=MC_CSVAC(IY)
    z[26] = dfd["MC_CSVAC"]

    #         Financial Services
    # T143(27,IY,IS)=MC_CSVFIN(IY)
    z[27] = dfd["MC_CSVFIN"]

    #         Insurance
    # T143(28,IY,IS)=MC_CSVINS(IY)
    z[28] = dfd["MC_CSVINS"]

    #         Other Services
    # T143(29,IY,IS)=MC_CSVO(IY)
    z[29] = dfd["MC_CSVO"]

    #       Nonprofits Serving Households, Final
    # T143(30,IY,IS)=MC_CSVNPISH(IY)
    z[30] = dfd["MC_CSVNPISH"]

    #

    #   Investment
    # T143(31,IY,IS)=MC_I(IY)
    z[31] = dfd["MC_I"]

    #     Nonresidential Fixed
    # T143(32,IY,IS)=MC_IFNRE(IY)
    z[32] = dfd["MC_IFNRE"]

    #       Equipment & Software
    # T143(33,IY,IS)=MC_IFNREE(IY)
    z[33] = dfd["MC_IFNREE"]

    #         Information Processing
    # T143(34,IY,IS)=MC_IFNREEIP(IY)
    z[34] = dfd["MC_IFNREEIP"]

    #         Industrial
    # T143(35,IY,IS)=MC_IFNREEIND(IY)
    z[35] = dfd["MC_IFNREEIND"]

    #         Light Vehicles
    # T143(36,IY,IS)=MC_IFNREETLV(IY)
    z[36] = dfd["MC_IFNREETLV"]

    #         Aircraft
    # T143(37,IY,IS)=MC_IFNREETAC(IY)
    z[37] = dfd["MC_IFNREETAC"]

    #         Other Transportation
    # T143(38,IY,IS)=MC_IFNREETO(IY)
    z[38] = dfd["MC_IFNREETO"]

    #         Other Equipment
    # T143(39,IY,IS)=MC_IFNREEO(IY)
    z[39] = dfd["MC_IFNREEO"]

    #       Structures
    # T143(40,IY,IS)=MC_IFNRES(IY)
    z[40] = dfd["MC_IFNRES"]

    #         Commercial & Health Care
    # T143(41,IY,IS)=MC_IFNRESC(IY)
    z[41] = dfd["MC_IFNRESC"]

    #         Manufacturing
    # T143(42,IY,IS)=MC_IFNRESMFG(IY)
    z[42] = dfd["MC_IFNRESMFG"]

    #         Power & Communications
    # T143(43,IY,IS)=MC_IFNRESP(IY)
    z[43] = dfd["MC_IFNRESP"]

    #           Power
    # T143(44,IY,IS)=MC_IFNRESPP(IY)
    z[44] = dfd["MC_IFNRESPP"]

    #           Communications
    # T143(45,IY,IS)=MC_IFNRESPC(IY)
    z[45] = dfd["MC_IFNRESPC"]

    #         Mining & Petroleum
    # T143(46,IY,IS)=MC_IFNRESMI(IY)
    z[46] = dfd["MC_IFNRESMI"]

    #         Other
    # T143(47,IY,IS)=MC_IFNRESO(IY)
    z[47] = dfd["MC_IFNRESO"]

    #     Residential Fixed
    # T143(48,IY,IS)=MC_IFRE(IY)
    z[48] = dfd["MC_IFRE"]

    #       Structures
    # T143(49,IY,IS)=MC_IFRES(IY)
    z[49] = dfd["MC_IFRES"]

    #       Equipment
    # T143(50,IY,IS)=MC_IFREE(IY)
    z[50] = dfd["MC_IFREE"]

    #     Change in Inventories
    # T143(51,IY,IS)=MC_II(IY)
    z[51] = dfd["MC_II"]

    #       Nonfarm
    # T143(52,IY,IS)=MC_IINF(IY)
    z[52] = dfd["MC_IINF"]

    #         Manufacturing
    # T143(53,IY,IS)=MC_IIM(IY)
    z[53] = dfd["MC_IIM"]

    #         Wholesale
    # T143(54,IY,IS)=MC_IIW(IY)
    z[54] = dfd["MC_IIW"]

    #         Retail
    # T143(55,IY,IS)=MC_IIRT(IY)
    z[55] = dfd["MC_IIRT"]

    #         Construction, Mining & Public Utilities
    # T143(56,IY,IS)=MC_IICMIU(IY)
    z[56] = dfd["MC_IICMIU"]

    #       Farm
    # T143(57,IY,IS)=MC_IIF(IY)
    z[57] = dfd["MC_IIF"]

    #   Exports
    # T143(58,IY,IS)=MC_X(IY)
    z[58] = dfd["MC_X"]

    #     Goods
    # T143(59,IY,IS)=MC_XG(IY)
    z[59] = dfd["MC_XG"]

    #     Services
    # T143(60,IY,IS)=MC_XSVTOT(IY)
    z[60] = dfd["MC_XSVTOT"]

    #   Imports
    # T143(61,IY,IS)=MC_M(IY)
    z[61] = dfd["MC_M"]

    #     Goods
    # T143(62,IY,IS)=MC_MG(IY)
    z[62] = dfd["MC_MG"]

    #     Services
    # T143(63,IY,IS)=MC_MSVTOT(IY)
    z[63] = dfd["MC_MSVTOT"]

    #   Government Purchases
    # T143(64,IY,IS)=MC_G(IY)
    z[64] = dfd["MC_G"]

    #     Federal
    # T143(65,IY,IS)=MC_GF(IY)
    z[65] = dfd["MC_GF"]

    #       Defense
    # T143(66,IY,IS)=MC_GFML(IY)
    z[66] = dfd["MC_GFML"]

    #       Nondefense
    # T143(67,IY,IS)=MC_GFO(IY)
    z[67] = dfd["MC_GFO"]

    #     State & Local
    # T143(68,IY,IS)=MC_GSL(IY)
    z[68] = dfd["MC_GSL"]

    return z
