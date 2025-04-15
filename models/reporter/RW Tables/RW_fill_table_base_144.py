# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
SZO Fixed the percentage calculations on 7/12/2024
"""


def fill_table_base_144(dfd, table_spec, table_id):
    """Fill table Distribution of GDP, GNP, and National Income

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

    #   Distribution of GDP, GNP, and National Income
    #   (billion nominal chain-weighted dollars)
    #   Breakdown

    #   Gross Domestic Product
    # T144(1,IY,IS)=MC_GDP(IY)
    z[1] = dfd["MC_GDP"]

    #     Plus: Income Receipts from Rest of World
    # T144(2,IY,IS)=MC_XFY(IY)
    z[2] = dfd["MC_XFY"]

    #     Less: Income Payments to Rest of World
    # T144(3,IY,IS)=MC_MFY(IY)
    z[3] = dfd["MC_MFY"]

    #   Equals: Gross National Product
    # T144(4,IY,IS)=MC_GNP(IY)
    z[4] = dfd["MC_GNP"]

    #     Less: Consumption of Fixed Capital
    # T144(5,IY,IS)=MC_CKF(IY)
    z[5] = dfd["MC_CKF"]

    #   Equals: Net National Product
    # T144(6,IY,IS)=MC_NNP(IY)
    z[6] = dfd["MC_NNP"]

    #     Less: Statistical Discrepancy
    # T144(7,IY,IS)=MC_STAT(IY)
    z[7] = dfd["MC_STAT"]

    #   Equals: National Income
    # T144(8,IY,IS)=MC_YN(IY)
    z[8] = dfd["MC_YN"]

    #

    #   Composition of National Income

    #     Compensation of Employees
    # T144(9,IY,IS)=MC_YPCOMP(IY)
    z[9] = dfd["MC_YPCOMP"]

    #     Nonfarm Proprietors
    # T144(10,IY,IS)=MC_YPPROPADJNF(IY)
    z[10] = dfd["MC_YPPROPADJNF"]

    #     Farm Proprietors
    # T144(11,IY,IS)=MC_YPPROPADJF(IY)
    z[11] = dfd["MC_YPPROPADJF"]

    #     Rental Income
    # T144(12,IY,IS)=MC_YPRENTADJ(IY)
    z[12] = dfd["MC_YPRENTADJ"]

    #     Net Interest
    # T144(13,IY,IS)=MC_INTNETBUS(IY)
    z[13] = dfd["MC_INTNETBUS"]

    #     Economic Profits
    # T144(14,IY,IS)=MC_ZBECON(IY)
    z[14] = dfd["MC_ZBECON"]

    #     Taxes on Production & Imports
    # T144(15,IY,IS)=MC_TXIM(IY)
    z[15] = dfd["MC_TXIM"]

    #     Business Current Transfer Payments (Net)
    # T144(16,IY,IS)=MC_TRFBUS(IY)
    z[16] = dfd["MC_TRFBUS"]

    #     Surplus less Subsidies of Gov't. Enterprises
    # T144(17,IY,IS)=MC_SUBLSURPGF(IY)+MC_SUBLSURPGSL(IY)
    z[17] = dfd["MC_SUBLSURPGF"] + dfd["MC_SUBLSURPGSL"]

    #

    #   Income Shares, Percent of National Income

    #     Compensation of Employees
    # T144(18,IY,IS)=100*MC_YPCOMP(IY)/MC_YN(IY)
    z[18] = 100 * dfd["MC_YPCOMP"] / dfd["MC_YN"]

    #     Nonfarm Proprietors
    # T144(19,IY,IS)=100*MC_YPPROPADJNF(IY)/MC_YN(IY)
    z[19] = 100 * dfd["MC_YPPROPADJNF"] / dfd["MC_YN"]

    #     Farm Proprietors
    # T144(20,IY,IS)=100*MC_YPPROPADJF(IY)/MC_YN(IY)
    z[20] = 100 * dfd["MC_YPPROPADJF"] / dfd["MC_YN"]

    #     Rental Income
    # T144(21,IY,IS)=100*MC_YPRENTADJ(IY)/MC_YN(IY)
    z[21] = 100 * dfd["MC_YPRENTADJ"] / dfd["MC_YN"]

    #     Net Interest
    # T144(22,IY,IS)=100*MC_INTNETBUS(IY)/MC_YN(IY)
    z[22] = 100 * dfd["MC_INTNETBUS"] / dfd["MC_YN"]

    #     Economic Profits
    # T144(23,IY,IS)=100*MC_ZBECON(IY)/MC_YN(IY)
    z[23] = 100 * dfd["MC_ZBECON"] / dfd["MC_YN"]

    #     Taxes on Production & Imports
    # T144(24,IY,IS)=100*MC_TXIM(IY)/MC_YN(IY)
    z[24] = 100 * dfd["MC_TXIM"] / dfd["MC_YN"]

    #     Business Current Transfer Payments (Net)
    # T144(25,IY,IS)=100*MC_TRFBUS(IY)/MC_YN(IY)
    z[25] = 100 * dfd["MC_TRFBUS"] / dfd["MC_YN"]

    #     Surplus less Subsidies of Gov't. Enterprises
    # T144(26,IY,IS)=100*(MC_SUBLSURPGF(IY)+MC_SUBLSURPGSL(IY))/MC_YN(IY)
    z[26] = 100 * (dfd["MC_SUBLSURPGF"] + dfd["MC_SUBLSURPGSL"]) / dfd["MC_YN"]

    #

    #   Corporate Profits

    #     Economic Profits
    # T144(27,IY,IS)=MC_ZBECON(IY)
    z[27] = dfd["MC_ZBECON"]

    #       percent change
    # T144(28,IY,IS)=((MC_ZBECON(IY)/MC_ZBECON(IY-1))-1)*100
    z[28] = dfd["MC_ZBECON"].pct_change() * 100

    #     Less:

    #       Capital Consumption Adjustment
    # T144(29,IY,IS)=MC_CKFADJCORP(IY)
    z[29] = dfd["MC_CKFADJCORP"]

    #       Inventory Valuation Adjustment
    # T144(30,IY,IS)=MC_IVACORP(IY)
    z[30] = dfd["MC_IVACORP"]

    #     Equals: Before-Tax Book Profits
    # T144(31,IY,IS)=MC_ZB(IY)
    z[31] = dfd["MC_ZB"]

    #       percent change
    # T144(32,IY,IS)=((MC_ZB(IY)/MC_ZB(IY-1))-1)*100
    z[32] = dfd["MC_ZB"].pct_change() * 100

    #     Less: Corporate Income Taxes
    # T144(33,IY,IS)=MC_TXCORP(IY)
    z[33] = dfd["MC_TXCORP"]

    #       Federal
    # T144(34,IY,IS)=MC_TXCORPGF(IY)
    z[34] = dfd["MC_TXCORPGF"]

    #       State & Local
    # T144(35,IY,IS)=MC_TXCORPGSL(IY)
    z[35] = dfd["MC_TXCORPGSL"]

    #       Rest of World
    # T144(36,IY,IS)=MC_TXCORPRW(IY)
    z[36] = dfd["MC_TXCORPRW"]

    #     Equals: After-Tax Profits
    # T144(37,IY,IS)=MC_ZA(IY)
    z[37] = dfd["MC_ZA"]

    #       Less: Dividends
    # T144(38,IY,IS)=MC_ZADIV(IY)
    z[38] = dfd["MC_ZADIV"]

    #     Equals: Retained Earnings
    # T144(39,IY,IS)=MC_ZARE(IY)
    z[39] = dfd["MC_ZARE"]

    #

    #   Profits Addenda

    #     Economic Profits
    # T144(40,IY,IS)=MC_ZBECON(IY)
    z[40] = dfd["MC_ZBECON"]

    #       Domestic Corporate
    # T144(41,IY,IS)=MC_ZBECON(IY)-MC_ZBIVARW(IY)-MC_ZBIVADFIN521(IY)
    z[41] = dfd["MC_ZBECON"] - dfd["MC_ZBIVARW"] - dfd["MC_ZBIVADFIN521"]

    #       Rest-of-World
    # T144(42,IY,IS)=MC_ZBIVARW(IY)
    z[42] = dfd["MC_ZBIVARW"]

    #       Federal Reserve
    # T144(43,IY,IS)=MC_ZBIVADFIN521(IY)
    z[43] = dfd["MC_ZBIVADFIN521"]

    #

    #   Real After-Tax Profits
    # T144(44,IY,IS)=MC_ZAR(IY)
    z[44] = dfd["MC_ZAR"]

    #   Dividend Payout Ratio
    # T144(45,IY,IS)=100*MC_ZADIV(IY)/MC_ZA(IY)
    z[45] = 100 * dfd["MC_ZADIV"] / dfd["MC_ZA"]

    #

    #   Book-Value of Depreciation
    # T144(46,IY,IS)=MC_CKFCORPBK(IY)
    z[46] = dfd["MC_CKFCORPBK"]

    #     Less: Economic Depreciation
    # T144(47,IY,IS)=MC_CKFCORP(IY)
    z[47] = dfd["MC_CKFCORP"]

    #   Equals: Capital Consumption Adjustment
    # T144(48,IY,IS)=MC_CKFADJCORP(IY)
    z[48] = dfd["MC_CKFADJCORP"]

    #

    #   After-Tax Book Profits
    # T144(49,IY,IS)=MC_ZA(IY)
    z[49] = dfd["MC_ZA"]

    #     Plus: Book Value of Depreciation
    # T144(50,IY,IS)=MC_CKFCORPBK(IY)
    z[50] = dfd["MC_CKFCORPBK"]

    #     Plus: Inventory Valuation Adjustment
    # T144(51,IY,IS)=MC_IVACORP(IY)
    z[51] = dfd["MC_IVACORP"]

    #     Less: Dividends
    # T144(52,IY,IS)=MC_ZADIV(IY)
    z[52] = dfd["MC_ZADIV"]

    #     Less: Capital Transfers (Net)
    # T144(53,IY,IS)=MC_NETCAPTRF(IY)
    z[53] = dfd["MC_NETCAPTRF"]

    #   Equals: Net Cash Flow
    # T144(54,IY,IS)=MC_NETCFIVA(IY)
    z[54] = dfd["MC_NETCFIVA"]

    return z
