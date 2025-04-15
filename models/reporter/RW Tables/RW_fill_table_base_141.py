# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
SZO rewrote on 7/12/2024
"""


def fill_table_base_141(dfd, table_spec, table_id):
    """Fill table  Crude Oil Refinery Input Price by Refinery Region

    The function returns a dict in which each key is an integer
    references a table row, and each value is a dataframe indexed
    by region number. The integer keys are the same as "IROWS" in
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
        dict.txt shows:
            MC_JPGDP R 4 1 (MNUMY3) INDEX  87=1.0 Chained price index-gross domestic product

        MC_JPGDP started in 1987!

        So a simple df.pct_change() doesn't work correctly. Need shift the base year to 1990
        in computing the change percentage of Gross Domestic Product Deflator !

    """

    z = {}
    MNUMCR = dfd["MNUMCR_rwpre"]

    #   Summary of the U.S. Economy
    #   (percent change from previous year, unless otherwise noted)

    #   Component

    #   Gross Domestic Product
    # T141( 1,IY,IS) = ((MC_GDPR(IY)/MC_GDPR(IY-1))-1)*100
    z[1] = dfd["MC_GDPR"].pct_change() * 100

    #     Final Sales of Domestic Product
    # T141(2,IY,IS)=((MC_SFDPRODR(IY)/MC_SFDPRODR(IY-1))-1)*100
    z[2] = dfd["MC_SFDPRODR"].pct_change() * 100

    #   Total Consumption
    # T141(3,IY,IS)=((MC_CONSR(IY)/MC_CONSR(IY-1))-1)*100
    z[3] = dfd["MC_CONSR"].pct_change() * 100

    #     Durables
    # T141(4,IY,IS)=((MC_CDR(IY)/MC_CDR(IY-1))-1)*100
    z[4] = dfd["MC_CDR"].pct_change() * 100

    #     Nondurables
    # T141(5,IY,IS)=((MC_CNR(IY)/MC_CNR(IY-1))-1)*100
    z[5] = dfd["MC_CNR"].pct_change(1) * 100

    #     Services
    # T141(6,IY,IS)=((MC_CSVR(IY)/MC_CSVR(IY-1))-1)*100
    z[6] = dfd["MC_CSVR"].pct_change() * 100

    #   Nonresidential Fixed Investment
    # T141(7,IY,IS)=((MC_IFNRER(IY)/MC_IFNRER(IY-1))-1)*100
    z[7] = dfd["MC_IFNRER"].pct_change() * 100

    #     Equipment & Software
    # T141(8,IY,IS)=((MC_IFNREER(IY)/MC_IFNREER(IY-1))-1)*100
    z[8] = dfd["MC_IFNREER"].pct_change() * 100

    #       Information Processing Equipment
    # T141(9,IY,IS)=((MC_IFNREEIPR(IY)/MC_IFNREEIPR(IY-1))-1)*100
    z[9] = dfd["MC_IFNREEIPR"].pct_change() * 100

    #         Computers & Peripherals
    # T141(10,IY,IS)=((MC_IFNREEIPCCR(IY)/MC_IFNREEIPCCR(IY-1))-1)*100
    z[10] = dfd["MC_IFNREEIPCCR"].pct_change() * 100

    #         Communications Equipment
    # T141(11,IY,IS)=((MC_IFNREEIPCTR(IY)/MC_IFNREEIPCTR(IY-1))-1)*100
    z[11] = dfd["MC_IFNREEIPCTR"].pct_change() * 100

    #       Industrial Equipment
    # T141(12,IY,IS)=((MC_IFNREEINDR(IY)/MC_IFNREEINDR(IY-1))-1)*100
    z[12] = dfd["MC_IFNREEINDR"].pct_change() * 100

    #       Transportation Equipment
    # T141(13,IY,IS)=((MC_IFNREETR(IY)/MC_IFNREETR(IY-1))-1)*100
    z[13] = dfd["MC_IFNREETR"].pct_change() * 100

    #         Aircraft
    # T141(14,IY,IS)=((MC_IFNREETACR(IY)/MC_IFNREETACR(IY-1))-1)*100
    z[14] = dfd["MC_IFNREETACR"].pct_change() * 100

    #       Other Equipment
    # T141(15,IY,IS)=((MC_IFNREEOR(IY)/MC_IFNREEOR(IY-1))-1)*100
    z[15] = dfd["MC_IFNREEOR"].pct_change() * 100

    #     Structures
    # T141(16,IY,IS)=((MC_IFNRESR(IY)/MC_IFNRESR(IY-1))-1)*100
    z[16] = dfd["MC_IFNRESR"].pct_change() * 100

    #       Commercial & Health Care
    # T141(17,IY,IS)=((MC_IFNRESCR(IY)/MC_IFNRESCR(IY-1))-1)*100
    z[17] = dfd["MC_IFNRESCR"].pct_change() * 100

    #       Manufacturing
    # T141(18,IY,IS)=((MC_IFNRESMFGR(IY)/MC_IFNRESMFGR(IY-1))-1)*100
    z[18] = dfd["MC_IFNRESMFGR"].pct_change() * 100

    #       Power & Communication
    # T141(19,IY,IS)=((MC_IFNRESPR(IY)/MC_IFNRESPR(IY-1))-1)*100
    z[19] = dfd["MC_IFNRESPR"].pct_change() * 100

    #       Mining & Petroleum
    # T141(20,IY,IS)=((MC_IFNRESMIR(IY)/MC_IFNRESMIR(IY-1))-1)*100
    z[20] = dfd["MC_IFNRESMIR"].pct_change() * 100

    #       Other
    # T141(21,IY,IS)=((MC_IFNRESOR(IY)/MC_IFNRESOR(IY-1))-1)*100
    z[21] = dfd["MC_IFNRESOR"].pct_change() * 100

    #   Residential Fixed Investment
    # T141(22,IY,IS)=((MC_IFRER(IY)/MC_IFRER(IY-1))-1)*100
    z[22] = dfd["MC_IFRER"].pct_change() * 100

    #   Exports
    # T141(23,IY,IS)=((MC_XR(IY)/MC_XR(IY-1))-1)*100
    z[23] = dfd["MC_XR"].pct_change() * 100

    #   Imports
    # T141(24,IY,IS)=((MC_MR(IY)/MC_MR(IY-1))-1)*100
    z[24] = dfd["MC_MR"].pct_change() * 100

    #   Federal Government
    # T141(25,IY,IS)=((MC_GFR(IY)/MC_GFR(IY-1))-1)*100
    z[25] = dfd["MC_GFR"].pct_change() * 100

    #   State & Local Government
    # T141(26,IY,IS)=((MC_GSLR(IY)/MC_GSLR(IY-1))-1)*100
    z[26] = dfd["MC_GSLR"].pct_change() * 100

    #   (billion chain-weighted dollars)

    #     Real Gross Domestic Product (m__m dollars)
    # T141(27,IY,IS)=MC_GDPR(IY)
    z[27] = dfd["MC_GDPR"]

    #     Nominal Gross Domestic Product (dollars)
    # T141(28,IY,IS)=MC_GDP(IY)
    z[28] = dfd["MC_GDP"]

    #

    #   Prices & Wages, Percent Change, Annual Rate

    #     Gross Domestic Product Deflator
    # T141(29,IY,IS)=((MC_JPGDP(IY)/MC_JPGDP(IY-1))-1)*100
    # dfd['MC_JPGDP'].index
    # Index([ 1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18,
    #     19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36,
    #     37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,
    #     55, 56, 57, 58, 59, 60, 61, 62, 63, 64],
    #     dtype='int64', name='MNUMY3')
    # DIM  MNUMY3    64      MNUMYR + 1987-1989
    #
    # z[29] = dfd['MC_JPGDP'].T.pct_change() * 100 - not working!
    # Get inflation index from 1990->2050
    MC_JPGDP = dfd["MC_JPGDP"].loc[1990:]
    z[29] = MC_JPGDP.pct_change().dropna() * 100

    #     Consumer Prices
    # T141(30,IY,IS)=((MC_CPI(MNUMCR,IY)/MC_CPI(MNUMCR,IY-1))-1)*100
    z[30] = dfd["MC_CPI"].loc[MNUMCR].pct_change() * 100

    #       Excluding Food & Energy
    # T141(31,IY,IS)=((MC_CPIXFAE(IY)/MC_CPIXFAE(IY-1))-1)*100
    z[31] = dfd["MC_CPIXFAE"].pct_change() * 100

    #     Producer Prices, Finished Goods
    # T141(32,IY,IS)=((MC_WPISOP3000(IY)/MC_WPISOP3000(IY-1))-1)*100
    z[32] = dfd["MC_WPISOP3000"].pct_change() * 100

    #     Employment Cost Index - Total Composition
    # T141(33,IY,IS)=((MC_JECIWSSP(IY)/MC_JECIWSSP(IY-1))-1)*100
    z[33] = dfd["MC_JECIWSSP"].pct_change() * 100

    #   Other Key Measures

    #     Oil Price - WTI (dollars per barrel)
    # T141(34,IY,IS)=MC_POILWTI(IY)
    z[34] = dfd["MC_POILWTI"]

    #     Productivity (percent change)
    # T141(35,IY,IS)=((MC_JQPCMHNF(IY)/MC_JQPCMHNF(IY-1))-1)*100
    z[35] = dfd["MC_JQPCMHNF"].pct_change() * 100

    #     Industrial Production (percent change)
    # T141(36,IY,IS)=((MC_IPSB50001(IY)/MC_IPSB50001(IY-1))-1)*100
    z[36] = dfd["MC_IPSB50001"].pct_change() * 100

    #     Factory Operating Rate
    # T141(37,IY,IS)=MC_UTLB00004(IY)
    z[37] = dfd["MC_UTLB00004"]

    #     Nonfarm Inventory Change (billion m__m dollars)
    # T141(38,IY,IS)=MC_IINFR(IY)
    z[38] = dfd["MC_IINFR"]

    #     Consumer Sentiment Index
    # T141(39,IY,IS)=MC_JCSMICH(IY)
    z[39] = dfd["MC_JCSMICH"]

    #     Light Vehicle Sales (million units)
    # T141(40,IY,IS)=MC_SUVLV(IY)
    z[40] = dfd["MC_SUVLV"]

    #     Housing Starts (million units)
    # T141(41,IY,IS)=MC_HUSPS(IY)
    z[41] = dfd["MC_HUSPS"]

    #     Total Existing House Sales (million units)
    # T141(42,IY,IS)=MC_HUESOLD(IY)
    z[42] = dfd["MC_HUESOLD"]

    #     Unemployment Rate (percent)
    # T141(43,IY,IS)=MC_RUC(IY)
    z[43] = dfd["MC_RUC"]

    #     Payroll Employment (percent change)
    # T141(44,IY,IS)=((MC_EEA(IY)/MC_EEA(IY-1))-1)*100
    z[44] = dfd["MC_EEA"].pct_change() * 100

    #     Federal Surplus (unified, FY, billion dollars)
    # T141(45,IY,IS)=MC_NETSAVGFUNIFY(IY)
    z[45] = dfd["MC_NETSAVGFUNIFY"]

    #     Current Account Balance (billion dollars)
    # T141(46,IY,IS)=MC_BOPCRNTAC(IY)
    z[46] = dfd["MC_BOPCRNTAC"]

    #   Financial Markets, NSA

    #     Federal Funds Rate (percent)
    # T141(47,IY,IS)=MC_RMFF(IY)
    z[47] = dfd["MC_RMFF"]

    #     3-Month Treasury Bill Rate (percent)
    # T141(48,IY,IS)=MC_RMTB3M(IY)
    z[48] = dfd["MC_RMTB3M"]

    #     10-Year Treasury Note Yield (percent)
    # T141(49,IY,IS)=MC_RMTCM10Y(IY)
    z[49] = dfd["MC_RMTCM10Y"]

    #     30-Year Fixed Mortgage Rate (percent)
    # T141(50,IY,IS)=MC_RMMTG30CON(IY)
    z[50] = dfd["MC_RMMTG30CON"]

    #     S&P 500 Stock Index
    # T141(51,IY,IS)=MC_SP500(IY)
    z[51] = dfd["MC_SP500"]

    #       as percent change
    # T141(52,IY,IS)=((MC_SP500(IY)/MC_SP500(IY-1))-1)*100
    z[52] = dfd["MC_SP500"].pct_change() * 100

    #     Exchange Rate, Major Trading Partners
    # T141(53,IY,IS)=MC_JEXCHMTP(IY)
    z[53] = dfd["MC_JEXCHMTP"]

    #       as percent change
    # T141(54,IY,IS)=((MC_JEXCHMTP(IY)/MC_JEXCHMTP(IY-1))-1)*100
    z[54] = dfd["MC_JEXCHMTP"].pct_change() * 100

    #   Incomes

    #     Personal Income (percent change)
    # T141(55,IY,IS)=((MC_YP(MNUMCR,IY)/MC_YP(MNUMCR,IY-1))-1)*100
    z[55] = dfd["MC_YP"].loc[MNUMCR].pct_change() * 100

    #     Real Disposable Income (percent change)
    # T141(56,IY,IS)=((MC_YPDR(MNUMCR,IY)/MC_YPDR(MNUMCR,IY-1))-1)*100
    z[56] = dfd["MC_YPDR"].loc[MNUMCR].pct_change() * 100

    #     Saving Rate (percent)
    # T141(57,IY,IS)=MC_SAVPRATE(IY)
    z[57] = dfd["MC_SAVPRATE"]

    #     After-Tax Profits (billion nominal dollars)
    # T141(58,IY,IS)=MC_ZA(IY)
    z[58] = dfd["MC_ZA"]

    #       as percent change
    # T141(59,IY,IS)=((MC_ZA(IY)/MC_ZA(IY-1))-1)*100
    z[59] = dfd["MC_ZA"].pct_change() * 100

    #     Disposable Income (percent change)
    # T141(60,IY,IS)=((MC_YPD(IY)/MC_YPD(IY-1))-1)*100
    z[60] = dfd["MC_YPD"].pct_change() * 100

    return z
