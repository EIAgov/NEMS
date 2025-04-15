# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
SZO Fixed cross-referencing on 7/10/20124
"""
import pandas as pd


def fill_table_base_020(dfd, table_spec, table_id):
    """Fill table for  Conversion Factors

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

    Note: SZO
    Note: A lot of variables read from the restart are scalar variables shape = (1,1)
    Solution: Create a blank df of 61 years, and assign the scalar  to each column

    """

    z = {}

    MNUMCR = dfd["MNUMCR_rwpre"]
    MNUMPR = dfd["MNUMPR_rwpre"]
    MNUMLR = dfd["MNUMLR_rwpre"]

    # Note: A lot of variables read from the restart are scalar variables shape = (1,1)
    # Solution: Create a blank df of 61 years, and assign the scalar  to each column
    years = table_spec["years"]
    df = pd.Series(0, years)

    #   Conversion Factors
    #   (from physical units to million Btu)
    #   Petroleum and Other Liquids

    #   (million Btu per barrel)
    #     Asphalt and Road Oil
    # T20(1,IY,IS)=CFASQ
    # z[1] = dfd['CFASQ']
    df[years] = dfd["CFASQ"].iloc[0]
    z[1] = df.copy()

    #     Aviation Gasoline
    # T20(64,IY,IS)=CFAVQ
    # z[64] = dfd['CFAVQ']
    df[years] = dfd["CFAVQ"].iloc[0]
    z[64] = df.copy()

    #     Biodiesel
    # T20(60,IY,IS)=CFBIOD(IY)
    # z[60] = dfd['CFBIOD']
    df[years] = dfd["CFBIOD"].iloc[0]
    z[60] = df.copy()

    # #     Butane
    # #T20(2,IY,IS)=CFBUQ
    # z[2] = dfd['CFBUQ']
    df[years] = dfd["CFBUQ"].iloc[0]
    z[2] = df.copy()

    # #     Distillate Fuel Oil
    # #T20(3,IY,IS)=CFDSQ
    # z[3] = dfd['CFDSQ']
    df[years] = dfd["CFDSQ"].iloc[0]
    z[3] = df.copy()

    #       Residential
    # T20(40,IY,IS)=CFDSRS(IY)
    z[40] = dfd["CFDSRS"]

    #       Commercial
    # T20(41,IY,IS)=CFDSCM(IY)
    z[41] = dfd["CFDSCM"]

    #       Transportation
    # T20(42,IY,IS)=CFDSTR(IY)
    z[42] = dfd["CFDSTR"]

    #       Industrial
    # T20(43,IY,IS)=CFDSIN(IY)
    z[43] = dfd["CFDSIN"]

    #       Electric Power
    # T20(44,IY,IS)=CFDSEL(IY)
    z[44] = dfd["CFDSEL"]

    #         Total Distillate
    # T20(45,IY,IS)=CFDSQT(IY)
    z[45] = dfd["CFDSQT"]

    #     Distillate Fuel Oil - Low Sulfur Diesel
    # T20(73,IY,IS)=CFDSLQ(IY)
    z[73] = dfd["CFDSLQ"]

    #     Distillate Fuel Oil - Ultra Low Sulfur Diesel
    # T20(74,IY,IS)=CFDSUQ(IY)
    z[74] = dfd["CFDSUQ"]

    #     Distillate Fuel Oil - California Diesel
    # T20(72,IY,IS)=CFDSCQ(IY)
    z[72] = dfd["CFDSCQ"]

    #     Ethane
    # T20(65,IY,IS)=CFEEQ
    # z[65] = dfd['CFEEQ']
    df[years] = dfd["CFEEQ"].iloc[0]
    z[65] = df.copy()

    #     Ethanol including denaturant
    # T20(4,IY,IS)=CFETQ(IY)
    z[4] = dfd["CFETQ"]

    #     E85 1/
    # T20(56,IY,IS)=CFE85Q(IY)
    z[56] = dfd["CFE85Q"]

    #     Isobutane
    # T20(5,IY,IS)=CFIBQ
    # z[5] = dfd['CFIBQ']
    df[years] = dfd["CFIBQ"].iloc[0]
    z[5] = df.copy()

    #     Jet Fuel - Kerosene
    # T20(6,IY,IS)=CFJFK
    # z[6] = dfd['CFJFK']
    df[years] = dfd["CFJFK"].iloc[0]
    z[6] = df.copy()

    #     Jet Fuel - Naphtha
    # T20(7,IY,IS)=CFJFN
    # z[7] = dfd['CFJFN']
    df[years] = dfd["CFJFN"].iloc[0]
    z[7] = df.copy()

    #       Total Jet Fuel
    # T20(81,IY,IS)=CFJFQ(IY)
    z[81] = dfd["CFJFQ"]

    #     Kerosene
    # T20(8,IY,IS)=CFKSQ
    # z[8] = dfd['CFKSQ']
    df[years] = dfd["CFKSQ"].iloc[0]
    z[8] = df.copy()

    #     Liquefied Petroleum Gases and Other
    # T20(9,IY,IS)=CFLGQ(IY)
    z[9] = dfd["CFLGQ"]

    #     Lubricants
    # T20(66,IY,IS)=CFLUQ
    # z[66] = dfd['CFLUQ']
    df[years] = dfd["CFLUQ"].iloc[0]
    z[66] = df.copy()

    #     Methanol
    # T20(57,IY,IS)=CFMEQT
    # z[57] = dfd['CFMEQT']
    df[years] = dfd["CFMEQT"].iloc[0]
    z[57] = df.copy()

    #     M85
    # T20(58,IY,IS)=CFM85Q(IY)
    z[58] = dfd["CFM85Q"]

    #     Motor Gasoline Average
    # T20(10,IY,IS)=CFMGQ(IY)
    z[10] = dfd["CFMGQ"]

    #        Conventional Motor Gasoline
    # T20(52,IY,IS)=CFTGQ(IY)
    z[52] = dfd["CFTGQ"]

    #        Reformulated Motor Gasoline
    # T20(53,IY,IS)=CFRGQ(IY)
    z[53] = dfd["CFRGQ"]

    #        California Air Resource Board Motor Gas
    # T20(55,IY,IS)=APICAMG(2,IY)
    z[55] = dfd["APICAMG"].loc[2]

    #        Conventional before Blending
    # T20(98,IY,IS)=CFCBOB(IY)
    z[98] = dfd["CFCBOB"]

    #        Reformulated before Blending
    # T20(99,IY,IS)=CFRBOB(IY)
    z[99] = dfd["CFRBOB"]

    #        California before Blending
    # T20(100,IY,IS)=CFCBQ(IY)
    z[100] = dfd["CFCBQ"]

    #     Naphthas, Special or Otherwise
    # T20(63,IY,IS)=CFNPQ
    # z[63] = dfd['CFNPQ']
    df[years] = dfd["CFNPQ"].iloc[0]
    z[63] = df.copy()

    #     Natural Gasoline
    # T20(12,IY,IS)=CFPPQ
    # z[12] = dfd['CFPPQ']
    df[years] = dfd["CFPPQ"].iloc[0]
    z[12] = df.copy()

    #     Other Petroleum
    # T20(11,IY,IS)=CFOTQ(IY)
    z[11] = dfd["CFOTQ"]

    #     Petrochemical Feedstocks
    # T20(13,IY,IS)=CFPFQ(IY)
    z[13] = dfd["CFPFQ"]

    #     Petroleum Coke
    # T20(14,IY,IS)=CFPCQ
    # z[14] = dfd['CFPCQ']
    df[years] = dfd["CFPCQ"].iloc[0]
    z[14] = df.copy()

    #     Catalytic Petroleum Coke
    # T20(82,IY,IS)=CFCCQ(IY)
    z[82] = dfd["CFCCQ"]

    #     Propane
    # T20(15,IY,IS)=CFPRQ
    # z[15] = dfd['CFPRQ']
    df[years] = dfd["CFPRQ"].iloc[0]
    z[15] = df.copy()

    #     Residual Fuel
    # T20(16,IY,IS)=CFRSQ
    # z[16] = dfd['CFRSQ']
    df[years] = dfd["CFRSQ"].iloc[0]
    z[16] = df.copy()

    #     Still Gas
    # T20(17,IY,IS)=CFSGQ
    # z[17] = dfd['CFSGQ']
    df[years] = dfd["CFSGQ"].iloc[0]
    z[17] = df.copy()

    #     Unfinished Oils
    # T20(70,IY,IS)=CFIMUO(IY)
    z[70] = dfd["CFIMUO"]

    #     Waxes
    # T20(67,IY,IS)=CFWXQ
    # z[67] = dfd['CFWXQ']
    df[years] = dfd["CFWXQ"].iloc[0]
    z[67] = df.copy()

    #     Miscellaneous Petroleum
    # T20(68,IY,IS)=CFMSQ
    # z[68] = dfd['CFMSQ']
    df[years] = dfd["CFMSQ"].iloc[0]
    z[68] = df.copy()

    #     Total Petroleum Consumption
    # T20(38,IY,IS)=T1(19,IY,IS)/(T11(48,IY,IS)*365./1000.)
    # Create a place holder here, and implement in the postprocessor
    z[38] = z[68] * 0

    #     Plant Condensate and Unfractionated Stream
    # T20(69,IY,IS)=CFUSQ
    # z[69] = dfd['CFUSQ']
    df[years] = dfd["CFUSQ"].iloc[0]
    z[69] = df.copy()

    #     Petroleum Product Imports    moved to postprocessor
    # T20(33,IY,IS)=T1(10,IY,IS)/(((BIODIMP(11,IY)+ETHIMP(11,IY)+RENEWDIMP(11,IY))/1000.+FSUM(T11(9,IY,IS),3))*365./1000.)
    z[33] = z[68] * 0

    #     Petroleum Product Imports from CFIMPRD
    # T20(71,IY,IS)=CFIMPRD(IY)
    z[71] = dfd["CFIMPRD"]

    #     Petroleum Product Exports
    # T20(34,IY,IS)=CFEXPRD(IY)
    z[34] = dfd["CFEXPRD"]

    #     Crude Oil
    #       Production
    # T20(35,IY,IS)=CFCRDDOM(IY)
    z[35] = dfd["CFCRDDOM"]

    #       Imports
    # T20(36,IY,IS)=CFCRDIMP(IY)
    z[36] = dfd["CFCRDIMP"]

    #       Exports
    # T20(83,IY,IS)=CFCRDEXP(IY)
    z[83] = dfd["CFCRDEXP"]

    #       API 35-40 Sweet
    # T20(84,IY,IS)=CFCRDLTSWT(IY)
    z[84] = dfd["CFCRDLTSWT"]

    #       API 35+ Sour
    # T20(85,IY,IS)=CFCRDLTSOUR(IY)
    z[85] = dfd["CFCRDLTSOUR"]

    #       API 27-35 Medium Sour
    # T20(86,IY,IS)=CFCRDMD2SOUR(IY)
    z[86] = dfd["CFCRDMD2SOUR"]

    #       API 27-35 Sour
    # T20(87,IY,IS)=CFCRDMDSOUR(IY)
    z[87] = dfd["CFCRDMDSOUR"]

    #       API less than 27 Sweet
    # T20(88,IY,IS)=CFCRDHVSWT(IY)
    z[88] = dfd["CFCRDHVSWT"]

    #       API less than 27 Sour
    # T20(89,IY,IS)=CFCRDHVSOUR(IY)
    z[89] = dfd["CFCRDHVSOUR"]

    #       California
    # T20(90,IY,IS)=CFCRDCA(IY)
    z[90] = dfd["CFCRDCA"]

    #       Syncrude
    # T20(91,IY,IS)=CFCRDSYN(IY)
    z[91] = dfd["CFCRDSYN"]

    #       Dilbit/Synbit
    # T20(92,IY,IS)=CFCRDDILBIT(IY)
    z[92] = dfd["CFCRDDILBIT"]

    #       API 40-50 Sweet
    # T20(93,IY,IS)=CFCRDLT2SWT(IY)
    z[93] = dfd["CFCRDLT2SWT"]

    #       API 50+ Sweet
    # T20(94,IY,IS)=CFCRDLSCOND(IY)
    z[94] = dfd["CFCRDLSCOND"]

    #     Natural Gas Plant Liquids
    # T20(37,IY,IS)=CFNGL(IY)
    z[37] = dfd["CFNGL"]

    #

    #

    #

    #   Natural Gas (thousand Btu per cubic foot)

    #     Consumption
    # T20(20,IY,IS)=CFNGC(IY)
    z[20] = dfd["CFNGC"]

    #       Electric Power Sector 2/
    # T20(18,IY,IS)=CFNGU(IY)
    z[18] = dfd["CFNGU"]

    #       End-use Sector
    # T20(19,IY,IS)=CFNGN(IY)
    z[19] = dfd["CFNGN"]

    #     Production
    # T20(20,IY,IS)=CFNGC(IY)
    z[20] = dfd["CFNGC"]

    #     Imports
    # T20(21,IY,IS)=CFNGI(IY)
    z[21] = dfd["CFNGI"]

    #     Exports
    # T20(22,IY,IS)=CFNGE(IY)
    z[22] = dfd["CFNGE"]

    #     Compressed/Liquefied Natural Gas
    # T20(62,IY,IS)=CFCNGQ(IY)
    z[62] = dfd["CFCNGQ"]

    #

    #   Coal (million Btu per short ton)

    #     Production
    # T20(25,IY,IS)=CQSBT(3,IY)
    z[25] = dfd["CQSBT"].loc[3]

    #       East of the Mississippi
    # T20(23,IY,IS)=CQSBT(1,IY)
    z[23] = dfd["CQSBT"].loc[1]

    #       West of the Mississippi
    # T20(24,IY,IS)=CQSBT(2,IY)
    z[24] = dfd["CQSBT"].loc[2]

    #     Consumption
    # T20(39,IY,IS)=((QCLRS(11,IY)+QCLCM(11,IY))*CQDBFT(11,1,IY)+QMCIN(11,IY)*CQDBFT(11,3,IY)+QCLEL(11,IY)*CQDBFT(11,6,IY)+(QCLIN(11,IY))*CQDBFT(11,2,IY)+QCLSN(11,IY)*CQDBFT(11,4,IY))/(QCLRS(11,IY)+QCLCM(11,IY)+QMCIN(11,IY)+QCLEL(11,IY)+QCLIN(11,IY)+QCLSN(11,IY))
    z[39] = (
        (dfd["QCLRS"].loc[MNUMCR] + dfd["QCLCM"].loc[MNUMCR])
        * dfd["CQDBFT"].loc[MNUMCR].loc[1]
        + dfd["QMCIN"].loc[MNUMCR] * dfd["CQDBFT"].loc[MNUMCR].loc[3]
        + dfd["QCLEL"].loc[MNUMCR] * dfd["CQDBFT"].loc[MNUMCR].loc[6]
        + (dfd["QCLIN"].loc[MNUMCR])
        * dfd["CQDBFT"].loc[MNUMCR].loc[2]
        + dfd["QCLSN"].loc[MNUMCR] * dfd["CQDBFT"].loc[11].loc[4]
    ) / (
        dfd["QCLRS"].loc[11]
        + dfd["QCLCM"].loc[MNUMCR]
        + dfd["QMCIN"].loc[MNUMCR]
        + dfd["QCLEL"].loc[MNUMCR]
        + dfd["QCLIN"].loc[MNUMCR]
        + dfd["QCLSN"].loc[MNUMCR]
    )

    #       Commercial and Institutional
    # T20(26,IY,IS)=CQDBFT(11,1,IY)
    z[26] = dfd["CQDBFT"].loc[MNUMCR].loc[1]

    #       Industrial 3/
    # T20(27,IY,IS)=CQDBFT(11,2,IY)
    z[27] = dfd["CQDBFT"].loc[MNUMCR].loc[2]

    #       Coking
    # T20(28,IY,IS)=CQDBFT(11,3,IY)
    z[28] = dfd["CQDBFT"].loc[MNUMCR].loc[3]

    #       Electric Power 2/
    # T20(30,IY,IS)=CQDBFT(11,6,IY)
    z[30] = dfd["CQDBFT"].loc[MNUMCR].loc[6]

    #     Imports
    # T20(31,IY,IS)=CQDBFT(11,7,IY)
    z[31] = dfd["CQDBFT"].loc[MNUMCR].loc[7]

    #     Exports
    # T20(29,IY,IS)=CQDBFT(11,5,IY)
    z[29] = dfd["CQDBFT"].loc[MNUMCR].loc[5]

    #     Waste Coal
    # T20(50,IY,IS)=SUM(WC_PROD_BTU(11,1:MNUMLR,IY))/SUM(WC_PROD_ST(11,1:MNUMLR,IY))
    z[50] = dfd["WC_PROD_BTU"].loc[MNUMCR].loc[1:MNUMLR].sum() / dfd["WC_PROD_ST"].loc[
        MNUMCR
    ].loc[1:MNUMLR].sum().replace(0, 1)

    #

    #   Approximate Heat Rates and Heat Content

    #   (Btu per kilowatthour)

    #     Electricity Heat Content
    # T20(32,IY,IS)=CFELQ
    # z[32] = dfd['CFELQ']
    df[years] = dfd["CFELQ"].iloc[0]
    z[32] = df.copy()

    #     Fossil Fuel Heat Rate
    # T20(110,IY,IS)=WHRFOSS(MNUMCR,IY)
    z[110] = dfd["WHRFOSS"].loc[MNUMCR]

    #       New England
    # T20(101:109,IY,IS)=WHRFOSS(1:9,IY)
    z[101] = dfd["WHRFOSS"].loc[1]

    #       Middle Atlantic
    # T20(101:109,IY,IS)=WHRFOSS(1:9,IY)
    z[102] = dfd["WHRFOSS"].loc[2]

    #       East North Central
    # T20(101:109,IY,IS)=WHRFOSS(1:9,IY)
    z[103] = dfd["WHRFOSS"].loc[3]

    #       West North Central
    z[104] = dfd["WHRFOSS"].loc[4]
    #       South Atlantic
    # T20(101:109,IY,IS)=WHRFOSS(1:9,IY)
    z[105] = dfd["WHRFOSS"].loc[5]

    #       East South Central
    # T20(101:109,IY,IS)=WHRFOSS(1:9,IY)
    z[106] = dfd["WHRFOSS"].loc[6]

    #       West South Central
    # T20(101:109,IY,IS)=WHRFOSS(1:9,IY)
    z[107] = dfd["WHRFOSS"].loc[7]

    #       Mountain
    # T20(101:109,IY,IS)=WHRFOSS(1:9,IY)
    z[108] = dfd["WHRFOSS"].loc[8]

    #       Pacific
    # T20(101:109,IY,IS)=WHRFOSS(1:9,IY)
    z[109] = dfd["WHRFOSS"].loc[9]

    #

    #   Corn (thousand Btu/bushel)
    # T20(47,IY,IS)=CFCORN/1000.
    # z[47] = dfd['CFCORN'] / 1000.
    df[years] = dfd["CFCORN"].iloc[0] / 1000.0
    z[47] = df.copy()

    #   Other Grain (thousand Btu/bushel)
    # T20(77,IY,IS)=CFGRAIN(IY)/1000.
    z[77] = dfd["CFGRAIN"] / 1000.0

    #   Cellulose (Switchgrass) (thousand Btu/bushel)
    # T20(48,IY,IS)=CFCELL/1000.
    # z[48] = dfd['CFCELL'] / 1000.
    df[years] = dfd["CFCELL"].iloc[0] / 1000.0
    z[48] = df.copy()

    #   Cellulosic Yield (gallon per short ton)
    # T20(51,IY,IS)=CONEFF(IY)
    z[51] = dfd["CONEFF"]

    #   Cellulose (million Btu per ton)
    # T20(59,IY,IS)=CFBMQ(IY)
    z[59] = dfd["CFBMQ"]

    #   Vegetable Oil as calculated from biodiesel out
    # T20(61,IY,IS)=CFVEGGIE(IY)
    z[61] = dfd["CFVEGGIE"]

    #   Liquids from Biomass
    # T20(75,IY,IS)=CFBTLLIQ(IY)
    z[75] = dfd["CFBTLLIQ"]

    return z
