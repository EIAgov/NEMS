# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
SZO Fixed the percentage calculations on 7/10/2024
"""


def fill_table_base_018(dfd, table_spec, table_id):
    """Fill table Macroeconomic Indicators.

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
    # d_base = {}

    MNUMCR = 11
    TRIL_TO_QUAD = 0.001
    # MACYR = 2012 #read from citation.txt -  code in ftab2.f
    MACYR = table_spec["MACYR"]

    # Macroeconomic Indicators
    # (billion m__m chain-weighted dollars, unless otherwise noted)
    # Indicators
    #
    # Real Gross Domestic Product
    # T18(2,IY,IS)=MC_GDPR(IY)
    z[2] = dfd["MC_GDPR"]
    # Real Potential Gross Domestic Product
    # T18(32,IY,IS)=MC_GDPFER(IY)
    z[32] = dfd["MC_GDPFER"]
    # Components of Real Gross Domestic Product
    # Real Consumption
    # T18(3,IY,IS)=MC_CONSR(IY)
    z[3] = dfd["MC_CONSR"]
    # Real Business Fixed Investment
    # T18(4,IY,IS)=MC_IFNRER(IY)
    z[4] = dfd["MC_IFNRER"]
    # Real Government Spending
    # T18(5,IY,IS)=MC_GR(IY)
    z[5] = dfd["MC_GR"]
    # Real Exports
    # T18(6,IY,IS)=MC_XR(IY)
    z[6] = dfd["MC_XR"]
    # Real Imports
    # T18(7,IY,IS)=MC_MR(IY)
    z[7] = dfd["MC_MR"]

    # Energy Intensity
    # (thousand Btu per m__m dollar of GDP)
    # Delivered Energy
    # T18(12,IY,IS)=T2(76,MNUMCR,IY,IS)/MC_GDPR(IY)*1000.
    # Create a place holder here, and implement in the postprocessor
    z[12] = 1 / dfd["MC_GDPR"] * 1000

    #     Total Energy
    # T18(13,IY,IS)=T2(106,MNUMCR,IY,IS)/MC_GDPR(IY)*1000.
    # Create a place holder here, and implement in the postprocessor
    z[13] = 1 / dfd["MC_GDPR"] * 1000

    #
    #   Price Indices
    #     GDP Chain-type Price Index (m__m=1.000)
    # T18(1,IY,IS)=MC_JPGDP(IY)/MC_JPGDP(MACYR-1989)
    # Get inflation index from 1990->2050

    # MC_JPGDP = dfd['MC_JPGDP'].loc[4:].T
    # # Rename columns: 1-61
    # MC_JPGDP.columns = list(range(1,len(table_spec['years'])+1))

    # z[1] = dfd['MC_JPGDP']/dfd['MC_JPGDP'].loc[MACYR-1989]
    # z[1] = z[1].iloc[:61,0]

    # Song Modified on 7/23/2024
    # Get MC_JPGDP for 1990-2050 (it starts in 1987)
    MC_JPGDP = dfd["MC_JPGDP"].loc[1990:]
    # Rename columns: 1-61
    MC_JPGDP.columns = list(range(1, 61 + 1))
    z[1] = MC_JPGDP / MC_JPGDP.iloc[MACYR - 1990]

    # Consumer Price Index (1982-84=1.00)
    # All-urban
    # T18(14,IY,IS)=MC_CPI(11,IY)
    z[14] = dfd["MC_CPI"].loc[MNUMCR]
    # Energy Commodities and Services
    # T18(35,IY,IS)=MC_CPIE(IY)
    z[35] = dfd["MC_CPIE"]
    # Wholesale Price Index (1982=1.00)
    # All Commodities
    # T18(33,IY,IS)=MC_WPI(IY)
    z[33] = dfd["MC_WPI"]
    # Fuel and Power
    # T18(34,IY,IS)=MC_WPI05(IY)
    z[34] = dfd["MC_WPI05"]
    # Metals and Metal Products
    # T18(38,IY,IS)=MC_WPI10(IY)
    z[38] = dfd["MC_WPI10"]
    # Industrial Commodities excluding Energy
    # T18(41,IY,IS)=MC_WPIIND05(IY)
    z[41] = dfd["MC_WPIIND05"]
    # Interest Rates (percent, nominal)
    # Federal Funds Rate
    # T18(10,IY,IS)=MC_RMFF(IY)
    z[10] = dfd["MC_RMFF"]
    # 10-Year Treasury Note
    # T18(11,IY,IS)=MC_RMTCM10Y(IY)
    z[11] = dfd["MC_RMTCM10Y"]
    # AA Utility Bond Rate
    # T18(9,IY,IS)=MC_RMCORPPUAA(IY)
    z[9] = dfd["MC_RMCORPPUAA"]

    # Value of Shipments (billion m==m dollars)
    # Non-Industrial and Service Sectors
    # T18(39,IY,IS)=SUM(MC_REVSER(11,1:10,IY))
    z[39] = dfd["MC_REVSER"].loc[MNUMCR].loc[1:10].sum()
    # Total Industrial
    # T18(20,IY,IS)=T18(20,IY,IS)+MC_REVIND(11,III,IY)
    # have to look at code in ftab.f for this section
    z[20] = 0
    z[21] = 0
    z[22] = 0
    z[23] = 0
    z[24] = 0
    for III in range(1, 49):
        if III in [2, 3, 4, 5, 21, 22, 23, 24, 11, 12, 13, 17, 29]:
            continue  # These are subsets of other elements
        # Total Industrial
        z[20] += dfd["MC_REVIND"].loc[MNUMCR].loc[III]
        if III <= 41:
            # Manufacturing
            z[22] += dfd["MC_REVIND"].loc[MNUMCR].loc[III]
            # Energy-Intensive
            if III in [1, 10, 15, 16, 17, 18, 19, 25, 28, 30, 31, 33, 34]:
                z[23] += dfd["MC_REVIND"].loc[MNUMCR].loc[III]
            else:
                # Non-Energy-Intensive
                z[24] += dfd["MC_REVIND"].loc[MNUMCR].loc[III]
        else:
            # Agriculture, Mining, and Construction
            z[21] += dfd["MC_REVIND"].loc[MNUMCR].loc[III]

    #   Total Shipments
    # T18(40,IY,IS)= T18(20,IY,IS)+T18(39,IY,IS)
    z[40] = z[20] + z[39]
    #
    #   Population and Employment (millions)
    #     Population, with Armed Forces Overseas
    # T18(26,IY,IS)=MC_NP(11,IY)
    z[26] = dfd["MC_NP"].loc[MNUMCR]
    #     Population, aged 16 and over
    # T18(27,IY,IS)=MC_NP16A(11,IY)
    z[27] = dfd["MC_NP16A"].loc[MNUMCR]
    #     Population, aged 65 and over
    # T18(36,IY,IS)=MC_NP65A(IY)
    z[36] = dfd["MC_NP65A"]
    # Employment, Manufacturing
    # T18(29,IY,IS)=SUM(MC_EMPNA(11,1:19,IY))
    z[29] = dfd["MC_EMPNA"].loc[MNUMCR].loc[1:19].sum()
    #     Employment, Nonfarm
    # T18(28,IY,IS)=T18(29,IY,IS)+SUM(MC_EMPNA(11,22:39,IY))
    z[28] = z[29] + dfd["MC_EMPNA"].loc[MNUMCR].loc[22:39].sum()
    # Key Labor Indicators
    # Labor Force (millions)
    # T18(30,IY,IS)=MC_NLFC(IY)
    z[30] = dfd["MC_NLFC"]
    # Nonfarm Labor Productivity (2012=1.00)
    # T18(37,IY,IS)=MC_JQPCMHNF(IY)
    z[37] = dfd["MC_JQPCMHNF"]
    #     Unemployment Rate (percent)
    # T18(15,IY,IS)=MC_RUC(IY)
    z[15] = dfd["MC_RUC"]
    #   Key Indicators for Energy Demand
    #     Real Disposable Personal Income
    # T18(8,IY,IS)=MC_YPDR(11,IY)
    z[8] = dfd["MC_YPDR"].loc[MNUMCR]
    #     Housing Starts (millions)
    # T18(31,IY,IS)=FSUM(T18(16,IY,IS),3)
    z[16] = dfd["MC_HUSPS1"].loc[MNUMCR]
    z[17] = dfd["MC_HUSPS2A"].loc[MNUMCR]
    z[18] = dfd["MC_HUSMFG"].loc[MNUMCR]
    z[31] = z[16] + z[17] + z[18]
    #     Commercial Floorspace (billion square feet)
    # T18(19,IY,IS)=CMUSSURVFLOORTOT(IY)+CMUSNEWFLOORTOT(IY)
    z[19] = dfd["CMUSSURVFLOORTOT"] + dfd["CMUSNEWFLOORTOT"]
    #     Unit Sales of Light-Duty Vehicles (millions)
    # T18(25,IY,IS)=MC_SUVTL(IY)+MC_SUVA(IY)
    z[25] = dfd["MC_SUVTL"] + dfd["MC_SUVA"]

    return z
