# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_057(dfd, table_spec, table_id):
    """Fill table Air Travel Energy Use
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




    #   Air Travel Energy Use
    #    Indicators                                                 
    #                                                               
    #   Fuel Cost (1987 dollars per million Btu)
    z[1] = dfd['AIROUT'].loc[1]

    #   Load Factor (fraction of seats filled)

    #     U.S. Domestic
    z[2] = dfd['AIROUT'].loc[2]

    #     U.S. International
    z[3] = dfd['AIROUT'].loc[3]

    #
    #   Driver Variables

    #     Gross Domestic Product

    #     (billion 2015 $ chain-weighted dollars)

    #       United States
    z[221] = dfd['WLD_GDP'].loc[1]

    #       Canada
    z[222] = dfd['WLD_GDP'].loc[2]

    #       Mexico and other OECD Americas
    z[223] = dfd['WLD_GDP'].loc[3]

    #       OECD Europe
    z[224] = dfd['WLD_GDP'].loc[4]

    #       Japan
    z[225] = dfd['WLD_GDP'].loc[5]

    #       Australia and New Zealand
    z[226] = dfd['WLD_GDP'].loc[6]

    #       South Korea
    z[227] = dfd['WLD_GDP'].loc[7]

    #       Russia
    z[228] = dfd['WLD_GDP'].loc[8]

    #       Other Europe and Eurasia
    z[229] = dfd['WLD_GDP'].loc[9]

    #       China
    z[230] = dfd['WLD_GDP'].loc[10]

    #       India
    z[231] = dfd['WLD_GDP'].loc[11]

    #       Other Non-OECD Asia
    z[232] = dfd['WLD_GDP'].loc[12]

    #       Middle East
    z[233] = dfd['WLD_GDP'].loc[13]

    #       Africa
    z[234] = dfd['WLD_GDP'].loc[14]

    #       Brazil
    z[235] = dfd['WLD_GDP'].loc[15]

    #       Other Non-OECD Americas
    z[236] = dfd['WLD_GDP'].loc[16]

    #     Population (millions)

    #       United States
    z[237] = dfd['WLD_POP'].loc[1]

    #       Canada
    z[238] = dfd['WLD_POP'].loc[2]

    #       Mexico and other OECD Americas
    z[239] = dfd['WLD_POP'].loc[3]

    #       OECD Europe
    z[240] = dfd['WLD_POP'].loc[4]

    #       Japan
    z[241] = dfd['WLD_POP'].loc[5]

    #       Australia and New Zealand
    z[242] = dfd['WLD_POP'].loc[6]

    #       South Korea
    z[243] = dfd['WLD_POP'].loc[7]

    #       Russia
    z[244] = dfd['WLD_POP'].loc[8]

    #       Other Europe and Eurasia
    z[245] = dfd['WLD_POP'].loc[9]

    #       China
    z[246] = dfd['WLD_POP'].loc[10]

    #       India
    z[247] = dfd['WLD_POP'].loc[11]

    #       Other Non-OECD Asia
    z[248] = dfd['WLD_POP'].loc[12]

    #       Middle East
    z[249] = dfd['WLD_POP'].loc[13]

    #       Africa
    z[250] = dfd['WLD_POP'].loc[14]

    #       Brazil
    z[251] = dfd['WLD_POP'].loc[15]

    #       Other Non-OECD Americas
    z[252] = dfd['WLD_POP'].loc[16]

    #
    #   Travel Demand

    #     Revenue Passenger Miles (billion miles)

    #       Domestic 1/

    #         United States
    z[4] = dfd['AIROUT'].loc[4]

    #         Canada
    z[5] = dfd['AIROUT'].loc[5]

    #         Mexico and other OECD Americas
    z[6] = dfd['AIROUT'].loc[6]

    #         OECD Europe
    z[7] = dfd['AIROUT'].loc[7]

    #         Japan
    z[8] = dfd['AIROUT'].loc[8]

    #         Australia and New Zealand
    z[9] = dfd['AIROUT'].loc[9]

    #         South Korea
    z[10] = dfd['AIROUT'].loc[10]

    #         Russia
    z[11] = dfd['AIROUT'].loc[11]

    #         Other Europe and Eurasia
    z[12] = dfd['AIROUT'].loc[12]

    #         China
    z[13] = dfd['AIROUT'].loc[13]

    #         India
    z[14] = dfd['AIROUT'].loc[14]

    #         Other Non-OECD Asia
    z[15] = dfd['AIROUT'].loc[15]

    #         Middle East
    z[16] = dfd['AIROUT'].loc[16]

    #         Africa
    z[17] = dfd['AIROUT'].loc[17]

    #         Brazil
    z[18] = dfd['AIROUT'].loc[18]

    #         Other Non-OECD Americas
    z[19] = dfd['AIROUT'].loc[19]

    #           Total World Domestic
    z[36] = dfd['AIROUT'].loc[36]

    #       International 1/

    #         United States
    z[20] = dfd['AIROUT'].loc[20]

    #         Canada
    z[21] = dfd['AIROUT'].loc[21]

    #         Mexico and other OECD Americas
    z[22] = dfd['AIROUT'].loc[22]

    #         OECD Europe
    z[23] = dfd['AIROUT'].loc[23]

    #         Japan
    z[24] = dfd['AIROUT'].loc[24]

    #         Australia and New Zealand
    z[25] = dfd['AIROUT'].loc[25]

    #         South Korea
    z[26] = dfd['AIROUT'].loc[26]

    #         Russia
    z[27] = dfd['AIROUT'].loc[27]

    #         Other Europe and Eurasia
    z[28] = dfd['AIROUT'].loc[28]

    #         China
    z[29] = dfd['AIROUT'].loc[29]

    #         India
    z[30] = dfd['AIROUT'].loc[30]

    #         Other Non-OECD Asia
    z[31] = dfd['AIROUT'].loc[31]

    #         Middle East
    z[32] = dfd['AIROUT'].loc[32]

    #         Africa
    z[33] = dfd['AIROUT'].loc[33]

    #         Brazil
    z[34] = dfd['AIROUT'].loc[34]

    #         Other Non-OECD Americas
    z[35] = dfd['AIROUT'].loc[35]

    #           Total World International
    z[37] = dfd['AIROUT'].loc[37]

    #       Total World
    z[38] = dfd['AIROUT'].loc[38]

    #     Freight Revenue Ton Miles (billion miles) 2/

    #       United States
    z[39] = dfd['AIROUT'].loc[39]

    #       Canada
    z[40] = dfd['AIROUT'].loc[40]

    #       Mexico and other OECD Americas
    z[41] = dfd['AIROUT'].loc[41]

    #       OECD Europe
    z[42] = dfd['AIROUT'].loc[42]

    #       Japan
    z[43] = dfd['AIROUT'].loc[43]

    #       Australia and New Zealand
    z[44] = dfd['AIROUT'].loc[44]

    #       South Korea
    z[45] = dfd['AIROUT'].loc[45]

    #       Russia
    z[46] = dfd['AIROUT'].loc[46]

    #       Other Europe and Eurasia
    z[47] = dfd['AIROUT'].loc[47]

    #       China
    z[48] = dfd['AIROUT'].loc[48]

    #       India
    z[49] = dfd['AIROUT'].loc[49]

    #       Other Non-OECD Asia
    z[50] = dfd['AIROUT'].loc[50]

    #       Middle East
    z[51] = dfd['AIROUT'].loc[51]

    #       Africa
    z[52] = dfd['AIROUT'].loc[52]

    #       Brazil
    z[53] = dfd['AIROUT'].loc[53]

    #       Other Non-OECD Americas
    z[54] = dfd['AIROUT'].loc[54]

    #         Total World Domestic
    z[55] = dfd['AIROUT'].loc[55]

    #         Total World International
    z[56] = dfd['AIROUT'].loc[56]

    #       Total World
    z[57] = dfd['AIROUT'].loc[57]

    #   Seat Miles Demanded (billion miles)

    #     United States
    z[58] = dfd['AIROUT'].loc[58]

    #       Narrow Body Aircraft
    z[59] = dfd['AIROUT'].loc[59]

    #       Wide Body Aircraft
    z[60] = dfd['AIROUT'].loc[60]

    #       Regional Jets
    z[61] = dfd['AIROUT'].loc[61]

    #     Canada
    z[62] = dfd['AIROUT'].loc[62]

    #       Narrow Body Aircraft
    z[63] = dfd['AIROUT'].loc[63]

    #       Wide Body Aircraft
    z[64] = dfd['AIROUT'].loc[64]

    #       Regional Jets
    z[65] = dfd['AIROUT'].loc[65]

    #     Mexico and other OECD Americas
    z[66] = dfd['AIROUT'].loc[66]

    #       Narrow Body Aircraft
    z[67] = dfd['AIROUT'].loc[67]

    #       Wide Body Aircraft
    z[68] = dfd['AIROUT'].loc[68]

    #       Regional Jets
    z[69] = dfd['AIROUT'].loc[69]

    #     OECD Europe
    z[70] = dfd['AIROUT'].loc[70]

    #       Narrow Body Aircraft
    z[71] = dfd['AIROUT'].loc[71]

    #       Wide Body Aircraft
    z[72] = dfd['AIROUT'].loc[72]

    #       Regional Jets
    z[73] = dfd['AIROUT'].loc[73]

    #     Japan
    z[74] = dfd['AIROUT'].loc[74]

    #       Narrow Body Aircraft
    z[75] = dfd['AIROUT'].loc[75]

    #       Wide Body Aircraft
    z[76] = dfd['AIROUT'].loc[76]

    #       Regional Jets
    z[77] = dfd['AIROUT'].loc[77]

    #     Australia and New Zealand
    z[78] = dfd['AIROUT'].loc[78]

    #       Narrow Body Aircraft
    z[79] = dfd['AIROUT'].loc[79]

    #       Wide Body Aircraft
    z[80] = dfd['AIROUT'].loc[80]

    #       Regional Jets
    z[81] = dfd['AIROUT'].loc[81]

    #     South Korea
    z[82] = dfd['AIROUT'].loc[82]

    #       Narrow Body Aircraft
    z[83] = dfd['AIROUT'].loc[83]

    #       Wide Body Aircraft
    z[84] = dfd['AIROUT'].loc[84]

    #       Regional Jets
    z[85] = dfd['AIROUT'].loc[85]

    #     Russia
    z[86] = dfd['AIROUT'].loc[86]

    #       Narrow Body Aircraft
    z[87] = dfd['AIROUT'].loc[87]

    #       Wide Body Aircraft
    z[88] = dfd['AIROUT'].loc[88]

    #       Regional Jets
    z[89] = dfd['AIROUT'].loc[89]

    #     Other Europe and Eurasia
    z[90] = dfd['AIROUT'].loc[90]

    #       Narrow Body Aircraft
    z[91] = dfd['AIROUT'].loc[91]

    #       Wide Body Aircraft
    z[92] = dfd['AIROUT'].loc[92]

    #       Regional Jets
    z[93] = dfd['AIROUT'].loc[93]

    #     China
    z[94] = dfd['AIROUT'].loc[94]

    #       Narrow Body Aircraft
    z[95] = dfd['AIROUT'].loc[95]

    #       Wide Body Aircraft
    z[96] = dfd['AIROUT'].loc[96]

    #       Regional Jets
    z[97] = dfd['AIROUT'].loc[97]

    #     India
    z[98] = dfd['AIROUT'].loc[98]

    #       Narrow Body Aircraft
    z[99] = dfd['AIROUT'].loc[99]

    #       Wide Body Aircraft
    z[100] = dfd['AIROUT'].loc[100]

    #       Regional Jets
    z[101] = dfd['AIROUT'].loc[101]

    #     Other Non-OECD Asia
    z[102] = dfd['AIROUT'].loc[102]

    #       Narrow Body Aircraft
    z[103] = dfd['AIROUT'].loc[103]

    #       Wide Body Aircraft
    z[104] = dfd['AIROUT'].loc[104]

    #       Regional Jets
    z[105] = dfd['AIROUT'].loc[105]

    #     Middle East
    z[106] = dfd['AIROUT'].loc[106]

    #       Narrow Body Aircraft
    z[107] = dfd['AIROUT'].loc[107]

    #       Wide Body Aircraft
    z[108] = dfd['AIROUT'].loc[108]

    #       Regional Jets
    z[109] = dfd['AIROUT'].loc[109]

    #     Africa
    z[110] = dfd['AIROUT'].loc[110]

    #       Narrow Body Aircraft
    z[111] = dfd['AIROUT'].loc[111]

    #       Wide Body Aircraft
    z[112] = dfd['AIROUT'].loc[112]

    #       Regional Jets
    z[113] = dfd['AIROUT'].loc[113]

    #     Brazil
    z[114] = dfd['AIROUT'].loc[114]

    #       Narrow Body Aircraft
    z[115] = dfd['AIROUT'].loc[115]

    #       Wide Body Aircraft
    z[116] = dfd['AIROUT'].loc[116]

    #       Regional Jets
    z[117] = dfd['AIROUT'].loc[117]

    #     Other Non-OECD Americas
    z[118] = dfd['AIROUT'].loc[118]

    #       Narrow Body Aircraft
    z[119] = dfd['AIROUT'].loc[119]

    #       Wide Body Aircraft
    z[120] = dfd['AIROUT'].loc[120]

    #       Regional Jets
    z[121] = dfd['AIROUT'].loc[121]

    #     Total Narrow Body
    z[122] = dfd['AIROUT'].loc[123]

    #     Total Wide Body
    z[123] = dfd['AIROUT'].loc[124]

    #     Total Regional Jet
    z[124] = dfd['AIROUT'].loc[125]

    #     Total World
    z[125] = dfd['AIROUT'].loc[122]

    #
    #   Aircraft Deliveries

    #     United States
    z[126] = dfd['AIROUT'].loc[126]

    #       Narrow Body Aircraft
    z[127] = dfd['AIROUT'].loc[127]

    #       Wide Body Aircraft
    z[128] = dfd['AIROUT'].loc[128]

    #       Regional Jets
    z[129] = dfd['AIROUT'].loc[129]

    #     Canada
    z[130] = dfd['AIROUT'].loc[130]

    #       Narrow Body Aircraft
    z[131] = dfd['AIROUT'].loc[131]

    #       Wide Body Aircraft
    z[132] = dfd['AIROUT'].loc[132]

    #       Regional Jets
    z[133] = dfd['AIROUT'].loc[133]

    #     Mexico and other OECD Americas
    z[134] = dfd['AIROUT'].loc[134]

    #       Narrow Body Aircraft
    z[135] = dfd['AIROUT'].loc[135]

    #       Wide Body Aircraft
    z[136] = dfd['AIROUT'].loc[136]

    #       Regional Jets
    z[137] = dfd['AIROUT'].loc[137]

    #     OECD Europe
    z[138] = dfd['AIROUT'].loc[138]

    #       Narrow Body Aircraft
    z[139] = dfd['AIROUT'].loc[139]

    #       Wide Body Aircraft
    z[140] = dfd['AIROUT'].loc[140]

    #       Regional Jets
    z[141] = dfd['AIROUT'].loc[141]

    #     Japan
    z[142] = dfd['AIROUT'].loc[142]

    #       Narrow Body Aircraft
    z[143] = dfd['AIROUT'].loc[143]

    #       Wide Body Aircraft
    z[144] = dfd['AIROUT'].loc[144]

    #       Regional Jets
    z[145] = dfd['AIROUT'].loc[145]

    #     Australia and New Zealand
    z[146] = dfd['AIROUT'].loc[146]

    #       Narrow Body Aircraft
    z[147] = dfd['AIROUT'].loc[147]

    #       Wide Body Aircraft
    z[148] = dfd['AIROUT'].loc[148]

    #       Regional Jets
    z[149] = dfd['AIROUT'].loc[149]

    #     South Korea
    z[150] = dfd['AIROUT'].loc[150]

    #       Narrow Body Aircraft
    z[151] = dfd['AIROUT'].loc[151]

    #       Wide Body Aircraft
    z[152] = dfd['AIROUT'].loc[152]

    #       Regional Jets
    z[153] = dfd['AIROUT'].loc[153]

    #     Russia
    z[154] = dfd['AIROUT'].loc[154]

    #       Narrow Body Aircraft
    z[155] = dfd['AIROUT'].loc[155]

    #       Wide Body Aircraft
    z[156] = dfd['AIROUT'].loc[156]

    #       Regional Jets
    z[157] = dfd['AIROUT'].loc[157]

    #     Other Europe and Eurasia
    z[158] = dfd['AIROUT'].loc[158]

    #       Narrow Body Aircraft
    z[159] = dfd['AIROUT'].loc[159]

    #       Wide Body Aircraft
    z[160] = dfd['AIROUT'].loc[160]

    #       Regional Jets
    z[161] = dfd['AIROUT'].loc[161]

    #     China
    z[162] = dfd['AIROUT'].loc[162]

    #       Narrow Body Aircraft
    z[163] = dfd['AIROUT'].loc[163]

    #       Wide Body Aircraft
    z[164] = dfd['AIROUT'].loc[164]

    #       Regional Jets
    z[165] = dfd['AIROUT'].loc[165]

    #     India
    z[166] = dfd['AIROUT'].loc[166]

    #       Narrow Body Aircraft
    z[167] = dfd['AIROUT'].loc[167]

    #       Wide Body Aircraft
    z[168] = dfd['AIROUT'].loc[168]

    #       Regional Jets
    z[169] = dfd['AIROUT'].loc[169]

    #     Other Non-OECD Asia
    z[170] = dfd['AIROUT'].loc[170]

    #       Narrow Body Aircraft
    z[171] = dfd['AIROUT'].loc[171]

    #       Wide Body Aircraft
    z[172] = dfd['AIROUT'].loc[172]

    #       Regional Jets
    z[173] = dfd['AIROUT'].loc[173]

    #     Middle East
    z[174] = dfd['AIROUT'].loc[174]

    #       Narrow Body Aircraft
    z[175] = dfd['AIROUT'].loc[175]

    #       Wide Body Aircraft
    z[176] = dfd['AIROUT'].loc[176]

    #       Regional Jets
    z[177] = dfd['AIROUT'].loc[177]

    #     Africa
    z[178] = dfd['AIROUT'].loc[178]

    #       Narrow Body Aircraft
    z[179] = dfd['AIROUT'].loc[179]

    #       Wide Body Aircraft
    z[180] = dfd['AIROUT'].loc[180]

    #       Regional Jets
    z[181] = dfd['AIROUT'].loc[181]

    #     Brazil
    z[182] = dfd['AIROUT'].loc[182]

    #       Narrow Body Aircraft
    z[183] = dfd['AIROUT'].loc[183]

    #       Wide Body Aircraft
    z[184] = dfd['AIROUT'].loc[184]

    #       Regional Jets
    z[185] = dfd['AIROUT'].loc[185]

    #     Other Non-OECD Americas
    z[186] = dfd['AIROUT'].loc[186]

    #       Narrow Body Aircraft
    z[187] = dfd['AIROUT'].loc[187]

    #       Wide Body Aircraft
    z[188] = dfd['AIROUT'].loc[188]

    #       Regional Jets
    z[189] = dfd['AIROUT'].loc[189]

    #     Total Narrow Body
    z[190] = dfd['AIROUT'].loc[191]

    #     Total Wide Body
    z[191] = dfd['AIROUT'].loc[192]

    #     Total Regional Jet
    z[192] = dfd['AIROUT'].loc[193]

    #   Total World
    z[193] = dfd['AIROUT'].loc[190]

    #
    #   Aircraft Efficiency (seat miles per gallon) 3/
    #
    #     New Aircraft
    #
    #       Narrow Body Aircraft
    z[194] = dfd['TRAIREFFN'].loc[1]

    #       Wide Body Aircraft
    z[195] = dfd['TRAIREFFN'].loc[2]

    #       Regional Jets
    z[196] = dfd['TRAIREFFN'].loc[3]

    #         Average Aircraft
    z[197] = dfd['TRAIREFFN'].loc[4]

    #     Aircraft Stock
    #
    #       Narrow Body Aircraft
    z[198] = dfd['TRAIREFFS'].loc[1]

    #       Wide Body Aircraft
    z[199] = dfd['TRAIREFFS'].loc[2]

    #       Regional Jets
    z[200] = dfd['TRAIREFFS'].loc[3]

    #         Average Aircraft
    z[201] = dfd['TRAIREFFS'].loc[4]

    #
    #   Fuel Consumption (trillion Btu)
    #
    #     Commercial Jet Fuel
    z[202] = dfd['AIROUT'].loc[415]

    #       Canada
    z[203] = dfd['AIROUT'].loc[416]

    #       Mexico and other OECD Americas
    z[204] = dfd['AIROUT'].loc[417]

    #       OECD Europe
    z[205] = dfd['AIROUT'].loc[418]

    #       Japan
    z[206] = dfd['AIROUT'].loc[419]

    #       Australia and New Zealand
    z[207] = dfd['AIROUT'].loc[420]

    #       South Korea
    z[208] = dfd['AIROUT'].loc[421]

    #       Russia
    z[209] = dfd['AIROUT'].loc[422]

    #       Other Europe and Eurasia
    z[210] = dfd['AIROUT'].loc[423]

    #       China
    z[211] = dfd['AIROUT'].loc[424]

    #       India
    z[212] = dfd['AIROUT'].loc[425]

    #       Other Non-OECD Asia
    z[213] = dfd['AIROUT'].loc[426]

    #       Middle East
    z[214] = dfd['AIROUT'].loc[427]

    #       Africa
    z[215] = dfd['AIROUT'].loc[428]

    #       Brazil
    z[216] = dfd['AIROUT'].loc[429]

    #       Other Non-OECD Americas
    z[217] = dfd['AIROUT'].loc[430]

    #         Total World
    z[218] = dfd['AIROUT'].loc[415:430,:].sum()

    #     Commercial Aviation Gasoline, U.S.
    z[219] = dfd['TRQAIRT'].loc[2]

    #     Military Jet Fuel, U.S.
    z[220] = dfd['TRQMIL'].loc[1] + dfd['TRQMIL'].loc[2]
    
    return z
