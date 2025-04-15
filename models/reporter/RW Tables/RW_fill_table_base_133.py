# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""

import pandas as pd


def fill_table_base_133(dfd, table_spec, table_id):
    """Fill table  Capacity with Carbon Capture and Carbon Flows
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
    CO2_CCS array has 0 and -1 indexing like below:
        T133(180,IY,IS)=SUM(CO2_CCS(0,1:7,2,IY))/18000+SUM(OGCO245Q(MNLNP1,1:13,IY))/18000
        T133(181,IY,IS)=CO2_CCS(0,-1,2,IY)/18000
    However, the dataframe loaded from the NEMS restart has no such index values.
    0 and -1 was assumed to be 1 in the following code

    """
    z = {}

    FYRPRC = int(table_spec["settings"]["year_index_real_dol_priceyr"])
    # Get inflation index from 1990->2050
    MC_JPGDP = dfd["MC_JPGDP"].loc[1990:]

    # Get inflation index of FYRPRC (2022)
    SCALPR2 = MC_JPGDP.loc[FYRPRC]  # 2022 = column 33

    MAXNFR = dfd["MAXNFR_rwpre"]
    MNUMNR = dfd["MNUMNR_rwpre"]
    MNLNP1 = dfd["MNLNP1_rwpre"]

    #   Capacity with Carbon Capture and Carbon Flows
    #    Region and Source
    #
    
    # C02 Supply by Capture Facility Type- National
    z[1] = (dfd['CO2_SUP_OUT'].loc[1:9,1,:].sum())/1000000
    z[2] = (dfd['CO2_SUP_OUT'].loc[1:9,2,:].sum())/1000000
    z[3] = (dfd['CO2_SUP_OUT'].loc[1:9,3,:].sum())/1000000
    z[4] = (dfd['CO2_SUP_OUT'].loc[1:9,4,:].sum())/1000000
    z[5] = (dfd['CO2_SUP_OUT'].loc[1:9,5,:].sum())/1000000
    z[6] = (dfd['CO2_SUP_OUT'].loc[1:9,6,:].sum())/1000000
    z[7] = (dfd['CO2_SUP_OUT'].loc[1:9,7,:].sum())/1000000
    z[8] = (dfd['CO2_SUP_OUT'].loc[1:9,8,:].sum())/1000000
    z[9] = z[1] + z[2] + z[3] + z[4] + z[5] + z[6] + z[7] + z[8]

    # CO2 Sequestration by Type - National
    z[10] = (dfd['CO2_SEQ_OUT'].loc[1:9,1,:].sum())/1000000
    z[11] = (dfd['CO2_SEQ_OUT'].loc[1:9,2,:].sum())/1000000
    z[12] = z[10] + z[11]

    # Industrial Net Carbon Dioxide Capture 2/
    z[14] = (dfd['CO2_SUP_OUT'].loc[1:9,4:8,:].sum())/1000000
    z[15] = (dfd['EM_INDY'].loc[(slice(1,19),11),:].sum())/1000 * (44/12)
    z[13] = z[15] + z[14]

    # Electric Power Net Carbon Dioxide Capture 3/
    z[17] = (dfd['CO2_SUP_OUT'].loc[1:9,1:3,:].sum())/1000000
    z[18] = (dfd['EM_ELEC'].loc[(slice(1,8),11),:].sum())/1000 * (44/12)
    z[16] = z[18] + z[17]

    # CO2 Supply - All Types
    z[19] = (dfd['CO2_SUP_OUT'].loc[1,1:8,:].sum())/1000000
    z[20] = (dfd['CO2_SUP_OUT'].loc[2,1:8,:].sum())/1000000
    z[21] = (dfd['CO2_SUP_OUT'].loc[3,1:8,:].sum())/1000000
    z[22] = (dfd['CO2_SUP_OUT'].loc[4,1:8,:].sum())/1000000
    z[23] = (dfd['CO2_SUP_OUT'].loc[5,1:8,:].sum())/1000000
    z[24] = (dfd['CO2_SUP_OUT'].loc[6,1:8,:].sum())/1000000
    z[25] = (dfd['CO2_SUP_OUT'].loc[7,1:8,:].sum())/1000000
    z[26] = (dfd['CO2_SUP_OUT'].loc[8,1:8,:].sum())/1000000
    z[27] = (dfd['CO2_SUP_OUT'].loc[9,1:8,:].sum())/1000000

    # CO2 Sequestration - All Types
    z[28] = (dfd['CO2_SEQ_OUT'].loc[1,1:2,:].sum())/1000000
    z[29] = (dfd['CO2_SEQ_OUT'].loc[2,1:2,:].sum())/1000000
    z[30] = (dfd['CO2_SEQ_OUT'].loc[3,1:2,:].sum())/1000000
    z[31] = (dfd['CO2_SEQ_OUT'].loc[4,1:2,:].sum())/1000000
    z[32] = (dfd['CO2_SEQ_OUT'].loc[5,1:2,:].sum())/1000000
    z[33] = (dfd['CO2_SEQ_OUT'].loc[6,1:2,:].sum())/1000000
    z[34] = (dfd['CO2_SEQ_OUT'].loc[7,1:2,:].sum())/1000000
    z[35] = (dfd['CO2_SEQ_OUT'].loc[8,1:2,:].sum())/1000000
    z[36] = (dfd['CO2_SEQ_OUT'].loc[9,1:2,:].sum())/1000000

    # CO2 Supply - Coal Powerplants
    z[37] = (dfd['CO2_SUP_OUT'].loc[1].loc[1])/1000000
    z[38] = (dfd['CO2_SUP_OUT'].loc[2].loc[1])/1000000
    z[39] = (dfd['CO2_SUP_OUT'].loc[3].loc[1])/1000000
    z[40] = (dfd['CO2_SUP_OUT'].loc[4].loc[1])/1000000
    z[41] = (dfd['CO2_SUP_OUT'].loc[5].loc[1])/1000000
    z[42] = (dfd['CO2_SUP_OUT'].loc[6].loc[1])/1000000
    z[43] = (dfd['CO2_SUP_OUT'].loc[7].loc[1])/1000000
    z[44] = (dfd['CO2_SUP_OUT'].loc[8].loc[1])/1000000
    z[45] = (dfd['CO2_SUP_OUT'].loc[9].loc[1])/1000000

    #CO2 Supply - Natural Gas Powerplants
    z[46] = (dfd['CO2_SUP_OUT'].loc[1].loc[2])/1000000
    z[47] = (dfd['CO2_SUP_OUT'].loc[2].loc[2])/1000000
    z[48] = (dfd['CO2_SUP_OUT'].loc[3].loc[2])/1000000
    z[49] = (dfd['CO2_SUP_OUT'].loc[4].loc[2])/1000000
    z[50] = (dfd['CO2_SUP_OUT'].loc[5].loc[2])/1000000
    z[51] = (dfd['CO2_SUP_OUT'].loc[6].loc[2])/1000000
    z[52] = (dfd['CO2_SUP_OUT'].loc[7].loc[2])/1000000
    z[53] = (dfd['CO2_SUP_OUT'].loc[8].loc[2])/1000000
    z[54] = (dfd['CO2_SUP_OUT'].loc[9].loc[2])/1000000

    # CO2 Supply - Bioenergy with Carbon Capture and Storage
    z[55] = (dfd['CO2_SUP_OUT'].loc[1].loc[3])/1000000
    z[56] = (dfd['CO2_SUP_OUT'].loc[2].loc[3])/1000000
    z[57] = (dfd['CO2_SUP_OUT'].loc[3].loc[3])/1000000
    z[58] = (dfd['CO2_SUP_OUT'].loc[4].loc[3])/1000000
    z[59] = (dfd['CO2_SUP_OUT'].loc[5].loc[3])/1000000
    z[60] = (dfd['CO2_SUP_OUT'].loc[6].loc[3])/1000000
    z[61] = (dfd['CO2_SUP_OUT'].loc[7].loc[3])/1000000
    z[62] = (dfd['CO2_SUP_OUT'].loc[8].loc[3])/1000000
    z[63] = (dfd['CO2_SUP_OUT'].loc[9].loc[3])/1000000
    
    # CO2 Supply - Natural Gas Processing
    z[64] = (dfd['CO2_SUP_OUT'].loc[1].loc[4])/1000000
    z[65] = (dfd['CO2_SUP_OUT'].loc[2].loc[4])/1000000
    z[66] = (dfd['CO2_SUP_OUT'].loc[3].loc[4])/1000000
    z[67] = (dfd['CO2_SUP_OUT'].loc[4].loc[4])/1000000
    z[68] = (dfd['CO2_SUP_OUT'].loc[5].loc[4])/1000000
    z[69] = (dfd['CO2_SUP_OUT'].loc[6].loc[4])/1000000
    z[70] = (dfd['CO2_SUP_OUT'].loc[7].loc[4])/1000000
    z[71] = (dfd['CO2_SUP_OUT'].loc[8].loc[4])/1000000
    z[72] = (dfd['CO2_SUP_OUT'].loc[9].loc[4])/1000000

    
    # CO2 Supply - Cement
    z[73] = (dfd['CO2_SUP_OUT'].loc[1].loc[5])/1000000
    z[74] = (dfd['CO2_SUP_OUT'].loc[2].loc[5])/1000000
    z[75] = (dfd['CO2_SUP_OUT'].loc[3].loc[5])/1000000
    z[76] = (dfd['CO2_SUP_OUT'].loc[4].loc[5])/1000000
    z[77] = (dfd['CO2_SUP_OUT'].loc[5].loc[5])/1000000
    z[78] = (dfd['CO2_SUP_OUT'].loc[6].loc[5])/1000000
    z[79] = (dfd['CO2_SUP_OUT'].loc[7].loc[5])/1000000
    z[80] = (dfd['CO2_SUP_OUT'].loc[8].loc[5])/1000000
    z[81] = (dfd['CO2_SUP_OUT'].loc[9].loc[5])/1000000
    
    # CO2 Supply - Ethanol
    z[82] = (dfd['CO2_SUP_OUT'].loc[1].loc[6])/1000000
    z[83] = (dfd['CO2_SUP_OUT'].loc[2].loc[6])/1000000
    z[84] = (dfd['CO2_SUP_OUT'].loc[3].loc[6])/1000000
    z[85] = (dfd['CO2_SUP_OUT'].loc[4].loc[6])/1000000
    z[86] = (dfd['CO2_SUP_OUT'].loc[5].loc[6])/1000000
    z[87] = (dfd['CO2_SUP_OUT'].loc[6].loc[6])/1000000
    z[88] = (dfd['CO2_SUP_OUT'].loc[7].loc[6])/1000000
    z[89] = (dfd['CO2_SUP_OUT'].loc[8].loc[6])/1000000
    z[90] = (dfd['CO2_SUP_OUT'].loc[9].loc[6])/1000000
    
    # CO2 Supply - Hydrogen 4/
    z[91] = (dfd['CO2_SUP_OUT'].loc[1].loc[7])/1000000
    z[92] = (dfd['CO2_SUP_OUT'].loc[2].loc[7])/1000000
    z[93] = (dfd['CO2_SUP_OUT'].loc[3].loc[7])/1000000
    z[94] = (dfd['CO2_SUP_OUT'].loc[4].loc[7])/1000000
    z[95] = (dfd['CO2_SUP_OUT'].loc[5].loc[7])/1000000
    z[96] = (dfd['CO2_SUP_OUT'].loc[6].loc[7])/1000000
    z[97] = (dfd['CO2_SUP_OUT'].loc[7].loc[7])/1000000
    z[98] = (dfd['CO2_SUP_OUT'].loc[8].loc[7])/1000000
    z[99] = (dfd['CO2_SUP_OUT'].loc[9].loc[7])/1000000
    
    # CO2 Supply - Other
    z[100] = (dfd['CO2_SUP_OUT'].loc[1].loc[8])/1000000
    z[101] = (dfd['CO2_SUP_OUT'].loc[2].loc[8])/1000000
    z[102] = (dfd['CO2_SUP_OUT'].loc[3].loc[8])/1000000
    z[103] = (dfd['CO2_SUP_OUT'].loc[4].loc[8])/1000000
    z[104] = (dfd['CO2_SUP_OUT'].loc[5].loc[8])/1000000
    z[105] = (dfd['CO2_SUP_OUT'].loc[6].loc[8])/1000000
    z[106] = (dfd['CO2_SUP_OUT'].loc[7].loc[8])/1000000
    z[107] = (dfd['CO2_SUP_OUT'].loc[8].loc[8])/1000000
    z[108] = (dfd['CO2_SUP_OUT'].loc[9].loc[8])/1000000

    # CO2 Sequestration - CO2 EOR
    z[109] = (dfd['CO2_SEQ_OUT'].loc[1].loc[1])/1000000
    z[110] = (dfd['CO2_SEQ_OUT'].loc[2].loc[1])/1000000
    z[111] = (dfd['CO2_SEQ_OUT'].loc[3].loc[1])/1000000
    z[112] = (dfd['CO2_SEQ_OUT'].loc[4].loc[1])/1000000
    z[113] = (dfd['CO2_SEQ_OUT'].loc[5].loc[1])/1000000
    z[114] = (dfd['CO2_SEQ_OUT'].loc[6].loc[1])/1000000
    z[115] = (dfd['CO2_SEQ_OUT'].loc[7].loc[1])/1000000
    z[116] = (dfd['CO2_SEQ_OUT'].loc[8].loc[1])/1000000
    z[117] = (dfd['CO2_SEQ_OUT'].loc[9].loc[1])/1000000
    
    # CO2 Sequestration - Storage
    z[118] = (dfd['CO2_SEQ_OUT'].loc[1].loc[2])/1000000
    z[119] = (dfd['CO2_SEQ_OUT'].loc[2].loc[2])/1000000
    z[120] = (dfd['CO2_SEQ_OUT'].loc[3].loc[2])/1000000
    z[121] = (dfd['CO2_SEQ_OUT'].loc[4].loc[2])/1000000
    z[122] = (dfd['CO2_SEQ_OUT'].loc[5].loc[2])/1000000
    z[123] = (dfd['CO2_SEQ_OUT'].loc[6].loc[2])/1000000
    z[124] = (dfd['CO2_SEQ_OUT'].loc[7].loc[2])/1000000
    z[125] = (dfd['CO2_SEQ_OUT'].loc[8].loc[2])/1000000
    z[126] = (dfd['CO2_SEQ_OUT'].loc[9].loc[2])/1000000

    #BONUS ROWS#
    
    # CO2 Price - 45Q Eligible
    z[127] = (dfd['CO2_PRC_DIS_45Q'].loc[1]) * SCALPR2
    z[128] = (dfd['CO2_PRC_DIS_45Q'].loc[2]) * SCALPR2
    z[129] = (dfd['CO2_PRC_DIS_45Q'].loc[3]) * SCALPR2
    z[130] = (dfd['CO2_PRC_DIS_45Q'].loc[4]) * SCALPR2
    z[131] = (dfd['CO2_PRC_DIS_45Q'].loc[5]) * SCALPR2
    z[132] = (dfd['CO2_PRC_DIS_45Q'].loc[6]) * SCALPR2
    z[133] = (dfd['CO2_PRC_DIS_45Q'].loc[7]) * SCALPR2
    z[134] = (dfd['CO2_PRC_DIS_45Q'].loc[8]) * SCALPR2
    z[135] = (dfd['CO2_PRC_DIS_45Q'].loc[9]) * SCALPR2
    
    # CO2 Price - No Tax Credit
    z[136] = (dfd['CO2_PRC_DIS_NTC'].loc[1]) * SCALPR2
    z[137] = (dfd['CO2_PRC_DIS_NTC'].loc[2]) * SCALPR2
    z[138] = (dfd['CO2_PRC_DIS_NTC'].loc[3]) * SCALPR2
    z[139] = (dfd['CO2_PRC_DIS_NTC'].loc[4]) * SCALPR2
    z[140] = (dfd['CO2_PRC_DIS_NTC'].loc[5]) * SCALPR2
    z[141] = (dfd['CO2_PRC_DIS_NTC'].loc[6]) * SCALPR2
    z[142] = (dfd['CO2_PRC_DIS_NTC'].loc[7]) * SCALPR2
    z[143] = (dfd['CO2_PRC_DIS_NTC'].loc[8]) * SCALPR2
    z[144] = (dfd['CO2_PRC_DIS_NTC'].loc[9]) * SCALPR2

    # Diff Supply and Sequestration by Census Division
    z[145] = (dfd['CO2_SUP_OUT'].loc[1,1:8,:].sum())/1000000 - (dfd['CO2_SEQ_OUT'].loc[1,1:2,:].sum())/1000000
    z[146] = (dfd['CO2_SUP_OUT'].loc[2,1:8,:].sum())/1000000 - (dfd['CO2_SEQ_OUT'].loc[2,1:2,:].sum())/1000000
    z[147] = (dfd['CO2_SUP_OUT'].loc[3,1:8,:].sum())/1000000 - (dfd['CO2_SEQ_OUT'].loc[3,1:2,:].sum())/1000000
    z[148] = (dfd['CO2_SUP_OUT'].loc[4,1:8,:].sum())/1000000 - (dfd['CO2_SEQ_OUT'].loc[4,1:2,:].sum())/1000000
    z[149] = (dfd['CO2_SUP_OUT'].loc[5,1:8,:].sum())/1000000 - (dfd['CO2_SEQ_OUT'].loc[5,1:2,:].sum())/1000000
    z[150] = (dfd['CO2_SUP_OUT'].loc[6,1:8,:].sum())/1000000 - (dfd['CO2_SEQ_OUT'].loc[6,1:2,:].sum())/1000000
    z[151] = (dfd['CO2_SUP_OUT'].loc[7,1:8,:].sum())/1000000 - (dfd['CO2_SEQ_OUT'].loc[7,1:2,:].sum())/1000000
    z[152] = (dfd['CO2_SUP_OUT'].loc[8,1:8,:].sum())/1000000 - (dfd['CO2_SEQ_OUT'].loc[8,1:2,:].sum())/1000000
    z[153] = (dfd['CO2_SUP_OUT'].loc[9,1:8,:].sum())/1000000 - (dfd['CO2_SEQ_OUT'].loc[9,1:2,:].sum())/1000000


    return z
