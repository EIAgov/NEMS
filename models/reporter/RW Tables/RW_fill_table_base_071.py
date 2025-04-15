# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_071(dfd, table_spec, table_id):
    """Fill table Lower 48 Crude Oil Production and Wellhead Prices by Supply
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

    FYRPRC = int(table_spec["settings"]["year_index_real_dol_priceyr"])
    # Get inflation index from 1990->2050
    MC_JPGDP = dfd["MC_JPGDP"].loc[1990:]

    # Get inflation index of FYRPRC (2022)
    SCALPR2 = MC_JPGDP.loc[FYRPRC]  # 2022 = column 33

    RDAYS = dfd["RDAYS_rwpre"]
    OILTYPES = dfd["OILTYPES_rwpre"]
    OGDIST = dfd["OGDIST_rwpre"]
    MNL48T = dfd["MNL48T_rwpre"]

    #   Lower 48 Crude Oil Production and Wellhead Prices by Supply
    #    Region
    #
    #   Production 1/
    #   (million barrels per day)
    #    Lower 48 Total
    #    Lower 48 Onshore
    #      East
    # T71(1:10,IY,IS)=OGCOPRD(1:MNL48T-1,IY)
    z[1] = dfd["OGCOPRD"].loc[1]

    #      Gulf Coast
    # T71(1:10,IY,IS)=OGCOPRD(1:MNL48T-1,IY)
    z[2] = dfd["OGCOPRD"].loc[2]

    #      Midcontinent
    # T71(1:10,IY,IS)=OGCOPRD(1:MNL48T-1,IY)
    z[3] = dfd["OGCOPRD"].loc[3]

    #      Southwest
    # T71(1:10,IY,IS)=OGCOPRD(1:MNL48T-1,IY)
    z[4] = dfd["OGCOPRD"].loc[4]

    #      Rocky Mountain
    # T71(1:10,IY,IS)=OGCOPRD(1:MNL48T-1,IY)
    z[5] = dfd["OGCOPRD"].loc[5]

    #      Northern Great Plains
    # T71(1:10,IY,IS)=OGCOPRD(1:MNL48T-1,IY)
    z[6] = dfd["OGCOPRD"].loc[6]

    #      West Coast
    # T71(1:10,IY,IS)=OGCOPRD(1:MNL48T-1,IY)
    z[7] = dfd["OGCOPRD"].loc[7]

    # T71(23,IY,IS)=FSUM(T71(1,IY,IS),7)
    z[23] = z[1] + z[2] + z[3] + z[4] + z[5] + z[6] + z[7]

    #    Lower 48 Offshore

    #      Gulf
    # T71(1:10,IY,IS)=OGCOPRD(1:MNL48T-1,IY)
    z[8] = dfd["OGCOPRD"].loc[8]

    #        Shallow (State), less than 200 meters
    # T71(21,IY,IS)=OGPRDOFF(1,1,IY)/RDAYS
    z[21] = dfd["OGPRDOFF"].loc[1].loc[1] / (RDAYS)

    #        Shallow (Federal), less than 200 meters
    # T71(25,IY,IS)=OGCOPRDGOM(1,IY)-OGPRDOFF(1,1,IY)/RDAYS
    z[25] = dfd["OGCOPRDGOM"].loc[1] - dfd["OGPRDOFF"].loc[1].loc[1] / (RDAYS)

    #        Deep (Federal)
    # T71(22,IY,IS)=OGCOPRDGOM(2,IY)
    z[22] = dfd["OGCOPRDGOM"].loc[2]

    #      Pacific
    # T71(1:10,IY,IS)=OGCOPRD(1:MNL48T-1,IY)
    z[9] = dfd["OGCOPRD"].loc[9]

    #        State
    # T71(26,IY,IS)=OGPRDOFF(2,1,IY)/RDAYS
    z[26] = dfd["OGPRDOFF"].loc[2].loc[1] / (RDAYS)

    #        Federal
    # T71(27,IY,IS)=T71(9,IY,IS)-T71(26,IY,IS)
    z[27] = z[9] - z[26]

    #      Atlantic
    # T71(1:10,IY,IS)=OGCOPRD(1:MNL48T-1,IY)
    z[10] = dfd["OGCOPRD"].loc[10]

    #        State
    # T71(28,IY,IS)=OGPRDOFF(3,1,IY)/RDAYS
    z[28] = dfd["OGPRDOFF"].loc[3].loc[1] / (RDAYS)

    #        Federal
    # T71(29,IY,IS)=T71(10,IY,IS)-T71(28,IY,IS)
    z[29] = z[10] - z[28]

    ##T71(115,IY,IS)=FSUM(T71(1,IY,IS),10)
    z[115] = z[1] + z[2] + z[3] + z[4] + z[5] + z[6] + z[7] + z[8] + z[9] + z[10]
    # T71(24,IY,IS)=FSUM(T71(8,IY,IS),3)
    z[24] = z[8] + z[9] + z[10]

    # Federal Land Crude Production
    z[117] = dfd["OGCOPRD_FED"].loc[14]
    z[118] = dfd["OGCOPRD_FED"].loc[1]
    z[119] = dfd["OGCOPRD_FED"].loc[2]
    z[120] = dfd["OGCOPRD_FED"].loc[3]
    z[121] = dfd["OGCOPRD_FED"].loc[4]
    z[122] = dfd["OGCOPRD_FED"].loc[5]
    z[123] = dfd["OGCOPRD_FED"].loc[6]
    z[124] = dfd["OGCOPRD_FED"].loc[7]
    z[125] = dfd["OGCOPRD_FED"].loc[8]
    z[126] = dfd["OGCOPRD_FED"].loc[9]
    z[127] = dfd["OGCOPRD_FED"].loc[10]
    z[128] = dfd["OGCOPRD_FED"].loc[11]
    z[129] = dfd["OGCOPRD_FED"].loc[12]
    z[130] = dfd["OGCOPRD_FED"].loc[13]
    z[131] = dfd["OGCOPRD_FED"].loc[14]

    # Non-Federal Land Crude Production
    z[132] = dfd["OGCOPRD_NONFED"].loc[14]
    z[133] = dfd["OGCOPRD_NONFED"].loc[1]
    z[134] = dfd["OGCOPRD_NONFED"].loc[2]
    z[135] = dfd["OGCOPRD_NONFED"].loc[3]
    z[136] = dfd["OGCOPRD_NONFED"].loc[4]
    z[137] = dfd["OGCOPRD_NONFED"].loc[5]
    z[138] = dfd["OGCOPRD_NONFED"].loc[6]
    z[139] = dfd["OGCOPRD_NONFED"].loc[7]
    z[140] = dfd["OGCOPRD_NONFED"].loc[8]
    z[141] = dfd["OGCOPRD_NONFED"].loc[9]
    z[142] = dfd["OGCOPRD_NONFED"].loc[10]
    z[143] = dfd["OGCOPRD_NONFED"].loc[11]
    z[144] = dfd["OGCOPRD_NONFED"].loc[12]
    z[145] = dfd["OGCOPRD_NONFED"].loc[13]
    z[146] = dfd["OGCOPRD_NONFED"].loc[14]

    #   Production by OGSM District 1/

    #      Alabama, North
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[30] = dfd["OGOILPRD"].loc[1].loc[OILTYPES]

    #      Alabama, South
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[31] = dfd["OGOILPRD"].loc[2].loc[OILTYPES]

    #      Alaska
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[32] = dfd["OGOILPRD"].loc[3].loc[OILTYPES]

    #      Arizona
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[33] = dfd["OGOILPRD"].loc[4].loc[OILTYPES]

    #      Arkansas
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[34] = dfd["OGOILPRD"].loc[5].loc[OILTYPES]

    #      California
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[35] = dfd["OGOILPRD"].loc[6].loc[OILTYPES]

    #      Colorado
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[36] = dfd["OGOILPRD"].loc[7].loc[OILTYPES]

    #      Connecticut
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[37] = dfd["OGOILPRD"].loc[8].loc[OILTYPES]

    #      Delaware
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[38] = dfd["OGOILPRD"].loc[9].loc[OILTYPES]

    #      Washington, D.C.
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[39] = dfd["OGOILPRD"].loc[10].loc[OILTYPES]

    #      Florida
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[40] = dfd["OGOILPRD"].loc[11].loc[OILTYPES]

    #      Georgia
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[41] = dfd["OGOILPRD"].loc[12].loc[OILTYPES]

    #      Hawaii
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[42] = dfd["OGOILPRD"].loc[13].loc[OILTYPES]

    #      Idaho
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[43] = dfd["OGOILPRD"].loc[14].loc[OILTYPES]

    #      Illinois
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[44] = dfd["OGOILPRD"].loc[15].loc[OILTYPES]

    #      Indiana
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[45] = dfd["OGOILPRD"].loc[16].loc[OILTYPES]

    #      Iowa
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[46] = dfd["OGOILPRD"].loc[17].loc[OILTYPES]

    #      Kansas
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[47] = dfd["OGOILPRD"].loc[18].loc[OILTYPES]

    #      Kentucky
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[48] = dfd["OGOILPRD"].loc[19].loc[OILTYPES]

    #      Louisiana, North
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[49] = dfd["OGOILPRD"].loc[20].loc[OILTYPES]

    #      Louisiana, South
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[50] = dfd["OGOILPRD"].loc[21].loc[OILTYPES]

    #      Maine
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[51] = dfd["OGOILPRD"].loc[22].loc[OILTYPES]

    #      Maryland
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[52] = dfd["OGOILPRD"].loc[23].loc[OILTYPES]

    #      Massachusetts
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[53] = dfd["OGOILPRD"].loc[24].loc[OILTYPES]

    #      Michigan
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[54] = dfd["OGOILPRD"].loc[25].loc[OILTYPES]

    #      Minnesota
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[55] = dfd["OGOILPRD"].loc[26].loc[OILTYPES]

    #      Mississippi, North
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[56] = dfd["OGOILPRD"].loc[27].loc[OILTYPES]

    #      Mississippi, South
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[57] = dfd["OGOILPRD"].loc[28].loc[OILTYPES]

    #      Missouri
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[58] = dfd["OGOILPRD"].loc[29].loc[OILTYPES]

    #      Montana
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[59] = dfd["OGOILPRD"].loc[30].loc[OILTYPES]

    #      Nebraska
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[60] = dfd["OGOILPRD"].loc[31].loc[OILTYPES]

    #      Nevada
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[61] = dfd["OGOILPRD"].loc[32].loc[OILTYPES]

    #      New Hampshire
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[62] = dfd["OGOILPRD"].loc[33].loc[OILTYPES]

    #      New Jersey
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[63] = dfd["OGOILPRD"].loc[34].loc[OILTYPES]

    #      New Mexico, East
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[64] = dfd["OGOILPRD"].loc[35].loc[OILTYPES]

    #      New Mexico, West
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[65] = dfd["OGOILPRD"].loc[36].loc[OILTYPES]

    #      New York
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[66] = dfd["OGOILPRD"].loc[37].loc[OILTYPES]

    #      North Carolina
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[67] = dfd["OGOILPRD"].loc[38].loc[OILTYPES]

    #      North Dakota
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[68] = dfd["OGOILPRD"].loc[39].loc[OILTYPES]

    #      Ohio
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[69] = dfd["OGOILPRD"].loc[40].loc[OILTYPES]

    #      Oklahoma
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[70] = dfd["OGOILPRD"].loc[41].loc[OILTYPES]

    #      Oregon
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[71] = dfd["OGOILPRD"].loc[42].loc[OILTYPES]

    #      Pennsylvania
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[72] = dfd["OGOILPRD"].loc[43].loc[OILTYPES]

    #      Rhode Island
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[73] = dfd["OGOILPRD"].loc[44].loc[OILTYPES]

    #      South Carolina
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[74] = dfd["OGOILPRD"].loc[45].loc[OILTYPES]

    #      South Dakota
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[75] = dfd["OGOILPRD"].loc[46].loc[OILTYPES]

    #      Tennessee
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[76] = dfd["OGOILPRD"].loc[47].loc[OILTYPES]

    #      Texas RRC 1
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[77] = dfd["OGOILPRD"].loc[48].loc[OILTYPES]

    #      Texas RRC 2
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[78] = dfd["OGOILPRD"].loc[49].loc[OILTYPES]

    #      Texas RRC 3
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[79] = dfd["OGOILPRD"].loc[50].loc[OILTYPES]

    #      Texas RRC 4
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[80] = dfd["OGOILPRD"].loc[51].loc[OILTYPES]

    #      Texas RRC 5
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[81] = dfd["OGOILPRD"].loc[52].loc[OILTYPES]

    #      Texas RRC 6
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[82] = dfd["OGOILPRD"].loc[53].loc[OILTYPES]

    #      Texas RRC 7B
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[83] = dfd["OGOILPRD"].loc[54].loc[OILTYPES]

    #      Texas RRC 7C
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[84] = dfd["OGOILPRD"].loc[55].loc[OILTYPES]

    #      Texas RRC 8
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[85] = dfd["OGOILPRD"].loc[56].loc[OILTYPES]

    #      Texas RRC 8A
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[86] = dfd["OGOILPRD"].loc[57].loc[OILTYPES]

    #      Texas RRC 9
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[87] = dfd["OGOILPRD"].loc[58].loc[OILTYPES]

    #      Texas RRC 10
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[88] = dfd["OGOILPRD"].loc[59].loc[OILTYPES]

    #      Utah
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[89] = dfd["OGOILPRD"].loc[60].loc[OILTYPES]

    #      Vermont
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[90] = dfd["OGOILPRD"].loc[61].loc[OILTYPES]

    #      Virginia
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[91] = dfd["OGOILPRD"].loc[62].loc[OILTYPES]

    #      Washington
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[92] = dfd["OGOILPRD"].loc[63].loc[OILTYPES]

    #      West Virginia
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[93] = dfd["OGOILPRD"].loc[64].loc[OILTYPES]

    #      Wisconsin
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[94] = dfd["OGOILPRD"].loc[65].loc[OILTYPES]

    #      Wyoming
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[95] = dfd["OGOILPRD"].loc[66].loc[OILTYPES]

    #      North Atlantic State Offshore
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[96] = dfd["OGOILPRD"].loc[67].loc[OILTYPES]

    #      Mid Atlantic State Offshore
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[97] = dfd["OGOILPRD"].loc[68].loc[OILTYPES]

    #      South Atlantic State Offshore
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[98] = dfd["OGOILPRD"].loc[69].loc[OILTYPES]

    #      Alabama State Offshore
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[99] = dfd["OGOILPRD"].loc[70].loc[OILTYPES]

    #      Louisiana State Offshore
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[100] = dfd["OGOILPRD"].loc[71].loc[OILTYPES]

    #      Texas State Offshore
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[101] = dfd["OGOILPRD"].loc[72].loc[OILTYPES]

    #      California State Offshore
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[102] = dfd["OGOILPRD"].loc[73].loc[OILTYPES]

    #      Northern Pacific State Offshore
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[103] = dfd["OGOILPRD"].loc[74].loc[OILTYPES]

    #      Alaska State Offshore
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[104] = dfd["OGOILPRD"].loc[75].loc[OILTYPES]

    #      North Atlantic Federal Offshore
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[105] = dfd["OGOILPRD"].loc[76].loc[OILTYPES]

    #      Mid Atlantic Federal Offshore
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[106] = dfd["OGOILPRD"].loc[77].loc[OILTYPES]

    #      South Atlantic Federal Offshore
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[107] = dfd["OGOILPRD"].loc[78].loc[OILTYPES]

    #      Eastern GOM Federal Offshore
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[108] = dfd["OGOILPRD"].loc[79].loc[OILTYPES]

    #      Central GOM Federal Offshore
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[109] = dfd["OGOILPRD"].loc[80].loc[OILTYPES]

    #      Western GOM Federal Offshore
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[110] = dfd["OGOILPRD"].loc[81].loc[OILTYPES]

    #      California Federal Offshore
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[111] = dfd["OGOILPRD"].loc[82].loc[OILTYPES]

    #      Northern Pacific Federal Offshore
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[112] = dfd["OGOILPRD"].loc[83].loc[OILTYPES]

    #      Alaska Federal Offshore
    # T71(30:30+OGDIST-1,IY,IS) = OGOILPRD(1:OGDIST,OILTYPES,IY)
    z[113] = dfd["OGOILPRD"].loc[84].loc[OILTYPES]

    #         Total
    # T71(30+OGDIST,IY,IS) = FSUM(T71(30,IY,IS),OGDIST)
    z[114] = (
        z[30]
        + z[31]
        + z[32]
        + z[33]
        + z[34]
        + z[35]
        + z[36]
        + z[37]
        + z[38]
        + z[39]
        + z[40]
        + z[41]
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
        + z[60]
        + z[61]
        + z[62]
        + z[63]
        + z[64]
        + z[65]
        + z[66]
        + z[67]
        + z[68]
        + z[69]
        + z[70]
        + z[71]
        + z[72]
        + z[73]
        + z[74]
        + z[75]
        + z[76]
        + z[77]
        + z[78]
        + z[79]
        + z[80]
        + z[81]
        + z[82]
        + z[83]
        + z[84]
        + z[85]
        + z[86]
        + z[87]
        + z[88]
        + z[89]
        + z[90]
        + z[91]
        + z[92]
        + z[93]
        + z[94]
        + z[95]
        + z[96]
        + z[97]
        + z[98]
        + z[99]
        + z[100]
        + z[101]
        + z[102]
        + z[103]
        + z[104]
        + z[105]
        + z[106]
        + z[107]
        + z[108]
        + z[109]
        + z[110]
        + z[111]
        + z[112]
        + z[113]
    )

    #
    #   Wellhead Prices (#### dollars per barrel)

    #    Lower 48 Average
    # T71(116,IY,IS)=OGCOWHP(MNL48T,IY)
    z[116] = dfd["OGCOWHP"].loc[MNL48T] * SCALPR2

    #    Lower 48 Onshore

    #      East
    # T71(11:20,IY,IS)=OGCOWHP(1:MNL48T-1,IY)
    z[11] = dfd["OGCOWHP"].loc[1] * SCALPR2

    #      Gulf Coast
    # T71(11:20,IY,IS)=OGCOWHP(1:MNL48T-1,IY)
    z[12] = dfd["OGCOWHP"].loc[2] * SCALPR2

    #      Midcontinent
    # T71(11:20,IY,IS)=OGCOWHP(1:MNL48T-1,IY)
    z[13] = dfd["OGCOWHP"].loc[3] * SCALPR2

    #      Southwest
    # T71(11:20,IY,IS)=OGCOWHP(1:MNL48T-1,IY)
    z[14] = dfd["OGCOWHP"].loc[4] * SCALPR2

    #      Rocky Mountain
    # T71(11:20,IY,IS)=OGCOWHP(1:MNL48T-1,IY)
    z[15] = dfd["OGCOWHP"].loc[5] * SCALPR2

    #      Northern Great Plains
    # T71(11:20,IY,IS)=OGCOWHP(1:MNL48T-1,IY)
    z[16] = dfd["OGCOWHP"].loc[6] * SCALPR2

    #      West Coast
    # T71(11:20,IY,IS)=OGCOWHP(1:MNL48T-1,IY)
    z[17] = dfd["OGCOWHP"].loc[7] * SCALPR2

    #    Lower 48 Offshore

    #      Gulf
    # T71(11:20,IY,IS)=OGCOWHP(1:MNL48T-1,IY)
    z[18] = dfd["OGCOWHP"].loc[8] * SCALPR2

    #      Pacific
    # T71(11:20,IY,IS)=OGCOWHP(1:MNL48T-1,IY)
    z[19] = dfd["OGCOWHP"].loc[9] * SCALPR2

    #      Atlantic
    # T71(11:20,IY,IS)=OGCOWHP(1:MNL48T-1,IY)
    z[20] = dfd["OGCOWHP"].loc[10] * SCALPR2

    return z
