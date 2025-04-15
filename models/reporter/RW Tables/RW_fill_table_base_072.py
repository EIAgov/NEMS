# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
SZO fixed Maine on 9/9/2024
"""


def fill_table_base_072(dfd, table_spec, table_id):
    """Fill table Lower 48 Natural Gas Production and Supply Prices by Supply
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

    GASTYPES = int(dfd["GASTYPES_rwpre"])
    OILTYPES = dfd["OILTYPES_rwpre"]
    OGDIST = dfd["OGDIST_rwpre"]
    MNL48T = dfd["MNL48T_rwpre"]

    #   Lower 48    #   Lower 48 Natural Gas Production and Supply Prices by Supply
    #    Region
    #
    #   Dry Production (trillion cubic feet) 1/
    #    Lower 48 Total
    # T72(117,IY,IS)=OGNGPRD(11,IY)*.001
    z[117] = dfd["OGNGPRD"].loc[11] * 0.001
    #

    #    Lower 48 Onshore

    #      East
    # T72(1:10,IY,IS)=OGNGPRD(1:10,IY)*.001
    z[1] = dfd["OGNGPRD"].loc[1] * 0.001

    #      Gulf Coast
    # T72(1:10,IY,IS)=OGNGPRD(1:10,IY)*.001
    z[2] = dfd["OGNGPRD"].loc[2] * 0.001

    #      Midcontinent
    # T72(1:10,IY,IS)=OGNGPRD(1:10,IY)*.001
    z[3] = dfd["OGNGPRD"].loc[3] * 0.001

    #      Southwest
    # T72(1:10,IY,IS)=OGNGPRD(1:10,IY)*.001
    z[4] = dfd["OGNGPRD"].loc[4] * 0.001

    #      Rocky Mountain
    # T72(1:10,IY,IS)=OGNGPRD(1:10,IY)*.001
    z[5] = dfd["OGNGPRD"].loc[5] * 0.001

    #      Northern Great Plains
    # T72(1:10,IY,IS)=OGNGPRD(1:10,IY)*.001
    z[6] = dfd["OGNGPRD"].loc[6] * 0.001

    #      West Coast
    # T72(1:10,IY,IS)=OGNGPRD(1:10,IY)*.001
    z[7] = dfd["OGNGPRD"].loc[7] * 0.001

    # T72(25,IY,IS)=FSUM(T72(1,IY,IS),7)
    z[25] = z[1] + z[2] + z[3] + z[4] + z[5] + z[6] + z[7]

    #    Lower 48 Offshore

    #      Gulf
    # T72(1:10,IY,IS)=OGNGPRD(1:10,IY)*.001
    z[8] = dfd["OGNGPRD"].loc[8] * 0.001

    #        Shallow (State), less than 200 meters
    # T72(21,IY,IS)=OGPRDOFF(1,2,IY)/1000.
    z[21] = dfd["OGPRDOFF"].loc[1].loc[2] / 1000.0

    #        Shallow (Federal), less than 200 meters
    # T72(27,IY,IS)=OGNGPRDGOM(1,IY)-OGPRDOFF(1,2,IY)/1000.
    z[27] = dfd["OGNGPRDGOM"].loc[1] - dfd["OGPRDOFF"].loc[1].loc[2] / 1000.0

    #        Deep (Federal)
    # T72(22,IY,IS)=OGNGPRDGOM(2,IY)
    z[22] = dfd["OGNGPRDGOM"].loc[2]

    #      Pacific
    # T72(1:10,IY,IS)=OGNGPRD(1:10,IY)*.001
    z[9] = dfd["OGNGPRD"].loc[9] * 0.001

    #        State
    # T72(28,IY,IS)=OGPRDOFF(2,2,IY)/1000.
    z[28] = dfd["OGPRDOFF"].loc[2].loc[2] / 1000.0

    #        Federal
    # T72(29,IY,IS)=T72(9,IY,IS)-T72(28,IY,IS)
    z[29] = z[9] - z[28]

    #      Atlantic
    # T72(1:10,IY,IS)=OGNGPRD(1:10,IY)*.001
    z[10] = dfd["OGNGPRD"].loc[10] * 0.001

    #        State
    # T72(30,IY,IS)=OGPRDOFF(3,2,IY)/1000.
    z[30] = dfd["OGPRDOFF"].loc[3].loc[2] / 1000.0

    #        Federal
    # T72(31,IY,IS)=T72(10,IY,IS)-T72(30,IY,IS)
    z[31] = z[10] - z[30]

    # T72(26,IY,IS)=FSUM(T72(8,IY,IS),3)
    z[26] = z[8] + z[9] + z[10]

    # Federal Land Natural Gas Production
    z[119] = dfd["OGNGPRD_FED"].loc[14]
    z[120] = dfd["OGNGPRD_FED"].loc[1]
    z[121] = dfd["OGNGPRD_FED"].loc[2]
    z[122] = dfd["OGNGPRD_FED"].loc[3]
    z[123] = dfd["OGNGPRD_FED"].loc[4]
    z[124] = dfd["OGNGPRD_FED"].loc[5]
    z[125] = dfd["OGNGPRD_FED"].loc[6]
    z[126] = dfd["OGNGPRD_FED"].loc[7]
    z[127] = dfd["OGNGPRD_FED"].loc[8]
    z[128] = dfd["OGNGPRD_FED"].loc[9]
    z[129] = dfd["OGNGPRD_FED"].loc[10]
    z[130] = dfd["OGNGPRD_FED"].loc[11]
    z[131] = dfd["OGNGPRD_FED"].loc[12]
    z[132] = dfd["OGNGPRD_FED"].loc[13]
    z[133] = dfd["OGNGPRD_FED"].loc[14]

    # Non-Federal Land Natural Gas Production
    z[134] = dfd["OGNGPRD_NONFED"].loc[14]
    z[135] = dfd["OGNGPRD_NONFED"].loc[1]
    z[136] = dfd["OGNGPRD_NONFED"].loc[2]
    z[137] = dfd["OGNGPRD_NONFED"].loc[3]
    z[138] = dfd["OGNGPRD_NONFED"].loc[4]
    z[139] = dfd["OGNGPRD_NONFED"].loc[5]
    z[140] = dfd["OGNGPRD_NONFED"].loc[6]
    z[141] = dfd["OGNGPRD_NONFED"].loc[7]
    z[142] = dfd["OGNGPRD_NONFED"].loc[8]
    z[143] = dfd["OGNGPRD_NONFED"].loc[9]
    z[144] = dfd["OGNGPRD_NONFED"].loc[10]
    z[145] = dfd["OGNGPRD_NONFED"].loc[11]
    z[146] = dfd["OGNGPRD_NONFED"].loc[12]
    z[147] = dfd["OGNGPRD_NONFED"].loc[13]
    z[148] = dfd["OGNGPRD_NONFED"].loc[14]

    # Bonus rows, production by OGSM district
    #       DO IR=1,GASTYPES-1
    #          T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,IR,IY)/1000.
    #       ENDDO
    #       T72(32+OGDIST,IY,IS) = FSUM(T72(32,IY,IS),OGDIST)
    #     ENDDO

    #   Production by OGSM District 1/

    #      Alabama, North
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[32] = (
        dfd["OGDNGPRD"]
        .loc[
            1,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Alabama, South
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[33] = (
        dfd["OGDNGPRD"]
        .loc[
            2,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Alaska
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[34] = (
        dfd["OGDNGPRD"]
        .loc[
            3,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Arizona
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[35] = (
        dfd["OGDNGPRD"]
        .loc[
            4,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Arkansas
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[36] = (
        dfd["OGDNGPRD"]
        .loc[
            5,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      California
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[37] = (
        dfd["OGDNGPRD"]
        .loc[
            6,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Colorado
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[38] = (
        dfd["OGDNGPRD"]
        .loc[
            7,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Connecticut
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[39] = (
        dfd["OGDNGPRD"]
        .loc[
            8,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Delaware
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[40] = (
        dfd["OGDNGPRD"]
        .loc[
            9,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Washington, D.C.
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[41] = (
        dfd["OGDNGPRD"]
        .loc[
            10,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Florida
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[42] = (
        dfd["OGDNGPRD"]
        .loc[
            11,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Georgia
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[43] = (
        dfd["OGDNGPRD"]
        .loc[
            12,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Hawaii
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[44] = (
        dfd["OGDNGPRD"]
        .loc[
            13,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Idaho
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[45] = (
        dfd["OGDNGPRD"]
        .loc[
            14,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Illinois
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[46] = (
        dfd["OGDNGPRD"]
        .loc[
            15,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Indiana
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[47] = (
        dfd["OGDNGPRD"]
        .loc[
            16,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Iowa
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[48] = (
        dfd["OGDNGPRD"]
        .loc[
            17,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Kansas
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[49] = (
        dfd["OGDNGPRD"]
        .loc[
            18,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Kentucky
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[50] = (
        dfd["OGDNGPRD"]
        .loc[
            19,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Louisiana, North
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[51] = (
        dfd["OGDNGPRD"]
        .loc[
            20,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Louisiana, South
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[52] = (
        dfd["OGDNGPRD"]
        .loc[
            21,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Maine
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[53] = (
        dfd["OGDNGPRD"]
        .loc[
            22,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )
    #      Maryland
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[54] = (
        dfd["OGDNGPRD"]
        .loc[
            23,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Massachusetts
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[55] = (
        dfd["OGDNGPRD"]
        .loc[
            24,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Michigan
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[56] = (
        dfd["OGDNGPRD"]
        .loc[
            25,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Minnesota
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[57] = (
        dfd["OGDNGPRD"]
        .loc[
            26,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Mississippi, North
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[58] = (
        dfd["OGDNGPRD"]
        .loc[
            27,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Mississippi, South
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[59] = (
        dfd["OGDNGPRD"]
        .loc[
            28,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Missouri
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[60] = (
        dfd["OGDNGPRD"]
        .loc[
            29,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Montana
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[61] = (
        dfd["OGDNGPRD"]
        .loc[
            30,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Nebraska
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[62] = (
        dfd["OGDNGPRD"]
        .loc[
            31,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Nevada
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[63] = (
        dfd["OGDNGPRD"]
        .loc[
            32,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      New Hampshire
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[64] = (
        dfd["OGDNGPRD"]
        .loc[
            33,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      New Jersey
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[65] = (
        dfd["OGDNGPRD"]
        .loc[
            34,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      New Mexico, East
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[66] = (
        dfd["OGDNGPRD"]
        .loc[
            35,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      New Mexico, West
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[67] = (
        dfd["OGDNGPRD"]
        .loc[
            36,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      New York
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[68] = (
        dfd["OGDNGPRD"]
        .loc[
            37,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      North Carolina
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[69] = (
        dfd["OGDNGPRD"]
        .loc[
            38,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      North Dakota
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[70] = (
        dfd["OGDNGPRD"]
        .loc[
            39,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Ohio
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[71] = (
        dfd["OGDNGPRD"]
        .loc[
            40,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Oklahoma
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[72] = (
        dfd["OGDNGPRD"]
        .loc[
            41,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Oregon
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[73] = (
        dfd["OGDNGPRD"]
        .loc[
            42,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Pennsylvania
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[74] = (
        dfd["OGDNGPRD"]
        .loc[
            43,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Rhode Island
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[75] = (
        dfd["OGDNGPRD"]
        .loc[
            44,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      South Carolina
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[76] = (
        dfd["OGDNGPRD"]
        .loc[
            45,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      South Dakota
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[77] = (
        dfd["OGDNGPRD"]
        .loc[
            46,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Tennessee
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[78] = (
        dfd["OGDNGPRD"]
        .loc[
            47,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Texas RRC 1
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[79] = (
        dfd["OGDNGPRD"]
        .loc[
            48,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Texas RRC 2
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[80] = (
        dfd["OGDNGPRD"]
        .loc[
            49,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Texas RRC 3
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[81] = (
        dfd["OGDNGPRD"]
        .loc[
            50,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Texas RRC 4
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[82] = (
        dfd["OGDNGPRD"]
        .loc[
            51,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Texas RRC 5
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[83] = (
        dfd["OGDNGPRD"]
        .loc[
            52,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Texas RRC 6
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[84] = (
        dfd["OGDNGPRD"]
        .loc[
            53,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Texas RRC 7B
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[85] = (
        dfd["OGDNGPRD"]
        .loc[
            54,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Texas RRC 7C
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[86] = (
        dfd["OGDNGPRD"]
        .loc[
            55,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Texas RRC 8
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[87] = (
        dfd["OGDNGPRD"]
        .loc[
            56,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Texas RRC 8A
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[88] = (
        dfd["OGDNGPRD"]
        .loc[
            57,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Texas RRC 9
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[89] = (
        dfd["OGDNGPRD"]
        .loc[
            58,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Texas RRC 10
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[90] = (
        dfd["OGDNGPRD"]
        .loc[
            59,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Utah
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[91] = (
        dfd["OGDNGPRD"]
        .loc[
            60,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Vermont
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[92] = (
        dfd["OGDNGPRD"]
        .loc[
            61,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Virginia
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[93] = (
        dfd["OGDNGPRD"]
        .loc[
            62,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Washington
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[94] = (
        dfd["OGDNGPRD"]
        .loc[
            63,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      West Virginia
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[95] = (
        dfd["OGDNGPRD"]
        .loc[
            64,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Wisconsin
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[96] = (
        dfd["OGDNGPRD"]
        .loc[
            65,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Wyoming
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[97] = (
        dfd["OGDNGPRD"]
        .loc[
            66,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      North Atlantic State Offshore
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[98] = (
        dfd["OGDNGPRD"]
        .loc[
            67,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Mid Atlantic State Offshore
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[99] = (
        dfd["OGDNGPRD"]
        .loc[
            68,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      South Atlantic State Offshore
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[100] = (
        dfd["OGDNGPRD"]
        .loc[
            69,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Alabama State Offshore
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[101] = (
        dfd["OGDNGPRD"]
        .loc[
            70,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Louisiana State Offshore
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[102] = (
        dfd["OGDNGPRD"]
        .loc[
            71,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Texas State Offshore
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[103] = (
        dfd["OGDNGPRD"]
        .loc[
            72,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      California State Offshore
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[104] = (
        dfd["OGDNGPRD"]
        .loc[
            73,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Northern Pacific State Offshore
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[105] = (
        dfd["OGDNGPRD"]
        .loc[
            74,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Alaska State Offshore
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[106] = (
        dfd["OGDNGPRD"]
        .loc[
            75,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      North Atlantic Federal Offshore
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[107] = (
        dfd["OGDNGPRD"]
        .loc[
            76,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Mid Atlantic Federal Offshore
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[108] = (
        dfd["OGDNGPRD"]
        .loc[
            77,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      South Atlantic Federal Offshore
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[109] = (
        dfd["OGDNGPRD"]
        .loc[
            78,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Eastern GOM Federal Offshore
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[110] = (
        dfd["OGDNGPRD"]
        .loc[
            79,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Central GOM Federal Offshore
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[111] = (
        dfd["OGDNGPRD"]
        .loc[
            80,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Western GOM Federal Offshore
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[112] = (
        dfd["OGDNGPRD"]
        .loc[
            81,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      California Federal Offshore
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[113] = (
        dfd["OGDNGPRD"]
        .loc[
            82,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Northern Pacific Federal Offshore
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[114] = (
        dfd["OGDNGPRD"]
        .loc[
            83,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #      Alaska Federal Offshore
    # T72(32:32+OGDIST-1,IY,IS) = T72(32:32+OGDIST-1,IY,IS) + OGDNGPRD(1:OGDIST,sum(IR 1:5),IY)/1000.
    z[115] = (
        dfd["OGDNGPRD"]
        .loc[
            84,
            1 : (GASTYPES - 1),
            :,
        ]
        .sum()
        / 1000.0
    )

    #         Total
    # T72(32+OGDIST,IY,IS) = FSUM(T72(32,IY,IS),OGDIST)
    z[116] = (
        z[32]
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
        + z[114]
        + z[115]
    )

    #   Supply Prices
    #     (#### dollars per million Btu)

    #       Henry Hub Spot Price
    # T72(23,IY,IS)=OGHHPRNG(IY)
    z[23] = dfd["OGHHPRNG"] * SCALPR2

    #       Lower 48 Average Price
    # T72(24,IY,IS)=OGNGWHP(11,IY)/CFNGC(IY)
    z[24] = dfd["OGNGWHP"].loc[11] / dfd["CFNGC"] * SCALPR2

    #     (#### dollars per thousand cubic feet)

    #       Lower 48 Average Price
    # T72(118,IY,IS)=OGNGWHP(11,IY)
    z[118] = dfd["OGNGWHP"].loc[11] * SCALPR2

    #       Lower 48 Onshore Price

    #         East
    # T72(11:20,IY,IS)=OGNGWHP(1:10,IY)
    z[11] = dfd["OGNGWHP"].loc[1] * SCALPR2

    #         Gulf Coast
    # T72(11:20,IY,IS)=OGNGWHP(1:10,IY)
    z[12] = dfd["OGNGWHP"].loc[2] * SCALPR2

    #         Midcontinent
    # T72(11:20,IY,IS)=OGNGWHP(1:10,IY)
    z[13] = dfd["OGNGWHP"].loc[3] * SCALPR2

    #         Southwest
    # T72(11:20,IY,IS)=OGNGWHP(1:10,IY)
    z[14] = dfd["OGNGWHP"].loc[4] * SCALPR2

    #         Rocky Mountain
    # T72(11:20,IY,IS)=OGNGWHP(1:10,IY)
    z[15] = dfd["OGNGWHP"].loc[5] * SCALPR2

    #         Northern Great Plains
    # T72(11:20,IY,IS)=OGNGWHP(1:10,IY)
    z[16] = dfd["OGNGWHP"].loc[6] * SCALPR2

    #         West Coast
    # T72(11:20,IY,IS)=OGNGWHP(1:10,IY)
    z[17] = dfd["OGNGWHP"].loc[7] * SCALPR2

    #       Lower 48 Offshore Price

    #         Gulf
    # T72(11:20,IY,IS)=OGNGWHP(1:10,IY)
    z[18] = dfd["OGNGWHP"].loc[8] * SCALPR2

    #         Pacific
    # T72(11:20,IY,IS)=OGNGWHP(1:10,IY)
    z[19] = dfd["OGNGWHP"].loc[9] * SCALPR2

    #         Atlantic
    # T72(11:20,IY,IS) = OGNGWHP(1:10,IY)
    z[20] = dfd["OGNGWHP"].loc[10] * SCALPR2

    return z
