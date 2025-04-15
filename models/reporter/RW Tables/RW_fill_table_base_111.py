# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
SZO fixed AGSCALPR on 8/13/2024
"""


def fill_table_base_111(dfd, table_spec, table_id):
    """Fill table  Biomass Prices by Coal Region, Sector, and Type

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
    # Ag uses a different deflator
    # AGSCALPR=(MC_DETAIL(100,FYRPRC-1989)/(MC_DETAIL(100,1)*0.97)) # $1990*.97=$1987

    AGSCALPR = dfd["MC_DETAIL"].loc[100][FYRPRC] / (
        dfd["MC_DETAIL"].loc[100][table_spec["first_year"]] * 0.97
    )

    #   Biomass Prices by Coal Region, Sector, and Type
    #   Type and Region
    #   Average Price for All Types
    #     Electric Power
    #       New England
    # T111(1:16,IY,IS)=PBMPWCL(0,1:16,IY)
    z[1] = dfd["PBMPWCL"].loc[1].loc[2] * SCALPR2
    #       Middle Atlantic
    # T111(1:16,IY,IS)=PBMPWCL(0,1:16,IY)
    z[2] = dfd["PBMPWCL"].loc[1].loc[3] * SCALPR2

    #       South Atlantic 1
    # T111(1:16,IY,IS)=PBMPWCL(0,1:16,IY)
    z[3] = dfd["PBMPWCL"].loc[1].loc[4] * SCALPR2

    #       Virginia, Carolinas (South Atlantic 2)
    # T111(1:16,IY,IS)=PBMPWCL(0,1:16,IY)
    z[4] = dfd["PBMPWCL"].loc[1].loc[5] * SCALPR2

    #       Georgia and Florida (South Atlantic 3)
    # T111(1:16,IY,IS)=PBMPWCL(0,1:16,IY)
    z[5] = dfd["PBMPWCL"].loc[1].loc[6] * SCALPR2

    #       Ohio (East North Central 1)
    # T111(1:16,IY,IS)=PBMPWCL(0,1:16,IY)
    z[6] = dfd["PBMPWCL"].loc[1].loc[7] * SCALPR2

    #       East North Central 2
    # T111(1:16,IY,IS)=PBMPWCL(0,1:16,IY)
    z[7] = dfd["PBMPWCL"].loc[1].loc[8] * SCALPR2

    #       Kentucky and Tennessee (East South Cent 1)
    # T111(1:16,IY,IS)=PBMPWCL(0,1:16,IY)
    z[8] = dfd["PBMPWCL"].loc[1].loc[9] * SCALPR2

    #       Alabama and Mississippi (East South Cent 2)
    # T111(1:16,IY,IS)=PBMPWCL(0,1:16,IY)
    z[9] = dfd["PBMPWCL"].loc[1].loc[10] * SCALPR2

    #       Minnesota, Dakotas (West North Central 1)
    # T111(1:16,IY,IS)=PBMPWCL(0,1:16,IY)
    z[10] = dfd["PBMPWCL"].loc[1].loc[11] * SCALPR2

    #       West North Central 2
    # T111(1:16,IY,IS)=PBMPWCL(0,1:16,IY)
    z[11] = dfd["PBMPWCL"].loc[1].loc[12] * SCALPR2

    #       West South Central
    # T111(1:16,IY,IS)=PBMPWCL(0,1:16,IY)
    z[12] = dfd["PBMPWCL"].loc[1].loc[13] * SCALPR2

    #       Montana, Wyoming, and Idaho (Mountain 1)
    # T111(1:16,IY,IS)=PBMPWCL(0,1:16,IY)
    z[13] = dfd["PBMPWCL"].loc[1].loc[14] * SCALPR2

    #       Colorado, Utah, and Nevada (Mountain 2)
    # T111(1:16,IY,IS)=PBMPWCL(0,1:16,IY)
    z[14] = dfd["PBMPWCL"].loc[1].loc[15] * SCALPR2

    #       Arizona and New Mexico (Mountain 3)
    # T111(1:16,IY,IS)=PBMPWCL(0,1:16,IY)
    z[15] = dfd["PBMPWCL"].loc[1].loc[16] * SCALPR2

    #       Pacific
    # T111(1:16,IY,IS)=PBMPWCL(0,1:16,IY)
    z[16] = dfd["PBMPWCL"].loc[1].loc[17] * SCALPR2

    #     Electric Power Total, All Types
    # T111(161,IY,IS)=(PBMPWCL(0,1,IY)*QBMPWCL(0,1,IY)+PBMPWCL(0,2,IY)*QBMPWCL(0,2,IY)+PBMPWCL(0,3,IY)*QBMPWCL(0,3,IY)+PBMPWCL(0,4,IY)*QBMPWCL(0,4,IY)+PBMPWCL(0,5,IY)*QBMPWCL(0,5,IY)+
    #                 PBMPWCL(0,6,IY)*QBMPWCL(0,6,IY)+PBMPWCL(0,7,IY)*QBMPWCL(0,7,IY)+PBMPWCL(0,8,IY)*QBMPWCL(0,8,IY)+PBMPWCL(0,9,IY)*QBMPWCL(0,9,IY)+PBMPWCL(0,10,IY)*QBMPWCL(0,10,IY)+
    #                 PBMPWCL(0,11,IY)*QBMPWCL(0,11,IY)+PBMPWCL(0,12,IY)*QBMPWCL(0,12,IY)+PBMPWCL(0,13,IY)*QBMPWCL(0,13,IY)+PBMPWCL(0,14,IY)*QBMPWCL(0,14,IY)+PBMPWCL(0,15,IY)*QBMPWCL(0,15,IY)+
    #                 PBMPWCL(0,16,IY)*QBMPWCL(0,16,IY))/SUM(QBMPWCL(0,1:16,IY))
    z[161] = (
        (
            (
                dfd["PBMPWCL"].loc[1].loc[2] * dfd["QBMPWCL"].loc[1].loc[2]
                + dfd["PBMPWCL"].loc[1].loc[3] * dfd["QBMPWCL"].loc[1].loc[3]
                + dfd["PBMPWCL"].loc[1].loc[4] * dfd["QBMPWCL"].loc[1].loc[4]
                + dfd["PBMPWCL"].loc[1].loc[5] * dfd["QBMPWCL"].loc[1].loc[5]
                + dfd["PBMPWCL"].loc[1].loc[6] * dfd["QBMPWCL"].loc[1].loc[6]
                + dfd["PBMPWCL"].loc[1].loc[7] * dfd["QBMPWCL"].loc[1].loc[7]
                + dfd["PBMPWCL"].loc[1].loc[8] * dfd["QBMPWCL"].loc[1].loc[8]
                + dfd["PBMPWCL"].loc[1].loc[9] * dfd["QBMPWCL"].loc[1].loc[9]
                + dfd["PBMPWCL"].loc[1].loc[10] * dfd["QBMPWCL"].loc[1].loc[10]
                + dfd["PBMPWCL"].loc[1].loc[11] * dfd["QBMPWCL"].loc[1].loc[11]
                + dfd["PBMPWCL"].loc[1].loc[12] * dfd["QBMPWCL"].loc[1].loc[12]
                + dfd["PBMPWCL"].loc[1].loc[13] * dfd["QBMPWCL"].loc[1].loc[13]
                + dfd["PBMPWCL"].loc[1].loc[14] * dfd["QBMPWCL"].loc[1].loc[14]
                + dfd["PBMPWCL"].loc[1].loc[15] * dfd["QBMPWCL"].loc[1].loc[15]
                + dfd["PBMPWCL"].loc[1].loc[16] * dfd["QBMPWCL"].loc[1].loc[16]
                + dfd["PBMPWCL"].loc[1].loc[17] * dfd["QBMPWCL"].loc[1].loc[17]
            )
            / dfd["QBMPWCL"].loc[1, 2:17, :].sum()
        )
        * SCALPR2
    ).fillna(0)

    #     Refining

    #       New England
    # T111(17:32,IY,IS) = (PBMETCL(0,1:16,IY) * QBMETCL(0,1:16,IY) + PBMBTCL(0,1:16,IY) * QBMBTCL(0,1:16,IY)) / (QBMETCL(0,1:16,IY) + QBMBTCL(0,1:16,IY))
    z[17] = (
        (
            dfd["PBMETCL"].loc[1].loc[2] * dfd["QBMETCL"].loc[1].loc[2]
            + dfd["PBMBTCL"].loc[1].loc[2] * dfd["QBMBTCL"].loc[1].loc[2]
        )
        / (dfd["QBMETCL"].loc[1].loc[2] + dfd["QBMBTCL"].loc[1].loc[2]).replace(0, 1)
    ) * SCALPR2

    #       Middle Atlantic
    z[18] = (
        (
            dfd["PBMETCL"].loc[1].loc[3] * dfd["QBMETCL"].loc[1].loc[3]
            + dfd["PBMBTCL"].loc[1].loc[3] * dfd["QBMBTCL"].loc[1].loc[3]
        )
        / (dfd["QBMETCL"].loc[1].loc[3] + dfd["QBMBTCL"].loc[1].loc[3]).replace(0, 1)
    ) * SCALPR2

    #       South Atlantic 1
    z[19] = (
        (
            dfd["PBMETCL"].loc[1].loc[4] * dfd["QBMETCL"].loc[1].loc[4]
            + dfd["PBMBTCL"].loc[1].loc[4] * dfd["QBMBTCL"].loc[1].loc[4]
        )
        / (dfd["QBMETCL"].loc[1].loc[4] + dfd["QBMBTCL"].loc[1].loc[4]).replace(0, 1)
    ) * SCALPR2

    #       Virginia, Carolinas (South Atlantic 2)
    z[20] = (
        (
            dfd["PBMETCL"].loc[1].loc[5] * dfd["QBMETCL"].loc[1].loc[5]
            + dfd["PBMBTCL"].loc[1].loc[5] * dfd["QBMBTCL"].loc[1].loc[5]
        )
        / (dfd["QBMETCL"].loc[1].loc[5] + dfd["QBMBTCL"].loc[1].loc[5]).replace(0, 1)
    ) * SCALPR2

    #       Georgia and Florida (South Atlantic 3)
    z[21] = (
        (
            dfd["PBMETCL"].loc[1].loc[6] * dfd["QBMETCL"].loc[1].loc[6]
            + dfd["PBMBTCL"].loc[1].loc[6] * dfd["QBMBTCL"].loc[1].loc[6]
        )
        / (dfd["QBMETCL"].loc[1].loc[6] + dfd["QBMBTCL"].loc[1].loc[6]).replace(0, 1)
    ) * SCALPR2

    #       Ohio (East North Central 1)
    z[22] = (
        (
            dfd["PBMETCL"].loc[1].loc[7] * dfd["QBMETCL"].loc[1].loc[7]
            + dfd["PBMBTCL"].loc[1].loc[7] * dfd["QBMBTCL"].loc[1].loc[7]
        )
        / (dfd["QBMETCL"].loc[1].loc[7] + dfd["QBMBTCL"].loc[1].loc[7]).replace(0, 1)
    ) * SCALPR2

    #       East North Central 2
    z[23] = (
        (
            dfd["PBMETCL"].loc[1].loc[8] * dfd["QBMETCL"].loc[1].loc[8]
            + dfd["PBMBTCL"].loc[1].loc[8] * dfd["QBMBTCL"].loc[1].loc[8]
        )
        / (dfd["QBMETCL"].loc[1].loc[8] + dfd["QBMBTCL"].loc[1].loc[8]).replace(0, 1)
    ) * SCALPR2

    #       Kentucky and Tennessee (East South Cent 1)
    z[24] = (
        (
            dfd["PBMETCL"].loc[1].loc[9] * dfd["QBMETCL"].loc[1].loc[9]
            + dfd["PBMBTCL"].loc[1].loc[9] * dfd["QBMBTCL"].loc[1].loc[9]
        )
        / (dfd["QBMETCL"].loc[1].loc[9] + dfd["QBMBTCL"].loc[1].loc[9]).replace(0, 1)
    ) * SCALPR2

    #       Alabama and Mississippi (East South Cent 2)
    z[25] = (
        (
            dfd["PBMETCL"].loc[1].loc[10] * dfd["QBMETCL"].loc[1].loc[10]
            + dfd["PBMBTCL"].loc[1].loc[10] * dfd["QBMBTCL"].loc[1].loc[10]
        )
        / (dfd["QBMETCL"].loc[1].loc[10] + dfd["QBMBTCL"].loc[1].loc[10]).replace(0, 1)
    ) * SCALPR2

    #       Minnesota, Dakotas (West North Central 1)
    z[26] = (
        (
            dfd["PBMETCL"].loc[1].loc[11] * dfd["QBMETCL"].loc[1].loc[11]
            + dfd["PBMBTCL"].loc[1].loc[11] * dfd["QBMBTCL"].loc[1].loc[11]
        )
        / (dfd["QBMETCL"].loc[1].loc[11] + dfd["QBMBTCL"].loc[1].loc[11]).replace(0, 1)
    ) * SCALPR2

    #       West North Central 2
    z[27] = (
        (
            dfd["PBMETCL"].loc[1].loc[12] * dfd["QBMETCL"].loc[1].loc[12]
            + dfd["PBMBTCL"].loc[1].loc[12] * dfd["QBMBTCL"].loc[1].loc[12]
        )
        / (dfd["QBMETCL"].loc[1].loc[12] + dfd["QBMBTCL"].loc[1].loc[12]).replace(0, 1)
    ) * SCALPR2

    #       West South Central
    z[28] = (
        (
            dfd["PBMETCL"].loc[1].loc[13] * dfd["QBMETCL"].loc[1].loc[13]
            + dfd["PBMBTCL"].loc[1].loc[13] * dfd["QBMBTCL"].loc[1].loc[13]
        )
        / (dfd["QBMETCL"].loc[1].loc[13] + dfd["QBMBTCL"].loc[1].loc[13]).replace(0, 1)
    ) * SCALPR2

    #       Montana, Wyoming, and Idaho (Mountain 1)
    z[29] = (
        (
            dfd["PBMETCL"].loc[1].loc[14] * dfd["QBMETCL"].loc[1].loc[14]
            + dfd["PBMBTCL"].loc[1].loc[14] * dfd["QBMBTCL"].loc[1].loc[14]
        )
        / (dfd["QBMETCL"].loc[1].loc[14] + dfd["QBMBTCL"].loc[1].loc[14]).replace(0, 1)
    ) * SCALPR2

    #       Colorado, Utah, and Nevada (Mountain 2)
    z[30] = (
        (
            dfd["PBMETCL"].loc[1].loc[15] * dfd["QBMETCL"].loc[1].loc[15]
            + dfd["PBMBTCL"].loc[1].loc[15] * dfd["QBMBTCL"].loc[1].loc[15]
        )
        / (dfd["QBMETCL"].loc[1].loc[15] + dfd["QBMBTCL"].loc[1].loc[15]).replace(0, 1)
    ) * SCALPR2
    #       Arizona and New Mexico (Mountain 3)
    z[31] = (
        (
            dfd["PBMETCL"].loc[1].loc[16] * dfd["QBMETCL"].loc[1].loc[16]
            + dfd["PBMBTCL"].loc[1].loc[16] * dfd["QBMBTCL"].loc[1].loc[16]
        )
        / (dfd["QBMETCL"].loc[1].loc[16] + dfd["QBMBTCL"].loc[1].loc[16]).replace(0, 1)
    ) * SCALPR2
    #       Pacific
    z[32] = (
        (
            dfd["PBMETCL"].loc[1].loc[17] * dfd["QBMETCL"].loc[1].loc[17]
            + dfd["PBMBTCL"].loc[1].loc[17] * dfd["QBMBTCL"].loc[1].loc[17]
        )
        / (dfd["QBMETCL"].loc[1].loc[17] + dfd["QBMBTCL"].loc[1].loc[17]).replace(0, 1)
    ) * SCALPR2

    #  Refining Total, All Types
    # T111(162,IY,IS)=T111(162,IY,IS)+(PBMETCL(0,IR,IY)*QBMETCL(0,IR,IY)+PBMBTCL(0,IR,IY)*QBMBTCL(0,IR,IY))/(SUM(QBMETCL(0,1:16,IY))+SUM(QBMBTCL(0,1:16,IY)))
    # z[162] = (dfd['PBMETCL'].loc[1, 2:17,:].sum()*dfd['QBMETCL'].loc[1, 2:17, :].sum()+dfd['PBMBTCL'].loc[1, 2:17, :].sum()*dfd['QBMBTCL'].loc[1, 2:17, :].sum())/(dfd['QBMETCL'].loc[1,2:17,:].sum()+ dfd['QBMBTCL'].loc[1,2:1,:].sum())* SCALPR2
    z[162] = (
        (
            dfd["PBMETCL"].loc[1].loc[2] * dfd["QBMETCL"].loc[1].loc[2]
            + dfd["PBMETCL"].loc[1].loc[3] * dfd["QBMETCL"].loc[1].loc[3]
            + dfd["PBMETCL"].loc[1].loc[4] * dfd["QBMETCL"].loc[1].loc[4]
            + dfd["PBMETCL"].loc[1].loc[5] * dfd["QBMETCL"].loc[1].loc[5]
            + dfd["PBMETCL"].loc[1].loc[6] * dfd["QBMETCL"].loc[1].loc[6]
            + dfd["PBMETCL"].loc[1].loc[7] * dfd["QBMETCL"].loc[1].loc[7]
            + dfd["PBMETCL"].loc[1].loc[8] * dfd["QBMETCL"].loc[1].loc[8]
            + dfd["PBMETCL"].loc[1].loc[9] * dfd["QBMETCL"].loc[1].loc[9]
            + dfd["PBMETCL"].loc[1].loc[10] * dfd["QBMETCL"].loc[1].loc[10]
            + dfd["PBMETCL"].loc[1].loc[11] * dfd["QBMETCL"].loc[1].loc[11]
            + dfd["PBMETCL"].loc[1].loc[12] * dfd["QBMETCL"].loc[1].loc[12]
            + dfd["PBMETCL"].loc[1].loc[13] * dfd["QBMETCL"].loc[1].loc[13]
            + dfd["PBMETCL"].loc[1].loc[14] * dfd["QBMETCL"].loc[1].loc[14]
            + dfd["PBMETCL"].loc[1].loc[15] * dfd["QBMETCL"].loc[1].loc[15]
            + dfd["PBMETCL"].loc[1].loc[16] * dfd["QBMETCL"].loc[1].loc[16]
            + dfd["PBMETCL"].loc[1].loc[17] * dfd["QBMETCL"].loc[1].loc[17]
            + dfd["PBMBTCL"].loc[1].loc[2] * dfd["QBMBTCL"].loc[1].loc[2]
            + dfd["PBMBTCL"].loc[1].loc[3] * dfd["QBMBTCL"].loc[1].loc[3]
            + dfd["PBMBTCL"].loc[1].loc[4] * dfd["QBMBTCL"].loc[1].loc[4]
            + dfd["PBMBTCL"].loc[1].loc[5] * dfd["QBMBTCL"].loc[1].loc[5]
            + dfd["PBMBTCL"].loc[1].loc[6] * dfd["QBMBTCL"].loc[1].loc[6]
            + dfd["PBMBTCL"].loc[1].loc[7] * dfd["QBMBTCL"].loc[1].loc[7]
            + dfd["PBMBTCL"].loc[1].loc[8] * dfd["QBMBTCL"].loc[1].loc[8]
            + dfd["PBMBTCL"].loc[1].loc[9] * dfd["QBMBTCL"].loc[1].loc[9]
            + dfd["PBMBTCL"].loc[1].loc[10] * dfd["QBMBTCL"].loc[1].loc[10]
            + dfd["PBMBTCL"].loc[1].loc[11] * dfd["QBMBTCL"].loc[1].loc[11]
            + dfd["PBMBTCL"].loc[1].loc[12] * dfd["QBMBTCL"].loc[1].loc[12]
            + dfd["PBMBTCL"].loc[1].loc[13] * dfd["QBMBTCL"].loc[1].loc[13]
            + dfd["PBMBTCL"].loc[1].loc[14] * dfd["QBMBTCL"].loc[1].loc[14]
            + dfd["PBMBTCL"].loc[1].loc[15] * dfd["QBMBTCL"].loc[1].loc[15]
            + dfd["PBMBTCL"].loc[1].loc[16] * dfd["QBMBTCL"].loc[1].loc[16]
            + dfd["PBMBTCL"].loc[1].loc[17] * dfd["QBMBTCL"].loc[1].loc[17]
        )
        / (
            dfd["QBMETCL"].loc[1, 2:17, :].sum() + dfd["QBMBTCL"].loc[1, 2:17, :].sum()
        ).replace(0, 1)
        * SCALPR2
    ).fillna(0)

    #   Total, All Types
    # T111(163,IY,IS)=(T111(161,IY,IS)*SUM(QBMPWCL(0,1:16,IY))+T111(162,IY,IS)*(SUM(QBMETCL(0,1:16,IY))+SUM(QBMBTCL(0,1:16,IY))))/(SUM(QBMPWCL(0,1:16,IY))+SUM(QBMETCL(0,1:16,IY))+SUM(QBMBTCL(0,1:16,IY)))
    z[163] = (
        z[161] * dfd["QBMPWCL"].loc[1, 2:17, :].sum()
        + z[162]
        * (dfd["QBMETCL"].loc[1, 2:17, :].sum() + dfd["QBMBTCL"].loc[1, 2:17, :].sum())
    ) / (
        dfd["QBMPWCL"].loc[1, 2:17, :].sum()
        + dfd["QBMETCL"].loc[1, 2:17, :].sum()
        + dfd["QBMBTCL"].loc[1, 2:17, :].sum()
    ).replace(
        0, 1
    )

    #   Urban Wood Waste

    #     Electric Power
    # T111(33:48,IY,IS) = PBMPWCL(1,1:16,IY)
    #       New England
    z[33] = dfd["PBMPWCL"].loc[2].loc[2] * SCALPR2
    #       Middle Atlantic
    z[34] = dfd["PBMPWCL"].loc[2].loc[3] * SCALPR2
    #       South Atlantic 1
    z[35] = dfd["PBMPWCL"].loc[2].loc[4] * SCALPR2
    #       Virginia, Carolinas (South Atlantic 2)
    z[36] = dfd["PBMPWCL"].loc[2].loc[5] * SCALPR2
    #       Georgia and Florida (South Atlantic 3)
    z[37] = dfd["PBMPWCL"].loc[2].loc[6] * SCALPR2
    #       Ohio (East North Central 1)
    z[38] = dfd["PBMPWCL"].loc[2].loc[7] * SCALPR2
    #       East North Central 2
    z[39] = dfd["PBMPWCL"].loc[2].loc[8] * SCALPR2
    #       Kentucky and Tennessee (East South Cent 1)
    z[40] = dfd["PBMPWCL"].loc[2].loc[9] * SCALPR2
    #       Alabama and Mississippi (East South Cent 2)
    z[41] = dfd["PBMPWCL"].loc[2].loc[10] * SCALPR2
    #       Minnesota, Dakotas (West North Central 1)
    z[42] = dfd["PBMPWCL"].loc[2].loc[11] * SCALPR2
    #       West North Central 2
    z[43] = dfd["PBMPWCL"].loc[2].loc[12] * SCALPR2
    #       West South Central
    z[44] = dfd["PBMPWCL"].loc[2].loc[13] * SCALPR2
    #       Montana, Wyoming, and Idaho (Mountain 1)
    z[45] = dfd["PBMPWCL"].loc[2].loc[14] * SCALPR2
    #       Colorado, Utah, and Nevada (Mountain 2)
    z[46] = dfd["PBMPWCL"].loc[2].loc[15] * SCALPR2
    #       Arizona and New Mexico (Mountain 3)
    z[47] = dfd["PBMPWCL"].loc[2].loc[16] * SCALPR2
    #       Pacific
    z[48] = dfd["PBMPWCL"].loc[2].loc[17] * SCALPR2

    #     Electric Power Total, Urban Wood Waste
    # T111(164,IY,IS)=(PBMPWCL(1,1,IY)*QBMPWCL(1,1,IY)+PBMPWCL(1,2,IY)*QBMPWCL(1,2,IY)+PBMPWCL(1,3,IY)*QBMPWCL(1,3,IY)+PBMPWCL(1,4,IY)*QBMPWCL(1,4,IY)+PBMPWCL(1,5,IY)*QBMPWCL(1,5,IY)+PBMPWCL(1,6,IY)*QBMPWCL(1,6,IY)+PBMPWCL(1,7,IY)*QBMPWCL(1,7,IY)+PBMPWCL(1,8,IY)*QBMPWCL(1,8,IY)+PBMPWCL(1,9,IY)*QBMPWCL(1,9,IY)+PBMPWCL(1,10,IY)*QBMPWCL(1,10,IY)+PBMPWCL(1,11,IY)*QBMPWCL(1,11,IY)+PBMPWCL(1,12,IY)*QBMPWCL(1,12,IY)+PBMPWCL(1,13,IY)*QBMPWCL(1,13,IY)+PBMPWCL(1,14,IY)*QBMPWCL(1,14,IY)+PBMPWCL(1,15,IY)*QBMPWCL(1,15,IY)+PBMPWCL(1,16,IY)*QBMPWCL(1,16,IY))/SUM(QBMPWCL(1,1:16,IY))
    z[164] = (
        (
            (
                dfd["PBMPWCL"].loc[2].loc[17] * dfd["QBMPWCL"].loc[2].loc[17]
                + dfd["PBMPWCL"].loc[2].loc[2] * dfd["QBMPWCL"].loc[2].loc[2]
                + dfd["PBMPWCL"].loc[2].loc[3] * dfd["QBMPWCL"].loc[2].loc[3]
                + dfd["PBMPWCL"].loc[2].loc[4] * dfd["QBMPWCL"].loc[2].loc[4]
                + dfd["PBMPWCL"].loc[2].loc[5] * dfd["QBMPWCL"].loc[2].loc[5]
                + dfd["PBMPWCL"].loc[2].loc[6] * dfd["QBMPWCL"].loc[2].loc[6]
                + dfd["PBMPWCL"].loc[2].loc[7] * dfd["QBMPWCL"].loc[2].loc[7]
                + dfd["PBMPWCL"].loc[2].loc[8] * dfd["QBMPWCL"].loc[2].loc[8]
                + dfd["PBMPWCL"].loc[2].loc[9] * dfd["QBMPWCL"].loc[2].loc[9]
                + dfd["PBMPWCL"].loc[2].loc[10] * dfd["QBMPWCL"].loc[2].loc[10]
                + dfd["PBMPWCL"].loc[2].loc[11] * dfd["QBMPWCL"].loc[2].loc[11]
                + dfd["PBMPWCL"].loc[2].loc[12] * dfd["QBMPWCL"].loc[2].loc[12]
                + dfd["PBMPWCL"].loc[2].loc[13] * dfd["QBMPWCL"].loc[2].loc[13]
                + dfd["PBMPWCL"].loc[2].loc[14] * dfd["QBMPWCL"].loc[2].loc[14]
                + dfd["PBMPWCL"].loc[2].loc[15] * dfd["QBMPWCL"].loc[2].loc[15]
                + dfd["PBMPWCL"].loc[2].loc[16] * dfd["QBMPWCL"].loc[2].loc[16]
            )
            / dfd["QBMPWCL"].loc[2].loc[2:17].sum().replace(0, 1)
        )
        * SCALPR2
    ).fillna(0)

    #     Refining

    #       New England
    # T111(49:64,IY,IS) = (PBMETCL(1,1:16,IY) * QBMETCL(1,1:16,IY) + PBMBTCL(1,1:16,IY) * QBMBTCL(1,1:16,IY)) / (QBMETCL(1,1:16,IY) + QBMBTCL(1,1:16,IY))
    z[49] = (
        (
            dfd["PBMETCL"].loc[2].loc[2] * dfd["QBMETCL"].loc[2].loc[2]
            + dfd["PBMBTCL"].loc[2].loc[2] * dfd["QBMBTCL"].loc[2].loc[2]
        )
        / (dfd["QBMETCL"].loc[2].loc[2] + dfd["QBMBTCL"].loc[2].loc[2]).replace(0, 1)
    ) * SCALPR2

    #       Middle Atlantic
    # T111(49:64,IY,IS) = (PBMETCL(1,1:16,IY) * QBMETCL(1,1:16,IY) + PBMBTCL(1,1:16,IY) * QBMBTCL(1,1:16,IY)) / (QBMETCL(1,1:16,IY) + QBMBTCL(1,1:16,IY))
    z[50] = (
        (
            dfd["PBMETCL"].loc[2].loc[3] * dfd["QBMETCL"].loc[2].loc[3]
            + dfd["PBMBTCL"].loc[2].loc[3] * dfd["QBMBTCL"].loc[2].loc[3]
        )
        / (dfd["QBMETCL"].loc[2].loc[3] + dfd["QBMBTCL"].loc[2].loc[3]).replace(0, 1)
    ) * SCALPR2

    #       South Atlantic 1
    # T111(49:64,IY,IS) = (PBMETCL(1,1:16,IY) * QBMETCL(1,1:16,IY) + PBMBTCL(1,1:16,IY) * QBMBTCL(1,1:16,IY)) / (QBMETCL(1,1:16,IY) + QBMBTCL(1,1:16,IY))
    z[51] = (
        (
            dfd["PBMETCL"].loc[2].loc[4] * dfd["QBMETCL"].loc[2].loc[4]
            + dfd["PBMBTCL"].loc[2].loc[4] * dfd["QBMBTCL"].loc[2].loc[4]
        )
        / (dfd["QBMETCL"].loc[2].loc[4] + dfd["QBMBTCL"].loc[2].loc[4]).replace(0, 1)
    ) * SCALPR2

    #       Virginia, Carolinas (South Atlantic 2)
    # T111(49:64,IY,IS) = (PBMETCL(1,1:16,IY) * QBMETCL(1,1:16,IY) + PBMBTCL(1,1:16,IY) * QBMBTCL(1,1:16,IY)) / (QBMETCL(1,1:16,IY) + QBMBTCL(1,1:16,IY))
    z[52] = (
        (
            dfd["PBMETCL"].loc[2].loc[5] * dfd["QBMETCL"].loc[2].loc[5]
            + dfd["PBMBTCL"].loc[2].loc[5] * dfd["QBMBTCL"].loc[2].loc[5]
        )
        / (dfd["QBMETCL"].loc[2].loc[5] + dfd["QBMBTCL"].loc[2].loc[5]).replace(0, 1)
    ) * SCALPR2

    #       Georgia and Florida (South Atlantic 3)
    # T111(49:64,IY,IS) = (PBMETCL(1,1:16,IY) * QBMETCL(1,1:16,IY) + PBMBTCL(1,1:16,IY) * QBMBTCL(1,1:16,IY)) / (QBMETCL(1,1:16,IY) + QBMBTCL(1,1:16,IY))
    z[53] = (
        (
            dfd["PBMETCL"].loc[2].loc[6] * dfd["QBMETCL"].loc[2].loc[6]
            + dfd["PBMBTCL"].loc[2].loc[6] * dfd["QBMBTCL"].loc[2].loc[6]
        )
        / (dfd["QBMETCL"].loc[2].loc[6] + dfd["QBMBTCL"].loc[2].loc[6]).replace(0, 1)
    ) * SCALPR2

    #       Ohio (East North Central 1)
    # T111(49:64,IY,IS) = (PBMETCL(1,1:16,IY) * QBMETCL(1,1:16,IY) + PBMBTCL(1,1:16,IY) * QBMBTCL(1,1:16,IY)) / (QBMETCL(1,1:16,IY) + QBMBTCL(1,1:16,IY))
    z[54] = (
        (
            dfd["PBMETCL"].loc[2].loc[7] * dfd["QBMETCL"].loc[2].loc[7]
            + dfd["PBMBTCL"].loc[2].loc[7] * dfd["QBMBTCL"].loc[2].loc[7]
        )
        / (dfd["QBMETCL"].loc[2].loc[7] + dfd["QBMBTCL"].loc[2].loc[7]).replace(0, 1)
    ) * SCALPR2

    #       East North Central 2
    # T111(49:64,IY,IS) = (PBMETCL(1,1:16,IY) * QBMETCL(1,1:16,IY) + PBMBTCL(1,1:16,IY) * QBMBTCL(1,1:16,IY)) / (QBMETCL(1,1:16,IY) + QBMBTCL(1,1:16,IY))
    z[55] = (
        (
            dfd["PBMETCL"].loc[2].loc[8] * dfd["QBMETCL"].loc[2].loc[8]
            + dfd["PBMBTCL"].loc[2].loc[8] * dfd["QBMBTCL"].loc[2].loc[8]
        )
        / (dfd["QBMETCL"].loc[2].loc[8] + dfd["QBMBTCL"].loc[2].loc[8]).replace(0, 1)
    ) * SCALPR2

    #       Kentucky and Tennessee (East South Cent 1)
    z[56] = (
        (
            dfd["PBMETCL"].loc[2].loc[9] * dfd["QBMETCL"].loc[2].loc[9]
            + dfd["PBMBTCL"].loc[2].loc[9] * dfd["QBMBTCL"].loc[2].loc[9]
        )
        / (dfd["QBMETCL"].loc[2].loc[9] + dfd["QBMBTCL"].loc[2].loc[9]).replace(0, 1)
    ) * SCALPR2

    #       Alabama and Mississippi (East South Cent 2)
    z[57] = (
        (
            dfd["PBMETCL"].loc[2].loc[10] * dfd["QBMETCL"].loc[2].loc[10]
            + dfd["PBMBTCL"].loc[2].loc[10] * dfd["QBMBTCL"].loc[2].loc[10]
        )
        / (dfd["QBMETCL"].loc[2].loc[10] + dfd["QBMBTCL"].loc[2].loc[10]).replace(0, 1)
    ) * SCALPR2

    #       Minnesota, Dakotas (West North Central 1)
    z[58] = (
        (
            dfd["PBMETCL"].loc[2].loc[11] * dfd["QBMETCL"].loc[2].loc[11]
            + dfd["PBMBTCL"].loc[2].loc[11] * dfd["QBMBTCL"].loc[2].loc[11]
        )
        / (dfd["QBMETCL"].loc[2].loc[11] + dfd["QBMBTCL"].loc[2].loc[11]).replace(0, 1)
    ) * SCALPR2

    #       West North Central 2
    z[59] = (
        (
            dfd["PBMETCL"].loc[2].loc[12] * dfd["QBMETCL"].loc[2].loc[12]
            + dfd["PBMBTCL"].loc[2].loc[12] * dfd["QBMBTCL"].loc[2].loc[12]
        )
        / (dfd["QBMETCL"].loc[2].loc[12] + dfd["QBMBTCL"].loc[2].loc[12]).replace(0, 1)
    ) * SCALPR2

    #       West South Central
    z[60] = (
        (
            dfd["PBMETCL"].loc[2].loc[13] * dfd["QBMETCL"].loc[2].loc[13]
            + dfd["PBMBTCL"].loc[2].loc[13] * dfd["QBMBTCL"].loc[2].loc[13]
        )
        / (dfd["QBMETCL"].loc[2].loc[13] + dfd["QBMBTCL"].loc[2].loc[13]).replace(0, 1)
    ) * SCALPR2

    #       Montana, Wyoming, and Idaho (Mountain 1)
    z[61] = (
        (
            dfd["PBMETCL"].loc[2].loc[14] * dfd["QBMETCL"].loc[2].loc[14]
            + dfd["PBMBTCL"].loc[2].loc[14] * dfd["QBMBTCL"].loc[2].loc[14]
        )
        / (dfd["QBMETCL"].loc[2].loc[14] + dfd["QBMBTCL"].loc[2].loc[14]).replace(0, 1)
    ) * SCALPR2
    #       Colorado, Utah, and Nevada (Mountain 2)

    z[62] = (
        (
            dfd["PBMETCL"].loc[2].loc[15] * dfd["QBMETCL"].loc[2].loc[15]
            + dfd["PBMBTCL"].loc[2].loc[15] * dfd["QBMBTCL"].loc[2].loc[15]
        )
        / (dfd["QBMETCL"].loc[2].loc[15] + dfd["QBMBTCL"].loc[2].loc[15]).replace(0, 1)
    ) * SCALPR2

    #       Arizona and New Mexico (Mountain 3)
    z[63] = (
        (
            dfd["PBMETCL"].loc[2].loc[16] * dfd["QBMETCL"].loc[2].loc[16]
            + dfd["PBMBTCL"].loc[2].loc[16] * dfd["QBMBTCL"].loc[2].loc[16]
        )
        / (dfd["QBMETCL"].loc[2].loc[16] + dfd["QBMBTCL"].loc[2].loc[16]).replace(0, 1)
    ) * SCALPR2
    #       Pacific
    z[64] = (
        (
            dfd["PBMETCL"].loc[2].loc[17] * dfd["QBMETCL"].loc[2].loc[17]
            + dfd["PBMBTCL"].loc[2].loc[17] * dfd["QBMBTCL"].loc[2].loc[17]
        )
        / (dfd["QBMETCL"].loc[2].loc[17] + dfd["QBMBTCL"].loc[2].loc[17]).replace(0, 1)
    ) * SCALPR2

    #     Refining Total, Urban Wood Waste
    # T111(165,IY,IS)=T111(165,IY,IS)+(PBMETCL(1,IR,IY)*QBMETCL(1,IR,IY)+PBMBTCL(1,IR,IY)*QBMBTCL(1,IR,IY))/(SUM(QBMETCL(1,1:16,IY))+SUM(QBMBTCL(1,1:16,IY)))
    # z[165] = z[165]+(dfd['PBMETCL'].loc[1]*dfd['QBMETCL'].loc[1]+dfd['PBMBTCL'].loc[1]*dfd['QBMBTCL'].loc[1])/(SUM(dfd['QBMETCL'].loc[1].loc[1:16])+SUM(dfd['QBMBTCL'].loc[1].loc[1:16]))
    z[165] = (
        (
            dfd["PBMETCL"].loc[2].loc[2] * dfd["QBMETCL"].loc[2].loc[2]
            + dfd["PBMETCL"].loc[2].loc[3] * dfd["QBMETCL"].loc[2].loc[3]
            + dfd["PBMETCL"].loc[2].loc[4] * dfd["QBMETCL"].loc[2].loc[4]
            + dfd["PBMETCL"].loc[2].loc[5] * dfd["QBMETCL"].loc[2].loc[5]
            + dfd["PBMETCL"].loc[2].loc[6] * dfd["QBMETCL"].loc[2].loc[6]
            + dfd["PBMETCL"].loc[2].loc[7] * dfd["QBMETCL"].loc[2].loc[7]
            + dfd["PBMETCL"].loc[2].loc[8] * dfd["QBMETCL"].loc[2].loc[8]
            + dfd["PBMETCL"].loc[2].loc[9] * dfd["QBMETCL"].loc[2].loc[9]
            + dfd["PBMETCL"].loc[2].loc[10] * dfd["QBMETCL"].loc[2].loc[10]
            + dfd["PBMETCL"].loc[2].loc[11] * dfd["QBMETCL"].loc[2].loc[11]
            + dfd["PBMETCL"].loc[2].loc[12] * dfd["QBMETCL"].loc[2].loc[12]
            + dfd["PBMETCL"].loc[2].loc[13] * dfd["QBMETCL"].loc[2].loc[13]
            + dfd["PBMETCL"].loc[2].loc[14] * dfd["QBMETCL"].loc[2].loc[14]
            + dfd["PBMETCL"].loc[2].loc[15] * dfd["QBMETCL"].loc[2].loc[15]
            + dfd["PBMETCL"].loc[2].loc[16] * dfd["QBMETCL"].loc[2].loc[16]
            + dfd["PBMETCL"].loc[2].loc[17] * dfd["QBMETCL"].loc[2].loc[17]
            + dfd["PBMBTCL"].loc[2].loc[2] * dfd["QBMBTCL"].loc[2].loc[2]
            + dfd["PBMBTCL"].loc[2].loc[3] * dfd["QBMBTCL"].loc[2].loc[3]
            + dfd["PBMBTCL"].loc[2].loc[4] * dfd["QBMBTCL"].loc[2].loc[4]
            + dfd["PBMBTCL"].loc[2].loc[5] * dfd["QBMBTCL"].loc[2].loc[5]
            + dfd["PBMBTCL"].loc[2].loc[6] * dfd["QBMBTCL"].loc[2].loc[6]
            + dfd["PBMBTCL"].loc[2].loc[7] * dfd["QBMBTCL"].loc[2].loc[7]
            + dfd["PBMBTCL"].loc[2].loc[8] * dfd["QBMBTCL"].loc[2].loc[8]
            + dfd["PBMBTCL"].loc[2].loc[9] * dfd["QBMBTCL"].loc[2].loc[9]
            + dfd["PBMBTCL"].loc[2].loc[10] * dfd["QBMBTCL"].loc[2].loc[10]
            + dfd["PBMBTCL"].loc[2].loc[11] * dfd["QBMBTCL"].loc[2].loc[11]
            + dfd["PBMBTCL"].loc[2].loc[12] * dfd["QBMBTCL"].loc[2].loc[12]
            + dfd["PBMBTCL"].loc[2].loc[13] * dfd["QBMBTCL"].loc[2].loc[13]
            + dfd["PBMBTCL"].loc[2].loc[14] * dfd["QBMBTCL"].loc[2].loc[14]
            + dfd["PBMBTCL"].loc[2].loc[15] * dfd["QBMBTCL"].loc[2].loc[15]
            + dfd["PBMBTCL"].loc[2].loc[16] * dfd["QBMBTCL"].loc[2].loc[16]
            + dfd["PBMBTCL"].loc[2].loc[17] * dfd["QBMBTCL"].loc[2].loc[17]
        )
        / (
            dfd["QBMETCL"].loc[2, 2:17, :].sum() + dfd["QBMBTCL"].loc[2, 2:17, :].sum()
        ).replace(0, 1)
        * SCALPR2
    ).fillna(0)

    #   Total, Urban Wood Waste
    # T111(166,IY,IS)=(T111(164,IY,IS)*SUM(QBMPWCL(1,1:16,IY))+T111(165,IY,IS)*(SUM(QBMETCL(1,1:16,IY))+SUM(QBMBTCL(1,1:16,IY))))/(SUM(QBMPWCL(1,1:16,IY))+SUM(QBMETCL(1,1:16,IY))+SUM(QBMBTCL(1,1:16,IY)))
    # z[166] = (z[164]*(dfd['QBMPWCL'].loc[(1].loc[1:16])+z[165]*(SUM(dfd['QBMETCL'].loc[1].loc[1:16])+SUM(dfd['QBMBTCL'].loc[1].loc[1:16])))/((dfd['QBMPWCL'].loc[(1].loc[1:16])+SUM(dfd['QBMETCL'].loc[1].loc[1:16])+SUM(dfd['QBMBTCL'].loc[1].loc[1:16]))
    z[166] = (
        z[164] * dfd["QBMPWCL"].loc[2, 2:17, :].sum()
        + z[165]
        * (dfd["QBMETCL"].loc[2, 2:17, :].sum() + dfd["QBMBTCL"].loc[2, 2:17, :].sum())
    ) / (
        dfd["QBMPWCL"].loc[2, 2:17, :].sum()
        + dfd["QBMETCL"].loc[2, 2:17, :].sum()
        + dfd["QBMBTCL"].loc[2, 2:17, :].sum()
    ).replace(
        0, 1
    )

    #

    #   Forestry Residue, Public Lands

    #     Electric Power

    #       New England

    #       Middle Atlantic
    # T111(65:80,IY,IS)=PBMPWCL(2,1:16,IY)
    z[65] = dfd["PBMPWCL"].loc[3].loc[2] * SCALPR2

    #       South Atlantic 1
    # T111(65:80,IY,IS)=PBMPWCL(2,1:16,IY)
    z[66] = dfd["PBMPWCL"].loc[3].loc[3] * SCALPR2

    #       Virginia, Carolinas (South Atlantic 2)
    # T111(65:80,IY,IS)=PBMPWCL(2,1:16,IY)
    z[67] = dfd["PBMPWCL"].loc[3].loc[4] * SCALPR2

    #       Georgia and Florida (South Atlantic 3)
    # T111(65:80,IY,IS)=PBMPWCL(2,1:16,IY)
    z[68] = dfd["PBMPWCL"].loc[3].loc[5] * SCALPR2

    #       Ohio (East North Central 1)
    # T111(65:80,IY,IS)=PBMPWCL(2,1:16,IY)
    z[69] = dfd["PBMPWCL"].loc[3].loc[6] * SCALPR2

    #       East North Central 2
    # T111(65:80,IY,IS)=PBMPWCL(2,1:16,IY)
    z[70] = dfd["PBMPWCL"].loc[3].loc[7] * SCALPR2

    #       Kentucky and Tennessee (East South Cent 1)
    # T111(65:80,IY,IS)=PBMPWCL(2,1:16,IY)
    z[71] = dfd["PBMPWCL"].loc[3].loc[8] * SCALPR2

    #       Alabama and Mississippi (East South Cent 2)
    # T111(65:80,IY,IS)=PBMPWCL(2,1:16,IY)
    z[72] = dfd["PBMPWCL"].loc[3].loc[9] * SCALPR2

    #       Minnesota, Dakotas (West North Central 1)
    # T111(65:80,IY,IS)=PBMPWCL(2,1:16,IY)
    z[73] = dfd["PBMPWCL"].loc[3].loc[10] * SCALPR2

    #       West North Central 2
    # T111(65:80,IY,IS)=PBMPWCL(2,1:16,IY)
    z[74] = dfd["PBMPWCL"].loc[3].loc[11] * SCALPR2

    #       West South Central
    # T111(65:80,IY,IS)=PBMPWCL(2,1:16,IY)
    z[75] = dfd["PBMPWCL"].loc[3].loc[12] * SCALPR2

    #       Montana, Wyoming, and Idaho (Mountain 1)
    # T111(65:80,IY,IS)=PBMPWCL(2,1:16,IY)
    z[76] = dfd["PBMPWCL"].loc[3].loc[13] * SCALPR2

    #       Colorado, Utah, and Nevada (Mountain 2)
    # T111(65:80,IY,IS)=PBMPWCL(2,1:16,IY)
    z[77] = dfd["PBMPWCL"].loc[3].loc[14] * SCALPR2

    #       Arizona and New Mexico (Mountain 3)
    # T111(65:80,IY,IS)=PBMPWCL(2,1:16,IY)
    z[78] = dfd["PBMPWCL"].loc[3].loc[15] * SCALPR2

    #       Pacific
    # T111(65:80,IY,IS)=PBMPWCL(2,1:16,IY)
    z[79] = dfd["PBMPWCL"].loc[3].loc[16] * SCALPR2
    z[80] = dfd["PBMPWCL"].loc[3].loc[17] * SCALPR2

    #     Electric Power Total, Public Forestry Residue
    # T111(167,IY,IS)=(PBMPWCL(2,1,IY)*QBMPWCL(2,1,IY)+PBMPWCL(2,2,IY)*QBMPWCL(2,2,IY)+PBMPWCL(2,3,IY)*QBMPWCL(2,3,IY)+PBMPWCL(2,4,IY)*QBMPWCL(2,4,IY)+PBMPWCL(2,5,IY)*QBMPWCL(2,5,IY)+PBMPWCL(2,6,IY)*QBMPWCL(2,6,IY)+PBMPWCL(2,7,IY)*QBMPWCL(2,7,IY)+PBMPWCL(2,8,IY)*QBMPWCL(2,8,IY)+PBMPWCL(2,9,IY)*QBMPWCL(2,9,IY)+PBMPWCL(2,10,IY)*QBMPWCL(2,10,IY)+PBMPWCL(2,11,IY)*QBMPWCL(2,11,IY)+PBMPWCL(2,12,IY)*QBMPWCL(2,12,IY)+PBMPWCL(2,13,IY)*QBMPWCL(2,13,IY)+PBMPWCL(2,14,IY)*QBMPWCL(2,14,IY)+PBMPWCL(2,15,IY)*QBMPWCL(2,15,IY)+PBMPWCL(2,16,IY)*QBMPWCL(2,16,IY))/SUM(QBMPWCL(2,1:16,IY))
    z[167] = (
        (
            dfd["PBMPWCL"].loc[3].loc[2] * dfd["QBMPWCL"].loc[3].loc[2]
            + dfd["PBMPWCL"].loc[3].loc[3] * dfd["QBMPWCL"].loc[3].loc[3]
            + dfd["PBMPWCL"].loc[3].loc[4] * dfd["QBMPWCL"].loc[3].loc[4]
            + dfd["PBMPWCL"].loc[3].loc[5] * dfd["QBMPWCL"].loc[3].loc[5]
            + dfd["PBMPWCL"].loc[3].loc[6] * dfd["QBMPWCL"].loc[3].loc[6]
            + dfd["PBMPWCL"].loc[3].loc[7] * dfd["QBMPWCL"].loc[3].loc[7]
            + dfd["PBMPWCL"].loc[3].loc[8] * dfd["QBMPWCL"].loc[3].loc[8]
            + dfd["PBMPWCL"].loc[3].loc[9] * dfd["QBMPWCL"].loc[3].loc[9]
            + dfd["PBMPWCL"].loc[3].loc[10] * dfd["QBMPWCL"].loc[3].loc[10]
            + dfd["PBMPWCL"].loc[3].loc[11] * dfd["QBMPWCL"].loc[3].loc[11]
            + dfd["PBMPWCL"].loc[3].loc[12] * dfd["QBMPWCL"].loc[3].loc[12]
            + dfd["PBMPWCL"].loc[3].loc[13] * dfd["QBMPWCL"].loc[3].loc[13]
            + dfd["PBMPWCL"].loc[3].loc[14] * dfd["QBMPWCL"].loc[3].loc[14]
            + dfd["PBMPWCL"].loc[3].loc[15] * dfd["QBMPWCL"].loc[3].loc[15]
            + dfd["PBMPWCL"].loc[3].loc[16] * dfd["QBMPWCL"].loc[3].loc[16]
            + dfd["PBMPWCL"].loc[3].loc[17] * dfd["QBMPWCL"].loc[3].loc[17]
        )
        / dfd["QBMPWCL"].loc[3, 2:17, :].sum().replace(0, 1)
        * SCALPR2
    ).fillna(0)

    #     Refining

    #       New England

    #       Middle Atlantic
    # T111(81:96,IY,IS) = (PBMETCL(2,1:16,IY) * QBMETCL(2,1:16,IY) + PBMBTCL(2,1:16,IY) * QBMBTCL(2,1:16,IY)) / (QBMETCL(2,1:16,IY) + QBMBTCL(2,1:16,IY))
    z[81] = (
        (
            dfd["PBMETCL"].loc[3].loc[2] * dfd["QBMETCL"].loc[2].loc[2]
            + dfd["PBMBTCL"].loc[2].loc[2] * dfd["QBMBTCL"].loc[2].loc[2]
        )
        / (dfd["QBMETCL"].loc[2].loc[2] + dfd["QBMBTCL"].loc[2].loc[2]).replace(0, 1)
    ) * SCALPR2

    #       South Atlantic 1
    # T111(81:96,IY,IS) = (PBMETCL(2,1:16,IY) * QBMETCL(2,1:16,IY) + PBMBTCL(2,1:16,IY) * QBMBTCL(2,1:16,IY)) / (QBMETCL(2,1:16,IY) + QBMBTCL(2,1:16,IY))
    z[82] = (
        (
            dfd["PBMETCL"].loc[3].loc[3] * dfd["QBMETCL"].loc[2].loc[3]
            + dfd["PBMBTCL"].loc[2].loc[3] * dfd["QBMBTCL"].loc[2].loc[3]
        )
        / (dfd["QBMETCL"].loc[2].loc[3] + dfd["QBMBTCL"].loc[2].loc[3]).replace(0, 1)
    ) * SCALPR2

    #       Virginia, Carolinas (South Atlantic 2)
    # T111(81:96,IY,IS) = (PBMETCL(2,1:16,IY) * QBMETCL(2,1:16,IY) + PBMBTCL(2,1:16,IY) * QBMBTCL(2,1:16,IY)) / (QBMETCL(2,1:16,IY) + QBMBTCL(2,1:16,IY))
    z[83] = (
        (
            dfd["PBMETCL"].loc[3].loc[4] * dfd["QBMETCL"].loc[2].loc[4]
            + dfd["PBMBTCL"].loc[2].loc[4] * dfd["QBMBTCL"].loc[2].loc[4]
        )
        / (dfd["QBMETCL"].loc[2].loc[4] + dfd["QBMBTCL"].loc[2].loc[4]).replace(0, 1)
    ) * SCALPR2

    #       Georgia and Florida (South Atlantic 3)
    # T111(81:96,IY,IS) = (PBMETCL(2,1:16,IY) * QBMETCL(2,1:16,IY) + PBMBTCL(2,1:16,IY) * QBMBTCL(2,1:16,IY)) / (QBMETCL(2,1:16,IY) + QBMBTCL(2,1:16,IY))
    z[84] = (
        (
            dfd["PBMETCL"].loc[3].loc[5] * dfd["QBMETCL"].loc[2].loc[5]
            + dfd["PBMBTCL"].loc[2].loc[5] * dfd["QBMBTCL"].loc[2].loc[5]
        )
        / (dfd["QBMETCL"].loc[2].loc[5] + dfd["QBMBTCL"].loc[2].loc[5]).replace(0, 1)
    ) * SCALPR2

    #       Ohio (East North Central 1)
    # T111(81:96,IY,IS) = (PBMETCL(2,1:16,IY) * QBMETCL(2,1:16,IY) + PBMBTCL(2,1:16,IY) * QBMBTCL(2,1:16,IY)) / (QBMETCL(2,1:16,IY) + QBMBTCL(2,1:16,IY))
    z[85] = (
        (
            dfd["PBMETCL"].loc[3].loc[6] * dfd["QBMETCL"].loc[2].loc[6]
            + dfd["PBMBTCL"].loc[2].loc[6] * dfd["QBMBTCL"].loc[2].loc[6]
        )
        / (dfd["QBMETCL"].loc[2].loc[6] + dfd["QBMBTCL"].loc[2].loc[6]).replace(0, 1)
    ) * SCALPR2

    #       East North Central 2
    # T111(81:96,IY,IS) = (PBMETCL(2,1:16,IY) * QBMETCL(2,1:16,IY) + PBMBTCL(2,1:16,IY) * QBMBTCL(2,1:16,IY)) / (QBMETCL(2,1:16,IY) + QBMBTCL(2,1:16,IY))
    z[86] = (
        (
            dfd["PBMETCL"].loc[3].loc[7] * dfd["QBMETCL"].loc[2].loc[7]
            + dfd["PBMBTCL"].loc[2].loc[7] * dfd["QBMBTCL"].loc[2].loc[7]
        )
        / (dfd["QBMETCL"].loc[2].loc[7] + dfd["QBMBTCL"].loc[2].loc[7]).replace(0, 1)
    ) * SCALPR2

    #       Kentucky and Tennessee (East South Cent 1)
    # T111(81:96,IY,IS) = (PBMETCL(2,1:16,IY) * QBMETCL(2,1:16,IY) + PBMBTCL(2,1:16,IY) * QBMBTCL(2,1:16,IY)) / (QBMETCL(2,1:16,IY) + QBMBTCL(2,1:16,IY))
    z[87] = (
        (
            dfd["PBMETCL"].loc[3].loc[8] * dfd["QBMETCL"].loc[2].loc[8]
            + dfd["PBMBTCL"].loc[2].loc[8] * dfd["QBMBTCL"].loc[2].loc[8]
        )
        / (dfd["QBMETCL"].loc[2].loc[8] + dfd["QBMBTCL"].loc[2].loc[8]).replace(0, 1)
    ) * SCALPR2

    #       Alabama and Mississippi (East South Cent 2)
    # T111(81:96,IY,IS) = (PBMETCL(2,1:16,IY) * QBMETCL(2,1:16,IY) + PBMBTCL(2,1:16,IY) * QBMBTCL(2,1:16,IY)) / (QBMETCL(2,1:16,IY) + QBMBTCL(2,1:16,IY))
    z[88] = (
        (
            dfd["PBMETCL"].loc[3].loc[9] * dfd["QBMETCL"].loc[2].loc[9]
            + dfd["PBMBTCL"].loc[2].loc[9] * dfd["QBMBTCL"].loc[2].loc[9]
        )
        / (dfd["QBMETCL"].loc[2].loc[9] + dfd["QBMBTCL"].loc[2].loc[9]).replace(0, 1)
    ) * SCALPR2

    #       Minnesota, Dakotas (West North Central 1)
    # T111(81:96,IY,IS) = (PBMETCL(2,1:16,IY) * QBMETCL(2,1:16,IY) + PBMBTCL(2,1:16,IY) * QBMBTCL(2,1:16,IY)) / (QBMETCL(2,1:16,IY) + QBMBTCL(2,1:16,IY))
    z[89] = (
        (
            dfd["PBMETCL"].loc[3].loc[10] * dfd["QBMETCL"].loc[2].loc[10]
            + dfd["PBMBTCL"].loc[2].loc[10] * dfd["QBMBTCL"].loc[2].loc[10]
        )
        / (dfd["QBMETCL"].loc[2].loc[10] + dfd["QBMBTCL"].loc[2].loc[10]).replace(0, 1)
    ) * SCALPR2

    #       West North Central 2
    # T111(81:96,IY,IS) = (PBMETCL(2,1:16,IY) * QBMETCL(2,1:16,IY) + PBMBTCL(2,1:16,IY) * QBMBTCL(2,1:16,IY)) / (QBMETCL(2,1:16,IY) + QBMBTCL(2,1:16,IY))
    z[90] = (
        (
            dfd["PBMETCL"].loc[3].loc[11] * dfd["QBMETCL"].loc[2].loc[11]
            + dfd["PBMBTCL"].loc[2].loc[11] * dfd["QBMBTCL"].loc[2].loc[11]
        )
        / (dfd["QBMETCL"].loc[2].loc[11] + dfd["QBMBTCL"].loc[2].loc[11]).replace(0, 1)
    ) * SCALPR2

    #       West South Central
    # T111(81:96,IY,IS) = (PBMETCL(2,1:16,IY) * QBMETCL(2,1:16,IY) + PBMBTCL(2,1:16,IY) * QBMBTCL(2,1:16,IY)) / (QBMETCL(2,1:16,IY) + QBMBTCL(2,1:16,IY))
    z[91] = (
        (
            dfd["PBMETCL"].loc[3].loc[12] * dfd["QBMETCL"].loc[2].loc[12]
            + dfd["PBMBTCL"].loc[2].loc[12] * dfd["QBMBTCL"].loc[2].loc[12]
        )
        / (dfd["QBMETCL"].loc[2].loc[12] + dfd["QBMBTCL"].loc[2].loc[12]).replace(0, 1)
    ) * SCALPR2

    #       Montana, Wyoming, and Idaho (Mountain 1)
    # T111(81:96,IY,IS) = (PBMETCL(2,1:16,IY) * QBMETCL(2,1:16,IY) + PBMBTCL(2,1:16,IY) * QBMBTCL(2,1:16,IY)) / (QBMETCL(2,1:16,IY) + QBMBTCL(2,1:16,IY))
    z[92] = (
        (
            dfd["PBMETCL"].loc[3].loc[13] * dfd["QBMETCL"].loc[2].loc[13]
            + dfd["PBMBTCL"].loc[2].loc[13] * dfd["QBMBTCL"].loc[2].loc[13]
        )
        / (dfd["QBMETCL"].loc[2].loc[13] + dfd["QBMBTCL"].loc[2].loc[13]).replace(0, 1)
    ) * SCALPR2

    #       Colorado, Utah, and Nevada (Mountain 2)
    # T111(81:96,IY,IS) = (PBMETCL(2,1:16,IY) * QBMETCL(2,1:16,IY) + PBMBTCL(2,1:16,IY) * QBMBTCL(2,1:16,IY)) / (QBMETCL(2,1:16,IY) + QBMBTCL(2,1:16,IY))
    z[93] = (
        (
            dfd["PBMETCL"].loc[3].loc[14] * dfd["QBMETCL"].loc[2].loc[14]
            + dfd["PBMBTCL"].loc[2].loc[14] * dfd["QBMBTCL"].loc[2].loc[14]
        )
        / (dfd["QBMETCL"].loc[2].loc[14] + dfd["QBMBTCL"].loc[2].loc[14]).replace(0, 1)
    ) * SCALPR2

    #       Arizona and New Mexico (Mountain 3)
    # T111(81:96,IY,IS) = (PBMETCL(2,1:16,IY) * QBMETCL(2,1:16,IY) + PBMBTCL(2,1:16,IY) * QBMBTCL(2,1:16,IY)) / (QBMETCL(2,1:16,IY) + QBMBTCL(2,1:16,IY))
    z[94] = (
        (
            dfd["PBMETCL"].loc[3].loc[15] * dfd["QBMETCL"].loc[2].loc[15]
            + dfd["PBMBTCL"].loc[2].loc[15] * dfd["QBMBTCL"].loc[2].loc[15]
        )
        / (dfd["QBMETCL"].loc[2].loc[15] + dfd["QBMBTCL"].loc[2].loc[15]).replace(0, 1)
    ) * SCALPR2

    #       Pacific
    # T111(81:96,IY,IS) = (PBMETCL(2,1:16,IY) * QBMETCL(2,1:16,IY) + PBMBTCL(2,1:16,IY) * QBMBTCL(2,1:16,IY)) / (QBMETCL(2,1:16,IY) + QBMBTCL(2,1:16,IY))
    z[95] = (
        (
            dfd["PBMETCL"].loc[3].loc[16] * dfd["QBMETCL"].loc[2].loc[16]
            + dfd["PBMBTCL"].loc[2].loc[16] * dfd["QBMBTCL"].loc[2].loc[16]
        )
        / (dfd["QBMETCL"].loc[2].loc[16] + dfd["QBMBTCL"].loc[2].loc[16]).replace(0, 1)
    ) * SCALPR2
    z[96] = (
        (
            dfd["PBMETCL"].loc[3].loc[17] * dfd["QBMETCL"].loc[2].loc[17]
            + dfd["PBMBTCL"].loc[2].loc[17] * dfd["QBMBTCL"].loc[2].loc[17]
        )
        / (dfd["QBMETCL"].loc[2].loc[17] + dfd["QBMBTCL"].loc[2].loc[17]).replace(0, 1)
    ) * SCALPR2

    #     Refining Total, Public Forestry Residue
    # T111(168,IY,IS)=T111(168,IY,IS)+(PBMETCL(2,IR,IY)*QBMETCL(2,IR,IY)+PBMBTCL(2,IR,IY)*QBMBTCL(2,IR,IY))/(SUM(QBMETCL(2,1:16,IY))+SUM(QBMBTCL(2,1:16,IY)))
    # z[168] = z[168]+(dfd['PBMETCL'].loc[2]*dfd['QBMETCL'].loc[2]+dfd['PBMBTCL'].loc[2]*dfd['QBMBTCL'].loc[2])/(SUM(dfd['QBMETCL'].loc[2].loc[1:16])+SUM(dfd['QBMBTCL'].loc[2].loc[1:16]))
    z[168] = (
        (
            dfd["PBMETCL"].loc[3].loc[2] * dfd["QBMETCL"].loc[3].loc[2]
            + dfd["PBMETCL"].loc[3].loc[3] * dfd["QBMETCL"].loc[3].loc[3]
            + dfd["PBMETCL"].loc[3].loc[4] * dfd["QBMETCL"].loc[3].loc[4]
            + dfd["PBMETCL"].loc[3].loc[5] * dfd["QBMETCL"].loc[3].loc[5]
            + dfd["PBMETCL"].loc[3].loc[6] * dfd["QBMETCL"].loc[3].loc[6]
            + dfd["PBMETCL"].loc[3].loc[7] * dfd["QBMETCL"].loc[3].loc[7]
            + dfd["PBMETCL"].loc[3].loc[8] * dfd["QBMETCL"].loc[3].loc[8]
            + dfd["PBMETCL"].loc[3].loc[9] * dfd["QBMETCL"].loc[3].loc[9]
            + dfd["PBMETCL"].loc[3].loc[10] * dfd["QBMETCL"].loc[3].loc[10]
            + dfd["PBMETCL"].loc[3].loc[11] * dfd["QBMETCL"].loc[3].loc[11]
            + dfd["PBMETCL"].loc[3].loc[12] * dfd["QBMETCL"].loc[3].loc[12]
            + dfd["PBMETCL"].loc[3].loc[13] * dfd["QBMETCL"].loc[3].loc[13]
            + dfd["PBMETCL"].loc[3].loc[14] * dfd["QBMETCL"].loc[3].loc[14]
            + dfd["PBMETCL"].loc[3].loc[15] * dfd["QBMETCL"].loc[3].loc[15]
            + dfd["PBMETCL"].loc[3].loc[16] * dfd["QBMETCL"].loc[3].loc[16]
            + dfd["PBMETCL"].loc[3].loc[17] * dfd["QBMETCL"].loc[3].loc[17]
            + dfd["PBMBTCL"].loc[3].loc[2] * dfd["QBMBTCL"].loc[3].loc[2]
            + dfd["PBMBTCL"].loc[3].loc[3] * dfd["QBMBTCL"].loc[3].loc[3]
            + dfd["PBMBTCL"].loc[3].loc[4] * dfd["QBMBTCL"].loc[3].loc[4]
            + dfd["PBMBTCL"].loc[3].loc[5] * dfd["QBMBTCL"].loc[3].loc[5]
            + dfd["PBMBTCL"].loc[3].loc[6] * dfd["QBMBTCL"].loc[3].loc[6]
            + dfd["PBMBTCL"].loc[3].loc[7] * dfd["QBMBTCL"].loc[3].loc[7]
            + dfd["PBMBTCL"].loc[3].loc[8] * dfd["QBMBTCL"].loc[3].loc[8]
            + dfd["PBMBTCL"].loc[3].loc[9] * dfd["QBMBTCL"].loc[3].loc[9]
            + dfd["PBMBTCL"].loc[3].loc[10] * dfd["QBMBTCL"].loc[3].loc[10]
            + dfd["PBMBTCL"].loc[3].loc[11] * dfd["QBMBTCL"].loc[3].loc[11]
            + dfd["PBMBTCL"].loc[3].loc[12] * dfd["QBMBTCL"].loc[3].loc[12]
            + dfd["PBMBTCL"].loc[3].loc[13] * dfd["QBMBTCL"].loc[3].loc[13]
            + dfd["PBMBTCL"].loc[3].loc[14] * dfd["QBMBTCL"].loc[3].loc[14]
            + dfd["PBMBTCL"].loc[3].loc[15] * dfd["QBMBTCL"].loc[3].loc[15]
            + dfd["PBMBTCL"].loc[3].loc[16] * dfd["QBMBTCL"].loc[3].loc[16]
            + dfd["PBMBTCL"].loc[3].loc[17] * dfd["QBMBTCL"].loc[3].loc[17]
        )
        / (
            dfd["QBMETCL"].loc[3, 2:17, :].sum() + dfd["QBMBTCL"].loc[3, 2:17, :].sum()
        ).replace(0, 1)
        * SCALPR2
    ).fillna(0)
    #   Total, Forestry Residue, Public Lands
    # T111(169,IY,IS)=(T111(167,IY,IS)*SUM(QBMPWCL(2,1:16,IY))+T111(168,IY,IS)*(SUM(QBMETCL(2,1:16,IY))+SUM(QBMBTCL(2,1:16,IY))))/(SUM(QBMPWCL(2,1:16,IY))+SUM(QBMETCL(2,1:16,IY))+SUM(QBMBTCL(2,1:16,IY)))
    # z[169] = (z[167]*(dfd['QBMPWCL'].loc[(2].loc[1:16])+z[168]*(SUM(dfd['QBMETCL'].loc[2].loc[1:16])+SUM(dfd['QBMBTCL'].loc[2].loc[1:16])))/((dfd['QBMPWCL'].loc[(2].loc[1:16])+SUM(dfd['QBMETCL'].loc[2].loc[1:16])+SUM(dfd['QBMBTCL'].loc[2].loc[1:16]))
    z[169] = (
        z[167] * dfd["QBMPWCL"].loc[3, 2:17, :].sum()
        + z[168]
        * (dfd["QBMETCL"].loc[3, 2:17, :].sum() + dfd["QBMBTCL"].loc[3, 2:17, :].sum())
    ) / (
        dfd["QBMPWCL"].loc[3, 2:17, :].sum()
        + dfd["QBMETCL"].loc[3, 2:17, :].sum()
        + dfd["QBMBTCL"].loc[3, 2:17, :].sum()
    ).replace(
        0, 1
    )

    #

    #   Agriculture Residue and Energy Crops

    #     Electric Power

    #       New England

    # T111(97:112,IY,IS)=PBMPWCL(3,1:16,IY)
    z[97] = dfd["PBMPWCL"].loc[4].loc[2] * AGSCALPR
    #       Middle Atlantic

    # T111(97:112,IY,IS)=PBMPWCL(3,1:16,IY)
    z[98] = dfd["PBMPWCL"].loc[4].loc[3] * AGSCALPR
    #       South Atlantic 1

    # T111(97:112,IY,IS)=PBMPWCL(3,1:16,IY)
    z[99] = dfd["PBMPWCL"].loc[4].loc[4] * AGSCALPR

    #       Virginia, Carolinas (South Atlantic 2)
    # T111(97:112,IY,IS)=PBMPWCL(3,1:16,IY)
    z[100] = dfd["PBMPWCL"].loc[4].loc[5] * AGSCALPR

    #       Georgia and Florida (South Atlantic 3)
    # T111(97:112,IY,IS)=PBMPWCL(3,1:16,IY)
    z[101] = dfd["PBMPWCL"].loc[4].loc[6] * AGSCALPR
    #       Ohio (East North Central 1)

    # T111(97:112,IY,IS)=PBMPWCL(3,1:16,IY)
    z[102] = dfd["PBMPWCL"].loc[4].loc[7] * AGSCALPR

    #       East North Central 2
    # T111(97:112,IY,IS)=PBMPWCL(3,1:16,IY)
    z[103] = dfd["PBMPWCL"].loc[4].loc[8] * AGSCALPR

    #       Kentucky and Tennessee (East South Cent 1)
    # T111(97:112,IY,IS)=PBMPWCL(3,1:16,IY)
    z[104] = dfd["PBMPWCL"].loc[4].loc[9] * AGSCALPR
    #       Alabama and Mississippi (East South Cent 2)

    # T111(97:112,IY,IS)=PBMPWCL(3,1:16,IY)
    z[105] = dfd["PBMPWCL"].loc[4].loc[10] * AGSCALPR

    #       Minnesota, Dakotas (West North Central 1)
    # T111(97:112,IY,IS)=PBMPWCL(3,1:16,IY)
    z[106] = dfd["PBMPWCL"].loc[4].loc[11] * AGSCALPR
    #       West North Central 2

    # T111(97:112,IY,IS)=PBMPWCL(3,1:16,IY)
    z[107] = dfd["PBMPWCL"].loc[4].loc[12] * AGSCALPR
    #       West South Central

    # T111(97:112,IY,IS)=PBMPWCL(3,1:16,IY)
    z[108] = dfd["PBMPWCL"].loc[4].loc[13] * AGSCALPR
    #       Montana, Wyoming, and Idaho (Mountain 1)

    # T111(97:112,IY,IS)=PBMPWCL(3,1:16,IY)
    z[109] = dfd["PBMPWCL"].loc[4].loc[14] * AGSCALPR

    #       Colorado, Utah, and Nevada (Mountain 2)
    # T111(97:112,IY,IS)=PBMPWCL(3,1:16,IY)
    z[110] = dfd["PBMPWCL"].loc[4].loc[15] * AGSCALPR

    #       Arizona and New Mexico (Mountain 3)
    # T111(97:112,IY,IS)=PBMPWCL(3,1:16,IY)
    z[111] = dfd["PBMPWCL"].loc[4].loc[16] * AGSCALPR
    #       Pacific
    z[112] = dfd["PBMPWCL"].loc[4].loc[17] * AGSCALPR

    #     Electric Power Total, Energy Crops
    # T111(170,IY,IS)=(PBMPWCL(3,1,IY)*QBMPWCL(3,1,IY)+PBMPWCL(3,2,IY)*QBMPWCL(3,2,IY)+PBMPWCL(3,3,IY)*QBMPWCL(3,3,IY)+PBMPWCL(3,4,IY)*QBMPWCL(3,4,IY)+PBMPWCL(3,5,IY)*QBMPWCL(3,5,IY)+PBMPWCL(3,6,IY)*QBMPWCL(3,6,IY)+PBMPWCL(3,7,IY)*QBMPWCL(3,7,IY)+PBMPWCL(3,8,IY)*QBMPWCL(3,8,IY)+PBMPWCL(3,9,IY)*QBMPWCL(3,9,IY)+PBMPWCL(3,10,IY)*QBMPWCL(3,10,IY)+PBMPWCL(3,11,IY)*QBMPWCL(3,11,IY)+PBMPWCL(3,12,IY)*QBMPWCL(3,12,IY)+PBMPWCL(3,13,IY)*QBMPWCL(3,13,IY)+PBMPWCL(3,14,IY)*QBMPWCL(3,14,IY)+PBMPWCL(3,15,IY)*QBMPWCL(3,15,IY)+PBMPWCL(3,16,IY)*QBMPWCL(3,16,IY))/SUM(QBMPWCL(3,1:16,IY))
    # z[170] = (dfd['PBMPWCL'].loc[3].loc[1]*dfd['QBMPWCL'].loc[3].loc[1]+dfd['PBMPWCL'].loc[3].loc[2]*dfd['QBMPWCL'].loc[3].loc[2]+dfd['PBMPWCL'].loc[3].loc[3]*dfd['QBMPWCL'].loc[3].loc[3]+dfd['PBMPWCL'].loc[3].loc[4]*dfd['QBMPWCL'].loc[3].loc[4]+dfd['PBMPWCL'].loc[3].loc[5]*dfd['QBMPWCL'].loc[3].loc[5]+dfd['PBMPWCL'].loc[3].loc[6]*dfd['QBMPWCL'].loc[3].loc[6]+dfd['PBMPWCL'].loc[3].loc[7]*dfd['QBMPWCL'].loc[3].loc[7]+dfd['PBMPWCL'].loc[3].loc[8]*dfd['QBMPWCL'].loc[3].loc[8]+dfd['PBMPWCL'].loc[3].loc[9]*dfd['QBMPWCL'].loc[3].loc[9]+dfd['PBMPWCL'].loc[3].loc[10]*dfd['QBMPWCL'].loc[3].loc[10]+dfd['PBMPWCL'].loc[3].loc[11]*dfd['QBMPWCL'].loc[3].loc[11]+dfd['PBMPWCL'].loc[3].loc[12]*dfd['QBMPWCL'].loc[3].loc[12]+dfd['PBMPWCL'].loc[3].loc[13]*dfd['QBMPWCL'].loc[3].loc[13]+dfd['PBMPWCL'].loc[3].loc[14]*dfd['QBMPWCL'].loc[3].loc[14]+dfd['PBMPWCL'].loc[3].loc[15]*dfd['QBMPWCL'].loc[3].loc[15]+dfd['PBMPWCL'].loc[3].loc[16]*dfd['QBMPWCL'].loc[3].loc[16])/(dfd['QBMPWCL'].loc[(3].loc[1:16])
    z[170] = (
        (
            dfd["PBMPWCL"].loc[4].loc[2] * dfd["QBMPWCL"].loc[4].loc[2]
            + dfd["PBMPWCL"].loc[4].loc[3] * dfd["QBMPWCL"].loc[4].loc[3]
            + dfd["PBMPWCL"].loc[4].loc[4] * dfd["QBMPWCL"].loc[4].loc[4]
            + dfd["PBMPWCL"].loc[4].loc[5] * dfd["QBMPWCL"].loc[4].loc[5]
            + dfd["PBMPWCL"].loc[4].loc[6] * dfd["QBMPWCL"].loc[4].loc[6]
            + dfd["PBMPWCL"].loc[4].loc[7] * dfd["QBMPWCL"].loc[4].loc[7]
            + dfd["PBMPWCL"].loc[4].loc[8] * dfd["QBMPWCL"].loc[4].loc[8]
            + dfd["PBMPWCL"].loc[4].loc[9] * dfd["QBMPWCL"].loc[4].loc[9]
            + dfd["PBMPWCL"].loc[4].loc[10] * dfd["QBMPWCL"].loc[4].loc[10]
            + dfd["PBMPWCL"].loc[4].loc[11] * dfd["QBMPWCL"].loc[4].loc[11]
            + dfd["PBMPWCL"].loc[4].loc[12] * dfd["QBMPWCL"].loc[4].loc[12]
            + dfd["PBMPWCL"].loc[4].loc[13] * dfd["QBMPWCL"].loc[4].loc[13]
            + dfd["PBMPWCL"].loc[4].loc[14] * dfd["QBMPWCL"].loc[4].loc[14]
            + dfd["PBMPWCL"].loc[4].loc[15] * dfd["QBMPWCL"].loc[4].loc[15]
            + dfd["PBMPWCL"].loc[4].loc[16] * dfd["QBMPWCL"].loc[4].loc[16]
            + dfd["PBMPWCL"].loc[4].loc[17] * dfd["QBMPWCL"].loc[4].loc[17]
        )
        / dfd["QBMPWCL"].loc[4, 2:17, :].sum().replace(0, 1)
        * AGSCALPR
    ).fillna(0)
    #     Refining

    #       New England
    # T111(113:128,IY,IS) = (PBMETCL(3,1:16,IY) * QBMETCL(3,1:16,IY) + PBMBTCL(3,1:16,IY) * QBMBTCL(3,1:16,IY)) / (QBMETCL(3,1:16,IY) + QBMBTCL(3,1:16,IY))

    #       Middle Atlantic
    # T111(113:128,IY,IS) = (PBMETCL(3,1:16,IY) * QBMETCL(3,1:16,IY) + PBMBTCL(3,1:16,IY) * QBMBTCL(3,1:16,IY)) / (QBMETCL(3,1:16,IY) + QBMBTCL(3,1:16,IY))
    z[113] = (
        (
            dfd["PBMETCL"].loc[4].loc[2] * dfd["QBMETCL"].loc[4].loc[2]
            + dfd["PBMBTCL"].loc[4].loc[2] * dfd["QBMBTCL"].loc[4].loc[2]
        )
        / (dfd["QBMETCL"].loc[4].loc[2] + dfd["QBMBTCL"].loc[4].loc[2]).replace(0, 1)
    ) * SCALPR2

    #       South Atlantic 1
    # T111(113:128,IY,IS) = (PBMETCL(3,1:16,IY) * QBMETCL(3,1:16,IY) + PBMBTCL(3,1:16,IY) * QBMBTCL(3,1:16,IY)) / (QBMETCL(3,1:16,IY) + QBMBTCL(3,1:16,IY))
    z[114] = (
        (
            dfd["PBMETCL"].loc[4].loc[3] * dfd["QBMETCL"].loc[4].loc[3]
            + dfd["PBMBTCL"].loc[4].loc[3] * dfd["QBMBTCL"].loc[4].loc[3]
        )
        / (dfd["QBMETCL"].loc[4].loc[3] + dfd["QBMBTCL"].loc[4].loc[3]).replace(0, 1)
    ) * SCALPR2

    #       Virginia, Carolinas (South Atlantic 2)
    # T111(113:128,IY,IS) = (PBMETCL(3,1:16,IY) * QBMETCL(3,1:16,IY) + PBMBTCL(3,1:16,IY) * QBMBTCL(3,1:16,IY)) / (QBMETCL(3,1:16,IY) + QBMBTCL(3,1:16,IY))
    z[115] = (
        (
            dfd["PBMETCL"].loc[4].loc[4] * dfd["QBMETCL"].loc[4].loc[4]
            + dfd["PBMBTCL"].loc[4].loc[4] * dfd["QBMBTCL"].loc[4].loc[4]
        )
        / (dfd["QBMETCL"].loc[4].loc[4] + dfd["QBMBTCL"].loc[4].loc[4]).replace(0, 1)
    ) * SCALPR2

    #       Georgia and Florida (South Atlantic 3)
    # T111(113:128,IY,IS) = (PBMETCL(3,1:16,IY) * QBMETCL(3,1:16,IY) + PBMBTCL(3,1:16,IY) * QBMBTCL(3,1:16,IY)) / (QBMETCL(3,1:16,IY) + QBMBTCL(3,1:16,IY))
    z[116] = (
        (
            dfd["PBMETCL"].loc[4].loc[5] * dfd["QBMETCL"].loc[4].loc[5]
            + dfd["PBMBTCL"].loc[4].loc[5] * dfd["QBMBTCL"].loc[4].loc[5]
        )
        / (dfd["QBMETCL"].loc[4].loc[5] + dfd["QBMBTCL"].loc[4].loc[5]).replace(0, 1)
    ) * SCALPR2

    #       Ohio (East North Central 1)
    # T111(113:128,IY,IS) = (PBMETCL(3,1:16,IY) * QBMETCL(3,1:16,IY) + PBMBTCL(3,1:16,IY) * QBMBTCL(3,1:16,IY)) / (QBMETCL(3,1:16,IY) + QBMBTCL(3,1:16,IY))
    z[117] = (
        (
            dfd["PBMETCL"].loc[4].loc[6] * dfd["QBMETCL"].loc[4].loc[6]
            + dfd["PBMBTCL"].loc[4].loc[6] * dfd["QBMBTCL"].loc[4].loc[6]
        )
        / (dfd["QBMETCL"].loc[4].loc[6] + dfd["QBMBTCL"].loc[4].loc[6]).replace(0, 1)
    ) * SCALPR2

    #       East North Central 2
    # T111(113:128,IY,IS) = (PBMETCL(3,1:16,IY) * QBMETCL(3,1:16,IY) + PBMBTCL(3,1:16,IY) * QBMBTCL(3,1:16,IY)) / (QBMETCL(3,1:16,IY) + QBMBTCL(3,1:16,IY))
    z[118] = (
        (
            dfd["PBMETCL"].loc[4].loc[7] * dfd["QBMETCL"].loc[4].loc[7]
            + dfd["PBMBTCL"].loc[4].loc[7] * dfd["QBMBTCL"].loc[4].loc[7]
        )
        / (dfd["QBMETCL"].loc[4].loc[7] + dfd["QBMBTCL"].loc[4].loc[7]).replace(0, 1)
    ) * SCALPR2

    #       Kentucky and Tennessee (East South Cent 1)
    # T111(113:128,IY,IS) = (PBMETCL(3,1:16,IY) * QBMETCL(3,1:16,IY) + PBMBTCL(3,1:16,IY) * QBMBTCL(3,1:16,IY)) / (QBMETCL(3,1:16,IY) + QBMBTCL(3,1:16,IY))
    z[119] = (
        (
            dfd["PBMETCL"].loc[4].loc[8] * dfd["QBMETCL"].loc[4].loc[8]
            + dfd["PBMBTCL"].loc[4].loc[8] * dfd["QBMBTCL"].loc[4].loc[8]
        )
        / (dfd["QBMETCL"].loc[4].loc[8] + dfd["QBMBTCL"].loc[4].loc[8]).replace(0, 1)
    ) * SCALPR2

    #       Alabama and Mississippi (East South Cent 2)
    # T111(113:128,IY,IS) = (PBMETCL(3,1:16,IY) * QBMETCL(3,1:16,IY) + PBMBTCL(3,1:16,IY) * QBMBTCL(3,1:16,IY)) / (QBMETCL(3,1:16,IY) + QBMBTCL(3,1:16,IY))
    z[120] = (
        (
            dfd["PBMETCL"].loc[4].loc[9] * dfd["QBMETCL"].loc[4].loc[9]
            + dfd["PBMBTCL"].loc[4].loc[9] * dfd["QBMBTCL"].loc[4].loc[9]
        )
        / (dfd["QBMETCL"].loc[4].loc[9] + dfd["QBMBTCL"].loc[4].loc[9]).replace(0, 1)
    ) * SCALPR2

    #       Minnesota, Dakotas (West North Central 1)
    # T111(113:128,IY,IS) = (PBMETCL(3,1:16,IY) * QBMETCL(3,1:16,IY) + PBMBTCL(3,1:16,IY) * QBMBTCL(3,1:16,IY)) / (QBMETCL(3,1:16,IY) + QBMBTCL(3,1:16,IY))
    z[121] = (
        (
            dfd["PBMETCL"].loc[4].loc[10] * dfd["QBMETCL"].loc[4].loc[10]
            + dfd["PBMBTCL"].loc[4].loc[10] * dfd["QBMBTCL"].loc[4].loc[10]
        )
        / (dfd["QBMETCL"].loc[4].loc[10] + dfd["QBMBTCL"].loc[4].loc[10]).replace(0, 1)
    ) * SCALPR2

    #       West North Central 2
    # T111(113:128,IY,IS) = (PBMETCL(3,1:16,IY) * QBMETCL(3,1:16,IY) + PBMBTCL(3,1:16,IY) * QBMBTCL(3,1:16,IY)) / (QBMETCL(3,1:16,IY) + QBMBTCL(3,1:16,IY))
    z[122] = (
        (
            dfd["PBMETCL"].loc[4].loc[11] * dfd["QBMETCL"].loc[4].loc[11]
            + dfd["PBMBTCL"].loc[4].loc[11] * dfd["QBMBTCL"].loc[4].loc[11]
        )
        / (dfd["QBMETCL"].loc[4].loc[11] + dfd["QBMBTCL"].loc[4].loc[11]).replace(0, 1)
    ) * SCALPR2

    #       West South Central
    # T111(113:128,IY,IS) = (PBMETCL(3,1:16,IY) * QBMETCL(3,1:16,IY) + PBMBTCL(3,1:16,IY) * QBMBTCL(3,1:16,IY)) / (QBMETCL(3,1:16,IY) + QBMBTCL(3,1:16,IY))
    z[123] = (
        (
            dfd["PBMETCL"].loc[4].loc[12] * dfd["QBMETCL"].loc[4].loc[12]
            + dfd["PBMBTCL"].loc[4].loc[12] * dfd["QBMBTCL"].loc[4].loc[12]
        )
        / (dfd["QBMETCL"].loc[4].loc[12] + dfd["QBMBTCL"].loc[4].loc[12]).replace(0, 1)
    ) * SCALPR2

    #       Montana, Wyoming, and Idaho (Mountain 1)
    # T111(113:128,IY,IS) = (PBMETCL(3,1:16,IY) * QBMETCL(3,1:16,IY) + PBMBTCL(3,1:16,IY) * QBMBTCL(3,1:16,IY)) / (QBMETCL(3,1:16,IY) + QBMBTCL(3,1:16,IY))
    z[124] = (
        (
            dfd["PBMETCL"].loc[4].loc[13] * dfd["QBMETCL"].loc[4].loc[13]
            + dfd["PBMBTCL"].loc[4].loc[13] * dfd["QBMBTCL"].loc[4].loc[13]
        )
        / (dfd["QBMETCL"].loc[4].loc[13] + dfd["QBMBTCL"].loc[4].loc[13]).replace(0, 1)
    ) * SCALPR2

    #       Colorado, Utah, and Nevada (Mountain 2)
    # T111(113:128,IY,IS) = (PBMETCL(3,1:16,IY) * QBMETCL(3,1:16,IY) + PBMBTCL(3,1:16,IY) * QBMBTCL(3,1:16,IY)) / (QBMETCL(3,1:16,IY) + QBMBTCL(3,1:16,IY))
    z[125] = (
        (
            dfd["PBMETCL"].loc[4].loc[14] * dfd["QBMETCL"].loc[4].loc[14]
            + dfd["PBMBTCL"].loc[4].loc[14] * dfd["QBMBTCL"].loc[4].loc[14]
        )
        / (dfd["QBMETCL"].loc[4].loc[14] + dfd["QBMBTCL"].loc[4].loc[14]).replace(0, 1)
    ) * SCALPR2

    #       Arizona and New Mexico (Mountain 3)
    # T111(113:128,IY,IS) = (PBMETCL(3,1:16,IY) * QBMETCL(3,1:16,IY) + PBMBTCL(3,1:16,IY) * QBMBTCL(3,1:16,IY)) / (QBMETCL(3,1:16,IY) + QBMBTCL(3,1:16,IY))
    z[126] = (
        (
            dfd["PBMETCL"].loc[4].loc[15] * dfd["QBMETCL"].loc[4].loc[15]
            + dfd["PBMBTCL"].loc[4].loc[15] * dfd["QBMBTCL"].loc[4].loc[15]
        )
        / (dfd["QBMETCL"].loc[4].loc[15] + dfd["QBMBTCL"].loc[4].loc[15]).replace(0, 1)
    ) * SCALPR2

    #       Pacific
    # T111(113:128,IY,IS) = (PBMETCL(3,1:16,IY) * QBMETCL(3,1:16,IY) + PBMBTCL(3,1:16,IY) * QBMBTCL(3,1:16,IY)) / (QBMETCL(3,1:16,IY) + QBMBTCL(3,1:16,IY))
    z[127] = (
        (
            dfd["PBMETCL"].loc[4].loc[16] * dfd["QBMETCL"].loc[4].loc[16]
            + dfd["PBMBTCL"].loc[4].loc[16] * dfd["QBMBTCL"].loc[4].loc[16]
        )
        / (dfd["QBMETCL"].loc[4].loc[16] + dfd["QBMBTCL"].loc[4].loc[16]).replace(0, 1)
    ) * SCALPR2
    z[128] = (
        (
            dfd["PBMETCL"].loc[4].loc[17] * dfd["QBMETCL"].loc[4].loc[17]
            + dfd["PBMBTCL"].loc[4].loc[17] * dfd["QBMBTCL"].loc[4].loc[17]
        )
        / (dfd["QBMETCL"].loc[4].loc[17] + dfd["QBMBTCL"].loc[4].loc[17]).replace(0, 1)
    ) * SCALPR2

    #     Refining Total, Energy Crops
    # T111(171,IY,IS)=T111(171,IY,IS)+(PBMETCL(3,IR,IY)*QBMETCL(3,IR,IY)+PBMBTCL(3,IR,IY)*QBMBTCL(3,IR,IY))/(SUM(QBMETCL(3,1:16,IY))+SUM(QBMBTCL(3,1:16,IY)))
    z[171] = (
        (
            dfd["PBMETCL"].loc[4].loc[2] * dfd["QBMETCL"].loc[4].loc[2]
            + dfd["PBMETCL"].loc[4].loc[3] * dfd["QBMETCL"].loc[4].loc[3]
            + dfd["PBMETCL"].loc[4].loc[4] * dfd["QBMETCL"].loc[4].loc[4]
            + dfd["PBMETCL"].loc[4].loc[5] * dfd["QBMETCL"].loc[4].loc[5]
            + dfd["PBMETCL"].loc[4].loc[6] * dfd["QBMETCL"].loc[4].loc[6]
            + dfd["PBMETCL"].loc[4].loc[7] * dfd["QBMETCL"].loc[4].loc[7]
            + dfd["PBMETCL"].loc[4].loc[8] * dfd["QBMETCL"].loc[4].loc[8]
            + dfd["PBMETCL"].loc[4].loc[9] * dfd["QBMETCL"].loc[4].loc[9]
            + dfd["PBMETCL"].loc[4].loc[10] * dfd["QBMETCL"].loc[4].loc[10]
            + dfd["PBMETCL"].loc[4].loc[11] * dfd["QBMETCL"].loc[4].loc[11]
            + dfd["PBMETCL"].loc[4].loc[12] * dfd["QBMETCL"].loc[4].loc[12]
            + dfd["PBMETCL"].loc[4].loc[13] * dfd["QBMETCL"].loc[4].loc[13]
            + dfd["PBMETCL"].loc[4].loc[14] * dfd["QBMETCL"].loc[4].loc[14]
            + dfd["PBMETCL"].loc[4].loc[15] * dfd["QBMETCL"].loc[4].loc[15]
            + dfd["PBMETCL"].loc[4].loc[16] * dfd["QBMETCL"].loc[4].loc[16]
            + dfd["PBMETCL"].loc[4].loc[17] * dfd["QBMETCL"].loc[4].loc[17]
            + dfd["PBMBTCL"].loc[4].loc[2] * dfd["QBMBTCL"].loc[4].loc[2]
            + dfd["PBMBTCL"].loc[4].loc[3] * dfd["QBMBTCL"].loc[4].loc[3]
            + dfd["PBMBTCL"].loc[4].loc[4] * dfd["QBMBTCL"].loc[4].loc[4]
            + dfd["PBMBTCL"].loc[4].loc[5] * dfd["QBMBTCL"].loc[4].loc[5]
            + dfd["PBMBTCL"].loc[4].loc[6] * dfd["QBMBTCL"].loc[4].loc[6]
            + dfd["PBMBTCL"].loc[4].loc[7] * dfd["QBMBTCL"].loc[4].loc[7]
            + dfd["PBMBTCL"].loc[4].loc[8] * dfd["QBMBTCL"].loc[4].loc[8]
            + dfd["PBMBTCL"].loc[4].loc[9] * dfd["QBMBTCL"].loc[4].loc[9]
            + dfd["PBMBTCL"].loc[4].loc[10] * dfd["QBMBTCL"].loc[4].loc[10]
            + dfd["PBMBTCL"].loc[4].loc[11] * dfd["QBMBTCL"].loc[4].loc[11]
            + dfd["PBMBTCL"].loc[4].loc[12] * dfd["QBMBTCL"].loc[4].loc[12]
            + dfd["PBMBTCL"].loc[4].loc[13] * dfd["QBMBTCL"].loc[4].loc[13]
            + dfd["PBMBTCL"].loc[4].loc[14] * dfd["QBMBTCL"].loc[4].loc[14]
            + dfd["PBMBTCL"].loc[4].loc[15] * dfd["QBMBTCL"].loc[4].loc[15]
            + dfd["PBMBTCL"].loc[4].loc[16] * dfd["QBMBTCL"].loc[4].loc[16]
            + dfd["PBMBTCL"].loc[4].loc[17] * dfd["QBMBTCL"].loc[4].loc[17]
        )
        / (
            dfd["QBMETCL"].loc[4, 2:17, :].sum() + dfd["QBMBTCL"].loc[4, 2:17, :].sum()
        ).replace(0, 1)
        * SCALPR2
    ).fillna(0)
    #   Total, Agriculture and Energy Crops
    # T111(172,IY,IS)=(T111(170,IY,IS)*SUM(QBMPWCL(3,1:16,IY))+T111(171,IY,IS)*(SUM(QBMETCL(3,1:16,IY))+SUM(QBMBTCL(3,1:16,IY))))/(SUM(QBMPWCL(3,1:16,IY))+SUM(QBMETCL(3,1:16,IY))+SUM(QBMBTCL(3,1:16,IY)))
    z[172] = (
        z[170] * dfd["QBMPWCL"].loc[4, 2:17, :].sum()
        + z[171]
        * (dfd["QBMETCL"].loc[4, 2:17, :].sum() + dfd["QBMBTCL"].loc[4, 2:17, :].sum())
    ) / (
        dfd["QBMPWCL"].loc[4, 2:17, :].sum()
        + dfd["QBMETCL"].loc[4, 2:17, :].sum()
        + dfd["QBMBTCL"].loc[4, 2:17, :].sum()
    ).replace(
        0, 1
    )

    #   Forestry Residue, Private Lands

    #     Electric Power

    #       New England
    # T111(129:144,IY,IS)=PBMPWCL(4,1:16,IY)

    #       Middle Atlantic
    # T111(129:144,IY,IS)=PBMPWCL(4,1:16,IY)
    z[129] = dfd["PBMPWCL"].loc[5].loc[2] * SCALPR2

    #       South Atlantic 1
    # T111(129:144,IY,IS)=PBMPWCL(4,1:16,IY)
    z[130] = dfd["PBMPWCL"].loc[5].loc[3] * SCALPR2

    #       Virginia, Carolinas (South Atlantic 2)
    # T111(129:144,IY,IS)=PBMPWCL(4,1:16,IY)
    z[131] = dfd["PBMPWCL"].loc[5].loc[4] * SCALPR2

    #       Georgia and Florida (South Atlantic 3)
    # T111(129:144,IY,IS)=PBMPWCL(4,1:16,IY)
    z[132] = dfd["PBMPWCL"].loc[5].loc[5] * SCALPR2

    #       Ohio (East North Central 1)
    # T111(129:144,IY,IS)=PBMPWCL(4,1:16,IY)
    z[133] = dfd["PBMPWCL"].loc[5].loc[6] * SCALPR2

    #       East North Central 2
    # T111(129:144,IY,IS)=PBMPWCL(4,1:16,IY)
    z[134] = dfd["PBMPWCL"].loc[5].loc[7] * SCALPR2

    #       Kentucky and Tennessee (East South Cent 1)
    # T111(129:144,IY,IS)=PBMPWCL(4,1:16,IY)
    z[135] = dfd["PBMPWCL"].loc[5].loc[8] * SCALPR2

    #       Alabama and Mississippi (East South Cent 2)
    # T111(129:144,IY,IS)=PBMPWCL(4,1:16,IY)
    z[136] = dfd["PBMPWCL"].loc[5].loc[9] * SCALPR2

    #       Minnesota, Dakotas (West North Central 1)
    # T111(129:144,IY,IS)=PBMPWCL(4,1:16,IY)
    z[137] = dfd["PBMPWCL"].loc[5].loc[10] * SCALPR2

    #       West North Central 2
    # T111(129:144,IY,IS)=PBMPWCL(4,1:16,IY)
    z[138] = dfd["PBMPWCL"].loc[5].loc[11] * SCALPR2

    #       West South Central
    # T111(129:144,IY,IS)=PBMPWCL(4,1:16,IY)
    z[139] = dfd["PBMPWCL"].loc[5].loc[12] * SCALPR2

    #       Montana, Wyoming, and Idaho (Mountain 1)
    # T111(129:144,IY,IS)=PBMPWCL(4,1:16,IY)
    z[140] = dfd["PBMPWCL"].loc[5].loc[13] * SCALPR2

    #       Colorado, Utah, and Nevada (Mountain 2)
    # T111(129:144,IY,IS)=PBMPWCL(4,1:16,IY)
    z[141] = dfd["PBMPWCL"].loc[5].loc[14] * SCALPR2

    #       Arizona and New Mexico (Mountain 3)
    # T111(129:144,IY,IS)=PBMPWCL(4,1:16,IY)
    z[142] = dfd["PBMPWCL"].loc[5].loc[15] * SCALPR2

    #       Pacific
    # T111(129:144,IY,IS)=PBMPWCL(4,1:16,IY)
    z[143] = dfd["PBMPWCL"].loc[5].loc[16] * SCALPR2
    z[144] = dfd["PBMPWCL"].loc[5].loc[17] * SCALPR2

    #     Electric Power Total, Private Forestry Residue
    # T111(173,IY,IS)=(PBMPWCL(4,1,IY)*QBMPWCL(4,1,IY)+PBMPWCL(4,2,IY)*QBMPWCL(4,2,IY)+PBMPWCL(4,3,IY)*QBMPWCL(4,3,IY)+PBMPWCL(4,4,IY)*QBMPWCL(4,4,IY)+PBMPWCL(4,5,IY)*QBMPWCL(4,5,IY)+PBMPWCL(4,6,IY)*QBMPWCL(4,6,IY)+PBMPWCL(4,7,IY)*QBMPWCL(4,7,IY)+PBMPWCL(4,8,IY)*QBMPWCL(4,8,IY)+PBMPWCL(4,9,IY)*QBMPWCL(4,9,IY)+PBMPWCL(4,10,IY)*QBMPWCL(4,10,IY)+PBMPWCL(4,11,IY)*QBMPWCL(4,11,IY)+PBMPWCL(4,12,IY)*QBMPWCL(4,12,IY)+PBMPWCL(4,13,IY)*QBMPWCL(4,13,IY)+PBMPWCL(4,14,IY)*QBMPWCL(4,14,IY)+PBMPWCL(4,15,IY)*QBMPWCL(4,15,IY)+PBMPWCL(4,16,IY)*QBMPWCL(4,16,IY))/SUM(QBMPWCL(4,1:16,IY))
    # z[173] = (dfd['PBMPWCL'].loc[4].loc[1]*dfd['QBMPWCL'].loc[4].loc[1]+dfd['PBMPWCL'].loc[4].loc[2]*dfd['QBMPWCL'].loc[4].loc[2]+dfd['PBMPWCL'].loc[4].loc[3]*dfd['QBMPWCL'].loc[4].loc[3]+dfd['PBMPWCL'].loc[4].loc[4]*dfd['QBMPWCL'].loc[4].loc[4]+dfd['PBMPWCL'].loc[4].loc[5]*dfd['QBMPWCL'].loc[4].loc[5]+dfd['PBMPWCL'].loc[4].loc[6]*dfd['QBMPWCL'].loc[4].loc[6]+dfd['PBMPWCL'].loc[4].loc[7]*dfd['QBMPWCL'].loc[4].loc[7]+dfd['PBMPWCL'].loc[4].loc[8]*dfd['QBMPWCL'].loc[4].loc[8]+dfd['PBMPWCL'].loc[4].loc[9]*dfd['QBMPWCL'].loc[4].loc[9]+dfd['PBMPWCL'].loc[4].loc[10]*dfd['QBMPWCL'].loc[4].loc[10]+dfd['PBMPWCL'].loc[4].loc[11]*dfd['QBMPWCL'].loc[4].loc[11]+dfd['PBMPWCL'].loc[4].loc[12]*dfd['QBMPWCL'].loc[4].loc[12]+dfd['PBMPWCL'].loc[4].loc[13]*dfd['QBMPWCL'].loc[4].loc[13]+dfd['PBMPWCL'].loc[4].loc[14]*dfd['QBMPWCL'].loc[4].loc[14]+dfd['PBMPWCL'].loc[4].loc[15]*dfd['QBMPWCL'].loc[4].loc[15]+dfd['PBMPWCL'].loc[4].loc[16]*dfd['QBMPWCL'].loc[4].loc[16])/(dfd['QBMPWCL'].loc[(4].loc[1:16])
    z[173] = (
        (
            dfd["PBMPWCL"].loc[5].loc[2] * dfd["QBMPWCL"].loc[5].loc[2]
            + dfd["PBMPWCL"].loc[5].loc[3] * dfd["QBMPWCL"].loc[5].loc[3]
            + dfd["PBMPWCL"].loc[5].loc[4] * dfd["QBMPWCL"].loc[5].loc[4]
            + dfd["PBMPWCL"].loc[5].loc[5] * dfd["QBMPWCL"].loc[5].loc[5]
            + dfd["PBMPWCL"].loc[5].loc[6] * dfd["QBMPWCL"].loc[5].loc[6]
            + dfd["PBMPWCL"].loc[5].loc[7] * dfd["QBMPWCL"].loc[5].loc[7]
            + dfd["PBMPWCL"].loc[5].loc[8] * dfd["QBMPWCL"].loc[5].loc[8]
            + dfd["PBMPWCL"].loc[5].loc[9] * dfd["QBMPWCL"].loc[5].loc[9]
            + dfd["PBMPWCL"].loc[5].loc[10] * dfd["QBMPWCL"].loc[5].loc[10]
            + dfd["PBMPWCL"].loc[5].loc[11] * dfd["QBMPWCL"].loc[5].loc[11]
            + dfd["PBMPWCL"].loc[5].loc[12] * dfd["QBMPWCL"].loc[5].loc[12]
            + dfd["PBMPWCL"].loc[5].loc[13] * dfd["QBMPWCL"].loc[5].loc[13]
            + dfd["PBMPWCL"].loc[5].loc[14] * dfd["QBMPWCL"].loc[5].loc[14]
            + dfd["PBMPWCL"].loc[5].loc[15] * dfd["QBMPWCL"].loc[5].loc[15]
            + dfd["PBMPWCL"].loc[5].loc[16] * dfd["QBMPWCL"].loc[5].loc[16]
            + dfd["PBMPWCL"].loc[5].loc[17] * dfd["QBMPWCL"].loc[5].loc[17]
        )
        / dfd["QBMPWCL"].loc[5, 2:17, :].sum().replace(0, 1)
        * SCALPR2
    ).fillna(0)
    #     Refining

    #       New England

    #       Middle Atlantic
    # T111(145:160,IY,IS) = (PBMETCL(4,1:16,IY) * QBMETCL(4,1:16,IY) + PBMBTCL(4,1:16,IY) * QBMBTCL(4,1:16,IY)) / (QBMETCL(4,1:16,IY) + QBMBTCL(4,1:16,IY))
    z[145] = (
        (
            dfd["PBMETCL"].loc[5].loc[2] * dfd["QBMETCL"].loc[5].loc[2]
            + dfd["PBMBTCL"].loc[5].loc[2] * dfd["QBMBTCL"].loc[5].loc[2]
        )
        / (dfd["QBMETCL"].loc[5].loc[2] + dfd["QBMBTCL"].loc[5].loc[2]).replace(0, 1)
    ) * SCALPR2

    #       South Atlantic 1
    # T111(145:160,IY,IS) = (PBMETCL(4,1:16,IY) * QBMETCL(4,1:16,IY) + PBMBTCL(4,1:16,IY) * QBMBTCL(4,1:16,IY)) / (QBMETCL(4,1:16,IY) + QBMBTCL(4,1:16,IY))
    z[146] = (
        (
            dfd["PBMETCL"].loc[5].loc[3] * dfd["QBMETCL"].loc[5].loc[3]
            + dfd["PBMBTCL"].loc[5].loc[3] * dfd["QBMBTCL"].loc[5].loc[3]
        )
        / (dfd["QBMETCL"].loc[5].loc[3] + dfd["QBMBTCL"].loc[5].loc[3]).replace(0, 1)
    ) * SCALPR2

    #       Virginia, Carolinas (South Atlantic 2)
    # T111(145:160,IY,IS) = (PBMETCL(4,1:16,IY) * QBMETCL(4,1:16,IY) + PBMBTCL(4,1:16,IY) * QBMBTCL(4,1:16,IY)) / (QBMETCL(4,1:16,IY) + QBMBTCL(4,1:16,IY))
    z[147] = (
        (
            dfd["PBMETCL"].loc[5].loc[4] * dfd["QBMETCL"].loc[5].loc[4]
            + dfd["PBMBTCL"].loc[5].loc[4] * dfd["QBMBTCL"].loc[5].loc[4]
        )
        / (dfd["QBMETCL"].loc[5].loc[4] + dfd["QBMBTCL"].loc[5].loc[4]).replace(0, 1)
    ) * SCALPR2

    #       Georgia and Florida (South Atlantic 3)
    # T111(145:160,IY,IS) = (PBMETCL(4,1:16,IY) * QBMETCL(4,1:16,IY) + PBMBTCL(4,1:16,IY) * QBMBTCL(4,1:16,IY)) / (QBMETCL(4,1:16,IY) + QBMBTCL(4,1:16,IY))
    z[148] = (
        (
            dfd["PBMETCL"].loc[5].loc[5] * dfd["QBMETCL"].loc[5].loc[5]
            + dfd["PBMBTCL"].loc[5].loc[5] * dfd["QBMBTCL"].loc[5].loc[5]
        )
        / (dfd["QBMETCL"].loc[5].loc[5] + dfd["QBMBTCL"].loc[5].loc[5]).replace(0, 1)
    ) * SCALPR2

    #       Ohio (East North Central 1)
    # T111(145:160,IY,IS) = (PBMETCL(4,1:16,IY) * QBMETCL(4,1:16,IY) + PBMBTCL(4,1:16,IY) * QBMBTCL(4,1:16,IY)) / (QBMETCL(4,1:16,IY) + QBMBTCL(4,1:16,IY))
    z[149] = (
        (
            dfd["PBMETCL"].loc[5].loc[6] * dfd["QBMETCL"].loc[5].loc[6]
            + dfd["PBMBTCL"].loc[5].loc[6] * dfd["QBMBTCL"].loc[5].loc[6]
        )
        / (dfd["QBMETCL"].loc[5].loc[6] + dfd["QBMBTCL"].loc[5].loc[6]).replace(0, 1)
    ) * SCALPR2

    #       East North Central 2
    # T111(145:160,IY,IS) = (PBMETCL(4,1:16,IY) * QBMETCL(4,1:16,IY) + PBMBTCL(4,1:16,IY) * QBMBTCL(4,1:16,IY)) / (QBMETCL(4,1:16,IY) + QBMBTCL(4,1:16,IY))
    z[150] = (
        (
            dfd["PBMETCL"].loc[5].loc[7] * dfd["QBMETCL"].loc[5].loc[7]
            + dfd["PBMBTCL"].loc[5].loc[7] * dfd["QBMBTCL"].loc[5].loc[7]
        )
        / (dfd["QBMETCL"].loc[5].loc[7] + dfd["QBMBTCL"].loc[5].loc[7]).replace(0, 1)
    ) * SCALPR2

    #       Kentucky and Tennessee (East South Cent 1)
    # T111(145:160,IY,IS) = (PBMETCL(4,1:16,IY) * QBMETCL(4,1:16,IY) + PBMBTCL(4,1:16,IY) * QBMBTCL(4,1:16,IY)) / (QBMETCL(4,1:16,IY) + QBMBTCL(4,1:16,IY))
    z[151] = (
        (
            dfd["PBMETCL"].loc[5].loc[8] * dfd["QBMETCL"].loc[5].loc[8]
            + dfd["PBMBTCL"].loc[5].loc[8] * dfd["QBMBTCL"].loc[5].loc[8]
        )
        / (dfd["QBMETCL"].loc[5].loc[8] + dfd["QBMBTCL"].loc[5].loc[8]).replace(0, 1)
    ) * SCALPR2

    #       Alabama and Mississippi (East South Cent 2)
    # T111(145:160,IY,IS) = (PBMETCL(4,1:16,IY) * QBMETCL(4,1:16,IY) + PBMBTCL(4,1:16,IY) * QBMBTCL(4,1:16,IY)) / (QBMETCL(4,1:16,IY) + QBMBTCL(4,1:16,IY))
    z[152] = (
        (
            dfd["PBMETCL"].loc[5].loc[9] * dfd["QBMETCL"].loc[5].loc[9]
            + dfd["PBMBTCL"].loc[5].loc[9] * dfd["QBMBTCL"].loc[5].loc[9]
        )
        / (dfd["QBMETCL"].loc[5].loc[9] + dfd["QBMBTCL"].loc[5].loc[9]).replace(0, 1)
    ) * SCALPR2

    #       Minnesota, Dakotas (West North Central 1)
    # T111(145:160,IY,IS) = (PBMETCL(4,1:16,IY) * QBMETCL(4,1:16,IY) + PBMBTCL(4,1:16,IY) * QBMBTCL(4,1:16,IY)) / (QBMETCL(4,1:16,IY) + QBMBTCL(4,1:16,IY))
    z[153] = (
        (
            dfd["PBMETCL"].loc[5].loc[10] * dfd["QBMETCL"].loc[5].loc[10]
            + dfd["PBMBTCL"].loc[5].loc[10] * dfd["QBMBTCL"].loc[5].loc[10]
        )
        / (dfd["QBMETCL"].loc[5].loc[10] + dfd["QBMBTCL"].loc[5].loc[10]).replace(0, 1)
    ) * SCALPR2

    #       West North Central 2
    # T111(145:160,IY,IS) = (PBMETCL(4,1:16,IY) * QBMETCL(4,1:16,IY) + PBMBTCL(4,1:16,IY) * QBMBTCL(4,1:16,IY)) / (QBMETCL(4,1:16,IY) + QBMBTCL(4,1:16,IY))
    z[154] = (
        (
            dfd["PBMETCL"].loc[5].loc[11] * dfd["QBMETCL"].loc[5].loc[11]
            + dfd["PBMBTCL"].loc[5].loc[11] * dfd["QBMBTCL"].loc[5].loc[11]
        )
        / (dfd["QBMETCL"].loc[5].loc[11] + dfd["QBMBTCL"].loc[5].loc[11]).replace(0, 1)
    ) * SCALPR2

    #       West South Central
    # T111(145:160,IY,IS) = (PBMETCL(4,1:16,IY) * QBMETCL(4,1:16,IY) + PBMBTCL(4,1:16,IY) * QBMBTCL(4,1:16,IY)) / (QBMETCL(4,1:16,IY) + QBMBTCL(4,1:16,IY))
    z[155] = (
        (
            dfd["PBMETCL"].loc[5].loc[12] * dfd["QBMETCL"].loc[5].loc[12]
            + dfd["PBMBTCL"].loc[5].loc[12] * dfd["QBMBTCL"].loc[5].loc[12]
        )
        / (dfd["QBMETCL"].loc[5].loc[12] + dfd["QBMBTCL"].loc[5].loc[12]).replace(0, 1)
    ) * SCALPR2

    #       Montana, Wyoming, and Idaho (Mountain 1)
    # T111(145:160,IY,IS) = (PBMETCL(4,1:16,IY) * QBMETCL(4,1:16,IY) + PBMBTCL(4,1:16,IY) * QBMBTCL(4,1:16,IY)) / (QBMETCL(4,1:16,IY) + QBMBTCL(4,1:16,IY))
    z[156] = (
        (
            dfd["PBMETCL"].loc[5].loc[13] * dfd["QBMETCL"].loc[5].loc[13]
            + dfd["PBMBTCL"].loc[5].loc[13] * dfd["QBMBTCL"].loc[5].loc[13]
        )
        / (dfd["QBMETCL"].loc[5].loc[13] + dfd["QBMBTCL"].loc[5].loc[13]).replace(0, 1)
    ) * SCALPR2

    #       Colorado, Utah, and Nevada (Mountain 2)
    # T111(145:160,IY,IS) = (PBMETCL(4,1:16,IY) * QBMETCL(4,1:16,IY) + PBMBTCL(4,1:16,IY) * QBMBTCL(4,1:16,IY)) / (QBMETCL(4,1:16,IY) + QBMBTCL(4,1:16,IY))
    z[157] = (
        (
            dfd["PBMETCL"].loc[5].loc[14] * dfd["QBMETCL"].loc[5].loc[14]
            + dfd["PBMBTCL"].loc[5].loc[14] * dfd["QBMBTCL"].loc[5].loc[14]
        )
        / (dfd["QBMETCL"].loc[5].loc[14] + dfd["QBMBTCL"].loc[5].loc[14]).replace(0, 1)
    ) * SCALPR2

    #       Arizona and New Mexico (Mountain 3)
    # T111(145:160,IY,IS) = (PBMETCL(4,1:16,IY) * QBMETCL(4,1:16,IY) + PBMBTCL(4,1:16,IY) * QBMBTCL(4,1:16,IY)) / (QBMETCL(4,1:16,IY) + QBMBTCL(4,1:16,IY))
    z[158] = (
        (
            dfd["PBMETCL"].loc[5].loc[15] * dfd["QBMETCL"].loc[5].loc[15]
            + dfd["PBMBTCL"].loc[5].loc[15] * dfd["QBMBTCL"].loc[5].loc[15]
        )
        / (dfd["QBMETCL"].loc[5].loc[15] + dfd["QBMBTCL"].loc[5].loc[15]).replace(0, 1)
    ) * SCALPR2

    #       Pacific
    # T111(145:160,IY,IS) = (PBMETCL(4,1:16,IY) * QBMETCL(4,1:16,IY) + PBMBTCL(4,1:16,IY) * QBMBTCL(4,1:16,IY)) / (QBMETCL(4,1:16,IY) + QBMBTCL(4,1:16,IY))
    z[159] = (
        (
            dfd["PBMETCL"].loc[5].loc[16] * dfd["QBMETCL"].loc[5].loc[16]
            + dfd["PBMBTCL"].loc[5].loc[16] * dfd["QBMBTCL"].loc[5].loc[16]
        )
        / (dfd["QBMETCL"].loc[5].loc[16] + dfd["QBMBTCL"].loc[5].loc[16]).replace(0, 1)
    ) * SCALPR2
    z[160] = (
        (
            dfd["PBMETCL"].loc[5].loc[17] * dfd["QBMETCL"].loc[5].loc[17]
            + dfd["PBMBTCL"].loc[5].loc[17] * dfd["QBMBTCL"].loc[5].loc[17]
        )
        / (dfd["QBMETCL"].loc[5].loc[17] + dfd["QBMBTCL"].loc[5].loc[17]).replace(0, 1)
    ) * SCALPR2

    #     Refining Total, Private Forestry Residue
    # T111(174,IY,IS)=T111(174,IY,IS)+(PBMETCL(4,IR,IY)*QBMETCL(4,IR,IY)+PBMBTCL(4,IR,IY)*QBMBTCL(4,IR,IY))/(SUM(QBMETCL(4,1:16,IY))+SUM(QBMBTCL(4,1:16,IY)))
    # z[174] = z[174]+(dfd['PBMETCL'].loc[4]*dfd['QBMETCL'].loc[4]+dfd['PBMBTCL'].loc[4]*dfd['QBMBTCL'].loc[4])/(SUM(dfd['QBMETCL'].loc[4].loc[1:16])+SUM(dfd['QBMBTCL'].loc[4].loc[1:16]))
    z[174] = (
        (
            dfd["PBMETCL"].loc[5].loc[2] * dfd["QBMETCL"].loc[5].loc[2]
            + dfd["PBMETCL"].loc[5].loc[3] * dfd["QBMETCL"].loc[5].loc[3]
            + dfd["PBMETCL"].loc[5].loc[4] * dfd["QBMETCL"].loc[5].loc[4]
            + dfd["PBMETCL"].loc[5].loc[5] * dfd["QBMETCL"].loc[5].loc[5]
            + dfd["PBMETCL"].loc[5].loc[6] * dfd["QBMETCL"].loc[5].loc[6]
            + dfd["PBMETCL"].loc[5].loc[7] * dfd["QBMETCL"].loc[5].loc[7]
            + dfd["PBMETCL"].loc[5].loc[8] * dfd["QBMETCL"].loc[5].loc[8]
            + dfd["PBMETCL"].loc[5].loc[9] * dfd["QBMETCL"].loc[5].loc[9]
            + dfd["PBMETCL"].loc[5].loc[10] * dfd["QBMETCL"].loc[5].loc[10]
            + dfd["PBMETCL"].loc[5].loc[11] * dfd["QBMETCL"].loc[5].loc[11]
            + dfd["PBMETCL"].loc[5].loc[12] * dfd["QBMETCL"].loc[5].loc[12]
            + dfd["PBMETCL"].loc[5].loc[13] * dfd["QBMETCL"].loc[5].loc[13]
            + dfd["PBMETCL"].loc[5].loc[14] * dfd["QBMETCL"].loc[5].loc[14]
            + dfd["PBMETCL"].loc[5].loc[15] * dfd["QBMETCL"].loc[5].loc[15]
            + dfd["PBMETCL"].loc[5].loc[16] * dfd["QBMETCL"].loc[5].loc[16]
            + dfd["PBMETCL"].loc[5].loc[17] * dfd["QBMETCL"].loc[5].loc[17]
            + dfd["PBMBTCL"].loc[5].loc[2] * dfd["QBMBTCL"].loc[5].loc[2]
            + dfd["PBMBTCL"].loc[5].loc[3] * dfd["QBMBTCL"].loc[5].loc[3]
            + dfd["PBMBTCL"].loc[5].loc[4] * dfd["QBMBTCL"].loc[5].loc[4]
            + dfd["PBMBTCL"].loc[5].loc[5] * dfd["QBMBTCL"].loc[5].loc[5]
            + dfd["PBMBTCL"].loc[5].loc[6] * dfd["QBMBTCL"].loc[5].loc[6]
            + dfd["PBMBTCL"].loc[5].loc[7] * dfd["QBMBTCL"].loc[5].loc[7]
            + dfd["PBMBTCL"].loc[5].loc[8] * dfd["QBMBTCL"].loc[5].loc[8]
            + dfd["PBMBTCL"].loc[5].loc[9] * dfd["QBMBTCL"].loc[5].loc[9]
            + dfd["PBMBTCL"].loc[5].loc[10] * dfd["QBMBTCL"].loc[5].loc[10]
            + dfd["PBMBTCL"].loc[5].loc[11] * dfd["QBMBTCL"].loc[5].loc[11]
            + dfd["PBMBTCL"].loc[5].loc[12] * dfd["QBMBTCL"].loc[5].loc[12]
            + dfd["PBMBTCL"].loc[5].loc[13] * dfd["QBMBTCL"].loc[5].loc[13]
            + dfd["PBMBTCL"].loc[5].loc[14] * dfd["QBMBTCL"].loc[5].loc[14]
            + dfd["PBMBTCL"].loc[5].loc[15] * dfd["QBMBTCL"].loc[5].loc[15]
            + dfd["PBMBTCL"].loc[5].loc[16] * dfd["QBMBTCL"].loc[5].loc[16]
            + dfd["PBMBTCL"].loc[5].loc[17] * dfd["QBMBTCL"].loc[5].loc[17]
        )
        / (
            dfd["QBMETCL"].loc[5, 2:17, :].sum() + dfd["QBMBTCL"].loc[5, 2:17, :].sum()
        ).replace(0, 1)
        * SCALPR2
    ).fillna(0)
    #   Total, Forestry Residue, Private Lands
    # T111(175,IY,IS)=(T111(173,IY,IS)*SUM(QBMPWCL(4,1:16,IY))+T111(174,IY,IS)*(SUM(QBMETCL(4,1:16,IY))+SUM(QBMBTCL(4,1:16,IY))))/(SUM(QBMPWCL(4,1:16,IY))+SUM(QBMETCL(4,1:16,IY))+SUM(QBMBTCL(4,1:16,IY)))
    z[175] = (
        z[173] * dfd["QBMPWCL"].loc[5, 2:17, :].sum()
        + z[174]
        * (dfd["QBMETCL"].loc[5, 2:17, :].sum() + dfd["QBMBTCL"].loc[5, 2:17, :].sum())
    ) / (
        dfd["QBMPWCL"].loc[5, 2:17, :].sum()
        + dfd["QBMETCL"].loc[5, 2:17, :].sum()
        + dfd["QBMBTCL"].loc[5, 2:17, :].sum()
    ).replace(
        0, 1
    )

    return z
