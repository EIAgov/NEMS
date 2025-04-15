# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
Fixed all issues on 8/27/2024. 
"""


def fill_table_base_129(dfd, table_spec, table_id):
    """Fill table for Petroleum Product Import Prices

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



    """

    z = {}

    FYRPRC = int(table_spec["settings"]["year_index_real_dol_priceyr"])
    # Get inflation index from 1990->2050
    MC_JPGDP = dfd["MC_JPGDP"].loc[1990:]

    # Get inflation index of FYRPRC (2022)
    SCALPR2 = MC_JPGDP.loc[FYRPRC]  # 2022 = column 33

    NUMREP = 18
    MNUMPR = dfd["MNUMPR_rwpre"]  # 10

    for PADD in range(1, int(MNUMPR) + 1):
        z[(PADD - 1) * NUMREP + 1] = dfd["RFIPQAS"].loc[PADD, 1] * SCALPR2
        z[(PADD - 1) * NUMREP + 2] = dfd["RFIPQAG"].loc[PADD, 1] * SCALPR2
        z[(PADD - 1) * NUMREP + 3] = dfd["RFIPQCG"].loc[PADD, 1] * SCALPR2
        # IF ((RFIPQMG(PADD,IY,2) + RFIPQCBOB(PADD,IY,2)) .NE. 0.0) &
        z[(PADD - 1) * NUMREP + 4] = (
            (
                dfd["RFIPQMG"].loc[PADD, 1] * dfd["RFIPQMG"].loc[PADD, 2]
                + dfd["RFIPQCBOB"].loc[PADD, 1] * dfd["RFIPQCBOB"].loc[PADD, 2]
            )
            / (dfd["RFIPQMG"].loc[PADD, 2] + dfd["RFIPQCBOB"].loc[PADD, 2])
            .replace(0, 1)
            .replace(0, 1)
        ) * SCALPR2
        z[(PADD - 1) * NUMREP + 5] = dfd["RFIPQCD"].loc[PADD, 1] * SCALPR2
        z[(PADD - 1) * NUMREP + 6] = dfd["RFIPQDL"].loc[PADD, 1] * SCALPR2
        z[(PADD - 1) * NUMREP + 7] = dfd["RFIPQDU"].loc[PADD, 1] * SCALPR2
        z[(PADD - 1) * NUMREP + 8] = dfd["RFIPQJF"].loc[PADD, 1] * SCALPR2
        # IF (RFIPQPR(PADD,IY,2) + RFIPQPY(PADD,IY,2) + RFIPQPP(PADD,IY,2) + &
        # RFIPQET(PADD,IY,2) + RFIPQBU(PADD,IY,2) + RFIPQIS(PADD,IY,2) .NE. 0.0) &

        z[(PADD - 1) * NUMREP + 9] = (
            (
                dfd["RFIPQPR"].loc[PADD, 1] * dfd["RFIPQPR"].loc[PADD, 2]
                + dfd["RFIPQPY"].loc[PADD, 1] * dfd["RFIPQPY"].loc[PADD, 2]
                + dfd["RFIPQET"].loc[PADD, 1] * dfd["RFIPQET"].loc[PADD, 2]
                + dfd["RFIPQBU"].loc[PADD, 1] * dfd["RFIPQBU"].loc[PADD, 2]
                + dfd["RFIPQIS"].loc[PADD, 1] * dfd["RFIPQIS"].loc[PADD, 2]
                + dfd["RFIPQPP"].loc[PADD, 1] * dfd["RFIPQPP"].loc[PADD, 2]
            )
            / (
                dfd["RFIPQPR"].loc[PADD, 2]
                + dfd["RFIPQPY"].loc[PADD, 2]
                + dfd["RFIPQPP"].loc[PADD, 2]
                + dfd["RFIPQET"].loc[PADD, 2]
                + dfd["RFIPQBU"].loc[PADD, 2]
                + dfd["RFIPQIS"].loc[PADD, 2]
            ).replace(0, 1)
        ) * SCALPR2

        z[(PADD - 1) * NUMREP + 10] = dfd["RFIPQLU"].loc[PADD, 1] * SCALPR2
        z[(PADD - 1) * NUMREP + 11] = dfd["RFIPQDS"].loc[PADD, 1] * SCALPR2
        z[(PADD - 1) * NUMREP + 12] = dfd["RFIPQRL"].loc[PADD, 1] * SCALPR2
        z[(PADD - 1) * NUMREP + 13] = dfd["RFIPQRH"].loc[PADD, 1] * SCALPR2
        z[(PADD - 1) * NUMREP + 14] = dfd["RFIPQPF"].loc[PADD, 1] * SCALPR2
        z[(PADD - 1) * NUMREP + 15] = dfd["RFIPQPC"].loc[PADD, 1] * SCALPR2

        #            IF ((RFIPQRG(PADD,IY,2) + RFIPQRBOB(PADD,IY,2)) .NE. 0.0) &
        #            T129(((PADD-1)*NUMREP)+16,IY,IS) = (RFIPQRG(PADD,IY,1) * RFIPQRG(PADD,IY,2) + &
        #                                                RFIPQRBOB(PADD,IY,1) * RFIPQRBOB(PADD,IY,2))/ &
        #                                               (RFIPQRG(PADD,IY,2) + RFIPQRBOB(PADD,IY,2))
        z[(PADD - 1) * NUMREP + 16] = (
            (
                dfd["RFIPQRG"].loc[PADD, 1] * dfd["RFIPQRG"].loc[PADD, 2]
                + dfd["RFIPQRBOB"].loc[PADD, 1] * dfd["RFIPQRBOB"].loc[PADD, 2]
            )
            / (dfd["RFIPQRG"].loc[PADD, 2] + dfd["RFIPQRBOB"].loc[PADD, 2]).replace(
                0, 1
            )
        ).fillna(0) * SCALPR2

        z[(PADD - 1) * NUMREP + 17] = dfd["RFPQUFC"].loc[PADD, 1] * SCALPR2
        # IF ((RFIPQAS(PADD,IY,2) + RFIPQAG(PADD,IY,2) + RFIPQCG(PADD,IY,2) + RFIPQMG(PADD,IY,2) + &
        # RFIPQRG(PADD,IY,2) + RFIPQCBOB(PADD,IY,2) + RFIPQRBOB(PADD,IY,2) + RFIPQCD(PADD,IY,2) + &
        # RFIPQDL(PADD,IY,2) + RFIPQDU(PADD,IY,2) + RFIPQJF(PADD,IY,2) + &
        # RFIPQPR(PADD,IY,2) + RFIPQPY(PADD,IY,2) + RFIPQPP(PADD,IY,2) + &
        # RFIPQET(PADD,IY,2) + RFIPQBU(PADD,IY,2) + RFIPQIS(PADD,IY,2) + &
        # RFIPQDS(PADD,IY,2) + RFIPQRL(PADD,IY,2) + RFIPQRH(PADD,IY,2) + &
        # RFIPQPC(PADD,IY,2) + RFIPQPF(PADD,IY,2) + RFPQUFC(PADD,IY,2)*1000.) .NE. 0.0) &
        z[(PADD - 1) * NUMREP + 18] = (
            (
                dfd["RFIPQMG"].loc[PADD, 1] * dfd["RFIPQMG"].loc[PADD, 2]
                + dfd["RFIPQRG"].loc[PADD, 1] * dfd["RFIPQRG"].loc[PADD, 2]
                + dfd["RFIPQAS"].loc[PADD, 1] * dfd["RFIPQAS"].loc[PADD, 2]
                + dfd["RFIPQAG"].loc[PADD, 1] * dfd["RFIPQAG"].loc[PADD, 2]
                + dfd["RFIPQCG"].loc[PADD, 1] * dfd["RFIPQCG"].loc[PADD, 2]
                + dfd["RFIPQCD"].loc[PADD, 1] * dfd["RFIPQCD"].loc[PADD, 2]
                + dfd["RFIPQLU"].loc[PADD, 1] * dfd["RFIPQLU"].loc[PADD, 2]
                + dfd["RFIPQCBOB"].loc[PADD, 1] * dfd["RFIPQCBOB"].loc[PADD, 2]
                + dfd["RFIPQRBOB"].loc[PADD, 1] * dfd["RFIPQRBOB"].loc[PADD, 2]
                + dfd["RFIPQJF"].loc[PADD, 1] * dfd["RFIPQJF"].loc[PADD, 2]
                + dfd["RFIPQPR"].loc[PADD, 1] * dfd["RFIPQPR"].loc[PADD, 2]
                + dfd["RFIPQPY"].loc[PADD, 1] * dfd["RFIPQPY"].loc[PADD, 2]
                + dfd["RFIPQET"].loc[PADD, 1] * dfd["RFIPQET"].loc[PADD, 2]
                + dfd["RFIPQPP"].loc[PADD, 1] * dfd["RFIPQPP"].loc[PADD, 2]
                + dfd["RFIPQBU"].loc[PADD, 1] * dfd["RFIPQBU"].loc[PADD, 2]
                + dfd["RFIPQIS"].loc[PADD, 1] * dfd["RFIPQIS"].loc[PADD, 2]
                + dfd["RFIPQDS"].loc[PADD, 1] * dfd["RFIPQDS"].loc[PADD, 2]
                + dfd["RFIPQDL"].loc[PADD, 1] * dfd["RFIPQDL"].loc[PADD, 2]
                + dfd["RFIPQDU"].loc[PADD, 1] * dfd["RFIPQDU"].loc[PADD, 2]
                + dfd["RFIPQRL"].loc[PADD, 1] * dfd["RFIPQRL"].loc[PADD, 2]
                + dfd["RFIPQRH"].loc[PADD, 1] * dfd["RFIPQRH"].loc[PADD, 2]
                + dfd["RFIPQPF"].loc[PADD, 1] * dfd["RFIPQPF"].loc[PADD, 2]
                + dfd["RFIPQPC"].loc[PADD, 1] * dfd["RFIPQPC"].loc[PADD, 2]
                + dfd["RFPQUFC"].loc[PADD, 1] * dfd["RFPQUFC"].loc[PADD, 2] * 1000
            )
            / (
                dfd["RFIPQMG"].loc[PADD, 2]
                + dfd["RFIPQRG"].loc[PADD, 2]
                + dfd["RFIPQJF"].loc[PADD, 2]
                + dfd["RFIPQPR"].loc[PADD, 2]
                + dfd["RFIPQPY"].loc[PADD, 2]
                + dfd["RFIPQPP"].loc[PADD, 2]
                + dfd["RFIPQET"].loc[PADD, 2]
                + dfd["RFIPQBU"].loc[PADD, 2]
                + dfd["RFIPQIS"].loc[PADD, 2]
                + dfd["RFIPQCBOB"].loc[PADD, 2]
                + dfd["RFIPQRBOB"].loc[PADD, 2]
                + dfd["RFIPQCD"].loc[PADD, 2]
                + dfd["RFIPQDS"].loc[PADD, 2]
                + dfd["RFIPQDL"].loc[PADD, 2]
                + dfd["RFIPQDU"].loc[PADD, 2]
                + dfd["RFIPQRL"].loc[PADD, 2]
                + dfd["RFIPQAS"].loc[PADD, 2]
                + dfd["RFIPQAG"].loc[PADD, 2]
                + dfd["RFIPQCG"].loc[PADD, 2]
                + dfd["RFIPQLU"].loc[PADD, 2]
                + dfd["RFIPQRH"].loc[PADD, 2]
                + dfd["RFIPQPC"].loc[PADD, 2]
                + dfd["RFIPQPF"].loc[PADD, 2]
                + dfd["RFPQUFC"].loc[PADD, 2] * 1000
            ).replace(0, 1)
        ) * SCALPR2

    return z
