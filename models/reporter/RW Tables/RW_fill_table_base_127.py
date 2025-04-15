# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM

SZO fixed all issues on 8/27/2024. 

"""


def fill_table_base_127(dfd, table_spec, table_id):
    """Fill table for Crude Oil Import Prices

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

    NUMREP = 12  # equal to number of rows per PADD
    MNUMPR = dfd["MNUMPR_rwpre"]

    # '''Fixed by SZO 8/27/2024'''
    # ftab.f
    #   ...
    # !Tables 126 and 127 are crude oil import volumes (126) and prices (127)
    #        NUMREP=12                        ! equal to number of rows per PADD
    #        DO IY = 1,LASTYR
    #          DO PADD=1,MNUMPR
    #  ...
    #   !  go into table 127, why don't we?
    #            T127(((PADD-1)*NUMREP)+ 1,IY,IS) = P_CRUDE_IMPORTS(PADD, 1,IY+1989)
    #            T127(((PADD-1)*NUMREP)+ 2,IY,IS) = P_CRUDE_IMPORTS(PADD, 2,IY+1989)
    #            T127(((PADD-1)*NUMREP)+ 3,IY,IS) = P_CRUDE_IMPORTS(PADD, 3,IY+1989)
    #            T127(((PADD-1)*NUMREP)+ 4,IY,IS) = P_CRUDE_IMPORTS(PADD, 4,IY+1989)
    #            T127(((PADD-1)*NUMREP)+ 5,IY,IS) = P_CRUDE_IMPORTS(PADD, 5,IY+1989)
    #            T127(((PADD-1)*NUMREP)+ 6,IY,IS) = P_CRUDE_IMPORTS(PADD, 6,IY+1989)
    #            T127(((PADD-1)*NUMREP)+ 7,IY,IS) = P_CRUDE_IMPORTS(PADD, 7,IY+1989)
    #            T127(((PADD-1)*NUMREP)+ 8,IY,IS) = P_CRUDE_IMPORTS(PADD, 8,IY+1989)
    #            T127(((PADD-1)*NUMREP)+ 9,IY,IS) = P_CRUDE_IMPORTS(PADD, 9,IY+1989)
    #            T127(((PADD-1)*NUMREP)+10,IY,IS) = P_CRUDE_IMPORTS(PADD,10,IY+1989)
    #            T127(((PADD-1)*NUMREP)+11,IY,IS) = P_CRUDE_IMPORTS(PADD,11,IY+1989)
    for PADD in range(1, int(MNUMPR) + 1):
        z[(PADD - 1) * NUMREP + 1] = dfd["P_CRUDE_IMPORTS"].loc[PADD, 1] * SCALPR2
        z[(PADD - 1) * NUMREP + 2] = dfd["P_CRUDE_IMPORTS"].loc[PADD, 2] * SCALPR2
        z[(PADD - 1) * NUMREP + 3] = dfd["P_CRUDE_IMPORTS"].loc[PADD, 3] * SCALPR2
        z[(PADD - 1) * NUMREP + 4] = dfd["P_CRUDE_IMPORTS"].loc[PADD, 4] * SCALPR2
        z[(PADD - 1) * NUMREP + 5] = dfd["P_CRUDE_IMPORTS"].loc[PADD, 5] * SCALPR2
        z[(PADD - 1) * NUMREP + 6] = dfd["P_CRUDE_IMPORTS"].loc[PADD, 6] * SCALPR2
        z[(PADD - 1) * NUMREP + 7] = dfd["P_CRUDE_IMPORTS"].loc[PADD, 7] * SCALPR2
        z[(PADD - 1) * NUMREP + 8] = dfd["P_CRUDE_IMPORTS"].loc[PADD, 8] * SCALPR2
        z[(PADD - 1) * NUMREP + 9] = dfd["P_CRUDE_IMPORTS"].loc[PADD, 9] * SCALPR2
        z[(PADD - 1) * NUMREP + 10] = dfd["P_CRUDE_IMPORTS"].loc[PADD, 10] * SCALPR2
        z[(PADD - 1) * NUMREP + 11] = dfd["P_CRUDE_IMPORTS"].loc[PADD, 11] * SCALPR2

        #     IF (( Q_CRUDE_IMPORTA(PADD,1,IY+1989) + Q_CRUDE_IMPORTA(PADD,2,IY+1989) + Q_CRUDE_IMPORTA(PADD,3,IY+1989) +  &
        #           Q_CRUDE_IMPORTA(PADD,4,IY+1989) + Q_CRUDE_IMPORTA(PADD,5,IY+1989) + Q_CRUDE_IMPORTA(PADD,6,IY+1989) +  &
        #           Q_CRUDE_IMPORTA(PADD,7,IY+1989) + Q_CRUDE_IMPORTA(PADD,8,IY+1989) + Q_CRUDE_IMPORTA(PADD,9,IY+1989)  +  &
        #           Q_CRUDE_IMPORTA(PADD,10,IY+1989) + Q_CRUDE_IMPORTA(PADD,11,IY+1989)) .GT. 0.0 ) &
        #    T127(((PADD-1)*NUMREP)+12,IY,IS) = &
        #        ((P_CRUDE_IMPORTS(PADD,1,IY+1989) * Q_CRUDE_IMPORTA(PADD,1,IY+1989) ) + &
        #         (P_CRUDE_IMPORTS(PADD,2,IY+1989) * Q_CRUDE_IMPORTA(PADD,2,IY+1989) ) + &
        #         (P_CRUDE_IMPORTS(PADD,3,IY+1989) * Q_CRUDE_IMPORTA(PADD,3,IY+1989) ) + &
        #         (P_CRUDE_IMPORTS(PADD,4,IY+1989) * Q_CRUDE_IMPORTA(PADD,4,IY+1989) ) + &
        #         (P_CRUDE_IMPORTS(PADD,5,IY+1989) * Q_CRUDE_IMPORTA(PADD,5,IY+1989) ) + &
        #         (P_CRUDE_IMPORTS(PADD,6,IY+1989) * Q_CRUDE_IMPORTA(PADD,6,IY+1989) ) + &
        #         (P_CRUDE_IMPORTS(PADD,7,IY+1989) * Q_CRUDE_IMPORTA(PADD,7,IY+1989) ) + &
        #         (P_CRUDE_IMPORTS(PADD,8,IY+1989) * Q_CRUDE_IMPORTA(PADD,8,IY+1989) ) + &
        #         (P_CRUDE_IMPORTS(PADD,9,IY+1989) * Q_CRUDE_IMPORTA(PADD,9,IY+1989) ) + &
        #         (P_CRUDE_IMPORTS(PADD,10,IY+1989) * Q_CRUDE_IMPORTA(PADD,10,IY+1989) ) + &
        #         (P_CRUDE_IMPORTS(PADD,11,IY+1989) * Q_CRUDE_IMPORTA(PADD,11,IY+1989) )) / &
        #        ( Q_CRUDE_IMPORTA(PADD,1,IY+1989) + Q_CRUDE_IMPORTA(PADD,2,IY+1989) + Q_CRUDE_IMPORTA(PADD,3,IY+1989) +  &
        #          Q_CRUDE_IMPORTA(PADD,4,IY+1989) + Q_CRUDE_IMPORTA(PADD,5,IY+1989) + Q_CRUDE_IMPORTA(PADD,6,IY+1989) +  &
        #          Q_CRUDE_IMPORTA(PADD,7,IY+1989) + Q_CRUDE_IMPORTA(PADD,8,IY+1989) + Q_CRUDE_IMPORTA(PADD,9,IY+1989) +  &
        #          Q_CRUDE_IMPORTA(PADD,10,IY+1989) + Q_CRUDE_IMPORTA(PADD,11,IY+1989))
        #  ENDDO
        z[(PADD - 1) * NUMREP + 12] = (
            (
                dfd["P_CRUDE_IMPORTS"].loc[PADD, 1]
                * dfd["Q_CRUDE_IMPORTA"].loc[PADD, 1]
                + dfd["P_CRUDE_IMPORTS"].loc[PADD, 2]
                * dfd["Q_CRUDE_IMPORTA"].loc[PADD, 2]
                + dfd["P_CRUDE_IMPORTS"].loc[PADD, 3]
                * dfd["Q_CRUDE_IMPORTA"].loc[PADD, 3]
                + dfd["P_CRUDE_IMPORTS"].loc[PADD, 4]
                * dfd["Q_CRUDE_IMPORTA"].loc[PADD, 4]
                + dfd["P_CRUDE_IMPORTS"].loc[PADD, 5]
                * dfd["Q_CRUDE_IMPORTA"].loc[PADD, 5]
                + dfd["P_CRUDE_IMPORTS"].loc[PADD, 6]
                * dfd["Q_CRUDE_IMPORTA"].loc[PADD, 6]
                + dfd["P_CRUDE_IMPORTS"].loc[PADD, 7]
                * dfd["Q_CRUDE_IMPORTA"].loc[PADD, 7]
                + dfd["P_CRUDE_IMPORTS"].loc[PADD, 8]
                * dfd["Q_CRUDE_IMPORTA"].loc[PADD, 8]
                + dfd["P_CRUDE_IMPORTS"].loc[PADD, 9]
                * dfd["Q_CRUDE_IMPORTA"].loc[PADD, 9]
                + dfd["P_CRUDE_IMPORTS"].loc[PADD, 10]
                * dfd["Q_CRUDE_IMPORTA"].loc[PADD, 10]
                + dfd["P_CRUDE_IMPORTS"].loc[PADD, 11]
                * dfd["Q_CRUDE_IMPORTA"].loc[PADD, 11]
            )
            / (
                dfd["Q_CRUDE_IMPORTA"].loc[PADD, 1]
                + dfd["Q_CRUDE_IMPORTA"].loc[PADD, 2]
                + dfd["Q_CRUDE_IMPORTA"].loc[PADD, 3]
                + dfd["Q_CRUDE_IMPORTA"].loc[PADD, 4]
                + dfd["Q_CRUDE_IMPORTA"].loc[PADD, 5]
                + dfd["Q_CRUDE_IMPORTA"].loc[PADD, 6]
                + dfd["Q_CRUDE_IMPORTA"].loc[PADD, 7]
                + dfd["Q_CRUDE_IMPORTA"].loc[PADD, 8]
                + dfd["Q_CRUDE_IMPORTA"].loc[PADD, 9]
                + dfd["Q_CRUDE_IMPORTA"].loc[PADD, 10]
                + dfd["Q_CRUDE_IMPORTA"].loc[PADD, 11]
            )
        ).fillna(0) * SCALPR2

        # DO PADD=1,11    !  re-using PADD counter variable here though we're looping over crude types
        #     IF (Q_CRUDE_IMPORTA(2,PADD,IY+1989) + Q_CRUDE_IMPORTA(3,PADD,IY+1989) .GT. 0.0) &
        #    T127(120+PADD,IY,IS) = &
        #        ((P_CRUDE_IMPORTS(2,PADD,IY+1989) * Q_CRUDE_IMPORTA(2,PADD,IY+1989)) + &
        #         (P_CRUDE_IMPORTS(3,PADD,IY+1989) * Q_CRUDE_IMPORTA(3,PADD,IY+1989))) / &
        #         (Q_CRUDE_IMPORTA(2,PADD,IY+1989) + Q_CRUDE_IMPORTA(3,PADD,IY+1989))
        #     IF (Q_CRUDE_IMPORTA(4,PADD,IY+1989) + Q_CRUDE_IMPORTA(5,PADD,IY+1989) .GT. 0.0) &
        #    T127(132+PADD,IY,IS) = &
        #        ((P_CRUDE_IMPORTS(4,PADD,IY+1989) * Q_CRUDE_IMPORTA(4,PADD,IY+1989)) + &
        #         (P_CRUDE_IMPORTS(5,PADD,IY+1989) * Q_CRUDE_IMPORTA(5,PADD,IY+1989))) / &
        #         (Q_CRUDE_IMPORTA(4,PADD,IY+1989) + Q_CRUDE_IMPORTA(5,PADD,IY+1989))
        #     IF (Q_CRUDE_IMPORTA(7,PADD,IY+1989) + Q_CRUDE_IMPORTA(8,PADD,IY+1989) .GT. 0.0) &
        #    T127(144+PADD,IY,IS) = &
        #        ((P_CRUDE_IMPORTS(7,PADD,IY+1989) * Q_CRUDE_IMPORTA(7,PADD,IY+1989)) + &
        #         (P_CRUDE_IMPORTS(8,PADD,IY+1989) * Q_CRUDE_IMPORTA(8,PADD,IY+1989))) / &
        #         (Q_CRUDE_IMPORTA(7,PADD,IY+1989) + Q_CRUDE_IMPORTA(8,PADD,IY+1989))
        #  ENDDO
    for x in range(121, 156):
        z[x] = z[1] * 0.0

    for PADD in range(
        1, 11 + 1
    ):  #  re-using PADD counter variable here though we're looping over crude types
        if (
            dfd["Q_CRUDE_IMPORTA"].loc[2, PADD] + dfd["Q_CRUDE_IMPORTA"].loc[3, PADD]
        ).any() > 0.0:
            z[120 + PADD] = (
                (
                    dfd["P_CRUDE_IMPORTS"].loc[2, PADD]
                    * dfd["Q_CRUDE_IMPORTA"].loc[2, PADD]
                    + dfd["P_CRUDE_IMPORTS"].loc[3, PADD]
                    * dfd["Q_CRUDE_IMPORTA"].loc[3, PADD]
                )
                / (
                    dfd["Q_CRUDE_IMPORTA"].loc[2, PADD]
                    + dfd["Q_CRUDE_IMPORTA"].loc[3, PADD]
                )
            ).fillna(0) * SCALPR2

        if (
            dfd["Q_CRUDE_IMPORTA"].loc[4, PADD] + dfd["Q_CRUDE_IMPORTA"].loc[5, PADD]
        ).any() > 0.0:
            z[132 + PADD] = (
                (
                    dfd["P_CRUDE_IMPORTS"].loc[4, PADD]
                    * dfd["Q_CRUDE_IMPORTA"].loc[4, PADD]
                    + (
                        dfd["P_CRUDE_IMPORTS"].loc[5, PADD]
                        * dfd["Q_CRUDE_IMPORTA"].loc[5, PADD]
                    )
                )
                / (
                    dfd["Q_CRUDE_IMPORTA"].loc[4, PADD]
                    + dfd["Q_CRUDE_IMPORTA"].loc[5, PADD]
                )
            ).fillna(0) * SCALPR2

        if (
            dfd["Q_CRUDE_IMPORTA"].loc[7, PADD] + dfd["Q_CRUDE_IMPORTA"].loc[8, PADD]
        ).any() > 0.0:
            z[144 + PADD] = (
                (
                    dfd["P_CRUDE_IMPORTS"].loc[7, PADD]
                    * dfd["Q_CRUDE_IMPORTA"].loc[7, PADD]
                    + (
                        dfd["P_CRUDE_IMPORTS"].loc[8, PADD]
                        * dfd["Q_CRUDE_IMPORTA"].loc[8, PADD]
                    )
                )
                / (
                    dfd["Q_CRUDE_IMPORTA"].loc[7, PADD]
                    + dfd["Q_CRUDE_IMPORTA"].loc[8, PADD]
                )
            ).fillna(0) * SCALPR2

        # Place holders
        z[132] = z[12] * 0
        z[144] = z[12] * 0
        z[156] = z[12] * 0

        #''' Moved the implementation of the following to the posprocessor_base'''
        #      IF ((sum(T126(121:131,IY,IS))) .GT. 0.0)     T127(132,IY,IS) = &
        #        ((T126(121,IY,IS) * T127(121,IY,IS)) + (T126(122,IY,IS) * T127(122,IY,IS)) + &
        #         (T126(123,IY,IS) * T127(123,IY,IS)) + (T126(124,IY,IS) * T127(124,IY,IS)) + &
        #         (T126(125,IY,IS) * T127(125,IY,IS)) + (T126(126,IY,IS) * T127(126,IY,IS)) + &
        #         (T126(127,IY,IS) * T127(127,IY,IS)) + (T126(128,IY,IS) * T127(128,IY,IS)) + &
        #         (T126(129,IY,IS) * T127(129,IY,IS)) + &
        #         (T126(130,IY,IS) * T127(130,IY,IS)) + (T126(131,IY,IS) * T127(131,IY,IS))) / &
        #                       (sum(T126(121:131,IY,IS)))
        #  IF ((sum(T126(133:143,IY,IS))) .GT. 0.0)     T127(144,IY,IS) = &
        #        ((T126(133,IY,IS) * T127(133,IY,IS)) + (T126(134,IY,IS) * T127(134,IY,IS)) + &
        #         (T126(135,IY,IS) * T127(135,IY,IS)) + (T126(136,IY,IS) * T127(136,IY,IS)) + &
        #         (T126(137,IY,IS) * T127(137,IY,IS)) + (T126(138,IY,IS) * T127(138,IY,IS)) + &
        #         (T126(139,IY,IS) * T127(139,IY,IS)) + (T126(140,IY,IS) * T127(140,IY,IS)) + &
        #         (T126(141,IY,IS) * T127(141,IY,IS)) + &
        #         (T126(142,IY,IS) * T127(142,IY,IS)) + (T126(143,IY,IS) * T127(143,IY,IS))) / &
        #                       (sum(T126(133:143,IY,IS)))
        #  IF ((sum(T126(145:155,IY,IS))) .GT. 0.0)     T127(156,IY,IS) = &
        #        ((T126(145,IY,IS) * T127(145,IY,IS)) + (T126(146,IY,IS) * T127(146,IY,IS)) + &
        #         (T126(147,IY,IS) * T127(147,IY,IS)) + (T126(148,IY,IS) * T127(148,IY,IS)) + &
        #         (T126(149,IY,IS) * T127(149,IY,IS)) + (T126(150,IY,IS) * T127(150,IY,IS)) + &
        #         (T126(151,IY,IS) * T127(151,IY,IS)) + (T126(152,IY,IS) * T127(152,IY,IS)) + &
        #         (T126(153,IY,IS) * T127(153,IY,IS)) + &
        #         (T126(154,IY,IS) * T127(154,IY,IS)) + (T126(155,IY,IS) * T127(155,IY,IS))) / &
        #                       (sum(T126(145:155,IY,IS)))
        # z[132]= ((dfd['Q_CRUDE_IMPORTA'].loc[2, 1] + dfd['Q_CRUDE_IMPORTA'].loc[3, 1])  * z[121]   + (dfd['Q_CRUDE_IMPORTA'].loc[2, 2] + dfd['Q_CRUDE_IMPORTA'].loc[3, 2]) * z[122] +
        #      (dfd['Q_CRUDE_IMPORTA'].loc[2, 3] + dfd['Q_CRUDE_IMPORTA'].loc[3, 3])  * z[123]   + (dfd['Q_CRUDE_IMPORTA'].loc[2, 4] + dfd['Q_CRUDE_IMPORTA'].loc[3, 4]) * z[124] +
        #      (dfd['Q_CRUDE_IMPORTA'].loc[2, 5] + dfd['Q_CRUDE_IMPORTA'].loc[3, 5])  * z[125]   + (dfd['Q_CRUDE_IMPORTA'].loc[2, 6] + dfd['Q_CRUDE_IMPORTA'].loc[3, 6]) * z[126] +
        #      (dfd['Q_CRUDE_IMPORTA'].loc[2, 7] + dfd['Q_CRUDE_IMPORTA'].loc[3, 7])  * z[127]   + (dfd['Q_CRUDE_IMPORTA'].loc[2, 8] + dfd['Q_CRUDE_IMPORTA'].loc[3, 8]) * z[128] +
        #      (dfd['Q_CRUDE_IMPORTA'].loc[2, 9] + dfd['Q_CRUDE_IMPORTA'].loc[3, 9])  * z[129]   + (dfd['Q_CRUDE_IMPORTA'].loc[2, 10] + dfd['Q_CRUDE_IMPORTA'].loc[3,10]) * z[130] +
        #      (dfd['Q_CRUDE_IMPORTA'].loc[2, 11] + dfd['Q_CRUDE_IMPORTA'].loc[3,11]) * z[131]) / (dfd['Q_CRUDE_IMPORTA'].loc[2, 1:11,:].sum()+dfd['Q_CRUDE_IMPORTA'].loc[3, 1:11,:].sum())

        # z[144]= ((dfd['Q_CRUDE_IMPORTA'].loc[4, 1] + dfd['Q_CRUDE_IMPORTA'].loc[5, 1])  * z[133] + (dfd['Q_CRUDE_IMPORTA'].loc[4, 2] + dfd['Q_CRUDE_IMPORTA'].loc[5, 2]) * z[134] +
        #      (dfd['Q_CRUDE_IMPORTA'].loc[4, 3] + dfd['Q_CRUDE_IMPORTA'].loc[5, 3]) * z[135] + (dfd['Q_CRUDE_IMPORTA'].loc[4, 4] + dfd['Q_CRUDE_IMPORTA'].loc[5, 4]) * z[136] +
        #      (dfd['Q_CRUDE_IMPORTA'].loc[4, 5] + dfd['Q_CRUDE_IMPORTA'].loc[5, 5]) * z[137] + (dfd['Q_CRUDE_IMPORTA'].loc[4, 6] + dfd['Q_CRUDE_IMPORTA'].loc[5, 6]) * z[138] +
        #      (dfd['Q_CRUDE_IMPORTA'].loc[4, 7] + dfd['Q_CRUDE_IMPORTA'].loc[5, 7]) * z[139] + (dfd['Q_CRUDE_IMPORTA'].loc[4, 8] + dfd['Q_CRUDE_IMPORTA'].loc[5, 8]) * z[140] +
        #      (dfd['Q_CRUDE_IMPORTA'].loc[4, 9] + dfd['Q_CRUDE_IMPORTA'].loc[5, 9]) * z[141] + (dfd['Q_CRUDE_IMPORTA'].loc[4, 10] + dfd['Q_CRUDE_IMPORTA'].loc[5,10]) * z[142] +
        #      (dfd['Q_CRUDE_IMPORTA'].loc[4, 11] + dfd['Q_CRUDE_IMPORTA'].loc[5,11]) * z[143]) / (dfd['Q_CRUDE_IMPORTA'].loc[4, 1:11,:].sum()+dfd['Q_CRUDE_IMPORTA'].loc[5, 1:11,:].sum())

        # z[156] =  ((dfd['Q_CRUDE_IMPORTA'].loc[7, 1] + dfd['Q_CRUDE_IMPORTA'].loc[8, 1]) * z[145] + (dfd['Q_CRUDE_IMPORTA'].loc[7, 2] + dfd['Q_CRUDE_IMPORTA'].loc[8, 2]) * z[146] +
        #       (dfd['Q_CRUDE_IMPORTA'].loc[7, 3] + dfd['Q_CRUDE_IMPORTA'].loc[8, 3]) * z[147] + (dfd['Q_CRUDE_IMPORTA'].loc[7, 4] + dfd['Q_CRUDE_IMPORTA'].loc[8, 4]) * z[148] +
        #       (dfd['Q_CRUDE_IMPORTA'].loc[7, 5] + dfd['Q_CRUDE_IMPORTA'].loc[8, 5]) * z[149] + (dfd['Q_CRUDE_IMPORTA'].loc[7, 6] + dfd['Q_CRUDE_IMPORTA'].loc[8, 6]) * z[150] +
        #       (dfd['Q_CRUDE_IMPORTA'].loc[7, 7] + dfd['Q_CRUDE_IMPORTA'].loc[8, 7]) * z[151] + (dfd['Q_CRUDE_IMPORTA'].loc[7, 8] + dfd['Q_CRUDE_IMPORTA'].loc[8, 8]) * z[152] +
        #       (dfd['Q_CRUDE_IMPORTA'].loc[7, 9] + dfd['Q_CRUDE_IMPORTA'].loc[8, 9]) * z[153] + (dfd['Q_CRUDE_IMPORTA'].loc[7, 10] + dfd['Q_CRUDE_IMPORTA'].loc[8,10]) * z[154] +
        #       (dfd['Q_CRUDE_IMPORTA'].loc[7, 11] + dfd['Q_CRUDE_IMPORTA'].loc[8,11])  * z[155]) / (dfd['Q_CRUDE_IMPORTA'].loc[7, 1:11,:].sum()+dfd['Q_CRUDE_IMPORTA'].loc[8, 1:11,:].sum())

    return z
