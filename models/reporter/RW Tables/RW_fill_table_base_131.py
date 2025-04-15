# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
SZO fixed all issues on 8/23/2024. 
"""


def fill_table_base_131(dfd, table_spec, table_id):
    """Fill table for Crude Oil Export Prices

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

    # LFMMOUT P_CRUDE_EXPORTS  R    4 3 (10,MNCRUD,MNXYRS)  PRICE  87$BBL CR     REFINE Exported crude oil price
    # LFMMOUT Q_CRUDE_EXPORTS  R    4 3 (10,MNCRUD,MNXYRS)  QUNTY  MBCD   CR     REFINE Exported crude oil quantity
    # DIM  MNXYRS    91      Expectation Years
    # DIM  MNCRUD    11      Crude oil types (LFMM)
    # ...
    # ftab.f
    # ...
    # !Tables 130 and 131 are crude oil import volumes (130) and prices (131)
    #        NUMREP=12                        ! equal to number of rows per PADD
    #        DO IY = 1,LASTYR
    #          DO PADD=1,MNUMPR
    # ...
    # !  go into table 127, why don't we?
    #            T131(((PADD-1)*NUMREP)+ 1,IY,IS) = P_CRUDE_EXPORTS(PADD, 1,IY+1989)
    #            T131(((PADD-1)*NUMREP)+ 2,IY,IS) = P_CRUDE_EXPORTS(PADD, 2,IY+1989)
    #            T131(((PADD-1)*NUMREP)+ 3,IY,IS) = P_CRUDE_EXPORTS(PADD, 3,IY+1989)
    #            T131(((PADD-1)*NUMREP)+ 4,IY,IS) = P_CRUDE_EXPORTS(PADD, 4,IY+1989)
    #            T131(((PADD-1)*NUMREP)+ 5,IY,IS) = P_CRUDE_EXPORTS(PADD, 5,IY+1989)
    #            T131(((PADD-1)*NUMREP)+ 6,IY,IS) = P_CRUDE_EXPORTS(PADD, 6,IY+1989)
    #            T131(((PADD-1)*NUMREP)+ 7,IY,IS) = P_CRUDE_EXPORTS(PADD, 7,IY+1989)
    #            T131(((PADD-1)*NUMREP)+ 8,IY,IS) = P_CRUDE_EXPORTS(PADD, 8,IY+1989)
    #            T131(((PADD-1)*NUMREP)+ 9,IY,IS) = P_CRUDE_EXPORTS(PADD, 9,IY+1989)
    #            T131(((PADD-1)*NUMREP)+10,IY,IS) = P_CRUDE_EXPORTS(PADD,10,IY+1989)
    #            T131(((PADD-1)*NUMREP)+11,IY,IS) = P_CRUDE_EXPORTS(PADD,11,IY+1989)
    #
    for PADD in range(1, int(MNUMPR) + 1):
        z[(PADD - 1) * NUMREP + 1] = dfd["P_CRUDE_EXPORTS"].loc[PADD, 1] * SCALPR2
        z[(PADD - 1) * NUMREP + 2] = dfd["P_CRUDE_EXPORTS"].loc[PADD, 2] * SCALPR2
        z[(PADD - 1) * NUMREP + 3] = dfd["P_CRUDE_EXPORTS"].loc[PADD, 3] * SCALPR2
        z[(PADD - 1) * NUMREP + 4] = dfd["P_CRUDE_EXPORTS"].loc[PADD, 4] * SCALPR2
        z[(PADD - 1) * NUMREP + 5] = dfd["P_CRUDE_EXPORTS"].loc[PADD, 5] * SCALPR2
        z[(PADD - 1) * NUMREP + 6] = dfd["P_CRUDE_EXPORTS"].loc[PADD, 6] * SCALPR2
        z[(PADD - 1) * NUMREP + 7] = dfd["P_CRUDE_EXPORTS"].loc[PADD, 7] * SCALPR2
        z[(PADD - 1) * NUMREP + 8] = dfd["P_CRUDE_EXPORTS"].loc[PADD, 8] * SCALPR2
        z[(PADD - 1) * NUMREP + 9] = dfd["P_CRUDE_EXPORTS"].loc[PADD, 9] * SCALPR2
        z[(PADD - 1) * NUMREP + 10] = dfd["P_CRUDE_EXPORTS"].loc[PADD, 10] * SCALPR2
        z[(PADD - 1) * NUMREP + 11] = dfd["P_CRUDE_EXPORTS"].loc[PADD, 11] * SCALPR2

        #     IF (( Q_CRUDE_EXPORTS(PADD,1,IY+1989) + Q_CRUDE_EXPORTS(PADD,2,IY+1989) + Q_CRUDE_EXPORTS(PADD,3,IY+1989) +  &
        #           Q_CRUDE_EXPORTS(PADD,4,IY+1989) + Q_CRUDE_EXPORTS(PADD,5,IY+1989) + Q_CRUDE_EXPORTS(PADD,6,IY+1989) +  &
        #           Q_CRUDE_EXPORTS(PADD,7,IY+1989) + Q_CRUDE_EXPORTS(PADD,8,IY+1989) + Q_CRUDE_EXPORTS(PADD,9,IY+1989)  +  &
        #           Q_CRUDE_EXPORTS(PADD,10,IY+1989) + Q_CRUDE_EXPORTS(PADD,11,IY+1989)) .GT. 0.0 ) &
        #    T131(((PADD-1)*NUMREP)+12,IY,IS) = &
        #        ((P_CRUDE_EXPORTS(PADD,1,IY+1989) * Q_CRUDE_EXPORTS(PADD,1,IY+1989) ) + &
        #         (P_CRUDE_EXPORTS(PADD,2,IY+1989) * Q_CRUDE_EXPORTS(PADD,2,IY+1989) ) + &
        #         (P_CRUDE_EXPORTS(PADD,3,IY+1989) * Q_CRUDE_EXPORTS(PADD,3,IY+1989) ) + &
        #         (P_CRUDE_EXPORTS(PADD,4,IY+1989) * Q_CRUDE_EXPORTS(PADD,4,IY+1989) ) + &
        #         (P_CRUDE_EXPORTS(PADD,5,IY+1989) * Q_CRUDE_EXPORTS(PADD,5,IY+1989) ) + &
        #         (P_CRUDE_EXPORTS(PADD,6,IY+1989) * Q_CRUDE_EXPORTS(PADD,6,IY+1989) ) + &
        #         (P_CRUDE_EXPORTS(PADD,7,IY+1989) * Q_CRUDE_EXPORTS(PADD,7,IY+1989) ) + &
        #         (P_CRUDE_EXPORTS(PADD,8,IY+1989) * Q_CRUDE_EXPORTS(PADD,8,IY+1989) ) + &
        #         (P_CRUDE_EXPORTS(PADD,9,IY+1989) * Q_CRUDE_EXPORTS(PADD,9,IY+1989) ) + &
        #         (P_CRUDE_EXPORTS(PADD,10,IY+1989) * Q_CRUDE_EXPORTS(PADD,10,IY+1989) ) + &
        #         (P_CRUDE_EXPORTS(PADD,11,IY+1989) * Q_CRUDE_EXPORTS(PADD,11,IY+1989) )) / &
        #        ( Q_CRUDE_EXPORTS(PADD,1,IY+1989) + Q_CRUDE_EXPORTS(PADD,2,IY+1989) + Q_CRUDE_EXPORTS(PADD,3,IY+1989) +  &
        #          Q_CRUDE_EXPORTS(PADD,4,IY+1989) + Q_CRUDE_EXPORTS(PADD,5,IY+1989) + Q_CRUDE_EXPORTS(PADD,6,IY+1989) +  &
        #          Q_CRUDE_EXPORTS(PADD,7,IY+1989) + Q_CRUDE_EXPORTS(PADD,8,IY+1989) + Q_CRUDE_EXPORTS(PADD,9,IY+1989) +  &
        #          Q_CRUDE_EXPORTS(PADD,10,IY+1989) + Q_CRUDE_EXPORTS(PADD,11,IY+1989))
        z[(PADD - 1) * NUMREP + 12] = SCALPR2 * (
            (
                dfd["P_CRUDE_EXPORTS"].loc[PADD, 1]
                * dfd["Q_CRUDE_EXPORTS"].loc[PADD, 1]
                + dfd["P_CRUDE_EXPORTS"].loc[PADD, 2]
                * dfd["Q_CRUDE_EXPORTS"].loc[PADD, 2]
                + dfd["P_CRUDE_EXPORTS"].loc[PADD, 3]
                * dfd["Q_CRUDE_EXPORTS"].loc[PADD, 3]
                + dfd["P_CRUDE_EXPORTS"].loc[PADD, 4]
                * dfd["Q_CRUDE_EXPORTS"].loc[PADD, 4]
                + dfd["P_CRUDE_EXPORTS"].loc[PADD, 5]
                * dfd["Q_CRUDE_EXPORTS"].loc[PADD, 5]
                + dfd["P_CRUDE_EXPORTS"].loc[PADD, 6]
                * dfd["Q_CRUDE_EXPORTS"].loc[PADD, 6]
                + dfd["P_CRUDE_EXPORTS"].loc[PADD, 7]
                * dfd["Q_CRUDE_EXPORTS"].loc[PADD, 7]
                + dfd["P_CRUDE_EXPORTS"].loc[PADD, 8]
                * dfd["Q_CRUDE_EXPORTS"].loc[PADD, 8]
                + dfd["P_CRUDE_EXPORTS"].loc[PADD, 9]
                * dfd["Q_CRUDE_EXPORTS"].loc[PADD, 9]
                + dfd["P_CRUDE_EXPORTS"].loc[PADD, 10]
                * dfd["Q_CRUDE_EXPORTS"].loc[PADD, 10]
                + dfd["P_CRUDE_EXPORTS"].loc[PADD, 11]
                * dfd["Q_CRUDE_EXPORTS"].loc[PADD, 11]
            )
            / (
                dfd["Q_CRUDE_EXPORTS"].loc[PADD, 1]
                + dfd["Q_CRUDE_EXPORTS"].loc[PADD, 2]
                + dfd["Q_CRUDE_EXPORTS"].loc[PADD, 3]
                + dfd["Q_CRUDE_EXPORTS"].loc[PADD, 4]
                + dfd["Q_CRUDE_EXPORTS"].loc[PADD, 5]
                + dfd["Q_CRUDE_EXPORTS"].loc[PADD, 6]
                + dfd["Q_CRUDE_EXPORTS"].loc[PADD, 7]
                + dfd["Q_CRUDE_EXPORTS"].loc[PADD, 8]
                + dfd["Q_CRUDE_EXPORTS"].loc[PADD, 9]
                + dfd["Q_CRUDE_EXPORTS"].loc[PADD, 10]
                + dfd["Q_CRUDE_EXPORTS"].loc[PADD, 11]
            )
        ).fillna(0)

        # DO PADD=1,11    !  re-using PADD counter variable here though we're looping over crude types
        #     IF (Q_CRUDE_EXPORTS(2,PADD,IY+1989) + Q_CRUDE_EXPORTS(3,PADD,IY+1989) .GT. 0.0) &
        #    T131(120+PADD,IY,IS) = &
        #        ((P_CRUDE_EXPORTS(2,PADD,IY+1989) * Q_CRUDE_EXPORTS(2,PADD,IY+1989)) + &
        #         (P_CRUDE_EXPORTS(3,PADD,IY+1989) * Q_CRUDE_EXPORTS(3,PADD,IY+1989))) / &
        #         (Q_CRUDE_EXPORTS(2,PADD,IY+1989) + Q_CRUDE_EXPORTS(3,PADD,IY+1989))
        #     IF (Q_CRUDE_EXPORTS(4,PADD,IY+1989) + Q_CRUDE_EXPORTS(5,PADD,IY+1989) .GT. 0.0) &
        #    T131(132+PADD,IY,IS) = &
        #        ((P_CRUDE_EXPORTS(4,PADD,IY+1989) * Q_CRUDE_EXPORTS(4,PADD,IY+1989)) + &
        #         (P_CRUDE_EXPORTS(5,PADD,IY+1989) * Q_CRUDE_EXPORTS(5,PADD,IY+1989))) / &
        #         (Q_CRUDE_EXPORTS(4,PADD,IY+1989) + Q_CRUDE_EXPORTS(5,PADD,IY+1989))
        #     IF (Q_CRUDE_EXPORTS(7,PADD,IY+1989) + Q_CRUDE_EXPORTS(8,PADD,IY+1989) .GT. 0.0) &
        #    T131(144+PADD,IY,IS) = &
        #        ((P_CRUDE_EXPORTS(7,PADD,IY+1989) * Q_CRUDE_EXPORTS(7,PADD,IY+1989)) + &
        #         (P_CRUDE_EXPORTS(8,PADD,IY+1989) * Q_CRUDE_EXPORTS(8,PADD,IY+1989))) / &
        #         (Q_CRUDE_EXPORTS(7,PADD,IY+1989) + Q_CRUDE_EXPORTS(8,PADD,IY+1989))
        #  ENDDO

    for PADD in range(
        1, 12
    ):  #  re-using PADD counter variable here though we're looping over crude types
        # IF (Q_CRUDE_EXPORTS(2,PADD,IY+1989) + Q_CRUDE_EXPORTS(3,PADD,IY+1989) .GT. 0.0) &
        z[120 + PADD] = (
            (
                dfd["P_CRUDE_EXPORTS"].loc[2, PADD]
                * dfd["Q_CRUDE_EXPORTS"].loc[2, PADD]
                + dfd["P_CRUDE_EXPORTS"].loc[3, PADD]
                * dfd["Q_CRUDE_EXPORTS"].loc[3, PADD]
            )
            / (
                dfd["Q_CRUDE_EXPORTS"].loc[2, PADD]
                + dfd["Q_CRUDE_EXPORTS"].loc[3, PADD]
            )
        ).fillna(0) * SCALPR2
        # IF (Q_CRUDE_EXPORTS(4,PADD,IY+1989) + Q_CRUDE_EXPORTS(5,PADD,IY+1989) .GT. 0.0) &
        z[132 + PADD] = (
            (
                dfd["P_CRUDE_EXPORTS"].loc[4, PADD]
                * dfd["Q_CRUDE_EXPORTS"].loc[4, PADD]
                + (
                    dfd["P_CRUDE_EXPORTS"].loc[5, PADD]
                    * dfd["Q_CRUDE_EXPORTS"].loc[5, PADD]
                )
            )
            / (
                dfd["Q_CRUDE_EXPORTS"].loc[4, PADD]
                + dfd["Q_CRUDE_EXPORTS"].loc[5, PADD]
            )
        ).fillna(0) * SCALPR2
        # IF (Q_CRUDE_EXPORTS(7,PADD,IY+1989) + Q_CRUDE_EXPORTS(8,PADD,IY+1989) .GT. 0.0) &
        z[144 + PADD] = (
            (
                dfd["P_CRUDE_EXPORTS"].loc[7, PADD]
                * dfd["Q_CRUDE_EXPORTS"].loc[7, PADD]
                + (
                    dfd["P_CRUDE_EXPORTS"].loc[8, PADD]
                    * dfd["Q_CRUDE_EXPORTS"].loc[8, PADD]
                )
            )
            / (
                dfd["Q_CRUDE_EXPORTS"].loc[7, PADD]
                + dfd["Q_CRUDE_EXPORTS"].loc[8, PADD]
            )
        ).fillna(0) * SCALPR2

        """ Moved the following code to the postprocessor_base"""
        #  IF ((sum(T130(121:131,IY,IS))) .GT. 0.0)     T131(132,IY,IS) = &
        #        ((T130(121,IY,IS) * T131(121,IY,IS)) + (T130(122,IY,IS) * T131(122,IY,IS)) + &
        #         (T130(123,IY,IS) * T131(123,IY,IS)) + (T130(124,IY,IS) * T131(124,IY,IS)) + &
        #         (T130(125,IY,IS) * T131(125,IY,IS)) + (T130(126,IY,IS) * T131(126,IY,IS)) + &
        #         (T130(127,IY,IS) * T131(127,IY,IS)) + (T130(128,IY,IS) * T131(128,IY,IS)) + &
        #         (T130(129,IY,IS) * T131(129,IY,IS)) + &
        #         (T130(130,IY,IS) * T131(130,IY,IS)) + (T130(131,IY,IS) * T131(131,IY,IS))) / &
        #                       (sum(T130(121:131,IY,IS)))
        #  IF ((sum(T130(133:143,IY,IS))) .GT. 0.0)     T131(144,IY,IS) = &
        #        ((T130(133,IY,IS) * T131(133,IY,IS)) + (T130(134,IY,IS) * T131(134,IY,IS)) + &
        #         (T130(135,IY,IS) * T131(135,IY,IS)) + (T130(136,IY,IS) * T131(136,IY,IS)) + &
        #         (T130(137,IY,IS) * T131(137,IY,IS)) + (T130(138,IY,IS) * T131(138,IY,IS)) + &
        #         (T130(139,IY,IS) * T131(139,IY,IS)) + (T130(140,IY,IS) * T131(140,IY,IS)) + &
        #         (T130(141,IY,IS) * T131(141,IY,IS)) + &
        #         (T130(142,IY,IS) * T131(142,IY,IS)) + (T130(143,IY,IS) * T131(143,IY,IS))) / &
        #                       (sum(T130(133:143,IY,IS)))
        #  IF ((sum(T130(145:155,IY,IS))) .GT. 0.0)     T131(156,IY,IS) = &
        #        ((T130(145,IY,IS) * T131(145,IY,IS)) + (T130(146,IY,IS) * T131(146,IY,IS)) + &
        #         (T130(147,IY,IS) * T131(147,IY,IS)) + (T130(148,IY,IS) * T131(148,IY,IS)) + &
        #         (T130(149,IY,IS) * T131(149,IY,IS)) + (T130(150,IY,IS) * T131(150,IY,IS)) + &
        #         (T130(151,IY,IS) * T131(151,IY,IS)) + (T130(152,IY,IS) * T131(152,IY,IS)) + &
        #         (T130(153,IY,IS) * T131(153,IY,IS)) + &
        #         (T130(154,IY,IS) * T131(154,IY,IS)) + (T130(155,IY,IS) * T131(155,IY,IS))) / &
        #                       (sum(T130(145:155,IY,IS)))
        z[132] = z[12] * 0
        z[144] = z[12] * 0
        z[156] = z[12] * 0

    return z
