# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_076(dfd, table_spec, table_id):
    """Fill table for Natural Gas Imports and Exports

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
    TRIL_TO_QUAD = dfd["TRIL_TO_QUAD_rwpre"]

    #   Natural Gas Imports and Exports
    #    Volumes and Prices
    #
    #   Volumes (trillion cubic feet)

    #      Pipeline Imports from Canada                             T76(1,IY,IS)=NGIMPVOL(1,IY)*.001
    z[1] = dfd["NGIMPVOL"].loc[1] * TRIL_TO_QUAD

    #      Pipeline Imports from Mexico                             T76(2,IY,IS)=NGIMPVOL(2,IY)*.001
    z[2] = dfd["NGIMPVOL"].loc[2] * TRIL_TO_QUAD

    #      Liquefied Natural Gas Imports                            T76(3,IY,IS)=NGIMPVOL(3,IY)*.001
    z[3] = dfd["NGIMPVOL"].loc[3] * TRIL_TO_QUAD
    #    Imports                                                    T76(10,IY,IS)=FSUM(T76(1,IY,IS),3)
    z[10] = z[1] + z[2] + z[3]

    #      Pipeline Exports to Canada                               T76(4,IY,IS)=NGEXPVOL(1,IY)*.001
    z[4] = dfd["NGEXPVOL"].loc[1] * TRIL_TO_QUAD

    #      Pipeline Exports to Mexico                               T76(5,IY,IS)=NGEXPVOL(2,IY)*.001
    z[5] = dfd["NGEXPVOL"].loc[2] * TRIL_TO_QUAD

    #      Liquefied Natural Gas Exports                            T76(6,IY,IS)=NGEXPVOL(3,IY)*.001
    z[6] = dfd["NGEXPVOL"].loc[3] * TRIL_TO_QUAD
    #    Exports                                                    T76(11,IY,IS)=FSUM(T76(4,IY,IS),3)
    z[11] = z[4] + z[5] + z[6]

    #      Canada                                                   T76(7,IY,IS)=T76(1,IY,IS)-T76(4,IY,IS)
    z[7] = z[1] - z[4]

    #      Mexico                                                   T76(8,IY,IS)=T76(2,IY,IS)-T76(5,IY,IS)
    z[8] = z[2] - z[5]

    #      Liquefied Natural Gas                                    T76(9,IY,IS)=T76(3,IY,IS)-T76(6,IY,IS)
    z[9] = z[3] - z[6]

    #    Net Imports                                                T76(12,IY,IS)=FSUM(T76(7,IY,IS),3)
    z[12] = z[7] + z[8] + z[9]

    #   Border Prices
    #   (#### dollars per thousand cubic feet)
    #      Average Import Price                                     T76(13,IY,IS)=NGIMPPRC(4,IY)
    z[13] = dfd["NGIMPPRC"].loc[4] * SCALPR2

    #         Pipeline Imports from Canada                          T76(14,IY,IS)=NGIMPPRC(1,IY)
    z[14] = dfd["NGIMPPRC"].loc[1] * SCALPR2

    #         Pipeline Imports from Mexico                          T76(15,IY,IS)=NGIMPPRC(2,IY)
    z[15] = dfd["NGIMPPRC"].loc[2] * SCALPR2

    #         Liquefied Natural Gas 1/                              T76(16,IY,IS)=NGIMPPRC(3,IY)
    z[16] = dfd["NGIMPPRC"].loc[3] * SCALPR2

    #
    #   -------------------------
    #   Exclude from here down when creating Supplement
    #   -------------------------
    #
    #   North American LNG Exports
    #       East - New England                                      T76(17,IY,IS)=NALNGEXP(1,IY+BASEYR-1)
    z[17] = dfd["NALNGEXP"].loc[1]

    #       East - Mid Atlantic                                     T76(18,IY,IS)=NALNGEXP(2,IY+BASEYR-1)
    z[18] = dfd["NALNGEXP"].loc[2]

    #       East - South Atlantic                                   T76(19,IY,IS)=NALNGEXP(3,IY+BASEYR-1)
    z[19] = dfd["NALNGEXP"].loc[3]

    #       Gulf Coast - Florida                                    T76(20,IY,IS)=NALNGEXP(4,IY+BASEYR-1)
    z[20] = dfd["NALNGEXP"].loc[4]

    #       Gulf Coast - Alabama and Mississippi                    T76(21,IY,IS)=NALNGEXP(5,IY+BASEYR-1)
    z[21] = dfd["NALNGEXP"].loc[5]

    #       Gulf Coast - Texas and Louisiana                        T76(22,IY,IS)=NALNGEXP(6,IY+BASEYR-1)
    z[22] = dfd["NALNGEXP"].loc[6]

    #       West Coast - Washington and Oregon                      T76(23,IY,IS)=NALNGEXP(7,IY+BASEYR-1)
    z[23] = dfd["NALNGEXP"].loc[7]

    #       West Coast - California                                 T76(24,IY,IS)=NALNGEXP(8,IY+BASEYR-1)
    z[24] = dfd["NALNGEXP"].loc[8]

    #       Canada East                                             T76(25,IY,IS)=NALNGEXP(9,IY+BASEYR-1)
    z[25] = dfd["NALNGEXP"].loc[9]

    #       Canada West                                             T76(26,IY,IS)=NALNGEXP(10,IY+BASEYR-1)
    z[26] = dfd["NALNGEXP"].loc[10]

    #       Alaska                                                  T76(27,IY,IS)=NALNGEXP(11,IY+BASEYR-1)
    z[27] = dfd["NALNGEXP"].loc[11]

    #         Total                                                 T76(28,IY,IS)=FSUM(T76(17,IY,IS),11)
    z[28] = (
        z[17]
        + z[18]
        + z[19]
        + z[20]
        + z[21]
        + z[22]
        + z[23]
        + z[24]
        + z[25]
        + z[26]
        + z[27]
    )

    #
    #   Price of Natural Gas in Europe                              T76(29,IY,IS)=PINTLNG(1,IY+BASEYR-1)
    z[29] = dfd["PINTLNG"].loc[1] * SCALPR2

    #   Price of Natural Gas in Japan                               T76(30,IY,IS)=PINTLNG(2,IY+BASEYR-1)
    z[30] = dfd["PINTLNG"].loc[2] * SCALPR2

    #   Compare to:
    #   United States Terminal Natural Gas Price
    #       East - New England                                      T76(31,IY,IS)=PUSNG(1,IY+BASEYR-1)
    z[31] = dfd["PUSNG"].loc[1] * SCALPR2

    #       East - Mid Atlantic                                     T76(32,IY,IS)=PUSNG(2,IY+BASEYR-1)
    z[32] = dfd["PUSNG"].loc[2] * SCALPR2

    #       East - South Atlantic                                   T76(33,IY,IS)=PUSNG(3,IY+BASEYR-1)
    z[33] = dfd["PUSNG"].loc[3] * SCALPR2

    #       Gulf Coast - Florida                                    T76(34,IY,IS)=PUSNG(4,IY+BASEYR-1)
    z[34] = dfd["PUSNG"].loc[4] * SCALPR2

    #       Gulf Coast - Alabama and Mississippi                    T76(35,IY,IS)=PUSNG(5,IY+BASEYR-1)
    z[35] = dfd["PUSNG"].loc[5] * SCALPR2

    #       Gulf Coast - Texas and Louisiana                        T76(36,IY,IS)=PUSNG(6,IY+BASEYR-1)
    z[36] = dfd["PUSNG"].loc[6] * SCALPR2

    #       West Coast - Washington and Oregon                      T76(37,IY,IS)=PUSNG(7,IY+BASEYR-1)
    z[37] = dfd["PUSNG"].loc[7] * SCALPR2

    #       West Coast - California                                 T76(38,IY,IS)=PUSNG(8,IY+BASEYR-1)
    z[38] = dfd["PUSNG"].loc[8] * SCALPR2

    #   U.S. Natural Gas Price + Transport to Europe
    #       East - New England                                      T76(39,IY,IS)=PUSNG(1,IY+BASEYR-1)+PTRANSNG(1,1,IY+BASEYR-1)
    z[39] = z[31] + dfd["PTRANSNG"].loc[1].loc[1] * SCALPR2

    #       East - Mid Atlantic                                     T76(40,IY,IS)=PUSNG(2,IY+BASEYR-1)+PTRANSNG(1,2,IY+BASEYR-1)
    z[40] = z[32] + dfd["PTRANSNG"].loc[1].loc[2] * SCALPR2

    #       East - South Atlantic                                   T76(41,IY,IS)=PUSNG(3,IY+BASEYR-1)+PTRANSNG(1,3,IY+BASEYR-1)
    z[41] = z[33] + dfd["PTRANSNG"].loc[1].loc[3] * SCALPR2

    #       Gulf Coast - Florida                                    T76(42,IY,IS)=PUSNG(4,IY+BASEYR-1)+PTRANSNG(1,4,IY+BASEYR-1)
    z[42] = z[34] + dfd["PTRANSNG"].loc[1].loc[4] * SCALPR2

    #       Gulf Coast - Alabama and Mississippi                    T76(43,IY,IS)=PUSNG(5,IY+BASEYR-1)+PTRANSNG(1,5,IY+BASEYR-1)
    z[43] = z[35] + dfd["PTRANSNG"].loc[1].loc[5] * SCALPR2

    #       Gulf Coast - Texas and Louisiana                        T76(44,IY,IS)=PUSNG(6,IY+BASEYR-1)+PTRANSNG(1,6,IY+BASEYR-1)
    z[44] = z[36] + dfd["PTRANSNG"].loc[1].loc[6] * SCALPR2

    #       West Coast - Washington and Oregon                      T76(45,IY,IS)=PUSNG(7,IY+BASEYR-1)+PTRANSNG(1,7,IY+BASEYR-1)
    z[45] = z[37] + dfd["PTRANSNG"].loc[1].loc[7] * SCALPR2

    #       West Coast - California                                 T76(46,IY,IS)=PUSNG(8,IY+BASEYR-1)+PTRANSNG(1,8,IY+BASEYR-1)
    z[46] = z[38] + dfd["PTRANSNG"].loc[1].loc[8] * SCALPR2

    #   U.S. Natural Gas Price + Transport to Japan
    #       East - New England                                      T76(47,IY,IS)=PUSNG(1,IY+BASEYR-1)+PTRANSNG(2,1,IY+BASEYR-1)
    z[47] = z[31] + dfd["PTRANSNG"].loc[2].loc[1] * SCALPR2

    #       East - Mid Atlantic                                     T76(48,IY,IS)=PUSNG(2,IY+BASEYR-1)+PTRANSNG(2,2,IY+BASEYR-1)
    z[48] = z[32] + dfd["PTRANSNG"].loc[2].loc[2] * SCALPR2

    #       East - South Atlantic                                   T76(49,IY,IS)=PUSNG(3,IY+BASEYR-1)+PTRANSNG(2,3,IY+BASEYR-1)
    z[49] = z[33] + dfd["PTRANSNG"].loc[2].loc[3] * SCALPR2

    #       Gulf Coast - Florida                                    T76(50,IY,IS)=PUSNG(4,IY+BASEYR-1)+PTRANSNG(2,4,IY+BASEYR-1)
    z[50] = z[34] + dfd["PTRANSNG"].loc[2].loc[4] * SCALPR2

    #       Gulf Coast - Alabama and Mississippi                    T76(51,IY,IS)=PUSNG(5,IY+BASEYR-1)+PTRANSNG(2,5,IY+BASEYR-1)
    z[51] = z[35] + dfd["PTRANSNG"].loc[2].loc[5] * SCALPR2

    #       Gulf Coast - Texas and Louisiana                        T76(52,IY,IS)=PUSNG(6,IY+BASEYR-1)+PTRANSNG(2,6,IY+BASEYR-1)
    z[52] = z[36] + dfd["PTRANSNG"].loc[2].loc[6] * SCALPR2

    #       West Coast - Washington and Oregon                      T76(53,IY,IS)=PUSNG(7,IY+BASEYR-1)+PTRANSNG(2,7,IY+BASEYR-1)
    z[53] = z[37] + dfd["PTRANSNG"].loc[2].loc[7] * SCALPR2

    #       West Coast - California                                 T76(54,IY,IS)=PUSNG(8,IY+BASEYR-1)+PTRANSNG(2,8,IY+BASEYR-1)
    z[54] = z[38] + dfd["PTRANSNG"].loc[2].loc[8] * SCALPR2

    return z
