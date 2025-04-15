# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM

SZO rewrote on 8/31/2024

"""


def fill_table_base_068(dfd, table_spec, table_id):
    """Fill table for  Domestic Refinery Distillation Base Capacity, Expansion, and Utilization

    The function returns a dict in which each key is an integer references a table row, and each
    value is a dataframe indexed by region number. The integer keys are the same as "IROWS" in layin

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

    # FYRPRC = int(table_spec['year_index_real_dol_priceyr'])
    FYRPRC = int(table_spec["ftab_dat"]["FYRPRC"])

    # Get inflation index from 1990->2050
    MC_JPGDP = dfd["MC_JPGDP"].loc[1990:]

    # Get inflation index of FYRPRC (2022)
    SCALPR2 = MC_JPGDP.loc[FYRPRC]  # 2022 = column 33

    MNUMPR = int(dfd["MNUMPR_rwpre"])
    CUMCAPADD = int(table_spec["settings"]["year_cum_oilgas_prod_begin"]) - 1

    NUMREP = 5  # equals number of rows per PADD

    for PADD in range(1, MNUMPR + 1):

        # T68(((PADD-1)*NUMREP)+1,IY,IS) = REF_CAP(1,PADD,CUMCAPADD-1989) / 1000.
        z[(PADD - 1) * NUMREP + 1] = dfd["REF_CAP"].loc[1, CUMCAPADD] / 1000.0

        # T68(((PADD-1)*NUMREP)+2,IY,IS) =(REF_CAP(1,PADD,IY) - REF_CAP(1,PADD,CUMCAPADD-1989))/1000.
        z[(PADD - 1) * NUMREP + 2] = (
            dfd["REF_CAP"].loc[1].loc[PADD].astype(float)
            - dfd["REF_CAP"].loc[1].loc[PADD][CUMCAPADD]
        ) / 1000.0

        # T68(((PADD-1)*NUMREP)+3,IY,IS) = REF_CAP(1,PADD,IY) * (1-REF_UTL(1,PADD,IY)) / 1000.
        z[(PADD - 1) * NUMREP + 3] = (
            dfd["REF_CAP"].loc[1].loc[PADD].astype(float)
            * (1 - dfd["REF_UTL"].loc[1].loc[PADD])
        ) / 1000.0

        # T68(((PADD-1)*NUMREP)+4,IY,IS) = REF_CAP(1,PADD,IY) / 1000.
        z[(PADD - 1) * NUMREP + 4] = dfd["REF_CAP"].loc[1].loc[PADD] / 1000

        # T68(((PADD-1)*NUMREP)+5,IY,IS) = REF_UTL(1,PADD,IY) * 100.
        z[(PADD - 1) * NUMREP + 5] = dfd["REF_UTL"].loc[1].loc[PADD] * 100

        # T68(PADD+50,IY,IS) = PROFIT_BBL(PADD,IY)
        z[PADD + 50] = dfd["PROFIT_BBL"].loc[PADD] * SCALPR2  # in FYRPRC $

    return z


# # NUMREP=5                       ! equals number of rows per PADD
# # CUMCAPADD is read from ftab.dat
# # year_cum_cap_addition_begin = 2022 in config.ini
# # need to grab correct year data
#     CUMCAPADD=2022

# #         DO IY = 1,LASTYR
# #           DO PADD=1,MNUMPR
# #             T68(((PADD-1)*NUMREP)+1,IY,IS) = REF_CAP(1,PADD,CUMCAPADD-1989) / 1000.
# #             T68(((PADD-1)*NUMREP)+2,IY,IS) =(REF_CAP(1,PADD,IY) - REF_CAP(1,PADD,CUMCAPADD-1989))/ 1000.
# #             T68(((PADD-1)*NUMREP)+3,IY,IS) = REF_CAP(1,PADD,IY) * (1-REF_UTL(1,PADD,IY)) / 1000.
# #             T68(((PADD-1)*NUMREP)+4,IY,IS) = REF_CAP(1,PADD,IY) / 1000.
# #             T68(((PADD-1)*NUMREP)+5,IY,IS) = REF_UTL(1,PADD,IY) * 100.
# #             T68(PADD+50,IY,IS) = PROFIT_BBL(PADD,IY)
# #           ENDDO
# #         ENDDO

#     #   Domestic Refinery Distillation Base Capacity, Expansion, and Utilization
#     #   (million barrels per day)
#     #    Region
#     #   Region 1 - PADD I
#     #     Base Capacity                                             T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[1] = dfd['REF_CAP'].loc[1].loc[1]/1000.

#     #     Capacity Additions                                        T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[2] = (dfd['REF_CAP'].loc[1].loc[1]-dfd['REF_CAP'].loc[1].loc[1])/1000.

#     #     Idled Capacity                                            T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[3] = dfd['REF_CAP'].loc[1].loc[1]*(1-dfd['REF_UTL'].loc[1].loc[1])/1000.

#     #     Total Capacity                                            T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[4] = dfd['REF_CAP'].loc[1].loc[1]/1000

#     #     Utilization                                               T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[5] = dfd['REF_UTL'].loc[1].loc[1]*100

#     #
#     #     Incremental Gross Profitability                           T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[51] = dfd['PROFIT_BBL'].loc[1]

#     #
#     #   Region 2 - PADD II Inland
#     #     Base Capacity                                             T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     #t68(6,iy,is)=REF_CAP(1,2,CUMCAPADD-1989) / 1000.
#     #note - need 2022 not current year
#     z[6] = dfd['REF_CAP'].loc[1].loc[2]/1000.

#     #     Capacity Additions                                        T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     #T68(7,IY,IS) =(REF_CAP(1,PADD,IY) - REF_CAP(1,PADD,CUMCAPADD-1989))/ 1000.
#     z[7] = (dfd['REF_CAP'].loc[1].loc[2]-dfd['REF_CAP'].loc[1].loc[2])/1000.

#     #     Idled Capacity                                            T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[8] = dfd['REF_CAP'].loc[1].loc[2]*(1-dfd['REF_UTL'].loc[1].loc[2])/1000.

#     #     Total Capacity                                            T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[9] = dfd['REF_CAP'].loc[1].loc[2]/1000

#     #     Utilization                                               T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[10] = dfd['REF_UTL'].loc[1].loc[2]*100

#     #
#     #     Incremental Gross Profitability                           T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[52] = dfd['PROFIT_BBL'].loc[2]

#     #
#     #   Region 3 - PADD II Lakes
#     #     Base Capacity                                             T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[11] = dfd['REF_CAP'].loc[1].loc[2]/1000.

#     #     Capacity Additions                                        T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[12] = (dfd['REF_CAP'].loc[1].loc[3]-dfd['REF_CAP'].loc[1].loc[3])/1000.

#     #     Idled Capacity                                            T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[13] = dfd['REF_CAP'].loc[1].loc[3]*(1-dfd['REF_UTL'].loc[1].loc[3])/1000.


#     #     Total Capacity                                            T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[14] = dfd['REF_CAP'].loc[1].loc[3]/1000

#     #     Utilization                                               T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[15] = dfd['REF_UTL'].loc[1].loc[3]*100

#     #
#     #     Incremental Gross Profitability                           T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[53] = dfd['PROFIT_BBL'].loc[3]

#     #
#     #   Region 4 - PADD III Gulf
#     #     Base Capacity                                             T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[16] = dfd['REF_CAP'].loc[1].loc[4]/1000.

#     #     Capacity Additions                                        T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[17] = (dfd['REF_CAP'].loc[1].loc[4]-dfd['REF_CAP'].loc[1].loc[4])/1000.

#     #     Idled Capacity                                            T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[18] = dfd['REF_CAP'].loc[1].loc[4]*(1-dfd['REF_UTL'].loc[1].loc[4])/1000.

#     #     Total Capacity                                            T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[19] = dfd['REF_CAP'].loc[1].loc[4]/1000

#     #     Utilization                                               T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[20] = dfd['REF_UTL'].loc[1].loc[4]*100

#     #
#     #     Incremental Gross Profitability                           T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[54] = dfd['PROFIT_BBL'].loc[4]

#     #
#     #   Region 5 - PADD III Inland
#     #     Base Capacity                                             T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[21] = dfd['REF_CAP'].loc[1].loc[5]/1000.

#     #     Capacity Additions                                        T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[22] = (dfd['REF_CAP'].loc[1].loc[5]-dfd['REF_CAP'].loc[1].loc[5])/1000.

#     #     Idled Capacity                                            T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[23] = dfd['REF_CAP'].loc[1].loc[5]*(1-dfd['REF_UTL'].loc[1].loc[5])/1000.

#     #     Total Capacity                                            T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[24] =  dfd['REF_CAP'].loc[1].loc[5]/1000

#     #     Utilization                                               T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[25] = dfd['REF_UTL'].loc[1].loc[5]*100

#     #
#     #     Incremental Gross Profitability                           T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[55] = dfd['PROFIT_BBL'].loc[5]

#     #
#     #
#     #
#     #   Region 6 - PADD IV

#     z[26] = dfd['REF_CAP'].loc[1].loc[6]/1000.

#     #     Capacity Additions                                        T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[27] = (dfd['REF_CAP'].loc[1].loc[6]-dfd['REF_CAP'].loc[1].loc[6])/1000.

#     #     Idled Capacity                                            T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[28] = dfd['REF_CAP'].loc[1].loc[6]*(1-dfd['REF_UTL'].loc[1].loc[6])/1000.

#     #     Total Capacity                                            T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[29] =  dfd['REF_CAP'].loc[1].loc[6]/1000

#     #     Utilization                                               T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[30] = dfd['REF_UTL'].loc[1].loc[6]*100

#     #
#     #     Incremental Gross Profitability                           T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[55] = dfd['PROFIT_BBL'].loc[6]

#     #
#     #     Incremental Gross Profitability                           T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[56] = dfd['PROFIT_BBL'].loc[6]

#     #
#     #   Region 7 - PADD V California
#     #     Base Capacity                                             T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[31] = dfd['REF_CAP'].loc[1].loc[7]/1000.

#     #     Capacity Additions                                        T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[32] = (dfd['REF_CAP'].loc[1].loc[7]-dfd['REF_CAP'].loc[1].loc[7])/1000.

#     #     Idled Capacity                                            T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[33] = dfd['REF_CAP'].loc[1].loc[7]*(1-dfd['REF_UTL'].loc[1].loc[7])/1000.

#     #     Total Capacity                                            T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[34] =  dfd['REF_CAP'].loc[1].loc[7]/1000

#     #     Utilization                                               T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[35] = dfd['REF_UTL'].loc[1].loc[7]*100

#     #
#     #     Incremental Gross Profitability                           T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[57] = dfd['PROFIT_BBL'].loc[7]


#     #
#     #   Region 8 - PADD V Other
#     #     Base Capacity                                             T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[36] = dfd['REF_CAP'].loc[1].loc[8]/1000.

#     #     Capacity Additions                                        T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[37] =(dfd['REF_CAP'].loc[1].loc[8]-dfd['REF_CAP'].loc[1].loc[8])/1000.

#     #     Idled Capacity                                            T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[38] = dfd['REF_CAP'].loc[1].loc[8]*(1-dfd['REF_UTL'].loc[1].loc[8])/1000.

#     #     Total Capacity                                            T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[39] = dfd['REF_CAP'].loc[1].loc[8]/1000

#     #     Utilization                                               T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[40] = dfd['REF_UTL'].loc[1].loc[8]*100

#     #
#     #     Incremental Gross Profitability                           T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[58] = dfd['PROFIT_BBL'].loc[8]

#     #
#     #   Region 9 - Maritime Canada and Caribbean
#     #     Base Capacity                                             T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[41] = dfd['REF_CAP'].loc[1].loc[9]/1000.

#     #     Capacity Additions                                        T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[42] = (dfd['REF_CAP'].loc[1].loc[9]-dfd['REF_CAP'].loc[1].loc[9])/1000.

#     #     Idled Capacity                                            T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[43] = dfd['REF_CAP'].loc[1].loc[9]*(1-dfd['REF_UTL'].loc[1].loc[9])/1000.

#     #     Total Capacity                                            T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[44] = dfd['REF_CAP'].loc[1].loc[9]/1000

#     #     Utilization                                               T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[45] = dfd['REF_UTL'].loc[1].loc[9]*100

#     #
#     #     Incremental Gross Profitability                           T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[59] = dfd['PROFIT_BBL'].loc[9]

#     #
#     #   United States
#     #     Base Capacity                                             T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[46] = dfd['REF_CAP'].loc[1].loc[10]/1000.

#     #     Capacity Additions                                        T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[47] =  (dfd['REF_CAP'].loc[1].loc[10]-dfd['REF_CAP'].loc[1].loc[10])/1000.

#     #     Idled Capacity                                            T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[48] = dfd['REF_CAP'].loc[1].loc[10]*(1-dfd['REF_UTL'].loc[1].loc[10])/1000.

#     #     Total Capacity                                            T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[49] = dfd['REF_CAP'].loc[1].loc[10]/1000

#     #     Utilization                                               T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[50] = dfd['REF_UTL'].loc[1].loc[10]*100

#     #
#     #     Incremental Gross Profitability                           T68(((PADD-1)*NUMREP)+3,IY,IS)=REF_CAP(1,PADD,IY)*(1-REF_UTL(1,PADD,IY))/1000.
#     z[60] = dfd['PROFIT_BBL'].loc[10]

#     return z
