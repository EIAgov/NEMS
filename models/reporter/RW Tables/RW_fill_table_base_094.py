# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
SZO fixed on 8/9/2024
"""


def fill_table_base_094(dfd, table_spec, table_id):
    """Fill table  Coal Production and Minemouth Prices by Region

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

    """

    z = {}

    FYRPRC = int(table_spec["settings"]["year_index_real_dol_priceyr"])
    # Get inflation index from 1990->2050
    MC_JPGDP = dfd["MC_JPGDP"].loc[1990:]

    # Get inflation index of FYRPRC (2022)
    SCALPR2 = MC_JPGDP.loc[FYRPRC]  # 2022 = column 33

    #   Coal Production and Minemouth Prices by Region
    #   (production in million short tons; prices in #### dollars pe
    #    Supply Regions
    #
    #   Production (million short tons)
    #
    #     Appalachia 1/
    # T94(1,IY,IS)=APPSTOCKS(IY)+ABSULF(4,IY)+APSULF(4,IY)
    z[1] = dfd["APPSTOCKS"] + dfd["ABSULF"].loc[4] + dfd["APSULF"].loc[4]

    #     Interior 2/
    # T94(2,IY,IS)=INTSTOCKS(IY)+IBSULF(4,IY)+ILSULF(4,IY)
    z[2] = dfd["INTSTOCKS"] + dfd["IBSULF"].loc[4] + dfd["ILSULF"].loc[4]

    #     West
    # T94(7,IY,IS)=WESTSTOCKS(IY)+WBSULF(4,IY)+WSSULF(4,IY)+WLSULF(4,IY)+WPSULF(4,IY)
    # T94(7,IY,IS)=WESTSTOCKS(IY)+WBSULF(4,IY)+WSSULF(4,IY)+WLSULF(4,IY) AEO2025 Change
    z[7] = (
        dfd["WESTSTOCKS"]
        + dfd["WBSULF"].loc[4]
        + dfd["WSSULF"].loc[4]
        + dfd["WLSULF"].loc[4]
    )

    #       Northern Great Plains 3/
    # T94(3,IY,IS)=WESTSTOCKS(IY)+CLSULF(7,3,2,IY)+CLSULF(8,1,1,IY)+CLSULF(8,2,1,IY)+CLSULF(8,2,2,IY)+CLSULF(9,2,1,IY)+CLSULF(10,2,1,IY)+CLSULF(9,2,2,IY)+CLSULF(11,1,1,IY)+CLSULF(11,2,1,IY)+CLSULF(11,2,2,IY)
    z[3] = (
        dfd["WESTSTOCKS"]
        + dfd["CLSULF"].loc[7].loc[3].loc[2]
        + dfd["CLSULF"].loc[8].loc[1].loc[1]
        + dfd["CLSULF"].loc[8].loc[2].loc[1]
        + dfd["CLSULF"].loc[8].loc[2].loc[2]
        + dfd["CLSULF"].loc[9].loc[2].loc[1]
        + dfd["CLSULF"].loc[10].loc[2].loc[1]
        + dfd["CLSULF"].loc[9].loc[2].loc[2]
        + dfd["CLSULF"].loc[11].loc[1].loc[1]
        + dfd["CLSULF"].loc[11].loc[2].loc[1]
        + dfd["CLSULF"].loc[11].loc[2].loc[2]
    )

    #       Other West and Non-Contiguous 4/
    # T94(4,IY,IS)=CLSULF(12,4,1,IY)+CLSULF(12,1,1,IY)+CLSULF(12,2,1,IY)+CLSULF(13,1,1,IY)+CLSULF(13,1,2,IY)+CLSULF(13,2,2,IY)+CLSULF(14,2,1,IY)
    z[4] = (
        dfd["CLSULF"].loc[12].loc[4].loc[1]
        + dfd["CLSULF"].loc[12].loc[1].loc[1]
        + dfd["CLSULF"].loc[12].loc[2].loc[1]
        + dfd["CLSULF"].loc[13].loc[1].loc[1]
        + dfd["CLSULF"].loc[13].loc[1].loc[2]
        + dfd["CLSULF"].loc[13].loc[2].loc[2]
        + dfd["CLSULF"].loc[14].loc[2].loc[1]
    )

    #     East of Mississippi River
    # T94(8,IY,IS)=APPSTOCKS(IY)+INTSTOCKS(IY)+CQSBB(1,IY)/CQSBT(1,IY)
    z[8] = (
        dfd["APPSTOCKS"] + dfd["INTSTOCKS"] + dfd["CQSBB"].loc[1] / dfd["CQSBT"].loc[1]
    )

    #     West of Mississippi River
    # T94(9,IY,IS)=WESTSTOCKS(IY)+CQSBB(2,IY)/CQSBT(2,IY)
    z[9] = dfd["WESTSTOCKS"] + dfd["CQSBB"].loc[2] / dfd["CQSBT"].loc[2]

    #   Total Production
    # T94(10,IY,IS)=T94(8,IY,IS)+T94(9,IY,IS)
    z[10] = z[8] + z[9]

    #   Minemouth Prices 5/
    #   (#### dollars per short ton)
    #     Appalachia 1/
    # T94(11,IY,IS)=(PABSULF(4,IY)+PAPSULF(4,IY))/(ABSULF(4,IY)+APSULF(4,IY))
    z[11] = (
        (dfd["PABSULF"].loc[4] + dfd["PAPSULF"].loc[4])
        / (dfd["ABSULF"].loc[4] + dfd["APSULF"].loc[4])
    ) * SCALPR2

    #     Interior 2/
    # T94(12,IY,IS)=(PIBSULF(4,IY)+PILSULF(4,IY))/(IBSULF(4,IY)+ILSULF(4,IY))
    z[12] = (
        (dfd["PIBSULF"].loc[4] + dfd["PILSULF"].loc[4])
        / (dfd["IBSULF"].loc[4] + dfd["ILSULF"].loc[4])
    ) * SCALPR2

    #     West
    # T94(17,IY,IS)=(PWBSULF(4,IY)+PWSSULF(4,IY)+PWLSULF(4,IY)+PWPSULF(4,IY))/(WBSULF(4,IY)+WSSULF(4,IY)+WLSULF(4,IY)+WPSULF(4,IY))
    # T94(17,IY,IS)=(PWBSULF(4,IY)+PWSSULF(4,IY)+PWLSULF(4,IY))/(WBSULF(4,IY)+WSSULF(4,IY)+WLSULF(4,IY)) AEO2025 Change
    z[17] = (
        (
            dfd["PWBSULF"].loc[4]
            + dfd["PWSSULF"].loc[4]
        )
        / (
            dfd["WBSULF"].loc[4]
            + dfd["WSSULF"].loc[4]
        )
    ) * SCALPR2

    #       Northern Great Plains 3/
    # T94(13,IY,IS)=(PCLSULF(7,3,2,IY)+PCLSULF(8,1,1,IY)+PCLSULF(8,2,1,IY)+PCLSULF(8,2,2,IY)+PCLSULF(9,2,1,IY)+PCLSULF(10,2,1,IY)+PCLSULF(9,2,2,IY)+PCLSULF(11,1,1,IY)+PCLSULF(11,2,1,IY)+PCLSULF(11,2,2,IY))
    # /(CLSULF(7,3,2,IY)+CLSULF(8,1,1,IY)+CLSULF(8,2,1,IY)+CLSULF(8,2,2,IY)+CLSULF(9,2,1,IY)+CLSULF(10,2,1,IY)+CLSULF(9,2,2,IY)+CLSULF(11,1,1,IY)+CLSULF(11,2,1,IY)+CLSULF(11,2,2,IY))

    z[13] = (
        (
            dfd["PCLSULF"].loc[7, 3, 2]
            + dfd["PCLSULF"].loc[8, 1, 1]
            + dfd["PCLSULF"].loc[8, 2, 1]
            + dfd["PCLSULF"].loc[8, 2, 2]
            + dfd["PCLSULF"].loc[9, 2, 1]
            + dfd["PCLSULF"].loc[10, 2, 1]
            + dfd["PCLSULF"].loc[9, 2, 2]
            + dfd["PCLSULF"].loc[11, 1, 1]
            + dfd["PCLSULF"].loc[11, 2, 1]
            + dfd["PCLSULF"].loc[11, 2, 2]
        )
        / (
            dfd["CLSULF"].loc[7, 3, 2]
            + dfd["CLSULF"].loc[8, 1, 1]
            + dfd["CLSULF"].loc[8, 2, 1]
            + dfd["CLSULF"].loc[8, 2, 2]
            + dfd["CLSULF"].loc[9, 2, 1]
            + dfd["CLSULF"].loc[10, 2, 1]
            + dfd["CLSULF"].loc[9, 2, 2]
            + dfd["CLSULF"].loc[11, 1, 1]
            + dfd["CLSULF"].loc[11, 2, 1]
            + dfd["CLSULF"].loc[11, 2, 2]
        ).replace(0, 1)
        * SCALPR2
    )

    #     Northern Great Plains 3/
    # T94(14,IY,IS) = (PCLSULF(12,4,1,IY) + PCLSULF(12,1,1,IY) + PCLSULF(12,2,1,IY) + &
    # PCLSULF(13,1,1,IY) + PCLSULF(13,1,2,IY) + &
    # PCLSULF(13,2,2,IY) + PCLSULF(14,2,1,IY)) / &
    # (CLSULF(12,4,1,IY) + CLSULF(12,1,1,IY) + CLSULF(12,2,1,IY) + CLSULF(13,1,1,IY) + &
    # CLSULF(13,1,2,IY) + CLSULF(13,2,2,IY) + CLSULF(14,2,1,IY))

    z[14] = (
        (
            dfd["PCLSULF"].loc[12, 4, 1]
            + dfd["PCLSULF"].loc[12, 1, 1]
            + dfd["PCLSULF"].loc[12, 2, 1]
            + dfd["PCLSULF"].loc[13, 1, 1]
            + dfd["PCLSULF"].loc[13, 1, 2]
            + dfd["PCLSULF"].loc[13, 2, 2]
            + dfd["PCLSULF"].loc[14, 2, 1]
        )
        / (
            dfd["CLSULF"].loc[12, 4, 1]
            + dfd["CLSULF"].loc[12, 1, 1]
            + dfd["CLSULF"].loc[12, 2, 1]
            + dfd["CLSULF"].loc[13, 1, 1]
            + dfd["CLSULF"].loc[13, 1, 2]
            + dfd["CLSULF"].loc[13, 2, 2]
            + dfd["CLSULF"].loc[14, 2, 1]
        ).replace(0, 1)
        * SCALPR2
    )
    #   Other West and Non-Contiguous 4/
    # T94(17,IY,IS) = (PWBSULF(4,IY) + PWSSULF(4,IY) + PWLSULF(4,IY) + PWPSULF(4,IY)) / &
    #                 ( WBSULF(4,IY) +  WSSULF(4,IY) +  WLSULF(4,IY) +  WPSULF(4,IY))
    z[17] = (
        (
            dfd["PWBSULF"].loc[4]
            + dfd["PWSSULF"].loc[4]
        )
        / (
            dfd["WBSULF"].loc[4]
            + dfd["WSSULF"].loc[4]
        ).replace(0, 1)
        * SCALPR2
    )
    #     East of Mississippi River
    # T94(18,IY,IS)=(PABSULF(4,IY)+PAPSULF(4,IY)+PCLSULF(4,3,2,IY)+PCLSULF(4,1,2,IY)+PCLSULF(4,1,3,IY))/(ABSULF(4,IY)+APSULF(4,IY)+CLSULF(4,3,2,IY)+CLSULF(4,1,2,IY)+CLSULF(4,1,3,IY))
    z[18] = (
        (
            dfd["PABSULF"].loc[4]
            + dfd["PAPSULF"].loc[4]
            + dfd["PIBSULF"].loc[4]
            + dfd["PILSULF"].loc[4]
        )
        / (
            dfd["ABSULF"].loc[4]
            + dfd["APSULF"].loc[4]
            + dfd["IBSULF"].loc[4]
            + dfd["ILSULF"].loc[4]
        ).replace(0, 1)
    ) * SCALPR2

    #     West of Mississippi River
    # T94(19,IY,IS) = (PCLSULF( 5,1,2,IY) + PCLSULF( 5,1,3,IY) + PCLSULF( 6,3,2,IY) + &
    # PCLSULF( 6,3,3,IY) + PCLSULF( 7,3,2,IY) + PCLSULF( 8,1,1,IY) + &
    # PCLSULF( 8,2,1,IY) + PCLSULF( 8,2,2,IY) + PCLSULF( 9,2,1,IY) + &
    # PCLSULF(10,2,1,IY) + PCLSULF( 9,2,2,IY) + PCLSULF(11,1,1,IY) + &
    # PCLSULF(11,2,1,IY) + PCLSULF(11,2,2,IY) + PCLSULF(12,4,1,IY) + &
    # PCLSULF(12,1,1,IY) + PCLSULF(12,2,1,IY) + PCLSULF(13,1,1,IY) + &
    # PCLSULF(13,1,2,IY) + PCLSULF(13,2,2,IY) + PCLSULF(14,2,1,IY)) / &

    # (CLSULF( 5,1,2,IY) + CLSULF( 5,1,3,IY) + CLSULF( 6,3,2,IY) + CLSULF( 6,3,3,IY) + &
    # CLSULF( 7,3,2,IY) + CLSULF( 8,1,1,IY) + CLSULF( 8,2,1,IY) + &
    # CLSULF( 8,2,2,IY) + CLSULF( 9,2,1,IY) + CLSULF(10,2,1,IY) + CLSULF( 9,2,2,IY) + &
    # CLSULF(11,1,1,IY) + CLSULF(11,2,1,IY) + CLSULF(11,2,2,IY) + &
    # CLSULF(12,4,1,IY) + CLSULF(12,1,1,IY) + CLSULF(12,2,1,IY) + CLSULF(13,1,1,IY) + &
    # CLSULF(13,1,2,IY) + CLSULF(13,2,2,IY) + CLSULF(14,2,1,IY))

    z[19] = (
        (
            dfd["PWBSULF"].loc[4]
            + dfd["PWSSULF"].loc[4]
        )
        / (
            dfd["WBSULF"].loc[4]
            + dfd["WSSULF"].loc[4]
        ).replace(0, 1)
    ) * SCALPR2
    #   Average Minemouth Price
    # T94(20,IY,IS)=(PABSULF(4,IY)+PIBSULF(4,IY)+PILSULF(4,IY)+PWPSULF(4,IY)+PWBSULF(4,IY)+PWSSULF(4,IY)+PWLSULF(4,IY)+PAPSULF(4,IY))/(ABSULF(4,IY)+IBSULF(4,IY)+ILSULF(4,IY)+WPSULF(4,IY)+WBSULF(4,IY)+WSSULF(4,IY)+WLSULF(4,IY)+APSULF(4,IY))
    z[20] = (
        (
            dfd["PABSULF"].loc[4]
            + dfd["PIBSULF"].loc[4]
            + dfd["PILSULF"].loc[4]
            + dfd["PWBSULF"].loc[4]
            + dfd["PWSSULF"].loc[4]
            + dfd["PAPSULF"].loc[4]
        )
        / (
            dfd["ABSULF"].loc[4]
            + dfd["IBSULF"].loc[4]
            + dfd["ILSULF"].loc[4]
            + dfd["WBSULF"].loc[4]
            + dfd["WSSULF"].loc[4]
            + dfd["APSULF"].loc[4]
        ).replace(0, 1)
    ) * SCALPR2

    return z
