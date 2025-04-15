# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_149(dfd, table_spec, table_id):
    """Fill table for Energy Prices for FEMP by Census Region

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

    MNUMCR = dfd["MNUMCR_rwpre"]
    TRIL_TO_QUAD = dfd["TRIL_TO_QUAD_rwpre"]
    dfd["APMORE/PPRRS"].loc[MNUMCR] = (
        (
            dfd["QPRRS"].loc[1] * dfd["APMORE/PPRRS"].loc[1]
            + dfd["QPRRS"].loc[2] * dfd["APMORE/PPRRS"].loc[2]
            + dfd["QPRRS"].loc[3] * dfd["APMORE/PPRRS"].loc[3]
            + dfd["QPRRS"].loc[4] * dfd["APMORE/PPRRS"].loc[4]
            + dfd["QPRRS"].loc[5] * dfd["APMORE/PPRRS"].loc[5]
            + dfd["QPRRS"].loc[6] * dfd["APMORE/PPRRS"].loc[6]
            + dfd["QPRRS"].loc[7] * dfd["APMORE/PPRRS"].loc[7]
            + dfd["QPRRS"].loc[8] * dfd["APMORE/PPRRS"].loc[8]
            + dfd["QPRRS"].loc[9] * dfd["APMORE/PPRRS"].loc[9]
        )
        / dfd["QPRRS"].loc[1:9, :].sum()
    ).fillna(dfd["APMORE/PPRRS"].loc[1:9, :].sum() / 9.0)
    dfd["APMORE/PPRCM"].loc[MNUMCR] = (
        (
            dfd["QPRCM"].loc[1] * dfd["APMORE/PPRCM"].loc[1]
            + dfd["QPRCM"].loc[2] * dfd["APMORE/PPRCM"].loc[2]
            + dfd["QPRCM"].loc[3] * dfd["APMORE/PPRCM"].loc[3]
            + dfd["QPRCM"].loc[4] * dfd["APMORE/PPRCM"].loc[4]
            + dfd["QPRCM"].loc[5] * dfd["APMORE/PPRCM"].loc[5]
            + dfd["QPRCM"].loc[6] * dfd["APMORE/PPRCM"].loc[6]
            + dfd["QPRCM"].loc[7] * dfd["APMORE/PPRCM"].loc[7]
            + dfd["QPRCM"].loc[8] * dfd["APMORE/PPRCM"].loc[8]
            + dfd["QPRCM"].loc[9] * dfd["APMORE/PPRCM"].loc[9]
        )
        / dfd["QPRCM"].loc[1:9, :].sum()
    ).fillna(dfd["APMORE/PPRCM"].loc[1:9, :].sum() / 9.0)
    dfd["APMORE/PPRIN"].loc[MNUMCR] = (
        (
            dfd["QPRIN"].loc[1] * dfd["APMORE/PPRIN"].loc[1]
            + dfd["QPRIN"].loc[2] * dfd["APMORE/PPRIN"].loc[2]
            + dfd["QPRIN"].loc[3] * dfd["APMORE/PPRIN"].loc[3]
            + dfd["QPRIN"].loc[4] * dfd["APMORE/PPRIN"].loc[4]
            + dfd["QPRIN"].loc[5] * dfd["APMORE/PPRIN"].loc[5]
            + dfd["QPRIN"].loc[6] * dfd["APMORE/PPRIN"].loc[6]
            + dfd["QPRIN"].loc[7] * dfd["APMORE/PPRIN"].loc[7]
            + dfd["QPRIN"].loc[8] * dfd["APMORE/PPRIN"].loc[8]
            + dfd["QPRIN"].loc[9] * dfd["APMORE/PPRIN"].loc[9]
        )
        / dfd["QPRIN"].loc[1:9, :].sum()
    ).fillna(dfd["APMORE/PPRIN"].loc[1:9, :].sum() / 9.0)

    #   Energy Prices for FEMP by Census Region
    #   (#### dollars per million Btu)
    #    Prices

    #   West Texas Intermediate Spot Price
    #      in #### dollars per barrel

    # T149(1,IY,IS)=BRENT_PRICE(IY)
    z[1] = dfd["BRENT_PRICE"] * SCALPR2

    #      Conversion Factor
    # T149(89,IY,IS)=CFCRDLTSWT(IY)
    z[89] = dfd["CFCRDLTSWT"]

    #      in #### dollars per million Btu
    # T149(90,IY,IS)=BRENT_PRICE(IY)/T149(89,IY,IS)
    z[90] = dfd["BRENT_PRICE"] / z[89] * SCALPR2

    #   Natural Spot Price at Henry Hub
    #      in #### dollars per thousand cubic feet

    # T149(2,IY,IS)=OGHHPRNG(IY)*CFNGC(IY)
    z[2] = dfd["OGHHPRNG"] * dfd["CFNGC"] * SCALPR2

    #      Conversion Factor
    # T149(91,IY,IS)=CFNGC(IY)
    z[91] = dfd["CFNGC"]

    #      in #### dollars per million Btu
    # T149(92,IY,IS)=OGHHPRNG(IY)
    z[92] = dfd["OGHHPRNG"] * SCALPR2

    #   Coal Minemouth Price

    #      in #### dollars per ton
    # T149(3,IY,IS)=CPSB(3,IY)*CPSBT(3,IY)
    z[3] = dfd["CPSB"].loc[3] * dfd["CPSBT"].loc[3] * SCALPR2

    #      Conversion Factor
    # T149(93,IY,IS)=CQSBT(3,IY)
    z[93] = dfd["CQSBT"].loc[3]

    #      in #### dollars per million Btu
    # T149(94,IY,IS)=CPSB(3,IY)
    z[94] = dfd["CPSB"].loc[3] * SCALPR2

    #

    #   Residential

    #      Northeast

    #         Distillate Fuel Oil
    # T149(4,IY,IS)=(dfd['PDSRS(1,IY)*QDSRS(1,IY)+dfd['PDSRS(2,IY)*QDSRS(2,IY))/(dfd['QDSRS(1,IY)+QDSRS(2,IY))
    z[4] = (
        (
            dfd["AMPBLK/PDSRS"].loc[1] * dfd["QDSRS"].loc[1]
            + dfd["AMPBLK/PDSRS"].loc[2] * dfd["QDSRS"].loc[2]
        )
        / (dfd["QDSRS"].loc[1] + dfd["QDSRS"].loc[2])
        * SCALPR2
    )

    #         Propane
    # T149(5,IY,IS)=(dfd['PPRRS(1,IY)*QPRRS(1,IY)+dfd['PPRRS(2,IY)*QPRRS(2,IY))/(dfd['QPRRS(1,IY)+QPRRS(2,IY))
    z[5] = (
        (
            dfd["APMORE/PPRRS"].loc[1] * dfd["QPRRS"].loc[1]
            + dfd["APMORE/PPRRS"].loc[2] * dfd["QPRRS"].loc[2]
        )
        / (dfd["QPRRS"].loc[1] + dfd["QPRRS"].loc[2])
        * SCALPR2
    )

    #         Natural Gas
    # T149(6,IY,IS)=(dfd['PNGRS(1,IY)*QNGRS(1,IY)+dfd['PNGRS(2,IY)*QNGRS(2,IY))/(dfd['QNGRS(1,IY)+QNGRS(2,IY))
    z[6] = (
        (
            dfd["AMPBLK/PNGRS"].loc[1] * dfd["QNGRS"].loc[1]
            + dfd["AMPBLK/PNGRS"].loc[2] * dfd["QNGRS"].loc[2]
        )
        / (dfd["QNGRS"].loc[1] + dfd["QNGRS"].loc[2])
        * SCALPR2
    )

    #         Electricity
    # T149(7,IY,IS)=(dfd['PELRS(1,IY)*QELRS(1,IY)+dfd['PELRS(2,IY)*QELRS(2,IY))/(dfd['QELRS(1,IY)+QELRS(2,IY))
    z[7] = (
        (
            dfd["AMPBLK/PELRS"].loc[1] * dfd["QELRS"].loc[1]
            + dfd["AMPBLK/PELRS"].loc[2] * dfd["QELRS"].loc[2]
        )
        / (dfd["QELRS"].loc[1] + dfd["QELRS"].loc[2])
        * SCALPR2
    )

    #      Midwest

    #         Distillate Fuel Oil
    # T149(8,IY,IS)=(dfd['PDSRS(3,IY)*QDSRS(3,IY)+dfd['PDSRS(4,IY)*QDSRS(4,IY))/(dfd['QDSRS(3,IY)+QDSRS(4,IY))
    z[8] = (
        (
            dfd["AMPBLK/PDSRS"].loc[3] * dfd["QDSRS"].loc[3]
            + dfd["AMPBLK/PDSRS"].loc[4] * dfd["QDSRS"].loc[4]
        )
        / (dfd["QDSRS"].loc[3] + dfd["QDSRS"].loc[4])
        * SCALPR2
    )

    #         Propane
    # T149(9,IY,IS)=(dfd['PPRRS(3,IY)*QPRRS(3,IY)+dfd['PPRRS(4,IY)*QPRRS(4,IY))/(dfd['QPRRS(3,IY)+QPRRS(4,IY))
    z[9] = (
        (
            dfd["APMORE/PPRRS"].loc[3] * dfd["QPRRS"].loc[3]
            + dfd["APMORE/PPRRS"].loc[4] * dfd["QPRRS"].loc[4]
        )
        / (dfd["QPRRS"].loc[3] + dfd["QPRRS"].loc[4])
        * SCALPR2
    )

    #         Natural Gas
    # T149(10,IY,IS)=(dfd['PNGRS(3,IY)*QNGRS(3,IY)+dfd['PNGRS(4,IY)*QNGRS(4,IY))/(dfd['QNGRS(3,IY)+QNGRS(4,IY))
    z[10] = (
        (
            dfd["AMPBLK/PNGRS"].loc[3] * dfd["QNGRS"].loc[3]
            + dfd["AMPBLK/PNGRS"].loc[4] * dfd["QNGRS"].loc[4]
        )
        / (dfd["QNGRS"].loc[3] + dfd["QNGRS"].loc[4])
        * SCALPR2
    )

    #         Electricity
    # T149(11,IY,IS)=(dfd['PELRS(3,IY)*QELRS(3,IY)+dfd['PELRS(4,IY)*QELRS(4,IY))/(dfd['QELRS(3,IY)+QELRS(4,IY))
    z[11] = (
        (
            dfd["AMPBLK/PELRS"].loc[3] * dfd["QELRS"].loc[3]
            + dfd["AMPBLK/PELRS"].loc[4] * dfd["QELRS"].loc[4]
        )
        / (dfd["QELRS"].loc[3] + dfd["QELRS"].loc[4])
        * SCALPR2
    )

    #      South

    #         Distillate Fuel Oil
    # T149(12,IY,IS)=(dfd['PDSRS(5,IY)*QDSRS(5,IY)+dfd['PDSRS(6,IY)*QDSRS(6,IY)+dfd['PDSRS(7,IY)*QDSRS(7,IY))/(dfd['QDSRS(5,IY)+QDSRS(6,IY)+QDSRS(7,IY))
    z[12] = (
        (
            dfd["AMPBLK/PDSRS"].loc[5] * dfd["QDSRS"].loc[5]
            + dfd["AMPBLK/PDSRS"].loc[6] * dfd["QDSRS"].loc[6]
            + dfd["AMPBLK/PDSRS"].loc[7] * dfd["QDSRS"].loc[7]
        )
        / (dfd["QDSRS"].loc[5] + dfd["QDSRS"].loc[6] + dfd["QDSRS"].loc[7])
        * SCALPR2
    )

    #         Propane
    # T149(13,IY,IS)=(dfd['PPRRS(5,IY)*QPRRS(5,IY)+dfd['PPRRS(6,IY)*QPRRS(6,IY)+dfd['PPRRS(7,IY)*QPRRS(7,IY))/(dfd['QPRRS(5,IY)+QPRRS(6,IY)+QPRRS(7,IY))
    z[13] = (
        (
            dfd["APMORE/PPRRS"].loc[5] * dfd["QPRRS"].loc[5]
            + dfd["APMORE/PPRRS"].loc[6] * dfd["QPRRS"].loc[6]
            + dfd["APMORE/PPRRS"].loc[7] * dfd["QPRRS"].loc[7]
        )
        / (dfd["QPRRS"].loc[5] + dfd["QPRRS"].loc[6] + dfd["QPRRS"].loc[7])
        * SCALPR2
    )

    #         Natural Gas
    # T149(14,IY,IS)=(dfd['PNGRS(5,IY)*QNGRS(5,IY)+dfd['PNGRS(6,IY)*QNGRS(6,IY)+dfd['PNGRS(7,IY)*QNGRS(7,IY))/(dfd['QNGRS(5,IY)+QNGRS(6,IY)+QNGRS(7,IY))
    z[14] = (
        (
            dfd["AMPBLK/PNGRS"].loc[5] * dfd["QNGRS"].loc[5]
            + dfd["AMPBLK/PNGRS"].loc[6] * dfd["QNGRS"].loc[6]
            + dfd["AMPBLK/PNGRS"].loc[7] * dfd["QNGRS"].loc[7]
        )
        / (dfd["QNGRS"].loc[5] + dfd["QNGRS"].loc[6] + dfd["QNGRS"].loc[7])
        * SCALPR2
    )

    #         Electricity
    # T149(15,IY,IS)=(dfd['PELRS(5,IY)*QELRS(5,IY)+dfd['PELRS(6,IY)*QELRS(6,IY)+dfd['PELRS(7,IY)*QELRS(7,IY))/(dfd['QELRS(5,IY)+QELRS(6,IY)+QELRS(7,IY))
    z[15] = (
        (
            dfd["AMPBLK/PELRS"].loc[5] * dfd["QELRS"].loc[5]
            + dfd["AMPBLK/PELRS"].loc[6] * dfd["QELRS"].loc[6]
            + dfd["AMPBLK/PELRS"].loc[7] * dfd["QELRS"].loc[7]
        )
        / (dfd["QELRS"].loc[5] + dfd["QELRS"].loc[6] + dfd["QELRS"].loc[7])
        * SCALPR2
    )

    #      West

    #         Distillate Fuel Oil
    # T149(16,IY,IS) =(PDSRS(8,IY)*QDSRS(8,IY)+PDSRS(9,IY)*QDSRS(9,IY))/(QDSRS(8,IY)+QDSRS(9,IY))
    z[16] = (
        (
            dfd["AMPBLK/PDSRS"].loc[8] * dfd["QDSRS"].loc[8]
            + dfd["AMPBLK/PDSRS"].loc[9] * dfd["QDSRS"].loc[9]
        )
        / (dfd["QDSRS"].loc[8] + dfd["QDSRS"].loc[9])
        * SCALPR2
    )

    #         Propane
    # T149(17,IY,IS)=(dfd['PPRRS(8,IY)*QPRRS(8,IY)+dfd['PPRRS(9,IY)*QPRRS(9,IY))/(dfd['QPRRS(8,IY)+QPRRS(9,IY))
    z[17] = (
        (
            dfd["APMORE/PPRRS"].loc[8] * dfd["QPRRS"].loc[8]
            + dfd["APMORE/PPRRS"].loc[9] * dfd["QPRRS"].loc[9]
        )
        / (dfd["QPRRS"].loc[8] + dfd["QPRRS"].loc[9])
        * SCALPR2
    )

    #         Natural Gas
    # T149(18,IY,IS)=(dfd['PNGRS(8,IY)*QNGRS(8,IY)+dfd['PNGRS(9,IY)*QNGRS(9,IY))/(dfd['QNGRS(8,IY)+QNGRS(9,IY))
    z[18] = (
        (
            dfd["AMPBLK/PNGRS"].loc[8] * dfd["QNGRS"].loc[8]
            + dfd["AMPBLK/PNGRS"].loc[9] * dfd["QNGRS"].loc[9]
        )
        / (dfd["QNGRS"].loc[8] + dfd["QNGRS"].loc[9])
        * SCALPR2
    )

    #         Electricity
    # T149(19,IY,IS)=(dfd['PELRS(8,IY)*QELRS(8,IY)+dfd['PELRS(9,IY)*QELRS(9,IY))/(dfd['QELRS(8,IY)+QELRS(9,IY))
    z[19] = (
        (
            dfd["AMPBLK/PELRS"].loc[8] * dfd["QELRS"].loc[8]
            + dfd["AMPBLK/PELRS"].loc[9] * dfd["QELRS"].loc[9]
        )
        / (dfd["QELRS"].loc[8] + dfd["QELRS"].loc[9])
        * SCALPR2
    )

    #      United States

    #         Distillate Fuel Oil
    # T149(20,IY,IS)=PDSRS(11,IY)
    z[20] = dfd["AMPBLK/PDSRS"].loc[MNUMCR] * SCALPR2

    #         Propane
    # T149(21,IY,IS)=PPRRS(11,IY)
    z[21] = dfd["APMORE/PPRRS"].loc[MNUMCR] * SCALPR2

    #         Natural Gas
    # T149(22,IY,IS)=PNGRS(11,IY)
    z[22] = dfd["AMPBLK/PNGRS"].loc[MNUMCR] * SCALPR2

    #         Electricity
    # T149(23,IY,IS)=PELRS(11,IY)
    z[23] = dfd["AMPBLK/PELRS"].loc[MNUMCR] * SCALPR2

    #

    #   Commercial

    #      Northeast

    #         Distillate Fuel Oil
    # T149(24,IY,IS)=(dfd['PDSCM(1,IY)*QDSCM(1,IY)+dfd['PDSCM(2,IY)*QDSCM(2,IY))/(dfd['QDSCM(1,IY)+QDSCM(2,IY))
    z[24] = (
        (
            dfd["AMPBLK/PDSCM"].loc[1] * dfd["QDSCM"].loc[1]
            + dfd["AMPBLK/PDSCM"].loc[2] * dfd["QDSCM"].loc[2]
        )
        / (dfd["QDSCM"].loc[1] + dfd["QDSCM"].loc[2])
        * SCALPR2
    )

    #         Propane
    # T149(95,IY,IS)=(dfd['PPRCM(1,IY)*QPRCM(1,IY)+dfd['PPRCM(2,IY)*QPRCM(2,IY))/(dfd['QPRCM(1,IY)+QPRCM(2,IY))
    z[95] = (
        (
            dfd["APMORE/PPRCM"].loc[1] * dfd["QPRCM"].loc[1]
            + dfd["APMORE/PPRCM"].loc[2] * dfd["QPRCM"].loc[2]
        )
        / (dfd["QPRCM"].loc[1] + dfd["QPRCM"].loc[2])
        * SCALPR2
    )

    #         Residual Fuel Oil
    # T149(25,IY,IS)=(dfd['PRSCM(1,IY)*QRSCM(1,IY)+dfd['PRSCM(2,IY)*QRSCM(2,IY))/(dfd['QRSCM(1,IY)+QRSCM(2,IY))
    z[25] = (
        (
            dfd["AMPBLK/PRSCM"].loc[1] * dfd["QRSCM"].loc[1]
            + dfd["AMPBLK/PRSCM"].loc[2] * dfd["QRSCM"].loc[2]
        )
        / (dfd["QRSCM"].loc[1] + dfd["QRSCM"].loc[2])
        * SCALPR2
    )

    #         Natural Gas
    # T149(26,IY,IS)=(dfd['PNGCM(1,IY)*QNGCM(1,IY)+dfd['PNGCM(2,IY)*QNGCM(2,IY))/(dfd['QNGCM(1,IY)+QNGCM(2,IY))
    z[26] = (
        (
            dfd["AMPBLK/PNGCM"].loc[1] * dfd["QNGCM"].loc[1]
            + dfd["AMPBLK/PNGCM"].loc[2] * dfd["QNGCM"].loc[2]
        )
        / (dfd["QNGCM"].loc[1] + dfd["QNGCM"].loc[2])
        * SCALPR2
    )

    #         Coal
    # T149(27,IY,IS)=(dfd['PCLCM(1,IY)*QCLCM(1,IY)+dfd['PCLCM(2,IY)*QCLCM(2,IY))/(dfd['QCLCM(1,IY)+QCLCM(2,IY))
    z[27] = (
        (
            dfd["AMPBLK/PCLCM"].loc[1] * dfd["QCLCM"].loc[1]
            + dfd["AMPBLK/PCLCM"].loc[2] * dfd["QCLCM"].loc[2]
        )
        / (dfd["QCLCM"].loc[1] + dfd["QCLCM"].loc[2])
        * SCALPR2
    )

    #         Electricity
    # T149(28,IY,IS)=(dfd['PELCM(1,IY)*QELCM(1,IY)+dfd['PELCM(2,IY)*QELCM(2,IY))/(dfd['QELCM(1,IY)+QELCM(2,IY))
    z[28] = (
        (
            dfd["AMPBLK/PELCM"].loc[1] * dfd["QELCM"].loc[1]
            + dfd["AMPBLK/PELCM"].loc[2] * dfd["QELCM"].loc[2]
        )
        / (dfd["QELCM"].loc[1] + dfd["QELCM"].loc[2])
        * SCALPR2
    )

    #      Midwest

    #         Distillate Fuel Oil
    # T149(29,IY,IS)=(dfd['PDSCM(3,IY)*QDSCM(3,IY)+dfd['PDSCM(4,IY)*QDSCM(4,IY))/(dfd['QDSCM(3,IY)+QDSCM(4,IY))
    z[29] = (
        (
            dfd["AMPBLK/PDSCM"].loc[3] * dfd["QDSCM"].loc[3]
            + dfd["AMPBLK/PDSCM"].loc[4] * dfd["QDSCM"].loc[4]
        )
        / (dfd["QDSCM"].loc[3] + dfd["QDSCM"].loc[4])
        * SCALPR2
    )

    #         Propane
    # T149(96,IY,IS)=(dfd['PPRCM(3,IY)*QPRCM(3,IY)+dfd['PPRCM(4,IY)*QPRCM(4,IY))/(dfd['QPRCM(3,IY)+QPRCM(4,IY))
    z[96] = (
        (
            dfd["APMORE/PPRCM"].loc[3] * dfd["QPRCM"].loc[3]
            + dfd["APMORE/PPRCM"].loc[4] * dfd["QPRCM"].loc[4]
        )
        / (dfd["QPRCM"].loc[3] + dfd["QPRCM"].loc[4])
        * SCALPR2
    )

    #         Residual Fuel Oil
    # T149(30,IY,IS)=(dfd['PRSCM(3,IY)*QRSCM(3,IY)+dfd['PRSCM(4,IY)*QRSCM(4,IY))/(dfd['QRSCM(3,IY)+QRSCM(4,IY))
    z[30] = (
        (
            dfd["AMPBLK/PRSCM"].loc[3] * dfd["QRSCM"].loc[3]
            + dfd["AMPBLK/PRSCM"].loc[4] * dfd["QRSCM"].loc[4]
        )
        / (dfd["QRSCM"].loc[3] + dfd["QRSCM"].loc[4])
        * SCALPR2
    )

    #         Natural Gas
    # T149(31,IY,IS)=(dfd['PNGCM(3,IY)*QNGCM(3,IY)+dfd['PNGCM(4,IY)*QNGCM(4,IY))/(dfd['QNGCM(3,IY)+QNGCM(4,IY))
    z[31] = (
        (
            dfd["AMPBLK/PNGCM"].loc[3] * dfd["QNGCM"].loc[3]
            + dfd["AMPBLK/PNGCM"].loc[4] * dfd["QNGCM"].loc[4]
        )
        / (dfd["QNGCM"].loc[3] + dfd["QNGCM"].loc[4])
        * SCALPR2
    )

    #         Coal
    # T149(32,IY,IS)=(dfd['PCLCM(3,IY)*QCLCM(3,IY)+dfd['PCLCM(4,IY)*QCLCM(4,IY))/(dfd['QCLCM(3,IY)+QCLCM(4,IY))
    z[32] = (
        (
            dfd["AMPBLK/PCLCM"].loc[3] * dfd["QCLCM"].loc[3]
            + dfd["AMPBLK/PCLCM"].loc[4] * dfd["QCLCM"].loc[4]
        )
        / (dfd["QCLCM"].loc[3] + dfd["QCLCM"].loc[4])
        * SCALPR2
    )

    #         Electricity
    # T149(33,IY,IS)=(dfd['PELCM(3,IY)*QELCM(3,IY)+dfd['PELCM(4,IY)*QELCM(4,IY))/(dfd['QELCM(3,IY)+QELCM(4,IY))
    z[33] = (
        (
            dfd["AMPBLK/PELCM"].loc[3] * dfd["QELCM"].loc[3]
            + dfd["AMPBLK/PELCM"].loc[4] * dfd["QELCM"].loc[4]
        )
        / (dfd["QELCM"].loc[3] + dfd["QELCM"].loc[4])
        * SCALPR2
    )

    #      South

    #         Distillate Fuel Oil
    # T149(34,IY,IS)=(dfd['PDSCM(5,IY)*QDSCM(5,IY)+dfd['PDSCM(6,IY)*QDSCM(6,IY)+dfd['PDSCM(7,IY)*QDSCM(7,IY))/(dfd['QDSCM(5,IY)+QDSCM(6,IY)+QDSCM(7,IY))
    z[34] = (
        (
            dfd["AMPBLK/PDSCM"].loc[5] * dfd["QDSCM"].loc[5]
            + dfd["AMPBLK/PDSCM"].loc[6] * dfd["QDSCM"].loc[6]
            + dfd["AMPBLK/PDSCM"].loc[7] * dfd["QDSCM"].loc[7]
        )
        / (dfd["QDSCM"].loc[5] + dfd["QDSCM"].loc[6] + dfd["QDSCM"].loc[7])
        * SCALPR2
    )

    #         Propane
    # T149(97,IY,IS)=(dfd['PPRCM(5,IY)*QPRCM(5,IY)+dfd['PPRCM(6,IY)*QPRCM(6,IY)+dfd['PPRCM(7,IY)*QPRCM(7,IY))/(dfd['QPRCM(5,IY)+QPRCM(6,IY)+QPRCM(7,IY))
    z[97] = (
        (
            dfd["APMORE/PPRCM"].loc[5] * dfd["QPRCM"].loc[5]
            + dfd["APMORE/PPRCM"].loc[6] * dfd["QPRCM"].loc[6]
            + dfd["APMORE/PPRCM"].loc[7] * dfd["QPRCM"].loc[7]
        )
        / (dfd["QPRCM"].loc[5] + dfd["QPRCM"].loc[6] + dfd["QPRCM"].loc[7])
        * SCALPR2
    )

    #         Residual Fuel Oil
    # T149(35,IY,IS)=(dfd['PRSCM(5,IY)*QRSCM(5,IY)+dfd['PRSCM(6,IY)*QRSCM(6,IY)+dfd['PRSCM(7,IY)*QRSCM(7,IY))/(dfd['QRSCM(5,IY)+QRSCM(6,IY)+QRSCM(7,IY))
    z[35] = (
        (
            dfd["AMPBLK/PRSCM"].loc[5] * dfd["QRSCM"].loc[5]
            + dfd["AMPBLK/PRSCM"].loc[6] * dfd["QRSCM"].loc[6]
            + dfd["AMPBLK/PRSCM"].loc[7] * dfd["QRSCM"].loc[7]
        )
        / (dfd["QRSCM"].loc[5] + dfd["QRSCM"].loc[6] + dfd["QRSCM"].loc[7])
        * SCALPR2
    )

    #         Natural Gas
    # T149(36,IY,IS)=(dfd['PNGCM(5,IY)*QNGCM(5,IY)+dfd['PNGCM(6,IY)*QNGCM(6,IY)+dfd['PNGCM(7,IY)*QNGCM(7,IY))/(dfd['QNGCM(5,IY)+QNGCM(6,IY)+QNGCM(7,IY))
    z[36] = (
        (
            dfd["AMPBLK/PNGCM"].loc[5] * dfd["QNGCM"].loc[5]
            + dfd["AMPBLK/PNGCM"].loc[6] * dfd["QNGCM"].loc[6]
            + dfd["AMPBLK/PNGCM"].loc[7] * dfd["QNGCM"].loc[7]
        )
        / (dfd["QNGCM"].loc[5] + dfd["QNGCM"].loc[6] + dfd["QNGCM"].loc[7])
        * SCALPR2
    )

    #         Coal
    # T149(37,IY,IS)=(dfd['PCLCM(5,IY)*QCLCM(5,IY)+dfd['PCLCM(6,IY)*QCLCM(6,IY)+dfd['PCLCM(7,IY)*QCLCM(7,IY))/(dfd['QCLCM(5,IY)+QCLCM(6,IY)+QCLCM(7,IY))
    z[37] = (
        (
            dfd["AMPBLK/PCLCM"].loc[5] * dfd["QCLCM"].loc[5]
            + dfd["AMPBLK/PCLCM"].loc[6] * dfd["QCLCM"].loc[6]
            + dfd["AMPBLK/PCLCM"].loc[7] * dfd["QCLCM"].loc[7]
        )
        / (dfd["QCLCM"].loc[5] + dfd["QCLCM"].loc[6] + dfd["QCLCM"].loc[7])
        * SCALPR2
    )

    #         Electricity
    # T149(38,IY,IS)=(dfd['PELCM(5,IY)*QELCM(5,IY)+dfd['PELCM(6,IY)*QELCM(6,IY)+dfd['PELCM(7,IY)*QELCM(7,IY))/(dfd['QELCM(5,IY)+QELCM(6,IY)+QELCM(7,IY))
    z[38] = (
        (
            dfd["AMPBLK/PELCM"].loc[5] * dfd["QELCM"].loc[5]
            + dfd["AMPBLK/PELCM"].loc[6] * dfd["QELCM"].loc[6]
            + dfd["AMPBLK/PELCM"].loc[7] * dfd["QELCM"].loc[7]
        )
        / (dfd["QELCM"].loc[5] + dfd["QELCM"].loc[6] + dfd["QELCM"].loc[7])
        * SCALPR2
    )

    #      West

    #         Distillate Fuel Oil
    # T149(39,IY,IS)=(dfd['PDSCM(8,IY)*QDSCM(8,IY)+dfd['PDSCM(9,IY)*QDSCM(9,IY))/(dfd['QDSCM(8,IY)+QDSCM(9,IY))
    z[39] = (
        (
            dfd["AMPBLK/PDSCM"].loc[8] * dfd["QDSCM"].loc[8]
            + dfd["AMPBLK/PDSCM"].loc[9] * dfd["QDSCM"].loc[9]
        )
        / (dfd["QDSCM"].loc[8] + dfd["QDSCM"].loc[9])
        * SCALPR2
    )

    #         Propane
    # T149(98,IY,IS)=(dfd['PPRCM(8,IY)*QPRCM(8,IY)+dfd['PPRCM(9,IY)*QPRCM(9,IY))/(dfd['QPRCM(8,IY)+QPRCM(9,IY))
    z[98] = (
        (
            dfd["APMORE/PPRCM"].loc[8] * dfd["QPRCM"].loc[8]
            + dfd["APMORE/PPRCM"].loc[9] * dfd["QPRCM"].loc[9]
        )
        / (dfd["QPRCM"].loc[8] + dfd["QPRCM"].loc[9])
        * SCALPR2
    )

    #         Residual Fuel Oil
    # T149(40,IY,IS)=(dfd['PRSCM(8,IY)*QRSCM(8,IY)+dfd['PRSCM(9,IY)*QRSCM(9,IY))/(dfd['QRSCM(8,IY)+QRSCM(9,IY))
    z[40] = (
        (
            dfd["AMPBLK/PRSCM"].loc[8] * dfd["QRSCM"].loc[8]
            + dfd["AMPBLK/PRSCM"].loc[9] * dfd["QRSCM"].loc[9]
        )
        / (dfd["QRSCM"].loc[8] + dfd["QRSCM"].loc[9]).replace(0, 1)
        * SCALPR2
    )

    #         Natural Gas
    # T149(41,IY,IS)=(dfd['PNGCM(8,IY)*QNGCM(8,IY)+dfd['PNGCM(9,IY)*QNGCM(9,IY))/(dfd['QNGCM(8,IY)+QNGCM(9,IY))
    z[41] = (
        (
            dfd["AMPBLK/PNGCM"].loc[8] * dfd["QNGCM"].loc[8]
            + dfd["AMPBLK/PNGCM"].loc[9] * dfd["QNGCM"].loc[9]
        )
        / (dfd["QNGCM"].loc[8] + dfd["QNGCM"].loc[9]).replace(0, 1)
        * SCALPR2
    )

    #         Coal
    # T149(42,IY,IS)=(dfd['PCLCM(8,IY)*QCLCM(8,IY)+dfd['PCLCM(9,IY)*QCLCM(9,IY))/(dfd['QCLCM(8,IY)+QCLCM(9,IY))
    z[42] = (
        (
            dfd["AMPBLK/PCLCM"].loc[8] * dfd["QCLCM"].loc[8]
            + dfd["AMPBLK/PCLCM"].loc[9] * dfd["QCLCM"].loc[9]
        )
        / (dfd["QCLCM"].loc[8] + dfd["QCLCM"].loc[9]).replace(0, 1)
        * SCALPR2
    )

    #         Electricity
    # T149(43,IY,IS)=(dfd['PELCM(8,IY)*QELCM(8,IY)+dfd['PELCM(9,IY)*QELCM(9,IY))/(dfd['QELCM(8,IY)+QELCM(9,IY))
    z[43] = (
        (
            dfd["AMPBLK/PELCM"].loc[8] * dfd["QELCM"].loc[8]
            + dfd["AMPBLK/PELCM"].loc[9] * dfd["QELCM"].loc[9]
        )
        / (dfd["QELCM"].loc[8] + dfd["QELCM"].loc[9]).replace(0, 1)
        * SCALPR2
    )

    #      United States

    #         Distillate Fuel Oil
    # T149(44,IY,IS)=PDSCM(11,IY)
    z[44] = dfd["AMPBLK/PDSCM"].loc[MNUMCR] * SCALPR2

    #         Propane
    # T149(99,IY,IS)=PPRCM(11,IY)
    z[99] = dfd["APMORE/PPRCM"].loc[MNUMCR] * SCALPR2

    #         Residual Fuel Oil
    # T149(45,IY,IS)=PRSCM(11,IY)
    z[45] = dfd["AMPBLK/PRSCM"].loc[MNUMCR] * SCALPR2
    #         Natural Gas
    # T149(46,IY,IS)=PNGCM(11,IY)
    z[46] = dfd["AMPBLK/PNGCM"].loc[MNUMCR] * SCALPR2

    #         Coal
    # T149(47,IY,IS)=PCLCM(11,IY)
    z[47] = dfd["AMPBLK/PCLCM"].loc[MNUMCR] * SCALPR2

    #         Electricity
    # T149(48,IY,IS)=PELCM(11,IY)
    z[48] = dfd["AMPBLK/PELCM"].loc[MNUMCR] * SCALPR2

    #

    #   Industrial

    #      Northeast

    #         Distillate Fuel Oil
    # T149(49,IY,IS)=(dfd['PDSIN(1,IY)*(dfd['QDSIN(1,IY)-QDSRF(1,IY))+dfd['PDSIN(2,IY)*(dfd['QDSIN(2,IY)-QDSRF(2,IY)))/(dfd['QDSIN(1,IY)+QDSIN(2,IY)-QDSRF(1,IY)-QDSRF(2,IY))
    z[49] = (
        (
            dfd["AMPBLK/PDSIN"].loc[1] * (dfd["QDSIN"].loc[1] - dfd["QDSRF"].loc[1])
            + dfd["AMPBLK/PDSIN"].loc[2] * (dfd["QDSIN"].loc[2] - dfd["QDSRF"].loc[2])
        )
        / (
            dfd["QDSIN"].loc[1]
            + dfd["QDSIN"].loc[2]
            - dfd["QDSRF"].loc[1]
            - dfd["QDSRF"].loc[2]
        ).replace(0, 1)
        * SCALPR2
    )

    #         Propane
    # T149(100,IY,IS)=(dfd['PPRIN(1,IY)*(dfd['QPRIN(1,IY)-QPRRF(1,IY))+dfd['PPRIN(2,IY)*(dfd['QPRIN(2,IY)-QPRRF(2,IY)))/(dfd['QPRIN(1,IY)+QPRIN(2,IY)-QPRRF(1,IY)-QPRRF(2,IY))
    z[100] = (
        (
            dfd["APMORE/PPRIN"].loc[1] * (dfd["QPRIN"].loc[1] - dfd["QPRRF"].loc[1])
            + dfd["APMORE/PPRIN"].loc[2] * (dfd["QPRIN"].loc[2] - dfd["QPRRF"].loc[2])
        )
        / (
            dfd["QPRIN"].loc[1]
            + dfd["QPRIN"].loc[2]
            - dfd["QPRRF"].loc[1]
            - dfd["QPRRF"].loc[2]
        ).replace(0, 1)
        * SCALPR2
    )

    #         Residual Fuel Oil
    # T149(50,IY,IS)=(dfd['PRSIN(1,IY)*(dfd['QRSIN(1,IY)-QRSRF(1,IY))+dfd['PRSIN(2,IY)*(dfd['QRSIN(2,IY)-QRSRF(2,IY)))/(dfd['QRSIN(1,IY)+QRSIN(2,IY)-QRSRF(1,IY)-QRSRF(2,IY))
    z[50] = (
        (
            dfd["AMPBLK/PRSIN"].loc[1] * (dfd["QRSIN"].loc[1] - dfd["QRSRF"].loc[1])
            + dfd["AMPBLK/PRSIN"].loc[2] * (dfd["QRSIN"].loc[2] - dfd["QRSRF"].loc[2])
        )
        / (
            dfd["QRSIN"].loc[1]
            + dfd["QRSIN"].loc[2]
            - dfd["QRSRF"].loc[1]
            - dfd["QRSRF"].loc[2]
        ).replace(0, 1)
        * SCALPR2
    )

    #         Natural Gas
    # T149(51,IY,IS)=(dfd['PNGIN(1,IY)*QNGIN(1,IY)+dfd['PNGIN(2,IY)*QNGIN(2,IY))/(dfd['QNGIN(1,IY)+QNGIN(2,IY))
    z[51] = (
        (
            dfd["AMPBLK/PNGIN"].loc[1] * dfd["QNGIN"].loc[1]
            + dfd["AMPBLK/PNGIN"].loc[2] * dfd["QNGIN"].loc[2]
        )
        / (dfd["QNGIN"].loc[1] + dfd["QNGIN"].loc[2]).replace(0, 1)
        * SCALPR2
    )

    #         Coal
    # T149(52,IY,IS)=(dfd['PCLIN(1,IY)*(dfd['QCLIN(1,IY))+dfd['PCLIN(2,IY)*(dfd['QCLIN(2,IY)))/(dfd['QCLIN(1,IY)+QCLIN(2,IY))
    z[52] = (
        (
            dfd["AMPBLK/PCLIN"].loc[1] * (dfd["QCLIN"].loc[1])
            + dfd["AMPBLK/PCLIN"].loc[2] * (dfd["QCLIN"].loc[2])
        )
        / (
            dfd["QCLIN"].loc[1]
            + dfd["QCLIN"].loc[2]
        ).replace(0, 1)
        * SCALPR2
    )

    #         Electricity
    # T149(53,IY,IS)=(dfd['PELIN(1,IY)*QELIN(1,IY)+dfd['PELIN(2,IY)*QELIN(2,IY))/(dfd['QELIN(1,IY)+QELIN(2,IY))
    z[53] = (
        (
            dfd["AMPBLK/PELIN"].loc[1] * dfd["QELIN"].loc[1]
            + dfd["AMPBLK/PELIN"].loc[2] * dfd["QELIN"].loc[2]
        )
        / (dfd["QELIN"].loc[1] + dfd["QELIN"].loc[2]).replace(0, 1)
        * SCALPR2
    )

    #      Midwest

    #         Distillate Fuel Oil
    # T149(54,IY,IS)=(dfd['PDSIN(3,IY)*(dfd['QDSIN(3,IY)-QDSRF(3,IY))+dfd['PDSIN(4,IY)*(dfd['QDSIN(4,IY)-QDSRF(4,IY)))/(dfd['QDSIN(3,IY)+QDSIN(4,IY)-QDSRF(3,IY)-QDSRF(4,IY))
    z[54] = (
        (
            dfd["AMPBLK/PDSIN"].loc[3] * (dfd["QDSIN"].loc[3] - dfd["QDSRF"].loc[3])
            + dfd["AMPBLK/PDSIN"].loc[4] * (dfd["QDSIN"].loc[4] - dfd["QDSRF"].loc[4])
        )
        / (
            dfd["QDSIN"].loc[3]
            + dfd["QDSIN"].loc[4]
            - dfd["QDSRF"].loc[3]
            - dfd["QDSRF"].loc[4]
        ).replace(0, 1)
        * SCALPR2
    )

    #         Propane
    # T149(101,IY,IS)=(dfd['PPRIN(3,IY)*(dfd['QPRIN(3,IY)-QPRRF(3,IY))+dfd['PPRIN(4,IY)*(dfd['QPRIN(4,IY)-QPRRF(4,IY)))/(dfd['QPRIN(3,IY)+QPRIN(4,IY)-QPRRF(3,IY)-QPRRF(4,IY))
    z[101] = (
        (
            dfd["APMORE/PPRIN"].loc[3] * (dfd["QPRIN"].loc[3] - dfd["QPRRF"].loc[3])
            + dfd["APMORE/PPRIN"].loc[4] * (dfd["QPRIN"].loc[4] - dfd["QPRRF"].loc[4])
        )
        / (
            dfd["QPRIN"].loc[3]
            + dfd["QPRIN"].loc[4]
            - dfd["QPRRF"].loc[3]
            - dfd["QPRRF"].loc[4]
        ).replace(0, 1)
        * SCALPR2
    )

    #         Residual Fuel Oil
    # T149(55,IY,IS)=(dfd['PRSIN(3,IY)*(dfd['QRSIN(3,IY)-QRSRF(3,IY))+dfd['PRSIN(4,IY)*(dfd['QRSIN(4,IY)-QRSRF(4,IY)))/(dfd['QRSIN(3,IY)+QRSIN(4,IY)-QRSRF(3,IY)-QRSRF(4,IY))
    z[55] = (
        (
            dfd["AMPBLK/PRSIN"].loc[3] * (dfd["QRSIN"].loc[3] - dfd["QRSRF"].loc[3])
            + dfd["AMPBLK/PRSIN"].loc[4] * (dfd["QRSIN"].loc[4] - dfd["QRSRF"].loc[4])
        )
        / (
            dfd["QRSIN"].loc[3]
            + dfd["QRSIN"].loc[4]
            - dfd["QRSRF"].loc[3]
            - dfd["QRSRF"].loc[4]
        ).replace(0, 1)
        * SCALPR2
    )

    #         Natural Gas
    # T149(56,IY,IS)=(dfd['PNGIN(3,IY)*QNGIN(3,IY)+dfd['PNGIN(4,IY)*QNGIN(4,IY))/(dfd['QNGIN(3,IY)+QNGIN(4,IY))
    z[56] = (
        (
            dfd["AMPBLK/PNGIN"].loc[3] * dfd["QNGIN"].loc[3]
            + dfd["AMPBLK/PNGIN"].loc[4] * dfd["QNGIN"].loc[4]
        )
        / (dfd["QNGIN"].loc[3] + dfd["QNGIN"].loc[4]).replace(0, 1)
        * SCALPR2
    )

    #         Coal
    # T149(57,IY,IS)=(dfd['PCLIN(3,IY)*(dfd['QCLIN(3,IY))+dfd['PCLIN(4,IY)*(dfd['QCLIN(4,IY)))/(dfd['QCLIN(3,IY)+QCLIN(4,IY))
    z[57] = (
        (
            dfd["AMPBLK/PCLIN"].loc[3] * (dfd["QCLIN"].loc[3])
            + dfd["AMPBLK/PCLIN"].loc[4] * (dfd["QCLIN"].loc[4])
        )
        / (
            dfd["QCLIN"].loc[3]
            + dfd["QCLIN"].loc[4]
        ).replace(0, 1)
        * SCALPR2
    )

    #         Electricity
    # T149(58,IY,IS)=(dfd['PELIN(3,IY)*QELIN(3,IY)+dfd['PELIN(4,IY)*QELIN(4,IY))/(dfd['QELIN(3,IY)+QELIN(4,IY))
    z[58] = (
        (
            dfd["AMPBLK/PELIN"].loc[3] * dfd["QELIN"].loc[3]
            + dfd["AMPBLK/PELIN"].loc[4] * dfd["QELIN"].loc[4]
        )
        / (dfd["QELIN"].loc[3] + dfd["QELIN"].loc[4]).replace(0, 1)
        * SCALPR2
    )

    #      South

    #         Distillate Fuel Oil
    # T149(59,IY,IS)=(dfd['PDSIN(5,IY)*(dfd['QDSIN(5,IY)-QDSRF(5,IY))+dfd['PDSIN(6,IY)*(dfd['QDSIN(6,IY)-QDSRF(6,IY))+dfd['PDSIN(7,IY)*(dfd['QDSIN(7,IY)-QDSRF(7,IY)))/(dfd['QDSIN(5,IY)+QDSIN(6,IY)+QDSIN(7,IY)-QDSRF(5,IY)-QDSRF(6,IY)-QDSRF(7,IY))
    z[59] = (
        (
            dfd["AMPBLK/PDSIN"].loc[5] * (dfd["QDSIN"].loc[5] - dfd["QDSRF"].loc[5])
            + dfd["AMPBLK/PDSIN"].loc[6] * (dfd["QDSIN"].loc[6] - dfd["QDSRF"].loc[6])
            + dfd["AMPBLK/PDSIN"].loc[7] * (dfd["QDSIN"].loc[7] - dfd["QDSRF"].loc[7])
        )
        / (
            dfd["QDSIN"].loc[5]
            + dfd["QDSIN"].loc[6]
            + dfd["QDSIN"].loc[7]
            - dfd["QDSRF"].loc[5]
            - dfd["QDSRF"].loc[6]
            - dfd["QDSRF"].loc[7]
        ).replace(0, 1)
        * SCALPR2
    )

    #         Propane
    # T149(102,IY,IS)=(dfd['PPRIN(5,IY)*(dfd['QPRIN(5,IY)-QPRRF(5,IY))+dfd['PPRIN(6,IY)*(dfd['QPRIN(6,IY)-QPRRF(6,IY))+dfd['PPRIN(7,IY)*(dfd['QPRIN(7,IY)-QPRRF(7,IY)))/(dfd['QPRIN(5,IY)+QPRIN(6,IY)+QPRIN(7,IY)-QPRRF(5,IY)-QPRRF(6,IY)-QPRRF(7,IY))
    z[102] = (
        (
            dfd["APMORE/PPRIN"].loc[5] * (dfd["QPRIN"].loc[5] - dfd["QPRRF"].loc[5])
            + dfd["APMORE/PPRIN"].loc[6] * (dfd["QPRIN"].loc[6] - dfd["QPRRF"].loc[6])
            + dfd["APMORE/PPRIN"].loc[7] * (dfd["QPRIN"].loc[7] - dfd["QPRRF"].loc[7])
        )
        / (
            dfd["QPRIN"].loc[5]
            + dfd["QPRIN"].loc[6]
            + dfd["QPRIN"].loc[7]
            - dfd["QPRRF"].loc[5]
            - dfd["QPRRF"].loc[6]
            - dfd["QPRRF"].loc[7]
        ).replace(0, 1)
        * SCALPR2
    )

    #         Residual Fuel Oil
    # T149(60,IY,IS)=(dfd['PRSIN(5,IY)*(dfd['QRSIN(5,IY)-QRSRF(5,IY))+dfd['PRSIN(6,IY)*(dfd['QRSIN(6,IY)-QRSRF(6,IY))+dfd['PRSIN(7,IY)*(dfd['QRSIN(7,IY)-QRSRF(7,IY)))/(dfd['QRSIN(5,IY)+QRSIN(6,IY)+QRSIN(7,IY)-QRSRF(5,IY)-QRSRF(6,IY)-QRSRF(7,IY))
    z[60] = (
        (
            dfd["AMPBLK/PRSIN"].loc[5] * (dfd["QRSIN"].loc[5] - dfd["QRSRF"].loc[5])
            + dfd["AMPBLK/PRSIN"].loc[6] * (dfd["QRSIN"].loc[6] - dfd["QRSRF"].loc[6])
            + dfd["AMPBLK/PRSIN"].loc[7] * (dfd["QRSIN"].loc[7] - dfd["QRSRF"].loc[7])
        )
        / (
            dfd["QRSIN"].loc[5]
            + dfd["QRSIN"].loc[6]
            + dfd["QRSIN"].loc[7]
            - dfd["QRSRF"].loc[5]
            - dfd["QRSRF"].loc[6]
            - dfd["QRSRF"].loc[7]
        ).replace(0, 1)
        * SCALPR2
    )

    #         Natural Gas
    # T149(61,IY,IS)=(dfd['PNGIN(5,IY)*QNGIN(5,IY)+dfd['PNGIN(6,IY)*QNGIN(6,IY)+dfd['PNGIN(7,IY)*QNGIN(7,IY))/(dfd['QNGIN(5,IY)+QNGIN(6,IY)+QNGIN(7,IY))
    z[61] = (
        (
            dfd["AMPBLK/PNGIN"].loc[5] * dfd["QNGIN"].loc[5]
            + dfd["AMPBLK/PNGIN"].loc[6] * dfd["QNGIN"].loc[6]
            + dfd["AMPBLK/PNGIN"].loc[7] * dfd["QNGIN"].loc[7]
        )
        / (dfd["QNGIN"].loc[5] + dfd["QNGIN"].loc[6] + dfd["QNGIN"].loc[7]).replace(
            0, 1
        )
        * SCALPR2
    )

    #         Coal
    # T149(62,IY,IS)=(dfd['PCLIN(5,IY)*(dfd['QCLIN(5,IY))+dfd['PCLIN(6,IY)*(dfd['QCLIN(6,IY))+dfd['PCLIN(7,IY)*(dfd['QCLIN(7,IY)))/(dfd['QCLIN(5,IY)+QCLIN(6,IY)+QCLIN(7,IY))
    z[62] = (
        (
            dfd["AMPBLK/PCLIN"].loc[5] * (dfd["QCLIN"].loc[5])
            + dfd["AMPBLK/PCLIN"].loc[6] * (dfd["QCLIN"].loc[6])
            + dfd["AMPBLK/PCLIN"].loc[7] * (dfd["QCLIN"].loc[7])
        )
        / (
            dfd["QCLIN"].loc[5]
            + dfd["QCLIN"].loc[6]
            + dfd["QCLIN"].loc[7]
        ).replace(0, 1)
        * SCALPR2
    )

    #         Electricity
    # T149(63,IY,IS)=(dfd['PELIN(5,IY)*QELIN(5,IY)+dfd['PELIN(6,IY)*QELIN(6,IY)+dfd['PELIN(7,IY)*QELIN(7,IY))/(dfd['QELIN(5,IY)+QELIN(6,IY)+QELIN(7,IY))
    z[63] = (
        (
            dfd["AMPBLK/PELIN"].loc[5] * dfd["QELIN"].loc[5]
            + dfd["AMPBLK/PELIN"].loc[6] * dfd["QELIN"].loc[6]
            + dfd["AMPBLK/PELIN"].loc[7] * dfd["QELIN"].loc[7]
        )
        / (dfd["QELIN"].loc[5] + dfd["QELIN"].loc[6] + dfd["QELIN"].loc[7]).replace(
            0, 1
        )
        * SCALPR2
    )

    #      West

    #         Distillate Fuel Oil
    # T149(64,IY,IS)=(dfd['PDSIN(8,IY)*(dfd['QDSIN(8,IY)-QDSRF(8,IY))+dfd['PDSIN(9,IY)*(dfd['QDSIN(9,IY)-QDSRF(9,IY)))/(dfd['QDSIN(8,IY)+QDSIN(9,IY)-QDSRF(8,IY)-QDSRF(9,IY))
    z[64] = (
        (
            dfd["AMPBLK/PDSIN"].loc[8] * (dfd["QDSIN"].loc[8] - dfd["QDSRF"].loc[8])
            + dfd["AMPBLK/PDSIN"].loc[9] * (dfd["QDSIN"].loc[9] - dfd["QDSRF"].loc[9])
        )
        / (
            dfd["QDSIN"].loc[8]
            + dfd["QDSIN"].loc[9]
            - dfd["QDSRF"].loc[8]
            - dfd["QDSRF"].loc[9]
        ).replace(0, 1)
        * SCALPR2
    )

    #         Propane
    # T149(103,IY,IS)=(dfd['PPRIN(8,IY)*(dfd['QPRIN(8,IY)-QPRRF(8,IY))+dfd['PPRIN(9,IY)*(dfd['QPRIN(9,IY)-QPRRF(9,IY)))/(dfd['QPRIN(8,IY)+QPRIN(9,IY)-QPRRF(8,IY)-QPRRF(9,IY))
    z[103] = (
        (
            dfd["APMORE/PPRIN"].loc[8] * (dfd["QPRIN"].loc[8] - dfd["QPRRF"].loc[8])
            + dfd["APMORE/PPRIN"].loc[9] * (dfd["QPRIN"].loc[9] - dfd["QPRRF"].loc[9])
        )
        / (
            dfd["QPRIN"].loc[8]
            + dfd["QPRIN"].loc[9]
            - dfd["QPRRF"].loc[8]
            - dfd["QPRRF"].loc[9]
        ).replace(0, 1)
        * SCALPR2
    )

    #         Residual Fuel Oil
    # T149(65,IY,IS)=(dfd['PRSIN(8,IY)*(dfd['QRSIN(8,IY)-QRSRF(8,IY))+dfd['PRSIN(9,IY)*(dfd['QRSIN(9,IY)-QRSRF(9,IY)))/(dfd['QRSIN(8,IY)+QRSIN(9,IY)-QRSRF(8,IY)-QRSRF(9,IY))
    z[65] = (
        (
            dfd["AMPBLK/PRSIN"].loc[8] * (dfd["QRSIN"].loc[8] - dfd["QRSRF"].loc[8])
            + dfd["AMPBLK/PRSIN"].loc[9] * (dfd["QRSIN"].loc[9] - dfd["QRSRF"].loc[9])
        )
        / (
            dfd["QRSIN"].loc[8]
            + dfd["QRSIN"].loc[9]
            - dfd["QRSRF"].loc[8]
            - dfd["QRSRF"].loc[9]
        ).replace(0, 1)
        * SCALPR2
    )

    #         Natural Gas
    # T149(66,IY,IS)=(dfd['PNGIN(8,IY)*QNGIN(8,IY)+dfd['PNGIN(9,IY)*QNGIN(9,IY))/(dfd['QNGIN(8,IY)+QNGIN(9,IY))
    z[66] = (
        (
            dfd["AMPBLK/PNGIN"].loc[8] * dfd["QNGIN"].loc[8]
            + dfd["AMPBLK/PNGIN"].loc[9] * dfd["QNGIN"].loc[9]
        )
        / (dfd["QNGIN"].loc[8] + dfd["QNGIN"].loc[9]).replace(0, 1)
        * SCALPR2
    )

    #         Coal
    # T149(67,IY,IS)=(dfd['PCLIN(8,IY)*(dfd['QCLIN(8,IY))+dfd['PCLIN(9,IY)*(dfd['QCLIN(9,IY)))/(dfd['QCLIN(8,IY)+QCLIN(9,IY))
    z[67] = (
        (
            dfd["AMPBLK/PCLIN"].loc[8] * (dfd["QCLIN"].loc[8])
            + dfd["AMPBLK/PCLIN"].loc[9] * (dfd["QCLIN"].loc[9])
        )
        / (
            dfd["QCLIN"].loc[8]
            + dfd["QCLIN"].loc[9]
        ).replace(0, 1)
        * SCALPR2
    )

    #         Electricity
    # T149(68,IY,IS)=(dfd['PELIN(8,IY)*QELIN(8,IY)+dfd['PELIN(9,IY)*QELIN(9,IY))/(dfd['QELIN(8,IY)+QELIN(9,IY))
    z[68] = (
        (
            dfd["AMPBLK/PELIN"].loc[8] * dfd["QELIN"].loc[8]
            + dfd["AMPBLK/PELIN"].loc[9] * dfd["QELIN"].loc[9]
        )
        / (dfd["QELIN"].loc[8] + dfd["QELIN"].loc[9]).replace(0, 1)
        * SCALPR2
    )

    #      United States

    #         Distillate Fuel Oil
    # T149(69,IY,IS)=PDSIN(11,IY)
    z[69] = dfd["AMPBLK/PDSIN"].loc[MNUMCR] * SCALPR2

    #         Propane
    # T149(104,IY,IS)=PPRIN(11,IY)
    z[104] = dfd["APMORE/PPRIN"].loc[MNUMCR] * SCALPR2

    #         Residual Fuel Oil
    # T149(70,IY,IS)=PRSIN(11,IY)
    z[70] = dfd["AMPBLK/PRSIN"].loc[MNUMCR] * SCALPR2

    #         Natural Gas
    # T149(71,IY,IS)=PNGIN(11,IY)
    z[71] = dfd["AMPBLK/PNGIN"].loc[MNUMCR] * SCALPR2

    #         Coal
    # T149(72,IY,IS)=PCLIN(11,IY)
    z[72] = dfd["AMPBLK/PCLIN"].loc[MNUMCR] * SCALPR2

    #         Electricity
    # T149(73,IY,IS)=PELIN(11,IY)
    z[73] = dfd["AMPBLK/PELIN"].loc[MNUMCR] * SCALPR2

    #

    #   Transportation

    #      Northeast

    #         Motor Gasoline
    # T149(74,IY,IS)=(dfd['PMGTR(1,IY)*(dfd['QMGTR(1,IY)-QMGBS(1,IY))+dfd['PMGTR(2,IY)*(dfd['QMGTR(2,IY)-QMGBS(2,IY)))/(dfd['QMGTR(1,IY)+QMGTR(2,IY)-QMGBS(1,IY)-QMGBS(2,IY))
    z[74] = (
        (
            dfd["AMPBLK/PMGTR"].loc[1] * (dfd["QMGTR"].loc[1] - dfd["QMGBS"].loc[1])
            + dfd["AMPBLK/PMGTR"].loc[2] * (dfd["QMGTR"].loc[2] - dfd["QMGBS"].loc[2])
        )
        / (
            dfd["QMGTR"].loc[1]
            + dfd["QMGTR"].loc[2]
            - dfd["QMGBS"].loc[1]
            - dfd["QMGBS"].loc[2]
        ).replace(0, 1)
        * SCALPR2
    )

    #         E85
    # T149(79,IY,IS)=(dfd['PETTR(1,IY)*QETTR(1,IY)+dfd['PETTR(2,IY)*QETTR(2,IY))/(dfd['QETTR(1,IY)+QETTR(2,IY))
    z[79] = (
        (
            dfd["AMPBLK/PETTR"].loc[1] * dfd["QETTR"].loc[1]
            + dfd["AMPBLK/PETTR"].loc[2] * dfd["QETTR"].loc[2]
        )
        / (dfd["QETTR"].loc[1] + dfd["QETTR"].loc[2]).replace(0, 1)
        * SCALPR2
    )

    #      Midwest

    #         Motor Gasoline
    # T149(75,IY,IS)=(dfd['PMGTR(3,IY)*(dfd['QMGTR(3,IY)-QMGBS(3,IY))+dfd['PMGTR(4,IY)*(dfd['QMGTR(4,IY)-QMGBS(4,IY)))/(dfd['QMGTR(3,IY)+QMGTR(4,IY)-QMGBS(3,IY)-QMGBS(4,IY))
    z[75] = (
        (
            dfd["AMPBLK/PMGTR"].loc[3] * (dfd["QMGTR"].loc[3] - dfd["QMGBS"].loc[3])
            + dfd["AMPBLK/PMGTR"].loc[4] * (dfd["QMGTR"].loc[4] - dfd["QMGBS"].loc[4])
        )
        / (
            dfd["QMGTR"].loc[3]
            + dfd["QMGTR"].loc[4]
            - dfd["QMGBS"].loc[3]
            - dfd["QMGBS"].loc[4]
        ).replace(0, 1)
        * SCALPR2
    )

    #         E85
    # T149(80,IY,IS)=(dfd['PETTR(3,IY)*QETTR(3,IY)+dfd['PETTR(4,IY)*QETTR(4,IY))/(dfd['QETTR(3,IY)+QETTR(4,IY))
    z[80] = (
        (
            dfd["AMPBLK/PETTR"].loc[3] * dfd["QETTR"].loc[3]
            + dfd["AMPBLK/PETTR"].loc[4] * dfd["QETTR"].loc[4]
        )
        / (dfd["QETTR"].loc[3] + dfd["QETTR"].loc[4]).replace(0, 1)
        * SCALPR2
    )

    #      South

    #         Motor Gasoline
    # T149(76,IY,IS)=(dfd['PMGTR(5,IY)*(dfd['QMGTR(5,IY)-QMGBS(5,IY))+dfd['PMGTR(6,IY)*(dfd['QMGTR(6,IY)-QMGBS(6,IY))+dfd['PMGTR(7,IY)*(dfd['QMGTR(7,IY)-QMGBS(7,IY)))/(dfd['QMGTR(5,IY)+QMGTR(6,IY)+QMGTR(7,IY)-QMGBS(5,IY)-QMGBS(6,IY)-QMGBS(7,IY))
    z[76] = (
        (
            dfd["AMPBLK/PMGTR"].loc[5] * (dfd["QMGTR"].loc[5] - dfd["QMGBS"].loc[5])
            + dfd["AMPBLK/PMGTR"].loc[6] * (dfd["QMGTR"].loc[6] - dfd["QMGBS"].loc[6])
            + dfd["AMPBLK/PMGTR"].loc[7] * (dfd["QMGTR"].loc[7] - dfd["QMGBS"].loc[7])
        )
        / (
            dfd["QMGTR"].loc[5]
            + dfd["QMGTR"].loc[6]
            + dfd["QMGTR"].loc[7]
            - dfd["QMGBS"].loc[5]
            - dfd["QMGBS"].loc[6]
            - dfd["QMGBS"].loc[7]
        ).replace(0, 1)
        * SCALPR2
    )

    #         E85
    # T149(81,IY,IS)=(dfd['PETTR(5,IY)*QETTR(5,IY)+dfd['PETTR(6,IY)*QETTR(6,IY)+dfd['PETTR(7,IY)*QETTR(7,IY))/(dfd['QETTR(5,IY)+QETTR(6,IY)+QETTR(7,IY))
    z[81] = (
        (
            dfd["AMPBLK/PETTR"].loc[5] * dfd["QETTR"].loc[5]
            + dfd["AMPBLK/PETTR"].loc[6] * dfd["QETTR"].loc[6]
            + dfd["AMPBLK/PETTR"].loc[7] * dfd["QETTR"].loc[7]
        )
        / (dfd["QETTR"].loc[5] + dfd["QETTR"].loc[6] + dfd["QETTR"].loc[7]).replace(
            0, 1
        )
        * SCALPR2
    )

    #      West

    #         Motor Gasoline
    # T149(77,IY,IS)=(dfd['PMGTR(8,IY)*(dfd['QMGTR(8,IY)-QMGBS(8,IY))+dfd['PMGTR(9,IY)*(dfd['QMGTR(9,IY)-QMGBS(9,IY)))/(dfd['QMGTR(8,IY)+QMGTR(9,IY)-QMGBS(8,IY)-QMGBS(9,IY))
    z[77] = (
        (
            dfd["AMPBLK/PMGTR"].loc[8] * (dfd["QMGTR"].loc[8] - dfd["QMGBS"].loc[8])
            + dfd["AMPBLK/PMGTR"].loc[9] * (dfd["QMGTR"].loc[9] - dfd["QMGBS"].loc[9])
        )
        / (
            dfd["QMGTR"].loc[8]
            + dfd["QMGTR"].loc[9]
            - dfd["QMGBS"].loc[8]
            - dfd["QMGBS"].loc[9]
        ).replace(0, 1)
        * SCALPR2
    )

    #         E85
    # T149(82,IY,IS)=(dfd['PETTR(8,IY)*QETTR(8,IY)+dfd['PETTR(9,IY)*QETTR(9,IY))/(dfd['QETTR(8,IY)+QETTR(9,IY))
    z[82] = (
        (
            dfd["AMPBLK/PETTR"].loc[8] * dfd["QETTR"].loc[8]
            + dfd["AMPBLK/PETTR"].loc[9] * dfd["QETTR"].loc[9]
        )
        / (dfd["QETTR"].loc[8] + dfd["QETTR"].loc[9]).replace(0, 1)
        * SCALPR2
    )

    #      United States

    #         Motor Gasoline
    # T149(78,IY,IS)=PMGTR(11,IY)
    z[78] = dfd["AMPBLK/PMGTR"].loc[MNUMCR] * SCALPR2

    #         E85
    # T149(83,IY,IS)=PETTR(11,IY)
    z[83] = dfd["AMPBLK/PETTR"].loc[MNUMCR] * SCALPR2

    #

    #   Electric Power Biomass

    #      Northeast
    # T149(84,IY,IS)=(UPRWDCR(1,IY)*QBMEL(1,IY)+UPRWDCR(2,IY)*QBMEL(2,IY))/(dfd['QBMEL(1,IY)+QBMEL(2,IY))
    z[84] = (
        (
            dfd["UPRWDCR"].loc[1] * dfd["QBMEL"].loc[1]
            + dfd["UPRWDCR"].loc[2] * dfd["QBMEL"].loc[2]
        )
        / (dfd["QBMEL"].loc[1] + dfd["QBMEL"].loc[2]).replace(0, 1)
        * SCALPR2
    )

    #      Midwest
    # T149(85,IY,IS)=(UPRWDCR(3,IY)*QBMEL(3,IY)+UPRWDCR(4,IY)*QBMEL(4,IY))/(dfd['QBMEL(3,IY)+QBMEL(4,IY))
    z[85] = (
        (
            dfd["UPRWDCR"].loc[3] * dfd["QBMEL"].loc[3]
            + dfd["UPRWDCR"].loc[4] * dfd["QBMEL"].loc[4]
        )
        / (dfd["QBMEL"].loc[3] + dfd["QBMEL"].loc[4]).replace(0, 1)
        * SCALPR2
    )

    #      South
    # T149(86,IY,IS)=(UPRWDCR(5,IY)*QBMEL(5,IY)+UPRWDCR(6,IY)*QBMEL(6,IY)+UPRWDCR(7,IY)*QBMEL(7,IY))/(dfd['QBMEL(5,IY)+QBMEL(6,IY)+QBMEL(7,IY))
    z[86] = (
        (
            dfd["UPRWDCR"].loc[5] * dfd["QBMEL"].loc[5]
            + dfd["UPRWDCR"].loc[6] * dfd["QBMEL"].loc[6]
            + dfd["UPRWDCR"].loc[7] * dfd["QBMEL"].loc[7]
        )
        / (dfd["QBMEL"].loc[5] + dfd["QBMEL"].loc[6] + dfd["QBMEL"].loc[7]).replace(
            0, 1
        )
        * SCALPR2
    )

    #      West
    # T149(87,IY,IS)=(UPRWDCR(8,IY)*QBMEL(8,IY)+UPRWDCR(9,IY)*QBMEL(9,IY))/(dfd['QBMEL(8,IY)+QBMEL(9,IY))
    z[87] = (
        (
            dfd["UPRWDCR"].loc[8] * dfd["QBMEL"].loc[8]
            + dfd["UPRWDCR"].loc[9] * dfd["QBMEL"].loc[9]
        )
        / (dfd["QBMEL"].loc[8] + dfd["QBMEL"].loc[9]).replace(0, 1)
        * SCALPR2
    )

    #      United States
    # T149(88,IY,IS)=UPRWDCR(11,IY)
    z[88] = dfd["UPRWDCR"].loc[MNUMCR] * SCALPR2

    #

    #

    #   Residential

    #      Northeast

    #         Distillate Fuel Oil
    # T149(105,IY,IS)=QDSRS(1,IY)+QDSRS(2,IY)
    z[105] = (dfd["QDSRS"].loc[1] + dfd["QDSRS"].loc[2]) * TRIL_TO_QUAD

    #         Propane
    # T149(106,IY,IS)=QPRRS(1,IY)+QPRRS(2,IY)
    z[106] = (dfd["QPRRS"].loc[1] + dfd["QPRRS"].loc[2]) * TRIL_TO_QUAD

    #         Natural Gas
    # T149(107,IY,IS)=QNGRS(1,IY)+QNGRS(2,IY)
    z[107] = (dfd["QNGRS"].loc[1] + dfd["QNGRS"].loc[2]) * TRIL_TO_QUAD

    #         Electricity
    # T149(108,IY,IS)=QELRS(1,IY)+QELRS(2,IY)
    z[108] = (dfd["QELRS"].loc[1] + dfd["QELRS"].loc[2]) * TRIL_TO_QUAD

    #      Midwest

    #         Distillate Fuel Oil
    # T149(109,IY,IS)=QDSRS(3,IY)+QDSRS(4,IY)
    z[109] = (dfd["QDSRS"].loc[3] + dfd["QDSRS"].loc[4]) * TRIL_TO_QUAD

    #         Propane
    # T149(110,IY,IS)=QPRRS(3,IY)+QPRRS(4,IY)
    z[110] = (dfd["QPRRS"].loc[3] + dfd["QPRRS"].loc[4]) * TRIL_TO_QUAD

    #         Natural Gas
    # T149(111,IY,IS)=QNGRS(3,IY)+QNGRS(4,IY)
    z[111] = (dfd["QNGRS"].loc[3] + dfd["QNGRS"].loc[4]) * TRIL_TO_QUAD

    #         Electricity
    # T149(112,IY,IS)=QELRS(3,IY)+QELRS(4,IY)
    z[112] = (dfd["QELRS"].loc[3] + dfd["QELRS"].loc[4]) * TRIL_TO_QUAD

    #      South

    #         Distillate Fuel Oil
    # T149(113,IY,IS)=QDSRS(5,IY)+QDSRS(6,IY)+QDSRS(7,IY)
    z[113] = (
        dfd["QDSRS"].loc[5] + dfd["QDSRS"].loc[6] + dfd["QDSRS"].loc[7]
    ) * TRIL_TO_QUAD

    #         Propane
    # T149(114,IY,IS)=QPRRS(5,IY)+QPRRS(6,IY)+QPRRS(7,IY)
    z[114] = (
        dfd["QPRRS"].loc[5] + dfd["QPRRS"].loc[6] + dfd["QPRRS"].loc[7]
    ) * TRIL_TO_QUAD

    #         Natural Gas
    # T149(115,IY,IS)=QNGRS(5,IY)+QNGRS(6,IY)+QNGRS(7,IY)
    z[115] = (
        dfd["QNGRS"].loc[5] + dfd["QNGRS"].loc[6] + dfd["QNGRS"].loc[7]
    ) * TRIL_TO_QUAD

    #         Electricity
    # T149(116,IY,IS)=QELRS(5,IY)+QELRS(6,IY)+QELRS(7,IY)
    z[116] = (
        dfd["QELRS"].loc[5] + dfd["QELRS"].loc[6] + dfd["QELRS"].loc[7]
    ) * TRIL_TO_QUAD

    #      West

    #         Distillate Fuel Oil
    # T149(117,IY,IS)=QDSRS(8,IY)+QDSRS(9,IY)
    z[117] = (dfd["QDSRS"].loc[8] + dfd["QDSRS"].loc[9]) * TRIL_TO_QUAD

    #         Propane
    # T149(118,IY,IS)=QPRRS(8,IY)+QPRRS(9,IY)
    z[118] = (dfd["QPRRS"].loc[8] + dfd["QPRRS"].loc[9]) * TRIL_TO_QUAD
    #         Natural Gas
    # T149(119,IY,IS)=QNGRS(8,IY)+QNGRS(9,IY)
    z[119] = (dfd["QNGRS"].loc[8] + dfd["QNGRS"].loc[9]) * TRIL_TO_QUAD

    #         Electricity
    # T149(120,IY,IS)=QELRS(8,IY)+QELRS(9,IY)
    z[120] = (dfd["QELRS"].loc[8] + dfd["QELRS"].loc[9]) * TRIL_TO_QUAD

    #

    #   Commercial

    #      Northeast

    #         Distillate Fuel Oil
    # T149(121,IY,IS)=QDSCM(1,IY)+QDSCM(2,IY)
    z[121] = (dfd["QDSCM"].loc[1] + dfd["QDSCM"].loc[2]) * TRIL_TO_QUAD

    #         Propane
    # T149(122,IY,IS)=QPRCM(1,IY)+QPRCM(2,IY)
    z[122] = (dfd["QPRCM"].loc[1] + dfd["QPRCM"].loc[2]) * TRIL_TO_QUAD

    #         Residual Fuel Oil
    # T149(123,IY,IS)=QRSCM(1,IY)+QRSCM(2,IY)
    z[123] = (dfd["QRSCM"].loc[1] + dfd["QRSCM"].loc[2]) * TRIL_TO_QUAD

    #         Natural Gas
    # T149(124,IY,IS)=QNGCM(1,IY)+QNGCM(2,IY)
    z[124] = (dfd["QNGCM"].loc[1] + dfd["QNGCM"].loc[2]) * TRIL_TO_QUAD

    #         Coal
    # T149(125,IY,IS)=QCLCM(1,IY)+QCLCM(2,IY)
    z[125] = (dfd["QCLCM"].loc[1] + dfd["QCLCM"].loc[2]) * TRIL_TO_QUAD

    #         Electricity
    # T149(126,IY,IS)=QELCM(1,IY)+QELCM(2,IY)
    z[126] = (dfd["QELCM"].loc[1] + dfd["QELCM"].loc[2]) * TRIL_TO_QUAD

    #      Midwest

    #         Distillate Fuel Oil
    # T149(127,IY,IS)=QDSCM(3,IY)+QDSCM(4,IY)
    z[127] = (dfd["QDSCM"].loc[3] + dfd["QDSCM"].loc[4]) * TRIL_TO_QUAD

    #         Propane
    # T149(128,IY,IS)=QPRCM(3,IY)+QPRCM(4,IY)
    z[128] = (dfd["QPRCM"].loc[3] + dfd["QPRCM"].loc[4]) * TRIL_TO_QUAD

    #         Residual Fuel Oil
    # T149(129,IY,IS)=QRSCM(3,IY)+QRSCM(4,IY)
    z[129] = (dfd["QRSCM"].loc[3] + dfd["QRSCM"].loc[4]) * TRIL_TO_QUAD

    #         Natural Gas
    # T149(130,IY,IS)=QNGCM(3,IY)+QNGCM(4,IY)
    z[130] = (dfd["QNGCM"].loc[3] + dfd["QNGCM"].loc[4]) * TRIL_TO_QUAD

    #         Coal
    # T149(131,IY,IS)=QCLCM(3,IY)+QCLCM(4,IY)
    z[131] = (dfd["QCLCM"].loc[3] + dfd["QCLCM"].loc[4]) * TRIL_TO_QUAD

    #         Electricity
    # T149(132,IY,IS)=QELCM(3,IY)+QELCM(4,IY)
    z[132] = (dfd["QELCM"].loc[3] + dfd["QELCM"].loc[4]) * TRIL_TO_QUAD

    #      South

    #         Distillate Fuel Oil
    # T149(133,IY,IS)=QDSCM(5,IY)+QDSCM(6,IY)+QDSCM(7,IY)
    z[133] = (
        dfd["QDSCM"].loc[5] + dfd["QDSCM"].loc[6] + dfd["QDSCM"].loc[7]
    ) * TRIL_TO_QUAD

    #         Propane
    # T149(134,IY,IS)=QPRCM(5,IY)+QPRCM(6,IY)+QPRCM(7,IY)
    z[134] = (
        dfd["QPRCM"].loc[5] + dfd["QPRCM"].loc[6] + dfd["QPRCM"].loc[7]
    ) * TRIL_TO_QUAD

    #         Residual Fuel Oil
    # T149(135,IY,IS)=QRSCM(5,IY)+QRSCM(6,IY)+QRSCM(7,IY)
    z[135] = (
        dfd["QRSCM"].loc[5] + dfd["QRSCM"].loc[6] + dfd["QRSCM"].loc[7]
    ) * TRIL_TO_QUAD

    #         Natural Gas
    # T149(136,IY,IS)=QNGCM(5,IY)+QNGCM(6,IY)+QNGCM(7,IY)
    z[136] = (
        dfd["QNGCM"].loc[5] + dfd["QNGCM"].loc[6] + dfd["QNGCM"].loc[7]
    ) * TRIL_TO_QUAD

    #         Coal
    # T149(137,IY,IS)=QCLCM(5,IY)+QCLCM(6,IY)+QCLCM(7,IY)
    z[137] = (
        dfd["QCLCM"].loc[5] + dfd["QCLCM"].loc[6] + dfd["QCLCM"].loc[7]
    ) * TRIL_TO_QUAD

    #         Electricity
    # T149(138,IY,IS)=QELCM(5,IY)+QELCM(6,IY)+QELCM(7,IY)
    z[138] = (
        dfd["QELCM"].loc[5] + dfd["QELCM"].loc[6] + dfd["QELCM"].loc[7]
    ) * TRIL_TO_QUAD

    #      West

    #         Distillate Fuel Oil
    # T149(139,IY,IS)=QDSCM(8,IY)+QDSCM(9,IY)
    z[139] = (dfd["QDSCM"].loc[8] + dfd["QDSCM"].loc[9]) * TRIL_TO_QUAD

    #         Propane
    # T149(140,IY,IS)=QPRCM(8,IY)+QPRCM(9,IY)
    z[140] = (dfd["QPRCM"].loc[8] + dfd["QPRCM"].loc[9]) * TRIL_TO_QUAD

    #         Residual Fuel Oil
    # T149(141,IY,IS)=QRSCM(8,IY)+QRSCM(9,IY)
    z[141] = (dfd["QRSCM"].loc[8] + dfd["QRSCM"].loc[9]) * TRIL_TO_QUAD

    #         Natural Gas
    # T149(142,IY,IS)=QNGCM(8,IY)+QNGCM(9,IY)
    z[142] = (dfd["QNGCM"].loc[8] + dfd["QNGCM"].loc[9]) * TRIL_TO_QUAD

    #         Coal
    # T149(143,IY,IS)=QCLCM(8,IY)+QCLCM(9,IY)
    z[143] = (dfd["QCLCM"].loc[8] + dfd["QCLCM"].loc[9]) * TRIL_TO_QUAD

    #         Electricity
    # T149(144,IY,IS)=QELCM(8,IY)+QELCM(9,IY)
    z[144] = (dfd["QELCM"].loc[8] + dfd["QELCM"].loc[9]) * TRIL_TO_QUAD

    #

    #   Industrial

    #      Northeast

    #         Distillate Fuel Oil
    # T149(145,IY,IS)=QDSIN(1,IY)+QDSIN(2,IY)
    z[145] = (dfd["QDSIN"].loc[1] + dfd["QDSIN"].loc[2]) * TRIL_TO_QUAD

    #         Propane
    # T149(146,IY,IS)=QPRIN(1,IY)+QPRIN(2,IY)
    z[146] = (dfd["QPRIN"].loc[1] + dfd["QPRIN"].loc[2]) * TRIL_TO_QUAD

    #         Residual Fuel Oil
    # T149(147,IY,IS)=QRSIN(1,IY)+QRSIN(2,IY)
    z[147] = (dfd["QRSIN"].loc[1] + dfd["QRSIN"].loc[2]) * TRIL_TO_QUAD

    #         Natural Gas
    # T149(148,IY,IS)=QNGIN(1,IY)+QNGIN(2,IY)
    z[148] = (dfd["QNGIN"].loc[1] + dfd["QNGIN"].loc[2]) * TRIL_TO_QUAD

    #         Coal
    # T149(149,IY,IS)=QCLIN(1,IY)+QCLIN(2,IY)
    z[149] = (
        dfd["QCLIN"].loc[1]
        + dfd["QCLIN"].loc[2]
    ) * TRIL_TO_QUAD

    #         Electricity
    # T149(150,IY,IS)=QELIN(1,IY)+QELIN(2,IY)
    z[150] = (dfd["QELIN"].loc[1] + dfd["QELIN"].loc[2]) * TRIL_TO_QUAD

    #      Midwest

    #         Distillate Fuel Oil
    # T149(151,IY,IS)=QDSIN(3,IY)+QDSIN(4,IY)
    z[151] = (dfd["QDSIN"].loc[3] + dfd["QDSIN"].loc[4]) * TRIL_TO_QUAD

    #         Propane
    # T149(152,IY,IS)=QPRIN(3,IY)+QPRIN(4,IY)
    z[152] = (dfd["QPRIN"].loc[3] + dfd["QPRIN"].loc[4]) * TRIL_TO_QUAD

    #         Residual Fuel Oil
    # T149(153,IY,IS)=QRSIN(3,IY)+QRSIN(4,IY)
    z[153] = (dfd["QRSIN"].loc[3] + dfd["QRSIN"].loc[4]) * TRIL_TO_QUAD

    #         Natural Gas
    # T149(154,IY,IS)=QNGIN(3,IY)+QNGIN(4,IY)
    z[154] = (dfd["QNGIN"].loc[3] + dfd["QNGIN"].loc[4]) * TRIL_TO_QUAD

    #         Coal
    # T149(155,IY,IS)=QCLIN(3,IY)+QCLIN(4,IY)
    z[155] = (
        dfd["QCLIN"].loc[3]
        + dfd["QCLIN"].loc[4]
    ) * TRIL_TO_QUAD

    #         Electricity
    # T149(156,IY,IS)=QELIN(3,IY)+QELIN(4,IY)
    z[156] = (dfd["QELIN"].loc[3] + dfd["QELIN"].loc[4]) * TRIL_TO_QUAD

    #      South

    #         Distillate Fuel Oil
    # T149(157,IY,IS)=QDSIN(5,IY)+QDSIN(6,IY)+QDSIN(7,IY)
    z[157] = (
        dfd["QDSIN"].loc[5] + dfd["QDSIN"].loc[6] + dfd["QDSIN"].loc[7]
    ) * TRIL_TO_QUAD

    #         Propane
    # T149(158,IY,IS)=QPRIN(5,IY)+QPRIN(6,IY)+QPRIN(7,IY)
    z[158] = (
        dfd["QPRIN"].loc[5] + dfd["QPRIN"].loc[6] + dfd["QPRIN"].loc[7]
    ) * TRIL_TO_QUAD

    #         Residual Fuel Oil
    # T149(159,IY,IS)=QRSIN(5,IY)+QRSIN(6,IY)+QRSIN(7,IY)
    z[159] = (
        dfd["QRSIN"].loc[5] + dfd["QRSIN"].loc[6] + dfd["QRSIN"].loc[7]
    ) * TRIL_TO_QUAD

    #         Natural Gas
    # T149(160,IY,IS)=QNGIN(5,IY)+QNGIN(6,IY)+QNGIN(7,IY)
    z[160] = (
        dfd["QNGIN"].loc[5] + dfd["QNGIN"].loc[6] + dfd["QNGIN"].loc[7]
    ) * TRIL_TO_QUAD

    #         Coal
    # T149(161,IY,IS)=QCLIN(5,IY)+QCLIN(6,IY)+QCLIN(7,IY)
    z[161] = (
        dfd["QCLIN"].loc[5]
        + dfd["QCLIN"].loc[6]
        + dfd["QCLIN"].loc[7]
    ) * TRIL_TO_QUAD

    #         Electricity
    # T149(162,IY,IS)=QELIN(5,IY)+QELIN(6,IY)+QELIN(7,IY)
    z[162] = (
        dfd["QELIN"].loc[5] + dfd["QELIN"].loc[6] + dfd["QELIN"].loc[7]
    ) * TRIL_TO_QUAD

    #      West

    #         Distillate Fuel Oil
    # T149(163,IY,IS)=QDSIN(8,IY)+QDSIN(9,IY)
    z[163] = (dfd["QDSIN"].loc[8] + dfd["QDSIN"].loc[9]) * TRIL_TO_QUAD

    #         Propane
    # T149(164,IY,IS)=QPRIN(8,IY)+QPRIN(9,IY)
    z[164] = (dfd["QPRIN"].loc[8] + dfd["QPRIN"].loc[9]) * TRIL_TO_QUAD

    #         Residual Fuel Oil
    # T149(165,IY,IS)=QRSIN(8,IY)+QRSIN(9,IY)
    z[165] = (dfd["QRSIN"].loc[8] + dfd["QRSIN"].loc[9]) * TRIL_TO_QUAD

    #         Natural Gas
    # T149(166,IY,IS)=QNGIN(8,IY)+QNGIN(9,IY)
    z[166] = (dfd["QNGIN"].loc[8] + dfd["QNGIN"].loc[9]) * TRIL_TO_QUAD

    #         Coal
    # T149(167,IY,IS)=QCLIN(8,IY)+QCLIN(9,IY)
    z[167] = (
        dfd["QCLIN"].loc[8]
        + dfd["QCLIN"].loc[9]
    ) * TRIL_TO_QUAD

    #         Electricity
    # T149(168,IY,IS)=QELIN(8,IY)+QELIN(9,IY)
    z[168] = (dfd["QELIN"].loc[8] + dfd["QELIN"].loc[9]) * TRIL_TO_QUAD

    #

    #   Transportation

    #      Northeast

    #         Motor Gasoline
    # T149(169,IY,IS)=QMGTR(1,IY)+QMGTR(2,IY)-QMGBS(1,IY)-QMGBS(2,IY)
    z[169] = (
        dfd["QMGTR"].loc[1]
        + dfd["QMGTR"].loc[2]
        - dfd["QMGBS"].loc[1]
        - dfd["QMGBS"].loc[2]
    ) * TRIL_TO_QUAD

    #         E85
    # T149(170,IY,IS)=QETTR(1,IY)+QETTR(2,IY)
    z[170] = (dfd["QETTR"].loc[1] + dfd["QETTR"].loc[2]) * TRIL_TO_QUAD

    #      Midwest

    #         Motor Gasoline
    # T149(171,IY,IS)=QMGTR(3,IY)+QMGTR(4,IY)-QMGBS(3,IY)-QMGBS(4,IY)
    z[171] = (
        dfd["QMGTR"].loc[3]
        + dfd["QMGTR"].loc[4]
        - dfd["QMGBS"].loc[3]
        - dfd["QMGBS"].loc[4]
    ) * TRIL_TO_QUAD

    #         E85
    # T149(172,IY,IS)=QETTR(3,IY)+QETTR(4,IY)
    z[172] = (dfd["QETTR"].loc[3] + dfd["QETTR"].loc[4]) * TRIL_TO_QUAD

    #      South

    #         Motor Gasoline
    # T149(173,IY,IS)=QMGTR(5,IY)+QMGTR(6,IY)+QMGTR(7,IY)-QMGBS(5,IY)-QMGBS(6,IY)-QMGBS(7,IY)
    z[173] = (
        dfd["QMGTR"].loc[5]
        + dfd["QMGTR"].loc[6]
        + dfd["QMGTR"].loc[7]
        - dfd["QMGBS"].loc[5]
        - dfd["QMGBS"].loc[6]
        - dfd["QMGBS"].loc[7]
    ) * TRIL_TO_QUAD

    #         E85
    # T149(174,IY,IS)=QETTR(5,IY)+QETTR(6,IY)+QETTR(7,IY)
    z[174] = (
        dfd["QETTR"].loc[5] + dfd["QETTR"].loc[6] + dfd["QETTR"].loc[7]
    ) * TRIL_TO_QUAD

    #      West

    #         Motor Gasoline
    # T149(175,IY,IS)=QMGTR(8,IY)+QMGTR(9,IY)-QMGBS(8,IY)-QMGBS(9,IY)
    z[175] = (
        dfd["QMGTR"].loc[8]
        + dfd["QMGTR"].loc[9]
        - dfd["QMGBS"].loc[8]
        - dfd["QMGBS"].loc[9]
    ) * TRIL_TO_QUAD

    #         E85
    # T149(176,IY,IS)=QETTR(8,IY)+QETTR(9,IY)
    z[176] = (dfd["QETTR"].loc[8] + dfd["QETTR"].loc[9]) * TRIL_TO_QUAD

    #

    #   Electric Power Biomass

    #      Northeast
    # T149(177,IY,IS)=QBMEL(1,IY)+QBMEL(2,IY)
    z[177] = (dfd["QBMEL"].loc[1] + dfd["QBMEL"].loc[2]) * TRIL_TO_QUAD

    #      Midwest
    # T149(178,IY,IS)=QBMEL(3,IY)+QBMEL(4,IY)
    z[178] = (dfd["QBMEL"].loc[3] + dfd["QBMEL"].loc[4]) * TRIL_TO_QUAD

    #      South
    # T149(179,IY,IS)=QBMEL(5,IY)+QBMEL(6,IY)+QBMEL(7,IY)
    z[179] = (
        dfd["QBMEL"].loc[5] + dfd["QBMEL"].loc[6] + dfd["QBMEL"].loc[7]
    ) * TRIL_TO_QUAD

    #      West
    # T149(180,IY,IS)=QBMEL(8,IY)+QBMEL(9,IY)
    z[180] = (dfd["QBMEL"].loc[8] + dfd["QBMEL"].loc[9]) * TRIL_TO_QUAD

    return z
