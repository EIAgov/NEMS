# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM

SZO rewrote code on 8/28/2024

"""

import numpy as np


def fill_table_base_093(dfd, table_spec, table_id):
    """Fill table Primary  Domestic Coal Supply, Disposition, and Prices

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

    MNUMLR = dfd["MNUMLR_rwpre"]

    #     import numpy as np
    z = {}

    #   Domestic Coal Supply, Disposition, and Prices
    #   (quantities in million short tons; prices in #### dollars pe
    #    Supply, Consumption, and Prices
    #
    #   Sources of Supply (million short tons)
    #     Distribution (excluding exports) from

    #       Appalachia
    # T93(1,IR,IY,IS)=COALPROD(IR,1,IY)+COALPROD(IR,2,IY)+COALPROD(IR,3,IY)
    # T93(1,IR,IY,IS)=COALPROD(IR,1,IY)+COALPROD(IR,2,IY) AEO2025 Change
    z[1] = dfd["COALPROD"].loc[(slice(None), (1, 2)), :].groupby(level=0).sum()

    #       Interior
    # T93(2,IR,IY,IS)=COALPROD(IR,4,IY)+COALPROD(IR,5,IY)+COALPROD(IR,6,IY)
    # T93(2,IR,IY,IS)=COALPROD(IR,3,IY)+COALPROD(IR,4,IY) AEO2025 Change
    z[2] = dfd["COALPROD"].loc[(slice(None), (3)), :].groupby(level=0).sum()
    #       Northern Great Plains
    # T93(3,IR,IY,IS)=COALPROD(IR,7,IY)+COALPROD(IR,8,IY)+COALPROD(IR,9,IY)+COALPROD(IR,10,IY)+COALPROD(IR,11,IY)
    # T93(3,IR,IY,IS)=COALPROD(IR,5,IY)+COALPROD(IR,6,IY) AEO2025 Change
    z[3] = dfd["COALPROD"].loc[(slice(None), (4)), :].groupby(level=0).sum()
    #       Other West and Non-Contiguous
    # T93(4,IR,IY,IS)=COALPROD(IR,12,IY)+COALPROD(IR,13,IY)+COALPROD(IR,14,IY)
    z[4] = dfd["COALPROD"].loc[(slice(None), (5)), :].groupby(level=0).sum()
    
    #Sources of Supply: Distribution: Other Western
    z[5] = dfd["COALPROD"].loc[(slice(None), (6)), :].groupby(level=0).sum()
    
    #         Total Distribution
    # T93(23,IR,IY,IS)=FSUM(T93(1,IR,IY,IS),5)
    z[23] = z[1] + z[2] + z[3] + z[4] + z[5]

    #     Waste Coal
    # T93(21,IR,IY,IS)=SUM(WC_DIST_ST(IR,1:MNUMLR,IY))
    # z[21] = dfd['WC_DIST_ST'].loc[1:MNUMLR].sum()
    z[21] = (
        dfd["WC_DIST_ST"]
        .loc[
            (
                slice(None),
                (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, MNUMLR),
            ),
            :,
        ]
        .groupby(level=0)
        .sum()
    )

    #     Imports
    # T93(6,IR,IY,IS)=CQDBFB(IR,7,IY)/CQDBFT(IR,7,IY)
    # z[6] = dfd['CQDBFB'].loc[7] /dfd['CQDBFT'].loc[7]
    z[6] = (
        (
            dfd["CQDBFB"].loc[(slice(None), 7), :]
            / dfd["CQDBFT"].loc[(slice(None), 7), :]
        )
        .groupby(level=0)
        .sum()
        .fillna(0)
    )

    #   Total Supply
    # T93(7,IR,IY,IS)=T93(5,IR,IY,IS)+T93(6,IR,IY,IS)+T93(21,IR,IY,IS)
    z[7] = z[6] + z[21] + z[23]

    #
    #   Consumption (million short tons)

    #     Commercial and Institutional
    # T93(8,IR,IY,IS)=(QCLRS(IR,IY)+QCLCM(IR,IY))/CQDBFT(IR,1,IY)*1000.
    z[8] = (dfd["QCLRS"] + dfd["QCLCM"]) / dfd["CQDBFT"].loc[:, 1, :]

    #     Coke Plants
    # T93(10,IR,IY,IS)=QMCIN(IR,IY)/CQDBFT(IR,3,IY)*1000.
    z[10] = dfd["QMCIN"] / dfd["CQDBFT"].loc[:, 3, :].replace(0, np.inf)

    #     Other Industrial excluding Coal to Liquids 1/
    # T93(9,IR,IY,IS)=(QCLIN(IR,IY))/CQDBFT(IR,2,IY)*1000.
    z[9] = ((dfd["QCLIN"]) / dfd["CQDBFT"].loc[:, 2, :]).fillna(0)


    #     Coal-to-Liquids Liquids Production
    # T93(19,IR,IY,IS)=(QCLSN(IR,IY))/CQDBFT(IR,4,IY)*1000.
    z[19] = (
        ((dfd["QCLSN"]) / dfd["CQDBFT"].loc[(slice(None), 4), :])
        .groupby(level=0)
        .sum()
    )  # * 1000.

    #     Electric Power 2/
    # T93(11,IR,IY,IS)=QCLEL(IR,IY)/CQDBFT(IR,6,IY)*1000.
    z[11] = (
        (dfd["QCLEL"] / dfd["CQDBFT"].xs((slice(None), 6))).groupby(level=0).sum()
    )  # * 1000.
    z[11].replace([np.inf, -np.inf], 0, inplace=True)

    #   Total Coal Consumption
    # T93(12,IR,IY,IS)=FSUM(T93(8,IR,IY,IS),4)+FSUM(T93(18,IR,IY,IS),2)
    z[12] = z[8] + z[9] + z[10] + z[11] + z[19]

    #

    #   Discrepancy 3/
    # T93(13,IR,IY,IS)=T93(7,IR,IY,IS)-T93(12,IR,IY,IS)
    z[13] = (z[7] - z[12]).replace([np.inf, -np.inf], 0).fillna(0)

    #
    #   Delivered Prices
    #   (#### dollars per short ton)

    #     Commercial and Institutional
    # T93(22,IR,IY,IS)=PCLCM(IR,IY)*CPDBFT(IR,1,IY)
    z[22] = (
        dfd["AMPBLK/PCLCM"]
        * dfd["CPDBFT"].loc[(slice(None), 1), :].groupby(level=0).sum()
        * SCALPR2
    )

    #     Coke Plants
    # T93(15,IR,IY,IS)=PMCIN(IR,IY)*CPDBFT(IR,3,IY)
    z[15] = (
        dfd["AMPBLK/PMCIN"]
        * dfd["CPDBFT"].loc[(slice(None), 3), :].groupby(level=0).sum()
        * SCALPR2
    )

    #     Other Industrial excluding Coal to Liquids 1/
    # T93(14,IR,IY,IS)=PCLIN(IR,IY)*CPDBFT(IR,2,IY)
    z[14] = (
        dfd["AMPBLK/PCLIN"]
        * dfd["CPDBFT"].loc[(slice(None), 2), :].groupby(level=0).sum()
        * SCALPR2
    )

    #     Coal to Liquids
    # T93(20,IR,IY,IS)=PCLSN(IR,IY)*CPDBFT(IR,4,IY)
    z[20] = (
        dfd["AMPBLK/PCLSN"]
        * dfd["CPDBFT"].loc[(slice(None), 4), :].groupby(level=0).sum()
        * SCALPR2
    )

    #     Electric Power 2/
    # T93(16,IR,IY,IS)=PCLEL(IR,IY)*CPDBFT(IR,6,IY)
    z[16] = (
        dfd["AMPBLK/PCLEL"]
        * dfd["CPDBFT"].loc[(slice(None), 6), :].groupby(level=0).sum()
        * SCALPR2
    )

    # FFF = 0.0
    # RRR = 0.0
    # IF (CQDBFT(IR,1,IY) .NE. 0.0) THEN
    # RRR = RRR + PCLCM(IR,IY)*(QCLCM(IR,IY))
    # FFF = FFF + (QCLCM(IR,IY)) / CQDBFT(IR,1,IY)
    # ENDIF
    # IF (CQDBFT(IR,4,IY) .NE. 0.0) THEN
    # RRR = RRR + PCLSN(IR,IY)*CPDBFT(IR,4,IY)*QCLSN(IR,IY)/CQDBFT(IR,4,IY) !Imports are not an option for qclsn
    # FFF = FFF + QCLSN(IR,IY) / CQDBFT(IR,4,IY)
    # ENDIF
    # IF (CQDBFT(IR,2,IY) .NE. 0.0) THEN
    # RRR = RRR + PCLIN(IR,IY)*(QCLIN(IR,IY)-QCTLRF(IR,IY))
    # FFF = FFF +(QCLIN(IR,IY)-QCTLRF(IR,IY))/CQDBFT(IR,2,IY)
    # ENDIF
    # IF (CQDBFT(IR,3,IY) .NE. 0.0) THEN
    # RRR = RRR + PMCIN(IR,IY)*(QMCIN(IR,IY))  !Total dollars without imports
    # FFF = FFF + (QMCIN(IR,IY)) / CQDBFT(IR,3,IY)
    # ENDIF
    # IF (CQDBFT(IR,6,IY) .NE. 0.0) THEN
    # RRR = RRR + PCLEL(IR,IY)*(QCLEL(IR,IY))
    # FFF = FFF + (QCLEL(IR,IY)) / CQDBFT(IR,6,IY)
    # ENDIF
    # IF (FFF .NE. 0.0) T93(17,IR,IY,IS) = RRR / FFF
    RRR = (
        (dfd["AMPBLK/PCLCM"] * dfd["QCLCM"]).fillna(0)
        + (
            dfd["AMPBLK/PCLSN"]
            * dfd["CPDBFT"].loc[(slice(None), 4), :].groupby(level=0).sum()
            * dfd["QCLSN"]
            / dfd["CQDBFT"].loc[(slice(None), 4), :].groupby(level=0).sum()
        ).fillna(0)
        + dfd["AMPBLK/PCLIN"] * (dfd["QCLIN"])
        + dfd["AMPBLK/PMCIN"] * (dfd["QMCIN"])  # Total dollars without imports
        + dfd["AMPBLK/PCLEL"] * (dfd["QCLEL"])
    )
    FFF = (
        (
            dfd["QCLCM"] / dfd["CQDBFT"].loc[(slice(None), 1), :].groupby(level=0).sum()
        ).fillna(0)
        + (
            dfd["QCLSN"] / dfd["CQDBFT"].loc[(slice(None), 4), :].groupby(level=0).sum()
        ).fillna(0)
        + (
            (dfd["QCLIN"])
            / dfd["CQDBFT"].loc[(slice(None), 2), :].groupby(level=0).sum()
        ).fillna(0)
        + (
            dfd["QMCIN"] / dfd["CQDBFT"].loc[(slice(None), 3), :].groupby(level=0).sum()
        ).fillna(0)
        + (dfd["QCLEL"] / dfd["CQDBFT"].loc[(slice(None), 6), :].groupby(level=0).sum())
    ).fillna(0)
    RRR.replace([np.inf, -np.inf], 0, inplace=True)
    FFF.replace([np.inf, -np.inf], 0, inplace=True)

    z[17] = (RRR / FFF).fillna(0) * SCALPR2

    return z
