# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
Fixed by SZO on 7/1/20124
Fixed index issues on 7/23/2024
"""


def fill_table_base_015(dfd, table_spec, table_id):
    """Fill table   Coal Supply, Disposition, and Prices

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

    MNUMLR = dfd["MNUMLR_rwpre"]
    MNUMCR = dfd["MNUMCR_rwpre"]

    #   Coal Supply, Disposition, and Prices
    #   (million short tons, unless otherwise noted)
    #    Supply, Disposition, and Prices

    #   Production 1/

    #     Appalachia
    # T15(1,IY,IS)=APPSTOCKS(IY)+ABSULF(4,IY)+APSULF(4,IY)
    z[1] = dfd["APPSTOCKS"] + dfd["ABSULF"].loc[4] + dfd["APSULF"].loc[4]

    #     Interior
    # T15(2,IY,IS)=INTSTOCKS(IY)+IBSULF(4,IY)+ILSULF(4,IY)
    z[2] = dfd["INTSTOCKS"] + dfd["IBSULF"].loc[4] + dfd["ILSULF"].loc[4]

    #     West
    # T15(3,IY,IS)=WESTSTOCKS(IY)+WBSULF(4,IY)+WSSULF(4,IY)+WLSULF(4,IY)+WPSULF(4,IY)
    z[3] = (
        dfd["WESTSTOCKS"]
        + dfd["WBSULF"].loc[4]
        + dfd["WSSULF"].loc[4]
        + dfd["WLSULF"].loc[4]
        + dfd["WPSULF"].loc[4]
    )

    #

    #     East of the Mississippi
    # T15(4,IY,IS)=APPSTOCKS(IY)+INTSTOCKS(IY)+CQSBB(1,IY)/CQSBT(1,IY)
    z[4] = (
        dfd["APPSTOCKS"] + dfd["INTSTOCKS"] + dfd["CQSBB"].loc[1] / dfd["CQSBT"].loc[1]
    ).fillna(0)

    #     West of the Mississippi
    # T15(5,IY,IS)=WESTSTOCKS(IY)+CQSBB(2,IY)/CQSBT(2,IY)
    z[5] = (dfd["WESTSTOCKS"] + dfd["CQSBB"].loc[2] / dfd["CQSBT"].loc[2]).fillna(0)

    #       Total
    # T15(6,IY,IS)=APPSTOCKS(IY)+INTSTOCKS(IY)+WESTSTOCKS(IY)+CQSBB(3,IY)/CQSBT(3,IY)

    z[6] = (
        dfd["APPSTOCKS"]
        + dfd["INTSTOCKS"]
        + dfd["WESTSTOCKS"]
        + dfd["CQSBB"].loc[3] / dfd["CQSBT"].loc[3]
    ).fillna(0)

    #

    #   Waste Coal Supplied 2/
    # T15(29,IY,IS)=SUM(WC_PROD_ST(11,1:MNUMLR,IY))
    # z[29] = dfd['WC_PROD_ST'].loc[MNUMCR].loc[1:MNUMLR]
    z[29] = dfd["WC_PROD_ST"].loc[MNUMCR].loc[1:MNUMLR].sum()

    #

    #   Net Imports

    #     Imports 3/
    # T15(7,IY,IS)=CQDBFB(11,7,IY)/CQDBFT(11,7,IY)
    z[7] = (dfd["CQDBFB"].loc[MNUMCR].loc[7] / dfd["CQDBFT"].loc[MNUMCR].loc[7]).fillna(
        0
    )

    #     Exports
    # T15(8,IY,IS)=CQDBFB(11,5,IY)/CQDBFT(11,5,IY)
    z[8] = (dfd["CQDBFB"].loc[MNUMCR].loc[5] / dfd["CQDBFT"].loc[MNUMCR].loc[5]).fillna(
        0
    )

    #Steam Coal
    z[25] = dfd["WSCF"].loc[4].loc[2]
    
    #Metallurgical Coal
    z[26] = dfd["WMCF"].loc[4].loc[2]

    #       Total
    # T15(9,IY,IS)=T15(7,IY,IS)-T15(8,IY,IS)
    z[9] = z[7] - z[8]

    #

    #   Total Supply 4/
    # T15(10,IY,IS)=T15(6,IY,IS)+T15(9,IY,IS)+T15(29,IY,IS)
    z[10] = z[6] + z[9] + z[29]

    #

    #   Consumption by Sector

    #     Commercial and Institutional
    # T15(11,IY,IS)=(QCLRS(11,IY)+QCLCM(11,IY))/CQDBFT(11,1,IY)*1000.
    z[11] = (
        (dfd["QCLRS"].loc[MNUMCR] + dfd["QCLCM"].loc[MNUMCR])
        / dfd["CQDBFT"].loc[MNUMCR].loc[1]
    ).fillna(0)

    #     Coke Plants
    # T15(13,IY,IS)=INDCOKEBAL(IY)+QMCIN(11,IY)/CQDBFT(11,3,IY)*1000.
    z[13] = (
        dfd["INDCOKEBAL"] + dfd["QMCIN"].loc[MNUMCR] / dfd["CQDBFT"].loc[MNUMCR].loc[3]
    ).fillna(0)

    #     Other Industrial 5/
    # T15(12,IY,IS)=INDSTEAMBAL(IY)+(QCLIN(11,IY))/CQDBFT(11,2,IY)*1000.
    z[12] = (
        dfd["INDSTEAMBAL"]
        + (dfd["QCLIN"].loc[MNUMCR])
        / dfd["CQDBFT"].loc[MNUMCR].loc[2]
    ).fillna(0)


    #     Electric Power 6/
    # T15(14,IY,IS)=QCLEL(11,IY)/CQDBFT(11,6,IY)*1000.
    z[14] = (dfd["QCLEL"].loc[MNUMCR] / dfd["CQDBFT"].loc[MNUMCR].loc[6]).fillna(0)

    #       Total
    # T15(27,IY,IS)=FSUM(T15(11,IY,IS),4)+FSUM(T15(25,IY,IS),2)
    # z[27] = z[11] +z[12]+z[13] +z[14] +z[25] #+z[26]

    z[27] = z[11] + z[12] + z[13] + z[14] # need align

    #

    #   Discrepancy and Stock Change 7/
    # T15(16,IY,IS)=T15(10,IY,IS)-T15(27,IY,IS)
    z[16] = z[10] - z[27]

    #

    #   Average Minemouth Price 8/

    #     (#### dollars per short ton)
    # T15(17,IY,IS)=CPSB(3,IY)*CPSBT(3,IY)
    z[17] = dfd["CPSB"].loc[3] * dfd["CPSBT"].loc[3] * SCALPR2

    #     (#### dollars per million Btu)
    # T15(18,IY,IS)=CPSB(3,IY)
    z[18] = dfd["CPSB"].loc[3] * SCALPR2

    #

    #   Delivered Prices 9/

    #   (#### dollars per short ton)

    #     Commercial and Institutional
    # T15(39,IY,IS)=PCLCM(11,IY)*CPDBFT(11,1,IY)
    z[39] = dfd["AMPBLK/PCLCM"].loc[MNUMCR] * dfd["CPDBFT"].loc[MNUMCR].loc[1] * SCALPR2

    #     Coke Plants
    # T15(20,IY,IS)=PMCIN(11,IY)*CPDBFT(11,3,IY)
    z[20] = dfd["AMPBLK/PMCIN"].loc[MNUMCR] * dfd["CPDBFT"].loc[MNUMCR].loc[3] * SCALPR2

    #     Other Industrial 5/
    # T15(19,IY,IS)=PCLIN(11,IY)*CPDBFT(11,2,IY)
    z[19] = dfd["AMPBLK/PCLIN"].loc[MNUMCR] * dfd["CPDBFT"].loc[MNUMCR].loc[2] * SCALPR2

    #     Coal to Liquids
    # T15(28,IY,IS)=PCLSN(11,IY)*CPDBFT(11,4,IY)
    z[28] = dfd["AMPBLK/PCLSN"].loc[MNUMCR] * dfd["CPDBFT"].loc[MNUMCR].loc[4] * SCALPR2

    #     Electric Power 6/

    #       (#### dollars per short ton)
    # T15(21,IY,IS)=PCLEL(11,IY)*CPDBFT(11,6,IY)
    z[21] = dfd["AMPBLK/PCLEL"].loc[MNUMCR] * dfd["CPDBFT"].loc[MNUMCR].loc[6] * SCALPR2

    #       (#### dollars per million Btu)
    # T15(22,IY,IS)=PCLEL(11,IY)
    z[22] = dfd["AMPBLK/PCLEL"].loc[MNUMCR] * SCALPR2

    #         Average
    # T15(23,IY,IS)=(PCLCM(11,IY)*QCLCM(11,IY)+PCLSN(11,IY)*QCLSN(11,IY)+PCLIN(11,IY)*(QCLIN(11,IY))+PMCIN(11,IY)*(QMCIN(11,IY))+PCLEL(11,IY)*(QCLEL(11,IY)))/((QCLCM(11,IY))/CPDBFT(11,1,IY)+(QCLIN(11,IY))/CPDBFT(11,2,IY)+(QMCIN(11,IY))/CPDBFT(11,3,IY)+(QCLEL(11,IY))/CPDBFT(11,6,IY)+QCLSN(11,IY)/CPDBFT(11,4,IY))
    # z[23] = ((dfd['PCLCM'].loc[MNUMCR] * dfd['QCLCM'].loc[MNUMCR] + dfd['PCLSN'].loc[MNUMCR] * dfd['QCLSN'].loc[MNUMCR] + dfd['PCLIN'].loc[MNUMCR] * (dfd['QCLIN'].loc[MNUMCR]) + dfd['PMCIN'].loc[MNUMCR] * dfd['QMCIN'].loc[MNUMCR] + dfd['PCLEL'].loc[MNUMCR] * (dfd['QCLEL'].loc[MNUMCR])) / ( dfd['QCLCM'].loc[MNUMCR] / \
    #          dfd['CPDBFT'].loc[MNUMCR].loc[1] + (dfd['QCLIN'].loc[MNUMCR]) / dfd['CPDBFT'].loc[MNUMCR].loc[2] + (dfd['QMCIN'].loc[MNUMCR]) / dfd['CPDBFT'].loc[MNUMCR].loc[3] + (dfd['QCLEL'].loc[MNUMCR]) / dfd['CPDBFT'].loc[MNUMCR].loc[6] + dfd['QCLSN'].loc[MNUMCR] / dfd['CPDBFT'].loc[MNUMCR].loc[4])) * SCALPR2
    #         Average
    # T15(23,IY,IS)=(PCLCM(11,IY)*QCLCM(11,IY)+PCLSN(11,IY)*QCLSN(11,IY)+PCLIN(11,IY)*(QCLIN(11,IY))+PMCIN(11,IY)*(QMCIN(11,IY))+PCLEL(11,IY)*(QCLEL(11,IY)))/((QCLCM(11,IY))/CPDBFT(11,1,IY)+(QCLIN(11,IY))/CPDBFT(11,2,IY)+(QMCIN(11,IY))/CPDBFT(11,3,IY)+(QCLEL(11,IY))/CPDBFT(11,6,IY)+QCLSN(11,IY)/CPDBFT(11,4,IY))
    z[23] = (
        (
            dfd["AMPBLK/PCLCM"].loc[MNUMCR] * dfd["QCLCM"].loc[MNUMCR]
            + dfd["AMPBLK/PCLSN"].loc[MNUMCR] * dfd["QCLSN"].loc[MNUMCR]
            + dfd["AMPBLK/PCLIN"].loc[MNUMCR]
            * (dfd["QCLIN"].loc[MNUMCR])
            + dfd["AMPBLK/PMCIN"].loc[MNUMCR] * dfd["QMCIN"].loc[MNUMCR]
            + dfd["AMPBLK/PCLEL"].loc[MNUMCR] * (dfd["QCLEL"].loc[MNUMCR])
        )
        / (
            dfd["QCLCM"].loc[MNUMCR] / dfd["CPDBFT"].loc[MNUMCR].loc[1]
            + (dfd["QCLIN"].loc[MNUMCR])
            / dfd["CPDBFT"].loc[MNUMCR].loc[2]
            + (dfd["QMCIN"].loc[MNUMCR]) / dfd["CPDBFT"].loc[MNUMCR].loc[3]
            + (dfd["QCLEL"].loc[MNUMCR]) / dfd["CPDBFT"].loc[MNUMCR].loc[6]
            + dfd["QCLSN"].loc[MNUMCR]
            / dfd["CPDBFT"].loc[MNUMCR].loc[4].replace(0, 1e-9)
        )
    ) * SCALPR2

    #     Exports 10/
    # T15(24,IY,IS)=PCLEX(11,IY)*CPDBFT(11,5,IY)
    z[24] = dfd["PCLEX"].loc[MNUMCR] * dfd["CPDBFT"].loc[MNUMCR].loc[5] * SCALPR2

    #

    #   Average Minemouth Price 8/

    #     (nominal dollars per short ton)
    # T15(30,IY,IS)=T15(17,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[30] = z[17] / SCALPR2 * MC_JPGDP

    #     (nominal dollars per million Btu)
    # T15(31,IY,IS)=T15(18,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[31] = z[18] / SCALPR2 * MC_JPGDP

    #

    #   Delivered Prices 9/

    #   (nominal dollars per short ton)

    #     Commercial and Institutional
    # T15(40,IY,IS)=T15(39,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[40] = z[39] / SCALPR2 * MC_JPGDP

    #     Coke Plants
    # T15(32,IY,IS)=T15(20,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[32] = z[20] / SCALPR2 * MC_JPGDP

    #     Other Industrial 5/
    # T15(33,IY,IS)=T15(19,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[33] = z[19] / SCALPR2 * MC_JPGDP

    #     Coal to Liquids
    # T15(34,IY,IS)=T15(28,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[34] = z[28] / SCALPR2 * MC_JPGDP

    #     Electric Power 6/

    #       (nominal dollars per short ton)
    # T15(35,IY,IS)=T15(21,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[35] = z[21] / SCALPR2 * MC_JPGDP

    #       (nominal dollars per million Btu)
    # T15(36,IY,IS)=T15(22,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[36] = z[22] / SCALPR2 * MC_JPGDP

    #         Average
    # T15(37,IY,IS)=T15(23,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[37] = z[23] / SCALPR2 * MC_JPGDP

    #     Exports 10/
    # T15(38,IY,IS)=T15(24,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[38] = z[24] / SCALPR2 * MC_JPGDP

    return z
