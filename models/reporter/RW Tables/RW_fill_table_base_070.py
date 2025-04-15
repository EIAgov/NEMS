# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM

SZO fixed on 8/31/2024

"""

import pandas as pd


def fill_table_base_070(dfd, table_spec, table_id):
    """Fill table  Components of Selected Petroleum Product Prices

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

    # Create MUFTAX with 11 repeative rows for each M15 index
    df = dfd["MUFTAX"]
    MUFTAX = df.loc[df.index.repeat(11)].reset_index()
    MUFTAX["MNUMCR"] = MUFTAX.groupby("M15").cumcount() + 1
    # Set MultiIndex
    MUFTAX.set_index(["M15", "MNUMCR"], inplace=True)

    # Create WTI_PRICE with 11 regions
    WTI_PRICE = pd.DataFrame([dfd["WTI_PRICE"]] * 11).reset_index(drop=True)
    WTI_PRICE.index = WTI_PRICE.index + 1
    WTI_PRICE.rename_axis("MNUMCR", inplace=True)

    # Create BRENT_PRICE with 11 regions
    BRENT_PRICE = pd.DataFrame([dfd["BRENT_PRICE"]] * 11).reset_index(drop=True)
    BRENT_PRICE.index = WTI_PRICE.index
    BRENT_PRICE.rename_axis("MNUMCR", inplace=True)

    # Create IT_WOP with 11 repeative rows for each M2 index
    df = dfd["IT_WOP"]
    IT_WOP = df.loc[df.index.repeat(11)].reset_index()
    IT_WOP["MNUMCR"] = IT_WOP.groupby("M2").cumcount() + 1
    # Set MultiIndex
    IT_WOP.set_index(["M2", "MNUMCR"], inplace=True)

    # Create RFSCREDPRC with 11 repeative rows for each M4 index
    df = dfd["RFSCREDPRC"]
    RFSCREDPRC = df.loc[df.index.repeat(11)].reset_index()
    RFSCREDPRC["MNUMCR"] = RFSCREDPRC.groupby("M4").cumcount() + 1
    # Set MultiIndex
    RFSCREDPRC.set_index(["M4", "MNUMCR"], inplace=True)

    #   Components of Selected Petroleum Product Prices
    #   (#### dollars per gallon)
    #    Product Price Components
    #
    #     End-User Price
    # T70(1,IR,IY,IS) = PDSTRHWY(IR,IY) * CFDSTRHWY(IY)/42.
    z[1] = (
        dfd["APONROAD/PDSTRHWY"]
        * dfd["CFDSTRHWY"].values
        / dfd["GAL_PER_BBL_rwpre"]
        * SCALPR2
    )

    # #       Federal Taxes
    # #T70(2,IR,IY,IS)=MUFTAX(IY,10)/MC_JPGDP(IY)*CFDSTRHWY(IY)/42.
    # z[2] = dfd['MUFTAX'].loc[10]/dfd['MC_JPGDP'] * dfd['CFDSTRHWY'] / dfd['GAL_PER_BBL_rwpre'] * SCALPR2
    # df = dfd['MUFTAX'].loc[10] / MC_JPGDP * dfd['CFDSTRHWY'] / dfd['GAL_PER_BBL_rwpre'] * SCALPR2
    # z[2] = pd.concat([df] * 11, ignore_index=True)
    # z[2].index = z[2].index+1
    z[2] = (
        MUFTAX.loc[10]
        / MC_JPGDP.values
        * dfd["CFDSTRHWY"].values
        / dfd["GAL_PER_BBL_rwpre"]
        * SCALPR2
    )

    #       State Taxes
    # T70(3,IR,IY,IS)=DSMUTR(IR,IY,2)*CFDSTRHWY(IY)/42.
    # z[3] = dfd['DSMUTR'].loc[2] * dfd['CFDSTRHWY'] / dfd['GAL_PER_BBL_rwpre'] * SCALPR2
    z[3] = (
        dfd["DSMUTR"].loc[(slice(None), 2), :].groupby(level=0).sum()
        * dfd["CFDSTRHWY"].values
        / dfd["GAL_PER_BBL_rwpre"]
        * SCALPR2
    )

    #       Energy Tax/Allowance Fee
    # T70(50,IR,IY,IS)=(ADSTR(IR,IY)-PDSTR(IR,IY))*CFDSTRHWY(IY)/42.
    # z[50] = (dfd['ADSTR'] - dfd['PDSTR']) * dfd['CFDSTRHWY'] / dfd['GAL_PER_BBL_rwpre'] * SCALPR2
    z[50] = (
        (dfd["AMPBLK/PDSTR"] - dfd["MPBLK/PDSTR"])
        * dfd["CFDSTRHWY"].values
        / dfd["GAL_PER_BBL_rwpre"]
        * SCALPR2
    )

    #       Distribution Costs
    # T70(4,IR,IY,IS)=DSMUTR(IR,IY,1)*CFDSTRHWY(IY)/42.
    # z[4] = dfd['DSMUTR'].loc[1] * dfd['CFDSTRHWY'] / dfd['GAL_PER_BBL_rwpre'] * SCALPR2
    z[4] = (
        dfd["DSMUTR"].loc[(slice(None), 1), :].groupby(level=0).sum()
        * dfd["CFDSTRHWY"].values
        / dfd["GAL_PER_BBL_rwpre"]
        * SCALPR2
    )

    #       Wholesale Price
    # T70(5,IR,IY,IS)=T70(1,IR,IY,IS)-FSUM(T70(2,IR,IY,IS),3)-T70(50,IR,IY,IS)
    z[5] = z[1] - z[2] - z[3] - z[4] - z[50]

    #       RFS Contribution
    # T70(45,IR,IY,IS)=RFSDSTR(IR,IY)/42.
    z[45] = dfd["RFSDSTR"] / dfd["GAL_PER_BBL_rwpre"] * SCALPR2

    #       WTI-based Margin
    # T70(20,IR,IY,IS)=T70(5,IR,IY,IS)-WTI_PRICE(IY)/42.
    # z[20] = z[5] - dfd['WTI_PRICE'] / dfd['GAL_PER_BBL_rwpre'] * SCALPR2
    z[20] = z[5] - dfd["WTI_PRICE"].values / dfd["GAL_PER_BBL_rwpre"] * SCALPR2

    #       IRAC-based Margin
    # T70(27,IR,IY,IS)=T70(5,IR,IY,IS)-IT_WOP(IY,1)/42.
    # z[27] = z[5] - dfd['IT_WOP'].loc[1] / dfd['GAL_PER_BBL_rwpre'] * SCALPR2
    z[27] = z[5] - dfd["IT_WOP"].loc[1] / dfd["GAL_PER_BBL_rwpre"] * SCALPR2

    #       Wholesale Price - RFS Contribution
    # T70(32,IR,IY,IS)=T70(5,IR,IY,IS)-T70(45,IR,IY,IS)
    z[32] = z[5] - z[45]

    #   Motor Gasoline (All Sectors)

    #     End-User Price
    # T70(6,IR,IY,IS)=PMGAS(IR,IY)*CFMGQ(IY)/42.
    z[6] = (
        dfd["AMPBLK/PMGAS"] * dfd["CFMGQ"].values / dfd["GAL_PER_BBL_rwpre"] * SCALPR2
    )

    #       Federal Taxes
    # T70(7,IR,IY,IS)=MUFTAX(IY,2)/MC_JPGDP(IY)*CFMGQ(IY)/42.
    z[7] = (
        MUFTAX.loc[2]
        / MC_JPGDP.values
        * dfd["CFMGQ"].values
        / dfd["GAL_PER_BBL_rwpre"]
        * SCALPR2
    )

    #       State Taxes 1/
    # T70(8,IR,IY,IS)=MGMUTR(IR,IY,2)*CFMGQ(IY)/42.
    z[8] = (
        dfd["MGMUTR"].loc[(slice(None), 2), :].groupby(level=0).sum()
        * dfd["CFMGQ"].values
        / dfd["GAL_PER_BBL_rwpre"]
        * SCALPR2
    )

    #       Energy Tax/Allowance Fee
    # T70(51,IR,IY,IS)=(AMGTR(IR,IY)-PMGTR(IR,IY))*CFMGQ(IY)/42.
    z[51] = (
        (dfd["AMPBLK/PMGTR"] - dfd["MPBLK/PMGTR"])
        * dfd["CFMGQ"].values
        / dfd["GAL_PER_BBL_rwpre"]
        * SCALPR2
    )

    #       Distribution Costs
    # T70(9,IR,IY,IS)=MGMUTR(IR,IY,1)*CFMGQ(IY)/42.
    z[9] = (
        dfd["MGMUTR"].loc[(slice(None), 1), :].groupby(level=0).sum()
        * dfd["CFMGQ"].values
        / dfd["GAL_PER_BBL_rwpre"]
        * SCALPR2
    )

    #       Wholesale Price
    # T70(10,IR,IY,IS)=T70(6,IR,IY,IS)-FSUM(T70(7,IR,IY,IS),3)-T70(51,IR,IY,IS)
    z[10] = z[6] - z[7] - z[8] - z[9] - z[51]

    #       RFS Contribution
    # T70(46,IR,IY,IS)=RFSMGTR(IR,IY)/42.
    z[46] = dfd["RFSMGTR"] / dfd["GAL_PER_BBL_rwpre"] * SCALPR2

    #       WTI-based Margin
    # T70(21,IR,IY,IS)=T70(10,IR,IY,IS)-WTI_PRICE(IY)/42.
    # WTI_PRICE = pd.concat([dfd['WTI_PRICE']] * 11, ignore_index=True)
    # WTI_PRICE.index = WTI_PRICE.index +1
    z[21] = z[10] - WTI_PRICE * SCALPR2 / dfd["GAL_PER_BBL_rwpre"]

    #       IRAC-based Margin
    # T70(28,IR,IY,IS)=T70(10,IR,IY,IS)-IT_WOP(IY,1)/42.
    z[28] = z[10] - dfd["IT_WOP"].loc[1] * SCALPR2 / dfd["GAL_PER_BBL_rwpre"]

    #       Wholesale Price - RFS Contribution
    # T70(35,IR,IY,IS)=T70(10,IR,IY,IS)-T70(46,IR,IY,IS)
    z[35] = z[10] - z[46]

    #   RBOB (All Sectors)

    #       Wholesale Price
    # T70(37,IR,IY,IS)=(WS_RBOB(IR,IY)+RFENVFX(IR,IY,2))/42.
    z[37] = (
        (
            dfd["WS_RBOB"]
            + dfd["RFENVFX"].loc[(slice(None), 2), :].groupby(level=0).sum()
        )
        / dfd["GAL_PER_BBL_rwpre"]
        * SCALPR2
    )

    z[37].to_excel("z18.xlsx")

    #       RFS Contribution
    # T70(47,IR,IY,IS)=RFSRBOB(IR,IY)/42.
    z[47] = dfd["RFSRBOB"] / dfd["GAL_PER_BBL_rwpre"] * SCALPR2

    #       WTI-based Margin
    # T70(38,IR,IY,IS)=T70(37,IR,IY,IS)-WTI_PRICE(IY)/42.
    z[38] = z[37] - WTI_PRICE / dfd["GAL_PER_BBL_rwpre"] * SCALPR2

    #       IRAC-based Margin
    # T70(54,IR,IY,IS)=T70(37,IR,IY,IS)-IT_WOP(IY,1)/42.
    z[54] = z[37] - dfd["IT_WOP"].loc[1] / dfd["GAL_PER_BBL_rwpre"] * SCALPR2

    #       WTI-based Margin net RFS
    # T70(38,IR,IY,IS)=T70(37,IR,IY,IS)-WTI_PRICE(IY)/42.
    z[39] = z[37] - WTI_PRICE / dfd["GAL_PER_BBL_rwpre"] * SCALPR2

    #   Jet Fuel

    #     End-User Price
    # T70(11,IR,IY,IS)=PJFTR(IR,IY)*CFJFK/42.
    z[11] = (
        dfd["AMPBLK/PJFTR"]
        * dfd["CFJFK"].squeeze()
        / dfd["GAL_PER_BBL_rwpre"]
        * SCALPR2
    )

    #       Federal Taxes
    # T70(12,IR,IY,IS)=MUFTAX(IY,3)/MC_JPGDP(IY)*CFJFK/42.
    z[12] = (
        MUFTAX.loc[3]
        / MC_JPGDP.values
        * dfd["CFJFK"].squeeze()
        / dfd["GAL_PER_BBL_rwpre"]
        * SCALPR2
    )

    #       State Taxes
    # T70(13,IR,IY,IS)=JFMUTR(IR,IY,2)*CFJFK/42.
    z[13] = (
        dfd["JFMUTR"].loc[(slice(None), 2), :].groupby(level=0).sum()
        * dfd["CFJFK"].squeeze()
        / dfd["GAL_PER_BBL_rwpre"]
        * SCALPR2
    )

    #       Energy Tax/Allowance Fee
    # T70(52,IR,IY,IS)=(AJFTR(IR,IY)-PJFTR(IR,IY))*CFJFK/42.
    z[52] = (
        (dfd["AMPBLK/PJFTR"] - dfd["MPBLK/PJFTR"])
        * dfd["CFJFK"].squeeze()
        / dfd["GAL_PER_BBL_rwpre"]
        * SCALPR2
    )

    #       Distribution Costs
    # T70(14,IR,IY,IS)=JFMUTR(IR,IY,1)*CFJFK/42.
    z[14] = (
        dfd["JFMUTR"].loc[(slice(None), 1), :].groupby(level=0).sum()
        * dfd["CFJFK"].squeeze()
        / dfd["GAL_PER_BBL_rwpre"]
        * SCALPR2
    )

    #       Wholesale Price
    # T70(15,IR,IY,IS)=T70(11,IR,IY,IS)-FSUM(T70(12,IR,IY,IS),3)-T70(52,IR,IY,IS)
    z[15] = z[11] - z[12] - z[13] - z[14] - z[52]

    #       RFS Contribution
    # T70(48,IR,IY,IS)=RFSJFTR(IR,IY)/42.
    z[48] = dfd["RFSJFTR"] / dfd["GAL_PER_BBL_rwpre"] * SCALPR2

    #       WTI-based Margin
    # T70(22,IR,IY,IS)=T70(15,IR,IY,IS)-WTI_PRICE(IY)/42.
    z[22] = z[15] - WTI_PRICE * SCALPR2 / dfd["GAL_PER_BBL_rwpre"]

    #       IRAC-based Margin
    # T70(29,IR,IY,IS)=T70(15,IR,IY,IS)-IT_WOP(IY,1)/42.
    z[29] = z[15] - dfd["IT_WOP"].loc[1] * SCALPR2 / dfd["GAL_PER_BBL_rwpre"]

    #       Wholesale Price - RFS Contribution
    # T70(36,IR,IY,IS)=T70(15,IR,IY,IS)-T70(48,IR,IY,IS)
    z[36] = z[15] - z[48]

    #   Residential Distillate Fuel Oil/ Heating Oil

    #     End-User Price
    # T70(16,IR,IY,IS)=PDSRS(IR,IY)*CFDSRS(IY)/42.
    z[16] = (
        dfd["AMPBLK/PDSRS"] * dfd["CFDSRS"].values / dfd["GAL_PER_BBL_rwpre"] * SCALPR2
    )

    #       Energy Tax/Allowance Fee
    # T70(53,IR,IY,IS) =(ADSRS(IR,IY) - PDSRS(IR,IY))*CFDSRS(IY)/42.
    z[53] = (
        (dfd["AMPBLK/PDSRS"] - dfd["MPBLK/PDSRS"])
        * dfd["CFDSRS"]
        / dfd["GAL_PER_BBL_rwpre"]
        * SCALPR2
    )

    #       Distribution Costs
    # T70(17,IR,IY,IS)=DSMURS(IR,IY,1)*CFDSRS(IY)/42.
    z[17] = (
        dfd["DSMURS"].loc[(slice(None), 1), :].groupby(level=0).sum()
        * dfd["CFDSRS"].values
        / dfd["GAL_PER_BBL_rwpre"]
        * SCALPR2
    )

    #       Wholesale Price
    # T70(18,IR,IY,IS)=T70(16,IR,IY,IS)-FSUM(T70(17,IR,IY,IS),1)-T70(53,IR,IY,IS)
    z[18] = z[16] - z[17] - z[53]

    z[18].to_excel("z18.xlsx")

    #       RFS Contribution
    # T70(49,IR,IY,IS)=RFSDSRS(IR,IY)/42.
    z[49] = dfd["RFSDSRS"] / dfd["GAL_PER_BBL_rwpre"] * SCALPR2

    #       WTI-based Margin
    # T70(23,IR,IY,IS)=T70(18,IR,IY,IS)-WTI_PRICE(IY)/42.
    z[23] = z[18] - WTI_PRICE / dfd["GAL_PER_BBL_rwpre"] * SCALPR2

    #       IRAC-based Margin
    # T70(30,IR,IY,IS)=T70(18,IR,IY,IS)-IT_WOP(IY,1)/42.
    z[30] = z[18] - dfd["IT_WOP"].loc[1] / dfd["GAL_PER_BBL_rwpre"] * SCALPR2

    #       Wholesale Price - RFS Contribution
    # T70(40,IR,IY,IS)=T70(18,IR,IY,IS)-T70(49,IR,IY,IS)
    z[40] = z[18] - z[49]

    #   Average Residual Fuel End User Price
    # T70(34,IR,IY,IS)=PRSAS(IR,IY)*CFRSQ/42.
    z[34] = (
        dfd["AMPBLK/PRSAS"]
        * dfd["CFRSQ"].squeeze()
        / dfd["GAL_PER_BBL_rwpre"]
        * SCALPR2
    )

    #   Residual Fuel End User Price - WTI
    # T70(26,IR,IY,IS)=PRSAS(IR,IY)*CFRSQ/42.-WTI_PRICE(IY)/42.
    z[26] = (
        dfd["AMPBLK/PRSAS"] * dfd["CFRSQ"].squeeze() / dfd["GAL_PER_BBL_rwpre"]
        - WTI_PRICE / dfd["GAL_PER_BBL_rwpre"]
    ) * SCALPR2

    #   Residual Fuel End User Price - IRAC
    # T70(31,IR,IY,IS)=PRSAS(IR,IY)*CFRSQ/42.-IT_WOP(IY,1)/42.
    z[31] = (
        dfd["AMPBLK/PRSAS"] * dfd["CFRSQ"].squeeze() / dfd["GAL_PER_BBL_rwpre"]
        - dfd["IT_WOP"].loc[1] / dfd["GAL_PER_BBL_rwpre"]
    ) * SCALPR2

    #   Brent Spot Price
    # T70(57,IR,IY,IS)=BRENT_PRICE(IY)/42.
    z[57] = BRENT_PRICE / dfd["GAL_PER_BBL_rwpre"] * SCALPR2

    #   West Texas Intermediate Spot Price
    # T70(19,IR,IY,IS)=WTI_PRICE(IY)/42.
    z[19] = WTI_PRICE / dfd["GAL_PER_BBL_rwpre"] * SCALPR2

    #   Imported Crude Oil Price 2/
    # T70(33,IR,IY,IS)=IT_WOP(IY,1)/42.
    z[33] = IT_WOP.loc[1] / dfd["GAL_PER_BBL_rwpre"] * SCALPR2

    #   Aggregate WTI-Based Refinery Margin

    #      Standard 3-2-1 Crack Spread (Crude/Gas/#2)
    # T70(24,IR,IY,IS) = (T70(37, 2,IY,IS)*2*42 + T70(18, 2,IY,IS)*42 - WTI_PRICE(IY)*3)/3
    # z[24] = (z[37].loc[2]*2*42 + z[18].loc[2]*42 - WTI_PRICE*3)/3
    z37_2 = pd.concat([z[37].loc[2].to_frame().T] * 11, ignore_index=True)
    z37_2.index = z37_2.index + 1
    z37_2.rename_axis("MNUMCR", inplace=True)

    z18_2 = pd.concat([z[18].loc[2].to_frame().T] * 11, ignore_index=True)
    z18_2.index = z18_2.index + 1
    z18_2.rename_axis("MNUMCR", inplace=True)

    z[24] = (z37_2 * 2 * 42 + z18_2 * 42 - WTI_PRICE * 3 * SCALPR2) / 3

    #      Regional 3-2-1 Crack Spread (Crude/Gas/#2)
    # T70(25,IR,IY,IS) = (T70(37,IR,IY,IS)*2*42 + T70(18,IR,IY,IS)*42 - WTI_PRICE(IY)*3)/3
    z[25] = (z[37] * 2 * 42 + z[18] * 42 - WTI_PRICE * 3 * SCALPR2) / 3

    #     Total
    # T70(41,IR,IY,IS)=RFSCREDPRC(1,IY)/42.
    z[41] = RFSCREDPRC.loc[1] / dfd["GAL_PER_BBL_rwpre"] * SCALPR2

    #     Advanced
    # T70(42,IR,IY,IS)=RFSCREDPRC(2,IY)/42.
    z[42] = RFSCREDPRC.loc[2] / dfd["GAL_PER_BBL_rwpre"] * SCALPR2

    #     Cellulosic
    # T70(43,IR,IY,IS)=RFSCREDPRC(3,IY)/42.
    z[43] = RFSCREDPRC.loc[3] / dfd["GAL_PER_BBL_rwpre"] * SCALPR2

    #     Biodiesel
    # T70(44,IR,IY,IS)=RFSCREDPRC(4,IY)/42.
    z[44] = RFSCREDPRC.loc[4] / dfd["GAL_PER_BBL_rwpre"] * SCALPR2

    #   Incremental Gross Profitability
    # T70(56,IR,IY,IS)=PROFIT_BBL(10,IY)/42.
    # Create PROFIT_BBL with 11 regions
    PROFIT_BBL = pd.concat(
        [dfd["PROFIT_BBL"].loc[10].to_frame().T] * 11, ignore_index=True
    )
    PROFIT_BBL.index = PROFIT_BBL.index + 1
    PROFIT_BBL.rename_axis("MNUMCR", inplace=True)
    z[56] = PROFIT_BBL / dfd["GAL_PER_BBL_rwpre"] * SCALPR2

    return z
