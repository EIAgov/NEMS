# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
SZO fixed multi-index issues
"""


def fill_table_base_019(dfd, table_spec, table_id):
    """Fill table for International Petroleum and Other Liquids Supply, Dispositio

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

    ETHANOL_PARAM = dfd["ETHANOL_PARAM_rwpre"]
    MNUMOR = dfd["MNUMOR_rwpre"]
    MNUMPR = dfd["MNUMPR_rwpre"]
    MNUMCR = dfd["MNUMCR_rwpre"]
    RDAYS = dfd["RDAYS_rwpre"]

    #   International Petroleum and Other Liquids Supply, Dispositio
    #   (million barrels per day, unless otherwise noted)
    #    Supply, Disposition, and Prices

    #   Crude Oil Spot Prices
    #    (#### dollars per barrel)

    #     Brent
    # T19(66,IY,IS)=BRENT_PRICE(IY)
    z[66] = dfd["BRENT_PRICE"] * SCALPR2

    #     West Texas Intermediate
    # T19(65,IY,IS)=WTI_PRICE(IY)
    z[65] = dfd["WTI_PRICE"] * SCALPR2

    #    (nominal dollars per barrel)

    #     Brent
    # T19(68,IY,IS)=T19(66,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[68] = z[66] / SCALPR2 * MC_JPGDP

    #     West Texas Intermediate
    # T19(67,IY,IS)=T19(65,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[67] = z[65] / SCALPR2 * MC_JPGDP

    #

    #   Petroleum and Other Liquids Consumption 1/

    #      OECD

    #         United States (50 states)
    # T19(29,IY,IS)=RFQMG(11,IY)+RFQJF(11,IY)+RFQDS(11,IY)+RFQRL(11,IY)+RFQRH(11,IY)+RFQPF(11,IY)+RFQKS(11,IY)+RFQLG(11,IY)+RFQPCK(11,IY)+RFQARO(11,IY)+RFQSTG(11,IY)+RFQOTH(11,IY)
    z[29] = (
        dfd["RFQMG"].loc[MNUMCR]
        + dfd["RFQJF"].loc[MNUMCR]
        + dfd["RFQDS"].loc[MNUMCR]
        + dfd["RFQRL"].loc[MNUMCR]
        + dfd["RFQRH"].loc[MNUMCR]
        + dfd["RFQPF"].loc[MNUMCR]
        + dfd["RFQKS"].loc[MNUMCR]
        + dfd["RFQLG"].loc[MNUMCR]
        + dfd["RFQPCK"].loc[MNUMCR]
        + dfd["RFQARO"].loc[MNUMCR]
        + dfd["RFQSTG"].loc[MNUMCR]
        + dfd["RFQOTH"].loc[MNUMCR]
    )

    #         United States Territories
    # T19(1:57,IY,IS)=REPORT(IY,1:57)
    # T19(70:87,IY,IS) = REPORT2(IY,1:18)

    z[2] = dfd["REPORT"].loc[2]
    z[3] = dfd["REPORT"].loc[3]
    z[4] = dfd["REPORT"].loc[4]
    z[5] = dfd["REPORT"].loc[5]
    z[6] = dfd["REPORT"].loc[6]
    z[7] = dfd["REPORT"].loc[7]
    z[8] = dfd["REPORT"].loc[8]
    z[9] = dfd["REPORT"].loc[9]
    z[10] = dfd["REPORT"].loc[10]
    z[11] = dfd["REPORT"].loc[11]
    z[12] = dfd["REPORT"].loc[12]
    z[13] = dfd["REPORT"].loc[13]
    z[14] = dfd["REPORT"].loc[14]
    z[30] = dfd["REPORT"].loc[30]
    z[31] = dfd["REPORT"].loc[31]
    z[32] = dfd["REPORT"].loc[32]
    z[33] = dfd["REPORT"].loc[33]
    z[34] = dfd["REPORT"].loc[34]
    z[35] = dfd["REPORT"].loc[35]
    z[36] = dfd["REPORT"].loc[36]
    z[37] = dfd["REPORT"].loc[37]
    z[38] = dfd["REPORT"].loc[38]
    z[39] = dfd["REPORT"].loc[39]
    z[40] = dfd["REPORT"].loc[40]
    z[41] = dfd["REPORT"].loc[41]
    z[42] = dfd["REPORT"].loc[42]
    z[43] = dfd["REPORT"].loc[43]
    z[44] = dfd["REPORT"].loc[44]
    z[45] = dfd["REPORT"].loc[45]
    z[50] = dfd["REPORT"].loc[50]
    z[52] = z[29] + z[30] + z[31] + z[32] + z[33] + z[34] + z[35] + z[40]
    z[53] = (
        z[36]
        + z[37]
        + z[38]
        + z[39]
        + z[40]
        + z[41]
        + z[42]
        + z[43]
        + z[44]
        + z[45]
        - z[40]
    )
    z[54] = z[52] + z[53]
    z[16] = dfd["REPORT"].loc[16]
    z[17] = dfd["REPORT"].loc[17]
    z[18] = dfd["REPORT"].loc[18]
    z[19] = dfd["REPORT"].loc[19]
    z[46] = z[16] + z[17] + z[18] + z[19]

    # T19(1,IY,IS)=RFQTDCRD(MNUMOR+2,IY)+RFQNGPL(MNUMPR,IY,6)/1000.+RFQPRCG(MNUMPR,IY)+0.9751*(CRNETHCD(11,IY)+CLLETHCD(11,IY)+OTHETHCD(11,IY))/1000.+SUM(BIMQTYCD(1:4,11,IY))/1000.+SUM(GTLFRAC(1:4,MNUMPR,IY))/1000.+
    #     SUM(CTLFRAC(1:4,MNUMPR,IY))/1000+SUM(CBTLFRAC(1,1:4,MNUMPR,IY))/1000.+SUM(BTLFRAC(1:4,MNUMPR,IY))/1000+SUM(CBTLFRAC(2,1:4,MNUMPR,IY))/1000.+UBAVOL(MNUMPR,IY)/1000.+RFBIOBUTECD(MNUMCR,IY)/1000.+(GRD2DSQTY(MNUMPR,IY)+
    #     GRN2MGQTY(MNUMPR,IY))/1000.+RFHCXH2IN(MNUMPR,IY)+RFMETM85(MNUMPR,IY)

    # z[1] = dfd['RFQTDCRD'].loc[MNUMOR+2] + dfd['RFQNGPL'].loc[MNUMPR].loc[6] / 1000. + dfd['RFQPRCG'].loc[MNUMPR] + \
    #        ETHANOL_PARAM * (dfd['CRNETHCD'].loc[MNUMCR] + dfd['CLLETHCD'].loc[MNUMCR] + dfd['OTHETHCD'].loc[MNUMCR]) / 1000. + \
    #        dfd['BIMQTYCD'].loc[1:4,MNUMCR].sum() / 1000. + dfd['GTLFRAC'].loc[1:4,MNUMPR].sum() / 1000. + \
    #        dfd['CTLFRAC'].loc[1:4,MNUMPR].sum() / 1000+ dfd['CBTLFRAC'].loc[1,1:4,MNUMPR].sum() / 1000. + \
    #        dfd['BTLFRAC'].loc[1:4,MNUMPR].sum() / 1000. +  \
    #        dfd['CBTLFRAC'].loc[2,1:4,MNUMPR].sum() / 1000. + dfd['UBAVOL'].loc[MNUMPR] / 1000. + dfd['RFBIOBUTECD'].loc[MNUMCR] / 1000. + (dfd['GRD2DSQTY'].loc[MNUMPR] + \
    #        dfd['GRN2MGQTY'].loc[MNUMPR]) / 1000. + dfd['RFHCXH2IN'].loc[MNUMPR] + dfd['RFMETM85'].loc[MNUMPR]

    z[1] = (
        dfd["RFQTDCRD"].loc[MNUMOR + 2]
        + dfd["RFQNGPL"].loc[MNUMPR].loc[6] / 1000.0
        + dfd["RFQPRCG"].loc[MNUMPR]
        + ETHANOL_PARAM
        * (
            dfd["CRNETHCD"].loc[MNUMCR]
            + dfd["CLLETHCD"].loc[MNUMCR]
            + dfd["OTHETHCD"].loc[MNUMCR]
        )
        / 1000.0
        + dfd["BIMQTYCD"].loc[(slice(None), MNUMCR), :].sum() / 1000.0
        + dfd["GTLFRAC"].loc[(range(1, 5), MNUMPR), :].sum() / 1000.0
        + dfd["CTLFRAC"].loc[(range(1, 5), MNUMPR), :].sum() / 1000
        + dfd["CBTLFRAC"].loc[(1, range(1, 5), MNUMPR), :].sum() / 1000.0
        + dfd["BTLFRAC"].loc[(range(1, 5), MNUMPR), :].sum() / 1000.0
        + dfd["CBTLFRAC"].loc[(2, range(1, 5), MNUMPR), :].sum() / 1000.0
        + dfd["UBAVOL"].loc[MNUMPR] / 1000.0
        + dfd["RFBIOBUTECD"].loc[MNUMCR] / 1000.0
        + (dfd["GRD2DSQTY"].loc[MNUMPR] + dfd["GRN2MGQTY"].loc[MNUMPR]) / 1000.0
        + dfd["RFHCXH2IN"].loc[MNUMPR]
        + dfd["RFMETM85"].loc[MNUMPR]
    )

    z[47] = z[1] + z[2] + z[3] + z[4] + z[5] + z[6]
    z[48] = z[7] + z[8] + z[9] + z[10] + z[11] + z[12] + z[13] + z[14]
    z[49] = z[46] + z[47] + z[48]
    z[55] = z[46] / z[49] * 100.0

    z[21] = dfd["REPORT"].loc[21]
    z[22] = dfd["REPORT"].loc[22]
    z[24] = dfd["REPORT"].loc[24]
    z[23] = dfd["REPORT"].loc[23]
    z[25] = dfd["REPORT"].loc[25]
    z[26] = dfd["REPORT"].loc[26]
    z[27] = dfd["REPORT"].loc[27]
    z[15] = dfd["REPORT"].loc[15]

    # T19(70:87,IY,IS)=REPORT2(IY,1:18)
    z[70] = dfd["REPORT2"].loc[1]
    z[71] = dfd["REPORT2"].loc[2]
    z[72] = dfd["REPORT2"].loc[3]
    z[73] = dfd["REPORT2"].loc[4]
    z[74] = dfd["REPORT2"].loc[5]
    z[75] = dfd["REPORT2"].loc[6]
    z[76] = dfd["REPORT2"].loc[7]
    z[77] = dfd["REPORT2"].loc[8]
    z[79] = dfd["REPORT2"].loc[10]
    z[80] = dfd["REPORT2"].loc[11]
    z[81] = dfd["REPORT2"].loc[12]
    z[82] = dfd["REPORT2"].loc[13]
    z[83] = dfd["REPORT2"].loc[14]
    z[84] = dfd["REPORT2"].loc[15]
    z[85] = dfd["REPORT2"].loc[16]
    z[86] = dfd["REPORT2"].loc[17]
    z[87] = dfd["REPORT2"].loc[18]

    #           Total OPEC Production
    # T19(88,IY,IS)=FSUM(T19(79,IY,IS),4)
    z[88] = z[79] + z[80] + z[81] + z[82]

    # US
    # T19(28,IY,IS)=RFQTDCRD(MNUMOR+2,IY)-(OGQCRREP(1,IY)/365.)
    z[28] = dfd["RFQTDCRD"].loc[MNUMOR + 2] - (dfd["OGQCRREP"].loc[1] / RDAYS)

    #              Total OECD Production
    # T19(89,IY,IS)=FSUM(T19(70,IY,IS),5)+T19(28,IY,IS)
    z[89] = z[70] + z[71] + z[72] + z[73] + z[74] + z[28]

    #              Total Non-OECD Production
    # T19(90,IY,IS)=FSUM(T19(75,IY,IS),3)+FSUM(T19(83,IY,IS),5)
    z[90] = z[75] + z[76] + z[77] + z[83] + z[84] + z[85] + z[86] + z[87]

    #   Total Crude Oil Production 6/
    # T19(69,IY,IS)=FSUM(T19(88,IY,IS),3)
    z[69] = z[88] + z[89] + z[90]
    z[20] = z[69]

    #   OPEC Petroleum Market Share (percent)
    # T19(78,IY,IS)=T19(88,IY,IS)/T19(69,IY,IS)*100.
    z[78] = z[88] / z[69] * 100.0

    return z
