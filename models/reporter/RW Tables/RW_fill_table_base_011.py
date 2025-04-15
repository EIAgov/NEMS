# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
SZO fixed on 7/24/2024
"""


def fill_table_base_011(dfd, table_spec, table_id):
    """Fill table Petroleum and Other Liquids Supply and Disposition
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

    import pandas as pd

    z = {}

    FYRPRC = int(table_spec["settings"]["year_index_real_dol_priceyr"])
    # Get inflation index from 1990->2050
    MC_JPGDP = dfd["MC_JPGDP"].loc[1990:]

    # Get inflation index of FYRPRC (2022)
    SCALPR2 = MC_JPGDP.loc[FYRPRC]  # 2022 = column 33

    # Note: A lot of variables read from the restart are scalar variables shape = (1,1)
    # Solution: Create a blank df of 61 columns, and assign the scalar  to each column
    col_count = len(table_spec["years"])
    columns = [i for i in range(1, col_count + 1)]
    df = pd.DataFrame([[0] * col_count], columns=columns)

    MNUMOR = dfd["MNUMOR_rwpre"]
    MNUMCR = dfd["MNUMCR_rwpre"]
    MNUMPR = dfd["MNUMPR_rwpre"]
    ETHANOL_PARAM = dfd["ETHANOL_PARAM_rwpre"]
    RDAYS = dfd["RDAYS_rwpre"]

    #   Petroleum and Other Liquids Supply and Disposition
    #   (million barrels per day, unless otherwise noted)
    #    Supply and Disposition
    #
    #    Crude Oil
    #      Domestic Crude Production 1/
    # T11(5,IY,IS)=RFQTDCRD(MNUMOR+2,IY)
    z[5] = dfd["RFQTDCRD"].loc[MNUMOR + 2]
    #        Alaska
    # T11(1,IY,IS)=RFQTDCRD(MNUMOR+0,IY)
    z[1] = dfd["RFQTDCRD"].loc[MNUMOR]
    #        Lower 48 States
    # T11(2,IY,IS)=RFQTDCRD(MNUMOR+1,IY)
    z[2] = dfd["RFQTDCRD"].loc[MNUMOR + 1]
    #      Net Imports
    # T11(6,IY,IS)=RFIMCR(MNUMPR,IY)
    z[6] = dfd["RFIMCR"].loc[MNUMPR]
    #        Gross Imports
    # T11(3,IY,IS)=RFQICRD(MNUMPR,IY)
    z[3] = dfd["RFQICRD"].loc[MNUMPR]
    #        Exports
    # T11(4,IY,IS)=RFQEXCRD(MNUMPR,IY)/1000.
    z[4] = dfd["RFQEXCRD"].loc[MNUMPR] / 1000
    #      Other Crude Supply 2/
    # T11(7,IY,IS)=RFCRDOTH(MNUMPR,IY)
    z[7] = dfd["RFCRDOTH"].loc[MNUMPR]
    #        Total Crude Supply
    # T11(8,IY,IS)=RFQTDCRD(MNUMOR+2,IY)+RFIMCR(MNUMPR,IY)+RFCRDOTH(MNUMPR,IY)
    z[8] = z[5] + z[6] + z[7]

    #    Net Product Imports
    # T11(13,IY,IS)=RFIMTP(MNUMPR,IY)
    z[13] = dfd["RFIMTP"].loc[MNUMPR]
    #      Gross Refined Product Imports 3/
    # T11(9,IY,IS)=RFPQIPRDT(MNUMPR,IY,2)
    z[9] = dfd["RFPQIPRDT"].loc[MNUMPR].loc[2]
    #      Unfinished Oil Imports
    # T11(10,IY,IS)=RFPQUFC(MNUMPR,IY,2)
    z[10] = dfd["RFPQUFC"].loc[MNUMPR].loc[2]
    #      Blending Component Imports
    # T11(11,IY,IS)=RFMTBI(MNUMPR,IY)+RFIPQCBOB(MNUMPR,IY,2)/1000.+RFIPQRBOB(MNUMPR,IY,2)/1000.
    z[11] = (
        dfd["RFMTBI"].loc[MNUMPR]
        + dfd["RFIPQCBOB"].loc[MNUMPR].loc[2] / 1000
        + dfd["RFIPQRBOB"].loc[MNUMPR].loc[2] / 1000
    )
    #      Exports
    # T11(12,IY,IS)=RFQEXPRDT(MNUMPR,IY)
    z[12] = dfd["RFQEXPRDT"].loc[MNUMPR]
    #    Refinery Processing Gain 4/
    # T11(14,IY,IS)=RFQPRCG(MNUMPR,IY)
    z[14] = dfd["RFQPRCG"].loc[MNUMPR]
    #    Product Stock Withdrawal
    # T11(15,IY,IS)=PRDSTKWDR(MNUMPR,IY)
    z[15] = dfd["PRDSTKWDR"].loc[MNUMPR]
    #    Other Petroleum Supply
    # T11(16,IY,IS)=FSUM(T11(13,IY,IS),3)
    z[16] = z[13] + z[14] + z[15]
    #    Natural Gas Plant Liquids
    # T11(26,IY,IS)=RFQNGPL(MNUMPR,IY,6)/1000.
    z[26] = dfd["RFQNGPL"].loc[MNUMPR].loc[6] / 1000
    #        Domestic Production
    # T11(17,IY,IS)=0.9751*(CRNETHCD(11,IY)+CLLETHCD(11,IY)+OTHETHCD(11,IY)+GRNETHCD(11,IY))/1000.
    z[17] = (
        ETHANOL_PARAM
        * (
            dfd["CRNETHCD"].loc[MNUMCR]
            + dfd["CLLETHCD"].loc[MNUMCR]
            + dfd["OTHETHCD"].loc[MNUMCR]
            + dfd["GRNETHCD"].loc[MNUMCR]
        )
        / 1000
    )
    #        Net Imports
    # T11(18,IY,IS)=(ETHIMP(11,IY)-ETHEXP(11,IY))/1000.
    z[18] = (dfd["ETHIMP"].loc[MNUMCR] - dfd["ETHEXP"].loc[MNUMCR]) / 1000
    #      Ethanol (undenatured)
    # T11(28,IY,IS)=FSUM(T11(17,IY,IS),2)
    z[28] = z[17] + z[18]
    #        Domestic Production
    # T11(20,IY,IS)=SUM(BIMQTYCD(1:4,11,IY))/1000.
    z[20] = dfd["BIMQTYCD"].loc[((1, 2, 3, 4), MNUMCR), :].sum() / 1000
    #        Net Imports
    # T11(21,IY,IS)=(BIODIMP(11,IY)-BIODEXP(11,IY))/1000.
    z[21] = (dfd["BIODIMP"].loc[MNUMCR] - dfd["BIODEXP"].loc[MNUMCR]) / 1000

    #      Biodiesel
    # T11(29,IY,IS)=FSUM(T11(20,IY,IS),2)
    z[29] = z[20] + z[21]
    #        Domestic Production
    # T11(137,IY,IS)=GRD2DSQTY(MNUMPR,IY)/1000.
    z[137] = dfd["GRD2DSQTY"].loc[MNUMPR] / 1000
    #        Net Imports
    # T11(138,IY,IS)=RENEWDIMP(11,IY)/1000.
    z[138] = dfd["RENEWDIMP"].loc[MNUMCR] / 1000
    #     Renewable Diesel
    # T11(139,IY,IS)=FSUM(T11(137,IY,IS),2)
    z[139] = z[137] + z[138]
    #           Liquids from Biomass
    # T11(132,IY,IS)=SUM(BTLFRAC(1:4,MNUMPR,IY))/1000.+SUM(CBTLFRAC(2,1:4,MNUMPR,IY))/1000.
    z[132] = (
        dfd["BTLFRAC"].loc[((1, 2, 3, 4), MNUMPR), :].sum() / 1000
        + dfd["CBTLFRAC"].loc[(2, (1, 2, 3, 4), MNUMPR), :].sum() / 1000
    )
    #           Pyrolysis Oils
    # T11(133,IY,IS)=UBAVOL(MNUMPR,IY)/1000.
    z[133] = dfd["UBAVOL"].loc[MNUMPR] / 1000
    #           Renewable Jet Fuel (HEFA-SPK)
    # T11(134,IY,IS)=SAF2JTQTY(MNUMPR,IY)/1000.
    z[134] = dfd["SAF2JTQTY"].loc[MNUMPR] / 1000
    #           Renewable Gasoline
    # T11(135,IY,IS)=GRN2MGQTY(MNUMPR,IY)/1000.
    z[135] = dfd["GRN2MGQTY"].loc[MNUMPR] / 1000.0
    #           Biobutanol
    # T11(136,IY,IS)=RFBIOBUTECD(MNUMCR,IY)/1000.
    z[136] = dfd["RFBIOBUTECD"].loc[MNUMCR] / 1000.0
    #        Domestic Production
    # T11(23,IY,IS)=SUM(BTLFRAC(1:4,MNUMPR,IY))/1000.+SUM(CBTLFRAC(2,1:4,MNUMPR,IY))/1000.+UBAVOL(MNUMPR,IY)/1000.+SAF2JTQTY(MNUMPR,IY)/1000.+GRN2MGQTY(MNUMPR,IY)/1000.+RFBIOBUTECD(MNUMCR,IY)/1000.
    z[23] = z[132] + z[133] + z[134] + z[135] + z[136]
    #        Net Imports
    # T11(24,IY,IS)=(BIOBUTEIMP(IY)-BIOBUTEEXP(IY))/1000.
    z[24] = (dfd["BIOBUTEIMP"] - dfd["BIOBUTEEXP"]) / 1000
    #      Stock Withdrawal
    z[25] = z[24] * 0.0
    #      Other Biomass-derived Liquids 5/
    # T11(30,IY,IS)=FSUM(T11(23,IY,IS),3)
    z[30] = z[23] + z[24]  # +z[25]
    #    Biofuels
    # T11(31,IY,IS)=FSUM(T11(28,IY,IS),3)+T11(139,IY,IS)
    z[31] = z[28] + z[29] + z[30] + z[139]
    #    Liquids from Gas
    # T11(27,IY,IS)=SUM(GTLFRAC(1:4,MNUMPR,IY))/1000.
    z[27] = dfd["GTLFRAC"].loc[((1, 2, 3, 4), MNUMPR), :].sum() / 1000
    #    Liquids from Coal
    # T11(33,IY,IS)=SUM(CTLFRAC(1:4,MNUMPR,IY))/1000+SUM(CBTLFRAC(1,1:4,MNUMPR,IY))/1000
    z[33] = (
        dfd["CTLFRAC"].loc[((1, 2, 3, 4), MNUMPR), :].sum() / 1000
        + dfd["CBTLFRAC"].loc[(1, (1, 2, 3, 4), MNUMPR), :].sum() / 1000
    )
    #    Other 6/
    # T11(34,IY,IS)=RFHCXH2IN(MNUMPR,IY)+RFMETM85(MNUMPR,IY)
    z[34] = dfd["RFHCXH2IN"].loc[MNUMPR] + dfd["RFMETM85"].loc[MNUMPR]
    # from gas total
    z[32] = z[26] + z[27]
    #    Other Non-petroleum Supply
    # T11(35,IY,IS)=FSUM(T11(31,IY,IS),4)
    z[35] = z[31] + z[32] + z[33] + z[34]
    #    Total Primary Supply 7/
    # T11(36,IY,IS)=T11(8,IY,IS)+T11(16,IY,IS)+T11(35,IY,IS)
    z[36] = z[8] + z[16] + z[35]
    #    Product Supplied
    #      by Fuel
    #        Hydrocarbon Gas Liquids 8/
    # T11(37,IY,IS)=RFQLG(11,IY)
    z[37] = dfd["RFQLG"].loc[MNUMCR]
    #        Motor Gasoline 9/
    # T11(39,IY,IS)=RFQMG(11,IY)
    z[39] = dfd["RFQMG"].loc[MNUMCR]
    #           of which:  E85 10/
    # T11(38,IY,IS)=QETTR(11,IY)/CFE85Q(IY)/365.*1000.
    z[38] = dfd["QETTR"].loc[MNUMCR] / dfd["CFE85Q"] / RDAYS
    #        Jet Fuel 11/
    # T11(40,IY,IS)=RFQJF(11,IY)
    z[40] = dfd["RFQJF"].loc[MNUMCR]
    #        Distillate Fuel Oil 12/
    # T11(41,IY,IS)=RFQDS(11,IY)
    z[41] = dfd["RFQDS"].loc[MNUMCR]
    #          of which:  Diesel
    # T11(49,IY,IS)=TDIESEL(11,IY)/1000.
    z[49] = dfd["TDIESEL"].loc[MNUMCR] / 1000
    #        Residual Fuel Oil
    # T11(42,IY,IS)=RFQRL(11,IY)+RFQRH(11,IY)
    z[42] = dfd["RFQRL"].loc[MNUMCR] + dfd["RFQRH"].loc[MNUMCR]
    #        Other 13/
    # T11(43,IY,IS)=RFQOTH(11,IY)+RFQKS(11,IY)+RFQPCK(11,IY)+RFQPF(11,IY)+RFQARO(11,IY)+RFQSTG(11,IY)+RFMETM85(MNUMPR,IY)
    z[43] = (
        dfd["RFQOTH"].loc[MNUMCR]
        + dfd["RFQKS"].loc[MNUMCR]
        + dfd["RFQPCK"].loc[MNUMCR]
        + dfd["RFQPF"].loc[MNUMCR]
        + dfd["RFQARO"].loc[MNUMCR]
        + dfd["RFQSTG"].loc[MNUMCR]
        + dfd["RFMETM85"].loc[MNUMPR]
    )

    #      by Sector
    #        Residential and Commercial
    # T11(44,IY,IS)=(QMGCM(11,IY)/CFMGQ(IY)+QDSCM(11,IY)/CFDSCM(IY)+QKSCM(11,IY)/CFKSQ+
    #               QPRCM(11,IY)/CFPRQ+QRSCM(11,IY)/CFRSQ+QDSRS(11,IY)/CFDSRS(IY)+
    #               QKSRS(11,IY)/CFKSQ+QPRRS(11,IY)/CFPRQ)/365.*1000.

    # df[columns] = dfd['CFDSCM'].iloc[0]
    # CFDSCM = df.copy()
    # df[columns] = dfd['CFMGQ'].iloc[0]
    # CFMGQ = df.copy()
    # df[columns] = dfd['CFKSQ'].iloc[0]
    # CFKSQ = df.copy()
    # df[columns] = dfd['CFPRQ'].iloc[0]
    # CFPRQ = df.copy()
    # df[columns] = dfd['CFRSQ'].iloc[0]
    # CFRSQ = df.copy()
    # df[columns] = dfd['CFDSRS'].iloc[0]
    # CFDSRS = df.copy()

    z[44] = (
        dfd["QMGCM"].loc[MNUMCR] / dfd["CFMGQ"]
        + dfd["QDSCM"].loc[MNUMCR] / dfd["CFDSCM"]
        + dfd["QKSCM"].loc[MNUMCR] / dfd["CFKSQ"].iloc[0]
        + dfd["QPRCM"].loc[MNUMCR] / dfd["CFPRQ"].iloc[0]
        + dfd["QRSCM"].loc[MNUMCR] / dfd["CFRSQ"].iloc[0]
        + dfd["QDSRS"].loc[MNUMCR] / dfd["CFDSRS"]
        + dfd["QKSRS"].loc[MNUMCR] / dfd["CFKSQ"].iloc[0]
        + dfd["QPRRS"].loc[MNUMCR] / dfd["CFPRQ"].iloc[0]
    ) / RDAYS

    #        Industrial 14/
    # T11(45,IY,IS)=(QMGIN(11,IY)/CFMGQ(IY)+QDSIN(11,IY)/CFDSIN(IY)+QKSIN(11,IY)/CFKSQ+QETIN(11,IY)/CFEEQ+(QPROLENERF(11,IY)+QPRIN(11,IY))/CFPRQ+
    #               QBUIN(11,IY)/CFBUQ+QISIN(11,IY)/CFIBQ+QPPIN(11,IY)/CFPPQ+
    #               QRSIN(11,IY)/CFRSQ+QPFIN(11,IY)/CFPFQ(IY)+QSGIN(11,IY)/CFSGQ+
    #              (QPCIN(11,IY)-QCCRF(11,IY))/CFPCQ+QCCRF(11,IY)/CFCCQ(IY)+QASIN(11,IY)/CFASQ+
    #               QLUIN(11,IY)/CFLUQ+(QOTIN(11,IY)-QLUIN(11,IY))/CFOTQ(IY))/365.*1000.
    # before 2013 use this formula:
    # T11(45,IY,IS) =(QMGIN(11,IY)/CFMGQ(IY) + QDSIN(11,IY)/CFDSIN(IY) + &
    # QKSIN(11,IY)/CFJFQ(IY) + QLGIN(11,IY)/CFLGQ(IY) + &
    # QRSIN(11,IY)/CFRSQ + QPFIN(11,IY)/CFPFQ(IY) + &
    # QSGIN(11,IY)/CFSGQ + QPCIN(11,IY)/CFPCQ + &
    # QASIN(11,IY)/CFASQ + QLUIN(11,IY)/CFLUQ + &
    # (QOTIN(11,IY)-QLUIN(11,IY))/CFOTQ(IY))/365.*1000.

    # use this before 2013
    # z[45] = (dfd['QMGIN'].loc[MNUMCR]/dfd['CFMGQ']+dfd['QDSIN'].loc[MNUMCR]/dfd['CFDSIN']+dfd['QKSIN'].loc[MNUMCR]/dfd['CFJFQ']+dfd['QLGIN'].loc[MNUMCR]/dfd['CFLGQ']+
    # dfd['QRSIN'].loc[MNUMCR]/dfd['CFRSQ'].iloc[0]+dfd['QPFIN'].loc[MNUMCR]/dfd['CFPFQ']+dfd['QSGIN'].loc[MNUMCR]/dfd['CFSGQ'].iloc[0]+ \
    # (dfd['QPCIN'].loc[MNUMCR]-dfd['QCCRF'].loc[MNUMCR])/dfd['CFPCQ'].iloc[0]+dfd['QCCRF'].loc[MNUMCR]/dfd['CFCCQ']+dfd['QASIN'].loc[MNUMCR]/dfd['CFASQ'].iloc[0]+ \
    # dfd['QLUIN'].loc[MNUMCR]/dfd['CFLUQ'].iloc[0]+(dfd['QOTIN'].loc[MNUMCR]-dfd['QLUIN'].loc[MNUMCR])/dfd['CFOTQ'])/RDAYS

    # 2013-2050 use this
    z[45] = (
        dfd["QMGIN"].loc[MNUMCR] / dfd["CFMGQ"]
        + dfd["QDSIN"].loc[MNUMCR] / dfd["CFDSIN"]
        + dfd["QKSIN"].loc[MNUMCR] / dfd["CFKSQ"].iloc[0]
        + dfd["QETIN"].loc[MNUMCR] / dfd["CFEEQ"].iloc[0]
        + (dfd["QPROLENERF"].loc[MNUMCR] + dfd["QPRIN"].loc[MNUMCR])
        / dfd["CFPRQ"].iloc[0]
        + dfd["QBUIN"].loc[MNUMCR] / dfd["CFBUQ"].iloc[0]
        + dfd["QISIN"].loc[MNUMCR] / dfd["CFIBQ"].iloc[0]
        + dfd["QPPIN"].loc[MNUMCR] / dfd["CFPPQ"].iloc[0]
        + dfd["QRSIN"].loc[MNUMCR] / dfd["CFRSQ"].iloc[0]
        + dfd["QPFIN"].loc[MNUMCR] / dfd["CFPFQ"]
        + dfd["QSGIN"].loc[MNUMCR] / dfd["CFSGQ"].iloc[0]
        + (dfd["QPCIN"].loc[MNUMCR] - dfd["QCCRF"].loc[MNUMCR]) / dfd["CFPCQ"].iloc[0]
        + dfd["QCCRF"].loc[MNUMCR] / dfd["CFCCQ"]
        + dfd["QASIN"].loc[MNUMCR] / dfd["CFASQ"].iloc[0]
        + dfd["QLUIN"].loc[MNUMCR] / dfd["CFLUQ"].iloc[0]
        + (dfd["QOTIN"].loc[MNUMCR] - dfd["QLUIN"].loc[MNUMCR]) / dfd["CFOTQ"]
    ) / RDAYS

    # use this before 2013
    years = table_spec["years"]  # range(1990, 2050+1)
    year_idx = years.index(2013)
    z[45].iloc[0:year_idx] = (
        dfd["QMGIN"].iloc[:, 0:year_idx].loc[MNUMCR] / dfd["CFMGQ"].iloc[0:year_idx]
        + dfd["QDSIN"].iloc[:, 0:year_idx].loc[MNUMCR] / dfd["CFDSIN"].iloc[0:year_idx]
        + dfd["QKSIN"].iloc[:, 0:year_idx].loc[MNUMCR] / dfd["CFJFQ"].iloc[0:year_idx]
        + dfd["QLGIN"].iloc[:, 0:year_idx].loc[MNUMCR] / dfd["CFLGQ"].iloc[0:year_idx]
        + dfd["QRSIN"].iloc[:, 0:year_idx].loc[MNUMCR] / dfd["CFRSQ"].iloc[0]
        + dfd["QPFIN"].iloc[:, 0:year_idx].loc[MNUMCR] / dfd["CFPFQ"].iloc[0:year_idx]
        + dfd["QSGIN"].iloc[:, 0:year_idx].loc[MNUMCR] / dfd["CFSGQ"].iloc[0]
        + (
            dfd["QPCIN"].iloc[:, 0:year_idx].loc[MNUMCR]
            - dfd["QCCRF"].iloc[:, 0:year_idx].loc[MNUMCR]
        )
        / dfd["CFPCQ"].iloc[0]
        + dfd["QCCRF"].iloc[:, 0:year_idx].loc[MNUMCR] / dfd["CFCCQ"].iloc[0:year_idx]
        + dfd["QASIN"].iloc[:, 0:year_idx].loc[MNUMCR] / dfd["CFASQ"].iloc[0]
        + dfd["QLUIN"].iloc[:, 0:year_idx].loc[MNUMCR] / dfd["CFLUQ"].iloc[0]
        + (
            dfd["QOTIN"].iloc[:, 0:year_idx].loc[MNUMCR]
            - dfd["QLUIN"].iloc[:, 0:year_idx].loc[MNUMCR]
        )
        / dfd["CFOTQ"].iloc[0:year_idx]
    ) / RDAYS

    #        Transportation

    # T11(46,IY,IS)=((QMGTR(11,IY)-QMGBS(11,IY))/CFMGQ(IY)+(QDSTR(11,IY)-QDSBS(11,IY))/CFDSTR(IY)+(QJFTR(11,IY)-QJFBS(11,IY))/CFJFQ(IY)+QPRTR(11,IY)/CFPRQ+QRSTR(11,IY)/CFRSQ+QAGTR(11,IY)/CFAVQ+
    #                QLUTR(11,IY)/CFLUQ + QETTR(11,IY)/CFE85Q(IY) + QMETR(11,IY)/CFM85Q(IY))/365.*1000.
    z[46] = (
        (dfd["QMGTR"].loc[MNUMCR] - dfd["QMGBS"].loc[MNUMCR]) / dfd["CFMGQ"]
        + (dfd["QDSTR"].loc[MNUMCR] - dfd["QDSBS"].loc[MNUMCR]) / dfd["CFDSTR"]
        + (dfd["QJFTR"].loc[MNUMCR] - dfd["QJFBS"].loc[MNUMCR]) / dfd["CFJFQ"]
        + dfd["QPRTR"].loc[MNUMCR] / dfd["CFPRQ"].iloc[0]
        + dfd["QRSTR"].loc[MNUMCR] / dfd["CFRSQ"].iloc[0]
        + dfd["QAGTR"].loc[MNUMCR] / dfd["CFAVQ"].iloc[0]
        + dfd["QLUTR"].loc[MNUMCR] / dfd["CFLUQ"].iloc[0]
        + dfd["QETTR"].loc[MNUMCR] / dfd["CFE85Q"]
        + dfd["QMETR"].loc[MNUMCR] / dfd["CFM85Q"]
    ) / RDAYS
    #        Electric Power 15/
    # T11(47,IY,IS)=(QDSEL(11,IY)/CFDSEL(IY)+QRSEL(11,IY)/CFRSQ+QPCEL(11,IY)/CFPCQ)/365.*1000.
    z[47] = (
        dfd["QDSEL"].loc[MNUMCR] / dfd["CFDSEL"]
        + dfd["QRSEL"].loc[MNUMCR] / dfd["CFRSQ"].iloc[0]
        + dfd["QPCEL"].loc[MNUMCR] / dfd["CFPCQ"].iloc[0]
    ) / RDAYS
    #        Unspecified Sector 16/
    # T11(128,IY,IS)=(QMGBS(11,IY)/CFMGQ(IY)+QJFBS(11,IY)/CFJFQ(IY)+QDSBS(11,IY)/CFDSTR(IY))/365.*1000
    z[128] = (
        dfd["QMGBS"].loc[MNUMCR] / dfd["CFMGQ"]
        + dfd["QJFBS"].loc[MNUMCR] / dfd["CFJFQ"]
        + dfd["QDSBS"].loc[MNUMCR] / dfd["CFDSTR"]
    ) / RDAYS
    #      Total
    # T11(48,IY,IS)=FSUM(T11(37,IY,IS),7)-T11(38,IY,IS)
    z[48] = z[37] + z[39] + z[40] + z[41] + z[42] + z[43]
    #
    #    Discrepancy 17/
    # T11(50,IY,IS)=T11(36,IY,IS)-T11(48,IY,IS)
    z[50] = z[36] - z[48]
    #
    #   Domestic Refinery Distillation Capacity 18/
    # T11(51,IY,IS)=RFDSTCAP(MNUMPR,IY)
    z[51] = dfd["RFDSTCAP"].loc[MNUMPR]
    #   Capacity Utilization Rate (percent) 19/
    # T11(52,IY,IS)=RFDSTUTL(MNUMPR,IY)
    z[52] = dfd["RFDSTUTL"].loc[MNUMPR]

    #   Total Gross Imports
    # Note: indices reversed
    # T11(129,IY,IS)=RFQICRD(MNUMPR,IY)+RFPQIPRDT(MNUMPR,IY,2)+RFPQUFC(MNUMPR,IY,2)+RFMTBI(MNUMPR,IY)+(RFIPQCBOB(MNUMPR,IY,2)+RFIPQRBOB(MNUMPR,IY,2))/1000.+(ETHIMP(11,IY)+BIODIMP(11,IY)+RENEWDIMP(11,IY)+BIOBUTEIMP(IY))/1000.
    z[129] = (
        dfd["RFQICRD"].loc[MNUMPR]
        + dfd["RFPQIPRDT"].loc[MNUMPR].loc[2]
        + dfd["RFPQUFC"].loc[MNUMPR].loc[2]
        + dfd["RFMTBI"].loc[MNUMPR]
        + (dfd["RFIPQCBOB"].loc[MNUMPR].loc[2] + dfd["RFIPQRBOB"].loc[MNUMPR].loc[2])
        / 1000.0
        + (
            dfd["ETHIMP"].loc[MNUMCR]
            + dfd["BIODIMP"].loc[MNUMCR]
            + dfd["RENEWDIMP"].loc[MNUMCR]
            + dfd["BIOBUTEIMP"]
        )
        / 1000.0
    )
    #   Total Gross Exports
    # T11(130,IY,IS)=RFQEXCRD(MNUMPR,IY)/1000.+RFQEXPRDT(MNUMPR,IY)+(ETHEXP(11,IY)+BIODEXP(11,IY)+BIOBUTEEXP(IY))/1000.
    z[130] = (
        dfd["RFQEXCRD"].loc[MNUMPR] / 1000.0
        + dfd["RFQEXPRDT"].loc[MNUMPR]
        + (dfd["ETHEXP"].loc[MNUMCR] + dfd["BIODEXP"].loc[MNUMCR] + dfd["BIOBUTEEXP"])
        / 1000.0
    )
    #   Total Net Imports
    # T11(131,IY,IS)=T11(129,IY,IS)-T11(130,IY,IS)
    z[131] = z[129] - z[130]
    #   Net Import Share of Product Supplied (percent)
    # T11(53,IY,IS)=(RFIMCR(MNUMPR,IY)+RFIMTP(MNUMPR,IY)+(ETHIMP(11,IY)-ETHEXP(11,IY)+BIODIMP(11,IY)-BIODEXP(11,IY)+RENEWDIMP(11,IY))/1000.)/
    #               T11(36,IY,IS)*100.
    z[53] = (
        (
            dfd["RFIMCR"].loc[MNUMPR]
            + dfd["RFIMTP"].loc[MNUMPR]
            + (
                dfd["ETHIMP"].loc[MNUMCR]
                - dfd["ETHEXP"].loc[MNUMCR]
                + dfd["BIODIMP"].loc[MNUMCR]
                - dfd["BIODEXP"].loc[MNUMCR]
                + dfd["RENEWDIMP"].loc[MNUMCR]
            )
            / 1000
        )
        / z[36]
        * 100
    )
    #   Gross Inputs to Crude Oil Distillation Units
    # T11(54,IY,IS)=T11(52,IY,IS)*T11(51,IY,IS)/100.
    z[54] = z[52] * z[51] / 100
    #   Expenditures for Imported Crude Oil and
    #    Petroleum Products (billion #### dollars)
    # T11(55,IY,IS)=RFIMPEXPEND(IY)
    z[55] = dfd["RFIMPEXPEND"] * SCALPR2
    #
    #
    #   --------------------------------------------
    #     Bonus Rows - Internal Only
    #   --------------------------------------------
    #
    #   Hydrocarbon Gas Liquids Balance
    #
    #         Ethane
    # T11(70,IY,IS)=RFQNGPL(MNUMPR,IY,1)/1000.
    z[70] = dfd["RFQNGPL"].loc[MNUMPR].loc[1] / 1000
    #         Propane
    # T11(71,IY,IS)=RFQNGPL(MNUMPR,IY,2)/1000.
    z[71] = dfd["RFQNGPL"].loc[MNUMPR].loc[2] / 1000
    #         Normal Butane
    # T11(72,IY,IS)=RFQNGPL(MNUMPR,IY,3)/1000.
    z[72] = dfd["RFQNGPL"].loc[MNUMPR].loc[3] / 1000
    #         Isobutane
    # T11(73,IY,IS)=RFQNGPL(MNUMPR,IY,4)/1000.
    z[73] = dfd["RFQNGPL"].loc[MNUMPR].loc[4] / 1000
    #         Natural Gasoline
    # T11(74,IY,IS)=RFQNGPL(MNUMPR,IY,5)/1000.
    z[74] = dfd["RFQNGPL"].loc[MNUMPR].loc[5] / 1000
    #            Other Refinery Olefins
    # T11(81,IY,IS)=0.0
    z[81] = z[74] * 0.0
    #
    #            Ethane
    # T11(82,IY,IS)=RFIPQET(MNUMPR,IY,2)/1000.
    z[82] = dfd["RFIPQET"].loc[MNUMPR].loc[2] / 1000
    #            Propane
    # T11(83,IY,IS)=RFIPQPR(MNUMPR,IY,2)/1000.
    z[83] = dfd["RFIPQPR"].loc[MNUMPR].loc[2] / 1000.0
    #            Normal Butane
    # T11(84,IY,IS)=RFIPQBU(MNUMPR,IY,2)/1000.
    z[84] = dfd["RFIPQBU"].loc[MNUMPR].loc[2] / 1000.0
    #            Isobutane
    # T11(85,IY,IS)=RFIPQIS(MNUMPR,IY,2)/1000.
    z[85] = dfd["RFIPQIS"].loc[MNUMPR].loc[2] / 1000.0
    #            Natural Gasoline
    # T11(86,IY,IS)=RFIPQPP(MNUMPR,IY,2)/1000.
    z[86] = dfd["RFIPQPP"].loc[MNUMPR].loc[2] / 1000.0
    #            Propylene
    # T11(87,IY,IS)=RFIPQPY(MNUMPR,IY,2)/1000.
    z[87] = dfd["RFIPQPY"].loc[MNUMPR].loc[2] / 1000.0
    #            Other Refinery Olefins
    # T11(88,IY,IS)=0.0
    z[88] = z[87] * 0.0
    #            Ethane
    # T11(89,IY,IS)=QPRDEX(27,IY)
    z[89] = dfd["QPRDEX"].loc[27]
    #            Propane
    # T11(90,IY,IS)=QPRDEX(1,IY)
    z[90] = dfd["QPRDEX"].loc[1]
    #            Normal Butane
    # T11(91,IY,IS)=QPRDEX(15,IY)
    z[91] = dfd["QPRDEX"].loc[15]
    #            Isobutane
    # T11(92,IY,IS)=QPRDEX(28,IY)
    z[92] = dfd["QPRDEX"].loc[28]
    #            Natural Gasoline
    # T11(93,IY,IS)=QPRDEX(29,IY)
    z[93] = dfd["QPRDEX"].loc[29]
    #            Propylene
    # T11(94,IY,IS)=QPRDEX(14,IY)
    z[94] = dfd["QPRDEX"].loc[14]
    #            Other Refinery Olefins
    # T11(95,IY,IS)=0.0
    z[95] = z[94] * 0
    #            Ethane
    # T11(96,IY,IS)=QETINPF(MNUMCR,IY)/CFEEQ/365.*1000.
    z[96] = dfd["QETINPF"].loc[MNUMCR] / dfd["CFEEQ"].iloc[0] / RDAYS
    #            Propane
    # T11(97,IY,IS)=QPRINPF(MNUMCR,IY)/CFPRQ/365.*1000.
    z[97] = dfd["QPRINPF"].loc[MNUMCR] / dfd["CFPRQ"].iloc[0] / RDAYS
    #            Normal Butane
    # T11(98,IY,IS)=QBUINPF(MNUMCR,IY)/CFBUQ/365.*1000.
    z[98] = dfd["QBUINPF"].loc[MNUMCR] / dfd["CFBUQ"].iloc[0] / RDAYS
    #            Isobutane
    # T11(99,IY,IS)=QISINPF(MNUMCR,IY)/CFIBQ/365.*1000.
    z[99] = dfd["QISINPF"].loc[MNUMCR] / dfd["CFIBQ"].iloc[0] / RDAYS
    #            Natural Gasoline
    # T11(100,IY,IS)=QPPINPF(MNUMCR,IY)/CFPPQ/365.*1000.
    z[100] = dfd["QPPINPF"].loc[MNUMCR] / dfd["CFPPQ"].iloc[0] / RDAYS
    #            Propylene
    # T11(101,IY,IS)=QPROLENERF(MNUMCR,IY)/CFPRQ/365.*1000.
    z[101] = dfd["QPROLENERF"].loc[MNUMCR] / dfd["CFPRQ"].iloc[0] / RDAYS
    #            Other Refinery Olefins
    # T11(102,IY,IS)=0.0
    z[102] = z[100] * 0.0
    #         Propane
    # T11(103,IY,IS)=QPRTR(MNUMCR,IY)/CFPRQ/365.*1000.
    z[103] = dfd["QPRTR"].loc[MNUMCR] / dfd["CFPRQ"].iloc[0] / RDAYS
    #         Natural Gasoline
    # T11(104,IY,IS)=SUM(MOTOR_FUEL(1:3,3,IY))/1000.
    z[104] = dfd["MOTOR_FUEL"].loc[((1, 2, 3), 3), :].sum() / 1000
    #         Residential
    # T11(105,IY,IS)=QPRRS(MNUMCR,IY)/CFPRQ/365.*1000.
    z[105] = dfd["QPRRS"].loc[MNUMCR] / dfd["CFPRQ"].iloc[0] / RDAYS
    #         Commercial
    # T11(106,IY,IS)=QPRCM(MNUMCR,IY)/CFPRQ/365.*1000.
    z[106] = dfd["QPRCM"].loc[MNUMCR] / dfd["CFPRQ"].iloc[0] / RDAYS
    #         Industrial
    # T11(107,IY,IS)=(QPRIN(MNUMCR,IY)-QPRINPF(MNUMCR,IY))/CFPRQ/365.*1000.
    z[107] = (
        (dfd["QPRIN"].loc[MNUMCR] - dfd["QPRINPF"].loc[MNUMCR])
        / dfd["CFPRQ"].iloc[0]
        / RDAYS
    )
    #            Ethane
    # T11(75,IY,IS)=T11(96,IY,IS)+T11(89,IY,IS)-T11(70,IY,IS)-T11(82,IY,IS)
    z[75] = z[96] + z[89] - z[70] - z[82]
    #            Propane
    # T11(76,IY,IS)=T11(97,IY,IS)+T11(90,IY,IS)-T11(71,IY,IS)-T11(83,IY,IS)+T11(103,IY,IS)+SUM(T11(105:107,IY,IS))
    z[76] = z[97] + z[90] - z[71] - z[83] + z[103] + z[105] + z[106] + z[107]
    #            Normal Butane
    # T11(77,IY,IS)=T11(98,IY,IS)+T11(91,IY,IS)-T11(72,IY,IS)-T11(84,IY,IS)
    z[77] = z[98] + z[91] - z[72] - z[84]
    #            Isobutane
    # T11(78,IY,IS)=T11(99,IY,IS)+T11(92,IY,IS)-T11(73,IY,IS)-T11(85,IY,IS)
    z[78] = z[99] + z[92] - z[73] - z[85]
    #            Natural Gasoline
    # T11(79,IY,IS)=T11(100,IY,IS)+T11(104,IY,IS)+T11(93,IY,IS)-T11(74,IY,IS)-T11(86,IY,IS)
    z[79] = z[100] + z[104] + z[93] - z[74] - z[86]
    #            Propylene
    # T11(80,IY,IS)=T11(101,IY,IS)+T11(94,IY,IS)-T11(87,IY,IS)
    z[80] = z[101] + z[94] - z[87]
    #      For Transportation
    # T11(108,IY,IS)=FSUM(T11(103,IY,IS),2)
    z[108] = z[103] + z[104]
    #         Paraffins
    # T11(109,IY,IS)=FSUM(T11(75,IY,IS),5)
    z[109] = z[75] + z[76] + z[77] + z[78] + z[79]
    #         Refinery Olefins
    # T11(110,IY,IS)=FSUM(T11(80,IY,IS),2)
    z[110] = z[80] + z[81]
    #
    z[111] = z[82] + z[83] + z[84] + z[85] + z[86]
    #         Refinery Olefins
    # T11(112,IY,IS)=FSUM(T11(87,IY,IS),2)
    z[112] = z[87]
    #         Natural Gas Liquids
    # T11(113,IY,IS)=FSUM(T11(89,IY,IS),5)
    z[113] = z[89] + z[90] + z[91] + z[92] + z[93]
    #         Refinery Olefins
    # T11(114,IY,IS)=FSUM(T11(94,IY,IS),2)
    z[114] = z[94]
    #         Natural Gas Liquids
    # T11(115,IY,IS)=FSUM(T11(96,IY,IS),5)
    z[115] = z[96] + z[97] + z[98] + z[99] + z[100]
    #         Refinery Olefins
    # T11(116,IY,IS)=FSUM(T11(101,IY,IS),2)
    z[116] = z[101]
    #      Natural Gas Plant Liquids
    # T11(117,IY,IS)=FSUM(T11(70,IY,IS),5)
    z[117] = z[70] + z[71] + z[72] + z[73] + z[74]
    #      Net Refinery Production (Balancing Item)
    # T11(118,IY,IS)=FSUM(T11(109,IY,IS),2)
    z[118] = z[109] + z[110]
    #      Imports
    # T11(119,IY,IS)=FSUM(T11(111,IY,IS),2)
    z[119] = z[111] + z[112]
    #      Exports
    # T11(120,IY,IS)=FSUM(T11(113,IY,IS),2)
    z[120] = z[113] + z[114]
    #      Light Chemical Feedstock
    # T11(121,IY,IS)=FSUM(T11(115,IY,IS),2)
    z[121] = z[115] + z[116]
    #      For Heat and Power
    # T11(122,IY,IS)=FSUM(T11(105,IY,IS),3)
    z[122] = z[105] + z[106] + z[107]
    #   Domestic Sources
    # T11(123,IY,IS)=FSUM(T11(117,IY,IS),2)
    z[123] = z[117] + z[118]
    #   Trade
    # T11(124,IY,IS)=T11(119,IY,IS)-T11(120,IY,IS)
    z[124] = z[119] - z[120]
    #   Total Supply
    # T11(125,IY,IS)=FSUM(T11(123,IY,IS),2)
    z[125] = z[123] + z[124]
    #   Consumption
    # T11(126,IY,IS)=FSUM(T11(121,IY,IS),2)+T11(108,IY,IS)
    z[126] = z[121] + z[122] + z[108]
    #
    #   Discrepancy
    # T11(127,IY,IS)=T11(125,IY,IS)-T11(126,IY,IS)
    z[127] = z[125] - z[126]
    #
    #
    #   Net Expenditures for Imports of Crude Oil and
    #   Petroleum Products (billion nominal dollars)
    # T11(56,IY,IS)=T11(55,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[56] = z[55] / SCALPR2 * MC_JPGDP
    #
    #   Product Net Import Share of Total Net Imports
    # T11(57,IY,IS)=(RFIMTP(MNUMPR,IY)+(ETHIMP(11,IY)-ETHEXP(11,IY)+BIODIMP(11,IY)-BIODEXP(11,IY)+RENEWDIMP(11,IY))/1000.)/
    #              (RFIMCR(MNUMPR,IY)+RFIMTP(MNUMPR,IY)+(ETHIMP(11,IY)-ETHEXP(11,IY)+BIODIMP(11,IY)-BIODEXP(11,IY)+RENEWDIMP(11,IY))/1000.)*100.
    z[57] = (
        (
            dfd["RFIMTP"].loc[MNUMPR]
            + (
                dfd["ETHIMP"].loc[MNUMCR]
                - dfd["ETHEXP"].loc[MNUMCR]
                + dfd["BIODIMP"].loc[MNUMCR]
                - dfd["BIODEXP"].loc[MNUMCR]
                + dfd["RENEWDIMP"].loc[MNUMCR]
            )
            / 1000
        )
        / (
            dfd["RFIMCR"].loc[MNUMPR]
            + dfd["RFIMTP"].loc[MNUMPR]
            + (
                dfd["ETHIMP"].loc[MNUMCR]
                - dfd["ETHEXP"].loc[MNUMCR]
                + dfd["BIODIMP"].loc[MNUMCR]
                - dfd["BIODEXP"].loc[MNUMCR]
                + dfd["RENEWDIMP"].loc[MNUMCR]
            )
            / 1000
        )
        * 100
    )
    #   Refinery Gain as percent of Crude Input
    # T11(58,IY,IS)=T11(14,IY,IS)/T11(54,IY,IS)*100.
    z[58] = z[14] / z[54] * 100
    #        Imports
    # T11(59,IY,IS)=T11(3,IY,IS)+FSUM(T11(9,IY,IS),3)
    z[59] = z[3] + z[9] + z[10] + z[11]
    #        Exports
    # T11(60,IY,IS)=T11(4,IY,IS)+T11(12,IY,IS)
    z[60] = z[4] + z[12]
    #
    #   Petroleum Only Stuff
    #      Net Imports
    # T11(61,IY,IS)=T11(59,IY,IS)-T11(60,IY,IS)
    z[61] = z[59] - z[60]
    #      Consumption
    # T11(62,IY,IS)=T11(48,IY,IS)-T11(35,IY,IS)
    z[62] = z[48] - z[35]
    #   This is convergence-checked GLBCRDDMD:
    #      Global Crude-like Demand
    # T11(67,IY,IS)=GLBCRDDMD(IY)/1000.
    z[67] = dfd["GLBCRDDMD"] / 1000
    #
    #   Biobutanol
    # T11(66,IY,IS)=QBIOBUTE(MNUMCR,IY)/1000.
    z[66] = dfd["QBIOBUTE"].loc[MNUMCR] / 1000
    #      Domestic Production
    # T11(63,IY,IS)=RFBIOBUTECD(MNUMCR,IY)/1000.
    z[63] = dfd["RFBIOBUTECD"].loc[MNUMCR] / 1000
    #      Net Imports
    # T11(64,IY,IS)=(BIOBUTEIMP(IY)-BIOBUTEEXP(IY))/1000.
    z[64] = (dfd["BIOBUTEIMP"] - dfd["BIOBUTEEXP"]) / 1000
    #      Stock Withdrawal
    # T11(65,IY,IS)=BIOBUTESTK(IY)/1000.
    z[65] = dfd["BIOBUTESTK"] / 1000
    return z
