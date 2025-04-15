# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
SZO fixed cross-referencing on 7/10/2024
Fixed "Consumption of Petroleum/Liquid Products" on 7/30/2024
"""


def fill_table_base_107(dfd, table_spec, table_id):
    """Fill table  Supply and Disposition Balances by Fuel
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
        TODO: Hard-coded lines in ftab.f will be implemented next stage.
        At some point, we will delete the commented rows with Fortran formulas.

    """

    z = {}

    RDAYS = dfd["RDAYS_rwpre"]
    MNUMOR = dfd["MNUMOR_rwpre"]
    MNUMPR = dfd["MNUMPR_rwpre"]
    ETHANOL_PARAM = dfd["ETHANOL_PARAM_rwpre"]
    GAL_PER_BBL = dfd["GAL_PER_BBL_rwpre"]
    MNUMCR = dfd["MNUMCR_rwpre"]
    MNUMLR = dfd["MNUMLR_rwpre"]

    # FYRPRC = int(table_spec['year_index_real_dol_priceyr'])
    FYRPRC = int(table_spec["ftab_dat"]["FYRPRC"])

    # Get inflation index from 1990->2050
    MC_JPGDP = dfd["MC_JPGDP"].loc[1990:]

    # Get inflation index of FYRPRC (2022)
    SCALPR2 = MC_JPGDP.loc[FYRPRC]  # 2022 = column 33

    #   Supply and Disposition Balances by Fuel
    #    Supply and Disposition
    #
    #   quadrillion Btu

    #   Liquids as in Table 11

    #     Crude Oil Production
    # T107(1,IY,IS)=RFQTDCRD(MNUMOR+2,IY)*RDAYS*CFCRDDOM(IY)*.001
    z[1] = dfd["RFQTDCRD"].loc[MNUMOR + 2] * RDAYS * dfd["CFCRDDOM"] * 0.001

    #     Crude Oil Imports for Use
    # T107(3,IY,IS)=RFQICRD(MNUMPR,IY)*CFCRDIMP(IY)*RDAYS/1000.
    z[3] = dfd["RFQICRD"].loc[MNUMPR] * dfd["CFCRDIMP"] * RDAYS / 1000.0

    #     Crude Oil Exports
    # T107(5,IY,IS)=RFQEXCRD(MNUMPR,IY)*CFCRDEXP(IY)*RDAYS/1000000.
    z[5] = dfd["RFQEXCRD"].loc[MNUMPR] * dfd["CFCRDEXP"] * RDAYS / 1000000.0

    #     Other Crude Oil Supply
    # T107(4,IY,IS)=RFCRDOTH(MNUMPR,IY)*CFCRDDOM(IY)*RDAYS/1000.
    z[4] = dfd["RFCRDOTH"].loc[MNUMPR] * dfd["CFCRDDOM"] * RDAYS / 1000.0

    #     Refined Product Imports
    # T107(126,IY,IS)=RFPQIPRDT(MNUMPR,IY,2)*CFIMPRD(IY)*RDAYS/1000.
    z[126] = dfd["RFPQIPRDT"].loc[MNUMPR].loc[2] * dfd["CFIMPRD"] * RDAYS / 1000.0

    #     Unfinished Oil Imports
    # T107(127,IY,IS)=RFPQUFC(MNUMPR,IY,2)*CFIMUO(IY)*RDAYS/1000.
    z[127] = dfd["RFPQUFC"].loc[MNUMPR].loc[2] * dfd["CFIMUO"] * RDAYS / 1000.0

    #     Conventional Blending Component Imports
    # T107(128,IY,IS)=RFIPQCBOB(MNUMPR,IY,2)*CFCBOB(IY)*RDAYS/1000000.
    z[128] = dfd["RFIPQCBOB"].loc[MNUMPR].loc[2] * dfd["CFCBOB"] * RDAYS / 1000000.0

    #     Reformulated Blending Component Imports
    # T107(129,IY,IS)=RFIPQRBOB(MNUMPR,IY,2)*CFRBOB(IY)*RDAYS/1000000.
    z[129] = dfd["RFIPQRBOB"].loc[MNUMPR].loc[2] * dfd["CFRBOB"] * RDAYS / 1000000.0

    #     MTBE Imports
    # T107(130,IY,IS)=RFMTBI(MNUMPR,IY)*4.24*RDAYS/1000.
    z[130] = dfd["RFMTBI"].loc[MNUMPR] * 4.24 * RDAYS / 1000.0

    #     Refined Product Exports
    # T107(6,IY,IS)=RFQEXPRDT(MNUMPR,IY)*CFEXPRD(IY)*RDAYS/1000.
    z[6] = dfd["RFQEXPRDT"].loc[MNUMPR] * dfd["CFEXPRD"] * RDAYS / 1000.0

    #     Product Stock Withdrawal
    # T107(131,IY,IS)=PRDSTKWDR(MNUMPR,IY)*CFCRDDOM(IY)*RDAYS/1000.
    z[131] = dfd["PRDSTKWDR"].loc[MNUMPR] * dfd["CFCRDDOM"] * RDAYS / 1000.0

    #     Natural Gas Plant Liquids
    # T107(2,IY,IS)=RFQNGPL(MNUMPR,IY,6)*RDAYS*CFNGL(IY)/1000000.
    z[2] = dfd["RFQNGPL"].loc[MNUMPR].loc[6] * RDAYS * dfd["CFNGL"] / 1000000.0

    #     Domestic Ethanol from Corn
    # T107(132,IY,IS)=ETHANOL_PARAM*CRNETHCD(MNUMCR,IY)*RDAYS*CFPET/1000000.
    z[132] = (
        ETHANOL_PARAM
        * dfd["CRNETHCD"].loc[MNUMCR]
        * RDAYS
        * dfd["CFPET"].iloc[0]
        / 1000000.0
    )

    #     Domestic Ethanol from Cellulose
    # T107(133,IY,IS)=ETHANOL_PARAM*CLLETHCD(MNUMCR,IY)*RDAYS*CFPET/1000000.
    z[133] = (
        ETHANOL_PARAM
        * dfd["CLLETHCD"].loc[MNUMCR]
        * RDAYS
        * dfd["CFPET"].iloc[0]
        / 1000000.0
    )

    #     Domestic Ethanol from Other Grain
    # T107(134,IY,IS)=ETHANOL_PARAM*GRNETHCD(MNUMCR,IY)*RDAYS*CFPET/1000000.
    z[134] = (
        ETHANOL_PARAM
        * dfd["GRNETHCD"].loc[MNUMCR]
        * RDAYS
        * dfd["CFPET"].iloc[0]
        / 1000000.0
    )

    #     Domestic Ethanol from Other
    # T107(86,IY,IS)=ETHANOL_PARAM*OTHETHCD(MNUMCR,IY)*RDAYS*CFPET/1000000.
    z[86] = (
        ETHANOL_PARAM
        * dfd["OTHETHCD"].loc[MNUMCR]
        * RDAYS
        * dfd["CFPET"].iloc[0]
        / 1000000.0
    )

    #     Ethanol Imports
    # T107(99,IY,IS)=ETHIMP(11,IY)*RDAYS/1000000.*CFPET
    z[99] = dfd["ETHIMP"].loc[MNUMCR] * RDAYS / 1000000.0 * dfd["CFPET"].iloc[0]

    #     Ethanol Exports
    # T107(100,IY,IS)=ETHEXP(11,IY)*RDAYS/1000000.*CFPET
    z[100] = dfd["ETHEXP"].loc[MNUMCR] * RDAYS / 1000000.0 * dfd["CFPET"].iloc[0]

    #     Ethanol Stock Withdrawal
    # T107(135,IY,IS)=ETHSTKCHG(IY)*RDAYS*CFETQ(IY)/1000000.
    z[135] = dfd["ETHSTKCHG"] * RDAYS * dfd["CFETQ"] / 1000000.0

    #     Domestic Biodiesel
    # T107(87,IY,IS)=SUM(BIMQTYCD(1:4,MNUMCR,IY))*CFBIOD(IY)*RDAYS/1000000.
    z[87] = (
        dfd["BIMQTYCD"].loc[1:4, MNUMCR, :].sum() * dfd["CFBIOD"] * RDAYS / 1000000.0
    )

    #     Biodiesel Imports
    # T107(111,IY,IS)=BIODIMP(11,IY)/1000000.*RDAYS*CFBIOD(IY)
    z[111] = dfd["BIODIMP"].loc[MNUMCR] / 1000000.0 * RDAYS * dfd["CFBIOD"]

    #     Biodiesel Exports
    # T107(120,IY,IS)=BIODEXP(11,IY)/1000000.*RDAYS*CFBIOD(IY)
    z[120] = dfd["BIODEXP"].loc[MNUMCR] / 1000000.0 * RDAYS * dfd["CFBIOD"]

    #     Biodiesel Stock Withdrawal
    # T107(136,IY,IS)=BIODSTKCHG(IY)*RDAYS*CFBIOD(IY)/1000000.
    z[136] = dfd["BIODSTKCHG"] * RDAYS * dfd["CFBIOD"] / 1000000.0

    #     Domestic Biobutanol
    # T107(118,IY,IS)=RFBIOBUTECD(MNUMCR,IY)*RDAYS*CFBIOBUTE(IY)/1000000.
    z[118] = dfd["RFBIOBUTECD"].loc[MNUMCR] * RDAYS * dfd["CFBIOBUTE"] / 1000000.0

    #     Biobutanol Imports
    # T107(122,IY,IS)=BIOBUTEIMP(IY)/1000000.*RDAYS*CFBIOBUTE(IY)
    z[122] = dfd["BIOBUTEIMP"] / 1000000.0 * RDAYS * dfd["CFBIOBUTE"]

    #     Biobutanol Exports
    # T107(123,IY,IS)=BIOBUTEEXP(IY)/1000000.*RDAYS*CFBIOBUTE(IY)
    z[123] = dfd["BIOBUTEEXP"] / 1000000.0 * RDAYS * dfd["CFBIOBUTE"]

    #     Biobutanol Stock Withdrawal
    # T107(137,IY,IS)=BIOBUTESTK(IY)*RDAYS*CFBIOBUTE(IY)/1000000.
    z[137] = dfd["BIOBUTESTK"] * RDAYS * dfd["CFBIOBUTE"] / 1000000.0

    #     Refinery Produced Biogasoline
    # T107(88,IY,IS)=GRN2MGQTY(MNUMPR,IY)*CFBIOD(IY)*RDAYS/1000000.
    z[88] = dfd["GRN2MGQTY"].loc[MNUMPR] * dfd["CFBIOD"] * RDAYS / 1000000.0

    #     Refinery Produced Biodiesel
    # T107(82,IY,IS)=GRD2DSQTY(MNUMPR,IY)*CFBIOD(IY)*RDAYS/1000000.
    z[82] = dfd["GRD2DSQTY"].loc[MNUMPR] * dfd["CFBIOD"] * RDAYS / 1000000.0

    #     Liquids from Pyrolysis
    # T107(83,IY,IS)=UBAVOL(MNUMPR,IY)*5.763*RDAYS/1000000.
    z[83] = dfd["UBAVOL"].loc[MNUMPR] * 5.763 * RDAYS / 1000000.0

    #     Fischer-Tropsch Liquids from Coal
    # T107(91,IY,IS)=SUM(CTLFRAC(1:4,MNUMPR,IY))*RDAYS/1000000.*CFCTLLIQ(IY)+(SUM(CBTLFRAC(1,1:4,MNUMPR,IY))*CFCBTLLIQ(1,IY))*RDAYS/1000000.
    z[91] = (
        dfd["CTLFRAC"].loc[1:4, MNUMPR, :].sum() * RDAYS / 1000000.0 * dfd["CFCTLLIQ"]
        + (dfd["CBTLFRAC"].loc[1, 1:4, MNUMPR, :].sum() * dfd["CFCBTLLIQ"].loc[1])
        * RDAYS
        / 1000000.0
    )

    #     Fischer-Tropsch Liquids from Natural Gas
    # T107(95,IY,IS)=SUM(GTLFRAC(1:4,MNUMPR,IY))*RDAYS/1000000.*CFGTLLIQ(IY)
    z[95] = (
        dfd["GTLFRAC"].loc[1:4, MNUMPR, :].sum() * RDAYS / 1000000.0 * dfd["CFGTLLIQ"]
    )

    #     Fischer-Tropsch Liquids from Biomass
    # T107(81,IY,IS)=(SUM(BTLFRAC(1:4,MNUMPR,IY))*CFBTLLIQ(IY))*RDAYS/1000000.+(SUM(CBTLFRAC(2,1:4,MNUMPR,IY))*CFCBTLLIQ(2,IY))*RDAYS/1000000.
    z[81] = (
        dfd["BTLFRAC"].loc[1:4, MNUMPR, :].sum() * dfd["CFBTLLIQ"]
    ) * RDAYS / 1000000.0 + (
        dfd["CBTLFRAC"].loc[2, 1:4, MNUMPR, :].sum() * dfd["CFCBTLLIQ"].loc[2]
    ) * RDAYS / 1000000.0

    #     Natural Gas for Hydrogen
    # T107(84,IY,IS)=RFHCXH2IN(MNUMPR,IY)*CFRSQ*RDAYS/1000.
    z[84] = dfd["RFHCXH2IN"].loc[MNUMPR] * dfd["CFRSQ"].iloc[0] * RDAYS / 1000.0

    #     M85
    # T107(138,IY,IS)=RFMETM85(MNUMPR,IY)*CFM85Q(IY)*RDAYS/1000.
    z[138] = dfd["RFMETM85"].loc[MNUMPR] * dfd["CFM85Q"] * RDAYS / 1000.0

    #     Consumption of Petroleum/Liquid Products
    # T107(7,IY,IS)=QTPAS(11,IY)
    z[7] = dfd["QTPAS"].loc[MNUMCR] / 1000.0

    #     Consumption of E85
    # T107(139,IY,IS)=QETTR(11,IY)
    z[139] = dfd["QETTR"].loc[MNUMCR] / 1000.0

    #     Consumption of M85
    # T107(140,IY,IS)=QMETR(11,IY)
    z[140] = dfd["QMETR"].loc[MNUMCR] / 1000.0

    #   Liquids Discrepancy
    # T107( 8,IY,IS) =  &
    # T107(  1,IY,IS) + T107(  3,IY,IS) + T107(  4,IY,IS) - T107(  5,IY,IS) + &
    # T107(126,IY,IS) + T107(127,IY,IS) + T107(128,IY,IS) + &
    # T107(129,IY,IS) + T107(130,IY,IS) - T107(  6,IY,IS) + T107(131,IY,IS) + &
    # T107(  2,IY,IS) + T107(132,IY,IS) + T107(133,IY,IS) + T107(134,IY,IS) + &
    # T107( 86,IY,IS) + T107( 99,IY,IS) - T107(100,IY,IS) + T107(135,IY,IS) + &
    # T107( 87,IY,IS) + T107(111,IY,IS) - T107(120,IY,IS) + T107(136,IY,IS) + &
    # T107(118,IY,IS) + T107(122,IY,IS) - T107(123,IY,IS) + T107(137,IY,IS) + &
    # T107( 88,IY,IS) + T107( 82,IY,IS) + T107( 83,IY,IS) + T107( 91,IY,IS) + &
    # T107( 95,IY,IS) + T107( 81,IY,IS) + T107( 84,IY,IS) + T107(138,IY,IS) - &
    # T107(  7,IY,IS) - T107(139,IY,IS) - T107(140,IY,IS)
    z[8] = (
        z[1]
        + z[3]
        + z[4]
        - z[5]
        + z[126]
        + z[127]
        + z[128]
        + z[129]
        + z[130]
        - z[6]
        + z[131]
        + z[2]
        + z[132]
        + z[133]
        + z[134]
        + z[86]
        + z[99]
        - z[100]
        + z[135]
        + z[87]
        + z[111]
        - z[120]
        + z[136]
        + z[118]
        + z[122]
        - z[123]
        + z[137]
        + z[88]
        + z[82]
        + z[83]
        + z[91]
        + z[95]
        + z[81]
        + z[84]
        + z[138]
        - z[7]
        - z[139]
        - z[140]
    )

    #

    #   Natural Gas

    #     Dry Gas Production
    # T107(9,IY,IS)=CFNGC(IY)*(OGQNGREP(1,IY)+OGQNGREP(2,IY)+OGQNGREP(3,IY)+OGQNGREP(4,IY)+OGQNGREP(5,IY)+OGQNGREP(6,IY)+OGQNGREP(7,IY)+OGQNGREP(8,IY)+OGSHALENG(IY))*.001
    z[9] = (
        dfd["CFNGC"]
        * (
            dfd["OGQNGREP"].loc[1]
            + dfd["OGQNGREP"].loc[2]
            + dfd["OGQNGREP"].loc[3]
            + dfd["OGQNGREP"].loc[4]
            + dfd["OGQNGREP"].loc[5]
            + dfd["OGQNGREP"].loc[6]
            + dfd["OGQNGREP"].loc[7]
            + dfd["OGQNGREP"].loc[8]
            + dfd["OGSHALENG"]
        )
        / 1000.0
    )

    #     Supplemental Gas
    # T107(10,IY,IS)=OGPRSUP(IY)*CFNGC(IY)*.001
    z[10] = dfd["OGPRSUP"] * dfd["CFNGC"] / 1000.0

    #     Imports
    # T107(11,IY,IS)=NGIMPVOL(4,IY)*CFNGI(IY)*.001
    z[11] = dfd["NGIMPVOL"].loc[4] * dfd["CFNGI"] / 1000.0

    #     Exports
    # T107(12,IY,IS)=NGEXPVOL(4,IY)*CFNGE(IY)*.001
    z[12] = dfd["NGEXPVOL"].loc[4] * dfd["CFNGE"] / 1000.0

    #     Consumption
    # T107(13,IY,IS)=QNGAS(11,IY)+QGPTR(11,IY)+QLPIN(11,IY)+QNGLQ(11,IY)+QNGRFPD(MNUMPR,IY)
    z[13] = (
        dfd["QNGAS"].loc[MNUMCR]
        + dfd["QGPTR"].loc[MNUMCR]
        + dfd["QLPIN"].loc[MNUMCR]
        + dfd["QNGLQ"].loc[MNUMCR]
        + dfd["QNGRFPD"].loc[MNUMPR]
    ) / 1000.0

    #     Loss or Whatever
    # T107(106,IY,IS)=NGBAL(IY)*CFNGC(IY)/1000.
    z[106] = dfd["NGBAL"] * dfd["CFNGC"] / 1000.0

    #   Natural Gas Discrepancy
    # T107(14,IY,IS)=FSUM(T107(9,IY,IS),3)-FSUM(T107(12,IY,IS),2)
    z[14] = z[9] + z[10] + z[11] - z[12] - z[13]

    #

    #   Coal

    #     Production
    # T107(15,IY,IS)=CQSBB(3,IY)*.001
    z[15] = dfd["CQSBB"].loc[3] / 1000.0

    #     Waste Coal
    # T107(104,IY,IS)=SUM(WC_PROD_BTU(11,1:MNUMLR,IY))/1000.
    z[104] = dfd["WC_PROD_BTU"].loc[11, 1:MNUMLR, :].sum() / 1000.0

    #     Imports
    # T107(16,IY,IS)=CQDBFB(11,7,IY)*.001
    z[16] = dfd["CQDBFB"].loc[MNUMCR].loc[7] / 1000.0

    #     Exports
    # T107(17,IY,IS)=CQDBFB(11,5,IY)*.001
    z[17] = dfd["CQDBFB"].loc[MNUMCR].loc[5] / 1000.0

    #     Consumption
    # T107(18,IY,IS)=QCLAS(11,IY)+QMCIN(11,IY)+QCLSN(11,IY)
    z[18] = (
        dfd["QCLAS"].loc[MNUMCR]
        + dfd["QMCIN"].loc[MNUMCR]
        + dfd["QCLSN"].loc[MNUMCR]
    ) / 1000.0

    #   Coal Discrepancy
    # T107(19,IY,IS)=FSUM(T107(15,IY,IS),2)+T107(104,IY,IS)-FSUM(T107(17,IY,IS),2)
    z[19] = z[15] + z[16] + z[104] - z[17] - z[18]

    #

    #   Liquids + Natural Gas + Coal Discrepancy
    # T107(115,IY,IS)=T107(8,IY,IS)+T107(14,IY,IS)+T107(19,IY,IS)
    z[115] = z[8] + z[14] + z[19]

    #   Discrepancy from Table 1
    # T107(141,IY,IS)=T1(18,IY,IS)
    # z[141] = dfd['TN 001'].loc[18]  # Cross reference
    # Create a place holder here, and implement in the postprocessor
    z[141] = 0 * z[1]
    # TODO: check RW_postprocessor_base to make sure calculations are correct

    #   Coal to Liquids as seen by Table 1

    #     Production (Coal)
    # T107(90,IY,IS)=QCLSN(11,IY)
    z[90] = dfd["QCLSN"].loc[MNUMCR]

    #     Consumption as Liquids
    # T107(91,IY,IS)=SUM(CTLFRAC(1:4,MNUMPR,IY))*RDAYS/1000000.*CFCTLLIQ(IY)+(SUM(CBTLFRAC(1,1:4,MNUMPR,IY))*CFCBTLLIQ(1,IY))*RDAYS/1000000.
    z[91] = (
        dfd["CTLFRAC"].loc[1:4, MNUMPR, :].sum() * RDAYS / 1000000.0 * dfd["CFCTLLIQ"]
        + (dfd["CBTLFRAC"].loc[1, 1:4, MNUMPR, :].sum() * dfd["CFCBTLLIQ"].loc[1])
        * RDAYS
        / 1000000.0
    )


    #   Gas to Liquids as seen by Table 1


    #     Consumption as Liquids
    # T107(95,IY,IS)=SUM(GTLFRAC(1:4,MNUMPR,IY))*RDAYS/1000000.*CFGTLLIQ(IY)
    z[95] = (
        dfd["GTLFRAC"].loc[1:4, MNUMPR, :].sum() * RDAYS / 1000000.0 * dfd["CFGTLLIQ"]
    )

    #   Ethanol as seen by Table 1

    #     Production of Corn
    # T107(97,IY,IS)=CORNCD(1,11,IY)*CFCORN/1000000000.
    z[97] = dfd["CORNCD"].loc[1].loc[MNUMCR] * dfd["CFCORN"].iloc[0] / 1000000000.0

    #     Production of Cellulose
    # T107(98,IY,IS)=ETHANOL_PARAM*CLLETHCD(11,IY)*RDAYS*CFBMQ(IY)*42./CONEFF(IY)/1000000.
    z[98] = (
        ETHANOL_PARAM
        * dfd["CLLETHCD"].loc[MNUMCR]
        * RDAYS
        * dfd["CFBMQ"]
        * GAL_PER_BBL
        / dfd["CONEFF"]
        / 1000000.0
    )

    #     Imports of Ethanol
    # T107(99,IY,IS)=ETHIMP(11,IY)*RDAYS/1000000.*CFPET
    z[99] = dfd["ETHIMP"].loc[MNUMCR] * RDAYS / 1000000.0 * dfd["CFPET"].iloc[0]

    #     Exports of Ethanol
    # T107(100,IY,IS)=ETHEXP(11,IY)*RDAYS/1000000.*CFPET
    z[100] = dfd["ETHEXP"].loc[MNUMCR] * RDAYS / 1000000.0 * dfd["CFPET"].iloc[0]

    #     Domestic Consumption of Ethanol
    # T107(101,IY,IS)=(ETHANOL_PARAM*(CRNETHCD(11,IY)+CLLETHCD(11,IY)+OTHETHCD(11,IY))+ETHIMP(11,IY)-ETHEXP(11,IY))/1000.*RDAYS*.001*CFPET
    z[101] = (
        (
            ETHANOL_PARAM
            * (
                dfd["CRNETHCD"].loc[MNUMCR]
                + dfd["CLLETHCD"].loc[MNUMCR]
                + dfd["OTHETHCD"].loc[MNUMCR]
            )
            + dfd["ETHIMP"].loc[MNUMCR]
            - dfd["ETHEXP"].loc[MNUMCR]
        )
        / 1000.0
        * RDAYS
        / 1000.0
        * dfd["CFPET"].iloc[0]
    )

    #     Corn Losses
    # T107(102,IY,IS)=CORNCD(1,11,IY)*CFCORN/1000000000.-ETHANOL_PARAM*(CRNETHCD(11,IY)+OTHETHCD(11,IY))*365.*CFPET/1000000.
    z[102] = (
        dfd["CORNCD"].loc[1].loc[MNUMCR] * dfd["CFCORN"].iloc[0] / 1000000000.0
        - ETHANOL_PARAM
        * (dfd["CRNETHCD"].loc[MNUMCR] + dfd["OTHETHCD"].loc[MNUMCR])
        * RDAYS
        * dfd["CFPET"].iloc[0]
        / 1000000.0
    )

    #     Cellulose Losses
    # T107(103,IY,IS)=ETHANOL_PARAM*CLLETHCD(11,IY)*RDAYS/1000000.*(CFBMQ(IY)*42./CONEFF(IY)-CFPET)
    z[103] = (
        ETHANOL_PARAM
        * dfd["CLLETHCD"].loc[MNUMCR]
        * RDAYS
        / 1000000.0
        * (dfd["CFBMQ"] * GAL_PER_BBL / dfd["CONEFF"] - dfd["CFPET"].iloc[0])
    )

    #   Biobutanol as seen by Table 1

    #     Production of Corn
    # T107(121,IY,IS)=CORNCD(2,MNUMCR,IY)*CFCORN/1000000000.
    z[121] = dfd["CORNCD"].loc[2].loc[MNUMCR] * dfd["CFCORN"].iloc[0] / 1000000000.0

    #     Imports of Biobutanol
    # T107(122,IY,IS)=BIOBUTEIMP(IY)/1000000.*RDAYS*CFBIOBUTE(IY)
    z[122] = dfd["BIOBUTEIMP"] / 1000000.0 * RDAYS * dfd["CFBIOBUTE"]

    #     Exports of Biobutanol
    # T107(123,IY,IS)=BIOBUTEEXP(IY)/1000000.*RDAYS*CFBIOBUTE(IY)
    z[123] = dfd["BIOBUTEEXP"] / 1000000.0 * RDAYS * dfd["CFBIOBUTE"]

    #     Corn Losses
    # T107(124,IY,IS)=CORNCD(2,MNUMCR,IY)*CFCORN/1000000000.-RFBIOBUTECD(MNUMCR,IY)/1000000.*RDAYS*CFBIOBUTE(IY)
    z[124] = (
        dfd["CORNCD"].loc[2].loc[MNUMCR] * dfd["CFCORN"].iloc[0] / 1000000000.0
        - dfd["RFBIOBUTECD"].loc[MNUMCR] / 1000000.0 * RDAYS * dfd["CFBIOBUTE"]
    )

    #     Consumption of Biobutanol
    # T107(125,IY,IS)=QBIOBUTE(MNUMCR,IY)/1000000.*RDAYS*CFBIOBUTE(IY)
    z[125] = dfd["QBIOBUTE"].loc[MNUMCR] / 1000000.0 * RDAYS * dfd["CFBIOBUTE"]

    #   Biodiesel as seen by Table 1

    #     Soy for Biodiesel
    # T107(108,IY,IS)=(BIMQTYCD(1,11,IY)+BIMQTYCD(4,11,IY))/1000000.*RDAYS*CFVEGGIE(IY)
    z[108] = (
        (dfd["BIMQTYCD"].loc[1].loc[MNUMCR] + dfd["BIMQTYCD"].loc[4].loc[MNUMCR])
        / 1000000.0
        * RDAYS
        * dfd["CFVEGGIE"]
    )

    #     Yellow Grease for Biodiesel
    # T107(109,IY,IS)=BIMQTYCD(2,11,IY)/1000000.*RDAYS*CFVEGGIE(IY)
    z[109] = dfd["BIMQTYCD"].loc[2].loc[MNUMCR] / 1000000.0 * RDAYS * dfd["CFVEGGIE"]

    #     White Grease for Biodiesel
    # T107(110,IY,IS)=BIMQTYCD(3,11,IY)/1000000.*RDAYS*CFVEGGIE(IY)
    z[110] = dfd["BIMQTYCD"].loc[3].loc[MNUMCR] / 1000000.0 * RDAYS * dfd["CFVEGGIE"]

    #     Imports
    # T107(111,IY,IS)=BIODIMP(11,IY)/1000000.*RDAYS*CFBIOD(IY)
    z[111] = dfd["BIODIMP"].loc[MNUMCR] / 1000000.0 * RDAYS * dfd["CFBIOD"]

    #     Exports
    # T107(120,IY,IS)=BIODEXP(11,IY)/1000000.*RDAYS*CFBIOD(IY)
    z[120] = dfd["BIODEXP"].loc[MNUMCR] / 1000000.0 * RDAYS * dfd["CFBIOD"]

    #     Biodiesel Losses
    # T107(112,IY,IS)=SUM(BIMQTYCD(1:4,11,IY))/1000000.*RDAYS*(CFVEGGIE(IY)-CFBIOD(IY))
    z[112] = (
        dfd["BIMQTYCD"].loc[1:4, 11, :].sum()
        / 1000000.0
        * RDAYS
        * (dfd["CFVEGGIE"] - dfd["CFBIOD"])
    )

    #     Biodiesel Consumption
    # T107(113,IY,IS)=(SUM(BIMQTYCD(1:4,11,IY))+BIODIMP(11,IY)-BIODEXP(11,IY))/1000000.*RDAYS*CFBIOD(IY)
    z[113] = (
        (
            dfd["BIMQTYCD"].loc[1:4, 11, :].sum()
            + dfd["BIODIMP"].loc[MNUMCR]
            - dfd["BIODEXP"].loc[MNUMCR]
        )
        / 1000000.0
        * RDAYS
        * dfd["CFBIOD"]
    )

    #   Biomass to Liquids as seen by Table 1

    #     Biomass In
    # T107(117,IY,IS)=QBMRFBTL(11,IY)/1000.
    z[117] = dfd["QBMRFBTL"].loc[MNUMCR] / 1000.0

    #     Losses
    # T107(119,IY,IS)=T107(117,IY,IS)-T107(81,IY,IS)-T107(83,IY,IS)
    z[119] = z[117] - z[81] - z[83]

    #     Consumption of Liquids from Pyrolysis
    # T107(83,IY,IS)=UBAVOL(MNUMPR,IY)*5.763*RDAYS/1000000.
    z[83] = dfd["UBAVOL"].loc[MNUMPR] * 5.763 * RDAYS / 1000000.0

    #     Consumption of Liquids from Fischer-Tropsch
    # T107(81,IY,IS)=(SUM(BTLFRAC(1:4,MNUMPR,IY))*CFBTLLIQ(IY))*RDAYS/1000000.+(SUM(CBTLFRAC(2,1:4,MNUMPR,IY))*CFCBTLLIQ(2,IY))*RDAYS/1000000.
    z[81] = (
        dfd["BTLFRAC"].loc[1:4, MNUMPR, :].sum() * dfd["CFBTLLIQ"]
    ) * RDAYS / 1000000.0 + (
        dfd["CBTLFRAC"].loc[2, 1:4, MNUMPR, :].sum() * dfd["CFCBTLLIQ"].loc[2]
    ) * RDAYS / 1000000.0

    #

    #

    #   physical units

    #   Liquids (million barrels per day)

    #     Crude Oil Production
    # T107(20,IY,IS)=RFQTDCRD(MNUMOR+2,IY)
    z[20] = dfd["RFQTDCRD"].loc[MNUMOR + 2]

    #     Crude Oil Imports
    # T107(23,IY,IS)=RFQICRD(MNUMPR,IY)
    z[23] = dfd["RFQICRD"].loc[MNUMPR]

    #     Crude Oil Exports
    # T107(169,IY,IS)=RFQEXCRD(MNUMPR,IY)/1000.
    z[169] = dfd["RFQEXCRD"].loc[MNUMPR] / 1000.0

    #     Other Crude Oil Supply
    # T107(176,IY,IS)=RFCRDOTH(MNUMPR,IY)
    z[176] = dfd["RFCRDOTH"].loc[MNUMPR]

    #     Refined Product Imports
    # T107(24,IY,IS)=RFPQIPRDT(MNUMPR,IY,2)
    z[24] = dfd["RFPQIPRDT"].loc[MNUMPR].loc[2]

    #     Unfinished Oil Imports
    # T107(25,IY,IS)=RFPQUFC(MNUMPR,IY,2)
    z[25] = dfd["RFPQUFC"].loc[MNUMPR].loc[2]

    #     Conventional Blending Component Imports
    # T107(145,IY,IS)=RFIPQCBOB(MNUMPR,IY,2)/1000.
    z[145] = dfd["RFIPQCBOB"].loc[MNUMPR].loc[2] / 1000.0

    #     Reformulated Blending Component Imports
    # T107(146,IY,IS)=RFIPQRBOB(MNUMPR,IY,2)/1000.
    z[146] = dfd["RFIPQRBOB"].loc[MNUMPR].loc[2] / 1000.0

    #     MTBE Imports
    # T107(147,IY,IS)=RFMTBI(MNUMPR,IY)
    z[147] = dfd["RFMTBI"].loc[MNUMPR]

    #     Refined Product Exports
    # T107(170,IY,IS)=RFQEXPRDT(MNUMPR,IY)
    z[170] = dfd["RFQEXPRDT"].loc[MNUMPR]

    #     Product Stock Withdrawal
    # T107(148,IY,IS)=PRDSTKWDR(MNUMPR,IY)
    z[148] = dfd["PRDSTKWDR"].loc[MNUMPR]

    #     Refinery Processing Gain
    # T107(22,IY,IS)=RFQPRCG(MNUMPR,IY)
    z[22] = dfd["RFQPRCG"].loc[MNUMPR]

    #     Natural Gas Plant Liquids
    # T107(21,IY,IS)=RFQNGPL(MNUMPR,IY,6)/1000.
    z[21] = dfd["RFQNGPL"].loc[MNUMPR].loc[6] / 1000.0

    #     Domestic Ethanol from Corn
    # T107(149,IY,IS)=ETHANOL_PARAM*CRNETHCD(MNUMCR,IY)/1000.
    z[149] = ETHANOL_PARAM * dfd["CRNETHCD"].loc[MNUMCR] / 1000.0

    #     Domestic Ethanol from Cellulose
    # T107(150,IY,IS)=ETHANOL_PARAM*CLLETHCD(MNUMCR,IY)/1000.
    z[150] = ETHANOL_PARAM * dfd["CLLETHCD"].loc[MNUMCR] / 1000.0

    #     Domestic Ethanol from Other Grain
    # T107(151,IY,IS)=ETHANOL_PARAM*GRNETHCD(MNUMCR,IY)/1000.
    z[151] = ETHANOL_PARAM * dfd["GRNETHCD"].loc[MNUMCR] / 1000.0

    #     Domestic Ethanol from Other
    # T107(152,IY,IS)=ETHANOL_PARAM*OTHETHCD(MNUMCR,IY)/1000.
    z[152] = ETHANOL_PARAM * dfd["OTHETHCD"].loc[MNUMCR] / 1000.0

    #     Ethanol Imports
    # T107(153,IY,IS)=ETHIMP(11,IY)/1000.
    z[153] = dfd["ETHIMP"].loc[MNUMCR] / 1000.0

    #     Ethanol Exports
    # T107(171,IY,IS)=ETHEXP(11,IY)/1000.
    z[171] = dfd["ETHEXP"].loc[MNUMCR] / 1000.0

    #     Ethanol Stock Withdrawal
    # T107(154,IY,IS)=ETHSTKCHG(IY)/1000.
    z[154] = dfd["ETHSTKCHG"] / 1000.0

    #     Domestic Biodiesel
    # T107(155,IY,IS)=SUM(BIMQTYCD(1:4,11,IY))/1000.
    z[155] = dfd["BIMQTYCD"].loc[1:4, 11, :].sum() / 1000.0

    #     Biodiesel Imports
    # T107(156,IY,IS)=(BIODIMP(11,IY)+RENEWDIMP(11,IY))/1000.
    z[156] = (dfd["BIODIMP"].loc[MNUMCR] + dfd["RENEWDIMP"].loc[MNUMCR]) / 1000.0

    #     Biodiesel Exports
    # T107(172,IY,IS)=BIODEXP(11,IY)/1000.
    z[172] = dfd["BIODEXP"].loc[MNUMCR] / 1000.0

    #     Biodiesel Stock Withdrawal
    # T107(157,IY,IS)=BIODSTKCHG(IY)/1000.
    z[157] = dfd["BIODSTKCHG"] / 1000.0

    #     Domestic Biobutanol
    # T107(158,IY,IS)=RFBIOBUTECD(MNUMCR,IY)/1000.
    z[158] = dfd["RFBIOBUTECD"].loc[MNUMCR] / 1000.0

    #     Biobutanol Imports
    # T107(159,IY,IS)=BIOBUTEIMP(IY)/1000.
    z[159] = dfd["BIOBUTEIMP"] / 1000.0

    #     Biobutanol Exports
    # T107(173,IY,IS)=BIOBUTEEXP(IY)/1000.
    z[173] = dfd["BIOBUTEEXP"] / 1000.0

    #     Biobutanol Stock Withdrawal
    # T107(160,IY,IS)=BIOBUTESTK(IY)/1000.
    z[160] = dfd["BIOBUTESTK"] / 1000.0

    #     Refinery Produced Biogasoline
    # T107(161,IY,IS)=GRN2MGQTY(MNUMPR,IY)/1000.
    z[161] = dfd["GRN2MGQTY"].loc[MNUMPR] / 1000.0

    #     Refinery Produced Biodiesel
    # T107(162,IY,IS)=GRD2DSQTY(MNUMPR,IY)/1000.
    z[162] = dfd["GRD2DSQTY"].loc[MNUMPR] / 1000.0

    #     Liquids from Pyrolysis
    # T107(163,IY,IS)=UBAVOL(MNUMPR,IY)/1000.
    z[163] = dfd["UBAVOL"].loc[MNUMPR] / 1000.0

    #     Fischer-Tropsch Liquids from Coal
    # T107(164,IY,IS)=(SUM(CTLFRAC(1:4,MNUMPR,IY))+SUM(CBTLFRAC(1,1:4,MNUMPR,IY)))/1000.
    z[164] = (
        dfd["CTLFRAC"].loc[1:4, MNUMPR, :].sum()
        + dfd["CBTLFRAC"].loc[1, 1:4, MNUMPR, :].sum()
    ) / 1000.0

    #     Fischer-Tropsch Liquids from Natural Gas
    # T107(165,IY,IS)=SUM(GTLFRAC(1:4,MNUMPR,IY))/1000.
    z[165] = dfd["GTLFRAC"].loc[1:4, MNUMPR, :].sum() / 1000.0

    #     Fischer-Tropsch Liquids from Biomass
    # T107(166,IY,IS)=(SUM(BTLFRAC(1:4,MNUMPR,IY))+SUM(CBTLFRAC(2,1:4,MNUMPR,IY)))/1000.
    z[166] = (
        dfd["BTLFRAC"].loc[1:4, MNUMPR, :].sum()
        + dfd["CBTLFRAC"].loc[2, 1:4, MNUMPR, :].sum()
    ) / 1000.0

    #     Natural Gas for Hydrogen
    # T107(167,IY,IS)=RFHCXH2IN(MNUMPR,IY)
    z[167] = dfd["RFHCXH2IN"].loc[MNUMPR]

    #     M85
    # T107(168,IY,IS)=RFMETM85(MNUMPR,IY)
    z[168] = dfd["RFMETM85"].loc[MNUMPR]

    #     Consumption of Petroleum/Liquid Products
    # ''' SZO updated on 7/30/2024 '''
    # fta.f ----------------------------------------------------------------------------------------
    #     ! Product supplied
    #         T107(26,IY,IS) = 0.0                                              ! build total consumption
    #         IF (CFPRQ .NE. 0.0) T107(26,IY,IS) = T107(26,IY,IS) + &
    #                                 (QPRRS(11,IY) + QPRCM(11,IY) + QPRTR(11,IY) + QPRIN(11,IY) + &
    #                                  QPROLENERF(11,IY))/CFPRQ/365.*1000.
    #         IF (CFEEQ .NE. 0.0) T107(26,IY,IS) = T107(26,IY,IS) + QETIN(11,IY)/CFEEQ/365.*1000.
    #         IF (CFBUQ .NE. 0.0) T107(26,IY,IS) = T107(26,IY,IS) + QBUIN(11,IY)/CFBUQ/365.*1000.
    #         IF (CFIBQ .NE. 0.0) T107(26,IY,IS) = T107(26,IY,IS) + QISIN(11,IY)/CFIBQ/365.*1000.
    #         IF (CFPPQ .NE. 0.0) T107(26,IY,IS) = T107(26,IY,IS) + QPPIN(11,IY)/CFPPQ/365.*1000.
    #         IF (CFMGQ(IY) .NE. 0.0) T107(26,IY,IS) = T107(26,IY,IS) + QMGAS(11,IY)/CFMGQ(IY)/365.*1000.
    #         IF (CFJFK .NE. 0.0) T107(26,IY,IS) = T107(26,IY,IS) + QJFTR(11,IY)/CFJFK/365*1000.
    #         IF (CFDSRS(IY) .NE. 0.0) T107(26,IY,IS) = T107(26,IY,IS) + QDSRS(11,IY)/CFDSRS(IY)/365.*1000.
    #         IF (CFDSCM(IY) .NE. 0.0) T107(26,IY,IS) = T107(26,IY,IS) + QDSCM(11,IY)/CFDSCM(IY)/365.*1000.
    #         IF (CFDSIN(IY) .NE. 0.0) T107(26,IY,IS) = T107(26,IY,IS) + QDSIN(11,IY)/CFDSIN(IY)/365.*1000.
    #         IF (CFDSTR(IY) .NE. 0.0) T107(26,IY,IS) = T107(26,IY,IS) + QDSTR(11,IY)/CFDSTR(IY)/365.*1000.
    #         IF (CFDSEL(IY) .NE. 0.0) T107(26,IY,IS) = T107(26,IY,IS) + QDSEL(11,IY)/CFDSEL(IY)/365.*1000.
    #         IF (CFRSQ .NE. 0.0) T107(26,IY,IS) = T107(26,IY,IS) + (QRLAS(11,IY) + QRHAS(11,IY))/CFRSQ/365.*1000.
    #         IF (CFPFQ(IY) .NE. 0.0) T107(26,IY,IS) = T107(26,IY,IS) + QPFIN(11,IY)/CFPFQ(IY)/365.*1000.
    #         IF (CFKSQ .NE. 0.0) T107(26,IY,IS) = T107(26,IY,IS) + QKSAS(11,IY)/CFKSQ/365.*1000.
    #         IF (CFPCQ .NE. 0.0) T107(26,IY,IS) = T107(26,IY,IS) + QPCAS(11,IY)/CFPCQ/365.*1000.
    #         IF (CFASQ .NE. 0.0) T107(26,IY,IS) = T107(26,IY,IS) + QASIN(11,IY)/CFASQ/365.*1000.
    #         IF (CFSGQ .NE. 0.0) T107(26,IY,IS) = T107(26,IY,IS) + QSGIN(11,IY)/CFSGQ/365.*1000.
    #         IF (CFOTQ(IY) .NE. 0.0) T107(26,IY,IS) = T107(26,IY,IS) +(QOTIN(11,IY)-QLUIN(11,IY))/CFOTQ(IY)/365.*1000.
    #         IF (CFLUQ .NE. 0.0) T107(26,IY,IS) = T107(26,IY,IS) +(QLUIN(11,IY)+QLUTR(11,IY))/CFLUQ/365.*1000.
    #         IF (CFAVQ .NE. 0.0) T107(26,IY,IS) = T107(26,IY,IS) + QAGTR(11,IY)/CFAVQ/365.*1000.
    #         IF (CFE85Q(IY) .NE. 0.0) T107(174,IY,IS) = QETTR(11,IY)/CFE85Q(IY)/365.*1000.
    #         IF (CFM85Q(IY) .NE. 0.0) T107(175,IY,IS) = QMETR(11,IY)/CFM85Q(IY)/365.*1000.
    #         T107(27,IY,IS) = FSUM(T107(20,IY,IS),6) + FSUM(T107(145,IY,IS),24) + &     ! Discrepancy
    #                          T107(176,IY,IS) - T107(26,IY,IS) - FSUM(T107(169,IY,IS),7)
    #
    z[26] = 0.0 * z[168]  # build total consumption
    #
    # if dfd['CFPRQ'].iloc[0] != 0.0:
    #     z[26] += (dfd['QPRRS'].loc[MNUMCR] + dfd['QPRCM'].loc[MNUMCR] + dfd['QPRTR'].loc[MNUMCR]
    #       + dfd['QPRIN'].loc[MNUMCR] + dfd['QPROLENERF'].loc[MNUMCR]) / dfd['CFPRQ'].iloc[0] / RDAYS
    # if dfd['CFEEQ'].iloc[0] != 0.0:
    #     z[26] += dfd['QETIN'].loc[MNUMCR] / dfd['CFEEQ'].iloc[0] / RDAYS
    # if dfd['CFBUQ'].iloc[0] != 0.0:
    #     z[26] += dfd['QBUIN'].loc[MNUMCR] / dfd['CFBUQ'].iloc[0] / RDAYS
    # if dfd['CFIBQ'].iloc[0] != 0.0:
    #     z[26] += dfd['QISIN'].loc[MNUMCR] / dfd['CFIBQ'].iloc[0] / RDAYS
    # if dfd['CFPPQ'].iloc[0] != 0.0:
    #     z[26] += dfd['QPPIN'].loc[MNUMCR] / dfd['CFPPQ'].iloc[0] / RDAYS
    # if dfd['CFMGQ'].iloc[0] != 0.0:
    #     z[26] += dfd['QMGAS'].loc[MNUMCR] / dfd['CFMGQ'].iloc[0] / RDAYS
    # if dfd['CFJFK'].iloc[0] != 0.0:
    #     z[26] += dfd['QJFTR'].loc[MNUMCR] / dfd['CFJFK'].iloc[0] / RDAYS
    # if dfd['CFDSRS'].iloc[0] != 0.0:
    #     z[26] += dfd['QDSRS'].loc[MNUMCR] / dfd['CFDSRS'].iloc[0] / RDAYS
    # if dfd['CFDSCM'].iloc[0] != 0.0:
    #     z[26] += dfd['QDSCM'].loc[MNUMCR] / dfd['CFDSCM'].iloc[0] / RDAYS
    # if dfd['CFDSIN'].iloc[0] != 0.0:
    #     z[26] += dfd['QDSIN'].loc[MNUMCR] / dfd['CFDSIN'].iloc[0] / RDAYS
    # if dfd['CFDSTR'].iloc[0] != 0.0:
    #     z[26] += dfd['QDSTR'].loc[MNUMCR] / dfd['CFDSTR'].iloc[0] / RDAYS
    # if dfd['CFDSEL'].iloc[0] != 0.0:
    #     z[26] += dfd['QDSEL'].loc[MNUMCR] / dfd['CFDSEL'].iloc[0] / RDAYS
    # if dfd['CFRSQ'].iloc[0] != 0.0
    #     z[26] += (dfd['QRLAS'].loc[MNUMCR] + dfd['QRHAS'].loc[MNUMCR]) / dfd['CFRSQ'].iloc[0] / RDAYS
    # if dfd['CFPFQ'].iloc[0] != 0.0:
    #     z[26] += dfd['QPFIN'].loc[MNUMCR] / dfd['CFPFQ'].iloc[0] / RDAYS
    # if dfd['CFKSQ'].iloc[0] != 0.0:
    #     z[26] += dfd['QKSAS'].loc[MNUMCR] / dfd['CFKSQ'].iloc[0] / RDAYS
    # if dfd['CFPCQ'].iloc[0] != 0.0:
    #     z[26] += dfd['QPCAS'].loc[MNUMCR] / dfd['CFPCQ'].iloc[0] / RDAYS
    # if dfd['CFASQ'].iloc[0] != 0.0:
    #     z[26] += dfd['QASIN'].loc[MNUMCR] / dfd['CFASQ'].iloc[0] / RDAYS
    # if dfd['CFSGQ'].iloc[0] != 0.0:
    #     z[26] += dfd['QSGIN'].loc[MNUMCR] / dfd['CFSGQ'].iloc[0] / RDAYS
    # if dfd['CFOTQ'].iloc[0] != 0.0:
    #     z[26] += (dfd['QOTIN'].loc[MNUMCR] - dfd['QLUIN'].loc[MNUMCR]) / dfd['CFOTQ'].iloc[0] / RDAYS
    # if dfd['CFLUQ'].iloc[0] != 0.0:
    #     z[26] += (dfd['QLUIN'].loc[MNUMCR] + dfd['QLUTR'].loc[MNUMCR]) / dfd['CFLUQ'].iloc[0] / RDAYS
    # if dfd['CFAVQ'].iloc[0] != 0.0:
    #     z[26] += dfd['QAGTR'].loc[MNUMCR] / dfd['CFAVQ'].iloc[0] / RDAYS
    #
    z[26] = z[26] + (
        (
            dfd["QPRRS"].loc[MNUMCR]
            + dfd["QPRCM"].loc[MNUMCR]
            + dfd["QPRTR"].loc[MNUMCR]
            + dfd["QPRIN"].loc[MNUMCR]
            + dfd["QPROLENERF"].loc[MNUMCR]
        )
        / dfd["CFPRQ"].iloc[0]
        / RDAYS
    ).fillna(0)
    z[26] = z[26] + (dfd["QETIN"].loc[MNUMCR] / dfd["CFEEQ"].iloc[0] / RDAYS).fillna(0)
    z[26] = z[26] + (dfd["QBUIN"].loc[MNUMCR] / dfd["CFBUQ"].iloc[0] / RDAYS).fillna(0)
    z[26] = z[26] + (dfd["QISIN"].loc[MNUMCR] / dfd["CFIBQ"].iloc[0] / RDAYS).fillna(0)
    z[26] = z[26] + (dfd["QPPIN"].loc[MNUMCR] / dfd["CFPPQ"].iloc[0] / RDAYS).fillna(0)
    z[26] = z[26] + (dfd["QMGAS"].loc[MNUMCR] / dfd["CFMGQ"] / RDAYS).fillna(0)
    z[26] = z[26] + (dfd["QJFTR"].loc[MNUMCR] / dfd["CFJFK"].iloc[0] / RDAYS).fillna(0)
    z[26] = z[26] + (dfd["QDSRS"].loc[MNUMCR] / dfd["CFDSRS"] / RDAYS).fillna(0)
    z[26] = z[26] + (dfd["QDSCM"].loc[MNUMCR] / dfd["CFDSCM"] / RDAYS).fillna(0)
    z[26] = z[26] + (dfd["QDSIN"].loc[MNUMCR] / dfd["CFDSIN"] / RDAYS).fillna(0)
    z[26] = z[26] + (dfd["QDSTR"].loc[MNUMCR] / dfd["CFDSTR"] / RDAYS).fillna(0)
    z[26] = z[26] + (dfd["QDSEL"].loc[MNUMCR] / dfd["CFDSEL"] / RDAYS).fillna(0)
    z[26] = z[26] + (
        (dfd["QRLAS"].loc[MNUMCR] + dfd["QRHAS"].loc[MNUMCR])
        / dfd["CFRSQ"].iloc[0]
        / RDAYS
    ).fillna(0)
    z[26] = z[26] + (dfd["QPFIN"].loc[MNUMCR] / dfd["CFPFQ"] / RDAYS).fillna(0)
    z[26] = z[26] + (dfd["QKSAS"].loc[MNUMCR] / dfd["CFKSQ"].iloc[0] / RDAYS).fillna(0)
    z[26] = z[26] + (dfd["QPCAS"].loc[MNUMCR] / dfd["CFPCQ"].iloc[0] / RDAYS).fillna(0)
    z[26] = z[26] + (dfd["QASIN"].loc[MNUMCR] / dfd["CFASQ"].iloc[0] / RDAYS).fillna(0)
    z[26] = z[26] + (dfd["QSGIN"].loc[MNUMCR] / dfd["CFSGQ"].iloc[0] / RDAYS).fillna(0)
    z[26] = z[26] + (
        (dfd["QOTIN"].loc[MNUMCR] - dfd["QLUIN"].loc[MNUMCR]) / dfd["CFOTQ"] / RDAYS
    ).fillna(0)
    z[26] = z[26] + (
        (dfd["QLUIN"].loc[MNUMCR] + dfd["QLUTR"].loc[MNUMCR])
        / dfd["CFLUQ"].iloc[0]
        / RDAYS
    ).fillna(0)
    z[26] = z[26] + (dfd["QAGTR"].loc[MNUMCR] / dfd["CFAVQ"].iloc[0] / RDAYS).fillna(0)

    #     Consumption of E85
    # T107(174,IY,IS)=QETTR(11,IY)/CFE85Q(IY)/365.*1000.
    z[174] = dfd["QETTR"].loc[MNUMCR] / dfd["CFE85Q"] / RDAYS

    #     Consumption of M85
    # T107(175,IY,IS)=QMETR(11,IY)/CFM85Q(IY)/365.*1000.
    z[175] = dfd["QMETR"].loc[MNUMCR] / dfd["CFM85Q"].iloc[0] / RDAYS

    #   Liquids Discrepancy
    # T107(27,IY,IS)=FSUM(T107(20,IY,IS),6)+FSUM(T107(145,IY,IS),24)+T107(176,IY,IS)-T107(26,IY,IS)-FSUM(T107(169,IY,IS),7)
    z[27] = (
        z[20]
        + z[21]
        + z[22]
        + z[23]
        + z[24]
        + z[25]
        + z[145]
        + z[146]
        + z[147]
        + z[148]
        + z[149]
        + z[150]
        + z[151]
        + z[152]
        + z[153]
        + z[154]
        + z[155]
        + z[156]
        + z[157]
        + z[158]
        + z[159]
        + z[160]
        + z[161]
        + z[162]
        + z[163]
        + z[164]
        + z[165]
        + z[166]
        + z[167]
        + z[168]
        + z[176]
        - z[26]
        - (z[169] + z[170] + z[171] + z[172] + z[173] + z[174] + z[175])
    )

    #   Liquids Discrepancy from Table 11
    # T107(142,IY,IS)=T11(50,IY,IS)
    # z[142] = dfd['TN 011'].loc[50]
    # Create a place holder here, and implement in the postprocessor
    z[142] = 0 * z[1]
    # TODO: check RW_postprocessor_base to make sure calculations are correct

    #

    #   Natural Gas (trillion cubic feet)

    #     Dry Gas Production
    # T107(28,IY,IS)=(OGQNGREP(1,IY)+OGQNGREP(2,IY)+OGQNGREP(3,IY)+OGQNGREP(4,IY)+OGQNGREP(5,IY)+OGQNGREP(6,IY)+OGQNGREP(7,IY)+OGQNGREP(8,IY)+OGSHALENG(IY))*.001
    z[28] = (
        dfd["OGQNGREP"].loc[1]
        + dfd["OGQNGREP"].loc[2]
        + dfd["OGQNGREP"].loc[3]
        + dfd["OGQNGREP"].loc[4]
        + dfd["OGQNGREP"].loc[5]
        + dfd["OGQNGREP"].loc[6]
        + dfd["OGQNGREP"].loc[7]
        + dfd["OGQNGREP"].loc[8]
        + dfd["OGSHALENG"]
    ) / 1000.0

    #     Supplemental Gas
    # T107(29,IY,IS)=OGPRSUP(IY)*.001
    z[29] = dfd["OGPRSUP"] / 1000.0

    #     Imports
    # T107(30,IY,IS)=NGIMPVOL(4,IY)*.001
    z[30] = dfd["NGIMPVOL"].loc[4] / 1000.0

    #     Exports
    # T107(31,IY,IS)=NGEXPVOL(4,IY)*.001
    z[31] = dfd["NGEXPVOL"].loc[4] / 1000.0

    #     Consumption
    # IF (CFNGN(IY) .NE. 0.0) T107(32,IY,IS) =  T107(32,IY,IS) + &
    # (QNGRS(11,IY)+QNGCM(11,IY)+QNGIN(11,IY)+QNGTR(11,IY))/CFNGN(IY)
    # IF (CFNGU(IY) .NE. 0.0) T107(32,IY,IS) = T107(32,IY,IS) + QNGEL(11,IY)/CFNGU(IY)
    # IF (CFNGC(IY) .NE. 0.0) T107(32,IY,IS) = T107(32,IY,IS) + (QLPIN(11,IY)+QGPTR(11,IY)+QNGLQ(11,IY) + &
    # QNGRFPD(MNUMPR,IY))/CFNGC(IY)

    # if dfd['CFNGN'].values != 0.0:
    z[32] = (
        (
            dfd["QNGRS"].loc[MNUMCR]
            + dfd["QNGCM"].loc[MNUMCR]
            + dfd["QNGIN"].loc[MNUMCR]
            + dfd["QNGTR"].loc[MNUMCR]
        )
        / dfd["CFNGN"]
    ) / 1000.0
    # if dfd['CFNGU'] != 0.0:
    z[32] += dfd["QNGEL"].loc[MNUMCR] / dfd["CFNGU"] / 1000.0
    # if dfd['CFNGC'] != 0.0:
    z[32] += (
        (
            dfd["QLPIN"].loc[MNUMCR]
            + dfd["QGPTR"].loc[MNUMCR]
            + dfd["QNGLQ"].loc[MNUMCR]
            + dfd["QNGRFPD"].loc[MNUMPR]
        )
        / dfd["CFNGC"]
    ) / 1000.0

    #     Loss or Whatever
    # T107(107,IY,IS)=NGBAL(IY)/1000.
    z[107] = dfd["NGBAL"] / 1000.0

    #   Natural Gas Discrepancy
    # T107(33,IY,IS)=FSUM(T107(28,IY,IS),3)-FSUM(T107(31,IY,IS),2)
    z[33] = z[28] + z[29] + z[30] - z[31] - z[32]

    #   Natural Gas Discrepancy from Table 13
    # T107(143,IY,IS)=T13(16,IY,IS)
    # z[143] = dfd['TN 013'].loc[16]  # Cross reference
    # Create a place holder here, and implement in the postprocessor
    z[143] = 0 * z[1]
    # TODO: check RW_postprocessor_base to make sure calculations are correct

    #   Coal (million tons)

    #     Production
    # T107(34,IY,IS)=CQSBB(3,IY)/CQSBT(3,IY)
    z[34] = dfd["CQSBB"].loc[3] / dfd["CQSBT"].loc[3]

    #     Waste Coal
    # T107(105,IY,IS)=SUM(WC_PROD_ST(11,1:MNUMLR,IY))
    z[105] = dfd["WC_PROD_ST"].loc[11, 1:MNUMLR, :].sum()

    #     Imports
    # T107(35,IY,IS)=CQDBFB(11,7,IY)/CQDBFT(11,7,IY)
    z[35] = dfd["CQDBFB"].loc[MNUMCR].loc[7] / dfd["CQDBFT"].loc[MNUMCR].loc[7]

    #     Exports
    # T107(36,IY,IS)=CQDBFB(11,5,IY)/CQDBFT(11,5,IY)
    z[36] = dfd["CQDBFB"].loc[MNUMCR].loc[5] / dfd["CQDBFT"].loc[MNUMCR].loc[5]

    # '''Updated by SZO on 7/30/2027'''
    #     Consumption
    #   ftab,f
    # 'Cumul. Capacity as of... ',CUMCAPADD
    # ...
    # IF (CUMCAPADD .EQ. 0) CUMCAPADD = FYRPRC
    # ...
    #         T107(37,IY,IS) = 0.0                                                   ! build total consumption
    #         IF (CQDBFT(11,4,IY) .NE. 0.0) T107(37,IY,IS) = T107(37,IY,IS) + QCLSN(11,IY)/CQDBFT(11,4,IY)*1000.
    #         IF (CQDBFT(11,1,IY) .NE. 0.0) T107(37,IY,IS) = T107(37,IY,IS) + QCLRS(11,IY)/CQDBFT(11,1,IY)*1000.
    #         IF (CQDBFT(11,1,IY) .NE. 0.0) T107(37,IY,IS) = T107(37,IY,IS) + QCLCM(11,IY)/CQDBFT(11,1,IY)*1000.
    # IF (IY .GT. (CUMCAPADD+2)) THEN
    #         IF (CQDBFT(11,2,IY) .NE. 0.0) T107(37,IY,IS) = T107(37,IY,IS) +(QCLIN(11,IY))/CQDBFT(11,2,IY)*1000.
    # ELSE
    #         IF (CQDBFT(11,2,IY) .NE. 0.0) T107(37,IY,IS) = T107(37,IY,IS) + QCLIN(11,IY)/CQDBFT(11,2,IY)*1000.
    # ENDIF
    #         IF (CQDBFT(11,3,IY) .NE. 0.0) T107(37,IY,IS) = T107(37,IY,IS) + QMCIN(11,IY)/CQDBFT(11,3,IY)*1000.
    #         IF (CQDBFT(11,6,IY) .NE. 0.0) T107(37,IY,IS) = T107(37,IY,IS) + QCLEL(11,IY)/CQDBFT(11,6,IY)*1000.
    #         T107(38,IY,IS) = FSUM(T107(34,IY,IS),2) + T107(105,IY,IS) - FSUM(T107(36,IY,IS),2)   ! discrepancy
    #
    FYRPRC = int(table_spec["ftab_dat"]["FYRPRC"])
    year_index = table_spec["years"].index(FYRPRC) + 1  # !!!
    #
    z[37] = (dfd["QCLSN"].loc[MNUMCR] / dfd["CQDBFT"].loc[MNUMCR].loc[4]).fillna(0)
    z[37] += (dfd["QCLRS"].loc[MNUMCR] / dfd["CQDBFT"].loc[MNUMCR].loc[1]).fillna(0)
    z[37] += (dfd["QCLCM"].loc[MNUMCR] / dfd["CQDBFT"].loc[MNUMCR].loc[1]).fillna(0)
    z[37][year_index:] += (
        (dfd["QCLIN"].loc[MNUMCR][year_index:])
        / dfd["CQDBFT"].loc[MNUMCR].loc[2][year_index:]
    ).fillna(0)
    z[37][:year_index] += (
        dfd["QCLIN"].loc[MNUMCR][:year_index]
        / dfd["CQDBFT"].loc[MNUMCR].loc[2][:year_index]
    ).fillna(0)
    z[37] += (dfd["QMCIN"].loc[MNUMCR] / dfd["CQDBFT"].loc[MNUMCR].loc[3]).fillna(0)
    z[37] += (dfd["QCLEL"].loc[MNUMCR] / dfd["CQDBFT"].loc[MNUMCR].loc[6]).fillna(0)

    #   Coal Discrepancy
    # T107(38,IY,IS)=FSUM(T107(34,IY,IS),2)+T107(105,IY,IS)-FSUM(T107(36,IY,IS),2)
    z[38] = (
        z[34].fillna(0)
        + z[35].fillna(0)
        + z[105].fillna(0)
        - z[36].fillna(0)
        - z[37].fillna(0)
    )

    #   Coal Discrepancy from Table 15
    # T107(144,IY,IS)=T15(16,IY,IS)
    # z[144] = dfd['TN 015'].loc[16]  # Cross reference
    # Create a place holder here, and implement in the postprocessor
    z[144] = 0 * z[1]
    # TODO: check RW_postprocessor_base to make sure calculations are correct

    #

    #   Petroleum Product Balances

    #   (million barrels per day)

    #     Motor Gasoline

    #       External Streams
    # T107(80,IY,IS)=(CRNETHCD(11,IY)+CLLETHCD(11,IY)+OTHETHCD(11,IY)+GRNETHCD(11,IY)+ETHIMP(11,IY)-ETHEXP(11,IY))/1000.
    z[80] = (
        dfd["CRNETHCD"].loc[MNUMCR]
        + dfd["CLLETHCD"].loc[MNUMCR]
        + dfd["OTHETHCD"].loc[MNUMCR]
        + dfd["GRNETHCD"].loc[MNUMCR]
        + dfd["ETHIMP"].loc[MNUMCR]
        - dfd["ETHEXP"].loc[MNUMCR]
    ) / 1000.0

    #       Product Supplied
    # T107(42,IY,IS)=RFQMG(11,IY)
    z[42] = dfd["RFQMG"].loc[MNUMCR]

    #       Exports
    # T107(41,IY,IS)=QPRDEX(2,IY)+QPRDEX(3,IY)+QPRDEX(4,IY)+QPRDEX(5,IY)+QPRDEX(26,IY)
    z[41] = (
        dfd["QPRDEX"].loc[2]
        + dfd["QPRDEX"].loc[3]
        + dfd["QPRDEX"].loc[4]
        + dfd["QPRDEX"].loc[5]
        + dfd["QPRDEX"].loc[26]
    )

    #       Imports
    # T107(40,IY,IS)=(RFIPQMG(MNUMPR,IY,2)+RFIPQRG(MNUMPR,IY,2)+RFIPQCBOB(MNUMPR,IY,2)+RFIPQRBOB(MNUMPR,IY,2)+RFIPQCG(MNUMPR,IY,2))/1000.
    z[40] = (
        dfd["RFIPQMG"].loc[MNUMPR].loc[2]
        + dfd["RFIPQRG"].loc[MNUMPR].loc[2]
        + dfd["RFIPQCBOB"].loc[MNUMPR].loc[2]
        + dfd["RFIPQRBOB"].loc[MNUMPR].loc[2]
        + dfd["RFIPQCG"].loc[MNUMPR].loc[2]
    ) / 1000.0

    #       Implied Domestic Production
    # T107(39,IY,IS)=T107(42,IY,IS)+T107(41,IY,IS)-T107(40,IY,IS)-T107(80,IY,IS)
    z[39] = z[42] + z[41] - z[40] - z[80]

    #

    #     Distillate Fuel Oil

    #       External Streams
    # T107(116,IY,IS)=(SUM(BIMQTYCD(1:4,11,IY))+SUM(GTLFRAC(1:4,MNUMPR,IY))+SUM(CTLFRAC(1:4,MNUMPR,IY))+SUM(BTLFRAC(1:4,MNUMPR,IY))+SUM(CBTLFRAC(1:2,1:4,MNUMPR,IY))+SBO2GDTPD(MNUMPR,IY)+YGR2GDTPD(MNUMPR,IY)+WGR2GDTPD(MNUMPR,IY))/1000.
    z[116] = (
        dfd["BIMQTYCD"].loc[1:4, MNUMCR, :].sum()
        + dfd["GTLFRAC"].loc[1:4, MNUMPR, :].sum()
        + dfd["CTLFRAC"].loc[1:4, MNUMPR, :].sum()
        + dfd["BTLFRAC"].loc[1:4, MNUMPR, :].sum()
        + dfd["CBTLFRAC"].loc[1:2, 1:4, MNUMPR, :].sum()
        + dfd["SBO2GDTPD"].loc[MNUMPR]
        + dfd["YGR2GDTPD"].loc[MNUMPR]
        + dfd["WGR2GDTPD"].loc[MNUMPR]
    ) / 1000.0

    #       Imports
    # T107(44,IY,IS)=(RFIPQDS(MNUMPR,IY,2)+RFIPQDL(MNUMPR,IY,2)+RFIPQDU(MNUMPR,IY,2)+BIODIMP(11,IY)-BIODEXP(11,IY)+RENEWDIMP(11,IY))/1000.
    z[44] = (
        dfd["RFIPQDS"].loc[MNUMPR].loc[2]
        + dfd["RFIPQDL"].loc[MNUMPR].loc[2]
        + dfd["RFIPQDU"].loc[MNUMPR].loc[2]
        + dfd["BIODIMP"].loc[MNUMCR]
        - dfd["BIODEXP"].loc[MNUMCR]
        + dfd["RENEWDIMP"].loc[MNUMCR]
    ) / 1000.0

    #       Exports
    # T107(45,IY,IS)=QPRDEX(7,IY)+QPRDEX(13,IY)+QPRDEX(24,IY)+QPRDEX(25,IY)
    z[45] = (
        dfd["QPRDEX"].loc[7]
        + dfd["QPRDEX"].loc[13]
        + dfd["QPRDEX"].loc[24]
        + dfd["QPRDEX"].loc[25]
    )

    #       Product Supplied
    # T107(46,IY,IS)=RFQDS(11,IY)
    z[46] = dfd["RFQDS"].loc[MNUMCR]

    #       Implied Domestic Supply
    # T107(43,IY,IS)=T107(46,IY,IS)+T107(45,IY,IS)-T107(44,IY,IS)-T107(116,IY,IS)
    z[43] = z[46] + z[45] - z[44] - z[116]

    #     Residual Fuel Oil

    #       Imports
    # T107(48,IY,IS)=(RFIPQRL(MNUMPR,IY,2)+RFIPQRH(MNUMPR,IY,2))/1000.
    z[48] = (
        dfd["RFIPQRL"].loc[MNUMPR].loc[2] + dfd["RFIPQRH"].loc[MNUMPR].loc[2]
    ) / 1000.0

    #       Exports
    # T107(49,IY,IS)=QPRDEX(8,IY)+QPRDEX(9,IY)
    z[49] = dfd["QPRDEX"].loc[8] + dfd["QPRDEX"].loc[9]

    #       Product Supplied
    # T107(50,IY,IS)=RFQRL(11,IY)+RFQRH(11,IY)
    z[50] = dfd["RFQRL"].loc[MNUMCR] + dfd["RFQRH"].loc[MNUMCR]

    #       Implied Domestic Supply
    # T107(47,IY,IS)=T107(50,IY,IS)+T107(49,IY,IS)-T107(48,IY,IS)
    z[47] = z[50] + z[49] - z[48]

    #

    #   more Petroleum Product Balances

    #     Jet Fuel

    #       Imports
    # T107(52,IY,IS)=RFIPQJF(MNUMPR,IY,2)/1000.
    z[52] = dfd["RFIPQJF"].loc[MNUMPR].loc[2] / 1000.0

    #       Exports
    # T107(53,IY,IS)=QPRDEX(6,IY)
    z[53] = dfd["QPRDEX"].loc[6]

    #       Product Supplied
    # T107(54,IY,IS)=RFQJF(11,IY)
    z[54] = dfd["RFQJF"].loc[MNUMCR]

    #       Implied Domestic Supply
    # T107(51,IY,IS)=T107(54,IY,IS)+T107(53,IY,IS)-T107(52,IY,IS)
    z[51] = z[54] + z[53] - z[52]

    #     Ethane

    #       External Streams
    # T107(177,IY,IS)=RFQNGPL(MNUMPR,IY,1)/1000.
    z[177] = dfd["RFQNGPL"].loc[MNUMPR].loc[1] / 1000.0

    #       Imports
    # T107(179,IY,IS)=RFIPQET(MNUMPR,IY,2)/1000.
    # ftab.f ...
    # RFIPQET(:,:,1) = RFIPQET(:,:,1) * SCALPR
    # dfd['RFIPQET'].loc[(slice(None), 1), :] = dfd['RFIPQET'].loc[(slice(None), 1), :] * SCALPR2
    z[179] = dfd["RFIPQET"].loc[MNUMPR].loc[2] / 1000.0

    #       Exports
    # T107(180,IY,IS)=QPRDEX(27,IY)
    z[180] = dfd["QPRDEX"].loc[27]

    #       Implied Refinery Inputs
    # T107(181,IY,IS)=0
    z[181] = 0 * z[51]

    #       Product Supplied
    # T107(182,IY,IS)=QETIN(MNUMCR,IY)/CFEEQ/365*1000.
    z[182] = dfd["QETIN"].loc[MNUMCR] / dfd["CFEEQ"].iloc[0] / RDAYS

    #       Implied Net Refinery Supply
    # T107(178,IY,IS)=FSUM(T107(180,IY,IS),3)-T107(177,IY,IS)-T107(179,IY,IS)
    z[178] = z[180] + z[181] + z[182] - z[177] - z[179]

    #     Propane

    #       External Streams
    # T107(55,IY,IS)=RFQNGPL(MNUMPR,IY,2)/1000.
    z[55] = dfd["RFQNGPL"].loc[MNUMPR].loc[2] / 1000.0

    #       Imports
    # T107(57,IY,IS)=RFIPQPR(MNUMPR,IY,2)/1000.
    z[57] = dfd["RFIPQPR"].loc[MNUMPR].loc[2] / 1000.0

    #       Exports
    # T107(58,IY,IS)=QPRDEX(1,IY)
    z[58] = dfd["QPRDEX"].loc[1]

    #       Implied Refinery Inputs
    # T107(85,IY,IS)=0
    z[85] = 0 * z[1]

    #       Product Supplied
    # T107(59,IY,IS)=(QPRRS(MNUMCR,IY)+QPRCM(MNUMCR,IY)+QPRIN(MNUMCR,IY)+QPRTR(MNUMCR,IY))/CFPRQ/365.*1000.
    z[59] = (
        (
            dfd["QPRRS"].loc[MNUMCR]
            + dfd["QPRCM"].loc[MNUMCR]
            + dfd["QPRIN"].loc[MNUMCR]
            + dfd["QPRTR"].loc[MNUMCR]
        )
        / dfd["CFPRQ"].iloc[0]
        / RDAYS
    )

    #       Net Domestic Refinery Supply
    # T107(56,IY,IS)=T107(59,IY,IS)+T107(58,IY,IS)+T107(85,IY,IS)-T107(55,IY,IS)-T107(57,IY,IS)
    z[56] = z[59] + z[58] + z[85] - z[55] - z[57]

    #     Butane

    #       External Streams
    # T107(183,IY,IS)=RFQNGPL(MNUMPR,IY,3)/1000.
    z[183] = dfd["RFQNGPL"].loc[MNUMPR].loc[3] / 1000.0

    #       Imports
    # T107(185,IY,IS)=RFIPQBU(MNUMPR,IY,2)/1000.
    z[185] = dfd["RFIPQBU"].loc[MNUMPR].loc[2] / 1000.0

    #       Exports
    # T107(186,IY,IS)=QPRDEX(15,IY)
    z[186] = dfd["QPRDEX"].loc[15]

    #       Implied Refinery Inputs
    # T107(187,IY,IS)=0
    z[187] = 0 * z[1]

    #       Product Supplied
    # T107(188,IY,IS)=QBUIN(MNUMCR,IY)/CFBUQ/365*1000.
    z[188] = dfd["QBUIN"].loc[MNUMCR] / dfd["CFBUQ"].iloc[0] / RDAYS

    #       Net Domestic Refinery Supply
    # T107(184,IY,IS)=FSUM(T107(186,IY,IS),3)-T107(183,IY,IS)-T107(185,IY,IS)
    z[184] = z[186] + z[187] + z[188] - z[183] - z[185]

    #     Isobutane

    #       External Streams
    # T107(189,IY,IS)=RFQNGPL(MNUMPR,IY,4)/1000.
    z[189] = dfd["RFQNGPL"].loc[MNUMPR].loc[4] / 1000.0

    #       Imports
    # T107(191,IY,IS)=RFIPQIS(MNUMPR,IY,2)/1000.
    z[191] = dfd["RFIPQIS"].loc[MNUMPR].loc[2] / 1000.0

    #       Exports
    # T107(192,IY,IS)=QPRDEX(28,IY)
    z[192] = dfd["QPRDEX"].loc[28]

    #       Implied Refinery Inputs
    # T107(193,IY,IS)=0
    z[193] = 0 * z[1]

    #       Product Supplied
    # T107(194,IY,IS)=QISIN(MNUMCR,IY)/CFIBQ/365*1000.
    z[194] = dfd["QISIN"].loc[MNUMCR] / dfd["CFIBQ"].iloc[0] / RDAYS

    #       Net Domestic Refinery Supply
    # T107(190,IY,IS)=FSUM(T107(192,IY,IS),3)-T107(189,IY,IS)-T107(191,IY,IS)
    z[190] = z[192] + z[193] + z[194] - z[189] - z[191]

    #     Propylene

    #       External Streams
    # T107(195,IY,IS)=0
    z[195] = 0 * z[1]

    #       Imports
    # T107(197,IY,IS)=RFIPQPY(MNUMPR,IY,2)/1000.
    z[197] = dfd["RFIPQPY"].loc[MNUMPR].loc[2] / 1000.0

    #       Exports
    # T107(198,IY,IS)=QPRDEX(14,IY)
    z[198] = dfd["QPRDEX"].loc[14]

    #       Implied Refinery Inputs
    # T107(199,IY,IS)=0
    z[199] = 0 * z[1]

    #       Product Supplied
    # T107(200,IY,IS)=QPROLENERF(MNUMCR,IY)/CFPRQ/365*1000.
    z[200] = dfd["QPROLENERF"].loc[MNUMCR] / dfd["CFPRQ"].iloc[0] / RDAYS

    #       Net Domestic Refinery Supply
    # T107(196,IY,IS)=FSUM(T107(198,IY,IS),3)-T107(195,IY,IS)-T107(197,IY,IS)
    z[196] = z[198] + z[199] + z[200] - z[195] - z[197]

    #     Natural Gasoline

    #       External Streams
    # T107(201,IY,IS)=RFQNGPL(MNUMPR,IY,5)/1000.
    z[201] = dfd["RFQNGPL"].loc[MNUMPR].loc[5] / 1000.0

    #       Imports
    # T107(203,IY,IS)=RFIPQPP(MNUMPR,IY,2)/1000.
    z[203] = dfd["RFIPQPP"].loc[MNUMPR].loc[2] / 1000.0

    #       Exports
    # T107(204,IY,IS)=QPRDEX(29,IY)
    z[204] = dfd["QPRDEX"].loc[29]

    #       Implied Refinery Inputs
    # T107(205,IY,IS)=0
    z[205] = 0 * z[1]

    #       Product Supplied
    # T107(206,IY,IS)=QPPIN(MNUMCR,IY)/CFPPQ/365*1000.
    z[206] = dfd["QPPIN"].loc[MNUMCR] / dfd["CFPPQ"].iloc[0] / RDAYS

    #       Net Domestic Refinery Supply
    # T107(202,IY,IS)=FSUM(T107(204,IY,IS),3)-T107(201,IY,IS)-T107(203,IY,IS)
    z[202] = z[204] + z[205] + z[206] - z[201] - z[203]

    #     Petroleum Coke

    #       Implied Imports
    # T107(68,IY,IS)=0
    z[68] = 0 * z[1]

    #       Exports
    # T107(61,IY,IS)=QPRDEX(16,IY)
    z[61] = dfd["QPRDEX"].loc[16]

    #       Product Supplied
    # T107(62,IY,IS)=RFQPCK(11,IY)
    z[62] = dfd["RFQPCK"].loc[MNUMCR]

    #       Implied Domestic Supply
    # T107(60,IY,IS)=T107(62,IY,IS)+T107(61,IY,IS)-T107(68,IY,IS)
    z[60] = z[62] + z[61] - z[68]

    #     Still Gas

    #       Product Supplied
    # T107(64,IY,IS)=RFQSTG(11,IY)
    z[64] = dfd["RFQSTG"].loc[MNUMCR]

    #       Implied Domestic Supply
    # T107(63,IY,IS)=T107(64,IY,IS)
    z[63] = z[64]

    #

    #     Asphalt and Road Oil

    #       Imports
    # T107(89,IY,IS)=RFIPQAS(MNUMPR,IY,2)/1000.
    z[89] = dfd["RFIPQAS"].loc[MNUMPR].loc[2] / 1000.0

    #       Exports
    # T107(73,IY,IS)=QPRDEX(12,IY)
    z[73] = dfd["QPRDEX"].loc[12]

    #       Product Supplied
    # T107(66,IY,IS)=RFQARO(11,IY)
    z[66] = dfd["RFQARO"].loc[MNUMCR]

    #       Implied Domestic Supply
    # T107(65,IY,IS)=T107(66,IY,IS)+T107(73,IY,IS)-T107(89,IY,IS)
    z[65] = z[66] + z[73] - z[89]

    #

    #     Unfinished Oils

    #       Imports
    # T107(67,IY,IS)=RFPQUFC(MNUMPR,IY,2)
    z[67] = dfd["RFPQUFC"].loc[MNUMPR].loc[2]

    #         Atmospheric Residuum
    # T107(209,IY,IS)=RFIPQAR3(MNUMPR,IY,2)/1000.
    z[209] = dfd["RFIPQAR3"].loc[MNUMPR].loc[2] / 1000.0

    #         Medium Naphtha
    # T107(210,IY,IS)=RFIPQMN3(MNUMPR,IY,2)/1000.
    z[210] = dfd["RFIPQMN3"].loc[MNUMPR].loc[2] / 1000.0

    #         Gas Oil
    # T107(211,IY,IS)=RFIPQGO3(MNUMPR,IY,2)/1000.
    z[211] = dfd["RFIPQGO3"].loc[MNUMPR].loc[2] / 1000.0

    #       Exports
    # T107(207,IY,IS)=QPRDEX(20,IY)+QPRDEX(21,IY)+QPRDEX(10,IY)+QPRDEX(23,IY)
    z[207] = (
        dfd["QPRDEX"].loc[20]
        + dfd["QPRDEX"].loc[21]
        + dfd["QPRDEX"].loc[10]
        + dfd["QPRDEX"].loc[23]
    )

    #         Atmospheric Residuum
    # T107(212,IY,IS)=QPRDEX(20,IY)
    z[212] = dfd["QPRDEX"].loc[20]

    #         Medium Naphtha
    # T107(213,IY,IS)=QPRDEX(21,IY)
    z[213] = dfd["QPRDEX"].loc[21]

    #         Gas Oil
    # T107(214,IY,IS)=QPRDEX(10,IY)
    z[214] = dfd["QPRDEX"].loc[10]

    #         Gas Oil Co-Product
    # T107(215,IY,IS)=QPRDEX(23,IY)
    z[215] = dfd["QPRDEX"].loc[23]

    #       Implied Refinery Inputs
    # T107(208,IY,IS)=T107(67,IY,IS)-T107(207,IY,IS)
    z[208] = z[67] - z[207]

    #

    #     Petrochemical Feedstocks

    #       External Streams

    #       Imports
    # T107(70,IY,IS)=RFIPQPF(MNUMPR,IY,2)/1000.
    z[70] = dfd["RFIPQPF"].loc[MNUMPR].loc[2] / 1000.0

    #       Exports
    # T107(71,IY,IS)=QPRDEX(11,IY)
    z[71] = dfd["QPRDEX"].loc[MNUMCR]

    #       Product Supplied
    # T107(72,IY,IS)=RFQPF(11,IY)
    z[72] = dfd["RFQPF"].loc[MNUMCR]

    #       Implied Domestic Supply
    # T107(69,IY,IS)=T107(72,IY,IS)+T107(71,IY,IS)-T107(70,IY,IS)
    z[69] = z[72] + z[71] - z[70]

    #   more Petroleum Product Balances

    #     Other

    #       Imports
    # T107(75,IY,IS)=(RFIPQLU(MNUMPR,IY,2)+RFIPQAG(MNUMPR,IY,2))/1000.
    z[75] = (
        dfd["RFIPQLU"].loc[MNUMPR].loc[2] + dfd["RFIPQAG"].loc[MNUMPR].loc[2]
    ) / 1000.0

    #       Refinery Inputs
    # T107(76,IY,IS)=0
    z[76] = 0 * z[1]

    #       Exports
    # T107(77,IY,IS)=QPRDEX(18,IY)+QPRDEX(19,IY)+QPRDEX(20,IY)+QPRDEX(21,IY)
    z[77] = (
        dfd["QPRDEX"].loc[18]
        + dfd["QPRDEX"].loc[19]
        + dfd["QPRDEX"].loc[20]
        + dfd["QPRDEX"].loc[21]
    )

    #       Product Supplied
    # T107(78,IY,IS)=RFQOTH(11,IY)+RFQKS(11,IY)
    z[78] = dfd["RFQOTH"].loc[MNUMCR] + dfd["RFQKS"].loc[MNUMCR]

    #       Implied Domestic Supply
    # T107(74,IY,IS)=T107(78,IY,IS)+T107(77,IY,IS)+T107(76,IY,IS)-T107(75,IY,IS)
    z[74] = z[78] + z[77] + z[76] - z[75]

    return z
