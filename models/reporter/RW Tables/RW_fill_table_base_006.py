# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
SZO fixed on 7/18/2024
"""


def fill_table_base_006(dfd, table_spec, table_id):
    """Fill table Industrial Sector Key Indicators and Consumption.

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

    # Set year and deflators for price conversions
    FYRPRC = int(table_spec["settings"]["year_index_real_dol_priceyr"])
    # Get inflation index from 1990->2050
    MC_JPGDP = dfd["MC_JPGDP"].loc[1990:]

    # Get inflation index of FYRPRC (2022)
    SCALPR2 = MC_JPGDP.loc[FYRPRC]  # 2022 = column 33

    MNUMPR = dfd["MNUMPR_rwpre"]
    MNUMCR = dfd["MNUMCR_rwpre"]

    IXEL = 1
    IXNG = 2
    IXCL = 3
    IXMC = 4
    IXCI = 5
    IXRF = 6
    IXDS = 7
    IXLG = 8
    IXMG = 9
    IXSG = 10
    IXPC = 11
    IXAS = 12
    IXPF = 13
    IXKS = 14
    IXOP = 15
    IXNF = 16
    IXLF = 17
    IXRN = 18
    IXHF = 19

    RDAYS = dfd["RDAYS_rwpre"]
    TRIL_TO_QUAD = dfd["TRIL_TO_QUAD_rwpre"]
    ELECTRO_EFF = 0.6017 # This variable was not defined for AEO2025, needs to be defined in restart file
    ELECLOSS = dfd["QTSEL"].loc[MNUMCR] + (1 - ELECTRO_EFF) * dfd["QH2EL"].loc[MNUMCR] - dfd["QELAS"].loc[MNUMCR]
    H2LOSS = (dfd["QNGHMPF"].loc[MNUMCR]  + dfd["QNGHMHP"].loc[MNUMCR]  + dfd["QELHM"].loc[MNUMCR]  + dfd["BYPRDH2IN"].loc[MNUMCR]  +
              ELECLOSS*(dfd["QELHM"].loc[MNUMCR] /dfd["QELAS"].loc[MNUMCR] ) - (1-ELECTRO_EFF)*dfd["QH2EL"].loc[MNUMCR]  - dfd["QH2IN"].loc[MNUMCR]  - dfd["QH2TR"].loc[MNUMCR]  - dfd["QELHM"].loc[MNUMCR] )
    ETH_CNV = dfd["ETHANOL_PARAM_rwpre"]

    MBCD_CNV = dfd["CFUBA_rwpre"]
    TOT_H2_CONS = (dfd['PRDH2NG'].loc[11] + dfd['PRDH2NG_CCS'].loc[11] + dfd['PRDH2EL'].loc[11])/dfd['CFH2Q_KG'].iloc[0] * 1000 + dfd['BYPRDH2IN'].loc[11]
    #TOT_H2_CONS = dfd["QH2IN"].loc[MNUMCR] + dfd["QH2TR"].loc[MNUMCR] + dfd["QH2EL"].loc[MNUMCR] 
    # preprocess propane, move to preprocessor
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

    # Industrial Sector Key Indicators and Consumption
    # Shipments, Prices, and Consumption

    #  1 (food) and 16 (other chemical) are totals of 2-5 and 17-20, respectively, so don't include
    #  also, ethanol (17) and flat glass (29) are subsets of 16 and 28
    #  calculation is from the fortran code
    z[1] = (
        dfd["MC_REVIND"].loc[11].loc[1]
        + dfd["MC_REVIND"].loc[11].loc[6:10].sum()
        + dfd["MC_REVIND"].loc[11].loc[14:16].sum()
        + dfd["MC_REVIND"].loc[11].loc[18:20].sum()
        + dfd["MC_REVIND"].loc[11].loc[25:28].sum()
        + dfd["MC_REVIND"].loc[11].loc[30:41].sum()
    )
    z[2] = dfd["MC_REVIND"].loc[11].loc[42:48].sum()
    #   Total
    #   T6(3,IY,IS)=T6(1,IY,IS)+T6(2,IY,IS)
    z[3] = z[1] + z[2]
    # Energy Prices
    #  (#### dollars per million Btu)
    #   Propane
    # T6( 9,IY,IS) = PLGIN(11,IY)
    #! but it is simpler (the row in now labelled "propane") if we just use the propane price variable:
    # T6( 9,IY,IS) = PPRIN(11,IY)
    z[9] = dfd["APMORE/PPRIN"].loc[MNUMCR] * SCALPR2
    #   Propane Feedstocks
    #   T6(67,IY,IS)=PPRINPF(11,IY)
    z[67] = dfd["PPRINPF"].loc[MNUMCR] * SCALPR2
    #   Petrochemical Feedstocks
    #   T6(61,IY,IS)=PPFIN(11,IY)
    z[61] = dfd["PPFIN"].loc[MNUMCR] * SCALPR2
    #   Ethane Feedstocks
    #   T6(123,IY,IS)=PETIN(11,IY)
    z[123] = dfd["PETIN"].loc[MNUMCR] * SCALPR2
    #   Motor Gasoline
    #   T6(10,IY,IS)=PMGIN(11,IY)
    z[10] = dfd["PMGIN"].loc[MNUMCR] * SCALPR2
    #   Distillate Fuel Oil
    #   T6(8,IY,IS)=PDSIN(11,IY)
    z[8] = dfd["AMPBLK/PDSIN"].loc[MNUMCR] * SCALPR2
    #   Residual Fuel Oil
    #   T6(7,IY,IS)=PRSIN(11,IY)
    z[7] = dfd["AMPBLK/PRSIN"].loc[MNUMCR] * SCALPR2
    #   Asphalt and Road Oil
    #   T6(62,IY,IS)=PASIN(11,IY)
    z[62] = dfd["PASIN"].loc[MNUMCR] * SCALPR2
    #   Natural Gas
    #   T6(5,IY,IS)=PGIIN(11,IY)
    z[5] = dfd["PGIIN"].loc[MNUMCR] * SCALPR2
    #   Hydrogen
    #   (new row)
    z[142] = dfd["PH2IN"].loc[MNUMCR] * SCALPR2
    #   Metallurgical Coal
    #   T6(11,IY,IS)=PMCIN(11,IY)
    z[11] = dfd["AMPBLK/PMCIN"].loc[MNUMCR] * SCALPR2
    #   Steam Coal
    #   T6(6,IY,IS)=PCLIN(11,IY)
    z[6] = dfd["AMPBLK/PCLIN"].loc[MNUMCR] * SCALPR2
    #z[60] = dfd["AMPBLK/PCLSN"].loc[MNUMCR] * SCALPR2
    #   Electricity	4
    #   T6(4,IY,IS)=PELIN(11,IY)
    z[4] = dfd["AMPBLK/PELIN"].loc[MNUMCR] * SCALPR2
    #   Primary
    #   T6(131,IY,IS)=PELINP(11,IY)
    z[131] = dfd["PELINP"].loc[MNUMCR] * SCALPR2
    #   Shift
    #   T6(132,IY,IS)=PELINS(11,IY)
    z[132] = dfd["PELINS"].loc[MNUMCR] * SCALPR2
    #   Miscellaneous
    #   T6(133,IY,IS)=PELINM(11,IY)
    z[133] = dfd["PELINM"].loc[MNUMCR] * SCALPR2
    #   Electrolyzer
    z[141] = dfd["AEUSPRC/PELINH2E"].loc[MNUMCR] * SCALPR2    

    #  (nominal dollars per million Btu)
    #   Propane
    #   T6(119,IY,IS)=T6(9,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[119] = z[9] / SCALPR2 * MC_JPGDP
    #   Motor Gasoline
    #   T6(120,IY,IS)=T6(10,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[120] = z[10] / SCALPR2 * MC_JPGDP
    #   Distillate Fuel Oil
    #   T6(121,IY,IS)=T6(8,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[121] = z[8] / SCALPR2 * MC_JPGDP
    #   Residual Fuel Oil
    #   T6(122,IY,IS)=T6(7,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[122] = z[7] / SCALPR2 * MC_JPGDP
    #   Asphalt and Road Oil
    #   T6(124,IY,IS)=T6(62,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[124] = z[62] / SCALPR2 * MC_JPGDP
    #   Natural Gas
    #   T6(125,IY,IS)=T6(5,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[125] = z[5] / SCALPR2 * MC_JPGDP
    #   Hydrogen
    #   (new row)
    z[143] = z[142] / SCALPR2 * MC_JPGDP
    #   Metallurgical Coal
    #   T6(127,IY,IS)=T6(11,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[127] = z[11] / SCALPR2 * MC_JPGDP
    #   Steam Coal	128	T6(128,IY,IS)=T6(6,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[128] = z[6] / SCALPR2 * MC_JPGDP
    #   Electricity	130	T6(130,IY,IS)=T6(4,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[130] = z[4] / SCALPR2 * MC_JPGDP

    # Energy Consumption 1/ (quadrillion Btu)
    # Industrial Consumption Excluding Refining
    #   Propane Heat and Power		T6(70,IY,IS)=QLGIN(11,IY)-REFCON(IXLG,5,IY)/1000.-INQLGPF(11,IY)/1000.
    z[70] = (
        dfd["QLGIN"].loc[11]
        - dfd["INDREP/REFCON"].loc[IXLG].loc[5]
        - dfd["INQLGPF"].loc[MNUMCR]
    ) * TRIL_TO_QUAD
    #   Hydrocarbon Gas Liquid Feedstocks 2/	71	T6(71,IY,IS)=INQLGPF(11,IY)/1000.
    z[71] = dfd["INQLGPF"].loc[MNUMCR] * TRIL_TO_QUAD
    #      Ethane		T6(137,IY,IS)=QETINPF(MNUMCR,IY)
    z[137] = dfd["QETINPF"].loc[MNUMCR] * TRIL_TO_QUAD
    #      Propane		T6(138,IY,IS)=QPRINPF(MNUMCR,IY)
    z[138] = dfd["QPRINPF"].loc[MNUMCR] * TRIL_TO_QUAD
    #      Propylene		T6(139,IY,IS)=QPROLENERF(MNUMCR,IY)
    z[139] = dfd["QPROLENERF"].loc[MNUMCR] * TRIL_TO_QUAD
    #      Butane 3/	1	T6(140,IY,IS)=QBUINPF(MNUMCR,IY)+QISINPF(MNUMCR,IY)
    z[140] = (dfd["QBUINPF"].loc[MNUMCR] + dfd["QISINPF"].loc[MNUMCR]) * TRIL_TO_QUAD
    #      Natural Gasoline	136	T6(136,IY,IS)=QPPINPF(MNUMCR,IY)
    z[136] = dfd["QPPINPF"].loc[MNUMCR] * TRIL_TO_QUAD
    #   Motor Gasoline	74	T6(74,IY,IS)=QMGIN(11,IY)
    z[74] = dfd["QMGIN"].loc[MNUMCR] * TRIL_TO_QUAD
    #   Distillate Fuel Oil	69	T6(69,IY,IS)=QDSIN(11,IY)-REFCON(IXDS,5,IY)/1000.
    z[69] = (
        dfd["QDSIN"].loc[MNUMCR] - dfd["INDREP/REFCON"].loc[IXDS].loc[5]
    ) * TRIL_TO_QUAD
    #   Residual Fuel Oil	73	T6(73,IY,IS)=QRSIN(11,IY)-REFCON(IXRF,5,IY)/1000.
    z[73] = (
        dfd["QRSIN"].loc[MNUMCR] - dfd["INDREP/REFCON"].loc[IXRF].loc[5]
    ) * TRIL_TO_QUAD
    #   Petrochemical Feedstocks	72	T6(72,IY,IS)=QPFIN(11,IY)
    z[72] = dfd["QPFIN"].loc[MNUMCR] * TRIL_TO_QUAD
    #   Petroleum Coke	75	T6(75,IY,IS)=QPCIN(11,IY)-REFCON(IXPC,5,IY)/1000.
    z[75] = (
        dfd["QPCIN"].loc[MNUMCR] - dfd["INDREP/REFCON"].loc[IXPC].loc[5]
    ) * TRIL_TO_QUAD
    #   Asphalt and Road Oil		T6(77,IY,IS)=QASIN(11,IY)
    z[77] = dfd["QASIN"].loc[MNUMCR] * TRIL_TO_QUAD
    #   Miscellaneous Petroleum 4/		T6(78,IY,IS)=QOTIN(11,IY)+QKSIN(11,IY)-REFCON(IXOP,5,IY)/1000.
    z[78] = (
        dfd["QOTIN"].loc[MNUMCR]
        + dfd["QKSIN"].loc[MNUMCR]
        - dfd["INDREP/REFCON"].loc[IXOP].loc[5]
    ) * TRIL_TO_QUAD
    #     Petroleum and Other Liquids Subtotal		T6(79,IY,IS)=FSUM(T6(69,IY,IS),10)
    z[79] = (
        z[69] + z[70] + z[71] + z[72] + z[73] + z[74] + z[75] + z[77] + z[78]
    )
    #   Natural Gas Heat and Power		T6(80,IY,IS)=QNGIN(11,IY)-REFCON(IXNG,5,IY)/1000.-INQNGPF(11,IY)/1000.
    z[80] = (
        dfd["QNGIN"].loc[MNUMCR]
        - dfd["INDREP/REFCON"].loc[IXNG].loc[5]
        - dfd["INQNGPF"].loc[MNUMCR]
        - dfd["QNGHM"].loc[MNUMCR]
    ) * TRIL_TO_QUAD
    #   Natural Gas Feedstocks		T6(81,IY,IS)=INQNGPF(11,IY)/1000.
    z[81] = dfd["INQNGPF"].loc[MNUMCR] * TRIL_TO_QUAD
    #   Lease and Plant Fuel 5/		T6(39,IY,IS)=QLPIN(11,IY)
    z[39] = dfd["QLPIN"].loc[MNUMCR] * TRIL_TO_QUAD
    #     Natural Gas Subtotal		T6(83,IY,IS)=FSUM(T6(80,IY,IS),3)+T6(39,IY,IS)
    # note T6(82,iy,is) is not assigned in ftab
    z[83] = z[80] + z[81] + z[39]
    #   Hydrogen Consumed as Feedstocks
    z[144] = (dfd["QH2INPF"].loc[MNUMCR] - dfd["QH2RF"].loc[MNUMCR])* TRIL_TO_QUAD
    #   Metallurgical Coal and Coke 7/	84	T6(84,IY,IS)=QMCIN(11,IY)+QCIIN(11,IY)
    z[84] = (dfd["QMCIN"].loc[MNUMCR] + dfd["QCIIN"].loc[MNUMCR]) * TRIL_TO_QUAD
    # Other Industrial Coal
    # T6(85,IY,IS)=QCLIN(11,IY)-REFCON(IXCL,5,IY)/1000.
    z[85] = (
        dfd["QCLIN"].loc[MNUMCR]
        - dfd["INDREP/REFCON"].loc[IXCL].loc[5]
    ) * TRIL_TO_QUAD
    #     Coal Subtotal	87	T6(87,IY,IS)=FSUM(T6(84,IY,IS),2)
    z[87] = z[84] + z[85]
    #   Renewables 8/		T6(88,IY,IS)=QTRIN(11,IY)-REFCON(IXRN,5,IY)/1000.
    z[88] = (
        dfd["QTRIN"].loc[MNUMCR] - dfd["INDREP/REFCON"].loc[IXRN].loc[5]
    ) * TRIL_TO_QUAD
    #   Purchased Electricity		T6(89,IY,IS)=QELIN(11,IY)-REFCON(IXEL,5,IY)/1000.-QELHM(11,IY)
    z[89] = (
        dfd["QELIN"].loc[MNUMCR] - dfd["INDREP/REFCON"].loc[IXEL].loc[5] - dfd["QELHM"].loc[MNUMCR]
    ) * TRIL_TO_QUAD
    #     Delivered Energy		T6(90,IY,IS)=T6(79,IY,IS)+T6(83,IY,IS)+FSUM(T6(87,IY,IS),3)
    z[90] = z[79] + z[83] + z[87] + z[88] + z[89] + z[144]
    #   Electricity-Related Losses	91	T6(91,IY,IS)=(QELIN(11,IY)-REFCON(IXEL,5,IY)/1000.-QELHM(11,IY))/QELAS(11,IY)*ELECLOSS
    z[91] = (
        (dfd["QELIN"].loc[MNUMCR] - dfd["INDREP/REFCON"].loc[IXEL].loc[5] - dfd["QELHM"].loc[MNUMCR])
        / dfd["QELAS"].loc[MNUMCR]
        * ELECLOSS
    ) * TRIL_TO_QUAD
    #   Hydrogen-Related Losses c/
    z[145] = (
        (dfd["QH2INPF"].loc[MNUMCR] - dfd["QH2RF"].loc[MNUMCR])
        / TOT_H2_CONS
        * H2LOSS
    ) * TRIL_TO_QUAD
    #     Total	92	T6(92,IY,IS)=FSUM(T6(90,IY,IS),2)
    z[92] = z[90] + z[91] + z[145]

    # Refining Consumption
    #   Liquefied Petroleum Gas Heat and Power 2/	94	T6(94,IY,IS)=REFCON(IXLG,5,IY)/1000.
    z[94] = dfd["INDREP/REFCON"].loc[IXLG].loc[5] * TRIL_TO_QUAD
    #   Distillate Fuel Oil		T6(93,IY,IS)=REFCON(IXDS,5,IY)/1000.
    z[93] = dfd["INDREP/REFCON"].loc[IXDS].loc[5] * TRIL_TO_QUAD
    #   Residual Fuel Oil		T6(97,IY,IS)=REFCON(IXRF,5,IY)/1000.
    z[97] = dfd["INDREP/REFCON"].loc[IXRF].loc[5] * TRIL_TO_QUAD
    #   Petroleum Coke		T6(99,IY,IS)=REFCON(IXPC,5,IY)/1000.
    z[99] = dfd["INDREP/REFCON"].loc[IXPC].loc[5] * TRIL_TO_QUAD
    #   Still Gas		T6(108,IY,IS)=REFCON(IXSG,5,IY)/1000.
    z[108] = dfd["INDREP/REFCON"].loc[IXSG].loc[5] * TRIL_TO_QUAD
    #   Miscellaneous Petroleum 4/	T6(102,IY,IS)=REFCON(IXOP,5,IY)/1000.
    z[102] = dfd["INDREP/REFCON"].loc[IXOP].loc[5] * TRIL_TO_QUAD
    #     Petroleum and Other Liquids Subtotal	103	T6(103,IY,IS)=FSUM(T6(93,IY,IS),10)
    z[103] = z[93] + z[94] + z[97] + z[99] + z[102] + z[108]
    #   Natural Gas Heat and Power	T6(104,IY,IS)=REFCON(IXNG,5,IY)/1000.-T6(106,IY,IS)-T6(105,IY,IS)
    z[104] = dfd["INDREP/REFCON"].loc[IXNG].loc[5] * TRIL_TO_QUAD
    #   Hydrogen Consumed as Feedstocks b/
    z[146] = dfd["INDREP/REFCON"].loc[IXHF].loc[5] * TRIL_TO_QUAD
    #   Steam Coal	109	T6(109,IY,IS)=QCLETH(IY,11)/1000.
    z[109] = dfd["QCLETH"].loc[MNUMCR] * TRIL_TO_QUAD

    # Note: A lot of variables read from the restart are scalar variables shape = (1,1)
    # Solution: Create a blank df of 61 columns, and assign the scalar  to each column
    # col_count = len(table_spec['years'])
    ## columns = [i for i in range(1, col_count+1)]
    # df = pd.DataFrame([[0] * col_count], columns=columns)

    # df[columns] = dfd['CFCORN'].iloc[0]
    # z[112] = df.copy()

    #   Biofuels Heat and Coproducts
    # T6(112,IY,IS) = &
    # (CORNCD(3,11,IY) * CFCORN / 1000. -0.9751*(CRNETHCD(11,IY)+OTHETHCD(11,IY)) * 365. * CFPET - &
    # RFBIOBUTECD(MNUMCR,IY)*365*CFBIOBUTE(IY)) / 1000000. + &
    # sum(BIMQTYCD(1:4,11,IY))/1000000.*RDAYS*(CFVEGGIE(IY)-CFBIOD(IY)) + &
    # (sum(BTLFRAC(1:4,MNUMPR,IY)) * CFBTLLIQ(IY) + &
    # sum(CBTLFRAC(2,1:4,MNUMPR,IY)) * CFCBTLLIQ(2,IY) + &
    # UBAVOL(MNUMPR,IY) * 5.763)
    # IF (CONEFF(IY) .NE. 0.0) T6(112,IY,IS) = T6(112,IY,IS) + &
    # (0.9751*CLLETHCD(11,IY) * RDAYS * (CFBMQ(IY) * 42. / CONEFF(IY) - CFPET)) / 1000000.
    z[112] = (
        (
            dfd["CORNCD"].loc[3].loc[MNUMCR] * dfd["CFCORN"].iloc[0] / 1000
            - dfd["ETHANOL_PARAM_rwpre"]
            * (dfd["CRNETHCD"].loc[MNUMCR] + dfd["OTHETHCD"].loc[MNUMCR])
            * RDAYS
            * dfd["CFPET"].iloc[0]
            - dfd["RFBIOBUTECD"].loc[MNUMCR] * RDAYS * dfd["CFBIOBUTE"]
        )
        / 1000000
        + dfd["BIMQTYCD"].loc[1:4, MNUMCR, :].sum()
        / 1000000
        * RDAYS
        * (dfd["CFVEGGIE"] - dfd["CFBIOD"])
        - RDAYS
        / 1000000
        * (
            dfd["BTLFRAC"].loc[1:4, MNUMPR, :].sum() * dfd["CFBTLLIQ"]
            + dfd["CBTLFRAC"].loc[2, 1:4, MNUMPR, :].sum() * dfd["CFCBTLLIQ"].loc[2]
            + dfd["UBAVOL"].loc[MNUMPR] * MBCD_CNV
        )
        + (
            (
                dfd["ETHANOL_PARAM_rwpre"]
                * dfd["CLLETHCD"].loc[MNUMCR]
                * RDAYS
                * (dfd["CFBMQ"] * 42.0 / dfd["CONEFF"] - dfd["CFPET"].iloc[0])
            )
            / 1000000.0
        )
    )
    # z[112] =dfd["Biofuels Heat and Coproducts rwpre"]
    #   Purchased Electricity
    #   T6(113,IY,IS)=REFCON(IXEL,5,IY)/1000.
    z[113] = dfd["INDREP/REFCON"].loc[IXEL].loc[5] * TRIL_TO_QUAD
    #   Delivered Energy
    #   T6(114,IY,IS)=T6(103,IY,IS)+T6(107,IY,IS)+FSUM(T6(111,IY,IS),3)
    z[114] = z[103] + z[104] + z[109] + z[112] + z[113] +z[146]
    #   Electricity Related Losses
    #   T6(115,IY,IS)=REFCON(IXEL,5,IY)/1000./QELAS(11,IY)*ELECLOSS
    z[115] = (
        dfd["INDREP/REFCON"].loc[IXEL].loc[5] / dfd["QELAS"].loc[MNUMCR] * ELECLOSS
    ) * TRIL_TO_QUAD
    #   Hydrogen-Related Losses c/
    # (new row)
    z[147] = (
        dfd["INDREP/REFCON"].loc[IXHF].loc[5]
        / TOT_H2_CONS
        * H2LOSS
    ) * TRIL_TO_QUAD
    #   Total
    #   T6(116,IY,IS)=FSUM(T6(114,IY,IS),2)
    z[116] = z[114] + z[115] + z[147]
    
    # Hydrogen Production-Related Consumption (trillion Btu) cc/
    #   Natural Gas Heat and Power
    z[148] = dfd["QNGHMHP"].loc[MNUMCR] * TRIL_TO_QUAD
    #   Natural Gas Feedstocks
    z[149] = dfd["QNGHMPF"].loc[MNUMCR] * TRIL_TO_QUAD
    #   Natural Gas Subtotal
    z[150] = z[148] + z[149]
    #   Purchased Electricity
    z[151] = dfd["QELHM"].loc[MNUMCR] * TRIL_TO_QUAD
    #   Delivered Energy
    z[152] = z[150] + z[151]
    #   Electricity-Related Losses
    z[153] = (
        dfd["QELHM"].loc[MNUMCR] / dfd["QELAS"].loc[MNUMCR] * ELECLOSS
    ) * TRIL_TO_QUAD
    # Total
    z[154] = z[152] + z[153]
    
    #  Industrial Sector End-Use Consumption Excluding Hydrogen Production aa/
    #   Liquefied Petroleum Gas Heat and Power 2/
    #   T6(18,IY,IS)=QLGIN(11,IY)-INQLGPF(11,IY)/1000.
    z[18] = (dfd["QLGIN"].loc[MNUMCR] - dfd["INQLGPF"].loc[MNUMCR]) * TRIL_TO_QUAD
    #   Hydrocarbon Gas Liquid Feedstocks 2/
    #   T6(63,IY,IS)=INQLGPF(11,IY)/1000.
    z[63] = dfd["INQLGPF"].loc[MNUMCR] * TRIL_TO_QUAD
    #   Motor Gasoline
    #   T6(50,IY,IS)=QMGIN(11,IY)
    z[50] = dfd["QMGIN"].loc[MNUMCR] * TRIL_TO_QUAD
    #   Distillate Fuel Oil
    #   T6(17,IY,IS)=QDSIN(11,IY)
    z[17] = dfd["QDSIN"].loc[MNUMCR] * TRIL_TO_QUAD
    #   Residual Fuel Oil
    #   T6(16,IY,IS)=QRSIN(11,IY)
    z[16] = dfd["QRSIN"].loc[MNUMCR] * TRIL_TO_QUAD
    #   Petrochemical Feedstocks
    #   T6(19,IY,IS)=QPFIN(11,IY)
    z[19] = dfd["QPFIN"].loc[MNUMCR] * TRIL_TO_QUAD
    #   Petroleum Coke
    #   T6(51,IY,IS)=QPCIN(11,IY)
    z[51] = dfd["QPCIN"].loc[MNUMCR] * TRIL_TO_QUAD
    #   Asphalt and Road Oil
    #   T6(53,IY,IS)=QASIN(11,IY)
    z[53] = dfd["QASIN"].loc[MNUMCR] * TRIL_TO_QUAD
    #   Still Gas
    #   T6(52,IY,IS)=QSGIN(11,IY)
    z[52] = dfd["QSGIN"].loc[MNUMCR] * TRIL_TO_QUAD
    #   Miscellaneous Petroleum 4/
    #   T6(20,IY,IS)=QOTIN(11,IY)+QKSIN(11,IY)
    z[20] = (dfd["QOTIN"].loc[MNUMCR] + dfd["QKSIN"].loc[MNUMCR]) * TRIL_TO_QUAD
    #   Petroleum and Other Liquids Subtotal
    #   T6(42,IY,IS)=FSUM(T6(16,IY,IS),5)+FSUM(T6(50,IY,IS),4)+    T6(63,IY,IS)
    z[42] = (
        z[16] + z[17] + z[18] + z[19] + z[20] + z[50] + z[51] + z[52] + z[53] + z[63]
    )
    #   Natural Gas Heat and Power
    #   T6(38,IY,IS)=QNGIN(11,IY)-INQNGPF(11,IY)/1000.
    z[38] = (
        dfd["QNGIN"].loc[MNUMCR]
        - dfd["INQNGPF"].loc[MNUMCR]
        - dfd["QNGHM"].loc[MNUMCR]
    ) * TRIL_TO_QUAD
    #   Natural Gas Feedstocks
    #   T6(64,IY,IS)=INQNGPF(11,IY)/1000.
    z[64] = (dfd["INQNGPF"].loc[MNUMCR]) * TRIL_TO_QUAD
    #   Lease and Plant Fuel 5/	39	T6(39,IY,IS)=QLPIN(11,IY)
    z[39] = dfd["QLPIN"].loc[MNUMCR] * TRIL_TO_QUAD
    #   Natural Gas Subtotal
    #   T6(13,IY,IS)=FSUM(T6(38,IY,IS),2)+T6(64,IY,IS)
    z[13] = z[38] + z[39] + z[64]
    #   Hydrogen Consumed as Feedstocks b/
    z[155] = (dfd["QH2INPF"].loc[MNUMCR]) * TRIL_TO_QUAD
    #   Metallurgical Coal and Coke 7/
    #   T6(15,IY,IS)=QMCIN(11,IY)+QCIIN(11,IY)
    z[15] = (dfd["QMCIN"].loc[MNUMCR] + dfd["QCIIN"].loc[MNUMCR]) * TRIL_TO_QUAD
    #   Steam Coal
    #   T6(14,IY,IS)= QCLIN(11,IY)
    z[14] = (dfd["QCLIN"].loc[MNUMCR]) * TRIL_TO_QUAD
    #   Coal Subtotal
    #   T6(43,IY,IS)=FSUM(T6(14,IY,IS),2)
    z[43] = z[14] + z[15]
    #   Renewables 8/
    #   T6(21,IY,IS)=QTRIN(11,IY)-REFCON(IXRN,5,IY)/1000.
    z[21] = (
        dfd["QTRIN"].loc[MNUMCR] - dfd["INDREP/REFCON"].loc[IXRN].loc[5]
    ) * TRIL_TO_QUAD
    #   Purchased Electricity
    #   T6(12,IY,IS)=QELIN(11,IY)
    z[12] = (dfd["QELIN"].loc[MNUMCR] - dfd["QELHM"].loc[MNUMCR]) * TRIL_TO_QUAD
    #     Delivered Energy
    #   T6(22,IY,IS)=FSUM(T6(42,IY,IS),2)+FSUM(T6(12,IY,IS),2)+T6(21,IY,IS)+T6(112,IY,IS)
    z[22] = z[42] + z[43] + z[12] + z[13] + z[21] + z[112] + z[155]
    #   Electricity Related Losses
    #   T6(23,IY,IS)=QELIN(11,IY)/QELAS(11,IY)*ELECLOSS
    z[23] = (
        (dfd["QELIN"].loc[MNUMCR] - dfd["QELHM"].loc[MNUMCR])
        / dfd["QELAS"].loc[MNUMCR] * ELECLOSS
    ) * TRIL_TO_QUAD
    #   Hydrogen-Related Losses c/
    z[156] = (
        (dfd["QH2IN"].loc[MNUMCR])
        / TOT_H2_CONS
        * H2LOSS
    ) * TRIL_TO_QUAD
    #   Total
    #   T6(24,IY,IS)=FSUM(T6(22,IY,IS),2)
    z[24] = z[22] + z[23] + z[156]
    
    #  Total Industrial Sector Primary Consumption e/
    # Most of this section repeats the z[] values used in the previous section. Only lines where there is a difference get defined here.
    #   Liquefied Petroleum Gas Heat and Power 2/
    #   Hydrocarbon Gas Liquid Feedstocks 2/
    #   Motor Gasoline
    #   Distillate Fuel Oil
    #   Residual Fuel Oil
    #   Petrochemical Feedstocks
    #   Petroleum Coke
    #   Asphalt and Road Oil
    #   Still Gas
    #   Miscellaneous Petroleum 4/
    #       Petroleum and Other Liquids Subtotal
    #   Natural Gas Heat and Power
    z[157] = (
        dfd["QNGIN"].loc[MNUMCR]
        - dfd["INQNGPF"].loc[MNUMCR]
        - dfd["QNGHMPF"].loc[MNUMCR]
    ) * TRIL_TO_QUAD
    #   Natural Gas Feedstocks
    z[158] = (dfd["INQNGPF"].loc[MNUMCR] + dfd["QNGHMPF"].loc[MNUMCR]) * TRIL_TO_QUAD
    #   Lease and Plant Fuel 5/
    #       Natural Gas Subtotal
    z[159] = z[157] + z[158] + z[39]
    #   Metallurgical Coal and Coke 7/
    #   Steam Coal
    #       Coal Subtotal
    #   Renewables 8/
    #   Purchased Electricity
    z[160] = (dfd["QELIN"].loc[MNUMCR]) * TRIL_TO_QUAD
    #     Delivered Energy
    z[161] = z[42] + z[43] + z[160] + z[159] + z[21] + z[112]
    #   Electricity Related Losses
    z[162] = (
        (dfd["QELIN"].loc[MNUMCR])
        / dfd["QELAS"].loc[MNUMCR] * ELECLOSS
    ) * TRIL_TO_QUAD
    #   Total
    z[163] = z[161] + z[162]
    
    
    # Energy Consumption per dollar of Shipments 1/
    # (thousand Btu per m==m dollar)
    #   Liquefied Petroleum Gas Heat and Power 2/	T6(31,IY,IS)=(QLGIN(11,IY)*1000.-INQLGPF(11,IY))/T6(3,IY,IS)
    z[31] = (dfd["QLGIN"].loc[MNUMCR] - dfd["INQLGPF"].loc[MNUMCR]) / z[3]
    #   Hydrocarbon Gas Liquid Feedstocks 2/	    T6(65,IY,IS)=INQLGPF(11,IY)/T6(3,IY,IS)
    z[65] = dfd["INQLGPF"].loc[MNUMCR] / z[3]
    #   Motor Gasoline	54	T6(54,IY,IS)=QMGIN(11,IY)*1000./T6(3,IY,IS)
    z[54] = dfd["QMGIN"].loc[MNUMCR] / z[3]
    #   Distillate Fuel Oil	30	T6(30,IY,IS)=QDSIN(11,IY)*1000./T6(3,IY,IS)
    z[30] = dfd["QDSIN"].loc[MNUMCR] / z[3]
    #   Residual Fuel Oil	29	T6(29,IY,IS)=QRLIN(11,IY)*1000./T6(3,IY,IS)
    z[29] = dfd["QRLIN"].loc[MNUMCR] / z[3]
    #   Petrochemical Feedstocks	32	T6(32,IY,IS)=QPFIN(11,IY)*1000./T6(3,IY,IS)
    z[32] = dfd["QPFIN"].loc[MNUMCR] / z[3]
    #   Petroleum Coke	55	T6(55,IY,IS)=QPCIN(11,IY)*1000./T6(3,IY,IS)
    z[55] = dfd["QPCIN"].loc[MNUMCR] / z[3]
    #   Asphalt and Road Oil	57	T6(57,IY,IS)=QASIN(11,IY)*1000./T6(3,IY,IS)
    z[57] = dfd["QASIN"].loc[MNUMCR] / z[3]
    #   Still Gas	56	T6(56,IY,IS)=QSGIN(11,IY)*1000./T6(3,IY,IS)
    z[56] = dfd["QSGIN"].loc[MNUMCR] / z[3]
    #   Miscellaneous Petroleum 4/
    #   T6(33,IY,IS)=(QOTIN(11,IY)+QKSIN(11,IY))*1000./T6(3,IY,IS)
    z[33] = (dfd["QOTIN"].loc[MNUMCR] + dfd["QKSIN"].loc[MNUMCR]) / z[3]
    #     Petroleum and Other Liquids Subtotal	44	T6(44,IY,IS)=FSUM(T6(29,IY,IS),5)+FSUM(T6(54,IY,IS),4)+T6(65,IY,IS)
    z[44] = (
        z[29] + z[30] + z[31] + z[32] + z[33] + z[54] + z[55] + z[56] + z[57] + z[65]
    )
    #   Natural Gas Heat and Power
    #   T6(40,IY,IS)=(QNGIN(11,IY)*1000.-INQNGPF(11,IY))/T6(3,IY,IS)
    z[40] = (
        dfd["QNGIN"].loc[MNUMCR]
        - dfd["INQNGPF"].loc[MNUMCR]
        - dfd["QNGHMPF"].loc[MNUMCR]
    ) / z[3]
    #    Natural Gas Feedstock--------------------------
    #   T6(66,IY,IS)=(INQNGPF(11,IY))/T6(3,IY,IS)
    z[66] = (dfd["INQNGPF"].loc[MNUMCR] + dfd["QNGHMPF"].loc[MNUMCR]) / z[3]
    #    Lease and Plant Fuel 5/------------------------
    #   T6(41,IY,IS)=(QLPIN(11,IY))*1000./T6(3,IY,IS)
    z[41] = dfd["QLPIN"].loc[MNUMCR] / z[3]
    #   Natural Gas Subtotal-------------------------
    #   T6(26,IY,IS)=FSUM(T6(40,IY,IS),2)+T6(66,IY,IS)+T6(117,IY,IS)+T6(135,IY,IS)
    #   from code: T6(26,IY,IS)= FSUM(T6(40,IY,IS),2) + T6(66,IY,IS)
    z[26] = z[40] + z[41] + z[66]
    #   Metallurgical Coal and Coke 6/-----------------
    #   T6(28,IY,IS)=(QMCIN(11,IY)+QCIIN(11,IY))*1000./T6(3,IY,IS)
    z[28] = (dfd["QMCIN"].loc[MNUMCR] + dfd["QCIIN"].loc[MNUMCR]) / z[3]
    #   Other Industrial Coal--------------------------
    #   T6(27,IY,IS)=(QCLIN(11,IY))*1000./T6(3,IY,IS)
    z[27] = (
        dfd["QCLIN"].loc[MNUMCR]
    ) / z[3]
    #   Coal Subtotal--------------------------------
    #   T6(45,IY,IS)=FSUM(T6(27,IY,IS),2)
    z[45] = z[27] + z[28]
    ##   Biofuels Heat and Coproducts
    #  T6(118,IY,IS) = 1000. * ((CORNCD(3,11,IY) * CFCORN / 1000. -0.9751*(CRNETHCD(11,IY)+OTHETHCD(11,IY)) * 365. * CFPET - &
    # RFBIOBUTECD(MNUMCR,IY)*365*CFBIOBUTE(IY))/1000000. + &
    # - RDAYS / 1000000. * &
    # (sum(BTLFRAC(1:4,MNUMPR,IY)) * CFBTLLIQ(IY) + &
    # sum(CBTLFRAC(2,1:4,MNUMPR,IY)) * CFCBTLLIQ(2,IY) + &
    # UBAVOL(MNUMPR,IY) * 5.763) + &
    # sum(BIMQTYCD(1:4,11,IY))/1000000.*RDAYS*(CFVEGGIE(IY)-CFBIOD(IY))) / T6(3,IY,IS)
    # IF (CONEFF(IY) .NE. 0.0) T6(118,IY,IS) = T6(118,IY,IS) + 1000. / T6(3,IY,IS) * &
    # ((0.9751*CLLETHCD(11,IY) * RDAYS * (CFBMQ(IY) * 42. / CONEFF(IY) - CFPET)) / 1000000.)
    z[118] = 1000 * (
        (
            dfd["CORNCD"].loc[3].loc[MNUMCR] * dfd["CFCORN"].iloc[0] / 1000
            - dfd["ETHANOL_PARAM_rwpre"]
            * (dfd["CRNETHCD"].loc[MNUMCR] + dfd["OTHETHCD"].loc[MNUMCR])
            * RDAYS
            * dfd["CFPET"].iloc[0]
            - dfd["RFBIOBUTECD"].loc[MNUMCR] * 365 * dfd["CFBIOBUTE"]
        )
        / 1000000
        - RDAYS
        / 1000000
        * (
            dfd["BTLFRAC"].loc[(1, 2, 3, 4), MNUMPR, :].sum() * dfd["CFBTLLIQ"]
            + dfd["CBTLFRAC"].loc[2, (1, 2, 3, 4), MNUMPR, :].sum()
            * dfd["CFCBTLLIQ"].loc[2]
            + dfd["UBAVOL"].loc[MNUMPR] * MBCD_CNV
        )
        + dfd["BIMQTYCD"].loc[(1, 2, 3, 4), MNUMCR, :].sum()
        / 1000000
        * RDAYS
        * (dfd["CFVEGGIE"] - dfd["CFBIOD"])
    ) / z[3] + (
        1000.0
        / z[3]
        * (
            (
                dfd["ETHANOL_PARAM_rwpre"]
                * dfd["CLLETHCD"].loc[MNUMCR]
                * RDAYS
                * (dfd["CFBMQ"] * 42.0 / dfd["CONEFF"] - dfd["CFPET"].iloc[0])
            )
            / 1000000.0
        )
    )

    #    Renewables 7/----------------------------------
    #   T6(34,IY,IS)=(QTRIN(11,IY)-REFCON(IXRN,5,IY)/1000.)*1000./T6(3,IY,IS)
    z[34] = (dfd["QTRIN"].loc[MNUMCR] - dfd["INDREP/REFCON"].loc[IXRN].loc[5]) / z[3]
    #   Purchased Electricity--------------------------
    #   T6(25,IY,IS)=QELIN(11,IY)*1000./T6(3,IY,IS)
    z[25] = dfd["QELIN"].loc[MNUMCR] / z[3]
    #   Delivered Energy-----------------------------
    #   T6(35,IY,IS)=FSUM(T6(44,IY,IS),2)+FSUM(T6(25,IY,IS),2)+T6(34,IY,IS)+T6(118,IY,IS)
    z[35] = z[44] + z[45] + z[25] + z[26] + z[34] + z[118]
    #   Electricity Related Losses---------------------
    #   T6(36,IY,IS)=QELIN(11,IY)/QELAS(11,IY)*ELECLOSS*1000./T6(3,IY,IS)
    z[36] = dfd["QELIN"].loc[MNUMCR] / dfd["QELAS"].loc[MNUMCR] * ELECLOSS / z[3]
    #   Total----------------------------------------
    #   T6(37,IY,IS)=FSUM(T6(35,IY,IS),2)
    z[37] = z[35] + z[36]
    #
    #   Total Industrial Combined Heat and Power 1/-------
    #   Capacity (gigawatts) 8/-------------------------
    #   T6(48,IY,IS)=.001*(CGREFCAP(11,IY,1)+CGREFCAP(11,IY,2)+CGREFCAP(11,IY,3)+CGREFCAP(11,IY,6)+CGREFCAP(11,IY,7)+CGREFCAP(11,IY,9)+CGOGSCAP(11,IY,1)+CGOGSCAP(11,IY,2)+\
    #                 CGOGSCAP(11,IY,3)+CGOGSCAP(11,IY,4)+CGINDLCAP(11,IY,1)+CGINDLCAP(11,IY,2)+CGINDLCAP(11,IY,3)+CGINDLCAP(11,IY,6)+CGINDLCAP(11,IY,7)+CGINDLCAP(11,IY,9)+CGINDLCAP(11,IY,10))
    z[48] = 0.001 * (
        dfd["CGREFCAP"].loc[11].loc[1]
        + dfd["CGREFCAP"].loc[11].loc[2]
        + dfd["CGREFCAP"].loc[11].loc[3]
        + dfd["CGREFCAP"].loc[11].loc[6]
        + dfd["CGREFCAP"].loc[11].loc[7]
        + dfd["CGREFCAP"].loc[11].loc[9]
        + dfd["CGOGSCAP"].loc[11].loc[1]
        + dfd["CGOGSCAP"].loc[11].loc[2]
        + dfd["CGOGSCAP"].loc[11].loc[3]
        + dfd["CGOGSCAP"].loc[11].loc[4]
        + dfd["CGINDLCAP"].loc[11].loc[1]
        + dfd["CGINDLCAP"].loc[11].loc[2]
        + dfd["CGINDLCAP"].loc[11].loc[3]
        + dfd["CGINDLCAP"].loc[11].loc[6]
        + dfd["CGINDLCAP"].loc[11].loc[7]
        + dfd["CGINDLCAP"].loc[11].loc[9]
        + dfd["CGINDLCAP"].loc[11].loc[10]
    )
    #   Generation (billion kilowatthours)--------------
    #   T6(49,IY,IS)=.001*(CGREFGEN(11,IY,1,1)+CGREFGEN(11,IY,1,2)+CGREFGEN(11,IY,2,1)+CGREFGEN(11,IY,2,2)+CGREFGEN(11,IY,3,1)+CGREFGEN(11,IY,3,2)+CGREFGEN(11,IY,6,1)+CGREFGEN(11,IY,6,2)+
    #                CGREFGEN(11,IY,7,1)+CGREFGEN(11,IY,7,2)+CGREFGEN(11,IY,9,1)+CGREFGEN(11,IY,9,2)+CGOGSGEN(11,IY,1,1)+CGOGSGEN(11,IY,1,2)+CGOGSGEN(11,IY,2,1)+CGOGSGEN(11,IY,2,2)+CGOGSGEN(11,IY,3,1)+
    #                CGOGSGEN(11,IY,3,2)+CGOGSGEN(11,IY,4,1)+CGOGSGEN(11,IY,4,2)+CGINDLGEN(11,IY,1,1)+CGINDLGEN(11,IY,1,2)+CGINDLGEN(11,IY,2,1)+CGINDLGEN(11,IY,2,2)+CGINDLGEN(11,IY,3,1)+
    #                CGINDLGEN(11,IY,3,2)+CGINDLGEN(11,IY,6,1)+CGINDLGEN(11,IY,6,2)+CGINDLGEN(11,IY,7,1)+CGINDLGEN(11,IY,7,2)+CGINDLGEN(11,IY,9,1)+CGINDLGEN(11,IY,9,2)+CGINDLGEN(11,IY,10,1)+CGINDLGEN(11,IY,10,2))
    z[49] = 0.001 * (
        dfd["CGREFGEN"].loc[11].loc[1].loc[1]
        + dfd["CGREFGEN"].loc[11].loc[1].loc[2]
        + dfd["CGREFGEN"].loc[11].loc[2].loc[1]
        + dfd["CGREFGEN"].loc[11].loc[2].loc[2]
        + dfd["CGREFGEN"].loc[11].loc[3].loc[1]
        + dfd["CGREFGEN"].loc[11].loc[3].loc[2]
        + dfd["CGREFGEN"].loc[11].loc[6].loc[1]
        + dfd["CGREFGEN"].loc[11].loc[6].loc[2]
        + dfd["CGREFGEN"].loc[11].loc[7].loc[1]
        + dfd["CGREFGEN"].loc[11].loc[7].loc[2]
        + dfd["CGREFGEN"].loc[11].loc[9].loc[1]
        + dfd["CGREFGEN"].loc[11].loc[9].loc[2]
        + dfd["CGOGSGEN"].loc[11].loc[1].loc[1]
        + dfd["CGOGSGEN"].loc[11].loc[1].loc[2]
        + dfd["CGOGSGEN"].loc[11].loc[2].loc[1]
        + dfd["CGOGSGEN"].loc[11].loc[2].loc[2]
        + dfd["CGOGSGEN"].loc[11].loc[3].loc[1]
        + dfd["CGOGSGEN"].loc[11].loc[3].loc[2]
        + dfd["CGOGSGEN"].loc[11].loc[4].loc[1]
        + dfd["CGOGSGEN"].loc[11].loc[4].loc[2]
        + dfd["CGINDLGEN"].loc[11].loc[1].loc[1]
        + dfd["CGINDLGEN"].loc[11].loc[1].loc[2]
        + dfd["CGINDLGEN"].loc[11].loc[2].loc[1]
        + dfd["CGINDLGEN"].loc[11].loc[2].loc[2]
        + dfd["CGINDLGEN"].loc[11].loc[3].loc[1]
        + dfd["CGINDLGEN"].loc[11].loc[3].loc[2]
        + dfd["CGINDLGEN"].loc[11].loc[6].loc[1]
        + dfd["CGINDLGEN"].loc[11].loc[6].loc[2]
        + dfd["CGINDLGEN"].loc[11].loc[7].loc[1]
        + dfd["CGINDLGEN"].loc[11].loc[7].loc[2]
        + dfd["CGINDLGEN"].loc[11].loc[9].loc[1]
        + dfd["CGINDLGEN"].loc[11].loc[9].loc[2]
        + dfd["CGINDLGEN"].loc[11].loc[10].loc[1]
        + dfd["CGINDLGEN"].loc[11].loc[10].loc[2]
    )
    #   Bonus Rows - Internal Only:-----------------------
    #   --------------------------------------------------
    #   On-site Industrial Combined Heat and Power--------
    #   Capacity (gigawatts) 9/-------------------------
    #   T6(46,IY,IS)=.001*(CGINDLCAP(11,IY,1)+CGINDLCAP(11,IY,2)+CGINDLCAP(11,IY,3)+CGINDLCAP(11,IY,6)+CGINDLCAP(11,IY,7)+CGINDLCAP(11,IY,9)+CGINDLCAP(11,IY,10))
    z[46] = 0.001 * (
        dfd["CGINDLCAP"].loc[11].loc[1]
        + dfd["CGINDLCAP"].loc[11].loc[2]
        + dfd["CGINDLCAP"].loc[11].loc[3]
        + dfd["CGINDLCAP"].loc[11].loc[6]
        + dfd["CGINDLCAP"].loc[11].loc[7]
        + dfd["CGINDLCAP"].loc[11].loc[9]
        + dfd["CGINDLCAP"].loc[11].loc[10]
    )
    #   Generation (billion kwh)------------------------
    #   T6(47,IY,IS)=.001*(CGINDLGEN(11,IY,1,1)+CGINDLGEN(11,IY,1,2)+CGINDLGEN(11,IY,2,1)+CGINDLGEN(11,IY,2,2)+CGINDLGEN(11,IY,3,1)+CGINDLGEN(11,IY,3,2)+CGINDLGEN(11,IY,6,1)+CGINDLGEN(11,IY,6,2)+CGINDLGEN(11,IY,7,1)+CGINDLGEN(11,IY,7,2)+CGINDLGEN(11,IY,9,1)+CGINDLGEN(11,IY,9,2)+CGINDLGEN(11,IY,10,1)+CGINDLGEN(11,IY,10,2))
    z[47] = 0.001 * (
        dfd["CGINDLGEN"].loc[11].loc[1].loc[1]
        + dfd["CGINDLGEN"].loc[11].loc[1].loc[2]
        + dfd["CGINDLGEN"].loc[11].loc[2].loc[1]
        + dfd["CGINDLGEN"].loc[11].loc[2].loc[2]
        + dfd["CGINDLGEN"].loc[11].loc[3].loc[1]
        + dfd["CGINDLGEN"].loc[11].loc[3].loc[2]
        + dfd["CGINDLGEN"].loc[11].loc[6].loc[1]
        + dfd["CGINDLGEN"].loc[11].loc[6].loc[2]
        + dfd["CGINDLGEN"].loc[11].loc[7].loc[1]
        + dfd["CGINDLGEN"].loc[11].loc[7].loc[2]
        + dfd["CGINDLGEN"].loc[11].loc[9].loc[1]
        + dfd["CGINDLGEN"].loc[11].loc[9].loc[2]
        + dfd["CGINDLGEN"].loc[11].loc[10].loc[1]
        + dfd["CGINDLGEN"].loc[11].loc[10].loc[2]
    )

    return z
