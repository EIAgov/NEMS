# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: SZO
"""


def fill_table_base_001(dfd, table_spec, table_id):
    """Fill table Total Energy Supply, Disposition, and Price Summary.

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

        TODO: Hard-coded lines in ftab.f will be implemented next stage.
        At some point, we will delete the commented rows with Fortran formulas.

    """

    z = {}

    FYRPRC = int(table_spec["settings"]["year_index_real_dol_priceyr"])

    # Update MC_JPGDP to match year values; trim to first year - last year columns
    MC_JPGDP = dfd["MC_JPGDP"]
    lyr = table_spec["last_year"] + 1

    # Change 1-indexed to year-indexed, then trim to first year - last year
    fyr = lyr - len(MC_JPGDP)
    MC_JPGDP.index = range(fyr, lyr)
    MC_JPGDP = MC_JPGDP.loc[table_spec["first_year"] : lyr]
    dfd["MC_JPGDP"] = MC_JPGDP

    # Get inflation index of FYRPRC (2022)
    SCALPR2 = MC_JPGDP.loc[FYRPRC]  # 2022 = column 33

    MNUMOR = dfd["MNUMOR_rwpre"]
    MNUMPR = dfd["MNUMPR_rwpre"]
    MNUMLR = dfd["MNUMLR_rwpre"]
    MNUMCR = dfd["MNUMCR_rwpre"]
    RDAYS = dfd["RDAYS_rwpre"]
    ETHANOL_PARAM = dfd["ETHANOL_PARAM_rwpre"]
    GAL_PER_BBL = dfd["GAL_PER_BBL_rwpre"]

    # Production------------
    #    Crude Oil and Lease Condensate
    # formula_F = T1(1,IY,IS)=RFQTDCRD(MNUMOR+2,IY)*RDAYS*CFCRDDOM(IY)*.001
    z[1] = (
        dfd["RFQTDCRD"].loc[MNUMOR + 2] * dfd["RDAYS_rwpre"] * dfd["CFCRDDOM"] * 0.001
    )

    #    Natural Gas Plant Liquids
    # formula_F = T1(2,IY,IS)=RFQNGPL(MNUMPR,IY,6)/1000.*RDAYS*CFNGL(IY)*.001
    z[2] = (
        dfd["RFQNGPL"].loc[MNUMPR].loc[6]
        / 1000.0
        * dfd["RDAYS_rwpre"]
        * dfd["CFNGL"]
        * dfd["TRIL_TO_QUAD_rwpre"]
    )

    #    Dry Natural Gas
    # formula_F = T1(3,IY,IS)=CFNGC(IY)*(OGQNGREP(1,IY)+OGQNGREP(2,IY)+OGQNGREP(3,IY)\
    # +OGQNGREP(4,IY)+OGQNGREP(5,IY)+OGQNGREP(6,IY)+OGQNGREP(7,IY)\
    # +OGQNGREP(8,IY)+OGSHALENG(IY))*.001
    z[3] = (
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
        * dfd["TRIL_TO_QUAD_rwpre"]
    )

    #    Coal 1/
    # formula_F = T1(4,IY,IS)=(CQSBB(3,IY)+SUM(WC_PROD_BTU(11,1:MNUMLR,IY)))/1000.
    z[4] = (
        dfd["CQSBB"].loc[3] + dfd["WC_PROD_BTU"].loc[MNUMCR].loc[1:MNUMLR].sum()
    ) / 1000.0
    # z[4] = z[4].to_frame().T

    #    Nuclear / Uranium 2/
    # formula_F = T1(5,IY,IS)=QUREL(11,IY)
    z[5] = dfd["QUREL"].loc[MNUMCR]
    # z[5] = z[5].to_frame().T
    z[5] = z[5] * dfd["TRIL_TO_QUAD_rwpre"]

    #    Conventional Hydroelectric Power
    # formula_F = T1(31,IY,IS)=QHOAS(11,IY)
    z[31] = dfd["QHOAS"].loc[MNUMCR] * dfd["TRIL_TO_QUAD_rwpre"]

    #    Biomass 3/
    # T1(32,IY,IS)=QBMAS(11,IY)-QBMRF(11,IY)+CORNCD(3,11,IY)*CFCORN/1000000000. +
    #              SUM(BIMQTYCD(1:4,11,IY))/1000000.*RDAYS*CFVEGGIE(IY) +(SBO2GDTPD(MNUMPR,IY)+YGR2GDTPD(MNUMPR,IY)
    #              +WGR2GDTPD(MNUMPR,IY))*CFVEGGIE(IY)*RDAYS/1000.
    #
    # IF (CONEFF(IY) .NE. 0.0) T1(32,IY,IS) = &
    # T1(32,IY,IS) + 0.9751*CLLETHCD(11,IY) * RDAYS * CFBMQ(IY) * 42. / CONEFF(IY) / 1000000.

    z[32] = (
        (
            dfd["QBMAS"].loc[MNUMCR]
            - dfd["QBMRF"].loc[MNUMCR]
            + dfd["CORNCD"].loc[3].loc[MNUMCR] * dfd["CFCORN"].iloc[0] / 1000000.0
            + dfd["BIMQTYCD"].loc[(slice(None), 11), :].sum()
            / 1000.0
            * dfd["RDAYS_rwpre"]
            * dfd["CFVEGGIE"]
            + (
                dfd["SBO2GDTPD"].loc[MNUMPR]
                + dfd["YGR2GDTPD"].loc[MNUMPR]
                + dfd["WGR2GDTPD"].loc[MNUMPR]
            )
            * dfd["CFVEGGIE"]
            * dfd["RDAYS_rwpre"]
        )
        * dfd["TRIL_TO_QUAD_rwpre"]
    ) + (
        ETHANOL_PARAM
        * dfd["CLLETHCD"].loc[MNUMCR]
        * RDAYS
        * dfd["CFBMQ"]
        * GAL_PER_BBL
        / dfd["CONEFF"]
        / 1000000.0
    )

    #    Other Renewable Energy 4/
    # formula_F = T1(6,IY,IS)=QTRAS(11,IY)-WNCMSEL(IY,11)
    # -QGERS(11,IY)-QSTRS(11,IY)
    # -QSTCM(11,IY)-QPVCM(11,IY)
    # -QPVRS(11,IY)-QBMAS(11,IY)
    # -QHOAS(11,IY)-QETTR(11,IY)
    # z[6] = (dfd['QTRAS'].loc[MNUMCR] - dfd['WNCMSEL'].loc[(slice(None), 11), :].groupby('MNUMCR').sum()
    # z[6] = (dfd['QTRAS'].loc[MNUMCR] - dfd['WNCMSEL'].loc[MNUMCR]
    z[6] = (
        dfd["QTRAS"].loc[MNUMCR]
        - dfd["QGERS"].loc[MNUMCR]
        - dfd["QSTRS"].loc[MNUMCR]
        - dfd["QSTCM"].loc[MNUMCR]
        - dfd["QPVCM"].loc[MNUMCR]
        - dfd["QPVRS"].loc[MNUMCR]
        - dfd["QBMAS"].loc[MNUMCR]
        - dfd["QHOAS"].loc[MNUMCR]
        - dfd["QETTR"].loc[MNUMCR]
    ) * dfd["TRIL_TO_QUAD_rwpre"] - dfd["WNCMSEL"].loc[MNUMCR]

    #    Other 5/
    # T1(7,IY,IS)=WNCMSEL(IY,11)+RFCRDOTH(MNUMPR,IY) *CFCRDDOM(IY)*RDAYS/1000.+RFHCXH2IN(MNUMPR,IY)*CFRSQ*RDAYS/1000. +RFMETM85(MNUMPR,IY)*CFM85Q(IY)*RDAYS/1000.
    z[7] = (
        dfd["WNCMSEL"].loc[MNUMCR]
        + dfd["RFCRDOTH"].loc[MNUMPR] * dfd["CFCRDDOM"] * dfd["RDAYS_rwpre"] / 1000.0
        + dfd["RFHCXH2IN"].loc[MNUMPR]
        * dfd["CFRSQ"].iloc[0]
        * dfd["RDAYS_rwpre"]
        / 1000.0
        + dfd["RFMETM85"].loc[MNUMPR] * dfd["CFM85Q"] * dfd["RDAYS_rwpre"] / 1000.0
    )

    #    Total
    # T1(8,IY,IS)=FSUM(T1(1,IY,IS),7)+FSUM(T1(31,IY,IS),2)

    z[8] = z[1] + z[2] + z[3] + z[4] + z[5] + z[6] + z[7] + z[31] + z[32]

    # Imports----------
    #    Crude Oil
    # T1(9,IY,IS)=(RFQICRD(MNUMPR,IY)+RFSPRIM(IY))*CFCRDIMP(IY)*RDAYS*.001
    z[9] = (
        (dfd["RFQICRD"].loc[MNUMPR] + dfd["RFSPRIM"])
        * dfd["CFCRDIMP"]
        * dfd["RDAYS_rwpre"]
        * dfd["TRIL_TO_QUAD_rwpre"]
    )

    #    Petroleum and Other Liquids 6/
    # T1(10,IY,IS)=(RFPQIPRDT(MNUMPR,IY,2)*CFIMPRD(IY) +ETHIMP(11,IY)/1000.*CFPET +BIODIMP(11,IY)/1000.*CFBIOD(IY)+RENEWDIMP(11,IY)/1000.*CFDSQT(IY) +RFIPQCBOB(MNUMPR,IY,2)*CFCBOB(IY)/1000.
    #             +RFIPQRBOB(MNUMPR,IY,2)*CFRBOB(IY)/1000. +RFMTBI(MNUMPR,IY)*4.24+RFPQUFC(MNUMPR,IY,2)*CFIMUO(IY))*RDAYS*.001

    z[10] = (
        (
            dfd["RFPQIPRDT"].loc[MNUMPR].loc[2] * dfd["CFIMPRD"]
            + dfd["ETHIMP"].loc[MNUMCR] / 1000.0 * dfd["CFPET"].iloc[0]
            + dfd["BIODIMP"].loc[MNUMCR] / 1000.0 * dfd["CFBIOD"]
            + dfd["RENEWDIMP"].loc[MNUMCR] / 1000.0 * dfd["CFDSQT"]
            + dfd["RFIPQCBOB"].loc[MNUMPR].loc[2] * dfd["CFCBOB"] / 1000.0
            + dfd["RFIPQRBOB"].loc[MNUMPR].loc[2] * dfd["CFRBOB"] / 1000.0
            + dfd["RFMTBI"].loc[MNUMPR] * dfd["CFMTBE_rwpre"]
            + dfd["RFPQUFC"].loc[MNUMPR].loc[2] * dfd["CFIMUO"]
        )
        * dfd["RDAYS_rwpre"]
        * dfd["TRIL_TO_QUAD_rwpre"]
    )

    #    Natural Gas
    # formula_F = T1(11,IY,IS)=NGIMPVOL(4,IY)*CFNGI(IY)*.001

    z[11] = dfd["NGIMPVOL"].loc[4] * dfd["CFNGI"] * dfd["TRIL_TO_QUAD_rwpre"]

    #    Other 7/
    # formula_F = T1(12,IY,IS)=QCIIN(11,IY)+CQDBFB(11,7,IY)*.001+QEIEL(11,IY)
    z[12] = (
        dfd["QCIIN"].loc[MNUMCR]
        + dfd["CQDBFB"].loc[MNUMCR].loc[7]
        + dfd["QEIEL"].loc[MNUMCR]
    ) * dfd["TRIL_TO_QUAD_rwpre"]

    #    Total
    # formula_F = T1(13,IY,IS)=FSUM(T1(9,IY,IS),4)
    z[13] = z[9] + z[10] + z[11] + z[12]

    # Exports----------
    #    Petroleum and Other Liquids 8/
    # T1(14,IY,IS)=(RFQEXCRD(MNUMPR,IY)*.001*CFCRDEXP(IY)+RFQEXPRDT(MNUMPR,IY)*CFEXPRD(IY) +ETHEXP(11,IY)/1000.*CFPET+BIODEXP(11,IY)/1000.*CFBIOD(IY))*RDAYS*.001

    z[14] = (
        (
            dfd["RFQEXCRD"].loc[MNUMPR] * 0.001 * dfd["CFCRDEXP"]
            + dfd["RFQEXPRDT"].loc[MNUMPR] * dfd["CFEXPRD"]
            + dfd["ETHEXP"].loc[MNUMCR] / 1000.0 * dfd["CFPET"].iloc[0]
            + dfd["BIODEXP"].loc[MNUMCR] / 1000.0 * dfd["CFBIOD"]
        )
        * dfd["RDAYS_rwpre"]
        * dfd["TRIL_TO_QUAD_rwpre"]
    )

    #    Natural Gas
    # formula_F = T1(15,IY,IS)=NGEXPVOL(4,IY)*CFNGE(IY)*.001
    z[15] = dfd["NGEXPVOL"].loc[4] * dfd["CFNGE"] * 0.001

    #    Coal
    # T1(16,IY,IS)=CQDBFB(11,5,IY)*.001
    z[16] = dfd["CQDBFB"].loc[MNUMCR].loc[5] * dfd["TRIL_TO_QUAD_rwpre"]

    #    Total
    # T1(17,IY,IS)=FSUM(T1(14,IY,IS),3)
    z[17] = z[14] + z[15] + z[16]

    # Consumption-----------
    #    Petroleum and Other Liquids 10/
    # T1(19,IY,IS)=QTPAS(11,IY)+QETTR(11,IY)+QMETR(11,IY)
    z[19] = (
        dfd["QTPAS"].loc[MNUMCR] + dfd["QETTR"].loc[MNUMCR] + dfd["QMETR"].loc[MNUMCR]
    ) * dfd["TRIL_TO_QUAD_rwpre"]

    #    Natural Gas
    # formula_F = T1(20,IY,IS)=QNGAS(11,IY)+QGPTR(11,IY)+QLPIN(11,IY)+QNGLQ(11,IY)-QHYTR(11,IY)/.7
    z[20] = (
        dfd["QNGAS"].loc[MNUMCR]
        + dfd["QGPTR"].loc[MNUMCR]
        + dfd["QLPIN"].loc[MNUMCR]
        + dfd["QNGLQ"].loc[MNUMCR]
        - dfd["QHYTR"].loc[MNUMCR] / 0.7
    ) * dfd["TRIL_TO_QUAD_rwpre"]

    #    Coal 11/
    # T1(21,IY,IS)=QCLAS(11,IY)+QMCIN(11,IY)+QCIIN(11,IY)-OGSUPGAS(1,11,IY)*CFNGC(IY)*.001
    z[21] = (
        dfd["QCLAS"].loc[MNUMCR]
        + dfd["QMCIN"].loc[MNUMCR]
        + dfd["QCIIN"].loc[MNUMCR]
        - dfd["OGSUPGAS"].loc[1, MNUMCR] * dfd["CFNGC"] * 0.001
    ) * dfd["TRIL_TO_QUAD_rwpre"]

    #    Nuclear / Uranium 2/
    # formula_F = T1(22,IY,IS)=QUREL(11,IY)
    z[22] = dfd["QUREL"].loc[MNUMCR] * dfd["TRIL_TO_QUAD_rwpre"]

    #    Biomass 12/

    # z[33] = dfd['T1_33_rwpre'] * dfd['TRIL_TO_QUAD_rwpre']
    # T1(33,IY,IS) = QBMAS(11,IY) - QBMRF(11,IY) + CORNCD(3,11,IY) * CFCORN / 1000000000. - &
    # RFBIOBUTECD(MNUMCR,IY)*RDAYS*CFBIOBUTE(IY)/1000000. - &
    # 0.9751*(CRNETHCD(11,IY)+CLLETHCD(11,IY)+OTHETHCD(11,IY))/1000. * CFPET * RDAYS / 1000. + &
    # sum(BIMQTYCD(1:4,11,IY))/1000000.*RDAYS*(CFVEGGIE(IY)-CFBIOD(IY)) + &
    # - RDAYS / 1000000. * &
    # (sum(BTLFRAC(1:4,MNUMPR,IY)) * CFBTLLIQ(IY) + &
    # sum(CBTLFRAC(2,1:4,MNUMPR,IY)) * CFCBTLLIQ(2,IY) + &
    # UBAVOL(MNUMPR,IY) * 5.763)
    # IF (CONEFF(IY) .NE. 0.0) T1(33,IY,IS) = &
    # T1(33,IY,IS) + 0.9751*CLLETHCD(11,IY) * RDAYS * CFBMQ(IY) * 42. / CONEFF(IY) / 1000000.

    z[33] = (
        dfd["QBMAS"].loc[11] / 1000
        - dfd["QBMRF"].loc[11] / 1000
        + dfd["CORNCD"].loc[3].loc[11] * dfd["CFCORN"].iloc[0] / 1000000000.0
        - dfd["RFBIOBUTECD"].loc[MNUMCR] * RDAYS * dfd["CFBIOBUTE"] / 1000000.0
        - dfd["ETHANOL_PARAM_rwpre"]
        * (dfd["CRNETHCD"].loc[11] + dfd["CLLETHCD"].loc[11] + dfd["OTHETHCD"].loc[11])
        / 1000.0
        * dfd["CFPET"].iloc[0]
        * RDAYS
        / 1000.0
        + dfd["BIMQTYCD"].loc[1:4, 11, :].sum()
        / 1000000.0
        * RDAYS
        * (dfd["CFVEGGIE"] - dfd["CFBIOD"])
        - RDAYS
        / 1000000.0
        * (
            dfd["BTLFRAC"].loc[1:4, MNUMPR, :].sum() * dfd["CFBTLLIQ"]
            + dfd["CBTLFRAC"].loc[2, 1:4, MNUMPR, :].sum() * dfd["CFCBTLLIQ"].loc[2]
            + dfd["UBAVOL"].loc[MNUMPR] * dfd["CFUBA_rwpre"]
        )
        + dfd["ETHANOL_PARAM_rwpre"]
        * dfd["CLLETHCD"].loc[11]
        * RDAYS
        * dfd["CFBMQ"]
        * dfd["GAL_PER_BBL_rwpre"]
        / dfd["CONEFF"]
        / 1000000.0
    )

    #    Other Renewable Energy 4/
    # formula_F = T1(23,IY,IS)=QTRAS(11,IY)-WNCMSEL(IY,11)-QGERS(11,IY)-QSTRS(11,IY)-QSTCM(11,IY)-QPVCM(11,IY)-QPVRS(11,IY)-QBMAS(11,IY)-QHOAS(11,IY)-QETTR(11,IY)

    z[23] = (
        dfd["QTRAS"].loc[MNUMCR]
        - dfd["QGERS"].loc[MNUMCR]
        - dfd["QSTRS"].loc[MNUMCR]
        - dfd["QSTCM"].loc[MNUMCR]
        - dfd["QPVCM"].loc[MNUMCR]
        - dfd["QPVRS"].loc[MNUMCR]
        - dfd["QBMAS"].loc[MNUMCR]
        - dfd["QHOAS"].loc[MNUMCR]
        - dfd["QETTR"].loc[MNUMCR]
    ) * dfd["TRIL_TO_QUAD_rwpre"] - dfd["WNCMSEL"].loc[MNUMCR]
    #    Other 13/
    # formula_F = T1(24,IY,IS)=QEIEL(11,IY)+WNCMSEL(IY,11)+QHYTR(11,IY)
    z[24] = (dfd["QEIEL"].loc[MNUMCR] + dfd["QHYTR"].loc[MNUMCR]) * dfd[
        "TRIL_TO_QUAD_rwpre"
    ] + dfd["WNCMSEL"].loc[MNUMCR]

    #  Total
    # formula_F = T1(25,IY,IS)=FSUM(T1(19,IY,IS),6)+T1(31,IY,IS)+T1(33,IY,IS)
    z[25] = z[19] + z[20] + z[21] + z[22] + z[23] + z[24] + z[31] + z[33]

    # Discrepancy 9/----------------
    # formula_F = T1(18,IY,IS)=T1(8,IY,IS)-T1(25,IY,IS)+T1(13,IY,IS)-T1(17,IY,IS)
    z[18] = z[8] - z[25] + z[13] - z[17]

    # Prices (#### dollars per unit)------------
    #   Brent Spot Price (dollars per barrel)
    # Prices (#### dollars per unit)
    # T1(27,IY,IS)=BRENT_PRICE(IY)
    z[27] = dfd["BRENT_PRICE"] * SCALPR2

    #   West Texas Intermediate Spot Price (dollars per barrel)
    # formula_F = T1(26,IY,IS)=T1(9,IY,IS)+T1(10,IY,IS)-T1(14,IY,IS)
    # T1(26,IY,IS) = WTI_PRICE(IY)
    z[26] = dfd["WTI_PRICE"] * SCALPR2

    #   Base World Oil Price
    # formula_F = T1(28,IY,IS)=START_PRICE(IY)
    z[28] = dfd["START_PRICE"] * SCALPR2

    #  Upper uncertainty bound
    # formula_F = T1(41,IY,IS)=T1(27,IY,IS)*1.32
    z[41] = z[27] * 1.32

    #  Lower uncertainty bound
    # formula_F = T1(42,IY,IS)=T1(27,IY,IS)*0.75
    z[42] = z[27] * 0.75

    #   Natural Gas at Henry Hub (dollars per mmBtu)
    # formula_F = T1(34,IY,IS)=OGHHPRNG(IY)
    z[34] = dfd["OGHHPRNG"] * SCALPR2

    #   Coal, Minemouth (dollars per ton) 14/
    # formula_F = T1(29,IY,IS)=CPSB(3,IY)*CPSBT(3,IY)
    z[29] = (dfd["CPSB"].loc[3] * dfd["CPSBT"].loc[3]) * SCALPR2

    #   Coal, Minemouth (dollars per million Btu) 14/
    # formula_F =  T1(37,IY,IS) = CPSB(3,IY)
    z[37] = dfd["CPSB"].loc[3] * SCALPR2

    #   Coal, Delivered (dollars per million Btu) 15/
    # T1(36,IY,IS) = (PCLSN(11,IY)*QCLSN(11,IY) + PCLIN(11,IY)*(QCLIN(11,IY)) + &
    #        PCLCM(11,IY)*QCLCM(11,IY) + PMCIN(11,IY)*QMCIN(11,IY) + PCLEL(11,IY)*QCLEL(11,IY)) / &
    #       (QCLCM(11,IY) + QCLIN(11,IY) + QMCIN(11,IY) + QCLEL(11,IY) + QCLSN(11,IY))

    z[36] = (
        (
            dfd["AMPBLK/PCLSN"].loc[MNUMCR] * dfd["QCLSN"].loc[MNUMCR]
            + dfd["AMPBLK/PCLIN"].loc[MNUMCR]
            * (dfd["QCLIN"].loc[MNUMCR])
            + dfd["AMPBLK/PCLCM"].loc[MNUMCR] * dfd["QCLCM"].loc[MNUMCR]
            + dfd["AMPBLK/PMCIN"].loc[MNUMCR] * dfd["QMCIN"].loc[MNUMCR]
            + dfd["AMPBLK/PCLEL"].loc[MNUMCR] * dfd["QCLEL"].loc[MNUMCR]
        )
        / (
            dfd["QCLCM"].loc[MNUMCR]
            + dfd["QCLIN"].loc[MNUMCR]
            + dfd["QMCIN"].loc[MNUMCR]
            + dfd["QCLEL"].loc[MNUMCR]
            + dfd["QCLSN"].loc[MNUMCR]
        )
    ) * SCALPR2

    #   Electricity (cents per kilowatthour)
    # T1(30,IY,IS)=PELAS(11,IY)*.3412
    z[30] = dfd["AMPBLK/PELAS"].loc[MNUMCR] * dfd["GWh_to_TRIL_rwpre"] * 0.1 * SCALPR2

    # Prices (nominal dollars per unit)--------------------
    #   Brent Spot Price (dollars per barrel)
    #  T1(39,IY,IS)=T1(27,IY,IS)/SCALPR*MC_JPGDP(IY)
    # z[39] = z[27] / (dfd['SCALPR'][0].values * dfd['MC_JPGDP'].loc[1:61])[0]
    z[39] = dfd["BRENT_PRICE"] * MC_JPGDP

    #   West Texas Intermediate Spot Price (dollars per barrel)
    # T1(38,IY,IS)=T1(26,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[38] = z[26] / SCALPR2 * MC_JPGDP
    #   Natural Gas at Henry Hub (dollars per mmBtu)
    # formula_F = T1(40,IY,IS)=T1(34,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[40] = dfd["OGHHPRNG"] * MC_JPGDP

    #   Coal, Minemouth (dollars per ton) 14/
    # formula_F = T1(43,IY,IS)=T1(29,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[43] = z[29] / SCALPR2 * MC_JPGDP

    #   Coal, Minemouth (dollars per million Btu) 14/
    # formula_F = T1(35,IY,IS)=T1(37,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[35] = z[37] / SCALPR2 * MC_JPGDP

    #   Coal, Delivered (dollars per million Btu) 15/
    # formula_F = T1(44,IY,IS)=T1(36,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[44] = z[36] / SCALPR2 * MC_JPGDP

    #   Electricity (cents per kilowatthour)
    # formula_F = T1(45,IY,IS)=T1(30,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[45] = z[30] / SCALPR2 * MC_JPGDP

    return z
