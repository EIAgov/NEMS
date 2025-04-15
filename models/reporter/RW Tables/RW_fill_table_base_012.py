# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""
import pandas as pd


def fill_table_base_012(dfd, table_spec, table_id):
    """Fill table Petroleum and Other Liquids Prices

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

    MNUMCR = dfd["MNUMCR_rwpre"]
    RDAYS = dfd["RDAYS_rwpre"]
    GAL_PER_BBL = dfd["GAL_PER_BBL_rwpre"]

    # Preprocess propane
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

    dfd["APMORE/PPRTR"].loc[MNUMCR] = (
        (
            dfd["QPRTR"].loc[1] * dfd["APMORE/PPRTR"].loc[1]
            + dfd["QPRTR"].loc[2] * dfd["APMORE/PPRTR"].loc[2]
            + dfd["QPRTR"].loc[3] * dfd["APMORE/PPRTR"].loc[3]
            + dfd["QPRTR"].loc[4] * dfd["APMORE/PPRTR"].loc[4]
            + dfd["QPRTR"].loc[5] * dfd["APMORE/PPRTR"].loc[5]
            + dfd["QPRTR"].loc[6] * dfd["APMORE/PPRTR"].loc[6]
            + dfd["QPRTR"].loc[7] * dfd["APMORE/PPRTR"].loc[7]
            + dfd["QPRTR"].loc[8] * dfd["APMORE/PPRTR"].loc[8]
            + dfd["QPRTR"].loc[9] * dfd["APMORE/PPRTR"].loc[9]
        )
        / dfd["QPRTR"].loc[1:9, :].sum()
    ).fillna(dfd["APMORE/PPRTR"].loc[1:9, :].sum() / 9.0)
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

    #   Petroleum and Other Liquids Prices
    #   (#### dollars per gallon, unless otherwise noted)
    #    Sector and Fuel
    #
    #   Crude Oil Prices (#### dollars per barrel)

    #      Brent Spot
    # T12(1,IY,IS)=BRENT_PRICE(IY)
    z[1] = dfd["BRENT_PRICE"] * SCALPR2

    #      West Texas Intermediate Spot
    # T12(29,IY,IS)=WTI_PRICE(IY)
    z[29] = dfd["WTI_PRICE"] * SCALPR2

    #      Average Imported Cost 1/
    # Note : indices are reversed
    # T12(55,IY,IS)=IT_WOP(IY,1)
    z[55] = dfd["IT_WOP"].loc[1] * SCALPR2

    #      Brent / West Texas Intermediate Spread
    # T12(57,IY,IS)=BRENT_PRICE(IY)-WTI_PRICE(IY)
    z[57] = (dfd["BRENT_PRICE"] - dfd["WTI_PRICE"]) * SCALPR2

    #

    #    Delivered Sector Product Prices

    #

    #    Residential

    #      Propane
    # T12(3,IY,IS)=PPRRS(11,IY)*CFPRQ/42.
    z[3] = (
        dfd["APMORE/PPRRS"].loc[MNUMCR] * dfd["CFPRQ"].iloc[0] / GAL_PER_BBL * SCALPR2
    )

    #      Distillate Fuel Oil
    # T12(2,IY,IS)=PDSRS(11,IY)*CFDSRS(IY)/42.
    z[2] = dfd["AMPBLK/PDSRS"].loc[MNUMCR] * dfd["CFDSRS"] / GAL_PER_BBL * SCALPR2

    #

    #    Commercial

    #      Distillate Fuel Oil
    # T12(4,IY,IS)=PDSCM(11,IY)*CFDSCM(IY)/42.
    z[4] = dfd["AMPBLK/PDSCM"].loc[MNUMCR] * dfd["CFDSCM"] / GAL_PER_BBL * SCALPR2

    #      Residual Fuel Oil
    # T12(5,IY,IS)=PRSCM(11,IY)*CFRSQ/42.
    z[5] = (
        dfd["AMPBLK/PRSCM"].loc[MNUMCR] * dfd["CFRSQ"].iloc[0] / GAL_PER_BBL * SCALPR2
    )

    #      Residual Fuel Oil (#### dollars per barrel)
    # T12(6,IY,IS)=PRSCM(11,IY)*CFRSQ
    z[6] = dfd["AMPBLK/PRSCM"].loc[MNUMCR] * dfd["CFRSQ"].iloc[0] * SCALPR2

    #

    #    Industrial 2/

    #      Propane
    # T12( 8,IY,IS) = PPRIN(11,IY) * CFPRQ / 42.
    z[8] = (
        dfd["APMORE/PPRIN"].loc[MNUMCR] * dfd["CFPRQ"].iloc[0] / GAL_PER_BBL * SCALPR2
    )

    #      Distillate Fuel Oil
    # T12(7,IY,IS)=PDSIN(11,IY)*CFDSIN(IY)/42.
    z[7] = dfd["AMPBLK/PDSIN"].loc[MNUMCR] * dfd["CFDSIN"] / GAL_PER_BBL * SCALPR2

    #      Residual Fuel Oil
    # T12(9,IY,IS)=PRSIN(11,IY)*CFRSQ/42.
    z[9] = (
        dfd["AMPBLK/PRSIN"].loc[MNUMCR] * dfd["CFRSQ"].iloc[0] / GAL_PER_BBL * SCALPR2
    )

    #      Residual Fuel Oil (#### dollars per barrel)
    # T12(10,IY,IS)=PRSIN(11,IY)*CFRSQ
    z[10] = dfd["AMPBLK/PRSIN"].loc[MNUMCR] * dfd["CFRSQ"].iloc[0] * SCALPR2

    #      Petrochemical Feedstocks
    # T12(58,IY,IS)=PPFIN(11,IY)*CFPFQ(IY)/42.
    z[58] = dfd["PPFIN"].loc[MNUMCR] * dfd["CFPFQ"] / GAL_PER_BBL * SCALPR2

    #

    #    Transportation

    #      Propane
    # T12(14,IY,IS)=PPRTR(11,IY)*CFPRQ/42.
    z[14] = (
        dfd["APMORE/PPRTR"].loc[MNUMCR] * dfd["CFPRQ"].iloc[0] / GAL_PER_BBL * SCALPR2
    )

    #      E85 3/
    # T12(17,IY,IS)=PETTR(11,IY)*CFE85Q(IY)/42.
    z[17] = dfd["AMPBLK/PETTR"].loc[MNUMCR] * dfd["CFE85Q"] / GAL_PER_BBL * SCALPR2

    #      Ethanol Wholesale Price
    # T12(30,IY,IS)=PETHM(11,IY)/42.
    z[30] = dfd["PETHM"].loc[MNUMCR] / 42.0 * SCALPR2

    #      Motor Gasoline 4/
    # T12(13,IY,IS)=PMGTR(11,IY)*CFMGQ(IY)/42.
    z[13] = dfd["AMPBLK/PMGTR"].loc[MNUMCR] * dfd["CFMGQ"] / GAL_PER_BBL * SCALPR2

    #      Jet Fuel 5/
    # T12(12,IY,IS)=PJFTR(11,IY)*CFJFK/42.
    z[12] = (
        dfd["AMPBLK/PJFTR"].loc[MNUMCR] * dfd["CFJFK"].iloc[0] / GAL_PER_BBL * SCALPR2
    )

    #      Diesel Fuel (distillate fuel oil) 6/
    # T12(11,IY,IS)=PDSTRHWY(11,IY)*CFDSTRHWY(IY)/42.
    z[11] = (
        dfd["APONROAD/PDSTRHWY"].loc[MNUMCR] * dfd["CFDSTRHWY"] / GAL_PER_BBL * SCALPR2
    )

    #      Residual Fuel Oil
    # T12(15,IY,IS)=PRSTR(11,IY)*CFRSQ/42.
    z[15] = dfd["PRSTR"].loc[MNUMCR] * dfd["CFRSQ"].iloc[0] / GAL_PER_BBL * SCALPR2

    #      Residual Fuel Oil (#### dollars per barrel)
    # T12(16,IY,IS)=PRSTR(11,IY)*CFRSQ
    z[16] = dfd["PRSTR"].loc[MNUMCR] * dfd["CFRSQ"].iloc[0] * SCALPR2

    #

    #    Electric Power 7/

    #      Distillate Fuel Oil
    # T12(19,IY,IS)=PDSEL(11,IY)*CFDSEL(IY)/42.
    z[19] = dfd["AMPBLK/PDSEL"].loc[MNUMCR] * dfd["CFDSEL"] / GAL_PER_BBL * SCALPR2

    #      Residual Fuel Oil
    # T12(20,IY,IS)=PRSEL(11,IY)*CFRSQ/42.
    # z[1] = dfd['CFASQ']

    # z[20] = dfd['PRSEL'].loc[MNUMCR] * dfd['CFRSQ'].iloc[0] / GAL_PER_BBL * SCALPR2
    z[20] = (
        dfd["AMPBLK/PRSEL"].loc[MNUMCR] * dfd["CFRSQ"].iloc[0] / GAL_PER_BBL * SCALPR2
    ).T

    #      Residual Fuel Oil (#### dollars per barrel)
    # T12(21,IY,IS)=PRSEL(11,IY)*CFRSQ
    z[21] = dfd["AMPBLK/PRSEL"].loc[MNUMCR] * dfd["CFRSQ"].iloc[0] * SCALPR2

    #

    #   Average Prices, All Sectors 8/

    #      Propane
    # T12(24,IY,IS)=((QPRRS(11,IY)*PPRRS(11,IY)+QPRCM(11,IY)*PPRCM(11,IY)+(QPRIN(11,IY)-QPRINPF(11,IY))*PPRIN(11,IY)+QPRTR(11,IY)*PPRTR(11,IY))/(QPRRS(11,IY)+QPRCM(11,IY)+QPRIN(11,IY)-QPRINPF(11,IY)+QPRTR(11,IY)))*CFPRQ/42.
    z[24] = (
        (
            (
                dfd["QPRRS"].loc[MNUMCR] * dfd["APMORE/PPRRS"].loc[MNUMCR]
                + dfd["QPRCM"].loc[MNUMCR] * dfd["APMORE/PPRCM"].loc[MNUMCR]
                + (dfd["QPRIN"].loc[MNUMCR] - dfd["QPRINPF"].loc[MNUMCR])
                * dfd["APMORE/PPRIN"].loc[MNUMCR]
                + dfd["QPRTR"].loc[MNUMCR] * dfd["APMORE/PPRTR"].loc[MNUMCR]
            )
            / (
                dfd["QPRRS"].loc[MNUMCR]
                + dfd["QPRCM"].loc[MNUMCR]
                + dfd["QPRIN"].loc[MNUMCR]
                - dfd["QPRINPF"].loc[MNUMCR]
                + dfd["QPRTR"].loc[MNUMCR]
            )
        )
        * dfd["CFPRQ"].iloc[0]
        / GAL_PER_BBL
        * SCALPR2
    )

    #      Motor Gasoline 4/
    # T12(25,IY,IS)=PMGAS(11,IY)*CFMGQ(IY)/42.
    z[25] = dfd["AMPBLK/PMGAS"].loc[MNUMCR] * dfd["CFMGQ"] / GAL_PER_BBL * SCALPR2

    #      Jet Fuel 5/
    # T12(23,IY,IS)=PJFTR(11,IY)*CFJFK/42.
    z[23] = (
        dfd["AMPBLK/PJFTR"].loc[MNUMCR] * dfd["CFJFK"].iloc[0] / GAL_PER_BBL * SCALPR2
    )

    #      Distillate Fuel Oil
    # T12(22,IY,IS)=PDSAS(11,IY)*CFDSQT(IY)/42.
    z[22] = dfd["AMPBLK/PDSAS"].loc[MNUMCR] * dfd["CFDSQT"] / GAL_PER_BBL * SCALPR2

    #      Residual Fuel Oil
    # T12(26,IY,IS)=PRSAS(11,IY)*CFRSQ/42.
    z[26] = (
        dfd["AMPBLK/PRSAS"].loc[MNUMCR] * dfd["CFRSQ"].iloc[0] / GAL_PER_BBL * SCALPR2
    )

    #      Residual Fuel Oil (#### dollars per barrel)
    # T12(27,IY,IS)=PRSAS(11,IY)*CFRSQ
    z[27] = dfd["AMPBLK/PRSAS"].loc[MNUMCR] * dfd["CFRSQ"].iloc[0] * SCALPR2

    #        Average
    # T12(28,IY,IS)=(PDSAS(11,IY)*CFDSQT(IY)*QDSAS(11,IY)/CFDSQT(IY)/365+PKSAS(11,IY)*
    #               CFKSQ*QKSAS(11,IY)/CFKSQ/365+PMGAS(11,IY)*CFMGQ(IY)*(QMGAS(11,IY)/
    #               CFMGQ(IY)/365+0.15*QMETR(11,IY)/CFM85Q(IY)/365+TRGNE85*QETTR(11,IY)/
    #               CFE85Q(IY)/365)+PJFTR(11,IY)*CFJFK*QJFTR(11,IY)/CFJFK/365+PRSAS(11,IY)*
    #               CFRSQ*(QRLAS(11,IY)/CFRSQ/365+QRHAS(11,IY)/CFRSQ/365)+PLGAS(11,IY)*CFLGQ(IY)*
    #               QLGAS(11,IY)/CFLGQ(IY)/365+PPFIN(11,IY)*CFPFQ(IY)*QPFIN(11,IY)/
    #               CFPFQ(IY)/365)/ T12(28,IY,IS)/42.
    # look at code below
    # T12(28,IY,IS) = QMGAS(11,IY)/CFMGQ(IY)/365+ &
    # 0.15*QMETR(11,IY)/CFM85Q(IY)/365+ &
    # TRGNE85*QETTR(11,IY)/CFE85Q(IY)/365+ &
    # QJFTR(11,IY)/CFJFK/365+QDSAS(11,IY)/CFDSQT(IY)/365 + &
    # QKSAS(11,IY)/CFKSQ/365+QRLAS(11,IY)/CFRSQ/365+ &
    # QRHAS(11,IY)/CFRSQ/365+QLGAS(11,IY)/CFLGQ(IY)/365+ &
    # QPFIN(11,IY)/CFPFQ(IY)/365
    # IF (T12(28,IY,IS) .NE. 0.0) T12(28,IY,IS) = &
    # (PDSAS(11,IY)*CFDSQT(IY)* QDSAS(11,IY)/CFDSQT(IY)/365 + &
    # PKSAS(11,IY)*CFKSQ     * QKSAS(11,IY)/CFKSQ/365 + &
    # PMGAS(11,IY)*CFMGQ(IY) *(QMGAS(11,IY)/CFMGQ(IY)/365+ &
    # 0.15*QMETR(11,IY)/CFM85Q(IY)/365+ &
    # TRGNE85*QETTR(11,IY)/CFE85Q(IY)/365)+ &
    # PJFTR(11,IY) * CFJFK  * QJFTR(11,IY)/CFJFK/365 + &
    # PRSAS(11,IY) * CFRSQ  *(QRLAS(11,IY)/CFRSQ/365 + QRHAS(11,IY)/CFRSQ/365) + &
    # PLGAS(11,IY) * CFLGQ(IY) *QLGAS(11,IY)/CFLGQ(IY)/365+ &
    # PPFIN(11,IY) * CFPFQ(IY) *QPFIN(11,IY)/CFPFQ(IY)/365) / T12(28,IY,IS) / 42.

    # z[28] = ((dfd['PDSAS'].loc[MNUMCR] * dfd['CFDSQT'] * dfd['QDSAS'].loc[MNUMCR] / dfd['CFDSQT'] /RDAYS + \
    #          dfd['PKSAS'].loc[MNUMCR]*dfd['CFKSQ'].iloc[0] * dfd['QKSAS'].loc[MNUMCR] / dfd['CFKSQ'].iloc[0]/RDAYS + \
    #          dfd['PMGAS'].loc[MNUMCR] * dfd['CFMGQ'] * (dfd['QMGAS'].loc[MNUMCR] /dfd['CFMGQ']/RDAYS+ \
    #          0.15 * dfd['QMETR'].loc[MNUMCR] / dfd['CFM85Q']/RDAYS+ \
    #          dfd['TRGNE85'] * dfd['QETTR'].loc[MNUMCR]/dfd['CFE85Q']/RDAYS)+ \
    #          dfd['PJFTR'].loc[MNUMCR] * dfd['CFJFK'].iloc[0] * dfd['QJFTR'].loc[MNUMCR] / dfd['CFJFK'].iloc[0]/RDAYS + \
    #          dfd['PRSAS'].loc[MNUMCR] * dfd['CFRSQ'].iloc[0] * (dfd['QRLAS'].loc[MNUMCR] / dfd['CFRSQ'].iloc[0]/RDAYS + dfd['QRHAS'].loc[MNUMCR] / dfd['CFRSQ'].iloc[0]/RDAYS) + \
    #          dfd['PLGAS'].loc[MNUMCR] * dfd['CFLGQ'] *dfd['QLGAS'].loc[MNUMCR]/dfd['CFLGQ']/RDAYS+ \
    #          dfd['PPFIN'].loc[MNUMCR] * dfd['CFPFQ'] * dfd['QPFIN'].loc[MNUMCR]/dfd['CFPFQ']/RDAYS) / (dfd['QMGAS'].loc[MNUMCR]/dfd['CFMGQ']/RDAYS+ \
    #          0.15*dfd['QMETR'].loc[MNUMCR]/dfd['CFM85Q']/RDAYS+ \
    #          dfd['TRGNE85']*dfd['QMETR'].loc[MNUMCR]/dfd['CFE85Q']/RDAYS+ \
    #          dfd['QJFTR'].loc[MNUMCR]/dfd['CFJFK'].iloc[0]/RDAYS+dfd['QDSAS'].loc[MNUMCR]/dfd['CFDSQT']/RDAYS + \
    #          dfd['QKSAS'].loc[MNUMCR]/dfd['CFKSQ'].iloc[0]/RDAYS+dfd['QRLAS'].loc[MNUMCR]/dfd['CFRSQ'].iloc[0]/RDAYS+ \
    #          dfd['QRHAS'].loc[MNUMCR]/dfd['CFRSQ'].iloc[0]/RDAYS+dfd['QLGAS'].loc[MNUMCR]/dfd['CFLGQ']/RDAYS+ \
    #          dfd['QPFIN'].loc[MNUMCR]/dfd['CFPFQ']/RDAYS) / GAL_PER_BBL) * SCALPR2

    z[28] = (
        (
            dfd["AMPBLK/PDSAS"].loc[MNUMCR]
            * dfd["CFDSQT"]
            * dfd["QDSAS"].loc[MNUMCR]
            / dfd["CFDSQT"]
            / RDAYS
            + dfd["AMPBLK/PKSAS"].loc[MNUMCR]
            * dfd["CFKSQ"].iloc[0]
            * dfd["QKSAS"].loc[MNUMCR]
            / dfd["CFKSQ"].iloc[0]
            / RDAYS
            + dfd["AMPBLK/PMGAS"].loc[MNUMCR]
            * dfd["CFMGQ"]
            * (
                dfd["QMGAS"].loc[MNUMCR] / dfd["CFMGQ"] / RDAYS
                + 0.15 * dfd["QMETR"].loc[MNUMCR] / dfd["CFM85Q"] / RDAYS
                + dfd["TRGNE85"].iloc[0]
                * dfd["QETTR"].loc[MNUMCR]
                / dfd["CFE85Q"]
                / RDAYS
            )
            + dfd["AMPBLK/PJFTR"].loc[MNUMCR]
            * dfd["CFJFK"].iloc[0]
            * dfd["QJFTR"].loc[MNUMCR]
            / dfd["CFJFK"].iloc[0]
            / RDAYS
            + dfd["AMPBLK/PRSAS"].loc[MNUMCR]
            * dfd["CFRSQ"].iloc[0]
            * (
                dfd["QRLAS"].loc[MNUMCR] / dfd["CFRSQ"].iloc[0] / RDAYS
                + dfd["QRHAS"].loc[MNUMCR] / dfd["CFRSQ"].iloc[0] / RDAYS
            )
            + dfd["AMPBLK/PLGAS"].loc[MNUMCR]
            * dfd["CFLGQ"]
            * dfd["QLGAS"].loc[MNUMCR]
            / dfd["CFLGQ"]
            / RDAYS
            + dfd["AMPBLK/PPFIN"].loc[MNUMCR]
            * dfd["CFPFQ"]
            * dfd["QPFIN"].loc[MNUMCR]
            / dfd["CFPFQ"]
            / RDAYS
        )
        / (
            dfd["QMGAS"].loc[MNUMCR] / dfd["CFMGQ"] / RDAYS
            + 0.15 * dfd["QMETR"].loc[MNUMCR] / dfd["CFM85Q"] / RDAYS
            + dfd["TRGNE85"].iloc[0] * dfd["QMETR"].loc[MNUMCR] / dfd["CFE85Q"] / RDAYS
            + dfd["QJFTR"].loc[MNUMCR] / dfd["CFJFK"].iloc[0] / RDAYS
            + dfd["QDSAS"].loc[MNUMCR] / dfd["CFDSQT"] / RDAYS
            + dfd["QKSAS"].loc[MNUMCR] / dfd["CFKSQ"].iloc[0] / RDAYS
            + dfd["QRLAS"].loc[MNUMCR] / dfd["CFRSQ"].iloc[0] / RDAYS
            + dfd["QRHAS"].loc[MNUMCR] / dfd["CFRSQ"].iloc[0] / RDAYS
            + dfd["QLGAS"].loc[MNUMCR] / dfd["CFLGQ"] / RDAYS
            + dfd["QPFIN"].loc[MNUMCR] / dfd["CFPFQ"] / RDAYS
        )
        / GAL_PER_BBL
    ) * SCALPR2

    #   Prices in Nominal Dollars

    #   Crude Oil Spot Prices (nominal dollars per barrel)

    #      Brent Spot
    # T12(32,IY,IS)=T12(1,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[32] = z[1] / SCALPR2 * MC_JPGDP

    #      West Texas Intermediate Spot
    # T12(31,IY,IS)=T12(29,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[31] = z[29] / SCALPR2 * MC_JPGDP

    #      Average Imported Cost 1/
    # T12(56,IY,IS)=T12(55,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[56] = z[55] / SCALPR2 * MC_JPGDP

    #

    #   Delivered Sector Product Prices

    #   Nominal Dollars per Gallon

    #    Residential

    #      Propane
    # T12(33,IY,IS)=T12(3,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[33] = z[3] / SCALPR2 * MC_JPGDP

    #      Distillate Fuel Oil
    # T12(34,IY,IS)=T12(2,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[34] = z[2] / SCALPR2 * MC_JPGDP

    #

    #    Commercial

    #      Distillate Fuel Oil
    # T12(35,IY,IS)=T12(4,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[35] = z[4] / SCALPR2 * MC_JPGDP

    #      Residual Fuel Oil
    # T12(36,IY,IS)=T12(5,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[36] = z[5] / SCALPR2 * MC_JPGDP

    #

    #    Industrial 2/

    #      Propane
    # T12(37,IY,IS)=T12(8,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[37] = z[8] / SCALPR2 * MC_JPGDP

    #      Distillate Fuel Oil
    # T12(38,IY,IS)=T12(7,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[38] = z[7] / SCALPR2 * MC_JPGDP

    #      Residual Fuel Oil
    # T12(39,IY,IS)=T12(9,IY,IS)/SCALPR*MC_JPGDP(IY)
    # z[39] = z[9] / SCALPR2 * MC_JPGDP
    z[39] = (
        dfd["AMPBLK/PRSIN"].loc[MNUMCR] * dfd["CFRSQ"].iloc[0] / GAL_PER_BBL * MC_JPGDP
    )

    #

    #    Transportation

    #      Propane
    # T12(40,IY,IS)=T12(14,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[40] = z[14] / SCALPR2 * MC_JPGDP

    #      E85 3/
    # T12(41,IY,IS)=T12(17,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[41] = z[17] / SCALPR2 * MC_JPGDP

    #      Ethanol Wholesale Price
    # T12(42,IY,IS)=T12(30,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[42] = z[30] / SCALPR2 * MC_JPGDP

    #      Motor Gasoline 4/
    # T12(43,IY,IS)=T12(13,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[43] = z[13] / SCALPR2 * MC_JPGDP

    #      Jet Fuel 5/
    # T12(44,IY,IS)=T12(12,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[44] = z[12] / SCALPR2 * MC_JPGDP

    #      Diesel Fuel (distillate fuel oil) 6/
    # T12(45,IY,IS)=T12(11,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[45] = z[11] / SCALPR2 * MC_JPGDP

    #      Residual Fuel Oil
    # T12(46,IY,IS)=T12(15,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[46] = z[15] / SCALPR2 * MC_JPGDP

    #

    #    Electric Power 7/

    #      Distillate Fuel Oil
    # T12(47,IY,IS)=T12(19,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[47] = z[19] / SCALPR2 * MC_JPGDP

    #      Residual Fuel Oil
    # T12(48,IY,IS)=T12(20,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[48] = z[20] / SCALPR2 * MC_JPGDP

    #

    #    Average Prices, All Sectors 8/

    #      Propane
    # T12(49,IY,IS)=T12(24,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[49] = z[24] / SCALPR2 * MC_JPGDP

    #      Motor Gasoline 4/
    # T12(50,IY,IS)=T12(25,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[50] = z[25] / SCALPR2 * MC_JPGDP

    #      Jet Fuel 5/
    # T12(51,IY,IS)=T12(23,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[51] = z[23] / SCALPR2 * MC_JPGDP

    #      Distillate Fuel Oil
    # T12(52,IY,IS)=T12(22,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[52] = z[22] / SCALPR2 * MC_JPGDP

    #      Residual Fuel Oil (dollars per barrel)
    # T12(53,IY,IS)=T12(27,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[53] = z[27] / SCALPR2 * MC_JPGDP

    #        Average
    # T12(54,IY,IS)=T12(28,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[54] = z[28] / SCALPR2 * MC_JPGDP

    return z
