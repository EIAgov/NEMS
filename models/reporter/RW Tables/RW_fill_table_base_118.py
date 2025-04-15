# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
SZO fixed cross-referencing on 7/10/2024
Updated on 7/30/2024 to fix emissions, allowance bank, and balance
"""

import pandas as pd


def fill_table_base_118(dfd, table_spec, table_id):
    """Fill table  Greenhouse Gas Compliance Results

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
    Other Greenhouse Gases need to be converted by C_CO2_FACTOR.

    """

    z = {}

    FYRPRC = int(table_spec["settings"]["year_index_real_dol_priceyr"])
    # Get inflation index from 1990->2050
    MC_JPGDP = dfd["MC_JPGDP"].loc[1990:]

    # Get inflation index of FYRPRC (2022)
    SCALPR2 = MC_JPGDP.loc[FYRPRC]  # 2022 = column 33

    # MNUMPR=dfd['MNUMPR_rwpre']
    # MNUMNR=dfd['MNUMNR_rwpre']
    # MNUMCR=dfd['MNUMCR_rwpre']
    MNUMPR = int(dfd["MNUMPR_rwpre"])
    MNUMNR = int(dfd["MNUMNR_rwpre"])
    MNUMCR = int(dfd["MNUMCR_rwpre"])
    ALLOW_PER_OFFSET = dfd["ALLOW_PER_OFFSET_rwpre"]
    C_CO2_FACTOR = dfd["C_CO2_FACTOR_rwpre"]

    #   Greenhouse Gas Compliance Results
    #   (million metric tons carbon_dd_)
    #   (#### dollars per million Btu)
    #    Sector and Source
    #   Effect of Allowances on Direct Fuel Cost

    #   Residential

    #      Distillate Fuel Oil
    # T118(4,IY,IS)=ADSRS(MNUMCR,IY)-PDSRS(11,IY)
    z[4] = (dfd["AMPBLK/PDSRS"].loc[MNUMCR] - dfd["MPBLK/PDSRS"].loc[MNUMCR]) * SCALPR2

    #      Propane
    # T118(5,IY,IS)=ALGRS(11,IY)-PLGRS(11,IY)
    z[5] = (dfd["AMPBLK/PLGRS"].loc[MNUMCR] - dfd["MPBLK/PLGRS"].loc[MNUMCR]) * SCALPR2

    #      Natural Gas
    # T118(6,IY,IS)=ANGRS(11,IY)-PNGRS(11,IY)
    z[6] = (dfd["AMPBLK/PNGRS"].loc[MNUMCR] - dfd["MPBLK/PNGRS"].loc[MNUMCR]) * SCALPR2

    #      Electricity
    # T118(7,IY,IS)=AELRS(11,IY)-PELRS(11,IY)
    z[7] = (dfd["AMPBLK/PELRS"].loc[MNUMCR] - dfd["MPBLK/PELRS"].loc[MNUMCR]) * SCALPR2

    #   Commercial
    #      Distillate Fuel Oil
    # T118(11,IY,IS)=ADSCM(11,IY)-PDSCM(11,IY)
    z[11] = (dfd["AMPBLK/PDSCM"].loc[MNUMCR] - dfd["MPBLK/PDSCM"].loc[MNUMCR]) * SCALPR2

    #      Residual Fuel Oil
    # T118(12,IY,IS)=ARSCM(11,IY)-PRSCM(11,IY)
    z[12] = (dfd["AMPBLK/PRSCM"].loc[MNUMCR] - dfd["MPBLK/PRSCM"].loc[MNUMCR]) * SCALPR2

    #      Natural Gas
    # T118(13,IY,IS)=ANGCM(11,IY)-PNGCM(11,IY)
    z[13] = (dfd["AMPBLK/PNGCM"].loc[MNUMCR] - dfd["MPBLK/PNGCM"].loc[MNUMCR]) * SCALPR2

    #      Electricity
    # T118(14,IY,IS)=AELCM(11,IY)-PELCM(11,IY)
    z[14] = (dfd["AMPBLK/PELCM"].loc[MNUMCR] - dfd["MPBLK/PELCM"].loc[MNUMCR]) * SCALPR2

    #   Industrial
    #      Distillate Fuel Oil
    # T118(18,IY,IS)=ADSIN(11,IY)-PDSIN(11,IY)
    z[18] = (dfd["AMPBLK/PDSIN"].loc[MNUMCR] - dfd["MPBLK/PDSIN"].loc[MNUMCR]) * SCALPR2

    #      Propane
    # T118(19,IY,IS)=((ALGIN(11,IY)-PLGIN(11,IY))*(QLGIN(11,IY)-INQLGPF(11,IY)/1000.)+(ALGINPF(11,IY)-PLGINPF(11,IY))*INQLGPF(11,IY)/1000.)/QLGIN(11,IY) ##ALGINPF
    z[19] = SCALPR2 * (
        (
            (dfd["AMPBLK/PLGIN"].loc[MNUMCR] - dfd["MPBLK/PLGIN"].loc[MNUMCR])
            * (dfd["QLGIN"].loc[MNUMCR] - dfd["INQLGPF"].loc[MNUMCR] / 1000.0)
            + (dfd["PLGINPF"].loc[MNUMCR] - dfd["PLGINPF"].loc[MNUMCR])
            * dfd["INQLGPF"].loc[MNUMCR]
            / 1000.0
        )
        / dfd["QLGIN"].loc[MNUMCR]
    )

    #      Residual Fuel Oil
    # T118(20,IY,IS)=ARSIN(11,IY)-PRSIN(11,IY)
    z[20] = (dfd["AMPBLK/PRSIN"].loc[MNUMCR] - dfd["MPBLK/PRSIN"].loc[MNUMCR]) * SCALPR2

    #      Natural Gas
    # T118(21,IY,IS)=ANGIN(11,IY)-PNGIN(11,IY)
    z[21] = (dfd["AMPBLK/PNGIN"].loc[MNUMCR] - dfd["MPBLK/PNGIN"].loc[MNUMCR]) * SCALPR2

    #      Metallurgical Coal
    # T118(22,IY,IS)=AMCIN(11,IY)-PMCIN(11,IY)
    z[22] = (dfd["AMPBLK/PMCIN"].loc[MNUMCR] - dfd["MPBLK/PMCIN"].loc[MNUMCR]) * SCALPR2

    #      Steam Coal
    # T118(23,IY,IS)=ACLIN(11,IY)-PCLIN(11,IY)
    z[23] = (dfd["AMPBLK/PCLIN"].loc[MNUMCR] - dfd["MPBLK/PCLIN"].loc[MNUMCR]) * SCALPR2

    #      Electricity
    # T118(24,IY,IS)=AELIN(11,IY)-PELIN(11,IY)
    z[24] = (dfd["AMPBLK/PELIN"].loc[MNUMCR] - dfd["MPBLK/PELIN"].loc[MNUMCR]) * SCALPR2

    #   Transportation
    #      Motor Gasoline
    # T118(30,IY,IS)=AMGTR(11,IY)-PMGTR(11,IY)
    z[30] = (dfd["AMPBLK/PMGTR"].loc[MNUMCR] - dfd["MPBLK/PMGTR"].loc[MNUMCR]) * SCALPR2

    #      Distillate Fuel Oil
    # T118(28,IY,IS)=ADSTR(11,IY)-PDSTR(11,IY)
    z[28] = (dfd["AMPBLK/PDSTR"].loc[MNUMCR] - dfd["MPBLK/PDSTR"].loc[MNUMCR]) * SCALPR2

    #      Jet Fuel
    # T118(29,IY,IS)=AJFTR(11,IY)-PJFTR(11,IY)
    z[29] = (dfd["AMPBLK/PJFTR"].loc[MNUMCR] - dfd["MPBLK/PJFTR"].loc[MNUMCR]) * SCALPR2

    #      Residual Fuel Oil
    # T118(31,IY,IS)=ARSTR(11,IY)-PRSTR(11,IY)
    z[31] = (dfd["AMPBLK/PRSTR"].loc[MNUMCR] - dfd["MPBLK/PRSTR"].loc[MNUMCR]) * SCALPR2

    #      Natural Gas
    # T118(32,IY,IS)=ANGTR(11,IY)-PNGTR(11,IY)
    z[32] = (dfd["AMPBLK/PNGTR"].loc[MNUMCR] - dfd["MPBLK/PNGTR"].loc[MNUMCR]) * SCALPR2

    #      Electricity
    # T118(33,IY,IS)=AELTR(11,IY)-PELTR(11,IY)
    z[33] = (dfd["AMPBLK/PELTR"].loc[MNUMCR] - dfd["MPBLK/PELTR"].loc[MNUMCR]) * SCALPR2

    #   Electric Power
    #      Distillate Fuel Oil
    # T118(39,IY,IS)=ADSEL(11,IY)-PDSEL(11,IY)
    z[39] = dfd["AMPBLK/PDSEL"].loc[MNUMCR] - dfd["MPBLK/PDSEL"].loc[MNUMCR]

    #      Residual Fuel Oil
    # T118(40,IY,IS)=ARSEL(11,IY)-PRSEL(11,IY)
    z[40] = dfd["AMPBLK/PRSEL"].loc[MNUMCR] - dfd["MPBLK/PRSEL"].loc[MNUMCR]

    #      Natural Gas
    # T118(41,IY,IS)=ANGEL(11,IY)-PNGEL(11,IY)
    z[41] = dfd["AMPBLK/PNGEL"].loc[MNUMCR] - dfd["MPBLK/PNGEL"].loc[MNUMCR]

    #      Steam Coal
    # T118(42,IY,IS)=ACLEL(11,IY)-PCLEL(11,IY)
    z[42] = dfd["AMPBLK/PCLEL"].loc[MNUMCR] - dfd["MPBLK/PCLEL"].loc[MNUMCR]

    #
    #   Emission Prices (#### dollars per metric ton)
    #     Allowance Price for Foresight
    # T118(141,IY,IS)=EMETAX(2,IY)*1000.*SCALPR
    # z[141] = dfd['EMETAX'].loc[2]*1000.* dfd['SCALPR']
    z[141] = dfd["EMETAX"].loc[2] * 1000.0 * SCALPR2

    #     Allowance Price
    # T118(52,IY,IS)=EMETAX(1,IY)*1000.*SCALPR
    # z[52] = dfd['EMETAX'].loc[1]*1000.* dfd['SCALPR']
    z[52] = dfd["EMETAX"].loc[1] * 1000.0 * SCALPR2

    #     Domestic Offset Price
    # T118(55,IY,IS)=GHG_OFFSETP(IY)*1000.*SCALPR
    # z[55] = dfd['GHG_OFFSETP'] * 1000.* dfd['SCALPR']
    z[55] = dfd["GHG_OFFSETP"] * 1000.0 * SCALPR2

    #     International Offset Price
    # T118(53,IY,IS)=GHG_OFFSETPINT(IY)*1000.*SCALPR
    # z[53] = dfd['GHG_OFFSETPINT'] * 1000.* dfd['SCALPR']
    z[53] = dfd["GHG_OFFSETPINT"] * 1000.0 * SCALPR2

    #     Maximum Allowance Price, if applicable
    # T118(138,IY,IS)=EMETAX(3,IY)*1000.*SCALPR
    # z[138] = dfd['EMETAX'].loc[3]*1000.* dfd['SCALPR']
    z[138] = dfd["EMETAX"].loc[3] * 1000.0 * SCALPR2

    #
    #   Allowance Price
    #   (nominal dollars per metric ton)
    # T118(54,IY,IS)=EMETAX(1,IY)*1000.*MC_JPGDP(IY)
    # z[54] = dfd['EMETAX'].loc[1]*1000.* dfd['MC_JPGDP']
    z[54] = dfd["EMETAX"].loc[1] * 1000.0 * MC_JPGDP

    #
    #   Allowance Value (billion #### dollars)
    #     Potential If All Auctioned
    # T118(56,IY,IS)=(EMREV(6,IY)/MC_JPGDP(IY))*SCALPR
    # z[56] = (dfd['EMREV'].loc[6]/ dfd['MC_JPGDP'])* dfd['SCALPR']
    z[56] = (dfd["EMREV"].loc[6]) * SCALPR2

    #     Auctioned Allowances
    # T118(57,IY,IS)=(EMREV(8,IY)/MC_JPGDP(IY))*SCALPR
    # z[57] = (dfd['EMREV'].loc[8]/ dfd['MC_JPGDP'])* dfd['SCALPR']
    z[57] = (dfd["EMREV"].loc[8]) * SCALPR2

    #     Distributed Allowances
    # T118(58,IY,IS)=((EMREV(6,IY)-EMREV(12,IY))/MC_JPGDP(IY))*SCALPR
    # z[58] = ((dfd['EMREV'].loc[6]- dfd['EMREV'].loc[12]) / dfd['MC_JPGDP'])* dfd['SCALPR']
    z[58] = ((dfd["EMREV"].loc[6] - dfd["EMREV"].loc[12])) * SCALPR2

    #       Carbon Capture and Sequestration Bonus
    # T118(59,IY,IS)=(EMREV(9,IY)/MC_JPGDP(IY))*SCALPR
    # z[59] = (dfd['EMREV'].loc[9]/ dfd['MC_JPGDP'])* dfd['SCALPR']
    z[59] = (dfd["EMREV"].loc[9]) * SCALPR2

    #       Other Sequestration Incentives
    # T118(60,IY,IS)=(EMREV(10,IY)/MC_JPGDP(IY))*SCALPR
    # z[60] = (dfd['EMREV'].loc[10]/ dfd['MC_JPGDP'])* dfd['SCALPR']
    z[60] = (dfd["EMREV"].loc[10]) * SCALPR2

    #       Other Allocations
    # T118(61,IY,IS)=T118(58,IY,IS)-T118(59,IY,IS)-T118(60,IY,IS)
    z[61] = z[58] - z[59] - z[60]

    #     Value of Offset Credits and Other Allowances
    #       Noncovered Greenhouse Gases
    # T118(62,IY,IS)=GHG_REV(2,IY)*SCALPR
    # z[62] = dfd['GHG_REV'].loc[2]* dfd['SCALPR']
    z[62] = dfd["GHG_REV"].loc[2] * SCALPR2

    #       Biogenic Sequestration
    # T118(63,IY,IS)=GHG_REV(3,IY)*SCALPR
    # z[63] = dfd['GHG_REV'].loc[3]* dfd['SCALPR']
    z[63] = dfd["GHG_REV"].loc[3] * SCALPR2

    #       International Offset Credits
    #       International Allowances (Sec. 728)
    # T118(154,IY,IS)=OFFSET(4,IY)*EMETAX(1,IY)*SCALPR
    z[154] = dfd["OFFSET"].loc[4] * dfd["EMETAX"].loc[1] * dfd["SCALPR"].iloc[0]
    # T118(64,IY,IS)=GHG_REV(4,IY)*SCALPR-T118(154,IY,IS)
    z[64] = dfd["GHG_REV"].loc[4] * dfd["SCALPR"].iloc[0] - z[154]
    #
    #    GHG Emissions Cap Compliance
    # ! compute covered other ghg as sum of ghg emissions with a ghg classification of 1 (see offsets.wk1 for defn)
    #           covered_oghg=0.
    #           uncovered_oghg=0.
    #           do i=1,ghg_ncat
    #             if(ghg_class(i).eq.1) then
    #               covered_oghg=covered_oghg+ghg_oghg(iy,i)
    #             elseif(ghg_class(i).eq.2.or.ghg_class(i).eq.0) then
    #               uncovered_oghg=uncovered_oghg+ghg_oghg(iy,i)
    #             endif
    #           enddo
    #
    # dict.txt:
    #   GHG_NCat  I 4 1 ( 1) Number of categories of Other GHGes with baselines and MACs
    #   GHG_CLASS I 2 1 (18) Classifier for GHG:
    #       1) covered
    #       2) not covered, domestic,
    #       3) not covered, sequestration
    #       4) not covered, int'l offset
    #
    # covered_oghg
    df = (
        dfd["GHG_CLASS"]
        .to_frame()
        .join(dfd["GHG_OGHG"])
        .groupby("GHGREP/GHG_CLASS")
        .sum()
    )
    if 1 in df.index:
        covered_oghg = df.loc[1]
    else:  # Create a single row of all zeros if it's empty
        num_cols = len(df.columns)
        covered_oghg = pd.DataFrame([[0] * num_cols], columns=df.columns, index=[0])
    #
    # uncovered_oghg
    rows_to_sum = [df.loc[i] for i in [0, 2] if i in df.index]
    if rows_to_sum:
        new_row = pd.Series(sum(rows_to_sum))
        uncovered_oghg = new_row.to_frame().T
    else:  # Create a single row of all zeros if it's empty
        num_cols = len(df.columns)
        uncovered_oghg = pd.DataFrame([[0] * num_cols], columns=df.columns, index=[0])

    #    (million metric tons _cc_ equivalent)

    #     Covered Energy-Related Carbon Dioxide
    # T118(65,IY,IS)=EMSOL(1,IY)-covered_oghg
    # z[65]= z[64]*0.0
    z[65] = (
        dfd["EMSOL"].loc[1] - covered_oghg
    ) * C_CO2_FACTOR  # oghg is computed - put in preprocessor?

    #     Covered Other GHG Emissions
    # T118(66,IY,IS)=COVERED_OGHG
    # z[66]= z[65]*0.0 #placeholder
    z[66] = covered_oghg * C_CO2_FACTOR

    #       Total Covered Emissions
    # T118(148,IY,IS)=EMSOL(1,IY)
    z[148] = dfd["EMSOL"].loc[1] * C_CO2_FACTOR

    #     Offset Limits

    #       Offset Limit, percent of Covered Emissions
    # T118(142,IY,IS)=OFF_LIMITS(1,IY)
    z[142] = dfd["OFF_LIMITS"].loc[1] * C_CO2_FACTOR

    #       Maximum Offsets, Given Covered Emissions
    # T118(143,IY,IS)=OFF_LIMITS(2,IY)
    z[143] = dfd["OFF_LIMITS"].loc[2] * C_CO2_FACTOR

    #       Maximum Domestic Offsets
    # T118(144,IY,IS)=OFF_LIMITS(3,IY)
    z[144] = dfd["OFF_LIMITS"].loc[3] * C_CO2_FACTOR

    #       Initial International Offset Limit
    # T118(145,IY,IS)=OFF_LIMITS(3,IY)
    z[145] = dfd["OFF_LIMITS"].loc[3] * C_CO2_FACTOR

    #       Adjusted International Offset Limit
    # T118(146,IY,IS)=OFF_LIMITS(4,IY)
    z[146] = dfd["OFF_LIMITS"].loc[4] * C_CO2_FACTOR

    #     Offsets and International Allowances
    # T118(133,IY,IS)=OFFSET(1,IY)+OFFSET(2,IY)+ALLOW_PER_OFFSET*OFFSET(3,IY)+OFFSET(4,IY)
    z[133] = (
        dfd["OFFSET"].loc[1]
        + dfd["OFFSET"].loc[2]
        + ALLOW_PER_OFFSET * dfd["OFFSET"].loc[3]
        + dfd["OFFSET"].loc[4]
    ) * C_CO2_FACTOR

    #       Noncovered Gases
    # T118(67,IY,IS)=OFFSET(1,IY)
    z[67] = dfd["OFFSET"].loc[1] * C_CO2_FACTOR

    #       Biogenic Sequestration
    # T118(68,IY,IS)=OFFSET(2,IY)
    z[68] = dfd["OFFSET"].loc[2] * C_CO2_FACTOR

    #         Total Domestic Offsets
    # T118(147,IY,IS)=OFFSET(1,IY)+OFFSET(2,IY)
    z[147] = (dfd["OFFSET"].loc[1] + dfd["OFFSET"].loc[2]) * C_CO2_FACTOR

    #       International Offsets (Discounted 2018-on)
    # T118(69,IY,IS)=OFFSET(3,IY)*ALLOW_PER_OFFSET
    z[69] = dfd["OFFSET"].loc[3] * ALLOW_PER_OFFSET * C_CO2_FACTOR

    #       International Allowances (Sec. 728)
    # T118(140,IY,IS)=OFFSET(4,IY)
    z[140] = dfd["OFFSET"].loc[4] * C_CO2_FACTOR

    #     Covered Emissions less Offsets and Allowances
    # T118(70,IY,IS)=EMSOL(1,IY)-(OFFSET(1,IY)+OFFSET(2,IY)+ALLOW_PER_OFFSET*OFFSET(3,IY)+OFFSET(4,IY))
    z[70] = (
        dfd["EMSOL"].loc[1]
        - (
            dfd["OFFSET"].loc[1]
            + dfd["OFFSET"].loc[2]
            + ALLOW_PER_OFFSET * dfd["OFFSET"].loc[3]
            + dfd["OFFSET"].loc[4]
        )
    ) * C_CO2_FACTOR

    #     Covered Emissions Goal
    # T118(71,IY,IS)=EMLIM(1,IY)
    z[71] = dfd["EMLIM"].loc[1] * C_CO2_FACTOR

    #     Allowance Banking (borrowing)
    # T118(72,IY,IS)=BANK
    z[72] = dfd[
        "BANKING"
    ]  # bnk is computed in the covered_ghg function in the preprocessor

    #     Cumulative Bank Balance
    # T118(73,IY,IS)=BALANCE(IY)
    z[73] = dfd["BALANCE"]

    #     TAP Purchases
    # T118(138,IY,IS)=EMETAX(3,IY)*1000.*SCALPR
    z[139] = dfd["EMETAX"].loc[3] * 1000.0 * dfd["SCALPR"].iloc[0]

    #   Total Energy-CO2 Emissions
    # T118(74,IY,IS)=T17(30,11,IY,IS)
    # Create a place holder and crosss-reference T17 in postprocessor_base
    z[74] = z[139] * 0
    # print(z[74])

    #   Total Other Greenhouse Gases
    # T118(75,IY,IS)=COVERED_OGHG+uncovered_oghg
    # z[75] = pd.concat([COVERED_OGHG, uncovered_oghg], ignore_index=True).sum() # preprocessor
    z[75] = (covered_oghg + uncovered_oghg) * C_CO2_FACTOR

    #   Total GHG Emissions
    # T118(76,IY,IS)=SUM(T118(74:75,IY,IS))
    # Create a place holder and crosss-reference T17 in postprocessor_base
    z[76] = z[75].copy()

    #   Total GHGs less Bio. Seq. & Int'l Reductions
    # T118(153,IY,IS)=T118(76,IY,IS)-SUM(OFFSET(2:4,IY))
    z[153] = -dfd["OFFSET"].loc[2:4, :].sum()

    #   Carbon Capture and Storage
    # T118(134,IY,IS)=EMCARBON(6,MNUMNR,IY)+SUM(CCS_PMM(1:4,MNUMPR,IY))
    z[134] = (
        (
            dfd["EMCARBON"].loc[(6, MNUMNR), :]
            + dfd["CCS_PMM"].loc[(slice(1, 4), MNUMPR), :].sum()
        ) * C_CO2_FACTOR
    )

    #     Electric Power Sector
    #       Petroleum
    # T118(135,IY,IS)=SUM(EMCARBON(1:2,MNUMNR,IY))
    z[135] = dfd["EMCARBON"].loc[(slice(1, 2), MNUMNR), :].sum() * C_CO2_FACTOR

    #       Natural Gas
    # T118(136,IY,IS)=EMCARBON(4,MNUMNR,IY)
    z[136] = dfd["EMCARBON"].loc[(4, MNUMNR), :] * C_CO2_FACTOR

    #       Coal
    # T118(137,IY,IS)=EMCARBON(3,MNUMNR,IY)
    z[137] = dfd["EMCARBON"].loc[(3, MNUMNR), :] * C_CO2_FACTOR

    #       Biomass
    # T118(132,IY,IS)=EMCARBON(5,MNUMNR,IY)
    z[132] = dfd["EMCARBON"].loc[(5, MNUMNR), :] * C_CO2_FACTOR

    #     Refining/Other
    #       Still Gas
    # T118(152,IY,IS)=CCS_PMM(4,MNUMPR,IY)
    z[152] = dfd["CCS_PMM"].loc[(4, MNUMPR), :] * C_CO2_FACTOR

    #       Natural Gas
    # T118(151,IY,IS)=CCS_PMM(3,MNUMPR,IY)
    z[151] = dfd["CCS_PMM"].loc[(3, MNUMPR), :] * C_CO2_FACTOR

    #       Coal
    # T118(150,IY,IS)=CCS_PMM(2,MNUMPR,IY)
    z[150] = dfd["CCS_PMM"].loc[(2, MNUMPR), :] * C_CO2_FACTOR

    #       Biomass
    # T118(149,IY,IS)=CCS_PMM(1,MNUMPR,IY)
    z[149] = dfd["CCS_PMM"].loc[(1, MNUMPR), :] * C_CO2_FACTOR

    #   Other Greenhouse Gases
    #    (million metric tons _cc_ equivalent)
    #     Baseline Emissions Assumed

    #       Methane
    # T118(77,IY,IS)=SUM(GHG_BL(IY,1:5))
    z[77] = dfd["GHG_BL"].loc[1:5, :].sum() * C_CO2_FACTOR

    #         Landfills
    # T118(78,IY,IS)=GHG_BL(IY,1)
    z[78] = dfd["GHG_BL"].loc[1] * C_CO2_FACTOR

    #         Coal Mining
    # T118(79,IY,IS)=GHG_BL(IY,2)
    z[79] = dfd["GHG_BL"].loc[2] * C_CO2_FACTOR

    #         Natural Gas and Oil Systems
    # T118(80,IY,IS)=GHG_BL(IY,3)
    z[80] = dfd["GHG_BL"].loc[3] * C_CO2_FACTOR

    #         Stationary Combustion and Mobile Sources
    # T118(81,IY,IS)=GHG_BL(IY,4)
    z[81] = dfd["GHG_BL"].loc[4] * C_CO2_FACTOR

    #         Other
    # T118(82,IY,IS)=GHG_BL(IY,5)
    z[82] = dfd["GHG_BL"].loc[5] * C_CO2_FACTOR

    #       Non-Energy Carbon Dioxide
    # T118(83,IY,IS)=GHG_BL(IY,6)
    z[83] = dfd["GHG_BL"].loc[6] * C_CO2_FACTOR

    #       Nitrous Oxide
    # T118(84,IY,IS)=SUM(GHG_BL(IY,7:10))
    z[84] = dfd["GHG_BL"].loc[7:10, :].sum() * C_CO2_FACTOR

    #         Agriculture
    # T118(85,IY,IS)=GHG_BL(IY,7)
    z[85] = dfd["GHG_BL"].loc[7] * C_CO2_FACTOR

    #         Stationary and Mobile Combustion
    # T118(86,IY,IS)=GHG_BL(IY,8)
    z[86] = dfd["GHG_BL"].loc[8] * C_CO2_FACTOR

    #         Adipic and Nitric Acid Production
    # T118(87,IY,IS)=GHG_BL(IY,9)
    z[87] = dfd["GHG_BL"].loc[9] * C_CO2_FACTOR

    #         Other
    # T118(88,IY,IS)=GHG_BL(IY,10)
    z[88] = dfd["GHG_BL"].loc[10] * C_CO2_FACTOR

    #       Fluorinated Gases
    # T118(89,IY,IS)=SUM(GHG_BL(IY,11:14))
    z[89] = dfd["GHG_BL"].loc[11:14, :].sum() * C_CO2_FACTOR

    #         HFC-23
    # T118(90,IY,IS)=GHG_BL(IY,11)
    z[90] = dfd["GHG_BL"].loc[11] * C_CO2_FACTOR

    #         Other HFCs and ODS Substitutes
    # T118(91,IY,IS)=GHG_BL(IY,12)
    z[91] = dfd["GHG_BL"].loc[12] * C_CO2_FACTOR

    #         PFCs
    # T118(92,IY,IS)=GHG_BL(IY,13)
    z[92] = dfd["GHG_BL"].loc[13] * C_CO2_FACTOR

    #         SF6
    # T118(93,IY,IS)=GHG_BL(IY,14)
    z[93] = dfd["GHG_BL"].loc[14] * C_CO2_FACTOR

    #       Total Baseline
    # T118(94,IY,IS)=SUM(GHG_BL(IY,1:14))
    z[94] = dfd["GHG_BL"].loc[1:14, :].sum() * C_CO2_FACTOR

    #     Abatement and Offsets
    #       Methane
    # T118(95,IY,IS)=SUM(GHG_ABATE(IY,1:5))
    z[95] = dfd["GHG_ABATE"].loc[1:5, :].sum() * C_CO2_FACTOR

    #         Landfills
    # T118(96,IY,IS)=GHG_ABATE(IY,1)
    z[96] = dfd["GHG_ABATE"].loc[1] * C_CO2_FACTOR

    #         Coal Mining
    # T118(97,IY,IS)=GHG_ABATE(IY,2)
    z[97] = dfd["GHG_ABATE"].loc[2] * C_CO2_FACTOR

    #         Natural Gas and Oil Systems
    # T118(98,IY,IS)=GHG_ABATE(IY,3)
    z[98] = dfd["GHG_ABATE"].loc[3] * C_CO2_FACTOR

    #         Stationary Combustion and Mobile Sources
    # T118(99,IY,IS)=GHG_ABATE(IY,4)
    z[99] = dfd["GHG_ABATE"].loc[4] * C_CO2_FACTOR

    #         Other
    # T118(100,IY,IS)=GHG_ABATE(IY,5)
    z[100] = dfd["GHG_ABATE"].loc[5] * C_CO2_FACTOR

    #       Non-Energy Carbon Dioxide
    # T118(101,IY,IS)=GHG_ABATE(IY,6)
    z[101] = dfd["GHG_ABATE"].loc[6] * C_CO2_FACTOR

    #       Nitrous Oxide
    # T118(102,IY,IS)=SUM(GHG_ABATE(IY,7:10))
    z[102] = dfd["GHG_ABATE"].loc[7:10, :].sum() * C_CO2_FACTOR

    #         Agriculture
    # T118(103,IY,IS)=GHG_ABATE(IY,7)
    z[103] = dfd["GHG_ABATE"].loc[7] * C_CO2_FACTOR

    #         Stationary and Mobile Combustion
    # T118(104,IY,IS)=GHG_ABATE(IY,8)
    z[104] = dfd["GHG_ABATE"].loc[8] * C_CO2_FACTOR

    #         Adipic and Nitric Acid Production
    # T118(105,IY,IS)=GHG_ABATE(IY,9)
    z[105] = dfd["GHG_ABATE"].loc[9] * C_CO2_FACTOR

    #         Other
    # T118(106,IY,IS)=GHG_ABATE(IY,10)
    z[106] = dfd["GHG_ABATE"].loc[10] * C_CO2_FACTOR

    #       Fluorinated Gases
    # T118(107,IY,IS)=SUM(GHG_ABATE(IY,11:14))
    z[107] = dfd["GHG_ABATE"].loc[11:14, :].sum() * C_CO2_FACTOR

    #         HFC-23
    # T118(108,IY,IS)=GHG_ABATE(IY,11)
    z[108] = dfd["GHG_ABATE"].loc[11] * C_CO2_FACTOR

    #         Other HFCs and ODS Substitutes
    # T118(109,IY,IS)=GHG_ABATE(IY,12)
    z[109] = dfd["GHG_ABATE"].loc[12] * C_CO2_FACTOR

    #         PFCs
    # T118(110,IY,IS)=GHG_ABATE(IY,13)
    z[110] = dfd["GHG_ABATE"].loc[13] * C_CO2_FACTOR

    #         SF6
    # T118(111,IY,IS)=GHG_ABATE(IY,14)
    z[111] = dfd["GHG_ABATE"].loc[14] * C_CO2_FACTOR

    #       Total Reductions
    # T118(112,IY,IS)=SUM(GHG_ABATE(IY,1:14))
    # z[112] = dfd['GHG_ABATE'].loc[1:14]
    z[112] = dfd["GHG_ABATE"].loc[1:14].sum() * C_CO2_FACTOR

    #     Resulting Emissions
    #       Methane
    # T118(113,IY,IS)=SUM(GHG_OGHG(IY,1:5))
    # z[113] = dfd['GHG_OGHG'].loc[1:5,:].sum()
    z[113] = (
        dfd["GHG_OGHG"].loc[1]
        + dfd["GHG_OGHG"].loc[2]
        + dfd["GHG_OGHG"].loc[3]
        + dfd["GHG_OGHG"].loc[4]
        + dfd["GHG_OGHG"].loc[5]
    ) * C_CO2_FACTOR

    #         Landfills
    # T118(114,IY,IS)=GHG_OGHG(IY,1)
    z[114] = dfd["GHG_OGHG"].loc[1] * C_CO2_FACTOR

    #         Coal Mining
    # T118(115,IY,IS)=GHG_OGHG(IY,2)
    z[115] = dfd["GHG_OGHG"].loc[2] * C_CO2_FACTOR

    #         Natural Gas and Oil Systems
    # T118(116,IY,IS)=GHG_OGHG(IY,3)
    z[116] = dfd["GHG_OGHG"].loc[3] * C_CO2_FACTOR

    #         Stationary Combustion and Mobile Sources
    # T118(117,IY,IS)=GHG_OGHG(IY,4)
    z[117] = dfd["GHG_OGHG"].loc[4] * C_CO2_FACTOR

    #         Other
    # T118(118,IY,IS)=GHG_OGHG(IY,5)
    z[118] = dfd["GHG_OGHG"].loc[5] * C_CO2_FACTOR

    #       Non-Energy Carbon Dioxide
    # T118(119,IY,IS)=GHG_OGHG(IY,6)
    z[119] = dfd["GHG_OGHG"].loc[6] * C_CO2_FACTOR

    #       Nitrous Oxide
    # T118(120,IY,IS)=SUM(GHG_OGHG(IY,7:10))
    z[120] = dfd["GHG_OGHG"].loc[7:10, :].sum() * C_CO2_FACTOR

    #         Agriculture
    # T118(121,IY,IS)=GHG_OGHG(IY,7)
    z[121] = dfd["GHG_OGHG"].loc[7] * C_CO2_FACTOR

    #         Stationary and Mobile Combustion
    # T118(122,IY,IS)=GHG_OGHG(IY,8)
    z[122] = dfd["GHG_OGHG"].loc[8] * C_CO2_FACTOR

    #         Adipic and Nitric Acid Production
    # T118(123,IY,IS)=GHG_OGHG(IY,9)
    z[123] = dfd["GHG_OGHG"].loc[9] * C_CO2_FACTOR

    #         Other
    # T118(124,IY,IS)=GHG_OGHG(IY,10)
    z[124] = dfd["GHG_OGHG"].loc[10] * C_CO2_FACTOR

    #       Fluorinated Gases
    # T118(125,IY,IS)=SUM(GHG_OGHG(IY,11:14))
    z[125] = dfd["GHG_OGHG"].loc[11:14, :].sum() * C_CO2_FACTOR

    #         HFC-23
    # T118(126,IY,IS)=GHG_OGHG(IY,11)
    z[126] = dfd["GHG_OGHG"].loc[11] * C_CO2_FACTOR

    #         Other HFCs and ODS Substitutes
    # T118(127,IY,IS)=GHG_OGHG(IY,12)
    z[127] = dfd["GHG_OGHG"].loc[12] * C_CO2_FACTOR

    #         PFCs
    # T118(128,IY,IS)=GHG_OGHG(IY,13)
    z[128] = dfd["GHG_OGHG"].loc[13] * C_CO2_FACTOR

    #         SF6
    # T118(129,IY,IS)=GHG_OGHG(IY,14)
    z[129] = dfd["GHG_OGHG"].loc[14] * C_CO2_FACTOR

    #       Total Other GHG Emissions
    # T118(130,IY,IS)=SUM(GHG_OGHG(IY,1:14))
    # z[130] = dfd['GHG_OGHG'].loc[1:14]
    z[130] = dfd["GHG_OGHG"].loc[1:14].sum() * C_CO2_FACTOR

    #   California AB32 Representation
    #     Allowances Issued
    # T118(155,IY,IS)=AB_CAP_TOT(IY)
    z[155] = dfd["AB_CAP_TOT"] * C_CO2_FACTOR

    #     Allowances Allocated to Containment Reserve
    # T118(156,IY,IS)=AB_CAP_TOT(IY)*AB_CSTCONT_FRAC(IY)
    z[156] = dfd["AB_CAP_TOT"] * dfd["AB_CSTCONT_FRAC"] * C_CO2_FACTOR

    #       Allowances Net of Reserve Allocation
    # T118(157,IY,IS)=T118(155,IY,IS)-T118(156,IY,IS)
    z[157] = z[155] - z[156]

    #     Allowance Bank Balance
    # T118(158,IY,IS)=AB_ALLBANK_AVL(IY)
    z[158] = dfd["AB_ALLBANK_AVL"] * C_CO2_FACTOR

    #     Containment Reserve Balance
    # T118(159,IY,IS)=AB_CSTCONT_AVL(IY)
    z[159] = dfd["AB_CSTCONT_AVL"] * C_CO2_FACTOR

    #     Maximum Offsets Available
    # T118(160,IY,IS)=AB_CAP_TOT(IY)*AB_OFFSET_FRAC(IY)
    z[160] = dfd["AB_CAP_TOT"] * dfd["AB_OFFSET_FRAC"] * C_CO2_FACTOR

    #     Covered Emissions
    #       Electric Power Sector
    # T118(161,IY,IS)=AB_COVD_EM_ELE(IY)
    z[161] = dfd["AB_COVD_EM_ELE"] * C_CO2_FACTOR

    #       Refining
    # T118(162,IY,IS)=AB_COVD_EM_REF(IY)
    z[162] = dfd["AB_COVD_EM_REF"] * C_CO2_FACTOR

    #       Industrial Energy-related
    # T118(163,IY,IS)=AB_COVD_EM_IND(IY)
    z[163] = dfd["AB_COVD_EM_IND"] * C_CO2_FACTOR

    #       Fuel Providers
    # T118(164,IY,IS)=AB_COVD_EM_FUE(IY)
    z[164] = dfd["AB_COVD_EM_FUE"] * C_CO2_FACTOR

    #       Other (non-CO2 or not modeled explicitly)
    # T118(165,IY,IS)=AB_COVD_EM_OTH(IY)
    z[165] = dfd["AB_COVD_EM_OTH"] * C_CO2_FACTOR

    #         Total Covered Emissions
    # T118(166,IY,IS)=AB_COVD_EM_TOT(IY)
    z[166] = dfd["AB_COVD_EM_TOT"] * C_CO2_FACTOR

    #     Allowance Bank Activity
    # T118(167,IY,IS)=AB_ALLBANK_AVL(IY)-AB_ALLBANK_AVL(IY-1)
    # z[167]=(dfd['AB_ALLBANK_AVL'] - dfd['AB_ALLBANK_AVL'])* C_CO2_FACTOR #placeholder
    # z[167] = dfd['AB_ALLBANK_AVL'] - dfd['AB_ALLBANK_AVL'].loc[(IY-1)] # <check> how do you do lags?
    z[167] = dfd["AB_ALLBANK_AVL"] * C_CO2_FACTOR
    z[167] = z[167].diff().fillna(0)  # Differences between consecutive rows

    #     Containment Reserves Purchased
    # T118(168,IY,IS)=AB_CSTCONT_USE(IY)
    z[168] = dfd["AB_CSTCONT_USE"] * C_CO2_FACTOR

    #     Offsets Used
    # T118(169,IY,IS)=AB_OFFSET_USED(IY)
    z[169] = dfd["AB_OFFSET_USED"] * C_CO2_FACTOR

    #     Balance (Allw-Bnk+Off+Rsv-CovEmis)
    # T118(170,IY,IS)=T118(157,IY,IS)-T118(167,IY,IS)+T118(168,IY,IS)+T118(169,IY,IS)-T118(166,IY,IS)
    z[170] = z[157] + z[168] + z[169] - z[166] - z[167]

    #     Allowance Price
    # T118(171,IY,IS)=AB_ALLOW_P(IY)*1000.*SCALPR
    # z[171] = dfd['AB_ALLOW_P'] * 1000.* dfd['SCALPR']
    z[171] = dfd["AB_ALLOW_P"] * 1000.0 * SCALPR2 / C_CO2_FACTOR

    #     Cost Containment Trigger Price
    # T118(172,IY,IS)=AB_RESERVE_P(2,IY)*1000.*SCALPR
    z[172] = dfd["AB_RESERVE_P"].loc[2] * 1000.0 * SCALPR2 / C_CO2_FACTOR

    #   Regional Greenhouse Gas Initiative Representation

    #     Allowances (Adjusted) Issued
    # T118(173,IY,IS)=RG_CAP_TOT(IY)
    z[173] = dfd["RG_CAP_TOT"] * C_CO2_FACTOR

    #     Allocated to Emissions Containment Reserve
    # T118(185,IY,IS)=RG_RSRVECR_AVL(IY)
    z[185] = dfd["RG_RSRVECR_AVL"] * C_CO2_FACTOR

    #       Allowances Net of Reserve Allocation
    # T118(186,IY,IS)=T118(173,IY,IS)-T118(185,IY,IS)
    z[186] = z[173] - z[185]

    #     Allowance Bank Balance
    # T118(174,IY,IS)=RG_ALLBANK_AVL(IY)
    z[174] = dfd["RG_ALLBANK_AVL"] * C_CO2_FACTOR

    #     Emis Containment Reserve Allowances Available
    # T118(187,IY,IS)=RG_RSRVECR_AVL(IY)
    z[187] = dfd["RG_RSRVECR_AVL"] * C_CO2_FACTOR

    #     Cost Containment Reserve Allowances Available
    # T118(175,IY,IS)=RG_RESERVE_AVL(IY)
    z[175] = dfd["RG_RESERVE_AVL"] * C_CO2_FACTOR

    #     Maximum Offsets Available
    # T118(176,IY,IS)=RG_OFFSETS_AVL(IY)
    z[176] = dfd["RG_OFFSETS_AVL"] * C_CO2_FACTOR

    #     Electric Power Sector Emissions
    # T118(177,IY,IS)=RG_COVD_EM_ELE(IY)
    z[177] = dfd["RG_COVD_EM_ELE"] * C_CO2_FACTOR

    #     Allowance Banking (borrowing)
    # T118(178,IY,IS)=RG_ALLBANK_USE(IY)
    z[178] = dfd["RG_ALLBANK_USE"] * C_CO2_FACTOR

    #     Emis Containment Reserve Allowances Purchased
    # T118(188,IY,IS)=RG_RSRVECR_USE(IY)
    z[188] = dfd["RG_RSRVECR_USE"] * C_CO2_FACTOR

    #     Cost Containment Reserve Allowances Purchased
    # T118(179,IY,IS)=RG_RESERVE_USE(IY)
    z[179] = dfd["RG_RESERVE_USE"] * C_CO2_FACTOR

    #     Offsets Used
    # T118(180,IY,IS)=RG_OFFSETS_USE(IY)
    z[180] = dfd["RG_OFFSETS_USE"] * C_CO2_FACTOR

    #     Balance (Allw-Bnk+Off+ECR+CCR-CovEmis)
    # T118(181,IY,IS)=T118(173,IY,IS)-T118(178,IY,IS)+T118(179,IY,IS)+T118(188,IY,IS)+T118(180,IY,IS)-T118(177,IY,IS)
    z[181] = z[173] - z[178] + z[179] + z[188] + z[180] - z[177]

    #     Allowance Price
    # T118(182,IY,IS)=RG_ALLOW_P(IY)
    z[182] = dfd["RG_ALLOW_P"] * SCALPR2 / C_CO2_FACTOR

    #     Floor Price
    # T118(183,IY,IS)=RG_AUCTION_P(IY)
    z[183] = dfd["RG_AUCTION_P"] * SCALPR2 / C_CO2_FACTOR

    #     Emission Containment Reserve Price
    # T118(189,IY,IS)=RG_RSRVECR_P(IY)
    z[189] = dfd["RG_RSRVECR_P"] * SCALPR2 / C_CO2_FACTOR

    #     Cost Containment Reserve Price
    # T118(184,IY,IS)=RG_RESERVE_P(IY)
    z[184] = dfd["RG_RESERVE_P"] * SCALPR2 / C_CO2_FACTOR

    return z
