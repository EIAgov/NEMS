# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
SZO fixed on 7/8/2024
"""
import numpy as np
import pandas as pd


def fill_table_base_003(dfd, table_spec, table_id):
    """Fill table Energy Prices by Sector and Source.

    The function returns a dict in which each integer key
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

    MNUMCR = dfd["MNUMCR_rwpre"]

    # FYRPRC = int(table_spec['year_index_real_dol_priceyr'])
    FYRPRC = int(table_spec["ftab_dat"]["FYRPRC"])

    # Get inflation index from 1990->2050
    MC_JPGDP = dfd["MC_JPGDP"].loc[1990:]

    # Get inflation index of FYRPRC (2022)
    SCALPR2 = MC_JPGDP.loc[FYRPRC]

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

    # Residential
    #   Propane              T3(5,IR,IY,IS)=PPRRS(IR,IY)
    #   Distillate Fuel Oil  T3(4,IR,IY,IS)=PDSRS(IR,IY)
    #   Natural Gas          T3(6,IR,IY,IS)=PNGRS(IR,IY)
    #   Electricity          T3(7,IR,IY,IS)=PELRS(IR,IY)
    z[1] = (
        (
            dfd["AMPBLK/PTPRS"] * dfd["QTPRS"]
            + dfd["AMPBLK/PNGRS"] * dfd["QNGRS"]
            + dfd["AMPBLK/PCLRS"] * dfd["QCLRS"]
            + dfd["AMPBLK/PELRS"] * dfd["QELRS"]
        )
        / (dfd["QTPRS"] + dfd["QNGRS"] + dfd["QCLRS"] + dfd["QELRS"])
    ) * SCALPR2
    # if((dfd['QTPRS']+dfd['QNGRS']+dfd['QCLRS']) != 0.0)
    z[2] = (
        (
            dfd["AMPBLK/PTPRS"] * dfd["QTPRS"]
            + dfd["AMPBLK/PNGRS"] * dfd["QNGRS"]
            + dfd["AMPBLK/PCLRS"] * dfd["QCLRS"]
        )
        / (dfd["QTPRS"] + dfd["QNGRS"] + dfd["QCLRS"])
    ) * SCALPR2
    z[5] = dfd["APMORE/PPRRS"] * SCALPR2
    z[4] = dfd["AMPBLK/PDSRS"] * SCALPR2
    z[6] = dfd["AMPBLK/PNGRS"] * SCALPR2
    z[7] = dfd["AMPBLK/PELRS"] * SCALPR2
    # if ((dfd['QTPCM']+dfd['QNGCM']+dfd['QCLCM']+dfd['QELCM']) != 0.0)
    z[8] = (
        (
            dfd["AMPBLK/PTPCM"] * dfd["QTPCM"]
            + dfd["AMPBLK/PNGCM"] * dfd["QNGCM"]
            + dfd["AMPBLK/PCLCM"] * dfd["QCLCM"]
            + dfd["AMPBLK/PELCM"] * dfd["QELCM"]
        )
        / (dfd["QTPCM"] + dfd["QNGCM"] + dfd["QCLCM"] + dfd["QELCM"])
    ) * SCALPR2
    # if ((dfd['QTPCM']+dfd['QNGCM']+dfd['QCLCM']) != 0.0)
    z[9] = (
        (
            dfd["AMPBLK/PTPCM"] * dfd["QTPCM"]
            + dfd["AMPBLK/PNGCM"] * dfd["QNGCM"]
            + dfd["AMPBLK/PCLCM"] * dfd["QCLCM"]
        )
        / (dfd["QTPCM"] + dfd["QNGCM"] + dfd["QCLCM"])
    ) * SCALPR2
    #z[10] = dfd["AMPBLK/PTPCM"] * SCALPR2
    # Commercial
    #   Propane              T3(125,IR,IY,IS)=PPRCM(IR,IY)
    #   Distillate Fuel Oil  T3(11,IR,IY,IS)=PDSCM(IR,IY)
    #   Residual Fuel Oil    T3(12,IR,IY,IS)=PRSCM(IR,IY)
    #   Natural Gas          T3(13,IR,IY,IS)=PNGCM(IR,IY)
    #   Electricity          T3(14,IR,IY,IS)=PELCM(IR,IY)
    z[125] = dfd["APMORE/PPRCM"] * SCALPR2
    z[11] = dfd["AMPBLK/PDSCM"] * SCALPR2
    z[12] = dfd["AMPBLK/PRSCM"] * SCALPR2
    z[13] = dfd["AMPBLK/PNGCM"] * SCALPR2
    z[14] = dfd["AMPBLK/PELCM"] * SCALPR2
    # if((dfd['QTPIN']+dfd['QNGIN']+dfd['QCLIN']+dfd['QELIN']+dfd['QMCIN']) != 0.0)
    z[15] = (
        (
            dfd["AMPBLK/PTPIN"] * dfd["QTPIN"]
            + dfd["AMPBLK/PNGIN"] * dfd["QNGIN"]
            + dfd["AMPBLK/PCLIN"] * (dfd["QCLIN"])
            + dfd["AMPBLK/PELIN"] * dfd["QELIN"]
            + dfd["AMPBLK/PMCIN"] * dfd["QMCIN"]
        )
        / (dfd["QTPIN"] + dfd["QNGIN"] + dfd["QCLIN"] + dfd["QELIN"] + dfd["QMCIN"])
    ) * SCALPR2
    # if((dfd['QTPIN']+dfd['QNGIN']+dfd['QCLIN']+dfd['QMCIN']) != 0.0)
    z[16] = (
        (
            dfd["AMPBLK/PTPIN"] * dfd["QTPIN"]
            + dfd["AMPBLK/PNGIN"] * dfd["QNGIN"]
            + dfd["AMPBLK/PCLIN"] * (dfd["QCLIN"])
            + dfd["AMPBLK/PMCIN"] * dfd["QMCIN"]
        )
        / (dfd["QTPIN"] + dfd["QNGIN"] + dfd["QCLIN"] + dfd["QMCIN"])
    ) * SCALPR2
    z[17] = dfd["AMPBLK/PTPIN"] * SCALPR2
    z[18] = dfd["AMPBLK/PDSIN"] * SCALPR2
    # Industrial
    #   Propane                T3(19,IR,IY,IS)=PPRIN(IR,IY)
    #   Distillate Fuel Oil    T3(18,IR,IY,IS)=PDSIN(IR,IY)
    #   Residual Fuel Oil      T3(20,IR,IY,IS)=PRSIN(IR,IY)
    #   Natural Gas 2/         T3(21,IR,IY,IS)=PNGIN(IR,IY)
    #   Metallurgical Coal     T3(22,IR,IY,IS)=PMCIN(IR,IY)
    #   Other Industrial Coal  T3(23,IR,IY,IS)=PCLIN(IR,IY)
    #   Electricity            T3(24,IR,IY,IS)=PELIN(IR,IY)
    #   Biomass                T3(46,IR,IY,IS)=0.0
    z[19] = dfd["APMORE/PPRIN"] * SCALPR2
    z[18] = dfd["AMPBLK/PDSIN"] * SCALPR2
    z[20] = dfd["AMPBLK/PRSIN"] * SCALPR2
    z[21] = dfd["AMPBLK/PNGIN"] * SCALPR2
    z[22] = dfd["AMPBLK/PMCIN"] * SCALPR2
    z[23] = dfd["AMPBLK/PCLIN"] * SCALPR2
    z[3] = dfd["PH2IN"] * SCALPR2
    z[24] = dfd["AMPBLK/PELIN"] * SCALPR2
    z[46] = z[24] * 0.0

    # Transportation
    #   Propane                              T3(32,IR,IY,IS)=PPRTR(IR,IY)
    #   E85 3/                               T3(34,IR,IY,IS)=PETTR(IR,IY)
    #   Motor Gasoline 4/                    T3(30,IR,IY,IS)=PMGTR(IR,IY)
    #   Jet Fuel 5/                          T3(29,IR,IY,IS)=PJFTR(IR,IY)
    #   Diesel Fuel (distillate fuel oil) 6/ T3(28,IR,IY,IS)=PDSTRHWY(IR,IY)
    #   Residual Fuel Oil                    T3(31,IR,IY,IS)=PRSTR(IR,IY)
    #   Natural Gas 7/                       T3(33,IR,IY,IS)=PNGTR(IR,IY)
    #   Electricity                          T3(36,IR,IY,IS)=PELTR(IR,IY)

    # if ((dfd['QTPTR']-dfd['QDSBS']-dfd['QJFBS']-dfd['QMGBS']+dfd['QNGTR']+dfd['QELTR']+dfd['QETTR'])!= 0.0)
    z[25] = (
        (
            dfd["AMPBLK/PTPTR"]
            * (dfd["QTPTR"] - dfd["QDSBS"] - dfd["QJFBS"] - dfd["QMGBS"])
            + dfd["AMPBLK/PNGTR"] * dfd["QNGTR"]
            + dfd["AMPBLK/PELTR"] * dfd["QELTR"]
            + dfd["AMPBLK/PETTR"] * dfd["QTRTR"]
        )
        / (
            dfd["QTPTR"]
            - dfd["QDSBS"]
            - dfd["QJFBS"]
            - dfd["QMGBS"]
            + dfd["QNGTR"]
            + dfd["QELTR"]
            + dfd["QETTR"]
        )
    ) * SCALPR2
    # if ((dfd['QTPTR']-dfd['QDSBS']-dfd['QJFBS']-dfd['QMGBS']+dfd['QNGTR']+dfd['QETTR']) != 0.0)
    z[26] = (
        (
            dfd["AMPBLK/PTPTR"]
            * (dfd["QTPTR"] - dfd["QDSBS"] - dfd["QJFBS"] - dfd["QMGBS"])
            + dfd["AMPBLK/PNGTR"] * dfd["QNGTR"]
            + dfd["AMPBLK/PETTR"] * dfd["QETTR"]
        )
        / (
            dfd["QTPTR"]
            - dfd["QDSBS"]
            - dfd["QJFBS"]
            - dfd["QMGBS"]
            + dfd["QNGTR"]
            + dfd["QETTR"]
        )
    ) * SCALPR2
    # if ((dfd['QTPTR']-dfd['QDSBS']-dfd['QJFBS']-dfd['QMGBS']) != 0.0)
    z[27] = (
        dfd["AMPBLK/PTPTR"] * dfd["QTPTR"]
        - dfd["AMPBLK/PDSTR"] * dfd["QDSBS"]
        - dfd["AMPBLK/PJFTR"] * dfd["QJFBS"]
        - dfd["AMPBLK/PMGTR"] * dfd["QMGBS"]
    ) / (dfd["QTPTR"] - dfd["QDSBS"] - dfd["QJFBS"] - dfd["QMGBS"])
    z[32] = dfd["APMORE/PPRTR"] * SCALPR2
    z[10] = dfd["PH2TR"] * SCALPR2
    z[34] = dfd["AMPBLK/PETTR"] * SCALPR2
    z[30] = dfd["AMPBLK/PMGTR"] * SCALPR2
    z[29] = dfd["AMPBLK/PJFTR"] * SCALPR2
    z[28] = dfd["APONROAD/PDSTRHWY"] * SCALPR2
    z[31] = dfd["AMPBLK/PRSTR"] * SCALPR2
    z[33] = dfd["AMPBLK/PNGTR"] * SCALPR2
    z[10] = dfd["AMPBLK/PH2TR"] * SCALPR2
    z[36] = dfd["AMPBLK/PELTR"] * SCALPR2
    # Total End-Use Energy
    DENOM = (
        dfd["QTPRS"]
        + dfd["QNGRS"]
        + dfd["QCLRS"]
        + dfd["QELRS"]
        + dfd["QTPCM"]
        + dfd["QNGCM"]
        + dfd["QCLCM"]
        + dfd["QELCM"]
        + dfd["QTPIN"]
        + dfd["QNGIN"]
        + dfd["QCLIN"]
        + dfd["QELIN"]
        + dfd["QMCIN"]
        + dfd["QTPTR"]
        + dfd["QNGTR"]
        + dfd["QELTR"]
        + dfd["QETTR"]
    )
    # IF (DENOM != 0.0)
    z[37] = (
        z[1] * (dfd["QTPRS"] + dfd["QNGRS"] + dfd["QCLRS"] + dfd["QELRS"])
        + z[8] * (dfd["QTPCM"] + dfd["QNGCM"] + dfd["QCLCM"] + dfd["QELCM"])
        + z[15]
        * (dfd["QTPIN"] + dfd["QNGIN"] + dfd["QCLIN"] + dfd["QELIN"] + dfd["QMCIN"])
        + z[25] * (dfd["QTPTR"] + dfd["QNGTR"] + dfd["QELTR"] + dfd["QETTR"])
    ) / DENOM
    DENOM = (
        dfd["QTPRS"]
        + dfd["QNGRS"]
        + dfd["QCLRS"]
        + dfd["QTPCM"]
        + dfd["QNGCM"]
        + dfd["QCLCM"]
        + dfd["QTPIN"]
        + dfd["QNGIN"]
        + dfd["QCLIN"]
        + dfd["QMCIN"]
        + dfd["QTPTR"]
        + dfd["QNGTR"]
        + dfd["QETTR"]
    )
    # IF (DENOM != 0.0)
    z[38] = (
        z[2] * (dfd["QTPRS"] + dfd["QNGRS"] + dfd["QCLRS"])
        + z[9] * (dfd["QTPCM"] + dfd["QNGCM"] + dfd["QCLCM"])
        + z[16] * (dfd["QTPIN"] + dfd["QNGIN"] + dfd["QCLIN"] + dfd["QMCIN"])
        + z[26] * (dfd["QTPTR"] + dfd["QNGTR"] + dfd["QETTR"])
    ) / DENOM
    z[39] = dfd["AMPBLK/PELAS"] * SCALPR2

    # Electric Power 8/
    #   Distillate Fuel Oil     T3(42,IR,IY,IS)=PDSEL(IR,IY)
    #   Residual Fuel Oil       T3(43,IR,IY,IS)=PRSEL(IR,IY)
    #   Natural Gas             T3(44,IR,IY,IS)=PNGEL(IR,IY)
    #   Steam Coal              T3(45,IR,IY,IS)=PCLEL(IR,IY)
    #   Uranium                 T3(35,IR,IY,IS)=PUREL(IR,IY)
    #   Biomass                 T3(64,IR,IY,IS)=UPRWDCR(IR,IY)
    # if ((QTPEL(IR,IY)+QNGEL(IR,IY)+QCLEL(IR,IY)) != 0.0)
    z[40] = (
        (
            dfd["AMPBLK/PTPEL"] * dfd["QTPEL"]
            + dfd["AMPBLK/PNGEL"] * dfd["QNGEL"]
            + dfd["AMPBLK/PCLEL"] * dfd["QCLEL"]
        )
        / (dfd["QTPEL"] + dfd["QNGEL"] + dfd["QCLEL"])
    ) * SCALPR2
    z[41] = dfd["AMPBLK/PTPEL"] * SCALPR2
    z[42] = dfd["AMPBLK/PDSEL"] * SCALPR2
    z[43] = dfd["AMPBLK/PRSEL"] * SCALPR2
    z[44] = dfd["AMPBLK/PNGEL"] * SCALPR2
    z[45] = dfd["AMPBLK/PCLEL"] * SCALPR2
    z[35] = dfd["AMPBLK/PUREL"] * SCALPR2
    z[17] = dfd["PH2EL"] * SCALPR2
    z[64] = dfd["UPRWDCR"] * SCALPR2
    #
    # Average Price to All Users
    #   Propane              T3(49,IR,IY,IS)=PLGAS(IR,IY)
    #   E85                  T3(54,IR,IY,IS)=PETTR(IR,IY)
    #   Motor Gasoline       T3(50,IR,IY,IS)=PMGAS(IR,IY)
    #   Jet Fuel             T3(48,IR,IY,IS)=PJFTR(IR,IY)
    #   Distillate Fuel Oil  T3(47,IR,IY,IS)=PDSAS(IR,IY)
    #   Residual Fuel Oil    T3(51,IR,IY,IS)=PRSAS(IR,IY)
    #   Natural Gas          T3(52,IR,IY,IS)=PNGAS(IR,IY)
    #   Metallurgical Coal   T3(22,IR,IY,IS)=PMCIN(IR,IY)
    #   Other Coal           T3(53,IR,IY,IS)=PCLAS(IR,IY)
    #   Electricity          T3(56,IR,IY,IS)=PELAS(IR,IY)
    #
    #! calculated based on the sectoral prices shown (as stated in the footnote):
    #          IF (QPRRS(IR,IY)+QPRCM(IR,IY)+QPRIN(IR,IY)-QPRINPF(IR,IY)+QPRTR(IR,IY) .NE. 0.0) &
    #          T3(49,IR,IY,IS)= &
    #             (QPRRS(IR,IY)*PPRRS(IR,IY)+QPRCM(IR,IY)*PPRCM(IR,IY)+ &
    #             (QPRIN(IR,IY)-QPRINPF(IR,IY))*PPRIN(IR,IY)+QPRTR(IR,IY)*PPRTR(IR,IY)) / &
    #             (QPRRS(IR,IY)+QPRCM(IR,IY)+QPRIN(IR,IY)-QPRINPF(IR,IY)+QPRTR(IR,IY))
    # z[49] = ((dfd['QPRRS'].loc[MNUMCR]*dfd['PPRRS'].loc[MNUMCR]+dfd['QPRCM'].loc[MNUMCR]*dfd['PPRCM'].loc[MNUMCR]+
    # (dfd['QPRIN'].loc[MNUMCR]-dfd['QPRINPF'].loc[MNUMCR])*dfd['PPRIN'].loc[MNUMCR]+dfd['QPRTR'].loc[MNUMCR]*dfd['PPRTR'].loc[MNUMCR]) /
    # (dfd['QPRRS'].loc[MNUMCR]+dfd['QPRCM'].loc[MNUMCR]+dfd['QPRIN'].loc[MNUMCR]-dfd['QPRINPF'].loc[MNUMCR]+dfd['QPRTR'].loc[MNUMCR])) * SCALPR2 * dfd['TRIL_TO_QUAD_rwpre']
    z[49] = (
        (
            dfd["QPRRS"] * dfd["APMORE/PPRRS"]
            + dfd["QPRCM"] * dfd["APMORE/PPRCM"]
            + (dfd["QPRIN"] - dfd["QPRINPF"]) * dfd["APMORE/PPRIN"]
            + dfd["QPRTR"] * dfd["APMORE/PPRTR"]
        )
        / (dfd["QPRRS"] + dfd["QPRCM"] + dfd["QPRIN"] - dfd["QPRINPF"] + dfd["QPRTR"])
    ) * SCALPR2

    z[54] = dfd["AMPBLK/PETTR"] * SCALPR2
    z[50] = dfd["AMPBLK/PMGAS"] * SCALPR2
    z[48] = dfd["AMPBLK/PJFTR"] * SCALPR2
    z[47] = dfd["AMPBLK/PDSAS"] * SCALPR2
    z[51] = dfd["AMPBLK/PRSAS"] * SCALPR2
    z[52] = dfd["AMPBLK/PNGAS"] * SCALPR2
    z[22] = dfd["AMPBLK/PMCIN"] * SCALPR2
    z[53] = dfd["AMPBLK/PCLAS"] * SCALPR2
    # Average Hydrogen Price
    z[27] = (
        (
            dfd["QH2IN"] * dfd["PH2IN"]
            + dfd["QH2TR"] * dfd["PH2TR"]
            + dfd["QH2EL"] * dfd["PH2EL"]
        )
        / (dfd["QH2IN"] + dfd["QH2TR"] + dfd["QH2EL"])
    ) * SCALPR2
    
    z[56] = dfd["AMPBLK/PELAS"] * SCALPR2
    z[57] = (
        (
            (
                dfd["AMPBLK/PTPRS"] * dfd["QTPRS"]
                + dfd["AMPBLK/PNGRS"] * dfd["QNGRS"]
                + dfd["AMPBLK/PCLRS"] * dfd["QCLRS"]
                + dfd["AMPBLK/PELRS"] * dfd["QELRS"]
            )
        )
        * SCALPR2
        * dfd["TRIL_TO_QUAD_rwpre"]
    )
    z[58] = (
        (
            (
                dfd["AMPBLK/PTPCM"] * dfd["QTPCM"]
                + dfd["AMPBLK/PNGCM"] * dfd["QNGCM"]
                + dfd["AMPBLK/PCLCM"] * dfd["QCLCM"]
                + dfd["AMPBLK/PELCM"] * dfd["QELCM"]
            )
        )
        * SCALPR2
        * dfd["TRIL_TO_QUAD_rwpre"]
    )
    z[59] = (
        (
            dfd["AMPBLK/PTPIN"] * (dfd["QTPIN"] - dfd["QTPRF"])
            + dfd["AMPBLK/PNGIN"] * (dfd["QNGIN"] - dfd["QNGRF"])
            + dfd["AMPBLK/PCLIN"] * (dfd["QCLIN"])
            + dfd["AMPBLK/PELIN"] * (dfd["QELIN"] - dfd["QELRF"])
            + dfd["AMPBLK/PMCIN"] * dfd["QMCIN"]
            + dfd["AMPBLK/PMCIN"] * dfd["QCIIN"]
        )
        * SCALPR2
        * dfd["TRIL_TO_QUAD_rwpre"]
    )

    z[60] = (
        (
            (
                dfd["AMPBLK/PTPTR"] * dfd["QTPTR"]
                + dfd["AMPBLK/PNGTR"] * dfd["QNGTR"]
                + dfd["AMPBLK/PELTR"] * dfd["QELTR"]
                + dfd["AMPBLK/PMETR"] * dfd["QMETR"]
            )
        )
        * SCALPR2
        * dfd["TRIL_TO_QUAD_rwpre"]
    )
    z[61] = z[57] + z[58] + z[59] + z[60]
    z[62] = dfd["AMPBLK/PETTR"] * dfd["QTRTR"] * SCALPR2 * dfd["TRIL_TO_QUAD_rwpre"]
    z[63] = z[61] + z[62]

    z[66] = z[5] / SCALPR2 * MC_JPGDP.values

    """SZO updates"""
    # z[67] = z[4] / SCALPR2* dfd['MC_JPGDP'].loc[1:61][0]
    # Convert prices to nominal dollar
    z[67] = z[4] / SCALPR2 * MC_JPGDP.values
    z[68] = z[6] / SCALPR2 * MC_JPGDP.values
    z[69] = z[7] / SCALPR2 * MC_JPGDP.values
    z[70] = z[11] / SCALPR2 * MC_JPGDP.values
    z[71] = z[12] / SCALPR2 * MC_JPGDP.values
    z[72] = z[13] / SCALPR2 * MC_JPGDP.values
    z[73] = z[14] / SCALPR2 * MC_JPGDP.values
    z[74] = z[19] / SCALPR2 * MC_JPGDP.values
    z[75] = z[18] / SCALPR2 * MC_JPGDP.values
    z[76] = z[20] / SCALPR2 * MC_JPGDP.values
    z[77] = z[21] / SCALPR2 * MC_JPGDP.values
    z[78] = z[22] / SCALPR2 * MC_JPGDP.values
    z[79] = z[23] / SCALPR2 * MC_JPGDP.values
    z[39] = z[23] / SCALPR2 * MC_JPGDP.values
    z[81] = z[24] / SCALPR2 * MC_JPGDP.values
    z[82] = z[46] / SCALPR2 * MC_JPGDP.values
    z[83] = z[32] / SCALPR2 * MC_JPGDP.values
    z[84] = z[34] / SCALPR2 * MC_JPGDP.values
    z[85] = z[30] / SCALPR2 * MC_JPGDP.values
    z[86] = z[29] / SCALPR2 * MC_JPGDP.values
    z[87] = z[28] / SCALPR2 * MC_JPGDP.values
    z[88] = z[31] / SCALPR2 * MC_JPGDP.values
    z[89] = z[33] / SCALPR2 * MC_JPGDP.values
    z[41] = z[10] / SCALPR2 * MC_JPGDP.values
    z[90] = z[36] / SCALPR2 * MC_JPGDP.values
    z[91] = z[42] / SCALPR2 * MC_JPGDP.values
    z[92] = z[43] / SCALPR2 * MC_JPGDP.values
    z[93] = z[44] / SCALPR2 * MC_JPGDP.values
    z[94] = z[45] / SCALPR2 * MC_JPGDP.values
    z[55] = z[35] / SCALPR2 * MC_JPGDP.values
    z[65] = z[17] / SCALPR2 * MC_JPGDP.values
    z[95] = z[64] / SCALPR2 * MC_JPGDP.values
    z[96] = z[49] / SCALPR2 * MC_JPGDP.values
    z[97] = z[54] / SCALPR2 * MC_JPGDP.values
    z[98] = z[50] / SCALPR2 * MC_JPGDP.values
    z[99] = z[48] / SCALPR2 * MC_JPGDP.values
    z[100] = z[47] / SCALPR2 * MC_JPGDP.values
    z[101] = z[51] / SCALPR2 * MC_JPGDP.values
    z[102] = z[52] / SCALPR2 * MC_JPGDP.values
    z[103] = z[22] / SCALPR2 * MC_JPGDP.values
    z[104] = z[53] / SCALPR2 * MC_JPGDP.values
    z[80] = z[27] / SCALPR2 * MC_JPGDP.values
    z[106] = z[56] / SCALPR2 * MC_JPGDP.values
    z[107] = z[57] / SCALPR2 * MC_JPGDP.values
    z[108] = z[58] / SCALPR2 * MC_JPGDP.values
    z[109] = z[59] / SCALPR2 * MC_JPGDP.values
    z[110] = z[60] / SCALPR2 * MC_JPGDP.values
    z[111] = z[61] / SCALPR2 * MC_JPGDP.values
    z[112] = z[62] / SCALPR2 * MC_JPGDP.values
    z[113] = z[63] / SCALPR2 * MC_JPGDP.values
    z[114] = z[1] / SCALPR2 * MC_JPGDP.values
    z[115] = z[8] / SCALPR2 * MC_JPGDP.values
    z[116] = z[15] / SCALPR2 * MC_JPGDP.values
    z[117] = z[25] / SCALPR2 * MC_JPGDP.values
    z[118] = z[37] / SCALPR2 * MC_JPGDP.values
    z[119] = z[2] / SCALPR2 * MC_JPGDP.values
    z[120] = z[9] / SCALPR2 * MC_JPGDP.values
    z[121] = z[16] / SCALPR2 * MC_JPGDP.values
    z[122] = z[26] / SCALPR2 * MC_JPGDP.values

    z[123] = z[38] / SCALPR2 * MC_JPGDP.values
    z[124] = z[40] / SCALPR2 * MC_JPGDP.values
    z[126] = z[125] / SCALPR2 * MC_JPGDP.values

    return z
