# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
SZO fixed propane national prices based on averages of the regional values on 9/4/2024
"""


def fill_table_base_119(dfd, table_spec, table_id):
    """Fill table

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

    MNUMCR = dfd["MNUMCR_rwpre"]
    TRIL_TO_QUAD = dfd["TRIL_TO_QUAD_rwpre"]

    FYRPRC = int(table_spec["settings"]["year_index_real_dol_priceyr"])
    # Get inflation index from 1990->2050
    MC_JPGDP = dfd["MC_JPGDP"].loc[1990:]

    # Get inflation index of FYRPRC (2022)
    SCALPR2 = MC_JPGDP.loc[FYRPRC]  # 2022 = column 33

    # ftab.f ---
    # ! propane price national averages don't seem to be averages of the regional values, so calculate here:
    #   DO IY=1,LASTYR
    #   IF (sum(QPRRS(1:9,IY)) .NE. 0.0) THEN
    #   PPRRS(MNUMCR,IY) = (QPRRS( 1,IY)*PPRRS( 1,IY) + QPRRS( 2,IY)*PPRRS( 2,IY) + &
    #                       QPRRS( 3,IY)*PPRRS( 3,IY) + QPRRS( 4,IY)*PPRRS( 4,IY) + &
    #                       QPRRS( 5,IY)*PPRRS( 5,IY) + QPRRS( 6,IY)*PPRRS( 6,IY) + &
    #                       QPRRS( 7,IY)*PPRRS( 7,IY) + QPRRS( 8,IY)*PPRRS( 8,IY) + &
    #                       QPRRS( 9,IY)*PPRRS( 9,IY)) / sum(QPRRS(1:9,IY))
    #   ELSE
    #       PPRRS(MNUMCR,IY) = sum(PPRRS(1:9,IY)) / 9.
    #   ENDIF
    #   IF (sum(QPRCM(1:9,IY)) .NE. 0.0) THEN
    #   PPRCM(MNUMCR,IY) = (QPRCM( 1,IY)*PPRCM( 1,IY) + QPRCM( 2,IY)*PPRCM( 2,IY) + &
    #                       QPRCM( 3,IY)*PPRCM( 3,IY) + QPRCM( 4,IY)*PPRCM( 4,IY) + &
    #                       QPRCM( 5,IY)*PPRCM( 5,IY) + QPRCM( 6,IY)*PPRCM( 6,IY) + &
    #                       QPRCM( 7,IY)*PPRCM( 7,IY) + QPRCM( 8,IY)*PPRCM( 8,IY) + &
    #                       QPRCM( 9,IY)*PPRCM( 9,IY)) / sum(QPRCM(1:9,IY))
    #   ELSE
    #       PPRCM(MNUMCR,IY) = sum(PPRCM(1:9,IY)) / 9.
    #   ENDIF
    #   IF (sum(QPRIN(1:9,IY)) .NE. 0.0) THEN
    #   PPRIN(MNUMCR,IY) = (QPRIN( 1,IY)*PPRIN( 1,IY) + QPRIN( 2,IY)*PPRIN( 2,IY) + &
    #                       QPRIN( 3,IY)*PPRIN( 3,IY) + QPRIN( 4,IY)*PPRIN( 4,IY) + &
    #                       QPRIN( 5,IY)*PPRIN( 5,IY) + QPRIN( 6,IY)*PPRIN( 6,IY) + &
    #                       QPRIN( 7,IY)*PPRIN( 7,IY) + QPRIN( 8,IY)*PPRIN( 8,IY) + &
    #                       QPRIN( 9,IY)*PPRIN( 9,IY)) / sum(QPRIN(1:9,IY))
    #   ELSE
    #       PPRIN(MNUMCR,IY) = sum(PPRIN(1:9,IY)) / 9.
    #   ENDIF
    #   IF (sum(QPRTR(1:9,IY)) .NE. 0.0) THEN
    #   PPRTR(MNUMCR,IY) = (QPRTR( 1,IY)*PPRTR( 1,IY) + QPRTR( 2,IY)*PPRTR( 2,IY) + &
    #                       QPRTR( 3,IY)*PPRTR( 3,IY) + QPRTR( 4,IY)*PPRTR( 4,IY) + &
    #                       QPRTR( 5,IY)*PPRTR( 5,IY) + QPRTR( 6,IY)*PPRTR( 6,IY) + &
    #                       QPRTR( 7,IY)*PPRTR( 7,IY) + QPRTR( 8,IY)*PPRTR( 8,IY) + &
    #                       QPRTR( 9,IY)*PPRTR( 9,IY)) / sum(QPRTR(1:9,IY))
    #   ELSE
    #       PPRTR(MNUMCR,IY) = sum(PPRTR(1:9,IY)) / 9.
    #   ENDIF
    #   ENDDO
    PPRRS = dfd["PMORE/PPRRS"].copy()
    mask = dfd["QPRRS"].loc[1:9].sum() != 0
    df1 = (dfd["QPRRS"].loc[1:9] * dfd["PMORE/PPRRS"].loc[1:9]).sum()
    df2 = dfd["QPRRS"].loc[1:9].sum()
    PPRRS.loc[MNUMCR].loc[mask] = df1.loc[mask] / df2.loc[mask]
    PPRRS.loc[MNUMCR].loc[~mask] = (dfd["PMORE/PPRRS"].loc[1:9].sum()).loc[~mask] / 9

    PPRCM = dfd["PMORE/PPRCM"].copy()
    mask = dfd["QPRCM"].loc[1:9].sum() != 0
    df1 = (dfd["QPRCM"].loc[1:9] * dfd["PMORE/PPRCM"].loc[1:9]).sum()
    df2 = dfd["QPRCM"].loc[1:9].sum()
    PPRCM.loc[MNUMCR].loc[mask] = df1.loc[mask] / df2.loc[mask]
    PPRCM.loc[MNUMCR].loc[~mask] = (dfd["PMORE/PPRCM"].loc[1:9].sum()).loc[~mask] / 9

    PPRIN = dfd["PMORE/PPRIN"].copy()
    mask = dfd["QPRIN"].loc[1:9].sum() != 0
    df1 = (dfd["QPRIN"].loc[1:9] * dfd["PMORE/PPRIN"].loc[1:9]).sum()
    df2 = dfd["QPRIN"].loc[1:9].sum()
    PPRIN.loc[MNUMCR].loc[mask] = df1.loc[mask] / df2.loc[mask]
    PPRIN.loc[MNUMCR].loc[~mask] = (dfd["PMORE/PPRIN"].loc[1:9].sum()).loc[~mask] / 9

    PPRTR = dfd["PMORE/PPRTR"].copy()
    mask = dfd["QPRTR"].loc[1:9].sum() != 0
    df1 = (dfd["QPRTR"].loc[1:9] * dfd["PMORE/PPRTR"].loc[1:9]).sum()
    df2 = dfd["QPRTR"].loc[1:9].sum()
    PPRTR.loc[MNUMCR].loc[mask] = df1.loc[mask] / df2.loc[mask]
    PPRTR.loc[MNUMCR].loc[~mask] = (dfd["PMORE/PPRTR"].loc[1:9].sum()).loc[~mask] / 9

    #  Rows 1-4 aren't in the layin file, along with several others. Layin also includes IEA submissions for some reason (not filled in)

    #   Unadjusted Energy Prices by Sector and Source
    #   (#### dollars per million Btu, unless otherwise noted)
    #    Sector and Source

    #    Residential

    #      Propane
    # T119(5,IR,IY,IS)=PPRRS(IR,IY)
    # z[5] = dfd['PMORE/PPRRS'] * SCALPR2
    z[5] = PPRRS * SCALPR2

    #      Distillate Fuel Oil
    # T119(4,IR,IY,IS)=PDSRS(IR,IY)
    z[4] = dfd["MPBLK/PDSRS"] * SCALPR2

    #      Natural Gas
    # T119(6,IR,IY,IS)=PNGRS(IR,IY)
    z[6] = dfd["MPBLK/PNGRS"] * SCALPR2

    #      Electricity
    # T119(7,IR,IY,IS)=PELRS(IR,IY)
    z[7] = dfd["MPBLK/PELRS"] * SCALPR2

    #

    #    Commercial

    #      Distillate Fuel Oil
    # T119(11,IR,IY,IS)=PDSCM(IR,IY)
    z[11] = dfd["MPBLK/PDSCM"] * SCALPR2

    #      Residual Fuel Oil
    # T119(12,IR,IY,IS)=PRSCM(IR,IY)
    z[12] = dfd["MPBLK/PRSCM"] * SCALPR2

    #      Natural Gas
    # T119(13,IR,IY,IS)=PNGCM(IR,IY)
    z[13] = dfd["MPBLK/PNGCM"] * SCALPR2

    #      Electricity
    # T119(14,IR,IY,IS)=PELCM(IR,IY)
    z[14] = dfd["MPBLK/PELCM"] * SCALPR2

    #

    #    Industrial 1/

    #      Propane
    # ftab.f ---
    # ! do this if we want to average in feedstock prices:
    #         IF(QLGIN(IR,IY) .NE. 0.0) &
    #         T119(19,IR,IY,IS)=(PLGIN(IR,IY)*(QLGIN(IR,IY)-INQLGPF(IR,IY)/1000.) + &
    #                             PLGINPF(IR,IY)*INQLGPF(IR,IY)/1000.) / QLGIN(IR,IY)
    # ! otherwise do this:
    #         T119(19,IR,IY,IS)=PLGIN(IR,IY)
    # ! but it is simpler (the row in now labelled "propane") if we just use the propane price variable:
    #         T119(19,IR,IY,IS)=PPRIN(IR,IY)
    # ...
    # PLGINPF R    4 2 (MNUMCR,MNUMYR)  PRICE  87$BTU LG     INDUST Industrial LPG feedstock price
    # PPRRS   R    4 2 (MNUMCR,MNUMYR)  PRICE  87$BTU        RESIDN Residential propane
    # PPRCM   R    4 2 (MNUMCR,MNUMYR)  PRICE  87$BTU        COMMER Commercial propane
    # PPRIN   R    4 2 (MNUMCR,MNUMYR)  PRICE  87$BTU        INDUST Industrial propane
    # PPRTR   R    4 2 (MNUMCR,MNUMYR)  PRICE  87$BTU        TRANSP Transportation propane
    # PLGIN   R    4 2 (MNUMCR,MNUMYR)  PRICE  87$BTU LG     INDUST Liquid Petroleum Gases - Industrial
    # QLGIN   R    4 2 (MNUMCR,MNUMYR)  QUNTY  tBTU   LG     INDUST Liquid Petroleum Gases - Industrial
    # QLGIN   R    4 2 (MNUMCR,MNUMYR)  QUNTY  tBTU   LG     INDUST Liquid Petroleum Gases - Industrial
    # INQLGPF R    4 2 (MNUMCR,MNUMYR)  QUNTY  tBTU   LG     INDUST Consumption of LPG feedstocks
    #
    # T119(19,IR,IY,IS)=(PLGIN(IR,IY)*(QLGIN(IR,IY)-INQLGPF(IR,IY)/1000.)+PLGINPF(IR,IY)*INQLGPF(IR,IY)/1000.)/QLGIN(IR,IY)
    # z[19] = ((dfd['MPBLK/PLGIN'] * (dfd['QLGIN'] - dfd['INQLGPF']/ 1000.) + dfd['PMORE/PLGINPF'] * dfd['INQLGPF']/ 1000. ) / dfd['QLGIN']) * SCALPR2
    z[19] = PPRIN * SCALPR2

    #      Distillate Fuel Oil
    # T119(18,IR,IY,IS)=PDSIN(IR,IY)
    z[18] = dfd["MPBLK/PDSIN"] * SCALPR2

    #      Residual Fuel Oil
    # T119(20,IR,IY,IS)=PRSIN(IR,IY)
    z[20] = dfd["MPBLK/PRSIN"] * SCALPR2

    #      Natural Gas 2/
    # T119(21,IR,IY,IS)=PNGIN(IR,IY)
    z[21] = dfd["MPBLK/PNGIN"] * SCALPR2

    #      Metallurgical Coal
    # T119(22,IR,IY,IS)=PMCIN(IR,IY)
    z[22] = dfd["MPBLK/PMCIN"] * SCALPR2

    #      Other Industrial Coal
    # T119(23,IR,IY,IS)=PCLIN(IR,IY)
    z[23] = dfd["MPBLK/PCLIN"] * SCALPR2

    #      Coal to Liquids
    # T119(65,IR,IY,IS)=PCLSN(IR,IY)
    z[65] = dfd["MPBLK/PCLSN"] * SCALPR2

    #      Electricity
    # T119(24,IR,IY,IS)=PELIN(IR,IY)
    z[24] = dfd["MPBLK/PELIN"] * SCALPR2

    #

    #    Transportation

    #      Propane
    # T119(32,IR,IY,IS)=PPRTR(IR,IY)
    # z[32] = dfd['PMORE/PPRTR'] * SCALPR2
    z[32] = PPRTR * SCALPR2

    #      E85 3/
    # T119(34,IR,IY,IS)=PETTR(IR,IY)
    z[34] = dfd["MPBLK/PETTR"] * SCALPR2

    #      Motor Gasoline 4/
    # T119(30,IR,IY,IS)=PMGTR(IR,IY)
    z[30] = dfd["MPBLK/PMGTR"] * SCALPR2

    #      Jet Fuel 5/
    # T119(29,IR,IY,IS)=PJFTR(IR,IY)
    z[29] = dfd["MPBLK/PJFTR"] * SCALPR2

    #      Diesel Fuel (distillate fuel oil) 6/
    # T119(28,IR,IY,IS)=PDSTRHWY(IR,IY)
    z[28] = dfd["PONROAD/PDSTRHWY"] * SCALPR2

    #      Residual Fuel Oil
    # T119(31,IR,IY,IS)=PRSTR(IR,IY)
    z[31] = dfd["MPBLK/PRSTR"] * SCALPR2

    #      Natural Gas 7/
    # T119(33,IR,IY,IS)=PNGTR(IR,IY)
    z[33] = dfd["MPBLK/PNGTR"] * SCALPR2

    #      Electricity
    # T119(36,IR,IY,IS)=PELTR(IR,IY)
    z[36] = dfd["MPBLK/PELTR"] * SCALPR2

    #

    #    Electric Power 8/

    #      Distillate Fuel Oil
    # T119(42,IR,IY,IS)=PDSEL(IR,IY)
    z[42] = dfd["MPBLK/PDSEL"] * SCALPR2

    #      Residual Fuel Oil
    # T119(43,IR,IY,IS)=PRSEL(IR,IY)
    z[43] = dfd["MPBLK/PRSEL"] * SCALPR2

    #      Natural Gas
    # T119(44,IR,IY,IS)=PNGEL(IR,IY)
    z[44] = dfd["MPBLK/PNGEL"] * SCALPR2

    #      Steam Coal
    # T119(45,IR,IY,IS)=PCLEL(IR,IY)
    z[45] = dfd["MPBLK/PCLEL"] * SCALPR2

    #      Biomass (remove for AEO tables)
    # T119(64,IR,IY,IS)=UPRWDCR(IR,IY)
    z[64] = dfd["UPRWDCR"] * SCALPR2

    #

    #    Average Price to All Users 9/

    #      Propane
    # T119(49,IR,IY,IS)=(QPRRS(IR,IY)*PPRRS(IR,IY)+QPRCM(IR,IY)*PPRCM(IR,IY)+(QPRIN(IR,IY)-QPRINPF(IR,IY))*PPRIN(IR,IY)+QPRTR(IR,IY)*PPRTR(IR,IY))\
    #     /(QPRRS(IR,IY)+QPRCM(IR,IY)+QPRIN(IR,IY)-QPRINPF(IR,IY)+QPRTR(IR,IY))
    # z[49] = (( dfd['QPRRS'] * dfd['PPRRS'] + dfd['QPRCM'] * dfd['PPRCM'] + (dfd['QPRIN'] - dfd['QPRINPF']) * dfd['PMORE/PPRIN'] + dfd['QPRTR'] * dfd['PPRTR']) / (dfd['QPRRS'] + dfd['QPRCM'] + dfd['QPRIN'] - dfd['QPRINPF'] + dfd['QPRTR'])) * SCALPR2
    # z[49] = (( dfd['QPRRS'] * dfd['PPRRS'] + dfd['QPRCM'] * PPRCM + (dfd['QPRIN'] - dfd['QPRINPF']) * PPRIN + dfd['QPRTR'] * PPRTR) / (dfd['QPRRS'] + dfd['QPRCM'] + dfd['QPRIN'] - dfd['QPRINPF'] + dfd['QPRTR'])) * SCALPR2
    z[49] = (
        (
            (
                dfd["QPRRS"] * PPRRS
                + dfd["QPRCM"] * PPRCM
                + (dfd["QPRIN"] - dfd["QPRINPF"]) * PPRIN
                + dfd["QPRTR"] * PPRTR
            )
        )
        / (dfd["QPRRS"] + dfd["QPRCM"] + dfd["QPRIN"] - dfd["QPRINPF"] + dfd["QPRTR"])
        * SCALPR2
    )
    #      E85 3/
    # T119(54,IR,IY,IS)=PETTR(IR,IY)
    z[54] = dfd["MPBLK/PETTR"] * SCALPR2

    #      Motor Gasoline 4/
    # T119(50,IR,IY,IS)=PMGAS(IR,IY)
    z[50] = dfd["MPBLK/PMGAS"] * SCALPR2

    #      Jet Fuel 5/
    # T119(48,IR,IY,IS)=PJFTR(IR,IY)
    z[48] = dfd["MPBLK/PJFTR"] * SCALPR2

    #      Distillate Fuel Oil
    # T119(47,IR,IY,IS)=PDSAS(IR,IY)
    z[47] = dfd["MPBLK/PDSAS"] * SCALPR2

    #      Residual Fuel Oil
    # T119(51,IR,IY,IS)=PRSAS(IR,IY)
    z[51] = dfd["MPBLK/PRSAS"] * SCALPR2

    #      Natural Gas
    # T119(52,IR,IY,IS)=PNGAS(IR,IY)
    z[52] = dfd["MPBLK/PNGAS"] * SCALPR2

    #      Metallurgical Coal
    # T119(22,IR,IY,IS)=PMCIN(IR,IY)
    z[22] = dfd["MPBLK/PMCIN"] * SCALPR2

    #      Other Coal
    # T119(53,IR,IY,IS)=PCLAS(IR,IY)
    z[53] = dfd["MPBLK/PCLAS"] * SCALPR2

    #      Coal to Liquids
    # T119(65,IR,IY,IS)=PCLSN(IR,IY)
    z[65] = dfd["MPBLK/PCLSN"] * SCALPR2

    #      Electricity
    # T119(56,IR,IY,IS)=PELAS(IR,IY)
    z[56] = dfd["MPBLK/PELAS"] * SCALPR2

    #   (billion #### dollars)

    #    Residential
    # T119(57,IR,IY,IS)=(PTPRS(IR,IY)*QTPRS(IR,IY)+PNGRS(IR,IY)*QNGRS(IR,IY)+PCLRS(IR,IY)*QCLRS(IR,IY)+PELRS(IR,IY)*QELRS(IR,IY))
    z[57] = (
        (
            dfd["MPBLK/PTPRS"] * dfd["QTPRS"]
            + dfd["MPBLK/PNGRS"] * dfd["QNGRS"]
            + dfd["MPBLK/PCLRS"] * dfd["QCLRS"]
            + dfd["MPBLK/PELRS"] * dfd["QELRS"]
        )
        * SCALPR2
        * TRIL_TO_QUAD
    )

    #    Commercial
    # T119(58,IR,IY,IS)=(PTPCM(IR,IY)*QTPCM(IR,IY)+PNGCM(IR,IY)*QNGCM(IR,IY)+PCLCM(IR,IY)*QCLCM(IR,IY)+PELCM(IR,IY)*QELCM(IR,IY))
    z[58] = (
        (
            dfd["MPBLK/PTPCM"] * dfd["QTPCM"]
            + dfd["MPBLK/PNGCM"] * dfd["QNGCM"]
            + dfd["MPBLK/PCLCM"] * dfd["QCLCM"]
            + dfd["MPBLK/PELCM"] * dfd["QELCM"]
        )
        * SCALPR2
        * TRIL_TO_QUAD
    )

    #    Industrial
    # T119(59,IR,IY,IS)=PTPIN(IR,IY)*(QTPIN(IR,IY)-QTPRF(IR,IY))+PNGIN(IR,IY)*(QNGIN(IR,IY)-QNGRF(IR,IY))+PCLIN(IR,IY)*(QCLIN(IR,IY))+PELIN(IR,IY)*(QELIN(IR,IY)-QELRF(IR,IY))+PMCIN(IR,IY)*QMCIN(IR,IY)+PMCIN(IR,IY)*QCIIN(IR,IY)
    z[59] = (
        (
            dfd["MPBLK/PTPIN"] * (dfd["QTPIN"] - dfd["QTPRF"])
            + dfd["MPBLK/PNGIN"] * (dfd["QNGIN"] - dfd["QNGRF"])
            + dfd["MPBLK/PCLIN"] * (dfd["QCLIN"])
            + dfd["MPBLK/PELIN"] * (dfd["QELIN"] - dfd["QELRF"])
            + dfd["MPBLK/PMCIN"] * dfd["QMCIN"]
            + dfd["MPBLK/PMCIN"] * dfd["QCIIN"]
        )
        * SCALPR2
        * TRIL_TO_QUAD
    )

    #    Transportation
    # T119(60,IR,IY,IS)=(PTPTR(IR,IY)*QTPTR(IR,IY)+PNGTR(IR,IY)*QNGTR(IR,IY)+PELTR(IR,IY)*QELTR(IR,IY)+PMETR(IR,IY)*QMETR(IR,IY))
    z[60] = (
        (
            dfd["MPBLK/PTPTR"] * dfd["QTPTR"]
            + dfd["MPBLK/PNGTR"] * dfd["QNGTR"]
            + dfd["MPBLK/PELTR"] * dfd["QELTR"]
            + dfd["MPBLK/PMETR"] * dfd["QMETR"]
        )
        * SCALPR2
        * TRIL_TO_QUAD
    )

    #      Total Non-Renewable Expenditures
    # T119(61,IR,IY,IS)=T119(57,IR,IY,IS)+T119(58,IR,IY,IS)+T119(59,IR,IY,IS)+T119(60,IR,IY,IS)
    z[61] = z[57] + z[58] + z[59] + z[60]

    #      Transportation Renewable Expenditures
    # T119(62,IR,IY,IS)=PETTR(IR,IY)*QTRTR(IR,IY)
    z[62] = dfd["MPBLK/PETTR"] * dfd["QTRTR"] * SCALPR2 * TRIL_TO_QUAD

    #      Total Expenditures
    # T119(63,IR,IY,IS)=T119(61,IR,IY,IS)+T119(62,IR,IY,IS)
    z[63] = z[61] + z[62]

    return z
