# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM

SZO rewrote code on 8/28/2024

"""


def fill_table_base_117(dfd, table_spec, table_id):
    """Fill table for  National Impacts of the Clean Air Act Amendments of 1990

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

    FYRPRC = int(table_spec["settings"]["year_index_real_dol_priceyr"])
    # Get inflation index from 1990->2050
    MC_JPGDP = dfd["MC_JPGDP"].loc[1990:]

    # Get inflation index of FYRPRC (2022)
    SCALPR2 = MC_JPGDP.loc[FYRPRC]  # 2022 = column 33

    MNUMNR = dfd["MNUMNR_rwpre"]
    C_CO2_FACTOR = dfd["C_CO2_FACTOR_rwpre"]
    NUTSEC = dfd["NUTSEC_rwpre"]
    NDREG = dfd["NDREG_rwpre"]

    yearpr = int(dfd["YEARPR"].iloc[0])
    year_idx = table_spec["years"].index(yearpr)

    # Init
    CUMRET = dfd["EMELRET"].loc[MNUMNR].copy().to_frame().T  # Scrubber retrofits
    CUMRETP = dfd["EMELRETP"].loc[MNUMNR].copy().to_frame().T  # Planned
    CUMNOX = dfd["EMELNOX"].loc[MNUMNR].copy().to_frame().T  # NOX combustion controls
    CUMNOXP = dfd["EMELNOXP"].loc[MNUMNR].copy().to_frame().T  # Planned
    CUMSCR = (
        dfd["EMELSCR"].loc[MNUMNR].copy().to_frame().T
    )  # NOX SCR post-combustion controls
    CUMSCRP = dfd["EMELSCRP"].loc[MNUMNR].copy().to_frame().T  # planned
    CUMNCR = (
        dfd["EMELNCR"].loc[MNUMNR].copy().to_frame().T
    )  # NOX NCR post-combustion controls
    CUMNCRP = dfd["EMELNCRP"].loc[MNUMNR].copy().to_frame().T  # planned
    CUMSC = dfd["EMELSC"].loc[MNUMNR].copy().to_frame().T  # ACI Spray Cooling Controls
    CUMFF = (
        dfd["EMELFF"].loc[MNUMNR].copy().to_frame().T
    )  # ACI Supplemental Fabric Filter Controls
    CUMDSI = dfd["EMELDSI"].loc[MNUMNR].copy().to_frame().T  # HCI DSI Controls
    CUMHRI = (
        dfd["ECAPNRHR"].loc[MNUMNR].copy().to_frame().T
    )  # ELECTR Capacity with heat rate improvements by EMM region

    # set to 0 before YEARPR
    CUMRET.iloc[:, :year_idx] = 0
    CUMRETP.iloc[:, :year_idx] = 0
    CUMNOX.iloc[:, :year_idx] = 0
    CUMNOXP.iloc[:, :year_idx] = 0
    CUMSCR.iloc[:, :year_idx] = 0
    CUMSCRP.iloc[:, :year_idx] = 0
    CUMNCR.iloc[:, :year_idx] = 0
    CUMNCRP.iloc[:, :year_idx] = 0
    CUMSC.iloc[:, :year_idx] = 0
    CUMFF.iloc[:, :year_idx] = 0
    CUMDSI.iloc[:, :year_idx] = 0
    CUMHRI.iloc[:, :year_idx] = 0

    # Cumulative
    CUMRET = CUMRET.cumsum(axis=1)
    CUMRETP = CUMRETP.cumsum(axis=1)
    CUMNOX = CUMNOX.cumsum(axis=1)
    CUMNOXP = CUMNOXP.cumsum(axis=1)
    CUMSCR = CUMSCR.cumsum(axis=1)
    CUMSCRP = CUMSCRP.cumsum(axis=1)
    CUMNCR = CUMNCR.cumsum(axis=1)
    CUMNCRP = CUMNCRP.cumsum(axis=1)
    CUMSC = CUMSC.cumsum(axis=1)
    CUMFF = CUMFF.cumsum(axis=1)
    CUMDSI = CUMDSI.cumsum(axis=1)
    CUMHRI = CUMHRI.cumsum(axis=1)

    # NUTSEC    38      Utility demand sectors
    # UTTSO2 R    4 2 (NUTSEC,MNUMYR) QUNTY  mMTONS CLUtil total SO2 emissions
    # UCSO2  R    4 2 (NUTSEC,MNUMYR) QUNTY  LBS    CL Util uncontrolled SO2 emissions
    # UCMERC R    4 2 (NUTSEC,MNUMYR) QUNTY  LBS    CL Util uncontrolled Hg  emissions
    SO2COAL = dfd["UTTSO2"].sum()
    SO2CNT = dfd["UCSO2"].sum()
    HGCNT = dfd["UCMERC"].sum()

    # NDREGN    16      Coal Demand Regions
    # QCLCLNR          R    4 3 (NDREGN,MNUMYR,NUTSEC) QUNTY  tBTU Demand for Coal by ECP Coal Plant
    TLCOAL = dfd["QCLCLNR"].sum()

    # CTLSO2EM R    4 2 (NDREGN,MNUMYR) QUNTY  TONS ELECTR SO2 Emissions from CTL by coal region
    # CTLNOXEM R    4 2 (NDREGN,MNUMYR) QUNTY  TONS ELECTR NOX Emissions from CTL by coal region
    # CTLHGEM  R    4 2 (NDREGN,MNUMYR) QUNTY  TONS ELECTR Mercury Emissions from CTL by coal region
    CTLSO2 = dfd["CTLSO2EM"].sum() * 0.000001
    CTLNOX = dfd["CTLNOXEM"].sum() * 0.000001
    CTLHG = dfd["CTLHGEM"].sum()

    # ftab.f
    # EMELPSO2 = EMELPSO2 * SCALPR
    # ECP_PSO2 = ECP_PSO2 * SCALPR
    #
    EMELPSO2 = dfd["EMELPSO2"] * SCALPR2
    ECP_PSO2 = dfd["ECP_PSO2"] * SCALPR2

    # # T117(9,IY,IS)=UNOXOTR(MNUMNR,IY)+UNOXINR(MNUMNR,IY)+CTLNOX
    # z[9] = dfd['UNOXOTR'].sum() + dfd['UNOXINR'].sum() + CTLNOX
    z[9] = dfd["UNOXOTR"].loc[MNUMNR] + dfd["UNOXINR"].loc[MNUMNR] + CTLNOX

    # # T117(5,IY,IS)=T117(5,IY,IS)+SO2OTHER(IY,ISO2)*0.000001
    # z[5] = dfd['SO2OTHER'].sum() * 0.000001
    # T117( 5,IY,IS) = so2coal
    # ftab.f
    #           so2coal = 0.0
    #           so2cnt = 0.0
    #           hgcnt = 0.0
    #           tlcoal = 0.0
    #           DO ISEC = 1, NUTSEC
    #            so2coal = so2coal + UTTSO2(ISEC,IY)
    #            so2cnt = so2cnt + UCSO2(ISEC,IY)
    #            hgcnt = hgcnt + UCMERC(ISEC,IY)
    #            DO IC = 1 , NDREG
    #             tlcoal = tlcoal + QCLCLNR(IC,IY,ISEC)
    #            END DO
    #           ENDDO
    # !     Sum up emissions from CTL
    #               CTLSO2 = 0.0
    #               CTLNOX = 0.0
    #               CTLHG  = 0.0
    #            DO IR = 1 , NDREG
    #               CTLSO2 = CTLSO2 + CTLSO2EM(IR,IY) * 0.000001
    #               CTLNOX = CTLNOX + CTLNOXEM(IR,IY) * 0.000001
    #               CTLHG  = CTLHG  + CTLHGEM(IR,IY)
    #            END DO
    #            T117( 5,IY,IS) = so2coal
    #            T117( 7,IY,IS) = 0.0
    #            DO ISO2 = 1 , NUM_SO2_GRP
    #               T117( 5,IY,IS) = T117( 5,IY,IS) + so2other(IY,ISO2) * 0.000001
    #               T117( 7,IY,IS) = T117( 7,IY,IS) + so2other(IY,ISO2) * 0.000001
    #            END DO
    #            T117( 5,IY,IS) = T117( 5,IY,IS) + CTLSO2
    #            T117( 6,IY,IS) = so2coal + CTLSO2
    #
    # NUM_SO2_GRP      I    4 1 (     1)    INDEX  BLANK    Number of SO2 Compliance Groups
    # SO2OTHER         R    4 2 (MNUMYR,MX_SO2) QUNTY  mMTONS   Other SO2 Emissions
    so2coal = dfd["UTTSO2"].sum()

    z[5] = so2coal + dfd["SO2OTHER"].sum() * 0.000001

    # T117(6,IY,IS)=SO2COAL+CTLSO2
    z[6] = SO2COAL + CTLSO2

    # T117(7,IY,IS)=T117(7,IY,IS)+SO2OTHER(IY,ISO2)*0.000001
    ISO2 = int(dfd["NUM_SO2_GRP"].iloc[0])
    z[7] = dfd["SO2OTHER"].loc[:ISO2].sum() * 0.000001

    #     Mercury (tons)
    # T117(18,IY,IS)=TOT_MERC(IY)/2000.0+CTLHG
    z[18] = dfd["TOT_MERC"] / 2000.0 + CTLHG

    #     Carbon Dioxide (million mt_cc_e)
    # T117(24,IY,IS)=EM_ELEC(9,11,ICY)
    z[24] = dfd["EM_ELEC"].loc[9].loc[11] * C_CO2_FACTOR * 0.001

    #   Allowance Prices
    #     Nitrogen Oxides (#### dollars per ton)

    #       Regional/Seasonal
    # T117(8,IY,IS)=EPNOXPR(1,IY)
    # EPNOXPR   R    4 2 (NOXGRP,MECPYR) PRICE  87$KWH   ELECTR ECP NOX Allowance Prices
    # NOXGRP     5      NOX Cap and Trade Groups
    z[8] = dfd["EPNOXPR"].loc[1]

    #       East/Annual
    # T117(28,IY,IS)=CUMRETP+CUMRET
    z[29] = dfd["EPNOXPR"].loc[2]

    # #       West/Annual
    # #T117(28,IY,IS)=CUMRETP+CUMRET
    z[30] = dfd["EPNOXPR"].loc[3]

    # #     Sulfur Dioxide (#### dollars per ton)

    #       CMM East
    # T117(4,IY,IS)=EMELPSO2(IY,1)
    # EMELPSO2  R    4 2 (MNUMYR,MX_SO2) PRICE  $/TON    CDS Sulfur dioxide emission allowance price
    #
    # EMELPSO2 = EMELPSO2 * SCALPR
    z[4] = EMELPSO2.loc[1]

    #       EMM East
    # T117(26,IY,IS)=ECP_PSO2(0,IY,1)
    z[26] = ECP_PSO2.loc[1].loc[1]

    #       CMM West
    # T117(44,IY,IS)=EMELPSO2(IY,2)
    z[44] = EMELPSO2.loc[2]

    #       EMM West
    # T117(45,IY,IS)=ECP_PSO2(0,IY,2)
    z[45] = ECP_PSO2.loc[1].loc[2]

    #     Mercury

    #       (million #### dollars per ton)
    # T117(16,IY,IS)=EMEL_PHG(1,IY)
    z[16] = dfd["EMEL_PHG"].loc[1]

    #       (thousand #### dollars per pound)
    # T117(17,IY,IS)=EMEL_PHG(1,IY)/2.0
    z[17] = dfd["EMEL_PHG"].loc[1] / 2.0

    #       EMM (thousand #### dollars per pound)
    # T117(50,IY,IS)=ECP_PHG(1,IY)/2.0
    z[50] = dfd["ECP_PHG"].loc[1] / 2.0

    #     Carbon Dioxide (#### dollars per mt_cc_e)
    # T117(25,IY,IS)=EMETAX(1,IY)*1000.*SCALPR
    z[25] = dfd["EMETAX"].loc[1] * 1000.0 * SCALPR2

    # #   Retrofits (gigawatts)

    # #     Scrubber

    # # #       Planned
    # # #T117(27,IY,IS)=CUMRETP
    # z[27] = CUMRETP

    # # #       Unplanned
    # # #T117(1,IY,IS)=CUMRET
    # z[1] = CUMRET

    # # #       Total
    # # #T117(28,IY,IS)=CUMRETP+CUMRET
    # z[28] = CUMRETP + CUMRET

    # #     Nitrogen Oxides Controls

    # #       Combustion

    # #         Planned
    # #T117(38,IY,IS)=CUMNOXP
    z[38] = CUMNOXP

    # #         Unplanned
    # #T117(13,IY,IS)=CUMNOX
    z[13] = CUMNOX

    # #         Total
    # #T117(39,IY,IS)=CUMNOXP+CUMNOX
    z[39] = CUMNOXP + CUMNOX

    # #       SCR Post-combustion

    # #         Planned
    # #T117(40,IY,IS)=CUMSCRP
    z[40] = CUMSCRP

    # #         Unplanned
    # #T117(14,IY,IS)=CUMSCR
    z[14] = CUMSCR

    # #         Total
    # #T117(41,IY,IS)=CUMSCRP+CUMSCR
    z[41] = CUMSCRP + CUMSCR

    # #       SNCR Post-combustion

    # #CUMNCRP =  dfd['EMELNCRP'].loc[MNUMNR,((YEARPR + 1) - 1989): ].sum()
    # #         Planned
    # #T117(42,IY,IS)=CUMNCRP
    z[42] = CUMNCRP

    # #         Unplanned
    # #T117(15,IY,IS)=CUMNCR
    z[15] = CUMNCR

    # #         Total
    # #T117(43,IY,IS)=CUMNCRP+CUMNCR
    z[43] = CUMNCRP + CUMNCR

    # #

    #   Coal Production by Sulfur Category
    #   (million tons)

    #     Low Sulfur (< .61 pounds per million Btu)
    # T117(10,IY,IS)=ABSULF(1,IY)+ALSULF(1,IY)+IBSULF(1,IY)+ILSULF(1,IY)+APSULF(1,IY)+WBSULF(1,IY)+WSSULF(1,IY)+WLSULF(1,IY)+WPSULF(1,IY)
    z[10] = (
        dfd["ABSULF"].loc[1]
        + dfd["ALSULF"].loc[1]
        + dfd["IBSULF"].loc[1]
        + dfd["ILSULF"].loc[1]
        + dfd["APSULF"].loc[1]
        + dfd["WBSULF"].loc[1]
        + dfd["WSSULF"].loc[1]
        + dfd["WLSULF"].loc[1]
        + dfd["WPSULF"].loc[1]
    )

    #     Medium Sulfur
    # T117(11,IY,IS)=ABSULF(2,IY)+ALSULF(2,IY)+IBSULF(2,IY)+ILSULF(2,IY)+APSULF(2,IY)+WBSULF(2,IY)+WSSULF(2,IY)+WLSULF(2,IY)+WPSULF(2,IY)
    z[11] = (
        dfd["ABSULF"].loc[2]
        + dfd["ALSULF"].loc[2]
        + dfd["IBSULF"].loc[2]
        + dfd["ILSULF"].loc[2]
        + dfd["APSULF"].loc[2]
        + dfd["WBSULF"].loc[2]
        + dfd["WSSULF"].loc[2]
        + dfd["WLSULF"].loc[2]
        + dfd["WPSULF"].loc[2]
    )

    #     High Sulfur (> 1.67 pounds per million Btu)
    # T117(12,IY,IS)=ABSULF(3,IY)+ALSULF(3,IY)+IBSULF(3,IY)+ILSULF(3,IY)+APSULF(3,IY)+WBSULF(3,IY)+WSSULF(3,IY)+WLSULF(3,IY)+WPSULF(3,IY)
    z[12] = (
        dfd["ABSULF"].loc[3]
        + dfd["ALSULF"].loc[3]
        + dfd["IBSULF"].loc[3]
        + dfd["ILSULF"].loc[3]
        + dfd["APSULF"].loc[3]
        + dfd["WBSULF"].loc[3]
        + dfd["WSSULF"].loc[3]
        + dfd["WLSULF"].loc[3]
        + dfd["WPSULF"].loc[3]
    )
    #

    # #   Interregional Sulfur Dioxide Allowances

    # ftab.f
    # CUMRETP = 0.0
    # ...
    #    DO IY1 = (YEARPR + 1) - 1989, IY
    #      CUMRET = CUMRET + EMELRET(MNUMNR,IY1)
    #      CUMRETP = CUMRETP + EMELRETP(MNUMNR,IY1)
    #      CUMNOX = CUMNOX + EMELNOX(MNUMNR,IY1)
    #      CUMNOXP = CUMNOXP + EMELNOXP(MNUMNR,IY1)
    #      CUMSCR = CUMSCR + EMELSCR(MNUMNR,IY1)
    #      CUMSCRP = CUMSCRP + EMELSCRP(MNUMNR,IY1)
    #      CUMNCR = CUMNCR + EMELNCR(MNUMNR,IY1)
    #      CUMNCRP = CUMNCRP + EMELNCRP(MNUMNR,IY1)
    #      CUMSC = CUMSC + EMELSC(MNUMNR,IY1)
    #      CUMFF = CUMFF + EMELFF(MNUMNR,IY1)
    #      CUMDSI = CUMDSI + EMELDSI(MNUMNR,IY1)
    #      CUMHRI = CUMHRI + ECAPNRHR(MNUMNR,IY1)
    #   END DO
    #   CUMCCS = EMELCCS(MNUMNR,IY)
    #   CUMCDR = EMELCDR(MNUMNR,IY)
    #   CUMGCS = EMELGCS(MNUMNR,IY)
    #   CUMGDR = EMELGDR(MNUMNR,IY)
    #   T117( 1,IY,IS) = CUMRET
    #   T117(27,IY,IS) = CUMRETP
    #   T117(28,IY,IS) = CUMRETP + CUMRET
    #
    #       Unplanned
    # #T117(1,IY,IS)=CUMRET
    z[1] = CUMRET

    #       Planned
    # #T117(27,IY,IS)=CUMRETP
    z[27] = CUMRETP

    # #       Total
    # #T117(28,IY,IS)=CUMRETP+CUMRET
    z[28] = CUMRETP + CUMRET

    #       CUMRET = 0.0
    #   DO ISO2 = 1 , NUM_SO2_GRP
    #      CUMRET = CUMRET + EMRFSA(IY,ISO2) / 1.0D6
    #   END DO
    #   T117(46,IY,IS) = EMRFSA(IY,1) / 1.0D6
    #   T117(47,IY,IS) = EMRFSA(IY,2) / 1.0D6
    #   T117( 2,IY,IS) = CUMRET
    # #     Target (millions tons)
    # #T117(2,IY,IS)=CUMRET
    CUMRET = dfd["EMRFSA"].sum() / 1000000
    z[2] = CUMRET

    #       East
    # T117(46,IY,IS)=EMRFSA(IY,1)/1.0D6
    z[46] = dfd["EMRFSA"].loc[1] / 1000000

    #       West
    # T117(47,IY,IS)=EMRFSA(IY,2)/1.0D6
    z[47] = dfd["EMRFSA"].loc[2] / 1000000

    # #     Cumulative Banked Allowances
    # #T117(3,IY,IS)=CUMRET
    #   CUMRET = 0.0
    #   IF (IY .GT. 1) THEN
    #      DO ISO2 = 1 , NUM_SO2_GRP
    #         CUMRET = CUMRET + EMELBNK(IY,ISO2) * 0.000001
    #      END DO
    #      T117(48,IY,IS) = EMELBNK(IY,1) * 0.000001
    #      T117(49,IY,IS) = EMELBNK(IY,2) * 0.000001
    #   END IF
    #
    #  EMELBNK  R   4 2 (MNUMYR,MX_SO2) QUNTY  TONS Banked sufur dioxide allowances
    #  DIM  MX_SO2     2      SO2 Compliance Groups
    CUMRET = dfd["EMELBNK"].sum() * 0.000001
    z[3] = CUMRET

    #       East
    # T117(48,IY,IS)=EMELBNK(IY,1)*0.000001
    z[48] = dfd["EMELBNK"].loc[1] * 0.000001

    #       West
    # T117(49,IY,IS)=EMELBNK(IY,2)*0.000001
    z[49] = dfd["EMELBNK"].loc[2] * 0.000001

    # #

    # #   Coal Characteristics

    #     SO2 Content (pounds per million Btu)
    # T117(19,IY,IS)=SO2CNT*2000.0/TLCOAL
    z[19] = SO2CNT * 2000.0 / TLCOAL

    #     Mercury Content (pounds per trillion Btu)
    # T117(20,IY,IS)=HGCNT/TLCOAL
    z[20] = HGCNT / TLCOAL

    # #

    # #   ACI Controls

    # #     Spray Cooling
    # #T117(21,IY,IS)=CUMSC
    z[21] = CUMSC

    # #     Supplemental Fabric Filter
    # #T117(22,IY,IS)=CUMFF
    z[22] = CUMFF

    # #

    #   ACI Mercury Removal (tons)
    # T117(23,IY,IS)=ACMERC(IY)
    z[23] = dfd["ACMERC"]

    #     Coal

    #       Gross Capacity (gigawatts)
    # T117(51,IY,IS)=CUMCCS
    z[51] = dfd["EMELCCS"].loc[MNUMNR]

    #       Derated Capacity (gigawatts)
    # T117(52,IY,IS)=CUMCDR
    z[52] = dfd["EMELCDR"].loc[MNUMNR]

    #       Generation (billion kilowatthours)
    # T117(53,IY,IS)=SUM(UGENSQ(1:MNUMNR-1,IY))-SUM(UGENSQ_ALT(1:MNUMNR-1,IY))
    z[53] = (
        dfd["UGENSQ"].loc[1 : MNUMNR - 1, :].sum()
        - dfd["UGENSQ_ALT"].loc[1 : MNUMNR - 1, :].sum()
    )

    #     Natural Gas

    #       Gross Capacity (gigawatts)
    # T117(56,IY,IS)=CUMGCS
    z[56] = dfd["EMELGCS"].loc[MNUMNR]

    #       Derated Capacity (gigawatts)
    # T117(57,IY,IS)=CUMGDR
    z[57] = dfd["EMELGDR"].loc[MNUMNR]

    #       Generation (billion kilowatthours)
    # T117(58,IY,IS)=SUM(UGENA2(1:MNUMNR-1,IY))-SUM(UGENA2_ALT(1:MNUMNR-1,IY))
    z[58] = (
        dfd["UGENA2"].loc[1 : MNUMNR - 1, :].sum()
        - dfd["UGENA2_ALT"].loc[1 : MNUMNR - 1, :].sum()
    )

    # #

    # #   Acid Gas Controls

    # #     Dry Sorbent Injection (gigawatts)
    # #T117(54,IY,IS)=CUMDSI
    z[54] = CUMDSI

    # #

    # #   Heat Rate Improvement (gigawatts)
    # #T117(55,IY,IS)=CUMHRI
    z[55] = CUMHRI

    # #

    # #   Allowance Revenues (billion #### dollars)

    #       Nitrogen Oxides
    # z[33]=z[55]*0.0
    # T117(33,IY,IS)=T117(33,IY,IS)+T117(INOX+27,IY,IS)*(EMRFNA(INOX,IY)*0.001)*0.001
    # for INOX in range ( 2 , dfd['NOX_D_GRP_rwpre']+1):
    #
    # !   Allowance Revenues
    #            T117(33,IY,IS) = T117( 8,IY,IS) * (EMRFNA(1,IY) * 0.001) * 0.001
    #           DO INOX = 2 , NOX_D_GRP
    #            T117(33,IY,IS) = T117(33,IY,IS) + T117(INOX+27,IY,IS) *  &
    #                            (EMRFNA(INOX,IY) * 0.001) * 0.001
    #           END DO
    #            T117(34,IY,IS) = T117( 5,IY,IS) * T117( 4,IY,IS) * 0.001
    #            T117(35,IY,IS) = T117(16,IY,IS) * T117(18,IY,IS) * 0.001
    #            T117(36,IY,IS) = T117(24,IY,IS) * T117(25,IY,IS) * 0.001
    #            T117(37,IY,IS) = FSUM(T117(33,IY,IS),4)
    #          ENDDO
    #
    # EMRFNA           R    4 2 (NOXGRP,MNUMYR)                     QUNTY  mTONS                Nitrogen Oxide allowances

    z[33] = z[8] * (dfd["EMRFNA"].loc[1][:61] * 0.001) * 0.001

    # TODO: check NOXGRP
    # NOXGRP = 5
    # for INOX in (2, NOX_D_GRP+1):
    #     z[33] += z[INOX+27] * (dfd['EMRFNA'].loc[INOX] * 0.001) * 0.001

    # #       Sulfur Dioxide
    # #T117(34,IY,IS)=T117(5,IY,IS)*T117(4,IY,IS)*0.001
    z[34] = z[5] * z[4] * 0.001

    # #       Mercury
    # #T117(35,IY,IS)=T117(16,IY,IS)*T117(18,IY,IS)*0.001
    z[35] = z[16] * z[18] * 0.001

    # #       Carbon
    # #T117(36,IY,IS)=T117(24,IY,IS)*T117(25,IY,IS)*0.001
    # z[36] = z[25] * 0.001
    z[36] = z[24] * z[25] * 0.001

    # #          Total
    # #T117(37,IY,IS)=FSUM(T117(33,IY,IS),4)
    z[37] = z[33] + z[34] + z[35] + z[36]

    return z
