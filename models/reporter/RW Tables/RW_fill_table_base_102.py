# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM

SZO rewrote on 8/29/2024

"""

import numpy as np
import pandas as pd


def fill_table_base_102(dfd, table_spec, table_id):
    """Fill table for  Alternative Fuels Table

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
    * Prices of Soybean Oil and Corn are adjusted by AGSCALPR rather than SCALPR2! This seems to be
      true for all biomass prices!

    * Everything in QBLK gets divided by 1000 in ftab.f at the beginning of the program!

    * ftab.f SUBROUTINE CAPFLATE has adjustmens to ETHTOTCD as below which may impact region tables!
        IF (ETHTOTCD(11,IY) .EQ. 0.0) THEN
            ETHTOTCD(11,IY) = CRNETHCD(11,IY) + CLLETHCD(11,IY) + OTHETHCD(11,IY) + (ETHIMP(11,IY) - ETHEXP(11,IY))/0.9751
            IF (ETHTOTCD(11,IY) .GT. 0.0) THEN
                !find first year with non-zero
                J=0
                DO IR=IY+1,MNUMYR
                IF (ETHTOTCD(11,IR) .NE. 0.0 .AND. J .EQ. 0) THEN
                    J=IR
    !                    write(6,'(" First non-zero year is ",I4)') 1989+J
                ENDIF
                ENDDO
                IF (J .GT. 0) THEN
                    DO IR=1,9
                    ETHTOTCD(IR,IY) = ETHTOTCD(IR,J)/ETHTOTCD(11,J)*ETHTOTCD(11,IY)
                    ENDDO
                ENDIF
            ENDIF
        ENDIF

    """

    z = {}

    MNUMCR = dfd["MNUMCR_rwpre"]

    FYRPRC = int(table_spec["ftab_dat"]["FYRPRC"])  # 2022
    # Get inflation index from 1990->2050
    MC_JPGDP = dfd["MC_JPGDP"].loc[table_spec["first_year"] :]

    # Get inflation index of FYRPRC (2022)
    SCALPR2 = MC_JPGDP.loc[FYRPRC]  # 2022 = column 33

    # Prices of Soybean Oil and Corn are adjusted by AGSCALPR
    # ftab.f
    #  ...
    # AGSCALPR=(MC_DETAIL(100,FYRPRC-1989)/(MC_DETAIL(100,1)*0.97))! $1990*.97=$1987
    #  ...
    # SBO_PRICE = SBO_PRICE * AGSCALPR !esh
    #  ...
    year_idx = table_spec["years"].index(FYRPRC + 1)
    AGSCALPR = dfd["MC_DETAIL"].loc[100, FYRPRC] / (
        dfd["MC_DETAIL"].loc[100, table_spec["first_year"]] * 0.97
    )

    RDAYS = dfd["RDAYS_rwpre"]
    GAL_PER_BBL = dfd["GAL_PER_BBL_rwpre"]

    # ftab.f ----------------
    # !  Map Census divisions to PADD regions - note overall omission of Kentucky, Tennessee, Mississippi, Alabama
    # !       IF (IR .EQ. 1) PADD=7        ! not much in New England, put California here so it's not left out
    # !       IF (IR .EQ. 2) PADD=1        ! most activity of PADD 1 is in NY, NJ, PA
    # !       IF (IR .EQ. 3) PADD=3        ! PADD includes Minnesota
    # !       IF (IR .EQ. 4) PADD=2        ! CD omits Oklahoma, Kentucky, Tennessee, Minnesota
    # !  mapping CD 5 to PADD 9 to get everyone included
    # !       IF (IR .EQ. 5) PADD=4
    # !  Kentucky, Tennessee, Alabama, Mississippi in CD 6 are PADDed elsewhere, so map PADD 9 here:
    # !       IF (IR .EQ. 6) PADD=9
    # !  PADD 4 consists of Texas and Louisiana coast and Gulf of Mexico
    # !       IF (IR .EQ. 7) PADD=5        ! CD includes Oklahoma; omits New Mexico, Mississippi, Alabama
    # !       IF (IR .EQ. 8) PADD=6        ! CD includes Nevada, Arizona, New Mexico
    # !       IF (IR .EQ. 9) PADD=8        ! CD omits Nevada, Arizona; PADD omits California
    # !  PADD 9 in outside the U.S.
    #         PADD=IR
    #         IF (IR .EQ. MNUMCR) PADD=MNUMPR
    #         IF (IR .EQ. 10) CYCLE      ! skip California

    #   Alternative Fuels Table
    #   (frequently reported units)
    #    Supply, Disposition, and Prices
    #
    #   Ethanol Production, undenatured (billion gallons)
    #     Starch (mostly corn)
    # T102(1,IR,IY,IS)=0.9751*CRNETHCD(IR,IY)*RDAYS*GAL_PER_BBL/1000000
    z[1] = 0.9751 * dfd["CRNETHCD"] * RDAYS * GAL_PER_BBL / 1000000

    #     Cellulose Based
    # T102(2,IR,IY,IS)=0.9751*CLLETHCD(IR,IY)*RDAYS*GAL_PER_BBL/1000000.
    z[2] = 0.9751 * dfd["CLLETHCD"] * RDAYS * GAL_PER_BBL / 1000000.0

    #     Advanced
    # T102(3,IR,IY,IS)=0.9751*OTHETHCD(IR,IY)*RDAYS*GAL_PER_BBL/1000000.
    z[3] = 0.9751 * dfd["OTHETHCD"] * RDAYS * GAL_PER_BBL / 1000000.0

    #     Imports
    # T102(5,IR,IY,IS)=ETHIMP(IR,IY)*RDAYS*GAL_PER_BBL/1000000.
    z[5] = dfd["ETHIMP"] * RDAYS * GAL_PER_BBL / 1000000.0

    #        Cellulosic
    # T102(6,IR,IY,IS)=T102(5,IR,IY,IS)*CELLIMPFRAC(IR,IY)
    z[6] = z[5] * dfd["CELLIMPFRAC"]

    #        Non-cellulosic
    # T102(7,IR,IY,IS)=T102(5,IR,IY,IS)*(1.0-CELLIMPFRAC(IR,IY))
    z[7] = z[5] * (1.0 - dfd["CELLIMPFRAC"])

    #     Exports
    # T102(8,IR,IY,IS)=ETHEXP(IR,IY)*RDAYS*GAL_PER_BBL/1000000.
    z[8] = dfd["ETHEXP"] * RDAYS * GAL_PER_BBL / 1000000.0

    #        Total
    # T102(9,IR,IY,IS)=FSUM(T102(1,IR,IY,IS),5)-T102(8,IR,IY,IS)
    z[9] = z[1] + z[2] + z[3] + z[5] - z[8]

    #   Biobutanol Production from Corn
    # T102(118,IR,IY,IS)=RFBIOBUTECD(IR,IY)*RDAYS*GAL_PER_BBL/1000000.
    z[118] = dfd["RFBIOBUTECD"] * RDAYS * GAL_PER_BBL / 1000000.0

    #   Ethanol Plant Capacity

    #     Corn Ethanol
    # T102(103,IR,IY,IS)=CRNCAPCD(IR,IY)*RDAYS*GAL_PER_BBL/1000000.
    z[103] = dfd["CRNCAPCD"] * RDAYS * GAL_PER_BBL / 1000000.0

    #     Cellulosic Ethanol
    # T102(105,IR,IY,IS)=CLLCAPCD(IR,IY)*RDAYS*GAL_PER_BBL/1000000.
    z[105] = dfd["CLLCAPCD"] * RDAYS * GAL_PER_BBL / 1000000.0

    #     Advanced Ethanol
    # T102(106,IR,IY,IS)=ADVCAPCD(IR,IY)*RDAYS*GAL_PER_BBL/1000000.
    z[106] = dfd["ADVCAPCD"] * RDAYS * GAL_PER_BBL / 1000000.0

    #       Total
    # T102(107,IR,IY,IS)=FSUM(T102(103,IR,IY,IS),4)
    z[107] = z[103] + z[105] + z[106]

    #   Capacity Utilization

    #     Corn Ethanol
    # T102(108,IR,IY,IS)=T102(1,IR,IY,IS)/T102(103,IR,IY,IS)
    z[108] = z[1] / z[103]

    #     Cellulosic Ethanol
    # T102(110,IR,IY,IS)=T102(2,IR,IY,IS)/T102(105,IR,IY,IS)
    z[110] = z[2] / z[105]

    #     Advanced Ethanol
    # T102(111,IR,IY,IS)=T102(3,IR,IY,IS)/T102(106,IR,IY,IS)
    z[111] = z[3] / z[106]

    #

    #   Ethanol Consumption, denatured (billion gallons)

    #     Used in E85 1/  Refinery model view
    # T102(13,IR,IY,IS)=ETHE85CD(IR,IY)*RDAYS*GAL_PER_BBL/1000000.
    z[13] = dfd["ETHE85CD"] * RDAYS * GAL_PER_BBL / 1000000.0

    #     Used in E85 1/
    # T102(10,IR,IY,IS)=QETTR(IR,IY)/CFE85Q(IY)*GAL_PER_BBL*ETHNE85
    # z[10] = dfd['QETTR'] / dfd['CFE85Q'] * GAL_PER_BBL* dfd['ETHNE85']
    z[10] = (
        dfd["QETTR"]
        / dfd["CFE85Q"].values
        * GAL_PER_BBL
        * dfd["ETHNE85"].squeeze()
        * 0.001
    )

    #     Used in Gasoline Blending
    # T102(11,IR,IY,IS)=(ETHTOTCD(IR,IY)-ETHE85CD(IR,IY))*RDAYS*GAL_PER_BBL/1000000.
    z[11] = (dfd["ETHTOTCD"] - dfd["ETHE85CD"]) * RDAYS * GAL_PER_BBL / 1000000.0

    #        Total
    # T102(12,IR,IY,IS)=ETHTOTCD(IR,IY)*RDAYS*GAL_PER_BBL/1000000.
    z[12] = dfd["ETHTOTCD"] * RDAYS * GAL_PER_BBL / 1000000.0

    #   Biobutanol Consumption
    # T102(119,IR,IY,IS)=QBIOBUTE(IR,IY)*RDAYS*GAL_PER_BBL/1000000.
    z[119] = dfd["QBIOBUTE"] * RDAYS * GAL_PER_BBL / 1000000.0

    #     Total Ethanol Percent of Motor Gasoline Pool
    # T102(15,IR,IY,IS)=(ETHTOTCD(IR,IY)/(RFQMG(IR,IY)*1000))*100
    z[15] = dfd["ETHTOTCD"] / dfd["RFQMG"] * 0.10

    #     Ethanol Splash Blend Percent of Motor Gasoline
    # T102(16,IR,IY,IS)=((ETHTOTCD(IR,IY)-ETHE85CD(IR,IY))/(RFQMG(IR,IY)*1000.-(QETTR(11,IY)/CFE85Q(IY)/365.*1000000.)))*100.
    # z[16] = (dfd['ETHTOTCD'] - dfd['ETHE85CD'])/(dfd['RFQMG'] *1000. - (dfd['QETTR'].loc[11] / dfd['CFE85Q'] / 365. * 1000000.)) * 100.
    #
    # ftab.f ---------------
    # ...
    # T102(1:NTAB102,1:MNUMCR,1:MNUMYR,IS) = 0.0  # NTAB102 = row count of T102?
    # ...
    # !   Splash blend percentage ethanol less E85 ethanol /gasoline less E85 gasoline
    #         IF ((RFQMG(IR,IY)*1000 - ETHE85CD(IR,IY)) .NE. 0.0) &
    #         T102(16,IR,IY,IS) =((ETHTOTCD(IR,IY) - ETHE85CD(IR,IY))/(RFQMG(IR,IY)*1000. - &
    #                              (QETTR(11,IY)/CFE85Q(IY)/365.*1000000.))) * 100.
    #         IF ((RFQMG(IR,IY)*1000 - ETHE85CD(IR,IY)) .NE. 0.0) &
    #         T102(120,IR,IY,IS) =((QBIOBUTE(IR,IY))/(RFQMG(IR,IY)*1000. - &
    #                              (QETTR(11,IY)/CFE85Q(IY)/365.*1000000.))) * 100.
    #         IF ((RFQMG(IR,IY)*1000 - ETHE85CD(IR,IY)) .NE. 0.0) &
    #         T102(121,IR,IY,IS) =((QBIOBUTE(IR,IY) + ETHTOTCD(IR,IY) - ETHE85CD(IR,IY))/(RFQMG(IR,IY)*1000. - &
    #                              (QETTR(11,IY)/CFE85Q(IY)/365.*1000000.))) * 100.
    # ...
    mask = (dfd["RFQMG"] * 1000 - dfd["ETHE85CD"]) == 0.0

    # df11 = (dfd['MPBLK/QETTR'] / dfd['CFE85Q'].values / 365. * 1000000.)
    # Create a df with 11 rows, replicating the values
    #   (Everything in QBLK gets divided by 1000 in ftab.f at the beginning of the program !)
    df11 = dfd["QETTR"].loc[11] / 1000 / dfd["CFE85Q"] / 365 * 1000000
    df11 = pd.DataFrame([df11] * 11)
    df11.index = df11.index + 1

    z[16] = ((dfd["ETHTOTCD"] - dfd["ETHE85CD"]) / (dfd["RFQMG"] * 1000 - df11)) * 100
    z[16].loc[:, 11:].fillna(0)
    z[16][mask] = 0.0

    # #     Biobutanol Splash Blend Percent of Motor Gasoline
    # #T102(120,IR,IY,IS)=((QBIOBUTE(IR,IY))/(RFQMG(IR,IY)*1000.-(QETTR(11,IY)/CFE85Q(IY)/365.*1000000.)))*100.
    # z[120] = dfd['QBIOBUTE'] / (dfd['RFQMG'] * 1000. - (dfd['QETTR'].loc[11] / dfd['CFE85Q'] / 365.* 1000000.)) * 100.
    z[120] = (dfd["QBIOBUTE"] / (dfd["RFQMG"] * 1000 - df11)) * 100.0
    z[120].loc[:, 11:].fillna(0)
    z[120][mask] = 0.0

    #     Ethanol and Biobutanol Percent of Motor Gasoline
    # T102(121,IR,IY,IS)=((QBIOBUTE(IR,IY)+ETHTOTCD(IR,IY)-ETHE85CD(IR,IY))/(RFQMG(IR,IY)*1000.-(QETTR(11,IY)/CFE85Q(IY)/365.*1000000.)))*100.
    # z[121] = (dfd['QBIOBUTE'] + dfd['ETHTOTCD'] - dfd['ETHE85CD'])/(dfd['RFQMG'] * 1000. - (dfd['QETTR'].loc[11] / dfd['CFE85Q'] / 365. * 1000000.)) * 100.
    z[121] = (
        (dfd["QBIOBUTE"] + dfd["ETHTOTCD"] - dfd["ETHE85CD"])
        / (dfd["RFQMG"] * 1000 - df11)
    ) * 100.0
    z[121].loc[:, 11:].fillna(0)
    z[121][mask] = 0.0

    #     E85 Fuel Availability (percent)
    # T102(14,IR,IY,IS)=E85AVAIL(IR,IY)*100.
    z[14] = dfd["E85AVAIL"] * 100.0

    #

    #   Biodiesel (billion gallons)

    #     Production of Biodiesel from

    #       Seed Based Oil
    # T102(17,IR,IY,IS)=BIMQTYCD(1,IR,IY)*GAL_PER_BBL*RDAYS/1000000.
    z[17] = dfd["BIMQTYCD"].loc[1] * GAL_PER_BBL * RDAYS / 1000000.0

    #       Yellow/White Grease
    # T102(18,IR,IY,IS)=BIMQTYCD(3,IR,IY)*GAL_PER_BBL*RDAYS/1000000.
    z[18] = dfd["BIMQTYCD"].loc[3] * GAL_PER_BBL * RDAYS / 1000000.0

    #          Total Production
    # T102(26,IR,IY,IS)=FSUM(T102(17,IR,IY,IS),2)
    z[26] = z[17] + z[18]

    #       Imports
    # T102(20,IR,IY,IS)=BIODIMP(IR,IY)*GAL_PER_BBL*RDAYS/1000000.
    z[20] = dfd["BIODIMP"] * GAL_PER_BBL * RDAYS / 1000000.0

    #       Exports
    # T102(112,IR,IY,IS)=BIODEXP(IR,IY)*GAL_PER_BBL*RDAYS/1000000.
    z[112] = dfd["BIODEXP"] * GAL_PER_BBL * RDAYS / 1000000.0

    #       Biodiesel from Palm Oil Imports
    # T102(21,IR,IY,IS)=BIMQTYCD(4,IR,IY)*GAL_PER_BBL*RDAYS/1000000.
    z[21] = dfd["BIMQTYCD"].loc[4] * GAL_PER_BBL * RDAYS / 1000000.0

    #          Total
    # T102(22,IR,IY,IS)=FSUM(T102(17,IR,IY,IS),5)-T102(112,IR,IY,IS)
    z[22] = z[17] + z[18] + z[20] + z[21] - z[112]

    #     Percent of Distillate Pool
    # T102(23,IR,IY,IS)=T102(22,IR,IY,IS)/RFQDS(IR,IY)*100./GAL_PER_BBL/RDAYS
    z[23] = z[22] / dfd["RFQDS"] * 100.0 / GAL_PER_BBL / RDAYS

    #

    #   Renewable Diesel/Gasoline (billion gallons)

    # T102(28,IR,IY,IS)=SBOQGD(PADD,IY)*GAL_PER_BBL*RDAYS/1000000.
    z[28] = dfd["SBOQGD"] * GAL_PER_BBL * RDAYS / 1000000.0
    # --- PADD has 10 regions only while CD = 11 ---
    # ftab.f
    # ...
    # IF (IR .EQ. MNUMCR) PADD=MNUMPR
    # ...
    z[28].loc[11] = z[28].loc[10]

    #       Yellow/White Grease
    # T102(29,IR,IY,IS)=WGRQGD(PADD,IY)*GAL_PER_BBL*RDAYS/1000000.
    z[29] = dfd["WGRQGD"] * GAL_PER_BBL * RDAYS / 1000000.0
    z[29].loc[11] = z[29].loc[10]

    #          Total Production
    # T102(122,IR,IY,IS)=FSUM(T102(28,IR,IY,IS),2)
    z[122] = z[28] + z[29]

    #       Imports
    # T102(31,IR,IY,IS)=RENEWDIMP(IR,IY)*GAL_PER_BBL*RDAYS/1000000.
    z[31] = dfd["RENEWDIMP"] * GAL_PER_BBL * RDAYS / 1000000.0

    #          Total
    # T102(32,IR,IY,IS)=FSUM(T102(28,IR,IY,IS),4)
    z[32] = z[28] + z[29] + z[31]

    #       Percent of Distillate Pool
    # T102(123,IR,IY,IS)=T102(32,IR,IY,IS)/RFQDS(IR,IY)*100./GAL_PER_BBL/RDAYS
    z[123] = z[32] / dfd["RFQDS"] * 100.0 / GAL_PER_BBL / RDAYS

    #     Distillates to Diesel
    # T102(33,IR,IY,IS)=GRD2DSQTY(PADD,IY)*GAL_PER_BBL*RDAYS/1000000.
    z[33] = dfd["GRD2DSQTY"] * GAL_PER_BBL * RDAYS / 1000000.0
    z[33].loc[11] = z[33].loc[10]

    #     Naphthas to Motor Gasoline
    # T102(34,IR,IY,IS)=GRN2MGQTY(PADD,IY)*GAL_PER_BBL*RDAYS/1000000.
    z[34] = dfd["GRN2MGQTY"] * GAL_PER_BBL * RDAYS / 1000000.0
    z[34].loc[11] = z[34].loc[10]

    #   Biomass-based Diesel Percent of Distillate Pool
    # T102(124,IR,IY,IS)=(T102(22,IR,IY,IS)+T102(32,IR,IY,IS))/RFQDS(IR,IY)*100./GAL_PER_BBL/RDAYS
    z[124] = ((z[22] + z[32]) / dfd["RFQDS"] * 100.0 / GAL_PER_BBL / RDAYS).fillna(0)

    #
    #   Renewable Jet Fuel (HEFA-SPK) (billion gallons)

    #     Feedstocks Used

    #       Seed Oil Based
    # T102(125,IR,IY,IS)=SBOQRJH(PADD,IY)*GAL_PER_BBL*RDAYS/1000000.
    z[125] = dfd["SBOQRJH"] * GAL_PER_BBL * RDAYS / 1000000.0
    z[125].loc[11] = z[125].loc[10]

    #       Yellow/White Grease
    # T102(126,IR,IY,IS)=WGRQRJH(PADD,IY)*GAL_PER_BBL*RDAYS/1000000.
    z[126] = dfd["WGRQRJH"] * GAL_PER_BBL * RDAYS / 1000000.0
    z[126].loc[11] = z[126].loc[10]

    #   Total Production
    # T102(127,IR,IY,IS)=FSUM(T102(125,IR,IY,IS),2)
    z[127] = z[125] + z[126]

    #   SAF Percent of Jet Pool
    # T102(128,IR,IY,IS)=T102(127,IR,IY,IS)/RFQJF(IR,IY)*100./GAL_PER_BBL/RDAYS
    z[128] = z[127] / dfd["RFQJF"] * 100.0 / GAL_PER_BBL / RDAYS

    #   HEFA-SPK SAF to Jet
    # T102(129,IR,IY,IS)=SAF2JTQTY(PADD,IY)*GAL_PER_BBL*RDAYS/1000000.
    z[129] = dfd["SAF2JTQTY"] * GAL_PER_BBL * RDAYS / 1000000.0
    z[129].loc[11] = z[129].loc[10]

    #
    #   WARNING - Data below at PADD level
    #   in million barrels per day

    #   Liquids from Biomass

    #     Fischer-Tropsch Process

    #       Paraffinic Naphtha
    # T102(35,IR,IY,IS)=BTLFRAC(1,PADD,IY)/1000.
    z[35] = dfd["BTLFRAC"].loc[1] / 1000.0
    z[35].loc[11] = z[35].loc[10]

    #       Blendable Naphtha
    # T102(36,IR,IY,IS)=BTLFRAC(2,PADD,IY)/1000.
    z[36] = dfd["BTLFRAC"].loc[2] / 1000.0
    z[36].loc[11] = z[36].loc[10]

    #       Distillate Fuel Oil
    # T102(37,IR,IY,IS)=(BTLFRAC(3,PADD,IY)+BTLFRAC(4,PADD,IY))/1000.
    z[37] = (dfd["BTLFRAC"].loc[3] + dfd["BTLFRAC"].loc[4]) / 1000.0
    z[37].loc[11] = z[37].loc[10]

    # T102(38,IR,IY,IS)=FSUM(T102(35,IR,IY,IS),3)
    z[38] = z[35] + z[36] + z[37]
    #     Pyrolysis Oils
    # T102(39,IR,IY,IS)=UBAVOL(PADD,IY)/1000.
    z[39] = dfd["UBAVOL"] / 1000.0
    z[39].loc[11] = z[39].loc[10]

    # T102(42,IR,IY,IS)=T102(38,IR,IY,IS)+T102(39,IR,IY,IS)
    z[42] = z[38] + z[39]
    #       Motor Gasoline
    # T102(113,IR,IY,IS)=UBAVOLMG(PADD,IY)/1000.
    z[113] = dfd["UBAVOLMG"] / 1000.0
    z[113].loc[11] = z[113].loc[10]

    #       Distillates
    # T102(114,IR,IY,IS)=UBAVOLDS(PADD,IY)/1000.
    z[114] = dfd["UBAVOLDS"] / 1000.0
    z[114].loc[11] = z[114].loc[10]

    #       Other (Petrochemical Feedstocks)
    # T102(115,IR,IY,IS)=T102(39,IR,IY,IS)-T102(113,IR,IY,IS)-T102(114,IR,IY,IS)
    z[115] = z[39] - z[113] - z[114]

    #   Liquids from Coal

    #     Paraffinic Naphtha
    # T102(45,IR,IY,IS)=CTLFRAC(1,PADD,IY)/1000.
    z[45] = dfd["CTLFRAC"].loc[1] / 1000.0
    z[45].loc[11] = z[45].loc[10]

    #     Blendable Naphtha
    # T102(46,IR,IY,IS)=CTLFRAC(2,PADD,IY)/1000.
    z[46] = dfd["CTLFRAC"].loc[2] / 1000.0
    z[46].loc[11] = z[46].loc[10]

    #     Distillate Fuel Oil
    # T102(47,IR,IY,IS)=(CTLFRAC(3,PADD,IY)+CTLFRAC(4,PADD,IY))/1000.
    z[47] = (dfd["CTLFRAC"].loc[3] + dfd["CTLFRAC"].loc[4]) / 1000.0
    z[47].loc[11] = z[47].loc[10]

    #     Liquids from Coal
    # T102(48,IR,IY,IS)=FSUM(T102(45,IR,IY,IS),3)
    z[48] = z[45] + z[46] + z[47]

    #     Coal Part

    #       Paraffinic Naphtha
    # T102(51,IR,IY,IS)=CBTLFRAC(1,1,PADD,IY)/1000.
    z[51] = dfd["CBTLFRAC"].loc[1].loc[1] / 1000.0
    z[51].loc[11] = z[51].loc[10]

    #       Blendable Naphtha
    # T102(52,IR,IY,IS)=CBTLFRAC(1,2,PADD,IY)/1000.
    z[52] = dfd["CBTLFRAC"].loc[1].loc[2] / 1000.0
    z[52].loc[11] = z[52].loc[10]

    #       Distillate Fuel Oil
    # T102(53,IR,IY,IS)=(CBTLFRAC(1,3,PADD,IY)+CBTLFRAC(1,4,PADD,IY))/1000.
    z[53] = (dfd["CBTLFRAC"].loc[1].loc[3] + dfd["CBTLFRAC"].loc[1].loc[4]) / 1000.0
    z[53].loc[11] = z[53].loc[10]

    # T102(57,IR,IY,IS)=FSUM(T102(51,IR,IY,IS),3)
    z[57] = z[51] + z[52] + z[53]
    #     Biomass Part

    #       Paraffinic Naphtha
    # T102(54,IR,IY,IS)=CBTLFRAC(2,1,PADD,IY)/1000.
    z[54] = dfd["CBTLFRAC"].loc[2].loc[1] / 1000.0
    z[54].loc[11] = z[54].loc[10]

    #       Blendable Naphtha
    # T102(55,IR,IY,IS)=CBTLFRAC(2,2,PADD,IY)/1000.
    z[55] = dfd["CBTLFRAC"].loc[2].loc[2] / 1000.0
    z[55].loc[11] = z[55].loc[10]

    #       Distillate Fuel Oil
    # T102(56,IR,IY,IS)=(CBTLFRAC(2,3,PADD,IY)+CBTLFRAC(2,4,PADD,IY))/1000.
    z[56] = (dfd["CBTLFRAC"].loc[2].loc[3] + dfd["CBTLFRAC"].loc[2].loc[4]) / 1000.0
    z[56].loc[11] = z[56].loc[10]

    #  Biomass Part
    # T102(58,IR,IY,IS)=FSUM(T102(54,IR,IY,IS),3)
    z[58] = z[54] + z[55] + z[56]

    # T102(59,IR,IY,IS)=FSUM(T102(57,IR,IY,IS),2)
    z[59] = z[57] + z[58]

    #     Paraffinic Naphtha
    # T102(64,IR,IY,IS)=GTLFRAC(1,PADD,IY)/1000.
    z[64] = dfd["GTLFRAC"].loc[1] / 1000.0
    z[64].loc[11] = z[64].loc[10]

    #     Blendable Naphtha
    # T102(65,IR,IY,IS)=GTLFRAC(2,PADD,IY)/1000.
    z[65] = dfd["GTLFRAC"].loc[2] / 1000.0
    z[65].loc[11] = z[65].loc[10]

    #     Distillate Fuel Oil
    # T102(66,IR,IY,IS)=(GTLFRAC(3,PADD,IY)+GTLFRAC(4,PADD,IY))/1000.
    z[66] = (dfd["GTLFRAC"].loc[3] + dfd["GTLFRAC"].loc[4]) / 1000.0
    z[66].loc[11] = z[66].loc[10]

    #   Liquids from Gas
    # T102(67,IR,IY,IS)=FSUM(T102(64,IR,IY,IS),3)
    z[67] = z[64] + z[65] + z[66]
    #
    #   in billion gallons

    #     Fischer-Tropsch Process
    # T102(40,IR,IY,IS)=T102(38,IR,IY,IS)*365.*GAL_PER_BBL/1000.
    z[40] = z[38] * 365.0 * GAL_PER_BBL / 1000.0

    # T102(41,IR,IY,IS)=UBAVOL(PADD,IY)/1000.*365.*42./1000.
    z[41] = dfd["UBAVOL"] / 1000.0 * RDAYS * GAL_PER_BBL / 1000.0
    z[41].loc[11] = z[41].loc[10]

    #   Liquids from Biomass
    # T102(43,IR,IY,IS)=T102(40,IR,IY,IS)+T102(41,IR,IY,IS)
    z[43] = z[40] + z[41]

    # T102(49,IR,IY,IS)=T102(48,IR,IY,IS)*365.*42./1000.
    # T102(60,IR,IY,IS)=T102(57,IR,IY,IS)*365.*42./1000.
    # T102(61,IR,IY,IS)=T102(58,IR,IY,IS)*365.*42./1000.
    # T102(62,IR,IY,IS)=FSUM(T102(60,IR,IY,IS),2)
    # T102(68,IR,IY,IS)=T102(67,IR,IY,IS)*365.*42./1000.
    z[49] = z[48] * RDAYS * GAL_PER_BBL / 1000.0
    z[60] = z[57] * RDAYS * GAL_PER_BBL / 1000.0
    z[61] = z[58] * RDAYS * GAL_PER_BBL / 1000.0
    z[62] = z[60] + z[61]
    z[68] = z[67] * RDAYS * GAL_PER_BBL / 1000.0

    # T102(70,IR,IY,IS)=T102(44,IR,IY,IS)+T102(50,IR,IY,IS)+T102(63,IR,IY,IS)+T102(69,IR,IY,IS)
    # T102(44,IR,IY,IS)=(T102(37,IR,IY,IS)+T102(114,IR,IY,IS))/TDIESEL(IR,IY)*100.*1000.
    # T102(50,IR,IY,IS)=T102(47,IR,IY,IS)/TDIESEL(IR,IY)*100.*1000.
    # T102(63,IR,IY,IS)=(T102(53,IR,IY,IS)+T102(56,IR,IY,IS))/TDIESEL(IR,IY)*100.*1000.
    # T102(69,IR,IY,IS)=T102(66,IR,IY,IS)/TDIESEL(IR,IY)*100.*1000.

    z[44] = (z[37] + z[114]) / dfd["TDIESEL"] * 100000.0
    z[50] = z[47] / dfd["TDIESEL"] * 100000.0
    z[63] = (z[53] + z[56]) / dfd["TDIESEL"] * 100000.0
    z[69] = z[66] / dfd["TDIESEL"] * 100000.0
    z[70] = z[44] + z[50] + z[63] + z[69]

    # T102(93,IR,IY,IS)=CORNCD(3,IR,IY)
    # T102(98,IR,IY,IS)=GRAINCD(IR,IY)
    # T102(101,IR,IY,IS)=SBOQTYCD(IR,IY)*42.*RDAYS/1000000.
    # T102(102,IR,IY,IS)=PLMQTYCD(IR,IY)*42.*RDAYS/1000000.
    z[93] = dfd["CORNCD"].loc[3]
    z[98] = dfd["GRAINCD"]
    z[101] = dfd["SBOQTYCD"] * RDAYS * GAL_PER_BBL / 1000000.0
    z[102] = dfd["PLMQTYCD"] * RDAYS * GAL_PER_BBL / 1000000.0

    # No implementation for:
    # Agricultural Stuff
    #   Total Corn Crop (million bushels)	92
    #
    #   Fraction of Corn going to Ethanol (fraction)	94
    #   Net Corn Exports (million bushels)	95
    #   Corn Acres (million acres)	96
    #   Corn Yield (bushels per acre)	97
    #   DDGS Sold as Feed	99
    #   DDGS Sold as Fuel	100

    #  Set to 0
    z[92] = z[93] * 0
    z[94] = z[93] * 0
    z[95] = z[93] * 0
    z[96] = z[93] * 0
    z[97] = z[93] * 0
    z[99] = z[93] * 0
    z[100] = z[93] * 0

    #    Prices (#### dollars per gallon)

    # !  These are Census division variables again
    # !       --- Prices
    #       T102( 71,IR,IY,IS) = BRENT_PRICE(IY) / 42.
    #       T102( 72,IR,IY,IS) = WTI_PRICE(IY) / 42.
    #       T102(116,IR,IY,IS) = IT_WOP(IY,1) / 42.
    #       T102( 73,IR,IY,IS) = PETHM(IR,IY) / 42.
    #       T102( 74,IR,IY,IS) = (PALMG(IR,IY) + RFENVFX(IR,IY,2)) / 42.     ! not PALBOB(IR,IY)
    #       T102( 75,IR,IY,IS) = T102( 73,IR,IY,IS) / T102( 74,IR,IY,IS)
    #       T102( 76,IR,IY,IS) = PMGTR(IR,IY) * CFMGQ(IY) / 42.
    #       T102( 77,IR,IY,IS) = PETTR(IR,IY) * CFE85Q(IY) / 42.
    #       T102( 78,IR,IY,IS) = SBO_PRICE(IR,IY) / 42.
    #       T102( 79,IR,IY,IS) = YGR_PRICE(IR,IY) / 42.
    #       T102( 80,IR,IY,IS) = BIODPRICE(IR,IY) / 42.
    #
    #       T102( 81,IR,IY,IS) = CRNPRICE(IR,IY)

    z[71] = dfd["BRENT_PRICE"] / GAL_PER_BBL * SCALPR2
    # Replicate national to to CD
    z[71] = pd.DataFrame([z[71]] * 11).reset_index(drop=True)
    z[71].index += 1

    z[72] = dfd["WTI_PRICE"] / GAL_PER_BBL * SCALPR2

    # Replicate national to to CD
    z[72] = pd.DataFrame([z[72]] * 11).reset_index(drop=True)
    z[72].index += 1

    z[73] = dfd["PETHM"] / GAL_PER_BBL * SCALPR2

    z[74] = (
        (dfd["PALMG"] + dfd["RFENVFX"].loc[1:11, 2, :].groupby(level=0).sum())
        / GAL_PER_BBL
        * SCALPR2
    )
    z[74].replace([np.inf, -np.inf], 0, inplace=True)

    z[75] = z[73] / z[74]
    z[75].replace([np.inf, -np.inf], 0, inplace=True)
    z[75] = z[75].fillna(0)

    # CFMGQR    4 1 (MNUMYR)CONFAC BTUBBL MG    Motor Gasoline
    #   Only national prices!
    df_CFMGQ11 = dfd["CFMGQ"]
    df_CFMGQ11 = pd.DataFrame([df_CFMGQ11] * 11).reset_index(drop=True)
    df_CFMGQ11.index = df_CFMGQ11.index + 1
    z[76] = (dfd["AMPBLK/PMGTR"] * df_CFMGQ11 / GAL_PER_BBL) * SCALPR2

    z[77] = dfd["AMPBLK/PETTR"] * dfd["CFE85Q"].values / GAL_PER_BBL * SCALPR2

    # Prices of Soybean Oil and Corn are adjusted by AGSCALPR !!!
    z[78] = dfd["SBO_PRICE"] / GAL_PER_BBL * AGSCALPR  # SCALPR2

    z[79] = dfd["YGR_PRICE"] / GAL_PER_BBL * SCALPR2
    z[80] = dfd["BIODPRICE"] / GAL_PER_BBL * SCALPR2

    # Prices of Soybean Oil and Corn are adjusted by AGSCALPR !!!
    z[81] = dfd["CRNPRICE"] * AGSCALPR  # SCALPR2

    z[82] = z[81] * 0  # No implementation in ftab

    z[116] = dfd["IT_WOP"].loc[1] / GAL_PER_BBL * SCALPR2
    # Replicate national to to CD
    z[116] = pd.DataFrame([z[116]] * 11).reset_index(drop=True)
    z[116].index = z[116].index + 1

    #   Prices (#### dollars per million Btu)

    # T102( 83,IR,IY,IS) = BRENT_PRICE(IY) / CFCRDLTSWT(IY)
    # T102( 84,IR,IY,IS) = WTI_PRICE(IY) / CFCRDLTSWT(IY)
    # T102(117,IR,IY,IS) = IT_WOP(IY,2)
    # T102( 85,IR,IY,IS) = PETHM(IR,IY) / CFETQ(IY)
    # T102( 86,IR,IY,IS) = PMGTR(IR,IY)
    # T102( 87,IR,IY,IS) = PETTR(IR,IY)
    # T102( 88,IR,IY,IS) = SBO_PRICE(IR,IY) / CFBIOD(IY)
    # T102( 89,IR,IY,IS) = YGR_PRICE(IR,IY) / CFBIOD(IY)
    # T102( 90,IR,IY,IS) = BIODPRICE(IR,IY) / CFBIOD(IY)
    # T102( 91,IR,IY,IS) = CRNPRICE(IR,IY) / CFCORN * 1000000.

    z[83] = dfd["BRENT_PRICE"] / dfd["CFCRDLTSWT"] * SCALPR2
    # ---These prices are national - we need replicate to CD!
    z[83] = pd.DataFrame([z[83]] * 11).reset_index(drop=True)
    z[83].index = df11.index + 1

    z[84] = dfd["WTI_PRICE"] / dfd["CFCRDLTSWT"] * SCALPR2
    # ---These prices are national - we need replicate to CD!
    z[84] = pd.DataFrame([z[84]] * 11).reset_index(drop=True)
    z[84].index = df11.index + 1

    z[117] = dfd["IT_WOP"].loc[2] * SCALPR2
    # ---These prices are national - we need replicate to CD!
    z[117] = pd.DataFrame([z[117]] * 11).reset_index(drop=True)
    z[117].index += 1

    z[85] = dfd["PETHM"] / dfd["CFETQ"].values * SCALPR2
    z[86] = dfd["AMPBLK/PMGTR"] * SCALPR2
    z[87] = dfd["AMPBLK/PETTR"] * SCALPR2

    # Prices of Soybean Oil and Corn are adjusted by AGSCALPR !!!
    z[88] = dfd["SBO_PRICE"] / dfd["CFBIOD"].values * AGSCALPR  # SCALPR2

    z[89] = dfd["YGR_PRICE"] / dfd["CFBIOD"].values * SCALPR2
    z[90] = dfd["BIODPRICE"] / dfd["CFBIOD"].values * SCALPR2

    # Prices of Soybean Oil and Corn are adjusted by AGSCALPR !!!
    z[91] = dfd["CRNPRICE"] / dfd["CFCORN"].iloc[0] * 1000000.0 * AGSCALPR  # SCALPR2

    return z
