# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
Fixed by SZO on 6/30/20124
"""
import numpy as np
import pandas as pd


def fill_table_base_005(dfd, table_spec, table_id):
    """Fill table  Commercial Sector Key Indicators and Consumption

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
    GWh_to_TRIL = dfd["GWh_to_TRIL_rwpre"]
    ELECLOSS = dfd["QTSEL"].loc[MNUMCR] - dfd["QELAS"].loc[MNUMCR]
    FYRPRC = int(table_spec["settings"]["year_index_real_dol_priceyr"])
    SCALPR2 = float(dfd["MC_JPGDP"].loc[FYRPRC])

    #   Commercial Sector Key Indicators and Consumption
    #   (quadrillion Btu, unless otherwise noted)
    #    Key Indicators and Consumption

    #   Key Indicators

    #    Total Floorspace (billion square feet)

    #      Surviving
    # T5(1,IY,IS)=CMUSSURVFLOORTOT(IY)
    z[1] = dfd["CMUSSURVFLOORTOT"]

    #      New Additions
    # T5(2,IY,IS)=CMUSNEWFLOORTOT(IY)
    z[2] = dfd["CMUSNEWFLOORTOT"]

    #        Total
    # T5(3,IY,IS)=T5(1,IY,IS)+T5(2,IY,IS)
    z[3] = z[1] + z[2]

    #

    #    Energy Consumption Intensity (thousand Btu per square foot)
    #      Gross End-use Consumption 1/
    # T5( 4,IY,IS)=((QTSCM(11,IY) - QSTCM(11,IY) - QPVCM(11,IY) + SUM(CGCOMMGEN(11,IY,1:12,2)) * 3.412 * 0.000001) / T5(3,IY,IS)) * 1000.
    #         check for divide by zero
    #  Note: CGCOMMGEN(11,IY,1:12,2) indices order

    # QELCM_rev = QELCM(MNUMCR,IY) - (TRQ_ELEC(2,MNUMCR,IY) +
    # + TRQ_ELEC(3,MNUMCR,IY)      ! transit bus
    # + TRQ_ELEC(4,MNUMCR,IY)     ! school bus
    # + TRQ_ELEC(5,MNUMCR,IY)     ! commercial light truck
    # + TRQ_ELEC(6,MNUMCR,IY)     ! heavy truck
    # + TRQ_ELEC(7,MNUMCR,IY)
    # + TRQ_ELEC(8,MNUMCR,IY)
    # + TRQ_ELEC(9,MNUMCR,IY) )/1000.0 !LC2

    QCOMM_EV = dfd["TRQ_ELEC"].loc[2:9].groupby("MNUMCR").sum().loc[MNUMCR]
    QELCM_rev = dfd["QELCM"].loc[1:9].sum() - QCOMM_EV

    z[4] = (
        dfd["QTSCM"].loc[MNUMCR]
        - QCOMM_EV
        - dfd["QSTCM"].loc[MNUMCR]
        - dfd["QPVCM"].loc[MNUMCR]
        + dfd["CGCOMMGEN"].loc[MNUMCR, 1:12, 2].sum() * GWh_to_TRIL * 0.001
    ) / z[3].replace(0, np.inf)

    #      Delivered Energy Consumption
    # T5(5,IY,IS)=((QTSCM(11,IY)-QSTCM(11,IY)-QPVCM(11,IY))/T5(3,IY,IS))*1000
    # Note: check for z[3]=0
    z[5] = (
        dfd["QTSCM"].loc[MNUMCR]
        - dfd["QSTCM"].loc[MNUMCR]
        - dfd["QPVCM"].loc[MNUMCR]
        - QCOMM_EV
    ) / z[3].replace(0, np.inf)

    #   Energy Consumption by Fuel
    #    Electricity 1/
    #      Space Heating 2/
    # T5(7,IY,IS)=CMUSCONSUMPTION(1,1,IY)
    z[7] = dfd["CMUSCONSUMPTION"].loc[1].loc[1]

    #      Space Cooling 2/
    # T5(8,IY,IS)=CMUSCONSUMPTION(2,1,IY)
    z[8] = dfd["CMUSCONSUMPTION"].loc[2].loc[1]

    #      Water Heating 2/
    # T5(9,IY,IS)=CMUSCONSUMPTION(3,1,IY)
    z[9] = dfd["CMUSCONSUMPTION"].loc[3].loc[1]

    #      Ventilation
    # T5(10,IY,IS)=CMUSCONSUMPTION(4,1,IY)
    z[10] = dfd["CMUSCONSUMPTION"].loc[4].loc[1]

    #      Cooking
    # T5(11,IY,IS)=CMUSCONSUMPTION(5,1,IY)
    z[11] = dfd["CMUSCONSUMPTION"].loc[5].loc[1]

    #      Lighting
    # T5(12,IY,IS)=CMUSCONSUMPTION(6,1,IY)
    z[12] = dfd["CMUSCONSUMPTION"].loc[6].loc[1]

    #      Refrigeration
    # T5(13,IY,IS)=CMUSCONSUMPTION(7,1,IY)
    z[13] = dfd["CMUSCONSUMPTION"].loc[7].loc[1]

    #      Computing
    # T5(14,IY,IS)=CMUSCONSUMPTION(8,1,IY)
    z[14] = dfd["CMUSCONSUMPTION"].loc[8].loc[1]

    #      Office Equipment
    # T5(15,IY,IS)=CMUSCONSUMPTION(9,1,IY)
    z[15] = dfd["CMUSCONSUMPTION"].loc[9].loc[1]

    #      Other Uses 3/
    # T5(16,IY,IS)=QELCM(11,IY)-FSUM(T5(7,IY,IS),9)+SUM(CGCOMMGEN(11,IY,1:12,2))*3.412*0.000001
    onsite = dfd["CGCOMMGEN"].loc[11, 1:12, 2, :].sum() * GWh_to_TRIL * 0.000001
    z[16] = (
        QELCM_rev * 0.001
        - (z[7] + z[8] + z[9] + z[10] + z[11] + z[12] + z[13] + z[14] + z[15])
        + onsite
    )

    #        Gross End-Use Electricity Consumption
    # T5(17,IY,IS)=QELCM(11,IY)+SUM(CGCOMMGEN(11,IY,1:12,2))*3.412*0.000001
    z[17] = QELCM_rev * 0.001 + onsite

    # Purchased Electricity for Electricity Vehicle Charging
    z[94] = QCOMM_EV * 0.001

    #      On-site Generation for Own Use
    # T5(87,IY,IS)=SUM(CGCOMMGEN(11,IY,1:12,2))*3.412*0.000001
    z[87] = onsite

    #        Purchased Electricity at Commercial Location
    # T5(88,IY,IS) = QELCM_rev ! Total net of DG   !LC2
    z[88] = dfd["QELCM"].loc[MNUMCR] * 0.001

    #    Natural Gas
    #      Space Heating 2/
    # T5(18,IY,IS)=CMUSCONSUMPTION(1,2,IY)
    z[18] = dfd["CMUSCONSUMPTION"].loc[1].loc[2]

    #      Space Cooling 2/
    # T5(19,IY,IS)=CMUSCONSUMPTION(2,2,IY)
    z[19] = dfd["CMUSCONSUMPTION"].loc[2].loc[2]

    #      Water Heating 2/
    # T5(20,IY,IS)=CMUSCONSUMPTION(3,2,IY)
    z[20] = dfd["CMUSCONSUMPTION"].loc[3].loc[2]

    #      Cooking
    # T5(21,IY,IS)=CMUSCONSUMPTION(5,2,IY)
    z[21] = dfd["CMUSCONSUMPTION"].loc[5].loc[2]

    #      Other Uses 4/
    # T5(22,IY,IS)=QNGCM(11,IY)-FSUM(T5(18,IY,IS),4)
    z[22] = dfd["QNGCM"].loc[MNUMCR] * 0.001 - (z[18] + z[19] + z[20] + z[21])

    #        Delivered Energy
    # T5(23,IY,IS)=QNGCM(11,IY)
    z[23] = dfd["QNGCM"].loc[MNUMCR] * 0.001

    #    Distillate Fuel Oil
    #      Space Heating 2/
    # T5(24,IY,IS)=CMUSCONSUMPTION(1,3,IY)
    z[24] = dfd["CMUSCONSUMPTION"].loc[1].loc[3]

    #      Water Heating 2/
    # T5(25,IY,IS)=CMUSCONSUMPTION(3,3,IY)
    z[25] = dfd["CMUSCONSUMPTION"].loc[3].loc[3]

    #      Other Uses 5/
    # T5(26,IY,IS)=QDSCM(11,IY)-T5(24,IY,IS)-T5(25,IY,IS)
    z[26] = dfd["QDSCM"].loc[MNUMCR] * 0.001 - z[24] - z[25]

    #        Delivered Energy
    # T5(27,IY,IS)=QDSCM(11,IY)
    z[27] = dfd["QDSCM"].loc[MNUMCR] * 0.001

    #

    #    Marketed Renewables (biomass)
    # T5(29,IY,IS)=QBMCM(11,IY)
    z[29] = dfd["QBMCM"].loc[MNUMCR] * 0.001

    #    Other Fuels 6/
    # T5(28,IY,IS)=QTSCM(11,IY)-QELCM(11,IY)-QNGCM(11,IY)-QDSCM(11,IY)-QSTCM(11,IY)-QBMCM(11,IY)-QPVCM(11,IY)
    z[28] = (
        dfd["QTSCM"].loc[MNUMCR]
        - dfd["QELCM"].loc[MNUMCR]
        - dfd["QNGCM"].loc[MNUMCR]
        - dfd["QDSCM"].loc[MNUMCR]
        - dfd["QSTCM"].loc[MNUMCR]
        - dfd["QBMCM"].loc[MNUMCR]
        - dfd["QPVCM"].loc[MNUMCR]
    ) * 0.001

    #
    #

    #   Energy Consumption by End Use 1/

    #      Space Heating 2/
    # T5(31,IY,IS)=CMUSCONSUMPTION(1,1,IY)+CMUSCONSUMPTION(1,2,IY)+CMUSCONSUMPTION(1,3,IY)
    z[31] = (
        dfd["CMUSCONSUMPTION"].loc[1].loc[1]
        + dfd["CMUSCONSUMPTION"].loc[1].loc[2]
        + dfd["CMUSCONSUMPTION"].loc[1].loc[3]
    )

    #      Space Cooling 2/
    # T5(32,IY,IS)=CMUSCONSUMPTION(2,1,IY)+CMUSCONSUMPTION(2,2,IY)
    z[32] = dfd["CMUSCONSUMPTION"].loc[2].loc[1] + dfd["CMUSCONSUMPTION"].loc[2].loc[2]

    #      Water Heating 2/
    # T5(33,IY,IS)=CMUSCONSUMPTION(3,1,IY)+CMUSCONSUMPTION(3,2,IY)+CMUSCONSUMPTION(3,3,IY)
    z[33] = dfd["CMUSCONSUMPTION"].loc[3].sum()

    #      Ventilation
    # T5(34,IY,IS)=CMUSCONSUMPTION(4,1,IY)
    z[34] = dfd["CMUSCONSUMPTION"].loc[4].loc[1]

    #      Cooking
    # T5(35,IY,IS)=CMUSCONSUMPTION(5,1,IY)+CMUSCONSUMPTION(5,2,IY)
    z[35] = dfd["CMUSCONSUMPTION"].loc[5].loc[1] + dfd["CMUSCONSUMPTION"].loc[5].loc[2]

    #      Lighting
    # T5(36,IY,IS)=CMUSCONSUMPTION(6,1,IY)
    z[36] = dfd["CMUSCONSUMPTION"].loc[6].loc[1]

    #      Refrigeration
    # T5(37,IY,IS)=CMUSCONSUMPTION(7,1,IY)
    z[37] = dfd["CMUSCONSUMPTION"].loc[7].loc[1]

    #      Computing
    # T5(38,IY,IS)=CMUSCONSUMPTION(8,1,IY)
    z[38] = dfd["CMUSCONSUMPTION"].loc[8].loc[1]

    #      Office Equipment
    # T5(39,IY,IS)=CMUSCONSUMPTION(9,1,IY)
    z[39] = dfd["CMUSCONSUMPTION"].loc[9].loc[1]

    #      Other Uses 7/
    # T5(40,IY,IS)=QTSCM(11,IY)-FSUM(T5(31,IY,IS),9)-QSTCM(11,IY)-QPVCM(11,IY)+SUM(CGCOMMGEN(11,IY,1:12,2))*3.412*0.000001
    z[40] = (
        (dfd["QTSCM"].loc[MNUMCR] - QCOMM_EV) * 0.001
        - (z[31] + z[32] + z[33] + z[34] + z[35] + z[36] + z[37] + z[38] + z[39])
        - dfd["QSTCM"].loc[MNUMCR] * 0.001
        - dfd["QPVCM"].loc[MNUMCR] * 0.001
        + dfd["CGCOMMGEN"].loc[11, 1:12, 2, :].sum() * GWh_to_TRIL * 0.000001
    )

    #        Gross End-use Consumption
    # T5(41,IY,IS)=QTSCM(11,IY)-QSTCM(11,IY)-QPVCM(11,IY)+SUM(CGCOMMGEN(11,IY,1:12,2))*3.412*0.000001
    z[41] = (
        dfd["QTSCM"].loc[MNUMCR]
        - QCOMM_EV
        - dfd["QSTCM"].loc[MNUMCR]
        - dfd["QPVCM"].loc[MNUMCR]
    ) * 0.001 + dfd["CGCOMMGEN"].loc[11, 1:12, 2, :].sum() * GWh_to_TRIL * 0.000001

    # Purchased Electricity for Electric Vehicle Charging
    z[93] = QCOMM_EV * 0.001

    #    On-site Generation for Own Use
    # T5(89,IY,IS)=SUM(CGCOMMGEN(11,IY,1:12,2))*3.412*0.000001
    z[89] = dfd["CGCOMMGEN"].loc[11, 1:12, 2, :].sum() * GWh_to_TRIL * 0.000001

    #        Delivered Energy
    # T5(90,IY,IS) = QTSCM(11,IY) - QCOMM_EV -  QSTCM(11,IY) - QPVCM(11,IY) ! Total net of distributed generation !LC2
    z[90] = z[41] + z[93] - z[89]

    #

    #   Electricity Related Losses
    # T5(42,IY,IS)=QELCM(11,IY) / dfd['QELAS(11,IY)*ELECLOSS
    z[42] = z[88] / dfd["QELAS"].loc[MNUMCR] * ELECLOSS

    #

    #   Total Energy Consumption by End Use 1/
    #      Space Heating 2/
    # T5(43,IY,IS)=T5(31,IY,IS)+CMUSCONSUMPTION(1,1,IY)*(QELCM(11,IY)/T5(17,IY,IS)) / dfd['QELAS(11,IY)*ELECLOSS
    z[43] = z[31] + z[7] / z[88]  * z[42] * (QELCM_rev * 0.001 / z[17])

    #      Space Cooling 2/
    # T5(44,IY,IS)=T5(32,IY,IS)+CMUSCONSUMPTION(2,1,IY)*(QELCM(11,IY)/T5(17,IY,IS)) / dfd['QELAS(11,IY)*ELECLOSS
    z[44] = z[32] + z[8] / z[88]  * z[42] * (QELCM_rev * 0.001 / z[17])
    #      Water Heating 2/
    # T5(45,IY,IS)=T5(33,IY,IS)+CMUSCONSUMPTION(3,1,IY)*(QELCM(11,IY)/T5(17,IY,IS)) / dfd['QELAS(11,IY)*ELECLOSS
    z[45] = z[33] + z[9] / z[88]  * z[42] * (QELCM_rev * 0.001 / z[17])

    #      Ventilation
    # T5(46,IY,IS)=T5(34,IY,IS)+CMUSCONSUMPTION(4,1,IY)*(QELCM(11,IY)/T5(17,IY,IS)) / dfd['QELAS(11,IY)*ELECLOSS
    z[46] = z[34] + z[10] / z[88]  * z[42] * (QELCM_rev * 0.001 / z[17])

    #      Cooking
    # T5(47,IY,IS)=T5(35,IY,IS)+CMUSCONSUMPTION(5,1,IY)*(QELCM(11,IY)/T5(17,IY,IS)) / dfd['QELAS(11,IY)*ELECLOSS
    z[47] = z[35] + z[11] / z[88]  * z[42] * (QELCM_rev * 0.001 / z[17])

    #      Lighting
    # T5(48,IY,IS)=T5(36,IY,IS)+CMUSCONSUMPTION(6,1,IY)*(QELCM(11,IY)/T5(17,IY,IS)) / dfd['QELAS(11,IY)*ELECLOSS
    z[48] = z[36] + z[12] / z[88]  * z[42] * (QELCM_rev * 0.001 / z[17])

    #      Refrigeration
    # T5(49,IY,IS)=T5(37,IY,IS)+CMUSCONSUMPTION(7,1,IY)*(QELCM(11,IY)/T5(17,IY,IS)) / dfd['QELAS(11,IY)*ELECLOSS
    z[49] = z[37] + z[13] / z[88]  * z[42] * (QELCM_rev * 0.001 / z[17])

    #      Computing
    # T5(50,IY,IS)=T5(38,IY,IS)+CMUSCONSUMPTION(8,1,IY)*(QELCM(11,IY)/T5(17,IY,IS)) / dfd['QELAS(11,IY)*ELECLOSS
    z[50] = z[38] + z[14] / z[88]  * z[42] * (QELCM_rev * 0.001 / z[17])

    #      Office Equipment
    # T5(51,IY,IS)=T5(39,IY,IS)+CMUSCONSUMPTION(9,1,IY)*(QELCM(11,IY)/T5(17,IY,IS)) / dfd['QELAS(11,IY)*ELECLOSS
    z[51] = z[39] + z[15] / z[88]  * z[42] * (QELCM_rev * 0.001 / z[17])

    #      Other Uses 7/
    # T5(52,IY,IS)=T5(40,IY,IS)+T5(16,IY,IS)*(QELCM(11,IY)/T5(17,IY,IS)) / dfd['QELAS(11,IY)*ELECLOSS
    z[52] = z[40] + z[16] / z[88]  * z[42] * (QELCM_rev * 0.001 / z[17])

    #        Total Gross End-use Consumption
    # T5(53,IY,IS)=T5(41,IY,IS)+T5(42,IY,IS)
    z[53] =  (
        z[43]
        + z[44]
        + z[45]
        + z[46]
        + z[47]
        + z[48]
        + z[49]
        + z[50]
        + z[51]
        + z[52]
    )

    # Purchased Electricity for Electric Vehicle Charging
    z[95] = QCOMM_EV * 0.001 + QCOMM_EV * 0.001 / dfd["QELAS"].loc[MNUMCR] * ELECLOSS #z[95] = z[94] + z[94] / (z[94] + z[17]) * z[42]

    #    On-site Generation for Own Use
    # T5(89,IY,IS)=SUM(CGCOMMGEN(11,IY,1:12,2))*3.412*0.000001
    z[91] = dfd["CGCOMMGEN"].loc[11, 1:12, 2, :].sum() * GWh_to_TRIL * 0.000001

    #   Total Net Own-use Generation
    z[92] = z[53] + z[95]- z[91]


    #   Nonmarketed Renewable Fuels 8/
    #     Solar Thermal
    # T5(30,IY,IS)=QSTCM(11,IY)
    z[30] = dfd["QSTCM"].loc[MNUMCR] * 0.001

    #     Solar Photovoltaic
    # T5(54,IY,IS)=QPVCM(11,IY)
    z[54] = dfd["QPVCM"].loc[MNUMCR] * 0.001

    #     Wind
    # T5(56,IY,IS)=CGCOMMQ(11,IY,11)/1000.
    z[56] = dfd["CGCOMMQ"].loc[MNUMCR].loc[11] / 1000.0

    #       Total
    # T5(55,IY,IS)=QSTCM(11,IY)+QPVCM(11,IY)+CGCOMMQ(11,IY,11)/1000.
    z[55] = z[30] + z[54] + z[56]

    #

    #   Bonus Rows - Internal Only

    #   Electricity Prices by End Use

    #     Space Heating
    # T5(77,IY,IS)=PELSHCM(11,IY)

    z[77] = dfd["PELSHCM"].loc[MNUMCR] * SCALPR2

    #     Space Cooling
    # T5(78,IY,IS)=PELSCCM(11,IY)
    z[78] = dfd["PELSCCM"].loc[MNUMCR] * SCALPR2

    #     Water Heating
    # T5(79,IY,IS)=PELWHCM(11,IY)
    z[79] = dfd["PELWHCM"].loc[MNUMCR] * SCALPR2

    #     Ventilation
    # T5(80,IY,IS)=PELVTCM(11,IY)
    z[80] = dfd["PELVTCM"].loc[MNUMCR] * SCALPR2

    #     Cooking
    # T5(81,IY,IS)=PELCKCM(11,IY)
    z[81] = dfd["PELCKCM"].loc[MNUMCR] * SCALPR2

    #     Lighting
    # T5(82,IY,IS)=PELLTCM(11,IY)
    z[82] = dfd["PELLTCM"].loc[MNUMCR] * SCALPR2
    #     Refrigeration
    # T5(83,IY,IS)=PELRFCM(11,IY)
    z[83] = dfd["PELRFCM"].loc[MNUMCR] * SCALPR2

    #     Computing
    # T5(84,IY,IS)=PELOPCM(11,IY)
    z[84] = dfd["PELOPCM"].loc[MNUMCR] * SCALPR2

    #     Office Equipment
    # T5(85,IY,IS)=PELONCM(11,IY)
    z[85] = dfd["PELONCM"].loc[MNUMCR] * SCALPR2

    #     Other Uses
    # T5(86,IY,IS)=PELOTCM(11,IY)
    z[86] = dfd["AEUSPRC/PELOTCM"].loc[MNUMCR] * SCALPR2

    #   Electric Vehicle Charging - Public/Workplace L2
    z[96] = dfd["AEUSPRC/PELP2CM"].loc[MNUMCR] * SCALPR2

    #   Electric Vehicle Charging - Public DCFC
    z[97] = dfd["AEUSPRC/PELPFCM"].loc[MNUMCR] * SCALPR2

    #   Electric Vehicle Charging - School Bus
    z[98] = dfd["AEUSPRC/PELSBCM"].loc[MNUMCR] * SCALPR2

    #   Electric Vehicle Charging - Transit Bus
    z[99] = dfd["AEUSPRC/PELTBCM"].loc[MNUMCR] * SCALPR2

    #   Electric Vehicle Charging - Intercity Bus, Commercial Light Truck, and Freight Truck (fleet)
    z[100] = dfd["AEUSPRC/PELIBCM"].loc[MNUMCR] * SCALPR2

    #   Electric Vehicle Charging - Freight Truck (non-fleet)
    z[101] = dfd["AEUSPRC/PELFNCM"].loc[MNUMCR] * SCALPR2

    #   End Bonus Row Section

    #   Heating Degree Days

    #      New England
    # T5(57:65,IY,IS)=DEGREEDAYS(1,1:9,IY)
    z[57] = dfd["DEGREEDAYS"].loc[1].loc[1]

    #      Middle Atlantic
    # T5(57:65,IY,IS)=DEGREEDAYS(1,1:9,IY)
    z[58] = dfd["DEGREEDAYS"].loc[1].loc[2]

    #      East North Central
    # T5(57:65,IY,IS)=DEGREEDAYS(1,1:9,IY)
    z[59] = dfd["DEGREEDAYS"].loc[1].loc[3]

    #      West North Central
    # T5(57:65,IY,IS)=DEGREEDAYS(1,1:9,IY)
    z[60] = dfd["DEGREEDAYS"].loc[1].loc[4]

    #      South Atlantic
    # T5(57:65,IY,IS)=DEGREEDAYS(1,1:9,IY)
    z[61] = dfd["DEGREEDAYS"].loc[1].loc[5]

    #      East South Central
    # T5(57:65,IY,IS)=DEGREEDAYS(1,1:9,IY)
    z[62] = dfd["DEGREEDAYS"].loc[1].loc[6]

    #      West South Central
    # T5(57:65,IY,IS)=DEGREEDAYS(1,1:9,IY)
    z[63] = dfd["DEGREEDAYS"].loc[1].loc[7]

    #      Mountain
    # T5(57:65,IY,IS)=DEGREEDAYS(1,1:9,IY)
    z[64] = dfd["DEGREEDAYS"].loc[1].loc[8]

    #      Pacific
    # T5(57:65,IY,IS)=DEGREEDAYS(1,1:9,IY)
    z[65] = dfd["DEGREEDAYS"].loc[1].loc[9]

    #         United States
    # T5(66,IY,IS)=DEGREEDAYS(1,MNUMCR,IY)
    z[66] = dfd["DEGREEDAYS"].loc[1].loc[MNUMCR]

    #

    #   Cooling Degree Days

    #      New England
    # T5(67:75,IY,IS)=DEGREEDAYS(2,1:9,IY)
    z[67] = dfd["DEGREEDAYS"].loc[2].loc[1]

    #      Middle Atlantic
    # T5(67:75,IY,IS)=DEGREEDAYS(2,1:9,IY)
    z[68] = dfd["DEGREEDAYS"].loc[2].loc[2]

    #      East North Central
    # T5(67:75,IY,IS)=DEGREEDAYS(2,1:9,IY)
    z[69] = dfd["DEGREEDAYS"].loc[2].loc[3]

    #      West North Central
    # T5(67:75,IY,IS)=DEGREEDAYS(2,1:9,IY)
    z[70] = dfd["DEGREEDAYS"].loc[2].loc[4]

    #      South Atlantic
    # T5(67:75,IY,IS)=DEGREEDAYS(2,1:9,IY)
    z[71] = dfd["DEGREEDAYS"].loc[2].loc[5]

    #      East South Central
    # T5(67:75,IY,IS)=DEGREEDAYS(2,1:9,IY)
    z[72] = dfd["DEGREEDAYS"].loc[2].loc[6]

    #      West South Central
    # T5(67:75,IY,IS)=DEGREEDAYS(2,1:9,IY)
    z[73] = dfd["DEGREEDAYS"].loc[2].loc[7]

    #      Mountain
    # T5(67:75,IY,IS)=DEGREEDAYS(2,1:9,IY)
    z[74] = dfd["DEGREEDAYS"].loc[2].loc[8]

    #      Pacific
    # T5(67:75,IY,IS)=DEGREEDAYS(2,1:9,IY)
    z[75] = dfd["DEGREEDAYS"].loc[2].loc[9]

    #         United States
    # T5(76,IY,IS)=DEGREEDAYS(2,MNUMCR,IY)
    z[76] = dfd["DEGREEDAYS"].loc[2].loc[MNUMCR]

    return z
