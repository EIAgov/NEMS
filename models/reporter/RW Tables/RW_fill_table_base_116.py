# -*- coding: utf-8 -*-
"""
Created on 7/12/2024 

@author: SZO

"""


def fill_table_base_116(dfd, table_spec, table_id):
    """Fill table for Total Resource Costs - Electric Sector The function returns a dict in which
    each key is an integer references a table row, and each value is a dataframe indexed by region
    number. The integer keys are the same as "IROWS" in the layin.

    Parameters
    ----------
    dfd : dict of restart variables
        key = variable name
        value = pandas series of variable values

    Returns
    -------
    dict
        dict of dataframes, with integers as keys. The dict values are dataframes indexed by
        region number.

    Note
    ----
    NPV and cumulative cash flow will be calculated in the postprocessor_base in batch, which can
    save time compared to looping through individual items.
    DJS - I added NPV here, these values were not being calculated properly 10/2/2024
    """

    z = {}

    # Get reall price year
    FYRPRC = int(table_spec["settings"]["year_index_real_dol_priceyr"])

    # Get inflation index from 1990->2050 (MC_JPGDP: 1987-2050)
    MC_JPGDP = dfd["MC_JPGDP"].loc[1990:]

    # Get inflation index of FYRPRC
    SCALPR2 = MC_JPGDP.loc[FYRPRC]  # 2022 = column 33

    # '''Moved the following settings to postprocessor_base'''
    # # Get discount rate
    # discount_rate = float(table_spec['settings']['discount_rate_table116'])
    # year_index = table_spec['years'].index(FYRPRC)
    #
    # # MNUMNR = int(dfd['MNUMNR_rwpre'])
    #
    # # ftab.f
    # #   UMCAPADD=0     !  set these two to 0 initially, as there is corrective action
    # #   IF (CUMCAPADD .EQ. 0) CUMCAPADD = FYRPRC
    # CUMCAPADD = dfd['CUMCAPADD_rwpre']
    # UT_DR = dfd['UT_DR_rwpre']

    # Total Resource Costs in the Electric Power Sector
    # Annual Expense & Capital Commitment
    # (billion #### dollars)
    # Installed Capacity
    # T116(1,IR,IY,IS)=G_INST_ALL(IR,IY)*SCALPR*0.001
    z[1] = dfd["G_INST_ALL"] * SCALPR2 * 0.001

    # !        Transmission
    # T116(2,IR,IY,IS)=T_OVR(IR,IY)*SCALPR*0.001
    z[2] = dfd["T_OVR"] * SCALPR2 * 0.001

    # !        Retrofits
    # T116(3,IR,IY,IS)=RET_INV(IR,IY)*SCALPR*0.001
    z[3] = dfd["RET_INV"] * SCALPR2 * 0.001

    # !        Fixed O&M Costs
    # T116(4,IR,IY,IS)=TOTAL_FOM(IR,IY)*SCALPR*0.001
    z[4] = dfd["TOTAL_FOM"] * SCALPR2 * 0.001

    # !        Capital Additions
    # T116(5,IR,IY,IS)=CP_ADD(IR,IY)*SCALPR*0.001
    z[5] = dfd["CP_ADD"] * SCALPR2 * 0.001

    # !        Non-Fuel Variable O&M
    # T116(6,IR,IY,IS)=NON_FUEL_VOM(IR,IY)*SCALPR*0.001
    z[6] = dfd["NON_FUEL_VOM"] * SCALPR2 * 0.001

    # !        Fuel Expenses
    # T116(7,IR,IY,IS)=(FUEL_VOM(IR,IY)-CARBON_REV(IR,IY))*SCALPR*0.001
    z[7] = (dfd["FUEL_VOM"] - dfd["CARBON_REV"]) * SCALPR2 * 0.001

    # !        Purchased Power
    # T116(8,IR,IY,IS)=(T_DOMECON(IR,IY)+T_DOMFIRM(IR,IY)-T_INTEXP(IR,IY)+T_INTIMP(IR,IY)
    # +TOTAL_RCC(IR,IY)+TOTAL_RIC(IR,IY))*SCALPR*0.001
    z[8] = (
        (
            dfd["T_DOMECON"]
            + dfd["T_DOMFIRM"]
            - dfd["T_INTEXP"]
            + dfd["T_INTIMP"]
            + dfd["TOTAL_RCC"]
            + dfd["TOTAL_RIC"]
        )
        * SCALPR2
        * 0.001
    )

    # !        RPS Credit Expenses

    # !        use RPS penalty payment variable from Table 122 for consistency
    # T116(9,IR,IY,IS)=URPSPEN(IR,IY)*SCALPR*0.001
    z[9] = dfd["URPSPEN"] * SCALPR2 * 0.001

    #   Energy Efficiency Expenditures
    # T116(41,IR,IY,IS)=ECSTNREE(IR,IY)*SCALPR
    z[41] = dfd["ECSTNREE"] * SCALPR2  # * 0.001

    # !        Total
    # T116(10,IR,IY,IS)=(G_INST_ALL(IR,IY)+T_OVR(IR,IY)+RET_INV(IR,IY)+TOTAL_FOM(IR,IY)+CP_ADD(IR,IY)+NON_FUEL_VOM(IR,IY)
    # +FUEL_VOM(IR,IY)-CARBON_REV(IR,IY)+T_DOMECON(IR,IY)+T_DOMFIRM(IR,IY)-T_INTEXP(IR,IY)+T_INTIMP(IR,IY)
    # +TOTAL_RCC(IR,IY)+TOTAL_RIC(IR,IY)+URPSPEN(IR,IY)+ECSTNREE(IR,IY)*1000.0)*SCALPR*0.001
    # z[10] = (dfd['G_INST_ALL'] + dfd['T_OVR'] + dfd['RET_INV'] + dfd['TOTAL_FOM'] + dfd['CP_ADD'] + dfd['NON_FUEL_VOM'] + \
    #         dfd['FUEL_VOM'] - dfd['CARBON_REV'] + dfd['T_DOMECON'] + dfd['T_DOMFIRM'] - dfd['T_INTEXP'] + dfd['T_INTIMP'] + \
    #         dfd['TOTAL_RCC'] + dfd['TOTAL_RIC'] + dfd['URPSPEN'] + dfd['ECSTNREE'] * 1000.0 ) * SCALPR2 * 0.001
    z[10] = z[1] + z[2] + z[3] + z[4] + z[5] + z[6] + z[7] + z[8] + z[9] + z[41]

    # Cumulative Net Present Value of Resource Costs
    # (billion #### dollars)
    #
    # Installed Capacity
    # T116(11,IR,IY,IS)=NPV(UT_DR,TMP,CUMCAPADD-1989,IY,0,MAXYR-1)*SCALPR*0.001
    #
    # for year in df.columns:
    #    df[year] = df[year] / ((1 + discount_rate) ** (year - year_index))
    #    # df_NPV[year].apply(lambda x: x / (1 + discount_rate) ** (year - year_index))
    # df.iloc[:, :year_index] = 0
    # z[11] = df.cumsum(axis=1)
    #
    # We make a copy of Annual Expense & Capital Commitment here, and then calculate NPV and
    # acumulative cash flow in postprocessor_base.

    def npv(sr, year, rate):
        t = sr.index[-1] - year + 1
        if t < 1:
            return 0
        val = sr.iloc[-1] / ((1 + rate) ** t)
        return val

    d = {
        11: 1,  # Installed Capacity
        12: 2,  # Transmission
        13: 3,  # Retrofits
        14: 4,  # Fixed O&M Costs
        15: 5,  # Capital Additions
        16: 6,  # Non-Fuel Variable O&M
        17: 7,  # Fuel Expenses
        18: 8,  # Purchased Power
        19: 9,  # RPS Credit Expenses
        42: 41,  # Energy Efficiency Expenditures
        20: 10,  # Total
    }

    r = float(table_spec["settings"]["discount_rate_table116"])
    for out, src in d.items():
        z[out] = z[src].expanding(axis=1).apply(npv, args=(FYRPRC, r)).cumsum(axis=1)
        z[out][FYRPRC] = 0

    # Annual Expense & Capital Payment
    # (billion #### dollars)

    # Installed Capacity
    # T116(21,IR,IY,IS)=G_ANN(IR,IY)*SCALPR*0.001
    z[21] = dfd["G_ANN"] * SCALPR2 * 0.001

    # Transmission
    # T116(22,IR,IY,IS)=T_ANN(IR,IY)*SCALPR*0.001
    z[22] = dfd["T_ANN"] * SCALPR2 * 0.001

    # Retrofits
    # T116(23,IR,IY,IS)=RET_CST(IR,IY)*SCALPR*0.001
    z[23] = dfd["RET_CST"] * SCALPR2 * 0.001

    # Fixed O&M Costs
    # T116(24,IR,IY,IS)=TOTAL_FOM(IR,IY)*SCALPR*0.001
    z[24] = dfd["TOTAL_FOM"] * SCALPR2 * 0.001

    # Capital Additions
    # T116(25,IR,IY,IS)=CP_ADD(IR,IY)*SCALPR*0.001
    z[25] = dfd["CP_ADD"] * SCALPR2 * 0.001

    # Non-Fuel Variable O&M
    # T116(26,IR,IY,IS)=NON_FUEL_VOM(IR,IY)*SCALPR*0.001
    z[26] = dfd["NON_FUEL_VOM"] * SCALPR2 * 0.001

    # Fuel Expenses
    # T116(27,IR,IY,IS)=(FUEL_VOM(IR,IY)-CARBON_REV(IR,IY))*SCALPR*0.001
    z[27] = (dfd["FUEL_VOM"] - dfd["CARBON_REV"]) * SCALPR2 * 0.001

    # Purchased Power
    # T116(28,IR,IY,IS)=(T_DOMECON(IR,IY)+T_DOMFIRM(IR,IY)-T_INTEXP(IR,IY)
    # +T_INTIMP(IR,IY)+TOTAL_RCC(IR,IY)+TOTAL_RIC(IR,IY))*SCALPR*0.001
    z[28] = (
        (
            dfd["T_DOMECON"]
            + dfd["T_DOMFIRM"]
            - dfd["T_INTEXP"]
            + dfd["T_INTIMP"]
            + dfd["TOTAL_RCC"]
            + dfd["TOTAL_RIC"]
        )
        * SCALPR2
        * 0.001
    )

    # RPS Credit Expenses
    # T116(29,IR,IY,IS)=URPSPEN(IR,IY)*SCALPR*0.001
    z[29] = dfd["URPSPEN"] * SCALPR2 * 0.001

    # Energy Efficiency Expenditures
    # T116(43,IR,IY,IS)=T116(41,IR,IY,IS)
    z[43] = z[41]

    #    Total
    # T116(30,IR,IY,IS)=(G_ANN(IR,IY)+T_ANN(IR,IY)+RET_CST(IR,IY)+TOTAL_FOM(IR,IY)+CP_ADD(IR,IY)+NON_FUEL_VOM(IR,IY)
    # +FUEL_VOM(IR,IY)-CARBON_REV(IR,IY)+T_DOMECON(IR,IY)+T_DOMFIRM(IR,IY)-T_INTEXP(IR,IY)+T_INTIMP(IR,IY)
    # +TOTAL_RCC(IR,IY)+TOTAL_RIC(IR,IY)+URPSPEN(IR,IY)+ECSTNREE(IR,IY)*1000.0)*SCALPR*0.001
    # z[30] = (dfd['G_ANN'] + dfd['T_ANN'] - dfd['RET_CST'] + dfd['TOTAL_FOM'] + dfd['CP_ADD'] + dfd['NON_FUEL_VOM']
    #         + dfd['FUEL_VOM'] - dfd['CARBON_REV'] + dfd['T_DOMECON'] + dfd['T_DOMFIRM'] - dfd['T_INTEXP'] + dfd['T_INTIMP']
    #         + dfd['TOTAL_RCC'] + dfd['TOTAL_RIC'] + dfd['URPSPEN'] + dfd['ECSTNREE'] * 1000.0 ) * SCALPR2 * 0.001
    z[30] = (
        z[21] + z[22] + z[23] + z[24] + z[25] + z[26] + z[27] + z[28] + z[29] + z[43]
    )

    # Cumulative Net Present Value of Annual Cost
    # (billions #### dollars)

    d = {
        31: 21,  # Installed Capacity
        32: 22,  # Transmission
        33: 23,  # Retrofits
        34: 24,  # Fixed O&M Costs
        35: 25,  # Capital Additions
        36: 26,  # Non-Fuel Variable O&M
        37: 27,  # Fuel Expenses
        38: 28,  # Purchased Power
        39: 29,  # RPS Credit Expenses
        44: 43,  # Energy Efficiency Expenditures
        40: 30,  # Total
    }

    for out, src in d.items():
        z[out] = z[src].expanding(axis=1).apply(npv, args=(FYRPRC, r)).cumsum(axis=1)
        z[out][FYRPRC] = 0

    # !        Grid Resiliency/Reliability

    # DO III = 1 , NUMREP
    #     T116(44+(III - 1)*10 +1,IR,IY,IS) = UGRD_STC(III,IR,IY)     ! Capacity Target
    #     T116(44+(III - 1)*10 +2,IR,IY,IS) = UGRD_RTC(III,IR,IY)     ! Capacity Rating Achieved
    #     T116(44+(III - 1)*10 +3,IR,IY,IS) = UGRD_PRC(III,IR,IY)     ! Capacity Price
    #     T116(44+(III - 1)*10 +4,IR,IY,IS) = UGRD_CAP(III,IR,IY)     ! Qualifying Capacity
    #     T116(44+(III - 1)*10 +5,IR,IY,IS) = T62( 9,IR,IY,IS)        ! Total Capacity
    #     T116(44+(III - 1)*10 +6,IR,IY,IS) = UGRD_STG(III,IR,IY)
    #     T116(44+(III - 1)*10 +7,IR,IY,IS) = UGRD_RTG(III,IR,IY)
    #     T116(44+(III - 1)*10 +8,IR,IY,IS) = UGRD_PRG(III,IR,IY)
    #     T116(44+(III - 1)*10 +9,IR,IY,IS) = UGRD_GEN(III,IR,IY)
    #     T116(44+(III - 1)*10+10,IR,IY,IS) = T62(61,IR,IY,IS)
    #  END DO

    # TODO: Confirm 116 in postprocessor

    NUMREP = 3
    for III in range(1, NUMREP + 1):
        z[44 + (III - 1) * 10 + 1] = dfd["UGRD_STC"].loc[III]
        z[44 + (III - 1) * 10 + 2] = dfd["UGRD_RTC"].loc[III]
        z[44 + (III - 1) * 10 + 3] = dfd["UGRD_PRC"].loc[III]
        z[44 + (III - 1) * 10 + 4] = dfd["UGRD_CAP"].loc[III]
        z[44 + (III - 1) * 10 + 5] = z[44 + (III - 1) * 10 + 4] * 0  # Post-processor
        z[44 + (III - 1) * 10 + 6] = dfd["UGRD_STG"].loc[III]
        z[44 + (III - 1) * 10 + 7] = dfd["UGRD_RTG"].loc[III]
        z[44 + (III - 1) * 10 + 8] = dfd["UGRD_PRG"].loc[III]
        z[44 + (III - 1) * 10 + 9] = dfd["UGRD_GEN"].loc[III]
        z[44 + (III - 1) * 10 + 10] = z[44 + (III - 1) * 10 + 9] * 0  # Post-processor

    return z
