# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
SZO rewrote on Jun 25, 2024
Updated on 8/14/2024
"""
from RW_preprocessor import indcarb


def fill_table_base_022(dfd, table_spec, table_id):
    """Fill table Energy-Related Carbon Dioxide Emissions by End Use
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
        This table needs cross-reference table 24 to get emissions of Biogenic Energy Combustion.
        We designed place holders for z[76] - z[82] here, and populate them with data from table 24
        in RW_postprocessor_base.
    """

    z = {}

    # dict.txt -------------------------------------------------------------------------------------
    # REGCO2 EM_RESD R 4 3 (  6,MNUMCR,MNUMYR) QUNTY  tMTONS  Regional Residential CO2 by fuel
    # REGCO2 EM_COMM R 4 3 (  8,MNUMCR,MNUMYR) QUNTY  tMTONS  Regional Commercial CO2 by fuel
    # REGCO2 EM_INDY R 4 3 ( 20,MNUMCR,MNUMYR) QUNTY  tMTONS  INDUST Regional Industrial CO2 by fuel
    # REGCO2 EM_TRAN R 4 3 ( 12,MNUMCR,MNUMYR) QUNTY  tMTONS  Regional Transportation CO2 by fuel
    # REGCO2 EM_ELEC R 4 3 (  9,MNUMCR,MNUMYR) QUNTY  tMTONS  Regional Electric Power CO2 by fuel
    # ----------------------------------------------------------------------------------------------
    iel_R = 6
    iel_C = 8
    iel_I = 20
    iel_T = 12

    CARBON_OR_2 = table_spec["settings"]["carbon_or_carbondioxide"].upper()
    C_CO2_FACTOR = dfd["C_CO2_FACTOR_rwpre"] if CARBON_OR_2 == "CO2" else 1

    #   Energy-Related Carbon Dioxide Emissions by End Use
    #   (million metric tons carbon_dd_, unless otherwise noted)
    #    Sector and End Use
    #
    #   Residential
    #     Space Heating                     #T22(1:15,IY,IS)=T22(1:15,IY,IS)/10**9

    # z[1] =

    #     Space Cooling                     T22(2,IY,IS)=DOT_PRODUCT(RSCOOLCN(IY,1:3),C8(1:3))
    # z[2] =

    #     Water Heating                     T22(3,IY,IS)=DOT_PRODUCT(RSH2OCON(IY,1:4),C8(1:4))
    # z[3] =

    #     Refrigeration                     T22(4,IY,IS)=RSREFCON(IY)*C8(2)
    # z[4] =

    #     Cooking                           T22(5,IY,IS)=DOT_PRODUCT(RSCKCON(IY,1:3),C8(1:3))
    # z[5] =

    #     Clothes Dryers                    T22(6,IY,IS)=DOT_PRODUCT(RSDRYCON(IY,1:2),C8(1:2))
    # z[6] =

    #     Freezers                          T22(7,IY,IS)=RSFRZCON(IY)*C8(2)
    # z[7] =

    #     Lighting                          T22(8,IY,IS)=RSLTCON(IY)*C8(2)
    # z[8] =

    #     Clothes Washers 1/                T22(9,IY,IS)=RSCSWCON(IY)*C8(2)
    # z[9] =

    #     Dishwashers 1/                    T22(1:15,IY,IS)=T22(1:15,IY,IS)/10**9
    # z[10] =

    #     Televisions and Related Equipment 2/ T22(1:15,IY,IS)=T22(1:15,IY,IS)/10**9
    # z[11] =

    #     Computers and Related Equipment 3/T22(1:15,IY,IS)=T22(1:15,IY,IS)/10**9
    # z[12] =

    #     Furnace Fans and Boiler Circulation Pumps   T22(1:15,IY,IS)=T22(1:15,IY,IS)/10**9
    # z[13] =

    #     Other Uses 4/                     T22(1:15,IY,IS)=T22(1:15,IY,IS)/10**9
    # z[14] =

    #     Discrepancy 5/                    T22(65,IY,IS)=SUM(EM_RESD(1:IEL_R,11,ICY))-T22(15,IY,IS)
    # z[65] =

    #       Total Residential               T22(1:15,IY,IS)=T22(1:15,IY,IS)/10**9
    # z[15] =

    # ! emission factors only defined from 95 on, so use 95 for prior years
    #         iyy=max(iy,6)
    # !Space Heating
    #          c8(1)=engrs(iyy)   * C_CO2_FACTOR
    #          c8(2)=fsum(emel(1,1,iy),4)/qelas(11,iy)
    #          c8(3)=edsrs(iyy)   * C_CO2_FACTOR
    #          c8(4)=elgrs(iyy)   * C_CO2_FACTOR
    #          c8(5)=eksrs(iyy)  * C_CO2_FACTOR
    #          c8(6:7)=0.
    #          c8(8)=eclrs(iyy) * C_CO2_FACTOR
    #         T22( 1,IY,IS)=dot_product(RSHTRCON(iy,1:8),C8(1:8))
    #
    # Calculate Residential Emission Factors by fuel -----------
    #   ENGRS R    4 1 (MNUMYR)               CONFAC mMTC/Q NG     RESIDN Natural Gas - Residential
    #   EMEL  R    4 3 (     4,MNPOLL,MNUMYR) QUNTY  mMTONS               EMM Emissions by Fuel Type
    #   QELAS R    4 2 (MNUMCR,MNUMYR)        QUNTY  tBTU   EL     ALLSEC Purchased Electricity - All Sectors
    #   EDSRS R    4 1 (MNUMYR)               CONFAC mMTC/Q DS     RESIDN Distillate - Residential
    #   ELGRS R    4 1 (MNUMYR)               CONFAC mMTC/Q LG     RESIDN Liquid Petroleum Gases - Residential
    #   EKSRS R    4 1 (MNUMYR)               CONFAC mMTC/Q KS     RESIDN Kerosene - Residential
    #   RSHTRCON R    4 2 (MNUMYR,     8)     QUNTY  tBTU          RESIDN Space Heating Consumption
    #
    #   one quadrillion = 1,000 trillions
    #
    start_y = 5
    c8 = dfd["RSHTRCON"].copy()
    c8.loc[1] = dfd["ENGRS"].values * C_CO2_FACTOR  # RESIDN Natural Gas - Residential
    c8.loc[2] = (
        dfd["EMEL"].loc[1:4, 1, :].sum()
        / (dfd["QELAS"].loc[11] * dfd["TRIL_TO_QUAD_rwpre"])
    ) * C_CO2_FACTOR
    c8.loc[3] = dfd["EDSRS"].values * C_CO2_FACTOR  # RESIDN Distillate - Residential
    c8.loc[4] = (
        dfd["ELGRS"].values * C_CO2_FACTOR
    )  # RESIDN Liquid Petroleum Gases - Residential
    c8.loc[5] = dfd["EKSRS"].values * C_CO2_FACTOR  # RESIDN Kerosene - Residential
    c8.loc[6] = 0
    c8.loc[7] = 0
    c8.loc[8] = dfd["ECLRS"].values * C_CO2_FACTOR  # RESIDN Coal - Residential

    z[1] = (c8 * dfd["RSHTRCON"].loc[1:8]).sum() / 10**9

    # !Space Cooling
    #         c8=0.
    #         c8(1)=fsum(emel(1,1,iy),4)/qelas(11,iy)
    #         c8(3)=engrs(iyy)   * C_CO2_FACTOR
    #         T22( 2,IY,IS)=dot_product(RSCOOLCN(iy,1:3),C8(1:3))
    #
    #   RSCOOLCN    R    4 2 (MNUMYR,   3)    QUNTY  tBTU RESIDN Space Cooling Consumption
    c8 = dfd["RSCOOLCN"].copy()
    c8.loc[1] = (
        dfd["EMEL"].loc[1:4, 1, :].sum()
        / (dfd["QELAS"].loc[11] * dfd["TRIL_TO_QUAD_rwpre"])
        * C_CO2_FACTOR
    )
    c8.loc[2] = 0
    c8.loc[3] = dfd["ENGRS"].values * C_CO2_FACTOR

    z[2] = (c8 * dfd["RSCOOLCN"].loc[1:3]).sum() / 10**9

    # !Water Heating
    #         c8=0.
    #         c8(1)=engrs(iyy)   * C_CO2_FACTOR
    #         c8(2)=fsum(emel(1,1,iy),4)/qelas(11,iy)
    #         c8(3)=edsrs(iyy)   * C_CO2_FACTOR
    #         c8(4)=elgrs(iyy)   * C_CO2_FACTOR
    #         T22( 3,IY,IS)=dot_product(RSH2OCON(iy,1:4),C8(1:4))
    c8 = dfd["RSH2OCON"].copy()
    c8.loc[1] = dfd["ENGRS"].values * C_CO2_FACTOR
    c8.loc[2] = (
        dfd["EMEL"].loc[1:4, 1, :].sum()
        / (dfd["QELAS"].loc[11] * dfd["TRIL_TO_QUAD_rwpre"])
        * C_CO2_FACTOR
    )
    c8.loc[3] = dfd["EDSRS"].values * C_CO2_FACTOR
    c8.loc[4] = dfd["ELGRS"].values * C_CO2_FACTOR

    z[3] = (c8 * dfd["RSH2OCON"].loc[1:4]).sum() / 10**9

    # !Other Uses 6/
    #         T22(14,IY,IS)=dot_product(RSAPCON(iy,1:4),C8(1:4))
    # Perform element-wise multiplication
    # z[14] = c8.reset_index(drop=True) * dfd['RSAPCON']).reset_index(drop=True.loc[1:4].sum()/10**9
    z[14] = (c8.loc[1:4] * dfd["RSAPCON"].loc[1:4]).sum() / 10**9

    # !Refrigeration
    #         T22( 4,IY,IS)=RSREFCON(IY)*c8(2)
    # dfd['RSREFCON'] has only one row with index 0 !
    # dfd['RSREFCON'].index = dfd['RSREFCON'].index + 2
    # c8 = dfd['RSREFCON'].copy()
    # c8.loc[2] = dfd['EMEL'].loc[1:4, 1, :].sum() / (dfd['QELAS'].loc[11]
    #                                                 * dfd['TRIL_TO_QUAD_rwpre']) * C_CO2_FACTOR
    # z[4] =c8 * dfd['RSREFCON'].loc[2] / 10**9
    z[4] = c8.loc[2] * dfd["RSREFCON"] / 10**9

    # !Cooking
    #         c8=0.
    #         c8(1)=engrs(iyy)   * C_CO2_FACTOR
    #         c8(2)=elgrs(iyy)   * C_CO2_FACTOR
    #         c8(3)=fsum(emel(1,1,iy),4)/qelas(11,iy)
    #         T22( 5,IY,IS)=dot_product(RSCKCON(iy,1:3),C8(1:3))
    c8 = dfd["RSCKCON"].copy()
    c8.loc[1] = dfd["ENGRS"].values * C_CO2_FACTOR
    c8.loc[2] = dfd["ELGRS"].values * C_CO2_FACTOR
    c8.loc[3] = (
        dfd["EMEL"].loc[1:4, 1, :].sum()
        / (dfd["QELAS"].loc[11] * dfd["TRIL_TO_QUAD_rwpre"])
        * C_CO2_FACTOR
    )

    z[5] = (c8 * dfd["RSCKCON"].loc[1:3]).sum() / 10**9

    # !Clothes Dryers
    #         c8=0.
    #         c8(1)=engrs(iyy)   * C_CO2_FACTOR
    #         c8(2)=fsum(emel(1,1,iy),4)/qelas(11,iy)
    #         T22( 6,IY,IS)=dot_product(RSDRYCON(iy,1:2),C8(1:2))
    c8 = dfd["RSDRYCON"].copy()
    c8.loc[1] = dfd["ENGRS"].values * C_CO2_FACTOR
    c8.loc[2] = (
        dfd["EMEL"].loc[1:4, 1, :].sum()
        / (dfd["QELAS"].loc[11] * dfd["TRIL_TO_QUAD_rwpre"])
        * C_CO2_FACTOR
    )

    z[6] = (c8 * dfd["RSDRYCON"].loc[1:2]).sum() / 10**9

    # !All electric
    #         T22( 7,IY,IS)=RSFRZCON(IY)*c8(2)      !Freezers
    #         T22( 8,IY,IS)=RSLTCON(IY)*c8(2)       !Lighting
    #         T22( 9,IY,IS)=RSCSWCON(IY)*c8(2)      !Clothes Washers
    #         T22(10,IY,IS)=RSDSWCON(IY)*c8(2)      !Dishwashers
    #         T22(11,IY,IS)=RSTVRCON(IY)*c8(2)      !Color Televisions and Set-Top Boxes
    #         T22(12,IY,IS)=RSPCRCON(IY)*c8(2)      !Personal Computers
    #         T22(13,IY,IS)=RSFANCON(IY)*c8(2)      !Furnace Fans
    #         T22(15,IY,IS)=sum(T22(1:14,IY,IS))    !Total
    #         T22(1:15,iy,is)=T22(1:15,iy,is)/10**9 !Discrepancy 5/
    #         icy=iy+baseyr-1
    #         T22(65,IY,IS) = sum(em_resd(1:iel_R,11,icy))-T22(15,iy,is)   ! Discrepancy
    #         T22(15,IY,IS)= T22(15,IY,IS)+ T22(65,IY,IS)

    z[7] = dfd["RSFRZCON"] * c8.loc[2] / 10**9

    z[8] = dfd["RSLTCON"] * c8.loc[2] / 10**9

    z[9] = dfd["RSCSWCON"] * c8.loc[2] / 10**9

    z[10] = dfd["RSDSWCON"] * c8.loc[2] / 10**9

    z[11] = dfd["RSTVRCON"] * c8.loc[2] / 10**9

    z[12] = dfd["RSPCRCON"] * c8.loc[2] / 10**9

    z[13] = dfd["RSFANCON"] * c8.loc[2] / 10**9

    # T22(15,IY,IS)=sum(T22(1:14,IY,IS))    !  Total
    # T22(1:15,iy,is)=T22(1:15,iy,is)/10**9
    # icy=iy+baseyr-1
    # T22(65,IY,IS) = sum(em_resd(1:iel_R,11,icy))-T22(15,iy,is)   ! Discrepancy
    # T22(15,IY,IS)= T22(15,IY,IS)+ T22(65,IY,IS)

    #    Total Residential
    z[15] = (
        z[1]
        + z[2]
        + z[3]
        + z[4]
        + z[5]
        + z[6]
        + z[7]
        + z[8]
        + z[9]
        + z[10]
        + z[11]
        + z[12]
        + z[13]
        + z[14]
    )

    #   Discrepancy 5/
    # EM_RESD          R    4 3 (     6,MNUMCR,MNUMYR)              QUNTY  tMTONS               Regional Residential CO2 by fuel
    # Need convert from T to Q, and also time C_CO2_FACTOR  !!!
    z[65] = (
        dfd["EM_RESD"].loc[1:iel_R, 11, :].sum()
        * dfd["TRIL_TO_QUAD_rwpre"]
        * C_CO2_FACTOR
        - z[15]
    )

    # # TODO: Check with Tracy, ftab has 0 before 2020
    # y = 2020
    # y_idx = table_spec['years'].index(y)
    # z[65].loc[:, :y_idx] = 0
    # z[65].loc[:, :y_idx] = 0

    z[15] = z[15] + z[65]

    #
    #   Commercial
    #     Space Heating 6/          T22(16,IY,IS)=DOT_PRODUCT(CMUSCONSUMPTION(1,1:3,IY),C8(1:3))
    # z[16] =

    #     Space Cooling 6/          T22(17,IY,IS)=DOT_PRODUCT(CMUSCONSUMPTION(2,1:3,IY),C8(1:3))
    # z[17] =

    #     Water Heating 6/          T22(18,IY,IS)=DOT_PRODUCT(CMUSCONSUMPTION(3,1:3,IY),C8(1:3))
    # z[18] =

    #     Ventilation               T22(19,IY,IS)=DOT_PRODUCT(CMUSCONSUMPTION(4,1:3,IY),C8(1:3))
    # z[19] =

    #     Cooking                   T22(20,IY,IS)=DOT_PRODUCT(CMUSCONSUMPTION(5,1:3,IY),C8(1:3))
    # z[20] =

    #     Lighting                  T22(21,IY,IS)=DOT_PRODUCT(CMUSCONSUMPTION(6,1:3,IY),C8(1:3))
    # z[21] =

    #     Refrigeration             T22(22,IY,IS)=DOT_PRODUCT(CMUSCONSUMPTION(7,1:3,IY),C8(1:3))
    # z[22] =

    #     Computing                 T22(23,IY,IS)=DOT_PRODUCT(CMUSCONSUMPTION(8,1:3,IY),C8(1:3))
    # z[23] =

    #     Office Equipment          T22(24,IY,IS)=DOT_PRODUCT(CMUSCONSUMPTION(9,1:3,IY),C8(1:3))
    # z[24] =

    #     Other Uses 7/             T22(25,IY,IS)=SUM(EM_COMM(1:IEL_C,11,ICY))-SUM(T22(16:24,IY,IS))
    # z[25] =

    #       Total Commercial        T22(26,IY,IS)=SUM(T22(16:25,IY,IS))
    # z[26] =

    #   T22(16,IY,IS) = dot_product(CMUSCONSUMPTION(1,1:3,IY),c8(1:3))   ! Space Heating
    #   T22(17,IY,IS) = dot_product(CMUSCONSUMPTION(2,1:3,IY),c8(1:3))   ! Space Cooling
    #   T22(18,IY,IS) = dot_product(CMUSCONSUMPTION(3,1:3,IY),c8(1:3))   ! Water Heating
    #   T22(19,IY,IS) = dot_product(CMUSCONSUMPTION(4,1:3,IY),c8(1:3))   ! Ventilation
    #   T22(20,IY,IS) = dot_product(CMUSCONSUMPTION(5,1:3,IY),c8(1:3))   ! Cooking
    #   T22(21,IY,IS) = dot_product(CMUSCONSUMPTION(6,1:3,IY),c8(1:3))   ! Lighting
    #   T22(22,IY,IS) = dot_product(CMUSCONSUMPTION(7,1:3,IY),c8(1:3))   ! Refrigeration
    #   T22(23,IY,IS) = dot_product(CMUSCONSUMPTION(8,1:3,IY),c8(1:3))   ! Office Equipment (PC)
    #   T22(24,IY,IS) = dot_product(CMUSCONSUMPTION(9,1:3,IY),c8(1:3))   ! Office Equipment (non-PC)
    #   icy=iy+baseyr-1
    #   T22(25,IY,IS) = sum(em_comm(1:iel_C,11,icy))-sum(T22(16:24,iy,is))   ! Other Uses
    #   T22(26,IY,IS)= sum(T22(16:25,IY,IS))

    #   Commercial
    #     Space Heating 6/
    c8 = dfd["CMUSCONSUMPTION"].loc[1].loc[1:3, :].copy()
    c8.loc[1] = (
        dfd["EMEL"].loc[1:4, 1, :].sum()
        / (dfd["QELAS"].loc[11] * dfd["TRIL_TO_QUAD_rwpre"])
        * C_CO2_FACTOR
    )
    c8.loc[2] = dfd["ENGCM"].values * C_CO2_FACTOR
    c8.loc[3] = dfd["EDSCM"].values * C_CO2_FACTOR

    z[16] = (dfd["CMUSCONSUMPTION"].loc[1, 1:3, :] * c8.loc[1:3]).sum()

    #     Space Cooling 6/
    z[17] = (dfd["CMUSCONSUMPTION"].loc[2, 1:3, :] * c8.loc[1:3]).sum()

    #     Water Heating 6/
    z[18] = (dfd["CMUSCONSUMPTION"].loc[3, 1:3, :] * c8.loc[1:3]).sum()

    #     Ventilation
    z[19] = (dfd["CMUSCONSUMPTION"].loc[4, 1:3, :] * c8.loc[1:3]).sum()

    #     Cooking
    z[20] = (dfd["CMUSCONSUMPTION"].loc[5, 1:3, :] * c8.loc[1:3]).sum()

    #     Lighting
    z[21] = (dfd["CMUSCONSUMPTION"].loc[6, 1:3, :] * c8.loc[1:3]).sum()

    #     Refrigeration
    z[22] = (dfd["CMUSCONSUMPTION"].loc[7, 1:3, :] * c8.loc[1:3]).sum()

    #     Computing
    z[23] = (dfd["CMUSCONSUMPTION"].loc[8, 1:3, :] * c8.loc[1:3]).sum()

    #     Office Equipment
    z[24] = (dfd["CMUSCONSUMPTION"].loc[9, 1:3, :] * c8.loc[1:3]).sum()

    #     Other Uses 7/
    # Need convert from T to Q, and also time C_CO2_FACTOR  !!!
    z[25] = dfd["EM_COMM"].loc[1:iel_C, 11, :].groupby(level=1).sum() * dfd[
        "TRIL_TO_QUAD_rwpre"
    ] * C_CO2_FACTOR - (
        z[16] + z[17] + z[18] + z[19] + z[20] + z[21] + z[22] + z[23] + z[24]
    )

    #       Total Commercial
    z[26] = (
        z[16] + z[17] + z[18] + z[19] + z[20] + z[21] + z[22] + z[23] + z[24] + z[25]
    )

    #
    #   Industrial 8/
    #     Manufacturing
    #       Refining
    # T22(41,IY,IS)=INDCARB(REFCON(1,1,IY),IY)
    # There are two dfs for REFCON !!!
    # RSCON     REFCON  R    4 2 (MNUMYR, 9)        QUNTY  tBTU RESIDN Refrigerator Consumption
    # INDREP    REFCON  R    4 3 (18,MINDCR,MNUMYR) QUNTY  tBTU INDUST REFINERY INDUSTRY CONSUMPTION, 18 Fuels, Regions 1:4 and US:5

    z[41] = indcarb(dfd, dfd["INDREP/REFCON"], CARBON_OR_2)
    #       Food Products
    # T22(42,IY,IS)=INDCARB(FOODCON(1,1,IY),IY)
    z[42] = indcarb(dfd, dfd["FOODCON"], CARBON_OR_2)
    #       Paper Products
    # T22(43,IY,IS)=INDCARB(PAPERCON(1,1,IY),IY)
    z[43] = indcarb(dfd, dfd["PAPERCON"], CARBON_OR_2)
    #       Bulk Chemicals (including hydrogen production)
    # T22(44,IY,IS)=INDCARB(CHEMCON(1,1,IY),IY)+INDCARB(H2CON(1,1,IY),IY)
    z[44] = (
        indcarb(dfd, dfd["CHEMCON"], CARBON_OR_2)
        + indcarb(dfd, dfd["H2CON"], CARBON_OR_2)
        # Captured carbon from steam methane reforming
        - dfd["CC_INDY"].loc[(9, 11), :] * dfd['C_CO2_FACTOR_rwpre'] / 1000.0
    )
    #       Glass
    # T22(45,IY,IS)=INDCARB(GLASSCON(1,1,IY),IY)
    z[45] = indcarb(dfd, dfd["GLASSCON"], CARBON_OR_2)

    #       Cement and Lime
    # T22(46,IY,IS)=INDCARB(CEMENTCON(1,1,IY),IY)    
    # combustion emissions minus the sequestered emissions for cement & lime
    z[46] = (
        indcarb(dfd, dfd["CEMENTCON"], CARBON_OR_2)
        - (
            dfd['CC_INDY'].loc[(1, 11), :]  # Distillate
            + dfd['CC_INDY'].loc[(2, 11), :]  # Petroleum coke
            + dfd['CC_INDY'].loc[(4, 11), :]  # Other petroleum
            + dfd['CC_INDY'].loc[(5, 11), :]  # Steam coal
            + dfd['CC_INDY'].loc[(8, 11), :]  # Natural gas
        ) * dfd['C_CO2_FACTOR_rwpre'] / 1000.0
    )

    #       Iron and Steel
    # T22(47,IY,IS)=INDCARB(STEELCON(1,1,IY),IY)
    z[47] = indcarb(dfd, dfd["STEELCON"], CARBON_OR_2)
    #       Aluminum
    # T22(48,IY,IS)=INDCARB(ALUMCON(1,1,IY),IY)
    z[48] = indcarb(dfd, dfd["ALUMCON"], CARBON_OR_2)
    #       Fabricated Metal Products
    # T22(49,IY,IS)=INDCARB(FABMETALCON(1,1,IY),IY)
    z[49] = indcarb(dfd, dfd["FABMETALCON"], CARBON_OR_2)
    #       Machinery
    # T22(50,IY,IS)=INDCARB(MACHINECON(1,1,IY),IY)
    z[50] = indcarb(dfd, dfd["MACHINECON"], CARBON_OR_2)
    #       Computers and Electronics
    # T22(51,IY,IS)=INDCARB(COMPUTECON(1,1,IY),IY)
    z[51] = indcarb(dfd, dfd["COMPUTECON"], CARBON_OR_2)
    #       Transportation Equipment
    # T22(52,IY,IS)=INDCARB(TRANEQUIPCON(1,1,IY),IY)
    z[52] = indcarb(dfd, dfd["TRANEQUIPCON"], CARBON_OR_2)
    #       Electrical Equipment
    # T22(53,IY,IS)=INDCARB(ELECEQUIPON(1,1,IY),IY)
    z[53] = indcarb(dfd, dfd["ELECEQUIPON"], CARBON_OR_2)
    #       Wood Products
    # T22(54,IY,IS)=INDCARB(WOODPRODCON(1,1,IY),IY)
    z[54] = indcarb(dfd, dfd["WOODPRODCON"], CARBON_OR_2)
    #       Plastics
    # T22(55,IY,IS)=INDCARB(PLASTICCON(1,1,IY),IY)
    z[55] = indcarb(dfd, dfd["PLASTICCON"], CARBON_OR_2)
    #       Light Chemicals
    # T22(56,IY,IS)=INDCARB(LTCHEMCON(1,1,IY),IY)
    z[56] = indcarb(dfd, dfd["LTCHEMCON"], CARBON_OR_2)
    #       Other Non-metallic minerals
    # T22(89,IY,IS)=INDCARB(OTHRNMMCON(1,1,IY),IY)
    z[89] = indcarb(dfd, dfd["OTHRNMMCON"], CARBON_OR_2)
    #       Other Primary Metals
    # T22(90,IY,IS)=INDCARB(OTHRPRIMCON(1,1,IY),IY)
    z[90] = indcarb(dfd, dfd["OTHRPRIMCON"], CARBON_OR_2)
    #       Miscellaneous Finished Goods
    # T22(91,IY,IS)=INDCARB(MISCFINCON(1,1,IY),IY)
    z[91] = indcarb(dfd, dfd["MISCFINCON"], CARBON_OR_2)

    # #         Total Manufacturing
    # T22(57,IY,IS)=SUM(T22(41:56,IY,IS))+SUM(T22(89:91,IY,IS))
    z[57] = (
        z[41]
        + z[42]
        + z[43]
        + z[44]
        + z[45]
        + z[46]
        + z[47]
        + z[48]
        + z[49]
        + z[50]
        + z[51]
        + z[52]
        + z[53]
        + z[54]
        + z[55]
        + z[56]
        + z[89]
        + z[90]
        + z[91]
    )

    #     Nonmanufacturing
    #       Agriculture
    # T22(58,IY,IS)=INDCARB(AGCON(1,1,IY),IY)
    z[58] = indcarb(dfd, dfd["AGCON"], CARBON_OR_2)

    #       Construction
    # T22(59,IY,IS)=INDCARB(CONSTCON(1,1,IY),IY)
    z[59] = indcarb(dfd, dfd["CONSTCON"], CARBON_OR_2)

    #       Mining
    # T22(60,IY,IS)=INDCARB(MINECON(1,1,IY),IY)+EM_INDY(19,11,ICY)
    z[60] = (
        indcarb(dfd, dfd["MINECON"], CARBON_OR_2)
        # Mining includes oil & gas extraction, so add vented natural gas processing CO2 here
        + dfd["EM_INDY"].loc[19, 11] / 1000 * C_CO2_FACTOR
    )

    # #         Total Nonmanufacturing
    # T22(61,IY,IS)=SUM(T22(58:60,IY,IS))
    z[61] = z[58] + z[59] + z[60]

    #       Total Industrial 9/
    # T22(63,IY,IS)=SUM(EM_INDY(1:IEL_I,11,ICY))
    z[63] = dfd["EM_INDY"].loc[1:iel_I, 11, :].sum() / 1000 * C_CO2_FACTOR

    #     Discrepancy 5/
    # T22(62,IY,IS)=SUM(EM_INDY(1:IEL_I,11,ICY))-(T22(57,IY,IS)+T22(61,IY,IS))
    # z[62] = dfd['EM_INDY'].loc[1:iel_I, 11, :].groupby(level=1).sum().values /1000 - z[57] - z[61]
    z[62] = z[63] - z[57] - z[61]

    #
    #   Transportation
    #     Light-Duty Vehicles         T22(27,IY,IS)=DOT_PRODUCT(TRQLDV(1:8,11,IY),C8(1:8))
    # z[27] =

    #     Commercial Light Trucks 10/ T22(28,IY,IS)=DOT_PRODUCT(CLTFUELBTU(1:6,IY),C8(1:6))
    # z[28] =

    #     Bus Transportation          T22(29,IY,IS)=DOT_PRODUCT(C8(5:8),C8(1:4))
    # z[29] =

    #     Freight Trucks              T22(30,IY,IS)=DOT_PRODUCT(TRQFTRK(1:5,IY),C8(1:5))
    # z[30] =

    #     Rail, Passenger             T22(31,IY,IS)=DOT_PRODUCT(TRQRRP(1:9,IY),C8(1:9))
    # z[31] =

    #     Rail, Freight               T22(32,IY,IS)=DOT_PRODUCT(TRQRRF(1:4,IY),C8(1:4))
    # z[32] =

    #     Shipping, Domestic          T22(33,IY,IS)=DOT_PRODUCT(TRQDOMS(1:4,IY),C8(1:4))
    # z[33] =

    #     Shipping, International     T22(34,IY,IS)=DOT_PRODUCT(TRQINTS(1:4,IY),C8(1:4))
    # z[34] =

    #     Recreational Boats          T22(35,IY,IS)=DOT_PRODUCT(TRQBOAT(1:2,IY),C8(1:2))
    # z[35] =

    #     Air                         T22(36,IY,IS)=DOT_PRODUCT(TRQAIRT(1:2,IY),C8(1:2))
    # z[36] =

    #     Military Use                T22(37,IY,IS)=DOT_PRODUCT(TRQMIL(1:4,IY),C8(1:4))
    # z[37] =

    #     Lubricants                  T22(38,IY,IS)=TRQLUB(IY)*EOTTR(IY)*C_CO2_FACTOR
    # z[38] =

    #     Pipeline Fuel               T22(39,IY,IS)=QGPTR(11,IY)*EGPTR(IY)*C_CO2_FACTOR*1000.
    # z[39] =

    #     Discrepancy 5/              T22(66,IY,IS)=SUM(EM_TRAN(1:IEL_T,11,ICY))-T22(40,IY,IS)
    # z[66] =

    #       Total Transportation      T22(40,IY,IS)=T22(40,IY,IS)+T22(66,IY,IS)
    # z[40] =

    # ! light-duty vehicles
    #         c8=0.
    #         c8(1)=(em_tran(1,11,icy)+em_tran(3,11,icy))/qMGtr(11,iy) ! motor gasoline adjusted for ethanol
    #         c8(2)=0.
    #         c8(3)=0.
    #         if(qETtr(11,iy).gt.0) c8(3)=em_tran(2,11,icy)/qETtr(11,iy) ! e85
    #         c8(4)=eNGtr(iy)* C_CO2_FACTOR !   cng
    #         c8(5)=ePRtr(iy)* C_CO2_FACTOR !   propane
    #         c8(6)=fsum(emel(1,1,iy),4)/qelas(11,iy) !   elec
    #         c8(7)=0.!  hydrogen
    #         c8(8)=(em_tran(6,11,icy)+em_tran(7,11,icy))/qDStr(11,iy) !  diesel with biodiesel adjustment
    #         T22(27,iy,is) = dot_product(TRQLDV(1:8,11,IY),c8(1:8))   !  Light-Duty Vehicles

    # TRQLDV  R    4 3 (     8,MNUMCR,MNUMYR) QUNTY  tBTU   TRANSP Lt Duty Vehicle Energy Use
    # EM_TRAN R    4 3 (    12,MNUMCR,MNUMYR) QUNTY  tMTONS Regional Transportation CO2 by fuel
    # EMEL    R    4 3 (     4,MNPOLL,MNUMYR) QUNTY  mMTONS EMM Emissions by Fuel Type
    # dfd['TRQLDV'] = dfd['TRQLDV'].swaplevel(0, 1, axis=0)
    # c8 = dfd['TRQLDV'].loc[11].copy()
    c8 = dfd["TRQLDV"].loc[:, 11, :].copy()
    c8.loc[1] = (
        (dfd["EM_TRAN"].loc[1, 11] + dfd["EM_TRAN"].loc[3, 11])
        / dfd["QMGTR"].loc[11]
        * C_CO2_FACTOR
    )  #! motor gasoline adjusted for ethanol
    c8.loc[2] = 0.0
    c8.loc[3] = (
        dfd["EM_TRAN"].loc[2, 11] / dfd["QETTR"].loc[11] * C_CO2_FACTOR
    ).fillna(0.0)
    c8.loc[4] = dfd["ENGTR"].values * C_CO2_FACTOR
    c8.loc[5] = dfd["EPRTR"].values * C_CO2_FACTOR
    c8.loc[6] = (
        dfd["EMEL"].loc[1:4, 1, :].sum()
        / (dfd["QELAS"].loc[11] * dfd["TRIL_TO_QUAD_rwpre"])
        * C_CO2_FACTOR
    )
    c8.loc[7] = 0.0
    c8.loc[8] = (
        (dfd["EM_TRAN"].loc[6, 11] + dfd["EM_TRAN"].loc[7, 11])
        / dfd["QDSTR"].loc[11]
        * C_CO2_FACTOR
    )
    c8.loc[:, :start_y] = 0.0

    # # TODO: unit conversion ?
    # conv_factor = 10**6
    z[27] = (c8 * dfd["TRQLDV"].loc[:, 11, :]).sum() * dfd["TRIL_TO_QUAD_rwpre"]

    # ! commercial light trucks
    #         c8=0.
    #         c8(1)=(em_tran(1,11,icy)+em_tran(3,11,icy))/qMGtr(11,iy) ! motor gasoline adjusted for ethanol
    #         c8(2)=(em_tran(6,11,icy)+em_tran(7,11,icy))/qDStr(11,iy) ! diesel with biodiesel adjustment
    #         c8(3)=ePRtr(iy)* C_CO2_FACTOR !   propane
    #         c8(4)=eNGtr(iy)* C_CO2_FACTOR !   cng
    #         if(qETtr(11,iy).gt.0) c8(5)=em_tran(2,11,icy)/qETtr(11,iy) ! e85
    #         c8(6)=fsum(emel(1,1,iy),4)/qelas(11,iy) !   elec
    #         T22(28,iy,is) = dot_product(CLTFUELBTU(1:6,iy),c8(1:6))  !  Commercial Light Trucks skipping hydrogen with 0 carbon factor
    c8 = dfd["CLTFUELBTU"].copy()
    c8.loc[1] = (
        (dfd["EM_TRAN"].loc[1, 11] + dfd["EM_TRAN"].loc[3, 11])
        / dfd["QMGTR"].loc[11]
        * C_CO2_FACTOR
    )
    c8.loc[2] = (
        (dfd["EM_TRAN"].loc[6, 11] + dfd["EM_TRAN"].loc[7, 11])
        / dfd["QDSTR"].loc[11]
        * C_CO2_FACTOR
    )
    c8.loc[3] = dfd["EPRTR"].values * C_CO2_FACTOR
    c8.loc[4] = dfd["ENGTR"].values * C_CO2_FACTOR
    c8.loc[5] = dfd["EM_TRAN"].loc[2, 11] / dfd["QETTR"].loc[11] * C_CO2_FACTOR
    c8.loc[6] = (
        dfd["EMEL"].loc[1:4, 1, :].sum()
        / (dfd["QELAS"].loc[11] * dfd["TRIL_TO_QUAD_rwpre"])
        * C_CO2_FACTOR
    )
    c8.loc[:, :start_y] = 0

    z[28] = (c8.loc[1:6] * dfd["CLTFUELBTU"].loc[1:6]).sum() * dfd["TRIL_TO_QUAD_rwpre"]

    # ! Bus transport
    #         c8=0.
    #         c8(1)=(em_tran(1,11,icy)+em_tran(3,11,icy))/qMGtr(11,iy) ! motor gasoline adjusted for ethanol
    #         c8(2)=(em_tran(6,11,icy)+em_tran(7,11,icy))/qDStr(11,iy) ! diesel with biodiesel adjustment
    #         c8(3)=eNGtr(iy)* C_CO2_FACTOR
    #         c8(4)=ePRtr(iy)* C_CO2_FACTOR !   propane
    #         c8(5)=sum(TRQBUS(:,1,IY))
    #         c8(6)=sum(TRQBUS(:,2,IY))
    #         c8(7)=sum(TRQBUS(:,5,IY))
    #         c8(8)=sum(TRQBUS(:,6,IY))
    #         T22(29,iy,is) = dot_product(c8(5:8),c8(1:4))             ! Bus Transportation
    c8 = dfd["TRQBUS"].loc[1, :].copy()
    c8.loc[1] = (
        (dfd["EM_TRAN"].loc[1, 11] + dfd["EM_TRAN"].loc[3, 11])
        / dfd["QMGTR"].loc[11]
        * C_CO2_FACTOR
    )
    c8.loc[2] = (
        (dfd["EM_TRAN"].loc[6, 11] + dfd["EM_TRAN"].loc[7, 11])
        / dfd["QDSTR"].loc[11]
        * C_CO2_FACTOR
    )
    c8.loc[3] = dfd["ENGTR"].values * C_CO2_FACTOR
    c8.loc[4] = dfd["EPRTR"].values * C_CO2_FACTOR
    c8.loc[5] = dfd["TRQBUS"].loc[:, 1, :].sum()
    c8.loc[6] = dfd["TRQBUS"].loc[:, 2, :].sum()
    c8.loc[7] = dfd["TRQBUS"].loc[:, 5, :].sum()
    c8.loc[8] = dfd["TRQBUS"].loc[:, 6, :].sum()
    c8.loc[:, :start_y] = 0

    # z[29] = c8.loc[5:8].reset_index(drop=True) * c8.loc[1:4].reset_index(drop=True)).sum() / 10**9
    z[29] = (
        c8.loc[5:8].reset_index(drop=True) * c8.loc[1:4].reset_index(drop=True)
    ).sum() * dfd["TRIL_TO_QUAD_rwpre"]

    # ! freight trucks
    #         c8=0.
    #         c8(1)=(em_tran(1,11,icy)+em_tran(3,11,icy))/qMGtr(11,iy) ! motor gasoline adjusted for ethanol
    #         c8(2)=(em_tran(6,11,icy)+em_tran(7,11,icy))/qDStr(11,iy) ! diesel with biodiesel adjustment
    #         c8(3)=eNGtr(iy)* C_CO2_FACTOR !   cng
    #         c8(4)=0.                      !   ???
    #         c8(5)=ePRtr(iy)* C_CO2_FACTOR !   propane
    #         T22(30,iy,is) = dot_product(TRQFTRK(1:5,IY),c8(1:5))     !  Freight Trucks
    c8 = dfd["TRQFTRK"].copy()
    c8.loc[1] = (
        (dfd["EM_TRAN"].loc[1, 11] + dfd["EM_TRAN"].loc[3, 11])
        / dfd["QMGTR"].loc[11]
        * C_CO2_FACTOR
    )
    c8.loc[2] = (
        (dfd["EM_TRAN"].loc[6, 11] + dfd["EM_TRAN"].loc[7, 11])
        / dfd["QDSTR"].loc[11]
        * C_CO2_FACTOR
    )
    c8.loc[3] = dfd["ENGTR"].values * C_CO2_FACTOR
    c8.loc[4] = 0
    c8.loc[5] = dfd["EPRTR"].values * C_CO2_FACTOR
    c8.loc[:, :start_y] = 0

    z[30] = (c8.loc[1:5] * dfd["TRQFTRK"].loc[1:5]).sum() * dfd["TRIL_TO_QUAD_rwpre"]

    # ! passenger rail
    #         c8=0.
    #         c8(1)=fsum(emel(1,1,iy),4)/qelas(11,iy) !   elec
    #         c8(2)=(em_tran(6,11,icy)+em_tran(7,11,icy))/qDStr(11,iy) !   diesel with biodiesel adjustment
    #         c8(3)=eNGtr(iy)* C_CO2_FACTOR !   cng
    #         c8(4)=eNGtr(iy)* C_CO2_FACTOR !   lng
    #         c8(5)=c8(1)
    #         c8(6)=c8(1)
    #         c8(7)=c8(2)
    #         c8(8)=c8(3)
    #         c8(9)=c8(4)
    #         T22(31,iy,is) = dot_product(TRQRRP(1:9,IY),c8(1:9))      !  Rail, Passenger
    c8 = dfd["TRQRRP"].copy()
    c8.loc[1] = (
        dfd["EMEL"].loc[1:4, 1, :].sum()
        / (dfd["QELAS"].loc[11] * dfd["TRIL_TO_QUAD_rwpre"])
        * C_CO2_FACTOR
    )
    c8.loc[2] = (
        (dfd["EM_TRAN"].loc[6, 11] + dfd["EM_TRAN"].loc[7, 11])
        / dfd["QDSTR"].loc[11]
        * C_CO2_FACTOR
    )
    c8.loc[3] = dfd["ENGTR"].values * C_CO2_FACTOR
    c8.loc[4] = dfd["ENGTR"].values * C_CO2_FACTOR
    c8.loc[5] = c8.loc[1]
    c8.loc[6] = c8.loc[1]
    c8.loc[7] = c8.loc[2]
    c8.loc[8] = c8.loc[3]
    c8.loc[9] = c8.loc[4]
    c8.loc[:, :start_y] = 0

    z[31] = (c8 * dfd["TRQRRP"].loc[1:9]).sum() * dfd["TRIL_TO_QUAD_rwpre"]

    # !  Note next three categories (rail freight, domestic shipping, international shipping) all have 4
    # !  fuels in the same order (distillate, residual, CNG, LNG) so we can set c() once and cover all 3
    # ! Rail, freight
    #         c8=0.
    #         c8(1)=(em_tran(6,11,icy)+em_tran(7,11,icy))/qDStr(11,iy) !   diesel with biodiesel adjustment
    #         c8(2)=eRStr(iy)* C_CO2_FACTOR !   resid
    #         c8(3)=eNGtr(iy)* C_CO2_FACTOR !   cng
    #         c8(4)=eNGtr(iy)* C_CO2_FACTOR !   lng
    #         T22(32,iy,is) = dot_product(TRQRRF(1:4,IY),c8(1:4))      !  Rail, freight
    c8 = dfd["TRQRRF"].copy()
    c8.loc[1] = (
        (dfd["EM_TRAN"].loc[6, 11] + dfd["EM_TRAN"].loc[7, 11])
        / dfd["QDSTR"].loc[11]
        * C_CO2_FACTOR
    )
    c8.loc[2] = dfd["ERSTR"].values * C_CO2_FACTOR
    c8.loc[3] = dfd["ENGTR"].values * C_CO2_FACTOR
    c8.loc[4] = dfd["ENGTR"].values * C_CO2_FACTOR
    c8.loc[:, :start_y] = 0

    z[32] = (c8 * dfd["TRQRRF"].loc[1:4]).sum() * dfd["TRIL_TO_QUAD_rwpre"]

    # ! Domestic Shipping
    #         T22(33,iy,is) = dot_product(TRQDOMS(1:4,IY),c8(1:4))     !  Shipping, Domestic
    # z[33] = c8.reset_index(drop=True) * dfd['TRQDOMS']).reset_index(drop=True.loc[1:4].sum() / 10**9
    z[33] = (c8 * dfd["TRQDOMS"].loc[1:4]).sum() * dfd["TRIL_TO_QUAD_rwpre"]

    # ! International Shipping
    #         T22(34,iy,is) = dot_product(TRQINTS(1:4,IY),c8(1:4))     !  Shipping, International
    # z[34] = c8.reset_index(drop=True) * dfd['TRQINTS']).reset_index(drop=True.loc[1:4].sum() / 10**9
    z[34] = (c8 * dfd["TRQINTS"].loc[1:4]).sum() * dfd["TRIL_TO_QUAD_rwpre"]

    # ! Recreational Boats
    #         c8=0.
    #         c8(1)=(em_tran(1,11,icy)+em_tran(3,11,icy))/qMGtr(11,iy) ! motor gasoline adjusted for ethanol
    #         c8(2)=(em_tran(6,11,icy)+em_tran(7,11,icy))/qDStr(11,iy) !   diesel with biodiesel adjustment
    #         T22(35,iy,is) = dot_product(TRQBOAT(1:2,IY),c8(1:2))     !  Recreational Boats
    c8 = dfd["TRQBOAT"].copy()
    c8.loc[1] = (
        (dfd["EM_TRAN"].loc[1, 11] + dfd["EM_TRAN"].loc[3, 11])
        / dfd["QMGTR"].loc[11]
        * C_CO2_FACTOR
    )
    c8.loc[2] = (
        (dfd["EM_TRAN"].loc[6, 11] + dfd["EM_TRAN"].loc[7, 11])
        / dfd["QDSTR"].loc[11]
        * C_CO2_FACTOR
    )
    c8.loc[:, :start_y] = 0

    z[35] = (c8.loc[1:2] * dfd["TRQBOAT"].loc[1:2]).sum() * dfd["TRIL_TO_QUAD_rwpre"]

    # ! Air
    #         c8(1)=eJFtr(iy)*C_CO2_FACTOR
    #         c8(2)=eMGtr(iy)*C_CO2_FACTOR  ! aviation gas is carried in other tran category, which is mostly lubricants so has a low factor.  use pure mg factor instead here.
    #         T22(36,iy,is) = dot_product(TRQAIRT(1:2,IY),c8(1:2))     !  Air
    c8 = dfd["TRQAIRT"].copy()
    c8.loc[1] = dfd["EJFTR"].values * C_CO2_FACTOR
    c8.loc[2] = dfd["EMGTR"].values * C_CO2_FACTOR
    c8.loc[:, :start_y] = 0

    z[36] = (c8.loc[1:2] * dfd["TRQAIRT"].loc[1:2]).sum() * dfd["TRIL_TO_QUAD_rwpre"]

    # !  Military Use
    #         c8=0.
    #         c8(1:2)=eJFtr(iy)*C_CO2_FACTOR
    #         c8(3)=eRStr(iy)*C_CO2_FACTOR
    #         c8(4)=(em_tran(6,11,icy)+em_tran(7,11,icy))/qDStr(11,iy) !   diesel with biodiesel adjustment
    #         T22(37,iy,is) = dot_product(TRQMIL(1:4,IY),c8(1:4))      !  Military Use
    c8 = dfd["TRQMIL"].copy()
    c8.loc[1] = dfd["EJFTR"].values * C_CO2_FACTOR
    c8.loc[2] = dfd["EJFTR"].values * C_CO2_FACTOR
    c8.loc[3] = dfd["ERSTR"].values * C_CO2_FACTOR
    c8.loc[4] = (
        (dfd["EM_TRAN"].loc[6, 11] + dfd["EM_TRAN"].loc[7, 11])
        / dfd["QDSTR"].loc[11]
        * C_CO2_FACTOR
    )
    c8.loc[:, :start_y] = 0

    z[37] = (c8 * dfd["TRQMIL"].loc[1:4]).sum() * dfd["TRIL_TO_QUAD_rwpre"]

    #  Lubricants
    #         T22(38,iy,is) = TRQLUB(IY)*eOTtr(iy)*C_CO2_FACTOR      !  Lubricants

    #         T22(39,iy,is) = QGPTR(11,IY)*eGPtr(iy)*C_CO2_FACTOR*1000.    !  Pipeline Fuel
    #         T22(40,iy,is) = sum(T22(27:39,iy,is))                  !    Total
    #         T22(27:40,iy,is) = T22(27:40,iy,is)*.001
    #         T22(66,iy,is)= sum(em_tran(1:iel_T,11,icy))-T22(40,iy,is)  ! discrepancy
    #         T22(40,iy,is) = T22(40,iy,is) + T22(66,iy,is)           ! total with discrepancy to match table 17
    z[38] = dfd["TRQLUB"] * dfd["EOTTR"] * C_CO2_FACTOR * dfd["TRIL_TO_QUAD_rwpre"]

    #   Pipeline Fuel
    # QGPTR R    4 2 (MNUMCR,MNUMYR) QUNTY  tBTU   GP     TRANSP Natural Gas - Pipeline
    # EGPTR R    4 1 (MNUMYR)        CONFAC mMTC/Q GP     TRANSP Natural Gas - Pipeline
    z[39] = (
        dfd["QGPTR"].loc[11] * dfd["EGPTR"] * C_CO2_FACTOR * dfd["TRIL_TO_QUAD_rwpre"]
    )

    #   Total Transportation
    z[40] = (
        z[27]
        + z[28]
        + z[29]
        + z[30]
        + z[31]
        + z[32]
        + z[33]
        + z[34]
        + z[35]
        + z[36]
        + z[37]
        + z[38]
        + z[39]
    )

    #   Discrepancy 5/
    z[66] = dfd["EM_TRAN"].loc[1:iel_T, 11, :].sum() * C_CO2_FACTOR / 1000.0 - z[40]
    z[40] = z[40] + z[66]

    return z
