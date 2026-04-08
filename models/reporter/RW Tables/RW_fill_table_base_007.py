# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
Fixed by SZO on 7/8/20124
"""


def fill_table_base_007(dfd, table_spec, table_id):
    """Fill table  Transportation Sector Key Indicators and Delivered Energy Co

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

    FYRPRC = int(table_spec["settings"]["year_index_real_dol_priceyr"])
    # Get inflation index from 1990->2050
    MC_JPGDP = dfd["MC_JPGDP"].loc[1990:]

    # Get inflation index of FYRPRC (2022)
    SCALPR2 = MC_JPGDP.loc[FYRPRC]  # 2022 = column 33
    SCALPR90 = MC_JPGDP.iloc[0]  # tran prices are in $90 instead of $87
    
    
    z = {}

    TRIL_TO_QUAD = dfd["TRIL_TO_QUAD_rwpre"]
    MNUMCR = dfd["MNUMCR_rwpre"]
    RDAYS = dfd["RDAYS_rwpre"]
    BTU_BBL = dfd["BTU_BBL_rwpre"]

    FYRPRC = int(table_spec["settings"]["year_index_real_dol_priceyr"])

    # Get inflation index from 1990->2050
    MC_JPGDP = dfd["MC_JPGDP"].loc[1990:]

    # Get inflation index of FYRPRC (2022)
    SCALPR2 = MC_JPGDP.loc[FYRPRC]  # 2022 = column 33

    #   Transportation Sector Key Indicators and Delivered Energy Co
    #    Key Indicators and Consumption
    #
    #   Key Indicators
    #   Travel Indicators
    #    (billion vehicle miles traveled)

    #      Light-Duty Vehicles less than 8,501 pounds
    z[1] = dfd["TRLDVMTE"].loc[1]

    #      Commercial Light Trucks 1/
    z[2] = dfd["BCLTVMT"].loc[1:12, :].sum()

    #      2- and 3- Wheel Vehicles
    z[3] = dfd["PAS_RPM"].loc[1]

    #      Light-Duty Vehicles minus 2- and 3- Wheelers
    z[4] = dfd["TRLDVMTE"].loc[1] - dfd["PAS_RPM"].loc[1]

    #      Freight Trucks greater than 10,000 pounds
    z[5] = dfd["TRVMTTRK"].loc[1, 1:12, :].sum() + dfd["TRVMTTRK"].loc[2, 1:12, :].sum()

    #    (billion passenger miles traveled)

    #      Bus Transportation
    z[6] = dfd["PAS_RPM"].loc[2]

    #      Passenger Rail
    z[7] = dfd["PAS_RPM"].loc[3]

    #    (billion seat miles available)

    #      Air
    z[8] = dfd["TRSTMDEM"].loc[1]

    #    (billion ton miles traveled)

    #      Rail
    z[9] = dfd["TRTMRR"].loc[1]

    #      Domestic Shipping
    z[10] = dfd["TRTMSHIP"].loc[1]

    #

    #   Energy Efficiency Indicators
    #    (miles per gallon)

    #      New Light-Duty Vehicle CAFE Standard 2/
    z[11] = dfd["CAFESTD"].loc[3]

    #        New Car 2/
    z[12] = dfd["CAFESTD"].loc[1]

    #        New Light Truck 2/
    z[13] = dfd["CAFESTD"].loc[2]

    #      CAFE with Commercial Light Truck 2/
    z[14] = dfd["LDV_MPG"].loc[1]

    #      Compliance New Light-Duty Vehicle 3/
    z[15] = dfd["NEWMPG"].loc[3]

    #        New Car 3/
    z[16] = dfd["NEWMPG"].loc[1]

    #        New Light Truck 3/
    z[17] = dfd["NEWMPG"].loc[2]

    #      Tested New Light-Duty Vehicle 4/
    z[18] = dfd["TRUEMPG"].loc[3]

    #        New Car 4/
    z[19] = dfd["TRUEMPG"].loc[1]

    #        New Light Truck 4/
    z[20] = dfd["TRUEMPG"].loc[2]

    #      On-Road New Light-Duty Vehicle 5/
    z[23] = dfd["ADJMPG"].loc[3]
    
    #        New Car 5/
    z[21] = dfd["ADJMPG"].loc[1]

    #        New Light Truck 5/
    z[22] = dfd["ADJMPG"].loc[2]


    #      Light-Duty Stock 6/
    z[24] = dfd["TRLDMPGF"].loc[3]

    #      New Commercial Light Truck 1/
    z[25] = dfd["NCLTMPGT"]

    #      Stock Commercial Light Truck 1/
    z[26] = dfd["CLTMPGT"]

    #      Freight Truck
    z[27] = dfd["TRFTMPG"]

    #    (seat miles per gallon)

    #      Aircraft
    z[28] = dfd["TRAIREFFS"].loc[4]

    #    (ton miles/thousand Btu)

    #      Rail
    z[29] = dfd["TRTMRR"].loc[2]

    #      Domestic Shipping
    z[30] = dfd["TRTMSHIP"].loc[2]

    #   Energy Use by Mode

    #     (quadrillion Btu)

    #       Light-Duty Vehicles
    z[31] = (
        dfd["TRQLDV"].loc[1].loc[MNUMCR]
        + dfd["TRQLDV"].loc[2].loc[MNUMCR]
        + dfd["TRQLDV"].loc[3].loc[MNUMCR]
        + dfd["TRQLDV"].loc[4].loc[MNUMCR]
        + dfd["TRQLDV"].loc[5].loc[MNUMCR]
        + dfd["TRQLDV"].loc[6].loc[MNUMCR]
        + dfd["TRQLDV"].loc[7].loc[MNUMCR]
        + dfd["TRQLDV"].loc[8].loc[MNUMCR]
    ) * TRIL_TO_QUAD

    #       Commercial Light Trucks 1/
    z[32] = dfd["BCLTBTUT"].loc[1:12,:].sum() * TRIL_TO_QUAD

    #       Bus Transportation
    z[33] = dfd["TRQBUS"].loc[:, :, :].sum() * TRIL_TO_QUAD

    #       Freight Trucks
    z[34] = (
        dfd["TRQFTRK"].loc[1]
        + dfd["TRQFTRK"].loc[2]
        + dfd["TRQFTRK"].loc[3]
        + dfd["TRQFTRK"].loc[4]
        + dfd["TRQFTRK"].loc[5]
        + dfd["TRQFTRK"].loc[6]
        + dfd["TRQFTRK"].loc[7]
        + dfd["TRQFTRK"].loc[8]
    ) * TRIL_TO_QUAD

    #       Rail, Passenger
    z[35] = (
        dfd["TRQRRP"].loc[1]
        + dfd["TRQRRP"].loc[2]
        + dfd["TRQRRP"].loc[3]
        + dfd["TRQRRP"].loc[4]
        + dfd["TRQRRP"].loc[5]
        + dfd["TRQRRP"].loc[6]
        + dfd["TRQRRP"].loc[7]
        + dfd["TRQRRP"].loc[8]
        + dfd["TRQRRP"].loc[9]
    ) * TRIL_TO_QUAD

    #       Rail, Freight
    z[36] = (
        dfd["TRQRRF"].loc[1]
        + dfd["TRQRRF"].loc[2]
        + dfd["TRQRRF"].loc[3]
        + dfd["TRQRRF"].loc[4]
    ) * TRIL_TO_QUAD

    #       Shipping, Domestic
    z[37] = (
        dfd["TRQDOMS"].loc[1]
        + dfd["TRQDOMS"].loc[2]
        + dfd["TRQDOMS"].loc[3]
        + dfd["TRQDOMS"].loc[4]
    ) * TRIL_TO_QUAD

    #       Shipping, International
    z[38] = (
        dfd["TRQINTS"].loc[1]
        + dfd["TRQINTS"].loc[2]
        + dfd["TRQINTS"].loc[3]
        + dfd["TRQINTS"].loc[4]
    ) * TRIL_TO_QUAD

    #       Recreational Boats
    z[39] = (dfd["TRQBOAT"].loc[1] + dfd["TRQBOAT"].loc[2]) * TRIL_TO_QUAD

    #       Air
    z[40] = (dfd["TRQAIRT"].loc[1] + dfd["TRQAIRT"].loc[2]) * TRIL_TO_QUAD

    #       Military Use
    z[41] = (
        dfd["TRQMIL"].loc[1]
        + dfd["TRQMIL"].loc[2]
        + dfd["TRQMIL"].loc[3]
        + dfd["TRQMIL"].loc[4]
    ) * TRIL_TO_QUAD

    #       Lubricants
    z[42] = dfd["TRQLUB"] * TRIL_TO_QUAD

    #       Pipeline Fuel
    z[43] = dfd["QGPTR"].loc[MNUMCR] * TRIL_TO_QUAD

    #      Natural Gas Liquefaction for Export 7/
    z[44] = dfd["QNGLQ"].loc[MNUMCR] * TRIL_TO_QUAD

    #         Total
    z[45] = (
        z[31]
        + z[32]
        + z[33]
        + z[34]
        + z[35]
        + z[36]
        + z[37]
        + z[38]
        + z[39]
        + z[40]
        + z[41]
        + z[42]
        + z[43]
        + z[44]
    )

    #       Light-Duty Vehicles
    z[46] = (
        (dfd["TRQLDV"].loc[1].loc[MNUMCR] / dfd["CFMGQ"])
        + (dfd["TRQLDV"].loc[2].loc[MNUMCR] / dfd["CFM85Q"])
        + (dfd["TRQLDV"].loc[3].loc[MNUMCR] / dfd["CFETQ"])
        + (dfd["TRQLDV"].loc[4].loc[MNUMCR] / BTU_BBL)
        + (dfd["TRQLDV"].loc[5].loc[MNUMCR] / dfd["CFLGQ"])
        + (dfd["TRQLDV"].loc[6].loc[MNUMCR] / BTU_BBL)
        + (dfd["TRQLDV"].loc[7].loc[MNUMCR] / BTU_BBL)
        + (dfd["TRQLDV"].loc[8].loc[MNUMCR] / dfd["CFDSTR"])
    ) / RDAYS

    #       Commercial Light Trucks 1/
    z[47] = (
        (dfd["CLTFUELBTU"].loc[1] / dfd["CFMGQ"])
        + (dfd["CLTFUELBTU"].loc[2] / dfd["CFDSTR"])
        + (dfd["CLTFUELBTU"].loc[3] / dfd["CFPRQ"].iloc[0])
        + (dfd["CLTFUELBTU"].loc[4] / BTU_BBL)
        + (dfd["CLTFUELBTU"].loc[5] / dfd["CFE85Q"])
        + (dfd["CLTFUELBTU"].loc[6] / BTU_BBL)
        + (dfd["CLTFUELBTU"].loc[7] / BTU_BBL)
    ) / RDAYS

    #       Bus Transportation
    z[48] = (
        dfd["TRQBUS"].loc[:, 1, :].sum() / dfd["CFMGQ"]
        + dfd["TRQBUS"].loc[:, 2, :].sum() / dfd["CFDSTR"]
        + dfd["TRQBUS"].loc[:, 3, :].sum() / dfd["CFE85Q"]
        + dfd["TRQBUS"].loc[:, 5, :].sum() / 5.8
        + dfd["TRQBUS"].loc[:, 6, :].sum() / dfd["CFLGQ"]
        + dfd["TRQBUS"].loc[:, 7, :].sum() / 5.8
        + dfd["TRQBUS"].loc[:, 8, :].sum() / 5.8
    ) / RDAYS

    #       Freight Trucks
    z[49] = (
        (dfd["TRQFTRK"].loc[1] / dfd["CFMGQ"])
        + (dfd["TRQFTRK"].loc[2] / dfd["CFDSTR"])
        + (dfd["TRQFTRK"].loc[3] / BTU_BBL)
        + (dfd["TRQFTRK"].loc[5] / dfd["CFLGQ"])
    ) / RDAYS

    #       Rail, Passenger
    z[50] = (
        (
            dfd["TRQRRP"].loc[1]
            + dfd["TRQRRP"].loc[3]
            + dfd["TRQRRP"].loc[4]
            + dfd["TRQRRP"].loc[5]
            + dfd["TRQRRP"].loc[6]
            + dfd["TRQRRP"].loc[8]
            + dfd["TRQRRP"].loc[9]
        )
        / BTU_BBL
        + (dfd["TRQRRP"].loc[2] + dfd["TRQRRP"].loc[7]) / dfd["CFDSTR"]
    ) / RDAYS

    #       Rail, Freight       not printing
    z[51] = (
        dfd["TRQRRF"].loc[1] / dfd["CFDSTR"]
        + dfd["TRQRRF"].loc[2] / dfd["CFRSQ"].iloc[0]
        + (dfd["TRQRRF"].loc[3] + dfd["TRQRRF"].loc[4]) / BTU_BBL
    ) / RDAYS

    #       Shipping, Domestic
    z[52] = (
        dfd["TRQDOMS"].loc[1] / dfd["CFDSTR"]
        + dfd["TRQDOMS"].loc[2] / dfd["CFRSQ"].iloc[0]
        + (dfd["TRQDOMS"].loc[3] + dfd["TRQDOMS"].loc[4]) / BTU_BBL
    ) / RDAYS

    #       Shipping, International
    z[53] = (
        dfd["TRQINTS"].loc[1] / dfd["CFDSTR"]
        + dfd["TRQINTS"].loc[2] / dfd["CFRSQ"].iloc[0]
        + (dfd["TRQINTS"].loc[3] + dfd["TRQINTS"].loc[4]) / BTU_BBL
    ) / RDAYS

    #       Recreational Boats
    z[54] = ((dfd["TRQBOAT"].loc[1] + dfd["TRQBOAT"].loc[2]) / dfd["CFMGQ"]) / RDAYS

    #       Air
    z[55] = (
        (dfd["TRQAIRT"].loc[1] / dfd["CFJFK"].iloc[0])
        + (dfd["TRQAIRT"].loc[2] / dfd["CFMGQ"])
    ) / RDAYS

    #       Military Use
    z[56] = (
        ((dfd["TRQMIL"].loc[1] + dfd["TRQMIL"].loc[2]) / dfd["CFJFK"].iloc[0])
        + (dfd["TRQMIL"].loc[3] / dfd["CFRSQ"].iloc[0])
        + (dfd["TRQMIL"].loc[4] / dfd["CFDSTR"])
    ) / RDAYS

    #       Lubricants
    z[57] = (dfd["TRQLUB"] / dfd["CFOTQ"]) / RDAYS

    #       Pipeline Fuel
    z[58] = (dfd["QGPTR"].loc[MNUMCR] / BTU_BBL) / RDAYS

    #      Natural Gas Liquefaction for Export 7/
    z[59] = (dfd["QNGLQ"].loc[MNUMCR] / BTU_BBL) / RDAYS

    #         Total
    z[60] = (
        z[46]
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
        + z[57]
        + z[58]
        + z[59]
    )

    #   Electricity Prices by End Use ($/kWh)

    # Light Trains
    z[61] = dfd["PELLTTR"].loc[MNUMCR] * SCALPR2 * 3412 / 1000000 * 100

    # Light-duty vehicles

    z[62] =(dfd["EUSPRC/PELVHRS"].loc[1] * dfd['TRQ_ELEC'].loc[1].loc[1] +
            dfd["EUSPRC/PELVHRS"].loc[2] * dfd['TRQ_ELEC'].loc[1].loc[2] +
            dfd["EUSPRC/PELVHRS"].loc[3] * dfd['TRQ_ELEC'].loc[1].loc[3] +
            dfd["EUSPRC/PELVHRS"].loc[4] * dfd['TRQ_ELEC'].loc[1].loc[4] +
            dfd["EUSPRC/PELVHRS"].loc[5] * dfd['TRQ_ELEC'].loc[1].loc[5] +
            dfd["EUSPRC/PELVHRS"].loc[6] * dfd['TRQ_ELEC'].loc[1].loc[6] +
            dfd["EUSPRC/PELVHRS"].loc[7] * dfd['TRQ_ELEC'].loc[1].loc[7] +
            dfd["EUSPRC/PELVHRS"].loc[8] * dfd['TRQ_ELEC'].loc[1].loc[8] +
            dfd["EUSPRC/PELVHRS"].loc[9] * dfd['TRQ_ELEC'].loc[1].loc[9]) / (dfd['TRQ_ELEC'].loc[1,1:9,:].sum()) * SCALPR2 * 3412 / 1000000 * 1.25666666666667 * 100

    z[63] =(dfd["EUSPRC/PELP2CM"].loc[1] * dfd['TRQ_ELEC'].loc[2].loc[1] +
            dfd["EUSPRC/PELP2CM"].loc[2] * dfd['TRQ_ELEC'].loc[2].loc[2] +
            dfd["EUSPRC/PELP2CM"].loc[3] * dfd['TRQ_ELEC'].loc[2].loc[3] +
            dfd["EUSPRC/PELP2CM"].loc[4] * dfd['TRQ_ELEC'].loc[2].loc[4] +
            dfd["EUSPRC/PELP2CM"].loc[5] * dfd['TRQ_ELEC'].loc[2].loc[5] +
            dfd["EUSPRC/PELP2CM"].loc[6] * dfd['TRQ_ELEC'].loc[2].loc[6] +
            dfd["EUSPRC/PELP2CM"].loc[7] * dfd['TRQ_ELEC'].loc[2].loc[7] +
            dfd["EUSPRC/PELP2CM"].loc[8] * dfd['TRQ_ELEC'].loc[2].loc[8] +
            dfd["EUSPRC/PELP2CM"].loc[9] * dfd['TRQ_ELEC'].loc[2].loc[9]) / (dfd['TRQ_ELEC'].loc[2,1:9,:].sum()) * SCALPR2 * 3412 / 1000000 * 1.87096774193548 * 100

    z[64] =(dfd["EUSPRC/PELPFCM"].loc[1] * dfd['TRQ_ELEC'].loc[3].loc[1] +
            dfd["EUSPRC/PELPFCM"].loc[2] * dfd['TRQ_ELEC'].loc[3].loc[2] +
            dfd["EUSPRC/PELPFCM"].loc[3] * dfd['TRQ_ELEC'].loc[3].loc[3] +
            dfd["EUSPRC/PELPFCM"].loc[4] * dfd['TRQ_ELEC'].loc[3].loc[4] +
            dfd["EUSPRC/PELPFCM"].loc[5] * dfd['TRQ_ELEC'].loc[3].loc[5] +
            dfd["EUSPRC/PELPFCM"].loc[6] * dfd['TRQ_ELEC'].loc[3].loc[6] +
            dfd["EUSPRC/PELPFCM"].loc[7] * dfd['TRQ_ELEC'].loc[3].loc[7] +
            dfd["EUSPRC/PELPFCM"].loc[8] * dfd['TRQ_ELEC'].loc[3].loc[8] +
            dfd["EUSPRC/PELPFCM"].loc[9] * dfd['TRQ_ELEC'].loc[3].loc[9]) / (dfd['TRQ_ELEC'].loc[3,1:9,:].sum()) * SCALPR2 * 3412 / 1000000 * 2.95161290322581 * 100

    # LDV consumption-weighted average
    z[65] = (z[62] * dfd['TRQ_ELEC'].loc[1].loc[MNUMCR]
          +  z[63] * dfd['TRQ_ELEC'].loc[2].loc[MNUMCR]
          +  z[64] * dfd['TRQ_ELEC'].loc[3].loc[MNUMCR]
          ) / (dfd['TRQ_ELEC'].loc[1:3,MNUMCR,:].sum())

    # Bus
    z[66] = dfd["EUSPRC/PELSBCM"].loc[MNUMCR] * SCALPR2 * 3412 / 1000000  * 100        # School
    z[67] = dfd["EUSPRC/PELTBCM"].loc[MNUMCR] * SCALPR2 * 3412 / 1000000  * 100        # Transit
    z[68] = dfd["EUSPRC/PELIBCM"].loc[MNUMCR] * SCALPR2 * 3412 / 1000000  * 100        # Intercity

    # Bus consumption-weighted average
    z[69] = (z[66] * dfd['TRQ_ELEC'].loc[4].loc[MNUMCR]
          +  z[67] * dfd['TRQ_ELEC'].loc[5].loc[MNUMCR]
          +  z[68] * dfd['TRQ_ELEC'].loc[6].loc[MNUMCR]
          )  / (dfd['TRQ_ELEC'].loc[4:6,MNUMCR,:].sum())

    # Freight truck
    z[70] = z[68]                                                                      # Freight truck fleet
    z[71] = dfd["EUSPRC/PELFNCM"].loc[MNUMCR] * SCALPR2 * 3412 / 1000000 * 2.49193548387097 * 100  # Freight truck non-fleet    
                                                                                       
    # Commercial light truck                                                           
    z[95] = z[70]                                                                      # CLT fleet
    z[96] = dfd["EUSPRC/PELFNCM"].loc[MNUMCR] * SCALPR2 * 3412 / 1000000 * 2.49193548387097 * 100  # CLT non-fleet   

    # Freight truck consumption-weighted average
    z[72] = (z[70] * dfd['TRQ_ELEC'].loc[8].loc[MNUMCR]
          +  z[71] * dfd['TRQ_ELEC'].loc[9].loc[MNUMCR]
          ) / (dfd['TRQ_ELEC'].loc[8:9,MNUMCR,:].sum())

    # Hydrogen price (####$/kg)
    z[94] = dfd['AMPBLK/PH2TR'].loc[MNUMCR] / dfd['CFH2Q_KG'].iloc[0] * SCALPR2

    # Natural gas delivered prices

    # Compressed Natural Gas

    # Light-Duty Vehicles and Freight Trucks
    z[73] = (
        (
            (
                dfd["ANGTDM/PGFTRFV"].loc[MNUMCR] * dfd["QGFTRFV"].loc[MNUMCR]
                + dfd["ANGTDM/PGFTRPV"].loc[MNUMCR] * dfd["QGFTRPV"].loc[MNUMCR]
            )
            / (dfd["QGFTRFV"].loc[MNUMCR] + dfd["QGFTRPV"].loc[MNUMCR])
        )
        * SCALPR2
    ).fillna(0)

    # Non-fleet
    z[74] = dfd["ANGTDM/PGFTRPV"].loc[MNUMCR] * SCALPR2

    # Fleet
    z[75] = dfd["ANGTDM/PGFTRFV"].loc[MNUMCR] * SCALPR2

    # Marine
    z[76] = (
        (
            (
                (
                    dfd["ANGTDM/PGFTRSHIP"].loc[1].loc[MNUMCR]
                    * dfd["QGFTRSHIP"].loc[1].loc[MNUMCR]
                    + dfd["ANGTDM/PGFTRSHIP"].loc[2].loc[MNUMCR]
                    * dfd["QGFTRSHIP"].loc[2].loc[MNUMCR]
                    + dfd["ANGTDM/PGFTRSHIP"].loc[3].loc[MNUMCR]
                    * dfd["QGFTRSHIP"].loc[3].loc[MNUMCR]
                )
                / (
                    dfd["QGFTRSHIP"].loc[1].loc[MNUMCR]
                    + dfd["QGFTRSHIP"].loc[2].loc[MNUMCR]
                    + dfd["QGFTRSHIP"].loc[3].loc[MNUMCR]
                )
            )
            * SCALPR2
        )
    ).fillna(0)

    # Domestic
    z[77] = dfd["ANGTDM/PGFTRSHIP"].loc[1].loc[MNUMCR] * SCALPR2

    # International
    z[78] = dfd["ANGTDM/PGFTRSHIP"].loc[2].loc[MNUMCR] * SCALPR2


    # Liquefied Natural Gas

    # Freight trucks
    z[79] = (
        (
            dfd["ANGTDM/PGLTRFV"].loc[MNUMCR] * dfd["QGLTRFV"].loc[MNUMCR]
            + dfd["ANGTDM/PGLTRPV"].loc[MNUMCR] * dfd["QGLTRPV"].loc[MNUMCR]
        )
        / (dfd["QGLTRFV"].loc[MNUMCR] + dfd["QGLTRPV"].loc[MNUMCR])
        * SCALPR2 * 2
    ).fillna(0)

    # Non fleet
    z[80] = dfd["ANGTDM/PGLTRPV"].loc[MNUMCR] * SCALPR2 * 2

    # Fleet
    z[81] = dfd["ANGTDM/PGLTRFV"].loc[MNUMCR] * SCALPR2 * 2

    # Freight Rail
    z[82] = dfd["ANGTDM/PGLTRRAIL"].loc[1].loc[MNUMCR] * SCALPR2 * 2

    # Shipping
    z[83] = (
        (
            (
                dfd["ANGTDM/PGLTRSHIP"].loc[1].loc[MNUMCR]
                * dfd["QGLTRSHIP"].loc[1].loc[MNUMCR]
                + dfd["ANGTDM/PGLTRSHIP"].loc[2].loc[MNUMCR]
                * dfd["QGLTRSHIP"].loc[2].loc[MNUMCR]
                + dfd["ANGTDM/PGLTRSHIP"].loc[3].loc[MNUMCR]
                * dfd["QGLTRSHIP"].loc[3].loc[MNUMCR]
            )
            / (
                dfd["QGLTRSHIP"].loc[1].loc[MNUMCR]
                + dfd["QGLTRSHIP"].loc[2].loc[MNUMCR]
                + dfd["QGLTRSHIP"].loc[3].loc[MNUMCR]
            )
        )
        * SCALPR2
    ).fillna(0)

    # Domestic
    z[84] = dfd["ANGTDM/PGLTRSHIP"].loc[1].loc[MNUMCR] * SCALPR2

    # International
    z[85] = dfd["ANGTDM/PGLTRSHIP"].loc[2].loc[MNUMCR] * SCALPR2 
    
    
    # Battery prices
    
    z[86] = dfd["LI_ION_COST"].loc[15] / SCALPR90 * SCALPR2     # LDV BEV
    z[87] = dfd["LI_ION_COST"].loc[5] / SCALPR90 * SCALPR2      # LDV PHEV
    z[88] = dfd["LI_ION_COST"].loc[16] / SCALPR90 * SCALPR2     # LDV HEV

    z[89] = dfd["COST_BATTKWH_TRK"].loc[1] / SCALPR90 * SCALPR2 # HDV C4-5
    z[90] = dfd["COST_BATTKWH_TRK"].loc[2] / SCALPR90 * SCALPR2 # HDV C6-8V and PHEVG/D
    z[91] = dfd["COST_BATTKWH_TRK"].loc[3] / SCALPR90 * SCALPR2 # HDV C7&8T
    z[92] = dfd["COST_BATTKWH_TRK"].loc[4] / SCALPR90 * SCALPR2 # HDV C2b-3
    z[93] = dfd["COST_BATTKWH_TRK"].loc[5] / SCALPR90 * SCALPR2 # HDV HEV

    z[97] = dfd["GLOBAL_BATT_PROD"].loc[1].loc[1]
    z[98] = dfd["GLOBAL_BATT_PROD"].loc[2].loc[1]
    z[99] = z[97] + z[98]

    return z
