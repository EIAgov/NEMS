# -*- coding: utf-8 -*-
"""
Created on Thu Oct 24 13:40:27 2024

@author: GSV
"""

def fill_table_base_069(dfd, table_spec, table_id):
    """Fill table for Carbon Dioxide Emissions by Category and Sector.

    The function returns a dict in which each key is an integer
    references a table row, and each value is a dataframe indexed
    by region number. The integer keys are the same as "IROWS" in
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


    # Index for last census division (national total)
    MNUMCR = int(dfd["MNUMCR_rwpre"])


    # Fossil fuel combustion---------
    # Residential petroleum
    z[1] = (
        dfd['EM_RESD'].loc[(slice(1, 3), MNUMCR), :].sum()
        * dfd['C_CO2_FACTOR_rwpre'] / 1000.0
    )

    # Residential natural gas
    z[2] = (
        dfd['EM_RESD'].loc[(5, MNUMCR), :] * dfd['C_CO2_FACTOR_rwpre'] / 1000.0
    )

    # Commercial petroleum
    z[3] = (
        dfd['EM_COMM'].loc[(slice(1, 5), MNUMCR), :].sum()
        * dfd['C_CO2_FACTOR_rwpre'] / 1000.0
    )

    # Commercial natural gas
    z[4] = (
        dfd['EM_COMM'].loc[(7, MNUMCR), :] * dfd['C_CO2_FACTOR_rwpre'] / 1000.0
    )

    # Commercial coal
    z[5] = (
        dfd['EM_COMM'].loc[(6, MNUMCR), :] * dfd['C_CO2_FACTOR_rwpre'] / 1000.0
    )

    # Industrial petroleum
    z[6] = (
        (
            dfd['EM_INDY'].loc[(slice(1, 2), MNUMCR), :].sum()
            + dfd['EM_INDY'].loc[(slice(4, 6), MNUMCR), :].sum()
            + dfd['EM_INDY'].loc[(slice(8, 10), MNUMCR), :].sum()
        ) * dfd['C_CO2_FACTOR_rwpre'] / 1000.0
    )

    # Industrial natural gas
    z[7] = (
        (
            dfd['EM_INDY'].loc[(14, MNUMCR), :]
            + dfd['EM_INDY'].loc[(slice(16, 17), MNUMCR), :].sum()
        ) * dfd['C_CO2_FACTOR_rwpre'] / 1000.0
    )

    # Industrial coal
    z[8] = (
        (
            dfd['EM_INDY'].loc[(11, MNUMCR), :]
            + dfd['EM_INDY'].loc[(13, MNUMCR), :]
        ) * dfd['C_CO2_FACTOR_rwpre'] / 1000.0
    )

    # Transportation petroleum
    z[9] = (
        (
            dfd['EM_TRAN'].loc[(slice(1, 9), MNUMCR), :].sum()
            + dfd['EM_TRAN'].loc[(11, MNUMCR), :]
        ) * dfd['C_CO2_FACTOR_rwpre'] / 1000.0
    )

    # Transportation natural gas
    z[10] = (
        dfd['EM_TRAN'].loc[(10, MNUMCR), :]
        * dfd['C_CO2_FACTOR_rwpre'] / 1000.0
    )

    # Electric power petroleum
    z[11] = (
        dfd['EM_ELEC'].loc[(slice(1, 3), MNUMCR), :].sum()
        * dfd['C_CO2_FACTOR_rwpre'] / 1000.0
    )

    # Electric power natural gas
    z[12] = (
        dfd['EM_ELEC'].loc[(5, MNUMCR), :] * dfd['C_CO2_FACTOR_rwpre'] / 1000.0
    )

    # Electric power coal
    z[13] = (
        dfd['EM_ELEC'].loc[(4, MNUMCR), :] * dfd['C_CO2_FACTOR_rwpre'] / 1000.0
    )

    # Electric power non-biogenic waste
    z[14] = (
        dfd['EM_ELEC'].loc[(6, MNUMCR), :] * dfd['C_CO2_FACTOR_rwpre'] / 1000.0
    )

    # Total fossil fuel combustion
    z[15] = (
        z[1] + z[2]  # Residential
        + z[3] + z[4] + z[5]  # Commercial
        + z[6] + z[7] + z[8]  # Industrial
        + z[9] + z[10]  # Transportation
        + z[11] + z[12] + z[13] + z[14]  # Electric power
    )


    # Biomass Combustion---------
    # NOTE: This section consists mostly of placeholders for cross-references
    #       to T24. They are implemented in the RW_postprocessor_base file.
    # Residential woody biomass
    z[16] = z[1] * 0.0

    # Commercial woody biomass
    z[17] = z[1] * 0.0

    # Industrial woody biomass
    z[18] = z[1] * 0.0

    # Industrial biogenic waste
    z[19] = z[1] * 0.0

    # Industrial ethanol (commented out in layin.csv)
    # TODO: Is this included in biofuels heat and coproducts?
    z[20] = z[1] * 0.0

    # Industrial biofuels heat and coproducts
    z[21] = z[1] * 0.0

    # Transportation ethanol
    z[22] = z[1] * 0.0

    # Transportation biodiesel
    z[23] = z[1] * 0.0

    # Transportation renewable diesel and gasoline
    z[24] = z[1] * 0.0

    # Transportation renewable natural gas
    z[25] = (
        dfd["OGPRRNG"] * dfd["CFNGC"] * dfd["ENGTR"]
        * dfd['C_CO2_FACTOR_rwpre'] / 1000.0
    )

    # Transportation other biofuels
    z[26] = z[1] * 0.0

    # Electric power woody biomass
    z[27] = z[1] * 0.0

    # Electric power biogenic waste
    z[28] = z[1] * 0.0

    # Total biomass combustion
    z[29] = z[1] * 0.0

    # Sequestered biomass combustion
    z[30] = z[1] * 0.0

    # Net biomass combustion
    z[31] = z[1] * 0.0


    # Vented---------
    # Industrial natural gas processing
    z[32] = (
        dfd['EM_INDY'].loc[(19, MNUMCR), :]
        * dfd['C_CO2_FACTOR_rwpre'] / 1000.0
    )

    # Electric power geothermal
    z[33] = (
        dfd['EM_ELEC'].loc[(7, MNUMCR), :] * dfd['C_CO2_FACTOR_rwpre'] / 1000.0
    )

    # Total vented
    z[34] = z[32] + z[33]


    # Fuel-dependent process---------
    # Industrial petroleum
    z[35] = (
        (
            dfd['EM_INDY'].loc[(3, MNUMCR), :]  # HGL feedstock
            + dfd['EM_INDY'].loc[(7, MNUMCR), :]  # Petrochemical feedstock
        ) * dfd['C_CO2_FACTOR_rwpre'] / 1000.0
    )

    # Industrial natural gas
    z[36] = (
        (
            dfd['EM_INDY'].loc[(15, MNUMCR), :]  # Natural gas feedstock
            + dfd['EM_INDY'].loc[(18, MNUMCR), :]  # Hydrogen feedstock
        ) * dfd['C_CO2_FACTOR_rwpre'] / 1000.0
    )

    # Industrial coal
    z[37] = (
        dfd['EM_INDY'].loc[(12, MNUMCR), :]  # Metallurgical coal
        * dfd['C_CO2_FACTOR_rwpre'] / 1000.0
    )

    # Total fuel-dependent process
    z[38] = z[35] + z[36] + z[37]


    # Total energy-related CO2 emissions---------
    # NOTE: Due to cross-references in the biomass combustion section above, a
    #       couple parts of this section also need to be placeholders that are
    #       implemented in the RW_postprocessor_base file.
    # Fossil fuel combustion
    z[39] = z[15].copy()

    # Net biomass combustion
    z[40] = z[1] * 0.0  # Placeholder

    # Vented
    z[41] = z[34].copy()

    # Fuel-dependent process
    z[42] = z[38].copy()

    # Total energy-related CO2 emissions
    z[43] = z[1] * 0.0  # Placeholder


    # Biomass process---------
    # Industrial ethanol production
    z[44] = dfd['ETH_FERM_CO2'].loc[:] / 1000.0

    # Total biomass process
    z[45] = z[44].copy()  # Sum of 1 row(s)


    # Fuel-independent process---------
    # Industrial bulk chemicals
    z[46] = dfd['GHG_PROCESSIN'].loc[9, :].copy()

    # Industrial glass
    z[47] = dfd['GHG_PROCESSIN'].loc[10, :].copy()

    # Industrial cement and lime
    z[48] = (
        dfd['GHG_PROCESSIN'].loc[11, :]
        # Subtract out *all* captured CO2 emissions from cement kilns
        - (
            dfd['SUP_CMT_45Q'].loc[:, :].sum()  # Eligible for 45Q
            + dfd['SUP_CMT_NTC'].loc[:, :].sum()  # No tax credit
        ) / 1.0e6
        # The above also subtracted out combustion CO2 emissions, but those
        # weren't a part of GHG_PROCESSIN, so we have to add that captured CO2
        # back in to effectively only subtract captured *process* emissions
        + (
            dfd['CC_INDY'].loc[(1, MNUMCR), :]  # Distillate
            + dfd['CC_INDY'].loc[(2, MNUMCR), :]  # Petroleum coke
            + dfd['CC_INDY'].loc[(4, MNUMCR), :]  # Other petroleum
            + dfd['CC_INDY'].loc[(5, MNUMCR), :]  # Steam coal
            + dfd['CC_INDY'].loc[(8, MNUMCR), :]  # Natural gas
        ) * dfd['C_CO2_FACTOR_rwpre'] / 1000.0
    )

    # Industrial iron and steel
    z[49] = dfd['GHG_PROCESSIN'].loc[12, :].copy()

    # Total fuel-independent process
    z[50] = z[46] + z[47] + z[48] + z[49]


    # Other carbon capture and storage---------
    # Industrial cement and lime
    z[51] = (
        (
            dfd['SUP_CMT_45Q'].loc[:, :].sum()  # Eligible for 45Q
            + dfd['SUP_CMT_NTC'].loc[:, :].sum()  # No tax credit
        ) / 1.0e6
        # Exclude captured combustion emissions from cement kilns
        - (
            dfd['CC_INDY'].loc[(1, MNUMCR), :]  # Distillate
            + dfd['CC_INDY'].loc[(2, MNUMCR), :]  # Petroleum coke
            + dfd['CC_INDY'].loc[(4, MNUMCR), :]  # Other petroleum
            + dfd['CC_INDY'].loc[(5, MNUMCR), :]  # Steam coal
            + dfd['CC_INDY'].loc[(8, MNUMCR), :]  # Natural gas
        ) * dfd['C_CO2_FACTOR_rwpre'] / 1000.0
    )

    # Industrial ethanol production
    z[52] = (
        (
            dfd['SUP_ETH_45Q'].loc[1:9, :].sum()  # Eligible for 45Q
            + dfd['SUP_ETH_NTC'].loc[1:9, :].sum()  # No tax credit
        ) / 1.0e6
    )

    # Total other carbon capture and storage
    z[53] = z[51] + z[52]


    return z
