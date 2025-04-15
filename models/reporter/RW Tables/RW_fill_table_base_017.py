# -*- coding: utf-8 -*-
"""
Created on Mon Dec 27 09:10:06 2023

@author: SZO
"""

def fill_table_base_017(dfd, table_spec, table_id):
    """Fill table for Energy-Related Carbon Dioxide Emissions by Sector and Source.

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

    Note
    ----
        It appears tab.f module implemented T17 differently from equations of layin, 
        The cross-references (for electric power CO2 emissions) of T138 to T17 are 
        incorrect in the IROWS specified in layin (see the correct references of T138 
        to T17 in postprocessor_base). Any other tables cross-referencing to T17 
        will need to be carefully reviewed as well.
    """


    z = {}


    # Residential---------
    # Petroleum
    z[1] = (
        dfd['EM_RESD'].loc[1:3].groupby('MNUMCR').sum()
        * dfd['C_CO2_FACTOR_rwpre'] / 1000.0
    )

    # Natural gas
    z[2] = dfd['EM_RESD'].loc[5] * dfd['C_CO2_FACTOR_rwpre'] / 1000.0

    # Coal (commented out in layin.csv)
    z[3] = dfd['EM_RESD'].loc[4] * dfd['C_CO2_FACTOR_rwpre'] / 1000.0

    # Direct residential
    z[4] = z[1] + z[2] + z[3]

    # Electricity (indirect emissions)
    z[5] = dfd['EM_RESD'].loc[6] * dfd['C_CO2_FACTOR_rwpre'] / 1000.0

    # Total residential
    z[6] = z[4] + z[5]


    # Commercial---------
    # Petroleum
    z[7] = (
        dfd['EM_COMM'].loc[1:5].groupby('MNUMCR').sum()
        * dfd['C_CO2_FACTOR_rwpre'] / 1000.0
    )

    # Natural gas
    z[8] = dfd['EM_COMM'].loc[7] * dfd['C_CO2_FACTOR_rwpre'] / 1000.0

    # Coal
    z[9] = dfd['EM_COMM'].loc[6] * dfd['C_CO2_FACTOR_rwpre'] / 1000.0

    # Direct commercial
    z[10] = z[7] + z[8] + z[9]

    # Electricity (indirect emissions)
    z[11] = dfd['EM_COMM'].loc[8] * dfd['C_CO2_FACTOR_rwpre'] / 1000.0

    # Total commercial
    z[12] = z[10] + z[11]


    # Industrial---------
    # Petroleum
    z[13] = (
        dfd['EM_INDY'].loc[1:10].groupby('MNUMCR').sum()
        * dfd['C_CO2_FACTOR_rwpre'] / 1000.0
    )

    # Natural gas
    z[14] = (
        dfd['EM_INDY'].loc[14:18].groupby('MNUMCR').sum()
        * dfd['C_CO2_FACTOR_rwpre'] / 1000.0
    )

    # Coal
    z[15] = (
        dfd['EM_INDY'].loc[11:13].groupby('MNUMCR').sum()
        * dfd['C_CO2_FACTOR_rwpre'] / 1000.0
    )

    # Other (natural gas processing)
    z[16] = dfd['EM_INDY'].loc[19] * dfd['C_CO2_FACTOR_rwpre'] / 1000.0

    # Direct industrial
    z[17] = z[13] + z[14] + z[15] + z[16]

    # Electricity (indirect emissions)
    z[18] = dfd['EM_INDY'].loc[20] * dfd['C_CO2_FACTOR_rwpre'] / 1000.0

    # Total industrial
    z[19] = z[17] + z[18]


    # Transportation---------
    # Petroleum
    z[20] = (
        (
            dfd['EM_TRAN'].loc[1:9].groupby('MNUMCR').sum()
            + dfd['EM_TRAN'].loc[11]  # Methanol
        ) * dfd['C_CO2_FACTOR_rwpre'] / 1000.0
    )

    # Natural gas
    z[21] = dfd['EM_TRAN'].loc[10] * dfd['C_CO2_FACTOR_rwpre'] / 1000.0

    # Direct transportation
    z[22] = z[20] + z[21]

    # Electricity (indirect emissions)
    z[23] = dfd['EM_TRAN'].loc[12] * dfd['C_CO2_FACTOR_rwpre'] / 1000.0

    # Total transportation
    z[24] = z[22] + z[23]


    # Electric power---------
    # Petroleum
    z[25] = (
        dfd['EM_ELEC'].loc[1:3].groupby('MNUMCR').sum()
        * dfd['C_CO2_FACTOR_rwpre'] / 1000.0
    )

    # Natural gas
    z[26] = dfd['EM_ELEC'].loc[5] * dfd['C_CO2_FACTOR_rwpre'] / 1000.0

    # Coal
    z[27] = dfd['EM_ELEC'].loc[4] * dfd['C_CO2_FACTOR_rwpre'] / 1000.0

    # Other (nonbiogenic MSW, geothermal, BECCS)
    z[28] = (
        dfd['EM_ELEC'].loc[6:8].groupby('MNUMCR').sum()
        * dfd['C_CO2_FACTOR_rwpre'] / 1000.0
    )

    # Total electric power
    z[29] = z[25] + z[26] + z[27] + z[28]


    # Total by fuel---------
    # Petroleum
    z[30] = z[1] + z[7] + z[13] + z[20] + z[25]

    # Natural gas
    z[31] = z[2] + z[8] + z[14] + z[21] + z[26]

    # Coal
    z[32] = z[3] + z[9] + z[15] + z[27]

    # Other
    z[33] = z[16] + z[28]

    # Total
    z[34] = z[30] + z[31] + z[32] + z[33]


    # Carbon dioxide emissions---------
    # (emissions per capita)
    z[35] = z[34] / dfd['MC_NP']


    # Carbon capture and storage by sector and fuel---------
    # Industrial petroleum
    z[36] = (
        dfd['CC_INDY'].loc[1:4].groupby('MNUMCR').sum()
        * dfd['C_CO2_FACTOR_rwpre'] / 1000.0
    )

    # Industrial natural gas
    z[37] = (
        dfd['CC_INDY'].loc[7:9].groupby('MNUMCR').sum()
        * dfd['C_CO2_FACTOR_rwpre'] / 1000.0
    )

    # Industrial coal
    z[38] = (
        dfd['CC_INDY'].loc[5:6].groupby('MNUMCR').sum()
        * dfd['C_CO2_FACTOR_rwpre'] / 1000.0
    )

    # Industrial other (natural gas processing)
    z[39] = dfd['CC_INDY'].loc[10] * dfd['C_CO2_FACTOR_rwpre'] / 1000.0

    # Electric power petroleum
    z[40] = (
        dfd['CC_ELEC'].loc[1:2].groupby('MNUMCR').sum()
        * dfd['C_CO2_FACTOR_rwpre'] / 1000.0
    )

    # Electric power natural gas
    z[41] = dfd['CC_ELEC'].loc[4] * dfd['C_CO2_FACTOR_rwpre'] / 1000.0

    # Electric power coal
    z[42] = dfd['CC_ELEC'].loc[3] * dfd['C_CO2_FACTOR_rwpre'] / 1000.0

    # Electric power other (BECCS)
    z[43] = dfd['CC_ELEC'].loc[5] * dfd['C_CO2_FACTOR_rwpre'] / 1000.0

    # Total carbon capture and storage
    z[44] = (
        z[36] + z[37] + z[38] + z[39]  # Industrial
        + z[40] + z[41] + z[42] + z[43]  # Electric power
    )


    return z
