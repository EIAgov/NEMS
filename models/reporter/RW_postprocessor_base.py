# -*- coding: utf-8 -*-
"""
Created on Wen Jan 18 08:46:11 2024

@author: SZO

Last updated on 7/16/2024

"""

import tomllib
import numpy as np
import pandas as pd
from RW_preprocessor import (
    parse_parameter_file,
    read_stuff,
    get_table_format_csv,
    get_table_format,
    normalize_format,
    insert_citation,
)


# @log_execution(logging.getLogger(__name__), 'make_SMK_fixed')
def postprocessor_base(table_spec, d_base, dfd):
    """Fix individual base tables for any reasons, such as cross-reference and SME
    operations which can be performed only some table operations have conducted or
    referenced tables have been already generated.

    Parameters
    ----------
    d_base : dict
        key: table_id
        value: df with indexes of row and region, and column as 2019-2050:
                       2019, 2020, ......2050
            row region
    table_spec : dict

    Returns
    -------
    d_base : dict
        Dictionary containing the fixed base tables.

    Note
    ----
        For cross-reference, we create a placeholder in the base table, and then
        populate the placer holder here. In development, the try-except block may be
        needed to handle situations when the cross-referenced table is not available.
    """

    # TN 004 -------------------
    # EIA only fills the table from RECYEAR forward (2019-2020)
    # So just need to zero out everything before 2020
    # try:
    d_base["TN 004"].iloc[:, 0:30] = 0
    # except:
    #     pass # tables are referenced but not selected

    # TN 030
    # EIA only fills the table from RECYEAR forward (2019-2020)
    # So just need to zero out everything before 2020
    # try:
    d_base["TN 030"].iloc[:, 0:30] = 0
    # except:
    #     pass # tables are referenced but not selected

    # TN 018 --------------------
    # Energy Intensity
    # (thousand Btu per m__m dollar of GDP)
    #     Delivered Energy
    # T18(12,IY,IS)=T2(76,MNUMCR,IY,IS)/MC_GDPR(IY)*1000.
    df12 = d_base["TN 018"].loc[12].squeeze() * d_base["TN 002"].loc[76].loc[11]
    df13 = d_base["TN 018"].loc[13].squeeze() * d_base["TN 002"].loc[106].loc[11]

    #     Total Energy
    # T18(13,IY,IS)=T2(106,MNUMCR,IY,IS)/MC_GDPR(IY)*1000.
    d_base["TN 018"].loc[(12, 0)] = df12.astype("float32")
    d_base["TN 018"].loc[(13, 0)] = df13.astype("float32")

    # TN 020 --------------------
    with open(table_spec["table_var_def_path"], "rb") as f:
        temp = tomllib.load(f)
    my_vars = temp["TN 020"]["v"]

    #     Petroleum Product Imports
    # T20(33,IY,IS)=T1(10,IY,IS)/(((BIODIMP(11,IY)+ETHIMP(11,IY)+RENEWDIMP(11,IY))/1000.+FSUM(T11(9,IY,IS),3))*365./1000.)
    df = d_base["TN 001"].loc[10] / (
        (dfd["BIODIMP"].loc[11] + dfd["ETHIMP"].loc[11] + dfd["RENEWDIMP"].loc[11])
        / 1000
        + (
            d_base["TN 011"].loc[9]
            + d_base["TN 011"].loc[10]
            + d_base["TN 011"].loc[11]
        )
        * 365
        / 1000
    )

    d_base["TN 020"].loc[(33, 0)] = df.squeeze().astype("float32")

    #     Total Petroleum Consumption
    # T20(38,IY,IS)=T1(19,IY,IS)/(T11(48,IY,IS)*365./1000.)
    df = d_base["TN 001"].loc[19] / (d_base["TN 011"].loc[48] * 365 / 1000)
    d_base["TN 020"].loc[(38, 0)] = df.squeeze().astype("float32")

    # TN 044 --------------------
    #   Nonitemized and Benchmarking Adjustments
    # if(ir.eq.11) T44(58,IY,IS) = 1000*T2(39,11,IY,IS) ! copy industrial delivered energy total for use in Table 44 balancing item
    d_base["TN 044"].loc[(58, 0)] = 1000 * d_base["TN 002"].loc[
        (39, 11)
    ].squeeze().astype("float32")

    # Total Industrial Consumption
    # T44(57,IY,IS)=T44(58,IY,IS)-T44(30,IY,IS)
    d_base["TN 044"].loc[(57, 0)] = (
        d_base["TN 044"].loc[(58, 0)] - d_base["TN 044"].loc[(30, 0)]
    )

    # TN 069 --------------------
    # Emissions factors for biomass combustion (previously hard-coded) in units
    # of million metric tons CO2 per quad
    # TODO: Completely remove these factors from the Reporter. Maybe add them
    #       to the EMEBLK include file and the EPMDATA input file.
    T69_EMFAC = {
        "woody biomass": 93.81,
        "biogenic waste": 90.64,
        "biofuels heat and coproducts": 93.81,
        "ethanol": 68.42,
        "biodiesel": 72.73,
        "renewable diesel and gasoline": 73.15,
        "biobutanol": 19.25 * 44.0 / 12.0,
        "liquids from biomass": 73.15,
    }
    # Index for last census division (national total)
    MNUMCR = int(dfd["MNUMCR_rwpre"])
    # Residential woody biomass combustion
    d_base["TN 069"].loc[16] = (
        d_base["TN 024"].loc[1].values * T69_EMFAC["woody biomass"]
    )
    # Commercial woody biomass combustion
    d_base["TN 069"].loc[17] = (
        d_base["TN 024"].loc[2].values * T69_EMFAC["woody biomass"]
    )
    # Industrial woody biomass combustion
    d_base["TN 069"].loc[18] = (
        d_base["TN 024"].loc[6].values * T69_EMFAC["woody biomass"]
    )
    # Industrial biogenic waste combustion
    d_base["TN 069"].loc[19] = (
        d_base["TN 024"].loc[5].values * T69_EMFAC["biogenic waste"]
    )
    # Industrial ethanol combustion (commented out in layin.csv)
    # TODO: Is this included in biofuels heat and coproducts?
    d_base["TN 069"].loc[20] = d_base["TN 069"].loc[20].values * 0.0
    # Industrial biofuels heat and coproducts combustion
    d_base["TN 069"].loc[21] = (
        d_base["TN 024"].loc[35].values
        * T69_EMFAC["biofuels heat and coproducts"]
    )
    # Transportation ethanol combustion
    d_base["TN 069"].loc[22] = (
        (
            d_base["TN 024"].loc[8].values + d_base["TN 024"].loc[9].values
        ) * T69_EMFAC["ethanol"]
    )
    # Transportation biodiesel combustion
    d_base["TN 069"].loc[23] = (
        d_base["TN 024"].loc[34].values * T69_EMFAC["biodiesel"]
    )
    # Transportation renewable diesel and gasoline combustion
    d_base["TN 069"].loc[24] = (
        d_base["TN 024"].loc[44].values
        * T69_EMFAC["renewable diesel and gasoline"]
    )
    # Transportation other biofuels combustion
    d_base["TN 069"].loc[26] = (
        d_base["TN 024"].loc[45].values * T69_EMFAC["biobutanol"]
        + d_base["TN 024"].loc[43].values * T69_EMFAC["liquids from biomass"]
    )
    # Electric power woody biomass combustion
    d_base["TN 069"].loc[27] = (
        d_base["TN 024"].loc[14].values * T69_EMFAC["woody biomass"]
    )
    # Electric power biogenic waste combustion
    d_base["TN 069"].loc[28] = (
        d_base["TN 024"].loc[13].values * T69_EMFAC["biogenic waste"]
    )
    # Total biomass combustion
    d_base["TN 069"].loc[29] = (
        # Residential
        d_base["TN 069"].loc[16].values
        # Commercial
        + d_base["TN 069"].loc[17].values
        # Industrial
        + d_base["TN 069"].loc[18].values
        + d_base["TN 069"].loc[19].values
        + d_base["TN 069"].loc[20].values
        + d_base["TN 069"].loc[21].values
        # Transportation
        + d_base["TN 069"].loc[22].values
        + d_base["TN 069"].loc[23].values
        + d_base["TN 069"].loc[24].values
        + d_base["TN 069"].loc[25].values
        + d_base["TN 069"].loc[26].values
        # Electric power
        + d_base["TN 069"].loc[27].values
        + d_base["TN 069"].loc[28].values
    )
    # Sequestered biomass combustion
    d_base["TN 069"].loc[30] = (
        # Gross biomass combustion total (assumed net zero)
        d_base["TN 069"].loc[29].values
        # Bioenergy carbon capture and storage (BECCS)
        - dfd["EM_ELEC"].loc[(8, MNUMCR), :].values
        * dfd["C_CO2_FACTOR_rwpre"] / 1000.0
    )
    # Net biomass combustion
    d_base["TN 069"].loc[31] = (
        d_base["TN 069"].loc[29].values - d_base["TN 069"].loc[30].values
    )
    # Net biomass combustion (total energy-related CO2 emissions)
    d_base["TN 069"].loc[40] = d_base["TN 069"].loc[31].values.copy()
    # Total energy-related CO2 emissions
    d_base["TN 069"].loc[43] = (
        d_base["TN 069"].loc[39].values  # Fossil fuel combustion
        + d_base["TN 069"].loc[40].values  # Net biomass combustion
        + d_base["TN 069"].loc[41].values  # Vented
        + d_base["TN 069"].loc[42].values  # Fuel-dependent process
    )

    # TN 107 --------------------
    #   Discrepancy from Table 1
    # T107(141,IY,IS)=T1(18,IY,IS)
    # z[141] = dfd['TN 001'].loc[18]  # Cross reference
    d_base["TN 107"].loc[141] = d_base["TN 001"].loc[18].values

    #   Liquids Discrepancy from Table 11
    # T107(142,IY,IS)=T11(50,IY,IS)
    # z[142] = dfd['TN 011'].loc[50]
    d_base["TN 107"].loc[142] = d_base["TN 011"].loc[50].values

    #   Natural Gas Discrepancy from Table 13
    # T107(143,IY,IS)=T13(16,IY,IS)
    # z[143] = dfd['TN 013'].loc[16]  # Cross reference
    d_base["TN 107"].loc[143] = d_base["TN 013"].loc[16].values

    #   Coal Discrepancy from Table 15
    # T107(144,IY,IS)=T15(16,IY,IS)
    # z[144] = dfd['TN 015'].loc[16]  # Cross reference
    d_base["TN 107"].loc[144] = d_base["TN 015"].loc[16].values

    # TN 124 --------------------
    #     Electricity Related Losses
    # T124(10,IY,IS)=T4(57,IY,IS)
    d_base["TN 124"].loc[10] = d_base["TN 004"].loc[57].values

    #       Total
    # T124(11,IY,IS)=FSUM(T124(9,IY,IS),2)
    d_base["TN 124"].loc[11] = (
        d_base["TN 124"].loc[9].values + d_base["TN 124"].loc[10].values
    )

    #   Delivered Energy Intensity
    #   (million Btu per household)
    # T124(12,IY,IS)=T4(6,IY,IS)
    d_base["TN 124"].loc[12] = d_base["TN 004"].loc[6].values.astype("float32")

    #     Electricity Related Losses
    # T124(25,IY,IS)=T5(42,IY,IS)
    d_base["TN 124"].loc[25] = d_base["TN 005"].loc[42].values

    #       Total
    # T124(26,IY,IS)=FSUM(T124(24,IY,IS),2)
    d_base["TN 124"].loc[26] = (
        d_base["TN 124"].loc[24].values + d_base["TN 124"].loc[25].values
    )

    #   Delivered Energy Intensity
    #    (thousand Btu per square foot)
    # T124(27,IY,IS)=T5(4,IY,IS)
    d_base["TN 124"].loc[27] = d_base["TN 005"].loc[4].values

    #     Combined Heat and Power/Other
    # T138(25,IY,IS)=T9(45,IY,IS)
    d_base["TN 124"].loc[25] = d_base["TN 009"].loc[45].values

    # TN 138 -----------------------
    #     Combined Heat and Power/Other
    # T138(12,IY,IS)=T9(44,IY,IS)
    # Create a place holder
    d_base["TN 138"].loc[12] = d_base["TN 009"].loc[44].values

    #       Total
    # T138(13,IY,IS)=FSUM(T138(1,IY,IS),12)
    d_base["TN 138"].loc[13] = (
        d_base["TN 138"].loc[13].values + d_base["TN 138"].loc[12].values
    )

    #     Combined Heat and Power/Other
    # T138(25,IY,IS)=T9(45,IY,IS)
    d_base["TN 138"].loc[25] = d_base["TN 009"].loc[45].values

    # #     Combined Heat and Power/Other
    # #T138(34,IY,IS)=T8(15,IY,IS)
    d_base["TN 138"].loc[34] = d_base["TN 008"].loc[15].values

    # #       Total
    # #T138(35,IY,IS)=FSUM(T138(28,IY,IS),7)
    d_base["TN 138"].loc[35] = (
        d_base["TN 138"].loc[35].values + d_base["TN 138"].loc[34].values
    )

    #'''lay has incorrect cross-references below ! '''
    #   Carbon Dioxide Emissions
    #   (million metric tons carbon_dd_ equivalent)
    #     Coal
    # T138(44,IY,IS)=T17(23,11,IY,IS)

    # d_base['TN 138'].loc[44] = d_base['TN 017'].loc[23].loc[11].values

    # #     Petroleum
    # # #T138(42,IY,IS)=T17(21,11,IY,IS)
    # d_base['TN 138'].loc[42] = d_base['TN 017'].loc[21].loc[11].values

    # #     Natural Gas
    # # #T138(43,IY,IS)=T17(22,11,IY,IS)
    # d_base['TN 138'].loc[43] = d_base['TN 017'].loc[22].loc[11].values

    # #     Other
    # # #T138(45,IY,IS)=T17(24,11,IY,IS)
    # d_base['TN 138'].loc[45] = d_base['TN 017'].loc[24].loc[11].values

    # #       Total
    # # #T138(46,IY,IS)=FSUM(T138(42,IY,IS),4)
    # # Create a place holder
    # d_base['TN 138'].loc[46] = d_base['TN 138'].loc[42] \
    #                         + d_base['TN 138'].loc[43] \
    #                         + d_base['TN 138'].loc[44] \
    #                         + d_base['TN 138'].loc[45]

    # '''Correct cross-reference to TN 017 !'''
    d_base["TN 138"].loc[44] = d_base["TN 017"].loc[27].loc[11].values

    #     Petroleum
    # #T138(42,IY,IS)=T17(21,11,IY,IS)
    d_base["TN 138"].loc[42] = d_base["TN 017"].loc[25].loc[11].values

    #     Natural Gas
    # #T138(43,IY,IS)=T17(22,11,IY,IS)
    d_base["TN 138"].loc[43] = d_base["TN 017"].loc[26].loc[11].values

    #     Other
    # #T138(45,IY,IS)=T17(24,11,IY,IS)
    d_base["TN 138"].loc[45] = d_base["TN 017"].loc[28].loc[11].values

    #       Total
    # #T138(46,IY,IS)=FSUM(T138(42,IY,IS),4)
    d_base["TN 138"].loc[46] = d_base["TN 017"].loc[29].loc[11].values

    return d_base
