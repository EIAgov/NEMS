# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: SZO
"""
import pandas as pd


def fill_table_base_002(dfd, table_spec, table_id):
    """Fill table for Energy Consumption by Sector and Source.

    The function returns a dict in which each key is an integer references a table row, and each
    value is a dataframe indexed by region number. The integer keys are the same as "IROWS" in layin

    Parameters
    ----------
    dfd : dict of restart variables
        key = variable name
        value = pandas series of variable values

    Returns
    -------
    dict
        dict of dataframes, with integers as keys.
        The dict values are dataframes indexed by region number.

    """

    z = {}
    RDAYS = dfd["RDAYS_rwpre"]
    MNUMCR = dfd["MNUMCR_rwpre"]
    MNUMPR = dfd["MNUMPR_rwpre"]
    CFUBA = dfd["CFUBA_rwpre"]
    IEL_R = 6
    IEL_C = 8
    IEL_I = 20
    IEL_T = 12
    tril_to_quad = dfd["TRIL_TO_QUAD_rwpre"]
    Electrolizer_Efficiency = (1-0.6017) # This variable was not defined for AEO2025, defined
    # and add to restart file in AEO2026

    # Electric Power: Hydrogen Related Losses
    z[132] = (Electrolizer_Efficiency * dfd["QH2EL"]) * tril_to_quad

    # factor for electricity-related losses. used in multiple calculations
    # dfd["ELECLOSS"] = dfd["QTSEL"] - dfd["QELAS"] + z[132]
    dfd["ELECLOSS"] = dfd["QTSEL"] - dfd["QELAS"] + (Electrolizer_Efficiency * dfd["QH2EL"])

    # residential consumption of various fuels
    res = {
        1: "QDSRS",  # Distillate Fuel Oil 1/
        3: "QLGRS",  # Propane
        4: "QTPRS",  # Petroleum and Other Liquids Subtotal
        5: "QNGRS",  # Natural Gas
        6: "QCLRS",  # Coal
        7: "QBMRS",  # Renewable Energy 2/
        8: "QELRS",  # Purchased Electricity
    }
    # Looping through the above res codes
    for key, var in res.items():
        z[key] = dfd[var] * tril_to_quad

    # QELRS_EV(IR,IY) = TRQ_ELEC(1,IR,IY)/1000.0
    QELRS_EV = dfd["TRQ_ELEC"].loc[1]
    # remove EV from purchased electricity for residential use
    z[8] -= QELRS_EV * tril_to_quad

    # Residential Delivered Energy
    z[9] = (
        dfd["QTSRS"] - dfd["QSTRS"] - dfd["QGERS"] - dfd["QPVRS"] - QELRS_EV
    ) * tril_to_quad
    z[10] = (dfd["QELRS"] - QELRS_EV) / dfd["QELAS"] * dfd["ELECLOSS"] * tril_to_quad # Residential electricty-related losses
    z[11] = z[9] + z[10] # Residential total- Residential Delivered Energy + Residential electricity-related losses

    # COMMERCIAL
    com = {
        15: "QLGCM",  # Propane
        16: "QMGCM",  # Motor Gasoline 3/
        14: "QKSCM",  # Kerosene
        12: "QDSCM",  # Distillate Fuel Oil
        13: "QRSCM",  # Residual Fuel Oil
        17: "QTPCM",  # Petroleum and Other Liquids Subtotal
        18: "QNGCM",  # Natural Gas
        19: "QCLCM",  # Coal
    }
    for key, var in com.items():
        z[key] = dfd[var] * tril_to_quad

    # Commerical: Renewable Energy 4/
    z[20] = (dfd["QTRCM"] - dfd["QSTCM"] - dfd["QPVCM"]) * tril_to_quad

    # QELCM_EV of electric vehicle
    QELCM_EV = 0
    for fuel in range(2, 10):
        QELCM_EV += dfd["TRQ_ELEC"].loc[fuel]
    dfd["QELCM_EV"] = QELCM_EV.copy()

    z[21] = (dfd["QELCM"] - QELCM_EV) * tril_to_quad # Commerical: Purchased Electricity
    z[22] = ((dfd["QTSCM"] - dfd["QSTCM"] - dfd["QPVCM"]) - QELCM_EV) * tril_to_quad # Commercial: Delivered Energy
    z[23] = ((dfd["QELCM"] - QELCM_EV) / dfd["QELAS"]) * dfd["ELECLOSS"] * tril_to_quad # Commercial: Electricity Related Losses
     # Commercial: Total- Commercial: Delivered Energy + Commercial: Electricity Related Losses
    z[24] = z[22] + z[23]


    # INDUSTRIAL 5/
    # Industrial flags
    ind = {
        26: "QLGIN",  # Hydrocarbon Gas Liquids 6/
        29: "QMGIN",  # Motor Gasoline 3/
        25: "QDSIN",  # Distillate Fuel Oil
        28: "QRSIN",  # Residual Fuel Oil
        27: "QPFIN",  # Petrochemical Feedstocks    11T2(27,IR,IY,IS)=QPFIN(IR,IY)
    }
    # Looping through above keys
    for key, val in ind.items():
        z[key] = dfd[val] * tril_to_quad

    # Industrial: Other Petroleum 7/
    z[30] = (
        dfd["QOTIN"] + dfd["QASIN"] + dfd["QPCIN"] + dfd["QKSIN"] + dfd["QSGIN"]
    ) * tril_to_quad
    z[31] = dfd["QTPIN"] * tril_to_quad # Industrial: Petroleum and Other Liquids Subtotal
    z[115] = (dfd["QNGIN"] - dfd["QNGHM"]) * tril_to_quad # Industrial: Natural Gas
    z[116] = dfd["QLPIN"] * tril_to_quad # Industrial: Lease and Plant Fuel 8/
    z[32] = (dfd["QNGIN"] + dfd["QLPIN"] - dfd["QNGHM"]) * tril_to_quad # Industrial: Natural Gas Subtotal
    z[33] = dfd["QMCIN"] * tril_to_quad # Industrial: Metallurgical Coal
    z[34] = dfd["QCLIN"] * tril_to_quad # Industrial: Other Industrial Coal
    z[35] = dfd["QCIIN"] * tril_to_quad # Industrial: Net Coal Coke Imports
    z[36] = z[33] + z[34] + z[35] # Industrial: Coal Subtotal

    # Industrial: Biofuels Heat and Coproducts
    z[73] = (
        (
            (dfd["QMGTR"] - dfd["QMGBS"])
            / (dfd["QMGTR"].loc[MNUMCR] - dfd["QMGBS"].loc[MNUMCR])
        ).fillna(0)
        * (
            dfd["CORNCD"].loc[1].loc[MNUMCR] * dfd["CFCORN"].iloc[0] / 1000000000.0
            - dfd["ETHANOL_PARAM_rwpre"]
            * (dfd["CRNETHCD"].loc[MNUMCR] + dfd["OTHETHCD"].loc[MNUMCR])
            * RDAYS
            * dfd["CFPET"].iloc[0]
            / 1000000.0
        )
        + (
            (dfd["QDSTR"] - dfd["QDSBS"])
            / (dfd["QDSTR"].loc[MNUMCR] - dfd["QDSBS"].loc[MNUMCR])
        ).fillna(0)
        * dfd["BIMQTYCD"].loc[1:4, MNUMCR, :].sum()
        / 1000000
        * RDAYS
        * (dfd["CFVEGGIE"].values - dfd["CFBIOD"].values)
        + (
            (
                - RDAYS
                / 1000000.0
                * (
                    dfd["BTLFRAC"].loc[1:4, MNUMPR, :].sum() * dfd["CFBTLLIQ"]
                    + dfd["CBTLFRAC"].loc[2, 1:4, MNUMPR, :].sum()
                    * dfd["CFCBTLLIQ"].loc[2]
                    + dfd["UBAVOL"].loc[MNUMPR] * CFUBA
                )
            )
            + (
                dfd["CORNCD"].loc[2].loc[MNUMCR] * dfd["CFCORN"].iloc[0] / 1000.0
                - dfd["RFBIOBUTECD"].loc[MNUMCR] * RDAYS * dfd["CFBIOBUTE"]
            )
            / 1000000
        ).fillna(0)
        + (
            (
                (dfd["QMGTR"] - dfd["QMGBS"])
                / (dfd["QMGTR"].loc[MNUMCR] - dfd["QMGBS"].loc[MNUMCR])
            )
            * (
                dfd["ETHANOL_PARAM_rwpre"]
                * dfd["CLLETHCD"].loc[MNUMCR]
                * RDAYS
                * (
                    dfd["CFBMQ"] * dfd["GAL_PER_BBL_rwpre"] / dfd["CONEFF"]
                    - dfd["CFPET"].iloc[0]
                )
            )
            / 1000000.0
        ).fillna(0)
    )

    
    z[37] = (dfd["QTRIN"] - dfd["QBMRF"]) * tril_to_quad # Industrial: Renewable Energy 10/
    z[2] = dfd["QH2IN"] * tril_to_quad # Industrial: Hydrogen
    z[38] = (dfd["QELIN"] - dfd["QELHM"]) * tril_to_quad # Industiral: Purchased Electricity
    z[39] = z[31] + z[32] + z[36] + z[37] + z[2] + z[38] + z[73] # Add various industrial subtotals
    z[40] = (((dfd["QELIN"] / dfd["QELAS"]) - (dfd["QELHM"] / dfd["QELAS"])) * dfd["ELECLOSS"]) * tril_to_quad # Industrial: electricity-related losses


    # Transportation
    z[46] = dfd["QLGTR"] * tril_to_quad # Transportation: Propane
    # Transportation: Motor Gasoline 3/
    z[44] = (dfd["QMGTR"] - dfd["QMGBS"] + dfd["QETTR"] + dfd["QMETR"]) * dfd[
        "TRIL_TO_QUAD_rwpre"
    ]
    z[51] = dfd["QETTR"] * tril_to_quad # Transportation: Motor Gasoline of which:  E85 11/
    z[43] = (dfd["QJFTR"] - dfd["QJFBS"]) * tril_to_quad  # Transportation: Jet Fuel 12/
    z[42] = (dfd["QDSTR"] - dfd["QDSBS"]) * tril_to_quad # Transportation: Distillate Fuel Oil 13/
    z[45] = dfd["QRSTR"] * tril_to_quad # Transportation: Residual Fuel Oil
    z[47] = dfd["QOTTR"] * tril_to_quad # Transportation: Other Petroleum 14/
    # Transportation: Petroleum and Other Liquids Subtotal
    z[48] = (
        dfd["QTPTR"]
        + dfd["QETTR"]
        + dfd["QMETR"]
        - dfd["QDSBS"]
        - dfd["QJFBS"]
        - dfd["QMGBS"]
    ) * tril_to_quad

    z[49] = dfd["QGPTR"] * tril_to_quad # Transportation: Pipeline and Distribution Fuel Natural Gas
    z[128] = dfd["QNGLQ"] * tril_to_quad # Transportation: Natural Gas to Liquefy Gas for Export 9/
    z[50] = dfd["QNGTR"] * tril_to_quad # Transportation: Compressed / Liquefied Natural Gas
    z[53] = dfd["QH2TR"] * tril_to_quad # Transportation: Hydrogen
    z[54] = (dfd["QELTR"] + QELRS_EV + QELCM_EV) * tril_to_quad # Transportation: Purchased Electricity
    z[129] = QELRS_EV * tril_to_quad # Transportation: Electric Vehicle Charging (Residential)
    z[130] = QELCM_EV * tril_to_quad # Transportation: Electric Vehicle Charging (Commercial)
    z[131] = dfd["QELTR"] * tril_to_quad # Transportation: Passenger Rail

    # Transportation: Delivered Energy: 
    # Summation of: Petroleum and Other Liquids Subtotal + Pipeline and Distribution Fuel Natural Gas +
    # Compressed / Liquefied Natural Gas + Hydrogen + Purchased Electricity + Variable QNGLQ (Natural gas used for liquefaction)
    z[55] = z[48] + z[49] + z[50] + z[53] + z[54] + dfd["QNGLQ"] * tril_to_quad

    # Transportation: Electricity Related Losses
    z[56] = (
        (dfd["QELTR"] + QELCM_EV + QELRS_EV)
        / dfd["QELAS"]
        * dfd["ELECLOSS"]
        * tril_to_quad
    )

    # Unspecified Sector 15/
    z[124] = dfd["QMGBS"] * tril_to_quad # Unspecified Sector: Motor Gasoline 3
    z[125] = dfd["QJFBS"] * tril_to_quad # Unspecified Sector: Jet Fuel 12/
    z[126] = dfd["QDSBS"] * tril_to_quad # Unspecified Sector: Distillate Fuel Oil 13/
    # Unspecified Sector: Total - Motor Gasoline + Jet Fuel + Distillate Fuel Oil
    z[127] = z[124] + z[125] + z[126]

    # Delivered Energy Consumption, All Sectors
    z[61] = dfd["QLGAS"] * tril_to_quad # Delivered Energy Consumption, All Sectors: Liquefied Petroleum Gases and Other 6/
    z[62] = (dfd["QMGAS"] + dfd["QETTR"] + dfd["QMETR"]) * tril_to_quad # Delivered Energy Consumption, All Sectors: Motor Gasoline 3/
    z[51] = dfd["QETTR"] * tril_to_quad # Delivered Energy Consumption, All Sectors: Motor Gasoline 3/ of which:  E85 11/
    z[60] = dfd["QJFTR"] * tril_to_quad # Delivered Energy Consumption, All Sectors: Jet Fuel 12/
    z[59] = dfd["QKSAS"] * tril_to_quad # Delivered Energy Consumption, All Sectors: Kerosene 16/
    z[58] = (dfd["QDSAS"] - dfd["QDSEL"]) * tril_to_quad # Delivered Energy Consumption, All Sectors: Distillate Fuel Oil 1/
    z[64] = (dfd["QRSAS"] - dfd["QRSEL"]) * tril_to_quad # Delivered Energy Consumption, All Sectors: Residual Fuel Oil
    z[63] = dfd["QPFIN"] * tril_to_quad # Delivered Energy Consumption, All Sectors: Petrochemical Feedstocks
    
    # Delivered Energy Consumption, All Sectors: Other Petroleum 17/
    z[65] = (dfd["QOTAS"] + dfd["QSGIN"] + dfd["QPCIN"] + dfd["QASIN"]) * dfd[
        "TRIL_TO_QUAD_rwpre"
    ]
    
    # Delivered Energy Consumption, All Sectors: Petroleum and Other Liquids Subtotal
    # Distillate Fuel Oil + Kerosene + Jet Fuel + Liquefied Petroleum Gases and Others + 
    # Motor Gasoline + Petrochemical Feedstocks 6 Residual Fuel Oil + Other Petroleum
    z[66] = z[58] + z[59] + z[60] + z[61] + z[62] + z[63] + z[64] + z[65]
    
    z[117] = (dfd["QNGAS"] - dfd["QNGEL"] - dfd["QNGHM"]) * dfd["TRIL_TO_QUAD_rwpre"] # Delivered Energy Consumption, All Sectors: Natural Gas
    z[116] = dfd["QLPIN"] * tril_to_quad # Delivered Energy Consumption, All Sectors: Lease and Plant Fuel 8/
    z[128] = dfd["QNGLQ"] * tril_to_quad # Delivered Energy Consumption, All Sectors: Natural Gas to Liquefy Gas for Export 9/
    z[49] = dfd["QGPTR"] * tril_to_quad # Delivered Energy Consumption, All Sectors: Pipeline and Distribution Fuel Natural Gas
    
    # Delivered Energy Consumption, All Sectors: Natural Gas Subtotal
    # Natural Gas + Natural Gas to Liquefy Gas for Export + Lease and Plant Fuel +
    # Pipeline and Distribution Fuel Natural Gas
    z[67] = z[117] + z[128] + z[116] + z[49]
    
    z[68] = dfd["QMCIN"] * tril_to_quad # Delivered Energy Consumption, All Sectors: Metallurgical Coal
    z[69] = (dfd["QCLAS"] - dfd["QCLEL"]) * tril_to_quad # Delivered Energy Consumption, All Sectors: Other Coal
    z[70] = dfd["QCIIN"] * tril_to_quad # Delivered Energy Consumption, All Sectors: Net Coal Coke Imports
    
    # Delivered Energy Consumption, All Sectors: Coal Subtotal
    # Metallurgical Coal + Other Coal + Net Coal Coke Imports
    z[71] = z[68] + z[69] + z[70]

    # Delivered Energy Consumption, All Sectors: Renewable Energy 18/
    z[72] = (
        dfd["QBMRS"] + dfd["QBMCM"] + dfd["QTRIN"] - dfd["QBMRF"] + dfd["QTRSN"]
    ) * tril_to_quad
    
    # Delivered Energy Consumption, All Sectors: Hydrogen
    # Industrial: Hydrogen + Transportation: Hydrogen
    z[75] = z[2] + z[53]

    z[74] = (dfd["QELAS"] - dfd["QELHM"]) * tril_to_quad # Delivered Energy Consumption, All Sectors: Purchased Electricity
    
    # Delivered Energy Consumption, All Sectors: Delivered Energy
    # Sum of the following lines:
    # Delivered Energy Consumption, All Sectors: Petroleum and Other Liquids Subtotal
    # + Delivered Energy Consumption, All Sectors: Natural Gas Subtotal + 
    # Delivered Energy Consumption, All Sectors: Coal Subtotal +
    # Delivered Energy Consumption, All Sectors: Renewable Energy 18/ +
    # Industrial: Biofuels Heat and Coproducts + 
    # Delivered Energy Consumption, All Sectors: Purchased Electricity +
    # Delivered Energy Consumption, All Sectors: Hydrogen
    z[76] = z[66] + z[67] + z[71] + z[72] + z[73] + z[74] + z[75]
    
    
    # Electric Power 19/
    z[79] = dfd["QDSEL"] * tril_to_quad # Distillate Fuel Oil
    z[80] = dfd["QRSEL"] * tril_to_quad # Residual Fuel Oil
    # Petroleum and Other Liquids Subtotal
    z[81] = z[79] + z[80]
    
    z[82] = dfd["QNGEL"] * tril_to_quad # Natural Gas
    z[83] = dfd["QCLEL"] * tril_to_quad # Steam Coal
    z[84] = dfd["QUREL"] * tril_to_quad # Nuclear / Uranium 20/
    z[85] = (dfd["QTREL"] + dfd["QPCEL"]) * tril_to_quad - dfd["WNCMSEL"] # Renewable Energy 21/
    z[120] = dfd["QH2EL"] # Electric Power: Hydrogen 
    z[123] = dfd["WNCMSEL"] # Non-biogenic Municipal Waste
    z[86] = dfd["QEIEL"] * tril_to_quad # Electricity Imports
    
    # Electric Power: Total Energy
    z[122] = dfd["QTSEL"] * tril_to_quad #dfd["VAR"] Table Rows 116-125

    # Electric Power: Total
    # Summation: Electric Power: Total Energy + Electric Power: Hydrogen Related Losses
    z[87] = z[122] + z[132]

    # Industrial Hydrogen Production 26/
    
    z[133] = (dfd["QNGHMPF"] + dfd["QNGHMHP"]) * tril_to_quad # Natural Gas
    z[134] = (dfd["QELHM"] * tril_to_quad) # Purchased Electricity
    z[138] = dfd["BYPRDH2IN"] * tril_to_quad # Byproduct Hydrogen
    
    # Industrial Hydrogen Production: Total Energy
    # Sum of: Natural Gas + Purchased Electricity + Byproduct Hydrogen
    z[135] = z[133] + z[134] + z[138]
    
    # Electricity Related Losses
    z[136] = (dfd["ELECLOSS"] * (dfd["QELHM"]/dfd["QELAS"]))* tril_to_quad
    
    # Industrial Hydrogen Production: Total
    # Sum of: Industrial Hydrogen Production: Total Energy + Electricity Related Losses
    z[137] = z[135] + z[136]
    
    # Subtract total hydrogen consumed by everyone for hydrogen related losses
    # Delivered Energy Consumption
    # Difference of: Industrial Hydrogen Production: Total - Electric Power: Hydrogen Related Losses
    # - Delivered Energy Consumption, All Sectors: Hydrogen - Industrial Hydrogen Production: Purchased Electricity
    z[119] = z[137] -z[132] -z[75] - z[134]
    # Industrial Hydrogen Losses
    # Delivered Energy Consumption * (Var) / Delivered Energy Consumption, All Sectors: Hydrogen
    z[52] = z[119] * (dfd["QH2IN"]*tril_to_quad) / z[75]
    # Transportation Hydrogen Losses
    # Delivered Energy Consumption * (Var) / Delivered Energy Consumption, All Sectors: Hydrogen
    z[105] = z[119] * (dfd["QH2TR"]*tril_to_quad) / z[75]
    # Electricity Loss - Electricity Related Losses
    z[77] = dfd["ELECLOSS"] * tril_to_quad - z[136]
    # Total: Transportation: Delivered Energy + Transportation: Electricity Related Losses +
    # Transportation Hydrogen Losses
    z[57] = z[55] + z[56] + z[105]
    # Total: Delivered Energy Consumption, All Sectors: Delivered Energy + Electricity Related Loss
    # + Delivered Energy Consumption
    z[78] = z[76] + z[77] + z[119]
    # industrial total: Industrial: delivered energy + Industrial: electricity-related losses +
    # Industrial Hydrogen Losses
    z[41] = z[39] + z[40] + z[52]
    
    # Total Energy Consumption
    z[91] = dfd["QLGAS"] * tril_to_quad # Liquefied Petroleum Gases and Other 6/
    z[92] = (dfd["QMGAS"] + dfd["QETTR"] + dfd["QMETR"]) * tril_to_quad # Motor Gasoline 3/
    z[51] = dfd["QETTR"] * tril_to_quad # of which:  E85 11/
    z[90] = dfd["QJFTR"] * tril_to_quad # Jet Fuel 12/
    z[89] = dfd["QKSAS"] * tril_to_quad # Kerosene 16/
    z[88] = dfd["QDSAS"] * tril_to_quad # Distillate Fuel Oil 1/
    z[94] = dfd["QRSAS"] * tril_to_quad # Residual Fuel Oil
    z[93] = dfd["QPFIN"] * tril_to_quad #  Petrochemical Feedstocks
    #   Other Petroleum 17/
    z[95] = (dfd["QOTAS"] + dfd["QSGIN"] + dfd["QPCIN"] + dfd["QASIN"]) * dfd[
        "TRIL_TO_QUAD_rwpre"
    ]
     #     Petroleum and Other Liquids Subtotal
    z[96] = z[88] + z[89] + z[90] + z[91] + z[92] + z[93] + z[94] + z[95]
    
    z[118] = dfd["QNGAS"] * dfd["TRIL_TO_QUAD_rwpre"] #   Natural Gas
    
    z[116] = dfd["QLPIN"] * tril_to_quad #   Lease and Plant Fuel 8/
    
    z[128] = dfd["QNGLQ"] * tril_to_quad #   Natural Gas to Liquefy Gas for Export 9/
    
    z[49] = dfd["QGPTR"] * tril_to_quad #   Pipeline and Distribution Fuel Natural Gas
    #     Natural Gas Subtotal
    z[97] = z[118] + z[128] + z[116] + z[49]
    
    z[98] = dfd["QMCIN"] * tril_to_quad #   Metallurgical Coal
    
    z[99] = dfd["QCLAS"] * tril_to_quad #   Other Coal
    
    z[100] = dfd["QCIIN"] * tril_to_quad #   Net Coal Coke Imports
    #     Coal Subtotal
    z[101] = z[98] + z[99] + z[100]
    
    z[102] = dfd["QUREL"] * tril_to_quad #   Nuclear / Uranium 20/
    #   Renewable Energy 22/
    z[103] = (
        dfd["QBMRS"]
        + dfd["QBMCM"]
        + dfd["QTRIN"]
        - dfd["QBMRF"]
        + dfd["QTRSN"]
        + dfd["QTREL"]
    ) * tril_to_quad - dfd["WNCMSEL"]
    
    z[104] = dfd["QEIEL"] * tril_to_quad #   Electricity Imports
    
    z[139] = z[138] #  Byproduct Hydrogen 
    #     Total
    z[106] = z[96] + z[97] + z[101] + z[102] + z[103] + z[104] + z[73] + z[123] + z[139]
    
    
    # Energy Use & Related Statistics

    z[121] = dfd["ETHTOTCD"] * RDAYS * dfd["CFETQ"].values / 1000000.0

    #  Population (millions)
    # Billion to Million !
    z[110] = dfd["MC_NP"]
    z1 = z[110].copy()
    z1.iloc[:] = 1
    #  Gross Domestic Product (billion m__m dollars)
    z[111] = z1 * dfd["MC_GDPR"].values
    #   Carbon Dioxide Emissions (million metric tons carbon_dd_)
    z[112] = (
        (
            dfd["EM_RESD"].loc[1:IEL_R].groupby("MNUMCR").sum()
            + dfd["EM_COMM"].loc[1:IEL_C].groupby("MNUMCR").sum()
            + dfd["EM_INDY"].loc[1:IEL_I].groupby("MNUMCR").sum()
            + dfd["EM_TRAN"].loc[1:IEL_T].groupby("MNUMCR").sum()
        )
        * dfd["C_CO2_FACTOR_rwpre"]
        / 1000.0
    )

    #  Bonus Rows - Internal Only
    #  Energy Intensity (thousand Btu per m__m dollars of GDP)
    z[113] = z[106] / (z[111] / 1000.0)
    #  Carbon Intensity (mt_cc_e per million m__m dollars of GDP)
    z[114] = z[112] / (z[111] / 1000.0)
    #  Non-Energy GHG Emissions
    z[107] = dfd["GHG_OGHG"].loc[1:14].sum() * dfd["C_CO2_FACTOR_rwpre"]
    z[107] = z1 * z[107]
    #  Greenhouse Gas Intensity
    z[108] = (z[112] + z[107]) / (z[111] / 1000.0)
    #  % Decline from 2002
    z[109] = z[108].copy()

    YR = 2002
    for col in range(1990, 2051):
        z[109][col] = 0 if col <= YR else -(z[108][col] - z[108][YR]) / z[108][YR] * 100

    return z
