# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM

SZO fixed TRQLDV TRANSP Lt Duty Vehicle Energy Use on 9/4/2024
"""


def fill_table_base_047(dfd, table_spec, table_id):
    """Fill table Light-Duty Vehicle Energy Consumption by Technology Type and Fuel Type

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
    Move the fix for TRQLDV to preprocessor_base later
    """

    z = {}

    MNUMCR = dfd["MNUMCR_rwpre"]

    #   Light-Duty Vehicle Energy Consumption by Technology Type and Fuel Type
    #   (trillion Btu)
    #    Technology Type
    #
    #   Light-Duty Consumption by Technology Type 1/
    #   Conventional Vehicles
    #     Gasoline ICE Vehicles
    # T47(1,IY,IS)=TRLDQTEK(1,IY)
    z[1] = dfd["TRLDQTEK"].loc[1]
    #     TDI Diesel ICE
    # T47(2,IY,IS)=TRLDQTEK(2,IY)
    z[2] = dfd["TRLDQTEK"].loc[2]
    #       Total Conventional
    # T47(18,IY,IS)=T47(1,IY,IS)+T47(2,IY,IS)
    z[18] = z[1] + z[2]
    #
    #   Alternative-Fuel Vehicles
    #     Ethanol-Flex Fuel ICE
    # T47(4,IY,IS)=TRLDQTEK(3,IY)
    z[4] = dfd["TRLDQTEK"].loc[3]

    #     100-Mile Electric Vehicle
    # T47(5,IY,IS)=TRLDQTEK(4,IY)
    z[5] = dfd["TRLDQTEK"].loc[4]

    #     200-Mile Electric Vehicle
    # T47(11,IY,IS)=TRLDQTEK(7,IY)
    z[11] = dfd["TRLDQTEK"].loc[7]

    #     300-Mile Electric Vehicle
    # T47(15,IY,IS)=TRLDQTEK(15,IY)
    z[15] = dfd["TRLDQTEK"].loc[15]

    #     Plug-in 20 Gasoline Hybrid
    # T47(12,IY,IS)=TRLDQTEK(5,IY)
    z[12] = dfd["TRLDQTEK"].loc[5]

    #     Plug-in 50 Gasoline Hybrid
    # T47(3,IY,IS)=TRLDQTEK(6,IY)
    z[3] = dfd["TRLDQTEK"].loc[6]

    #     Electric-Diesel Hybrid
    # T47(13,IY,IS)=TRLDQTEK(8,IY)
    z[13] = dfd["TRLDQTEK"].loc[8]

    #     Electric-Gasoline Hybrid
    # T47(14,IY,IS)=TRLDQTEK(16,IY)
    z[14] = dfd["TRLDQTEK"].loc[16]

    #     Natural Gas ICE
    # T47(7,IY,IS)=TRLDQTEK(11,IY)
    z[7] = dfd["TRLDQTEK"].loc[11]

    #     Natural Gas Bi-fuel
    # T47(8,IY,IS)=TRLDQTEK(9,IY)
    z[8] = dfd["TRLDQTEK"].loc[9]

    #     Propane ICE
    # T47(9,IY,IS)=TRLDQTEK(12,IY)
    z[9] = dfd["TRLDQTEK"].loc[12]

    #     Propane Bi-fuel
    # T47(10,IY,IS)=TRLDQTEK(10,IY)
    z[10] = dfd["TRLDQTEK"].loc[10]

    #     Fuel Cell Methanol
    # T47(16,IY,IS)=TRLDQTEK(13,IY)
    z[16] = dfd["TRLDQTEK"].loc[13]

    #     Fuel Cell Hydrogen
    # T47(17,IY,IS)=TRLDQTEK(14,IY)
    z[17] = dfd["TRLDQTEK"].loc[14]

    #       Total Alternative
    # T47(19,IY,IS)=FSUM(T47(3,IY,IS),15) #there's no T47(6)
    z[19] = (
        z[3]
        + z[4]
        + z[5]
        + z[7]
        + z[8]
        + z[9]
        + z[10]
        + z[11]
        + z[12]
        + z[13]
        + z[14]
        + z[15]
        + z[16]
        + z[17]
    )

    #
    #   Total
    # T47(20,IY,IS)=T47(18,IY,IS)+T47(19,IY,IS)
    z[20] = z[18] + z[19]

    #
    #   Light-Duty Consumption by Fuel Type
    #     Motor Gasoline
    # T47(21,IY,IS)=TRQLDV(1,11,IY)
    z[21] = dfd["TRQLDV"].loc[1].loc[MNUMCR]

    #     Distillate Fuel Oil (diesel)
    # T47(22,IY,IS)=TRQLDV(8,11,IY)
    z[22] = dfd["TRQLDV"].loc[8].loc[MNUMCR]

    #     M85
    # T47(23,IY,IS)=TRQLDV(2,11,IY)
    z[23] = dfd["TRQLDV"].loc[2].loc[MNUMCR]

    #     E85 2/
    # T47(24,IY,IS)=TRQLDV(3,11,IY)
    z[24] = dfd["TRQLDV"].loc[3].loc[MNUMCR]

    #     Natural Gas
    # T47(25,IY,IS)=TRQLDV(4,11,IY)
    z[25] = dfd["TRQLDV"].loc[4].loc[MNUMCR]

    #     Propane
    # T47(26,IY,IS)=TRQLDV(5,11,IY)
    z[26] = dfd["TRQLDV"].loc[5].loc[MNUMCR]

    #     Electricity
    # T47(27,IY,IS)=TRQLDV(6,11,IY)
    z[27] = dfd["TRQLDV"].loc[6].loc[MNUMCR]

    #     Hydrogen
    # T47(28,IY,IS)=TRQLDV(7,11,IY)
    z[28] = dfd["TRQLDV"].loc[7].loc[MNUMCR]

    return z
