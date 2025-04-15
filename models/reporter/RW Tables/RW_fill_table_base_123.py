# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_123(dfd, table_spec, table_id):
    """Fill table  Overnight Capital Costs for New Electricity Generating Plant

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
    FYRPRC = int(table_spec["settings"]["year_index_real_dol_priceyr"])
    # Get inflation index from 1990->2050
    MC_JPGDP = dfd["MC_JPGDP"].loc[1990:]

    # Get inflation index of FYRPRC (2022)
    SCALPR2 = MC_JPGDP.loc[FYRPRC]  # 2022 = column 33

    #   Overnight Capital Costs for New Electricity Generating Plant
    #   (#### dollars per kilowatt)
    #    Technology

    #   Coal

    #      Ultra-supercritical (USC) Coal
    # T123(3,IR,IY,IS)=UPPCCCST(IR,IY)
    z[3] = dfd["UPPCCCST"] * SCALPR2

    #      Integrated Gasification Combined Cycle
    # T123(1,IR,IY,IS)=UPIGCCST(IR,IY)
    z[1] = dfd["UPIGCCST"] * SCALPR2

    #      USC Coal with 30% Sequestration
    # T123(21,IR,IY,IS)=UPPQCCST(IR,IY)
    z[21] = dfd["UPPQCCST"] * SCALPR2

    #      USC Coal with 90% Sequestration
    # T123(2,IR,IY,IS)=UPISCCST(IR,IY)
    z[2] = dfd["UPISCCST"] * SCALPR2

    #      Other New Coal
    # T123(23,IR,IY,IS)=UPOCCCST(IR,IY)
    z[23] = dfd["UPOCCCST"] * SCALPR2

    #      IGCC with Gas Cofiring
    # T123(24,IR,IY,IS)=UPI2CCST(IR,IY)
    z[24] = dfd["UPI2CCST"] * SCALPR2

    #   Combined Cycle

    #      Combined Cycle - Single Shaft
    # T123(6,IR,IY,IS)=UPCCCCST(IR,IY)
    z[6] = dfd["UPCCCCST"] * SCALPR2

    #      Combined Cycle - Multi Shaft
    # T123(4,IR,IY,IS)=UPACCCST(IR,IY)
    z[4] = dfd["UPACCCST"] * SCALPR2

    #      Combined Cycle with 90% Sequestration
    # T123(5,IR,IY,IS)=UPCSCCST(IR,IY)
    z[5] = dfd["UPCSCCST"] * SCALPR2

    #      Conventional (Retrofits) Combined Cycle
    # T123(27,IR,IY,IS)=UPA2CCST(IR,IY)
    z[27] = dfd["UPA2CCST"] * SCALPR2

    #   Combustion Turbine/Diesel

    #      Combustion Turbine - Aeroderivative
    # T123(8,IR,IY,IS)=UPCTCCST(IR,IY)
    z[8] = dfd["UPCTCCST"] * SCALPR2

    #      Combustion Turbine - Industrial Frame
    # T123(7,IR,IY,IS)=UPATCCST(IR,IY)
    z[7] = dfd["UPATCCST"] * SCALPR2

    #      Other Turbine
    # T123(26,IR,IY,IS)=UPT2CCST(IR,IY)
    z[26] = dfd["UPT2CCST"] * SCALPR2

    #   Nuclear Power

    #      Light Water Reactor
    # T123(9,IR,IY,IS)=UPANCCST(IR,IY)
    z[9] = dfd["UPANCCST"] * SCALPR2

    #      Small Modular Reactor
    # T123(22,IR,IY,IS)=UPSMCCST(IR,IY)
    z[22] = dfd["UPSMCCST"] * SCALPR2

    #      Greenfield
    # T123(28,IR,IY,IS)=UPGNCCST(IR,IY)
    z[28] = dfd["UPGNCCST"] * SCALPR2

    #   Fuel Cells
    # T123(10,IR,IY,IS)=UPFCCCST(IR,IY)
    z[10] = dfd["UPFCCCST"] * SCALPR2

    #   Hydrogen Turbine
    # T123(25,IR,IY,IS)=UPICCCST(IR,IY)
    z[25] = dfd["UPICCCST"] * SCALPR2

    #   Renewable Sources

    #      Conventional Hydroelectric Power 1/
    # T123(11,IR,IY,IS)=UPHYCCST(IR,IY)
    z[11] = dfd["UPHYCCST"] * SCALPR2

    #      Advanced Hydroelectric Power
    # T123(31,IR,IY,IS)=UPHOCCST(IR,IY)
    z[31] = dfd["UPHOCCST"] * SCALPR2

    #      Hydrokinetic In-stream Power
    # T123(32,IR,IY,IS)=UPHICCST(IR,IY)
    z[32] = dfd["UPHICCST"] * SCALPR2

    #      Tidal Hydroelectric Power
    # T123(33,IR,IY,IS)=UPTICCST(IR,IY)
    z[33] = dfd["UPTICCST"] * SCALPR2

    #      Geothermal 1/
    # T123(12,IR,IY,IS)=UPGTCCST(IR,IY)
    z[12] = dfd["UPGTCCST"] * SCALPR2

    #      Advanced Geothermal
    # T123(30,IR,IY,IS)=UPAGCCST(IR,IY)
    z[30] = dfd["UPAGCCST"] * SCALPR2

    #      Wood and Other Biomass
    # T123(14,IR,IY,IS)=UPWDCCST(IR,IY)
    z[14] = dfd["UPWDCCST"] * SCALPR2

    #      Biomass IGCC
    # T123(29,IR,IY,IS)=UPBICCST(IR,IY)
    z[29] = dfd["UPBICCST"] * SCALPR2

    #      Solar Thermal
    # T123(15,IR,IY,IS)=UPSOCCST(IR,IY)
    z[15] = dfd["UPSOCCST"] * SCALPR2

    #      Solar Thermal w/Storage
    # T123(38,IR,IY,IS)=UPSSCCST(IR,IY)
    z[38] = dfd["UPSSCCST"] * SCALPR2

    #      Solar Thermal w/Storage 2
    # T123(39,IR,IY,IS)=UPS2CCST(IR,IY)
    z[39] = dfd["UPS2CCST"] * SCALPR2

    #      Solar Photovoltaic with Axis Tracking
    # T123(16,IR,IY,IS)=UPPVCCST(IR,IY)
    z[16] = dfd["UPPVCCST"] * SCALPR2

    #      Solar Photovoltaic with Storage
    # T123(40,IR,IY,IS)=UPPTCCST(IR,IY)
    z[40] = dfd["UPPTCCST"] * SCALPR2

    #      Wind
    # T123(17,IR,IY,IS)=UPWNCCST(IR,IY)
    z[17] = dfd["UPWNCCST"] * SCALPR2

    #      Offshore Wind
    # T123(18,IR,IY,IS)=UPWFCCST(IR,IY)
    z[18] = dfd["UPWFCCST"] * SCALPR2

    #      Wind Low Speed
    # T123(37,IR,IY,IS)=UPWLCCST(IR,IY)
    z[37] = dfd["UPWLCCST"] * SCALPR2

    #      Quick Storage
    # T123(34,IR,IY,IS)=UPSQCCST(IR,IY)
    z[34] = dfd["UPSQCCST"] * SCALPR2

    #      Diurnal Storage
    # T123(35,IR,IY,IS)=UPDSCCST(IR,IY)
    z[35] = dfd["UPDSCCST"] * SCALPR2

    #      Other Storage
    # T123(36,IR,IY,IS)=UPZSCCST(IR,IY)
    z[36] = dfd["UPZSCCST"] * SCALPR2

    #      Other Intermittant
    # T123(41,IR,IY,IS)=UPINCCST(IR,IY)
    z[41] = dfd["UPINCCST"] * SCALPR2

    #   Distributed Generation

    #      Base
    # T123(19,IR,IY,IS)=UPDBCCST(IR,IY)
    z[19] = dfd["UPDBCCST"] * SCALPR2

    #      Peak
    # T123(20,IR,IY,IS)=UPDPCCST(IR,IY)
    z[20] = dfd["UPDPCCST"] * SCALPR2

    return z
