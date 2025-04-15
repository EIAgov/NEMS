# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""
from RW_preprocessor import indcarb


def fill_table_base_043(dfd, table_spec, table_id):
    """Fill table   Nonmanufacturing Sector Energy Consumption

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

    CARBON_OR_2 = table_spec["settings"]["carbon_or_carbondioxide"].upper()
    MNUMCR = dfd["MNUMCR_rwpre"]

    IXEL = 1
    IXNG = 2
    IXCL = 3
    IXMC = 4
    IXCI = 5
    IXRF = 6
    IXDS = 7
    IXLG = 8
    IXMG = 9
    IXSG = 10
    IXPC = 11
    IXAS = 12
    IXPF = 13
    IXKS = 14
    IXOP = 15
    IXNF = 16
    IXLF = 17
    IXRN = 18
    #   Nonmanufacturing Sector Energy Consumption
    #    Shipments, Energy Consumption, and Emissions
    #
    #   Energy Consumption (trillion Btu)
    #      Agriculture
    #         Residual Fuel Oil
    # T43(60,IY,IS)=AGCON(IXRF,5,IY)
    z[60] = dfd["AGCON"].loc[IXRF].loc[5]
    #         Distillate Fuel Oil
    # T43(1,IY,IS)=AGCON(IXDS,5,IY)
    z[1] = dfd["AGCON"].loc[IXDS].loc[5]
    #         Propane
    # T43(2,IY,IS)=AGCON(IXLG,5,IY)
    z[2] = dfd["AGCON"].loc[IXLG].loc[5]
    #         Motor Gasoline
    # T43(3,IY,IS)=AGCON(IXMG,5,IY)
    z[3] = dfd["AGCON"].loc[IXMG].loc[5]
    #         Other Petroleum 1/
    # T43(4,IY,IS)=AGCON(IXOP,5,IY)
    z[4] = dfd["AGCON"].loc[IXOP].loc[5]
    #            Petroleum and Other Liquids Subtotal
    # T43(5,IY,IS)=FSUM(T43(1,IY,IS),4)+T43(60,IY,IS)
    z[5] = z[1] + z[2] + z[3] + z[4] + z[60]
    #         Natural Gas
    # T43(6,IY,IS)=AGCON(IXNG,5,IY)
    z[6] = dfd["AGCON"].loc[IXNG].loc[5]
    #         Steam Coal
    # T43(7,IY,IS)=AGCON(IXCL,5,IY)
    z[7] = dfd["AGCON"].loc[IXCL].loc[5]
    #         Renewables
    # T43(8,IY,IS)=AGCON(IXRN,5,IY)
    z[8] = dfd["AGCON"].loc[IXRN].loc[5]
    #         Purchased Electricity
    # T43(9,IY,IS)=AGCON(IXEL,5,IY)
    z[9] = dfd["AGCON"].loc[IXEL].loc[5]
    #            Total
    # T43(10,IY,IS)=FSUM(T43(5,IY,IS),5)
    z[10] = z[5] + z[6] + z[7] + z[8] + z[9]
    #
    #      Construction
    #         Distillate Fuel Oil
    # T43(14,IY,IS)=CONSTCON(IXDS,5,IY)
    z[14] = dfd["CONSTCON"].loc[IXDS].loc[5]
    #         Propane
    # T43(15,IY,IS)=CONSTCON(IXLG,5,IY)
    z[15] = dfd["CONSTCON"].loc[IXLG].loc[5]
    #         Motor Gasoline
    # T43(16,IY,IS)=CONSTCON(IXMG,5,IY)
    z[16] = dfd["CONSTCON"].loc[IXMG].loc[5]
    #         Asphalt and Road Oil
    # T43(17,IY,IS)=CONSTCON(IXAS,5,IY)
    z[17] = dfd["CONSTCON"].loc[IXAS].loc[5]
    #         Other Petroleum 1/
    # T43(59,IY,IS)=CONSTCON(IXOP,5,IY)
    z[59] = dfd["CONSTCON"].loc[IXOP].loc[5]
    #            Petroleum and Other Liquids Subtotal
    # T43(18,IY,IS)=FSUM(T43(14,IY,IS),4)+T43(59,IY,IS)
    z[18] = z[14] + z[15] + z[16] + z[17] + z[59]
    #         Natural Gas
    # T43(19,IY,IS)=CONSTCON(IXNG,5,IY)
    z[19] = dfd["CONSTCON"].loc[IXNG].loc[5]
    #         Purchased Electricity
    # T43(21,IY,IS)=CONSTCON(IXEL,5,IY)
    z[21] = dfd["CONSTCON"].loc[IXEL].loc[5]
    #            Total
    # T43(22,IY,IS)=FSUM(T43(18,IY,IS),4)
    z[22] = z[18] + z[19] + z[21]
    #
    #      Mining
    #         Residual Fuel Oil
    # T43(26,IY,IS)=MINECON(IXRF,5,IY)
    z[26] = dfd["MINECON"].loc[IXRF].loc[5]
    #         Distillate Fuel Oil
    # T43(27,IY,IS)=MINECON(IXDS,5,IY)
    z[27] = dfd["MINECON"].loc[IXDS].loc[5]
    #         Motor Gasoline
    # T43(28,IY,IS)=MINECON(IXMG,5,IY)
    z[28] = dfd["MINECON"].loc[IXMG].loc[5]
    #         Other Petroleum 1/
    # T43(29,IY,IS)=MINECON(IXOP,5,IY)
    z[29] = dfd["MINECON"].loc[IXOP].loc[5]
    #            Petroleum and Other Liquids Subtotal
    # T43(30,IY,IS)=FSUM(T43(26,IY,IS),4)
    z[30] = z[26] + z[27] + z[28] + z[29]
    #         Natural Gas
    # T43(31,IY,IS)=MINECON(IXNG,5,IY)
    z[31] = dfd["MINECON"].loc[IXNG].loc[5]
    #         Lease and Plant Fuel 2/
    # T43(32,IY,IS)=QLPIN(11,IY)*1000.
    z[32] = dfd["QLPIN"].loc[MNUMCR]
    #         Steam Coal
    # T43(33,IY,IS)=MINECON(IXCL,5,IY)
    z[33] = dfd["MINECON"].loc[IXCL].loc[5]
    #         Renewables
    # T43(34,IY,IS)=MINECON(IXRN,5,IY)
    z[34] = dfd["MINECON"].loc[IXRN].loc[5]
    #         Purchased Electricity Excluding Oil Shale
    # T43(35,IY,IS)=MINECON(IXEL,5,IY)
    z[35] = dfd["MINECON"].loc[IXEL].loc[5]
    #         Purchased Electricity for Oil Shale
    # T43(36,IY,IS)=OGELSHALE(IY)
    z[36] = dfd["OGELSHALE"]
    #            Total
    # T43(37,IY,IS)=FSUM(T43(30,IY,IS),7)
    z[37] = z[30] + z[31] + z[32] + z[33] + z[34] + z[35] + z[36]
    #
    #   Value of Shipments (billion m==m dollars)
    #      Agriculture
    # T43(11,IY,IS)=MC_REVIND(11,42,IY)+MC_REVIND(11,43,IY)+MC_REVIND(11,44,IY)
    z[11] = (
        dfd["MC_REVIND"].loc[42].loc[MNUMCR]
        + dfd["MC_REVIND"].loc[43].loc[MNUMCR]
        + dfd["MC_REVIND"].loc[44].loc[MNUMCR]
    )
    #      Construction
    # T43(23,IY,IS)=MC_REVIND(11,48,IY)
    z[23] = dfd["MC_REVIND"].loc[48].loc[MNUMCR]
    #      Mining
    # T43(38,IY,IS)=MC_REVIND(11,45,IY)+MC_REVIND(11,46,IY)+MC_REVIND(11,47,IY)
    z[38] = (
        dfd["MC_REVIND"].loc[45].loc[MNUMCR]
        + dfd["MC_REVIND"].loc[46].loc[MNUMCR]
        + dfd["MC_REVIND"].loc[47].loc[MNUMCR]
    )
    #
    #   Energy Consumption per Unit of Output
    #   (thousand Btu per m==m dollar shipments)
    #      Agriculture
    # T43(12,IY,IS)=T43(10,IY,IS)/(MC_REVIND(11,42,IY)+MC_REVIND(11,43,IY)+MC_REVIND(11,44,IY))
    z[12] = z[10] / z[11]
    #      Construction
    # T43(24,IY,IS)=T43(22,IY,IS)/MC_REVIND(11,48,IY)
    # z[24] = z[22].div(z[23], fill_value=0)
    z[24] = z[22] / z[23]
    #      Mining
    # T43(39,IY,IS)=T43(37,IY,IS)/(MC_REVIND(11,45,IY)+MC_REVIND(11,46,IY)+MC_REVIND(11,47,IY))
    z[39] = z[37] / z[38]
    #
    #   Carbon Dioxide Emissions 3/
    #   (million metric tons carbon_dd_)
    #      Agriculture
    # T43(13,IY,IS)=INDCARB(AGCON(1,1,IY),IY)
    z[13] = indcarb(dfd, dfd["AGCON"], CARBON_OR_2)
    #      Construction
    # T43(25,IY,IS)=INDCARB(CONSTCON(1,1,IY),IY)
    z[25] = indcarb(dfd, dfd["CONSTCON"], CARBON_OR_2)
    #      Mining
    # T43(40,IY,IS)=INDCARB(MINECON(1,1,IY),IY)
    z[40] = indcarb(dfd, dfd["MINECON"], CARBON_OR_2)
    #
    #   Agriculture, Construction, and Mining:
    #
    #    Combined Heat and Power 4/
    #     Generating Capacity (gigawatts) 5/
    #       Petroleum
    # T43(42,IY,IS)=SUM(CHPINDCAP(2,V8,IY))
    z[42] = dfd["CHPINDCAP"].loc[2, (1, 2, 3, 4, 5, 6), :].sum()
    #       Natural Gas
    # T43(43,IY,IS)=SUM(CHPINDCAP(3,V8,IY))
    z[43] = dfd["CHPINDCAP"].loc[3, (1, 2, 3, 4, 5, 6), :].sum()
    #       Coal
    # T43(41,IY,IS)=SUM(CHPINDCAP(1,V8,IY))
    z[41] = dfd["CHPINDCAP"].loc[1, (1, 2, 3, 4, 5, 6), :].sum()
    #       Other 6/
    # T43(44,IY,IS)=SUM(CHPINDCAP(4:6,V8,IY))
    z[44] = dfd["CHPINDCAP"].loc[(4, 5, 6), (1, 2, 3, 4, 5, 6), :].sum()
    #         Total
    # T43(45,IY,IS)=SUM(CHPINDCAP(7,V8,IY))
    z[45] = dfd["CHPINDCAP"].loc[7, (1, 2, 3, 4, 5, 6), :].sum()
    #     Net Generation (billion kilowatthours)
    #       Petroleum
    # T43(47,IY,IS)=SUM(CHPINDGEN(2,V8,IY))
    z[47] = dfd["CHPINDGEN"].loc[2, (1, 2, 3, 4, 5, 6), :].sum()
    #       Natural Gas
    # T43(48,IY,IS)=SUM(CHPINDGEN(3,V8,IY))
    z[48] = dfd["CHPINDGEN"].loc[3, (1, 2, 3, 4, 5, 6), :].sum()
    #       Coal
    # T43(46,IY,IS)=SUM(CHPINDGEN(1,V8,IY))
    z[46] = dfd["CHPINDGEN"].loc[1, (1, 2, 3, 4, 5, 6), :].sum()
    #       Other 6/
    # T43(49,IY,IS)=SUM(CHPINDGEN(4:6,V8,IY))
    z[49] = dfd["CHPINDGEN"].loc[(4, 5, 6), (1, 2, 3, 4, 5, 6), :].sum()
    #         Total
    # T43(50,IY,IS)=SUM(CHPINDGEN(7,V8,IY))
    z[50] = dfd["CHPINDGEN"].loc[7, (1, 2, 3, 4, 5, 6), :].sum()
    #       Disposition
    #         Sales to the Grid
    z[51] = z[50] * 0.0
    # T43(51,IY,IS)=SUM(CHPINDGEN(7,V8,IY)*CHPGRDSHR(V8,IY))
    for x in range(1, 7):
        z[51] += dfd["CHPINDGEN"].loc[7, x] * dfd["CHPGRDSHR"].loc[x]
    #         Generation for Own Use
    # T43(52,IY,IS)=SUM(CHPINDGEN(7,V8,IY)*(1.-CHPGRDSHR(V8,IY)))
    z[52] = z[50] - z[51]
    return z