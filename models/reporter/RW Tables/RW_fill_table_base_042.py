# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_042(dfd, table_spec, table_id):
    """Fill table Aluminum Industry Energy Consumption

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
    from RW_preprocessor import indcarb

    CARBON_OR_2 = table_spec["settings"]["carbon_or_carbondioxide"].upper()

    MNUMCR = dfd["MNUMCR_rwpre"]
    INDDIR = 13
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

    #   Aluminum Industry Energy Consumption
    #    Shipments, Energy Consumption, and Emissions
    #
    #   Value of Shipments (billion m==m dollars)
    # T42(1,IY,IS)=MC_REVIND(11,34,IY)
    z[1] = dfd["MC_REVIND"].loc[34].loc[MNUMCR]
    #
    #   Energy Consumption (trillion Btu) 1/
    #    Residual Fuel Oil
    # T42(34,IY,IS)=ALUMCON(IXRF,5,IY)
    z[34] = dfd["ALUMCON"].loc[IXRF].loc[5]
    #    Distillate Fuel Oil
    # T42(2,IY,IS)=ALUMCON(IXDS,5,IY)
    z[2] = dfd["ALUMCON"].loc[IXDS].loc[5]
    #    Propane
    # T42(3,IY,IS)=ALUMCON(IXLG,5,IY)
    z[3] = dfd["ALUMCON"].loc[IXLG].loc[5]
    #    Petroleum Coke
    # T42(4,IY,IS)=ALUMCON(IXPC,5,IY)
    z[4] = dfd["ALUMCON"].loc[IXPC].loc[5]
    #    Other Petroleum 2/
    # T42(5,IY,IS)=ALUMCON(IXOP,5,IY)
    z[5] = dfd["ALUMCON"].loc[IXOP].loc[5]
    #      Petroleum and Other Liquids Subtotal
    # T42(6,IY,IS)=FSUM(T42(2,IY,IS),4)+T42(34,IY,IS)
    z[6] = z[2] + z[3] + z[4] + z[5] + z[34]
    #    Natural Gas
    # T42(7,IY,IS)=ALUMCON(IXNG,5,IY)
    z[7] = dfd["ALUMCON"].loc[IXNG].loc[5]
    #    Steam Coal
    # T42(8,IY,IS)=ALUMCON(IXCL,5,IY)
    z[8] = dfd["ALUMCON"].loc[IXCL].loc[5]
    #    Renewables
    # T42(42,IY,IS)=ALUMCON(IXRN,5,IY)
    z[42] = dfd["ALUMCON"].loc[IXRN].loc[5]
    #    Purchased Electricity
    # T42(9,IY,IS)=ALUMCON(IXEL,5,IY)
    z[9] = dfd["ALUMCON"].loc[IXEL].loc[5]
    #      Total
    # T42(10,IY,IS)=FSUM(T42(6,IY,IS),4)+T42(42,IY,IS)
    z[10] = z[6] + z[7] + z[8] + z[9] + z[42]
    #
    #   Energy Consumption per Unit of Output 1/
    #   (thousand Btu per m==m dollar shipments)
    #    Residual Fuel Oil
    # T42(35,IY,IS)=ALUMCON(IXRF,5,IY)/MC_REVIND(11,34,IY)
    z[35] = dfd["ALUMCON"].loc[IXRF].loc[5] / z[1]
    #    Distillate Fuel Oil
    # T42(11,IY,IS)=ALUMCON(IXDS,5,IY)/MC_REVIND(11,34,IY)
    z[11] = dfd["ALUMCON"].loc[IXDS].loc[5] / z[1]
    #    Propane
    # T42(12,IY,IS)=ALUMCON(IXLG,5,IY)/MC_REVIND(11,34,IY)
    z[12] = dfd["ALUMCON"].loc[IXLG].loc[5] / z[1]
    #    Petroleum Coke
    # T42(13,IY,IS)=ALUMCON(IXPC,5,IY)/MC_REVIND(11,34,IY)
    z[13] = dfd["ALUMCON"].loc[IXPC].loc[5] / z[1]
    #    Other Petroleum 2/
    # T42(14,IY,IS)=ALUMCON(IXOP,5,IY)/MC_REVIND(11,34,IY)
    z[14] = dfd["ALUMCON"].loc[IXOP].loc[5] / z[1]
    #      Petroleum and Other Liquids Subtotal
    # T42(15,IY,IS)=FSUM(T42(11,IY,IS),4)
    z[15] = z[11] + z[12] + z[13] + z[14] +z[35]
    #    Natural Gas
    # T42(16,IY,IS)=ALUMCON(IXNG,5,IY)/MC_REVIND(11,34,IY)
    z[16] = dfd["ALUMCON"].loc[IXNG].loc[5] / z[1]
    #    Steam Coal
    # T42(17,IY,IS)=ALUMCON(IXCL,5,IY)/MC_REVIND(11,34,IY)
    z[17] = dfd["ALUMCON"].loc[IXCL].loc[5] / z[1]
    #    Renewables
    # T42(43,IY,IS)=ALUMCON(IXRN,5,IY)/MC_REVIND(11,34,IY)
    z[43] = dfd["ALUMCON"].loc[IXRN].loc[5] / z[1]
    #    Purchased Electricity
    # T42(18,IY,IS)=ALUMCON(IXEL,5,IY)/MC_REVIND(11,34,IY)
    z[18] = dfd["ALUMCON"].loc[IXEL].loc[5] / z[1]
    #      Total
    # T42(19,IY,IS)=FSUM(T42(15,IY,IS),4)+T42(43,IY,IS)
    z[19] = z[15] + z[16] + z[17] + z[18] + z[43]
    #
    #   Carbon Dioxide Emissions 3/
    #   (million metric tons carbon_dd_)
    # T42(20,IY,IS)=INDCARB(ALUMCON(1,1,IY),IY)
    z[20] = indcarb(dfd, dfd["ALUMCON"], CARBON_OR_2)
    #
    #   Combined Heat and Power 4/
    #     Generating Capacity (gigawatts) 5/
    #       Petroleum
    # T42(22,IY,IS)=CHPINDCAP(2,inddir,iy)
    z[22] = dfd["CHPINDCAP"].loc[2].loc[INDDIR]
    #       Natural Gas
    # T42(23,IY,IS)=CHPINDCAP(3,inddir,iy)
    z[23] = dfd["CHPINDCAP"].loc[3].loc[INDDIR]
    #       Coal
    # T42(21,IY,IS)=CHPINDCAP(1,inddir,iy)
    z[21] = dfd["CHPINDCAP"].loc[1].loc[INDDIR]
    #       Other 6/
    # T42(24,IY,IS)=sum(CHPINDCAP(4:6,inddir,iy))
    z[24] = dfd["CHPINDCAP"].loc[((4, 5, 6), INDDIR), :].sum()
    #         Total
    # T42(25,IY,IS)=CHPINDCAP(7,inddir,iy)
    z[25] = dfd["CHPINDCAP"].loc[7].loc[INDDIR]
    #     Net Generation (billion kilowatthours)
    #       Petroleum
    # T42(27,IY,IS)=CHPINDGEN(2,inddir,iy)
    z[27] = dfd["CHPINDGEN"].loc[2].loc[INDDIR]
    #       Natural Gas
    # T42(28,IY,IS)=CHPINDGEN(3,inddir,iy)
    z[28] = dfd["CHPINDGEN"].loc[3].loc[INDDIR]
    #       Coal
    # T42(26,IY,IS)=CHPINDGEN(1,inddir,iy)
    z[26] = dfd["CHPINDGEN"].loc[1].loc[INDDIR]
    #       Other 6/
    # T42(29,IY,IS)=sum(CHPINDGEN(4:6,inddir,iy))
    z[29] = dfd["CHPINDGEN"].loc[((4, 5, 6), INDDIR), :].sum()
    #         Total
    # T42(30,IY,IS)=CHPINDGEN(7,inddir,iy)
    z[30] = dfd["CHPINDGEN"].loc[7].loc[INDDIR]
    #       Disposition
    #         Sales to the Grid
    # T42(31,IY,IS)=CHPINDGEN(7,inddir,iy)*    CHPGRDSHR(inddir,iy)
    z[31] = dfd["CHPINDGEN"].loc[7].loc[INDDIR] * dfd["CHPGRDSHR"].loc[INDDIR]
    #         Generation for Own Use
    # T42(32,IY,IS)=CHPINDGEN(7,inddir,iy)*(1.-CHPGRDSHR(inddir,iy))
    z[32] = dfd["CHPINDGEN"].loc[7].loc[INDDIR] - z[31]
    return z
