# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""
from RW_preprocessor import indcarb


def fill_table_base_036(dfd, table_spec, table_id):
    """Fill table Food Industry Energy Consumption

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
    INDDIR = 7
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
    #   Food Industry Energy Consumption
    #    Shipments & Energy Consumption
    #
    #   Value of Shipments (billion m==m dollars)
    # T36(1,IY,IS) = MC_REVIND(11,1,IY)
    z[1] = dfd["MC_REVIND"].loc[1, MNUMCR]
    #
    #   Energy Consumption (trillion Btu) 1/
    #    Residual Fuel Oil
    # T36(2,IY,IS)=FOODCON(IXRF,5,IY)
    z[2] = dfd["FOODCON"].loc[IXRF].loc[5]
    #    Distillate Fuel Oil
    # T36(3,IY,IS)=FOODCON(IXDS,5,IY)
    z[3] = dfd["FOODCON"].loc[IXDS].loc[5]
    #    Propane
    # T36(4,IY,IS)=FOODCON(IXLG,5,IY)
    z[4] = dfd["FOODCON"].loc[IXLG].loc[5]
    #    Other Petroleum 2/
    # T36(5,IY,IS)=FOODCON(IXOP,5,IY)
    z[5] = dfd["FOODCON"].loc[IXOP].loc[5]
    #      Petroleum and Other Liquids Subtotal
    # T36(6,IY,IS)=FSUM(T36(2,IY,IS),4)
    z[6] = z[2] + z[3] + z[4] + z[5]
    #    Natural Gas
    # T36(7,IY,IS)=FOODCON(IXNG,5,IY)
    z[7] = dfd["FOODCON"].loc[IXNG].loc[5]
    #    Steam Coal
    # T36(8,IY,IS)=FOODCON(IXCL,5,IY)
    z[8] = dfd["FOODCON"].loc[IXCL].loc[5]
    #    Renewables
    # T36(9,IY,IS)=FOODCON(IXRN,5,IY)
    z[9] = dfd["FOODCON"].loc[IXRN].loc[5]
    #    Purchased Electricity
    # T36(10,IY,IS)=FOODCON(IXEL,5,IY)
    z[10] = dfd["FOODCON"].loc[IXEL].loc[5]
    #      Total
    # T36(11,IY,IS)=FSUM(T36(6,IY,IS),5)
    z[11] = z[6] + z[7] + z[8] + z[9] + z[10]
    #
    #   Energy Consumption per Unit of Output 1/
    #   (thousand Btu per m==m dollar shipments)
    #    Residual Fuel Oil
    # T36(12,IY,IS)=FOODCON(IXRF,5,IY)/MC_REVIND(11,1,IY)
    z[12] = z[2].div(z[1], fill_value=0)

    #    Distillate Fuel Oil
    # T36(13,IY,IS)=FOODCON(IXDS,5,IY)/MC_REVIND(11,1,IY)
    z[13] = z[3].div(z[1], fill_value=0)
    #    Propane
    # T36(14,IY,IS)=FOODCON(IXLG,5,IY)/MC_REVIND(11,1,IY)
    z[14] = z[4].div(z[1], fill_value=0)
    #    Other Petroleum 2/
    # T36(15,IY,IS)=FOODCON(IXOP,5,IY)/MC_REVIND(11,1,IY)
    z[15] = z[5].div(z[1], fill_value=0)
    #      Petroleum and Other Liquids Subtotal
    # T36(16,IY,IS)=FSUM(T36(12,IY,IS),4)
    z[16] = z[12] + z[13] + z[14] + z[15]
    #    Natural Gas
    # T36(17,IY,IS)=FOODCON(IXNG,5,IY)/MC_REVIND(11,1,IY)
    z[17] = z[7].div(z[1], fill_value=0)
    #    Steam Coal
    # T36(18,IY,IS)=FOODCON(IXCL,5,IY)/MC_REVIND(11,1,IY)
    z[18] = z[8].div(z[1], fill_value=0)
    #    Renewables
    # T36(19,IY,IS)=FOODCON(IXRN,5,IY)/MC_REVIND(11,1,IY)
    z[19] = z[9].div(z[1], fill_value=0)
    #    Purchased Electricity
    # T36(20,IY,IS)=FOODCON(IXEL,5,IY)/MC_REVIND(11,1,IY)
    z[20] = z[10].divide(z[1], fill_value=0)
    #      Total
    # T36(21,IY,IS)=FSUM(T36(16,IY,IS),5)
    z[21] = z[16] + z[17] + z[18] + z[19] + z[20]
    #
    #   Carbon Dioxide Emissions 3/
    #   (million metric tons carbon_dd_)
    # T36(22,IY,IS)=INDCARB(FOODCON(1,1,IY),IY)

    z[22] = indcarb(dfd, dfd["FOODCON"], CARBON_OR_2)
    #
    #   Combined Heat and Power 4/
    #     Generating Capacity (gigawatts) 5/
    #       Petroleum
    # T36(24,IY,IS)=CHPINDCAP(2,inddir,iy)
    z[24] = dfd["CHPINDCAP"].loc[2].loc[INDDIR]
    #       Natural Gas
    # T36(25,IY,IS)=CHPINDCAP(3,inddir,iy)
    z[25] = dfd["CHPINDCAP"].loc[3].loc[INDDIR]
    #       Coal
    # T36(23,IY,IS)=CHPINDCAP(1,inddir,iy)
    z[23] = dfd["CHPINDCAP"].loc[1].loc[INDDIR]
    #       Other 6/
    # T36(26,IY,IS)=sum(CHPINDCAP(4:6,inddir,iy))
    z[26] = dfd["CHPINDCAP"].loc[((4, 5, 6), INDDIR), :].sum()
    #         Total
    # T36(27,IY,IS)=CHPINDCAP(7,inddir,iy)
    z[27] = dfd["CHPINDCAP"].loc[7].loc[INDDIR]
    #     Net Generation (billion kilowatthours)
    #       Petroleum
    # T36(29,IY,IS)=CHPINDGEN(2,inddir,iy)
    z[29] = dfd["CHPINDGEN"].loc[2].loc[INDDIR]
    #       Natural Gas
    # T36(30,IY,IS)=CHPINDGEN(3,inddir,iy)
    z[30] = dfd["CHPINDGEN"].loc[3].loc[INDDIR]
    #       Coal
    # T36(28,IY,IS)=CHPINDGEN(1,inddir,iy)
    z[28] = dfd["CHPINDGEN"].loc[1].loc[INDDIR]
    #       Other 6/
    # T36(31,IY,IS)=sum(CHPINDGEN(4:6,inddir,iy))
    z[31] = dfd["CHPINDGEN"].loc[((4, 5, 6), INDDIR), :].sum()
    #         Total
    # T36(32,IY,IS)=CHPINDGEN(7,inddir,iy)
    z[32] = dfd["CHPINDGEN"].loc[7].loc[INDDIR]
    #       Disposition
    #         Sales to the Grid
    # T36(33,IY,IS)=CHPINDGEN(7,inddir,iy)*    CHPGRDSHR(inddir,iy)
    z[33] = dfd["CHPINDGEN"].loc[7].loc[INDDIR] * dfd["CHPGRDSHR"].loc[INDDIR]
    #         Generation for Own Use
    # T36(34,IY,IS)=CHPINDGEN(7,inddir,iy)*(1.-CHPGRDSHR(inddir,iy))
    z[34] = dfd["CHPINDGEN"].loc[7].loc[INDDIR] - z[33]
    return z
