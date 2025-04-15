# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""
from RW_preprocessor import indcarb


def fill_table_base_041(dfd, table_spec, table_id):
    """Fill table  Iron and Steel Industry Energy Consumption

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

    INDDIR = 12
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
    IXHF = 19
    #   Iron and Steel Industry Energy Consumption
    #    Shipments, Energy Consumption, and Emissions
    #
    #   Value of Shipments (billion m==m dollars)
    # T41( 1,IY,IS) = MC_REVIND(11,33,IY)+MC_REVIND(11,26,IY)*(1.0-0.91)
    z[1] = dfd["MC_REVIND"].loc[33].loc[MNUMCR] + dfd["MC_REVIND"].loc[26].loc[
        MNUMCR
    ] * (1.0 - 0.91)
    #
    #   Energy Consumption (trillion Btu) 1/
    #    Distillate Fuel Oil
    # T41(2,IY,IS)=STEELCON(IXDS,5,IY)
    z[2] = dfd["STEELCON"].loc[IXDS].loc[5]
    #    Residual Fuel Oil
    # T41(3,IY,IS)=STEELCON(IXRF,5,IY)
    z[3] = dfd["STEELCON"].loc[IXRF].loc[5]
    #    Propane
    # T41(47,IY,IS)=STEELCON(IXLG,5,IY)
    z[47] = dfd["STEELCON"].loc[IXLG].loc[5]
    #    Other Petroleum 2/
    # T41(4,IY,IS)=STEELCON(IXOP,5,IY)
    z[4] = dfd["STEELCON"].loc[IXOP].loc[5]
    #    Petroleum and Other Liquids Subtotal
    # T41(5,IY,IS)=FSUM(T41(2,IY,IS),3)+T41(47,IY,IS)
    z[5] = z[2] + z[3] + z[4] + z[47]
    #    Natural Gas
    # T41(6,IY,IS)=STEELCON(IXNG,5,IY)
    z[6] = dfd["STEELCON"].loc[IXNG].loc[5]
    #    Hydrogen Feedstock
    # (new row)
    z[49] = dfd["STEELCON"].loc[IXHF].loc[5]
    #    Metallurgical Coal
    # T41(7,IY,IS)=STEELCON(IXMC,5,IY)
    z[7] = dfd["STEELCON"].loc[IXMC].loc[5]
    #    Net Coke Imports
    # T41(8,IY,IS)=STEELCON(IXCI,5,IY)
    z[8] = dfd["STEELCON"].loc[IXCI].loc[5]
    #    Steam Coal
    # T41(9,IY,IS)=STEELCON(IXCL,5,IY)
    z[9] = dfd["STEELCON"].loc[IXCL].loc[5]
    #      Coal Subtotal
    # T41(10,IY,IS)=FSUM(T41(7,IY,IS),3)
    z[10] = z[7] + z[8] + z[9]
    #    Renewables
    # T41(45,IY,IS)=STEELCON(IXRN,5,IY)
    z[45] = dfd["STEELCON"].loc[IXRN].loc[5]
    #    Purchased Electricity
    # T41(11,IY,IS)=STEELCON(IXEL,5,IY)
    z[11] = dfd["STEELCON"].loc[IXEL].loc[5]
    #      Total
    # T41(12,IY,IS)=FSUM(T41(5,IY,IS),2)+FSUM(T41(10,IY,IS),2)+T41(45,IY,IS)
    z[12] = z[5] + z[6] + z[10] + z[11] + z[45] + z[49]
    #
    #   Energy Consumption per Unit of Output 1/
    #   (thousand Btu per m==m dollar shipments)
    
    #    Distillate Fuel Oil
    z[13] = z[2] / z[1]
    
    #    Residual Fuel Oil
    z[14] = z[3] / z[1]
    
    #    Propane
    z[48] = z[47] / z[1]
    
    #    Other Petroleum 2/
    z[15] = z[4] / z[1]
    
    #      Petroleum and Other Liquids Subtotal
    # T41(16,IY,IS)=FSUM(T41(13,IY,IS),3)+T41(48,IY,IS)
    z[16] = z[13] + z[14] + z[15] + z[48]
    
    #    Natural Gas
    z[17] = z[6] / z[1]
    
    #    Hydrogen Feedstock
    z[50] = z[49] / z[1]
    
    #    Metallurgical Coal
    z[18] = z[7] / z[1]
    
    #    Net Coke Imports
    z[19] = z[8] / z[1]
    
    #    Steam Coal
    z[20] = z[9] / z[1]
    
    #      Coal Subtotal
    # T41(21,IY,IS)=FSUM(T41(18,IY,IS),3)
    z[21] = z[18] + z[19] + z[20]
    
    #    Renewables
    z[46] = z[45] / z[1]
    
    #    Purchased Electricity
    z[22] = z[11] / z[1]
    
    #      Total
    # T41(23,IY,IS)=FSUM(T41(16,IY,IS),2)+FSUM(T41(21,IY,IS),2)+T41(46,IY,IS)
    z[23] = z[16] + z[17] + z[21] + z[22] + z[46] +z[50]

    #   Carbon Dioxide Emissions 3/
    #   (million metric tons carbon_dd_)
    # T41(24,IY,IS)=INDCARB(STEELCON(1,1,IY),IY)
    z[24] = indcarb(dfd, dfd["STEELCON"], CARBON_OR_2)
    #
    #   Combined Heat and Power 4/
    #     Generating Capacity (gigawatts) 5/
    #       Petroleum
    # T41(26,IY,IS)=CHPINDCAP(2,inddir,iy)
    z[26] = dfd["CHPINDCAP"].loc[2].loc[INDDIR]
    #       Natural Gas
    # T41(27,IY,IS)=CHPINDCAP(3,inddir,iy)
    z[27] = dfd["CHPINDCAP"].loc[3].loc[INDDIR]
    #       Coal
    # T41(25,IY,IS)=CHPINDCAP(1,inddir,iy)
    z[25] = dfd["CHPINDCAP"].loc[1].loc[INDDIR]
    #       Other 6/
    # T41(28,IY,IS)=sum(CHPINDCAP(4:6,inddir,iy))
    z[28] = dfd["CHPINDCAP"].loc[((4, 5, 6), INDDIR), :].sum()
    #         Total
    # T41(29,IY,IS)=CHPINDCAP(7,inddir,iy)
    z[29] = dfd["CHPINDCAP"].loc[7].loc[INDDIR]
    #     Net Generation (billion kilowatthours)
    #       Petroleum
    # T41(31,IY,IS)=CHPINDGEN(2,inddir,iy)
    z[31] = dfd["CHPINDGEN"].loc[2].loc[INDDIR]
    #       Natural Gas
    # T41(32,IY,IS)=CHPINDGEN(3,inddir,iy)
    z[32] = dfd["CHPINDGEN"].loc[3].loc[INDDIR]
    #       Coal
    # T41(30,IY,IS)=CHPINDGEN(1,inddir,iy)
    z[30] = dfd["CHPINDGEN"].loc[1].loc[INDDIR]
    #       Other 6/
    # T41(33,IY,IS)=sum(CHPINDGEN(4:6,inddir,iy))
    z[33] = dfd["CHPINDGEN"].loc[((4, 5, 6), INDDIR), :].sum()
    #         Total
    # T41(34,IY,IS)=CHPINDGEN(7,inddir,iy)
    z[34] = dfd["CHPINDGEN"].loc[7].loc[INDDIR]
    #       Disposition
    #         Sales to the Grid
    # T41(35,IY,IS)=CHPINDGEN(7,inddir,iy)*CHPGRDSHR(inddir,iy)
    z[35] = dfd["CHPINDGEN"].loc[7].loc[INDDIR] * dfd["CHPGRDSHR"].loc[INDDIR]
    #         Generation for Own Use
    # T41(36,IY,IS)=CHPINDGEN(7,inddir,iy)*(1.-CHPGRDSHR(inddir,iy))
    z[36] = dfd["CHPINDGEN"].loc[7].loc[INDDIR] - z[35]
    return z
