# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""

import numpy as np
from RW_preprocessor import indcarb


def fill_table_base_037(dfd, table_spec, table_id):
    """Fill table Paper Industry Energy Consumption

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
    INDDIR = 8
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
    #   Paper Industry Energy Consumption
    #    Shipments, Energy Consumption, and Emissions
    #
    #   Value of Shipments (billion m==m dollars)
    # T37(1,IY,IS)=MC_REVIND(11,10,IY)
    z[1] = dfd["MC_REVIND"].loc[10].loc[MNUMCR]
    #
    #   Energy Consumption (trillion Btu) 1/
    #    Residual Fuel Oil
    # T37(2,IY,IS)=PAPERCON(IXRF,5,IY)
    z[2] = dfd["PAPERCON"].loc[IXRF].loc[5]

    #    Distillate Fuel Oil
    # T37(3,IY,IS)=PAPERCON(IXDS,5,IY)
    z[3] = dfd["PAPERCON"].loc[IXDS].loc[5]

    #    Propane
    # T37(4,IY,IS)=PAPERCON(IXLG,5,IY)
    z[4] = dfd["PAPERCON"].loc[IXLG].loc[5]

    #    Petroleum Coke
    # T37(5,IY,IS)=PAPERCON(IXPC,5,IY)
    z[5] = dfd["PAPERCON"].loc[IXPC].loc[5]

    #    Other Petroleum 2/
    # T37(43,IY,IS)=PAPERCON(IXOP,5,IY)
    z[43] = dfd["PAPERCON"].loc[IXOP].loc[5]

    #      Petroleum and Other Liquids Subtotal
    # T37(6,IY,IS)=FSUM(T37(2,IY,IS),4)+T37(43,IY,IS)
    z[6] = z[2] + z[3] + z[4] + z[5] + z[43]

    #    Natural Gas
    # T37(7,IY,IS)=PAPERCON(IXNG,5,IY)
    z[7] = dfd["PAPERCON"].loc[IXNG].loc[5]

    #    Steam Coal
    # T37(8,IY,IS)=PAPERCON(IXCL,5,IY)
    z[8] = dfd["PAPERCON"].loc[IXCL].loc[5]

    #    Renewables
    # T37(9,IY,IS)=PAPERCON(IXRN,5,IY)
    z[9] = dfd["PAPERCON"].loc[IXRN].loc[5]

    #    Purchased Electricity
    # T37(10,IY,IS)=PAPERCON(IXEL,5,IY)
    z[10] = dfd["PAPERCON"].loc[IXEL].loc[5]

    #      Total
    # T37(11,IY,IS)=FSUM(T37(6,IY,IS),5)
    z[11] = z[6] + z[7] + z[8] + z[9] + z[10]

    #   Energy Consumption per Unit of Output 1/
    #   (thousand Btu per m==m dollar shipments)
    #    Residual Fuel Oil
    # T37(12,IY,IS)=PAPERCON(IXRF,5,IY)/MC_REVIND(11,10,IY)
    z[1] = z[1].replace(0, np.inf)  # Replace 0 values with inf to avoid nan
    z[12] = z[2] / z[1]

    #    Distillate Fuel Oil
    # T37(13,IY,IS)=PAPERCON(IXDS,5,IY)/MC_REVIND(11,10,IY)
    z[13] = z[3] / z[1]

    #    Propane
    # T37(14,IY,IS)=PAPERCON(IXLG,5,IY)/MC_REVIND(11,10,IY)
    z[14] = z[4] / z[1]

    #    Petroleum Coke
    # T37(15,IY,IS)=PAPERCON(IXPC,5,IY)/MC_REVIND(11,10,IY)
    z[15] = z[5] / z[1]

    #    Other Petroleum 2/
    # T37(44,IY,IS)=PAPERCON(IXOP,5,IY)/MC_REVIND(11,10,IY)
    z[44] = z[43] / z[1]

    #      Petroleum and Other Liquids Subtotal
    # T37(16,IY,IS)=FSUM(T37(12,IY,IS),4)+T37(44,IY,IS)
    z[16] = z[6] / z[1]

    #    Natural Gas
    # T37(17,IY,IS)=PAPERCON(IXNG,5,IY)/MC_REVIND(11,10,IY)
    z[17] = z[7] / z[1]

    #    Steam Coal
    # T37(18,IY,IS)=PAPERCON(IXCL,5,IY)/MC_REVIND(11,10,IY)
    z[18] = z[8] / z[1]

    #    Renewables
    # T37(19,IY,IS)=PAPERCON(IXRN,5,IY)/MC_REVIND(11,10,IY)
    z[19] = z[9] / z[1]
    #    Purchased Electricity
    # T37(20,IY,IS)=PAPERCON(IXEL,5,IY)/MC_REVIND(11,10,IY)
    z[20] = z[10] / z[1]
    #      Total
    # T37(21,IY,IS)=FSUM(T37(16,IY,IS),5)
    z[21] = z[11] / z[1]

    z[1] = z[1].replace(np.inf, 0)  # Revert 1 values to 0
    #
    #   Carbon Dioxide Emissions 3/
    #   (million metric tons carbon_dd_)
    # T37(22,IY,IS)=INDCARB(PAPERCON(1,1,IY),IY)
    z[22] = indcarb(dfd, dfd["PAPERCON"], CARBON_OR_2)

    #
    #   Combined Heat and Power 4/
    #     Generating Capacity (gigawatts) 5/
    #       Petroleum
    # T37(24,IY,IS)=CHPINDCAP(2,inddir,iy)
    z[24] = dfd["CHPINDCAP"].loc[2].loc[INDDIR]
    #       Natural Gas
    # T37(25,IY,IS)=CHPINDCAP(3,inddir,iy)
    z[25] = dfd["CHPINDCAP"].loc[3].loc[INDDIR]
    #       Coal
    # T37(23,IY,IS)=CHPINDCAP(1,inddir,iy)
    z[23] = dfd["CHPINDCAP"].loc[1].loc[INDDIR]
    #       Other 6/
    # T37(26,IY,IS)=sum(CHPINDCAP(4:6,inddir,iy))
    z[26] = dfd["CHPINDCAP"].loc[((4, 5, 6), INDDIR), :].sum()
    #         Total
    # T37(27,IY,IS)=CHPINDCAP(7,inddir,iy)
    z[27] = dfd["CHPINDCAP"].loc[7].loc[INDDIR]
    #     Net Generation (billion kilowatthours)
    #       Petroleum
    # T37(29,IY,IS)=CHPINDGEN(2,inddir,iy)
    z[29] = dfd["CHPINDGEN"].loc[2].loc[INDDIR]
    #       Natural Gas
    # T37(30,IY,IS)=CHPINDGEN(3,inddir,iy)
    z[30] = dfd["CHPINDGEN"].loc[3].loc[INDDIR]
    #       Coal
    # T37(28,IY,IS)=CHPINDGEN(1,inddir,iy)
    z[28] = dfd["CHPINDGEN"].loc[1].loc[INDDIR]
    #       Other 6/
    # T37(31,IY,IS)=sum(CHPINDGEN(4:6,inddir,iy))
    z[31] = dfd["CHPINDGEN"].loc[((4, 5, 6), INDDIR), :].sum()
    #         Total
    # T37(32,IY,IS)=CHPINDGEN(7,inddir,iy)
    z[32] = dfd["CHPINDGEN"].loc[7].loc[INDDIR]
    #       Disposition
    #         Sales to the Grid
    # T37(33,IY,IS)=CHPINDGEN(7,inddir,iy)*    CHPGRDSHR(inddir,iy)
    z[33] = dfd["CHPINDGEN"].loc[7].loc[INDDIR] * dfd["CHPGRDSHR"].loc[INDDIR]
    #         Generation for Own Use
    # T37(34,IY,IS)=CHPINDGEN(7,inddir,iy)*(1.-CHPGRDSHR(inddir,iy))
    z[34] = dfd["CHPINDGEN"].loc[7].loc[INDDIR] - z[33]
    return z
