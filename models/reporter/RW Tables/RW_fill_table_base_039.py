# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""
from RW_preprocessor import indcarb


def fill_table_base_039(dfd, table_spec, table_id):
    """Fill table Glass Industry Energy Consumption

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
    INDDIR = 10
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

    #   Glass Industry Energy Consumption
    #    Shipments, Energy Consumption, and Emissions
    #
    #   Value of Shipments (billion m==m dollars)
    # T39(1,IY,IS)=MC_REVIND(11,28,IY)
    z[1] = dfd["MC_REVIND"].loc[28].loc[MNUMCR]
    #
    #   Energy Consumption (trillion Btu) 1/
    #    Residual Fuel Oil
    # T39(2,IY,IS)=GLASSCON(IXRF,5,IY)
    z[2] = dfd["GLASSCON"].loc[IXRF].loc[5]
    #    Distillate Fuel Oil
    # T39(3,IY,IS)=GLASSCON(IXDS,5,IY)
    z[3] = dfd["GLASSCON"].loc[IXDS].loc[5]
    #    Propane
    # T39(4,IY,IS)=GLASSCON(IXLG,5,IY)
    z[4] = dfd["GLASSCON"].loc[IXLG].loc[5]
    #      Petroleum and Other Liquids Subtotal
    # T39(5,IY,IS)=FSUM(T39(2,IY,IS),3)
    z[5] = z[2] + z[3] + z[4]
    #    Natural Gas
    # T39(6,IY,IS)=GLASSCON(IXNG,5,IY)
    z[6] = dfd["GLASSCON"].loc[IXNG].loc[5]
    #    Steam Coal
    # T39(7,IY,IS)=GLASSCON(IXCL,5,IY)
    z[7] = dfd["GLASSCON"].loc[IXCL].loc[5]
    #    Purchased Electricity
    # T39(8,IY,IS)=GLASSCON(IXEL,5,IY)
    z[8] = dfd["GLASSCON"].loc[IXEL].loc[5]
    #      Total
    # T39(9,IY,IS)=FSUM(T39(5,IY,IS),4)
    z[9] = z[5] + z[6] + z[7] + z[8]
    #
    #   Energy Consumption per Unit of Output 1/
    #   (thousand Btu per m==m dollar shipments)
    #    Residual Fuel Oil
    # T39(10,IY,IS)=GLASSCON(IXRF,5,IY)/MC_REVIND(11,28,IY)
    z[10] = z[2] / z[1]
    #    Distillate Fuel Oil
    # T39(11,IY,IS)=GLASSCON(IXDS,5,IY)/MC_REVIND(11,28,IY)
    z[11] = z[3] / z[1]
    #    Propane
    # T39(12,IY,IS)=GLASSCON(IXLG,5,IY)/MC_REVIND(11,28,IY)
    z[12] = z[4] / z[1]
    #      Petroleum and Other Liquids Subtotal
    # T39(13,IY,IS)=FSUM(T39(10,IY,IS),3)
    z[13] = z[10] + z[11] + z[12]
    #    Natural Gas
    # T39(14,IY,IS)=GLASSCON(IXNG,5,IY)/MC_REVIND(11,28,IY)
    z[14] = z[6] / z[1]
    #    Steam Coal
    # T39(15,IY,IS)=GLASSCON(IXCL,5,IY)/MC_REVIND(11,28,IY)
    z[15] = z[7] / z[1]
    #    Purchased Electricity
    # T39(16,IY,IS)=GLASSCON(IXEL,5,IY)/MC_REVIND(11,28,IY)
    z[16] = z[8] / z[1]
    #      Total
    # T39(17,IY,IS)=FSUM(T39(13,IY,IS),4)
    z[17] = z[13] + z[14] + z[15] + z[16]
    #
    #   Carbon Dioxide Emissions 2/
    #      Combustion (million metric tons): T39(18,IY,IS)=INDCARB(GLASSCON(1,1,IY),IY)

    z[18] = indcarb(dfd, dfd["GLASSCON"], CARBON_OR_2)

    #      Process (million metric tons): (32,IY,IS) = GHG_PROCESSIN(10,IY)
    z[32] = dfd["GHG_PROCESSIN"].loc[10]

    #   Combined Heat and Power 3/
    #     Generating Capacity (gigawatts) 4/
    #       Petroleum
    # T39(20,IY,IS)=CHPINDCAP(2,INDDIR,IY)
    z[20] = dfd["CHPINDCAP"].loc[2].loc[INDDIR]

    #       Natural Gas
    # T39(21,IY,IS)=CHPINDCAP(3,INDDIR,IY)
    z[21] = dfd["CHPINDCAP"].loc[3].loc[INDDIR]

    #       Coal
    # T39(19,IY,IS)=CHPINDCAP(1,INDDIR,IY)
    z[19] = dfd["CHPINDCAP"].loc[1].loc[INDDIR]

    #       Other 5/
    # T39(22,IY,IS)=SUM(CHPINDCAP(4:6,INDDIR,IY))
    # z[22] =  dfd['CHPINDCAP'].loc[4].loc[INDDIR]+ dfd['CHPINDCAP'].loc[5].loc[INDDIR]+ dfd['CHPINDCAP'].loc[6].loc[INDDIR]
    z[22] = dfd["CHPINDCAP"].loc[((4, 5, 6), INDDIR), :].sum()
    #         Total
    # T39(23,IY,IS)=CHPINDCAP(7,INDDIR,IY)
    z[23] = dfd["CHPINDCAP"].loc[7].loc[INDDIR]
    #     Net Generation (billion kilowatthours)
    #       Petroleum
    # T39(25,IY,IS)=CHPINDGEN(2,INDDIR,IY)
    z[25] = dfd["CHPINDGEN"].loc[2].loc[INDDIR]
    #       Natural Gas
    # T39(26,IY,IS)=CHPINDGEN(3,INDDIR,IY)
    z[26] = dfd["CHPINDGEN"].loc[3].loc[INDDIR]
    #       Coal
    # T39(24,IY,IS)=CHPINDGEN(1,INDDIR,IY)
    z[24] = dfd["CHPINDGEN"].loc[1].loc[INDDIR]
    #       Other 5/
    # T39(27,IY,IS)=SUM(CHPINDGEN(4:6,INDDIR,IY))
    z[27] = dfd["CHPINDGEN"].loc[((4, 5, 6), INDDIR), :].sum()
    #         Total
    # T39(28,IY,IS)=CHPINDGEN(7,INDDIR,IY)
    z[28] = dfd["CHPINDGEN"].loc[7].loc[INDDIR]
    #       Disposition
    #         Sales to the Grid
    # T39(29,IY,IS)=CHPINDGEN(7,INDDIR,IY) * CHPGRDSHR(INDDIR,IY)
    z[29] = dfd["CHPINDGEN"].loc[7].loc[INDDIR] * dfd["CHPGRDSHR"].loc[INDDIR]
    #         Generation for Own Use
    # T39(30,IY,IS)=CHPINDGEN(7,INDDIR,IY) * (1.-CHPGRDSHR(INDDIR,IY))
    z[30] = (
        dfd["CHPINDGEN"].loc[7].loc[INDDIR] - z[29]
    )
    return z
