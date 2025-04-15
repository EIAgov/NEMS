# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""
from RW_preprocessor import indcarb


def fill_table_base_040(dfd, table_spec, table_id):
    """Fill table Cement and Lime Industry Energy Consumption

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
    INDDIR = 11
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
    #   Cement and Lime Industry Energy Consumption
    #    Shipments, Energy Consumption, and Emissions
    #
    #   Value of Shipments (billion m==m dollars)
    # T40(1,IY,IS)=MC_REVIND(11,30,IY)+MC_REVIND(11,31,IY)
    z[1] = dfd["MC_REVIND"].loc[30].loc[MNUMCR] + dfd["MC_REVIND"].loc[31].loc[MNUMCR]
    #
    #   Energy Consumption (trillion Btu) 1/
    #    Distillate Fuel Oil
    # T40(3,IY,IS)=CEMENTCON(IXDS,5,IY)
    z[3] = dfd["CEMENTCON"].loc[IXDS].loc[5]
    #    Residual Fuel Oil
    # T40(2,IY,IS)=CEMENTCON(IXRF,5,IY)
    z[2] = dfd["CEMENTCON"].loc[IXRF].loc[5]
    #    Propane
    # T40(45,IY,IS)=CEMENTCON(IXLG,5,IY)
    z[45] = dfd["CEMENTCON"].loc[IXLG].loc[5]
    #    Petroleum Coke
    # T40(39,IY,IS)=CEMENTCON(IXPC,5,IY)
    z[39] = dfd["CEMENTCON"].loc[IXPC].loc[5]
    #    Other Petroleum 2/
    # T40(4,IY,IS)=CEMENTCON(IXOP,5,IY)
    z[4] = dfd["CEMENTCON"].loc[IXOP].loc[5]
    #      Petroleum and Other Liquids Subtotal
    # T40(5,IY,IS)=FSUM(T40(2,IY,IS),3)+T40(39,IY,IS)+T40(45,IY,IS)
    z[5] = z[2] + z[3] + z[4] + z[39] + z[45]
    #    Natural Gas
    # T40(6,IY,IS)=CEMENTCON(IXNG,5,IY)
    z[6] = dfd["CEMENTCON"].loc[IXNG].loc[5]
    #    Steam Coal
    # T40(7,IY,IS)=CEMENTCON(IXCL,5,IY)
    z[7] = dfd["CEMENTCON"].loc[IXCL].loc[5]
    #    Metallurgical Coal
    # T40(40,IY,IS)=CEMENTCON(IXMC,5,IY)
    z[40] = dfd["CEMENTCON"].loc[IXMC].loc[5]
    #      Coal Subtotal
    # T40(41,IY,IS)=T40(7,IY,IS)+T40(40,IY,IS)
    z[41] = z[7] + z[40]
    #    Renewables
    # T40(37,IY,IS)=CEMENTCON(IXRN,5,IY)
    z[37] = dfd["CEMENTCON"].loc[IXRN].loc[5]
    #    Purchased Electricity
    # T40(8,IY,IS)=CEMENTCON(IXEL,5,IY)
    z[8] = dfd["CEMENTCON"].loc[IXEL].loc[5]
    #      Total
    # T40(9,IY,IS)=FSUM(T40(5,IY,IS),4)+T40(40,IY,IS)+T40(37,IY,IS)
    z[9] = z[5] + z[6] + z[7] + z[8] + z[40] + z[37]
    #
    #   Energy Consumption per Unit of Output 1/
    #   (thousand Btu per m==m dollar shipments)
    #    Distillate Fuel Oil
    # T40(11,IY,IS)=CEMENTCON(IXDS,5,IY)/(MC_REVIND(11,30,IY)+MC_REVIND(11,31,IY))
    z[11] = dfd["CEMENTCON"].loc[IXDS].loc[5] / z[1]
    #    Residual Fuel Oil
    # T40(10,IY,IS)=CEMENTCON(IXRF,5,IY)/(MC_REVIND(11,30,IY)+MC_REVIND(11,31,IY))
    z[10] = dfd["CEMENTCON"].loc[IXRF].loc[5] / z[1]
    #    Propane
    # T40(46,IY,IS)=CEMENTCON(IXLG,5,IY)/(MC_REVIND(11,30,IY)+MC_REVIND(11,31,IY))
    z[46] = dfd["CEMENTCON"].loc[IXLG].loc[5] / z[1]
    #    Petroleum Coke
    # T40(42,IY,IS)=CEMENTCON(IXPC,5,IY)/(MC_REVIND(11,30,IY)+MC_REVIND(11,31,IY))
    z[42] = dfd["CEMENTCON"].loc[IXPC].loc[5] / z[1]
    #    Other Petroleum 2/
    # T40(12,IY,IS)=CEMENTCON(IXOP,5,IY)/(MC_REVIND(11,30,IY)+MC_REVIND(11,31,IY))
    z[12] = dfd["CEMENTCON"].loc[IXOP].loc[5] / z[1]
    #      Petroleum and Other Liquids Subtotal
    # T40(13,IY,IS)=FSUM(T40(10,IY,IS),3)+T40(42,IY,IS)+T40(46,IY,IS)
    z[13] = z[10] + z[11] + z[12] + z[42] + z[46]
    #    Natural Gas
    # T40(14,IY,IS)=CEMENTCON(IXNG,5,IY)/(MC_REVIND(11,30,IY)+MC_REVIND(11,31,IY))
    z[14] = dfd["CEMENTCON"].loc[IXNG].loc[5] / z[1]
    #    Steam Coal
    # T40(15,IY,IS)=CEMENTCON(IXCL,5,IY)/(MC_REVIND(11,30,IY)+MC_REVIND(11,31,IY))
    z[15] = dfd["CEMENTCON"].loc[IXCL].loc[5] / z[1]
    #    Metallurgical Coal
    # T40(43,IY,IS)=CEMENTCON(IXMC,5,IY)/(MC_REVIND(11,30,IY)+MC_REVIND(11,31,IY))
    z[43] = dfd["CEMENTCON"].loc[IXMC].loc[5] / z[1]
    #      Coal Subtotal
    # T40(44,IY,IS)=T40(15,IY,IS)+T40(43,IY,IS)
    z[44] = z[15] + z[43]
    #    Renewables
    # T40(38,IY,IS)=CEMENTCON(IXRN,5,IY)/(MC_REVIND(11,30,IY)+MC_REVIND(11,31,IY))
    z[38] = dfd["CEMENTCON"].loc[IXRN].loc[5] / z[1]
    #    Purchased Electricity
    # T40(16,IY,IS)=CEMENTCON(IXEL,5,IY)/(MC_REVIND(11,30,IY)+MC_REVIND(11,31,IY))
    z[16] = dfd["CEMENTCON"].loc[IXEL].loc[5] / z[1]
    #      Total
    # T40(17,IY,IS)=FSUM(T40(13,IY,IS),4)+T40(43,IY,IS)+T40(38,IY,IS)
    z[17] = z[13] + z[14] + z[15] + z[16] + z[43] + z[38]
    #
    #   Carbon Dioxide Emissions
    #       Combustion (million metric tons) 3/

# combustion emissions minus the sequestered emissions
    z[18] = indcarb(dfd, dfd["CEMENTCON"], CARBON_OR_2) - ( 
                     dfd['CC_INDY'].loc[(1, 11), :]  # Distillate
                   + dfd['CC_INDY'].loc[(2, 11), :]  # Petroleum coke
                   + dfd['CC_INDY'].loc[(4, 11), :]  # Other petroleum
                   + dfd['CC_INDY'].loc[(5, 11), :]  # Steam coal
                   + dfd['CC_INDY'].loc[(8, 11), :]  # Natural gas
                     ) * dfd['C_CO2_FACTOR_rwpre'] / 1000.0
    #       Process (million metric tons) 4/
    # T40(47,IY,IS)=GHG_PROCESS(11,IY)
# total national process emissions from cement & lime industry
    process = dfd["GHG_PROCESSIN"].loc[11]
# get total national captured CO2 from cement plants
    coototalseq = (dfd['CO2_SUP_OUT'].loc[1].loc[5])/1000000 + \
                  (dfd['CO2_SUP_OUT'].loc[2].loc[5])/1000000 + \
                  (dfd['CO2_SUP_OUT'].loc[3].loc[5])/1000000 + \
                  (dfd['CO2_SUP_OUT'].loc[4].loc[5])/1000000 + \
                  (dfd['CO2_SUP_OUT'].loc[5].loc[5])/1000000 + \
                  (dfd['CO2_SUP_OUT'].loc[6].loc[5])/1000000 + \
                  (dfd['CO2_SUP_OUT'].loc[7].loc[5])/1000000 + \
                  (dfd['CO2_SUP_OUT'].loc[8].loc[5])/1000000 + \
                  (dfd['CO2_SUP_OUT'].loc[9].loc[5])/1000000
# z{47} is process emisssions emitted after capture 
    z[47] = process - (coototalseq - ( 
                     dfd['CC_INDY'].loc[(1, 11), :]  # Distillate
                   + dfd['CC_INDY'].loc[(2, 11), :]  # Petroleum coke
                   + dfd['CC_INDY'].loc[(4, 11), :]  # Other petroleum
                   + dfd['CC_INDY'].loc[(5, 11), :]  # Steam coal
                   + dfd['CC_INDY'].loc[(8, 11), :]  # Natural gas
                     ) * dfd['C_CO2_FACTOR_rwpre'] / 1000.0 )

    #   Combined Heat and Power 5/
    #     Generating Capacity (gigawatts) 6/
    #       Petroleum
    # T40(20,IY,IS)=CHPINDCAP(2,inddir,iy)
    z[20] = dfd["CHPINDCAP"].loc[2].loc[INDDIR]
    #       Natural Gas
    # T40(21,IY,IS)=CHPINDCAP(3,inddir,iy)
    z[21] = dfd["CHPINDCAP"].loc[3].loc[INDDIR]
    #       Coal
    # T40(19,IY,IS)=CHPINDCAP(1,inddir,iy)
    z[19] = dfd["CHPINDCAP"].loc[1].loc[INDDIR]
    #       Other 7/
    # T40(22,IY,IS)=sum(CHPINDCAP(4:6,inddir,iy))
    z[22] = dfd["CHPINDCAP"].loc[((4, 5, 6), INDDIR), :].sum()
    #         Total
    # T40(23,IY,IS)=CHPINDCAP(7,inddir,iy)
    z[23] = dfd["CHPINDCAP"].loc[7].loc[INDDIR]
    #     Net Generation (billion kilowatthours)
    #       Petroleum
    # T40(25,IY,IS)=CHPINDGEN(2,inddir,iy)
    z[25] = dfd["CHPINDGEN"].loc[2].loc[INDDIR]
    #       Natural Gas
    # T40(26,IY,IS)=CHPINDGEN(3,inddir,iy)
    z[26] = dfd["CHPINDGEN"].loc[3].loc[INDDIR]
    #       Coal
    # T40(24,IY,IS)=CHPINDGEN(1,inddir,iy)
    z[24] = dfd["CHPINDGEN"].loc[1].loc[INDDIR]
    #       Other 7/
    # T40(27,IY,IS)=sum(CHPINDGEN(4:6,inddir,iy))
    z[27] = dfd["CHPINDGEN"].loc[((4, 5, 6), INDDIR), :].sum()
    #         Total
    # T40(28,IY,IS)=CHPINDGEN(7,inddir,iy)
    z[28] = dfd["CHPINDGEN"].loc[7].loc[INDDIR]
    #       Disposition
    #         Sales to the Grid
    # T40(29,IY,IS)=CHPINDGEN(7,inddir,iy)*    CHPGRDSHR(inddir,iy)
    z[29] = dfd["CHPINDGEN"].loc[7].loc[INDDIR] * dfd["CHPGRDSHR"].loc[INDDIR]
    #         Generation for Own Use
    # T40(30,IY,IS)=CHPINDGEN(7,inddir,iy)*(1.-CHPGRDSHR(inddir,iy))
    z[30] = dfd["CHPINDGEN"].loc[7].loc[INDDIR] - z[29]
    return z
