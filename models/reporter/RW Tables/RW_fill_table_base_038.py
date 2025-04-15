# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""

from RW_preprocessor import indcarb


def fill_table_base_038(dfd, table_spec, table_id):
    """Fill table Bulk Chemical Industry Energy Consumption

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
    INDDIR = 9
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
    #   Bulk Chemical Industry Energy Consumption
    #    Shipments, Energy Consumption, and Emissions
    #
    #   Value of Shipments (billion m==m dollars)
    # T38(1,IY,IS)=MC_REVIND(11,15,IY)+MC_REVIND(11,16,IY)+MC_REVIND(11,18,IY)+MC_REVIND(11,19,IY)
    z[1] = (
        dfd["MC_REVIND"].loc[15, MNUMCR]
        + dfd["MC_REVIND"].loc[16, MNUMCR]
        + dfd["MC_REVIND"].loc[18, MNUMCR]
        + dfd["MC_REVIND"].loc[19, MNUMCR]
    )
    #   Energy Consumption (trillion Btu) 1/
    #     Heat and Power
    #       Residual Fuel Oil
    # T38(2,IY,IS)=CHEMCON(IXRF,5,IY)
    z[2] = dfd["CHEMCON"].loc[IXRF].loc[5]
    #       Distillate Fuel Oil
    # T38(3,IY,IS)=CHEMCON(IXDS,5,IY)
    z[3] = dfd["CHEMCON"].loc[IXDS].loc[5]
    #       Propane
    # T38(4,IY,IS)=CHEMCON(IXLG,5,IY)
    z[4] = dfd["CHEMCON"].loc[IXLG].loc[5]
    #       Petroleum Coke
    # T38(5,IY,IS)=CHEMCON(IXPC,5,IY)
    z[5] = dfd["CHEMCON"].loc[IXPC].loc[5]
    #       Other Petroleum 2/
    # T38(6,IY,IS)=CHEMCON(IXOP,5,IY)
    z[6] = dfd["CHEMCON"].loc[IXOP].loc[5]
    #         Petroleum and Other Liquids Subtotal
    # T38(51,IY,IS)=FSUM(T38(2,IY,IS),5)
    z[51] = z[2] + z[3] + z[4] + z[5] + z[6]
    #       Natural Gas
    # T38(7,IY,IS)=CHEMCON(IXNG,5,IY)
    z[7] = dfd["CHEMCON"].loc[IXNG].loc[5]
    #       Steam Coal
    # T38(8,IY,IS)=CHEMCON(IXCL,5,IY)
    z[8] = dfd["CHEMCON"].loc[IXCL].loc[5]
    #       Renewables
    # T38(53,IY,IS)=CHEMCON(IXRN,5,IY)
    z[53] = dfd["CHEMCON"].loc[IXRN].loc[5]
    #       Purchased Electricity
    # T38(9,IY,IS)=CHEMCON(IXEL,5,IY)
    z[9] = dfd["CHEMCON"].loc[IXEL].loc[5]
    #         Total Heat and Power
    # T38(10,IY,IS)=FSUM(T38(2,IY,IS),8)+T38(53,IY,IS)
    z[10] = z[51] + z[7] + z[8] + z[9] + z[53]
    #
    #     Feedstock
    #       Hydrocarbon Gas Liquid Feedstocks 3/
    # T38(11,IY,IS)=CHEMCON(IXLF,5,IY)
    z[11] = dfd["CHEMCON"].loc[IXLF].loc[5]
    #         Natural Gasoline
    # T38(91,IY,IS)=QPPINPF(MNUMCR,IY)*1000.
    z[91] = dfd["QPPINPF"].loc[MNUMCR]
    #         Ethane
    # T38(92,IY,IS)=QETINPF(MNUMCR,IY)*1000.
    z[92] = dfd["QETINPF"].loc[MNUMCR]
    #         Propane
    # T38(93,IY,IS)=QPRINPF(MNUMCR,IY)*1000.
    z[93] = dfd["QPRINPF"].loc[MNUMCR]
    #         Propylene
    # T38(94,IY,IS)=QPROLENERF(MNUMCR,IY)*1000.
    z[94] = dfd["QPROLENERF"].loc[MNUMCR]
    #         Normal Butane
    # T38(95,IY,IS)=QBUINPF(MNUMCR,IY)*1000.
    z[95] = dfd["QBUINPF"].loc[MNUMCR]
    #         Isobutane
    # T38(96,IY,IS)=QISINPF(MNUMCR,IY)*1000.
    z[96] = dfd["QISINPF"].loc[MNUMCR]
    
    # total Hydrocarbon Gas Liquid Feedstocks 3/
    z[11] = z[91] + z[92] + z[93] + z[94] + z[95] + z[96]   
    
    #       Petrochemical Feedstocks
    # T38(12,IY,IS)=CHEMCON(IXPF,5,IY)*1.
    z[12] = dfd["CHEMCON"].loc[IXPF].loc[5]
    #       Natural Gas
    # T38(13,IY,IS)=CHEMCON(IXNF,5,IY)
    z[13] = dfd["CHEMCON"].loc[IXNF].loc[5]
    #       Hydrogen Consumed as Feedstocks 4/
    z[97] = dfd["CHEMCON"].loc[IXHF].loc[5]
    #         Total Feedstocks
    # T38(14,IY,IS)=FSUM(T38(11,IY,IS),3)
    z[14] = z[11] + z[12] + z[13] + z[97]
    #     Total
    # T38(15,IY,IS)=T38(10,IY,IS)+T38(14,IY,IS)
    z[15] = z[10] + z[14]
    #
    #   Energy Consumption per Unit of Output 1/
    #   (thousand Btu per m==m dollar shipments)
    #     Heat and Power
    #       Residual Fuel Oil
    # T38(16,IY,IS)=CHEMCON(IXRF,5,IY)/T38(1,IY,IS)
    z[16] = dfd["CHEMCON"].loc[IXRF].loc[5] / z[1]
    #       Distillate Fuel Oil
    # T38(17,IY,IS)=CHEMCON(IXDS,5,IY)/T38(1,IY,IS)
    z[17] = dfd["CHEMCON"].loc[IXDS].loc[5] / z[1]
    #       Propane
    # T38(18,IY,IS)=CHEMCON(IXLG,5,IY)/T38(1,IY,IS)
    z[18] = dfd["CHEMCON"].loc[IXLG].loc[5] / z[1]
    #       Petroleum Coke
    # T38(19,IY,IS)=CHEMCON(IXPC,5,IY)/T38(1,IY,IS)
    z[19] = dfd["CHEMCON"].loc[IXPC].loc[5] / z[1]
    #       Other Petroleum 2/
    # T38(20,IY,IS)=CHEMCON(IXOP,5,IY)/T38(1,IY,IS)
    z[20] = dfd["CHEMCON"].loc[IXOP].loc[5] / z[1]
    #         Petroleum and Other Liquids Subtotal
    # T38(52,IY,IS)=FSUM(T38(16,IY,IS),5)
    z[52] = z[16] + z[17] + z[18] + z[19] + z[20]
    #       Natural Gas
    # T38(21,IY,IS)=CHEMCON(IXNG,5,IY)/T38(1,IY,IS)
    z[21] = dfd["CHEMCON"].loc[IXNG].loc[5] / z[1]
    #       Steam Coal
    # T38(22,IY,IS)=CHEMCON(IXCL,5,IY)/T38(1,IY,IS)
    z[22] = dfd["CHEMCON"].loc[IXCL].loc[5] / z[1]
    #       Renewables
    # T38(54,IY,IS)=CHEMCON(IXRN,5,IY)/T38(1,IY,IS)
    z[54] = dfd["CHEMCON"].loc[IXRN].loc[5] / z[1]
    #       Purchased Electricity
    # T38(23,IY,IS)=CHEMCON(IXEL,5,IY)/T38(1,IY,IS)
    z[23] = dfd["CHEMCON"].loc[IXEL].loc[5] / z[1]
    #         Total Heat and Power
    # T38(24,IY,IS)=FSUM(T38(16,IY,IS),8)+T38(54,IY,IS)
    z[24] = z[52] + z[21] + z[22] + z[23] + z[54]
    #
    #     Feedstocks
    #       Hydrocarbon Gas Liquid Feedstocks 3/
    # T38(25,IY,IS)=CHEMCON(IXLF,5,IY)/T38(1,IY,IS)
    z[25] = dfd["CHEMCON"].loc[IXLF].loc[5] / z[1]
    #       Petrochemical Feedstocks
    # T38(26,IY,IS)=CHEMCON(IXPF,5,IY)/T38(1,IY,IS)
    z[26] = dfd["CHEMCON"].loc[IXPF].loc[5] / z[1]
    #       Natural Gas
    # T38(27,IY,IS)=CHEMCON(IXNF,5,IY)/T38(1,IY,IS)
    z[27] = dfd["CHEMCON"].loc[IXNF].loc[5] / z[1]
    #       Hydrogen Consumed as Feedstocks 4/
    z[98] = dfd["CHEMCON"].loc[IXHF].loc[5] / z[1]
    #         Total Feedstocks
    # T38(28,IY,IS)=FSUM(T38(25,IY,IS),3)
    z[28] = z[25] + z[26] + z[27] + z[98]
    #     Total
    # T38(29,IY,IS)=T38(24,IY,IS)+T38(28,IY,IS)
    z[29] = z[24] + z[28]
    
    # Energy Consumption for Hydrogen Production (trillion Btu) 5/
    #   Heat and Power
    #       Natural Gas
    z[99] = dfd["H2CON"].loc[IXNG].loc[5]
    #       Purchased Electricity
    z[100] = dfd["H2CON"].loc[IXEL].loc[5]
    #           Total Heat and Power
    z[101] = z[99] + z[100]
    #   Feedstocks
    #       Natural Gas
    z[102] = dfd["H2CON"].loc[IXNF].loc[5]
    #           Total Feedstocks
    z[103] = z[102]
    #   Total
    z[104] = z[101] + z[103]
    
    # Carbon Dioxide Emissions 6/
    # (million metric tons CO2)
    z[30] = (
        indcarb(dfd, dfd["CHEMCON"], CARBON_OR_2)
        + indcarb(dfd, dfd["H2CON"], CARBON_OR_2)
        # Captured carbon from steam methane reforming
        - dfd["CC_INDY"].loc[(9, 11), :] * dfd['C_CO2_FACTOR_rwpre'] / 1000.0
    )

    #   Combined Heat and Power 5/
    #     Generating Capacity (gigawatts) 6/
    #       Petroleum
    # T38(32,IY,IS)=CHPINDCAP(2,inddir,iy)
    z[32] = dfd["CHPINDCAP"].loc[2].loc[INDDIR]
    #       Natural Gas
    # T38(33,IY,IS)=CHPINDCAP(3,inddir,iy)
    z[33] = dfd["CHPINDCAP"].loc[3].loc[INDDIR]
    #       Coal
    # T38(31,IY,IS)=CHPINDCAP(1,inddir,iy)
    z[31] = dfd["CHPINDCAP"].loc[1].loc[INDDIR]
    #       Other 7/
    # T38(34,IY,IS)=sum(CHPINDCAP(4:6,inddir,iy))
    z[34] = dfd["CHPINDCAP"].loc[((4, 5, 6), INDDIR), :].sum()
    #         Total
    # T38(35,IY,IS)=CHPINDCAP(7,inddir,iy)
    z[35] = dfd["CHPINDCAP"].loc[7].loc[INDDIR]
    #     Net Generation (billion kilowatthours)
    #       Petroleum
    # T38(37,IY,IS)=CHPINDGEN(2,inddir,iy)
    z[37] = dfd["CHPINDGEN"].loc[2].loc[INDDIR]
    #       Natural Gas
    # T38(38,IY,IS)=CHPINDGEN(3,inddir,iy)
    z[38] = dfd["CHPINDGEN"].loc[3].loc[INDDIR]
    #       Coal
    # T38(36,IY,IS)=CHPINDGEN(1,inddir,iy)
    z[36] = dfd["CHPINDGEN"].loc[1].loc[INDDIR]
    #       Other 7/
    # T38(39,IY,IS)=sum(CHPINDGEN(4:6,inddir,iy))
    z[39] = dfd["CHPINDGEN"].loc[((4, 5, 6), INDDIR), :].sum()
    #         Total
    # T38(40,IY,IS)=CHPINDGEN(7,inddir,iy)
    z[40] = dfd["CHPINDGEN"].loc[7].loc[INDDIR]
    #       Disposition
    #         Sales to the Grid
    # T38(41,IY,IS)=CHPINDGEN(7,inddir,iy)*    CHPGRDSHR(inddir,iy)
    z[41] = dfd["CHPINDGEN"].loc[7].loc[INDDIR] * dfd["CHPGRDSHR"].loc[INDDIR]
    #         Generation for Own Use
    # T38(42,IY,IS)=CHPINDGEN(7,inddir,iy)*(1.-CHPGRDSHR(inddir,iy))
    z[42] = dfd["CHPINDGEN"].loc[7].loc[INDDIR] - z[41]
    return z
