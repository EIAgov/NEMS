# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
Fixed by SZO on 7/2/20124
"""
from RW_preprocessor import indcarb


def fill_table_base_140(dfd, table_spec, table_id):
    """Fill table for Other Manufacturing Industry Energy Consumption

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



    """

    z = {}

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
    IXRN = 18

    CARBON_OR_2 = table_spec["settings"]["carbon_or_carbondioxide"].upper()

    C_CO2_FACTOR = dfd["C_CO2_FACTOR_rwpre"] if CARBON_OR_2 == "CO2" else 1

    #   Other Manufacturing Industry Energy Consumption
    #    Shipments, Energy Consumption, and Emissions
    #
    #   Energy Consumption (trillion Btu)

    #      Wood Products Consumption 1/

    #         Propane
    # T140(3,IY,IS)=WOODPRODCON(IXLG,5,IY)
    z[3] = dfd["WOODPRODCON"].loc[IXLG].loc[5]

    #         Distillate Fuel Oil
    # T140(2,IY,IS)=WOODPRODCON(IXDS,5,IY)
    z[2] = dfd["WOODPRODCON"].loc[IXDS].loc[5]

    #         Residual Fuel Oil
    # T140(1,IY,IS)=WOODPRODCON(IXRF,5,IY)
    z[1] = dfd["WOODPRODCON"].loc[IXRF].loc[5]

    #         Petroleum and Other Liquids Subtotal
    # T140(4,IY,IS)=FSUM(T140(1,IY,IS),3)
    z[4] = z[1] + z[2] + z[3]

    #         Natural Gas
    # T140(5,IY,IS)=WOODPRODCON(IXNG,5,IY)
    z[5] = dfd["WOODPRODCON"].loc[IXNG].loc[5]

    #         Steam Coal
    # T140(6,IY,IS)=WOODPRODCON(IXCL,5,IY)
    z[6] = dfd["WOODPRODCON"].loc[IXCL].loc[5]

    #         Renewables
    # T140(8,IY,IS)=WOODPRODCON(ixRN,5,IY)
    z[8] = dfd["WOODPRODCON"].loc[IXRN].loc[5]

    #         Purchased Electricity
    # T140(9,IY,IS)=WOODPRODCON(IXEL,5,IY)
    z[9] = dfd["WOODPRODCON"].loc[IXEL].loc[5]

    #            Total
    # T140(10,IY,IS)=FSUM(T140(4,IY,IS),6)
    z[10] = z[4] + z[5] + z[6] + z[8] + z[9]

    #

    #      Plastics Consumption 1/

    #         Propane
    # T140(13,IY,IS)=PLASTICCON(IXLG,5,IY)
    z[13] = dfd["PLASTICCON"].loc[IXLG].loc[5]

    #         Distillate Fuel Oil
    # T140(12,IY,IS)=PLASTICCON(IXDS,5,IY)
    z[12] = dfd["PLASTICCON"].loc[IXDS].loc[5]

    #         Residual Fuel Oil
    # T140(11,IY,IS)=PLASTICCON(IXRF,5,IY)
    z[11] = dfd["PLASTICCON"].loc[IXRF].loc[5]

    #            Petroleum and Other Liquids Subtotal
    # T140(14,IY,IS)=FSUM(T140(11,IY,IS),3)
    z[14] = z[11] + z[12] + z[13]

    #         Natural Gas
    # T140(15,IY,IS)=PLASTICCON(IXNG,5,IY)
    z[15] = dfd["PLASTICCON"].loc[IXNG].loc[5]

    #         Steam Coal
    # T140(16,IY,IS)=PLASTICCON(IXCL,5,IY)
    z[16] = dfd["PLASTICCON"].loc[IXCL].loc[5]

    #         Renewables
    # T140(18,IY,IS)=PLASTICCON(ixRN,5,IY)
    z[18] = dfd["PLASTICCON"].loc[IXRN].loc[5]

    #         Purchased Electricity
    # T140(19,IY,IS)=PLASTICCON(IXEL,5,IY)
    z[19] = dfd["PLASTICCON"].loc[IXEL].loc[5]

    #            Total
    # T140(20,IY,IS)=FSUM(T140(14,IY,IS),6)
    z[20] = z[14] + z[15] + z[16] + z[18] + z[19]

    #

    #      Light Chemicals Consumption 1/

    #         Propane
    # T140(23,IY,IS)=LTCHEMCON(IXLG,5,IY)
    z[23] = dfd["LTCHEMCON"].loc[IXLG].loc[5]

    #         Distillate Fuel Oil
    # T140(22,IY,IS)=LTCHEMCON(IXDS,5,IY)
    z[22] = dfd["LTCHEMCON"].loc[IXDS].loc[5]

    #         Residual Fuel Oil
    # T140(21,IY,IS)=LTCHEMCON(IXRF,5,IY)
    z[21] = dfd["LTCHEMCON"].loc[IXRF].loc[5]

    #         Petroleum Coke
    # T140(96,IY,IS)=LTCHEMCON(IXPC,5,IY)
    z[96] = dfd["LTCHEMCON"].loc[11].loc[5]

    #         Other Petroleum 2/
    # T140(24,IY,IS)=LTCHEMCON(IXOP,5,IY)
    z[24] = dfd["LTCHEMCON"].loc[IXOP].loc[5]

    #         Petroleum and Other Liquids Subtotal
    # T140(25,IY,IS)=FSUM(T140(21,IY,IS),4)+FSUM(T140(96,IY,IS),1)
    z[25] = z[21] + z[22] + z[23] + z[24] + z[96]

    #         Natural Gas
    # T140(26,IY,IS)=LTCHEMCON(IXNG,5,IY)
    z[26] = dfd["LTCHEMCON"].loc[IXNG].loc[5]

    #         Steam Coal
    # T140(27,IY,IS)=LTCHEMCON(IXCL,5,IY)
    z[27] = dfd["LTCHEMCON"].loc[IXCL].loc[5]

    #         Renewables
    # T140(29,IY,IS)=LTCHEMCON(ixRN,5,IY)
    z[29] = dfd["LTCHEMCON"].loc[IXRN].loc[5]

    #         Purchased Electricity
    # T140(30,IY,IS)=LTCHEMCON(IXEL,5,IY)
    z[30] = dfd["LTCHEMCON"].loc[IXEL].loc[5]

    #            Total
    # T140(31,IY,IS)=FSUM(T140(25,IY,IS),6)
    z[31] = z[25] + z[26] + z[27] + z[29] + z[30]

    #

    #      Other Non-Metallic Minerals Consumption 1/

    #         Propane
    # T140(34,IY,IS)=OTHRNMMCON(IXLG,5,IY)
    z[34] = dfd["OTHRNMMCON"].loc[IXLG].loc[5]

    #         Distillate Fuel Oil
    # T140(33,IY,IS)=OTHRNMMCON(IXDS,5,IY)
    z[33] = dfd["OTHRNMMCON"].loc[IXDS].loc[5]

    #         Residual Fuel Oil
    # T140(32,IY,IS)=OTHRNMMCON(IXRF,5,IY)
    z[32] = dfd["OTHRNMMCON"].loc[IXRF].loc[5]

    #         Petroleum Coke
    # T140(97,IY,IS)=OTHRNMMCON(IXPC,5,IY)
    z[97] = dfd["OTHRNMMCON"].loc[IXPC].loc[5]

    #         Other Petroleum 2/
    # T140(35,IY,IS)=OTHRNMMCON(IXOP,5,IY)
    z[35] = dfd["OTHRNMMCON"].loc[IXOP].loc[5]

    #          Petroleum and Other Liquids Subtotal
    # T140(36,IY,IS)=FSUM(T140(32,IY,IS),4)+FSUM(T140(97,IY,IS),1)
    z[36] = z[32] + z[33] + z[34] + z[35] + z[97]

    #         Natural Gas
    # T140(37,IY,IS)=OTHRNMMCON(IXNG,5,IY)
    z[37] = dfd["OTHRNMMCON"].loc[IXNG].loc[5]

    #         Steam Coal
    # T140(38,IY,IS)=OTHRNMMCON(IXCL,5,IY)
    z[38] = dfd["OTHRNMMCON"].loc[IXCL].loc[5]

    #         Renewables
    # T140(40,IY,IS)=OTHRNMMCON(ixRN,5,IY)
    z[40] = dfd["OTHRNMMCON"].loc[IXRN].loc[5]

    #         Purchased Electricity
    # T140(41,IY,IS)=OTHRNMMCON(IXEL,5,IY
    z[41] = dfd["OTHRNMMCON"].loc[IXEL].loc[5]

    #            Total
    # T140(42,IY,IS)=FSUM(T140(36,IY,IS),6)
    z[42] = z[36] + z[37] + z[38] + z[40] + z[41]

    #

    #     Other Primary Metals Consumption 1/

    #         Propane
    # T140(45,IY,IS)=OTHRPRIMCON(IXLG,5,IY)
    z[45] = dfd["OTHRPRIMCON"].loc[IXLG].loc[5]

    #         Distillate Fuel Oil
    # T140(44,IY,IS)=OTHRPRIMCON(IXDS,5,IY)
    z[44] = dfd["OTHRPRIMCON"].loc[IXDS].loc[5]

    #         Residual Fuel Oil
    # T140(43,IY,IS)=OTHRPRIMCON(IXRF,5,IY)
    z[43] = dfd["OTHRPRIMCON"].loc[IXRF].loc[5]

    #         Petroleum Coke
    # T140(98,IY,IS)=OTHRPRIMCON(IXPC,5,IY)
    z[98] = dfd["OTHRPRIMCON"].loc[IXPC].loc[5]

    #         Other Petroleum 2/
    # T140(46,IY,IS)=OTHRPRIMCON(IXOP,5,IY)
    z[46] = dfd["OTHRPRIMCON"].loc[IXOP].loc[5]

    #          Petroleum and Other Liquids Subtotal
    # T140(47,IY,IS)=FSUM(T140(43,IY,IS),4)+FSUM(T140(98,IY,IS),1)
    z[47] = z[43] + z[44] + z[45] + z[46] + z[98]

    #         Natural Gas
    # T140(48,IY,IS)=OTHRPRIMCON(IXNG,5,IY)
    z[48] = dfd["OTHRPRIMCON"].loc[IXNG].loc[5]

    #         Steam Coal
    # T140(49,IY,IS)=OTHRPRIMCON(IXCL,5,IY)
    z[49] = dfd["OTHRPRIMCON"].loc[IXCL].loc[5]

    #         Renewables
    # T140(51,IY,IS)=OTHRPRIMCON(ixRN,5,IY)
    z[51] = dfd["OTHRPRIMCON"].loc[IXRN].loc[5]

    #         Purchased Electricity
    # T140(52,IY,IS)=OTHRPRIMCON(IXEL,5,IY)
    z[52] = dfd["OTHRPRIMCON"].loc[IXEL].loc[5]

    #            Total
    # T140(53,IY,IS)=FSUM(T140(47,IY,IS),6)
    z[53] = z[47] + z[48] + z[49] + z[51] + z[52]

    #

    #      Miscellaneous Finished Goods Consumption 1/

    #         Propane
    # T140(56,IY,IS)=MISCFINCON(IXLG,5,IY)
    z[56] = dfd["MISCFINCON"].loc[IXLG].loc[5]

    #         Distillate Fuel Oil
    # T140(55,IY,IS)=MISCFINCON(IXDS,5,IY)
    z[55] = dfd["MISCFINCON"].loc[IXDS].loc[5]

    #         Residual Fuel Oil
    # T140(54,IY,IS)=MISCFINCON(IXRF,5,IY)
    z[54] = dfd["MISCFINCON"].loc[IXRF].loc[5]

    #         Asphalt
    # T140(57,IY,IS)=MISCFINCON(IXAS,5,IY)
    z[57] = dfd["MISCFINCON"].loc[IXAS].loc[5]

    #         Petroleum Coke
    # T140(99,IY,IS)=MISCFINCON(IXPC,5,IY)
    z[99] = dfd["MISCFINCON"].loc[IXPC].loc[5]

    #         Other Petroleum 2/
    # T140(58,IY,IS)=MISCFINCON(IXOP,5,IY)
    z[58] = dfd["MISCFINCON"].loc[IXOP].loc[5]

    #         Petroleum and Other Liquids Subtotal
    # T140(59,IY,IS)=FSUM(T140(54,IY,IS),5)+FSUM(T140(99,IY,IS),1)
    z[59] = z[54] + z[55] + z[56] + z[57] + z[58] + z[99]

    #         Natural Gas
    # T140(60,IY,IS)=MISCFINCON(IXNG,5,IY)
    z[60] = dfd["MISCFINCON"].loc[IXNG].loc[5]

    #         Steam Coal
    # T140(61,IY,IS)=MISCFINCON(IXCL,5,IY)
    z[61] = dfd["MISCFINCON"].loc[IXCL].loc[5]

    #         Renewables
    # T140(63,IY,IS)=MISCFINCON(ixRN,5,IY)
    z[63] = dfd["MISCFINCON"].loc[IXRN].loc[5]

    #         Purchased Electricity
    # T140(64,IY,IS)=MISCFINCON(IXEL,5,IY)
    z[64] = dfd["MISCFINCON"].loc[IXEL].loc[5]

    #            Total
    # T140(65,IY,IS)=FSUM(T140(59,IY,IS),6)
    z[65] = z[59] + z[60] + z[61] + z[63] + z[64]

    #

    #

    #

    #   Value of Shipments (billion m==m dollars)

    #      Wood Products
    # T140(66,IY,IS)=MC_REVIND(11,8,IY)
    z[66] = dfd["MC_REVIND"].loc[8].loc[11]

    #      Plastics
    # T140(67,IY,IS)=MC_REVIND(11,27,IY)
    z[67] = dfd["MC_REVIND"].loc[27].loc[11]

    #      Light Chemicals
    # T140(68,IY,IS)=MC_REVIND(11,20,IY)
    z[68] = dfd["MC_REVIND"].loc[20].loc[11]

    #      Other Non-Metallic Minerals
    # T140(69,IY,IS)=MC_REVIND(11,32,IY)
    z[69] = dfd["MC_REVIND"].loc[32].loc[11]

    #      Other Primary Metals
    # T140(70,IY,IS)=MC_REVIND(11,35,IY)
    z[70] = dfd["MC_REVIND"].loc[35].loc[11]

    #      Miscellaneous Finished Goods
    # T140(71,IY,IS) = MC_REVIND(11,6,IY)+MC_REVIND(11,7,IY) + MC_REVIND(11,9,IY)+MC_REVIND(11,14,IY)+MC_REVIND(11,26,IY)*0.91 + MC_REVIND(11,41,IY)
    z[71] = (
        dfd["MC_REVIND"].loc[6].loc[11]
        + dfd["MC_REVIND"].loc[7].loc[11]
        + dfd["MC_REVIND"].loc[9].loc[11]
        + dfd["MC_REVIND"].loc[14].loc[11]
        + dfd["MC_REVIND"].loc[26].loc[11] * 0.91
        + dfd["MC_REVIND"].loc[41].loc[11]
    )

    #

    #   Energy Consumption per Unit of Output 1/

    #   (thousand Btu per m==m dollar shipments)

    #      Wood Products
    # T140(72,IY,IS)=T140(10,IY,IS)/MC_REVIND(11,8,IY)
    z[72] = z[10] / dfd["MC_REVIND"].loc[8].loc[11]

    #      Plastics
    # T140(73,IY,IS)=T140(20,IY,IS)/MC_REVIND(11,27,IY)
    z[73] = z[20] / dfd["MC_REVIND"].loc[27].loc[11]

    #      Light Chemicals
    # T140(74,IY,IS)=T140(31,IY,IS)/MC_REVIND(11,20,IY)
    z[74] = z[31] / dfd["MC_REVIND"].loc[20].loc[11]

    #      Other Non-Metallic Minerals
    # T140(75,IY,IS)=T140(42,IY,IS)/MC_REVIND(11,32,IY)
    z[75] = z[42] / dfd["MC_REVIND"].loc[32].loc[11]

    #      Other Primary Metals
    # T140(76,IY,IS)=T140(53,IY,IS)/MC_REVIND(11,35,IY)
    z[76] = z[53] / dfd["MC_REVIND"].loc[35].loc[11]

    #      Miscellaneous Finished Goods
    # T140(77,IY,IS)=T140(65,IY,IS)/((MC_REVIND(11,6,IY)+MC_REVIND(11,7,IY)+MC_REVIND(11,9,IY)+MC_REVIND(11,14,IY)+MC_REVIND(11,20,IY)+MC_REVIND(11,26,IY)*0.91+MC_REVIND(11,31,IY)*0.72+MC_REVIND(11,41,IY))
    # z[77] = z[65] / (dfd['MC_REVIND'].loc[11].loc[6] + dfd['MC_REVIND'].loc[11].loc[7] + \
    #                  dfd['MC_REVIND'].loc[11].loc[9] + dfd['MC_REVIND'].loc[11].loc[14] + \
    #                  dfd['MC_REVIND'].loc[11].loc[20] + dfd['MC_REVIND'].loc[11].loc[26]*0.91 + \
    #                  dfd['MC_REVIND'].loc[11].loc[31]*0.72 + dfd['MC_REVIND'].loc[11].loc[41])
    #      Miscellaneous Finished Goods
    # T140(77,IY,IS) = T140(65,IY,IS)/(MC_REVIND(11,6,IY)+MC_REVIND(11,7,IY)+MC_REVIND(11,9,IY) + &
    #                   MC_REVIND(11,14,IY)+MC_REVIND(11,20,IY)+MC_REVIND(11,26,IY)*0.91 + &
    #          MC_REVIND(11,41,IY))
    z[77] = z[65] / (
        dfd["MC_REVIND"].loc[6].loc[11]
        + dfd["MC_REVIND"].loc[7].loc[11]
        + dfd["MC_REVIND"].loc[9].loc[11]
        + dfd["MC_REVIND"].loc[14].loc[11]
        + dfd["MC_REVIND"].loc[20].loc[11]
        + dfd["MC_REVIND"].loc[26].loc[11] * 0.91
        + dfd["MC_REVIND"].loc[41].loc[11]
    )

    #

    #   Carbon Dioxide Emissions 3/ (million metric

    #    tons carbon dioxide)

    #      Wood Products
    # T140(100,IY,IS)=INDCARB(WOODPRODCON(1,1,IY),IY)
    z[100] = indcarb(dfd, dfd["WOODPRODCON"], CARBON_OR_2)

    #      Plastics
    # T140(101,IY,IS)=INDCARB(PLASTICCON(1,1,IY),IY)
    z[101] = indcarb(dfd, dfd["PLASTICCON"], CARBON_OR_2)

    #      Light Chemicals
    # T140(102,IY,IS)=INDCARB(LTCHEMCON(1,1,IY),IY)
    z[102] = indcarb(dfd, dfd["LTCHEMCON"], CARBON_OR_2)

    #      Other Non-Metallic Minerals
    # T140(103,IY,IS)=INDCARB(OTHRNMMCON(1,1,IY),IY)
    z[103] = indcarb(dfd, dfd["OTHRNMMCON"], CARBON_OR_2)

    #      Other Primary Metals
    # T140(104,IY,IS)=INDCARB(OTHRPRIMCON(1,1,IY),IY)
    z[104] = indcarb(dfd, dfd["OTHRPRIMCON"], CARBON_OR_2)

    #      Miscellaneous Finished Goods
    # T140(105,IY,IS)=INDCARB(MISCFINCON(1,1,IY),IY)
    z[105] = indcarb(dfd, dfd["MISCFINCON"], CARBON_OR_2)

    #

    #

    #   Other Manufacturing:

    #

    #    Combined Heat and Power 4/

    #     Generating Capacity (gigawatts)
    # Note industry indices only go to 21 as of AEO2025, even though there are 24 industries;
    # CHP data have not yet been updated to use 24 industries
    #       Petroleum
    # T140(107,IY,IS)=sum(CHPINDCAP(2,v888,iy))
    z[107] = dfd["CHPINDCAP"].loc[2, 19:21, :].sum()

    #       Natural Gas
    # T140(108,IY,IS)=sum(CHPINDCAP(3,v888,iy))
    z[108] = dfd["CHPINDCAP"].loc[3, 19:21, :].sum()

    #       Coal
    # T140(106,IY,IS)=sum(CHPINDCAP(1,v888,iy))
    z[106] = dfd["CHPINDCAP"].loc[1, 19:21, :].sum()

    #       Other 5/
    # T140(109,IY,IS)=sum(CHPINDCAP(4:6,v888,iy))
    z[109] = dfd["CHPINDCAP"].loc[4:6, 19:21, :].sum()

    #         Total
    # T140(110,IY,IS)=sum(CHPINDCAP(7,v888,iy))
    z[110] = dfd["CHPINDCAP"].loc[7, 19:21, :].sum()

    #     Net Generation (billion kilowatthours)

    #       Petroleum
    # T140(112,IY,IS)=sum(CHPINDGEN(2,v888,iy))
    z[112] = dfd["CHPINDGEN"].loc[2, 19:21, :].sum()

    #       Natural Gas
    # T140(113,IY,IS)=sum(CHPINDGEN(3,v888,iy))
    z[113] = dfd["CHPINDGEN"].loc[3, 19:21, :].sum()

    #       Coal
    # T140(111,IY,IS)=sum(CHPINDGEN(1,v888,iy))
    z[111] = dfd["CHPINDGEN"].loc[1, 19:21, :].sum()

    #       Other 5/
    # T140(114,IY,IS)=sum(CHPINDGEN(4:6,v888,iy))
    z[114] = dfd["CHPINDGEN"].loc[4:6, 19:21, :].sum()

    #         Total
    # T140(115,IY,IS)=sum(CHPINDGEN(7,v888,iy))
    z[115] = dfd["CHPINDGEN"].loc[7, 19:21, :].sum()

    #       Disposition
    # IL=39
    #      v888=(/19,20,21/)  ! vector subscript , used similarly to triplet 1:6
    #         Sales to the Grid
    # T140(116,IY,IS)=sum(CHPINDGEN(7,v888,iy)*CHPGRDSHR(v888,iy))
    z[116] = z[115] * 0.0
    for x in range(19, 22):
        z[116] += dfd["CHPINDGEN"].loc[7, x] * dfd["CHPGRDSHR"].loc[x]

    #         Generation for Own Use
    # T140(117,IY,IS)=sum(CHPINDGEN(7,v888,iy)*(1.-CHPGRDSHR(v888,iy)))
    z[117] = z[115] - z[116]

    return z
