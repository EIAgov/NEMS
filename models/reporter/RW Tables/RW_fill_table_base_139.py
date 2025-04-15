# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
Fixed by SZO on 7/2/20124
"""
from RW_preprocessor import indcarb


def fill_table_base_139(dfd, table_spec, table_id):
    """Fill table for Metal-based Durables Industry Energy Consumption

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

    MNUMCR = dfd["MNUMCR_rwpre"]

    IXRF = 6
    IXDS = 7
    IXLG = 8
    IXPC = 11

    CARBON_OR_2 = table_spec["settings"]["carbon_or_carbondioxide"].upper()
    C_CO2_FACTOR = dfd["C_CO2_FACTOR_rwpre"] if CARBON_OR_2 == "CO2" else 1

    #    v88=(/14,15,16,17,18/)  ! vector subscript , used similarly to triplet 1: 6
    #   Metal-based Durables Industry Energy Consumption

    #    Shipments, Energy Consumption, and Emissions
    #   Energy Consumption (trillion Btu)

    #      Fabricated Metal Products Consumption 1/

    #         Propane
    # T139(3,IY,IS)=FABMETALCON(ixLG,5,IY)
    z[3] = dfd["FABMETALCON"].loc[IXLG].loc[5]

    #         Distillate Fuel Oil
    # T139(2,IY,IS)=FABMETALCON(ixDS,5,IY)
    z[2] = dfd["FABMETALCON"].loc[IXDS].loc[5]

    #         Residual Fuel Oil
    # T139(1,IY,IS)=FABMETALCON(ixRF,5,IY)
    z[1] = dfd["FABMETALCON"].loc[IXRF].loc[5]

    #         Petroleum Coke
    # T139(4,IY,IS)=FABMETALCON(ixPC,5,IY)
    z[4] = dfd["FABMETALCON"].loc[IXPC].loc[5]

    #         Petroleum and Other Liquids Subtotal
    # T139(5,IY,IS)=FSUM(T139(1,IY,IS),4)
    z[5] = z[1] + z[2] + z[3] + z[4]

    #         Natural Gas
    # T139(6,IY,IS)=FABMETALCON(ixNG,5,IY)
    z[6] = dfd["FABMETALCON"].loc[2].loc[5]

    #         Steam Coal
    # T139(7,IY,IS)=FABMETALCON(ixCL,5,IY)
    z[7] = dfd["FABMETALCON"].loc[3].loc[5]

    #         Renewables
    # T139(8,IY,IS)=FABMETALCON(ixRN,5,IY)
    z[8] = dfd["FABMETALCON"].loc[18].loc[5]

    #         Purchased Electricity
    # T139(9,IY,IS)=FABMETALCON(ixEL,5,IY)
    z[9] = dfd["FABMETALCON"].loc[1].loc[5]

    #            Total
    # T139(10,IY,IS)=FSUM(T139(5,IY,IS),5)+T139(86,IY,IS)
    z[10] = z[5] + z[6] + z[7] + z[8] + z[9]

    #

    #      Machinery Consumption 1/

    #         Propane
    # T139(13,IY,IS)=MACHINECON(ixLG,5,IY)
    z[13] = dfd["MACHINECON"].loc[IXLG].loc[5]

    #         Distillate Fuel Oil
    # T139(12,IY,IS)=MACHINECON(ixDS,5,IY)
    z[12] = dfd["MACHINECON"].loc[IXDS].loc[5]

    #         Residual Fuel Oil
    # T139(11,IY,IS)=MACHINECON(ixRF,5,IY)
    z[11] = dfd["MACHINECON"].loc[IXRF].loc[5]

    #         Petroleum Coke
    # T139(14,IY,IS)=MACHINECON(ixPC,5,IY)
    z[14] = dfd["MACHINECON"].loc[IXPC].loc[5]

    #            Petroleum and Other Liquids Subtotal
    # T139(15,IY,IS)=FSUM(T139(11,IY,IS),4)
    z[15] = z[11] + z[12] + z[13] + z[14]

    #         Natural Gas
    # T139(16,IY,IS)=MACHINECON(ixNG,5,IY)
    z[16] = dfd["MACHINECON"].loc[2].loc[5]

    #         Steam Coal
    # T139(17,IY,IS)=MACHINECON(ixCL,5,IY)
    z[17] = dfd["MACHINECON"].loc[3].loc[5]

    #         Renewables
    # T139(18,IY,IS)=MACHINECON(ixRN,5,IY)
    z[18] = dfd["MACHINECON"].loc[18].loc[5]

    #         Purchased Electricity
    # T139(19,IY,IS)=MACHINECON(ixEL,5,IY)
    z[19] = dfd["MACHINECON"].loc[1].loc[5]

    #            Total
    # T139(20,IY,IS)=FSUM(T139(15,IY,IS),5)
    z[20] = z[15] + z[16] + z[17] + z[18] + z[19]

    #

    #      Computers Consumption 1/

    #         Propane
    # T139(23,IY,IS)=COMPUTECON(ixLG,5,IY)
    z[23] = dfd["COMPUTECON"].loc[IXLG].loc[5]

    #         Distillate Fuel Oil
    # T139(22,IY,IS)=COMPUTECON(ixDS,5,IY)
    z[22] = dfd["COMPUTECON"].loc[IXDS].loc[5]

    #         Residual Fuel Oil
    # T139(21,IY,IS)=COMPUTECON(ixRF,5,IY)
    z[21] = dfd["COMPUTECON"].loc[IXRF].loc[5]

    #         Petroleum Coke
    # T139(24,IY,IS)=COMPUTECON(ixPC,5,IY)
    z[24] = dfd["COMPUTECON"].loc[IXPC].loc[5]

    #            Petroleum and Other Liquids Subtotal
    # T139(25,IY,IS)=FSUM(T139(21,IY,IS),4)
    z[25] = z[21] + z[22] + z[23] + z[24]

    #         Natural Gas
    # T139(26,IY,IS)=COMPUTECON(ixNG,5,IY)
    z[26] = dfd["COMPUTECON"].loc[2].loc[5]

    #         Steam Coal
    # T139(27,IY,IS)=COMPUTECON(ixCL,5,IY)
    z[27] = dfd["COMPUTECON"].loc[3].loc[5]

    #         Renewables
    # T139(28,IY,IS)=COMPUTECON(ixRN,5,IY)
    z[28] = dfd["COMPUTECON"].loc[18].loc[5]

    #         Purchased Electricity
    # T139(29,IY,IS)=COMPUTECON(ixEL,5,IY)
    z[29] = dfd["COMPUTECON"].loc[1].loc[5]

    #            Total
    # T139(30,IY,IS)=FSUM(T139(25,IY,IS),5)
    z[30] = z[25] + z[26] + z[27] + z[28] + z[29]

    #

    #      Transportation Equipment Consumption 1/

    #         Propane
    # T139(33,IY,IS)=TRANEQUIPCON(ixLG,5,IY)
    z[33] = dfd["TRANEQUIPCON"].loc[IXLG].loc[5]

    #         Distillate Fuel Oil
    # T139(32,IY,IS)=TRANEQUIPCON(ixDS,5,IY)
    z[32] = dfd["TRANEQUIPCON"].loc[IXDS].loc[5]

    #         Residual Fuel Oil
    # T139(31,IY,IS)=TRANEQUIPCON(ixRF,5,IY)
    z[31] = dfd["TRANEQUIPCON"].loc[IXRF].loc[5]

    #         Petroleum Coke
    # T139(34,IY,IS)=TRANEQUIPCON(ixPC,5,IY)
    z[34] = dfd["TRANEQUIPCON"].loc[IXPC].loc[5]

    #            Petroleum and Other Liquids Subtotal
    # T139(35,IY,IS)=FSUM(T139(31,IY,IS),4)
    z[35] = z[31] + z[32] + z[33] + z[34]

    #         Natural Gas
    # T139(36,IY,IS)=TRANEQUIPCON(ixNG,5,IY)
    z[36] = dfd["TRANEQUIPCON"].loc[2].loc[5]

    #         Steam Coal
    # T139(37,IY,IS)=TRANEQUIPCON(ixCL,5,IY)
    z[37] = dfd["TRANEQUIPCON"].loc[3].loc[5]

    #         Renewables
    # T139(38,IY,IS)=TRANEQUIPCON(ixRN,5,IY)
    z[38] = dfd["TRANEQUIPCON"].loc[18].loc[5]

    #         Purchased Electricity
    # T139(39,IY,IS)=TRANEQUIPCON(ixEL,5,IY)
    z[39] = dfd["TRANEQUIPCON"].loc[1].loc[5]

    #            Total
    # T139(40,IY,IS)=FSUM(T139(35,IY,IS),5)
    z[40] = z[35] + z[36] + z[37] + z[38] + z[39]

    #

    #      Electrical Equipment Consumption 1/

    #         Propane
    # T139(43,IY,IS)=ELECEQUIPON(ixLG,5,IY)
    z[43] = dfd["ELECEQUIPON"].loc[IXLG].loc[5]

    #         Distillate Fuel Oil
    # T139(42,IY,IS)=ELECEQUIPON(ixDS,5,IY)
    z[42] = dfd["ELECEQUIPON"].loc[IXDS].loc[5]

    #         Residual Fuel Oil
    # T139(41,IY,IS)=ELECEQUIPON(ixRF,5,IY)
    z[41] = dfd["ELECEQUIPON"].loc[IXRF].loc[5]

    #         Petroleum Coke
    # T139(44,IY,IS)=ELECEQUIPON(ixPC,5,IY)
    z[44] = dfd["ELECEQUIPON"].loc[IXPC].loc[5]

    #            Petroleum and Other Liquids Subtotal
    # T139(45,IY,IS)=FSUM(T139(41,IY,IS),4)
    z[45] = z[41] + z[42] + z[43] + z[44]

    #         Natural Gas
    # T139(46,IY,IS)=ELECEQUIPON(ixNG,5,IY)
    z[46] = dfd["ELECEQUIPON"].loc[2].loc[5]

    #         Steam Coal
    # T139(47,IY,IS)=ELECEQUIPON(ixCL,5,IY)
    z[47] = dfd["ELECEQUIPON"].loc[3].loc[5]

    #         Renewables
    # T139(48,IY,IS)=ELECEQUIPON(ixRN,5,IY)
    z[48] = dfd["ELECEQUIPON"].loc[18].loc[5]

    #         Purchased Electricity
    # T139(49,IY,IS)=ELECEQUIPON(ixEL,5,IY)
    z[49] = dfd["ELECEQUIPON"].loc[1].loc[5]

    #            Total
    # T139(50,IY,IS)=FSUM(T139(45,IY,IS),5)
    z[50] = z[45] + z[46] + z[47] + z[48] + z[49]

    #

    #   Value of Shipments (billion m==m dollars)

    #      Fabricated Metal Products
    # T139(51,IY,IS)=MC_REVIND(11,36,IY)
    z[51] = dfd["MC_REVIND"].loc[36].loc[MNUMCR]

    #      Machinery
    # T139(52,IY,IS)=MC_REVIND(11,37,IY)
    z[52] = dfd["MC_REVIND"].loc[37].loc[MNUMCR]

    #      Computers
    # T139(53,IY,IS)=MC_REVIND(11,38,IY)
    z[53] = dfd["MC_REVIND"].loc[38].loc[MNUMCR]

    #      Transportation Equipment
    # T139(54,IY,IS)=MC_REVIND(11,39,IY)
    z[54] = dfd["MC_REVIND"].loc[39].loc[MNUMCR]

    #      Electrical Equipment
    # T139(55,IY,IS)=MC_REVIND(11,40,IY)
    z[55] = dfd["MC_REVIND"].loc[40].loc[MNUMCR]

    #

    #   Energy Consumption per Unit of Output 1/

    #   (thousand Btu per m==m dollar shipments)

    #      Fabricated Metal Products
    # T139(56,IY,IS)=T139(10,IY,IS)/MC_REVIND(11,36,IY)
    z[56] = z[10] / dfd["MC_REVIND"].loc[36].loc[MNUMCR]

    #      Machinery
    # T139(57,IY,IS)=T139(20,IY,IS)/MC_REVIND(11,37,IY)
    z[57] = z[20] / dfd["MC_REVIND"].loc[37].loc[MNUMCR]

    #      Computers
    # T139(58,IY,IS)=T139(30,IY,IS)/MC_REVIND(11,38,IY)
    z[58] = z[30] / dfd["MC_REVIND"].loc[38].loc[MNUMCR]

    #      Transportation Equipment
    # T139(59,IY,IS)=T139(40,IY,IS)/MC_REVIND(11,39,IY)
    z[59] = z[40] / dfd["MC_REVIND"].loc[39].loc[MNUMCR]

    #      Electrical Equipment
    # T139(60,IY,IS)=T139(50,IY,IS)/MC_REVIND(11,40,IY)
    z[60] = z[50] / dfd["MC_REVIND"].loc[40].loc[MNUMCR]

    #

    #   Carbon Dioxide Emissions 2/ (million metric

    #    tons carbon dioxide)
    ### put INDCARB into preprocessor
    #      Fabricated Metal Products
    # T139(61,IY,IS)=INDCARB(FABMETALCON(1,1,IY),IY)
    z[61] = indcarb(dfd, dfd["FABMETALCON"], CARBON_OR_2)
    # z[61] =

    #      Machinery
    # T139(62,IY,IS)=INDCARB(MACHINECON(1,1,IY),IY)
    z[62] = indcarb(dfd, dfd["MACHINECON"], CARBON_OR_2)

    #      Computers
    # T139(63,IY,IS)=INDCARB(COMPUTECON(1,1,IY),IY)
    z[63] = indcarb(dfd, dfd["COMPUTECON"], CARBON_OR_2)

    #      Transportation Equipment
    # T139(64,IY,IS)=INDCARB(TRANEQUIPCON(1,1,IY),IY)
    z[64] = indcarb(dfd, dfd["TRANEQUIPCON"], CARBON_OR_2)

    #      Electrical Equipment
    # T139(65,IY,IS) = INDCARB(ELECEQUIPON(1,1,IY),IY)
    z[65] = indcarb(dfd, dfd["ELECEQUIPON"], CARBON_OR_2)

    #

    #   Fabricated Metal Products, Machinery,
    #   Computers, Transportation Equipment,
    #   and Electrical Equipment:

    #    Combined Heat and Power 3/

    #     Generating Capacity (gigawatts) 4/
    # T139(66,IY,IS)=sum(CHPINDCAP(1,v88,iy))
    # T139(67,IY,IS)=sum(CHPINDCAP(2,v88,iy))
    # T139(68,IY,IS)=sum(CHPINDCAP(3,v88,iy))
    # T139(69,IY,IS)=sum(CHPINDCAP(4:6,v88,iy))
    # T139(70,IY,IS)=sum(CHPINDCAP(7,v88,iy))
    # T139(71,IY,IS)=sum(CHPINDGEN(1,v88,iy)) ! Fuels order: coal,oil,nat gas, wood, other,msw
    # T139(72,IY,IS)=sum(CHPINDGEN(2,v88,iy)) ! Fuels order: coal,oil,nat gas, wood, other,msw
    # T139(73,IY,IS)=sum(CHPINDGEN(3,v88,iy)) ! Fuels order: coal,oil,nat gas, wood, other,msw
    # T139(74,IY,IS)=sum(CHPINDGEN(4:6,v88,iy)) ! Fuels order: coal,oil,nat gas, wood, other,msw
    # T139(75,IY,IS)=sum(CHPINDGEN(7,v88,iy)) ! total across fuels
    # T139(76,IY,IS)=sum(CHPINDGEN(7,v88,iy)*    CHPGRDSHR(v88,iy))  ! Sales to grid
    # T139(77,IY,IS)=sum(CHPINDGEN(7,v88,iy)*(1.-CHPGRDSHR(v88,iy))) ! Own-Use

    z[66] = dfd["CHPINDCAP"].loc[1, 14:18, :].sum()
    z[67] = dfd["CHPINDCAP"].loc[2, 14:18, :].sum()
    z[68] = dfd["CHPINDCAP"].loc[3, 14:18, :].sum()
    z[69] = dfd["CHPINDCAP"].loc[4:6, 14:18, :].sum()
    z[70] = dfd["CHPINDCAP"].loc[7, 14:18, :].sum()
    
    z[71] = dfd["CHPINDGEN"].loc[1, 14:18, :].sum()
    z[72] = dfd["CHPINDGEN"].loc[2, 14:18, :].sum()
    z[73] = dfd["CHPINDGEN"].loc[3, 14:18, :].sum()
    z[74] = dfd["CHPINDGEN"].loc[4:6, 14:18, :].sum()
    z[75] = dfd["CHPINDGEN"].loc[7, 14:18, :].sum()
    
    z[76] = z[75] * 0.0
    for x in range(14, 19):
        z[76] += dfd["CHPINDGEN"].loc[7, x] * dfd["CHPGRDSHR"].loc[x]
    z[77] = z[75] - z[76]

    return z
