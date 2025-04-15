# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_021(dfd, table_spec, table_id):
    """Fill table for  Liquid Fuels Supply and Disposition

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


    """

    z = {}

    MNUMPR = dfd["MNUMPR_rwpre"]
    MNUMCR = dfd["MNUMCR_rwpre"]
    MNUMOR = dfd["MNUMOR_rwpre"]
    RDAYS = dfd["RDAYS_rwpre"]

    #   Liquid Fuels Supply and Disposition

    #      Domestic Crude Oil
    # T21( 1,1,IY,IS) = RFQTDCRD(MNUMOR+2,IY)
    # z[1]=dfd['RFQTDCRD'].loc[MNUMOR+2]
    z[1] = (
        dfd["FEEDSTOCKS"]
        .loc[1, 1]
        .where(dfd["FEEDSTOCKS"].loc[1, 1] > 0, dfd["RFQTDCRD"].loc[MNUMOR + 2])
    )

    #      Imported Crude Oil
    z[2] = (
        dfd["FEEDSTOCKS"]
        .loc[1, 2]
        .where(dfd["FEEDSTOCKS"].loc[1, 2] > 0, dfd["RFQICRD"].loc[MNUMPR])
    )
    #      Imported Unfinished Oil
    z[3] = (
        dfd["FEEDSTOCKS"]
        .loc[1, 3]
        .where(dfd["FEEDSTOCKS"].loc[1, 3] > 0, dfd["RFPQUFC"].loc[MNUMPR].loc[2])
    )
    #      Domestic Natural Gas Plant Liquids
    z[4] = (
        dfd["FEEDSTOCKS"]
        .loc[1, 4]
        .where(
            dfd["FEEDSTOCKS"].loc[1, 4] > 0, dfd["RFQNGPL"].loc[MNUMPR].loc[6] / 1000.0
        )
    )
    #      Imported Natural Gas Plant Liquids                       T21(5,4,IY,IS)=0.0
    z[5] = dfd["FEEDSTOCKS"].loc[1, 5].where(dfd["FEEDSTOCKS"].loc[1, 5] > 0, 0)
    #      Dry Natural Gas                                          T21(6,4,IY,IS)=0.0
    z[6] = dfd["FEEDSTOCKS"].loc[1, 6].where(dfd["FEEDSTOCKS"].loc[1, 6] > 0, 0)
    #      Coal                                                     T21(7,4,IY,IS)=0.0
    z[7] = dfd["FEEDSTOCKS"].loc[1, 7].where(dfd["FEEDSTOCKS"].loc[1, 7] > 0, 0)
    #      Grain Crops                                              T21(8,4,IY,IS)=0.0
    z[8] = dfd["FEEDSTOCKS"].loc[1, 8].where(dfd["FEEDSTOCKS"].loc[1, 8] > 0, 0)
    #      Cellulosic Biomass                                       T21(9,4,IY,IS)=0.0
    z[9] = dfd["FEEDSTOCKS"].loc[1, 9].where(dfd["FEEDSTOCKS"].loc[1, 9] > 0, 0)
    #      Renewable Oils
    z[10] = dfd["FEEDSTOCKS"].loc[1, 10].where(dfd["FEEDSTOCKS"].loc[1, 10] > 0, 0)

    #   Feedstocks                                                  T21(72,4,IY,IS)=FSUM(T21(1,4,IY,IS),10)
    z[72] = z[1] + z[2] + z[3] + z[4] + z[5] + z[6] + z[7] + z[8] + z[9] + z[10]

    # IF (sum(INTERMEDIATE(1,1:13,IY)) .GT. 0.0) T21(11:23,1,IY,IS) = INTERMEDIATE(1,1:13,IY)
    #      Petroleum Gasoline Blending Components                   T21(11:23,4,IY,IS)=INTERMEDIATE(4,1:13,IY)
    z[11] = dfd["INTERMEDIATE"].loc[1].loc[1]
    z[12] = dfd["INTERMEDIATE"].loc[1].loc[2]
    z[13] = dfd["INTERMEDIATE"].loc[1].loc[3]
    z[14] = dfd["INTERMEDIATE"].loc[1].loc[4]
    z[15] = dfd["INTERMEDIATE"].loc[1].loc[5]
    z[16] = dfd["INTERMEDIATE"].loc[1].loc[6]
    z[17] = dfd["INTERMEDIATE"].loc[1].loc[7]
    z[18] = dfd["INTERMEDIATE"].loc[1].loc[8]
    z[19] = dfd["INTERMEDIATE"].loc[1].loc[9]
    z[20] = dfd["INTERMEDIATE"].loc[1].loc[10]
    z[21] = dfd["INTERMEDIATE"].loc[1].loc[11]
    z[22] = dfd["INTERMEDIATE"].loc[1].loc[12]
    z[23] = dfd["INTERMEDIATE"].loc[1].loc[13]
    #      Nonpetroleum Fossil Gasoline Blending Components         T21(12,4,IY,IS)=0.0
    #      Biogasoline Blending Components                          T21(13,4,IY,IS)=0.0
    #      Fuel Ethanol                                             T21(14,4,IY,IS)=0.0
    #      Petroleum Distillate                                     T21(15,4,IY,IS)=0.0
    #      Nonpetroleum Fossil Distillate                           T21(16,4,IY,IS)=0.0
    #      Biomass Based Distillate                                 T21(17,4,IY,IS)=0.0
    #      Residual Fuel Oil                                        T21(18,4,IY,IS)=0.0
    #      Petrochemicals                                           T21(19,4,IY,IS)=0.0
    #      Nonpetroleum Fossil Chemicals                            T21(20,4,IY,IS)=0.0
    #      Biochemicals                                             T21(21,4,IY,IS)=0.0
    #      Petroleum Solids                                         T21(22,4,IY,IS)=0.0
    #      Biofuel Solids                                           T21(23,4,IY,IS)=0.0

    #   Refining Sector Intermediate Products                       T21(73,4,IY,IS)=FSUM(T21(11,4,IY,IS),13)
    z[73] = (
        z[11]
        + z[12]
        + z[13]
        + z[14]
        + z[15]
        + z[16]
        + z[17]
        + z[18]
        + z[19]
        + z[20]
        + z[21]
        + z[22]
        + z[23]
    )

    #      Gasoline Blending Components                             T21(24:25,4,IY,IS)=REFINE_PROD(4,1:2,IY)
    z[24] = dfd["REFINE_PROD"].loc[4].loc[1]
    #      Fuel Ethanol                                             T21(25,4,IY,IS)=0.0
    z[25] = z[24] * 0.0
    # IF (sum( REFINE_PROD(1,1: 2,IY)) .GT. 0.0) T21(24:25,1,IY,IS) =  REFINE_PROD(1,1: 2,IY)
    z[24] = dfd["REFINE_PROD"].loc[1].loc[1]
    z[25] = dfd["REFINE_PROD"].loc[1].loc[2]
    #      Biobutanol                                               T21(27:35,4,IY,IS)=REFINE_PROD(4,3:11,IY)
    # IF (sum( REFINE_PROD(2,3:11,IY)) .GT. 0.0) T21(27:35,2,IY,IS) =  REFINE_PROD(2,3:11,IY)
    #      Jet Fuel                                                 T21(27:35,4,IY,IS)=REFINE_PROD(4,3:11,IY)
    z[27] = dfd["REFINE_PROD"].loc[1].loc[3]

    #      Diesel Fuel                                              T21(28,4,IY,IS)=0.0
    z[28] = dfd["REFINE_PROD"].loc[1].loc[4]

    #      Light Heating Oil 5/                                     T21(29,4,IY,IS)=0.0
    z[29] = dfd["REFINE_PROD"].loc[1].loc[5]

    #      #6 Fuel Oil                                              T21(30,4,IY,IS)=0.0
    z[30] = dfd["REFINE_PROD"].loc[1].loc[6]

    #      Liquefied Gases                                          T21(31,4,IY,IS)=0.0
    z[31] = dfd["REFINE_PROD"].loc[1].loc[7]

    #      Liquid Chemicals                                         T21(32,4,IY,IS)=0.0
    z[32] = dfd["REFINE_PROD"].loc[1].loc[8]

    #      Petroleum Solids                                         T21(33,4,IY,IS)=0.0
    z[33] = dfd["REFINE_PROD"].loc[1].loc[9]

    #      Biofuel Solids                                           T21(34,4,IY,IS)=0.0
    z[34] = dfd["REFINE_PROD"].loc[1].loc[10]
    z[35] = dfd["REFINE_PROD"].loc[1].loc[11]
    #   Refined Products                                            T21(74,4,IY,IS)=FSUM(T21(24,4,IY,IS),12)
    z[74] = z[24] + z[27] + z[35]

    #      Gasoline Blending Components                             T21(36,4,IY,IS)=RFIPQCBOB(MNUMPR,IY,2)*RDAYS*CFCBOB(IY)/1000.+RFIPQRBOB(MNUMPR,IY,2)*RDAYS*CFRBOB(IY)/1000.
    z[36] = (
        dfd["RFIPQCBOB"].loc[MNUMPR].loc[2] * RDAYS * dfd["CFCBOB"] / 1000
        + dfd["RFIPQRBOB"].loc[MNUMPR].loc[2] * RDAYS * dfd["CFRBOB"] / 1000
    )

    #      Fuel Ethanol                                             T21(37,4,IY,IS)=ETHIMP(MNUMCR,IY)/1000.*RDAYS*CFPET/1000.
    z[37] = dfd["ETHIMP"].loc[MNUMCR] / 1000 * RDAYS * dfd["CFPET"] / 1000

    #      Biobutanol                                               T21(46,4,IY,IS)=0.0
    # z[46] =

    #      Motor Gasoline                                           T21(38,4,IY,IS)=RFIPQMG(MNUMPR,IY,2)*RDAYS*CFMGQ(IY)/1000.
    z[38] = dfd["RFIPQMG"].loc[MNUMPR].loc[2] * RDAYS * dfd["CFMGQ"] / 1000

    #      Jet Fuel                                                 T21(39,4,IY,IS)=RFIPQJF(MNUMPR,IY,2)*RDAYS*CFJFQ(IY)/1000.
    z[39] = dfd["RFIPQJF"].loc[MNUMPR].loc[2] * RDAYS * dfd["CFJFQ"] / 1000

    #      Diesel Fuel                                              T21(40,4,IY,IS)=0.0
    z[40] = z[39] * 0.0

    #      Light Heating Oil 5/                                     T21(41,4,IY,IS)=0.0
    z[41] = z[39] * 0.0

    #      #6 Fuel Oil                                              T21(42,4,IY,IS)=(RFIPQRH(MNUMPR,IY,2)+RFIPQRL(MNUMPR,IY,2))*RDAYS*CFRSQ/1000.
    z[42] = (
        (dfd["RFIPQRH"].loc[MNUMPR].loc[2] + dfd["RFIPQRL"].loc[MNUMPR].loc[2])
        * RDAYS
        * dfd["CFRSQ"]
        / 1000
    )

    #      Liquefied Gases         T21(43,4,IY,IS)=(RFIPQPR(MNUMPR,IY,2)+RFIPQPY(MNUMPR,IY,2)+RFIPQPP(MNUMPR,IY,2)+RFIPQET(MNUMPR,IY,2)+RFIPQBU(MNUMPR,IY,2)+RFIPQIS(MNUMPR,IY,2))*RDAYS*CFLGQ(IY)/1000.
    z[43] = (
        (
            dfd["RFIPQPR"].loc[MNUMPR].loc[2]
            + dfd["RFIPQPY"].loc[MNUMPR].loc[2]
            + dfd["RFIPQPP"].loc[MNUMPR].loc[2]
            + dfd["RFIPQET"].loc[MNUMPR].loc[2]
            + dfd["RFIPQBU"].loc[MNUMPR].loc[2]
            + dfd["RFIPQIS"].loc[MNUMPR].loc[2]
        )
        * RDAYS
        * dfd["CFLGQ"]
        / 1000.0
    )

    #      Liquid Chemicals                                         T21(44,4,IY,IS)=0.0
    z[44] = z[43] * 0.0

    #      Petroleum Solids                                         T21(45,4,IY,IS)=0.0
    z[45] = z[44] * 0.0
    # IF (sum(GROSS_IMPORT(2,1:11,IY)) .GT. 0.0) T21(36:46,2,IY,IS) = GROSS_IMPORT(2,1:11,IY)
    z[36] = dfd["GROSS_IMPORT"].loc[1].loc[1]
    z[37] = dfd["GROSS_IMPORT"].loc[1].loc[2]
    z[38] = dfd["GROSS_IMPORT"].loc[1].loc[3]
    z[39] = dfd["GROSS_IMPORT"].loc[1].loc[4]
    z[40] = dfd["GROSS_IMPORT"].loc[1].loc[5]
    z[41] = dfd["GROSS_IMPORT"].loc[1].loc[6]
    z[42] = dfd["GROSS_IMPORT"].loc[1].loc[7]
    z[43] = dfd["GROSS_IMPORT"].loc[1].loc[8]
    z[44] = dfd["GROSS_IMPORT"].loc[1].loc[9]
    z[45] = dfd["GROSS_IMPORT"].loc[1].loc[10]
    z[46] = dfd["GROSS_IMPORT"].loc[1].loc[11]

    #   Gross Product Imports                                       T21(75,4,IY,IS)=FSUM(T21(36,4,IY,IS),12)
    z[75] = z[36] + z[37] + z[38] + z[39] + z[42] + z[43]

    #      Gasoline Blending Components                             T21(48,4,IY,IS)=QPRDEX(4,IY)*RDAYS*CFCBOB(IY)/1000.+QPRDEX(5,IY)*RDAYS*CFRBOB(IY)/1000.
    z[48] = (
        dfd["QPRDEX"].loc[4] / 1000 * RDAYS * dfd["CFCBOB"] / 1000
        + dfd["QPRDEX"].loc[5] / 1000 * RDAYS * dfd["CFRBOB"] / 1000
    )

    #      Fuel Ethanol                                             T21(49,4,IY,IS)=ETHEXP(MNUMCR,IY)/1000.*RDAYS*CFPET/1000.
    z[49] = dfd["ETHEXP"].loc[MNUMCR] / 1000 * RDAYS * dfd["CFPET"] / 1000

    #      Biobutanol                                               T21(48:59,4,IY,IS)=GROSS_EXPORT(4,1:12,IY)
    z[59] = dfd["GROSS_EXPORT"].loc[4].loc[1]

    #      Motor Gasoline                                           T21(50,4,IY,IS)=(QPRDEX(2,IY)+QPRDEX(3,IY)+QPRDEX(26,IY))*RDAYS*CFMGQ(IY)/1000.
    z[50] = (
        (dfd["QPRDEX"].loc[2] + dfd["QPRDEX"].loc[3] + dfd["QPRDEX"].loc[26])
        * RDAYS
        * dfd["CFMGQ"]
        / 1000
    )

    #      Jet Fuel                                                 T21(51,4,IY,IS)=QPRDEX(6,IY)*RDAYS*CFJFQ(IY)/1000.
    z[51] = dfd["QPRDEX"].loc[6] * RDAYS * dfd["CFJFQ"] / 1000

    #      Diesel Fuel                                              T21(52,4,IY,IS)=(QPRDEX(13,IY)+QPRDEX(24,IY))*RDAYS*CFDSTR(IY)/1000.
    z[52] = (
        (dfd["QPRDEX"].loc[13] + dfd["QPRDEX"].loc[24]) * RDAYS * dfd["CFDSTR"] / 1000
    )

    #      Light Heating Oil 5/                                     T21(53,4,IY,IS)=QPRDEX(7,IY)*RDAYS*CFDSRS(IY)/1000.
    z[53] = dfd["QPRDEX"].loc[7] * RDAYS * dfd["CFPCQ"] / 1000

    #      #6 Fuel Oil                                              T21(54,4,IY,IS)=(QPRDEX(8,IY)+QPRDEX(9,IY))*RDAYS*CFRSQ/1000.
    z[54] = (dfd["QPRDEX"].loc[8] + dfd["QPRDEX"].loc[9]) * RDAYS * dfd["CFRSQ"] / 1000

    #      Liquefied Gases           T21(55,4,IY,IS)=(QPRDEX(1,IY)+QPRDEX(27,IY)+QPRDEX(28,IY)+QPRDEX(14,IY)+QPRDEX(15,IY)+QPRDEX(29,IY))*RDAYS*CFLGQ(IY)/1000.
    z[55] = (
        (
            dfd["QPRDEX"].loc[1]
            + dfd["QPRDEX"].loc[27]
            + dfd["QPRDEX"].loc[28]
            + dfd["QPRDEX"].loc[14]
            + dfd["QPRDEX"].loc[15]
            + dfd["QPRDEX"].loc[29]
        )
        * RDAYS
        * dfd["CFLGQ"]
        / 1000
    )

    #      Liquid Chemicals                                         T21(56,4,IY,IS)=0.0
    # z[56] =

    #      Petroleum Solids                                         T21(57,4,IY,IS)=QPRDEX(16,IY)*RDAYS*CFPCQ/1000.
    z[57] = dfd["QPRDEX"].loc[16] / 1000 * RDAYS * dfd["CFPCQ"] / 1000

    #      Biofuel Solids                                           T21(58,4,IY,IS)=BIODEXP(MNUMCR,IY)/1000.*RDAYS*CFBIOD(IY)/1000.
    z[58] = dfd["BIODEXP"].loc[MNUMCR] / 1000 * RDAYS * dfd["CFBIOD"] / 1000

    # IF (sum(GROSS_EXPORT(3,1:12,IY)) .GT. 0.0) T21(48:59,3,IY,IS) = GROSS_EXPORT(3,1:12,IY)
    z[48] = dfd["GROSS_EXPORT"].loc[1].loc[1]
    z[49] = dfd["GROSS_EXPORT"].loc[1].loc[2]
    z[50] = dfd["GROSS_EXPORT"].loc[1].loc[3]
    z[51] = dfd["GROSS_EXPORT"].loc[1].loc[4]
    z[52] = dfd["GROSS_EXPORT"].loc[1].loc[5]
    z[53] = dfd["GROSS_EXPORT"].loc[1].loc[6]
    z[54] = dfd["GROSS_EXPORT"].loc[1].loc[7]
    z[55] = dfd["GROSS_EXPORT"].loc[1].loc[8]
    z[56] = dfd["GROSS_EXPORT"].loc[1].loc[9]
    z[57] = dfd["GROSS_EXPORT"].loc[1].loc[10]
    z[58] = dfd["GROSS_EXPORT"].loc[1].loc[11]
    z[59] = dfd["GROSS_EXPORT"].loc[1].loc[12]
    #   Gross Product Exports                                       T21(76,4,IY,IS)=FSUM(T21(48,4,IY,IS),12)
    z[76] = (
        z[48] + z[49] + z[50] + z[51] + z[52] + z[53] + z[54] + z[55] + z[57] + z[58]
    )
    #      Propane                           T21(60,4,IY,IS)=QPRRS(MNUMCR,IY)+QPRCM(MNUMCR,IY)+QPRTR(MNUMCR,IY)+QPRTR(MNUMCR,IY)-QPRINPF(MNUMCR,IY)
    z[60] = (
        dfd["QPRRS"].loc[MNUMCR]
        + dfd["QPRCM"].loc[MNUMCR]
        + dfd["QPRTR"].loc[MNUMCR]
        + dfd["QPRTR"].loc[MNUMCR]
        - dfd["QPRINPF"].loc[MNUMCR]
    )

    #      E10 6/                                                   T21(61,4,IY,IS)=0.0
    #      E15 7/                                                   T21(62,4,IY,IS)=0.0
    #      E85 8/                                                   T21(63,4,IY,IS)=QETTR(MNUMCR,IY)
    z[63] = dfd["QETTR"].loc[MNUMCR]
    #      Jet Fuel                                                 T21(64,4,IY,IS)=QJFTR(MNUMCR,IY)-QJFBS(MNUMCR,IY)
    z[64] = dfd["QJFTR"].loc[MNUMCR] - dfd["QJFBS"].loc[MNUMCR]
    #      Diesel Fuel                                              T21(65,4,IY,IS)=0.0
    #      Light Heating Oil 5/                                     T21(66,4,IY,IS)=0.0
    #      #6 Fuel Oil                                              T21(67,4,IY,IS)=QRSAS(MNUMCR,IY)
    z[67] = dfd["QRSAS"].loc[MNUMCR]
    #      Chemical Feedstock                                       T21(68,4,IY,IS)=0.0
    #      Construction Sales                                       T21(69,4,IY,IS)=0.0
    #      Animal Feed                                              T21(70,4,IY,IS)=0.0
    #      Lignin Fuel Pellets                                      T21(71,4,IY,IS)=0.0
    # IF (sum( DOM_CONSUME(3,1:12,IY)) .GT. 0.0) T21(60:71,3,IY,IS) =  DOM_CONSUME(3,1:12,IY)
    z[60] = dfd["DOM_CONSUME"].loc[1].loc[1]
    z[61] = dfd["DOM_CONSUME"].loc[1].loc[2]
    z[62] = dfd["DOM_CONSUME"].loc[1].loc[3]
    z[63] = dfd["DOM_CONSUME"].loc[1].loc[4]
    z[64] = dfd["DOM_CONSUME"].loc[1].loc[5]
    z[65] = dfd["DOM_CONSUME"].loc[1].loc[6]
    z[66] = dfd["DOM_CONSUME"].loc[1].loc[7]
    z[67] = dfd["DOM_CONSUME"].loc[1].loc[8]
    z[68] = dfd["DOM_CONSUME"].loc[1].loc[9]
    z[69] = dfd["DOM_CONSUME"].loc[1].loc[10]
    z[70] = dfd["DOM_CONSUME"].loc[1].loc[11]
    z[71] = dfd["DOM_CONSUME"].loc[1].loc[12]
    # reset these to 0; Volume version now has mixed units:
    z[72] = z[67] * 0.0
    z[73] = z[67] * 0.0
    z[74] = z[67] * 0.0
    z[75] = z[67] * 0.0
    z[76] = z[67] * 0.0
    z[77] = z[67] * 0.0

    #   Summary
    #      Feedstocks - Intermediate Streams                        T21(78,:,IY,IS)=T21(72,:,IY,IS)-T21(73,:,IY,IS)
    z[78] = z[72] - z[73]

    #      Intermediate - Refined - Net Imports                     T21(79,:,IY,IS)=T21(73,:,IY,IS)-T21(74,:,IY,IS)-T21(75,1,IY,IS)+T21(76,1,IY,IS)
    z[79] = z[73] - z[74] - z[75] + z[76]

    #      Refined + Net Imports - Consumption                      T21(80,:,IY,IS)=T21(74,:,IY,IS)+T21(75,1,IY,IS)-T21(76,1,IY,IS)-T21(77,:,IY,IS)
    z[80] = z[74] + z[75] - z[76] - z[77]

    return z
