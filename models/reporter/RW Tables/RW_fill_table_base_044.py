# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM

SZO Fixed cross-referencing on 7/10/2024
Fixed all issues on 8/23/2024. 

"""


def fill_table_base_044(dfd, table_spec, table_id):
    """Fill table    Industrial Consumption by Sector
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
    MNUMPR = dfd["MNUMPR_rwpre"]
    MNUMCR = dfd["MNUMCR_rwpre"]
    RDAYS = dfd["RDAYS_rwpre"]

    #   Industrial Consumption by Sector
    #   (trillion Btu)
    #
    #   Manufacturing Heat and Power
    
    #    Residual Fuel Oil
    # T44(1,IY,IS)=MANHP(IXRF,5,IY)
    z[1] = dfd["MANHP"].loc[IXRF].loc[5]
    
    #    Distillate Fuel Oil
    # T44(2,IY,IS)=MANHP(IXDS,5,IY)
    z[2] = dfd["MANHP"].loc[IXDS].loc[5]

    #    Propane
    # T44(3,IY,IS)=MANHP(IXLG,5,IY)
    z[3] = dfd["MANHP"].loc[IXLG].loc[5]

    #    Petroleum Coke
    # T44(4,IY,IS)=MANHP(IXPC,5,IY)
    z[4] = dfd["MANHP"].loc[IXPC].loc[5]

    #    Still Gas
    # T44(5,IY,IS)=MANHP(IXSG,5,IY)
    z[5] = dfd["MANHP"].loc[IXSG].loc[5]

    #    Other Petroleum
    # T44(6,IY,IS)=MANHP(IXOP,5,IY)
    z[6] = dfd["MANHP"].loc[IXOP].loc[5]

    #      Petroleum and Other Liquids Subtotal
    # T44(7,IY,IS)=FSUM(T44(1,IY,IS),6)
    z[7] = z[1] + z[2] + z[3] + z[4] + z[5] + z[6]

    #    Natural Gas
    # T44(8,IY,IS)=MANHP(IXNG,5,IY)-RFQNGPF(11,IY)
    z[8] = (
        dfd["MANHP"].loc[IXNG].loc[5]
        #- dfd["RFQNGPF"].loc[11]
    )

    #    Metallurgical Coal
    # T44(9,IY,IS)=MANHP(IXMC,5,IY)
    z[9] = dfd["MANHP"].loc[IXMC].loc[5]

    #    Net Coke Imports
    # T44(10,IY,IS)=MANHP(IXCI,5,IY)
    z[10] = dfd["MANHP"].loc[IXCI].loc[5]

    #    Steam Coal
    # T44(11,IY,IS)=MANHP(IXCL,5,IY)
    z[11] = dfd["MANHP"].loc[IXCL].loc[5]

    #      Coal Subtotal
    # T44(12,IY,IS)=FSUM(T44(9,IY,IS),3)
    z[12] = z[9] + z[10] + z[11]

    #    Renewables
    # T44(13,IY,IS)=MANHP(IXRN,5,IY)
    z[13] = dfd["MANHP"].loc[IXRN].loc[5]

    #    Biofuels Heat and Coproducts
    # T44(56,IY,IS)=1000.*((CORNCD(3,11,IY)*CFCORN/1000.-0.9751*(CRNETHCD(11,IY)+OTHETHCD(11,IY))*365.*CFPET-RFBIOBUTECD(MNUMCR,IY)*365.*CFBIOBUTE(IY))/1000000.+SUM(BIMQTYCD(1:4,11,IY))/1000000.*RDAYS*(CFVEGGIE(IY)-CFBIOD(IY))-RDAYS/1000000.*(SUM(BTLFRAC(1:4,MNUMPR,IY))*CFBTLLIQ(IY)+SUM(CBTLFRAC(2,1:4,MNUMPR,IY))*CFCBTLLIQ(2,IY)+UBAVOL(MNUMPR,IY)*5.763))
    # z[56] = 1000.* ((dfd['CORNCD'].loc[3].loc[11] * dfd['CFCORN'] / 1000. - 0.9751 * (dfd['CRNETHCD'].loc[11] + dfd['OTHETHCD'].loc[11]) * 365. * dfd['CFPET'] - dfd['RFBIOBUTECD'].loc[MNUMCR] * 365. * dfd['CFBIOBUTE']) / 1000000. + dfd['BIMQTYCD'].loc[((1,2,3,4),11),:].sum() / 1000000. * RDAYS * (dfd['CFVEGGIE'] - dfd['CFBIOD']) - RDAYS / 1000000. * (dfd['BTLFRAC'].loc[((1,2,3,4),MNUMPR),:].sum() * dfd['CFBTLLIQ'] + dfd['CBTLFRAC'].loc[(2,(1,2,3,4),MNUMPR),:].sum() * dfd['CFCBTLLIQ'].loc[2] + dfd['UBAVOL'].loc[MNUMPR] * 5.763))

    z[56] = 1000.0 * (
        (
            dfd["CORNCD"].loc[3].loc[MNUMCR] * dfd["CFCORN"].iloc[0] / 1000
            - dfd["ETHANOL_PARAM_rwpre"]
            * (dfd["CRNETHCD"].loc[MNUMCR] + dfd["OTHETHCD"].loc[MNUMCR])
            * RDAYS
            * dfd["CFPET"].iloc[0]
            - dfd["RFBIOBUTECD"].loc[MNUMCR] * RDAYS * dfd["CFBIOBUTE"]
        )
        / 1000000
        + dfd["BIMQTYCD"].loc[1:4, MNUMCR, :].sum()
        / 1000000
        * RDAYS
        * (dfd["CFVEGGIE"] - dfd["CFBIOD"])
        - RDAYS
        / 1000000
        * (
            dfd["BTLFRAC"].loc[1:4, MNUMPR, :].sum() * dfd["CFBTLLIQ"]
            + dfd["CBTLFRAC"].loc[2, 1:4, MNUMPR, :].sum() * dfd["CFCBTLLIQ"].loc[2]
            + dfd["UBAVOL"].loc[MNUMPR] * dfd["CFUBA_rwpre"]
        )
        + (
            (
                dfd["ETHANOL_PARAM_rwpre"]
                * dfd["CLLETHCD"].loc[MNUMCR]
                * RDAYS
                * (
                    dfd["CFBMQ"] * dfd["GAL_PER_BBL_rwpre"] / dfd["CONEFF"]
                    - dfd["CFPET"].iloc[0]
                )
            )
            / 1000000.0
        )
    )

    #    Purchased Electricity
    # T44(14,IY,IS)=MANHP(IXEL,5,IY)
    z[14] = dfd["MANHP"].loc[IXEL].loc[5]

    #      Manufacturing Total
    # T44(15,IY,IS)=SUM(T44(12:14,IY,IS))+SUM(T44(7:8,IY,IS))+T44(56,IY,IS)
    z[15] = z[12] + z[13] + z[14] + z[7] + z[8] + z[56]

    #

    #   Nonmanufacturing Heat and Power
    #    Residual Fuel Oil
    # T44(16,IY,IS)=NONHP(IXRF,5,IY)
    z[16] = dfd["NONHP"].loc[IXRF].loc[5]

    #    Distillate Fuel Oil
    # T44(17,IY,IS)=NONHP(IXDS,5,IY)
    z[17] = dfd["NONHP"].loc[IXDS].loc[5]

    #    Propane
    # T44(18,IY,IS)=NONHP(IXLG,5,IY)
    z[18] = dfd["NONHP"].loc[IXLG].loc[5]

    #    Motor Gasoline
    # T44(19,IY,IS)=NONHP(IXMG,5,IY)
    z[19] = dfd["NONHP"].loc[IXMG].loc[5]

    #    Other Petroleum
    # T44(51,IY,IS)=NONHP(IXOP,5,IY)
    z[51] = dfd["NONHP"].loc[IXOP].loc[5]

    #      Petroleum and Other Liquids Subtotal
    # T44(20,IY,IS)=FSUM(T44(16,IY,IS),4)+T44(51,IY,IS)
    z[20] = z[16] + z[17] + z[18] + z[19] + z[51]

    #    Natural Gas
    # T44(21,IY,IS)=NONHP(IXNG,5,IY)
    z[21] = dfd["NONHP"].loc[IXNG].loc[5]

    #    Lease and Plant Fuel 1/
    # T44(55,IY,IS)=QLPIN(11,IY)*1000.
    z[55] = dfd["QLPIN"].loc[11]

    #    Steam Coal
    # T44(22,IY,IS)=NONHP(IXCL,5,IY)
    z[22] = dfd["NONHP"].loc[IXCL].loc[5]

    #    Renewables
    # T44(52,IY,IS)=NONHP(IXRN,5,IY)
    z[52] = dfd["NONHP"].loc[IXRN].loc[5]

    #    Purchased Electricity excluding Oil Shale
    # T44(23,IY,IS)=NONHP(IXEL,5,IY)
    z[23] = dfd["NONHP"].loc[IXEL].loc[5]

    #    Purchased Electricity for Oil Shale
    # T44(53,IY,IS)=OGELSHALE(IY)
    z[53] = dfd["OGELSHALE"]

    #      Nonmanufacturing Total
    # T44(24,IY,IS)=FSUM(T44(20,IY,IS),4)+FSUM(T44(52,IY,IS),4)
    z[24] = z[20] + z[21] + z[22] + z[23] + z[52] + z[53] + z[55]

    #   Feedstock and Miscellaneous

    #    Natural Gas Feedstocks
    # T44(25,IY,IS)=MANHP(IXNF,5,IY)+NONHP(IXNF,5,IY)+RFQNGPF(11,IY)
    z[25] = (
        dfd["MANHP"].loc[IXNF].loc[5]
        + dfd["NONHP"].loc[IXNF].loc[5]
        #+ dfd["RFQNGPF"].loc[11]
    )

    #   Hydrogen Feedstocks
    # (new row)
    z[59] = dfd["MANHP"].loc[IXHF].loc[5] + dfd["NONHP"].loc[IXHF].loc[5]
    
    #    Hydrocarbon Gas Liquid Feedstocks 2/
    z[26] = dfd["INQLGPF"].loc[MNUMCR]

    #    Petrochemical Feedstocks
    # T44(27,IY,IS)=MANHP(IXPF,5,IY)+NONHP(IXPF,5,IY)
    z[27] = dfd["MANHP"].loc[IXPF].loc[5] + dfd["NONHP"].loc[IXPF].loc[5]

    #    Asphalt and Road Oil
    # T44(28,IY,IS)=MANHP(IXAS,5,IY)+NONHP(IXAS,5,IY)
    z[28] = dfd["MANHP"].loc[IXAS].loc[5] + dfd["NONHP"].loc[IXAS].loc[5]

    #      Feedstock and Miscellaneous Total
    # T44(29,IY,IS)=FSUM(T44(25,IY,IS),4)
    z[29] = z[25] + z[26] + z[27] + z[28] + z[59]

    #   Total Itemized Consumption
    # T44(30,IY,IS)=T44(15,IY,IS)+T44(24,IY,IS)+T44(29,IY,IS)
    z[30] = z[15] + z[24] + z[29]

    #   Total Industrial Consumption

    # T44(57,IY,IS)=T44(58,IY,IS)-T44(30,IY,IS)
    # ftab.f ---
    # if(ir.eq.11) T44(58,IY,IS) = 1000*T2(39,11,IY,IS) ! copy industrial delivered energy total for use in Table 44 balancing item
    #
    # Create a place holder - calc in post processor
    z[58] = z[30] * 0.0

    #   Nonitemized and Benchmarking Adjustments
    # Create a place holder - calc in post processor
    # z[57] = z[58]-z[30]
    z[57] = z[30] * 0.0

    #   Combined Heat and Power 3/
    #     Generating Capacity (gigawatts) 4/
    # Note: CHP still only has 21 indices; has not been updated to represent the 4 new industries for AEO2025
    #       Petroleum
    # T44(32,IY,IS)=SUM(CHPINDCAP(2,1:21,IY))+(CGREFCAP(11,IY,2)+CGOGSCAP(11,IY,2))*.001
    z[32] = (
        dfd["CHPINDCAP"].loc[2, 1:21, :].sum()
        + (dfd["CGREFCAP"].loc[11].loc[2] + dfd["CGOGSCAP"].loc[11].loc[2]) * 0.001
    )

    #       Natural Gas
    # T44(33,IY,IS)=SUM(CHPINDCAP(3,1:21,IY))+(CGREFCAP(11,IY,3)+CGOGSCAP(11,IY,3))*.001
    z[33] = (
        dfd["CHPINDCAP"].loc[3, 1:21, :].sum()
        + (dfd["CGREFCAP"].loc[11].loc[3] + dfd["CGOGSCAP"].loc[11].loc[3]) * 0.001
    )

    #       Coal
    # T44(31,IY,IS)=SUM(CHPINDCAP(1,1:21,IY))+(CGREFCAP(11,IY,1)+CGOGSCAP(11,IY,1))*.001
    z[31] = (
        dfd["CHPINDCAP"].loc[1, 1:21, :].sum()
        + (dfd["CGREFCAP"].loc[11].loc[1] + dfd["CGOGSCAP"].loc[11].loc[1]) * 0.001
    )

    #       Other 5/
    # T44(34,IY,IS)=SUM(CHPINDCAP(4:6,1:21,IY))+(CGREFCAP(11,IY,6)+CGREFCAP(11,IY,7)+CGREFCAP(11,IY,9)+CGOGSCAP(11,IY,4))*.001
    z[34] = (
        dfd["CHPINDCAP"].loc[4:6, 1:21, :].sum()
        + (
            dfd["CGREFCAP"].loc[11].loc[6]
            + dfd["CGREFCAP"].loc[11].loc[7]
            + dfd["CGREFCAP"].loc[11].loc[9]
            + dfd["CGOGSCAP"].loc[11].loc[4]
        )
        * 0.001
    )

    #         Total
    # T44(35,IY,IS)=SUM(CHPINDCAP(7,1:21,IY))+(SUM(CGREFCAP(11,IY,1:12))+SUM(CGOGSCAP(11,IY,1:4)))*.001
    z[35] = (
        dfd["CHPINDCAP"].loc[7, 1:21, :].sum()
        + (
            dfd["CGREFCAP"].loc[11, 1:12, :].sum()
            + dfd["CGOGSCAP"].loc[11, 1:4, :].sum()
        )
        * 0.001
    )

    #     Net Generation (billion kilowatthours)

    #       Petroleum
    # T44(37,IY,IS)=SUM(CHPINDGEN(2,1:21,IY))+(CGREFGEN(11,IY,2,1)+CGREFGEN(11,IY,2,2))*.001+(CGOGSGEN(11,IY,2,1)+CGOGSGEN(11,IY,2,2))*.001
    z[37] = (
        dfd["CHPINDGEN"].loc[2, 1:21, :].sum()
        + (
            dfd["CGREFGEN"].loc[11].loc[2].loc[1]
            + dfd["CGREFGEN"].loc[11].loc[2].loc[2]
        )
        * 0.001
        + (
            dfd["CGOGSGEN"].loc[11].loc[2].loc[1]
            + dfd["CGOGSGEN"].loc[11].loc[2].loc[2]
        )
        * 0.001
    )

    #       Natural Gas
    # T44(38,IY,IS)=SUM(CHPINDGEN(3,1:21,IY))+(CGREFGEN(11,IY,3,1)+CGREFGEN(11,IY,3,2))*.001+(CGOGSGEN(11,IY,3,1)+CGOGSGEN(11,IY,3,2))*.001
    z[38] = (
        dfd["CHPINDGEN"].loc[3, 1:21, :].sum()
        + (
            dfd["CGREFGEN"].loc[11].loc[3].loc[1]
            + dfd["CGREFGEN"].loc[11].loc[3].loc[2]
        )
        * 0.001
        + (
            dfd["CGOGSGEN"].loc[11].loc[3].loc[1]
            + dfd["CGOGSGEN"].loc[11].loc[3].loc[2]
        )
        * 0.001
    )

    #       Coal
    # T44(36,IY,IS)=SUM(CHPINDGEN(1,1:21,IY))+(CGREFGEN(11,IY,1,1)+CGREFGEN(11,IY,1,2))*.001+(CGOGSGEN(11,IY,1,1)+CGOGSGEN(11,IY,1,2))*.001
    z[36] = (
        dfd["CHPINDGEN"].loc[1, 1:21, :].sum()
        + (
            dfd["CGREFGEN"].loc[11].loc[1].loc[1]
            + dfd["CGREFGEN"].loc[11].loc[1].loc[2]
        )
        * 0.001
        + (
            dfd["CGOGSGEN"].loc[11].loc[1].loc[1]
            + dfd["CGOGSGEN"].loc[11].loc[1].loc[2]
        )
        * 0.001
    )

    #       Other 5/
    # T44(39,IY,IS)=SUM(CHPINDGEN(4:6,1:21,IY))+(CGREFGEN(11,IY,6,1)+CGREFGEN(11,IY,6,2)+CGREFGEN(11,IY,7,1)+CGREFGEN(11,IY,7,2)+CGREFGEN(11,IY,9,1)+CGREFGEN(11,IY,9,2))*.001+(CGOGSGEN(11,IY,4,1)+CGOGSGEN(11,IY,4,2))*.001
    z[39] = (
        dfd["CHPINDGEN"].loc[4:6, 1:21, :].sum()
        + (
            dfd["CGREFGEN"].loc[11].loc[6].loc[1]
            + dfd["CGREFGEN"].loc[11].loc[6].loc[2]
            + dfd["CGREFGEN"].loc[11].loc[7].loc[1]
            + dfd["CGREFGEN"].loc[11].loc[7].loc[2]
            + dfd["CGREFGEN"].loc[11].loc[9].loc[1]
            + dfd["CGREFGEN"].loc[11].loc[9].loc[2]
        )
        * 0.001
        + (
            dfd["CGOGSGEN"].loc[11].loc[4].loc[1]
            + dfd["CGOGSGEN"].loc[11].loc[4].loc[2]
        )
        * 0.001
    )

    #         Total
    # T44(40,IY,IS)=SUM(CHPINDGEN(7,1:21,IY))+SUM(CGREFGEN(11,IY,1:12,1:2))*.001+SUM(CGOGSGEN(11,IY,1:4,1:2))*.001
    z[40] = (
        dfd["CHPINDGEN"].loc[7, 1:21, :].sum()
        + dfd["CGREFGEN"].loc[11, 1:12, 1:2, :].sum() * 0.001
        + dfd["CGOGSGEN"].loc[11, 1:4, 1:2, :].sum() * 0.001
    )

    #       Disposition

    #         Sales to the Grid
    # T44(41,IY,IS)=(SUM(CGINDLGEN(11,IY,1:12,1))+SUM(CGREFGEN(11,IY,1:12,1))+SUM(CGOGSGEN(11,IY,1:4,1)))*.001
    z[41] = (
        dfd["CGINDLGEN"].loc[11, 1:12, 1, :].sum()
        + dfd["CGREFGEN"].loc[11, 1:12, 1, :].sum()
        + dfd["CGOGSGEN"].loc[11, 1:4, 1, :].sum()
    ) * 0.001

    #         Generation for Own Use
    # T44(42,IY,IS)=(SUM(CGINDLGEN(11,IY,1:12,2))+SUM(CGREFGEN(11,IY,1:12,2))+SUM(CGOGSGEN(11,IY,1:4,2)))*.001
    z[42] = (
        dfd["CGINDLGEN"].loc[11, 1:12, 2, :].sum()
        + dfd["CGREFGEN"].loc[11, 1:12, 2, :].sum()
        + dfd["CGOGSGEN"].loc[11, 1:4, 2, :].sum()
    ) * 0.001

    return z
