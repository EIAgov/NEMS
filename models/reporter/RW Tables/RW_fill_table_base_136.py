# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_136(dfd, table_spec, table_id):
    """Fill table   Key Results for Nuclear Cost Cases
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

    FYRPRC = int(table_spec["settings"]["year_index_real_dol_priceyr"])
    # Get inflation index from 1990->2050
    MC_JPGDP = dfd["MC_JPGDP"].loc[1990:]

    # Get inflation index of FYRPRC (2022)
    SCALPR2 = MC_JPGDP.loc[FYRPRC]  # 2022 = column 33

    GWh_to_TRIL = dfd["GWh_to_TRIL_rwpre"]
    MNUMNR = dfd["MNUMNR_rwpre"]
    MNUMCR = dfd["MNUMCR_rwpre"]

    #   Key Results for Nuclear Cost Cases
    #   (gigawatts, unless otherwise noted)
    #    Net Summer Capacity

    #   Capacity

    #     Coal
    # T136(1,IY,IS)=T9(1,IY,IS)+T9(51,IY,IS)
    z[1] = (
        dfd["UCAPCSU"].loc[MNUMNR]
        + dfd["UCAPCSN"].loc[MNUMNR]
        + dfd["UCAPCSC"].loc[MNUMNR]
    )

    #     Oil and Natural Gas Steam
    # T136(2,IY,IS)=T9(2,IY,IS)+T9(52,IY,IS)
    z[2] = (
        dfd["UCAPOSU"].loc[MNUMNR]
        + dfd["UCAPOSN"].loc[MNUMNR]
        + dfd["UCAPNGU"].loc[MNUMNR]
        + dfd["UCAPNGN"].loc[MNUMNR]
        + dfd["UCAPOSC"].loc[MNUMNR]
    )

    #     Combined Cycle
    # T136(3,IY,IS)=T9(3,IY,IS)+T9(53,IY,IS)
    z[3] = (
        dfd["UCAPCCU"].loc[MNUMNR]
        + dfd["UCAPCCN"].loc[MNUMNR]
        + dfd["UCAPCCC"].loc[MNUMNR]
    )

    #     Combustion Turbine/Diesel
    # T136(4,IY,IS)=T9(4,IY,IS)+T9(54,IY,IS)
    z[4] = (
        dfd["UCAPCTU"].loc[MNUMNR]
        + dfd["UCAPCTN"].loc[MNUMNR]
        + dfd["UCAPCTC"].loc[MNUMNR]
    )

    #     Nuclear Power
    # T136(5,IY,IS)=T9(5,IY,IS)
    z[5] = (
        dfd["UCAPNUU"].loc[MNUMNR]
        + dfd["UCAPNUN"].loc[MNUMNR]
        + dfd["UCAPSMU"].loc[MNUMNR]
        + dfd["UCAPSMN"].loc[MNUMNR]
    )

    #     Pumped Storage
    # T136(6,IY,IS)=T9(6,IY,IS)
    z[6] = dfd["UCAPPSU"].loc[MNUMNR] + dfd["UCAPPSN"].loc[MNUMNR]

    #     Fuel Cells
    # T136(7,IY,IS)=T9(7,IY,IS)
    z[7] = dfd["UCAPFCU"].loc[MNUMNR] + dfd["UCAPFCN"].loc[MNUMNR]

    #     Renewable Sources
    # T136(8,IY,IS)=T9(8,IY,IS)+T9(55,IY,IS)
    z[8] = (
        dfd["UCAPHYU"].loc[MNUMNR]
        + dfd["UCAPGEU"].loc[MNUMNR]
        + dfd["UCAPMSU"].loc[MNUMNR]
        + dfd["UCAPWDU"].loc[MNUMNR]
        + dfd["UCAPSTU"].loc[MNUMNR]
        + dfd["UCAPPVU"].loc[MNUMNR]
        + dfd["UCAPWNU"].loc[MNUMNR]
        + dfd["UCAPWFU"].loc[MNUMNR]
        + dfd["UCAPPTU"].loc[MNUMNR]
        + dfd["UCAPWLU"].loc[MNUMNR]
        + dfd["UCAPHYN"].loc[MNUMNR]
        + dfd["UCAPGEN"].loc[MNUMNR]
        + dfd["UCAPMSN"].loc[MNUMNR]
        + dfd["UCAPWDN"].loc[MNUMNR]
        + dfd["UCAPSTN"].loc[MNUMNR]
        + dfd["UCAPPVN"].loc[MNUMNR]
        + dfd["UCAPWNN"].loc[MNUMNR]
        + dfd["UCAPWFN"].loc[MNUMNR]
        + dfd["UCAPPTN"].loc[MNUMNR]
        + dfd["UCAPWLN"].loc[MNUMNR]
        + dfd["UCAPHYC"].loc[MNUMNR]
        + dfd["UCAPGEC"].loc[MNUMNR]
        + dfd["UCAPMSC"].loc[MNUMNR]
        + dfd["UCAPWDC"].loc[MNUMNR]
        + dfd["UCAPSTC"].loc[MNUMNR]
        + dfd["UCAPPVC"].loc[MNUMNR]
        + dfd["UCAPWNC"].loc[MNUMNR]
        + dfd["UCAPWFC"].loc[MNUMNR]
        + dfd["UCAPWLC"].loc[MNUMNR]
        + dfd["UCAPPTC"].loc[MNUMNR]
    )

    #     Distributed Generation
    # T136(9,IY,IS)=T9(48,IY,IS)
    z[9] = (
        dfd["UCAPDBU"].loc[MNUMNR]
        + dfd["UCAPDBN"].loc[MNUMNR]
        + dfd["UCAPDPU"].loc[MNUMNR]
        + dfd["UCAPDPN"].loc[MNUMNR]
    )

    #     Combined Heat and Power
    # T136(10,IY,IS)=T9(44,IY,IS)

    z[10] = (
        dfd["CGREFCAP"].loc[MNUMCR].loc[1]
        + dfd["CGOGSCAP"].loc[MNUMCR].loc[1]
        + dfd["CGINDLCAP"].loc[MNUMCR].loc[1]
        + dfd["CGCOMMCAP"].loc[MNUMCR].loc[1]
        + dfd["CGREFCAP"].loc[MNUMCR].loc[2]
        + dfd["CGOGSCAP"].loc[MNUMCR].loc[2]
        + dfd["CGINDLCAP"].loc[MNUMCR].loc[2]
        + dfd["CGCOMMCAP"].loc[MNUMCR].loc[2]
        + dfd["CGREFCAP"].loc[MNUMCR].loc[3]
        + dfd["CGOGSCAP"].loc[MNUMCR].loc[3]
        + dfd["CGINDLCAP"].loc[MNUMCR].loc[3]
        + dfd["CGCOMMCAP"].loc[MNUMCR].loc[3]
        + dfd["CGRESCAP"].loc[MNUMCR].loc[3]
        + dfd["CGINDLCAP"].loc[MNUMCR].loc[9]
        + dfd["CGREFCAP"].loc[MNUMCR].loc[9]
        + dfd["CGCOMMCAP"].loc[MNUMCR].loc[9]
        + dfd["CGREFCAP"].loc[MNUMCR].loc[6]
        + dfd["CGREFCAP"].loc[MNUMCR].loc[7]
        + dfd["CGINDLCAP"].loc[MNUMCR].loc[6]
        + dfd["CGINDLCAP"].loc[MNUMCR].loc[7]
        + dfd["CGCOMMCAP"].loc[MNUMCR].loc[6]
        + dfd["CGCOMMCAP"].loc[MNUMCR].loc[7]
        + dfd["CGOGSCAP"].loc[MNUMCR].loc[4]
        + dfd["CGINDLCAP"].loc[MNUMCR].loc[10]
        + dfd["CGRESCAP"].loc[MNUMCR].loc[8]
        + dfd["CGRESCAP"].loc[MNUMCR].loc[11]
        + dfd["CGCOMMCAP"].loc[MNUMCR].loc[8]
        + dfd["CGINDLCAP"].loc[MNUMCR].loc[8]
        + dfd["CGINDLCAP"].loc[MNUMCR].loc[4]
        + dfd["CGCOMMCAP"].loc[MNUMCR].loc[4]
        + dfd["CGCOMMCAP"].loc[MNUMCR].loc[5]
        + dfd["CGCOMMCAP"].loc[MNUMCR].loc[11]
        + dfd["CGINDLCAP"].loc[MNUMCR].loc[5]
        + dfd["CGINDLCAP"].loc[MNUMCR].loc[11]
        + dfd["CGCOMMCAP"].loc[MNUMCR].loc[10]
    ) * 0.001

    #       Total
    # T136(11,IY,IS)=FSUM(T136(1,IY,IS),10)
    z[11] = z[1] + z[2] + z[3] + z[4] + z[5] + z[6] + z[7] + z[8] + z[9] + z[10]

    #

    #   Cumulative Additions

    #     Coal
    # T136(12,IY,IS)=T9(10,IY,IS)+T9(19,IY,IS)
    z[12] = (
        dfd["UADDCSU"].loc[1].loc[MNUMNR]
        + dfd["UADDCSN"].loc[1].loc[MNUMNR]
        + dfd["UADDCSC"].loc[MNUMNR]
        + dfd["UADDCSU"].loc[2].loc[MNUMNR]
        + dfd["UADDCSN"].loc[2].loc[MNUMNR]
    )

    #     Oil and Natural Gas Steam
    # T136(13,IY,IS)=T9(11,IY,IS)+T9(20,IY,IS)
    z[13] = (
        dfd["UADDOSU"].loc[1].loc[MNUMNR]
        + dfd["UADDOSN"].loc[1].loc[MNUMNR]
        + dfd["UADDOSC"].loc[MNUMNR]
        + dfd["UADDOSU"].loc[2].loc[MNUMNR]
        + dfd["UADDOSN"].loc[2].loc[MNUMNR]
    )

    #     Combined Cycle
    # T136(14,IY,IS)=T9(12,IY,IS)+T9(21,IY,IS)

    z[14] = (
        dfd["UADDCCU"].loc[1].loc[MNUMNR]
        + dfd["UADDCCN"].loc[1].loc[MNUMNR]
        + dfd["UADDCCC"].loc[MNUMNR]
        + dfd["UADDCCU"].loc[2].loc[MNUMNR]
        + dfd["UADDCCN"].loc[2].loc[MNUMNR]
    )

    #    Combustion Turbine / Diesel
    # T136(15,IY,IS) = T9(13,IY,IS) + T9(22,IY,IS)
    z[15] = (
        dfd["UADDCTU"].loc[:, MNUMNR, :].sum()
        + dfd["UADDCTN"].loc[:, MNUMNR, :].sum()
        + dfd["UADDCTC"].loc[MNUMNR]
    )

    #    Nuclear Power
    # T136(16,IY,IS) = T9(14,IY,IS) + T9(23,IY,IS)
    z[16] = (
        dfd["UADDNUU"].loc[:, MNUMNR, :].sum()
        + dfd["UADDNUN"].loc[:, MNUMNR, :].sum()
        + dfd["UADDSMU"].loc[:, MNUMNR, :].sum()
        + dfd["UADDSMN"].loc[:, MNUMNR, :].sum()
    )

    #    Pumped Storage
    # T136(17,IY,IS) = T9(15,IY,IS) + T9(24,IY,IS)
    z[17] = (
        dfd["UADDPSU"].loc[:, MNUMNR, :].sum() + dfd["UADDPSN"].loc[:, MNUMNR, :].sum()
    )

    #    Fuel Cells
    # T136(18,IY,IS) = T9(16,IY,IS) + T9(25,IY,IS)
    z[18] = (
        dfd["UADDFCU"].loc[:, MNUMNR, :].sum() + dfd["UADDFCN"].loc[:, MNUMNR, :].sum()
    )

    #    Renewable Sources
    # T136(19,IY,IS) = T9(17,IY,IS) + T9(26,IY,IS)
    z[19] = (
        dfd["UADDRNU"].loc[:, MNUMNR, :].sum()
        + dfd["UADDRNN"].loc[:, MNUMNR, :].sum()
        + dfd["UADDHYC"].loc[MNUMNR]
        + dfd["UADDGEC"].loc[MNUMNR]
        + dfd["UADDMSC"].loc[MNUMNR]
        + dfd["UADDWDC"].loc[MNUMNR]
        + dfd["UADDSTC"].loc[MNUMNR]
        + dfd["UADDPVC"].loc[MNUMNR]
        + dfd["UADDWNC"].loc[MNUMNR]
        + dfd["UADDWFC"].loc[MNUMNR]
    )

    #   Distributed Generation
    # T136(20,IY,IS) = T9(49,IY,IS) + T9(50,IY,IS)
    z[20] = (
        dfd["UADDDBU"].loc[:, MNUMNR, :].sum()
        + dfd["UADDDBN"].loc[:, MNUMNR, :].sum()
        + dfd["UADDDPU"].loc[:, MNUMNR, :].sum()
        + dfd["UADDDPN"].loc[:, MNUMNR, :].sum()
    )

    #   Combined Heat and Power - See RW_postprocessor_base
    # T136(21,IY,IS) = T9(45,IY,IS)
    z[21] = z[20].copy() * 0

    #   Total - See RW_postprocessor_base
    # T136(22,IY,IS) = FSUM(T136(12,IY,IS),10)
    z[22] = z[20].copy() * 0

    #   Cumulative Retirements
    # T136(23,IY,IS)=T9(37,IY,IS)
    z[23] = dfd["URETTLU"].loc[MNUMNR]

    #

    #   Generation by Fuel (billion kilowatthours)

    #     Coal
    # T136(24,IY,IS)=T8(1,IY,IS)+T8(42,IY,IS)
    z[24] = (
        dfd["UGNCLNR"].loc[1].loc[MNUMNR]
        + dfd["UGNCLNR"].loc[2].loc[MNUMNR]
        + (
            (
                dfd["CGNTGEN"].loc[MNUMNR].loc[1].loc[1]
                + dfd["CGNTGEN"].loc[MNUMNR].loc[1].loc[2]
            )
            * 0.001
        )
    )

    #     Petroleum
    # T136(25,IY,IS)=T8(2,IY,IS)+T8(43,IY,IS)
    z[25] = (
        dfd["UGNDSNR"].loc[1].loc[MNUMNR]
        + dfd["UGNDSNR"].loc[2].loc[MNUMNR]
        + dfd["UGNRHNR"].loc[1].loc[MNUMNR]
        + dfd["UGNRHNR"].loc[2].loc[MNUMNR]
        + dfd["UGNRLNR"].loc[1].loc[MNUMNR]
        + dfd["UGNRLNR"].loc[2].loc[MNUMNR]
        + (
            dfd["CGNTGEN"].loc[MNUMNR].loc[2].loc[1]
            + dfd["CGNTGEN"].loc[MNUMNR].loc[2].loc[2]
        )
        * 0.001
    )

    #     Natural Gas
    # T136(26,IY,IS)=T8(3,IY,IS)+T8(44,IY,IS)
    z[26] = (
        dfd["UGNGFNR"].loc[1].loc[MNUMNR]
        + dfd["UGNGFNR"].loc[2].loc[MNUMNR]
        + dfd["UGNGINR"].loc[1].loc[MNUMNR]
        + dfd["UGNGINR"].loc[2].loc[MNUMNR]
        + (
            dfd["CGNTGEN"].loc[MNUMNR].loc[3].loc[1]
            + dfd["CGNTGEN"].loc[MNUMNR].loc[3].loc[2]
        )
        * 0.001
    )

    #     Nuclear Power
    # T136(27,IY,IS)=T8(4,IY,IS)
    z[27] = dfd["UGNURNR"].loc[1].loc[MNUMNR] + dfd["UGNURNR"].loc[2].loc[MNUMNR]

    #     Pumped Storage / Other
    # T136(28,IY,IS)=T8(5,IY,IS)+T8(45,IY,IS)
    z[28] = (
        dfd["UGNPSNR"].loc[1].loc[MNUMNR]
        + dfd["UGNPSNR"].loc[2].loc[MNUMNR]
        + dfd["UGNSDNR"].loc[1].loc[MNUMNR]
        + dfd["UGNSDNR"].loc[2].loc[MNUMNR]
        + (
            dfd["CGNTGEN"].loc[MNUMNR].loc[9].loc[1]
            + dfd["CGNTGEN"].loc[MNUMNR].loc[9].loc[2]
            + dfd["CGNTGEN"].loc[MNUMNR].loc[10].loc[1]
            + dfd["CGNTGEN"].loc[MNUMNR].loc[10].loc[2]
        )
        * 0.001
    )

    #     Renewable Sources
    # T136(29,IY,IS)=T8(6,IY,IS)+T8(46,IY,IS)
    z[29] = (
        dfd["UGNHYNR"].loc[1].loc[MNUMNR]
        + dfd["UGNHYNR"].loc[2].loc[MNUMNR]
        + dfd["UGNGENR"].loc[1].loc[MNUMNR]
        + dfd["UGNGENR"].loc[2].loc[MNUMNR]
        + dfd["UGNMSNR"].loc[1].loc[MNUMNR]
        + dfd["UGNMSNR"].loc[2].loc[MNUMNR]
        + dfd["UGNWDNR"].loc[1].loc[MNUMNR]
        + dfd["UGNWDNR"].loc[2].loc[MNUMNR]
        + dfd["UGNSONR"].loc[1].loc[MNUMNR]
        + dfd["UGNSONR"].loc[2].loc[MNUMNR]
        + dfd["UGNPVNR"].loc[1].loc[MNUMNR]
        + dfd["UGNPVNR"].loc[2].loc[MNUMNR]
        + dfd["UGNPTNR"].loc[1].loc[MNUMNR]
        + dfd["UGNPTNR"].loc[2].loc[MNUMNR]
        + dfd["UGNWNNR"].loc[1].loc[MNUMNR]
        + dfd["UGNWNNR"].loc[2].loc[MNUMNR]
        + dfd["UGNWLNR"].loc[1].loc[MNUMNR]
        + dfd["UGNWLNR"].loc[2].loc[MNUMNR]
        + dfd["UGNWFNR"].loc[1].loc[MNUMNR]
        + dfd["UGNWFNR"].loc[2].loc[MNUMNR]
        + (
            dfd["CGNTGEN"].loc[MNUMNR].loc[4].loc[1]
            + dfd["CGNTGEN"].loc[MNUMNR].loc[4].loc[2]
            + dfd["CGNTGEN"].loc[MNUMNR].loc[5].loc[1]
            + dfd["CGNTGEN"].loc[MNUMNR].loc[5].loc[2]
            + dfd["CGNTGEN"].loc[MNUMNR].loc[6].loc[1]
            + dfd["CGNTGEN"].loc[MNUMNR].loc[6].loc[2]
            + dfd["CGNTGEN"].loc[MNUMNR].loc[7].loc[1]
            + dfd["CGNTGEN"].loc[MNUMNR].loc[7].loc[2]
            + dfd["CGNTGEN"].loc[MNUMNR].loc[8].loc[1]
            + dfd["CGNTGEN"].loc[MNUMNR].loc[8].loc[2]
        )
        * 0.001
    )

    #     Distributed Generation
    # T136(30,IY,IS)=T8(41,IY,IS)
    z[30] = (
        dfd["UGNDDNR"].loc[1].loc[MNUMNR]
        + dfd["UGNDDNR"].loc[2].loc[MNUMNR]
        + dfd["UGNDGNR"].loc[1].loc[MNUMNR]
        + dfd["UGNDGNR"].loc[2].loc[MNUMNR]
    )

    #     Combined Heat and Power
    # T136(31,IY,IS)=T8(15,IY,IS)

    z[31] = (
        dfd["CGOGSGEN"].loc[11, 1:4, 1, :].sum()
        + dfd["CGOGSGEN"].loc[11, 1:4, 2, :].sum()
        + dfd["CGINDLGEN"].loc[11, 1:11, 1, :].sum()
        + dfd["CGINDLGEN"].loc[11, 1:11, 2, :].sum()
        + dfd["CGCOMMGEN"].loc[11, 1:11, 1, :].sum()
        + dfd["CGCOMMGEN"].loc[11, 1:11, 2, :].sum()
        + dfd["CGREFGEN"].loc[11, 1:3, 1, :].sum()
        + dfd["CGREFGEN"].loc[11, 1:3, 2, :].sum()
        + dfd["CGREFGEN"].loc[11, 6:7, 1, :].sum()
        + dfd["CGREFGEN"].loc[11, 6:7, 2, :].sum()
        + dfd["CGREFGEN"].loc[11, 9, 1:2, :].sum()
        + dfd["CGRESGEN"].loc[11, 3, 1:2, :].sum()
        + dfd["CGRESGEN"].loc[11, 8, 1:2, :].sum()
        + dfd["CGRESGEN"].loc[11, 11, 1:2, :].sum()
    ) * 0.001

    #       Total
    # T136(32,IY,IS)=FSUM(T136(24,IY,IS),8)
    z[32] = z[24] + z[25] + z[26] + z[27] + z[28] + z[29] + z[30] + z[31]

    #   Electric Power Sector Carbon Dioxide Emissions

    #   (million metric tons carbon_dd_)
    # z[33]=z[32]*0.0
    #     Petroleum
    # T136(33,IY,IS)=T17(25,MNUMCR,IY,IS) = sum(em_elec(1:3,MNUMCR,ICY))  ! Petroleum
    z[33] = (
        (
            dfd["EM_ELEC"].loc[1].loc[MNUMCR]
            + dfd["EM_ELEC"].loc[2].loc[MNUMCR]
            + dfd["EM_ELEC"].loc[3].loc[MNUMCR]
        )
        * dfd["C_CO2_FACTOR_rwpre"]
        / 1000.0
    )

    #     Natural Gas
    # T136(34,IY,IS)=T17(26,11,IY,IS)
    z[34] = dfd["EM_ELEC"].loc[5].loc[MNUMCR] * dfd["C_CO2_FACTOR_rwpre"] / 1000.0

    #     Coal
    # T136(35,IY,IS)=T17(27,11,IY,IS)
    z[35] = dfd["EM_ELEC"].loc[4].loc[MNUMCR] * dfd["C_CO2_FACTOR_rwpre"] / 1000.0

    #     Other
    # T136(40,IY,IS)=T17(28,11,IY,IS)
    z[40] = (
        (
            dfd["EM_ELEC"].loc[6].loc[MNUMCR]
            + dfd["EM_ELEC"].loc[7].loc[MNUMCR]
            + dfd["EM_ELEC"].loc[8].loc[MNUMCR]
        )
        * dfd["C_CO2_FACTOR_rwpre"]
        / 1000.0
    )

    #       Total
    # T136(36,IY,IS)=FSUM(T136(33,IY,IS),3)+T136(40,IY,IS)
    z[36] = z[33] + z[34] + z[35] + z[40]

    #

    #   Prices to the Electric Power Sector

    #     (#### dollars per million Btu)

    #     Petroleum
    # T136(37,IY,IS)=PTPEL(11,IY)
    z[37] = dfd["AMPBLK/PTPEL"].loc[MNUMCR] * SCALPR2

    #     Natural Gas
    # T136(38,IY,IS)=PNGEL(11,IY)
    z[38] = dfd["AMPBLK/PNGEL"].loc[MNUMCR] * SCALPR2

    #     Coal
    # T136(39,IY,IS)=PCLEL(11,IY)
    z[39] = dfd["AMPBLK/PCLEL"].loc[MNUMCR] * SCALPR2

    #

    #   Total Electricity Sales
    # T136(41,IY,IS)=QELAS(11,IY)/.003412
    z[41] = dfd["QELAS"].loc[MNUMCR] / GWh_to_TRIL

    #   Average Electricity Price
    # T136(42,IY,IS)=PELAS(11,IY)*.3412
    z[42] = dfd["PELAS"].loc[MNUMCR] * GWh_to_TRIL / 10.0 * SCALPR2

    return z
