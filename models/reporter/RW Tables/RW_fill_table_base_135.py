# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_135(dfd, table_spec, table_id):
    """Fill table  Key Results for Coal Mining Cost Cases
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
    SCALPR92 = MC_JPGDP.iloc[1992 - 1990]

    MNUMNR = dfd["MNUMNR_rwpre"]
    C_CO2_FACTOR = dfd["C_CO2_FACTOR_rwpre"]

    #   Key Results for Coal Mining Cost Cases
    #    Prices, Productivity, and Wages
    #
    #
    #   Minemouth Price

    #   (#### dollars per short ton)
    # T135(1,IY,IS)=CPSB(3,IY)*CPSBT(3,IY)
    z[1] = dfd["CPSB"].loc[3] * dfd["CPSBT"].loc[3] * SCALPR2

    #
    #   Delivered Price to Electricity Generators

    #   (#### dollars per million Btu)
    # T135(2,IY,IS)=PCLEL(11,IY)
    z[2] = dfd["AMPBLK/PCLEL"].loc[11] * SCALPR2

    #
    #   Labor Productivity

    #   (short tons per miner per hour)
    # T135(3,IY,IS)=TOTLABPROD(IY)
    z[3] = dfd["TOTLABPROD"]

    #

    #   Labor Productivity

    #   (average annual growth from @@@@)
    # T135(4,IY,IS)=LABPRODGROWTH(IY)
    z[4] = dfd["LABPRODGROWTH"]

    #

    #   Average Coal Miner Wage

    #   (#### dollars per hour)
    # T135(5,IY,IS)=WAGEPHOUR(IY)
    # WAGEPHOUR = WAGEPHOUR * SCALPR / MC_JPGDP(1992 - 1989)
    z[5] = dfd["WAGEPHOUR"] * SCALPR2 / SCALPR92

    #

    #   Average Coal Miner Wage

    #   (average annual growth from @@@@)
    # T135(6,IY,IS)=WAGEGROWTH(IY)
    z[6] = dfd["WAGEGROWTH"]

    #

    #   Carbon Dioxide Emissions, Electric Power Secto
    #   (million metric tons carbon_dd_ equivalent)

    #     Petroleum
    # T135(7,IY,IS)=EMEL(2,1,IY)
    z[7] = dfd["EMEL"].loc[2].loc[1] * C_CO2_FACTOR

    #     Natural Gas
    # T135(8,IY,IS)=EMEL(1,1,IY)
    z[8] = dfd["EMEL"].loc[1].loc[1] * C_CO2_FACTOR
    #     Coal
    # T135(9,IY,IS)=EMEL(3,1,IY)
    z[9] = dfd["EMEL"].loc[3].loc[1] * C_CO2_FACTOR

    #     Other
    # T135(10,IY,IS)=EMEL(4,1,IY)
    z[10] = dfd["EMEL"].loc[4].loc[1] * C_CO2_FACTOR

    #       Total
    # T135(11,IY,IS)=SUM(EMEL(1:4,1,IY))
    z[11] = z[7] + z[8] + z[9] + z[10]

    #

    #   Electric Power Sector Capacity
    #   (gigawatts)

    #     Coal
    # T135(12,IY,IS)=UCAPCSU(MNUMNR,IY)+UCAPCSN(MNUMNR,IY)+UCAPCSC(MNUMNR,IY)
    z[12] = (
        dfd["UCAPCSU"].loc[MNUMNR]
        + dfd["UCAPCSN"].loc[MNUMNR]
        + dfd["UCAPCSC"].loc[MNUMNR]
    )

    #     Non-Coal
    # T135(13,IY,IS)=UCAPCCU(MNUMNR,IY)+UCAPCCN(MNUMNR,IY)+UCAPCTU(MNUMNR,IY)+UCAPCTN(MNUMNR,IY)+UCAPFCU(MNUMNR,IY)+UCAPFCN(MNUMNR,IY)+UCAPNUU(MNUMNR,IY)+UCAPNUN(MNUMNR,IY)+UCAPSMU(MNUMNR,IY)+UCAPSMN(MNUMNR,IY)+UCAPOSU(MNUMNR,IY)+UCAPOSN(MNUMNR,IY)+UCAPNGU(MNUMNR,IY)+UCAPNGN(MNUMNR,IY)+UCAPPSU(MNUMNR,IY)+UCAPPSN(MNUMNR,IY)+UCAPHYU(MNUMNR,IY)+UCAPGEU(MNUMNR,IY)+UCAPMSU(MNUMNR,IY)+UCAPWDU(MNUMNR,IY)+UCAPSTU(MNUMNR,IY)+UCAPPVU(MNUMNR,IY)+UCAPWNU(MNUMNR,IY)+UCAPWFU(MNUMNR,IY)+UCAPWLU(MNUMNR,IY)+UCAPPTU(MNUMNR,IY)+UCAPHYN(MNUMNR,IY)+UCAPGEN(MNUMNR,IY)+UCAPMSN(MNUMNR,IY)+UCAPWDN(MNUMNR,IY)+UCAPSTN(MNUMNR,IY)+UCAPPVN(MNUMNR,IY)+UCAPWNN(MNUMNR,IY)+UCAPWFN(MNUMNR,IY)+UCAPWLN(MNUMNR,IY)+UCAPPTN(MNUMNR,IY)+UCAPDBU(MNUMNR,IY)+UCAPDBN(MNUMNR,IY)+UCAPDPU(MNUMNR,IY)+UCAPDPN(MNUMNR,IY)+UCAPOSC(MNUMNR,IY)+UCAPCCC(MNUMNR,IY)+UCAPCTC(MNUMNR,IY)+UCAPHYC(MNUMNR,IY)+UCAPGEC(MNUMNR,IY)+UCAPMSC(MNUMNR,IY)+UCAPWDC(MNUMNR,IY)+UCAPSTC(MNUMNR,IY)+UCAPPVC(MNUMNR,IY)+UCAPPTC(MNUMNR,IY)+UCAPWNC(MNUMNR,IY)+UCAPWLC(MNUMNR,IY)+UCAPWFC(MNUMNR,IY)
    z[13] = (
        dfd["UCAPCCU"].loc[MNUMNR]
        + dfd["UCAPCCN"].loc[MNUMNR]
        + dfd["UCAPCTU"].loc[MNUMNR]
        + dfd["UCAPCTN"].loc[MNUMNR]
        + dfd["UCAPFCU"].loc[MNUMNR]
        + dfd["UCAPFCN"].loc[MNUMNR]
        + dfd["UCAPNUU"].loc[MNUMNR]
        + dfd["UCAPNUN"].loc[MNUMNR]
        + dfd["UCAPSMU"].loc[MNUMNR]
        + dfd["UCAPSMN"].loc[MNUMNR]
        + dfd["UCAPOSU"].loc[MNUMNR]
        + dfd["UCAPOSN"].loc[MNUMNR]
        + dfd["UCAPNGU"].loc[MNUMNR]
        + dfd["UCAPNGN"].loc[MNUMNR]
        + dfd["UCAPPSU"].loc[MNUMNR]
        + dfd["UCAPPSN"].loc[MNUMNR]
        + dfd["UCAPHYU"].loc[MNUMNR]
        + dfd["UCAPGEU"].loc[MNUMNR]
        + dfd["UCAPMSU"].loc[MNUMNR]
        + dfd["UCAPWDU"].loc[MNUMNR]
        + dfd["UCAPSTU"].loc[MNUMNR]
        + dfd["UCAPPVU"].loc[MNUMNR]
        + dfd["UCAPWNU"].loc[MNUMNR]
        + dfd["UCAPWFU"].loc[MNUMNR]
        + dfd["UCAPWLU"].loc[MNUMNR]
        + dfd["UCAPPTU"].loc[MNUMNR]
        + dfd["UCAPHYN"].loc[MNUMNR]
        + dfd["UCAPGEN"].loc[MNUMNR]
        + dfd["UCAPMSN"].loc[MNUMNR]
        + dfd["UCAPWDN"].loc[MNUMNR]
        + dfd["UCAPSTN"].loc[MNUMNR]
        + dfd["UCAPPVN"].loc[MNUMNR]
        + dfd["UCAPWNN"].loc[MNUMNR]
        + dfd["UCAPWFN"].loc[MNUMNR]
        + dfd["UCAPWLN"].loc[MNUMNR]
        + dfd["UCAPPTN"].loc[MNUMNR]
        + dfd["UCAPDBU"].loc[MNUMNR]
        + dfd["UCAPDBN"].loc[MNUMNR]
        + dfd["UCAPDPU"].loc[MNUMNR]
        + dfd["UCAPDPN"].loc[MNUMNR]
        + dfd["UCAPOSC"].loc[MNUMNR]
        + dfd["UCAPCCC"].loc[MNUMNR]
        + dfd["UCAPCTC"].loc[MNUMNR]
        + dfd["UCAPHYC"].loc[MNUMNR]
        + dfd["UCAPGEC"].loc[MNUMNR]
        + dfd["UCAPMSC"].loc[MNUMNR]
        + dfd["UCAPWDC"].loc[MNUMNR]
        + dfd["UCAPSTC"].loc[MNUMNR]
        + dfd["UCAPPVC"].loc[MNUMNR]
        + dfd["UCAPPTC"].loc[MNUMNR]
        + dfd["UCAPWNC"].loc[MNUMNR]
        + dfd["UCAPWLC"].loc[MNUMNR]
        + dfd["UCAPWFC"].loc[MNUMNR]
    )

    #       Total
    # T135(14,IY,IS)=UCAPCSU(MNUMNR,IY)+UCAPCSN(MNUMNR,IY)+UCAPCCU(MNUMNR,IY)+UCAPCCN(MNUMNR,IY)+UCAPCTU(MNUMNR,IY)+UCAPCTN(MNUMNR,IY)+UCAPFCU(MNUMNR,IY)+UCAPFCN(MNUMNR,IY)+UCAPNUU(MNUMNR,IY)+UCAPNUN(MNUMNR,IY)+UCAPOSU(MNUMNR,IY)+UCAPOSN(MNUMNR,IY)+UCAPNGU(MNUMNR,IY)+UCAPNGN(MNUMNR,IY)+UCAPPSU(MNUMNR,IY)+UCAPPSN(MNUMNR,IY)+UCAPHYU(MNUMNR,IY)+UCAPGEU(MNUMNR,IY)+UCAPMSU(MNUMNR,IY)+UCAPWDU(MNUMNR,IY)+UCAPSTU(MNUMNR,IY)+UCAPPVU(MNUMNR,IY)+UCAPWNU(MNUMNR,IY)+UCAPWFU(MNUMNR,IY)+UCAPWLU(MNUMNR,IY)+UCAPPTU(MNUMNR,IY)+UCAPHYN(MNUMNR,IY)+UCAPGEN(MNUMNR,IY)+UCAPMSN(MNUMNR,IY)+UCAPWDN(MNUMNR,IY)+UCAPSTN(MNUMNR,IY)+UCAPPVN(MNUMNR,IY)+UCAPWNN(MNUMNR,IY)+UCAPWFN(MNUMNR,IY)+UCAPWLN(MNUMNR,IY)+UCAPPTN(MNUMNR,IY)+UCAPDBU(MNUMNR,IY)+UCAPDBN(MNUMNR,IY)+UCAPDPU(MNUMNR,IY)+UCAPDPN(MNUMNR,IY)+UCAPCSC(MNUMNR,IY)+UCAPOSC(MNUMNR,IY)+UCAPCCC(MNUMNR,IY)+UCAPCTC(MNUMNR,IY)+UCAPHYC(MNUMNR,IY)+UCAPGEC(MNUMNR,IY)+UCAPMSC(MNUMNR,IY)+UCAPWDC(MNUMNR,IY)+UCAPSTC(MNUMNR,IY)+UCAPPVC(MNUMNR,IY)+UCAPPTC(MNUMNR,IY)+UCAPWNC(MNUMNR,IY)+UCAPWLC(MNUMNR,IY)+UCAPWFC(MNUMNR,IY)
    z[14] = z[12] + z[13]

    return z
