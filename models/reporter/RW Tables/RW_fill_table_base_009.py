# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM

SZO fixed on 7/9/2024
Updated on 8/1/2024
Fixed "Total Capacity Additions by Approximate Fuel" on 8/22/2024

"""

import pandas as pd


def get_cum_cap(df, year):
    """Get the cummulative capacity addtions beyond a reference year

    Parameters
    ----------
    df : pandas.DataFrame
        DataFrame to calculate cummulative capacity additions

    Returns
    -------
    pandas.DataFrame
        Cummulative capacity additions
    """
    # Calculate annual capacity additions (current - previous)
    df1 = df.fillna(0).diff(axis=1).fillna(0)

    # Fillin 0 when negative
    df1[df1 < 0] = 0

    # Clean year before CUMCAPADD (year_cum_cap_addition_begin, e.g., 2023)
    df1.loc[:, :year] = 0

    # Calculate cumulative attitions
    df2 = df1.cumsum(axis=1)
    df2[year + 1 : df2.columns[-1]] -= df2

    out = df2.sum(axis=0).to_frame().T / 1000
    return out


def set_add_ret(dfd, year):
    """Subtract base year (CUMCAPAD) from all years
    Values are cumulative by default, need to make adjust base year
    """
    keys = list(filter(lambda x: x.startswith(("UADD", "URET")), dfd.keys()))
    for key in keys:
        df = dfd[key]
        df1 = df.sub(df[year], axis=0)
        df1[df1 < 0] = 0
        dfd[key] = df1


def fill_table_base_009(dfd, table_spec, table_id):
    """Fill table   Electricity Generating Capacity

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

    MNUMNR = dfd["MNUMNR_rwpre"]
    MNUMCR = dfd["MNUMCR_rwpre"]

    CUMCAPADD = int(table_spec["settings"]["year_cum_cap_addition_begin"])

    # Reset Additions and Retirements to be based in CUMPCAPADD year
    set_add_ret(dfd, CUMCAPADD)

    #   Electricity Generating Capacity
    #   (gigawatts)
    #    Net Summer Capacity 1/

    #   Electric Power Sector 2/
    #     Power Only 3/

    #       Coal 4/
    # T9(1,IY,IS)=UCAPCSU(MNUMNR,IY)+UCAPCSN(MNUMNR,IY)
    z[1] = dfd["UCAPCSU"].loc[MNUMNR] + dfd["UCAPCSN"].loc[MNUMNR]

    #       Oil and Natural Gas Steam 4, 5/
    # T9(2,IY,IS)=UCAPOSU(MNUMNR,IY)+UCAPOSN(MNUMNR,IY)+UCAPNGU(MNUMNR,IY)+UCAPNGN(MNUMNR,IY)
    z[2] = (
        dfd["UCAPOSU"].loc[MNUMNR]
        + dfd["UCAPOSN"].loc[MNUMNR]
        + dfd["UCAPNGU"].loc[MNUMNR]
        + dfd["UCAPNGN"].loc[MNUMNR]
    )

    #       Combined Cycle
    # T9(3,IY,IS)=UCAPCCU(MNUMNR,IY)+UCAPCCN(MNUMNR,IY)
    z[3] = dfd["UCAPCCU"].loc[MNUMNR] + dfd["UCAPCCN"].loc[MNUMNR]

    #       Combustion Turbine/Diesel
    # T9(4,IY,IS)=UCAPCTU(MNUMNR,IY)+UCAPCTN(MNUMNR,IY)
    z[4] = dfd["UCAPCTU"].loc[MNUMNR] + dfd["UCAPCTN"].loc[MNUMNR]

    #       Nuclear Power 6/
    # T9(5,IY,IS)=UCAPNUU(MNUMNR,IY)+UCAPNUN(MNUMNR,IY)+UCAPSMU(MNUMNR,IY)+UCAPSMN(MNUMNR,IY)
    z[5] = (
        dfd["UCAPNUU"].loc[MNUMNR]
        + dfd["UCAPNUN"].loc[MNUMNR]
        + dfd["UCAPSMU"].loc[MNUMNR]
        + dfd["UCAPSMN"].loc[MNUMNR]
    )

    #       Pumped Storage
    # T9(6,IY,IS)=UCAPPSU(MNUMNR,IY)+UCAPPSN(MNUMNR,IY)
    z[6] = dfd["UCAPPSU"].loc[MNUMNR] + dfd["UCAPPSN"].loc[MNUMNR]

    #       Diurnal Storage
    # T9(72,IY,IS)=UCAPDSU(MNUMNR,IY)+UCAPDSN(MNUMNR,IY)
    z[72] = dfd["UCAPDSU"].loc[MNUMNR] + dfd["UCAPDSN"].loc[MNUMNR]

    #       Hydrogen Turbine
    # T9(76,IY,IS)=UCAPICU(MNUMNR,IY)+UCAPICN(MNUMNR,IY)
    z[76] = dfd["UCAPICU"].loc[MNUMNR] + dfd["UCAPICN"].loc[MNUMNR]

    #       Fuel Cells
    # T9(7,IY,IS)=UCAPFCU(MNUMNR,IY)+UCAPFCN(MNUMNR,IY)
    z[7] = dfd["UCAPFCU"].loc[MNUMNR] + dfd["UCAPFCN"].loc[MNUMNR]

    #       Renewable Sources 7/
    # T9(8,IY,IS)=UCAPHYU(MNUMNR,IY)+UCAPGEU(MNUMNR,IY)+UCAPMSU(MNUMNR,IY)+UCAPWDU(MNUMNR,IY)+UCAPSTU(MNUMNR,IY)+UCAPPVU(MNUMNR,IY)+UCAPWNU(MNUMNR,IY)+UCAPWFU(MNUMNR,IY)+UCAPPTU(MNUMNR,IY)+UCAPWLU(MNUMNR,IY)+UCAPHYN(MNUMNR,IY)+UCAPGEN(MNUMNR,IY)+UCAPMSN(MNUMNR,IY)+UCAPWDN(MNUMNR,IY)+UCAPSTN(MNUMNR,IY)+UCAPPVN(MNUMNR,IY)+UCAPWNN(MNUMNR,IY)+UCAPWFN(MNUMNR,IY)+UCAPPTN(MNUMNR,IY)+UCAPWLN(MNUMNR,IY)
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
        + dfd["UCAPBIU"].loc[MNUMNR]
        + dfd["UCAPBIN"].loc[MNUMNR]
    )

    #       Distributed Generation (Natural Gas) 8/
    # T9(48,IY,IS)=UCAPDBU(MNUMNR,IY)+UCAPDBN(MNUMNR,IY)+UCAPDPU(MNUMNR,IY)+UCAPDPN(MNUMNR,IY)
    z[48] = (
        dfd["UCAPDBU"].loc[MNUMNR]
        + dfd["UCAPDBN"].loc[MNUMNR]
        + dfd["UCAPDPU"].loc[MNUMNR]
        + dfd["UCAPDPN"].loc[MNUMNR]
    )

    #         Total
    # T9(9,IY,IS)=FSUM(T9(1,IY,IS),8)+T9(48,IY,IS)+T9(72,IY,IS)
    z[9] = (
        z[1]
        + z[2]
        + z[3]
        + z[4]
        + z[5]
        + z[6]
        + z[7]
        + z[8]
        + z[48]
        + z[72]
        + z[76]
    )

    #     Combined Heat and Power 9/

    #       Coal
    # T9(51,IY,IS)=UCAPCSC(MNUMNR,IY)
    z[51] = dfd["UCAPCSC"].loc[MNUMNR]

    #       Oil and Natural Gas Steam 5/
    # T9(52,IY,IS)=UCAPOSC(MNUMNR,IY)
    z[52] = dfd["UCAPOSC"].loc[MNUMNR]

    #       Combined Cycle
    # T9(53,IY,IS)=UCAPCCC(MNUMNR,IY)
    z[53] = dfd["UCAPCCC"].loc[MNUMNR]

    #       Combustion Turbine/Diesel
    # T9(54,IY,IS)=UCAPCTC(MNUMNR,IY)
    z[54] = dfd["UCAPCTC"].loc[MNUMNR]

    #       Renewable Sources 7/
    # T9(55,IY,IS)=UCAPHYC(MNUMNR,IY)+UCAPGEC(MNUMNR,IY)+UCAPMSC(MNUMNR,IY)+UCAPWDC(MNUMNR,IY)+UCAPSTC(MNUMNR,IY)+UCAPPVC(MNUMNR,IY)+UCAPWNC(MNUMNR,IY)+UCAPWFC(MNUMNR,IY)+UCAPWLC(MNUMNR,IY)+UCAPPTC(MNUMNR,IY)
    z[55] = (
        dfd["UCAPHYC"].loc[MNUMNR]
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

    #         Total
    # T9(56,IY,IS)=FSUM(T9(51,IY,IS),5)
    z[56] = z[51] + z[52] + z[53] + z[54] + z[55]

    #     Cumulative Planned Additions 10/

    #       Coal
    # T9(10,IY,IS)=UADDCSU(1,MNUMNR,IY)+UADDCSN(1,MNUMNR,IY)+UADDCSC(MNUMNR,IY)
    z[10] = (
        dfd["UADDCSU"].loc[1].loc[MNUMNR]
        + dfd["UADDCSN"].loc[1].loc[MNUMNR]
        + dfd["UADDCSC"].loc[MNUMNR]
    )
    z[10].loc[:CUMCAPADD] = 0

    #       Oil and Natural Gas Steam 5/
    # T9(11,IY,IS)=UADDOSU(1,MNUMNR,IY)+UADDOSN(1,MNUMNR,IY)+UADDOSC(MNUMNR,IY)
    z[11] = (
        dfd["UADDOSU"].loc[1].loc[MNUMNR]
        + dfd["UADDOSN"].loc[1].loc[MNUMNR]
        + dfd["UADDOSC"].loc[MNUMNR]
    )
    z[11].loc[:CUMCAPADD] = 0

    #       Combined Cycle
    # T9(12,IY,IS)=UADDCCU(1,MNUMNR,IY)+UADDCCN(1,MNUMNR,IY)+UADDCCC(MNUMNR,IY)
    z[12] = (
        dfd["UADDCCU"].loc[1].loc[MNUMNR]
        + dfd["UADDCCN"].loc[1].loc[MNUMNR]
        + dfd["UADDCCC"].loc[MNUMNR]
    )
    z[12].loc[:CUMCAPADD] = 0

    #       Combustion Turbine/Diesel
    # T9(13,IY,IS)=UADDCTU(1,MNUMNR,IY)+UADDCTN(1,MNUMNR,IY)+UADDCTC(MNUMNR,IY)
    z[13] = (
        dfd["UADDCTU"].loc[1].loc[MNUMNR]
        + dfd["UADDCTN"].loc[1].loc[MNUMNR]
        + dfd["UADDCTC"].loc[MNUMNR]
    )
    z[13].loc[:CUMCAPADD] = 0

    #       Nuclear Power
    # T9(14,IY,IS)=UADDNUU(1,MNUMNR,IY)+UADDNUN(1,MNUMNR,IY)+UADDSMU(1,MNUMNR,IY)+UADDSMN(1,MNUMNR,IY)
    z[14] = (
        dfd["UADDNUU"].loc[1].loc[MNUMNR]
        + dfd["UADDNUN"].loc[1].loc[MNUMNR]
        + dfd["UADDSMU"].loc[1].loc[MNUMNR]
        + dfd["UADDSMN"].loc[1].loc[MNUMNR]
    )
    z[14].loc[:CUMCAPADD] = 0

    #       Pumped Storage
    # T9(15,IY,IS)=UADDPSU(1,MNUMNR,IY)+UADDPSN(1,MNUMNR,IY)
    z[15] = dfd["UADDPSU"].loc[1].loc[MNUMNR] + dfd["UADDPSN"].loc[1].loc[MNUMNR]
    z[15].loc[:CUMCAPADD] = 0

    #       Diurnal Storage
    # T9(73,IY,IS)=UADDDSU(1,MNUMNR,IY)+UADDDSN(1,MNUMNR,IY)
    z[73] = dfd["UADDDSU"].loc[1].loc[MNUMNR] + dfd["UADDDSN"].loc[1].loc[MNUMNR]
    z[73].loc[:CUMCAPADD] = 0

    #       Fuel Cells
    # T9(16,IY,IS)=UADDFCU(1,MNUMNR,IY)+UADDFCN(1,MNUMNR,IY)
    z[16] = dfd["UADDFCU"].loc[1].loc[MNUMNR] + dfd["UADDFCN"].loc[1].loc[MNUMNR]
    z[16].loc[:CUMCAPADD] = 0

    #       Hydrogen Turbine
    # T9(78,IY,IS)=UADDICU(1,MNUMYR,IY)+UADDICN(1,MNUMYR,IY)
    # z[78] = dfd['UADDICU'].loc[1].loc[MNUMYR] + dfd['UADDICN'].loc[1].loc[MNUMYR]
    z[78] = dfd["UADDICU"].loc[1].sum() + dfd["UADDICN"].loc[1].sum()
    z[78].loc[:CUMCAPADD] = 0


    #       Renewable Sources 7/
    # T9(17,IY,IS)=UADDRNU(1,MNUMNR,IY)+UADDRNN(1,MNUMNR,IY)+UADDHYC(MNUMNR,IY)+UADDGEC(MNUMNR,IY)+UADDMSC(MNUMNR,IY)+UADDWDC(MNUMNR,IY)+UADDSTC(MNUMNR,IY)+UADDPVC(MNUMNR,IY)+UADDWNC(MNUMNR,IY)+UADDWFC(MNUMNR,IY)
    z[17] = (
        dfd["UADDRNU"].loc[1].loc[MNUMNR]
        + dfd["UADDRNN"].loc[1].loc[MNUMNR]
        + dfd["UADDHYC"].loc[MNUMNR]
        + dfd["UADDGEC"].loc[MNUMNR]
        + dfd["UADDMSC"].loc[MNUMNR]
        + dfd["UADDWDC"].loc[MNUMNR]
        + dfd["UADDSTC"].loc[MNUMNR]
        + dfd["UADDPVC"].loc[MNUMNR]
        + dfd["UADDWNC"].loc[MNUMNR]
        + dfd["UADDWFC"].loc[MNUMNR]
    )
    z[17].loc[:CUMCAPADD] = 0

    #       Distributed Generation 8/
    # T9(49,IY,IS)=UADDDBU(1,MNUMNR,IY)+UADDDBN(1,MNUMNR,IY)+UADDDPU(1,MNUMNR,IY)+UADDDPN(1,MNUMNR,IY)
    z[49] = (
        dfd["UADDDBU"].loc[1].loc[MNUMNR]
        + dfd["UADDDBN"].loc[1].loc[MNUMNR]
        + dfd["UADDDPU"].loc[1].loc[MNUMNR]
        + dfd["UADDDPN"].loc[1].loc[MNUMNR]
    )
    z[49].loc[:CUMCAPADD] = 0

    #         Total
    # T9(18,IY,IS)=UADDTLU(1,MNUMNR,IY)+UADDTLN(1,MNUMNR,IY)+UADDTLC(MNUMNR,IY)
    z[18] = (
        dfd["UADDTLU"].loc[1].loc[MNUMNR]
        + dfd["UADDTLN"].loc[1].loc[MNUMNR]
        + dfd["UADDTLC"].loc[MNUMNR]
    )
    z[18].loc[:CUMCAPADD] = 0

    #     Cumulative Unplanned Additions 10/

    #       Coal
    # T9(19,IY,IS)=UADDCSU(2,MNUMNR,IY)+UADDCSN(2,MNUMNR,IY)
    z[19] = dfd["UADDCSU"].loc[2].loc[MNUMNR] + dfd["UADDCSN"].loc[2].loc[MNUMNR]

    #       Oil and Natural Gas Steam 5/
    # T9(20,IY,IS)=UADDOSU(2,MNUMNR,IY)+UADDOSN(2,MNUMNR,IY)
    z[20] = dfd["UADDOSU"].loc[2].loc[MNUMNR] + dfd["UADDOSN"].loc[2].loc[MNUMNR]

    #       Combined Cycle
    # T9(21,IY,IS)=UADDCCU(2,MNUMNR,IY)+UADDCCN(2,MNUMNR,IY)
    z[21] = dfd["UADDCCU"].loc[2].loc[MNUMNR] + dfd["UADDCCN"].loc[2].loc[MNUMNR]

    #       Combustion Turbine/Diesel
    # T9(22,IY,IS)=UADDCTU(2,MNUMNR,IY)+UADDCTN(2,MNUMNR,IY)
    z[22] = dfd["UADDCTU"].loc[2].loc[MNUMNR] + dfd["UADDCTN"].loc[2].loc[MNUMNR]

    #       Nuclear Power
    # T9(23,IY,IS)=UADDNUU(2,MNUMNR,IY)+UADDNUN(2,MNUMNR,IY)+UADDSMU(2,MNUMNR,IY)+UADDSMN(2,MNUMNR,IY)
    z[23] = (
        dfd["UADDNUU"].loc[2].loc[MNUMNR]
        + dfd["UADDNUN"].loc[2].loc[MNUMNR]
        + dfd["UADDSMU"].loc[2].loc[MNUMNR]
        + dfd["UADDSMN"].loc[2].loc[MNUMNR]
    )

    #       Pumped Storage
    # T9(24,IY,IS)=UADDPSU(2,MNUMNR,IY)+UADDPSN(2,MNUMNR,IY)
    z[24] = dfd["UADDPSU"].loc[2].loc[MNUMNR] + dfd["UADDPSN"].loc[2].loc[MNUMNR]

    #       Diurnal Storage
    # T9(74,IY,IS)=UADDDSU(2,MNUMNR,IY)+UADDDSN(2,MNUMNR,IY)
    z[74] = dfd["UADDDSU"].loc[2].loc[MNUMNR] + dfd["UADDDSN"].loc[2].loc[MNUMNR]

    #       Hydrogen Turbine
    #  T9(80,IY,IS) = UADDICU(2,mnumnr,IY) + UADDICN(2,mnumnr,IY)
    z[80] = dfd["UADDICU"].loc[2].loc[MNUMNR] + dfd["UADDICN"].loc[2].loc[MNUMNR]

    #       Fuel Cells
    # T9(25,IY,IS)=UADDFCU(2,MNUMNR,IY)+UADDFCN(2,MNUMNR,IY)
    z[25] = dfd["UADDFCU"].loc[2].loc[MNUMNR] + dfd["UADDFCN"].loc[2].loc[MNUMNR]

    #       Renewable Sources 7/
    # T9(26,IY,IS)=UADDRNU(2,MNUMNR,IY)+UADDRNN(2,MNUMNR,IY)
    z[26] = dfd["UADDRNU"].loc[2].loc[MNUMNR] + dfd["UADDRNN"].loc[2].loc[MNUMNR]

    #       Distributed Generation 8/
    # T9(50,IY,IS)=UADDDBU(2,MNUMNR,IY)+UADDDBN(2,MNUMNR,IY)+UADDDPU(2,MNUMNR,IY)+UADDDPN(2,MNUMNR,IY)
    z[50] = (
        dfd["UADDDBU"].loc[2].loc[MNUMNR]
        + dfd["UADDDBN"].loc[2].loc[MNUMNR]
        + dfd["UADDDPU"].loc[2].loc[MNUMNR]
        + dfd["UADDDPN"].loc[2].loc[MNUMNR]
    )

    #         Total
    # T9(27,IY,IS)=UADDTLU(2,MNUMNR,IY)+UADDTLN(2,MNUMNR,IY)
    z[27] = dfd["UADDTLU"].loc[2].loc[MNUMNR] + dfd["UADDTLN"].loc[2].loc[MNUMNR]

    #     Cumulative Electric Power Sector Additions 10/
    # T9(28,IY,IS)=UADDTLU(1,MNUMNR,IY)+UADDTLU(2,MNUMNR,IY)+UADDTLC(MNUMNR,IY)+UADDTLN(1,MNUMNR,IY)+UADDTLN(2,MNUMNR,IY)
    z[28] = (
        dfd["UADDTLU"].loc[1].loc[MNUMNR]
        + dfd["UADDTLU"].loc[2].loc[MNUMNR]
        + dfd["UADDTLC"].loc[MNUMNR]
        + dfd["UADDTLN"].loc[1].loc[MNUMNR]
        + dfd["UADDTLN"].loc[2].loc[MNUMNR]
    )
    z[28].loc[:CUMCAPADD] = 0
    #

    #     Cumulative Retirements 11/

    #       Coal
    # T9(29,IY,IS)=URETCSU(MNUMNR,IY)
    z[29] = dfd["URETCSU"].loc[MNUMNR]
    z[29].loc[:CUMCAPADD] = 0

    #       Oil and Natural Gas Steam 5/
    # T9(30,IY,IS)=URETOSU(MNUMNR,IY)
    z[30] = dfd["URETOSU"].loc[MNUMNR]
    z[30].loc[:CUMCAPADD] = 0

    #       Combined Cycle
    # T9(31,IY,IS)=URETCCU(MNUMNR,IY)
    z[31] = dfd["URETCCU"].loc[MNUMNR]
    z[31].loc[:CUMCAPADD] = 0

    #       Combustion Turbine/Diesel
    # T9(32,IY,IS)=URETCTU(MNUMNR,IY)
    z[32] = dfd["URETCTU"].loc[MNUMNR]
    z[32].loc[:CUMCAPADD] = 0

    #       Nuclear Power
    # T9(33,IY,IS)=URETNUU(MNUMNR,IY)+URETSMU(MNUMNR,IY)
    z[33] = dfd["URETNUU"].loc[MNUMNR] + dfd["URETSMU"].loc[MNUMNR]
    z[33].loc[:CUMCAPADD] = 0

    #       Pumped Storage
    # T9(34,IY,IS)=URETPSU(MNUMNR,IY)
    z[34] = dfd["URETPSU"].loc[MNUMNR]
    z[34].loc[:CUMCAPADD] = 0

    #       Diurnal Storage
    # T9(75,IY,IS)=URETDSU(MNUMNR,IY)
    z[75] = dfd["URETDSU"].loc[MNUMNR]
    z[75].loc[:CUMCAPADD] = 0

    #       Hydrogen Turbine
    # T9(82,IY,IS)=URETICU(MNUMYR,IY)
    z[82] = dfd["URETICU"].loc[MNUMNR]
    z[82].loc[:CUMCAPADD] = 0

    #       Fuel Cells
    # T9(35,IY,IS)=URETFCU(MNUMNR,IY)
    z[35] = dfd["URETFCU"].loc[MNUMNR]
    z[35].loc[:CUMCAPADD] = 0

    #       Renewable Sources 7/
    # T9(36,IY,IS)=URETRNU(MNUMNR,IY)
    z[36] = dfd["URETRNU"].loc[MNUMNR]
    z[36].loc[:CUMCAPADD] = 0

    #         Total
    # T9(37,IY,IS)=URETTLU(MNUMNR,IY)
    z[37] = dfd["URETTLU"].loc[MNUMNR]
    z[37].loc[:CUMCAPADD] = 0

    #

    #   Total Electric Power Sector Capacity
    # T9(57,IY,IS)=T9(9,IY,IS)+T9(56,IY,IS)
    z[57] = z[9] + z[56]

    #   End-Use Generators 12/

    #       Coal
    # T9(38,IY,IS)=(CGREFCAP(11,IY,1)+CGOGSCAP(11,IY,1)+CGINDLCAP(11,IY,1)+CGCOMMCAP(11,IY,1))*.001
    z[38] = (
        dfd["CGREFCAP"].loc[MNUMCR].loc[1]
        + dfd["CGOGSCAP"].loc[MNUMCR].loc[1]
        + dfd["CGINDLCAP"].loc[MNUMCR].loc[1]
        + dfd["CGCOMMCAP"].loc[MNUMCR].loc[1]
    ) * 0.001

    #       Petroleum
    # T9(39,IY,IS)=(CGREFCAP(11,IY,2)+CGOGSCAP(11,IY,2)+CGINDLCAP(11,IY,2)+CGCOMMCAP(11,IY,2))*.001
    z[39] = (
        dfd["CGREFCAP"].loc[MNUMCR].loc[2]
        + dfd["CGOGSCAP"].loc[MNUMCR].loc[2]
        + dfd["CGINDLCAP"].loc[MNUMCR].loc[2]
        + dfd["CGCOMMCAP"].loc[MNUMCR].loc[2]
    ) * 0.001

    #       Natural Gas
    # T9(40,IY,IS)=(CGREFCAP(11,IY,3)+CGOGSCAP(11,IY,3)+CGINDLCAP(11,IY,3)+CGCOMMCAP(11,IY,3)+CGRESCAP(11,IY,3))*.001
    z[40] = (
        dfd["CGREFCAP"].loc[MNUMCR].loc[3]
        + dfd["CGOGSCAP"].loc[MNUMCR].loc[3]
        + dfd["CGINDLCAP"].loc[MNUMCR].loc[3]
        + dfd["CGCOMMCAP"].loc[MNUMCR].loc[3]
        + dfd["CGRESCAP"].loc[MNUMCR].loc[3]
    ) * 0.001

    #       Other Gaseous Fuels 13/
    # T9(41,IY,IS)=(CGINDLCAP(11,IY,9)+CGREFCAP(11,IY,9)+CGCOMMCAP(11,IY,9))*.001
    z[41] = (
        dfd["CGINDLCAP"].loc[MNUMCR].loc[9]
        + dfd["CGREFCAP"].loc[MNUMCR].loc[9]
        + dfd["CGCOMMCAP"].loc[MNUMCR].loc[9]
    ) * 0.001

    #       Renewable Sources 7/
    # T9(42,IY,IS)=(CGREFCAP(11,IY,6)+CGREFCAP(11,IY,7)+CGINDLCAP(11,IY,6)+CGINDLCAP(11,IY,7)+CGCOMMCAP(11,IY,6)+CGCOMMCAP(11,IY,7))*.001

    z[42] = (
        dfd["CGREFCAP"].loc[MNUMCR].loc[6]
        + dfd["CGREFCAP"].loc[MNUMCR].loc[7]
        + dfd["CGINDLCAP"].loc[MNUMCR].loc[6]
        + dfd["CGINDLCAP"].loc[MNUMCR].loc[7]
        + dfd["CGCOMMCAP"].loc[MNUMCR].loc[6]
        + dfd["CGCOMMCAP"].loc[MNUMCR].loc[7]
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
    ) * 0.001

    #       Other 14/
    # T9(43,IY,IS)=(CGOGSCAP(11,IY,4)+CGINDLCAP(11,IY,10)+CGCOMMCAP(11,IY,10))*.001
    z[43] = (
        dfd["CGOGSCAP"].loc[MNUMCR].loc[4]
        + dfd["CGINDLCAP"].loc[MNUMCR].loc[10]
        + dfd["CGCOMMCAP"].loc[MNUMCR].loc[10]
    ) * 0.001

    #         Total
    # T9(44,IY,IS)=T9(44,IY,IS)+T9(46,IY,IS)
    z[44] = z[38] + z[39] + z[40] + z[41] + z[42] + z[43]

    #   --------------------------------------------

    #     Bonus Rows - Internal Only

    #   --------------------------------------------

    #   Total Capacity by Approximate Fuel

    #     Coal
    # T9(58,IY,IS)=T9(1,IY,IS)+T9(51,IY,IS)+T9(38,IY,IS)
    z[58] = z[1] + z[51] + z[38]

    #     Petroleum
    # T9(59,IY,IS)=T9(2,IY,IS)+T9(52,IY,IS)+T9(39,IY,IS)
    z[59] = z[2] + z[52] + z[39]

    #     Natural Gas
    # T9(60,IY,IS)=T9(3,IY,IS)+T9(4,IY,IS)+T9(7,IY,IS)+T9(48,IY,IS)+T9(53,IY,IS)+T9(54,IY,IS)+T9(40,IY,IS)
    z[60] = z[3] + z[4] + z[7] + z[48] + z[53] + z[54] + z[40]

    #     Nuclear Power
    # T9(61,IY,IS)=T9(5,IY,IS)
    z[61] = z[5]

    #     Renewable Sources
    # T9(62,IY,IS)=T9(8,IY,IS)+T9(55,IY,IS)+T9(42,IY,IS)
    z[62] = z[8] + z[55] + z[42]

    #     Other including Pumped Storage
    # T9(63,IY,IS)=T9(6,IY,IS)+T9(41,IY,IS)+T9(43,IY,IS)
    z[63] = z[6] + z[41] + z[43] + z[72]

    #     Hydrogen
    # T9(84,IY,IS)=T9(76,IY,IS)+T9(77,IY,IS)
    z[84] = z[76]

    #       Total
    # T9(64,IY,IS)=FSUM(T9(58,IY,IS),6)+T9(84,IY,IS)
    z[64] = z[58] + z[59] + z[60] + z[61] + z[62] + z[63] + z[84]

    ###### see fortran code for preprocessing ######
    #
    #     Coal
    # layin: T9(65,IY,IS)=T9(65,IY,IS)+T9(10,IY,IS)+T9(19,IY,IS)
    #
    # ftab.f ---------------------------------------------------------------------------------------
    # !  TABLE 9.  ELECTRICITY GENERATING CAPABILITY (INCLUDES NONTRADITIONAL COGEN FOR AEO03)
    #
    #       RUNTOTS = 0.0
    #       DO 90 IY=1,LASTYR
    # ...
    #
    # !  capacity additions.  first we do end use sector additions, so we can go through the variables
    # !  only once.  then we sum it up for the end use capacity additions (published). then we tack on
    # !  the electric power sector additions for the bonus rows which are by (approximate) fuel.
    # !  as a reminder, tehse are the fuel indices for most (all but OGS) of the chap variables:
    # !            1=Coal            2=Petroleum      3=Natural Gas     4=Hydropower
    # !            5=Geothermal      6=MSW            7=Biomass         8=Solar Photovoltaic
    # !            9=Other Gaseous  10=Other         11=Wind           12=Solar Thermal
    # !  RUNTOTS fuels are:  1=coal, 2=oil, 3=natural gas, 4=nuclear, 5=renewables, 6=other
    #          T9(65:71,IY,IS) = 0.0
    #          T9(45,IY,IS) = 0.0
    #          IF (IY .GT. (CUMCAPADD - 1989)) THEN
    #          RUNTOTS(:,IY) = RUNTOTS(:,IY-1)
    #          DO IR=1,MNUMCR-2
    # ! coal
    #            IF (CGREFCAP(IR,IY,1) .GT. CGREFCAP(IR,IY-1,1)) &
    #               RUNTOTS(1,IY) = RUNTOTS(1,IY) + CGREFCAP(IR,IY,1) - CGREFCAP(IR,IY-1,1)
    #            IF (CGOGSCAP(IR,IY,1) .GT. CGOGSCAP(IR,IY-1,1)) &
    #               RUNTOTS(1,IY) = RUNTOTS(1,IY) + CGOGSCAP(IR,IY,1) - CGOGSCAP(IR,IY-1,1)
    #            IF (CGINDLCAP(IR,IY,1) .GT. CGINDLCAP(IR,IY-1,1)) &
    #               RUNTOTS(1,IY) = RUNTOTS(1,IY) + CGINDLCAP(IR,IY,1) - CGINDLCAP(IR,IY-1,1)
    #            IF (CGCOMMCAP(IR,IY,1) .GT. CGCOMMCAP(IR,IY-1,1)) &
    #               RUNTOTS(1,IY) = RUNTOTS(1,IY) + CGCOMMCAP(IR,IY,1) - CGCOMMCAP(IR,IY-1,1)
    # ...
    #
    #           T9(65,IY,IS) = T9(65,IY,IS) + RUNTOTS(1,IY) / 1000.
    #           T9(65,IY,IS) = T9(65,IY,IS) + T9(10,IY,IS) + T9(19,IY,IS)
    #
    # Aggregate End-Use Coal capacity by sector and technology
    df65 = pd.concat(
        [
            dfd["CGREFCAP"].loc[
                (range(1, MNUMCR - 1), 1), :
            ],  # REFINE Combined heat and power fuel capacity
            dfd["CGOGSCAP"].loc[
                (range(1, MNUMCR - 1), 1), :
            ],  # OILGAS Combined heat and power fuel capacity
            dfd["CGINDLCAP"].loc[
                (range(1, MNUMCR - 1), 1), :
            ],  # INDUST Combined heat and power fuel capacity
            dfd["CGCOMMCAP"].loc[
                (range(1, MNUMCR - 1), 1), :
            ],  # COMMER Combined heat and power fuel capacity
        ],
        ignore_index=True,
    )

    # Calculate the total
    z[65] = get_cum_cap(df65, CUMCAPADD)

    # Update End-Use Capacity Additions 10/
    z[45] = z[65]

    # Add Electric Power Sector 2: Cumulative Planned Additions 10, Cumulative Unplanned Additions 10
    z[65] = z[65] + z[10] + z[19]

    #     Petroleum
    # layin: T9(66,IY,IS)=T9(66,IY,IS)+T9(11,IY,IS)+T9(20,IY,IS)
    # ! oil
    #                IF (CGREFCAP(IR,IY,2) .GT. CGREFCAP(IR,IY-1,2)) &
    #                   RUNTOTS(2,IY) = RUNTOTS(2,IY) + CGREFCAP(IR,IY,2) - CGREFCAP(IR,IY-1,2)
    #                IF (CGOGSCAP(IR,IY,2) .GT. CGOGSCAP(IR,IY-1,2)) &
    #                   RUNTOTS(2,IY) = RUNTOTS(2,IY) + CGOGSCAP(IR,IY,2) - CGOGSCAP(IR,IY-1,2)
    #                IF (CGINDLCAP(IR,IY,2) .GT. CGINDLCAP(IR,IY-1,2)) &
    #                   RUNTOTS(2,IY) = RUNTOTS(2,IY) + CGINDLCAP(IR,IY,2) - CGINDLCAP(IR,IY-1,2)
    #                IF (CGCOMMCAP(IR,IY,2) .GT. CGCOMMCAP(IR,IY,2)) &
    #                   RUNTOTS(2,IY) = RUNTOTS(2,IY) + CGCOMMCAP(IR,IY,2) - CGCOMMCAP(IR,IY,2)
    #   ...
    #               9(66,IY,IS) = T9(66,IY,IS) + RUNTOTS(2,IY) / 1000.
    #               T9(66,IY,IS) = T9(66,IY,IS) + T9(11,IY,IS) + T9(20,IY,IS)
    #
    # Aggregate End-Use Petroleum capacity by sector and technology
    df66 = pd.concat(
        [
            dfd["CGREFCAP"].loc[
                (range(1, MNUMCR - 1), 2), :
            ],  # REFINE Combined heat and power fuel capacity
            dfd["CGOGSCAP"].loc[
                (range(1, MNUMCR - 1), 2), :
            ],  # OILGAS Combined heat and power fuel capacity
            dfd["CGINDLCAP"].loc[
                (range(1, MNUMCR - 1), 2), :
            ],  # INDUST Combined heat and power fuel capacity
            dfd["CGCOMMCAP"].loc[
                (range(1, MNUMCR - 1), 2), :
            ],  # COMMER Combined heat and power fuel capacity
        ],
        ignore_index=True,
    )
    z[66] = get_cum_cap(df66, CUMCAPADD)
    #
    # # Update End-Use Cumulative Capacity Additions 10/
    z[45] += z[66]
    #
    # Add Electric Power Sector 2: Cumulative Planned Additions 10, Cumulative Unplanned Additions 10
    z[66] = z[66] + z[11] + z[20]

    #     Natural Gas
    # layin: T9(67,IY,IS)=T9(67,IY,IS)+T9(12,IY,IS)+T9(13,IY,IS)+T9(16,IY,IS)+T9(49,IY,IS)+T9(21,IY,IS)+T9(22,IY,IS)+T9(25,IY,IS)+T9(50,IY,IS)
    # ftab.f
    # ! natural gas
    #                IF (CGREFCAP(IR,IY,3) .GT. CGREFCAP(IR,IY-1,3)) &
    #                   RUNTOTS(3,IY) = RUNTOTS(3,IY) + CGREFCAP(IR,IY,3) - CGREFCAP(IR,IY-1,3)
    #                IF (CGOGSCAP(IR,IY,3) .GT. CGOGSCAP(IR,IY-1,3)) &
    #                   RUNTOTS(3,IY) = RUNTOTS(3,IY) + CGOGSCAP(IR,IY,3) - CGOGSCAP(IR,IY-1,3)
    #                IF (CGINDLCAP(IR,IY,3) .GT. CGINDLCAP(IR,IY-1,3)) &
    #                   RUNTOTS(3,IY) = RUNTOTS(3,IY) + CGINDLCAP(IR,IY,3) - CGINDLCAP(IR,IY-1,3)
    #                IF (CGCOMMCAP(IR,IY,3) .GT. CGCOMMCAP(IR,IY-1,3)) &
    #                   RUNTOTS(3,IY) = RUNTOTS(3,IY) + CGCOMMCAP(IR,IY,3) - CGCOMMCAP(IR,IY-1,3)
    #                IF (CGRESCAP(IR,IY,3) .GT. CGRESCAP(IR,IY-1,3)) &
    #                   RUNTOTS(3,IY) = RUNTOTS(3,IY) + CGRESCAP(IR,IY,3) - CGRESCAP(IR,IY-1,3)
    # ...
    #               T9(67,IY,IS) = T9(67,IY,IS) + RUNTOTS(3,IY) / 1000.
    #               T9(67,IY,IS) = T9(67,IY,IS) + T9(12,IY,IS) + T9(13,IY,IS) + T9(16,IY,IS) + T9(49,IY,IS) + &
    #                        T9(21,IY,IS) + T9(22,IY,IS) + T9(25,IY,IS) + T9(50,IY,IS)
    #
    # Aggregate End-Use Natural Gas capacity by sector and technology
    df67 = pd.concat(
        [
            dfd["CGREFCAP"].loc[
                (range(1, MNUMCR - 1), 3), :
            ],  # REFINE Combined heat and power fuel capacity
            dfd["CGOGSCAP"].loc[
                (range(1, MNUMCR - 1), 3), :
            ],  # OILGAS Combined heat and power fuel capacity
            dfd["CGINDLCAP"].loc[
                (range(1, MNUMCR - 1), 3), :
            ],  # INDUST Combined heat and power fuel capacity
            dfd["CGCOMMCAP"].loc[
                (range(1, MNUMCR - 1), 3), :
            ],  # COMMER Combined heat and power fuel capacity
            dfd["CGRESCAP"].loc[(range(1, MNUMCR - 1), 3), :],  # NONUTIL CAPACITY
        ],
        ignore_index=True,
    )
    z[67] = get_cum_cap(df67, CUMCAPADD)
    #
    # Update End-Use Cumulative Capacity Additions 10/
    z[45] += z[67]
    #
    # Add Electric Power Sector 2 / Cumulative Planned Additions 10/
    z[67] = z[67] + z[12] + z[13] + z[16] + z[49] + z[21] + z[22] + z[25] + z[50]
    #
    # Clean before year CUMCAPADD (2023)
    z[67].loc[:, :CUMCAPADD] = 0

    #     Nuclear Power
    # Update End-Use Cumulative Capacity Additions 10/
    # No End_use Nuclear Power
    # z[45] += z[68]
    # T9(68,IY,IS)=T9(68,IY,IS)+T9(14,IY,IS)+T9(23,IY,IS)
    z[68] = z[14] + z[23]

    #     Renewable Sources
    # layin: T9(69,IY,IS)=T9(69,IY,IS)+T9(17,IY,IS)+T9(26,IY,IS)
    # ! renewables
    #                IF (CGREFCAP(IR,IY,6) .GT. CGREFCAP(IR,IY-1,6)) &
    #                   RUNTOTS(5,IY) = RUNTOTS(5,IY) + CGREFCAP(IR,IY,6) - CGREFCAP(IR,IY-1,6)
    #                IF (CGREFCAP(IR,IY,7) .GT. CGREFCAP(IR,IY-1,7)) &
    #                   RUNTOTS(5,IY) = RUNTOTS(5,IY) + CGREFCAP(IR,IY,7) - CGREFCAP(IR,IY-1,7)
    #                IF (CGINDLCAP(IR,IY,6) .GT. CGINDLCAP(IR,IY-1,6)) &
    #                   RUNTOTS(5,IY) = RUNTOTS(5,IY) + CGINDLCAP(IR,IY,6) - CGINDLCAP(IR,IY-1,6)
    #                IF (CGINDLCAP(IR,IY,7) .GT. CGINDLCAP(IR,IY-1,7)) &
    #                   RUNTOTS(5,IY) = RUNTOTS(5,IY) + CGINDLCAP(IR,IY,7) - CGINDLCAP(IR,IY-1,7)
    #                IF (CGCOMMCAP(IR,IY,6) .GT. CGCOMMCAP(IR,IY-1,6)) &
    #                   RUNTOTS(5,IY) = RUNTOTS(5,IY) + CGCOMMCAP(IR,IY,6) - CGCOMMCAP(IR,IY-1,6)
    #                IF (CGCOMMCAP(IR,IY,7) .GT. CGCOMMCAP(IR,IY-1,7)) &
    #                   RUNTOTS(5,IY) = RUNTOTS(5,IY) + CGCOMMCAP(IR,IY,7) - CGCOMMCAP(IR,IY-1,7)
    #                IF (CGRESCAP(IR,IY,8) .GT. CGRESCAP(IR,IY-1,8)) &
    #                   RUNTOTS(5,IY) = RUNTOTS(5,IY) + CGRESCAP(IR,IY,8) - CGRESCAP(IR,IY-1,8)
    #                IF (CGRESCAP(IR,IY,11) .GT. CGRESCAP(IR,IY-1,11)) &
    #                   RUNTOTS(5,IY) = RUNTOTS(5,IY) + CGRESCAP(IR,IY,11) - CGRESCAP(IR,IY-1,11)
    #                IF (CGCOMMCAP(IR,IY,8) .GT. CGCOMMCAP(IR,IY-1,8)) &
    #                   RUNTOTS(5,IY) = RUNTOTS(5,IY) + CGCOMMCAP(IR,IY,8) - CGCOMMCAP(IR,IY-1,8)
    #                IF (CGINDLCAP(IR,IY,8) .GT. CGINDLCAP(IR,IY-1,8)) &
    #                   RUNTOTS(5,IY) = RUNTOTS(5,IY) + CGINDLCAP(IR,IY,8) - CGINDLCAP(IR,IY-1,8)
    #                IF (CGINDLCAP(IR,IY,4) .GT. CGINDLCAP(IR,IY-1,4)) &
    #                   RUNTOTS(5,IY) = RUNTOTS(5,IY) + CGINDLCAP(IR,IY,4) - CGINDLCAP(IR,IY-1,4)
    #                IF (CGCOMMCAP(IR,IY,4) .GT. CGCOMMCAP(IR,IY-1,4)) &
    #                   RUNTOTS(5,IY) = RUNTOTS(5,IY) + CGCOMMCAP(IR,IY,4) - CGCOMMCAP(IR,IY-1,4)
    #                IF (CGCOMMCAP(IR,IY,5) .GT. CGCOMMCAP(IR,IY-1,5)) &
    #                   RUNTOTS(5,IY) = RUNTOTS(5,IY) + CGCOMMCAP(IR,IY,5) - CGCOMMCAP(IR,IY-1,5)
    #                IF (CGCOMMCAP(IR,IY,11) .GT. CGCOMMCAP(IR,IY-1,11)) &
    #                   RUNTOTS(5,IY) = RUNTOTS(5,IY) + CGCOMMCAP(IR,IY,11) - CGCOMMCAP(IR,IY-1,11)
    #                IF (CGINDLCAP(IR,IY,5) .GT. CGINDLCAP(IR,IY-1,5)) &
    #                   RUNTOTS(5,IY) = RUNTOTS(5,IY) + CGINDLCAP(IR,IY,5) - CGINDLCAP(IR,IY-1,5)
    #                IF (CGINDLCAP(IR,IY,11) .GT. CGINDLCAP(IR,IY-1,11)) &
    #                   RUNTOTS(5,IY) = RUNTOTS(5,IY) + CGINDLCAP(IR,IY,11) - CGINDLCAP(IR,IY-1,11)
    # ...
    #               T9(69,IY,IS) = T9(69,IY,IS) + RUNTOTS(5,IY) / 1000.
    #               T9(69,IY,IS) = T9(69,IY,IS) + T9(17,IY,IS) + T9(26,IY,IS)
    #
    # Aggregate End-Use Renewable capacity by sector and technology
    df69 = pd.concat(
        [
            dfd["CGREFCAP"].loc[
                (range(1, MNUMCR - 1), (6, 7)), :
            ],  # REFINE Combined heat and power fuel capacity
            dfd["CGINDLCAP"].loc[
                (range(1, MNUMCR - 1), (6, 7, 8, 4, 5, 11)), :
            ],  # INDUST Combined heat and power fuel capacity
            dfd["CGCOMMCAP"].loc[
                (range(1, MNUMCR - 1), (6, 7, 8, 4, 5, 11)), :
            ],  # COMMER Combined heat and power fuel capacity
            dfd["CGRESCAP"].loc[(range(1, MNUMCR - 1), (8, 11)), :],
        ],  # NONUTIL CAPACITY
        ignore_index=True,
    )

    z[69] = get_cum_cap(df69, CUMCAPADD)
    #
    # Update End-Use Cumulative Capacity Additions 10/
    z[45] += z[69]
    #
    # Add Electric Power Sector 2 / Cumulative Planned Additions 10/
    z[69] = z[69] + z[17] + z[26]

    #     Other including Pumped Storage
    # layin: T9(70,IY,IS)=T9(70,IY,IS)+T9(15,IY,IS)+T9(24,IY,IS)+T9(73,IY,IS)+T9(74,IY,IS)
    # ftab.f
    # ! other
    #            IF (CGINDLCAP(IR,IY,9) .GT. CGINDLCAP(IR,IY-1,9)) &
    #               RUNTOTS(6,IY) = RUNTOTS(6,IY) + CGINDLCAP(IR,IY,9) - CGINDLCAP(IR,IY-1,9)
    #            IF (CGREFCAP(IR,IY,9) .GT. CGREFCAP(IR,IY-1,9)) &
    #               RUNTOTS(6,IY) = RUNTOTS(6,IY) + CGREFCAP(IR,IY,9) - CGREFCAP(IR,IY-1,9)
    #            IF (CGCOMMCAP(IR,IY,9) .GT. CGCOMMCAP(IR,IY-1,9)) &
    #               RUNTOTS(6,IY) = RUNTOTS(6,IY) + CGCOMMCAP(IR,IY,9) - CGCOMMCAP(IR,IY-1,9)
    #            IF (CGOGSCAP(IR,IY,4) .GT. CGOGSCAP(IR,IY-1,4)) &
    #               RUNTOTS(6,IY) = RUNTOTS(6,IY) + CGOGSCAP(IR,IY,4) - CGOGSCAP(IR,IY-1,4)
    #            IF (CGINDLCAP(IR,IY,10) .GT. CGINDLCAP(IR,IY-1,10)) &
    #               RUNTOTS(6,IY) = RUNTOTS(6,IY) + CGINDLCAP(IR,IY,10) - CGINDLCAP(IR,IY-1,10)
    #            IF (CGCOMMCAP(IR,IY,10) .GT. CGCOMMCAP(IR,IY-1,10)) &
    #               RUNTOTS(6,IY) = RUNTOTS(6,IY) + CGCOMMCAP(IR,IY,10) - CGCOMMCAP(IR,IY-1,10)
    #  ...
    #           T9(70,IY,IS) = T9(70,IY,IS) + RUNTOTS(6,IY) / 1000.
    #           T9(70,IY,IS) = T9(70,IY,IS) + T9(15,IY,IS) + T9(24,IY,IS) + T9(73,IY,IS) + T9(74,IY,IS)
    #
    # Aggregate End-Use Other including Pumped Storage capacity by sector and technology
    df70 = pd.concat(
        [
            dfd["CGREFCAP"].loc[
                (range(1, MNUMCR - 1), 9), :
            ],  # REFINE Combined heat and power fuel capacity
            dfd["CGOGSCAP"].loc[
                (range(1, MNUMCR - 1), 4), :
            ],  # OILGAS Combined heat and power fuel capacity
            dfd["CGINDLCAP"].loc[
                (range(1, MNUMCR - 1), (9, 10)), :
            ],  # INDUST Combined heat and power fuel capacity
            dfd["CGCOMMCAP"].loc[
                (range(1, MNUMCR - 1), (9, 10)), :
            ],  # COMMER Combined heat and power fuel capacity
        ],
        ignore_index=True,
    )

    # Calculate the total
    z[70] = get_cum_cap(df70, CUMCAPADD)

    # Update End-Use Cumulative Capacity Additions 10/
    z[45] += z[70]
    #
    # Add Electric Power Sector 2 / Cumulative Planned Additions 10/
    z[70] = z[70] + z[15] + z[24] + z[73] + z[74]

    # #     Cumulative Capacity Additions 10/
    # #T9(45,IY,IS)=FSUM(T9(65,IY,IS),6)
    #   #  Cumulative Capacity Additions 10/
    # # T9(45,IY,IS) = FSUM(T9(65,IY,IS),6)
    # z[45] = z[65] + z[66] + z[67] + z[68] + z[69] + z[70]
    # z[45].iloc[:,:CUMCAPADD] = 0
    # z[45] = z[45].cumsum(axis=1)
    z[45].T.loc[:CUMCAPADD] = 0

    #     Hydrogen
    # T9(85,IY,IS)=T9(85,IY,IS)+T9(78,IY,IS)+T9(79,IY,IS)+T9(80,IY,IS)+T9(81,IY,IS)
    # z[85] = z[85]+z[78]+z[79]+z[80]+z[81]
    z[85] = z[78] + z[80]
    z[85].loc[:CUMCAPADD] = 0

    #       Total
    # T9(71,IY,IS)=FSUM(T9(65,IY,IS),6)+T9(85,IY,IS)
    z[71] = z[65] + z[66] + z[67] + z[68] + z[69] + z[70] + z[85]

    return z
