# -*- coding: utf-8 -*-
"""
Created on Mon Dec 27 09:00:06 2023

@author: SZO
Fixed multi-index issues on 7-15-2024
Fixed accessing EUPVGENADJ and EUPVCAPADJ in postprocessor_base on 8/12/2024
Fixed The Electricity Sales, End Use Prices, and Fuel Prices for all NERC regions on 8/15/2024
"""
import numpy as np
import pandas as pd


def fill_table_base_062(dfd, table_spec, table_id):  # , d_base):
    """Fill table for Electric Power Projections by Electricity Market Module Region.

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

    FYRPRC = int(table_spec["settings"]["year_index_real_dol_priceyr"])
    # Get inflation index from 1990->2050
    MC_JPGDP = dfd["MC_JPGDP"].loc[1990:]

    # Get inflation index of FYRPRC (2022)
    SCALPR2 = MC_JPGDP.loc[FYRPRC]  # 2022 = column 33

    # no! the following interferes with the NEMS nohup.out file
    #print(f"SCALPR2:=============== {SCALPR2}")

    MNUMNR = dfd["MNUMNR_rwpre"]
    MNUMCR = dfd["MNUMCR_rwpre"]
    NDREG = dfd["NDREG_rwpre"]
    GWh_to_TRIL = dfd["GWh_to_TRIL_rwpre"]

    # Assuming 0 to make the prototype work
    CTLSO2 = 0.0
    CTLNOX = 0.0
    CTLHG = 0.0
    CTLSO2 = dfd["CTLSO2EM"].loc[1:NDREG, :].sum() * 0.000001
    CTLHG = dfd["CTLHGEM"].loc[1:NDREG, :].sum() * 0.000001
    CTLNOX = dfd["CTLNOXEM"].loc[1:NDREG, :].sum() * 0.000001

    # Net Summer Electricity Generating Capacity 1/-----
    # (gigawatts)---------------------------------------
    #   Electric Power Sector 2/------------------------
    #     Coal 3/---------------------------------------	T62(1,IR,IY,IS)=UCAPCSU(IR,IY)+UCAPCSN(IR,IY)+UCAPCSC(IR,IY)
    #     Oil and Natural Gas Steam 3/ 4/---------------	T62(2,IR,IY,IS)=UCAPOSU(IR,IY)+UCAPOSN(IR,IY)+UCAPOSC(IR,IY)+UCAPNGU(IR,IY)+UCAPNGN(IR,IY)
    #     Combined Cycle--------------------------------	T62(3,IR,IY,IS)=UCAPCCU(IR,IY)+UCAPCCN(IR,IY)+UCAPCCC(IR,IY)
    #     Combustion Turbine/Diesel---------------------	T62(4,IR,IY,IS)=UCAPCTU(IR,IY)+UCAPCTN(IR,IY)+UCAPCTC(IR,IY)
    #     Nuclear Power---------------------------------	T62(5,IR,IY,IS)=UCAPNUU(IR,IY)+UCAPNUN(IR,IY)+UCAPSMU(IR,IY)+UCAPSMN(IR,IY)
    #     Pumped Storage--------------------------------	T62(6,IR,IY,IS)=UCAPPSU(IR,IY)+UCAPPSN(IR,IY)
    #     Diurnal Storage-------------------------------	T62(124,IR,IY,IS)=UCAPDSU(IR,IY)+UCAPDSN(IR,IY)
    #      Hydrogen Turbine-----------------------------	T62(128,IR,IY,IS)=UCAPICU(IR,IY)+UCAPICN(IR,IY)
    #      Hydrogen CC_FC-------------------------------	T62(129,IR,IY,IS)=0.0
    #     Fuel Cells------------------------------------	T62(7,IR,IY,IS)=UCAPFCU(IR,IY)+UCAPFCN(IR,IY)
    #     Renewable Sources 5/--------------------------	T62(8,IR,IY,IS)=UCAPHYU(IR,IY)+UCAPGEU(IR,IY)+UCAPMSU(IR,IY)+UCAPWDU(IR,IY)+UCAPSTU(IR,IY)+UCAPPVU(IR,IY)+UCAPWNU(IR,IY)+UCAPWFU(IR,IY)+UCAPWLU(IR,IY)+UCAPPTU(IR,IY)+UCAPHYN(IR,IY)+UCAPGEN(IR,IY)+UCAPMSN(IR,IY)+UCAPWDN(IR,IY)+UCAPSTN(IR,IY)+UCAPPVN(IR,IY)+UCAPWNN(IR,IY)+UCAPWFN(IR,IY)+UCAPWLN(IR,IY)+UCAPPTN(IR,IY)+UCAPHYC(IR,IY)+UCAPGEC(IR,IY)+UCAPMSC(IR,IY)+UCAPWDC(IR,IY)+UCAPSTC(IR,IY)+UCAPPVC(IR,IY)+UCAPWNC(IR,IY)+UCAPWFC(IR,IY)+UCAPWLC(IR,IY)+UCAPPTC(IR,IY)
    #     Distributed Generation 6/---------------------	T62(96,IR,IY,IS)=UCAPDBU(IR,IY)+UCAPDBN(IR,IY)+UCAPDPU(IR,IY)+UCAPDPN(IR,IY)
    #       Total Capacity------------------------------	T62( 9,IR,IY,IS)=FSUM(T62( 1,IR,IY,IS),8)+T62(96,IR,IY,IS)+T62(124,IR,IY,IS)+T62(128,IR,IY,IS)+T62(129,IR,IY,IS)

    # T62(1,IR,IY,IS)=UCAPCSU(IR,IY)+UCAPCSN(IR,IY)+UCAPCSC(IR,IY)
    z[1] = dfd["UCAPCSU"] + dfd["UCAPCSN"] + dfd["UCAPCSC"]

    # T62(2,IR,IY,IS)=UCAPOSU(IR,IY)+UCAPOSN(IR,IY)+UCAPOSC(IR,IY)+UCAPNGU(IR,IY)+UCAPNGN(IR,IY)
    z[2] = (
        dfd["UCAPOSU"]
        + dfd["UCAPOSN"]
        + dfd["UCAPOSC"]
        + dfd["UCAPNGU"]
        + dfd["UCAPNGN"]
    )

    # T62(3,IR,IY,IS)=UCAPCCU(IR,IY)+UCAPCCN(IR,IY)+UCAPCCC(IR,IY)
    z[3] = dfd["UCAPCCU"] + dfd["UCAPCCN"] + dfd["UCAPCCC"]

    # T62(4,IR,IY,IS)=UCAPCTU(IR,IY)+UCAPCTN(IR,IY)+UCAPCTC(IR,IY)
    z[4] = dfd["UCAPCTU"] + dfd["UCAPCTN"] + dfd["UCAPCTC"] + dfd["UCAPBIU"] + dfd["UCAPBIN"]

    # T62(5,IR,IY,IS)=UCAPNUU(IR,IY)+UCAPNUN(IR,IY)+UCAPSMU(IR,IY)+UCAPSMN(IR,IY)
    z[5] = dfd["UCAPNUU"] + dfd["UCAPNUN"] + dfd["UCAPSMU"] + dfd["UCAPSMN"]

    # T62(6,IR,IY,IS)=UCAPPSU(IR,IY)+UCAPPSN(IR,IY)
    z[6] = dfd["UCAPPSU"] + dfd["UCAPPSN"]

    # T62(124,IR,IY,IS)=UCAPDSU(IR,IY)+UCAPDSN(IR,IY)
    z[124] = dfd["UCAPDSU"] + dfd["UCAPDSN"]

    # T62(128,IR,IY,IS)=UCAPICU(IR,IY)+UCAPICN(IR,IY)
    z[128] = dfd["UCAPICU"] + dfd["UCAPICN"]

    # T62(129,IR,IY,IS)=0.0
    z[129] = z[128] * 0.0

    # T62(7,IR,IY,IS)=UCAPFCU(IR,IY)+UCAPFCN(IR,IY)
    z[7] = dfd["UCAPFCU"] + dfd["UCAPFCN"]

    # T62(8,IR,IY,IS)=UCAPHYU(IR,IY)+UCAPGEU(IR,IY)+UCAPMSU(IR,IY)+UCAPWDU(IR,IY)
    # +UCAPSTU(IR,IY)+UCAPPVU(IR,IY)+UCAPWNU(IR,IY)+UCAPWFU(IR,IY)
    # +UCAPWLU(IR,IY)+UCAPPTU(IR,IY)+UCAPHYN(IR,IY)+UCAPGEN(IR,IY)
    # +UCAPMSN(IR,IY)+UCAPWDN(IR,IY)+UCAPSTN(IR,IY)+UCAPPVN(IR,IY)
    # +UCAPWNN(IR,IY)+UCAPWFN(IR,IY)+UCAPWLN(IR,IY)+UCAPPTN(IR,IY)
    # +UCAPHYC(IR,IY)+UCAPGEC(IR,IY)+UCAPMSC(IR,IY)+UCAPWDC(IR,IY)
    # +UCAPSTC(IR,IY)+UCAPPVC(IR,IY)+UCAPWNC(IR,IY)+UCAPWFC(IR,IY)
    # +UCAPWLC(IR,IY)+UCAPPTC(IR,IY)
    z[8] = (
        dfd["UCAPHYU"]
        + dfd["UCAPGEU"]
        + dfd["UCAPMSU"]
        + dfd["UCAPWDU"]
        + dfd["UCAPSTU"]
        + dfd["UCAPPVU"]
        + dfd["UCAPWNU"]
        + dfd["UCAPWFU"]
        + dfd["UCAPWLU"]
        + dfd["UCAPPTU"]
        + dfd["UCAPHYN"]
        + dfd["UCAPGEN"]
        + dfd["UCAPMSN"]
        + dfd["UCAPWDN"]
        + dfd["UCAPSTN"]
        + dfd["UCAPPVN"]
        + dfd["UCAPWNN"]
        + dfd["UCAPWFN"]
        + dfd["UCAPWLN"]
        + dfd["UCAPPTN"]
        + dfd["UCAPHYC"]
        + dfd["UCAPGEC"]
        + dfd["UCAPMSC"]
        + dfd["UCAPWDC"]
        + dfd["UCAPSTC"]
        + dfd["UCAPPVC"]
        + dfd["UCAPWNC"]
        + dfd["UCAPWFC"]
        + dfd["UCAPWLC"]
        + dfd["UCAPPTC"]
    )

    # T62(96,IR,IY,IS)=UCAPDBU(IR,IY)+UCAPDBN(IR,IY)+UCAPDPU(IR,IY)+UCAPDPN(IR,IY)
    z[96] = dfd["UCAPDBU"] + dfd["UCAPDBN"] + dfd["UCAPDPU"] + dfd["UCAPDPN"]

    # T62( 9,IR,IY,IS)=FSUM(T62( 1,IR,IY,IS),8)+T62(96,IR,IY,IS)+T62(124,IR,IY,IS)+T62(128,IR,IY,IS)+T62(129,IR,IY,IS)
    z[9] = (
        z[1]
        + z[2]
        + z[3]
        + z[4]
        + z[5]
        + z[6]
        + z[7]
        + z[8]
        + z[96]
        + z[124]
        + z[128]
        + z[129]
    )

    #    Cumulative Planned Additions 7/----------------
    #      Coal-----------------------------------------	T62(12,IR,IY,IS)=UADDCSU(1,IR,IY)+UADDCSN(1,IR,IY)+UADDCSC(IR,IY)
    #      Oil and Natural Gas Steam 4/-----------------	T62(13,IR,IY,IS)=UADDOSU(1,IR,IY)+UADDOSN(1,IR,IY)+UADDOSC(IR,IY)
    #      Combined Cycle-------------------------------	T62(14,IR,IY,IS)=UADDCCU(1,IR,IY)+UADDCCN(1,IR,IY)+UADDCCC(IR,IY)
    #      Combustion Turbine/Diesel--------------------	T62(15,IR,IY,IS)=UADDCTU(1,IR,IY)+UADDCTN(1,IR,IY)+UADDCTC(IR,IY)
    #      Nuclear Power--------------------------------	T62(16,IR,IY,IS)=UADDNUU(1,IR,IY)+UADDNUN(1,IR,IY)+UADDSMU(1,IR,IY)+UADDSMN(1,IR,IY)
    #      Pumped Storage-------------------------------	T62(17,IR,IY,IS)=UADDPSU(1,IR,IY)+UADDPSN(1,IR,IY)
    #      Diurnal Storage------------------------------	T62(125,IR,IY,IS)=UADDDSU(1,IR,IY)+UADDDSN(1,IR,IY)
    #      Hydrogen Turbine-----------------------------	T62(130,IR,IY,IS)=UADDICU(1,IR,IY)
    #      Hydrogen CC_FC-------------------------------	T62(131,IR,IY,IS) = 0.0
    #      Fuel Cells-----------------------------------	T62(18,IR,IY,IS)=UADDFCU(1,IR,IY)+UADDFCN(1,IR,IY)
    #      Renewable Sources 5/-------------------------	T62(19,IR,IY,IS)=UADDRNU(1,IR,IY)+UADDRNN(1,IR,IY)+UADDHYC(IR,IY)+UADDGEC(IR,IY)+UADDMSC(IR,IY)+UADDWDC(IR,IY)+UADDSTC(IR,IY)+UADDPVC(IR,IY)+UADDWNC(IR,IY)+UADDWFC(IR,IY)
    #      Distributed Generation 6/--------------------	T62(97,IR,IY,IS)=UADDDBU(1,IR,IY)+UADDDBN(1,IR,IY)+UADDDPU(1,IR,IY)+UADDDPN(1,IR,IY)
    #        Total Planned Additions--------------------	T62(20,IR,IY,IS)=UADDTLU(1,IR,IY)+UADDTLN(1,IR,IY)+UADDTLC(IR,IY)
    #

    # T62(12,IR,IY,IS)=UADDCSU(1,IR,IY)+UADDCSN(1,IR,IY)+UADDCSC(IR,IY)
    z[12] = dfd["UADDCSU"].loc[1] + dfd["UADDCSN"].loc[1] + dfd["UADDCSC"]

    # T62(13,IR,IY,IS)=UADDOSU(1,IR,IY)+UADDOSN(1,IR,IY)+UADDOSC(IR,IY)
    z[13] = dfd["UADDOSU"].loc[1] + dfd["UADDOSN"].loc[1] + dfd["UADDOSC"]

    # T62(14,IR,IY,IS)=UADDCCU(1,IR,IY)+UADDCCN(1,IR,IY)+UADDCCC(IR,IY)
    z[14] = dfd["UADDCCU"].loc[1] + dfd["UADDCCN"].loc[1] + dfd["UADDCCC"]

    # T62(15,IR,IY,IS)=UADDCTU(1,IR,IY)+UADDCTN(1,IR,IY)+UADDCTC(IR,IY)
    z[15] = dfd["UADDCTU"].loc[1] + dfd["UADDCTN"].loc[1] + dfd["UADDCTC"]

    # T62(16,IR,IY,IS)=UADDNUU(1,IR,IY)+UADDNUN(1,IR,IY)+UADDSMU(1,IR,IY)+UADDSMN(1,IR,IY)
    z[16] = (
        dfd["UADDNUU"].loc[1]
        + dfd["UADDNUN"].loc[1]
        + dfd["UADDSMU"].loc[1]
        + dfd["UADDSMN"].loc[1]
    )

    # T62(17,IR,IY,IS)=UADDPSU(1,IR,IY)+UADDPSN(1,IR,IY)
    z[17] = dfd["UADDPSU"].loc[1] + dfd["UADDPSN"].loc[1]

    # T62(125,IR,IY,IS)=UADDDSU(1,IR,IY)+UADDDSN(1,IR,IY)
    z[125] = dfd["UADDDSU"].loc[1] + dfd["UADDDSN"].loc[1]

    # T62(130,IR,IY,IS)=UADDICU(1,IR,IY)
    z[130] = dfd["UADDICU"].loc[1]

    # T62(131,IR,IY,IS) = 0.0
    z[131] = z[130] * 0.0

    # T62(18,IR,IY,IS)=UADDFCU(1,IR,IY)+UADDFCN(1,IR,IY)
    z[18] = dfd["UADDFCU"].loc[1] + dfd["UADDFCN"].loc[1]

    # T62(19,IR,IY,IS)=UADDRNU(1,IR,IY)+UADDRNN(1,IR,IY)+UADDHYC(IR,IY)
    # +UADDGEC(IR,IY)+UADDMSC(IR,IY)+UADDWDC(IR,IY)+UADDSTC(IR,IY)
    # +UADDPVC(IR,IY)+UADDWNC(IR,IY)+UADDWFC(IR,IY)

    z[19] = (
        dfd["UADDRNU"].loc[1]
        + dfd["UADDRNN"].loc[1]
        + dfd["UADDHYC"]
        + dfd["UADDGEC"]
        + dfd["UADDMSC"]
        + dfd["UADDWDC"]
        + dfd["UADDSTC"]
        + dfd["UADDPVC"]
        + dfd["UADDWNC"]
        + dfd["UADDWFC"]
    )

    # T62(97,IR,IY,IS)=UADDDBU(1,IR,IY)+UADDDBN(1,IR,IY)+UADDDPU(1,IR,IY)+UADDDPN(1,IR,IY)
    z[97] = (
        dfd["UADDDBU"].loc[1]
        + dfd["UADDDBN"].loc[1]
        + dfd["UADDDPU"].loc[1]
        + dfd["UADDDPN"].loc[1]
    )

    # T62(20,IR,IY,IS)=UADDTLU(1,IR,IY)+UADDTLN(1,IR,IY)+UADDTLC(IR,IY)
    z[20] = dfd["UADDTLU"].loc[1] + dfd["UADDTLN"].loc[1] + dfd["UADDTLC"]

    #    Cumulative Unplanned Additions 7/--------------
    #      Coal-----------------------------------------	T62(23,IR,IY,IS)=UADDCSU(2,IR,IY)+UADDCSN(2,IR,IY)
    #      Oil and Natural Gas Steam 4/-----------------	T62(24,IR,IY,IS)=UADDOSU(2,IR,IY)+UADDOSN(2,IR,IY)
    #      Combined Cycle-------------------------------	T62(25,IR,IY,IS)=UADDCCU(2,IR,IY)+UADDCCN(2,IR,IY)
    #      Combustion Turbine/Diesel--------------------	T62(26,IR,IY,IS)=UADDCTU(2,IR,IY)+UADDCTN(2,IR,IY)
    #      Nuclear Power--------------------------------	T62(27,IR,IY,IS)=UADDNUU(2,IR,IY)+UADDNUN(2,IR,IY)+UADDSMU(2,IR,IY)+UADDSMN(2,IR,IY)
    #      Pumped Storage-------------------------------	T62(28,IR,IY,IS)=UADDPSU(2,IR,IY)+UADDPSN(2,IR,IY)
    #      Diurnal Storage------------------------------	T62(126,IR,IY,IS)=UADDDSU(2,IR,IY)+UADDDSN(2,IR,IY)
    #      Hydrogen Turbine-----------------------------	T62(132,IR,IY,IS)=UADDICU(2,IR,IY)+UADDICN(2,IR,IY)
    #      Hydrogen CC_FC-------------------------------	T62(133,IR,IY,IS)=0.0
    #      Fuel Cells-----------------------------------	T62(29,IR,IY,IS)=UADDFCU(2,IR,IY)+UADDFCN(2,IR,IY)
    #      Renewable Sources 5/-------------------------	T62(30,IR,IY,IS)=UADDRNU(2,IR,IY)+UADDRNN(2,IR,IY)
    #      Distributed Generation 6/--------------------	T62(98,IR,IY,IS)=UADDDBU(2,IR,IY)+UADDDBN(2,IR,IY)+UADDDPU(2,IR,IY)+UADDDPN(2,IR,IY)
    #        Total Unplanned Additions------------------	T62(31,IR,IY,IS)=UADDTLU(2,IR,IY)+UADDTLN(2,IR,IY)
    #    Cumulative Electric Power Sector Additions-----	T62(34,IR,IY,IS)=UADDTLU(1,IR,IY)+UADDTLU(2,IR,IY)+UADDTLC(IR,IY)+UADDTLN(1,IR,IY)+UADDTLN(2,IR,IY)

    # T62(23,IR,IY,IS)=UADDCSU(2,IR,IY)+UADDCSN(2,IR,IY)
    z[23] = dfd["UADDCSU"].loc[2] + dfd["UADDCSN"].loc[2]

    # T62(24,IR,IY,IS)=UADDOSU(2,IR,IY)+UADDOSN(2,IR,IY)
    z[24] = dfd["UADDOSU"].loc[2] + dfd["UADDOSN"].loc[2]

    # T62(25,IR,IY,IS)=UADDCCU(2,IR,IY)+UADDCCN(2,IR,IY)
    z[25] = dfd["UADDCCU"].loc[2] + dfd["UADDCCN"].loc[2]

    # T62(26,IR,IY,IS)=UADDCTU(2,IR,IY)+UADDCTN(2,IR,IY)
    z[26] = dfd["UADDCTU"].loc[2] + dfd["UADDCTN"].loc[2]

    # T62(27,IR,IY,IS)=UADDNUU(2,IR,IY)+UADDNUN(2,IR,IY)+UADDSMU(2,IR,IY)+UADDSMN(2,IR,IY)
    z[27] = (
        dfd["UADDNUU"].loc[2]
        + dfd["UADDNUN"].loc[2]
        + dfd["UADDSMU"].loc[2]
        + dfd["UADDSMN"].loc[2]
    )

    # T62(28,IR,IY,IS)=UADDPSU(2,IR,IY)+UADDPSN(2,IR,IY)
    z[28] = dfd["UADDPSU"].loc[2] + dfd["UADDPSN"].loc[2]

    # T62(126,IR,IY,IS)=UADDDSU(2,IR,IY)+UADDDSN(2,IR,IY)
    z[126] = dfd["UADDDSU"].loc[2] + dfd["UADDDSN"].loc[2]

    # T62(132,IR,IY,IS)=UADDICU(2,IR,IY)+UADDICN(2,IR,IY)
    z[132] = dfd["UADDICU"].loc[2] + dfd["UADDICN"].loc[2]

    # T62(133,IR,IY,IS)=0.0
    z[133] = z[132] * 0.0

    # T62(29,IR,IY,IS)=UADDFCU(2,IR,IY)+UADDFCN(2,IR,IY)
    z[29] = dfd["UADDFCU"].loc[2] + dfd["UADDFCN"].loc[2]

    # T62(30,IR,IY,IS)=UADDRNU(2,IR,IY)+UADDRNN(2,IR,IY)
    z[30] = dfd["UADDRNU"].loc[2] + dfd["UADDRNN"].loc[2]

    # T62(98,IR,IY,IS)=UADDDBU(2,IR,IY)+UADDDBN(2,IR,IY)+UADDDPU(2,IR,IY)+UADDDPN(2,IR,IY)
    z[98] = (
        dfd["UADDDBU"].loc[2]
        + dfd["UADDDBN"].loc[2]
        + dfd["UADDDPU"].loc[2]
        + dfd["UADDDPN"].loc[2]
    )

    # T62(31,IR,IY,IS)=UADDTLU(2,IR,IY)+UADDTLN(2,IR,IY)
    z[31] = dfd["UADDTLU"].loc[2] + dfd["UADDTLN"].loc[2]

    # T62(34,IR,IY,IS)=UADDTLU(1,IR,IY)+UADDTLU(2,IR,IY)+UADDTLC(IR,IY)+UADDTLN(1,IR,IY)+UADDTLN(2,IR,IY)
    z[34] = (
        dfd["UADDTLU"].loc[1]
        + dfd["UADDTLU"].loc[2]
        + dfd["UADDTLC"]
        + dfd["UADDTLN"].loc[1]
        + dfd["UADDTLN"].loc[2]
    )

    #    Cumulative Retirements 8/----------------------
    #      Coal-----------------------------------------	T62(102:110,IR,IY,IS)=-999.
    #      Oil and Natural Gas Steam 4/-----------------	T62(103,IR,IY,IS)=URETOSU(IR,IY)
    #      Combined Cycle-------------------------------	T62(104,IR,IY,IS)=URETCCU(IR,IY)
    #      Combustion Turbine/Diesel--------------------	T62(105,IR,IY,IS)=URETCTU(IR,IY)
    #      Nuclear Power--------------------------------	T62(106,IR,IY,IS)=URETNUU(IR,IY)+URETSMU(IR,IY)
    #      Pumped Storage-------------------------------	T62(107,IR,IY,IS)=URETPSU(IR,IY)
    #      Diurnal Storage------------------------------	T62(127,IR,IY,IS)=URETDSU(IR,IY)
    #      Hydrogen Turbine-----------------------------	T62(134,IR,IY,IS)=URETICU(IR,IY)
    #      Hydrogen CC_FC-------------------------------	T62(135,IR,IY,IS)=0.0
    #      Fuel Cells-----------------------------------	T62(108,IR,IY,IS)=URETFCU(IR,IY)
    #      Renewable Sources 5/-------------------------	T62(109,IR,IY,IS)=URETRNU(IR,IY)
    #        Total--------------------------------------	T62(110,IR,IY,IS)=URETTLU(IR,IY)

    #  T62(102,IR,IY,IS) = URETCSU(IR,IY)
    z[102] = dfd["URETCSU"]

    # T62(103,IR,IY,IS)=URETOSU(IR,IY)
    z[103] = dfd["URETOSU"]

    # T62(104,IR,IY,IS)=URETCCU(IR,IY)
    z[104] = dfd["URETCCU"]

    # T62(105,IR,IY,IS)=URETCTU(IR,IY)
    z[105] = dfd["URETCTU"]

    # T62(106,IR,IY,IS)=URETNUU(IR,IY)+URETSMU(IR,IY)
    z[106] = dfd["URETNUU"] + dfd["URETSMU"]

    # T62(107,IR,IY,IS)=URETPSU(IR,IY)
    z[107] = dfd["URETPSU"]

    # T62(127,IR,IY,IS)=URETDSU(IR,IY)
    z[127] = dfd["URETDSU"]

    # T62(134,IR,IY,IS)=URETICU(IR,IY)
    z[134] = dfd["URETICU"]

    # T62(135,IR,IY,IS)=0.0
    z[135] = z[134] * 0.0

    # T62(108,IR,IY,IS)=URETFCU(IR,IY)
    z[108] = dfd["URETFCU"]

    # T62(109,IR,IY,IS)=URETRNU(IR,IY)
    z[109] = dfd["URETRNU"]

    # T62(110,IR,IY,IS)=URETTLU(IR,IY)
    z[110] = dfd["URETTLU"]

    #   End-Use Sectors 9/------------------------------
    #     Coal------------------------------------------	T62(36,IR,IY,IS)=CGTOTCAPNR(IR,IY,1)*0.001
    #     Petroleum-------------------------------------	T62(37,IR,IY,IS)=CGTOTCAPNR(IR,IY,2)*0.001
    #     Natural Gas-----------------------------------	T62(38,IR,IY,IS)=CGTOTCAPNR(IR,IY,4)*0.001
    #     Other Gaseous Fuels 10/-----------------------	T62(39,IR,IY,IS)=CGTOTCAPNR(IR,IY,3)*0.001
    #     Renewable Sources 5/--------------------------	T62(40,IR,IY,IS)=(CGTOTCAPNR(IR,IY,5)+CGTOTCAPNR(IR,IY,6)+CGTOTCAPNR(IR,IY,7)+CGTOTCAPNR(IR,IY,8)+CGTOTCAPNR(IR,IY,9)+CGTOTCAPNR(IR,IY,10)+CGTOTCAPNR(IR,IY,12))*0.001
    #     Other 11/-------------------------------------	T62(41,IR,IY,IS)=CGTOTCAPNR(IR,IY,11)*0.001
    #       Total---------------------------------------	T62(42,IR,IY,IS)=FSUM(T62(36,IR,IY,IS),6)

    # T62(36,IR,IY,IS)=CGTOTCAPNR(IR,IY,1) * 0.001
    # Note, need groupby to remove index level 1
    z[36] = dfd["CGTOTCAPNR"].loc[:, 1, :] * 0.001

    # T62(37,IR,IY,IS)=CGTOTCAPNR(IR,IY,2) * 0.001
    # z[37] = dfd['CGTOTCAPNR'].loc[2] * 0.001
    z[37] = dfd["CGTOTCAPNR"].loc[:, 2, :] * 0.001

    # T62(38,IR,IY,IS)=CGTOTCAPNR(IR,IY,4) * 0.001
    # z[38] = dfd['CGTOTCAPNR'].loc[4] * 0.001
    z[38] = dfd["CGTOTCAPNR"].loc[:, 4, :] * 0.001

    # T62(39,IR,IY,IS)=CGTOTCAPNR(IR,IY,3) * 0.001
    # z[39] = dfd['CGTOTCAPNR'].loc[3] * 0.001
    z[39] = dfd["CGTOTCAPNR"].loc[:, 3, :] * 0.001

    # T62(40,IR,IY,IS)=(CGTOTCAPNR(IR,IY,5)+CGTOTCAPNR(IR,IY,6)
    # +CGTOTCAPNR(IR,IY,7)+CGTOTCAPNR(IR,IY,8)
    # +CGTOTCAPNR(IR,IY,9)+CGTOTCAPNR(IR,IY,10)
    # +CGTOTCAPNR(IR,IY,12)) * 0.001 * EUPVCAPADJ(IY)
    # (MNUMNR	NUMCGF) = (28 12)

    dfd["EUPVCAPADJ"] = (
        (
            dfd["CGCOMMCAP"].loc[11, 8]
            + dfd["CGRESCAP"].loc[11, 8]
            + dfd["CGINDLCAP"].loc[11, 8]
        )
        / dfd["CGTOTCAPNR"].loc[MNUMNR, 10]
    ).fillna(1)

    z[40] = (
        dfd["CGTOTCAPNR"].loc[:, 5:9, :].groupby("MNUMNR").sum()
        + dfd["CGTOTCAPNR"].loc[:, 12, :]
        + dfd["CGTOTCAPNR"].loc[:, 10, :] * dfd["EUPVCAPADJ"]
    ) * 0.001

    # T62(41,IR,IY,IS)=CGTOTCAPNR(IR,IY,11) * 0.001
    z[41] = dfd["CGTOTCAPNR"].loc[:, 11, :] * 0.001

    # T62(42,IR,IY,IS)=FSUM(T62(36,IR,IY,IS),6)
    z[42] = z[36] + z[37] + z[38] + z[39] + z[40] + z[41]

    # Electricity Sales---------------------------------
    # (billion kilowatthours)---------------------------
    #   Residential-------------------------------------	T62(43,IR,IY,IS)=QELRSN(IR,IY)*.001
    #   Commercial/Other--------------------------------	T62(44,IR,IY,IS)=QELCMN(IR,IY)*.001
    #   Industrial--------------------------------------	T62(45,IR,IY,IS)=QELINN(IR,IY)*.001
    #   Transportation----------------------------------	T62(46,IR,IY,IS)=QELTRN(IR,IY)*.001
    #     Total Sales-----------------------------------	T62(47,IR,IY,IS)=QELASN(IR,IY)*.001
    #
    # ftab.f ---------------------------------------------------------------------------------------
    #         IF (IR.EQ.mnumnr) THEN
    # !        --- Sales
    #            T62(43,IR,IY,IS) = QELRS(11,IY) / .003412
    #            T62(44,IR,IY,IS) = QELCM(11,IY) / .003412
    #            T62(45,IR,IY,IS) = QELIN(11,IY) / .003412
    #            T62(46,IR,IY,IS) = QELTR(11,IY) / .003412
    #            T62(47,IR,IY,IS) = QELAS(11,IY) / .003412
    # !          --- End Use Prices
    #            T62(75,IR,IY,IS) = PELRS(11,IY) * .3412
    #            T62(76,IR,IY,IS) = PELCM(11,IY) * .3412
    #            T62(77,IR,IY,IS) = PELIN(11,IY) * .3412
    #            T62(78,IR,IY,IS) = PELTR(11,IY) * .3412
    #            T62(79,IR,IY,IS) = PELAS(11,IY) * .3412
    # !        --- FUEL PRICES
    #            T62(80,IR,IY,IS) = PCLEL(11,IY)
    #            T62(81,IR,IY,IS) = PNGEL(11,IY)
    #            T62(82,IR,IY,IS) = PDSEL(11,IY)
    #            T62(83,IR,IY,IS) = PRSEL(11,IY)
    #         ELSE
    # !        --- Sales
    #            T62(43,IR,IY,IS) = QELRSN(IR,IY) * .001
    #            T62(44,IR,IY,IS) = QELCMN(IR,IY) * .001
    #            T62(45,IR,IY,IS) = QELINN(IR,IY) * .001
    #            T62(46,IR,IY,IS) = QELTRN(IR,IY) * .001
    #            T62(47,IR,IY,IS) = QELASN(IR,IY) * .001
    # !          --- End Use Prices
    #            T62(75,IR,IY,IS) = PELRSNR(IR,IY) * .1
    #            T62(76,IR,IY,IS) = PELCMNR(IR,IY) * .1
    #            T62(77,IR,IY,IS) = PELINNR(IR,IY) * .1
    #            T62(78,IR,IY,IS) = PELTRNR(IR,IY) * .1
    #            T62(79,IR,IY,IS) = PELASNR(IR,IY) * .1
    # !        --- FUEL PRICES
    #            T62(80,IR,IY,IS) = UPCOALAVG(IR,IY)
    #            T62(81,IR,IY,IS) = UPGASPRC(IR,IY)
    #            T62(82,IR,IY,IS) = UPDISPRC(IR,IY)
    #            T62(83,IR,IY,IS) = UPRESPRC(IR,IY)
    #          ENDIF
    # ----------------------------------------------------------------------------------------------

    # !        --- Sales
    z[43] = dfd["QELRSN"] * 0.001
    z[43].loc[MNUMNR] = dfd["QELRS"].loc[MNUMCR] / GWh_to_TRIL  # * .001

    z[44] = dfd["QELCMN"] * 0.001
    z[44].loc[MNUMNR] = dfd["QELCM"].loc[MNUMCR] / GWh_to_TRIL  # * .001

    z[45] = dfd["QELINN"] * 0.001
    z[45].loc[MNUMNR] = dfd["QELIN"].loc[MNUMCR] / GWh_to_TRIL  # * .001

    z[46] = dfd["QELTRN"] * 0.001
    z[46].loc[MNUMNR] = dfd["QELTR"].loc[MNUMCR] / GWh_to_TRIL  # * .001

    z[47] = dfd["QELASN"] * 0.001
    z[47].loc[MNUMNR] = dfd["QELAS"].loc[MNUMCR] / GWh_to_TRIL  # * .001

    # !          --- End Use Prices
    z[75] = dfd["PELRSNR"] * 0.1 * SCALPR2
    z[75].loc[MNUMNR] = dfd["AMPBLK/PELRS"].loc[MNUMCR] * 0.3412 * SCALPR2

    z[76] = dfd["PELCMNR"] * 0.1 * SCALPR2
    z[76].loc[MNUMNR] = dfd["AMPBLK/PELCM"].loc[MNUMCR] * 0.3412 * SCALPR2

    z[77] = dfd["PELINNR"] * 0.1 * SCALPR2
    z[77].loc[MNUMNR] = dfd["AMPBLK/PELIN"].loc[MNUMCR] * 0.3412 * SCALPR2

    z[78] = dfd["PELTRNR"] * 0.1 * SCALPR2
    z[78].loc[MNUMNR] = dfd["AMPBLK/PELTR"].loc[MNUMCR] * 0.3412 * SCALPR2

    z[79] = dfd["PELASNR"] * 0.1 * SCALPR2
    z[79].loc[MNUMNR] = dfd["AMPBLK/PELAS"].loc[MNUMCR] * 0.3412 * SCALPR2

    # !        --- FUEL PRICES
    z[80] = dfd["UPCOALAVG"] * SCALPR2
    z[80].loc[MNUMNR] = dfd["AMPBLK/PCLEL"].loc[MNUMCR] * SCALPR2

    z[81] = dfd["UPGASPRC"] * SCALPR2
    z[81].loc[MNUMNR] = dfd["AMPBLK/PNGEL"].loc[MNUMCR] * SCALPR2

    z[82] = dfd["UPDISPRC"] * SCALPR2
    z[82].loc[MNUMNR] = dfd["AMPBLK/PDSEL"].loc[MNUMCR] * SCALPR2

    z[83] = dfd["UPRESPRC"] * SCALPR2
    z[83].loc[MNUMNR] = dfd["AMPBLK/PRSEL"].loc[MNUMCR] * SCALPR2

    # Net Energy for Load (billion kilowatthours) 12/---
    #   Gross International Imports---------------------	T62(48,IR,IY,IS)=(UTIMPF(IR,IY)+UTIMPE(IR,IY))*.001
    #   Gross International Exports---------------------	T62(49,IR,IY,IS)=(UTEXPF(IR,IY)+UTEXPE(IR,IY))*.001
    #   Gross Interregional Electricity Imports---------	T62(50,IR,IY,IS)=(UTDMMF(IR,IY)+UTDMME(IR,IY)+UTEXMF(IR,IY)+UTEXME(IR,IY))*.001
    #   Gross Interregional Electricity Exports---------	T62(51,IR,IY,IS)=(UTEXMF(IR,IY)+UTEXME(IR,IY))*.001
    #   Purchases from Combined Heat and Power 9/-------	T62(52,IR,IY,IS)=T62(73,IR,IY,IS)
    #   Electric Power Sector Generation for Customers--	T62(53,IR,IY,IS)=T62(62,IR,IY,IS)
    #     Total Net Energy for Load---------------------	T62(54,IR,IY,IS)=T62(48,IR,IY,IS)-T62(49,IR,IY,IS)+T62(50,IR,IY,IS)-T62(51,IR,IY,IS)+T62(52,IR,IY,IS)+T62(53,IR,IY,IS)
    # --------------------------------------------------

    # T62(48,IR,IY,IS)=(UTIMPF(IR,IY)+UTIMPE(IR,IY))*.001
    z[48] = (dfd["UTIMPF"] + dfd["UTIMPE"]) * 0.001

    # T62(49,IR,IY,IS)=(UTEXPF(IR,IY)+UTEXPE(IR,IY))*.001
    z[49] = (dfd["UTEXPF"] + dfd["UTEXPE"]) * 0.001

    # T62(50,IR,IY,IS)=(UTDMMF(IR,IY)+UTDMME(IR,IY)+UTEXMF(IR,IY)+UTEXME(IR,IY))*.001
    z[50] = (dfd["UTDMMF"] + dfd["UTDMME"] + dfd["UTEXMF"] + dfd["UTEXME"]) * 0.001

    # T62(51,IR,IY,IS)=(UTEXMF(IR,IY)+UTEXME(IR,IY))*.001
    z[51] = (dfd["UTEXMF"] + dfd["UTEXME"]) * 0.001

    # T62(52,IR,IY,IS)=T62(73,IR,IY,IS)

    # T62(53,IR,IY,IS)=T62(62,IR,IY,IS)
    # z[53] =z[62]

    # # T62(54,IR,IY,IS)=T62(48,IR,IY,IS)-T62(49,IR,IY,IS)+T62(50,IR,IY,IS)-T62(51,IR,IY,IS)+T62(52,IR,IY,IS)+T62(53,IR,IY,IS)
    # z[54] = z[48] - z[49] + z[50] - z[51] + z[52] + z[53]

    # Generation by Fuel Type---------------------------
    # (billion kilowatthours)---------------------------
    #   Electric Power Sector---------------------------
    #     Coal------------------------------------------	T62(55,IR,IY,IS)=UGNCLNR(1,IR,IY)+UGNCLNR(2,IR,IY)+(CGNTGEN(IR,IY,1,1)+CGNTGEN(IR,IY,1,2))*0.001
    #     Petroleum-------------------------------------	T62(56,IR,IY,IS)=UGNDSNR(1,IR,IY)+UGNRLNR(1,IR,IY)+UGNRHNR(1,IR,IY)+UGNDSNR(2,IR,IY)+UGNRLNR(2,IR,IY)+UGNRHNR(2,IR,IY)+(CGNTGEN(IR,IY,2,1)+CGNTGEN(IR,IY,2,2))*0.001
    #     Natural Gas-----------------------------------	T62(57,IR,IY,IS)=UGNGFNR(1,IR,IY)+UGNGINR(1,IR,IY)+UGNGFNR(2,IR,IY)+UGNGINR(2,IR,IY)+(CGNTGEN(IR,IY,3,1)+CGNTGEN(IR,IY,3,2))*0.001
    #     Nuclear---------------------------------------	T62(58,IR,IY,IS)=UGNURNR(1,IR,IY)+UGNURNR(2,IR,IY)
    #     Pumped Storage and Other 13/------------------	T62(59,IR,IY,IS)=UGNPSNR(1,IR,IY)+UGNPSNR(2,IR,IY)+UGNSDNR(1,IR,IY)+UGNSDNR(2,IR,IY)+(CGNTGEN(IR,IY,9,1)+CGNTGEN(IR,IY,9,2)+CGNTGEN(IR,IY,10,1)+CGNTGEN(IR,IY,10,2))*0.001
    #     Hydrogen--------------------------------------	T62(136,IR,IY,IS)=UGNGCNR(1,IR,IY)+UGNGCNR(2,IR,IY)
    #     Renewable Sources 14/-------------------------	T62(60,IR,IY,IS)=UGNHYNR(1,IR,IY)+UGNHYNR(2,IR,IY)+UGNGENR(1,IR,IY)+UGNGENR(2,IR,IY)+UGNMSNR(1,IR,IY)+UGNMSNR(2,IR,IY)+UGNWDNR(1,IR,IY)+UGNWDNR(2,IR,IY)+UGNSONR(1,IR,IY)+UGNSONR(2,IR,IY)+UGNPVNR(1,IR,IY)+UGNPVNR(2,IR,IY)+UGNPTNR(1,IR,IY)+UGNPTNR(2,IR,IY)+UGNWNNR(1,IR,IY)+UGNWNNR(2,IR,IY)+UGNWLNR(1,IR,IY)+UGNWLNR(2,IR,IY)+UGNWFNR(1,IR,IY)+UGNWFNR(2,IR,IY)+(CGNTGEN(IR,IY,4,1)+CGNTGEN(IR,IY,4,2)+CGNTGEN(IR,IY,5,1)+CGNTGEN(IR,IY,5,2)+CGNTGEN(IR,IY,6,1)+CGNTGEN(IR,IY,6,2)+CGNTGEN(IR,IY,7,1)+CGNTGEN(IR,IY,7,2)+CGNTGEN(IR,IY,8,1)+CGNTGEN(IR,IY,8,2))*0.001
    #     Distributed Generation 6/---------------------	T62(99,IR,IY,IS)=UGNDDNR(1,IR,IY)+UGNDDNR(2,IR,IY)+UGNDGNR(1,IR,IY)+UGNDGNR(2,IR,IY)
    #       Total Generation----------------------------	T62(61,IR,IY,IS)=FSUM(T62(55,IR,IY,IS),6)+T62(99,IR,IY,IS) + T62(136,IR,IY,IS)
    #     Sales to Customers----------------------------	T62(62,IR,IY,IS)=T62(61,IR,IY,IS)-CGOTGEN(IR,IY,1)-CGOTGEN(IR,IY,2)
    #     Generation for Own Use------------------------	T62(63,IR,IY,IS)=CGOTGEN(IR,IY,1)+CGOTGEN(IR,IY,2)
    # --------------------------------------------------

    # Coal: T62(55,IR,IY,IS)=UGNCLNR(1,IR,IY)+UGNCLNR(2,IR,IY)+(CGNTGEN(IR,IY,1,1)+CGNTGEN(IR,IY,1,2)) * 0.001
    z[55] = (
        dfd["UGNCLNR"].loc[1]
        + dfd["UGNCLNR"].loc[2]
        + (
            dfd["CGNTGEN"].loc[(slice(None), 1, 1)]
            + dfd["CGNTGEN"].loc[(slice(None), 1, 2)]
        )
        * 0.001
    )

    # T62(56,IR,IY,IS)=UGNDSNR(1,IR,IY)+UGNRLNR(1,IR,IY)+UGNRHNR(1,IR,IY)+UGNDSNR(2,IR,IY)+UGNRLNR(2,IR,IY)+UGNRHNR(2,IR,IY)+(CGNTGEN(IR,IY,2,1)+CGNTGEN(IR,IY,2,2)) * 0.001
    z[56] = (
        dfd["UGNDSNR"].loc[1]
        + dfd["UGNRLNR"].loc[1]
        + dfd["UGNRHNR"].loc[1]
        + dfd["UGNDSNR"].loc[2]
        + dfd["UGNRLNR"].loc[2]
        + dfd["UGNRHNR"].loc[2]
        + (
            dfd["CGNTGEN"].loc[(slice(None), 2, 1)]
            + dfd["CGNTGEN"].loc[(slice(None), 2, 2)]
        )
        * 0.001
    )

    # T62(57,IR,IY,IS)=UGNGFNR(1,IR,IY)+UGNGINR(1,IR,IY)+UGNGFNR(2,IR,IY)+UGNGINR(2,IR,IY)+(CGNTGEN(IR,IY,3,1)+CGNTGEN(IR,IY,3,2)) * 0.001
    z[57] = (
        dfd["UGNGFNR"].loc[1]
        + dfd["UGNGINR"].loc[1]
        + dfd["UGNGFNR"].loc[2]
        + dfd["UGNGINR"].loc[2]
        + (
            dfd["CGNTGEN"].loc[(slice(None), 3, 1)]
            + dfd["CGNTGEN"].loc[(slice(None), 3, 2)]
        )
        * 0.001
    )

    # T62(58,IR,IY,IS)=UGNURNR(1,IR,IY)+UGNURNR(2,IR,IY)
    z[58] = dfd["UGNURNR"].loc[1] + dfd["UGNURNR"].loc[2]

    # T62(59,IR,IY,IS)=UGNPSNR(1,IR,IY)+UGNPSNR(2,IR,IY)+UGNSDNR(1,IR,IY)+UGNSDNR(2,IR,IY)
    # +(CGNTGEN(IR,IY,9,1)+CGNTGEN(IR,IY,9,2)+CGNTGEN(IR,IY,10,1)
    #   +CGNTGEN(IR,IY,10,2)) * 0.001
    z[59] = (
        dfd["UGNPSNR"].loc[1]
        + dfd["UGNPSNR"].loc[2]
        + dfd["UGNSDNR"].loc[1]
        + dfd["UGNSDNR"].loc[2]
        + (
            dfd["CGNTGEN"].loc[(slice(None), 9, 1)]
            + dfd["CGNTGEN"].loc[(slice(None), 9, 2)]
            + dfd["CGNTGEN"].loc[(slice(None), 10, 1)]
            + dfd["CGNTGEN"].loc[(slice(None), 10, 2)]
        )
        * 0.001
    )

    # T62(136,IR,IY,IS)=UGNGCNR(1,IR,IY)+UGNGCNR(2,IR,IY)
    z[136] = dfd["UGNGCNR"].loc[1] + dfd["UGNGCNR"].loc[2]

    # T62(60,IR,IY,IS) = UGNHYNR(1,IR,IY) + UGNHYNR(2,IR,IY) + &
    # UGNGENR(1,IR,IY) + UGNGENR(2,IR,IY) + &
    # UGNMSNR(1,IR,IY) + UGNMSNR(2,IR,IY) + &
    # UGNWDNR(1,IR,IY) + UGNWDNR(2,IR,IY) + &
    # UGNSONR(1,IR,IY) + UGNSONR(2,IR,IY) + &
    # UGNPVNR(1,IR,IY) + UGNPVNR(2,IR,IY) + &
    # UGNPTNR(1,IR,IY) + UGNPTNR(2,IR,IY) + &
    # UGNWNNR(1,IR,IY) + UGNWNNR(2,IR,IY) + &
    # UGNWLNR(1,IR,IY) + UGNWLNR(2,IR,IY) + &
    # UGNWFNR(1,IR,IY) + UGNWFNR(2,IR,IY) + &
    # (CGNTGEN(IR,IY, 4,1) + CGNTGEN(IR,IY, 4,2) + &
    # CGNTGEN(IR,IY, 5,1) + CGNTGEN(IR,IY, 5,2) + &
    # CGNTGEN(IR,IY, 6,1) + CGNTGEN(IR,IY, 6,2) + &
    # CGNTGEN(IR,IY, 7,1) + CGNTGEN(IR,IY, 7,2) + &
    # CGNTGEN(IR,IY, 8,1) + CGNTGEN(IR,IY, 8,2)) * 0.001

    z[60] = (
        dfd["UGNHYNR"].loc[1]
        + dfd["UGNHYNR"].loc[2]
        + dfd["UGNGENR"].loc[1]
        + dfd["UGNGENR"].loc[2]
        + dfd["UGNMSNR"].loc[1]
        + dfd["UGNMSNR"].loc[2]
        + dfd["UGNWDNR"].loc[1]
        + dfd["UGNWDNR"].loc[2]
        + dfd["UGNSONR"].loc[1]
        + dfd["UGNSONR"].loc[2]
        + dfd["UGNPVNR"].loc[1]
        + dfd["UGNPVNR"].loc[2]
        + dfd["UGNPTNR"].loc[1]
        + dfd["UGNPTNR"].loc[2]
        + dfd["UGNWNNR"].loc[1]
        + dfd["UGNWNNR"].loc[2]
        + dfd["UGNWLNR"].loc[1]
        + dfd["UGNWLNR"].loc[2]
        + dfd["UGNWFNR"].loc[1]
        + dfd["UGNWFNR"].loc[2]
        + (
            dfd["CGNTGEN"].loc[(slice(None), 4, [1, 2]), :].groupby(level=0).sum()
            + dfd["CGNTGEN"].loc[(slice(None), 5, [1, 2]), :].groupby(level=0).sum()
            + dfd["CGNTGEN"].loc[(slice(None), 6, [1, 2]), :].groupby(level=0).sum()
            + dfd["CGNTGEN"].loc[(slice(None), 7, [1, 2]), :].groupby(level=0).sum()
            + dfd["CGNTGEN"].loc[(slice(None), 8, [1, 2]), :].groupby(level=0).sum()
        )
        * 0.001
    )

    # T62(99,IR,IY,IS)=UGNDDNR(1,IR,IY)+UGNDDNR(2,IR,IY)+UGNDGNR(1,IR,IY)+UGNDGNR(2,IR,IY)
    z[99] = (
        dfd["UGNDDNR"].loc[1]
        + dfd["UGNDDNR"].loc[2]
        + dfd["UGNDGNR"].loc[1]
        + dfd["UGNDGNR"].loc[2]
    )

    # T62(61,IR,IY,IS)=FSUM(T62(55,IR,IY,IS),6)+T62(99,IR,IY,IS) + T62(136,IR,IY,IS)
    z[61] = z[55] + z[56] + z[57] + z[58] + z[59] + z[60] + z[99] + z[136]

    # T62(62,IR,IY,IS)=T62(61,IR,IY,IS)-CGOTGEN(IR,IY,1)-CGOTGEN(IR,IY,2)
    # dfd['CGOTGEN'] => (MNUMNR	M10) = 28 10

    z[62] = z[61] - dfd["CGOTGEN"].loc[(slice(None), (1, 2)), :].groupby("MNUMNR").sum()

    # ''' Moved z[53] to here because it need z[62] calculated first'''
    # T62(53,IR,IY,IS)=T62(62,IR,IY,IS)
    z[53] = z[62]

    # dfd['CGOTGEN'] => [MNUMNR	M10] = (28 10), M10: ?
    # T62(63,IR,IY,IS)=CGOTGEN(IR,IY,1)+CGOTGEN(IR,IY,2)
    z[63] = dfd["CGOTGEN"].loc[(slice(None), (1, 2)), :].groupby("MNUMNR").sum()

    # End-Use Sectors 9/
    #    Coal
    # T62(66,IR,IY,IS)=(CGTOTGENNR(IR,IY,1,1)+CGTOTGENNR(IR,IY,1,2)) * 0.001
    z[66] = dfd["CGTOTGENNR"].loc[:, 1, :, :].groupby("MNUMNR").sum() * 0.001

    #     Petroleum
    # # T62(67,IR,IY,IS) = (CGTOTGENNR(IR,IY,2,1) + CGTOTGENNR(IR,IY,2,2)) * 0.001
    z[67] = (
        dfd["CGTOTGENNR"]
        .loc[(slice(None), 2, range(1, 2 + 1)), :]
        .groupby("MNUMNR")
        .sum()
        * 0.001
    )

    #     Natural Gas----------------------------------
    #     T62(68,IR,IY,IS)=(CGTOTGENNR(IR,IY,4,1)+CGTOTGENNR(IR,IY,4,2)) * 0.001
    z[68] = (
        dfd["CGTOTGENNR"]
        .loc[(slice(None), 4, range(1, 2 + 1)), :]
        .groupby("MNUMNR")
        .sum()
        * 0.001
    )

    #     Other Gaseous Fuels 10/-----------------------
    # T62(69,IR,IY,IS)=(CGTOTGENNR(IR,IY,3,1)+CGTOTGENNR(IR,IY,3,2))*0.001
    z[69] = (
        dfd["CGTOTGENNR"]
        .loc[(slice(None), 3, range(1, 2 + 1)), :]
        .groupby("MNUMNR")
        .sum()
        * 0.001
    )

    # T62(70,IR,IY,IS) = (CGTOTGENNR(IR,IY,5,1) + CGTOTGENNR(IR,IY,5,2)  + &
    # CGTOTGENNR(IR,IY,6,1) + CGTOTGENNR(IR,IY,6,2)  + &
    # CGTOTGENNR(IR,IY,7,1) + CGTOTGENNR(IR,IY,7,2)  + &
    # CGTOTGENNR(IR,IY,8,1) + CGTOTGENNR(IR,IY,8,2)  + &
    # CGTOTGENNR(IR,IY,9,1) + CGTOTGENNR(IR,IY,9,2)  + &
    # (CGTOTGENNR(IR,IY,10,1) + CGTOTGENNR(IR,IY,10,2)) * EUPVGENADJ(IY) + &  !adj PV
    # CGTOTGENNR(IR,IY,12,1) + CGTOTGENNR(IR,IY,12,2) ) * 0.001
    # MNUMNR	NUMCGF	M2 = 28 12 2

    #     Renewable Sources 14/------------------------
    dfd["EUPVGENADJ"] = (
        (
            dfd["CGCOMMGEN"].loc[11, 8].sum()
            + dfd["CGRESGEN"].loc[11, 8].sum()
            + dfd["CGINDLGEN"].loc[11, 8].sum()
        )
        / (dfd["CGTOTGENNR"].loc[MNUMNR, 10].sum())
    ).fillna(1)

    # T62(70,IR,IY,IS) = (CGTOTGENNR(IR,IY,5,1) + CGTOTGENNR(IR,IY,5,2)  + CGTOTGENNR(IR,IY,6,1)
    # + CGTOTGENNR(IR,IY,6,2) + CGTOTGENNR(IR,IY,7,1) + CGTOTGENNR(IR,IY,7,2) + CGTOTGENNR(IR,IY,8,1)
    # + CGTOTGENNR(IR,IY,8,2) + CGTOTGENNR(IR,IY,9,1) + CGTOTGENNR(IR,IY,9,2)
    # + (CGTOTGENNR(IR,IY,10,1) + CGTOTGENNR(IR,IY,10,2)) * EUPVGENADJ(IY)
    # + CGTOTGENNR(IR,IY,12,1) + CGTOTGENNR(IR,IY,12,2) ) * 0.001

    z[70] = (
        dfd["CGTOTGENNR"].loc[:, (5, 6, 7, 8, 9, 12), :, :].groupby("MNUMNR").sum()
        + dfd["CGTOTGENNR"].loc[:, 10, :, :].groupby("MNUMNR").sum() * dfd["EUPVGENADJ"]
    ) * 0.001

    #     Other 10/-------------------------------------
    #     T62(71,IR,IY,IS) = (CGTOTGENNR(IR,IY,11,1) + CGTOTGENNR(IR,IY,11,2))*0.001
    z[71] = (
        dfd["CGTOTGENNR"].loc[(slice(None), 11, (1, 2)), :].groupby("MNUMNR").sum()
        * 0.001
    )

    #       Total---------------------------------------
    # T62(72,IR,IY,IS)=FSUM(T62(66,IR,IY,IS),6)
    z[72] = z[66] + z[67] + z[68] + z[69] + z[70] + z[71]

    #     Sales to the Grid-----------------------------
    # T62(73,IR,IY,IS) = (CGTOTGENNR(IR,IY,1,1) + CGTOTGENNR(IR,IY,2,1) + CGTOTGENNR(IR,IY,3,1) +  &
    # CGTOTGENNR(IR,IY,4,1) + CGTOTGENNR(IR,IY,5,1) + CGTOTGENNR(IR,IY,6,1) + &
    # CGTOTGENNR(IR,IY,7,1) + CGTOTGENNR(IR,IY,8,1) + CGTOTGENNR(IR,IY,9,1) + &
    # (CGTOTGENNR(IR,IY,10,1) * EUPVGENADJ(IY)) + CGTOTGENNR(IR,IY,11,1)+ CGTOTGENNR(IR,IY,12,1)) & !adj pv
    # / 1000.
    z[73] = (
        dfd["CGTOTGENNR"].loc[:, 1:9, 1, :].groupby("MNUMNR").sum()
        + dfd["CGTOTGENNR"].loc[:, 10, 1, :] * dfd["EUPVGENADJ"]
        + dfd["CGTOTGENNR"].loc[:, 11:12, 1, :].groupby("MNUMNR").sum()
    ) * 0.001

    # T62(52,IR,IY,IS)=T62(73,IR,IY,IS)
    z[52] = z[73]

    # T62(54,IR,IY,IS)=T62(48,IR,IY,IS)-T62(49,IR,IY,IS)+T62(50,IR,IY,IS)-T62(51,IR,IY,IS)+T62(52,IR,IY,IS)+T62(53,IR,IY,IS)
    z[54] = z[48] - z[49] + z[50] - z[51] + z[52] + z[53]
    # CGTOTGENNR => (MNUMNR	NUMCGF M2) = (28 12 2)

    #     Generation for Own Use------------------------
    # T62(74,IR,IY,IS) = (CGTOTGENNR(IR,IY,1,2) + CGTOTGENNR(IR,IY,2,2) + CGTOTGENNR(IR,IY,3,2) +  &
    # CGTOTGENNR(IR,IY,4,2) + CGTOTGENNR(IR,IY,5,2) + CGTOTGENNR(IR,IY,6,2) + &
    # CGTOTGENNR(IR,IY,7,2) + CGTOTGENNR(IR,IY,8,2) + CGTOTGENNR(IR,IY,9,2) + &
    # (CGTOTGENNR(IR,IY,10,2) * EUPVGENADJ(IY)) + CGTOTGENNR(IR,IY,11,2)+ CGTOTGENNR(IR,IY,12,2)) &  !adj pv
    # / 1000.
    z[74] = (
        dfd["CGTOTGENNR"]
        .loc[(slice(None), (1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12), 2), :]
        .groupby("MNUMNR")
        .sum()
        * 0.001
        + dfd["CGTOTGENNR"].loc[(slice(None), 10, 2), :].groupby("MNUMNR").sum()
        * 0.001
        * dfd["EUPVGENADJ"]
    )

    # Total Electricity Generation----------------------	T62(101,IR,IY,IS)=T62(61,IR,IY,IS)+T62(72,IR,IY,IS)
    z[101] = z[61] + z[72]

    # --------------------------------------------------
    # End-Use Prices------------------------------------
    # (#### cents per kilowatthour)---------------------
    #   Residential-------------------------------------	T62(75,IR,IY,IS)=PELRSNR(IR,IY)*.1
    #   Commercial--------------------------------------	T62(76,IR,IY,IS)=PELCMNR(IR,IY)*.1
    #   Industrial--------------------------------------	T62(77,IR,IY,IS)=PELINNR(IR,IY)*.1
    #   Transportation----------------------------------	T62(78,IR,IY,IS)=PELTRNR(IR,IY)*.1
    #     All Sectors Average---------------------------	T62(79,IR,IY,IS)=PELASNR(IR,IY)*.1

    # #z[75] = dfd['PELRSNR'] * .1 * SCALPR2
    # # #IF (IR.EQ.mnumnr)
    # df = dfd['PELRS'].loc[MNUMCR] * GWh_to_TRIL * SCALPR2
    # z[75] = pd.DataFrame([df.iloc[0]] * 28, index=range(1, 29))

    # # T62(76,IR,IY,IS)=PELCMNR(IR,IY)*.1
    # #z[76] = dfd['PELCMNR'] * .1 * SCALPR2
    # #  #IF (IR.EQ.mnumnr)
    # df = dfd['PELCM'].loc[MNUMCR] * GWh_to_TRIL * SCALPR2
    # z[76] = pd.DataFrame([df.iloc[0]] * 28, index=range(1, 29))

    # # T62(77,IR,IY,IS)=PELINNR(IR,IY)*.1
    # #z[77] = dfd['PELINNR'] * .1 * SCALPR2
    # #  #IF (IR.EQ.mnumnr)
    # df = dfd['PELIN'].loc[MNUMCR] * GWh_to_TRIL * SCALPR2
    # z[77] = pd.DataFrame([df.iloc[0]] * 28, index=range(1, 29))

    # # T62(78,IR,IY,IS)=PELTRNR(IR,IY)*.1
    # #z[78] = dfd['PELTRNR'] * .1 * SCALPR2
    # #  #IF (IR.EQ.mnumnr)
    # df = dfd['PELTR'].loc[MNUMCR] * GWh_to_TRIL * SCALPR2
    # z[78] = pd.DataFrame([df.iloc[0]] * 28, index=range(1, 29))

    # # T62(79,IR,IY,IS)=PELASNR(IR,IY)*.1
    # #z[79] = dfd['PELASNR'] * .1 * SCALPR2
    # #  #IF (IR.EQ.mnumnr)
    # df = dfd['PELAS'].loc[MNUMCR] * GWh_to_TRIL * SCALPR2
    # z[79] = pd.DataFrame([df.iloc[0]] * 28, index=range(1, 29))

    # (nominal cents per kilowatthour)------------------
    #   Residential-------------------------------------	T62(111:115,IR,IY,IS)=T62(75:79,IR,IY,IS)/SCALPR*MC_JPGDP(IY)
    #   Commercial--------------------------------------	T62(111:115,IR,IY,IS)=T62(75:79,IR,IY,IS)/SCALPR*MC_JPGDP(IY)
    #   Industrial--------------------------------------	T62(111:115,IR,IY,IS)=T62(75:79,IR,IY,IS)/SCALPR*MC_JPGDP(IY)
    #   Transportation----------------------------------	T62(111:115,IR,IY,IS)=T62(75:79,IR,IY,IS)/SCALPR*MC_JPGDP(IY)
    #     All Sectors Average---------------------------	T62(111:115,IR,IY,IS)=T62(75:79,IR,IY,IS)/SCALPR*MC_JPGDP(IY)
    # --------------------------------------------------
    # Note: dfd['SCALPR'] is 1x1 df with column 0 and index 1
    #           0
    #    M1
    #     1	    2.2304275
    #

    z[111] = z[75] / SCALPR2 * MC_JPGDP  # iloc[0]

    z[112] = z[76] / SCALPR2 * MC_JPGDP  # .iloc[0]

    z[113] = z[77] / SCALPR2 * MC_JPGDP  # .iloc[0]

    z[114] = z[78] / SCALPR2 * MC_JPGDP  # .iloc[0]

    z[115] = z[79] / SCALPR2 * MC_JPGDP  # .iloc[0]

    # Prices by Service Category------------------------
    # (#### cents per kilowatthour)---------------------
    #   Generation--------------------------------------	T62(93,IR,IY,IS)=PECGENN(IR,IY)*.1
    #   Transmission------------------------------------	T62(94,IR,IY,IS)=PECTRNN(IR,IY)*.1
    #   Distribution------------------------------------	T62(95,IR,IY,IS)=PECDISN(IR,IY)*.1

    # T62(93,IR,IY,IS)=PECGENN(IR,IY)*.1
    z[93] = dfd["PECGENN"] * 0.1 * SCALPR2

    # T62(94,IR,IY,IS)=PECTRNN(IR,IY)*.1
    z[94] = dfd["PECTRNN"] * 0.1 * SCALPR2

    # T62(95,IR,IY,IS)=PECDISN(IR,IY)*.1
    z[95] = dfd["PECDISN"] * 0.1 * SCALPR2

    # (nominal cents per kilowatthour)------------------
    #   Generation--------------------------------------	T62(116:118,IR,IY,IS)=T62(93:95,IR,IY,IS)/SCALPR*MC_JPGDP(IY)
    #   Transmission------------------------------------	T62(116:118,IR,IY,IS)=T62(93:95,IR,IY,IS)/SCALPR*MC_JPGDP(IY)
    #   Distribution------------------------------------	T62(116:118,IR,IY,IS)=T62(93:95,IR,IY,IS)/SCALPR*MC_JPGDP(IY)
    # T62(116:118,IR,IY,IS)=T62(93:95,IR,IY,IS)/SCALPR*MC_JPGDP(IY)
    # See z[111]
    z[116] = z[93] / SCALPR2 * MC_JPGDP.values

    z[117] = z[94] / SCALPR2 * MC_JPGDP.values

    z[118] = z[95] / SCALPR2 * MC_JPGDP.values

    # Fuel Consumption (quadrillion Btu) 2/-------------
    #   Coal--------------------------------------------	T62(85,IR,IY,IS)=(UFLCLNR(1,IR,IY)+UFLCLNR(2,IR,IY))*0.001
    #   Natural Gas-------------------------------------	T62(86,IR,IY,IS)=(UFLGFNR(1,IR,IY)+UFLGINR(1,IR,IY)+UFLGCNR(1,IR,IY)+UFLGFNR(2,IR,IY)+UFLGINR(2,IR,IY)+UFLGCNR(2,IR,IY)+UFLDGNR(1,IR,IY)+UFLDGNR(2,IR,IY))*0.001
    #   Oil---------------------------------------------	T62(87,IR,IY,IS)=(UFLDSNR(1,IR,IY)+UFLRLNR(1,IR,IY)+UFLRHNR(1,IR,IY)+UFLDSNR(2,IR,IY)+UFLRLNR(2,IR,IY)+UFLRHNR(2,IR,IY)+UFLDDNR(1,IR,IY)+UFLDDNR(2,IR,IY))*0.001
    #     Total-----------------------------------------	T62(88,IR,IY,IS)=FSUM(T62(85,IR,IY,IS),3)
    # T62(85,IR,IY,IS)=(UFLCLNR(1,IR,IY)+UFLCLNR(2,IR,IY)) * 0.001
    z[85] = (dfd["UFLCLNR"].loc[1] + dfd["UFLCLNR"].loc[2]) * 0.001

    # T62(86,IR,IY,IS)=(UFLGFNR(1,IR,IY)+UFLGINR(1,IR,IY)+UFLGCNR(1,IR,IY)
    # +UFLGFNR(2,IR,IY)+UFLGINR(2,IR,IY)+UFLGCNR(2,IR,IY)
    # +UFLDGNR(1,IR,IY)+UFLDGNR(2,IR,IY)) * 0.001
    z[86] = (
        dfd["UFLGFNR"].loc[1]
        + dfd["UFLGINR"].loc[1]
        + dfd["UFLGCNR"].loc[1]
        + dfd["UFLGFNR"].loc[2]
        + dfd["UFLGINR"].loc[2]
        + dfd["UFLGCNR"].loc[2]
        + dfd["UFLDGNR"].loc[1]
        + dfd["UFLDGNR"].loc[2]
    ) * 0.001

    # T62(87,IR,IY,IS)=(UFLDSNR(1,IR,IY)+UFLRLNR(1,IR,IY)+UFLRHNR(1,IR,IY)
    # +UFLDSNR(2,IR,IY)+UFLRLNR(2,IR,IY)+UFLRHNR(2,IR,IY)
    # +UFLDDNR(1,IR,IY)+UFLDDNR(2,IR,IY)) * 0.001
    z[87] = (
        dfd["UFLDSNR"].loc[1]
        + dfd["UFLRLNR"].loc[1]
        + dfd["UFLRHNR"].loc[1]
        + dfd["UFLDSNR"].loc[2]
        + dfd["UFLRLNR"].loc[2]
        + dfd["UFLRHNR"].loc[2]
        + dfd["UFLDDNR"].loc[1]
        + dfd["UFLDDNR"].loc[2]
    ) * 0.001

    # T62(88,IR,IY,IS)=FSUM(T62(85,IR,IY,IS),3)
    z[88] = z[85] + z[86] + z[87]

    # Fuel Prices to the Electric Power Sector 2/-------
    # (#### dollars per million Btu)--------------------
    #   Coal--------------------------------------------	T62(80,IR,IY,IS)=UPCOALAVG(IR,IY)
    #   Natural Gas-------------------------------------	T62(81,IR,IY,IS)=UPGASPRC(IR,IY)
    #   Distillate Fuel Oil-----------------------------	T62(82,IR,IY,IS)=UPDISPRC(IR,IY)
    #   Residual Fuel Oil-------------------------------	T62(83,IR,IY,IS)=UPRESPRC(IR,IY)
    #   Biomass-----------------------------------------	T62(84,IR,IY,IS)=UPRWDNR(IR,IY)

    # # T62(80,IR,IY,IS)=UPCOALAVG(IR,IY)
    # #z[80] = dfd['UPCOALAVG'] * SCALPR2
    # #  #IF (IR.EQ.mnumnr)
    # z[80] = dfd['PCLEL'].loc[MNUMCR] * SCALPR2

    # # T62(81,IR,IY,IS)=UPGASPRC(IR,IY)
    # #z[81] = dfd['UPGASPRC'] * SCALPR2
    # #  #IF (IR.EQ.mnumnr)
    # z[81] = dfd['PNGEL'].loc[MNUMCR] * SCALPR2

    # # T62(82,IR,IY,IS)=UPDISPRC(IR,IY)
    # #z[82] = dfd['UPDISPRC'] * SCALPR2
    # #  #IF (IR.EQ.mnumnr)
    # z[82] = dfd['PDSEL'].loc[MNUMCR] * SCALPR2

    # # T62(83,IR,IY,IS)=UPRESPRC(IR,IY)
    # #z[83] = dfd['UPRESPRC'] * SCALPR2
    # #  #IF (IR.EQ.mnumnr)
    # z[83] = dfd['PRSEL'].loc[MNUMCR] * SCALPR2

    # T62(84,IR,IY,IS)=UPRWDNR(IR,IY)
    z[84] = dfd["UPRWDNR"] * SCALPR2

    # (nominal dollars per million Btu)-----------------
    #   Coal--------------------------------------------	T62(119:123,IR,IY,IS)=T62(80:84,IR,IY,IS)/SCALPR*MC_JPGDP(IY)
    #   Natural Gas-------------------------------------	T62(119:123,IR,IY,IS)=T62(80:84,IR,IY,IS)/SCALPR*MC_JPGDP(IY)
    #   Distillate Fuel Oil-----------------------------	T62(119:123,IR,IY,IS)=T62(80:84,IR,IY,IS)/SCALPR*MC_JPGDP(IY)
    #   Residual Fuel Oil-------------------------------	T62(119:123,IR,IY,IS)=T62(80:84,IR,IY,IS)/SCALPR*MC_JPGDP(IY)
    #   Biomass-----------------------------------------	T62(119:123,IR,IY,IS)=T62(80:84,IR,IY,IS)/SCALPR*MC_JPGDP(IY)
    # T62(119:123,IR,IY,IS)=T62(80:84,IR,IY,IS)/SCALPR*MC_JPGDP(IY)

    z[119] = z[80] / SCALPR2 * MC_JPGDP.values

    z[120] = z[81] / SCALPR2 * MC_JPGDP.values

    z[121] = z[82] / SCALPR2 * MC_JPGDP.values

    z[122] = z[83] / SCALPR2 * MC_JPGDP.values

    z[123] = z[84] / SCALPR2 * MC_JPGDP.values

    # Emissions from the Electric Power Sector 2/-------
    #   Total Carbon (million short tons)---------------	T62(89,IR,IY,IS)=(UCARINR(IR,IY)+UCAROTR(IR,IY))*EM_ELEC(9,11,ICY)/(UCO2INR(MNUMNR,IY)+UCO2OTR(MNUMNR,IY))*2204./2000.
    #   Carbon Dioxide (million short tons)-------------	T62(90,IR,IY,IS)=(UCO2INR(IR,IY)+UCO2OTR(IR,IY))*EM_ELEC(9,11,ICY)/(UCO2INR(MNUMNR,IY)+UCO2OTR(MNUMNR,IY))*2204./2000.
    #   Sulfur Dioxide (million short tons)-------------	T62(91,IR,IY,IS)=SO2COAL*(USO2INR(IR,IY)+USO2OTR(IR,IY))/(USO2INR(MNUMNR,IY)+USO2OTR(MNUMNR,IY))+CTLSO2*CTLGEN(IR,IY)
    #   Nitrogen Oxide (million short tons)-------------	T62(92,IR,IY,IS)=UNOXOTR(IR,IY)+UNOXINR(IR,IY)+CTLNOX*CTLGEN(IR,IY)
    #   Mercury (short tons)----------------------------	T62(100,IR,IY,IS)=(TOT_MERC(IY)/2000.0)*(UHGINR(IR,IY)+UHGOTR(IR,IY))/(UHGINR(MNUMNR,IY)+UHGOTR(MNUMNR,IY))+CTLHG*CTLGEN(IR,IY)

    # T62(89,IR,IY,IS)=(UCARINR(IR,IY)+UCAROTR(IR,IY))*EM_ELEC(9,11,ICY)
    # /(UCO2INR(MNUMNR,IY)+UCO2OTR(MNUMNR,IY))*2204./2000.   dfd['C_CO2_FACTOR_rwpre']=3.66667
    z[89] = (
        (dfd["UCARINR"] + dfd["UCAROTR"])
        * dfd["EM_ELEC"].loc[9].loc[11]
        / (dfd["UCO2INR"].loc[MNUMNR] + dfd["UCO2OTR"].loc[MNUMNR])
        * dfd["T62_PARAM_2_rwpre"]
        / dfd["LBS_per_TON_rwpre"]
        * dfd["C_CO2_FACTOR_rwpre"]
        * 0.001
    )

    # T62(90,IR,IY,IS)=(UCO2INR(IR,IY)+UCO2OTR(IR,IY))*EM_ELEC(9,11,ICY)
    # /(UCO2INR(MNUMNR,IY)+UCO2OTR(MNUMNR,IY))*2204./2000.
    # dfd['T62_PARAM_2_rwpre'] converts kg to pounds 1kg = 2.204 lbs todo: change the name, also used in Table 109
    z[90] = (
        (dfd["UCO2INR"] + dfd["UCO2OTR"])
        * dfd["EM_ELEC"].loc[9].loc[11]
        / (dfd["UCO2INR"].loc[MNUMNR] + dfd["UCO2OTR"].loc[MNUMNR])
        * dfd["T62_PARAM_2_rwpre"]
        / dfd["LBS_per_TON_rwpre"]
        * dfd["C_CO2_FACTOR_rwpre"]
        * 0.001
    )

    #     so2coal = 0.0
    # DO III = 1, NUTSEC
    # so2coal = so2coal + UTTSO2(III,IY)
    # ENDDO
    # DO ISO2 = 1 , NUM_SO2_GRP
    #     so2coal = so2coal + so2other(IY,ISO2) * 0.000001

    SO2COAL = dfd["UTTSO2"].sum() + dfd["SO2OTHER"].sum() * 0.000001

    # T62(91,IR,IY,IS)=SO2COAL*(USO2INR(IR,IY)+USO2OTR(IR,IY))/(USO2INR(MNUMNR,IY)
    # +USO2OTR(MNUMNR,IY))+CTLSO2*CTLGEN(IR,IY)
    z[91] = (
        SO2COAL
        * (dfd["USO2INR"] + dfd["USO2OTR"])
        / (dfd["USO2INR"].loc[MNUMNR] + dfd["USO2OTR"].loc[MNUMNR])
        + CTLSO2 * dfd["CTLGEN"]
    )

    # T62(92,IR,IY,IS)=UNOXOTR(IR,IY)+UNOXINR(IR,IY)+CTLNOX*CTLGEN(IR,IY)
    z[92] = dfd["UNOXOTR"] + dfd["UNOXINR"] + CTLNOX * dfd["CTLGEN"]

    # T62(100,IR,IY,IS)=(TOT_MERC(IY)/2000.0)*(UHGINR(IR,IY)
    # +UHGOTR(IR,IY))/(UHGINR(MNUMNR,IY)
    # +UHGOTR(MNUMNR,IY))+CTLHG*CTLGEN(IR,IY)
    # Note: dfd['TOT_MERC'] has one row with index 0. Need convert to a series
    z[100] = (dfd["TOT_MERC"].values / 2000.0) * (dfd["UHGINR"] + dfd["UHGOTR"]) / (
        dfd["UHGINR"].loc[MNUMNR] + dfd["UHGOTR"].loc[MNUMNR]
    ) + CTLHG * dfd["CTLGEN"]

    return z
