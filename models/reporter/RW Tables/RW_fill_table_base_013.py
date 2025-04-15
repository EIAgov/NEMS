# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
Fixed by SZO on 7/8/20124
Fixed multi-block issues on 7/23/2024
"""


def fill_table_base_013(dfd, table_spec, table_id):
    """Fill table  Natural Gas Supply, Disposition, and Prices

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

    FYRPRC = int(table_spec["settings"]["year_index_real_dol_priceyr"])
    # Get inflation index from 1990->2050
    MC_JPGDP = dfd["MC_JPGDP"].loc[1990:]

    # Get inflation index of FYRPRC (2022)
    SCALPR2 = MC_JPGDP.loc[FYRPRC]  # 2022 = column 33

    MNUMPR = dfd["MNUMPR_rwpre"]
    MNUMCR = dfd["MNUMCR_rwpre"]
    MNUMOR = dfd["MNUMOR_rwpre"]
    TRIL_TO_QUAD = dfd["TRIL_TO_QUAD_rwpre"]

    #   Natural Gas Supply, Disposition, and Prices
    #   (trillion cubic feet, unless otherwise noted)
    #    Supply, Disposition, and Prices

    #    Production

    #      Dry Gas Production 1/
    # T13(1,IY,IS)=(OGQNGREP(1,IY)+OGQNGREP(2,IY)+OGQNGREP(3,IY)+OGQNGREP(4,IY)+OGQNGREP(5,IY)+OGQNGREP(6,IY)+OGQNGREP(7,IY)+OGQNGREP(8,IY)+OGSHALENG(IY))*.001
    z[1] = (
        dfd["OGQNGREP"].loc[1]
        + dfd["OGQNGREP"].loc[2]
        + dfd["OGQNGREP"].loc[3]
        + dfd["OGQNGREP"].loc[4]
        + dfd["OGQNGREP"].loc[5]
        + dfd["OGQNGREP"].loc[6]
        + dfd["OGQNGREP"].loc[7]
        + dfd["OGQNGREP"].loc[8]
        + dfd["OGSHALENG"]
    ) * 0.001

    #      Supplemental Natural Gas 2/
    # T13(2,IY,IS)=OGPRSUP(IY)*.001
    z[2] = dfd["OGPRSUP"] * 0.001

    #      Renewable Natural Gas

    z[57] = dfd["OGPRRNG"] * 0.001

    #

    #    Net Imports
    # T13(3,IY,IS)=(NGIMPVOL(4,IY)-NGEXPVOL(4,IY))*.001
    z[3] = (dfd["NGIMPVOL"].loc[4] - dfd["NGEXPVOL"].loc[4]) * 0.001

    #      Pipeline 3/
    # T13(33,IY,IS)=(NGIMPVOL(1,IY)-NGEXPVOL(1,IY))*.001+(NGIMPVOL(2,IY)-NGEXPVOL(2,IY))*.001
    z[33] = (dfd["NGIMPVOL"].loc[1] - dfd["NGEXPVOL"].loc[1]) * 0.001 + (
        dfd["NGIMPVOL"].loc[2] - dfd["NGEXPVOL"].loc[2]
    ) * 0.001

    #      Liquefied Natural Gas
    # T13(6,IY,IS)=(NGIMPVOL(3,IY)-NGEXPVOL(3,IY))*.001
    z[6] = (dfd["NGIMPVOL"].loc[3] - dfd["NGEXPVOL"].loc[3]) * 0.001

    #

    #    Total Supply
    # T13(7,IY,IS)=FSUM(T13(1,IY,IS),3)
    z[7] = z[1] + z[2] + z[3]

    #

    #    Consumption by Sector

    #      Residential
    # T13(8,IY,IS)=QNGRS(11,IY)/CFNGN(IY)
    z[8] = dfd["QNGRS"].loc[11] / dfd["CFNGN"] * TRIL_TO_QUAD

    #      Commercial
    # T13(9,IY,IS)=QNGCM(11,IY)/CFNGN(IY)
    z[9] = dfd["QNGCM"].loc[11] / dfd["CFNGN"] * TRIL_TO_QUAD

    #      Industrial 4/

    #        Lease and Plant Fuel 5/
    # T13(12,IY,IS)=QLPIN(11,IY)/CFNGC(IY)
    z[12] = dfd["QLPIN"].loc[11] / dfd["CFNGC"] * TRIL_TO_QUAD

    #        Hydrogen Production Natural Gas Feedstock 8/
    z[37] = (dfd["QNGHMPF"].loc[11] / dfd["CFNGN"]) / 1000
    
    #        Other Industrial 4/
    # T13(10,IY,IS)=(QNGIN(11,IY))/CFNGN(IY)
    z[10] = (
        (dfd["QNGIN"].loc[11]) / dfd["CFNGN"]
    ) * TRIL_TO_QUAD -z[37]

    # T13(35,IY,IS)=T13(10,IY,IS)+T13(12,IY,IS)+T13(37,IY,IS)
    z[35] = z[10] + z[12] + z[37]
    #        Natural Gas to Hydrogen

    #      Transportation

    #        Motor Vehicles, Trains, and Ships
    # T13(14,IY,IS)=QNGTR(11,IY)/CFNGN(IY)
    z[14] = dfd["QNGTR"].loc[11] / dfd["CFNGN"] * TRIL_TO_QUAD

    #        Pipeline and Distribution Fuel
    # T13(13,IY,IS)=QGPTR(11,IY)/CFNGC(IY)
    z[13] = dfd["QGPTR"].loc[11] / dfd["CFNGC"] * TRIL_TO_QUAD

    #        Fuel Used to Liquefy Gas for Export 8/
    # T13(56,IY,IS)=QNGLQ(11,IY)/CFNGC(IY)
    z[56] = dfd["QNGLQ"].loc[11] / dfd["CFNGC"] * TRIL_TO_QUAD
    # T13(36,IY,IS)=T13(13,IY,IS)+T13(14,IY,IS)+T13(56,IY,IS)
    z[36] = z[13] + z[14] + z[56]

    #      Electric Power 9/
    # T13(11,IY,IS)=QNGEL(11,IY)/CFNGU(IY)
    z[11] = dfd["QNGEL"].loc[11] / dfd["CFNGU"] * TRIL_TO_QUAD
    # T13(15,IY,IS)=FSUM(T13(8,IY,IS),7)+T13(37,IY,IS)+T13(56,IY,IS)
    z[15] = z[8] + z[9] + z[10] + z[11] + z[12] + z[13] + z[14] + z[37] + z[56]
    #

    #    Discrepancy 10/
    # T13(16,IY,IS)=T13(7,IY,IS)-T13(15,IY,IS)
    z[16] = z[7] - z[15]

    #

    #   Natural Gas Prices

    #

    #     Natural Gas Spot Price at Henry Hub

    #     (#### dollars per million Btu)
    # T13(34,IY,IS)=OGHHPRNG(IY)
    z[34] = dfd["OGHHPRNG"] * SCALPR2

    #

    #     Delivered Prices

    #     (#### dollars per thousand cubic feet)

    #        Residential
    # T13(21,IY,IS)=PNGRS(11,IY)*CFNGN(IY)
    z[21] = dfd["AMPBLK/PNGRS"].loc[11] * dfd["CFNGN"] * SCALPR2

    #        Commercial
    # T13(22,IY,IS)=PNGCM(11,IY)*CFNGN(IY)
    z[22] = dfd["AMPBLK/PNGCM"].loc[11] * dfd["CFNGN"] * SCALPR2

    #        Industrial 11/
    # T13(23,IY,IS)=PNGIN(11,IY)*CFNGN(IY)
    z[23] = dfd["AMPBLK/PNGIN"].loc[11] * dfd["CFNGN"] * SCALPR2

    #        Transportation 12/
    # T13(25,IY,IS)=PNGTR(11,IY)*CFNGN(IY)
    # 2050 restart file = 5.142001*1.039*2.2201138 = 11.86
    z[25] = dfd["AMPBLK/PNGTR"].loc[11] * dfd["CFNGN"] * SCALPR2

    #        Electric Power 9/
    # T13(24,IY,IS)=PNGEL(11,IY)*CFNGU(IY)
    z[24] = dfd["AMPBLK/PNGEL"].loc[11] * dfd["CFNGU"] * SCALPR2

    #           Average 13/
    # T13(26,IY,IS)=PNGAS(11,IY)*CFNGC(IY)
    z[26] = dfd["AMPBLK/PNGAS"].loc[11] * dfd["CFNGC"] * SCALPR2

    #

    #

    #

    #   --------------------------------------------

    #   Bonus Rows - Internal Only

    #   --------------------------------------------

    #

    #    Average Import Price
    # T13(19,IY,IS)=NGIMPPRC(4,IY)
    z[19] = dfd["NGIMPPRC"].loc[4] * SCALPR2

    #    Average Source Price
    # T13(20,IY,IS)=(NGIMPPRC(4,IY)*NGIMPVOL(4,IY)+OGWPRNG(MNUMOR,IY)*(OGQNGREP(1,IY)+OGQNGREP(2,IY)+OGQNGREP(3,IY)+OGQNGREP(4,IY)+OGQNGREP(5,IY)+OGQNGREP(6,IY)+OGQNGREP(7,IY)+OGQNGREP(8,IY)+OGSHALENG(IY)))/(NGIMPVOL(4,IY)+(OGQNGREP(1,IY)+OGQNGREP(2,IY)+OGQNGREP(3,IY)+OGQNGREP(4,IY)+OGQNGREP(5,IY)+OGQNGREP(6,IY)+OGQNGREP(7,IY)+OGQNGREP(8,IY)+OGSHALENG(IY)))
    z[20] = (
        (
            dfd["NGIMPPRC"].loc[4] * dfd["NGIMPVOL"].loc[4]
            + dfd["OGWPRNG"].loc[MNUMOR]
            * (
                dfd["OGQNGREP"].loc[1]
                + dfd["OGQNGREP"].loc[2]
                + dfd["OGQNGREP"].loc[3]
                + dfd["OGQNGREP"].loc[4]
                + dfd["OGQNGREP"].loc[5]
                + dfd["OGQNGREP"].loc[6]
                + dfd["OGQNGREP"].loc[7]
                + dfd["OGQNGREP"].loc[8]
                + dfd["OGSHALENG"]
            )
        )
        / (
            dfd["NGIMPVOL"].loc[4]
            + (
                dfd["OGQNGREP"].loc[1]
                + dfd["OGQNGREP"].loc[2]
                + dfd["OGQNGREP"].loc[3]
                + dfd["OGQNGREP"].loc[4]
                + dfd["OGQNGREP"].loc[5]
                + dfd["OGQNGREP"].loc[6]
                + dfd["OGQNGREP"].loc[7]
                + dfd["OGQNGREP"].loc[8]
                + dfd["OGSHALENG"]
            )
        )
        * SCALPR2
    )

    # ! Transmission and distribution margins by sector
    # T13(27,IY,IS) = PNGRS(11,IY)*CFNGN(IY) - T13(20,IY,IS)
    # T13(28,IY,IS) = PNGCM(11,IY)*CFNGN(IY) - T13(20,IY,IS)
    # T13(29,IY,IS) = PNGIN(11,IY)*CFNGN(IY) - T13(20,IY,IS)
    # T13(30,IY,IS) = PNGEL(11,IY)*CFNGU(IY) - T13(20,IY,IS)
    # T13(31,IY,IS) = PNGTR(11,IY)*CFNGN(IY) - T13(20,IY,IS)
    # T13(32,IY,IS) = PNGAS(11,IY)*CFNGC(IY) - T13(20,IY,IS)

    #       Residential
    z[27] = dfd["MPBLK/PNGRS"].loc[MNUMCR] * dfd["CFNGN"] * SCALPR2 - z[20]
    #       Commercial
    z[28] = dfd["MPBLK/PNGCM"].loc[MNUMCR] * dfd["CFNGN"] * SCALPR2 - z[20]
    #       Industrial
    z[29] = dfd["MPBLK/PNGIN"].loc[MNUMCR] * dfd["CFNGN"] * SCALPR2 - z[20]
    #       Electric Power
    z[30] = dfd["MPBLK/PNGEL"].loc[MNUMCR] * dfd["CFNGU"] * SCALPR2 - z[20]
    #       Transportation
    z[31] = dfd["MPBLK/PNGTR"].loc[MNUMCR] * dfd["CFNGN"] * SCALPR2 - z[20]
    #          Average
    z[32] = dfd["MPBLK/PNGAS"].loc[MNUMCR] * dfd["CFNGC"] * SCALPR2 - z[20]

    #

    #   Natural Gas Prices Again in Nominal Dollars

    #     Natural Gas Spot Price at Henry Hub

    #     (nominal dollars per million Btu)
    # T13(39,IY,IS)=T13(34,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[39] = z[34] / SCALPR2 * MC_JPGDP

    #

    #     Average Lower 48 Wellhead Price

    #     (#### dollars per thousand cubic feet)
    # T13(18,IY,IS)=OGWPRNG(MNUMOR,IY)
    z[18] = dfd["OGWPRNG"].loc[MNUMOR] * SCALPR2

    #     (nominal dollars per thousand cubic feet)
    # T13(41,IY,IS)=T13(18,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[41] = z[18] / SCALPR2 * MC_JPGDP

    #

    #     Delivered Prices

    #     (nominal dollars per thousand cubic feet)

    #        Residential
    # T13(42,IY,IS)=T13(21,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[42] = z[21] / SCALPR2 * MC_JPGDP

    #        Commercial
    # T13(43,IY,IS)=T13(22,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[43] = z[22] / SCALPR2 * MC_JPGDP

    #        Industrial 11/
    # T13(44,IY,IS)=T13(23,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[44] = z[23] / SCALPR2 * MC_JPGDP

    #        Transportation 12/
    # T13(46,IY,IS)=T13(25,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[46] = z[25] / SCALPR2 * MC_JPGDP

    #        Electric Power 9/
    # T13(45,IY,IS)=T13(24,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[45] = z[24] / SCALPR2 * MC_JPGDP

    #           Average 13/
    # T13(47,IY,IS)=T13(26,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[47] = z[26] / SCALPR2 * MC_JPGDP

    #

    #    Average Import Price
    # T13(48,IY,IS)=T13(19,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[48] = z[19] / SCALPR2 * MC_JPGDP

    #    Average Source Price
    # T13(49,IY,IS)=T13(20,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[49] = z[20] / SCALPR2 * MC_JPGDP

    #

    #    Transmission & Distribution Margins

    # ! Transmission and distribution margins by sector
    # T13(50,IY,IS) =(PNGRS(11,IY)*CFNGN(IY) - T13(20,IY,IS)) / SCALPR * MC_JPGDP(IY)
    # T13(51,IY,IS) =(PNGCM(11,IY)*CFNGN(IY) - T13(20,IY,IS)) / SCALPR * MC_JPGDP(IY)
    # T13(52,IY,IS) =(PNGIN(11,IY)*CFNGN(IY) - T13(20,IY,IS)) / SCALPR * MC_JPGDP(IY)
    # T13(53,IY,IS) =(PNGEL(11,IY)*CFNGU(IY) - T13(20,IY,IS)) / SCALPR * MC_JPGDP(IY)
    # T13(54,IY,IS) =(PNGTR(11,IY)*CFNGN(IY) - T13(20,IY,IS)) / SCALPR * MC_JPGDP(IY)
    # T13(55,IY,IS) =(PNGAS(11,IY)*CFNGC(IY) - T13(20,IY,IS)) / SCALPR * MC_JPGDP(IY)

    #       Residential
    # T13(50,IY,IS) = T13(42,IY,IS) - T13(49,IY,IS)
    z[50] = dfd["MPBLK/PNGRS"].loc[MNUMCR] * dfd["CFNGN"] * MC_JPGDP - z[49]

    #       Commercial
    # T13(51,IY,IS)=T13(43,IY,IS) - T13(49,IY,IS)
    z[51] = dfd["MPBLK/PNGCM"].loc[MNUMCR] * dfd["CFNGN"] * MC_JPGDP - z[49]

    #       Industrial
    # T13(52,IY,IS)=T13(44,IY,IS) - T13(49,IY,IS)
    z[52] = dfd["MPBLK/PNGIN"].loc[MNUMCR] * dfd["CFNGN"] * MC_JPGDP - z[49]

    #       Transportation
    # T13(54,IY,IS)=T13(46,IY,IS) - T13(49,IY,IS)
    z[54] = dfd["MPBLK/PNGTR"].loc[MNUMCR] * dfd["CFNGN"] * MC_JPGDP - z[49]

    #       Electric Power
    # T13(53,IY,IS)=T13(45,IY,IS) - T13(49,IY,IS)
    z[53] = dfd["MPBLK/PNGEL"].loc[MNUMCR] * dfd["CFNGU"] * MC_JPGDP - z[49]

    #          Average
    # T13(55,IY,IS)=T13(47,IY,IS) - T13(49,IY,IS)
    z[55] = dfd["MPBLK/PNGAS"].loc[MNUMCR] * dfd["CFNGC"] * MC_JPGDP - z[49]

    return z
