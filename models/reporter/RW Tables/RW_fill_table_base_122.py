# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM

SZO
last updated 7/12/2024, fixed:
   Residential CHP
   Commercial CHP
   Industrial CHP
   Refinery CHP
     Annual Credit Bank Activity
 Electricity Sales (billion kwh)
     Credits Achieved With Bank
 Credits Achieved With Bank
   Credit Cap
   Cumulative
"""
import numpy as np


def fill_table_base_122(dfd, table_spec, table_id):
    """Fill table National Impacts of Renewable or Carbon Portfolio Standards

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

    year_cum_cap_addition_begin = int(
        table_spec["settings"]["year_cum_cap_addition_begin"]
    )
    FYRPRC = int(table_spec["settings"]["year_index_real_dol_priceyr"])
    # Get inflation index from 1990->2050
    MC_JPGDP = dfd["MC_JPGDP"].loc[1990:]

    # Get inflation index of FYRPRC (2022)
    SCALPR2 = MC_JPGDP.loc[FYRPRC]  # 2022 = column 33

    MNUMNR = int(dfd["MNUMNR_rwpre"])
    MNUMCR = int(dfd["MNUMCR_rwpre"])
    GWh_to_TRIL = dfd["GWh_to_TRIL_rwpre"]

    #   National Impacts of Renewable or Carbon Portfolio Standards
    #
    #   Renewable or Carbon Portfolio Standard
    #
    #    RPS/CPS Credits (billion kwh)

    #      Coal (Sequestration)
    # T122(1,IR,IY,IS)=UCRCLNR(2,IR,IY)
    z[1] = dfd["UCRCLNR"].loc[2]

    #      Natural Gas (Sequestration)
    # T122(2,IR,IY,IS)=UCRNGNR(2,IR,IY)
    z[2] = dfd["UCRNGNR"].loc[2]

    #      Nuclear
    # T122(3,IR,IY,IS)=UCRNUNR(2,IR,IY)
    z[3] = dfd["UCRNUNR"].loc[2]

    #      Conventional Hydroelectric Power
    # T122(4,IR,IY,IS)=UCRHYNR(2,IR,IY)
    z[4] = dfd["UCRHYNR"].loc[2]

    #      Geothermal
    # T122(5,IR,IY,IS)=UCRGENR(2,IR,IY)
    z[5] = dfd["UCRGENR"].loc[2]

    #      Municipal Waste
    # T122(6,IR,IY,IS)=UCRMSNR(2,IR,IY)
    z[6] = dfd["UCRMSNR"].loc[2]

    #      Wood and Other Biomass
    # T122(7,IR,IY,IS)=UCRWDNR(2,IR,IY)+UCRCFNR(2,IR,IY)
    z[7] = dfd["UCRWDNR"].loc[2] + dfd["UCRCFNR"].loc[2]

    #         Dedicated Plants
    # T122(8,IR,IY,IS)=UCRWDNR(2,IR,IY)
    z[8] = dfd["UCRWDNR"].loc[2]

    #         Cofiring
    # T122(9,IR,IY,IS)=UCRCFNR(2,IR,IY)
    z[9] = dfd["UCRCFNR"].loc[2]

    #      Solar Thermal
    # T122(10,IR,IY,IS)=UCRSONR(2,IR,IY)
    z[10] = dfd["UCRSONR"].loc[2]

    #      Solar Photovoltaic
    # T122(11,IR,IY,IS)=UCRPVNR(2,IR,IY)+UCRPTNR(2,IR,IY)
    z[11] = dfd["UCRPVNR"].loc[2] + dfd["UCRPTNR"].loc[2]

    #      Wind
    # T122(12,IR,IY,IS)=UCRWNNR(2,IR,IY)+UCRWLNR(2,IR,IY)+UCRWFNR(2,IR,IY)
    z[12] = dfd["UCRWNNR"].loc[2] + dfd["UCRWLNR"].loc[2] + dfd["UCRWFNR"].loc[2]
    #        Annual Credit Bank Activity
    # T122(44,IR,IY,IS)=UCRBKNR(IR,IY)-UCRBKNR(IR,IY-1)
    # Calculate the difference between each column and the previous column
    z[44] = dfd["UCRBKNR"].diff(axis=1)

    #    Baseline Electricity Sales (billion kwh)
    # T122(47,IR,IY,IS)=UCRBSNR(IR,IY)
    z[47] = dfd["UCRBSNR"]

    #      Residential CHP   (no code in ftab)
    z[52] = z[47] * 0.0

    #      Commercial CHP
    z[53] = z[47] * 0.0

    #      Industrial CHP
    z[54] = z[47] * 0.0

    #      Refinery CHP
    z[55] = z[47] * 0.0

    #        Total Earned
    # T122(13,IR,IY,IS)=UCRTLNR(2,IR,IY)
    z[13] = dfd["UCRTLNR"].loc[2]

    #        Total Penalty
    # T122(14,IR,IY,IS)=UCRPNNR(IR,IY)
    z[14] = dfd["UCRPNNR"]

    #        Cumulative Credit Bank Balance
    # T122(45,IR,IY,IS)=UCRBKNR(IR,IY)
    z[45] = dfd["UCRBKNR"]

    #

    #    Electricity Sales (billion kwh)
    # T122(15,IR,IY,IS)=(QELAS(MNUMCR,IY)/0.003412)-(QELASN(MNUMNR-2,IY)+QELASN(MNUMNR-1,IY))*0.001
    # ! Electricity Sales (48 States)
    #         if (ir .eq. mnumnr) then
    #             T122(15,IR,IY,IS) = (QELAS(MNUMCR,IY) / 0.003412) - (QELASN(MNUMNR - 2,IY) + QELASN(MNUMNR - 1,IY)) * 0.001
    #         else
    #         T122(15,IR,IY,IS) = QELASN(IR,IY) * 0.001
    #         endif
    z[15] = dfd["QELASN"] * 0.001
    z[15].loc[MNUMNR] = (dfd["QELAS"].loc[MNUMCR] / GWh_to_TRIL) - (
        dfd["QELASN"].loc[MNUMNR - 2] + dfd["QELASN"].loc[MNUMNR - 1]
    ) * 0.001

    #    RPS/CES Levels

    #      (percent of total sales)

    #        Credits Required
    # T122(16,IR,IY,IS)=EPRPSLM(IY)*100.0*T122(47,IR,IY,IS)/T122(15,IR,IY,IS)
    z[16] = dfd["EPRPSLM"].loc[z[47].columns] * 100.0 * z[47] / z[15].replace(0, np.inf)

    #        Credits Achieved
    # T122(17,IR,IY,IS)=(UCRTLNR(2,IR,IY)/T122(15,IR,IY,IS))*100.0
    z[17] = (dfd["UCRTLNR"].loc[2] / z[15]) * 100.0

    #        Credits Achieved With Bank
    # T122(46,IR,IY,IS)=((UCRTLNR(2,IR,IY)-T122(44,IR,IY,IS))/T122(15,IR,IY,IS))*100.0
    z[46] = ((dfd["UCRTLNR"].loc[2] - z[44]) / z[15]) * 100.0

    #      (percent of baseline sales)

    #        Credits Required
    # T122(48,IR,IY,IS)=EPRPSLM(IY)*100.0
    lastyr = table_spec["settings"]["last_print_year"]
    z[48] = (dfd["EPRPSLM"] * 100.0).loc[:lastyr]

    #        Credits Achieved
    # T122(49,IR,IY,IS)=(UCRTLNR(2,IR,IY)/T122(47,IR,IY,IS))*100.0
    z[49] = (dfd["UCRTLNR"].loc[2] / z[47]) * 100.0

    #        Credits Achieved With Bank
    # T122(50,IR,IY,IS)=((UCRTLNR(2,IR,IY)-T122(44,IR,IY,IS))/T122(47,IR,IY,IS))*100.0
    z[50] = ((dfd["UCRTLNR"].loc[2] - z[44]) / z[47]) * 100.0

    #    RPS/CPS Credit Prices
    #    (#### mills per kilowatthour)

    #      Credit Price
    # T122(19,IR,IY,IS)=URPSPRC(IY)*SCALPR
    z1 = z[50].copy()
    z1.iloc[:] = 1
    z[19] = z1 * dfd["URPSPRC"].values * SCALPR2

    #      Credit Cap
    # T122(20,IR,IY,IS)=EPRPSCP(IY)*SCALPR
    z[20] = (dfd["EPRPSCP"] * SCALPR2).loc[:lastyr]

    #    RPS/CPS Payments (billion #### dollars)

    #      Annual

    #        Credits
    # T122(21,IR,IY,IS)=URPSCST(IR,IY)*0.001*SCALPR
    z[21] = dfd["URPSCST"] * 0.001 * SCALPR2

    #        Penalty
    # T122(22,IR,IY,IS)=URPSPEN(IR,IY)*0.001*SCALPR
    z[22] = dfd["URPSPEN"] * 0.001 * SCALPR2

    #          Total
    # T122(23,IR,IY,IS)=FSUM(T122(21,IR,IY,IS),2)
    z[23] = z[21] + z[22]

    #      Cumulative

    #        Credits
    # DO IY1 = (YEARPR + 1) - 1989, IY
    CUMRPS = 0.0
    CUMPEN = 0.0
    for IY1 in range(
        (dfd["YEARPR"].iloc[0] + 1) - 1989, 61
    ):  # need to replace 61 with YR
        CUMRPS += dfd["URPSCST"]  # .loc[IY1]
        CUMPEN += dfd["URPSPEN"]  # .loc[IY1]

    # T122(24,IR,IY,IS)=CUMRPS*0.001*SCALPR
    z[24] = CUMRPS * 0.001 * SCALPR2

    #        Penalty
    # T122(25,IR,IY,IS)=CUMPEN*0.001*SCALPR
    z[25] = CUMPEN * 0.001 * SCALPR2

    #          Total
    # T122(26,IR,IY,IS)=FSUM(T122(24,IR,IY,IS),2)
    z[26] = z[24] + z[25]

    #   Production Tax Credits

    #    Qualifying PTC Generation (billion kwh)

    #      Coal
    # T122(27,IR,IY,IS)=UPTCLNR(IR,IY)
    z[27] = dfd["UPTCLNR"]

    #      Natural Gas
    # T122(28,IR,IY,IS)=UPTNGNR(IR,IY)
    z[28] = dfd["UPTNGNR"]

    #      Nuclear
    # T122(29,IR,IY,IS)=UPTNUNR(IR,IY)
    z[29] = dfd["UPTNUNR"]

    #      Conventional Hydroelectric Power
    # T122(30,IR,IY,IS)=UPTHYNR(IR,IY)
    z[30] = dfd["UPTHYNR"]

    #      Geothermal
    # T122(31,IR,IY,IS)=UPTGENR(IR,IY)
    z[31] = dfd["UPTGENR"]

    #      Municipal Waste
    # T122(32,IR,IY,IS)=UPTMSNR(IR,IY)
    z[32] = dfd["UPTMSNR"]

    #      Wood and Other Biomass
    # T122(33,IR,IY,IS)=UPTWDNR(IR,IY)+UPTCFNR(IR,IY)
    z[33] = dfd["UPTWDNR"] + dfd["UPTCFNR"]

    #         Dedicated Plants
    # T122(34,IR,IY,IS)=UPTWDNR(IR,IY)
    z[34] = dfd["UPTWDNR"]

    #         Cofiring
    # T122(35,IR,IY,IS)=UPTCFNR(IR,IY)
    z[35] = dfd["UPTCFNR"]

    #      Solar Thermal
    # T122(36,IR,IY,IS)=UPTSONR(IR,IY)
    z[36] = dfd["UPTSONR"]

    #      Solar Photovoltaic
    # T122(37,IR,IY,IS)=UPTPVNR(IR,IY)+UPTPTNR(IR,IY)
    z[37] = dfd["UPTPVNR"] + dfd["UPTPTNR"]

    #      Wind
    # T122(38,IR,IY,IS)=UPTWNNR(IR,IY)+UPTWLNR(IR,IY)+UPTWFNR(IR,IY)
    z[38] = dfd["UPTWNNR"] + dfd["UPTWLNR"] + dfd["UPTWFNR"]

    #        Total
    # T122(39,IR,IY,IS)=FSUM(T122(27,IR,IY,IS),6)+FSUM(T122(34,IR,IY,IS),5)
    z[39] = (
        z[27]
        + z[28]
        + z[29]
        + z[30]
        + z[31]
        + z[32]
        + z[34]
        + z[35]
        + z[36]
        + z[37]
        + z[38]
    )

    #

    #    Cost of PTC Credits (billion #### dollars)

    #      Annual
    # T122(40,IR,IY,IS)=UPTCCST(IR,IY)*0.001*SCALPR
    z[40] = dfd["UPTCCST"] * 0.001 * SCALPR2
    # CUMPTC=0.0
    # #DO IY1 = (YEARPR + 1) - 1989, IY
    # for IY1 in range((dfd['YEARPR'].iloc[0] + 1) - 1989, 61):  #need to replace 61 with IY
    #    CUMPTC += dfd['UPTCCST'] # need IY1

    #      Cumulative
    # T122(41,IR,IY,IS)=CUMPTC*0.001*SCALPR
    # z[41] = CUMPTC * 0.001 * SCALPR2
    year_index = table_spec["years"].index(year_cum_cap_addition_begin) + 1
    z[41] = z[40].copy()
    z[41].iloc[:, :year_index] = 0
    z[41] = z[41].cumsum(axis=1)

    return z
