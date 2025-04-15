# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
SZO fixed on multi-index accessing issues 8/9/2024
Fixed Solar Photovoltaic 4/ on 9/7 2024  
"""


def fill_table_base_067(dfd, table_spec, table_id):
    """Fill table  Renewable Energy Generation by Fuel
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
        EUPVGENADJ and EUPVCAPADJ are calculated in RW+_preprocessor_sme !

    """

    z = {}

    FYRPRC = int(table_spec["ftab_dat"]["FYRPRC"])
    year_idx = table_spec["years"].index(FYRPRC)

    #  Index confusion around row 216, see note. >>

    #   Renewable Energy Generation by Fuel
    #    Capacity, Generation, and Consumption
    #
    #   Electric Power Sector 1/

    #     Generating Capacity
    #     (gigawatts)

    #       Conventional Hydroelectric Power
    # T67(1,IR,IY,IS)=UCAPHYU(IR,IY)+UCAPHYN(IR,IY)+UCAPHYC(IR,IY)
    z[1] = dfd["UCAPHYU"] + dfd["UCAPHYN"] + dfd["UCAPHYC"]

    #       Geothermal 2/
    # T67(2,IR,IY,IS)=UCAPGEU(IR,IY)+UCAPGEN(IR,IY)+UCAPGEC(IR,IY)
    z[2] = dfd["UCAPGEU"] + dfd["UCAPGEN"] + dfd["UCAPGEC"]

    #       Municipal Waste 3/
    # T67(3,IR,IY,IS)=UCAPMSU(IR,IY)+UCAPMSN(IR,IY)+UCAPMSC(IR,IY)
    z[3] = dfd["UCAPMSU"] + dfd["UCAPMSN"] + dfd["UCAPMSC"]

    #       Wood and Other Biomass
    # T67(4,IR,IY,IS)=UCAPWDU(IR,IY)+UCAPWDN(IR,IY)+UCAPWDC(IR,IY)
    z[4] = dfd["UCAPWDU"] + dfd["UCAPWDN"] + dfd["UCAPWDC"] + dfd["UCAPBIU"] + dfd["UCAPBIN"]

    #       Solar Thermal
    # T67(5,IR,IY,IS)=UCAPSTU(IR,IY)+UCAPSTN(IR,IY)+UCAPSTC(IR,IY)
    z[5] = dfd["UCAPSTU"] + dfd["UCAPSTN"] + dfd["UCAPSTC"]

    #       Solar Photovoltaic 4/
    # T67(6,IR,IY,IS)=UCAPPVU(IR,IY)+UCAPPVN(IR,IY)+UCAPPVC(IR,IY)+UCAPPTU(IR,IY)+UCAPPTN(IR,IY)+UCAPPTC(IR,IY)
    z[6] = (
        dfd["UCAPPVU"]
        + dfd["UCAPPVN"]
        + dfd["UCAPPVC"]
        + dfd["UCAPPTU"]
        + dfd["UCAPPTN"]
        + dfd["UCAPPTC"]
    )

    #       Wind
    # T67(7,IR,IY,IS)=UCAPWNU(IR,IY)+UCAPWNN(IR,IY)+UCAPWNC(IR,IY)+UCAPWLU(IR,IY)+UCAPWLN(IR,IY)+UCAPWLC(IR,IY)
    z[7] = (
        dfd["UCAPWNU"]
        + dfd["UCAPWNN"]
        + dfd["UCAPWNC"]
        + dfd["UCAPWLU"]
        + dfd["UCAPWLN"]
        + dfd["UCAPWLC"]
    )

    #       Offshore Wind
    # T67(8,IR,IY,IS)=UCAPWFU(IR,IY)+UCAPWFN(IR,IY)+UCAPWFC(IR,IY)
    z[8] = dfd["UCAPWFU"] + dfd["UCAPWFN"] + dfd["UCAPWFC"]

    #         Total
    # T67(9,IR,IY,IS)=FSUM(T67(1,IR,IY,IS),8)
    z[9] = z[1] + z[2] + z[3] + z[4] + z[5] + z[6] + z[7] + z[8]

    #     Cumulative Capacity Additions 5/
    #     (gigawatts)

    #       Conventional Hydroelectric Power
    # T67(70,IR,IY,IS)=UADDHYU(1,IR,IY)+UADDHYN(1,IR,IY)+UADDHYU(2,IR,IY)+UADDHYN(2,IR,IY)+UADDHYC(IR,IY)
    z[70] = (
        dfd["UADDHYU"].loc[1]
        + dfd["UADDHYN"].loc[1]
        + dfd["UADDHYU"].loc[2]
        + dfd["UADDHYN"].loc[2]
        + dfd["UADDHYC"]
    )

    #       Geothermal 2/
    # T67(71,IR,IY,IS)=UADDGEU(1,IR,IY)+UADDGEN(1,IR,IY)+UADDGEU(2,IR,IY)+UADDGEN(2,IR,IY)+UADDGEC(IR,IY)
    z[71] = (
        dfd["UADDGEU"].loc[1]
        + dfd["UADDGEN"].loc[1]
        + dfd["UADDGEU"].loc[2]
        + dfd["UADDGEN"].loc[2]
        + dfd["UADDGEC"]
    )

    #       Municipal Waste 3/
    # T67(72,IR,IY,IS)=UADDMSU(1,IR,IY)+UADDMSN(1,IR,IY)+UADDMSU(2,IR,IY)+UADDMSN(2,IR,IY)+UADDMSC(IR,IY)
    z[72] = (
        dfd["UADDMSU"].loc[1]
        + dfd["UADDMSN"].loc[1]
        + dfd["UADDMSU"].loc[2]
        + dfd["UADDMSN"].loc[2]
        + dfd["UADDMSC"]
    )

    #       Wood and Other Biomass
    # T67(73,IR,IY,IS)=UADDWDU(1,IR,IY)+UADDWDN(1,IR,IY)+UADDWDU(2,IR,IY)+UADDWDN(2,IR,IY)+UADDWDC(IR,IY)
    z[73] = (
        dfd["UADDWDU"].loc[1]
        + dfd["UADDWDN"].loc[1]
        + dfd["UADDWDU"].loc[2]
        + dfd["UADDWDN"].loc[2]
        + dfd["UADDWDC"]
        + dfd["UADDBIU"].loc[1]
        + dfd["UADDBIU"].loc[2]
        + dfd["UADDBIN"].loc[1]
        + dfd["UADDBIN"].loc[2]
    )

    #       Solar Thermal
    # T67(74,IR,IY,IS)=UADDSTU(1,IR,IY)+UADDSTN(1,IR,IY)+UADDSTU(2,IR,IY)+UADDSTN(2,IR,IY)+UADDSTC(IR,IY)
    z[74] = (
        dfd["UADDSTU"].loc[1]
        + dfd["UADDSTN"].loc[1]
        + dfd["UADDSTU"].loc[2]
        + dfd["UADDSTN"].loc[2]
        + dfd["UADDSTC"]
    )

    #       Solar Photovoltaic 4/
    # T67(75,IR,IY,IS)=UADDPVU(1,IR,IY)+UADDPVN(1,IR,IY)+UADDPVU(2,IR,IY)+UADDPVN(2,IR,IY)+UADDPVC(IR,IY)+UADDPTU(1,IR,IY)+UADDPTN(1,IR,IY)+UADDPTU(2,IR,IY)+UADDPTN(2,IR,IY)+UADDPTC(IR,IY)
    z[75] = (
        dfd["UADDPVU"].loc[1]
        + dfd["UADDPVN"].loc[1]
        + dfd["UADDPVU"].loc[2]
        + dfd["UADDPVN"].loc[2]
        + dfd["UADDPVC"]
        + dfd["UADDPTU"].loc[1]
        + dfd["UADDPTN"].loc[1]
        + dfd["UADDPTU"].loc[2]
        + dfd["UADDPTN"].loc[2]
        + dfd["UADDPTC"]
    )

    #       Wind
    # T67(76,IR,IY,IS)=UADDWNU(1,IR,IY)+UADDWNN(1,IR,IY)+UADDWNU(2,IR,IY)+UADDWNN(2,IR,IY)+UADDWNC(IR,IY)+UADDWLU(1,IR,IY)+UADDWLN(1,IR,IY)+UADDWLU(2,IR,IY)+UADDWLN(2,IR,IY)+UADDWLC(IR,IY)
    z[76] = (
        dfd["UADDWNU"].loc[1]
        + dfd["UADDWNN"].loc[1]
        + dfd["UADDWNU"].loc[2]
        + dfd["UADDWNN"].loc[2]
        + dfd["UADDWNC"]
        + dfd["UADDWLU"].loc[1]
        + dfd["UADDWLN"].loc[1]
        + dfd["UADDWLU"].loc[2]
        + dfd["UADDWLN"].loc[2]
        + dfd["UADDWLC"]
    )

    #       Offshore Wind
    # T67(77,IR,IY,IS)=UADDWFU(1,IR,IY)+UADDWFN(1,IR,IY)+UADDWFU(2,IR,IY)+UADDWFN(2,IR,IY)+UADDWFC(IR,IY)
    z[77] = (
        dfd["UADDWFU"].loc[1]
        + dfd["UADDWFN"].loc[1]
        + dfd["UADDWFU"].loc[2]
        + dfd["UADDWFN"].loc[2]
        + dfd["UADDWFC"]
    )

    #         Total
    # T67(78,IR,IY,IS)=FSUM(T67(70,IR,IY,IS),8)
    z[78] = z[70] + z[71] + z[72] + z[73] + z[74] + z[75] + z[76] + z[77]

    #

    #     Electricity Generation

    #     (billion kilowatthours)

    #       Conventional Hydroelectric Power
    # T67(10,IR,IY,IS)=UGNHYNR(1,IR,IY)+UGNHYNR(2,IR,IY)+(CGNTGEN(IR,IY,4,1)+CGNTGEN(IR,IY,4,2))*0.001
    # z[10] = dfd['UGNHYNR'].loc[1] + dfd['UGNHYNR'].loc[2] + (dfd['CGNTGEN'].loc[4].loc[1] + dfd['CGNTGEN'].loc[4].loc[2]) * 0.001
    z[10] = (
        dfd["UGNHYNR"].loc[1]
        + dfd["UGNHYNR"].loc[2]
        + dfd["CGNTGEN"].loc[(slice(None), 4, [1, 2]), :].groupby(level=0).sum() * 0.001
    )

    #       Geothermal 2/
    # T67(11,IR,IY,IS)=UGNGENR(1,IR,IY)+UGNGENR(2,IR,IY)+(CGNTGEN(IR,IY,5,1)+CGNTGEN(IR,IY,5,2))*0.001
    # z[11] = dfd['UGNGENR'].loc[1] + dfd['UGNGENR'].loc[2] + (dfd['CGNTGEN'].loc[5].loc[1] + dfd['CGNTGEN'].loc[5].loc[2]) * 0.001
    z[11] = (
        dfd["UGNGENR"].loc[1]
        + dfd["UGNGENR"].loc[2]
        + dfd["CGNTGEN"].loc[(slice(None), 5, [1, 2]), :].groupby(level=0).sum() * 0.001
    )

    #       Biogenic Municipal Waste 6/
    # T67(12,IR,IY,IS)=UGNMSNR(1,IR,IY)+UGNMSNR(2,IR,IY)+(CGNTGEN(IR,IY,6,1)+CGNTGEN(IR,IY,6,2))*0.001
    # z[12] = dfd['UGNMSNR'].loc[1] + dfd['UGNMSNR'].loc[2] + (dfd['CGNTGEN'].loc[6].loc[1] + dfd['CGNTGEN'].loc[6].loc[2]) * 0.001
    z[12] = (
        dfd["UGNMSNR"].loc[1]
        + dfd["UGNMSNR"].loc[2]
        + dfd["CGNTGEN"].loc[(slice(None), 6, [1, 2]), :].groupby(level=0).sum() * 0.001
    )

    #       Wood and Other Biomass
    # T67(13,IR,IY,IS)=UGNWDNR(1,IR,IY)+UGNWDNR(2,IR,IY)+(CGNTGEN(IR,IY,7,1)+CGNTGEN(IR,IY,7,2))*0.001
    z[13] = (
        dfd["UGNWDNR"].loc[1]
        + dfd["UGNWDNR"].loc[2]
        + dfd["CGNTGEN"].loc[(slice(None), 7, [1, 2]), :].groupby(level=0).sum() * 0.001
    )

    #       Solar Thermal
    # T67(14,IR,IY,IS)=UGNSONR(1,IR,IY)+UGNSONR(2,IR,IY)
    z[14] = dfd["UGNSONR"].loc[1] + dfd["UGNSONR"].loc[2]

    #       Solar Photovoltaic 4/
    # T67(15,IR,IY,IS)=UGNPVNR(1,IR,IY)+UGNPVNR(2,IR,IY)+UGNPTNR(1,IR,IY)+UGNPTNR(2,IR,IY)+(CGNTGEN(IR,IY,8,1)+CGNTGEN(IR,IY,8,2))*0.001
    # z[15] = dfd['UGNPVNR'].loc[1] + dfd['UGNPVNR'].loc[2] + dfd['UGNPTNR'].loc[1] + dfd['UGNPTNR'].loc[2] + (dfd['CGNTGEN'].loc[8].loc[1] + dfd['CGNTGEN'].loc[8].loc[2]) * 0.001
    z[15] = (
        dfd["UGNPVNR"].loc[1]
        + dfd["UGNPVNR"].loc[2]
        + dfd["UGNPTNR"].loc[1]
        + dfd["UGNPTNR"].loc[2]
        + dfd["CGNTGEN"].loc[(slice(None), 8, [1, 2]), :].groupby(level=0).sum() * 0.001
    )

    #       Wind
    # T67(16,IR,IY,IS)=UGNWNNR(1,IR,IY)+UGNWNNR(2,IR,IY)+UGNWLNR(1,IR,IY)+UGNWLNR(2,IR,IY)
    z[16] = (
        dfd["UGNWNNR"].loc[1]
        + dfd["UGNWNNR"].loc[2]
        + dfd["UGNWLNR"].loc[1]
        + dfd["UGNWLNR"].loc[2]
    )

    #       Offshore Wind
    # T67(17,IR,IY,IS)=UGNWFNR(1,IR,IY)+UGNWFNR(2,IR,IY)
    z[17] = dfd["UGNWFNR"].loc[1] + dfd["UGNWFNR"].loc[2]

    #         Total
    # T67(18,IR,IY,IS)=FSUM(T67(10,IR,IY,IS),8)
    z[18] = z[10] + z[11] + z[12] + z[13] + z[14] + z[15] + z[16] + z[17]

    #     Energy Consumption 7/
    #     (quadrillion Btu)

    #       Conventional Hydroelectric Power
    # T67(19,IR,IY,IS)=(UFLHYNR(1,IR,IY)+UFLHYNR(2,IR,IY))/1000.
    z[19] = (dfd["UFLHYNR"].loc[1] + dfd["UFLHYNR"].loc[2]) / 1000.0

    #       Geothermal 2/
    # T67(20,IR,IY,IS)=(UFLGTNR(1,IR,IY)+UFLGTNR(2,IR,IY))/1000.
    z[20] = (dfd["UFLGTNR"].loc[1] + dfd["UFLGTNR"].loc[2]) / 1000.0

    #       Biogenic Municipal Waste 6/
    # T67(21,IR,IY,IS)=((UFLMSNR(1,IR,IY)+UFLMSNR(2,IR,IY))/1000.)-WNCMSELN(IY,IR)
    z[21] = ((dfd["UFLMSNR"].loc[1] + dfd["UFLMSNR"].loc[2]) / 1000.0) - dfd["WNCMSELN"]

    #       Wood and Other Biomass
    # T67(22,IR,IY,IS)=(UFLWDNR(1,IR,IY)+UFLWDNR(2,IR,IY))/1000.
    z[22] = (dfd["UFLWDNR"].loc[1] + dfd["UFLWDNR"].loc[2]) / 1000.0

    #       Solar Thermal
    # T67(23,IR,IY,IS)=(UFLSONR(1,IR,IY)+UFLSONR(2,IR,IY))/1000.
    z[23] = (dfd["UFLSONR"].loc[1] + dfd["UFLSONR"].loc[2]) / 1000.0

    #       Solar Photovoltaic 4/
    # T67(24,IR,IY,IS)=(UFLPVNR(1,IR,IY)+UFLPVNR(2,IR,IY))/1000.
    z[24] = (dfd["UFLPVNR"].loc[1] + dfd["UFLPVNR"].loc[2]) / 1000.0

    #       Wind
    # T67(25,IR,IY,IS)=(UFLWNNR(1,IR,IY)+UFLWNNR(2,IR,IY))/1000.
    z[25] = (dfd["UFLWNNR"].loc[1] + dfd["UFLWNNR"].loc[2]) / 1000.0

    #       Offshore Wind
    # T67(26,IR,IY,IS)=(UFLWFNR(1,IR,IY)+UFLWFNR(2,IR,IY))/1000.
    z[26] = (dfd["UFLWFNR"].loc[1] + dfd["UFLWFNR"].loc[2]) / 1000.0

    #         Total
    # T67(27,IR,IY,IS)=FSUM(T67(19,IR,IY,IS),8)
    z[27] = z[19] + z[20] + z[21] + z[22] + z[23] + z[24] + z[25] + z[26]

    #
    #   End-Use Generators 8/

    #     Generating Capacity
    #     (gigawatts)

    #       Conventional Hydroelectric Power
    # T67(28,IR,IY,IS)=CGTOTCAPNR(IR,IY,5)*0.001
    # z[28] = dfd['CGTOTCAPNR'].loc[5] * 0.001
    z[28] = dfd["CGTOTCAPNR"].loc[(slice(None), 5), :].groupby(level=0).sum() * 0.001

    #       Geothermal 2/
    # T67(29,IR,IY,IS)=CGTOTCAPNR(IR,IY,6)*0.001
    # z[29] = dfd['CGTOTCAPNR'].loc[6] * 0.001
    z[29] = dfd["CGTOTCAPNR"].loc[(slice(None), 6), :].groupby(level=0).sum() * 0.001

    #       Municipal Waste
    # T67(30,IR,IY,IS)=CGTOTCAPNR(IR,IY,7)*0.001
    # z[30] = dfd['CGTOTCAPNR'].loc[7] * 0.001
    z[30] = dfd["CGTOTCAPNR"].loc[(slice(None), 7), :].groupby(level=0).sum() * 0.001

    #       Wood and Other Biomass
    # T67(31,IR,IY,IS)=CGTOTCAPNR(IR,IY,8)*0.001
    # z[31] = dfd['CGTOTCAPNR'].loc[8] * 0.001
    z[31] = dfd["CGTOTCAPNR"].loc[(slice(None), 8), :].groupby(level=0).sum() * 0.001

    #       Solar Photovoltaic 4/
    # #T67(32,IR,IY,IS)=CGTOTCAPNR(IR,IY,10)*0.001
    # z[32] = dfd['CGTOTCAPNR'].loc[10] * 0.001
    # T67(32,IR,IY,IS) = CGTOTCAPNR(IR,IY,10) * EUPVCAPADJ(IY) * 0.001  ! adj PV
    z[32] = (
        dfd["CGTOTCAPNR"].loc[(slice(None), 10), :].groupby(level=0).sum()
        * dfd["EUPVCAPADJ"]
        * 0.001
    )

    #       Wind
    # T67(33,IR,IY,IS)=CGTOTCAPNR(IR,IY,12)*0.001
    # z[33] = dfd['CGTOTCAPNR'].loc[12] * 0.001
    z[33] = dfd["CGTOTCAPNR"].loc[(slice(None), 12), :].groupby(level=0).sum() * 0.001

    #         Total
    # T67(34,IR,IY,IS)=FSUM(T67(28,IR,IY,IS),6)
    z[34] = z[28] + z[29] + z[30] + z[31] + z[32] + z[33]
    z[34] = z[34].groupby(level=0).sum()  # sum by NERC region

    #

    #     Cumulative Capacity Additions 5/
    #     (gigawatts)
    #
    # ftab.f
    #   CUMCAPADD=0     !  set these two to 0 initially, as there is corrective action
    # ...
    # IF (CUMCAPADD .EQ. 0) CUMCAPADD = FYRPRC
    # ...
    # !     -- end use generators Capacity additions
    #       IF (IY .GT. (CUMCAPADD - 1989)) THEN
    #          IF (IR .LE. MNUMNR-1) THEN
    #             T67(79,IR,IY,IS) = T67(28,IR,IY,IS) - T67(28,IR,(CUMCAPADD - 1989),IS)
    #             T67(80,IR,IY,IS) = T67(29,IR,IY,IS) - T67(29,IR,(CUMCAPADD - 1989),IS)
    #             T67(81,IR,IY,IS) = T67(30,IR,IY,IS) - T67(30,IR,(CUMCAPADD - 1989),IS)
    #             T67(82,IR,IY,IS) = T67(31,IR,IY,IS) - T67(31,IR,(CUMCAPADD - 1989),IS)
    #             T67(83,IR,IY,IS) = T67(32,IR,IY,IS) - T67(32,IR,(CUMCAPADD - 1989),IS)
    #             T67(84,IR,IY,IS) = T67(33,IR,IY,IS) - T67(33,IR,(CUMCAPADD - 1989),IS)
    #             T67(85,IR,IY,IS) = FSUM(T67(79,IR,IY,IS),6)
    #          ELSE
    #             T67(79,IR,IY,IS) = sum(T67(79,1:MNUMNR-1,IY,IS))
    #             T67(80,IR,IY,IS) = sum(T67(80,1:MNUMNR-1,IY,IS))
    #             T67(81,IR,IY,IS) = sum(T67(81,1:MNUMNR-1,IY,IS))
    #             T67(82,IR,IY,IS) = sum(T67(82,1:MNUMNR-1,IY,IS))
    #             T67(83,IR,IY,IS) = sum(T67(83,1:MNUMNR-1,IY,IS))
    #             T67(84,IR,IY,IS) = sum(T67(84,1:MNUMNR-1,IY,IS))
    #             T67(85,IR,IY,IS) = FSUM(T67(79,IR,IY,IS),6)
    #          ENDIF
    #
    # #
    # #       Conventional Hydroelectric Power
    # #T67(79,IR,IY,IS)=T67(28,IR,IY,IS)-T67(28,IR,(CUMCAPADD-1989),IS)
    # z[79] = z[28]-z[28]
    #
    # For year > CUMCAPADD - 1989, calculate the difference from year CUMCAPADD - 1989
    z[79] = z[28].subtract(z[28].iloc[:, year_idx], axis=0)
    # zero years before CUMCAPADD - 1989
    z[79].iloc[:, :year_idx] = 0
    # Sum all regions to get the total of US
    z[79].iloc[-1] = z[79].iloc[:-1].sum()

    # #       Geothermal 2/
    # #T67(80,IR,IY,IS)=T67(29,IR,IY,IS)-T67(29,IR,(CUMCAPADD-1989),IS)
    # z[80] = z[29]-z[29]
    # For year > CUMCAPADD - 1989, calculate the difference from year CUMCAPADD - 1989
    z[80] = z[29].subtract(z[29].iloc[:, year_idx], axis=0)
    # zero years before CUMCAPADD - 1989
    z[80].iloc[:, :year_idx] = 0
    # Sum all regions to get the total of US
    z[80].iloc[-1] = z[80].iloc[:-1].sum()
    #
    # #       Municipal Waste
    # #T67(81,IR,IY,IS)=T67(30,IR,IY,IS)-T67(30,IR,(CUMCAPADD-1989),IS)
    # z[81] = z[30]-z[30]
    # For year > CUMCAPADD - 1989, calculate the difference from year CUMCAPADD - 1989
    z[81] = z[30].subtract(z[30].iloc[:, year_idx], axis=0)
    # zero years before CUMCAPADD - 1989
    z[81].iloc[:, :year_idx] = 0
    # Sum all regions to get the total of US
    z[81].iloc[-1] = z[81].iloc[:-1].sum()

    # #       Wood and Other Biomass
    # #T67(82,IR,IY,IS)=T67(31,IR,IY,IS)-T67(31,IR,(CUMCAPADD-1989),IS)
    # z[82] = z[31]-z[31]
    z[82] = z[31].subtract(z[31].iloc[:, year_idx], axis=0)
    # zero years before CUMCAPADD - 1989
    z[82].iloc[:, :year_idx] = 0
    # Sum all regions to get the total of US
    z[82].iloc[-1] = z[82].iloc[:-1].sum()

    # #       Solar Photovoltaic 4/
    # #T67(83,IR,IY,IS)=T67(32,IR,IY,IS)-T67(32,IR,(CUMCAPADD-1989),IS)
    # z[83] = z[32]-z[32]
    z[83] = z[32].subtract(z[32].iloc[:, year_idx], axis=0)
    # zero years before CUMCAPADD - 1989
    z[83].iloc[:, :year_idx] = 0
    # Sum all regions to get the total of US
    z[83].iloc[-1] = z[83].iloc[:-1].sum()
    #
    # #       Wind
    # #T67(84,IR,IY,IS)=T67(33,IR,IY,IS)-T67(33,IR,(CUMCAPADD-1989),IS)
    # z[84] = z[33]-z[33]
    z[84] = z[33].subtract(z[33].iloc[:, year_idx], axis=0)
    # zero years before CUMCAPADD - 1989
    z[84].iloc[:, :year_idx] = 0
    # Sum all regions to get the total of US
    z[84].iloc[-1] = z[84].iloc[:-1].sum()

    # # <<  Index confusion above >>

    #         Total
    # T67(85,IR,IY,IS)=FSUM(T67(79,IR,IY,IS),6)
    z[85] = z[79] + z[80] + z[81] + z[82] + z[83] + z[84]
    z[85] = z[85].groupby(level=0).sum()  # sum by NERC region

    #     Electricity Generation
    #     (billion kilowatthours)

    #       Conventional Hydroelectric Power
    # T67(35,IR,IY,IS)=(CGTOTGENNR(IR,IY,5,1)+CGTOTGENNR(IR,IY,5,2))*0.001
    # z[35] = (dfd['CGTOTGENNR'].loc[5].loc[1] + dfd['CGTOTGENNR'].loc[5].loc[2]) * 0.001
    z[35] = (
        dfd["CGTOTGENNR"].loc[(slice(None), 5, [1, 2]), :].groupby(level=0).sum()
    ) * 0.001

    #       Geothermal 2/
    # T67(36,IR,IY,IS)=(CGTOTGENNR(IR,IY,6,1)+CGTOTGENNR(IR,IY,6,2))*0.001
    z[36] = (
        dfd["CGTOTGENNR"].loc[(slice(None), 6, [1, 2]), :].groupby(level=0).sum()
    ) * 0.001

    #       Municipal Waste
    # T67(37,IR,IY,IS)=(CGTOTGENNR(IR,IY,7,1)+CGTOTGENNR(IR,IY,7,2))*0.001
    z[37] = (
        dfd["CGTOTGENNR"].loc[(slice(None), 7, [1, 2]), :].groupby(level=0).sum()
    ) * 0.001

    #       Wood and Other Biomass
    # T67(38,IR,IY,IS)=(CGTOTGENNR(IR,IY,8,1)+CGTOTGENNR(IR,IY,8,2))*0.001
    z[38] = (
        dfd["CGTOTGENNR"].loc[(slice(None), 8, [1, 2]), :].groupby(level=0).sum()
    ) * 0.001

    #       Solar Photovoltaic 4/
    # #T67(39,IR,IY,IS)=(CGTOTGENNR(IR,IY,10,1)+CGTOTGENNR(IR,IY,10,2))*0.001
    # z[39] = (dfd['CGTOTGENNR'].loc[10].loc[1] + dfd['CGTOTGENNR'].loc[10].loc[2]) * 0.001
    # T67(39,IR,IY,IS) = (CGTOTGENNR(IR,IY,10,1) + CGTOTGENNR(IR,IY,10,2) ) * EUPVGENADJ(IY) * 0.001    ! adj PV
    z[39] = (
        dfd["CGTOTGENNR"].loc[(slice(None), 10, [1, 2]), :].groupby(level=0).sum()
        * dfd["EUPVGENADJ"]
        * 0.001
    )

    #       Wind
    # T67(40,IR,IY,IS)=(CGTOTGENNR(IR,IY,12,1)+CGTOTGENNR(IR,IY,12,2))*0.001
    z[40] = (
        dfd["CGTOTGENNR"].loc[(slice(None), 12, [1, 2]), :].groupby(level=0).sum()
        * 0.001
    )

    #         Total
    # T67(41,IR,IY,IS)=FSUM(T67(35,IR,IY,IS),6)
    z[41] = z[35] + z[36] + z[37] + z[38] + z[39] + z[40]
    #

    #     Energy Consumption 7/
    #     (quadrillion Btu)

    #       Conventional Hydroelectric Power
    # T67(42,IR,IY,IS)=CGTOTQNR(IR,IY,5)*0.001
    z[42] = dfd["CGTOTQNR"].loc[(slice(None), 5), :].groupby(level=0).sum() * 0.001

    #       Geothermal 2/
    # T67(43,IR,IY,IS)=CGTOTQNR(IR,IY,6)*0.001
    z[43] = dfd["CGTOTQNR"].loc[(slice(None), 6), :].groupby(level=0).sum() * 0.001

    #       Municipal Waste
    # T67(44,IR,IY,IS)=CGTOTQNR(IR,IY,7)*0.001
    z[44] = dfd["CGTOTQNR"].loc[(slice(None), 7), :].groupby(level=0).sum() * 0.001

    #       Wood and Other Biomass
    # T67(45,IR,IY,IS)=CGTOTQNR(IR,IY,8)*0.001
    z[45] = dfd["CGTOTQNR"].loc[(slice(None), 8), :].groupby(level=0).sum() * 0.001

    #       Solar Photovoltaic 4/
    # #T67(46,IR,IY,IS)=CGTOTQNR(IR,IY,10)*0.001
    # z[46] = dfd['CGTOTQNR'].loc[10] * 0.001
    # T67(46,IR,IY,IS) = CGTOTQNR(IR,IY,10) * EUPVGENADJ(IY) * 0.001 !adj PV consistent with gen
    z[46] = (
        dfd["CGTOTQNR"].loc[(slice(None), 10), :].groupby(level=0).sum()
        * dfd["EUPVGENADJ"]
        * 0.001
    )

    #       Wind
    # T67(47,IR,IY,IS)=CGTOTQNR(IR,IY,12)*0.001
    z[47] = dfd["CGTOTQNR"].loc[(slice(None), 12), :].groupby(level=0).sum() * 0.001

    #         Total
    # T67(48,IR,IY,IS)=FSUM(T67(42,IR,IY,IS),6)
    z[48] = z[42] + z[43] + z[44] + z[45] + z[46] + z[47]

    #
    #   All Sectors

    #     Generating Capacity
    #     (gigawatts)

    #       Conventional Hydroelectric Power
    # T67(49,IR,IY,IS)=T67(1,IR,IY,IS)+T67(28,IR,IY,IS)
    z[49] = z[1] + z[28]

    #       Geothermal 2/
    # T67(50,IR,IY,IS)=T67(2,IR,IY,IS)+T67(29,IR,IY,IS)
    z[50] = z[2] + z[29]

    #       Municipal Waste
    # T67(51,IR,IY,IS)=T67(3,IR,IY,IS)+T67(30,IR,IY,IS)
    z[51] = z[3] + z[30]

    #       Wood and Other Biomass
    # T67(52,IR,IY,IS)=T67(4,IR,IY,IS)+T67(31,IR,IY,IS)
    z[52] = z[4] + z[31]

    #       Solar 4/
    # T67(53,IR,IY,IS)=T67(5,IR,IY,IS)+T67(6,IR,IY,IS)+T67(32,IR,IY,IS)
    z[53] = z[5] + z[6] + z[32]

    #       Wind
    # T67(54,IR,IY,IS)=T67(7,IR,IY,IS)+T67(8,IR,IY,IS)+T67(33,IR,IY,IS)
    z[54] = z[7] + z[8] + z[33]

    #         Total
    # T67(55,IR,IY,IS)=FSUM(T67(49,IR,IY,IS),6)
    z[55] = z[49] + z[50] + z[51] + z[52] + z[53] + z[54]
    z[55] = z[55].groupby(level=0).sum()  # sum by NERC region

    #     Electricity Generation
    #     (billion kilowatthours)

    #       Conventional Hydroelectric Power
    # T67(56,IR,IY,IS)=T67(10,IR,IY,IS)+T67(35,IR,IY,IS)
    z[56] = z[10] + z[35]

    #       Geothermal 2/
    # T67(57,IR,IY,IS)=T67(11,IR,IY,IS)+T67(36,IR,IY,IS)
    z[57] = z[11] + z[36]

    #       Municipal Waste 9/
    # T67(58,IR,IY,IS)=T67(12,IR,IY,IS)+T67(37,IR,IY,IS)
    z[58] = z[12] + z[37]

    #       Wood and Other Biomass
    # T67(59,IR,IY,IS)=T67(13,IR,IY,IS)+T67(38,IR,IY,IS)
    z[59] = z[13] + z[38]

    #       Solar 4/
    # T67(60,IR,IY,IS)=T67(14,IR,IY,IS)+T67(15,IR,IY,IS)+T67(39,IR,IY,IS)
    z[60] = z[14] + z[15] + z[39]

    #       Wind
    # T67(61,IR,IY,IS)=T67(16,IR,IY,IS)+T67(17,IR,IY,IS)+T67(40,IR,IY,IS)
    z[61] = z[16] + z[17] + z[40]

    #         Total
    # T67(62,IR,IY,IS)=FSUM(T67(56,IR,IY,IS),6)
    z[62] = z[56] + z[57] + z[58] + z[59] + z[60] + z[61]

    #
    #     Energy Consumption 7/
    #     (quadrillion Btu)

    #       Conventional Hydroelectric Power
    # T67(63,IR,IY,IS)=T67(19,IR,IY,IS)+T67(42,IR,IY,IS)
    z[63] = z[19] + z[42]

    #       Geothermal 2/
    # T67(64,IR,IY,IS)=T67(20,IR,IY,IS)+T67(43,IR,IY,IS)
    z[64] = z[20] + z[43]

    #       Municipal Waste 9/
    # T67(65,IR,IY,IS)=T67(21,IR,IY,IS)+T67(44,IR,IY,IS)
    z[65] = z[21] + z[44]

    #       Wood and Other Biomass
    # T67(66,IR,IY,IS)=T67(22,IR,IY,IS)+T67(45,IR,IY,IS)
    z[66] = z[22] + z[45]

    #       Solar 4/
    # T67(67,IR,IY,IS)=T67(23,IR,IY,IS)+T67(24,IR,IY,IS)+T67(46,IR,IY,IS)
    z[67] = z[23] + z[24] + z[46]

    #       Wind
    # T67(68,IR,IY,IS)=T67(25,IR,IY,IS)+T67(26,IR,IY,IS)+T67(47,IR,IY,IS)
    z[68] = z[25] + z[26] + z[47]

    #         Total
    # T67(69,IR,IY,IS)=FSUM(T67(63,IR,IY,IS),6)
    z[69] = z[63] + z[64] + z[65] + z[66] + z[67] + z[68]

    return z
