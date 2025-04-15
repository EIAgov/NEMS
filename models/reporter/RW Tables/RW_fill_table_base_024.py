# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
Fixed by SZO on 7/1/20124
Fixed "Biofuels Heat and Coproducts" on 8/13/2024
"""


def fill_table_base_024(dfd, table_spec, table_id):
    """Fill table for  Renewable Energy Consumption by Sector and Source

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

    MNUMCR = dfd["MNUMCR_rwpre"]
    MNUMPR = dfd["MNUMPR_rwpre"]
    MNUMNR = dfd["MNUMNR_rwpre"]
    RDAYS = dfd["RDAYS_rwpre"]
    ETHANOL_PARAM = dfd["ETHANOL_PARAM_rwpre"]
    MNUMCR = dfd["MNUMCR_rwpre"]
    TRIL_TO_QUAD = dfd["TRIL_TO_QUAD_rwpre"]
    CFUBA = dfd["CFUBA_rwpre"]

    #   Renewable Energy Consumption by Sector and Source
    #   (quadrillion Btu, unless otherwise noted)
    #    Sector and Source

    #   Marketed Renewable Energy 1/
    #

    #     Residential (wood)
    # T24(1,IY,IS)=QBMRS(11,IY)
    z[1] = dfd["QBMRS"].loc[MNUMCR] * TRIL_TO_QUAD

    #

    #     Commercial (biomass)
    # T24(2,IY,IS)=QBMCM(11,IY)
    z[2] = dfd["QBMCM"].loc[MNUMCR] * TRIL_TO_QUAD

    #

    #     Industrial 2/ T24( 3,IY,IS) = FSUM(T24( 4,IY,IS),3) + T24(35,IY,IS)

    #       Conventional Hydroelectric Power
    # T24(4,IY,IS)=QHOIN(11,IY)
    z[4] = dfd["QHOIN"].loc[MNUMCR] * TRIL_TO_QUAD

    #       Municipal Waste 3/
    # T24(5,IY,IS)=QMSIN(11,IY)
    z[5] = dfd["QMSIN"].loc[MNUMCR] * TRIL_TO_QUAD

    #       Biomass
    # T24(6,IY,IS)=QBMIN(11,IY)-QBMRF(11,IY)
    z[6] = (dfd["QBMIN"].loc[MNUMCR] - dfd["QBMRF"].loc[MNUMCR]) * TRIL_TO_QUAD

    #       Biofuels Heat and Coproducts
    # T24(35,IY,IS)=((CORNCD(3,11,IY)*CFCORN/1000.-0.9751*(CRNETHCD(11,IY)+OTHETHCD(11,IY))*
    #        365.*CFPET-RFBIOBUTECD(MNUMCR,IY)*365.*CFBIOBUTE(IY))/1000000.+SUM(BIMQTYCD(1:4,11,IY))/1000000.*
    #        RDAYS*(CFVEGGIE(IY)-CFBIOD(IY))-RDAYS/1000000.*(SUM(BTLFRAC(1:4,MNUMPR,IY))*CFBTLLIQ(IY)+
    #       SUM(CBTLFRAC(2,1:4,MNUMPR,IY))*CFCBTLLIQ(2,IY)+UBAVOL(MNUMPR,IY)*5.763))
    # IF (CONEFF(IY) .NE. 0.0) T24(35,IY,IS) = T24(35,IY,IS) +  (0.9751*CLLETHCD(11,IY) * RDAYS * (CFBMQ(IY) * 42. / CONEFF(IY) - CFPET)) / 1000000.

    #       Biofuels Heat and Coproducts
    # T24(35,IY,IS)=((CORNCD(3,11,IY)*CFCORN/1000.-0.9751*(CRNETHCD(11,IY)+OTHETHCD(11,IY))*
    #        365.*CFPET(MNUMCR,IY)*365.*CFBIOBUTE(IY))/1000000.+SUM(BIMQTYCD(1:4,11,IY))/1000000.*
    #        RDAYS*(CFVEGGIE(IY)-CFBIOD(IY))-RDAYS/1000000.*(SUM(BTLFRAC(1:4,MNUMPR,IY))*CFBTLLIQ(IY)+
    #       SUM(CBTLFRAC(2,1:4,MNUMPR,IY))*CFCBTLLIQ(2,IY)+UBAVOL(MNUMPR,IY)*5.763))
    # IF (CONEFF(IY) .NE. 0.0) T24(35,IY,IS) = T24(35,IY,IS) +  (0.9751*CLLETHCD(11,IY) * RDAYS * (CFBMQ(IY) * 42. / CONEFF(IY) - CFPET)) / 1000000.
    # z[35] = (((dfd['CORNCD'].loc[3].loc[11] * dfd['CFCORN'].iloc[0] / 1000. - ETHANOL_PARAM
    # * (dfd['CRNETHCD'].loc[11] + dfd['OTHETHCD'].loc[11]) * RDAYS * dfd['CFPET'].iloc[0]
    # - dfd['RFBIOBUTECD'].loc[MNUMCR] * RDAYS * dfd['CFBIOBUTE']) / 1000000.
    # + dfd['BIMQTYCD'].loc[1:4,11,:].sum()/1000000.* RDAYS * (dfd['CFVEGGIE'] - dfd['CFBIOD'])
    # - RDAYS/1000000. * dfd['BTLFRAC'].loc[1:4,MNUMPR,:].sum()
    # * dfd['CFBTLLIQ'] + dfd['CBTLFRAC'].loc[2,1:4,MNUMPR,:].sum() * dfd['CFCBTLLIQ'].loc[2]
    # + dfd['UBAVOL'].loc[MNUMPR] * CFUBA))

    # z[35] = z[35] + ((ETHANOL_PARAM * dfd['CLLETHCD'].loc[11] * RDAYS * (dfd['CFBMQ'] * dfd['GAL_PER_BBL_rwpre']
    # / dfd['CONEFF'] - dfd['CFPET'].iloc[0])) / 1000000.).fillna(0.0)
    # z[35] = z[35].fillna(0.0)

    z[35] = (
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
            + dfd["UBAVOL"].loc[MNUMPR] * CFUBA
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

    z[3] = z[4] + z[5] + z[6] + z[35]
    #    Liquids from Biomass
    # T24(43,IY,IS)=RDAYS/1000000.*(SUM(BTLFRAC(1:4,MNUMPR,IY))*CFBTLLIQ(IY)+SUM(CBTLFRAC(2,1:4,MNUMPR,IY))*CFCBTLLIQ(2,IY)+UBAVOL(MNUMPR,IY)*5.763)
    z[43] = (
        RDAYS
        / 1000000.0
        * (
            dfd["BTLFRAC"].loc[1:4, MNUMPR, :].sum() * dfd["CFBTLLIQ"]
            + dfd["CBTLFRAC"].loc[2, 1:4, MNUMPR, :].sum() * dfd["CFCBTLLIQ"].loc[2]
            + dfd["UBAVOL"].loc[MNUMPR] * CFUBA
        )
    )

    #       Renewable Diesel and Gasoline 5/
    # T24(44,IY,IS)=(GRD2DSQTY(MNUMPR,IY)*CFDSQ+GRN2MGQTY(MNUMPR,IY)*CFNPQ+RENEWDIMP(MNUMPR,IY)*CFDSQ)*RDAYS/1000000.
    z[44] = (
        (
            dfd["GRD2DSQTY"].loc[MNUMPR] * dfd["CFDSQ"].iloc[0]
            + dfd["GRN2MGQTY"].loc[MNUMPR] * dfd["CFNPQ"].iloc[0]
            + dfd["RENEWDIMP"].loc[MNUMPR] * dfd["CFDSQ"].iloc[0]
        )
        * RDAYS
        / 1000000.0
    )

    #

    #     Electric Power 6/

    #       Conventional Hydroelectric Power
    # T24(11,IY,IS)=QHOEL(11,IY)
    z[11] = dfd["QHOEL"].loc[MNUMCR] * TRIL_TO_QUAD

    #       Geothermal
    # T24(12,IY,IS)=QGEEL(11,IY)
    z[12] = dfd["QGEEL"].loc[MNUMCR] * TRIL_TO_QUAD

    #       Biogenic Municipal Waste 7/
    # T24(13,IY,IS)=QMSEL(11,IY)-WNCMSEL(IY,11)
    z[13] = dfd["QMSEL"].loc[MNUMCR] * TRIL_TO_QUAD - dfd["WNCMSEL"].loc[MNUMCR]

    #         Landfill Gas-to-Energy
    # T24(31,IY,IS)=0
    z[31] = 0 * z[13]

    #         Other Municipal Waste
    # T24(32,IY,IS)=0
    z[32] = 0 * z[13]

    #       Biomass
    # T24(14,IY,IS)=QBMEL(11,IY)
    z[14] = dfd["QBMEL"].loc[MNUMCR] * TRIL_TO_QUAD

    #         Dedicated Plants    Note: What is 13500?    bKWH to  quad Btu?
    # T24(15,IY,IS)=13500.0*0.000001*(UGNWDNR(1,MNUMNR,IY)+UGNWDNR(2,MNUMNR,IY)+(CGNTGEN(MNUMNR,IY,7,1)+CGNTGEN(MNUMNR,IY,7,2))*0.001-UGNCFNR(1,MNUMNR,IY)-UGNCFNR(2,MNUMNR,IY))
    z[15] = (
        13500.0
        * 0.000001
        * (
            dfd["UGNWDNR"].loc[1].loc[MNUMNR]
            + dfd["UGNWDNR"].loc[2].loc[MNUMNR]
            + (
                dfd["CGNTGEN"].loc[MNUMNR].loc[7].loc[1]
                + dfd["CGNTGEN"].loc[MNUMNR].loc[7].loc[2]
            )
            * 0.001
            - dfd["UGNCFNR"].loc[1].loc[MNUMNR]
            - dfd["UGNCFNR"].loc[2].loc[MNUMNR]
        )
    )

    #         Cofiring
    # T24(16,IY,IS)=T24(14,IY,IS)-T24(15,IY,IS)
    z[16] = z[14] - z[15]

    #       Solar Thermal
    # T24(17,IY,IS)=QSTEL(11,IY)
    z[17] = dfd["QSTEL"].loc[MNUMCR] * TRIL_TO_QUAD

    #       Solar Photovoltaic 8/
    # T24(18,IY,IS)=QPVEL(11,IY)
    z[18] = dfd["QPVEL"].loc[MNUMCR] * TRIL_TO_QUAD

    #       Wind
    # T24(19,IY,IS)=QWIEL(11,IY)
    z[19] = dfd["QWIEL"].loc[MNUMCR] * TRIL_TO_QUAD

    # T24(10,IY,IS)=FSUM(T24(11,IY,IS),4)+FSUM(T24(17,IY,IS),3)
    z[10] = z[11] + z[12] + z[13] + z[14] + z[17] + z[18] + z[19]

    #

    #   Sources of Ethanol

    #     From Corn and Other Starch
    # T24(28,IY,IS)=0.9751*(CRNETHCD(11,IY)+OTHETHCD(11,IY))*RDAYS*CFPET/1000000.
    z[28] = (
        ETHANOL_PARAM
        * (dfd["CRNETHCD"].loc[MNUMCR] + dfd["OTHETHCD"].loc[MNUMCR])
        * (RDAYS)
        * dfd["CFPET"].iloc[0]
        / 1000000.0
    )

    #     From Cellulose
    # T24(29,IY,IS)=0.9751*CLLETHCD(11,IY)*RDAYS*CFPET/1000000.
    z[29] = (
        ETHANOL_PARAM
        * dfd["CLLETHCD"].loc[MNUMCR]
        * RDAYS
        * dfd["CFPET"].iloc[0]
        / 1000000.0
    )

    #     Net Imports
    # T24(30,IY,IS)=(ETHIMP(11,IY)-ETHEXP(11,IY))*RDAYS*CFPET/1000000.
    z[30] = (
        (dfd["ETHIMP"].loc[MNUMCR] - dfd["ETHEXP"].loc[MNUMCR])
        * (RDAYS)
        * dfd["CFPET"].iloc[0]
        / 1000000.0
    )

    #       Total U.S. Supply of Ethanol
    # T24(33,IY,IS)=FSUM(T24(28,IY,IS),3)
    z[33] = z[28] + z[29] + z[30]

    #     Electric Power Biomass + Cellulosic Ethanol
    # T24(36,IY,IS)=T24(29,IY,IS)+T24(14,IY,IS)
    z[36] = z[29] + z[14]

    #

    #   Sources of Biodiesel

    #       Soy Based
    # T24(37,IY,IS)=(BIMQTYCD(1,11,IY)+BIMQTYCD(4,11,IY))*RDAYS*CFBIOD(IY)/1000000.
    z[37] = (
        (dfd["BIMQTYCD"].loc[1].loc[MNUMCR] + dfd["BIMQTYCD"].loc[4].loc[MNUMCR])
        * (RDAYS)
        * dfd["CFBIOD"]
        / 1000000.0
    )

    #       Yellow Grease
    # T24(38,IY,IS)=BIMQTYCD(2,11,IY)*RDAYS*CFBIOD(IY)/1000000.
    z[38] = dfd["BIMQTYCD"].loc[2].loc[MNUMCR] * (RDAYS) * dfd["CFBIOD"] / 1000000.0

    #       White Grease
    # T24(39,IY,IS)=BIMQTYCD(3,11,IY)*RDAYS*CFBIOD(IY)/1000000.
    z[39] = dfd["BIMQTYCD"].loc[3].loc[MNUMCR] * (RDAYS) * dfd["CFBIOD"] / 1000000.0

    #       Net Imports
    # T24(40,IY,IS)=(BIODIMP(11,IY)-BIODEXP(11,IY))*RDAYS*CFBIOD(IY)/1000000.
    z[40] = (
        (dfd["BIODIMP"].loc[MNUMCR] - dfd["BIODEXP"].loc[MNUMCR])
        * (RDAYS)
        * dfd["CFBIOD"]
        / 1000000.0
    )

    # #       Biodiesel used in Distillate Blending
    # #T24(34,IY,IS)=(SUM(BIMQTYCD(1:4,11,IY))+BIODIMP(11,IY)-BIODEXP(11,IY))/1000000.*RDAYS*CFBIOD(IY)
    # #z[34] = dfd['BIMQTYCD'].loc[1:4].loc[MNUMCR].sum() + dfd['BIODIMP'].loc[MNUMCR] - dfd['BIODEXP'].loc[MNUMCR] / 1000000. * RDAYS * dfd['CFBIOD']
    z[34] = z[37] + z[38] + z[39] + z[40]
    #   Biobutanol
    z[45] = dfd["QBIOBUTE"].loc[MNUMCR] * dfd["CFBIOBUTE"] * RDAYS / 1000000.0
    #       Ethanol used in E85 4/
    # T24(8,IY,IS)=RFETHE85(MNUMPR,IY)*RDAYS*.001*CFETQ(IY)
    # z[8] = dfd['RFETHE85'].loc[MNUMPR] * RDAYS * .001 * dfd['CFETQ']
    # T24( 8,IY,IS) = QETTR(11,IY) * ETHNE85 * CFETQ(IY) / CFE85Q(IY)
    z[8] = (
        dfd["QETTR"].loc[MNUMCR]
        * TRIL_TO_QUAD
        * dfd["ETHNE85"].iloc[0]
        * dfd["CFETQ"]
        / dfd["CFE85Q"]
    )

    #     Transportation
    # T24(7,IY,IS)=(0.9751*(CRNETHCD(11,IY)+CLLETHCD(11,IY)+OTHETHCD(11,IY))+ETHIMP(11,IY)-ETHEXP(11,IY))/1000.*RDAYS*.001*CFPET
    # T24( 7,IY,IS) = T24( 7,IY,IS) + T24(34,IY,IS) + FSUM(T24(43,IY,IS),3)
    z[7] = (
        (
            (
                ETHANOL_PARAM
                * (
                    dfd["CRNETHCD"].loc[MNUMCR]
                    + dfd["CLLETHCD"].loc[MNUMCR]
                    + dfd["OTHETHCD"].loc[MNUMCR]
                )
                + dfd["ETHIMP"].loc[MNUMCR]
                - dfd["ETHEXP"].loc[MNUMCR]
            )
            / 1000.0
            * RDAYS
            * 0.001
            * dfd["CFPET"].iloc[0]
        )
        + z[34]
        + z[43]
        + z[44]
        + z[45]
    )

    #
    #       Ethanol used in Gasoline Blending
    z[9] = z[7] - z[8] - z[34] - z[45] - z[43] - z[44]

    #     Total Marketed Renewable Energy
    # T24(20,IY,IS)=T24(1,IY,IS)+T24(2,IY,IS)+T24(3,IY,IS)+T24(7,IY,IS)+T24(10,IY,IS)
    # z[20] = z[1]+z[2]+z[3]+z[7]+z[10]
    z[20] = z[1] + z[2] + z[3].iloc[0:] + z[7] + z[10]
    #   Nonmarketed Renewable Energy 9/

    #        Selected Consumption

    #       Solar Hot Water Heating
    # T24(22,IY,IS)=RSH2OCON(IY,5)/1000000000.
    z[22] = dfd["RSH2OCON"].loc[5] / 1000000000

    #       Solar Photovoltaic 8/
    # T24(24,IY,IS)=QPVRS(11,IY)
    z[24] = dfd["QPVRS"].loc[MNUMCR] * TRIL_TO_QUAD

    #       Wind
    # T24(41,IY,IS)=CGRESQ(11,IY,11)/1000.
    z[41] = dfd["CGRESQ"].loc[MNUMCR].loc[11] * TRIL_TO_QUAD

    #     Residential (Total)
    # T24(21,IY,IS)=(RSH2OCON(IY,5))/1000000000.+QPVRS(11,IY)+CGRESQ(11,IY,11)/1000.
    z[21] = z[22] + z[24] + z[41]

    #     Commercial
    # T24(25,IY,IS)=QSTCM(11,IY)+QPVCM(11,IY)+CGCOMMQ(11,IY,11)/1000.
    z[25] = (
        dfd["QSTCM"].loc[MNUMCR]
        + dfd["QPVCM"].loc[MNUMCR]
        + dfd["CGCOMMQ"].loc[MNUMCR].loc[11]
    ) * TRIL_TO_QUAD

    #       Solar Thermal
    # T24(26,IY,IS)=QSTCM(11,IY)
    z[26] = dfd["QSTCM"].loc[MNUMCR] * TRIL_TO_QUAD

    #       Solar Photovoltaic 8/
    # T24(27,IY,IS)=QPVCM(11,IY)
    z[27] = dfd["QPVCM"].loc[MNUMCR] * TRIL_TO_QUAD

    #       Wind
    # T24(42,IY,IS)=CGCOMMQ(11,IY,11)/1000.
    z[42] = dfd["CGCOMMQ"].loc[MNUMCR].loc[11] * TRIL_TO_QUAD

    return z
