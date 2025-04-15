# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
SZO fixed Refining Activity Only/Carbon Dioxide Emissions 5/. on 8/2024

"""
import pandas as pd
from RW_preprocessor import indcarb


def fill_table_base_035(dfd, table_spec, table_id):
    """Fill table   Refining Industry Energy Consumption
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
    MNUMOR = dfd["MNUMOR_rwpre"]
    MNUMCR = dfd["MNUMCR_rwpre"]
    RDAYS = dfd["RDAYS_rwpre"]
    CFUBA = dfd["CFUBA_rwpre"]
    ETHANOL_PARAM = dfd["ETHANOL_PARAM_rwpre"]
    CARBON_OR_2 = table_spec["settings"]["carbon_or_carbondioxide"].upper()

    # Index Constants for Industrial Tables 35 to 44
    #  integer ixEL/1/,ixNG/2/,ixCL/3/,ixMC/4/, &
    #          ixCI/5/,ixRF/6/,ixDS/7/,ixLG/8/,ixMG/9/, &
    #          ixSG/10/,ixPC/11/,ixAS/12/,ixPF/13/,ixKS/14/, &
    #          ixOP/15/,ixNF/16/,ixLF/17/,ixRN/18/

    #   Refining Industry Energy Consumption
    #    Shipments, Energy Consumption, and Emissions
    #
    #   Value of Shipments (billion m==m dollars)
    #   Inputs to Distillation Units
    # T35( 1,IY,IS) = MC_REVIND(11,25,IY)
    z[1] = dfd["MC_REVIND"].loc[25, MNUMCR]

    #   (million barrels per day)
    # T35(62,IY,IS)=RFQTDCRD(MNUMOR+2,IY)+RFIMCR(MNUMPR,IY)+RFCRDOTH(MNUMPR,IY)+RFPQUFC(MNUMPR,IY,2)
    z[62] = (
        dfd["RFQTDCRD"].loc[MNUMOR + 2]
        + dfd["RFIMCR"].loc[MNUMPR]
        + dfd["RFCRDOTH"].loc[MNUMPR]
        + dfd["RFPQUFC"].loc[MNUMPR].loc[2]
    )

    #   Total Energy Consumption (trillion Btu) 1/

    #    Residual Fuel Oil
    # T35(2,IY,IS)=REFCON(IXRF,5,IY)
    z[2] = dfd["INDREP/REFCON"].loc[IXRF].loc[5]

    #    Distillate Fuel Oil
    # T35(3,IY,IS)=REFCON(IXDS,5,IY)
    z[3] = dfd["INDREP/REFCON"].loc[IXDS].loc[5]

    #   Liquefied Petroleum Gas Heat and Power 2/
    # T35(4,IY,IS)=REFCON(IXLG,5,IY)
    z[4] = dfd["INDREP/REFCON"].loc[IXLG].loc[5]

    #    Petroleum Coke
    # T35(5,IY,IS)=REFCON(IXPC,5,IY)
    z[5] = dfd["INDREP/REFCON"].loc[IXPC].loc[5]

    #    Still Gas
    # T35(6,IY,IS)=REFCON(IXSG,5,IY)
    z[6] = dfd["INDREP/REFCON"].loc[IXSG].loc[5]

    #    Other Petroleum 3/
    # T35(7,IY,IS)=REFCON(IXOP,5,IY)
    z[7] = dfd["INDREP/REFCON"].loc[IXOP].loc[5]

    #      Petroleum and Other Liquids Subtotal
    # T35(8,IY,IS)=FSUM(T35(2,IY,IS),6)
    z[8] = z[2] + z[3] + z[4] + z[5] + z[6] + z[7]

    #    Natural Gas
    # T35(9,IY,IS)=REFCON(IXNG,5,IY)
    z[9] = dfd["INDREP/REFCON"].loc[IXNG].loc[5]

    #       of which:  Heat and Power
    # T35(68,IY,IS)=REFCON(IXNG,5,IY)-RFQNGPF(11,IY)
    z[68] = (
        dfd["INDREP/REFCON"].loc[IXNG].loc[5]
        - dfd["RFQNGPF"].loc[11]
    )

    #       of which:  Feedstocks
    # T35(69,IY,IS)=RFQNGPF(11,IY)
    z[69] = dfd["RFQNGPF"].loc[MNUMCR]

    #    Steam Coal 4/
    # T35(10,IY,IS)=REFCON(IXCL,5,IY)
    z[10] = dfd["INDREP/REFCON"].loc[IXCL].loc[5]

    #    Biofuels Heat and Coproducts
    # T35(48,IY,IS)=1000.*((CORNCD(3,11,IY)*CFCORN/1000.-0.9751*(CRNETHCD(11,IY)+OTHETHCD(11,IY))*365.*CFPET-RFBIOBUTECD(MNUMCR,IY)*365.*CFBIOBUTE(IY))/1000000.+SUM(BIMQTYCD(1:4,11,IY))/1000000.*RDAYS*(CFVEGGIE(IY)-CFBIOD(IY))-RDAYS/1000000.*
    #                    (SUM(BTLFRAC(1:4,MNUMPR,IY))*CFBTLLIQ(IY)+SUM(CBTLFRAC(2,1:4,MNUMPR,IY))*CFCBTLLIQ(2,IY)+UBAVOL(MNUMPR,IY)*5.763))
    z[48] = 1000.0 * (
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
                * (dfd["CFBMQ"] * 42.0 / dfd["CONEFF"] - dfd["CFPET"].iloc[0])
            )
            / 1000000.0
        )
    )
    #    Renewables
    # T35(63,IY,IS)=(CGREFQ(11,IY,6)+CGREFQ(11,IY,7))
    z[63] = dfd["INDREP/REFCON"].loc[IXHF].loc[5]

    #    Purchased Electricity
    # T35(11,IY,IS)=REFCON(IXEL,5,IY)
    z[11] = dfd["INDREP/REFCON"].loc[IXEL].loc[5]

    #      Total
    # T35(12,IY,IS)=FSUM(T35(8,IY,IS),4)+T35(48,IY,IS)
    z[12] = z[8] + z[9] + z[10] + z[11] + z[48]

    #   Summing LPG and Still Gas
    # T35(67,IY,IS)=REFCON(IXLG,5,IY)+REFCON(IXSG,5,IY)
    z[67] = (
        dfd["INDREP/REFCON"].loc[IXLG].loc[5] + dfd["INDREP/REFCON"].loc[IXSG].loc[5]
    )

    #

    #   Carbon Dioxide Emissions 5/

    #   (million metric tons carbon_dd_)
    # Note: get this from Table 22
    # T35(24,IY,IS)=INDCARB(REFCON(1,1,IY),IY)

    z[24] = indcarb(dfd, dfd["INDREP/REFCON"], CARBON_OR_2)

    #

    #   Energy Related to Refining Activity Only

    #     Energy Consumption

    #       Residual Fuel Oil
    # T35(13,IY,IS)=REFCON(IXRF,5,IY)
    z[13] = dfd["INDREP/REFCON"].loc[IXRF].loc[5]

    #       Distillate Fuel Oil
    # T35(14,IY,IS)=REFCON(IXDS,5,IY)
    z[14] = dfd["INDREP/REFCON"].loc[IXDS].loc[5]

    #       Liquefied Petroleum Gas Heat and Power 2/
    # T35(15,IY,IS)=REFCON(IXLG,5,IY)
    z[15] = dfd["INDREP/REFCON"].loc[IXLG].loc[5]

    #       Petroleum Coke
    # T35(16,IY,IS)=REFCON(IXPC,5,IY)-CGREFQ(11,IY,2)
    z[16] = dfd["INDREP/REFCON"].loc[IXPC].loc[5] - dfd["CGREFQ"].loc[MNUMCR].loc[2]

    #       Still Gas
    # T35(17,IY,IS)=REFCON(IXSG,5,IY)-CGREFQ(11,IY,9)
    z[17] = dfd["INDREP/REFCON"].loc[IXSG].loc[5] - dfd["CGREFQ"].loc[11].loc[9]

    #       Other Petroleum 3/
    # T35(18,IY,IS)=REFCON(IXOP,5,IY)
    z[18] = dfd["INDREP/REFCON"].loc[IXOP].loc[5]

    #         Petroleum and Other Liquids Subtotal
    # T35(19,IY,IS)=FSUM(T35(13,IY,IS),6)
    z[19] = z[13] + z[14] + z[15] + z[16] + z[17] + z[18]

    #       Natural Gas
    # T35(20,IY,IS)=REFCON(IXNG,5,IY)-CGREFQ(11,IY,3)-QNGETH(IY,11)-RFQNGPF(11,IY)
    z[20] = (
        dfd["INDREP/REFCON"].loc[IXNG].loc[5]
        - dfd["CGREFQ"].loc[11].loc[3]
        - dfd["QNGETH"].loc[11]
        - dfd["RFQNGPF"].loc[11]
    )

    #       Steam Coal 4/       note: indices reversed
    # T35(21,IY,IS)=MAX(0.0,(REFCON(IXCL,5,IY)-CGREFQ(11,IY,1)-QCLETH(IY,11)))
    z[21] = (
        dfd["INDREP/REFCON"].loc[IXCL].loc[5]
        - dfd["CGREFQ"].loc[MNUMCR].loc[1]
        - dfd["QCLETH"].loc[MNUMCR]
    )

    #       Purchased Electricity
    # T35(22,IY,IS)=REFCON(IXEL,5,IY)-QELETH(IY,11)
    z[22] = dfd["INDREP/REFCON"].loc[IXEL].loc[5] - dfd["QELETH"].loc[11]

    #         Total
    # T35(23,IY,IS)=FSUM(T35(19,IY,IS),4)
    z[23] = z[19] + z[20] + z[21] + z[22]

    #

    #     Carbon Dioxide Emissions 5/
    #     (million metric tons carbon_dd_)

    # # do carbon dioxide for refinery use only
    # # modREFCON(ixRF,5,IY) = z[13]
    # # modREFCON(ixDS,5,IY) = z[14]
    # # modREFCON(ixLG,5,IY) = z[15]
    # # modREFCON(ixPC,5,IY) = z[16]
    # # modREFCON(ixSG,5,IY) = z[17]
    # # modREFCON(ixOP,5,IY) = z[18]
    # # modREFCON(ixNG,5,IY) = z[20]
    # # modREFCON(ixCL,5,IY) = z[21]

    # # modREFCON(ixEL,1,IY) = dfd['REFCON'].loc[ixEL,1] - dfd['QELETH'].loc[:,1:2].sum()
    # # modREFCON(ixEL,2,IY) = dfd['REFCON'].loc[ixEL,2] - dfd['QELETH'].loc[:,3:4].sum()
    # # modREFCON(ixEL,3,IY) = dfd['REFCON'].loc[ixEL,3] - dfd['QELETH'].loc[:,5:7].sum()
    # # modREFCON(ixEL,4,IY) = dfd['REFCON'].loc[ixEL,4] - dfd['QELETH'].loc[:,8:9].sum()
    # # modREFCON(ixEL,5,IY) = sum(modREFCON(ixEL,1:4,IY))

    # new_df = dfd.copy()

    # # new_df['REFCON'].loc[IXEL].loc[1] = new_df['REFCON'].loc[IXEL].loc[1] - new_df['QELETH'].loc[1:2,:].sum()
    # # new_df['REFCON'].loc[IXEL].loc[2] = new_df['REFCON'].loc[IXEL].loc[2] - new_df['QELETH'].loc[3:4,:].sum()
    # # new_df['REFCON'].loc[IXEL].loc[3] = new_df['REFCON'].loc[IXEL].loc[3] - new_df['QELETH'].loc[5:7,:].sum()
    # # new_df['REFCON'].loc[IXEL].loc[4] = new_df['REFCON'].loc[IXEL].loc[4] - new_df['QELETH'].loc[8:9,:].sum()
    # # new_df['REFCON'].loc[IXEL].loc[5] = new_df['REFCON'].loc[IXEL,1:4,:].sum()

    # # #T35(66,IY,IS)=INDCARB(modREFCON(1,1,IY),IY)
    # TN 035 ---------------------------------------------------------------------------------------
    #
    # ftab.f
    # ...
    # REAL modREFCON(18,5,MNUMYR)
    # ...
    # ! Index Constants for Industrial Tables 35 to 44
    #       integer ixEL/1/,ixNG/2/,ixCL/3/,ixMC/4/, &
    #               ixCI/5/,ixRF/6/,ixDS/7/,ixLG/8/,ixMG/9/, &
    #               ixSG/10/,ixPC/11/,ixAS/12/,ixPF/13/,ixKS/14/, &
    #               ixOP/15/,ixNF/16/,ixLF/17/,ixRN/18/
    # ...
    # !     Table 35. Energy Consumption: Refineries

    # !      integer ixEL/1/,ixNG/2/,ixCL/3/,ixMC/4/,
    # !     &        ixCI/5/,ixRF/6/,ixDS/7/,ixLG/8/,ixMG/9/,
    # !     &        ixSG/10/,ixPC/11/,ixAS/12/,ixPF/13/,ixKS/14/,
    # !     &        ixOP/15/,ixNF/16/,ixLF/17/,ixRN/18/
    #          modREFCON(:,:,:) = 0.0
    #          DO 350, IY = 1,LASTYR
    #            RDAYS=365.
    # ...
    # ! do carbon dioxide for refinery use only
    #            modREFCON(ixRF,5,IY) = T35(13,IY,IS)
    #            modREFCON(ixDS,5,IY) = T35(14,IY,IS)
    #            modREFCON(ixLG,5,IY) = T35(15,IY,IS)
    #            modREFCON(ixPC,5,IY) = T35(16,IY,IS)
    #            modREFCON(ixSG,5,IY) = T35(17,IY,IS)
    #            modREFCON(ixOP,5,IY) = T35(18,IY,IS)
    #            modREFCON(ixNG,5,IY) = T35(20,IY,IS)
    #            modREFCON(ixCL,5,IY) = T35(21,IY,IS)

    #            modREFCON(ixEL,1,IY) = REFCON(ixEL,1,IY) -sum(QELETH(IY,1:2))
    #            modREFCON(ixEL,2,IY) = REFCON(ixEL,2,IY) -sum(QELETH(IY,3:4))
    #            modREFCON(ixEL,3,IY) = REFCON(ixEL,3,IY) -sum(QELETH(IY,5:7))
    #            modREFCON(ixEL,4,IY) = REFCON(ixEL,4,IY) -sum(QELETH(IY,8:9))
    #            modREFCON(ixEL,5,IY) = sum(modREFCON(ixEL,1:4,IY))
    #
    #            T35(66,IY,IS) = INDCARB(modREFCON(1,1,IY),IY) ! function indcarb calcs carbon emissions

    # Define the indices as constants
    ixEL = 1
    ixNG = 2
    ixCL = 3
    ixMC = 4
    ixCI = 5
    ixRF = 6
    ixDS = 7
    ixLG = 8
    ixMG = 9
    ixSG = 10
    ixPC = 11
    ixAS = 12
    ixPF = 13
    ixKS = 14
    ixOP = 15
    ixNF = 16
    ixLF = 17
    ixRN = 18

    # Create a blank df modREFCON
    indexes = pd.MultiIndex.from_product(
        [range(1, ixRN + 1), range(1, 6)], names=["a", "b"]
    )
    modREFCON = pd.DataFrame(0.0, index=indexes, columns=table_spec["years"])

    modREFCON.loc[ixRF, 5] = z[13]
    modREFCON.loc[ixDS, 5] = z[14]
    modREFCON.loc[ixLG, 5] = z[15]
    modREFCON.loc[ixPC, 5] = z[16]
    modREFCON.loc[ixSG, 5] = z[17]
    modREFCON.loc[ixOP, 5] = z[18]
    # modREFCON.loc[ixEL, 5] = z[19]
    modREFCON.loc[ixNG, 5] = z[20]
    modREFCON.loc[ixCL, 5] = z[21]

    # INDREP REFCON R 4 3 (18,MINDCR,MNUMYR) QUNTY tBTU INDUST REFINERY INDUSTRY CONSUMPTION,
    #   18 Fuels, Regions 1:4 and US:5
    # WRENEW QELETH R 4 2 (MNUMYR,MNUMCR) QUNTY tBTU REFINE Ethanol Plant Consumption - Electricity
    modREFCON.loc[ixEL, 1] = (
        dfd["INDREP/REFCON"].loc[ixEL, 1] - dfd["QELETH"].loc[1:2].sum()
    )
    modREFCON.loc[ixEL, 2] = (
        dfd["INDREP/REFCON"].loc[ixEL, 2] - dfd["QELETH"].loc[3:4].sum()
    )
    modREFCON.loc[ixEL, 3] = (
        dfd["INDREP/REFCON"].loc[ixEL, 3] - dfd["QELETH"].loc[5:7].sum()
    )
    modREFCON.loc[ixEL, 4] = (
        dfd["INDREP/REFCON"].loc[ixEL, 4] - dfd["QELETH"].loc[8:9].sum()
    )
    modREFCON.loc[ixEL, 5] = modREFCON.loc[ixEL, 1:4].sum()

    # # Call to the function for carbon emissions
    # T35[66, IY, IS] = INDCARB(modREFCON[0, 0, IY], IY)
    # T35(66,IY,IS) = INDCARB(modREFCON(1,1,IY),IY) ! function indcarb calcs carbon emissions
    z[66] = indcarb(dfd, modREFCON.fillna(0), CARBON_OR_2)

    #     Energy Consumption per Unit of Refinery Input
    #     (thousand Btu per barrel)
    DENOM = z[62] * RDAYS / 1000.0
    #       Residual Fuel Oil

    # T35(50,IY,IS)=T35(13,IY,IS)/DENOM
    z[50] = z[13] / DENOM  #  DENOM defined elsewhere?

    #       Distillate Fuel Oil
    # T35(51,IY,IS)=T35(14,IY,IS)/DENOM
    z[51] = z[14] / DENOM

    #       Liquefied Petroleum Gas Heat and Power 2/
    # T35(52,IY,IS)=T35(15,IY,IS)/DENOM
    z[52] = z[15] / DENOM

    #       Petroleum Coke
    # T35(53,IY,IS)=T35(16,IY,IS)/DENOM
    z[53] = z[16] / DENOM

    #       Still Gas
    # T35(54,IY,IS)=T35(17,IY,IS)/DENOM
    z[54] = z[17] / DENOM

    #       Other Petroleum 3/
    # T35(55,IY,IS)=T35(18,IY,IS)/DENOM
    z[55] = z[18] / DENOM

    #         Petroleum and Other Liquids Subtotal
    # T35(56,IY,IS)=FSUM(T35(50,IY,IS),6)
    z[56] = z[50] + z[51] + z[52] + z[53] + z[54] + z[55]

    #       Natural Gas
    # T35(57,IY,IS)=T35(20,IY,IS)/DENOM
    z[57] = z[20] / DENOM

    #       Steam Coal 4/
    # T35(58,IY,IS)=T35(21,IY,IS)/DENOM
    z[58] = z[21] / DENOM

    #       Purchased Electricity
    # T35(60,IY,IS)=T35(22,IY,IS)/DENOM
    z[60] = z[22] / DENOM

    #      Total
    # T35(61,IY,IS)=FSUM(T35(56,IY,IS),5)
    z[61] = z[56] + z[57] + z[58] + z[60]

    #
    #   Combined Heat and Power

    #     Generating Capacity (gigawatts) 6/

    #       Petroleum
    # T35(26,IY,IS) = CGREFCAP(11,IY,2) * .001
    z[26] = dfd["CGREFCAP"].loc[11].loc[2] * 0.001

    #       Natural Gas
    # T35(27,IY,IS) = CGREFCAP(11,IY,3) * .001
    z[27] = dfd["CGREFCAP"].loc[11].loc[3] * 0.001

    #       Coal
    # T35(25,IY,IS) = CGREFCAP(11,IY,1) * .001
    z[25] = dfd["CGREFCAP"].loc[11].loc[1] * 0.001

    #       Other 7/
    # T35(28,IY,IS) =(CGREFCAP(11,IY,6) + CGREFCAP(11,IY,7) + CGREFCAP(11,IY,9)) * .001
    z[28] = (
        dfd["CGREFCAP"].loc[11].loc[6]
        + dfd["CGREFCAP"].loc[11].loc[7]
        + dfd["CGREFCAP"].loc[11].loc[9]
    ) * 0.001

    #         Total
    # T35(29,IY,IS) = sum(T35(25:28,IY,IS))
    z[29] = z[25] + z[26] + z[27] + z[28]

    #     Net Generation (billion kilowatthours)

    #       Petroleum
    # T35(31,IY,IS) = (CGREFGEN(11,IY,2,1) + CGREFGEN(11,IY,2,2)) * .001
    z[31] = (
        dfd["CGREFGEN"].loc[11].loc[2].loc[1] + dfd["CGREFGEN"].loc[11].loc[2].loc[2]
    ) * 0.001

    #       Natural Gas
    # T35(32,IY,IS) = (CGREFGEN(11,IY,3,1) + CGREFGEN(11,IY,3,2)) * .001
    z[32] = (
        dfd["CGREFGEN"].loc[11].loc[3].loc[1] + dfd["CGREFGEN"].loc[11].loc[3].loc[2]
    ) * 0.001

    #       Coal
    # T35(30,IY,IS) = (CGREFGEN(11,IY,1,1) + CGREFGEN(11,IY,1,2)) * .001
    z[30] = (
        dfd["CGREFGEN"].loc[11].loc[1].loc[1] + dfd["CGREFGEN"].loc[11].loc[1].loc[2]
    ) * 0.001

    #       Other 7/
    # T35(33,IY,IS) = (CGREFGEN(11,IY,6,1) + CGREFGEN(11,IY,6,2) + CGREFGEN(11,IY,7,1) + CGREFGEN(11,IY,7,2) + CGREFGEN(11,IY,9,1) + CGREFGEN(11,IY,9,2)) * .001
    z[33] = (
        dfd["CGREFGEN"].loc[11].loc[6].loc[1]
        + dfd["CGREFGEN"].loc[11].loc[6].loc[2]
        + dfd["CGREFGEN"].loc[11].loc[7].loc[1]
        + dfd["CGREFGEN"].loc[11].loc[7].loc[2]
        + dfd["CGREFGEN"].loc[11].loc[9].loc[1]
        + dfd["CGREFGEN"].loc[11].loc[9].loc[2]
    ) * 0.001

    #         Total

    # T35(34,IY,IS) = sum(T35(IL+1:IL+4,IY,IS))
    z[34] = z[31] + z[32] + z[30] + z[33]

    #       Disposition

    #         Sales to the Grid
    # T35(35,IY,IS)=sum(CGREFGEN(11,IY,:,1)) *.001
    z[35] = dfd["CGREFGEN"].loc[11, :, 1, :].sum() * 0.001

    #         Generation for Own Use
    # T35(36,IY,IS)=sum(CGREFGEN(11,IY,:,2)) *.001
    z[36] = dfd["CGREFGEN"].loc[11, :, 2, :].sum() * 0.001

    #     Energy Input (trillion Btu)

    #       Petroleum
    # T35(38,IY,IS) =  CGREFQ(11,IY,2)
    z[38] = dfd["CGREFQ"].loc[11].loc[2]

    #       Natural Gas
    # T35(39,IY,IS) =  CGREFQ(11,IY,3)
    z[39] = dfd["CGREFQ"].loc[11].loc[3]

    #       Coal 3/
    # T35(37,IY,IS) =  CGREFQ(11,IY,1)
    z[37] = dfd["CGREFQ"].loc[11].loc[1]

    #       Other 5/
    # T35(40,IY,IS) = (CGREFQ(11,IY,6) + CGREFQ(11,IY,7) + CGREFQ(11,IY,9))
    z[40] = (
        dfd["CGREFQ"].loc[11].loc[6]
        + dfd["CGREFQ"].loc[11].loc[7]
        + dfd["CGREFQ"].loc[11].loc[9]
    )

    #         Total
    # T35(41,IY,IS) = sum(T35(37:40,IY,IS))
    z[41] = z[37] + z[38] + z[39] + z[40]

    #

    #   Energy Consumed at Ethanol Plants

    #   (trillion Btu)

    #     Natural Gas
    # T35(44,IY,IS)=QNGETH(IY,11)
    z[44] = dfd["QNGETH"].loc[11]

    #     Coal
    # T35(45,IY,IS)=QCLETH(IY,11)
    z[45] = dfd["QCLETH"].loc[11]

    #     Electricity
    # T35(46,IY,IS)=QELETH(IY,11)
    z[46] = dfd["QELETH"].loc[11]

    #       Total
    # T35(47,IY,IS)=FSUM(T35(44,IY,IS),3)
    z[47] = z[44] + z[45] + z[46]

    return z
