# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_004(dfd, table_spec, table_id):
    """Residential Sector Key Indicators and Consumption.

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

    RECYEAR = 2019  # (from include file resdrep ; there will be a disconnect since we aren't using the include file; either have to add to restart or change in two places??)
    # DO 40 IY=RECSYEAR-BASEYR+1,LASTYR  - this table only fills RECYEAR to end instead of starting in 1990
    FYRPRC = int(table_spec["settings"]["year_index_real_dol_priceyr"])
    # Get inflation index from 1990->2050
    MC_JPGDP = dfd["MC_JPGDP"].loc[1990:].T

    # Get inflation index of FYRPRC (2022)
    SCALPR2 = MC_JPGDP.iloc[FYRPRC - 1990]  # 2022 = column 33

    MNUMCR = dfd["MNUMCR_rwpre"]
    ELECLOSS = dfd["QTSEL"].loc[MNUMCR] - dfd["QELAS"].loc[MNUMCR]
    WattHr_to_BTU_rwpre = 3.412
    TRIL_TO_QUAD = 0.001

    # Residential Sector Key Indicators and Consumption	Residential
    # (quadrillion Btu, unless otherwise noted)
    # Key Indicators and Consumption

    # Key Indicators
    # Households (millions)
    # Single-Family	1
    # T4(1,IY,IS)=(RSEH(IY,1)+RSNH(IY,1))/1000000.
    z[1] = (dfd["RSEH"].loc[1] + dfd["RSNH"].loc[1]) / 1000000
    # Multifamily
    # T4(2,IY,IS)=(RSEH(IY,2)+RSNH(IY,2))/1000000.
    z[2] = (dfd["RSEH"].loc[2] + dfd["RSNH"].loc[2]) / 1000000
    # Mobile Homes
    # T4(3,IY,IS)=(RSEH(IY,3)+RSNH(IY,3))/1000000.
    z[3] = (dfd["RSEH"].loc[3] + dfd["RSNH"].loc[3]) / 1000000
    # Total
    # T4(4,IY,IS)=FSUM(T4(1,IY,IS),3)
    z[4] = z[1] + z[2] + z[3]
    # Average House Square Footage	5
    # T4(5,IY,IS)=SQFTAVG(IY)
    z[5] = dfd["SQFTAVG"]

    # Energy Consumption by Fuel
    # Electricity 1/
    # Space Heating
    # T4(9,IY,IS)=RSHTRCON(IY,2)/1000000000.
    z[9] = dfd["RSHTRCON"].loc[2] / 1000000000
    # Space Cooling
    # T4(10,IY,IS)=RSCOOLCN(IY,1)/1000000000.
    z[10] = dfd["RSCOOLCN"].loc[1] / 1000000000
    # Water Heating
    # T4(11,IY,IS)=RSH2OCON(IY,2)/1000000000.
    z[11] = dfd["RSH2OCON"].loc[2] / 1000000000
    # Refrigeration
    # T4(12,IY,IS)=RSREFCON(IY)/1000000000.
    z[12] = dfd["RSREFCON"] / 1000000000
    # Cooking	13
    # T4(13,IY,IS)=RSCKCON(IY,3)/1000000000.
    z[13] = dfd["RSCKCON"].loc[3] / 1000000000
    # Clothes Dryers
    # T4(14,IY,IS)=RSDRYCON(IY,2)/1000000000.
    z[14] = dfd["RSDRYCON"].loc[2] / 1000000000
    # Freezers	15
    # T4(15,IY,IS)=RSFRZCON(IY)/1000000000.
    z[15] = dfd["RSFRZCON"] / 1000000000
    # Lighting	16
    # T4(16,IY,IS)=RSLTCON(IY)/1000000000.
    z[16] = dfd["RSLTCON"] / 1000000000
    # Clothes Washers 2/
    # T4(17,IY,IS)=RSCSWCON(IY)/1000000000.
    z[17] = dfd["RSCSWCON"] / 1000000000
    # Dishwashers 2/
    # T4(18,IY,IS)=RSDSWCON(IY)/1000000000.
    z[18] = dfd["RSDSWCON"] / 1000000000
    # Televisions and Related Equipment 3/	19
    # T4(19,IY,IS)=RSTVRCON(IY)/1000000000.
    z[19] = dfd["RSTVRCON"] / 1000000000
    # Computers and Related Equipment 4/	20
    # T4(20,IY,IS)=RSPCRCON(IY)/1000000000.
    z[20] = dfd["RSPCRCON"] / 1000000000
    # Furnace Fans and Boiler Circulation Pumps	21
    # T4(21,IY,IS)=RSFANCON(IY)/1000000000.
    z[21] = dfd["RSFANCON"] / 1000000000
    # Other Uses 5/
    # T4(22,IY,IS)=RSAPCON(IY,2)/1000000000.
    z[22] = dfd["RSAPCON"].loc[2] / 1000000000
    # Gross End-User Electricity Consumption
    # T4(23,IY,IS)=SUM(T4(9:22,IY,IS))
    z[23] = (
        z[9]
        + z[10]
        + z[11]
        + z[12]
        + z[13]
        + z[14]
        + z[15]
        + z[16]
        + z[17]
        + z[18]
        + z[19]
        + z[20]
        + z[21]
        + z[22]
    )

    # QELRS_EV(IR,IY) = TRQ_ELEC(1,IR,IY)/1000.0
    QELRS_EV = dfd["TRQ_ELEC"].loc[1].loc[MNUMCR]

    # Purchased Electricity for Electric Vehicle Charging
    z[116] = QELRS_EV * TRIL_TO_QUAD

    # On-site Generation for Own Use
    # T4(109,IY,IS)=(CGRESGEN(11,IY,3,2)+CGRESGEN(11,IY,8,2)+CGRESGEN(11,IY,11,2))*3.412*0.000001
    z[109] = (
        (
            dfd["CGRESGEN"].loc[MNUMCR].loc[3].loc[2]
            + dfd["CGRESGEN"].loc[MNUMCR].loc[8].loc[2]
            + dfd["CGRESGEN"].loc[MNUMCR].loc[11].loc[2]
        )
        * WattHr_to_BTU_rwpre
        * 0.000001
    )
    # Purchased Electricity at Residential Location
    # T4(110,IY,IS) = T4(23,IY,IS) - T4(109,IY,IS) ! Total net of DG
    z[110] = z[23] + z[116] - z[109]

    # Natural Gas
    # Space Heating
    # T4(24,IY,IS)=RSHTRCON(IY,1)/1000000000.
    z[24] = dfd["RSHTRCON"].loc[1] / 1000000000
    # Space Cooling
    # T4(25,IY,IS)=RSCOOLCN(IY,3)/1000000000.
    z[25] = dfd["RSCOOLCN"].loc[3] / 1000000000
    # Water Heating
    # T4(26,IY,IS)=RSH2OCON(IY,1)/1000000000.
    z[26] = dfd["RSH2OCON"].loc[1] / 1000000000
    # Cooking
    # T4(27,IY,IS)=RSCKCON(IY,1)/1000000000.
    z[27] = dfd["RSCKCON"].loc[1] / 1000000000
    # Clothes Dryers
    # T4(28,IY,IS)=RSDRYCON(IY,1)/1000000000.
    z[28] = dfd["RSDRYCON"].loc[1] / 1000000000
    # Other Uses 6/
    # T4(29,IY,IS)=RSAPCON(IY,1)/1000000000.
    z[29] = dfd["RSAPCON"].loc[1] / 1000000000
    # Delivered Energy
    # T4(30,IY,IS)=QNGRS(11,IY)
    z[30] = dfd["QNGRS"].loc[MNUMCR] * TRIL_TO_QUAD

    # Distillate Fuel Oil 7/
    # Space Heating
    # T4(31,IY,IS)=RSHTRCON(IY,3)/1000000000.
    z[31] = dfd["RSHTRCON"].loc[3] / 1000000000
    # Water Heating
    # T4(32,IY,IS)=RSH2OCON(IY,3)/1000000000.
    z[32] = dfd["RSH2OCON"].loc[3] / 1000000000
    # Other Uses 8/
    # T4(33,IY,IS)=RSAPCON(IY,3)/1000000000.
    z[33] = dfd["RSAPCON"].loc[3] / 1000000000
    # Delivered Energy
    # T4(34,IY,IS)=QDSRS(11,IY)
    z[34] = dfd["QDSRS"].loc[MNUMCR] * TRIL_TO_QUAD

    # Propane
    # Space Heating
    # T4(35,IY,IS)=RSHTRCON(IY,4)/1000000000.
    z[35] = dfd["RSHTRCON"].loc[4] / 1000000000
    # Water Heating
    # T4(36,IY,IS)=RSH2OCON(IY,4)/1000000000.
    z[36] = dfd["RSH2OCON"].loc[4] / 1000000000
    # Cooking
    # T4(37,IY,IS)=RSCKCON(IY,2)/1000000000.
    z[37] = dfd["RSCKCON"].loc[2] / 1000000000
    # Other Uses 9/
    # T4(38,IY,IS)=RSAPCON(IY,4)/1000000000.
    z[38] = dfd["RSAPCON"].loc[4] / 1000000000
    # Delivered Energy
    # T4(39,IY,IS)=QLGRS(11,IY)
    z[39] = dfd["QLGRS"].loc[MNUMCR] * TRIL_TO_QUAD

    # Marketed Renewables (wood) 10/
    # T4(40,IY,IS)=RSHTRCON(IY,6)/1000000000.
    z[40] = dfd["RSHTRCON"].loc[6] / 1000000000
    # Energy Consumption by End Use 1/
    # Space Heating
    # T4(42,IY,IS)=(RSHTRCON(IY,1)+RSHTRCON(IY,2)+RSHTRCON(IY,3)+RSHTRCON(IY,4)+RSHTRCON(IY,5)+RSHTRCON(IY,6)+RSHTRCON(IY,8))/1000000000.
    z[42] = (
        dfd["RSHTRCON"].loc[1]
        + dfd["RSHTRCON"].loc[2]
        + dfd["RSHTRCON"].loc[3]
        + dfd["RSHTRCON"].loc[4]
        + dfd["RSHTRCON"].loc[5]
        + dfd["RSHTRCON"].loc[6]
        + dfd["RSHTRCON"].loc[8]
    ) / 1000000000
    # Space Cooling
    # T4(43,IY,IS)=(RSCOOLCN(IY,1)+RSCOOLCN(IY,3))/1000000000.
    z[43] = (dfd["RSCOOLCN"].loc[1] + dfd["RSCOOLCN"].loc[3]) / 1000000000
    # Water Heating
    # T4(44,IY,IS)=(RSH2OCON(IY,1)+RSH2OCON(IY,2)+RSH2OCON(IY,3)+RSH2OCON(IY,4))/1000000000.
    z[44] = (
        dfd["RSH2OCON"].loc[1]
        + dfd["RSH2OCON"].loc[2]
        + dfd["RSH2OCON"].loc[3]
        + dfd["RSH2OCON"].loc[4]
    ) / 1000000000
    # Refrigeration
    # T4(45,IY,IS)=RSREFCON(IY)/1000000000.
    z[45] = dfd["RSREFCON"] / 1000000000
    # Cooking	46
    # T4(46,IY,IS)=(RSCKCON(IY,1)+RSCKCON(IY,2)+RSCKCON(IY,3))/1000000000.
    z[46] = (
        dfd["RSCKCON"].loc[1] + dfd["RSCKCON"].loc[2] + dfd["RSCKCON"].loc[3]
    ) / 1000000000
    # Clothes Dryers	47
    # T4(47,IY,IS)=(RSDRYCON(IY,1)+RSDRYCON(IY,2))/1000000000.
    z[47] = (dfd["RSDRYCON"].loc[1] + dfd["RSDRYCON"].loc[2]) / 1000000000
    # Freezers	48
    # T4(48,IY,IS)=RSFRZCON(IY)/1000000000.
    z[48] = dfd["RSFRZCON"] / 1000000000
    # Lighting	49
    # T4(49,IY,IS)=RSLTCON(IY)/1000000000.
    z[49] = dfd["RSLTCON"] / 1000000000
    # Clothes Washers 2/	50
    # T4(50,IY,IS)=RSCSWCON(IY)/1000000000.
    z[50] = dfd["RSCSWCON"] / 1000000000
    # Dishwashers 2/	51
    # T4(51,IY,IS)=RSDSWCON(IY)/1000000000.
    z[51] = dfd["RSDSWCON"] / 1000000000
    # Televisions and Related Equipment 3/
    # T4(52,IY,IS)=RSTVRCON(IY)/1000000000.
    z[52] = dfd["RSTVRCON"] / 1000000000

    # Computers and Related Equipment 4/
    # T4(53,IY,IS)=RSPCRCON(IY)/1000000000.
    z[53] = dfd["RSPCRCON"] / 1000000000
    # Furnace Fans and Boiler Circulation Pumps
    # T4(54,IY,IS)=RSFANCON(IY)/1000000000.
    z[54] = dfd["RSFANCON"] / 1000000000
    # Other Uses 11/
    # T4(55,IY,IS)=(RSAPCON(IY,1)+RSAPCON(IY,2)+RSAPCON(IY,3)+RSAPCON(IY,4))/1000000000.
    z[55] = (
        dfd["RSAPCON"].loc[1]
        + dfd["RSAPCON"].loc[2]
        + dfd["RSAPCON"].loc[3]
        + dfd["RSAPCON"].loc[4]
    ) / 1000000000
    # Gross End-use Consumption
    # T4(56,IY,IS)=FSUM(T4(42,IY,IS),14)
    z[56] = (
        z[42]
        + z[43]
        + z[44]
        + z[45]
        + z[46]
        + z[47]
        + z[48]
        + z[49]
        + z[50]
        + z[51]
        + z[52]
        + z[53]
        + z[54]
        + z[55]
    )

    # Purchased Electricity for Electric Vehicle Charging
    z[115] = QELRS_EV * TRIL_TO_QUAD

    # On-site Generation for own use
    z[111] = z[109]
    # Delivered Energy
    z[112] = z[56] + z[115] - z[111]

    # Electricity Related Losses
    # T4(57,IY,IS)=QELRS(11,IY)/QELAS(11,IY)*ELECLOSS
    z[57] = dfd["QELRS"].loc[MNUMCR] / dfd["QELAS"].loc[MNUMCR] * ELECLOSS * TRIL_TO_QUAD

    # Total Energy Consumption by End Use 1/
    # Space Heating
    # T4(58,IY,IS)=(RSHTRCON(IY,1)+RSHTRCON(IY,2)+RSHTRCON(IY,3)+RSHTRCON(IY,4)+RSHTRCON(IY,5)+RSHTRCON(IY,6)+RSHTRCON(IY,8))/1000000000.+RSHTRCON(IY,2)/1000000000./QELAS(11,IY)*ELECLOSS
    # z[58]=(dfd['RSHTRCON'].loc[1]+dfd['RSHTRCON'].loc[2]+dfd['RSHTRCON'].loc[3]+dfd['RSHTRCON'].loc[4]+dfd['RSHTRCON'].loc[5]+dfd['RSHTRCON'].loc[6]+dfd['RSHTRCON'].loc[8])/1000000000+dfd['RSHTRCON'].loc[2]/1000000000/dfd['QELAS'].loc[MNUMCR]*ELECLOSS
    z[58] = z[42] + z[9] / z[110]  * z[57] * ((z[23] - z[109]) / z[23])
    # Space Cooling
    # T4(59,IY,IS)=(RSCOOLCN(IY,1)+RSCOOLCN(IY,3))/1000000000.+RSCOOLCN(IY,1)/1000000000./QELAS(11,IY)*ELECLOSS
    # z[59]=(dfd['RSCOOLCN'].loc[1]+dfd['RSCOOLCN'].loc[3])/ 1000000000 + dfd['RSCOOLCN'].loc[1] / 1000000000 / dfd['QELAS'].loc[MNUMCR]*ELECLOSS
    z[59] = z[43] + z[10] / z[110]  * z[57] * ((z[23] - z[109]) / z[23])
    # Water Heating
    # T4(60,IY,IS)=(RSH2OCON(IY,1)+RSH2OCON(IY,2)+RSH2OCON(IY,3)+RSH2OCON(IY,4))/1000000000.+RSH2OCON(IY,2)/1000000000./QELAS(11,IY)*ELECLOSS
    # z[60]=(dfd['RSH2OCON'].loc[1]+dfd['RSH2OCON'].loc[2]+dfd['RSH2OCON'].loc[3]+dfd['RSH2OCON'].loc[4])/ 1000000000 + dfd['RSH2OCON'].loc[2] /1000000000 /  dfd['QELAS'].loc[MNUMCR]*ELECLOSS
    z[60] = z[44] + z[11] / z[110]  * z[57] * ((z[23] - z[109]) / z[23])
    # Refrigeration
    # T4(61,IY,IS)=RSREFCON(IY)/1000000000.+RSREFCON(IY)/1000000000./QELAS(11,IY)*ELECLOSS
    z[61] = z[45] + z[45] / z[110]  * z[57] * ((z[23] - z[109]) / z[23])
    # Cooking
    # T4(62,IY,IS)=(RSCKCON(IY,1)+RSCKCON(IY,2)+RSCKCON(IY,3))/1000000000.+RSCKCON(IY,3)/1000000000./QELAS(11,IY)*ELECLOSS
    z[62] = z[46] + z[13] / z[110]  * z[57] * ((z[23] - z[109]) / z[23])
    # Clothes Dryers
    # T4(63,IY,IS)=(RSDRYCON(IY,1)+RSDRYCON(IY,2))/1000000000.+RSDRYCON(IY,2)/1000000000./QELAS(11,IY)*ELECLOSS
    z[63] = z[47] + z[14] / z[110]  * z[57]  * ((z[23] - z[109]) / z[23])
    # Freezers
    # T4(64,IY,IS)=RSFRZCON(IY)/1000000000.+RSFRZCON(IY)/1000000000./QELAS(11,IY)*ELECLOSS
    z[64] = z[48] + z[48] / z[110]  * z[57] * ((z[23] - z[109]) / z[23])
    # Lighting
    # T4(65,IY,IS)=RSLTCON(IY)/1000000000.+RSLTCON(IY)/1000000000./QELAS(11,IY)*ELECLOSS
    z[65] = z[49] + z[49] / z[110]  * z[57] * ((z[23] - z[109]) / z[23])
    # Clothes Washers 2/
    # T4(66,IY,IS)=RSCSWCON(IY)/1000000000.+RSCSWCON(IY)/1000000000./QELAS(11,IY)*ELECLOSS
    z[66] = z[50] + z[50] / z[110]  * z[57] * ((z[23] - z[109]) / z[23])
    # Dishwashers 2/
    # T4(67,IY,IS)=RSDSWCON(IY)/1000000000.+RSDSWCON(IY)/1000000000./QELAS(11,IY)*ELECLOSS
    z[67] = z[51] + z[51] / z[110]  * z[57] * ((z[23] - z[109]) / z[23])
    # Televisions and Related Equipment 3/
    # T4(68,IY,IS)=RSTVRCON(IY)/1000000000.+RSTVRCON(IY)/1000000000./QELAS(11,IY)*ELECLOSS
    z[68] = z[52] + z[52] / z[110]  * z[57] * ((z[23] - z[109]) / z[23])
    # Computers and Related Equipment 4/
    # T4(69,IY,IS)=RSPCRCON(IY)/1000000000.+RSPCRCON(IY)/1000000000./QELAS(11,IY)*ELECLOSS
    z[69] = z[53] + z[53] / z[110] * z[57] * ((z[23] - z[109]) / z[23])
    # Furnace Fans and Boiler Circulation Pumps
    # T4(70,IY,IS)=RSFANCON(IY)/1000000000.+RSFANCON(IY)/1000000000./QELAS(11,IY)*ELECLOSS
    z[70] = z[54] + z[54] / z[110]  * z[57] * ((z[23] - z[109]) / z[23])
    # Other Uses 11/
    # T4(71,IY,IS)=(RSAPCON(IY,1)+RSAPCON(IY,2)+RSAPCON(IY,3)+RSAPCON(IY,4))/1000000000.+RSAPCON(IY,2)/1000000000./QELAS(11,IY)*ELECLOSS
    z[71] = z[55] + z[22] / z[110]  * z[57] * ((z[23] - z[109]) / z[23])
    # Total Gross End-use Consumption
    # T4(72,IY,IS)=SUM(T4(58:71,IY,IS))
    z[72] = (
        z[58]
        + z[59]
        + z[60]
        + z[61]
        + z[62]
        + z[63]
        + z[64]
        + z[65]
        + z[66]
        + z[67]
        + z[68]
        + z[69]
        + z[70]
        + z[71]
    )

    # Purchased Electricity for Electric Vehicle Charging
    z[117] = z[115] + z[115] / dfd["QELAS"].loc[MNUMCR] * ELECLOSS #z[117] = z[115] + z[115] / (z[23] + z[116]) * z[57]
    # Generation for own use
    z[113] = z[109]
    # Total Net Own-use Generation
    z[114] = z[72] + z[117] - z[113]
    # Nonmarketed Renewables 12/
    ## Solar Hot Water Heating
    # T4(74,IY,IS)=RSH2OCON(IY,5)/1000000000.
    z[74] = dfd["RSH2OCON"].loc[5] / 1000000000
    # Solar Photovoltaic
    # T4(8,IY,IS)=QPVRS(11,IY)
    z[8] = dfd["QPVRS"].loc[MNUMCR] * TRIL_TO_QUAD
    ## Wind
    # T4(75,IY,IS)=CGRESQ(11,IY,11)/1000.
    z[75] = dfd["CGRESQ"].loc[MNUMCR].loc[11] / 1000
    # Total
    # T4(78,IY,IS)=FSUM(T4(73,IY,IS),3)+T4(8,IY,IS)
    z[78] = z[74] + z[75] + z[8]

    # Energy Intensity
    # (million Btu per household)
    # Gross End-use Consumption 1/
    z[6] = (
        (
            z[42]
            + z[43]
            + z[55]
            + z[44]
            + z[47]
            + z[46]
            + z[12]
            + z[15]
            + z[16]
            + z[17]
            + z[18]
            + z[19]
            + z[20]
            + z[21]
        )
        / z[4]
        * 1000
    )
    # Delivered Energy Consumption
    # T4(7,IY,IS)=T4(6,IY,IS)-(CGRESGEN(11,IY,3,2)+	CGRESGEN(11,IY,8,2)+CGRESGEN(11,IY,11,2))*3412./(T4(4,IY,IS)*1000000.)
    # (thousand Btu per square foot)
    z[7] = z[6] - (
        dfd["CGRESGEN"].loc[MNUMCR].loc[3].loc[2]
        + dfd["CGRESGEN"].loc[MNUMCR].loc[8].loc[2]
        + dfd["CGRESGEN"].loc[MNUMCR].loc[11].loc[2]
    ) * WattHr_to_BTU_rwpre * 1000 / (z[4] * 1000000)
    # Gross End-use Consumption 1/	76
    # T4(76,IY,IS)=T4(6,IY,IS)/T4(5,IY,IS)*1000.
    z[76] = z[6] / z[5] * 1000
    # Delivered Energy Consumption	77
    # T4(77,IY,IS)=T4(7,IY,IS)/T4(5,IY,IS)*1000.
    z[77] = z[7] / z[5] * 1000

    # Bonus Rows - Internal Only
    # Electricity Prices by End Use
    # Space Heating
    # T4(99,IY,IS)=PELSHRS(11,IY)
    z[99] = dfd["PELSHRS"].loc[MNUMCR] * SCALPR2
    # Space Cooling
    # T4(100,IY,IS)=PELCLRS(11,IY)
    z[100] = dfd["PELCLRS"].loc[MNUMCR] * SCALPR2
    # Water Heating
    # T4(101,IY,IS)=PELWHRS(11,IY)
    z[101] = dfd["PELWHRS"].loc[MNUMCR] * SCALPR2
    # Refrigeration
    # T4(104,IY,IS)=PELRFRS(11,IY)
    z[104] = dfd["PELRFRS"].loc[MNUMCR] * SCALPR2
    # Cooking
    # T4(102,IY,IS)=PELCKRS(11,IY)
    z[102] = dfd["PELCKRS"].loc[MNUMCR] * SCALPR2
    # Clothes Dryers
    # T4(103,IY,IS)=PELCDRS(11,IY)
    z[103] = dfd["PELCDRS"].loc[MNUMCR] * SCALPR2
    # Freezers
    # T4(105,IY,IS)=PELFZRS(11,IY)
    z[105] = dfd["PELFZRS"].loc[MNUMCR] * SCALPR2
    # Lighting
    # T4(106,IY,IS)=PELLTRS(11,IY)
    z[106] = dfd["PELLTRS"].loc[MNUMCR] * SCALPR2
    # Appliances/Other
    # T4(107,IY,IS)=PELOTRS(11,IY)
    z[107] = dfd["PELOTRS"].loc[MNUMCR] * SCALPR2
    # Secondary Heating
    # T4(108,IY,IS)=PELH2RS(11,IY)
    z[108] = dfd["PELH2RS"].loc[MNUMCR] * SCALPR2
    # Electric Vehicle Charging
    z[118] = dfd["PELVHRS"].loc[MNUMCR] * SCALPR2
    # End Bonus Row Section

    # Heating Degree Days
    # T4(79:87,IY,IS)=HDDADJ(IY+1989,1:9)
    # New England
    z[79] = dfd["HDDADJ"].loc[1]
    # Middle Atlantic
    z[80] = dfd["HDDADJ"].loc[2]
    # East North Central
    z[81] = dfd["HDDADJ"].loc[3]
    # West North Central
    z[82] = dfd["HDDADJ"].loc[4]
    # South Atlantic
    z[83] = dfd["HDDADJ"].loc[5]
    # East South Central
    z[84] = dfd["HDDADJ"].loc[6]
    # West South Central
    z[85] = dfd["HDDADJ"].loc[7]
    # Mountain
    z[86] = dfd["HDDADJ"].loc[8]
    # Pacific
    z[87] = dfd["HDDADJ"].loc[9]
    # United States
    z[88] = dfd["HDDADJ"].loc[11]

    # Cooling Degree Days
    # T4(89:97,IY,IS)=CDDADJ(IY+1989,1:9)
    # New England
    z[89] = dfd["CDDADJ"].loc[1]
    # Middle Atlantic
    z[90] = dfd["CDDADJ"].loc[2]
    # East North Central	91
    z[91] = dfd["CDDADJ"].loc[3]
    # West North Central	92
    z[92] = dfd["CDDADJ"].loc[4]
    # South Atlantic
    z[93] = dfd["CDDADJ"].loc[5]
    # East South Central
    z[94] = dfd["CDDADJ"].loc[6]
    # West South Central
    z[95] = dfd["CDDADJ"].loc[7]
    # Mountain
    z[96] = dfd["CDDADJ"].loc[8]
    # Pacific
    z[97] = dfd["CDDADJ"].loc[9]
    # United States
    z[98] = dfd["CDDADJ"].loc[11]

    return z
