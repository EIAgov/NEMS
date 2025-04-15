# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_099(dfd, table_spec, table_id):
    """Fill table   Coal Minemouth Prices by Region and Type

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

    NSREG = dfd["NSREG_rwpre"]

    #   Coal Minemouth Prices by Region and Type
    #   (#### dollars per short ton)
    #    Supply Regions and Coal Types
    #
    #   Northern Appalachia 1/ 2/
    #   All Sulfur Categories (Premium)  4/
    # T99(1,IY,IS)=(PCLSULF(1,1,1,IY))+PCLSULF(1,4,1,IY))/(CLSULF(1,1,1,IY)+CLSULF(1,4,1,IY))

    z[1] = (
        (
            dfd["PCLSULF"].loc[1].loc[1].loc[1]
            + dfd["PCLSULF"].loc[1].loc[4].loc[1]
        )
        / (
            dfd["CLSULF"].loc[1].loc[1].loc[1]
            + dfd["CLSULF"].loc[1].loc[4].loc[1]
        )
    ) * SCALPR2

    #   All Sulfur Categories (Premium)  4/
    # check code for conditionals ##############
    # T99(2,IY,IS)=PCLSULF(1,4,1,IY)/CLSULF(1,4,1,IY)

    z[2] = (
        dfd["PCLSULF"].loc[1].loc[4].loc[1]
        / dfd["CLSULF"].loc[1].loc[4].loc[1].replace(0, 1)
    ) * SCALPR2

    #       All Sulfur Categories (Bituminous) 
    # TT99(3,IY,IS)=PCLSULF(1,1,1,IY)/CLSULF(1,1,1,IY)
    z[3] = (
        dfd["PCLSULF"].loc[1].loc[1].loc[1]
        / dfd["CLSULF"].loc[1].loc[1].loc[1].replace(0, 1)
        * SCALPR2
    )

    #   Rest of Appalachia 3/
    # T99(7,IY,IS)=(PCLSULF(2,1,1,IY))+PCLSULF(2,4,1,IY))/(CLSULF(2,1,1,IY)+CLSULF(2,4,1,IY))
    z[7] = (
        (
            dfd["PCLSULF"].loc[2].loc[1].loc[1]
            + dfd["PCLSULF"].loc[2].loc[4].loc[1]
        )
        / (
            dfd["CLSULF"].loc[2].loc[1].loc[1]
            + dfd["CLSULF"].loc[2].loc[4].loc[1]
        ).replace(0, 1)
        * SCALPR2
    )

    #       All Sulfur Categories (Premium)  4/
    # T99(8,IY,IS)=PCLSULF(2,4,1,IY)/CLSULF(2,4,1,IY)
    z[8] = (
        dfd["PCLSULF"].loc[2].loc[4].loc[1]
        / dfd["CLSULF"].loc[2].loc[4].loc[1].replace(0, 1)
        * SCALPR2
    )

    #       All Sulfur Categories (Bituminous) 
    # T99(9,IY,IS)=PCLSULF(2,1,1,IY)/CLSULF(2,1,1,IY)
    z[9] = (
        dfd["PCLSULF"].loc[2].loc[1].loc[1]
        / dfd["CLSULF"].loc[2].loc[1].loc[1].replace(0, 1)
        * SCALPR2
    )

    #   Interior 6/
    # T99(15,IY,IS)=PCLSULF(3,1,1,IY)/CLSULF(3,1,1,IY)
    z[15] = (
        (
            dfd["PCLSULF"].loc[3].loc[1].loc[1]
        )
        / (
            dfd["CLSULF"].loc[3].loc[1].loc[1]
        ).replace(0, 1)
        * SCALPR2
    )

    #   All Sulfur Categories (Bituminous) 
    # T99(17,IY,IS)=PCLSULF(3,1,1,IY)/CLSULF(3,1,1,IY)
    z[17] = (
        dfd["PCLSULF"].loc[3].loc[1].loc[1]
        / dfd["CLSULF"].loc[3].loc[1].loc[1].replace(0, 1)
        * SCALPR2
    )

    #   All Lignite
    # T99(20,IY,IS)=PCLSULF(4,3,1,IY)/CLSULF(4,3,1,IY)
    z[20] = (
        (dfd["PCLSULF"].loc[4].loc[3].loc[1])
        / (dfd["CLSULF"].loc[4].loc[3].loc[1]).replace(0, 1)
        * SCALPR2
    )

    #    All Sulfur Categories (Lignite)
    # T99(21,IY,IS)=PCLSULF(4,3,1,IY)/CLSULF(4,3,1,IY)
    z[21] = (
        dfd["PCLSULF"].loc[4].loc[3].loc[1]
        / dfd["CLSULF"].loc[4].loc[3].loc[1].replace(0, 1)
        * SCALPR2
    )
    #   Wyoming, Powder River Basin 6/
    # T99(28,IY,IS)=PCLSULF(5,2,1,IY)/CLSULF(5,2,1,IY)
    z[28] = (
        (dfd["PCLSULF"].loc[5].loc[2].loc[1])
        / (dfd["CLSULF"].loc[5].loc[2].loc[1]).replace(0, 1)
        * SCALPR2
    )

    #       All Sulfur Categories (Subbituminous)
    # T99(29,IY,IS)=PCLSULF(5,2,1,IY)/CLSULF(5,2,1,IY)
    z[29] = (
        (dfd["PCLSULF"].loc[5].loc[2].loc[1])
        / (dfd["CLSULF"].loc[5].loc[2].loc[1]).replace(0, 1)
        * SCALPR2
    )


    #   Western Region 7/
    # T99(36,IY,IS)=(PCLSULF(6,1,1,IY)+PCLSULF(6,2,1,IY))/(CLSULF(6,1,1,IY)+CLSULF(6,2,1,IY))
    z[36] = (
        (
            dfd["PCLSULF"].loc[6].loc[1].loc[1]
            + dfd["PCLSULF"].loc[6].loc[2].loc[1]
        )
        / (
            dfd["CLSULF"].loc[6].loc[1].loc[1]
            + dfd["CLSULF"].loc[6].loc[2].loc[1]
        ).replace(0, 1)
        * SCALPR2
    )


    #       All Sulfur Categories (Bituminous)
    # T99(37,IY,IS)=PCLSULF(6,1,1,IY)/CLSULF(6,1,1,IY)
    z[37] = (
        dfd["PCLSULF"].loc[6].loc[1].loc[1]
        / dfd["CLSULF"].loc[6].loc[1].loc[1].replace(0, 1)
        * SCALPR2
    )

    #       All Sulfur Categories (Subbituminous)
    # T99(38,IY,IS)=PCLSULF(6,2,1,IY)/CLSULF(6,2,1,IY)
    z[38] = (
        dfd["PCLSULF"].loc[6].loc[2].loc[1]
        / dfd["CLSULF"].loc[6].loc[2].loc[1].replace(0, 1)
        * SCALPR2
    )

    #   Average by Type:  All Regions

    #     Premium Metallurgical 3/
    # T99(44,IY,IS)=(PAPSULF(4,IY))/(APSULF(4,IY))
    z[44] = (
        (dfd["PAPSULF"].loc[4])
        / (dfd["APSULF"].loc[4]).replace(0, 1)
        * SCALPR2
    )

    #     Bituminous
    # T99(45,IY,IS)=(PABSULF(4,IY)+PIBSULF(4,IY)+PWBSULF(4,IY))/(ABSULF(4,IY)+IBSULF(4,IY)+WBSULF(4,IY))
    z[45] = (
        (dfd["PABSULF"].loc[4] + dfd["PIBSULF"].loc[4] + dfd["PWBSULF"].loc[4])
        / (dfd["ABSULF"].loc[4] + dfd["IBSULF"].loc[4] + dfd["WBSULF"].loc[4]).replace(
            0, 1
        )
        * SCALPR2
    )

    #     Subbituminous
    # T99(46,IY,IS)=PWSSULF(4,IY)/WSSULF(4,IY)
    z[46] = dfd["PWSSULF"].loc[4] / dfd["WSSULF"].loc[4] * SCALPR2

    #     Lignite
    # T99(47,IY,IS)=(PILSULF(4,IY)+PWLSULF(4,IY))/(ILSULF(4,IY)+WLSULF(4,IY))
    z[47] = (
        (dfd["PILSULF"].loc[4] + dfd["PWLSULF"].loc[4])
        / (dfd["ILSULF"].loc[4] + dfd["WLSULF"].loc[4]).replace(0, 1)
        * SCALPR2
    )

    #     All Sulfur Categories
    # T99(48,IY,IS)=(PABSULF(1,IY)
    # +PIBSULF(1,IY)
    # +PILSULF(1,IY)
    # +PWPSULF(1,IY)
    # +PWBSULF(1,IY)
    # +PWSSULF(1,IY)
    # +PWLSULF(1,IY)
    # +PAPSULF(1,IY))
    # /
    # (ABSULF(1,IY)
    # +IBSULF(1,IY)
    # +ILSULF(1,IY)
    # +WPSULF(1,IY)
    # +WBSULF(1,IY)
    # +WSSULF(1,IY)
    # +WLSULF(1,IY)+APSULF(1,IY))
    z[48] = (
        (
            dfd["PABSULF"].loc[1]
            + dfd["PIBSULF"].loc[1]
            + dfd["PILSULF"].loc[1]
            + dfd["PWPSULF"].loc[1]
            + dfd["PWBSULF"].loc[1]
            + dfd["PWSSULF"].loc[1]
            + dfd["PWLSULF"].loc[1]
            + dfd["PAPSULF"].loc[1]
        )
        / (
            dfd["ABSULF"].loc[1]
            + dfd["IBSULF"].loc[1]
            + dfd["ILSULF"].loc[1]
            + dfd["WPSULF"].loc[1]
            + dfd["WBSULF"].loc[1]
            + dfd["WSSULF"].loc[1]
            + dfd["WLSULF"].loc[1]
            + dfd["APSULF"].loc[1]
        ).replace(0, 1)
        * SCALPR2
    )

    #     Underground
    # T99(51,IY,IS)=SUM(PMTDP(1:NSREG,IY))/SUM(PMTD(1:NSREG,IY))
    z[51] = dfd["PMTDP"].loc[1:NSREG].sum() / dfd["PMTD"].loc[1:NSREG].sum() * SCALPR2

    #     Surface
    # T99(52,IY,IS)=(SUM(PMTSP(1:NSREG,IY))-PCLSULF(1,3,3,IY))/(SUM(PMTS(1:NSREG,IY))-CLSULF(1,3,3,IY))
    z[52] = (
        (dfd["PMTSP"].loc[1:NSREG].sum() - dfd["PCLSULF"].loc[1].loc[3].loc[3])
        / (dfd["PMTS"].loc[1:NSREG].sum() - dfd["CLSULF"].loc[1].loc[3].loc[3]).replace(
            0, 1
        )
        * SCALPR2
    )

    #   United States Total 8/
    # T99(53,IY,IS)=(PABSULF(4,IY)+PIBSULF(4,IY)+PILSULF(4,IY)+PWPSULF(4,IY)+PWBSULF(4,IY)+PWSSULF(4,IY)+PWLSULF(4,IY)+PAPSULF(4,IY))/(ABSULF(4,IY)+IBSULF(4,IY)+ILSULF(4,IY)+WPSULF(4,IY)+WBSULF(4,IY)+WSSULF(4,IY)+WLSULF(4,IY)+APSULF(4,IY))
    z[53] = (
        (
            dfd["PABSULF"].loc[4]
            + dfd["PIBSULF"].loc[4]
            + dfd["PILSULF"].loc[4]
            + dfd["PWPSULF"].loc[4]
            + dfd["PWBSULF"].loc[4]
            + dfd["PWSSULF"].loc[4]
            + dfd["PWLSULF"].loc[4]
            + dfd["PAPSULF"].loc[4]
        )
        / (
            dfd["ABSULF"].loc[4]
            + dfd["IBSULF"].loc[4]
            + dfd["ILSULF"].loc[4]
            + dfd["WPSULF"].loc[4]
            + dfd["WBSULF"].loc[4]
            + dfd["WSSULF"].loc[4]
            + dfd["WLSULF"].loc[4]
            + dfd["APSULF"].loc[4]
        ).replace(0, 1)
        * SCALPR2
    )

    #   Waste Coal
    # T99(6,IY,IS)=PCLSULF(1,3,3,IY)/CLSULF(1,3,3,IY)
    z[6] = (
        dfd["PCLSULF"].loc[1].loc[3].loc[1]
        / dfd["CLSULF"].loc[1].loc[3].loc[1].replace(0, 1)
        * SCALPR2
    )

    return z
