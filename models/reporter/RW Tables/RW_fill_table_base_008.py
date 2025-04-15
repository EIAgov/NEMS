# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
Fixed by SZO on 7/8/20124
"""


def fill_table_base_008(dfd, table_spec, table_id):
    """Fill table  Electricity Supply, Disposition, Prices, and Emissions

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

    MNUMNR = dfd["MNUMNR_rwpre"]
    NUTSEC = dfd["NUTSEC_rwpre"]
    NDREG = dfd["NDREG_rwpre"]

    #   Electricity Supply, Disposition, Prices, and Emissions
    #   (billion kilowatthours, unless otherwise noted)
    #    Supply, Disposition, Prices, and Emissions

    #   Net Generation by Fuel Type

    #   Electric Power Sector 1/

    #     Power Only 2/

    #       Coal
    # T8(1,IY,IS)=UGNCLNR(1,MNUMNR,IY)+UGNCLNR(2,MNUMNR,IY)
    z[1] = dfd["UGNCLNR"].loc[1].loc[MNUMNR] + dfd["UGNCLNR"].loc[2].loc[MNUMNR]

    #       Petroleum
    # T8(2,IY,IS)=UGNDSNR(1,MNUMNR,IY)+UGNDSNR(2,MNUMNR,IY)+UGNRHNR(1,MNUMNR,IY)+UGNRHNR(2,MNUMNR,IY)+UGNRLNR(1,MNUMNR,IY)+UGNRLNR(2,MNUMNR,IY)
    z[2] = (
        dfd["UGNDSNR"].loc[1].loc[MNUMNR]
        + dfd["UGNDSNR"].loc[2].loc[MNUMNR]
        + dfd["UGNRHNR"].loc[1].loc[MNUMNR]
        + dfd["UGNRHNR"].loc[2].loc[MNUMNR]
        + dfd["UGNRLNR"].loc[1].loc[MNUMNR]
        + dfd["UGNRLNR"].loc[2].loc[MNUMNR]
    )

    #       Natural Gas 3/
    # T8(3,IY,IS)=UGNGFNR(1,MNUMNR,IY)+UGNGFNR(2,MNUMNR,IY)+UGNGINR(1,MNUMNR,IY)+UGNGINR(2,MNUMNR,IY)
    z[3] = (
        dfd["UGNGFNR"].loc[1].loc[MNUMNR]
        + dfd["UGNGFNR"].loc[2].loc[MNUMNR]
        + dfd["UGNGINR"].loc[1].loc[MNUMNR]
        + dfd["UGNGINR"].loc[2].loc[MNUMNR]
    )

    #       Nuclear Power
    # T8(4,IY,IS)=UGNURNR(1,MNUMNR,IY)+UGNURNR(2,MNUMNR,IY)
    z[4] = dfd["UGNURNR"].loc[1].loc[MNUMNR] + dfd["UGNURNR"].loc[2].loc[MNUMNR]

    #       Pumped Storage/Other 4/
    # T8(5,IY,IS)=UGNPSNR(1,MNUMNR,IY)+UGNPSNR(2,MNUMNR,IY)+UGNSDNR(1,MNUMNR,IY)+UGNSDNR(2,MNUMNR,IY)
    z[5] = (
        dfd["UGNPSNR"].loc[1].loc[MNUMNR]
        + dfd["UGNPSNR"].loc[2].loc[MNUMNR]
        + dfd["UGNSDNR"].loc[1].loc[MNUMNR]
        + dfd["UGNSDNR"].loc[2].loc[MNUMNR]
    )

    #       Hydrogen
    # T8(37,IY,IS)=UGNGCNR(1,MNUMNR,IY)+UGNGCNR(2,MNUMNR,IY)
    z[37] = dfd["UGNGCNR"].loc[1].loc[MNUMNR] + dfd["UGNGCNR"].loc[2].loc[MNUMNR]

    #       Renewable Sources 5/
    # T8(6,IY,IS)=UGNHYNR(1,MNUMNR,IY)+UGNHYNR(2,MNUMNR,IY)+UGNGENR(1,MNUMNR,IY)+UGNGENR(2,MNUMNR,IY)+UGNMSNR(1,MNUMNR,IY)+UGNMSNR(2,MNUMNR,IY)+UGNWDNR(1,MNUMNR,IY)+UGNWDNR(2,MNUMNR,IY)+UGNSONR(1,MNUMNR,IY)+UGNSONR(2,MNUMNR,IY)+UGNPVNR(1,MNUMNR,IY)+UGNPVNR(2,MNUMNR,IY)+UGNPTNR(1,MNUMNR,IY)+UGNPTNR(2,MNUMNR,IY)+UGNWNNR(1,MNUMNR,IY)+UGNWNNR(2,MNUMNR,IY)+UGNWLNR(1,MNUMNR,IY)+UGNWLNR(2,MNUMNR,IY)+UGNWFNR(1,MNUMNR,IY)+UGNWFNR(2,MNUMNR,IY)
    z[6] = (
        dfd["UGNHYNR"].loc[1].loc[MNUMNR]
        + dfd["UGNHYNR"].loc[2].loc[MNUMNR]
        + dfd["UGNGENR"].loc[1].loc[MNUMNR]
        + dfd["UGNGENR"].loc[2].loc[MNUMNR]
        + dfd["UGNMSNR"].loc[1].loc[MNUMNR]
        + dfd["UGNMSNR"].loc[2].loc[MNUMNR]
        + dfd["UGNWDNR"].loc[1].loc[MNUMNR]
        + dfd["UGNWDNR"].loc[2].loc[MNUMNR]
        + dfd["UGNSONR"].loc[1].loc[MNUMNR]
        + dfd["UGNSONR"].loc[2].loc[MNUMNR]
        + dfd["UGNPVNR"].loc[1].loc[MNUMNR]
        + dfd["UGNPVNR"].loc[2].loc[MNUMNR]
        + dfd["UGNPTNR"].loc[1].loc[MNUMNR]
        + dfd["UGNPTNR"].loc[2].loc[MNUMNR]
        + dfd["UGNWNNR"].loc[1].loc[MNUMNR]
        + dfd["UGNWNNR"].loc[2].loc[MNUMNR]
        + dfd["UGNWLNR"].loc[1].loc[MNUMNR]
        + dfd["UGNWLNR"].loc[2].loc[MNUMNR]
        + dfd["UGNWFNR"].loc[1].loc[MNUMNR]
        + dfd["UGNWFNR"].loc[2].loc[MNUMNR]
    )

    #       Distributed Generation (Natural Gas)
    # T8(41,IY,IS)=UGNDDNR(1,MNUMNR,IY)+UGNDDNR(2,MNUMNR,IY)+UGNDGNR(1,MNUMNR,IY)+UGNDGNR(2,MNUMNR,IY)
    z[41] = (
        dfd["UGNDDNR"].loc[1].loc[MNUMNR]
        + dfd["UGNDDNR"].loc[2].loc[MNUMNR]
        + dfd["UGNDGNR"].loc[1].loc[MNUMNR]
        + dfd["UGNDGNR"].loc[2].loc[MNUMNR]
    )

    #         Total
    # T8(8,IY,IS)=FSUM(T8(1,IY,IS),6)+T8(37,IY,IS)+T8(41,IY,IS)
    z[8] = z[1] + z[2] + z[3] + z[4] + z[5] + z[6] + z[37] + z[41]

    #     Combined Heat and Power 6/

    #       Coal
    # T8(42,IY,IS)=(CGNTGEN(MNUMNR,IY,1,1)+CGNTGEN(MNUMNR,IY,1,2))*0.001
    z[42] = (
        dfd["CGNTGEN"].loc[MNUMNR].loc[1].loc[1]
        + dfd["CGNTGEN"].loc[MNUMNR].loc[1].loc[2]
    ) * 0.001

    #       Petroleum
    # T8(43,IY,IS)=(CGNTGEN(MNUMNR,IY,2,1)+CGNTGEN(MNUMNR,IY,2,2))*0.001
    z[43] = (
        dfd["CGNTGEN"].loc[MNUMNR].loc[2].loc[1]
        + dfd["CGNTGEN"].loc[MNUMNR].loc[2].loc[2]
    ) * 0.001

    #       Natural Gas
    # T8(44,IY,IS)=(CGNTGEN(MNUMNR,IY,3,1)+CGNTGEN(MNUMNR,IY,3,2))*0.001
    z[44] = (
        dfd["CGNTGEN"].loc[MNUMNR].loc[3].loc[1]
        + dfd["CGNTGEN"].loc[MNUMNR].loc[3].loc[2]
    ) * 0.001

    #       Renewable Sources
    # T8(46,IY,IS)=(CGNTGEN(MNUMNR,IY,4,1)+CGNTGEN(MNUMNR,IY,4,2)+CGNTGEN(MNUMNR,IY,5,1)+CGNTGEN(MNUMNR,IY,5,2)+CGNTGEN(MNUMNR,IY,6,1)+CGNTGEN(MNUMNR,IY,6,2)+CGNTGEN(MNUMNR,IY,7,1)+CGNTGEN(MNUMNR,IY,7,2)+CGNTGEN(MNUMNR,IY,8,1)+CGNTGEN(MNUMNR,IY,8,2))*0.001
    z[46] = (
        dfd["CGNTGEN"].loc[MNUMNR].loc[4].loc[1]
        + dfd["CGNTGEN"].loc[MNUMNR].loc[4].loc[2]
        + dfd["CGNTGEN"].loc[MNUMNR].loc[5].loc[1]
        + dfd["CGNTGEN"].loc[MNUMNR].loc[5].loc[2]
        + dfd["CGNTGEN"].loc[MNUMNR].loc[6].loc[1]
        + dfd["CGNTGEN"].loc[MNUMNR].loc[6].loc[2]
        + dfd["CGNTGEN"].loc[MNUMNR].loc[7].loc[1]
        + dfd["CGNTGEN"].loc[MNUMNR].loc[7].loc[2]
        + dfd["CGNTGEN"].loc[MNUMNR].loc[8].loc[1]
        + dfd["CGNTGEN"].loc[MNUMNR].loc[8].loc[2]
    ) * 0.001

    #       Other
    # T8(45,IY,IS)=(CGNTGEN(MNUMNR,IY,9,1)+CGNTGEN(MNUMNR,IY,9,2)+CGNTGEN(MNUMNR,IY,10,1)+CGNTGEN(MNUMNR,IY,10,2))*0.001
    z[45] = (
        dfd["CGNTGEN"].loc[MNUMNR].loc[9].loc[1]
        + dfd["CGNTGEN"].loc[MNUMNR].loc[9].loc[2]
        + dfd["CGNTGEN"].loc[MNUMNR].loc[10].loc[1]
        + dfd["CGNTGEN"].loc[MNUMNR].loc[10].loc[2]
    ) * 0.001

    #         Total
    # T8(48,IY,IS)=FSUM(T8(42,IY,IS),5)
    z[48] = z[42] + z[43] + z[44] + z[45] + z[46]

    #     Total Net Electric Power Sector Generation
    # T8(55,IY,IS)=T8(8,IY,IS)+T8(48,IY,IS)
    z[55] = z[8] + z[48]

    #     Less Direct Use
    # T8(47,IY,IS)=CGOTGEN(MNUMNR,IY,1)+CGOTGEN(MNUMNR,IY,2)
    z[47] = dfd["CGOTGEN"].loc[MNUMNR].loc[1] + dfd["CGOTGEN"].loc[MNUMNR].loc[2]

    #

    #     Net Available to the Grid
    # T8(49,IY,IS)=T8(8,IY,IS)+T8(48,IY,IS)-T8(47,IY,IS)
    z[49] = z[8] + z[48] - z[47]

    #

    #     End-Use Sector 7/

    #       Coal
    # T8(9,IY,IS)=(CGOGSGEN(11,IY,1,1)+CGOGSGEN(11,IY,1,2)+CGINDLGEN(11,IY,1,1)+CGINDLGEN(11,IY,1,2)+CGCOMMGEN(11,IY,1,1)+CGCOMMGEN(11,IY,1,2)+CGREFGEN(11,IY,1,1)+CGREFGEN(11,IY,1,2))*.001
    z[9] = (
        dfd["CGOGSGEN"].loc[11].loc[1].loc[1]
        + dfd["CGOGSGEN"].loc[11].loc[1].loc[2]
        + dfd["CGINDLGEN"].loc[11].loc[1].loc[1]
        + dfd["CGINDLGEN"].loc[11].loc[1].loc[2]
        + dfd["CGCOMMGEN"].loc[11].loc[1].loc[1]
        + dfd["CGCOMMGEN"].loc[11].loc[1].loc[2]
        + dfd["CGREFGEN"].loc[11].loc[1].loc[1]
        + dfd["CGREFGEN"].loc[11].loc[1].loc[2]
    ) * 0.001

    #       Petroleum
    # T8(10,IY,IS)=(CGREFGEN(11,IY,2,1)+CGREFGEN(11,IY,2,2)+CGOGSGEN(11,IY,2,1)+CGOGSGEN(11,IY,2,2)+CGINDLGEN(11,IY,2,1)+CGINDLGEN(11,IY,2,2)+CGCOMMGEN(11,IY,2,1)+CGCOMMGEN(11,IY,2,2))*.001
    z[10] = (
        dfd["CGREFGEN"].loc[11].loc[2].loc[1]
        + dfd["CGREFGEN"].loc[11].loc[2].loc[2]
        + dfd["CGOGSGEN"].loc[11].loc[2].loc[1]
        + dfd["CGOGSGEN"].loc[11].loc[2].loc[2]
        + dfd["CGINDLGEN"].loc[11].loc[2].loc[1]
        + dfd["CGINDLGEN"].loc[11].loc[2].loc[2]
        + dfd["CGCOMMGEN"].loc[11].loc[2].loc[1]
        + dfd["CGCOMMGEN"].loc[11].loc[2].loc[2]
    ) * 0.001

    #       Natural Gas
    # T8(11,IY,IS)=(CGREFGEN(11,IY,3,1)+CGREFGEN(11,IY,3,2)+CGOGSGEN(11,IY,3,1)+CGOGSGEN(11,IY,3,2)+CGINDLGEN(11,IY,3,1)+CGINDLGEN(11,IY,3,2)+CGCOMMGEN(11,IY,3,1)+CGCOMMGEN(11,IY,3,2)+CGRESGEN(11,IY,3,1)+CGRESGEN(11,IY,3,2))*.001
    z[11] = (
        dfd["CGREFGEN"].loc[11].loc[3].loc[1]
        + dfd["CGREFGEN"].loc[11].loc[3].loc[2]
        + dfd["CGOGSGEN"].loc[11].loc[3].loc[1]
        + dfd["CGOGSGEN"].loc[11].loc[3].loc[2]
        + dfd["CGINDLGEN"].loc[11].loc[3].loc[1]
        + dfd["CGINDLGEN"].loc[11].loc[3].loc[2]
        + dfd["CGCOMMGEN"].loc[11].loc[3].loc[1]
        + dfd["CGCOMMGEN"].loc[11].loc[3].loc[2]
        + dfd["CGRESGEN"].loc[11].loc[3].loc[1]
        + dfd["CGRESGEN"].loc[11].loc[3].loc[2]
    ) * 0.001

    #       Other Gaseous Fuels 8/
    # T8(12,IY,IS)=(CGREFGEN(11,IY,9,1)+CGREFGEN(11,IY,9,2)+CGINDLGEN(11,IY,9,1)+CGINDLGEN(11,IY,9,2)+CGCOMMGEN(11,IY,9,1)+CGCOMMGEN(11,IY,9,2))*.001
    z[12] = (
        dfd["CGREFGEN"].loc[11].loc[9].loc[1]
        + dfd["CGREFGEN"].loc[11].loc[9].loc[2]
        + dfd["CGINDLGEN"].loc[11].loc[9].loc[1]
        + dfd["CGINDLGEN"].loc[11].loc[9].loc[2]
        + dfd["CGCOMMGEN"].loc[11].loc[9].loc[1]
        + dfd["CGCOMMGEN"].loc[11].loc[9].loc[2]
    ) * 0.001

    #       Other 10/
    # T8(14,IY,IS)=(CGOGSGEN(11,IY,4,1)+CGOGSGEN(11,IY,4,2)+CGINDLGEN(11,IY,10,1)+CGINDLGEN(11,IY,10,2)+CGCOMMGEN(11,IY,10,1)+CGCOMMGEN(11,IY,10,2))*.001
    z[14] = (
        dfd["CGOGSGEN"].loc[11].loc[4].loc[1]
        + dfd["CGOGSGEN"].loc[11].loc[4].loc[2]
        + dfd["CGINDLGEN"].loc[11].loc[10].loc[1]
        + dfd["CGINDLGEN"].loc[11].loc[10].loc[2]
        + dfd["CGCOMMGEN"].loc[11].loc[10].loc[1]
        + dfd["CGCOMMGEN"].loc[11].loc[10].loc[2]
    ) * 0.001

    z[18] = (
        dfd["CGCOMMGEN"].loc[11].loc[8].loc[1]
        + dfd["CGCOMMGEN"].loc[11].loc[8].loc[2]
        + dfd["CGRESGEN"].loc[11].loc[8].loc[1]
        + dfd["CGRESGEN"].loc[11].loc[8].loc[2]
        + dfd["CGRESGEN"].loc[11].loc[11].loc[1]
        + dfd["CGRESGEN"].loc[11].loc[11].loc[2]
        + dfd["CGINDLGEN"].loc[11].loc[8].loc[1]
        + dfd["CGINDLGEN"].loc[11].loc[8].loc[2]
        + dfd["CGINDLGEN"].loc[11].loc[11].loc[1]
        + dfd["CGINDLGEN"].loc[11].loc[11].loc[2]
        + dfd["CGCOMMGEN"].loc[11].loc[4].loc[1]
        + dfd["CGCOMMGEN"].loc[11].loc[4].loc[2]
        + dfd["CGINDLGEN"].loc[11].loc[4].loc[1]
        + dfd["CGINDLGEN"].loc[11].loc[4].loc[2]
        + dfd["CGCOMMGEN"].loc[11].loc[5].loc[1]
        + dfd["CGCOMMGEN"].loc[11].loc[5].loc[2]
        + dfd["CGCOMMGEN"].loc[11].loc[11].loc[1]
        + dfd["CGCOMMGEN"].loc[11].loc[11].loc[2]
        + dfd["CGINDLGEN"].loc[11].loc[5].loc[1]
        + dfd["CGINDLGEN"].loc[11].loc[5].loc[2]
    ) * 0.001

    #       Renewable Sources 9/
    # T8(13,IY,IS)=(CGREFGEN(11,IY,6,1)+CGREFGEN(11,IY,6,2)+CGREFGEN(11,IY,7,1)+CGREFGEN(11,IY,7,2)+CGINDLGEN(11,IY,6,1)+CGINDLGEN(11,IY,7,1)+CGINDLGEN(11,IY,6,2)+CGINDLGEN(11,IY,7,2)+CGCOMMGEN(11,IY,6,1)+CGCOMMGEN(11,IY,7,1)+CGCOMMGEN(11,IY,6,2)+CGCOMMGEN(11,IY,7,2))*.001
    # T8(13,IY,IS) = T8(13,IY,IS) + T8(18,IY,IS)
    z[13] = (
        dfd["CGREFGEN"].loc[11].loc[6].loc[1]
        + dfd["CGREFGEN"].loc[11].loc[6].loc[2]
        + dfd["CGREFGEN"].loc[11].loc[7].loc[1]
        + dfd["CGREFGEN"].loc[11].loc[7].loc[2]
        + dfd["CGINDLGEN"].loc[11].loc[6].loc[1]
        + dfd["CGINDLGEN"].loc[11].loc[7].loc[1]
        + dfd["CGINDLGEN"].loc[11].loc[6].loc[2]
        + dfd["CGINDLGEN"].loc[11].loc[7].loc[2]
        + dfd["CGCOMMGEN"].loc[11].loc[6].loc[1]
        + dfd["CGCOMMGEN"].loc[11].loc[7].loc[1]
        + dfd["CGCOMMGEN"].loc[11].loc[6].loc[2]
        + dfd["CGCOMMGEN"].loc[11].loc[7].loc[2]
    ) * 0.001 + z[18]

    #         Total End-Use Sector Net Generation
    # T8(15,IY,IS) = FSUM(T8(9,IY,IS),6)
    z[15] = z[9] + z[10] + z[11] + z[12] + z[13] + z[14]

    #       Less Direct Use
    # T8(17,IY,IS)=CGREFGEN(11,IY,1,2)+CGREFGEN(11,IY,2,2)+CGREFGEN(11,IY,3,2)+CGREFGEN(11,IY,6,2)+CGREFGEN(11,IY,7,2)+CGREFGEN(11,IY,9,2)+CGINDLGEN(11,IY,1,2)+CGINDLGEN(11,IY,2,2)+CGINDLGEN(11,IY,3,2)+CGINDLGEN(11,IY,4,2)+CGINDLGEN(11,IY,5,2)+CGINDLGEN(11,IY,6,2)+CGINDLGEN(11,IY,7,2)+CGINDLGEN(11,IY,8,2)+CGINDLGEN(11,IY,9,2)+CGINDLGEN(11,IY,10,2)+CGINDLGEN(11,IY,11,2)+CGOGSGEN(11,IY,1,2)+CGOGSGEN(11,IY,2,2)+CGOGSGEN(11,IY,3,2)+CGOGSGEN(11,IY,4,2)+CGRESGEN(11,IY,3,2)+CGRESGEN(11,IY,8,2)+CGRESGEN(11,IY,11,2)+CGCOMMGEN(11,IY,1,2)+CGCOMMGEN(11,IY,2,2)+CGCOMMGEN(11,IY,3,2)+CGCOMMGEN(11,IY,4,2)+CGCOMMGEN(11,IY,5,2)+CGCOMMGEN(11,IY,6,2)+CGCOMMGEN(11,IY,7,2)+CGCOMMGEN(11,IY,8,2)+CGCOMMGEN(11,IY,9,2)+CGCOMMGEN(11,IY,10,2)+CGCOMMGEN(11,IY,11,2)+CGCOMMGEN(11,IY,12,2)
    z[17] = (
        dfd["CGREFGEN"].loc[11].loc[1].loc[2]
        + dfd["CGREFGEN"].loc[11].loc[2].loc[2]
        + dfd["CGREFGEN"].loc[11].loc[3].loc[2]
        + dfd["CGREFGEN"].loc[11].loc[6].loc[2]
        + dfd["CGREFGEN"].loc[11].loc[7].loc[2]
        + dfd["CGREFGEN"].loc[11].loc[9].loc[2]
        + dfd["CGINDLGEN"].loc[11].loc[1].loc[2]
        + dfd["CGINDLGEN"].loc[11].loc[2].loc[2]
        + dfd["CGINDLGEN"].loc[11].loc[3].loc[2]
        + dfd["CGINDLGEN"].loc[11].loc[4].loc[2]
        + dfd["CGINDLGEN"].loc[11].loc[5].loc[2]
        + dfd["CGINDLGEN"].loc[11].loc[6].loc[2]
        + dfd["CGINDLGEN"].loc[11].loc[7].loc[2]
        + dfd["CGINDLGEN"].loc[11].loc[8].loc[2]
        + dfd["CGINDLGEN"].loc[11].loc[9].loc[2]
        + dfd["CGINDLGEN"].loc[11].loc[10].loc[2]
        + dfd["CGINDLGEN"].loc[11].loc[11].loc[2]
        + dfd["CGOGSGEN"].loc[11].loc[1].loc[2]
        + dfd["CGOGSGEN"].loc[11].loc[2].loc[2]
        + dfd["CGOGSGEN"].loc[11].loc[3].loc[2]
        + dfd["CGOGSGEN"].loc[11].loc[4].loc[2]
        + dfd["CGRESGEN"].loc[11].loc[3].loc[2]
        + dfd["CGRESGEN"].loc[11].loc[8].loc[2]
        + dfd["CGRESGEN"].loc[11].loc[11].loc[2]
        + dfd["CGCOMMGEN"].loc[11].loc[1].loc[2]
        + dfd["CGCOMMGEN"].loc[11].loc[2].loc[2]
        + dfd["CGCOMMGEN"].loc[11].loc[3].loc[2]
        + dfd["CGCOMMGEN"].loc[11].loc[4].loc[2]
        + dfd["CGCOMMGEN"].loc[11].loc[5].loc[2]
        + dfd["CGCOMMGEN"].loc[11].loc[6].loc[2]
        + dfd["CGCOMMGEN"].loc[11].loc[7].loc[2]
        + dfd["CGCOMMGEN"].loc[11].loc[8].loc[2]
        + dfd["CGCOMMGEN"].loc[11].loc[9].loc[2]
        + dfd["CGCOMMGEN"].loc[11].loc[10].loc[2]
        + dfd["CGCOMMGEN"].loc[11].loc[11].loc[2]
        + dfd["CGCOMMGEN"].loc[11].loc[12].loc[2]
    ) * 0.001

    #         Total Sales to the Grid
    # T8(16,IY,IS)=CGREFGEN(11,IY,1,1)+CGREFGEN(11,IY,2,1)+CGREFGEN(11,IY,3,1)+CGREFGEN(11,IY,6,1)+CGREFGEN(11,IY,7,1)+CGREFGEN(11,IY,9,1)+CGINDLGEN(11,IY,1,1)+CGINDLGEN(11,IY,2,1)+CGINDLGEN(11,IY,3,1)+CGINDLGEN(11,IY,4,1)+CGINDLGEN(11,IY,5,1)+CGINDLGEN(11,IY,6,1)+CGINDLGEN(11,IY,7,1)+CGINDLGEN(11,IY,8,1)+CGINDLGEN(11,IY,9,1)+CGINDLGEN(11,IY,10,1)+CGINDLGEN(11,IY,11,1)+CGOGSGEN(11,IY,1,1)+CGOGSGEN(11,IY,2,1)+CGOGSGEN(11,IY,3,1)+CGOGSGEN(11,IY,4,1)+CGRESGEN(11,IY,3,1)+CGRESGEN(11,IY,8,1)+CGRESGEN(11,IY,11,1)+CGCOMMGEN(11,IY,1,1)+CGCOMMGEN(11,IY,2,1)+CGCOMMGEN(11,IY,3,1)+CGCOMMGEN(11,IY,4,1)+CGCOMMGEN(11,IY,5,1)+CGCOMMGEN(11,IY,6,1)+CGCOMMGEN(11,IY,7,1)+CGCOMMGEN(11,IY,8,1)+CGCOMMGEN(11,IY,9,1)+CGCOMMGEN(11,IY,10,1)+CGCOMMGEN(11,IY,11,1)+CGCOMMGEN(11,IY,12,1)
    z[16] = (
        dfd["CGREFGEN"].loc[11].loc[1].loc[1]
        + dfd["CGREFGEN"].loc[11].loc[2].loc[1]
        + dfd["CGREFGEN"].loc[11].loc[3].loc[1]
        + dfd["CGREFGEN"].loc[11].loc[6].loc[1]
        + dfd["CGREFGEN"].loc[11].loc[7].loc[1]
        + dfd["CGREFGEN"].loc[11].loc[9].loc[1]
        + dfd["CGINDLGEN"].loc[11].loc[1].loc[1]
        + dfd["CGINDLGEN"].loc[11].loc[2].loc[1]
        + dfd["CGINDLGEN"].loc[11].loc[3].loc[1]
        + dfd["CGINDLGEN"].loc[11].loc[4].loc[1]
        + dfd["CGINDLGEN"].loc[11].loc[5].loc[1]
        + dfd["CGINDLGEN"].loc[11].loc[6].loc[1]
        + dfd["CGINDLGEN"].loc[11].loc[7].loc[1]
        + dfd["CGINDLGEN"].loc[11].loc[8].loc[1]
        + dfd["CGINDLGEN"].loc[11].loc[9].loc[1]
        + dfd["CGINDLGEN"].loc[11].loc[10].loc[1]
        + dfd["CGINDLGEN"].loc[11].loc[11].loc[1]
        + dfd["CGOGSGEN"].loc[11].loc[1].loc[1]
        + dfd["CGOGSGEN"].loc[11].loc[2].loc[1]
        + dfd["CGOGSGEN"].loc[11].loc[3].loc[1]
        + dfd["CGOGSGEN"].loc[11].loc[4].loc[1]
        + dfd["CGRESGEN"].loc[11].loc[3].loc[1]
        + dfd["CGRESGEN"].loc[11].loc[8].loc[1]
        + dfd["CGRESGEN"].loc[11].loc[11].loc[1]
        + dfd["CGCOMMGEN"].loc[11].loc[1].loc[1]
        + dfd["CGCOMMGEN"].loc[11].loc[2].loc[1]
        + dfd["CGCOMMGEN"].loc[11].loc[3].loc[1]
        + dfd["CGCOMMGEN"].loc[11].loc[4].loc[1]
        + dfd["CGCOMMGEN"].loc[11].loc[5].loc[1]
        + dfd["CGCOMMGEN"].loc[11].loc[6].loc[1]
        + dfd["CGCOMMGEN"].loc[11].loc[7].loc[1]
        + dfd["CGCOMMGEN"].loc[11].loc[8].loc[1]
        + dfd["CGCOMMGEN"].loc[11].loc[9].loc[1]
        + dfd["CGCOMMGEN"].loc[11].loc[10].loc[1]
        + dfd["CGCOMMGEN"].loc[11].loc[11].loc[1]
        + dfd["CGCOMMGEN"].loc[11].loc[12].loc[1]
    ) * 0.001

    #

    #     Total Net Electricity Generation by Fuel

    #       Coal
    # T8(57,IY,IS)=T8(1,IY,IS)+T8(42,IY,IS)+T8(9,IY,IS)
    z[57] = z[1] + z[42] + z[9]

    #       Petroleum
    # T8(58,IY,IS)=T8(2,IY,IS)+T8(43,IY,IS)+T8(10,IY,IS)
    z[58] = z[2] + z[43] + z[10]

    #       Natural Gas
    # T8(59,IY,IS)=T8(3,IY,IS)+T8(41,IY,IS)+T8(44,IY,IS)+T8(11,IY,IS)
    z[59] = z[3] + z[41] + z[44] + z[11]

    #       Nuclear Power
    # T8(60,IY,IS)=T8(4,IY,IS)
    z[60] = z[4]

    #       Renewable Sources 5,9/
    # T8(61,IY,IS)=T8(6,IY,IS)+T8(46,IY,IS)+T8(13,IY,IS)
    z[61] = z[6] + z[46] + z[13]

    #       Hydrogen
    # T8(74,IY,IS)=T8(37,IY,IS)
    z[74] = z[37]

    #       Other 11/
    # T8(62,IY,IS)=T8(5,IY,IS)+T8(45,IY,IS)+T8(12,IY,IS)+T8(14,IY,IS)
    z[62] = z[5] + z[45] + z[12] + z[14]

    #   Total Net Electricity Generation
    # T8(56,IY,IS)=T8(8,IY,IS)+T8(48,IY,IS)+T8(15,IY,IS)
    z[56] = z[8] + z[48] + z[15]

    #   Net Generation to the Grid
    # T8(52,IY,IS)=T8(49,IY,IS)+T8(16,IY,IS)
    z[52] = z[49] + z[16]

    #

    #   Net Imports
    # T8(19,IY,IS)=(UTIMPF(MNUMNR,IY)+UTIMPE(MNUMNR,IY)-UTEXPF(MNUMNR,IY)-UTEXPE(MNUMNR,IY))*.001
    z[19] = (
        dfd["UTIMPF"].loc[MNUMNR]
        + dfd["UTIMPE"].loc[MNUMNR]
        - dfd["UTEXPF"].loc[MNUMNR]
        - dfd["UTEXPE"].loc[MNUMNR]
    ) * 0.001

    #

    #

    #   Electricity Sales by Sector

    #     Residential
    # T8(20,IY,IS)=QELRS(11,IY)/.003412
    z[20] = dfd["QELRS"].loc[11] / dfd["GWh_to_TRIL_rwpre"]

    #     Commercial
    # T8(21,IY,IS)=QELCM(11,IY)/.003412
    z[21] = dfd["QELCM"].loc[11] / dfd["GWh_to_TRIL_rwpre"]

    #     Industrial
    # T8(22,IY,IS)=QELIN(11,IY)/.003412
    z[22] = dfd["QELIN"].loc[11] / dfd["GWh_to_TRIL_rwpre"]

    #     Transportation
    # T8(23,IY,IS)=QELTR(11,IY)/.003412
    z[23] = dfd["QELTR"].loc[11] / dfd["GWh_to_TRIL_rwpre"]

    #       Total
    # T8(24,IY,IS)=QELAS(11,IY)/.003412
    z[24] = dfd["QELAS"].loc[11] / dfd["GWh_to_TRIL_rwpre"]

    #   Direct Use
    # T8(53,IY,IS)=T8(47,IY,IS)+T8(17,IY,IS)
    z[53] = z[47] + z[17]

    #   Total Electricity Use
    # T8(54,IY,IS)=T8(24,IY,IS)+T8(53,IY,IS)+T8(75,IY,IS)
    z[54] = z[24] + z[53]

    #                          Row format of 1 decimal pt overrides
    #                          the Table Format (TF) of integer (9)
    #

    #   End-Use Prices
    #   (#### cents per kilowatthour)

    #     Residential
    # T8(25,IY,IS)=PELRS(11,IY)*.3412
    z[25] = dfd["AMPBLK/PELRS"].loc[11] * dfd["GWh_to_TRIL_rwpre"] / 10 * SCALPR2

    #     Commercial
    # T8(26,IY,IS)=PELCM(11,IY)*.3412
    z[26] = dfd["AMPBLK/PELCM"].loc[11] * dfd["GWh_to_TRIL_rwpre"] / 10 * SCALPR2

    #     Industrial
    # T8(27,IY,IS)=PELIN(11,IY)*.3412
    z[27] = dfd["AMPBLK/PELIN"].loc[11] * dfd["GWh_to_TRIL_rwpre"] / 10 * SCALPR2

    #     Transportation
    # T8(28,IY,IS)=PELTR(11,IY)*.3412
    z[28] = dfd["AMPBLK/PELTR"].loc[11] * dfd["GWh_to_TRIL_rwpre"] / 10 * SCALPR2

    #       All Sectors Average
    # T8(29,IY,IS)=PELAS(11,IY)*.3412
    z[29] = dfd["AMPBLK/PELAS"].loc[11] * dfd["GWh_to_TRIL_rwpre"] / 10 * SCALPR2

    #   (nominal cents per kilowatthour)

    #     Residential
    # T8(32,IY,IS)=T8(25,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[32] = z[25] / SCALPR2 * MC_JPGDP

    #     Commercial
    # T8(33,IY,IS)=T8(26,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[33] = z[26] / SCALPR2 * MC_JPGDP

    #     Industrial
    # T8(34,IY,IS)=T8(27,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[34] = z[27] / SCALPR2 * MC_JPGDP

    #     Transportation
    # T8(35,IY,IS)=T8(28,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[35] = z[28] / SCALPR2 * MC_JPGDP

    #       All Sectors Average
    # T8(36,IY,IS)=T8(29,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[36] = z[29] / SCALPR2 * MC_JPGDP

    #

    #   Prices by Service Category

    #   (#### cents per kilowatthour)

    #     Generation
    # T8(38,IY,IS)=PECGENN(MNUMNR,IY)*.1
    z[38] = dfd["PECGENN"].loc[MNUMNR] * 0.10 * SCALPR2

    #     Transmission
    # T8(39,IY,IS)=PECTRNN(MNUMNR,IY)*.1
    z[39] = dfd["PECTRNN"].loc[MNUMNR] * 0.10 * SCALPR2

    #     Distribution
    # T8(40,IY,IS)=PECDISN(MNUMNR,IY)*.1
    z[40] = dfd["PECDISN"].loc[MNUMNR] * 0.10 * SCALPR2

    #   (nominal cents per kilowatthour)

    # #     Generation
    # T8(71,IY,IS)=T8(38,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[71] = z[38] / SCALPR2 * MC_JPGDP

    #     Transmission
    # T8(72,IY,IS)=T8(39,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[72] = z[39] / SCALPR2 * MC_JPGDP

    #     Distribution
    # T8(73,IY,IS)=T8(40,IY,IS)/SCALPR*MC_JPGDP(IY)
    z[73] = z[40] / SCALPR2 * MC_JPGDP

    #

    #   Electric Power Sector Emissions 1/

    #     Carbon Dioxide (million short tons)
    # T8(51,IY,IS)=0
    z[51] = 0 * z[40]

    #     Sulfur Dioxide (million short tons)
    # T8(30,IY,IS)=SUM(UTTSO2(1:NUTSEC,IY))+(SUM(SO2OTHER(IY,1:NUM_SO2_GRP))+SUM(CTLSO2EM(1:NDREG,IY)))/1000000.
    x = dfd["NUM_SO2_GRP"].iloc[0]
    z[30] = (
        dfd["UTTSO2"].loc[1:NUTSEC].sum()
        + (dfd["SO2OTHER"].loc[1 : int(x)].sum() + dfd["CTLSO2EM"].loc[1:NDREG].sum())
        / 1000000.0
    )

    #     Nitrogen Oxide (million short tons)
    # T8(31,IY,IS)=UNOXOTR(MNUMNR,IY)+UNOXINR(MNUMNR,IY)+SUM(CTLNOXEM(1:NDREG,IY))/1000000.
    z[31] = (
        dfd["UNOXOTR"].loc[MNUMNR]
        + dfd["UNOXINR"].loc[MNUMNR]
        + dfd["CTLNOXEM"].loc[1:NDREG].sum() / 1000000.0
    )

    #     Mercury (short tons)
    # T8(50,IY,IS)=TOT_MERC(IY)/2000.0+SUM(CTLHGEM(1:NDREG,IY))
    z[50] = (
        dfd["TOT_MERC"] / dfd["LBS_per_TON_rwpre"] + dfd["CTLHGEM"].loc[1:NDREG].sum()
    )

    return z
