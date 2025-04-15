# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_134(dfd, table_spec, table_id):
    """Fill table  Pipeline, Transport, and Storage Costs for Captured Carbon
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
    MC_JPGDP = dfd["MC_JPGDP"].loc[4:].T

    # Get inflation index of FYRPRC (2022)
    SCALPR2 = MC_JPGDP.loc[FYRPRC]  # 2022 = column 33

    MAXNFR = dfd["MAXNFR_rwpre"]

    # NUMREP = 3
    # IR = 1,72

    #   Pipeline, Transport, and Storage Costs for Captured Carbon
    #   (#### dollars per metric ton)
    #    Region and Cost
    #   Pipeline and Transport Costs
    #

    #   1. New England: CT, MA, ME, NH, RI, VT

    #     Capital
    # T134(((IR-1)*NUMREP)+1,IY,IS)=TFCCS(IR,IY)*dfd['SCALPR']
    z[1] = dfd["TFCCS"].loc[1] * SCALPR2

    #     Variable
    # T134(((IR-1)*NUMREP)+2,IY,IS)=TVCCS(IR,IY)*dfd['SCALPR']
    z[2] = dfd["TVCCS"].loc[1] * SCALPR2

    #       Total
    # T134(((IR-1)*NUMREP)+3,IY,IS)=FSUM(T134(((IR-1)*NUMREP)+1,IY,IS),2)
    z[3] = z[1] + z[2]

    #

    #   2. New York

    #     Capital
    # T134(((IR-1)*NUMREP)+1,IY,IS)=TFCCS(IR,IY)*dfd['SCALPR']
    z[4] = dfd["TFCCS"].loc[2] * SCALPR2

    #     Variable
    # T134(((IR-1)*NUMREP)+2,IY,IS)=TVCCS(IR,IY)*dfd['SCALPR']
    z[5] = dfd["TVCCS"].loc[2] * SCALPR2

    #       Total
    # T134(((IR-1)*NUMREP)+3,IY,IS)=FSUM(T134(((IR-1)*NUMREP)+1,IY,IS),2)
    z[6] = z[4] + z[5]

    #
    #   3. Pennsylvania and New Jersey

    #     Capital
    # T134(((IR-1)*NUMREP)+1,IY,IS)=TFCCS(IR,IY)*dfd['SCALPR']
    z[7] = dfd["TFCCS"].loc[3] * SCALPR2

    #     Variable
    # T134(((IR-1)*NUMREP)+2,IY,IS)=TVCCS(IR,IY)*dfd['SCALPR']
    z[8] = dfd["TVCCS"].loc[3] * SCALPR2

    #       Total
    # T134(((IR-1)*NUMREP)+3,IY,IS)=FSUM(T134(((IR-1)*NUMREP)+1,IY,IS),2)
    z[9] = z[7] + z[8]

    #

    #   4. Ohio

    #     Capital
    # T134(((IR-1)*NUMREP)+1,IY,IS)=TFCCS(IR,IY)*dfd['SCALPR']
    z[10] = dfd["TFCCS"].loc[4] * SCALPR2
    #     Variable
    # T134(((IR-1)*NUMREP)+2,IY,IS)=TVCCS(IR,IY)*dfd['SCALPR']
    z[11] = dfd["TVCCS"].loc[4] * SCALPR2

    #       Total
    # T134(((IR-1)*NUMREP)+3,IY,IS)=FSUM(T134(((IR-1)*NUMREP)+1,IY,IS),2)
    z[12] = z[10] + z[11]

    #

    #   5. Illinois, Michigan, and Wisconsin

    #     Capital
    # T134(((IR-1)*NUMREP)+1,IY,IS)=TFCCS(IR,IY)*dfd['SCALPR']
    z[13] = dfd["TFCCS"].loc[5] * SCALPR2

    #     Variable
    # T134(((IR-1)*NUMREP)+2,IY,IS)=TVCCS(IR,IY)*dfd['SCALPR']
    z[14] = dfd["TVCCS"].loc[5] * SCALPR2

    #       Total
    # T134(((IR-1)*NUMREP)+3,IY,IS)=FSUM(T134(((IR-1)*NUMREP)+1,IY,IS),2)
    z[15] = z[13] + z[14]

    #

    #   6. Indiana

    #     Capital
    # T134(((IR-1)*NUMREP)+1,IY,IS)=TFCCS(IR,IY)*dfd['SCALPR']
    z[16] = dfd["TFCCS"].loc[6] * SCALPR2

    #     Variable
    # T134(((IR-1)*NUMREP)+2,IY,IS)=TVCCS(IR,IY)*dfd['SCALPR']
    z[17] = dfd["TVCCS"].loc[6] * SCALPR2

    #       Total
    # T134(((IR-1)*NUMREP)+3,IY,IS)=FSUM(T134(((IR-1)*NUMREP)+1,IY,IS),2)
    z[18] = z[16] + z[17]

    #

    #   7. Kansas and Iowa

    #     Capital
    # T134(((IR-1)*NUMREP)+1,IY,IS)=TFCCS(IR,IY)*dfd['SCALPR']
    z[19] = dfd["TFCCS"].loc[7] * SCALPR2

    #     Variable
    # T134(((IR-1)*NUMREP)+2,IY,IS)=TVCCS(IR,IY)*dfd['SCALPR']
    z[20] = dfd["TVCCS"].loc[7] * SCALPR2

    #       Total
    # T134(((IR-1)*NUMREP)+3,IY,IS)=FSUM(T134(((IR-1)*NUMREP)+1,IY,IS),2)
    z[21] = z[19] + z[20]

    #

    #   8. Minnesota

    #     Capital
    # T134(((IR-1)*NUMREP)+1,IY,IS)=TFCCS(IR,IY)*dfd['SCALPR']
    z[22] = dfd["TFCCS"].loc[8] * SCALPR2

    #     Variable
    # T134(((IR-1)*NUMREP)+2,IY,IS)=TVCCS(IR,IY)*dfd['SCALPR']
    z[23] = dfd["TVCCS"].loc[8] * SCALPR2

    #       Total
    # T134(((IR-1)*NUMREP)+3,IY,IS)=FSUM(T134(((IR-1)*NUMREP)+1,IY,IS),2)
    z[24] = z[22] + z[23]

    #

    #   9. Missouri and Nebraska

    #     Capital
    # T134(((IR-1)*NUMREP)+1,IY,IS)=TFCCS(IR,IY)*dfd['SCALPR']
    z[25] = dfd["TFCCS"].loc[9] * SCALPR2

    #     Variable
    # T134(((IR-1)*NUMREP)+2,IY,IS)=TVCCS(IR,IY)*dfd['SCALPR']
    z[26] = dfd["TVCCS"].loc[9] * SCALPR2

    #       Total
    # T134(((IR-1)*NUMREP)+3,IY,IS)=FSUM(T134(((IR-1)*NUMREP)+1,IY,IS),2)
    z[27] = z[25] + z[26]

    #

    #   10. North Dakota and South Dakota

    #     Capital
    # T134(((IR-1)*NUMREP)+1,IY,IS)=TFCCS(IR,IY)*dfd['SCALPR']
    z[28] = dfd["TFCCS"].loc[10] * SCALPR2

    #     Variable
    # T134(((IR-1)*NUMREP)+2,IY,IS)=TVCCS(IR,IY)*dfd['SCALPR']
    z[29] = dfd["TVCCS"].loc[10] * SCALPR2

    #       Total
    # T134(((IR-1)*NUMREP)+3,IY,IS)=FSUM(T134(((IR-1)*NUMREP)+1,IY,IS),2)
    z[30] = z[28] + z[29]

    #

    #   11. District of Columbia and West Virginia

    #     Capital
    # T134(((IR-1)*NUMREP)+1,IY,IS)=TFCCS(IR,IY)*dfd['SCALPR']
    z[31] = dfd["TFCCS"].loc[11] * SCALPR2

    #     Variable
    # T134(((IR-1)*NUMREP)+2,IY,IS)=TVCCS(IR,IY)*dfd['SCALPR']
    z[32] = dfd["TVCCS"].loc[11] * SCALPR2

    #       Total
    # T134(((IR-1)*NUMREP)+3,IY,IS)=FSUM(T134(((IR-1)*NUMREP)+1,IY,IS),2)
    z[33] = z[31] + z[32]

    #

    #   12. NC, SC, VA

    #     Capital
    # T134(((IR-1)*NUMREP)+1,IY,IS)=TFCCS(IR,IY)*dfd['SCALPR']
    z[34] = dfd["TFCCS"].loc[12] * SCALPR2

    #     Variable
    # T134(((IR-1)*NUMREP)+2,IY,IS)=TVCCS(IR,IY)*dfd['SCALPR']
    z[35] = dfd["TVCCS"].loc[12] * SCALPR2

    #       Total
    # T134(((IR-1)*NUMREP)+3,IY,IS)=FSUM(T134(((IR-1)*NUMREP)+1,IY,IS),2)
    z[36] = z[34] + z[35]

    #

    #   13. Delaware and Maryland

    #     Capital
    # T134(((IR-1)*NUMREP)+1,IY,IS)=TFCCS(IR,IY)*dfd['SCALPR']
    z[37] = dfd["TFCCS"].loc[13] * SCALPR2

    #     Variable
    # T134(((IR-1)*NUMREP)+2,IY,IS)=TVCCS(IR,IY)*dfd['SCALPR']
    z[38] = dfd["TVCCS"].loc[13] * SCALPR2

    #       Total
    # T134(((IR-1)*NUMREP)+3,IY,IS)=FSUM(T134(((IR-1)*NUMREP)+1,IY,IS),2)
    z[39] = z[37] + z[38]

    #

    #   14. Georgia

    #     Capital
    # T134(((IR-1)*NUMREP)+1,IY,IS)=TFCCS(IR,IY)*dfd['SCALPR']
    z[40] = dfd["TFCCS"].loc[14] * SCALPR2

    #     Variable
    # T134(((IR-1)*NUMREP)+2,IY,IS)=TVCCS(IR,IY)*dfd['SCALPR']
    z[41] = dfd["TVCCS"].loc[14] * SCALPR2

    #       Total
    # T134(((IR-1)*NUMREP)+3,IY,IS)=FSUM(T134(((IR-1)*NUMREP)+1,IY,IS),2)
    z[42] = z[40] + z[41]

    #

    #   15. Florida

    #     Capital
    # T134(((IR-1)*NUMREP)+1,IY,IS)=TFCCS(IR,IY)*dfd['SCALPR']
    z[43] = dfd["TFCCS"].loc[15] * SCALPR2

    #     Variable
    # T134(((IR-1)*NUMREP)+2,IY,IS)=TVCCS(IR,IY)*dfd['SCALPR']
    z[44] = dfd["TVCCS"].loc[15] * SCALPR2

    #       Total
    # T134(((IR-1)*NUMREP)+3,IY,IS)=FSUM(T134(((IR-1)*NUMREP)+1,IY,IS),2)
    z[45] = z[43] + z[44]

    #

    #   16. Kentucky and Tennessee

    #     Capital
    # T134(((IR-1)*NUMREP)+1,IY,IS)=TFCCS(IR,IY)*dfd['SCALPR']
    z[46] = dfd["TFCCS"].loc[16] * SCALPR2

    #     Variable
    # T134(((IR-1)*NUMREP)+2,IY,IS)=TVCCS(IR,IY)*dfd['SCALPR']
    z[47] = dfd["TVCCS"].loc[16] * SCALPR2

    #       Total
    # T134(((IR-1)*NUMREP)+3,IY,IS)=FSUM(T134(((IR-1)*NUMREP)+1,IY,IS),2)
    z[48] = z[46] + z[47]

    #

    #   17. Alabama and Mississippi

    #     Capital
    # T134(((IR-1)*NUMREP)+1,IY,IS)=TFCCS(IR,IY)*dfd['SCALPR']
    z[49] = dfd["TFCCS"].loc[17] * SCALPR2

    #     Variable
    # T134(((IR-1)*NUMREP)+2,IY,IS)=TVCCS(IR,IY)*dfd['SCALPR']
    z[50] = dfd["TVCCS"].loc[17] * SCALPR2

    #       Total
    # T134(((IR-1)*NUMREP)+3,IY,IS)=FSUM(T134(((IR-1)*NUMREP)+1,IY,IS),2)
    z[51] = z[49] + z[50]

    #

    #   18. WS, AR, LA, OK, TX

    #     Capital
    # T134(((IR-1)*NUMREP)+1,IY,IS)=TFCCS(IR,IY)*dfd['SCALPR']
    z[52] = dfd["TFCCS"].loc[18] * SCALPR2

    #     Variable
    # T134(((IR-1)*NUMREP)+2,IY,IS)=TVCCS(IR,IY)*dfd['SCALPR']
    z[53] = dfd["TVCCS"].loc[18] * SCALPR2

    #       Total
    # T134(((IR-1)*NUMREP)+3,IY,IS)=FSUM(T134(((IR-1)*NUMREP)+1,IY,IS),2)
    z[54] = z[52] + z[53]

    #

    #   19. Montana, Idaho, Wyoming

    #     Capital
    # T134(((IR-1)*NUMREP)+1,IY,IS)=TFCCS(IR,IY)*dfd['SCALPR']
    z[55] = dfd["TFCCS"].loc[19] * SCALPR2

    #     Variable
    # T134(((IR-1)*NUMREP)+2,IY,IS)=TVCCS(IR,IY)*dfd['SCALPR']
    z[56] = dfd["TVCCS"].loc[19] * SCALPR2

    #       Total
    # T134(((IR-1)*NUMREP)+3,IY,IS)=FSUM(T134(((IR-1)*NUMREP)+1,IY,IS),2)
    z[57] = z[55] + z[56]

    #

    #   20. Colorado, Nevada, Utah

    #     Capital
    # T134(((IR-1)*NUMREP)+1,IY,IS)=TFCCS(IR,IY)*dfd['SCALPR']
    z[58] = dfd["TFCCS"].loc[20] * SCALPR2

    #     Variable
    # T134(((IR-1)*NUMREP)+2,IY,IS)=TVCCS(IR,IY)*dfd['SCALPR']
    z[59] = dfd["TVCCS"].loc[20] * SCALPR2

    #       Total
    # T134(((IR-1)*NUMREP)+3,IY,IS)=FSUM(T134(((IR-1)*NUMREP)+1,IY,IS),2)
    z[60] = z[58] + z[59]

    #

    #   21. Arizona and New Mexico

    #     Capital
    # T134(((IR-1)*NUMREP)+1,IY,IS)=TFCCS(IR,IY)*dfd['SCALPR']
    z[61] = dfd["TFCCS"].loc[21] * SCALPR2

    #     Variable
    # T134(((IR-1)*NUMREP)+2,IY,IS)=TVCCS(IR,IY)*dfd['SCALPR']
    z[62] = dfd["TVCCS"].loc[21] * SCALPR2

    #       Total
    # T134(((IR-1)*NUMREP)+3,IY,IS)=FSUM(T134(((IR-1)*NUMREP)+1,IY,IS),2)
    z[63] = z[61] + z[62]

    #

    #   22. Oregon and Washington

    #     Capital
    # T134(((IR-1)*NUMREP)+1,IY,IS)=TFCCS(IR,IY)*dfd['SCALPR']
    z[64] = dfd["TFCCS"].loc[22] * SCALPR2

    #     Variable
    # T134(((IR-1)*NUMREP)+2,IY,IS)=TVCCS(IR,IY)*dfd['SCALPR']
    z[65] = dfd["TVCCS"].loc[22] * SCALPR2

    #       Total
    # T134(((IR-1)*NUMREP)+3,IY,IS)=FSUM(T134(((IR-1)*NUMREP)+1,IY,IS),2)
    z[66] = z[64] + z[65]

    #

    #   23. California

    #     Capital
    # T134(((IR-1)*NUMREP)+1,IY,IS)=TFCCS(IR,IY)*dfd['SCALPR']
    z[67] = dfd["TFCCS"].loc[23] * SCALPR2

    #     Variable
    # T134(((IR-1)*NUMREP)+2,IY,IS)=TVCCS(IR,IY)*dfd['SCALPR']
    z[68] = dfd["TVCCS"].loc[23] * SCALPR2

    #       Total
    # T134(((IR-1)*NUMREP)+3,IY,IS)=FSUM(T134(((IR-1)*NUMREP)+1,IY,IS),2)
    z[69] = z[67] + z[68]

    #

    #   United States

    #     Capital
    # T134(((IR-1)*NUMREP)+1,IY,IS)=TFCCS(IR,IY)*dfd['SCALPR']
    z[70] = dfd["TFCCS"].loc[24] * SCALPR2

    #     Variable
    # T134(((IR-1)*NUMREP)+2,IY,IS)=TVCCS(IR,IY)*dfd['SCALPR']
    z[71] = dfd["TVCCS"].loc[24] * SCALPR2

    #       Total
    # T134(((IR-1)*NUMREP)+3,IY,IS)=FSUM(T134(((IR-1)*NUMREP)+1,IY,IS),2)
    z[72] = z[70] + z[71]

    #

    #   CO2 Storage Costs
    #

    #   1. New England: CT, MA, ME, NH, RI, VT

    #     Capital
    # T134(((IR-1)*NUMREP)+73,IY,IS)=IFCCS(IR,IY)*dfd['SCALPR']
    z[73] = dfd["IFCCS"].loc[1] * SCALPR2

    #     Variable
    # T134(((IR-1)*NUMREP)+74,IY,IS)=IVCCS(IR,IY)*dfd['SCALPR']
    z[74] = dfd["IVCCS"].loc[1] * SCALPR2

    #       Total
    # T134(((IR-1)*NUMREP)+75,IY,IS)=FSUM(T134(((IR-1)*NUMREP)+73,IY,IS),2)
    z[75] = z[73] + z[74]

    #

    #   2. New York and New Jersey

    #     Capital
    # T134(((IR-1)*NUMREP)+73,IY,IS)=IFCCS(IR,IY)*dfd['SCALPR']
    z[76] = dfd["IFCCS"].loc[2] * SCALPR2

    #     Variable
    # T134(((IR-1)*NUMREP)+74,IY,IS)=IVCCS(IR,IY)*dfd['SCALPR']
    z[77] = dfd["IVCCS"].loc[2] * SCALPR2

    #       Total
    # T134(((IR-1)*NUMREP)+75,IY,IS)=FSUM(T134(((IR-1)*NUMREP)+73,IY,IS),2)
    z[78] = z[76] + z[77]

    #

    #   3. Pennsylvania

    #     Capital
    # T134(((IR-1)*NUMREP)+73,IY,IS)=IFCCS(IR,IY)*dfd['SCALPR']
    z[79] = dfd["IFCCS"].loc[3] * SCALPR2

    #     Variable
    # T134(((IR-1)*NUMREP)+74,IY,IS)=IVCCS(IR,IY)*dfd['SCALPR']
    z[80] = dfd["IVCCS"].loc[3] * SCALPR2

    #       Total
    # T134(((IR-1)*NUMREP)+75,IY,IS)=FSUM(T134(((IR-1)*NUMREP)+73,IY,IS),2)
    z[81] = z[79] + z[80]

    #

    #   4. Ohio

    #     Capital
    # T134(((IR-1)*NUMREP)+73,IY,IS)=IFCCS(IR,IY)*dfd['SCALPR']
    z[82] = dfd["IFCCS"].loc[4] * SCALPR2

    #     Variable
    # T134(((IR-1)*NUMREP)+74,IY,IS)=IVCCS(IR,IY)*dfd['SCALPR']
    z[83] = dfd["IVCCS"].loc[4] * SCALPR2

    #       Total
    # T134(((IR-1)*NUMREP)+75,IY,IS)=FSUM(T134(((IR-1)*NUMREP)+73,IY,IS),2)
    z[84] = z[82] + z[83]

    #

    #   5. Illinois, Michigan, and Wisconsin

    #     Capital
    # T134(((IR-1)*NUMREP)+73,IY,IS)=IFCCS(IR,IY)*dfd['SCALPR']
    z[85] = dfd["IFCCS"].loc[5] * SCALPR2

    #     Variable
    # T134(((IR-1)*NUMREP)+74,IY,IS)=IVCCS(IR,IY)*dfd['SCALPR']
    z[86] = dfd["IVCCS"].loc[5] * SCALPR2

    #       Total
    # T134(((IR-1)*NUMREP)+75,IY,IS)=FSUM(T134(((IR-1)*NUMREP)+73,IY,IS),2)
    z[87] = z[85] + z[86]

    #

    #   6. Indiana

    #     Capital
    # T134(((IR-1)*NUMREP)+73,IY,IS)=IFCCS(IR,IY)*dfd['SCALPR']
    z[88] = dfd["IFCCS"].loc[6] * SCALPR2

    #     Variable
    # T134(((IR-1)*NUMREP)+74,IY,IS)=IVCCS(IR,IY)*dfd['SCALPR']
    z[89] = dfd["IVCCS"].loc[6] * SCALPR2

    #       Total
    # T134(((IR-1)*NUMREP)+75,IY,IS)=FSUM(T134(((IR-1)*NUMREP)+73,IY,IS),2)
    z[90] = z[88] + z[89]

    #

    #   7. Kansas and Iowa

    #     Capital
    # T134(((IR-1)*NUMREP)+73,IY,IS)=IFCCS(IR,IY)*dfd['SCALPR']
    z[91] = dfd["IFCCS"].loc[7] * SCALPR2

    #     Variable
    # T134(((IR-1)*NUMREP)+74,IY,IS)=IVCCS(IR,IY)*dfd['SCALPR']
    z[92] = dfd["IVCCS"].loc[7] * SCALPR2

    #       Total
    # T134(((IR-1)*NUMREP)+75,IY,IS)=FSUM(T134(((IR-1)*NUMREP)+73,IY,IS),2)
    z[93] = z[91] + z[92]

    #

    #   8. Minnesota

    #     Capital
    # T134(((IR-1)*NUMREP)+73,IY,IS)=IFCCS(IR,IY)*dfd['SCALPR']
    z[94] = dfd["IFCCS"].loc[8] * SCALPR2

    #     Variable
    # T134(((IR-1)*NUMREP)+74,IY,IS)=IVCCS(IR,IY)*dfd['SCALPR']
    z[95] = dfd["IVCCS"].loc[8] * SCALPR2

    #       Total
    # T134(((IR-1)*NUMREP)+75,IY,IS)=FSUM(T134(((IR-1)*NUMREP)+73,IY,IS),2)
    z[96] = z[94] + z[95]

    #

    #   9. Missouri and Nebraska

    #     Capital
    # T134(((IR-1)*NUMREP)+73,IY,IS)=IFCCS(IR,IY)*dfd['SCALPR']
    z[97] = dfd["IFCCS"].loc[9] * SCALPR2

    #     Variable
    # T134(((IR-1)*NUMREP)+74,IY,IS)=IVCCS(IR,IY)*dfd['SCALPR']
    z[98] = dfd["IVCCS"].loc[9] * SCALPR2

    #       Total
    # T134(((IR-1)*NUMREP)+75,IY,IS)=FSUM(T134(((IR-1)*NUMREP)+73,IY,IS),2)
    z[99] = z[97] + z[98]

    #

    #   10. North Dakota and South Dakota

    #     Capital
    # T134(((IR-1)*NUMREP)+73,IY,IS)=IFCCS(IR,IY)*dfd['SCALPR']
    z[100] = dfd["IFCCS"].loc[10] * SCALPR2

    #     Variable
    # T134(((IR-1)*NUMREP)+74,IY,IS)=IVCCS(IR,IY)*dfd['SCALPR']
    z[101] = dfd["IVCCS"].loc[10] * SCALPR2

    #       Total
    # T134(((IR-1)*NUMREP)+75,IY,IS)=FSUM(T134(((IR-1)*NUMREP)+73,IY,IS),2)
    z[102] = z[100] + z[101]

    #

    #   11. District of Columbia and West Virginia

    #     Capital
    # T134(((IR-1)*NUMREP)+73,IY,IS)=IFCCS(IR,IY)*dfd['SCALPR']
    z[103] = dfd["IFCCS"].loc[11] * SCALPR2

    #     Variable
    # T134(((IR-1)*NUMREP)+74,IY,IS)=IVCCS(IR,IY)*dfd['SCALPR']
    z[104] = dfd["IVCCS"].loc[11] * SCALPR2

    #       Total
    # T134(((IR-1)*NUMREP)+75,IY,IS)=FSUM(T134(((IR-1)*NUMREP)+73,IY,IS),2)
    z[105] = z[103] + z[104]

    #

    #   12. NC, SC, VA

    #     Capital
    # T134(((IR-1)*NUMREP)+73,IY,IS)=IFCCS(IR,IY)*dfd['SCALPR']
    z[106] = dfd["IFCCS"].loc[12] * SCALPR2

    #     Variable
    # T134(((IR-1)*NUMREP)+74,IY,IS)=IVCCS(IR,IY)*dfd['SCALPR']
    z[107] = dfd["IVCCS"].loc[12] * SCALPR2

    #       Total
    # T134(((IR-1)*NUMREP)+75,IY,IS)=FSUM(T134(((IR-1)*NUMREP)+73,IY,IS),2)
    z[108] = z[106] + z[107]

    #

    #   13. Delaware and Maryland

    #     Capital
    # T134(((IR-1)*NUMREP)+73,IY,IS)=IFCCS(IR,IY)*dfd['SCALPR']
    z[109] = dfd["IFCCS"].loc[13] * SCALPR2

    #     Variable
    # T134(((IR-1)*NUMREP)+74,IY,IS)=IVCCS(IR,IY)*dfd['SCALPR']
    z[110] = dfd["IVCCS"].loc[13] * SCALPR2

    #       Total
    # T134(((IR-1)*NUMREP)+75,IY,IS)=FSUM(T134(((IR-1)*NUMREP)+73,IY,IS),2)
    z[111] = z[109] + z[110]

    #

    #   14. Georgia

    #     Capital
    # T134(((IR-1)*NUMREP)+73,IY,IS)=IFCCS(IR,IY)*dfd['SCALPR']
    z[112] = dfd["IFCCS"].loc[14] * SCALPR2

    #     Variable
    # T134(((IR-1)*NUMREP)+74,IY,IS)=IVCCS(IR,IY)*dfd['SCALPR']
    z[113] = dfd["IVCCS"].loc[14] * SCALPR2

    #       Total
    # T134(((IR-1)*NUMREP)+75,IY,IS)=FSUM(T134(((IR-1)*NUMREP)+73,IY,IS),2)
    z[114] = z[112] + z[113]

    #

    #   15. Florida

    #     Capital
    # T134(((IR-1)*NUMREP)+73,IY,IS)=IFCCS(IR,IY)*dfd['SCALPR']
    z[115] = dfd["IFCCS"].loc[15] * SCALPR2

    #     Variable
    # T134(((IR-1)*NUMREP)+74,IY,IS)=IVCCS(IR,IY)*dfd['SCALPR']
    z[116] = dfd["IVCCS"].loc[15] * SCALPR2

    #       Total
    # T134(((IR-1)*NUMREP)+75,IY,IS)=FSUM(T134(((IR-1)*NUMREP)+73,IY,IS),2)
    z[117] = z[115] + z[116]

    #

    #   16. Kentucky and Tennessee

    #     Capital
    # T134(((IR-1)*NUMREP)+73,IY,IS)=IFCCS(IR,IY)*dfd['SCALPR']
    z[118] = dfd["IFCCS"].loc[16] * SCALPR2

    #     Variable
    # T134(((IR-1)*NUMREP)+74,IY,IS)=IVCCS(IR,IY)*dfd['SCALPR']
    z[119] = dfd["IVCCS"].loc[16] * SCALPR2

    #       Total
    # T134(((IR-1)*NUMREP)+75,IY,IS)=FSUM(T134(((IR-1)*NUMREP)+73,IY,IS),2)
    z[120] = z[118] + z[119]

    #

    #   17. Alabama and Mississippi

    #     Capital
    # T134(((IR-1)*NUMREP)+73,IY,IS)=IFCCS(IR,IY)*dfd['SCALPR']
    z[121] = dfd["IFCCS"].loc[17] * SCALPR2

    #     Variable
    # T134(((IR-1)*NUMREP)+74,IY,IS)=IVCCS(IR,IY)*dfd['SCALPR']
    z[122] = dfd["IVCCS"].loc[17] * SCALPR2

    #       Total
    # T134(((IR-1)*NUMREP)+75,IY,IS)=FSUM(T134(((IR-1)*NUMREP)+73,IY,IS),2)
    z[123] = z[121] + z[122]

    #

    #   18. WS, AR, LA, OK, TX

    #     Capital
    # T134(((IR-1)*NUMREP)+73,IY,IS)=IFCCS(IR,IY)*dfd['SCALPR']
    z[124] = dfd["IFCCS"].loc[18] * SCALPR2

    #     Variable
    # T134(((IR-1)*NUMREP)+74,IY,IS)=IVCCS(IR,IY)*dfd['SCALPR']
    z[125] = dfd["IVCCS"].loc[18] * SCALPR2

    #       Total
    # T134(((IR-1)*NUMREP)+75,IY,IS)=FSUM(T134(((IR-1)*NUMREP)+73,IY,IS),2)
    z[126] = z[124] + z[125]

    #

    #   19. Montana, Idaho, Wyoming

    #     Capital
    # T134(((IR-1)*NUMREP)+73,IY,IS)=IFCCS(IR,IY)*dfd['SCALPR']
    z[127] = dfd["IFCCS"].loc[19] * SCALPR2

    #     Variable
    # T134(((IR-1)*NUMREP)+74,IY,IS)=IVCCS(IR,IY)*dfd['SCALPR']
    z[128] = dfd["IVCCS"].loc[19] * SCALPR2

    #       Total
    # T134(((IR-1)*NUMREP)+75,IY,IS)=FSUM(T134(((IR-1)*NUMREP)+73,IY,IS),2)
    z[129] = z[127] + z[128]

    #

    #   20. Colorado, Nevada, Utah

    #     Capital
    # T134(((IR-1)*NUMREP)+73,IY,IS)=IFCCS(IR,IY)*dfd['SCALPR']
    z[130] = dfd["IFCCS"].loc[20] * SCALPR2

    #     Variable
    # T134(((IR-1)*NUMREP)+74,IY,IS)=IVCCS(IR,IY)*dfd['SCALPR']
    z[131] = dfd["IVCCS"].loc[20] * SCALPR2

    #       Total
    # T134(((IR-1)*NUMREP)+75,IY,IS)=FSUM(T134(((IR-1)*NUMREP)+73,IY,IS),2)
    z[132] = z[130] + z[131]

    #

    #   21. Arizona and New Mexico

    #     Capital
    # T134(((IR-1)*NUMREP)+73,IY,IS)=IFCCS(IR,IY)*dfd['SCALPR']
    z[133] = dfd["IFCCS"].loc[21] * SCALPR2

    #     Variable
    # T134(((IR-1)*NUMREP)+74,IY,IS)=IVCCS(IR,IY)*dfd['SCALPR']
    z[134] = dfd["IVCCS"].loc[21] * SCALPR2

    #       Total
    # T134(((IR-1)*NUMREP)+75,IY,IS)=FSUM(T134(((IR-1)*NUMREP)+73,IY,IS),2)
    z[135] = z[133] + z[134]

    #

    #   22. Oregon and Washington

    #     Capital
    # T134(((IR-1)*NUMREP)+73,IY,IS)=IFCCS(IR,IY)*dfd['SCALPR']
    z[136] = dfd["IFCCS"].loc[22] * SCALPR2

    #     Variable
    # T134(((IR-1)*NUMREP)+74,IY,IS)=IVCCS(IR,IY)*dfd['SCALPR']
    z[137] = dfd["IVCCS"].loc[22] * SCALPR2

    #       Total
    # T134(((IR-1)*NUMREP)+75,IY,IS)=FSUM(T134(((IR-1)*NUMREP)+73,IY,IS),2)
    z[138] = z[136] + z[137]

    #

    #   23. California

    #     Capital
    # T134(((IR-1)*NUMREP)+73,IY,IS)=IFCCS(IR,IY)*dfd['SCALPR']
    z[139] = dfd["IFCCS"].loc[23] * SCALPR2

    #     Variable
    # T134(((IR-1)*NUMREP)+74,IY,IS)=IVCCS(IR,IY)*dfd['SCALPR']
    z[140] = dfd["IVCCS"].loc[23] * SCALPR2

    #       Total
    # T134(((IR-1)*NUMREP)+75,IY,IS)=FSUM(T134(((IR-1)*NUMREP)+73,IY,IS),2)
    z[141] = z[139] + z[140]

    #

    #   United States

    #     Capital
    # T134(((IR-1)*NUMREP)+73,IY,IS)=IFCCS(IR,IY)*dfd['SCALPR']
    z[142] = dfd["IFCCS"].loc[24] * SCALPR2

    #     Variable
    # T134(((IR-1)*NUMREP)+74,IY,IS)=IVCCS(IR,IY)*dfd['SCALPR']
    z[143] = dfd["IVCCS"].loc[24] * SCALPR2

    #       Total
    # T134(((IR-1)*NUMREP)+75,IY,IS)=FSUM(T134(((IR-1)*NUMREP)+73,IY,IS),2)
    z[144] = z[142] + z[143]

    return z
