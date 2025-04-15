# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_100(dfd, table_spec, table_id):
    """Fill table  Employment and Shipments byIndustry and Income and Employment by Region

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

    # Employment and Shipments byIndustry and Income and Employment by Region
    #    Indicator
    #
    #   Value of Shipments (billion m==m dollars)
    #     Non-Industrial and Service Sectors

    #       Total Nonmanufacturing
    # T100(68,IY,IS)=SUM(MC_REVIND(11,42:48,IY))
    z[68] = dfd["MC_REVIND"].loc[range(42, 49)].groupby(level=1).sum().loc[MNUMCR]
    # z[68] = dfd['MC_REVIND'].loc[(11, (42,43,44,45,46,47,48)), :].sum()

    #         Agriculture/Forestry/Fishing/Hunting
    # T100(62,IY,IS)=SUM(MC_REVIND(11,42:44,IY))
    z[62] = dfd["MC_REVIND"].loc[range(42, 45)].groupby(level=1).sum().loc[MNUMCR]

    #         Mining
    # T100(63,IY,IS)=SUM(MC_REVIND(11,45:47,IY))
    z[63] = dfd["MC_REVIND"].loc[range(45, 48)].groupby(level=1).sum().loc[MNUMCR]

    #           Coal Mining
    # T100(64:67,IY,IS)=MC_REVIND(11,45:48,IY)
    z[64] = dfd["MC_REVIND"].loc[45].loc[MNUMCR]

    #           Oil and Gas Extraction
    # T100(64:67,IY,IS)=MC_REVIND(11,45:48,IY)
    z[65] = dfd["MC_REVIND"].loc[46].loc[MNUMCR]

    #           Other Mining
    # T100(64:67,IY,IS)=MC_REVIND(11,45:48,IY)
    z[66] = dfd["MC_REVIND"].loc[47].loc[MNUMCR]

    #         Construction
    # T100(64:67,IY,IS)=MC_REVIND(11,45:48,IY)
    z[67] = dfd["MC_REVIND"].loc[48].loc[MNUMCR]

    #       Manufacturing Sector
    # T100(75,IY,IS)=SUM(MC_REVIND(11,1:41,IY))-SUM(MC_REVIND(11,2:5,IY))-SUM(MC_REVIND(11,11:13,IY))-SUM(MC_REVIND(11,21:24,IY))-MC_REVIND(11,17,IY)-MC_REVIND(11,29,IY)
    z[75] = (
        dfd["MC_REVIND"].loc[range(1, 42)].groupby(level=1).sum().loc[MNUMCR]
        - dfd["MC_REVIND"].loc[range(2, 6)].groupby(level=1).sum().loc[MNUMCR]
        - dfd["MC_REVIND"].loc[range(11, 14)].groupby(level=1).sum().loc[MNUMCR]
        - dfd["MC_REVIND"].loc[range(21, 25)].groupby(level=1).sum().loc[MNUMCR]
        - dfd["MC_REVIND"].loc[17].loc[MNUMCR]
        - dfd["MC_REVIND"].loc[29].loc[MNUMCR]
    )
    #     Total Industrial
    # T100(76,IY,IS)=T100(68,IY,IS)+T100(75,IY,IS)
    z[76] = z[68] + z[75]
    # Services
    z[77] = dfd["MC_REVSER"].loc[11].loc[1:10].sum()
    # Total Employment
    z[78] = z[76] + z[77]
    #         Food Products
    # T100(21:61,IY,IS)=MC_REVIND(11,1:41,IY)
    z[21] = dfd["MC_REVIND"].loc[1].loc[MNUMCR]

    #           Grain and Oil Seed Milling
    # T100(21:61,IY,IS)=MC_REVIND(11,1:41,IY)
    z[22] = dfd["MC_REVIND"].loc[2].loc[MNUMCR]

    #           Dairy Products
    # T100(21:61,IY,IS)=MC_REVIND(11,1:41,IY)
    z[23] = dfd["MC_REVIND"].loc[3].loc[MNUMCR]

    #           Animal Slaughter and Seafood Products
    # T100(21:61,IY,IS)=MC_REVIND(11,1:41,IY)
    z[24] = dfd["MC_REVIND"].loc[4].loc[MNUMCR]

    #           Other
    # T100(21:61,IY,IS)=MC_REVIND(11,1:41,IY)
    z[25] = dfd["MC_REVIND"].loc[5].loc[MNUMCR]

    #         Beverages and Tobacco Products
    # T100(21:61,IY,IS)=MC_REVIND(11,1:41,IY)
    z[26] = dfd["MC_REVIND"].loc[6].loc[MNUMCR]

    #         Textile Mills and Products
    # T100(21:61,IY,IS)=MC_REVIND(11,1:41,IY)
    z[27] = dfd["MC_REVIND"].loc[7].loc[MNUMCR]

    #         Wood Products
    # T100(21:61,IY,IS)=MC_REVIND(11,1:41,IY)
    z[28] = dfd["MC_REVIND"].loc[8].loc[MNUMCR]

    #         Furniture and Related Products
    # T100(21:61,IY,IS)=MC_REVIND(11,1:41,IY)
    z[29] = dfd["MC_REVIND"].loc[9].loc[MNUMCR]

    #         Paper Products
    # T100(21:61,IY,IS)=MC_REVIND(11,1:41,IY)
    z[30] = dfd["MC_REVIND"].loc[10].loc[MNUMCR]

    #           Pulp and Paper Mills
    # T100(21:61,IY,IS)=MC_REVIND(11,1:41,IY)
    z[31] = dfd["MC_REVIND"].loc[11].loc[MNUMCR]

    #           Paperboard Container
    # T100(21:61,IY,IS)=MC_REVIND(11,1:41,IY)
    z[32] = dfd["MC_REVIND"].loc[12].loc[MNUMCR]

    #           Other Paper
    # T100(21:61,IY,IS)=MC_REVIND(11,1:41,IY)
    z[33] = dfd["MC_REVIND"].loc[13].loc[MNUMCR]

    #         Printing
    # T100(21:61,IY,IS)=MC_REVIND(11,1:41,IY)
    z[34] = dfd["MC_REVIND"].loc[14].loc[MNUMCR]

    #           Bulk Chemicals
    # T100(69,IY,IS)=SUM(MC_REVIND(11,15:19,IY))-MC_REVIND(11,17,IY)
    z[69] = (
        dfd["MC_REVIND"].loc[range(15, 20)].groupby(level=1).sum().loc[11]
        - dfd["MC_REVIND"].loc[17].loc[MNUMCR]
    )

    #             Inorganic
    # T100(21:61,IY,IS)=MC_REVIND(11,1:41,IY)
    z[35] = dfd["MC_REVIND"].loc[15].loc[MNUMCR]

    #               Industrial Gas Manufacturing
    # T100(129,IY,IS)=MC_REVIND(11,51,IY)
    z[129] = dfd["MC_REVIND"].loc[51].loc[MNUMCR]

    #               Synthetic Dye and Pigment/Other
    # T100(130,IY,IS)=MC_REVIND(11,52,IY)
    z[130] = dfd["MC_REVIND"].loc[52].loc[MNUMCR]

    #             Organic
    # T100(21:61,IY,IS)=MC_REVIND(11,1:41,IY)
    z[36] = dfd["MC_REVIND"].loc[16].loc[MNUMCR]

    #               Petrochemical Manufacturing
    # T100(121,IY,IS)=MC_REVIND(11,49,IY)
    z[131] = dfd["MC_REVIND"].loc[49].loc[MNUMCR]

    #               Ethanol
    # T100(21:61,IY,IS)=MC_REVIND(11,1:41,IY)
    z[37] = dfd["MC_REVIND"].loc[17].loc[MNUMCR]

    #               All Other Basic Organic Chem Man
    # T100(132,IY,IS)=MC_REVIND(11,50,IY)
    z[132] = dfd["MC_REVIND"].loc[50].loc[MNUMCR]

    #             Resin, Synthetic Rubber, and Fibers
    # T100(21:61,IY,IS)=MC_REVIND(11,1:41,IY)
    z[38] = dfd["MC_REVIND"].loc[18].loc[MNUMCR]

    #             Agricultural Chemicals
    # T100(21:61,IY,IS)=MC_REVIND(11,1:41,IY)
    z[39] = dfd["MC_REVIND"].loc[19].loc[MNUMCR]

    #           Other Chemical Products
    # T100(21:61,IY,IS)=MC_REVIND(11,1:41,IY)
    z[40] = dfd["MC_REVIND"].loc[20].loc[MNUMCR]

    #         Chemical Manufacturing
    # T100(70,IY,IS)=T100(69,IY,IS)+T100(40,IY,IS)
    z[70] = z[69] + z[40]
    #             Pharma Products
    # T100(21:61,IY,IS)=MC_REVIND(11,1:41,IY)
    z[41] = dfd["MC_REVIND"].loc[21].loc[MNUMCR]

    #             Paint Products
    # T100(21:61,IY,IS)=MC_REVIND(11,1:41,IY)
    z[42] = dfd["MC_REVIND"].loc[22].loc[MNUMCR]

    #             Soaps and Cleaning Products
    # T100(21:61,IY,IS)=MC_REVIND(11,1:41,IY)
    z[43] = dfd["MC_REVIND"].loc[23].loc[MNUMCR]

    #             Other
    # T100(21:61,IY,IS)=MC_REVIND(11,1:41,IY)
    z[44] = dfd["MC_REVIND"].loc[24].loc[MNUMCR]

    #         Petroleum and Coal Products
    # T100(71,IY,IS)=MC_REVIND(11,25,IY)+MC_REVIND(11,26,IY)
    z[71] = dfd["MC_REVIND"].loc[25].loc[MNUMCR] + dfd["MC_REVIND"].loc[26].loc[MNUMCR]

    #           Petroleum Refineries
    # T100(21:61,IY,IS)=MC_REVIND(11,1:41,IY)
    z[45] = dfd["MC_REVIND"].loc[25].loc[MNUMCR]

    #           Other Petroleum and Coal Products
    # T100(21:61,IY,IS)=MC_REVIND(11,1:41,IY)
    z[46] = dfd["MC_REVIND"].loc[26].loc[MNUMCR]

    #         Plastics and Rubber Products
    # T100(21:61,IY,IS)=MC_REVIND(11,1:41,IY)
    z[47] = dfd["MC_REVIND"].loc[27].loc[MNUMCR]

    #           Glass and Glass Products
    # T100(21:61,IY,IS)=MC_REVIND(11,1:41,IY)
    z[48] = dfd["MC_REVIND"].loc[28].loc[MNUMCR]

    #             Flat Glass
    # T100(21:61,IY,IS)=MC_REVIND(11,1:41,IY)
    z[49] = dfd["MC_REVIND"].loc[29].loc[MNUMCR]

    #             Container Glass
    # T100(133,IY,IS)=MC_REVIND(11,53,IY)
    z[133] = dfd["MC_REVIND"].loc[53].loc[MNUMCR]

    #             Manufac Excl. Flat and Contain Glass
    # T100(134,IY,IS)=MC_REVIND(11,54,IY)
    z[134] = dfd["MC_REVIND"].loc[54].loc[MNUMCR]

    #           Cement and Lime
    # T100(72,IY,IS)=MC_REVIND(11,30,IY)+MC_REVIND(11,31,IY)
    z[72] = dfd["MC_REVIND"].loc[30].loc[MNUMCR] + dfd["MC_REVIND"].loc[31].loc[MNUMCR]

    #             Cement
    # T100(21:61,IY,IS)=MC_REVIND(11,1:41,IY)
    z[50] = dfd["MC_REVIND"].loc[30].loc[MNUMCR]

    #             Lime
    # T100(51,IY,IS)=MC_REVIND(11,55,IY)
    z[51] = dfd["MC_REVIND"].loc[55].loc[MNUMCR]

    #           Other Nonmetallic Mineral Products
    # T100(21:61,IY,IS)=MC_REVIND(11,1:41,IY)
    z[52] = dfd["MC_REVIND"].loc[32].loc[MNUMCR]

    #         Stone, Clay, and Glass Products
    # T100(73,IY,IS)=T100(48,IY,IS)+T100(72,IY,IS)+T100(52,IY,IS)
    z[73] = z[48] + z[72] + z[52]

    #         Primary Metals Industry
    # T100(74,IY,IS)=SUM(MC_REVIND(11,33:35,IY))
    z[74] = dfd["MC_REVIND"].loc[range(33, 36)].groupby(level=1).sum().loc[MNUMCR]

    #           Iron and Steel Mills and Products
    # T100(21:61,IY,IS)=MC_REVIND(11,1:41,IY)
    z[53] = dfd["MC_REVIND"].loc[33].loc[MNUMCR]

    #           Alumina and Aluminum Products
    # T100(21:61,IY,IS)=MC_REVIND(11,1:41,IY)
    z[54] = dfd["MC_REVIND"].loc[34].loc[MNUMCR]

    #           Other Primary Metal Products
    # T100(21:61,IY,IS)=MC_REVIND(11,1:41,IY)
    z[55] = dfd["MC_REVIND"].loc[35].loc[MNUMCR]

    #         Fabricated Metal Products
    # T100(21:61,IY,IS)=MC_REVIND(11,1:41,IY)
    z[56] = dfd["MC_REVIND"].loc[36].loc[MNUMCR]

    #         Machinery
    # T100(21:61,IY,IS)=MC_REVIND(11,1:41,IY)
    z[57] = dfd["MC_REVIND"].loc[37].loc[MNUMCR]

    #         Computers and Electronics
    # T100(21:61,IY,IS)=MC_REVIND(11,1:41,IY)
    z[58] = dfd["MC_REVIND"].loc[38].loc[MNUMCR]

    #         Transportation Equipment
    # T100(21:61,IY,IS)=MC_REVIND(11,1:41,IY)
    z[59] = dfd["MC_REVIND"].loc[39].loc[MNUMCR]

    #         Electrical Equipment
    # T100(21:61,IY,IS)=MC_REVIND(11,1:41,IY)
    z[60] = dfd["MC_REVIND"].loc[40].loc[MNUMCR]

    #         Miscellaneous Manufacturing
    # T100(21:61,IY,IS)=MC_REVIND(11,1:41,IY)
    z[61] = dfd["MC_REVIND"].loc[41].loc[MNUMCR]

    # Disposable income by region
    #      T100(11:19,IY,IS) = MC_YPDR(1:9,IY)
    #      T100(20,IY,IS) = MC_YPDR(MNUMCR,IY)
    z[11] = dfd["MC_YPDR"].loc[1]
    z[12] = dfd["MC_YPDR"].loc[2]
    z[13] = dfd["MC_YPDR"].loc[3]
    z[14] = dfd["MC_YPDR"].loc[4]
    z[15] = dfd["MC_YPDR"].loc[5]
    z[16] = dfd["MC_YPDR"].loc[6]
    z[17] = dfd["MC_YPDR"].loc[7]
    z[18] = dfd["MC_YPDR"].loc[8]
    z[19] = dfd["MC_YPDR"].loc[9]
    z[20] = dfd["MC_YPDR"].loc[MNUMCR]

    #     Non-Industrial and Service Sectors
    # T100(127,IY,IS)=SUM(MC_EMPNA(11,28:39,IY))
    z[127] = dfd["MC_EMPNA"].loc[11].loc[28:39].sum()

    #       Total Nonmanufacturing
    # T100(118,IY,IS)=SUM(MC_EMPNA(11,20:27,IY))
    z[118] = dfd["MC_EMPNA"].loc[11].loc[20:27].sum()

    #         Agriculture/Forestry/Fishing/Hunting
    # T100(112,IY,IS)=SUM(MC_EMPNA(11,20:21,IY))
    z[112] = dfd["MC_EMPNA"].loc[11].loc[20:21].sum()

    #         Mining
    # T100(113,IY,IS)=SUM(MC_EMPNA(11,22:24,IY))
    z[113] = dfd["MC_EMPNA"].loc[11].loc[22:24].sum()

    #           Coal Mining
    # T100(114:116,IY,IS)=MC_EMPNA(11,22:24,IY)
    z[114] = dfd["MC_EMPNA"].loc[11].loc[22]

    #           Oil and Gas Extraction and Support
    # T100(114:116,IY,IS)=MC_EMPNA(11,22:24,IY)
    z[115] = dfd["MC_EMPNA"].loc[11].loc[23]

    #           Other Mining
    # T100(114:116,IY,IS)=MC_EMPNA(11,22:24,IY)
    z[116] = dfd["MC_EMPNA"].loc[11].loc[24]

    #         Construction
    # T100(117,IY,IS)=SUM(MC_EMPNA(11,25:27,IY))
    z[117] = dfd["MC_EMPNA"].loc[11].loc[25:27].sum()

    #           Residential
    # T100(109:111,IY,IS)=MC_EMPNA(11,25:27,IY)
    z[109] = dfd["MC_EMPNA"].loc[11].loc[25]

    #           Nonresidential
    # T100(109:111,IY,IS)=MC_EMPNA(11,25:27,IY)
    z[110] = dfd["MC_EMPNA"].loc[11].loc[26]

    #           Heavy
    # T100(109:111,IY,IS)=MC_EMPNA(11,25:27,IY)
    z[111] = dfd["MC_EMPNA"].loc[11].loc[27]

    #       Manufacturing Sector
    # T100(125,IY,IS)=SUM(MC_EMPNA(11,1:19,IY))
    z[125] = dfd["MC_EMPNA"].loc[11].loc[1:19].sum()

    #     Total Industrial
    # T100(126,IY,IS)=T100(118,IY,IS)+T100(125,IY,IS)
    z[126] = z[118] + z[125]

    #   Total Employment (millions)
    # T100(128,IY,IS)=T100(126,IY,IS)+T100(127,IY,IS)
    z[128] = z[126] + z[127]

    #         Food Products
    # T100(79:97,IY,IS)=MC_EMPNA(11,1:19,IY)
    z[79] = dfd["MC_EMPNA"].loc[11].loc[1]

    #         Beverages and Tobacco Products
    # T100(79:97,IY,IS)=MC_EMPNA(11,1:19,IY)
    z[80] = dfd["MC_EMPNA"].loc[11].loc[2]

    #         Textile Mills and Products
    # T100(79:97,IY,IS)=MC_EMPNA(11,1:19,IY)
    z[81] = dfd["MC_EMPNA"].loc[11].loc[3]

    #         Wood Products
    # T100(79:97,IY,IS)=MC_EMPNA(11,1:19,IY)
    z[82] = dfd["MC_EMPNA"].loc[11].loc[4]

    #         Furniture and Related Products
    # T100(79:97,IY,IS)=MC_EMPNA(11,1:19,IY)
    z[83] = dfd["MC_EMPNA"].loc[11].loc[5]

    #         Paper Products
    # T100(79:97,IY,IS)=MC_EMPNA(11,1:19,IY)
    z[84] = dfd["MC_EMPNA"].loc[11].loc[6]

    #         Printing
    # T100(79:97,IY,IS)=MC_EMPNA(11,1:19,IY)
    z[85] = dfd["MC_EMPNA"].loc[11].loc[7]

    #           Bulk Chemicals
    # T100(79:97,IY,IS)=MC_EMPNA(11,1:19,IY)
    z[86] = dfd["MC_EMPNA"].loc[11].loc[8]

    #           Other Chemical Products
    # T100(79:97,IY,IS)=MC_EMPNA(11,1:19,IY)
    z[87] = dfd["MC_EMPNA"].loc[11].loc[9]

    #         Chemical Manufacturing
    # T100(120,IY,IS)=T100(86,IY,IS)+T100(87,IY,IS)
    z[120] = z[86] + z[87]

    #         Petroleum and Coal Products
    # T100(79:97,IY,IS)=MC_EMPNA(11,1:19,IY)
    z[88] = dfd["MC_EMPNA"].loc[11].loc[10]

    #         Plastics and Rubber Products
    # T100(79:97,IY,IS)=MC_EMPNA(11,1:19,IY)
    z[89] = dfd["MC_EMPNA"].loc[11].loc[11]

    #         Nonmetallic Mineral Products
    # T100(79:97,IY,IS)=MC_EMPNA(11,1:19,IY)
    z[90] = dfd["MC_EMPNA"].loc[11].loc[12]

    #         Primary Metals Industry
    # T100(79:97,IY,IS)=MC_EMPNA(11,1:19,IY)
    z[91] = dfd["MC_EMPNA"].loc[11].loc[13]

    #         Fabricated Metal Products
    # T100(79:97,IY,IS)=MC_EMPNA(11,1:19,IY)
    z[92] = dfd["MC_EMPNA"].loc[11].loc[14]

    #         Machinery
    # T100(79:97,IY,IS)=MC_EMPNA(11,1:19,IY)
    z[93] = dfd["MC_EMPNA"].loc[11].loc[15]

    #         Computers and Electronics
    # T100(79:97,IY,IS)=MC_EMPNA(11,1:19,IY)
    z[94] = dfd["MC_EMPNA"].loc[11].loc[16]

    #         Transportation Equipment
    # T100(79:97,IY,IS)=MC_EMPNA(11,1:19,IY)
    z[95] = dfd["MC_EMPNA"].loc[11].loc[17]
    z[96] = dfd["MC_EMPNA"].loc[11].loc[18]
    z[97] = dfd["MC_EMPNA"].loc[11].loc[19]

    # Employment by region
    for IR in range(1, 10):
        z[IR] = (
            dfd["MC_EMPNA"].loc[IR, 1:19, :].sum()
            + dfd["MC_EMPNA"].loc[IR, 22:39, :].sum()
        )

    #       DO IR=1,9
    #         T100(IR,IY,IS) = sum(MC_EMPNA(IR,1:19,IY)) + sum(MC_EMPNA(IR,22:39,IY))
    #       ENDDO
    #       T100(10,IY,IS) = sum(MC_EMPNA(MNUMCR,1:19,IY)) + sum(MC_EMPNA(MNUMCR,22:39,IY))
    z[10] = (
        dfd["MC_EMPNA"].loc[MNUMCR, 1:19, :].sum()
        + dfd["MC_EMPNA"].loc[MNUMCR, 22:39, :].sum()
    )

    return z
