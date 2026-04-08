# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""

from RW_preprocessor import size_class_car, size_class_trk

def fill_table_base_052(dfd, table_spec, table_id):
    """Fill table  Summary of New Light-Duty Vehicle Size Class Attributes
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

    #   Summary of New Light-Duty Vehicle Size Class Attributes
    #    Class Attributes                                           
    #                                                               
    #   Personal Vehicles                                           
    #      EPA Rated New Vehicle Fuel Efficiency                    

    #         Conventional Cars (miles per gallon)                  
    z[1] = dfd['TREFFCAR'].loc[size_class_car['minicompact']]
    z[2] = dfd['TREFFCAR'].loc[size_class_car['subcompact']]
    z[3] = dfd['TREFFCAR'].loc[size_class_car['compact']]
    z[4] = dfd['TREFFCAR'].loc[size_class_car['midsize']]
    z[5] = dfd['TREFFCAR'].loc[size_class_car['large']]
    z[6] = dfd['TREFFCAR'].loc[size_class_car['two_seat']]
    z[7] = dfd['TREFFCAR'].loc[size_class_car['small_cuv']]
    z[8] = dfd['TREFFCAR'].loc[size_class_car['large_cuv']]

    #         Average New Car
    z[9] = dfd['TREFFCAR'].loc[9]

    #         Average New Car On-Road
    z[10] = dfd['TREFFCAR'].loc[10]

    #         Conventional Light Trucks
    z[11] = dfd['TREFFTRK'].loc[size_class_trk['small_pickup']]
    z[12] = dfd['TREFFTRK'].loc[size_class_trk['stnd_pickup']]
    z[13] = dfd['TREFFTRK'].loc[size_class_trk['small_van']]
    z[14] = dfd['TREFFTRK'].loc[size_class_trk['stnd_van']]
    z[15] = dfd['TREFFTRK'].loc[size_class_trk['small_suv']]
    z[16] = dfd['TREFFTRK'].loc[size_class_trk['large_suv']]
    z[17] = dfd['TREFFTRK'].loc[size_class_trk['small_cuv']]
    z[18] = dfd['TREFFTRK'].loc[size_class_trk['large_cuv']]

    #         Average New Light Truck
    z[19] = dfd['TREFFTRK'].loc[9]

    #         Average New Light Truck On-Road
    z[20] = dfd['TREFFTRK'].loc[10]


    #      Degradation Factors 1/
    #         Cars
    z[21] = dfd['DEGRPT'].loc[1]

    #         Light Trucks
    z[22] = dfd['DEGRPT'].loc[2]


    #      New Fuel Efficiency by Size Class 2/
    #         Alternative-Fuel Cars
    z[23] = dfd['TREFFALTC'].loc[size_class_car['minicompact']]
    z[24] = dfd['TREFFALTC'].loc[size_class_car['subcompact']]
    z[25] = dfd['TREFFALTC'].loc[size_class_car['compact']]
    z[26] = dfd['TREFFALTC'].loc[size_class_car['midsize']]
    z[27] = dfd['TREFFALTC'].loc[size_class_car['large']]
    z[28] = dfd['TREFFALTC'].loc[size_class_car['two_seat']]
    z[29] = dfd['TREFFALTC'].loc[size_class_car['small_cuv']]
    z[30] = dfd['TREFFALTC'].loc[size_class_car['large_cuv']]

    #         Average New Alternative Cars
    z[31] = dfd['TREFFALTC'].loc[9]


    #         Alternative-Fuel Light Trucks
    z[32] = dfd['TREFFALTT'].loc[size_class_trk['small_pickup']]
    z[33] = dfd['TREFFALTT'].loc[size_class_trk['stnd_pickup']]
    z[34] = dfd['TREFFALTT'].loc[size_class_trk['small_van']]
    z[35] = dfd['TREFFALTT'].loc[size_class_trk['stnd_van']]
    z[36] = dfd['TREFFALTT'].loc[size_class_trk['small_suv']]
    z[37] = dfd['TREFFALTT'].loc[size_class_trk['large_suv']]
    z[38] = dfd['TREFFALTT'].loc[size_class_trk['small_cuv']]
    z[39] = dfd['TREFFALTT'].loc[size_class_trk['large_cuv']]

    #         Average New Alternative Light Trucks
    z[40] = dfd['TREFFALTT'].loc[9]


    #   Fleet Vehicles

    #      EPA Rated New Vehicle Fuel Efficiency
    #         Cars
    z[41] = dfd['TREFFFLT'].loc[1]

    #         Light Trucks
    z[42] = dfd['TREFFFLT'].loc[2]


    #      Average On-Road Miles per Gallon
    #         Cars
    z[43] = dfd['TREFFFLT'].loc[3]

    #         Light Trucks
    z[44] = dfd['TREFFFLT'].loc[4]


    #   New Vehicle Sales Shares (percent)
    #      Cars
    z[45] = dfd['TRSLSHRC'].loc[size_class_car['minicompact']] * 100.
    z[46] = dfd['TRSLSHRC'].loc[size_class_car['subcompact']] * 100.
    z[47] = dfd['TRSLSHRC'].loc[size_class_car['compact']] * 100.
    z[48] = dfd['TRSLSHRC'].loc[size_class_car['midsize']] * 100.
    z[49] = dfd['TRSLSHRC'].loc[size_class_car['large']] * 100.
    z[50] = dfd['TRSLSHRC'].loc[size_class_car['two_seat']] * 100.
    z[51] = dfd['TRSLSHRC'].loc[size_class_car['small_cuv']] * 100.
    z[52] = dfd['TRSLSHRC'].loc[size_class_car['large_cuv']] * 100.

    z[99] = dfd['TRSLSHRC'].loc[1:8].sum() * 100

    #      Light Trucks
    z[53] = dfd['TRSLSHRT'].loc[size_class_trk['small_pickup']] * 100.
    z[54] = dfd['TRSLSHRT'].loc[size_class_trk['stnd_pickup']] * 100.
    z[55] = dfd['TRSLSHRT'].loc[size_class_trk['small_van']] * 100.
    z[56] = dfd['TRSLSHRT'].loc[size_class_trk['stnd_van']] * 100.
    z[57] = dfd['TRSLSHRT'].loc[size_class_trk['small_suv']] * 100.
    z[58] = dfd['TRSLSHRT'].loc[size_class_trk['large_suv']] * 100.
    z[59] = dfd['TRSLSHRT'].loc[size_class_trk['small_cuv']] * 100.
    z[60] = dfd['TRSLSHRT'].loc[size_class_trk['large_cuv']] * 100.

    z[100] = dfd['TRSLSHRT'].loc[1:8].sum() * 100


    #   New Vehicle Average Horsepower
    #      Conventional Cars
    z[61] = dfd['TRHPCAR'].loc[size_class_car['minicompact']]
    z[62] = dfd['TRHPCAR'].loc[size_class_car['subcompact']]
    z[63] = dfd['TRHPCAR'].loc[size_class_car['compact']]
    z[64] = dfd['TRHPCAR'].loc[size_class_car['midsize']]
    z[65] = dfd['TRHPCAR'].loc[size_class_car['large']]
    z[66] = dfd['TRHPCAR'].loc[size_class_car['two_seat']]
    z[67] = dfd['TRHPCAR'].loc[size_class_car['small_cuv']]
    z[68] = dfd['TRHPCAR'].loc[size_class_car['large_cuv']]

    #      Average New Car
    z[69] = dfd['TRHPCAR'].loc[9]


    #      Conventional Light Trucks
    z[70] = dfd['TRHPTRK'].loc[size_class_trk['small_pickup']]
    z[71] = dfd['TRHPTRK'].loc[size_class_trk['stnd_pickup']]
    z[72] = dfd['TRHPTRK'].loc[size_class_trk['small_van']]
    z[73] = dfd['TRHPTRK'].loc[size_class_trk['stnd_van']]
    z[74] = dfd['TRHPTRK'].loc[size_class_trk['small_suv']]
    z[75] = dfd['TRHPTRK'].loc[size_class_trk['large_suv']]
    z[76] = dfd['TRHPTRK'].loc[size_class_trk['small_cuv']]
    z[77] = dfd['TRHPTRK'].loc[size_class_trk['large_cuv']]

    #      Average New Light Truck
    z[78] = dfd['TRHPTRK'].loc[9]


    #   New Vehicle Average Weight
    #      Conventional Cars
    z[79] = dfd['TRWTCAR'].loc[size_class_car['minicompact']]
    z[80] = dfd['TRWTCAR'].loc[size_class_car['subcompact']]
    z[81] = dfd['TRWTCAR'].loc[size_class_car['compact']]
    z[82] = dfd['TRWTCAR'].loc[size_class_car['midsize']]
    z[83] = dfd['TRWTCAR'].loc[size_class_car['large']]
    z[84] = dfd['TRWTCAR'].loc[size_class_car['two_seat']]
    z[85] = dfd['TRWTCAR'].loc[size_class_car['small_cuv']]
    z[86] = dfd['TRWTCAR'].loc[size_class_car['large_cuv']]

    #      Average New Car
    z[87] = dfd['TRWTCAR'].loc[9]


    #      Conventional Light Trucks
    z[88] = dfd['TRWTTRK'].loc[size_class_trk['small_pickup']]
    z[89] = dfd['TRWTTRK'].loc[size_class_trk['stnd_pickup']]
    z[90] = dfd['TRWTTRK'].loc[size_class_trk['small_van']]
    z[91] = dfd['TRWTTRK'].loc[size_class_trk['stnd_van']]
    z[92] = dfd['TRWTTRK'].loc[size_class_trk['small_suv']]
    z[93] = dfd['TRWTTRK'].loc[size_class_trk['large_suv']]
    z[94] = dfd['TRWTTRK'].loc[size_class_trk['small_cuv']]
    z[95] = dfd['TRWTTRK'].loc[size_class_trk['large_cuv']]

    #      Average New Light Truck
    z[96] = dfd['TRWTTRK'].loc[9]


    #   Average Weight for the Stock

    #      Conventional Cars
    z[97] = dfd['TRWTCAR_STOCK']

    #      Conventional Light Trucks
    z[98] = dfd['TRWTTRK_STOCK']
    
    return z
 