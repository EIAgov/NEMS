#-*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_115(dfd, table_spec, table_id):
    """Fill table for New Light-Duty Vehicle Range
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
    TNUMCLASS=dfd['TNUMCLASS_rwpre']  
    # NUMREP = TNUMCLASS-1
    NUMREP = int(TNUMCLASS-1) 
       
    for II in range(1,8+1):
#        --- GASOLINE
       z[0*NUMREP+II] = dfd['LDVRNG'].loc[1,1,II]
       z[1*NUMREP+II] = dfd['LDVRNG'].loc[2,1,II]
#         --- TDI DIESEL
       z[ 2*NUMREP+II] =dfd['LDVRNG'].loc[1, 2,II]
       z[3*NUMREP+II] =dfd['LDVRNG'].loc[2, 2,II]
#         --- PLUG-IN 10 GASOLINE HYBRID
       z[4*NUMREP+II] =dfd['LDVRNG'].loc[1, 5,II]
       z[5*NUMREP+II] =dfd['LDVRNG'].loc[2, 5,II]
#         --- PLUG-IN 40 GASOLINE HYBRID
       z[6*NUMREP+II] =dfd['LDVRNG'].loc[1, 6,II]
       z[7*NUMREP+II] =dfd['LDVRNG'].loc[2, 6,II]
#         --- ETHANOL FLEX
       z[8*NUMREP+II] =dfd['LDVRNG'].loc[1, 3,II]
       z[9*NUMREP+II] =dfd['LDVRNG'].loc[2, 3,II]
#         --- CNG
       z[10*NUMREP+II] =dfd['LDVRNG'].loc[1,11,II]
       z[11*NUMREP+II] =dfd['LDVRNG'].loc[2,11,II]
#         --- CNG BI-FUEL
       z[12*NUMREP+II] =dfd['LDVRNG'].loc[1, 9,II]
       z[13*NUMREP+II] =dfd['LDVRNG'].loc[2, 9,II]
#         --- LPG
       z[14*NUMREP+II] =dfd['LDVRNG'].loc[1,12,II]
       z[15*NUMREP+II] =dfd['LDVRNG'].loc[2,12,II]
#         --- LPG BI-FUEL
       z[16*NUMREP+II] =dfd['LDVRNG'].loc[1,10,II]
       z[17*NUMREP+II] =dfd['LDVRNG'].loc[2,10,II]
#         --- 100 MILE ELECTRIC
       z[18*NUMREP+II] =dfd['LDVRNG'].loc[1, 4,II]
       z[19*NUMREP+II] =dfd['LDVRNG'].loc[2, 4,II]
#         --- 200 MILE ELECTRIC
       z[20*NUMREP+II] =dfd['LDVRNG'].loc[1, 7,II]
       z[21*NUMREP+II] =dfd['LDVRNG'].loc[2, 7,II]
#         --- 300 MILE ELECTRIC
       z[22*NUMREP+II] =dfd['LDVRNG'].loc[1,15,II]
       z[23*NUMREP+II] =dfd['LDVRNG'].loc[2,15,II]
#         --- DIESEL-ELECTRIC HYBRID
       z[24*NUMREP+II] =dfd['LDVRNG'].loc[1, 8,II]
       z[25*NUMREP+II] =dfd['LDVRNG'].loc[2, 8,II]
#         --- GASOLINE-ELECTRIC HYBRID
       z[26*NUMREP+II] =dfd['LDVRNG'].loc[1,16,II]
       z[27*NUMREP+II] =dfd['LDVRNG'].loc[2,16,II]
#         --- FUEL CELL METHANOL
       z[28*NUMREP+II] =dfd['LDVRNG'].loc[1,13,II]
       z[29*NUMREP+II] =dfd['LDVRNG'].loc[2,13,II]
#         --- FUEL CELL HYDROGEN
       z[30*NUMREP+II] =dfd['LDVRNG'].loc[1,14,II]
       z[31*NUMREP+II] =dfd['LDVRNG'].loc[2,14,II]       
 
    return z