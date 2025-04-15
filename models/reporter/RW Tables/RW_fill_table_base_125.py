#-*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_125(dfd, table_spec, table_id):
    """Fill table for Hydrogen Model Results
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
    
   
    hydrogen_factor = dfd['hydrogen_factor_rwpre']
    GWh_to_TRIL = dfd['GWh_to_TRIL_rwpre']
    HMKT = 3
    
    dfd['HMMPRD'] = dfd['HMMPRD'].swaplevel()
    dfd['HMMPRD'] = dfd['HMMPRD'].swaplevel(0)
    
    
 
# !         Hydrogen Production
          
          #HMMPRD(IY,IR,K,J) (MNUMYR,MNUMCR,     3,    26) 
    z[1] = dfd['HMMPRD'].loc[1,1] + dfd['HMMPRD'].loc[2,1] + dfd['HMMPRD'].loc[3,1]
    z[2] = dfd['HMMPRD'].loc[1,2] + dfd['HMMPRD'].loc[2,2] + dfd['HMMPRD'].loc[3,2]
    z[3] = dfd['HMMPRD'].loc[1,3] + dfd['HMMPRD'].loc[2,3] + dfd['HMMPRD'].loc[3,3]
    z[4] = dfd['HMMPRD'].loc[1,4] + dfd['HMMPRD'].loc[2,4] + dfd['HMMPRD'].loc[3,4]
    z[5] = dfd['HMMPRD'].loc[1,5] + dfd['HMMPRD'].loc[2,5] + dfd['HMMPRD'].loc[3,5]
    z[6] = dfd['HMMPRD'].loc[1,6] + dfd['HMMPRD'].loc[2,6] + dfd['HMMPRD'].loc[3,6]
    z[7] = dfd['HMMPRD'].loc[1,7] + dfd['HMMPRD'].loc[2,7] + dfd['HMMPRD'].loc[3,7]
    z[8] = dfd['HMMPRD'].loc[1,8] + dfd['HMMPRD'].loc[2,8] + dfd['HMMPRD'].loc[3,8]
    z[9] = dfd['HMMPRD'].loc[1,9] + dfd['HMMPRD'].loc[2,9] + dfd['HMMPRD'].loc[3,9]
    #z[10] =  z[1] + z[2] + z[3] + z[4] + z[5] + z[6] + z[7] + z[8] + z[9]   
    # Large City Market        
    z[76] = dfd['HMMPRD'].loc[1,1]
    z[77] = dfd['HMMPRD'].loc[1,2]
    z[78] = dfd['HMMPRD'].loc[1,3]
    z[79] = dfd['HMMPRD'].loc[1,4]
    z[80] = dfd['HMMPRD'].loc[1,5]
    z[81] = dfd['HMMPRD'].loc[1,6]
    z[82] = dfd['HMMPRD'].loc[1,7]
    z[83] = dfd['HMMPRD'].loc[1,8]      
    z[84] = dfd['HMMPRD'].loc[1,9]    
   # z[85] =  z[76]+z[77]+z[78]+z[79]+z[80]+z[81]+z[82]+z[83]+z[84]    
    #  Small City Market
    z[89] = dfd['HMMPRD'].loc[2,1]
    z[90] = dfd['HMMPRD'].loc[2,2]
    z[91] = dfd['HMMPRD'].loc[2,3]
    z[92] = dfd['HMMPRD'].loc[2,4]
    z[93] = dfd['HMMPRD'].loc[2,5]
    z[94] = dfd['HMMPRD'].loc[2,6]
    z[95] = dfd['HMMPRD'].loc[2,7]
    z[96] = dfd['HMMPRD'].loc[2,8]      
    z[97] = dfd['HMMPRD'].loc[2,9]    
   # z[98] =  z[89]+z[90]+z[91]+z[92]+z[93]+z[94]+z[95]+z[96]+z[97]
    #  Rural Market
    z[102] = dfd['HMMPRD'].loc[3,1]
    z[103] = dfd['HMMPRD'].loc[2,2]
    z[104] = dfd['HMMPRD'].loc[2,3]
    z[105] = dfd['HMMPRD'].loc[2,4]
    z[106] = dfd['HMMPRD'].loc[2,5]
    z[107] = dfd['HMMPRD'].loc[2,6]
    z[108] = dfd['HMMPRD'].loc[2,7]
    z[109] = dfd['HMMPRD'].loc[2,8]      
    z[110] = dfd['HMMPRD'].loc[2,9]    
   # z[111] =  z[102]+z[103]+z[104]+z[105]+z[106]+z[107]+z[108]+z[109]+z[110]
    #  Hydrogen Transport (Large City ONLY)
      
    z[20] = dfd['HMMPRD'].loc[1,1] + dfd['HMMPRD'].loc[1,4]
    z[21] = dfd['HMMPRD'].loc[1,2] + dfd['HMMPRD'].loc[1,5]
    z[22] = dfd['HMMPRD'].loc[1,3] + dfd['HMMPRD'].loc[1,6]    
    z[23] = z[20] + z[21] + z[22]
    #  Hydrogen Distribution
    z[24] = dfd['HMMPRD'].loc[1,17] + dfd['HMMPRD'].loc[1,20] + dfd['HMMPRD'].loc[2,17] + dfd['HMMPRD'].loc[2,20] + dfd['HMMPRD'].loc[3,17] + dfd['HMMPRD'].loc[3,20] 
    z[25] = dfd['HMMPRD'].loc[1,18] + dfd['HMMPRD'].loc[1,21] + dfd['HMMPRD'].loc[2,18] + dfd['HMMPRD'].loc[2,21] + dfd['HMMPRD'].loc[3,18] + dfd['HMMPRD'].loc[3,21]
    z[26] = dfd['HMMPRD'].loc[1,16] + dfd['HMMPRD'].loc[2,16] + dfd['HMMPRD'].loc[3,16]
    z[27] = z[24] + z[25] + z[26] 
    #    Large City
    z[124] = dfd['HMMPRD'].loc[1,17] + dfd['HMMPRD'].loc[1,20]
    z[125] = dfd['HMMPRD'].loc[1,18] + dfd['HMMPRD'].loc[1,21]
    z[126] = dfd['HMMPRD'].loc[1,16]
    z[127] = z[124] + z[125] + z[126] 
    #    Small City     
    z[128] = dfd['HMMPRD'].loc[2,17] + dfd['HMMPRD'].loc[2,20]
    z[129] = dfd['HMMPRD'].loc[2,18] + dfd['HMMPRD'].loc[2,21]
    z[130] = dfd['HMMPRD'].loc[2,16]
    z[131] = z[128] + z[129] + z[130] 
    #    Rural
    z[132] = dfd['HMMPRD'].loc[3,17] + dfd['HMMPRD'].loc[3,20]
    z[133] = dfd['HMMPRD'].loc[3,18] + dfd['HMMPRD'].loc[3,21]
    z[134] = dfd['HMMPRD'].loc[3,16]
    z[135] = z[132] + z[133] + z[134] 
    # Hydrogen Demand by market type
    z[42] = dfd['HMMPRD'].loc[1,1] + dfd['HMMPRD'].loc[1,2] + dfd['HMMPRD'].loc[1,3]
    z[43] = dfd['HMMPRD'].loc[2,1] + dfd['HMMPRD'].loc[2,2] + dfd['HMMPRD'].loc[2,3]
    z[44] = dfd['HMMPRD'].loc[3,1] + dfd['HMMPRD'].loc[3,2] + dfd['HMMPRD'].loc[3,3]
    z[45] = z[44]*0.0
    z[46] = z[42] +z[43]+ z[44] +z[45]
    #      Fuel Consumption
    z[47] = dfd['HMCLCNS'].loc[1] + dfd['HMCLCNS'].loc[2] + dfd['HMCLCNS'].loc[3]
    z[48] = dfd['HMGSCNS'].loc[1] + dfd['HMGSCNS'].loc[2] + dfd['HMGSCNS'].loc[3]
    z[49] = dfd['HMBICNS'].loc[1] + dfd['HMBICNS'].loc[2] + dfd['HMBICNS'].loc[3]
    z[50] = dfd['HMURCNS'].loc[1] + dfd['HMURCNS'].loc[2] + dfd['HMURCNS'].loc[3]
    z[51] = dfd['HMELCNS'].loc[1] + dfd['HMELCNS'].loc[2] + dfd['HMELCNS'].loc[3]
    z[31] = dfd['HMETCNS'].loc[1] + dfd['HMETCNS'].loc[2] + dfd['HMETCNS'].loc[3]
    z[52] = z[47] + z[48] +z[49] +z[50] +z[51] +z[31]
     
    # Compute MARGINAL hydrogen price in $/kg for each market in pricing year dollars
    z[53] = dfd['PH1TR'] * hydrogen_factor
    z[54] = dfd['PH2TR'] * hydrogen_factor
    z[55] = dfd['PH3TR'] * hydrogen_factor  
    z[56] = z[55] * 0.0
    
# !     Compute average marginal hydrogen price over all uses

       
      # IF (z[46] .GT. 0.000001) THEN
    z[57] = (z[53] * z[42] + z[54] * z[43] + z[55] * z[44]) / z[46]
        # else
         # z[57] = z[56] * 0.0
        # endif

# !     Combined heat and power
# CGHMMGEN(IR,IY,1,1) 
# CGHMMCAP(IR,IY,1)
    z[136] = dfd['CGHMMGEN'].loc[1,1]  * 0.001
    z[137] = dfd['CGHMMCAP'].loc[1]  * 0.001
    z[138] = dfd['PELBS'] * GWh_to_TRIL

#      Hydrogen Emissions

    z[62] = dfd['EMHM'].loc[1,1]
    z[63] = dfd['EMHM'].loc[2,1]
    z[64] = dfd['EMHM'].loc[3,1]
            
    return z