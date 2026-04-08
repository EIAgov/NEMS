# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_046(dfd, table_spec, table_id):
    """Fill table    Transportation Sector Energy Use by Fuel Type Within a Mode
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

    MNUMCR=dfd['MNUMCR_rwpre']

    #   Transportation Sector Energy Use by Fuel Type Within a Mode
    #   (trillion Btu)
    #    Mode and Type                                              
    
    #     Motor Gasoline excluding E85 1/
    z[1] = dfd['TRQLDV'].loc[1].loc[MNUMCR]
 
    #     E85 1/ 
    z[3] = dfd['TRQLDV'].loc[3].loc[MNUMCR]
 
    #     Distillate Fuel Oil (diesel)                              
    z[8] = dfd['TRQLDV'].loc[8].loc[MNUMCR]
 
    #     M85                                                       
    z[2] = dfd['TRQLDV'].loc[2].loc[MNUMCR]
 
    #     Compressed/Liquefied Natural Gas                          
    z[4] = dfd['TRQLDV'].loc[4].loc[MNUMCR]
 
    #     Propane                                                   
    z[5] = dfd['TRQLDV'].loc[5].loc[MNUMCR]
 
    #     Electricity                                               
    z[6] = dfd['TRQLDV'].loc[6].loc[MNUMCR]
 
    #     Hydrogen                                                  
    z[7] = dfd['TRQLDV'].loc[7].loc[MNUMCR]
 
     #   Light-Duty Vehicle                                          
    z[9] = z[1]+z[2]+z[3]+z[4]+z[5]+z[6]+z[7]+z[8]
   

    #     Motor Gasoline excluding E85 1/                           
    z[86] = dfd['TTHCONS'].loc[1]
 
    #     E85 1/                                                    
    z[88] = 0 * z[86]
 
    #     Distillate Fuel Oil (diesel)                              
    z[93] = dfd['TTHCONS'].loc[2]
 
    #     M85                                                       
    z[87] = 0 * z[86]
 
    #     Compressed/Liquefied Natural Gas                          
    z[89] = dfd['TTHCONS'].loc[5]
 
    #     Propane                                                   
    z[90] = dfd['TTHCONS'].loc[3]
 
    #     Electricity                                               
    z[91] = dfd['TTHCONS'].loc[4]
 
    #     Hydrogen                                                  
    z[92] = dfd['TTHCONS'].loc[6]
 
     #   2- and 3- Wheel Vehicles                                    
    z[94] = z[86]+z[87]+z[88]+z[89]+z[90]+z[91]+z[92]+z[93]                                                               

    #     Motor Gasoline excluding E85 1/                           
    z[95] = z[1]-z[86]
 
    #     E85 1/                                                    
    z[97] = z[3]-z[88]
 
    #     Distillate Fuel Oil (diesel)                              
    z[102] = z[8]-z[93]
 
    #     M85                                                       
    z[96] = z[2]-z[87]
 
    #     Compressed/Liquefied Natural Gas                          
    z[98] = z[4]-z[89]
 
    #     Propane                                                   
    z[99] = z[5]-z[90]
 
    #     Electricity                                               
    z[100] = z[6]-z[91]
 
    #     Hydrogen                                                  
    z[101] = z[7]-z[92]
 
    ##   Light-Duty Vehicles excluding 2- and 3- Wheel               
    z[103] = z[95]+z[96]+z[97]+z[98]+z[99]+z[100]+z[101]+z[102]                                                              

 
    #     Motor Gasoline excluding E85 1/                           
    z[58] = dfd['CLTFUELBTU'].loc[1]
 
    #     E85 1/                                                    
    z[78] = dfd['CLTFUELBTU'].loc[5]
 
    #     Distillate Fuel Oil (diesel)                              
    z[59] = dfd['CLTFUELBTU'].loc[2]
 
    #     Propane                                                   
    z[76] = dfd['CLTFUELBTU'].loc[3]
 
    #     Compressed/Liquefied Natural Gas                          
    z[77] = dfd['CLTFUELBTU'].loc[4]
 
    #     Electricity                                               
    z[79] = dfd['CLTFUELBTU'].loc[6]
 
    #     Hydrogen                                                  
    z[80] = dfd['CLTFUELBTU'].loc[7]
 
    #   Commercial Light Trucks 2/                                  
    z[10] = z[58] + z[78] + z[59] + z[76] + z[77] + z[79] + z[80]

    #     Motor Gasoline                                            
    z[11] = dfd['TRQFTRK'].loc[1]
 
    #     Distillate Fuel Oil (diesel)                              
    z[12] = dfd['TRQFTRK'].loc[2]
 
    #     Compressed/Liquefied Natural Gas                          
    z[13] = dfd['TRQFTRK'].loc[3]
 
    #     Propane                                                   
    z[14] = dfd['TRQFTRK'].loc[5]
 
    #     E85 1/                                                    
    z[83] = dfd['TRQFTRK'].loc[6]
 
    #     Electricity                                               
    z[84] = dfd['TRQFTRK'].loc[7]
 
    #     Hydrogen                                                  
    z[85] = dfd['TRQFTRK'].loc[8]
 
     #   Freight Trucks 3/                                           
    z[15] = z[11]+z[12]+z[13]+z[14] +z[83]+z[84]+z[85]                                                              

    
 
    #     Distillate Fuel Oil (diesel)
    z[16] = dfd['TRQRRF'].loc[1]

    #     Residual Fuel Oil
    z[63] = dfd['TRQRRF'].loc[2]

    #     Compressed Natural Gas
    z[64] = dfd['TRQRRF'].loc[3]

    #     Liquefied Natural Gas
    z[65] = dfd['TRQRRF'].loc[4]

      #   Freight Rail 4/
    z[17] = z[16]+ z[63]+z[64]+z[65]


    #     Distillate Fuel Oil (diesel)
    z[18] = dfd['TRQDOMS'].loc[1]

    #     Residual Oil
    z[19] = dfd['TRQDOMS'].loc[2]

    #     Compressed Natural Gas
    z[20] = dfd['TRQDOMS'].loc[3]

    #     Liquefied Natural Gas
    z[66] = dfd['TRQDOMS'].loc[4]

     #   Domestic Shipping
    z[21] = z[18]+z[19]+z[20] +z[66]



    #     Distillate Fuel Oil (diesel)
    z[22] = dfd['TRQINTS'].loc[1]

    #     Residual Oil
    z[23] = dfd['TRQINTS'].loc[2]

    #     Compressed Natural Gas
    z[67] = dfd['TRQINTS'].loc[3]

    #     Liquefied Natural Gas
    z[68] = dfd['TRQINTS'].loc[4]

     #   International Shipping
    z[24] = z[22]+z[23] +z[67]+z[68]

    #     Jet Fuel
    z[25] = dfd['TRQAIRT'].loc[1]

    #     Aviation Gasoline
    z[26] = dfd['TRQAIRT'].loc[2]

     #   Air Transportation
    z[27] = z[25]+z[26]


    #     Jet Fuel and Aviation Gasoline
    z[28] = dfd['TRQMIL'].loc[1] + dfd['TRQMIL'].loc[2]

    #     Residual Fuel Oil
    z[29] = dfd['TRQMIL'].loc[3]

    #     Distillates and Diesel
    z[30] = dfd['TRQMIL'].loc[4]

     #   Military Use
    z[31] = z[28]+z[29]+z[30]


    #     Transit Bus
    z[60] = dfd['TRQBUS'].loc[1,1:8,:].sum()

    #       Motor Gasoline
    z[32] = dfd['TRQBUS'].loc[1].loc[1]

    #       E85 1/
    z[104] = dfd['TRQBUS'].loc[1].loc[3]

    #       Distillate Fuel Oil (diesel)
    z[33] = dfd['TRQBUS'].loc[1].loc[2]

    #       M85
    z[105] = dfd['TRQBUS'].loc[1].loc[4]

    #       Compressed/Liquefied Natural Gas
    z[51] = dfd['TRQBUS'].loc[1].loc[5]

    #       Propane
    z[52] = dfd['TRQBUS'].loc[1].loc[6]

    #       Electricity
    z[106] = dfd['TRQBUS'].loc[1].loc[7]

    #       Hydrogen
    z[107] = dfd['TRQBUS'].loc[1].loc[8]

    #     Intercity Bus
    z[61] = dfd['TRQBUS'].loc[2,1:8,:].sum()

    #       Motor Gasoline
    z[53] = dfd['TRQBUS'].loc[2].loc[1]

    #       E85 1/
    z[108] = dfd['TRQBUS'].loc[2].loc[3]

    #       Distillate Fuel Oil (diesel)
    z[34] = dfd['TRQBUS'].loc[2].loc[2]

    #       M85
    z[109] = dfd['TRQBUS'].loc[2].loc[4]

    #       Compressed/Liquefied Natural Gas
    z[54] = dfd['TRQBUS'].loc[2].loc[5]

    #       Propane
    z[55] = dfd['TRQBUS'].loc[2].loc[6]

    #       Electricity
    z[110] = dfd['TRQBUS'].loc[2].loc[7]

    #       Hydrogen
    z[111] = dfd['TRQBUS'].loc[2].loc[8]

    #     School Bus
    z[62] = dfd['TRQBUS'].loc[3].loc[1:8].sum()

    #       Motor Gasoline
    z[35] = dfd['TRQBUS'].loc[3].loc[1]

    #       E85 1/
    z[112] = dfd['TRQBUS'].loc[3].loc[3]

    #       Distillate Fuel Oil (diesel)
    z[36] = dfd['TRQBUS'].loc[3].loc[2]

    #       M85
    z[113] = dfd['TRQBUS'].loc[3].loc[4]

    #       Compressed/Liquefied Natural Gas
    z[56] = dfd['TRQBUS'].loc[3].loc[5]

    #       Propane
    z[57] = dfd['TRQBUS'].loc[3].loc[6]

    #       Electricity
    z[114] = dfd['TRQBUS'].loc[3].loc[7]

    #       Hydrogen
    z[115] = dfd['TRQBUS'].loc[3].loc[8]

    #   Bus Transportation
    z[37] = z[60]+z[61]+z[62]

    # Rail
    #       Electricity
    z[38] = dfd['TRQRRP'].loc[1]

    #       Diesel
    z[39] = dfd['TRQRRP'].loc[2]

    #       Compressed Natural Gas
    z[69] = dfd['TRQRRP'].loc[3]

    #       Liquefied Natural Gas
    z[70] = dfd['TRQRRP'].loc[4]

    #     Intercity Rail
    z[73] = z[38]+z[39] +z[69]+z[70]



    #       Electricity
    z[40] = dfd['TRQRRP'].loc[5]

     #     Transit Rail
    z[74] = z[40]



    #       Electricity
    z[41] = dfd['TRQRRP'].loc[6]

    #       Diesel
    z[42] = dfd['TRQRRP'].loc[7]

    #       Compressed Natural Gas
    z[71] = dfd['TRQRRP'].loc[8]

    #       Liquefied Natural Gas
    z[72] = dfd['TRQRRP'].loc[9]

     #     Commuter Rail
    z[75] = z[41]+z[42] +z[71]+z[72]

     #   Rail Transportation
    z[43] = z[38]+z[39]+z[40]+z[41]+z[42] +z[69]+z[70]+z[71]+z[72]


    #   Recreational Boats
    z[44] = dfd['TRQBOAT'].loc[1] + dfd['TRQBOAT'].loc[2]

    #     Gasoline
    z[49] = dfd['TRQBOAT'].loc[1]

    #     Distillate Fuel Oil (diesel)
    z[50] = dfd['TRQBOAT'].loc[2]


    #   Lubricants
    z[45] = dfd['TRQLUB']

    #   Pipeline Fuel Natural Gas
    z[46] = dfd['QGPTR'].loc[MNUMCR]

    #   Natural Gas Liquefaction for Export
    z[116] = dfd['QNGLQ'].loc[MNUMCR]


    #   Total Miscellaneous
    z[47] = z[31]+z[37]+z[43]+z[44]+z[45]+z[46]


    #   Total Consumption
    z[48] = z[9]+z[10]+z[15]+z[17]+z[21]+z[24]+z[27]+z[47]+z[116]
    
    return z
 
