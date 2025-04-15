#-*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_058(dfd, table_spec, table_id):
    """Fill table Freight Transportation Energy Use
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
    
    #   Freight Transportation Energy Use
    #    Technology and Fuel Type                                   


    # Freight trucks -- each of these tables is broken out by powertrain type (FRFUEL=12)
    # Powertrains:
    #   {1: Diesel, 2: Gasoline, 3: LPG, 4:CNG, 5:Flex/E85, 6:BEV, 7:PHEV diesel,
    #    8: PHEV gasoline, 9:FCEV, 10:FCHEV, 11: Gasoline HEV, 12:H2 ICE}
    
    # VEHICLE MILES TRAVELED (billion miles)
    # By powertrain (12) and size class (3)
    for i in range(1,13):
        z[i] = dfd['TFR_VMT_FAS_T'].loc[1].loc[i].loc[2]            # Light Medium (Class 3)
        z[i+13] = dfd['TFR_VMT_FAS_T'].loc[2].loc[i].loc[2]         # Medium (Class 4-6)
        z[i+26] = dfd['TFR_VMT_FAS_T'].loc[3].loc[i].loc[2]         # Heavy (Class 7&8)
    
    # Size class subtotals
    z[13] = dfd['TFR_VMT_FASF_T'].loc[1].loc[2]
    z[26] = dfd['TFR_VMT_FASF_T'].loc[2].loc[2]
    z[39] = dfd['TFR_VMT_FASF_T'].loc[3].loc[2]

    # Light Medium, Medium, and Heavy Total    
    z[248] = dfd['TFR_VMT_TR'].loc[2]                                                           
 
    # CONSUMPTION (trillion Btu)                                
    # By powertrain (12) and size class (3)
    for i in range(1,13):
        z[i+39] = dfd['TFR_FBTU_FAS_T'].loc[1].loc[i].loc[2]        # Light Medium (Class 3)
        z[i+52] = dfd['TFR_FBTU_FAS_T'].loc[2].loc[i].loc[2]        # Medium (Class 4-6)
        z[i+65] = dfd['TFR_FBTU_FAS_T'].loc[3].loc[i].loc[2]        # Heavy (Class 7&8)
    z[52] = dfd['TFR_FBTU_FASF_T'].loc[1].loc[2]
    z[65] = dfd['TFR_FBTU_FASF_T'].loc[2].loc[2]
    z[78] = dfd['TFR_FBTU_FASF_T'].loc[3].loc[2]

    # Size class subtotals (by powertrain for consumption)
    for i in range(1,13):
        z[79+i-1] = z[40+i-1] + z[53+i-1] + z[66+i-1]

    # Light Medium, Medium, and Heavy Total    
    z[91] = dfd['TFR_FBTU_TR'].loc[2]

    # FUEL EFFICIENCY (miles per gallon) -- Stock
    # By powertrain (12) and size class (3)
    for i in range(1,13):
        z[i+91]  = dfd['TFR_FTMPG'].loc[1].loc[i].loc[2]            # Light Medium (Class 3)
        z[i+104] = dfd['TFR_FTMPG'].loc[2].loc[i].loc[2]            # Medium (Class 4-6)
        z[i+117] = dfd['TFR_FTMPG'].loc[3].loc[i].loc[2]            # Heavy (Class 7&8)

    # Size class averages
    z[104] = dfd['TFR_FTMPG_S'].loc[1].loc[2]
    z[117] = dfd['TFR_FTMPG_S'].loc[2].loc[2]
    z[130] = dfd['TFR_FTMPG_S'].loc[3].loc[2]

    # Overall fleet average
    z[249] = dfd['TFR_FTMPG_TR'].loc[2]  
 
    # STOCK (millions)                                          
    # By powertrain (12) and size class (3)
    for i in range(1,13):
        z[i+130] = dfd['TFR_TRK_FAS_T'].loc[1].loc[i].loc[2]        # Light Medium (Class 3)
        z[i+143] = dfd['TFR_TRK_FAS_T'].loc[2].loc[i].loc[2]        # Medium (Class 4-6)
        z[i+156] = dfd['TFR_TRK_FAS_T'].loc[3].loc[i].loc[2]        # Heavy (Class 7&8)

    # Size class subtotals
    z[143] = dfd['TFR_TRK_FASF_T'].loc[1].loc[2]
    z[156] = dfd['TFR_TRK_FASF_T'].loc[2].loc[2]
    z[169] = dfd['TFR_TRK_FASF_T'].loc[3].loc[2]

    # Light Medium, Medium, and Heavy Total    
    z[250] = dfd['TFR_TRK_TR'].loc[2]

    # New Trucks by Size Class                                    

    # FUEL EFFICIENCY (miles per gallon) -- New sales
    # By powertrain (12) and size class (3)
    for i in range(1,13):
        z[i+169] = dfd['TFR_FTMPG'].loc[1].loc[i].loc[1]            # Light Medium (Class 3)
        z[i+182] = dfd['TFR_FTMPG'].loc[2].loc[i].loc[1]            # Medium (Class 4-6)
        z[i+195] = dfd['TFR_FTMPG'].loc[3].loc[i].loc[1]            # Heavy (Class 7&8)

    # Size class averages
    z[182] = dfd['TFR_FTMPG_S'].loc[1].loc[1]
    z[195] = dfd['TFR_FTMPG_S'].loc[2].loc[1]
    z[208] = dfd['TFR_FTMPG_S'].loc[3].loc[1]

    # Overall fleet average
    z[251] = dfd['TFR_FTMPG_TR'].loc[1] 

    # SALES (thousands)                                         
    # By powertrain (12) and size class (3)
    for i in range(1,13):
        z[i+208] = dfd['TFR_TRK_FAS_T'].loc[1].loc[i].loc[1] * 1000.    # Light Medium (Class 3)
        z[i+221] = dfd['TFR_TRK_FAS_T'].loc[2].loc[i].loc[1] * 1000.    # Medium (Class 4-6)
        z[i+234] = dfd['TFR_TRK_FAS_T'].loc[3].loc[i].loc[1] * 1000.    # Heavy (Class 7&8)

    # Size class subtotals
    z[221] = dfd['TFR_TRK_FASF_T'].loc[1].loc[1] * 1000.
    z[234] = dfd['TFR_TRK_FASF_T'].loc[2].loc[1] * 1000.
    z[247] = dfd['TFR_TRK_FASF_T'].loc[3].loc[1] * 1000.

    # Light Medium, Medium, and Heavy Total    
    z[252] = dfd['TFR_TRK_TR'].loc[1] * 1000.

    # CONSUMPTION BY SIZE CLASS AND FUEL                          
    # By powertrain (12) and size class (3)
    for i in range(1,7):
        z[i+271] = dfd['TRQFTRK_NEW'].loc[1].loc[i]                 # Light Medium (Class 3)
        z[i+278] = dfd['TRQFTRK_NEW'].loc[2].loc[i]                 # Medium (Class 4-6)
        z[i+285] = dfd['TRQFTRK_NEW'].loc[3].loc[i]                 # Heavy (Class 7&8)

    # Size class subtotals
    z[278] = z[272] + z[273] + z[274] + z[275] + z[276] + z[277]
    z[285] = z[279] + z[280] + z[281] + z[282] + z[283] + z[284]    
    z[292] = z[286] + z[287] + z[288] + z[289] + z[290] + z[291]

    # Light Medium, Medium, and Heavy Total    
    z[293] = z[278] + z[285] + z[292]


    # Ton Miles Traveled by Size Class                            
 
    #      Light Medium                                             
    z[294] = dfd['TRK_TMT'].loc[1]
 
    #      Medium                                                   
    z[295] = dfd['TRK_TMT'].loc[2]
 
    #      Heavy                                                    
    z[296] = dfd['TRK_TMT'].loc[3]
 
    #         Total                                                 
    z[297] = z[294]+z[295]+z[296]

    #                                                               
 
    #   Railroads                                                   
 
    #    Ton Miles by Rail (billion)                                
    #T58(196,IY,IS)=TRTMRR(1,IY)
    z[253] = dfd['TRTMRR'].loc[1]
 
    #    Fuel Efficiency (ton miles per thousand Btu)               
    #T58(197,IY,IS)=TRTMRR(2,IY)
    z[254] = dfd['TRTMRR'].loc[2]
 
    #    Fuel Consumption (trillion Btu)                            
 
    #      Distillate Fuel Oil (diesel)                             
    #T58(198,IY,IS)=TRQRRF(1,IY)
    z[255] = dfd['TRQRRF'].loc[1]
 
    #      Residual Fuel Oil                                        
    #T58(199,IY,IS)=TRQRRF(2,IY)
    z[256] = dfd['TRQRRF'].loc[2]
 
    #      Compressed Natural Gas                                   
    #T58(200,IY,IS)=TRQRRF(3,IY)
    z[257] = dfd['TRQRRF'].loc[3]
 
    #      Liquefied Natural Gas                                    
    #T58(201,IY,IS)=TRQRRF(4,IY)
    z[258] = dfd['TRQRRF'].loc[4]
 
    #                                                               
 
    #                                                               
 
    #   Domestic Shipping                                           
 
    #    Ton Miles Shipping (billion)                               
    #T58(202,IY,IS)=TRTMSHIP(1,IY)
    z[259] = dfd['TRTMSHIP'].loc[1]
 
    #    Fuel Efficiency (ton miles per thousand Btu)               
    #T58(203,IY,IS)=TRTMSHIP(2,IY)
    z[260] = dfd['TRTMSHIP'].loc[2]
 
    #    Fuel Consumption (trillion Btu)                            
 
    #      Distillate Fuel Oil (diesel)                             
    #T58(204,IY,IS)=TRQDOMS(1,IY)
    z[261] = dfd['TRQDOMS'].loc[1]
 
    #      Residual Fuel Oil                                        
    #T58(205,IY,IS)=TRQDOMS(2,IY)
    z[262] = dfd['TRQDOMS'].loc[2]
 
    #      Compressed Natural Gas                                   
    #T58(206,IY,IS)=TRQDOMS(3,IY)
    z[263] = dfd['TRQDOMS'].loc[3]
 
    #      Liquefied Natural Gas                                    
    #T58(207,IY,IS)=TRQDOMS(4,IY)
    z[264] = dfd['TRQDOMS'].loc[4]
 
    #                                                               
 
    #   International Shipping                                      
 
    #    Gross Trade (billion m__m dollars)                         
    #T58(208,IY,IS)=TRTRAVLD(8,IY)+TRIMSHIP(IY)
    z[265] = dfd['TRTRAVLD'].loc[8] + dfd['TRIMSHIP']
 
    #    Exports (billion m__m dollars)                             
    #T58(209,IY,IS)=TRTRAVLD(8,IY)
    z[266] = dfd['TRTRAVLD'].loc[8]
 
    #    Imports (billion m__m dollars)                             
    #T58(210,IY,IS)=TRIMSHIP(IY)
    z[267] = dfd['TRIMSHIP']
 
    #    Fuel Consumption (trillion Btu)                            
 
    #      Distillate Fuel Oil (diesel)                             
    #T58(211,IY,IS)=TRQINTS(1,IY)
    z[268] = dfd['TRQINTS'].loc[1]
 
    #      Residual Fuel Oil                                        
    #T58(212,IY,IS)=TRQINTS(2,IY)
    z[269] = dfd['TRQINTS'].loc[2]
 
    #      Compressed Natural Gas                                   
    #T58(213,IY,IS)=TRQINTS(3,IY)
    z[270] = dfd['TRQINTS'].loc[3]
 
    #      Liquefied Natural Gas                                    
    #T58(214,IY,IS)=TRQINTS(4,IY)
    z[271] = dfd['TRQINTS'].loc[4]
    
    return z
