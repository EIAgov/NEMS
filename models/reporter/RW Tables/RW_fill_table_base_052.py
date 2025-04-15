# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


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

    #            Minicompact                                        
    #T52(1:10,IY,IS)=TREFFCAR(1:10,IY)
    z[1] = dfd['TREFFCAR'].loc[1]
 
    #            Subcompact                                         
    #T52(1:10,IY,IS)=TREFFCAR(1:10,IY)
    z[2] = dfd['TREFFCAR'].loc[2]
 
    #            Compact                                            
    #T52(1:10,IY,IS)=TREFFCAR(1:10,IY)
    z[3] = dfd['TREFFCAR'].loc[3]
 
    #            Midsize                                            
    #T52(1:10,IY,IS)=TREFFCAR(1:10,IY)
    z[4] = dfd['TREFFCAR'].loc[4]
 
    #            Large                                              
    #T52(1:10,IY,IS)=TREFFCAR(1:10,IY)
    z[5] = dfd['TREFFCAR'].loc[5]
 
    #            Two Seater                                         
    #T52(1:10,IY,IS)=TREFFCAR(1:10,IY)
    z[6] = dfd['TREFFCAR'].loc[6]
 
    #            Small Crossover Utility                            
    #T52(1:10,IY,IS)=TREFFCAR(1:10,IY)
    z[7] = dfd['TREFFCAR'].loc[7]
 
    #            Large Crossover Utility                            
    #T52(1:10,IY,IS)=TREFFCAR(1:10,IY)
    z[8] = dfd['TREFFCAR'].loc[8]
 
    #         Average New Car                                       
    #T52(1:10,IY,IS)=TREFFCAR(1:10,IY)
    z[9] = dfd['TREFFCAR'].loc[9]
 
    #         Average New Car On-Road                               
    #T52(1:10,IY,IS)=TREFFCAR(1:10,IY)
    z[10] = dfd['TREFFCAR'].loc[10]
 
    #                                                               

    #         Conventional Light Trucks                             

    #            Small Pickup                                       
    #T52(11:20,IY,IS)=TREFFTRK(1:10,IY)
    z[11] = dfd['TREFFTRK'].loc[1]
 
    #            Large Pickup                                       
    #T52(11:20,IY,IS)=TREFFTRK(1:10,IY)
    z[12] = dfd['TREFFTRK'].loc[2]
 
    #            Small Van                                          
    #T52(11:20,IY,IS)=TREFFTRK(1:10,IY)
    z[13] = dfd['TREFFTRK'].loc[3]
 
    #            Large Van                                          
    #T52(11:20,IY,IS)=TREFFTRK(1:10,IY)
    z[14] = dfd['TREFFTRK'].loc[4]
 
    #            Small Utility                                      
    #T52(11:20,IY,IS)=TREFFTRK(1:10,IY)
    z[15] = dfd['TREFFTRK'].loc[5]
 
    #            Large Utility                                      
    #T52(11:20,IY,IS)=TREFFTRK(1:10,IY)
    z[16] = dfd['TREFFTRK'].loc[6]
 
    #            Small Crossover Utility                            
    #T52(11:20,IY,IS)=TREFFTRK(1:10,IY)
    z[17] = dfd['TREFFTRK'].loc[7]
 
    #            Large Crossover Utility                            
    #T52(11:20,IY,IS)=TREFFTRK(1:10,IY)
    z[18] = dfd['TREFFTRK'].loc[8]
 
    #         Average New Light Truck                               
    #T52(11:20,IY,IS)=TREFFTRK(1:10,IY)
    z[19] = dfd['TREFFTRK'].loc[9]
 
    #         Average New Light Truck On-Road                       
    #T52(11:20,IY,IS)=TREFFTRK(1:10,IY)
    z[20] = dfd['TREFFTRK'].loc[10]
 
    #                                                               

    #      Degradation Factors 1/                                   

    #         Cars                                                  
    #T52(21:22,IY,IS)=DEGRPT(1:2,IY)
    z[21] = dfd['DEGRPT'].loc[1]
 
    #         Light Trucks                                          
    #T52(21:22,IY,IS)=DEGRPT(1:2,IY)
    z[22] = dfd['DEGRPT'].loc[2]
 
    #                                                               

    #      New Fuel Efficiency by Size Class 2/                     

    #         Alternative-Fuel Cars                                 

    #            Minicompact                                        
    #T52(23:31,IY,IS)=TREFFALTC(1:9,IY)
    z[23] = dfd['TREFFALTC'].loc[1]
 
    #            Subcompact                                         
    #T52(23:31,IY,IS)=TREFFALTC(1:9,IY)
    z[24] = dfd['TREFFALTC'].loc[2]
 
    #            Compact                                            
    #T52(23:31,IY,IS)=TREFFALTC(1:9,IY)
    z[25] = dfd['TREFFALTC'].loc[3]
 
    #            Midsize                                            
    #T52(23:31,IY,IS)=TREFFALTC(1:9,IY)
    z[26] = dfd['TREFFALTC'].loc[4]
 
    #            Large                                              
    #T52(23:31,IY,IS)=TREFFALTC(1:9,IY)
    z[27] = dfd['TREFFALTC'].loc[5]
 
    #            Two Seater                                         
    #T52(23:31,IY,IS)=TREFFALTC(1:9,IY)
    z[28] = dfd['TREFFALTC'].loc[6]
 
    #            Small Crossover Utility                            
    #T52(23:31,IY,IS)=TREFFALTC(1:9,IY)
    z[29] = dfd['TREFFALTC'].loc[7]
 
    #            Large Crossover Utility                            
    #T52(23:31,IY,IS)=TREFFALTC(1:9,IY)
    z[30] = dfd['TREFFALTC'].loc[8]
 
    #         Average New Alternative Cars                          
    #T52(23:31,IY,IS)=TREFFALTC(1:9,IY)
    z[31] = dfd['TREFFALTC'].loc[9]
 
    #                                                               

    #         Alternative-Fuel Light Trucks                         

    #            Small Pickup                                       
    #T52(32:40,IY,IS)=TREFFALTT(1:9,IY)
    z[32] = dfd['TREFFALTT'].loc[1]
 
    #            Large Pickup                                       
    #T52(32:40,IY,IS)=TREFFALTT(1:9,IY)
    z[33] = dfd['TREFFALTT'].loc[2]
 
    #            Small Van                                          
    #T52(32:40,IY,IS)=TREFFALTT(1:9,IY)
    z[34] = dfd['TREFFALTT'].loc[3]
 
    #            Large Van                                          
    #T52(32:40,IY,IS)=TREFFALTT(1:9,IY)
    z[35] = dfd['TREFFALTT'].loc[4]
 
    #            Small Utility                                      
    #T52(32:40,IY,IS)=TREFFALTT(1:9,IY)
    z[36] = dfd['TREFFALTT'].loc[5]
 
    #            Large Utility                                      
    #T52(32:40,IY,IS)=TREFFALTT(1:9,IY)
    z[37] = dfd['TREFFALTT'].loc[6]
 
    #            Small Crossover Utility                            
    #T52(32:40,IY,IS)=TREFFALTT(1:9,IY)
    z[38] = dfd['TREFFALTT'].loc[7]
 
    #            Large Crossover Utility                            
    #T52(32:40,IY,IS)=TREFFALTT(1:9,IY)
    z[39] = dfd['TREFFALTT'].loc[8]
 
    #         Average New Alternative Light Trucks                  
    #T52(32:40,IY,IS)=TREFFALTT(1:9,IY)
    z[40] = dfd['TREFFALTT'].loc[9]
 
    #                                                               

    #   Fleet Vehicles                                              

    #      EPA Rated New Vehicle Fuel Efficiency                    

    #         Cars                                                  
    #T52(41:44,IY,IS)=TREFFFLT(1:4,IY)
    z[41] = dfd['TREFFFLT'].loc[1]
 
    #         Light Trucks                                          
    #T52(41:44,IY,IS)=TREFFFLT(1:4,IY)
    z[42] = dfd['TREFFFLT'].loc[2]
 
    #                                                               

    #      Average On-Road Miles per Gallon                         

    #         Cars                                                  
    #T52(41:44,IY,IS)=TREFFFLT(1:4,IY)
    z[43] = dfd['TREFFFLT'].loc[3]
 
    #         Light Trucks                                          
    #T52(41:44,IY,IS)=TREFFFLT(1:4,IY)
    z[44] = dfd['TREFFFLT'].loc[4]
 
    #                                                               

    #   New Vehicle Sales Shares (percent)                          

    #      Cars                                                     

    #         Minicompact                                           
    #T52(45:52,IY,IS)=TRSLSHRC(1:8,IY)*100.
    z[45] = dfd['TRSLSHRC'].loc[1] * 100.
 
    #         Subcompact                                            
    #T52(45:52,IY,IS)=TRSLSHRC(1:8,IY)*100.
    z[46] = dfd['TRSLSHRC'].loc[2] * 100.
 
    #         Compact                                               
    #T52(45:52,IY,IS)=TRSLSHRC(1:8,IY)*100.
    z[47] = dfd['TRSLSHRC'].loc[3] * 100.
 
    #         Midsize                                               
    #T52(45:52,IY,IS)=TRSLSHRC(1:8,IY)*100.
    z[48] = dfd['TRSLSHRC'].loc[4] * 100.
 
    #         Large                                                 
    #T52(45:52,IY,IS)=TRSLSHRC(1:8,IY)*100.
    z[49] = dfd['TRSLSHRC'].loc[5] * 100.
 
    #         Two Seater                                            
    #T52(45:52,IY,IS)=TRSLSHRC(1:8,IY)*100.
    z[50] = dfd['TRSLSHRC'].loc[6] * 100.
 
    #         Small Crossover Utility                               
    #T52(45:52,IY,IS)=TRSLSHRC(1:8,IY)*100.
    z[51] = dfd['TRSLSHRC'].loc[7] * 100.
 
    #         Large Crossover Utility                               
    #T52(45:52,IY,IS)=TRSLSHRC(1:8,IY)*100.
    z[52] = dfd['TRSLSHRC'].loc[8] * 100.
 
    #                                                               

    #      Light Trucks                                             

    #         Small Pickup                                          
    #T52(53:60,IY,IS)=TRSLSHRT(1:8,IY)*100.
    z[53] = dfd['TRSLSHRT'].loc[1] * 100.
 
    #         Large Pickup                                          
    #T52(53:60,IY,IS)=TRSLSHRT(1:8,IY)*100.
    z[54] = dfd['TRSLSHRT'].loc[2] * 100.
 
    #         Small Van                                             
    #T52(53:60,IY,IS)=TRSLSHRT(1:8,IY)*100.
    z[55] = dfd['TRSLSHRT'].loc[3] * 100.
 
    #         Large Van                                             
    #T52(53:60,IY,IS)=TRSLSHRT(1:8,IY)*100.
    z[56] = dfd['TRSLSHRT'].loc[4] * 100.
 
    #         Small Utility                                         
    #T52(53:60,IY,IS)=TRSLSHRT(1:8,IY)*100.
    z[57] = dfd['TRSLSHRT'].loc[5] * 100.
 
    #         Large Utility                                         
    #T52(53:60,IY,IS)=TRSLSHRT(1:8,IY)*100.
    z[58] = dfd['TRSLSHRT'].loc[6] * 100.
 
    #         Small Crossover Utility                               
    #T52(53:60,IY,IS)=TRSLSHRT(1:8,IY)*100.
    z[59] = dfd['TRSLSHRT'].loc[7] * 100.
 
    #         Large Crossover Utility                               
    #T52(53:60,IY,IS)=TRSLSHRT(1:8,IY)*100.
    z[60] = dfd['TRSLSHRT'].loc[8] * 100.
 
    #                                                               
 
    #                                                               
 
    #   New Vehicle Average Horsepower                              

    #      Conventional Cars                                        

    #         Minicompact                                           
    #T52(61:69,IY,IS)=TRHPCAR(1:9,IY)
    z[61] = dfd['TRHPCAR'].loc[1]
 
    #         Subcompact                                            
    #T52(61:69,IY,IS)=TRHPCAR(1:9,IY)
    z[62] = dfd['TRHPCAR'].loc[2]
 
    #         Compact                                               
    #T52(61:69,IY,IS)=TRHPCAR(1:9,IY)
    z[63] = dfd['TRHPCAR'].loc[3]
 
    #         Midsize                                               
    #T52(61:69,IY,IS)=TRHPCAR(1:9,IY)
    z[64] = dfd['TRHPCAR'].loc[4]
 
    #         Large                                                 
    #T52(61:69,IY,IS)=TRHPCAR(1:9,IY)
    z[65] = dfd['TRHPCAR'].loc[5]
 
    #         Two Seater                                            
    #T52(61:69,IY,IS)=TRHPCAR(1:9,IY)
    z[66] = dfd['TRHPCAR'].loc[6]
 
    #         Small Crossover Utility                               
    #T52(61:69,IY,IS)=TRHPCAR(1:9,IY)
    z[67] = dfd['TRHPCAR'].loc[7]
 
    #         Large Crossover Utility                               
    #T52(61:69,IY,IS)=TRHPCAR(1:9,IY)
    z[68] = dfd['TRHPCAR'].loc[8]
 
    #      Average New Car                                          
    #T52(61:69,IY,IS)=TRHPCAR(1:9,IY)
    z[69] = dfd['TRHPCAR'].loc[9]
 
    #                                                               

    #      Conventional Light Trucks                                

    #         Small Pickup                                          
    #T52(70:78,IY,IS)=TRHPTRK(1:9,IY)
    z[70] = dfd['TRHPTRK'].loc[1]
 
    #         Large Pickup                                          
    #T52(70:78,IY,IS)=TRHPTRK(1:9,IY)
    z[71] = dfd['TRHPTRK'].loc[2]
 
    #         Small Van                                             
    #T52(70:78,IY,IS)=TRHPTRK(1:9,IY)
    z[72] = dfd['TRHPTRK'].loc[3]
 
    #         Large Van                                             
    #T52(70:78,IY,IS)=TRHPTRK(1:9,IY)
    z[73] = dfd['TRHPTRK'].loc[4]
 
    #         Small Utility                                         
    #T52(70:78,IY,IS)=TRHPTRK(1:9,IY)
    z[74] = dfd['TRHPTRK'].loc[5]
 
    #         Large Utility                                         
    #T52(70:78,IY,IS)=TRHPTRK(1:9,IY)
    z[75] = dfd['TRHPTRK'].loc[6]
 
    #         Small Crossover Utility                               
    #T52(70:78,IY,IS)=TRHPTRK(1:9,IY)
    z[76] = dfd['TRHPTRK'].loc[7]
 
    #         Large Crossover Utility                               
    #T52(70:78,IY,IS)=TRHPTRK(1:9,IY)
    z[77] = dfd['TRHPTRK'].loc[8]
 
    #      Average New Light Truck                                  
    #T52(70:78,IY,IS)=TRHPTRK(1:9,IY)
    z[78] = dfd['TRHPTRK'].loc[9]
 
    #                                                               

    #   New Vehicle Average Weight                                  

    #      Conventional Cars                                        

    #         Minicompact                                           
    #T52(79:87,IY,IS)=TRWTCAR(1:9,IY)
    z[79] = dfd['TRWTCAR'].loc[1]
 
    #         Subcompact                                            
    #T52(79:87,IY,IS)=TRWTCAR(1:9,IY)
    z[80] = dfd['TRWTCAR'].loc[2]
 
    #         Compact                                               
    #T52(79:87,IY,IS)=TRWTCAR(1:9,IY)
    z[81] = dfd['TRWTCAR'].loc[3]
 
    #         Midsize                                               
    #T52(79:87,IY,IS)=TRWTCAR(1:9,IY)
    z[82] = dfd['TRWTCAR'].loc[4]
 
    #         Large                                                 
    #T52(79:87,IY,IS)=TRWTCAR(1:9,IY)
    z[83] = dfd['TRWTCAR'].loc[5]
 
    #         Two Seater                                            
    #T52(79:87,IY,IS)=TRWTCAR(1:9,IY)
    z[84] = dfd['TRWTCAR'].loc[6]
 
    #         Small Crossover Utility                               
    #T52(79:87,IY,IS)=TRWTCAR(1:9,IY)
    z[85] = dfd['TRWTCAR'].loc[7]
 
    #         Large Crossover Utility                               
    #T52(79:87,IY,IS)=TRWTCAR(1:9,IY)
    z[86] = dfd['TRWTCAR'].loc[8]
 
    #      Average New Car                                          
    #T52(79:87,IY,IS)=TRWTCAR(1:9,IY)
    z[87] = dfd['TRWTCAR'].loc[9]
 
    #                                                               

    #      Conventional Light Trucks                                

    #         Small Pickup                                          
    #T52(88:96,IY,IS)=TRWTTRK(1:9,IY)
    z[88] = dfd['TRWTTRK'].loc[1]
 
    #         Large Pickup                                          
    #T52(88:96,IY,IS)=TRWTTRK(1:9,IY)
    z[89] = dfd['TRWTTRK'].loc[2]
 
    #         Small Van                                             
    #T52(88:96,IY,IS)=TRWTTRK(1:9,IY)
    z[90] = dfd['TRWTTRK'].loc[3]
 
    #         Large Van                                             
    #T52(88:96,IY,IS)=TRWTTRK(1:9,IY)
    z[91] = dfd['TRWTTRK'].loc[4]
 
    #         Small Utility                                         
    #T52(88:96,IY,IS)=TRWTTRK(1:9,IY)
    z[92] = dfd['TRWTTRK'].loc[5]
 
    #         Large Utility                                         
    #T52(88:96,IY,IS)=TRWTTRK(1:9,IY)
    z[93] = dfd['TRWTTRK'].loc[6]
 
    #         Small Crossover Utility                               
    #T52(88:96,IY,IS)=TRWTTRK(1:9,IY)
    z[94] = dfd['TRWTTRK'].loc[7]
 
    #         Large Crossover Utility                               
    #T52(88:96,IY,IS)=TRWTTRK(1:9,IY)
    z[95] = dfd['TRWTTRK'].loc[8]
 
    #      Average New Light Truck                                  
    #T52(88:96,IY,IS)=TRWTTRK(1:9,IY)
    z[96] = dfd['TRWTTRK'].loc[9]
 
    #                                                               

    #   Average Weight for the Stock                                

    #      Conventional Cars                                        
    #T52(97,IY,IS)=TRWTCAR_STOCK(IY)
    z[97] = dfd['TRWTCAR_STOCK']
 
    #      Conventional Light Trucks                                
    #T52(98,IY,IS)=TRWTTRK_STOCK(IY)
    z[98] = dfd['TRWTTRK_STOCK']
    
    return z
 