# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_057(dfd, table_spec, table_id):
    """Fill table Air Travel Energy Use
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




    #   Air Travel Energy Use
    #    Indicators                                                 
    #                                                               
    #   Fuel Cost (1987 dollars per million Btu)                    
    z[1] = dfd['AIROUT'].loc[1]                                                               

    #   Load Factor (fraction of seats filled)                      

    #     U.S. Domestic                                             
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[2] = dfd['AIROUT'].loc[2]
 
    #     U.S. International                                        
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[3] = dfd['AIROUT'].loc[3]
 
    #                                                               

    #   Driver Variables                                            

    #     Gross Domestic Product                                    

    #     (billion 2015 $ chain-weighted dollars)                   

    #       United States                                           
    #T57(221:236,IY,IS)=WLD_GDP(1:16,IY)
    z[221] = dfd['WLD_GDP'].loc[1]
 
    #       Canada                                                  
    #T57(221:236,IY,IS)=WLD_GDP(1:16,IY)
    z[222] = dfd['WLD_GDP'].loc[2]
 
    #       Mexico and other OECD Americas                          
    #T57(221:236,IY,IS)=WLD_GDP(1:16,IY)
    z[223] = dfd['WLD_GDP'].loc[3]
 
    #       OECD Europe                                             
    #T57(221:236,IY,IS)=WLD_GDP(1:16,IY)
    z[224] = dfd['WLD_GDP'].loc[4]
 
    #       Japan                                                   
    #T57(221:236,IY,IS)=WLD_GDP(1:16,IY)
    z[225] = dfd['WLD_GDP'].loc[5]
 
    #       Australia and New Zealand                               
    #T57(221:236,IY,IS)=WLD_GDP(1:16,IY)
    z[226] = dfd['WLD_GDP'].loc[6]
 
    #       South Korea                                             
    #T57(221:236,IY,IS)=WLD_GDP(1:16,IY)
    z[227] = dfd['WLD_GDP'].loc[7]
 
    #       Russia                                                  
    #T57(221:236,IY,IS)=WLD_GDP(1:16,IY)
    z[228] = dfd['WLD_GDP'].loc[8]
 
    #       Other Europe and Eurasia                                
    #T57(221:236,IY,IS)=WLD_GDP(1:16,IY)
    z[229] = dfd['WLD_GDP'].loc[9]
 
    #       China                                                   
    #T57(221:236,IY,IS)=WLD_GDP(1:16,IY)
    z[230] = dfd['WLD_GDP'].loc[10]
 
    #       India                                                   
    #T57(221:236,IY,IS)=WLD_GDP(1:16,IY)
    z[231] = dfd['WLD_GDP'].loc[11]
 
    #       Other Non-OECD Asia                                     
    #T57(221:236,IY,IS)=WLD_GDP(1:16,IY)
    z[232] = dfd['WLD_GDP'].loc[12]
 
    #       Middle East                                             
    #T57(221:236,IY,IS)=WLD_GDP(1:16,IY)
    z[233] = dfd['WLD_GDP'].loc[13]
 
    #       Africa                                                  
    #T57(221:236,IY,IS)=WLD_GDP(1:16,IY)
    z[234] = dfd['WLD_GDP'].loc[14]
 
    #       Brazil                                                  
    #T57(221:236,IY,IS)=WLD_GDP(1:16,IY)
    z[235] = dfd['WLD_GDP'].loc[15]
 
    #       Other Non-OECD Americas                                 
    #T57(221:236,IY,IS)=WLD_GDP(1:16,IY)
    z[236] = dfd['WLD_GDP'].loc[16]
 
    #     Population (millions)                                     

    #       United States                                           
    #T57(237:252,IY,IS)=WLD_POP(1:16,IY)
    z[237] = dfd['WLD_POP'].loc[1]
 
    #       Canada                                                  
    #T57(237:252,IY,IS)=WLD_POP(1:16,IY)
    z[238] = dfd['WLD_POP'].loc[2]
 
    #       Mexico and other OECD Americas                          
    #T57(237:252,IY,IS)=WLD_POP(1:16,IY)
    z[239] = dfd['WLD_POP'].loc[3]
 
    #       OECD Europe                                             
    #T57(237:252,IY,IS)=WLD_POP(1:16,IY)
    z[240] = dfd['WLD_POP'].loc[4]
 
    #       Japan                                                   
    #T57(237:252,IY,IS)=WLD_POP(1:16,IY)
    z[241] = dfd['WLD_POP'].loc[5]
 
    #       Australia and New Zealand                               
    #T57(237:252,IY,IS)=WLD_POP(1:16,IY)
    z[242] = dfd['WLD_POP'].loc[6]
 
    #       South Korea                                             
    #T57(237:252,IY,IS)=WLD_POP(1:16,IY)
    z[243] = dfd['WLD_POP'].loc[7]
 
    #       Russia                                                  
    #T57(237:252,IY,IS)=WLD_POP(1:16,IY)
    z[244] = dfd['WLD_POP'].loc[8]
 
    #       Other Europe and Eurasia                                
    #T57(237:252,IY,IS)=WLD_POP(1:16,IY)
    z[245] = dfd['WLD_POP'].loc[9]
 
    #       China                                                   
    #T57(237:252,IY,IS)=WLD_POP(1:16,IY)
    z[246] = dfd['WLD_POP'].loc[10]
 
    #       India                                                   
    #T57(237:252,IY,IS)=WLD_POP(1:16,IY)
    z[247] = dfd['WLD_POP'].loc[11]
 
    #       Other Non-OECD Asia                                     
    #T57(237:252,IY,IS)=WLD_POP(1:16,IY)
    z[248] = dfd['WLD_POP'].loc[12]
 
    #       Middle East                                             
    #T57(237:252,IY,IS)=WLD_POP(1:16,IY)
    z[249] = dfd['WLD_POP'].loc[13]
 
    #       Africa                                                  
    #T57(237:252,IY,IS)=WLD_POP(1:16,IY)
    z[250] = dfd['WLD_POP'].loc[14]
 
    #       Brazil                                                  
    #T57(237:252,IY,IS)=WLD_POP(1:16,IY)
    z[251] = dfd['WLD_POP'].loc[15]
 
    #       Other Non-OECD Americas                                 
    #T57(237:252,IY,IS)=WLD_POP(1:16,IY)
    z[252] = dfd['WLD_POP'].loc[16]
 
    #                                                               

    #   Travel Demand                                               

    #     Revenue Passenger Miles (billion miles)                   

    #       Domestic 1/                                             

    #         United States                                         
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[4] = dfd['AIROUT'].loc[4]
 
    #         Canada                                                
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[5] = dfd['AIROUT'].loc[5]
 
    #         Mexico and other OECD Americas                        
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[6] = dfd['AIROUT'].loc[6]
 
    #         OECD Europe                                           
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[7] = dfd['AIROUT'].loc[7]
 
    #         Japan                                                 
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[8] = dfd['AIROUT'].loc[8]
 
    #         Australia and New Zealand                             
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[9] = dfd['AIROUT'].loc[9]
 
    #         South Korea                                           
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[10] = dfd['AIROUT'].loc[10]
 
    #         Russia                                                
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[11] = dfd['AIROUT'].loc[11]
 
    #         Other Europe and Eurasia                              
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[12] = dfd['AIROUT'].loc[12]
 
    #         China                                                 
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[13] = dfd['AIROUT'].loc[13]
 
    #         India                                                 
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[14] = dfd['AIROUT'].loc[14]
 
    #         Other Non-OECD Asia                                   
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[15] = dfd['AIROUT'].loc[15]
 
    #         Middle East                                           
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[16] = dfd['AIROUT'].loc[16]
 
    #         Africa                                                
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[17] = dfd['AIROUT'].loc[17]
 
    #         Brazil                                                
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[18] = dfd['AIROUT'].loc[18]
 
    #         Other Non-OECD Americas                               
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[19] = dfd['AIROUT'].loc[19]
 
    #           Total World Domestic                                
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[36] = dfd['AIROUT'].loc[36]
 
    #       International 1/                                        

    #         United States                                         
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[20] = dfd['AIROUT'].loc[20]
 
    #         Canada                                                
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[21] = dfd['AIROUT'].loc[21]
 
    #         Mexico and other OECD Americas                        
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[22] = dfd['AIROUT'].loc[22]
 
    #         OECD Europe                                           
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[23] = dfd['AIROUT'].loc[23]
 
    #         Japan                                                 
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[24] = dfd['AIROUT'].loc[24]
 
    #         Australia and New Zealand                             
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[25] = dfd['AIROUT'].loc[25]
 
    #         South Korea                                           
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[26] = dfd['AIROUT'].loc[26]
 
    #         Russia                                                
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[27] = dfd['AIROUT'].loc[27]
 
    #         Other Europe and Eurasia                              
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[28] = dfd['AIROUT'].loc[28]
 
    #         China                                                 
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[29] = dfd['AIROUT'].loc[29]
 
    #         India                                                 
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[30] = dfd['AIROUT'].loc[30]
 
    #         Other Non-OECD Asia                                   
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[31] = dfd['AIROUT'].loc[31]
 
    #         Middle East                                           
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[32] = dfd['AIROUT'].loc[32]
 
    #         Africa                                                
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[33] = dfd['AIROUT'].loc[33]
 
    #         Brazil                                                
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[34] = dfd['AIROUT'].loc[34]
 
    #         Other Non-OECD Americas                               
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[35] = dfd['AIROUT'].loc[35]
 
    #           Total World International                           
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[37] = dfd['AIROUT'].loc[37]
 
    #       Total World                                             
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[38] = dfd['AIROUT'].loc[38]
 
    #     Freight Revenue Ton Miles (billion miles) 2/              

    #       United States                                           
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[39] = dfd['AIROUT'].loc[39]
 
    #       Canada                                                  
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[40] = dfd['AIROUT'].loc[40]
 
    #       Mexico and other OECD Americas                          
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[41] = dfd['AIROUT'].loc[41]
 
    #       OECD Europe                                             
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[42] = dfd['AIROUT'].loc[42]
 
    #       Japan                                                   
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[43] = dfd['AIROUT'].loc[43]
 
    #       Australia and New Zealand                               
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[44] = dfd['AIROUT'].loc[44]
 
    #       South Korea                                             
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[45] = dfd['AIROUT'].loc[45]
 
    #       Russia                                                  
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[46] = dfd['AIROUT'].loc[46]
 
    #       Other Europe and Eurasia                                
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[47] = dfd['AIROUT'].loc[47]
 
    #       China                                                   
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[48] = dfd['AIROUT'].loc[48]
 
    #       India                                                   
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[49] = dfd['AIROUT'].loc[49]
 
    #       Other Non-OECD Asia                                     
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[50] = dfd['AIROUT'].loc[50]
 
    #       Middle East                                             
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[51] = dfd['AIROUT'].loc[51]
 
    #       Africa                                                  
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[52] = dfd['AIROUT'].loc[52]
 
    #       Brazil                                                  
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[53] = dfd['AIROUT'].loc[53]
 
    #       Other Non-OECD Americas                                 
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[54] = dfd['AIROUT'].loc[54]
 
    #         Total World Domestic                                  
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[55] = dfd['AIROUT'].loc[55]
 
    #         Total World International                             
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[56] = dfd['AIROUT'].loc[56]
 
    #       Total World                                             
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[57] = dfd['AIROUT'].loc[57]
  
    #   Seat Miles Demanded (billion miles)                         

    #     United States                                             
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[58] = dfd['AIROUT'].loc[58]
 
    #       Narrow Body Aircraft                                    
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[59] = dfd['AIROUT'].loc[59]
 
    #       Wide Body Aircraft                                      
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[60] = dfd['AIROUT'].loc[60]
 
    #       Regional Jets                                           
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[61] = dfd['AIROUT'].loc[61]
 
    #     Canada                                                    
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[62] = dfd['AIROUT'].loc[62]
 
    #       Narrow Body Aircraft                                    
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[63] = dfd['AIROUT'].loc[63]
 
    #       Wide Body Aircraft                                      
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[64] = dfd['AIROUT'].loc[64]
 
    #       Regional Jets                                           
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[65] = dfd['AIROUT'].loc[65]
 
    #     Mexico and other OECD Americas                            
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[66] = dfd['AIROUT'].loc[66]
 
    #       Narrow Body Aircraft                                    
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[67] = dfd['AIROUT'].loc[67]
 
    #       Wide Body Aircraft                                      
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[68] = dfd['AIROUT'].loc[68]
 
    #       Regional Jets                                           
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[69] = dfd['AIROUT'].loc[69]
 
    #     OECD Europe                                               
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[70] = dfd['AIROUT'].loc[70]
 
    #       Narrow Body Aircraft                                    
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[71] = dfd['AIROUT'].loc[71]
 
    #       Wide Body Aircraft                                      
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[72] = dfd['AIROUT'].loc[72]
 
    #       Regional Jets                                           
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[73] = dfd['AIROUT'].loc[73]
 
    #     Japan                                                     
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[74] = dfd['AIROUT'].loc[74]
 
    #       Narrow Body Aircraft                                    
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[75] = dfd['AIROUT'].loc[75]
 
    #       Wide Body Aircraft                                      
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[76] = dfd['AIROUT'].loc[76]
 
    #       Regional Jets                                           
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[77] = dfd['AIROUT'].loc[77]
 
    #     Australia and New Zealand                                 
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[78] = dfd['AIROUT'].loc[78]
 
    #       Narrow Body Aircraft                                    
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[79] = dfd['AIROUT'].loc[79]
 
    #       Wide Body Aircraft                                      
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[80] = dfd['AIROUT'].loc[80]
 
    #       Regional Jets                                           
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[81] = dfd['AIROUT'].loc[81]
 
    #     South Korea                                               
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[82] = dfd['AIROUT'].loc[82]
 
    #       Narrow Body Aircraft                                    
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[83] = dfd['AIROUT'].loc[83]
 
    #       Wide Body Aircraft                                      
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[84] = dfd['AIROUT'].loc[84]
 
    #       Regional Jets                                           
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[85] = dfd['AIROUT'].loc[85]
 
    #     Russia                                                    
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[86] = dfd['AIROUT'].loc[86]
 
    #       Narrow Body Aircraft                                    
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[87] = dfd['AIROUT'].loc[87]
 
    #       Wide Body Aircraft                                      
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[88] = dfd['AIROUT'].loc[88]
 
    #       Regional Jets                                           
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[89] = dfd['AIROUT'].loc[89]
 
    #     Other Europe and Eurasia                                  
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[90] = dfd['AIROUT'].loc[90]
 
    #       Narrow Body Aircraft                                    
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[91] = dfd['AIROUT'].loc[91]
 
    #       Wide Body Aircraft                                      
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[92] = dfd['AIROUT'].loc[92]
 
    #       Regional Jets                                           
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[93] = dfd['AIROUT'].loc[93]
 
    #     China                                                     
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[94] = dfd['AIROUT'].loc[94]
 
    #       Narrow Body Aircraft                                    
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[95] = dfd['AIROUT'].loc[95]
 
    #       Wide Body Aircraft                                      
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[96] = dfd['AIROUT'].loc[96]
 
    #       Regional Jets                                           
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[97] = dfd['AIROUT'].loc[97]
 
    #     India                                                     
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[98] = dfd['AIROUT'].loc[98]
 
    #       Narrow Body Aircraft                                    
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[99] = dfd['AIROUT'].loc[99]
 
    #       Wide Body Aircraft                                      
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[100] = dfd['AIROUT'].loc[100]
 
    #       Regional Jets                                           
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[101] = dfd['AIROUT'].loc[101]
 
    #     Other Non-OECD Asia                                       
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[102] = dfd['AIROUT'].loc[102]
 
    #       Narrow Body Aircraft                                    
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[103] = dfd['AIROUT'].loc[103]
 
    #       Wide Body Aircraft                                      
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[104] = dfd['AIROUT'].loc[104]
 
    #       Regional Jets                                           
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[105] = dfd['AIROUT'].loc[105]
 
    #     Middle East                                               
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[106] = dfd['AIROUT'].loc[106]
 
    #       Narrow Body Aircraft                                    
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[107] = dfd['AIROUT'].loc[107]
 
    #       Wide Body Aircraft                                      
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[108] = dfd['AIROUT'].loc[108]
 
    #       Regional Jets                                           
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[109] = dfd['AIROUT'].loc[109]
 
    #     Africa                                                    
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[110] = dfd['AIROUT'].loc[110]
 
    #       Narrow Body Aircraft                                    
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[111] = dfd['AIROUT'].loc[111]
 
    #       Wide Body Aircraft                                      
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[112] = dfd['AIROUT'].loc[112]
 
    #       Regional Jets                                           
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[113] = dfd['AIROUT'].loc[113]
 
    #     Brazil                                                    
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[114] = dfd['AIROUT'].loc[114]
 
    #       Narrow Body Aircraft                                    
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[115] = dfd['AIROUT'].loc[115]
 
    #       Wide Body Aircraft                                      
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[116] = dfd['AIROUT'].loc[116]
 
    #       Regional Jets                                           
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[117] = dfd['AIROUT'].loc[117]
 
    #     Other Non-OECD Americas                                   
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[118] = dfd['AIROUT'].loc[118]
 
    #       Narrow Body Aircraft                                    
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[119] = dfd['AIROUT'].loc[119]
 
    #       Wide Body Aircraft                                      
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[120] = dfd['AIROUT'].loc[120]
 
    #       Regional Jets                                           
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[121] = dfd['AIROUT'].loc[121]
 
    #     Total Narrow Body                                         
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[122] = dfd['AIROUT'].loc[122]
 
    #     Total Wide Body                                           
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[123] = dfd['AIROUT'].loc[123]
 
    #     Total Regional Jet                                        
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[124] = dfd['AIROUT'].loc[124]
 
    #     Total World                                               
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[125] = dfd['AIROUT'].loc[125]
 
    #                                                               

    #   Aircraft Deliveries                                         
 
    #     United States                                             
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[126] = dfd['AIROUT'].loc[126]
 
    #       Narrow Body Aircraft                                    
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[127] = dfd['AIROUT'].loc[127]
 
    #       Wide Body Aircraft                                      
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[128] = dfd['AIROUT'].loc[128]
 
    #       Regional Jets                                           
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[129] = dfd['AIROUT'].loc[129]
 
    #     Canada                                                    
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[130] = dfd['AIROUT'].loc[130]
 
    #       Narrow Body Aircraft                                    
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[131] = dfd['AIROUT'].loc[131]
 
    #       Wide Body Aircraft                                      
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[132] = dfd['AIROUT'].loc[132]
 
    #       Regional Jets                                           
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[133] = dfd['AIROUT'].loc[133]
 
    #     Mexico and other OECD Americas                            
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[134] = dfd['AIROUT'].loc[134]
 
    #       Narrow Body Aircraft                                    
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[135] = dfd['AIROUT'].loc[135]
 
    #       Wide Body Aircraft                                      
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[136] = dfd['AIROUT'].loc[136]
 
    #       Regional Jets                                           
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[137] = dfd['AIROUT'].loc[137]
 
    #     OECD Europe                                               
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[138] = dfd['AIROUT'].loc[138]
 
    #       Narrow Body Aircraft                                    
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[139] = dfd['AIROUT'].loc[139]
 
    #       Wide Body Aircraft                                      
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[140] = dfd['AIROUT'].loc[140]
 
    #       Regional Jets                                           
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[141] = dfd['AIROUT'].loc[141]
 
    #     Japan                                                     
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[142] = dfd['AIROUT'].loc[142]
 
    #       Narrow Body Aircraft                                    
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[143] = dfd['AIROUT'].loc[143]
 
    #       Wide Body Aircraft                                      
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[144] = dfd['AIROUT'].loc[144]
 
    #       Regional Jets                                           
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[145] = dfd['AIROUT'].loc[145]
 
    #     Australia and New Zealand                                 
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[146] = dfd['AIROUT'].loc[146]
 
    #       Narrow Body Aircraft                                    
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[147] = dfd['AIROUT'].loc[147]
 
    #       Wide Body Aircraft                                      
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[148] = dfd['AIROUT'].loc[148]
 
    #       Regional Jets                                           
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[149] = dfd['AIROUT'].loc[149]
 
    #     South Korea                                               
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[150] = dfd['AIROUT'].loc[150]
 
    #       Narrow Body Aircraft                                    
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[151] = dfd['AIROUT'].loc[151]
 
    #       Wide Body Aircraft                                      
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[152] = dfd['AIROUT'].loc[152]
 
    #       Regional Jets                                           
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[153] = dfd['AIROUT'].loc[153]
 
    #     Russia                                                    
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[154] = dfd['AIROUT'].loc[154]
 
    #       Narrow Body Aircraft                                    
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[155] = dfd['AIROUT'].loc[155]
 
    #       Wide Body Aircraft                                      
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[156] = dfd['AIROUT'].loc[156]
 
    #       Regional Jets                                           
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[157] = dfd['AIROUT'].loc[157]
 
    #     Other Europe and Eurasia                                  
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[158] = dfd['AIROUT'].loc[158]
 
    #       Narrow Body Aircraft                                    
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[159] = dfd['AIROUT'].loc[159]
 
    #       Wide Body Aircraft                                      
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[160] = dfd['AIROUT'].loc[160]
 
    #       Regional Jets                                           
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[161] = dfd['AIROUT'].loc[161]
 
    #     China                                                     
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[162] = dfd['AIROUT'].loc[162]
 
    #       Narrow Body Aircraft                                    
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[163] = dfd['AIROUT'].loc[163]
 
    #       Wide Body Aircraft                                      
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[164] = dfd['AIROUT'].loc[164]
 
    #       Regional Jets                                           
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[165] = dfd['AIROUT'].loc[165]
 
    #     India                                                     
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[166] = dfd['AIROUT'].loc[166]
 
    #       Narrow Body Aircraft                                    
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[167] = dfd['AIROUT'].loc[167]
 
    #       Wide Body Aircraft                                      
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[168] = dfd['AIROUT'].loc[168]
 
    #       Regional Jets                                           
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[169] = dfd['AIROUT'].loc[169]
 
    #     Other Non-OECD Asia                                       
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[170] = dfd['AIROUT'].loc[170]
 
    #       Narrow Body Aircraft                                    
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[171] = dfd['AIROUT'].loc[171]
 
    #       Wide Body Aircraft                                      
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[172] = dfd['AIROUT'].loc[172]
 
    #       Regional Jets                                           
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[173] = dfd['AIROUT'].loc[173]
 
    #     Middle East                                               
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[174] = dfd['AIROUT'].loc[174]
 
    #       Narrow Body Aircraft                                    
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[175] = dfd['AIROUT'].loc[175]
 
    #       Wide Body Aircraft                                      
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[176] = dfd['AIROUT'].loc[176]
 
    #       Regional Jets                                           
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[177] = dfd['AIROUT'].loc[177]
 
    #     Africa                                                    
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[178] = dfd['AIROUT'].loc[178]
 
    #       Narrow Body Aircraft                                    
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[179] = dfd['AIROUT'].loc[179]
 
    #       Wide Body Aircraft                                      
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[180] = dfd['AIROUT'].loc[180]
 
    #       Regional Jets                                           
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[181] = dfd['AIROUT'].loc[181]
 
    #     Brazil                                                    
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[182] = dfd['AIROUT'].loc[182]
 
    #       Narrow Body Aircraft                                    
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[183] = dfd['AIROUT'].loc[183]
 
    #       Wide Body Aircraft                                      
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[184] = dfd['AIROUT'].loc[184]
 
    #       Regional Jets                                           
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[185] = dfd['AIROUT'].loc[185]
 
    #     Other Non-OECD Americas                                   
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[186] = dfd['AIROUT'].loc[186]
 
    #       Narrow Body Aircraft                                    
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[187] = dfd['AIROUT'].loc[187]
 
    #       Wide Body Aircraft                                      
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[188] = dfd['AIROUT'].loc[188]
 
    #       Regional Jets                                           
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[189] = dfd['AIROUT'].loc[189]
 
    #     Total Narrow Body                                         
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[190] = dfd['AIROUT'].loc[191]
 
    #     Total Wide Body                                           
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[191] = dfd['AIROUT'].loc[192]
 
    #     Total Regional Jet                                        
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[192] = dfd['AIROUT'].loc[193]
 
    #   Total World                                                 
    #T57(1:193,IY,IS)=AIROUT(1:193,IY)
    z[193] = dfd['AIROUT'].loc[190]
 
    #                                                               


 
    #   Aircraft Efficiency (seat miles per gallon) 3/              


 
    #     New Aircraft                                              


 
    #       Narrow Body Aircraft                                    
    #T57(194:197,IY,IS)=TRAIREFFN(1:4,IY)
    z[194] = dfd['TRAIREFFN'].loc[1]
 
    #       Wide Body Aircraft                                      
    #T57(194:197,IY,IS)=TRAIREFFN(1:4,IY)
    z[195] = dfd['TRAIREFFN'].loc[2]
 
    #       Regional Jets                                           
    #T57(194:197,IY,IS)=TRAIREFFN(1:4,IY)
    z[196] = dfd['TRAIREFFN'].loc[3]
 
    #         Average Aircraft                                      
    #T57(194:197,IY,IS)=TRAIREFFN(1:4,IY)
    z[197] = dfd['TRAIREFFN'].loc[4]
 
    #     Aircraft Stock                                            


 
    #       Narrow Body Aircraft                                    
    #T57(198:201,IY,IS)=TRAIREFFS(1:4,IY)
    z[198] = dfd['TRAIREFFS'].loc[1]
 
    #       Wide Body Aircraft                                      
    #T57(198:201,IY,IS)=TRAIREFFS(1:4,IY)
    z[199] = dfd['TRAIREFFS'].loc[2]
 
    #       Regional Jets                                           
    #T57(198:201,IY,IS)=TRAIREFFS(1:4,IY)
    z[200] = dfd['TRAIREFFS'].loc[3]
 
    #         Average Aircraft                                      
    #T57(198:201,IY,IS)=TRAIREFFS(1:4,IY)
    z[201] = dfd['TRAIREFFS'].loc[4]
 
    #                                                               


 
    #   Fuel Consumption (trillion Btu)                             


 
    #     Commercial Jet Fuel                                       
   
    #       United States                                           
    #T57(202:217,IY,IS)=AIROUT(415:430,IY)	
    z[202] = dfd['AIROUT'].loc[415]	
    
 
    #       Canada                                                  
    #T57(202:217,IY,IS)=AIROUT(415:430,IY)	
    z[203] = dfd['AIROUT'].loc[416]	
 
    #       Mexico and other OECD Americas                          
    #T57(202:217,IY,IS)=AIROUT(415:430,IY)	
    z[204] = dfd['AIROUT'].loc[417]	
 
    #       OECD Europe                                             
    #T57(202:217,IY,IS)=AIROUT(415:430,IY)	
    z[205] = dfd['AIROUT'].loc[418]	
 
    #       Japan                                                   
    #T57(202:217,IY,IS)=AIROUT(415:430,IY)	
    z[206] = dfd['AIROUT'].loc[419]	
 
    #       Australia and New Zealand                               
    #T57(202:217,IY,IS)=AIROUT(415:430,IY)	
    z[207] = dfd['AIROUT'].loc[420]	
 
    #       South Korea                                             
    #T57(202:217,IY,IS)=AIROUT(415:430,IY)	
    z[208] = dfd['AIROUT'].loc[421]	
 
    #       Russia                                                  
    #T57(202:217,IY,IS)=AIROUT(415:430,IY)	
    z[209] = dfd['AIROUT'].loc[422]	
 
    #       Other Europe and Eurasia                                
    #T57(202:217,IY,IS)=AIROUT(415:430,IY)	
    z[210] = dfd['AIROUT'].loc[423]	
 
    #       China                                                   
    #T57(202:217,IY,IS)=AIROUT(415:430,IY)	
    z[211] = dfd['AIROUT'].loc[424]	
 
    #       India                                                   
    #T57(202:217,IY,IS)=AIROUT(415:430,IY)	
    z[212] = dfd['AIROUT'].loc[425]	
 
    #       Other Non-OECD Asia                                     
    #T57(202:217,IY,IS)=AIROUT(415:430,IY)	
    z[213] = dfd['AIROUT'].loc[426]	
 
    #       Middle East                                             
    #T57(202:217,IY,IS)=AIROUT(415:430,IY)	
    z[214] = dfd['AIROUT'].loc[427]	
 
    #       Africa                                                  
    #T57(202:217,IY,IS)=AIROUT(415:430,IY)	
    z[215] = dfd['AIROUT'].loc[428]	
 
    #       Brazil                                                  
    #T57(202:217,IY,IS)=AIROUT(415:430,IY)	
    z[216] = dfd['AIROUT'].loc[429]	
 
    #       Other Non-OECD Americas                                 
    #T57(202:217,IY,IS)=AIROUT(415:430,IY)	
    z[217] = dfd['AIROUT'].loc[430]	
 
    #         Total World                                           
    #T57(218,IY,IS)=SUM(AIROUT(415:430,IY))
    z[218] = dfd['AIROUT'].loc[415:430,:].sum()
 
    #     Commercial Aviation Gasoline, U.S.                        
    #T57(219,IY,IS)=TRQAIRT(2,IY)
    z[219] = dfd['TRQAIRT'].loc[2]
    
    #     Military Jet Fuel, U.S.                                   
    #T57(220,IY,IS)=TRQMIL(1,IY)+TRQMIL(2,IY)
    z[220] = dfd['TRQMIL'].loc[1] + dfd['TRQMIL'].loc[2]
    
    return z
