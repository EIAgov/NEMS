# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_106(dfd, table_spec, table_id):
    """Fill table 
   
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



    #   (thousand barrels per day)
    #    Inputs to Refineries                                       
    #                                                               
    #   Region 1 - PADD I                                           
   

    #     API 35-40 Sweet                                           
    #T106(01:11,IY,IS)=RFCRUDEINP(1,1:11,IY)
    z[1] = dfd['RFCRUDEINP'].loc[1].loc[1]
 
    #     API 35+ Sour                                              
    #T106(01:11,IY,IS)=RFCRUDEINP(1,1:11,IY)
    z[2] = dfd['RFCRUDEINP'].loc[1].loc[2]
 
    #     API 27-35 MediSour                                        
    #T106(01:11,IY,IS)=RFCRUDEINP(1,1:11,IY)
    z[3] = dfd['RFCRUDEINP'].loc[1].loc[3]
 
    #     API 27-35 Sour                                            
    #T106(01:11,IY,IS)=RFCRUDEINP(1,1:11,IY)
    z[4] = dfd['RFCRUDEINP'].loc[1].loc[4]
 
    #     API less than 27 Sweet                                    
    #T106(01:11,IY,IS)=RFCRUDEINP(1,1:11,IY)
    z[5] = dfd['RFCRUDEINP'].loc[1].loc[5]
 
    #     API less than 27 Sour                                     
    #T106(01:11,IY,IS)=RFCRUDEINP(1,1:11,IY)
    z[6] = dfd['RFCRUDEINP'].loc[1].loc[6]
 
    #     California                                                
    #T106(01:11,IY,IS)=RFCRUDEINP(1,1:11,IY)
    z[7] = dfd['RFCRUDEINP'].loc[1].loc[7]
 
    #     Syncrude                                                  
    #T106(01:11,IY,IS)=RFCRUDEINP(1,1:11,IY)
    z[8] = dfd['RFCRUDEINP'].loc[1].loc[8]
 
    #     Dilbit/Synbit                                             
    #T106(01:11,IY,IS)=RFCRUDEINP(1,1:11,IY)
    z[9] = dfd['RFCRUDEINP'].loc[1].loc[9]
 
    #     API 40-50 Sweet                                           
    #T106(01:11,IY,IS)=RFCRUDEINP(1,1:11,IY)
    z[10] = dfd['RFCRUDEINP'].loc[1].loc[10]
 
    #     API 50+ Sweet                                             
    #T106(01:11,IY,IS)=RFCRUDEINP(1,1:11,IY)
    z[11] = dfd['RFCRUDEINP'].loc[1].loc[11]
 
    #T106(12,IY,IS)=FSUM(T106(01,IY,IS),11)
    z[12] = z[1]+z[2]+z[3]+z[4]+z[5]+z[6]+z[7]+z[8]+z[9]+z[10]+z[11]                                                              


 
    #   Region 2 - PADD II Inland                                   
    
 
    #     API 35-40 Sweet                                           
    #T106(13:23,IY,IS)=RFCRUDEINP(2,1:11,IY)
    z[13] = dfd['RFCRUDEINP'].loc[2].loc[1]
 
    #     API 35+ Sour                                              
    #T106(13:23,IY,IS)=RFCRUDEINP(2,1:11,IY)
    z[14] = dfd['RFCRUDEINP'].loc[2].loc[2]
 
    #     API 27-35 MediSour                                        
    #T106(13:23,IY,IS)=RFCRUDEINP(2,1:11,IY)
    z[15] = dfd['RFCRUDEINP'].loc[2].loc[3]
 
    #     API 27-35 Sour                                            
    #T106(13:23,IY,IS)=RFCRUDEINP(2,1:11,IY)
    z[16] = dfd['RFCRUDEINP'].loc[2].loc[4]
 
    #     API less than 27 Sweet                                    
    #T106(13:23,IY,IS)=RFCRUDEINP(2,1:11,IY)
    z[17] = dfd['RFCRUDEINP'].loc[2].loc[5]
 
    #     API less than 27 Sour                                     
    #T106(13:23,IY,IS)=RFCRUDEINP(2,1:11,IY)
    z[18] = dfd['RFCRUDEINP'].loc[2].loc[6]
 
    #     California                                                
    #T106(13:23,IY,IS)=RFCRUDEINP(2,1:11,IY)
    z[19] = dfd['RFCRUDEINP'].loc[2].loc[7]
 
    #     Syncrude                                                  
    #T106(13:23,IY,IS)=RFCRUDEINP(2,1:11,IY)
    z[20] = dfd['RFCRUDEINP'].loc[2].loc[8]
 
    #     Dilbit/Synbit                                             
    #T106(13:23,IY,IS)=RFCRUDEINP(2,1:11,IY)
    z[21] = dfd['RFCRUDEINP'].loc[2].loc[9]
 
    #     API 40-50 Sweet                                           
    #T106(13:23,IY,IS)=RFCRUDEINP(2,1:11,IY)
    z[22] = dfd['RFCRUDEINP'].loc[2].loc[10]
 
    #     API 50+ Sweet                                             
    #T106(13:23,IY,IS)=RFCRUDEINP(2,1:11,IY)
    z[23] = dfd['RFCRUDEINP'].loc[2].loc[11]
 
    #T106(24,IY,IS)=FSUM(T106(13,IY,IS),11)
    z[24] = z[13]+z[14]+z[15]+z[16]+z[17]+z[18]+z[19]+z[20]+z[21]+z[22]+z[23]                                                               


 
    #   Region 3 - PADD II Lakes                                    
   
 
    #     API 35-40 Sweet                                           
    #T106(25:35,IY,IS)=RFCRUDEINP(3,1:11,IY)
    z[25] = dfd['RFCRUDEINP'].loc[3].loc[1]
 
    #     API 35+ Sour                                              
    #T106(25:35,IY,IS)=RFCRUDEINP(3,1:11,IY)
    z[26] = dfd['RFCRUDEINP'].loc[3].loc[2]
 
    #     API 27-35 MediSour                                        
    #T106(25:35,IY,IS)=RFCRUDEINP(3,1:11,IY)
    z[27] = dfd['RFCRUDEINP'].loc[3].loc[3]
 
    #     API 27-35 Sour                                            
    #T106(25:35,IY,IS)=RFCRUDEINP(3,1:11,IY)
    z[28] = dfd['RFCRUDEINP'].loc[3].loc[4]
 
    #     API less than 27 Sweet                                    
    #T106(25:35,IY,IS)=RFCRUDEINP(3,1:11,IY)
    z[29] = dfd['RFCRUDEINP'].loc[3].loc[5]
 
    #     API less than 27 Sour                                     
    #T106(25:35,IY,IS)=RFCRUDEINP(3,1:11,IY)
    z[30] = dfd['RFCRUDEINP'].loc[3].loc[6]
 
    #     California                                                
    #T106(25:35,IY,IS)=RFCRUDEINP(3,1:11,IY)
    z[31] = dfd['RFCRUDEINP'].loc[3].loc[7]
 
    #     Syncrude                                                  
    #T106(25:35,IY,IS)=RFCRUDEINP(3,1:11,IY)
    z[32] = dfd['RFCRUDEINP'].loc[3].loc[8]
 
    #     Dilbit/Synbit                                             
    #T106(25:35,IY,IS)=RFCRUDEINP(3,1:11,IY)
    z[33] = dfd['RFCRUDEINP'].loc[3].loc[9]
 
    #     API 40-50 Sweet                                           
    #T106(25:35,IY,IS)=RFCRUDEINP(3,1:11,IY)
    z[34] = dfd['RFCRUDEINP'].loc[3].loc[10]
 
    #     API 50+ Sweet                                             
    #T106(25:35,IY,IS)=RFCRUDEINP(3,1:11,IY)
    z[35] = dfd['RFCRUDEINP'].loc[3].loc[11]
 
    #T106(36,IY,IS)=FSUM(T106(25,IY,IS),11)
    z[36] = z[25]+z[26]+z[27]+z[28]+z[29]+z[30]+z[31]+z[32]+z[33]+z[34]+z[35]                                                               


 
    #   Region 4 - PADD III Gulf                                    
    
 
    #     API 35-40 Sweet                                           
    #T106(37:47,IY,IS)=RFCRUDEINP(4,1:11,IY)
    z[37] = dfd['RFCRUDEINP'].loc[4].loc[1]
 
    #     API 35+ Sour                                              
    #T106(37:47,IY,IS)=RFCRUDEINP(4,1:11,IY)
    z[38] = dfd['RFCRUDEINP'].loc[4].loc[2]
 
    #     API 27-35 MediSour                                        
    #T106(37:47,IY,IS)=RFCRUDEINP(4,1:11,IY)
    z[39] = dfd['RFCRUDEINP'].loc[4].loc[3]
 
    #     API 27-35 Sour                                            
    #T106(37:47,IY,IS)=RFCRUDEINP(4,1:11,IY)
    z[40] = dfd['RFCRUDEINP'].loc[4].loc[4]
 
    #     API less than 27 Sweet                                    
    #T106(37:47,IY,IS)=RFCRUDEINP(4,1:11,IY)
    z[41] = dfd['RFCRUDEINP'].loc[4].loc[5]
 
    #     API less than 27 Sour                                     
    #T106(37:47,IY,IS)=RFCRUDEINP(4,1:11,IY)
    z[42] = dfd['RFCRUDEINP'].loc[4].loc[6]
 
    #     California                                                
    #T106(37:47,IY,IS)=RFCRUDEINP(4,1:11,IY)
    z[43] = dfd['RFCRUDEINP'].loc[4].loc[7]
 
    #     Syncrude                                                  
    #T106(37:47,IY,IS)=RFCRUDEINP(4,1:11,IY)
    z[44] = dfd['RFCRUDEINP'].loc[4].loc[8]
 
    #     Dilbit/Synbit                                             
    #T106(37:47,IY,IS)=RFCRUDEINP(4,1:11,IY)
    z[45] = dfd['RFCRUDEINP'].loc[4].loc[9]
 
    #     API 40-50 Sweet                                           
    #T106(37:47,IY,IS)=RFCRUDEINP(4,1:11,IY)
    z[46] = dfd['RFCRUDEINP'].loc[4].loc[10]
 
    #     API 50+ Sweet                                             
    #T106(37:47,IY,IS)=RFCRUDEINP(4,1:11,IY)
    z[47] = dfd['RFCRUDEINP'].loc[4].loc[11]
 
    #T106(48,IY,IS)=FSUM(T106(37,IY,IS),11)
    z[48] = z[37]+z[38]+z[39]+z[40]+z[41]+z[42]+z[43]+z[44]+z[45]+z[46]+z[47]                                                               

 
    #   Region 5 - PADD III Inland                                  
    
 
    #     API 35-40 Sweet                                           
    #T106(49:59,IY,IS)=RFCRUDEINP(5,1:11,IY)
    z[49] = dfd['RFCRUDEINP'].loc[5].loc[1]
 
    #     API 35+ Sour                                              
    #T106(49:59,IY,IS)=RFCRUDEINP(5,1:11,IY)
    z[50] = dfd['RFCRUDEINP'].loc[5].loc[2]
 
    #     API 27-35 MediSour                                        
    #T106(49:59,IY,IS)=RFCRUDEINP(5,1:11,IY)
    z[51] = dfd['RFCRUDEINP'].loc[5].loc[3]
 
    #     API 27-35 Sour                                            
    #T106(49:59,IY,IS)=RFCRUDEINP(5,1:11,IY)
    z[52] = dfd['RFCRUDEINP'].loc[5].loc[4]
 
    #     API less than 27 Sweet                                    
    #T106(49:59,IY,IS)=RFCRUDEINP(5,1:11,IY)
    z[53] = dfd['RFCRUDEINP'].loc[5].loc[5]
 
    #     API less than 27 Sour                                     
    #T106(49:59,IY,IS)=RFCRUDEINP(5,1:11,IY)
    z[54] = dfd['RFCRUDEINP'].loc[5].loc[6]
 
    #     California                                                
    #T106(49:59,IY,IS)=RFCRUDEINP(5,1:11,IY)
    z[55] = dfd['RFCRUDEINP'].loc[5].loc[7]
 
    #     Syncrude                                                  
    #T106(49:59,IY,IS)=RFCRUDEINP(5,1:11,IY)
    z[56] = dfd['RFCRUDEINP'].loc[5].loc[8]
 
    #     Dilbit/Synbit                                             
    #T106(49:59,IY,IS)=RFCRUDEINP(5,1:11,IY)
    z[57] = dfd['RFCRUDEINP'].loc[5].loc[9]
 
    #     API 40-50 Sweet                                           
    #T106(49:59,IY,IS)=RFCRUDEINP(5,1:11,IY)
    z[58] = dfd['RFCRUDEINP'].loc[5].loc[10]
 
    #     API 50+ Sweet                                             
    #T106(49:59,IY,IS)=RFCRUDEINP(5,1:11,IY)
    z[59] = dfd['RFCRUDEINP'].loc[5].loc[11]
 
    #T106(60,IY,IS)=FSUM(T106(49,IY,IS),11)
    z[60] = z[49]+z[50]+z[51]+z[52]+z[53]+z[54]+z[55]+z[56]+z[57]+z[58]+z[59]                                                               

 
    #   Region 6 - PADD IV                                          
   
 
    #     API 35-40 Sweet                                           
    #T106(61:71,IY,IS)=RFCRUDEINP(6,1:11,IY)
    z[61] = dfd['RFCRUDEINP'].loc[6].loc[1]
 
    #     API 35+ Sour                                              
    #T106(61:71,IY,IS)=RFCRUDEINP(6,1:11,IY)
    z[62] = dfd['RFCRUDEINP'].loc[6].loc[2]
 
    #     API 27-35 MediSour                                        
    #T106(61:71,IY,IS)=RFCRUDEINP(6,1:11,IY)
    z[63] = dfd['RFCRUDEINP'].loc[6].loc[3]
 
    #     API 27-35 Sour                                            
    #T106(61:71,IY,IS)=RFCRUDEINP(6,1:11,IY)
    z[64] = dfd['RFCRUDEINP'].loc[6].loc[4]
 
    #     API less than 27 Sweet                                    
    #T106(61:71,IY,IS)=RFCRUDEINP(6,1:11,IY)
    z[65] = dfd['RFCRUDEINP'].loc[6].loc[5]
 
    #     API less than 27 Sour                                     
    #T106(61:71,IY,IS)=RFCRUDEINP(6,1:11,IY)
    z[66] = dfd['RFCRUDEINP'].loc[6].loc[6]
 
    #     California                                                
    #T106(61:71,IY,IS)=RFCRUDEINP(6,1:11,IY)
    z[67] = dfd['RFCRUDEINP'].loc[6].loc[7]
 
    #     Syncrude                                                  
    #T106(61:71,IY,IS)=RFCRUDEINP(6,1:11,IY)
    z[68] = dfd['RFCRUDEINP'].loc[6].loc[8]
 
    #     Dilbit/Synbit                                             
    #T106(61:71,IY,IS)=RFCRUDEINP(6,1:11,IY)
    z[69] = dfd['RFCRUDEINP'].loc[6].loc[9]
 
    #     API 40-50 Sweet                                           
    #T106(61:71,IY,IS)=RFCRUDEINP(6,1:11,IY)
    z[70] = dfd['RFCRUDEINP'].loc[6].loc[10]
 
    #     API 50+ Sweet                                             
    #T106(61:71,IY,IS)=RFCRUDEINP(6,1:11,IY)
    z[71] = dfd['RFCRUDEINP'].loc[6].loc[11]
 
     #T106(72,IY,IS)=FSUM(T106(61,IY,IS),11)
    z[72] = z[61]+z[62]+z[63]+z[64]+z[65]+z[66]+z[67]+z[68]+z[69]+z[70]+z[71]                                                               


 
    #   Region 7 - PADD V California                                
   
 
    #     API 35-40 Sweet                                           
    #T106(73:83,IY,IS)=RFCRUDEINP(7,1:11,IY)
    z[73] = dfd['RFCRUDEINP'].loc[7].loc[1]
 
    #     API 35+ Sour                                              
    #T106(73:83,IY,IS)=RFCRUDEINP(7,1:11,IY)
    z[74] = dfd['RFCRUDEINP'].loc[7].loc[2]
 
    #     API 27-35 MediSour                                        
    #T106(73:83,IY,IS)=RFCRUDEINP(7,1:11,IY)
    z[75] = dfd['RFCRUDEINP'].loc[7].loc[3]
 
    #     API 27-35 Sour                                            
    #T106(73:83,IY,IS)=RFCRUDEINP(7,1:11,IY)
    z[76] = dfd['RFCRUDEINP'].loc[7].loc[4]
 
    #     API less than 27 Sweet                                    
    #T106(73:83,IY,IS)=RFCRUDEINP(7,1:11,IY)
    z[77] = dfd['RFCRUDEINP'].loc[7].loc[5]
 
    #     API less than 27 Sour                                     
    #T106(73:83,IY,IS)=RFCRUDEINP(7,1:11,IY)
    z[78] = dfd['RFCRUDEINP'].loc[7].loc[6]
 
    #     California                                                
    #T106(73:83,IY,IS)=RFCRUDEINP(7,1:11,IY)
    z[79] = dfd['RFCRUDEINP'].loc[7].loc[7]
 
    #     Syncrude                                                  
    #T106(73:83,IY,IS)=RFCRUDEINP(7,1:11,IY)
    z[80] = dfd['RFCRUDEINP'].loc[7].loc[8]
 
    #     Dilbit/Synbit                                             
    #T106(73:83,IY,IS)=RFCRUDEINP(7,1:11,IY)
    z[81] = dfd['RFCRUDEINP'].loc[7].loc[9]
 
    #     API 40-50 Sweet                                           
    #T106(73:83,IY,IS)=RFCRUDEINP(7,1:11,IY)
    z[82] = dfd['RFCRUDEINP'].loc[7].loc[10]
 
    #     API 50+ Sweet                                             
    #T106(73:83,IY,IS)=RFCRUDEINP(7,1:11,IY)
    z[83] = dfd['RFCRUDEINP'].loc[7].loc[11]
 
     #T106(84,IY,IS)=FSUM(T106(73,IY,IS),11)
    z[84] = z[73]+z[74]+z[75]+z[76]+z[77]+z[78]+z[79]+z[80]+z[81]+z[82]+z[83]                                                               


 
    #   Region 8 - PADD V Other                                     
   
 
    #     API 35-40 Sweet                                           
    #T106(85:95,IY,IS)=RFCRUDEINP(8,1:11,IY)
    z[85] = dfd['RFCRUDEINP'].loc[8].loc[1]
 
    #     API 35+ Sour                                              
    #T106(85:95,IY,IS)=RFCRUDEINP(8,1:11,IY)
    z[86] = dfd['RFCRUDEINP'].loc[8].loc[2]
 
    #     API 27-35 MediSour                                        
    #T106(85:95,IY,IS)=RFCRUDEINP(8,1:11,IY)
    z[87] = dfd['RFCRUDEINP'].loc[8].loc[3]
 
    #     API 27-35 Sour                                            
    #T106(85:95,IY,IS)=RFCRUDEINP(8,1:11,IY)
    z[88] = dfd['RFCRUDEINP'].loc[8].loc[4]
 
    #     API less than 27 Sweet                                    
    #T106(85:95,IY,IS)=RFCRUDEINP(8,1:11,IY)
    z[89] = dfd['RFCRUDEINP'].loc[8].loc[5]
 
    #     API less than 27 Sour                                     
    #T106(85:95,IY,IS)=RFCRUDEINP(8,1:11,IY)
    z[90] = dfd['RFCRUDEINP'].loc[8].loc[6]
 
    #     California                                                
    #T106(85:95,IY,IS)=RFCRUDEINP(8,1:11,IY)
    z[91] = dfd['RFCRUDEINP'].loc[8].loc[7]
 
    #     Syncrude                                                  
    #T106(85:95,IY,IS)=RFCRUDEINP(8,1:11,IY)
    z[92] = dfd['RFCRUDEINP'].loc[8].loc[8]
 
    #     Dilbit/Synbit                                             
    #T106(85:95,IY,IS)=RFCRUDEINP(8,1:11,IY)
    z[93] = dfd['RFCRUDEINP'].loc[8].loc[9]
 
    #     API 40-50 Sweet                                           
    #T106(85:95,IY,IS)=RFCRUDEINP(8,1:11,IY)
    z[94] = dfd['RFCRUDEINP'].loc[8].loc[10]
 
    #     API 50+ Sweet                                             
    #T106(85:95,IY,IS)=RFCRUDEINP(8,1:11,IY)
    z[95] = dfd['RFCRUDEINP'].loc[8].loc[11]
 
     #T106(96,IY,IS)=FSUM(T106(85,IY,IS),11)
    z[96] = z[85]+z[86]+z[87]+z[88]+z[89]+z[90]+z[91]+z[92]+z[93]+z[94]+z[95]                                                               
 
    #   United States                                               
   
 
    #     API 35-40 Sweet                                           
    #T106(97,IY,IS)=SUM(RFCRUDEINP(1:8,PADD,IY)
    z[97] = dfd['RFCRUDEINP'].loc[((1,2,3,4,5,6,7,8),1),:].sum()
 
    #     API 35+ Sour                                              
    #T106(98,IY,IS)=SUM(RFCRUDEINP(1:8,PADD,IY)
    z[98] = dfd['RFCRUDEINP'].loc[((1,2,3,4,5,6,7,8),2),:].sum()
 
    #     API 27-35 MediSour                                        
    #T106(99,IY,IS)=SUM(RFCRUDEINP(1:8,PADD,IY)
    z[99] = dfd['RFCRUDEINP'].loc[((1,2,3,4,5,6,7,8),3),:].sum()
 
    #     API 27-35 Sour                                            
    #T106(100,IY,IS)=SUM(RFCRUDEINP(1:8,PADD,IY)
    z[100] = dfd['RFCRUDEINP'].loc[((1,2,3,4,5,6,7,8),4),:].sum()
 
    #     API less than 27 Sweet                                    
    #T106(101,IY,IS)=SUM(RFCRUDEINP(1:8,PADD,IY)
    z[101] = dfd['RFCRUDEINP'].loc[((1,2,3,4,5,6,7,8),5),:].sum()
 
    #     API less than 27 Sour                                     
    #T106(102,IY,IS)=SUM(RFCRUDEINP(1:8,PADD,IY)
    z[102] = dfd['RFCRUDEINP'].loc[((1,2,3,4,5,6,7,8),6),:].sum()
 
    #     California                                                
    #T106(103,IY,IS)=SUM(RFCRUDEINP(1:8,PADD,IY)
    z[103] = dfd['RFCRUDEINP'].loc[((1,2,3,4,5,6,7,8),7),:].sum()
 
    #     Syncrude                                                  
    #T106(104,IY,IS)=SUM(RFCRUDEINP(1:8,PADD,IY)
    z[104] = dfd['RFCRUDEINP'].loc[((1,2,3,4,5,6,7,8),8),:].sum()
 
    #     Dilbit/Synbit                                             
    #T106(105,IY,IS)=SUM(RFCRUDEINP(1:8,PADD,IY)
    z[105] = dfd['RFCRUDEINP'].loc[((1,2,3,4,5,6,7,8),9),:].sum()
 
    #     API 40-50 Sweet                                           
    #T106(106,IY,IS)=SUM(RFCRUDEINP(1:8,PADD,IY)
    z[106] = dfd['RFCRUDEINP'].loc[((1,2,3,4,5,6,7,8),10),:].sum()
 
    #     API 50+ Sweet                                             
    #T106(107,IY,IS)=SUM(RFCRUDEINP(1:8,PADD,IY)
    z[107] = dfd['RFCRUDEINP'].loc[((1,2,3,4,5,6,7,8),11),:].sum()
 
     #T106(108,IY,IS)=FSUM(T106(97,IY,IS),11)
    z[108] = z[97]+z[98]+z[99]+z[100]+z[101]+z[102]+z[103]+z[104]+z[105]+z[106]+z[107]                                                               

 
    #   Region 9 - Maritime Canada and Caribbean                    
   
 
    #     API 35-40 Sweet                                           
    #T106(109:119,IY,IS)=RFCRUDEINP(9,1:11,IY)
    z[109] = dfd['RFCRUDEINP'].loc[9].loc[1]
 
    #     API 35+ Sour                                              
    #T106(109:119,IY,IS)=RFCRUDEINP(9,1:11,IY)
    z[110] = dfd['RFCRUDEINP'].loc[9].loc[2]
 
    #     API 27-35 MediSour                                        
    #T106(109:119,IY,IS)=RFCRUDEINP(9,1:11,IY)
    z[111] = dfd['RFCRUDEINP'].loc[9].loc[3]
 
    #     API 27-35 Sour                                            
    #T106(109:119,IY,IS)=RFCRUDEINP(9,1:11,IY)
    z[112] = dfd['RFCRUDEINP'].loc[9].loc[4]
 
    #     API less than 27 Sweet                                    
    #T106(109:119,IY,IS)=RFCRUDEINP(9,1:11,IY)
    z[113] = dfd['RFCRUDEINP'].loc[9].loc[5]
 
    #     API less than 27 Sour                                     
    #T106(109:119,IY,IS)=RFCRUDEINP(9,1:11,IY)
    z[114] = dfd['RFCRUDEINP'].loc[9].loc[6]
 
    #     California                                                
    #T106(109:119,IY,IS)=RFCRUDEINP(9,1:11,IY)
    z[115] = dfd['RFCRUDEINP'].loc[9].loc[7]
 
    #     Syncrude                                                  
    #T106(109:119,IY,IS)=RFCRUDEINP(9,1:11,IY)
    z[116] = dfd['RFCRUDEINP'].loc[9].loc[8]
 
    #     Dilbit/Synbit                                             
    #T106(109:119,IY,IS)=RFCRUDEINP(9,1:11,IY)
    z[117] = dfd['RFCRUDEINP'].loc[9].loc[9]
 
    #     API 40-50 Sweet                                           
    #T106(109:119,IY,IS)=RFCRUDEINP(9,1:11,IY)
    z[118] = dfd['RFCRUDEINP'].loc[9].loc[10]
 
    #     API 50+ Sweet                                             
    #T106(109:119,IY,IS)=RFCRUDEINP(9,1:11,IY)
    z[119] = dfd['RFCRUDEINP'].loc[9].loc[11]
    #T106(120,IY,IS)=FSUM(T106(109,IY,IS),11)
    z[120] = z[109]+z[110]+z[111]+z[112]+z[113]+z[114]+z[115]+z[116]+z[117]+z[118]+z[119]

    return z