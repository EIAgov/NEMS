# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""

def fill_table_base_108(dfd, table_spec, table_id):
    """Fill table for Domestic Crude Oil Production by Region and Type
   
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
    
    
       
    """
    
    z = {}
    
    MNUMOR = dfd['MNUMOR_rwpre']
    MNCRUD = dfd['MNCRUD_rwpre']
    RDAYS = dfd['RDAYS_rwpre']



    #   Domestic Crude Oil Production by Region and Type
    #   (million barrels per day)
    #    Production                                                 
    #                                                               
    #   Domestic Crude Oil Production                               
   
    #      Lower 48 Onshore                                         
    
 
    #         East                                                  
    #OGCRDPRD = OGCRDPRD / 365.    ! converting to million barrels per day 
    #            API 35-40 Sweet                                    
    #T108(1:6,IY,IS)=OGCRDPRD(1,1:6,IY)
    z[1] = dfd['OGCRDPRD'].loc[1].loc[1] / RDAYS
 
    #            API 35+ Sour                                       
    #T108(1:6,IY,IS)=OGCRDPRD(1,1:6,IY)
    z[2] = dfd['OGCRDPRD'].loc[1].loc[2]/ RDAYS
 
    #            API 27-35 MediSour                                 
    #T108(1:6,IY,IS)=OGCRDPRD(1,1:6,IY)
    z[3] = dfd['OGCRDPRD'].loc[1].loc[3]/ RDAYS
 
    #            API 27-35 Sour                                     
    #T108(1:6,IY,IS)=OGCRDPRD(1,1:6,IY)
    z[4] = dfd['OGCRDPRD'].loc[1].loc[4]/ RDAYS
 
    #            API less than 27 Sweet                             
    #T108(1:6,IY,IS)=OGCRDPRD(1,1:6,IY)
    z[5] = dfd['OGCRDPRD'].loc[1].loc[5]/ RDAYS
 
    #            API less than 27 Sour                              
    #T108(1:6,IY,IS)=OGCRDPRD(1,1:6,IY)
    z[6] = dfd['OGCRDPRD'].loc[1].loc[6]/ RDAYS
 
    #            API 40-50 Sweet                                    
    #T108(72:73,IY,IS)=OGCRDPRD(1,10:11,IY)
    z[72] = dfd['OGCRDPRD'].loc[1].loc[10]/ RDAYS
 
    #            API 50+ Sweet                                      
    #T108(72:73,IY,IS)=OGCRDPRD(1,10:11,IY)
    z[73] = dfd['OGCRDPRD'].loc[1].loc[11]/ RDAYS
    #T108(44,IY,IS)=FSUM(T108(1,IY,IS,6)+FSUM(T108(72,IY,IS,2)
    z[44] = z[1]+z[2]+z[3]+z[4]+z[5]+z[6] +z[72]+z[73]
    #         Gulf Coast                                            
   
 
    #            API 35-40 Sweet                                    
    #T108(7:12,IY,IS)=OGCRDPRD(2,1:6,IY)
    z[7] = dfd['OGCRDPRD'].loc[2].loc[1]/ RDAYS
 
    #            API 35+ Sour                                       
    #T108(7:12,IY,IS)=OGCRDPRD(2,1:6,IY)
    z[8] = dfd['OGCRDPRD'].loc[2].loc[2]/ RDAYS
 
    #            API 27-35 MediSour                                 
    #T108(7:12,IY,IS)=OGCRDPRD(2,1:6,IY)
    z[9] = dfd['OGCRDPRD'].loc[2].loc[3]/ RDAYS
 
    #            API 27-35 Sour                                     
    #T108(7:12,IY,IS)=OGCRDPRD(2,1:6,IY)
    z[10] = dfd['OGCRDPRD'].loc[2].loc[4]/ RDAYS
 
    #            API less than 27 Sweet                             
    #T108(7:12,IY,IS)=OGCRDPRD(2,1:6,IY)
    z[11] = dfd['OGCRDPRD'].loc[2].loc[5]/ RDAYS
 
    #            API less than 27 Sour                              
    #T108(7:12,IY,IS)=OGCRDPRD(2,1:6,IY)
    z[12] = dfd['OGCRDPRD'].loc[2].loc[6]/ RDAYS
 
    #            API 40-50 Sweet                                    
    #T108(74:75,IY,IS)=OGCRDPRD(2,10:11,IY)
    z[74] = dfd['OGCRDPRD'].loc[2].loc[10]/ RDAYS
 
    #            API 50+ Sweet                                      
    #T108(74:75,IY,IS)=OGCRDPRD(2,10:11,IY)
    z[75] = dfd['OGCRDPRD'].loc[2].loc[11]/ RDAYS
    #T108(45,IY,IS)=FSUM(T108(7,IY,IS,6)+FSUM(T108(74,IY,IS,2)
    z[45] = z[7]+z[8]+z[9]+z[10]+z[11]+z[12] +z[74]+z[75]
    #         Midcontinent                                          
    
 
    #            API 35-40 Sweet                                    
    #T108(13:18,IY,IS)=OGCRDPRD(3,1:6,IY)
    z[13] = dfd['OGCRDPRD'].loc[3].loc[1]/ RDAYS
 
    #            API 35+ Sour                                       
    #T108(13:18,IY,IS)=OGCRDPRD(3,1:6,IY)
    z[14] = dfd['OGCRDPRD'].loc[3].loc[2]/ RDAYS
 
    #            API 27-35 MediSour                                 
    #T108(13:18,IY,IS)=OGCRDPRD(3,1:6,IY)
    z[15] = dfd['OGCRDPRD'].loc[3].loc[3]/ RDAYS
 
    #            API 27-35 Sour                                     
    #T108(13:18,IY,IS)=OGCRDPRD(3,1:6,IY)
    z[16] = dfd['OGCRDPRD'].loc[3].loc[4]/ RDAYS
 
    #            API less than 27 Sweet                             
    #T108(13:18,IY,IS)=OGCRDPRD(3,1:6,IY)
    z[17] = dfd['OGCRDPRD'].loc[3].loc[5]/ RDAYS
 
    #            API less than 27 Sour                              
    #T108(13:18,IY,IS)=OGCRDPRD(3,1:6,IY)
    z[18] = dfd['OGCRDPRD'].loc[3].loc[6]/ RDAYS
 
    #            API 40-50 Sweet                                    
    #T108(76:77,IY,IS)=OGCRDPRD(3,10:11,IY)
    z[76] = dfd['OGCRDPRD'].loc[3].loc[10]/ RDAYS
 
    #            API 50+ Sweet                                      
    #T108(76:77,IY,IS)=OGCRDPRD(3,10:11,IY)
    z[77] = dfd['OGCRDPRD'].loc[3].loc[11]/ RDAYS
    #Midcontinent Total
    #T108(46,IY,IS)=FSUM(T108(13,IY,IS,6)+FSUM(T108(76,IY,IS,2)
    z[46] = z[13] +z[14]+z[15]+z[16]+z[17]+z[18] +z[76]+z[77]
    
                                            
    
   #         Southwest   
    #            API 35-40 Sweet                                    
    #T108(19:24,IY,IS)=OGCRDPRD(4,1:6,IY)
    z[19] = dfd['OGCRDPRD'].loc[4].loc[1]/RDAYS
   
    
    #            API 35+ Sour                                       
    #T108(19:24,IY,IS)=OGCRDPRD(4,1:6,IY)
    z[20] = dfd['OGCRDPRD'].loc[4].loc[2]/ RDAYS
 
    #            API 27-35 MediSour                                 
    #T108(19:24,IY,IS)=OGCRDPRD(4,1:6,IY)
    z[21] = dfd['OGCRDPRD'].loc[4].loc[3]/ RDAYS
 
    #            API 27-35 Sour                                     
    #T108(19:24,IY,IS)=OGCRDPRD(4,1:6,IY)
    z[22] = dfd['OGCRDPRD'].loc[4].loc[4]/ RDAYS
 
    #            API less than 27 Sweet                             
    #T108(19:24,IY,IS)=OGCRDPRD(4,1:6,IY)
    z[23] = dfd['OGCRDPRD'].loc[4].loc[5]/ RDAYS
 
    #            API less than 27 Sour                              
    #T108(19:24,IY,IS)=OGCRDPRD(4,1:6,IY)
    z[24] = dfd['OGCRDPRD'].loc[4].loc[6]/ RDAYS
 
    #            API 40-50 Sweet                                    
    #T108(78:79,IY,IS)=OGCRDPRD(4,10:11,IY)
    z[78] = dfd['OGCRDPRD'].loc[4].loc[10]/ RDAYS
 
    #            API 50+ Sweet                                      
    #T108(78:79,IY,IS)=OGCRDPRD(4,10:11,IY)
    z[79] = dfd['OGCRDPRD'].loc[4].loc[11]/ RDAYS
    #T108(47,IY,IS)=FSUM(T108(19,IY,IS,6)+FSUM(T108(78,IY,IS,2)
    z[47] = z[19]+z[20]+z[21]+z[22]+z[23]+z[24] +z[78]+z[79]
    #         Rocky Mountain                                        
    
 
    #            API 35-40 Sweet                                    
    #T108(25:30,IY,IS)=OGCRDPRD(5,1:6,IY)
    z[25] = dfd['OGCRDPRD'].loc[5].loc[1]/ RDAYS
 
    #            API 35+ Sour                                       
    #T108(25:30,IY,IS)=OGCRDPRD(5,1:6,IY)
    z[26] = dfd['OGCRDPRD'].loc[5].loc[2]/ RDAYS
 
    #            API 27-35 MediSour                                 
    #T108(25:30,IY,IS)=OGCRDPRD(5,1:6,IY)
    z[27] = dfd['OGCRDPRD'].loc[5].loc[3]/ RDAYS
 
    #            API 27-35 Sour                                     
    #T108(25:30,IY,IS)=OGCRDPRD(5,1:6,IY)
    z[28] = dfd['OGCRDPRD'].loc[5].loc[4]/ RDAYS
 
    #            API less than 27 Sweet                             
    #T108(25:30,IY,IS)=OGCRDPRD(5,1:6,IY)
    z[29] = dfd['OGCRDPRD'].loc[5].loc[5]/ RDAYS
 
    #            API less than 27 Sour                              
    #T108(25:30,IY,IS)=OGCRDPRD(5,1:6,IY)
    z[30] = dfd['OGCRDPRD'].loc[5].loc[6]/ RDAYS
 
    #            API 40-50 Sweet                                    
    #T108(80:81,IY,IS)=OGCRDPRD(5,10:11,IY)
    z[80] = dfd['OGCRDPRD'].loc[5].loc[10]/ RDAYS
 
    #            API 50+ Sweet                                      
    #T108(80:81,IY,IS)=OGCRDPRD(5,10:11,IY)
    z[81] = dfd['OGCRDPRD'].loc[5].loc[11]/ RDAYS
    #T108(48,IY,IS)=FSUM(T108(25,IY,IS,6)+FSUM(T108(80,IY,IS,2)
    z[48] = z[25]+z[26]+z[27]+z[28]+z[29]+z[30] +z[80]+z[81]
    #         Northern Great Plains                                 
   
    #            API 35-40 Sweet                                    
    #T108(31:36,IY,IS)=OGCRDPRD(6,1:6,IY)
    z[31] = dfd['OGCRDPRD'].loc[6].loc[1]/ RDAYS
 
    #            API 35+ Sour                                       
    #T108(31:36,IY,IS)=OGCRDPRD(6,1:6,IY)
    z[32] = dfd['OGCRDPRD'].loc[6].loc[2]/ RDAYS
 
    #            API 27-35 MediSour                                 
    #T108(31:36,IY,IS)=OGCRDPRD(6,1:6,IY)
    z[33] = dfd['OGCRDPRD'].loc[6].loc[3]/ RDAYS
 
    #            API 27-35 Sour                                     
    #T108(31:36,IY,IS)=OGCRDPRD(6,1:6,IY)
    z[34] = dfd['OGCRDPRD'].loc[6].loc[4]/ RDAYS
 
    #            API less than 27 Sweet                             
    #T108(31:36,IY,IS)=OGCRDPRD(6,1:6,IY)
    z[35] = dfd['OGCRDPRD'].loc[6].loc[5]/ RDAYS
 
    #            API less than 27 Sour                              
    #T108(31:36,IY,IS)=OGCRDPRD(6,1:6,IY)
    z[36] = dfd['OGCRDPRD'].loc[6].loc[6]/ RDAYS
 
    #            API 40-50 Sweet                                    
    #T108(82:83,IY,IS)=OGCRDPRD(6,10:11,IY)
    z[82] = dfd['OGCRDPRD'].loc[6].loc[10]/ RDAYS
 
    #            API 50+ Sweet                                      
    #T108(82:83,IY,IS)=OGCRDPRD(6,10:11,IY)
    z[83] = dfd['OGCRDPRD'].loc[6].loc[11]/ RDAYS
    #T108(49,IY,IS)=FSUM(T108(31,IY,IS,6)+FSUM(T108(82,IY,IS,2)
    z[49] = z[31]+z[32]+z[33]+z[34]+z[35]+z[36] +z[82]+z[83]
 
    #         West Coast, California                                
    #T108(37,IY,IS)=OGCRDPRD(7,7,IY)
    z[37] = dfd['OGCRDPRD'].loc[7].loc[7]/ RDAYS
    #T108(50,IY,IS)=FSUM(T108(44,IY,IS,6)+T108(37,IY,IS)
    z[50] = z[44]+z[45]+z[46]+z[47]+z[48]+z[49] +z[37]
    #      Lower 48 Offshore                                        
  
 
    #         Atlantic API 27-35 MediSour                           
    #T108(38,IY,IS)=OGCRDPRD(8,3,IY)
    z[38] = dfd['OGCRDPRD'].loc[8].loc[3]/ RDAYS
 
    #         Gulf of Mexico API 27-35 MediSour                     
    #T108(39,IY,IS)=OGCRDPRD(9,3,IY)
    z[39] = dfd['OGCRDPRD'].loc[9].loc[3]/ RDAYS
 
    #         Pacific California                                    
    #T108(40,IY,IS)=OGCRDPRD(10,7,IY)
    z[40] = dfd['OGCRDPRD'].loc[10].loc[7]/ RDAYS
    #T108(51,IY,IS)=FSUM(T108(38,IY,IS,3)
    z[51] = z[38]+z[39]+z[40]
   
 
    #         North Slope Offshore API 27-35 MediSour               
    #T108(41,IY,IS)=OGCRDPRD(11,3,IY)
    z[41] = dfd['OGCRDPRD'].loc[11].loc[3]/ RDAYS
 
    #         North Slope Onshore API 27-35 MediSour                
    #T108(42,IY,IS)=OGCRDPRD(12,3,IY)
    z[42] = dfd['OGCRDPRD'].loc[12].loc[3]/ RDAYS
    
    #         South Alaska API 27-35 MediSour                       
    #T108(43,IY,IS)=OGCRDPRD(13,3,IY)
    z[43] = dfd['OGCRDPRD'].loc[13].loc[3]/ RDAYS
    #      Alaska                                                   
    #T108(52,IY,IS)=FSUM(T108(41,IY,IS,3)
    z[52] = z[41]+z[42]+z[43]
    #      Other Crudes Not Listed                                  
    #T108(53,IY,IS)=SUM(OGCRDPRD(1:MNUMOR-1,1:MNCRUD,IY))-FSUM(T108(50,IY,IS,3)
    # z[53] = dfd['OGCRDPRD'].loc[1:MNUMOR-1,1:MNCRUD,:] - (z[50]+z[51]+z[52])
    z[53] = dfd['OGCRDPRD'].loc[1:MNUMOR-1,1:MNCRUD,:].sum()/ RDAYS - (z[50]+z[51]+z[52])

    #T108(54,IY,IS)=FSUM(T108(50,IY,IS,4)
    z[54] = z[50]+z[51]+z[52]+z[53]
    #                                                               
   
 
    #      API 35-40 Sweet                                          
    #T108(56,IY,IS)=SUM(OGCRDPRD(1:MNUMOR-1,1,IY))
    z[56] = dfd['OGCRDPRD'].loc[1:MNUMOR-1,1,:].sum()/ RDAYS
 
    #      API 35+ Sour                                             
    #T108(57,IY,IS)=SUM(OGCRDPRD(1:MNUMOR-1,2,IY))
    z[57] = dfd['OGCRDPRD'].loc[1:MNUMOR-1,2,:].sum()/ RDAYS
 
    #      API 27-35 MediSour                                       
    #T108(58,IY,IS)=SUM(OGCRDPRD(1:MNUMOR-1,3,IY))
    z[58] = dfd['OGCRDPRD'].loc[1:MNUMOR-1,3,:].sum()/ RDAYS
 
    #      API 27-35 Sour                                           
    #T108(59,IY,IS)=SUM(OGCRDPRD(1:MNUMOR-1,4,IY))
    z[59] = dfd['OGCRDPRD'].loc[1:MNUMOR-1,4,:].sum()/ RDAYS
 
    #      API less than 27 Sweet                                   
    #T108(60,IY,IS)=SUM(OGCRDPRD(1:MNUMOR-1,5,IY))
    z[60] = dfd['OGCRDPRD'].loc[1:MNUMOR-1,5,:].sum()/ RDAYS
 
    #      API less than 27 Sour                                    
    #T108(61,IY,IS)=SUM(OGCRDPRD(1:MNUMOR-1,6,IY))
    z[61] = dfd['OGCRDPRD'].loc[1:MNUMOR-1,6,:].sum()/ RDAYS
 
    #      California                                               
    #T108(62,IY,IS)=SUM(OGCRDPRD(1:MNUMOR-1,7,IY))
    z[62] = dfd['OGCRDPRD'].loc[1:MNUMOR-1,7,:].sum()/ RDAYS
 
    #      API 40-50 Sweet                                          
    #T108(84,IY,IS)=SUM(OGCRDPRD(1:MNUMOR-1,10,IY))
    z[84] = dfd['OGCRDPRD'].loc[1:MNUMOR-1,10,:].sum()/ RDAYS
 
    #      API 50+ Sweet                                            
    #T108(85,IY,IS)=SUM(OGCRDPRD(1:MNUMOR-1,11,IY))
    z[85] = dfd['OGCRDPRD'].loc[1:MNUMOR-1,11,:].sum()/ RDAYS
    #   Crude Oil Production by Type                                
    #T108(55,IY,IS)=FSUM(T108(56,IY,IS,7)+FSUM(T108(84,IY,IS,2)
    z[55] = z[56]+z[57]+z[58]+z[59]+z[60]+z[61]+z[62] +z[84]+z[85]
 
    #   Crude Oil Heat Content                                      

    #      API 35-40 Sweet                                          
    #T108(63:71,IY,IS)=OGCRDHEAT(1:9,IY)
    z[63] = dfd['OGCRDHEAT'].loc[1]
 
    #      API 35+ Sour                                             
    #T108(63:71,IY,IS)=OGCRDHEAT(1:9,IY)
    z[64] = dfd['OGCRDHEAT'].loc[2]
 
    #      API 27-35 MediSour                                       
    #T108(63:71,IY,IS)=OGCRDHEAT(1:9,IY)
    z[65] = dfd['OGCRDHEAT'].loc[3]
 
    #      API 27-35 Sour                                           
    #T108(63:71,IY,IS)=OGCRDHEAT(1:9,IY)
    z[66] = dfd['OGCRDHEAT'].loc[4]
 
    #      API less than 27 Sweet                                   
    #T108(63:71,IY,IS)=OGCRDHEAT(1:9,IY)
    z[67] = dfd['OGCRDHEAT'].loc[5]
 
    #      API less than 27 Sour                                    
    #T108(63:71,IY,IS)=OGCRDHEAT(1:9,IY)
    z[68] = dfd['OGCRDHEAT'].loc[6]
 
    #      California                                               
    #T108(63:71,IY,IS)=OGCRDHEAT(1:9,IY)
    z[69] = dfd['OGCRDHEAT'].loc[7]
 
    #      Syncrude                                                 
    #T108(63:71,IY,IS)=OGCRDHEAT(1:9,IY)
    z[70] = dfd['OGCRDHEAT'].loc[8]
 
    #      Dilbit/Synbit                                            
    #T108(63:71,IY,IS)=OGCRDHEAT(1:9,IY)
    z[71] = dfd['OGCRDHEAT'].loc[9]
 
    #      API 40-50 Sweet                                          
    #T108(86:87,IY,IS)=OGCRDHEAT(10:11,IY)
    z[86] = dfd['OGCRDHEAT'].loc[10]
 
    #      API 50+ Sweet                                            
    #T108(86:87,IY,IS)=OGCRDHEAT(10:11,IY)
    z[87] = dfd['OGCRDHEAT'].loc[11]
    
    return z
 
