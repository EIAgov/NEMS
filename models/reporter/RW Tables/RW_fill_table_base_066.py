# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_066(dfd, table_spec, table_id):
    """Fill table Technically Recoverable Natural Gas Resources by Region
   
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

    MNUMOR=dfd['MNUMOR_rwpre']
    #MNUMOR=14
    
    #   Technically Recoverable Natural Gas Resources by Region
    #   (trillion cubic feet)
    #     Resource Category                                         
    #                                                               
    #         East 
    #T66(1,IY,IS)=OGEOYURR(1,2,IY)
    z[1] =  dfd['OGEOYURR'].loc[1].loc[2]
    #         Gulf Coast 
    #T66(2,IY,IS)=OGEOYURR(2,2,IY)
    z[2] = dfd['OGEOYURR'].loc[2].loc[2]
    #         Midcontinent 
    #T66(3,IY,IS)=OGEOYURR(3,2,IY)
    z[3] = dfd['OGEOYURR'].loc[3].loc[2]
    #         Southwest
    #T66(4,IY,IS)=OGEOYURR(4,2,IY)
    z[4] = dfd['OGEOYURR'].loc[4].loc[2]
    #         Rocky Mountain
    #T66(5,IY,IS)=OGEOYURR(5,2,IY)
    z[5] = dfd['OGEOYURR'].loc[5].loc[2]
    #         Northern Great Plains 
    #T66(6,IY,IS)=OGEOYURR(6,2,IY)
    z[6] = dfd['OGEOYURR'].loc[6].loc[2]
    #         West Coast  
    #T66(7,IY,IS)=OGEOYURR(7,2,IY)
    z[7] = dfd['OGEOYURR'].loc[7].loc[2]
    #       Onshore 
    #T66(11,IY,IS)=FSUM(T66(1,IY,IS),7)
    z[11] = z[1]+z[2]+z[3]+z[4]+z[5]+z[6]+z[7]
    #         Atlantic 
    #T66(8,IY,IS)=OGEOYURR(8,2,IY)
    z[8] = dfd['OGEOYURR'].loc[8].loc[2]
    #         Gulf of Mexico   
    #T66(9,IY,IS)=OGEOYURR(9,2,IY)
    z[9] = dfd['OGEOYURR'].loc[9].loc[2]
    #         Pacific  
    #T66(10,IY,IS)=OGEOYURR(10,2,IY)
    z[10] = dfd['OGEOYURR'].loc[10].loc[2]
    #       Offshore
    #T66(12,IY,IS)=FSUM(T66(8,IY,IS),3)
    z[12] = z[8]+z[9]+z[10]
    #     Undiscovered Non-Associated Conventional 
    #T66(13,IY,IS)=FSUM(T66(1,IY,IS),10)
    z[13] = z[11]+z[12]
    #         East
    #T66(14,IY,IS)=OGEOYINF(1,2,IY)
    z[14] =  dfd['OGEOYINF'].loc[1].loc[2]
    #         Gulf Coast  
    #T66(15,IY,IS)=OGEOYINF(2,2,IY)
    z[15] = dfd['OGEOYINF'].loc[2].loc[2]
    #         Midcontinent  
    #T66(16,IY,IS)=OGEOYINF(3,2,IY)
    z[16] = dfd['OGEOYINF'].loc[3].loc[2]
    #         Southwest
    #T66(17,IY,IS)=OGEOYINF(4,2,IY)
    z[17] = dfd['OGEOYINF'].loc[4].loc[2]
    #         Rocky Mountain 
    #T66(18,IY,IS)=OGEOYINF(5,2,IY)
    z[18] = dfd['OGEOYINF'].loc[5].loc[2]
    #         Northern Great Plains
    #T66(19,IY,IS)=OGEOYINF(6,2,IY)
    z[19] = dfd['OGEOYINF'].loc[6].loc[2]
    #         West Coast
    #T66(20,IY,IS)=OGEOYINF(7,2,IY)
    z[20] = dfd['OGEOYINF'].loc[7].loc[2]
    #       Onshore 
    #T66(24,IY,IS)=FSUM(T66(14,IY,IS),7)
    z[24] =  z[14]+z[15]+z[16]+z[17]+z[18]+z[19]+z[20]
    #         Atlantic  
    #T66(21,IY,IS)=OGEOYINF(8,2,IY)
    z[21] = dfd['OGEOYINF'].loc[8].loc[2]
    #         Gulf of Mexico 
    #T66(22,IY,IS)=OGEOYINF(9,2,IY)
    z[22] = dfd['OGEOYINF'].loc[9].loc[2]
    #         Pacific  
    #T66(23,IY,IS)=OGEOYINF(10,2,IY)
    z[23] = dfd['OGEOYINF'].loc[10].loc[2]
    #       Offshore 
    #T66(25,IY,IS)=FSUM(T66(21,IY,IS),3)
    z[25] = z[21]+z[22]+z[23]
    #     Inferred Non-Associated Conventional  
    #T66(26,IY,IS)=FSUM(T66(14,IY,IS),10)
    z[26] = z[24] + z[25]
    #         East    
    #T66(27,IY,IS)=OGEOYUGR(1,1,IY)
    z[27] = dfd['OGEOYUGR'].loc[1].loc[1]
    #         Gulf Coast
    #T66(28,IY,IS)=OGEOYUGR(2,1,IY)
    z[28] = dfd['OGEOYUGR'].loc[2].loc[1]
    #         Midcontinent 
    #T66(29,IY,IS)=OGEOYUGR(3,1,IY)
    z[29] = dfd['OGEOYUGR'].loc[3].loc[1]
    #         Southwest 
    #T66(30,IY,IS)=OGEOYUGR(4,1,IY)
    z[30] = dfd['OGEOYUGR'].loc[4].loc[1]
    #         Rocky Mountain  
    #T66(31,IY,IS)=OGEOYUGR(5,1,IY)
    z[31] = dfd['OGEOYUGR'].loc[5].loc[1]
    #         Northern Great Plains
    #T66(32,IY,IS)=OGEOYUGR(6,1,IY)
    z[32] = dfd['OGEOYUGR'].loc[6].loc[1]
    #         West Coast 
    #T66(33,IY,IS)=OGEOYUGR(7,1,IY)
    z[33] = dfd['OGEOYUGR'].loc[7].loc[1]   
    
    #       Tight Gas
    #T66(48,IY,IS)=FSUM(T66(27,IY,IS),7)
    z[48] = z[27]+z[28]+z[29]+z[30]+z[31]+z[32]+z[33]
    #         East
    #T66(34,IY,IS)=OGEOYUGR(1,2,IY)
    z[34] = dfd['OGEOYUGR'].loc[1].loc[2]
    #         Gulf Coast                                            
    #T66(35,IY,IS)=OGEOYUGR(2,2,IY)
    z[35] = dfd['OGEOYUGR'].loc[2].loc[2]
    #         Midcontinent                                          
    #T66(36,IY,IS)=OGEOYUGR(3,2,IY)
    z[36] = dfd['OGEOYUGR'].loc[3].loc[2]
    #         Southwest                                             
    #T66(37,IY,IS)=OGEOYUGR(4,2,IY)
    z[37] = dfd['OGEOYUGR'].loc[4].loc[2]
    #         Rocky Mountain                                        
    #T66(38,IY,IS)=OGEOYUGR(5,2,IY)
    z[38] = dfd['OGEOYUGR'].loc[5].loc[2]
    #         Northern Great Plains                                 
    #T66(39,IY,IS)=OGEOYUGR(6,2,IY)
    z[39] = dfd['OGEOYUGR'].loc[6].loc[2]
    #         West Coast                                            
    #T66(40,IY,IS)=OGEOYUGR(7,2,IY)
    z[40] = dfd['OGEOYUGR'].loc[7].loc[2]
    #       Shale Gas
    #T66(49,IY,IS)=FSUM(T66(34,IY,IS),7)
    z[49] = z[34]+z[35]+z[36]+z[37]+z[38]+z[39]+z[40]
    #         East                                                  
    #T66(41,IY,IS)=OGEOYUGR(1,3,IY)
    z[41] = dfd['OGEOYUGR'].loc[1].loc[3]
    #         Gulf Coast                                            
    #T66(42,IY,IS)=OGEOYUGR(2,3,IY)
    z[42] = dfd['OGEOYUGR'].loc[2].loc[3]
    #         Midcontinent                                          
    #T66(43,IY,IS)=OGEOYUGR(3,3,IY)
    z[43] = dfd['OGEOYUGR'].loc[3].loc[3]
    #         Southwest                                             
    #T66(44,IY,IS)=OGEOYUGR(4,3,IY)
    z[44] = dfd['OGEOYUGR'].loc[4].loc[3]
    #         Rocky Mountain                                        
    #T66(45,IY,IS)=OGEOYUGR(5,3,IY)
    z[45] = dfd['OGEOYUGR'].loc[5].loc[3]
    #         Northern Great Plains                                 
    #T66(46,IY,IS)=OGEOYUGR(6,3,IY)
    z[46] = dfd['OGEOYUGR'].loc[6].loc[3]
    #         West Coast                                            
    #T66(47,IY,IS)=OGEOYUGR(7,3,IY)
    z[47] = dfd['OGEOYUGR'].loc[7].loc[3] 
    #       Coalbed Methane                                         
    #T66(50,IY,IS)=FSUM(T66(41,IY,IS),7)
    z[50] = z[41]+z[42]+z[43]+z[44]+z[45]+z[46]+z[47] 
    #     Unconventional Gas Recovery
    #T66(51,IY,IS)=FSUM(T66(27,IY,IS),21)
    z[51] = z[48]+z[49]+z[50] 
    #Lower 48 Unproved 
    #T66(52,IY,IS)=T66(13,IY,IS)+T66(26,IY,IS)+T66(51,IY,IS)
    z[52] = z[13]+z[26]+z[51]   
    #     Associated-Dissolved Gas                                  
    #T66(57,IY,IS)=SUM(OGEOYAD(1:MNL48N,IY)
    z[57] = dfd['OGEOYAD'].loc[1]+dfd['OGEOYAD'].loc[2]+dfd['OGEOYAD'].loc[3]+ dfd['OGEOYAD'].loc[4]+dfd['OGEOYAD'].loc[5]+dfd['OGEOYAD'].loc[6]++dfd['OGEOYAD'].loc[7]
    #   Alaska Unproved                                             
    #T66(53,IY,IS)=FSUM(OGEOYURR(11,2,IY),3)+FSUM(OGEOYINF(11,2,IY),3)
    z[53] = dfd['OGEOYURR'].loc[11].loc[2]+dfd['OGEOYURR'].loc[12].loc[2]+dfd['OGEOYURR'].loc[13].loc[2]+dfd['OGEOYINF'].loc[11].loc[2]+dfd['OGEOYINF'].loc[12].loc[2]+dfd['OGEOYINF'].loc[13].loc[2]
    #   Total Unproved                                              
    #T66(54,IY,IS)=FSUM(T66(52,IY,IS),2)+T66(57,IY,IS)
    z[54] = z[52]+z[53]+z[57]                                       
    #   Total Proved Reserves                                       
    #T66(55,IY,IS)=OGEOYRSV(MNUMOR,2,IY)
    z[55] = dfd['OGEOYRSV'].loc[MNUMOR].loc[2]
    #   Total Resources                                             
    #T66(56,IY,IS)=FSUM(T66(54,IY,IS),2)
    z[56] = z[54]+z[55]

    return z