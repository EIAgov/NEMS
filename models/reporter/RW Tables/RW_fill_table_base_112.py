# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_112(dfd, table_spec, table_id):
    """Fill table Electric Power Sector Generating Capacity and Generation by Plant Type and Technology
   
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



    #   Biomass Quantities by Coal Region, Sector, and Type
    #   Quantities                                                  
    #   Quantity for All Types                                      
    #     Electric Power                                            
    #       New England                                             
    #T112(1:16,IY,IS)=QBMPWCL(0,1:16,IY)
    z[1] = dfd['QBMPWCL'].loc[1].loc[2]
    #       Middle Atlantic                                         
    #T112(1:16,IY,IS)=QBMPWCL(0,1:16,IY)
    z[2] = dfd['QBMPWCL'].loc[1].loc[3]
 
    #       South Atlantic 1                                        
    #T112(1:16,IY,IS)=QBMPWCL(0,1:16,IY)
    z[3] = dfd['QBMPWCL'].loc[1].loc[4]
 
    #       Virginia, Carolinas (South Atlantic 2)                  
    #T112(1:16,IY,IS)=QBMPWCL(0,1:16,IY)
    z[4] = dfd['QBMPWCL'].loc[1].loc[5]
 
    #       Georgia and Florida (South Atlantic 3)                  
    #T112(1:16,IY,IS)=QBMPWCL(0,1:16,IY)
    z[5] = dfd['QBMPWCL'].loc[1].loc[6]
 
    #       Ohio (East North Central 1)                             
    #T112(1:16,IY,IS)=QBMPWCL(0,1:16,IY)
    z[6] = dfd['QBMPWCL'].loc[1].loc[7]
 
    #       East North Central 2                                    
    #T112(1:16,IY,IS)=QBMPWCL(0,1:16,IY)
    z[7] = dfd['QBMPWCL'].loc[1].loc[8]
 
    #       Kentucky and Tennessee (East South Cent 1)              
    #T112(1:16,IY,IS)=QBMPWCL(0,1:16,IY)
    z[8] = dfd['QBMPWCL'].loc[1].loc[9]
 
    #       Alabama and Mississippi (East South Cent 2)             
    #T112(1:16,IY,IS)=QBMPWCL(0,1:16,IY)
    z[9] = dfd['QBMPWCL'].loc[1].loc[10]
 
    #       Minnesota, Dakotas (West North Central 1)               
    #T112(1:16,IY,IS)=QBMPWCL(0,1:16,IY)
    z[10] = dfd['QBMPWCL'].loc[1].loc[11]
 
    #       West North Central 2                                    
    #T112(1:16,IY,IS)=QBMPWCL(0,1:16,IY)
    z[11] = dfd['QBMPWCL'].loc[1].loc[12]
 
    #       West South Central                                      
    #T112(1:16,IY,IS)=QBMPWCL(0,1:16,IY)
    z[12] = dfd['QBMPWCL'].loc[1].loc[13]
 
    #       Montana, Wyoming, and Idaho (Mountain 1)                
    #T112(1:16,IY,IS)=QBMPWCL(0,1:16,IY)
    z[13] = dfd['QBMPWCL'].loc[1].loc[14]
 
    #       Colorado, Utah, and Nevada (Mountain 2)                 
    #T112(1:16,IY,IS)=QBMPWCL(0,1:16,IY)
    z[14] = dfd['QBMPWCL'].loc[1].loc[15]
 
    #       Arizona and New Mexico (Mountain 3)                     
    #T112(1:16,IY,IS)=QBMPWCL(0,1:16,IY)
    z[15] = dfd['QBMPWCL'].loc[1].loc[16]
 
    #       Pacific                                                 
    #T112(1:16,IY,IS)=QBMPWCL(0,1:16,IY)
    z[16] = dfd['QBMPWCL'].loc[1].loc[17]
 
    #     Electric Power Total, All Types                           
    #T112(161,IY,IS)=FSUM(T112(1,IY,IS),16)
    z[161] = z[1]+z[2]+z[3]+z[4]+z[5]+z[6]+z[7]+z[8]+z[9]+z[10]+z[11]+z[12]+z[13]+z[14]+z[15]+z[16]
 
    #     Refining                                                  

 
    #       New England                                             
    #T112(17:32,IY,IS)=QBMETCL(0,1:16,IY)+QBMBTCL(0,1:16,IY)
    z[17] = dfd['QBMETCL'].loc[1].loc[2]+dfd['QBMBTCL'].loc[1].loc[2]
 
    #       Middle Atlantic                                         
    #T112(17:32,IY,IS)=QBMETCL(0,1:16,IY)+QBMBTCL(0,1:16,IY)
    z[18] = dfd['QBMETCL'].loc[1].loc[3]+dfd['QBMBTCL'].loc[1].loc[3]
 
    #       South Atlantic 1                                        
    #T112(17:32,IY,IS)=QBMETCL(0,1:16,IY)+QBMBTCL(0,1:16,IY)
    z[19] = dfd['QBMETCL'].loc[1].loc[4]+dfd['QBMBTCL'].loc[1].loc[4]
 
    #       Virginia, Carolinas (South Atlantic 2)                  
    #T112(17:32,IY,IS)=QBMETCL(0,1:16,IY)+QBMBTCL(0,1:16,IY)
    z[20] = dfd['QBMETCL'].loc[1].loc[5]+dfd['QBMBTCL'].loc[1].loc[5]
 
    #       Georgia and Florida (South Atlantic 3)                  
    #T112(17:32,IY,IS)=QBMETCL(0,1:16,IY)+QBMBTCL(0,1:16,IY)
    z[21] = dfd['QBMETCL'].loc[1].loc[6]+dfd['QBMBTCL'].loc[1].loc[6]
 
    #       Ohio (East North Central 1)                             
    #T112(17:32,IY,IS)=QBMETCL(0,1:16,IY)+QBMBTCL(0,1:16,IY)
    z[22] = dfd['QBMETCL'].loc[1].loc[7]+dfd['QBMBTCL'].loc[1].loc[7]
 
    #       East North Central 2                                    
    #T112(17:32,IY,IS)=QBMETCL(0,1:16,IY)+QBMBTCL(0,1:16,IY)
    z[23] = dfd['QBMETCL'].loc[1].loc[8]+dfd['QBMBTCL'].loc[1].loc[8]
 
    #       Kentucky and Tennessee (East South Cent 1)              
    #T112(17:32,IY,IS)=QBMETCL(0,1:16,IY)+QBMBTCL(0,1:16,IY)
    z[24] = dfd['QBMETCL'].loc[1].loc[9]+dfd['QBMBTCL'].loc[1].loc[9]
 
    #       Alabama and Mississippi (East South Cent 2)             
    #T112(17:32,IY,IS)=QBMETCL(0,1:16,IY)+QBMBTCL(0,1:16,IY)
    z[25] = dfd['QBMETCL'].loc[1].loc[10]+dfd['QBMBTCL'].loc[1].loc[10]
 
    #       Minnesota, Dakotas (West North Central 1)               
    #T112(17:32,IY,IS)=QBMETCL(0,1:16,IY)+QBMBTCL(0,1:16,IY)
    z[26] = dfd['QBMETCL'].loc[1].loc[11]+dfd['QBMBTCL'].loc[1].loc[11]
 
    #       West North Central 2                                    
    #T112(17:32,IY,IS)=QBMETCL(0,1:16,IY)+QBMBTCL(0,1:16,IY)
    z[27] = dfd['QBMETCL'].loc[1].loc[12]+dfd['QBMBTCL'].loc[1].loc[12]
 
    #       West South Central                                      
    #T112(17:32,IY,IS)=QBMETCL(0,1:16,IY)+QBMBTCL(0,1:16,IY)
    z[28] = dfd['QBMETCL'].loc[1].loc[13]+dfd['QBMBTCL'].loc[1].loc[13]
 
    #       Montana, Wyoming, and Idaho (Mountain 1)                
    #T112(17:32,IY,IS)=QBMETCL(0,1:16,IY)+QBMBTCL(0,1:16,IY)
    z[29] = dfd['QBMETCL'].loc[1].loc[14]+dfd['QBMBTCL'].loc[1].loc[14]
 
    #       Colorado, Utah, and Nevada (Mountain 2)                 
    #T112(17:32,IY,IS)=QBMETCL(0,1:16,IY)+QBMBTCL(0,1:16,IY)
    z[30] = dfd['QBMETCL'].loc[1].loc[15]+dfd['QBMBTCL'].loc[1].loc[15]
 
    #       Arizona and New Mexico (Mountain 3)                     
    #T112(17:32,IY,IS)=QBMETCL(0,1:16,IY)+QBMBTCL(0,1:16,IY)
    z[31] = dfd['QBMETCL'].loc[1].loc[16]+dfd['QBMBTCL'].loc[1].loc[16]
 
    #       Pacific                                                 
    #T112(17:32,IY,IS)=QBMETCL(0,1:16,IY)+QBMBTCL(0,1:16,IY)
    z[32] = dfd['QBMETCL'].loc[1].loc[17]+dfd['QBMBTCL'].loc[1].loc[17]
 
    #     Refining Total, All Types                                 
    #T112(162,IY,IS)=FSUM(T112(17,IY,IS),16)
    z[162] = z[17]+z[18]+z[19]+z[20]+z[21]+z[22]+z[23]+z[24]+z[25]+z[26]+z[27]+z[28]+z[29]+z[30]+z[31]+z[32]
 
    #   Total, All Types                                            
    #T112(163,IY,IS)=T112(161,IY,IS)+T112(162,IY,IS)
    z[163] = z[161]+z[162]
 
    #                                                               

 
    #   Urban Wood Waste                                            

    #     Electric Power                                            
 
    #       New England                                             
    #T112(33:48,IY,IS)=QBMPWCL(1,1:16,IY)
    z[33] = dfd['QBMPWCL'].loc[2].loc[2]
 
    #       Middle Atlantic                                         
    #T112(33:48,IY,IS)=QBMPWCL(1,1:16,IY)
    z[34] = dfd['QBMPWCL'].loc[2].loc[3]
 
    #       South Atlantic 1                                        
    #T112(33:48,IY,IS)=QBMPWCL(1,1:16,IY)
    z[35] = dfd['QBMPWCL'].loc[2].loc[4]
 
    #       Virginia, Carolinas (South Atlantic 2)                  
    #T112(33:48,IY,IS)=QBMPWCL(1,1:16,IY)
    z[36] = dfd['QBMPWCL'].loc[2].loc[5]
 
    #       Georgia and Florida (South Atlantic 3)                  
    #T112(33:48,IY,IS)=QBMPWCL(1,1:16,IY)
    z[37] = dfd['QBMPWCL'].loc[2].loc[6]
 
    #       Ohio (East North Central 1)                             
    #T112(33:48,IY,IS)=QBMPWCL(1,1:16,IY)
    z[38] = dfd['QBMPWCL'].loc[2].loc[7]
 
    #       East North Central 2                                    
    #T112(33:48,IY,IS)=QBMPWCL(1,1:16,IY)
    z[39] = dfd['QBMPWCL'].loc[2].loc[8]
 
    #       Kentucky and Tennessee (East South Cent 1)              
    #T112(33:48,IY,IS)=QBMPWCL(1,1:16,IY)
    z[40] = dfd['QBMPWCL'].loc[2].loc[9]
 
    #       Alabama and Mississippi (East South Cent 2)             
    #T112(33:48,IY,IS)=QBMPWCL(1,1:16,IY)
    z[41] = dfd['QBMPWCL'].loc[2].loc[10]
 
    #       Minnesota, Dakotas (West North Central 1)               
    #T112(33:48,IY,IS)=QBMPWCL(1,1:16,IY)
    z[42] = dfd['QBMPWCL'].loc[2].loc[11]
 
    #       West North Central 2                                    
    #T112(33:48,IY,IS)=QBMPWCL(1,1:16,IY)
    z[43] = dfd['QBMPWCL'].loc[2].loc[12]
 
    #       West South Central                                      
    #T112(33:48,IY,IS)=QBMPWCL(1,1:16,IY)
    z[44] = dfd['QBMPWCL'].loc[2].loc[13]
 
    #       Montana, Wyoming, and Idaho (Mountain 1)                
    #T112(33:48,IY,IS)=QBMPWCL(1,1:16,IY)
    z[45] = dfd['QBMPWCL'].loc[2].loc[14]
 
    #       Colorado, Utah, and Nevada (Mountain 2)                 
    #T112(33:48,IY,IS)=QBMPWCL(1,1:16,IY)
    z[46] = dfd['QBMPWCL'].loc[2].loc[15]
 
    #       Arizona and New Mexico (Mountain 3)                     
    #T112(33:48,IY,IS)=QBMPWCL(1,1:16,IY)
    z[47] = dfd['QBMPWCL'].loc[2].loc[16]
 
    #       Pacific                                                 
    #T112(33:48,IY,IS)=QBMPWCL(1,1:16,IY)
    z[48] = dfd['QBMPWCL'].loc[2].loc[17]
 
    #     Electric Power Total, Urban Wood Waste                    
    #T112(164,IY,IS)=FSUM(T112(33,IY,IS),16)
    z[164] = z[33]+z[34]+z[35]+z[36]+z[37]+z[38]+z[39]+z[40]+z[41]+z[42]+z[43]+z[44]+z[45]+z[46]+z[47]+z[48]
 
    #     Refining                                                  


    #       New England                                             
    #T112(49:64,IY,IS)=QBMETCL(1,1:16,IY)+QBMBTCL(1,1:16,IY)
    z[49] = dfd['QBMETCL'].loc[2].loc[2]+dfd['QBMBTCL'].loc[2].loc[2]
 
    #       Middle Atlantic                                         
    #T112(49:64,IY,IS)=QBMETCL(1,1:16,IY)+QBMBTCL(1,1:16,IY)
    z[50] = dfd['QBMETCL'].loc[2].loc[3]+dfd['QBMBTCL'].loc[2].loc[3]
 
    #       South Atlantic 1                                        
    #T112(49:64,IY,IS)=QBMETCL(1,1:16,IY)+QBMBTCL(1,1:16,IY)
    z[51] = dfd['QBMETCL'].loc[2].loc[4]+dfd['QBMBTCL'].loc[2].loc[4]
 
    #       Virginia, Carolinas (South Atlantic 2)                  
    #T112(49:64,IY,IS)=QBMETCL(1,1:16,IY)+QBMBTCL(1,1:16,IY)
    z[52] = dfd['QBMETCL'].loc[2].loc[5]+dfd['QBMBTCL'].loc[2].loc[5]
 
    #       Georgia and Florida (South Atlantic 3)                  
    #T112(49:64,IY,IS)=QBMETCL(1,1:16,IY)+QBMBTCL(1,1:16,IY)
    z[53] = dfd['QBMETCL'].loc[2].loc[6]+dfd['QBMBTCL'].loc[2].loc[6]
 
    #       Ohio (East North Central 1)                             
    #T112(49:64,IY,IS)=QBMETCL(1,1:16,IY)+QBMBTCL(1,1:16,IY)
    z[54] = dfd['QBMETCL'].loc[2].loc[7]+dfd['QBMBTCL'].loc[2].loc[7]
 
    #       East North Central 2                                    
    #T112(49:64,IY,IS)=QBMETCL(1,1:16,IY)+QBMBTCL(1,1:16,IY)
    z[55] = dfd['QBMETCL'].loc[2].loc[8]+dfd['QBMBTCL'].loc[2].loc[8]
 
    #       Kentucky and Tennessee (East South Cent 1)              
    #T112(49:64,IY,IS)=QBMETCL(1,1:16,IY)+QBMBTCL(1,1:16,IY)
    z[56] = dfd['QBMETCL'].loc[2].loc[9]+dfd['QBMBTCL'].loc[2].loc[9]
 
    #       Alabama and Mississippi (East South Cent 2)             
    #T112(49:64,IY,IS)=QBMETCL(1,1:16,IY)+QBMBTCL(1,1:16,IY)
    z[57] = dfd['QBMETCL'].loc[2].loc[10]+dfd['QBMBTCL'].loc[2].loc[10]
 
    #       Minnesota, Dakotas (West North Central 1)               
    #T112(49:64,IY,IS)=QBMETCL(1,1:16,IY)+QBMBTCL(1,1:16,IY)
    z[58] = dfd['QBMETCL'].loc[2].loc[11]+dfd['QBMBTCL'].loc[2].loc[11]
 
    #       West North Central 2                                    
    #T112(49:64,IY,IS)=QBMETCL(1,1:16,IY)+QBMBTCL(1,1:16,IY)
    z[59] = dfd['QBMETCL'].loc[2].loc[12]+dfd['QBMBTCL'].loc[2].loc[12]
 
    #       West South Central                                      
    #T112(49:64,IY,IS)=QBMETCL(1,1:16,IY)+QBMBTCL(1,1:16,IY)
    z[60] = dfd['QBMETCL'].loc[2].loc[13]+dfd['QBMBTCL'].loc[2].loc[13]
 
    #       Montana, Wyoming, and Idaho (Mountain 1)                
    #T112(49:64,IY,IS)=QBMETCL(1,1:16,IY)+QBMBTCL(1,1:16,IY)
    z[61] = dfd['QBMETCL'].loc[2].loc[14]+dfd['QBMBTCL'].loc[2].loc[14]
 
    #       Colorado, Utah, and Nevada (Mountain 2)                 
    #T112(49:64,IY,IS)=QBMETCL(1,1:16,IY)+QBMBTCL(1,1:16,IY)
    z[62] = dfd['QBMETCL'].loc[2].loc[15]+dfd['QBMBTCL'].loc[2].loc[15]
 
    #       Arizona and New Mexico (Mountain 3)                     
    #T112(49:64,IY,IS)=QBMETCL(1,1:16,IY)+QBMBTCL(1,1:16,IY)
    z[63] = dfd['QBMETCL'].loc[2].loc[16]+dfd['QBMBTCL'].loc[2].loc[16]
 
    #       Pacific                                                 
    #T112(49:64,IY,IS)=QBMETCL(1,1:16,IY)+QBMBTCL(1,1:16,IY)
    z[64] = dfd['QBMETCL'].loc[2].loc[17]+dfd['QBMBTCL'].loc[2].loc[17]
 
    #     Refining Total, Urban Wood Waste                          
    #T112(165,IY,IS)=FSUM(T112(49,IY,IS),16)
    z[165] = z[49]+z[50]+z[51]+z[52]+z[53]+z[54]+z[55]+z[56]+z[57]+z[58]+z[59]+z[60]+z[61]+z[62]+z[63]+z[64]
 
    #   Total, Urban Wood Waste                                     
    #T112(166,IY,IS)=T112(164,IY,IS)+T112(165,IY,IS)
    z[166] = z[164]+z[165]
 
    #                                                               

 
    #   Forestry Residue, Public Lands                              

 
    #     Electric Power                                            
 
    #       New England                                             
    #T112(65:80,IY,IS)=QBMPWCL(2,1:16,IY)
    z[65] = dfd['QBMPWCL'].loc[3].loc[2]
 
    #       Middle Atlantic                                         
    #T112(65:80,IY,IS)=QBMPWCL(2,1:16,IY)
    z[66] = dfd['QBMPWCL'].loc[3].loc[3]
 
    #       South Atlantic 1                                        
    #T112(65:80,IY,IS)=QBMPWCL(2,1:16,IY)
    z[67] = dfd['QBMPWCL'].loc[3].loc[4]
 
    #       Virginia, Carolinas (South Atlantic 2)                  
    #T112(65:80,IY,IS)=QBMPWCL(2,1:16,IY)
    z[68] = dfd['QBMPWCL'].loc[3].loc[5]
 
    #       Georgia and Florida (South Atlantic 3)                  
    #T112(65:80,IY,IS)=QBMPWCL(2,1:16,IY)
    z[69] = dfd['QBMPWCL'].loc[3].loc[6]
 
    #       Ohio (East North Central 1)                             
    #T112(65:80,IY,IS)=QBMPWCL(2,1:16,IY)
    z[70] = dfd['QBMPWCL'].loc[3].loc[7]
 
    #       East North Central 2                                    
    #T112(65:80,IY,IS)=QBMPWCL(2,1:16,IY)
    z[71] = dfd['QBMPWCL'].loc[3].loc[8]
 
    #       Kentucky and Tennessee (East South Cent 1)              
    #T112(65:80,IY,IS)=QBMPWCL(2,1:16,IY)
    z[72] = dfd['QBMPWCL'].loc[3].loc[9]
 
    #       Alabama and Mississippi (East South Cent 2)             
    #T112(65:80,IY,IS)=QBMPWCL(2,1:16,IY)
    z[73] = dfd['QBMPWCL'].loc[3].loc[10]
 
    #       Minnesota, Dakotas (West North Central 1)               
    #T112(65:80,IY,IS)=QBMPWCL(2,1:16,IY)
    z[74] = dfd['QBMPWCL'].loc[3].loc[11]
 
    #       West North Central 2                                    
    #T112(65:80,IY,IS)=QBMPWCL(2,1:16,IY)
    z[75] = dfd['QBMPWCL'].loc[3].loc[12]
 
    #       West South Central                                      
    #T112(65:80,IY,IS)=QBMPWCL(2,1:16,IY)
    z[76] = dfd['QBMPWCL'].loc[3].loc[13]
 
    #       Montana, Wyoming, and Idaho (Mountain 1)                
    #T112(65:80,IY,IS)=QBMPWCL(2,1:16,IY)
    z[77] = dfd['QBMPWCL'].loc[3].loc[14]
 
    #       Colorado, Utah, and Nevada (Mountain 2)                 
    #T112(65:80,IY,IS)=QBMPWCL(2,1:16,IY)
    z[78] = dfd['QBMPWCL'].loc[3].loc[15]
 
    #       Arizona and New Mexico (Mountain 3)                     
    #T112(65:80,IY,IS)=QBMPWCL(2,1:16,IY)
    z[79] = dfd['QBMPWCL'].loc[3].loc[16]
 
    #       Pacific                                                 
    #T112(65:80,IY,IS)=QBMPWCL(2,1:16,IY)
    z[80] = dfd['QBMPWCL'].loc[3].loc[17]
 
    #     Electric Power Total, Public Forestry Residue             
    #T112(167,IY,IS)=FSUM(T112(65,IY,IS),16)
    z[167] = z[65]+z[66]+z[67]+z[68]+z[69]+z[70]+z[71]+z[72]+z[73]+z[74]+z[75]+z[76]+z[77]+z[78]+z[79]+z[80]
 
    #     Refining                                                  

 
    #       New England                                             
    #T112(81:96,IY,IS)=QBMETCL(2,1:16,IY)+QBMBTCL(2,1:16,IY)
    z[81] = dfd['QBMETCL'].loc[3].loc[1]+dfd['QBMBTCL'].loc[3].loc[1]
 
    #       Middle Atlantic                                         
    #T112(81:96,IY,IS)=QBMETCL(2,1:16,IY)+QBMBTCL(2,1:16,IY)
    z[82] = dfd['QBMETCL'].loc[3].loc[2]+dfd['QBMBTCL'].loc[3].loc[2]
 
    #       South Atlantic 1                                        
    #T112(81:96,IY,IS)=QBMETCL(2,1:16,IY)+QBMBTCL(2,1:16,IY)
    z[83] = dfd['QBMETCL'].loc[3].loc[3]+dfd['QBMBTCL'].loc[3].loc[3]
 
    #       Virginia, Carolinas (South Atlantic 2)                  
    #T112(81:96,IY,IS)=QBMETCL(2,1:16,IY)+QBMBTCL(2,1:16,IY)
    z[84] = dfd['QBMETCL'].loc[3].loc[4]+dfd['QBMBTCL'].loc[3].loc[4]
 
    #       Georgia and Florida (South Atlantic 3)                  
    #T112(81:96,IY,IS)=QBMETCL(2,1:16,IY)+QBMBTCL(2,1:16,IY)
    z[85] = dfd['QBMETCL'].loc[3].loc[5]+dfd['QBMBTCL'].loc[3].loc[5]
 
    #       Ohio (East North Central 1)                             
    #T112(81:96,IY,IS)=QBMETCL(2,1:16,IY)+QBMBTCL(2,1:16,IY)
    z[86] = dfd['QBMETCL'].loc[3].loc[6]+dfd['QBMBTCL'].loc[3].loc[6]
 
    #       East North Central 2                                    
    #T112(81:96,IY,IS)=QBMETCL(2,1:16,IY)+QBMBTCL(2,1:16,IY)
    z[87] = dfd['QBMETCL'].loc[3].loc[7]+dfd['QBMBTCL'].loc[3].loc[7]
 
    #       Kentucky and Tennessee (East South Cent 1)              
    #T112(81:96,IY,IS)=QBMETCL(2,1:16,IY)+QBMBTCL(2,1:16,IY)
    z[88] = dfd['QBMETCL'].loc[3].loc[8]+dfd['QBMBTCL'].loc[3].loc[8]
 
    #       Alabama and Mississippi (East South Cent 2)             
    #T112(81:96,IY,IS)=QBMETCL(2,1:16,IY)+QBMBTCL(2,1:16,IY)
    z[89] = dfd['QBMETCL'].loc[3].loc[9]+dfd['QBMBTCL'].loc[3].loc[9]
 
    #       Minnesota, Dakotas (West North Central 1)               
    #T112(81:96,IY,IS)=QBMETCL(2,1:16,IY)+QBMBTCL(2,1:16,IY)
    z[90] = dfd['QBMETCL'].loc[3].loc[10]+dfd['QBMBTCL'].loc[3].loc[10]
 
    #       West North Central 2                                    
    #T112(81:96,IY,IS)=QBMETCL(2,1:16,IY)+QBMBTCL(2,1:16,IY)
    z[91] = dfd['QBMETCL'].loc[3].loc[11]+dfd['QBMBTCL'].loc[3].loc[11]
 
    #       West South Central                                      
    #T112(81:96,IY,IS)=QBMETCL(2,1:16,IY)+QBMBTCL(2,1:16,IY)
    z[92] = dfd['QBMETCL'].loc[3].loc[12]+dfd['QBMBTCL'].loc[3].loc[12]
 
    #       Montana, Wyoming, and Idaho (Mountain 1)                
    #T112(81:96,IY,IS)=QBMETCL(2,1:16,IY)+QBMBTCL(2,1:16,IY)
    z[93] = dfd['QBMETCL'].loc[3].loc[13]+dfd['QBMBTCL'].loc[3].loc[13]
 
    #       Colorado, Utah, and Nevada (Mountain 2)                 
    #T112(81:96,IY,IS)=QBMETCL(2,1:16,IY)+QBMBTCL(2,1:16,IY)
    z[94] = dfd['QBMETCL'].loc[3].loc[14]+dfd['QBMBTCL'].loc[3].loc[14]
 
    #       Arizona and New Mexico (Mountain 3)                     
    #T112(81:96,IY,IS)=QBMETCL(2,1:16,IY)+QBMBTCL(2,1:16,IY)
    z[95] = dfd['QBMETCL'].loc[3].loc[15]+dfd['QBMBTCL'].loc[3].loc[15]
 
    #       Pacific                                                 
    #T112(81:96,IY,IS)=QBMETCL(2,1:16,IY)+QBMBTCL(2,1:16,IY)
    z[96] = dfd['QBMETCL'].loc[3].loc[16]+dfd['QBMBTCL'].loc[3].loc[16]
 
    #     Refining Total, Public Forestry Residue                   
    #T112(168,IY,IS)=FSUM(T112(81,IY,IS),16)
    z[168] = z[81]+z[82]+z[83]+z[84]+z[85]+z[86]+z[87]+z[88]+z[89]+z[90]+z[91]+z[92]+z[93]+z[94]+z[95]+z[96]
 
    #   Total, Forestry Residue, Public Lands                       
    #T112(169,IY,IS)=T112(167,IY,IS)+T112(168,IY,IS)
    z[169] = z[167]+z[168]
 
    #                                                               


 
    #   Agriculture Residue and Energy Crops                        


 
    #     Electric Power                                            


 
    #       New England                                             
    #T112(97:112,IY,IS)=QBMPWCL(3,1:16,IY)
    z[97] = dfd['QBMPWCL'].loc[4].loc[2]
 
    #       Middle Atlantic                                         
    #T112(97:112,IY,IS)=QBMPWCL(3,1:16,IY)
    z[98] = dfd['QBMPWCL'].loc[4].loc[3]
 
    #       South Atlantic 1                                        
    #T112(97:112,IY,IS)=QBMPWCL(3,1:16,IY)
    z[99] = dfd['QBMPWCL'].loc[4].loc[4]
 
    #       Virginia, Carolinas (South Atlantic 2)                  
    #T112(97:112,IY,IS)=QBMPWCL(3,1:16,IY)
    z[100] = dfd['QBMPWCL'].loc[4].loc[5]
 
    #       Georgia and Florida (South Atlantic 3)                  
    #T112(97:112,IY,IS)=QBMPWCL(3,1:16,IY)
    z[101] = dfd['QBMPWCL'].loc[4].loc[6]
 
    #       Ohio (East North Central 1)                             
    #T112(97:112,IY,IS)=QBMPWCL(3,1:16,IY)
    z[102] = dfd['QBMPWCL'].loc[4].loc[7]
 
    #       East North Central 2                                    
    #T112(97:112,IY,IS)=QBMPWCL(3,1:16,IY)
    z[103] = dfd['QBMPWCL'].loc[4].loc[8]
 
    #       Kentucky and Tennessee (East South Cent 1)              
    #T112(97:112,IY,IS)=QBMPWCL(3,1:16,IY)
    z[104] = dfd['QBMPWCL'].loc[4].loc[9]
 
    #       Alabama and Mississippi (East South Cent 2)             
    #T112(97:112,IY,IS)=QBMPWCL(3,1:16,IY)
    z[105] = dfd['QBMPWCL'].loc[4].loc[10]
 
    #       Minnesota, Dakotas (West North Central 1)               
    #T112(97:112,IY,IS)=QBMPWCL(3,1:16,IY)
    z[106] = dfd['QBMPWCL'].loc[4].loc[11]
 
    #       West North Central 2                                    
    #T112(97:112,IY,IS)=QBMPWCL(3,1:16,IY)
    z[107] = dfd['QBMPWCL'].loc[4].loc[12]
 
    #       West South Central                                      
    #T112(97:112,IY,IS)=QBMPWCL(3,1:16,IY)
    z[108] = dfd['QBMPWCL'].loc[4].loc[13]
 
    #       Montana, Wyoming, and Idaho (Mountain 1)                
    #T112(97:112,IY,IS)=QBMPWCL(3,1:16,IY)
    z[109] = dfd['QBMPWCL'].loc[4].loc[14]
 
    #       Colorado, Utah, and Nevada (Mountain 2)                 
    #T112(97:112,IY,IS)=QBMPWCL(3,1:16,IY)
    z[110] = dfd['QBMPWCL'].loc[4].loc[15]
 
    #       Arizona and New Mexico (Mountain 3)                     
    #T112(97:112,IY,IS)=QBMPWCL(3,1:16,IY)
    z[111] = dfd['QBMPWCL'].loc[4].loc[16]
 
    #       Pacific                                                 
    #T112(97:112,IY,IS)=QBMPWCL(3,1:16,IY)
    z[112] = dfd['QBMPWCL'].loc[4].loc[17]
 
    #     Electric Power Total, Energy Crops                        
    #T112(170,IY,IS)=FSUM(T112(97,IY,IS),16)
    z[170] = z[97]+z[98]+z[99]+z[100]+z[101]+z[102]+z[103]+z[104]+z[105]+z[106]+z[107]+z[108]+z[109]+z[110]+z[111]+z[112]

    #     Refining                                                  

 
    #       New England                                             
    #T112(113:128,IY,IS)=QBMETCL(3,1:16,IY)+QBMBTCL(3,1:16,IY)
    
 
    #       Middle Atlantic                                         
    #T112(113:128,IY,IS)=QBMETCL(3,1:16,IY)+QBMBTCL(3,1:16,IY)
    z[113] = dfd['QBMETCL'].loc[4].loc[2]+dfd['QBMBTCL'].loc[4].loc[2]
 
    #       South Atlantic 1                                        
    #T112(113:128,IY,IS)=QBMETCL(3,1:16,IY)+QBMBTCL(3,1:16,IY)
    z[114] = dfd['QBMETCL'].loc[4].loc[3]+dfd['QBMBTCL'].loc[4].loc[3]
 
    #       Virginia, Carolinas (South Atlantic 2)                  
    #T112(113:128,IY,IS)=QBMETCL(3,1:16,IY)+QBMBTCL(3,1:16,IY)
    z[115] = dfd['QBMETCL'].loc[4].loc[4]+dfd['QBMBTCL'].loc[4].loc[4]
 
    #       Georgia and Florida (South Atlantic 3)                  
    #T112(113:128,IY,IS)=QBMETCL(3,1:16,IY)+QBMBTCL(3,1:16,IY)
    z[116] = dfd['QBMETCL'].loc[4].loc[5]+dfd['QBMBTCL'].loc[4].loc[5]
 
    #       Ohio (East North Central 1)                             
    #T112(113:128,IY,IS)=QBMETCL(3,1:16,IY)+QBMBTCL(3,1:16,IY)
    z[117] = dfd['QBMETCL'].loc[4].loc[6]+dfd['QBMBTCL'].loc[4].loc[6]
 
    #       East North Central 2                                    
    #T112(113:128,IY,IS)=QBMETCL(3,1:16,IY)+QBMBTCL(3,1:16,IY)
    z[118] = dfd['QBMETCL'].loc[4].loc[7]+dfd['QBMBTCL'].loc[4].loc[7]
 
    #       Kentucky and Tennessee (East South Cent 1)              
    #T112(113:128,IY,IS)=QBMETCL(3,1:16,IY)+QBMBTCL(3,1:16,IY)
    z[119] = dfd['QBMETCL'].loc[4].loc[8]+dfd['QBMBTCL'].loc[4].loc[8]
 
    #       Alabama and Mississippi (East South Cent 2)             
    #T112(113:128,IY,IS)=QBMETCL(3,1:16,IY)+QBMBTCL(3,1:16,IY)
    z[120] = dfd['QBMETCL'].loc[4].loc[9]+dfd['QBMBTCL'].loc[4].loc[9]
 
    #       Minnesota, Dakotas (West North Central 1)               
    #T112(113:128,IY,IS)=QBMETCL(3,1:16,IY)+QBMBTCL(3,1:16,IY)
    z[121] = dfd['QBMETCL'].loc[4].loc[10]+dfd['QBMBTCL'].loc[4].loc[10]
 
    #       West North Central 2                                    
    #T112(113:128,IY,IS)=QBMETCL(3,1:16,IY)+QBMBTCL(3,1:16,IY)
    z[122] = dfd['QBMETCL'].loc[4].loc[11]+dfd['QBMBTCL'].loc[4].loc[11]
 
    #       West South Central                                      
    #T112(113:128,IY,IS)=QBMETCL(3,1:16,IY)+QBMBTCL(3,1:16,IY)
    z[123] = dfd['QBMETCL'].loc[4].loc[12]+dfd['QBMBTCL'].loc[4].loc[12]
 
    #       Montana, Wyoming, and Idaho (Mountain 1)                
    #T112(113:128,IY,IS)=QBMETCL(3,1:16,IY)+QBMBTCL(3,1:16,IY)
    z[124] = dfd['QBMETCL'].loc[4].loc[13]+dfd['QBMBTCL'].loc[4].loc[13]
 
    #       Colorado, Utah, and Nevada (Mountain 2)                 
    #T112(113:128,IY,IS)=QBMETCL(3,1:16,IY)+QBMBTCL(3,1:16,IY)
    z[125] = dfd['QBMETCL'].loc[4].loc[14]+dfd['QBMBTCL'].loc[4].loc[14]
 
    #       Arizona and New Mexico (Mountain 3)                     
    #T112(113:128,IY,IS)=QBMETCL(3,1:16,IY)+QBMBTCL(3,1:16,IY)
    z[126] = dfd['QBMETCL'].loc[4].loc[15]+dfd['QBMBTCL'].loc[4].loc[15]
 
    #       Pacific                                                 
    #T112(113:128,IY,IS)=QBMETCL(3,1:16,IY)+QBMBTCL(3,1:16,IY)
    z[127] = dfd['QBMETCL'].loc[4].loc[16]+dfd['QBMBTCL'].loc[4].loc[16]
    z[128] = dfd['QBMETCL'].loc[4].loc[17]+dfd['QBMBTCL'].loc[4].loc[17]
 
    #     Refining Total, Energy Crops                              
    #T112(171,IY,IS)=FSUM(T112(113,IY,IS),16)
    z[171] = z[113]+z[114]+z[115]+z[116]+z[117]+z[118]+z[119]+z[120]+z[121]+z[122]+z[123]+z[124]+z[125]+z[126]+z[127]+z[128]
 
    #   Total, Agriculture and Energy Crops                         
    #T112(172,IY,IS)=T112(170,IY,IS)+T112(171,IY,IS)
    z[172] = z[170]+z[171]
 
    #                                                               

 
    #   Forestry Residue, Private Lands                             


    #     Electric Power                                            
 
    #       New England                                             
    #T112(129:144,IY,IS)=QBMPWCL(4,1:16,IY)
    z[129] = dfd['QBMPWCL'].loc[5].loc[2]

    #       Middle Atlantic                                         
    #T112(129:144,IY,IS)=QBMPWCL(4,1:16,IY)
    z[130] = dfd['QBMPWCL'].loc[5].loc[3]
 
    #       South Atlantic 1                                        
    #T112(129:144,IY,IS)=QBMPWCL(4,1:16,IY)
    z[131] = dfd['QBMPWCL'].loc[5].loc[4]
 
    #       Virginia, Carolinas (South Atlantic 2)                  
    #T112(129:144,IY,IS)=QBMPWCL(4,1:16,IY)
    z[132] = dfd['QBMPWCL'].loc[5].loc[5]
 
    #       Georgia and Florida (South Atlantic 3)                  
    #T112(129:144,IY,IS)=QBMPWCL(4,1:16,IY)
    z[133] = dfd['QBMPWCL'].loc[5].loc[6]
 
    #       Ohio (East North Central 1)                             
    #T112(129:144,IY,IS)=QBMPWCL(4,1:16,IY)
    z[134] = dfd['QBMPWCL'].loc[5].loc[7]
 
    #       East North Central 2                                    
    #T112(129:144,IY,IS)=QBMPWCL(4,1:16,IY)
    z[135] = dfd['QBMPWCL'].loc[5].loc[8]
 
    #       Kentucky and Tennessee (East South Cent 1)              
    #T112(129:144,IY,IS)=QBMPWCL(4,1:16,IY)
    z[136] = dfd['QBMPWCL'].loc[5].loc[9]
 
    #       Alabama and Mississippi (East South Cent 2)             
    #T112(129:144,IY,IS)=QBMPWCL(4,1:16,IY)
    z[137] = dfd['QBMPWCL'].loc[5].loc[10]
 
    #       Minnesota, Dakotas (West North Central 1)               
    #T112(129:144,IY,IS)=QBMPWCL(4,1:16,IY)
    z[138] = dfd['QBMPWCL'].loc[5].loc[11]
 
    #       West North Central 2                                    
    #T112(129:144,IY,IS)=QBMPWCL(4,1:16,IY)
    z[139] = dfd['QBMPWCL'].loc[5].loc[12]
 
    #       West South Central                                      
    #T112(129:144,IY,IS)=QBMPWCL(4,1:16,IY)
    z[140] = dfd['QBMPWCL'].loc[5].loc[13]
 
    #       Montana, Wyoming, and Idaho (Mountain 1)                
    #T112(129:144,IY,IS)=QBMPWCL(4,1:16,IY)
    z[141] = dfd['QBMPWCL'].loc[5].loc[14]
 
    #       Colorado, Utah, and Nevada (Mountain 2)                 
    #T112(129:144,IY,IS)=QBMPWCL(4,1:16,IY)
    z[142] = dfd['QBMPWCL'].loc[5].loc[15]
 
    #       Arizona and New Mexico (Mountain 3)                     
    #T112(129:144,IY,IS)=QBMPWCL(4,1:16,IY)
    z[143] = dfd['QBMPWCL'].loc[5].loc[16]
 
    #       Pacific                                                 
    #T112(129:144,IY,IS)=QBMPWCL(4,1:16,IY)
    z[144] = dfd['QBMPWCL'].loc[5].loc[17]
 
    #     Electric Power Total, Private Forestry Residue            
    #T112(173,IY,IS)=FSUM(T112(129,IY,IS),16)
    z[173] = z[129]+z[130]+z[131]+z[132]+z[133]+z[134]+z[135]+z[136]+z[137]+z[138]+z[139]+z[140]+z[141]+z[142]+z[143]+z[144]
 
    #     Refining                                                  

 
    #       New England                                             
    #T112(145:160,IY,IS)=QBMETCL(4,1:16,IY)+QBMBTCL(4,1:16,IY)
    
    #       Middle Atlantic                                         
    #T112(145:160,IY,IS)=QBMETCL(4,1:16,IY)+QBMBTCL(4,1:16,IY)
    z[145] = dfd['QBMETCL'].loc[5].loc[2]+dfd['QBMBTCL'].loc[5].loc[2]
 
    #       South Atlantic 1                                        
    #T112(145:160,IY,IS)=QBMETCL(4,1:16,IY)+QBMBTCL(4,1:16,IY)
    z[146] = dfd['QBMETCL'].loc[5].loc[3]+dfd['QBMBTCL'].loc[5].loc[3]
 
    #       Virginia, Carolinas (South Atlantic 2)                  
    #T112(145:160,IY,IS)=QBMETCL(4,1:16,IY)+QBMBTCL(4,1:16,IY)
    z[147] = dfd['QBMETCL'].loc[5].loc[4]+dfd['QBMBTCL'].loc[5].loc[4]
 
    #       Georgia and Florida (South Atlantic 3)                  
    #T112(145:160,IY,IS)=QBMETCL(4,1:16,IY)+QBMBTCL(4,1:16,IY)
    z[148] = dfd['QBMETCL'].loc[5].loc[5]+dfd['QBMBTCL'].loc[5].loc[5]
 
    #       Ohio (East North Central 1)                             
    #T112(145:160,IY,IS)=QBMETCL(4,1:16,IY)+QBMBTCL(4,1:16,IY)
    z[149] = dfd['QBMETCL'].loc[5].loc[6]+dfd['QBMBTCL'].loc[5].loc[6]
 
    #       East North Central 2                                    
    #T112(145:160,IY,IS)=QBMETCL(4,1:16,IY)+QBMBTCL(4,1:16,IY)
    z[150] = dfd['QBMETCL'].loc[5].loc[7]+dfd['QBMBTCL'].loc[5].loc[7]
 
    #       Kentucky and Tennessee (East South Cent 1)              
    #T112(145:160,IY,IS)=QBMETCL(4,1:16,IY)+QBMBTCL(4,1:16,IY)
    z[151] = dfd['QBMETCL'].loc[5].loc[8]+dfd['QBMBTCL'].loc[5].loc[8]
 
    #       Alabama and Mississippi (East South Cent 2)             
    #T112(145:160,IY,IS)=QBMETCL(4,1:16,IY)+QBMBTCL(4,1:16,IY)
    z[152] = dfd['QBMETCL'].loc[5].loc[9]+dfd['QBMBTCL'].loc[5].loc[9]
 
    #       Minnesota, Dakotas (West North Central 1)               
    #T112(145:160,IY,IS)=QBMETCL(4,1:16,IY)+QBMBTCL(4,1:16,IY)
    z[153] = dfd['QBMETCL'].loc[5].loc[10]+dfd['QBMBTCL'].loc[5].loc[10]
 
    #       West North Central 2                                    
    #T112(145:160,IY,IS)=QBMETCL(4,1:16,IY)+QBMBTCL(4,1:16,IY)
    z[154] = dfd['QBMETCL'].loc[5].loc[11]+dfd['QBMBTCL'].loc[5].loc[11]
 
    #       West South Central                                      
    #T112(145:160,IY,IS)=QBMETCL(4,1:16,IY)+QBMBTCL(4,1:16,IY)
    z[155] = dfd['QBMETCL'].loc[5].loc[12]+dfd['QBMBTCL'].loc[5].loc[12]
 
    #       Montana, Wyoming, and Idaho (Mountain 1)                
    #T112(145:160,IY,IS)=QBMETCL(4,1:16,IY)+QBMBTCL(4,1:16,IY)
    z[156] = dfd['QBMETCL'].loc[5].loc[13]+dfd['QBMBTCL'].loc[5].loc[13]
 
    #       Colorado, Utah, and Nevada (Mountain 2)                 
    #T112(145:160,IY,IS)=QBMETCL(4,1:16,IY)+QBMBTCL(4,1:16,IY)
    z[157] = dfd['QBMETCL'].loc[5].loc[14]+dfd['QBMBTCL'].loc[5].loc[14]
 
    #       Arizona and New Mexico (Mountain 3)                     
    #T112(145:160,IY,IS)=QBMETCL(4,1:16,IY)+QBMBTCL(4,1:16,IY)
    z[158] = dfd['QBMETCL'].loc[5].loc[15]+dfd['QBMBTCL'].loc[5].loc[15]
 
    #       Pacific                                                 
    #T112(145:160,IY,IS)=QBMETCL(4,1:16,IY)+QBMBTCL(4,1:16,IY)
    z[159] = dfd['QBMETCL'].loc[5].loc[16]+dfd['QBMBTCL'].loc[5].loc[16]
    z[160] = dfd['QBMETCL'].loc[5].loc[17]+dfd['QBMBTCL'].loc[5].loc[17]
 
    #     Refining Total, Private Forestry Residue                  
    #T112(174,IY,IS)=FSUM(T112(145,IY,IS),16)
    z[174] = z[145]+z[146]+z[147]+z[148]+z[149]+z[150]+z[151]+z[152]+z[153]+z[154]+z[155]+z[156]+z[157]+z[158]+z[159]+z[160]
 
    #   Total, Forestry Residue, Private Lands                      
    #T112(175,IY,IS)=T112(173,IY,IS)+T112(174,IY,IS)
    z[175] = z[173].fillna(0)+z[174].fillna(0)
 
    return z
 
 
 
 
 

