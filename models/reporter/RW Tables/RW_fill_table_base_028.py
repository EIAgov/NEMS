# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_028(dfd, table_spec, table_id):
    """Fill table   Non-Utility Fuel Consumption
   
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
    
    MNUMNR=dfd['MNUMNR_rwpre']



    #   Non-Utility Fuel Consumption
    #   (trillion Btu, unless otherwise noted)
    #    Sector and Fuel                                            

    #   Consumption by Fuel Type                                    
    #    Independent Power Producers                                
 
    #      Coal                                                     
    #T28(1,IY,IS)=UFLCLNR(2,MNUMNR,IY)
    z[1] = dfd['UFLCLNR'].loc[2].loc[MNUMNR]
 
    #      Petroleum                                                
    #T28(2,IY,IS)=UFLDSNR(2,MNUMNR,IY)+UFLRLNR(2,MNUMNR,IY)
    z[2] = dfd['UFLDSNR'].loc[2].loc[MNUMNR] + dfd['UFLRLNR'].loc[2].loc[MNUMNR]
 
    #      Natural Gas                                              
    #T28(3,IY,IS)=UFLGFNR(2,MNUMNR,IY)+UFLGINR(2,MNUMNR,IY)+UFLGCNR(2,MNUMNR,IY)
    z[3] = dfd['UFLGFNR'].loc[2].loc[MNUMNR] + dfd['UFLGINR'].loc[2].loc[MNUMNR] + dfd['UFLGCNR'].loc[2].loc[MNUMNR]
 
    #      Nuclear / Uranium                                        
    #T28(4,IY,IS)=UFLURNR(2,MNUMNR,IY)
    z[4] = dfd['UFLURNR'].loc[2].loc[MNUMNR]
 
    #      Renewable Sources                                        
    #T28(5,IY,IS)=(UGNHYNR(2,MNUMNR,IY)*WHRHYEL(MNUMNR,IY)+UGNMSNR(2,MNUMNR,IY)*WHRMSEL(MNUMNR,IY)+UGNSONR(2,MNUMNR,IY)*WHRSTEL(MNUMNR,IY)+UGNPVNR(2,MNUMNR,IY)*WHRPVEL(MNUMNR,IY)+UGNPTNR(2,MNUMNR,IY)*WHRPTEL(MNUMNR,IY)+UGNWNNR(2,MNUMNR,IY)*WHRWIEL(MNUMNR,IY)+UGNWLNR(2,MNUMNR,IY)*WHRWLEL(MNUMNR,IY])/1000000.+UFLGTNR(2,MNUMNR,IY)+UFLWDNR(2,MNUMNR,IY)
    z[5] = (dfd['UGNHYNR'].loc[2].loc[MNUMNR] * dfd['WHRHYEL'].loc[MNUMNR] + dfd['UGNMSNR'].loc[2].loc[MNUMNR] * dfd['WHRMSEL'].loc[MNUMNR] + dfd['UGNSONR'].loc[2].loc[MNUMNR] * dfd['WHRSTEL'].loc[MNUMNR] + dfd['UGNPVNR'].loc[2].loc[MNUMNR] * dfd['WHRPVEL'].loc[MNUMNR] + dfd['UGNPTNR'].loc[2].loc[MNUMNR] * dfd['WHRPTEL'].loc[MNUMNR] + dfd['UGNWNNR'].loc[2].loc[MNUMNR] * dfd['WHRWIEL'].loc[MNUMNR] + dfd['UGNWLNR'].loc[2].loc[MNUMNR] * dfd['WHRWLEL'].loc[MNUMNR]) / 1000000. + dfd['UFLGTNR'].loc[2].loc[MNUMNR] + dfd['UFLWDNR'].loc[2].loc[MNUMNR]
 
    #      Other                                                    
    #T28(6,IY,IS)=UFLOTNR(2,MNUMNR,IY)
    z[6] = dfd['UFLOTNR'].loc[2].loc[MNUMNR]
 
    #        Total                                                  
    #T28(12,IY,IS)=FSUM(T28(1,IY,IS),6)
    z[12] = z[1]+z[2]+z[3]+z[4]+z[5]+z[6]
 
    #                                                               
 
    #    Combined Heat and Power                                    
 
    #     Industrial                                                
 
    #      Coal                                                     
    #T28(13,IY,IS)=CGINDLQ(11,IY,1)
    z[13] = dfd['CGINDLQ'].loc[11].loc[1] 
    #      Petroleum                                                
    #T28(14,IY,IS)=CGINDLQ(11,IY,2)
    z[14] = dfd['CGINDLQ'].loc[11].loc[2]
 
    #      Natural Gas                                              
    #T28(15,IY,IS)=CGINDLQ(11,IY,3)
    z[15] = dfd['CGINDLQ'].loc[11].loc[3]
 
    #      Renewable Sources                                        
    #T28(16,IY,IS)=CGINDLQ(11,IY,6)+CGINDLQ(11,IY,7)
    z[16] = dfd['CGINDLQ'].loc[11].loc[6] + dfd['CGINDLQ'].loc[11].loc[7]
 
    #      Other                                                    
    #T28(17,IY,IS)=CGINDLQ(11,IY,9)+CGINDLQ(11,IY,10)
    z[17] = dfd['CGINDLQ'].loc[11].loc[9] + dfd['CGINDLQ'].loc[11].loc[10]
 
    #        Total                                                  
    #T28(18,IY,IS)=FSUM(T28(13,IY,IS),5)
    z[18] = z[13]+z[14]+z[15]+z[16]+z[17]
 
    #      Other Industrial Generators                              
    #T28(55,IY,IS)=CGINDLQ(11,IY,4)+CGINDLQ(11,IY,5)+CGINDLQ(11,IY,8)+CGINDLQ(11,IY,11)
    z[55] = dfd['CGINDLQ'].loc[11].loc[4] + dfd['CGINDLQ'].loc[11].loc[5] + dfd['CGINDLQ'].loc[11].loc[8] + dfd['CGINDLQ'].loc[11].loc[11]
 
    #                                                               
 
    #     Refining                                                  
 
    #      Coal                                                     
    #T28(23,IY,IS)=CGREFQ(11,IY,1)
    z[23] = dfd['CGREFQ'].loc[11].loc[1]
 
    #      Other Gaseous Fuels                                      
    #T28(19,IY,IS)=CGREFQ(11,IY,9)
    z[19] = dfd['CGREFQ'].loc[11].loc[9]
 
    #      Petroleum                                                
    #T28(20,IY,IS)=CGREFQ(11,IY,2)
    z[20] = dfd['CGREFQ'].loc[11].loc[2]
 
    #      Natural Gas                                              
    #T28(21,IY,IS)=CGREFQ(11,IY,3)
    z[21] = dfd['CGREFQ'].loc[11].loc[3]
 
    #      Renewable                                                
    #T28(22,IY,IS)=CGREFQ(11,IY,6)+CGREFQ(11,IY,7)
    z[22] = dfd['CGREFQ'].loc[11].loc[6] + dfd['CGREFQ'].loc[11].loc[7]
 
    #        Total                                                  
    #T28(24,IY,IS)=FSUM(T28(19,IY,IS),5)
    z[24] = z[19]+z[20]+z[21]+z[22]+z[23]
 
    #                                                               
 
    #    Enhanced Oil Recovery (EOR)                                

    #      Coal                                                     
    #T28(25,IY,IS)=CGOGSQ(11,IY,1)
    z[25] = dfd['CGOGSQ'].loc[11].loc[1]
 
    #      Petroleum                                                
    #T28(26,IY,IS)=CGOGSQ(11,IY,2)
    z[26] = dfd['CGOGSQ'].loc[11].loc[2]
 
    #      Natural Gas                                              
    #T28(27,IY,IS)=CGOGSQ(11,IY,3)
    z[27] = dfd['CGOGSQ'].loc[11].loc[3]
 
    #      Other                                                    
    #T28(28,IY,IS)=CGOGSQ(11,IY,4)
    z[28] = dfd['CGOGSQ'].loc[11].loc[4]
 
    #        Total                                                  
    #T28(29,IY,IS)=FSUM(T28(25,IY,IS),4)
    z[29] = z[25]+z[26]+z[27]+z[28]

    #                                                               
 
    #    Residential and Commercial                                 
 
    #      Coal                                                     
    #T28(30,IY,IS)=(CGCOMMQ(11,IY,1)+CGRESQ(11,IY,1))
    z[30] = (dfd['CGCOMMQ'].loc[11].loc[1] + dfd['CGRESQ'].loc[11].loc[1])
 
    #      Petroleum                                                
    #T28(31,IY,IS)=(CGCOMMQ(11,IY,2)+CGRESQ(11,IY,2))
    z[31] = (dfd['CGCOMMQ'].loc[11].loc[2] + dfd['CGRESQ'].loc[11].loc[2])
 
    #      Natural Gas                                              
    #T28(32,IY,IS)=(CGCOMMQ(11,IY,3)+CGRESQ(11,IY,3))
    z[32] = (dfd['CGCOMMQ'].loc[11].loc[3] + dfd['CGRESQ'].loc[11].loc[3])
 
    #      Renewable Sources                                        
    #T28(33,IY,IS)=(CGCOMMQ(11,IY,6)+CGCOMMQ(11,IY,7)+CGRESQ(11,IY,6)+CGRESQ(11,IY,7))
    z[33] = (dfd['CGCOMMQ'].loc[11].loc[6] + dfd['CGCOMMQ'].loc[11].loc[7] + dfd['CGRESQ'].loc[11].loc[6] + dfd['CGRESQ'].loc[11].loc[7])
 
    #      Other                                                    
    #T28(34,IY,IS)=(CGCOMMQ(11,IY,9)+CGCOMMQ(11,IY,10)+CGRESQ(11,IY,9)+CGRESQ(11,IY,10))
    z[34] = (dfd['CGCOMMQ'].loc[11].loc[9] + dfd['CGCOMMQ'].loc[11].loc[10] + dfd['CGRESQ'].loc[11].loc[9] + dfd['CGRESQ'].loc[11].loc[10])
 
    #        Total                                                  
    #T28(35,IY,IS)=FSUM(T28(30,IY,IS),5)
    z[35] = z[30]+z[31]+z[32]+z[33]+z[34]

    #      Other Resd & Comm Generators                             
    #T28(56,IY,IS)=(CGCOMMQ(11,IY,4)+CGCOMMQ(11,IY,5)+CGCOMMQ(11,IY,8)+CGCOMMQ(11,IY,11)+CGRESQ(11,IY,4)+CGRESQ(11,IY,5)+CGRESQ(11,IY,8)+CGRESQ(11,IY,11])
    z[56] = (dfd['CGCOMMQ'].loc[11].loc[4] + dfd['CGCOMMQ'].loc[11].loc[5] + dfd['CGCOMMQ'].loc[11].loc[8] + dfd['CGCOMMQ'].loc[11].loc[11] + dfd['CGRESQ'].loc[11].loc[4] + dfd['CGRESQ'].loc[11].loc[5] + dfd['CGRESQ'].loc[11].loc[8] + dfd['CGRESQ'].loc[11].loc[11])
 
    #                                                               
    #                                                               
 
    #    Traditional Cogenerators                                   
 
    #      Coal                                                     
    #T28(36,IY,IS)=T28(13,IY,IS)+T28(23,IY,IS)+T28(25,IY,IS)+T28(30,IY,IS)
    z[36] = z[13]+z[23]+z[25]+z[30]
 
    #      Petroleum                                                
    #T28(37,IY,IS)=T28(14,IY,IS)+T28(20,IY,IS)+T28(26,IY,IS)+T28(31,IY,IS)
    z[37] = z[14]+z[20]+z[26]+z[31]
 
    #      Other Gaseous Fuel                                       
    #T28(38,IY,IS)=T28(19,IY,IS)
    z[38] = z[19]
 
    #      Natural Gas                                              
    #T28(39,IY,IS)=T28(15,IY,IS)+T28(21,IY,IS)+T28(27,IY,IS)+T28(32,IY,IS)
    z[39] = z[15]+z[21]+z[27]+z[32]
 
    #      Renewable Sources                                        
    #T28(40,IY,IS)=T28(16,IY,IS)+T28(22,IY,IS)+T28(33,IY,IS)
    z[40] = z[16]+z[22]+z[33]
 
    #      Other                                                    
    #T28(41,IY,IS)=T28(17,IY,IS)+T28(28,IY,IS)+T28(34,IY,IS)
    z[41] = z[17]+z[28]+z[34]
 
    #        Total                                                  
    #T28(42,IY,IS)=(FSUM(T28(36,IY,IS),6))
    z[42] = z[36]+z[37]+z[38]+z[39]+z[40]+z[41]

    #                                                               
 
    #    Non-Traditional Cogenerators                               
 
    #      Coal                                                     
    #T28(43,IY,IS)=(CGNTQ(MNUMNR,IY,1])
    z[43] = (dfd['CGNTQ'].loc[MNUMNR].loc[1])
 
    #      Petroleum                                                
    #T28(44,IY,IS)=(CGNTQ(MNUMNR,IY,2])
    z[44] = (dfd['CGNTQ'].loc[MNUMNR].loc[2])
 
    #      Natural Gas                                              
    #T28(45,IY,IS)=(CGNTQ(MNUMNR,IY,3])
    z[45] = (dfd['CGNTQ'].loc[MNUMNR].loc[3])
 
    #      Renewable Sources                                        
    #T28(46,IY,IS)=(CGNTQ(MNUMNR,IY,4)+CGNTQ(MNUMNR,IY,5)+CGNTQ(MNUMNR,IY,6)+CGNTQ(MNUMNR,IY,7)+CGNTQ(MNUMNR,IY,8])
    z[46] = (dfd['CGNTQ'].loc[MNUMNR].loc[4] + dfd['CGNTQ'].loc[MNUMNR].loc[5] + dfd['CGNTQ'].loc[MNUMNR].loc[6] + dfd['CGNTQ'].loc[MNUMNR].loc[7] + dfd['CGNTQ'].loc[MNUMNR].loc[8])
 
    #      Other                                                    
    #T28(47,IY,IS)=(CGNTQ(MNUMNR,IY,9)+CGNTQ(MNUMNR,IY,10])
    z[47] = (dfd['CGNTQ'].loc[MNUMNR].loc[9] + dfd['CGNTQ'].loc[MNUMNR].loc[10])
 
    #        Total                                                  
    #T28(48,IY,IS)=(FSUM(T28(43,IY,IS),5])
    z[48] = z[43]+z[44]+z[45]+z[46]+z[47]

    #                                                               
 
    #    Total Combined Heat and Power                              
 
    #      Coal                                                     
    #T28(49,IY,IS)=CGNTQ(MNUMNR,IY,1)+T28(36,IY,IS)
    z[49] = dfd['CGNTQ'].loc[MNUMNR].loc[1] + z[36]
 
    #      Petroleum                                                
    #T28(50,IY,IS)=(CGNTQ(MNUMNR,IY,2])+T28(37,IY,IS)+T28(38,IY,IS)
    z[50] = dfd['CGNTQ'].loc[MNUMNR].loc[2] + z[37]+z[38]
 
    #      Natural Gas                                              
    #T28(51,IY,IS)=(CGNTQ(MNUMNR,IY,3])+T28(39,IY,IS)
    z[51] = dfd['CGNTQ'].loc[MNUMNR].loc[3] + z[39]
 
    #      Renewable Sources                                        
    #T28(52,IY,IS)=(CGNTQ(MNUMNR,IY,4)+CGNTQ(MNUMNR,IY,5)+CGNTQ(MNUMNR,IY,6)+CGNTQ(MNUMNR,IY,7)+CGNTQ(MNUMNR,IY,8])+T28(40,IY,IS)
    z[52] = dfd['CGNTQ'].loc[MNUMNR].loc[4] + dfd['CGNTQ'].loc[MNUMNR].loc[5] + dfd['CGNTQ'].loc[MNUMNR].loc[6] + dfd['CGNTQ'].loc[MNUMNR].loc[7] + dfd['CGNTQ'].loc[MNUMNR].loc[8] + z[40]
 
    #      Other                                                    
    #T28(53,IY,IS)=(CGNTQ(MNUMNR,IY,9)+CGNTQ(MNUMNR,IY,10])+T28(41,IY,IS)
    z[53] = dfd['CGNTQ'].loc[MNUMNR].loc[9] + dfd['CGNTQ'].loc[MNUMNR].loc[10] + z[41]
 
    #        Total                                                  
    #T28(54,IY,IS)=(FSUM(T28(49,IY,IS),5])
    z[54] = z[49]+z[50]+z[51]+z[52]+z[53]
    
    return z
