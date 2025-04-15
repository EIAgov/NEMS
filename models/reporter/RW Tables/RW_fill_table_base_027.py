# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_027(dfd, table_spec, table_id):
    """Fill table    Non-Utility Electricity Capacity
   
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



    #   Non-Utility Electricity Capacity
    #   (thousand megawatts, unless otherwise noted)
    #    Sector and Fuel                                            

    #   Capacity by Fuel Type                                       
    #    Independent Power Producers                                
 
    #      Coal                                                     
    #T27(1,IY,IS)=UCAPCSN(MNUMNR,IY)
    z[1] = dfd['UCAPCSN'].loc[MNUMNR]
 
    #      Oil and Natural Gas Steam                                
    #T27(2,IY,IS)=UCAPOSN(MNUMNR,IY)
    z[2] = dfd['UCAPOSN'].loc[MNUMNR]
 
    #      Combined Cycle                                           
    #T27(3,IY,IS)=UCAPCCN(MNUMNR,IY)
    z[3] = dfd['UCAPCCN'].loc[MNUMNR]
 
    #      Combustion Turbine/Diesel                                
    #T27(4,IY,IS)=UCAPCTN(MNUMNR,IY)
    z[4] = dfd['UCAPCTN'].loc[MNUMNR]
 
    #      Nuclear Power                                            
    #T27(5,IY,IS)=UCAPNUN(MNUMNR,IY)+UCAPSMN(MNUMNR,IY)
    z[5] = dfd['UCAPNUN'].loc[MNUMNR] + dfd['UCAPSMN'].loc[MNUMNR]
 
    #      Pumped Storage                                           
    #T27(6,IY,IS)=UCAPPSN(MNUMNR,IY)
    z[6] = dfd['UCAPPSN'].loc[MNUMNR]
 
    #      Fuel Cells                                               
    #T27(7,IY,IS)=UCAPFCN(MNUMNR,IY)
    z[7] = dfd['UCAPFCN'].loc[MNUMNR]
 
    #      Renewable Sources                                        
    #T27(8,IY,IS)=UCAPHYN(MNUMNR,IY)+UCAPGEN(MNUMNR,IY)+UCAPMSN(MNUMNR,IY)+UCAPWDN(MNUMNR,IY)+UCAPSTN(MNUMNR,IY)+UCAPPVN(MNUMNR,IY)+UCAPWNN(MNUMNR,IY)+UCAPPTN(MNUMNR,IY)+UCAPWLN(MNUMNR,IY)
    z[8] = dfd['UCAPHYN'].loc[MNUMNR] + dfd['UCAPGEN'].loc[MNUMNR] + dfd['UCAPMSN'].loc[MNUMNR] + dfd['UCAPWDN'].loc[MNUMNR] + dfd['UCAPSTN'].loc[MNUMNR] + dfd['UCAPPVN'].loc[MNUMNR] + dfd['UCAPWNN'].loc[MNUMNR] + dfd['UCAPPTN'].loc[MNUMNR] + dfd['UCAPWLN'].loc[MNUMNR] + dfd["UCAPBIU"].loc[MNUMNR] + dfd["UCAPBIN"].loc[MNUMNR]
 
    #        Total                                                  
    #T27(9,IY,IS)=FSUM(T27(1,IY,IS),8)
    z[9] = z[1]+z[2]+z[3]+z[4]+z[5]+z[6]+z[7]+z[8]
 
    #                                                               
 
    #    Combined Heat and Power                                    
 
    #     Industrial                                                
 
    #      Coal                                                     
    #T27(10,IY,IS)=CGINDLCAP(11,IY,1)/1000.
    z[10] = dfd['CGINDLCAP'].loc[11].loc[1] / 1000.
 
    #      Petroleum                                                
    #T27(11,IY,IS)=CGINDLCAP(11,IY,2)/1000.
    z[11] = dfd['CGINDLCAP'].loc[11].loc[2] / 1000.
 
    #      Natural Gas                                              
    #T27(12,IY,IS)=CGINDLCAP(11,IY,3)/1000.
    z[12] = dfd['CGINDLCAP'].loc[11].loc[3] / 1000.
 
    #      Renewable Sources                                        
    #T27(13,IY,IS)=(CGINDLCAP(11,IY,6)+CGINDLCAP(11,IY,7))/1000.
    z[13] = (dfd['CGINDLCAP'].loc[11].loc[6] + dfd['CGINDLCAP'].loc[11].loc[7] ) / 1000.
 
    #      Other                                                    
    #T27(14,IY,IS)=(CGINDLCAP(11,IY,9)+CGINDLCAP(11,IY,10))/1000.
    z[14] = (dfd['CGINDLCAP'].loc[11].loc[9] + dfd['CGINDLCAP'].loc[11].loc[10] ) / 1000.
 
    #        Total                                                  
    #T27(15,IY,IS)=FSUM(T27(10,IY,IS),5)
    z[15] = z[10]+z[11]+z[12]+z[13]+z[14]
 
    #      Other Industrial Generators                              
    #T27(52,IY,IS)=(CGINDLCAP(11,IY,4)+CGINDLCAP(11,IY,5)+CGINDLCAP(11,IY,8)+CGINDLCAP(11,IY,11))/1000.
    z[52] = (dfd['CGINDLCAP'].loc[11].loc[4] + dfd['CGINDLCAP'].loc[11].loc[5] + dfd['CGINDLCAP'].loc[11].loc[8] + dfd['CGINDLCAP'].loc[11].loc[11] ) / 1000.
 
    #                                                               
 
    #     Refining                                                  
 
    #      Coal                                                     
    #T27(20,IY,IS)=CGREFCAP(11,IY,1)*.001
    z[20] = dfd['CGREFCAP'].loc[11].loc[1] * .001
 
    #      Other Gaseous Fuels                                      
    #T27(16,IY,IS)=CGREFCAP(11,IY,9)*.001
    z[16] = dfd['CGREFCAP'].loc[11].loc[9] * .001
 
    #      Petroleum                                                
    #T27(17,IY,IS)=CGREFCAP(11,IY,2)*.001
    z[17] = dfd['CGREFCAP'].loc[11].loc[2] * .001
 
    #      Natural Gas                                              
    #T27(18,IY,IS)=CGREFCAP(11,IY,3)*.001
    z[18] = dfd['CGREFCAP'].loc[11].loc[3] * .001
 
    #      Renewable                                                
    #T27(19,IY,IS)=(CGREFCAP(11,IY,6)+CGREFCAP(11,IY,7))*.001
    z[19] = (dfd['CGREFCAP'].loc[11].loc[6] + dfd['CGREFCAP'].loc[11].loc[7]) * .001
 
    #        Total                                                  
    #T27(21,IY,IS)=FSUM(T27(16,IY,IS),5)
    z[21] = z[16]+z[17]+z[18]+z[19]+z[20]
 
    #                                                               
 
    #    Enhanced Oil Recovery (EOR)                                
 
    #      Coal                                                     
    #T27(22,IY,IS)=CGOGSCAP(11,IY,1)/1000.
    z[22] = dfd['CGOGSCAP'].loc[11].loc[1] / 1000.
 
    #      Petroleum                                                
    #T27(23,IY,IS)=CGOGSCAP(11,IY,2)/1000.
    z[23] = dfd['CGOGSCAP'].loc[11].loc[2] / 1000.
 
    #      Natural Gas                                              
    #T27(24,IY,IS)=CGOGSCAP(11,IY,3)/1000.
    z[24] = dfd['CGOGSCAP'].loc[11].loc[3] / 1000.
 
    #      Other                                                    
    #T27(25,IY,IS)=CGOGSCAP(11,IY,4)/1000.
    z[25] = dfd['CGOGSCAP'].loc[11].loc[4] / 1000.
 
    #        Total                                                  
    #T27(26,IY,IS)=(FSUM(T27(22,IY,IS),4))
    z[26] = z[22]+z[23]+z[24]+z[25]
 
    #                                                               
 
    #    Residential and Commercial                                 
 
    #      Coal                                                     
    #T27(27,IY,IS)=(CGCOMMCAP(11,IY,1)+CGRESCAP(11,IY,1))*.001
    z[27] = (dfd['CGCOMMCAP'].loc[11].loc[1] + dfd['CGRESCAP'].loc[11].loc[1] ) * .001
 
    #      Petroleum                                                
    #T27(28,IY,IS)=(CGCOMMCAP(11,IY,2)+CGRESCAP(11,IY,2))*.001
    z[28] = (dfd['CGCOMMCAP'].loc[11].loc[2] + dfd['CGRESCAP'].loc[11].loc[2] ) * .001
 
    #      Natural Gas                                              
    #T27(29,IY,IS)=(CGCOMMCAP(11,IY,3)+CGRESCAP(11,IY,3))*.001
    z[29] = (dfd['CGCOMMCAP'].loc[11].loc[3] + dfd['CGRESCAP'].loc[11].loc[3] ) * .001
 
    #      Renewable Sources                                        
    #T27(30,IY,IS)=(CGCOMMCAP(11,IY,6)+CGCOMMCAP(11,IY,7)+CGRESCAP(11,IY,6)+CGRESCAP(11,IY,7))*.001
    z[30] = (dfd['CGCOMMCAP'].loc[11].loc[6] + dfd['CGCOMMCAP'].loc[11].loc[7] + dfd['CGRESCAP'].loc[11].loc[6] + dfd['CGRESCAP'].loc[11].loc[7] ) * .001
 
    #      Other                                                    
    #T27(31,IY,IS)=(CGCOMMCAP(11,IY,9)+CGCOMMCAP(11,IY,10)+CGRESCAP(11,IY,9)+CGRESCAP(11,IY,10))*.001
    z[31] = (dfd['CGCOMMCAP'].loc[11].loc[9] + dfd['CGCOMMCAP'].loc[11].loc[10] + dfd['CGRESCAP'].loc[11].loc[9] + dfd['CGRESCAP'].loc[11].loc[10] ) * .001
 
    #        Total                                                  
    #T27(32,IY,IS)=FSUM(T27(27,IY,IS),5)
    z[32] = z[27]+z[28]+z[29]+z[30]+z[31]
 
    #      Other Resd & Comm Generators                             
    #T27(53,IY,IS)=(CGCOMMCAP(11,IY,4)+CGCOMMCAP(11,IY,5)+CGCOMMCAP(11,IY,8)+CGCOMMCAP(11,IY,11)+CGRESCAP(11,IY,4)+CGRESCAP(11,IY,5)+CGRESCAP(11,IY,8)+CGRESCAP(11,IY,11))*.001
    z[53] = (dfd['CGCOMMCAP'].loc[11].loc[4] + dfd['CGCOMMCAP'].loc[11].loc[5] + dfd['CGCOMMCAP'].loc[11].loc[8] + dfd['CGCOMMCAP'].loc[11].loc[11] + dfd['CGRESCAP'].loc[11].loc[4] + dfd['CGRESCAP'].loc[11].loc[5] + dfd['CGRESCAP'].loc[11].loc[8] + dfd['CGRESCAP'].loc[11].loc[11] ) * .001
 
    #                                                               
 
    #                                                               
 
    #    Traditional Cogenerators                                   
 
    #      Coal                                                     
    #T27(33,IY,IS)=T27(10,IY,IS)+T27(20,IY,IS)+T27(22,IY,IS)+T27(27,IY,IS)
    z[33] = z[10]+z[20]+z[22]+z[27]
 
    #      Petroleum                                                
    #T27(34,IY,IS)=T27(11,IY,IS)+T27(17,IY,IS)+T27(23,IY,IS)+T27(28,IY,IS)
    z[34] = z[11]+z[17]+z[23]+z[28]
 
    #      Other Gaseous Fuel                                       
    #T27(35,IY,IS)=T27(16,IY,IS)
    z[35] = z[16]
 
    #      Natural Gas                                              
    #T27(36,IY,IS)=T27(12,IY,IS)+T27(18,IY,IS)+T27(24,IY,IS)+T27(29,IY,IS)
    z[36] = z[12]+z[18]+z[24]+z[29]
 
    #      Renewable Sources                                        
    #T27(37,IY,IS)=T27(13,IY,IS)+T27(19,IY,IS)+T27(30,IY,IS)
    z[37] = z[13]+z[19]+z[30]
 
    #      Other                                                    
    #T27(38,IY,IS)=T27(14,IY,IS)+T27(25,IY,IS)+T27(31,IY,IS)
    z[38] = z[14]+z[25]+z[31]
 
    #        Total                                                  
    #T27(39,IY,IS)=(FSUM(T27(33,IY,IS),6))
    z[39] = z[33]+z[34]+z[35]+z[36]+z[37]+z[38]
 
    #                                                               
 
    #    Non-Traditional Cogenerators                               
 
    #      Coal                                                     
    #T27(40,IY,IS)=(CGNTCAP(MNUMNR,IY,1))*.001
    z[40] = (dfd['CGNTCAP'].loc[MNUMNR].loc[1] ) * .001
 
    #      Petroleum                                                
    #T27(41,IY,IS)=(CGNTCAP(MNUMNR,IY,2))*.001
    z[41] = (dfd['CGNTCAP'].loc[MNUMNR].loc[2] ) * .001
 
    #      Natural Gas                                              
    #T27(42,IY,IS)=(CGNTCAP(MNUMNR,IY,3))*.001
    z[42] = (dfd['CGNTCAP'].loc[MNUMNR].loc[3] ) * .001
 
    #      Renewable Sources                                        
    #T27(43,IY,IS)=(CGNTCAP(MNUMNR,IY,4)+CGNTCAP(MNUMNR,IY,5)+CGNTCAP(MNUMNR,IY,6)+CGNTCAP(MNUMNR,IY,7)+CGNTCAP(MNUMNR,IY,8))*.001
    z[43] = (dfd['CGNTCAP'].loc[MNUMNR].loc[4] + dfd['CGNTCAP'].loc[MNUMNR].loc[5] + dfd['CGNTCAP'].loc[MNUMNR].loc[6] + dfd['CGNTCAP'].loc[MNUMNR].loc[7] + dfd['CGNTCAP'].loc[MNUMNR].loc[8] ) * .001
 
    #      Other                                                    
    #T27(44,IY,IS)=(CGNTCAP(MNUMNR,IY,9)+CGNTCAP(MNUMNR,IY,10))*.001
    z[44] = (dfd['CGNTCAP'].loc[MNUMNR].loc[9] + dfd['CGNTCAP'].loc[MNUMNR].loc[10] ) * .001
 
    #        Total                                                  
    #T27(45,IY,IS)=(FSUM(T27(40,IY,IS),5))
    z[45] = z[40]+z[41]+z[42]+z[43]+z[44]
 
    #                                                               
 
    #    Total Combined Heat and Power                              
 
    #      Coal                                                     
    #T27(46,IY,IS)=(CGNTCAP(MNUMNR,IY,1)*.001)+T27(33,IY,IS)
    z[46] = dfd['CGNTCAP'].loc[MNUMNR].loc[1]* 0.001 + z[33]
 
    #      Petroleum                                                
    #T27(47,IY,IS)=CGNTCAP(MNUMNR,IY,2)*.001+T27(34,IY,IS)+T27(35,IY,IS)
    z[47] = dfd['CGNTCAP'].loc[MNUMNR].loc[2] * .001 + z[34]+z[35]
 
    #      Natural Gas                                              
    #T27(48,IY,IS)=CGNTCAP(MNUMNR,IY,3)*.001+T27(36,IY,IS)
    z[48] = dfd['CGNTCAP'].loc[MNUMNR].loc[3] * .001 + z[36]
 
    #      Renewable Sources                                        
    #T27(49,IY,IS)=((CGNTCAP(MNUMNR,IY,4)+CGNTCAP(MNUMNR,IY,5)+CGNTCAP(MNUMNR,IY,6)+CGNTCAP(MNUMNR,IY,7)+CGNTCAP(MNUMNR,IY,8))*.001)+T27(37,IY,IS)
    z[49] = (dfd['CGNTCAP'].loc[MNUMNR].loc[4] + dfd['CGNTCAP'].loc[MNUMNR].loc[5] + dfd['CGNTCAP'].loc[MNUMNR].loc[6] + dfd['CGNTCAP'].loc[MNUMNR].loc[7] + dfd['CGNTCAP'].loc[MNUMNR].loc[8] ) * 0.001 + z[37]
 
    #      Other                                                    
    #T27(50,IY,IS)=((CGNTCAP(MNUMNR,IY,9)+CGNTCAP(MNUMNR,IY,10))*.001)+T27(38,IY,IS)
    z[50] = (dfd['CGNTCAP'].loc[MNUMNR].loc[9] + dfd['CGNTCAP'].loc[MNUMNR].loc[10] ) * 0.001 + z[38]
 
    #        Total                                                  
    #T27(51,IY,IS)=(FSUM(T27(46,IY,IS),5))
    z[51] = z[46]+z[47]+z[48]+z[49]+z[50]
    
    return z
