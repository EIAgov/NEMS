# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_026(dfd, table_spec, table_id):
    """Fill table   Non-Utility Electricity Generation
   
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


    #   Non-Utility Electricity Generation
    #   (billion kilowatthours, unless otherwise noted)
    #    Sector and Fuel                                            
    #                                                               
    #   Generation by Fuel Type                                     
    #    Independent Power Producers                                
 
    #      Coal                                                     
    #T26(1,IY,IS)=UGNCLNR(2,MNUMNR,IY)
    z[1] = dfd['UGNCLNR'].loc[2].loc[MNUMNR]
 
    #      Petroleum                                                
    #T26(2,IY,IS)=UGNDSNR(2,MNUMNR,IY)+UGNRLNR(2,MNUMNR,IY)+UGNRHNR(2,MNUMNR,IY)
    z[2] = dfd['UGNDSNR'].loc[2].loc[MNUMNR] + dfd['UGNRLNR'].loc[2].loc[MNUMNR] + dfd['UGNRHNR'].loc[2].loc[MNUMNR]
 
    #      Natural Gas                                              
    #T26(3,IY,IS)=UGNGFNR(2,MNUMNR,IY)+UGNGINR(2,MNUMNR,IY)+UGNGCNR(2,MNUMNR,IY)
    z[3] = dfd['UGNGFNR'].loc[2].loc[MNUMNR] + dfd['UGNGINR'].loc[2].loc[MNUMNR] + dfd['UGNGCNR'].loc[2].loc[MNUMNR]
 
    #      Nuclear Power                                            
    #T26(58,IY,IS)=UGNURNR(2,MNUMNR,IY)
    z[58] = dfd['UGNURNR'].loc[2].loc[MNUMNR]
 
    #      Renewable Sources                                        
    #T26(4,IY,IS)=UGNHYNR(2,MNUMNR,IY)+UGNGENR(2,MNUMNR,IY)+UGNMSNR(2,MNUMNR,IY)+UGNWDNR(2,MNUMNR,IY)+UGNSONR(2,MNUMNR,IY)+UGNPVNR(2,MNUMNR,IY)+UGNPTNR(2,MNUMNR,IY)+UGNWNNR(2,MNUMNR,IY)+UGNWLNR(2,MNUMNR,IY)+UGNWFNR(2,MNUMNR,IY)+UGNPSNR(2,MNUMNR,IY)+UGNSDNR(2,MNUMNR,IY)
    z[4] = dfd['UGNHYNR'].loc[2].loc[MNUMNR] + dfd['UGNGENR'].loc[2].loc[MNUMNR] + dfd['UGNMSNR'].loc[2].loc[MNUMNR] + dfd['UGNWDNR'].loc[2].loc[MNUMNR] + dfd['UGNSONR'].loc[2].loc[MNUMNR] + dfd['UGNPVNR'].loc[2].loc[MNUMNR] + dfd['UGNPTNR'].loc[2].loc[MNUMNR] + dfd['UGNWNNR'].loc[2].loc[MNUMNR] + dfd['UGNWLNR'].loc[2].loc[MNUMNR] + dfd['UGNWFNR'].loc[2].loc[MNUMNR] + dfd['UGNPSNR'].loc[2].loc[MNUMNR] + dfd['UGNSDNR'].loc[2].loc[MNUMNR]
 
    #        Total                                                  
    #T26(5,IY,IS)=FSUM(T26(1,IY,IS),4)+T26(58,IY,IS)
    z[5] = z[1]+z[2]+z[3]+z[4] + z[58]
 
    #                                                               
 
    #    Combined Heat and Power                                    
 
    #     Industrial                                                
 
    #      Coal                                                     
    #T26(6,IY,IS)=(CGINDLGEN(11,IY,1,1)+CGINDLGEN(11,IY,1,2))*.001
    z[6] = (dfd['CGINDLGEN'].loc[11].loc[1,1] + dfd['CGINDLGEN'].loc[11].loc[1].loc[2]) * .001
 
    #      Petroleum                                                
    #T26(7,IY,IS)=(CGINDLGEN(11,IY,2,1)+CGINDLGEN(11,IY,2,2))*.001
    z[7] = (dfd['CGINDLGEN'].loc[11].loc[2,1] + dfd['CGINDLGEN'].loc[11].loc[2].loc[2]) * .001
 
    #      Natural Gas                                              
    #T26(8,IY,IS)=(CGINDLGEN(11,IY,3,1)+CGINDLGEN(11,IY,3,2))*.001
    z[8] = (dfd['CGINDLGEN'].loc[11].loc[3,1] + dfd['CGINDLGEN'].loc[11].loc[3].loc[2]) * .001
 
    #      Renewable Sources                                        
    #T26(9,IY,IS)=(CGINDLGEN(11,IY,6,1)+CGINDLGEN(11,IY,6,2)+CGINDLGEN(11,IY,7,1)+CGINDLGEN(11,IY,7,2))*.001
    z[9] = (dfd['CGINDLGEN'].loc[11].loc[6,1] + dfd['CGINDLGEN'].loc[11].loc[6,2] + dfd['CGINDLGEN'].loc[11].loc[7,1] + dfd['CGINDLGEN'].loc[11].loc[7].loc[2]) * .001
 
    #      Other                                                    
    #T26(10,IY,IS)=(CGINDLGEN(11,IY,9,1)+CGINDLGEN(11,IY,9,2)+CGINDLGEN(11,IY,10,1)+CGINDLGEN(11,IY,10,2))*.001
    z[10] = (dfd['CGINDLGEN'].loc[11].loc[9,1] + dfd['CGINDLGEN'].loc[11].loc[9,2] + dfd['CGINDLGEN'].loc[11].loc[10,1] + dfd['CGINDLGEN'].loc[11].loc[10].loc[2]) * .001
 
    #        Total                                                  
    #T26(11,IY,IS)=FSUM(T26(6,IY,IS),5)
    z[11] = z[6]+z[7]+z[8]+z[9]+z[10]
 
    #      Other Industrial Generators                              
    #T26(48,IY,IS)=(CGINDLGEN(11,IY,4,1)+CGINDLGEN(11,IY,4,2)+CGINDLGEN(11,IY,5,1)+CGINDLGEN(11,IY,5,2)+CGINDLGEN(11,IY,11,1)+CGINDLGEN(11,IY,11,2)+CGINDLGEN(11,IY,8,1)+CGINDLGEN(11,IY,8,2))*.001
    z[48] = (dfd['CGINDLGEN'].loc[11].loc[4,1] + dfd['CGINDLGEN'].loc[11].loc[4,2] + dfd['CGINDLGEN'].loc[11].loc[5,1] + dfd['CGINDLGEN'].loc[11].loc[5,2] + dfd['CGINDLGEN'].loc[11].loc[11,1] + dfd['CGINDLGEN'].loc[11].loc[11,2] + dfd['CGINDLGEN'].loc[11].loc[8,1] + dfd['CGINDLGEN'].loc[11].loc[8].loc[2]) * .001
 
    #                                                               
 
    #     Refining                                                  
 
    #      Coal                                                     
    #T26(16,IY,IS)=(CGREFGEN(11,IY,1,1)+CGREFGEN(11,IY,1,2))*.001
    z[16] = (dfd['CGREFGEN'].loc[11].loc[1,1] + dfd['CGREFGEN'].loc[11].loc[1].loc[2]) * .001
 
    #      Other Gaseous Fuels                                      
    #T26(12,IY,IS)=(CGREFGEN(11,IY,9,1)+CGREFGEN(11,IY,9,2))*.001
    z[12] = (dfd['CGREFGEN'].loc[11].loc[9,1] + dfd['CGREFGEN'].loc[11].loc[9].loc[2]) * .001
 
    #      Petroleum                                                
    #T26(13,IY,IS)=(CGREFGEN(11,IY,2,1)+CGREFGEN(11,IY,2,2))*.001
    z[13] = (dfd['CGREFGEN'].loc[11].loc[2,1] + dfd['CGREFGEN'].loc[11].loc[2].loc[2]) * .001
 
    #      Natural Gas                                              
    #T26(14,IY,IS)=(CGREFGEN(11,IY,3,1)+CGREFGEN(11,IY,3,2))*.001
    z[14] = (dfd['CGREFGEN'].loc[11].loc[3,1] + dfd['CGREFGEN'].loc[11].loc[3].loc[2]) * .001
 
    #      Renewable                                                
    #T26(15,IY,IS)=(CGREFGEN(11,IY,6,1)+CGREFGEN(11,IY,6,2)+CGREFGEN(11,IY,7,1)+CGREFGEN(11,IY,7,2))*.001
    z[15] = (dfd['CGREFGEN'].loc[11].loc[6,1] + dfd['CGREFGEN'].loc[11].loc[6,2] + dfd['CGREFGEN'].loc[11].loc[7,1] + dfd['CGREFGEN'].loc[11].loc[7].loc[2]) * .001
 
    #        Total                                                  
    #T26(17,IY,IS)=FSUM(T26(12,IY,IS),5)
    z[17] = z[12]+z[13]+z[14]+z[15]+z[16]
 
    #                                                               
 
    #    Enhanced Oil Recovery (EOR)                                
 
    #      Coal                                                     
    #T26(18,IY,IS)=(CGOGSGEN(11,IY,1,1)+CGOGSGEN(11,IY,1,2))*.001
    z[18] = (dfd['CGOGSGEN'].loc[11].loc[1,1] + dfd['CGOGSGEN'].loc[11].loc[1].loc[2]) * .001
 
    #      Petroleum                                                
    #T26(19,IY,IS)=(CGOGSGEN(11,IY,2,1)+CGOGSGEN(11,IY,2,2))*.001
    z[19] = (dfd['CGOGSGEN'].loc[11].loc[2,1] + dfd['CGOGSGEN'].loc[11].loc[2].loc[2]) * .001
 
    #      Natural Gas                                              
    #T26(20,IY,IS)=(CGOGSGEN(11,IY,3,1)+CGOGSGEN(11,IY,3,2))*.001
    z[20] = (dfd['CGOGSGEN'].loc[11].loc[3,1] + dfd['CGOGSGEN'].loc[11].loc[3].loc[2]) * .001
 
    #      Other                                                    
    #T26(21,IY,IS)=(CGOGSGEN(11,IY,4,1)+CGOGSGEN(11,IY,4,2))*.001
    z[21] = (dfd['CGOGSGEN'].loc[11].loc[4,1] + dfd['CGOGSGEN'].loc[11].loc[4].loc[2]) * .001
 
    #        Total                                                  
    #T26(22,IY,IS)=(FSUM(T26(18,IY,IS),4))
    z[22] = z[18]+z[19]+z[20]+z[21]
    
    #                                                               
 
    #    Residential and Commercial                                 
 
    #      Coal                                                     
    #T26(23,IY,IS)=(CGCOMMGEN(11,IY,1,1)+CGCOMMGEN(11,IY,1,2)+CGRESGEN(11,IY,1,1)+CGRESGEN(11,IY,1,2))*.001
    z[23] = (dfd['CGCOMMGEN'].loc[11].loc[1,1] + dfd['CGCOMMGEN'].loc[11].loc[1,2] + dfd['CGRESGEN'].loc[11].loc[1,1] + dfd['CGRESGEN'].loc[11].loc[1].loc[2]) * .001
 
    #      Petroleum                                                
    #T26(24,IY,IS)=(CGCOMMGEN(11,IY,2,1)+CGCOMMGEN(11,IY,2,2)+CGRESGEN(11,IY,2,1)+CGRESGEN(11,IY,2,2))*.001
    z[24] = (dfd['CGCOMMGEN'].loc[11].loc[2,1] + dfd['CGCOMMGEN'].loc[11].loc[2,2] + dfd['CGRESGEN'].loc[11].loc[2,1] + dfd['CGRESGEN'].loc[11].loc[2].loc[2]) * .001
 
    #      Natural Gas                                              
    #T26(25,IY,IS)=(CGCOMMGEN(11,IY,3,1)+CGCOMMGEN(11,IY,3,2)+CGRESGEN(11,IY,3,1)+CGRESGEN(11,IY,3,2))*.001
    z[25] = (dfd['CGCOMMGEN'].loc[11].loc[3,1] + dfd['CGCOMMGEN'].loc[11].loc[3,2] + dfd['CGRESGEN'].loc[11].loc[3,1] + dfd['CGRESGEN'].loc[11].loc[3].loc[2]) * .001
 
    #      Renewable Sources                                        
    #T26(26,IY,IS)=(CGCOMMGEN(11,IY,6,1)+CGCOMMGEN(11,IY,7,1)+CGCOMMGEN(11,IY,6,2)+CGCOMMGEN(11,IY,7,2)+CGRESGEN(11,IY,6,1)+CGRESGEN(11,IY,6,2)+CGRESGEN(11,IY,7,1)+CGRESGEN(11,IY,7,2))*.001
    z[26] = (dfd['CGCOMMGEN'].loc[11].loc[6,1] + dfd['CGCOMMGEN'].loc[11].loc[7,1] + dfd['CGCOMMGEN'].loc[11].loc[6,2] + dfd['CGCOMMGEN'].loc[11].loc[7,2] + dfd['CGRESGEN'].loc[11].loc[6,1] + dfd['CGRESGEN'].loc[11].loc[6,2] + dfd['CGRESGEN'].loc[11].loc[7,1] + dfd['CGRESGEN'].loc[11].loc[7].loc[2]) * .001
 
    #      Other                                                    
    #T26(27,IY,IS)=(CGCOMMGEN(11,IY,9,1)+CGCOMMGEN(11,IY,10,1)+CGCOMMGEN(11,IY,9,2)+CGCOMMGEN(11,IY,10,2)+CGRESGEN(11,IY,9,1)+CGRESGEN(11,IY,9,2)+CGRESGEN(11,IY,10,1)+CGRESGEN(11,IY,10,2))*.001
    z[27] = (dfd['CGCOMMGEN'].loc[11].loc[9,1] + dfd['CGCOMMGEN'].loc[11].loc[10,1] + dfd['CGCOMMGEN'].loc[11].loc[9,2] + dfd['CGCOMMGEN'].loc[11].loc[10,2] + dfd['CGRESGEN'].loc[11].loc[9,1] + dfd['CGRESGEN'].loc[11].loc[9,2] + dfd['CGRESGEN'].loc[11].loc[10,1] + dfd['CGRESGEN'].loc[11].loc[10].loc[2]) * .001
 
    #        Total                                                  
    #T26(28,IY,IS)=FSUM(T26(23,IY,IS),5)
    z[28] = z[23]+z[24]+z[25]+z[26]+z[27]
 
    #      Other Resd & Comm Generators                             
    #T26(49,IY,IS)=(CGCOMMGEN(11,IY,4,1)+CGCOMMGEN(11,IY,5,1)+CGCOMMGEN(11,IY,4,2)+CGCOMMGEN(11,IY,5,2)+CGCOMMGEN(11,IY,8,1)+CGCOMMGEN(11,IY,11,1)+CGCOMMGEN(11,IY,8,2)+CGCOMMGEN(11,IY,11,2)+CGRESGEN(11,IY,4,1)+CGRESGEN(11,IY,4,2)+CGRESGEN(11,IY,5,1)+CGRESGEN(11,IY,5,2)+CGRESGEN(11,IY,11,1)+CGRESGEN(11,IY,11,2)+CGRESGEN(11,IY,8,1)+CGRESGEN(11,IY,8,2))*.001
    z[49] = (dfd['CGCOMMGEN'].loc[11].loc[4].loc[1] + dfd['CGCOMMGEN'].loc[11].loc[5].loc[1] + dfd['CGCOMMGEN'].loc[11].loc[4].loc[2] + dfd['CGCOMMGEN'].loc[11].loc[5].loc[2] + dfd['CGCOMMGEN'].loc[11].loc[8].loc[1] + dfd['CGCOMMGEN'].loc[11].loc[11].loc[1] + dfd['CGCOMMGEN'].loc[11].loc[8].loc[2] + dfd['CGCOMMGEN'].loc[11].loc[11].loc[2] + dfd['CGRESGEN'].loc[11].loc[4].loc[1] + dfd['CGRESGEN'].loc[11].loc[4].loc[2] + dfd['CGRESGEN'].loc[11].loc[5].loc[1] + dfd['CGRESGEN'].loc[11].loc[5].loc[2] + dfd['CGRESGEN'].loc[11].loc[11].loc[1] + dfd['CGRESGEN'].loc[11].loc[11].loc[2] + dfd['CGRESGEN'].loc[11].loc[8].loc[1] + dfd['CGRESGEN'].loc[11].loc[8].loc[2]) * .001
 
    #                                                               
 
    #    Traditional Cogenerators                                   
 
    #      Coal                                                     
    #T26(29,IY,IS)=T26(6,IY,IS)+T26(16,IY,IS)+T26(18,IY,IS)+T26(23,IY,IS)
    z[29] = z[6]+z[16]+z[18]+z[23]
 
    #      Petroleum                                                
    #T26(30,IY,IS)=T26(7,IY,IS)+T26(13,IY,IS)+T26(19,IY,IS)+T26(24,IY,IS)
    z[30] = z[7]+z[13]+z[19]+z[24]
 
    #      Other Gaseous Fuel                                       
    #T26(31,IY,IS)=T26(12,IY,IS)
    z[31] = z[12]
 
    #      Natural Gas                                              
    #T26(32,IY,IS)=T26(8,IY,IS)+T26(14,IY,IS)+T26(20,IY,IS)+T26(25,IY,IS)
    z[32] = z[8]+z[14]+z[20]+z[25]
 
    #      Renewable Sources                                        
    #T26(33,IY,IS)=T26(9,IY,IS)+T26(15,IY,IS)+T26(26,IY,IS)
    z[33] = z[9]+z[15]+z[26]
 
    #      Other                                                    
    #T26(34,IY,IS)=T26(10,IY,IS)+T26(21,IY,IS)+T26(27,IY,IS)
    z[34] = z[10]+z[21]+z[27]
 
    #        Total                                                  
    #T26(35,IY,IS)=(FSUM(T26(29,IY,IS),6))
    z[35] = z[29]+z[30]+z[31]+z[32]+z[33]+z[34]
 
    #                                                               
 
    #                                                               
 
    #    Non-Traditional Cogenerators                               
 
    #      Coal                                                     
    #T26(36,IY,IS)=(CGNTGEN(MNUMNR,IY,1,1)+CGNTGEN(MNUMNR,IY,1,2))*.001
    z[36] = (dfd['CGNTGEN'].loc[MNUMNR].loc[1].loc[1] + dfd['CGNTGEN'].loc[MNUMNR].loc[1].loc[2]) * .001
 
    #      Petroleum                                                
    #T26(37,IY,IS)=(CGNTGEN(MNUMNR,IY,2,1)+CGNTGEN(MNUMNR,IY,2,2))*.001
    z[37] = (dfd['CGNTGEN'].loc[MNUMNR].loc[2].loc[1] + dfd['CGNTGEN'].loc[MNUMNR].loc[2].loc[2]) * .001
 
    #      Natural Gas                                              
    #T26(38,IY,IS)=(CGNTGEN(MNUMNR,IY,3,1)+CGNTGEN(MNUMNR,IY,3,2))*.001
    z[38] = (dfd['CGNTGEN'].loc[MNUMNR].loc[3].loc[1] + dfd['CGNTGEN'].loc[MNUMNR].loc[3].loc[2]) * .001
 
    #      Renewable Sources                                        
    #T26(39,IY,IS)=(CGNTGEN(MNUMNR,IY,4,1)+CGNTGEN(MNUMNR,IY,4,2)+CGNTGEN(MNUMNR,IY,5,1)+CGNTGEN(MNUMNR,IY,5,2)+CGNTGEN(MNUMNR,IY,6,1)+CGNTGEN(MNUMNR,IY,6,2)+CGNTGEN(MNUMNR,IY,7,1)+CGNTGEN(MNUMNR,IY,7,2)+CGNTGEN(MNUMNR,IY,8,1)+CGNTGEN(MNUMNR,IY,8,2))*.001
    z[39] = (dfd['CGNTGEN'].loc[MNUMNR].loc[4].loc[1] + dfd['CGNTGEN'].loc[MNUMNR].loc[4].loc[2] + dfd['CGNTGEN'].loc[MNUMNR].loc[5].loc[1] + dfd['CGNTGEN'].loc[MNUMNR].loc[5].loc[2] + dfd['CGNTGEN'].loc[MNUMNR].loc[6].loc[1] + dfd['CGNTGEN'].loc[MNUMNR].loc[6].loc[2] + dfd['CGNTGEN'].loc[MNUMNR].loc[7].loc[1] + dfd['CGNTGEN'].loc[MNUMNR].loc[7].loc[2] + dfd['CGNTGEN'].loc[MNUMNR].loc[8].loc[1] + dfd['CGNTGEN'].loc[MNUMNR].loc[8].loc[2]) * .001
 
    #      Other                                                    
    #T26(40,IY,IS)=(CGNTGEN(MNUMNR,IY,9,1)+CGNTGEN(MNUMNR,IY,9,2)+CGNTGEN(MNUMNR,IY,10,1)+CGNTGEN(MNUMNR,IY,10,2))*.001
    z[40] = (dfd['CGNTGEN'].loc[MNUMNR].loc[9].loc[1] + dfd['CGNTGEN'].loc[MNUMNR].loc[9].loc[2] + dfd['CGNTGEN'].loc[MNUMNR].loc[10].loc[1] + dfd['CGNTGEN'].loc[MNUMNR].loc[10].loc[2]) * .001
 
    #        Total                                                  
    #T26(41,IY,IS)=(FSUM(T26(36,IY,IS),5))
    z[41] = z[36]+z[37]+z[38]+z[39]+z[40]
 
    #                                                               
 
    #    Total Combined Heat and Power                              
 
    #      Coal                                                     
    #T26(42,IY,IS)=T26(36,IY,IS)+T26(29,IY,IS)
    z[42] = z[36]+z[29]
 
    #      Petroleum                                                
    #T26(43,IY,IS)=T26(37,IY,IS)+T26(30,IY,IS)
    z[43] = z[37]+z[30]
 
    #      Natural Gas                                              
    #T26(44,IY,IS)=T26(38,IY,IS)+T26(31,IY,IS)+T26(32,IY,IS)
    z[44] = z[38]+z[31]+z[32]
 
    #      Renewable Sources                                        
    #T26(45,IY,IS)=T26(39,IY,IS)+T26(33,IY,IS)
    z[45] = z[39]+z[33]
 
    #      Other                                                    
    #T26(46,IY,IS)=T26(40,IY,IS)+T26(34,IY,IS)
    z[46] = z[40]+z[34]
 
    #        Total                                                  
    #T26(47,IY,IS)=(FSUM(T26(42,IY,IS),5))
    z[47] = z[42]+z[43]+z[44]+z[45]+z[46]
 
    #                                                               
 
    #    Generation for Own Use                                     
 
    #      Independent Power Producers                              
    #T26(50,IY,IS)=CGOTGEN(MNUMNR,IY,1)
    z[50] = dfd['CGOTGEN'].loc[MNUMNR].loc[1]
 
    #      Industrial                                               
    #T26(51,IY,IS)=SUM(CGINDLGEN(11,IY,:,2))/1000.
    z[51] = dfd['CGINDLGEN'].loc[11,:,2,:].sum()/1000.
 
    
    #      Refinery                                                 
    #T26(52,IY,IS)=SUM(CGREFGEN(11,IY,:,2))/1000.
    z[52] = dfd['CGREFGEN'].loc[11,:,2,:].sum()/1000.
 
    #      Enhanced Oil Recovery                                    
    #T26(53,IY,IS)=SUM(CGOGSGEN(11,IY,:,2))/1000.     
    z[53] = dfd['CGOGSGEN'].loc[11,:,2,:].sum()/1000.
 
    #      Residential and Commercial                               
    #T26(54,IY,IS)=SUM(CGRESGEN(11,IY,:,2))/1000.+SUM(CGCOMMGEN(11,IY,:,2))/1000.
    z[54] = dfd['CGRESGEN'].loc[11,:,2,:].sum()/1000. + dfd['CGCOMMGEN'].loc[11,:,2,:].sum()/1000.
 
    #        Traditional CHaP Total                                 
    #T26(55,IY,IS)=FSUM(T26(50,IY,IS),5)
    z[55] = z[50]+z[51]+z[52]+z[53]+z[54]
 
    #      Non-Traditional CHaP                                     
    #T26(56,IY,IS)=CGOTGEN(MNUMNR,IY,2)
    z[56] = dfd['CGOTGEN'].loc[MNUMNR].loc[2]
 
    #        Total                                                  
    #T26(57,IY,IS)=FSUM(T26(55,IY,IS),2)
    z[57] = z[55]+z[56]
    
    return z
 
