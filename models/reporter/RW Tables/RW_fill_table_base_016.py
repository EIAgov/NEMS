# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""

def fill_table_base_016(dfd, table_spec, table_id):
    """Fill table for Renewable Energy Generating Capacity and Generation
   
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
      
       
    """
    
    z = {}
    
    MNUMNR=dfd['MNUMNR_rwpre']


    #   Renewable Energy Generating Capacity and Generation
    #   (gigawatts, unless otherwise noted)
    #    Net Summer Capacity and Generation                         

    #   Electric Power Sector 1/                                    
    #    Net Summer Capacity                                        
 
    #      Conventional Hydroelectric Power                         
    #T16(1,IY,IS)=UCAPHYU(MNUMNR,IY)+UCAPHYN(MNUMNR,IY)+UCAPHYC(MNUMNR,IY)
    z[1] = dfd['UCAPHYU'].loc[MNUMNR] + dfd['UCAPHYN'].loc[MNUMNR] + dfd['UCAPHYC'].loc[MNUMNR]
 
    #      Geothermal 2/                                            
    #T16(2,IY,IS)=UCAPGEU(MNUMNR,IY)+UCAPGEN(MNUMNR,IY)+UCAPGEC(MNUMNR,IY)
    z[2] = dfd['UCAPGEU'].loc[MNUMNR] + dfd['UCAPGEN'].loc[MNUMNR] + dfd['UCAPGEC'].loc[MNUMNR]
 
    #      Municipal Waste 3/                                       
    #T16(3,IY,IS)=UCAPMSU(MNUMNR,IY)+UCAPMSN(MNUMNR,IY)+UCAPMSC(MNUMNR,IY)
    z[3] = dfd['UCAPMSU'].loc[MNUMNR] + dfd['UCAPMSN'].loc[MNUMNR] + dfd['UCAPMSC'].loc[MNUMNR]
 
    #        Landfill Gas-to-Energy                                 
 
    #        Other Municipal Waste                                  
 
    #      Wood and Other Biomass 4/                                
    #T16(4,IY,IS)=UCAPWDU(MNUMNR,IY)+UCAPWDN(MNUMNR,IY)+UCAPWDC(MNUMNR,IY)
    z[4] = dfd['UCAPWDU'].loc[MNUMNR] + dfd['UCAPWDN'].loc[MNUMNR] + dfd['UCAPWDC'].loc[MNUMNR] + dfd["UCAPBIU"].loc[MNUMNR] + dfd["UCAPBIN"].loc[MNUMNR]
 
    #      Solar Photovoltaic 5/                                    
    #T16(6,IY,IS)=UCAPPVU(MNUMNR,IY)+UCAPPVN(MNUMNR,IY)+UCAPPVC(MNUMNR,IY)+UCAPPTU(MNUMNR,IY)+UCAPPTN(MNUMNR,IY)+UCAPPTC(MNUMNR,IY)
    z[6] = dfd['UCAPPVU'].loc[MNUMNR] + dfd['UCAPPVN'].loc[MNUMNR] + dfd['UCAPPVC'].loc[MNUMNR] + dfd['UCAPPTU'].loc[MNUMNR] + dfd['UCAPPTN'].loc[MNUMNR] + dfd['UCAPPTC'].loc[MNUMNR] + dfd['UCAPSTU'].loc[MNUMNR] + dfd['UCAPSTN'].loc[MNUMNR] + dfd['UCAPSTC'].loc[MNUMNR]
 
    #      Wind                                                     
    #T16(7,IY,IS)=UCAPWNU(MNUMNR,IY)+UCAPWNN(MNUMNR,IY)+UCAPWNC(MNUMNR,IY)+UCAPWLU(MNUMNR,IY)+UCAPWLN(MNUMNR,IY)+UCAPWLC(MNUMNR,IY)
    z[7] = dfd['UCAPWNU'].loc[MNUMNR] + dfd['UCAPWNN'].loc[MNUMNR] + dfd['UCAPWNC'].loc[MNUMNR] + dfd['UCAPWLU'].loc[MNUMNR] + dfd['UCAPWLN'].loc[MNUMNR] + dfd['UCAPWLC'].loc[MNUMNR]
 
    #      Offshore Wind                                            
    #T16(8,IY,IS)=UCAPWFU(MNUMNR,IY)+UCAPWFN(MNUMNR,IY)+UCAPWFC(MNUMNR,IY)
    z[8] = dfd['UCAPWFU'].loc[MNUMNR] + dfd['UCAPWFN'].loc[MNUMNR] + dfd['UCAPWFC'].loc[MNUMNR]
 
    #        Total Electric Power Sector Capacity                   
    #T16(9,IY,IS)=FSUM(T16(1,IY,IS),8)
    z[9] = z[1]+z[2]+z[3]+z[4]+z[6]+z[7]+z[8]

    #                                                               

    #    Generation (billion kilowatthours)                         
 
    #      Conventional Hydroelectric Power                         
    #T16(10,IY,IS)=UGNHYNR(1,MNUMNR,IY)+UGNHYNR(2,MNUMNR,IY)+(CGNTGEN(MNUMNR,IY,4,1)+CGNTGEN(MNUMNR,IY,4].loc[2])*0.001
    z[10] = dfd['UGNHYNR'].loc[1].loc[MNUMNR] + dfd['UGNHYNR'].loc[2].loc[MNUMNR] + ( dfd['CGNTGEN'].loc[MNUMNR].loc[4].loc[1] + dfd['CGNTGEN'].loc[MNUMNR].loc[4].loc[2] ) * 0.001
 
    #      Geothermal 2/                                            
    #T16(11,IY,IS)=UGNGENR(1,MNUMNR,IY)+UGNGENR(2,MNUMNR,IY)+(CGNTGEN(MNUMNR,IY,5,1)+CGNTGEN(MNUMNR,IY,5].loc[2])*0.001
    z[11] = dfd['UGNGENR'].loc[1].loc[MNUMNR] + dfd['UGNGENR'].loc[2].loc[MNUMNR] +  ( dfd['CGNTGEN'].loc[MNUMNR].loc[5].loc[1] + dfd['CGNTGEN'].loc[MNUMNR].loc[5].loc[2] ) * 0.001
 
    #      Biogenic Municipal Waste 6/                              
    #T16(12,IY,IS)=UGNMSNR(1,MNUMNR,IY)+UGNMSNR(2,MNUMNR,IY)+(CGNTGEN(MNUMNR,IY,6,1)+CGNTGEN(MNUMNR,IY,6].loc[2])*0.001
    z[12] = dfd['UGNMSNR'].loc[1].loc[MNUMNR] + dfd['UGNMSNR'].loc[2].loc[MNUMNR] + ( dfd['CGNTGEN'].loc[MNUMNR].loc[6].loc[1] + dfd['CGNTGEN'].loc[MNUMNR].loc[6].loc[2] ) * 0.001
 
    #        Landfill Gas-to-Energy                                 
 
    #        Other Municipal Solid Waste                            
 
    #      Wood and Other Biomass                                   
    #T16(13,IY,IS)=UGNWDNR(1,MNUMNR,IY)+UGNWDNR(2,MNUMNR,IY)+(CGNTGEN(MNUMNR,IY,7,1)+CGNTGEN(MNUMNR,IY,7].loc[2])*0.001
    z[13] = dfd['UGNWDNR'].loc[1].loc[MNUMNR] + dfd['UGNWDNR'].loc[2].loc[MNUMNR] + ( dfd['CGNTGEN'].loc[MNUMNR].loc[7].loc[1] + dfd['CGNTGEN'].loc[MNUMNR].loc[7].loc[2] ) * 0.001
 
    #         Dedicated Plants                                      
    #T16(14,IY,IS)=UGNWDNR(1,MNUMNR,IY)+UGNWDNR(2,MNUMNR,IY)+(CGNTGEN(MNUMNR,IY,7,1)+CGNTGEN(MNUMNR,IY,7].loc[2])*0.001-UGNCFNR(1,MNUMNR,IY)-UGNCFNR(2,MNUMNR,IY)
    z[14] = dfd['UGNWDNR'].loc[1].loc[MNUMNR] + dfd['UGNWDNR'].loc[2].loc[MNUMNR] +  ( dfd['CGNTGEN'].loc[MNUMNR].loc[7].loc[1] + dfd['CGNTGEN'].loc[MNUMNR].loc[7].loc[2] ) * 0.001 - dfd['UGNCFNR'].loc[1].loc[MNUMNR] - dfd['UGNCFNR'].loc[2].loc[MNUMNR]
 
    #         Cofiring                                              
    #T16(15,IY,IS)=UGNCFNR(1,MNUMNR,IY)+UGNCFNR(2,MNUMNR,IY)
    z[15] = dfd['UGNCFNR'].loc[1].loc[MNUMNR] + dfd['UGNCFNR'].loc[2].loc[MNUMNR]
 
 
    #      Solar Photovoltaic 5/      off a little in 2007,2009  
    #T16(17,IY,IS) = UGNPVNR(1,mnumnr,IY) + UGNPVNR(2,mnumnr,IY) + UGNPTNR(1,mnumnr,IY) + UGNPTNR(2,mnumnr,IY) + (CGNTGEN(mnumnr,IY, 8,1) + CGNTGEN(mnumnr,IY, 8,2)) * 0.001    
    z[17] = dfd['UGNPVNR'].loc[1].loc[MNUMNR] + dfd['UGNPVNR'].loc[2].loc[MNUMNR] + dfd['UGNPTNR'].loc[1].loc[MNUMNR] + dfd['UGNPTNR'].loc[2].loc[MNUMNR] + (dfd['CGNTGEN'].loc[MNUMNR].loc[8].loc[1] + dfd['CGNTGEN'].loc[MNUMNR].loc[8].loc[2] + dfd['UGNSONR'].loc[1].loc[MNUMNR] + dfd['UGNSONR'].loc[2].loc[MNUMNR]) * 0.001 
    
    #      Wind                                                     
    #T16(18,IY,IS)=UGNWNNR(1,MNUMNR,IY)+UGNWNNR(2,MNUMNR,IY)+UGNWLNR(1,MNUMNR,IY)+UGNWLNR(2,MNUMNR,IY)
    z[18] = dfd['UGNWNNR'].loc[1].loc[MNUMNR] + dfd['UGNWNNR'].loc[2].loc[MNUMNR] + dfd['UGNWLNR'].loc[1].loc[MNUMNR] + dfd['UGNWLNR'].loc[2].loc[MNUMNR]
 
    #      Offshore Wind                                            
    #T16(19,IY,IS)=UGNWFNR(1,MNUMNR,IY)+UGNWFNR(2,MNUMNR,IY)
    z[19] = dfd['UGNWFNR'].loc[1].loc[MNUMNR] + dfd['UGNWFNR'].loc[2].loc[MNUMNR]
 
    #        Total Electric Power Sector Generation                 
    #T16(20,IY,IS)=FSUM(T16(10,IY,IS),4)+FSUM(T16(16,IY,IS),4)
    z[20] = z[10]+z[11]+z[12]+z[13] +z[17]+z[18]+z[19]
 
    #                                                               
 
    #   End-Use Sectors 7/                                          
 
    #    Net Summer Capacity                                        
 
    #      Conventional Hydroelectric Power                         
    #T16(21,IY,IS)=(CGCOMMCAP(11,IY,4)+CGINDLCAP(11,IY,4))/1000.
    z[21] = (dfd['CGCOMMCAP'].loc[11].loc[4] + dfd['CGINDLCAP'].loc[11].loc[4]) / 1000.
 
    #      Geothermal                                               
    #T16(29,IY,IS)=(CGCOMMCAP(11,IY,5)+CGINDLCAP(11,IY,5))/1000.
    z[29] = (dfd['CGCOMMCAP'].loc[11].loc[5] + dfd['CGINDLCAP'].loc[11].loc[5]) / 1000.
 
    #      Municipal Waste 3/                                       
    #T16(22,IY,IS)=(CGCOMMCAP(11,IY,6)+CGREFCAP(11,IY,6)+CGINDLCAP(11,IY,6))/1000.
    z[22] = (dfd['CGCOMMCAP'].loc[11].loc[6] + dfd['CGREFCAP'].loc[11].loc[6] + dfd['CGINDLCAP'].loc[11].loc[6]) / 1000.
 
    #      Biomass                                                  
    #T16(23,IY,IS)=(CGCOMMCAP(11,IY,7)+CGREFCAP(11,IY,7)+CGINDLCAP(11,IY,7))/1000.
    z[23] = (dfd['CGCOMMCAP'].loc[11].loc[7] + dfd['CGREFCAP'].loc[11].loc[7] + dfd['CGINDLCAP'].loc[11].loc[7]) / 1000.
 
    #      Solar Photovoltaic 5/                                    
    #T16(30,IY,IS)=(CGCOMMCAP(11,IY,8)+CGRESCAP(11,IY,8)+CGINDLCAP(11,IY,8))/1000.
    z[30] = (dfd['CGCOMMCAP'].loc[11].loc[8] + dfd['CGRESCAP'].loc[11].loc[8] + dfd['CGINDLCAP'].loc[11].loc[8]) / 1000.
 
    #      Wind                                                     
    #T16(35,IY,IS)=(CGCOMMCAP(11,IY,11)+CGRESCAP(11,IY,11)+CGINDLCAP(11,IY,11))/1000.
    z[35] = (dfd['CGCOMMCAP'].loc[11].loc[11] + dfd['CGRESCAP'].loc[11].loc[11] + dfd['CGINDLCAP'].loc[11].loc[11]) / 1000.
 
    #        Total End-Use Sector Capacity                          
    #T16(31,IY,IS)=FSUM(T16(21,IY,IS),3)+FSUM(T16(29,IY,IS)].loc[2]+T16(35,IY,IS)
    z[31] = z[21]+z[22]+z[23] + z[29]+z[30] + z[35]
 
    #                                                               


 
    #    Generation (billion kilowatthours)                         


 
    #      Conventional Hydroelectric Power                         
    #T16(25,IY,IS)=(CGCOMMGEN(11,IY,4,1)+CGCOMMGEN(11,IY,4].loc[2]+CGINDLGEN(11,IY,4,1)+CGINDLGEN(11,IY,4].loc[2])*.001
    z[25] = (dfd['CGCOMMGEN'].loc[11].loc[4].loc[1] + dfd['CGCOMMGEN'].loc[11].loc[4].loc[2] + dfd['CGINDLGEN'].loc[11].loc[4].loc[1] + dfd['CGINDLGEN'].loc[11].loc[4].loc[2]) * .001
 
    #      Geothermal                                               
    #T16(32,IY,IS)=(CGCOMMGEN(11,IY,5,1)+CGCOMMGEN(11,IY,5].loc[2]+CGINDLGEN(11,IY,5,1)+CGINDLGEN(11,IY,5].loc[2])*.001
    z[32] = (dfd['CGCOMMGEN'].loc[11].loc[5].loc[1] + dfd['CGCOMMGEN'].loc[11].loc[5].loc[2] + dfd['CGINDLGEN'].loc[11].loc[5].loc[1] + dfd['CGINDLGEN'].loc[11].loc[5].loc[2]) * .001
 
    #      Municipal Waste 3/                                       
    #T16(26,IY,IS)=(CGCOMMGEN(11,IY,6,1)+CGCOMMGEN(11,IY,6].loc[2]+CGREFGEN(11,IY,6,1)+CGREFGEN(11,IY,6].loc[2]+CGINDLGEN(11,IY,6,1)+CGINDLGEN(11,IY,6].loc[2])*.001
    z[26] = (dfd['CGCOMMGEN'].loc[11].loc[6].loc[1] + dfd['CGCOMMGEN'].loc[11].loc[6].loc[2] + dfd['CGREFGEN'].loc[11].loc[6].loc[1] + dfd['CGREFGEN'].loc[11].loc[6].loc[2] + dfd['CGINDLGEN'].loc[11].loc[6].loc[1] + dfd['CGINDLGEN'].loc[11].loc[6].loc[2]) * .001
 
    #      Biomass                                                  
    #T16(27,IY,IS)=(CGCOMMGEN(11,IY,7,1)+CGCOMMGEN(11,IY,7].loc[2]+CGREFGEN(11,IY,7,1)+CGREFGEN(11,IY,7].loc[2]+CGINDLGEN(11,IY,7,1)+CGINDLGEN(11,IY,7].loc[2])*.001
    z[27] = (dfd['CGCOMMGEN'].loc[11].loc[7].loc[1] + dfd['CGCOMMGEN'].loc[11].loc[7].loc[2] + dfd['CGREFGEN'].loc[11].loc[7].loc[1] + dfd['CGREFGEN'].loc[11].loc[7].loc[2] + dfd['CGINDLGEN'].loc[11].loc[7].loc[1] + dfd['CGINDLGEN'].loc[11].loc[7].loc[2]) * .001
 
    #      Solar Photovoltaic 5/                                    
    #T16(33,IY,IS)=(CGCOMMGEN(11,IY,8,1)+CGCOMMGEN(11,IY,8].loc[2]+CGRESGEN(11,IY,8,1)+CGRESGEN(11,IY,8].loc[2]+CGINDLGEN(11,IY,8,1)+CGINDLGEN(11,IY,8].loc[2])*.001
    z[33] = (dfd['CGCOMMGEN'].loc[11].loc[8].loc[1] + dfd['CGCOMMGEN'].loc[11].loc[8].loc[2] + dfd['CGRESGEN'].loc[11].loc[8].loc[1] + dfd['CGRESGEN'].loc[11].loc[8].loc[2] + dfd['CGINDLGEN'].loc[11].loc[8].loc[1] + dfd['CGINDLGEN'].loc[11].loc[8].loc[2]) * .001
 
    #      Wind                                                     
    #T16(36,IY,IS)=(CGCOMMGEN(11,IY,11,1)+CGCOMMGEN(11,IY,11].loc[2]+CGRESGEN(11,IY,11,1)+CGRESGEN(11,IY,11].loc[2]+CGINDLGEN(11,IY,11,1)+CGINDLGEN(11,IY,11].loc[2])*.001
    z[36] = (dfd['CGCOMMGEN'].loc[11].loc[11].loc[1] + dfd['CGCOMMGEN'].loc[11].loc[11].loc[2] + dfd['CGRESGEN'].loc[11].loc[11].loc[1] + dfd['CGRESGEN'].loc[11].loc[11].loc[2] + dfd['CGINDLGEN'].loc[11].loc[11].loc[1] + dfd['CGINDLGEN'].loc[11].loc[11].loc[2]) * .001
 
    #        Total End-Use Sector Generation                        
    #T16(34,IY,IS)=FSUM(T16(25,IY,IS),3)+FSUM(T16(32,IY,IS)].loc[2]+T16(36,IY,IS)
    z[34] = z[25]+z[26]+z[27] + z[32]+z[33] + z[36]

    #                                                               
 
    #                                                               
 
    #                                                               
 
    #   All Sectors                                                 
 
    #     Net Summer Capacity                                       
 
    #       Conventional Hydroelectric Power                        
    #T16(39,IY,IS)=T16(1,IY,IS)+T16(21,IY,IS)
    z[39] = z[1]+z[21]
 
    #       Geothermal                                              
    #T16(40,IY,IS)=T16(2,IY,IS)+T16(29,IY,IS)
    z[40] = z[2]+z[29]
 
    #       Municipal Waste 3/                                      
    #T16(41,IY,IS)=T16(3,IY,IS)+T16(22,IY,IS)
    z[41] = z[3]+z[22]
 
    #       Wood and Other Biomass 4/                               
    #T16(42,IY,IS)=T16(4,IY,IS)+T16(23,IY,IS)
    z[42] = z[4]+z[23]
 
    #       Solar 5/                                                
    #T16(43,IY,IS)=FSUM(T16(5,IY,IS)].loc[2]+T16(30,IY,IS)
    z[43] = z[6] + z[30]
 
    #       Wind                                                    
    #T16(44,IY,IS)=FSUM(T16(7,IY,IS)].loc[2]+T16(35,IY,IS)
    z[44] = z[7]+z[8] + z[35]
 
    #         Total Capacity, All Sectors                           
    #T16(45,IY,IS)=FSUM(T16(39,IY,IS),6)
    z[45] = z[39]+z[40]+z[41]+z[42]+z[43]+z[44]
 
    #                                                               
 
    #     Generation (billion kilowatthours)                        
 
    #       Conventional Hydroelectric Power                        
    #T16(46,IY,IS)=T16(10,IY,IS)+T16(25,IY,IS)
    z[46] = z[10]+z[25]
 
    #       Geothermal                                              
    #T16(47,IY,IS)=T16(11,IY,IS)+T16(32,IY,IS)
    z[47] = z[11]+z[32]
 
    #       Municipal Waste 8/                                      
    #T16(48,IY,IS)=T16(12,IY,IS)+T16(26,IY,IS)
    z[48] = z[12]+z[26]
 
    #       Wood and Other Biomass                                  
    #T16(49,IY,IS)=T16(13,IY,IS)+T16(27,IY,IS)
    z[49] = z[13]+z[27]
 
    #       Solar 5/                                                
    #T16(50,IY,IS)=FSUM(T16(16,IY,IS)].loc[2]+T16(33,IY,IS)
    z[50] = z[17] + z[33]
 
    #       Wind                                                    
    #T16(51,IY,IS)=FSUM(T16(18,IY,IS)].loc[2]+T16(36,IY,IS)
    z[51] = z[18]+z[19] + z[36]
 
    #         Total Generation, All Sectors                         
    #T16(52,IY,IS)=FSUM(T16(46,IY,IS),6)
    z[52] = z[46]+z[47]+z[48]+z[49]+z[50]+z[51]
    
    return z
