#-*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_137(dfd, table_spec, table_id):
    """Fill table   Key Results for Renewable Energy Technology Cases   
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
        
    MNUMNR =  dfd['MNUMNR_rwpre'] 
    C_CO2_FACTOR=dfd['C_CO2_FACTOR_rwpre']
 
    #   Key Results for Renewable Energy Technology Cases
    #    Capacity, Generation, and Emissions                        
    #                                                               
    #   Net Summer Capacity (gigawatts)                             
 
    #    Electric Power Sector                                      
 
    #     Conventional Hydroelectric Power                          
    #T137(1,IY,IS)=UCAPHYU(MNUMNR,IY)+UCAPHYN(MNUMNR,IY)+UCAPHYC(MNUMNR,IY)
    z[1] = dfd['UCAPHYU'].loc[MNUMNR] + dfd['UCAPHYN'].loc[MNUMNR] + dfd['UCAPHYC'].loc[MNUMNR]
 
    #     Geothermal                                                
    #T137(2,IY,IS)=UCAPGEU(MNUMNR,IY)+UCAPGEN(MNUMNR,IY)+UCAPGEC(MNUMNR,IY)
    z[2] = dfd['UCAPGEU'].loc[MNUMNR] + dfd['UCAPGEN'].loc[MNUMNR] + dfd['UCAPGEC'].loc[MNUMNR]
 
    #     Municipal Waste                                           
    #T137(3,IY,IS)=UCAPMSU(MNUMNR,IY)+UCAPMSN(MNUMNR,IY)+UCAPMSC(MNUMNR,IY)
    z[3] = dfd['UCAPMSU'].loc[MNUMNR] + dfd['UCAPMSN'].loc[MNUMNR] + dfd['UCAPMSC'].loc[MNUMNR]
 
    #     Wood and Other Biomass                                    
    #T137(4,IY,IS)=UCAPWDU(MNUMNR,IY)+UCAPWDN(MNUMNR,IY)+UCAPWDC(MNUMNR,IY)
    z[4] = dfd['UCAPWDU'].loc[MNUMNR] + dfd['UCAPWDN'].loc[MNUMNR] + dfd['UCAPWDC'].loc[MNUMNR]
 
    #     Solar Thermal                                             
    #T137(5,IY,IS)=UCAPSTU(MNUMNR,IY)+UCAPSTN(MNUMNR,IY)+UCAPSTC(MNUMNR,IY)
    z[5] = dfd['UCAPSTU'].loc[MNUMNR] + dfd['UCAPSTN'].loc[MNUMNR] + dfd['UCAPSTC'].loc[MNUMNR]
 
    #     Solar Photovoltaic                                        
    #T137(6,IY,IS)=UCAPPVU(MNUMNR,IY)+UCAPPVN(MNUMNR,IY)+UCAPPVC(MNUMNR,IY)+UCAPPTU(MNUMNR,IY)+UCAPPTN(MNUMNR,IY)+UCAPPTC(MNUMNR,IY)
    z[6] = dfd['UCAPPVU'].loc[MNUMNR] + dfd['UCAPPVN'].loc[MNUMNR] + dfd['UCAPPVC'].loc[MNUMNR] + \
           dfd['UCAPPTU'].loc[MNUMNR] + dfd['UCAPPTN'].loc[MNUMNR] + dfd['UCAPPTC'].loc[MNUMNR]
 
    #     Wind                                                      
    #T137(7,IY,IS)=UCAPWNU(MNUMNR,IY)+UCAPWNN(MNUMNR,IY)+UCAPWNC(MNUMNR,IY)+UCAPWLU(MNUMNR,IY)+UCAPWLN(MNUMNR,IY)+UCAPWLC(MNUMNR,IY)+UCAPWFU(MNUMNR,IY)+UCAPWFN(MNUMNR,IY)+UCAPWFC(MNUMNR,IY)
    z[7] = dfd['UCAPWNU'].loc[MNUMNR] + dfd['UCAPWNN'].loc[MNUMNR] + dfd['UCAPWNC'].loc[MNUMNR] + \
           dfd['UCAPWLU'].loc[MNUMNR] + dfd['UCAPWLN'].loc[MNUMNR] + dfd['UCAPWLC'].loc[MNUMNR] + \
           dfd['UCAPWFU'].loc[MNUMNR] + dfd['UCAPWFN'].loc[MNUMNR] + dfd['UCAPWFC'].loc[MNUMNR]
 
    #      Total                                                    
    #T137(8,IY,IS)=FSUM(T137(1,IY,IS),7)
    z[8] = z[1]+z[2]+z[3]+z[4]+z[5]+z[6]+z[7]

    #                                                               
 
    #    End-Use Sectors                                            
 
    #     Conventional Hydroelectric Power                          
    #T137(12,IY,IS)=(CGCOMMCAP(11,IY,4)+CGINDLCAP(11,IY,4))/1000.
    z[12] = (dfd['CGCOMMCAP'].loc[11].loc[4] + dfd['CGINDLCAP'].loc[11].loc[4]) / 1000.
 
    #     Geothermal                                                
    #T137(13,IY,IS)=(CGCOMMCAP(11,IY,5)+CGINDLCAP(11,IY,5))/1000.
    z[13] = (dfd['CGCOMMCAP'].loc[11].loc[5] + dfd['CGINDLCAP'].loc[11].loc[5]) / 1000.
 
    #     Municipal Waste                                           
    #T137(9,IY,IS)=(CGCOMMCAP(11,IY,6)+C'CGREFCAP'(11,IY,6)+CGINDLCAP(11,IY,6))/1000.
    z[9] = (dfd['CGCOMMCAP'].loc[11].loc[6] + dfd['CGREFCAP'].loc[11].loc[6] + dfd['CGINDLCAP'].loc[11].loc[6]) / 1000.
 
    #     Wood and Other Biomass                                    
    #T137(10,IY,IS)=(CGCOMMCAP(11,IY,7)+C'CGREFCAP'(11,IY,7)+CGINDLCAP(11,IY,7))/1000.
    z[10] = (dfd['CGCOMMCAP'].loc[11].loc[7] + dfd['CGREFCAP'].loc[11].loc[7] + dfd['CGINDLCAP'].loc[11].loc[7]) / 1000.
 
    #     Solar Photovoltaic                                        
    #T137(14,IY,IS)=(CGCOMMCAP(11,IY,8)+CGRESCAP(11,IY,8)+CGINDLCAP(11,IY,8))/1000.
    z[14] = (dfd['CGCOMMCAP'].loc[11].loc[8] + dfd['CGRESCAP'].loc[11].loc[8] + dfd['CGINDLCAP'].loc[11].loc[8]) / 1000.
 
    #     Wind                                                      
    #T137(49,IY,IS)=(CGCOMMCAP(11,IY,11)+CGRESCAP(11,IY,11)+CGINDLCAP(11,IY,11))/1000.
    z[49] = (dfd['CGCOMMCAP'].loc[11].loc[11] + dfd['CGRESCAP'].loc[11].loc[11] + dfd['CGINDLCAP'].loc[11].loc[11]) / 1000.
 
    #      Total                                                    
    #T137(41,IY,IS)=FSUM(T137(11,IY,IS),4)+T137(49,IY,IS)
    z[41] = z[12]+z[13]+z[9]+z[10]+z[14] +z[49]
 
    #                                                               
 
    #   Generation (billion kilowatthours)                          
 
    #    Electric Power Sector                                      
 
    #     Coal                                                      
    #T137(15,IY,IS)=UGNCLNR(1,MNUMNR,IY)+UGNCLNR(2,MNUMNR,IY)+(CGNTGEN(MNUMNR,IY,1,1)+CGNTGEN(MNUMNR,IY,1,2))*0.001
    z[15] = dfd['UGNCLNR'].loc[1].loc[MNUMNR] + dfd['UGNCLNR'].loc[2].loc[MNUMNR] + \
           (dfd['CGNTGEN'].loc[MNUMNR].loc[1].loc[1] + dfd['CGNTGEN'].loc[MNUMNR].loc[1].loc[2]) * 0.001 
 
    #     Petroleum                                                 
    #T137(16,IY,IS)=UGNDSNR(1,MNUMNR,IY)+UGNRLNR(1,MNUMNR,IY)+UGNDSNR(2,MNUMNR,IY)+UGNRLNR(2,MNUMNR,IY)+UGNRHNR(1,MNUMNR,IY)+UGNRHNR(2,MNUMNR,IY)+(CGNTGEN(MNUMNR,IY,2,1)+CGNTGEN(MNUMNR,IY,2,2])*0.001
    z[16] = dfd['UGNDSNR'].loc[1].loc[MNUMNR] + dfd['UGNRLNR'].loc[1].loc[MNUMNR] + \
            dfd['UGNDSNR'].loc[2].loc[MNUMNR] + dfd['UGNRLNR'].loc[2].loc[MNUMNR] + \
            dfd['UGNRHNR'].loc[1].loc[MNUMNR] + dfd['UGNRHNR'].loc[2].loc[MNUMNR] + \
           (dfd['CGNTGEN'].loc[MNUMNR].loc[2].loc[1] + dfd['CGNTGEN'].loc[MNUMNR].loc[2].loc[2]) * 0.001 
 
    #     Natural Gas                                               
    #T137(17,IY,IS)=UGNGFNR(1,MNUMNR,IY)+UGNGINR(1,MNUMNR,IY)+UGNGFNR(2,MNUMNR,IY)+UGNGINR(2,MNUMNR,IY)+UGNGCNR(1,MNUMNR,IY)+UGNGCNR(2,MNUMNR,IY)+(CGNTGEN(MNUMNR,IY,3,1)+CGNTGEN(MNUMNR,IY,3,2))*0.001
    z[17] = dfd['UGNGFNR'].loc[1].loc[MNUMNR] + dfd['UGNGINR'].loc[1].loc[MNUMNR] + \
            dfd['UGNGFNR'].loc[2].loc[MNUMNR] + dfd['UGNGINR'].loc[2].loc[MNUMNR] + \
            dfd['UGNGCNR'].loc[1].loc[MNUMNR] + dfd['UGNGCNR'].loc[2].loc[MNUMNR] + \
           (dfd['CGNTGEN'].loc[MNUMNR].loc[3].loc[1] + dfd['CGNTGEN'].loc[MNUMNR].loc[3].loc[2]) * 0.001 
 
    #      Total Fossil 1/                                          
    #T137(18,IY,IS)=FSUM(T137(15,IY,IS),3)
    z[18] = z[15]+z[16]+z[17]

    #     Conventional Hydroelectric Power                          
    #T137(19,IY,IS)=UGNHYNR(1,MNUMNR,IY)+UGNHYNR(2,MNUMNR,IY)+(CGNTGEN(MNUMNR,IY,4,1)+CGNTGEN(MNUMNR,IY,4,2])*0.001
    z[19] = dfd['UGNHYNR'].loc[1].loc[MNUMNR] + dfd['UGNHYNR'].loc[2].loc[MNUMNR] + \
           (dfd['CGNTGEN'].loc[MNUMNR].loc[4].loc[1] + dfd['CGNTGEN'].loc[MNUMNR].loc[4].loc[2]) * 0.001 
 
    #     Geothermal                                                
    #T137(20,IY,IS)=UGNGENR(1,MNUMNR,IY)+UGNGENR(2,MNUMNR,IY)+(CGNTGEN(MNUMNR,IY,5,1)+CGNTGEN(MNUMNR,IY,5,2))*0.001
    z[20] = dfd['UGNGENR'].loc[1].loc[MNUMNR] + dfd['UGNGENR'].loc[2].loc[MNUMNR] + \
           (dfd['CGNTGEN'].loc[MNUMNR].loc[5].loc[1] + dfd['CGNTGEN'].loc[MNUMNR].loc[5].loc[2]) * 0.001 
 
    #     Municipal Waste                                           
    #T137(21,IY,IS)=UGNMSNR(1,MNUMNR,IY)+UGNMSNR(2,MNUMNR,IY)+(CGNTGEN(MNUMNR,IY,6,1)+CGNTGEN(MNUMNR,IY,6,2))*0.001
    z[21] = dfd['UGNMSNR'].loc[1].loc[MNUMNR] + dfd['UGNMSNR'].loc[2].loc[MNUMNR] + \
           (dfd['CGNTGEN'].loc[MNUMNR].loc[6].loc[1] + dfd['CGNTGEN'].loc[MNUMNR].loc[6].loc[2]) * 0.001 
 
    #     Wood and Other Biomass                                    
    #T137(22,IY,IS)=UGNWDNR(1,MNUMNR,IY)+UGNWDNR(2,MNUMNR,IY)+(CGNTGEN(MNUMNR,IY,7,1)+CGNTGEN(MNUMNR,IY,7,2))*0.001
    z[22] = dfd['UGNWDNR'].loc[1].loc[MNUMNR] + dfd['UGNWDNR'].loc[2].loc[MNUMNR] + \
           (dfd['CGNTGEN'].loc[MNUMNR].loc[7].loc[1] + dfd['CGNTGEN'].loc[MNUMNR].loc[7].loc[2]) * 0.001 
 
    #       Dedicated Plants                                        
    #T137(43,IY,IS)=UGNWDNR(1,MNUMNR,IY)+UGNWDNR(2,MNUMNR,IY)+(CGNTGEN(MNUMNR,IY,7,1)+CGNTGEN(MNUMNR,IY,7,2))*0.001-UGNCFNR(1,MNUMNR,IY)-UGNCFNR(2,MNUMNR,IY)
    z[43] = dfd['UGNWDNR'].loc[1].loc[MNUMNR] + dfd['UGNWDNR'].loc[2].loc[MNUMNR] + \
           (dfd['CGNTGEN'].loc[MNUMNR].loc[7].loc[1] + dfd['CGNTGEN'].loc[MNUMNR].loc[7].loc[2]) * 0.001 - \
            dfd['UGNCFNR'].loc[1].loc[MNUMNR] - dfd['UGNCFNR'].loc[2].loc[MNUMNR]
 
    #       Cofiring                                                
    #T137(44,IY,IS)=UGNCFNR(1,MNUMNR,IY)+UGNCFNR(2,MNUMNR,IY)
    z[44] = dfd['UGNCFNR'].loc[1].loc[MNUMNR] + dfd['UGNCFNR'].loc[2].loc[MNUMNR]
 
    #     Solar Thermal                                             
    #T137(23,IY,IS)=UGNSONR(1,MNUMNR,IY)+UGNSONR(2,MNUMNR,IY)
    z[23] = dfd['UGNSONR'].loc[1].loc[MNUMNR] + dfd['UGNSONR'].loc[2].loc[MNUMNR]
 
    #     Solar Photovoltaic                                        
    #T137(24,IY,IS)=UGNPVNR(1,MNUMNR,IY)+UGNPVNR(2,MNUMNR,IY)+UGNPTNR(1,MNUMNR,IY)+UGNPTNR(2,MNUMNR,IY)+(CGNTGEN(MNUMNR,IY,8,1)+CGNTGEN(MNUMNR,IY,8,2))*0.001
    z[24] = dfd['UGNPVNR'].loc[1].loc[MNUMNR] + dfd['UGNPVNR'].loc[2].loc[MNUMNR] + \
            dfd['UGNPTNR'].loc[1].loc[MNUMNR] + dfd['UGNPTNR'].loc[2].loc[MNUMNR] + \
           (dfd['CGNTGEN'].loc[MNUMNR].loc[8].loc[1] + dfd['CGNTGEN'].loc[MNUMNR].loc[8].loc[2]) * 0.001 
 
    #     Wind                                                      
    #T137(25,IY,IS)=UGNWNNR(1,MNUMNR,IY)+UGNWNNR(2,MNUMNR,IY)+UGNWLNR(1,MNUMNR,IY)+UGNWLNR(2,MNUMNR,IY)+UGNWFNR(1,MNUMNR,IY)+UGNWFNR(2,MNUMNR,IY)
    z[25] = dfd['UGNWNNR'].loc[1].loc[MNUMNR] + dfd['UGNWNNR'].loc[2].loc[MNUMNR] + \
            dfd['UGNWLNR'].loc[1].loc[MNUMNR] + dfd['UGNWLNR'].loc[2].loc[MNUMNR] + \
            dfd['UGNWFNR'].loc[1].loc[MNUMNR] + dfd['UGNWFNR'].loc[2].loc[MNUMNR]
 
    #      Total Renewable                                          
    #T137(26,IY,IS)=FSUM(T137(19,IY,IS),4)+FSUM(T137(23,IY,IS),3)
    z[26] = z[19]+z[20]+z[21]+z[22] +z[23]+z[24]+z[25]

    #                                                               
 
    # !        --- SELECTED FOSSIL FUEL'S COGENERATOR GENERATION
    # T137(27,IY,IS) = (CGOGSGEN(11,IY,1,1)  + CGOGSGEN(11,IY,1,2) +CGREFGEN(11,IY,1,1)  + CGREFGEN(11,IY,1,2) + &
                           # CGINDLGEN(11,IY,1,1) + CGINDLGEN(11,IY,1,2) +  CGCOMMGEN(11,IY,1,1) + CGCOMMGEN(11,IY,1,2)) * .001
    z[27]= (dfd['CGOGSGEN'].loc[11,1,1]  + dfd['CGOGSGEN'].loc[11,1,2] + dfd['CGREFGEN'].loc[11,1,1] + dfd['CGREFGEN'].loc[11,1,2] + dfd['CGINDLGEN'].loc[11,1,1] + dfd['CGINDLGEN'].loc[11,1,2] + dfd['CGCOMMGEN'].loc[11,1,1] + dfd['CGCOMMGEN'].loc[11,1,2]) * .001
    # T137(28,IY,IS) = (CGREFGEN(11,IY,2,1)  + CGREFGEN(11,IY,2,2) +  CGOGSGEN(11,IY,2,1)  + CGOGSGEN(11,IY,2,2) + &
                           # CGINDLGEN(11,IY,2,1) + CGINDLGEN(11,IY,2,2) + CGCOMMGEN(11,IY,2,1) + CGCOMMGEN(11,IY,2,2)) * .001
    z[28]= (dfd['CGRESGEN'].loc[11,2,1]  + dfd['CGRESGEN'].loc[11,2,2] + dfd['CGOGSGEN'].loc[11,2,1]  + dfd['CGOGSGEN'].loc[11,2,2] + dfd['CGREFGEN'].loc[11,2,1] + \
            dfd['CGREFGEN'].loc[11,2,2] + dfd['CGINDLGEN'].loc[11,2,1] + dfd['CGINDLGEN'].loc[11,2,2] + dfd['CGCOMMGEN'].loc[11,2,1] + dfd['CGCOMMGEN'].loc[11,2,2]) * .001
    
    
    
    # T137(29,IY,IS) = (CGREFGEN(11,IY,3,1)  + CGREFGEN(11,IY,3,2) +  CGOGSGEN(11,IY,3,1)  + CGOGSGEN(11,IY,3,2) + &
                           # CGINDLGEN(11,IY,3,1) + CGINDLGEN(11,IY,3,2) +CGCOMMGEN(11,IY,3,1) + CGCOMMGEN(11,IY,3,2) + &
                           # CGRESGEN(11,IY,3,1)  + CGRESGEN(11,IY,3,2)) * .001                                                               
    z[29]= (dfd['CGREFGEN'].loc[11,3,1] + dfd['CGREFGEN'].loc[11,3,2] + dfd['CGRESGEN'].loc[11,3,1]  + dfd['CGRESGEN'].loc[11,3,2] +  \
            dfd['CGOGSGEN'].loc[11,3,1]  + dfd['CGOGSGEN'].loc[11,3,2] + \
            dfd['CGINDLGEN'].loc[11,3,1] + dfd['CGINDLGEN'].loc[11,3,2] + dfd['CGCOMMGEN'].loc[11,3,1] + dfd['CGCOMMGEN'].loc[11,3,2]) * .001
    #    End-Use Sectors                                            
 
    #        Total Fossil 1/                                        
    #T137(30,IY,IS)=FSUM(T137(27,IY,IS),3)
    z[30] = z[27]+z[28]+z[29]

    #                                                               
 
    #      Conventional Hydroelectric Power                         
    #T137(34,IY,IS)=(CGCOMMGEN(11,IY,4,1)+CGCOMMGEN(11,IY,4,2)+CGINDLGEN(11,IY,4,1)+CGINDLGEN(11,IY,4,2))*.001
    z[34] = (dfd['CGCOMMGEN'].loc[11].loc[4].loc[1] + dfd['CGCOMMGEN'].loc[11].loc[4].loc[2] + \
             dfd['CGINDLGEN'].loc[11].loc[4].loc[1] + dfd['CGINDLGEN'].loc[11].loc[4].loc[2])*.001
 
    #      Geothermal                                               
    #T137(35,IY,IS)=(CGCOMMGEN(11,IY,5,1)+CGCOMMGEN(11,IY,5,2)+CGINDLGEN(11,IY,5,1)+CGINDLGEN(11,IY,5,2))*.001
    z[35] = (dfd['CGCOMMGEN'].loc[11].loc[5].loc[1] + dfd['CGCOMMGEN'].loc[11].loc[5].loc[2] + \
             dfd['CGINDLGEN'].loc[11].loc[5].loc[1] + dfd['CGINDLGEN'].loc[11].loc[5].loc[2])*.001
 
    #      Municipal Waste                                          
    #T137(31,IY,IS)=(CGCOMMGEN(11,IY,6,1)+CGCOMMGEN(11,IY,6,2)+CGREFGEN(11,IY,6,1)+CGREFGEN(11,IY,6,2)+CGINDLGEN(11,IY,6,1)+CGINDLGEN(11,IY,6,2))*.001
    z[31] = (dfd['CGCOMMGEN'].loc[11].loc[6].loc[1] + dfd['CGCOMMGEN'].loc[11].loc[6].loc[2] + \
             dfd['CGREFGEN'].loc[11].loc[6].loc[1] + dfd['CGREFGEN'].loc[11].loc[6].loc[2] + \
             dfd['CGINDLGEN'].loc[11].loc[6].loc[1] + dfd['CGINDLGEN'].loc[11].loc[6].loc[2])*.001
 
    #      Wood and Other Biomass                                   
    #T137(32,IY,IS)=(CGCOMMGEN(11,IY,7,1)+CGCOMMGEN(11,IY,7,2)+CGREFGEN(11,IY,7,1)+CGREFGEN(11,IY,7,2)+CGINDLGEN(11,IY,7,1)+CGINDLGEN(11,IY,7,2))*.001
    z[32] = (dfd['CGCOMMGEN'].loc[11].loc[7].loc[1] + dfd['CGCOMMGEN'].loc[11].loc[7].loc[2] + \
             dfd['CGREFGEN'].loc[11].loc[7].loc[1] + dfd['CGREFGEN'].loc[11].loc[7].loc[2] + \
             dfd['CGINDLGEN'].loc[11].loc[7].loc[1] + dfd['CGINDLGEN'].loc[11].loc[7].loc[2])*.001
 
    #      Solar Photovoltaic                                       
    #T137(36,IY,IS)=(CGCOMMGEN(11,IY,8,1)+CGCOMMGEN(11,IY,8,2)+CGRESGEN(11,IY,8,1)+CGRESGEN(11,IY,8,2)+CGINDLGEN(11,IY,8,1)+CGINDLGEN(11,IY,8,2))*.001
    z[36] = (dfd['CGCOMMGEN'].loc[11].loc[8].loc[1] + dfd['CGCOMMGEN'].loc[11].loc[8].loc[2] + \
             dfd['CGRESGEN'].loc[11].loc[8].loc[1] + dfd['CGRESGEN'].loc[11].loc[8].loc[2] + \
             dfd['CGINDLGEN'].loc[11].loc[8].loc[1] + dfd['CGINDLGEN'].loc[11].loc[8].loc[2])*.001
 
    #      Wind                                                     
    #T137(50,IY,IS)=(CGCOMMGEN(11,IY,11,1)+CGCOMMGEN(11,IY,11,2)+CGRESGEN(11,IY,11,1)+CGRESGEN(11,IY,11,2)+CGINDLGEN(11,IY,11,1)+CGINDLGEN(11,IY,11,2))*.001
    z[50] = (dfd['CGCOMMGEN'].loc[11].loc[11].loc[1] + dfd['CGCOMMGEN'].loc[11].loc[11].loc[2] + \
             dfd['CGRESGEN'].loc[11].loc[11].loc[1] + dfd['CGRESGEN'].loc[11].loc[11].loc[2] + \
             dfd['CGINDLGEN'].loc[11].loc[11].loc[1] + dfd['CGINDLGEN'].loc[11].loc[11].loc[2])*.001
 
    #        Total Renewable                                        
    #T137(42,IY,IS)=FSUM(T137(33,IY,IS),4)+T137(50,IY,IS)
   
    z[42] = z[34]+z[35]+z[36] +z[50]+ z[32] + z[31]
                                                              
 
    #   Sources of Ethanol (million barrels per day)                
 
    #     from Corn                                                 
    #T137(44,IY,IS)=UGNCFNR(1,MNUMNR,IY)+UGNCFNR(2,MNUMNR,IY)
    z[45] = dfd['UGNCFNR'].loc[1].loc[MNUMNR] + dfd['UGNCFNR'].loc[2].loc[MNUMNR]
 
    #     from Cellulose                                            
    #T137(44,IY,IS)=UGNCFNR(1,MNUMNR,IY)+UGNCFNR(2,MNUMNR,IY)
    z[46] = dfd['UGNCFNR'].loc[1].loc[MNUMNR] + dfd['UGNCFNR'].loc[2].loc[MNUMNR]
 
    #       Total                                                   
    #T137(44,IY,IS)=UGNCFNR(1,MNUMNR,IY)+UGNCFNR(2,MNUMNR,IY)
    z[47] = dfd['UGNCFNR'].loc[1].loc[MNUMNR] + dfd['UGNCFNR'].loc[2].loc[MNUMNR]
 
    #                                                               
 
    #   Electric Power Sector Carbon Dioxide Emissions              
 
    #   (million metric tons carbon_dd_ equivalent)                 
 
    #       Coal                                                    
    #T137(39,IY,IS)=EMEL(3,1,IY)
    z[39] = dfd['EMEL'].loc[3].loc[1]*C_CO2_FACTOR
 
    #       Petroleum                                               
    #T137(37,IY,IS)=EMEL(2,1,IY)
    z[37] = dfd['EMEL'].loc[2].loc[1]*C_CO2_FACTOR
 
    #       Natural Gas                                             
    #T137(38,IY,IS)=EMEL(1,1,IY)
    z[38] = dfd['EMEL'].loc[1].loc[1]*C_CO2_FACTOR
 
    #       Other                                                   
    #T137(48,IY,IS)=EMEL(4,1,IY)
    z[48] = dfd['EMEL'].loc[4].loc[1]*C_CO2_FACTOR
 
    #         Total                                                 
    #T137(40,IY,IS)=SUM(EMEL(1:4,1,IY))
    z[40] = z[39] + z[37] + z[38] + z[48]
 
    return z                                                               
        