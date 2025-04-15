#-*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_138(dfd, table_spec, table_id):
    """Fill table   Key Results for Electric Power Sector Technology Cases 
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
    MNUMCR =  dfd['MNUMCR_rwpre']    
    TRIL_TO_QUAD = dfd['TRIL_TO_QUAD_rwpre']    
 
    #   Key Results for Electric Power Sector Technology Cases
    #   (gigawatts)
    #    Capacity, Generation, Consumption, Emissions                   #   Net Summer Capacity                                         
 
    #     Pulverized Coal                                           
    #T138(1,IY,IS)=UCAPCSU(MNUMNR,IY)+UCAPCSN(MNUMNR,IY)+UCAPCSC(MNUMNR,IY)-UCAPIGU(MNUMNR,IY)-UCAPIGN(MNUMNR,IY)-UCAPIGC(MNUMNR,IY)-UCAPISU(MNUMNR,IY)-UCAPISN(MNUMNR,IY)
    z[1] = dfd['UCAPCSU'].loc[MNUMNR] + dfd['UCAPCSN'].loc[MNUMNR] + dfd['UCAPCSC'].loc[MNUMNR] - \
           dfd['UCAPIGU'].loc[MNUMNR] - dfd['UCAPIGN'].loc[MNUMNR] - dfd['UCAPIGC'].loc[MNUMNR] - \
           dfd['UCAPISU'].loc[MNUMNR] - dfd['UCAPISN'].loc[MNUMNR]
 
    #     Coal Gasification Combined Cycle                          
    #T138(2,IY,IS)=UCAPIGU(MNUMNR,IY)+UCAPIGN(MNUMNR,IY)+UCAPIGC(MNUMNR,IY)+UCAPISU(MNUMNR,IY)+UCAPISN(MNUMNR,IY)
    z[2] = dfd['UCAPIGU'].loc[MNUMNR] + dfd['UCAPIGN'].loc[MNUMNR] + dfd['UCAPIGC'].loc[MNUMNR] + \
           dfd['UCAPISU'].loc[MNUMNR] + dfd['UCAPISN'].loc[MNUMNR]
 
    #     Conventional Natural Gas Combined Cycle                   
    #T138(3,IY,IS)=UCAPCCU(MNUMNR,IY)+UCAPCCN(MNUMNR,IY)+UCAPCCC(MNUMNR,IY)-UCAPACU(MNUMNR,IY)-UCAPACN(MNUMNR,IY)-UCAPACC(MNUMNR,IY)-UCAPASU(MNUMNR,IY)-UCAPASN(MNUMNR,IY)
    z[3] = dfd['UCAPCCU'].loc[MNUMNR] + dfd['UCAPCCN'].loc[MNUMNR] + dfd['UCAPCCC'].loc[MNUMNR] - \
           dfd['UCAPACU'].loc[MNUMNR] - dfd['UCAPACN'].loc[MNUMNR] - dfd['UCAPACC'].loc[MNUMNR] - \
           dfd['UCAPASU'].loc[MNUMNR] - dfd['UCAPASN'].loc[MNUMNR]
 
    #     Advance Natural Gas Combined Cycle                        
    #T138(4,IY,IS)=UCAPACU(MNUMNR,IY)+UCAPACN(MNUMNR,IY)+UCAPACC(MNUMNR,IY)+UCAPASU(MNUMNR,IY)+UCAPASN(MNUMNR,IY)
    z[4] = dfd['UCAPACU'].loc[MNUMNR] + dfd['UCAPACN'].loc[MNUMNR] + dfd['UCAPACC'].loc[MNUMNR] + \
           dfd['UCAPASU'].loc[MNUMNR] + dfd['UCAPASN'].loc[MNUMNR]
 
    #     Convential Combustion Turbine                             
    #T138(5,IY,IS)=UCAPCTU(MNUMNR,IY)+UCAPCTN(MNUMNR,IY)+UCAPCTC(MNUMNR,IY)-UCAPATU(MNUMNR,IY)-UCAPATN(MNUMNR,IY)-UCAPATC(MNUMNR,IY)
    z[5] = dfd['UCAPCTU'].loc[MNUMNR] + dfd['UCAPCTN'].loc[MNUMNR] + dfd['UCAPCTC'].loc[MNUMNR] - \
           dfd['UCAPATU'].loc[MNUMNR] - dfd['UCAPATN'].loc[MNUMNR] - dfd['UCAPATC'].loc[MNUMNR]
 
    #     Advanced Combustion Turbine                               
    #T138(6,IY,IS)=UCAPATU(MNUMNR,IY)+UCAPATN(MNUMNR,IY)+UCAPATC(MNUMNR,IY)
    z[6] = dfd['UCAPATU'].loc[MNUMNR] + dfd['UCAPATN'].loc[MNUMNR] + dfd['UCAPATC'].loc[MNUMNR]
 
    #     Fuel Cells                                                
    #T138(7,IY,IS)=UCAPFCU(MNUMNR,IY)+UCAPFCN(MNUMNR,IY)
    z[7] = dfd['UCAPFCU'].loc[MNUMNR] + dfd['UCAPFCN'].loc[MNUMNR]
 
    #     Nuclear                                                   
    #T138(8,IY,IS)=UCAPNUU(MNUMNR,IY)+UCAPNUN(MNUMNR,IY)+UCAPSMU(MNUMNR,IY)+UCAPSMN(MNUMNR,IY)
    z[8] = dfd['UCAPNUU'].loc[MNUMNR] + dfd['UCAPNUN'].loc[MNUMNR] + dfd['UCAPSMU'].loc[MNUMNR] + \
           dfd['UCAPSMN'].loc[MNUMNR]
 
    #     Oil and Gas Steam                                         
    #T138(9,IY,IS)=UCAPOSU(MNUMNR,IY)+UCAPOSN(MNUMNR,IY)+UCAPOSC(MNUMNR,IY)+UCAPNGU(MNUMNR,IY)+UCAPNGN(MNUMNR,IY)
    z[9] = dfd['UCAPOSU'].loc[MNUMNR] + dfd['UCAPOSN'].loc[MNUMNR] + dfd['UCAPOSC'].loc[MNUMNR] + \
           dfd['UCAPNGU'].loc[MNUMNR] + dfd['UCAPNGN'].loc[MNUMNR]
 
    #     Renewable Sources/Pumped Storage                          
    #T138(10,IY,IS)=UCAPPSU(MNUMNR,IY)+UCAPPSN(MNUMNR,IY)+UCAPHYU(MNUMNR,IY)+UCAPGEU(MNUMNR,IY)+UCAPMSU(MNUMNR,IY)+UCAPWDU(MNUMNR,IY)+UCAPSTU(MNUMNR,IY)+UCAPPVU(MNUMNR,IY)+UCAPWNU(MNUMNR,IY)+UCAPWLU(MNUMNR,IY)+UCAPWFU(MNUMNR,IY)+UCAPPTU(MNUMNR,IY)+UCAPHYN(MNUMNR,IY)+UCAPGEN(MNUMNR,IY)+UCAPMSN(MNUMNR,IY)+UCAPWDN(MNUMNR,IY)+UCAPSTN(MNUMNR,IY)+UCAPPVN(MNUMNR,IY)+UCAPWNN(MNUMNR,IY)+UCAPWFN(MNUMNR,IY)+UCAPWLN(MNUMNR,IY)+UCAPPTN(MNUMNR,IY)+UCAPHYC(MNUMNR,IY)+UCAPGEC(MNUMNR,IY)+UCAPMSC(MNUMNR,IY)+UCAPWDC(MNUMNR,IY)+UCAPSTC(MNUMNR,IY)+UCAPPVC(MNUMNR,IY)+UCAPPTC(MNUMNR,IY)+UCAPWNC(MNUMNR,IY)+UCAPWLC(MNUMNR,IY)
    z[10] = dfd['UCAPPSU'].loc[MNUMNR] + dfd['UCAPPSN'].loc[MNUMNR] + dfd['UCAPHYU'].loc[MNUMNR] + \
            dfd['UCAPGEU'].loc[MNUMNR] + dfd['UCAPMSU'].loc[MNUMNR] + dfd['UCAPWDU'].loc[MNUMNR] + \
            dfd['UCAPSTU'].loc[MNUMNR] + dfd['UCAPPVU'].loc[MNUMNR] + dfd['UCAPWNU'].loc[MNUMNR] + \
            dfd['UCAPWLU'].loc[MNUMNR] + dfd['UCAPWFU'].loc[MNUMNR] + dfd['UCAPPTU'].loc[MNUMNR] + \
            dfd['UCAPHYN'].loc[MNUMNR] + dfd['UCAPGEN'].loc[MNUMNR] + dfd['UCAPMSN'].loc[MNUMNR] + \
            dfd['UCAPWDN'].loc[MNUMNR] + dfd['UCAPSTN'].loc[MNUMNR] + dfd['UCAPPVN'].loc[MNUMNR] + \
            dfd['UCAPWNN'].loc[MNUMNR] + dfd['UCAPWFN'].loc[MNUMNR] + dfd['UCAPWLN'].loc[MNUMNR] + \
            dfd['UCAPPTN'].loc[MNUMNR] + dfd['UCAPHYC'].loc[MNUMNR] + dfd['UCAPGEC'].loc[MNUMNR] + \
            dfd['UCAPMSC'].loc[MNUMNR] + dfd['UCAPWDC'].loc[MNUMNR] + dfd['UCAPSTC'].loc[MNUMNR] + \
            dfd['UCAPPVC'].loc[MNUMNR] + dfd['UCAPPTC'].loc[MNUMNR] + dfd['UCAPWNC'].loc[MNUMNR] + \
            dfd['UCAPWLC'].loc[MNUMNR]
 
    #     Distributed Generation                                    
    #T138(11,IY,IS)=UCAPDBU(MNUMNR,IY)+UCAPDBN(MNUMNR,IY)+UCAPDPU(MNUMNR,IY)+UCAPDPN(MNUMNR,IY)
    z[11] = dfd['UCAPDBU'].loc[MNUMNR] + dfd['UCAPDBN'].loc[MNUMNR] + dfd['UCAPDPU'].loc[MNUMNR] + \
            dfd['UCAPDPN'].loc[MNUMNR]
 
    #     Combined Heat and Power/Other                             
    #T138(12,IY,IS)=T9(44,IY,IS)
    # Create a place holder
    z[12] = z[11] * 0
 
    #       Total                                                   
    #T138(13,IY,IS)=FSUM(T138(1,IY,IS),12)
    # Place holder with partial result
    z[13] = z[1]+z[2]+z[3]+z[4]+z[5]+z[6]+z[7]+z[8]+z[9]+z[10]+z[11]+z[12]

    #                                                               
 
    #   Cumulative Additions                                        
 
    #     Pulverized Coal                                           
    #T138(14,IY,IS)=UADDCSU(1,MNUMNR,IY)+UADDCSN(1,MNUMNR,IY)-UADDIGU(1,MNUMNR,IY)-UADDIGN(1,MNUMNR,IY)+UADDCSU(2,MNUMNR,IY)+UADDCSN(2,MNUMNR,IY)-UADDIGU(2,MNUMNR,IY)-UADDIGN(2,MNUMNR,IY)+UADDCSC(MNUMNR,IY)
    z[14] = dfd['UADDCSU'].loc[1].loc[MNUMNR] + dfd['UADDCSN'].loc[1].loc[MNUMNR] - \
            dfd['UADDIGU'].loc[1].loc[MNUMNR] - dfd['UADDIGN'].loc[1].loc[MNUMNR] + \
            dfd['UADDCSU'].loc[2].loc[MNUMNR] + dfd['UADDCSN'].loc[2].loc[MNUMNR] - \
            dfd['UADDIGU'].loc[2].loc[MNUMNR] - dfd['UADDIGN'].loc[2].loc[MNUMNR] + \
            dfd['UADDCSC'].loc[MNUMNR]
 
    #     Coal Gasification Combined Cycle                          
    #T138(15,IY,IS)=UADDIGU(1,MNUMNR,IY)+UADDIGN(1,MNUMNR,IY)+UADDIGU(2,MNUMNR,IY)+UADDIGN(2,MNUMNR,IY)+UADDIGC(MNUMNR,IY)
    z[15] = dfd['UADDIGU'].loc[1].loc[MNUMNR] + dfd['UADDIGN'].loc[1].loc[MNUMNR] + \
            dfd['UADDIGU'].loc[2].loc[MNUMNR] + dfd['UADDIGN'].loc[2].loc[MNUMNR] + \
            dfd['UADDIGC'].loc[MNUMNR]
 
    #     Conventional Natural Gas Combined Cycle                   
    #T138(16,IY,IS)=UADDCCU(1,MNUMNR,IY)+UADDCCN(1,MNUMNR,IY)-UADDACU(1,MNUMNR,IY)-UADDACN(1,MNUMNR,IY)+UADDCCU(2,MNUMNR,IY)+UADDCCN(2,MNUMNR,IY)-UADDACU(2,MNUMNR,IY)-UADDACN(2,MNUMNR,IY)+UADDCCC(MNUMNR,IY)
    z[16] = dfd['UADDCCU'].loc[1].loc[MNUMNR] + dfd['UADDCCN'].loc[1].loc[MNUMNR] - \
            dfd['UADDACU'].loc[1].loc[MNUMNR] - dfd['UADDACN'].loc[1].loc[MNUMNR] + \
            dfd['UADDCCU'].loc[2].loc[MNUMNR] + dfd['UADDCCN'].loc[2].loc[MNUMNR] - \
            dfd['UADDACU'].loc[2].loc[MNUMNR] - dfd['UADDACN'].loc[2].loc[MNUMNR] + \
            dfd['UADDCCC'].loc[MNUMNR]
 
    #     Advanced Natural Gas Combined Cycle                       
    #T138(17,IY,IS)=UADDACU(1,MNUMNR,IY)+UADDACN(1,MNUMNR,IY)+UADDACU(2,MNUMNR,IY)+UADDACN(2,MNUMNR,IY)+UADDACC(MNUMNR,IY)
    z[17] = dfd['UADDACU'].loc[1].loc[MNUMNR] + dfd['UADDACN'].loc[1].loc[MNUMNR] + \
            dfd['UADDACU'].loc[2].loc[MNUMNR] + dfd['UADDACN'].loc[2].loc[MNUMNR] + \
            dfd['UADDACC'].loc[MNUMNR]
 
    #     Convential Combustion Turbine                             
    #T138(18,IY,IS)=UADDCTU(1,MNUMNR,IY)+UADDCTN(1,MNUMNR,IY)-UADDATU(1,MNUMNR,IY)-UADDATN(1,MNUMNR,IY)+UADDCTU(2,MNUMNR,IY)+UADDCTN(2,MNUMNR,IY)-UADDATU(2,MNUMNR,IY)-UADDATN(2,MNUMNR,IY)+UADDCTC(MNUMNR,IY)
    z[18] = dfd['UADDCTU'].loc[1].loc[MNUMNR] + dfd['UADDCTN'].loc[1].loc[MNUMNR] - \
            dfd['UADDATU'].loc[1].loc[MNUMNR] - dfd['UADDATN'].loc[1].loc[MNUMNR] + \
            dfd['UADDCTU'].loc[2].loc[MNUMNR] + dfd['UADDCTN'].loc[2].loc[MNUMNR] - \
            dfd['UADDATU'].loc[2].loc[MNUMNR] - dfd['UADDATN'].loc[2].loc[MNUMNR] + \
            dfd['UADDCTC'].loc[MNUMNR]
 
    #     Advanced Combustion Turbine                               
    #T138(19,IY,IS)=UADDATU(1,MNUMNR,IY)+UADDATN(1,MNUMNR,IY)+UADDATU(2,MNUMNR,IY)+UADDATN(2,MNUMNR,IY)+UADDATC(MNUMNR,IY)
    z[19] = dfd['UADDATU'].loc[1].loc[MNUMNR] + dfd['UADDATN'].loc[1].loc[MNUMNR] + \
            dfd['UADDATU'].loc[2].loc[MNUMNR] + dfd['UADDATN'].loc[2].loc[MNUMNR] + \
            dfd['UADDATC'].loc[MNUMNR]
 
    #     Fuel Cells                                                
    #T138(20,IY,IS)=UADDFCU(1,MNUMNR,IY)+UADDFCN(1,MNUMNR,IY)+UADDFCU(2,MNUMNR,IY)+UADDFCN(2,MNUMNR,IY)
    z[20] = dfd['UADDFCU'].loc[1].loc[MNUMNR] + dfd['UADDFCN'].loc[1].loc[MNUMNR] + \
            dfd['UADDFCU'].loc[2].loc[MNUMNR] + dfd['UADDFCN'].loc[2].loc[MNUMNR]
 
    #     Nuclear                                                   
    #T138(21,IY,IS)=UADDNUU(1,MNUMNR,IY)+UADDNUN(1,MNUMNR,IY)+UADDNUU(2,MNUMNR,IY)+UADDNUN(2,MNUMNR,IY)+UADDSMU(1,MNUMNR,IY)+UADDSMN(1,MNUMNR,IY)+UADDSMU(2,MNUMNR,IY)+UADDSMN(2,MNUMNR,IY)
    z[21] = dfd['UADDNUU'].loc[1].loc[MNUMNR] + dfd['UADDNUN'].loc[1].loc[MNUMNR] + \
            dfd['UADDNUU'].loc[2].loc[MNUMNR] + dfd['UADDNUN'].loc[2].loc[MNUMNR] + \
            dfd['UADDSMU'].loc[1].loc[MNUMNR] + dfd['UADDSMN'].loc[1].loc[MNUMNR] + \
            dfd['UADDSMU'].loc[2].loc[MNUMNR] + dfd['UADDSMN'].loc[2].loc[MNUMNR]
 
    #     Oil and Gas Steam                                         
    #T138(22,IY,IS)=UADDOSU(1,MNUMNR,IY)+UADDOSN(1,MNUMNR,IY)+UADDOSU(2,MNUMNR,IY)+UADDOSN(2,MNUMNR,IY)+UADDOSC(MNUMNR,IY)
    z[22] = dfd['UADDOSU'].loc[1].loc[MNUMNR] + dfd['UADDOSN'].loc[1].loc[MNUMNR] + \
            dfd['UADDOSU'].loc[2].loc[MNUMNR] + dfd['UADDOSN'].loc[2].loc[MNUMNR] + \
            dfd['UADDOSC'].loc[MNUMNR]
 
    #     Renewable Sources/Pumped Storage                          
    #T138(23,IY,IS)=UADDPSU(1,MNUMNR,IY)+UADDPSN(1,MNUMNR,IY)+UADDRNU(1,MNUMNR,IY)+UADDRNN(1,MNUMNR,IY)+UADDPSU(2,MNUMNR,IY)+UADDPSN(2,MNUMNR,IY)+UADDRNU(2,MNUMNR,IY)+UADDRNN(2,MNUMNR,IY)+UADDHYC(MNUMNR,IY)+UADDGEC(MNUMNR,IY)+UADDMSC(MNUMNR,IY)+UADDWDC(MNUMNR,IY)+UADDSTC(MNUMNR,IY)+UADDPVC(MNUMNR,IY)+UADDWNC(MNUMNR,IY)+UADDWFC(MNUMNR,IY)+UADDWLC(MNUMNR,IY)+UADDPTC(MNUMNR,IY)
    z[23] = dfd['UADDPSU'].loc[1].loc[MNUMNR] + dfd['UADDPSN'].loc[1].loc[MNUMNR] + \
            dfd['UADDRNU'].loc[1].loc[MNUMNR] + dfd['UADDRNN'].loc[1].loc[MNUMNR] + \
            dfd['UADDPSU'].loc[2].loc[MNUMNR] + dfd['UADDPSN'].loc[2].loc[MNUMNR] + \
            dfd['UADDRNU'].loc[2].loc[MNUMNR] + dfd['UADDRNN'].loc[2].loc[MNUMNR] + \
            dfd['UADDHYC'].loc[MNUMNR] + dfd['UADDGEC'].loc[MNUMNR] + dfd['UADDMSC'].loc[MNUMNR] + \
            dfd['UADDWDC'].loc[MNUMNR] + dfd['UADDSTC'].loc[MNUMNR] + dfd['UADDPVC'].loc[MNUMNR] + \
            dfd['UADDWNC'].loc[MNUMNR] + dfd['UADDWFC'].loc[MNUMNR] + dfd['UADDWLC'].loc[MNUMNR] + \
            dfd['UADDPTC'].loc[MNUMNR]
 
    #     Distributed Generation                                    
    #T138(24,IY,IS)=UADDDBU(1,MNUMNR,IY)+UADDDBN(1,MNUMNR,IY)+UADDDPU(1,MNUMNR,IY)+UADDDPN(1,MNUMNR,IY)+UADDDBU(2,MNUMNR,IY)+UADDDBN(2,MNUMNR,IY)+UADDDPU(2,MNUMNR,IY)+UADDDPN(2,MNUMNR,IY)
    z[24] = dfd['UADDDBU'].loc[1].loc[MNUMNR] + dfd['UADDDBN'].loc[1].loc[MNUMNR] + \
            dfd['UADDDPU'].loc[1].loc[MNUMNR] + dfd['UADDDPN'].loc[1].loc[MNUMNR] + \
            dfd['UADDDBU'].loc[2].loc[MNUMNR] + dfd['UADDDBN'].loc[2].loc[MNUMNR] + \
            dfd['UADDDPU'].loc[2].loc[MNUMNR] + dfd['UADDDPN'].loc[2].loc[MNUMNR]
 
    #     Combined Heat and Power/Other                             
    #T138(25,IY,IS)=T9(45,IY,IS)
    #z[25] = T9(z[45]
    # Create a place holder
    z[25]=z[24] * 0
 
    #       Total                                                   
    #T138(26,IY,IS)=FSUM(T138(14,IY,IS),12)
    # Place holder with partial result
    # z[26] = z[14]+z[15]+z[16]+z[17]+z[18]+z[19]+z[20]+z[21]+z[22]+z[23]+z[24]+z[25]
    z[26] = z[25]  # 0
    #                                                               
 
    #   Cumulative Retirements                                      
    #T138(27,IY,IS)=URETTLU(MNUMNR,IY)
    z[27] = dfd['URETTLU'].loc[MNUMNR]
 
    #                                                               
 
    #   Generation by Fuel (billion kilowatthours)                  
 
    # #     Coal                                                      
    # #T138(28,IY,IS)=T8(1,IY,IS)+T8(42,IY,IS)
    z[28] = dfd['UGNCLNR'].loc[1].loc[MNUMNR] + dfd['UGNCLNR'].loc[2].loc[MNUMNR] + (dfd['CGNTGEN'].loc[MNUMNR].loc[1].loc[1] + dfd['CGNTGEN'].loc[MNUMNR].loc[1].loc[2]) * 0.001
 
    # #     Petroleum                                                 
    # #T138(29,IY,IS)=T8(2,IY,IS)+T8(43,IY,IS)
    z[29] = dfd['UGNDSNR'].loc[1].loc[MNUMNR] + dfd['UGNDSNR'].loc[2].loc[MNUMNR] + \
           dfd['UGNRHNR'].loc[1].loc[MNUMNR] + dfd['UGNRHNR'].loc[2].loc[MNUMNR] + \
           dfd['UGNRLNR'].loc[1].loc[MNUMNR] + dfd['UGNRLNR'].loc[2].loc[MNUMNR] + (dfd['CGNTGEN'].loc[MNUMNR].loc[2].loc[1] + dfd['CGNTGEN'].loc[MNUMNR].loc[2].loc[2]) * 0.001
 
    # #     Natural Gas                                               
    # #T138(30,IY,IS)=T8(3,IY,IS)+T8(44,IY,IS)
    z[30] = dfd['UGNGFNR'].loc[1].loc[MNUMNR] + dfd['UGNGFNR'].loc[2].loc[MNUMNR] + \
           dfd['UGNGINR'].loc[1].loc[MNUMNR] + dfd['UGNGINR'].loc[2].loc[MNUMNR] + (dfd['CGNTGEN'].loc[MNUMNR].loc[3].loc[1] + dfd['CGNTGEN'].loc[MNUMNR].loc[3].loc[2]) * 0.001
 
    # #     Nuclear Power                                             
    # #T138(31,IY,IS)=T8(4,IY,IS)
    z[31] = dfd['UGNURNR'].loc[1].loc[MNUMNR] + dfd['UGNURNR'].loc[2].loc[MNUMNR]
 
    # #     Renewable Sources/Pumped Storage                          
    # #T138(32,IY,IS)=T8(5,IY,IS)+T8(6,IY,IS)+T8(46,IY,IS)
    z[32] = dfd['UGNPSNR'].loc[1].loc[MNUMNR] + dfd['UGNPSNR'].loc[2].loc[MNUMNR] + \
           dfd['UGNSDNR'].loc[1].loc[MNUMNR] + dfd['UGNSDNR'].loc[2].loc[MNUMNR] + dfd['UGNHYNR'].loc[1].loc[MNUMNR] + dfd['UGNHYNR'].loc[2].loc[MNUMNR] + \
           dfd['UGNGENR'].loc[1].loc[MNUMNR] + dfd['UGNGENR'].loc[2].loc[MNUMNR] + \
           dfd['UGNMSNR'].loc[1].loc[MNUMNR] + dfd['UGNMSNR'].loc[2].loc[MNUMNR] + \
           dfd['UGNWDNR'].loc[1].loc[MNUMNR] + dfd['UGNWDNR'].loc[2].loc[MNUMNR] + \
           dfd['UGNSONR'].loc[1].loc[MNUMNR] + dfd['UGNSONR'].loc[2].loc[MNUMNR] + \
           dfd['UGNPVNR'].loc[1].loc[MNUMNR] + dfd['UGNPVNR'].loc[2].loc[MNUMNR] + \
           dfd['UGNPTNR'].loc[1].loc[MNUMNR] + dfd['UGNPTNR'].loc[2].loc[MNUMNR] + \
           dfd['UGNWNNR'].loc[1].loc[MNUMNR] + dfd['UGNWNNR'].loc[2].loc[MNUMNR] + \
           dfd['UGNWLNR'].loc[1].loc[MNUMNR] + dfd['UGNWLNR'].loc[2].loc[MNUMNR] + \
           dfd['UGNWFNR'].loc[1].loc[MNUMNR] + dfd['UGNWFNR'].loc[2].loc[MNUMNR] + (dfd['CGNTGEN'].loc[MNUMNR].loc[4].loc[1] + dfd['CGNTGEN'].loc[MNUMNR].loc[4].loc[2] + \
           dfd['CGNTGEN'].loc[MNUMNR].loc[5].loc[1] + dfd['CGNTGEN'].loc[MNUMNR].loc[5].loc[2] + \
           dfd['CGNTGEN'].loc[MNUMNR].loc[6].loc[1] + dfd['CGNTGEN'].loc[MNUMNR].loc[6].loc[2] + \
           dfd['CGNTGEN'].loc[MNUMNR].loc[7].loc[1] + dfd['CGNTGEN'].loc[MNUMNR].loc[7].loc[2] + \
           dfd['CGNTGEN'].loc[MNUMNR].loc[8].loc[1] + dfd['CGNTGEN'].loc[MNUMNR].loc[8].loc[2]) * 0.001
 
    # #     Distributed Generation                                    
    # #T138(33,IY,IS)=T8(41,IY,IS)
    z[33] = dfd['UGNDDNR'].loc[1].loc[MNUMNR] + dfd['UGNDDNR'].loc[2].loc[MNUMNR] + \
            dfd['UGNDGNR'].loc[1].loc[MNUMNR] + dfd['UGNDGNR'].loc[2].loc[MNUMNR]
 
    # #     Combined Heat and Power/Other                             
    # #T138(34,IY,IS)=T8(15,IY,IS)
    # Create a place holder
    z[34] = z[33] * 0
 
    # #       Total                                                   
    # #T138(35,IY,IS)=FSUM(T138(28,IY,IS),7)
    # Create a place holder with partical result   
    z[35] = z[28]+z[29]+z[30]+z[31]+z[32]+z[33]+z[34]

    #                                                               
 
    #                                                               
 
    #   Fuel Consumption (quadrillion Btu)                          
 
    #     Coal                                                      
    #T138(36,IY,IS)=QCLEL(11,IY)
    z[36] = dfd['QCLEL'].loc[MNUMCR] * TRIL_TO_QUAD
 
    #     Petroleum                                                 
    #T138(37,IY,IS)=QDSEL(11,IY)+QRSEL(11,IY)+QPCEL(11,IY)
    z[37] = (dfd['QDSEL'].loc[MNUMCR] + dfd['QRSEL'].loc[MNUMCR] + dfd['QPCEL'].loc[MNUMCR])*TRIL_TO_QUAD
 
    #     Natural Gas                                               
    #T138(38,IY,IS)=QNGEL(11,IY)
    z[38] = dfd['QNGEL'].loc[MNUMCR]*TRIL_TO_QUAD
 
    #     Nuclear / Uranium                                         
    #T138(39,IY,IS)=QUREL(11,IY)
    z[39] = dfd['QUREL'].loc[MNUMCR]*TRIL_TO_QUAD
 
    #     Renewable Sources  (indices reversed)                                       
    #T138(40,IY,IS)=QTREL(11,IY)-WNCMSEL(IY,11)
    # z[40] = (dfd['QTREL'].loc[MNUMCR] - dfd['WNCMSEL'].loc[MNUMCR])*TRIL_TO_QUAD
    z[40] = dfd['QTREL'].loc[MNUMCR]*TRIL_TO_QUAD - dfd['WNCMSEL'].loc[MNUMCR]
    
    #       Total                                                   
    #T138(41,IY,IS)=FSUM(T138(36,IY,IS),5)+WNCMSEL(IY,11)
    z[41] = z[36]+z[37]+z[38]+z[39]+z[40] + dfd['WNCMSEL'].loc[MNUMCR]#*TRIL_TO_QUAD
 
    #                                                               
 
    #   Carbon Dioxide Emissions                                    
 
    #   (million metric tons carbon_dd_ equivalent)                 
 
    #     Coal                                                      
    #T138(44,IY,IS)=T17(23,11,IY,IS)
    # z[44] = T17(z[23,11]           # Note:  Cross-refernced table, with index. Don't know how to handle.
    # Create a place holder
    z[44] = z[41] * 0

    # #     Petroleum                                                 
    # #T138(42,IY,IS)=T17(21,11,IY,IS)
    # Create a place holder
    z[42] = z[41] * 0
 
    # #     Natural Gas                                               
    # #T138(43,IY,IS)=T17(22,11,IY,IS)
    # Create a place holder    
    z[43] = z[41] * 0
 
    # #     Other                                                     
    # #T138(45,IY,IS)=T17(24,11,IY,IS)
    # Create a place holder    
    z[45] = z[41] * 0
 
    # #       Total                                                   
    # #T138(46,IY,IS)=FSUM(T138(42,IY,IS),4)
    # Create a place holder    
    z[46] = z[42]+z[43]+z[44]+z[45]

    
 
    return z                                                               
                         
