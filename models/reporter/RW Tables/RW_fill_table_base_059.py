# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_059(dfd, table_spec, table_id):
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

    TRIL_TO_QUAD =  dfd['TRIL_TO_QUAD_rwpre']

    #   Electric Power Sector Generating Capacity and Generation by Plant Type and Technology
    #    Capacity and Generation                                    
    #                                                               
    #   Net Summer Capacity (gigawatts) 1/                          
    #     Capacity                                                  
    #       Coal                                                    
    #T59(1,IR,IY,IS)=UCAPCSU(IR,IY)+UCAPCSN(IR,IY)+UCAPCSC(IR,IY)
    z[1] = dfd['UCAPCSU'] + dfd['UCAPCSN'] + dfd['UCAPCSC']
    #         Advanced w/o Sequestration                            
    #T59(2,IR,IY,IS)=UCAPIGU(IR,IY)+UCAPIGN(IR,IY)+UCAPIGC(IR,IY)
    z[2] = dfd['UCAPIGU'] + dfd['UCAPIGN'] + dfd['UCAPIGC']
    #         Advanced with Sequestration                           
    #T59(73,IR,IY,IS)=UCAPISU(IR,IY)+UCAPISN(IR,IY)
    z[73] = dfd['UCAPISU'] + dfd['UCAPISN']
    #         Advanced with Partial Sequestration                   
    #T59(161,IR,IY,IS)=UCAPPQU(IR,IY)+UCAPPQN(IR,IY)
    z[161] = dfd['UCAPPQU'] + dfd['UCAPPQN']
    #         Other New Coal                                        
    #T59(142,IR,IY,IS)=UCAPOCU(IR,IY)+UCAPOCN(IR,IY)
    z[142] = dfd['UCAPOCU'] + dfd['UCAPOCN']
    #         IGCC with Gas Cofiring                                
    #T59(143,IR,IY,IS)=UCAPI2U(IR,IY)+UCAPI2N(IR,IY)
    z[143] = dfd['UCAPI2U'] + dfd['UCAPI2N']
    #         Conventional                                          
    #T59(3,IR,IY,IS)=T59(1,IR,IY,IS)-T59(2,IR,IY,IS)-T59(73,IR,IY,IS)-T59(161,IR,IY,IS)-T59(142,IR,IY,IS)-T59(143,IR,IY,IS)
    z[3] = z[1] - z[2] - z[73] - z[161] - z[142] - z[143]
    #       Fossil Steam 2/                            
    #T59(4,IR,IY,IS)=UCAPOSU(IR,IY)+UCAPOSN(IR,IY)+UCAPOSC(IR,IY)+UCAPNGU(IR,IY)+UCAPNGN(IR,IY)
    z[4] = dfd['UCAPOSU'] + dfd['UCAPOSN'] + dfd['UCAPOSC'] + dfd['UCAPNGU'] + dfd['UCAPNGN']
    #         Existing Oil and Natural Gas Steam                    
    #T59(133,IR,IY,IS)=UCAPOSU(IR,IY)+UCAPOSN(IR,IY)+UCAPOSC(IR,IY)
    z[133] = dfd['UCAPOSU'] + dfd['UCAPOSN'] + dfd['UCAPOSC']
    #         Coal Steam Converted to Natural Gas Steam             
    #T59(134,IR,IY,IS)=UCAPNGU(IR,IY)+UCAPNGN(IR,IY)
    z[134] = dfd['UCAPNGU'] + dfd['UCAPNGN'] - dfd["UCAPNGCF"]
    #T59 New- Converted to Coal-Gas Cofiring
    z[79] = dfd["UCAPNGCF"]
    #       Combined Cycle                                          
    #T59(5,IR,IY,IS)=UCAPCCU(IR,IY)+UCAPCCN(IR,IY)+UCAPCCC(IR,IY)
    z[5] = dfd['UCAPCCU'] + dfd['UCAPCCN'] + dfd['UCAPCCC']
    #         Advanced w/o Sequestration  - Multi Shaft                          
    #T59(6,IR,IY,IS)=UCAPACU(IR,IY)+UCAPACN(IR,IY)+UCAPACC(IR,IY)
    z[6] = dfd['UCAPACU'] + dfd['UCAPACN'] + dfd['UCAPACC']
    #         Advanced with Sequestration - Single Shaft with CCS
    #T59(74,IR,IY,IS)=UCAPASU(IR,IY)+UCAPASN(IR,IY)
    z[74] = dfd['UCAPASU'] + dfd['UCAPASN']
    #         Conventional (Retrofits)                              
    #T59(146,IR,IY,IS)=UCAPA2U(IR,IY)+UCAPA2N(IR,IY)
    z[146] = dfd['UCAPA2U'] + dfd['UCAPA2N']
    #         Conventional - Existing and single shaft                                         
    #T59(7,IR,IY,IS)=T59(5,IR,IY,IS)-T59(6,IR,IY,IS)-T59(74,IR,IY,IS)
    z[7] = z[5] - z[6] - z[74]
    #       Combustion Turbine/Diesel                               
    #T59(8,IR,IY,IS)=UCAPCTU(IR,IY)+UCAPCTN(IR,IY)+UCAPCTC(IR,IY)
    z[8] = dfd['UCAPCTU'] + dfd['UCAPCTN'] + dfd['UCAPCTC']
    #         Advanced - Industrial frame                                              
    #T59(9,IR,IY,IS)=UCAPATU(IR,IY)+UCAPATN(IR,IY)+UCAPATC(IR,IY)
    z[9] = dfd['UCAPATU'] + dfd['UCAPATN'] + dfd['UCAPATC']
    #        Other Turbine - not used
    #T59(145,IR,IY,IS)=UCAPT2U(IR,IY)+UCAPT2N(IR,IY)
    z[145] = dfd['UCAPT2U'] + dfd['UCAPT2N']
    #         Conventional - Existing and Aeroderivative
    #T59(10,IR,IY,IS)=T59(8,IR,IY,IS)-T59(9,IR,IY,IS)-T59(145,IR,IY,IS)
    z[10] = z[8] - z[9] #- z[145]
    #       Nuclear Power - Light Water Reactor                     
    #T59(11,IR,IY,IS)=UCAPNUU(IR,IY)+UCAPNUN(IR,IY)
    z[11] = dfd['UCAPNUU'] + dfd['UCAPNUN']
    #       Nuclear Power - Small Modular Reactor                   
    #T59(137,IR,IY,IS)=UCAPSMU(IR,IY)+UCAPSMN(IR,IY)
    z[137] = dfd['UCAPSMU'] + dfd['UCAPSMN']
    #       Greenfield Nuclear                                      
    #T59(147,IR,IY,IS)=UCAPGNU(IR,IY)+UCAPGNN(IR,IY)
    z[147] = dfd['UCAPGNU'] + dfd['UCAPGNN']
    #       Pumped Storage                                          
    #T59(12,IR,IY,IS)=UCAPPSU(IR,IY)+UCAPPSN(IR,IY)
    z[12] = dfd['UCAPPSU'] + dfd['UCAPPSN']
    #       Quick Storage                                           
    #T59(153,IR,IY,IS)=UCAPQSU(IR,IY)+UCAPQSN(IR,IY)
    z[153] = dfd['UCAPQSU'] + dfd['UCAPQSN']
    #       Diurnal Storage                                         
    #T59(154,IR,IY,IS)=UCAPDSU(IR,IY)+UCAPDSN(IR,IY)
    z[154] = dfd['UCAPDSU'] + dfd['UCAPDSN']
    #       Hydrogen Turbine                                        
    #T59(144,IR,IY,IS)=UCAPICU(IR,IY)+UCAPICN(IR,IY)
    z[144] = dfd['UCAPICU'] + dfd['UCAPICN']
    #       Hydrogen CC_FC                                          
    #T59(261,IR,IY,IS)=0.0
    z[261] = 0 * z[144]
    #       Other Storage                                           
    #T59(155,IR,IY,IS)=UCAPZSU(IR,IY)+UCAPZSN(IR,IY)
    z[155] = dfd['UCAPZSU'] + dfd['UCAPZSN']
    #       Fuel Cells                                              
    #T59(13,IR,IY,IS)=UCAPFCU(IR,IY)+UCAPFCN(IR,IY)
    z[13] = dfd['UCAPFCU'] + dfd['UCAPFCN']
    #       Renewable Sources 3/                                    
    #T59(14,IR,IY,IS)=UCAPHYU(IR,IY)+UCAPHYN(IR,IY)+UCAPHYC(IR,IY)
    z[14] = dfd['UCAPHYU'] + dfd['UCAPHYN'] + dfd['UCAPHYC']
    #       Conventional Hydroelectric Power                        
    #T59(14,IR,IY,IS)=UCAPHYU(IR,IY)+UCAPHYN(IR,IY)+UCAPHYC(IR,IY)
    z[14] = dfd['UCAPHYU'] + dfd['UCAPHYN'] + dfd['UCAPHYC']
    #       Advanced Hydroelectric Power                            
    #T59(150,IR,IY,IS)=UCAPHOU(IR,IY)+UCAPHON(IR,IY)
    z[150] = dfd['UCAPHOU'] + dfd['UCAPHON']
    #       In Stream Hydroelectric Power                           
    #T59(151,IR,IY,IS)=UCAPHIU(IR,IY)+UCAPHIN(IR,IY)
    z[151] = dfd['UCAPHIU'] + dfd['UCAPHIN']
    #       Tidal Hydroelectric Power                               
    #T59(152,IR,IY,IS)=UCAPTIU(IR,IY)+UCAPTIN(IR,IY)
    z[152] = dfd['UCAPTIU'] + dfd['UCAPTIN']
    #       Geothermal 3/                                           
    #T59(112,IR,IY,IS)=UCAPGEU(IR,IY)+UCAPGEN(IR,IY)+UCAPGEC(IR,IY)
    z[112] = dfd['UCAPGEU'] + dfd['UCAPGEN'] + dfd['UCAPGEC']
    #       Advanced Geothermal                                     
    #T59(149,IR,IY,IS)=UCAPAGU(IR,IY)+UCAPAGN(IR,IY)
    #z[149] = dfd['UCAPAGU'] + dfd['UCAPAGN']
    #       Municipal Waste 4/                                      
    #T59(113,IR,IY,IS)=UCAPMSU(IR,IY)+UCAPMSN(IR,IY)+UCAPMSC(IR,IY)
    z[113] = dfd['UCAPMSU'] + dfd['UCAPMSN'] + dfd['UCAPMSC']
    #       Wood and Other Biomass 5/                               
    #T59(114,IR,IY,IS)=UCAPWDU(IR,IY)+UCAPWDN(IR,IY)+UCAPWDC(IR,IY)
    z[114] = dfd['UCAPWDU'] + dfd['UCAPWDN'] + dfd['UCAPWDC']
    #       Biomass CCS                                            
    #T59(148,IR,IY,IS)=UCAPBIU(IR,IY)+UCAPBIN(IR,IY)
    z[148] = dfd['UCAPBIU'] + dfd['UCAPBIN']
    #       Solar Thermal                                           
    #T59(115,IR,IY,IS)=UCAPSTU(IR,IY)+UCAPSTN(IR,IY)+UCAPSTC(IR,IY)
    z[115] = dfd['UCAPSTU'] + dfd['UCAPSTN'] + dfd['UCAPSTC']
    #       Solar Thermal w/Storage                                 
    #T59(157,IR,IY,IS)=UCAPSSU(IR,IY)+UCAPSSN(IR,IY)
    z[157] = dfd['UCAPSSU'] + dfd['UCAPSSN']
    #       Solar Thermal w/Storage 2                               
    #T59(158,IR,IY,IS)=UCAPS2U(IR,IY)+UCAPS2N(IR,IY)
    z[158] = dfd['UCAPS2U'] + dfd['UCAPS2N']
    #       Solar Photovoltaic with Axis Tracking 6/                
    #T59(116,IR,IY,IS)=UCAPPVU(IR,IY)+UCAPPVN(IR,IY)+UCAPPVC(IR,IY)
    z[116] = dfd['UCAPPVU'] + dfd['UCAPPVN'] + dfd['UCAPPVC']
    #       Solar Photovoltaic with Storage 6/                      
    #T59(159,IR,IY,IS)=UCAPPTU(IR,IY)+UCAPPTN(IR,IY)+UCAPPTC(IR,IY)
    z[159] = dfd['UCAPPTU'] + dfd['UCAPPTN'] + dfd['UCAPPTC']
    #       Wind                                                    
    #T59(117,IR,IY,IS)=UCAPWNU(IR,IY)+UCAPWNN(IR,IY)+UCAPWNC(IR,IY)
    z[117] = dfd['UCAPWNU'] + dfd['UCAPWNN'] + dfd['UCAPWNC']
    #       Offshore Wind                                           
    #T59(118,IR,IY,IS)=UCAPWFU(IR,IY)+UCAPWFN(IR,IY)+UCAPWFC(IR,IY)
    z[118] = dfd['UCAPWFU'] + dfd['UCAPWFN'] + dfd['UCAPWFC']
    #       Low Speed Wind                                          
    #T59(156,IR,IY,IS)=UCAPWLU(IR,IY)+UCAPWLN(IR,IY)+UCAPWLC(IR,IY)
    z[156] = dfd['UCAPWLU'] + dfd['UCAPWLN'] + dfd['UCAPWLC']
    #       Other Intermittant                                      
    #T59(160,IR,IY,IS)=UCAPINU(IR,IY)+UCAPINN(IR,IY)
    z[160] = dfd['UCAPINU'] + dfd['UCAPINN']
    #       Distributed Generation 7/                               
    #T59(70,IR,IY,IS)=UCAPDBU(IR,IY)+UCAPDBN(IR,IY)+UCAPDPU(IR,IY)+UCAPDPN(IR,IY)
    z[70] = dfd['UCAPDBU'] + dfd['UCAPDBN'] + dfd['UCAPDPU'] + dfd['UCAPDPN']
    #         Total                                                 
    #T59(15,IR,IY,IS)=T59(1,IR,IY,IS)+T59(4,IR,,IS)+T59(5,IR,IY,IS)+T59(70,IR,IY,IS)+FSUM(T59(9,IR,IY,IS),6)+FSUM(T59(112,IR,IY,IS),7)+T59(137,IR,IY,IS) + T59(144,IR,IY,IS)+T59(145,IR,IY,IS)+FSUM(T59(147,IR,IY,IS),14) + T59(261,IR,IY,IS)
    z[15] = z[1]+ z[4]+ z[5]+ z[8]+z[11]+z[137]+z[12]+ z[154] +z[144]+ z[13]+z[14]+z[112]+z[113]+ \
            z[114] + z[148] +z[115]+z[116]+ z[159]+z[117]+z[118] + z[70]
    #       Hybrid Storage                                          
    #T59(257)=0
    #T59(257,IR,IY,IS) = T59(159,IR,IY,IS) / 3.0
    z[257] = z[159] / 3.0
    #       Total Storage                                           
    #T59(258)=0
    #T59(258,IR,IY,IS) = T59(257,IR,IY,IS) + T59(154,IR,IY,IS)
    z[258] = z[257] + z[154]
    #     Cumulative Planned Additions 8/                           
    #
    #       Coal                                                    
    #T59(16,IR,IY,IS)=UADDCSU(1,IR,IY)+UADDCSN(1,IR,IY)+UADDCSC(IR,IY)
    z[16] = dfd['UADDCSU'].loc[1] + dfd['UADDCSN'].loc[1] + dfd['UADDCSC']
    #         Advanced w/o Sequestration                            
    #T59(17,IR,IY,IS)=UADDIGU(1,IR,IY)+UADDIGN(1,IR,IY)+UADDIGC(IR,IY)
    z[17] = dfd['UADDIGU'].loc[1] + dfd['UADDIGN'].loc[1] + dfd['UADDIGC']
    #         Advanced with Sequestration                           
    #T59(75,IR,IY,IS)=UADDISU(1,IR,IY)+UADDISN(1,IR,IY)
    z[75] = dfd['UADDISU'].loc[1] + dfd['UADDISN'].loc[1]
    #         Advanced with Partial Sequestration                   
    #T59(181,IR,IY,IS)=UADDPQU(1,IR,IY)+UADDPQN(1,IR,IY)
    z[181] = dfd['UADDPQU'].loc[1] + dfd['UADDPQN'].loc[1]
    #         Other New Coal                                        
    #T59(162,IR,IY,IS)=UADDOCU(1,IR,IY)+UADDOCN(1,IR,IY)
    z[162] = dfd['UADDOCU'].loc[1] + dfd['UADDOCN'].loc[1]
    #         IGCC with Gas Cofiring                                
    #T59(163,IR,IY,IS)=UADDI2U(1,IR,IY)+UADDI2N(1,IR,IY)
    z[163] = dfd['UADDI2U'].loc[1] + dfd['UADDI2N'].loc[1]
    #         Conventional                                          
    #T59(18,IR,IY,IS)=T59(16,IR,IY,IS)-T59(17,IR,IY,IS)-T59(75,IR,IY,IS)-T59(181,IR,IY,IS)-T59(162,IR,IY,IS)-T59(163,IR,IY,IS)
    z[18] = z[16] - z[17] - z[75] - z[181] - z[162] - z[163]
    #       Fossil Steam 2/                            
    #T59(19,IR,IY,IS)=UADDOSU(1,IR,IY)+UADDOSN(1,IR,IY)+UADDOSC(IR,IY)
    z[19] = dfd['UADDOSU'].loc[1] + dfd['UADDOSN'].loc[1] + dfd['UADDOSC']
    #       Combined Cycle                                          
    #T59(20,IR,IY,IS)=UADDCCU(1,IR,IY)+UADDCCN(1,IR,IY)+UADDCCC(IR,IY)
    z[20] = dfd['UADDCCU'].loc[1] + dfd['UADDCCN'].loc[1] + dfd['UADDCCC']
    #         Advanced w/o Sequestration - Multi Shaft
    #T59(21,IR,IY,IS)=UADDACU(1,IR,IY)+UADDACN(1,IR,IY)+UADDACC(IR,IY)
    z[21] = dfd['UADDACU'].loc[1] + dfd['UADDACN'].loc[1] + dfd['UADDACC']
    #         Advanced with Sequestration - Single shaft with CCS
    #T59(76,IR,IY,IS)=UADDASU(1,IR,IY)+UADDASN(1,IR,IY)
    z[76] = dfd['UADDASU'].loc[1] + dfd['UADDASN'].loc[1]
    #        Conventional (Retrofits)                               
    #T59(166,IR,IY,IS)=UADDA2U(1,IR,IY)+UADDA2N(1,IR,IY)
    z[166] = dfd['UADDA2U'].loc[1] + dfd['UADDA2N'].loc[1]
    #         Conventional - Existing and single shaft
    #T59(22,IR,IY,IS)=T59(20,IR,IY,IS)-T59(21,IR,IY,IS)-T59(76,IR,IY,IS)-T59(166,IR,IY,IS)
    z[22] = z[20] - z[21] - z[76]
    #       Combustion Turbine/Diesel                               
    #T59(23,IR,IY,IS)=UADDCTU(1,IR,IY)+UADDCTN(1,IR,IY)+UADDCTC(IR,IY)
    z[23] = dfd['UADDCTU'].loc[1] + dfd['UADDCTN'].loc[1] + dfd['UADDCTC']
    #         Advanced - Industrial Frame
    #T59(24,IR,IY,IS)=UADDATU(1,IR,IY)+UADDATN(1,IR,IY)+UADDATC(IR,IY)
    z[24] = dfd['UADDATU'].loc[1] + dfd['UADDATN'].loc[1] + dfd['UADDATC']
    #         Other Turbine - not used                               
    #T59(165,IR,IY,IS)=UADDT2U(1,IR,IY)+UADDT2N(1,IR,IY)
    z[165] = dfd['UADDT2U'].loc[1] + dfd['UADDT2N'].loc[1]
    #         Conventional  - Existing and Aeroderivative                                        
    #T59(25,IR,IY,IS)=T59(23,IR,IY,IS)-T59(24,IR,IY,IS)-T59(165,IR,IY,IS)
    z[25] = z[23] - z[24] - z[165]
    #       Nuclear Power - Light Water Reactor                     
    #T59(26,IR,IY,IS)=UADDNUU(1,IR,IY)+UADDNUN(1,IR,IY)
    z[26] = dfd['UADDNUU'].loc[1] + dfd['UADDNUN'].loc[1]
    #       Nuclear Power - Small Modular Reactor                   
    #T59(138,IR,IY,IS)=UADDSMU(1,IR,IY)+UADDSMN(1,IR,IY)
    z[138] = dfd['UADDSMU'].loc[1] + dfd['UADDSMN'].loc[1]
    #       Greenfield Nuclear                                      
    #T59(167,IR,IY,IS)=UADDGNU(1,IR,IY)+UADDGNN(1,IR,IY)
    z[167] = dfd['UADDGNU'].loc[1] + dfd['UADDGNN'].loc[1]
    #       Pumped Storage                                          
    #T59(27,IR,IY,IS)=UADDPSU(1,IR,IY)+UADDPSN(1,IR,IY)
    z[27] = dfd['UADDPSU'].loc[1] + dfd['UADDPSN'].loc[1]
    #       Quick Storage                                           
    #T59(173,IR,IY,IS)=UADDSQU(1,IR,IY)+UADDSQN(1,IR,IY)
    z[173] = dfd['UADDSQU'].loc[1] + dfd['UADDSQN'].loc[1]
    #       Diurnal Storage                                         
    #T59(174,IR,IY,IS)=UADDDSU(1,IR,IY)+UADDDSN(1,IR,IY)
    z[174] = dfd['UADDDSU'].loc[1] + dfd['UADDDSN'].loc[1]
    #       Hydrogen Turbine                                        
    #T59(164,IR,IY,IS)=UADDICU(1,IR,IY)+UADDICN(1,IR,IY)
    z[164] = dfd['UADDICU'].loc[1] + dfd['UADDICN'].loc[1]
    #       Hydrogen CC_FC                                          
    #T59(262,IR,IY,IS)=0.0
    z[262] = 0 * z[164]
    #       Other Storage                                           
    #T59(175,IR,IY,IS)=UADDZSU(1,IR,IY)+UADDZSN(1,IR,IY)
    z[175] = dfd['UADDZSU'].loc[1] + dfd['UADDZSN'].loc[1]
    #       Fuel Cells                                              
    #T59(28,IR,IY,IS)=UADDFCU(1,IR,IY)+UADDFCN(1,IR,IY)
    z[28] = dfd['UADDFCU'].loc[1] + dfd['UADDFCN'].loc[1]
    #       Conventional Hydroelectric Power                      
    #T59( 29,IR,IY,IS) = UADDHYU(1,IR,IY) + UADDHYN(1,IR,IY) + UADDHYC(IR,IY)
    z[29] = dfd['UADDHYU'].loc[1] + dfd['UADDHYN'].loc[1] + dfd['UADDHYC'] 
    #       Advanced Hydroelectric                                  
    #T59(170,IR,IY,IS)=UADDHOU(1,IR,IY)+UADDHON(1,IR,IY)
    z[170] = dfd['UADDHOU'].loc[1] + dfd['UADDHON'].loc[1]
    #       In Stream Hydroelectric                                 
    #T59(171,IR,IY,IS)=UADDHIU(1,IR,IY)+UADDHIN(1,IR,IY)
    z[171] = dfd['UADDHIU'].loc[1] + dfd['UADDHIN'].loc[1]
    #       Tidal Hydroelectric                                     
    #T59(172,IR,IY,IS)=UADDTIU(1,IR,IY)+UADDTIN(1,IR,IY)
    z[172] = dfd['UADDTIU'].loc[1] + dfd['UADDTIN'].loc[1]
    #       Geothermal 3/                                           
    #T59(119,IR,IY,IS)=UADDGEU(1,IR,IY)+UADDGEN(1,IR,IY)+UADDGEC(IR,IY)
    z[119] = dfd['UADDGEU'].loc[1] + dfd['UADDGEN'].loc[1] + dfd['UADDGEC']
    #       Advanced Geothermal                                     
    #T59(169,IR,IY,IS)=UADDAGU(1,IR,IY)+UADDAGN(1,IR,IY)
    z[169] = dfd['UADDAGU'].loc[1] + dfd['UADDAGN'].loc[1]
    #       Municipal Waste 4/                                      
    #T59(120,IR,IY,IS)=UADDMSU(1,IR,IY)+UADDMSN(1,IR,IY)+UADDMSC(IR,IY)
    z[120] = dfd['UADDMSU'].loc[1] + dfd['UADDMSN'].loc[1] + dfd['UADDMSC']
    #       Wood and Other Biomass 5/                               
    #T59(121,IR,IY,IS)=UADDWDU(1,IR,IY)+UADDWDN(1,IR,IY)+UADDWDC(IR,IY)
    z[121] = dfd['UADDWDU'].loc[1] + dfd['UADDWDN'].loc[1] + dfd['UADDWDC']
    #       Biomass CCS                                            
    #T59(168,IR,IY,IS)=UADDBIU(1,IR,IY)+UADDBIN(1,IR,IY)
    z[168] = dfd['UADDBIU'].loc[1] + dfd['UADDBIN'].loc[1]
    #       Solar Thermal                                           
    #T59(122,IR,IY,IS)=UADDSTU(1,IR,IY)+UADDSTN(1,IR,IY)+UADDSTC(IR,IY)
    z[122] = dfd['UADDSTU'].loc[1] + dfd['UADDSTN'].loc[1] + dfd['UADDSTC']
    #       Solar Thermal w/Storage                                 
    #T59(177,IR,IY,IS)=UADDSSU(1,IR,IY)+UADDSSN(1,IR,IY)
    z[177] = dfd['UADDSSU'].loc[1] + dfd['UADDSSN'].loc[1]
    #       Solar Thermal w/Storage 2                               
    #T59(178,IR,IY,IS)=UADDS2U(1,IR,IY)+UADDS2N(1,IR,IY)
    z[178] = dfd['UADDS2U'].loc[1] + dfd['UADDS2N'].loc[1]
    #       Solar Photovoltaic with Axis Tracking 6/                
    #T59(123,IR,IY,IS)=UADDPVU(1,IR,IY)+UADDPVN(1,IR,IY)+UADDPVC(IR,IY)
    z[123] = dfd['UADDPVU'].loc[1] + dfd['UADDPVN'].loc[1] + dfd['UADDPVC']
    #       Solar Photovoltaic with Storage 6/                      
    #T59(179,IR,IY,IS)=UADDPTU(1,IR,IY)+UADDPTN(1,IR,IY)
    z[179] = dfd['UADDPTU'].loc[1] + dfd['UADDPTN'].loc[1]
    #       Wind                                                    
    #T59(124,IR,IY,IS)=UADDWNU(1,IR,IY)+UADDWNN(1,IR,IY)+UADDWNC(IR,IY)
    z[124] = dfd['UADDWNU'].loc[1] + dfd['UADDWNN'].loc[1] + dfd['UADDWNC']
    #       Offshore Wind                                           
    #T59(125,IR,IY,IS)=UADDWFU(1,IR,IY)+UADDWFN(1,IR,IY)+UADDWFC(IR,IY)
    z[125] = dfd['UADDWFU'].loc[1] + dfd['UADDWFN'].loc[1] + dfd['UADDWFC']
    #       Low Speed Wind                                          
    #T59(176,IR,IY,IS)=UADDWLU(1,IR,IY)+UADDWLN(1,IR,IY)
    z[176] = dfd['UADDWLU'].loc[1] + dfd['UADDWLN'].loc[1]
    #       Other Intermittant                                      
    #T59(180,IR,IY,IS)=UADDINU(1,IR,IY)+UADDINN(1,IR,IY)
    z[180] = dfd['UADDINU'].loc[1] + dfd['UADDINN'].loc[1]
    #       Distributed Generation 7/                               
    #T59(71,IR,IY,IS)=UADDDBU(1,IR,IY)+UADDDBN(1,IR,IY)+UADDDPU(1,IR,IY)+UADDDPN(1,IR,IY)
    z[71] = dfd['UADDDBU'].loc[1] + dfd['UADDDBN'].loc[1] + dfd['UADDDPU'].loc[1] + dfd['UADDDPN'].loc[1]
    #         Total                                                 
    #T59(30,IR,IY,IS)=FSUM(T59(16,IR,IY,IS),14)+FSUM(T59(119,IR,IY,IS),7)+FSUM(T59(167,IR,IY,IS),14)+T59(71,IR,IY,IS)+T59(75,IR,IY,IS)+T59(76,IR,IY,IS)+T59(162,IR,IY,IS)+T59(163,IR,IY,IS)+T59(181,IR,IY,IS)-T59(16,IR,IY,IS)-T59(20,IR,IY,IS)-T59(23,IR,IY,IS
    z[30]= z[16]+z[17]+z[18]+z[19]+z[20]+z[21]+z[22]+z[23]+z[24]+z[25]+z[26]+z[27]+z[28]+z[29]+ \
           z[119]+z[120]+z[121]+z[122]+z[123]+z[124]+z[125]+ \
           z[167]+z[168]+z[169]+z[170]+z[171]+z[172]+z[173]+z[174]+z[175]+z[176]+z[177]+z[178]+z[179]+z[180]+ \
           z[71] + z[75] + z[76] + z[162] + z[163] + z[181] - z[16] - z[20] - z[23]
    #     Cumulative Unplanned Additions 
    #       Coal                                                    
    #T59(31,IR,IY,IS)=UADDCSU(2,IR,IY)+UADDCSN(2,IR,IY)
    z[31] = dfd['UADDCSU'].loc[2] + dfd['UADDCSN'].loc[2]
    #         Advanced w/o Sequestration                            
    #T59(32,IR,IY,IS)=UADDIGU(2,IR,IY)+UADDIGN(2,IR,IY)
    z[32] = dfd['UADDIGU'].loc[2] + dfd['UADDIGN'].loc[2]
    #         Advanced with Sequestration                           
    #T59(77,IR,IY,IS)=UADDISU(2,IR,IY)+UADDISN(2,IR,IY)
    z[77] = dfd['UADDISU'].loc[2] + dfd['UADDISN'].loc[2]
    #         Advanced with Partial Sequestration                   
    #T59(201,IR,IY,IS)=UADDPQU(2,IR,IY)+UADDPQN(2,IR,IY)
    z[201] = dfd['UADDPQU'].loc[2] + dfd['UADDPQN'].loc[2]
    #         Other New Coal                                        
    #T59(182,IR,IY,IS)=UADDOCU(2,IR,IY)+UADDOCN(2,IR,IY)
    z[182] = dfd['UADDOCU'].loc[2] + dfd['UADDOCN'].loc[2]
    #         IGCC with Gas Cofiring                                
    #T59(183,IR,IY,IS)=UADDI2U(2,IR,IY)+UADDI2N(2,IR,IY)
    z[183] = dfd['UADDI2U'].loc[2] + dfd['UADDI2N'].loc[2]
    #         Conventional                                          
    #T59(33,IR,IY,IS)=T59(31,IR,IY,IS)-T59(32,IR,IY,IS)-T59(77,IR,IY,IS)-T59(201,IR,IY,IS)-T59(182,IR,IY,IS)-T59(183,IR,IY,IS)
    z[33] = z[31] - z[32] - z[77] - z[201] - z[182] - z[183]
    #       Fossil Steam 2/                            
    #T59(34,IR,IY,IS)=UADDOSU(2,IR,IY)+UADDOSN(2,IR,IY)
    z[34] = dfd['UADDOSU'].loc[2] + dfd['UADDOSN'].loc[2]
    #       Combined Cycle                                          
    #T59(35,IR,IY,IS)=UADDCCU(2,IR,IY)+UADDCCN(2,IR,IY)
    z[35] = dfd['UADDCCU'].loc[2] + dfd['UADDCCN'].loc[2]
    #         Advanced w/o Sequestration  - Multi Shaft                          
    #T59(36,IR,IY,IS)=UADDACU(2,IR,IY)+UADDACN(2,IR,IY)
    z[36] = dfd['UADDACU'].loc[2] + dfd['UADDACN'].loc[2]
    #         Advanced with Sequestration - Single Shaft with CCS
    #T59(78,IR,IY,IS)=UADDASU(2,IR,IY)+UADDASN(2,IR,IY)
    z[78] = dfd['UADDASU'].loc[2] + dfd['UADDASN'].loc[2]
    #         Conventional (Retrofits)                              
    #T59(186,IR,IY,IS)=UADDA2U(2,IR,IY)+UADDA2N(2,IR,IY)
    z[186] = dfd['UADDA2U'].loc[2] + dfd['UADDA2N'].loc[2]
    #         Conventional - Existing and Single shaft
    #T59(37,IR,IY,IS)=T59(35,IR,IY,IS)-T59(36,IR,IY,IS)-T59(78,IR,IY,IS)-T59(186,IR,IY,IS)
    z[37] = z[35] - z[36] - z[78]
    #       Combustion Turbine/Diesel                               
    #T59(38,IR,IY,IS)=UADDCTU(2,IR,IY)+UADDCTN(2,IR,IY)
    z[38] = dfd['UADDCTU'].loc[2] + dfd['UADDCTN'].loc[2]
    #         Advanced - Industrial Frame                                             
    #T59(39,IR,IY,IS)=UADDATU(2,IR,IY)+UADDATN(2,IR,IY)
    z[39] = dfd['UADDATU'].loc[2] + dfd['UADDATN'].loc[2]
    #         Other Turbine - not used                               
    #T59(185,IR,IY,IS)=UADDT2U(2,IR,IY)+UADDT2N(2,IR,IY)
    z[185] = dfd['UADDT2U'].loc[2] + dfd['UADDT2N'].loc[2]
    #         Conventional - Existing and Aeroderivative                                          
    #T59(40,IR,IY,IS)=T59(38,IR,IY,IS)-T59(39,IR,IY,IS)-T59(185,IR,IY,IS)
    z[40] = z[38] - z[39] - z[185]
    #       Nuclear Power - Light Water Reactor                     
    #T59(41,IR,IY,IS)=UADDNUU(2,IR,IY)+UADDNUN(2,IR,IY)
    z[41] = dfd['UADDNUU'].loc[2] + dfd['UADDNUN'].loc[2]
    #       Nuclear Power - Small Modular Reactor                   
    #T59(139,IR,IY,IS)=UADDSMU(2,IR,IY)+UADDSMN(2,IR,IY)
    z[139] = dfd['UADDSMU'].loc[2] + dfd['UADDSMN'].loc[2]
    #       Greenfield Nuclear                                      
    #T59(187,IR,IY,IS)=UADDGNU(2,IR,IY)+UADDGNN(2,IR,IY)
    z[187] = dfd['UADDGNU'].loc[2] + dfd['UADDGNN'].loc[2]
    #       Pumped Storage                                          
    #T59(42,IR,IY,IS)=UADDPSU(2,IR,IY)+UADDPSN(2,IR,IY)
    z[42] = dfd['UADDPSU'].loc[2] + dfd['UADDPSN'].loc[2]
    #       Quick Storage                                           
    #T59(193,IR,IY,IS)=UADDSQU(2,IR,IY)+UADDSQN(2,IR,IY)
    z[193] = dfd['UADDSQU'].loc[2] + dfd['UADDSQN'].loc[2]
    #       Diurnal Storage                                         
    #T59(194,IR,IY,IS)=UADDDSU(2,IR,IY)+UADDDSN(2,IR,IY)
    z[194] = dfd['UADDDSU'].loc[2] + dfd['UADDDSN'].loc[2]
    #       Hydrogen Turbine                                        
    #T59(184,IR,IY,IS)=UADDICU(2,IR,IY)+UADDICN(2,IR,IY)
    z[184] = dfd['UADDICU'].loc[2] + dfd['UADDICN'].loc[2]
    #       Hydrogen CC_FC                                          
    #T59(263,IR,IY,IS)=0.0
    z[263] = 0 * z[184]
    #       Other Storage                                           
    #T59(195,IR,IY,IS)=UADDZSU(2,IR,IY)+UADDZSN(2,IR,IY)
    z[195] = dfd['UADDZSU'].loc[2] + dfd['UADDZSN'].loc[2]
    #       Fuel Cells                                              
    #T59(43,IR,IY,IS)=UADDFCU(2,IR,IY)+UADDFCN(2,IR,IY)
    z[43] = dfd['UADDFCU'].loc[2] + dfd['UADDFCN'].loc[2]
    #       Conventional Hydroelectric Power                        
    #T59(44,IR,IY,IS)=UADDHYU(2,IR,IY)+UADDHYN(2,IR,IY)
    z[44] = dfd['UADDHYU'].loc[2] + dfd['UADDHYN'].loc[2]
    #       Advanced Hydroelectric                                  
    #T59(190,IR,IY,IS)=UADDHOU(2,IR,IY)+UADDHON(2,IR,IY)
    z[190] = dfd['UADDHOU'].loc[2] + dfd['UADDHON'].loc[2]
    #       In Stream Hydroelectric                                 
    #T59(191,IR,IY,IS)=UADDHIU(2,IR,IY)+UADDHIN(2,IR,IY)
    z[191] = dfd['UADDHIU'].loc[2] + dfd['UADDHIN'].loc[2]
    #       Tidal Hydroelectric                                     
    #T59(192,IR,IY,IS)=UADDTIU(2,IR,IY)+UADDTIN(2,IR,IY)
    z[192] = dfd['UADDTIU'].loc[2] + dfd['UADDTIN'].loc[2]
    #       Geothermal 3/                                           
    #T59(126,IR,IY,IS)=UADDGEU(2,IR,IY)+UADDGEN(2,IR,IY)
    z[126] = dfd['UADDGEU'].loc[2] + dfd['UADDGEN'].loc[2]
    #       Advanced Geothermal                                     
    #T59(189,IR,IY,IS)=UADDAGU(2,IR,IY)+UADDAGN(2,IR,IY)
    z[189] = dfd['UADDAGU'].loc[2] + dfd['UADDAGN'].loc[2]
    #       Municipal Waste 4/                                      
    #T59(127,IR,IY,IS)=UADDMSU(2,IR,IY)+UADDMSN(2,IR,IY)
    z[127] = dfd['UADDMSU'].loc[2] + dfd['UADDMSN'].loc[2]
    #       Wood and Other Biomass 5/                               
    #T59(128,IR,IY,IS)=UADDWDU(2,IR,IY)+UADDWDN(2,IR,IY)
    z[128] = dfd['UADDWDU'].loc[2] + dfd['UADDWDN'].loc[2]
    #       Biomass CCS                                            
    #T59(188,IR,IY,IS)=UADDBIU(2,IR,IY)+UADDBIN(2,IR,IY)
    z[188] = dfd['UADDBIU'].loc[2] + dfd['UADDBIN'].loc[2]
    #       Solar Thermal                                           
    #T59(129,IR,IY,IS)=UADDSTU(2,IR,IY)+UADDSTN(2,IR,IY)
    z[129] = dfd['UADDSTU'].loc[2] + dfd['UADDSTN'].loc[2]
    #       Solar Thermal w/Storage                                 
    #T59(197,IR,IY,IS)=UADDSSU(2,IR,IY)+UADDSSN(2,IR,IY)
    z[197] = dfd['UADDSSU'].loc[2] + dfd['UADDSSN'].loc[2]
    #       Solar Thermal w/Storage 2                               
    #T59(198,IR,IY,IS)=UADDS2U(2,IR,IY)+UADDS2N(2,IR,IY)
    z[198] = dfd['UADDS2U'].loc[2] + dfd['UADDS2N'].loc[2]
    #       Solar Photovoltaic with Axis Tracking 6/                
    #T59(130,IR,IY,IS)=UADDPVU(2,IR,IY)+UADDPVN(2,IR,IY)
    z[130] = dfd['UADDPVU'].loc[2] + dfd['UADDPVN'].loc[2]
    #       Solar Photovoltaic with Storage 6/                      
    #T59(199,IR,IY,IS)=UADDPTU(2,IR,IY)+UADDPTN(2,IR,IY)
    z[199] = dfd['UADDPTU'].loc[2] + dfd['UADDPTN'].loc[2]
    #       Wind                                                    
    #T59(131,IR,IY,IS)=UADDWNU(2,IR,IY)+UADDWNN(2,IR,IY)
    z[131] = dfd['UADDWNU'].loc[2] + dfd['UADDWNN'].loc[2]
    #       Offshore Wind                                           
    #T59(132,IR,IY,IS)=UADDWFU(2,IR,IY)+UADDWFN(2,IR,IY)
    z[132] = dfd['UADDWFU'].loc[2] + dfd['UADDWFN'].loc[2]
    #       Low Speed Wind                                          
    #T59(196,IR,IY,IS)=UADDWLU(2,IR,IY)+UADDWLN(2,IR,IY)
    z[196] = dfd['UADDWLU'].loc[2] + dfd['UADDWLN'].loc[2]
    #       Other Intermittant                                      
    #T59(200,IR,IY,IS)=UADDINU(2,IR,IY)+UADDINN(2,IR,IY)
    z[200] = dfd['UADDINU'].loc[2] + dfd['UADDINN'].loc[2]
    #       Distributed Generation 7/                               
    #T59(72,IR,IY,IS)=UADDDBU(2,IR,IY)+UADDDBN(2,IR,IY)+UADDDPU(2,IR,IY)+UADDDPN(2,IR,IY)
    z[72] = dfd['UADDDBU'].loc[2] + dfd['UADDDBN'].loc[2] + dfd['UADDDPU'].loc[2] + dfd['UADDDPN'].loc[2]
    #         Total                                                 
    #T59(45,IR,IY,IS)=FSUM(T59(31,IR,IY,IS),14)+FSUM(T59(126,IR,IY,IS),7)+T59(72,IR,IY,IS)+T59(77,IR,IY,IS)+T59(78,IR,IY,IS)+T59(201,IR,IY,IS)+T59(182,IR,IY,IS)+T59(183,IR,IY,IS)+T59(186,IR,IY,IS)-T59(31,IR,IY,IS)-T59(35,IR,IY,IS)-T59(38,IR,IY,IS)+T59(139,IR,IY,IS)+T59(184,IR,IY,IS)+T59(185,IR,IY,IS)+FSUM(T59(187,IR,IY,IS),14)
    z[45] = z[31] + z[32] + z[33] + z[34] + z[35] + z[36] + z[37]+z[38] + z[39] + z[40] + z[41] + z[42] + z[43] + z[44]+ \
            z[126] + z[127] + z[128] + z[129] + z[130] + z[131] + z[132] + \
            z[72] + z[77] + z[78] + z[201] + z[182] + z[183] + z[186] - z[31] - z[35] - z[38] + z[139] + z[184] + z[185] + \
            z[187] + z[188] + z[189] + z[190] + z[190] + z[191] + z[192]+z[193] + z[194] + z[195] + z[196] + z[197] + z[198] + z[199]
    #     Cumulative Total Additions                                
    #T59(46,IR,IY,IS)=UADDTLU(1,IR,IY)+UADDTLU(2,IR,IY)+UADDTLN(1,IR,IY)+UADDTLN(2,IR,IY)+UADDTLC(IR,IY)
    z[46] = dfd['UADDTLU'].loc[1] + dfd['UADDTLU'].loc[2] + dfd['UADDTLN'].loc[1] + dfd['UADDTLN'].loc[2] + dfd['UADDTLC']
    #     Cumulative Retirements 9/                                 
    #       Coal                                                    
    #T59(47,IR,IY,IS)=URETCSU(IR,IY)
    z[47] = dfd['URETCSU']
    #         Advanced                                              
    #T59(48,IR,IY,IS)=URETIGU(IR,IY)
    z[48] = dfd['URETIGU']
    #         Advanced with Sequestration                   
    #T59(221,IR,IY,IS)=+URETISU(IR,IY)
    z[272] = dfd['URETISU']
    #         Advanced with Partial Sequestration                   
    #T59(221,IR,IY,IS)=URETPQU(IR,IY)
    z[221] = dfd['URETPQU']
    #         Other New Coal                                        
    #T59(202,IR,IY,IS)=URETOCU(IR,IY)
    z[202] = dfd['URETOCU']
    #         IGCC with Gas Cofiring                                
    #T59(203,IR,IY,IS)=URETI2U(IR,IY)
    z[203] = dfd['URETI2U']
    #         Conventional                                          
    #T59(49,IR,IY,IS)=T59(47,IR,IY,IS)-T59(48,IR,IY,IS)-T59(221,IR,IY,IS)-T59(202,IR,IY,IS)-T59(203,IR,IY,IS)
    z[49] = z[47] - z[48] - z[221] - z[202] - z[203]
    #       Fossil Steam 2/                            
    #T59(50,IR,IY,IS)=URETOSU(IR,IY)
    z[50] = dfd['URETOSU']
    #       Combined Cycle                                          
    #T59(51,IR,IY,IS)=URETCCU(IR,IY)
    z[51] = dfd['URETCCU']
    #         Advanced w/o Sequestration - Multi Shaft                                             
    #T59(52,IR,IY,IS)=URETACU(IR,IY) + URETASU(IR,IY)
    z[52] = dfd['URETACU']
    #         Advanced with Sequestration - Single Shaft with CCS                                          
    #T59 - new row to break out with and without CCS
    z[266] = dfd['URETASU']
    #         Conventional (Retrofits)                              
    #T59(206,IR,IY,IS)=URETA2U(IR,IY)
    z[206] = dfd['URETA2U']
    #         Conventional - Existing and Single Shaft                                         
    #T59(53,IR,IY,IS)=T59(51,IR,IY,IS)-T59(52,IR,IY,IS)-T59(206,IR,IY,IS)
    z[53] = z[51] - z[52]
    #       Combustion Turbine/Diesel                               
    #T59(54,IR,IY,IS)=URETCTU(IR,IY)
    z[54] = dfd['URETCTU']
    #         Advanced - Industrial Frame                                             
    #T59(55,IR,IY,IS)=URETATU(IR,IY)
    z[55] = dfd['URETATU']
    #         Other Turbine - Not used                               
    #T59(205,IR,IY,IS)=URETT2U(IR,IY)
    z[205] = dfd['URETT2U']
    #         Conventional - Existing and Aeroderivative                                         
    #T59(56,IR,IY,IS)=T59(54,IR,IY,IS)-T59(55,IR,IY,IS)-T59(205,IR,IY,IS)
    z[56] = z[54] - z[55] - z[205]
    #       Nuclear Power - Light Water Reactor                     
    #T59(57,IR,IY,IS)=URETNUU(IR,IY)
    z[57] = dfd['URETNUU']
    #       Nuclear Power - Small Modular Reactor                   
    #T59(140,IR,IY,IS)=URETSMU(IR,IY)
    z[140] = dfd['URETSMU']
    #       Greenfield Nuclear                                      
    #T59(207,IR,IY,IS)=URETGNU(IR,IY)
    z[207] = dfd['URETGNU']
    #       Pumped Storage                                          
    #T59(58,IR,IY,IS)=URETPSU(IR,IY)
    z[58] = dfd['URETPSU']
    #       Quick Storage                                           
    #T59(213,IR,IY,IS)=URETSQU(IR,IY)
    z[213] = dfd['URETSQU']
    #       Diurnal Storage                                         
    #T59(214,IR,IY,IS)=URETDSU(IR,IY)
    z[214] = dfd['URETDSU']
    #       Hydrogen Turbine                                        
    #T59(204,IR,IY,IS)=URETICU(IR,IY)
    z[204] = dfd['URETICU']
    #       Hydrogen CC_FC                                          
    #T59(264,IR,IY,IS)=0.0 
    # z[264] = 0
    z[264] = 0 * z[1]
    #       Other Storage                                           
    #T59(215,IR,IY,IS)=URETZSU(IR,IY)
    z[215] = dfd['URETZSU']
    #       Fuel Cells                                              
    #T59(59,IR,IY,IS)=URETFCU(IR,IY)
    z[59] = dfd['URETFCU']
    #       Renewable Sources 3/                                    
    #T59(60,IR,IY,IS)=URETRNU(IR,IY)
    z[60] = dfd['URETRNU']
    #       Conventional Hydroelectric Power                        
    #T59(62,IR,IY,IS)=URETHYU(IR,IY)
    z[62] = dfd['URETHYU']
    #       Advanced Hydroelectric                                  
    #T59(210,IR,IY,IS)=URETHOU(IR,IY)
    z[210] = dfd['URETHOU']
    #       In Stream Hydroelectric                                 
    #T59(211,IR,IY,IS)=URETHIU(IR,IY)
    z[211] = dfd['URETHIU']
    #       Tidal Hydroelectric                                     
    #T59(212,IR,IY,IS)=URETTIU(IR,IY)
    z[212] = dfd['URETTIU']
    #       Geothermal 3/                                           
    #T59(63,IR,IY,IS)=URETGEU(IR,IY)
    z[63] = dfd['URETGEU']
    #       Advanced Geothermal                                     
    #T59(209,IR,IY,IS)=URETAGU(IR,IY)
    z[209] = dfd['URETAGU']
    #       Municipal Waste 4/                                      
    #T59(64,IR,IY,IS)=URETMSU(IR,IY)
    z[64] = dfd['URETMSU']
    #       Wood and Other Biomass 5/                               
    #T59(65,IR,IY,IS)=URETWDU(IR,IY)
    z[65] = dfd['URETWDU']
    #       Biomass CCS                                            
    #T59(208,IR,IY,IS)=URETBIU(IR,IY)
    z[208] = dfd['URETBIU']
    #       Solar Thermal                                           
    #T59(66,IR,IY,IS)=URETSTU(IR,IY)
    z[66] = dfd['URETSTU']
    #       Solar Thermal w/Storage                                 
    #T59(217,IR,IY,IS)=URETSSU(IR,IY)
    z[217] = dfd['URETSSU']
    #       Solar Thermal w/Storage 2                               
    #T59(218,IR,IY,IS)=URETS2U(IR,IY)
    z[218] = dfd['URETS2U']
    #       Solar Photovoltaic with Axis Tracking 6/                
    #T59(67,IR,IY,IS)=URETPVU(IR,IY)
    z[67] = dfd['URETPVU']
    #       Solar Photovoltaic with Storage 6/                      
    #T59(219,IR,IY,IS)=URETPTU(IR,IY)
    z[219] = dfd['URETPTU']
    #       Wind                                                    
    #T59(68,IR,IY,IS)=URETWNU(IR,IY)
    z[68] = dfd['URETWNU']
    #       Offshore Wind                                           
    #T59(69,IR,IY,IS)=URETWFU(IR,IY)
    z[69] = dfd['URETWFU']
    #       Low Speed Wind                                          
    #T59(216,IR,IY,IS)=URETWLU(IR,IY)
    z[216] = dfd['URETWLU']
    #       Other Intermittant                                      
    #T59(220,IR,IY,IS)=URETINU(IR,IY)
    z[220] = dfd['URETINU']
    #         Total                                                 
    #T59(61,IR,IY,IS)=T59( 47,IR,IY,IS)+T59( 50,IR,IY,IS)+T59( 51,IR,IY,IS)+T59(140,IR,IY,IS)+T59(204,IR,IY,IS)+T59(205,IR,IY,IS)+FSUM(T59(55,IR,IY,IS),5)+FSUM(T59(62,IR,IY,IS),8)+FSUM(T59(207,IR,IY,IS),14)+T59(264,IR,IY,IS)
    z[61] = z[47] + z[50] + z[51] + z[140] + z[204] + z[205] + z[55]+z[56]+z[57]+z[58]+z[59]+ \
            z[62]+z[63] + z[64] + z[65] + z[66] + z[67] + z[68] + z[69]   +\
            z[207]+z[208]+z[209]+z[210]+z[211]+z[212]+z[213]+z[214]+z[215]+z[216]+z[217]+z[218]+z[219]+z[220]+z[264]
    #   Generation By Plant Type 
    #   (billion kilowatthours) 
    #     Coal                                                      
    #T59(80,IR,IY,IS)=UGENPC(IR,IY)+UGENIG(IR,IY)+UGENIS(IR,IY)+UGENPQ(IR,IY)+UGENOC(IR,IY)+UGENI2(IR,IY)
    z[80] = dfd['UGENPC'] + dfd['UGENIG'] + dfd['UGENIS'] + dfd['UGENPQ'] + dfd['UGENOC'] + dfd['UGENI2']
    #       Advanced w/o Sequestration                              
    #T59(81,IR,IY,IS)=UGENIG(IR,IY)
    z[81] = dfd['UGENIG']
    #       Advanced with Sequestration                             
    #T59(82,IR,IY,IS)=UGENIS(IR,IY)
    z[82] = dfd['UGENIS']
    #       Advanced with Partial Sequestration                     
    #T59(241,IR,IY,IS)=UGENPQ(IR,IY)
    z[241] = dfd['UGENPQ']
    #       Other New Coal                                          
    #T59(222,IR,IY,IS)=UGENOC(IR,IY)
    z[222] = dfd['UGENOC']
    #       IGCC with Gas Cofiring                                  
    #T59(223,IR,IY,IS)=UGENI2(IR,IY)
    z[223] = dfd['UGENI2']
    #       Conventional                                            
    #T59(83,IR,IY,IS)=UGENPC(IR,IY)
    z[83] = dfd['UGENPC']
    #     Fossil Steam 2/                              
    #T59(84,IR,IY,IS)=UGENOS(IR,IY)
    z[84] = dfd['UGENOS'] + dfd['UGENNG']
    #         Existing Oil and Natural Gas Steam                    
    #T59 - new breakdowns of fossil steam types
    z[267] = dfd['UGENOS']
    #         Coal Steam Converted to Natural Gas Steam             
    z[268] = dfd['UGENNG'] - dfd['UGENNGCF']
    #         Coal Steam Converted to Coal-gas cofiring           
    z[269] = dfd['UGENNGCF']  
    #     Combined Cycle                                            
    #T59(85,IR,IY,IS)=UGENCC(IR,IY)+UGENAC(IR,IY)+UGENCS(IR,IY)+UGENA2(IR,IY)
    z[85] = dfd['UGENCC'] + dfd['UGENAC'] + dfd['UGENCS'] + dfd['UGENA2']
    #       Advanced w/o Sequestration - Multi Shaft                             
    #T59(86,IR,IY,IS)=UGENAC(IR,IY)
    z[86] = dfd['UGENAC']
    #       Advanced with Sequestration - Single Shaft with CCS                             
    #T59(87,IR,IY,IS)=UGENCS(IR,IY)
    z[87] = dfd['UGENCS']
    #       Conventional (Retrofits)                                
    #T59(226,IR,IY,IS)=UGENA2(IR,IY)
    z[226] = dfd['UGENA2']
    #       Conventional - Existing and Single Shaft                                            
    #T59(88,IR,IY,IS)=UGENCC(IR,IY)
    z[88] = dfd['UGENCC'] + dfd["UGENA2"]
    #     Combustion Turbine/Diesel                                 
    #T59(89,IR,IY,IS)=UGENCT(IR,IY)+UGENAT(IR,IY)+UGENT2(IR,IY)
    z[89] = dfd['UGENCT'] + dfd['UGENAT'] + dfd['UGENT2']
    #       Advanced - Industrial Frame                                               
    #T59(90,IR,IY,IS)=UGENAT(IR,IY)
    z[90] = dfd['UGENAT']
    #       Other Turbine - not used                                 
    #T59(225,IR,IY,IS)=UGENT2(IR,IY)
    z[225] = dfd['UGENT2']
    #       Conventional - Existing and Aeroderivative                                           
    #T59(91,IR,IY,IS)=UGENCT(IR,IY)
    z[91] = dfd['UGENCT']
    #     Nuclear Power - Light Water Reactor                       
    #T59(92,IR,IY,IS)=UGENNU(IR,IY)
    z[92] = dfd['UGENNU']
    #     Nuclear Power - Small Modular Reactor                     
    #T59(141,IR,IY,IS)=UGENSM(IR,IY)
    z[141] = dfd['UGENSM']
    #     Greenfield Nuclear                                        
    #T59(227,IR,IY,IS)=UGENGN(IR,IY)
    z[227] = dfd['UGENGN']
    #     Pumped Storage and Other 10/                              
    #T59(93,IR,IY,IS)=UGENPS(IR,IY)
    z[93] = dfd['UGENPS'] - dfd['WNGMSEL']
    #     Quick Storage                                             
    #T59(233,IR,IY,IS)=UGENQS(IR,IY)
    z[233] = dfd['UGENQS']
    #     Diurnal Storage                                           
    #T59(234,IR,IY,IS)=UGENDS(IR,IY)
    z[234] = dfd['UGENDS']
    #     Hydrogen Turbine                                          
    #T59(224,IR,IY,IS)=UGENIC(IR,IY)
    z[224] = dfd['UGENIC']
    #     Hydrogen CC_FC                                            
    #T59(265,IR,IY,IS)=0.0
    # z[265] = 0
    z[265] = 0 * z[1]
    #     Other Storage                                             
    #T59(235,IR,IY,IS)=UGENZS(IR,IY)
    z[235] = dfd['UGENZS']
    #     Fuel Cells                                                
    #T59(94,IR,IY,IS)=UGENFC(IR,IY)
    z[94] = dfd['UGENFC']
    #     Renewable Sources 3/                                      
    #T59(95,IR,IY,IS)=UGNHYNR(1,IR,IY)+UGNHYNR(2,IR,IY)+(CGNTGEN(IR,IY,4,1)+CGNTGEN(IR,IY,4,2))*0.001
    #z[95] = dfd['UGNHYNR'].loc[1] + dfd['UGNHYNR'].loc[2] + (dfd['CGNTGEN'].loc[4].loc[1] + dfd['CGNTGEN'].loc[4].loc[1])*TRIL_TO_QUAD
    #     *Conventional Hydroelectric Power                          
    #T59(95,IR,IY,IS)=UGNHYNR(1,IR,IY)+UGNHYNR(2,IR,IY)+(CGNTGEN(IR,IY,4,1)+CGNTGEN(IR,IY,4,2))*0.001
    z[95] = dfd['UGNHYNR'].loc[1] + dfd['UGNHYNR'].loc[2]  + (dfd['CGNTGEN'].loc[:,4,1] + dfd['CGNTGEN'].loc[:,4,2])*0.001
    #     Advanced Hydroelectric                                    
    #T59(230,IR,IY,IS)=UGENHO(IR,IY)
    z[230] = dfd['UGENHO']
    #     In Stream Hydroelectric                                   
    #T59(231,IR,IY,IS)=UGENHI(IR,IY)
    z[231] = dfd['UGENHI']
    #     Tidal Hydroelectric                                       
    #T59(232,IR,IY,IS)=UGENTI(IR,IY)
    z[232] = dfd['UGENTI']
    #     *Geothermal 3/                                             
    #T59(250,IR,IY,IS)=UGNGENR(1,IR,IY)+UGNGENR(2,IR,IY)+(CGNTGEN(IR,IY,5,1)+CGNTGEN(IR,IY,5,2))*0.001
    # z[250] = dfd['UGNGENR'] + dfd['UGNGENR'] + (dfd['CGNTGEN'].loc[5].loc[1] + dfd['CGNTGEN'].loc[5].loc[2])*TRIL_TO_QUAD
    z[250] = dfd['UGNGENR'].loc[1] + dfd['UGNGENR'].loc[2] + (dfd['CGNTGEN'].loc[:,5,1] + dfd['CGNTGEN'].loc[:,5,2])*TRIL_TO_QUAD
    
    #     Advanced Geothermal                                       
    #T59(229,IR,IY,IS)=UGENAG(IR,IY)
    z[229] = dfd['UGENAG']
    #     Municipal Waste 4/                                        
    #T59(251,IR,IY,IS)=UGNMSNR(1,IR,IY)+UGNMSNR(2,IR,IY)+(CGNTGEN(IR,IY,6,1)+CGNTGEN(IR,IY,6,2))*0.001
    # z[251] = dfd['UGNMSNR'] + dfd['UGNMSNR'] + (dfd['CGNTGEN'].loc[6].loc[1] + dfd['CGNTGEN'].loc[6].loc[2])*TRIL_TO_QUAD
    z[251] = dfd['UGNMSNR'].loc[1] + dfd['UGNMSNR'].loc[2] + (dfd['CGNTGEN'].loc[:,6,1] + dfd['CGNTGEN'].loc[:,6,2])*TRIL_TO_QUAD
    #     *Wood and Other Biomass 5/                                 
    #T59(252,IR,IY,IS)=UGNWDNR(1,IR,IY)+UGNWDNR(2,IR,IY)+(CGNTGEN(IR,IY,7,1)+CGNTGEN(IR,IY,7,2))*0.001-UGNCFNR(1,IR,IY)-UGNCFNR(2,IR,IY)
    # z[252] = dfd['UGNWDNR'] + dfd['UGNWDNR'] + (dfd['CGNTGEN'].loc[7].loc[1] + dfd['CGNTGEN'].loc[7].loc[2])*TRIL_TO_QUAD - dfd['UGNCFNR'].loc[2]
    z[252] = dfd['UGNWDNR'].loc[1] + dfd['UGNWDNR'].loc[2] + (dfd['CGNTGEN'].loc[:,7,1] + dfd['CGNTGEN'].loc[:,7,2])*TRIL_TO_QUAD - dfd['UGNCFNR'].loc[1] - dfd['UGNCFNR'].loc[2] - dfd["UGENBI"]
    #     Biomass CCS                                              
    #T59(228,IR,IY,IS)=UGENBI(IR,IY)
    z[228] = dfd['UGENBI']
    #     Solar Thermal                                             
    #T59(253,IR,IY,IS)=UGNSONR(1,IR,IY)+UGNSONR(2,IR,IY)
    # z[253] = dfd['UGNSONR'] + dfd['UGNSONR']
    z[253] = dfd['UGNSONR'].loc[1] + dfd['UGNSONR'].loc[2]
    #     Solar Thermal w/Storage                                   
    #T59(237,IR,IY,IS)=UGENSS(IR,IY)
    z[237] = dfd['UGENSS']
    #     Solar Thermal w/Storage 2                                 
    #T59(238,IR,IY,IS)=UGENS2(IR,IY)
    z[238] = dfd['UGENS2']
    #     *Solar Photovoltaic with Axis Tracking 6/                  
    #T59(254,IR,IY,IS)=UGNPVNR(1,IR,IY)+UGNPVNR(2,IR,IY)+(CGNTGEN(IR,IY,8,1)+CGNTGEN(IR,IY,8,2))*0.001
    # z[254] = dfd['UGNPVNR'] + dfd['UGNPVNR'] + (dfd['CGNTGEN'].loc[8].loc[1] + dfd['CGNTGEN'].loc[8].loc[2])*TRIL_TO_QUAD
    z[254] = dfd['UGNPVNR'].loc[1] + dfd['UGNPVNR'].loc[2] + (dfd['CGNTGEN'].loc[:,8,1] + dfd['CGNTGEN'].loc[:,8,2])*TRIL_TO_QUAD
    #     Solar Photovoltaic with Storage 6/                        
    #T59(239,IR,IY,IS)=UGNPTNR(1,IR,IY)+UGNPTNR(2,IR,IY)
    z[239] = dfd['UGNPTNR'].loc[1] + dfd['UGNPTNR'].loc[2]
    #     Wind                                                      
    #T59(255,IR,IY,IS)=UGNWNNR(1,IR,IY)+UGNWNNR(2,IR,IY)
    z[255] = dfd['UGNWNNR'].loc[1] + dfd['UGNWNNR'].loc[2]
    #     Offshore Wind                                             
    #T59(256,IR,IY,IS)=UGNWFNR(1,IR,IY)+UGNWFNR(2,IR,IY)
    z[256] = dfd['UGNWFNR'].loc[1] + dfd['UGNWFNR'].loc[2]
    #     Low Speed Wind                                            
    #T59(236,IR,IY,IS)=UGNWLNR(1,IR,IY)+UGNWLNR(2,IR,IY)
    z[236] = dfd['UGNWLNR'].loc[1] + dfd['UGNWLNR'].loc[2]
    #     Other Intermittant                                        
    #T59(240,IR,IY,IS)=UGENIN(IR,IY)
    z[240] = dfd['UGENIN']
    #     Distributed Generation 7/                                 
    #T59(96,IR,IY,IS)=UGENDG(IR,IY)
    z[96] = dfd['UGENDG']
    #    Other
    #T59 new line to break out non-biogenic MSW
    z[270] = dfd['WNGMSEL']
   #       Total                                                   
    #T59(97,IR,IY,IS)=T59( 80,IR,IY,IS)+T59( 84,IR,IY,IS)+T59( 85,IR,IY,IS)+T59( 89,IR,IY,IS)+T59(141,IR,IY,IS)+FSUM(T59(92,IR,IY,IS),5)+
    #FSUM(T59(227,IR,IY,IS),14)+FSUM(T59(242,IR,IY,IS),15) + T59(265,IR,IY,IS)
    z[97] = z[80] + z[84] + z[85] + z[89] + z[141] + z[92]+z[93]+z[94]+z[95]+z[96]+\
            z[227]+z[228]+z[229]+z[230]+z[231]+z[232]+z[233]+z[234]+z[235]+z[236]+z[237]+z[238]+z[239]+z[240]+\
            z[250]+z[251]+z[252]+z[253]+z[254]+z[255]+z[256]+z[265]+z[270]
    #   Curtailments                                                
    #       Wind                                                    
    #T59(259)=0
    z[259] = (dfd['CURTAIL'].loc[1] + dfd['CURTAIL'].loc[2] + dfd['CURTAIL'].loc[3]) / 1000.0
    #       Solar                                                   
    #T59(260)=0
    z[260] = (dfd['CURTAIL'].loc[4] + dfd['CURTAIL'].loc[5] + dfd['CURTAIL'].loc[6] + dfd['CURTAIL'].loc[7] + dfd['CURTAIL'].loc[8]) / 1000.0
    #   Total Curtailments                                                
    z[271] = z[259] + z[260]
    #   Capacity with Sequestration
    #     Coal                                                      
    #T59(98,IR,IY,IS)=UCAPISU(IR,IY)+UCAPISN(IR,IY)+UCAPSQU(IR,IY)+UCAPSQN(IR,IY)+UCAPPQU(IR,IY)+UCAPPQN(IR,IY)
    z[98] = dfd['UCAPISU'] + dfd['UCAPISN'] + dfd['UCAPSQU'] + dfd['UCAPSQN'] + dfd['UCAPPQU'] + dfd['UCAPPQN']
    #       Advanced with Partial Sequestration                     
    #T59(135,IR,IY,IS)=UCAPPQU(IR,IY)+UCAPPQN(IR,IY)
    z[135] = dfd['UCAPPQU'] + dfd['UCAPPQN']
    #       Advanced with Sequestration                             
    #T59(99,IR,IY,IS)=UCAPISU(IR,IY)+UCAPISN(IR,IY)
    z[99] = dfd['UCAPISU'] + dfd['UCAPISN']
    #       Conventional (Retrofits)                                
    #T59(100,IR,IY,IS)=UCAPSQU(IR,IY)+UCAPSQN(IR,IY)
    z[100] = dfd['UCAPSQU'] + dfd['UCAPSQN']
    #     Combined Cycle                                            
    #T59(101,IR,IY,IS)=UCAPASU(IR,IY)+UCAPASN(IR,IY)+UCAPA2U(IR,IY)+UCAPA2N(IR,IY)
    z[101] = dfd['UCAPASU'] + dfd['UCAPASN'] + dfd['UCAPA2U'] + dfd['UCAPA2N']
    #       Advanced with Sequestration                             
    #T59(102,IR,IY,IS)=UCAPASU(IR,IY)+UCAPASN(IR,IY)
    z[102] = dfd['UCAPASU'] + dfd['UCAPASN']
    #       Conventional (Retrofits)                                
    #T59(103,IR,IY,IS)=UCAPA2U(IR,IY)+UCAPA2N(IR,IY)
    z[103] = dfd['UCAPA2U'] + dfd['UCAPA2N']
    #       Biomass with CCS
    # defined above - z[148]    
    #   Total Capacity with Sequestration                           
    #T59(104,IR,IY,IS)=T59(98,IR,IY,IS)+T59(101,IR,IY,IS)
    z[104] = z[98] + z[101] + z[148]
    #   Generation with Sequestration 
    #     Coal                                                      
    #T59(105,IR,IY,IS)=UGENIS(IR,IY)+UGENSQ(IR,IY)+UGENPQ(IR,IY)-(UGENIS_ALT(IR,IY)+UGENSQ_ALT(IR,IY)+UGENPQ_ALT(IR,IY))
    z[105] = dfd['UGENIS'] + dfd['UGENSQ'] + dfd['UGENPQ'] - (dfd['UGENSQ_ALT'] + dfd['UGENPQ_ALT'])
    #       Advanced with Partial Sequestration                     
    #T59(136,IR,IY,IS)=UGENPQ(IR,IY)-UGENPQ_ALT(IR,IY)
    z[136] = dfd['UGENPQ'] - dfd['UGENPQ_ALT']
    #       Advanced with Sequestration                             
    #T59(106,IR,IY,IS)=UGENIS(IR,IY)-UGENIS_ALT(IR,IY)
    z[106] = dfd['UGENIS'] - dfd['UGENIS_ALT']
    #       Conventional (Retrofits)                                
    #T59(107,IR,IY,IS)=UGENSQ(IR,IY)-UGENSQ_ALT(IR,IY)
    z[107] = dfd['UGENSQ'] - dfd['UGENSQ_ALT']
    #     Combined Cycle                                            
    #T59(108,IR,IY,IS)=UGENCS(IR,IY)-UGENCS_ALT(IR,IY)+UGENA2(IR,IY)-UGENA2_ALT(IR,IY)
    z[108] = dfd['UGENCS'] -dfd['UGENCS_ALT']+ dfd['UGENA2'] -dfd['UGENA2_ALT']
    #       Advanced with Sequestration                             
    #T59(109,IR,IY,IS)=UGENCS(IR,IY)-UGENCS_ALT(IR,IY)
    z[109] = dfd['UGENCS'] - dfd['UGENCS_ALT']
    #       Conventional (Retrofits)                                
    #T59(110,IR,IY,IS)=UGENA2(IR,IY)-UGENA2_ALT(IR,IY)
    z[110] = dfd['UGENA2'] - dfd['UGENA2_ALT']
    #       Biomass with CCS
    # defined above - z[228]     
    #   Total Generation with Sequestration                         
    #T59(111,IR,IY,IS)=T59(105,IR,IY,IS)+T59(108,IR,IY,IS)
    z[111] = z[105] + z[108] + z[228]

    return z