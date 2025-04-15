# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_030(dfd, table_spec, table_id):
    """Fill table  Residential Sector Equipment Stock and Efficiency, and Distr
   
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



    #   Residential Sector Equipment Stock and Efficiency, and Distr
    #    Equipment Stock Data                                       
    #                                                               
    #   Equipment Stock (million units)                             
    #   Equipment Stock (million units)                             
    #    Main Space Heaters                                         

    #      Electric Heat Pumps                                      
    #T30(1,IY,IS)=RSHTRS(IY,1)*.000001
    z[1] = dfd['RSHTRS'].loc[1]  * .000001
 
    #      Electric Other                                           
    #T30(2,IY,IS)=RSHTRS(IY,2)*.000001
    z[2] = dfd['RSHTRS'].loc[2]  * .000001
 
    #      Natural Gas Heat Pumps                                   
    #T30(3,IY,IS)=RSHTRS(IY,3)*.000001
    z[3] = dfd['RSHTRS'].loc[3]  * .000001
 
    #      Natural Gas Other                                        
    #T30(4,IY,IS)=RSHTRS(IY,4)*.000001
    z[4] = dfd['RSHTRS'].loc[4]  * .000001
 
    #      Distillate Fuel Oil                                      
    #T30(5,IY,IS)=RSHTRS(IY,5)*.000001
    z[5] = dfd['RSHTRS'].loc[5]  * .000001
 
    #      Propane                                                  
    #T30(6,IY,IS)=RSHTRS(IY,6)*.000001
    z[6] = dfd['RSHTRS'].loc[6]  * .000001
 
    #      Kerosene                                                 
    #T30(7,IY,IS)=RSHTRS(IY,7)*.000001
    z[7] = dfd['RSHTRS'].loc[7]  * .000001
 
    #      Wood Stoves                                              
    #T30(8,IY,IS)=RSHTRS(IY,8)*.000001
    z[8] = dfd['RSHTRS'].loc[8]  * .000001
 
    #      Geothermal Heat Pumps                                    
    #T30(9,IY,IS)=RSHTRS(IY,9)*.000001
    z[9] = dfd['RSHTRS'].loc[9] * .000001
 
    #        Total                                                  
    #T30(10,IY,IS)=FSUM(T30(1,IY,IS),9)
    z[10] = z[1]+z[2]+z[3]+z[4]+z[5]+z[6]+z[7]+z[8]+z[9]
 
    #                                                               

    #    Space Cooling (million units)                              
 
    #      Electric Heat Pumps                                      
    #T30(11,IY,IS)=RSCOOLERS(IY,1)*.000001
    z[11] = dfd['RSCOOLERS'].loc[1]  * .000001
 
    #      Natural Gas Heat Pumps                                   
    #T30(12,IY,IS)=RSCOOLERS(IY,2)*.000001
    z[12] = dfd['RSCOOLERS'].loc[2]  * .000001
 
    #      Geothermal Heat Pumps                                    
    #T30(13,IY,IS)=RSCOOLERS(IY,3)*.000001
    z[13] = dfd['RSCOOLERS'].loc[3]  * .000001
 
    #      Central Air Conditioners                                 
    #T30(14,IY,IS)=RSCOOLERS(IY,4)*.000001
    z[14] = dfd['RSCOOLERS'].loc[4]  * .000001
 
    #      Room Air Conditioners                                    
    #T30(15,IY,IS)=RSCOOLERS(IY,5)*.000001
    z[15] = dfd['RSCOOLERS'].loc[5]  * .000001
 
    #        Total                                                  
    #T30(16,IY,IS)=FSUM(T30(11,IY,IS),5)
    z[16] = z[11]+z[12]+z[13]+z[14]+z[15]

    #                                                               
 
    #    Water Heaters (million units)                              
 
    #      Electric                                                 
    #T30(17,IY,IS)=RSWATER(IY,2)*.000001
    z[17] = dfd['RSWATER'].loc[2]  * .000001
 
    #      Natural Gas                                              
    #T30(18,IY,IS)=RSWATER(IY,1)*.000001
    z[18] = dfd['RSWATER'].loc[1]  * .000001
 
    #      Distillate Fuel Oil                                      
    #T30(19,IY,IS)=RSWATER(IY,3)*.000001
    z[19] = dfd['RSWATER'].loc[3]  * .000001
 
    #      Propane                                                  
    #T30(20,IY,IS)=RSWATER(IY,4)*.000001
    z[20] = dfd['RSWATER'].loc[4]  * .000001
 
    #      Solar Thermal                                            
    #T30(21,IY,IS)=RSWATER(IY,5)*.000001
    z[21] = dfd['RSWATER'].loc[5]  * .000001
 
    #        Total                                                  
    #T30(22,IY,IS)=FSUM(T30(17,IY,IS),5)
    z[22] = z[17]+z[18]+z[19]+z[20]+z[21]

    #                                                               
 
    #    Cooking Equipment (million units) 1/                       
 
    #      Electric                                                 
    #T30(23,IY,IS)=RSCOOK(IY,3)*.000001
    z[23] = dfd['RSCOOK'].loc[3]  * .000001
 
    #      Natural Gas                                              
    #T30(24,IY,IS)=RSCOOK(IY,1)*.000001
    z[24] = dfd['RSCOOK'].loc[1]  * .000001
 
    #      Propane                                                  
    #T30(25,IY,IS)=RSCOOK(IY,2)*.000001
    z[25] = dfd['RSCOOK'].loc[2]  * .000001
 
    #        Total                                                  
    #T30(26,IY,IS)=FSUM(T30(23,IY,IS),3)
    z[26] = z[23]+z[24]+z[25]

    #                                                               
 
    #    Clothes Dryers (million units)                             
 
    #      Electric                                                 
    #T30(27,IY,IS)=RSDRY(IY,2)*.000001
    z[27] = dfd['RSDRY'].loc[2]  * .000001
 
    #      Natural Gas                                              
    #T30(28,IY,IS)=RSDRY(IY,1)*.000001
    z[28] = dfd['RSDRY'].loc[1]  * .000001
 
    #        Total                                                  
    #T30(29,IY,IS)=FSUM(T30(27,IY,IS),2)
    z[29] = z[27]+z[28]

    #                                                               
 
    #    Other Appliances (million units)                           
 
    #      Refrigerators                                            
    #T30(30,IY,IS)=RSREF(IY)*.000001
    z[30] = dfd['RSREF'] * .000001
 
    #      Freezers                                                 
    #T30(31,IY,IS)=RSFRZ(IY)*.000001
    z[31] = dfd['RSFRZ'] * .000001
 
    #                                                               
    #                                                               
    #   Stock Average Equipment Efficiency                          
 
    #    Main Space Heaters                                         
 
    #      Electric Heat Pumps (HSPF)                               
    #T30(32,IY,IS)=RSEEFHT(IY,1)*3.412
    z[32] = dfd['RSEEFHT'].loc[1] *3.412
 
    #      Natural Gas Heat Pumps (GCOP)                            
    #T30(33,IY,IS)=RSEEFHT(IY,2)
    z[33] = dfd['RSEEFHT'].loc[2] 
 
    #      Geothermal Heat Pumps (COP)                              
    #T30(34,IY,IS)=RSEEFHT(IY,3)
    z[34] = dfd['RSEEFHT'].loc[3] 
 
    #      Natural Gas Furnace (AFUE)                               
    #T30(35,IY,IS)=RSEEFHT(IY,4)
    z[35] = dfd['RSEEFHT'].loc[4] 
 
    #      Distillate Furnace (AFUE)                                
    #T30(36,IY,IS)=RSEEFHT(IY,5)
    z[36] = dfd['RSEEFHT'].loc[5] 
 
    #                                                               
 
    #    Space Cooling                                              
 
    #      Electric Heat Pumps (SEER)                               
    #T30(37,IY,IS)=RSEEFCL(IY,1)*3.412
    z[37] = dfd['RSEEFCL'].loc[1] *3.412
 
    #      Natural Gas Heat Pumps (GCOP)                            
    #T30(38,IY,IS)=RSEEFCL(IY,2)
    z[38] = dfd['RSEEFCL'].loc[2] 
 
    #      Geothermal Heat Pumps (EER)                              
    #T30(39,IY,IS)=RSEEFCL(IY,3)
    z[39] = dfd['RSEEFCL'].loc[3] 
 
    #      Central Air Conditioners (SEER)                          
    #T30(40,IY,IS)=RSEEFCL(IY,4)*3.412
    z[40] = dfd['RSEEFCL'].loc[4] *3.412
 
    #      Room Air Conditioners (EER)                              
    #T30(41,IY,IS)=RSEEFCL(IY,5)*3.412
    z[41] = dfd['RSEEFCL'].loc[5] *3.412
 
    #                                                               
 
    #    Water Heaters                                              
 
    #      Electric (EF)                                            
    #T30(42,IY,IS)=RSEEFHW(IY,1)
    z[42] = dfd['RSEEFHW'].loc[1] 
 
    #      Natural Gas (EF)                                         
    #T30(43,IY,IS)=RSEEFHW(IY,2)
    z[43] = dfd['RSEEFHW'].loc[2] 
 
    #      Distillate Fuel Oil (EF)                                 
    #T30(44,IY,IS)=RSEEFHW(IY,3)
    z[44] = dfd['RSEEFHW'].loc[3] 
 
    #      Propane (EF)                                             
    #T30(45,IY,IS)=RSEEFHW(IY,4)
    z[45] = dfd['RSEEFHW'].loc[4] 
 
    #                                                               
 
    #    Other Appliances (kilowatthours per year) 2/               
 
    #      Refrigerators                                            
    #T30(46,IY,IS)=RSEEFRF(IY)
    z[46] = dfd['RSEEFRF']
 
    #      Freezers                                                 
    #T30(47,IY,IS)=RSEEFFZ(IY)
    z[47] = dfd['RSEEFFZ']
 
    #                                                               
 
    #   Building Shell Efficiency Index 3/                          
 
    #    Space Heating                                              
 
    #      Pre-2020 Homes                                           
    #T30(48,IY,IS)=HSHELL1(IY)
    z[48] = dfd['HSHELL1']
 
    #      New Construction                                         
    #T30(49,IY,IS)=HSHELL2(IY)
    z[49] = dfd['HSHELL2']
 
    #        All Homes                                              
    #T30(50,IY,IS)=HSHELL3(IY)
    z[50] = dfd['HSHELL3']
    #                                                               
 
    #    Space Cooling                                              
 
    #      Pre-2020 Homes                                           
    #T30(51,IY,IS)=CSHELL1(IY)
    z[51] = dfd['CSHELL1']
 
    #      New Construction                                         
    #T30(52,IY,IS)=CSHELL2(IY)
    z[52] = dfd['CSHELL2']
 
    #        All Homes                                              
    #T30(53,IY,IS)=CSHELL3(IY)
    z[53] = dfd['CSHELL3']
 
    #                                                               
    #    Distributed Generation and                                 
 
    #    Combined Heat and Power                                    
 
    #      Generating Capacity (gigawatts) 4/                       
 
    #        Natural Gas Fuel Cells                                 
    #T30(54,IY,IS)=CGRESCAP(11,IY,3)/1000.
    z[54] = dfd['CGRESCAP'].loc[11].loc[3]  / 1000.
 
    #        Solar Photovoltaic                                     
    #T30(55,IY,IS)=CGRESCAP(11,IY,8)/1000.
    z[55] = dfd['CGRESCAP'].loc[11].loc[8]  / 1000.
 
    #        Wind                                                   
    #T30(56,IY,IS)=CGRESCAP(11,IY,11)/1000.
    z[56] = dfd['CGRESCAP'].loc[11].loc[11] / 1000.
 
    #          Total                                                
    #T30(57,IY,IS)=FSUM(T30(54,IY,IS),3)
    z[57] = z[54]+z[55]+z[56]

    #      Net Generation (billion kilowatthours)                   
 
    #        Natural Gas Fuel Cells                                 
    #T30(58,IY,IS)=(CGRESGEN(11,IY,3,1)+CGRESGEN(11,IY,3,2))/1000.
    z[58] = (dfd['CGRESGEN'].loc[11].loc[3].loc[1] + dfd['CGRESGEN'].loc[11].loc[3].loc[2]) / 1000.
 
    #        Solar Photovoltaic                                     
    #T30(59,IY,IS)=(CGRESGEN(11,IY,8,1)+CGRESGEN(11,IY,8,2))/1000.
    z[59] = (dfd['CGRESGEN'].loc[11].loc[8].loc[1] + dfd['CGRESGEN'].loc[11].loc[8].loc[2]) / 1000.
 
    #        Wind                                                   
    #T30(60,IY,IS)=(CGRESGEN(11,IY,11,1)+CGRESGEN(11,IY,11,2))/1000.
    z[60] = (dfd['CGRESGEN'].loc[11].loc[11].loc[1] + dfd['CGRESGEN'].loc[11].loc[11].loc[2]) / 1000.
 
    #          Total                                                
    #T30(61,IY,IS)=FSUM(T30(58,IY,IS),3)
    z[61] = z[58]+z[59]+z[60]

    #        Disposition                                            
 
    #          Sales to the Grid                                    
    #T30(62,IY,IS)=(CGRESGEN(11,IY,3,1)+CGRESGEN(11,IY,8,1)+CGRESGEN(11,IY,11,1))/1000.
    z[62] = (dfd['CGRESGEN'].loc[11].loc[3].loc[1] + dfd['CGRESGEN'].loc[11].loc[8].loc[1] + dfd['CGRESGEN'].loc[11].loc[11].loc[1]) / 1000.
 
    #          Generation for Own Use                               
    #T30(63,IY,IS)=(CGRESGEN(11,IY,3,2)+CGRESGEN(11,IY,8,2)+CGRESGEN(11,IY,11,2))/1000.
    z[63] = (dfd['CGRESGEN'].loc[11].loc[3].loc[2] + dfd['CGRESGEN'].loc[11].loc[8].loc[2] + dfd['CGRESGEN'].loc[11].loc[11].loc[2]) / 1000.
 
    #      Energy Input (trillion Btu)                              
 
    #        Natural Gas Fuel Cells                                 
    #T30(64,IY,IS)=CGRESQ(11,IY,3)
    z[64] = dfd['CGRESQ'].loc[11].loc[3] 
 
    #        Solar Photovoltaic                                     
    #T30(65,IY,IS)=CGRESQ(11,IY,8)
    z[65] = dfd['CGRESQ'].loc[11].loc[8] 
 
    #        Wind                                                   
    #T30(66,IY,IS)=CGRESQ(11,IY,11)
    z[66] = dfd['CGRESQ'].loc[11].loc[11]
 
    #          Total              
    #T30(67,IY,IS)=FSUM(T30(64,IY,IS),3)
    z[67] = z[64]+z[65]+z[66]
    
    return z


