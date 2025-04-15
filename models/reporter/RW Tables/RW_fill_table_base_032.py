# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_032(dfd, table_spec, table_id):
    """Fill table   Commercial Sector Energy Consumption, Floorspace, Equipment 
   
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

    MNUMCR=dfd['MNUMCR_rwpre']


    #   Commercial Sector Energy Consumption, Floorspace, Equipment 
    #    Indicators                                                 
    #                                                               
    #   Commercial Building Delivered                               
    #    Energy Consumption (quadrillion Btu) 1/                    

    #    Assembly                                                   
    #T32(1,IY,IS)=CMFINALENDUSECON(1,IY)
    z[1] = dfd['CMFINALENDUSECON'].loc[1]
 
    #    Education                                                  
    #T32(2,IY,IS)=CMFINALENDUSECON(2,IY)
    z[2] = dfd['CMFINALENDUSECON'].loc[2]
 
    #    Food Sales                                                 
    #T32(3,IY,IS)=CMFINALENDUSECON(3,IY)
    z[3] = dfd['CMFINALENDUSECON'].loc[3]
 
    #    Food Service                                               
    #T32(4,IY,IS)=CMFINALENDUSECON(4,IY)
    z[4] = dfd['CMFINALENDUSECON'].loc[4]
 
    #    Health Care                                                
    #T32(5,IY,IS)=CMFINALENDUSECON(5,IY)
    z[5] = dfd['CMFINALENDUSECON'].loc[5]
 
    #    Lodging                                                    
    #T32(6,IY,IS)=CMFINALENDUSECON(6,IY)
    z[6] = dfd['CMFINALENDUSECON'].loc[6]
 
    #    Office - Large                                             
    #T32(7,IY,IS)=CMFINALENDUSECON(7,IY)
    z[7] = dfd['CMFINALENDUSECON'].loc[7]
 
    #    Office - Small                                             
    #T32(8,IY,IS)=CMFINALENDUSECON(8,IY)
    z[8] = dfd['CMFINALENDUSECON'].loc[8]
 
    #    Mercantile/Service                                         
    #T32(9,IY,IS)=CMFINALENDUSECON(9,IY)
    z[9] = dfd['CMFINALENDUSECON'].loc[9]
 
    #    Warehouse                                                  
    #T32(10,IY,IS)=CMFINALENDUSECON(10,IY)
    z[10] = dfd['CMFINALENDUSECON'].loc[10]
 
    #    Other                                                      
    #T32(11,IY,IS)=CMFINALENDUSECON(11,IY)
    z[11] = dfd['CMFINALENDUSECON'].loc[11]
 
    #      Total                                                    
    #T32(12,IY,IS)=FSUM(T32(1,IY,IS),11)
    z[12] = z[1]+z[2]+z[3]+z[4]+z[5]+z[6]+z[7]+z[8]+z[9]+z[10]+z[11]
 
    #                                                               
 
    #   Commercial Building Floorspace                              
    #    (billion square feet)                                      
 
    #    Assembly                                                   
    #T32(13,IY,IS)=CMSURVFLOORTOT(1,IY)+CMNEWFLRSPACE(1,IY)
    z[13] = dfd['CMSURVFLOORTOT'].loc[1] + dfd['CMNEWFLRSPACE'].loc[1]
 
    #    Education                                                  
    #T32(14,IY,IS)=CMSURVFLOORTOT(2,IY)+CMNEWFLRSPACE(2,IY)
    z[14] = dfd['CMSURVFLOORTOT'].loc[2] + dfd['CMNEWFLRSPACE'].loc[2]
 
    #    Food Sales                                                 
    #T32(15,IY,IS)=CMSURVFLOORTOT(3,IY)+CMNEWFLRSPACE(3,IY)
    z[15] = dfd['CMSURVFLOORTOT'].loc[3] + dfd['CMNEWFLRSPACE'].loc[3]
 
    #    Food Service                                               
    #T32(16,IY,IS)=CMSURVFLOORTOT(4,IY)+CMNEWFLRSPACE(4,IY)
    z[16] = dfd['CMSURVFLOORTOT'].loc[4] + dfd['CMNEWFLRSPACE'].loc[4]
 
    #    Health Care                                                
    #T32(17,IY,IS)=CMSURVFLOORTOT(5,IY)+CMNEWFLRSPACE(5,IY)
    z[17] = dfd['CMSURVFLOORTOT'].loc[5] + dfd['CMNEWFLRSPACE'].loc[5]
 
    #    Lodging                                                    
    #T32(18,IY,IS)=CMSURVFLOORTOT(6,IY)+CMNEWFLRSPACE(6,IY)
    z[18] = dfd['CMSURVFLOORTOT'].loc[6] + dfd['CMNEWFLRSPACE'].loc[6]
 
    #    Office - Large                                             
    #T32(19,IY,IS)=CMSURVFLOORTOT(7,IY)+CMNEWFLRSPACE(7,IY)
    z[19] = dfd['CMSURVFLOORTOT'].loc[7] + dfd['CMNEWFLRSPACE'].loc[7]
 
    #    Office - Small                                             
    #T32(20,IY,IS)=CMSURVFLOORTOT(8,IY)+CMNEWFLRSPACE(8,IY)
    z[20] = dfd['CMSURVFLOORTOT'].loc[8] + dfd['CMNEWFLRSPACE'].loc[8]
 
    #    Mercantile/Service                                         
    #T32(21,IY,IS)=CMSURVFLOORTOT(9,IY)+CMNEWFLRSPACE(9,IY)
    z[21] = dfd['CMSURVFLOORTOT'].loc[9] + dfd['CMNEWFLRSPACE'].loc[9]
 
    #    Warehouse                                                  
    #T32(22,IY,IS)=CMSURVFLOORTOT(10,IY)+CMNEWFLRSPACE(10,IY)
    z[22] = dfd['CMSURVFLOORTOT'].loc[10] + dfd['CMNEWFLRSPACE'].loc[10]
 
    #    Other                                                      
    #T32(23,IY,IS)=CMSURVFLOORTOT(11,IY)+CMNEWFLRSPACE(11,IY)
    z[23] = dfd['CMSURVFLOORTOT'].loc[11] + dfd['CMNEWFLRSPACE'].loc[11]
 
    #      Total                                                    
    #T32(24,IY,IS)=FSUM(T32(13,IY,IS),11)
    z[24] = z[13]+z[14]+z[15]+z[16]+z[17]+z[18]+z[19]+z[20]+z[21]+z[22]+z[23]
    #                                                               
 
    #   Stock Average Equipment Efficiency 2/                       
 
    #                                                               
    #    Space Heating                                              
 
    #      Electricity                                              
    #T32(25,IY,IS)=CMUSAVGEFF(1,1,IY)
    z[25] = dfd['CMUSAVGEFF'].loc[1].loc[1]
 
    #      Natural Gas                                              
    #T32(26,IY,IS)=CMUSAVGEFF(1,2,IY)
    z[26] = dfd['CMUSAVGEFF'].loc[1].loc[2]
 
    #      Distillate Fuel Oil                                      
    #T32(27,IY,IS)=CMUSAVGEFF(1,3,IY)
    z[27] = dfd['CMUSAVGEFF'].loc[1].loc[3]
 
    #                                                               
 
    #    Space Cooling                                              
 
    #      Electricity                                              
    #T32(28,IY,IS)=CMUSAVGEFF(2,1,IY)
    z[28] = dfd['CMUSAVGEFF'].loc[2].loc[1]
 
    #      Natural Gas                                              
    #T32(29,IY,IS)=CMUSAVGEFF(2,2,IY)
    z[29] = dfd['CMUSAVGEFF'].loc[2].loc[2]
 
    #                                                               
 
    #    Water Heating                                              
 
    #      Electricity                                              
    #T32(30,IY,IS)=CMUSAVGEFF(3,1,IY)
    z[30] = dfd['CMUSAVGEFF'].loc[3].loc[1]
 
    #      Natural Gas                                              
    #T32(31,IY,IS)=CMUSAVGEFF(3,2,IY)
    z[31] = dfd['CMUSAVGEFF'].loc[3].loc[2]
 
    #      Distillate Fuel Oil                                      
    #T32(32,IY,IS)=CMUSAVGEFF(3,3,IY)
    z[32] = dfd['CMUSAVGEFF'].loc[3].loc[3]
 
    #                                                               
 
    #    Ventilation (cubic feet per minute per Btu) 3/             
 
    #      Electricity                                              
    #T32(33,IY,IS)=CMUSAVGEFF(4,1,IY)
    z[33] = dfd['CMUSAVGEFF'].loc[4].loc[1]
 
    #                                                               
 
    #    Cooking                                                    
 
    #      Electricity                                              
    #T32(34,IY,IS)=CMUSAVGEFF(5,1,IY)
    z[34] = dfd['CMUSAVGEFF'].loc[5].loc[1]
 
    #      Natural Gas                                              
    #T32(35,IY,IS)=CMUSAVGEFF(5,2,IY)
    z[35] = dfd['CMUSAVGEFF'].loc[5].loc[2]
 
    #                                                               
 
    #    Lighting Efficacy 4/                                       
    #      (efficacy in lumens per watt)                            
 
    #      Electricity                                              
    #T32(36,IY,IS)=CMUSAVGEFF(6,1,IY)
    z[36] = dfd['CMUSAVGEFF'].loc[6].loc[1]
    #                                                               
 
    #    Refrigeration                                              
 
    #      Electricity                                              
    #T32(37,IY,IS)=CMUSAVGEFF(7,1,IY)
    z[37] = dfd['CMUSAVGEFF'].loc[7].loc[1]
    #                                                               

    #                                                               
 
    #    Computing                                                  
 
    #      Electricity                                              
    #T32(38,IY,IS)=CMUSAVGEFF(8,1,IY)
    z[38] = dfd['CMUSAVGEFF'].loc[8].loc[1]
 
    #                                                               
 
    #    Office Equipment                                           
 
    #      Electricity                                              
    #T32(39,IY,IS)=CMUSAVGEFF(9,1,IY)
    z[39] = dfd['CMUSAVGEFF'].loc[9].loc[1]
 
    #                                                               

    #    Distributed Generation and                                 

    #    Combined Heat and Power                                    

    #      Generating Capacity (gigawatts) 5/                       

    #        Petroleum                                              
    #T32(40,IY,IS)=CGCOMMCAP(11,IY,2)/1000.
    z[40] = dfd['CGCOMMCAP'].loc[11].loc[2]  / 1000.
 
    #        Natural Gas                                            
    #T32(41,IY,IS)=CGCOMMCAP(11,IY,3)/1000.
    z[41] = dfd['CGCOMMCAP'].loc[11].loc[3]  / 1000.
 
    #        Solar Photovoltaic                                     
    #T32(42,IY,IS)=CGCOMMCAP(11,IY,8)/1000.
    z[42] = dfd['CGCOMMCAP'].loc[11].loc[8]  / 1000.
 
    #        Wind                                                   
    #T32(43,IY,IS)=CGCOMMCAP(11,IY,11)/1000.
    z[43] = dfd['CGCOMMCAP'].loc[MNUMCR].loc[11] / 1000.
 
    #        Other 6/                                               
    #T32(44,IY,IS)=(CGCOMMCAP(11,IY,1)+CGCOMMCAP(11,IY,4)+CGCOMMCAP(11,IY,5)+CGCOMMCAP(11,IY,6)+CGCOMMCAP(11,IY,7)+CGCOMMCAP(11,IY,9)+CGCOMMCAP(11,IY,10)+CGCOMMCAP(11,IY,12))/1000.
    z[44] = (dfd['CGCOMMCAP'].loc[MNUMCR].loc[1] + dfd['CGCOMMCAP'].loc[MNUMCR].loc[4] + \
             dfd['CGCOMMCAP'].loc[MNUMCR].loc[5] + dfd['CGCOMMCAP'].loc[MNUMCR].loc[6] + \
             dfd['CGCOMMCAP'].loc[MNUMCR].loc[7] + dfd['CGCOMMCAP'].loc[MNUMCR].loc[9] + \
             dfd['CGCOMMCAP'].loc[MNUMCR].loc[10] + dfd['CGCOMMCAP'].loc[MNUMCR].loc[12] ) / 1000.
 
    #          Total                                                
    #T32(45,IY,IS)=FSUM(T32(40,IY,IS),5)
    z[45] = z[40]+z[41]+z[42]+z[43]+z[44]

    #      Net Generation (billion kilowatthours)                   
 
    #        Petroleum                                              
    #T32(46,IY,IS)=(CGCOMMGEN(11,IY,2,1)+CGCOMMGEN(11,IY,2,2))/1000.
    z[46] = (dfd['CGCOMMGEN'].loc[MNUMCR].loc[2].loc[1] + dfd['CGCOMMGEN'].loc[11].loc[2].loc[2]) / 1000.
 
    #        Natural Gas                                            
    #T32(47,IY,IS)=(CGCOMMGEN(11,IY,3,1)+CGCOMMGEN(11,IY,3,2))/1000.
    z[47] = (dfd['CGCOMMGEN'].loc[MNUMCR].loc[3].loc[1] + dfd['CGCOMMGEN'].loc[MNUMCR].loc[3].loc[2]) / 1000.
 
    #        Solar Photovoltaic                                     
    #T32(48,IY,IS)=(CGCOMMGEN(11,IY,8,1)+CGCOMMGEN(11,IY,8,2))/1000.
    z[48] = (dfd['CGCOMMGEN'].loc[MNUMCR].loc[8].loc[1] + dfd['CGCOMMGEN'].loc[MNUMCR].loc[8].loc[2]) / 1000.
 
    #        Wind                                                   
    #T32(49,IY,IS)=(CGCOMMGEN(11,IY,11,1)+CGCOMMGEN(11,IY,11,2))/1000.
    z[49] = (dfd['CGCOMMGEN'].loc[MNUMCR].loc[11].loc[1] + dfd['CGCOMMGEN'].loc[MNUMCR].loc[11].loc[2]) / 1000.
 
    #        Other 6/                                               
    #T32(50,IY,IS)=(CGCOMMGEN(11,IY,1,1)+CGCOMMGEN(11,IY,4,1)+CGCOMMGEN(11,IY,5,1)+CGCOMMGEN(11,IY,6,1)+CGCOMMGEN(11,IY,7,1)+CGCOMMGEN(11,IY,9,1)+CGCOMMGEN(11,IY,10,1)+CGCOMMGEN(11,IY,12,1)+CGCOMMGEN(11,IY,1,2)+CGCOMMGEN(11,IY,4,2)+CGCOMMGEN(11,IY,5,2)+CGCOMMGEN(11,IY,6,2)+CGCOMMGEN(11,IY,7,2)+CGCOMMGEN(11,IY,9,2)+CGCOMMGEN(11,IY,10,2)+CGCOMMGEN(11,IY,12,2))/1000.
    z[50] = (dfd['CGCOMMGEN'].loc[MNUMCR].loc[1].loc[1] + dfd['CGCOMMGEN'].loc[MNUMCR].loc[4].loc[1] + \
             dfd['CGCOMMGEN'].loc[MNUMCR].loc[5].loc[1] + dfd['CGCOMMGEN'].loc[MNUMCR].loc[6].loc[1] + \
             dfd['CGCOMMGEN'].loc[MNUMCR].loc[7].loc[1] + dfd['CGCOMMGEN'].loc[MNUMCR].loc[9].loc[1] + \
             dfd['CGCOMMGEN'].loc[MNUMCR].loc[10].loc[1] + dfd['CGCOMMGEN'].loc[MNUMCR].loc[12].loc[1] + \
             dfd['CGCOMMGEN'].loc[MNUMCR].loc[1].loc[2] + dfd['CGCOMMGEN'].loc[MNUMCR].loc[4].loc[2] + \
             dfd['CGCOMMGEN'].loc[MNUMCR].loc[5].loc[2] + dfd['CGCOMMGEN'].loc[MNUMCR].loc[6].loc[2] + \
             dfd['CGCOMMGEN'].loc[MNUMCR].loc[7].loc[2] + dfd['CGCOMMGEN'].loc[MNUMCR].loc[9].loc[2] + \
             dfd['CGCOMMGEN'].loc[MNUMCR].loc[10].loc[2] + dfd['CGCOMMGEN'].loc[MNUMCR].loc[12].loc[2] ) / 1000.
 
    #          Total                                                
    #T32(51,IY,IS)=FSUM(T32(46,IY,IS),5)
    z[51] = z[46]+z[47]+z[48]+z[49]+z[50]

    #        Disposition                                            


 
    #          Sales to the Grid                                    
    #T32(52,IY,IS)=(CGCOMMGEN(11,IY,1,1)+CGCOMMGEN(11,IY,2,1)+CGCOMMGEN(11,IY,3,1)+CGCOMMGEN(11,IY,4,1)+CGCOMMGEN(11,IY,5,1)+CGCOMMGEN(11,IY,6,1)+CGCOMMGEN(11,IY,7,1)+CGCOMMGEN(11,IY,8,1)+CGCOMMGEN(11,IY,9,1)+CGCOMMGEN(11,IY,10,1)+CGCOMMGEN(11,IY,11,1)+CGCOMMGEN(11,IY,12,1))/1000.
    z[52] = (dfd['CGCOMMGEN'].loc[MNUMCR].loc[1].loc[1] + dfd['CGCOMMGEN'].loc[MNUMCR].loc[2].loc[1] + \
             dfd['CGCOMMGEN'].loc[MNUMCR].loc[3].loc[1] + dfd['CGCOMMGEN'].loc[MNUMCR].loc[4].loc[1] + \
             dfd['CGCOMMGEN'].loc[MNUMCR].loc[5].loc[1] + dfd['CGCOMMGEN'].loc[MNUMCR].loc[6].loc[1] + \
             dfd['CGCOMMGEN'].loc[MNUMCR].loc[7].loc[1] + dfd['CGCOMMGEN'].loc[MNUMCR].loc[8].loc[1] + \
             dfd['CGCOMMGEN'].loc[MNUMCR].loc[9].loc[1] + dfd['CGCOMMGEN'].loc[MNUMCR].loc[10].loc[1] + \
             dfd['CGCOMMGEN'].loc[MNUMCR].loc[11].loc[1] + dfd['CGCOMMGEN'].loc[MNUMCR].loc[12].loc[1] ) / 1000.
 
    #          Generation for Own Use                               
    #T32(53,IY,IS)=(CGCOMMGEN(11,IY,1,2)+CGCOMMGEN(11,IY,2,2)+CGCOMMGEN(11,IY,3,2)+CGCOMMGEN(11,IY,4,2)+CGCOMMGEN(11,IY,5,2)+CGCOMMGEN(11,IY,6,2)+CGCOMMGEN(11,IY,7,2)+CGCOMMGEN(11,IY,8,2)+CGCOMMGEN(11,IY,9,2)+CGCOMMGEN(11,IY,10,2)+CGCOMMGEN(11,IY,11,2)+CGCOMMGEN(11,IY,12,2))/1000.
    z[53] = (dfd['CGCOMMGEN'].loc[MNUMCR].loc[1].loc[2] + dfd['CGCOMMGEN'].loc[MNUMCR].loc[2].loc[2] + \
             dfd['CGCOMMGEN'].loc[MNUMCR].loc[3].loc[2] + dfd['CGCOMMGEN'].loc[MNUMCR].loc[4].loc[2] + \
             dfd['CGCOMMGEN'].loc[MNUMCR].loc[5].loc[2] + dfd['CGCOMMGEN'].loc[MNUMCR].loc[6].loc[2] + \
             dfd['CGCOMMGEN'].loc[MNUMCR].loc[7].loc[2] + dfd['CGCOMMGEN'].loc[MNUMCR].loc[8].loc[2] + \
             dfd['CGCOMMGEN'].loc[MNUMCR].loc[9].loc[2] + dfd['CGCOMMGEN'].loc[MNUMCR].loc[10].loc[2] + \
             dfd['CGCOMMGEN'].loc[MNUMCR].loc[11].loc[2] + dfd['CGCOMMGEN'].loc[MNUMCR].loc[12].loc[2] ) / 1000.
 
    #      Energy Input (trillion Btu)                              


 
    #        Petroleum                                              
    #T32(54,IY,IS)=CGCOMMQ(11,IY,2)
    z[54] = dfd['CGCOMMQ'].loc[MNUMCR].loc[2] 
 
    #        Natural Gas                                            
    #T32(55,IY,IS)=CGCOMMQ(11,IY,3)
    z[55] = dfd['CGCOMMQ'].loc[MNUMCR].loc[3] 
 
    #        Solar Photovoltaic                                     
    #T32(56,IY,IS)=CGCOMMQ(11,IY,8)
    z[56] = dfd['CGCOMMQ'].loc[MNUMCR].loc[8] 
 
    #        Wind                                                   
    #T32(57,IY,IS)=CGCOMMQ(11,IY,11)
    z[57] = dfd['CGCOMMQ'].loc[MNUMCR].loc[11]
 
    #        Other 6/                                               
    #T32(58,IY,IS)=CGCOMMQ(11,IY,1)+CGCOMMQ(11,IY,4)+CGCOMMQ(11,IY,5)+CGCOMMQ(11,IY,6)+CGCOMMQ(11,IY,7)+CGCOMMQ(11,IY,9)+CGCOMMQ(11,IY,10)+CGCOMMQ(11,IY,12)
    z[58] = dfd['CGCOMMQ'].loc[MNUMCR].loc[1] + dfd['CGCOMMQ'].loc[MNUMCR].loc[4] + \
            dfd['CGCOMMQ'].loc[MNUMCR].loc[5] + dfd['CGCOMMQ'].loc[MNUMCR].loc[6] + \
            dfd['CGCOMMQ'].loc[MNUMCR].loc[7] + dfd['CGCOMMQ'].loc[MNUMCR].loc[9] + \
            dfd['CGCOMMQ'].loc[MNUMCR].loc[10] + dfd['CGCOMMQ'].loc[MNUMCR].loc[12]
 
    #          Total                                                
    #T32(59,IY,IS)=FSUM(T32(54,IY,IS),5)
    z[59] = z[54]+z[55]+z[56]+z[57]+z[58]
    
    return z

