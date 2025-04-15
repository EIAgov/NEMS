# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_124(dfd, table_spec, table_id):
    """Fill table Key Results for Residential and Commercial Sector Technology
   
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
    
  
    MNUMCR = int(dfd['MNUMCR_rwpre'])
    TRIL_TO_QUAD=dfd['TRIL_TO_QUAD_rwpre']



    #   Key Results for Residential and Commercial Sector Technology
    #    Energy Consumption                                         
    #                                                               
    #   Residential                                                 

    #   Energy Consumption                                          
    #     (quadrillion Btu)                                         

    #     Propane                                                   
    #T124(3,IY,IS)=QLGRS(11,IY)
    z[3] = dfd['QLGRS'].loc[MNUMCR]*TRIL_TO_QUAD
 
    #     Kerosene                                                  
    #T124(2,IY,IS)=QKSRS(11,IY)
    z[2] = dfd['QKSRS'].loc[MNUMCR]*TRIL_TO_QUAD
 
    #     Distillate Fuel Oil                                       
    #T124(1,IY,IS)=QDSRS(11,IY)
    z[1] = dfd['QDSRS'].loc[MNUMCR]*TRIL_TO_QUAD
 
    #       Petroleum Subtotal                                      
    #T124(4,IY,IS)=QTPRS(11,IY)
    z[4] = dfd['QTPRS'].loc[MNUMCR]*TRIL_TO_QUAD
 
    #     Natural Gas                                               
    #T124(5,IY,IS)=QNGRS(11,IY)
    z[5] = dfd['QNGRS'].loc[MNUMCR]*TRIL_TO_QUAD
 
    #     Coal                                                      
    #T124(6,IY,IS)=QCLRS(11,IY)
    z[6] = dfd['QCLRS'].loc[MNUMCR]*TRIL_TO_QUAD
 
    #     Renewable Energy 1/                                       
    #T124(7,IY,IS)=QBMRS(11,IY)
    z[7] = dfd['QBMRS'].loc[MNUMCR]*TRIL_TO_QUAD
 
    #     Electricity                                               
    #T124(8,IY,IS)=QELRS(11,IY)
    z[8] = dfd['QELRS'].loc[MNUMCR]*TRIL_TO_QUAD
 
    #       Delivered Energy                                        
    #T124(9,IY,IS)=QTSRS(11,IY)-QSTRS(11,IY)-QGERS(11,IY)-QPVRS(11,IY)
    z[9] = (dfd['QTSRS'].loc[MNUMCR] - dfd['QSTRS'].loc[MNUMCR] - dfd['QGERS'].loc[MNUMCR] - dfd['QPVRS'].loc[MNUMCR])*TRIL_TO_QUAD
 
    #     Electricity Related Losses                                
    #T124(10,IY,IS)=T4(57,IY,IS)
    # Create a place holder
    z[10] = z[9] * 0
 
    #       Total                                                   
    #T124(11,IY,IS)=FSUM(T124(9,IY,IS),2)
    # Create a place holder
    z[11] = z[9]+z[10]  # will be populated again in postprocessor_base
 
    #                                                               
 
    #   Delivered Energy Intensity                                  

    #   (million Btu per household)                                 

    #T124(12,IY,IS)=T4(6,IY,IS)
    # Create a place holder
    z[12] = z[9] * 0
    
    #                                                               

    #   Nonmarketed Renewables Consumption                          

    #   (quadrillion Btu)                                           
    #T124(13,IY,IS)=(RSH2OCON(IY,5)+RSHTRCON(IY,7)+RSCOOLCN(IY,2))/1000000000.+QPVRS(11,IY)+CGRESQ(11,IY,11)/1000.
    z[13] = (dfd['RSH2OCON'].loc[5] + dfd['RSHTRCON'].loc[7] + dfd['RSCOOLCN'].loc[2] ) / 1000000000. \
             + (dfd['QPVRS'].loc[MNUMCR] + dfd['CGRESQ'].loc[MNUMCR].loc[11]) *TRIL_TO_QUAD
 
    #                                                               
 
    #                                                               

    #   Commercial                                                  
    #                                                               
 
    #   Energy Consumption                                          
    #     (quadrillion Btu)                                         

 
    #     Propane                                                   
    #T124(17,IY,IS)=QLGCM(11,IY)
    z[17] = dfd['QLGCM'].loc[MNUMCR]*TRIL_TO_QUAD
 
    #     Motor Gasoline 2/                                         
    #T124(18,IY,IS)=QMGCM(11,IY)
    z[18] = dfd['QMGCM'].loc[MNUMCR]*TRIL_TO_QUAD
 
    #     Kerosene                                                  
    #T124(16,IY,IS)=QKSCM(11,IY)
    z[16] = dfd['QKSCM'].loc[MNUMCR]*TRIL_TO_QUAD
 
    #     Distillate Fuel Oil                                       
    #T124(14,IY,IS)=QDSCM(11,IY)
    z[14] = dfd['QDSCM'].loc[MNUMCR]*TRIL_TO_QUAD
 
    #     Residual Fuel Oil                                         
    #T124(15,IY,IS)=QRSCM(11,IY)
    z[15] = dfd['QRSCM'].loc[MNUMCR]*TRIL_TO_QUAD
 
    #       Petroleum Subtotal                                      
    #T124(19,IY,IS)=QTPCM(11,IY)
    z[19] = dfd['QTPCM'].loc[MNUMCR]*TRIL_TO_QUAD
 
    #     Natural Gas                                               
    #T124(20,IY,IS)=QNGCM(11,IY)
    z[20] = dfd['QNGCM'].loc[MNUMCR]*TRIL_TO_QUAD
 
    #     Coal                                                      
    #T124(21,IY,IS)=QCLCM(11,IY)
    z[21] = dfd['QCLCM'].loc[MNUMCR]*TRIL_TO_QUAD
 
    #     Renewable Energy 3/                                       
    #T124(22,IY,IS)=QTRCM(11,IY)-QSTCM(11,IY)-QPVCM(11,IY)
    z[22] = (dfd['QTRCM'].loc[MNUMCR] - dfd['QSTCM'].loc[MNUMCR] - dfd['QPVCM'].loc[MNUMCR])*TRIL_TO_QUAD
 
    #     Electricity                                               
    #T124(23,IY,IS)=QELCM(11,IY)
    z[23] = dfd['QELCM'].loc[MNUMCR]*TRIL_TO_QUAD
 
    #       Delivered Energy                                        
    #T124(24,IY,IS)=QTSCM(11,IY)-QSTCM(11,IY)-QPVCM(11,IY)
    z[24] = (dfd['QTSCM'].loc[MNUMCR] - dfd['QSTCM'].loc[MNUMCR] - dfd['QPVCM'].loc[MNUMCR])*TRIL_TO_QUAD
 
    #     Electricity Related Losses                                
    #T124(25,IY,IS)=T5(42,IY,IS)
    # Create a place holder
    z[25] = z[24] * 0
     
    #       Total                                                   
    #T124(26,IY,IS)=FSUM(T124(24,IY,IS),2)
    # Create a place holder
    z[26] = z[24]+z[25]  # will be populated again in postprocessor_base
 
    #                                                               

    #   Delivered Energy Intensity                                  
    #    (thousand Btu per square foot)                             
    
    #T124(27,IY,IS)=T5(4,IY,IS)
    # Create a place holder
    z[27] = z[24] * 0
 
    #                                                               

    #   Commercial Sector Generation                                

    #     Net Summer Generation Capacity                            
    #     (megawatts)                                               
 
    #       Natural Gas                                             
    # T124(28,IY,IS)=CGCOMMCAP(11,IY,3)
    z[28] = dfd['CGCOMMCAP'].loc[MNUMCR].loc[3] 
 
    #       Solar Photovoltaic                                      
    # T124(29,IY,IS)=CGCOMMCAP(11,IY,8)
    z[29] = dfd['CGCOMMCAP'].loc[MNUMCR].loc[8]
 
    #       Wind                                                    
    #T124(30,IY,IS)=CGCOMMCAP(11,IY,11)
    z[30] = dfd['CGCOMMCAP'].loc[MNUMCR].loc[11]
 
    #     Generation (billion kilowatthours)                        
 
    #       Natural Gas                                             
    #T124(31,IY,IS)=(CGRESGEN(11,IY,3,1)+CGRESGEN(11,IY,3,2)+CGCOMMGEN(11,IY,3,1)+CGCOMMGEN(11,IY,3,2))/1000.
    z[31] = (dfd['CGRESGEN'].loc[MNUMCR].loc[3].loc[1] + dfd['CGRESGEN'].loc[MNUMCR].loc[3].loc[2] + dfd['CGCOMMGEN'].loc[MNUMCR].loc[3].loc[1] + dfd['CGCOMMGEN'].loc[MNUMCR].loc[3].loc[2]) / 1000.
 
    #       Solar Photovoltaic                                      
    # T124(32,IY,IS)=(CGCOMMGEN(11,IY,8,1)+CGCOMMGEN(11,IY,8,2))/1000.
    z[32] = (dfd['CGCOMMGEN'].loc[MNUMCR].loc[8].loc[1] + dfd['CGCOMMGEN'].loc[MNUMCR].loc[8].loc[2]) / 1000.
 
    #       Wind                                                    
    #T124(33,IY,IS)=(CGCOMMGEN(11,IY,11,1)+CGCOMMGEN(11,IY,11,2))/1000.
    z[33] = (dfd['CGCOMMGEN'].loc[MNUMCR].loc[11].loc[1] + dfd['CGCOMMGEN'].loc[MNUMCR].loc[11,2]) * TRIL_TO_QUAD
 
    #   Nonmarketed Renewables Consumption
    #   (quadrillion Btu)                                           
    #T124(34,IY,IS)=QSTCM(11,IY)+QPVCM(11,IY)+CGCOMMQ(11,IY,11)/1000.     
    z[34] = (dfd['QSTCM'].loc[MNUMCR] + dfd['QPVCM'].loc[MNUMCR] + dfd['CGCOMMQ'].loc[MNUMCR].loc[11])* TRIL_TO_QUAD 
    
    return z
 
