# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""

def fill_table_base_101(dfd, table_spec, table_id):
    """Fill table for  Imported Liquids by Source
   
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
    
    
       
    """
    
    z = {}


    #   Imported Liquids by Source
    #   (million barrels per day)
    #    Sources                                                    
    #                                                               
    #   Crude Oil                                                   
    #      Canada                                                   
    #T101(1,IY,IS)=ICOCANADA(IY)
    z[1] = dfd['ICOCANADA']
 
    #      Mexico                                                   
    #T101(2,IY,IS)=ICOMEXICO(IY)
    z[2] = dfd['ICOMEXICO']
 
    #      North Sea                                                
    #T101(3,IY,IS)=ICONORTHSEA(IY)
    z[3] = dfd['ICONORTHSEA']
 
    #      OPEC                                                     
    #T101(4,IY,IS)=ICOOPEC(IY)
    z[4] = dfd['ICOOPEC']
 
    #         Latin America                                         
    #T101(5,IY,IS)=ICOOPAMERICAS(IY)
    z[5] = dfd['ICOOPAMERICAS']
 
    #         North Africa                                          
    #T101(6,IY,IS)=ICOOPNOAFRICA(IY)
    z[6] = dfd['ICOOPNOAFRICA']
 
    #         West Africa                                           
    #T101(7,IY,IS)=ICOOPWESTAFRICA(IY)
    z[7] = dfd['ICOOPWESTAFRICA']
 
    #         Persian Gulf                                          
    #T101(9,IY,IS)=ICOOPPERSIANGULF(IY)
    z[9] = dfd['ICOOPPERSIANGULF']
 
    #      Other Middle East                                        
    #T101(10,IY,IS)=ICOOTHERMIDEAST(IY)
    z[10] = dfd['ICOOTHERMIDEAST']
 
    #      Other Latin America                                      
    #T101(11,IY,IS)=ICOOTHERAMERICAS(IY)
    z[11] = dfd['ICOOTHERAMERICAS']
 
    #      Other Africa                                             
    #T101(12,IY,IS)=ICOOTHERAFRICA(IY)
    z[12] = dfd['ICOOTHERAFRICA']
 
    #      Other Asia                                               
    #T101(13,IY,IS)=ICOOTHERASIA(IY)
    z[13] = dfd['ICOOTHERASIA']
    
    #Crude Oil
    # T101(14,IY,IS) = ICOTotal(IY)
    z[14]= dfd['ICOTOTAL']
 
    #                                                               
 
    #   Light Refined Products 1/                                   
    #T101(27,IY,IS)=ILPTOTAL(IY)
    z[27] = dfd['ILPTOTAL']
 
    #      Canada                                                   
    #T101(15,IY,IS)=ILPCANADA(IY)
    z[15] = dfd['ILPCANADA']
 
    #      Northern Europe                                          
    #T101(16,IY,IS)=ILPNORTHEUROPE(IY)
    z[16] = dfd['ILPNORTHEUROPE']
 
    #      Southern Europe                                          
    #T101(17,IY,IS)=ILPSOUTHEUROPE(IY)
    z[17] = dfd['ILPSOUTHEUROPE']
 
    #      OPEC                                                     
    #T101(18,IY,IS)=ILPOPEC(IY)
    z[18] = dfd['ILPOPEC']
 
    #         Latin America                                         
    #T101(19,IY,IS)=ILPOPAMERICAS(IY)
    z[19] = dfd['ILPOPAMERICAS']
 
    #         North Africa                                          
    #T101(20,IY,IS)=ILPOPNOAFRICA(IY)
    z[20] = dfd['ILPOPNOAFRICA']
 
    #         West Africa                                           
    #T101(21,IY,IS)=ILPOPWESTAFRICA(IY)
    z[21] = dfd['ILPOPWESTAFRICA']
 
    #         Persian Gulf                                          
    #T101(23,IY,IS)=ILPOPPERSIANGULF(IY)
    z[23] = dfd['ILPOPPERSIANGULF']
 
    #      Caribbean Basin                                          
    #T101(24,IY,IS)=ILPCARIBBEAN(IY)
    z[24] = dfd['ILPCARIBBEAN']
 
    #      Asian Exporters                                          
    #T101(25,IY,IS)=ILPASIA(IY)
    z[25] = dfd['ILPASIA']
 
    #      Other                                                    
    #T101(26,IY,IS)=ILPOTHER(IY)
    z[26] = dfd['ILPOTHER']
 
    #                                                               
 
    #   Heavy Refined Products 2/                                   
    #T101(40,IY,IS)=IHPTOTAL(IY)
    z[40] = dfd['IHPTOTAL']
 
    #      Canada                                                   
    #T101(28,IY,IS)=IHPCANADA(IY)
    z[28] = dfd['IHPCANADA']
 
    #      Northern Europe                                          
    #T101(29,IY,IS)=IHPNORTHEUROPE(IY)
    z[29] = dfd['IHPNORTHEUROPE']
 
    #      Southern Europe                                          
    #T101(30,IY,IS)=IHPSOUTHEUROPE(IY)
    z[30] = dfd['IHPSOUTHEUROPE']
 
    #      OPEC                                                     
    #T101(31,IY,IS)=IHPOPEC(IY)
    z[31] = dfd['IHPOPEC']
 
    #         Latin America                                         
    #T101(32,IY,IS)=IHPOPAMERICAS(IY)
    z[32] = dfd['IHPOPAMERICAS']
 
    #         North Africa                                          
    #T101(33,IY,IS)=IHPOPNOAFRICA(IY)
    z[33] = dfd['IHPOPNOAFRICA']
 
    #         West Africa                                           
    #T101(34,IY,IS)=IHPOPWESTAFRICA(IY)
    z[34] = dfd['IHPOPWESTAFRICA']
 
    #         Persian Gulf                                          
    #T101(36,IY,IS)=IHPOPPERSIANGULF(IY)
    z[36] = dfd['IHPOPPERSIANGULF']
 
    #      Caribbean Basin                                          
    #T101(37,IY,IS)=IHPCARIBBEAN(IY)
    z[37] = dfd['IHPCARIBBEAN']
 
    #      Asian Exporters                                          
    #T101(38,IY,IS)=IHPASIA(IY)
    z[38] = dfd['IHPASIA']
 
    #      Other                                                    
    #T101(39,IY,IS)=IHPOTHER(IY)
    z[39] = dfd['IHPOTHER']
    
    return z
