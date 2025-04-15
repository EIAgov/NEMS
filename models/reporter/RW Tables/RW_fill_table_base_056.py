# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_056(dfd, table_spec, table_id):
    """Fill table Transportation Fleet Car and Truck Vehicle Miles Traveled by Type and Technology
   
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

    #   Transportation Fleet Car and Truck Vehicle Miles Traveled by Type and Technology
    #   (billion miles)
    #    Technology Type                                            
    #                                                               
    #   Cars 1/                                                     
    #    Conventional Cars                                          
    #      Gasoline ICE Vehicles                                    
    #T56(1,IY,IS)=FLTECHVMTRPT(1,1,IY)
    z[1] =  dfd['FLTECHVMTRPT'].loc[1].loc[1]
    #      TDI Diesel ICE                                           
    #T56(2,IY,IS)=FLTECHVMTRPT(1,2,IY)
    z[2] = dfd['FLTECHVMTRPT'].loc[1].loc[2]
    #        Total Conventional Cars                                
    #T56(3,IY,IS)=T56(1,IY,IS)+T56(2,IY,IS)
    z[3] = z[1] +z[2]                                               
    #    Alternative-Fuel Cars                                      
    #      Ethanol-Flex Fuel ICE                                    
    #T56(5,IY,IS)=FLTECHVMTRPT(1,3,IY)
    z[5] = dfd['FLTECHVMTRPT'].loc[1].loc[3]
    #      100-Mile Electric Vehicle                                
    #T56(6,IY,IS)=FLTECHVMTRPT(1,4,IY)
    z[6] = dfd['FLTECHVMTRPT'].loc[1].loc[4]
    #      200-Mile Electric Vehicle                                
    #T56(7,IY,IS)=FLTECHVMTRPT(1,7,IY)
    z[7] = dfd['FLTECHVMTRPT'].loc[1].loc[7]
    #      300-Mile Electric Vehicle                                
    #T56(15,IY,IS)=FLTECHVMTRPT(1,15,IY)
    z[15] = dfd['FLTECHVMTRPT'].loc[1].loc[15]
    #      Plug-in 20 Gasoline Hybrid                               
    #T56(8,IY,IS)=FLTECHVMTRPT(1,5,IY)
    z[8] = dfd['FLTECHVMTRPT'].loc[1].loc[5]
    #      Plug-in 50 Gasoline Hybrid                               
    #T56(4,IY,IS)=FLTECHVMTRPT(1,6,IY)
    z[4] = dfd['FLTECHVMTRPT'].loc[1].loc[6]
    #      Electric-Diesel Hybrid                                   
    #T56(9,IY,IS)=FLTECHVMTRPT(1,8,IY)
    z[9] = dfd['FLTECHVMTRPT'].loc[1].loc[8]
    #      Electric-Gasoline Hybrid                                 
    #T56(10,IY,IS)=FLTECHVMTRPT(1,16,IY)
    z[10] = dfd['FLTECHVMTRPT'].loc[1].loc[16]
    #      Natural Gas ICE                                          
    #T56(11,IY,IS)=FLTECHVMTRPT(1,11,IY)
    z[11] = dfd['FLTECHVMTRPT'].loc[1].loc[11]
    #      Natural Gas Bi-fuel                                      
    #T56(12,IY,IS)=FLTECHVMTRPT(1,9,IY)
    z[12] = dfd['FLTECHVMTRPT'].loc[1].loc[9]
    #      Propane ICE                                              
    #T56(13,IY,IS)=FLTECHVMTRPT(1,12,IY)
    z[13] = dfd['FLTECHVMTRPT'].loc[1].loc[12]
    #      Propane Bi-fuel                                          
    #T56(14,IY,IS)=FLTECHVMTRPT(1,10,IY)
    z[14] = dfd['FLTECHVMTRPT'].loc[1].loc[10]
    #      Fuel Cell Methanol                                       
    #T56(16,IY,IS)=FLTECHVMTRPT(1,13,IY)
    z[16] = dfd['FLTECHVMTRPT'].loc[1].loc[13]
    #      Fuel Cell Hydrogen                                       
    #T56(17,IY,IS)=FLTECHVMTRPT(1,14,IY)
    z[17] = dfd['FLTECHVMTRPT'].loc[1].loc[14]
    #        Total Alternative Cars                                 
    #T56(18,IY,IS)=FSUM(T56(4,IY,IS),14)
    z[18] = z[4]+z[5]+z[6]+z[7]+z[8]+z[9]+z[10]+z[11]+z[12]+z[13]+z[14]+z[15]+z[16]+z[17]
    #    Total Cars                                                 
    #T56(20,IY,IS)=T56(3,IY,IS)+T56(18,IY,IS)
    z[20] = z[3]+z[18]                                             
    #   Light Trucks 1/
    #    Conventional Light Trucks
    #      Gasoline ICE Vehicles
    #T56(21,IY,IS)=FLTECHVMTRPT(2,1,IY)
    z[21] = dfd['FLTECHVMTRPT'].loc[2].loc[1]
    #      TDI Diesel ICE
    #T56(22,IY,IS)=FLTECHVMTRPT(2,2,IY)
    z[22] = dfd['FLTECHVMTRPT'].loc[2].loc[2]
    #        Total Conventional Light Trucks                        
    #T56(23,IY,IS)=T56(21,IY,IS)+T56(22,IY,IS)
    z[23] = z[21] + z[22]                                           
    #    Alternative-Fuel Light Trucks
    #      Ethanol-Flex Fuel ICE
    #T56(25,IY,IS)=FLTECHVMTRPT(2,3,IY)
    z[25] = dfd['FLTECHVMTRPT'].loc[2].loc[3]
    #      100-Mile Electric Vehicle
    #T56(26,IY,IS)=FLTECHVMTRPT(2,4,IY)
    z[26] = dfd['FLTECHVMTRPT'].loc[2].loc[4]
    #      200-Mile Electric Vehicle
    #T56(27,IY,IS)=FLTECHVMTRPT(2,7,IY)
    z[27] = dfd['FLTECHVMTRPT'].loc[2].loc[7]
    #      300-Mile Electric Vehicle
    #T56(35,IY,IS)=FLTECHVMTRPT(2,15,IY)
    z[35] = dfd['FLTECHVMTRPT'].loc[2].loc[15]
    #      Plug-in 20 Gasoline Hybrid
    #T56(28,IY,IS)=FLTECHVMTRPT(2,5,IY)
    z[28] = dfd['FLTECHVMTRPT'].loc[2].loc[5]
    #      Plug-in 50 Gasoline Hybrid
    #T56(24,IY,IS)=FLTECHVMTRPT(2,6,IY)
    z[24] = dfd['FLTECHVMTRPT'].loc[2].loc[6]
    #      Electric-Diesel Hybrid
    #T56(29,IY,IS)=FLTECHVMTRPT(2,8,IY)
    z[29] = dfd['FLTECHVMTRPT'].loc[2].loc[8]
    #      Electric-Gasoline Hybrid
    #T56(30,IY,IS)=FLTECHVMTRPT(2,16,IY)
    z[30] = dfd['FLTECHVMTRPT'].loc[2].loc[16]
    #      Natural Gas ICE
    #T56(31,IY,IS)=FLTECHVMTRPT(2,11,IY)
    z[31] = dfd['FLTECHVMTRPT'].loc[2].loc[11]
    #      Natural Gas Bi-fuel
    #T56(32,IY,IS)=FLTECHVMTRPT(2,9,IY)
    z[32] = dfd['FLTECHVMTRPT'].loc[2].loc[9]
    #      Propane ICE
    #T56(33,IY,IS)=FLTECHVMTRPT(2,12,IY)
    z[33] = dfd['FLTECHVMTRPT'].loc[2].loc[12]
    #      Propane Bi-fuel
    #T56(34,IY,IS)=FLTECHVMTRPT(2,10,IY)
    z[34] = dfd['FLTECHVMTRPT'].loc[2].loc[10]
    #      Fuel Cell Methanol
    #T56(36,IY,IS)=FLTECHVMTRPT(2,13,IY)
    z[36] = dfd['FLTECHVMTRPT'].loc[2].loc[13]
    #      Fuel Cell Hydrogen
    #T56(37,IY,IS)=FLTECHVMTRPT(2,14,IY)
    z[37] = dfd['FLTECHVMTRPT'].loc[2].loc[14]
    #        Total Alternative Light Trucks
    #T56(38,IY,IS)=FSUM(T56(24,IY,IS),14)
    z[38] = z[24]+z[25]+z[26]+z[27]+z[28]+z[29]+z[30]+z[31]+z[32]+z[33]+z[34]+z[35]+z[36]+z[37]
    #    Total Light Trucks
    #T56(40,IY,IS)=T56(23,IY,IS)+T56(38,IY,IS)
    z[40] = z[23]+z[38]
    #   Total Fleet Vehicles
    #T56(41,IY,IS)=T56(20,IY,IS)+T56(40,IY,IS)
    z[41] = z[20] + z[40]
    #   Commercial Light Trucks 2/                                  
    #      Motor Gasoline
    #T56(42:51,IY,IS)=BCLTVMT(1:10,IY)
    z[42] = dfd['BCLTVMT'].loc[1]
    #      Diesel
    #T56(42:51,IY,IS)=BCLTVMT(1:10,IY)
    z[43] = dfd['BCLTVMT'].loc[2]
    #      Propane
    #T56(42:51,IY,IS)=BCLTVMT(1:10,IY)
    z[44] = dfd['BCLTVMT'].loc[3]
    #      Compressed/Liquefied Natural Gas                         
    #T56(42:51,IY,IS)=BCLTVMT(1:10,IY)
    z[45] = dfd['BCLTVMT'].loc[4]
    #      Ethanol-Flex Fuel
    #T56(42:51,IY,IS)=BCLTVMT(1:10,IY)
    z[46] = dfd['BCLTVMT'].loc[5]
    #      Electric
    #T56(42:51,IY,IS)=BCLTVMT(1:10,IY)
    z[47] = dfd['BCLTVMT'].loc[6]
    #      Plug-in Gasoline Hybrid
    #T56(42:51,IY,IS)=BCLTVMT(1:10,IY)
    z[48] = dfd['BCLTVMT'].loc[7]
    #      Plug-in Diesel Hybrid
    #T56(42:51,IY,IS)=BCLTVMT(1:10,IY)
    z[49] = dfd['BCLTVMT'].loc[8]
    #      Fuel Cell
    #T56(42:51,IY,IS)=BCLTVMT(1:10,IY)
    z[50] = dfd['BCLTVMT'].loc[9]
    #      Fuel Cell Battery Dominant
    z[51] = dfd['BCLTVMT'].loc[10]
    #      Gasoline HEV
    z[52] = dfd['BCLTVMT'].loc[11]
    #      H2 ICE           
    z[53] = dfd['BCLTVMT'].loc[12]
    #         Total Commercial Light Trucks 
    z[54] = z[42] + z[43] + z[44] + z[45] + z[46] + z[47] + z[48] + z[49] + z[50] + z[52] + z[53]
    #         Total Commercial Light Trucks
    #T56(42:51,IY,IS)=BCLTVMT(1:10,IY)
#    z[51] = dfd['BCLTVMT'].loc[10]

    return z
