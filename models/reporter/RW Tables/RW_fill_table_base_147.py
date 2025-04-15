# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""

def fill_table_base_147(dfd, table_spec, table_id):
    """Fill table for  Transportation Sector Criteria Pollutants Emissions
   
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



    #   Transportation Sector Criteria Pollutants Emissions
    #   (million metric tons)
    #    Emissions                                                  
    #   Hydrocarbons                                                
    #     Light-Duty Passenger Cars 1/                              


    #       Motor Gasoline                                          
    #T147(1:12,IY,IS)=TOT_EMIS(IY,1,1:12)
    z[1] = dfd['TOT_EMIS'].loc[1].loc[1]
 
    #       Distillate Fuel Oil (diesel)                            
    #T147(1:12,IY,IS)=TOT_EMIS(IY,1,1:12)
    z[2] = dfd['TOT_EMIS'].loc[1].loc[2]
 
    #     Light Duty Trucks                                         
 
    #       Motor Gasoline                                          
    #T147(1:12,IY,IS)=TOT_EMIS(IY,1,1:12)
    z[3] = dfd['TOT_EMIS'].loc[1].loc[3]
 
    #       Distillate Fuel Oil (diesel)                            
    #T147(1:12,IY,IS)=TOT_EMIS(IY,1,1:12)
    z[4] = dfd['TOT_EMIS'].loc[1].loc[4]
 
    #     Commercial Light Truck 8.5-10k                            

    #       Motor Gasoline                                          
    #T147(1:12,IY,IS)=TOT_EMIS(IY,1,1:12)
    z[5] = dfd['TOT_EMIS'].loc[1].loc[5]
 
    #       Distillate Fuel Oil (diesel)                            
    #T147(1:12,IY,IS)=TOT_EMIS(IY,1,1:12)
    z[6] = dfd['TOT_EMIS'].loc[1].loc[6]
 
    #     Class 3 Vehicles (10k-14k lbs)                            

    #       Motor Gasoline                                          
    #T147(1:12,IY,IS)=TOT_EMIS(IY,1,1:12)
    z[7] = dfd['TOT_EMIS'].loc[1].loc[7]
 
    #       Distillate Fuel Oil (diesel)                            
    #T147(1:12,IY,IS)=TOT_EMIS(IY,1,1:12)
    z[8] = dfd['TOT_EMIS'].loc[1].loc[8]
 
    #     Class 4-6 Vehicles 14k-26k lbs                            


    #       Motor Gasoline                                          
    #T147(1:12,IY,IS)=TOT_EMIS(IY,1,1:12)
    z[9] = dfd['TOT_EMIS'].loc[1].loc[9]
 
    #       Distillate Fuel Oil (diesel)                            
    #T147(1:12,IY,IS)=TOT_EMIS(IY,1,1:12)
    z[10] = dfd['TOT_EMIS'].loc[1].loc[10]
 
    #     Class 7-8 Vehicles ( >26k lbs)                            

    #       Motor Gasoline                                          
    #T147(1:12,IY,IS)=TOT_EMIS(IY,1,1:12)
    z[11] = dfd['TOT_EMIS'].loc[1].loc[11]
 
    #       Distillate Fuel Oil (diesel)                            
    #T147(1:12,IY,IS)=TOT_EMIS(IY,1,1:12)
    z[12] = dfd['TOT_EMIS'].loc[1].loc[12]
 
    #                                                               

    #     Total Hydrocarbons 2/                                     
    #T147(13,IY,IS)=FSUM(T147(1,IY,IS),12)
    z[13] = z[1]+z[2]+z[3]+z[4]+z[5]+z[6]+z[7]+z[8]+z[9]+z[10]+z[11]+z[12]
 

    #   Carbon Monoxide                                             

    #     Light-Duty Passenger Cars 1/                              

    #       Motor Gasoline                                          
    #T147(14:25,IY,IS)=TOT_EMIS(IY,2,1:12)
    z[14] = dfd['TOT_EMIS'].loc[2].loc[1]
 
    #       Distillate Fuel Oil (diesel)                            
    #T147(14:25,IY,IS)=TOT_EMIS(IY,2,1:12)
    z[15] = dfd['TOT_EMIS'].loc[2].loc[2]
 
    #     Light Duty Trucks                                         

    #       Motor Gasoline                                          
    #T147(14:25,IY,IS)=TOT_EMIS(IY,2,1:12)
    z[16] = dfd['TOT_EMIS'].loc[2].loc[3]
 
    #       Distillate Fuel Oil (diesel)                            
    #T147(14:25,IY,IS)=TOT_EMIS(IY,2,1:12)
    z[17] = dfd['TOT_EMIS'].loc[2].loc[4]
 
    #     Commercial Light Truck 8.5-1k                             

    #       Motor Gasoline                                          
    #T147(14:25,IY,IS)=TOT_EMIS(IY,2,1:12)
    z[18] = dfd['TOT_EMIS'].loc[2].loc[5]
 
    #       Distillate Fuel Oil (diesel)                            
    #T147(14:25,IY,IS)=TOT_EMIS(IY,2,1:12)
    z[19] = dfd['TOT_EMIS'].loc[2].loc[6]
 
    #     Class 3 Vehicles (10k-14k lbs)                            

    #       Motor Gasoline                                          
    #T147(14:25,IY,IS)=TOT_EMIS(IY,2,1:12)
    z[20] = dfd['TOT_EMIS'].loc[2].loc[7]
 
    #       Distillate Fuel Oil (diesel)                            
    #T147(14:25,IY,IS)=TOT_EMIS(IY,2,1:12)
    z[21] = dfd['TOT_EMIS'].loc[2].loc[8]
 
    #     Class 4-6 Vehicles 14k-26k lbs                            

    #       Motor Gasoline                                          
    #T147(14:25,IY,IS)=TOT_EMIS(IY,2,1:12)
    z[22] = dfd['TOT_EMIS'].loc[2].loc[9]
 
    #       Distillate Fuel Oil (diesel)                            
    #T147(14:25,IY,IS)=TOT_EMIS(IY,2,1:12)
    z[23] = dfd['TOT_EMIS'].loc[2].loc[10]
 
    #     Class 7-8 Vehicles ( >26k lbs)                            

    #       Motor Gasoline                                          
    #T147(14:25,IY,IS)=TOT_EMIS(IY,2,1:12)
    z[24] = dfd['TOT_EMIS'].loc[2].loc[11]
 
    #       Distillate Fuel Oil (diesel)                            
    #T147(14:25,IY,IS)=TOT_EMIS(IY,2,1:12)
    z[25] = dfd['TOT_EMIS'].loc[2].loc[12]
 
    #                                                               
 
    #     Total Carbon Monoxide 2/                                  
    #T147(26,IY,IS)=FSUM(T147(14,IY,IS),12)
    z[26] = z[14]+z[15]+z[16]+z[17]+z[18]+z[19]+z[20]+z[21]+z[22]+z[23]+z[24]+z[25]
 
  
    #                                                               

    #   Nitrogen Oxides (NOx)                                       

    #     Light-Duty Passenger Cars 1/                              

    #       Motor Gasoline                                          
    #T147(27:38,IY,IS)=TOT_EMIS(IY,3,1:12)
    z[27] = dfd['TOT_EMIS'].loc[3].loc[1]
 
    #       Distillate Fuel Oil (diesel)                            
    #T147(27:38,IY,IS)=TOT_EMIS(IY,3,1:12)
    z[28] = dfd['TOT_EMIS'].loc[3].loc[2]
 
    #     Light Duty Trucks                                         

    #       Motor Gasoline                                          
    #T147(27:38,IY,IS)=TOT_EMIS(IY,3,1:12)
    z[29] = dfd['TOT_EMIS'].loc[3].loc[3]
 
    #       Distillate Fuel Oil (diesel)                            
    #T147(27:38,IY,IS)=TOT_EMIS(IY,3,1:12)
    z[30] = dfd['TOT_EMIS'].loc[3].loc[4]
 
    #     Commercial Light Truck 8.5-10k                            

    #       Motor Gasoline                                          
    #T147(27:38,IY,IS)=TOT_EMIS(IY,3,1:12)
    z[31] = dfd['TOT_EMIS'].loc[3].loc[5]
 
    #       Distillate Fuel Oil (diesel)                            
    #T147(27:38,IY,IS)=TOT_EMIS(IY,3,1:12)
    z[32] = dfd['TOT_EMIS'].loc[3].loc[6]
 
    #     Class 3 Vehicles (10k-14k lbs)                            

    #       Motor Gasoline                                          
    #T147(27:38,IY,IS)=TOT_EMIS(IY,3,1:12)
    z[33] = dfd['TOT_EMIS'].loc[3].loc[7]
 
    #       Distillate Fuel Oil (diesel)                            
    #T147(27:38,IY,IS)=TOT_EMIS(IY,3,1:12)
    z[34] = dfd['TOT_EMIS'].loc[3].loc[8]
 
    #     Class 4-6 Vehicles 14k-26k lbs                            

    #       Motor Gasoline                                          
    #T147(27:38,IY,IS)=TOT_EMIS(IY,3,1:12)
    z[35] = dfd['TOT_EMIS'].loc[3].loc[9]
 
    #       Distillate Fuel Oil (diesel)                            
    #T147(27:38,IY,IS)=TOT_EMIS(IY,3,1:12)
    z[36] = dfd['TOT_EMIS'].loc[3].loc[10]
 
    #     Class 7-8 Vehicles ( >26k lbs)                            

    #       Motor Gasoline                                          
    #T147(27:38,IY,IS)=TOT_EMIS(IY,3,1:12)
    z[37] = dfd['TOT_EMIS'].loc[3].loc[11]
 
    #       Distillate Fuel Oil (diesel)                            
    #T147(27:38,IY,IS)=TOT_EMIS(IY,3,1:12)
    z[38] = dfd['TOT_EMIS'].loc[3].loc[12]
 
    #                                                               

    #     Total Nitrogen Oxides 2/                                  
    #T147(39,IY,IS)=FSUM(T147(27,IY,IS),12)
    z[39] = z[27]+z[28]+z[29]+z[30]+z[31]+z[32]+z[33]+z[34]+z[35]+z[36]+z[37]+z[38]
 
    #                                                               
    #     Total Criteria Pollutants 2/                              
    #T147(40,IY,IS)=T147(13,IY,IS)+T147(26,IY,IS)+T147(39,IY,IS)
    z[40] = z[13]+z[26]+z[39]
    
    return z
 
