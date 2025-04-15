# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023
@author: TDM
"""
def fill_table_base_045(dfd, table_spec, table_id):
    """Fill table  Transportation Sector Energy Use by Mode and Type
   
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
# issue with z[46]
    MNUMCR = dfd['MNUMCR_rwpre'] 

    #   Transportation Sector Energy Use by Mode and Type
    #   (trillion Btu)
    #    Mode and Type                                              
    #                                                               
    #   Energy Use by Mode                                          
    #      Highway                                                  
    #         Light-Duty Vehicles                                   
    #T45((1,IY,IS)=TRQHWY(1,IY)+TRQHWY(2,IY)+TRQHWY(3,IY)
    z[1] = dfd['TRQHWY'].loc[1] + dfd['TRQHWY'].loc[2]+dfd['TRQHWY'].loc[3]
    #            Automobiles                                        
    #T45((2,IY,IS)=TRQHWY(1,IY)
    z[2] = dfd['TRQHWY'].loc[1]
    #            Light Trucks                                       
    #T45((3,IY,IS)=TRQHWY(2,IY)
    z[3] = dfd['TRQHWY'].loc[2]
    #            Motorcycles                                        
    #T45((4,IY,IS)=TRQHWY(3,IY)
    z[4] = dfd['TRQHWY'].loc[3]
    #         Commercial Light Trucks 1/                            
    #T45((5,IY,IS)=BCLTBTUT(10,IY)
    z[5] = dfd["BCLTBTUT"].loc[1:12,:].sum()
    #         Buses                                                 
    #T45((6,IY,IS)=SUM(TRQBUS(:,:,IY))
    z[6] = dfd['TRQBUS'].loc[((1,2,3), (1,2,3,4,5,6,7,8)), :].sum()
    #            Transit                                            
    #T45((7,IY,IS)=SUM(TRQBUS(1,:,IY))
    z[7] = dfd['TRQBUS'].loc[(1, (1,2,3,4,5,6,7,8)), :].sum()
    #            Intercity                                          
    #T45((8,IY,IS)=SUM(TRQBUS(2,:,IY))
    z[8] = dfd['TRQBUS'].loc[(2, (1,2,3,4,5,6,7,8)), :].sum()
    #            School                                             
    #T45((9,IY,IS)=SUM(TRQBUS(3,:,IY))
    z[9] = dfd['TRQBUS'].loc[(3, (1,2,3,4,5,6,7,8)), :].sum()
    #         Freight Trucks 2/                                     
    #T45((10,IY,IS)=TRQHWY(5,IY)+TRQHWY(6,IY)
    z[10] = dfd['TRQHWY'].loc[5] + dfd['TRQHWY'].loc[6]
    #            Light Medium                                       
    #T45((51,IY,IS)=TRQHWY_NEW(5,IY)
    z[51] = dfd['TRQHWY_NEW'].loc[5]
    #            Medium                                             
    #T45((52,IY,IS)=TRQHWY_NEW(6,IY)
    z[52] = dfd['TRQHWY_NEW'].loc[6]
    #            Medium (10001-26000 pounds)    NO LONGER USED (Light + Medium now)                    
    #T45((11,IY,IS)=TRQHWY(5,IY)
    z[11] = dfd['TRQHWY'].loc[5]
    #            Heavy
    #T45((12,IY,IS)=TRQHWY(6,IY)
    z[12] = dfd['TRQHWY'].loc[6]
    #                                                               
    #      Non-Highway 
    #            General Aviation                                   
    #T45((14,IY,IS)=TRQNHWY(1,IY)
    z[14] =  dfd['TRQNHWY'].loc[1]
    #            Domestic Passenger 4/                              
    #T45((15,IY,IS)=TRQNHWY(2,IY)
    z[15] = dfd['TRQNHWY'].loc[2]
    #            International Passenger 4/                         
    #T45((16,IY,IS)=TRQNHWY(3,IY)
    z[16] = dfd['TRQNHWY'].loc[3]
    #            Dedicated Freight                                  
    #T45((17,IY,IS)=TRQNHWY(4,IY)
    z[17] = dfd['TRQNHWY'].loc[4]
    #         Air 3/                                                
    #T45((13,IY,IS)=TRQNHWY(1,IY)+TRQNHWY(2,IY)+TRQNHWY(3,IY)+TRQNHWY(4,IY)
    z[13] = z[14]+z[15]+z[16]+z[17]
    #               Domestic Shipping                               
    #T45((20,IY,IS)=TRQDOMS(1,IY)+TRQDOMS(2,IY)+TRQDOMS(3,IY)+TRQDOMS(4,IY)
    z[20] =  dfd['TRQDOMS'].loc[(1,2,3,4), :].sum()
    #               International Shipping                          
    #T45((21,IY,IS)=TRQINTS(1,IY)+TRQINTS(2,IY)+TRQINTS(3,IY)+TRQINTS(4,IY)
    z[21] = dfd['TRQINTS'].loc[(1,2,3,4), :].sum()
    #            Total Freight                                            
    #T45((19,IY,IS)=SUM(TRQDOMS(1:4,IY))+SUM(TRQINTS(1:4,IY))
    z[19] =  z[20]+z[21]
    #            Recreational Boats                                 
    #T45((22,IY,IS)=TRQBOAT(1,IY)+TRQBOAT(2,IY)
    z[22] = dfd['TRQBOAT'].loc[1]+dfd['TRQBOAT'].loc[2]
    #         Water 5/                                              
    #T45((18,IY,IS)=SUM(TRQDOMS(1:4,IY))+SUM(TRQINTS(1:4,IY))+TRQBOAT(1,IY)+TRQBOAT(2,IY)
    z[18] = z[20]+z[21]+z[22]
    #            Rail Freight                                            
    #T45((24,IY,IS)=TRQRRF(1,IY)+TRQRRF(2,IY)+TRQRRF(3,IY)+TRQRRF(4,IY)
    z[24] = dfd['TRQRRF'].loc[(1,2,3,4), :].sum()
    #           Rail Passenger                                          
    #T45((25,IY,IS)=SUM(TRQRRP(1:9,IY))
    z[25] = dfd['TRQRRP'].loc[(1,2,3,4,5,6,7,8,9), :].sum()
    #         Rail Total                                                 
    #T45((23,IY,IS)=SUM(TRQRRF(1:4,IY))+SUM(TRQRRP(1:9,IY))
    z[23] = z[24]+z[25]
    #               Intercity                                       
    #T45((26,IY,IS)=SUM(TRQRRP(1:4,IY))
    z[26] = dfd['TRQRRP'].loc[(1,2,3,4), :].sum()
    #               Transit                                         
    #T45((27,IY,IS)=TRQRRP(5,IY)
    z[27] = dfd['TRQRRP'].loc[5]
    #               Commuter                                        
    #T45((28,IY,IS)=SUM(TRQRRP(6:9,IY))
    z[28] = dfd['TRQRRP'].loc[(6,7,8,9), :].sum()
    #         Lubricants                                            
    #T45((29,IY,IS)=TRQLUB(IY)
    z[29] = dfd['TRQLUB']
    #         Pipeline Fuel Natural Gas                             
    #T45((30,IY,IS)=QGPTR(11,IY)*1000.
    z[30] = dfd['QGPTR'].loc[MNUMCR]
    #         Natural Gas Liquefaction for Export                   
    #T45((53,IY,IS)=QNGLQ(11,IY)*1000
    z[53] = dfd['QNGLQ'].loc[MNUMCR]
    #                                                               
    #         Military Use Jet Fuel and Aviation Gasoline                        
    #T45((32,IY,IS)=TRQMIL(1,IY)+TRQMIL(2,IY)
    z[32] = dfd['TRQMIL'].loc[1]+dfd['TRQMIL'].loc[2]
    #         Military Use Residual Fuel Oil                                     
    #T45((33,IY,IS)=TRQMIL(3,IY)
    z[33] = dfd['TRQMIL'].loc[3]
    #         Military Use Distillates and Diesel                                
    #T45((34,IY,IS)=TRQMIL(4,IY)
    z[34] = dfd['TRQMIL'].loc[4]
    #      Military Use Total                                           
    #T45((31,IY,IS)=TRQMIL(1,IY)+TRQMIL(2,IY)+TRQMIL(3,IY)+TRQMIL(4,IY)
    z[31] = z[32]+z[33]+z[34]
    #                                                               
    #   Total                                                       
    #T45((35,IY,IS)=T45(1,IY,IS)+T45(6,IY,IS)+T45(10,IY,IS)+T45(13,IY,IS)+T45(18,IY,IS)+T45(23,IY,IS)+T45(29,IY,IS)+T45(30,IY,IS)+T45(31,IY,IS)+T45(5,IY,IS)
    z[35] = z[1]+z[6]+z[10]+z[13]+z[18]+z[23]+z[29]+z[30]+z[31]+z[5]+z[53]
                                                               
    #                                                               
    # Energy Use by Type
    # Motor Gasoline excluding E85 6/
    z[36] = dfd['TRQLDV'].loc[1].loc[MNUMCR]+dfd['CLTFUELBTU'].loc[1]+dfd['TRQFTRK'].loc[1]+dfd['TRQDOMS'].loc[3]+dfd['TRQBUS'].loc[1].loc[1]+dfd['TRQBUS'].loc[2].loc[1]+dfd['TRQBUS'].loc[3].loc[1]+dfd['TRQBOAT'].loc[1]
    # E85 6/                                                
    z[45] = dfd['TRQLDV'].loc[3].loc[MNUMCR]+dfd['TRQFTRK'].loc[6]+dfd['CLTFUELBTU'].loc[5]+dfd['TRQBUS'].loc[1].loc[3]+dfd['TRQBUS'].loc[2].loc[3]+dfd['TRQBUS'].loc[3].loc[3]
    # Diesel 7/                                             
    z[37] = dfd['TRQLDV'].loc[8].loc[MNUMCR]+dfd['TRQFTRK'].loc[2]+dfd['TRQRRF'].loc[1]+dfd['TRQDOMS'].loc[1]+dfd['TRQINTS'].loc[1]+dfd['TRQMIL'].loc[4]+ \
            dfd['TRQBUS'].loc[1].loc[2]+dfd['TRQBUS'].loc[2].loc[2]+dfd['TRQBUS'].loc[3].loc[2]+dfd['CLTFUELBTU'].loc[2]+dfd['TRQRRP'].loc[2]+dfd['TRQRRP'].loc[7]+dfd['TRQBOAT'].loc[2]
    # Jet Fuel (kerosene & naphtha)                         
    z[38] = dfd['TRQAIRT'].loc[1]+dfd['TRQMIL'].loc[1]+dfd['TRQMIL'].loc[2]
    # Residual Fuel Oil                                     
    z[39] = dfd['TRQDOMS'].loc[2]+dfd['TRQINTS'].loc[2]+dfd['TRQMIL'].loc[3]
    # Aviation Gasoline                                     
    z[40] = dfd['TRQAIRT'].loc[2]
    # Propane                                               
    z[41] = dfd['TRQLDV'].loc[5].loc[MNUMCR]+dfd['TRQFTRK'].loc[5]+ dfd['TRQBUS'].loc[1].loc[6]+dfd['TRQBUS'].loc[2].loc[6]+dfd['TRQBUS'].loc[3].loc[6]+dfd['CLTFUELBTU'].loc[3]
    # Lubricants                                            
    z[42] = z[29]
    # Petroleum and Other Liquids Subtotal                     
    z[43] =  z[36]+z[37]+z[38]+z[39]+z[40]+z[41]+z[42]+z[45]
    # M85       ## not in the output                                               
    z[44] = dfd['TRQLDV'].loc[2].loc[MNUMCR]
    # Electricity                                                 
    z[46] = dfd['TRQLDV'].loc[6].loc[MNUMCR]+dfd['TRQRRP'].loc[1]+dfd['TRQRRP'].loc[5]+dfd['TRQRRP'].loc[6]+dfd['CLTFUELBTU'].loc[6]+dfd['TRQBUS'].loc[1:3,7,:].sum()+ dfd['TRQFTRK'].loc[6]
    # Compressed/Liquefied Natural Gas                         
    z[47] = dfd['TRQLDV'].loc[4].loc[MNUMCR]+dfd['TRQFTRK'].loc[3]+dfd['TRQBUS'].loc[1].loc[5]+dfd['TRQBUS'].loc[2].loc[5]+dfd['TRQBUS'].loc[3].loc[5]+dfd['CLTFUELBTU'].loc[4]+dfd['TRQRRF'].loc[3]+dfd['TRQRRF'].loc[4] + \
            dfd['TRQDOMS'].loc[3]+dfd['TRQDOMS'].loc[4]+dfd['TRQINTS'].loc[3]+dfd['TRQINTS'].loc[4]+dfd['TRQRRP'].loc[3]+dfd['TRQRRP'].loc[4]+dfd['TRQRRP'].loc[8]++dfd['TRQRRP'].loc[9]
    # Hydrogen                                                 
    z[48] = dfd['TRQLDV'].loc[7].loc[MNUMCR]+dfd['TRQFTRK'].loc[8]+dfd['CLTFUELBTU'].loc[7]+dfd['TRQBUS'].loc[1].loc[8]+dfd['TRQBUS'].loc[2].loc[8]+dfd['TRQBUS'].loc[3].loc[8]
    # Pipeline Fuel Natural Gas                                
    z[49] = z[30]
    # Natural Gas Liquefaction for Export                      
    z[54] =  z[53]
    # Total Consumption                                           
    z[50] = z[43]+z[46]+z[47]+z[48]+z[49]+z[54]
    
    return z                                                               