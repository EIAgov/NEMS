# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_046(dfd, table_spec, table_id):
    """Fill table    Transportation Sector Energy Use by Fuel Type Within a Mode
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

    #   Transportation Sector Energy Use by Fuel Type Within a Mode
    #   (trillion Btu)
    #    Mode and Type                                              

   
    
    #     Motor Gasoline excluding E85 1/                           
    #T46(1,IY,IS)=TRQLDV(1,11,IY)
    z[1] = dfd['TRQLDV'].loc[1].loc[MNUMCR]
 
    #     E85 1/                                                    
    #T46(3,IY,IS)=TRQLDV(3,11,IY)
    z[3] = dfd['TRQLDV'].loc[3].loc[MNUMCR]
 
    #     Distillate Fuel Oil (diesel)                              
    #T46(8,IY,IS)=TRQLDV(8,11,IY)
    z[8] = dfd['TRQLDV'].loc[8].loc[MNUMCR]
 
    #     M85                                                       
    #T46(2,IY,IS)=TRQLDV(2,11,IY)
    z[2] = dfd['TRQLDV'].loc[2].loc[MNUMCR]
 
    #     Compressed/Liquefied Natural Gas                          
    #T46(4,IY,IS)=TRQLDV(4,11,IY)
    z[4] = dfd['TRQLDV'].loc[4].loc[MNUMCR]
 
    #     Propane                                                   
    #T46(5,IY,IS)=TRQLDV(5,11,IY)
    z[5] = dfd['TRQLDV'].loc[5].loc[MNUMCR]
 
    #     Electricity                                               
    #T46(6,IY,IS)=TRQLDV(6,11,IY)
    z[6] = dfd['TRQLDV'].loc[6].loc[MNUMCR]
 
    #     Hydrogen                                                  
    #T46(7,IY,IS)=TRQLDV(7,11,IY)
    z[7] = dfd['TRQLDV'].loc[7].loc[MNUMCR]
 
     #   Light-Duty Vehicle                                          
    #T46(9,IY,IS)=FSUM(T46(1,IY,IS),8)
    z[9] = z[1]+z[2]+z[3]+z[4]+z[5]+z[6]+z[7]+z[8]
   

    #     Motor Gasoline excluding E85 1/                           
    #T46(86,IY,IS)=TTHCONS(1,IY)
    z[86] = dfd['TTHCONS'].loc[1]
 
    #     E85 1/                                                    
    #T46(88,IY,IS)=0
    z[88] = 0 * z[86]
 
    #     Distillate Fuel Oil (diesel)                              
    #T46(93,IY,IS)=TTHCONS(2,IY)
    z[93] = dfd['TTHCONS'].loc[2]
 
    #     M85                                                       
    #T46(87,IY,IS)=0
    z[87] = 0 * z[86]
 
    #     Compressed/Liquefied Natural Gas                          
    #T46(89,IY,IS)=TTHCONS(5,IY)
    z[89] = dfd['TTHCONS'].loc[5]
 
    #     Propane                                                   
    #T46(90,IY,IS)=TTHCONS(3,IY)
    z[90] = dfd['TTHCONS'].loc[3]
 
    #     Electricity                                               
    #T46(91,IY,IS)=TTHCONS(4,IY)
    z[91] = dfd['TTHCONS'].loc[4]
 
    #     Hydrogen                                                  
    #T46(92,IY,IS)=TTHCONS(6,IY)
    z[92] = dfd['TTHCONS'].loc[6]
 
     #   2- and 3- Wheel Vehicles                                    
    #T46(94,IY,IS)=FSUM(T46(86,IY,IS),8)
    z[94] = z[86]+z[87]+z[88]+z[89]+z[90]+z[91]+z[92]+z[93]                                                               

 
    

    #     Motor Gasoline excluding E85 1/                           
    #T46(95,IY,IS)=T46(1,IY,IS)-T46(86,IY,IS)
    z[95] = z[1]-z[86]
 
    #     E85 1/                                                    
    #T46(97,IY,IS)=T46(3,IY,IS)-T46(88,IY,IS)
    z[97] = z[3]-z[88]
 
    #     Distillate Fuel Oil (diesel)                              
    #T46(102,IY,IS)=T46(8,IY,IS)-T46(93,IY,IS)
    z[102] = z[8]-z[93]
 
    #     M85                                                       
    #T46(96,IY,IS)=T46(2,IY,IS)-T46(87,IY,IS)
    z[96] = z[2]-z[87]
 
    #     Compressed/Liquefied Natural Gas                          
    #T46(98,IY,IS)=T46(4,IY,IS)-T46(89,IY,IS)
    z[98] = z[4]-z[89]
 
    #     Propane                                                   
    #T46(99,IY,IS)=T46(5,IY,IS)-T46(90,IY,IS)
    z[99] = z[5]-z[90]
 
    #     Electricity                                               
    #T46(100,IY,IS)=T46(6,IY,IS)-T46(91,IY,IS)
    z[100] = z[6]-z[91]
 
    #     Hydrogen                                                  
    #T46(101,IY,IS)=T46(7,IY,IS)-T46(92,IY,IS)
    z[101] = z[7]-z[92]
 
    ##   Light-Duty Vehicles excluding 2- and 3- Wheel               
    #T46(103,IY,IS)=FSUM(T46(95,IY,IS),8)
    z[103] = z[95]+z[96]+z[97]+z[98]+z[99]+z[100]+z[101]+z[102]                                                              

 
    #     Motor Gasoline excluding E85 1/                           
    #T46(58,IY,IS)=CLTFUELBTU(1,IY)
    z[58] = dfd['CLTFUELBTU'].loc[1]
 
    #     E85 1/                                                    
    #T46(78,IY,IS)=CLTFUELBTU(5,IY)
    z[78] = dfd['CLTFUELBTU'].loc[5]
 
    #     Distillate Fuel Oil (diesel)                              
    #T46(59,IY,IS)=CLTFUELBTU(2,IY)
    z[59] = dfd['CLTFUELBTU'].loc[2]
 
    #     Propane                                                   
    #T46(76,IY,IS)=CLTFUELBTU(3,IY)
    z[76] = dfd['CLTFUELBTU'].loc[3]
 
    #     Compressed/Liquefied Natural Gas                          
    #T46(77,IY,IS)=CLTFUELBTU(4,IY)
    z[77] = dfd['CLTFUELBTU'].loc[4]
 
    #     Electricity                                               
    #T46(79,IY,IS)=CLTFUELBTU(6,IY)
    z[79] = dfd['CLTFUELBTU'].loc[6]
 
    #     Hydrogen                                                  
    #T46(80,IY,IS)=CLTFUELBTU(7,IY)
    z[80] = dfd['CLTFUELBTU'].loc[7]
 
    #   Commercial Light Trucks 2/                                  
    z[10] = z[58] + z[78] + z[59] + z[76] + z[77] + z[79] + z[80]
    
    #                                                               

   
 
    #     Motor Gasoline                                            
    #T46(11,IY,IS)=TRQFTRK(1,IY)
    z[11] = dfd['TRQFTRK'].loc[1]
 
    #     Distillate Fuel Oil (diesel)                              
    #T46(12,IY,IS)=TRQFTRK(2,IY)
    z[12] = dfd['TRQFTRK'].loc[2]
 
    #     Compressed/Liquefied Natural Gas                          
    #T46(13,IY,IS)=TRQFTRK(3,IY)
    z[13] = dfd['TRQFTRK'].loc[3]
 
    #     Propane                                                   
    #T46(14,IY,IS)=TRQFTRK(5,IY)
    z[14] = dfd['TRQFTRK'].loc[5]
 
    #     E85 1/                                                    
    #T46(83,IY,IS)=TRQFTRK(6,IY)
    z[83] = dfd['TRQFTRK'].loc[6]
 
    #     Electricity                                               
    #T46(84,IY,IS)=TRQFTRK(7,IY)
    z[84] = dfd['TRQFTRK'].loc[7]
 
    #     Hydrogen                                                  
    #T46(85,IY,IS)=TRQFTRK(8,IY)
    z[85] = dfd['TRQFTRK'].loc[8]
 
     #   Freight Trucks 3/                                           
    #T46(15,IY,IS)=FSUM(T46(11,IY,IS),4)+FSUM(T46(83,IY,IS),3)
    z[15] = z[11]+z[12]+z[13]+z[14] +z[83]+z[84]+z[85]                                                              

    
 
    #     Distillate Fuel Oil (diesel)                              
    #T46(16,IY,IS)=TRQRRF(1,IY)
    z[16] = dfd['TRQRRF'].loc[1]
 
    #     Residual Fuel Oil                                         
    #T46(63,IY,IS)=TRQRRF(2,IY)
    z[63] = dfd['TRQRRF'].loc[2]
 
    #     Compressed Natural Gas                                    
    #T46(64,IY,IS)=TRQRRF(3,IY)
    z[64] = dfd['TRQRRF'].loc[3]
 
    #     Liquefied Natural Gas                                     
    #T46(65,IY,IS)=TRQRRF(4,IY)
    z[65] = dfd['TRQRRF'].loc[4]
 
      #   Freight Rail 4/                                             
    #T46(17,IY,IS)=T46(16,IY,IS)+FSUM(T46(63,IY,IS),3)
    z[17] = z[16]+ z[63]+z[64]+z[65]
                                                               

   
    #     Distillate Fuel Oil (diesel)                              
    #T46(18,IY,IS)=TRQDOMS(1,IY)
    z[18] = dfd['TRQDOMS'].loc[1]
 
    #     Residual Oil                                              
    #T46(19,IY,IS)=TRQDOMS(2,IY)
    z[19] = dfd['TRQDOMS'].loc[2]
 
    #     Compressed Natural Gas                                    
    #T46(20,IY,IS)=TRQDOMS(3,IY)
    z[20] = dfd['TRQDOMS'].loc[3]
 
    #     Liquefied Natural Gas                                     
    #T46(66,IY,IS)=TRQDOMS(4,IY)
    z[66] = dfd['TRQDOMS'].loc[4]
 
     #   Domestic Shipping                                           
    #T46(21,IY,IS)=FSUM(T46(18,IY,IS),3)+T46(66,IY,IS)
    z[21] = z[18]+z[19]+z[20] +z[66]
                                                                

   
 
    #     Distillate Fuel Oil (diesel)                              
    #T46(22,IY,IS)=TRQINTS(1,IY)
    z[22] = dfd['TRQINTS'].loc[1]
 
    #     Residual Oil                                              
    #T46(23,IY,IS)=TRQINTS(2,IY)
    z[23] = dfd['TRQINTS'].loc[2]
 
    #     Compressed Natural Gas                                    
    #T46(67,IY,IS)=TRQINTS(3,IY)
    z[67] = dfd['TRQINTS'].loc[3]
 
    #     Liquefied Natural Gas                                     
    #T46(68,IY,IS)=TRQINTS(4,IY)
    z[68] = dfd['TRQINTS'].loc[4]
 
     #   International Shipping                                      
    #T46(24,IY,IS)=FSUM(T46(22,IY,IS),2)+FSUM(T46(67,IY,IS),2)
    z[24] = z[22]+z[23] +z[67]+z[68]                                                               

   
 
    #     Jet Fuel                                                  
    #T46(25,IY,IS)=TRQAIRT(1,IY)
    z[25] = dfd['TRQAIRT'].loc[1]
 
    #     Aviation Gasoline                                         
    #T46(26,IY,IS)=TRQAIRT(2,IY)
    z[26] = dfd['TRQAIRT'].loc[2]
 
     #   Air Transportation                                          
    #T46(27,IY,IS)=T46(25,IY,IS)+T46(26,IY,IS)
    z[27] = z[25]+z[26]                                                               

   
    #     Jet Fuel and Aviation Gasoline                            
    #T46(28,IY,IS)=TRQMIL(1,IY)+TRQMIL(2,IY)
    z[28] = dfd['TRQMIL'].loc[1] + dfd['TRQMIL'].loc[2]
 
    #     Residual Fuel Oil                                         
    #T46(29,IY,IS)=TRQMIL(3,IY)
    z[29] = dfd['TRQMIL'].loc[3]
 
    #     Distillates and Diesel                                    
    #T46(30,IY,IS)=TRQMIL(4,IY)
    z[30] = dfd['TRQMIL'].loc[4]
 
     #   Military Use                                                
    #T46(31,IY,IS)=FSUM(T46(28,IY,IS),3)
    z[31] = z[28]+z[29]+z[30]
                                                               

    #                                                               

   

    #     Transit Bus                                               
    #T46(60,IY,IS)=SUM(TRQBUS(1,1:8,IY))
    z[60] = dfd['TRQBUS'].loc[1,1:8,:].sum()
 
    #       Motor Gasoline                                          
    #T46(32,IY,IS)=TRQBUS(1,1,IY)
    z[32] = dfd['TRQBUS'].loc[1].loc[1]
 
    #       E85 1/                                                  
    #T46(104,IY,IS)=TRQBUS(1,3,IY)
    z[104] = dfd['TRQBUS'].loc[1].loc[3]
 
    #       Distillate Fuel Oil (diesel)                            
    #T46(33,IY,IS)=TRQBUS(1,2,IY)
    z[33] = dfd['TRQBUS'].loc[1].loc[2]
 
    #       M85                                                     
    #T46(105,IY,IS)=TRQBUS(1,4,IY)
    z[105] = dfd['TRQBUS'].loc[1].loc[4]
 
    #       Compressed/Liquefied Natural Gas                        
    #T46(51,IY,IS)=TRQBUS(1,5,IY)
    z[51] = dfd['TRQBUS'].loc[1].loc[5]
 
    #       Propane                                                 
    #T46(52,IY,IS)=TRQBUS(1,6,IY)
    z[52] = dfd['TRQBUS'].loc[1].loc[6]
 
    #       Electricity                                             
    #T46(106,IY,IS)=TRQBUS(1,7,IY)
    z[106] = dfd['TRQBUS'].loc[1].loc[7]
 
    #       Hydrogen                                                
    #T46(107,IY,IS)=TRQBUS(1,8,IY)
    z[107] = dfd['TRQBUS'].loc[1].loc[8]
 
    #     Intercity Bus                                             
    #T46(61,IY,IS)=SUM(TRQBUS(2,1:8,IY))
    z[61] = dfd['TRQBUS'].loc[2,1:8,:].sum()
 
    #       Motor Gasoline                                          
    #T46(53,IY,IS)=TRQBUS(2,1,IY)
    z[53] = dfd['TRQBUS'].loc[2].loc[1]
 
    #       E85 1/                                                  
    #T46(108,IY,IS)=TRQBUS(2,3,IY)
    z[108] = dfd['TRQBUS'].loc[2].loc[3]
 
    #       Distillate Fuel Oil (diesel)                            
    #T46(34,IY,IS)=TRQBUS(2,2,IY)
    z[34] = dfd['TRQBUS'].loc[2].loc[2]
 
    #       M85                                                     
    #T46(109,IY,IS)=TRQBUS(2,4,IY)
    z[109] = dfd['TRQBUS'].loc[2].loc[4]
 
    #       Compressed/Liquefied Natural Gas                        
    #T46(54,IY,IS)=TRQBUS(2,5,IY)
    z[54] = dfd['TRQBUS'].loc[2].loc[5]
 
    #       Propane                                                 
    #T46(55,IY,IS)=TRQBUS(2,6,IY)
    z[55] = dfd['TRQBUS'].loc[2].loc[6]
 
    #       Electricity                                             
    #T46(110,IY,IS)=TRQBUS(2,7,IY)
    z[110] = dfd['TRQBUS'].loc[2].loc[7]
 
    #       Hydrogen                                                
    #T46(111,IY,IS)=TRQBUS(2,8,IY)
    z[111] = dfd['TRQBUS'].loc[2].loc[8]
 
    #     School Bus                                                
    #T46(62,IY,IS)=SUM(TRQBUS(3,1:8,IY))
    z[62] = dfd['TRQBUS'].loc[3].loc[1:8].sum()
 
    #       Motor Gasoline                                          
    #T46(35,IY,IS)=TRQBUS(3,1,IY)
    z[35] = dfd['TRQBUS'].loc[3].loc[1]
 
    #       E85 1/                                                  
    #T46(112,IY,IS)=TRQBUS(3,3,IY)
    z[112] = dfd['TRQBUS'].loc[3].loc[3]
 
    #       Distillate Fuel Oil (diesel)                            
    #T46(36,IY,IS)=TRQBUS(3,2,IY)
    z[36] = dfd['TRQBUS'].loc[3].loc[2]
 
    #       M85                                                     
    #T46(113,IY,IS)=TRQBUS(3,4,IY)
    z[113] = dfd['TRQBUS'].loc[3].loc[4]
 
    #       Compressed/Liquefied Natural Gas                        
    #T46(56,IY,IS)=TRQBUS(3,5,IY)
    z[56] = dfd['TRQBUS'].loc[3].loc[5]
 
    #       Propane                                                 
    #T46(57,IY,IS)=TRQBUS(3,6,IY)
    z[57] = dfd['TRQBUS'].loc[3].loc[6]
 
    #       Electricity                                             
    #T46(114,IY,IS)=TRQBUS(3,7,IY)
    z[114] = dfd['TRQBUS'].loc[3].loc[7]
 
    #       Hydrogen                                                
    #T46(115,IY,IS)=TRQBUS(3,8,IY)
    z[115] = dfd['TRQBUS'].loc[3].loc[8]

    #   Bus Transportation                                          
    #T46(37,IY,IS)=FSUM(T46(32,IY,IS),5)+FSUM(T46(51,IY,IS),7)
    z[37] = z[60]+z[61]+z[62]
 
    # Rail
    #       Electricity                                             
    #T46(38,IY,IS)=TRQRRP(1,IY)
    z[38] = dfd['TRQRRP'].loc[1]
 
    #       Diesel                                                  
    #T46(39,IY,IS)=TRQRRP(2,IY)
    z[39] = dfd['TRQRRP'].loc[2]
 
    #       Compressed Natural Gas                                  
    #T46(69,IY,IS)=TRQRRP(3,IY)
    z[69] = dfd['TRQRRP'].loc[3]
 
    #       Liquefied Natural Gas                                   
    #T46(70,IY,IS)=TRQRRP(4,IY)
    z[70] = dfd['TRQRRP'].loc[4]

    #     Intercity Rail                                            
    #T46(73,IY,IS)=FSUM(T46(38,IY,IS),2)+FSUM(T46(69,IY,IS),2)
    z[73] = z[38]+z[39] +z[69]+z[70]

   
 
    #       Electricity                                             
    #T46(40,IY,IS)=TRQRRP(5,IY)
    z[40] = dfd['TRQRRP'].loc[5]

     #     Transit Rail                                              
    #T46(74,IY,IS)=T46(40,IY,IS)
    z[74] = z[40]
 
   
 
    #       Electricity                                             
    #T46(41,IY,IS)=TRQRRP(6,IY)
    z[41] = dfd['TRQRRP'].loc[6]
 
    #       Diesel                                                  
    #T46(42,IY,IS)=TRQRRP(7,IY)
    z[42] = dfd['TRQRRP'].loc[7]
 
    #       Compressed Natural Gas                                  
    #T46(71,IY,IS)=TRQRRP(8,IY)
    z[71] = dfd['TRQRRP'].loc[8]
 
    #       Liquefied Natural Gas                                   
    #T46(72,IY,IS)=TRQRRP(9,IY)
    z[72] = dfd['TRQRRP'].loc[9]

     #     Commuter Rail                                             
    #T46(75,IY,IS)=FSUM(T46(41,IY,IS),2)+FSUM(T46(71,IY,IS),2)
    z[75] = z[41]+z[42] +z[71]+z[72]
 
     #   Rail Transportation                                         
    #T46(43,IY,IS)=FSUM(T46(38,IY,IS),5)+FSUM(T46(69,IY,IS),4)
    z[43] = z[38]+z[39]+z[40]+z[41]+z[42] +z[69]+z[70]+z[71]+z[72]
                                                               

    #   Recreational Boats                                          
    #T46(44,IY,IS)=TRQBOAT(1,IY)+TRQBOAT(2,IY)
    z[44] = dfd['TRQBOAT'].loc[1] + dfd['TRQBOAT'].loc[2]
 
    #     Gasoline                                                  
    #T46(49,IY,IS)=TRQBOAT(1,IY)
    z[49] = dfd['TRQBOAT'].loc[1]
 
    #     Distillate Fuel Oil (diesel)                              
    #T46(50,IY,IS)=TRQBOAT(2,IY)
    z[50] = dfd['TRQBOAT'].loc[2]
 
    #                                                               

    #   Lubricants                                                  
    #T46(45,IY,IS)=TRQLUB(IY)
    z[45] = dfd['TRQLUB']
 
    #   Pipeline Fuel Natural Gas                                   
    #T46(46,IY,IS)=QGPTR(11,IY)*1000.
    z[46] = dfd['QGPTR'].loc[MNUMCR] 
 
    #   Natural Gas Liquefaction for Export                         
    #T46(116,IY,IS)=QNGLQ(11,IY)*1000
    z[116] = dfd['QNGLQ'].loc[MNUMCR] 
 
    #                                                               

    #   Total Miscellaneous                                         
    #T46(47,IY,IS)=T46(31,IY,IS)+T46(37,IY,IS)+FSUM(T46(43,IY,IS),4)
    z[47] = z[31]+z[37]+z[43]+z[44]+z[45]+z[46]
 
    #                                                               

    #   Total Consumption                                           
    #T46(48,IY,IS)=T46(9,IY,IS)+T46(10,IY,IS)+T46(15,IY,IS)+T46(17,IY,IS)+T46(21,IY,IS)+T46(24,IY,IS)+T46(27,IY,IS)+T46(47,IY,IS)+T46(116,IY,IS)
    z[48] = z[9]+z[10]+z[15]+z[17]+z[21]+z[24]+z[27]+z[47]+z[116]
    
    return z
 
