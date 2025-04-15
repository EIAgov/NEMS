#-*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_065(dfd, table_spec, table_id):
    """Fill table Regional Natural Gas Plant Liquid Production by Region and T
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


    #   Regional Natural Gas Plant Liquid Production by Region and T
    #   (million barrels per day)
    #     Region and Stream                                         

    #   Ethane Production                                           

    #      East                                                     
    #T65(1,IY,IS)=OGNGPLET(8,IY)+OGNGPLET(9,IY)+OGNGPLET(10,IY)+OGNGPLET(12,IY)+OGNGPLET(15,IY)+OGNGPLET(16,IY)+OGNGPLET(19,IY)+OGNGPLET(22,IY)+OGNGPLET(23,IY)+OGNGPLET(24,IY)+OGNGPLET(25,IY)+OGNGPLET(33,IY)+OGNGPLET(34,IY)+OGNGPLET(37,IY)+OGNGPLET(38,IY)+OGNGPLET(40,IY)+OGNGPLET(43,IY)+OGNGPLET(44,IY)+OGNGPLET(45,IY)+OGNGPLET(47,IY)+OGNGPLET(61,IY)+OGNGPLET(62,IY)+OGNGPLET(64,IY)+OGNGPLET(65,IY)
    z[1] = dfd['OGNGPLET'].loc[8] + dfd['OGNGPLET'].loc[9] + dfd['OGNGPLET'].loc[10] + \
           dfd['OGNGPLET'].loc[12] + dfd['OGNGPLET'].loc[15] + dfd['OGNGPLET'].loc[16] + \
           dfd['OGNGPLET'].loc[19] + dfd['OGNGPLET'].loc[22] + dfd['OGNGPLET'].loc[23] + \
           dfd['OGNGPLET'].loc[24] + dfd['OGNGPLET'].loc[25] + dfd['OGNGPLET'].loc[33] + \
           dfd['OGNGPLET'].loc[34] + dfd['OGNGPLET'].loc[37] + dfd['OGNGPLET'].loc[38] + \
           dfd['OGNGPLET'].loc[40] + dfd['OGNGPLET'].loc[43] + dfd['OGNGPLET'].loc[44] + \
           dfd['OGNGPLET'].loc[45] + dfd['OGNGPLET'].loc[47] + dfd['OGNGPLET'].loc[61] + \
           dfd['OGNGPLET'].loc[62] + dfd['OGNGPLET'].loc[64] + dfd['OGNGPLET'].loc[65]
 
    #      Gulf Coast                                               
    #T65(2,IY,IS)=OGNGPLET(1,IY)+OGNGPLET(2,IY)+OGNGPLET(11,IY)+OGNGPLET(20,IY)+OGNGPLET(21,IY)+OGNGPLET(27,IY)+OGNGPLET(28,IY)+OGNGPLET(48,IY)+OGNGPLET(49,IY)+OGNGPLET(50,IY)+OGNGPLET(51,IY)+OGNGPLET(53,IY)
    z[2] = dfd['OGNGPLET'].loc[1] + dfd['OGNGPLET'].loc[2] + dfd['OGNGPLET'].loc[11] + \
           dfd['OGNGPLET'].loc[20] + dfd['OGNGPLET'].loc[21] + dfd['OGNGPLET'].loc[27] + \
           dfd['OGNGPLET'].loc[28] + dfd['OGNGPLET'].loc[48] + dfd['OGNGPLET'].loc[49] + \
           dfd['OGNGPLET'].loc[50] + dfd['OGNGPLET'].loc[51] + dfd['OGNGPLET'].loc[53]
 
    #      Midcontinent                                             
    #T65(3,IY,IS)=OGNGPLET(5,IY)+OGNGPLET(17,IY)+OGNGPLET(18,IY)+OGNGPLET(26,IY)+OGNGPLET(29,IY)+OGNGPLET(31,IY)+OGNGPLET(41,IY)+OGNGPLET(59,IY)
    z[3] = dfd['OGNGPLET'].loc[5] + dfd['OGNGPLET'].loc[17] + dfd['OGNGPLET'].loc[18] + \
           dfd['OGNGPLET'].loc[26] + dfd['OGNGPLET'].loc[29] + dfd['OGNGPLET'].loc[31] + \
           dfd['OGNGPLET'].loc[41] + dfd['OGNGPLET'].loc[59]
 
    #      Southwest                                                
    #T65(4,IY,IS)=OGNGPLET(35,IY)+OGNGPLET(52,IY)+OGNGPLET(54,IY)+OGNGPLET(55,IY)+OGNGPLET(56,IY)+OGNGPLET(57,IY)+OGNGPLET(58,IY)
    z[4] = dfd['OGNGPLET'].loc[35] + dfd['OGNGPLET'].loc[52] + dfd['OGNGPLET'].loc[54] + \
           dfd['OGNGPLET'].loc[55] + dfd['OGNGPLET'].loc[56] + dfd['OGNGPLET'].loc[57] + \
           dfd['OGNGPLET'].loc[58]
 
    #      Rocky Mountain                                           
    #T65(5,IY,IS)=OGNGPLET(4,IY)+OGNGPLET(7,IY)+OGNGPLET(14,IY)+OGNGPLET(32,IY)+OGNGPLET(36,IY)+OGNGPLET(60,IY)+OGNGPLET(66,IY)
    z[5] = dfd['OGNGPLET'].loc[4] + dfd['OGNGPLET'].loc[7] + dfd['OGNGPLET'].loc[14] + \
           dfd['OGNGPLET'].loc[32] + dfd['OGNGPLET'].loc[36] + dfd['OGNGPLET'].loc[60] + \
           dfd['OGNGPLET'].loc[66]
 
    #      Northern Great Plains                                    
    #T65(6,IY,IS)=OGNGPLET(30,IY)+OGNGPLET(39,IY)+OGNGPLET(46,IY)
    z[6] = dfd['OGNGPLET'].loc[30] + dfd['OGNGPLET'].loc[39] + dfd['OGNGPLET'].loc[46]
 
    #      West Coast                                               
    #T65(7,IY,IS)=OGNGPLET(6,IY)+OGNGPLET(42,IY)+OGNGPLET(63,IY)
    z[7] = dfd['OGNGPLET'].loc[6] + dfd['OGNGPLET'].loc[42] + dfd['OGNGPLET'].loc[63]
 
    #      Atlantic                                                 
    #T65(8,IY,IS)=OGNGPLET(67,IY)+OGNGPLET(68,IY)+OGNGPLET(69,IY)+OGNGPLET(76,IY)+OGNGPLET(77,IY)+OGNGPLET(78,IY)
    z[8] = dfd['OGNGPLET'].loc[67] + dfd['OGNGPLET'].loc[68] + dfd['OGNGPLET'].loc[69] + \
           dfd['OGNGPLET'].loc[76] + dfd['OGNGPLET'].loc[77] + dfd['OGNGPLET'].loc[78]
 
    #      Pacific                                                  
    #T65(9,IY,IS)=OGNGPLET(73,IY)+OGNGPLET(74,IY)+OGNGPLET(82,IY)+OGNGPLET(83,IY)
    z[9] = dfd['OGNGPLET'].loc[73] + dfd['OGNGPLET'].loc[74] + dfd['OGNGPLET'].loc[82] + \
           dfd['OGNGPLET'].loc[83]
 
    #      Gulf of Mexico                                           
    #T65(10,IY,IS)=OGNGPLET(70,IY)+OGNGPLET(71,IY)+OGNGPLET(72,IY)+OGNGPLET(79,IY)+OGNGPLET(80,IY)+OGNGPLET(81,IY)
    z[10] = dfd['OGNGPLET'].loc[70] + dfd['OGNGPLET'].loc[71] + dfd['OGNGPLET'].loc[72] + \
            dfd['OGNGPLET'].loc[79] + dfd['OGNGPLET'].loc[80] + dfd['OGNGPLET'].loc[81]
 
    #      Alaska                                                   
    #T65(11,IY,IS)=OGNGPLET(3,IY)+OGNGPLET(75,IY)+OGNGPLET(84,IY)
    z[11] = dfd['OGNGPLET'].loc[3] + dfd['OGNGPLET'].loc[75] + dfd['OGNGPLET'].loc[84]
 
    #         Total                                                 
    #T65(12,IY,IS)=FSUM(T65(1,IY,IS),11)
    z[12] = z[1]+z[2]+z[3]+z[4]+z[5]+z[6]+z[7]+z[8]+z[9]+z[10]+z[11]
 
    #                                                               

    #   Propane Production                                          

    #      East                                                     
    #T65(13,IY,IS)=OGNGPLPR(8,IY)+OGNGPLPR(9,IY)+OGNGPLPR(10,IY)+OGNGPLPR(12,IY)+OGNGPLPR(15,IY)+OGNGPLPR(16,IY)+OGNGPLPR(19,IY)+OGNGPLPR(22,IY)+OGNGPLPR(23,IY)+OGNGPLPR(24,IY)+OGNGPLPR(25,IY)+OGNGPLPR(33,IY)+OGNGPLPR(34,IY)+OGNGPLPR(37,IY)+OGNGPLPR(38,IY)+OGNGPLPR(40,IY)+OGNGPLPR(43,IY)+OGNGPLPR(44,IY)+OGNGPLPR(45,IY)+OGNGPLPR(47,IY)+OGNGPLPR(61,IY)+OGNGPLPR(62,IY)+OGNGPLPR(64,IY)+OGNGPLPR(65,IY)
    z[13] = dfd['OGNGPLPR'].loc[8] + dfd['OGNGPLPR'].loc[9] + dfd['OGNGPLPR'].loc[10] + \
            dfd['OGNGPLPR'].loc[12] + dfd['OGNGPLPR'].loc[15] + dfd['OGNGPLPR'].loc[16] + \
            dfd['OGNGPLPR'].loc[19] + dfd['OGNGPLPR'].loc[22] + dfd['OGNGPLPR'].loc[23] + \
            dfd['OGNGPLPR'].loc[24] + dfd['OGNGPLPR'].loc[25] + dfd['OGNGPLPR'].loc[33] + \
            dfd['OGNGPLPR'].loc[34] + dfd['OGNGPLPR'].loc[37] + dfd['OGNGPLPR'].loc[38] + \
            dfd['OGNGPLPR'].loc[40] + dfd['OGNGPLPR'].loc[43] + dfd['OGNGPLPR'].loc[44] + \
            dfd['OGNGPLPR'].loc[45] + dfd['OGNGPLPR'].loc[47] + dfd['OGNGPLPR'].loc[61] + \
            dfd['OGNGPLPR'].loc[62] + dfd['OGNGPLPR'].loc[64] + dfd['OGNGPLPR'].loc[65]
 
    #      Gulf Coast                                               
    #T65(14,IY,IS)=OGNGPLPR(1,IY)+OGNGPLPR(2,IY)+OGNGPLPR(11,IY)+OGNGPLPR(20,IY)+OGNGPLPR(21,IY)+OGNGPLPR(27,IY)+OGNGPLPR(28,IY)+OGNGPLPR(48,IY)+OGNGPLPR(49,IY)+OGNGPLPR(50,IY)+OGNGPLPR(51,IY)+OGNGPLPR(53,IY)
    z[14] = dfd['OGNGPLPR'].loc[1] + dfd['OGNGPLPR'].loc[2] + dfd['OGNGPLPR'].loc[11] + \
            dfd['OGNGPLPR'].loc[20] + dfd['OGNGPLPR'].loc[21] + dfd['OGNGPLPR'].loc[27] + \
            dfd['OGNGPLPR'].loc[28] + dfd['OGNGPLPR'].loc[48] + dfd['OGNGPLPR'].loc[49] + \
            dfd['OGNGPLPR'].loc[50] + dfd['OGNGPLPR'].loc[51] + dfd['OGNGPLPR'].loc[53]
 
    #      Midcontinent                                             
    #T65(15,IY,IS)=OGNGPLPR(5,IY)+OGNGPLPR(17,IY)+OGNGPLPR(18,IY)+OGNGPLPR(26,IY)+OGNGPLPR(29,IY)+OGNGPLPR(31,IY)+OGNGPLPR(41,IY)+OGNGPLPR(59,IY)
    z[15] = dfd['OGNGPLPR'].loc[5] + dfd['OGNGPLPR'].loc[17] + dfd['OGNGPLPR'].loc[18] + \
            dfd['OGNGPLPR'].loc[26] + dfd['OGNGPLPR'].loc[29] + dfd['OGNGPLPR'].loc[31] + \
            dfd['OGNGPLPR'].loc[41] + dfd['OGNGPLPR'].loc[59]
 
    #      Southwest                                                
    #T65(16,IY,IS)=OGNGPLPR(35,IY)+OGNGPLPR(52,IY)+OGNGPLPR(54,IY)+OGNGPLPR(55,IY)+OGNGPLPR(56,IY)+OGNGPLPR(57,IY)+OGNGPLPR(58,IY)
    z[16] = dfd['OGNGPLPR'].loc[35] + dfd['OGNGPLPR'].loc[52] + dfd['OGNGPLPR'].loc[54] + \
            dfd['OGNGPLPR'].loc[55] + dfd['OGNGPLPR'].loc[56] + dfd['OGNGPLPR'].loc[57] + \
            dfd['OGNGPLPR'].loc[58]
 
    #      Rocky Mountain                                           
    #T65(17,IY,IS)=OGNGPLPR(4,IY)+OGNGPLPR(7,IY)+OGNGPLPR(14,IY)+OGNGPLPR(32,IY)+OGNGPLPR(36,IY)+OGNGPLPR(60,IY)+OGNGPLPR(66,IY)
    z[17] = dfd['OGNGPLPR'].loc[4] + dfd['OGNGPLPR'].loc[7] + dfd['OGNGPLPR'].loc[14] + \
            dfd['OGNGPLPR'].loc[32] + dfd['OGNGPLPR'].loc[36] + dfd['OGNGPLPR'].loc[60] + \
            dfd['OGNGPLPR'].loc[66]
 
    #      Northern Great Plains                                    
    #T65(18,IY,IS)=OGNGPLPR(30,IY)+OGNGPLPR(39,IY)+OGNGPLPR(46,IY)
    z[18] = dfd['OGNGPLPR'].loc[30] + dfd['OGNGPLPR'].loc[39] + dfd['OGNGPLPR'].loc[46]
 
    #      West Coast                                               
    #T65(19,IY,IS)=OGNGPLPR(6,IY)+OGNGPLPR(42,IY)+OGNGPLPR(63,IY)
    z[19] = dfd['OGNGPLPR'].loc[6] + dfd['OGNGPLPR'].loc[42] + dfd['OGNGPLPR'].loc[63]
 
    #      Atlantic                                                 
    #T65(20,IY,IS)=OGNGPLPR(67,IY)+OGNGPLPR(68,IY)+OGNGPLPR(69,IY)+OGNGPLPR(76,IY)+OGNGPLPR(77,IY)+OGNGPLPR(78,IY)
    z[20] = dfd['OGNGPLPR'].loc[67] + dfd['OGNGPLPR'].loc[68] + dfd['OGNGPLPR'].loc[69] + \
            dfd['OGNGPLPR'].loc[76] + dfd['OGNGPLPR'].loc[77] + dfd['OGNGPLPR'].loc[78]
 
    #      Pacific                                                  
    #T65(21,IY,IS)=OGNGPLPR(73,IY)+OGNGPLPR(74,IY)+OGNGPLPR(82,IY)+OGNGPLPR(83,IY)
    z[21] = dfd['OGNGPLPR'].loc[73] + dfd['OGNGPLPR'].loc[74] + dfd['OGNGPLPR'].loc[82] + \
            dfd['OGNGPLPR'].loc[83]
 
    #      Gulf of Mexico                                           
    #T65(22,IY,IS)=OGNGPLPR(70,IY)+OGNGPLPR(71,IY)+OGNGPLPR(72,IY)+OGNGPLPR(79,IY)+OGNGPLPR(80,IY)+OGNGPLPR(81,IY)
    z[22] = dfd['OGNGPLPR'].loc[70] + dfd['OGNGPLPR'].loc[71] + dfd['OGNGPLPR'].loc[72] + \
            dfd['OGNGPLPR'].loc[79] + dfd['OGNGPLPR'].loc[80] + dfd['OGNGPLPR'].loc[81]
 
    #      Alaska                                                   
    #T65(23,IY,IS)=OGNGPLPR(3,IY)+OGNGPLPR(75,IY)+OGNGPLPR(84,IY)
    z[23] = dfd['OGNGPLPR'].loc[3] + dfd['OGNGPLPR'].loc[75] + dfd['OGNGPLPR'].loc[84]
 
    #         Total                                                 
    #T65(24,IY,IS)=FSUM(T65(13,IY,IS),11)
    z[24] = z[13]+z[14]+z[15]+z[16]+z[17]+z[18]+z[19]+z[20]+z[21]+z[22]+z[23]

    #                                                               

    #   Normal Butane Production                                    

    #      East                                                     
    #T65(25,IY,IS)=OGNGPLBU(8,IY)+OGNGPLBU(9,IY)+OGNGPLBU(10,IY)+OGNGPLBU(12,IY)+OGNGPLBU(15,IY)+OGNGPLBU(16,IY)+OGNGPLBU(19,IY)+OGNGPLBU(22,IY)+OGNGPLBU(23,IY)+OGNGPLBU(24,IY)+OGNGPLBU(25,IY)+OGNGPLBU(33,IY)+OGNGPLBU(34,IY)+OGNGPLBU(37,IY)+OGNGPLBU(38,IY)+OGNGPLBU(40,IY)+OGNGPLBU(43,IY)+OGNGPLBU(44,IY)+OGNGPLBU(45,IY)+OGNGPLBU(47,IY)+OGNGPLBU(61,IY)+OGNGPLBU(62,IY)+OGNGPLBU(64,IY)+OGNGPLBU(65,IY)
    z[25] = dfd['OGNGPLBU'].loc[8] + dfd['OGNGPLBU'].loc[9] + dfd['OGNGPLBU'].loc[10] + \
            dfd['OGNGPLBU'].loc[12] + dfd['OGNGPLBU'].loc[15] + dfd['OGNGPLBU'].loc[16] + \
            dfd['OGNGPLBU'].loc[19] + dfd['OGNGPLBU'].loc[22] + dfd['OGNGPLBU'].loc[23] + \
            dfd['OGNGPLBU'].loc[24] + dfd['OGNGPLBU'].loc[25] + dfd['OGNGPLBU'].loc[33] + \
            dfd['OGNGPLBU'].loc[34] + dfd['OGNGPLBU'].loc[37] + dfd['OGNGPLBU'].loc[38] + \
            dfd['OGNGPLBU'].loc[40] + dfd['OGNGPLBU'].loc[43] + dfd['OGNGPLBU'].loc[44] + \
            dfd['OGNGPLBU'].loc[45] + dfd['OGNGPLBU'].loc[47] + dfd['OGNGPLBU'].loc[61] + \
            dfd['OGNGPLBU'].loc[62] + dfd['OGNGPLBU'].loc[64] + dfd['OGNGPLBU'].loc[65]
 
    #      Gulf Coast                                               
    #T65(26,IY,IS)=OGNGPLBU(1,IY)+OGNGPLBU(2,IY)+OGNGPLBU(11,IY)+OGNGPLBU(20,IY)+OGNGPLBU(21,IY)+OGNGPLBU(27,IY)+OGNGPLBU(28,IY)+OGNGPLBU(48,IY)+OGNGPLBU(49,IY)+OGNGPLBU(50,IY)+OGNGPLBU(51,IY)+OGNGPLBU(53,IY)
    z[26] = dfd['OGNGPLBU'].loc[1] + dfd['OGNGPLBU'].loc[2] + dfd['OGNGPLBU'].loc[11] + \
            dfd['OGNGPLBU'].loc[20] + dfd['OGNGPLBU'].loc[21] + dfd['OGNGPLBU'].loc[27] + \
            dfd['OGNGPLBU'].loc[28] + dfd['OGNGPLBU'].loc[48] + dfd['OGNGPLBU'].loc[49] + \
            dfd['OGNGPLBU'].loc[50] + dfd['OGNGPLBU'].loc[51] + dfd['OGNGPLBU'].loc[53]
 
    #      Midcontinent                                             
    #T65(27,IY,IS)=OGNGPLBU(5,IY)+OGNGPLBU(17,IY)+OGNGPLBU(18,IY)+OGNGPLBU(26,IY)+OGNGPLBU(29,IY)+OGNGPLBU(31,IY)+OGNGPLBU(41,IY)+OGNGPLBU(59,IY)
    z[27] = dfd['OGNGPLBU'].loc[5] + dfd['OGNGPLBU'].loc[17] + dfd['OGNGPLBU'].loc[18] + \
            dfd['OGNGPLBU'].loc[26] + dfd['OGNGPLBU'].loc[29] + dfd['OGNGPLBU'].loc[31] + \
            dfd['OGNGPLBU'].loc[41] + dfd['OGNGPLBU'].loc[59]
 
    #      Southwest                                                
    #T65(28,IY,IS)=OGNGPLBU(35,IY)+OGNGPLBU(52,IY)+OGNGPLBU(54,IY)+OGNGPLBU(55,IY)+OGNGPLBU(56,IY)+OGNGPLBU(57,IY)+OGNGPLBU(58,IY)
    z[28] = dfd['OGNGPLBU'].loc[35] + dfd['OGNGPLBU'].loc[52] + dfd['OGNGPLBU'].loc[54] + \
            dfd['OGNGPLBU'].loc[55] + dfd['OGNGPLBU'].loc[56] + dfd['OGNGPLBU'].loc[57] + \
            dfd['OGNGPLBU'].loc[58]
 
    #      Rocky Mountain                                           
    #T65(29,IY,IS)=OGNGPLBU(4,IY)+OGNGPLBU(7,IY)+OGNGPLBU(14,IY)+OGNGPLBU(32,IY)+OGNGPLBU(36,IY)+OGNGPLBU(60,IY)+OGNGPLBU(66,IY)
    z[29] = dfd['OGNGPLBU'].loc[4] + dfd['OGNGPLBU'].loc[7] + dfd['OGNGPLBU'].loc[14] + \
            dfd['OGNGPLBU'].loc[32] + dfd['OGNGPLBU'].loc[36] + dfd['OGNGPLBU'].loc[60] + \
            dfd['OGNGPLBU'].loc[66]
 
    #      Northern Great Plains                                    
    #T65(30,IY,IS)=OGNGPLBU(30,IY)+OGNGPLBU(39,IY)+OGNGPLBU(46,IY)
    z[30] = dfd['OGNGPLBU'].loc[30] + dfd['OGNGPLBU'].loc[39] + dfd['OGNGPLBU'].loc[46]
 
    #      West Coast                                               
    #T65(31,IY,IS)=OGNGPLBU(6,IY)+OGNGPLBU(42,IY)+OGNGPLBU(63,IY)
    z[31] = dfd['OGNGPLBU'].loc[6] + dfd['OGNGPLBU'].loc[42] + dfd['OGNGPLBU'].loc[63]
 
    #      Atlantic                                                 
    #T65(32,IY,IS)=OGNGPLBU(67,IY)+OGNGPLBU(68,IY)+OGNGPLBU(69,IY)+OGNGPLBU(76,IY)+OGNGPLBU(77,IY)+OGNGPLBU(78,IY)
    z[32] = dfd['OGNGPLBU'].loc[67] + dfd['OGNGPLBU'].loc[68] + dfd['OGNGPLBU'].loc[69] + \
            dfd['OGNGPLBU'].loc[76] + dfd['OGNGPLBU'].loc[77] + dfd['OGNGPLBU'].loc[78]
 
    #      Pacific                                                  
    #T65(33,IY,IS)=OGNGPLBU(73,IY)+OGNGPLBU(74,IY)+OGNGPLBU(82,IY)+OGNGPLBU(83,IY)
    z[33] = dfd['OGNGPLBU'].loc[73] + dfd['OGNGPLBU'].loc[74] + dfd['OGNGPLBU'].loc[82] + \
            dfd['OGNGPLBU'].loc[83]
 
    #      Gulf of Mexico                                           
    #T65(34,IY,IS)=OGNGPLBU(70,IY)+OGNGPLBU(71,IY)+OGNGPLBU(72,IY)+OGNGPLBU(79,IY)+OGNGPLBU(80,IY)+OGNGPLBU(81,IY)
    z[34] = dfd['OGNGPLBU'].loc[70] + dfd['OGNGPLBU'].loc[71] + dfd['OGNGPLBU'].loc[72] + \
            dfd['OGNGPLBU'].loc[79] + dfd['OGNGPLBU'].loc[80] + dfd['OGNGPLBU'].loc[81]
 
    #      Alaska                                                   
    #T65(35,IY,IS)=OGNGPLBU(3,IY)+OGNGPLBU(75,IY)+OGNGPLBU(84,IY)
    z[35] = dfd['OGNGPLBU'].loc[3] + dfd['OGNGPLBU'].loc[75] + dfd['OGNGPLBU'].loc[84]
 
    #         Total                                                 
    #T65(36,IY,IS)=FSUM(T65(25,IY,IS),11)
    z[36] = z[25]+z[26]+z[27]+z[28]+z[29]+z[30]+z[31]+z[32]+z[33]+z[34]+z[35]

    #                                                               

    #   Isobutane Production                                        

    #      East                                                     
    #T65(37,IY,IS)=OGNGPLIS(8,IY)+OGNGPLIS(9,IY)+OGNGPLIS(10,IY)+OGNGPLIS(12,IY)+OGNGPLIS(15,IY)+OGNGPLIS(16,IY)+OGNGPLIS(19,IY)+OGNGPLIS(22,IY)+OGNGPLIS(23,IY)+OGNGPLIS(24,IY)+OGNGPLIS(25,IY)+OGNGPLIS(33,IY)+OGNGPLIS(34,IY)+OGNGPLIS(37,IY)+OGNGPLIS(38,IY)+OGNGPLIS(40,IY)+OGNGPLIS(43,IY)+OGNGPLIS(44,IY)+OGNGPLIS(45,IY)+OGNGPLIS(47,IY)+OGNGPLIS(61,IY)+OGNGPLIS(62,IY)+OGNGPLIS(64,IY)+OGNGPLIS(65,IY)
    z[37] = dfd['OGNGPLIS'].loc[8] + dfd['OGNGPLIS'].loc[9] + dfd['OGNGPLIS'].loc[10] + \
            dfd['OGNGPLIS'].loc[12] + dfd['OGNGPLIS'].loc[15] + dfd['OGNGPLIS'].loc[16] + \
            dfd['OGNGPLIS'].loc[19] + dfd['OGNGPLIS'].loc[22] + dfd['OGNGPLIS'].loc[23] + \
            dfd['OGNGPLIS'].loc[24] + dfd['OGNGPLIS'].loc[25] + dfd['OGNGPLIS'].loc[33] + \
            dfd['OGNGPLIS'].loc[34] + dfd['OGNGPLIS'].loc[37] + dfd['OGNGPLIS'].loc[38] + \
            dfd['OGNGPLIS'].loc[40] + dfd['OGNGPLIS'].loc[43] + dfd['OGNGPLIS'].loc[44] + \
            dfd['OGNGPLIS'].loc[45] + dfd['OGNGPLIS'].loc[47] + dfd['OGNGPLIS'].loc[61] + \
            dfd['OGNGPLIS'].loc[62] + dfd['OGNGPLIS'].loc[64] + dfd['OGNGPLIS'].loc[65]
 
    #      Gulf Coast                                               
    #T65(38,IY,IS)=OGNGPLIS(1,IY)+OGNGPLIS(2,IY)+OGNGPLIS(11,IY)+OGNGPLIS(20,IY)+OGNGPLIS(21,IY)+OGNGPLIS(27,IY)+OGNGPLIS(28,IY)+OGNGPLIS(48,IY)+OGNGPLIS(49,IY)+OGNGPLIS(50,IY)+OGNGPLIS(51,IY)+OGNGPLIS(53,IY)
    z[38] = dfd['OGNGPLIS'].loc[1] + dfd['OGNGPLIS'].loc[2] + dfd['OGNGPLIS'].loc[11] + \
            dfd['OGNGPLIS'].loc[20] + dfd['OGNGPLIS'].loc[21] + dfd['OGNGPLIS'].loc[27] + \
            dfd['OGNGPLIS'].loc[28] + dfd['OGNGPLIS'].loc[48] + dfd['OGNGPLIS'].loc[49] + \
            dfd['OGNGPLIS'].loc[50] + dfd['OGNGPLIS'].loc[51] + dfd['OGNGPLIS'].loc[53]
 
    #      Midcontinent                                             
    #T65(39,IY,IS)=OGNGPLIS(5,IY)+OGNGPLIS(17,IY)+OGNGPLIS(18,IY)+OGNGPLIS(26,IY)+OGNGPLIS(29,IY)+OGNGPLIS(31,IY)+OGNGPLIS(41,IY)+OGNGPLIS(59,IY)
    z[39] = dfd['OGNGPLIS'].loc[5] + dfd['OGNGPLIS'].loc[17] + dfd['OGNGPLIS'].loc[18] + \
            dfd['OGNGPLIS'].loc[26] + dfd['OGNGPLIS'].loc[29] + dfd['OGNGPLIS'].loc[31] + \
            dfd['OGNGPLIS'].loc[41] + dfd['OGNGPLIS'].loc[59]
 
    #      Southwest                                                
    #T65(40,IY,IS)=OGNGPLIS(35,IY)+OGNGPLIS(52,IY)+OGNGPLIS(54,IY)+OGNGPLIS(55,IY)+OGNGPLIS(56,IY)+OGNGPLIS(57,IY)+OGNGPLIS(58,IY)
    z[40] = dfd['OGNGPLIS'].loc[35] + dfd['OGNGPLIS'].loc[52] + dfd['OGNGPLIS'].loc[54] + \
            dfd['OGNGPLIS'].loc[55] + dfd['OGNGPLIS'].loc[56] + dfd['OGNGPLIS'].loc[57] + \
            dfd['OGNGPLIS'].loc[58]
 
    #      Rocky Mountain                                           
    #T65(41,IY,IS)=OGNGPLIS(4,IY)+OGNGPLIS(7,IY)+OGNGPLIS(14,IY)+OGNGPLIS(32,IY)+OGNGPLIS(36,IY)+OGNGPLIS(60,IY)+OGNGPLIS(66,IY)
    z[41] = dfd['OGNGPLIS'].loc[4] + dfd['OGNGPLIS'].loc[7] + dfd['OGNGPLIS'].loc[14] + \
            dfd['OGNGPLIS'].loc[32] + dfd['OGNGPLIS'].loc[36] + dfd['OGNGPLIS'].loc[60] + \
            dfd['OGNGPLIS'].loc[66]
 
    #      Northern Great Plains                                    
    #T65(42,IY,IS)=OGNGPLIS(30,IY)+OGNGPLIS(39,IY)+OGNGPLIS(46,IY)
    z[42] = dfd['OGNGPLIS'].loc[30] + dfd['OGNGPLIS'].loc[39] + dfd['OGNGPLIS'].loc[46]
 
    #      West Coast                                               
    #T65(43,IY,IS)=OGNGPLIS(6,IY)+OGNGPLIS(42,IY)+OGNGPLIS(63,IY)
    z[43] = dfd['OGNGPLIS'].loc[6] + dfd['OGNGPLIS'].loc[42] + dfd['OGNGPLIS'].loc[63]
 
    #      Atlantic                                                 
    #T65(44,IY,IS)=OGNGPLIS(67,IY)+OGNGPLIS(68,IY)+OGNGPLIS(69,IY)+OGNGPLIS(76,IY)+OGNGPLIS(77,IY)+OGNGPLIS(78,IY)
    z[44] = dfd['OGNGPLIS'].loc[67] + dfd['OGNGPLIS'].loc[68] + dfd['OGNGPLIS'].loc[69] + \
            dfd['OGNGPLIS'].loc[76] + dfd['OGNGPLIS'].loc[77] + dfd['OGNGPLIS'].loc[78]
 
    #      Pacific                                                  
    #T65(45,IY,IS)=OGNGPLIS(73,IY)+OGNGPLIS(74,IY)+OGNGPLIS(82,IY)+OGNGPLIS(83,IY)
    z[45] = dfd['OGNGPLIS'].loc[73] + dfd['OGNGPLIS'].loc[74] + dfd['OGNGPLIS'].loc[82] + \
            dfd['OGNGPLIS'].loc[83]
 
    #      Gulf of Mexico                                           
    #T65(46,IY,IS)=OGNGPLIS(70,IY)+OGNGPLIS(71,IY)+OGNGPLIS(72,IY)+OGNGPLIS(79,IY)+OGNGPLIS(80,IY)+OGNGPLIS(81,IY)
    z[46] = dfd['OGNGPLIS'].loc[70] + dfd['OGNGPLIS'].loc[71] + dfd['OGNGPLIS'].loc[72] + \
            dfd['OGNGPLIS'].loc[79] + dfd['OGNGPLIS'].loc[80] + dfd['OGNGPLIS'].loc[81]
 
    #      Alaska                                                   
    #T65(47,IY,IS)=OGNGPLIS(3,IY)+OGNGPLIS(75,IY)+OGNGPLIS(84,IY)
    z[47] = dfd['OGNGPLIS'].loc[3] + dfd['OGNGPLIS'].loc[75] + dfd['OGNGPLIS'].loc[84]
 
    #         Total                                                 
    #T65(48,IY,IS)=FSUM(T65(37,IY,IS),11)
    z[48] = z[37]+z[38]+z[39]+z[40]+z[41]+z[42]+z[43]+z[44]+z[45]+z[46]+z[47]

    #                                                               
 
    #   Natural Gasoline Production                                 
 
    #      East                                                     
    #T65(49,IY,IS)=OGNGPLPP(8,IY)+OGNGPLPP(9,IY)+OGNGPLPP(10,IY)+OGNGPLPP(12,IY)+OGNGPLPP(15,IY)+OGNGPLPP(16,IY)+OGNGPLPP(19,IY)+OGNGPLPP(22,IY)+OGNGPLPP(23,IY)+OGNGPLPP(24,IY)+OGNGPLPP(25,IY)+OGNGPLPP(33,IY)+OGNGPLPP(34,IY)+OGNGPLPP(37,IY)+OGNGPLPP(38,IY)+OGNGPLPP(40,IY)+OGNGPLPP(43,IY)+OGNGPLPP(44,IY)+OGNGPLPP(45,IY)+OGNGPLPP(47,IY)+OGNGPLPP(61,IY)+OGNGPLPP(62,IY)+OGNGPLPP(64,IY)+OGNGPLPP(65,IY)
    z[49] = dfd['OGNGPLPP'].loc[8] + dfd['OGNGPLPP'].loc[9] + dfd['OGNGPLPP'].loc[10] + \
            dfd['OGNGPLPP'].loc[12] + dfd['OGNGPLPP'].loc[15] + dfd['OGNGPLPP'].loc[16] + \
            dfd['OGNGPLPP'].loc[19] + dfd['OGNGPLPP'].loc[22] + dfd['OGNGPLPP'].loc[23] + \
            dfd['OGNGPLPP'].loc[24] + dfd['OGNGPLPP'].loc[25] + dfd['OGNGPLPP'].loc[33] + \
            dfd['OGNGPLPP'].loc[34] + dfd['OGNGPLPP'].loc[37] + dfd['OGNGPLPP'].loc[38] + \
            dfd['OGNGPLPP'].loc[40] + dfd['OGNGPLPP'].loc[43] + dfd['OGNGPLPP'].loc[44] + \
            dfd['OGNGPLPP'].loc[45] + dfd['OGNGPLPP'].loc[47] + dfd['OGNGPLPP'].loc[61] + \
            dfd['OGNGPLPP'].loc[62] + dfd['OGNGPLPP'].loc[64] + dfd['OGNGPLPP'].loc[65]
 
    #      Gulf Coast                                               
    #T65(50,IY,IS)=OGNGPLPP(1,IY)+OGNGPLPP(2,IY)+OGNGPLPP(11,IY)+OGNGPLPP(20,IY)+OGNGPLPP(21,IY)+OGNGPLPP(27,IY)+OGNGPLPP(28,IY)+OGNGPLPP(48,IY)+OGNGPLPP(49,IY)+OGNGPLPP(50,IY)+OGNGPLPP(51,IY)+OGNGPLPP(53,IY)
    z[50] = dfd['OGNGPLPP'].loc[1] + dfd['OGNGPLPP'].loc[2] + dfd['OGNGPLPP'].loc[11] + \
            dfd['OGNGPLPP'].loc[20] + dfd['OGNGPLPP'].loc[21] + dfd['OGNGPLPP'].loc[27] + \
            dfd['OGNGPLPP'].loc[28] + dfd['OGNGPLPP'].loc[48] + dfd['OGNGPLPP'].loc[49] + \
            dfd['OGNGPLPP'].loc[50] + dfd['OGNGPLPP'].loc[51] + dfd['OGNGPLPP'].loc[53]
 
    #      Midcontinent                                             
    #T65(51,IY,IS)=OGNGPLPP(5,IY)+OGNGPLPP(17,IY)+OGNGPLPP(18,IY)+OGNGPLPP(26,IY)+OGNGPLPP(29,IY)+OGNGPLPP(31,IY)+OGNGPLPP(41,IY)+OGNGPLPP(59,IY)
    z[51] = dfd['OGNGPLPP'].loc[5] + dfd['OGNGPLPP'].loc[17] + dfd['OGNGPLPP'].loc[18] + \
            dfd['OGNGPLPP'].loc[26] + dfd['OGNGPLPP'].loc[29] + dfd['OGNGPLPP'].loc[31] + \
            dfd['OGNGPLPP'].loc[41] + dfd['OGNGPLPP'].loc[59]
 
    #      Southwest                                                
    #T65(52,IY,IS)=OGNGPLPP(35,IY)+OGNGPLPP(52,IY)+OGNGPLPP(54,IY)+OGNGPLPP(55,IY)+OGNGPLPP(56,IY)+OGNGPLPP(57,IY)+OGNGPLPP(58,IY)
    z[52] = dfd['OGNGPLPP'].loc[35] + dfd['OGNGPLPP'].loc[52] + dfd['OGNGPLPP'].loc[54] + \
            dfd['OGNGPLPP'].loc[55] + dfd['OGNGPLPP'].loc[56] + dfd['OGNGPLPP'].loc[57] + \
            dfd['OGNGPLPP'].loc[58]
 
    #      Rocky Mountain                                           
    #T65(53,IY,IS)=OGNGPLPP(4,IY)+OGNGPLPP(7,IY)+OGNGPLPP(14,IY)+OGNGPLPP(32,IY)+OGNGPLPP(36,IY)+OGNGPLPP(60,IY)+OGNGPLPP(66,IY)
    z[53] = dfd['OGNGPLPP'].loc[4] + dfd['OGNGPLPP'].loc[7] + dfd['OGNGPLPP'].loc[14] + \
            dfd['OGNGPLPP'].loc[32] + dfd['OGNGPLPP'].loc[36] + dfd['OGNGPLPP'].loc[60] + \
            dfd['OGNGPLPP'].loc[66]
 
    #      Northern Great Plains                                    
    #T65(54,IY,IS)=OGNGPLPP(30,IY)+OGNGPLPP(39,IY)+OGNGPLPP(46,IY)
    z[54] = dfd['OGNGPLPP'].loc[30] + dfd['OGNGPLPP'].loc[39] + dfd['OGNGPLPP'].loc[46]
 
    #      West Coast                                               
    #T65(55,IY,IS)=OGNGPLPP(6,IY)+OGNGPLPP(42,IY)+OGNGPLPP(63,IY)
    z[55] = dfd['OGNGPLPP'].loc[6] + dfd['OGNGPLPP'].loc[42] + dfd['OGNGPLPP'].loc[63]
 
    #      Atlantic                                                 
    #T65(56,IY,IS)=OGNGPLPP(67,IY)+OGNGPLPP(68,IY)+OGNGPLPP(69,IY)+OGNGPLPP(76,IY)+OGNGPLPP(77,IY)+OGNGPLPP(78,IY)
    z[56] = dfd['OGNGPLPP'].loc[67] + dfd['OGNGPLPP'].loc[68] + dfd['OGNGPLPP'].loc[69] + \
            dfd['OGNGPLPP'].loc[76] + dfd['OGNGPLPP'].loc[77] + dfd['OGNGPLPP'].loc[78]
 
    #      Pacific                                                  
    #T65(57,IY,IS)=OGNGPLPP(73,IY)+OGNGPLPP(74,IY)+OGNGPLPP(82,IY)+OGNGPLPP(83,IY)
    z[57] = dfd['OGNGPLPP'].loc[73] + dfd['OGNGPLPP'].loc[74] + dfd['OGNGPLPP'].loc[82] + \
            dfd['OGNGPLPP'].loc[83]
 
    #      Gulf of Mexico                                           
    #T65(58,IY,IS)=OGNGPLPP(70,IY)+OGNGPLPP(71,IY)+OGNGPLPP(72,IY)+OGNGPLPP(79,IY)+OGNGPLPP(80,IY)+OGNGPLPP(81,IY)
    z[58] = dfd['OGNGPLPP'].loc[70] + dfd['OGNGPLPP'].loc[71] + dfd['OGNGPLPP'].loc[72] + \
            dfd['OGNGPLPP'].loc[79] + dfd['OGNGPLPP'].loc[80] + dfd['OGNGPLPP'].loc[81]
 
    #      Alaska                                                   
    #T65(59,IY,IS)=OGNGPLPP(3,IY)+OGNGPLPP(75,IY)+OGNGPLPP(84,IY)
    z[59] = dfd['OGNGPLPP'].loc[3] + dfd['OGNGPLPP'].loc[75] + dfd['OGNGPLPP'].loc[84]
 
    #         Total                                                 
    #T65(60,IY,IS)=FSUM(T65(49,IY,IS),11)
    z[60] = z[49]+z[50]+z[51]+z[52]+z[53]+z[54]+z[55]+z[56]+z[57]+z[58]+z[59]

    #                                                               

    #   Total Natural Gas Plant Liquid Production                   

    #      East                                                     
    #T65(61,IY,IS)=OGNGPLPRD(8,IY)+OGNGPLPRD(9,IY)+OGNGPLPRD(10,IY)+OGNGPLPRD(12,IY)+OGNGPLPRD(15,IY)+OGNGPLPRD(16,IY)+OGNGPLPRD(19,IY)+OGNGPLPRD(22,IY)+OGNGPLPRD(23,IY)+OGNGPLPRD(24,IY)+OGNGPLPRD(25,IY)+OGNGPLPRD(33,IY)+OGNGPLPRD(34,IY)+OGNGPLPRD(37,IY)+OGNGPLPRD(38,IY)+OGNGPLPRD(40,IY)+OGNGPLPRD(43,IY)+OGNGPLPRD(44,IY)+OGNGPLPRD(45,IY)+OGNGPLPRD(47,IY)+OGNGPLPRD(61,IY)+OGNGPLPRD(62,IY)+OGNGPLPRD(64,IY)+OGNGPLPRD(65,IY)
    z[61] = dfd['OGNGPLPRD'].loc[8] + dfd['OGNGPLPRD'].loc[9] + dfd['OGNGPLPRD'].loc[10] + \
            dfd['OGNGPLPRD'].loc[12] + dfd['OGNGPLPRD'].loc[15] + dfd['OGNGPLPRD'].loc[16] + \
            dfd['OGNGPLPRD'].loc[19] + dfd['OGNGPLPRD'].loc[22] + dfd['OGNGPLPRD'].loc[23] + \
            dfd['OGNGPLPRD'].loc[24] + dfd['OGNGPLPRD'].loc[25] + dfd['OGNGPLPRD'].loc[33] + \
            dfd['OGNGPLPRD'].loc[34] + dfd['OGNGPLPRD'].loc[37] + dfd['OGNGPLPRD'].loc[38] + \
            dfd['OGNGPLPRD'].loc[40] + dfd['OGNGPLPRD'].loc[43] + dfd['OGNGPLPRD'].loc[44] + \
            dfd['OGNGPLPRD'].loc[45] + dfd['OGNGPLPRD'].loc[47] + dfd['OGNGPLPRD'].loc[61] + \
            dfd['OGNGPLPRD'].loc[62] + dfd['OGNGPLPRD'].loc[64] + dfd['OGNGPLPRD'].loc[65]
 
    #      Gulf Coast                                               
    #T65(62,IY,IS)=OGNGPLPRD(1,IY)+OGNGPLPRD(2,IY)+OGNGPLPRD(11,IY)+OGNGPLPRD(20,IY)+OGNGPLPRD(21,IY)+OGNGPLPRD(27,IY)+OGNGPLPRD(28,IY)+OGNGPLPRD(48,IY)+OGNGPLPRD(49,IY)+OGNGPLPRD(50,IY)+OGNGPLPRD(51,IY)+OGNGPLPRD(53,IY)
    z[62] = dfd['OGNGPLPRD'].loc[1] + dfd['OGNGPLPRD'].loc[2] + dfd['OGNGPLPRD'].loc[11] + \
            dfd['OGNGPLPRD'].loc[20] + dfd['OGNGPLPRD'].loc[21] + dfd['OGNGPLPRD'].loc[27] + \
            dfd['OGNGPLPRD'].loc[28] + dfd['OGNGPLPRD'].loc[48] + dfd['OGNGPLPRD'].loc[49] + \
            dfd['OGNGPLPRD'].loc[50] + dfd['OGNGPLPRD'].loc[51] + dfd['OGNGPLPRD'].loc[53]
 
    #      Midcontinent                                             
    #T65(63,IY,IS)=OGNGPLPRD(5,IY)+OGNGPLPRD(17,IY)+OGNGPLPRD(18,IY)+OGNGPLPRD(26,IY)+OGNGPLPRD(29,IY)+OGNGPLPRD(31,IY)+OGNGPLPRD(41,IY)+OGNGPLPRD(59,IY)
    z[63] = dfd['OGNGPLPRD'].loc[5] + dfd['OGNGPLPRD'].loc[17] + dfd['OGNGPLPRD'].loc[18] + \
            dfd['OGNGPLPRD'].loc[26] + dfd['OGNGPLPRD'].loc[29] + dfd['OGNGPLPRD'].loc[31] + \
            dfd['OGNGPLPRD'].loc[41] + dfd['OGNGPLPRD'].loc[59]
 
    #      Southwest                                                
    #T65(64,IY,IS)=OGNGPLPRD(35,IY)+OGNGPLPRD(52,IY)+OGNGPLPRD(54,IY)+OGNGPLPRD(55,IY)+OGNGPLPRD(56,IY)+OGNGPLPRD(57,IY)+OGNGPLPRD(58,IY)
    z[64] = dfd['OGNGPLPRD'].loc[35] + dfd['OGNGPLPRD'].loc[52] + dfd['OGNGPLPRD'].loc[54] + \
            dfd['OGNGPLPRD'].loc[55] + dfd['OGNGPLPRD'].loc[56] + dfd['OGNGPLPRD'].loc[57] + \
            dfd['OGNGPLPRD'].loc[58]
 
    #      Rocky Mountain                                           
    #T65(65,IY,IS)=OGNGPLPRD(4,IY)+OGNGPLPRD(7,IY)+OGNGPLPRD(14,IY)+OGNGPLPRD(32,IY)+OGNGPLPRD(36,IY)+OGNGPLPRD(60,IY)+OGNGPLPRD(66,IY)
    z[65] = dfd['OGNGPLPRD'].loc[4] + dfd['OGNGPLPRD'].loc[7] + dfd['OGNGPLPRD'].loc[14] + \
            dfd['OGNGPLPRD'].loc[32] + dfd['OGNGPLPRD'].loc[36] + dfd['OGNGPLPRD'].loc[60] + \
            dfd['OGNGPLPRD'].loc[66]
 
    #      Northern Great Plains                                    
    #T65(66,IY,IS)=OGNGPLPRD(30,IY)+OGNGPLPRD(39,IY)+OGNGPLPRD(46,IY)
    z[66] = dfd['OGNGPLPRD'].loc[30] + dfd['OGNGPLPRD'].loc[39] + dfd['OGNGPLPRD'].loc[46]
 
    #      West Coast                                               
    #T65(67,IY,IS)=OGNGPLPRD(6,IY)+OGNGPLPRD(42,IY)+OGNGPLPRD(63,IY)
    z[67] = dfd['OGNGPLPRD'].loc[6] + dfd['OGNGPLPRD'].loc[42] + dfd['OGNGPLPRD'].loc[63]
 
    #      Atlantic                                                 
    #T65(68,IY,IS)=OGNGPLPRD(67,IY)+OGNGPLPRD(68,IY)+OGNGPLPRD(69,IY)+OGNGPLPRD(76,IY)+OGNGPLPRD(77,IY)+OGNGPLPRD(78,IY)
    z[68] = dfd['OGNGPLPRD'].loc[67] + dfd['OGNGPLPRD'].loc[68] + dfd['OGNGPLPRD'].loc[69] + \
            dfd['OGNGPLPRD'].loc[76] + dfd['OGNGPLPRD'].loc[77] + dfd['OGNGPLPRD'].loc[78]
 
    #      Pacific                                                  
    #T65(69,IY,IS)=OGNGPLPRD(73,IY)+OGNGPLPRD(74,IY)+OGNGPLPRD(82,IY)+OGNGPLPRD(83,IY)
    z[69] = dfd['OGNGPLPRD'].loc[73] + dfd['OGNGPLPRD'].loc[74] + dfd['OGNGPLPRD'].loc[82] + \
            dfd['OGNGPLPRD'].loc[83]
 
    #      Gulf of Mexico                                           
    #T65(70,IY,IS)=OGNGPLPRD(70,IY)+OGNGPLPRD(71,IY)+OGNGPLPRD(72,IY)+OGNGPLPRD(79,IY)+OGNGPLPRD(80,IY)+OGNGPLPRD(81,IY)
    z[70] = dfd['OGNGPLPRD'].loc[70] + dfd['OGNGPLPRD'].loc[71] + dfd['OGNGPLPRD'].loc[72] + \
            dfd['OGNGPLPRD'].loc[79] + dfd['OGNGPLPRD'].loc[80] + dfd['OGNGPLPRD'].loc[81]
 
    #      Alaska                                                   
    #T65(71,IY,IS)=OGNGPLPRD(3,IY)+OGNGPLPRD(75,IY)+OGNGPLPRD(84,IY)
    z[71] = dfd['OGNGPLPRD'].loc[3] + dfd['OGNGPLPRD'].loc[75] + dfd['OGNGPLPRD'].loc[84]
 
    #         Total                                                 
    #T65(72,IY,IS)=FSUM(T65(61,IY,IS),11)
    z[72] = z[61]+z[62]+z[63]+z[64]+z[65]+z[66]+z[67]+z[68]+z[69]+z[70]+z[71]
 
    #                                                               
 
    #                                                               

    #   This section is a check for total:                          
  
    #   Check Total Natural Gas Plant Liquid Production             

    #      East                                                     
    #T65(73,IY,IS)=T65(1,IY,IS)+T65(13,IY,IS)+T65(25,IY,IS)+T65(37,IY,IS)+T65(49,IY,IS)
    z[73] = z[1]+z[13]+z[25]+z[37]+z[49]
 
    #      Gulf Coast                                               
    #T65(74,IY,IS)=T65(2,IY,IS)+T65(14,IY,IS)+T65(26,IY,IS)+T65(38,IY,IS)+T65(50,IY,IS)
    z[74] = z[2]+z[14]+z[26]+z[38]+z[50]
 
    #      Midcontinent                                             
    #T65(75,IY,IS)=T65(3,IY,IS)+T65(15,IY,IS)+T65(27,IY,IS)+T65(39,IY,IS)+T65(51,IY,IS)
    z[75] = z[3]+z[15]+z[27]+z[39]+z[51]
 
    #      Southwest                                                
    #T65(76,IY,IS)=T65(4,IY,IS)+T65(16,IY,IS)+T65(28,IY,IS)+T65(40,IY,IS)+T65(52,IY,IS)
    z[76] = z[4]+z[16]+z[28]+z[40]+z[52]
 
    #      Rocky Mountain                                           
    #T65(77,IY,IS)=T65(5,IY,IS)+T65(17,IY,IS)+T65(29,IY,IS)+T65(41,IY,IS)+T65(53,IY,IS)
    z[77] = z[5]+z[17]+z[29]+z[41]+z[53]
 
    #      Northern Great Plains                                    
    #T65(78,IY,IS)=T65(6,IY,IS)+T65(18,IY,IS)+T65(30,IY,IS)+T65(42,IY,IS)+T65(54,IY,IS)
    z[78] = z[6]+z[18]+z[30]+z[42]+z[54]
 
    #      West Coast                                               
    #T65(79,IY,IS)=T65(7,IY,IS)+T65(19,IY,IS)+T65(31,IY,IS)+T65(43,IY,IS)+T65(55,IY,IS)
    z[79] = z[7]+z[19]+z[31]+z[43]+z[55]
 
    #      Atlantic                                                 
    #T65(80,IY,IS)=T65(8,IY,IS)+T65(20,IY,IS)+T65(32,IY,IS)+T65(44,IY,IS)+T65(56,IY,IS)
    z[80] = z[8]+z[20]+z[32]+z[44]+z[56]
 
    #      Pacific                                                  
    #T65(81,IY,IS)=T65(9,IY,IS)+T65(21,IY,IS)+T65(33,IY,IS)+T65(45,IY,IS)+T65(57,IY,IS)
    z[81] = z[9]+z[21]+z[33]+z[45]+z[57]
 
    #      Gulf of Mexico                                           
    #T65(82,IY,IS)=T65(10,IY,IS)+T65(22,IY,IS)+T65(34,IY,IS)+T65(46,IY,IS)+T65(58,IY,IS)
    z[82] = z[10]+z[22]+z[34]+z[46]+z[58]
 
    #      Alaska                                                   
    #T65(83,IY,IS)=T65(11,IY,IS)+T65(23,IY,IS)+T65(35,IY,IS)+T65(47,IY,IS)+T65(59,IY,IS)
    z[83] = z[11]+z[23]+z[35]+z[47]+z[59]
 
    #         Total                                                 
    #T65(84,IY,IS)=FSUM(T65(73,IY,IS),11)
    z[84] = z[73]+z[74]+z[75]+z[76]+z[77]+z[78]+z[79]+z[80]+z[81]+z[82]+z[83]

    return z