 # -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_077(dfd, table_spec, table_id):
    """Fill table    Natural Gas Consumption by End-Use Sector and Census Division
   
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
   
    #   Natural Gas Consumption by End-Use Sector and Census Division
    #   (trillion cubic feet)
    #    Sector and Region                                          
    #                                                               
    #     New England                                               
    #T77(1:9,IY,IS)=QNGRS(1:9,IY)/CFNGN(IY)
    z[1] = dfd['QNGRS'].loc[1]/dfd['CFNGN']/1000
    #     Middle Atlantic                                           
    z[2] = dfd['QNGRS'].loc[2]/dfd['CFNGN']/1000
    #     East North Central                                        
    z[3] = dfd['QNGRS'].loc[3]/dfd['CFNGN']/1000
    #     West North Central                                        
    z[4] = dfd['QNGRS'].loc[4]/dfd['CFNGN']/1000
    #     South Atlantic                                            
    z[5] = dfd['QNGRS'].loc[5]/dfd['CFNGN']/1000
    #     East South Central                                        
    z[6] = dfd['QNGRS'].loc[6]/dfd['CFNGN']/1000
    #     West South Central                                        
    z[7] = dfd['QNGRS'].loc[7]/dfd['CFNGN']/1000
    #     Mountain                                                  
    z[8] = dfd['QNGRS'].loc[8]/dfd['CFNGN']/1000
    #     Pacific                                                   
    z[9] = dfd['QNGRS'].loc[9]/dfd['CFNGN']/1000
    #   Residential                                                 
    #T77(10,IY,IS)=FSUM(T77(1,IY,IS),9)
    z[10] = z[1]+z[2]+ z[3]+z[4]+ z[5]+z[6]+ z[7]+z[8]+z[9]
    #                                                               
    #     New England                                               
    #T77(11:19,IY,IS)=QNGCM(1:9,IY)/CFNGN(IY)
    z[11] = dfd['QNGCM'].loc[1]/dfd['CFNGN']/1000
    #     Middle Atlantic                                           
    z[12] = dfd['QNGCM'].loc[2]/dfd['CFNGN']/1000
    #     East North Central                                        
    z[13] = dfd['QNGCM'].loc[3]/dfd['CFNGN']/1000
    #     West North Central                                        
    z[14] = dfd['QNGCM'].loc[4]/dfd['CFNGN']/1000
    #     South Atlantic                                            
    z[15] = dfd['QNGCM'].loc[5]/dfd['CFNGN']/1000
    #     East South Central                                        
    z[16] = dfd['QNGCM'].loc[6]/dfd['CFNGN']/1000
    #     West South Central                                        
    z[17] = dfd['QNGCM'].loc[7]/dfd['CFNGN']/1000
    #     Mountain                                                  
    z[18] = dfd['QNGCM'].loc[8]/dfd['CFNGN']/1000
    #     Pacific                                                   
    z[19] = dfd['QNGCM'].loc[9]/dfd['CFNGN']/1000
    #   Commercial                                                  
    #T77(20,IY,IS)=FSUM(T77(11,IY,IS),9)
    z[20] = z[11]+z[12]+ z[13]+z[14]+ z[15]+z[16]+ z[17]+z[18]+z[19]
    #     New England                                               
    #T77(21:29,IY,IS)=(QNGIN(1:9,IY)-QGTLRF(1:9,IY))/CFNGN(IY)
    z[21] = ((dfd['QNGIN'].loc[1]-dfd['QGTLRF'].loc[1])/dfd['CFNGN'])/1000
    #     Middle Atlantic                                           
    z[22] = ((dfd['QNGIN'].loc[2]-dfd['QGTLRF'].loc[2])/dfd['CFNGN'])/1000
    #     East North Central                                        
    z[23] = ((dfd['QNGIN'].loc[3]-dfd['QGTLRF'].loc[3])/dfd['CFNGN'])/1000
    #     West North Central                                        
    z[24] = ((dfd['QNGIN'].loc[4]-dfd['QGTLRF'].loc[4])/dfd['CFNGN'])/1000
    #     South Atlantic                                            
    z[25] = ((dfd['QNGIN'].loc[5]-dfd['QGTLRF'].loc[5])/dfd['CFNGN'])/1000
    #     East South Central                                        
    z[26] = ((dfd['QNGIN'].loc[6]-dfd['QGTLRF'].loc[6])/dfd['CFNGN'])/1000
    #     West South Central                                        
    z[27] = ((dfd['QNGIN'].loc[7]-dfd['QGTLRF'].loc[7])/dfd['CFNGN'])/1000
    #     Mountain                                                  
    z[28] = ((dfd['QNGIN'].loc[8]-dfd['QGTLRF'].loc[8])/dfd['CFNGN'])/1000
    #     Pacific                                                   
    z[29] = ((dfd['QNGIN'].loc[9]-dfd['QGTLRF'].loc[9])/dfd['CFNGN'])/1000
    #                                                               
    #   Industrial 1/                                               
    #T77(30,IY,IS)=FSUM(T77(21,IY,IS),9)
    z[30] = z[21]+z[22]+ z[23]+z[24]+ z[25]+z[26]+ z[27]+z[28]+z[29]
    #                                                               
    #     New England                                               
    #T77(31:39,IY,IS)=QNGEL(1:9,IY)/CFNGU(IY)
    z[31] = dfd['QNGEL'].loc[1]/dfd['CFNGU']/1000
    #     Middle Atlantic                                           
    z[32] = dfd['QNGEL'].loc[2]/dfd['CFNGU']/1000
    #     East North Central                                        
    z[33] = dfd['QNGEL'].loc[3]/dfd['CFNGU']/1000
    #     West North Central                                        
    z[34] = dfd['QNGEL'].loc[4]/dfd['CFNGU']/1000
    #     South Atlantic                                            
    z[35] = dfd['QNGEL'].loc[5]/dfd['CFNGU']/1000
    #     East South Central                                        
    z[36] = dfd['QNGEL'].loc[6]/dfd['CFNGU']/1000
    #     West South Central                                        
    z[37] = dfd['QNGEL'].loc[7]/dfd['CFNGU']/1000
    #     Mountain                                                  
    z[38] = dfd['QNGEL'].loc[8]/dfd['CFNGU']/1000
    #     Pacific                                                   
    z[39] = dfd['QNGEL'].loc[9]/dfd['CFNGU']/1000
    #   Electric Power 2/                                           
    #T77(40,IY,IS)=FSUM(T77(31,IY,IS),9)
    z[40] = z[31]+z[32]+ z[33]+z[34]+ z[35]+z[36]+ z[37]+z[38]+z[39]
    #     New England                                               
    #T77(41:49,IY,IS)=QNGTR(1:9,IY)/CFNGN(IY)
    z[41] = dfd['QNGTR'].loc[1]/dfd['CFNGN']/1000
    #     Middle Atlantic                                           
    z[42] = dfd['QNGTR'].loc[2]/dfd['CFNGN']/1000
    #     East North Central                                        
    z[43] = dfd['QNGTR'].loc[3]/dfd['CFNGN']/1000
    #     West North Central                                        
    z[44] = dfd['QNGTR'].loc[4]/dfd['CFNGN']/1000
    #     South Atlantic                                            
    z[45] = dfd['QNGTR'].loc[5]/dfd['CFNGN']/1000
    #     East South Central                                        
    z[46] = dfd['QNGTR'].loc[6]/dfd['CFNGN']/1000
    #     West South Central                                        
    z[47] = dfd['QNGTR'].loc[7]/dfd['CFNGN']/1000
    #     Mountain                                                  
    z[48] = dfd['QNGTR'].loc[8]/dfd['CFNGN']/1000
    #     Pacific                                                   
    z[49] = dfd['QNGTR'].loc[9]/dfd['CFNGN']/1000
    #   Transportation 3/                                           
    z[50] = z[41]+z[42]+ z[43]+z[44]+ z[45]+z[46]+ z[47]+z[48]+z[49]
    #                                                               
    #     New England                                               
    #T77(50,IY,IS)=FSUM(T77(41,IY,IS),9)
    z[51] = z[1]+z[11]+z[21]+z[31]+z[41]
    #     Middle Atlantic                                           
    z[52] = z[2]+z[12]+z[22]+z[32]+z[42]
    #     East North Central                                        
    z[53] = z[3]+z[13]+z[23]+z[33]+z[43]
    #     West North Central                                        
    z[54] = z[4]+z[14]+z[24]+z[34]+z[44]
    #     South Atlantic                                            
    z[55] = z[5]+z[15]+z[25]+z[35]+z[45]
    #     East South Central                                        
    z[56] = z[6]+z[16]+z[26]+z[36]+z[46]
    #     West South Central                                        
    z[57] = z[7]+z[17]+z[27]+z[37]+z[47]
    #     Mountain                                                  
    z[58] = z[8]+z[18]+z[28]+z[38]+z[48]
    #     Pacific                                                   
    z[59] = z[9]+z[19]+z[29]+z[39]+z[49]
    #   All Sectors 4/                                              
    #T77(60,IY,IS)=FSUM(T77(51,IY,IS),9)
    z[60] = z[51]+z[52]+ z[53]+z[54]+ z[55]+z[56]+ z[57]+z[58]+z[59]
    return z    