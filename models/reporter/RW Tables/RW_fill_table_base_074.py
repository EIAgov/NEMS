#-*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_074(dfd, table_spec, table_id):
    """Fill table Successful Onshore Wells by OGSM District                   
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


    GASTYPES =  dfd['GASTYPES_rwpre']    
    OILTYPES = dfd['OILTYPES_rwpre']
    #OGDIST= dfd['OGDIST_rwpre']
    MNL48T = dfd['MNL48T_rwpre']

    #   Successful Onshore Wells by OGSM District                   

    #      Alabama, North  
    z[1] = dfd['OGOGWELLS'].loc[1,1:6,:].sum()    
    #      Alabama, South                  
    z[2] = dfd['OGOGWELLS'].loc[2,1:6,:].sum()    
    #      Alaska                          
    z[3] = dfd['OGOGWELLS'].loc[3,1:6,:].sum()    
    #      Arizona                                                  
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[4] = dfd['OGOGWELLS'].loc[4,1:6,:].sum()
    #      Arkansas                                                 
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[5] = dfd['OGOGWELLS'].loc[5,1:6,:].sum()
 
    #      California                                               
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[6] = dfd['OGOGWELLS'].loc[6,1:6,:].sum()
 
    #      Colorado                                                 
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[7] = dfd['OGOGWELLS'].loc[7,1:6,:].sum()
 
    #      Connecticut                                              
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[8] = dfd['OGOGWELLS'].loc[8,1:6,:].sum()
 
    #      Delaware                                                 
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[9] = dfd['OGOGWELLS'].loc[9,1:6,:].sum()
 
    #      Washington, D.C.                                         
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[10] = dfd['OGOGWELLS'].loc[10,1:6,:].sum()
 
    #      Florida                                                  
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[11] = dfd['OGOGWELLS'].loc[11,1:6,:].sum()
 
    #      Georgia                                                  
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[12] = dfd['OGOGWELLS'].loc[12,1:6,:].sum()
 
    #      Hawaii                                                   
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[13] = dfd['OGOGWELLS'].loc[13,1:6,:].sum()
 
    #      Idaho                                                    
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[14] = dfd['OGOGWELLS'].loc[14,1:6,:].sum()
 
    #      Illinois                                                 
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[15] = dfd['OGOGWELLS'].loc[15,1:6,:].sum()
 
    #      Indiana                                                  
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[16] = dfd['OGOGWELLS'].loc[16,1:6,:].sum()
 
    #      Iowa                                                     
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[17] = dfd['OGOGWELLS'].loc[17,1:6,:].sum()
 
    #      Kansas                                                   
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[18] = dfd['OGOGWELLS'].loc[18,1:6,:].sum()
 
    #      Kentucky                                                 
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[19] = dfd['OGOGWELLS'].loc[19,1:6,:].sum()
 
    #      Louisiana, North                                         
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[20] = dfd['OGOGWELLS'].loc[20,1:6,:].sum()
 
    #      Louisiana, South                                         
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[21] = dfd['OGOGWELLS'].loc[21,1:6,:].sum()
 
    #      Maine                                                    
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[22] = dfd['OGOGWELLS'].loc[22,1:6,:].sum()
 
    #      Maryland                                                 
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[23] = dfd['OGOGWELLS'].loc[23,1:6,:].sum()
 
    #      Massachusetts                                            
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[24] = dfd['OGOGWELLS'].loc[24,1:6,:].sum()
 
    #      Michigan                                                 
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[25] = dfd['OGOGWELLS'].loc[25,1:6,:].sum()
 
    #      Minnesota                                                
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[26] = dfd['OGOGWELLS'].loc[26,1:6,:].sum()
 
    #      Mississippi, North                                       
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[27] = dfd['OGOGWELLS'].loc[27,1:6,:].sum()
 
    #      Mississippi, South                                       
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[28] = dfd['OGOGWELLS'].loc[28,1:6,:].sum()
 
    #      Missouri                                                 
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[29] = dfd['OGOGWELLS'].loc[29,1:6,:].sum()
 
    #      Montana                                                  
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[30] = dfd['OGOGWELLS'].loc[30,1:6,:].sum()
 
    #      Nebraska                                                 
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[31] = dfd['OGOGWELLS'].loc[31,1:6,:].sum()
 
    #      Nevada                                                   
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[32] = dfd['OGOGWELLS'].loc[32,1:6,:].sum()
 
    #      New Hampshire                                            
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[33] = dfd['OGOGWELLS'].loc[33,1:6,:].sum()
 
    #      New Jersey                                               
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[34] = dfd['OGOGWELLS'].loc[34,1:6,:].sum()
 
    #      New Mexico, East                                         
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[35] = dfd['OGOGWELLS'].loc[35,1:6,:].sum()
 
    #      New Mexico, West                                         
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[36] = dfd['OGOGWELLS'].loc[36,1:6,:].sum()
 
    #      New York                                                 
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[37] = dfd['OGOGWELLS'].loc[37,1:6,:].sum()
 
    #      North Carolina                                           
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[38] = dfd['OGOGWELLS'].loc[38,1:6,:].sum()
 
    #      North Dakota                                             
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[39] = dfd['OGOGWELLS'].loc[39,1:6,:].sum()
 
    #      Ohio                                                     
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[40] = dfd['OGOGWELLS'].loc[40,1:6,:].sum()
 
    #      Oklahoma                                                 
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[41] = dfd['OGOGWELLS'].loc[41,1:6,:].sum()
 
    #      Oregon                                                   
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[42] = dfd['OGOGWELLS'].loc[42,1:6,:].sum()
 
    #      Pennsylvania                                             
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[43] = dfd['OGOGWELLS'].loc[43,1:6,:].sum()
 
    #      Rhode Island                                             
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[44] = dfd['OGOGWELLS'].loc[44,1:6,:].sum()
 
    #      South Carolina                                           
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[45] = dfd['OGOGWELLS'].loc[45,1:6,:].sum()
 
    #      South Dakota                                             
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[46] = dfd['OGOGWELLS'].loc[46,1:6,:].sum()
 
    #      Tennessee                                                
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[47] = dfd['OGOGWELLS'].loc[47,1:6,:].sum()
 
    #      Texas RRC 1                                              
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[48] = dfd['OGOGWELLS'].loc[48,1:6,:].sum()
 
    #      Texas RRC 2                                              
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[49] = dfd['OGOGWELLS'].loc[49,1:6,:].sum()
 
    #      Texas RRC 3                                              
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[50] = dfd['OGOGWELLS'].loc[50,1:6,:].sum()
 
    #      Texas RRC 4                                              
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[51] = dfd['OGOGWELLS'].loc[51,1:6,:].sum()
 
    #      Texas RRC 5                                              
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[52] = dfd['OGOGWELLS'].loc[52,1:6,:].sum()
 
    #      Texas RRC 6                                              
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[53] = dfd['OGOGWELLS'].loc[53,1:6,:].sum()
 
    #      Texas RRC 7B                                             
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[54] = dfd['OGOGWELLS'].loc[54,1:6,:].sum()
 
    #      Texas RRC 7C                                             
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[55] = dfd['OGOGWELLS'].loc[55,1:6,:].sum()
 
    #      Texas RRC 8                                              
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[56] = dfd['OGOGWELLS'].loc[56,1:6,:].sum()
 
    #      Texas RRC 8A                                             
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[57] = dfd['OGOGWELLS'].loc[57,1:6,:].sum()
 
    #      Texas RRC 9                                              
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[58] = dfd['OGOGWELLS'].loc[58,1:6,:].sum()
 
    #      Texas RRC 10                                             
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[59] = dfd['OGOGWELLS'].loc[59,1:6,:].sum()
 
    #      Utah                                                     
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[60] = dfd['OGOGWELLS'].loc[60,1:6,:].sum()
 
    #      Vermont                                                  
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[61] = dfd['OGOGWELLS'].loc[61,1:6,:].sum()
 
    #      Virginia                                                 
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[62] = dfd['OGOGWELLS'].loc[62,1:6,:].sum()
 
    #      Washington                                               
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[63] = dfd['OGOGWELLS'].loc[63,1:6,:].sum()
 
    #      West Virginia                                            
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[64] = dfd['OGOGWELLS'].loc[64,1:6,:].sum()
 
    #      Wisconsin                                                
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[65] = dfd['OGOGWELLS'].loc[65,1:6,:].sum()
 
    #      Wyoming                                                  
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[66] = dfd['OGOGWELLS'].loc[66,1:6,:].sum()
 
    #      North Atlantic State Offshore                            
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[67] = dfd['OGOGWELLS'].loc[67,1:6,:].sum()
 
    #      Mid Atlantic State Offshore                              
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[68] = dfd['OGOGWELLS'].loc[68,1:6,:].sum()
 
    #      South Atlantic State Offshore                            
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[69] = dfd['OGOGWELLS'].loc[69,1:6,:].sum()
 
    #      Alabama State Offshore                                   
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[70] = dfd['OGOGWELLS'].loc[70,1:6,:].sum()
 
    #      Louisiana State Offshore                                 
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[71] = dfd['OGOGWELLS'].loc[71,1:6,:].sum()
 
    #      Texas State Offshore                                     
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[72] = dfd['OGOGWELLS'].loc[72,1:6,:].sum()
 
    #      California State Offshore                                
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[73] = dfd['OGOGWELLS'].loc[73,1:6,:].sum()
 
    #      Northern Pacific State Offshore                          
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[74] = dfd['OGOGWELLS'].loc[74,1:6,:].sum()
 
    #      Alaska State Offshore                                    
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[75] = dfd['OGOGWELLS'].loc[75,1:6,:].sum()
 
    #      North Atlantic Federal Offshore                          
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[76] = dfd['OGOGWELLS'].loc[76,1:6,:].sum()
 
    #      Mid Atlantic Federal Offshore                            
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[77] = dfd['OGOGWELLS'].loc[77,1:6,:].sum()
 
    #      South Atlantic Federal Offshore                          
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[78] = dfd['OGOGWELLS'].loc[78,1:6,:].sum()
 
    #      Eastern GOM Federal Offshore                             
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[79] = dfd['OGOGWELLS'].loc[79,1:6,:].sum()
 
    #      Central GOM Federal Offshore                             
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[80] = dfd['OGOGWELLS'].loc[80,1:6,:].sum()
 
    #      Western GOM Federal Offshore                             
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[81] = dfd['OGOGWELLS'].loc[81,1:6,:].sum()
 
    #      California Federal Offshore                              
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[82] = dfd['OGOGWELLS'].loc[82,1:6,:].sum()
 
    #      Northern Pacific Federal Offshore                        
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[83] = dfd['OGOGWELLS'].loc[83,1:6,:].sum()
 
    #      Alaska Federal Offshore                                  
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[84] = dfd['OGOGWELLS'].loc[84,1:6,:].sum()
 
    #         Total                                                 
    #T74(OGDIST+1,IY,IS)=FSUM(T74(1,IY,IS),66)-T74(3,IY,IS)-T74(13,IY,IS)
    z[85] = z[1]+z[2]+z[3]+z[4]+z[5]+z[6]+z[7]+z[8]+z[9]+z[10]+z[11]+z[12]+z[13]+z[14]+z[15]+ \
            z[16]+z[17]+z[18]+z[19]+z[20]+z[21]+z[22]+z[23]+z[24]+z[25]+z[26]+z[27]+z[28]+z[29]+z[30]+ \
            z[31]+z[32]+z[33]+z[34]+z[35]+z[36]+z[37]+z[38]+z[39]+z[40]+z[41]+z[42]+z[43]+z[44]+z[45]+ \
            z[46]+z[47]+z[48]+z[49]+z[50]+z[51]+z[52]+z[53]+z[54]+z[55]+z[56]+z[57]+z[58]+z[59]+z[60]+ \
            z[61]+z[62]+z[63]+z[64]+z[65]+z[66] -z[3] -z[13]
 
    #                                                               
     #   Successful Offshore Wells                                   
    #T74(OGDIST+2,IY,IS)=FSUM(T74(67,IY,IS),18)-T74(75,IY,IS)-T74(84,IY,IS)
    z[86] = z[67]+z[68]+z[69]+z[70]+z[71]+z[72]+z[73]+z[74]+z[75]+z[76]+z[77]+z[78]+z[79]+z[80]+ \
            z[81]+z[82]+z[83]+z[84] -z[75] -z[84]
    #      Onshore                                                  
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[3] = dfd['OGOGWELLS'].loc[3,1:6,:].sum()
 
    #      Offshore                                                 
    #T74(OGDIST+3,IY,IS)=T74(75,IY,IS)+T74(84,IY,IS)
    z[87] = z[75]+z[84]
    
    #   Alaska                                                      
    #T74(OGDIST+4,IY,IS)=T74(13,IY,IS)+T74(OGDIST+3,IY,IS)
    z[88] = z[13]+z[87]
 
    #   Hawaii                                                      
    #T74(1:OGDIST,IY,IS)=SUM(OGOGWELLS(1:OGDIST,(1:6),IY))
    z[13] = dfd['OGOGWELLS'].loc[13,1:6,:].sum()
 
    #                                                               

    #   Total Successful Wells                                      
    #T74(OGDIST+5,IY,IS)=FSUM(T74(1,IY,IS),84)
    z[89] = z[1]+z[2]+z[3]+z[4]+z[5]+z[6]+z[7]+z[8]+z[9]+z[10]+z[11]+z[12]+z[13]+z[14]+z[15]+ \
            z[16]+z[17]+z[18]+z[19]+z[20]+z[21]+z[22]+z[23]+z[24]+z[25]+z[26]+z[27]+z[28]+z[29]+ \
            z[30]+z[31]+z[32]+z[33]+z[34]+z[35]+z[36]+z[37]+z[38]+z[39]+z[40]+z[41]+z[42]+z[43]+ \
            z[44]+z[45]+z[46]+z[47]+z[48]+z[49]+z[50]+z[51]+z[52]+z[53]+z[54]+z[55]+z[56]+z[57]+ \
            z[58]+z[59]+z[60]+z[61]+z[62]+z[63]+z[64]+z[65]+z[66]+z[67]+z[68]+z[69]+z[70]+z[71]+ \
            z[72]+z[73]+z[74]+z[75]+z[76]+z[77]+z[78]+z[79]+z[80]+z[81]+z[82]+z[83]+z[84]

    #                                                               

    #   Lower 48 Wells Drilled (thousands)                          

    #   Total Lower 48 Drilling, All Formations                     
    #T74(OGDIST+6,IY,IS)=(EXFTAGE(4,IY)+DVFTAGE(4,IY))/1000.
    z[90] = (dfd['EXFTAGE'].loc[4] + dfd['DVFTAGE'].loc[4]) / 1000.
 
    #      Oil Formations                                           
    #T74(OGDIST+7,IY,IS)=(EXOILFT(4,IY)+DVOILFT(4,IY))/1000.
    z[91] = (dfd['EXOILFT'].loc[4] + dfd['DVOILFT'].loc[4]) / 1000.
 
    #      Gas Formations                                           
    #T74(OGDIST+8,IY,IS)=(EXGASFT(4,IY)+DVGASFT(4,IY))/1000.
    z[92] = (dfd['EXGASFT'].loc[4] + dfd['DVGASFT'].loc[4]) / 1000.
 
    #      Dry Holes                                                
    #T74(OGDIST+9,IY,IS)=(EXDRYFT(4,IY)+DVDRYFT(4,IY))/1000.
    z[93] = (dfd['EXDRYFT'].loc[4] + dfd['DVDRYFT'].loc[4]) / 1000.
    
    return z
 

