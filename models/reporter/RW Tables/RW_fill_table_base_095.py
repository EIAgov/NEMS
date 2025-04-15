# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_095(dfd, table_spec, table_id):
    """Fill table  Coal Production and Minemouth Prices by Region
   
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

    MTOECONV=dfd['MTOECONV_rwpre']
    NSREG = dfd['NSREG_rwpre']
    MNUMCR = dfd['MNUMCR_rwpre']

    #   Coal Production by Region and Type
    #   (million short tons)
    #    Supply Regions and Coal Types                              

    #   Northern Appalachia 1/ 2/                                   
    #     Medium Sulfur (Premium)                                   
    #T95(2,IY,IS)=CLSULF(1,4,2,IY)
    # AEO2025 Change
    #   Northern Appalachia 1/ 2/                                   
    #     All Sulfur Categories (Premium)  
    #T95(2,IY,IS)=CLSULF(1,4,2,IY)
    #T95(2,IY,IS)=CLSULF(1,4,1,IY) AEO2025 Change
    z[2] = dfd['CLSULF'].loc[1].loc[4].loc[1]
 
    #     Low Sulfur (Bituminous)
    # All Sulfur Categories (Bituminous) AEO2025 Change                                
    #T95(3,IY,IS)=CLSULF(1,1,1,IY)
    z[3] = dfd['CLSULF'].loc[1].loc[1].loc[1]
 
    #     High Sulfur (Bituminous)                                  
    #T95(5,IY,IS)=CLSULF(1,1,3,IY)
    #z[5] = dfd['CLSULF'].loc[1].loc[1].loc[3]
    z[1] = z[2]+z[3]

    #     All Sulfur Categories (Premium) 4/                                
    #T95(8,IY,IS)=CLSULF(2,4,2,IY)
    ##T95(8,IY,IS)=CLSULF(2,4,1,IY)
    z[8] = dfd['CLSULF'].loc[2].loc[4].loc[1]
 
    #     All Sulfur Categories (Bituminous)                                   
    #T95(9,IY,IS)=CLSULF(2,1,1,IY)
    z[9] = dfd['CLSULF'].loc[2].loc[1].loc[1]
   
    #   Rest of Appalachia 3/                                       
    #T95(7,IY,IS)=FSUM(T95(8,IY,IS),3)
    z[7] = z[8]+z[9]
    
 
    #  Interior 5/                                
    #T95(17,IY,IS)=CLSULF(4,1,2,IY)
    #T95(15,IY,IS)=CLSULF(3,1,1,IY) AEO2025 Change
    z[15] = dfd['CLSULF'].loc[3].loc[1].loc[1]
 
    #     All Sulfur Categories (Bituminous) AEO2025 Change                                  
    #T95(18,IY,IS)=CLSULF(4,1,3,IY)
    #T95(17,IY,IS)=CLSULF(3,1,1,IY) AEO2025 Change
    z[17] = dfd['CLSULF'].loc[3].loc[1].loc[1]
  
    #   All Lignite AEO2025 Change
    z[20] = dfd['CLSULF'].loc[4].loc[3].loc[1] 
    #  All Sulfur Categories (Lignite)  AEO2025 Change                              
    #T95(20,IY,IS)=CLSULF(4,3,1,IY)
    z[21] = dfd['CLSULF'].loc[4].loc[3].loc[1]
 
     #     Wyoming, Powder River Basin 6/ AEO2025 Change              
    #T95(28,IY,IS)=FSUM(T95(29,IY,IS),2)
    #T95(28,IY,IS)=CFLSULF(5,2,1,IY) AEO2025 Change
    z[28] = dfd['CLSULF'].loc[5].loc[2].loc[1]

    #  All Sulfur Categories (Subbituminous) AEO2025 Change                                 
    #T95(29,IY,IS)=CLSULF(9,2,1,IY)+CLSULF(10,2,1,IY)
    z[29] = dfd['CLSULF'].loc[5].loc[2].loc[1]
 
    #      All Sulfur Categories (Bituminous)  AEO2025 Change                                 
    #T95(37,IY,IS)=CLSULF(6,1,1,IY)
    z[37] = dfd['CLSULF'].loc[6].loc[1].loc[1]
 
    #     All Sulfur Categories (Subbituminous) AEO2025 Change                          
    #T95(38,IY,IS)=CLSULF(6,2,1,IY)
    z[38] = dfd['CLSULF'].loc[6].loc[2].loc[1]

    #   Western Region 7/                                           
    #T95(36,IY,IS)=FSUM(T95(37,IY,IS),2) AEO2025 Change
    z[36] = z[37]+z[38]
    
    #   Subtotals:  All Regions       
    #     Premium Metallurgical 4/                                  
    #T95(44,IY,IS)=APSULF(4,IY)
    z[44] = dfd['APSULF'].loc[4]
 
    #     Bituminous                                                
    #T95(45,IY,IS)=ABSULF(4,IY)+IBSULF(4,IY)+WBSULF(4,IY)
    z[45] = dfd['ABSULF'].loc[4] + dfd['IBSULF'].loc[4] + dfd['WBSULF'].loc[4]
 
    #     Subbituminous                                             
    #T95(46,IY,IS)=WSSULF(4,IY)
    z[46] = dfd['WSSULF'].loc[4]
 
    #     Lignite                                                   
    #T95(47,IY,IS)=ILSULF(4,IY)+WLSULF(4,IY)
    z[47] = dfd['ILSULF'].loc[4] + dfd['WLSULF'].loc[4]
 
    #     Low Sulfur                                                
    #T95(48,IY,IS)=ABSULF(1,IY)+APSULF(1,IY)+IBSULF(1,IY)+ILSULF(1,IY)+WBSULF(1,IY)+WSSULF(1,IY)+WLSULF(1,IY)+WPSULF(1,IY)
    
    z[48] = dfd['ABSULF'].loc[1] + dfd['APSULF'].loc[1] + dfd['IBSULF'].loc[1] + dfd['ILSULF'].loc[1] + dfd['WBSULF'].loc[1] + dfd['WSSULF'].loc[1] + dfd['WLSULF'].loc[1] + dfd['WPSULF'].loc[1]
 
     #     Underground                                               
    #T95(51,IY,IS)=SUM(PMTD(1:NSREG,IY))
    z[51] = dfd['PMTD'].loc[1:NSREG].sum()   
 
    #     Surface                                                   
    #T95(52,IY,IS)=SUM(PMTS(1:NSREG,IY))
    z[52] = dfd['PMTS'].loc[1:NSREG].sum()
 
    #   United States Total 14/                                     
    #T95(53,IY,IS)=ABSULF(4,IY)+APSULF(4,IY)+IBSULF(4,IY)+ILSULF(4,IY)+WBSULF(4,IY)+WSSULF(4,IY)+WLSULF(4,IY)+WPSULF(4,IY)+APPSTOCKS(IY)+INTSTOCKS(IY)+WESTSTOCKS(IY)
    z[53] = dfd['ABSULF'].loc[4] + dfd['APSULF'].loc[4] + dfd['IBSULF'].loc[4] + dfd['ILSULF'].loc[4] + dfd['WBSULF'].loc[4] + dfd['WSSULF'].loc[4] + dfd['WLSULF'].loc[4] + dfd['WPSULF'].loc[4] + dfd['APPSTOCKS'] + dfd['INTSTOCKS'] + dfd['WESTSTOCKS']
 
    
    #   Waste Coal                                                  
    #T95(6,IY,IS)=CLSULF(1,3,1,IY)
    z[6] = dfd['CLSULF'].loc[1].loc[3].loc[1]
 
    #   for IEA submission in Mtoes  
    #     Premium Metallurgical 4/                                  
    #T95(55,IY,IS)=(APSULF_BTU(4,IY)+WPSULF_BTU(4,IY))*25.2/1000.
    z[55] = (dfd['APSULF_BTU'].loc[4] + dfd['WPSULF_BTU'].loc[4])*MTOECONV/1000.
 
    #     Bituminous                                                
    #T95(56,IY,IS)=(ABSULF_BTU(4,IY)+IBSULF_BTU(4,IY)+WBSULF_BTU(4,IY))*25.2/1000.
    z[56] = (dfd['ABSULF_BTU'].loc[4] + dfd['IBSULF_BTU'].loc[4] + dfd['WBSULF_BTU'].loc[4])*MTOECONV/1000.
 
    #     Subbituminous                                             
    #T95(57,IY,IS)=(WSSULF_BTU(4,IY))*25.2/1000.
    z[57] = (dfd['WSSULF_BTU'].loc[4])*MTOECONV/1000.
 
    #     Lignite (includes gob)                                    
    #T95(58,IY,IS)=(ILSULF_BTU(4,IY)+WLSULF_BTU(4,IY)+ALSULF_BTU(4,IY))*25.2/1000.
    z[58] = (dfd['ILSULF_BTU'].loc[4] + dfd['WLSULF_BTU'].loc[4] + dfd['ALSULF_BTU'].loc[4])*MTOECONV/1000.
   #                                                               
 
    #     Imports                                                   
 
    #     Premium Metallurgical 4/                                  
    #T95(59,IY,IS)=CLIMPRANK(4,IY)*25.2/1000.
    z[59] = dfd['CLIMPRANK'].loc[4] *MTOECONV/1000.
 
    #     Bituminous                                                
    #T95(60,IY,IS)=CLIMPRANK(1,IY)*25.2/1000.
    z[60] = dfd['CLIMPRANK'].loc[1] *MTOECONV/1000.
 
    #     Subbituminous                                             
    #T95(61,IY,IS)=CLIMPRANK(2,IY)*25.2/1000.
    z[61] = dfd['CLIMPRANK'].loc[2] *MTOECONV/1000.
 
    #     Lignite (includes gob)                                    
    #T95(62,IY,IS)=CLIMPRANK(3,IY)*25.2/1000.
    z[62] = dfd['CLIMPRANK'].loc[3] *MTOECONV/1000.
 
    #     Coal Products (coal coke)                                 
    #T95(63,IY,IS)=QCIIN(MNUMCR,IY)*25.2
    #### conditional in code to set to zero if negative
    z[63] = dfd['QCIIN'].loc[MNUMCR].copy()
    z[63] = z[63].where(z[63] >= 0, 0) #keep values >= to zero and replaces others with zero
    #z[63] = z[63] * dfd['QCIIN'].loc[MNUMCR] *MTOECONV/1000
    z[63] = z[63] * MTOECONV / 1000
                                                 
 
    #     Premium Metallurgical 4/                                  
    #T95(64,IY,IS)=CLEXPRANK(4,IY)*25.2/1000.
    z[64] = dfd['CLEXPRANK'].loc[4] *MTOECONV/1000.
 
    #     Bituminous                                                
    #T95(65,IY,IS)=CLEXPRANK(1,IY)*25.2/1000.
    z[65] = dfd['CLEXPRANK'].loc[1] *MTOECONV/1000.
 
    #     Subbituminous                                             
    #T95(66,IY,IS)=CLEXPRANK(2,IY)*25.2/1000.
    z[66] = dfd['CLEXPRANK'].loc[2] *MTOECONV/1000.
 
    #     Lignite (includes gob)                                    
    #T95(67,IY,IS)=CLEXPRANK(3,IY)*25.2/1000.
    z[67] = dfd['CLEXPRANK'].loc[3] *MTOECONV/1000.
 
    #     Coal Products (coal coke)  
    #IF (QCIIN(MNUMCR,IY) .LT. 0.0) &    
    #T95(68,IY,IS)=QCIIN(MNUMCR,IY)*25.2*(-1.0)
    # QCIIN R 4 2 (MNUMCR,MNUMYR) QUNTY tBTU CI INDUST Net Coal Coke Imports - Industrial
    z[68] = dfd['QCIIN'].loc[MNUMCR].copy()
    z[68] = z[68].where(z[68] < 0, 0)
    z[68] = z[68] * 25.2 / 1000

    return z