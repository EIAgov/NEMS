# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_031(dfd, table_spec, table_id):
    """Fill table     Energy and Energy Efficiency Indices
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

############## LOTS of preprocessing - see code

    #   Energy and Energy Efficiency Indices
    #   (index values, unless otherwise noted)
    #    Key Indicators and Consumption                             
    #                                                               
    #   Aggregate Primary Energy Indices                            
    #      Energy/GDP (m__m$) Ratio                                 T31(1,IY,IS)=(113,11,IY,IS)
   #z[1] = 

    #      Energy/GDP (m__m$) Index                                 T31(8,IY,IS)=T31(1,IY,IS)/T31(1,INDEXSTARTYEAR-1,IS)
   #z[8] = 

    #      Carbon/GDP (m__m$) Ratio                                 T31(1:16,1:INDEXSTARTYEAR-1,IS)=1.
   #z[14] = 

    #      Carbon/GDP (m__m$) Index                                 T31(15,IY,IS)=T31(14,IY,IS)/T31(14,INDEXSTARTYEAR-1,IS)
   #z[15] = 

    #      Aggregate Energy Efficiency                              T31(7,IY,IS)=T31(7,IY,IS)+0.5*(TRNEN_CY(ILINE_NBR)/T2(ITOTAL_NBR,11,IY,IS)+TRNEN_PY(ILINE_NBR)/T2(ITOTAL_NBR,11,IY-1,IS))*LOG((TRNEN_CY(ILINE_NBR)/TRDRV_CY(ILINE_NBR))/(TRNEN_PY(ILINE_NBR)/TRDRV_PY(ILINE_NBR)))
   #z[7] = 

    #      Aggregate Carbon Efficiency                              T31(13,IY,IS)=T31(13,IY,IS)+0.5*(TRNEN_CY(ILINE_NBR)/T17(ITOTAL_NBR_CARB,11,IY,IS)+TRNEN_PY(ILINE_NBR)/T17(ITOTAL_NBR_CARB,11,IY-1,IS))*LOG((TRNEN_CY(ILINE_NBR)/TRDRV_CY(ILINE_NBR))/(TRNEN_PY(ILINE_NBR)/TRDRV_PY(ILINE_NBR)))
   #z[13] = 

    #                                                               
    #   Energy Efficiency by Sector                                 
    #      Residential                                              T31(2,IY,IS)=T31(2,IY,IS)+0.5*(RS(ILINE_NBR)/T2(ITOTAL_NBR_RES,11,IY,IS)+RSLY(ILINE_NBR)/T2(ITOTAL_NBR_RES,11,IY-1,IS))*LOG((RSEF(ILINE_NBR)/DRIVER_CY)/(RSEFLY(ILINE_NBR)/DRIVER_PY))
   #z[2] = 

    #      Commercial                                               T31(3,IY,IS)=T31(3,IY,IS)+0.5*(CMEN_CY(ILINE_NBR)/T2(ITOTAL_NBR_COM,11,IY,IS)+CMEN_PY(ILINE_NBR)/T2(ITOTAL_NBR_COM,11,IY-1,IS))*LOG((CMINDX_CY(ILINE_NBR)/CMDRIVER_CY(ILINE_NBR))/(CMINDX_PY(ILINE_NBR)/CMDRIVER_PY(ILINE_NBR)))
   #z[3] = 

    #      Industrial                                               T31(4,IY,IS)=T31(4,IY,IS)+0.5*(TAB34_CY(ILINE_NBR)/T2(ITOTAL_NBR_IND,11,IY,IS)+TAB34_PY(ILINE_NBR)/T2(ITOTAL_NBR_IND,11,IY-1,IS))*LOG((TAB34_CY(ILINE_NBR)/DRIVER_CY)/(TAB34_PY(ILINE_NBR)/DRIVER_PY))
   #z[4] = 

    #      Transportation                                           T31(5,IY,IS)=T31(5,IY,IS)+0.5*(TRNEN_CY(ILINE_NBR)/T2(ITOTAL_NBR_TRAN,11,IY,IS)+TRNEN_PY(ILINE_NBR)/T2(ITOTAL_NBR_TRAN,11,IY-1,IS))*LOG((TRNEN_CY(ILINE_NBR)/TRDRV_CY(ILINE_NBR))/(TRNEN_PY(ILINE_NBR)/TRDRV_PY(ILINE_NBR)))
   #z[5] = 

    #      Electric Power                                           T31(6,IY,IS)=T31(6,IY,IS)+0.5*(T2(ILINE_NBR,11,IY,IS)/T2(ITOTAL_NBR_UTIL,11,IY,IS)+T2(ILINE_NBR,11,IY-1,IS)/T2(ITOTAL_NBR_UTIL,11,IY-1,IS))*LOG((T2(ILINE_NBR,11,IY,IS)/DRIVER_CY)/(T2(ILINE_NBR,11,IY-1,IS)/DRIVER_PY))
   #z[6] = 

    #                                                               
    #   Carbon Efficiency by Sector                                 
    #      Residential                                              T31(9,IY,IS)=T31(9,IY,IS)+0.5*(RS(ILINE_NBR)/T17(ITOTAL_NBR_CARB_RES,11,IY,IS)+RSLY(ILINE_NBR)/T17(ITOTAL_NBR_CARB_RES,11,IY-1,IS))*LOG((RSEF(ILINE_NBR)/DRIVER_CY)/(RSEFLY(ILINE_NBR)/DRIVER_PY))
   #z[9] = 

    #      Commercial                                               T31(10,IY,IS)=T31(10,IY,IS)+0.5*(CMEN_CY(ILINE_NBR)/T17(ITOTAL_NBR_CARB_COM,11,IY,IS)+CMEN_PY(ILINE_NBR)/T17(ITOTAL_NBR_CARB_COM,11,IY-1,IS))*LOG((CMINDX_CY(ILINE_NBR)/CMDRIVER_CY(ILINE_NBR))/(CMINDX_PY(ILINE_NBR)/CMDRIVER_PY(ILINE_NBR)))
   #z[10] = 

    #      Industrial                                               T31(11,IY,IS)=T31(11,IY,IS)+0.5*(INDCRB(IX,JX,2)/T17(ITOTAL_NBR_CARB_IND,11,IY,IS)+INDCRB(IX,JX,1)/T17(ITOTAL_NBR_CARB_IND,11,IY-1,IS))*LOG((INDCRB(IX,JX,2)/INDDRIVER(IX,2))/(INDCRB(IX,JX,1)/INDDRIVER(IX,1)))
   #z[11] = 

    #      Transportation                                           T31(12,IY,IS)=T31(12,IY,IS)+0.5*(TRNEN_CY(ILINE_NBR)/T17(ITOTAL_NBR_CARB_TRAN,11,IY,IS)+TRNEN_PY(ILINE_NBR)/T17(ITOTAL_NBR_CARB_TRAN,11,IY-1,IS))*LOG((TRNEN_CY(ILINE_NBR)/TRDRV_CY(ILINE_NBR))/(TRNEN_PY(ILINE_NBR)/TRDRV_PY(ILINE_NBR)))
   #z[12] = 

    return z