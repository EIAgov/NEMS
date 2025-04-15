# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM
"""


def fill_table_base_029(dfd, table_spec, table_id):
    """Fill table  Refinery Process Unit Capacity and Utilization
   
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
    
    MNUMPR=dfd['MNUMPR_rwpre']


    #   Refinery Process Unit Capacity and Utilization
    #                                                               
    #   Capacity                                                    
    #   (thousand barrels per day)                                  

    #   Crude Oil Distillation                                      
 
    #     Atmospheric Distillation Unit                             
    #T29(1:48,IY,IS)=REF_CAP(1:48,MNUMPR,IY)
    z[1] = dfd['REF_CAP'].loc[1].loc[MNUMPR]
 
    #     Vacuum Distillation Unit                                  
    #T29(1:48,IY,IS)=REF_CAP(1:48,MNUMPR,IY)
    z[2] = dfd['REF_CAP'].loc[2].loc[MNUMPR]
 
    #     Condenser on Atmospheric Distillation Unit                
    #T29(103:104,IY,IS)=REF_CAP(52:53,MNUMPR,IY)
    z[103] = dfd['REF_CAP'].loc[52].loc[MNUMPR]
 
    #     Condensate Splitter                                       
    #T29(103:104,IY,IS)=REF_CAP(52:53,MNUMPR,IY)
    z[104] = dfd['REF_CAP'].loc[53].loc[MNUMPR]
 
    #       ADU + Splitter                                          
    #T29(107,IY,IS)=REF_CAP(1,MNUMPR,IY)+REF_CAP(53,MNUMPR,IY)
    z[107] = dfd['REF_CAP'].loc[1].loc[MNUMPR] + dfd['REF_CAP'].loc[53].loc[MNUMPR]
 
    #   Conversion                                                  
 
    #     Delayed Coker                                             
    #T29(1:48,IY,IS)=REF_CAP(1:48,MNUMPR,IY)
    z[3] = dfd['REF_CAP'].loc[3].loc[MNUMPR]
 
    #     Propane De-asphalter                                      
    #T29(1:48,IY,IS)=REF_CAP(1:48,MNUMPR,IY)
    z[4] = dfd['REF_CAP'].loc[4].loc[MNUMPR]
 
    #     Distillate Hydrocracker                                   
    #T29(1:48,IY,IS)=REF_CAP(1:48,MNUMPR,IY)
    z[5] = dfd['REF_CAP'].loc[5].loc[MNUMPR]
 
    #     Fluid Catalytic Cracker                                   
    #T29(1:48,IY,IS)=REF_CAP(1:48,MNUMPR,IY)
    z[6] = dfd['REF_CAP'].loc[6].loc[MNUMPR]
 
    #   Upgrading                                                   
 
    #     Alkylation - Sulfuric Acid                                
    #T29(1:48,IY,IS)=REF_CAP(1:48,MNUMPR,IY)
    z[7] = dfd['REF_CAP'].loc[7].loc[MNUMPR]
 
    #     Butane Isomerization                                      
    #T29(1:48,IY,IS)=REF_CAP(1:48,MNUMPR,IY)
    z[8] = dfd['REF_CAP'].loc[8].loc[MNUMPR]
 
    #     Catalytic Polymerization                                  
    #T29(1:48,IY,IS)=REF_CAP(1:48,MNUMPR,IY)
    z[9] = dfd['REF_CAP'].loc[9].loc[MNUMPR]
 
    #     Pentane/Hexane Isomerization                              
    #T29(1:48,IY,IS)=REF_CAP(1:48,MNUMPR,IY)
    z[10] = dfd['REF_CAP'].loc[10].loc[MNUMPR]
 
    #     Pentane/Hexane Isomerization -Total Recycle               
    #T29(1:48,IY,IS)=REF_CAP(1:48,MNUMPR,IY)
    z[11] = dfd['REF_CAP'].loc[11].loc[MNUMPR]
 
    #     Reformer - Continuous/Cyclic - Low Pres                   
    #T29(1:48,IY,IS)=REF_CAP(1:48,MNUMPR,IY)
    z[12] = dfd['REF_CAP'].loc[12].loc[MNUMPR]
 
    #     Reformer - Semi-regenerative                              
    #T29(1:48,IY,IS)=REF_CAP(1:48,MNUMPR,IY)
    z[13] = dfd['REF_CAP'].loc[13].loc[MNUMPR]
 
    #   Hydrotreating                                               
 
    #     Benzene Saturation                                        
    #T29(1:48,IY,IS)=REF_CAP(1:48,MNUMPR,IY)
    z[14] = dfd['REF_CAP'].loc[14].loc[MNUMPR]
 
    #     Distillate Dearomatizer                                   
    #T29(1:48,IY,IS)=REF_CAP(1:48,MNUMPR,IY)
    z[15] = dfd['REF_CAP'].loc[15].loc[MNUMPR]
 
    #     Distillate Hydrotreater                                   
    #T29(1:48,IY,IS)=REF_CAP(1:48,MNUMPR,IY)
    z[16] = dfd['REF_CAP'].loc[16].loc[MNUMPR]
 
    #     FCC Naphtha Hydrotreater                                  
    #T29(1:48,IY,IS)=REF_CAP(1:48,MNUMPR,IY)
    z[17] = dfd['REF_CAP'].loc[17].loc[MNUMPR]
 
    #     Reformer Feed Naphtha Hydrotreater                        
    #T29(1:48,IY,IS)=REF_CAP(1:48,MNUMPR,IY)
    z[18] = dfd['REF_CAP'].loc[18].loc[MNUMPR]
 
    #     Gas Oil Desulfurizer/Mild Hydrocracker                    
    #T29(1:48,IY,IS)=REF_CAP(1:48,MNUMPR,IY)
    z[19] = dfd['REF_CAP'].loc[19].loc[MNUMPR]
 
    #     Vegetable Oil/Tallow Hydroprocessing (Green Diesel)       
    #T29(1:48,IY,IS)=REF_CAP(1:48,MNUMPR,IY)
    z[20] = dfd['REF_CAP'].loc[20].loc[MNUMPR]
 
    #   Splitting/Fractionization                                   
 
    #     Debutanization                                            
    #T29(1:48,IY,IS)=REF_CAP(1:48,MNUMPR,IY)
    z[21] = dfd['REF_CAP'].loc[21].loc[MNUMPR]
 
    #     FCC Naphtha Depentanizer                                  
    #T29(1:48,IY,IS)=REF_CAP(1:48,MNUMPR,IY)
    z[22] = dfd['REF_CAP'].loc[22].loc[MNUMPR]
 
    #     Gasoline Fractionation                                    
    #T29(1:48,IY,IS)=REF_CAP(1:48,MNUMPR,IY)
    z[23] = dfd['REF_CAP'].loc[23].loc[MNUMPR]
 
    #     Light Naphtha Splitter                                    
    #T29(1:48,IY,IS)=REF_CAP(1:48,MNUMPR,IY)
    z[24] = dfd['REF_CAP'].loc[24].loc[MNUMPR]
 
    #     Saturated Gas Plant                                       
    #T29(1:48,IY,IS)=REF_CAP(1:48,MNUMPR,IY)
    z[25] = dfd['REF_CAP'].loc[25].loc[MNUMPR]
 
    #     Unsaturated Gas Plant                                     
    #T29(1:48,IY,IS)=REF_CAP(1:48,MNUMPR,IY)
    z[26] = dfd['REF_CAP'].loc[26].loc[MNUMPR]
 
    #   Chemical Plants                                             
 
    #     Aromatics Extraction                                      
    #T29(1:48,IY,IS)=REF_CAP(1:48,MNUMPR,IY)
    z[27] = dfd['REF_CAP'].loc[27].loc[MNUMPR]
 
    #     Lube Oil and Wax Production                               
    #T29(1:48,IY,IS)=REF_CAP(1:48,MNUMPR,IY)
    z[28] = dfd['REF_CAP'].loc[28].loc[MNUMPR]
 
    #     Sulfur Plant                                              
    #T29(1:48,IY,IS)=REF_CAP(1:48,MNUMPR,IY)
    z[29] = dfd['REF_CAP'].loc[29].loc[MNUMPR]
 
    #   Refinery Utilities                                          
 
    #     CHP Unit                                                  
    #T29(1:48,IY,IS)=REF_CAP(1:48,MNUMPR,IY)
    z[30] = dfd['REF_CAP'].loc[30].loc[MNUMPR]
 
    #     FCC Regenerator                                           
    #T29(1:48,IY,IS)=REF_CAP(1:48,MNUMPR,IY)
    z[31] = dfd['REF_CAP'].loc[31].loc[MNUMPR]
 
    #     H2 Production - Steam Methane Reforming                   
    #T29(1:48,IY,IS)=REF_CAP(1:48,MNUMPR,IY)
    z[32] = dfd['REF_CAP'].loc[32].loc[MNUMPR]
 
    #     H2 Recovery                                               
    #T29(1:48,IY,IS)=REF_CAP(1:48,MNUMPR,IY)
    z[33] = dfd['REF_CAP'].loc[33].loc[MNUMPR]
 
    #     Power Generation                                          
    #T29(1:48,IY,IS)=REF_CAP(1:48,MNUMPR,IY)
    z[34] = dfd['REF_CAP'].loc[34].loc[MNUMPR]
 
    #     Refinery Fuel Psuedo-unit                                 
    #T29(1:48,IY,IS)=REF_CAP(1:48,MNUMPR,IY)
    z[35] = dfd['REF_CAP'].loc[35].loc[MNUMPR]
 
    #     Steam Generation                                          
    #T29(1:48,IY,IS)=REF_CAP(1:48,MNUMPR,IY)
    z[36] = dfd['REF_CAP'].loc[36].loc[MNUMPR]
 
    #   Non-Petroleum                                               
 
    #     Advanced Ethanol                                          
    #T29(1:48,IY,IS)=REF_CAP(1:48,MNUMPR,IY)
    z[37] = dfd['REF_CAP'].loc[37].loc[MNUMPR]
 
    #     Biobutanol                                                
    #T29(97:99,IY,IS)=REF_CAP(49:51,MNUMPR,IY)
    z[99] = dfd['REF_CAP'].loc[49].loc[MNUMPR]
 
    #     Biomass Pyrolysis Unit                                    
    #T29(1:48,IY,IS)=REF_CAP(1:48,MNUMPR,IY)
    z[38] = dfd['REF_CAP'].loc[38].loc[MNUMPR]
 
    #     Biomass-to-Liquids Conversion                             
    #T29(1:48,IY,IS)=REF_CAP(1:48,MNUMPR,IY)
    z[39] = dfd['REF_CAP'].loc[39].loc[MNUMPR]
 
    #     Cellulosic Ethanol Production                             
    #T29(1:48,IY,IS)=REF_CAP(1:48,MNUMPR,IY)
    z[40] = dfd['REF_CAP'].loc[40].loc[MNUMPR]
 
    #     Coal/Biomass-to-Liquids Conversion                        
    #T29(1:48,IY,IS)=REF_CAP(1:48,MNUMPR,IY)
    z[41] = dfd['REF_CAP'].loc[41].loc[MNUMPR]
 
    #     Coal/Biomass-to-Liquids with Carbon Capture               
    #T29(97:99,IY,IS)=REF_CAP(49:51,MNUMPR,IY)
    z[98] = dfd['REF_CAP'].loc[50].loc[MNUMPR]
 
    #     Coal-to-Liquids Conversion                                
    #T29(1:48,IY,IS)=REF_CAP(1:48,MNUMPR,IY)
    z[42] = dfd['REF_CAP'].loc[42].loc[MNUMPR]
 
    #     Coal-to-Liquids with Carbon Capture                       
    #T29(97:99,IY,IS)=REF_CAP(49:51,MNUMPR,IY)
    z[97] = dfd['REF_CAP'].loc[51].loc[MNUMPR]
 
    #     FAME Biodiesel                                            
    #T29(1:48,IY,IS)=REF_CAP(1:48,MNUMPR,IY)
    z[43] = dfd['REF_CAP'].loc[43].loc[MNUMPR]
 
    #     Gas Fischer-Tropsch                                       
    #T29(1:48,IY,IS)=REF_CAP(1:48,MNUMPR,IY)
    z[44] = dfd['REF_CAP'].loc[44].loc[MNUMPR]
 
    #     Starch Ethanol Dry Mill Hi                                
    #T29(1:48,IY,IS)=REF_CAP(1:48,MNUMPR,IY)
    z[45] = dfd['REF_CAP'].loc[45].loc[MNUMPR]
 
    #     Starch Ethanol Dry Mill Mod                               
    #T29(1:48,IY,IS)=REF_CAP(1:48,MNUMPR,IY)
    z[46] = dfd['REF_CAP'].loc[46].loc[MNUMPR]
 
    #     Starch Ethanol Non Corn                                   
    #T29(1:48,IY,IS)=REF_CAP(1:48,MNUMPR,IY)
    z[47] = dfd['REF_CAP'].loc[47].loc[MNUMPR]
 
    #     Starch Ethanol Wet Mill                                   
    #T29(1:48,IY,IS)=REF_CAP(1:48,MNUMPR,IY)
    z[48] = dfd['REF_CAP'].loc[48].loc[MNUMPR]
     #                                                               


    #   Utilization                                                 
    #                                                               

    #   Crude Oil Distillation                                      
 
    #     Atmospheric Distillation Unit                             
    #T29(49:96,IY,IS)=REF_UTL(1:48,MNUMPR,IY)
    z[49] = dfd['REF_UTL'].loc[1].loc[MNUMPR]
 
    #     Vacuum Distillation Unit                                  
    #T29(49:96,IY,IS)=REF_UTL(1:48,MNUMPR,IY)
    z[50] = dfd['REF_UTL'].loc[2].loc[MNUMPR]
 
    #     Condenser on Atmospheric Distillation Unit                
    #T29(105:106,IY,IS)=REF_UTL(52:53,MNUMPR,IY)
    z[105] = dfd['REF_UTL'].loc[52].loc[MNUMPR]
 
    #     Condensate Splitter                                       
    #T29(105:106,IY,IS)=REF_UTL(52:53,MNUMPR,IY)
    z[106] = dfd['REF_UTL'].loc[53].loc[MNUMPR]
 
    #       ADU + Splitter                                          
    #T29(108,IY,IS)=(REF_CAP(1,MNUMPR,IY)*REF_UTL(1,MNUMPR,IY)+REF_CAP(53,MNUMPR,IY)*REF_UTL(53,MNUMPR,IY))/(REF_CAP(1,MNUMPR,IY)+REF_CAP(53,MNUMPR,IY))
    z[108] = (dfd['REF_CAP'].loc[1].loc[MNUMPR] * dfd['REF_UTL'].loc[1].loc[MNUMPR] + dfd['REF_CAP'].loc[53].loc[MNUMPR] * dfd['REF_UTL'].loc[53].loc[MNUMPR]) / (dfd['REF_CAP'].loc[1].loc[MNUMPR] + dfd['REF_CAP'].loc[53].loc[MNUMPR])
 
    #   Conversion                                                  
 
    #     Delayed Coker                                             
    #T29(49:96,IY,IS)=REF_UTL(1:48,MNUMPR,IY)
    z[51] = dfd['REF_UTL'].loc[3].loc[MNUMPR]
 
    #     Propane De-asphalter                                      
    #T29(49:96,IY,IS)=REF_UTL(1:48,MNUMPR,IY)
    z[52] = dfd['REF_UTL'].loc[4].loc[MNUMPR]
 
    #     Distillate Hydrocracker                                   
    #T29(49:96,IY,IS)=REF_UTL(1:48,MNUMPR,IY)
    z[53] = dfd['REF_UTL'].loc[5].loc[MNUMPR]
 
    #     Fluid Catalytic Cracker                                   
    #T29(49:96,IY,IS)=REF_UTL(1:48,MNUMPR,IY)
    z[54] = dfd['REF_UTL'].loc[6].loc[MNUMPR]
 
    #   Upgrading                                                   
 
    #     Alkylation - Sulfuric Acid                                
    #T29(49:96,IY,IS)=REF_UTL(1:48,MNUMPR,IY)
    z[55] = dfd['REF_UTL'].loc[7].loc[MNUMPR]
 
    #     Butane Isomerization                                      
    #T29(49:96,IY,IS)=REF_UTL(1:48,MNUMPR,IY)
    z[56] = dfd['REF_UTL'].loc[8].loc[MNUMPR]
 
    #     Catalytic Polymerization                                  
    #T29(49:96,IY,IS)=REF_UTL(1:48,MNUMPR,IY)
    z[57] = dfd['REF_UTL'].loc[9].loc[MNUMPR]
 
    #     Pentane/Hexane Isomerization                              
    #T29(49:96,IY,IS)=REF_UTL(1:48,MNUMPR,IY)
    z[58] = dfd['REF_UTL'].loc[10].loc[MNUMPR]
 
    #     Pentane/Hexane Isomerization -Total Recycle               
    #T29(49:96,IY,IS)=REF_UTL(1:48,MNUMPR,IY)
    z[59] = dfd['REF_UTL'].loc[11].loc[MNUMPR]
 
    #     Reformer - Continuous/Cyclic - Low Pres                   
    #T29(49:96,IY,IS)=REF_UTL(1:48,MNUMPR,IY)
    z[60] = dfd['REF_UTL'].loc[12].loc[MNUMPR]
 
    #     Reformer - Semi-regenerative                              
    #T29(49:96,IY,IS)=REF_UTL(1:48,MNUMPR,IY)
    z[61] = dfd['REF_UTL'].loc[13].loc[MNUMPR]
 
    #   Hydrotreating                                               
 
    #     Benzene Saturation                                        
    #T29(49:96,IY,IS)=REF_UTL(1:48,MNUMPR,IY)
    z[62] = dfd['REF_UTL'].loc[14].loc[MNUMPR]
 
    #     Distillate Dearomatizer                                   
    #T29(49:96,IY,IS)=REF_UTL(1:48,MNUMPR,IY)
    z[63] = dfd['REF_UTL'].loc[15].loc[MNUMPR]
 
    #     Distillate Hydrotreater                                   
    #T29(49:96,IY,IS)=REF_UTL(1:48,MNUMPR,IY)
    z[64] = dfd['REF_UTL'].loc[16].loc[MNUMPR]
 
    #     FCC Naphtha Hydrotreater                                  
    #T29(49:96,IY,IS)=REF_UTL(1:48,MNUMPR,IY)
    z[65] = dfd['REF_UTL'].loc[17].loc[MNUMPR]
 
    #     Reformer Feed Naphtha Hydrotreater                        
    #T29(49:96,IY,IS)=REF_UTL(1:48,MNUMPR,IY)
    z[66] = dfd['REF_UTL'].loc[18].loc[MNUMPR]
 
    #     Gas Oil Desulfurizer/Mild Hydrocracker                    
    #T29(49:96,IY,IS)=REF_UTL(1:48,MNUMPR,IY)
    z[67] = dfd['REF_UTL'].loc[19].loc[MNUMPR]
 
    #     Vegetable Oil/Tallow Hydroprocessing (Green Diesel)       
    #T29(49:96,IY,IS)=REF_UTL(1:48,MNUMPR,IY)
    z[68] = dfd['REF_UTL'].loc[20].loc[MNUMPR]
 
    #   Splitting/Fractionization                                   
 
    #     Debutanization                                            
    #T29(49:96,IY,IS)=REF_UTL(1:48,MNUMPR,IY)
    z[69] = dfd['REF_UTL'].loc[21].loc[MNUMPR]
 
    #     FCC Naphtha Depentanizer                                  
    #T29(49:96,IY,IS)=REF_UTL(1:48,MNUMPR,IY)
    z[70] = dfd['REF_UTL'].loc[22].loc[MNUMPR]
 
    #     Gasoline Fractionation                                    
    #T29(49:96,IY,IS)=REF_UTL(1:48,MNUMPR,IY)
    z[71] = dfd['REF_UTL'].loc[23].loc[MNUMPR]
 
    #     Light Naphtha Splitter                                    
    #T29(49:96,IY,IS)=REF_UTL(1:48,MNUMPR,IY)
    z[72] = dfd['REF_UTL'].loc[24].loc[MNUMPR]
 
    #     Saturated Gas Plant                                       
    #T29(49:96,IY,IS)=REF_UTL(1:48,MNUMPR,IY)
    z[73] = dfd['REF_UTL'].loc[25].loc[MNUMPR]
 
    #     Unsaturated Gas Plant   
    z[74] = dfd['REF_UTL'].loc[26].loc[MNUMPR]    
 
    #   Chemical Plants                                             
 
    #     Aromatics Extraction                                      
    #T29(49:96,IY,IS)=REF_UTL(1:48,MNUMPR,IY)
    z[75] = dfd['REF_UTL'].loc[27].loc[MNUMPR]
 
    #     Lube Oil and Wax Production                               
    #T29(49:96,IY,IS)=REF_UTL(1:48,MNUMPR,IY)
    z[76] = dfd['REF_UTL'].loc[28].loc[MNUMPR]
 
    #     Sulfur Plant                                              
    #T29(49:96,IY,IS)=REF_UTL(1:48,MNUMPR,IY)
    z[77] = dfd['REF_UTL'].loc[29].loc[MNUMPR]
 
    #   Refinery Utilities                                          
 
    #     CHP Unit                                                  
    #T29(49:96,IY,IS)=REF_UTL(1:48,MNUMPR,IY)
    z[78] = dfd['REF_UTL'].loc[30].loc[MNUMPR]
 
    #     FCC Regenerator                                           
    #T29(49:96,IY,IS)=REF_UTL(1:48,MNUMPR,IY)
    z[79] = dfd['REF_UTL'].loc[31].loc[MNUMPR]
 
    #     H2 Production - Steam Methane Reforming                   
    #T29(49:96,IY,IS)=REF_UTL(1:48,MNUMPR,IY)
    z[80] = dfd['REF_UTL'].loc[32].loc[MNUMPR]
 
    #     H2 Recovery                                               
    #T29(49:96,IY,IS)=REF_UTL(1:48,MNUMPR,IY)
    z[81] = dfd['REF_UTL'].loc[33].loc[MNUMPR]
 
    #     Power Generation                                          
    #T29(49:96,IY,IS)=REF_UTL(1:48,MNUMPR,IY)
    z[82] = dfd['REF_UTL'].loc[34].loc[MNUMPR]
 
    #     Refinery Fuel Psuedo-unit                                 
    #T29(49:96,IY,IS)=REF_UTL(1:48,MNUMPR,IY)
    z[83] = dfd['REF_UTL'].loc[35].loc[MNUMPR]
 
    #     Steam Generation                                          
    #T29(49:96,IY,IS)=REF_UTL(1:48,MNUMPR,IY)
    z[84] = dfd['REF_UTL'].loc[36].loc[MNUMPR]
 
    #   Non-Petroleum                                               
 
    #     Advanced Ethanol                                          
    #T29(49:96,IY,IS)=REF_UTL(1:48,MNUMPR,IY)
    z[85] = dfd['REF_UTL'].loc[37].loc[MNUMPR]
 
    #     Biobutanol                                                
    #T29(100:102,IY,IS)=REF_UTL(49:51,MNUMPR,IY)
    z[102] = dfd['REF_UTL'].loc[49].loc[MNUMPR]
 
    #     Biomass Pyrolysis Unit                                    
    #T29(49:96,IY,IS)=REF_UTL(1:48,MNUMPR,IY)
    z[86] = dfd['REF_UTL'].loc[38].loc[MNUMPR]
 
    #     Biomass-to-Liquids Conversion                             
    #T29(49:96,IY,IS)=REF_UTL(1:48,MNUMPR,IY)
    z[87] = dfd['REF_UTL'].loc[39].loc[MNUMPR]
 
    #     Cellulosic Ethanol Production                             
    #T29(49:96,IY,IS)=REF_UTL(1:48,MNUMPR,IY)
    z[88] = dfd['REF_UTL'].loc[40].loc[MNUMPR]
 
    #     Coal/Biomass-to-Liquids Conversion                        
    #T29(49:96,IY,IS)=REF_UTL(1:48,MNUMPR,IY)
    z[89] = dfd['REF_UTL'].loc[41].loc[MNUMPR]
 
    #     Coal/Biomass-to-Liquids with Carbon Capture               
    #T29(100:102,IY,IS)=REF_UTL(49:51,MNUMPR,IY)
    z[101] = dfd['REF_UTL'].loc[50].loc[MNUMPR]
 
    #     Coal-to-Liquids Conversion                                
    #T29(49:96,IY,IS)=REF_UTL(1:48,MNUMPR,IY)
    z[90] = dfd['REF_UTL'].loc[42].loc[MNUMPR]
 
    #     Coal-to-Liquids with Carbon Capture                       
    #T29(100:102,IY,IS)=REF_UTL(49:51,MNUMPR,IY)
    z[100] = dfd['REF_UTL'].loc[51].loc[MNUMPR]
 
    #     FAME Biodiesel                                            
    #T29(49:96,IY,IS)=REF_UTL(1:48,MNUMPR,IY)
    z[91] = dfd['REF_UTL'].loc[43].loc[MNUMPR]
 
    #     Gas Fischer-Tropsch                                       
    #T29(49:96,IY,IS)=REF_UTL(1:48,MNUMPR,IY)
    z[92] = dfd['REF_UTL'].loc[44].loc[MNUMPR]
 
    #     Starch Ethanol Dry Mill Hi                                
    #T29(49:96,IY,IS)=REF_UTL(1:48,MNUMPR,IY)
    z[93] = dfd['REF_UTL'].loc[45].loc[MNUMPR]
 
    #     Starch Ethanol Dry Mill Mod                               
    #T29(49:96,IY,IS)=REF_UTL(1:48,MNUMPR,IY)
    z[94] = dfd['REF_UTL'].loc[46].loc[MNUMPR]
 
    #     Starch Ethanol Non Corn                                   
    #T29(49:96,IY,IS)=REF_UTL(1:48,MNUMPR,IY)
    z[95] = dfd['REF_UTL'].loc[47].loc[MNUMPR]
 
    #     Starch Ethanol Wet Mill                                   
    #T29(49:96,IY,IS)=REF_UTL(1:48,MNUMPR,IY)
    z[96] = dfd['REF_UTL'].loc[48].loc[MNUMPR]
    
    return z
