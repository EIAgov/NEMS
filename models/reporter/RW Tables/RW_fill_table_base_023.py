# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM

Re-coded by SZO on Jun 25, 2024
"""


def fill_table_base_023(dfd, table_spec, table_id):
    """Fill table Energy-Related Carbon Dioxide Emissions by End Use
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
        This table needs cross-reference table 24 to get emissions of Biogenic Energy Combustion.
        We designed place holders for z[76] - z[82] here, and populate them with data from table 24
        in RW_postprocessor_base. 
    """

    z = {}
    
    FYRPRC = int(table_spec["settings"]["year_index_real_dol_priceyr"])
    # Get inflation index from 1990->2050
    MC_JPGDP = dfd["MC_JPGDP"].loc[1990:]

    # Get inflation index of FYRPRC (2022)
    SCALPR2 = MC_JPGDP.loc[FYRPRC]  # 2022 = column 33
    
    MNUMCR = dfd["MNUMCR_rwpre"]
    
    #Supply, Production, Steam Methane Reforming
    z[1] = dfd['PRDH2NG'].loc[11]
    #Supply, Production, Steam Methane Reforming with CCS
    z[2] = dfd['PRDH2NG_CCS'].loc[11]
    #Supply, Production, Electrolysis
    z[3] = dfd['PRDH2EL'].loc[11]
    #Suply, Byproduct Supply to Market
    z[4] = dfd['BYPRDH2IN'].loc[11] * dfd['CFH2Q_KG'].iloc[0] / 1000
    # Supply, Total Hydrogen Supply
    z[5] = (dfd['PRDH2NG'].loc[11] + dfd['PRDH2NG_CCS'].loc[11] + dfd['PRDH2EL'].loc[11]) + (dfd['BYPRDH2IN'].loc[11] * dfd['CFH2Q_KG'].iloc[0] / 1000)

    
    # Consumption Industrial Feedstock
    z[7] = dfd['QH2IN'].loc[11] * dfd['CFH2Q_KG'].iloc[0] / 1000
    
    # Consumption Bulk Chemicals
    z[8] = dfd['CHEMCON'].loc[19].loc[5] * dfd['CFH2Q_KG'].iloc[0] / 1000
    
    # Consumption Refinery
    z[9] = dfd['QH2RF'].loc[11] * dfd['CFH2Q_KG'].iloc[0] / 1000    
    
    # Consumption Other Industrial
    z[10] = z[7] - z[8] - z[9]
    
    # Consumption Transportation
    z[11] = dfd['QH2TR'].loc[11] * dfd['CFH2Q_KG'].iloc[0] / 1000       
    
    # Consumption Transportation Lighty Duty Vehicles
    z[18] = dfd['TRQLDV'].loc[7].loc[MNUMCR] * dfd['CFH2Q_KG'].iloc[0] / 1000
    
    # Consumption Transportation Commercial Light Trucks
    z[19] = dfd['CLTFUELBTU'].loc[7] * dfd['CFH2Q_KG'].iloc[0] / 1000
    
    # Consumption Transportation Freight Trucks
    z[20] = dfd['TRQFTRK'].loc[8] * dfd['CFH2Q_KG'].iloc[0] / 1000
    
    # Consumption Transportation Bus Transportation
    # Transit Bus + Intercity Bus + School Bus
    z[21] = (dfd['TRQBUS'].loc[1].loc[8] + dfd['TRQBUS'].loc[2].loc[8] + dfd['TRQBUS'].loc[3].loc[8]) * dfd['CFH2Q_KG'].iloc[0] / 1000
    
    # Consumption Electric Power
    z[12] = dfd['QH2EL'].loc[11] * dfd['CFH2Q_KG'].iloc[0] / 1000   
    
    # Total consumption
    z[6] = z[7] + z[11] + z[12]
    
    # Discrepancy
    z[13] = z[5] - z[6]
    
    # Average market spot price
    z[14] = dfd['PH2_SPOT'].loc[11] * SCALPR2
    
    # Industrial end use price
    z[15] = dfd['PH2IN'].loc[11] * SCALPR2
    
    # Transportation end use price
    z[16] = dfd['PH2TR'].loc[11] * SCALPR2
    
    # Electric power end use price
    z[17] = dfd['PH2EL'].loc[11] * SCALPR2
    
    return z
