# -*- coding: utf-8 -*-
"""
Created on Mon Dec 18 09:49:06 2023

@author: TDM

Re-coded by SZO on Jun 25, 2024
"""


def fill_table_base_073(dfd, table_spec, table_id):
    """Fill table Hydrogen Market Projections by Census Division
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
    """

    z = {}
    
    FYRPRC = int(table_spec["settings"]["year_index_real_dol_priceyr"])
    # Get inflation index from 1990->2050
    MC_JPGDP = dfd["MC_JPGDP"].loc[1990:]

    # Get inflation index of FYRPRC (2022)
    SCALPR2 = MC_JPGDP.loc[FYRPRC]  # 2022 = column 33
    
    MNUMCR = dfd["MNUMCR_rwpre"]
    
    #Supply, Production, Production by Technology (TBtu)
    z[1] = (dfd['PRDH2NG'] + dfd['PRDH2NG_CCS'] + dfd['PRDH2EL']) / dfd['CFH2Q_KG'].iloc[0] * 1000
    #Supply, Production, Steam Methane Reforming
    z[2] = dfd['PRDH2NG'] / dfd['CFH2Q_KG'].iloc[0] * 1000.0
    #Supply, Production, Steam Methane Reforming with CCS
    z[3] = dfd['PRDH2NG_CCS'] / dfd['CFH2Q_KG'].iloc[0] * 1000.0
    #Supply, Production, Electrolysis
    z[4] = dfd['PRDH2EL'] / dfd['CFH2Q_KG'].iloc[0] * 1000.0
    #Suply, Byproduct Supply to Market
    z[5] = dfd['BYPRDH2IN']
    # Supply, Total Hydrogen Supply
    z[6] = (dfd['PRDH2NG'] + dfd['PRDH2NG_CCS'] + dfd['PRDH2EL']) / dfd['CFH2Q_KG'].iloc[0] * 1000.0 + (dfd['BYPRDH2IN'])
    #z[6] = z[1] + z[5]

    # Total Consumption by Fuel, Hydrogen Production, Natural gas
    z[7] = dfd['QNGHM']
    
    # Total Consumption by Fuel, Hydrogen Production, Purchased Electricity
    z[8] = dfd['QELHM']
    
    # Total Consumption by Fuel, Hydrogen Production, Total Energy Consumed in H2 Production
    z[9] = z[7]+z[8]
    
    # Energy Consumption by Hydrogen Production Technology and Use (TBtu), Steam Methane Reforming (with and without CCS), Natural Gas
    z[10] = dfd['QNGHM']
    
    # Energy Consumption by Hydrogen Production Technology and Use (TBtu), Steam Methane Reforming (with and without CCS), Natural Gas, Feedstock
    z[11] = dfd['QNGHMPF']
    
    # Energy Consumption by Hydrogen Production Technology and Use (TBtu), Steam Methane Reforming (with and without CCS), Natural Gas, Head and Power
    z[12] = dfd['QNGHMHP']
    
    # Energy Consumption by Hydrogen Production Technology and Use (TBtu), Steam Methane Reforming (with and without CCS), Purchased Electricity
    z[13] = dfd['QELINH2NG']
    
    # Energy Consumption by Hydrogen Production Technology and Use (TBtu), Steam Methane Reforming (with and without CCS), Total
    z[14] = z[10] + z[13]
    
    # Energy Consumption by Hydrogen Production Technology and Use (TBtu), PEM Electrolysis, Puchased Electricity
    z[15] = dfd['QELINH2E']
    
    # Consumption (H2) by sector, Industrial Feedstock
    z[16] = dfd['QH2IN']
    
    # Consumption (H2) by sector, Industrial Feedstock, Refinery
    z[17] = dfd['QH2RF']
    
    # Consumption (H2) by sector, Industrial Feedstock, Bulk Chemicals and Other Industrial
    z[18] = dfd['QH2IN'] - dfd['QH2RF']
    
    # Consumption (H2) by sector, Transportation
    z[19] = dfd['QH2TR']
    
    # Consumption (H2) by sector, Electric Power
    z[20] = dfd['QH2EL']
    
    # Prices, H2 Average Market Spot Price
    z[21] = dfd['PH2_SPOT'] * SCALPR2
    
    # Delivered End-Use Prices, Industrial
    z[22] = dfd['PH2IN'] * SCALPR2
    
    # Delivered End-Use Prices, Transportation
    z[23] = dfd['AMPBLK/PH2TR'] * SCALPR2
    
    # Delivered End-Use Prices, Electric Power
    z[24] = dfd['PH2EL'] * SCALPR2
    
    return z
