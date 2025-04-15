import numpy as np
import pandas as pd
import os
import gzip
import tarfile
import sqlite3
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages


class DataPrep:
    """ Empty Class to store data """    
    pass


def get_hmm_transporation(d):

    file = 'HydrogenTransportationByYearSeason.csv'

    try:
        df = pd.read_csv(os.path.join(os.getcwd(),'hmm', 'debugINFO', file))    #     debugINFO
    except FileNotFoundError:
        df = pd.read_csv(os.path.join(os.getcwd(),'p2', 'hmm', 'debugINFO', file))    #     debugINFO

    # add to the data exchange object
    d.hydrogen_transportation = df


def get_hydrogen_storage_withdrawal(d):

    file = 'HydrogenStorageWithdrawalByYearSeason.csv'

    try:
        df = pd.read_csv(os.path.join(os.getcwd(),'hmm', 'debugINFO', file))    #     debugINFO
    except FileNotFoundError:
        df = pd.read_csv(os.path.join(os.getcwd(),'p2', 'hmm', 'debugINFO', file))    #     debugINFO

    # add to the data exchange object
    d.hydrogen_storage_withdrawal = df


def get_hydrogen_demand_seasonal(d):

    file = 'H2DemandBySectorSeasonalMMTons.csv'

    try:
        df = pd.read_csv(os.path.join(os.getcwd(),'hmm', 'debugINFO', file ))    #     debugINFO
    except FileNotFoundError:
        df = pd.read_csv(os.path.join(os.getcwd(),'p2', 'hmm', 'debugINFO', file))    #     debugINFO

    df.columns = ['Regions','Sectors','Seasons','Years', 'Demand']
    # add to the data exchange object
    d.hydrogen_demand_by_sector = df


def get_hydrogen_storage_injection(d):

    file = 'HydrogenStorageInjectionByYearSeason.csv'

    try:
        df = pd.read_csv(os.path.join(os.getcwd(),'hmm', 'debugINFO', file ))    #     debugINFO
    except FileNotFoundError:
        df = pd.read_csv(os.path.join(os.getcwd(),'p2', 'hmm', 'debugINFO', file))    #     debugINFO

    # add to the data exchange object
    d.hydrogen_storage_injection = df


def get_seasonal_byproductsupply(d):    

    file = 'SeasonalRegionalByproductSupply_'+str(d.year)+'.csv'

    try:
        df = pd.read_csv(os.path.join(os.getcwd(),'hmm', 'debugINFO', file ))    #     debugINFO
    except FileNotFoundError:
        df = pd.read_csv(os.path.join(os.getcwd(),'p2', 'hmm', 'debugINFO', file))    #     debugINFO
    
    df.columns = ['Regions', 'Season', 'Years', 'ByProduct']
    
    # Combine HMM seasons 1 and 3 by setting 1 to 3 and doing a groupby where EMM = 3 
    df.loc[df['Season']==1, 'Season']=3 # Combine HMM shoulder seasons by setting 1 to 3
    df.loc[df['Season']==4, 'Season']=1 # HMM 4th seasons is EMM 1

    # the groupby
    df = df.groupby(['Regions', 'Season', 'Years']).sum().reset_index(drop=False)

    # add to the data exchange object
    d.df_byproduct = pd.concat([d.df_byproduct, df], axis=0)


def get_natgas_consumption(d):
    file = 'QBLK_QNGHM_'+str(d.year)+'.csv'

    try:
        df = pd.read_csv(os.path.join(os.getcwd(),'hmm', 'debugINFO', file ))    #     debugINFO
    except FileNotFoundError:
        df = pd.read_csv(os.path.join(os.getcwd(),'p2', 'hmm', 'debugINFO', file))    #     debugINFO

    df.columns = ['Regions', 'Years', 'NatGasConsumption']
    

    # add to the data exchange object
    d.df_natgas_consumption = pd.concat([d.df_natgas_consumption, df], axis=0)


def get_hmm_electrolyzer_power_consumption(d):
    file = 'HMMBLK_QELHM_HR_'+str(d.year)+'.csv'

    try:
        df = pd.read_csv(os.path.join(os.getcwd(),'hmm', 'debugINFO', file ))    #     debugINFO
    except FileNotFoundError:
        df = pd.read_csv(os.path.join(os.getcwd(),'p2', 'hmm', 'debugINFO', file))    #     debugINFO

    df.columns = ['Regions', 'Season', 'Hours', 'Years', 'Power (Mwh)']
    
    # Combine HMM seasons 1 and 3 by setting 1 to 3 and doing a groupby where EMM = 3 
    df.loc[df['Season']==1, 'Season']=3 # Combine HMM shoulder seasons by setting 1 to 3
    df.loc[df['Season']==4, 'Season']=1 # HMM 4th seasons is EMM 1

    # the groupby
    df = df.groupby(['Regions', 'Season', 'Hours','Years']).sum().reset_index(drop=False)

    # add to the data exchange object
    d.df_electrolyzer_power_consumption = pd.concat([d.df_electrolyzer_power_consumption, df], axis=0)


def unpack_restore_tarfile(d):
    parallel = os.path.exists('p2')

    if parallel:
        f_tar = 'p2\\rest\\fromAIMMS\\Output_'+str(d.year)+'.tar.gz'
        f_dir = 'p2\\rest\\fromAIMMS\\Output_'+str(d.year)
    else:
        f_tar = 'rest\\fromAIMMS\\Output_'+str(d.year)+'.tar.gz'
        f_dir = 'rest\\fromAIMMS\\Output_'+str(d.year)

    if not os.path.isdir(f_dir):

        extract_direcotry = os.path.dirname(f_tar)

        if tarfile.is_tarfile(f_tar):
            with tarfile.open(f_tar) as f:
                f.extractall(path=extract_direcotry)

    # assign directory for later use
    d.f_dir = f_dir


def get_restore_generation_data(d):
    p = os.path.join(d.f_dir, 'Rep_Generation.csv')
    df = pd.read_csv(p, compression='infer')
    df['season'] = df['Month'].map(d.month2EMMseason)
    d.df_gen = pd.concat([d.df_gen, df], axis=0)


# 1:jan, daytype 1: weekday, 2:weekend, 3: 
def get_hmm_smr_data(d):
    pass

def get_h2_transporation(d):
    file = 'HydrogenTransportationByYearSeason.csv'

    try:
        df = pd.read_csv(os.path.join(os.getcwd(),'hmm', 'debugINFO', file ))    #     debugINFO
    except FileNotFoundError:
        df = pd.read_csv(os.path.join(os.getcwd(),'p2', 'hmm', 'debugINFO', file))    #     debugINFO

    df.columns = ['Year', 'Region_1', 'Region_2', 'Seasons', 'HydrogenTransporation']

    df.loc[df['Seasons']==1, 'Seasons']=3 # Combine HMM shoulder seasons by setting 1 to 3
    df.loc[df['Seasons']==4, 'Seasons']=1 # HMM 4th seasons is EMM 1

    # the groupby
    df = df.groupby(['Year', 'Seasons', 'Region_1', 'Region_2']).sum().reset_index(drop=False)

    # add to the data exchange object
    d.df_transporation = df


def get_seasonal_regional_production(d):
    file = 'SeasonalRegionalH2ProductionByTech.csv'

    try:
        df = pd.read_csv(os.path.join(os.getcwd(),'hmm', 'debugINFO', file ))    #     debugINFO
    except FileNotFoundError:
        df = pd.read_csv(os.path.join(os.getcwd(),'p2', 'hmm', 'debugINFO', file))    #     debugINFO

    df.columns = ['Regions', 'Seasons', 'Tech', 'Years', 'H2Production']

    # convert to EMM regions
    # Combine HMM seasons 1 and 3 by setting 1 to 3 and doing a groupby where EMM = 3 
    df.loc[df['Seasons']==1, 'Seasons']=3 # Combine HMM shoulder seasons by setting 1 to 3
    df.loc[df['Seasons']==4, 'Seasons']=1 # HMM 4th seasons is EMM 1

    # the groupby
    df = df.groupby(['Regions', 'Seasons', 'Tech','Years']).sum().reset_index(drop=False)

    # add to the data exchange object
    d.df_production_by_tech = df


def get_restore_battery_data(d):
    p = os.path.join(d.f_dir, 'Rep_Storage.csv')
    df = pd.read_csv(p, compression='infer')
    d.df_storage = pd.concat([d.df_storage, df], axis=0)


def get_curtailment_data(d):
    p = os.path.join(d.f_dir, 'Rep_Curtailment.csv')
    df = pd.read_csv(p, compression='infer')
    d.df_curtailment = pd.concat([d.df_curtailment, df], axis=0)


def get_emm_dayweights(d):
    parallel = os.path.exists('p2')

    if parallel:
        f_db = r'p2\\emm_db\\NEMS_INPUT.db'
    else:
        f_db = r'emm_db\\NEMS_INPUT.db'

    # Read day weights from the NEMS_INPUT.db sqlite db from the V_SYS_LOAD_WEIGHTS and store for later use
    con = sqlite3.connect(f_db)
    d.df_dayweights = pd.read_sql_query("SELECT * from V_SYS_LOAD_WEIGHTS", con)
    con.close()


def convert_restore_generation_timeslices(d):
    df_generation = d.df_gen # restore generation
    df_dayweights = d.df_dayweights # emm day weights

    df = pd.merge(df_generation, df_dayweights, left_on=['Month','DayType'], right_on=['Month','DAY'])
    df['Generation (Energy)'] = 1000.0 * df['Rep_Generation'] * df['WEIGHTS'] # Convert Generation Power to Energy Units (gw to gwhrs to mwhrs)
    
    # filtering for solveindex = 1 which contains normal Restore results whereas solveindex = 2 is only for ECP to decide how much battery storage to add
    df = df[(df.SolveIndex==1) & (df.Ecpgroup==15)] # Ecpgroup = 15 is hydrogen

    df_hourly = df.groupby(['Year','EMMReg', 'season', 'ECP_Hour'])['Generation (Energy)'].sum().reset_index(drop=False)
    df_season = df.groupby(['Year','EMMReg', 'season'])['Generation (Energy)'].sum().reset_index(drop=False)

    # add to the data object
    d.df_generation_hourly = df_hourly
    d.df_generation_season = df_season


def convert_restore_storage_timeslices(d):
    df_storage = d.df_storage # restore generation
    df_dayweights = d.df_dayweights # emm day weights

    df = pd.merge(df_storage, df_dayweights, left_on=['Month','DayType'], right_on=['Month','DAY'])
    df['Storage (Energy)'] = df['Rep_Storage'] * df['WEIGHTS'] # Convert Storage Power to Energy Units 
    
    # filtering for solveindex = 1 which contains normal Restore results whereas solveindex = 2 is only for ECP to decide how much battery storage to add
    # Ecpgroup = 11 is Pumped Hydro, 13 standalone battery, 21 is hybrid battery, we exclued 11 (pumpedhydro) because
    # we are comparing the short term duration battery to hydrogen storage dispatch GenStep = solveindex here
    df = df[(df.GenSteps==1) & (df.Ecpgroup!=11)] 
    df['season'] = df['Month'].map(d.month2EMMseason)

    df = df.groupby(['Year', 'EMMReg', 'season', 'ECP_Hour'])['Storage (Energy)'].sum().reset_index(drop=False)
    # add to the data object
    d.df_storage = df


def convert_restore_curtailment(d):
    df_curtailment = d.df_curtailment # restore generation
    df_dayweights = d.df_dayweights # emm day weights

    df = pd.merge(df_curtailment, df_dayweights, left_on=['Month','DayType'], right_on=['Month','DAY'])
    df['Curtailment (Energy)'] = df['Rep_Curtailment'] * df['WEIGHTS'] # Convert Storage Power to Energy Units 
    
    # filtering for solveindex = 1 which contains normal Restore results whereas solveindex = 2 is only for ECP to decide how much battery storage to add
    df = df[(df.SolveIndex==1)] 

    # groupby over the techs on an hourly levels
    df_curtailment = df.groupby(['Ecpgroup', 'DayType','EMMReg','Month', 'ECP_Hour'])['Curtailment (Energy)'].sum().reset_index(drop=False)
    df['season'] = df['Month'].map(d.month2EMMseason)

    # add to the data object
    d.df_curtailment = df_curtailment


def months2seasons(d):
    # TODO: don't hardcode
    # EMM seasons are 1: Winter, 2: Summer, 3: Shoulder
    d.month2EMMseason = {1:1, 2:1, 3:3, 4:3, 5:2, 6:2, 7:2, 8:2, 9:3, 10:3, 11:1, 12:1}
    

def plot_hydrogen_production_by_tech(d):

    #os.makedirs('production',exist_ok=True)
    #os.chdir('production')

    # problems
    # 1 Fix units on ylabel, not mwhrs

    figs = [] # list of figures

    df = d.df_production_by_tech
    for year in df.Years.unique():
        for region in df.Regions.unique():
            df_plot = df.loc[(df['Regions']==region) & (df['Years']==year)]
            df_plot = pd.pivot_table(df_plot, values='H2Production', index=['Seasons'], columns=['Tech'], aggfunc="sum")
            df_plot.fillna(0.0, inplace=True)
            if not df_plot.empty:
                plot = df_plot.plot.bar(rot=0, xlabel='Seasons', ylabel='Mwhrs', title=str(year) + ' ' + str(region))
                figs.append(plot.get_figure())

    with PdfPages('productionByTech.pdf') as pdf:
        for fig in figs:
            pdf.savefig(fig, bbox_inches='tight') 


def plot_demand_vs_production(d):

    #Todo: convert the hmm seasons to EMM seasons when reading, except for production, to avoid double count.

    # todo: convert to emm season upon reading
    df_hmm_storage = pd.merge(d.hydrogen_storage_injection, d.hydrogen_storage_withdrawal, on=['Regions_','Seasons_','cal_Year'], how='outer')
    df_hmm_storage.fillna(0.0, inplace=True)
    df_hmm_storage['HydrogenStorageWithdrawalByYearSeason'] = -1.0 * df_hmm_storage['HydrogenStorageWithdrawalByYearSeason'] 
    df_hmm_storage.columns = ['Years', 'Regions', 'Seasons', 'StgInject', 'StgWithdrawal']
    df_hmm_storage.loc[df_hmm_storage['Seasons']==1, 'Seasons']=3 # Combine HMM shoulder seasons by setting 1 to 3
    df_hmm_storage.loc[df_hmm_storage['Seasons']==4, 'Seasons']=1 # HMM 4th seasons is EMM 1
    df_hmm_storage = df_hmm_storage.groupby(['Years', 'Regions', 'Seasons']).sum().reset_index(drop=False)

    # todo: convert to emm season upon reading
    df_transport_import = d.df_transporation    
    df_transport_import.columns = ['Years', 'Seasons', 'Import', 'Export', 'Transport_Imports']
    df_transport_import = df_transport_import.groupby(['Import', 'Seasons', 'Years'])[['Transport_Imports']].sum().reset_index(drop=False)
    df_transport_import.rename(columns={'Import':'Regions'},inplace=True)
    df_transport_import.loc[df_transport_import['Seasons']==1, 'Seasons']=3 # Combine HMM shoulder seasons by setting 1 to 3
    df_transport_import.loc[df_transport_import['Seasons']==4, 'Seasons']=1 # HMM 4th seasons is EMM 1
    df_transport_import = df_transport_import.groupby(['Years', 'Seasons','Regions']).sum().reset_index(drop=False)

    # todo: convert to emm season upon reading
    df_transport_export = d.df_transporation
    df_transport_export.columns = ['Years', 'Seasons', 'Import', 'Export', 'Transport_Exports']
    df_transport_export = df_transport_export.groupby(['Export', 'Seasons', 'Years'])[['Transport_Exports']].sum().reset_index(drop=False)
    df_transport_export.rename(columns={'Export':'Regions'},inplace=True)
    df_transport_export['Transport_Exports'] = -1.0 * df_transport_export['Transport_Exports']
    df_transport_export.loc[df_transport_export['Seasons']==1, 'Seasons']=3 # Combine HMM shoulder seasons by setting 1 to 3
    df_transport_export.loc[df_transport_export['Seasons']==4, 'Seasons']=1 # HMM 4th seasons is EMM 1
    df_transport_export = df_transport_export.groupby(['Years', 'Seasons','Regions']).sum().reset_index(drop=False)

    # todo: no need to convert to seasons, already done
    df_prod_by_tech = d.df_production_by_tech

    # todo: convert to emm season upon reading
    df_h2_demand = d.hydrogen_demand_by_sector
    df_h2_demand.loc[df_h2_demand['Seasons']==1, 'Seasons']=3 # Combine HMM shoulder seasons by setting 1 to 3
    df_h2_demand.loc[df_h2_demand['Seasons']==4, 'Seasons']=1 # HMM 4th seasons is EMM 1
    df_h2_demand = df_h2_demand.groupby(['Regions', 'Sectors', 'Seasons', 'Years']).sum().reset_index(drop=False)

    df_demand = pd.pivot_table(df_h2_demand, values='Demand', index=['Regions','Seasons','Years'], columns=['Sectors'], aggfunc="sum").reset_index(drop=False)
    df_production = pd.pivot_table(df_prod_by_tech, values='H2Production', index=['Regions','Seasons','Years'], columns=['Tech'], aggfunc="sum").reset_index(drop=False)

    df = pd.merge(df_production, df_demand, left_on=['Regions', 'Seasons', 'Years'], right_on=['Regions','Seasons','Years'])
    df = pd.merge(df, df_hmm_storage, on=['Regions', 'Seasons', 'Years'], how='outer')
    df = pd.merge(df, df_transport_import, left_on=['Regions', 'Seasons', 'Years'], right_on=['Regions','Seasons','Years'], how='outer')
    df = pd.merge(df, df_transport_export, left_on=['Regions', 'Seasons', 'Years'], right_on=['Regions','Seasons','Years'], how='outer')
    df = pd.merge(df, d.df_byproduct, left_on=['Regions', 'Seasons', 'Years'], right_on=['Regions','Season','Years'], how='outer')

    df.fillna(0.0, inplace=True)

    try:
        df['Industrial'] = -1.0 * df['Industrial']
    except:
        df['Industrial'] = 0.0
        pass

    try:
        df['Electricity'] = -1.0 * df['Electricity']
    except:
        df['Electricity'] = 0.0
        pass
    
    try:
        df['Transportation'] = -1.0 * df['Transportation']
    except:
        df['Transportation'] = 0.0
        pass
    
    try:
        df['Refining'] = -1.0 * df['Refining']  
    except:
        df['Refining'] = 0.0
        pass
    # convert to EMM regions
    # Combine HMM seasons 1 and 3 by setting 1 to 3 and doing a groupby where EMM = 3 
    #df.loc[df['Seasons']==1, 'Seasons']=3 # Combine HMM shoulder seasons by setting 1 to 3
    #df.loc[df['Seasons']==4, 'Seasons']=1 # HMM 4th seasons is EMM 1

    # the groupby
    #df = df.groupby(['Regions', 'Seasons', 'Years'])[['PEM', 'SMR', 'SMR_CCS', 'Electricity','Industrial', 'Refining', 'Transportation','StgInject','StgWithdrawal','Transport_Imports','Transport_Exports']].sum().reset_index(drop=False)
    df.to_csv('data_demand_vs_production_regional.csv')

    figs = [] # list of figures
    colors = ['green','blue','lightblue','pink', 'orange','purple','brown','black', 'black','red', 'red','yellow']

    for region in df.Regions.unique():
         for year in df.Years.unique():
             df_plot = df.loc[(df['Regions']==region) &(df['Years']==year)]
             df_plot.index =df_plot.Seasons
             df_plot = df_plot[['PEM', 'SMR', 'SMR_CCS', 'Electricity','Industrial', 'Transportation','Refining','StgInject','StgWithdrawal','Transport_Imports','Transport_Exports','ByProduct']]
             if not df_plot.empty:
                 plot = df_plot.plot.bar(rot=0, xlabel='Seasons', ylabel='MMTons', title=str(year) + ' ' + str(region), color=colors)
                 plot.legend(bbox_to_anchor=(1.05, 1), loc='upper left')
                 figs.append(plot.get_figure())

         with PdfPages('demandVsProduction_'+str(region)+'.pdf') as pdf:
             for fig in figs:
                 pdf.savefig(fig, bbox_inches='tight') 
         
         figs = [] # reset list of figures

    # all usa
    df = df.groupby(['Seasons', 'Years'])[['PEM', 'SMR', 'SMR_CCS', 'Electricity','Industrial', 'Transportation','Refining','StgInject','StgWithdrawal','Transport_Imports','Transport_Exports','ByProduct']].sum().reset_index(drop=False)
    df.to_csv('data_demand_vs_production_totalUSA.csv')

    for year in df.Years.unique():
        df_plot = df.loc[(df['Years']==year)]
        df_plot.index =df_plot.Seasons
        df_plot = df_plot[['PEM', 'SMR', 'SMR_CCS', 'Electricity','Industrial', 'Transportation','Refining','StgInject','StgWithdrawal','Transport_Imports','Transport_Exports','ByProduct']]
        if not df_plot.empty:
            plot = df_plot.plot.bar(rot=0, xlabel='Seasons', ylabel='MMTons', title=str(year)+' Total USA',color=colors)
            plot.legend(bbox_to_anchor=(1.05, 1), loc='upper left')
            figs.append(plot.get_figure())

    with PdfPages('demandVsProduction_totalUSA.pdf') as pdf:
        for fig in figs:
            pdf.savefig(fig, bbox_inches='tight') 


def plot_generation_consumption(d):
    df_consumption = d.df_electrolyzer_power_consumption
    df_consumption['Years'] = df_consumption['Years'] + 1989

    df_generation = d.df_generation_hourly

    df = pd.merge(df_consumption, df_generation, left_on=['Regions','Season','Hours','Years'], right_on=['EMMReg','season','ECP_Hour','Year'])

    df_plot = df.groupby(['Regions','Season','Years'])[['Power (Mwh)','Generation (Energy)']].sum().reset_index(drop=False)

    figs = [] # list of figures

    for year in df_plot.Years.unique():
        for region in df.Regions.unique():
                df_regional_plot = df_plot.loc[(df_plot['Regions']==region) & (df_plot['Years']==year)]
                if not df_regional_plot.empty:
                    df_regional_plot.index = df_regional_plot.Season
                    df_regional_plot=df_regional_plot[['Power (Mwh)', 'Generation (Energy)']]
                    plot = df_regional_plot.plot.bar(rot=0, xlabel='Seasons', ylabel='Mwhrs', title='Year :'+str(year) + ' Region: '+str(region))
                    figs.append(plot.get_figure())

    with PdfPages('generationAndConsumption.pdf') as pdf:
        for fig in figs:
            pdf.savefig(fig, bbox_inches='tight') 


def write_table_inout(d):
    df = pd.merge(d.hydrogen_storage_injection, d.hydrogen_storage_withdrawal, on=['Regions_','Seasons_','cal_Year'], how='outer')
    df.fillna(0.0, inplace=True)
    df.to_csv('hydrogen_storage_in_out.csv')


def write_table_transportation(d):
    d.df_transporation.to_csv('seasonal_transporation.csv')


def plot_HydrogenVsStorage(d):
    df_electrolyzer_use = d.df_electrolyzer_power_consumption
    df_electrolyzer_use['Years'] = df_electrolyzer_use['Years'] + 1989 # Gwhrs
    df_electrolyzer_use['Power (Mwh)'] = df_electrolyzer_use['Power (Mwh)']/1000.0 # GWhrs
    df_turbine_generation = d.df_generation_hourly # mwhrs 
    df_restore_storage = d.df_storage # Gwhrs
    df_turbine_generation['Generation (Energy)'] = df_turbine_generation['Generation (Energy)']/1000.0

    # combine into a single dataframe
    df = pd.merge(df_electrolyzer_use, df_turbine_generation, left_on=['Regions', 'Season', 'Hours', 'Years'], right_on=['Year','EMMReg','season','ECP_Hour'], how='outer').reset_index(drop=False)
    df = pd.merge(df, df_restore_storage, left_on=['Year','EMMReg','season','ECP_Hour'], right_on=['Year','EMMReg','season','ECP_Hour'], how='outer').reset_index(drop=False)
    
    df['Regions'] = df['Regions'].fillna(df['EMMReg'])
    df['Season'] = df['Season'].fillna(df['season'])
    df['Hours'] = df['Hours'].fillna(df['ECP_Hour'])
    df['Years'] = df['Years'].fillna(df['Year'])

    df = df[['Regions','Season','Hours','Years','Power (Mwh)','Generation (Energy)','Storage (Energy)']].fillna(0.0)
    df = df.groupby(['Regions', 'Season', 'Hours', 'Years'])[['Power (Mwh)','Generation (Energy)', 'Storage (Energy)']].sum().reset_index(drop=False)
    df['Hours'] = df['Hours'].astype(int)
    # for charting purposes we want the electrolyzer use to be negative
    df['Power (Mwh)'] = -1.0 * df['Power (Mwh)']

    figs = []

    years = list(range(2025,2055,5))
    for region in df.Regions.unique():
        for year in years:
            for season in df.Season.unique():
                df_plot = df.loc[(df.Regions == region) & (df.Season == season) & (df.Years == year)]

                df_plot.index = df_plot.Hours
                df_plot = df_plot[['Power (Mwh)', 'Generation (Energy)', 'Storage (Energy)']]
                df_plot.columns = ['Electrolyzer Use', 'H2 Turbine', 'Storage Charge/Discharge']
                if not df_plot.empty:
                    plot = df_plot.plot.bar(rot=0, xlabel='Hours', ylabel='Mwhrs', title='Year :'+str(year) + ' Region: '+str(region) + ' Season: '+str(season), stacked=True)
                    plot.legend(bbox_to_anchor=(1.05, 1), loc='upper left')
                    figs.append(plot.get_figure())

        with PdfPages('hourlyHydrogenVsStorage_'+str(region)+'.pdf') as pdf:
            for fig in figs:
                pdf.savefig(fig, bbox_inches='tight')

        figs = [] 
    
    df.to_csv('hourlyHydrogenConsumption.csv')

    # total USA

    df = df.groupby(['Season', 'Hours', 'Years'])[['Power (Mwh)','Generation (Energy)', 'Storage (Energy)']].sum().reset_index(drop=False)

    years = list(range(2025,2055,5))
    for year in years:
        for season in df.Season.unique():
            df_plot = df.loc[(df.Season == season) & (df.Years == year)]

            df_plot.index = df_plot.Hours
            df_plot = df_plot[['Power (Mwh)', 'Generation (Energy)', 'Storage (Energy)']]
            df_plot.columns = ['Electrolyzer Use', 'H2 Turbine', 'Storage Charge/Discharge']
            if not df_plot.empty:
                plot = df_plot.plot.bar(rot=0, xlabel='Hours', ylabel='Mwhrs', title='Total USA Year :'+str(year) + ' Season: '+str(season), stacked=True)
                plot.legend(bbox_to_anchor=(1.05, 1), loc='upper left')
                figs.append(plot.get_figure())

    with PdfPages('hourlyHydrogenVsStorage_totalUSA.pdf') as pdf:
        for fig in figs:
            pdf.savefig(fig, bbox_inches='tight')


def run(YEAR):
    print("Processing HMM/EMM data")
    # object for storing data across functiions
    d = DataPrep()
    d.df_curtailment = pd.DataFrame()
    d.df_gen = pd.DataFrame()
    d.df_storage = pd.DataFrame()
    d.df_electrolyzer_power_consumption = pd.DataFrame()
    d.df_natgas_consumption = pd.DataFrame() 
    d.df_byproduct = pd.DataFrame()

    # mapping data
    months2seasons(d)
    get_emm_dayweights(d)        

    # loop over years and collect data from restore
    years = list(range(2025,2051,1))

    for year in years:

        d.year = year
        # unpack tarfile for restore given a year
        unpack_restore_tarfile(d)

        # get data from HMM or Restore prior to any analysis
        get_curtailment_data(d)
        get_restore_generation_data(d)
        get_restore_battery_data(d) # exclue pumped hydro 
        get_hmm_electrolyzer_power_consumption(d) # seasonal hourly electricity consumption
        get_natgas_consumption(d)
        get_seasonal_byproductsupply(d)

    get_hmm_transporation(d) 
    get_hydrogen_storage_withdrawal(d)
    get_hydrogen_storage_injection(d)
    get_hydrogen_demand_seasonal(d)
    get_h2_transporation(d)

    #TODO: this needs a _year tag to collect all the years   
    get_seasonal_regional_production(d)

    # analysis
    convert_restore_generation_timeslices(d)
    convert_restore_storage_timeslices(d)
    convert_restore_curtailment(d)

    # g
    os.makedirs('plots',exist_ok=True)
    os.chdir('plots')

    plot_demand_vs_production(d)
    #plot_generation_consumption(d)
    plot_hydrogen_production_by_tech(d)
    write_table_inout(d)
    write_table_transportation(d)
    plot_HydrogenVsStorage(d)

    # TODO: Remove folders that we unzipped?
    print("")



if __name__ == "__main__":
    PATH = r'U:\output\vla\ref2025\d012425d'
    os.chdir(PATH)
    YEAR = 2036
    run(YEAR)
    print('finished')
    print("")
