# import logging
import numpy as np
import pandas as pd
import os
# import sys
import warnings
# import timeit
# import cProfile
# import traceback

warnings.filterwarnings("ignore")


def read_data(path, cmd_line_year, iteration, input_file, ephrts_print_file):
    """
    Reads data from output folder in AIMMS project,
    does some light sorting and prepares name structures for each dataframe.
    writes these to debug files if production turned on.

    Parameters
    ----------
    path: str
        ephrts output directory path
    cmd_line_year: str or int
        ephrts run year
    iteration: str or int
        ephrts iteration (1 or 2)
    input_file: str
        parameter input file path that was used for ephrts model
    ephrts_print_file: str
        log file path

    Returns
    -------
    df_demand: pd.DataFrame
        seasonal hydrogen demand data by region (kg)
    df_h2_produced: pd.DataFrame
        amount of hydrogen produced by seasons, regions, hours, technology (kg)
    df_storage: pd.DataFrame
        seasonal storage charge level data by region (kg)
    df_transfer: pd.DataFrame
        seasonal transfer data by import and export region (kg)
    df_capacity: pd.DataFrame
        hydrogen production capacity by regions and technology (kW)
    df_cost: pd.DataFrame
        hydrogen production cost data by seasons, regions, hours, technology ($/kg)
    df_transfer_cost: pd.DataFrame
        cost to transfer between regions by import and export regions ($/kg)
    df_D_Fuel: pd.DataFrame
        hydrogen generation by seasons, regions, and hours (MWh)
    cost_storage: float
        per-season cost to store hydrogen ($/kg)
    storage_capacity_additions_cost: float
        total capital costs for additional storage capacity ($/kg)
    df_storage_capacity: pd.DataFrame
        total storage capacity (kg)
    production_capacity_costs: pd.DataFrame
        total capital cost for additional h2 production capacity ($/kW)
    total_cost_noadd: float
        total cost if we DO NOT add capital cost of electrolyzers ($)
    total_cost_add: float
        total cost if we DO add capital cost of electrolyzers ($)
    """

    df_demand = pd.read_csv(path + "/demand_hydrogen.csv")
    df_demand.sort_values(by=['seasons', 'regions'], ascending=[True, True], na_position='first',
                          inplace=True)

    df_h2_produced = pd.read_csv(path + "/H.csv")
    df_h2_produced.rename(columns={'H': 'H2_Produced'}, inplace=True)
    df_h2_produced.sort_values(by=['seasons', 'regions', 'hours', 'technology'], ascending=[True, True, True, True],
                               na_position='first',
                               inplace=True)

    df_storage = pd.read_csv(path + "/S.csv")
    df_storage.rename(columns={'regions': 'Rgn_Import', 'regions.1': 'Rgn_Export'}, inplace=True)

    df_transfer = pd.read_csv(path + "/T.csv")
    df_transfer.rename(columns={'T': 'H2_Transfer', 'regions': 'Rgn_Export', 'regions.1': 'Rgn_Import'}, inplace=True)

    df_capacity = pd.read_csv(path + "/h2_production_capacity.csv")
    df_capacity.sort_values(by=['regions', 'technology'], ascending=[True, True], na_position='first',
                            inplace=True)

    df_cost = pd.read_csv(path + "/cost_hydrogen.csv")
    df_cost.sort_values(by=['seasons', 'regions', 'hours', 'technology'], ascending=[True, True, True, True],
                        na_position='first', inplace=True)

    df_transfer_cost = pd.read_csv(path + "/transfer_cost.csv")
    df_transfer_cost.rename(columns={'regions': 'Rgn_Export', 'regions.1': 'Rgn_Import'}, inplace=True)

    df_hydrogen_capacity_additions_cost = pd.read_csv(path + "/hydrogen_capacity_additions_cost.csv")
    df_D_Fuel = pd.read_csv(path + "/D_Fuel.csv")

    # read from hmm input file
    with open(input_file) as f:
        hmm_data_input = f.readlines()

    cost_storage_str = str([x for x in hmm_data_input if 'storage_cost' in x])
    cost_storage_str = cost_storage_str.replace(':=', ',').replace(';', ',')
    cost_storage_str = cost_storage_str.split(',')
    cost_storage = float(cost_storage_str[1].strip())

    storage_capacity_additions_cost_str = str(([x for x in hmm_data_input if 'storage_capacity_additions_cost' in x]))
    storage_capacity_additions_cost_str = storage_capacity_additions_cost_str.replace(':=', ',').replace(';', ',')
    storage_capacity_additions_cost_str = storage_capacity_additions_cost_str.split(',')
    storage_capacity_additions_cost = float(storage_capacity_additions_cost_str[1].strip())

    # read from aimms print file
    with open(ephrts_print_file) as f2:
        aimms_print_data = f2.readlines()

    total_cost_noadd_str = str([x for x in aimms_print_data if 'total_cost_noadd' in x])
    total_cost_noadd_str = total_cost_noadd_str.replace(':=', ',').replace(';', ',')
    total_cost_noadd_str = total_cost_noadd_str.split(',')
    # -2 because it chooses the last value if you ran aimms multiple times
    total_cost_noadd = float(total_cost_noadd_str[-2].strip())

    total_cost_add_str = str([x for x in aimms_print_data if 'total_cost_add' in x])
    total_cost_add_str = total_cost_add_str.replace(':=', ',').replace(';', ',')
    total_cost_add_str = total_cost_add_str.split(',')
    total_cost_add = float(total_cost_add_str[-2].strip())

    # production capacity additions cost
    df_final_storage_capacity = pd.read_csv(path + "/SCAPMAX.csv")
    df_initial_storage_capacity = pd.read_csv(path + "/SCAPMAX_INT.csv")
    df_storage_capacity = pd.merge(df_initial_storage_capacity,
                                   df_final_storage_capacity, on=['regions']).reset_index(drop=True)
    df_final_production_capacity = pd.read_csv(path + "/h2_production_capacity.csv")
    df_initial_production_capacity = pd.read_csv(path + "/h2_production_capacity_int.csv")
    df_production_capacity = pd.merge(df_initial_production_capacity,
                                      df_final_production_capacity, on=['regions', 'technology']).reset_index(drop=True)
    production_capacity_costs = pd.read_csv(path + "/hydrogen_capacity_additions_cost.csv")

    debug = True
    # output debug to log file
    if debug:
        debug_path = os.getcwd() + '/ephrts_debug'
        if os.path.isdir(debug_path):
            print(debug_path)
            print("Ephrts Folder Exists")
        else:
            print("Ephrts Folder Does not Exists")
            print(debug_path)
            os.mkdir(debug_path)

        df_demand.to_csv(debug_path + '/df_demand_' + cmd_line_year + "_" + iteration + ".txt")
        df_h2_produced.to_csv(debug_path + '/df_h2_produced' + cmd_line_year + "_" + iteration + ".txt")
        df_capacity.to_csv(debug_path + '/df_capacity' + cmd_line_year + "_" + iteration + ".txt")
        df_cost.to_csv(debug_path + '/df_cost' + cmd_line_year + "_" + iteration + ".txt")
        df_transfer.to_csv(debug_path + '/df_transfer' + cmd_line_year + "_" + iteration + ".txt")
        df_transfer_cost.to_csv(debug_path + '/df_transfer_cost' + cmd_line_year + "_" + iteration + ".txt")
        df_hydrogen_capacity_additions_cost.to_csv(
            debug_path + '/df_hydrogen_capacity_additions_cost' + cmd_line_year + "_" + iteration + ".txt")
        df_storage.to_csv(debug_path + '/df_storage' + cmd_line_year + "_" + iteration + ".txt")

    return df_demand, df_h2_produced, df_storage, df_transfer, df_capacity, \
        df_cost, df_transfer_cost, df_D_Fuel, cost_storage, storage_capacity_additions_cost, df_storage_capacity, \
        df_production_capacity, production_capacity_costs, total_cost_noadd, total_cost_add


def check_demand(df):
    """
    Checks for demand from ephrts, throws error if no demand data.

    Parameters
    ----------
    df: pd.DataFrame
        demand data dataframe

    Returns
    -------
    none

    """
    if df.empty:
        raise RuntimeError(
            "No Demand found for hydrogen model, crash since there should always be a minimum demand found")


# functions finding seasonal values

def interregional_transfers_sum(df_transfer, df_transfer_cost):
    """
    Finds total net export by region/season

    Parameters
    ----------
    df_transfer: pd.DataFrame
        seasonal transfer data by import and export region (kg)
    df_transfer_cost: pd.DataFrame
        cost of transfers by import and export region ($/kg)

    Returns
    -------
    df_transfers_net_export: pd.DataFrame
        net exports by region/season (kg)
    df_transfers: pd.DataFrame
        seasonal transfers including transfer_cost values (kg and $/kg)
    """

    if df_transfer.empty:
        return pd.DataFrame(), pd.DataFrame()

    df_transfers = pd.merge(df_transfer, df_transfer_cost, on=['Rgn_Export', 'Rgn_Import']).reset_index(drop=True)
    df_transfers = df_transfers.sort_values(by=['seasons'], ascending=[True]).reset_index(drop=True)

    df_transfers_import = df_transfers.copy()
    df_transfers_import.rename(columns={'Rgn_Import': 'region', 'Rgn_Export': 'Rgn_Import'}, inplace=True)
    df_transfers_import.rename(columns={'region': 'Rgn_Export'}, inplace=True)
    df_transfers_import.loc[:, 'H2_Transfer'] = -1.0 * df_transfers_import.H2_Transfer

    df_transfers_net_export = df_transfers.groupby(['seasons', 'Rgn_Export'], as_index=False).agg(
        {'H2_Transfer': 'sum'})
    df_transfers_net_import = df_transfers.groupby(['seasons', 'Rgn_Import'], as_index=False).agg(
        {'H2_Transfer': 'sum'})
    df_transfers_net_import.rename(columns={'H2_Transfer': 'Import', 'Rgn_Import': 'Rgn_Export'}, inplace=True)

    df_transfers_net_export = pd.merge(df_transfers_net_export, df_transfers_net_import, on=['seasons', 'Rgn_Export'],
                                       how='outer').fillna(0)
    df_transfers_net_export.loc[:, 'net_export'] = df_transfers_net_export['H2_Transfer'] - df_transfers_net_export[
        'Import']

    return df_transfers_net_export, df_transfers


def get_seasonal_setup(df_transfer, df_transfer_cost, df_demand, df_storage, df_h2_produced,
                       seasons, regions, zero_round):
    """
    Computes an overview of transfer, storage, production at a seasonal level

    Parameters
    ----------
    df_transfer: pd.DataFrame
        seasonal transfer data by import and export region (kg)
    df_transfer_cost: pd.DataFrame
        cost of transfers by import and export regions ($/kg)
    df_demand: pd.DataFrame
        seasonal hydrogen demand data by region (kg)
    df_storage: pd.DataFrame
        seasonal storage level data by region (kg)
    df_h2_produced: pd.DataFrame
        amount of hydrogen produced by seasons, regions, hours, technology (kg)
    seasons: list
    regions: list
    zero_round: float

    Returns
    -------
    df_h2_seasonal: pd.DataFrame

    df_transfers_net_export: pd.DataFrame

    df_transfers_left: pd.DataFrame

    """

    # looking at the regions that are producing to meet demand,
    # and creating steps with the hours that hydrogen is produced for a given hour to meet demand
    df_total_produced = df_h2_produced.groupby(['seasons', 'regions'], as_index=False).agg({'H2_Produced': 'sum'})

    # find net exports of each region on a seasonal basis
    df_transfers_net_export, df_transfers = interregional_transfers_sum(df_transfer, df_transfer_cost)
    df_transfers_net_export.rename(columns={'Rgn_Export': 'regions', 'H2_Transfer': 'Export'}, inplace=True)

    # storage, is looking at cases without storage-transfer component,
    # and just looking at simple storage where the difference in the storage amounts
    # uses the difference of the seasonal storage amount to deduce what was consumed, that is taken out of storage
    df_storage_steps_seasonal = get_storage_steps_seasonal(df_storage, seasons, regions)
    # find seasonal production details
    df_h2_seasonal = get_seasonal_summary(df_total_produced, df_demand, df_storage_steps_seasonal,
                                          df_transfers_net_export)

    # if there is transfer, find seasonal details
    if df_transfers_net_export.empty:
        df_transfers_left = pd.DataFrame()
    else:
        df_transfers_left = get_transfer_seasonal(df_h2_seasonal, df_transfers, zero_round)

    return df_h2_seasonal, df_transfers_net_export, df_transfers_left


def get_seasonal_summary(df_total_produced, df_demand, df_storage_steps, df_transfers_net_export):
    """
    Compare production and demand by region/season, calculates columns including:
    transfer_stored_out, stored_transfer_out, missing demand (storage), and local production

    Parameters
    ----------
    df_total_produced: pd.DataFrame
    df_demand: pd.DataFrame
        seasonal hydrogen demand data by region
    df_storage_steps: pd.DataFrame
        net seasonal storage charge (charge & dispatch) behavior data by region
    df_transfers_net_export: pd.DataFrame

    Returns
    -------
    df_h2_seasonal: pd.DataFrame

    """

    # compare demand and production
    df_h2_seasonal = pd.merge(df_total_produced, df_demand, on=['regions', 'seasons'], how='outer').fillna(0)
    # local production is all production that doesn't exceed demand
    df_h2_seasonal.loc[:, 'local_produced_season'] = df_h2_seasonal[['H2_Produced', 'demand_hydrogen']].min(axis=1)
    # get leftover demand not met by local production
    df_h2_seasonal.loc[:, 'demand_leftover'] = df_h2_seasonal['demand_hydrogen'] - \
        df_h2_seasonal['local_produced_season']
    # get leftover production that exceeds demand
    df_h2_seasonal.loc[:, 'produced_leftover_season'] = df_h2_seasonal['H2_Produced'] - \
                                                        df_h2_seasonal['local_produced_season']

    if not df_storage_steps.empty:
        # compare with storage values
        df_h2_seasonal = pd.merge(df_h2_seasonal, df_storage_steps, on=['regions', 'seasons'], how='outer').fillna(0)
    else:
        df_h2_seasonal.loc[:, 'storage'] = 0.0

    # only do most of the caluclations if there is transfer
    if not df_transfers_net_export.empty:
        df_h2_seasonal = pd.merge(df_h2_seasonal, df_transfers_net_export,
                                  left_on=['regions', 'seasons'],
                                  right_on=['regions', 'seasons'], how='outer').fillna(0)
        df_h2_seasonal.loc[:, 'transfer_in'] = df_h2_seasonal[['net_export', 'demand_leftover']].abs().min(axis=1)
        df_h2_seasonal.loc[:, 'transfer_stored_out'] = np.maximum(0, -1.0 * (
                df_h2_seasonal['transfer_in'] + df_h2_seasonal['net_export']))
        df_h2_seasonal.loc[:, 'missing_demand'] = df_h2_seasonal['demand_leftover'] - df_h2_seasonal['transfer_in']
        df_h2_seasonal.loc[:, 'transfer_out'] = df_h2_seasonal[['net_export', 'produced_leftover_season']].abs().min(
            axis=1)
        df_h2_seasonal.loc[:, 'stored_transfer_out'] = np.maximum(0, df_h2_seasonal['net_export']) - df_h2_seasonal[
            'transfer_out']
        df_h2_seasonal.loc[:, 'transfer'] = -1.0 * df_h2_seasonal.net_export
        df_h2_seasonal = df_h2_seasonal[
            ['regions', 'seasons', 'local_produced_season', 'storage', 'transfer', 'stored_transfer_out',
             'missing_demand', 'transfer_stored_out']]
    else:
        # still need storage values if there is no transfer
        df_h2_seasonal.loc[:, 'missing_demand'] = df_h2_seasonal['demand_leftover']

    return df_h2_seasonal


def get_storage_steps_seasonal(df_storage, seasons, regions):
    """
    Finds storage behaviour (charge/discharge amount) over a season

    Parameters
    ----------
    df_storage: pd.DataFrame
        seasonal storage charge level data by region (kg)
    seasons: list
    regions: list

    Returns
    -------
    df_storage_steps: pd.DataFrame
        net seasonal storage charge (charge & dispatch) behavior data by region (kg)
    """

    df_storage.columns = ['seasons', 'regions', 'S']

    if df_storage.empty:
        return pd.DataFrame()

    # initialize empty pd.DataFrame
    df_storage_steps = pd.DataFrame({'seasons': np.repeat(seasons, len(regions)),
                                     'regions': regions * len(seasons)})
    # merge storge data with blank df
    df_storage_steps = pd.merge(left=df_storage_steps, right=df_storage,
                                on=['seasons', 'regions'], how='outer').reset_index(drop=True).fillna({'S': 0.0})
    # this step finds the difference between each season to get amount of storage discharged
    df_storage_steps.loc[:, 'diff'] = -1.0 * df_storage_steps.groupby('regions')['S'].diff()
    # if there is no previous season to compare to, assume the previous season's SOC was 0
    df_storage_steps.loc[df_storage_steps['diff'].isna(), 'diff'] \
        = -1.0 * df_storage_steps[df_storage_steps['diff'].isna()]['S']
    # storage is the charge from production (negative)/ discharge (positive)
    df_storage_steps.loc[:, 'storage'] = df_storage_steps['diff']
    df_storage_steps = df_storage_steps[['seasons', 'regions', 'storage']].reset_index(drop=True)
    return df_storage_steps


def get_transfer_seasonal(df_h2_seasonal, df_transfers, zero_round):
    """
    This function separates seasonal transfers into
    transfers without storage, transfer_stored, stored_transfer
    and matches the transfers to specific import/export regions.

    Parameters
    ----------
    df_h2_seasonal: pd.DataFrame
        seasonal storage data frame
    df_transfers:: pd.DataFrame
        transfers data frame
    zero_round: float

    Returns
    -------
    df_transfers_left: pd.DataFrame
        transfers left after accounting for storage
    """

    # this df is where storage_transfer exists
    # (after storage, in the season of the transfer from the export region)
    df_trans_export = df_h2_seasonal[df_h2_seasonal['stored_transfer_out'] > zero_round][
        ['seasons', 'regions', 'stored_transfer_out']]
    # this df is where transfer_storage exists
    # (in the season of the transfer, where the import region is sending it to be stored)
    df_trans_import = df_h2_seasonal[df_h2_seasonal['transfer_stored_out'] > zero_round][
        ['seasons', 'regions', 'transfer_stored_out']]

    # initialize leftover df
    df_transfers_left = df_transfers.copy()
    df_transfers_left.loc[:, 'direct_transfer'] = df_transfers_left.H2_Transfer

    # separate out storage_transfer
    df_transfer_reg = pd.merge(df_transfers_left, df_trans_export,
                               right_on=['regions', 'seasons'], left_on=['Rgn_Export', 'seasons'],
                               how='left').fillna(0)
    df_transfer_reg = get_leftover(df_transfer_reg,
                                   total_in='stored_transfer_out', total_out='stored_transfer_out',
                                   hourly_in='direct_transfer', hourly_left='H2_Transfer_leftover',
                                   groupby=['Rgn_Export', 'seasons'])
    df_transfer_reg = df_transfer_reg[
        ['seasons', 'Rgn_Export', 'Rgn_Import', 'transfer_cost', 'H2_Transfer_leftover', 'stored_transfer_out']]

    df_transfers_left.loc[:, 'direct_transfer'] = list(df_transfer_reg.H2_Transfer_leftover)
    df_transfers_left.loc[:, 'stored_transfer_out'] = list(df_transfer_reg.stored_transfer_out)

    # separate out transfer_stored
    df_transfer_reg = pd.merge(df_transfers_left, df_trans_import,
                               left_on=['Rgn_Import', 'seasons'], right_on=['regions', 'seasons'],
                               how='left').fillna(0)
    df_transfer_reg = get_leftover(df_transfer_reg,
                                   total_in='transfer_stored_out', total_out='transfer_stored_out',
                                   hourly_in='direct_transfer', hourly_left="H2_Transfer_leftover",
                                   groupby=['Rgn_Import', 'seasons'])
    df_transfer_reg = df_transfer_reg[
        ['seasons', 'Rgn_Export', 'Rgn_Import', 'transfer_cost', 'H2_Transfer_leftover', 'transfer_stored_out']]

    # what's left is transfer without storage
    df_transfers_left.loc[:, 'direct_transfer'] = list(df_transfer_reg.H2_Transfer_leftover)
    df_transfers_left.loc[:, 'transfer_stored_out'] = list(df_transfer_reg.transfer_stored_out)

    return df_transfers_left


# functions for creating steps

def get_production_steps(df_h2_seasonal, df_cost, df_h2_produced, keep_cols, zero_round):
    """
    Finds local production steps

    Parameters
    ----------
    df_h2_seasonal: pd.DataFrame

    df_cost: pd.DataFrame
        hydrogen production cost data by seasons, regions, hours, technology ($/kg)
    df_h2_produced: pd.DataFrame
        amount of hydrogen produced by seasons, regions, hours, technology (kg)
    keep_cols: list
    zero_round: float

    Returns
    -------
    df_production: pd.DataFrame
        data containing the hourly amount of h2 which was produced,
        then used to meet demand in the same region (kg)
    df_h2_hourly: pd.DataFrame

    """

    # next, move production around to proper hours
    df_h2_produced = pd.merge(left=df_h2_produced, right=df_cost,
                              on=['seasons', 'regions', 'hours', 'technology'],
                              how='inner')
    # sort by lowest price for a given hour
    df_h2_produced = df_h2_produced.sort_values(by=['cost_hydrogen'],
                                                ascending=[True]).reset_index(drop=True)
    # previously found total seasonal local production, so use that to determine which hours are cheapest
    df_production = pd.merge(df_h2_produced,
                             df_h2_seasonal[['regions', 'seasons', 'local_produced_season']],
                             on=['seasons', 'regions'], how='outer')
    # find amount of local production by hour up to local_produced_season
    # find leftover production
    df_production = get_leftover(df_production,
                                 total_in='local_produced_season', total_out='H2_Produced',
                                 hourly_in='H2_Produced', hourly_left='produced_leftover',
                                 groupby=['regions', 'seasons']).drop(columns='local_produced_season')

    # clean up df_production and save leftover production to df_h2_hourly which is used in all subsequent steps
    df_production.loc[:, 'type'] = 'produced'
    df_h2_hourly = df_production[
        ['regions', 'seasons', 'hours', 'technology', 'cost_hydrogen', 'produced_leftover']].copy()
    # drop unneeded columns and only keep hours with production
    df_production = df_production[keep_cols][df_production.H2_Produced > zero_round]

    df_h2_hourly.rename(columns={'seasons': 'seasons_in'}, inplace=True)
    # sort costs
    df_h2_hourly = df_h2_hourly.sort_values(by=['regions', 'seasons_in', 'cost_hydrogen'],
                                            ascending=[True, False, True]).reset_index(drop=True)

    return df_production, df_h2_hourly


def get_storage_steps(df_h2_hourly, df_h2_seasonal, storage_costs, season_diff_range, zero_round, keep_cols):
    """
    Finds local storage steps.
    updates df_h2_hourly (col: produced_leftover) and df_h2_seasonal (cols: local_storage, missing_demand)
    because they are mutable

    Parameters
    ----------
    df_h2_hourly: pd.DataFrame
    df_h2_seasonal: pd.DataFrame
    storage_costs: float
        per-season cost to store hydrogen ($/kg)
    season_diff_range: list
    zero_round: float
    keep_cols: list

    Returns
    -------
    df_storage_steps: pd.DataFrame
        data containing the hourly amount of h2 which was produced, then stored,
        then used to meet demand (kg) and updates its total cost column (cost_hydrogen)
        accounting for all storage/production costs ($/kg)
    """
    # initialize
    df_storage_steps = pd.DataFrame()
    df_h2_seasonal.loc[:, 'local_storage'] = 0.0

    # loop through the difference between seasons.
    # first, we see if the previous season has the excess production that would
    # have charged the storage. next, we see if the 2nd previous season has the production, etc.
    for seas_diff in season_diff_range:
        # missing_demand = amount of demand that was satisfied using (any kind of) storage
        df_storage_sub = df_h2_seasonal[df_h2_seasonal['missing_demand'] > zero_round][
            ['seasons', 'regions', 'missing_demand', 'local_storage']]
        df_storage_sub.rename(columns={'seasons': 'seasons_out'}, inplace=True)
        # test if previous (#) season from storage has matching leftover production
        df_storage_sub.loc[:, 'seasons_in'] = df_storage_sub.seasons_out - seas_diff
        df_total_leftover = df_h2_hourly[(df_h2_hourly.produced_leftover > zero_round)][
            ['regions', 'seasons_in', 'hours', 'technology', 'cost_hydrogen', 'produced_leftover']]

        # only continue if neither of these df's are blank
        if (len(df_storage_sub) > 0) & (len(df_total_leftover) > 0):
            # try to match storage with production
            df_storage_shift = pd.merge(
                df_total_leftover,
                df_storage_sub,
                on=['regions', 'seasons_in'],
                how='left').fillna(0)

            df_storage_shift = get_leftover(df_storage_shift,
                                            total_in='missing_demand',
                                            total_out='missing_demand',
                                            hourly_in='produced_leftover',
                                            hourly_left='produced_leftover',
                                            groupby=['regions', 'seasons_in'])

            # update leftover production after subtracting off local storage amount
            df_h2_hourly.loc[(df_h2_hourly.produced_leftover > zero_round), 'produced_leftover'] \
                = list(df_storage_shift.produced_leftover)

            # only keep rows that have local storage
            df_storage_shift = df_storage_shift[(df_storage_shift.missing_demand > zero_round)]
            df_storage_shift.rename(columns={'missing_demand': 'local_storage_update'}, inplace=True)

            # update leftover missing_demand after subtracting off local storage amount
            df_storage_steps_discharge = pd.merge(df_storage_sub,
                                                  df_storage_shift.groupby(['regions', 'seasons_out'])[
                                                      'local_storage_update'].sum(),
                                                  on=['regions', 'seasons_out'], how='left').fillna(0)
            df_storage_sub.loc[:, 'local_storage'] = \
                df_storage_sub.local_storage + list(df_storage_steps_discharge.local_storage_update)
            df_storage_sub.loc[:, 'missing_demand'] = \
                np.maximum(0.0, df_storage_sub.missing_demand -
                           list(df_storage_steps_discharge.local_storage_update))
            # updating 'missing_demand' to subtract off the amount of local_storage
            # (this variable is used again later) in df_h2_seasonal
            df_h2_seasonal.loc[df_h2_seasonal.missing_demand > zero_round, 'local_storage'] = \
                list(df_storage_sub.local_storage)
            df_h2_seasonal.loc[df_h2_seasonal.missing_demand > zero_round, 'missing_demand'] = \
                list(df_storage_sub.missing_demand)

            # add on to output df
            df_storage_steps = pd.concat([df_storage_steps, df_storage_shift]).reset_index(drop=True)

    if not df_storage_steps.empty:
        df_storage_steps.loc[:, 'local_storage'] = df_storage_steps.local_storage_update

        # update cost_hydrogen based on storage costs
        df_storage_steps.loc[:, 'cost_hydrogen'] = df_storage_steps.cost_hydrogen \
                                                   + storage_costs * (
                                                           df_storage_steps.seasons_out - df_storage_steps.seasons_in)
        # refine df_storage_steps to match format
        df_storage_steps.loc[:, 'type'] = 'storage'
        df_storage_steps.rename(columns={'local_storage': 'H2_Produced', 'seasons_out': 'seasons'},
                                inplace=True)
        df_storage_steps = df_storage_steps[keep_cols]

    return df_storage_steps


def get_interregional_transfer_steps(df_h2_hourly, df_transfers_left, regions, zero_round, keep_cols):
    """
    finds transfer without storage.
    updates df_h2_hourly (col: produced_leftover) because it's mutable

    Parameters
    ----------
    df_h2_hourly: pd.DataFrame

    df_transfers_left: pd.DataFrame
    regions: list
    zero_round: float
    keep_cols: list


    Returns
    -------
    df_interregional_transfers: pd.DataFrame

    """
    # initialize output df
    df_interregional_transfers = pd.DataFrame()
    # loop through each import region to trace its original production info
    for reg in regions:
        # this contains total direct transfers
        df_transfer_reg = df_transfers_left[df_transfers_left.Rgn_Import == reg].reset_index(drop=True)
        df_h2_hourly_tmp = df_h2_hourly[(df_h2_hourly.produced_leftover > zero_round)].reset_index(drop=True)
        df_h2_hourly_tmp.rename(columns={'regions': 'Rgn_Export', 'seasons_in': 'seasons'}, inplace=True)
        if len(df_transfer_reg) > 0:
            # map total direct transfers to leftover production by export region
            df_transfer_reg = pd.merge(df_h2_hourly_tmp, df_transfer_reg,
                                       on=['seasons', 'Rgn_Export'],
                                       how='left').fillna(0)
            # find hourly direct transfers and update leftover production
            df_transfer_reg = get_leftover(df_transfer_reg,
                                           total_in='direct_transfer', total_out='direct_transfer',
                                           hourly_in='produced_leftover',
                                           hourly_left='produced_leftover',
                                           groupby=['Rgn_Export', 'seasons'])
            # update produced_leftover in df_h2_hourly for future use
            df_h2_hourly.loc[df_h2_hourly.produced_leftover > zero_round, 'produced_leftover'] = list(
                df_transfer_reg.produced_leftover)
            # only keep if there is a direct transfer
            df_transfer_reg = df_transfer_reg[df_transfer_reg.direct_transfer > zero_round]
            # add to output df
            df_interregional_transfers = pd.concat([df_interregional_transfers, df_transfer_reg]).reset_index(drop=True)

    # clean up output df to match formatting
    # df_interregional_transfers.drop(columns=['regions'], inplace=True)
    df_interregional_transfers.rename(columns={'Rgn_Import': 'regions', 'direct_transfer': 'H2_Produced'},
                                      inplace=True)
    df_interregional_transfers.loc[:, 'type'] = 'transfer'
    df_interregional_transfers.loc[:, 'cost_hydrogen'] = \
        df_interregional_transfers.cost_hydrogen + df_interregional_transfers.transfer_cost

    df_interregional_transfers = df_interregional_transfers[keep_cols]
    return df_interregional_transfers


def get_storage_to_transfer_steps(df_h2_hourly, df_transfers_left, storage_costs, season_diff_range,
                                  regions, zero_round, keep_cols):
    """
    finds storage to transfer.
    updates df_h2_hourly (col: produced_leftover) and df_transfers_left (col: stored_transfer_out)
    because they're mutable

    Parameters
    ----------
    df_h2_hourly: pd.DataFrame

    df_transfers_left: pd.DataFrame

    storage_costs: float
        per-season cost to store hydrogen ($/kg)
    regions: list
    zero_round: float
    keep_cols: list

    season_diff_range: list

    Returns
    -------
    df_storage_to_transfer_steps: pd.DataFrame
        data containing the hourly amount of h2 which was produced, then stored, then transferred
        to another region, then used to meet demand (kg) and updates its total cost column (cost_hydrogen)
        accounting for all transfer/storage/production costs ($/kg)
    """
    # initialize output df
    df_storage_to_transfer_steps = pd.DataFrame()

    # loop through all seasonal differences
    for seas_diff in season_diff_range:
        # loop through all import regions
        for reg in regions:
            df_transfers_reg = df_transfers_left[(df_transfers_left.Rgn_Import == reg)].reset_index(drop=True)
            df_h2_hourly_tmp = df_h2_hourly[(df_h2_hourly.produced_leftover > zero_round)].rename(
                columns={'regions': 'Rgn_Export'}).reset_index(drop=True)
            if len(df_transfers_reg) > 0:
                # backtrack the season from the discharge season to find a charge match
                df_transfers_reg.loc[:, 'seasons_in'] = df_transfers_reg.seasons - seas_diff
                df_storage_transfer = pd.merge(
                    df_h2_hourly_tmp,
                    df_transfers_reg,
                    on=['Rgn_Export', 'seasons_in'],
                    how='left').fillna(0)
                # find where stored_transfer was produced, and update produced_leftover
                df_storage_transfer = get_leftover(df_storage_transfer,
                                                   total_in='stored_transfer_out', total_out='stored_transfer',
                                                   hourly_in='produced_leftover', hourly_left='produced_leftover',
                                                   groupby=['Rgn_Export', 'seasons_in'])
                df_h2_hourly.loc[(df_h2_hourly.produced_leftover > zero_round), 'produced_leftover'] = list(
                    df_storage_transfer.produced_leftover)

                # updating remaining stored_transfer_out in df_transfers_left
                # by subtracting off discovered stored_transfer
                df_transfers_update = pd.merge(df_transfers_reg,
                                               df_storage_transfer.groupby(
                                                   ['Rgn_Export', 'Rgn_Import', 'seasons_in', 'seasons'])[
                                                   'stored_transfer'].sum().reset_index(),
                                               on=['Rgn_Export', 'Rgn_Import', 'seasons'],
                                               how='left').fillna(0)
                df_transfers_left.loc[(df_transfers_left.Rgn_Import == reg), 'stored_transfer_out'] = \
                    list(df_transfers_update.stored_transfer_out - df_transfers_update.stored_transfer)

                # only keep if there is stored_transfer
                df_storage_transfer = df_storage_transfer[df_storage_transfer.stored_transfer > zero_round]
                # add on to final df
                df_storage_to_transfer_steps = pd.concat(
                    [df_storage_to_transfer_steps, df_storage_transfer]).reset_index(drop=True)
    # update cost using storage, transfer, and production
    df_storage_to_transfer_steps.loc[:, 'cost_hydrogen'] = df_storage_to_transfer_steps.cost_hydrogen \
                                                           + storage_costs * (
                                                                   df_storage_to_transfer_steps.seasons -
                                                                   df_storage_to_transfer_steps.seasons_in) \
                                                           + df_storage_to_transfer_steps.transfer_cost
    # clean up df to match template
    # df_storage_to_transfer_steps.drop(columns=['regions'], inplace=True)
    df_storage_to_transfer_steps.rename(columns={'Rgn_Import': 'regions', 'stored_transfer': 'H2_Produced'},
                                        inplace=True)
    df_storage_to_transfer_steps.loc[:, 'type'] = 'stored_transfer'
    df_storage_to_transfer_steps = df_storage_to_transfer_steps[keep_cols]
    return df_storage_to_transfer_steps


def get_transfer_to_storage_steps(df_h2_hourly, df_h2_seasonal, df_transfers_left, storage_costs,
                                  season_diff_range, regions, zero_round, keep_cols):
    """
    finds transfer to storage.
    updates df_h2_hourly (col: produced_left), df_h2_seasonal (col: missing demand),
    df_transfers_left (col: transfer_stored_out) because they're mutable

    Parameters
    ----------
    df_h2_hourly: pd.DataFrame

    df_h2_seasonal: pd.DataFrame

    df_transfers_left: pd.DataFrame
    season_diff_range: list
    regions: list
    zero_round: float
    keep_cols: list

    storage_costs: float
        per-season cost to store hydrogen ($/kg)

    Returns
    -------
    df_transfer_to_storage_steps: pd.DataFrame
        data containing the hourly amount of h2 which was produced, then transferred to another region,
        then stored, then used to meet demand (kg) and updates its total cost column (cost_hydrogen)
        accounting for all transfer/storage/production costs ($/kg)
    """
    # initialize output df
    df_transfer_to_storage_steps = pd.DataFrame()
    # loop through each seasonal difference
    for seas_diff in season_diff_range:
        # loop through each import region
        for reg in regions:
            # first, match the storage to transfer_storage_out seasonally
            # create df with storage and in matching import region
            df_storage_out = df_h2_seasonal[
                (df_h2_seasonal.missing_demand > zero_round) &
                (df_h2_seasonal.regions == reg)].rename(
                columns={'regions': 'Rgn_Import'}).reset_index(drop=True)[[
                'Rgn_Import', 'seasons', 'missing_demand']]
            # create df with transfer_stored_out and in matching import region
            df_transfer_reg = df_transfers_left[(df_transfers_left.Rgn_Import == reg)
                                                & (df_transfers_left.transfer_stored_out > zero_round)].rename(
                columns={'seasons': 'seasons_in'}).reset_index(drop=True)
            # try possible backtrack seasons for storage charging
            df_storage_out.loc[:, 'seasons_in'] = df_storage_out.seasons - seas_diff
            df_storage_out.rename(columns={'seasons': 'seasons_out'}, inplace=True)

            if (len(df_storage_out) > 0) & (len(df_transfer_reg) > 0):
                # merge storage and transfer_stored_out dfs to find original storage charging
                df_transfer_storage_season = pd.merge(df_storage_out,
                                                      df_transfer_reg,
                                                      on=['Rgn_Import', 'seasons_in'],
                                                      how='left').fillna(0)

                df_transfer_storage_season = get_leftover(df_transfer_storage_season,
                                                          total_in='transfer_stored_out',
                                                          total_out='transfer_stored_out',
                                                          hourly_in='missing_demand',
                                                          hourly_left='missing_demand_leftover',
                                                          groupby=['Rgn_Import', 'seasons_in'])
                # update missing_demand (storage) value in df_h2_seasonal
                df_h2_seasonal.loc[(df_h2_seasonal.missing_demand > zero_round) & (
                        df_h2_seasonal.regions == reg), 'missing_demand'] = list(
                    df_transfer_storage_season.missing_demand_leftover)
                # only keep transfer_stored_out rows
                df_transfer_storage_season = \
                    df_transfer_storage_season[df_transfer_storage_season.transfer_stored_out > zero_round][
                        ['Rgn_Import', 'Rgn_Export', 'seasons_out', 'seasons_in', 'transfer_cost',
                         'transfer_stored_out']]

                # find hourly transfer_storage and leftover production
                df_h2_hourly_tmp = df_h2_hourly[(df_h2_hourly.produced_leftover > zero_round)].rename(
                    columns={'regions': 'Rgn_Export'})
                df_transfer_storage = pd.merge(
                    df_h2_hourly_tmp,
                    df_transfer_storage_season,
                    on=['Rgn_Export', 'seasons_in'],
                    how='left').fillna(0)
                df_transfer_storage = get_leftover(df_transfer_storage,
                                                   total_in='transfer_stored_out', total_out='transfer_stored',
                                                   hourly_in='produced_leftover', hourly_left='produced_leftover',
                                                   groupby=['Rgn_Export', 'seasons_in'])
                # update produced_leftover in df_h2_hourly to exclude this transfer_storage
                df_h2_hourly.loc[(df_h2_hourly.produced_leftover > zero_round), 'produced_leftover'] = list(
                    df_transfer_storage.produced_leftover)
                # update transfer_stored_out (leftover)
                df_transfers_update = pd.merge(df_transfer_reg,
                                               df_transfer_storage.groupby(
                                                   ['Rgn_Export', 'Rgn_Import', 'seasons_in', 'seasons_out'])[
                                                   'transfer_stored'].sum().reset_index(),
                                               on=['Rgn_Export', 'Rgn_Import', 'seasons_in'],
                                               how='left').fillna(0)
                # update transfer_stored_out (leftover) in df_transfers_left
                df_transfers_left.loc[(df_transfers_left.Rgn_Import == reg) & (
                        df_transfers_left.transfer_stored_out > zero_round), 'transfer_stored_out'] \
                    = list(df_transfers_update.transfer_stored_out - df_transfers_update.transfer_stored)
                # keep if there is transfer_stored
                df_transfer_storage = df_transfer_storage[df_transfer_storage.transfer_stored > zero_round]
                # add to output df
                df_transfer_to_storage_steps = pd.concat(
                    [df_transfer_to_storage_steps, df_transfer_storage]).reset_index(
                    drop=True)

    # df_transfer_to_storage_steps.drop(columns=['regions'], inplace=True)
    if len(df_transfer_to_storage_steps) > 0:
        # update costs to reflect transfer, storage, and production
        df_transfer_to_storage_steps.loc[:, 'cost_hydrogen'] = df_transfer_to_storage_steps.cost_hydrogen \
                                                               + storage_costs * (
                                                                       df_transfer_to_storage_steps.seasons_out
                                                                       - df_transfer_to_storage_steps.seasons_in) \
                                                               + df_transfer_to_storage_steps.transfer_cost
        # clean up df to match template
        df_transfer_to_storage_steps.rename(
            columns={'Rgn_Import': 'regions', 'transfer_stored': 'H2_Produced', 'seasons_out': 'seasons'},
            inplace=True)
        df_transfer_to_storage_steps.loc[:, 'type'] = 'transfer_stored'
        df_transfer_to_storage_steps = df_transfer_to_storage_steps[keep_cols]

    return df_transfer_to_storage_steps


# utility functions

def get_leftover(df_in, total_in, total_out, hourly_in, hourly_left, groupby):
    """
    This is a general utility function.
    this can be generalized to any situation where something is being summed (hourly_in)
    until you hit a value (total_in),
    then it returns the amount that is summed (total_out) and the amount leftover (hourly_left) in new columns.

    Parameters
    ----------
    df_in: pd.DataFrame
        data containing (at least) columns hourly_in, total_in, and groupby
    total_in: str
        name of column which is total sum cutoff (col already exists)
    total_out: str
        name of column which will keep track of amount that is summed (new)
    hourly_in: str
        name of column which will be summed (col already exists)
    hourly_left: str
        name of column which will keep track of leftovers of total_out (new)
    groupby: list of str
        columns of how to group the summing algorithm
    Returns
    -------
    df: pd.DataFrame
        data containing additional columns hourly_out and total_out
    """
    df = df_in.copy()

    # For all of these comments, I will use the example assuming we are trying to find hourly local production.
    # Here, hourly_in = 'H2_Produced' aka the amount of produced h2
    # and total_in = 'hydrogen_demand' aka the demand of hydrogen.

    # This is the cumulative sum of produced h2 up to and INCLUDING this hour (grouped by 'groupby' list)
    df.loc[:, 'produced_sum'] = df.groupby(groupby)[hourly_in].cumsum()
    # This is the cumulative sum of produced h2 up to, NOT including this hour
    df.loc[:, 'produced_sum_upto'] = df.produced_sum - df[hourly_in]
    # This is the remaining demand BEFORE this hour's production is included
    df.loc[:, 'produced_b4'] = df[total_in] - df.produced_sum_upto
    # This is the remaining demand AFTER this hour's production is included
    df.loc[:, 'produced_af'] = df[total_in] - df.produced_sum
    # This is the remaining demand before this hour
    df.loc[:, 'produced_pos'] = np.maximum(0, df.produced_b4)
    # This is the excess demand after this hour
    df.loc[:, 'produced_neg'] = np.maximum(0, -1.0 * df.produced_af)
    # This is the amount of local production: the min of the remaining demand before this hour
    # and the hourly produced h2
    df.loc[:, 'produced_real'] = df[['produced_pos', hourly_in]].min(axis=1)
    # This is the amount of non-local production (the leftover production) that must be transfered or stored:
    # the min of the excess demand after this hour and the hourly produced h2
    df.loc[:, 'leftover'] = df[['produced_neg', hourly_in]].min(axis=1)

    drop_cols = ['produced_b4', 'produced_pos', 'produced_af', 'produced_neg', 'produced_sum_upto', 'produced_sum'] + \
                ([total_out] if total_out in df.columns else []) + \
                ([hourly_left] if hourly_left in df.columns else [])
    df.drop(columns=drop_cols, inplace=True)
    # rename produced_real to be total_out and 'leftover' to be hourly_left
    df.rename(columns={'produced_real': total_out, 'leftover': hourly_left}, inplace=True)
    return df


def get_costs_factor_file(ephrts_cost_factor_file):
    """

    Fetches cost factor provided by ECP in a file

    Parameters
    ----------
    ephrts_cost_factor_file: str
        path to file

    Returns
    -------
    cost_factor: float
        fraction used for finaicial planning (costs of capitial debt payment calculation)
    """

    # fetch costs factor
    with open(ephrts_cost_factor_file) as f:
        temp = f.readlines()
        cost_factor = (float(str(temp[1].strip())))

    return cost_factor


# main

def construct_raw_step_data(output_folder, cmd_line_year, iteration, ephrts_input_file,
                            ephrts_cost_factor_file, ephrts_print_file, seasons,
                            season_diff_range, regions, keep_cols, zero_round):
    """
    Constructs the raw data step from eprhts without yet converting to emm format

    Parameters
    ----------
    output_folder: str
        ephrts output directory path
    cmd_line_year: str or int
        ephrts run year
    iteration: str or int
        ephrts iteration (1 or 2)
    ephrts_input_file: str
        parameter input file path that was used for ephrts model
    ephrts_cost_factor_file: str
        path to cost_factor file
    ephrts_print_file: str
        log file path
    seasons: list
    season_diff_range: list
    regions: list
    keep_cols: list
    zero_round: float

    Returns
    -------
    df_results: pd.DataFrame
        final disaggregated supply curves with cols: h2_produced (kg) and cost_hydrogen ($/kg)
    df_demand: pd.DataFrame
        hourly demand data to be used for validator (kg)
    total_cost_noadd: float
        total cost if we DO NOT add capital cost of electrolyzers ($)
    total_cost_add: float
        total cost if we DO add capital cost of electrolyzers ($)

    """
    df_demand, df_h2_produced, df_storage, df_transfer, df_capacity, \
    df_cost, df_transfer_cost, df_D_Fuel, storage_costs, storage_capacity_additions_cost, \
    df_storage_capacity_costs, df_final_production_capacity, production_capacity_costs, \
    total_cost_noadd, total_cost_add = \
        read_data(output_folder, cmd_line_year, iteration, ephrts_input_file, ephrts_print_file)

    check_demand(df_demand)

    # this computes everything at a seasonal level
    df_h2_seasonal, df_transfers_net_export, df_transfers_left = \
        get_seasonal_setup(df_transfer, df_transfer_cost, df_demand, df_storage, df_h2_produced,
                           seasons, regions, zero_round)

    # next, move production around to proper hours
    df_production_steps, df_h2_hourly = get_production_steps(df_h2_seasonal, df_cost, df_h2_produced,
                                                             keep_cols, zero_round)

    # get local storage steps
    # this step updates df_h2_hourly's leftover production to exclude that originally produces for local storage
    # also updates df_h2_seasonal with updated missing_demand (excluding local storage)
    df_storage_steps = get_storage_steps(df_h2_hourly, df_h2_seasonal, storage_costs, season_diff_range,
                                         zero_round, keep_cols)

    # check if there are any transfers
    if df_transfers_net_export.empty:
        df_interregional_transfer_steps = pd.DataFrame()
        df_storage_to_transfer_steps = pd.DataFrame()
        df_transfer_to_storage_steps = pd.DataFrame()
    else:
        # get direct (non-storage) transfer
        # this step updates df_h2_hourly's leftover production to exclude that which
        # originally produces for direct transfer
        df_interregional_transfer_steps = get_interregional_transfer_steps(df_h2_hourly, df_transfers_left,
                                                                           regions, zero_round, keep_cols)

        # get storage-transfer
        # this step updates df_h2_hourly's leftover production to exclude that which
        # originally produces for storage transfer
        df_storage_to_transfer_steps = get_storage_to_transfer_steps(df_h2_hourly, df_transfers_left,
                                                                     storage_costs, season_diff_range,
                                                                     regions, zero_round, keep_cols)

        # get transfer-storage
        # this step updates df_h2_hourly's leftover production to exclude that which
        # originally produces for transfer storage
        # also updates df_h2_seasonal with updated missing_demand (excluding transfer-storage)
        df_transfer_to_storage_steps = get_transfer_to_storage_steps(df_h2_hourly, df_h2_seasonal, df_transfers_left,
                                                                     storage_costs, season_diff_range,
                                                                     regions, zero_round, keep_cols)
    # combine all types of production
    results = {}
    results['production'] = df_production_steps
    results['storage'] = df_storage_steps
    results['transfer'] = df_interregional_transfer_steps
    results['storage-transfer'] = df_storage_to_transfer_steps
    results['transfer-storage'] = df_transfer_to_storage_steps
    df_results = pd.concat(results).reset_index(drop=True)
    df_results = df_results.sort_values(by=['seasons', 'regions'],
                                        ascending=[True, True])

    #
    df_results = df_results.fillna(1.0)
    df_results['steps'] = 1.0
    df_results = pd.merge(df_results, df_demand, on=['seasons', 'regions'])
    df_results['additions_costs'] = 0.0

    # storage capacity additions cost
    df_storage_capacity_costs['final_cost_storage'] = \
        (df_storage_capacity_costs['SCAPMAX']) * storage_capacity_additions_cost

    df_production_capacity_costs = pd.merge(df_final_production_capacity, production_capacity_costs,
                                            on=['technology']).reset_index(drop=True)

    df_production_capacity_costs['final_cost_production'] = df_production_capacity_costs[
                                                                'hydrogen_capacity_additions_cost'] * \
                                                            (df_production_capacity_costs['h2_production_capacity'])

    df_production_capacity_costs.drop(columns=['hydrogen_capacity_additions_cost'], inplace=True)

    df_production_capacity_costs = df_production_capacity_costs.groupby(['regions']).sum().reset_index()

    df_final_additions_costs = pd.merge(df_storage_capacity_costs, df_production_capacity_costs,
                                        on='regions').reset_index(drop=True)

    # fetch costs factor
    costs_factor = get_costs_factor_file(ephrts_cost_factor_file)  # e.g. 0.07

    df_final_additions_costs['final_additions_cost_annualized'] = costs_factor * \
                                                                  (df_final_additions_costs['final_cost_production'] +
                                                                   df_final_additions_costs['final_cost_storage'])

    df_results.loc[:, 'additions_costs'] = (df_results.H2_Produced / df_results.H2_Produced.sum()) \
                                           * df_final_additions_costs.final_additions_cost_annualized.sum()
    df_results['cost_hydrogen_additions'] = (df_results['additions_costs'] / df_results['H2_Produced'])
    df_results['cost_hydrogen_no_additions'] = df_results['cost_hydrogen']
    # add additions / no additions costs together
    df_results['cost_hydrogen'] = df_results['cost_hydrogen_no_additions'] + df_results[
        'cost_hydrogen_additions']

    return df_results, df_demand, total_cost_noadd, total_cost_add


def simple_bin_algo(number_of_bins, df_input):
    """
    This simple binning algorithm is used instead of the k-means method because it is quicker.
    We saw in run time the kmeans method adds almost 90% of the run time to the python processor.
    This method, does not capture the level of fidelity of the kmeans method (capture cheaper hours in the first step)
    but it is quicker...

    Parameters
    ----------
    number_of_bins: int
        number of bins
    df_input: df.DataFrame
        df with raw steps

    Returns
    -------
    df: df.DataFrame
        df with aggregated steps

    """
    df_temp = df_input.copy()

    first_step_price = .0001

    # set prices less than cutoff value equal to cutoff value (basically get rid of negative prices)
    # comment this out if you want to allow the first step to have a negative price
    df_temp.loc[df_temp.cost_hydrogen < first_step_price, 'cost_hydrogen'] = first_step_price

    # Group the same prices together into 1 step
    df_temp = df_temp.groupby(
        ["regions", "seasons", "cost_hydrogen"]).agg(H2_Produced=("H2_Produced", "sum")).reset_index()
    # Initialize "step" column
    df_temp.loc[:, 'step'] = 0

    # Loop over all regions and seasons
    for region in df_temp.regions.unique():
        for season in df_temp.seasons.unique():
            # temporary df with this region and season only
            df_temp_rs = df_temp[(df_temp.seasons == season) & (df_temp.regions == region)]
            # if the number of steps is <= the number of bins needed, no aggregation is needed, leave them
            if len(df_temp_rs) <= number_of_bins:
                # set step number in order
                df_temp.loc[df_temp_rs.index, 'step'] = df_temp_rs.reset_index(drop=True).index + 1
                df_temp.loc[df_temp_rs.index, 'final_price'] = df_temp_rs.cost_hydrogen
            else:  # if there are more steps than number of bins, we need to aggregate
                # if there are any values <= to first_step_price, make that the first step
                if len(df_temp_rs.loc[df_temp_rs.cost_hydrogen <= first_step_price]) > 0:
                    df_temp_rs.loc[df_temp_rs.cost_hydrogen <= first_step_price, 'step'] = 1
                    df_temp.loc[(df_temp.cost_hydrogen <= first_step_price) &
                                (df_temp.seasons == season) & (df_temp.regions == region), 'step'] = 1
                    step_start = 2
                else:  # if there are no first_step_price, you can have 1 additional step when cutting
                    step_start = 1
                # create equally-sized bins based on this region/season's data
                ls_cut = pd.cut(df_temp_rs[df_temp_rs.step == 0].cost_hydrogen,
                       bins=number_of_bins - step_start + 1,
                       labels=list(range(step_start, number_of_bins + 1)))
                df_temp.loc[(df_temp.step == 0) &
                            (df_temp.seasons == season) & (df_temp.regions == region), 'step'] = list(ls_cut)

    # Define a lambda function to compute the weighted mean:
    wm = lambda x: np.average(x, weights=df_temp.loc[x.index, "H2_Produced"])

    # Group the same prices together into 1 step
    df_temp = df_temp.groupby(["regions", "seasons", "step"]).agg(H2_Produced=("H2_Produced", "sum"),
                                                          final_price=("cost_hydrogen", wm)).reset_index()
    # reset steps names so it starts at 1 and increases by 1.
    # If there aren't the full # of steps as number_of_bins, it will fill it the rest later.
    df_temp.loc[:, 'steps'] = df_temp.groupby(["regions", "seasons"]).cumcount().astype(int) + 1

    df_temp = df_temp[['regions', 'seasons', 'steps', 'final_price', 'H2_Produced']]

    return df_temp


def simple_bin_algo_neg(number_of_bins, df_input):
    """
    This simple binning algorithm is used instead of the k-means method because it is quicker.
    We saw in run time the kmeans method adds almost 90% of the run time to the python processor.
    This method, does not capture the level of fidelity of the kmeans method (capture cheaper hours in the first step)
    but it is quicker...

    Parameters
    ----------
    number_of_bins: int
        number of bins
    df_input: df.DataFrame
        df with raw steps

    Returns
    -------
    df: df.DataFrame
        df with aggregated steps

    """
    df_temp = df_input.copy()

    first_step_price = .0001

    # set prices less than cutoff value equal to cutoff value (basically get rid of negative prices)
    # comment this out if you want to allow the first step to have a negative price
    #df_temp.loc[df_temp.cost_hydrogen < first_step_price, 'cost_hydrogen'] = first_step_price

    # Group the same prices together into 1 step
    df_temp = df_temp.groupby(
        ["regions", "seasons", "cost_hydrogen"]).agg(H2_Produced=("H2_Produced", "sum")).reset_index()
    # Initialize "step" column
    df_temp.loc[:, 'step'] = 0

    # Loop over all regions and seasons
    for region in df_temp.regions.unique():
        for season in df_temp.seasons.unique():
            # temporary df with this region and season only
            df_temp_rs = df_temp[(df_temp.seasons == season) & (df_temp.regions == region)]
            # if the number of steps is <= the number of bins needed, no aggregation is needed, leave them
            if len(df_temp_rs) <= number_of_bins:
                # set step number in order
                df_temp.loc[df_temp_rs.index, 'step'] = df_temp_rs.reset_index(drop=True).index + 1
                df_temp.loc[df_temp_rs.index, 'final_price'] = df_temp_rs.cost_hydrogen
            else:  # if there are more steps than number of bins, we need to aggregate
                # if there are any values <= to first_step_price, make that the first step
                #if len(df_temp_rs.loc[df_temp_rs.cost_hydrogen <= first_step_price]) > 0:
                #    df_temp_rs.loc[df_temp_rs.cost_hydrogen <= first_step_price, 'step'] = 1
                #    df_temp.loc[(df_temp.cost_hydrogen <= first_step_price) &
                #                (df_temp.seasons == season) & (df_temp.regions == region), 'step'] = 1
                #    step_start = 2
                #else:  # if there are no first_step_price, you can have 1 additional step when cutting
                #    step_start = 1
                # create equally-sized bins based on this region/season's data
                ls_cut = pd.cut(df_temp_rs.cost_hydrogen,
                       bins=number_of_bins,
                       labels=list(range(1, number_of_bins + 1)))
                df_temp.loc[(df_temp.seasons == season) & (df_temp.regions == region), 'step'] = list(ls_cut)

    # Define a lambda function to compute the weighted mean:
    wm = lambda x: np.average(x, weights=df_temp.loc[x.index, "H2_Produced"])

    # Group the same prices together into 1 step
    df_temp = df_temp.groupby(["regions", "seasons", "step"]).agg(H2_Produced=("H2_Produced", "sum"),
                                                          final_price=("cost_hydrogen", wm)).reset_index()
    # reset steps names so it starts at 1 and increases by 1.
    # If there aren't the full # of steps as number_of_bins, it will fill it the rest later.
    df_temp.loc[:, 'steps'] = df_temp.groupby(["regions", "seasons"]).cumcount().astype(int) + 1

    df_temp = df_temp[['regions', 'seasons', 'steps', 'final_price', 'H2_Produced']]

    return df_temp



# validating

def validate_steps(df_results, df_demand, total_cost_noadd, total_cost_add, zero_round):
    """
    unit tests, check total demand and total cost to aimms results.
    prints results in log file

    Parameters
    ----------
    df_results: pd.DataFrame
        final disaggregated supply curves with cols: h2_produced (kg) and cost_hydrogen ($/kg)
    df_demand: pd.DataFrame
        seasonal hydrogen demand data by region (kg)
    total_cost_noadd: float
        total cost if we DO NOT add capital cost of electrolyzers ($)
    total_cost_add: float
        total cost if we DO add capital cost of electrolyzers ($)
    zero_round: float

    Returns
    -------
    demand_check: boolean 
        True if demand out of ephrts and at the end of supply curve are equal, False if not & will throw error
    cost_check: boolean
        True if operating costs out of ephrts and at the end of supply curve are equal, False if not & will throw error
    frac_demand: float
        Total error of demand
    frac_cost_noadd: float
        Total error of operating costs
    """
    demand_check = True
    cost_check = True

    # check total demand
    df_new = df_results.groupby(['seasons', 'regions'])['H2_Produced'].agg('sum').reset_index()
    df_temp = pd.merge(df_new, df_demand, on=['seasons', 'regions'], how='outer')
    df_temp.loc[:, 'diff'] = abs(df_temp['demand_hydrogen'] - df_temp['H2_Produced'])
    df_temp2 = df_temp[df_temp['diff'] > zero_round]
    if not df_temp2.empty:
        total_diff = df_temp2['diff'].sum()
    else:
        total_diff = 0
    frac_demand = total_diff / df_demand.demand_hydrogen.sum() * 100

    # check total cost no additions to aimms total cost no additions
    df_temp = df_results.copy()
    df_temp.loc[:, 'total_cost_no_add'] = df_temp['cost_hydrogen_no_additions'] * df_temp['H2_Produced']
    if not df_temp.empty:
        total_cost_noadd_check = df_temp.total_cost_no_add.sum()
    else:
        total_cost_noadd_check = 0
    frac_cost_noadd = (total_cost_noadd_check - total_cost_noadd) / total_cost_noadd * 100

    # check total cost additions to aimms total cost additions
    df_temp = df_results.copy()
    df_temp.loc[:, 'total_cost_add'] = df_temp['cost_hydrogen_additions'] * df_temp['H2_Produced']
    if not df_temp.empty:
        total_cost_add_check = df_temp.total_cost_add.sum()
    else:
        total_cost_add_check = 0
    #  frac_cost_add = (total_cost_add_check - total_cost_add) / total_cost_add * 100
    #  TODO - pass checks to validator

    # check if we should error out due to demand check or cost check
    if round(frac_demand, 1) != 0.0:
        demand_check = False
    if round(frac_cost_noadd, 1) != 0.0:
        cost_check = False

    return demand_check, cost_check, frac_demand, frac_cost_noadd


# constructing supply curves


def construct_emm_supply_curves(df_results, seasons_emm, regions, steps, num_bin):
    """
    constructs "num_step"-step curve to pass into emm

    Parameters
    ----------
    df_results: pd.DataFrame
        final disaggregated supply curves with cols: h2_produced (kg) and cost_hydrogen ($/kg)
    seasons_emm: list
    regions: list
    steps: list
    num_bin: int

    Returns
    -------
    df_final_prices: pd.DataFrame
        final aggregated supply curves with cols: h2_produced (kg) and cost_hydrogen ($/kg)
    """
    testing_multiplier = 1.0

    # now aggregate into emm steps
    # From HMM : PREVIOUS 1 - Summer, 2 - Shoulder, 3- Winter    NEW 1 - Winter, 2 - Summer, 3 Spring/Fall
    # To EMM : 1 - Winter, 2 - Summer, 3- Shoulder
    df_results.loc[(df_results['seasons'] == 1) | (df_results['seasons'] == 3), 'seasons'] = 3
    df_results.loc[(df_results['seasons'] == 4), 'seasons'] = 1

    df_emm_steps = df_results.copy()
    df_emm_steps.loc[:, 'cost_hydrogen'] = df_emm_steps.cost_hydrogen.astype(float).round(4)
    # sort by cost

    df_emm_steps.sort_values(by=['seasons', 'regions', 'cost_hydrogen'], ascending=[True, True, True], inplace=True)
    df_emm_steps.loc[:, 'H2_Produced'] = pd.to_numeric(df_emm_steps['H2_Produced'])
    # find sum of production until this point
    df_emm_steps.loc[:, 'H2_Produced_sum'] = df_emm_steps.groupby(['seasons', 'regions'])['H2_Produced'].cumsum()

    # write out raw steps for debugging
    # if (cmd_line_year == "2050") and (iteration == '2'):
    #    df_emm_steps.to_csv('df_emm_steps_h2_extreme_d042123h.csv')

    df_emm_steps = df_emm_steps[['seasons', 'regions', 'cost_hydrogen', 'H2_Produced', 'H2_Produced_sum']]
    df_emm_steps = df_emm_steps.copy()

    # remove very small steps to save time
    df_emm_steps = df_emm_steps[~(df_emm_steps.H2_Produced < 1)]
    # remove duplicate cost points and aggregate them into 1 H2_Produced to save run time
    df_emm_steps.loc[:, 'H2_Produced_sum'] = df_emm_steps.groupby(['seasons', 'regions', 'cost_hydrogen'])[
        'H2_Produced_sum'].transform('max')

    df_emm_steps = df_emm_steps.groupby(['seasons', 'regions', 'cost_hydrogen', 'H2_Produced_sum'])[
        'H2_Produced'].sum().reset_index()

    # starttime = timeit.default_timer()
    # print("The start time is :", starttime)

    df_emm_steps = simple_bin_algo(num_bin, df_emm_steps)

    # print("The time difference is :", timeit.default_timer() - starttime)

    # df_emm_steps = df_emm_steps.groupby(['regions', 'seasons']).apply(get_steps).reset_index(drop=True)

    df_final_prices = df_emm_steps[['regions', 'seasons', 'steps', 'H2_Produced', 'final_price']]

    # multiplying costs by testing_multiplier
    df_final_prices.loc[:, 'final_price'] = testing_multiplier * df_final_prices.final_price

    # this step adds 2nd and 3rd steps if not there (or missing seasons/regions)
    blank = pd.DataFrame({'seasons': np.repeat(seasons_emm, len(regions) * len(steps)),
                          'regions': np.repeat(regions * len(seasons_emm), len(steps)),
                          'steps': steps * (len(seasons_emm) * len(regions))})

    df_final_prices = pd.merge(df_final_prices, blank,
                               on=['regions', 'seasons', 'steps'], how='outer')

    df_final_prices.loc[df_final_prices.H2_Produced.isna(), 'H2_Produced'] = 999999999  # 0.01

    df_final_prices.loc[df_final_prices.final_price.isna(), 'final_price'] = 999.9
    # this is because it might round to 0 in the matrix if too small.
    # df_final_prices.loc[df_final_prices['final_price'].between(0, 0.0001), 'final_price'] = 0.0001
    # df_final_prices.loc[df_final_prices['final_price'].between(-0.0001, 0), 'final_price'] = -0.0001

    # df_final_prices.loc[df_final_prices.final_price < 0.0001, 'final_price'] = 0.0001
    # now doing this in the simple_bin_algo earlier in the callstack

    df_final_prices.sort_values(by=['regions', 'seasons', 'steps'], ascending=[True, True, True],
                                na_position='first',
                                inplace=True)
    return df_final_prices


# keep get_steps commented out so python does not load we don't want to load any code that's unused.
# TODO: decide if we want to keep get_steps and refactor it to make it faster or just use the simple bin algo
# def get_steps(df1):
#     """
#     distribute the binning steps using weighted SSE minimization
#
#     :param df1: pd.DataFrame with many cost steps
#     :return: df: pd.DataFrame with columns including weighted mean "final_price" and "steps"
#     """
#
#     df = df1.copy().reset_index(drop=True)
#
#     if np.shape(df)[0] > num_bin:
#         # kmeans = KMeans(n_clusters=3, random_state=0, max_iter=1000)
#         kmeans = KMeans(init='k-means++', n_clusters=num_bin, n_init=10)
#         X = np.array(df['cost_hydrogen'].astype(float)).reshape(-1, 1)
#
#         max_produced = df.H2_Produced_sum.max()
#         sum_list = df["H2_Produced_sum"]
#         sum_list_wt1 = (max_produced - sum_list) / max_produced
#
#         # this is the amount of weight given to the
#         # (a lower value means the split is pushed to beginning), generally I would say go 0-2
#         beg_wt = 0.04
#         sum_list_wt = np.maximum(sum_list_wt1 + beg_wt, 0)
#         # sum_list_wt = sum_list_wt * beg_wt
#
#         Y = np.array(df['H2_Produced'].astype(float) * sum_list_wt)  # * sum_list_wt
#         wt_kmeansclus = kmeans.fit(X, sample_weight=Y)
#         predicted_kmeans = kmeans.predict(X, sample_weight=Y)
#         centers = wt_kmeansclus.cluster_centers_
#         # group = wt_kmeansclus.labels_.astype(float)
#
#         # re-order clusters in sorted order and map to steps in df
#         idx = np.argsort(centers.sum(axis=1))
#         lut = np.zeros_like(idx)
#         lut[idx] = np.arange(num_bin)
#         centers = centers[idx]
#         group = lut[wt_kmeansclus.labels_]
#
#         # assigning final steps
#         df.loc[:, 'steps'] = group + 1
#
#         # calculating the mean price in that step
#         df.loc[:, 'wt'] = df.H2_Produced * df.cost_hydrogen
#         wt_steps = df.groupby('steps')['H2_Produced'].sum()
#         total_wt = df.groupby(['steps'])['wt'].sum()
#         center_price = (total_wt / wt_steps).reset_index()[0]
#
#         # sum up everything in the same step
#         df = df.groupby(['seasons', 'regions', 'steps'])['H2_Produced'].sum().reset_index()
#         # assign the final price as the center of each cluster
#         df.loc[:, 'final_price'] = center_price
#
#     else:
#         df.loc[:, 'steps'] = df.index + 1
#         df.loc[:, 'final_price'] = df.cost_hydrogen
#
#     return df


# writing results

def write_results(df_final_prices, ephrts_supply_curve):
    """
    write out results to csv to be used by emm

    Parameters
    ----------
    df_final_prices: pd.DataFrame
        final aggregated supply curve price and quantity data
    ephrts_supply_curve: str
        output supply curve path

    Returns
    -------
    none
    """

    df_final_prices.reset_index(inplace=True, drop=True)
    df_final_prices[['regions', 'seasons', 'steps']] = df_final_prices[
        ['regions', 'seasons', 'steps']].astype(int)
    df_final_prices.rename(columns={'regions': 'Region',
                                    'seasons': ' Season',
                                    'steps': ' Step',
                                    'H2_Produced': ' Quantity',
                                    'final_price': ' Price'}, inplace=True)

    df_final_prices.to_csv(ephrts_supply_curve, float_format='%.4f', index=False)


def crash_nems():
    # write out crash nems, using os.sys.exit() won't work because python is on a different thread
    stop_file = os.path.join(os.getcwd(), "ephrts", "crash_nems.txt")

    # crash nems
    with open(stop_file, 'w') as file:  # Use file to refer to the file object
        line = 'EPHRTS - Error within supply curve algorithm, throwing call stack \n'
        file.write(line)


def main(cmd_line_year, iteration, current_path):
    """
    This is the main call for the python post processor's supply curve algorithm,
    it's intent is to generate supply curves for efd & ecp where it adds them
    to a csv file (hydrogen_data.csv).

    Parameters
    ----------
    cmd_line_year: int
        nems current year
    iteration: int
        ephrts iteration
    current_path: str
        current working directory

    Returns
    -------
    void
    """

    # ignore values smaller than this
    zero_round = 0.001

    # seasons in aimms model
    seasons = list(range(1, 5))
    # seasons in emm
    seasons_emm = list(range(1, 4))
    # differences between aimms model seasons
    season_diff_range = list(range(1, 4))
    # census regions
    regions = list(range(1, 12))
    # number of bin splits for final supply curve steps
    # if you want 3 supply steps (excluding spillover step), make this 2.
    num_bin = 6
    # steps for final supply curve to be output (num_bin + 1 for extra spillover)
    steps = list(range(1, num_bin + 2))

    # columns to keep in df_results
    keep_cols = ['regions', 'seasons', 'hours', 'technology', 'cost_hydrogen', 'H2_Produced', 'type']
    # starttime = timeit.default_timer()

    output_folder = os.path.join(current_path, 'ephrts', 'output')
    ephrts_input_file = os.path.join(current_path, 'ephrts', 'input', 'ephrts_inputs.txt')
    ephrts_cost_factor_file = os.path.join(current_path, 'ephrts', 'input', 'ephrts_costs_factor.txt')
    ephrts_cost_factor_file = os.path.join(current_path, 'ephrts', 'input', 'ephrts_costs_factor.txt')
    ephrts_print_file = os.path.join(current_path, 'ephrts', 'log', 'ephrts.lis')
    ephrts_supply_curve = os.path.join(current_path, 'input', 'hydrogen_data.csv')

    # construct all possible steps from the LP and put into one governing dataframe
    df_results, df_demand, total_cost_noadd, total_cost_add = \
        construct_raw_step_data(output_folder, cmd_line_year, iteration, ephrts_input_file,
                                ephrts_cost_factor_file, ephrts_print_file,
                                seasons, season_diff_range, regions, keep_cols, zero_round)

    # check to see if the data is correct. for example, is demand met?
    # The amount of h2 should be close to the demand being met
    demand_check, cost_check, frac_demand, frac_cost_noadd = \
        validate_steps(df_results, df_demand, total_cost_noadd, total_cost_add, zero_round)

    if (not demand_check) or (not cost_check):
        print('ephrts demand or costs are different - error')
        print('demand error: ' + str(round(frac_demand, 3)) + '%')
        print('cost error: ' + str(round(frac_cost_noadd, 3)) + '%')
        crash_nems()

    # aggregate to emm
    df_emm = construct_emm_supply_curves(df_results, seasons_emm, regions, steps, num_bin)

    # debug option, only used to fix supply
    debug = False
    if debug:
        # put two steps to zero
        df_emm.loc[df_emm.steps == 1, 'final_price'] = 0.0001
        df_emm.loc[df_emm.steps == 2, 'final_price'] = 0.0001

        # multiply quantity by 10
        df_emm['H2_Produced'] = df_emm['H2_Produced'] * 10.0

    # write out to efd and ecp
    print(ephrts_supply_curve)
    write_results(df_emm, ephrts_supply_curve)
