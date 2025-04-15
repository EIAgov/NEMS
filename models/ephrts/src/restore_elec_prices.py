import pandas as pd
import os
import restore_elec_prices_util as ut
import warnings

warnings.filterwarnings("ignore")


def capacity_additions_algo(cmd_line_year, path_capacity_additions, path_vintage, year):
    """
    This function takes capacity additions data, assigns a year, and stores them into a vintage csv file for later use

    This function also modifies the df_additions by assigning 45v label to electrolyers built within ten years

    Parameters
    ----------
    cmd_line_year : year for cmd file
    path_capacity_additions :  path to the capacity additions for a given year
    path_vintage : path to the ephrts vintage data contains capacities and years
    year : current year
    Returns
    -------
    df_additions : this additions dataframe is then processed later and utilized in ephrts via the cmd text file

    """

    df_additions = pd.DataFrame()

    if os.path.exists(path_capacity_additions):
        df_additions = pd.read_csv(path_capacity_additions)

        if not df_additions.empty:

            # assign year
            df_additions['year'] = year

            # check if vintage path exists, if not use empty dataframe
            if os.path.exists(path_vintage):
                df_vintage = pd.read_csv(path_vintage)
            else:
                df_vintage = pd.DataFrame()

            # concatenate to vintage dataframe, then write out to csv
            df_vintage = pd.concat([df_vintage, df_additions], ignore_index=True).reset_index(drop=True)
            df_vintage = df_vintage[['regions', 'technology', 'h2_production_capacity_additions', 'year']].reset_index(drop=True)

            # reset all the technology types back to electrolysis and then apply the _45v label
            df_vintage['technology'] = 'electrolysis'
            df_vintage.loc[df_vintage.year > (cmd_line_year - 10), 'technology'] = 'electrolysis_sec45v'            # adds _sec45v to the prior 10 years, we use 10 years because that's specified within the sec45. this label will be picked up in the aimms module.
            df_vintage.to_csv(path_vintage, mode='w')

            df_capacity = df_vintage[['regions', 'technology', 'h2_production_capacity_additions']]
            df_capacity_summed = df_capacity.groupby(['regions', 'technology'])['h2_production_capacity_additions'].sum().reset_index()
            df_additions = df_capacity_summed.copy()

    return df_additions  # returns empty dataframe


def mapload(iteration, load_path, df_dayweight, df_monthseason, Nerc2CensusMapping):
    """
    This function maps the load from restore by applying the day weights and converts the Nerc to Census depending on the EPHRTS iteration

    Parameters
    ----------
    iteration : ephrts iteration
    load_path : the path for the restore load file
    df_dayweight : dataframe containing dayweights
    df_monthseason : dataframe containing month season
    Nerc2CensusMapping : nerc to census regional mapping

    Returns
    -------
    df_load : a dataframe containing the load from restore which accounts for the load slices width using the dayweights and converted regions

    """
    df_load = pd.read_csv(load_path)
    df_load = pd.merge(df_load, df_dayweight, on=['Month', 'DayType'])
    df_load = pd.merge(df_load, df_monthseason, left_on=['Month'], right_on=['m'])
    df_load['Rep_Load*Idaytq'] = df_load['Rep_Load'] * df_load['Idaytq']
    df_load = df_load.groupby(by=['Year', 'EMMReg', 's', 'ECP_Hour'])['Rep_Load*Idaytq'].sum().reset_index()
    df_load['load'] = df_load['Rep_Load*Idaytq']

    if iteration == "0":
        df_load['EMMReg'] = df_load['EMMReg'].apply(lambda x: Nerc2CensusMapping.get(x))
        df_load = df_load.groupby(by=['Year', 'EMMReg', 's', 'ECP_Hour'])['Rep_Load*Idaytq'].sum().reset_index()
        df_load['load'] = df_load['Rep_Load*Idaytq']

    elif iteration == "1":
        df_load['EMMReg'] = df_load['EMMReg'].apply(lambda x: Nerc2CensusMapping.get(x))
        df_load = df_load.groupby(by=['Year', 'EMMReg', 's', 'ECP_Hour'])['Rep_Load*Idaytq'].sum().reset_index()
        df_load['load'] = df_load['Rep_Load*Idaytq']

    df_load.loc[df_load.load < 0.0, 'load'] = 0.0001  # if load past in is negative, make it a number close to 0. Do not use 0 since AIMMS handles 0 by using null values

    return df_load


def compute_load_ratio_tolerance(df_ratio, df_ratio_constraint):
    """
    Given the load ratio and the exisiting load ratio constraint in ephrts, if a region is using too much load, increment the load ratio for the given region
    if within specified tolerance

    Parameters
    ----------
    df_ratio : dataframe containing the computed load ratios (fuel demand/ load) for each region
    df_ratio_constraint : dataframe containing the current load ratios in the ephrts model constraint

    Returns
    -------

    df_ratio_compare : a dataframe with incremented load ratios if applicable to prevent ephrts constraint from going infeasible

    """
    df_ratio_constraint.columns = ['regions', 'load_ratio']
    df_ratio_compare = pd.merge(df_ratio, df_ratio_constraint, on=['regions'])

    # compute the delta between the load ratio and the ephrts load ratio, if within tolerance then double the ratio
    df_ratio_compare['tolerance'] = df_ratio_compare.load_ratio - df_ratio_compare.ratio
    df_ratio_compare.loc[df_ratio_compare['tolerance'] < 0.15, 'load_ratio'] = df_ratio_compare.load_ratio * 2.0  # note: this is aggressive

    # if the load_ratio constraint exits above as a ratio above the upper tolerance then set the load_ratio to the upper tolerance
    upper_tolerance = 1.0
    df_ratio_compare.loc[df_ratio_compare.load_ratio > upper_tolerance, 'load_ratio'] = upper_tolerance  # set max to 1.0
    return df_ratio_compare[['regions', 'load_ratio']]


def get_regional_mapping(path_regional_mappings):
    """
    Using excel workbook found in ephrts inputs folder, returns a mapping between the Nerc and Census Regions

    Parameters
    ----------
    path_regional_mappings : path to the excel work book

    Returns
    -------
    Nerc2CensusMapping : dictionary mapping between Nerc and Census regions

    """
    # use proposed mapping found in the regional mapping excel workbook
    df_regional_mappings = pd.read_excel(path_regional_mappings, sheet_name='proposed_mapping')
    df_regional_mappings.index = df_regional_mappings.nerc
    Nerc2CensusMapping = df_regional_mappings.to_dict()
    Nerc2CensusMapping = Nerc2CensusMapping.get('census')
    return Nerc2CensusMapping


def compute_marginal_prices(iteration, path, df_dayweight, df_monthseason, Nerc2CensusMapping, sec45v_path):
    """
    Computes the marginal fuel prices (electricity) for the electrolzyers from restore

    Parameters
    ----------
    iteration - ephrts iteration
    path - path to the
    df_dayweight - dataframe containing dayweight
    df_monthseason - dataframe containing month and season
    Nerc2CensusMapping - nerc to census region mapping

    Returns
    -------

    """
    # marginal prices
    df = pd.read_csv(path)
    df['tech'] = "electrolysis"

    df_sec45v = pd.read_csv(sec45v_path)
    df_new = pd.merge(left=df, right=df_sec45v, left_on=['EMMReg', 'Month', 'ECP_Hour'], right_on=[' NERC_RGNS', ' MONTHS_EPHRTS', ' HOURS_EPHRTS'])
    df_new['tech'] = "electrolysis_sec45v"
    df_new['Rep_MarginalPrices'] = df_new['Rep_MarginalPrices'] - df_new[' PRICE_REDUCER']
    df_new = df_new[df.columns]  # filter w/ orginal columns
    df = pd.concat([df, df_new]).reset_index(drop=True)

    if iteration == "0":
        df['EMMReg'] = df['EMMReg'].apply(lambda x: Nerc2CensusMapping.get(x))
    elif iteration == "1":
        df['EMMReg'] = df['EMMReg'].apply(lambda x: Nerc2CensusMapping.get(x))

    df = df.loc[df['SolveIndex'] == 1]

    df = pd.merge(df, df_dayweight, on=['Month', 'DayType'])
    df = pd.merge(df, df_monthseason, left_on=['Month'], right_on=['m'])

    df['Rep_MarginalPrices_Idaytq'] = df['Rep_MarginalPrices'] * df['Idaytq']  # multiply the price by the dayweight

    df = df[['Year', 'tech', 'EMMReg', 'Month', 's', 'ECP_Hour', 'Idaytq', 'Rep_MarginalPrices_Idaytq']]  # filter

    df = df.groupby(by=['Year', 'tech', 'EMMReg', 'Month', 's', 'ECP_Hour']).sum().reset_index()  # sum marginal price per ecp hour

    df['weight_avg_price'] = df['Rep_MarginalPrices_Idaytq'] / df['Idaytq']
    df = df.apply(pd.to_numeric, errors='ignore')

    df = df.groupby(by=['Year', 'tech', 'EMMReg', 's', 'ECP_Hour'])['weight_avg_price'].mean().reset_index()

    df = df[['EMMReg', 's', 'ECP_Hour', 'weight_avg_price', 'tech']]
    return df


def get_h2_production_capacity(ephrts_path, Nerc2CensusMapping):
    """
    Fetches the produciton compacity, converts to nerc or census regions

    Parameters
    ----------
    ephrts_path - input path to ephrts model
    Nerc2CensusMapping - mapping between census and nerc regions

    Returns
    -------
    df_h2_production_capacity_census - data frame containing production capacity in census regions
    df_h2_production_capacity_nerc - data frame containing production capacity in nerc regions

    """
    # each iteration write out the same information, aimms will figure out which to use
    df_h2_production_capacity_nerc = pd.read_csv(ephrts_path + '\\h2_production_capacity_nxtyr.csv')
    df_h2_production_capacity_census = df_h2_production_capacity_nerc.copy()
    df_h2_production_capacity_census['regions'] = df_h2_production_capacity_census['regions'].apply(lambda x: Nerc2CensusMapping.get(x))
    df_h2_production_capacity_census = df_h2_production_capacity_census.groupby(by=['regions', 'technology'])['h2_production_capacity_nxtyr'].sum().reset_index()
    return df_h2_production_capacity_census, df_h2_production_capacity_nerc


def convert_h2_prodution_to_census(df_h2_production_capacity_nerc, Nerc2CensusMapping):
    """
    Fetches the produciton compacity, converts to nerc or census regions

    Parameters
    ----------
    ephrts_path - input path to ephrts model
    Nerc2CensusMapping - mapping between census and nerc regions

    Returns
    -------
    df_h2_production_capacity_census - data frame containing production capacity in census regions
    df_h2_production_capacity_nerc - data frame containing production capacity in nerc regions

    """
    # each iteration write out the same information, aimms will figure out which to use
    df_h2_production_capacity_census = df_h2_production_capacity_nerc.copy()
    df_h2_production_capacity_census['regions'] = df_h2_production_capacity_census['regions'].apply(lambda x: Nerc2CensusMapping.get(x))
    df_h2_production_capacity_census = df_h2_production_capacity_census.groupby(by=['regions', 'technology'])['h2_production_capacity_nxtyr'].sum().reset_index()
    return df_h2_production_capacity_census, df_h2_production_capacity_nerc


def get_storage_capacity_maximum(ephrts_path, Nerc2CensusMapping):
    """
    fetches the maximum storage capacity values

    Parameters
    ----------
    ephrts_path - input path to ephrts model
    Nerc2CensusMapping - mapping between census and nerc regions

    Returns
    -------
    df_SCAPMAX_nerc - dataframe containing the maximum storage values in nerc regions
    df_SCAPMAX_census - dataframe containing the maximum storage values in census regions
    """

    df_SCAPMAX_nerc = pd.read_csv(ephrts_path + '\\SCAPMAX_nxtyr.csv')  # in the future take object from main.py for file path
    df_SCAPMAX_census = df_SCAPMAX_nerc.copy()
    df_SCAPMAX_census['regions'] = df_SCAPMAX_census['regions'].apply(lambda x: Nerc2CensusMapping.get(x))
    df_SCAPMAX_census = df_SCAPMAX_census.groupby(by=['regions'])['SCAPMAX_nxtyr'].sum().reset_index()
    return df_SCAPMAX_nerc, df_SCAPMAX_census


def get_input_data_load_ratio(df_load_for_load_ratio, df_dfuel):
    """
    The goal of this function is to compute the ratio of the fuel demand to load used to adjust the load ratio constraint programatically in ephrts

    Parameters
    ----------
    df_load_for_load_ratio - adjusted load from restore
    df_dfuel - fuel demand from ephrts

    Returns
    -------
    df_ratio - a dataframe containing the regional ratios

    """
    df = pd.merge(left=df_dfuel, right=df_load_for_load_ratio, left_on=['seasons', 'regions', 'hours'],
                  right_on=['s', 'EMMReg', 'ECP_Hour'], how='outer')  # we do not want to drop regions if there's no demand, we want to explicitly keep them and assign zero fuel demanad

    # TODO: figure out if ephrts should be N number of months, how does this work in a mixed langauge environment?
    #  How does this flow though EFD, ECP, LDSM, supply curve algorithm without writing many custom parsers for each language? Should all hydrogen be consumed within same year? Or reserves held for next year? Are those reserves added to spinning reserves in EMM (requires another custom parser)
    #  Also, requires coordination with the new HMM model. Regions are programatic in ephrts, seasons are not. This setup ensures ephrts will break in run time (we'd rather it break than fail silent) if seasons are changed.

    # EPHRTS Seasons 1 and 3 are the shoulder seasons which are 2 months each, Seasons 2/4 are summer/winter and are 4 months each
    # All months adds up to 12 months before doing groupby which finds total ratio of dfuel:load
    df.loc[df['seasons'] == 1, 'D_Fuel'] = df.D_Fuel * 2.0
    df.loc[df['seasons'] == 1, 'load'] = df.load * 2.0
    df.loc[df['seasons'] == 3, 'D_Fuel'] = df.D_Fuel * 2.0
    df.loc[df['seasons'] == 3, 'load'] = df.load * 2.0
    df.loc[df['seasons'] == 2, 'D_Fuel'] = df.D_Fuel * 4.0
    df.loc[df['seasons'] == 2, 'load'] = df.load * 4.0
    df.loc[df['seasons'] == 4, 'D_Fuel'] = df.D_Fuel * 4.0
    df.loc[df['seasons'] == 4, 'load'] = df.load * 4.0

    df_ratio = df.groupby(by=['EMMReg'])[['D_Fuel', 'load']].sum().reset_index(drop=False).copy()

    df_ratio['ratio'] = df_ratio.D_Fuel / (df_ratio.load * 1000.0)
    df_ratio = df_ratio[['EMMReg', 'ratio']]
    df_ratio.columns = ['regions', 'ratio']

    return df_ratio


def compute_load_ratio(iteration, cmd_line_year, start_year, dfuel_path, load_path_for_load_ratio, load_ratio_input_path, load_ratio_output_path, df_dayweight, df_monthseason, Nerc2CensusMapping):
    """

    In extreme decarbonization scenarios, the electrolzyers use a high percentage of load which may cause an infeasiblity in ephrts.
    This routine increases the percentages automatically provided they are within a tolerance.

    Parameters
    ----------
    iteration - ephrts iteration
    dfuel_path - path to the demand fuel for ephrts
    load_path_for_load_ratio - load ratio input path
    load_ratio_input_path - load ratio input path
    load_ratio_output_path - load ratio output path

    Returns
    -------
    Empty - writes out a file for ephrts to read into it's load ratio constraint programtically

    """

    # Maps hourly load used in REStore run from two years prior for calculation of load ratio contraint adjustment
    # if iteration == "1" or iteration == "2":

    if iteration == "1" or iteration == "2" and int(cmd_line_year) > int(start_year):

        if os.path.exists(dfuel_path):
            # This will be true for AEOYR+2/itr0, AEOYR+2/itr2, AEOYR+3/itr1 becasue there is not matching dfuel.csv output file for previous year
            # note: put simply, this means that technically at the beginning of the ephrts run, around the first start year. This dfuel recalculation may
            #       not be executed programmatically but instead pre-determined by the input load ratio composite file. This only occurs when referencing the start year due to data initilization.

            df_dfuel = pd.read_csv(dfuel_path)

            # this load ratio is used in cases where there's a lot of load on the grid. it's the amount of load from the electrolyzers compared to the total grid load.
            # in these extreme cases (e.g. zero fossil fuels by 2030, this mechanism allows us to increase the laod on the grid for the electrolyzers programtically,
            # and prevent the model from going infeasible.

            if not df_dfuel.empty:

                df_load_for_load_ratio = mapload(iteration, load_path_for_load_ratio, df_dayweight, df_monthseason, Nerc2CensusMapping)
                df_ratio = get_input_data_load_ratio(df_load_for_load_ratio, df_dfuel)  # this ratio is calculated
                df_ratio_constraint = pd.read_csv(load_ratio_input_path)  # this ratio is the constraint used in the model

                df_ratio_final = compute_load_ratio_tolerance(df_ratio, df_ratio_constraint)
                ut.write_load_ratio_to_aimms(load_ratio_output_path, df_ratio_final, iteration)
    return


def main(root_path, cmd_line_year, iteration, start_year):
    """
    This is the main call for the ephrts python preprocessor. Its job is to setup the cmd.txt file and composite files for the ephrts aimms project.

    Parameters
    ----------
    root_path - current working directory
    cmd_line_year - current NEMS year
    iteration - ephrts iteration
    start_year - nems start year

    Returns
    -------
    void
    """
    # note - file manager in NEMS is not supported, at least not to this level. In the future, hopefully, main.py will pass us an object that we can then retrieve
    #        the files from. else if added to filemanager in nems, requires homegrown parser between nems shell script output and python (not ideal).

    restore_path = os.path.join(root_path, "rest", "fromAIMMS")
    path = os.path.join(restore_path, "Output_" + str(int(cmd_line_year) - 1), "Rep_MarginalPrices.csv")
    load_path = os.path.join(restore_path, "Output_" + str(int(cmd_line_year) - 1), "Rep_Load.csv")
    path_dayweight = os.path.join(root_path, "ephrts", "src", "input", "restoredayweight.csv")
    path_monthseason = os.path.join(root_path, "ephrts", "src", "input", "month_season_map.csv")
    path_regional_mappings = os.path.join(root_path, "ephrts", "src", "input", "regional_mappings.xlsx")
    load_composite_file = os.path.join(root_path, "ephrts", "input", "restore_load.txt")
    load_path_for_load_ratio = os.path.join(restore_path, "Output_" + str(int(cmd_line_year) - 2), "Rep_Load.csv")  # dfuel in any give year is compared to load in year-1 for load ratio constraint in ephrts (if dfuel used is from year-1, then load to compare is year-2)
    dfuel_path = os.path.join(root_path, "ephrts_debug", "D_Fuel_" + str(int(cmd_line_year) - 1) + "_" + str(int(iteration)) + ".csv")
    load_ratio_input_path = os.path.join(root_path, "ephrts", "input", "load_ratio" + "_" + str(int(iteration)) + ".csv")
    load_ratio_output_path = os.path.join(root_path, "ephrts", "input", "load_ratio" + "_" + str(int(iteration)) + ".txt")
    path_production_capacity_additions = os.path.join(root_path, "ephrts", "output", "h2_production_capacity_additions.csv")
    path_h2_production_capacity_vintage = os.path.join(root_path, "ephrts", "src", "h2_production_capacity_vintage.csv")
    file_cmdline_aimms = os.path.join(root_path, "ephrts_debug", "ephrts_cmds_" + cmd_line_year + "_" + str(iteration) + ".txt")
    fuel_cost_composite_file = os.path.join(root_path, "ephrts", "input", "fuel_costs.txt")
    stop_file = os.path.join(root_path, "ephrts", "crash_nems.txt")
    ephrts_path = os.path.join(root_path, "ephrts", "output")
    sec45_price_reducer_file = os.path.join(root_path, 'ephrts', 'src', 'sec45v_electricity_price_reducer.csv')
    ephrts_costs_factor_file = os.path.join(root_path, 'ephrts', 'input', 'ephrts_costs_factor.txt')

    # shared parameters
    Nerc2CensusMapping = get_regional_mapping(path_regional_mappings)
    df_dayweight = pd.read_csv(path_dayweight)
    df_monthseason = pd.read_csv(path_monthseason)

    # main driver
    df_sec45_capacity_additions = capacity_additions_algo(int(cmd_line_year), path_production_capacity_additions, path_h2_production_capacity_vintage, int(cmd_line_year))
    # todo figure out how to use the df_newly_calculated additions, we think we need to merge or overwrite the df_h2_production_capacity_nerc file

    df_marginal_prices = compute_marginal_prices(iteration, path, df_dayweight, df_monthseason, Nerc2CensusMapping, sec45_price_reducer_file)
    #  apply_sec45v_price_reduction(sec45_price_reducer_file, df_marginal_prices)

    df_load = mapload(iteration, load_path, df_dayweight, df_monthseason, Nerc2CensusMapping)  # Maps hourly load used in previous year's REStore run for load ratio constraint in EPHRTS

    # write to ephrts
    compute_load_ratio(iteration, cmd_line_year, start_year, dfuel_path, load_path_for_load_ratio, load_ratio_input_path, load_ratio_output_path, df_dayweight, df_monthseason, Nerc2CensusMapping)

    # if no capacity additions use existing structure for aimms initializaion purposes else compute new additions
    if df_sec45_capacity_additions.empty:
        df_h2_production_capacity_census, df_h2_production_capacity_nerc = get_h2_production_capacity(ephrts_path, Nerc2CensusMapping)
    else:
        df_h2_production_capacity_nerc = pd.read_csv(ephrts_path + '\\h2_production_capacity_nxtyr.csv')
        df_sec45_capacity_additions.columns = df_h2_production_capacity_nerc.columns
        df_h2_production_capacity_census, df_h2_production_capacity_nerc = convert_h2_prodution_to_census(df_sec45_capacity_additions, Nerc2CensusMapping)

    # fetch the maximum storage in nerc and census in order to write out to aimms cmd file. We include both nerc and census in aimms since
    # it's easier to do both at the same time. also helps in the future if we collaspe the two ephrts calls to one call.
    df_SCAPMAX_nerc, df_SCAPMAX_census = get_storage_capacity_maximum(ephrts_path, Nerc2CensusMapping)

    # error checking
    ut.error_crash_nems(stop_file, df_h2_production_capacity_census, df_h2_production_capacity_nerc, df_SCAPMAX_nerc, df_SCAPMAX_census)

    # write to ephrts
    ut.write_load_to_aimms(load_composite_file, df_load)
    ut.write_cost_factor_to_aimms(stop_file, file_cmdline_aimms, ephrts_costs_factor_file)
    ut.write_capacity_to_aimms(file_cmdline_aimms, df_h2_production_capacity_census, ['regions', 'technology', 'h2_production_capacity'], 'h2_production_capacity_int_census')
    ut.write_capacity_to_aimms(file_cmdline_aimms, df_h2_production_capacity_nerc, ['regions', 'technology', 'h2_production_capacity'], 'h2_production_capacity_int_nerc')
    ut.write_storage_to_aimms(file_cmdline_aimms, df_SCAPMAX_nerc, column_names=['regions', 'SCAPMAX'], aimms_label='SCAPMAX_INT_nerc')
    ut.write_storage_to_aimms(file_cmdline_aimms, df_SCAPMAX_census, column_names=['regions', 'SCAPMAX'], aimms_label='SCAPMAX_INT_census')
    ut.write_cmd_execution(file_cmdline_aimms)
    ut.write_fuel_cost_to_aimms(fuel_cost_composite_file, df_marginal_prices)

    # now delete output folder in ephrts aimms folder and make a new one, the previous command for this was in the aimms module but it is acting strange (different behavior with same inputs) since the aimms update
    # deleting files is very nuances and gets into the windows symbolic links, read/write permissions, etc.
    current_dir = os.getcwd()
    os.chdir(ephrts_path)  # go to the ephrts output folder
    os.system('rm *')  # deletes all files in the ephrts_path
    os.chdir(current_dir)  # now go back to the current directory, not needed now but in the future might be required
