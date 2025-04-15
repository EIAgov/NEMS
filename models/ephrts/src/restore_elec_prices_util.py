import pandas as pd
import numpy as np
import os
import argparse
import sys
from pathlib import Path


# helper routines that write to a composite file format for aimms or aimms cmd file

def write_capacity_to_aimms(file_cmdline_aimms, df_input, column_names, aimms_label):
    """
    Writes the ephrts aimms capacity to the cmd input file

    file_cmdline_aimms: input cmd file
    df_input: input data frame
    column_names: column names to write
    aimms_label: label for aimms internal usage

    return: void
    """
    with open(file_cmdline_aimms, 'a') as file:  # Use file to refer to the file object
        line = '\n'
        file.write(line)

        df_input.columns = column_names

        if not df_input.empty:

            line = 'Let ' + aimms_label + ' := List { \n'
            file.write(line)

            for index in df_input.index:
                row = df_input.iloc[index]
                capacity = float(row.iloc[2])

                capacity = str("{:.6f}".format(capacity))

                if index == len(df_input.index) - 1:
                    line = "('" + str(int(row.iloc[0])) + "', " + "'" + str(row.iloc[1]) + "') : " + capacity + "\n"
                    file.write(line)

                else:
                    line = "('" + str(int(row.iloc[0])) + "', " + "'" + str(row.iloc[1]) + "') : " + capacity + ",\n"
                    file.write(line)

            line = '};\n'
            file.write(line)

            line = '\n'
            file.write(line)


def write_cost_factor_to_aimms(stop_file, file_cmdline_aimms, ephrts_costs_factor_file):
    """
    Writes the costs factor from emm to the aimms input file

    @param stop_file: used to crash nems if applicable
    @param file_cmdline_aimms: path input for cmd file
    @param ephrts_costs_factor_file: file for the cost factor to be read into ephrts aimms
    @return:
    """
    # fetch costs factor
    with open(ephrts_costs_factor_file) as f:
        temp = f.readlines()
        cost_factor = (float(str(temp[1].strip())))

    if not 0.02 <= cost_factor <= 0.2:
        with open(stop_file, 'w') as file:  # Use file to refer to the file object
            line = 'cost_factor is not within range, Crash NEMS; \n'
            file.write(line)

    # write to cmd file
    with open(file_cmdline_aimms, 'a') as file:  # Use file to refer to the file object
        line = '\n'
        file.write(line)

        line = 'Let cost_factor :=' + str(cost_factor) + ';'
        file.write(line)

        line = '\n'
        file.write(line)


def write_storage_to_aimms(file_cmdline_aimms, df_input, column_names, aimms_label):
    """
    Writes the storage data from to ephrts aimms model

    @param file_cmdline_aimms: path to cmd aimms
    @param df_input: dataframe input for storage
    @param column_names: column names used in aimms
    @param aimms_label:  aimms label for storage
    @return: void
    """
    # print("adding annual h2 storage capacities")

    df_input.columns = column_names

    if not df_input.empty:

        with open(file_cmdline_aimms, 'a') as file:  # Use file to refer to the file object
            line = '\n'
            file.write(line)

            line = 'Let ' + aimms_label + ' := List { \n'
            file.write(line)

            for index in df_input.index:
                row = df_input.iloc[index]

                scapmax = float(row.iloc[1])

                scapmax = str("{:.6f}".format(scapmax))

                if index == len(df_input.index) - 1:
                    line = "'" + str(int(row.iloc[0])) + "' : " + scapmax + "\n"
                    file.write(line)

                else:
                    line = "'" + str(int(row.iloc[0])) + "' : " + scapmax + ",\n"
                    file.write(line)

            line = '};\n'
            file.write(line)

            line = '\n'
            file.write(line)


def error_crash_nems(stop_file, df_h2_production_capacity_census, df_h2_production_capacity_nerc, df_SCAPMAX_nerc, df_SCAPMAX_census):
    """
    Error handling to crash nems if dataframes are empty. Note: os.sys.exit() will not work here in production.

    @param stop_file: file that nems reads and upon reading, crashes nems
    @param df_h2_production_capacity_census: dataframe for hydrogen production capacity in census regions
    @param df_h2_production_capacity_nerc: dataframe for hydrogen production capacity in nerc regions
    @param df_SCAPMAX_nerc: dataframe for maximum storage capacity in nerc
    @param df_SCAPMAX_census: dataframe for maxiumum storage capacity in census
    @return: void
    """
    if df_h2_production_capacity_census.empty or df_h2_production_capacity_nerc.empty or df_SCAPMAX_nerc.empty or df_SCAPMAX_census.empty:
        with open(stop_file, 'w') as file:  # Use file to refer to the file object
            line = 'Error (most likely ephrts infeasbility in prior year, Crash NEMS; \n'
            file.write(line)


def write_load_to_aimms(load_composite_file, df_load):
    """
    Writes the load to the aimms file in the format of a composite file
    @param load_composite_file: path to the composite file location
    @param df_load: dataframe for the load
    @return: void
    """
    with open(load_composite_file, 'w') as file:  # Use file to refer to the file object
        line = 'Composite Table: \n'
        file.write(line)

        line = 'seasons  regions   hours       load\n'
        file.write(line)

        for index in df_load.index:
            row = df_load.iloc[index]

            # hour = float(row.ECP_Hour)
            # hour = str("{:.1f}".format(hour))

            load = float(row.load)
            load = str("{:.6f}".format(load))

            # line = "'" + str(row.s).strip() + "' \t " + "'" + str(row.EMMReg).strip() + "' \t '" + str(row.ECP_Hour).strip() + "'" + '\t' + load + "\n"
            # file.write(line)
            file.write("'{0}'      '{1}'       '{2}'      {3}\n".format(int(row.s), int(row.EMMReg), int(row.ECP_Hour), load))

        line = ';\n'
        file.write(line)

        line = '\n'
        file.write(line)


def write_cmd_execution(file_cmdline_aimms):
    """
    Appends the final Run and Quit commands to the cmd aimms wrapper
    @param file_cmdline_aimms: aimms cmd input file
    @return: void
    """
    with open(file_cmdline_aimms, 'a') as file:  # Use file to refer to the file object
        line = 'Run MainExecution; \n'
        file.write(line)
        line = 'Quit; \n'
        file.write(line)


def write_fuel_cost_to_aimms(fuel_cost_composite_file, df_marginal_prices):
    """
    Writes the fuel costs to the aimms module
    @param fuel_cost_composite_file: path for the composite file
    @param df_marginal_prices: dataframe containing the marginal prices
    @return: void
    """
    with open(fuel_cost_composite_file, 'w') as file:  # Use file to refer to the file object
        line = 'Composite Table: \n'
        file.write(line)

        line = 'seasons\tregions\thours  \t  technology \t        fuelcost\n'
        file.write(line)

        for index in df_marginal_prices.index:
            row = df_marginal_prices.iloc[index]

            weight_avg_price = float(row.weight_avg_price)
            weight_avg_price = str("{:.6f}".format(weight_avg_price))

            line = "'" + str(row.s) + "' \t " + "'" + str(row.EMMReg) + "' \t '" + str(row.ECP_Hour) + "' \t '" + str(row.tech) + "' \t " + weight_avg_price + "\n"
            file.write(line)

        line = ';\n'
        file.write(line)

        line = '\n'
        file.write(line)


def write_load_ratio_to_aimms(load_ratio_output_path, df_ratio_final, iteration):
    """
    Writes the load ratio file for ephrts aimms.

    @param load_ratio_output_path: path to the load ratio file
    @param df_ratio_final: dataframe containing the load ratio data
    @param iteration: iteration of the ephrts call
    @return: void
    """
    with open(load_ratio_output_path, 'w') as file:  # Use file to refer to the file object
        line = 'Composite Table: \n'
        file.write(line)

        line = 'regions     load_ratio_' + str(iteration) + '\n'
        file.write(line)

        for index in df_ratio_final.index:
            row = df_ratio_final.iloc[index]

            load = float(row.load_ratio)
            load_ratio_str = str("{:.6f}".format(load))

            file.write("'{0}'          {1}\n".format(int(row.regions), load_ratio_str))

        line = ';\n'
        file.write(line)

        line = '\n'
        file.write(line)
