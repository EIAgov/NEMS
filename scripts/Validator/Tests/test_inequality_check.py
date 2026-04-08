"""
This test compares a variable to an inequality such as (>0) or (<0). 

This file don't need to be editted when adding tests, that is done in the validator_inequality_keys.csv and 
validator_controller.csv spreadsheet.

The create_test_prep.py will be initailized by validate.py to write into this .py file with the test functions 
from a list of tests that was read from the validator_inequality_keys.csv for PyTest to find. 
"""

from DataModel.Model import Model
import pandas as pd
import numpy as np
import os

from Controller.StatusHelper import StatusHelper
from validate import get_lastyr_entry

def load_table(start_year, end_year, str_num, str_name):
    """
    Loads the table # from the NEMS unif.api and parse the strings from the Variable Name 2 column for start year to end year.

    Args:
        start_year (float64): the start year
        end_year (float64): the end year
        str_num (str): the table number
        str_name (str): the variable name
    
    Returns:
        df (pd.dataframe): cleaned dataframe
        ls_year_columns (list): list of range from start year to end year
    """    
    # get table from api and filter the table to RegionNum, VarNam2, and start year to end year. The RegionNum is converted to string for easier search. 
    df = Model.getInstance().files.csv.load_table(str_num)
    ls_year_columns = [str(i) for i in range(start_year, end_year+1)]
    ls_column_names = ['TableNumber','RegionNum', 'VarNam2','Gunits'] + ls_year_columns
    df = df.loc[:, ls_column_names]
    df = df[df['VarNam2'] == str_name]
    df = df.drop_duplicates()
    df['TableNumber'] = df['TableNumber'].astype(str)
    df['RegionNum'] = df['RegionNum'].astype(str)
    df[ls_year_columns] = df[ls_year_columns].astype(float)
    
    return df, ls_year_columns

def get_check_inequality(df, ls_year_columns, str_inequality, abs_flag):
    """
    Get the boolean of a dataframe given the inequality to check.

    Args:
        df (pd.dataframe): cleaned dataframe from load_table()
        ls_year_columns (list): list of range from start year to end year
        str_inequality (str): string of the inequality such as (>0)

    Returns:
        df_boolean (pd.dataframe): a dataframe of true or false
        ls_tables (list): a list of table numbers
        ls_regions (list): a list of region numbers
        str_unit (str): string of the units
    """    
    
    # get inequality and value from the string
    str_check = str_inequality[0]
    str_inequality_value = pd.to_numeric(str_inequality[1:])
    
    # get units
    str_units = df['Gunits'].unique()[0]
    
    # get tables and regions columns from df as a list
    ls_tables = df["TableNumber"].unique()
    ls_regions = df["RegionNum"].unique()
    
    # do the abs value if it's flagged
    if abs_flag == 1:
        df[ls_year_columns] = abs(df[ls_year_columns])
    
    df_boolean = df.copy()
    # test inequality
    if str_check == ">":
        df_boolean[ls_year_columns] = df[ls_year_columns] > str_inequality_value
    else:
        df_boolean[ls_year_columns] = df[ls_year_columns] < str_inequality_value
        
    return df_boolean, ls_tables, ls_regions, str_units

def check_inequality(test):
    """
    reads the validator_inequality_keys.csv and extracts thre relevant information from the test.

    Args:
        test (str): a string of the test name

    Returns:
        df (pd.dataframe): cleaned dataframe
        df_boolean (pd.dataframe): a dataframe of true or false
        ls_year_columns (list): list of range from start year to end year
        ls_tables (list): a list of table numbers
        ls_regions (list): a list of region numbers
        start_year (float64): the start year
        str_name (str): the variable name
        str_inequality (str): string of the inequality such as (>0)
        str_unit (str): string of the units
    """    
    
    # read inequality keys csv file
    df_keys = pd.read_csv('input\\validator_inequality_keys.csv')
    
    start_year = df_keys.loc[df_keys['test'].str.contains(test+'\\('), 'start_year'].iloc[0]
    scedes_end_year = get_lastyr_entry()

    str_name = df_keys.loc[df_keys['test'].str.contains(test+'\\('), 'varnam2'].iloc[0]
    str_inequality = df_keys.loc[df_keys['test'].str.contains(test+'\\('), 'inequality'].iloc[0]
    abs_flag = df_keys.loc[df_keys['test'].str.contains(test+'\\('), 'abs'].iloc[0]
    
    df, ls_year_columns = load_table(start_year,
                                    scedes_end_year,
                                    df_keys.loc[df_keys['test'].str.contains(test+'\\('), 'TableNum'].iloc[0],
                                    str_name)
    
    df_boolean, ls_tables, ls_regions, str_units = get_check_inequality(df, ls_year_columns, str_inequality, abs_flag)
    
    return df, df_boolean, ls_year_columns, ls_tables, ls_regions, start_year, str_name, str_inequality, str_units, abs_flag

def return_errors(record_property, df, df_boolean, ls_year_columns, ls_tables, ls_regions, start_year, str_inequality, str_name, test, str_units, abs_flag):
    """_summary_

    Args:
        record_property (): stores the information of the error and used in the reporter
        df (pd.dataframe): cleaned dataframe
        df_boolean (pd.dataframe): a dataframe of true or false
        ls_year_columns (list): list of range from start year to end year
        ls_tables (list): a list of table numbers
        ls_regions (list): a list of region numbers
        start_year (float64): the start year
        str_name (str): the variable name
        str_inequality (str): string of the inequality such as (>0)
        test (str): a string of the test name
        str_unit (str): string of the units
    """      
    
    record_property("csv_header", f"year, region, table, test, variable, value ({str_units}), check")
    errors = False

    for table in ls_tables:
        df_t = df[df['TableNumber'] == table]
        df_boolean_t = df_boolean[df_boolean['TableNumber'] == table]
        for region in ls_regions:
            df_t_r = df_t[df_t['RegionNum'] == region]
            df_boolean_t_r = df_boolean_t[df_boolean_t['RegionNum'] == region]
            for year in range(len(ls_year_columns)):
                if df_boolean_t_r[str(start_year+year)].any() == False:
                    if abs_flag == 1:
                        record_property("ERROR",f"{start_year+year},{region},{table},{test},{str_name},abs({df_t_r.iloc[0][str(start_year+year)]}){str_inequality},{df_boolean_t_r.iloc[0][str(start_year+year)]}")
                    else:
                        record_property("ERROR",f"{start_year+year},{region},{table},{test},{str_name},{df_t_r.iloc[0][str(start_year+year)]}{str_inequality},{df_boolean_t_r.iloc[0][str(start_year+year)]}")
                    errors = True
    
    assert not errors
