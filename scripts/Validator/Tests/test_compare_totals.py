"""
This test compares two variables or the sum of a subset of variables or regions. 

This file don't need to be editted when adding tests, that is done in the validator_subset_keys.csv and 
validator_controller.csv spreadsheet.

The compare_totals_prep.py will be initailized by validate.py to write into this .py file with the test functions 
from a list of tests that was read from the validator_subset_keys.csv for PyTest to find. 
"""

from DataModel.Model import Model
import pandas as pd
import numpy as np
import os

from Controller.StatusHelper import StatusHelper
from validate import get_lastyr_entry

def get_units(df, ls_year_columns):
    """
    Get the units from the dataframe and does the conversion of trillion BTU to quads

    Args:
        df (pd.dataframe): input dataframe
        ls_year_columns (list): list of range from start year to end year

    Returns:
        df (pd.dataframe): converted dataframe
        str_unit (str): string of the units
    """   
    # get the unit 
    str_unit = df['Gunits'].unique()[0]
    
    # check that the unit is trillion BTU
    if str_unit == 'trillion Btu':
        df[ls_year_columns] = df[ls_year_columns]/1000
        str_unit = 'quads'
    
    return df, str_unit
    
def load_table(start_year, end_year, str_totnum, str_subnum):
    """
    Loads the table # from the NEMS unif.api and parse the strings from the Variable Name 2 column for start year to end year.

    Args:
        start_year (float64): the start year
        end_year (float64): the end year
        str_totnum (str): the table number of the total, left side
        str_subnum (str): the table number of the subset, right side
    
    Returns:
        df (pd.dataframe): cleaned dataframe
        ls_year_columns (list): list of range from start year to end year
    """    
    # get table from api and filter the table to RegionNum, VarNam2, and start year to end year. The RegionNum is converted to string for easier search. 
    df_tot = Model.getInstance().files.csv.load_table(str_totnum)
    df_sub = Model.getInstance().files.csv.load_table(str_subnum)
    df = pd.concat([df_tot, df_sub], axis = 0, ignore_index = True)
    ls_year_columns = [str(i) for i in range(start_year, end_year+1)]
    ls_column_names = ['TableNumber','RegionNum', 'VarNam2', 'Gunits'] + ls_year_columns
    df = df.loc[:, ls_column_names]
    df = df.drop_duplicates()
    df['TableNumber'] = df['TableNumber'].astype(str)
    df['RegionNum'] = df['RegionNum'].astype(str)
    df[ls_year_columns] = df[ls_year_columns].astype(float)
    
    return df, ls_year_columns

def get_compare_total(df, ls_year_columns, str_total, str_totreg, str_totnum, str_sub, str_subreg, str_subnum):
    """
    Get the difference between the total reported and sum of the subsets. 

    Args:
        df (pd.dataframe): cleaned dataframe from load_table()
        ls_year_columns (list): list of range from start year to end year
        str_total (str): string of the variable name 2 for total, left side
        str_totreg (str): string of the region number for total, left side
        str_totnum ( str): string of the table number for total, left side
        str_sub (str) or (list): general common string or list (entries separated by | ) across the subsets
        str_subreg (str) or (list): string or list of the region number(s) for subset, right side
        str_subnum (str) or (list): stirng or list of the table number(s) for subset, right side

    Returns:
        s_difference (series): the difference between total minus sum of subset
        ls_total (list): list of the value of the total, left side, of the comparison
        ls_concat_str (list): list of strings that contains the values in the calculation
        str_sub_units (str): string of the units of the subset, right side
        str_total_units (str): string of the units of the total, left side
    """   
    
    # make sure that the input to the region numbers are string
    str_totreg = str(str_totreg)
    str_totnum = str(str_totnum)
    str_subreg = str(str_subreg)
    str_subnum = str(str_subnum)
    
    # if the entry in str_sub starts with a "-", then values from that list negative and reconstruct str_sub without the "-"
    str_sub_split = str_sub.split('|')
    str_sub_new = []
    for i in str_sub_split:
        if i.startswith('-'):
            str_sub_new.append(i[1:])
            df.loc[df["VarNam2"] == i[1:], ls_year_columns] = df.loc[df["VarNam2"] == i[1:], ls_year_columns]*-1
        else:
            str_sub_new.append(i)
    str_sub = "|".join(str_sub_new)
    
    # get the subset dataframe
    df_sub = df[df['VarNam2'].str.contains(f'^{str_sub}$') & df['RegionNum'].str.contains(f'^{str_subreg}$') & df['TableNumber'].str.contains(f'^{str_subnum}$')]
    df_sub, str_sub_units = get_units(df_sub, ls_year_columns)
    df_sub_sum = df_sub.sum()
    
    # create an concatenated string of the values 
    ls_concat_str = []
    for yr in ls_year_columns:
        str_var_value = df_sub[yr].astype(str)
        str_joined = " + ".join(str_var_value)
        ls_concat_str.append(str_joined)
    
    # get the total dataframe
    df_total = df[df['VarNam2'].str.contains(f'^{str_total}$') & df['RegionNum'].str.contains(f'^{str_totreg}$') & df['TableNumber'].str.contains(f'^{str_totnum}$')]
    df_total, str_total_units = get_units(df_total, ls_year_columns)
    df_total = df_total.transpose().squeeze()
    ls_total = df_total.iloc[4:]
    
    s_difference = df_total.iloc[4:] - df_sub_sum.iloc[4:]
    
    return s_difference, ls_total, ls_concat_str, str_sub_units, str_total_units

def calculate_subset(test):
    """
    reads the validator_subset_keys.csv and extracts relevant information from the test.

    Args:
        test (str): a string of the test name

    Returns:
        s_discrepancy (series): the difference between total minus sum of subset
        DISCREPANCY_TOLERANCE (float64): the percentage used in determining the tolerance based on the total, left side
        start_year (float64): the start year
        str_total (str): string of the variable name 2 for total, left side
        str_subset (str) or (list): general common string or list (entries separated by | ) across the subsets
        ls_total (list): list of the value of the total, left side, of the comparison
        ls_concat_str (list): list of strings that contains the values in the calculation
        str_totnum ( str): string of the table number for total, left side
        str_subnum (str) or (list): stirng or list of the table number(s) for subset, right side
        str_subreg (str) or (list): string or list of the region number(s) for subset, right side
        str_sub_units (str): string of the units of the subset, right side
        str_total_units (str): string of the units of the total, left side
    """    
    # read subset keys csv file
    df_keys = pd.read_csv('input\\validator_subset_keys.csv')
    scedes_end_year = get_lastyr_entry()
    start_year = df_keys.loc[df_keys['test'].str.contains(test+'\\('), 'start_year'].iloc[0]
    str_total = df_keys.loc[df_keys['test'].str.contains(test+'\\('), 'varnam2_total'].iloc[0]
    str_subset = df_keys.loc[df_keys['test'].str.contains(test+'\\('), 'varnam2_subset'].iloc[0]
    str_totnum = df_keys.loc[df_keys['test'].str.contains(test+'\\('), 'TableNum_total'].iloc[0]
    str_subnum = df_keys.loc[df_keys['test'].str.contains(test+'\\('), 'TableNum_subset'].iloc[0]
    str_subreg = df_keys.loc[df_keys['test'].str.contains(test+'\\('), 'RegNum_subset'].iloc[0]
    
    df, ls_year_columns = load_table(df_keys.loc[df_keys['test'].str.contains(test+'\\('), 'start_year'].iloc[0],
                                                scedes_end_year,
                                                str_totnum,
                                                str_subnum)

    s_discrepancy, ls_total, ls_concat_str, str_sub_units, str_total_units = get_compare_total(df, ls_year_columns,
                                    df_keys.loc[df_keys['test'].str.contains(test+'\\('), 'varnam2_total'].iloc[0],
                                    df_keys.loc[df_keys['test'].str.contains(test+'\\('), 'RegNum_total'].iloc[0],
                                    str_totnum,
                                    df_keys.loc[df_keys['test'].str.contains(test+'\\('), 'varnam2_subset'].iloc[0],
                                    str_subreg,
                                    str_subnum)
    
    DISCREPANCY_TOLERANCE = df_keys.loc[df_keys['test'].str.contains(test+'\\('), 'tolerance'].iloc[0]
    
    return s_discrepancy, DISCREPANCY_TOLERANCE, start_year, str_total, str_subset, ls_total, ls_concat_str, str_totnum, str_subnum, str_subreg, str_sub_units, str_total_units

def return_errors(s_discrepancy, DISCREPANCY_TOLERANCE, record_property, test, start_year, str_total, str_subset, ls_total, ls_concat_str, str_totnum, str_subnum, str_subreg, str_sub_units, str_total_units):
    """
    Returns the errors if the discrepancy is greater than the tolerance which contains the year, calculation, and discrepancy.

    Args:
        s_discrepancy (series): the difference between total minus sum of subset
        DISCREPANCY_TOLERANCE (float64): the percentage used in determining the tolerance based on the total, left side
        record_property (): stores the information of the error and used in the reporter
        test (str): a string of the test name
        start_year (float64): the start year
        str_total (str): string of the variable name 2 for total, left side
        str_subset (str) or (list): general common string or list (entries separated by | ) across the subsets
        ls_total (list): list of the value of the total, left side, of the comparison
        ls_concat_str (list): list of strings that contains the values in the calculation
        str_totnum ( str): string of the table number for total, left side
        str_subnum (str) or (list): stirng or list of the table number(s) for subset, right side
        str_subreg (str) or (list): string or list of the region number(s) for subset, right side
        str_sub_units (str): string of the units of the subset, right side
        str_total_units (str): string of the units of the total, left side
    """    
    record_property("csv_header", f"year, test, calculation({str_total_units}-{str_sub_units}), values({str_total_units}-{str_sub_units}), tolerance ({DISCREPANCY_TOLERANCE*100}%)({str_total_units}), discrepancy({str_total_units})")
    errors = False
    
    str_subreg_split = str_subreg.split('|')
    str_subset_split = str_subset.split('|')

    for year in range(len(s_discrepancy)):
        if abs(s_discrepancy[year]) > abs(DISCREPANCY_TOLERANCE*ls_total[year]):
            if len(str_subreg_split) == 1:
                if len(str_subset_split) == 1:
                    record_property("ERROR",f"{start_year+year},{test},{str_total}(Table:{str_totnum}) - {str_subset}(Table:{str_subnum}),{ls_total.iloc[year]} - {ls_concat_str[year]},{abs(DISCREPANCY_TOLERANCE*ls_total[year])},{s_discrepancy[year]}")
                    errors = True
                else:
                    record_property("ERROR",f"{start_year+year},{test},{str_total}(Table:{str_totnum}) - sum({str_subset})(Table:{str_subnum}),{ls_total.iloc[year]} - ({ls_concat_str[year]}),{abs(DISCREPANCY_TOLERANCE*ls_total[year])},{s_discrepancy[year]}")
                    errors = True
            else:
                record_property("ERROR",f"{start_year+year},{test},{str_total}(Table:{str_totnum}) - sum([regions]:{str_subset})(Table:{str_subnum}),{ls_total.iloc[year]} - ({ls_concat_str[year]}),{abs(DISCREPANCY_TOLERANCE*ls_total[year])},{s_discrepancy[year]}")
                errors = True
    
    assert not errors
