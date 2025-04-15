# -*- coding: utf-8 -*-
"""
Created on Oct 22 2024

@author: Claire Su
"""
from datetime import datetime
import os, sys

dll_dirs = ([r"C:\Program Files (x86)\Intel\oneAPI\compiler\2023.2.1\windows\redist\intel64_win\compiler",
            r"C:\Windows\System32"])

for i in dll_dirs:
    if i not in sys.path:
        sys.path.append(i)
        os.add_dll_directory(i)

j = os.path.join(os.getcwd(), "PyFiler")
if j not in sys.path:
    sys.path.append(j)

import os, sys
import pandas as pd


# --- Do not delete the following. Debug file switch ----
is_debug = True
debug_file='./input/mnfactorx_processed.csv'
debug_xlsx_const_list_file='./input/mnfactorx_xlsx_const_list.txt'
# --- End of the Debug file switch ----
''' *************************** Module Level Design Concepts ************************************************************
This module mnfactorx_parser.py execution involves 3 input files:
    -the Conversion Factors historical data Excel file: input/mnfactorx.xlsx
    -api_file: input/mnfactorx_calc_api_list.txt
    -xlsx_eq_file: input/mnfactorx_calc_xlsx_eq_list.txt

The api_file lists the variables which are handled and computed in mnfactorx_calc.py module (i.e. origin utils_docvfacts.f90 docvfacts() subroutine).
Each api_file listed variable shall have a cooresponding calculation handling method in mnfactorx_calc.py

The xlsx_eq_file lists the non-constant variables, excluding the api_file variables. 
'''
def main(user, pyfiler):
    """the main entry point to control the manfactorx parser process flow. This methods calls sub methods to parse mnfactorx.xlsx,
    programmatically extend year data from the last mnfactorx year to year 2050, and return the df back to conversion_factor_load.main()

    Parameters
    ----------

    user : SimpleNamespace
        user class object, containing dict with NEMS runtime information
    pyfiler : module
        pyfiler fortran module

    Returns
    -------
    None.

    """
    file=user.SCEDES['MNFACTORXN']
    mnfactortx_yr,df=get_mnfactorx_end_yr(file)

    # declare the table columns:
    years_ends_mnfactorx=list(range(1990, mnfactortx_yr+1))    #1990-2023
    #years_after_mnfactorx=list(range(mnfactortx_yr+1,int(user.SCEDES['LASTYR'])+1))    #2024-2050
    years_after_mnfactorx=list(range(mnfactortx_yr+1,2050+1))    # Pyfiler dimension is static for 61. Altering LASTYR in SCEDES will cause conversion_factor_load to crash.

    # preparation. get the shall-calculated API list:
    api_file='./input/mnfactorx_calc_api_list.txt'
    xlsx_eq_file='./input/mnfactorx_calc_xlsx_eq_list.txt'
    shall_calc_api_list,shall_calc_xlsx_eq_list=get_shall_calc_list(api_file,xlsx_eq_file)

    # read in and transform the mnfactorx file content
    df=transform_mnfactorx_excel_file(df,years_ends_mnfactorx)

    # calcuate and extend year data over prediction years
    df=extend_data_over_prediction_years(pyfiler,df,years_after_mnfactorx,shall_calc_api_list,shall_calc_xlsx_eq_list)
    return df

def get_mnfactorx_end_yr(file):
    """read in input/mnfactorx.xlsx file, convert to a df and return an integer represents the last Conversion Factors year.
    If the input/mnfactorx.xlsx file does not exist, print error message and exit the program.

    Parameters
    ----------
    file : str
        the relative file path of input/mnfactorx.xlsx

    Returns
    -------
    int
        the last Conversion Factors year (e.g.2023)
    DataFrame
        the mnfactorx df

    """
    if not os.path.isfile(file):
        print(' Unable to open conversion factor file.')
        print('  You are stuck with the values from the previous run. Exit the program now')
        os.sys.exit()
        return

    xls = pd.ExcelFile(file)
    df = pd.read_excel(xls, sheet_name='mncvfact',skiprows=1)
    return int(list(df.columns)[-1]),df

def extend_data_over_prediction_years(pyfiler,df,years_after_mnfactorx,shall_calc_api_list,shall_calc_xlsx_eq_list):
    """iterate through each mnfactorx df row and determine if the row data is constant and shall extend the value on prediction years. Return the computed result df.

    Parameters
    ----------
    pyfiler : module
        pyfiler fortran module
    
    df : dataframe
        the mnfactorx df
    
    years_after_mnfactorx : list
        a integer list to list the prediction years
    
    shall_calc_api_list : list
        the string list of variables defined in source/utils_docvfacts.f90 DOCVFACTS SUBROUTINE.
    
    shall_calc_xlsx_eq_list : list
        the string list of variables which are not constant in input/mnfactorx.xlsx.

    Returns
    -------
    dataframe
        the mnfactorx df with computed prediction year data
    """
    # Add new columns with empty values and dtype float64    
    for col in years_after_mnfactorx:
        df[col] = pd.Series(dtype='float64')

    # now iterate through each API. If Conversion Factor end year data is not empty and API not in the shall_calc_api_list,
    #  use the end year data for the rest years. Otherwise, leave it empty or do calculation
    end_year=years_after_mnfactorx[0]-1   #2023
    debug_xlsx_const_list=[]
    for idx, row in df.iterrows():
        # check if the row at end_year is empy
        if df.isna().loc[idx,end_year]: continue
        api=row['API']

        if api.startswith('WEIGHT'):  # WEIGHTLPG1, WEIGHTLPG2 etc. weight variables stops on the end_yr. Do not extend to 2050
            continue 
        elif api in shall_calc_xlsx_eq_list:
            # for variables are computed by Excel formula, do nothing
            continue
        elif api in shall_calc_api_list:
            # currently utils_docvfacts.f90 handles. Reserve and integrate mnfactorx_calc.py to replace utils_docvfacts.f90
            continue
        else:
            # treat it as constant and append to the rest years
            n=row[end_year]
            for col in years_after_mnfactorx:
                df.loc[idx,col]=n
            # log the recognized constants in mnfactorx.xlsx for debugging
            debug_xlsx_const_list.append(api)

    if is_debug:
        df.to_csv(debug_file,index=False)
        with open(debug_xlsx_const_list_file,'w+') as f:
            for i in debug_xlsx_const_list:
                f.write(i+'\n')

    return df

def transform_mnfactorx_excel_file(df,years_ends_mnfactorx):
    """do feature engineering on the mnfactorx df. Filer out null API value rows, add prediction year columns, and return the df.

    Parameters
    ----------
    df : dataframe
        the mnfactorx df
    years_after_mnfactorx : list
        a integer list to list the prediction years

    Returns
    -------
    dataframe
        the mnfactorx df without null API and adding prediction year columns
    """
    # cut to up to the column years_ends_mnfactorx
    df=df.loc[:,:years_ends_mnfactorx[-1]]
    # only select the CONST, API, and years columns:
    headers=['API']+years_ends_mnfactorx
    df=df[headers]
    # Drop rows where column 'API' is NaN
    df.dropna(subset=['API'], inplace=True)

    # Sort by column 'API' in ascending order
    df=df.sort_values(by='API')

    # Reindex the DataFrame
    df.reset_index(drop=True, inplace=True)
    
    #df[df.columns[1:]]=df[df.columns[1:]].astype(float)
    return df

def get_shall_calc_list(api_file,xlsx_eq_file):
    """read in the 2 text files defined in api_file and xlsx_eq_file parameters, and return 2 lists of api variables:
       a list of API variables which shall be calculated in source/utils_docvfacts.f90, and a list of non-constant vairables in input/mnfactorx.xlsx

    Parameters
    ----------
    api_file : st
        the file lists the API variables computed in source/utils_docvfacts.f90 DOCVFACTS SUBROUTINE
    
    xlsx_eq_file : str
        the file lists input/mnfactorx.xlsx non-constant api variables

    Returns
    -------
    list
        the string list of variables defined in source/utils_docvfacts.f90 DOCVFACTS SUBROUTINE
    
    list
        the string list of variables which are not constant in input/mnfactorx.xlsx
    """
    # get the list of shall-be-calculated APIs
    shall_calc_api_list=[]
    with open(api_file, 'r') as f:
        shall_calc_api_list = [line.strip() for line in f.readlines()][1:]
    # get the list of shall-be-calculated .xlsx equation variables
    shall_calc_xlsx_eq_list=[]
    with open(xlsx_eq_file, 'r') as f:
        shall_calc_xlsx_eq_list = [line.strip() for line in f.readlines()][1:]      
    return shall_calc_api_list,shall_calc_xlsx_eq_list

class User:
    def __init__(self):
        self.SCEDES = {}

if __name__ == '__main__':
    import pyfiler1
    pyfiler1.utils.read_filer('restart_t0929a.unf')
    
    user=User()
    user.SCEDES['LASTYR'] = '2050'

    # --- start mnfactorx feature --    
    main(user,pyfiler1)