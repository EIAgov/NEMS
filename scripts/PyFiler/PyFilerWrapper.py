# -*- coding: utf-8 -*-
"""
Created on Wed Mar 16 15:00:45 2022

@author: JMW

A wrapper code to initialize PyFiler and convert restart files from 'restart.unf' to 'restart.hdf' and vice versa
"""

# Importing neccessary components, where pyfiler is the pyfiler.pyd file
import os
import sys
import argparse as ap
import numpy as np
import pandas as pd
import NEMSHDFRestartIO as NEMSRestart
import pyfiler
import time
from shutil import move, copy, rmtree
import subprocess


def get_args(in_args=None):
    """parses the arguments passed in via command line of NEMS.
        NOTE: NEMS calls Pyfiler with trailing options for PyFiler to parse through for the input file, output file,
        and direction of restart to hdf formatting.

    Parameters
    ----------
    in_args: list
        in arguments first set to none, then creates a list of system arguments passed via the command line

    Returns
    -------
    arguments: Namespace
        command line arguments saved into namespace variable
    """

    if in_args is None:
        in_args = sys.argv[1:]

    parser = ap.ArgumentParser(formatter_class=ap.ArgumentDefaultsHelpFormatter,
                               prog=os.path.basename(sys.argv[0]))

    parser.add_argument('-o', '--output',
                        #default='restart_HSM.unf',
                        # name if user-defined
                        default='restartfull_08112022-6.hdf',
                        #default = 'restartHSM_0801022_5.hdf',
                        help=('Path to output folder to use for NEMS run'))

    parser.add_argument('-i', '--input',
                        # this will need fixed to input appropriately for the user to give a meaningful name
                        default='restart_HSM.unf',
                        #default='restartfull_08102022-1.hdf',  # this will need fixed to input appropriately
                        #default='restartHSM_080922_1.hdf',
                        help=('Path to input folder to use for NEMS run'))

    parser.add_argument('-d', '--direction',
                        default='restart2hdf',
                        #default='hdf2restart',
                        help=('Direction of PyFiler- Restart2HDF or HDF2Restart'))

    parser.add_argument('-y', '--curcalyr',
                        default='did not load',
                        help=('Current Calendar Year for NEMS'))

    parser.add_argument('-t', '--iteration',
                        default='no load on iteration',
                        help=('Iteration Number of NEMS'))

    parser.add_argument('-w', '--nemsrestartout',
                        default='restart_newHSM.unf',
                        #default='no load on NEMS Restart Output File',
                        help=('Output File for NEMSHDFRestartIO.py to write out file. Usual name is restart_(insert).unf'))

    parser.add_argument('-e', '--excel',
                        # not using entire restart file
                        #default='NEMS_report_ref_tables_HSM.xls',
                        default='',
                        # using entire restart file
                        help=('excel workbook of Fortran Variables and parameters of NEMS'))
                        # NOTE: for argument excel, if blank, the entire restart file is sent back.
    # 'args' conflicts with a debugger command, so use 'arguments' instead
    arguments = parser.parse_args(in_args)
    return arguments

def RestartHDF(PyFilerDirection, NEMSOutput):
    """switch for going from Restart.unf to Restart.HDF or vice versa. This takes the direction from the function
        get_args and then decides which format the restart code will go from to.

    Parameters
    ----------
    PyFilerDirection: string
        direction that comes from get_args. The direction will either be restart to hdf(r2h) or hdf to restart (h2r)

    Returns
    -------
    None:
        Depending on the PyFilerDirection, function writes either HDF or UNF formatted restart file.
    """

    # Calls ParseDict and parses
    NEMSFortTable, NEMSAttributesTable = ParseDict(user.file_map['dict.txt'])
    # Saving files into user class
    user.NEMSfortvars = NEMSFortTable
    user.NEMSfortatt = NEMSAttributesTable
    # user.NEMSVardf = NEMSVardf

    if PyFilerDirection.lower() in ["restart2hdf", 'r2h', '1']:
        # Code for Restart to HDF
        print("This will go from the Restart File to HDF")
        print(arguments)
        print(arguments.direction)
        print(arguments.curcalyr)
        print(arguments.iteration)
        # Retrieves CommonBlock, Variable Names, Dimension Parameters, and Extra Indeces and saves in dataframe
        NEMSVardf = RetrieveVarDim(NEMSFortTable, user.file_map['NEMS_report_ref_table'], NEMSAttributesTable)
        user.NEMSVardf = NEMSVardf
        user.file_map['RestartFile'] = arguments.input
        user.file_map['RestartHDF'] = arguments.output
        # Initializing PyFiler
        PyFiler(user.file_map['RestartFile'])
        # Sends the PyFiler initialized .pyd to NEMSHDFRestartIO.py and user class files for creation of the HDF file section
        # wrt_hdf.
        NEMSRestart.wrt_hdf(pyfiler, user)
    elif PyFilerDirection.lower() in ["hdf2restart", 'h2r', '2']:
        # Code for HDF to Restart
        print("This will go from the HDF to Restart File")
        print(arguments)
        print(arguments.direction)
        print(arguments.curcalyr)
        print(arguments.iteration)
        user.file_map['RestartFile'] = arguments.output
        user.file_map['RestartHDF'] = arguments.input
        # Initializing PyFiler
        pyfiler.utils.read_filer('restarti.unf')
        # Sends the PyFiler initialized .pyd to NEMSHDFRestartIO.py and user class files for creation/replacement of the restart.unf
        # file section update_restart_matrix.
        NEMSRestart.update_restart_matrix(user.file_map['RestartHDF'], pyfiler, NEMSOutput)
    else:
        print("Incorrect Direction Input for PyFiler")
        sys.exit("Fatal Error- Wrong format for input/output/direction. Please check")

def ParseDict(file_dict):
    """parses dict.txt (given as an input) and outputs the Fortran Variable Table and Attribute Table.

    Parameters
    ----------
    file_dict: dictionary file path from NEMS (dict.txt).

    Returns
    -------
    NEMSFortAttributesTable: dataframe
        consisting of the first half of dict.txt
    NEMSFortVarTable: dataframe
        consisting of the second half of dict.txt comprised of commonblock, variable names, dimensions, dimension parameters, etc
    """
    try:
        with open(file_dict) as resf:
            dictlines = resf.readlines()
    except:
        with open('input/dict.txt') as resf:
            dictlines = resf.readlines()
    dictlines = [xx for xx in dictlines if not xx.startswith('*')]
    dictlines = [yy for yy in dictlines if not yy.endswith('TYPE   UNITS  FUEL   SECTOR                   QUANTITIES')]

    index = [idx for idx, s in enumerate(dictlines) if 'QBLK' in s][0]
    index_ATT = [idx for idx, s in enumerate(dictlines) if 'ATT' in s][0]

    dictattrib = dictlines[:index - 1]
    comblock = dictlines[index:]
    dictattrib = dictattrib[:index_ATT]

    # Create DictAttributes in pandas dataframe
    n_rows = len(dictattrib)
    n_cols = 4
    NEMSFortranAttributesTable = pd.DataFrame(np.zeros((n_rows, n_cols)))
    NEMSFortranAttributesTable.columns = ['Dimensions', 'DimName', 'DimSize', 'DimDescrip']

    for jj in range(len(dictattrib)):
        dictparse = dictattrib[jj]
        DIM = str.rstrip(str.strip(dictparse[0:5]))
        FDIMNAM = str.rstrip(str.strip(dictparse[6:15]))
        FDIMSIZ = str.rstrip(str.strip(dictparse[16:23]))
        FDIMDESCRIP = str.rstrip(str.strip(dictparse[24:-1]))
        NEMSFortranAttributesTable.loc[jj] = DIM, FDIMNAM, FDIMSIZ, FDIMDESCRIP

    NEMSFortranAttributesTable = NEMSFortranAttributesTable.set_index('DimName', drop=False)

    # Create NEMS Fortran Variable Table in pandas dataframe vectorized

    dictparse = pd.Series(comblock)
    # Common Block
    ComBlockName = dictparse.str.slice(start=0, stop=17).str.replace(' ', '')
    # Fortran Variable
    NEMSFortVarName = dictparse.str.slice(start=18, stop=34).str.replace(' ', '')
    NEMSFortVarName = NEMSFortVarName.str.upper()
    # Real or Imaginary?
    NEMSReOrImagVal = dictparse.str.slice(start=35, stop=38).str.replace(' ', '')
    # Dimensions
    NEMSFortVarDim = dictparse.str.slice(start=38, stop=43)
    # Dimension Parameters
    NEMSFortVarParam = dictparse.str.slice(start=44, stop=79).str.replace(' ', '')
    NEMSFortVarParam = NEMSFortVarParam.str.lstrip('(')
    NEMSFortVarParam = NEMSFortVarParam.str.rstrip(')')
    NEMSFortVarType = dictparse.str.slice(start=80, stop=86).str.replace(' ', '')
    NEMSFortVarUnit = dictparse.str.slice(start=87, stop=93).str.replace(' ', '')
    NEMSFortVarFuel = dictparse.str.slice(start=94, stop=100).str.replace(' ', '')
    NEMSFortVarSect = dictparse.str.slice(start=101, stop=107).str.replace(' ', '')
    NEMSFortVarNote = dictparse.str.slice(start=108, stop=-1)

    NEMSFortVarTable = pd.concat([ComBlockName, NEMSFortVarName, NEMSReOrImagVal, NEMSFortVarDim, NEMSFortVarParam,
                                  NEMSFortVarType, NEMSFortVarUnit, NEMSFortVarFuel, NEMSFortVarSect, NEMSFortVarNote],
                                 axis=1)
    NEMSFortVarTable.columns = ["Common Block Name", "Fortran Variable Name", "Real or Imaginary",
                                "Dimensions", "Dimensions Parameters", "Parameters", "Units", "Fuel", "Sector", "Note"]

    NEMSFortVarTable = NEMSFortVarTable.set_index('Fortran Variable Name', drop=False)
    NEMSFortVarTable['Fortran Variable Name'].replace('', np.nan, inplace=True)
    NEMSFortVarTable.dropna(subset=['Fortran Variable Name'], inplace=True)

    return NEMSFortVarTable, NEMSFortranAttributesTable


def PyFiler(restart_file):
    """read Filer (FORTRAN PROGRAM) into python for PyFiler use by pyfiler.utils.(variablename). The file for
        this code part is pyfiler.f90 which houses all of the commonblocks as well as the Filer (FORTRAN) code. Reads using
        pyfiler.utils.read_file(restart_file)

    Parameters
    ----------
    restart_file: File path
        path to restart file to be loaded

    Returns
    -------
    None
        Loads restart file data into memory
    """

    t1 = time.time()
    try:
        # pyfiler reads in restart file using subroutine read_filer() from pyfiler.f90.
        pyfiler.utils.read_filer(restart_file)
        print(("FILER Init:", time.time() - t1, "seconds"))
    finally:
        pass


def PyFilerWrite(restart_file):
    """initializes Filer (FORTRAN PROGRAM) into python for PyFiler use by pyfiler.utils to prepare for writing.
        NOTE: The file for this code part is pyfiler.f90 which houses all of the commonblocks as well as the Filer
        (FORTRAN) code. This function initializes Filer using the command pyfiler.utils.init_filer()

    Parameters
    ----------
    restart_file: currently unused.

    Returns
    -------
    loads pyfiler.utils into memory
    """

    t1 = time.time()
    try:
        # initializes pyfiler using subroutine init_filer() from pyfiler.f90.
        # pyfiler.utils.read_filer(restart_file)
        pyfiler.utils.init_filer()
        print(("FILER Init:", time.time() - t1, "seconds"))
    finally:
        pass


def RetrieveVarDim(NEMSFortTable, excelbook, NEMSAttributeTable):
    """Construct a dataframe that includes columns for restart variable name, common block name, dimensions, and extra indexes

    Parameters
    ----------
    NEMSFortTable: dataframe of variable names and dimensions
        Typically this dataframe is the return value of function ‘ParseDict’

    excelbook: xlsx filename (e.g., ‘NEMS_restart_ref_tables.xlsx’)
        Provides common block, dimensions, and extra indeces for every NEMS restart variable

    Returns
    -------
    dataframe : columns are NEMS Fortran Variable Name, Common Block Name, Dimensions, and Extra Indeces necessary for
        conversion of restart to HDF and vice versa.
    """
    # this loop goes through the dimension parameters, creates an empty list of lists, then goes through individual
    # dimension parameters to select from the refExtraIndexdf the indeces associated to the dimension parameter.
    # For example, for MNUMYR, it would find all the years from 1990 to 2050 (or other years when that is updated).
    # The loop then appends the list to list_of_list and at the end places it into the appropriate column.
    # TODO: investigate if can be done using apply.
    if arguments.excel != '':
        # Given the variable name and common block in ref_restart sheet, look at NEMSFortTable, find column that has dimensions parameters.
        # Read in excelbook sheets for variables, parameter mapping, and extra indeces.
        dfs = pd.read_excel(excelbook, sheet_name='ref_restart', converters={'Fortan Variable Name': str,
                                                                             'Common Block Name': str, 'location': str,
                                                                             'dimensions': str, 'vartype': str,
                                                                             'note': str})
        # Build dataframe of variables and commonblock
        vardf = dfs[['var', 'commonblock']]
        vardf.index = vardf['var'].str.upper()
        # Rename columns to match NEMSFortTable
        vardf = vardf.rename(columns={'var': 'Fortran Variable Name', 'commonblock': 'Common Block Name'})
        # Get Dimension Parameters from NEMSFortTable using Variable name (MUST BE UPPER CASE)
        vardf['Dimension Params'] = NEMSFortTable.loc[vardf['Fortran Variable Name'].str.upper()][
            'Dimensions Parameters']
    else:
        # the else section of code works as a non-option passed through for the excel option for the restart file. If this
        # is the case, then PyFilerWrapper will output a full NEMS HDF Restart File
        dfs = NEMSFortTable
        dfs = dfs[['Fortran Variable Name', 'Common Block Name', 'Dimensions', 'Dimensions Parameters']]
        dfs['Fortran Variable Name'] = dfs['Fortran Variable Name'].str.lower()
        refParamMapdf = NEMSAttributeTable[['DimSize']]
        # Build dataframe of variables and commonblock
        vardf = dfs[['Fortran Variable Name', 'Common Block Name']]
        vardf.index = vardf['Fortran Variable Name'].str.upper()
        # Get Dimension Parameters from NEMSFortTable using Variable name (MUST BE UPPER CASE)
        vardf['Dimension Params'] = dfs['Dimensions Parameters']

    excelrefParamMapdf = pd.read_excel(user.file_map['NEMS_ref_param_map'], sheet_name='ref_parameter_mapping')
    excelrefExtraIndexdf = pd.read_excel(user.file_map['NEMS_ref_param_map'], sheet_name='ref_extra_index', index_col=0)
    # Split any Dimension Parameters that have more than one by commas
    vardf['Dimension Params'] = vardf['Dimension Params'].str.split(',')
    # Create new column for extra indeces, which will hold a list of lists for each extra index
    vardf['Extra Indeces'] = ''
    for jj in range(len(vardf['Dimension Params'])):
        list_of_list = []
        for kk in range(len(vardf['Dimension Params'][jj])):
            try:
                # TODO: Could do in pandas to clean up code and be more efficient
                c = str.strip(vardf['Dimension Params'][jj][kk])
                if c[-1] == ')':
                    c = c[:-1]
                a = excelrefParamMapdf.where(excelrefParamMapdf['parameter'] == c).dropna(how='all').dropna(axis=1)
                paramExtIndex = excelrefParamMapdf.iat[a.index[0], 1]
                b = excelrefExtraIndexdf.where(excelrefExtraIndexdf == paramExtIndex).dropna(how='all').dropna(
                    axis=1)
                list_of_list.append(list(b.index))
            except IndexError:
                # This denotes that the dimension parameter does not correspond to the list as a string and may need
                # converted to an integer.
                c = str.strip(vardf['Dimension Params'][jj][kk])
                if c[-1] == ')':
                    c = c[:-1]
                if str(c).isdigit():
                    c = int(c)
                # TODO: Could do in pandas to clean up code and be more efficient
                try:
                    a = excelrefParamMapdf.loc[c]['DimSize'][0]
                    paramExtIndex = excelrefParamMapdf.iat[a.index[0], 1]
                    b = excelrefExtraIndexdf.where(excelrefExtraIndexdf == paramExtIndex).dropna(how='all').dropna(
                        axis=1)
                    list_of_list.append(list(b.index))
                    c = str(c)
                    a = refParamMapdf.loc[c]['DimSize'][0]
                    a = int(a)
                    b = range(1, a + 1)
                    blist = []
                    for ll in b:
                        blist.append(ll)
                    list_of_list.append(blist)
                except:
                    # Indicating that an error occured in finding the extra indeces for the dimension parameters.
                    # list_of_list.append['Error in Finding Extra Indeces']
                    if isinstance(c, str):
                        if c[-1] == ')':
                            c = c[:-1]
                    if str(c).isdigit():
                        c = int(c)
                    NumRan = range(1, c + 1)
                    blist = []
                    for ll in NumRan:
                        blist.append(ll)
                    list_of_list.append(blist)
            except KeyError:
                c = str.strip(vardf['Dimension Params'][jj][kk])
                sys.exit("Fatal Error- Missing variable" + c)
            except:
                c = str.strip(vardf['Dimension Params'][jj][kk])
                if str(c).isdigit():
                    c = int(c)
                NumRan = range(1, c + 1)
                list_of_list.append(list(NumRan))

        vardf['Extra Indeces'][jj] = list_of_list
    print('End of ParseDict')
    return vardf


def update_restart_var(restart_filepath, var_name, new_data):
    """
    This fuction updates the restart variable directly in the HDF. Using HDF store, a temporary dataframe is created and
    then updates the value directly.
    Parameters
    ----------
    restart_filepath: filepath
    var_name: string
    new_data: array

    Returns
    -------
    None
    overwrites data, saves, and closes the HDF
    """

    with pd.HDFStore(restart_filepath) as dat:
        # In an HDF file, the entire variable must be replaced, so get a copy of the data and update it first,
        # and then set the whole thing back.
        temp_df = dat[var_name]
        if isinstance(new_data, pd.Series):
            # Single column df, so need to update the 'val' column
            temp_df['val'].update(new_data)
        else:
            temp_df.update(new_data)
        dat[var_name] = temp_df
    dat.close()


def profile(fun_in, temparg1, temparg2):
    """
    Profiles code in cumulative time to see what is taking the longest.
    Parameters
    ----------
    fun_in: Python Function
    temparg: Python Function Argument

    Returns
    -------
    Prints table of time in console.
    """

    import cProfile, pstats
    profiler = cProfile.Profile()
    profiler.enable()
    fun_in(temparg1, temparg2)
    profiler.disable()
    stats = pstats.Stats(profiler).sort_stats('cumtime')
    stats.print_stats()


if __name__ == "__main__":
    tstart = time.time()
    # pull arguments from command line
    arguments = get_args()
    # initialize user class to pass necessary files throughout PyFiler
    class user:
        pass
    user.file_map = {}
    # For running on own outside of NEMS, will need to comment out elif and uncomment else
    if os.getcwd()[-2:] == 'p1':
        user.file_map['dict.txt'] = os.path.abspath(os.path.join(os.getcwd(), "..")) + '\\PyFiler\\dict.txt'
        user.file_map['NEMS_report_ref_table'] = os.path.abspath(os.path.join(os.getcwd(), "..")) + '\\PyFiler\\' + arguments.excel
        user.file_map['NEMS_ref_param_map'] = os.path.abspath(os.path.join(os.getcwd(), "..")) + '\\PyFiler\\' + 'NEMS_ref_param_map.xls'
    # elif os.getcwd()[-2] not in ('p2', 'p3'):
    #     user.file_map['dict.txt'] = os.getcwd() + '\\PyFiler\\input\\dict.txt'
    #     user.file_map['NEMS_report_ref_table'] = os.getcwd() + '\\PyFiler\\input\\' + arguments.excel
    #     user.file_map['NEMS_ref_param_map'] = os.getcwd() + '\\PyFiler\\' + 'NEMS_ref_param_map.xls'
    else:
        #sys.exit('Error! Code Ending, look at line 61 of PyFilerWrapper')
        # debug use
        user.file_map['dict.txt'] = os.getcwd() + '\\input\\dict.txt'
        user.file_map['NEMS_report_ref_table'] = os.getcwd() +'\\'+ arguments.excel
        user.file_map['NEMS_ref_param_map'] = os.getcwd() + '\\' + 'NEMS_ref_param_map.xls'

    # Print Time for debug of launch times.
    timeObj = time.localtime()
    print('Current TimeStamp on Run is: %d-%d-%d %d.%d.%d' % (timeObj.tm_mon, timeObj.tm_mday, timeObj.tm_year,
                                                              timeObj.tm_hour, timeObj.tm_min, timeObj.tm_sec))

    # Sends arguments.direction to RestartHDF to select which direction the restart file is going (Restart.unf to hdf or vice versa)
    RestartHDF(arguments.direction, arguments.nemsrestartout)

    # Debug Profile for Understanding of Time required for each function.
    #profile(RestartHDF, arguments.direction,arguments.nemsrestartout)

    #prints total time of PyFiler Required
    print(("Total Time for PyFiler:", time.time() - tstart, "seconds"))