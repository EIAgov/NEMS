"""Utility file containing miscellaneous common functions used in CCATS.

Common: Summary
_______________
This is a utility file containing general functions that are either a) used frequently in a single submodule, or are used
by several submodules. Example functions include :func:`~ccats_common.common.read_dataframe`, which is a universal function for reading files into
dataframes, and :func:`~ccats_common.common.calculate_inflation` which applies the NEMS inflation multipliers to CCATS DataFrames. 

Convention for import alias is import common as com


Common: Input Files
___________________

Unique to each function.


Model Functions and Class Methods
_________________________________

* :func:`~ccats_common.common.read_dataframe` - Reads multiple filetypes into python as Pandas DataFrames, checking for nans.
* :func:`~ccats_common.common.calculate_inflation` - Inflation calculator using restart file inflation multiplier, applied to Pandas DataFrames.
* :func:`~ccats_common.common.array_to_df` - Creates a Pandas DataFrame from a NumPy array.
* :func:`~ccats_common.common.df_to_array` - Creates a NumPy array from a Pandas DataFrame.
* :func:`~ccats_common.common.unpack_pyomo` - Unpacks Pyomo results and converts results to DataFrames.
* :func:`~ccats_common.common.check_results` - Checks optimization termination condition.
* :func:`~ccats_common.common.align_index` - Aligns the index types of two tables based on the restart variable type.
* :func:`~ccats_common.common.compare_lists` - Check if list A is equal or a subset of list B.
* :func:`~ccats_common.common.check_dicts_ruleset` - Check the list of dicts against defined ruleset.


Common: Output Debug Files
__________________________
None


Common: Output Restart Variables
________________________________
None


Common: Code
____________
"""

import pandas as pd
import numpy as np
from itertools import product
import warnings

from ccats import logging
from ccats_common.common_debug import check_nans

logger = logging.getLogger('common.py')

def read_dataframe(filename, sheet_name=0, index_col=None, skiprows=None, to_int=True):
    """Reads multiple filetypes into python as pandas DataFrames, checking for nans.

    Parameters
    ----------
    
    filename : str
        Filename, including file type extension (e.g. .csv).
    
    sheet_name : str
        Name of the sheet if using excel or hdf.
    
    index_col : int, str, list, or False
        Column number to use as row labels.
    
    skiprows : int
        Column number to use as row labels.

    to_int : bool
        If True, values are converted to integers.

        
    Returns
    -------

    DataFrame
    """
    tablename = str(filename + '|' + str(sheet_name))
    logger.info('Loading Table: ' + tablename)

    df = pd.DataFrame

    if filename.split('.')[1] == 'xlsx':
        df = pd.read_excel(filename, sheet_name=sheet_name, index_col=index_col, skiprows=skiprows)
    elif filename.split('.')[1] == 'hdf':
        # Read_hdf does not return DataFrame, wrap with pd.DataFrame to satisfy PyCharm, may cause error
        df = pd.DataFrame(pd.read_hdf(filename, key=sheet_name))
        if type(index_col) == type('string'):
            df = df.set_index(index_col)
        elif type(index_col) == type(0):
            df = df.set_index(df.columns[index_col])
    elif filename.split('.')[1] == 'csv':
        df = pd.read_csv(filename, index_col=index_col, skiprows=skiprows, engine = 'c')
    else:
        warnings.warn(filename + ' filetype not recognized', UserWarning)
    
    # check if the dataframe has nans
    check_nans(df,tablename)
    
    if to_int:
        columns = list(df.columns)
        for col in range(len(columns)):
            try:
                columns[col] = int(columns[col])
            except ValueError:
                pass
        df.columns = columns

    return df


def calculate_inflation(rest_mc_jpgdp, from_year, to_year=None):
    """Inflation calculator using restart file inflation multiplier, applied to pandas DataFrames.

    Parameters
    ----------
    
    rest_mc_jpgdp : DataFrame
        DataFrame of NEMS restart file inflation multipliers indexed by year.

    from_year : int
        Starting year for inflation calculation.

    to_year : None or int
        Target year for inflation calculation, if None defaults to 1987.


    Returns
    -------
    
    float

    """
    if to_year is None:
        to_year = 1987 # Hardcoded in NEMS

    temp = rest_mc_jpgdp.at[(to_year), 'value'] / \
        rest_mc_jpgdp.at[(from_year), 'value']
    return temp


def array_to_df(array):
    """Create DataFrame from NumPy array.

        * Creates a multi-index DataFrame from a multi-dimensional NumPy array
        * Each array dimension is stored as a binary index column in the DataFrame multi-index
        * Multi-index columns are currently numbered to reflect array dimension level

    Parameters
    ----------
    array : NumPy array
        A multi-dimensional array containing restart file data

    Returns
    -------
    DataFrame

    """
    num_dims = array.ndim # Number of array dimensions
    column_options = list('1234567890') # List of numbered array dimensions
    columns = column_options[0:(num_dims)] # Set column numbers equal to numbered array dimensions
    shape = array.shape # Shape array
    index = pd.MultiIndex.from_product([range(s) for s in shape], names=columns) # Set df index
    df = pd.DataFrame({'value': array.flatten()}, index=index) # Create df from array with flatten

    return df


def df_to_array(df):
    """Turns multi-index DataFrame into multi-dimensional NumPy array.

        * Creates a multi-dimensional array from a multi-index dataframe
        * Each DataFrame multi-index is stored as an array dimension

    Parameters
    ----------

    df: DataFrame
        A multi-index DataFrame containing restart file data.

    Returns
    -------

    NumPy array
    """
    try: # Multi-index
        # Create an empty array of NaN of the right dimensions
        shape = list(map(len, df.index.levels))
        arr = np.full(shape, np.nan)

        # Fill the empty array using Numpy's advanced indexing
        arr[tuple(df.index.codes)] = df.values.flat

        # Set number of dimensional levels based on df index
        levels = map(tuple, df.index.levels)
        index = list(product(*levels))
        df = df.reindex(index)

        # Shape and create array
        shape = list(map(len, df.index.levels))
        array = df.values.reshape(shape)

    except: # 2-dimensional DF
        array = np.array(df)

    return array


def unpack_pyomo(variable, values, levels):
    '''Tool for unpacking Pyomo variable outputs and converting them to DataFrames.

    Parameters
    ----------
    
    variable: Pyomo Var
        Target variable to unpack.
    
    values: Pyomo Datatype (for example, pyo.value)
        Pyomo value type.
    
    levels: int
        Number of levels in array to unpack (i.e. CO\ :sub:`2 supplied from i is one level, while CO\ :sub:`2` piped from i to j is two levels).

    Returns
    -------

    df
    '''
    if levels == 1:
        pyomo_dict = {(i, v.name): values(v) for (i), v in variable.items()}

    elif levels == 2:
        pyomo_dict = {(i, j, v.name): values(v) for (i, j), v in variable.items()}

    elif levels == 3:
        pyomo_dict = {(i, j, k, v.name): values(v) for (i, j, k), v in variable.items()}

    df = pd.DataFrame.from_dict(pyomo_dict, orient="index", columns=["variable value"])
    df = df.reset_index()
    temp = pd.DataFrame(df['index'].tolist())
    df[temp.columns] = temp[temp.columns]

    return df


def check_results(results,SolutionStatus,TerminationCondition):
    '''Check Pyomo termination condition

    Parameters
    ----------
    
    results : pyomo.SolverResults
        Results from Pyomo.
    
    SolutionStatus : enum.EnumType
        Solution Status from Pyomo.
    
    TerminationCondition : enum.EnumType
        Termination Condition from Pyomo.

    Returns
    -------

    bool
        True = problem with solution, False = good solution.
    '''
    return (results is None) or (len(results.solution) == 0) or \
        (results.solution(0).status == SolutionStatus.infeasible) or \
        (results.solver.termination_condition == TerminationCondition.infeasible) or \
        (results.solver.termination_condition == TerminationCondition.unbounded)


def align_index(df1, df2, restart_var=None):
    """Aligns the index types of two tables based on the restart variable type.

        * If restart_var is provided we use that to determine the index types
        * If restart_var is none, we assume that df1 has the correct index type

    Parameters
    ----------
    
    df1: DataFrame (2 dimensional)
        Restart variable table.
    
    df2: DataFrame (2 dimensional)
        Restart variable table.
    
    restart_var: DataFrame (2 dimensional)
        Restart variable, used to decide the index type.

    Returns
    -------
    
    df1 : DataFrame (2 dimensional)
        Updated version of df1 parameter.
    
    df2 : DataFrame (2 dimensional)
        Updated version of df2 parameter.
    """
    index_type = ''
    if restart_var is not None:
        index_type = restart_var.index.dtype
    else:
        index_type = df1.index.dtype

    if index_type == 'int64':
        df1.index = df2.index.map(int)
        df2.index = df2.index.map(int)
    else:
        df1.index = df2.index.map(int)
        df2.index = df2.index.map(str)
    return df1, df2


def compare_lists(lista, listb):
    """Check if lista is equal or a subset of listb
        
    * used to check that node ids(supply, hubs, storage)in lista 
        are part of the set in listb
        
    Parameters
    ----------
    
    lista: list 
        First list for comparison.

    listb: list
        Second list for comparison.

    Returns
    -------
    
    None
    """
    if set(lista) ==  set(listb):
        return
    if set(lista).issubset(listb):
        return
    logger.error('sets do not match')
    raise Exception('sets do not match')


def check_dicts_ruleset(dict_list):
    """Check the list of dicts against defined ruleset
        
        * 1. check for nans
        * 2. make sure lengths of each dict are the same length
        
    Parameters
    ----------
    
    dict_list: list
        list of dicts

    Returns
    -------

    None
    """
    length = len(dict_list[0])
    for d in dict_list:
        # check for nans
        if any(np.isnan(val) for val in d.values()):
            logger.error('dicts have nans')
            raise Exception('sets do not match')
        # make sure lengths are the same size
        if len(d) != length:
            logger.error('dict length is incorrect')
            raise Exception('dict length is incorrect')

