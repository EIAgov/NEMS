"""Module containing miscellaneous common functions used in hsm.

Notes
-----
Convention for import alias is import common as com

"""

import pandas as pd
import numpy as np
from itertools import product
import inspect
import warnings
import names as nam


def print_out(text_in):
    """Prints text to output with tabs denoting stack depth.

    Parameters
    ----------
    text_in : str, int, float

    Returns
    -------
    None
    """
    space = (len(inspect.stack(0)) - 3) * '\t'
    print(space+str(text_in))


def read_dataframe(filename, sheet_name=0, index_col=None, skiprows=None, to_int=True):
    """Prints text to output with tabs denoting stack depth.

    Parameters
    ----------
    filename : str
        Filename, including file type extension (e.g. .csv)
    sheet_name : str
        Name of the sheet if using excel or hdf
    index_col : int, str, list, or False
        Column number to use as row labels
    skiprows : int
        Column number to use as row labels

    Returns
    -------
    pd.DataFrame
    """
    print_out('Loading Table: ' + str(filename + '|' + str(sheet_name)))

    df = pd.DataFrame

    if filename.split('.')[1] == 'xlsx':
        df = pd.read_excel(filename, sheet_name=sheet_name, index_col=index_col, skiprows=skiprows)
    elif filename.split('.')[1] == 'hdf':
        # read_hdf does not return DataFrame, wrap with pd.DataFrame to satisfy PyCharm, may cause error
        df = pd.DataFrame(pd.read_hdf(filename, key=sheet_name))
        if type(index_col) == type('string'):
            df = df.set_index(index_col)
        elif type(index_col) == type(0):
            df = df.set_index(df.columns[index_col])
    elif filename.split('.')[1] == 'csv':
        df = pd.read_csv(filename, index_col=index_col, skiprows=skiprows, engine = 'c')
    else:
        warnings.warn(filename + ' filetype not recognized', UserWarning)

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
    """Calculates inflation.

    Returns
    -------
    self.rest_mc_jpgdp
    """
    if to_year is None:
        to_year = 1987 #hardcoded in NEMS

    temp = rest_mc_jpgdp.at[(to_year), 'value'] / \
        rest_mc_jpgdp.at[(from_year), 'value']
    return temp


def assign_ngpls_to_district(df, ngpl_shares, steo_years):
    """Assigns historic ngpl volumes to a district number based on ngpl shares.

    Parameters
    ----------
    df : df
        Dataframe of historic ngpl production
    ngpl_shares : df
        DataFrame of NGPL shares by HSM district
    steo_years : list
        List of STEO years


    Returns
    -------
    pd.DataFrame
    """
    df = ngpl_shares[[nam.ngpl_region_number, nam.ngpl_share_ratio]].merge(df,
                                               how='left', left_on=nam.ngpl_region_number, right_index=True)
    df = df.fillna(0.0)  # fill ngpl district 99s
    for year in steo_years:
        df[year] = df[year].mul(df[nam.ngpl_share_ratio])
    df = df.drop([nam.ngpl_region_number, nam.ngpl_share_ratio], axis=1)
    return df


def df_interpolate_2(left, right, xcol, ycol, xdir='nearest', ydir='nearest',
                     xmerge='merge_asof'):
    """Merges field size classes in the Offshore submodule.

    Parameters
    ----------
    left : df
        Left merge df
    right : df
        Right merge df
    xcol
        Left merge column
    ycol
        Right merge column
    xdir
        Left direction for "merge_asof"
    ydir
        Right direction for "merge_asof"
    xmerge : str
        Type of merge


    Returns
    -------
    pd.DataFrame
    """

    df_in = left[[xcol, ycol]].reset_index().copy()
    right = right.reset_index().copy()

    right = right.sort_values([xcol, ycol])
    df_in = df_in.sort_values([xcol, ycol])
    right[xcol + '_round'] = right[xcol]
    df = pd.DataFrame()
    if xmerge == 'merge_asof':
        df = pd.merge_asof(df_in, right[[xcol, xcol + '_round']],
                           on=xcol,
                           direction=xdir)
    elif xmerge == 'merge':
        # basically no rounding necessary, just copy the column with _round
        df = df_in
        df[xcol + '_round'] = df[xcol].copy()
    else:
        warnings.warn('Invalid merge type in offshore interpolation', UserWarning)
        exit()
        pass

    right = right.sort_values([ycol, xcol])
    df = df.sort_values([ycol, xcol])
    right[ycol + '_round'] = right[ycol]
    df = pd.merge_asof(df, right[[ycol, ycol + '_round']], direction=ydir)

    df = pd.merge(df, right,
                  left_on=[xcol + '_round', ycol + '_round'],
                  right_on=[xcol, ycol],
                  how='left',
                  suffixes=[None, '_y'])
    df = df.drop(columns=[xcol + '_round', ycol + '_round',
                          xcol + '_round_y', ycol + '_round_y',
                          xcol + '_y', ycol + '_y',
                          xcol, ycol])

    # note, this will probably not work if the index of left is named,
    # resetting index (see beginning) assumes the name of 'index'
    df = df.set_index('index')
    return df




def leg_cost_conversion(cost, capacity, discount_rate, plant_life, operating_factor):
    """! calculate costs in 10**6 dollars (see reference (2)).

        * Then convert to $/MCF via subroutine calc_ucc, which assumes that the capacity is in MMCF/day

        * Costs for each impurity will be generated only if impurity %'s are above associated limits

        * input cost = capital costs ($10**6)
        * ac   = annual capital charge ($10**6/year)
        * output cost = unitized capital cost ($/MCF)
        * avt  = average volume thoughput ($10**6 cubic feet/day) (calendar basis)
        * Note: capacity is expressed in MMCF/day)
    """
    ac = cost * discount_rate / (1.00 - np.exp(-discount_rate * plant_life))
    avt = capacity * 365.00 * operating_factor
    cost = ac * (1000000 / 1000) / (avt)

    return cost



def array_to_df(array):
    """Create df from array.

        * Creates a multi-index DataFrame from a multi-dimensional array
        * Each array dimension is stored as a binary index column in the DataFrame multi-index
        * Multi-index columns are currently numbered to reflect array dimension level

    Parameters
    ----------
    series : array
        A multi-dimensional array containing restart file data

    Returns
    -------
    df

    """
    num_dims = array.ndim #number of array dimensions
    column_options = list('1234567890') #list of numbered array dimensions
    columns = column_options[0:(num_dims)] #Set column numbers equal to numbered array dimensions
    shape = array.shape #Shape array
    index = pd.MultiIndex.from_product([range(s) for s in shape], names=columns) #Set df index
    df = pd.DataFrame({'value': array.flatten()}, index=index) #create df from array with flatten

    return df


def df_to_array(df):
    """Turns multi-index DataFrame into multi-dimensional array.

        * Creates a multi-dimensional array from a multi-index dataframe
        * Each DataFrame multi-index is stored as an array dimension

    Parameters
    ----------
    series : df
        A multi-index DataFrame containing restart file data

    Returns
    -------
    array

    """
    try: #Multi-index
        #create an empty array of NaN of the right dimensions
        shape = list(map(len, df.index.levels))
        arr = np.full(shape, np.nan)

        #fill the empty array using Numpy's advanced indexing
        arr[tuple(df.index.codes)] = df.values.flat

        #Set number of dimensional levels based on df index
        levels = map(tuple, df.index.levels)
        index = list(product(*levels))
        df = df.reindex(index)

        #shape and create array
        shape = list(map(len, df.index.levels))
        array = df.values.reshape(shape)

    except: # 2-dimensional DF
        array = np.array(df)

    return array