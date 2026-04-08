import os
import pandas as pd
import time
import numpy as np

# parse_parametr_include_file---------------------------------------------------
def parse_parameter_file(f_in):  # ='parametr.txt'):
    parametr_include_file = f_in

    with open(parametr_include_file) as file:
        lines = [line.strip() for line in file]

    d = {}
    for i in lines:
        if i.startswith('PARAMETER('):
            z = i[i.find('(')+1:i.find(')')].split('=')
            try:
                d.update({z[0]: int(z[1])})
            except:

                s = z[1].split('+')
                temp = []
                for j in s:
                    if not j.isdigit():
                        temp.append(str(d[j]))
                    else:
                        temp.append(j)
                d.update({z[0]: eval('+'.join(temp))})

    return d

# write_NEMS_HDF ----------------------------------------------------------------

def write_var(varname, df, dat, variablelisting):
    """updates the dict of dataframe with relaxed values

    Parameters
    ----------
    varname : str
        name of the convergence variable (e.g. PELRS) in upper case
    df : DataFrame
        DataFrame of the updated variable values
    dat : dict
        dict of dataframe for convergence variables and values
    variablelisting : DataFrame
        DataFrame of variables, common block, and dimensions available in the restart file (NEMSVardf)

    Returns
    -------
    dict
        dictionary of dataframe with updated variable value
    """

    my_var = varname
    temp = variablelisting.loc[my_var.upper(), 'Common Block Name']
    if isinstance(temp, str):
        common_block = temp
        my_dim = variablelisting.loc[my_var.upper(), 'Dimension Params']

    else:
        common_block = temp.iloc[0]
        my_dim = variablelisting.loc[my_var.upper(), 'Dimension Params'].iloc[0]

    assert isinstance(common_block, str)
    

    if len(my_dim) == 1:
        i = my_dim[0]
        if str(i).isdigit():
            temp = [f'M{i}']
        else:
            temp=[i]
        
        if df.index.names !=temp:
            df = df.T
        df = df.reset_index(drop=True)
        df.columns = pd.RangeIndex(df.columns.size)
        dat[f'{common_block}/{my_var.lower()}'] = df


    elif len(my_dim) > 1:
        temp = []
        for i in my_dim:
            if str(i).isdigit():
                temp.append(f'M{i}')
            else:
                temp.append(i)

        if len(my_dim) == 2:
            if df.stack().index.names !=temp:
                df = df.T
            df = df.reset_index(drop=True)
            df.columns = pd.RangeIndex(df.columns.size) 
            dat[f'{common_block}/{my_var.lower()}'] = df

        elif len(my_dim) == 3:
            if df.stack().index.names !=temp:
                df = df.stack().reset_index().pivot( index= temp[: -1], columns=temp[-1])


            for i in range(1, df.index.get_level_values(0).nunique()):
                dft = df.xs(i+1, level=0, drop_level=True)
                dft = dft.reset_index(drop=True)
                dft.columns = pd.RangeIndex(dft.columns.size)
                dat[f'{common_block}/{my_var.lower()}/{i}'] = dft

    return dat
    
def write_restart(dat, my_vars, dfd):
    """updates the dict of dataframe with relaxed variable values

    Parameters
    ----------
    dat : dict
        dict of convergence variables in dataframes, not yet updated values
    my_vars : list
        list of variables from restart file used for convergence and relaxation
    dfd : dict
        dict of convergence variables in dataframes, contains updated values, data not yet transformed

    Returns
    -------
    dict
        dict of convergence variables in dataframes, updated values

    Raises
    ------
    Exception
        if variable does not exist in the restart file
    """
    
    variablelisting = dat['_variablelisting']
    variablelisting = variablelisting[~variablelisting.index.duplicated(keep='first')] # drop duplicate indeces

    for varname in my_vars:
        try:
            df = dfd[varname]
            dat = write_var(varname, df, dat, variablelisting)

        except Exception as e:
            # Stop execution if the variable is not available in restart
            raise Exception(f"Error writing variable '{varname}': {str(e)}")    
        
    return dat