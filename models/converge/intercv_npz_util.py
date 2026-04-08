import numpy as np
import os
import pandas as pd
from ast import literal_eval


def write_to_npz(npz_start, npz_out, vars_list, dfd_updated, NEMSVardf):
    """Write out selected variables from list to .npz file

    This function lazy loads an existing .npz starting file, and loads selected variables from 
    a given list into memory. The selected variables are updated as arrays. Variables are transposed and/or
    their dimensions are rearrange to match with the .npz structure before writing out the output .npz file.

    Parameters
    ----------
    npz_start : str
        path to starting input npz file that will be read into memory

    npz_out : str
        filename or path of .npz restart file output to be written out
    
    vars_list : list
        list of variables that will be updated
    
    dfd_updated : dict
        dict of pandas.DataFrame variables with data to be written out
        
    NEMSVardf : pandas.DataFrame
        dataframe of all variables in the restart file with their dimensions
        
    Returns
    -------
    None
    
    """

    # keep first instance of variables (unadjusted)
    NEMSVardf = NEMSVardf[~NEMSVardf.index.duplicated(keep='first')]

    # create list of variables using existing keys if input list if blank
    if vars_list == []:
        vars_list = dfd_updated.keys()

    # lazy load starting .npz file
    npz_array = np.load(npz_start, allow_pickle=True)
    # create empty dictionary that will be used to combined all npz variables to be written out
    arrd = {}
    # create list of available keys from npz file
    keys_npz = list(npz_array.keys())
    # assign all variables from lazy load to memory
    for i in keys_npz:
        try:
            arrd[i] = npz_array[i]
        except TypeError:
            pass

    # create list of lowercase variables with "/" appended to front of string
    lowercased_rlx = [f"/{s.lower()}" for s in vars_list]
    tmp_list_var = []
    # create a list of commonblock and variable pairs
    for i in lowercased_rlx:
        for j in keys_npz:
            if j.endswith(i):
                tmp_list_var.append(j)
                break
    
    # updates variables loaded from the npz based on list of selected variables
    for i in tmp_list_var:
        tmp_var = i.split("/")[-1].upper()
        dim_len = len(NEMSVardf.loc[tmp_var]["Dimension Params"])

        # update if variable is 1D
        if dim_len == 1:
            arrd[i] = dfd_updated[tmp_var].to_numpy()[0]

        # transform and update if variable is 2D
        elif dim_len == 2:
            if arrd[i].shape == dfd_updated[tmp_var].to_numpy().shape:
                arrd[i] = dfd_updated[tmp_var].to_numpy()
            else:
                arrd[i] = dfd_updated[tmp_var].to_numpy().T
            
        # transform, rearrange, and update if variable is 3D
        elif dim_len == 3:

            left_index = dfd_updated[tmp_var].index[-1][0]
            right_index = dfd_updated[tmp_var].index[-1][1]
            tmp_dict = {}
            
            for j in range(1, right_index + 1):
                tmp_dict[j] = []
                for k in range(1, left_index + 1):
                    tmp_dict[j].append(dfd_updated[tmp_var].loc[(k,j)].tolist())
                tmp_dict[j] = np.array(tmp_dict[j])
            
            arrays = [tmp_dict[key] for key in sorted(tmp_dict.keys())]

            # rearrange the array dimensions to match the .npz structure
            for j in range(3):
                combined_array = np.stack(arrays, axis=j)

                if combined_array.shape == arrd[i].shape:
                    break

            arrd[i] = combined_array

    # write out the numpy array to .npz file
    np.savez_compressed(npz_out, **arrd)


def array_to_converge_dfd(varname, dat, NEMSVardf):
    """Transform numpy arrays into 1D or 2D dataframes

    Parameters
    ----------
    varname : list
        list of variables
    
    dat : dict
        dict of numpy arrays to be transformed into 1D or 2D
    
    NEMSVardf : pandas.DataFrame
        dataframe of variable dimensions from the restart file

    Returns
    -------
    dict
        dict of pandas.DataFrame variables
    
    """

    dfd = {}
    for var in varname:
        
        my_dim = NEMSVardf['Dimension Params'][var]

        if len(my_dim) == 1:
            df = pd.Series(dat[var])
            df.index += 1
            df = df.reset_index().rename(columns={'index': my_dim[0]})

        elif len(my_dim) == 2:
            df = pd.DataFrame(dat[var])
            df.index += 1
            df.columns += 1
            df = df.reset_index().rename(columns={'index': my_dim[0]})
            df = df.set_index(my_dim[: -1]).reset_index()
        
        elif len(my_dim) == 3:
            column_name = list(range(1, dat[var].shape[2] + 1))
            df = pd.DataFrame(dat[var].reshape(-1, dat[var].shape[2]), columns = column_name)

            var_shape = dat[var].shape
            second_dim_list_tmp = list(range(1, dat[var].shape[1] + 1))
            second_dim_list = []
            first_dim_list = []
            for i in range(1, var_shape[0] + 1):
                second_dim_list.extend(second_dim_list_tmp)
                first_dim_list.extend([i] * var_shape[1])
            df.insert(0, my_dim[0], first_dim_list)
            df.insert(1, my_dim[1], second_dim_list)

        
        # --------
        # prefix integer column names with 'M'
        if len(my_dim) == 1:
            i = my_dim[0]
            if str(i).isdigit():
                temp = f'M{i}'
                df = df.rename(columns={i: f'M{i}'})
            else:
                temp = i
            df = df.set_index(temp)

        elif len(my_dim) > 1:
            temp = []
            for i in my_dim[: -1]:
                if str(i).isdigit():
                    temp.append(f'M{i}')
                    df = df.rename(columns={i: f'M{i}'})
                else:
                    temp.append(i)
            df = df.set_index(temp)

            if str(my_dim[-1]).isdigit():
                temp = f'M{str(my_dim[-1])}'
            else:
                temp = str(my_dim[-1])
            df.columns.name = temp

        dfd[var] = df.copy()
    
    return dfd

def read_NEMSvardf():
    """Read in NEMSVardf.csv file as a pandas dataframe

    Parameters
    ----------
    None


    Returns
    -------
    pandas.DataFrame
        dataframe of variable dimensions from the restart file
    
    """

    # Read in NEMSVardf.csv as pandas dataframe
    NEMSVardf = pd.read_csv('NEMSVardf.csv', index_col = 'Fortran Variable Name')

    # Rename a dataframe column
    NEMSVardf.rename(columns={'Fortran Variable Name.1' : 'Fortran Variable Name'}, inplace=True)

    # Convert the string representation in 'Dimension Params'  to literals (list)
    NEMSVardf['Dimension Params'] = NEMSVardf['Dimension Params'].apply(lambda x: literal_eval(str(x)))

    # Convert the string representation in 'Extra Indeces'  to literals (list)
    NEMSVardf['Extra Indeces'] = NEMSVardf['Extra Indeces'].apply(lambda x: literal_eval(str(x)))

    return NEMSVardf

def read_npz_to_dfd(file_path, variable_list, NEMSVardf, adj_flag):
    """Read in restart variables from .npz restart file as 2D dataframes

    1. Load the npz and setup variable keys
    2. Filter the NEMSVardf and restart variables by using either Adjusted or Unadjusted variables
    3. Transform the dict of numpy arrays into dict of pandas.DataFrame of 1D or 2D
    4. Iterate through the dict of dataframe and transpose the dataframe if last dimension is not MNUMYR

    Parameters
    ----------
    file_path : str
        filepath to input .npz restart file
    
    variable_list : list
        list of variables to read into memory

    NEMSVardf : pandas.DataFrame
        dataframe of all variables in the restart file with their dimensions
    
    adj_flag : str
        flag to determine if read in the Adjusted or Unadjusted variables from the restart file


    Returns
    -------
    dict
        dict containing dataframes of variables read into memory transformed into 2 dimension
    
    """

    ################################
    ## 1. Load the npz and setup variable keys
    # Lazy load .npz file
    npz_array = np.load(file_path, allow_pickle=True)
    # Create a list of lowercase variables with "/" appended to the front of the string
    variable_list_lower = ["/" + x.lower() for x  in variable_list]
    # Create a list of variables from .npz keys
    key_var_list = list(npz_array.keys())
    ################################

    ################################
    ## 2. Filter the NEMSVardf and restart variables by using either Adjusted or Unadjusted variables
    # If using Unadjusted variables, keep first instance of variable occurance in NEMSVardf
    # Create a dict of Unadjusted variable numpy array by iterating though the npz file
    if adj_flag == "Unadjusted":
        NEMSVardf = NEMSVardf[~NEMSVardf.index.duplicated(keep='first')]
        dfd = {}
        for index, i in enumerate(variable_list_lower):
            for j in key_var_list:
                if j.endswith(i):
                    dfd[variable_list[index]] = npz_array[j]
                    break
    
    # If using Adjusted variables, keep last instance of variable occurance in NEMSVardf
    # Create a dict of Adjusted variable numpy array by iterating though the npz file
    elif adj_flag == "Adjusted":
        NEMSVardf = NEMSVardf[~NEMSVardf.index.duplicated(keep='last')]
        dfd = {}
        for index, i in enumerate(variable_list_lower):
            for j in reversed(key_var_list):
                if j.endswith(i):
                    dfd[variable_list[index]] = npz_array[j]
                    break
    ## end of 2. Filter the NEMSVardf and restart variables by using either Adjusted or Unadjusted variables
    ################################

    ################################
    ## 3. Transform the dict of numpy arrays into dict of pandas.DataFrame of 1D or 2D
    dfd = array_to_converge_dfd(variable_list, dfd, NEMSVardf)
    ################################

    ################################
    ## 4. Iterate through the dict of dataframe and transpose the dataframe if last dimension is not MNUMYR
    for k in dfd.keys():
        my_dim = NEMSVardf.loc[k, 'Dimension Params']
        x = len(my_dim)
        
        if 'MNUMYR' in my_dim:
            if x == 1:
                dfd[k] = dfd[k].T

            elif (x == 2) and (my_dim[-1] != 'MNUMYR'):
                dfd[k] = dfd[k].T

            elif (x > 2) and (my_dim[-1] != 'MNUMYR'):
                nonsense_string = 'nonsense_' + '_'.join(my_dim)
                z = dfd[k].copy()
                df_long = pd.melt(z.reset_index(), id_vars=z.index.names,
                                  value_vars=z.columns, value_name=nonsense_string)
                my_index = [i for i in df_long.columns if i not in [
                    'MNUMYR', nonsense_string]]
                dfd[k] = pd.pivot_table(df_long, values=nonsense_string, index=my_index, columns=[
                                        'MNUMYR'], aggfunc="sum")

            else:
                # x >= 2, but MNUMYR is in the columns (last dimension element)
                pass
    
    ## end of 4. Iterate through the dict of dataframe and transpose the dataframe if last dimension is not MNUMYR
    ################################

    return dfd

if __name__ == "__main__":

    print('Run file from convergence code main file')
    os.sys.exit()
