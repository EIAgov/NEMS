import os
import pandas as pd
import numpy as np

from PyFiler import PyFilerWrapper as pfw


def export_restart_for_convergence(pyfiler1, variable_list, NEMSVardf, adj_flag, skipdat=0):
    """converts the pyfiler numpy array to dictionary array for convergence processing
        output a dictionary array of processed dataframes

    Parameters
    ----------
    pyfiler1 : module
        pyfiler fortran module

    variable_list : list
        list of variables used for iteration-convergence and relax algorithm
    
    NEMSVardf : dat_processedaFrame
        dat_processedaFrame of variables, common block, and dimensions available in the restart file
        
    skipdat : bianry
        a binary input that skips creating the dat dataframes, which are used in the write_restart
    
    Returns
    -------
    dat
        dict containing all variables available in the restart file in pandas.dat_processedaFrame format
    dat_processed
        dict containing all variables available in the restart file in pandas.dat_processedaFrame format,
        processed similarly to the read_vars from restart_util.py 
    """
    def prefix_numeric_col(col_name):
        return f'M{col_name}' if str(col_name).isdigit() else col_name
    
    def set_column_name(my_dim, dat):
        if len(my_dim) == 1:
            temp = prefix_numeric_col(my_dim[0])
            if temp != my_dim[0]:
                dat.rename(columns={my_dim[0]: temp}, inplace=True)
            dat = dat.set_index(temp)

        elif len(my_dim) > 1:
            # Set columns name
            last_dim_name = prefix_numeric_col(my_dim[-1])
            dat.columns.name = last_dim_name
        
        return dat
    
    # Create a local dict variable to hold values for calculations
    dat_processed = {}
    dat = {}
    dat['_varrange'] = pd.Series("", index = NEMSVardf.index)
    dat['_variablelisting'] = NEMSVardf
    
    # get the variablelisting copy for using carbon-adjusted prices or unadjusted prices
    variablelisting = dat['_variablelisting']
    #drop duplicate indices
    if adj_flag == 'Unadjusted': # use unadjusted prices from mpblk, ngtdmout, coalprc common blocks
        variablelisting = variablelisting[~variablelisting.index.duplicated(keep='first')]
        
    elif adj_flag == 'Adjusted': # use carbon-adjusted prices from ampblk, angtdm, acoalprc common blocks
        variablelisting = variablelisting[~variablelisting.index.duplicated(keep='last')]

    # Assign integer variables from NCNTRL list to local dict
    for i in ['curiyr','curitr','maxitr','irelax','ctest']:
        # create the series for the dat
        if skipdat == 0 :
            dat[f'NCNTRL/{i}']=pd.Series(getattr(pyfiler1.ncntrl,i))
        
        # create the processed series for the integer variables
        temp_series = pd.Series(getattr(pyfiler1.ncntrl, i))
        temp_series.index = temp_series.index + 1
        dat_processed[i.upper()] = (temp_series.reset_index().rename(columns={'index': '1'}))
        dat_processed[i.upper()] = set_column_name(['1'], dat_processed[i.upper()])

    # Assign numpy array variables from NCNTRL list to local dict
    for i in ['cnvtst']:
        # create the series for the dat
        if skipdat == 0:
            dat[f'NCNTRL/{i}']=pd.DataFrame(getattr(pyfiler1.ncntrl,i))
        
        # create the processed series for the integer variables, this one is 2 dimensional
        my_dim = variablelisting.loc[i.upper(), 'Dimension Params']
        z = getattr(pyfiler1.ncntrl,i)
        row_index = np.arange(1, z.shape[0] + 1)
        col_index = np.arange(1, z.shape[1] + 1)
        dat_processed[i.upper()] = pd.DataFrame(z, index = row_index, columns = col_index)
        dat_processed[i.upper()].index.name = my_dim[0]
        dat_processed[i.upper()] = set_column_name(my_dim, dat_processed[i.upper()])
        
    if skipdat == 0:
        for i in variable_list:
            #if its a single series, the length will be four.  Otherwise, there are two series.
            # measure=len(dat['_variablelisting'].loc[("{sub}".format(sub=i)).upper()])
            cb=dat['_variablelisting'].loc[("{sub}".format(sub=i)).upper()]['Common Block Name']
            #if there is one series, you are just pulling out the series, so you put it in the list
            #rather than measuring the number of series, you are getting the number of elements1
            if isinstance(cb, str):
                cb=[cb]
            for cname in cb:
                if cname.lower()=='ampblk': continue
                try:
                    pyfiler_combine=getattr(pyfiler1,cname.lower())
                    z=getattr(pyfiler_combine, i)
                    variable_dim_len = len(z.shape)
                    if variable_dim_len <= 2:
                        dat[cname+'/'+i] = pd.DataFrame(z)
                    elif variable_dim_len == 3:
                        tmp_dim = len(getattr(pyfiler1.utils, i)[:])
                        for j in range(0, tmp_dim):
                            dat[f'{cname}/{i}/{j}']=pd.DataFrame(z[j])

                # If the above variable does not exist in the commonblock, raise the attribute error and search in the .utils module
                except AttributeError:
                    z = getattr(pyfiler1.utils, i)
                    variable_dim_len = len(z.shape)
                    if variable_dim_len <= 2:
                        dat[cname+'/'+i] = pd.DataFrame(z)
                    elif variable_dim_len == 3:
                        tmp_dim = len(z[:])
                        for j in range(0, tmp_dim):
                            dat[f'{cname}/{i}/{j}']=pd.DataFrame(z[j])
                except:
                    pass

    for i in variable_list:
        #if its a single series, the length will be four.  Otherwise, there are two series.
        # measure=len(dat['_variablelisting'].loc[("{sub}".format(sub=i)).upper()])
        cb=variablelisting.loc[("{sub}".format(sub=i)).upper()]['Common Block Name']
        #if there is one series, you are just pulling out the series, so you put it in the list
        #rather than measuring the number of series, you are getting the number of elements1
        if isinstance(cb, str):
            cb=[cb]
        for cname in cb:
            if cname.lower()=='ampblk': continue
            
            # get the dims of the variables in the list
            my_dim = variablelisting.loc[i.upper(), 'Dimension Params']

            # The variable qclelcdr which have dimensions start with '2' and is the only variable that has that
            my_dim = ['M' + item if item.isdigit() else item for item in my_dim]
            try:
                pyfiler_combine=getattr(pyfiler1,cname.lower())
                z=getattr(pyfiler_combine, i)
                variable_dim_len = len(z.shape)
                if variable_dim_len == 1:
                    # process the arrays into a dataframe while setting the indexes to start at 1
                    row_index = np.arange(1, z.shape[0] + 1)
                    dat_processed[i.upper()] = pd.DataFrame(z, index = row_index)
                    dat_processed[i.upper()] = dat_processed[i.upper()].reset_index().rename(columns={'index': my_dim[0]})
                if variable_dim_len == 2:
                    # process the arrays into a dataframe while setting the indexes to start at 1
                    row_index = np.arange(1, z.shape[0] + 1)
                    col_index = np.arange(1, z.shape[1] + 1)
                    dat_processed[i.upper()] = pd.DataFrame(z, index = row_index, columns = col_index)
                    dat_processed[i.upper()].index.name = my_dim[0]
                elif variable_dim_len == 3:
                    # process the arrays into one dataframe using multi-indexing
                    num_multi_index_dims = len(z.shape) - 1
                    index_levels = [np.arange(1, z.shape[j] + 1) for j in range(num_multi_index_dims)]
                    multi_index_names = my_dim[:num_multi_index_dims]
                    multi_index = pd.MultiIndex.from_product(index_levels, names=multi_index_names)
                    contents_reshaped = z.reshape(-1, z.shape[-1])
                    dat_processed[i.upper()] = pd.DataFrame(contents_reshaped, index=multi_index)
                
                dat_processed[i.upper()] = set_column_name(my_dim, dat_processed[i.upper()])

            # If the above variable does not exist in the commonblock, raise the attribute error and search in the .utils module
            # This currently only happens to the variable qclclnr or qclc1nr
            except AttributeError:
                # save the contents from the pyfiler1.utils
                z = getattr(pyfiler1.utils, i)
                variable_dim_len = len(z.shape)
                if variable_dim_len == 1:
                    # process the arrays into a dataframe while setting the indexes to start at 1
                    # for one dimension, this is saved as a row index
                    row_index = np.arange(1, z.shape[0] + 1)
                    dat_processed[i.upper()] = pd.DataFrame(z, index = row_index)
                    dat_processed[i.upper()] = dat_processed[i.upper()].reset_index().rename(columns={'index': my_dim[0]})
                if variable_dim_len == 2:
                    # process the arrays into a dataframe while setting the indexes to start at 1
                    # for two dimension, this is saved as a row index and a column index
                    row_index = np.arange(1, z.shape[0] + 1)
                    col_index = np.arange(1, z.shape[1] + 1)
                    dat_processed[i.upper()] = pd.DataFrame(z, index = row_index, columns = col_index)
                    dat_processed[i.upper()].index.name = my_dim[0]
                elif variable_dim_len == 3:   
                    # process the arrays into one dataframe using multi-indexing
                    num_multi_index_dims = len(z.shape) - 1
                    index_levels = [np.arange(1, z.shape[j] + 1) for j in range(num_multi_index_dims)]
                    multi_index_names = my_dim[:num_multi_index_dims]
                    multi_index = pd.MultiIndex.from_product(index_levels, names=multi_index_names)
                    contents_reshaped = z.reshape(-1, z.shape[-1])
                    dat_processed[i.upper()] = pd.DataFrame(contents_reshaped, index=multi_index)
                    
                dat_processed[i.upper()] = set_column_name(my_dim, dat_processed[i.upper()])
                        
            except:
                pass
    
    for k in dat_processed.keys():
        my_dim = variablelisting.loc[k, 'Dimension Params']
        x = len(my_dim)
        
        if 'MNUMYR' in my_dim:
            if 'MNUMYR' in my_dim and (x == 1 or (x == 2 and my_dim[-1] != 'MNUMYR')):
                dat_processed[k] = dat_processed[k].T
                
            elif (x > 2) and (my_dim[-1] != 'MNUMYR'):
                nonsense_string = 'nonsense_' + '_'.join(my_dim)
                z = dat_processed[k].copy()
                
                id_vars_melt = z.index.names
                if isinstance(id_vars_melt, pd.core.indexes.base.Index):
                    id_vars_melt = list(id_vars_melt)
                
                nonsense_string = f'nonsense_{"_".join(my_dim)}'
                
                df_long = z.reset_index().melt(id_vars=id_vars_melt,
                                    value_vars=z.columns,
                                    value_name=nonsense_string)
                
                my_index = [col for col in df_long.columns if col not in ['MNUMYR', nonsense_string]]
                
                dat_processed[k] = df_long.pivot_table(values=nonsense_string,
                                            index=my_index,
                                            columns=['MNUMYR'],
                                            aggfunc="sum")
            else:
                pass
            
    return dat, dat_processed

if __name__ == "__main__":
    """Program cannot be executed independently and must be launched from nems_flow.py
    """

    print("Program cannot be executed independently and must be launched from nems_flow.py")
    print("exiting program now.")
    os.sys.exit
