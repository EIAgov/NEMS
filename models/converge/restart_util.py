# read_NEMS_HDF ----------------------------------------------------------------
import os
import pandas as pd
# import time
# from RW_preprocessor_sme import preprocessor_sme


def read_var(varname, dat, variablelisting):

    # my_var = varname
    # common_block = variablelisting.loc[my_var.upper(), 'Common Block Name']
    # my_dim = variablelisting.loc[my_var.upper(), 'Dimension Params']
    # my_dim_list = variablelisting.loc[my_var.upper(), 'Extra Indeces']
    # # another approach: my_dim_list = varrange.loc[my_var.upper()]

    # Mike modified on 1/5/2024
    my_var = varname
    temp = variablelisting.loc[my_var.upper(), 'Common Block Name']
    extra_indeces_3d = []

    if isinstance(temp, str):
        common_block = temp
        my_dim = variablelisting.loc[my_var.upper(), 'Dimension Params']
        my_dim_list = variablelisting.loc[my_var.upper(), 'Extra Indeces']
    else:
        # print(f'multiple common blocks for: {varname}')
        # iloc[0] for P variables which are in two common blocks
        common_block = temp.iloc[0]
        my_dim = variablelisting.loc[my_var.upper(
        ), 'Dimension Params'].iloc[0]
        my_dim_list = variablelisting.loc[my_var.upper(
        ), 'Extra Indeces'].iloc[0]
    assert isinstance(common_block, str)
    
    if len(my_dim) == 1:
        df = dat[f'{common_block}/{my_var.lower()}']
        df.index += 1
        df = df.reset_index() \
               .rename(columns={'index': my_dim[0]})

    elif len(my_dim) == 2:
        df = dat[f'{common_block}/{my_var.lower()}']
        df.index += 1
        df.columns += 1
        df = df.reset_index() \
               .rename(columns={'index': my_dim[0]})
        df = df.set_index(my_dim[: -1]).reset_index()

    elif len(my_dim) == 3:
        L = []
        extra_indeces_counter = 0

        for i in range(100):
            
            try:
                extra_indeces_counter += 1
                df = dat[f'{common_block}/{my_var.lower()}/{i}']
                df.index += 1
                df.columns += 1
                df = df.reset_index().rename(columns={'index': my_dim[1]})
                df.loc[:, my_dim[0]] = i + 1
                L.append(df.copy())
                extra_indeces_3d.append(extra_indeces_counter)

            except KeyError:
                #print(f'End of: \t{common_block}/{my_var.lower()}/{i}')
                break

        df = pd.concat(L).set_index(my_dim[: -1]).reset_index()

    # elif len(my_dim) == 3:
    #     L = []
    #     for i, j in enumerate(my_dim_list[0]):
    #         df = dat[f'{common_block}/{my_var.lower()}/{i}']
    #         df.index += 1
    #         df.columns += 1
    #         df = df.reset_index() \
    #                .rename(columns={'index': my_dim[1]})
    #         df.loc[:, my_dim[0]] = i + 1
    #         L.append(df.copy())
    #     df = pd.concat(L).set_index(my_dim[: -1]).reset_index()

    elif len(my_dim) == 4:
        print("There are 4 dimensions in a convergence variable. Exiting")
        os.sys.exit()
        # L = []
        # for i, j in enumerate(my_dim_list[0]):
        #     for k, m in enumerate(my_dim_list[1]):
        #         # print(j, m)
        #         df = dat[f'{common_block}/{my_var.lower()}/{i}/{k}']
        #         df.index += 1
        #         df.columns += 1
        #         df = df.reset_index() \
        #                .rename(columns={'index': my_dim[2]})
        #         df.loc[:, my_dim[0]] = i + 1
        #         df.loc[:, my_dim[1]] = k + 1
        #         L.append(df.copy())
        # df = pd.concat(L).set_index(my_dim[: -1]).reset_index()

    # ---------
    # construct new dataframe with:
    #     convert non-year index items to strings;
    #     convert years from 1-based to 1990-based

    my_dim_dict = {}
    tmpdat1 = {}
    for i in range(1, df[my_dim[0]].nunique() + 1):
        tmpdat1[i] = i

    my_dim_dict[my_dim[0]] = tmpdat1

    if len(my_dim) == 2:

        tmpdat2 = {}
        for i in range(1, df.shape[1]):
            tmpdat2[i] = i
        
        my_dim_dict[my_dim[1]] = tmpdat2
    
    if len(my_dim) == 3:

        tmpdat2 = {}
        for i in range(1, df[my_dim[1]].nunique() + 1):
            tmpdat2[i] = i
        
        my_dim_dict[my_dim[1]] = tmpdat2

        tmpdat3 = {}
        for i in range(1, df.shape[1] - 1):
            tmpdat3[i] = i
        
        my_dim_dict[my_dim[2]] = tmpdat3

    # my_dim_dict = {}
    # for i, j in enumerate(my_dim):
    #     my_dim_dict[j] = {j+1: my_dim_list[i][j]
    #                       for j in range(len(my_dim_list[i]))}

    df2 = df.copy()
    if len(my_dim) == 1:
        i = my_dim[0]
        df2[i] = df2[i].apply(lambda x: my_dim_dict[i][x])
        df2 = df2.rename(columns=my_dim_dict[my_dim[0]])
    else:
        for i in my_dim[: -1]:
            df2[i] = df2[i].apply(lambda x: my_dim_dict[i][x])
        df2 = df2.rename(columns=my_dim_dict[my_dim[-1]])

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
        df2 = df2.set_index(my_dim[0])

    elif len(my_dim) > 1:
        temp = []
        for i in my_dim[: -1]:
            if str(i).isdigit():
                temp.append(f'M{i}')
                df = df.rename(columns={i: f'M{i}'})
            else:
                temp.append(i)
        # df = df.set_index(my_dim[: -1])
        df = df.set_index(temp)
        df2 = df2.set_index(my_dim[: -1])

        if str(my_dim[-1]).isdigit():
            temp = f'M{str(my_dim[-1])}'
        else:
            temp = str(my_dim[-1])
        df.columns.name = temp
        df2.columns.name = temp
    # --------

    # return raw dataframe and 'prettified' dataframe
    return df, df2


def read_stuff(dat, my_vars, adj_flag):
    # dat = pd.HDFStore(restarthdf, 'r')
    variablelisting = dat['_variablelisting']
    #drop duplicate indices
    if adj_flag == 'Unadjusted': # use unadjusted prices from mpblk, ngtdmout, coalprc common blocks
        variablelisting = variablelisting[~variablelisting.index.duplicated(keep='first')]
        
    elif adj_flag == 'Adjusted': # use carbon-adjusted prices from ampblk, angtdm, acoalprc common blocks
        variablelisting = variablelisting[~variablelisting.index.duplicated(keep='last')]
        
        
    dfd = {}
    # #for varname in nems_vars['sup_prd']['v']:
    # for varname in my_vars:
    #     df = read_var(varname, dat, variablelisting)
    #     dfd[varname] = df.copy()

    for varname in my_vars:
        # print(varname)
        try:
            df, df2 = read_var(varname, dat, variablelisting)
            dfd[varname] = df.copy()
        # except:
        #     # Ignore dimensions, constant variables, and values
        #     print(f'varname==========: {varname}')  
        except Exception as e:
            # Stop execution if the variable is not available in restart
            raise Exception(f"Error reading variable '{varname}': {str(e)}")

    # if table has dimension >= 2 and MNUMYR is an elemment, then make MNUMYR the columns
    # note: need a nonsense string that is guaranetted not to be in the dimension elements
    for k in dfd.keys():
        my_dim = variablelisting.loc[k, 'Dimension Params']
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

    return dfd


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
                # print(i)
                d.update({z[0]: int(z[1])})
            except:

                s = z[1].split('+')
                temp = []
                for j in s:
                    if not j.isdigit():
                        temp.append(str(d[j]))
                    else:
                        temp.append(j)
                # eval should be safe here, right?
                d.update({z[0]: eval('+'.join(temp))})

    return d

# write_NEMS_HDF ----------------------------------------------------------------

def write_var(varname, df, dat, variablelisting):

    my_var = varname
    temp = variablelisting.loc[my_var.upper(), 'Common Block Name']
    if isinstance(temp, str):
        common_block = temp
        my_dim = variablelisting.loc[my_var.upper(), 'Dimension Params']
        my_dim_list = variablelisting.loc[my_var.upper(), 'Extra Indeces']
    else:
        # print(f'multiple common blocks for: {varname}')
        # iloc[0] for P variables which are in two common blocks
        common_block = temp.iloc[0]
        my_dim = variablelisting.loc[my_var.upper(
        ), 'Dimension Params'].iloc[0]
        my_dim_list = variablelisting.loc[my_var.upper(
        ), 'Extra Indeces'].iloc[0]
    assert isinstance(common_block, str)
    
    #print(df)
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


            #for i, j in enumerate(my_dim_list[0]):
            for i in range(1, df.index.get_level_values(0).nunique()):
                dft = df.xs(i+1, level=0, drop_level=True)
                dft = dft.reset_index(drop=True)
                dft.columns = pd.RangeIndex(dft.columns.size)
                dat[f'{common_block}/{my_var.lower()}/{i}'] = dft
        # elif len(my_dim) == 4:
        #     if df.stack().index.names !=temp:
        #         df = df.stack().reset_index().pivot( index= temp[: -1], columns=temp[-1])
        #     for i, j in enumerate(my_dim_list[0]):
        #         for k, m in enumerate(my_dim_list[1]):
        #             dft = df.xs(i+1, level=0, drop_level=True)
        #             dft = dft.xs(k+1, level=0, drop_level=True)
        #             dft = dft.reset_index(drop=True)
        #             dft.columns = pd.RangeIndex(dft.columns.size)
        #             # print(j, m)
        #             dat[f'{common_block}/{my_var.lower()}/{i}/{k}'] = dft

    return dat
   
    
def write_restart(dat, my_vars, dfd):
    # dat = pd.HDFStore(restarthdf, 'r+')
    variablelisting = dat['_variablelisting']
    #drop duplicate indeces
    
    variablelisting = variablelisting[~variablelisting.index.duplicated(keep='first')]

    for varname in my_vars:
        try:
            df = dfd[varname]
            dat = write_var(varname, df, dat, variablelisting)
        # except:
        #     # Ignore dimensions, constant variables, and values
        #     print(f'varname==========: {varname}')  
        except Exception as e:
            # Stop execution if the variable is not available in restart
            raise Exception(f"Error writing variable '{varname}': {str(e)}")    
        
    return dat
    