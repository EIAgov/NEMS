import os
import time
import pandas as pd
import sys

from PyFiler import PyFilerWrapper as pfw

def get_convergence_variable_list():

    variable_list = []
    input_files_conv = ['conv_CoalToUtilities.csv', 
                        'conv_CoalVariables.csv', 
                        'conv_GasToUtilities.csv',
                        'conv_Other.csv', 
                        'conv_Prices.csv',
                        'conv_Quantities.csv',
                        'conv_SeasonalGasToUtilities.csv']

    for i in input_files_conv:
        temp_path = os.path.join('converge', 'input', i)
        df = pd.read_csv(temp_path)

        variable_list = variable_list + df['Variable'].tolist()
        variable_list = [x.lower() for x in variable_list]

    return variable_list

def export_hdf(pyfiler1,variable_list, current_iter, current_year):
    
    dat = {}
    # add 2 extra tables
    dictloc=r"..\input\dict.txt"
    dat['_varrange'],dat['_variablelisting']=extra_tables(dictloc)
    
    for i in ['curiyr','curitr','maxitr','irelax','ctest']:
        dat[f'NCNTRL/{i}']=pd.Series(getattr(pyfiler1.ncntrl,i))
    
    for i in ['cnvtst']:
        dat[f'NCNTRL/{i}']=pd.DataFrame(getattr(pyfiler1.ncntrl,i))
    
    
    for i in variable_list:
        #if its a singel series, the length will be four.  Otherwise, there are two series.
        measure=len(dat['_variablelisting'].loc[("{sub}".format(sub=i)).upper()])
        cb=dat['_variablelisting'].loc[("{sub}".format(sub=i)).upper()]['Common Block Name']
        #if there is one series, you are just pulling out the series, so you put it in the list
        #rather than measuring the number of series, you are getting the numebr of elements
        if measure==4:
            cb=[cb]
        for cname in cb:
            try:
                pyfiler_combine=getattr(pyfiler1,cname.lower())
                variable_dim_len = len(getattr(pyfiler_combine, i).shape)
                if variable_dim_len <= 2:
                    dat[cname+'/'+i] = pd.DataFrame(getattr(pyfiler_combine, i))
                elif variable_dim_len == 3:
                    tmp_dim = len(getattr(pyfiler1.utils, i)[:])
                    for j in range(0, tmp_dim):
                        dat[f'{cname}/{i}/{j}']=pd.DataFrame(getattr(pyfiler_combine, i)[j])
            
            except AttributeError:
                
                #print(f"AttributeError in PyfilerToHDF5: {i}")
                variable_dim_len = len(getattr(pyfiler1.utils, i).shape)
                if variable_dim_len <= 2:
                    dat[cname+'/'+i] = pd.DataFrame(getattr(pyfiler1.utils, i))
                elif variable_dim_len == 3:
                    tmp_dim = len(getattr(pyfiler1.utils, i)[:])
                    for j in range(0, tmp_dim):
                        dat[f'{cname}/{i}/{j}']=pd.DataFrame(getattr(pyfiler1.utils, i)[j])

            except:
                pass
                #print(i)

    return dat


def extra_tables(dictloc):

    NEMSFortTable, NEMSAttributeTable=pfw.ParseDict(dictloc)
    dfs = NEMSFortTable
    dfs = dfs[['Fortran Variable Name', 'Common Block Name', 'Dimensions', 'Dimensions Parameters']]
    dfs['Fortran Variable Name'] = dfs['Fortran Variable Name'].str.lower()
    refParamMapdf = NEMSAttributeTable[['DimSize']]
    # Build dataframe of variables and commonblock
    vardf = dfs[['Fortran Variable Name', 'Common Block Name']]
    vardf.index = vardf['Fortran Variable Name'].str.upper()
    # Get Dimension Parameters from NEMSFortTable using Variable name (MUST BE UPPER CASE)
    vardf['Dimension Params'] = dfs['Dimensions Parameters']
    # Split any Dimension Parameters that have more than one by commas
    vardf['Dimension Params'] = vardf['Dimension Params'].str.split(',')
    # Create new column for extra indeces, which will hold a list of lists for each extra index
    vardf['Extra Indeces'] = ''
    _varrange = vardf['Extra Indeces']
    _variablelisting = vardf
    return _varrange , _variablelisting

def main(pyfiler1, current_iter, current_year, variable_list):
    #variable_list = get_convergence_variable_list()
    file_toConverg=export_hdf(pyfiler1,variable_list, current_iter, current_year)
    return (file_toConverg)

if __name__ == "__main__":
    main()