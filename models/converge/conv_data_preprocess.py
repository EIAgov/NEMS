#Read convergence input file

import pandas as pd

def read_conv_file(input_file):
    """
    Read convergence input file and return a DataFrame with convergence settings and a list 
    of variables for convergence test.

    Args:
        input_file (str): The path to the input file.

    Returns:
        df_conv (pandas.DataFrame): The DataFrame with convergence setting data.
        df_rlx (pandas.DataFrame): The DataFrame with relax factor (0.33 - 1.0) for each variable.
        vars_available (list): A list of all variables listed in the input .csv files.
        vars_conv (list): A list of variables to test for convergence.
        vars_rlx (list): A list of variables to apply relaxation factor between iterations.
    """
    
    dfQ = pd.read_csv(input_file + 'Quantities.csv', index_col='Variable')
    dfP = pd.read_csv(input_file + 'Prices.csv', index_col='Variable')
    dfGU = pd.read_csv(input_file + 'GasToUtilities.csv', index_col='Variable')
    dfSGU = pd.read_csv(input_file + 'SeasonalGasToUtilities.csv', index_col='Variable')
    dfCU = pd.read_csv(input_file + 'CoalToUtilities.csv', index_col='Variable')
    dfOTH = pd.read_csv(input_file + 'Other.csv', index_col='Variable')
    dfCProd = pd.read_csv(input_file + 'CoalVariables.csv', index_col='Variable')

    #merge all price variables

    df_conv = pd.concat([dfP, dfQ, dfGU, dfSGU, dfCU, dfOTH, dfCProd]).iloc[:, 0:8]

    # Trim spaces (may need to "clean" the Excel file)
    #df_conv.index = df_conv.index.str.strip()

    # Create a list of available variables
    vars_available = df_conv.index.to_list()

    # Create dataframe for relaxation
    df_rlx = df_conv.copy() 
    
    # Remove vars for which convergence should not be checked
    df_conv = df_conv[df_conv['Test']==0]

    # Remove vars for which relaxation should not be performed
    df_rlx = df_rlx[df_rlx['Relax']==1]    
        
    # Add column to flag convergence
    df_conv.insert(loc=8, column='NonconvVar', value=0)

    #Remove extra columns
    df_conv = df_conv.drop(columns=['Test','Relax','RelaxFactor'])
    df_rlx = df_rlx.drop(columns=['Test','Relax',  \
                                  'QMATCH','AbsoluteTolerance','FractionTolerance'])
    
    # Create a list of available variables
    vars_conv = df_conv.index.to_list()
    vars_rlx = df_rlx.index.to_list()
            
    return df_conv, df_rlx, vars_available, vars_conv, vars_rlx