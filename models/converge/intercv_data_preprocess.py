#Read convergence input file

import pandas as pd

def read_icnv_file(input_file):
    """
    Read convergence input file and return a DataFrame with convergence settings and a list 
    of variables for convergence test.

    Parameters:
        input_file (str): The path to the input file.

    Returns:
        df_conv (pandas.DataFrame): The DataFrame with convergence setting data.
        vars_available (list): A list of variables to test for convergence.
    """
    
    dfQ = pd.read_csv(input_file + 'Quantities.csv', index_col='Variable')
    dfP = pd.read_csv(input_file + 'Prices.csv', index_col='Variable')
    dfGU = pd.read_csv(input_file + 'GasToUtilities.csv', index_col='Variable')
    dfSGU = pd.read_csv(input_file + 'SeasonalGasToUtilities.csv', index_col='Variable')
    dfCU = pd.read_csv(input_file + 'CoalToUtilities.csv', index_col='Variable')
    dfOTH = pd.read_csv(input_file + 'Other.csv', index_col='Variable')
    dfCProd = pd.read_csv(input_file + 'CoalVariables.csv', index_col='Variable')

    #merge all variables

    df_conv = pd.concat([dfP, dfQ, dfGU, dfSGU, dfCU, dfOTH, dfCProd]).iloc[:, 0:8]

    # Create a list of available variables
    vars_available = df_conv.index.to_list()

    # Create a dataframe for relaxation
    df_rlx = df_conv.copy() 
    
    # Remove vars for which convergence should not be tested
    df_conv = df_conv[df_conv['Test']==0]

    # Remove vars for which relaxation should not be performed
    df_rlx = df_rlx[df_rlx['Relax']==1]    
        
    # Add column to flag convergence
    df_conv.insert(loc=8, column='NonconvVar', value=0)

    #Remove extra columns
    df_conv = df_conv.drop(columns=['Test','Relax','RelaxFactor'])
    df_rlx = df_rlx.drop(columns=['Test','Relax', 'QMATCH', \
                                  'AbsoluteTolerance','FractionTolerance'])
    
    # Create a list of available variables
    vars_conv = df_conv.index.to_list()
    vars_rlx = df_rlx.index.to_list()

    #create a dataframe to hold Convergence Summary Table
    df_cvtab = pd.concat([dfP, dfQ, dfGU, dfSGU, dfCU, dfOTH, dfCProd]).iloc[:, 5:14]
    #remove empty rows and sort
    df_cvtab = df_cvtab[df_cvtab['CVTAB ID'].notnull()].sort_values(by=['CVTAB ID'])
    vars_cvtab = df_cvtab.index.to_list()
               
    return df_conv, df_rlx, df_cvtab, vars_available, vars_conv, vars_rlx, vars_cvtab