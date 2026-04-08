"""

@authors: YDA
"""

import os
import time
import numpy as np
import pandas as pd
import copy

from restart_util import write_restart
from convergence_check import check_conv
from relax import perform_relax

import warnings
from tables import NaturalNameWarning
warnings.filterwarnings('ignore', category=NaturalNameWarning)


def reset_index_column(data_out):
    """reset all index in dataframe to start from 0

    Parameters
    ----------
    data_out : dict
        dict of variables dataframe with non-updated index (start from 1)

    Returns
    -------
    dict
        dict of variables dataframe with updated index (start from 0)
    """

    for i in data_out.keys():

        if i[0] == '_':
            continue

        else:
            #Reset Index
            data_out[i].reset_index(drop = True, inplace = True)

            #Reset Column Names
            try:
                data_out[i].rename(columns={x:y for x,y in zip(data_out[i].columns,range(0,len(data_out[i].columns)))}, inplace = True)
            except AttributeError:
                pass

    return data_out


def set_flag(df_model_conv, df):
    """Set the convergence flag to pass back
                
    Parameters
    ----------
    df_model : pandas.DataFrame
        The path to the input file.

    df : pandas.DataFrame
        containing non-converged variables.
    
    Returns
    -------
    pandas.DataFrame
        containing DataFrame with flags.

    """

    df_model = copy.deepcopy(df_model_conv)

    df_check = pd.DataFrame(df.groupby(['Module'])[["NonconvVar"]].sum()).reset_index()
    
    df_model['CNVTST'] = np.where(df_model['Model'].isin(df_check['Module']), 0, 1)

    return df_model
             
def write_debug(df, df_reg, RegRepFlag, path):
    """Write debug files

    Parameters
    ----------
    df : pandas.DataFrame
        containing non-converged variables.

    df_reg : pandas.DataFrame
        regional level non-converged variables.

    RegRepFlag : int
        flag to write out regional info

    path : str
        absolute path to converge\\output
    """
    name = 'conv_summary.csv'
    out_path = os.path.join(path, name)
    # if file does not exist write header 
    if not os.path.isfile(out_path):
        df.to_csv(out_path, header='column_names')
    else: # else it exists so append without mentioning the header
        df.to_csv(out_path, mode='a', header=False)

    #Write regional info
    name = 'conv_summary_reg.csv'
    out_path = os.path.join(path, name)
    if RegRepFlag == 1:
        df_reg=df_reg.set_index(['Variable','Region', 'Sector'])
        df_reg = df_reg.sort_values(by=['Percent Change'], ascending=False)
        if not os.path.isfile(out_path):
            df_reg.to_csv(out_path, header='column_names')
        else: # else it exists so append without mentioning the header
            df_reg.to_csv(out_path, mode='a', header=False)
    
def set_trigger(datminus1_processed, my_vars, df_conv, yr, iter):
    """PERTURB VALUES ON EVEN ITERATIONS TO TRIGGER NONCONVERGENCE

    Parameters
    ----------
    datminus1_processed : dict
        dict containing dataframe of variables used for convergence, not yet perturbed
    my_vars : list
        list of variables used for convergence
    df_conv : DataFrame
        input from .csv files from model/converge/input read in a dataframe
    yr : int
        integer year in Python (e.g. if Fortran CURIYR == 32, it will be 31 here)
    iter : int
        current iteration number (CURITR)

    Returns
    -------
    dict
        dict containing dataframe of variables used for convergence, perturbed
    """

    i=int(iter/2)*2
    
    if iter == i:
        Trigger = 1.11
    else:
        Trigger = 1/1.11
    
    for var in my_vars:
        #Find the type of variable for convergence test
        PQType = df_conv.loc[var, 'PQType']
        datminus1_processed[var] = datminus1_processed[var].astype(float)

        ## set a specific slice
        # From conv_Other.csv
        if PQType == 'O' or PQType == 'PR' or PQType == 'QR': 
                    
            datminus1_processed[var].iloc[:, yr] *= Trigger
            
        # From conv_CoalVariables.csv
        elif PQType == 'Z':
            RegID = 10
            
            datminus1_processed[var].iloc[RegID, yr] *= Trigger
            
        # From other .csv input files not listed above (in /model/converge/input)
        else:
            RegID = 6
            
            datminus1_processed[var].iloc[RegID, yr] *= Trigger
            
    return datminus1_processed

def main(datminus1, datminus1_processed, datcur, datcur_processed, curirun, current_nems_module, conv_dat, conv_imodel):
    """
    Runs convergence and relaxation code in the following order
    
    1. set convergence runtime parameters, including for debugging

    2. Set variables for output paths, and create output path if doesnt exist

    3. Read convergence and relaxation settings and deep copy variable settings

    4. Create a list of all restart file variables used in convergence and relaxation

    5. Transform data from dict to dict of dataframes and select only "adjusted" or "unadjusted" variables (this step was moved to PyFilerTo_conv)

    6. Get scalar values of IRELAX, CURIYR, CURITR, MAXITR

    7. Set the flag to write the regional report

    8. Trigger nonconvergence for debugging (only activate when debug flag is ON)

    9. Check convergencies

    10. Perform Relaxation

    11. Set a flag for non-converged models

    12. Update dict of dataframe with relaxed values

    13. Setup and Write debug file, convergence summary and by regional

    14. Reset the index for all dataframe in the dictionary
    
    
    Parameters
    ----------
    datminus1 : dict
        dict containing pandas dataframes of convergence variables from previous iteration
    datminus1_processed : dict
        dict containing pandas dataframes of convergence variables from previous iteration, processed and reshaped for 'unadjusted' variables
    datcur : dict
        dict containing pandas dataframes of convergence variables from previous iteration
    datcur_processed : dict
        dict containing pandas dataframes of convergence variables from previous iteration, processed and reshaped for 'unadjusted' variables
    curirun : int
        current cycle number
    current_nems_module : str
        current NEMS model (e.g. IDM, RDM, CDM)
    conv_dat : dict
        dict containing data from the following keys : values
            1. df_conv : pandas.DataFrame
                The DataFrame with convergence setting data.
            2. df_rls : pandas.DataFrame
                The DataFrame with relax factor (0.33 - 1.0) for each variable.
            3. all_vars : list
                A list of all variables listed in the input .csv files.
            4. vars_conv : list 
                A list of variables to test for convergence.
            5. vars_rlx : list
                A list of variables to apply relaxation factor between iterations.
    conv_imodel : DataFrame
        Dataframe of IMODEL number corresponding with model acronym
    
    Returns
    -------
    DataFrame
        Contains flag for convergence test 0/1 (0 = Fail; 1 = Pass). Currently only the 'ALL' model is used
    dict
        Contains updated variable values after applying RELAX algorithm    

    """

    #####################################
    ## 1. set convergence runtime parameters, including for debugging
    # RUNTIME PARAMETER MNDBGCV TO SEE IF DEBUG CODE SHOULD EXECUTE (Harcoded for now)
    MNDBGCV = 0
    #Flag: write out regional convergence info =1; no=0
    RegRptFlag = 1
    #Flag: write out regional convergence info every iteration =1; no=0
    ItrRptFlag = 0
    #for time tests
    Timetest = 0
    #####################################
    
    #####################################
    ## 2. Set variables for output paths, and create output path if doesnt exist
    output_path = os.path.join(os.getcwd(), 'converge', 'output')
    os.makedirs(output_path, exist_ok=True)
    #####################################

    #####################################
    ## 3. Read convergence and relaxation settings and deep copy variable settings
    df_conv     = copy.deepcopy(conv_dat['df_conv'])
    df_rlx      = copy.deepcopy(conv_dat['df_rlx'])
    all_vars_in = copy.deepcopy(conv_dat['all_vars'])
    vars_conv   = conv_dat['vars_conv']
    vars_rlx    = conv_dat['vars_rlx']
    #####################################
    
    #####################################
    ## 4. Create a list of all restart file variables used in convergence and relaxation
    my_params = ['CURIYR','CURITR','MAXITR','IRELAX']
    cnv_vars = ['CNVTST', 'CTEST']
    all_vars = my_params + cnv_vars + all_vars_in
    #####################################
    
    #####################################
    ## 5. Transform data from dict to dict of dataframes and select only "adjusted" or "unadjusted" variables (moved to PyFilerto_conv)
    #####################################
    
    #####################################
    ## 6. Get scalar values of IRELAX, CURIYR, CURITR, MAXITR
    I_Relax = datcur_processed['IRELAX'].iloc[0,0]
    Cur_yr = datcur_processed['CURIYR'].iloc[0,0]
    Iteration = datcur_processed['CURITR'].iloc[0,0]
    MaxIteration = datcur_processed['MAXITR'].iloc[0,0]
    #####################################
    
    #####################################
    ## 7. Set the flag to write the regional report
    if (RegRptFlag == 1 and Iteration>=MaxIteration) or ItrRptFlag ==1:
        RptFlag = 1
    else:
        RptFlag = 0
    #####################################
    
    #####################################
    ## 8. Trigger nonconvergence for debugging (only activate when debug flag is ON)
    if MNDBGCV == 1:
        datminus1_processed = set_trigger(datminus1_processed, vars_conv, df_conv, Cur_yr-1, Iteration)
    #####################################
    
    #####################################
    ## 9. Check convergencies
    starttime = time.process_time()
    
    df_conv_res, df_regcv = check_conv(datminus1_processed, datcur_processed, vars_conv, df_conv, Cur_yr-1, RptFlag)
    
    # time test
    elapsed = time.process_time() - starttime
    if Timetest==1:
        print(f'convergence test: {elapsed} seconds.\n')
    #####################################
    
    #####################################
    ## 10. Perform Relaxation
    starttime = time.process_time()
    if I_Relax > 0:
        dfd_updated = perform_relax(datminus1_processed, datcur_processed, vars_rlx, df_rlx, Cur_yr-1)
    elapsed = time.process_time() - starttime
    
    if Timetest==1:
        print(f'relaxation: {elapsed} seconds.\n')
    #####################################
    
    #####################################
    ## 11. Set a flag for non-converged models
    ConvTest = set_flag(conv_imodel, df_conv_res)
    aligned_source = ConvTest['CNVTST'].reindex(datcur_processed['CNVTST'].iloc[:,Cur_yr-1].index)
    datcur_processed['CNVTST'].iloc[:,Cur_yr-1] = aligned_source
    if (ConvTest['CNVTST']==0).any():
        datcur_processed['CTEST'].iloc[0,0] = 0
    else:
        datcur_processed['CTEST'].iloc[0,0] = 1
    
    #####################################
    # To avoid PyTables Performance warning when writing to HDF, change the column type to string
    #columns = datcur_processed['CTEST'].columns
    #datcur_processed['CTEST'].loc[:,columns] = datcur_processed['CTEST'][columns].applymap(str)

    #####################################
    ## 12. Update dict of dataframe with relaxed values
    starttime = time.process_time()

    # Reset ['CNVTST', 'CTEST'] results if they were overwritten by relaxation algorithm
    for var in cnv_vars:
        if I_Relax > 0:
            dfd_updated[var]=datcur_processed[var]
    
    # Add ['CNVTST', 'CTEST'] to list
    vars_upd = vars_rlx + cnv_vars
    
    # Update the dict of dataframe
    datupd = ""
    if I_Relax > 0:
        datupd = write_restart(datcur, vars_upd, dfd_updated)

    elapsed = time.process_time() - starttime
    if Timetest==1:
        print(f'write to restart files: {elapsed} seconds.\n')

    #####################################

    #####################################
    ## 13. Setup and Write debug file, convergence summary and by regional
    df_conv_res.insert(0, 'Year', Cur_yr + 1989)
    df_conv_res.insert(1, 'Iteration', Iteration)
    df_conv_res.insert(2, 'Cycle', curirun)
    df_conv_res.insert(3, 'Model', current_nems_module)
    if RptFlag == 1:
        df_regcv.insert(2, 'Iteration', Iteration)
        df_regcv.insert(3, 'Cycle', curirun)
        df_regcv.insert(4, 'Model', current_nems_module)
    starttime = time.process_time()
    write_debug(df_conv_res, df_regcv, RptFlag, output_path)
    elapsed = time.process_time() - starttime
    if Timetest==1:
        print(f'write reports: {elapsed} seconds.\n')

    #####################################

    #####################################
    ## 14. Reset the index for all dataframe in the dictionary
    if I_Relax > 0:
        datupd = reset_index_column(datupd)
    #####################################
    
    return ConvTest, datupd

if __name__ == "__main__":
    print('Start testing......')
    start = time.process_time()

    df_conv_res_pass = main()

    elapsed = time.process_time() - start
    print(f'Elapsed time: {elapsed} seconds.\n')
    print('Completed!')
