"""

@authors: YDA
"""

import os
import shutil
import time
import random
import numpy as np
import pandas as pd
import copy

from restart_util import (read_stuff, write_restart)
from conv_data_preprocess import read_conv_file
from convergence_check import check_conv
from relax import perform_relax

import warnings
from tables import NaturalNameWarning
warnings.filterwarnings('ignore', category=NaturalNameWarning)


def reset_index_column(data_out):

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

def load_values(restarthdf_path, my_vars, adj_flag):
    """
    Load data from a restart HDF file and return a dict of df.

    Parameters:
        restarthdf_path (str): The path to the restart HDF file.
        my_vars (list): A list of variables to load from the restart file.
        adj_flag: 'Adjusted' or  'Unadjusted' for carbon-adjusted and unadjusted prices (AMPBLK/MPBLK, 
        ANGTDM/NGTDMOUT, ACOALPRC/COALPRC)
                
    Returns:
        dict: containing df of old data.

    Example:
        restarthdf_path = 'restart_data.h5'
        my_vars = ['var1', 'var2', 'var3']
        dfd = load_values(restarthdf_path, my_Qvars)
    """
    
    # Get data from restart
    #dat = pd.HDFStore(restarthdf_path, 'r')

    # Load data
    dfd = read_stuff(restarthdf_path, my_vars, adj_flag)
    #dfd = read_stuff(dat, my_vars, adj_flag)

    #TODO: need only data for CURIYR, but for that all dict keys need to be converted to Dataframe
    #pd.DataFrame.from_dict(dfd). Might be easier to get HDF restart with only curiyr values.
    # Need to check
    
    #dat.close()

    return dfd

def write_values(restarthdf_path, my_vars, dfd):
    """
    Write data to a restart HDF file.

    Parameters:
        restarthdf_path (str): The path to the restart HDF file.
        my_vars (list): A list of variables to load from the restart file.
        dfd (dict): Data to write

    Example:
        restarthdf_path = 'restart_data.h5'
        my_vars = ['var1', 'var2', 'var3']
        dfd = write_values(restarthdf_path, my_vars)
    """
    
    # Get data from restart
    dat = pd.HDFStore(restarthdf_path, 'r+')

    # Write data
    write_restart(dat, my_vars, dfd)
             
    dat.close()

def get_current_values(dfd_old, my_vars):
    """
    Get current data FOR TESTING PURPOSES ONLY

    Parameters:
        dfd_old (dict): containing DataFrames of the old data.

    Returns:
        dict: containing DataFrames of the current data.

    Note:
        We randomly select a variable and alter its values to get its "new" values.
        This is for testing purposes only.     
    """
 
    # Here we assume current values = old values + changes
    dfd_cur = dfd_old.copy()

    # Let's randomly select a variable and alter its values for testing
    #df_var_random = random.choice(list(dfd_cur.keys()))
    df_var_random = random.choice(my_vars)
    print(f'randomly select variable to change: {df_var_random}')
    #dfd_cur[df_var_random] = dfd_cur[df_var_random].map(lambda value: value + np.random.randn())
    dfd_cur[df_var_random] = dfd_cur[df_var_random].map(lambda value: value *1.11)

    return dfd_cur, df_var_random

def set_flag(path, df):
    """
    Set the convergence flag to pass back

    Parameters:
        path (str): The path to the input file.
        df(pandas.DataFrame): containing non-converged variables.

    Returns:
        df_model(pandas.DataFrame): containing DataFrame with flags.     
    """
    df_model = pd.read_csv(path + 'IMODEL.csv', index_col='NMODEL')
    
    df_check = pd.DataFrame(df.groupby(['Module'])[["NonconvVar"]].sum()).reset_index()
    
    df_model['CNVTST'] = np.where(df_model['Model'].isin(df_check['Module']), 0, 1)
    #print(df_model)

    return df_model

def write_debug(df, df_reg, RegRepFlag, path):
    """
    Write debug files

    Parameters:
        df(pandas.DataFrame): containing non-converged variables.
        df_reg(pandas.DataFrame): regional level non-converged variables.
        RegRepFlag: flag to write out regional info
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
    
def set_trigger(dfd_prev, my_vars, df_conv, yr, iter):
    '''
    PERTURB VALUES ON EVEN ITERATIONS TO TRIGGER NONCONVERGENCE
    '''
    i=int(iter/2)*2
    
    if iter == i:
        Trigger = 1.11
    else:
        Trigger = 1/1.11
    
    for var in my_vars:
        #Find the type of variable for convergence test
        PQType = df_conv.loc[var, 'PQType']
        dfd_prev[var] = dfd_prev[var].astype(float)

        # set a specific slice 
        if PQType == 'O' or PQType == 'PR' or PQType == 'QR': 
                    
            dfd_prev[var].iloc[:, yr] *= Trigger
            
        elif PQType == 'Z':
            RegID = 10
            
            dfd_prev[var].iloc[RegID, yr] *= Trigger
            
        else:
            RegID = 6
            
            dfd_prev[var].iloc[RegID, yr] *= Trigger
            
    return dfd_prev

def main(datminus1, datcur, curirun, fcrl, current_nems_module, conv_dat):
#def main():
    
    #set hardcoded parameters;   
    # RUNTIME PARAMETER MNDBGCV TO SEE IF DEBUG CODE SHOULD EXECUTE (Harcoded for now)
    MNDBGCV = 0
    #Flag: write out regional convergence info =1; no=0
    RegRptFlag = 1
    #Flag: write out regional convergence info every iteration =1; no=0
    ItrRptFlag = 0

    #for time tests
    Timetest = 0

    input_path = os.path.join(os.getcwd(), 'converge' ,'input')
    output_path = os.path.join(os.getcwd(), 'converge', 'output')
    os.makedirs(output_path, exist_ok=True)
    #restarthdf_prev_path = os.path.join(input_path, 'RestartRepWrt_Cycle3_Year2050.hdf5')
    #restarthdf_path = os.path.join(input_path, 'RestartRepWrt_Cycle5_Year2050.hdf5')
    convergence_path = os.path.join(input_path, 'conv_')

    # Read convergence and relaxation settings
    #df_conv, df_rlx, all_vars, vars_conv, vars_rlx = read_conv_file(convergence_path)
    df_conv     = copy.deepcopy(conv_dat['df_conv'])
    df_rlx      = copy.deepcopy(conv_dat['df_rlx'])
    all_vars_in = copy.deepcopy(conv_dat['all_vars'])
    vars_conv   = conv_dat['vars_conv']
    vars_rlx    = conv_dat['vars_rlx']
    

    # Get CURIYR and other parameters from the restart as well
    my_params = ['CURIYR','CURITR','MAXITR','IRELAX']
    cnv_vars = ['CNVTST', 'CTEST']
    all_vars = my_params + cnv_vars + all_vars_in

    starttime = time.process_time() #ydatemp
    # Load variables from hdf
    # Use unadjusted prices
    dfd_prev = load_values(datminus1, all_vars, adj_flag = 'Unadjusted')
    dfd_cur = load_values(datcur, all_vars, adj_flag = 'Unadjusted')

    elapsed = time.process_time() - starttime
    if Timetest==1:
        print(f'Load restart files: {elapsed} seconds.\n') #ydatemp

    # Update variables    
    #dfd_cur, var_rand = get_current_values(dfd_prev, vars_conv)

    # Get scalar values of IRELAX, CURIYR
    I_Relax = dfd_cur['IRELAX'].iloc[0,0]
    Cur_yr = dfd_cur['CURIYR'].iloc[0,0]
    Iteration = dfd_cur['CURITR'].iloc[0,0]
    MaxIteration = dfd_cur['MAXITR'].iloc[0,0]
    
    # Set the flag to write the regional report
    if (RegRptFlag == 1 and Iteration>=MaxIteration) or ItrRptFlag ==1:
        RptFlag = 1
    else:
        RptFlag = 0

    #FOR TESTING PURPOSES set curiyr and curitr
    #Cur_yr-=3
    #Iteration=2
    
    # Trigger nonconvergence for debugging
    if MNDBGCV == 1:
        dfd_prev = set_trigger(dfd_prev, vars_conv, df_conv, Cur_yr-1, Iteration)
    starttime = time.process_time() #ydatemp
    # Check convergencies
    df_conv_res, df_regcv = check_conv(dfd_prev, dfd_cur, vars_conv, df_conv, Cur_yr-1, RptFlag)
    elapsed = time.process_time() - starttime
    if Timetest==1:
        print(f'convergence test: {elapsed} seconds.\n') #ydatemp
    starttime = time.process_time() #ydatemp
    # Perform Relaxation
    if I_Relax > 0:
        dfd_updated = perform_relax(dfd_prev, dfd_cur, vars_rlx, df_rlx, Cur_yr-1)
    elapsed = time.process_time() - starttime
    if Timetest==1:
        print(f'relaxation: {elapsed} seconds.\n') #ydatemp

    
    # Set a flag for non-converged models
    ConvTest = set_flag(convergence_path, df_conv_res)
    dfd_cur['CNVTST'].iloc[:,Cur_yr-1]=ConvTest['CNVTST']
    if (ConvTest['CNVTST']==0).any():
        dfd_cur['CTEST'].iloc[0,0] = 0
    else:
        dfd_cur['CTEST'].iloc[0,0] = 1
    # To avoid PyTables Performance warning when writing to HDF, change the column type to string
    columns = dfd_cur['CTEST'].columns
    dfd_cur['CTEST'].loc[:,columns] = dfd_cur['CTEST'][columns].applymap(str)
    
    dfd_cv = {}
    for var in cnv_vars:
        dfd_cv[var] = dfd_cur[var]
        dfd_updated[var]=dfd_cur[var]
    
    vars_upd = vars_rlx + cnv_vars
    
    starttime = time.process_time() #ydatemp
    # Write CNVTST and CTEST to regular restart
    # Create RLX restart with updated relaxed values
    # name = 'RestartOUT' +'.hdf5'    
    # outname = os.path.join(output_path, name)
    # name_rlx = 'RestartRLX' +'.hdf5'
    # pathrlx = os.path.join(output_path, name_rlx)
    # Copy restart to output
    ##BTO
    datupd = write_restart(datcur, vars_upd, dfd_updated)

    # return datupd


    # if not os.path.isfile(outname):
    #     shutil.copy(restarthdf_path, outname)
    # write_values(outname, cnv_vars, dfd_cv)
    # # create RLX copy of OUT restart file to output to modify
    # if not os.path.isfile(pathrlx):
    #     shutil.copy(outname, pathrlx)
    # write_values(pathrlx, vars_upd, dfd_updated)

    elapsed = time.process_time() - starttime
    if Timetest==1:
        print(f'write to restart files: {elapsed} seconds.\n') #ydatemp     
    # Write debug file
    df_conv_res.insert(0, 'Year', Cur_yr + 1989)
    df_conv_res.insert(1, 'Iteration', Iteration)
    df_conv_res.insert(2, 'Cycle', curirun)
    df_conv_res.insert(3, 'Model', current_nems_module)
    if RptFlag == 1:
        df_regcv.insert(2, 'Iteration', Iteration)
        df_regcv.insert(3, 'Cycle', curirun)
        df_regcv.insert(4, 'Model', current_nems_module)
    starttime = time.process_time() #ydatemp
    write_debug(df_conv_res, df_regcv, RptFlag, output_path)
    elapsed = time.process_time() - starttime
    if Timetest==1:
        print(f'write reports: {elapsed} seconds.\n') #ydatemp             


    # with open(f'{output_path}/maindbg.txt', 'a') as mdbgt:
        # status = f'\n\n\n\ncycle: {curirun}, year:{Cur_yr}, iter:{Iteration}, Model:{current_nems_module}, FCRL:{fcrl}\n\n'
        # mdbgt.write(status)
        # ConvTest.to_string(mdbgt)
    
    datupd = reset_index_column(datupd)

    return ConvTest, datupd

if __name__ == "__main__":
    print('Start testing......')
    start = time.process_time()

    df_conv_res_pass = main()

    elapsed = time.process_time() - start
    print(f'Elapsed time: {elapsed} seconds.\n')
    print('Completed!')
