"""

@authors: YDA
"""

import os
import shutil
import time
import random
import numpy as np
import pandas as pd

from restart_util import (read_stuff, write_restart)
from intercv_data_preprocess import read_icnv_file
from convergence_check import check_conv
from relax import perform_relax
from intercv_sum_conv import sum_conv

import warnings
from tables import NaturalNameWarning
warnings.filterwarnings('ignore', category=NaturalNameWarning)

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
    dat = pd.HDFStore(restarthdf_path, 'r')
    # Load data
    dfd = read_stuff(dat, my_vars, adj_flag)
             
    dat.close()

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
    #df_var_random = random.choice(my_vars)
    df_var_random = 'QELTR'
    print(f'randomly select variable to change: {df_var_random}')
    #dfd_cur[df_var_random] = dfd_cur[df_var_random].map(lambda value: value + np.random.randn())
    #dfd_cur[df_var_random] = dfd_cur[df_var_random].map(lambda value: value *1.11)
    dfd_cur[df_var_random] = dfd_cur[df_var_random].map(lambda value: value *100)

    return dfd_cur, df_var_random

def calc_average_score(GPA):
    """
    Sort the scores and take average of 3 worst (lowest) scores

    Parameters:
        GPA(pandas.Series): containing GPA scores.        
    """
    #sort only the scores in the projected period that starts 2 years after last SEDS year and
    Score_sorted = GPA.sort_values()
    #take average of 3 worst (lowest) scores
    Average = Score_sorted[:3].mean()

    return Average

def write_GPA(ave, ave_US, cycle, StopCode, MinScore, path):
    """
    Write debug files

    Parameters:
        df(pandas.DataFrame): containing non-converged variables.
        df_reg(pandas.DataFrame): regional level non-converged variables.
        RegRepFlag: flag to write out regional info
    """
        
    df= pd.DataFrame.from_records([{'Cycle': str(cycle), 'Regional GPA': ave, 'US GPA': ave_US, \
                                     'PASS/FAIL': ''}], index='Cycle')
    
    print(f'Cycle {str(cycle)} GPA (US: {ave_US = :.2f}; REG {ave = :.2f}) on a 4 point scale \
(averaged over 3 worst years) with {MinScore } considered minimally acceptable.')
    if StopCode == 1:
        df['PASS/FAIL'] = 'PASS'
        print(f'Congratulations for passing our rigorous testing standards.   GPA: **PASS**')
    else:
        df['PASS/FAIL'] = 'FAIL'
        print(f'Warning:  additional run cycles may be needed.   GPA: **FAIL**')     

    name = 'Iconv_AverageGPA.csv'
    out_path = os.path.join(path, name)
    # if file does not exist write header 
    if not os.path.isfile(out_path):
        df.to_csv(out_path, header='column_names')
    else: # else it exists so append without mentioning the header
        df.to_csv(out_path, mode='a', header=False)

def write_debug(df_sum, df_weights, df_GPA, cycle, fl, path):
    """
    Write debug files

    Parameters:
        df_sum(pandas.DataFrame): containing summary of non-converged variables.
        df_weights(pandas.DataFrame): weighted scores for debug.
        df_GPA(pandas.DataFrame): Weighted scores by class and final GPA for all years
        cycle: current cycle number
        fl: flag REG or US
    """
    df_sum = df_sum.set_index(['Year','CVTAB ID'])
    df_sum = df_sum.drop(columns = ['Weight_Start', 'Weight_Sum'])

    df_GPA.columns = df_GPA.columns.astype(str)
    df_GPA.columns = [f"Weight_Class_{col}" if col.isdigit() else col for col in df_GPA]
    
    d = {'IConvSummary_':df_sum,'IConv_Weighted_Deviations_':df_weights, \
         'IConv_GPAscores_debug':df_GPA}
    for k, my_df in d.items():
        my_df['Level'] = fl
        name = k + str(cycle) +'.csv'
        out_path = os.path.join(path, name)
        # if file does not exist write header 
        if not os.path.isfile(out_path):
            my_df.to_csv(out_path, header='column_names')
        else: # else it exists, append without the header
            my_df.to_csv(out_path, mode='a', header=False)  


def update_CVTAB(score, score_US, df, path, path_out, cycle):
    """
    Update CONVERGE common block variables for restart file

    Parameters:
        score, score_US: GPA scores to fill CVSCORE and CVSCORE_US
        path: restart file location
        cycle: current cycle number
        df(pandas.DataFrame): containing info for CVTAB variable.        
    """
    cv_vars = ['CVTAB', 'CVSCORE', 'CVSCORE_US', 'CVSCOREHIST']
    # Load data
    dfd = load_values(path, cv_vars, adj_flag='Adjusted')
    
    if cycle == 1:
        dfd['CVSCOREHIST'].iloc[:,:]=0
        dfd['CVSCORE'].iloc[:,:]=0
    else:
        # removing the last row from 10-cycle history of GPA scores to make room for latest
        dfd['CVSCOREHIST'] = dfd['CVSCOREHIST'].iloc[:-1,:] 
        # store score from prior cycle in history
        dfd['CVSCOREHIST'] = pd.concat([dfd['CVSCORE'],dfd['CVSCOREHIST']]).set_index(pd.RangeIndex(1, 11))
    
    score = np.float32(score)
    score_US = np.float32(score_US)
    dfd['CVSCORE'].iloc[:,:] = score
    dfd['CVSCORE_US'].iloc[:,:] = score_US
    
    #reshape df_sum to match "CVTAB" format
    df['Year'] -= 1989
    df = df.drop(columns = ['Variables_in_Group', 'Weight_Sum','Weight_Class_Name',\
                            'Weight_Class', 'Weight_Start','Deviation'])
    df.rename(columns={'Year':'MNUMYR', 'CVTAB ID': 'M106', 'Current':1, \
                            'Previous':2, 'Abs Change':3, 'Signed_Deviation':4 }, inplace=True)
    df[['MNUMYR','M106']] = df[['MNUMYR','M106']].astype('int64')
    df = df.set_index(['MNUMYR', 'M106']).stack(future_stack=True)
    df.index.set_names('M4', level=2, inplace=True)
    df=df.unstack(level=0)
    df = df.astype('float32')
    dfd['CVTAB'].update(df)
    
    write_values(path_out, cv_vars, dfd) 

    return 

def update_reason(path, PFgpa):
    '''
    Updates the reason for stopping or continuing
    Parameters:
        path(str): path for restart file 
        PFgpa: Pass/Fail indicator.
    '''
    ReasonYes_var = ['REASONYES']
    reason_vars = ['REASONW', 'REASONM', 'REASONR', 'REASONK', 'REASONI', 'REASONT', 
                   'REASONE', 'REASONC', 'REASONL', 'REASONG', 'REASONO', 'REASONN', 'REASONH']
    continew_vars = ['CONTINW', 'CONTINM', 'CONTINR', 'CONTINK', 'CONTINI', 'CONTINT', 
                     'CONTINE', 'CONTINC', 'CONTINL', 'CONTING', 'CONTINO', 'CONTINN', 'CONTINH']
    continew = load_values(path, continew_vars, adj_flag='Adjusted')
    reason = load_values(path, reason_vars, adj_flag='Adjusted')
    ReasonYes = load_values(path, ReasonYes_var, adj_flag='Adjusted')
    RYes = ReasonYes['REASONYES'].iloc[0,0]
    for var in continew_vars:
        if continew[var].any().any() == 0:
            RYes = 0
            Reason_char = reason[var]
    
    if PFgpa == 1 and RYes == 1:
        StopCode = 1
    else:
        StopCode = 0
    
    return StopCode
    

def main():

    #StopCode = code to be picked up to stop the cycling; 1 indicates a Passing GPA

    #Command line argument min_score_c hardcoded for now
    Min_score = 3.9

    #SET HARDCODED PARAMETERS
    #Flag: write out regional convergence info =1; no=0
    RegRepFlag = 1
    #Set FirstYear = MSEDYR+2; PARAMETER(MSEDYR=32)   ! Number of Historical SEDS years
    FirstYear = 34
        
    input_path = os.path.join(os.getcwd(), 'input')
    output_path = os.path.join(os.getcwd(), 'output')
    os.makedirs(output_path, exist_ok=True)
    restarthdf_prev_path = os.path.join(input_path, 'RestartRepWrt_Cycle3_Year2050.hdf5')
    restarthdf_path = os.path.join(input_path, 'RestartRepWrt_Cycle5_Year2050.hdf5')
    convergence_path = os.path.join(input_path, 'icnv_')

    
    # Read convergence and relaxation settings
    df_conv, df_rlx, df_cvtab, all_vars, vars_conv, vars_rlx, vars_cvtab = read_icnv_file(convergence_path)

    # Get CURIYR and other parameters from the restart as well
    my_params = ['IRELAX','CURIRUN','CURIYR']
        
    all_vars = my_params + all_vars
        
    # Load variables from hdf
    # For convergence testing use adjusted prices
    dfd_prev = load_values(restarthdf_prev_path, all_vars, adj_flag='Adjusted')
    dfd_cur = load_values(restarthdf_path, all_vars, adj_flag='Adjusted')
    #dfd_prev, df_var_random = get_current_values(dfd_cur, all_vars)
    # For relaxation use unadjusted prices
    dfd_prev_rlx = load_values(restarthdf_prev_path, vars_rlx, adj_flag='Unadjusted')
    dfd_cur_rlx = load_values(restarthdf_path, vars_rlx, adj_flag='Unadjusted')
        
    # Get scalar values of IRELAX, CURIYR
    I_Relax = dfd_cur['IRELAX'].iloc[0,0]
    I_Cycle = dfd_cur['CURIRUN'].iloc[0,0]
    LastYear = dfd_cur['CURIYR'].iloc[0,0]
    TestYears = range( FirstYear-1,LastYear)

    name = 'RestartOUT_Cycle' + str(I_Cycle) +'.hdf5'    
    restartout_path = os.path.join(output_path, name)
    # copy current restart file to output to modify
    if not os.path.isfile(restartout_path):
        shutil.copy(restarthdf_path, restartout_path)
    
    # Check convergencies

    df_conv_res, df_regcv  = check_conv(dfd_prev, dfd_cur, vars_conv, df_conv, TestYears, RegRepFlag)
    # print regional debug report
    if RegRepFlag == 1:
        df_regcv = df_regcv.sort_values(by=['Year','Variable','Region', 'Sector'])
        df_regcv=df_regcv.set_index(['Variable','Year','Region', 'Sector'])
        OutputName = "IConv_reg_details" +"_" + str(I_Cycle) + ".csv"
        out_path = os.path.join(output_path, OutputName)
        df_regcv.to_csv(out_path)
    
    # Compute summary table
    RegFlag = ['US','REG']
    for fl in RegFlag:
        df_sum, df_weights, df_GPA = sum_conv(dfd_prev, dfd_cur, vars_cvtab, df_cvtab, fl)

        # write out the summary info
        #Limit which years to print
        write_debug(df_sum[df_sum['Year']>=FirstYear+1989], df_weights, df_GPA, I_Cycle, fl, output_path)

        #sort only the scores in the projected period that starts 2 years after last SEDS year and
        #take average of 3 worst (lowest) scores
        if fl == 'REG':
            cvscore = df_GPA.iloc[:, 8]
            Average_Score = calc_average_score(df_GPA.iloc[TestYears, 8])            
        else:
            cvscore_US = df_GPA.iloc[:, 8]
            Average_Score_US = calc_average_score(df_GPA.iloc[TestYears, 8])
    if Average_Score > Min_score:
        PFgpa = 1
    else:
        PFgpa = 0
    StopCode = update_reason(restarthdf_path, PFgpa) 

    #Update and write out variables for CONVERGE common block
    update_CVTAB(cvscore, cvscore_US, df_sum, restarthdf_path, restartout_path, I_Cycle) 

    #write GPA scores     
    write_GPA(Average_Score, Average_Score_US, I_Cycle, StopCode, Min_score, output_path)    
    
    # Perform Relaxation
    if I_Relax > 0:
        dfd_updated = perform_relax(dfd_prev_rlx, dfd_cur_rlx, vars_rlx, df_rlx, TestYears)

    name_rlx = 'RestartRLX_Cycle' + str(I_Cycle) +'.hdf5'
    pathrlx = os.path.join(output_path, name_rlx)
    # copy OUT restart file to output to create RLX restart 
    if not os.path.isfile(pathrlx):
        shutil.copy(restartout_path, pathrlx)

    # Create RLX restart with updated relaxed values
    write_values(pathrlx, vars_rlx, dfd_updated)
                
    return StopCode

if __name__ == "__main__":
    print('Start testing......')
    start = time.process_time()

    StopCode = main()

    elapsed = time.process_time() - start
    print(f'Elapsed time: {elapsed} seconds.\n')
    print('Completed!')
