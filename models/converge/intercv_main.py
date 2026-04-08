"""

@authors: YDA
"""

import os
import time
import numpy as np
import pandas as pd

from intercv_data_preprocess import read_icnv_file
from convergence_check import check_conv
from relax import perform_relax
from intercv_sum_conv import sum_conv
import intercv_npz_util
from datetime import datetime

import warnings
from tables import NaturalNameWarning
warnings.filterwarnings('ignore', category=NaturalNameWarning)

MODULE_NAME = "intercv_main.py"
LOGFILE = "nohup.out"

def log_it(n, s, fout=False):
    """Append a formatted message to LOGFILE (nohup.out).
    
    LOGGFILE is a module-level hardcoded parameter.

    sys.stdout.flush() and sys.stederr.flush() ensure that
    nothing is left in the buffer.

    Parameters
    ----------
    n : integer
        cycle number
    s : string
        message to write in logfile
    """

    if not fout:
        fout = LOGFILE

    with open(fout, "a", encoding="utf-8") as f:
        f.write(f"{datetime.now()} :: cycle {n} :: {MODULE_NAME} :: {s}\n")
        f.flush()



def calc_average_score(GPA):
    """Sort the scores and take average of 3 worst (lowest) scores

    Parameters
    ----------
    GPA : pandas.Series
        containing GPA scores.
        
    Returns
    -------
    numpy.float64
        average of the lowest 3 GPA scores
    
    """
    
    #sort only the scores in the projected period that starts 2 years after last SEDS year and
    Score_sorted = GPA.sort_values()
    #take average of 3 worst (lowest) scores
    Average = Score_sorted[:3].mean()

    return Average

def write_GPA(ave, ave_US, cycle, StopCode, MinScore, path):
    """Write US and Regional GPA summary for corresponding cycle number to log file

    Writes a summary message to the nohup.out or terminal if the cycle has passed or failed
    the GPA threshold. Logs the GPA value for both US and regional to the following file in
    the "converge/output" directory:

    * Iconv_AverageGPA.csv

    Parameters
    ----------
    ave : float64
        Regional GPA value
    
    ave_US : float64
        National GPA value
    
    cycle : int
        current cycle number
    
    StopCode : int
        return code to indicate if cycle has passed the minimum GPA. 1 = Pass, 0 = Fail

    MinScore : float
        selected GPA value for convergence to exceed (e.g. 3.9) to be considered "PASS"

    path : str
        path to converge/output directory
        
    Returns
    -------
    numpy.float64
        average of the lowest 3 GPA scores
    
    """
        
    df= pd.DataFrame.from_records([{'Cycle': str(cycle), 'Regional GPA': ave, 'US GPA': ave_US, \
                                     'PASS/FAIL': ''}], index='Cycle')
    
    log_it(cycle, f'Cycle {str(cycle)} GPA (US: {ave_US = :.2f}; REG {ave = :.2f}) on a 4 point scale \
(averaged over 3 worst years) with {MinScore} considered minimally acceptable.')
    if StopCode == 1:
        df['PASS/FAIL'] = 'PASS'
        log_it(cycle, f'Congratulations for passing our rigorous testing standards.   GPA: **PASS**')
    else:
        df['PASS/FAIL'] = 'FAIL'
        log_it(cycle, f'Warning:  additional run cycles may be needed.   GPA: **FAIL**')

    name = 'Iconv_AverageGPA.csv'
    out_path = os.path.join(path, name)
    # if file does not exist write header 
    if not os.path.isfile(out_path):
        df.to_csv(out_path, header='column_names')
    else: # else it exists so append without mentioning the header
        df.to_csv(out_path, mode='a', header=False)

def write_debug(df_sum, df_weights, df_GPA, cycle, fl, path):
    """Write debug information into .csv under "converge/output" directory

    The following are output files written to:

    * IConvSummary_<cycleNumber>.csv
    * IConv_Weighted_Deviations_<cycleNumber>.csv
    * IConv_GPAscores_debug<cycleNumber>.csv

    Parameters
    ----------
    df_sum : pandas.DataFrame
        containing summary of non-converged variables.
    
    df_weights : pandas.DataFrame
        weighted scores for debug.
    
    df_GPA : pandas.DataFrame
        Weighted scores by class and final GPA for all years
    
    cycle : int
        current cycle number
    
    fl : str
        flag REG or US
    
    path : str
        path to output directory
        
    Returns
    -------
    None
    
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


def update_CVTAB(score, score_US, df, path, cycle, NEMSVardf):
    """Update CONVERGE common block variables for restart file

    Parameters
    ----------
    score : numpy.ndarray
        GPA scores to fill CVSCORE
     
    score_US : numpy.ndarray
        GPA scores to fill CVSCORE_US

    df : pandas.DataFrame
        containing info for CVTAB variable. 
    
    path : str
        restart file path location
    
    cycle : int
        current cycle number
        
    NEMSVardf : pandas.DataFrame
        dataframe of all variables in the restart file with their dimensions
        
    Returns
    -------
    None
    
    """

    cv_vars = ['CVTAB', 'CVSCORE', 'CVSCORE_US', 'CVSCOREHIST']
    
    # Load data
    dfd = intercv_npz_util.read_npz_to_dfd(path, cv_vars, NEMSVardf, adj_flag='Adjusted')
    
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
    df = df.set_index(['MNUMYR', 'M106']).stack()
    df.index.set_names('M4', level=2, inplace=True)
    df=df.unstack(level=0)
    df = df.astype('float32')
    dfd['CVTAB'].update(df)
    
    #write out the restart file with updated CVTAB information
    npz_out = "restart.npz"
    intercv_npz_util.write_to_npz(path, npz_out, cv_vars, dfd, NEMSVardf)

    return

def update_reason(path, PFgpa, NEMSVardf):
    """Updates the reason (flag) for stopping or continuing

    Parameters
    ----------
    path : str
        path for restart file 
    
    PFgpa : int
        Pass/Fail indicator (1 = Pass, 0 = Fail)

    NEMSVardf : pandas.DataFrame
        dataframe of all variables in the restart file with their dimensions
        
    Returns
    -------
    int
        return code to indicate if cycle has passed the minimum GPA. 1 = Pass, 0 = Fail
    
    """

    ReasonYes_var = ['REASONYES']
    continew_vars = ['CONTINW', 'CONTINM', 'CONTINR', 'CONTINK', 'CONTINI', 'CONTINT', 
                     'CONTINE', 'CONTINC', 'CONTINL', 'CONTING', 'CONTINO', 'CONTINN', 'CONTINH']

    continew = intercv_npz_util.read_npz_to_dfd(path, continew_vars, NEMSVardf, adj_flag='Adjusted')
    ReasonYes = intercv_npz_util.read_npz_to_dfd(path, ReasonYes_var, NEMSVardf, adj_flag='Adjusted')

    RYes = ReasonYes['REASONYES'].iloc[0,0]
    for var in continew_vars:
        if continew[var].any().any() == 0:
            RYes = 0
    
    if PFgpa == 1 and RYes == 1:
        StopCode = 1
    else:
        StopCode = 0
    
    return StopCode
    

def main(restartnpz_prev, restartnpz_cur, Min_score):
    """Main entrypoint into inter-cycle convergence code

    Parameters
    ----------
    restartnpz_prev : str
        file path to input previous cycle restart file

    restartnpz_cur : str
        file path to input current cycle restart file

    Min_score : float
        GPA value for convergence passing score (e.g. 3.9)
        
    Returns
    -------
    int
        return code to determine if convergence is met (0: convergence not achieved, 1: convergence is achieved)
    
    """


    #Flag: write out regional convergence info =1; no=0
    RegRepFlag = 1
    #Set FirstYear = MSEDYR+2; PARAMETER(MSEDYR=34)   ! Number of Historical SEDS years
    FirstYear = 36
    
    # Set working paths (e.g. input and output)
    input_path = os.path.join(os.getcwd(), r'converge\\input')
    output_path = os.path.join(os.getcwd(), r'converge\\output')
    convergence_path = os.path.join(input_path, 'icnv_')

    # Create output directory
    os.makedirs(output_path, exist_ok=True)

    # Read in NEMSVardf.csv as dataframe
    NEMSVardf = intercv_npz_util.read_NEMSvardf()
    
    # Read convergence and relaxation settings
    df_conv, df_rlx, df_cvtab, all_vars, vars_conv, vars_rlx, vars_cvtab = read_icnv_file(convergence_path)

    # Get CURIYR and other parameters from the restart as well
    my_params = ['IRELAX','CURIRUN','CURIYR']
        
    all_vars = my_params + all_vars
    
    
    # Load variables from hdf
    # For convergence testing use adjusted prices
    dfd_prev = intercv_npz_util.read_npz_to_dfd(restartnpz_prev, all_vars, NEMSVardf, adj_flag='Adjusted')
    dfd_cur = intercv_npz_util.read_npz_to_dfd(restartnpz_cur, all_vars, NEMSVardf, adj_flag='Adjusted')

    # For relaxation use unadjusted prices
    dfd_prev_rlx = intercv_npz_util.read_npz_to_dfd(restartnpz_prev, all_vars, NEMSVardf, adj_flag='Unadjusted')
    dfd_cur_rlx = intercv_npz_util.read_npz_to_dfd(restartnpz_cur, all_vars, NEMSVardf, adj_flag='Unadjusted')

    # Get scalar values of IRELAX, CURIRUN, CURIYR
    I_Relax = dfd_cur['IRELAX'].iloc[0,0]
    I_Cycle = dfd_cur['CURIRUN'].iloc[0,0]
    LastYear = dfd_cur['CURIYR'].iloc[0,0]
    TestYears = range(FirstYear-1,LastYear)

    
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
    StopCode = update_reason(restartnpz_cur, PFgpa, NEMSVardf)


    #Update and write out variables for CONVERGE common block
    update_CVTAB(cvscore, cvscore_US, df_sum, restartnpz_cur, I_Cycle, NEMSVardf)

    #write GPA scores     
    write_GPA(Average_Score, Average_Score_US, I_Cycle, StopCode, Min_score, output_path)
    
    # Perform Relaxation
    if I_Relax > 1:
        dfd_updated = perform_relax(dfd_prev_rlx, dfd_cur_rlx, vars_rlx, df_rlx, TestYears)

        npz_start = "restart.npz"   #starting npz to read in
        npz_out = "restart.rlx"     #output name of npz file with relaxed values
        # Create RLX restart with updated relaxed values
        intercv_npz_util.write_to_npz(npz_start, npz_out, vars_rlx, dfd_updated, NEMSVardf)

        #rename/replace the writtenout npz from restart.rlx.npz to restart.rlx
        if os.path.exists(npz_out):
                os.replace(f"{npz_out}.npz", npz_out)
        else:
            os.rename(f"{npz_out}.npz", npz_out)

    return StopCode

if __name__ == "__main__":
    log_it(0, 'Start testing......')
    start = time.process_time()

    prev_restart = "converge/input/restart.1.npz"
    curr_restart = "converge/input/restart.2.npz"
    min_score = 3.9
    os.chdir("..")
    StopCode = main(prev_restart, curr_restart, min_score)

    elapsed = time.process_time() - start
    log_it(0, f'Elapsed time: {elapsed} seconds.\n')
    log_it(0, 'Completed!')
