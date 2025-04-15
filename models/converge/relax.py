# Perform relaxation

import os
import pandas as pd

def perform_relax(dfd_prev, dfd_cur, my_vars, df_rlx, fyear):
    """
    Heuristic routine to limit or reset certain model outputs
    between iterations to speed overall convergence.

    Parameters:
        dfd_prev (dict): containing variable dataframes of the previous iteration data.
        dfd_cur (dict): containing variable dataframes of the current iteration data.
        vars_rlx (list): A list of variables to check for convergences.
        df_rlx (pandas.DataFrame): containing relaxation parameters.

    Returns:
        pandas.DataFrame: containing the relaxation results.

    Note:
     
    """
    dfd_updated = dfd_cur
    for var in my_vars:

        RegCD = 9
        #exclude MNUMCR and reg=10 from P and Q data and get a slice for specified years
        #Find the type of variable 
        #PQType = df_rlx.loc[var, 'PQType']
        #if PQType == 'Q' or PQType == 'P':
        #    df_cur = dfd_cur[var].iloc[0:RegCD, fyear]
        #    df_prev = dfd_prev[var].iloc[0:RegCD, fyear]
        #else:
        #    df_cur = dfd_cur[var].iloc[:, fyear]
        #    df_prev = dfd_prev[var].iloc[:, fyear]
        df_cur = dfd_cur[var].iloc[:, fyear]
        df_prev = dfd_prev[var].iloc[:, fyear]

 
        # Get the Relaxation factor
        RLXParam = df_rlx.loc[var, 'RelaxFactor']

        dfd_updated[var].iloc[:, fyear] = df_prev + RLXParam * (df_cur - df_prev)

    return dfd_updated