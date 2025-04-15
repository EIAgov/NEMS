# 
import os
import pandas as pd

def set_change(df_prev, df_cur):
    """
    Set fractional and absolute change variables, given current and previous values.

    Parameters:
        df_prev: variable dataframe of the previous iteration.
        df_cur:  variable dataframe of the current iteration.
        
    Returns:
        abs_cng: absolute change variable.
        frc_cng: fractional change variable.
    
    """    
    abs_cng = abs(df_cur - df_prev)
                
    denom = (df_cur + df_prev)/2

    frc_cng = (abs_cng / denom).fillna(0)

    return abs_cng, frc_cng

def check_conv(dfd_old, dfd_cur, my_vars, df_conv, fyear, RegRepFlag):
    """
    Check convergences between previous and current data based on specified tolerances.

    Parameters:
        dfd_old (dict): containing variable dataframes of the previous iteration data.
        dfd_cur (dict): containing variable dataframes of the current iteration data.
        my_vars (list): A list of variables to check for convergences.
        df_conv (pandas.DataFrame): containing convergence parameters.
        fyear: first year of data to be checked
        lyear: last year of data to be checked

    Returns:
        pandas.DataFrame: containing the convergence results.

    Note:
     
    """
    
    RegCD = 9
    df_regConv=pd.DataFrame()
    #TODO: Set up the TRIGGER. Need to get MNDBGCN RTOValue for that
    for var in my_vars:
        
        #Find the type of variable for convergence test
        PQType = df_conv.loc[var, 'PQType']

        #exclude MNUMCR and reg=10 from P and Q data and get a slice for specified years
        if PQType == 'Q' or PQType == 'P':
            
            df_cur = dfd_cur[var].iloc[0:RegCD, fyear]
            df_old = dfd_old[var].iloc[0:RegCD, fyear]
            
            if PQType == 'P':
                #find matching Q value
                PQmatch = df_conv.loc[var, 'QMATCH']
                dfQ_cur = dfd_cur[PQmatch].iloc[0:RegCD, fyear]
                dfQ_old = dfd_old[PQmatch].iloc[0:RegCD, fyear]
        else:
            df_cur = dfd_cur[var].iloc[:, fyear]
            df_old = dfd_old[var].iloc[:, fyear]
            #print(df_cur)
            if PQType == 'PU' or PQType == 'PS':
                #find matching Q value
                PQmatch = df_conv.loc[var, 'QMATCH']
                dfQ_cur = dfd_cur[PQmatch].iloc[:, fyear]
                dfQ_old = dfd_old[PQmatch].iloc[:, fyear]


        abs_cng, frc_cng = set_change(df_old, df_cur)
        AbsTol = df_conv.loc[var, 'AbsoluteTolerance']
        FrcTol = df_conv.loc[var, 'FractionTolerance']
        
        
        if PQType == 'P' or PQType == 'PU' or PQType == 'PS': #if Prices
            
            #find matching Q value
            PQmatch = df_conv.loc[var, 'QMATCH']
            QAbsTol = df_conv.loc[PQmatch, 'AbsoluteTolerance']
            
            
            # Compare with tolerances
            if (frc_cng >= FrcTol).any().any() \
                and (abs_cng >= AbsTol).any().any() \
                    and ((dfQ_cur>=QAbsTol).any().any() \
                        or (dfQ_old>=QAbsTol).any().any()) :
            
                # Put a flag to indicate not converged
                df_conv.loc[var, 'NonconvVar'] = 1
            #create a dataframe for the regional output/debug file 
            if RegRepFlag == 1:
                #print(var)
                #print(df_cur)
                if isinstance(df_cur,pd.DataFrame):
                    df_temp = pd.concat([df_cur.stack(), df_old.stack(), frc_cng.stack()], axis=1)
                    df_temp.columns = ['cur', 'prev', 'prcng']
                    df_temp = df_temp[(frc_cng.stack().reindex_like(df_temp) >= FrcTol) & \
                                    (abs_cng.stack().reindex_like(df_temp) >= AbsTol) & \
                                    ((dfQ_cur.stack().reindex_like(df_temp)>=QAbsTol) | \
                                     (dfQ_old.stack().reindex_like(df_temp)>=QAbsTol))]
                elif isinstance(df_cur,pd.Series): # stack doesn't work when df has a single year
                    df_temp = pd.concat([df_cur, df_old, frc_cng], axis=1)
                    df_temp.columns = ['cur', 'prev', 'prcng']
                    df_temp = df_temp[(frc_cng.reindex_like(df_temp) >= FrcTol) & \
                                    (abs_cng.reindex_like(df_temp) >= AbsTol) & \
                                    ((dfQ_cur.reindex_like(df_temp)>=QAbsTol) | \
                                     (dfQ_old.reindex_like(df_temp)>=QAbsTol))]
                    df_temp.insert(0, 'MNUMYR', fyear+1)
                
                
                df_temp = df_temp.reset_index()
                
                df_temp.columns = ["Year" if x=='MNUMYR' else "Current" if x=='cur' else "Previous" \
                                   if x=='prev' else "Percent Change" if x=='prcng' else \
                                   "Region" if (x=='MNUMCR' or x=='NDREGN' \
                                    or x=='NNGEMM' or x=='MNUMPR' or x=='MNUMOR' or x=='MX_SO2' or \
                                        x=='NUMCAN' or x=='MX_NCL' or x=='MX_UNT' or x=='M15') else \
                                            "Sector" for x in df_temp.columns]
                df_temp.insert(0, 'Variable', var)
                df_temp['Limit']= df_conv.loc[var, 'FractionTolerance']*100
                if not 'Sector' in df_temp.columns:
                    df_temp['Sector']=0
                if not 'Region' in df_temp.columns:
                    df_temp['Region']=0
 
        else:
            # if quantities
            # Compare with tolerances
            if (frc_cng >= FrcTol).any().any() \
                and (abs_cng >= AbsTol).any().any():

                # Put a flag to indicate not converged
                df_conv.loc[var, 'NonconvVar'] = 1

            #create a dataframe for the regional output/debug file 
            if RegRepFlag == 1 and PQType !='Z':
                if isinstance(df_cur,pd.DataFrame):
                    df_temp = pd.concat([df_cur.stack(), df_old.stack(), frc_cng.stack()], axis=1)
                    df_temp.columns = ['cur', 'prev', 'prcng']
                    df_temp = df_temp[(frc_cng.stack().reindex_like(df_temp) >= FrcTol) & \
                                    (abs_cng.stack().reindex_like(df_temp) >= AbsTol) ]
                elif isinstance(df_cur,pd.Series): # stack doesn't work when df has a single year
                    df_temp = pd.concat([df_cur, df_old, frc_cng], axis=1)
                    df_temp.columns = ['cur', 'prev', 'prcng']
                    df_temp = df_temp[(frc_cng.reindex_like(df_temp) >= FrcTol) & \
                                    (abs_cng.reindex_like(df_temp) >= AbsTol) ]
                    df_temp.insert(0, 'MNUMYR', fyear+1)
                
                df_temp = df_temp.reset_index()
                
                df_temp.columns = ["Year" if x=='MNUMYR' else "Current" if x=='cur' else "Previous" \
                                   if x=='prev' else "Percent Change" if x=='prcng' else \
                                   "Region" if (x=='MNUMCR' or x=='NDREGN' \
                                    or x=='NNGEMM' or x=='MNUMPR' or x=='MNUMOR' or x=='MX_SO2' or \
                                        x=='NUMCAN' or x=='MX_NCL' or x=='MX_UNT' or x=='M15') else \
                                            "Sector" for x in df_temp.columns]
                df_temp.insert(0, 'Variable', var)
                df_temp['Limit']= df_conv.loc[var, 'FractionTolerance']*100
                if not 'Sector' in df_temp.columns:
                    df_temp['Sector']=0
                if not 'Region' in df_temp.columns:
                    df_temp['Region']=0
 
        if RegRepFlag == 1:
            df_regConv = (df_temp.copy() if df_regConv.empty else df_regConv.copy() if df_temp.empty \
                      else pd.concat([df_regConv, df_temp],ignore_index=True))
        
    if RegRepFlag == 1:
        df_regConv = df_regConv[['Variable','Year','Region','Sector','Current','Previous','Percent Change', \
                                 'Limit']]
        df_regConv["Year"] += 1989
        df_regConv["Percent Change"] *= 100
    #print(df_regConv)
    #df_regConv = df_regConv.sort_values(by=['Percent Change'], ascending= False)
    #df_regConv = df_regConv.sort_values(by=['Year','Variable'])
    
    #Filter non-converged variables and sort by the model
    df_conv = df_conv[df_conv['NonconvVar']==1].sort_values(by=['Module'])
    #df_regConv.to_csv('conv_debug.csv')    
    return df_conv, df_regConv