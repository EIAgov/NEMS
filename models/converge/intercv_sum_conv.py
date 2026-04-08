import pandas as pd
import numpy as np
from convergence_check import set_change

def sum_conv(dfd_old, dfd_cur, my_vars, df, flag):
    """Summarizes the convergence for table 150 in NEMS Report Writer and summary report

    Parameters
    ----------
    dfd_old : dict
        containing variable dataframes of the previous iteration data.

    dfd_cur : dict
        containing variable dataframes of the current iteration data.

    my_vars : list
        A list of variables to check for convergences.
    
    df : pandas.DataFrame
        dataframe of Convergence Summary Table read in from converge/input/
        
    flag : str
        string to flag if computing summary table for US or REG
    

    Returns
    -------
    pandas.DataFrame
        variable dataframe with convergence results filtered by Variables in Group (QELRS QELCM QELIN QELTR, QNGRS QNGCM QNGIN QNGTR)

    pandas.DataFrame
        variable dataframe with convergence results filtered by Weight Class Name (e.g. End-Use Quantity, Electricity Quntity, End-Use Prices)

    pandas.DataFrame
        variable dataframe with Weighted Score for each Weighted Class Name and interpolated assigned GPA value
    """

    # Maximum fractional deviation for scoring purposes so large change in one class' 
    #percentage doesn't outweigh everything else
    MaxDev = 0.15
    #Compile score at US level ('US') or at the regional level ('REG')
    if flag=='REG':
        RegID = range(9)
    elif flag == 'US':
        RegID = 10 # indexing in Python starts @ 0

    df_temp2=pd.DataFrame()


    for var in my_vars:
        
        #Find the type of variable for test
        PQType = df.loc[var, 'PQType']

        if PQType == "Q":

            mask_tol_q = 100
            
            dfd_old_var = dfd_old[var]
            dfd_cur_var = dfd_cur[var]

            # Create log of Q's that are below the tolerance trills that are being modified
            if flag == "US": # Only do when flag == US so check only occurs once

                values_filtered = dfd_old_var[(dfd_old_var < 100) & (dfd_old_var > 0)]
                values_filtered = values_filtered.dropna(how = "all")
                values_filtered = values_filtered.dropna(axis=1, how="all")
                values_filtered = values_filtered.iloc[:, 30:]

                if values_filtered.empty != True:
                    with open ("converge/output/conv_tol_prev.txt", "a") as f:
                        f.write("======================== \n")
                        f.write(var)
                        f.write("\n")
                        f.write(values_filtered.to_string())
                        f.write("\n")
                        f.close

                values_filtered = dfd_cur_var[(dfd_cur_var < 100) & (dfd_cur_var > 0)]
                values_filtered = values_filtered.dropna(how = "all")
                values_filtered = values_filtered.dropna(axis=1, how="all")
                values_filtered = values_filtered.iloc[:, 30:]

                # Create log of Q's that are below the tolerance trills that are being modified
                if values_filtered.empty != True:
                    with open ("converge/output/conv_tol_cur.txt", "a") as f:
                        f.write("======================== \n")
                        f.write(var)
                        f.write("\n")
                        f.write(values_filtered.to_string())
                        f.write("\n")
                        f.close

            # Set to zero to apply the tolerance
            dfd_old_var = dfd_old_var.mask(dfd_cur_var < mask_tol_q, 0)
            dfd_cur_var = dfd_cur_var.mask(dfd_old_var < mask_tol_q, 0)

            df_cur = dfd_cur_var.iloc[RegID,:]
            df_old = dfd_old_var.iloc[RegID,:]

        #slice for appropriate regional level

        elif PQType == 'P':
            PQmatch = df.loc[var, 'QMATCH']

            #   Values increasing from zero or near-zero causes lots of instability.
            #   If a price/quantity is zero or near-zero for either the cur/old df, we set it to zero for both.
            dfd_old_PQmatch = dfd_old[PQmatch]
            dfd_cur_PQmatch = dfd_cur[PQmatch]

            dfd_old_var = dfd_old[var]
            dfd_cur_var = dfd_cur[var]

            mask_tol_q = 0.00001
            mask_tol_p = 0.00001

            # If data is less than the declared tolerance in one dataframe,
            # set the other dataframe's value to 0.
            dfd_old_PQmatch = dfd_old_PQmatch.mask(dfd_cur_PQmatch < mask_tol_q, 0)
            dfd_cur_PQmatch = dfd_cur_PQmatch.mask(dfd_old_PQmatch < mask_tol_q, 0)
            dfd_old_var = dfd_old_var.mask(dfd_cur_var < mask_tol_p, 0)
            dfd_cur_var = dfd_cur_var.mask(dfd_old_var < mask_tol_p,0)

            df_cur = (dfd_cur_var * np.maximum(dfd_cur_PQmatch, dfd_old_PQmatch)).iloc[RegID,:]
            df_old = (dfd_old_var * np.maximum(dfd_cur_PQmatch, dfd_old_PQmatch)).iloc[RegID,:]

        elif PQType == 'O':
            dfd_old_var = dfd_old[var]
            dfd_cur_var = dfd_cur[var]
            df_cur = dfd_cur_var.iloc[0,:]
            df_old = dfd_old_var.iloc[0,:]
        elif PQType == 'PC' or PQType == 'QC':
            dfd_old_var = dfd_old[var]
            dfd_cur_var = dfd_cur[var]
            df_cur = dfd_cur_var
            df_old = dfd_old_var
        else:
            dfd_old_var = dfd_old[var]
            dfd_cur_var = dfd_cur[var]
            df_cur = dfd_cur_var.iloc[RegID,:]
            df_old = dfd_old_var.iloc[RegID,:]
                
        abs_cng, frc_cng = set_change(df_old, df_cur)

        #start filling the summary table
        if PQType == 'PC' or PQType == 'QC' :
                                    
            df_temp = pd.concat([df_cur.stack(), df_old.stack(), abs_cng.stack()], axis=1)
            df_temp = df_temp.reset_index()
            df_temp.columns = ["Year" if x=='MNUMYR' else "Current" if x==0 else "Previous" if x==1 \
                                      else "Abs Change" if x==2 else "Region" if (x=='MNUMCR' or x=='NDREGN') \
                                       else "Sector" for x in df_temp.columns]
            
            #sum all the regions
            df_temp1 = pd.DataFrame(df_temp.groupby(['Sector','Year'])[["Current","Previous","Abs Change"]].sum()).reset_index()
            df_temp1.insert(0, 'Variable', var)
            df_temp1['CVTAB ID']= df.loc[var, 'CVTAB ID'] 
            df_temp1['Weight_Class_Name']= df.loc[var, 'Weight_Class_Name']
            df_temp1['Weight_Class']= df.loc[var, 'Weight_Class']
            df_temp1['Weight_Start']= df.loc[var, 'Weight_Start']
            
            #Get rid of Sector to get format needed for CVTAB variable           
            df_temp1['Variable'] = df_temp1['Variable'] + df_temp1["Sector"].astype(str)
            df_temp1['Variables_in_Group']= df_temp1['Variable']
            df_temp1['CVTAB ID'] = df_temp1['CVTAB ID'] + df_temp1["Sector"]-1
            df_temp1 = df_temp1[df_temp1['Sector']<=35]
            df_temp1 = df_temp1.drop(columns=['Sector'])
                      
        elif flag=='REG' and PQType != 'O':
            
            df_temp = pd.concat([df_cur.stack(), df_old.stack(), abs_cng.stack()], axis=1)
            df_temp = df_temp.reset_index()
            df_temp.columns = ["Year" if x=='MNUMYR' else "Current" if x==0 else "Previous" if x==1 \
                                      else "Abs Change" if x==2 else "Region" if x=='MNUMCR' \
                                       else "Sector" for x in df_temp.columns]

            #sum all the regions
            df_temp1 = pd.DataFrame(df_temp.groupby(['Year'])[["Current","Previous","Abs Change"]].sum()).reset_index()
            df_temp1.insert(0, 'Variable', var)
            df_temp1['CVTAB ID']= df.loc[var, 'CVTAB ID']
            df_temp1['Variables_in_Group']= df.loc[var, 'Variables_in_Group']
            df_temp1['Weight_Class_Name']= df.loc[var, 'Weight_Class_Name']
            df_temp1['Weight_Class']= df.loc[var, 'Weight_Class']
            df_temp1['Weight_Start']= df.loc[var, 'Weight_Start']
        
        else:
            df_temp = pd.concat([df_cur, df_old, abs_cng], axis=1)
            df_temp = df_temp.reset_index()
            
            df_temp.columns = ['Year', 'Current', 'Previous', 'Abs Change']
            
            df_temp.insert(0, 'Variable', var)
            df_temp['CVTAB ID']= df.loc[var, 'CVTAB ID']
            df_temp['Variables_in_Group']= df.loc[var, 'Variables_in_Group']
            df_temp['Weight_Class_Name']= df.loc[var, 'Weight_Class_Name']
            df_temp['Weight_Class']= df.loc[var, 'Weight_Class']
            df_temp['Weight_Start']= df.loc[var, 'Weight_Start']
            df_temp1 = df_temp
        
        df_temp2 = (df_temp1.copy() if df_temp2.empty else df_temp2.copy() if df_temp1.empty \
                      else pd.concat([df_temp2, df_temp1],ignore_index=True))
    
    #Sum by CVTAB ID 
    df_sum = pd.DataFrame(df_temp2.groupby(['Year','CVTAB ID','Variables_in_Group','Weight_Class_Name', \
                                            'Weight_Class','Weight_Start']) \
                                                [["Current","Previous","Abs Change"]].sum()).reset_index()
    df_sum = df_sum.sort_values(by=['Year','CVTAB ID'])
    df_sum["Year"] += 1989
    
    #compute absolute value deviations
    df_sum['Deviation'] = np.where(df_sum['Previous']!=0, 100*df_sum['Abs Change']/abs(df_sum['Previous']), \
                                   np.where(df_sum['Current']!=0, \
                                            100*df_sum['Abs Change']/abs(df_sum['Current']), 0))
    #compute signed percentage changes for table 150
    df_sum['Signed_Deviation'] = np.where(df_sum['Previous']!=0, 100*(df_sum['Current']/ \
                                                                     df_sum['Previous'] - 1), 0)
            
    #Calculate weighted scores and GPA
    df_sum['Weight_Sum'] = df_sum['Current'] * df_sum['Deviation'] * 0.01
    

    df_weights = pd.DataFrame(df_sum.groupby(['Year','Weight_Class_Name','Weight_Class','Weight_Start']) \
                              [['Current', 'Weight_Sum']].sum()).reset_index()
    df_weights = df_weights[df_weights['Weight_Class'] !=0].sort_values(by=['Year','Weight_Class'])
    
    #Assign weights 
    df_weights['Weight'] = np.where(df_weights['Current']!=0, df_weights['Weight_Start'], 0)
    df_weights['Average Deviation'] = np.where(df_weights['Current']>0, \
                                               df_weights['Weight_Sum']/df_weights['Current'], 0)
    df_weights['Deviation for Scoring'] = df_weights['Average Deviation'].clip(upper= MaxDev)

    df_w = df_weights.set_index(['Year', 'Weight_Class']).unstack(level=-1)

    df1 = (df_w.xs('Deviation for Scoring', axis=1, level=0) * \
           df_w.xs('Weight', axis=1, level=0)).join(df_weights[['Year','Weight']].groupby(['Year']).sum())

    df1=df1.div(df1['Weight'], axis=0).drop(columns = ['Weight'])

    df_GPA = df1.copy()
    df_GPA.columns = df_GPA.columns.astype(int)
    df_GPA['Total'] = df_GPA.sum(axis=1)

    df1.columns = df1.columns.astype(float)
    df1.columns = pd.MultiIndex.from_product([['Weighted Score'], df1.columns])

    df_w = df_w.join(df1)
    df_w.infer_objects(copy=False)
    df_w = df_w.stack(level=1, future_stack=True)
    df_w.rename(columns={'Current':'Sum of Current Values', 'Weight_Sum':'Weighted Sum of Deviations'}, inplace=True)

    #Convert a score to a GPA-type grade (4.0 to 0.) based on a grading scale (scale_it)
    score = df_GPA['Total']
    scale_it = [0.005, 0.02, 0.05, 0.10, 0.15]
    real_grade = [4, 3, 2, 1, 0.0001]
    df_GPA['Grade'] = np.interp(score, scale_it, real_grade)

    return df_sum, df_w, df_GPA
