# -*- coding: utf-8 -*-
"""

NEMS Production Code for propane and ethane price projections

This Python code is based on the R code constructed by
Giovanni Petris and Janice Lent. Interested readers should consult
the working paper "A Multi-process Dynamic Model for Prices of
Hydrocarbon Gas Liquids" by Janice Lent, Peter Gross, and Giovanni Petris
(08/09/2019).

Parameter assignments in the original R code were maintained in most
instances so that interested readers can refer to the original R code
descriptions found in the monograph:
    "Dynamic Linear Models with R"
    by Giovanni Petris, Sonia Petrone, and Patrizia Campagnoli
    Submitted 2007 to Springer and published June 2009
    on pages 31-84 of the Journal of Applied Statistics
     10.1007/b135794_2. 

The concept is that annual propane and ethane prices may be 
forecast in future years from forecasts of Brent prices, 
Henry Hub prices, organics demand, and propane production 
quantities based on a dynamic linear model that determines linear
coefficients based on the dependencies in past years.

Selections from Petris et al. (2009) to guide parameter conventions where
t indicates a time-based subscript:
    'The observable process (Yt) depends on the latent state 
    pro￾cess (θt), which has a simpler, Markovian dynamics, and we can reasonably
    assume that the observation Yt only depends on the state of the system at
    the time the measurement is taken, θt.'

    'Dynamic linear models are specified by means of two equations
        Yt = Ftθt + vt, vt ∼ Nm(0, Vt),              (2.4)
        θt = Gtθt−1 + wt, wt ∼ Np(0, Wt),
    where Gt and Ft are known matrices and the (vt) and (wt) are two independent
    white noise sequences (i.e., they are independent, both between them and
    within each of them), with mean zero and known covariance matrices Vt and
    Wt respectively.'
    
    'Furthermore, it is assumed that θ0 has a Gaussian distribution,
        θ0 ∼ Np(m0, C0),                            (2.5)
    for some non-random vector m0 and matrix C0, and it is independent on
    (vt) and (wt).'

    'The required components of a dlm object are m0, C0, FF, V, GG, W, 
    which correspond to the vector/matrices m0, C0, Ft, Vt, Gt, Wt 
    in (2.5) and (2.4), assuming that Ft, Vt, Gt, Wt do not vary with t.'

This code is organized within a main definition that relies on eleven 
definitions/functions:
    setFG
    setF
    setup_random_walk_dlm
    read_input_data
    setup_expert_based_dlms
    compute_forecast_weights
    create_pdf_plots
    create_propane_consumption_trend
    prep_propane_model
    fit_new_propane_model
    pickle_and_save_results
In addition, a class package named dlm.py is called.

The model expects a single argument, the Run Name, to identify the
file to be processed.

The model operates from two inputs:
    NGLPriceInputParameters.csv:
        -- directories and file name extensions
        -- years describing historical and future events
        -- parameters chosen by Petris and Lent for running R code 
            to describe expected behavior
    Input data from NEMS:
        Single csv file with rows beginning in historic years and extending through the 
        model forecast period containing the following NEMS parameters
        in this particular order (* indicates columns used in this model):
            A. * Year
            B. total NGPL production (Mbbl/d)
            C. ethane annual NGPL production (Mbbl/d)
            D. * propane annual NGPL production (Mbbl/d)
            E. ethane annual cents/gal (nominal)
            F. * ethane annual cents/gal (real 2011$)
            G. propane annual cents/gal (nominal)
            H. * propane annual cents/gal (real 2011$)
            I. * organic shipment demand (billion 2009$)
            J. resin shipment demand (billion 2009$)
            K. total chemical demand (billion 2009$)
            L. WTI (2011$/ barrel)
            M. * HH (2011$/MMBTU)
            N. deflator to 1987$
            O. * Brent (2011$)
            P. * ethane consumed (trils)
            Q. * propane consumed (trils) 
"""

from dlm import dlm
from matplotlib.backends.backend_pdf import PdfPages
import pandas as pd
import numpy as np
import statsmodels.formula.api as smf
import pickle  
import copy
import matplotlib.pyplot as plt
from sklearn.linear_model import LinearRegression
from numpy.linalg import inv
# from sys import argv
import os
import shutil
import time

''' The functions setFG and setF expand the X matrix in the
dlm object into a collection of separate FF and GG matrices,
one for each time period.  The expansion is dictated by the 
elements of the JFF and GFF matrices.  The X matrix contains 
the time varying components of the observation and system
matrices, as discussed on page 45 of Petris et al. (2009).'''
def setFG(mod):
    """
    Expand the X matrix in the dlm object into separate FF and GG matrices,
    one for each time period, as specified by JFF and JGG matrices.

    Arguments:
    mod: a dlm object with time-varying observation and system matrices.

    Returns:
    mod: the same input object with additional components 'FFa' and 'GGa'
      containing sequences of observation and system matrices built 
      from the 'X' component.
    
    Create a local copy of the dlm object"""
    mod = copy.deepcopy(mod)

    ''' Create a matrix of logical variables indicating which elements of 
    mod.components['JFF'] (mod.components['JGG']) are positive.
    The values of the positive elements of mod.components['JFF'] 
    (mod.components['JGG']) identify the columns of X that contain
    the values of the time-varying elements of FF (GG).'''
    nz_FF = mod.components['JFF'] != 0
    nz_GG = mod.components['JGG'] != 0

    ''' Create new JFF and JGG matrices with the correct dimensions'''
    rows, cols = np.where(nz_FF)
    mod.components['JFF'] = np.column_stack((rows + 1, cols + 1, mod.components['JFF'][nz_FF]))
    rows, cols = np.where(nz_GG)
    mod.components['JGG'] = np.column_stack((rows + 1, cols + 1, mod.components['JGG'][nz_GG]))

    ''' Assign a variable ll indicating the number of time periods.'''
    ll = mod.components['X'].shape[0]

    ''' Create 3-dimensional arrays to hold the set of GG and FF matrices, 
    one matrix for each time period.'''
    FFa = np.zeros((mod.components['FF'].shape[0], mod.components['FF'].shape[1], ll))
    GGa = np.zeros((mod.components['GG'].shape[0], mod.components['GG'].shape[1], ll))

    ''' Create local copies of the FF and GG matrices.'''
    FFmat = mod.components['FF'].copy()
    GGmat = mod.components['GG'].copy()

    ''' Populate the 3-dimensional arrays. Each row of X contains the 
    time-varying elements for one time period.
    The loop runs through the time periods from 1 to ll.'''
    for tt in range(ll):
        ''' Update FFmat'''
        for jff in mod.components['JFF']:
            if jff[2] > 0:
                ''' Convert to integer and adjust for Python's 'zero-based indexing'''
                row_index = int(jff[0]) - 1  
                col_index = int(jff[1]) - 1
                x_col_index = int(jff[2]) - 1

                ''' Check if indices are within the bounds of the matrices'''
                if 0 <= row_index < FFmat.shape[0] and \
                    0 <= col_index < FFmat.shape[1] and \
                    0 <= x_col_index < mod.components['X'].shape[1]:
                    FFmat[row_index, col_index] = mod.components['X'][tt, x_col_index]

        ''' Update FFa'''
        FFa[:, :, tt] = FFmat

        ''' Update GGmat with values from the X matrix'''
        for index in range(mod.components['JGG'].shape[0]):
            i, j, val = mod.components['JGG'][index]
            GGmat[int(i)-1, int(j)-1] = mod.components['X'][tt, int(val)-1]

        ''' Assign the updated GGmat to the corresponding slice of GGa.
        Using .copy() to ensure a deep copy is made'''
        GGa[:, :, tt] = GGmat.copy() 

    ''' Incorporate the new arrays into the dlm object mod'''
    mod.components['FFa'] = FFa
    mod.components['GGa'] = GGa

    ''' Return the expanded dlm object'''
    return mod

def setF(mod):
    """
    Enhances an object of class 'dlm' with a time-varying observation matrix.

    This function takes a 'dlm' class object with a time-varying 
    observation matrix and adds an additional component 'FFa'. 
    This component contains a sequence of observation matrices 
    constructed from the 'X' component of the input object. The 
    matrices are stored as 3-dimensional arrays, with the last index 
    representing the time index.

    Arguments:
    mod (dlm object): An object of class 'dlm' that needs to be enhanced 
      with the time-varying observation matrix.

    Returns:
    dlm object: The same object as the input, but enhanced with an 
      additional 'FFa' component, which includes
      the sequence of observation matrices as 3-dimensional arrays.
    """
    mod = copy.deepcopy(mod)

    ''' Create a matrix of logical variables indicating 
    which elements of mod.components['JFF'] are positive.'''
    nz_FF = mod.components['JFF'] != 0

    ''' Initialize a list to store (row, column, value) tuples.'''
    jff_list = []

    ''' Iterate over the nz_FF array and append the non-zero values 
    to jff_list.'''
    for i in range(nz_FF.shape[0]):
        for j in range(nz_FF.shape[1]):
            if nz_FF[i, j]:
                jff_list.append((i + 1, j + 1, mod.components['JFF'][i, j]))

    ''' Convert jff_list to a numpy array and assign it back to JFF component.'''
    mod.components['JFF'] = np.array(jff_list)

    ''' Assign a variable ll indicating the number of time periods.'''
    ll = mod.components['X'].shape[0]

    ''' Create 3-dimensional arrays to hold the set of FF matrices, 
    one matrix for each time period.'''
    FFa = np.zeros((mod.components['FF'].shape[0], mod.components['FF'].shape[1], ll))

    ''' Create local copies of the FF and GG matrices.'''
    FFmat = mod.components['FF'].copy()

    ''' Populate the 3-dimensional arrays. Each row of X contains 
    the time-varying elements for one time period.
    The loop runs through the time periods from 1 to ll.'''
    for tt in range(ll):
        ''' Update FFmat'''
        for jff in mod.components['JFF']:
            if jff[2] > 0:
                ''' Convert to integer and adjust for zero-based indexing'''
                row_index = int(jff[0]) - 1
                col_index = int(jff[1]) - 1
                x_col_index = int(jff[2]) - 1

                ''' Check if indices are within the bounds of the matrices'''
                if 0 <= row_index < FFmat.shape[0] and \
                    0 <= col_index < FFmat.shape[1] and \
                    0 <= x_col_index < mod.components['X'].shape[1]:
                    FFmat[row_index, col_index] = mod.components['X'][tt, x_col_index]

        ''' Update FFa'''
        FFa[:, :, tt] = FFmat
    mod.components['FFa'] = FFa
    return mod

def setup_random_walk_dlm(subset_data, NIndep, WVarFrac, MovingAvgLength, dat, dim_k, mod_list):
    """
    Sets up a random walk dynamic regression DLM based on linear regression 
    estimates. The random walk DLM is the last element of mod_list.

    Arguments:
    subset_data (DataFrame): The subset of data used for setting up the DLM.
    NIndep (int): Number of independent predictors.
    WVarFrac (float): Weight variance fraction used in the DLM.
    MovingAvgLength (int): Length of the rolling average.
    dat (DataFrame): The original dataset used for predictions.
    dim_k (int): Dimension parameter for the model list.
    mod_list (list): List to store the model.

    Returns:
    mod_list (list): Updated list with the DLM model added as the last element.
    """

    """
    Sets up a random walk dynamic regression DLM based on linear regression
    estimates. The random walk dlm is the last element of mod_list.
    """
    X = subset_data[['O', 'M', 'D', 'I']]
    Y = subset_data[['F', 'H']]

    ''' Linear regression to fit the model '''
    joint_lm = LinearRegression(fit_intercept=False)
    joint_lm.fit(X, Y)

    ''' Create the FF matrix '''
    FF = np.zeros((2, NIndep * 2))

    ''' Create the JFF matrix with 2 rows and NIndep*2 columns '''
    row1 = np.concatenate((np.arange(1, NIndep + 1), np.zeros(NIndep)))
    row2 = np.concatenate((np.zeros(NIndep), np.arange(1, NIndep + 1)))
    JFF = np.vstack((row1, row2))

    ''' Calculate the residuals and the covariance matrix of the residuals '''
    residuals = Y - joint_lm.predict(X)
    residuals_crossprod = np.dot(residuals.T, residuals)

    ''' Retrieve the degrees of freedom, which is the number of observations
    minus the number of predictors. Because the model is fit without an intercept,
    we subtract the number of predictors directly. '''
    degrees_of_freedom = X.shape[0] - X.shape[1]

    ''' Calculate V, which is the covariance matrix of the residuals 
    divided by the degrees of freedom. '''
    V = residuals_crossprod / degrees_of_freedom

    ''' Create GG matrix, corresponding to Gt. '''
    GG = np.eye(NIndep * 2)

    ''' Calculate RSS for columns F and H (ethane and propane prices)'''
    RSS_F = np.sum(residuals['F'] ** 2)
    RSS_H = np.sum(residuals['H'] ** 2)

    ''' Degrees of freedom for each model (assuming no intercept) '''
    df_F = X.shape[0] - X.shape[1]  
    df_H = X.shape[0] - X.shape[1]  

    ''' Mean squared error (MSE) for each model '''
    MSE_F = RSS_F / df_F
    MSE_H = RSS_H / df_H

    ''' Calculate covariance matrix for the coefficients for each model '''
    XtX_inv = inv(X.T @ X)
    cov_matrix_F = XtX_inv * MSE_F
    cov_matrix_H = XtX_inv * MSE_H
    n, k = X.shape

    ''' Calculate the residuals' covariance matrix with degrees of freedom correction '''
    residual_cov_matrix_dof_corrected = np.cov(residuals, rowvar=False, ddof=k)
    ''' Compute (X^TX)^(-1) '''
    xtx_inv = np.linalg.inv(X.T @ X)

    ''' Use the Kronecker product to compute the full 
    variance-covariance matrix in block form '''
    full_var_cov_matrix = np.kron(residual_cov_matrix_dof_corrected, xtx_inv)

    ''' Extract the joint covariance matrix for the coefficients between F and H
    the response variables are in the order [F, H]
    and the coefficients are in the order [F1, F2, F3, F4, H1, H2, H3, H4] '''
    joint_cov_matrix_F_H = full_var_cov_matrix[:4, 4:8]

    ''' Construct the full block covariance matrix
    - The top-left block is the covariance matrix for F
    - The bottom-right block is the covariance matrix for H
    - The top-right and bottom-left blocks are the joint 
    covariance matrices (which are transposes of each other) '''
    full_block_cov_matrix = np.block([
        [cov_matrix_F, joint_cov_matrix_F_H],
        [joint_cov_matrix_F_H.T, cov_matrix_H]
    ])

    ''' Variables are prepared for the dlm '''
    W = np.diag(np.diag(full_block_cov_matrix) * WVarFrac)
    
    ''' Convert the coefficient array into a 1D vector '''
    m0 = joint_lm.coef_.flatten()
    C0 = full_block_cov_matrix
    X = dat.loc[MovingAvgLength:, ["O", "M", "D", "I"]]
   
    ''' Reshapes m0 to have MovingAvgLength rows and 1 column '''
    m0 = np.array(m0).reshape(-1, 1) 
    
    ''' The dlm is run.'''
    mod_list[dim_k-1] = dlm(FF=FF, JFF=JFF, V=V, GG=GG, W=W, m0=m0, C0=C0, X=X)

    return mod_list

def read_input_data(Param, RunName):
    """
    Reads input data from a CSV file based on parameters and run name.

    This function reads data from a specified CSV file, using parameters 
    from a DataFrame and a run name.  The data is read twice: first, the 
    entire dataset is read, and then a subset of the dataset with fewer columns.
    The first dataset is read with 17 columns (labeled 'A' to 'Q'), 
    and the second dataset is read with 15 columns (labeled 'A' to 'O'). 
    The second dataset is specifically used for fitting a new propane model.

    Arguments:
    Param (pandas.DataFrame): A DataFrame containing parameters, including directory and file name information.
    RunName (str): A string representing the name of the run, used in forming the file name.

    Returns:
    tuple of pandas.DataFrame: A tuple containing two DataFrames.
        The first DataFrame (`dat`) contains the full dataset with 17 columns,
        and the second DataFrame (`dat_1`) contains a subset with just 15 columns.
    """
    directory = str(Param.loc['Directory', 'Value'])
    input_name_extension = str(Param.loc['InputNameExtension', 'Value'])

    if directory and directory.lower() != 'nan':
        InputFile = f"{directory}{RunName}{input_name_extension}.csv"
    else:
        InputFile = f"{RunName}{input_name_extension}.csv"

    dat = pd.read_csv(InputFile, header=None, skiprows=1, sep=",", dtype=float, usecols=range(17))
    dat.columns = list('ABCDEFGHIJKLMNOPQ')

    dat_1 = pd.read_csv(InputFile, header=None, skiprows=1, sep=",", dtype=float, usecols=range(15))
    dat_1.columns = list('ABCDEFGHIJKLMNO')

    return dat, dat_1

def setup_expert_based_dlms(dat, MovingAvgLength, dim_k, mod_list, NIndep, delta_ethane, delta_propane):
    """
    Set up the expert-based DLMs using temporary dataframes to format and calculate relative variances.

    Arguments:
    dat (DataFrame): Input data used for model setup.
    MovingAvgLength (int): Length of the rolling average window.
    dim_k (int): Dimension parameter for the model list.
    mod_list (list): List containing models, to be updated with new component models.
    NIndep (int): Number of independent variables.
    delta_ethane (DataFrame): DataFrame containing delta values for ethane.
    delta_propane (DataFrame): DataFrame containing delta values for propane.

    Returns:
    mod_list (list): Updated list with expert-based DLMs added.
    """
    
    ''' Establish rolling average values for Brent and Henry Hub prices 
    (columns O and M).'''
    running_ave = dat[['O', 'M']].rolling(window=MovingAvgLength).mean()
    tmp = running_ave.iloc[1:].values / running_ave.iloc[:-1].values
    tmp_df = pd.DataFrame(tmp)
    tmp_df = pd.concat( [ pd.DataFrame([np.nan, np.nan]).T, tmp_df ], ignore_index=True)
    relative_var = tmp_df.iloc[ MovingAvgLength:, 0] /\
        tmp_df.iloc[ MovingAvgLength:, 1]
    del tmp, tmp_df

    ''' Iterate across the component models to create a combined matrix.'''
    for k in range(dim_k - 1):
        tmp_mod = copy.deepcopy( mod_list[dim_k - 1] )
        tmp_mod.components['JFF'] = np.zeros((2, 8))
        
        ''' Construct the indices as specified in the R code
        and adjust for 0-based indexing in Python.'''
        indices = np.array([
            [1, 1],
            [1, 2],
            [2, 1 + NIndep],
            [2, 2 + NIndep]
            ]) - 1

        ''' Fill in the specified values'''
        values = np.array([1, 2, 1, 2])
        for idx, val in zip( indices, values ):
            tmp_mod.components['JFF'][idx[0], idx[1]] = val
        tmp_mod.components['JGG'] = \
            np.zeros( ( tmp_mod.components['GG'].shape[1], \
            tmp_mod.components['GG'].shape[1] ) )

        ''' Create the index matrix 
        and adjust for 0-based indexing in Python.'''
        index_matrix = np.array([
            [1, 1],
            [2, 1],
            [1 + NIndep, 1 + NIndep],
            [2 + NIndep, 1 + NIndep]
            ]) - 1

        ''' Assign values 3, 4, 5, 6 to the specified indices in the JGG matrix'''
        tmp_mod.components['JGG'][index_matrix[:, 0], index_matrix[:, 1]] = np.arange(3, 7)

        ''' Calculate new columns based on the power function operation, 
        where k is the index in delta_ethane/delta_propane.'''
        ethane_column =  np.power( relative_var.values, delta_ethane.iloc[:, k].values )
        ethane_complement = 1 - ethane_column
        propane_column = np.power( relative_var.values, delta_propane.iloc[:, k].values ) 
        propane_complement = 1 - propane_column

        ''' Stack the NumPy arrays horizontally to create a combined matrix.'''
        combined_matrix = np.hstack((
            tmp_mod.components['X'][:, 0].reshape(-1, 1),
            tmp_mod.components['X'][:, 1].reshape(-1, 1),
            ethane_column.reshape(-1, 1),
            ethane_complement.reshape(-1, 1),
            propane_column.reshape(-1, 1),
            propane_complement.reshape(-1, 1)
        ))
        tmp_mod.components['X'] = combined_matrix
        mod_list[k] = tmp_mod

    return mod_list

def compute_forecast_weights(forecast_err, dim_k, aa, BurnIn, phi, eta_bar_df, NumberNGLs, y, modKF, mod_list):
    """
    Compute the model forecasting weights based on one-step forecast errors.
    Compute the adjustment term in the updating of the logit weights ('eta').
    The function performs various operations to adjust forecast errors, 
    compute weights, and set up time-varying matrices for forecasting.
    """

    ''' Select specific slice from forecast_err for subtraction'''
    stats = forecast_err[..., dim_k-1]

    ''' Subtract stats from forecast_err across the specified dimensions
    and omitting the dim_k slice (achieved by np.newaxis)'''
    adjusted_forecast_err = forecast_err - stats[..., np.newaxis]

    ''' Exclude the dim_k slice, weight the adjustment, and 
    initialize eta0 with the first slice of wgt_adjust.'''
    adjusted_forecast_err = np.delete(adjusted_forecast_err, dim_k-1, axis=-1)
    wgt_adjust = -aa * adjusted_forecast_err 
    eta0 = wgt_adjust[0, :, :]

    ''' Apply the adjustments to eta0 based on the user-specified
    values for BurnIn iterations and the autoregressive coefficient phi.
    Adjust for zero-based indexing.'''
    tt = 2
    while tt <= BurnIn:
        eta0 = phi * eta0 + wgt_adjust[tt - 1, :, :]
        tt += 1

    ''' For each Omega (projected year sequence), compute the logit 
    weights (eta) for all lead times.'''
    nrow_y = len(y)   
    ''' First populate the initial etas.'''
    eta = np.zeros((nrow_y - BurnIn, nrow_y - BurnIn, NumberNGLs, dim_k - 1))
    eta[0, 0, :, :] = eta0
    ''' Loop to compute eta values.'''
    for ll in range(1, eta.shape[1]):
        eta[0, ll, :, :] = phi * (eta[0, ll - 1, :, :] - eta_bar_df) + eta_bar_df
    for tt in range(1, eta.shape[0]):
        ''' One-step-ahead logit weights first.'''
        eta[tt, 0, :, :] = phi * (eta[tt - 1, 0, :, :] - eta_bar_df) \
            + eta_bar_df + wgt_adjust[tt, :, :]
        ''' Then compute all the remaining logit weights.'''
        for ll in range(1, eta.shape[1]):
            eta[tt, ll, :, :] = phi * (eta[tt, ll - 1, :, :] - eta_bar_df) + eta_bar_df

    ''' Set the initial weights across the fourth dimension.'''
    tmp = np.concatenate([np.exp(eta), np.ones((eta.shape[0], eta.shape[1], eta.shape[2], 1))], axis=3)
    ''' Calculate the sum across the fourth dimension for 
    each combination of the first three dimensions.'''
    sums = np.sum(tmp, axis=3)
    
    ''' Initialize an array wgt to hold the result. Iterate over the 
    fourth dimension to perform the division.'''
    wgt = np.empty_like(tmp)
    for i in range(tmp.shape[3]):
        wgt[:, :, :, i] = tmp[:, :, :, i] / sums

    ''' Generate the forecasts, calling on the setFG and setF functions.'''
    forecast0 = np.zeros_like(wgt)
    forecast = forecast0[:, :, :, 0]
    for j in range(dim_k - 1):
        mod_list[j] = setFG(mod_list[j])
        for Omega in range(BurnIn + 1, modKF[j]['m'].shape[0] - 1):
            a = modKF[j]['m'][Omega - 1, :].copy()
            for i in range(Omega, modKF[j]['f'].shape[0] + 1):
                a = a.reshape(-1, 1)
                a = np.dot(mod_list[j].components['GGa'][:, :, i - 1], a)
                forecast0[Omega - BurnIn - 1, i - Omega, :, j] = \
                    np.dot(mod_list[j].components['FFa'][:, :, i - 1], a).flatten()
    
    mod_list[dim_k - 1] = setF(mod_list[dim_k - 1])
    for Omega in range(BurnIn + 1, modKF[dim_k - 1]['m'].shape[0] - 1):
        a = modKF[dim_k - 1]['m'][Omega - 1, :].copy()
        for i in range(Omega, modKF[dim_k - 1]['f'].shape[0] + 1):
            forecast0[Omega - BurnIn - 1, i - Omega, :, dim_k - 1] = \
                np.dot(mod_list[dim_k - 1].components['FFa'][:, :, i - 1], a).flatten()

    forecast = np.sum(forecast0 * wgt, axis=3)
    
    ''' The variable y0 was included in the R code but never used.'''
    ''' 
    y0 = np.zeros_like(forecast)
    for ii in range(2):
        for iii in range(y0.shape[0]):
            ind = y.shape[0] - BurnIn - iii
            y0[iii, :ind, ii] = y[-ind:, ii]
    '''

    return forecast0, forecast, wgt

def create_pdf_plots(RunName, MovingAvgLength, delprop1, delprop2, delprop3, Efrac, 
                     ethaneForecast_ts, propaneForecast_ts, NewPropaneForecast_ts, Brent_ts,
                     lasthistory, NowComp1, NowComp2, NowComp3, NowComp4, LastHistoricalYear,
                     NewPropaneForecast_1_ts):
    """
    Generates PDF plots for ethane and propane price projections and components based on the provided data.

    Arguments:
    RunName (str): The name of the current run, used for naming the output PDF file.
    MovingAvgLength (int): The length of the moving average window used in calculations.
    delprop1, delprop2, delprop3, Efrac (float): Parameters used in the computation of the forecasts.
    ethaneForecast_ts, propaneForecast_ts, NewPropaneForecast_ts (Series): Time series data for ethane and propane forecasts.
    Brent_ts (Series): Time series data for Brent oil prices.
    NowComp1, NowComp2, NowComp3, NowComp4: forecasts for ethane (col 0) and propane (col 1).
    LastHistoricalYear: last year that is read directly and not modeled
    lasthistory: record associated with last historical year
    NewPropaneForecast_1_ts (Series): Adjusted time series data for new propane forecasts.

    This function will create and save a PDF file with plots if the 
    'Plotting' parameter is set to 'Yes' in the NGLPriceInputParameters.csv.
    The plots were copied from the original R code and include 
    various projections and components for ethane and 
    propane prices, as well as Brent oil prices.
    """
        
    ''' Create time series for ethane and propane.'''
    EthaneSeries1 = np.concatenate([lasthistory['Ethane'].values, NowComp1[:, 0]])
    EthaneSeries2 = np.concatenate([lasthistory['Ethane'].values, NowComp2[:, 0]])
    EthaneSeries3 = np.concatenate([lasthistory['Ethane'].values, NowComp3[:, 0]])
    EthaneSeries4 = np.concatenate([lasthistory['Ethane'].values, NowComp4[:, 0]])

    PropaneSeries1 = np.concatenate([lasthistory['Propane'].values, NowComp1[:, 1]])
    PropaneSeries2 = np.concatenate([lasthistory['Propane'].values, NowComp2[:, 1]])
    PropaneSeries3 = np.concatenate([lasthistory['Propane'].values, NowComp3[:, 1]])
    PropaneSeries4 = np.concatenate([lasthistory['Propane'].values, NowComp4[:, 1]])

    ethaneComp1_ts = pd.Series(EthaneSeries1, index=pd.date_range(start=str(LastHistoricalYear), periods=len(EthaneSeries1), freq='AS'))
    ethaneComp2_ts = pd.Series(EthaneSeries2, index=pd.date_range(start=str(LastHistoricalYear), periods=len(EthaneSeries2), freq='AS'))
    ethaneComp3_ts = pd.Series(EthaneSeries3, index=pd.date_range(start=str(LastHistoricalYear), periods=len(EthaneSeries3), freq='AS'))
    ethaneComp4_ts = pd.Series(EthaneSeries4, index=pd.date_range(start=str(LastHistoricalYear), periods=len(EthaneSeries4), freq='AS'))

    propaneComp1_ts = pd.Series(PropaneSeries1, index=pd.date_range(start=str(LastHistoricalYear), periods=len(PropaneSeries1), freq='AS'))
    propaneComp2_ts = pd.Series(PropaneSeries2, index=pd.date_range(start=str(LastHistoricalYear), periods=len(PropaneSeries2), freq='AS'))
    propaneComp3_ts = pd.Series(PropaneSeries3, index=pd.date_range(start=str(LastHistoricalYear), periods=len(PropaneSeries3), freq='AS'))
    propaneComp4_ts = pd.Series(PropaneSeries4, index=pd.date_range(start=str(LastHistoricalYear), periods=len(PropaneSeries4), freq='AS'))

    pdf_filename = f"{RunName}_{MovingAvgLength}_d_{delprop1}_{delprop2}_{delprop3}_e_{Efrac}2_Python.pdf"

    with PdfPages(pdf_filename) as pdf_pages:
            
        ''' Plotting Ethane prices'''
        plt.figure(figsize=(10, 6))
        plt.plot(ethaneForecast_ts, color='red', linestyle='-', label='Projected')
        plt.plot(ethaneComp1_ts, color='blue', linestyle='--', label='Comp. 1')
        plt.plot(ethaneComp2_ts, color='darkgreen', linestyle='--', label='Comp. 2')
        plt.plot(ethaneComp3_ts, color='brown', linestyle='--', label='Comp. 3')
        plt.plot(ethaneComp4_ts, color='purple', linestyle='--', label='Comp. 4')
        plt.title(f"Ethane Price Projections and Components, {RunName} Case")
        plt.xlabel("Year")
        plt.ylabel("2011 Cents per Gallon")
        plt.legend(loc='lower right')
        plt.grid(True)
        ''' Save plot to the pdf.'''
        pdf_pages.savefig()
        plt.close()

        ''' Plotting Propane prices'''
        plt.figure(figsize=(10, 6))
        plt.plot(propaneForecast_ts, color='red', linestyle='-', label='Projected')
        plt.plot(propaneComp1_ts, color='blue', linestyle='--', label='Comp. 1')
        plt.plot(propaneComp2_ts, color='darkgreen', linestyle='--', label='Comp. 2')
        plt.plot(propaneComp3_ts, color='brown', linestyle='--', label='Comp. 3')
        plt.plot(propaneComp4_ts, color='purple', linestyle='--', label='Comp. 4')
        plt.title(f"Propane Price Projections and Components, {RunName} Case")
        plt.xlabel("Year")
        plt.ylabel("2011 Cents per Gallon")
        plt.legend(loc='lower right')
        plt.grid(True)
        ''' Save plot to the pdf.'''
        pdf_pages.savefig()
        plt.close()

        plt.figure(figsize=(10, 6))
        ''' Plot NewPropaneForecast_ts'''
        plt.plot(NewPropaneForecast_ts, color='blue', label='Propane, No HH, cents per gallon')
        ''' Plot Brent_ts'''
        plt.plot(Brent_ts, color='brown', label='Brent, dollars per barrel')
        ''' Adding titles and labels'''
        plt.title("New Propane and Brent Price Projections, different units")
        plt.ylabel("2011 Cents per Gallon or Dollars per Barrel")
        plt.xlabel("Year")
        ''' Adjusting x-axis to show years '''
        plt.gca().xaxis.set_major_locator(plt.MaxNLocator(integer=True))
        ''' Adding legend'''
        plt.legend(loc='lower right')
        ''' Save plot to the pdf.'''
        pdf_pages.savefig()
        plt.close()

        ''' Create the first plot (Ethane Price Projections)'''
        plt.figure(figsize=(10, 6))
        plt.plot(ethaneForecast_ts, color='red', linestyle='-')
        plt.title(f"Ethane Price Projections, {RunName} Case")
        plt.ylabel("2011 Cents per Gallon")
        plt.xlabel("Year")
        plt.legend(["Projected"], loc='lower right')
        ''' Save plot to the pdf.'''
        pdf_pages.savefig()
        plt.close()

        ''' Create the second plot (New Propane and Brent Price Projections)'''
        plt.figure(figsize=(10, 6))
        plt.plot(NewPropaneForecast_1_ts, color='blue', label='Propane, No HH, cents per gallon')
        plt.plot(Brent_ts, color='brown', label='Brent, dollars per barrel')
        plt.title("New Propane and Brent Price Projections, different units")
        plt.ylabel("2011 Cents per Gallon or Dollars per Barrel")
        plt.xlabel("Year")
        plt.legend(loc='lower right')
        ''' Save plot to the pdf.'''
        pdf_pages.savefig()
        plt.close()

        print("PDF plots saved.")
        
    return

def create_propane_consumption_trend(dat, NoExcessEthaneYear, StartYear):
    """
    Creates a subset of data focusing on the period before the disappearance of excess ethane and 
    determines the propane consumption trend within this period using linear regression.

    Arguments:
    dat (DataFrame): The complete dataset containing various yearly data points.
    NoExcessEthaneYear (int): The year after which excess ethane is no longer available, 
                              used to filter the dataset.
    StartYear (int): The starting year for the analysis, used to further filter the dataset.

    Returns:
    LinearRegression Model: A linear regression model object representing the trend in propane 
                            consumption over the specified years. This model can be used to 
                            predict future trends based on historical data.
    """

    ''' Call the data from Year (A), Ethane Consumption (P), and 
    Propane Consumption (Q) columns in the model '''
    indcons = dat[['A', 'P', 'Q']].copy()
    indcons.columns = list('ABC')
    indcons = indcons[indcons['A'] <= NoExcessEthaneYear]
    indcons = indcons[indcons['A'] >= StartYear]
    
    ''' Determine the propane consumption trend by ordinary least squares.'''
    propane_trend = smf.ols('C ~ A', data=indcons).fit()
    coeff_1 = propane_trend.params['Intercept']
    coeff_2 = propane_trend.params['A']
    
    ''' Add a new column with propane consumption
    calculations based on the retrieved coefficients.'''
    dat['new_column'] = dat.iloc[:, 16] - (coeff_1 + coeff_2 * dat.iloc[:, 0])
    
    ''' Make conditional adjustments based on excess ethane availability.'''
    dat['new_column'] = np.where( dat.iloc[:, 0] < NoExcessEthaneYear, 0, dat['new_column'] )
    dat['new_column'] = np.where( dat['new_column'] < 0, 0, dat['new_column'] )
    
    ''' Add an adjusted column with calculations. This column did not appear to
    be used in the R code.'''
    dat['adj_column'] = dat['new_column'] / (dat['new_column'] + dat.iloc[:, 15])


def prep_propane_model(train_data, newprojdata_1, Param, LastHistoricalYear, PFloor, dat):
    """
    Prepares and fits a linear regression model for propane pricing using 
    specified training data. This function also computes propane price 
    projections for future years.

    Arguments:
    train_data (DataFrame): The training data set used for fitting the linear regression model.
    newprojdata_1 (DataFrame): A modified version of the primary dataset, used for projecting future prices.
    Param (DataFrame): A DataFrame containing the user-specified reduction in the Propane Production coefficient.
    LastHistoricalYear (int): The last year of historical data used in the training set.
    PFloor (float): The minimum threshold value for propane prices used in projections.
    dat (DataFrame): The original dataset, used for additional data manipulation and processing.

    Returns:
    projonly_1 (DataFrame): projected propane prices for future years
    NewPropForecast_0 (DataFrame): projected propane prices for future years, along with other relevant data.
    lasthistory (Series): ethane prices, propane prices, and Brent prices associated only with the last historical year
    """
    
    ''' Prepare the features (independent variables) and target 
    (dependent variable).'''
    X = train_data[['Brent', 'PropProduction', 'OrgDemand']]
    Y = train_data['PropPrice']

    ''' Fit a linear regression model without an intercept.'''
    prop_lm_1 = LinearRegression(fit_intercept=False)
    prop_lm_1.fit(X, Y)

    ''' Reduce the effect of the PropProduction coefficient.'''
    prop_lm_1.coef_[1] *= float(Param.loc['ReductionInPropProdCoef', 'Value'])

    ''' Isolate the projection years.'''
    projonly_1 = newprojdata_1[newprojdata_1['Year'] > LastHistoricalYear].copy()

    ''' Compute propane price projections from covariates.'''
    projonly_1['PropProject'] = (projonly_1['Brent'] * prop_lm_1.coef_[0] +
        projonly_1['PropProduction'] * prop_lm_1.coef_[1] +
        projonly_1['OrgDemand'] * prop_lm_1.coef_[2] - int(Param.loc['AdjToPropPrice', 'Value']))

    ''' Compute the constant to raise the price projections.'''
    PConstant = max(PFloor - projonly_1['PropProject'].iloc[0], 0)
    projonly_1['PropProject'] = projonly_1['PropProject'] + PConstant

    ''' Make adjustments for the last historical year.'''
    indata = dat.copy()
    lasthistory = indata[indata['A'] == LastHistoricalYear]
    lasthistory = lasthistory[['F', 'H', 'O']].copy()
    lasthistory.rename(columns={'F': 'Ethane2', 'H': 'Propane2', 'O': 'Brent2'}, inplace=True)
    lasthistory['Year'] = LastHistoricalYear

    ''' Combine the last historical value of Propane2 with the PropProject values.'''
    NewPropForecast_0 = pd.concat([lasthistory['Propane2'], projonly_1['PropProject']])

    return NewPropForecast_0, projonly_1, lasthistory

def fit_new_propane_model(dat, dat_1, Param, LastHistoricalYear, PFloor, FirstModelYear):
    """
    Fits a new propane model based on given datasets and parameters. 
    It involves data preparation, linear regression modeling, 
    and time series creation for propane price projections.

    Arguments:
    dat (DataFrame): The primary dataset containing historical and other relevant data.
    dat_1 (DataFrame): A modified version of the primary dataset with specific columns for the model.
    Param (DataFrame): User-specified parameters and settings.
    LastHistoricalYear (int): The last year of historical data.
    PFloor (float): User-specified parameter representing the minimum price for any commodity.
    FirstModelYear (int): First year from which the model projections start.

    Returns:
    Tuple: Contains three elements - 
           1. NewPropaneForecast_ts (Series): A time series of the new propane forecast.
           2. NewPropaneForecast_1_ts (Series): Propane price forecast from linear regression of covariates.
           3. Brent_ts (Series): Brent oil prices corresponding to the propane forecasts.
    """

    ''' Rename specific columns and filter the data to develop a training set.'''
    newprojdata_1 = dat_1[['A', 'O', 'D', 'I', 'H']].copy()
    newprojdata_1.rename(columns={'A': 'Year', 'O': 'Brent', 'D': 'PropProduction', 
                                  'I': 'OrgDemand', 'H': 'PropPrice'}, inplace=True)
    train_data = newprojdata_1[ newprojdata_1['Year'] <= LastHistoricalYear ]

    ''' The prep_propane_model function assembles the arrays
    into records that can be fed directly to the model.'''
    NewPropForecast_0, projonly_1, lasthistory = prep_propane_model( train_data, \
        newprojdata_1, Param, LastHistoricalYear, PFloor, dat )

    ''' Create a time series for plotting
    Generate a date range starting from (FirstModelYear - 1) '''
    date_range = pd.date_range(start=str(FirstModelYear - 1), periods=len(NewPropForecast_0), freq='AS')
    NewPropaneForecast_1_ts = pd.Series( NewPropForecast_0.values, index=date_range )

    ''' Next train the model with the price variables in log scale,
    resulting in the time series NewPropaneForecast_ts.'''

    ''' Select specific columns and rename them.'''
    projdata = dat.copy()
    newprojdata = projdata[['A', 'O', 'D', 'I', 'H']].copy()
    newprojdata.rename(columns={'A': 'Year', 'O': 'Brent', 'D': 'PropProduction', 
                            'I': 'OrgDemand', 'H': 'PropPrice'}, inplace=True)
    ''' Filter the data to include only rows before modeling starts for training'''
    train_data = newprojdata[newprojdata['Year'] <= LastHistoricalYear]
    ''' Prepare the features (independent variables) and target 
    (dependent variable).'''
    X_train = train_data[[ 'Brent', 'PropProduction', 'OrgDemand' ]]
    y_train = train_data['PropPrice']
    ''' run the 3-covariate model for propane. '''
    prop_lm = LinearRegression(fit_intercept=False)
    prop_lm.fit(X_train, y_train)
    ''' Isolate the projection years for predictions.'''
    projonly = newprojdata[ newprojdata['Year'] > LastHistoricalYear ].copy()
    ''' Compute propane projections from covariates.'''
    projonly['PropProject'] = (projonly['Brent'] * prop_lm.coef_[0] +
                           projonly['PropProduction'] * prop_lm.coef_[1] +
                           projonly['OrgDemand'] * prop_lm.coef_[2])
    
    ''' Convert from log scale to linear scale.'''
    projonly['PropProject'] = np.exp(projonly['PropProject']).copy()
    ''' Compute a constant to raise the projections to the PFloor constant.'''
    PConstant = max(PFloor - projonly['PropProject'].iloc[0], 0)
    ''' Add PConstant to the PropProject column.'''
    projonly['PropProject'] = projonly['PropProject'].copy() + PConstant
    date_range_propane = pd.date_range(start=str(FirstModelYear), 
                                       periods=len(projonly), freq='AS')
    NewPropaneForecast_ts = pd.Series(projonly['PropProject'].values, 
                                      index=date_range_propane)
    NewBrentForecast_0 = pd.concat([lasthistory['Brent2'][1:], 
                                    projonly_1['Brent']]) 
    ''' Create a time series for Brent prices.'''
    date_range_brent = pd.date_range(start=str(FirstModelYear - 1), 
                                     periods=len(NewBrentForecast_0), freq='AS')
    Brent_ts = pd.Series(NewBrentForecast_0.values, index=date_range_brent)

    return  NewPropaneForecast_ts, NewPropaneForecast_1_ts, Brent_ts

def pickle_and_save_results(RunName, forecast, wgt):
    """
    Pickle and save results.
    Saves the 'allForecast' and 'allWeight' data to a pickle file.

    Arguments:
    RunName (str): The name of the current run, used in naming the output file.
    forecast (DataFrame): The forecast data to be saved.
    wgt (DataFrame): The weight data to be saved.
    NowForecast_df (DataFrame): DataFrame containing the current forecast data.
    """
    
    allForecast = forecast.copy()
    allWeight = wgt.copy()

    file_name = f"{RunName}.pkl"
    with open(file_name, 'wb') as file:
        pickle.dump({'allForecast': allForecast, 'allWeight': allWeight}, file)


''' The main definition makes calls to the definitions.'''
def main(cycle_number):

    print('Hello from NGPLprice.py!')
    print(os.getcwd())
    # print('ARG for Cycle is: ' +argv[1])
    print('ARG for Cycle is: ' +cycle_number)
    currentdir = os.getcwd()

    # os.chdir('./ngpl')

    """
    delete any inputs that are unused
    """
    del currentdir

    # in_file_name = 'NGPL_Price'+ argv[1].zfill(2) + "_RInputData.csv"
    in_file_name = 'NGPL_Price'+ cycle_number.zfill(2) + "_RInputData.csv"

    try:
        shutil.copyfile(in_file_name, './ngpl/' + in_file_name)

    # If source and destination are same
    except shutil.SameFileError:
        print("Source and destination represents the same file.")
 
    # If destination is a directory.
    except IsADirectoryError:
        print("Destination is a directory.")
 
    # If there is any permission issue
    except PermissionError:
        print("Permission denied.")
 
    # For other errors
    except:
        print('Cannot find '+ in_file_name + '! Quitting now')
        return
    
    ''' The first argument dictates the Run Name.'''
    try:
        # RunName = argv[1].zfill(2)
        RunName = cycle_number.zfill(2)
    except IndexError:
        RunName = ''
    
    ''' SET UP BASE INPUT/OUTPUT PARAMETERS.'''
    
    ''' Read all of the input parameters from a file, including
    the file names and directories.'''

    os.chdir('./ngpl')

    Param = pd.read_csv('NGLPriceInputParameters.csv', sep = ',', \
        header = 0, index_col = 0, na_values = ['-'], comment='#')
        
    ''' Translate the numeric input parameters to shorter variables
    for convenience and readability.'''
    LastHistoricalYear = int(Param.loc['LastHistoricalYear', 'Value'])
    NoExcessEthaneYear = int(Param.loc['NoExcessEthaneYear', 'Value'])
    FinalYear = int(Param.loc['FinalYear', 'Value'])
    PFloor = int(Param.loc['PFloor', 'Value']) 
    delprop1 = float(Param.loc['delprop1', 'Value'])
    delprop2 = float(Param.loc['delprop2', 'Value'])
    delprop3 = float(Param.loc['delprop3', 'Value'])
    MovingAvgLength = int(Param.loc['MovingAvgLength', 'Value'])
    Efrac = float(Param.loc['Efrac', 'Value'])
    NIndep = int(Param.loc['NIndep', 'Value'])
    dim_k = int(Param.loc['dim_k', 'Value'])
    aa = float(Param.loc['aa', 'Value'])
    phi = float(Param.loc['phi', 'Value'])
    BurnIn = int(Param.loc['BurnIn', 'Value'])
    WVarFrac = float(Param.loc['WVarFrac', 'Value'])
    NumberNGLs = int(Param.loc['NumberNGLs', 'Value'])

    StartPlotYear = LastHistoricalYear - MovingAvgLength
        
    ''' Assemble a long-term forecast Weights dataframe 
    from the input parameters. Then the resulting long-term log odds.'''
    wgt_bar_df = pd.DataFrame({
        0: [float(Param.loc['wgt_bar0', 'Value']), float(Param.loc['wgt_bar0', 'Value'])],
        1: [float(Param.loc['wgt_bar1', 'Value']), float(Param.loc['wgt_bar1', 'Value'])],
        2: [float(Param.loc['wgt_bar2', 'Value']), float(Param.loc['wgt_bar2', 'Value'])],
        3: [float(Param.loc['wgt_bar3', 'Value']), float(Param.loc['wgt_bar3', 'Value'])]
    })

    eta_bar_df = pd.DataFrame(np.log(wgt_bar_df.iloc[:, :-1].div(wgt_bar_df.iloc[:, -1], axis=0)))
    
    ''' Assemble dataframes to represent the expectation of propane and
    ethane prices to depend on relative changes in Brent/HH prices.'''
    delprop1_series = pd.Series( [delprop1] )
    delprop2_series = pd.Series( [delprop2] )
    delprop3_series = pd.Series( [delprop3] )
    delta_propane = pd.concat( [delprop1_series, delprop2_series, delprop3_series], axis=1)
    delta_ethane = delta_propane * Efrac

    ''' List variables are created to hold the output.'''
    mod_list = [None] * dim_k
    ''' Included in R code but unused.
    nowForecast = [None] * 1
    allForecast = [None] * 1
    allWeight = [None] * 1 '''

    ''' Read the input csv file directly and establish StartYear
    for training the model from that.'''
    dat, dat_1 = read_input_data(Param, RunName)
    StartYear = int(dat.iloc[0,0] + MovingAvgLength - 1)
    
    ''' Fit the historical data for ethane and propane prices 
    (columns F/H/O/M = ethane/propane/Brent/Henry Hub prices and
     column A is year). Store the logarithmic values.'''
    dat[ ['F', 'H', 'O', 'M'] ] = np.log( dat[ ['F', 'H', 'O', 'M'] ] )
    subset_data = dat[ dat['A'] <= LastHistoricalYear ]

    ''' create and save dlms to mod_list '''
    setup_random_walk_dlm( subset_data, NIndep, WVarFrac, MovingAvgLength, 
                          dat, dim_k, mod_list )
    setup_expert_based_dlms( dat, MovingAvgLength, dim_k, mod_list, NIndep, 
                            delta_ethane, delta_propane )

    ''' Start filtering DLM's
         Run Kalman filter on each model, saving one-step forecast
         errors y contains the E and P prices with the first M 
         rows removed and with the first row number equal to 1 
         (rather than 1+M). With these matrix definitions,
         GG_t * theta_{t-1} = theta_t 
         
         First create a numpy array.'''
    y = dat[["F", "H"]].iloc[MovingAvgLength - 1: -1].to_numpy()
    nrow = dat.shape[0]
    forecast_err = np.zeros( (nrow - MovingAvgLength, 2, dim_k) )
    modKF = [None] * dim_k
    for j in range(dim_k):
        modKF[j] = mod_list[j].filter(y)
        forecast_err[:, :, j] = np.abs(y - modKF[j]['f'])

    ''' Compute the appropriate forecast weights.'''
    forecast0, forecast, wgt = compute_forecast_weights(forecast_err, \
        dim_k, aa, BurnIn, phi, eta_bar_df, NumberNGLs, y, modKF, mod_list)

    ''' Set parameters that define the different model regimes (training
    versus projections) and lengths of time.'''
    FirstModelYear = LastHistoricalYear + 1 
    StartRow = LastHistoricalYear - (StartYear + 2) 
    TotalYears = FinalYear - StartYear
    TotalOutputYears = FinalYear - LastHistoricalYear

    ''' Populate the matrices with the exponential values.'''
    Forecast = np.exp(forecast)
    Forecast = Forecast[ (StartRow - 1) : TotalYears, 0 : (TotalOutputYears) ] 
    forecast0_1 = forecast0[ (StartRow - 1) : TotalYears, 0 : (TotalOutputYears), :, :] 
    ''' Starting from the second element.'''
    NowComp1 = np.exp( forecast0_1[0, :, :, 0] )
    NowComp2 = np.exp( forecast0_1[0, :, :, 1] )
    NowComp3 = np.exp( forecast0_1[0, :, :, 2] )
    NowComp4 = np.exp( forecast0_1[0, :, :, 3] )

    ''' Prepare the input data.'''
    indata = pd.DataFrame(dat)

    ''' Filter rows where 'A' equals LastHistoricalYear.'''
    lasthistory = indata[indata['A'] == LastHistoricalYear]
    lasthistory = lasthistory[['A', 'F', 'H']]
    lasthistory['Year'] = lasthistory['A']
    lasthistory['Ethane'] = np.exp(lasthistory['F'])
    lasthistory['Propane'] = np.exp(lasthistory['H'])

    ''' Make adjustments for when excess ethane will no longer be 
    available.'''
    NowForecast = Forecast[0, :, :]
    ''' Concatenate last history values with the projected values.'''
    EthaneProject = np.concatenate([lasthistory['Ethane'].values, NowForecast[:, 0]])
    PropaneProject = np.concatenate([lasthistory['Propane'].values, NowForecast[:, 1]])

    ''' Determine the propane consumption trends, accounting for behavior 
    when excess ethane will no longer be available. '''
    create_propane_consumption_trend( dat, NoExcessEthaneYear, StartYear )

    ''' Subset the data.'''
    dat2 = dat[ dat.iloc[:, 0] >= LastHistoricalYear ]
    adj_matrix = dat2[ dat2.iloc[:, 0] <= FinalYear ]

    ''' Column 18 carries the "new_column" designation from 
    create_propane_consumption_trend to represent 
    adjusted propane consumption trends.''' 
    EthaneProject = adj_matrix.iloc[:, 18] * PropaneProject + \
        (1 - adj_matrix.iloc[:, 18]) * EthaneProject
    EthaneProject = EthaneProject.to_numpy()

    ''' When EthaneProject is valid, create the time series.'''
    ethaneForecast_ts = pd.Series( EthaneProject, \
        index=pd.date_range(start=str(LastHistoricalYear), \
                            periods=len(PropaneProject), freq='AS'))
    propaneForecast_ts = pd.Series( PropaneProject, \
        index=pd.date_range(start=str(LastHistoricalYear), \
                            periods=len(PropaneProject), freq='AS'))

    ''' Fit and generate new propane model forecasts and time series '''
    NewPropaneForecast_ts, NewPropaneForecast_1_ts, Brent_ts = \
        fit_new_propane_model(dat, dat_1, Param, LastHistoricalYear, PFloor, FirstModelYear)

    ''' Convert NowForecast to a DataFrame with row and column names.'''
    years = np.arange( LastHistoricalYear + 1, FinalYear + 1 )
    NowForecast_df = pd.DataFrame(NowForecast, index=years, columns=['Ethane', 'Propane'])

    ''' Replace historical data.'''
    indata = dat.copy()
    history = indata[indata['A'] <= LastHistoricalYear]
    history = history[['A', 'F', 'H']]
    history.rename(columns={'A': 'Year', 'F': 'Ethane', 'H': 'Propane'}, inplace=True)
    history = history[history['Year'] >= StartPlotYear]
    history['Ethane'] = np.exp(history['Ethane'])
    history['Propane'] = np.exp(history['Propane'])

    ''' Pickle and save the results.'''
    pickle_and_save_results(RunName, forecast, wgt)

    ''' Combine historical and projected values.'''   
    nowForecast_df = pd.DataFrame(NowForecast_df)
    nowForecast_df['Year'] = range(FirstModelYear, FinalYear + 1)
    newForecast = pd.concat( [ history, nowForecast_df ], ignore_index=True )
    newForecast.drop( 'Year', axis=1, inplace=True )
    dat2 = dat[ dat.iloc[:, 0] >= StartPlotYear ]
    adj_matrix = dat2[ dat2.iloc[:, 0] <= FinalYear ]
    newForecast['Ethane'] = adj_matrix.iloc[:, 18].values * \
        newForecast['Propane'] + \
        ( 1 - adj_matrix.iloc[:, 18].values ) * newForecast['Ethane']
    newForecast.index = pd.Index( range( StartPlotYear, FinalYear + 1 ) )
    newForecast = newForecast[1:]
    
    ''' Write the MAPE, MPE, and nowForecast lists out to csv files.'''    
    print(newForecast)
    file_name = 'NGPL_Price'+ RunName + Param.loc[ 'OutputNameExtension', 'Value' ] + ".csv"
    newForecast.to_csv( file_name, index=True )
    
    print('Forecast files saved')

    ''' The traditional graphs created by the R code are generated 
    only if the user has specified to generate the plots.'''
    if str(Param.loc[ 'Plotting', 'Value' ]) == 'Yes':
        create_pdf_plots( RunName, MovingAvgLength, delprop1, delprop2, 
                    delprop3, Efrac, ethaneForecast_ts, propaneForecast_ts, 
                    NewPropaneForecast_ts, Brent_ts, lasthistory, 
                    NowComp1, NowComp2, NowComp3, NowComp4, LastHistoricalYear,
                    NewPropaneForecast_1_ts )
                    
    shutil.copyfile(file_name, '../' + file_name)
    time.sleep(2)

    # Change curent directory
    os.chdir("..")
# os.chdir("..")
    
if __name__ == '__main__':
    """
    This python code may only be launched from nems_flow.py
    print('Hello from NGPLprice.py!')
    print(os.getcwd())
    print('ARG for Cycle is: ' +argv[1])
    currentdir = os.getcwd()
    in_file_name = 'NGPL_Price'+ argv[1].zfill(2) + "_RInputData.csv"
    try:
        shutil.copyfile(in_file_name, './ngpl/' + in_file_name)
    except:
        print('Cannot find '+ in_file_name + '! Quitting now')
    print('first' + os.getcwd())
    os.chdir('./ngpl')
    print('second' + os.getcwd())
    main()
    """
    print("This python code may only be launched from nems_flow.py")