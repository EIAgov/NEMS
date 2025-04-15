# -*- coding: utf-8 -*-
"""
Created on Oct 25 2024

@author: Claire Su
"""
import os,sys
import numpy as np

''' *************************** Module Level Design Concepts ************************************************************
This module mnfactorx_calc.py is designed to provide a Python version source/utils_docvfacts.f90 DOCVFACTS SUBROUTINE functionality.
The file input/mnfactorx_calc_api_list.txt is also used to support this module functionality:
    api_file='./input/mnfactorx_calc_api_list.txt'

When you add or remove a matched case in calc_api_main_menu() swtich case statement, do make sure you add or remove the API in the above api_file as well.
'''
def calc_api_main_menu(pyfiler,df,api,years_after_mnfactorx):
    """the contral controller as entry point Python code for the functionality of utils_docvfacts.f90 docvfacts()
    NOTICE: When add a new matched case in the switch case statement, make sure you add the API in input/mnfactorx_calc_api_list.txt file as well

    Parameters
    ----------
    pyfiler : module
        pyfiler fortran module
    
    df : dataframe
        the mnfactorx df
    
    api : str
        the API variable string
    
    years_after_mnfactorx : list
        list of the prediction years

    Returns
    -------
    dataframe
        the mnfactorx df with re-calcuated api varible values
    """
    match api.upper():
        case 'CFCRDDOM':
            df=calc_CFCRDDOM(pyfiler,df,api,years_after_mnfactorx)
        case 'CFCRDEXP':
            df=calc_CFCRDEXP(pyfiler,df,api,years_after_mnfactorx)
        case 'CFCRDIMP':
            df=calc_CFCRDIMP(pyfiler,df,api,years_after_mnfactorx)
        case 'CFDSQT':
            df=calc_CFDSQT(pyfiler,df,api,years_after_mnfactorx)
        case 'CFEXPRD':
            df=calc_CFEXPRD(pyfiler,df,api,years_after_mnfactorx)
        case 'CFIMPRD':
            df=calc_CFIMPRD(pyfiler,df,api,years_after_mnfactorx)
        case 'CFTPQ':
            df=calc_CFTPQ(pyfiler,df,api,years_after_mnfactorx)            
    return df

def calc_CFTPQ(pyfiler,df,api,years_after_mnfactorx):
    """calculate the CFTPQ value, update pyfiler and return the updated mnfactorx df.

    Parameters
    ----------
    pyfiler : module
        pyfiler fortran module

    df : dataframe
        the mnfactorx df
    api : str
        the API variable string
    years_after_mnfactorx : list
        list of the prediction years

    Returns
    -------
    dataframe
        the updated mnfactorx df
    """
    # https://git.eia.gov/oea/NEMS/-/blob/main/source/main.f#L3152
    #!  now it is all clear for total petroleum:
    pu=pyfiler.utils
    mnumcr=pu.mnumcr

    for yr in years_after_mnfactorx:
        curiyr=yr-1990+1-1      #34=2024-1990+1-1    

        #df.loc[df['API']==api.upper(),yr] 
        t = (pu.qmgas[11-1,curiyr] + pu.qjftr[11-1,curiyr] +
                            pu.qdsas[11-1,curiyr] + pu.qlgas[11-1,curiyr] + pu.qpfin[11-1,curiyr] +
                            pu.qrlas[11-1,curiyr] + pu.qrhas[11-1,curiyr] +
            #! replacing other with the pieces - ind [lubricants-1, pentanes plus] and tran [av gas-1, lubricants]
                            pu.qppin[mnumcr-1,curiyr] + pu.qppinpf[mnumcr-1,curiyr] +
                            pu.qluin[mnumcr-1,curiyr] + pu.qotin[mnumcr-1,curiyr] +
                            pu.qagtr[mnumcr-1,curiyr] + pu.qlutr[mnumcr-1,curiyr] +
                                                pu.qksas[11-1,curiyr] +
                            pu.qasin[11-1,curiyr] + pu.qpcas[11-1,curiyr] + pu.qsgin[11-1,curiyr]) / \
                            (pu.qmgas[11-1,curiyr]/pu.cfmgq[curiyr] +
                            pu.qjftr[11-1,curiyr]/pu.cfjfq[curiyr] + pu.qdsas[11-1,curiyr]/pu.cfdsqt[curiyr] +
                            pu.qrlas[11-1,curiyr]/pu.cfrsq + pu.qrhas[11-1,curiyr]/pu.cfrsq +
            #!                       pu.qotas[11-1,curiyr]/pu.cfotq[curiyr] +
                            [pu.qppin[mnumcr-1,curiyr]+pu.qppinpf[mnumcr-1,curiyr]]/pu.cfppq +
                            pu.qotin[mnumcr-1,curiyr]/pu.cfotq[curiyr] +
                            pu.qluin[mnumcr-1,curiyr]/pu.cfluq + pu.qlutr[mnumcr-1,curiyr]/pu.cfluq +
                            pu.qagtr[mnumcr-1,curiyr]/pu.cfavq + pu.qksas[11-1,curiyr]/pu.cfksq +
                            pu.qlgas[11-1,curiyr]/pu.cflgq[curiyr] + pu.qpcas[11-1,curiyr]/pu.cfpcq +
                            pu.qpfin[11-1,curiyr]/pu.cfpfq[curiyr] + pu.qasin[11-1,curiyr]/pu.cfasq +
                            pu.qsgin[11-1,curiyr]/pu.cfsgq)
        df.loc[df['API']==api.upper(),yr] = t
        pu.cftpq[curiyr] = t
    #print(df.loc[df['API']==api.upper()])
    return df

def calc_CFIMPRD(pyfiler,df,api,years_after_mnfactorx):
    """calculate the CFIMPRD value, update pyfiler and return the updated mnfactorx df.

    Parameters
    ----------
    pyfiler : module
        pyfiler fortran module
    df : dataframe
        the mnfactorx df
    api : str
        the API variable string
    years_after_mnfactorx : list
        list of the prediction years

    Returns
    -------
    dataframe
        the updated mnfactorx df
    """  
    # https://git.eia.gov/oea/NEMS/-/blob/main/source/main.f#L3242
    pu=pyfiler.utils
    mnumpr=pu.mnumpr

    for yr in years_after_mnfactorx:
        curiyr=yr-1990+1-1      #34=2024-1990+1-1  
        
        n=pu.rfipqas[mnumpr-1,curiyr,1] + pu.rfipqag[mnumpr-1,curiyr,1] + pu.rfipqcd[mnumpr-1,curiyr,1] + \
            pu.rfipqmg[mnumpr-1,curiyr,1] + pu.rfipqrg[mnumpr-1,curiyr,1] + pu.rfipqdl[mnumpr-1,curiyr,1] + \
            pu.rfipqdu[mnumpr-1,curiyr,1] + pu.rfipqjf[mnumpr-1,curiyr,1] + pu.rfipqpf[mnumpr-1,curiyr,1] + \
            pu.rfipqpr[mnumpr-1,curiyr,1] + pu.rfipqpy[mnumpr-1,curiyr,1] + pu.rfipqpp[mnumpr-1,curiyr,1] + \
            pu.rfipqet[mnumpr-1,curiyr,1] + pu.rfipqbu[mnumpr-1,curiyr,1] + pu.rfipqis[mnumpr-1,curiyr,1] + \
            pu.rfipqlu[mnumpr-1,curiyr,1] + pu.rfipqds[mnumpr-1,curiyr,1] + pu.rfipqrl[mnumpr-1,curiyr,1] + \
            pu.rfipqrh[mnumpr-1,curiyr,1] + pu.rfipqpc[mnumpr-1,curiyr,1]

        if n == 0:
            df.loc[df['API']==api.upper(),yr] = 5.8
            pu.cfimprd[curiyr] = 5.8            
            continue

        t = ( \
            pu.rfipqas[mnumpr-1,curiyr,1] * pu.cfasq + \
            pu.rfipqag[mnumpr-1,curiyr,1] * pu.cfavq + \
            pu.rfipqcd[mnumpr-1,curiyr,1] * pu.cfdscq[curiyr] + \
            pu.rfipqmg[mnumpr-1,curiyr,1] * pu.cftgq[curiyr] + \
            pu.rfipqrg[mnumpr-1,curiyr,1] * pu.cfrgq[curiyr] + \
            pu.rfipqdl[mnumpr-1,curiyr,1] * pu.cfdslq[curiyr] + \
            pu.rfipqdu[mnumpr-1,curiyr,1] * pu.cfdsuq[curiyr] + \
            pu.rfipqjf[mnumpr-1,curiyr,1] * pu.cfjfq[curiyr] + \
            pu.rfipqpf[mnumpr-1,curiyr,1] * pu.cfpfq[curiyr] + \
            pu.rfipqpr[mnumpr-1,curiyr,1] * pu.cfprq + \
            pu.rfipqpy[mnumpr-1,curiyr,1] * pu.cfprq + \
            pu.rfipqet[mnumpr-1,curiyr,1] * pu.cfeeq + \
            pu.rfipqbu[mnumpr-1,curiyr,1] * pu.cfbuq + \
            pu.rfipqis[mnumpr-1,curiyr,1] * pu.cfibq + \
            pu.rfipqpp[mnumpr-1,curiyr,1] * pu.cfppq + \
            pu.rfipqlu[mnumpr-1,curiyr,1] * pu.cfluq + \
            pu.rfipqds[mnumpr-1,curiyr,1] * pu.cfdsq + \
            pu.rfipqrl[mnumpr-1,curiyr,1] * pu.cfrsq + \
            pu.rfipqrh[mnumpr-1,curiyr,1] * pu.cfrsq + \
            pu.rfipqpc[mnumpr-1,curiyr,1] * pu.cfpcq) / \
            (pu.rfipqas[mnumpr-1,curiyr,1] + pu.rfipqag[mnumpr-1,curiyr,1] + pu.rfipqcd[mnumpr-1,curiyr,1] + \
            pu.rfipqmg[mnumpr-1,curiyr,1] + pu.rfipqrg[mnumpr-1,curiyr,1] + pu.rfipqdl[mnumpr-1,curiyr,1] + \
            pu.rfipqdu[mnumpr-1,curiyr,1] + pu.rfipqjf[mnumpr-1,curiyr,1] + pu.rfipqpf[mnumpr-1,curiyr,1] + \
            pu.rfipqpr[mnumpr-1,curiyr,1] + pu.rfipqpy[mnumpr-1,curiyr,1] + pu.rfipqpp[mnumpr-1,curiyr,1] + \
            pu.rfipqet[mnumpr-1,curiyr,1] + pu.rfipqbu[mnumpr-1,curiyr,1] + pu.rfipqis[mnumpr-1,curiyr,1] + \
            pu.rfipqlu[mnumpr-1,curiyr,1] + pu.rfipqds[mnumpr-1,curiyr,1] + \
            pu.rfipqrl[mnumpr-1,curiyr,1] + pu.rfipqrh[mnumpr-1,curiyr,1] + pu.rfipqpc[mnumpr-1,curiyr,1])        
        df.loc[df['API']==api.upper(),yr] = t
        pu.cfimprd[curiyr] = t
    #print(df.loc[df['API']==api.upper()])
    return df

def calc_CFEXPRD(pyfiler,df,api,years_after_mnfactorx):
    """calculate the CFEXPRD value, update pyfiler and return the updated mnfactorx df.

    Parameters
    ----------
    pyfiler : module
        the restart file object
    df : dataframe
        the mnfactorx df
    api : str
        the API variable string
    years_after_mnfactorx : list
        list of the prediction years

    Returns
    -------
    dataframe
        the updated mnfactorx df
    """    
    # https://git.eia.gov/oea/NEMS/-/blob/main/source/main.f#L3273
    pu=pyfiler.utils

    for yr in years_after_mnfactorx:
        curiyr=yr-1990+1-1      #34=2024-1990+1-1  
        
        if pu.qprdex[30-1,curiyr] == 0:
            df.loc[df['API']==api.upper(),yr] = 5.8
            pu.cfexprd[curiyr] = 5.8
            continue
        
        t = (
            pu.qprdex[ 1-1,curiyr] * pu.cfprq +             #! use straight propane factor
            pu.qprdex[ 2-1,curiyr] * pu.cftgq[curiyr] + 
            pu.qprdex[ 3-1,curiyr] * pu.cfrgq[curiyr] + 
            pu.qprdex[ 4-1,curiyr] * pu.cftgq[curiyr] + 
            pu.qprdex[ 5-1,curiyr] * pu.cfrgq[curiyr] + 
            pu.qprdex[ 6-1,curiyr] * pu.cfjfq[curiyr] + 
            pu.qprdex[ 7-1,curiyr] * pu.cfdsq + 
            pu.qprdex[ 8-1,curiyr] * pu.cfrsq + 
            pu.qprdex[ 9-1,curiyr] * pu.cfrsq + 
            pu.qprdex[10-1,curiyr] * pu.cfgo3[curiyr] + 
            pu.qprdex[11-1,curiyr] * pu.cfpfq[curiyr] + 
            pu.qprdex[12-1,curiyr] * pu.cfasq + 
            pu.qprdex[13-1,curiyr] * pu.cfdslq[curiyr] + 
            pu.qprdex[14-1,curiyr] * pu.cfprq + 
            pu.qprdex[15-1,curiyr] * pu.cfbuq + 
            pu.qprdex[16-1,curiyr] * pu.cfpcq + 
            pu.qprdex[17-1,curiyr] * pu.cfe85q[curiyr] + 
            pu.qprdex[18-1,curiyr] * pu.cfavq + 
            pu.qprdex[19-1,curiyr] * pu.cfluq + 
            pu.qprdex[20-1,curiyr] * pu.cfar3[curiyr] + 
            pu.qprdex[21-1,curiyr] * pu.cfmn3[curiyr] + 
            pu.qprdex[22-1,curiyr] * pu.cfmeqt + 
            pu.qprdex[23-1,curiyr] * pu.cfgop[curiyr] + 
            pu.qprdex[24-1,curiyr] * pu.cfdsuq[curiyr] + 
            pu.qprdex[25-1,curiyr] * pu.cfdscq[curiyr] +    #!  carbdsuout; next [26-1] is carbobout
            pu.qprdex[26-1,curiyr] * pu.cfrgq[curiyr] + 
            pu.qprdex[27-1,curiyr] * pu.cfeeq + 
            pu.qprdex[28-1,curiyr] * pu.cfibq + 
            pu.qprdex[29-1,curiyr] * pu.cfppq) / pu.qprdex[30-1,curiyr]     
        df.loc[df['API']==api.upper(),yr] = t
        pu.cfexprd[curiyr] = t
    #print(df.loc[df['API']==api.upper()])
    return df

def calc_CFDSQT(pyfiler,df,api,years_after_mnfactorx):
    """calculate the CFDSQT value, update pyfiler and return the updated mnfactorx df.

    Parameters
    ----------
    pyfiler : module
        the restart file object
    
    df : dataframe
        the mnfactorx df
    
    api : str
        the API variable string
    
    years_after_mnfactorx : list
        list of the prediction years

    Returns
    -------
    dataframe
        the updated mnfactorx df
    """    
    # https://git.eia.gov/oea/NEMS/-/blob/main/source/main.f#L3143
    #!  but first somebody needs to calculate a total distillate conversion factor:
    pu=pyfiler.utils
    mnumcr=pu.mnumcr

    # Create a list of all quantities (qdsrs, qdscm, etc.) and corresponding factors
    quantities = np.array([
        pu.qdsrs,  # Distillate - Residential
        pu.qdscm,  # Distillate - Commercial
        pu.qdsin,  # Distillate - Industrial
        pu.qdstr,  # Distillate - Transportation
        pu.qdsel   # Distillate - Electricity
    ])

    factors = np.array([
        pu.cfdsrs,  # Distillate - Residential factor
        pu.cfdscm,  # Distillate - Commercial factor
        pu.cfdsin,  # Distillate - Industrial factor
        pu.cfdstr,  # Distillate - Transportation factor
        pu.cfdsel   # Distillate - Electricity factor
    ])

    # Loop over years
    for yr in years_after_mnfactorx:
        curiyr = yr - 1990  # Offset year to index correctly

        # Extract the quantities and factors for the current year
        year_quantities = quantities[:, mnumcr - 1, curiyr]
        year_factors = factors[:, curiyr]

        # Compute total quantity and ensure it is greater than 0
        total_quantity = np.sum(year_quantities)

        # the division can't be zero
        if total_quantity != 0:
            # Calculate the weighted average
            t = np.average(year_factors, weights=year_quantities)
            '''
            t = \
                (pu.cfdsrs[curiyr] * pu.qdsrs[mnumcr-1,curiyr] + \
                    pu.cfdscm[curiyr] * pu.qdscm[mnumcr-1,curiyr] + \
                    pu.cfdsin[curiyr] * pu.qdsin[mnumcr-1,curiyr] + \
                    pu.cfdstr[curiyr] * pu.qdstr[mnumcr-1,curiyr] + \
                    pu.cfdsel[curiyr] * pu.qdsel[mnumcr-1,curiyr]) / \
                (pu.qdsrs[mnumcr-1,curiyr] + pu.qdscm[mnumcr-1,curiyr] + pu.qdsin[mnumcr-1,curiyr] + \
                pu.qdstr[mnumcr-1,curiyr] + pu.qdsel[mnumcr-1,curiyr])     
            '''
            # Update DataFrame and pytfiler
            df.loc[df['API'] == api.upper(), yr] = t
            pu.cfdsqt[curiyr] = t

    #print(df.loc[df['API']==api.upper()])
    return df

def calc_CFCRDIMP(pyfiler,df,api,years_after_mnfactorx):
    """calculate the CFCRDIMP value, update pyfiler and return the updated mnfactorx df.

    Parameters
    ----------
    pyfiler : module
        the restart file object
    df : dataframe
        the mnfactorx df
    api : str
        the API variable string
    years_after_mnfactorx : list
        list of the prediction years

    Returns
    -------
    dataframe
        the updated mnfactorx df
    """    
    # https://git.eia.gov/oea/NEMS/-/blob/main/source/main.f#L3094
    pu=pyfiler.utils
    mnumpr=pu.mnumpr    #10
    mncrud=pu.mncrud    #11
    # for debug:
    #pu.q_crude_importa shape (10,11,91)
    factors = np.vstack([
        pu.cfcrdltswt, pu.cfcrdltsour, pu.cfcrdmd2sour, pu.cfcrdmdsour,
        pu.cfcrdhvswt, pu.cfcrdhvsour, pu.cfcrdca, pu.cfcrdsyn,
        pu.cfcrddilbit, pu.cfcrdlt2swt, pu.cfcrdlscond
    ])

    for yr in years_after_mnfactorx:
        curiyr = yr - 1990  # current index for weights
        curcalyr = yr - 1990  # current year index for imports

        # Access the relevant slice of the crude imports array
        crude_importa = pu.q_crude_importa[mnumpr - 1, :mncrud, curcalyr]

        # Check if the sum of imports is non-zero
        if np.sum(crude_importa) > 0:
            # Compute the weighted average using np.average()
            t = np.average(factors[:mncrud, curiyr], weights=crude_importa)
            '''
            t = \
                (pu.q_crude_importa[mnumpr-1, 0,curcalyr] * pu.cfcrdltswt[curiyr]     + \
                pu.q_crude_importa[mnumpr-1, 1,curcalyr] * pu.cfcrdltsour[curiyr]     + \
                pu.q_crude_importa[mnumpr-1, 2,curcalyr] * pu.cfcrdmd2sour[curiyr]    + \
                pu.q_crude_importa[mnumpr-1, 3,curcalyr] * pu.cfcrdmdsour[curiyr]     + \
                pu.q_crude_importa[mnumpr-1, 4,curcalyr] * pu.cfcrdhvswt[curiyr]      + \
                pu.q_crude_importa[mnumpr-1, 5,curcalyr] * pu.cfcrdhvsour[curiyr]     + \
                pu.q_crude_importa[mnumpr-1, 6,curcalyr] * pu.cfcrdca[curiyr]         + \
                pu.q_crude_importa[mnumpr-1, 7,curcalyr] * pu.cfcrdsyn[curiyr]        + \
                pu.q_crude_importa[mnumpr-1, 8,curcalyr] * pu.cfcrddilbit[curiyr]     + \
                pu.q_crude_importa[mnumpr-1,9,curcalyr] * pu.cfcrdlt2swt[curiyr]     + \
                pu.q_crude_importa[mnumpr-1,10,curcalyr] * pu.cfcrdlscond[curiyr]) /    \
                np.sum(pu.q_crude_importa[mnumpr-1, 1-1:11,curcalyr])    
            '''
            # Store the result in the DataFrame and pyfiler
            df.loc[df['API']==api.upper(),yr] = t
            pu.cfcrdimp[curiyr]=t
    #print(df.loc[df['API']==api.upper()])                  
    return df

def calc_CFCRDEXP(pyfiler,df,api,years_after_mnfactorx):
    """calculate the CFCRDEXP value, update pyfiler and return the updated mnfactorx df.

    Parameters
    ----------
    pyfiler : module
        the restart file object
    df : dataframe
        the mnfactorx df
    api : str
        the API variable string
    years_after_mnfactorx : list
        list of the prediction years

    Returns
    -------
    dataframe
        the updated mnfactorx df
    """    
    # https://git.eia.gov/oea/NEMS/-/blob/main-fort/source/main.f#L3030
    pu=pyfiler.utils
    mnumpr=pu.mnumpr    #10
    mncrud=pu.mncrud    #11
    # for debug:
    #pu.q_crude_importa shape (10,11,91)

    factors = np.array([
        pu.cfcrdltswt, pu.cfcrdltsour, pu.cfcrdmd2sour, pu.cfcrdmdsour, 
        pu.cfcrdhvswt, pu.cfcrdhvsour, pu.cfcrdca, pu.cfcrdsyn, 
        pu.cfcrddilbit, pu.cfcrdlt2swt, pu.cfcrdlscond
    ])

    for yr in years_after_mnfactorx:
        curiyr = yr - 1990  # current index for weights
        curcalyr = yr - 1990  # current year index for crude exports

        # Extract the relevant slice of crude exports for the current year
        crude_exports = pu.q_crude_exports[mnumpr - 1, :mncrud, curcalyr]  # Shape (11,)
        if np.sum(crude_exports) > 0:

            # Compute the weighted average using np.average()
            t = np.average(factors[:mncrud, curiyr], weights=crude_exports)
            '''
            t = \
                (pu.q_crude_exports[mnumpr-1, 0,curcalyr] * pu.cfcrdltswt[curiyr]      + \
                pu.q_crude_exports[mnumpr-1, 1,curcalyr] * pu.cfcrdltsour[curiyr]     + \
                pu.q_crude_exports[mnumpr-1, 2,curcalyr] * pu.cfcrdmd2sour[curiyr]    + \
                pu.q_crude_exports[mnumpr-1, 3,curcalyr] * pu.cfcrdmdsour[curiyr]     + \
                pu.q_crude_exports[mnumpr-1, 4,curcalyr] * pu.cfcrdhvswt[curiyr]      + \
                pu.q_crude_exports[mnumpr-1, 5,curcalyr] * pu.cfcrdhvsour[curiyr]     + \
                pu.q_crude_exports[mnumpr-1, 6,curcalyr] * pu.cfcrdca[curiyr]         + \
                pu.q_crude_exports[mnumpr-1, 7,curcalyr] * pu.cfcrdsyn[curiyr]        + \
                pu.q_crude_exports[mnumpr-1, 8,curcalyr] * pu.cfcrddilbit[curiyr]     + \
                pu.q_crude_exports[mnumpr-1, 9,curcalyr] * pu.cfcrdlt2swt[curiyr]     + \
                pu.q_crude_exports[mnumpr-1,10,curcalyr] * pu.cfcrdlscond[curiyr]) /    \
                np.sum(pu.q_crude_exports[mnumpr-1, 1-1:11,curcalyr])
            '''     
            # Store the result in the DataFrame and update pyfiler
            df.loc[df['API']==api.upper(),yr] = t
            pu.cfcrdexp[curiyr]=t

    #print(df.loc[df['API']==api.upper()])
    return df

def calc_CFCRDDOM(pyfiler,df,api,years_after_mnfactorx):
    """calculate the CFCRDDOM value, update pyfiler and return the updated mnfactorx df.

    Parameters
    ----------
    pyfiler : module
        the restart file object
    df : dataframe
        the mnfactorx df
    api : str
        the API variable string
    years_after_mnfactorx : list
        list of the prediction years

    Returns
    -------
    dataframe
        the updated mnfactorx df
    """    
    # https://git.eia.gov/oea/NEMS/-/blob/main/source/main.f#L3079
    pu=pyfiler.utils
    mnumor=pu.mnumor    #14
    mncrud=pu.mncrud    #11

    # Stack the weights into a single array for easier computation
    weights = np.vstack([
        pu.cfcrdltswt, pu.cfcrdltsour, pu.cfcrdmd2sour, pu.cfcrdmdsour, 
        pu.cfcrdhvswt, pu.cfcrdhvsour, pu.cfcrdca, pu.cfcrdsyn, 
        pu.cfcrddilbit, pu.cfcrdlt2swt, pu.cfcrdlscond
    ])

    for yr in years_after_mnfactorx:
        curiyr=yr-1990+1-1      #34=2024-1990+1-1

        # Slice the production data for the current year and first (mnumor - 1) 
        production = pu.ogcrdprd[:mnumor-1, :mncrud, curiyr]    # (13,11)

        if np.sum(production) > 0:  # Check if total production is non-zero
            # Compute weighted average using np.average
            category_weights = weights[:mncrud, curiyr]  # Get weights for current year
            t = np.average(
                category_weights,  # Use corresponding weights
                weights=np.sum(production, axis=0)  # Sum production for each category
            )
            # Update DataFrame and external variable
            df.loc[df['API'] == api.upper(), yr] = t
            pu.cfcrddom[curiyr] = t

    #print(df.loc[df['API']==api.upper()])
    return df

    '''
    # for debug:
    curiyr=2023-1990+1
    print(cfcrddom[curiyr])     #5.6927150
    print(pu.cfcrddom[curiyr])  #5.6927147

    #debug expeted result
    Stephanie's new data: (curiyr=35)
    #cfcrddom[2024-1990+1]=5.6860166
    #pu.cfcrddom[2024-1990+1]=5.6860156

    my old data: good! (curiyr=34)
    #cfcrddom[2023-1990+1]=5.692715
    #pu.cfcrddom[2023-1990+1]=5.6927147
    # --------
    t = ( \
                np.sum(pu.ogcrdprd[:mnumor-1, 0, curiyr]) * pu.cfcrdltswt[curiyr] +\
                np.sum(pu.ogcrdprd[:mnumor-1, 1, curiyr]) * pu.cfcrdltsour[curiyr] +\
                np.sum(pu.ogcrdprd[:mnumor-1, 2, curiyr]) * pu.cfcrdmd2sour[curiyr] +\
                np.sum(pu.ogcrdprd[:mnumor-1, 3, curiyr]) * pu.cfcrdmdsour[curiyr] +\
                np.sum(pu.ogcrdprd[:mnumor-1, 4, curiyr]) * pu.cfcrdhvswt[curiyr] +\
                np.sum(pu.ogcrdprd[:mnumor-1, 5, curiyr]) * pu.cfcrdhvsour[curiyr] +\
                np.sum(pu.ogcrdprd[:mnumor-1, 6, curiyr]) * pu.cfcrdca[curiyr] +\
                np.sum(pu.ogcrdprd[:mnumor-1, 7, curiyr]) * pu.cfcrdsyn[curiyr] +\
                np.sum(pu.ogcrdprd[:mnumor-1, 8, curiyr]) * pu.cfcrddilbit[curiyr] +\
                np.sum(pu.ogcrdprd[:mnumor-1, 9, curiyr]) * pu.cfcrdlt2swt[curiyr] +\
                np.sum(pu.ogcrdprd[:mnumor-1, 10, curiyr]) * pu.cfcrdlscond[curiyr] \
                ) / np.sum(pu.ogcrdprd[:mnumor-1, :mncrud, curiyr])
    '''

    ''' Fortran original code:------------------
      IF (sum(OGCRDPRD(1:MNUMOR-1, 1:MNCRUD,CURIYR)) .GT. 0.0) THEN
         CFCRDDOM(CURIYR) =                                           &
              (sum(OGCRDPRD(1:MNUMOR-1, 1,CURIYR)) * CFCRDLTSWT(CURIYR)      + &
               sum(OGCRDPRD(1:MNUMOR-1, 2,CURIYR)) * CFCRDLTSOUR(CURIYR)     + &
               sum(OGCRDPRD(1:MNUMOR-1, 3,CURIYR)) * CFCRDMD2SOUR(CURIYR)    + &
               sum(OGCRDPRD(1:MNUMOR-1, 4,CURIYR)) * CFCRDMDSOUR(CURIYR)     + &
               sum(OGCRDPRD(1:MNUMOR-1, 5,CURIYR)) * CFCRDHVSWT(CURIYR)      + &
               sum(OGCRDPRD(1:MNUMOR-1, 6,CURIYR)) * CFCRDHVSOUR(CURIYR)     + &
               sum(OGCRDPRD(1:MNUMOR-1, 7,CURIYR)) * CFCRDCA(CURIYR)         + &
               sum(OGCRDPRD(1:MNUMOR-1, 8,CURIYR)) * CFCRDSYN(CURIYR)        + &
               sum(OGCRDPRD(1:MNUMOR-1, 9,CURIYR)) * CFCRDDILBIT(CURIYR)     + &
               sum(OGCRDPRD(1:MNUMOR-1,10,CURIYR)) * CFCRDLT2SWT(CURIYR)     + &
               sum(OGCRDPRD(1:MNUMOR-1,11,CURIYR)) * CFCRDLSCOND(CURIYR)) /    &
                  sum(OGCRDPRD(1:MNUMOR-1, 1:MNCRUD,CURIYR))
      ENDIF
      # OGCRDPRD is includes/ogsmout 
    '''