# -*- coding: utf-8 -*-
"""
Created on Oct 25 2024

@author: Claire Su
"""
from datetime import datetime
import os, sys
import numpy as np

from logging_utilities import print_it

dll_dirs = ([r"C:\Program Files (x86)\Intel\oneAPI\compiler\2023.2.1\windows\redist\intel64_win\compiler",
            r"C:\Windows\System32"])

for i in dll_dirs:
    if i not in sys.path:
        sys.path.append(i)
        os.add_dll_directory(i)

j = os.path.join(os.getcwd(), "PyFiler")
if j not in sys.path:
    sys.path.append(j)

MODULE_NAME = "mnfactorx_calc.py"
''' *************************** Module Level Design Concepts ************************************************************
This module mnfactorx_calc.py is designed to provide a Python version source/utils_docvfacts.f90 DOCVFACTS SUBROUTINE functionality.
The file input/mnfactorx_calc_api_list.txt is also used to support this module functionality:   \
api_file='./input/mnfactorx_calc_api_list.txt'
'''

def calc_CFTPQ(pyfiler,curiyr):
    """calculate the total petroleum CFTPQ value, and update pyfiler.

    Parameters
    ----------
    pyfiler : object
        the restart file object
    curiyr : int
        Current NEMS year. For example, for year 1990, curiyr=1. For year 2023, curiyr=34=2023-1990+1.
    """
    pu=pyfiler.utils
    mnumcr=pu.mnumcr
    t = (pu.qmgas[11-1,curiyr-1] + pu.qjftr[11-1,curiyr-1] +
                        pu.qdsas[11-1,curiyr-1] + pu.qlgas[11-1,curiyr-1] + pu.qpfin[11-1,curiyr-1] +
                        pu.qrlas[11-1,curiyr-1] + pu.qrhas[11-1,curiyr-1] +
        #! replacing other with the pieces - ind [lubricants-1, pentanes plus] and tran [av gas-1, lubricants]
                        pu.qppin[mnumcr-1,curiyr-1] + pu.qppinpf[mnumcr-1,curiyr-1] +
                        pu.qluin[mnumcr-1,curiyr-1] + pu.qotin[mnumcr-1,curiyr-1] +
                        pu.qagtr[mnumcr-1,curiyr-1] + pu.qlutr[mnumcr-1,curiyr-1] +
                                            pu.qksas[11-1,curiyr-1] +
                        pu.qasin[11-1,curiyr-1] + pu.qpcas[11-1,curiyr-1] + pu.qsgin[11-1,curiyr-1]) / \
                        (pu.qmgas[11-1,curiyr-1]/pu.cfmgq[curiyr-1] +
                        pu.qjftr[11-1,curiyr-1]/pu.cfjfq[curiyr-1] + pu.qdsas[11-1,curiyr-1]/pu.cfdsqt[curiyr-1] +
                        pu.qrlas[11-1,curiyr-1]/pu.cfrsq + pu.qrhas[11-1,curiyr-1]/pu.cfrsq +
        #!                       pu.qotas[11-1,curiyr-1]/pu.cfotq[curiyr-1] +
                        [pu.qppin[mnumcr-1,curiyr-1]+pu.qppinpf[mnumcr-1,curiyr-1]]/pu.cfppq +
                        pu.qotin[mnumcr-1,curiyr-1]/pu.cfotq[curiyr-1] +
                        pu.qluin[mnumcr-1,curiyr-1]/pu.cfluq + pu.qlutr[mnumcr-1,curiyr-1]/pu.cfluq +
                        pu.qagtr[mnumcr-1,curiyr-1]/pu.cfavq + pu.qksas[11-1,curiyr-1]/pu.cfksq +
                        pu.qlgas[11-1,curiyr-1]/pu.cflgq[curiyr-1] + pu.qpcas[11-1,curiyr-1]/pu.cfpcq +
                        pu.qpfin[11-1,curiyr-1]/pu.cfpfq[curiyr-1] + pu.qasin[11-1,curiyr-1]/pu.cfasq +
                        pu.qsgin[11-1,curiyr-1]/pu.cfsgq)
    pu.cftpq[curiyr-1] = t

def calc_CFIMPRD(pyfiler,curiyr):
    """calculate the CFIMPRD value, and update pyfiler.

    Parameters
    ----------
    pyfiler : object
        the restart file object
    curiyr : int
        Current NEMS year. For example, for year 1990, curiyr=1. For year 2023, curiyr=34=2023-1990+1.
    """
    pu=pyfiler.utils
    mnumpr=pu.mnumpr
    # cpnvert index for 0-based indexing
    curiyr=curiyr-1

    n=pu.rfipqas[mnumpr-1,curiyr,1] + pu.rfipqag[mnumpr-1,curiyr,1] + pu.rfipqcd[mnumpr-1,curiyr,1] + \
        pu.rfipqmg[mnumpr-1,curiyr,1] + pu.rfipqrg[mnumpr-1,curiyr,1] + pu.rfipqdl[mnumpr-1,curiyr,1] + \
        pu.rfipqdu[mnumpr-1,curiyr,1] + pu.rfipqjf[mnumpr-1,curiyr,1] + pu.rfipqpf[mnumpr-1,curiyr,1] + \
        pu.rfipqpr[mnumpr-1,curiyr,1] + pu.rfipqpy[mnumpr-1,curiyr,1] + pu.rfipqpp[mnumpr-1,curiyr,1] + \
        pu.rfipqet[mnumpr-1,curiyr,1] + pu.rfipqbu[mnumpr-1,curiyr,1] + pu.rfipqis[mnumpr-1,curiyr,1] + \
        pu.rfipqlu[mnumpr-1,curiyr,1] + pu.rfipqds[mnumpr-1,curiyr,1] + pu.rfipqrl[mnumpr-1,curiyr,1] + \
        pu.rfipqrh[mnumpr-1,curiyr,1] + pu.rfipqpc[mnumpr-1,curiyr,1]

    if n == 0:
        pu.cfimprd[curiyr] = 5.8

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
    pu.cfimprd[curiyr] = t

def calc_CFEXPRD(pyfiler,curiyr):
    """calculate the CFEXPRD value, update pyfiler and return the updated mnfactorx df.

    Parameters
    ----------
    pyfiler : object
        the restart file object
    curiyr : int
        Current NEMS year. For example, for year 1990, curiyr=1. For year 2023, curiyr=34=2023-1990+1.
    """
    pu=pyfiler.utils
    mnumpr=pu.mnumpr
    '''
    for yr in years_after_mnfactorx:
        curiyr=yr-1990+1-1      #34=2024-1990+1-1  
    '''
    # cpnvert index for 0-based indexing
    curiyr=curiyr-1
        
    if pu.qprdex[30-1,curiyr] == 0:
        pu.cfexprd[curiyr] = 5.8
    
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
    pu.cfexprd[curiyr] = t

def calc_CFDSQT(pyfiler,curiyr):
    """calculate the CFDSQT value, update pyfiler and return the updated mnfactorx df.

    Parameters
    ----------
    pyfiler : object
        the restart file object
    curiyr : int
        Current NEMS year. For example, for year 1990, curiyr=1. For year 2023, curiyr=34=2023-1990+1.
    """
    pu=pyfiler.utils
    mnumcr=pu.mnumcr

    #!  but first somebody needs to calculate a total distillate conversion factor:
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

    # Extract the quantities and factors for the current year
    year_quantities = quantities[:, mnumcr - 1, curiyr-1]
    year_factors = factors[:, curiyr-1]

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
        pu.cfdsqt[curiyr-1] = t

def calc_CFCRDIMP(pyfiler,curiyr):    
    """calculate the CFCRDIMP value, and update pyfiler.

    Parameters
    ----------
    pyfiler : object
        the restart file object
    curiyr : int
        Current NEMS year. For example, for year 1990, curiyr=1. For year 2023, curiyr=34=2023-1990+1.
    """   
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

    curcalyr = curiyr + 1989  # current year index for imports
    # Access the relevant slice of the crude imports array
    crude_importa = pu.q_crude_importa[mnumpr - 1, :mncrud, (curcalyr-1989)-1]

    # Check if the sum of imports is non-zero
    if np.sum(crude_importa) > 0:
        # Compute the weighted average using np.average()
        t = np.average(factors[:mncrud, curiyr-1], weights=crude_importa)
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
        pu.cfcrdimp[curiyr-1]=t

def calc_CFCRDEXP(pyfiler,curiyr):
    """calculate the CFCRDEXP value, and update pyfiler.

    Parameters
    ----------
    pyfiler : object
        the restart file object
    curiyr : int
        Current NEMS year. For example, for year 1990, curiyr=1. For year 2023, curiyr=34=2023-1990+1.
    """   
    pu=pyfiler.utils
    mnumpr=pu.mnumpr    #10
    mncrud=pu.mncrud    #11

    factors = np.array([
        pu.cfcrdltswt, pu.cfcrdltsour, pu.cfcrdmd2sour, pu.cfcrdmdsour, 
        pu.cfcrdhvswt, pu.cfcrdhvsour, pu.cfcrdca, pu.cfcrdsyn, 
        pu.cfcrddilbit, pu.cfcrdlt2swt, pu.cfcrdlscond
    ])

    curcalyr = curiyr + 1989  # current year index for imports
    # Extract the relevant slice of crude exports for the current year
    crude_exports = pu.q_crude_exports[mnumpr - 1, :mncrud, (curcalyr-1989)-1]  # Shape (11,)
    if np.sum(crude_exports) > 0:

        # Compute the weighted average using np.average()
        t = np.average(factors[:mncrud, curiyr-1], weights=crude_exports)
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
        pu.cfcrdexp[curiyr-1]=t

def calc_CFCRDDOM(pyfiler,curiyr):
    """calculate the CFCRDDOM value, and update pyfiler.

    Parameters
    ----------
    pyfiler : object
        the restart file object
    curiyr : int
        Current NEMS year. For example, for year 1990, curiyr=1. For year 2023, curiyr=34=2023-1990+1.
    """
    pu=pyfiler.utils
    mnumor=pu.mnumor    #14
    mncrud=pu.mncrud    #11

    # Stack the weights into a single array for easier computation
    weights = np.vstack([
        pu.cfcrdltswt, pu.cfcrdltsour, pu.cfcrdmd2sour, pu.cfcrdmdsour, 
        pu.cfcrdhvswt, pu.cfcrdhvsour, pu.cfcrdca, pu.cfcrdsyn, 
        pu.cfcrddilbit, pu.cfcrdlt2swt, pu.cfcrdlscond
    ])

    # Slice the production data for the current year and first (mnumor - 1) 
    production = pu.ogcrdprd[:mnumor-1, :mncrud, curiyr-1]    # (13,11)

    if np.sum(production) > 0:  # Check if total production is non-zero
        # Compute weighted average using np.average
        category_weights = weights[:mncrud, curiyr-1]  # Get weights for current year
        t = np.average(
            category_weights,  # Use corresponding weights
            weights=np.sum(production, axis=0)  # Sum production for each category
        )
        # Update DataFrame and external variable
        pu.cfcrddom[curiyr-1] = t

def docvfacts(pyfiler, user):
    """The major flow control method in this mnfactorx_calc module.

    Parameters
    ----------
    pyfiler : object
        the restart file object
    curiyr : int
        Current NEMS year. For example, for year 1990, curiyr=1. For year 2023, curiyr=34=2023-1990+1.

    Returns
    -------
    object
        the updated restart file object
    """
    curiyr = pyfiler.ncntrl.curiyr
    curitr = pyfiler.ncntrl.curitr
    po = pyfiler.convfact
    pu = pyfiler.utils
    mnumpr = pu.mnumpr    #10
    mncrud = pu.mncrud    #11

    #if curiyr < pyfiler.cvhistyr.histyr and user.SCEDES["DBDUMP"] == "1":
    if curiyr < pyfiler.convfact.histyr+1 and user.SCEDES["DBDUMP"] == "1":
        print_it(-9, f"   Historical year {curiyr+1989}.  Not updating conversion factors!", MODULE_NAME)
        return pyfiler

    #! Calculate overall petroleum conversion factor
    if curitr == 1 and user.SCEDES["DBDUMP"] == "1":
        print_it(-9, f" Petroleum consumption conversion factor from file in year {curiyr+1989}, is {po.cftpq[curiyr-1]}", MODULE_NAME)
    #!  unfinished oils
    calc_CFIMUO(pyfiler, curiyr)

    pu.cfe85q[curiyr-1] = pu.ethne85 * pu.cfetq[curiyr-1] + pu.trgne85 * pu.cfrbob[curiyr-1]
    # now main play
    calc_CFCRDDOM(pyfiler, curiyr)
    calc_CFCRDIMP(pyfiler, curiyr)
    pu.it_wop[curiyr-1,1] = pu.it_wop[curiyr-1,0] / pu.cfcrdimp[curiyr-1]
    calc_CFCRDEXP(pyfiler, curiyr)

    # call api_to_btu() for more calculation:
    #!  calculation option 12 in API_TO_BTU function converts API to Btu (array element 1 converted to array element 2)
    #!  calculation option 21 in API_TO_BTU function converts Btu to API (array element 2 converted to array element 1)
    pc=pyfiler.convfact
    pc.apicamg[0,curiyr-1] = api_to_btu(pyfiler, pc.apicamg[1,curiyr-1],21)
    pc.apiltsw[1,curiyr-1] = api_to_btu(pyfiler, pc.apiltsw[0,curiyr-1],12)
    pc.apiltso[1,curiyr-1] = api_to_btu(pyfiler, pc.apiltso[0,curiyr-1],12)
    pc.apimmso[1,curiyr-1] = api_to_btu(pyfiler, pc.apimmso[0,curiyr-1],12)
    pc.apimdso[1,curiyr-1] = api_to_btu(pyfiler, pc.apimdso[0,curiyr-1],12)
    pc.apihvsw[1,curiyr-1] = api_to_btu(pyfiler, pc.apihvsw[0,curiyr-1],12)
    pc.apihvso[1,curiyr-1] = api_to_btu(pyfiler, pc.apihvso[0,curiyr-1],12)
    pc.apica[1,curiyr-1]   = api_to_btu(pyfiler, pc.apica[0,curiyr-1],12)
    pc.apisyn[1,curiyr-1]  = api_to_btu(pyfiler, pc.apisyn[0,curiyr-1],12)
    pc.apidil[1,curiyr-1]  = api_to_btu(pyfiler, pc.apidil[0,curiyr-1],12)
    pc.apillsw[1,curiyr-1] = api_to_btu(pyfiler, pc.apillsw[0,curiyr-1],12)
    pc.api50pl[1,curiyr-1] = api_to_btu(pyfiler, pc.api50pl[0,curiyr-1],12)
    pc.apicrddom[1,curiyr-1] = api_to_btu(pyfiler, pc.apicrddom[0,curiyr-1],12)
    pc.apicrdimp[1,curiyr-1] = api_to_btu(pyfiler, pc.apicrdimp[0,curiyr-1],12)
    pc.apicrdexp[1,curiyr-1] = api_to_btu(pyfiler, pc.apicrdexp[0,curiyr-1],12)

    #!  but first somebody needs to calculate a total distillate conversion factor:
    calc_CFDSQT(pyfiler, curiyr)

    #!  now it is all clear for total petroleum:
    calc_CFTPQ(pyfiler, curiyr)

    if user.SCEDES["DBDUMP"] == "1":
        print_it(-9, f" Petroleum conversion factor after iteration {curitr}, is {po.cftpq[curiyr-1]}", MODULE_NAME)

    pu.qprdex[29,curiyr-1] = sum(pu.qprdex[:29,curiyr-1])

    ######! Fischer-Tropsch conversion factors
    #!  GTL:
    calc_CFGTLLIQ(pyfiler, curiyr)
    #!  CTL:
    calc_CFCTLLIQ(pyfiler, curiyr)
    #!  BTL:
    calc_CFBTLLIQ(pyfiler, curiyr)
    #!  CBTL:
    calc_CFCBTLLIQ(pyfiler, curiyr)
    ######! end Fischer-Tropsch conversion factors

    calc_CFIMPRD(pyfiler,curiyr)
    calc_CFEXPRD(pyfiler,curiyr)

    print("Done with docvfacts()!")
    return pyfiler

def calc_CFCBTLLIQ(pyfiler, curiyr):
    """calculate the CFCBTLLIQ value, and update pyfiler.

    Parameters
    ----------
    pyfiler : object
        the restart file object
    curiyr : int
        Current NEMS year. For example, for year 1990, curiyr=1. For year 2023, curiyr=34=2023-1990+1.
    """    
    pu=pyfiler.utils
    mnumpr=pu.mnumpr    #10
    # convert indices for 0-based indexing
    mnum_idx = mnumpr - 1
    curi_idx = curiyr - 1

    # compute values for cfcbtlliq[0, curi_idx]
    cbtlfrac_sum_1 = np.sum(pu.cbtlfrac[0, :4, mnum_idx, curi_idx])
    if cbtlfrac_sum_1 != 0.0:
        weighted_sum_1 = np.dot(pu.cbtlfrac[0, :4, mnum_idx, curi_idx], pu.cfftliq[:4, curi_idx])
        pu.cfcbtlliq[0, curi_idx] = weighted_sum_1 / cbtlfrac_sum_1
    else:
        pu.cfcbtlliq[0, curi_idx] = pu.cfdsq

    # compute values for cfcbtlliq[1, curi_idx]
    cbtlfrac_sum_2 = np.sum(pu.cbtlfrac[1, :4, mnum_idx, curi_idx])
    if cbtlfrac_sum_2 != 0.0:
        weighted_sum_2 = np.dot(pu.cbtlfrac[1, :4, mnum_idx, curi_idx], pu.cfftliq[:4, curi_idx])
        pu.cfcbtlliq[1, curi_idx] = weighted_sum_2 / cbtlfrac_sum_2
    else:
        pu.cfcbtlliq[1, curi_idx] = pu.cfdsq

    # compute values for cfcbtlliq[2, curi_idx]
    cbtlfrac_sum_1_total = np.sum(pu.cbtlfrac[0, :, mnum_idx, curi_idx])
    cbtlfrac_sum_2_total = np.sum(pu.cbtlfrac[1, :, mnum_idx, curi_idx])

    if (cbtlfrac_sum_1_total + cbtlfrac_sum_2_total) != 0.0:
        pu.cfcbtlliq[2, curi_idx] = (
            cbtlfrac_sum_1_total * pu.cfcbtlliq[0, curi_idx]
            + cbtlfrac_sum_2_total * pu.cfcbtlliq[1, curi_idx]
        ) / (cbtlfrac_sum_1_total + cbtlfrac_sum_2_total)
    else:
        pu.cfcbtlliq[2, curi_idx] = pu.cfdsq

def calc_CFBTLLIQ(pyfiler, curiyr):
    """calculate the CFBTLLIQ value, and update pyfiler.

    Parameters
    ----------
    pyfiler : object
        the restart file object
    curiyr : int
        Current NEMS year. For example, for year 1990, curiyr=1. For year 2023, curiyr=34=2023-1990+1.
    """
    pu=pyfiler.utils
    mnumpr=pu.mnumpr    #10
    # convert indices for 0-based indexing. Adjust mnumpr and curiyr to 0-based indexing for python.
    mnum_idx = mnumpr - 1
    curi_idx = curiyr - 1

    # slice arrays for the relevant data
    btlfrac_slice = pu.btlfrac[:4, mnum_idx, curi_idx]  # corresponds to Fortran btlfrac(1:4, mnumpr, curiyr)
    cfftliq_slice = pu.cfftliq[:4, curi_idx]            # corresponds to Fortran cfftliq(1:4, curiyr)

    # compute the sum of btlfrac slice
    btlfrac_sum = np.sum(btlfrac_slice)

    # perform calculation
    if btlfrac_sum != 0.0:
        # compute weighted sum
        weighted_sum = np.dot(btlfrac_slice, cfftliq_slice)
        pu.cfbtlliq[curi_idx] = weighted_sum / btlfrac_sum
    else:
        # assign fallback value
        pu.cfbtlliq[curi_idx] = pu.cfdsq

def calc_CFCTLLIQ(pyfiler, curiyr):
    """calculate the CFCTLLIQ value, and update pyfiler.

    Parameters
    ----------
    pyfiler : object
        the restart file object
    curiyr : int
        Current NEMS year. For example, for year 1990, curiyr=1. For year 2023, curiyr=34=2023-1990+1.
    """    
    pu=pyfiler.utils
    mnumpr=pu.mnumpr    #10
    # convert indices for 0-based indexing. Adjust mnumpr and curiyr to 0-based indexing for python.
    mnum_idx = mnumpr - 1
    curi_idx = curiyr - 1

    # slice arrays for the relevant data
    ctlfrac_slice = pu.ctlfrac[:4, mnum_idx, curi_idx]  # corresponds to Fortran ctlfrac(1:4, mnumpr, curiyr)
    cfftliq_slice = pu.cfftliq[:4, curi_idx]            # corresponds to Fortran cfftliq(1:4, curiyr)

    # compute the sum of ctlfrac slice
    ctlfrac_sum = np.sum(ctlfrac_slice)

    # perform calculation
    if ctlfrac_sum != 0.0:
        # compute weighted sum
        weighted_sum = np.dot(ctlfrac_slice, cfftliq_slice)
        pu.cfctlliq[curi_idx] = weighted_sum / ctlfrac_sum
    else:
        # assign fallback value
        pu.cfctlliq[curi_idx] = pu.cfdsq


def calc_CFGTLLIQ(pyfiler, curiyr):
    """calculate the value, and update pyfiler.

    Parameters
    ----------
    pyfiler : object
        the restart file object
    curiyr : int
        Current NEMS year. For example, for year 1990, curiyr=1. For year 2023, curiyr=34=2023-1990+1.
    """    
    pu=pyfiler.utils
    mnumpr=pu.mnumpr    #10

    # convert indices for 0-based indexing
    mnum_idx = mnumpr - 1
    curi_idx = curiyr - 1

    # slice arrays for the relevant data
    gtlfrac_slice = pu.gtlfrac[:4, mnum_idx, curi_idx]  # corresponds to Fortran gtlfrac(1:4, mnumpr, curiyr)
    cfftliq_slice = pu.cfftliq[:4, curi_idx]            # corresponds to Fortran cfftliq(1:4, curiyr)

    # compute the sum of gtlfrac slice
    gtlfrac_sum = np.sum(gtlfrac_slice)

    # perform calculation
    if gtlfrac_sum != 0.0:
        # compute weighted sum
        weighted_sum = np.dot(gtlfrac_slice, cfftliq_slice)
        pu.cfgtlliq[curi_idx] = weighted_sum / gtlfrac_sum
    else:
        # assign fallback value
        pu.cfgtlliq[curi_idx] = pu.cfdsq

    '''
      IF (sum(GTLFRAC(1:4,MNUMPR,CURIYR)) .NE. 0.0) THEN
         CFGTLLIQ(CURIYR) =(GTLFRAC(1,MNUMPR,CURIYR) * CFFTLIQ(1,CURIYR) + &
                            GTLFRAC(2,MNUMPR,CURIYR) * CFFTLIQ(2,CURIYR) + &
                            GTLFRAC(3,MNUMPR,CURIYR) * CFFTLIQ(3,CURIYR) + &
                            GTLFRAC(4,MNUMPR,CURIYR) * CFFTLIQ(4,CURIYR))/ &
                        sum(GTLFRAC(1:4,MNUMPR,CURIYR))
      ELSE
         CFGTLLIQ(CURIYR) = CFDSQ
      ENDIF    
    '''

def calc_CFIMUO(pyfiler, curiyr):
    """calculate the unfinished oils CFIMUO value, and update pyfiler.

    Parameters
    ----------
    pyfiler : object
        the restart file object
    curiyr : int
        Current NEMS year. For example, for year 1990, curiyr=1. For year 2023, curiyr=34=2023-1990+1.
    """    
    pu=pyfiler.utils
    mnumpr=pu.mnumpr    #10
    
    #!  unfinished oils
    if (pu.rfipqar3[mnumpr-1,curiyr-1,1]+pu.rfipqgo3[mnumpr-1,curiyr-1,1]+pu.rfipqmn3[mnumpr-1,curiyr-1,1]) != 0.0:
          pu.cfimuo[curiyr-1] =(pu.rfipqar3[mnumpr-1,curiyr-1,1]*pu.cfar3[curiyr-1]+ \
                           pu.rfipqgo3[mnumpr-1,curiyr-1,1]*pu.cfgo3[curiyr-1]+   \
                           pu.rfipqmn3[mnumpr-1,curiyr-1,1]*pu.cfmn3[curiyr-1])/  \
                     (pu.rfipqar3[mnumpr-1,curiyr-1,1]+pu.rfipqgo3[mnumpr-1,curiyr-1,1]+pu.rfipqmn3[mnumpr-1,curiyr-1,1])
    else:
        pu.cfimuo[curiyr-1] = pu.cfdsq

def api_to_btu(pyfiler, api_in, api_calc):
    """a function to return a million Btu/barrel conversion factor when sent an API gravity, any API gravity.
    If the passed-in calculation mode is not either 21 or 12, throws ValueError.

    Parameters
    ----------
    pyfiler : object
        the restart file object
    api_in : float
        the input API value
    api_calc : int
        the calculation mode (12 or 21)

    Returns
    -------
    float
        the output API value

    Raises
    ------
    ValueError
        the error will be threw when the passed-in calculation mode is not either 21 or 12
    """
    p=pyfiler.api_block
    '''
    includes/apiblk
    common /api_block/ api_grav,btu_per_gal,api_count
    '''
    match api_calc:
        case 21:    # mode 21: API to BTU intepolation
            btu_in = api_in / 42 * 1_000_000
            if btu_in > p.btu_per_gal[0]:
                btu_out = ((btu_in - p.btu_per_gal[0]) / (p.btu_per_gal[0] - p.btu_per_gal[1]) * 
                        (p.btu_per_gal[0] - p.btu_per_gal[1]) + p.btu_per_gal[0])
                api_out = ((btu_in - p.btu_per_gal[0]) / (p.btu_per_gal[0] - p.btu_per_gal[1]) * 
                        (p.api_grav[0] - p.api_grav[1]) + p.api_grav[0])
                return api_out
            elif btu_in < p.btu_per_gal[p.api_count - 1]:
                btu_out = ((btu_in - p.btu_per_gal[p.api_count - 1]) / 
                        (p.btu_per_gal[p.api_count - 2] - p.btu_per_gal[p.api_count - 1]) * 
                        (p.btu_per_gal[p.api_count - 2] - p.btu_per_gal[p.api_count - 1]) + 
                        p.btu_per_gal[p.api_count - 1])
                api_out = ((btu_in - p.btu_per_gal[p.api_count - 1]) / 
                        (p.btu_per_gal[p.api_count - 2] - p.btu_per_gal[p.api_count - 1]) * 
                        (p.api_grav[p.api_count - 2] - p.api_grav[p.api_count - 1]) + 
                        p.api_grav[p.api_count - 1])
                return api_out
            
            #for i in range(p.api_count - 1):
            for i in range(p.api_count):
                if btu_in <= p.btu_per_gal[i] and btu_in > p.btu_per_gal[i + 1]:
                    btu_out = ((btu_in - p.btu_per_gal[i + 1]) / (p.btu_per_gal[i] - p.btu_per_gal[i + 1]) * 
                            (p.btu_per_gal[i] - p.btu_per_gal[i + 1]) + p.btu_per_gal[i + 1])
                    api_out = ((btu_in - p.btu_per_gal[i + 1]) / (p.btu_per_gal[i] - p.btu_per_gal[i + 1]) * 
                            (p.api_grav[i] - p.api_grav[i + 1]) + p.api_grav[i + 1])
                    return api_out
       
        case 12:    # mode 12: BTU to API intepolation
            if api_in < p.api_grav[0]:
                #!  off table, extrapolate off low end
                api_out = ((p.api_grav[0] - api_in) / (p.api_grav[1] - p.api_grav[0]) * 
                        (p.api_grav[0] - p.api_grav[1]) + p.api_grav[0])
                btu_out = ((p.api_grav[0] - api_in) / (p.api_grav[1] - p.api_grav[0]) * 
                        (p.btu_per_gal[0] - p.btu_per_gal[1]) + p.btu_per_gal[0])
                return btu_out * 42 / 1_000_000
            elif api_in > p.api_grav[p.api_count - 1]:
                #!  off table, extrapolate off high end
                api_out = ((api_in - p.api_grav[p.api_count - 1]) / 
                        (p.api_grav[p.api_count - 1] - p.api_grav[p.api_count - 2]) * 
                        (p.api_grav[p.api_count - 1] - p.api_grav[p.api_count - 2]) + 
                        p.api_grav[p.api_count - 1])
                btu_out = ((api_in - p.api_grav[p.api_count - 1]) / 
                        (p.api_grav[p.api_count - 1] - p.api_grav[p.api_count - 2]) * 
                        (p.btu_per_gal[p.api_count - 1] - p.btu_per_gal[p.api_count - 2]) + 
                        p.btu_per_gal[p.api_count - 1])
                return btu_out * 42 / 1_000_000
            
            #for i in range(p.api_count - 1):
            for i in range(p.api_count):
                if api_in >= p.api_grav[i] and api_in < p.api_grav[i + 1]:
                    #!  within bounds of table, find place and interpolate
                    api_out = ((p.api_grav[i + 1] - api_in) / (p.api_grav[i + 1] - p.api_grav[i]) * 
                            (p.api_grav[i] - p.api_grav[i + 1]) + p.api_grav[i + 1])
                    btu_out = ((p.api_grav[i + 1] - api_in) / (p.api_grav[i + 1] - p.api_grav[i]) * 
                            (p.btu_per_gal[i] - p.btu_per_gal[i + 1]) + p.btu_per_gal[i + 1])
                    return btu_out * 42 / 1_000_000

        case _: # Default case (if API_CALC is not 21 or 12) 
            raise ValueError("Invalid API_CALC value provided.")


# Do NOT delete! keep the below settings for local debugging ----------
class User:
    def __init__(self):
        self.SCEDES={"DBDUMP":""}
        self.first_year=1990
        self.last_year=2050


if __name__ == "__main__":
    import pyfiler1
    pyfiler1.utils.read_filer('restart0206.unf')

    # Do NOT delete. Keep for debug:
    pyfiler1.ncntrl.curiyr = 34#35#1
    pyfiler1.convfact.histyr = 34 #34=2023-1990+1
    pyfiler1.ncntrl.curitr = 1

    user = User()
    user.SCEDES["DBDUMP"] = "1"
    pyfiler = docvfacts(pyfiler1, user)