# -*- coding: utf-8 -*-
"""
Created on Mar 13 2024

@author: Claire Su
"""

import nexec
import PyfilerToHDF5
import copy
import conv_main
import HDF5topyfiler
from logging_utilities import print_it

MODULE_NAME = "nems_iteration_loop.py"

def update_expectation_vars(pyfiler1, pyfiler2, loop_info, user):
    """Runs accounting functions for price and quantity variables before/after executing any NEMS model and calls to NEMS models

    This function executes a series of accounting functions for price and quantity variables that includes establishing a 
    starting point using data from the previous NEMS year and preprocessing data for iteration convergence test. This function 
    then places a call to the NEXEC function that executes the NEMS model. After the NEMS model completes the execution, a 
    series of post processing is executed including recalculating the national totals by summing all regions. This function 
    also places a call to the iteration convergence test to check if the model has converged. Finally, this function also includes 
    a call to the iteration relax function that weigh averages the value of the current price and quantity variables using values 
    from current and previous iteration. The following summarizes the major steps for this function:

    1. Assign commonly used variables to local variables.

    2. Updates the expectation variables for CURIYR and beyond by copying previous year value to start.
        Only execute on first iteration (current_iteration == 1)
    3. Store previous iteration values for next convergence check.

    4. Inter-Iteration Convergence Code - Before Running NEXEC, save convergence variable values as dictionary
        Only execute after first iteration and after year 2021 (current_iteration > 1 and curiyr >= 32)
    5. Call to NEMS models.
        Executing NEXEC function to call NEMS model
    6. Recalculate national totals for PBMET and QBMET to ensure they add up.

    7. Set up the array for convergence testing of non-PQ items. Assign new variables to be tested.

    8. Executing iteration convergence and iteration relax code

    9. Code block for: SUMQAS, AVEPAS, COPY_ADJUSTED, AVEPASA, DOCVFACTS

    Parameters
    ----------
    pyfiler1 : module
        pyfiler fortran module

    pyfiler2 : module
        pyfiler fortran module

    loop_info : dict
        contains information on the current NEMS model running: Module Name (e.g. IDM), Integer Year (e.g. 42), 
        Iteration number (e.g. 2), Integer model number (e.g. 5)
    
    user : SimpleNamespace
        user class object, containing dict with NEMS
    """
    #####################################
    # 1. Code block to Assign commonly used variables to local variables
    current_nems_module     = loop_info['module']       # Current NEMS model name running this loop (e.g. IDM)
    current_iteration       = loop_info['iter']         # Current Iteration Number (e.g. 2)
    imodel                  = loop_info['imodel']       # Current NEMS model integer running this loop (e.g. 5)
    curiyr                  = pyfiler1.ncntrl.curiyr    # Current Integer Year (e.g. 42)

    mnump   = pyfiler1.utils.mnump  # Number of NEMS price variables
    mnumq   = pyfiler1.utils.mnumq	# Number of NEMS quntity variables
    mnoth   = pyfiler1.utils.mnoth  # Number of miscellaneous other convergence variables

    mnumpr  = pyfiler1.utils.mnumpr # Petroleum Administration for Defence Districts (PADD) Regions
    mnumor  = pyfiler1.utils.mnumor # Oil & Gas Regions
    mnumcr  = pyfiler1.utils.mnumcr # Census Regions
    nngem   = pyfiler1.utils.nngem  # Natural gas - Electricity Regions
    ndrgg   = pyfiler1.utils.ndrgg  # Coal - Electricity Regions
    
    nclut1      = pyfiler1.utils.nclut1         # COAL QUANTS
    mx_ncoals   = pyfiler1.utils.mx_ncoals      # Maximum Number U.S. Coal Supply Curves   
    mx_units    = pyfiler1.utils.mx_units       # COAL GENERATION UNITS

    # for recalculating national totals for PBMET and QBMET to ensure they add up
    numethq=pyfiler1.utils.numethq              # Number of ethanol steps PMM passes to RFM

    # set abbreviations
    ppr     = pyfiler1.prev     # access to 'prev' commonblock
    pwr     = pyfiler1.wrenew   # access to 'wrenew' commonblock

    # End of 1. Assign commonly used variables to local variables
    #####################################


    #####################################
    # 2. Update expectations variables for the current year. (Copy current P,Q to expectations variables.)
    if current_iteration == 1: pyfiler1.nxpect_run.go_nxpect(imodel)
    # End of 2. Update expectations variables for the current year. (Copy current P,Q to expectations variables.)
    #####################################
    

    #####################################
    # 3. Code block to Store previous iteration values for next convergence check.
    # MUST USE UNADJUSTED PRICES, SINCE WE TEST THEM
    for ir in range(0, mnumcr):
        ppr.prevp[ir,curiyr-1,0:mnump]=pyfiler1.utils.mprc[ir,curiyr-1,0:mnump]
        ppr.prevq[ir,curiyr-1,0:mnumq]=pyfiler1.utils.mqty[ir,curiyr-1,0:mnumq]

    #!NGMM  REGION
    for ir in range(0,nngem):
        #! UTIL GAS P,Q
        ppr.prevup[ir,curiyr-1,0:3]=pyfiler1.utils.muprc[ir,curiyr-1,0:3]
        ppr.prevuq[ir,curiyr-1,0:3]=pyfiler1.utils.muqty[ir,curiyr-1,0:3]

        # seasonal natural gas to util
        ppr.prevup[ir,curiyr-1,3:7]=pyfiler1.utils.smuprc[ir,curiyr-1,0:4]
        ppr.prevuq[ir,curiyr-1,3:7]=pyfiler1.utils.smuqty[ir,curiyr-1,0:4]

        #! seasonal (3 of them) electric power GAS P,Q
        ppr.prevgasp[ir,curiyr-1,0:3]=pyfiler1.utils.sgasprice[ir,curiyr-1,0:3]
        ppr.prevgasq[ir,curiyr-1,0:3]=pyfiler1.utils.sgasquant[ir,curiyr-1,0:3]        

    #!COAL REGION
    for ir in range(0,ndrgg):
        ppr.prevcps[0,ir,curiyr-1]=pyfiler1.utils.pclelcdr[0,ir,curiyr-1]
        ppr.prevcps[1,ir,curiyr-1]=pyfiler1.utils.pclelcdr[1,ir,curiyr-1]

        #! UTIL COAL QUANTS
        ppr.prevcqs[ir,curiyr-1,0:nclut1]=pyfiler1.utils.qclclnr[ir,curiyr-1,0:nclut1]

    ppr.convoth[0, 0:mnoth, curiyr - 1] = ppr.convoth[1, 0:mnoth, curiyr - 1]

    # COAL SUPPLY CURVES
    ppr.prevpcap[0:mx_ncoals,curiyr-1] = pyfiler1.uso2grp.xcl_pcap[0:mx_ncoals,curiyr-1]

    # COAL GENERATION UNITS
    ppr.prev_cl_cf[0:mx_units,curiyr-1] = pyfiler1.coalemm.emm_cl_cf[0:mx_units,curiyr-1]
    ppr.prev_cl_btus[0:mx_units,curiyr-1] = pyfiler1.coalemm.emm_cl_btus[0:mx_units,curiyr-1]
    # End of 3. Code block to Store previous iteration values for next convergence check.
    #####################################


    #####################################
    # 4. Inter-Iteration Convergence Code - Before Running NEXEC, save convergence variable values as dictionary
    if current_iteration > 1 and curiyr >= 32:
        datminus1 = copy.deepcopy(PyfilerToHDF5.main(pyfiler1,current_iteration,curiyr,user.CONV_VAR))
    # End of 4. Inter-Iteration Convergence Code - Before Running NEXEC, save convergence variable values as dictionary
    #####################################


    #####################################
    # 5. Call to NEMS models
    nexec.nexec(imodel, pyfiler1, pyfiler2, curiyr, current_iteration, user, current_nems_module)
    # End of 5. Call to NEMS models
    #####################################


    #####################################
    # 6. Recalculate national totals for PBMET and QBMET to ensure they add up
    pwr.qbmet[mnumcr-1,curiyr-1,0:numethq] = sum(pwr.qbmet[0:9,curiyr-1,0:numethq])
    pwr.pbmet[mnumcr-1,curiyr-1,0:numethq] = 0.0
    for iv in range(0,numethq):
        for ir in range(0,9):
            if pwr.qbmet[mnumcr-1,curiyr-1,iv] != 0: 
                pwr.pbmet[mnumcr-1,curiyr-1,iv] = pwr.pbmet[mnumcr-1,curiyr-1,iv] + pwr.qbmet[ir,curiyr-1,iv] * pwr.pbmet[ir,curiyr-1,iv] / pwr.qbmet[mnumcr-1,curiyr-1,iv]
    # End of # 6. Recalculate national totals for PBMET and QBMET to ensure they add up
    #####################################


    #####################################
    # 7. Set up the array for convergence testing of non-PQ items. Assign new variables to be tested.
    ppr.convoth[1, 0, curiyr - 1] = pyfiler1.macout.mc_gdpr[curiyr - 1]
    ppr.convoth[1, 1, curiyr - 1] = pyfiler1.intout.it_wop[curiyr - 1, 0]
    ppr.convoth[1, 2, curiyr - 1] = pyfiler1.pmmrpt.rfqicrd[mnumpr - 1, curiyr - 1]
    ppr.convoth[1, 3, curiyr - 1] = pyfiler1.pmmrpt.rfpqiprdt[mnumpr - 1, curiyr - 1, 1]
    ppr.convoth[1, 4, curiyr - 1] = pyfiler1.emission.emetax[0, curiyr - 1]
    ppr.convoth[1, 5, curiyr - 1] = pyfiler1.emission.emelpso2[curiyr - 1, 0]
    ppr.convoth[1, 6, curiyr - 1] = pyfiler1.emission.ecp_phg[0, curiyr - 1]
    ppr.convoth[1, 7, curiyr - 1] = pyfiler1.ngtdmrep.ogwprng[mnumor - 1, curiyr - 1]
    ppr.convoth[1, 8, curiyr - 1] = pyfiler1.ogsmout.ogcnpprd[0, curiyr - 1]
    ppr.convoth[1, 9, curiyr - 1] = pyfiler1.ogsmout.ogcnpprd[1, curiyr - 1]
    ppr.convoth[1, 20:28+1, curiyr - 1] = pyfiler1.wrenew.pbmet[0:8+1, curiyr - 1, 0]
    ppr.convoth[1, 29, curiyr - 1] = pyfiler1.wrenew.pbmet[mnumcr - 1, curiyr - 1, 0]
    ppr.convoth[1, 30:38+1, curiyr - 1] = pyfiler1.wrenew.qbmet[0:8+1, curiyr - 1, 0]
    ppr.convoth[1, 39, curiyr - 1] = pyfiler1.wrenew.qbmet[mnumcr - 1, curiyr - 1, 0]
    ppr.convoth[1, 42, curiyr - 1] = pyfiler1.pmmout.glbcrddmd[curiyr - 1]
    ppr.convoth[1, 43:53+1, curiyr - 1] = pyfiler1.emission.ecp_pso2[0:10+1, curiyr - 1, 0]
    ppr.convoth[1, 54:64+1, curiyr - 1] = pyfiler1.emission.ecp_pso2[0:10+1, curiyr - 1, 1]
    # End of 7. Set up the array for convergence testing of non-PQ items. Assign new variables to be tested.
    #####################################


    #####################################
    # 8. Code block for running iteration convergence and relax code
    if current_iteration > 1 and curiyr >= 32:

        # Copy convergence variables from CURRENT iteration from pyfiler into local variable as dict
        datcur = copy.deepcopy(PyfilerToHDF5.main(pyfiler1,current_iteration,pyfiler1.ncntrl.curcalyr,user.CONV_VAR))
        # Executes iteration convergence and relax function
        df_conv_res, dat_upd = conv_main.main(datminus1, datcur, pyfiler1.cycleinfo.curirun, pyfiler1.ncntrl.fcrl, current_nems_module, user.CONV_DAT)
        # Writes relaxed values back into pyfiler
        HDF5topyfiler.values_back_to_pyfiler(pyfiler1, dat_upd)
        # Retrieve the index for current NEMS model running. Fortran to Python index, subtract 1
        model_index_number = df_conv_res.index[df_conv_res['Model'] == current_nems_module].tolist()[0] - 1
        # Write the convergence status (0 for fail, 1 for pass) to corresponding model and integer year index
        pyfiler1.utils.cnvtst[model_index_number,curiyr-1] = df_conv_res['CNVTST'].values[-1]
    # End of 8. Code block for running iteration convergence and relax code
    #####################################


    #####################################
    # 9. Code block for: SUMQAS, AVEPAS, COPY_ADJUSTED, AVEPASA, DOCVFACTS
    #! +++ Compute ALL-SECTOR totals for NEMS QUANTITY variables
    pyfiler1.sumqas()
    #! +++ Compute ALL-SECTOR averages for NEMS PRICE variables
    pyfiler1.avepas()   
    #! +++ COPY UNADJUSTED PRICES to ADJUSTED PRICES, ADDING ANY POLICY-RELATED TAXES (eg CARBON)
    pyfiler1.copy_adjusted()
    #! +++ COMPUTE ALL-SECTOR AVERAGES FOR NEMS ADJUSTED PRICE VARIABLES FOR EPM
    pyfiler1.avepasa()
    #! +++ CALCULATES SOME WEIGHTED AVERAGE CONVERSION FACTORS
    pyfiler1.docvfacts()
    # End of 9. Code block for: SUMQAS, AVEPAS, COPY_ADJUSTED, AVEPASA, DOCVFACTS
    #####################################


def act_ctest(CTEST, pyfiler, MAXITR, STABLE, current_iteration):
    """Check to switch FCRL flag (0 or 1) or STABLE flag (False or True) depending on iteration convergence check

    This function assesses the iteration convergence results from CURRENT and PREVIOUS iteration loop to determine if
    the next iteration should enter the final loop (switch FCRL = 1), if there were 2 subsequent iterations that passed
    the convergence test and no more iterations are needed (switch STABLE = True), more iterations are required because
    CURRENT iteration Failed the convergence test even if PREVIOUS iteration Passed (switch FCRL = 0).

    The following summarizes the logic:

    1. Check if iteration convergence FAILED for CURRENT iteration (CTEST == 0)
        a. Check if current iteration is less than or equal to MAXITR AND if FCRL == 1
            - FCRL flag was 1 (iteration convergence PASSED the PREVIOUS iteration)
            - CURITR <= MAXITR (current iteration has not exceeded maximum iteration)
            - CTEST was 0 (iteration convergence FAILED for THIS iteration)
            - Action: Switch the FCRL flag back to 0
        b. If currention exceeds MAXITR and if FCRL == 1
            - CTEST was 0 (iteration convergence FAILED for THIS iteration)
            - Current iteration exceeded the MAXITR
            - Action: No action
    2. If iteration convergence passed for CURRENT iteration (CTEST != 0)
        a. Check if FCRL == 1
            - FCRL flag was 1 (iteration convergence PASSED the PREVIOUS iteration)
            - CTEST flag was NOT 0 (iteration convergence PASSED the CURRENT iteration)
            - Action: Assign True to variable 'STABLE'
        b. if FCRL != 1
            - FCRL flag was NOT 1 (iteration convergence FAILED the PREVIOUS iteration or current iteration is 2)
            - CTEST flag was NOT 0 (iteration convergence PASSED the CURRENT iteration)
            - Action: Assign FCRL flag as 1

    Parameters
    ----------
    CTEST : int
        (0 or 1), 0 if iteration convergence check does NOT pass. 1 if iteration convergence check passes.
    pyfiler : module
        pyfiler fortran module
    MAXITR : int
        Maximum iteration to run per NEMS year. Setting input from SCEDES file 
    STABLE : bool
        Flag to determine if model is stable for this iteration (TRUE if stable, FALSE if not stable)
    current_iteration : int
        current iteration number

    Returns
    -------
    bool
        True if model is stable and ready to exit out of the iteration loop even if current iteration is less than maxitr.
        False if model is not stable.
    """

    # Assign Commonly used variable to local local variable
    CURIRUN = pyfiler.cycleinfo.curirun     # Current NEMS Cycle Number

    # Check if CURRENT iteration convergence failed (CTEST == 0)
    if CTEST==0:
        # Check if current iteration is less than or equal to MAXITR and the FCRL flag = 1 (previous iteration convergence passed)
        if current_iteration <= MAXITR and pyfiler.ncntrl.fcrl == 1:
            print_it(CURIRUN, f'##  Out of convergence on  Final Convergence and Reporting Loop:  Year= {pyfiler.ncntrl.curcalyr}, Iteration= {current_iteration}', MODULE_NAME)
            print_it(CURIRUN, ' ##  Resetting FCRL=0 and continuing.', MODULE_NAME)
            pyfiler.ncntrl.fcrl=0
        elif pyfiler.ncntrl.fcrl==1:
            print_it(CURIRUN, f'##  Not converged on Final Convergence and Reporting Loop. CURIYR={current_iteration}', MODULE_NAME)
            print_it(CURIRUN, ' ##  Will not continue because the maximum iterations have been performed.', MODULE_NAME)
    
    # If CURRENT iteration convergence passed (CTEST != 0) for CURRENT iteration
    else:

        # Check if PREVIOUS iteration convergence passed, FCRL == 1
        if pyfiler.ncntrl.fcrl==1:
            print_it(CURIRUN, f'##   Convergence maintained on Final Convergence and Reporting Loop.  CURIYR={current_iteration}', MODULE_NAME)
            STABLE=True
        else:
            print_it(CURIRUN, f'##   Convergence achieved on iteration {current_iteration}', MODULE_NAME)
            print_it(CURIRUN, ' ##   Entering Final Convergence and Reporting Loop.  Setting FCRL=1', MODULE_NAME)
            pyfiler.ncntrl.fcrl=1
    
    return STABLE