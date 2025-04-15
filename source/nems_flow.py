from datetime import datetime
import os
import pickle
import sys
import time

import PyFiler.NEMSRestartIO as nemres


dll_dirs = ([r"C:\Program Files (x86)\Intel\oneAPI\compiler\2023.2.1\windows\redist\intel64_win\compiler",
             r"C:\Windows\System32"])

for i in dll_dirs:
    if i not in sys.path:
        sys.path.append(i)
        os.add_dll_directory(i)

for i in ["PyFiler", "hsm", "main", "ngpl", "converge", "ccats", "epm"]:
    j = os.path.join(os.getcwd(), i)
    if j not in sys.path:
        sys.path.append(j)


global pyfiler1
import pyfiler1
global pyfiler2
import pyfiler2


try:
    import run_epm as epm
except ModuleNotFoundError:
    pass
import prenems
import prenems_aimms
import nexec
import postnems

import nems_iteration_loop, imodel_helper
import check_nan_inf
import ngplprice

from logging_utilities import print_it

# Declaire File name used for Logging
MODULE_NAME = "nems_flow.py"


def nsolve(user):
    """Main entry point into NEMS year, iteration, and model loops. The following are executed as part of nsolve

    1. Set local variable to hold common used variable values

    2. NEMS Year Loop (CURIYR and CURCALYR)

        a. Set variables at the start of each 'Year' loop

        b. Reset variables at the start of each 'Year' loop

        c. Restart Point for Final Convergence and Reporting Loop

            i. Initialize Global Convergence Flag and Submodel Convergence Flags
                'CTEST' and 'cnvtst' MUST be reset to zero on first iteration AND after FCRL is set to 1 the first time

            ii. Convergence/Iteration Loop (CURITR)

                I. Print/Log the start of the new iteration loop

                II. Set 'cnvtst' to 1 after first iteration

                III. EPM Initialization and Read-in Data. Only Execute for First CURIYR and CURITR

                    1. Store End-Use and Other (GEO/MSW) Carbon Emissions to Use in ECP Row

                    2. CALL READ EPM Data

                IV. Model Loop (RUNMOD, NEMS_Modules)

                    1. TO STOP NEMS: Find a stop.txt file before the start of each model execution

                    2. Set variables at the start of each 'Model' loop

                    3. Print/Log the start of the new iteration loop

                    4. Write current NEMS model that will be executed to 'nems_run_status_log.txt' file

                    5. Enter function that setups foresight then runs NEMS model, will enter NEXEC

                    6. Check for NaN/Inf in pyfiler within model loop

                V. Call EPM After Model Loop. Computes carbon emission and implements some policy options.

                VI. Set CTEST to 0 if any model did NOT converge

                VII. Check if convergence test is stable for current and previous iteration

                VIII. Check for NaN/Inf in pyfiler within iteration loop, after model loop

                IX. Increment NEMS iteration number

            iii. Check CTEST. If not converging, print message and set FCRL = 1

        d. CALL NEMS Model in NCRL. Reporting loop for IDM, HSM, NGMM, CCATS models

        e. CALL EMISSION Model in NCRL. Computes carbon emission and implements some policy options.

        f. Code block for: SUMQAS, AVEPAS, COPY_ADJUSTED, AVEPASA, DOCVFACTS

    Parameters
    ----------
    user : SimpleNamespace
        Contains dictionary of NEMS runtime information
    
    """

    
    # 1. Set local variable to hold common used variable values
    BASEYR  = pyfiler1.utils.baseyr     # NEMS base year (e.g. 1990)
    CURIRUN = pyfiler1.utils.curirun    # Current cycle number (e.g. 2)
    NMODEL  = pyfiler1.utils.nmodel     # Total number of NEMS models to check for convergence (e.g. 14)
    MNUMYR  = pyfiler1.utils.mnumyr     # Number of NEMS years (e.g. 61)
    LASTYR  = pyfiler1.utils.lastyr     # Last forecast index year (e.g. 61)
    MAXITR  = pyfiler1.utils.maxitr     # Maximum number of iterations, not including FCRL and NCRL loop (e.g. 4)
    name_to_imodel_dict = imodel_helper.set_model_dicts()   # dict variable with model name key to imodel int pair (e.g. {'IEM':1, 'MAM':2})
    NEMS_Modules = user.module_order    # List of NEMS Models Activated (e.g. ['RDM', 'CDM', 'IDM', 'TDM'])


    #####################################
    # 2. NEMS Year Loop (CURIYR and CURCALYR)
    for current_integer_year in range(1, LASTYR+1):
        
        # Print/Log the start of the new year loop
        print_it(CURIRUN, f" ## Starting New Year Loop, CURIYR={current_integer_year}", MODULE_NAME)

        # a. Set variables at the start of each 'Year' loop
        pyfiler1.ncntrl.curiyr      = current_integer_year
        pyfiler1.ncntrl.curcalyr    = current_integer_year + BASEYR - 1

        # b. Reset variables at the start of each 'Year' loop
        pyfiler1.ncntrl.fcrl    = 0                     # Reset FINAL CONVERGENCE AND REPORTING LOOP SWITCH (0=OFF, 1=ON)
        pyfiler1.ncntrl.ncrl    = 0                     # Reset REPORTING LOOP SWITCH FOR EACH MODEL (0=OFF, 1=ON)
        current_iteration       = 1                     # Reset local variable to 1 for current iteration
        pyfiler1.ncntrl.curitr  = current_iteration     # Reset CURRENT ITERATION (e.g. 1)
        STABLE                  = False                 # Reset flag for iteratin (True/False)

        #####################################
        # c. Restart Point for Final Convergence and Reporting Loop
        while (not STABLE) and current_iteration <= MAXITR+1:

            # i. Initialize Global Convergence Flag and Submodel Convergence Flags
            # 'CTEST' and 'cnvtst' MUST be reset to zero on first iteration AND after FCRL is set to 1 the first time
            CTEST   =   0
            pyfiler1.utils.cnvtst[0:NMODEL,current_integer_year-1] = 0

            #####################################
            # ii. Convergence/Iteration Loop (CURITR)
            while (CTEST==0 and (current_iteration <= MAXITR or (current_iteration == MAXITR+1 and pyfiler1.ncntrl.fcrl == 1)) and pyfiler1.ncntrl.ncrl==0):
                
                # I. Print/Log the start of the new iteration loop
                print_it(CURIRUN, f" ##   Starting Iteration Loop,CURITR={current_iteration}", MODULE_NAME)

                # II. Set 'cnvtst' to 1 after first iteration
                if current_iteration >= 2:
                    pyfiler1.utils.cnvtst[:,current_integer_year-1]=1


                #####################################
                # III. EPM Initialization and Read-in Data. Only Execute for First CURIYR and CURITR
                if current_integer_year == 1 and current_iteration == 1:

                    # 1. Store End-Use and Other (GEO/MSW) Carbon Emissions to Use in ECP Row
                    pyfiler1.emission.ecp_scar[0:MNUMYR] = pyfiler1.emission.emrsc[10,0,0:MNUMYR] + pyfiler1.emission.emcmc[10,0,0:MNUMYR] + pyfiler1.emission.emincc[10,0,0:MNUMYR] +  pyfiler1.emission.emtrc[10,0,0:MNUMYR] + pyfiler1.emission.emnt[10,0,0:MNUMYR] * 0.001
                    pyfiler1.emission.ecp_ocar[0:MNUMYR] = pyfiler1.emission.emel[3,0,0:MNUMYR]

                    #####################################
                    # 2. CALL READ EPM Data
                    print_it(CURIRUN, "run epm read", MODULE_NAME)
                    epm.run_epm(epm.Mode.READ, pyfiler1)
                    # End of 2. CALL READ EPM Data
                    #####################################

                # End of III. EPM Initialization and Read-in Data. Only Execute for First CURIYR and CURITR
                #####################################
                
                #####################################
                # IV. Model Loop (RUNMOD, NEMS_Modules)
                for current_nems_module in NEMS_Modules:

                    # 1. TO STOP NEMS: Find a stop.txt file before the start of each model execution
                    if os.path.isfile('stop.txt'):
                        print_it(CURIRUN, "STOP NEMS by User", MODULE_NAME)
                        with open('nems_run_status_log.txt', 'a') as ns:
                            status = f'module: STOP, cycle: {pyfiler1.cycleinfo.curirun}, year:{pyfiler1.ncntrl.curcalyr}, iteration:{current_iteration}, FCRL:{pyfiler1.ncntrl.fcrl}\n'
                            print_it(CURIRUN, status, MODULE_NAME)
                            ns.write(status)
                        os.sys.exit()
                    
                    # 2. Set variables at the start of each 'Model' loop
                    imodel=imodel_helper.get_imodel(name_to_imodel_dict,current_nems_module)
                    loop_info={'module':current_nems_module,'year':current_integer_year,'iter':current_iteration,'imodel':imodel}
                    
                    # 3. Print/Log the start of the new iteration loop
                    print_it(CURIRUN, f'      Submodel Loop, IMODEL={imodel_helper.get_subr_name(name_to_imodel_dict, imodel)}', MODULE_NAME)

                    # 4. Write current NEMS model that will be executed to 'nems_run_status_log.txt' file
                    with open('nems_run_status_log.txt', 'a') as ns:
                        status = f'module:{current_nems_module}, cycle: {pyfiler1.cycleinfo.curirun}, year:{pyfiler1.ncntrl.curcalyr}, iteration:{current_iteration}, FCRL:{pyfiler1.ncntrl.fcrl}\n'
                        print_it(CURIRUN, status, MODULE_NAME)
                        ns.write(status)

                    # 5. Enter function that setups foresight then runs NEMS model, will enter NEXEC
                    nems_iteration_loop.update_expectation_vars(pyfiler1, pyfiler2, loop_info, user)

                    # 6. Check for NaN/Inf in pyfiler within model loop
                    if int(user.SCEDES["CHKNAN"]) == 2 and pyfiler1.ncntrl.curcalyr > int(user.SCEDES['CLBASEYR']):
                        msg = f'going to check NaN/inf. module:{current_nems_module}, cycle: {pyfiler1.cycleinfo.curirun}, year:{pyfiler1.ncntrl.curcalyr}, iteration:{current_iteration}, FCRL:{pyfiler1.ncntrl.fcrl}\n'
                        print_it(CURIRUN, msg, MODULE_NAME)
                        check_nan_inf.main_check_nan_inf(pyfiler1)

                # End of IV. Model Loop (RUNMOD, NEMS_Modules)
                #####################################

                #####################################
                # V. Call EPM After Model Loop. Computes carbon emission and implements some policy options.
                if user.SCEDES["RUNEPM"] > "0":
                    print_it(CURIRUN, f' ## Executing EPM After Model Loop, CURITR={current_iteration}', MODULE_NAME)
                    epm.run_epm(epm.Mode.MAIN, pyfiler1)
                else:
                    print_it(CURIRUN, f' ## NOT Executing EPM After Model Loop, CURITR={current_iteration}', MODULE_NAME)
                    print_it(CURIRUN, f' ## SCEDES value set to, RUNEPM={user.SCEDES["RUNEPM"]}', MODULE_NAME)
                    
                #####################################

                #####################################
                # VI. Set CTEST to 0 if any model did NOT converge
                CTEST=1
                for i in range(0,NMODEL):
                    if pyfiler1.utils.cnvtst[i,current_integer_year-1]==0: CTEST=0
                #####################################

                # VII. Check if convergence test is stable for current and previous iteration
                STABLE = nems_iteration_loop.act_ctest(CTEST, pyfiler1, MAXITR, STABLE, current_iteration)
                

                # VIII. Check for NaN/Inf in pyfiler within iteration loop, after model loop
                if int(user.SCEDES["CHKNAN"]) == 1 and pyfiler1.ncntrl.curcalyr > int(user.SCEDES['CLBASEYR']):
                    msg = f'going to check NaN/inf. cycle: {pyfiler1.cycleinfo.curirun}, year:{pyfiler1.ncntrl.curcalyr}, iteration:{current_iteration}, FCRL:{pyfiler1.ncntrl.fcrl}\n'
                    print_it(CURIRUN, msg, MODULE_NAME)
                    check_nan_inf.main_check_nan_inf(pyfiler1)

                # IX. Increment NEMS iteration number
                current_iteration += 1
                pyfiler1.ncntrl.curitr = current_iteration

            # End of ii. Convergence/Iteration Loop (CURITR)
            #####################################
            
            #####################################
            # iii. Check CTEST. If not converging, print message and set FCRL = 1
            if CTEST==0 and current_iteration <= MAXITR+1 and pyfiler1.ncntrl.fcrl==0:
                print_it(CURIRUN, '##   Maximum iterations reached without converging.', MODULE_NAME)
                print_it(CURIRUN, '##   Entering Final Convergence and Reporting Loop', MODULE_NAME)
                print_it(CURIRUN, '##   Setting FCRL=1', MODULE_NAME)
                pyfiler1.ncntrl.fcrl=1
        
        # End of c. Restart Point for Final Convergence and Reporting Loop
        #####################################
        
        #####################################
        # d. CALL NEMS Model in NCRL. Reporting loop for IDM, HSM, NGMM, CCATS models
        pyfiler1.ncntrl.ncrl=1
        print_it(CURIRUN, f' ## Executing modules set for NCRL reporting loop, CURITR={current_iteration}', MODULE_NAME)
        if pyfiler1.ncntrl.runmod[4]==1: nexec.nexec2(pyfiler1,user,current_integer_year,5, 'IDM')
        if pyfiler1.ncntrl.runmod[8]==1: nexec.nexec2(pyfiler1,user,current_integer_year,9, 'HSM')
        if pyfiler1.ncntrl.runmod[9]==1: nexec.nexec2(pyfiler1,user,current_integer_year,10, 'NGMM')
        if pyfiler1.ncntrl.runmod[13]==1: nexec.nexec2(pyfiler1,user,current_integer_year,14, 'CCATS')
        # End of d. CALL NEMS Model in NCRL. Reporting loop for IDM, HSM, NGMM, CCATS models
        #####################################

        #####################################
        # e. CALL EMISSION Model in NCRL. Computes carbon emission and implements some policy options.
        if user.SCEDES["RUNEPM"] > "0":
            print_it(CURIRUN, f' ## Executing EPM in NCRL reporting loop, CURITR={current_iteration}', MODULE_NAME)
            epm.run_epm(epm.Mode.MAIN, pyfiler1)
        else:
            print_it(CURIRUN, f' ## NOT Executing EPM in NCRL reporting loop, CURITR={current_iteration}', MODULE_NAME)
            print_it(CURIRUN, f' ## SCEDES value set to, RUNEPM={user.SCEDES["RUNEPM"]}', MODULE_NAME)
        # End of e. CALL EMISSION Model in NCRL. Computes carbon emission and implements some policy options.
        #####################################


        #####################################
        # f. Code block for: SUMQAS, AVEPAS, COPY_ADJUSTED, AVEPASA, DOCVFACTS
        #! +++ Compute ALL-SECTOR totals for NEMS QUANTITY variables
        pyfiler1.sumqas()
        #! +++ Compute ALL-SECTOR averages for NEMS PRICE variables
        pyfiler1.avepas()
        #! +++ COPY UNADJUSTED PRICES to ADJUSTED PRICES, ADDING ANY POLICY-RELATED TAXES (eg CARBON)
        pyfiler1.copy_adjusted()
        #! +++ COMPUTE ALL-SECTOR AVERAGES FOR NEMS ADJUSTED PRICE VARIABLES FOR EPM
        pyfiler1.avepasa()
        #! +++ CALCULATES SOME WEIGHTED AVERAGE CONVERSION FACTORS
        pyfiler1.cvhistyr.histyr = pyfiler1.convfact.histyr
        pyfiler1.docvfacts()
        # End of f. Code block for: SUMQAS, AVEPAS, COPY_ADJUSTED, AVEPASA, DOCVFACTS
        #####################################


def main(arg_curirun=1):
    """main entry point into NEMS

    1. Print the Cycle number and remove "done.txt" from folder if generated from previous cycle

    2. Initiate prenems function to setup NEMS run

    3. Check restart file variables for any NaN or inf

    4. Save user namespace variable as pickle file for potential post-run examination

    5. Executes NEMS main function (NSOLVE) to run models in year, iteration, and model loops

    6. Execute postnems function to write out restart files, final accounting, and close out any models

    7. Write a "done.txt" file that cycle.py looks for to ensure NEMS completed the cycle successfully

    Parameters
    ----------
    arg_curirun : int, optional
        Current Cycle Number

    """

    # Record start file for NEMS run
    MODEL_START_TIME = time.time()


    # 1. Print the Cycle number onto the terminal and logs
    curirun = int(arg_curirun)
    print_it(curirun, f">>>> NEMS CYCLE {curirun} <<<<", MODULE_NAME)

    # 1. Remove "done.txt" from folder if generated from previous cycle
    if os.path.exists("done.txt"):
        os.remove("done.txt")

    # 2. Initiate prenems function to setup NEMS run
    user = prenems.nems_setup(pyfiler1, pyfiler2, arg_curirun)
    user = prenems_aimms.fill_aimms_user_items(user, pyfiler1)
    user.MODEL_START_TIME = MODEL_START_TIME

    # 3. Check restart file variables for any NaN or inf
    if int(user.SCEDES["CHKNAN"]) in (1,2):
        msg = f'going to check NaN/inf. We are in NEW CYCLE {curirun}\n'
        print_it(curirun, msg, MODULE_NAME)
        check_nan_inf.main_check_nan_inf(pyfiler1)

    # 4. Save user namespace variable as pickle file for potential post-run examination
    with open('user.pickle', 'wb') as f:
        pickle.dump(user, f, pickle.HIGHEST_PROTOCOL)

    # 5. Executes NEMS main function (NSOLVE) to run models in year, iteration, and model loops
    nsolve(user)

    # 6. Execute postnems function to write out restart files, final accounting, and close out any models
    postnems.nems_post(pyfiler1, pyfiler2, user)

    # 7. Write a "done.txt" file that cycle.py looks for to ensure NEMS completed the cycle successfully
    with open('done.txt', 'w') as dummy_name:
        pass


if __name__ == "__main__":
    
    # Enter this IF block when nems_flow.py is executed by cycle.py. Curirun.txt is created by cycle.py
    if os.path.isfile("curirun.txt"):
        with open("curirun.txt", "r") as f:
            z = f.readline()    # Assign cycle number
        assert z.isdigit()
    # Enter this IF block when nems_flow.py is executed in debug mode outside of cycle.py
    else:
        z = 1   # Assign cycle number

    main(int(z))