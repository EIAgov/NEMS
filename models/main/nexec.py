from datetime import datetime
import os
import time
import aimms_wrapper
import pyd_datatransfer
from logging_utilities import print_it

try:
    import hsm
except ModuleNotFoundError:
    pass

try:
    import ccats
except ModuleNotFoundError:
    pass

try:
    from idm import idm_h2_accounting
except ModuleNotFoundError:
    pass


MODULE_NAME = "nexec.py"


def process_aimms_wrapper_return(my_result, pyfiler1, current_nems_module):
    """Checks if the AIMMS process completed successfully, else stop the NEMS process

    Check the AIMMS model return status. If status is "success", the NEMS run continues. If the
    status is "stop", "failure", or unknown, a message is printed onto the terminal with details
    including the model name, year, iteation, and FCRL flag written into the log file.

    Parameters
    ----------
    my_result : str
        return status message from the completed AIMMS process (e.g. success, stop, failure)

    pyfiler1 : module
        pyfiler fortran module

    current_nems_module : str
        name of the current AIMMS model that just completed
    """

    # Assigns frequently used variables to local variables
    CURIRUN = pyfiler1.cycleinfo.curirun
    CURITR = pyfiler1.ncntrl.curitr

    # Checks to ensure AIMMS model status is returned "success"
    if my_result == "success":
        pass

    # If return status is not "success"
    else:
        # Prints message the AIMMS process was terminated by user
        if my_result == "stop":
            print_it(CURIRUN, "STOP NEMS by User", MODULE_NAME)
            with open('nems_run_status_log.txt', 'a') as ns:
                status = f'module:STOP, cycle: {CURIRUN}, year:{pyfiler1.ncntrl.curcalyr}, iteration:{CURITR}, FCRL:{pyfiler1.ncntrl.fcrl}\n'
                print_it(CURIRUN, status, MODULE_NAME)
                ns.write(status)

        # Prints message the AIMMS process did not complete as expected
        elif my_result == "failure":
            print_it(CURIRUN, "AIMMS failed", MODULE_NAME)
            with open('nems_run_status_log.txt', 'a') as ns:
                status = f'module:{current_nems_module}, cycle: {CURIRUN}, year:{pyfiler1.ncntrl.curcalyr}, iteration:{CURITR}, FCRL:{pyfiler1.ncntrl.fcrl}\n'
                print_it(CURIRUN, status, MODULE_NAME)
                ns.write(status)
        
        # Prints message the AIMMS process return status is unknown
        else:
            status = f"unknown or unexpected result from aimms_wrapper.run_module: {my_result}"
            print_it(CURIRUN, status, MODULE_NAME)
        
        # Stop NEMS run if stopped the return status is not "success"
        os.sys.exit()


def nexec(IMODEL, pyfiler1, pyfiler2, current_integer_year, current_iteration, user, current_nems_module):
    """Executes the call to individual NEMS models

    Calls to individual NEMS models and is called between iteration = 1 to MAXITR (specified in scedes file)
    plus one final loop when FCRL flag = 1

    Parameters
    ----------
    IMODEL : int
        Corresponding number to NEMS model
        1 = WORLD, 2 = MAC, 3 = RESD, 4 = COMM, 5 = IND, 6 = TRAN, 7 = UTIL, 8 = COAL,
        9 = WELL, 10 = PIPE, 11 = REFINE, 12 = RENEW, 13 = HMM, 14 = CCATS
    
    pyfiler1 : module
        pyfiler fortran module
    
    pyfiler2 : module
        pyfiler fortran module
    
    current_integer_year : int
        Corresponding integer to NEMS calendar year (e.g. 1 = 1990)
    
    current_iteration : int
        NEMS iteration number
    
    user : SimpleNamespace
        Contains dictionary of NEMS runtime information
    current_nems_module : str
        Corresponding NEMS model name to IMODEL integer
    
    Returns
    -------
    None.

    """
    
    
    # Assign commonly used variables to local variables
    my_cycle = pyfiler1.cycleinfo.curirun
    current_cal_year = 1989 + current_integer_year
    # Print current NEMS run information onto terminal: Current Calendar year, current iteration, current model
    s = f" ---- Cal Yr: {current_cal_year}, Cur Iter: {current_iteration}, Module: {current_nems_module}"
    print_it(my_cycle, s, MODULE_NAME)

    # Sums to total number of models for the NEMS Run.If the model is 
    # an AIMMS model with python wrapper (CMM, NGMM, or HMM) and is running 
    # solo without any other model on, a time.sleep() is required for model stability.
    number_of_modules = int(user.SCEDES["EXW"]) + \
                        int(user.SCEDES["EXM"]) + \
                        int(user.SCEDES["EXR"]) + \
                        int(user.SCEDES["EXK"]) + \
                        int(user.SCEDES["EXT"]) + \
                        int(user.SCEDES["EXI"]) + \
                        int(user.SCEDES["EXC"]) + \
                        int(user.SCEDES["EXE"]) + \
                        int(user.SCEDES["EXG"]) + \
                        int(user.SCEDES["EXL"]) + \
                        int(user.SCEDES["EXO"]) + \
                        int(user.SCEDES["EXN"]) + \
                        int(user.SCEDES["EXH"]) + \
                        int(user.SCEDES["EXQ"])

    
    # Timer for NEMS model start time
    mod_start_time = time.time()

    # Match function to determine which NEMS model to run
    match IMODEL:
        
        # Call to IEM model
        case 1: pyfiler1.world_run.go_world()

        # Call to MAM model
        case 2: pyfiler1.mac_run.go_mac()

        # Call to RDM model
        case 3: pyfiler1.rdm_run.go_rdm()

        # Call to CDM model
        case 4: pyfiler1.cdm_run.go_cdm()

        # Call to IDM model
        case 5: pyfiler1.ind_run.go_ind()

        # Call to TDM model
        case 6: pyfiler1.tran_run.go_tran()

        # Call to EMM model
        case 7:
            # copies all data in exposed commonblocks from pyfiler1 to pyfiler2
            swap_pyd1to2(pyfiler1, pyfiler2)
            
            # turn on/off EPHRTS and its debugger before rujning EMM
            pyfiler2.emm_run.ephrts_on_off(int(user.SCEDES["EPHRTS"]),int(user.SCEDES["EPHRTSDB"]))

            # copies all data in exposed commonblocks from pyfiler2 to pyfiler1
            swap_pyd2to1(pyfiler1, pyfiler2)
        
        # Call to CMM model
        case 8 if current_cal_year >= int(user.SCEDES['CLBASEYR']):
            
            # Create a string to the AIMMS executable using path from SCEDES file
            # e.g. 'c:\\aimms_installation_free_releases\\4.96.4.6-x64-VS2017\\Bin\\aimms.exe'
            z = os.path.join(user.SCEDES["AIMMSLOC"], 'Bin', 'aimms.exe')
            
            # AIMMS wrapper to run CMM AIMMS Model
            my_result = aimms_wrapper.run_module('coal', pyfiler1, user.CMMCONF["putvar"], z, user.CMMCONF["base_year"])
            
            # Check if AIMMS process completed successfully
            process_aimms_wrapper_return(my_result, pyfiler1, current_nems_module)
            
            # Coal fortran subroutine
            # Fill ECP's coal expectations horizon for current year using prior run values, or perfect foresight
            pyfiler1.cmm_run.coal_expect_go()

            # Pause NEMS run for 5 seconds if this is a solo run with only one NEMS AIMMS model
            if number_of_modules == 1:
                print_it(my_cycle, "sleeping for 5 seconds to avoid collisions", MODULE_NAME)
                time.sleep(5)
        
        # Call to HSM model
        case 9 if current_integer_year >= 32:
            hsm.run_hsm(pyfiler1.ncntrl.curcalyr, pyfiler1.ncntrl.curitr, pyfiler1, pyfiler1.cycleinfo.curirun, user.SCEDES)
        
        # Call to NGMM model
        case 10 if current_integer_year >= int(user.NGMMCONF["base_year"]) - 1989:
            
            # Create a string to the AIMMS executable using path from SCEDES file
            # e.g. 'c:\\aimms_installation_free_releases\\4.96.4.6-x64-VS2017\\Bin\\aimms.exe'
            z = os.path.join(user.SCEDES["AIMMSLOC"], 'Bin', 'aimms.exe')
            
            # AIMMS wrapper to run NGMM AIMMS Model
            my_result = aimms_wrapper.run_module('ngas', pyfiler1, user.NGMMCONF["putvar"], z, user.NGMMCONF["base_year"])
            
            # Check if AIMMS process completed successfully
            process_aimms_wrapper_return(my_result, pyfiler1, current_nems_module)
    
            # Pause NEMS run for 5 seconds if this is a solo run with only one NEMS AIMMS model
            if number_of_modules == 1:
                print_it(my_cycle, "sleeping for 5 seconds to avoid collisions", MODULE_NAME)
                time.sleep(5)
        
        # Call to LFMM model
        case 11: 
            pyfiler1.refine_run.go_nexec_call_refine()
        
        # Call to RFM model
        case 12:
            # copies all data in exposed commonblocks from pyfiler1 to pyfiler2
            swap_pyd1to2(pyfiler1, pyfiler2)
            pyfiler2.renew_run.go_renew()
            # copies all data in exposed commonblocks from pyfiler2 to pyfiler1
            swap_pyd2to1(pyfiler1, pyfiler2)

        # Call to HMM model
        case 13 if current_integer_year >= int(user.HMMCONF["base_year"]) - 1989:
            
            # IDM accounting required for H2 before executing HMM model
            th2con = idm_h2_accounting.h2_accounting_before_hmm(pyfiler1)
            z = os.path.join(user.SCEDES["AIMMSLOC"], 'Bin', 'aimms.exe')
            my_result = aimms_wrapper.run_module('hmm', pyfiler1, user.HMMCONF["putvar"], z, user.HMMCONF["base_year"])
            
            # Check if AIMMS process completed successfully
            process_aimms_wrapper_return(my_result, pyfiler1, current_nems_module)
            
            # IDM accounting required for H2 after executing HMM model
            idm_h2_accounting.h2_accounting_after_hmm(pyfiler1, th2con)

            # add some sleep in case HMM is the only module being called
            if number_of_modules == 1:
                # This only needs to be done if this is the only module on
                print_it(my_cycle, "sleeping for 5 seconds to avoid collisions", MODULE_NAME)
                time.sleep(5)
            
        # Call to CCATS model
        case 14 if current_integer_year >= 32:
            ccats.run_ccats(pyfiler1.ncntrl.curcalyr, pyfiler1.ncntrl.curitr, pyfiler1, pyfiler1.cycleinfo.curirun, user.SCEDES)

        # UNKNOWN MODEL
        case _:
            print_it(my_cycle, f"Cal Yr:{1989 + current_integer_year}, Cur Iter:{current_iteration}, Module:{current_nems_module}", MODULE_NAME)
            print_it(my_cycle, f"NCRL:{pyfiler1.ncntrl.ncrl}, FCRL:{pyfiler1.ncntrl.fcrl}", MODULE_NAME)

    # Calculate and print time to execute the NEMS model with the following information: Model name, runtime (sec), cycle number, year, iteration
    mod_elapsed_time = time.time() - mod_start_time
    print_it(my_cycle, f" ***Module: {current_nems_module}, Runtime (sec): {mod_elapsed_time}, cycle: {my_cycle}, year: {1989 + current_integer_year}, iter: {current_iteration}", MODULE_NAME)


def swap_pyd1to2(pyfiler1, pyfiler2):
    """Copies data from pyfiler1 object to pyfiler2 object

    Parameters
    ----------
    pyfiler1 : module
        pyfiler fortran module
    
    pyfiler2 : module
        pyfiler fortran module
    
    """
    
    # Copies data by fortran Equivalence
    pyd_datatransfer.copy_equivalences_between_pyd(pyfiler1, pyfiler2)
    # Copies data by fortran common blocks and individual variables
    pyd_datatransfer.copy_tables_between_pyd(pyfiler1, pyfiler2)


def swap_pyd2to1(pyfiler1, pyfiler2):
    """Copies data from pyfiler2 object to pyfiler1 object

    Parameters
    ----------
    pyfiler1 : module
        pyfiler fortran module
    
    pyfiler2 : module
        pyfiler fortran module
    
    """
    
    # Copies data by fortran Equivalence
    pyd_datatransfer.copy_equivalences_between_pyd(pyfiler2, pyfiler1)
    # Copies data by fortran common blocks and individual variables
    pyd_datatransfer.copy_tables_between_pyd(pyfiler2, pyfiler1)

def nexec2(pyfiler1, user, current_integer_year, IMODEL, current_nems_module):
    """Executes the final call to individual NEMS models for the iteration

    Calls to individual NEMS models and is called one final time for the iteration. NCRL flag is 1.
    Only the following NEMS models are called in NEXEC2: IDM, HSM, NGMM, CCATS

    Parameters
    ----------
    pyfiler1 : object
    
    user : class
        Contains dictionary of NEMS runtime information
    
    current_integer_year : int
        Corresponding integer to NEMS calendar year (e.g. 1 = 1990)
    
    IMODEL : int
        Corresponding number to NEMS model
        1 = WORLD, 2 = MAC, 3 = RESD, 4 = COMM, 5 = IND, 6 = TRAN, 7 = UTIL, 8 = COAL,
        9 = WELL, 10 = PIPE, 11 = REFINE, 12 = RENEW, 13 = HMM, 14 = CCATS
    
    current_nems_module : str
        Corresponding NEMS model name to IMODEL integer

    """

    # Assign commonly used variables to local variables
    my_cycle = pyfiler1.cycleinfo.curirun
    current_iteration = pyfiler1.ncntrl.curitr
    # Print current NEMS run information onto terminal for NEXEC2: Current NEMS model name
    print_it(my_cycle, f"NEXEC2 MODEL {current_nems_module} entering 'nexec2'", MODULE_NAME)

    # Sums to total number of models for the NEMS Run.If the model is 
    # an AIMMS model with python wrapper (CMM, NGMM, or HMM) and is running 
    # solo without any other model on, a time.sleep() is required for model stability.
    number_of_modules = int(user.SCEDES["EXW"]) + \
                        int(user.SCEDES["EXM"]) + \
                        int(user.SCEDES["EXR"]) + \
                        int(user.SCEDES["EXK"]) + \
                        int(user.SCEDES["EXT"]) + \
                        int(user.SCEDES["EXI"]) + \
                        int(user.SCEDES["EXC"]) + \
                        int(user.SCEDES["EXE"]) + \
                        int(user.SCEDES["EXG"]) + \
                        int(user.SCEDES["EXL"]) + \
                        int(user.SCEDES["EXO"]) + \
                        int(user.SCEDES["EXN"]) + \
                        int(user.SCEDES["EXH"]) + \
                        int(user.SCEDES["EXQ"])
    
    # Timer for NEMS model start time
    mod_start_time = time.time()
    
    # Match function to determine which NEMS model to run
    match IMODEL:

        # Call to IDM model
        case 5: pyfiler1.ind_run.go_ind()

        # Call to HSM model
        case 9 if current_integer_year >= 32:
            hsm.run_hsm(pyfiler1.ncntrl.curcalyr, pyfiler1.ncntrl.curitr, pyfiler1, pyfiler1.cycleinfo.curirun, user.SCEDES)
        
        # Call to NGMM model
        case 10 if current_integer_year >= int(user.NGMMCONF["base_year"]) - 1989:
            
            # Create a string to the AIMMS executable using path from SCEDES file
            # e.g. 'c:\\aimms_installation_free_releases\\4.96.4.6-x64-VS2017\\Bin\\aimms.exe'
            z = os.path.join(user.SCEDES["AIMMSLOC"], 'Bin', 'aimms.exe')
            # AIMMS wrapper to run NGMM AIMMS Model
            aimms_wrapper.run_module('ngas', pyfiler1, user.NGMMCONF["putvar"], z, user.NGMMCONF["base_year"])
    
            # add some sleep in case HMM is the only module being called
            if number_of_modules == 1:
                # This only needs to be done if this is the only module on
                print_it(my_cycle, "sleeping for 5 seconds to avoid collisions", MODULE_NAME)
                time.sleep(5)
        
        # Call to CCATS model
        case 14 if current_integer_year >= 32:
            ccats.run_ccats(pyfiler1.ncntrl.curcalyr, pyfiler1.ncntrl.curitr, pyfiler1, pyfiler1.cycleinfo.curirun, user.SCEDES)
            
        # UNKNOWN MODEL
        case _: print_it(my_cycle, f"IMODEL {current_nems_module} IN SUBROUTINE NEXEC2()", MODULE_NAME)

    # Calculate and print time to execute the NEMS model with the following information: Model name, runtime (sec), cycle number, year, iteration
    mod_elapsed_time = time.time() - mod_start_time
    print_it(my_cycle, f" ***Module: {current_nems_module}, Runtime (sec): {mod_elapsed_time}, cycle: {my_cycle}, year: {1989 + current_integer_year}, iter: {current_iteration}", MODULE_NAME)
    

if __name__ == '__main__':
    print("Please load from the main program script. Exit now...")
    os.sys.exit()