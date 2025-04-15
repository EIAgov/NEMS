import time
import nexec
from logging_utilities import print_it

MODULE_NAME = "postnems.py"

def nems_post(pyfiler1, pyfiler2, user):
    
    """executes a series of functions after nsolve before the NEMS run completes
    
    After NSOLVE completes, NEMS executes several functions to complete any variable accounting,
    writing restart file, NEMS runtime reporting, closing out NEMS models. The following lists 
    the functions executed as part of postnems
    
    1. Assign commonly used variables to local variables
    
    2. Sum up everything to catch retroactive history updates (sumqas, avepas, copy_adjusted, avepasa)
    
    3. Write out restart file - .unf format
    
    4. Write out restart file - .gdx format
    
    5. Print out final log for NEMS runtime
    
    6. Close all open Fortran files
    
    7. Close EMM AIMMS programs (ECP and EFD)
    
    Parameters
    ----------
    pyfiler1 : module

    pyfiler2 : module

    user : SimpleNamespace
    
    """
    
    # 1. Assign commonly used variables to local variables
    CURIRUN                 = pyfiler1.cycleinfo.curirun    # Cycle number (e.g. 1)
    LASTYR                  = pyfiler1.utils.lastyr         # Integer last year (e.g. 61)
    FIRSYR                  = pyfiler1.utils.firsyr         # Integer first year (e.g. 1)
    BASEYR                  = pyfiler1.utils.baseyr         # Calendar base year (e.g. 1990)

    current_iteration       = pyfiler1.ncntrl.curitr        # Integer current iteration (e.g. 2)
    current_integer_year    = pyfiler1.ncntrl.curiyr        # Integer current year (e.g. 8)
    current_calendar_year   = pyfiler1.ncntrl.curcalyr      # Calendar current year (e.g. 1997)


    # 2. Sum up everything to catch retroactive history updates (sumqas, avepas, copy_adjusted, avepasa)
    print_it(CURIRUN, 'run pyfiler: sumqas, avepas, copy_adjusted, avepasa', MODULE_NAME)
    for yr in range(FIRSYR,LASTYR+1):
        CURIYR=yr
        CURCALYR=CURIYR + BASEYR - 1
        pyfiler1.ncntrl.curiyr = yr
        pyfiler1.ncntrl.curcalyr = CURCALYR
        pyfiler1.sumqas()
        pyfiler1.avepas()
        pyfiler1.copy_adjusted()
        pyfiler1.avepasa()

    # 3. Write out restart file - .unf format
    print_it(CURIRUN, "write out restart file (unf)", MODULE_NAME)
    pyfiler1.utils.write_filer("restart.unf")

    # 4. Write out restart file - .gdx format
    print_it(CURIRUN, "write out restart file (gdx)", MODULE_NAME)
    pyfiler1.utils.write_filer("restart.gdx")

    # 5. Print out final log for NEMS runtime
    print_it(CURIRUN, f"Final Log. current_iteration: {current_iteration}, \tcurrent_integer_year: {current_integer_year} \tcurrent calendar year: {current_calendar_year}", MODULE_NAME)
    print_it(CURIRUN, f"NCRL: {pyfiler1.ncntrl.ncrl}, \tFCRL: {pyfiler1.ncntrl.fcrl}", MODULE_NAME)
    TOTAL_MODEL_TIME = time.time() - user.MODEL_START_TIME
    print_it(CURIRUN, f"****TOTAL MODEL TIME: {TOTAL_MODEL_TIME}", MODULE_NAME)
    
    # 6. Close all open Fortran files
    print_it(CURIRUN, "close all open Fortran files via 'pyfiler.mnclose'", MODULE_NAME)
    pyfiler1.mnclose()
    pyfiler2.mnclose()

    # 7. Close EMM AIMMS programs (ECP and EFD)
    if "EMM" in user.module_order:
        # check if need to invoke subroutine in uaimms.f to invoke maintermination routine and close the aimms project.
        print_it(CURIRUN, 'some emm-related operations', MODULE_NAME)
        AIMMSEFD=int(user.SCEDES['AIMMSEFD'])
        AIMMSECP=int(user.SCEDES['AIMMSECP'])
        # Check if EFD was turned on for this NEMS run
        if AIMMSEFD==1:
            nexec.swap_pyd1to2(pyfiler1, pyfiler2)
            pyfiler2.emm_run.close_aimms_efd()
            nexec.swap_pyd2to1(pyfiler1, pyfiler2)
        
        # Check if ECP was turned on for this NEMS run
        if AIMMSECP==1:
            nexec.swap_pyd1to2(pyfiler1, pyfiler2)
            pyfiler2.emm_run.close_aimms_ecp()
            nexec.swap_pyd2to1(pyfiler1, pyfiler2)