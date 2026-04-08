"""Main execution function for HSM.

Summary
-------
**hsm.py** is the main execution function for HSM. It is called by NEMS via a .bat file, performs basic model setup functions,
and then calls module_unf.py to run the main NEMS processes. The file operates as follows:

    1. hsm.py is called by a .bat file from NEMS or run directly from the IDE.

    2. File directory variables and model timer are instantiated.

    3. *get_args* is called, pulling run information from the NEMS command line (i.e. iteration and model year).

    4. *run_hsm* is called, kicking off the main HSM run operations:

        a. Module in module_unf.py is declared, main directories are declared, and main setup file is identified.

        b. hsm.setup in module_unf.py is called to perform year 1 model setup.

        c. HSM determines whether to run standalone vs. integrated based on run location.

        d. Main HSM functions are run via **module_unf.py** from history year through final AEO year.

        e. After main HSM model functions run, results are prepared and reported to the restart file.


Input Files
-----------
None


HSM Functions and Class Methods
_______________________________
    * get_args - Parses the arguments passed in via command line of NEMS
    * run_hsm - Executes main HSM functions


Output Debug Files
__________________
None


HSM Class Methods
_________________
"""
import sys
import os
import logging
import argparse as ap
import time

### Setup Logging
# Delete old log
try:
    os.remove('hsm_debug.log')
except OSError:
    pass

#Path to main directories
standalone_directory = os.getcwd() + '\\'
integrated_directory = os.getcwd() + '\\'
integrated_input_path = standalone_directory + 'hsm\\input\\'
setup_file_path = os.path.join(integrated_input_path, 'setup.csv')

# Configure Logger
for handler in logging.root.handlers[:]:
    logging.root.removeHandler(handler)

handlers = [logging.FileHandler("hsm_debug.log")]

# Determine whether or not to include stream handler by identifying if this is an integrated run or not based on whether there's a setup file
file_exists = os.path.exists(setup_file_path)
if not file_exists:
    handlers.append(logging.StreamHandler())  # Add StreamHandler if the file does not exist

logging.basicConfig(level=logging.DEBUG,
                    format='[%(asctime)s][%(name)s]' +
                           '[%(funcName)s][%(levelname)s]  :: |%(message)s|',
                    handlers=handlers)

logger = logging.getLogger('hsm.py')
logger.info('Log All Model System Paths')
logger.info(sys.path)


###Add Pyfiler Paths
# Add current directory first (where pyfiler1.pyd is located)
sys.path.insert(0, os.getcwd())
sys.path.append(os.getcwd() + '\\Pyfiler')
sys.path.append(os.getcwd() + '\\..\\Pyfiler')
sys.path.append(os.getcwd() + '\\..\\..\\Pyfiler')
sys.path.append(os.getcwd() + '\\..\\..\\scripts\\Pyfiler')
sys.path.append(os.getcwd() + '\\..\\scripts\\Pyfiler')

# Add Intel oneAPI compiler paths if they exist
intel_compiler_path = r"C:/Program Files (x86)/Intel/oneAPI/compiler/2023.2.1/windows/redist/intel64_win/compiler"
if os.path.exists(intel_compiler_path):
    sys.path.append(intel_compiler_path)
    os.add_dll_directory(intel_compiler_path)
else:
    logger.warning(f'Intel oneAPI compiler path not found: {intel_compiler_path}')

logger.info('Log All System Paths')
logger.info(sys.path)

# Import module after pyfiler setup
import module_unf
from visualizations import generate_charts


def get_args(in_args=None):
    """parses the arguments passed in via command line of NEMS.
    NOTE: NEMS calls Pyfiler with trailing options for PyFiler to parse through for the input file, output file,
    and direction of restart to hdf formatting.

    Parameters
    ----------
    in_args: list
        "In arguments" variable first set equal to NONE, then creates a list of system arguments passed from NEMS via the command line

    Returns
    -------
    arguments: Namespace
        Command line arguments saved into namespace variable
    """
    #Read in arguments transmitted from the command line
    if in_args is None:
        in_args = sys.argv[1:]

        parser = ap.ArgumentParser(formatter_class=ap.ArgumentDefaultsHelpFormatter,
        prog=os.path.basename(sys.argv[0]))

        parser.add_argument('-y', '--curcalyr',
        default= 0,
        help=('Current Calendar Year for NEMS'))

        parser.add_argument('-t', '--iteration',
        default= 0,
        help=('Iteration Number of NEMS'))

        parser.add_argument('-c', '--cycle',
        default= 0,
        help=('Cycle Number of NEMS'))


    #Assign arguments object
    arguments = parser.parse_args(in_args)

    #Set arguments as int type if possible
    try:
        arguments.curcalyr  = int(arguments.curcalyr)
        arguments.iteration = int(arguments.iteration)
        arguments.cycle     = int(arguments.cycle)
    except:
        pass

    return arguments


def run_hsm(year, iteration, pyfiler1, cycle, scedes):
    '''Executes main HSM functions.

    Returns
    -------
    None
    '''
    logger.info('Argument Year')
    logger.info(year)
    logger.info('Argument Cycle')
    logger.info(cycle)
    logger.info('Argument Iteration')
    logger.info(iteration)

    #Define main HSM module (where HSM operations are called from) as "hsm"
    hsm = module_unf.Module()

    #Path to main directories
    standalone_directory = os.getcwd() + '\\'
    integrated_directory = os.getcwd() + '\\'
    integrated_input_path = standalone_directory + 'hsm\\input\\'

    #Delete restart_HSMOUT.unf if it exists so NEMS bombs when there's a traceback
    hsm_out_exists = os.path.exists(integrated_directory + 'restart_HSMOUT.unf')
    if hsm_out_exists:
        logger.info('Delete last year restart_HSMOUT.unf file')
        os.remove(integrated_directory + 'restart_HSMOUT.unf')

    #Get setup filename
    setup_filename  = 'setup.csv'

    #Setup hsm module with relevant directories and setup filename
    logger.info('Start setup HSM module')
    hsm.setup(standalone_directory,
              integrated_directory,
              integrated_input_path,
              setup_filename,
              year,
              pyfiler1,
              cycle,
              scedes)

    # Reset timing variables and start overall HSM timer
    hsm.reset_timing()
    hsm_start_time = time.time()

    ###Run HSM based on integrated vs. standalone switch value
    if hsm.integrated_switch == False: #Standalone run
        # TEMPORARY DEBUG: Exit early for faster debugging
        DEBUG_STOP_YEAR = 2050
        
        for year in range(hsm.history_year, hsm.final_aeo_year + 1):
            ###Run hsm
            logger.info('Run year: ' + str(year))
            hsm.run(year=year)

            #Prepare results
            logger.info('Prepare Results ' + str(year))
            hsm.prepare_results()

            # Run Natural Gas Processing Submodule
            logger.info('Run Natural Gas Processing' + str(year))
            hsm.run_ngp()

            #Set filename for restart output
            logger.info('Write Results ' + str(year))
            temp_filename = integrated_directory + 'restart_HSMOUT.unf'
            hsm.restart.write_results(temp_filename)
            
            # TEMPORARY DEBUG: Break after processing DEBUG_STOP_YEAR
            if year >= DEBUG_STOP_YEAR:
                logger.info(f'DEBUG: Stopping at year {year} for faster debugging')
                break

        # Calculate total runtime for standalone run
        total_hsm_runtime = time.time() - hsm_start_time
        
        # Display timing summary
        timing_summary = hsm.get_timing_summary()
        timing_summary += f"Total HSM Runtime:     {hsm.format_time(total_hsm_runtime)}\n"
        timing_summary += "="*50 + "\n"
        
        logger.info(timing_summary)
        print(timing_summary)
        
        # Display development charts
        chart_config_path = os.path.join(hsm.input_path, 'chart_config.csv')
        generate_charts(hsm, chart_config_path)

        pass


    else: #Integrated run
        if year < hsm.history_year: #Before HSM Start Year just write results back to NEMS
            temp_filename = integrated_directory + 'restart_HSMOUT.unf'
            hsm.restart.write_results(temp_filename)
            logger.info('Reporting Results before HSM start year ' + str(year))



        elif (hsm.run_every_itr_switch == True) | (iteration == 1) | (hsm.param_fcrl == 1) | (hsm.param_ncrl == 1): #Standard NEMS Run Year
            #Run HSM
            logger.info('Start Run year: ' + str(year))
            hsm.run(year=year)
            logger.info('End Run year: ' + str(year))

            #Prepare Results
            logger.info('Start Reporting Results year: ' + str(year))
            hsm.prepare_results()

            #Run Natural Gas Processing Submodule
            logger.info('Run Natural Gas Processing ' + str(year))
            hsm.run_ngp()

            #Write local variables to Pickle
            logger.info('Write to Pickle ' + str(year))
            hsm.write_pkl()

            #Write results to Restart File
            logger.info('Start Write Results to Restart File')
            temp_filename = integrated_directory + 'restart_HSMOUT.unf'
            hsm.restart.write_results(temp_filename)
            logger.info('End Write Results to Restart File')
            
            # Calculate total runtime for integrated run
            total_hsm_runtime = time.time() - hsm_start_time
            
            # Display timing summary
            timing_summary = hsm.get_timing_summary()
            timing_summary += f"Total HSM Runtime:     {hsm.format_time(total_hsm_runtime)}\n"
            timing_summary += "="*50 + "\n"
            
            logger.info(timing_summary)
            print(timing_summary)


        else: #If "Run every iteration" flag is off or run limit is outside HSM parameters, just write restart file back to NEMS
            logger.info('Year, iteration or ncrl_flag are outside of hsm.py run parameters')
            logger.info('Reporting Results before HSM start year ' + str(year))
            temp_filename = integrated_directory + 'restart_HSMOUT.unf'
            hsm.restart.write_results(temp_filename)





if __name__ == '__main__':
    arguments = get_args()
    run_hsm(arguments.curcalyr, arguments.iteration, None, arguments.cycle, None)

    logger.info('HSM Finished')