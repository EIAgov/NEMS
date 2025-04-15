"""Main execution function for the Carbon Capture, Transportation, Allocation and Sequestration (CCATS) Module.

CCATS Main: Summary
___________________
**ccats.py** is the main execution function for CCATS. It is called from NEMS **main.py**, performs basic model setup functions,
and then calls :ref:`Module` to run the main CCATS processes. The file operates as follows:

    1. **ccats.py** is called from NEMS **main.py** or run directly from an IDE.

    2. Logging setup:

        a. The previous run's log is deleted from the directory.

        b. Logger is configured and written to *"ccats_debug.log"*.

    3. File directory paths are instantiated.

    4. :func:`get_args` is called, pulling run information from the NEMS command line for integrated runs (i.e. iteration and model year).

    5. :func:`run_ccats` is called, kicking off the main CCATS run operations:

        a. :class:`~module.Module` in :ref:`module` is declared, main directories are declared, and main setup file is identified.

        b. :meth:`module.Module.setup` in :ref:`module` is called to perform year 1 model setup.

        c. CCATS determines whether to run standalone vs. integrated based on run location.

        d. Main CCATS functions are run via :ref:`module`.

        e. After main CCATS model functions run, results are prepared and reported to the restart file in
           :meth:`restart.Restart.write_results`.

           
CCATS Dependencies
__________________

CCATS relies on the below list of Python libraries to run.

* Libraries included with the default distribution or available via pip or conda:

    * sys
    * os
    * io
    * shutil
    * pathlib
    * logging
    * argparse
    * shutil
    * pylint
    * tabulate
    * pylab
    * itertools
    * warnings
    * pickle
    * numpy
    * pandas
    * pyomo
    * matplotlib
    * folium (for mapping)
    * xpress (if using the FICO Xpress solver)
    
* NEMS specific libraries:
    * pyfiler1 - maintined by the NEMS Integration Team.

    
CCATS Main: Input Files
_______________________
None


CCATS Main: Functions and Class Methods
_______________________________________
    * :func:`get_args` - Parses the arguments passed in via command line of NEMS
    * :func:`run_ccats` - Executes main CCATS processes from :ref:`Module`.


CCATS Main: Output Debug Files
______________________________
None


CCATS Main: Output Restart Variables
____________________________________
None


CCATS Main: Code
________________
"""
import sys
import os
import logging
import argparse as ap

### Declare Cores
os.environ['NUMEXPR_MAX_THREADS'] = '4' # intial amount of threads, overwritten in module.setup() based on user input


### Setup Logging
# Delete last run log
try:
    os.remove('ccats_debug.log')
except:
    pass

# Configure Logger
for handler in logging.root.handlers[:]:
    logging.root.removeHandler(handler)
logging.basicConfig( level=logging.DEBUG,
                    format='[%(asctime)s][%(name)s]' +
                           '[%(funcName)s][%(levelname)s]  :: |%(message)s|',
                    handlers=[logging.FileHandler("ccats_debug.log"),
                              logging.StreamHandler()])

logger = logging.getLogger('ccats.py')
logger.info('All Model System Paths')
logger.info(sys.path)


### Add Pyfiler Paths
sys.path.append(os.getcwd() + '\\Pyfiler')
sys.path.append(os.getcwd() + '\\..\\Pyfiler')
sys.path.append(os.getcwd() + '\\..\\..\\Pyfiler')
sys.path.append(os.getcwd() + '\\..\\..\\scripts\\Pyfiler')
sys.path.append(os.getcwd() + '\\..\\scripts\\Pyfiler')

# Import Module
import module


def get_args(in_args=None):
    """Parses the arguments passed in via command line of NEMS.
    NOTE: NEMS calls Pyfiler with trailing options for PyFiler to parse through for the input file, output file,
    and direction of restart to hdf formatting.

    Parameters
    ----------
    in_args: list
        in_args first set to none, then creates a list of system arguments passed via the command line.

    Returns
    -------
    arguments: Namespace
        command line arguments saved into namespace variable.
    """
    # Call arguments from namespace
    if in_args is None:
        in_args = sys.argv[1:]

        parser = ap.ArgumentParser(formatter_class=ap.ArgumentDefaultsHelpFormatter,
        prog=os.path.basename(sys.argv[0]))

        parser.add_argument('-y', '--curcalyr',
        default='did not load',
        help=('Current Calendar Year for NEMS'))

        parser.add_argument('-t', '--iteration',
        default='no load on iteration',
        help=('Iteration Number of NEMS'))

        parser.add_argument('-c', '--cycle',
        default='no load on cycle',
        help=('Cycle Number of NEMS'))



    # 'args' conflicts with a debugger command, so use 'arguments' instead
    arguments = parser.parse_args(in_args)

    # Force arguments as int type if possible
    try:
        arguments.curcalyr  = int(arguments.curcalyr)
        arguments.iteration = int(arguments.iteration)
        arguments.cycle     = int(arguments.cycle)
    except:
        pass

    return arguments


def run_ccats(year, iteration, pyfiler1, cycle, scedes):
    '''Executes main CCATS processes from :ref:`Module`.

        * Instantiates main directory,
        * Declares :class:`module.Module` parent class for CCATS,
        * Runs :meth:`module.Module.setup` processes from :ref:`module`,
        * Runs :meth:`module.Module.run` processes from :ref:`module`, and
        * Runs :meth:`restart.Restart.write_results` processes from :ref:`restart`.

    Parameters
    ----------
    year : str or int
        Current run year.

    iteration: str or int
        Current NEMS iteration.

    pyfiler1 : None or pyfiler
        Tool for reading and writing to the restart file.

    cycle : str or int
        Current NEMS cycle.

    scedes : None or scedes file inputs from **main.py**.
        NEMS Scenario Description data.

    Returns
    -------
    None
    '''
    # Log model year and iteration
    logger.info('Argument Year: ' + str(year))
    logger.info('Argument Iteration: ' + str(iteration))

    # Instantiate path to main directory
    main_directory = os.getcwd() + '\\'

    # Instantiate ccats parent class
    ccats = module.Module()

    # Get setup filename
    setup_filename = 'setup.csv'

    # Setup CCATS module
    logger.info('Start setup CCATS module')
    ccats.setup(main_directory,
               setup_filename,
               year,
               pyfiler1,
               cycle,
               scedes)


    # Run CCATS based on integrated vs. standalone switch value
    if ccats.integrated_switch == False: #Standalone Run
        for year in range(ccats.year_start, ccats.year_final + 1):

            if ccats.write_restart_before_run_switch == True:
                # Write to Restart File
                logger.info('Start Write Results to Restart File')
                temp_filename = main_directory + 'restart_CCATSo.unf'
                ccats.restart.write_results(temp_filename)
                logger.info('End Write Results to Restart File')

            ###Run CCATS
            logger.info('Start Run year: ' + str(year))
            ccats.run(year=year)
            logger.info('End Run year: ' + str(year))

            # Write to Restart File
            logger.info('Start Write Results to Restart File')
            temp_filename = main_directory + 'restart_CCATSo.unf'
            ccats.restart.write_results(temp_filename)
            logger.info('End Write Results to Restart File')

        pass


    else: # Integrated Run
        if year < ccats.year_start: # Before ccats Start Year
            temp_filename = main_directory + 'restart_CCATSo.unf'
            #ccats.restart.write_results(temp_filename)
            logger.info('Returning Restart File before CCATS start year ' + str(year))


        else: # Normal NEMS Iteration
            #Write results before running CCATS for debugging
            if ccats.write_restart_before_run_switch == True:
                # Write to Restart File
                logger.info('Start Write Results to Restart File')
                temp_filename = main_directory + 'restart_CCATSo.unf'
                ccats.restart.write_results(temp_filename)
                logger.info('End Write Results to Restart File')

            # Run CCATS
            logger.info('Start Run year: ' + str(year))
            ccats.run(year=year)
            logger.info('End Run year: ' + str(year))

            # Write to Restart File
            logger.info('Start Write Results to Restart File')
            temp_filename = main_directory + 'restart_CCATSo.unf'
            ccats.restart.write_results(temp_filename)
            logger.info('End Write Results to Restart File')


if __name__ == '__main__':
    arguments = get_args()
    try:
        run_ccats(arguments.curcalyr, arguments.iteration, arguments.cycle)
    except:
        run_ccats(arguments.curcalyr, arguments.iteration, None, arguments.cycle, None)
    logger.info('CCATS Finished')
