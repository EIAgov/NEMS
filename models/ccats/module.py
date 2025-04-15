"""Parent module of the Carbon Capture, Allocation, Transportation and Sequestration (CCATS) Module.

Module: Summary
_______________
The **"Module"** class performs all key model setup functions and runs all sub-class processes (i.e. :ref:`restart`, :ref:`preprocessor` etc.).

The module operates as follows:

    1. Instantiates parent-level variables in :meth:`~module.Module.__init__`

    2. Declares parent-level utilities and variables, and runs submodule setup functions in :meth:`~module.Module.setup`, this function operations as follows:

        a. Declare *logger* as class variable,

        b. Declare filepaths,

        c. Read in *setup.csv* and other global input files (i.e. *mapping.csv*),

        d. Run :ref:`restart` to get restart variables,

        e. Localize restart variables at the parent class level in :class:`~localize_restart.Localize`, and

        f. Run setup functions for the CCATS :ref:`Preprocessor`, :ref:`CCATS Optimization`, ref:`Postprocessor` and ref:`Output` modules.

    3. Runs CCATS modules


Module: Functions and Class Methods
___________________________________
    * :meth:`~module.Module.__init__` - Constructor to initialize Class (called by :func:`ccats.run_ccats`).
    * :meth:`~module.Module.setup` - CCATS Setup method.
    * :meth:`~module.Module.load_local_parameters` - Loads in CCATS Parameters from the restart file
    * :meth:`~module.Module.copy_restart_variables` - Copies Restart Variables for CCATS from the Restart File into local variables.
    * :meth:`~module.Module.aggregate_restart_variables` - Aggregate localized restart variables for :ref:`preprocessor`.
    * :meth:`~module.Module.read_pkl` - Call :ref:`CCATS Pickle` to read in pickle files from the previous model year fcrl and ncrl iterations.
    * :meth:`~module.Module.run` - Runs main CCATS Model Processes.


Module: Input Files
___________________
    * **setup.csv** - setup file with key model inputs (model flags, model history year, etc.).
    * **mapping.csv** - CCATS regional mapping (mapping states to CCATS regions, etc.).
    * **co2_eor_mapping.csv** - Census division and census region mapping for CO\ :sub:`2` EOR plays.
    * **idm_mapping.csv** - Proportional allocation of IDM CO\ :sub:`2` capture volumes from census regions to census divisions.
    * **co2_supply_index.csv** - Restart file index numbers for CO\ :sub:`2` supply types.
    * **co2_seq_index.csv** - Restart file index numbers for CO\ :sub:`2` sequestration types.


Module: Output Debug Files
__________________________
None


Module: Output Restart Variables
________________________________
None


Module: Code
____________
"""
from ccats import logging
logger = logging.getLogger('module.py')

from ccats_common import common as com

logger.info('Import Shutil')
import shutil
logger.info('Import pandas')
import pandas as pd
logger.info('Import os')
import os
logger.info('Import Pathlib')
from pathlib import Path


class Module:
    """
    Parent module for CCATS.
    """

    def __init__(self):
        """Initializes Module object.

        Parameters
        ----------
        None

        Returns
        -------
        None

        """
        ### Input and output paths
        # Input Paths
        self.main_directory         = ''  # Path to main source directory
        self.input_path             = ''  # Path to input directory containing top-level input files and submodule directories
        self.preproc_input_path     = ''  # Path to preprocessor submodule directory

        # Output Paths
        self.output_path            = ''  # Path to debug directory
        self.ccats_var_output_path  = ''  # Path to pickle directory


        ### Tables
        self.setup_table        = pd.DataFrame()  # Setup table

        ### General
        self.mapping            = pd.DataFrame() # Dataframe of regional relationships
        self.co2_eor_mapping    = pd.DataFrame() # DataFrame of regional relationships for CO2 EOR
        self.idm_mapping        = pd.DataFrame() # Dataframe of Industrial Demand Module distribution of CO2 volumes from census regions to census divisions
        self.co2_supply_index   = {}    # Dictionary of supply index values for restart file outputs
        self.co2_seq_index      = {}    # Dictionary of sequestration index values for restart file outputs
        self.scedes             = {} # Dictionary of scedes flags from keys.sed
        self.year_aeo           = 0  # Year of the current AEO (e.g. AEO2020, aeo_year = 2020)
        self.year_start         = 0  # First model year
        self.year_final         = 0  # Last aeo year (MNUMYR)
        self.year_base_price    = 0  # Base year for prices in NEMS (i.e. prices are in 1987$)
        self.years_steo         = [] # List of STEO years
        self.eor_45q            = 0  # 45Q tax credit value for CO2 EOR
        self.saline_45q         = 0  # 45Q tax credit value for saline storage
        self.year_new_45q       = 0  # Year new policy provisions relating to 45Q start (i.e. inflation adjustment)
        self.legacy_eor_45q     = 0  # Legacy 45Q tax credit value for CO2 EOR
        self.legacy_saline_45q  = 0  # Legacy 45Q tax credit value for saline storage
        self.year_leg_45q       = 0  # Year legacy policy provisions relating to 45Q start (i.e. inflation adjustment)

        ### Switches
        self.integrated_switch                  = False # False means standalone run, True means integrated run
        self.debug_switch                       = False # False means local debug files are not printed out, True means local debug files are printed out
        self.update_45q_switch                  = False # False means 45Q tax credit values are not updated, True means 45Q tax credit values are updated
        self.visualize_preproc_switch           = False # False means preprocessor visualizations will not be created
        self.visualize_postproc_switch          = False # False means postprocessor visualizations will not be created
        self.pytest_switch                      = False # False means value testing in preproc and postproc is turned off
        self.price_average_switch               = False # True means co2 price is based on a weighted average
        self.price_marginal_switch              = False # True (and price_average_switch False) means co2 price is on a marginal basis
        self.linear_model_switch                = False # True means model is linear (LP), False means model is a mixed integer linear program (MILP)
        self.write_restart_before_run_switch    = False # True means "write restart" method in restart.py is called before CCATS runs
        self.debug_restart_itr_switch           = False # True means that the restart file debug in the "write_restart" method in restart.py is called every iteration
        self.output_restart_unf_switch          = False # True means restart_CCATSo.unf file is generated

        ### Model control variables
        # Model control variables
        self.year_current       = 0  # Current model year
        self.iteration_current  = 0  # Current model iteration
        self.cycle_current      = 0  # Current model cycle


        ### Parameters
        self.param_baseyr   = 1990  # Base calendar year corresponding to curiyr=1
        self.param_fcrl     = 0     # Flag to indicate last regular model iteration
        self.param_ncrl     = 0     # Flag to indicate reporting model iteration

        ### Logger
        self.logger = ''    # Object logger


        ### Pyomo model class variables
        self.pyomo_model            = None # CCATS pyomo model object
        self.pyomo_model_results    = None # CCATS pyomo results object
        self.pyomo_block0           = None # CCATS pyomo block 0 object
        self.pyomo_block1           = None # CCATS pyomo block 1 object
        self.pyomo_block2           = None # CCATS pyomo block 2 object
        self.opt                    = None # CCATS optimization object
        self.model_name             = ''   # CCATS optimization model name


        ### Legacy FUTURE Act 45Q tax credit values
        self.leg_ccs_eor_45q    = pd.DataFrame()  # Legacy 45Q tax credit value for CO2 EOR
        self.leg_ccs_saline_45q = pd.DataFrame()  # Legacy 45Q tax credit value for Saline Formations


        ### Concatenated restart file data
        self.industrial_co2_supply_45q  = pd.DataFrame() # DataFrame of industrial CO2 volume supplied by other NEMS modules that are 45q eligible
        self.industrial_co2_supply_ntc  = pd.DataFrame() # DataFrame of industrial CO2 volume supplied by other NEMS modules ntc
        self.industrial_co2_cost_inv    = pd.DataFrame() # DataFrame of carbon capture cost from industrial CO2 cupplied from other modules
        self.industrial_co2_cost_om     = pd.DataFrame() # DataFrame of carbon capture cost from industrial CO2 cupplied from other modules

        
        ### Data passed between model years 
        self.new_built_pipes_df         = pd.DataFrame() # DataFrame of new pipelines built in previous model year b1
        self.new_aors_df                = pd.DataFrame() # DataFrame of new AORS in the previous storage year
        self.store_prev_b0_df           = pd.DataFrame() # DataFrame of previous model year store_b0

        pass


#######################################
###CCATS MODEL SETUP
#######################################

    def setup(self,
               main_directory,
               setup_filename,
               year,
               pyfiler1,
               cycle,
               scedes):
        """CCATS Setup method.

            * Imports and declares *pyfiler* object,
            * Declares *logger* as parent object executable,
            * Assigns input paths and global variables,
            * Reads in setup tables,
            * Calls :ref:`restart` to read in restart variables,
            * Calls :meth:`load_local_parameters` to load in model parameters,
            * Reads in relevant scedes flags,
            * Calls :ref:ccats_pickle: to read in iterative model intermediate variables,
            * Calls :ref:ccats_history: to loads in history, and write history data to restart file variables, and
            * Calls the :ref:`Preprocessor`, :ref:`CCATS Optimization`, :ref:`Postprocessor`, and :ref:`Output` *setup* methods.

        Parameters
        ----------
        main_directory : str
            Filepath for CCATS main run directory.

        setup_filename : str
            setup.txt filename.

        year : str or int
            Current model year.

        pyfiler1 : None or pyfiler
            Tool for reading and writing to the restart file.

        cycle : int
            Model run cycle from main.py.

        scedes : None or scedes file inputs from **main.py**.
            NEMS Scenario Description data.

        Returns
        -------
        self.pyfiler1 : object
            Object containing restart file data from **main.py**.
        
        self.threads : int
            Number of threads available to run CCATS.

        self.logger : Logger
            Logger utility.

        *directories* : strings
            Assorted directory paths including *self.main_directory* and *self.input_path*.

        self.integrated_switch : int
            Switch indicating whether CCATS run is standalone or integrated.

        self.setup_table : DataFrame
            Input file containing CCATS model user inputs.

        self.debug_switch : bool
            Output debug files if True (will result in longer model runtime).

        self.update_45q_switch : bool
            Update the 45Q policy path with input from setup.txt "eor_45q" and "saline_45q" variables if True.

        self.visualize_preproc_switch : bool
            Output folium visualizations of optimization inputs if True.

        self.visualize_postproc_switch : bool
            Output folium visualizations of optimization outputs if True.

        self.pytest_switch : bool
            Perform pytest internal validation tests if True.

        self.price_reset_switch : bool
            Reset CO\ :sub:`2` prices to produce endogenous CO\ :sub:`2` prices from models which would otherwise have 0 CO\ :sub:`2` capture if True.

        self.price_average_switch : bool
            CO\ :sub:`2` prices are set as a weighted average of duals by CD if True.

        self.price_marginal_switch : bool
            CO\ :sub:`2` prices are set as the marginal CO\ :sub:`2` price by CD if True.

        self.linear_model_switch : bool
            Model is run as a linear model (vs. a MIP) if True.

        self.write_restart_before_run_switch : bool
            Output the restart file debug received by CCATS before running if True.

        self.debug_restart_itr_switch : bool
            Output the restart file debug file every iteration if True (will result in slightly longer model runtime).

        self.output_restart_unf_switch : bool
            Output the restart file as a .unf after running CCATS if True.

        self.year_aeo : int
            AEO vintage year.

        self.year_start : int
            Model start year.

        self.year_final : int
            Model final year.

        self.year_base_price : int
            Base dollar year.

        self.years_steo : list
            Model STEO years.

        self.eor_45q : float
            EOR 45Q tax credit value.

        self.saline_45q : float
            Saline Formation storage 45Q tax credit value.

        self.year_new_45q : float
            Year in which most recent 45Q policy update occurred (2022 Inflation Reduction Act).

        self.legacy_eor_45q : float
            Legacy 45Q policy EOR tax credit value.

        self.legacy_saline_45q : float
            Legacy 45Q policy saline formation storage  tax credit value.

        self.year_leg_45q : float
            Year in which legacy 45Q policy update occurred (2018 Bipartisan Budget Act).

        self.price_reset_value_45q : float
            Default starting price for 45Q eligible CO\ :sub:`2` volumes when model is reset to restart convergence.

        self.price_reset_value_ntc : float
            Default starting price for 45Q ineligible CO\ :sub:`2` volumes when model is reset to restart convergence.

        self.supply_select_ts_penalty : float
            Penalty for ts node costs in the model.

        self.mapping : DataFrame
            DataFrame of NEMS mapping (i.e. HSM regions to LFMM regions).
            
        self.co2_eor_mapping : DataFrame
            DataFrame of NEMS mapping to CO\ :sub:`2` EOR projects.

        self.idm_mapping : DataFrame
            DataFrame of Industrial demand module mapping (Census regions to divisions with distrutions).

        self.co2_supply_index : dict
            Dictionary of restart file index numbers for CO\ :sub:`2` supply types.

        self.co2_seq_index : dict
            Dictionary of index numbers for CO\ :sub:`2` sequestration types.
        """
        ### Instantiate Pyfiler
        if self.integrated_switch == True:
            self.pyfiler1 = pyfiler1
        else:
            import pyfiler1
            self.pyfiler1 = pyfiler1

        ### Instantiate logger
        self.logger = logger

        ### Get model cycle
        if cycle == 'no load on cycle':
            self.cycle_current = 0
        else:
            self.cycle_current = cycle

        # Assign directories for stanalone vs. integrated run based on where setup.csv is located
        file_exists = os.path.exists(main_directory + 'CCATS\\input\\setup.csv')
        logger.info('parent input path: ' + main_directory)
        if file_exists:
            self.main_directory             = main_directory
            self.input_path                 = main_directory + 'CCATS\\input\\'
            self.output_path                = main_directory + 'CCATS\\debug\\'
            self.ccats_var_output_path      = main_directory + 'CCATS\\pickle\\'
            self.integrated_switch          = True
            self.logger.info('INTEGRATED SWITCH ON')

        else:
            self.main_directory             = main_directory
            self.parent_directory           = str(Path(self.main_directory).parent)
            self.input_path                 = self.main_directory + 'input\\'
            self.output_path                = self.main_directory + 'debug\\'
            self.ccats_var_output_path      = self.main_directory + 'pickle\\'

            self.integrated_switch          = False
            self.logger.info('INTEGRATED SWITCH OFF')

        # Read in setup table
        self.setup_table                = com.read_dataframe(self.input_path + setup_filename, index_col=0)

        # Assign switch values
        self.debug_switch                       = str(self.setup_table.at['debug_switch', 'value']).upper() == 'True'.upper()
        self.update_45q_switch                  = str(self.setup_table.at['update_45q_switch', 'value']).upper() == 'True'.upper()
        self.visualize_preproc_switch           = str(self.setup_table.at['visualize_preproc_switch', 'value']).upper() == 'True'.upper()
        self.visualize_postproc_switch          = str(self.setup_table.at['visualize_postproc_switch', 'value']).upper() == 'True'.upper()
        self.pytest_switch                      = str(self.setup_table.at['pytest_switch', 'value']).upper() == 'True'.upper()
        self.price_reset_switch                 = str(self.setup_table.at['price_reset_switch', 'value']).upper() == 'True'.upper()
        self.price_average_switch               = str(self.setup_table.at['price_average_switch', 'value']).upper() == 'True'.upper()
        self.price_marginal_switch              = str(self.setup_table.at['price_marginal_switch', 'value']).upper() == 'True'.upper()
        self.linear_model_switch                = str(self.setup_table.at['linear_model_switch', 'value']).upper() == 'True'.upper()
        self.write_restart_before_run_switch    = str(self.setup_table.at['write_restart_before_run_switch', 'value']).upper() == 'True'.upper()
        self.debug_restart_itr_switch           = str(self.setup_table.at['debug_restart_itr_switch', 'value']).upper() == 'True'.upper()
        self.output_restart_unf_switch          = str(self.setup_table.at['output_restart_unf_switch', 'value']).upper() == 'True'.upper()

        # Assign base model parameters
        self.threads                    = int(self.setup_table.at['threads', 'value'])
        self.year_aeo                   = int(self.setup_table.at['year_aeo', 'value'])
        self.year_start                 = int(self.setup_table.at['year_start','value'])
        self.year_final                 = int(self.setup_table.at['year_final', 'value'])
        self.year_base_price            = int(self.setup_table.at['year_base_price', 'value'])
        self.years_steo                 = list(range(self.year_aeo - 1, self.year_aeo + 2))
        self.eor_45q                    = int(self.setup_table.at['eor_45q', 'value'])
        self.saline_45q                 = int(self.setup_table.at['saline_45q', 'value'])
        self.year_new_45q               = int(self.setup_table.at['year_new_45q', 'value'])
        self.year_45q_duration          = int(self.setup_table.at['year_45q_duration', 'value'])
        self.year_45q_last_new          = int(self.setup_table.at['year_45q_last_new', 'value'])
        self.year_leg_45q               = int(self.setup_table.at['year_leg_45q', 'value'])
        self.legacy_eor_45q             = int(self.setup_table.at['legacy_eor_45q', 'value'])
        self.legacy_saline_45q          = int(self.setup_table.at['legacy_saline_45q', 'value'])
        self.year_leg_45q               = int(self.setup_table.at['year_leg_45q', 'value'])
        self.price_reset_value_45q      = float(self.setup_table.at['price_reset_value_45q', 'value'])
        self.price_reset_value_ntc      = float(self.setup_table.at['price_reset_value_ntc', 'value'])
        self.supply_select_ts_penalty   = float(self.setup_table.at['supply_select_ts_penalty', 'value'])

        # Load in regional mapping
        self.mapping                    = com.read_dataframe(self.input_path + self.setup_table.at['mapping', 'value'])
        self.co2_eor_mapping            = com.read_dataframe(self.input_path + self.setup_table.at['co2_eor_mapping', 'value'])
        self.idm_mapping                = com.read_dataframe(self.input_path + self.setup_table.at['idm_mapping', 'value'])

        # Load in Restart File Mapping
        self.co2_supply_index = com.read_dataframe(self.input_path + self.setup_table.at['co2_supply_index','value'], index_col = ['co2_supply_type'])
        self.co2_supply_index = self.co2_supply_index.to_dict()['key']

        self.co2_seq_index = com.read_dataframe(self.input_path + self.setup_table.at['co2_seq_index','value'], index_col = ['co2_seq_type'])
        self.co2_seq_index = self.co2_seq_index.to_dict()['key']

        # Set-up number of threads available for CCATS
        os.environ['NUMEXPR_MAX_THREADS'] = str(self.threads)
        self.logger.info('CCATS set to run with ' + str(self.threads) + ' threads.')

        ### Import Submodules
        logger.info('Import Restart')
        import restart as rst
        logger.info('Import History')
        import ccats_history as hist
        logger.info('Import Financials')
        import ccats_financial as fin
        logger.info('Localize Restart Variables')
        import localize_restart as loc_rst
        logger.info('Import Preprocessor')
        import preprocessor as prp
        logger.info('Import Model')
        from models import ccats_optimization as ccats_opt

        logger.info('Import Postprocessor')
        import postprocessor as pop
        logger.info('Import Pickle')
        import ccats_pickle as pkl
        logger.info('Import Outputs')
        import output as opt
        logger.info('Import Common functions')
        from ccats_common import common_pytest as com_test


        # Declare Modules
        self.restart            = rst.Restart(self)                 # Restart object
        self.ccats_hist         = hist.CCATS_History(self)          # Historical Data object
        self.ccats_fin          = fin.CCATS_Finance(self)           # Financial Assumptions object
        self.loc_rst            = loc_rst.Localize(self)            # Localize Restart Variables for Preprocessor
        self.preproc            = prp.Preprocessor(self)            # Preprocessor object
        self.ccats_opt          = ccats_opt.OptimizationModel(self) # CCATS optimization model
        self.postproc           = pop.Postprocessor(self)           # Postprocessor object
        self.pkl                = pkl.CCATS_Pickle(self)            # Pickling object
        self.output             = opt.Output(self)                  # Output object
        self.pytest             = com_test.TestCollection(self)     # Pytest Collection

        ### Load restart file
        self.logger.info('Start Load Restart File')
        temp_filename   = self.main_directory + self.setup_table.at['restart_in', 'value']
        self.logger.info(temp_filename)
        temp_rest       = self.main_directory + 'restart.unf'
        self.restart.run(temp_filename, temp_rest)
        
        # Load parameters
        self.logger.info('Load Local Parameters')
        self.load_local_parameters()
        
        # Create local copies of restart variables
        self.logger.info('Create local copies of restart variables')
        self.copy_restart_variables()

        # Localize and Reformat input restart files to improve data processing
        self.logger.info('Localize and reformat input restart files')
        self.loc_rst.setup_eor()
        self.loc_rst.concat_restart_variables_supply()
        self.loc_rst.concat_restart_variables_cost()
        self.aggregate_restart_variables()
        self.logger.info('End Load Restart File')


        ###Load pickled local variables
        self.logger.info('Start Load Intermediate Variables')
        self.read_pkl()
        self.logger.info('End Load Local Variables')


        ### Load Scedes scenario flag to determine if run is a side case
        if scedes == None:
            self.scedes = None
        else:
            self.scedes = scedes.copy()

        ### Load in history
        if self.year_current == self.year_start:
            self.logger.info('Load in history')
            temp_filename = self.setup_table.at['hist_setup', 'value']
            self.ccats_hist.setup(temp_filename)
            self.ccats_hist.run()


        ### Declare Financial Assumptions
        self.logger.info('Declare Financial Assumptions')
        temp_filename = self.setup_table.at['fin_setup', 'value']
        self.ccats_fin.setup(temp_filename)
        self.ccats_fin.run()


        ### Setup Submodules
        if (self.year_current >= self.year_start) | (self.integrated_switch == False):

            # Setup Preprocessor Submodule
            self.preproc_switch = str(self.setup_table.at['preproc_switch', 'value']).upper() == 'True'.upper()
            if self.preproc_switch:
                self.logger.info('Setup Preprocessor Submodule')
                temp_filename = self.setup_table.at['preproc_setup', 'value']
                self.preproc.setup(temp_filename)
            else:
                self.logger.warning('Preprocessor Submodule Turned Off')


            ### Setup Optimization Submodule
            # CCATS Optimization
            self.ccats_opt_switch = str(self.setup_table.at['ccats_opt_switch', 'value']).upper() == 'True'.upper()
            if self.ccats_opt_switch:
                self.logger.info('Setup CCATS Optimization')
                temp_filename = self.setup_table.at['opt_setup', 'value']
                self.ccats_opt.setup(temp_filename)
            else:
                self.logger.warning('CCATS Optimization Submodule Turned Off')


            ### Setup Postprocessor Submodule
            self.postproc_switch = str(self.setup_table.at['postproc_switch', 'value']).upper() == 'True'.upper()
            if self.postproc_switch:
                self.logger.info('Setup Postprocessor Submodule')
                temp_filename = self.setup_table.at['postproc_setup', 'value']
                self.postproc.setup(temp_filename)
            else:
                self.logger.warning('Postprocessor Submodule Turned Off')


            ### Setup Output Submodule
            self.output_switch = str(self.setup_table.at['output_switch', 'value']).upper() == 'True'.upper()
            if self.output_switch:
                self.logger.info('Setup Output Submodule')
                self.output.setup()
            else:
                self.logger.warning('Output Submodule Turned Off')

            if self.pytest_switch:
                self.pytest.output_path = self.output_path

        pass


    def load_local_parameters(self):
        """Loads in CCATS Parameters from the restart file.

        Parameters
        ----------
        None

        Returns
        -------
        self.param_baseyr : int
             Base AEO model year for history (hardcoded as 1990).

        self.param_fcrl : boolean
            Flag to indicate last regular model iteration.

        self.param_ncrl : boolean
            Flag to indicate reporting model iteration.

        """
        self.param_baseyr               = self.restart.parametr_baseyr
        self.param_fcrl                 = self.restart.parametr_fcrl
        self.param_ncrl                 = self.restart.parametr_ncrl

        # Log key parameters for debug
        logger.info('param_fcrl_switch')
        logger.info(self.param_fcrl)
        logger.info('param_ncrl_switch')
        logger.info(self.param_ncrl)

        pass

    def copy_restart_variables(self):
        """Copy Restart Variables for CCATS from the Restart File into local variables.

            * We instantiate local versions of the restart files to ensure data fidelity
            * Local variables are instantiated form the restart file exactly once, and written to the restart file exactly once

        Parameters
        ----------
        None

        Returns
        -------
        self.year_current : int
            Current model year.

        self.iteration_current : int
            Current model iteration.

        self.rest_mc_jpgdp : DataFrame
            DataFrame of inflation multipliers.

        self.rest_mc_rmcorpbaa : DataFrame
            DataFrame of corporate bond rate path.

        self.rest_mc_rmtcm10y : DataFrame
            DataFrame of ten-year treasury rate path.

        self.rest_pelin : DataFrame
            DataFrame of industrial electricity prices (1987$/MWh).

        self.rest_ccs_eor_45q : DataFrame
            DataFrame of 45Q tax credit values for carbon capture from saline formation storage (1987$).

        self.rest_ccs_saline_45q : DataFrame
            DataFrame of 45Q tax credits values for carbon capture from CO\ :sub:`2` EOR (1987$).


        self.leg_ccs_eor_45q : df
            DataFrame of legacy 45Q tax credit values for carbon capture from saline formation storage (1987$)

        self.rest_i_45q_duration : int
            Tax code section 45Q subsidy duration

        self.rest_i_45q_syr : int
            Start year of tax code section 45Q subsidy

        self.rest_i_45q_lyr_ret : int
            End year of tax code section 45Q subsidy for retrofits

        self.rest_i_45q_lyr_new : int
            End year of tax code section 45Q subsidy for new builds

        self.leg_ccs_eor_45q : df
            DataFrame of legacy 45Q tax credit values for carbon capture from saline formation storage (1987$)
        self.leg_ccs_saline_45q : DataFrame
            DataFrame of legacy 45Q tax credits values for carbon capture from CO\ :sub:`2` EOR (1987$).

        self.eor_45q : int
            Base 45Q tax credit value for carbon capture from CO\ :sub:`2` EOR (1987$).

        self.saline_45q : int
            Base 45Q tax credit value for carbon capture from saline formation storage (1987$).

        self.legacy_eor_45q : int
            Legacy base 45Q tax credit value for carbon capture from CO\ :sub:`2` EOR (1987$).

        self.legacy_saline_45q : int
            Legacy base 45Q tax credit value for carbon capture from saline formation storage (1987$).

        self.rest_i_45q_duration : int
            45Q tax credit duration (years).

        self.rest_i_45q_lyr_ret : int
            Final year for 45Q retrofits.

        self.rest_i_45q_syr : int
            Start year of 45Q tax credit program.

        self.rest_dem_eor : DataFrame
            DataFrame of CO\ :sub:`2` demand (metric tonnes) from CO\ :sub:`2` EOR.

        self.rest_cst_eor : DataFrame
            DataFrame of CO\ :sub:`2` price offers (1987$/tonne) from CO\ :sub:`2` EOR.

        self.rest_play_map : DataFrame
            DataFrame of CO\ :sub:`2` EOR play number indices.

        CO\ :sub:`2` Supply and Capture Cost Variables : DataFrames
            DataFrames of CO\ :sub:`2` supply (metric tonnes) and capture costs (1987$) from other NEMS modules (HSM, HMM, LFMM, IDM, EMM).

        CO\ :sub:`2` Price Variables : DataFrames
            CO\ :sub:`2` prices (1987$) calculated in CCATS and sent to other NEMS by Census Region/Division.

        CCATS Reporting Variables : DataFrames
            CO\ :sub:`2` supply and demand reporting variables.

        """
        ### Read in model years
        if self.integrated_switch:
            self.year_current       = self.restart.ncntrl_curcalyr
            self.iteration_current  = self.restart.ncntrl_curitr
        else:
            self.year_current = self.year_start
            self.iteration_current = 1


        ### Macro Variables
        self.rest_mc_jpgdp = self.restart.macout_mc_jpgdp.copy()
        self.rest_mc_jpgdp.index  = list(range(self.year_base_price, (self.year_final + 1)))
        self.rest_mc_rmcorpbaa = self.restart.macout_mc_rmcorpbaa.copy()
        self.rest_mc_rmtcm10y = self.restart.macout_mc_rmtcm10y.copy()


        ### Electricity Cost
        self.rest_pelin = self.restart.mpblk_pelin.copy()
        self.rest_pelin.columns = ['elec_cost_1987$']


        ### EMM Retrofits
        self.rest_ucapisn = self.restart.udatout_ucapisn.copy()
        self.rest_ucapsqn = self.restart.udatout_ucapsqn.copy()
        self.rest_ucappqn = self.restart.udatout_ucappqn.copy()
        self.rest_ucapasn = self.restart.udatout_ucapasn.copy()
        self.rest_ucapa2n = self.restart.udatout_ucapa2n.copy()


        ### Electricity demand
        self.rest_co2_elec = self.restart.ccatsdat_co2_elec


        ### 45Q
        # EOR 45Q value - 1987$
        self.rest_ccs_eor_45q       = self.restart.tcs45q_ccs_eor_45q.copy()
        self.rest_ccs_eor_45q.index = list(range(self.param_baseyr, (self.year_final + 1)))

        # Saline 45Q value - 1987$
        self.rest_ccs_saline_45q = self.restart.tcs45q_ccs_saline_45q.copy()
        self.rest_ccs_saline_45q.index  = list(range(self.param_baseyr, (self.year_final + 1)))

        # 45Q accounting
        self.rest_i_45q_duration    = self.restart.tcs45q_i_45q_duration
        self.rest_i_45q_syr         = self.restart.tcs45q_i_45q_syr
        self.rest_i_45q_lyr_ret     = self.restart.tcs45q_i_45q_lyr_ret
        self.rest_i_45q_lyr_new     = self.restart.tcs45q_i_45q_lyr_new

        # Update 45Q tax credit variables in Restart File
        if self.update_45q_switch == True:
            # Current law 45Q tax credits
            self.logger.info('Update current law 45Q tax credit values')
            self.eor_45q = self.eor_45q * com.calculate_inflation(self.rest_mc_jpgdp.copy(), 2022)
            self.saline_45q = self.saline_45q * com.calculate_inflation(self.rest_mc_jpgdp.copy(), 2022)

            self.rest_ccs_eor_45q.loc[list(range(self.year_new_45q, (self.year_final + 1)))] = self.eor_45q
            self.rest_ccs_saline_45q.loc[list(range(self.year_new_45q, (self.year_final + 1)))] = self.saline_45q
            
            # 45Q is inflation adjusted, starts in 2027 from 2025
            for year in list(range(self.year_new_45q, self.year_new_45q + 5)):
                self.rest_ccs_eor_45q.loc[year] = (self.rest_ccs_eor_45q.loc[year]/ \
                                                    com.calculate_inflation(self.rest_mc_jpgdp.copy(), self.year_new_45q, (min(year,(self.year_new_45q + 3)))))
                self.rest_ccs_saline_45q.loc[year] = (self.rest_ccs_saline_45q.loc[year] / \
                                                    com.calculate_inflation(self.rest_mc_jpgdp.copy(), self.year_new_45q, (min(year,(self.year_new_45q + 3)))))
                
            for year in list(range(self.year_new_45q + 5, self.year_final + 1)):
                self.rest_ccs_eor_45q.loc[year] = self.rest_ccs_eor_45q.loc[(self.year_new_45q + 3)]
                self.rest_ccs_saline_45q.loc[year] = self.rest_ccs_saline_45q.loc[(self.year_new_45q + 3)]

            # 45Q accounting
            self.rest_i_45q_duration    = self.year_45q_duration
            self.rest_i_45q_syr         = self.year_leg_45q
            self.rest_i_45q_lyr_ret     = self.year_45q_last_new
            self.rest_i_45q_lyr_new     = self.year_45q_last_new


        # 2018 FUTURE ACT 45Q tax credits
        # Legacy tax credits use the same inflator as current law tax credits because they're not inflation adjusted until 2026
        self.legacy_eor_45q     = self.legacy_eor_45q * com.calculate_inflation(self.rest_mc_jpgdp.copy(), 2022)
        self.legacy_saline_45q  = self.legacy_saline_45q * com.calculate_inflation(self.rest_mc_jpgdp.copy(), 2022)

        self.leg_ccs_eor_45q = self.rest_ccs_eor_45q.copy()
        self.leg_ccs_eor_45q.loc[list(range(self.year_leg_45q, (self.year_final + 1)))] = self.legacy_eor_45q

        self.leg_ccs_saline_45q = self.rest_ccs_saline_45q.copy()
        self.leg_ccs_saline_45q.loc[list(range(self.year_leg_45q, (self.year_final + 1)))] = self.legacy_saline_45q

        # 45Q inflation adjustment start in 2027 from 2025
        for year in list(range(self.year_leg_45q + 10, self.year_final + 1)):
            self.leg_ccs_eor_45q.loc[year] = (self.leg_ccs_eor_45q.loc[year] * \
                                                com.calculate_inflation(self.rest_mc_jpgdp.copy(), (self.year_leg_45q + 8), year))
            self.leg_ccs_saline_45q.loc[year] = (self.leg_ccs_saline_45q.loc[year] * \
                                                com.calculate_inflation(self.rest_mc_jpgdp.copy(), (self.year_leg_45q + 8), year))


        ### Supply
        # Natural Gas Processing
        self.rest_cst_ngp_inv  = self.restart.ccatsdat_cst_ngp_inv.copy()   # Investment cost for CO2 capture from natural gas processing facilities
        self.rest_cst_ngp_om   = self.restart.ccatsdat_cst_ngp_om.copy()    # O&M cost for CO2 capture from natural gas processing facilities
        self.rest_sup_ngp_45q  = self.restart.ccatsdat_sup_ngp_45q.copy()   # CO2 volumes from natural gas processing facilities that are 45Q eligible
        self.rest_sup_ngp_ntc  = self.restart.ccatsdat_sup_ngp_ntc.copy()   # CO2 volumes from natural gas processing facilities, no tax credit

        # Powerplants
        self.rest_cst_emm_inv  = self.restart.ccatsdat_cst_emm_inv.copy()   # Investment costs for CO2 capture from power plants
        self.rest_cst_emm_om   = self.restart.ccatsdat_cst_emm_om.copy()    # O&M cost for CO2 capture from power plants
        self.rest_sup_emm_45q  = self.restart.ccatsdat_sup_emm_45q.copy()   # CO2 volumes from power plants that are 45Q eligible
        self.rest_sup_emm_ntc  = self.restart.ccatsdat_sup_emm_ntc.copy()   # CO2 volumes from power plants, no tax credit

        # Cement
        self.rest_cst_cmt_inv  = self.restart.ccatsdat_cst_cmt_inv.copy()   # Investment cost for CO2 capture from cement plants
        self.rest_cst_cmt_om   = self.restart.ccatsdat_cst_cmt_om.copy()    # O&M cost for CO2 capture from cement plants
        self.rest_sup_cmt_45q  = self.restart.ccatsdat_sup_cmt_45q.copy()   # CO2 volumes from cement plants that are 45Q eligible
        self.rest_sup_cmt_ntc  = self.restart.ccatsdat_sup_cmt_ntc.copy()   # CO2 volumes from cement plants, no tax credit

        # Ethanol
        self.rest_cst_eth_inv  = self.restart.ccatsdat_cst_eth_inv.copy()   # Investment cost for carbon capture from ethanol production
        self.rest_cst_eth_om   = self.restart.ccatsdat_cst_eth_om.copy()    # O&M cost for carbon capture from ethanol production
        self.rest_sup_eth_45q  = self.restart.ccatsdat_sup_eth_45q.copy()   # CO2 volumes from ethanol production that are 45Q eligible
        self.rest_sup_eth_ntc  = self.restart.ccatsdat_sup_eth_ntc.copy()   # CO2 volumes from ethanol production, no tax credit

        # Hydrogen
        self.rest_cst_h2_inv   = self.restart.ccatsdat_cst_h2_inv.copy()    # Investment cost for carbon capture from hydrogen production
        self.rest_cst_h2_om    = self.restart.ccatsdat_cst_h2_om.copy()     # O&M cost for carbon capture from hydrogen production
        self.rest_sup_h2_45q   = self.restart.ccatsdat_sup_h2_45q.copy()    # CO2 volumes from hydrogen production that are 45Q eligible
        self.rest_sup_h2_ntc   = self.restart.ccatsdat_sup_h2_ntc.copy()    # CO2 volumes from hydrogen production, no tax credit.copy()


        ### Demand
        self.rest_dem_eor   = self.restart.ccatsdat_dem_eor.copy()  # CO2 demand from EOR volumes
        self.rest_cst_eor   = self.restart.ccatsdat_cst_eor.copy()  # CO2 price CO2 EOR projects are willing to offer for CO2

        self.rest_play_map  = self.restart.ogsmout_play_map.copy() # Play Mapping from restart file
        self.rest_play_map.index = list(range(self.rest_play_map.size))
        self.rest_play_map.index = self.rest_play_map.index + 1

        ### Prices
        self.rest_co2_prc_dis_45q = self.restart.ccatsdat_co2_prc_dis_45q.copy()   # CO2 price output after optimization, 45Q eligible CO2, Census Division
        self.rest_co2_prc_dis_ntc = self.restart.ccatsdat_co2_prc_dis_ntc.copy()   # CO2 price output after optimization, no tax credit, Census Division
        self.rest_co2_prc_reg_45q = self.restart.ccatsdat_co2_prc_reg_45q.copy()   # CO2 price output after optimization, 45Q eligible CO2, Census Region
        self.rest_co2_prc_reg_ntc = self.restart.ccatsdat_co2_prc_reg_ntc.copy()   # CO2 price output after optimization, no tax credit, Census Region


        ### Reporting
        self.rest_co2_sup_out   = self.restart.ccatsdat_co2_sup_out.copy()     # CO2 supply output after optimization
        self.rest_co2_seq_out   = self.restart.ccatsdat_co2_seq_out.copy()     # CO2 demand output after optimization
        self.rest_co2_sup_out_r = self.restart.ccatsdat_co2_sup_out_r.copy()   # CO2 supply output after optimization, realized
        self.rest_co2_seq_out_r = self.restart.ccatsdat_co2_seq_out_r.copy()   # CO2 demand output after optimization, realized

        ### Set model year reporting volumes to 0
        temp = self.rest_co2_sup_out.loc[self.rest_co2_sup_out.index.get_level_values(2) == self.year_current].copy()
        temp['value'] = 0
        self.rest_co2_sup_out.update(temp)

        temp = self.rest_co2_seq_out.loc[self.rest_co2_seq_out.index.get_level_values(2) == self.year_current].copy()
        temp['value'] = 0
        self.rest_co2_seq_out.update(temp)

        temp = self.rest_co2_sup_out_r.loc[self.rest_co2_sup_out_r.index.get_level_values(2) == self.year_current].copy()
        temp['value'] = 0
        self.rest_co2_sup_out_r.update(temp)

        temp = self.rest_co2_seq_out_r.loc[self.rest_co2_seq_out_r.index.get_level_values(2) == self.year_current].copy()
        temp['value'] = 0
        self.rest_co2_seq_out_r.update(temp)

        pass


    def aggregate_restart_variables(self):
        '''Aggregate localized restart variables for :ref:`preprocessor`.

        Parameters
        ----------
        None

        Returns
        -------
        self.rest_dem_eor : DataFrame
            DataFrame of CO\ :sub:`2` EOR CO\ :sub:`2` demand (metric tonnes) by play.

        self.rest_cst_eor : DataFrame
            DataFrame of CO\ :sub:`2` EOR price offers for CO\ :sub:`2` (1987$/tonne) by play.

        self.rest_industrial_co2_supply_45q : DataFrame
            DataFrame of industrial CO\ :sub:`2` supply (metric tonnes) eligible for 45Q tax credits.

        self.rest_industrial_co2_supply_ntc : DataFrame
            DataFrame of industrial CO\ :sub:`2` supply (metric tonnes) not eligible for 45Q tax credits.

        self.rest_industrial_co2_cost_inv : DataFrame
            DataFrame of carbon capture investment costs (1987$) by NEMS module and region.

        self.rest_industrial_co2_cost_om : DataFrame
            DataFrame of carbon capture O&M costs (1987$) by NEMS module and region.
        '''
        # Declare concatenated versions of localized restart variables
        self.rest_dem_eor                       = self.loc_rst.rest_dem_eor.copy()
        self.rest_cst_eor                       = self.loc_rst.rest_cst_eor.copy()
        self.rest_industrial_co2_supply_45q     = self.loc_rst.industrial_co2_supply_45q.copy()
        self.rest_industrial_co2_supply_ntc     = self.loc_rst.industrial_co2_supply_ntc.copy()
        self.rest_industrial_co2_cost_inv       = self.loc_rst.industrial_co2_cost_inv.copy()
        self.rest_industrial_co2_cost_om        = self.loc_rst.industrial_co2_cost_om.copy()

        pass


    def read_pkl(self):
        '''Call :meth:`ccats_pickle.CCATS_Pickle.read_pkl_vars` to read in pickle files from the previous model year fcrl and ncrl iterations.

            * Different pickle outputs are read in to CCATS depending on whether ncrl=1 (if we are running a reporting loop)
            * All non-reporting iterations use the previous model year fcrl=1 iteration variables
            * All reporting iterations use the previous model year ncrl=1 iteration variables

        Parameters
        ----------
        None

        Returns
        -------
        None

        '''
        ###Intermediate variable pre-processing
        # Clear old intermediate variable files
        self.logger.info('Delete old pickle variables to clear up memory')
        if (self.year_current == self.year_start) & (self.iteration_current == 1):  # Delete Existing pickle files to prevent duplication
            for filename in os.listdir(self.ccats_var_output_path):
                file_path = os.path.join(self.ccats_var_output_path, filename)
                try:
                    if 'git.keep' in os.path.isfile(file_path):
                        pass
                    elif os.path.isfile(file_path) or os.path.islink(file_path):
                        os.unlink(file_path)
                    elif os.path.isdir(file_path):
                        shutil.rmtree(file_path)
                except Exception as e:
                    print('Failed to delete %s. Reason: %s' % (file_path, e))
        else:  # Delete files from model year - 2 to clear out model bloat
            for filename in os.listdir(self.ccats_var_output_path):
                if str(self.year_current - 2) in filename:
                    file_path = os.path.join(self.ccats_var_output_path, filename)
                    try:
                        if os.path.isfile(file_path) or os.path.islink(file_path):
                            os.unlink(file_path)
                        elif os.path.isdir(file_path):
                            shutil.rmtree(file_path)
                    except Exception as e:
                        print('Failed to delete %s. Reason: %s' % (file_path, e))
                else:
                    pass


        ###Load CCATS local variables
        # Normal loop files (ncrl != 1)
        if (self.param_ncrl != 1) & (self.integrated_switch == 1) & (self.year_current > self.year_start):
            print('Loading FCRL Pickle Vars')
            self.pkl.read_pkl_vars('fcrl', self.year_current, self.ccats_var_output_path)

        # Reporting loop files (ncrl == 1)
        elif (self.param_ncrl == 1) & (self.integrated_switch == 1) & (self.year_current > self.year_start):  # Reporting loop files
            self.logger.info('Loading NCRL Pickle Vars')
            self.pkl.read_pkl_vars('ncrl', self.year_current, self.ccats_var_output_path)

        else:
            self.logger.warning('No Pickle Vars read')
            pass
        pass


    #######################################
    ###CCATS MODEL RUN
    #######################################

    def run(self, year=None, iteration=None):
        """Runs main CCATS Model Processes.

            * :ref:`Preprocessor`
            * :ref:`CCATS Optimization`
            * :ref:`Postprocessor`
            * :ref:`Output`

        Parameters
        ----------
        year : int
            Current model year.

        iteration : int
            Current model iteration.

        Returns
        -------
        self.year_current : DataFrame
            Current model year.

        self.iteration_current : DataFrame
            Current model iteration.
        """
        ###Years
        if self.integrated_switch:
            self.year_current       = self.restart.ncntrl_curcalyr
            self.iteration_current  = self.restart.ncntrl_curitr
        else:
            self.year_current = year
            self.iteration_current = 1


        # Set current model year and iteration
        if year is not None:
            self.logger.warning('Overriding ncntrl_curcalyr')
            self.year_current = year
        if iteration is not None:
            self.logger.warning('Overriding ncntrl_curitr')
            self.iteration_current = iteration

        # Run Preprocessor Submodule
        if self.preproc_switch:
            self.logger.info('Running Preprocessor Submodule')
            self.preproc.run()

        ### Run Main Optimization Submodules
        # Run CCATS Optimization
        if self.ccats_opt_switch:
            self.logger.info('Running CCATS Optimization')
            self.ccats_opt.run()  # Optimization model

        # Run Postprocessor Submodule
        if self.postproc_switch:
            self.logger.info('Running Postprocessor Submodule')
            self.postproc.run() #Postprocessor

        # Run Output Submodule
        if self.output_switch:
            self.logger.info('Running Output Submodule')
            self.output.run()

        if self.pytest_switch:
            self.logger.info('Runnning Pytests')
            self.pytest.logger = self.logger
            self.pytest.run()

        pass
