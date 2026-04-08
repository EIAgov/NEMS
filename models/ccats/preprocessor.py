"""Submodule for Preprocessing CCATS data during runtime.

.. _preprocessor:

Preprocessor: Summary
_____________________
This submodule preprocesses CCATS data from the restart file and input files and prepares it for use in the main CCATS optimization model.
The preprocessor runs as follows:

    1. :meth:`~preprocessor.Preprocessor.setup` is called from :ref:`module`.

    2. In :meth:`~preprocessor.Preprocessor.setup`, input variables are read in from *preproc_setupd.csv*, then :meth:`~preprocessor.Preprocessor.load_inputs_restart`,
       :meth:`~preprocessor.Preprocessor.load_inputs_csv` and :meth:`~preprocessor.Preprocessor.load_inputs_pkl` are called to read in input DataFrames.

    3. Other model year 1 setup processes are performed, (i.e. input costs are inflation adjusted in :meth:`~preprocessor.Preprocessor.harmonize_costs_inflation` and 45Q tax
       credit values are assigned to demand and storage input DataFrames in :meth:`~preprocessor.Preprocessor.assign_tax_credits`).

    4. :ref:`preprocessor` setup ends.

    5. :meth:`~preprocessor.Preprocessor.run` is called from :ref:`module`.

    6. Instantiate model year process DataFrames:

        a. Main model year CO\ :sub:`2` supply facility  pipeline lookup, storage, and EOR demand DataFrames are instantiatied in
           :meth:`~preprocessor.Preprocessor.instantiate_model_year_dfs`.

        b. Block-level model year CO\ :sub:`2` supply and EOR demand DataFrames are instantiated in :meth:`~preprocessor.Preprocessor.setup_co2_supply_blocks`
           and :meth:`~preprocessor.Preprocessor.setup_co2_eor_blocks`, respectively.

        c. Model year pipeline infrastructure is declared based on generalized Department of Transportation (DOT) pipeline network and previous model year results in
           :meth:`~preprocessor.Preprocessor.declare_existing_pipe_infrastructure`.

    7. Produce a point-source CO\ :sub:`2` supply roster for the main CCATS optimization:

        a. CO\ :sub:`2` supply sources from the NETL CCRD database have their prices adjusted based on technology improvement
           (:meth:`~preprocessor.Preprocessor.apply_tech_rate_capture_costs`) and transport costs (:meth:`~preprocessor.Preprocessor.add_transport_costs_to_netl_capture_costs`).

        b. Assign NEMS capture costs to Facility Aggregated Nodes (FANs) in :meth:`~preprocessor.Preprocessor.assign_nems_capture_costs`.

        c. Match CO\ :sub:`2` supply from NEMS against the point-source facility rosters in *self.ccs_facility_year_df* and *self.cluster_facility_df*,
           based on facility capture cost, producing block-level CO\ :sub:`2` supply facility rosters for the main optimization.

        d. Set 45Q eligibility status for each block-level CO\ :sub:`2` supply facility roster, and adjust capital costs down for the next model year.

    8. Produce a transportation roster for the main CCATS optimization:

        a. Make the dataset sparse in :meth:`~preprocessor.Preprocessor.filter_available_pipelines` based on selected CO\ :sub:`2` supply
           faciliites and available CO\ :sub:`2` EOR and saline formation storage sites.

        b. Assign pipeline electricity costs in :meth:`~preprocessor.Preprocessor.assign_electricity_costs`.

    9. Produce a CO\ :sub:`2` saline formation storage roster for the main CCATS optimization

        a. Update saline formation storage capacity and injectivity based on previous model year results

    10. Setup inputs to CCATS optimization.

    11. Run CCATS preprocessor utiliites (i.e. pickle variables, debug outputs)


Preprocessor: Input Files
_________________________

    * preproc_setup.csv - preprocessor setup file
    * storage_formations_lookup.csv - Lookup of saline formation attributes
    * co2_facility_lookup.csv - Lookup of CO\ :sub:`2` capture facility attributes
    * master_pipeline_lookup_multi.csv - Lookup of CO\ :sub:`2` pipeline transportation attributes
    * ts_multiplier.csv - Set of CO\ :sub:`2` multipliers for TS arcs by year
    * hsm_eor_centroid.csv - CO\ :sub:`2` demand from HSM centroids during STEO years


Preprocessor: Model Functions and Class Methods
_______________________________________________

Setup
-----


    * :meth:`~preprocessor.Preprocessor.__init__` - Constructor to initialize Preprocessor submodule (instantiated by :meth:`module.Module.setup` in :ref:`module`)
    * :meth:`~preprocessor.Preprocessor.setup` - Setup Preprocessor submodule for CCATS (called by :meth:`module.Module.setup`).
    * :meth:`~preprocessor.Preprocessor.load_inputs_restart` - Load CCATS inputs from Restart File (called by :meth:`~preprocessor.Preprocessor.setup`).
    * :meth:`~preprocessor.Preprocessor.load_inputs_csv` - Load CCATS inputs from .csv files (called by :meth:`~preprocessor.Preprocessor.setup`).
    * :meth:`~preprocessor.Preprocessor.load_inputs_pkl` - Load CCATS inputs from .pkl files (called by :meth:`~preprocessor.Preprocessor.setup`).
    * :meth:`~preprocessor.Preprocessor.harmonize_costs_inflation` - Harmonize model input costs using NEMS inflation multipliers (called by :meth:`~preprocessor.Preprocessor.setup`).
    * :meth:`~preprocessor.Preprocessor.filter_pnw` - Assess Pacific Northwest (PNW) EMM region's viability for CCS activities (called by :meth:`~preprocessor.Preprocessor.setup`).


    
Run
---

    * :meth:`~preprocessor.Preprocessor.run` - Run Preprocessor Submodule for CCATS (called by :meth:`module.Module.run`).

    
Model Year Instantiations
-------------------------

    * :meth:`~preprocessor.Preprocessor.assign_tax_credits` - Assign 45Q tax credits values to storage and demand input DataFrames (called by :meth:`~preprocessor.Preprocessor.run`).
    * :meth:`~preprocessor.Preprocessor.instantiate_model_year_dfs` - Instantiate model year DataFrames (i.e. model year pipeline lookup, available saline formations, etc.) (called by :meth:`~preprocessor.Preprocessor.run`).
    * :meth:`~preprocessor.Preprocessor.account_for_facility_year_operations` - Account for the number of years a facility has been operational (called by :meth:`~preprocessor.Preprocessor.run`).
    * :meth:`~preprocessor.Preprocessor.setup_co2_supply_blocks` - Instantiate model DataFrames for CO\ :sub:`2` supply (called by :meth:`~preprocessor.Preprocessor.run`).
    * :meth:`~preprocessor.Preprocessor.setup_co2_eor_blocks` - Setup model year DataFrames for CO\ :sub:`2` EOR demand (called by :meth:`~preprocessor.Preprocessor.run`).
    * :meth:`~preprocessor.Preprocessor.declare_existing_pipe_infrastructure` - Declare and update existing pipeline network (called by :meth:`~preprocessor.Preprocessor.run`).

    
Produce CO\ :sub:`2` supply source roster
-----------------------------------------

    * :meth:`~preprocessor.Preprocessor.apply_tech_rate_capture_costs` - Apply tech learning rate to NETL capture costs (called by :meth:`~preprocessor.Preprocessor.run`).
    * :meth:`~preprocessor.Preprocessor.assign_nems_capture_costs` - Assign CO\ :sub:`2` capture cost ($/tonne) for net-new facilities based on restart file variables (called by :meth:`~preprocessor.Preprocessor.run`).
    * :meth:`~preprocessor.Preprocessor.get_co2_facility_rosters` - Determine rosters of available CO\ :sub:`2` capture facilities (called by :meth:`~preprocessor.Preprocessor.run`).
    * :meth:`~preprocessor.Preprocessor.add_transport_costs_to_netl_capture_costs` - Prioritize CO\ :sub:`2` capture facilities based on transport connection costs (called by :meth:`~preprocessor.Preprocessor.run`).
    * :meth:`~preprocessor.Preprocessor.determine_point_source_supplies` - Match CO\ :sub:`2` supplies against the NETL curve and produce a DataFrame of model year supply sources (called by :meth:`~preprocessor.Preprocessor.run`).
    * :meth:`~preprocessor.Preprocessor.determine_facility_eligibility_45q` - Determine 45Q eligibility for CO\ :sub:`2` capture facilities by model block (called by :meth:`~preprocessor.Preprocessor.run`).
    * :meth:`~preprocessor.Preprocessor.zero_inv_costs_for_retrofit_facilities` - Zeroes out investment costs for facilities which have been retrofit (called by :meth:`~preprocessor.Preprocessor.run`).

    
Update CO\ :sub:`2` transportation roster
-----------------------------------------

    * :meth:`~preprocessor.Preprocessor.filter_available_pipelines` - Remove inactive CO\ :sub:`2` supply, storage and CO\ :sub:`2` EOR nodes from the pipeline network to make more sparse (called by :meth:`~preprocessor.Preprocessor.run`).
    * :meth:`~preprocessor.Preprocessor.assign_electricity_costs` - Assign electricity costs ($/MWh) for pipelines (called by :meth:`~preprocessor.Preprocessor.run`).

    
Update CO\ :sub:`2` saline formation storage roster
---------------------------------------------------

    * :meth:`~preprocessor.Preprocessor.update_storage_infrastructure` - Declare and update existing storage infrastructure based on last model year results (called by :meth:`~preprocessor.Preprocessor.run`).

    
Setup CCATS Optimization
------------------------

    * :meth:`~preprocessor.Preprocessor.declare_supply_opt` - Declare supply DataFrames for the main CCATS optimization (called by :meth:`~preprocessor.Preprocessor.run`).
    * :meth:`~preprocessor.Preprocessor.prepare_storage_opt` - Prepare storage DataFrames for the main CCATS optimization (called by :meth:`~preprocessor.Preprocessor.run`).
    * :meth:`~preprocessor.Preprocessor.prepare_eor_opt` - Prepare EOR DataFrames for the main CCATS optimization (called by :meth:`~preprocessor.Preprocessor.run`).
    * :meth:`~preprocessor.Preprocessor.setup_optimization` - Format and prepare data in optimization DataFrames for optimization (called by :meth:`~preprocessor.Preprocessor.run`).
    * :meth:`~preprocessor.Preprocessor.instantiate_pyomo_series` - Declare Pyomo optimization inputs as series (called by :meth:`~preprocessor.Preprocessor.run`).

    
CCATS Preprocessor Utilities
----------------------------

    * :meth:`~preprocessor.Preprocessor.write_pkl` - Write CCATS variables that need to be passed between model iterations via .pkl files (called by :meth:`~preprocessor.Preprocessor.run`).
    * :meth:`~preprocessor.Preprocessor.summarize_inputs` - Output debug file of preprocessor data summary (called by :meth:`~preprocessor.Preprocessor.run`).
    * :meth:`~preprocessor.Preprocessor.visualize_inputs` - Visualize preprocessed data (called by :meth:`~preprocessor.Preprocessor.run`).


Preprocessor: Output Debug Files
________________________________

Some debug files are output multiple times during a NEMS run, and use the following convention:

    * <model> is the name of the model (default is ccats_opt),
    * <block> is the number of the current block (0, 1, or 2),
    * <year> is the current model year, for example 2024,
    * <iteration> is the current NEMS iteration, and
    * <cycle> is the current NEMS cycle.

* In debug// (output by :meth:`~preprocessor.Preprocessor.summarize_inputs`):
    * **<model>_data_stats_preproc.csv** - High level summary of model inputs
* In debug//tech_learning (output by :meth:`~preprocessor.Preprocessor.apply_tech_rate_capture_costs`):
    * **tech_learning_capture.csv** - Tech learning rate for carbon capture technologies by year
* In debug//tech_learning (output by :meth:`~preprocessor.Preprocessor.instantiate_pyomo_series`):
    * **tech_learning_transport_storage.csv** - Tech learning rate for carbon capture technologies by year
* In debug//optimization_inputs//supply (output by :meth:`~preprocessor.Preprocessor.determine_facility_eligibility_45q`):
    * **before_45q_supply_sources_sparse_<block>_df_<year>.csv** - Point-source CO\ :sub:`2` supply sources before being updated for 45Q eligibility
    * **after_45q_supply_sources_sparse_<block>_df_<year>.csv** - Point-source CO\ :sub:`2` supply sources after being updated for 45Q eligibility
* In debug//optimization_inputs//elec_costs (output by :meth:`~preprocessor.Preprocessor.assign_electricity_costs`):
    * **elec_prices_<year>_<iteration>_<cycle>.csv** - Electricity prices used by CCATS main optimization
* In debug//optimization_inputs//elec_costs (output by :meth:`~preprocessor.Preprocessor.instantiate_pyomo_series`):
    * **opex_transport_elec_<year>.csv** - Electricity opex ($/tonne) used by CCATS main optimization
    * **electricity_demand_<year>.csv** - Electricity demand (MWh/tonne) used by ccats main optimization
* In debug//optimization_inputs//storage (output by :meth:`~preprocessor.Preprocessor.instantiate_pyomo_series`):
    * **storage_existing_opt.csv** - Existing storage capacity in the CCATS main optimization 
    * **storage_new_opt.csv** - Potential for new storage capacity in the CCATS main optimization
    * **storage_opt_inputs.csv** - Main storage inputs into the CCATS main optimization storage (storage AORs available and operating, injectivity, etc.)
* In debug//optimization_inputs//transport (output by :meth:`~preprocessor.Preprocessor.instantiate_pyomo_series`):
    * **pipeline_network_<year>.csv** - Available arcs for pipeline network in the CCATS main optimization
* In debug//financing (output by :meth:`~preprocessor.Preprocessor.instantiate_pyomo_series`):
    * **discount_multipliers.csv** - Block durations and discount multipliers by variable used in the CCATS main optimization


Preprocessor: Output Restart Variables
______________________________________
None


Preprocessor: Code
__________________
"""

import pandas as pd
import numpy as np
from ccats_common import common as com
import submodule_ccats as subccats


class Preprocessor(subccats.Submodule):
    """Preprocessor Submodule for CCATS.

    """
    def __init__(self, parent):
        """Initializes Preprocessor object.

        Parameters
        ----------
        parent : str
            Module.Module (Pointer to parent module).

        Returns
        -------
        self.i_source_data : DataFrame
            DataFrame input of sources to be modeled.

        self.i_source_annual_capacity_existing : DataFrame
            DataFrame input of existing annual source capacity.

        self.i_source_total_capacity_existing : DataFrame
            DataFrame input of existing total source capacity.
        
        self.source_kinds : list
            List of kinds of sources.

        self.vintages_source : list
            List of vintages of sources (year constructed).

        self.cost_invest_source : DataFrame
            Source investment costs, indexed by kind of source and node (k,n).

        self.cost_variable_source : DataFrame
            Source variable costs, indexed by kind of source and node (k,n).
        
        self.build_limit_source_node : Series
            Source node build limit, indexed by kind of source (k).
        
        self.build_limit_source_total : Series
            Source nationwide build limit, indexed by kind of source (k).

        self.capacity_exist_source : Series
            Existing source capacity (indexed by kind of source and node, k and n).

        self.policy_eligibility_source : Series
            Source policy eligiblity indexed by kind of source, and vintage (k,v).

        self.cost_invest_source_b0 : Series
            Investment source costs for block 0 (indexed by kind of source and node, k and n).
        
        self.cost_invest_source_b1 : Series
            Investment source costs for block 1 (indexed by kind of source and node, k and n).
        
        self.cost_invest_source_b0 : Series
            Investment source costs for block 2 (indexed by kind of source and node, k and n).

        self.cost_variable_source_b0 : Series
            Variable source costs for block 0 (indexed by kind of source and node, k and n).

        self.cost_variable_source_b1 : Series
            Variable source costs for block 1 (indexed by kind of source and node, k and n).

        self.cost_variable_source_b2 : Series
            Variable source costs for block 2 (indexed by kind of source and node, k and n).

        self.discount_invest_source_b0 : float
            Investment discount multiplier for source for block 0.
        
        self.discount_invest_source_b1 : float
            Investment discount multiplier for source for block 1.
        
        self.discount_invest_source_b2 : float
            Investment discount multiplier for source for block 2.
        
        self.build_limit_source_node_b0 : Series
            Built limit at a single node for block 0 (indexed by kind of source, k).
        
        self.build_limit_source_node_b1 : Series
            Built limit at a single node for block 0 (indexed by kind of source, k).
        
        self.build_limit_source_node_b2 : Series
            Built limit at a single node for block 0 (indexed by kind of source, k).
        
        self.build_limit_source_total_b0 : float
            Nationwide build limit for block 0.

        self.build_limit_source_total_b1 :float
            Nationwide build limit for block 1.

        self.build_limit_source_total_b2 : float
            Nationwide build limit for block 2.

        """
        ### I/O Setup
        self.parent = parent  # module.Module head module

        ### Input Variables
        self.small_sample_division          = 0     # Census Division tested when running a "small sample" test
        self.block_45q_yr                   = 0     # Decision variable for number of 45Q eligibility years remaining in model (see method: determine_facility_eligibility_45q)
        self.new_pipes_to_exist_facilities  = 0     # First year that new pipelines can be built to existing capture facilities (first operational in the following year)
        self.tech_learn_ammonia             = 0.0   # Tech rate for capture at ammonia facilities
        self.tech_learn_ethanol             = 0.0   # Tech rate for capture at ethanol facilities
        self.tech_learn_cement              = 0.0   # Tech rate for capture at cement facilities
        self.tech_learn_ng_processing       = 0.0   # Tech rate for capture at natural gas processing
        self.tech_learn_other               = 0.0   # Tech rate for capture at all other facilities
        self.tech_learn_power               = 0.0   # Tech rate for capture at power plants (coal and natural gas)
        self.tech_learn_transport           = 0.0   # Tech rate for carbon transport
        self.tech_learn_storage             = 0.0   # Tech rate for carbon storage
        self.transport_buffer               = 0.0   # Buffer (percentage) added above CO2 flow to transport network during optimization
        self.storage_buffer                 = 0.0   # Buffer (percentage) added above CO2 flow to storage network during optimization

        ### Switches
        self.small_sample_switch        = False # Model solves for only a single census division for testing if TRUE
        self.split_45q_supply_switch    = False # Split CO2 supply into 45Q and NTC components if TRUE
        self.colocate_dac_ts_switch     = False # Enable colocating direct air capture at ts nodes

        ### Input dfs
        self.i_industrial_supply_45q_df = pd.DataFrame() # DataFrame of CO2 supply 45Q eligible from NEMS modules
        self.i_industrial_supply_ntc_df = pd.DataFrame() # DataFrame of CO2 supply no tax credit from NEMS modules
        self.i_nems_facility_cost_df    = pd.DataFrame() # DataFrame of CO2 investment costs from NEMS modules
        self.i_co2_supply_facility_df   = pd.DataFrame() # DataFrame of CO2 facilities and associated costs from NETL
        self.i_storage_df               = pd.DataFrame() # DataFrame of storage formations
        self.i_pipeline_lookup_df       = pd.DataFrame() # DataFrame of CO2 pipeline lookup
        self.i_eor_demand_df            = pd.DataFrame() # DataFrame of CO2 EOR site CO2 demanded
        self.i_eor_cost_net_df          = pd.DataFrame() # DataFrame of CO2 EOR cost maximum for CO2
        self.i_ts_multiplier_df         = pd.DataFrame() # DataFrame of multipliers for ts-ts node arcs


        ### Process dfs
        # General Process dfs
        self.storage_new_df                 = pd.DataFrame() # DataFrame of storage formations
        self.storage_existing_df            = pd.DataFrame() # DataFrame of existing CO2 storage infrastructure in a given model year
        self.co2_supply_facility_df         = pd.DataFrame() # DataFrame of CO2 cost facility data and costs from NETL
        self.year_nems_facility_cost_df     = pd.DataFrame() # DataFrame of model year CO2 capture costs from NEMS modules
        self.pipeline_lookup_df             = pd.DataFrame() # DataFrame of CO2 pipeline lookup
        self.pipes_existing_df              = pd.DataFrame() # DataFrame of existing CO2 pipeline infrastructure in a given model year
        self.year_industrial_supply_45q_df  = pd.DataFrame() # DataFrame of 45Q eligible CO2 supply from CATF for model year
        self.year_industrial_supply_ntc_df  = pd.DataFrame() # DataFrame of 45Q ineligible CO2 supply from restart file for model year
        self.supply_sources_df              = pd.DataFrame() # DataFrame of available CO2 supply sources
        self.supply_sources_sparse_df       = pd.DataFrame() # DataFrame of available CO2 supply sources for model year
        self.eor_demand_df                  = pd.DataFrame() # DataFrame of CO2 EOR site CO2 demanded for model year
        self.eor_cost_net_df                = pd.DataFrame() # DataFrame of CO2 EOR net cost for CO2 for model year
        self.sequester_existing_df          = pd.DataFrame() # DataFrame of existing sequestration nodes
        self.sequester_new_df               = pd.DataFrame() # DataFrame of new sequestration nodes
        self.preproc_df                     = pd.DataFrame() # DataFrame of preproc statistcal outputs for debugging
        self.co2_facility_eligibility_df    = pd.DataFrame() # DataFrame of co2 capture facilities and their eligibility


        # Block Process dfs
        self.year_industrial_supply_b0_df   = pd.DataFrame() # DataFrame of model year CO2 supply for model time period 0
        self.year_industrial_supply_b1_df   = pd.DataFrame() # DataFrame of model year CO2 supply for model time period 1
        self.year_industrial_supply_b2_df   = pd.DataFrame() # DataFrame of model year CO2 supply for model time period 2
        self.supply_sources_sparse_b0_df    = pd.DataFrame() # DataFrame of sparse model year CO2 supply sources for time period 0
        self.supply_sources_sparse_b1_df    = pd.DataFrame() # DataFrame of sparse model year CO2 supply sources for time period 1
        self.supply_sources_sparse_b2_df    = pd.DataFrame() # DataFrame of sparse model year CO2 supply sources for time period 2
        self.co2_supply_b0_df               = pd.DataFrame() # DataFrame of time period 0 CO2 supply sources and volumes for the main optimization
        self.co2_supply_b1_df               = pd.DataFrame() # DataFrame of time period 1 CO2 supply sources and volumes for the main optimization
        self.co2_supply_b2_df               = pd.DataFrame() # DataFrame of time period 2 CO2 supply sources and volumes for the main optimization


        ### Optimization Output dfs
        self.pipeline_lookup_opt        = pd.DataFrame() # DataFrame of model year new pipeline lookup for optimization
        self.pipes_existing_opt         = pd.DataFrame() # DataFrame of model year existing pipeline lookup for optimization
        self.built_storage_opt          = pd.DataFrame() # DataFrame of model year existing CO2 pipeline infrastructure
        self.supply_sources_opt         = pd.DataFrame() # DataFrame of model year supply source data for optimization
        self.sequester_new_opt          = pd.DataFrame() # DataFrame of model year new sequestration nodes


        ### Preprocessor summary data df
        self.preproc_df             = pd.DataFrame() # DataFrame of preprocessor inputs to optimization

        #============================
        ### Endogenous sources of CO2
        #============================
        
        #----------------------------
        ## Input Data
        # ---------------------------
        # Natural CO2 Fields
        self.i_nat_co2_lookup_df = pd.DataFrame() # DataFrame of natural CO2 fields

        self.i_source_data = pd.DataFrame() # sources to be modeled

        # capacity
        self.i_source_annual_capacity_existing   = pd.Series() # Annual capacity existing before NEMS begins
        self.i_source_total_capacity_existing    = pd.Series() # Total capacity existing before NEMS begins

        
        # capacity
        self.i_source_capacity_limits     = pd.Series() # source capacity limits by kind and node

        #----------------------------
        ## Preprocessor variables
        # ---------------------------

        # kinds of sources
        self.source_kinds = []

        # costs
        self.cost_invest_source   = pd.Series()
        self.cost_variable_source = pd.Series()

        # build limits
        self.build_limit_source_node  = pd.Series()
        self.build_limit_source_total = pd.Series()

        #----------------------------
        ## Optimization - Sets
        # ---------------------------
        # Types of source
        self.kinds_source = [] # List of values

        # Locations for sources
        self.nodes_source = [] # List of values

        # Source vintages (year constructed)
        self.vintages_source = [] # List of values
        
        #---------------------------
        ## Optimization - Parameters
        #----------------------------
        # existing capacity
        self.capacity_exist_source       = pd.Series() # MultiIndex - Indexed by kind of source, node and vintage (k,n,v)

        # policy eligibility (by block)
        self.policy_eligibility_source_exist_b0  = pd.Series() # Multiindex - Indexed by kind of source, policy, and vintage (k,p,v) - block 0
        self.policy_eligibility_source_exist_b1  = pd.Series() # Multiindex - Indexed by kind of source, policy, and vintage (k,p,v) - block 1
        self.policy_eligibility_source_exist_b2  = pd.Series() # Multiindex - Indexed by kind of source, policy, and vintage (k,p,v) - block 2
        self.policy_eligibility_source_new_b0    = pd.Series() # Multiindex - Indexed by kind of source, and policy (k,p) - block 0
        self.policy_eligibility_source_new_b1    = pd.Series() # Multiindex - Indexed by kind of source, and policy (k,p) - block 1
        self.policy_eligibility_source_new_b2    = pd.Series() # Multiindex - Indexed by kind of source, and policy (k,p) - block 2

        # investment costs by block
        self.cost_invest_source_b0       = pd.Series() # MultiIndex - Indexed by kind of source and node (k,n) - block 0
        self.cost_invest_source_b1       = pd.Series() # MultiIndex - Indexed by kind of source and node (k,n) - block 1
        self.cost_invest_source_b2       = pd.Series() # MultiIndex - Indexed by kind of source and node (k,n) - block 2

        # variable costs by block
        self.cost_variable_source_b0     = pd.Series() # MultiIndex - Indexed by kind of source and node (k,n) - block 0
        self.cost_variable_source_b1     = pd.Series() # MultiIndex - Indexed by kind of source and node (k,n) - block 1
        self.cost_variable_source_b2     = pd.Series() # MultiIndex - Indexed by kind of source and node (k,n) - block 2

        # financing
        self.discount_invest_source_b0   = 0.0 # Single value - block 0
        self.discount_invest_source_b1   = 0.0 # Single value - block 1
        self.discount_invest_source_b2   = 0.0 # Single value - block 2
        
        # Built limit at a single node by block
        self.build_limit_source_node_b0  = pd.Series() # Single Index - Indexed by kind of source and node (k,n) - block 0
        self.build_limit_source_node_b1  = pd.Series() # Single Index - Indexed by kind of source and node (k,n) - block 1
        self.build_limit_source_node_b2  = pd.Series() # Single Index - Indexed by kind of source and node (k,n) - block 2

        # Built limit at a single node b- cuulative
        self.build_limit_source_node_cumulative = pd.Series() # Single Index - Indexed by kind of source and node (k,n)
        
        # Nationwide build limit by block
        self.build_limit_source_total_b0 = 0.0 # Single value - block 0
        self.build_limit_source_total_b1 = 0.0 # Single value - block 1
        self.build_limit_source_total_b2 = 0.0 # Single value - block 2

        pass


    #####################################
    ###Preprocessor Setup Processes
    #####################################

    def setup(self, setup_filename):
        """Setup Preprocessor Submodule for CCATS.

        Parameters
        ----------
        setup_filename : str
            Path to preprocessor setup file.

        Returns
        -------
        self.logger : Logger
            Logger utility, declared in parent.

        self.input_path : str
            Model input path, declared in parent.
            
        self.output_path : str
            Model output path, declared in parent.

        self.preproc_input_path : str
            Preprocessor submodule input path.

        self.setup_table : DataFrame
            Setup table for Preprocessor.py, with input values and switches relating to the submodule.

        self.small_sample_switch : DataFrame
            Model solves for only a single census division for testing if TRUE.

        self.split_45q_supply_switch : DataFrame
            Split CO\ :sub:`2` supply into 45Q and NTC components if TRUE.

        self.small_sample_division : int
            Census Division tested when running a "small sample" test.

        self.block_45q_yr : int
            Decision variable for number of 45Q eligibility years remaining in model (see method: determine_facility_eligibility_45q).

        self.new_pipes_to_exist_facilities  : int
            First year that new pipelines can be built to existing capture facilities (first operational in the following year).

        self.tech_learn_ammonia : float
            Tech rate for capture at ammonia facilities.

        self.tech_learn_ethanol : float
            Tech rate for capture at ethanol facilities.

        self.tech_learn_cement : float
            Tech rate for capture at cement facilities.

        self.tech_learn_ng_processing : float
            Tech rate for capture at natural gas processing.

        self.tech_learn_other : float
            Tech rate for capture at all other facilities.

        self.tech_learn_power : float
            Tech rate for capture at power plants (coal and natural gas).

        self.tech_learn_transport : float
            Tech rate for carbon transport.

        self.tech_learn_storage : float
            Tech rate for carbon storage.

        self.transport_buffer : float
            Buffer (fraction) added above CO\ :sub:`2` flow to transport network during optimization.

        self.storage_buffer : float
            Buffer (fraction) added above CO\ :sub:`2` flow to storage network during optimization.
        
        self.self.colocate_dac_ts_switch : bool
            Enable co-locating direct air capture at ts nodes

        self.i_source_data : DataFrame
            DataFrame input of sources to be modeled.

        self.i_source_annual_capacity_existing : DataFrame
            DataFrame input of existing source capacity.

        self.i_source_total_capacity_existing : DataFrame
            DataFrame input of existing source capacity.

        self.i_source_capacity_limits : DataFrame
            DataFrame input of builds limits for endogenous sources by node.
        """
        super().setup(setup_filename)

        ### Localize parent variables
        self.logger = self.parent.logger
        self.input_path = self.parent.input_path
        self.output_path = self.parent.output_path


        ### General Setup
        # Declare Input path
        self.preproc_input_path = self.input_path + 'preprocessor\\'

        # Read in setup file
        self.setup_table = com.read_dataframe(self.input_path + setup_filename, index_col=0)

        ### Switches
        self.small_sample_switch = self.setup_table.at['small_sample_switch', 'value'].upper() == 'True'.upper()
        self.split_45q_supply_switch = self.setup_table.at['split_45q_supply_switch', 'value'].upper() == 'True'.upper()  # If TRUE, split a facility's supply into 45Q and NTC components within a block
        self.colocate_dac_ts_switch = self.setup_table.at['colocate_dac_ts_switch', 'value'].upper() == 'True'.upper()  # If TRUE, enables co-locating direct air capture at ts nodes


        ### Variables
        self.small_sample_division         = int(self.setup_table.at['small_sample_division',           'value']) #Census division of small sample
        self.transport_buffer              = float(self.setup_table.at['transport_buffer',              'value']) # Build buffer for transport
        self.storage_buffer                = float(self.setup_table.at['storage_buffer',                'value']) # Build buffer for storage
        self.tech_learn_ammonia            = float(self.setup_table.at['tech_learn_ammonia',            'value']) # Tech learning - hydrogen capture (fr/yr)
        self.tech_learn_cement             = float(self.setup_table.at['tech_learn_cement',             'value']) # Tech learning - cement capture (fr/yr)
        self.tech_learn_ethanol            = float(self.setup_table.at['tech_learn_ethanol',            'value']) # Tech learning - ethanol capture (fr/yr)
        self.tech_learn_ng_processing      = float(self.setup_table.at['tech_learn_ng_processing',      'value']) # Tech learning - nat gas processing capture (fr/yr)
        self.tech_learn_power              = float(self.setup_table.at['tech_learn_power',              'value']) # Tech learning - power plant capture (fr/yr)
        self.tech_learn_other              = float(self.setup_table.at['tech_learn_other',              'value']) # Tech learning - other capture (fr/yr)        
        self.tech_learn_transport          = float(self.setup_table.at['tech_learn_transport',          'value']) # Tech learning - transport (fr/yr)
        self.tech_learn_storage            = float(self.setup_table.at['tech_learn_storage',            'value']) # Tech learning - storage (fr/yr)
        self.block_45q_yr                  = int(  self.setup_table.at['block_45q_yr',                  'value']) # Years within a block that a facility must be 45Q elgibile to get tax credit (only applies if split_45q_supply_switch is FALSE)
        self.new_pipes_to_exist_facilities = int(  self.setup_table.at['new_pipes_to_exist_facilities', 'value']) # first year that new pipelines can be built to existing capture facilities (first operational in the following year)

        ## Endogenous sources of CO2

        # Read in the names of tables to import
        filepath_source_data              = str(self.setup_table.at['source_data',  'value']) # file containing source data
        filepath_source_annual_existing_capacity = str(self.setup_table.at['source_annual_existing_capacity', 'value'])
        filepath_source_total_existing_capacity = str(self.setup_table.at['source_total_existing_capacity', 'value'])
        filepath_source_capacity_limits   = str(self.setup_table.at['source_capacity_limits', 'value'])

        # Read in tables as DataFrames
        self.i_source_data       = pd.read_csv(self.preproc_input_path + filepath_source_data, skiprows=1)
        annual_capacity_existing = pd.read_csv(self.preproc_input_path + filepath_source_annual_existing_capacity)
        total_capacity_existing = pd.read_csv(self.preproc_input_path + filepath_source_total_existing_capacity)
        source_capacity_limits   = pd.read_csv(self.preproc_input_path + filepath_source_capacity_limits)

        # Process source existing capacity
        self.i_source_annual_capacity_existing = annual_capacity_existing.groupby(['kind','node','vintage']).sum()['capacity']
        self.i_source_total_capacity_existing  = total_capacity_existing.groupby(['kind','node','vintage']).sum()['capacity']


        # Process source build limits
        self.i_source_capacity_limits = pd.melt(source_capacity_limits, id_vars=['node'], var_name='kind',value_name='capacity')
        self.i_source_capacity_limits = self.i_source_capacity_limits.set_index(['kind','node'])


        ### Run setup methods
        self.logger.info('Load Inputs Restart')
        self.load_inputs_restart()

        # Load csv files year 1, then pkl for maintaining local variables
        if (self.parent.integrated_switch == 0) | (int(self.parent.year_current) <= self.parent.year_start):
            self.logger.info('Load Inputs CSV')
            self.load_inputs_csv() # Load real data inputs
        else:
            self.logger.info('Load Inputs PKL')
            self.load_inputs_pkl()  # Load real data inputs

       # Direct Air Capture
        if (self.parent.endogenous_source_switch) and (self.colocate_dac_ts_switch):
            if (self.parent.integrated_switch==False) | ((self.parent.integrated_switch==True) & (self.parent.year_current == self.parent.year_start)):
                self.logger.info('Co-locate direct air capture with ts nodes') # Only need to set-up during the start year
                self.colocate_dac_with_ts_nodes()

        # Harmonize costs to inflation
        self.logger.info('Harmonize Costs to Inflation')
        self.harmonize_costs_inflation()

        # Filter PNW for new CCS builds
        # PNW has lots of electricity generation, but limited CCS options, so pull FANs out of the representation if no builds in the previous cycle
        if self.parent.year_current == self.parent.year_start:
            self.logger.info('Filter PNW for new CCS builds')
            self.filter_pnw()

        pass


    def load_inputs_restart(self):
        '''Load CCATS inputs from Restart File.

            * Read in relevant DataFrames from parent module, and
            * Assign instantiate "eligibility_45q" column in each supply DataFrame.

        Returns
        -------
        self.i_industrial_supply_45q_df : DataFrame
            DataFrame of 45Q eligibile CO\ :sub:`2` supply from NEMS.

        self.i_industrial_supply_ntc_df : DataFrame
            DataFrame of CO\ :sub:`2` supply from NEMS that is not eligibile for a tax credit (NTC = no tax credit).

        self.i_nems_facility_cost_df : DataFrame
            DataFrame of CO\ :sub:`2` capture costs from NEMS.

        '''
        ### CO2 industrial supply
        #self.i_industrial_supply_45q_df['new_vs_retrofit'] = 'both'
        self.i_industrial_supply_45q_df = self.parent.rest_industrial_co2_supply_45q.copy()
        self.i_industrial_supply_ntc_df = self.parent.rest_industrial_co2_supply_ntc.copy()


        ### Assign 45Q eligibility to supplies
        self.i_industrial_supply_45q_df['eligibility_45q'] = 1
        self.i_industrial_supply_ntc_df['eligibility_45q'] = 0


        ### CO2 investment cost
        self.i_nems_facility_cost_df = self.parent.rest_industrial_co2_cost_inv.copy()
        self.i_nems_facility_cost_df = self.i_nems_facility_cost_df.rename(columns ={'nems_cost_1987$':'nems_inv_cost_1987$'})
        self.i_nems_facility_cost_df = self.i_nems_facility_cost_df.merge(self.parent.rest_industrial_co2_cost_om[['census_division','year','facility_type','new_vs_retrofit','nems_cost_1987$']],
                                                                                                 how = 'left',
                                                                                                 on = ['census_division','year','facility_type','new_vs_retrofit']).drop_duplicates()
        self.i_nems_facility_cost_df = self.i_nems_facility_cost_df.rename(columns ={'nems_cost_1987$':'nems_om_cost_1987$'})


        ### EOR Facilities
        # Endogenous EOR Demand
        self.i_eor_demand_df = self.parent.rest_dem_eor.copy()
        self.i_eor_demand_df['play_id'] = 'p' + self.i_eor_demand_df['play_id'].astype(int).astype(str)

        # EOR net cost
        self.i_eor_cost_net_df = self.parent.rest_cst_eor.copy()
        self.i_eor_cost_net_df['play_id'] = 'p' + self.i_eor_cost_net_df['play_id'].astype(int).astype(str)

        pass


    def load_inputs_csv(self):
        '''Load CCATS inputs from CSV.

        * Read in input data from .csvs, and
        * Concat EOR demand data from the restart file to EOR centroid data from **hsm_eor_centroid.csv** input file, and filter out
          any EOR plays that have no demand.

        Returns
        -------
        self.i_storage_df : DataFrame
            DataFrame of storage formations - input data.

        self.i_co2_supply_facility_df : DataFrame
            DataFrame of CO\ :sub:`2` capture facility attributes from NETL - input data.

        self.i_pipeline_lookup_df : DataFrame
            DataFrame of CO\ :sub:`2` pipeline lookup  - input data.

        self.i_eor_demand_df : DataFrame
            DataFrame of CO\ :sub:`2` EOR site CO\ :sub:`2` demanded - input data.

        self.i_eor_cost_net_df : DataFrame
            DataFrame of CO\ :sub:`2` EOR net cost for CO\ :sub:`2` - input data.
        
        self.i_nat_co2_lookup_df : DataFrame
            DataFrame of natural CO\ :sub:`2` field attributes
        '''
        ### Read in DataFrames
        ### Storage
        self.i_storage_df = com.read_dataframe(self.preproc_input_path + self.setup_table.at['storage_formations_lookup','value'])

        ### CO2 supply facilities
        self.i_co2_supply_facility_df = com.read_dataframe(self.preproc_input_path + self.setup_table.at['co2_supply_facilities','value'])
        self.i_co2_supply_facility_df['facility_selection_ranking'] = np.nan


        ### Pipeline lookup
        self.i_pipeline_lookup_df = com.read_dataframe(self.preproc_input_path + self.setup_table.at['pipeline_lookup_multi','value'])

        # Linearize - only use pipeline segment 1
        if self.parent.linear_model_switch:
            self.i_pipeline_lookup_df = self.i_pipeline_lookup_df.query("pipe_segment == 's1'")

        ### ts multiplier lookup - read-in
        self.i_ts_multiplier_df = com.read_dataframe(self.preproc_input_path + self.setup_table.at['ts_multiplier','value']).set_index('year')


        ### EOR Centroid data
        self.i_eor_demand_centroid = com.read_dataframe(self.preproc_input_path + self.setup_table.at['hsm_eor_centroid', 'value'])
        # Concat EOR demand and demand centroids
        self.i_eor_demand_df = pd.concat([self.i_eor_demand_df, self.i_eor_demand_centroid], ignore_index=True)

        # filter restart file variable rows without associated play
        self.i_eor_demand_df = self.i_eor_demand_df[self.i_eor_demand_df['census_division'].notnull()]
        self.i_eor_cost_net_df = self.i_eor_cost_net_df[self.i_eor_cost_net_df['census_division'].notnull()]

        ### CO2 Sources
        # Read in natural CO2 data
        self.i_nat_co2_lookup_df = com.read_dataframe(self.preproc_input_path + self.setup_table.at['nat_co2_fields_lookup','value'])
        self.i_nat_co2_lookup_df = self.i_nat_co2_lookup_df.rename(columns = {'field_id':'node_id'})

        pass


    def load_inputs_pkl(self):
        '''Load CCATS inputs from Pickle.

            * CCATS Pickle variables are loaded from :ref:`CCATS Pickle` after model year 1 using the pickle library (https://docs.python.org/3/library/pickle.html),
            * This is done because the CCATS Python environment is not maintained between model iterations, pickling allows us to store working memory between iterations, and
            * This code can be deprecated once user objects are implemented in main.py.

        Returns
        -------
        self.i_storage_df : DataFrame
            DataFrame of storage formations - input data.

        self.i_co2_supply_facility_df : DataFrame
            DataFrame of CO\ :sub:`2` capture facility attributes from NETL - input data.

        self.i_pipeline_lookup_df : DataFrame
            DataFrame of CO\ :sub:`2` pipeline lookup  - input data.

        self.i_eor_demand_df : DataFrame
            DataFrame of CO\ :sub:`2` EOR site CO\ :sub:`2` demanded - input data.

        self.i_eor_cost_net_df : DataFrame
            DataFrame of CO\ :sub:`2` EOR net cost for CO\ :sub:`2` - input data.
        
        self.i_source_data : DataFrame
            DataFrame of endogenous sources of CO\ :sub:`2` data - input data.

        self.i_source_annual_capacity_existing : DataFrame
            DataFrame of endogenous sources of CO\ :sub:`2` existing annual capacity - input data.
            
        self.i_source_total_capacity_existing : DataFrame
            DataFrame of endogenous sources of CO\ :sub:`2` existing total capacity - input data.

        self.i_source_capacity_limits : DataFrame
            DataFrame of endogenous sources of CO\ :sub:`2` capacity limits - input data.

        self.i_nat_co2_lookup_df : DataFrame
            DataFrame of natural CO\ :sub:`2` field attributes
        
        self.pipes_existing_df : DataFrame
            DataFrame of existing CO\ :sub:`2` pipeline infrastructure in a given model year.

        self.storage_existing_df : DataFrame
            DataFrame of existing CO\ :sub:`2` storage infrastructure in a given model year.

        self.co2_facility_eligibility_df : DataFrame
            DataFrame of CO\ :sub:`2` facility 45Q eligibility.

        self.parent.new_built_pipes_df : DataFrame
            DataFrame of new pipelines built in previous model year b1.

        self.parent.new_aors_df : DataFrame
            DataFrame of new AORS, carried over from the previous model year.

        self.parent.store_prev_b0_df : DataFrame
            DataFrame of previous model year CO\ :sub:`2` stored in b0.

        self.parent.co2_supply_prev_b1_df : DataFrame
            DataFrame of previous model year CO\ :sub:`2` supplied in b1.
        
        self.parent.i_source_annual_capacity_existing : DataFrame
            DataFrame of endogenous sources of CO\ :sub:`2` built annual capacity.
        
        self.parent.i_source_total_capacity_existing : DataFrame
            DataFrame of endogenous sources of CO\ :sub:`2` built total capacity.

        self.parent.pkl.mod_nat_co2_prev_b0 : DataFrame
            DataFrame of previous model year natural CO2 flows
        '''
        # Load pickled input tables
        self.i_storage_df                   = self.parent.pkl.preproc_i_storage_df.copy()
        self.i_co2_supply_facility_df       = self.parent.pkl.preproc_i_co2_supply_facility_df.copy()
        self.i_pipeline_lookup_df           = self.parent.pkl.preproc_i_pipeline_lookup_df.copy()
        self.i_eor_demand_df                = self.parent.pkl.preproc_i_eor_demand_df.copy()
        self.i_eor_cost_net_df              = self.parent.pkl.preproc_i_eor_cost_net_df.copy()
        self.i_ts_multiplier_df             = self.parent.pkl.preproc_i_ts_multiplier_df.copy()
        self.i_source_data                  = self.parent.pkl.preproc_i_source_data.copy() 
        self.i_source_capacity_limits       = self.parent.pkl.preproc_i_source_capacity_limits.copy()
        self.i_nat_co2_lookup_df            = self.parent.pkl.preproc_i_nat_co2_lookup_df

        # Load pickled process tables
        self.pipes_existing_df                  = self.parent.pkl.preproc_pipes_existing_df.copy()
        self.storage_existing_df                = self.parent.pkl.preproc_storage_existing_df.copy()
        self.nat_co2_lookup_df                  = self.parent.pkl.preproc_nat_co2_lookup_df
        self.co2_facility_eligibility_df        = self.parent.pkl.preproc_co2_facility_eligibility_df.copy()
        self.source_annual_capacity_existing    = self.parent.pkl.preproc_source_annual_capacity_existing.copy()
        self.source_total_capacity_existing     = self.parent.pkl.preproc_source_total_capacity_existing.copy()

        # Load module-level pickle tables
        self.parent.new_built_pipes_df      = self.parent.pkl.mod_new_built_pipes_df.copy()
        self.parent.new_aors_df             = self.parent.pkl.mod_new_aors_df.copy()
        self.parent.store_prev_b0_df        = self.parent.pkl.mod_store_prev_b0_df.copy()
        self.parent.nat_co2_prev_b0         = self.parent.pkl.mod_nat_co2_prev_b0_df.copy()

        pass


    def harmonize_costs_inflation(self):
        '''Harmonize model input costs using NEMS inflation multipliers.

            * Inflation calculations are called using the :func:`common.calculate_inflation` function.

        Returns
        -------
        self.i_nems_facility_cost_df : DataFrame
            DataFrame of industrial facility CO\ :sub:`2` capture cost from NEMS - input data.

        self.i_pipeline_lookup_df : DataFrame
            DataFrame of CO\ :sub:`2` pipeline lookup - input data.

        self.i_co2_supply_facility_df : DataFrame
            DataFrame of CO\ :sub:`2` capture facility attributes from NETL - input data.

        self.i_storage_df : DataFrame
            DataFrame of storage formations - input data.

        '''
        ### Restart File Variables
        # NEMS capture costs (redundant, but here in case any models don't send costs in 1987$)
        self.i_nems_facility_cost_df['nems_inv_cost'] = self.i_nems_facility_cost_df['nems_inv_cost_1987$'] * com.calculate_inflation(self.parent.rest_mc_jpgdp.copy(), 1987)
        self.i_nems_facility_cost_df['nems_om_cost'] = self.i_nems_facility_cost_df['nems_om_cost_1987$'] * com.calculate_inflation(self.parent.rest_mc_jpgdp.copy(), 1987)


        ### Input Variables
        if (self.parent.integrated_switch == 0) | (int(self.parent.year_current) <= self.parent.year_start):

            # Transport costs - 2018$ to 1987$ year
            self.i_pipeline_lookup_df['pipe_capex_intercept'] = self.i_pipeline_lookup_df['pipe_capex_intercept'] * com.calculate_inflation(self.parent.rest_mc_jpgdp.copy(), 2018)
            self.i_pipeline_lookup_df['pipe_capex_slope'] = self.i_pipeline_lookup_df['pipe_capex_slope'] * com.calculate_inflation(self.parent.rest_mc_jpgdp.copy(), 2018)

            # CO2 capture facilities costs - 2008$ to 1987$ year
            self.i_co2_supply_facility_df['co2_capture_inv_cost'] = self.i_co2_supply_facility_df['co2_cost_inv_2018$'] * com.calculate_inflation(self.parent.rest_mc_jpgdp.copy(), 2018)
            self.i_co2_supply_facility_df['co2_capture_om_cost']  = self.i_co2_supply_facility_df['co2_cost_om_2018$'] * com.calculate_inflation(self.parent.rest_mc_jpgdp.copy(), 2018)

            # Storage costs  - 2008$ to 1987$ year
            inflate_from_2008_dollars = com.calculate_inflation(self.parent.rest_mc_jpgdp.copy(), 2008)
            self.i_storage_df['capex_site_dev'] = self.i_storage_df['capex_site_dev_2008$'] * inflate_from_2008_dollars
            self.i_storage_df['fixom_site_dev'] = self.i_storage_df['fixedom_site_dev_2008$_per_yr'] * inflate_from_2008_dollars
            self.i_storage_df['varom_site_dev'] = self.i_storage_df['varom_site_dev_2008$_per_ton-yr'] * inflate_from_2008_dollars

            self.i_storage_df['capex_construction'] = self.i_storage_df['capex_construction_2008$'] * inflate_from_2008_dollars
            self.i_storage_df['fixom_construction'] = self.i_storage_df['fixedom_construction_2008$_per_yr'] * inflate_from_2008_dollars
            self.i_storage_df['varom_construction'] = self.i_storage_df['varom_construction_2008$_per_ton-yr'] * inflate_from_2008_dollars

            self.i_storage_df['capex_injection'] = self.i_storage_df['capex_injection_2008$'] * inflate_from_2008_dollars
            self.i_storage_df['fixom_injection'] = self.i_storage_df['fixedom_injection_2008$_per_yr'] * inflate_from_2008_dollars
            self.i_storage_df['varom_injection'] = self.i_storage_df['varom_injection_2008$_per_ton-yr'] * inflate_from_2008_dollars

            self.i_storage_df['capex_pisc'] = self.i_storage_df['capex_pisc_2008$'] * inflate_from_2008_dollars
            self.i_storage_df['fixom_pisc'] = self.i_storage_df['fixedom_pisc_2008$_per_yr'] * inflate_from_2008_dollars
            self.i_storage_df['varom_pisc'] = self.i_storage_df['varom_pisc_2008$_per_ton-yr'] * inflate_from_2008_dollars

            self.i_storage_df['capex_closure'] = self.i_storage_df['capex_closure_2008$'] * inflate_from_2008_dollars
            self.i_storage_df['fixom_closure'] = self.i_storage_df['fixedom_closure_2008$_per_yr'] * inflate_from_2008_dollars
            self.i_storage_df['varom_closure'] = self.i_storage_df['varom_closure_2008$_per_ton-yr'] * inflate_from_2008_dollars
        
            # Source costs - specified dollar year to 1987$
            for i in self.i_source_data.index:
                dollar_year_starting = self.i_source_data.loc[i,'dollar_year']
                self.i_source_data.loc[i,'CAPEX_USD_t'] = self.i_source_data.loc[i,'CAPEX_USD_t'] * com.calculate_inflation(self.parent.rest_mc_jpgdp.copy(), dollar_year_starting)
                self.i_source_data.loc[i,'VOM_USD_t']   = self.i_source_data.loc[i,'VOM_USD_t']   * com.calculate_inflation(self.parent.rest_mc_jpgdp.copy(), dollar_year_starting)

        else:
            pass

        pass


    def filter_pnw(self):
        '''Assess Pacific Northwest (PNC) EMM region's viability for CCS activities.

        * PNW has lots of electricity generation, but limited CCS options, so we pull FANs (Faciliy Aggregated Nodes) out of the representation if no builds in the previous cycle.

        Parameters
        ----------
        None

        Returns
        -------
        self.i_co2_supply_facility_df : DataFrame
            DataFrame of CO\ :sub:`2` capture facility attributes from NETL - input data.

        self.i_pipeline_lookup_df : DataFrame
            DataFrame of CO\ :sub:`2` pipeline lookup - input data.

        '''
        # Sum all ccs builds across type
        pnw_check = self.parent.rest_ucapisn.copy()
        pnw_check['value'] = pnw_check['value'] + self.parent.rest_ucapsqn['value']
        pnw_check['value'] = pnw_check['value'] + self.parent.rest_ucappqn['value']
        pnw_check['value'] = pnw_check['value'] + self.parent.rest_ucapasn['value']
        pnw_check['value'] = pnw_check['value'] + self.parent.rest_ucapa2n['value']

        # Get builds for region 23 (PNW) only
        pnw_check = pnw_check.loc[pnw_check.index.get_level_values(0) == 23]


        ### If PNW builds == 0, scrub clusters from facility and pipeline builds
        if pnw_check['value'].sum() == 0:
            ### Remove FANs from i_co2_supply_facility_df
            # Get facility ids for pipeline lookup
            type_mask       = self.i_co2_supply_facility_df['facility_type'].isin(['pp_coal','pp_natgas','beccs'])
            cluster_mask    = self.i_co2_supply_facility_df['facility_new'] == 'Y'
            state_mask      = self.i_co2_supply_facility_df['state'].isin(['OR','WA'])
            fac_mask        = type_mask & cluster_mask & state_mask

            fac_ids = self.i_co2_supply_facility_df[fac_mask].copy()

            # Remove FANS from CO2 Supply Facility Lookup
            self.i_co2_supply_facility_df = self.i_co2_supply_facility_df[~fac_mask].copy()


            ### Update CO2 ratio from remaining CD 9 FANs
            type_mask       = self.i_co2_supply_facility_df['facility_type'].isin(['pp_coal','pp_natgas','beccs'])
            cluster_mask    = self.i_co2_supply_facility_df['facility_new'] == 'Y'
            cr_mask         = self.i_co2_supply_facility_df['supply_census_division'] == 9
            fac_mask        = type_mask & cluster_mask & cr_mask

            temp_facs = self.i_co2_supply_facility_df[fac_mask].copy()

            # Coal
            temp_coal = temp_facs.loc[temp_facs['facility_type'] == 'pp_coal'].copy()
            temp_coal['co2_ratio_sum'] = temp_coal['co2_ratio'].sum()
            temp_coal['co2_ratio'] = temp_coal['co2_ratio'] / temp_coal['co2_ratio_sum']
            self.i_co2_supply_facility_df.update(temp_coal)

            # Natgas
            temp_ng = temp_facs.loc[temp_facs['facility_type'] == 'pp_natgas'].copy()
            temp_ng['co2_ratio_sum'] = temp_ng['co2_ratio'].sum()
            temp_ng['co2_ratio'] = temp_ng['co2_ratio'] / temp_ng['co2_ratio_sum']
            self.i_co2_supply_facility_df.update(temp_ng)

            # BECCS
            temp_beccs = temp_facs.loc[temp_facs['facility_type'] == 'pp_coal'].copy()
            temp_beccs['co2_ratio_sum'] = temp_beccs['co2_ratio'].sum()
            temp_beccs['co2_ratio'] = temp_beccs['co2_ratio'] / temp_beccs['co2_ratio_sum']
            self.i_co2_supply_facility_df.update(temp_beccs)


            ### Filter Pipelines from i_pipeline_lookup_df
            self.i_pipeline_lookup_df = self.i_pipeline_lookup_df.loc[~self.i_pipeline_lookup_df['node_i_id'].isin(fac_ids['facility_id'])].copy()

        pass


    def assign_tax_credits(self):
        '''Assign 45Q tax credits values to storage and demand input DataFrames.

        Returns
        -------
        self.i_storage_df : DataFrame
            DataFrame of storage formations - input data.

        self.i_eor_demand_df : DataFrame
            DataFrame of EOR demand - input data.

        '''
        # Assign tax credit values
        self.i_storage_df['tax_credit_value']           = self.parent.rest_ccs_saline_45q.at[int(self.parent.year_current),'value']
        self.i_eor_demand_df['tax_credit_value']        = self.parent.rest_ccs_eor_45q.at[int(self.parent.year_current),'value']

        self.i_storage_df['leg_tax_credit_value']       = self.parent.leg_ccs_saline_45q.at[int(self.parent.year_current),'value']
        self.i_eor_demand_df['leg_tax_credit_value']    = self.parent.leg_ccs_eor_45q.at[int(self.parent.year_current),'value']

        self.i_storage_df['tax_credit_value_dac']       = self.parent.dac_saline_45q * com.calculate_inflation(self.parent.rest_mc_jpgdp.copy(), self.parent.year_new_45q)
        self.i_eor_demand_df['tax_credit_value_dac']    = self.parent.dac_eor_45q    * com.calculate_inflation(self.parent.rest_mc_jpgdp.copy(), self.parent.year_new_45q)

        pass


    #####################################
    ###Preprocessor Run Processes
    #####################################

    def run(self):
        """Run Preprocessor Submodule for CCATS.

        Parameters
        ----------
        None

        Returns
        -------
        None

        """
        super().run()

        # Assign tax credit values
        self.logger.info('Assign tax credit values to CO2 sequestration sources')
        self.assign_tax_credits()

        ### Run Preprocessor processes
        # Declare model-year dfs
        self.logger.info('Instantiate Model Year DataFrames')
        self.instantiate_model_year_dfs()
        
        # Account for facility operations
        self.logger.info('Account for number of years facilities have been operating')
        self.account_for_facility_year_operations()

        # Setup CO2 Supply Blocks
        self.logger.info('Setup CO2 Supply Blocks')
        self.setup_co2_supply_blocks()

        # Set CO2 EOR Blocks
        self.logger.info('Setup CO2 EOR Blocks')
        self.setup_co2_eor_blocks()

        # Declare existing pipeline infrastructure
        self.logger.info('Declare existing pipeline infrastructure')
        self.declare_existing_pipe_infrastructure()

        # Reduce NETL capture costs (tech learning)
        self.logger.info('Reduce NETL capture costs (tech learning)')
        self.apply_tech_rate_capture_costs()

        # Assign CO2 capture costs for net new facilities based on restart file data
        self.logger.info('Assign CO2 capture costs for net new facilities based on restart file data')
        self.assign_nems_capture_costs()

        # Get roster of available CO2 capture facilities
        self.logger.info('Get roster of CO2 capture facilities')
        self.get_co2_facility_rosters()

        # Prioritize CO2 capture facilities based on transport connections
        self.logger.info('Prioritize CO2 capture facilities based on transport connections')
        self.add_transport_costs_to_netl_capture_costs()

        # Assess model year CO2 supplies
        self.logger.info('Determine model year point source supplies')
        self.determine_point_source_supplies()

        # Determine 45Q eligibility of CO2 facilities
        self.logger.info('Determine 45Q Eligibility')
        self.determine_facility_eligibility_45q()

        # Zero out investment costs for retrofit facilities
        self.logger.info('Zero out investment costs for facilities which have retrofit')
        self.zero_inv_costs_for_retrofit_facilities()

        # Filter available CO2 pipelines
        self.logger.info('Filter CO2 Pipelines')
        self.filter_available_pipelines()

        # Assign electricity costs for CO2 pipelines
        self.logger.info('Assign electricity costs')
        self.assign_electricity_costs()

        # Update storage infrastructure
        self.logger.info('Update storage infrastructure')
        self.update_storage_infrastructure()

        # Update natural CO2 reserves
        self.logger.info('Update natural CO2 reserves')
        self.update_natural_co2_reserves()

        # Declare optimization dfs
        self.logger.info('Declare, filter and format optimization DataFrames')
        self.declare_supply_opt()
        self.prepare_storage_opt()
        self.prepare_eor_opt()

        # Endogenous sources of CO2
        if self.parent.endogenous_source_switch:
            self.logger.info('Prepare endogenous sources of CO2 for optimization')
            self.prepare_endogenous_sources()
            
        if self.parent.natural_co2_switch:
            self.logger.info('Prepare natural sources of CO2 for optimization')
            self.prepare_natural_sources()

        # Re-index, set dataframes up for optimization
        self.logger.info('Set up for optimization')
        self.setup_optimization()
        self.instantiate_pyomo_series()

        # Write Pickle
        if ((self.parent.integrated_switch == 1) & (self.parent.param_fcrl == 1)) | ((self.parent.integrated_switch == 1) & (self.parent.param_ncrl == 1)):
            self.logger.info('Write Local Variables to PKL')
            self.write_pkl()

        # Prepare summary stats and visualizations
        self.logger.info('Print out summary input data')
        if self.parent.debug_switch == True:
            self.summarize_inputs()
        if self.parent.visualize_preproc_switch == True:
            self.visualize_inputs()

        else:
            pass

        pass


    def instantiate_model_year_dfs(self):
        '''Instantiate model year DataFrames (i.e. model year pipeline lookup, available saline formations, etc.).

        * Pipeline CO\ :sub:`2` supplies and pipeline network change every model year, so need to refresh DataFrames every model year.

        Parameters
        ----------
        None

        Returns
        -------
        self.pipeline_lookup_df : DataFrame
            DataFrame of model year CO\ :sub:`2` pipeline lookup.
        
        self.storage_new_df : DataFrame
            DataFrame of model year potential new storage formations.
            
        self.eor_demand_df : DataFrame
            DataFrame of model year CO\ :sub:`2` EOR demand by play.

        self.eor_cost_net_df : DataFrame
           DataFrame of model CO\ :sub:`2` EOR net cost by play.
          
        self.co2_supply_facility_df : DataFrame
            DataFrame of available facilities for carbon capture.

        '''
        # Model year CO2 pipeline lookup
        self.pipeline_lookup_df     = self.i_pipeline_lookup_df.copy()

        # Model year CO2 storage
        self.storage_new_df             = self.i_storage_df.copy()

        # CO2 EOR demand
        self.eor_demand_df          = self.i_eor_demand_df.copy()
        self.eor_cost_net_df        = self.i_eor_cost_net_df.copy()

        # Model year CO2 supply facilities
        self.co2_supply_facility_df = self.i_co2_supply_facility_df.copy()

        pass

    #######################################
    ###Setup Model Year Preprocessor
    #######################################

    def account_for_facility_year_operations(self):
        '''Account for the number of years a facility has been operational.

        * Update years_operating and 45Q_eligibility before facilities are selected for the optimization.

        Returns
        -------

        self.co2_supply_facility_df : DataFrame
            DataFrame of CO\ :sub:`2` facility data and costs - input data.

        '''
        ### Include operation projected by CCATS for non-cluster facilities (if after first model year)
        if int(self.parent.year_current) > self.parent.year_start:

            # Make a copy
            temp_df = self.co2_facility_eligibility_df.copy()
            
            # group by facility_id
            temp_df = temp_df.groupby('facility_id').max()
            temp_df = temp_df.reset_index()
              
            # merge
            self.co2_supply_facility_df = self.co2_supply_facility_df.merge(
                                               temp_df[['facility_id','years_operating']],
                                               how = 'left',
                                               left_on = 'facility_id',
                                               right_on = 'facility_id',
                                               suffixes=(None, '_ccats'))
            
            # fill N/A
            self.co2_supply_facility_df['years_operating_ccats'] = self.co2_supply_facility_df['years_operating_ccats'].fillna(0)

            # replace with years_operating_ccats
            indices = (self.co2_supply_facility_df.years_operating_ccats != 0)
            self.co2_supply_facility_df.loc[indices, 'years_operating'] = self.co2_supply_facility_df.loc[indices, 'years_operating_ccats']

            # update 45Q eligibility
            indices = (self.co2_supply_facility_df.years_operating_ccats > self.parent.ccats_fin.duration_45q)
            self.co2_supply_facility_df.loc[indices, 'eligibility_45q'] = 0.0

            # remove any duplicate facilities based on facility_id and elgibility_45q
            temp_supply = self.co2_supply_facility_df.copy()
            temp_supply = temp_supply.sort_values(by='cluster_type', ascending=False) # prioritize 'ntc' because the duplicates come from 45Q -> NTC, and therefore we want the NTC details
            temp_supply = temp_supply.groupby(['facility_id', 'eligibility_45q'], sort=False).agg('first').reset_index()
            self.co2_supply_facility_df = temp_supply

        pass


    def setup_co2_supply_blocks(self):
        '''Instantiate block-level model DataFrames for CO\ :sub:`2` supply.

        * Instantiate individual block-level DataFrames for CO2 supply by facility type, and
        * Instantiate a single DataFrame for model year capture costs, with

            * Block 0 CO\ :sub:`2` supply == model year CO\ :sub:`2` supply,
            * Block 1 CO\ :sub:`2` supply == model year + 1 CO\ :sub:`2` supply, and
            * Block 2 CO\ :sub:`2` supply == mean(model year + 2 max(final AEO model year, model year + 7)) CO\ :sub:`2` demand.

        Returns
        -------
        self.year_industrial_supply_b0_df : DataFrame
            DataFrame of model year CO\ :sub:`2` supply for model time period 0.

        self.year_industrial_supply_b1_df : DataFrame
            DataFrame of model year CO\ :sub:`2` supply for model time period 1.

        self.year_industrial_supply_b2_df : DataFrame
            DataFrame of model year CO\ :sub:`2` supply for model time period 2.

        self.year_nems_facility_cost_df : DataFrame
            DataFrame of model year CO\ :sub:`2` capture costs from NEMS modules.

        '''
        ### Industrial supply 45Q and NTC
        # combine 45Q and NTC data from restart file
        i_industrial_supply_df = pd.concat([self.i_industrial_supply_45q_df, self.i_industrial_supply_ntc_df]).reset_index()

        # Time Period 0
        self.year_industrial_supply_b0_df = i_industrial_supply_df.copy().loc[i_industrial_supply_df['year'] == int(self.year_current)].copy()
        self.year_industrial_supply_b0_df = self.year_industrial_supply_b0_df.loc[self.year_industrial_supply_b0_df['capture_volume_tonnes'] > 0].copy()

        # Time Period 1
        if int(self.year_current) < self.year_final:
            self.year_industrial_supply_b1_df = i_industrial_supply_df.copy().loc[i_industrial_supply_df['year'] == (int(self.year_current) + 1)].copy()
            self.year_industrial_supply_b1_df = self.year_industrial_supply_b1_df.loc[self.year_industrial_supply_b1_df['capture_volume_tonnes'] > 0].copy()
        else:
            self.year_industrial_supply_b1_df = i_industrial_supply_df.copy().loc[i_industrial_supply_df['year'] == (int(self.year_current))].copy()
            self.year_industrial_supply_b1_df = self.year_industrial_supply_b1_df.loc[self.year_industrial_supply_b1_df['capture_volume_tonnes'] > 0].copy()

        # Time Period 2
        model_time_remaining = self.year_final + 1 - int(self.year_current)
        if model_time_remaining >= 5:
            assessed_years = 5
        else:
            assessed_years = model_time_remaining

        self.year_industrial_supply_b2_df = i_industrial_supply_df.copy().loc[i_industrial_supply_df['year'].isin(list(range(int(self.year_current), int(self.year_current) + assessed_years)))].copy()
        self.year_industrial_supply_b2_df = self.year_industrial_supply_b2_df.loc[self.year_industrial_supply_b2_df['capture_volume_tonnes'] > 0].copy()
        self.year_industrial_supply_b2_df = self.year_industrial_supply_b2_df.groupby(['census_division','facility_type','new_vs_retrofit','eligibility_45q']).mean().reset_index()
        self.year_industrial_supply_b2_df['year'] = int(self.year_current) + 2


        ### NEMS Industrial supply cost
        # Time Period 0
        nems_facility_cost_b0_df = self.i_nems_facility_cost_df.copy().loc[self.i_nems_facility_cost_df['year'] == int(self.year_current)].copy()
        nems_facility_cost_b0_df = nems_facility_cost_b0_df.rename(columns = {'nems_inv_cost':'b0_nems_inv_cost', 'nems_om_cost':'b0_nems_om_cost'})

        # Time Period 1
        if int(self.year_current) < self.year_final:
            nems_facility_cost_b1_df = self.i_nems_facility_cost_df.copy().loc[self.i_nems_facility_cost_df['year'] == (int(self.year_current) + 1)].copy()
        else:
            nems_facility_cost_b1_df = self.i_nems_facility_cost_df.copy().loc[self.i_nems_facility_cost_df['year'] == (int(self.year_current))].copy()

        nems_facility_cost_b1_df = nems_facility_cost_b1_df.rename(columns = {'nems_inv_cost':'b1_nems_inv_cost', 'nems_om_cost':'b1_nems_om_cost'})

        # Time Period 2
        model_time_remaining = self.year_final + 1 - int(self.year_current)
        if model_time_remaining >= 5:
            assessed_years = 5
        else:
            assessed_years = model_time_remaining

        nems_facility_cost_b2_df = self.i_nems_facility_cost_df.copy().loc[self.i_nems_facility_cost_df['year'].isin(list(range(int(self.year_current), int(self.year_current) + assessed_years)))].copy()
        nems_facility_cost_b2_df = nems_facility_cost_b2_df.groupby(['census_division','facility_type','new_vs_retrofit']).mean().reset_index()
        nems_facility_cost_b2_df['year'] = int(self.year_current) + 2
        nems_facility_cost_b2_df = nems_facility_cost_b2_df.rename(columns = {'nems_inv_cost':'b2_nems_inv_cost', 'nems_om_cost':'b2_nems_om_cost'})

        # Merge costs
        self.year_nems_facility_cost_df = nems_facility_cost_b0_df[['census_division','facility_type','new_vs_retrofit','b0_nems_inv_cost','b0_nems_om_cost']].copy()

        self.year_nems_facility_cost_df = self.year_nems_facility_cost_df.merge(nems_facility_cost_b1_df[['census_division','facility_type','new_vs_retrofit','b1_nems_inv_cost','b1_nems_om_cost']],
                                                                                how = 'left',
                                                                                on = ['census_division','facility_type','new_vs_retrofit'])
        self.year_nems_facility_cost_df = self.year_nems_facility_cost_df.merge(nems_facility_cost_b2_df[['census_division','facility_type','new_vs_retrofit','b2_nems_inv_cost','b2_nems_om_cost']],
                                                                                how = 'left',
                                                                                on = ['census_division','facility_type','new_vs_retrofit'])

        pass


    def setup_co2_eor_blocks(self):
        '''Setup model year DataFrames for CO\ :sub:`2` EOR demand.

        * Block 0 CO\ :sub:`2` demand == model year CO\ :sub:`2` demand,
        * Block 1 CO\ :sub:`2` demand == model year + 1 CO\ :sub:`2` demand, and
        * Block 2 CO\ :sub:`2` demand == mean(model year + 2 max(final AEO model year, model year + 7) CO\ :sub:`2` demand.

        Returns
        -------
        self.eor_demand_df : DataFrame
            DataFrame of model year CO\ :sub:`2` EOR demand by play.

        self.eor_cost_net_df : DataFrame
           DataFrame of model CO\ :sub:`2` EOR net cost by play.
        '''
        # Get model time remaining
        model_time_remaining = self.year_final + 1 - int(self.year_current)
        if model_time_remaining >= 5:
            assessed_years = 5
        else:
            assessed_years = model_time_remaining

        ### Get model year EOR demand
        # Remove CO2 EOR plays that have never produced
        temp_eor_demand_df = self.eor_demand_df.copy()
        temp_eor_demand_df['total_volume'] = temp_eor_demand_df[(list(range(self.parent.year_start, int(self.year_current) + assessed_years)))].sum(axis = 1)
        temp_eor_demand_df = self.eor_demand_df.loc[temp_eor_demand_df['total_volume'] > 0]

        # Get time periods 0 & 1 EOR demands
        if int(self.year_current) < self.parent.year_final:
            self.eor_demand_df = temp_eor_demand_df[['play_id', 'census_division', 'tax_credit_value', 'tax_credit_value_dac', int(self.year_current), (int(self.year_current) + 1)]].copy()
        else:
            self.eor_demand_df = temp_eor_demand_df[['play_id', 'census_division', 'tax_credit_value', 'tax_credit_value_dac', int(self.year_current)]].copy()
            self.eor_demand_df[(int(self.year_current) + 1)] = temp_eor_demand_df[int(self.year_current)]

        # Get time period 2 EOR demand
        self.eor_demand_df[(int(self.year_current) + 2)] = temp_eor_demand_df[list(range(int(self.year_current), int(self.year_current) + assessed_years))].copy().mean(axis = 1).copy()

        # Update column names
        self.eor_demand_df = self.eor_demand_df.rename(columns={'play_id': 'node_id',
                                                                int(self.year_current): 'b0_co2_volume',
                                                                (int(self.year_current) + 1): 'b1_co2_volume',
                                                                (int(self.year_current) + 2): 'b2_co2_volume'})


        ### Get model year EOR net cost
        temp_eor_cost_net_df = self.eor_cost_net_df.copy()
        
        # Get time periods 0 & 1 EOR demands
        if int(self.year_current) < self.parent.year_final:
            self.eor_cost_net_df = temp_eor_cost_net_df[['play_id', 'census_division', int(self.year_current), (int(self.year_current) + 1)]].copy()
        else:
            self.eor_cost_net_df = temp_eor_cost_net_df[['play_id', 'census_division',  int(self.year_current)]].copy()
            self.eor_cost_net_df[(int(self.year_current) + 1)] = temp_eor_cost_net_df[int(self.year_current)]

        # Get time period 2 EOR demand
        self.eor_cost_net_df[(int(self.year_current) + 2)] = temp_eor_cost_net_df[list(range(int(self.year_current), int(self.year_current) + assessed_years))].copy().mean(axis = 1).copy()

        # Update column names
        self.eor_cost_net_df = self.eor_cost_net_df.rename(columns={'play_id': 'node_id',
                                                                int(self.year_current): 'b0_net_co2_cost',
                                                                (int(self.year_current) + 1): 'b1_net_co2_cost',
                                                                (int(self.year_current) + 2): 'b2_net_co2_cost'})
        self.eor_cost_net_df = self.eor_cost_net_df.loc[self.eor_cost_net_df['node_id'].isin(self.eor_demand_df['node_id'])]

        pass


    def declare_existing_pipe_infrastructure(self):
        '''Declare and update existing pipeline network.

            * If first model year, instantiate existing pipeline infrastructure based on generalized DOT network, and
            * After first model year, update existing transport infrastructure with previous model year's results (pipelines bult in b1).

        Parameters
        ----------
        None

        Returns
        -------
        self.pipes_existing_df : DataFrame
            DataFrame of model year existing CO\ :sub:`2` pipeline infrastructure.

        '''
        # Instantiate existing infrastructure df
        if int(self.year_current) <= self.parent.year_start:
            self.pipes_existing_df = self.i_pipeline_lookup_df[
                ['operational_status', 'existing_throughput_tonnes', 'max_throughput_tonnes',
                 'node_i_census_division',
                 'node_i_id', 'node_j_census_division', 'node_j_id', 'average_electricity',
                 'pipe_capex_intercept', 'pipe_capex_slope', 'pipe_segment', 'route_id', 'pipeline_type']].copy()
            self.pipes_existing_df = self.pipes_existing_df.rename(
                columns={'existing_throughput_tonnes': 'thruput_tonnes'})
            self.pipes_existing_df.loc[self.pipes_existing_df['operational_status'] == 'N', 'thruput_tonnes'] = 0

            # only use segment 1 for existing pipelines
            self.pipes_existing_df = self.pipes_existing_df.loc[self.pipes_existing_df['pipe_segment'] == 's1']

        # If after first model year, update existing transport infrastructure with last year's b1 built pipelines
        elif int(self.year_current) > self.parent.year_start:
            self.pipes_existing_df = self.pipes_existing_df.merge(
                self.parent.new_built_pipes_df[['node_i_id', 'node_j_id', 'thruput_tonnes']],
                how='left',
                on=['node_i_id', 'node_j_id'],
                suffixes=('', '_new'))
            self.pipes_existing_df['thruput_tonnes_new'] = self.pipes_existing_df['thruput_tonnes_new'].fillna(0.0)
            self.pipes_existing_df['thruput_tonnes'] = self.pipes_existing_df['thruput_tonnes'] + \
                                                       self.pipes_existing_df['thruput_tonnes_new']
            self.pipes_existing_df = self.pipes_existing_df.drop(['thruput_tonnes_new'], axis=1)
            self.parent.pytest.pipes_existing_df = self.pipes_existing_df

        pass

    #######################################
    ###CO2 Capture Facilities Preprocessing
    #######################################

    
    def apply_tech_rate_capture_costs(self):
        '''Apply tech learning rate to NETL capture costs.

        Parameters
        ----------
        None

        Returns
        -------
        self.co2_supply_facility_df : DataFrame
            DataFrame of available CO\ :sub:`2` facilities for carbon capture.
        
        '''
        # Calculate reduction multipliers by facility type
        cost_reduce_fr = {}
        cost_reduce_fr['ammonia']                    = (1.0 - self.tech_learn_ammonia)       **(self.parent.year_current - self.parent.year_start)
        cost_reduce_fr['ethanol']                    = (1.0 - self.tech_learn_ethanol)       **(self.parent.year_current - self.parent.year_start)
        cost_reduce_fr['cement']                     = (1.0 - self.tech_learn_cement)        **(self.parent.year_current - self.parent.year_start)
        cost_reduce_fr['ng_processing']              = (1.0 - self.tech_learn_ng_processing) **(self.parent.year_current - self.parent.year_start)
        cost_reduce_fr['pp_coal']                    = (1.0 - self.tech_learn_power)         **(self.parent.year_current - self.parent.year_start)
        cost_reduce_fr['pp_natgas']                  = (1.0 - self.tech_learn_power)         **(self.parent.year_current - self.parent.year_start)
        cost_reduce_fr['other_operational_facility'] = (1.0 - self.tech_learn_other)         **(self.parent.year_current - self.parent.year_start)

        # Apply reduction multiplier (tech learning) to capture costs by facility type
        for facility_type in self.co2_supply_facility_df.facility_type.unique():
            
            # Get the appropriate multiplier, if not in dictionary use the default (other)
            if facility_type in cost_reduce_fr.keys():
                cost_reduce_multiplier = cost_reduce_fr[facility_type] 
            else:
                cost_reduce_multiplier = cost_reduce_fr['other_operational_facility'] 

            # Apply multiplier to all facilities of this type
            ind = self.co2_supply_facility_df.facility_type == facility_type
            self.co2_supply_facility_df.loc[ind, 'co2_capture_inv_cost'] = self.co2_supply_facility_df.loc[ind, 'co2_capture_inv_cost'] * cost_reduce_multiplier
            self.co2_supply_facility_df.loc[ind, 'co2_capture_om_cost']  = self.co2_supply_facility_df.loc[ind, 'co2_capture_om_cost']  * cost_reduce_multiplier

        ### Output reduction multipliers for debugging
        if self.parent.debug_switch == True:
            # Store technologic learning data as a Series
            tech_learning = pd.Series()
            tech_learning['year_current']                  = int(self.parent.year_current)
            tech_learning['cost_reduce_fr_ammonia']        = cost_reduce_fr['ammonia']   
            tech_learning['cost_reduce_fr_ethanol']        = cost_reduce_fr['ethanol']  
            tech_learning['cost_reduce_fr_cement']         = cost_reduce_fr['cement'] 
            tech_learning['cost_reduce_fr_ng_processing']  = cost_reduce_fr['ng_processing'] 
            tech_learning['cost_reduce_fr_pp_coal']        = cost_reduce_fr['pp_coal']
            tech_learning['cost_reduce_fr_pp_natgas']      = cost_reduce_fr['pp_natgas']
            tech_learning['cost_reduce_fr_other']          = cost_reduce_fr['other_operational_facility']

            # Transform to a dataframe and save to a debug CSV
            tech_learning_df = tech_learning.to_frame().transpose()
            tech_learning_filename = self.output_path + 'tech_learning//' + 'tech_learning_capture.csv'
            if self.year_current == self.parent.year_start:
                tech_learning_df.to_csv(tech_learning_filename)
            else:
                tech_learning_df.to_csv(tech_learning_filename, mode='a', header=False)

        pass


    def assign_nems_capture_costs(self):
        '''Assign CO\ :sub:`2` capture cost ($/tonne) for net-new facilities based on restart file variables.
        
        Parameters
        ----------
        None

        Returns
        -------
        self.co2_supply_facility_df : DataFrame
            DataFrame of available CO\ :sub:`2` facilities for carbon capture.
        
        '''
        # Only merge on greenfield (new) or generalized (both) costs, not EMM retrofit costs
        nem_fac_costs = self.year_nems_facility_cost_df.loc[self.year_nems_facility_cost_df['new_vs_retrofit'].isin(['new','both'])].copy()

        # Merge to NEMS restart file capture costs
        self.co2_supply_facility_df = self.co2_supply_facility_df.reset_index().merge(nem_fac_costs,
                                                          how = 'left',
                                                          left_on = ['facility_type','supply_census_division'],
                                                          right_on = ['facility_type','census_division']).set_index('index')
        self.co2_supply_facility_df['census_division'] = self.co2_supply_facility_df['supply_census_division'].astype('int64')

        # Filter for cluster facilities
        temp_cap_cost_df = self.co2_supply_facility_df.loc[self.co2_supply_facility_df['facility_new'] == 'Y'].copy()
        temp_cap_cost_df['co2_capture_inv_cost'] = temp_cap_cost_df['b0_nems_inv_cost']
        temp_om_cost_df = self.co2_supply_facility_df.loc[self.co2_supply_facility_df['facility_new'] == 'Y'].copy()
        temp_om_cost_df['co2_capture_om_cost'] = temp_om_cost_df['b0_nems_om_cost']

        # Update master facility df
        self.co2_supply_facility_df.update(temp_cap_cost_df)
        self.co2_supply_facility_df.update(temp_om_cost_df)

        pass


    def get_co2_facility_rosters(self):
        '''Produce model year rosters of available CO\ :sub:`2` capture facilities.
        
        Parameters
        ----------
        None

        Returns
        -------
        self.ccs_facility_year_df : DataFrame
            DataFrame of existing carbon capture facilities eligible for carbon capture retrofit.

        self.cluster_facility_df : DataFrame
            DataFrame of representative clustered carbon capture facilities.
        
        '''
        ### Create NETL database and cluster facility rosters
        self.ccs_facility_year_df = self.co2_supply_facility_df.loc[self.co2_supply_facility_df['facility_new'] == 'N'].copy()
        self.cluster_facility_df = self.co2_supply_facility_df.loc[self.co2_supply_facility_df['facility_new'] == 'Y'].copy()

        # If small sample switch == True, make df empty so only clusters are available
        if self.small_sample_switch:
            self.ccs_facility_year_df = self.ccs_facility_year_df.loc[self.ccs_facility_year_df['operational_status'] == 'Y'].copy()

        pass


    def add_transport_costs_to_netl_capture_costs(self):
        '''Prioritize CO\ :sub:`2` capture facilities based on transport connection costs.

            * Update NETL costs to represent capture cost by facility + transport to the closest site, and
            * If the closest site isn't source-to-sink, double cost.

        Parameters
        ----------
        None

        Returns
        -------
        self.ccs_facility_year_df : DataFrame
            DataFrame of existing carbon capture facilities eligible for carbon capture retrofit.
        
        '''
        # Calculate the cost of the shortest transportation path
        self.ccs_facility_year_df['shortest_transport_cost'] = 0.0

        # Find the shortest pipeline option to storage/demand/hsm_cent for each existing facility
        short_s1_pipeline_options = self.pipeline_lookup_df.copy()
        short_s1_pipeline_options['source_direct_connect'] = short_s1_pipeline_options['pipeline_type'].isin(['source_to_storage', 'source_to_demand', 'source_to_hsm_cent'])
        short_s1_pipeline_options = short_s1_pipeline_options.query("pipe_segment == 's1'").sort_values(['source_direct_connect', 'pipeline_miles'], ascending=[False, True]).groupby(by='node_i_id').nth(0)
        short_s1_pipeline_options = short_s1_pipeline_options.rename(columns={'pipe_capex_slope': 'short_pipe_capex_slope', 'pipe_capex_intercept': 'short_pipe_capex_intercept'})
        short_s1_pipeline_options = short_s1_pipeline_options.loc[:, ['node_i_id', 'short_pipe_capex_slope', 'short_pipe_capex_intercept', 'source_direct_connect']]

        # Merge with self.ccs_facility_year_df
        self.ccs_facility_year_df = pd.merge(self.ccs_facility_year_df, short_s1_pipeline_options, how='left',
                                             left_on='facility_id', right_on='node_i_id')

        # Calculate the cost of shortest transport (dollars/t for investment, no financing assumed)
        self.ccs_facility_year_df['shortest_transport_cost'] = self.ccs_facility_year_df['short_pipe_capex_intercept'] / (self.ccs_facility_year_df['co2_volume']) + self.ccs_facility_year_df['short_pipe_capex_slope']

        # Apply cost reduction to transport due to technologic learning
        self.ccs_facility_year_df['shortest_transport_cost'] = self.ccs_facility_year_df['shortest_transport_cost'] * (1.0 - self.tech_learn_transport)**(self.parent.year_current - self.parent.year_start)

        # Double shortest_transport_cost if it is not a source_direct_connect
        self.ccs_facility_year_df.loc[self.ccs_facility_year_df.loc[:,'source_direct_connect']==False,'shortest_transport_cost'] = self.parent.supply_select_ts_penalty * self.ccs_facility_year_df.loc[self.ccs_facility_year_df.loc[:,'source_direct_connect']==False,'shortest_transport_cost']

        # Apply the transport cost to investment costs for non-operational sites
        temp_df = self.ccs_facility_year_df.loc[self.ccs_facility_year_df['years_operating'] == 0.00].copy()
        
        temp_df['co2_capture_inv_cost'] = temp_df['co2_capture_inv_cost'] + temp_df['shortest_transport_cost']
        temp_df['b0_nems_inv_cost']     = temp_df['b0_nems_inv_cost'] + temp_df['shortest_transport_cost']
        temp_df['b1_nems_inv_cost']     = temp_df['b1_nems_inv_cost'] + temp_df['shortest_transport_cost']
        temp_df['b2_nems_inv_cost']     = temp_df['b2_nems_inv_cost'] + temp_df['shortest_transport_cost']
        self.ccs_facility_year_df.update(temp_df)

        pass


    def determine_point_source_supplies(self):
        '''Match CO` :sub:`2` supplies against the NETL supply curve and produce block-level DataFrames of model year supply sources.
        
        Parameters
        ----------
        None

        Returns
        -------
        self.supply_sources_sparse_b0_df : DataFrame
            DataFrame of model year CO\ :sub:`2` supply sources for time period 0.

        self.supply_sources_sparse_b1_df : DataFrame
            DataFrame of model year CO\ :sub:`2` supply sources for time period 1.

        self.supply_sources_sparse_b2_df : DataFrame
            DataFrame of model year CO\ :sub:`2` supply sources for time period 2.
        
        '''
        ### Calculate CO2 supplies function
        def calc_supplies(ccs_facility_year_df, year_industrial_supply_df, cluster_facility_df, nems_inv_costs_block, block_year):
            ''' Calculate CO\ :sub:`2` supplies.

            * Cut facilities which have a cost of capture greater than the endogenous costs provided by the other NEMS models from *ccs_facility_year_df*.
            * Assess which economical NETL facilities to select for the optimization based on CO\ :sub:`2` supply.
                * Borderline CO\ :sub:`2` capture facilities where capture capacity > remaining CO\ :sub:`2` supply are selected, but only for the remaining CO\ :sub:`2` supply required.
            * Assess which cluster facilities to assign to remaining CO\ :sub:`2` supply requirements
            * Load selected supplies into block-level model year DataFrames.

            Parameters
            ----------
            ccs_facility_year_df : DataFrame
                DataFrame of existing carbon capture facilities eligible for carbon capture retrofit.

            year_industrial_supply_df : DataFrame
                DataFrame of model year CO\ :sub:`2` supply by block.

            cluster_facility_df : DataFrame
                DataFrame of representative clustered carbon capture facilities.

            nems_inv_costs_block : str
                Column name for block-level costs in *ccs_facility_year_df*.

            block_year : int
                CCATS optimization block year (i.e. 0, 1, 2).

            Returns
            -------
            temp_supply_sources_df : DataFrame
                DataFrame of block-level sparse CO\ :sub:`2` supply sources for model year optimization.
            

            '''
            # for block 0, limit capture at facilities to existing capacity
            if block_year == 0:

                # existing pipeline capacity for facilities
                if self.year_current <= self.parent.years_steo[-1]:
                    facility_exist_pipe_capacity = self.pipes_existing_df.copy()
                else:
                    facility_exist_pipe_capacity = self.pipes_existing_df.copy().loc[self.pipes_existing_df['pipeline_type'] != 'source_to_hsm_cent'] # exclude HSM centroids
                facility_exist_pipe_capacity = facility_exist_pipe_capacity.query("thruput_tonnes > 0").groupby('node_i_id')['thruput_tonnes'].sum().reset_index()

                # ccs_facility_year_df
                ccs_facility_year_df = ccs_facility_year_df.merge(facility_exist_pipe_capacity, how = 'inner', left_on = ['facility_id'], right_on = ['node_i_id'])
                ccs_facility_year_df = ccs_facility_year_df.reset_index()
                ind = ccs_facility_year_df['co2_volume'] > ccs_facility_year_df['thruput_tonnes']
                ccs_facility_year_df.loc[ind, 'co2_volume']  = ccs_facility_year_df.loc[ind, 'thruput_tonnes']

                # cluster_facility_df
                cluster_facility_df = cluster_facility_df.merge(facility_exist_pipe_capacity, how = 'inner', left_on = ['facility_id'], right_on = ['node_i_id'])
                cluster_facility_df = cluster_facility_df.reset_index()
                ind = cluster_facility_df['co2_volume'] > cluster_facility_df['thruput_tonnes']
                cluster_facility_df.loc[ind, 'co2_volume']  = cluster_facility_df.loc[ind, 'thruput_tonnes']

            # Assign years operating by block
            ccs_facility_year_df['years_operating'] = ccs_facility_year_df['years_operating'] + block_year
            ccs_facility_year_df.loc[ccs_facility_year_df['years_operating'] > 12, 'eligibility_45q'] = 0

            # Remove facilities which are uneconomical relative to NEMS model endogenous prices
            cost_mask = ccs_facility_year_df['co2_capture_inv_cost'] < ccs_facility_year_df[nems_inv_costs_block]
            years_operating_mask = (ccs_facility_year_df['years_operating'] <= self.parent.ccats_fin.duration_45q) & (ccs_facility_year_df['years_operating'] > 1)
            mask = cost_mask | years_operating_mask
            ccs_facility_year_df = ccs_facility_year_df[mask].copy()
            
            # Calibrate "facility_selection_ranking" variable for block-level differences
            # The 'facility_selection_ranking' column is how we maintain consistency for the order in which facilities are selected to capture CO2 between model years
            max_rank = ccs_facility_year_df['facility_selection_ranking'].max() + 1
            ccs_facility_year_df['facility_selection_ranking'] = ccs_facility_year_df['facility_selection_ranking'].fillna(max_rank) # Fill unranked facilities with the max ranking numbers
            ccs_facility_year_df.loc[ccs_facility_year_df['eligibility_45q'] == 0, 'facility_selection_ranking'] = max_rank # Set 45Q ineligible facilities as max rank

            ### Sort cost data in NETL facility roster in order of operational status, 45Q status and economics and then produce cumulative sum of CO2 capture potential by type and region
            ccs_facility_year_df = ccs_facility_year_df.sort_values(['operational_status',
                                                                     'eligibility_45q',
                                                                     'facility_selection_ranking',
                                                                     'years_operating',
                                                                     'co2_capture_inv_cost',
                                                                     'co2_capture_om_cost'], ascending= [False, False, True, False, True, True])
            ccs_facility_year_df['cum_capture'] = ccs_facility_year_df.groupby(['facility_type','supply_census_division', 'eligibility_45q'])['co2_volume'].cumsum()

    
            ### Merge capture cost df to supply df and only select most economical capture sources which meet requirements
            # df merge to industrial supply and capture costs
            temp_supply_sources_df = ccs_facility_year_df.reset_index().merge(year_industrial_supply_df,
                                                              how = 'outer',
                                                              left_on = ['facility_type','supply_census_division', 'new_vs_retrofit', 'eligibility_45q'],
                                                              right_on = ['facility_type','census_division', 'new_vs_retrofit', 'eligibility_45q'],
                                                              suffixes = ['','_merge']).set_index('index')
            temp_supply_sources_df = temp_supply_sources_df.drop(['census_division','index_merge'], axis = 1)
            temp_supply_sources_df = temp_supply_sources_df.rename(columns = {'census_division_merge':'census_division'})
            temp_supply_sources_df = temp_supply_sources_df.dropna(subset = ['capture_volume_tonnes'])
    
    
            ### Mask the dataset to remove faciliites that aren't economical and retain operating 45Q eligible facilities
            # Remove facilities that aren't as economical as NEMS facilities
            # Include facilities that were retrofit in the last 12 years
            operating_mask_min = (temp_supply_sources_df['years_operating'] > 0) & (temp_supply_sources_df['years_operating'] < self.parent.ccats_fin.duration_45q)
            cost_mask = temp_supply_sources_df['co2_capture_inv_cost'] < temp_supply_sources_df[nems_inv_costs_block]
            temp_supply_sources_df = temp_supply_sources_df[(operating_mask_min | cost_mask)]
    
    
            ### Mask facilities based on cumulative capture volume relative to CO2 supply
            # Mask for most economical or operating facilities which fall under the capture volume requirements
            volume_mask = temp_supply_sources_df['cum_capture'] <= temp_supply_sources_df['capture_volume_tonnes']
    
            # Include borderline facilities where only partial capacity is required, adjusting volume to only incorporate remaining capture requirements
            partial_facility_df = temp_supply_sources_df.copy()
            partial_facility_df['volume_diff'] = temp_supply_sources_df['capture_volume_tonnes'] - temp_supply_sources_df['cum_capture']
            partial_facility_df = partial_facility_df.loc[partial_facility_df['volume_diff'] < 0]
            partial_facility_mask = partial_facility_df.groupby(['facility_type','census_division','eligibility_45q'])['volume_diff'].idxmax()
            partial_facility_df = partial_facility_df.loc[partial_facility_mask]
            partial_facility_df['co2_volume'] = partial_facility_df['co2_volume'] - partial_facility_df['volume_diff'].apply(abs)
            partial_facility_df = partial_facility_df.drop(['volume_diff'], axis = 1)
            partial_facility_df = partial_facility_df.loc[partial_facility_df['co2_volume'] > 0]
    
    
            ### Apply volume mask then concat final facilities to main "supply sources" df
            temp_supply_sources_df = temp_supply_sources_df[volume_mask].copy()
            temp_supply_sources_df = pd.concat([temp_supply_sources_df, partial_facility_df], ignore_index = False)
    
    
            ### Check if facility volumes line up against model supply volumes and get remaining CO2 supply volumes to be assigned
            nems_co2_supply = year_industrial_supply_df.copy().groupby(['census_division', 'facility_type','eligibility_45q']).sum()
            netl_supply = temp_supply_sources_df[['co2_volume','census_division','facility_type','eligibility_45q']].copy().groupby(['census_division', 'facility_type','eligibility_45q']).sum()
            netl_supply = netl_supply.combine_first(nems_co2_supply[[]]).fillna(0.0) # Ensure all supplied volumes are represented
            nems_co2_supply['diff'] = nems_co2_supply['capture_volume_tonnes'] - netl_supply['co2_volume']
    
    
            ### Move on to FANs
            # If CO2 supplied exceeds facility capacities from NETL CCRD, assign remaining CO2 supply to FANs
            if nems_co2_supply['diff'].sum() > 0:
                # Get missing supplies by region, facility type, and 45q elgibility
                nems_co2_supply = nems_co2_supply.loc[nems_co2_supply['diff'] > 0].copy()
                remaining_supply_df = nems_co2_supply.reset_index().copy()
    
                # Get relevant cluster facility types
                temp_cluster_df = cluster_facility_df.copy()
                temp_cluster_df = temp_cluster_df.reset_index().merge(remaining_supply_df,
                                              how = 'left',
                                              left_on = ['supply_census_division','facility_type'],
                                              right_on = ['census_division','facility_type'],
                                              suffixes = ['', '_merge']).set_index('index')
                temp_cluster_df['eligibility_45q'] = temp_cluster_df['eligibility_45q_merge']
                temp_cluster_df = temp_cluster_df.dropna(subset = ['diff'])
                temp_cluster_df = temp_cluster_df.drop(['census_division', 'index_merge'], axis=1)
                temp_cluster_df = temp_cluster_df.rename(columns={'census_division_merge': 'census_division'})
    
                # Get number of each new facility required and adjust CO2 volumes to match supply
                temp_cluster_df['facility_count'] = (temp_cluster_df['diff'] / (temp_cluster_df['co2_volume'] * temp_cluster_df['co2_ratio'])).apply(np.ceil)
                temp_cluster_df['co2_volume'] = temp_cluster_df['diff'] * temp_cluster_df['co2_ratio'] # co2_ratio, the split of CO2 going between cluster facilities within the same region
    
                # Format temp_cluster_df for merge
                temp_cluster_df = temp_cluster_df.drop(['capture_volume_tonnes','diff','facility_count'],axis = 1)
                temp_cluster_df['facility_name'] = temp_cluster_df['facility_name'] + temp_cluster_df.index.astype(str)
    
                # Concat cluster sources to main supply_sources_df df
                temp_supply_sources_df = pd.concat([temp_supply_sources_df, temp_cluster_df], ignore_index = False)
            else:
                pass


            ### Reformat temp_supply_sources for write to supply_sources_sparse
            temp_supply_sources_df[['eligibility_45q','census_division']] = temp_supply_sources_df[['eligibility_45q','census_division']].astype(int)
            cols_to_drop = ['co2_capture_inv_transport_cost', 'b0_nems_inv_transport_cost',
                            'b1_nems_inv_transport_cost', 'b2_nems_inv_transport_cost', 'eligibility_45q_merge', 'new_vs_retrofit_merge',
                            'year', 'capture_volume_tonnes', 'cum_capture']
            for col in cols_to_drop:
                if col in temp_supply_sources_df.columns:
                    temp_supply_sources_df = temp_supply_sources_df.drop(col, axis = 1)

            ### Drop very small edge-case supply sources
            temp_supply_sources_df = temp_supply_sources_df.loc[temp_supply_sources_df['co2_volume'] > 100]

            if block_year == 0:
                self.parent.pytest.nems_co2_supply = year_industrial_supply_df.copy().groupby(['census_division', 'facility_type', 'eligibility_45q']).sum()
                self.parent.pytest.supply_sources = temp_supply_sources_df

            return temp_supply_sources_df
        
        ### END FUNCTION ####

        ### Load in relevant supply source dfs
        # For each block, determine which NETL facilities and cluster facilities will be most economical for allocating the CO2 supply from NEMS
        self.supply_sources_sparse_b0_df = calc_supplies(self.ccs_facility_year_df,
                                                  self.year_industrial_supply_b0_df,
                                                  self.cluster_facility_df,
                                                  'b0_nems_inv_cost',
                                                  0).reset_index()
        self.supply_sources_sparse_b1_df = calc_supplies(self.ccs_facility_year_df,
                                                  self.year_industrial_supply_b1_df,
                                                  self.cluster_facility_df,
                                                  'b1_nems_inv_cost',
                                                  1).reset_index()
        self.supply_sources_sparse_b2_df = calc_supplies(self.ccs_facility_year_df,
                                                  self.year_industrial_supply_b2_df,
                                                  self.cluster_facility_df,
                                                  'b2_nems_inv_cost',
                                                   2).reset_index()

        pass


    def determine_facility_eligibility_45q(self):
        '''Determine 45Q eligibility for CO\ :sub:`2` capture facilities by model block.
        
            * Calculate remaining years of 45Q eligibility by CO\ :sub:`2` supply source by block:
                * If self.split_45q_supply_switch == True, split each CO\ :sub:`2` facility supply into 45Q and NTC components by year, 
                  else determine whether flow is 45Q or NTC based on yr_remain_45q and self.block_45q_yr,
                * Concatenate new CO\ :sub:`2` capture faciliites to *co2_facility_eligibility_df* and update years operating.

        Parameters
        ---------
        None

        Returns
        -------
        self.supply_sources_sparse_b0_df : DataFrame
            DataFrame of time period 0 sparse CO\ :sub:`2` supply sources for model year optimization.

        self.supply_sources_sparse_b1_df : DataFrame
            DataFrame of time period 1 sparse CO\ :sub:`2` supply sources for model year optimization.

        self.supply_sources_sparse_b2_df : DataFrame
            DataFrame of time period 2 sparse CO\ :sub:`2` supply sources for model year optimization.

        self.co2_facility_eligibility_df : DataFrame
            DataFrame of model year CO\ :sub:`2` capture facility 45Q eligibility.
        
        '''
        ### Update blocks 1 and 2 using block 0 duration
        # Identify facilities operating in block 0
        supply_facilities_b0 = self.supply_sources_sparse_b0_df.query('co2_volume > 0').copy()['facility_id']

        # Block 1
        indices = self.supply_sources_sparse_b1_df.facility_id.isin(supply_facilities_b0)
        self.supply_sources_sparse_b1_df.loc[indices, 'years_operating'] = self.supply_sources_sparse_b1_df.loc[
                                                                               indices, 'years_operating'] + self.parent.ccats_fin.duration_b0

        # Block 2
        indices = self.supply_sources_sparse_b2_df.facility_id.isin(supply_facilities_b0)
        self.supply_sources_sparse_b2_df.loc[indices, 'years_operating'] = self.supply_sources_sparse_b2_df.loc[
                                                                               indices, 'years_operating'] + self.parent.ccats_fin.duration_b0

        ### Update block 2 using block 1 duration
        # Identify facilities operating in block 1
        supply_facilities_b1 = self.supply_sources_sparse_b1_df.query('co2_volume > 0').copy()['facility_id']

        # Block 2
        indices = self.supply_sources_sparse_b2_df.facility_id.isin(supply_facilities_b1)
        self.supply_sources_sparse_b2_df.loc[indices, 'years_operating'] = self.supply_sources_sparse_b2_df.loc[
                                                                               indices, 'years_operating'] + self.parent.ccats_fin.duration_b1

        if self.parent.debug_switch:
            filename_supply_b0 = self.output_path + 'optimization_inputs//supply//before_45q_update_supply_sources_sparse_b0_df_' + str(
                self.parent.year_current) + '.csv'
            filename_supply_b1 = self.output_path + 'optimization_inputs//supply//before_45q_update_supply_sources_sparse_b1_df_' + str(
                self.parent.year_current) + '.csv'
            filename_supply_b2 = self.output_path + 'optimization_inputs//supply//before_45q_update_supply_sources_sparse_b2_df_' + str(
                self.parent.year_current) + '.csv'
            self.supply_sources_sparse_b0_df.to_csv(filename_supply_b0)
            self.supply_sources_sparse_b1_df.to_csv(filename_supply_b1)
            self.supply_sources_sparse_b2_df.to_csv(filename_supply_b2)

        ### Update eligibility for current run
        # Update eligibility for block 1
        if self.parent.ccats_fin.duration_b1 == 1:
            self.supply_sources_sparse_b1_df.loc[self.supply_sources_sparse_b1_df[
                                                     'years_operating'] > self.parent.ccats_fin.duration_45q, 'eligibility_45q'] = 0
        else:
            # mark faciliites as ineligibile if they have fully used up the 45Q duration
            self.supply_sources_sparse_b1_df.loc[self.supply_sources_sparse_b1_df[
                                                     'years_operating'] > self.parent.ccats_fin.duration_45q, 'eligibility_45q'] = 0

            # calculate years of 45Q for remaining 45Q eligible facilities
            self.supply_sources_sparse_b1_df['yr_remain_45q'] = 0.0
            indices = self.supply_sources_sparse_b1_df.eligibility_45q == 1
            self.supply_sources_sparse_b1_df.loc[indices, 'yr_remain_45q'] = self.parent.ccats_fin.duration_45q - \
                                                                             self.supply_sources_sparse_b1_df.loc[
                                                                                 indices, 'years_operating']

            # If TRUE, split supply into 45Q and NTC components
            if self.split_45q_supply_switch:

                # if a facility is only eligibile for part of the 45q eligibile portion of the block, split the supply into eligible and non-eligible, proportional to the years of eligiblity
                block_45q_duration = min(self.parent.ccats_fin.duration_b1, self.parent.ccats_fin.duration_45q)
                indices = (0 < self.supply_sources_sparse_b1_df.yr_remain_45q) & (
                            self.supply_sources_sparse_b1_df.yr_remain_45q < block_45q_duration)

                non_45q_supply = self.supply_sources_sparse_b1_df.loc[indices, :].copy()
                non_45q_supply.co2_volume = non_45q_supply.co2_volume * (
                            1.0 - (non_45q_supply.yr_remain_45q / block_45q_duration))
                non_45q_supply.eligibility_45q = 0

                self.supply_sources_sparse_b1_df.loc[indices, 'co2_volume'] = self.supply_sources_sparse_b1_df.loc[
                                                                                  indices, 'co2_volume'] * (
                                                                                          self.supply_sources_sparse_b1_df.loc[
                                                                                              indices, 'yr_remain_45q'] / block_45q_duration)

                # join self.supply_sources_sparse_b1_df and non_45q_supply
                self.supply_sources_sparse_b1_df = pd.concat([self.supply_sources_sparse_b1_df, non_45q_supply])

            # If self.split_45q_supply_switch is FALSE, determine whether flow is 45Q or NTC based on yr_remain_45q and self.block_45q_yr
            else:

                # if block duration is less than self.block_45q_yr, the limit is the entire length of the block
                if self.parent.ccats_fin.duration_b1 < self.block_45q_yr:
                    yr_remain_45q_limit = self.parent.ccats_fin.duration_b1

                # Otherwise, limit is self.block_45q_yr
                else:
                    yr_remain_45q_limit = self.block_45q_yr

                self.supply_sources_sparse_b1_df.loc[
                    self.supply_sources_sparse_b1_df['yr_remain_45q'] < yr_remain_45q_limit, 'eligibility_45q'] = 0

        # Update eligibility for block 2
        if self.parent.ccats_fin.duration_b2 == 1:
            self.supply_sources_sparse_b2_df.loc[self.supply_sources_sparse_b2_df[
                                                     'years_operating'] > self.parent.ccats_fin.duration_45q, 'eligibility_45q'] = 0
        else:
            # mark faciliites as ineligibile if they have fully used up the 45Q duration
            self.supply_sources_sparse_b2_df.loc[self.supply_sources_sparse_b2_df[
                                                     'years_operating'] > self.parent.ccats_fin.duration_45q, 'eligibility_45q'] = 0

            # calculate years of 45Q for remaining 45Q eligible facilities
            self.supply_sources_sparse_b2_df['yr_remain_45q'] = 0.0
            indices = self.supply_sources_sparse_b2_df.eligibility_45q == 1
            self.supply_sources_sparse_b2_df.loc[indices, 'yr_remain_45q'] = self.parent.ccats_fin.duration_45q - \
                                                                             self.supply_sources_sparse_b2_df.loc[
                                                                                 indices, 'years_operating']

            # If TRUE, split supply into 45Q and NTC components
            if self.split_45q_supply_switch:

                # if a facility is only eligibile for part of the 45q eligibile portion of the block, split the supply into eligible and non-eligible, proportional to the years of eligiblity
                block_45q_duration = min(self.parent.ccats_fin.duration_b2, self.parent.ccats_fin.duration_45q)
                indices = (0 < self.supply_sources_sparse_b2_df.yr_remain_45q) & (
                            self.supply_sources_sparse_b2_df.yr_remain_45q < block_45q_duration)

                non_45q_supply = self.supply_sources_sparse_b2_df.loc[indices, :].copy()
                non_45q_supply.co2_volume = non_45q_supply.co2_volume * (
                            1.0 - (non_45q_supply.yr_remain_45q / block_45q_duration))
                non_45q_supply.eligibility_45q = 0

                self.supply_sources_sparse_b2_df.loc[indices, 'co2_volume'] = self.supply_sources_sparse_b2_df.loc[
                                                                                  indices, 'co2_volume'] * (
                                                                                          self.supply_sources_sparse_b2_df.loc[
                                                                                              indices, 'yr_remain_45q'] / block_45q_duration)

                # join self.supply_sources_sparse_b2_df and non_45q_supply
                self.supply_sources_sparse_b2_df = pd.concat([self.supply_sources_sparse_b2_df, non_45q_supply])

            # If self.split_45q_supply_switch is FALSE, determine whether flow is 45Q or NTC based on yr_remain_45q and self.block_45q_yr
            else:

                # if block duration is less than self.block_45q_yr, the limit is the entire length of the block
                if self.parent.ccats_fin.duration_b2 < self.block_45q_yr:
                    yr_remain_45q_limit = self.parent.ccats_fin.duration_b2

                # Otherwise, limit is self.block_45q_yr
                else:
                    yr_remain_45q_limit = self.block_45q_yr

                self.supply_sources_sparse_b2_df.loc[
                    self.supply_sources_sparse_b2_df['yr_remain_45q'] < yr_remain_45q_limit, 'eligibility_45q'] = 0

        ### Eliminate duplicates by grouping by facility_id and eligibility_45q
        # approach for combining duplicates - sum co2 volume, and take the first value of everything else
        agg_method = {'facility_name': 'first',
                      'facility_type': 'first',
                      'state': 'first',
                      'co2_volume': 'sum',
                      'co2_cost_inv_2018$': 'first',
                      'co2_cost_om_2018$': 'first',
                      'supply_latitude': 'first',
                      'supply_longitude': 'first',
                      'plant_id': 'first',
                      'unit_id': 'first',
                      'summer_cap': 'first',
                      'cap_factor': 'first',
                      'emm_region': 'first',
                      'emm_census_region': 'first',
                      'ccs_overnight_cost': 'first',
                      'ccs_fixed_om_cost': 'first',
                      'ccs_var_om_cost': 'first',
                      'rrc': 'first',
                      'supply_hsm_region': 'first',
                      'supply_census_region': 'first',
                      'supply_census_division': 'first',
                      'facility_new': 'first',
                      'co2_ratio': 'first',
                      'cluster_type': 'first',
                      'operational_status': 'first',
                      'years_operating': 'first',
                      'co2_capture_inv_cost': 'first',
                      'co2_capture_om_cost': 'first'}

        # groubpy required index and combine
        self.supply_sources_sparse_b0_df = self.supply_sources_sparse_b0_df.groupby(
            ['facility_id', 'eligibility_45q']).agg(agg_method).reset_index()
        self.supply_sources_sparse_b1_df = self.supply_sources_sparse_b1_df.groupby(
            ['facility_id', 'eligibility_45q']).agg(agg_method).reset_index()
        self.supply_sources_sparse_b2_df = self.supply_sources_sparse_b2_df.groupby(
            ['facility_id', 'eligibility_45q']).agg(agg_method).reset_index()

        if self.parent.debug_switch:
            filename_supply_b0 = self.output_path + 'optimization_inputs//supply//after_45q_update_supply_sources_sparse_b0_df_' + str(self.parent.year_current) + '.csv'
            filename_supply_b1 = self.output_path + 'optimization_inputs//supply//after_45q_update_supply_sources_sparse_b1_df_' + str(self.parent.year_current) + '.csv'
            filename_supply_b2 = self.output_path + 'optimization_inputs//supply//after_45q_update_supply_sources_sparse_b2_df_' + str(self.parent.year_current) + '.csv'
            self.supply_sources_sparse_b0_df.to_csv(filename_supply_b0)
            self.supply_sources_sparse_b1_df.to_csv(filename_supply_b1)
            self.supply_sources_sparse_b2_df.to_csv(filename_supply_b2)

        ### Update co2_facility_eligibility_df based on current CCATS run (block 0)
        temp_supply_df = self.supply_sources_sparse_b0_df.query('co2_volume > 0').copy()
        temp_supply_df = temp_supply_df[['facility_id', 'facility_type', 'years_operating', 'eligibility_45q']]

        # Filter for new facilities
        if int(self.year_current) > self.parent.year_start:
            temp_supply_df = temp_supply_df.loc[~temp_supply_df['facility_id'].isin(self.co2_facility_eligibility_df['facility_id'])].copy()
        else:
            pass

        # Concatenate new facilities to eligibility df
        self.co2_facility_eligibility_df = pd.concat([self.co2_facility_eligibility_df, temp_supply_df],
                                                     ignore_index=True)

        # Update years operating for facilities
        years_operating_df = self.supply_sources_sparse_b0_df.loc[self.supply_sources_sparse_b0_df['facility_id'].isin(
            self.co2_facility_eligibility_df['facility_id'])].copy()
        years_operating_df = years_operating_df[['facility_id', 'years_operating']]
        years_operating_df = years_operating_df.set_index('facility_id')
        years_operating_df = years_operating_df.drop_duplicates()
        self.co2_facility_eligibility_df = self.co2_facility_eligibility_df.set_index('facility_id', drop=True)
        self.co2_facility_eligibility_df.update(years_operating_df)
        self.co2_facility_eligibility_df = self.co2_facility_eligibility_df.reset_index(drop=False)
        

        ### Update co2_facility_eligibility_df for next CCATS run
        # Set 45q eligibility for facilities
        self.co2_facility_eligibility_df.loc[:, 'years_operating'] += 1

        # Set 45q eligibility for facilities
        self.co2_facility_eligibility_df.loc[self.co2_facility_eligibility_df['years_operating'] > self.parent.ccats_fin.duration_45q, 'eligibility_45q'] = 0

        # Remove duplicates - only keep one instance of each facility, sort by years_operating, and then eligibility
        self.co2_facility_eligibility_df = self.co2_facility_eligibility_df.sort_values(['years_operating','eligibility_45q'],ascending=False).groupby('facility_id').first().reset_index()

        pass


    def zero_inv_costs_for_retrofit_facilities(self):
        '''Zero out investment costs for facilities which have been retrofit, and establish facility ranking to maintain facility selection
           consistency between model years.

            * Set capital costs for CO\ :sub:`2` facilities which have already been retrofit to 0.01% of initial costs to maintain sort order in facility roster.

        Parameters
        ----------
        None

        Returns
        -------
        self.i_co2_supply_facility_df : DataFrame
            DataFrame of CO2 capture facility attributes from NETL - input data.
        
        '''
        # Select supply sources from previous model year time period
        temp_df = self.i_co2_supply_facility_df.loc[self.i_co2_supply_facility_df['facility_id'].isin(self.supply_sources_sparse_b1_df['facility_id'])].copy()

        # In first year update all dfs, in future years just when years operating of self.co2_supply_facility_df['years_operating'] == 0
        if self.year_current == self.parent.year_start:
            # Select capturing facilities and rank by CO2 capture investment cost
            temp_df = temp_df.loc[temp_df['facility_new'] == 'N'].copy()
            temp_df = temp_df.sort_values(['co2_capture_inv_cost'], ascending = True)
            temp_df = temp_df.reset_index()
            temp_df.index = temp_df.index + 1
            temp_df['facility_selection_ranking'] = temp_df.index.copy()
            temp_df = temp_df.set_index('index')

            # Zero out investment costs
            temp_df['co2_capture_inv_cost'] = 0.0

            # Update i_co2_supply_facility_df
            self.i_co2_supply_facility_df.update(temp_df)

        else:
            # Select newly capturing facilities and add to ranking
            temp_df = temp_df.reset_index().merge(self.co2_facility_eligibility_df[['facility_id','years_operating']],
                                    how = 'left',
                                    on = 'facility_id',
                                    suffixes = ['_drop','']).set_index('index')
            temp_df = temp_df.drop(['years_operating_drop'],axis = 1)
            temp_df = temp_df.loc[temp_df['years_operating'] == 1]
            temp_df = temp_df.loc[temp_df['facility_selection_ranking'].isna()]
            temp_df = temp_df.loc[temp_df['facility_new'] == 'N'].copy()
            temp_df = temp_df.sort_values(['co2_capture_inv_cost'], ascending = True)
            temp_df = temp_df.reset_index()
            temp_df.index = temp_df.index + self.i_co2_supply_facility_df['facility_selection_ranking'].max() + 1
            temp_df['facility_selection_ranking'] = temp_df.index.copy()
            temp_df = temp_df.set_index('index')
            
            # Zero out investment costs
            temp_df['co2_capture_inv_cost'] = temp_df['co2_capture_inv_cost'] = 0.0
            
            # Update i_co2_supply_facility_df
            self.i_co2_supply_facility_df.update(temp_df[['co2_capture_inv_cost', 'facility_selection_ranking']])

        ### Debug facility ranking
        if self.parent.debug_switch:
            ranked_facilities = self.i_co2_supply_facility_df.copy().dropna(subset=['facility_selection_ranking']).sort_values('facility_selection_ranking')
            ranked_facilities.loc[:, 'year_current'] = self.year_current
            
            # integrated run - separate file for each year (only keep last cycle and iteration)
            if self.parent.integrated_switch:
                ranked_facilities_filename = self.output_path + 'facility_ranking//facility_ranking_integrated_' + str(self.year_current) + '.csv'
                ranked_facilities.to_csv(ranked_facilities_filename)
            
            # standalone run - single file for all years
            else:
                ranked_facilities_filename = self.output_path + 'facility_ranking//facility_ranking_standalone_all_years.csv'
                if self.year_current == self.parent.year_start:
                    ranked_facilities.to_csv(ranked_facilities_filename)
                else:
                    ranked_facilities.to_csv(ranked_facilities_filename, mode='a', header=False)

        pass


    #######################################
    ###Pipeline Network Preprocessing
    #######################################


    def filter_available_pipelines(self):
        '''Remove inactive CO\ :sub:`2` supply, storage and CO\ :sub:`2` EOR nodes from the pipeline network to make more sparse.

        Parameters
        ----------
        None

        Returns
        -------
        pipeline_lookup_df : DataFrame
            DataFrame of model year CO\ :sub:`2` pipeline lookup.
        
        '''
        # Mask for supply facilities in sparse df
        supply_source_list = (self.supply_sources_sparse_b0_df['facility_id'].tolist() + \
                              self.supply_sources_sparse_b1_df['facility_id'].tolist() + \
                              self.supply_sources_sparse_b2_df['facility_id'].tolist())
        supply_mask = self.pipeline_lookup_df['node_i_id'].isin(supply_source_list)

        # Mask for EOR facilities in EOR demand set
        eor_mask = self.pipeline_lookup_df['node_j_id'].isin(self.eor_demand_df['node_id'].copy())

        # Mask for storage facilities in storage input file
        storage_mask = self.pipeline_lookup_df['node_j_id'].isin(self.storage_new_df['node_id'].copy())

        # Mask for node and arc types that are always in pipeline lookup
        ts_mask             = self.pipeline_lookup_df['node_i_type'].isin(['operational_ts_node','uniform_ts_node'])
        op_ts_mask          = self.pipeline_lookup_df['pipeline_type'] == 'op_source_to_ts_node'
        nat_co2_dem_mask    = self.pipeline_lookup_df['pipeline_type'] == 'nat_co2_to_demand'        
        op_eor_mask         = self.pipeline_lookup_df['pipeline_type'] == 'source_to_hsm_cent'
        supply_ts_mask      = self.pipeline_lookup_df['pipeline_type'] == 'source_to_ts_node'
        ts_ts_mask          = self.pipeline_lookup_df['pipeline_type'] == 'ts_node_to_ts_node'
        dac_ts_mask         = self.pipeline_lookup_df['pipeline_type'] == 'dac_to_ts_node'
        nat_co2_ts_mask     = self.pipeline_lookup_df['pipeline_type'] == 'nat_co2_to_ts_node'

        # Create cumulative mask
        mask = ((supply_mask & eor_mask)        |
                (supply_mask & storage_mask)    |
                (ts_mask & eor_mask)            |
                (ts_mask & storage_mask)        |
                (supply_mask & supply_ts_mask)  |
                (supply_mask & op_ts_mask)      |
                (supply_mask & op_eor_mask)     |
                ts_ts_mask                      |
                dac_ts_mask                     |
                nat_co2_dem_mask                |
                nat_co2_ts_mask)

        # Apply mask to pipeline lookup
        self.pipeline_lookup_df = self.pipeline_lookup_df[mask].copy()
        self.parent.pytest.pipeline_lookup_df = self.pipeline_lookup_df

        # prevent the construction of new pipelines to operational facilities before new_pipes_to_exist_facilities
        if self.parent.year_current < self.new_pipes_to_exist_facilities:
            for node_i_id in self.i_co2_supply_facility_df.loc[self.i_co2_supply_facility_df.loc[:,'operational_status']=='Y','facility_id']:
                self.pipeline_lookup_df.loc[self.pipeline_lookup_df.loc[:,'node_i_id']==node_i_id, 'max_throughput_tonnes'] = 0.0

        pass


    def assign_electricity_costs(self):
        '''Assign electricity costs ($/MWh) for pipelines.

        Parameters
        ----------
        None
        
        Returns
        -------
        self.pipes_existing_df : DataFrame
            DataFrame of model year existing CO\ :sub:`2` pipeline infrastructure.

        self.pipeline_lookup_df : DataFrame
            DataFrame of model year CO\ :sub:`2` pipeline lookup.
        
        '''
        # Get annual electricity prices
        temp_elec_prices = self.parent.rest_pelin.copy()
        temp_elec_prices = temp_elec_prices.loc[temp_elec_prices.index.get_level_values(1) == int(self.year_current)]
        temp_elec_prices = temp_elec_prices.droplevel(1)

        ### Debug electricity prices
        if self.parent.debug_switch == True:
            elec_price_filename = self.output_path + 'optimization_inputs//elec_costs//elec_prices_' + str(self.year_current) + '_' + '_' + str(self.parent.cycle_current) + '.csv'
            temp_elec_prices.to_csv(elec_price_filename)
        
        # Apply to existing pipelines
        try:
            self.pipes_existing_df = self.pipes_existing_df.drop('elec_cost_1987$', axis = 1)
            self.pipeline_lookup_df = self.pipeline_lookup_df.drop('elec_cost_1987$', axis = 1)

        except:
            self.pipes_existing_df = self.pipes_existing_df.merge(temp_elec_prices,
                                                              how = 'left',
                                                              left_on = 'node_i_census_division',
                                                              right_index = True)
        
        # Apply to pipeline lookup
            self.pipeline_lookup_df = self.pipeline_lookup_df.merge(temp_elec_prices,
                                                              how = 'left',
                                                              left_on = 'node_i_census_division',
                                                              right_index = True)
        
        pass


    #######################################
    ###Storage Site Preprocessing
    #######################################


    def update_storage_infrastructure(self):
        '''Declare and update existing storage infrastructure based on last model year results.

            * If first model year, instantiate existing storage sites, and
            * After first model year, merge prior year model results to existing storage infrastructure and update relevant storage site attributes.
        
        Parameters
        ----------
        None

        Returns
        -------
        self.storage_existing_df : DataFrame
            DataFrame of existing CO\ :sub:`2` storage infrastructure in a given model year.
        
        '''
        # Instantiate existing storage infrastructure
        if int(self.year_current) <= self.parent.year_start:
            self.storage_existing_df = self.i_storage_df[['node_id', 'active_aor_co2_store_remaining', 'max_co2_store_cap', 'max_co2_project_tonnes',
                                                          'max_inj_projects','inj_rate_project_tonnes_yr', 'max_injectivity', 'storage_aors_existing', 'tax_credit_value',
                                                          'tax_credit_value_dac',
                                                          'capex_site_dev', 'fixom_site_dev', 'varom_site_dev',
                                                          'capex_construction', 'fixom_construction', 'varom_construction',
                                                          'capex_injection', 'fixom_injection', 'varom_injection',
                                                          'capex_pisc', 'fixom_pisc', 'varom_pisc',
                                                          'capex_closure', 'fixom_closure', 'varom_closure',
                                                          'storage_aors_available']].copy()
            self.storage_existing_df['net_storage_injectivity'] = 0
            self.storage_existing_df['net_storage_injectivity'] = self.storage_existing_df['storage_aors_existing'] * self.storage_existing_df['inj_rate_project_tonnes_yr']
            self.storage_existing_df['active_aor_co2_store_remaining'] = self.storage_existing_df['storage_aors_existing'] * self.storage_existing_df['max_co2_project_tonnes']


        # If after first model year, update existing storage infrastructure with last year's injection sites
        else:
            ### Existing Storage
            # Merge last year AORs and CO2 Stored
            self.storage_existing_df = self.storage_existing_df.merge(self.parent.new_aors_df[['node_id','new_aors']],
                                                            how = 'left',
                                                            on = 'node_id')
            self.storage_existing_df = self.storage_existing_df.merge(self.parent.store_prev_b0_df[['node_id','co2_stored']],
                                                            how = 'left',
                                                            on = 'node_id')

            # Update storage injectivity and total capacity
            self.storage_existing_df['storage_aors_existing'] = self.storage_existing_df['storage_aors_existing'] + self.storage_existing_df['new_aors']
            self.storage_existing_df['storage_aors_available'] = self.storage_existing_df['storage_aors_available'] - self.storage_existing_df['new_aors']
            self.storage_existing_df['net_storage_injectivity'] = self.storage_existing_df['storage_aors_existing'] * self.storage_existing_df['inj_rate_project_tonnes_yr']
            self.storage_existing_df['active_aor_co2_store_remaining'] = self.storage_existing_df['active_aor_co2_store_remaining'] + self.storage_existing_df['new_aors'] * self.storage_existing_df['max_co2_project_tonnes']
            self.storage_existing_df['active_aor_co2_store_remaining'] = self.storage_existing_df['active_aor_co2_store_remaining'] - self.storage_existing_df['co2_stored']
            self.storage_existing_df['max_co2_store_cap'] = self.storage_existing_df['max_co2_store_cap'] - self.storage_existing_df['co2_stored']
            self.storage_existing_df = self.storage_existing_df.drop(['new_aors', 'co2_stored'], axis = 1)


            ### Potential Storage
            self.storage_new_df['storage_aors_existing'] = self.storage_existing_df['storage_aors_existing'].copy()
            self.storage_new_df['storage_aors_available'] = self.storage_existing_df['storage_aors_available'].copy()
            self.storage_new_df['active_aor_co2_store_remaining'] = self.storage_existing_df['active_aor_co2_store_remaining'].copy()
            self.storage_new_df['co2_storage_cap'] = self.storage_existing_df['max_co2_store_cap'].copy()

        pass

    #######################################
    ###Natural CO2 Preprocessing
    #######################################
    def update_natural_co2_reserves(self):
        '''Declare and update natural CO2 reserves based on last model year results.

        Returns
        -------

        '''
        # Instantiate existing natural CO2 fields
        if int(self.year_current) <= self.parent.year_start:
            self.nat_co2_lookup_df = self.i_nat_co2_lookup_df.copy()
            

        else:
            self.nat_co2_lookup_df = self.nat_co2_lookup_df.merge(self.parent.nat_co2_prev_b0,
                                                                  how = 'left',
                                                                  left_on = 'node_id',
                                                                  right_index= True)
            self.nat_co2_lookup_df = self.nat_co2_lookup_df.rename(columns = {'co2_volume':'prev_year_nat_co2'})
            self.nat_co2_lookup_df['net_err_mt_current'] = self.nat_co2_lookup_df['net_err_mt_current'] - self.nat_co2_lookup_df['prev_year_nat_co2']
            self.nat_co2_lookup_df = self.nat_co2_lookup_df.drop(['prev_year_nat_co2'],axis = 1)

        # todo add check to make sure that CO2 used never exceeds CO2 avaialble

        pass

    #######################################
    ###Setup Optimization
    #######################################


    def declare_supply_opt(self):
        '''Declare supply DataFrames for the main CCATS optimization.

        Parameters
        ----------
        None

        Returns
        -------
        self.supply_sources_sparse_b0_df : DataFrame
            DataFrame of time period 0 sparse CO\ :sub:`2` supply sources for model year optimization.

        self.supply_sources_sparse_b1_df : DataFrame
            DataFrame of time period 1 sparse CO\ :sub:`2` supply sources for model year optimization.

        self.supply_sources_sparse_b2_df : DataFrame
            DataFrame of time period 2 sparse CO\ :sub:`2` supply sources for model year optimization.

        self.co2_supply_b0_df : DataFrame
            DataFrame of time period 0 CO\ :sub:`2` supply sources and volumes for the main optimization.

        self.co2_supply_b1_df : DataFrame
            DataFrame of time period 1 CO\ :sub:`2` supply sources and volumes for the main optimization.

        self.co2_supply_b2_df : DataFrame
            DataFrame of time period 2 CO\ :sub:`2` supply sources and volumes for the main optimization.

        '''
        ### Existing Supply Declarations
        # Time Period 0
        self.supply_sources_sparse_b0_df = self.supply_sources_sparse_b0_df.rename(columns={'facility_id': 'node_id', 'supply_latitude': 'latitude', 'supply_longitude': 'longitude'})
        self.supply_sources_sparse_b0_df['node_type'] = 'supply'
        
        # Time Period 1
        self.supply_sources_sparse_b1_df = self.supply_sources_sparse_b1_df.rename(columns={'facility_id': 'node_id', 'supply_latitude': 'latitude', 'supply_longitude': 'longitude'})
        self.supply_sources_sparse_b1_df['node_type'] = 'supply'

        # Time Period 2
        self.supply_sources_sparse_b2_df = self.supply_sources_sparse_b2_df.rename(columns={'facility_id': 'node_id', 'supply_latitude': 'latitude', 'supply_longitude': 'longitude'})
        self.supply_sources_sparse_b2_df['node_type'] = 'supply'

        ### Filter supply to match existing pipeline network in first model year, then use assessed sparse sites
        # Time Period 0
        if int(self.year_current) <= self.parent.year_start:
            supply_sources_df = self.supply_sources_sparse_b0_df.copy()
            temp_pipes_existing_df = self.pipes_existing_df.copy()

            # Only get pipes with volumes
            temp_pipes_existing_df = temp_pipes_existing_df.loc[temp_pipes_existing_df['thruput_tonnes'] > 0].copy()

            # Get supply that is operational
            temp_df = supply_sources_df.loc[~supply_sources_df['node_id'].isin(temp_pipes_existing_df['node_i_id'])].copy()
            temp_df['co2_volume'] = 0
            supply_sources_df.update(temp_df)
            self.co2_supply_b0_df = supply_sources_df.copy()
        else:
            self.co2_supply_b0_df = self.supply_sources_sparse_b0_df.copy()

        # Time Periods 1 & 2
        self.co2_supply_b1_df = self.supply_sources_sparse_b1_df.copy()
        self.co2_supply_b2_df = self.supply_sources_sparse_b2_df.copy()

        pass


    def prepare_storage_opt(self):
        '''Prepare storage DataFrames for the main CCATS optimization.

        Parameters
        ----------
        None
        
        Returns
        -------
        self.storage_existing_df : DataFrame
            DataFrame of existing CO\ :sub:`2` storage infrastructure in a given model year.

        self.storage_new_df : DataFrame
            DataFrame of potential new CO\ :sub:`2` storage infrastructure.

        self.storage_costs_df  : DataFrame
            DataFrame of CO\ :sub:`2` storage costs.
        
        '''
        ### Declarations
        # Declare existing storage
        self.storage_existing_df = self.storage_existing_df.copy()
        self.storage_existing_df['node_type'] = 'storage'


        ### Costs
        # Existing Storage
        self.storage_existing_df['capex_site_dev'] = 1
        self.storage_existing_df['fixom_site_dev'] = 1
        self.storage_existing_df['varom_site_dev'] = 1
        self.storage_existing_df['capex_construction'] = 1
        self.storage_existing_df['fixom_construction'] = 1
        self.storage_existing_df['varom_construction'] = 1
        self.storage_existing_df['capex_injection'] = 1
        self.storage_existing_df['fixom_injection'] = 1

        self.storage_existing_df['capex'] = 1
        self.storage_existing_df['varom'] = self.storage_existing_df['varom_injection']

        # New Storage
        # Aggregate into capex (for investment) and varom (for operation) in the optimization model
        # Note: PISC O&M and closure costs are currently excluded (fixom_pisc, varom_pisc, capex_closure, fixom_closure, and varom_closure)
        self.storage_new_df['capex'] = (
                (self.i_storage_df['capex_site_dev']  + self.parent.ccats_fin.site_dev_years_storage * (self.i_storage_df['fixom_site_dev'] + self.i_storage_df['inj_rate_project_tonnes_yr'] * self.i_storage_df['varom_site_dev']))
              + (self.i_storage_df['capex_construction'] + self.parent.ccats_fin.construction_years_storage * (self.i_storage_df['fixom_construction'] + self.i_storage_df['inj_rate_project_tonnes_yr'] * self.i_storage_df['varom_construction']))
              + (self.i_storage_df['capex_injection'] + (self.parent.ccats_fin.financing_years_storage - self.parent.ccats_fin.site_dev_years_storage - self.parent.ccats_fin.construction_years_storage) * (self.i_storage_df['fixom_injection']))
              + (self.i_storage_df['capex_pisc'] )
                                       )

        self.storage_new_df['varom'] = self.storage_new_df['varom_injection']

        # Filter storage so that only eligible sites are in the dataset
        self.storage_costs_df = self.storage_existing_df.loc[self.storage_existing_df['net_storage_injectivity'] > 0].copy()
        temp_new = self.storage_new_df.loc[~self.storage_new_df['node_id'].isin(self.storage_costs_df['node_id'])]
        self.storage_costs_df = pd.concat([self.storage_costs_df, temp_new], ignore_index=True)

        pass
        

    def prepare_eor_opt(self):
        '''Prepare EOR dataframes for the main CCATS optimization.

        Parameters
        ----------
        None

        Returns
        -------
        self.eor_demand_df : DataFrame
            DataFrame of model CO\ :sub:`2` EOR net cost by play.

        self.eor_cost_net_df : DataFrame
            DataFrame of model year EOR demand net costs.
        
        '''
        ### Declarations
        # Existing EOR
        self.eor_demand_df = self.eor_demand_df[['node_id','census_division','b0_co2_volume','b1_co2_volume','b2_co2_volume','tax_credit_value','tax_credit_value_dac']].copy()
        self.eor_demand_df['node_type'] = 'co2_eor'

        # Remove any empty HSM centroids
        self.eor_demand_df = self.eor_demand_df.loc[(self.eor_demand_df['b0_co2_volume'] +\
                                                         self.eor_demand_df['b1_co2_volume'] +\
                                                     self.eor_demand_df['b2_co2_volume']) > 0]

        pass

    def colocate_dac_with_ts_nodes(self):
        """Create new nodes co-located with transshipment nodes for building direct air capture.

        self.i_pipeline_lookup_df is updated with an arc going to each of the new nodes.
        
        Returns
        -------
        self.i_pipeline_lookup_df

        """
        # offsets - for visualization only
        offset_latitude = 0.1
        offset_longitude = 0.1

        # type of nodes to use
        node_types = ['operational_ts_node', 'uniform_ts_node']
        
        # make a local copy
        pipeline_lookup_df = self.i_pipeline_lookup_df.copy()

        # --------
        ## find ts nodes based on i nodes
        # --------
        # dictionary, columns to get -> new name
        d_cols_i = {'node_i_id':'id', 'node_i_type':'type','i_latitude':'latitude', 'i_longitude':'longitude', 
                    'node_i_census_division':'census_division','node_i_census_region':'census_region',
                    'pipe_segment':'pipe_segment', 'min_throughput_tonnes':'min_throughput_tonnes', 
                    'max_throughput_tonnes':'max_throughput_tonnes'}
        # get values
        i_nodes = pipeline_lookup_df.loc[pipeline_lookup_df.loc[:, 'node_i_type'].isin(node_types), d_cols_i.keys()]
        # rename columns
        i_nodes = i_nodes.rename(columns=d_cols_i)

        # --------
        ## find ts nodes based on j nodes
        # --------
        # dictionary, columns to get -> new name
        d_cols_j = {'node_j_id':'id', 'node_j_type':'type','j_latitude':'latitude', 'j_longitude':'longitude', 
                    'node_j_census_division':'census_division','node_j_census_region':'census_region',
                    'pipe_segment':'pipe_segment', 'min_throughput_tonnes':'min_throughput_tonnes', 
                    'max_throughput_tonnes':'max_throughput_tonnes'}
        # get values
        j_nodes = pipeline_lookup_df.loc[pipeline_lookup_df.loc[:, 'node_j_type'].isin(node_types), d_cols_j.keys()]
        # rename columns
        j_nodes = j_nodes.rename(columns=d_cols_j)
        
        # --------
        ## combine ts nodes based on i and j nodes, keep unique entries
        # --------
        ts_nodes = pd.concat([i_nodes, j_nodes])
        ts_nodes = ts_nodes.drop_duplicates(subset='id',keep='first')

        # --------
        # Create new source arcs
        # --------

        # empty Series to fill in for each new arc, maintain column names
        s_empty = pd.Series(index=pipeline_lookup_df.columns)

        # empty dataframe to store the new arcs
        arcs_source = pd.DataFrame()

        # iterate through each ts node
        for i in ts_nodes.index:
            s = s_empty.copy()
            s['node_i_id']              = 'dac_' + ts_nodes.loc[i, 'id']
            s['node_i_type']            = 'dac'
            s['i_latitude']             = ts_nodes.loc[i, 'latitude'] + offset_latitude
            s['i_longitude']            = ts_nodes.loc[i, 'longitude'] + offset_longitude
            s['node_i_census_division'] = ts_nodes.loc[i, 'census_division']
            s['node_i_census_region']   = ts_nodes.loc[i, 'census_region']

            s['node_j_id']              = ts_nodes.loc[i, 'id']
            s['node_j_type']            = ts_nodes.loc[i, 'type']
            s['j_latitude']             = ts_nodes.loc[i, 'latitude']
            s['j_longitude']            = ts_nodes.loc[i, 'longitude']
            s['node_j_census_division'] = ts_nodes.loc[i, 'census_division']
            s['node_j_census_region']   = ts_nodes.loc[i, 'census_region']

            s['route_id']               = s['node_i_id'] + '_to_' + s['node_j_id']
            s['pipe_segment']           = ts_nodes.loc[i, 'pipe_segment']
            s['pipe_capex_intercept']   = 0.0
            s['pipe_capex_slope']       = 0.0
            s['min_throughput_tonnes']  = float(ts_nodes.loc[i, 'min_throughput_tonnes'])
            s['max_throughput_tonnes']  = float(ts_nodes.loc[i, 'max_throughput_tonnes'])
            s['pipeline_miles']         = 0.0
            s['average_electricity']    = 0.0

            s['pipeline_type'] = 'dac_to_ts_node'
            s['existing_throughput_tonnes'] = 0.0
            s['operational_status'] = 'N'

            # store
            arcs_source = pd.concat([arcs_source, s.to_frame().transpose()])

        # --------
        # Set datatypes
        # --------
        arcs_source = arcs_source.astype({'i_latitude':'float64',
                                          'i_longitude':'float64',
                                          'j_latitude':'float64',
                                          'j_longitude':'float64',
                                          'min_throughput_tonnes':'float64',
                                          'max_throughput_tonnes':'float64',
                                          'pipeline_miles':'float64',
                                          'average_electricity':'float64',
                                          'existing_throughput_tonnes':'float64',
                                          'node_i_census_division':'int',
                                          'node_i_census_region':'int',
                                          'node_j_census_division':'int',
                                          'node_j_census_region':'int'})

        # --------
        # Save nodes and arcs by appending to i_pipeline_lookup_df
        # --------
        self.i_pipeline_lookup_df = pd.concat([self.i_pipeline_lookup_df, arcs_source])

        pass

    
    def prepare_endogenous_sources(self):
        """Prepare enodgenous sources of CO\ :sub:`2`.

        Returns
        -------
        self.source_annual_capacity_existing : Series
            Existing source annual capacity, updated for last year's run, indexed by kind of source and node (k,n).
            
        self.parent.source_total_capacity_existing : Series
            Existing source total capacity, updated for last year's run, indexed by kind of source and node (k,n).
        
        self.source_kinds : list
            List of kinds of sources.

        self.cost_invest_source : DataFrame
            Source investment costs, indexed by kind of source and node (k,n).

        self.cost_variable_source : DataFrame
            Source variable costs, indexed by kind of source and node (k,n).
        
        self.build_limit_source_node : Series
            Source node build limit, indexed by kind of source (k).
        
        self.build_limit_source_total : Series
            Source nationwide build limit, indexed by kind of source (k).

        """
        #----------------------------
        ## Get nodes and kinds, used for indexing
        #----------------------------

        # get source nodes from self.pipeline_lookup_df
        source_df = self.pipeline_lookup_df.loc[self.pipeline_lookup_df['node_i_type'].isin(['dac'])].copy()
        nodes_source = source_df['node_i_id'].tolist()
        node_prices = source_df[['node_i_id','node_i_census_division']].groupby(['node_i_id']).first().copy()

        # get kinds of sources
        kinds_source = self.i_source_data.kind.tolist()

        # get vintages of sources
        if int(self.year_current) == int(self.parent.year_start):
            vintages_source   = self.i_source_annual_capacity_existing.copy().reset_index().vintage.unique().tolist()
        else: 
            vintages_source   = self.source_annual_capacity_existing.copy().reset_index().vintage.unique().tolist()

        #----------------------------
        ## Existing Capacity
        #----------------------------
        # Only include if node is in nodes_source
        if int(self.year_current) == int(self.parent.year_start):
            self.source_annual_capacity_existing   = self.i_source_annual_capacity_existing.copy() # MultiIndex - Indexed by kind of source, node and vintage (k,n,v)
            self.source_total_capacity_existing   = self.i_source_total_capacity_existing.copy() # MultiIndex - Indexed by kind of source, node and vintage (k,n,v)

        else: 
            pass


        #----------------------------
        # Energy prices
        #----------------------------
        
        ## create dataframe with prices for the current year

        # local copies of restart file variables
        prices_natgas = self.parent.restart.mpblk_pngin.copy()
        prices_elec = self.parent.restart.mpblk_pelin.copy()

        # store prices for each census division
        prices = pd.DataFrame()
        for i in range(1,10):

            # store values of interest in a Series
            s = pd.Series()
            s['division']    = int(i)
            s['natgas']      = prices_elec.loc[i, int(self.parent.year_current)].value
            s['electricity'] = prices_natgas.loc[i, int(self.parent.year_current)].value
            
            # store Series in the prices DataFrame
            if len(prices)==0:
                prices = s.to_frame().transpose()
            else:
                prices = pd.concat([prices, s.to_frame().transpose()])

        prices['division'] = prices['division'].astype('int')
        prices = prices.set_index(['division'])

        ## store prices for each node

        # default as nationwide average
        node_prices.loc[:,'natgas']      = prices.natgas.mean()
        node_prices.loc[:,'electricity'] = prices.electricity.mean()

        # store prices for each census division
        for i in range(1,10):

            # get indicies of nodes for this division
            ind = node_prices.node_i_census_division == i

            # store prices
            node_prices.loc[ind,'natgas']      = prices.loc[i,'natgas']
            node_prices.loc[ind,'electricity'] = prices.loc[i,'electricity']

        # node_prices = node_prices.set_index('node_i_id')

        #----------------------------
        ## Costs
        #----------------------------
        # adjust LCFS credit for inflation
        self.LCFS_credit = self.parent.LCFS_credit * com.calculate_inflation(self.parent.rest_mc_jpgdp.copy(), self.parent.LCFS_dollar_year)

        # make a copy of data to work with
        source_data = self.i_source_data.copy().set_index('kind')
        
        # create a dataframe to store results
        source_costs = pd.DataFrame()

        # iterate through each source kind
        for k in kinds_source:
            
            # access data regarding this kind
            CAPEX_USD_t     = source_data.loc[k,'CAPEX_USD_t']
            VOM_USD_t       = source_data.loc[k,'VOM_USD_t']
            natgas_GJ_t     = source_data.loc[k,'natgas_GJ_t']
            elec_GJ_t       = source_data.loc[k,'elec_GJ_t']
            cost_multiplier = source_data.loc[k,'cost_multiplier']
            LCFS_eligible   = source_data.loc[k,'LCFS_eligible']=='Y'
            
            # iterate through each node
            for n in node_prices.index:

                # get node specific energy costs (convert from $/MMBtu to specified units)
                MMBtu_to_GJ = 1.055056
                natgas_USD_GJ = node_prices.loc[n,'natgas'] / MMBtu_to_GJ
                elec_USD_GJ  = node_prices.loc[n,'electricity'] / MMBtu_to_GJ

                # LCFS
                VOM_USD_t2 = VOM_USD_t
                if LCFS_eligible:
                    VOM_USD_t2 = VOM_USD_t - self.LCFS_credit

                # store in a Series
                s = pd.Series()
                s['k'] = k
                s['n'] = n
                s['cost_invest'] = cost_multiplier * (CAPEX_USD_t)
                s['cost_variable'] = cost_multiplier * (VOM_USD_t2 + natgas_USD_GJ * natgas_GJ_t + elec_USD_GJ * elec_GJ_t)
                # append
                source_costs = pd.concat([source_costs, s.to_frame().transpose()])

        # ------------------
        # policy eligibility  - Existing capacity
        # ------------------
        block_start = np.array([0, self.parent.ccats_fin.duration_b0, self.parent.ccats_fin.duration_b0 + self.parent.ccats_fin.duration_b1])
        for b in range(len(block_start)):
            eligibility = pd.DataFrame()
            for k in kinds_source:
                for p in [0, 1, 2]:
                    for v in vintages_source:
                        s = pd.Series()
                        s['kind'] = k
                        s['policy'] = p
                        s['vintage'] = v

                        # Determine whether this kind of source counts as 1) DAC, 2) CCS, or 3) neither
                        is_DAC = source_data.loc[k,'45Q-DAC']=='Y'
                        is_CCS = source_data.loc[k,'45Q-CCS']=='Y'
                        if is_DAC and is_CCS:
                            is_CCS = False

                        # Determine eligibily
                        if p==0: # NTC (Always an option)
                            s['eligibility'] = 1 # Elgibile

                        elif p==1: # 45Q - CCS (Does not apply to DAC)
                            
                            if (is_CCS) and (int(self.year_current) + block_start[b] < v + self.parent.ccats_fin.duration_45q):
                                s['eligibility'] = 1 # Eligible
                            else:
                                s['eligibility'] = 0 # Inelgibile

                        elif p==2: #45Q - DAC (Vintage dependent)

                            if (is_DAC) and (int(self.year_current) + block_start[b] < v + self.parent.ccats_fin.duration_45q):
                                s['eligibility'] = 1 # Eligible
                            else:
                                s['eligibility'] = 0 # Inelgibile

                        # store
                        eligibility = pd.concat([eligibility, s.to_frame().transpose()])
            
            # save
            if b==0:
                policy_eligibility_source_exist_b0 = eligibility.set_index(['kind','policy','vintage'])
            elif b==1:
                policy_eligibility_source_exist_b1 = eligibility.set_index(['kind','policy','vintage'])
            elif b==2:
                policy_eligibility_source_exist_b2 = eligibility.set_index(['kind','policy','vintage'])

        # ------------------
        # policy eligibility  - New builds
        # ------------------
        block_start = np.array([0, self.parent.ccats_fin.duration_b0, self.parent.ccats_fin.duration_b0 + self.parent.ccats_fin.duration_b1])
        for b in range(len(block_start)):
            eligibility = pd.DataFrame()
            for k in kinds_source:
                for p in [0, 1, 2]:
                    s = pd.Series()
                    s['kind'] = k
                    s['policy'] = p

                    # Determine whether this kind of source counts as 1) DAC, 2) CCS, or 3) neither
                    is_DAC = source_data.loc[k,'45Q-DAC']=='Y'
                    is_CCS = source_data.loc[k,'45Q-CCS']=='Y'
                    if is_DAC and is_CCS:
                        is_CCS = False

                    # Determine eligibily
                    if p==0: # NTC (Always an option)
                        s['eligibility'] = 1 # Elgibile

                    elif p==1: # 45Q - CCS (Does not apply to DAC)
                        if (is_CCS) and (int(self.year_current) + block_start[b] <= self.parent.year_45q_last_new):
                            s['eligibility'] = 1 # Eligible
                        else:
                            s['eligibility'] = 0 # Inelgibile

                    elif p==2: #45Q - DAC (Vintage dependent)

                        if (is_DAC) and (int(self.year_current) + block_start[b] <= self.parent.year_45q_last_new):
                            s['eligibility'] = 1 # Eligible
                        else:
                            s['eligibility'] = 0 # Inelgibile

                    # store
                    eligibility = pd.concat([eligibility, s.to_frame().transpose()])
            # save
            if b==0:
                policy_eligibility_source_new_b0 = eligibility.set_index(['kind','policy'])
            elif b==1:
                policy_eligibility_source_new_b1 = eligibility.set_index(['kind','policy'])
            elif b==2:
                policy_eligibility_source_new_b2 = eligibility.set_index(['kind','policy'])

        #----------------------------
        ### Optimization inputs
        #----------------------------
        # only populate if there are source nodes
        if len(nodes_source)>0:
            # set-up multi-index
            source_costs = source_costs.set_index(['k','n'])

            ## Sets
            self.source_kinds = kinds_source # List
            self.vintages_source = vintages_source # List

            ## Parameters
            # policy eligibility - existing capacity: Multiindex - Indexed by kind of source, policy and vintage (k,p,v)
            self.policy_eligibility_source_exist_b0  = policy_eligibility_source_exist_b0['eligibility'].astype('int') # block 0
            self.policy_eligibility_source_exist_b1  = policy_eligibility_source_exist_b1['eligibility'].astype('int') # block 1
            self.policy_eligibility_source_exist_b2  = policy_eligibility_source_exist_b2['eligibility'].astype('int') # block 2

            # policy eligibility - new capacity: Multiindex - Indexed by kind of source and policy and vintage (k,p)
            self.policy_eligibility_source_new_b0  = policy_eligibility_source_new_b0['eligibility'].astype('int') # block 0
            self.policy_eligibility_source_new_b1  = policy_eligibility_source_new_b1['eligibility'].astype('int') # block 1
            self.policy_eligibility_source_new_b2  = policy_eligibility_source_new_b2['eligibility'].astype('int') # block 2

            # investment costs (MultiIndex - Indexed by kind of source and node (k,n))
            self.cost_invest_source       = source_costs.copy()['cost_invest'].astype('float')

            # variable costs (MultiIndex - Indexed by kind of source and node (k,n))
            self.cost_variable_source     = source_costs.copy()['cost_variable'].astype('float')
            
            ## build limits (dictionaries - Indexed by kind of source)
            # make a copy of data to work with
            source_data = self.i_source_data.copy().set_index('kind')

            # iterate through each source kind
            self.build_limit_source_node = pd.Series()
            self.build_limit_source_total = pd.Series()
            for k in kinds_source:
                # access data regarding this kind
                init_build_limit_node   = source_data.loc[k,'init_build_limit_node']
                rate_build_limit_node   = source_data.loc[k,'rate_build_limit_node']
                init_build_limit_total = source_data.loc[k,'init_build_limit_total']
                rate_build_limit_total = source_data.loc[k,'rate_build_limit_total']

                # store result
                self.build_limit_source_node[k]  = init_build_limit_node  * (1.0 + rate_build_limit_node  * (int(self.year_current) - int(self.parent.year_start)))
                self.build_limit_source_total[k] = init_build_limit_total * (1.0 + rate_build_limit_total * (int(self.year_current) - int(self.parent.year_start)))

            self.build_limit_source_node_cumulative = self.i_source_capacity_limits.copy()

        pass


    def prepare_natural_sources(self):
        """Prepare natural sources of CO\ :sub:`2`.

        Returns
        -------
        self.parent.source_annual_capacity_existing : Series
            Existing source capacity, updated for last year's run, indexed by kind of source and node (k,n).
            
        self.parent.total_capacity_existing : Series
            Existing source capacity, updated for last year's run, indexed by kind of source and node (k,n).

        self.source_kinds : list
            List of kinds of sources.

        self.cost_invest_source : DataFrame
            Source investment costs, indexed by kind of source and node (k,n).

        self.cost_variable_source : DataFrame
            Source variable costs, indexed by kind of source and node (k,n).

        self.build_limit_source_node : Series
            Source node build limit, indexed by kind of source (k).

        self.build_limit_source_total : Series
            Source nationwide build limit, indexed by kind of source (k).

        """
        ### Source Kinds
        self.source_kinds = self.source_kinds + ['nat_co2']


        ### Create temp_nat_co2 dataframe containing natural CO2 site input parameters
        temp_nat_co2 = self.nat_co2_lookup_df.copy()
        temp_nat_co2['kind'] = 'nat_co2'
        temp_nat_co2['vintage'] = int(self.parent.year_start)
        temp_nat_co2 = temp_nat_co2.set_index(['kind','node_id','vintage'],append = False)


        ### Parameter inputs
        # Policy eligibility
        temp_nat_co2['eligibility'] = 0

        # Investment costs
        temp_nat_co2['cost_invest'] = 0

        # Variable costs
        # $15 is a rough estimate based on https://www.netl.doe.gov/sites/default/files/netl-file/co2_eor_primer.pdf, waiting on CREAM study
        temp_nat_co2['cost_variable'] = 15
        temp_nat_co2['cost_variable'] = temp_nat_co2['cost_variable'] * com.calculate_inflation(self.parent.rest_mc_jpgdp.copy(), 2010)

        # Build Limit
        temp_nat_co2['capacity'] = temp_nat_co2['gross_err_mt'].sum()


        ### Concatenate natural CO2 parameters to main source parameters series
        # Only include if node is in nodes_source
        if int(self.year_current) == int(self.parent.year_start):
            self.source_annual_capacity_existing = pd.concat([self.source_annual_capacity_existing,temp_nat_co2['max_co2_prod']])  # MultiIndex - Indexed by kind of source, node and vintage (k,n,v)
            self.source_total_capacity_existing = pd.concat([self.source_total_capacity_existing,temp_nat_co2['net_err_mt_current']])
        else:
            self.source_total_capacity_existing.update(temp_nat_co2['net_err_mt_current'])


        ### Optimization parameter inputs
        # only populate if there are source nodes

        ### Optimization parameters
        # Policy eligibility - existing capacity: Multiindex - Indexed by kind of source, policy and vintage (k,p,v)
        temp_policy = pd.DataFrame()
        for p in [0, 1, 2]:
            temp = temp_nat_co2.copy().droplevel(level=1)
            temp['policy'] = p
            temp = temp.set_index(['policy'], append = True)
            temp.index = temp.index.reorder_levels([0,2,1])
            if p == 0: # NTC - always available
                temp['eligibility'] = 1
            temp_policy = pd.concat([temp_policy,temp])
        temp_policy = temp_policy[['eligibility']]
        temp_policy = temp_policy[~temp_policy.index.duplicated(keep='first')]

        self.policy_eligibility_source_exist_b0 = pd.concat([self.policy_eligibility_source_exist_b0, temp_policy['eligibility'].astype('int')]) # block 0
        self.policy_eligibility_source_exist_b1 = pd.concat([self.policy_eligibility_source_exist_b1, temp_policy['eligibility'].astype('int')]) # block 1
        self.policy_eligibility_source_exist_b2 = pd.concat([self.policy_eligibility_source_exist_b2, temp_policy['eligibility'].astype('int')]) # block 2

        # Policy eligibility - new capacity: Multiindex - Indexed by kind of source and policy and vintage (k,p)
        self.policy_eligibility_source_new_b0 = pd.concat([self.policy_eligibility_source_new_b0, temp_policy['eligibility'].astype('int')]) # block 0
        self.policy_eligibility_source_new_b1 = pd.concat([self.policy_eligibility_source_new_b1, temp_policy['eligibility'].astype('int')]) # block 1
        self.policy_eligibility_source_new_b2 = pd.concat([self.policy_eligibility_source_new_b2, temp_policy['eligibility'].astype('int')]) # block 2

        # Costs (MultiIndex - Indexed by kind of source and node (k,n))
        temp_cost = temp_nat_co2.droplevel(level=2).copy()

        self.cost_invest_source = pd.concat([self.cost_invest_source,temp_cost.copy()['cost_invest'].astype('float')])
        self.cost_variable_source = pd.concat([self.cost_variable_source,temp_cost.copy()['cost_variable'].astype('float')])

        # Build Limits
        temp_build_limits = temp_nat_co2.droplevel(level=2).copy()

        self.build_limit_source_node.loc['nat_co2'] = 0
        self.build_limit_source_total.loc['nat_co2'] = 0
        self.build_limit_source_node_cumulative = pd.concat([self.build_limit_source_node_cumulative, temp_build_limits[['capacity']].astype('float')])

        pass

    def setup_optimization(self):
        '''Format and prepare data in optimization DataFrames for optimization.

        Parameters
        ----------
        None

        Returns
        -------
        self.co2_supply_b0_opt : DataFrame
            Optimization DataFrame of time period 0 CO\ :sub:`2` supply sources and volumes for the main optimization.

        self.co2_supply_b1_opt : DataFrame
            Optimization DataFrame of time period 1 CO\ :sub:`2` supply sources and volumes for the main optimization.

        self.co2_supply_b2_opt : DataFrame
            Optimization DataFrame of time period 2 CO\ :sub:`2` supply sources and volumes for the main optimization.

        self.storage_existing_opt : DataFrame
            Optimization DataFrame of existing CO\ :sub:`2` storage infrastructure in a given model year.

        self.storage_new_opt : DataFrame
            Optimization DataFrame of potential new CO\ :sub:`2` storage infrastructure.

        self.storage_costs_opt  : DataFrame
            Optimization DataFrame of CO\ :sub:`2` storage costs.

        self.eor_demand_opt : DataFrame
            Optimization DataFrame of model year EOR demand.

        self.eor_cost_net_opt : DataFrame
            Optimization DataFrame of model year EOR demand net costs.

        self.pipeline_lookup_opt : DataFrame
            Optimization DataFrame of pipeline network options for optimization.

        self.pipes_existing_opt : DataFrame
            Optimization DataFrame of existing pipeline infrastructure for optimization.
        
        '''
        ### Produce Supply Optimization dfs
        self.co2_supply_b0_opt = self.co2_supply_b0_df.copy()
        self.co2_supply_b0_opt = self.co2_supply_b0_opt.set_index(['node_id','eligibility_45q'], drop=False).sort_index()

        self.co2_supply_b1_opt = self.co2_supply_b1_df.copy()
        self.co2_supply_b1_opt = self.co2_supply_b1_opt.set_index(['node_id','eligibility_45q'], drop=False).sort_index()

        self.co2_supply_b2_opt = self.co2_supply_b2_df.copy()
        self.co2_supply_b2_opt = self.co2_supply_b2_opt.set_index(['node_id','eligibility_45q'], drop=False).sort_index()


        ### Produce Storage/EOR optimization dfs
        self.storage_existing_opt   = self.storage_existing_df.copy()
        self.storage_existing_opt   = self.storage_existing_opt.set_index(['node_id'], drop=False).sort_index()

        self.storage_new_opt        = self.storage_new_df.copy()
        self.storage_new_opt        = self.storage_new_opt.set_index(['node_id'], drop=False).sort_index()

        self.eor_demand_opt         = self.eor_demand_df.copy()
        self.eor_demand_opt         = self.eor_demand_opt.set_index(['node_id'], drop=False).sort_index()

        self.storage_costs_opt      = self.storage_costs_df.copy()
        self.storage_costs_opt      = self.storage_costs_opt.set_index(['node_id'], drop=False).sort_index()

        self.eor_cost_net_opt       = self.eor_cost_net_df.copy()
        self.eor_cost_net_opt       = self.eor_cost_net_opt.set_index(['node_id'], drop=False).sort_index()


        ### Instantiate OPT versions of other dfs
        # Potential new pipelines
        self.pipeline_lookup_opt    = self.pipeline_lookup_df.copy()
        self.pipeline_lookup_opt    = self.pipeline_lookup_opt .rename(columns = {'max_throughput_tonnes':'thruput_tonnes'})
        self.pipeline_lookup_opt    = self.pipeline_lookup_opt.set_index(['node_i_id', 'node_j_id','pipe_segment'], drop=False).sort_index()

        # Existing pipelines
        # Remove any existing pipelines not required for the solve
        self.pipes_existing_opt     = self.pipes_existing_df.copy()
        self.pipes_existing_opt     = self.pipes_existing_opt.loc[self.pipes_existing_opt['route_id'].isin(self.pipeline_lookup_df['route_id'])]
        self.pipes_existing_opt     = self.pipes_existing_opt.set_index(['node_i_id', 'node_j_id'], drop=False).sort_index()

        pass


    def instantiate_pyomo_series(self):
        '''Declare pyomo optimization inputs as Series.

        Parameters
        ----------
        None

        Returns
        -------
        self.nodes_supply : Series
            Supply nodes.

        self.nodes_trans_ship : Series
            Trans-shipment (TS) nodes.

        self.nodes_sequester : Series
            Sequestration nodes.

        self.nodes_demand : Series
            EOR Demand nodes.

        self.nodes_storage : Series
            Storage nodes.

        self.nodes : Series
            All optimization nodes.

        self.arcs : Series
            All optimization arcs.

        self.policy_cost : Series
            CO\ :sub:`2` policy cost.

        self.eligibility_45q : Series
            Arc eligibility for 45Q.

        self.transport_existing : Series
            Transportation capacity that does not require investment decision (metric tonnes).

        self.transport_add_min : Series
            Transport capacity (lower bound) that requires an investment decision (metric tonnes).

        self.transport_add : Series
            Transport capacity (upper bound) that requires an investment decision (metric tonnes).

        self.pipeline_lookup_opt_b0 : Series
            Transport pipeline lookup for net-new pipeline builds, block 0.

        self.pipeline_lookup_opt_b1 : Series
            Transport pipeline lookup for net-new pipeline builds, block 1.

        self.pipeline_lookup_opt_b2 : Series
            Transport pipeline lookup for net-new pipeline builds, block 2.

        self.ts_multiplier_b0 : Series
            Multiplier for TS arcs to represent added complexity of a regional/national pipeline network, block 0.

        self.ts_multiplier_b1 : Series
            Multiplier for TS arcs to represent added complexity of a regional/national pipeline network, block 1.

        self.ts_multiplier_b2 : Series
            Multiplier for TS arcs to represent added complexity of a regional/national pipeline network, block 2.

        self.cost_reduce_fr_transport_b0 : Series
            Technology improvement rate for transport costs, block 0.

        self.cost_reduce_fr_transport_b1 : Series
            Technology improvement rate for transport costs, block 1.

        self.cost_reduce_fr_transport_b2 : Series
            Technology improvement rate for transport costs, block 2.

        self.capex_transport_base_b0 : Series
            Base capital cost of transportation investment decision used in capex piecewise (1987$), block 0.

        self.capex_transport_base_b1 : Series
            Base capital cost of transportation investment decision used in capex piecewise (1987$), block 1.

        self.capex_transport_base_b2 : Series
            Base capital cost of transportation investment decision used in capex piecewise (1987$), block 2.

        self.capex_transport_slope_b0 : Series
            Capital cost slope of transportation investment decision used in capex piecewise (1987$), block 0.

        self.capex_transport_slope_b1 : Series
            Capital cost slope of transportation investment decision used in capex piecewise (1987$), block 1.

        self.capex_transport_slope_b2 : Series
            Capital cost slope of transportation investment decision used in capex piecewise (1987$), block 2.

        self.electricity_demand : Series
            Electricity demand from pumps (MWh).

        self.opex_transport_elec_b0 : Series
            Operating cost for electricity ($/MWh), block 0.

        self.opex_transport_elec_b1 : Series
            Operating cost for electricity ($/MWh), block 1.

        self.opex_transport_elec_b2 : Series
            Operating cost for electricity ($/MWh), block 2.

        self.cost_reduce_fr_storage_b0 : Series
            Technology improvement rate for storage costs, block 0.

        self.cost_reduce_fr_storage_b1 : Series
            Technology improvement rate for storage costs, block 1.

        self.cost_reduce_fr_storage_b2 : Series
            Technology improvement rate for storage costs, block 2.

        self.storage_capex_b0 : Series
            Capital and Fixed O&M costs ($/tonne) for saline formation storage, block 0.

        self.storage_capex_b1 : Series
            Capital and Fixed O&M costs ($/tonne) for saline formation storage, block 1.

        self.storage_capex_b2 : Series
            Capital and Fixed O&M costs ($/tonne) for saline formation storage, block 2.

        self.storage_varom_b0 : Series
            Variable O&M costs ($/tonne) for saline formation storage, block 0.

        self.storage_varom_b1 : Series
            Variable O&M costs ($/tonne) for saline formation storage, block 1.

        self.storage_varom_b2 : Series
            Variable O&M costs ($/tonne) for saline formation storage, block 2.

        self.co2_supply_b0 : Series
            CO\ :sub:`2` supply sources and volumes for the main optimization (metric tonnes), block 0.

        self.co2_supply_b1 : Series
            CO\ :sub:`2` supply sources and volumes for the main optimization (metric tonnes), block 1.

        self.co2_supply_b2 : Series
            CO\ :sub:`2` supply sources and volumes for the main optimization (metric tonnes), block 2.

        self.co2_demand_b0 : Series
            CO\ :sub:`2` demand for the main optimization (metric tonnes), block 0.

        self.co2_demand_b1 : Series
            CO\ :sub:`2` demand for the main optimization (metric tonnes), block 1.

        self.co2_demand_b2 : Series
            CO\ :sub:`2` demand for the main optimization (metric tonnes), block 2.

        self.co2_demand_cost_b0 : Series
            CO\ :sub:`2` demand net cost for the main optimization (1987$/tonne), block 0.

        self.co2_demand_cost_b1 : Series
            CO\ :sub:`2` demand net cost for the main optimization (1987$/tonne), block 1.

        self.co2_demand_cost_b2 : Series
            CO\ :sub:`2` demand net cost for the main optimization (1987$/tonne), block 2.

        self.co2_injectivity_existing_b0 : Series
            Existing injectivity for the main optimization (metric tonnes), block 0.

        self.co2_injectivity_existing_b1 : Series
            Existing injectivity for the main optimization (metric tonnes), block 1.

        self.co2_injectivity_existing_b2 : Series
            Existing injectivity for the main optimization (metric tonnes), block 2.

        self.co2_injectivity_new_b0 : Series
            Potential new injectivity for the main optimization (metric tonnes), block 0.

        self.co2_injectivity_new_b1 : Series
            Potential new injectivity for the main optimization (metric tonnes), block 1.

        self.co2_injectivity_new_b2 : Series
            Potential new injectivity for the main optimization (metric tonnes), block 2.

        self.co2_max_injectivity_b0 : Series
            Max injectivity for the main optimization (metric tonnes), block 0.

        self.co2_max_injectivity_b1 : Series
            Max injectivity for the main optimization (metric tonnes), block 1.

        self.co2_max_injectivity_b2 : Series
            Max injectivity for the main optimization (metric tonnes), block 2.

        self.co2_store_net_existing_b0 : Series
            Existing CO\ :sub:`2` storage capacity (metric tonnes), block 0.

        self.co2_store_net_existing_b1 : Series
            Existing CO\ :sub:`2` storage capacity (metric tonnes), block 1.

        self.co2_store_net_existing_b2 : Series
            Existing CO\ :sub:`2` storage capacity (metric tonnes), block 2.

        self.co2_store_net_adder_b0 : Series
            Potential new CO\ :sub:`2` storage capacity (metric tonnes), block 0.

        self.co2_store_net_adder_b1 : Series
            Potential new CO\ :sub:`2` storage capacity (metric tonnes), block 1.

        self.co2_store_net_adder_b2 : Series
            Potential new CO\ :sub:`2` storage capacity (metric tonnes), block 2.

        self.storage_aors_available_b0 : Series
            Remaining storage areas of review available, block 0.

        self.storage_aors_available_b1 : Series
            Remaining storage areas of review available, block 1.

        self.storage_aors_available_b2 : Series
           Remaining storage areas of review available, block 2.

        self.duration_b0 : int
            Time period 0 duration (years).

        self.duration_b1 : int
            Time period 1 duration (years).

        self.duration_b2 : int
            Time period 2 duration (years).

        self.duration_45q : int
            Duration of 45Q tax credit eligibility (years).

        self.discount_invest_storage_b0  : Series
            Discount rate for investment in storage, block 0.

        self.discount_invest_storage_b1  : Series
            Discount rate for investment in storage, block 1.

        self.discount_invest_storage_b2  : Series
            Discount rate for investment in storage, block 2.

        self.discount_invest_transport_b0 : Series
            Discount rate for investment in transportation, block 0.

        self.discount_invest_transport_b1 : Series
            Discount rate for investment in transportation, block 1.

        self.discount_invest_transport_b2 : Series
            Discount rate for investment in transportation, block 2.

        self.discount_variable_b0 : Series
            Discount rate for variables, block 0.

        self.discount_variable_b1 : Series
            Discount rate for variables, block 1.

        self.discount_variable_b2 : Series
            Discount rate for variables, block 2.

        self.discount_policy_b0 : Series
            Discount rate for policy, block 0.

        self.discount_policy_b1 : Series
            Discount rate for policy, block 1.

        self.discount_policy_b2 : Series
            Discount rate for policy, block 2.
        
        '''
        ### Nodes

        # Nodes source
        source_df = self.pipeline_lookup_opt.loc[self.pipeline_lookup_opt['node_i_type'].isin(['dac','nat_co2_field'])].copy()
        self.nodes_source = set(source_df['node_i_id'].tolist())

        # Nodes_supply
        supply_df = self.pipeline_lookup_opt.loc[self.pipeline_lookup_opt['node_i_type'].isin(['other_existing','ethanol','ammonia','ng_processing','cement','pp_coal','pp_natgas','beccs'])].copy()
        self.nodes_supply = set(supply_df['node_i_id'].tolist())

        # Nodes trans_ship
        trans_ship1_df = self.pipeline_lookup_opt.loc[self.pipeline_lookup_opt['node_i_type'].isin(['uniform_ts_node','operational_ts_node'])].copy()
        trans_ship2_df = self.pipeline_lookup_opt.loc[self.pipeline_lookup_opt['node_j_type'].isin(['uniform_ts_node','operational_ts_node'])].copy()
        self.nodes_trans_ship = set(trans_ship1_df['node_i_id'].tolist() + trans_ship2_df['node_j_id'].tolist())

        # Nodes sequester
        demand_df = self.pipeline_lookup_opt.loc[self.pipeline_lookup_opt['node_j_type'].isin(['co2_eor','hsm_centroid','storage'])].copy()
        self.nodes_sequester = set(demand_df['node_j_id'].tolist())

        # Nodes demand
        demand_df = self.pipeline_lookup_opt.loc[self.pipeline_lookup_opt['node_j_type'].isin(['co2_eor','hsm_centroid'])].copy()
        self.nodes_demand = set(demand_df['node_j_id'].tolist())

        # Nodes storage
        demand_df = self.pipeline_lookup_opt.loc[self.pipeline_lookup_opt['node_j_type'].isin(['storage'])].copy()
        self.nodes_storage = set(demand_df['node_j_id'].tolist())

        # All nodes
        self.nodes = set(self.pipeline_lookup_opt['node_i_id'].tolist() + self.pipeline_lookup_opt['node_j_id'].tolist())


        ### Arcs
        # All arcs
        self.arcs = set(list(self.pipeline_lookup_opt[['node_i_id','node_j_id']].itertuples(index=False, name = None)))


        ### Parameters
        #### Policy Incentives
        
        # Get tax credit array
        tax_credit_45q_df  = pd.concat([self.storage_existing_opt[['tax_credit_value']], self.eor_demand_opt[['tax_credit_value']]])
        tax_credit_45q_df['eligibility_45q'] = 1

        # Get ineligible array
        credit_ineligible_df = tax_credit_45q_df.copy()
        credit_ineligible_df['tax_credit_value'] = 0.0
        credit_ineligible_df['eligibility_45q'] = 0

        if self.parent.endogenous_source_switch:
            # Get tax credit array for DAC
            tax_credit_45q_dac_df = pd.concat([self.storage_existing_opt[['tax_credit_value_dac']], self.eor_demand_opt[['tax_credit_value_dac']]])
            tax_credit_45q_dac_df['tax_credit_value'] = tax_credit_45q_dac_df['tax_credit_value_dac']
            tax_credit_45q_dac_df['eligibility_45q'] = 2
            tax_credit_45q_dac_df = tax_credit_45q_dac_df.drop(columns=['tax_credit_value_dac'])

        # Combine dfs and declare index
        if self.parent.endogenous_source_switch:
            self.policy_cost = pd.concat([tax_credit_45q_df, credit_ineligible_df, tax_credit_45q_dac_df])
        else:
            self.policy_cost = pd.concat([tax_credit_45q_df, credit_ineligible_df])
        self.policy_cost = self.policy_cost.set_index('eligibility_45q', append = True)

        # Declare optimization variable
        self.policy_cost  = self.policy_cost['tax_credit_value']

        ### Transport capacity
        self.transport_existing = self.pipes_existing_opt['thruput_tonnes']
        self.transport_add_min  = self.pipeline_lookup_opt['min_throughput_tonnes']  # minimum throughput per segment
        self.transport_add      = self.pipeline_lookup_opt['thruput_tonnes']  # maximum throughput per segment

        ### Transport costs by block 
        # create copies for each block
        self.pipeline_lookup_opt_b0 = self.pipeline_lookup_opt.copy()
        self.pipeline_lookup_opt_b1 = self.pipeline_lookup_opt.copy()
        self.pipeline_lookup_opt_b2 = self.pipeline_lookup_opt.copy()
        
        # find ts multiplier per block
        # block 0
        self.ts_multiplier_b0 = self.i_ts_multiplier_df.loc[self.parent.year_current,'ts_multiplier']
        # block 1
        if self.parent.year_current + 1 <= self.parent.year_final:
            self.ts_multiplier_b1 = self.i_ts_multiplier_df.loc[self.parent.year_current+1,'ts_multiplier']
        else:
            self.ts_multiplier_b1 = self.ts_multiplier_b0
        # block 2
        if self.parent.year_current + 2 <= self.parent.year_final:
            self.ts_multiplier_b2 = self.i_ts_multiplier_df.loc[self.parent.year_current+2,'ts_multiplier']
        else:
            self.ts_multiplier_b2 = self.ts_multiplier_b1

        # apply ts multipliers
        types_to_apply_multiplier = ['ts_node_to_ts_node']
        for pipeline_type in types_to_apply_multiplier:
            self.pipeline_lookup_opt_b0.loc[self.pipeline_lookup_opt_b0.loc[:,'pipeline_type']==pipeline_type, 'pipe_capex_intercept'] = self.pipeline_lookup_opt_b0.loc[self.pipeline_lookup_opt_b0.loc[:,'pipeline_type']==pipeline_type, 'pipe_capex_intercept'] * self.ts_multiplier_b0
            self.pipeline_lookup_opt_b1.loc[self.pipeline_lookup_opt_b1.loc[:,'pipeline_type']==pipeline_type, 'pipe_capex_intercept'] = self.pipeline_lookup_opt_b1.loc[self.pipeline_lookup_opt_b1.loc[:,'pipeline_type']==pipeline_type, 'pipe_capex_intercept'] * self.ts_multiplier_b1
            self.pipeline_lookup_opt_b2.loc[self.pipeline_lookup_opt_b2.loc[:,'pipeline_type']==pipeline_type, 'pipe_capex_intercept'] = self.pipeline_lookup_opt_b2.loc[self.pipeline_lookup_opt_b2.loc[:,'pipeline_type']==pipeline_type, 'pipe_capex_intercept'] * self.ts_multiplier_b2

            self.pipeline_lookup_opt_b0.loc[self.pipeline_lookup_opt_b0.loc[:,'pipeline_type']==pipeline_type, 'pipe_capex_slope'] = self.pipeline_lookup_opt_b0.loc[self.pipeline_lookup_opt_b0.loc[:,'pipeline_type']==pipeline_type, 'pipe_capex_slope'] * self.ts_multiplier_b0
            self.pipeline_lookup_opt_b1.loc[self.pipeline_lookup_opt_b1.loc[:,'pipeline_type']==pipeline_type, 'pipe_capex_slope'] = self.pipeline_lookup_opt_b1.loc[self.pipeline_lookup_opt_b1.loc[:,'pipeline_type']==pipeline_type, 'pipe_capex_slope'] * self.ts_multiplier_b1
            self.pipeline_lookup_opt_b2.loc[self.pipeline_lookup_opt_b2.loc[:,'pipeline_type']==pipeline_type, 'pipe_capex_slope'] = self.pipeline_lookup_opt_b2.loc[self.pipeline_lookup_opt_b2.loc[:,'pipeline_type']==pipeline_type, 'pipe_capex_slope'] * self.ts_multiplier_b2
        
        # apply technologic learning to capex and varom
        self.cost_reduce_fr_transport_b0 = (1.0 - self.tech_learn_transport)**(self.parent.year_current - self.parent.year_start)
        self.cost_reduce_fr_transport_b1 = (1.0 - self.tech_learn_transport)**(self.parent.year_current + self.parent.ccats_fin.duration_b0 - self.parent.year_start)
        self.cost_reduce_fr_transport_b2 = (1.0 - self.tech_learn_transport)**(self.parent.year_current + self.parent.ccats_fin.duration_b0 + self.parent.ccats_fin.duration_b1 - self.parent.year_start)

        self.capex_transport_base_b0   = self.pipeline_lookup_opt_b0['pipe_capex_intercept'].copy() * self.cost_reduce_fr_transport_b0
        self.capex_transport_base_b1   = self.pipeline_lookup_opt_b1['pipe_capex_intercept'].copy() * self.cost_reduce_fr_transport_b1
        self.capex_transport_base_b2   = self.pipeline_lookup_opt_b2['pipe_capex_intercept'].copy() * self.cost_reduce_fr_transport_b2

        self.capex_transport_slope_b0  = self.pipeline_lookup_opt_b0['pipe_capex_slope'].copy() * self.cost_reduce_fr_transport_b0
        self.capex_transport_slope_b1  = self.pipeline_lookup_opt_b1['pipe_capex_slope'].copy() * self.cost_reduce_fr_transport_b1
        self.capex_transport_slope_b2  = self.pipeline_lookup_opt_b2['pipe_capex_slope'].copy() * self.cost_reduce_fr_transport_b2


        ### Electricity transport costs
        temp_series = self.pipeline_lookup_opt['elec_cost_1987$'].loc[self.pipeline_lookup_opt.index.get_level_values(2) == 's1']
        temp_series.index = temp_series.index.droplevel(2)
        self.opex_transport_elec    = temp_series.copy()
        self.pipeline_lookup_opt.loc[self.pipeline_lookup_opt['average_electricity'] == 0.0, 'average_electricity'] = 0.00005 # set min average electricity so flows don't appear between existing TS-nodes
        temp_series = self.pipeline_lookup_opt['average_electricity'].loc[self.pipeline_lookup_opt.index.get_level_values(2) == 's1']
        temp_series.index = temp_series.index.droplevel(2)
        self.electricity_demand     = temp_series.copy()

        self.opex_transport_elec_b0 = self.opex_transport_elec.copy() * self.cost_reduce_fr_transport_b0
        self.opex_transport_elec_b1 = self.opex_transport_elec.copy() * self.cost_reduce_fr_transport_b1
        self.opex_transport_elec_b2 = self.opex_transport_elec.copy() * self.cost_reduce_fr_transport_b2

        ### Storage costs by block - apply technologic learning to capex and varom

        # apply tech learning to storage costs
        self.cost_reduce_fr_storage_b0 = (1.0 - self.tech_learn_storage)**(self.parent.year_current - self.parent.year_start)
        self.cost_reduce_fr_storage_b1 = (1.0 - self.tech_learn_storage)**(self.parent.year_current + self.parent.ccats_fin.duration_b0 - self.parent.year_start)
        self.cost_reduce_fr_storage_b2 = (1.0 - self.tech_learn_storage)**(self.parent.year_current + self.parent.ccats_fin.duration_b0 + self.parent.ccats_fin.duration_b1 - self.parent.year_start)
        
        self.storage_capex_b0 = self.storage_costs_opt['capex'].copy() * self.cost_reduce_fr_storage_b0
        self.storage_capex_b1 = self.storage_costs_opt['capex'].copy() * self.cost_reduce_fr_storage_b1
        self.storage_capex_b2 = self.storage_costs_opt['capex'].copy() * self.cost_reduce_fr_storage_b2

        self.storage_varom_b0 = self.storage_costs_opt['varom'].copy() * self.cost_reduce_fr_storage_b0
        self.storage_varom_b1 = self.storage_costs_opt['varom'].copy() * self.cost_reduce_fr_storage_b1
        self.storage_varom_b2 = self.storage_costs_opt['varom'].copy() * self.cost_reduce_fr_storage_b2

        ### Debug technologic learning (cost reduction by year/block)
        if self.parent.debug_switch == True:
            # store technologic learning data as a Series
            tech_learning = pd.Series()
            tech_learning['year_current']                 = int(self.parent.year_current)

            tech_learning['tech_learn_transport']         = self.tech_learn_transport
            tech_learning['cost_reduce_fr_transport_b0']  = self.cost_reduce_fr_transport_b0
            tech_learning['cost_reduce_fr_transport_b1']  = self.cost_reduce_fr_transport_b1
            tech_learning['cost_reduce_fr_transport_b2']  = self.cost_reduce_fr_transport_b2

            tech_learning['tech_learn_storage']           = self.tech_learn_storage
            tech_learning['cost_reduce_fr_storage_b0']    = self.cost_reduce_fr_storage_b0
            tech_learning['cost_reduce_fr_storage_b1']    = self.cost_reduce_fr_storage_b1
            tech_learning['cost_reduce_fr_storage_b2']    = self.cost_reduce_fr_storage_b2

            # transform to a dataframe and save to CSV
            tech_learning_df = tech_learning.to_frame().transpose()
            tech_learning_filename = self.output_path + 'tech_learning//' + 'tech_learning_transport_storage.csv'
            if self.year_current == self.parent.year_start:
                tech_learning_df.to_csv(tech_learning_filename)
            else:
                tech_learning_df.to_csv(tech_learning_filename, mode='a', header=False)

        ### CO2 Supply
        # Supply
        self.co2_supply_b0 = self.co2_supply_b0_opt.copy()['co2_volume']
        self.co2_supply_b1 = self.co2_supply_b1_opt.copy()['co2_volume']
        self.co2_supply_b2 = self.co2_supply_b2_opt.copy()['co2_volume']

        ### CO2 Demand
        self.co2_demand_b0 = self.eor_demand_opt.copy()['b0_co2_volume']
        self.co2_demand_b1 = self.eor_demand_opt.copy()['b1_co2_volume']
        self.co2_demand_b2 = self.eor_demand_opt.copy()['b2_co2_volume']

        ### CO2 Demand net cost
        self.co2_demand_cost_b0 = self.eor_cost_net_opt.copy()['b0_net_co2_cost']
        self.co2_demand_cost_b1 = self.eor_cost_net_opt.copy()['b1_net_co2_cost']
        self.co2_demand_cost_b2 = self.eor_cost_net_opt.copy()['b2_net_co2_cost']

        ### CO2 Storage
        # Injectivity
        self.co2_injectivity_existing_b0 = self.storage_existing_opt.copy()['net_storage_injectivity']
        self.co2_injectivity_existing_b1 = self.storage_existing_opt.copy()['net_storage_injectivity']
        self.co2_injectivity_existing_b2 = self.storage_existing_opt.copy()['net_storage_injectivity']
        self.co2_injectivity_new_b0      = self.storage_new_opt.copy()['inj_rate_project_tonnes_yr']
        self.co2_injectivity_new_b1      = self.storage_new_opt.copy()['inj_rate_project_tonnes_yr']
        self.co2_injectivity_new_b2      = self.storage_new_opt.copy()['inj_rate_project_tonnes_yr']

        # Max injectivity
        self.co2_max_injectivity_b0 = self.storage_new_opt.copy()['max_injectivity']
        self.co2_max_injectivity_b1 = self.storage_new_opt.copy()['max_injectivity']
        self.co2_max_injectivity_b2 = self.storage_new_opt.copy()['max_injectivity']

        # Net Storage
        self.co2_store_net_existing_b0 = self.storage_existing_opt.copy()['active_aor_co2_store_remaining']
        self.co2_store_net_existing_b1 = self.storage_existing_opt.copy()['active_aor_co2_store_remaining']
        self.co2_store_net_existing_b2 = self.storage_existing_opt.copy()['active_aor_co2_store_remaining']
        self.co2_store_net_adder_b0    = self.storage_new_opt.copy()['max_co2_store_cap']
        self.co2_store_net_adder_b1    = self.storage_new_opt.copy()['max_co2_store_cap']
        self.co2_store_net_adder_b2    = self.storage_new_opt.copy()['max_co2_store_cap']

        # AORS available
        self.storage_aors_available_b0 = self.storage_existing_opt.copy()['storage_aors_available']
        self.storage_aors_available_b1 = self.storage_existing_opt.copy()['storage_aors_available']
        self.storage_aors_available_b2 = self.storage_existing_opt.copy()['storage_aors_available']

        if self.parent.debug_switch == True:
            # store relevant data
            existing_storage = self.storage_existing_opt.copy()
            new_storage = self.storage_new_opt.copy()

            # store data used in the optimization program
            opt_storage = self.storage_existing_opt.copy()[['net_storage_injectivity','active_aor_co2_store_remaining','storage_aors_available']].merge(self.storage_new_opt.copy()[['inj_rate_project_tonnes_yr','max_co2_store_cap']],
                                                                                                                                                      left_index=True, right_index=True)
            
            opt_storage = opt_storage.rename(columns={'net_storage_injectivity':'storage_injectivity_existing',
                                                      'active_aor_co2_store_remaining':'storage_net_existing',
                                                      'storage_aors_available':'storage_aors_available',
                                                      'inj_rate_project_tonnes_yr':'storage_injectivity_new',
                                                      'max_co2_store_cap':'storage_net_adder'})
            
            # store year
            existing_storage['year_current'] = int(self.year_current)
            new_storage['year_current']      = int(self.year_current)
            opt_storage['year_current']      = int(self.year_current)

            # filenames
            storage_existing_filename = self.output_path + 'optimization_inputs//storage//' + 'storage_existing_opt.csv'
            storage_new_filename = self.output_path + 'optimization_inputs//storage//' + 'storage_new_opt.csv'
            storage_opt_filename = self.output_path + 'optimization_inputs//storage//' + 'storage_opt_inputs.csv'
            
            # save to csv
            if self.year_current == self.parent.year_start:
                existing_storage.to_csv(storage_existing_filename)
                new_storage.to_csv(storage_new_filename)
                opt_storage.to_csv(storage_opt_filename)
            else:
                existing_storage.to_csv(storage_existing_filename, mode='a', header=False)
                new_storage.to_csv(storage_new_filename, mode='a', header=False)
                opt_storage.to_csv(storage_opt_filename, mode='a', header=False)

        ### Time periods
        self.duration_b0 = self.parent.ccats_fin.duration_b0
        self.duration_b1 = self.parent.ccats_fin.duration_b1
        self.duration_b2 = self.parent.ccats_fin.duration_b2
        self.duration_45q = self.parent.ccats_fin.duration_45q

        ### Discounting/Financing
        # year that each block begins
        block_start = int(self.year_current) + np.array([0, self.duration_b0, self.duration_b0 + self.duration_b1])

        # ccats financing assumptions
        n_source         = self.parent.ccats_fin.financing_years_source      # number of years that source projects are financed over
        FOM_source       = self.parent.ccats_fin.fixed_om_fraction_source    # fraction of CAPEX paid as Fixed O&M annually        
        n_transport      = self.parent.ccats_fin.financing_years_transport   # number of years that transport projects are financed over
        FOM_transport    = self.parent.ccats_fin.fixed_om_fraction_transport # fraction of CAPEX paid as Fixed O&M annually
        n_storage        = self.parent.ccats_fin.financing_years_storage     # number of years that storage projects are financed over
        FOM_storage      = self.parent.ccats_fin.fixed_om_fraction_storage   # fraction of CAPEX paid as Fixed O&M annually

        self.discount_invest_source_b0  = self.parent.ccats_fin.calculate_discount_investment(block_start[0], n_source, FOM_source)
        self.discount_invest_source_b1  = self.parent.ccats_fin.calculate_discount_investment(block_start[1], n_source, FOM_source)
        self.discount_invest_source_b2  = self.parent.ccats_fin.calculate_discount_investment(block_start[2], n_source, FOM_source)

        self.discount_invest_storage_b0 = self.parent.ccats_fin.calculate_discount_investment(block_start[0], n_storage, FOM_storage)
        self.discount_invest_storage_b1 = self.parent.ccats_fin.calculate_discount_investment(block_start[1], n_storage, FOM_storage)
        self.discount_invest_storage_b2 = self.parent.ccats_fin.calculate_discount_investment(block_start[2], n_storage, FOM_storage)

        self.discount_invest_transport_b0 = self.parent.ccats_fin.calculate_discount_investment(block_start[0], n_transport, FOM_transport)
        self.discount_invest_transport_b1 = self.parent.ccats_fin.calculate_discount_investment(block_start[1], n_transport, FOM_transport)
        self.discount_invest_transport_b2 = self.parent.ccats_fin.calculate_discount_investment(block_start[2], n_transport, FOM_transport)

        variable_include_inflation = False
        self.discount_variable_b0 = self.parent.ccats_fin.calculate_discount_variable_policy(block_start[0], self.duration_b0, variable_include_inflation)
        self.discount_variable_b1 = self.parent.ccats_fin.calculate_discount_variable_policy(block_start[1], self.duration_b1, variable_include_inflation)
        self.discount_variable_b2 = self.parent.ccats_fin.calculate_discount_variable_policy(block_start[2], self.duration_b2, variable_include_inflation)

        # for tax credits, do not use more than the duration of the tax credit
        policy_duration_b0 = min(self.duration_b0, self.duration_45q)
        policy_duration_b1 = min(self.duration_b1, self.duration_45q)
        policy_duration_b2 = min(self.duration_b2, self.duration_45q)
        
        policy_include_inflation = False  #
        self.discount_policy_b0 = self.parent.ccats_fin.calculate_discount_variable_policy(block_start[0], policy_duration_b0, policy_include_inflation)
        self.discount_policy_b1 = self.parent.ccats_fin.calculate_discount_variable_policy(block_start[1], policy_duration_b1, policy_include_inflation)
        self.discount_policy_b2 = self.parent.ccats_fin.calculate_discount_variable_policy(block_start[2], policy_duration_b2, policy_include_inflation)

        if self.parent.debug_switch == True:
            # store relevant data as a Series
            discount_multipliers = pd.Series()
            discount_multipliers['year_current']                 = int(self.year_current)
            discount_multipliers['n_source']                     = n_source
            discount_multipliers['n_storage']                    = n_storage
            discount_multipliers['n_transport']                  = n_transport
            discount_multipliers['duration_b0']                  = self.duration_b0
            discount_multipliers['duration_b1']                  = self.duration_b1
            discount_multipliers['duration_b2']                  = self.duration_b2
            discount_multipliers['discount_invest_source_b0']    = self.discount_invest_source_b0
            discount_multipliers['discount_invest_source_b1']    = self.discount_invest_source_b1
            discount_multipliers['discount_invest_source_b2']    = self.discount_invest_source_b2
            discount_multipliers['discount_invest_storage_b0']   = self.discount_invest_storage_b0
            discount_multipliers['discount_invest_storage_b1']   = self.discount_invest_storage_b1
            discount_multipliers['discount_invest_storage_b2']   = self.discount_invest_storage_b2
            discount_multipliers['discount_invest_transport_b0'] = self.discount_invest_transport_b0
            discount_multipliers['discount_invest_transport_b1'] = self.discount_invest_transport_b1
            discount_multipliers['discount_invest_transport_b2'] = self.discount_invest_transport_b2
            discount_multipliers['discount_variable_b0']         = self.discount_variable_b0
            discount_multipliers['discount_variable_b1']         = self.discount_variable_b1
            discount_multipliers['discount_variable_b2']         = self.discount_variable_b2
            discount_multipliers['discount_policy_b0']           = self.discount_policy_b0
            discount_multipliers['discount_policy_b1']           = self.discount_policy_b1
            discount_multipliers['discount_policy_b2']           = self.discount_policy_b2

            # transform to a dataframe and save to CSV
            discount_multipliers_df = discount_multipliers.to_frame().transpose()
            discount_multipliers_filename = self.output_path + 'financing//' + 'discount_multipliers.csv'
            if self.year_current == self.parent.year_start:
                discount_multipliers_df.to_csv(discount_multipliers_filename)
            else:
                discount_multipliers_df.to_csv(discount_multipliers_filename, mode='a', header=False)

        ### Endogeneous sources of CO2
        if self.parent.endogenous_source_switch:

            #----------------------------
            ## Sets
            #----------------------------
            # Kinds of sources
            self.kinds_source               = self.source_kinds # List of values
            
            #----------------------------
            ## Parameters
            #----------------------------

            ### existing capacity (MultiIndex - Indexed by kind of DAC and node (k,n))
            if len(self.kinds_source)>0 and len(self.nodes_source)>0:
                self.annual_capacity_exist_source   = self.source_annual_capacity_existing.copy()
                self.total_capacity_exist_source   = self.source_total_capacity_existing.copy()

            else:
                self.annual_capacity_exist_source   = pd.Series()
                self.total_capacity_exist_source   = pd.Series()

            # investment costs by block (MultiIndex - Indexed by kind of DAC and node (k,n))
            self.cost_invest_source_b0       = self.cost_invest_source.copy()
            self.cost_invest_source_b1       = self.cost_invest_source.copy()
            self.cost_invest_source_b2       = self.cost_invest_source.copy()

            # variable costs by block (MultiIndex - Indexed by kind of DAC and node (k,n))
            self.cost_variable_source_b0     = self.cost_variable_source.copy()
            self.cost_variable_source_b1     = self.cost_variable_source.copy()
            self.cost_variable_source_b2     = self.cost_variable_source.copy()
            
            # Build limit - At a single node by block (Single Index - Indexed by kind of source,k)
            self.build_limit_source_node_b0  = self.build_limit_source_node
            self.build_limit_source_node_b1  = self.build_limit_source_node
            self.build_limit_source_node_b2  = self.build_limit_source_node

            # Build limit - Nationwide by block  (Single Index - Indexed by kind of source,k)
            self.build_limit_source_total_b0 = self.build_limit_source_total
            self.build_limit_source_total_b1 = self.build_limit_source_total
            self.build_limit_source_total_b2 = self.build_limit_source_total

            if self.parent.debug_switch:
                with open(self.output_path                               + 'source//input_kinds_source.dat', 'w') as f:
                    for kind in self.kinds_source:
                        f.write(f"{kind}\n")

                with open(self.output_path                               + 'source//input_vintages_source.dat', 'w') as f:
                    for vintage in self.vintages_source:
                        f.write(f"{vintage}\n")

                with open(self.output_path                               + 'source//input_nodes_source.dat', 'w') as f:
                    for node in self.nodes_source:
                        f.write(f"{node}\n")

                self.capacity_exist_source.to_csv(self.output_path       + 'source//input_capacity_exist_source.csv')
                self.cost_invest_source_b0.to_csv(self.output_path       + 'source//input_cost_invest_source_b0.csv')
                self.cost_invest_source_b1.to_csv(self.output_path       + 'source//input_cost_invest_source_b1.csv')
                self.cost_invest_source_b2.to_csv(self.output_path       + 'source//input_cost_invest_source_b2.csv')
                self.cost_variable_source_b0.to_csv(self.output_path     + 'source//input_cost_variable_source_b0.csv')
                self.cost_variable_source_b1.to_csv(self.output_path     + 'source//input_cost_variable_source_b1.csv')
                self.cost_variable_source_b2.to_csv(self.output_path     + 'source//input_cost_variable_source_b2.csv')
                self.build_limit_source_node_b0.to_csv(self.output_path  + 'source//input_build_limit_source_node_b0.csv')
                self.build_limit_source_node_b1.to_csv(self.output_path  + 'source//input_build_limit_source_node_b1.csv')
                self.build_limit_source_node_b2.to_csv(self.output_path  + 'source//input_build_limit_source_node_b2.csv')
                self.build_limit_source_total_b0.to_csv(self.output_path + 'source//input_build_limit_source_total_b0.csv')
                self.build_limit_source_total_b1.to_csv(self.output_path + 'source//input_build_limit_source_total_b1.csv')
                self.build_limit_source_total_b2.to_csv(self.output_path + 'source//input_build_limit_source_total_b2.csv')

            # LCFS and tax credits
            dac_tax_credits = pd.Series()
            dac_tax_credits['year_current']   = int(self.year_current)
            dac_tax_credits['dac_saline_45q'] = self.parent.dac_saline_45q * com.calculate_inflation(self.parent.rest_mc_jpgdp.copy(), self.parent.year_new_45q)
            dac_tax_credits['dac_eor_45q']    = self.parent.dac_eor_45q    * com.calculate_inflation(self.parent.rest_mc_jpgdp.copy(), self.parent.year_new_45q)
            dac_tax_credits['LCFS']           = self.LCFS_credit

            # transform to a dataframe and save to CSV
            dac_tax_credits = dac_tax_credits.to_frame().transpose()
            dac_tax_credits_filename = self.output_path + 'source//' + 'tax_credits.csv'
            if self.year_current == self.parent.year_start:
                dac_tax_credits.to_csv(dac_tax_credits_filename)
            else:
                dac_tax_credits.to_csv(dac_tax_credits_filename, mode='a', header=False)

        ### Test data
        self.parent.pytest.nodes_supply                     = self.nodes_supply.copy()
        self.parent.pytest.nodes_trans_ship                 = self.nodes_trans_ship.copy()
        self.parent.pytest.nodes_sequester                  = self.nodes_sequester.copy()
        self.parent.pytest.nodes_demand                     = self.nodes_demand.copy()
        self.parent.pytest.nodes_storage                    = self.nodes_storage.copy()
        self.parent.pytest.nodes                            = self.nodes.copy()
        self.parent.pytest.arcs                             = self.arcs.copy()
        self.parent.pytest.co2_supply_b0                    = self.co2_supply_b0.copy()
        self.parent.pytest.co2_supply_b1                    = self.co2_supply_b1.copy()
        self.parent.pytest.co2_supply_b2                    = self.co2_supply_b2.copy()
        self.parent.pytest.co2_demand_b0                    = self.co2_demand_b0.copy()
        self.parent.pytest.rest_industrial_co2_supply_45q   = self.parent.rest_industrial_co2_supply_45q.copy()
        self.parent.pytest.rest_industrial_co2_supply_ntc   = self.parent.rest_industrial_co2_supply_ntc.copy()
        self.parent.pytest.rest_eor_demand_df               = self.parent.rest_dem_eor.copy()


        if self.parent.debug_switch == True:
            self.pipeline_lookup_opt.to_csv(self.output_path + 'optimization_inputs//transport//pipeline_network_' + str(self.parent.year_current) + '.csv')
            self.opex_transport_elec.to_csv(self.output_path + 'optimization_inputs//transport//electricity_cost_' + str(self.parent.year_current) + '.csv')
            self.electricity_demand.to_csv(self.output_path + 'optimization_inputs//transport//electricity_demand_' + str(self.parent.year_current) + '.csv')

        pass


    #######################################
    ###Pickling and Debugging
    #######################################


    def write_pkl(self):
        '''Write CCATS variables that need to be passed between model iterations to .pkl files.

        Parameters
        ----------
        None

        Returns
        -------
        self.parent.pkl.preproc_i_storage_df : DataFrame
            DataFrame of storage formations - input data.

        self.parent.pkl.preproc_i_co2_supply_facility_df : DataFrame
            DataFrame of CO\ :sub:`2` cost curve from NETL - input data.

        self.parent.pkl.preproc_i_pipeline_lookup_df : DataFrame
            DataFrame of CO\ :sub:`2` pipeline lookup table - input data.

        self.parent.pkl.preproc_i_eor_demand_df : DataFrame
            DataFrame of CO\ :sub:`2` EOR site CO\ :sub:`2` demanded - input data.

        self.parent.pkl.preproc_i_eor_cost_net_df : DataFrame
            DataFrame of CO\ :sub:`2` EOR net cost for CO\ :sub:`2` - input data.

        self.parent.pkl.preproc_i_ts_multiplier_df : DataFrame
            DataFrame of multipliers for ts-ts node arcs - input data.

        self.parent.pkl.preproc_i_nat_co2_lookup_df : DataFrame
            DataFrame of natural CO\ :sub:`2` field attributes            

        self.parent.pkl.preproc_pipes_existing_df : DataFrame
            DataFrame of existing CO\ :sub:`2` pipeline infrastructure in a given model year.

        self.parent.pkl.preproc_storage_existing_df : DataFrame
            DataFrame of existing CO\ :sub:`2` storage infrastructure in a given model year.

        self.parent.pkl.preproc_co2_facility_eligibility_df : DataFrame
            DataFrame of CO\ :sub:`2` facility 45Q eligibility.
        
        self.parent.pkl.preproc_i_source_data : DataFrame
            DataFrame of endogenous sources of CO\ :sub:`2` data.

        self.parent.pkl.preproc_i_source_annual_capacity_existing : DataFrame
            DataFrame of endogenous sources of CO\ :sub:`2` existing capacity.

        self.parent.pkl.preproc_i_source_capacity_limits : DataFrame
            DataFrame of endogenous sources of CO\ :sub:`2` capacity limits.        
        
        '''
        ### Write input tables
        self.parent.pkl.preproc_i_storage_df                = self.i_storage_df.copy()
        self.parent.pkl.preproc_i_co2_supply_facility_df    = self.i_co2_supply_facility_df.copy()
        self.parent.pkl.preproc_i_pipeline_lookup_df        = self.i_pipeline_lookup_df.copy()
        self.parent.pkl.preproc_i_eor_demand_df             = self.i_eor_demand_df.copy()
        self.parent.pkl.preproc_i_eor_cost_net_df           = self.i_eor_cost_net_df.copy()
        self.parent.pkl.preproc_i_ts_multiplier_df          = self.i_ts_multiplier_df.copy()
        self.parent.pkl.preproc_i_nat_co2_lookup_df         = self.i_nat_co2_lookup_df.copy()

        ### Write local process tables
        self.parent.pkl.preproc_pipes_existing_df                 = self.pipes_existing_df.copy()
        self.parent.pkl.preproc_storage_existing_df               = self.storage_existing_df.copy()
        self.parent.pkl.preproc_co2_facility_eligibility_df       = self.co2_facility_eligibility_df.copy()
        self.parent.pkl.preproc_nat_co2_lookup_df                 = self.nat_co2_lookup_df.copy()
        self.parent.pkl.preproc_source_annual_capacity_existing   = self.source_annual_capacity_existing.copy()
        self.parent.pkl.preproc_source_total_capacity_existing    = self.source_total_capacity_existing.copy()

        # Endogenous sources of CO2
        self.parent.pkl.preproc_i_source_data                     = self.i_source_data.copy()
        self.parent.pkl.preproc_i_source_capacity_limits          = self.i_source_capacity_limits.copy()

        pass


    def summarize_inputs(self):
        '''Output debug file of preprocessor data summary.

        Parameters
        ----------
        None

        Returns
        -------
        None

        '''
        # Create datat summary dictionary
        preproc_data = {
            'year' : [int(self.year_current)],
            'iteration': [int(self.parent.iteration_current)],
            'cycle': [int(self.parent.cycle_current)],
            'number of nodes': [len(self.nodes)],
            'number of supply nodes': [len(self.nodes_supply)],
            'number of trans_ship nodes': [len(self.nodes_trans_ship)],
            'number of demand nodes': [len(self.nodes_demand)],
            'number of storage nodes': [len(self.nodes_storage)],
            'number of arcs': [len(self.arcs)],
            'total CO2 volume supplied time period 0':[self.supply_sources_sparse_b0_df['co2_volume'].sum()],
            'total CO2 volume supplied time period 1': [self.supply_sources_sparse_b1_df['co2_volume'].sum()],
            'total CO2 volume supplied time period 2': [self.supply_sources_sparse_b2_df['co2_volume'].sum()],
            'total CO2 volume demanded time period 0': [self.eor_demand_df['b0_co2_volume'].sum()],
            'total CO2 volume demanded time period 1': [self.eor_demand_df['b1_co2_volume'].sum()],
            'total CO2 volume demanded time period 2': [self.eor_demand_df['b2_co2_volume'].sum()],
            'total existing annual CO2 storage': [(self.storage_existing_df['inj_rate_project_tonnes_yr'] * self.storage_new_df['storage_aors_existing']).sum()],
            'total undeveloped annual CO2 storage': [(self.storage_new_df['inj_rate_project_tonnes_yr'] * self.storage_new_df['storage_aors_available']).sum()]
        }

        # Convert to df an output debug file
        preproc_df = pd.DataFrame(preproc_data)

        if int(self.year_current) == self.parent.year_start:
            preproc_df.to_csv(self.output_path + 'data_stats_preproc.csv', mode='w')
        else:
            preproc_df.to_csv(self.output_path + 'data_stats_preproc.csv', mode='a', header=False)

        pass


    def visualize_inputs(self):
        '''Visualize preprocessed data.

        Parameters
        ----------
        None

        Returns
        -------
        None

        '''
        # Only import visualization as needed, computationally expensive to setup
        from ccats_common import common_visual as com_vis

        vis_folder = self.output_path + 'visualization_results//' + 'preprocessor'
        with open(vis_folder + '//map.run', 'w') as file:
            file.write('run folder:' + vis_folder)

        temp_pipeline_lookup_df = self.pipeline_lookup_df.copy()
        temp_pipeline_lookup_df = temp_pipeline_lookup_df.loc[temp_pipeline_lookup_df['pipe_segment'] == 's1'] 

        temp_pipeline_lookup_df['co2_volume'] = self.pipeline_lookup_df['max_throughput_tonnes'].copy()

        com_vis.create_folium_map(temp_pipeline_lookup_df[['node_i_id',
                                                   'node_i_type',
                                                   'i_longitude',
                                                   'i_latitude',
                                                   'node_j_id',
                                                   'node_j_type',
                                                   'j_longitude',
                                                   'j_latitude',
                                                   'co2_volume',
                                                   'route_id', 
                                                   'pipe_segment']],
                                  self.output_path + 'visualization_results//preprocessor//',
                                  'preproc_pipeline_map_' + str(int(self.year_current)) + '.html')
        
        pass
