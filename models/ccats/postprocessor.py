"""Submodule for Postprocessing CCATS results.

Postprocessor: Summary
______________________
This submodule postprocesses CCATS model results and prepares results to be sent to the restart file:

    1. :meth:`~postprocessor.Postprocessor.setup` is called from :ref:`module`, assigning postprocessor price smoothing variables
       and infeasibility output switches.

    2. :meth:`~postprocessor.Postprocessor.run` is called from :ref:`module`.

    3. Extract results from Pyomo model, storing results as Pandas DataFrames.

    4. Convert dual variables to weighted-average CO2 price variables used by other NEMS modules in :meth:`~postprocessor.Postprocessor.duals_to_df`.

    5. Test for infeasibilities.

    6. Write data required for next model year calculations to local variables, and pickle.

    6. Prepare results to write to the restart file.

    7. Create result summaries.

    8. Visualize results.


Postprocessor: Input Files
__________________________

* postproc_setup.csv - postprocessor setup file


Postprocessor: Model Functions and Class Methods
________________________________________________

* :meth:`~postprocessor.Postprocessor.__init__` - Initializes variables to be populated by Postprocessor (called by :meth:`module.Module.setup`)
* :meth:`~postprocessor.Postprocessor.setup` - Sets run settings using the setup file (called by :meth:`module.Module.setup`)
* :meth:`~postprocessor.Postprocessor.run` - Calls remaining Postprocessor functions (called by :meth:`module.Module.run`)
* :meth:`~postprocessor.Postprocessor.variables_to_df` - Process Pyomo results into Pandas DataFrames (called by :meth:`~postprocessor.Postprocessor.run`)
* :meth:`~postprocessor.Postprocessor.slacks_to_df` - Processes slack variables (called by :meth:`~postprocessor.Postprocessor.run`)
* :meth:`~postprocessor.Postprocessor.assign_flows_to_pipes` - Aggregates flows by pipeline segment (called by :meth:`~postprocessor.Postprocessor.run`)
* :meth:`~postprocessor.Postprocessor.binding_constraints_to_df` - Process binding constraints (called by :meth:`~postprocessor.Postprocessor.run`)
* :meth:`~postprocessor.Postprocessor.duals_to_df` - Process dual variables (called by :meth:`~postprocessor.Postprocessor.run`)
* :meth:`~postprocessor.Postprocessor.electricity_demand_to_df` - Process electricity consumed (called by :meth:`~postprocessor.Postprocessor.run`)
* :meth:`~postprocessor.Postprocessor.copy_variables_for_pytest` - Copy variables for Pytest (called by :meth:`~postprocessor.Postprocessor.run`)
* :meth:`~postprocessor.Postprocessor.test_flows` - Test slack variables (called by :meth:`~postprocessor.Postprocessor.run`)
* :meth:`~postprocessor.Postprocessor.update_existing_pipeline_network` - Process new pipeline capacity (called by :meth:`~postprocessor.Postprocessor.run`)
* :meth:`~postprocessor.Postprocessor.update_storage_volumes` - Process new storage capacity :sub:`2` stored (called by :meth:`~postprocessor.Postprocessor.run`)
* :meth:`~postprocessor.Postprocessor.write_restart_file` - Output variables for the restart file (called by :meth:`~postprocessor.Postprocessor.run`)
* :meth:`~postprocessor.Postprocessor.write_pkl` - Output variables for pickling (called by :meth:`~postprocessor.Postprocessor.run`)
* :meth:`~postprocessor.Postprocessor.results_to_csv` - Save results to CSV (called by :meth:`~postprocessor.Postprocessor.run`)
* :meth:`~postprocessor.Postprocessor.summarize_outputs` - Create high level run summary (called by :meth:`~postprocessor.Postprocessor.run`)
* :meth:`~postprocessor.Postprocessor.visualize_results` - Create interactive folium maps and datatable (called by :meth:`~postprocessor.Postprocessor.run`)

Postprocessor: Output Debug Files
_________________________________

Some debug files are output multiple times during a NEMS run, and use the following convention:

    * <model> is the name of the model (default is ccats_opt),
    * <block> is the number of the current block (0, 1, or 2),
    * <year> is the current model year, for example 2024,
    * <iteration> is the current NEMS iteration, and
    * <cycle> is the current NEMS cycle.

* In debug// (output by :meth:`~postprocessor.Postprocessor.summarize_outputs`):
    * **<model>_data_stats_postproc.csv** - High level summary of the run
* In debug//optimization_results//binding_constraints// (output by :meth:`~postprocessor.Postprocessor.run`):
    * **max_cap_binding_trans_existing_<block>_<year>.csv** - Existing transport capacity constraints that are binding (hit limit)
    * **max_cap_binding_trans_add_<block>_<year>.csv** - Added transport capacity constraints that are binding
    * **max_cap_binding_store_<block>_<year>.csv** - Storage injectivity constraints that are binding
* In debug//optimization_tests// (output by :meth:`~postprocessor.Postprocessor.test_flows`):
    * **infeasible_supply_b<block>_<year>_<iteration>_<cycle>.csv** - Supply node infeasibilities
    * **infeasible_ts_in_b<block>_<year>_<iteration>_<cycle>.csv** - Infeasibilities into trans-shipment nodes
    * **infeasible_ts_out_b<block>_<year>_<iteration>_<cycle>.csv** - Infeasibilities out of trans-shipment nodes
* In debug//optimization_results// (output by :meth:`~postprocessor.Postprocessor.results_to_csv`):
    * flows//**b<block>_flows_all_years.csv** - All flows
    * investment_decisions//**<model>_bins_b<block>_<year>.csv** - Pipeline binary investment decisions (only meaningful for MILP)
    * transport_cap_added//**<model>_cap_add_b<block>_<year>.csv** - Transportation capacity added
    * investment_decisions//**<model>_store_aors_b<block>_<year>.csv** - Storage capacity added
    * duals//**<model>_duals_<year>.csv** - Dual variables
* In debug//visualization_results//<model>// (output by :meth:`~postprocessor.Postprocessor.visualize_results`):
    * **postproc_pipeline_map_<year>_<block>.html** - Map of active CO\ :sub:`2` flows
    * **datatable_postproc_pipeline_map_<year>_<block>.html** - Table summarizing results.
* In debug//electricity_demand// (output by :meth:`~postprocessor.Postprocessor.write_restart_file`):
    * **electricity_demand.csv** - Overview of electricity consumed for transportation


Postprocessor: Output Restart Variables
_______________________________________

* **rest_co2_sup_out** - Supplied CO\ :sub:`2` by census division and type
        * 1 = PP Coal,
        * 2 = PP Natgas,
        * 3 = BECCS,
        * 4 = Natural Gas Processing,
        * 5 = Cement,
        * 6 = Ethanol,
        * 7 = Hydrogen,
        * 8 = Other, and
        * 9 = Total.
* **rest_co2_seq_out** - Sequestered CO\ :sub:`2` by census division and storage type where
    * 1 = EOR, and
    * 2 = Saline.
* **rest_co2_sup_out_r** - Supplied CO\ :sub:`2` by census region and type
        * 1 = PP Coal,
        * 2 = PP Natgas,
        * 3 = BECCS,
        * 4 = Natural Gas Processing,
        * 5 = Cement,
        * 6 = Ethanol,
        * 7 = Hydrogen,
        * 8 = Other, and
        * 9 = Total.
* **rest_co2_seq_out_r** - Sequestered CO\ :sub:`2` by census region and storage type where
    * 1 = EOR, and
    * 2 = Saline.
* **rest_co2_prc_dis_45q** - 45Q CO\ :sub:`2` price by census division
* **rest_co2_prc_dis_ntc** - NTC CO\ :sub:`2` price by census division
* **rest_co2_prc_reg_45q** - 45Q CO\ :sub:`2` price by census region
* **rest_co2_prc_reg_ntc** - NTC CO\ :sub:`2` price by census region
* **rest_co2_elec** - Electricity consumed by pipelines by census region


Postprocessor: Code
___________________
"""

import pandas as pd
import numpy as np
import pyomo
import pyomo.contrib
import pyomo.environ as pyo
import os
import shutil

import submodule_ccats as subccats
from ccats_common import common as com
from ccats_common import common_visual as com_vis

class Postprocessor(subccats.Submodule):
    """Postprocessor Submodule for CCATS.

    """


    def __init__(self, parent):
        """Initializes Postprocessor object.

        Parameters
        ----------

        parent : str
            Module.Module (Pointer to parent module)


        Returns
        -------

        None

        """
        super().__init__(parent, submodule_name='postprocessor')
        ### I/O Setup
        self.parent = parent  #: module.Module head module

        ### Postprocessor inputs
        self.price_limit        = 0.0 # Min/Max price produced by CCATS
        self.price_peg          = 0.0 # Price Peg
        self.slack_threshold    = 0.0 # Volume threshold at which infeasibilities are recorded in debug outputs

        ### Model class objects
        self.m  = None # Overarching Pyomo model
        self.b0 = None # Model block 0
        self.b1 = None # Model block 1
        self.b2 = None # Model block 2

        ### Model output tables
        # Block 0
        self.O_flows_b0_df              = pd.DataFrame() # DataFrame of flow output variable from optimization
        self.O_pipe_bins_b0_df          = pd.DataFrame() # DataFrame of pipeline binary investment decision output variable from optimization
        self.O_store_new_aors_b0_df     = pd.DataFrame() # DataFrame of storage investment decision
        self.O_infeasible_supply_b0_df  = pd.DataFrame() # DataFrame of total supply slack variable from optimization
        self.O_infeasible_ts_in_b0_df   = pd.DataFrame() # DataFrame of total supply ts_in variable from optimization
        self.O_infeasible_ts_out_b0_df  = pd.DataFrame() # DataFrame of total supply ts_out variable from optimization
        self.O_slack_demand_b0_df       = pd.DataFrame() # DataFrame of total demand slack variable from optimization
        self.O_slack_storage_b0_df      = pd.DataFrame() # DataFrame of total storage slack variable from optimization

        # Block 1
        self.O_flows_b1_df              = pd.DataFrame() # DataFrame of flow output variable from optimization
        self.O_pipe_bins_b1_df          = pd.DataFrame() # DataFrame of pipeline binary investment decision output variable from optimization
        self.O_store_new_aors_b1_df     = pd.DataFrame() # DataFrame of storage investment decision output variable from optimization
        self.O_infeasible_supply_b1_df  = pd.DataFrame() # DataFrame of total supply slack variable from optimization
        self.O_infeasible_ts_in_b1_df   = pd.DataFrame() # DataFrame of total supply ts_in variable from optimization
        self.O_infeasible_ts_out_b1_df  = pd.DataFrame() # DataFrame of total supply ts_out variable from optimization
        self.O_slack_demand_b1_df       = pd.DataFrame() # DataFrame of total demand slack variable from optimization
        self.O_slack_storage_b1_df      = pd.DataFrame() # DataFrame of total storage slack variable from optimization

        # Block 2
        self.O_flows_b2_df              = pd.DataFrame() # DataFrame of flow output variable from optimization
        self.O_pipe_bins_b2_df          = pd.DataFrame() # DataFrame of pipeline binary investment decision output variable from optimization
        self.O_store_new_aors_b2_df     = pd.DataFrame() # DataFrame of storage investment decision output variable from optimization
        self.O_infeasible_supply_b2_df  = pd.DataFrame() # DataFrame of total supply slack variable from optimization
        self.O_infeasible_ts_in_b2_df   = pd.DataFrame() # DataFrame of total supply ts_in variable from optimization
        self.O_infeasible_ts_out_b2_df  = pd.DataFrame() # DataFrame of total supply ts_out variable from optimization
        self.O_slack_demand_b2_df       = pd.DataFrame() # DataFrame of total demand slack variable from optimization
        self.O_slack_storage_b2_df      = pd.DataFrame() # DataFrame of total storage slack variable from optimization

        ### Model constraints
        # Block 0
        self.C_bind_trans_existing_b0_df    = pd.DataFrame() # DataFrame of flows which hit max bounding constraint for existing transport
        self.C_bind_trans_add_b0_df         = pd.DataFrame() # DataFrame of flows which hit max bounding constraint for additional transport
        self.C_bind_store_b0_df             = pd.DataFrame() # DataFrame of flows which hit max bounding constraint for existing storage

        # Block 1
        self.C_bind_trans_existing_b1_df    = pd.DataFrame() # DataFrame of flows which hit max bounding constraint for existing transport
        self.C_bind_trans_add_b1_df         = pd.DataFrame() # DataFrame of flows which hit max bounding constraint for additional transport
        self.C_bind_store_b1_df             = pd.DataFrame() # DataFrame of flows which hit max bounding constraint for storage

        # Block 2
        self.C_bind_trans_existing_b2_df    = pd.DataFrame() # DataFrame of flows which hit max bounding constraint for existing transport
        self.C_bind_trans_add_b2_df         = pd.DataFrame() # DataFrame of flows which hit max bounding constraint for additional transport
        self.C_bind_store_b2_df             = pd.DataFrame() # DataFrame of flows which hit max bounding constraint for storage

        ### Model dual variables
        self.duals_df               = pd.DataFrame() # DataFrame of optimization dual variables
        self.shadow_price_df        = pd.DataFrame() # DataFrame of shadow prices
        self.shadow_price_div_df    = pd.DataFrame() # DataFrame of shadow prices grouped by census division
        self.shadow_price_reg_df    = pd.DataFrame() # DataFrame of shadow prices group by census region

        ### Postprocessor process tables
        self.postproc_df = pd.DataFrame() # Reporting table
        self.new_pipes_existing_df = pd.DataFrame() # Accounting of net new pipelines to be read into existing network next model year


    def setup(self, setup_filename):
        """Setup postprocessor Submodule for CCATS.

        Parameters
        ----------

        setup_filename : str
            Path to postprocessor setup file.


        Returns
        -------

        self.logger : Logger
            Logger utility, declared in parent.

        self.setup_table : DataFrame
            Setup table for Preprocessor.py, with input values and switches relating to the submodule

        self.price_limit : float
            Maximum price (+/-) returned by CCATS in $1987.

        self.price_peg : float
            Maximum price (+/-) movement allowed per cycle relative to the previous cycle in $1987.

        self.slack_threshold : float
            Volume threshold at which infeasibilities are recorded in debug outputs

        self.log_b1_infeasibilities : bool
            Determines whether or not to log infeasibilities for block 1.

        self.log_b2_infeasibilities : bool
            Determines whether or not to log infeasibilities for block 2.

        self.infeasibility_threshold_pct : float
            Log infeasibilities if greater than provided percentage of total flow.
        
        """
        super().setup(setup_filename)

        ### Localize parent variables
        self.logger = self.parent.logger


        ### General Setup
        # Declare input path
        self.postproc_input_path = self.input_path + 'postprocessor\\'

        # Read in setup file
        self.setup_table = com.read_dataframe(self.input_path + setup_filename, index_col=0)

        # Variables
        self.price_limit                 = float(self.setup_table.at['price_limit', 'value']) # Limit for CO2 price
        self.price_peg                   = float(self.setup_table.at['price_peg', 'value']) # Price peg
        self.slack_threshold             = float(self.setup_table.at['slack_threshold', 'value']) # Volume threshold at which infeasibilities are recorded in debug outputs
        self.log_b1_infeasibilities      = self.setup_table.at['log_b1_infeasibilities', 'value'].upper() == 'True'.upper() # Option to log block 1 infeasibilities (block 0 is always logged)
        self.log_b2_infeasibilities      = self.setup_table.at['log_b2_infeasibilities', 'value'].upper() == 'True'.upper() # Option to log block 2 infeasibilities (block 0 is always logged)
        self.infeasibility_threshold_pct = float(self.setup_table.at['infeasibility_threshold_pct', 'value']) # Log infeasibilities if greater than provided percentage of total flow

        pass


    def run(self):
        """Run postprocessor Submodule for CCATS.

        Parameters
        ----------

        None


        Returns
        -------

        self.m : Pyomo Model
            Pyomo model solved by :class:`models.ccats_optimization.OptimizationModel`.

        self.b0 : Pyomo Block
            Block 0 extracted from self.m.

        self.b1 : Pyomo Block
            Block 1 extracted from self.m.

        self.b2 : Pyomo Block
            Block 2 extracted from self.m.

        self.O_pipe_bins_b0_df : DataFrame
            Block 0 transportation binary investment decisions.

        self.O_pipe_bins_b1_df : DataFrame
            Block 1 transportation binary investment decisions.

        self.O_pipe_bins_b2_df : DataFrame
            Block 2 transportation binary investment decisions.

        self.O_store_new_aors_b0_df : DataFrame
            Block 0 aors opened.

        self.O_store_new_aors_b1_df : DataFrame
            Block 1 aors opened.

        self.O_store_new_aors_b2_df : DataFrame
            Block 2 aors opened.

        self.O_transport_cap_add_b0_df : DataFrame
            Block 0 transportation capacity added.

        self.O_transport_cap_add_b1_df : DataFrame
            Block 1 transportation capacity added.

        self.O_transport_cap_add_b2_df : DataFrame
            Block 2 transportation capacity added.

        self.O_infeasible_supply_b0_df : DataFrame
            Block 0 infeasible supply.

        self.O_infeasible_supply_b1_df : DataFrame
            Block 1 infeasible supply.

        self.O_infeasible_supply_b2_df : DataFrame
            Block 2 infeasible supply.

        self.O_infeasible_ts_in_b0_df : DataFrame
            Block 0 infeasible flow into trans-shipment nodes.

        self.O_infeasible_ts_in_b1_df : DataFrame
            Block 1 infeasible flow into trans-shipment nodes.

        self.O_infeasible_ts_in_b2_df : DataFrame
            Block 2 infeasible flow into trans-shipment nodes.

        self.O_infeasible_ts_out_b0_df : DataFrame
            Block 0 infeasible flow out of trans-shipment nodes.

        self.O_infeasible_ts_out_b1_df : DataFrame
            Block 1 infeasible flow out of trans-shipment nodes.

        self.O_infeasible_ts_out_b2_df : DataFrame
            Block 2 infeasible flow out of trans-shipment nodes.

        self.O_slack_demand_b0_df : DataFrame
            Block 0 slack demand for EOR nodes.

        self.O_slack_demand_b1_df : DataFrame
            Block 1 slack demand for EOR nodes.

        self.O_slack_demand_b2_df : DataFrame
            Block 2 slack demand for EOR nodes.

        self.O_slack_storage_b0_df : DataFrame
            Block 0 slack storage for saline storage nodes.

        self.O_slack_storage_b1_df : DataFrame
            Block 1 slack storage for saline storage nodes.

        self.O_slack_storage_b2_df : DataFrame
            Block 2 slack storage for saline storage nodes.

        self.O_flows_b0_df : DataFrame
            Block 0 flows assigned to pipelines.

        self.O_flows_b1_df : DataFrame
            Block 1 flows assigned to pipelines.

        self.O_flows_b2_df : DataFrame
            Block 2 flows assigned to pipelines.

        self.C_bind_trans_existing_b0_df : DataFrame
            Block 0 binding existing transportation constraints.

        self.C_bind_trans_existing_b1_df : DataFrame
            Block 1 binding existing transportation constraints.

        self.C_bind_trans_existing_b2_df : DataFrame
            Block 2 binding existing transportation constraints.

        self.C_bind_trans_add_b0_df : DataFrame
            Block 0 binding added transportation constraints.

        self.C_bind_trans_add_b1_df : DataFrame
            Block 1 binding added transportation constraints.

        self.C_bind_trans_add_b2_df : DataFrame
            Block 2 binding added transportation constraints.

        self.C_bind_store_b0_df : DataFrame
            Block 0 binding storage constraints.

        self.C_bind_store_b1_df : DataFrame
            Block 1 binding storage constraints.

        self.C_bind_store_b2_df : DataFrame
            Block 2 binding storage constraints.
        """
        super().run()

        self.logger.info('Running Postprocessor')

        # Declare model and block variables
        self.m  = self.parent.pyomo_model
        self.b0 = self.parent.pyomo_block0
        self.b1 = self.parent.pyomo_block1
        self.b2 = self.parent.pyomo_block2

        # Write model results to dfs
        self.logger.info('Write model results to block 0 dfs')
        self.O_flows_b0_df, self.O_pipe_bins_b0_df, self.O_store_new_aors_b0_df, self.O_transport_cap_add_b0_df = (
            self.variables_to_df(self.b0, self.O_flows_b0_df, self.O_pipe_bins_b0_df, self.O_store_new_aors_b0_df))

        self.logger.info('Write model results to block 1 dfs')
        self.O_flows_b1_df, self.O_pipe_bins_b1_df, self.O_store_new_aors_b1_df, self.O_transport_cap_add_b1_df = (
            self.variables_to_df(self.b1, self.O_flows_b1_df, self.O_pipe_bins_b1_df, self.O_store_new_aors_b1_df))

        self.logger.info('Write model results to block 2 dfs')
        self.O_flows_b2_df, self.O_pipe_bins_b2_df, self.O_store_new_aors_b2_df, self.O_transport_cap_add_b2_df = (
            self.variables_to_df(self.b2, self.O_flows_b2_df, self.O_pipe_bins_b2_df, self.O_store_new_aors_b2_df))

        # Write model infeasibilities to dfs
        if (self.m.objective_value == 0) | (self.parent.preproc.co2_supply_b0.sum() == 0) | (self.parent.preproc.co2_supply_b0.empty):
            self.O_infeasible_supply_b0_df = pd.DataFrame(columns = ['node_id', 'infeasible_slack'])
            self.O_infeasible_supply_b1_df = pd.DataFrame(columns = ['node_id', 'infeasible_slack'])
            self.O_infeasible_supply_b2_df = pd.DataFrame(columns = ['node_id', 'infeasible_slack'])

            self.O_infeasible_ts_in_b0_df = pd.DataFrame(columns = ['node_id', 'infeasible_slack'])
            self.O_infeasible_ts_in_b1_df = pd.DataFrame(columns=['node_id', 'infeasible_slack'])
            self.O_infeasible_ts_in_b2_df = pd.DataFrame(columns=['node_id', 'infeasible_slack'])

            self.O_infeasible_ts_out_b0_df = pd.DataFrame(columns=['node_id', 'infeasible_slack'])
            self.O_infeasible_ts_out_b1_df = pd.DataFrame(columns=['node_id', 'infeasible_slack'])
            self.O_infeasible_ts_out_b2_df = pd.DataFrame(columns=['node_id', 'infeasible_slack'])

            self.O_slack_demand_b0_df = pd.DataFrame(columns=['node_id', 'infeasible_slack'])
            self.O_slack_demand_b1_df = pd.DataFrame(columns=['node_id', 'infeasible_slack'])
            self.O_slack_demand_b2_df = pd.DataFrame(columns=['node_id', 'infeasible_slack'])

            self.O_slack_storage_b0_df = pd.DataFrame(columns=['node_id', 'infeasible_slack'])
            self.O_slack_storage_b1_df = pd.DataFrame(columns=['node_id', 'infeasible_slack'])
            self.O_slack_storage_b2_df = pd.DataFrame(columns=['node_id', 'infeasible_slack'])

        else:
            self.logger.info('Write model slacks to block 0 dfs')
            self.O_infeasible_supply_b0_df, self.O_infeasible_ts_in_b0_df,self.O_infeasible_ts_out_b0_df, self.O_slack_demand_b0_df, self.O_slack_storage_b0_df = (
                self.slacks_to_df(self.b0,self.O_infeasible_supply_b0_df, self.O_infeasible_ts_in_b0_df,self.O_infeasible_ts_out_b0_df, self.O_slack_demand_b0_df, self.O_slack_storage_b0_df))

            self.logger.info('Write model slacks to block 1 dfs')
            self.O_infeasible_supply_b1_df, self.O_infeasible_ts_in_b1_df,self.O_infeasible_ts_out_b1_df, self.O_slack_demand_b1_df, self.O_slack_storage_b1_df = (
                self.slacks_to_df(self.b1, self.O_infeasible_supply_b1_df, self.O_infeasible_ts_in_b1_df,self.O_infeasible_ts_out_b1_df, self.O_slack_demand_b1_df, self.O_slack_storage_b1_df))

            self.logger.info('Write model slacks to block 2 dfs')
            self.O_infeasible_supply_b2_df, self.O_infeasible_ts_in_b2_df, self.O_infeasible_ts_out_b2_df,self.O_slack_demand_b2_df, self.O_slack_storage_b2_df = (
                self.slacks_to_df(self.b2, self.O_infeasible_supply_b2_df, self.O_infeasible_ts_in_b2_df, self.O_infeasible_ts_out_b2_df, self.O_slack_demand_b2_df, self.O_slack_storage_b2_df))

        # Assign flows to pipelines
        self.logger.info('Assign flows to Pipelines')
        self.O_flows_b0_df = self.assign_flows_to_pipes(self.O_flows_b0_df, None, 0)
        self.O_flows_b1_df = self.assign_flows_to_pipes(self.O_flows_b1_df, self.O_pipe_bins_b0_df, 1)
        self.O_flows_b2_df = self.assign_flows_to_pipes(self.O_flows_b2_df, self.O_pipe_bins_b1_df, 2)


        # Write binding constraints to dfs
        self.logger.info('Write model binding constraints to block 0 dfs')
        self.C_bind_trans_existing_b0_df, self.C_bind_trans_add_b0_df, self.C_bind_store_b0_df =(
            self.binding_constraints_to_df('b0', self.O_flows_b0_df,
                                           self.C_bind_trans_existing_b0_df,
                                           self.C_bind_trans_add_b0_df,
                                           self.C_bind_store_b0_df,
                                           self.O_store_new_aors_b0_df))

        self.logger.info('Write model binding constraints to block 1 dfs')
        self.C_bind_trans_existing_b1_df, self.C_bind_trans_add_b1_df, self.C_bind_store_b1_df = (
            self.binding_constraints_to_df('b1', self.O_flows_b1_df,
                                           self.C_bind_trans_existing_b1_df,
                                           self.C_bind_trans_add_b1_df,
                                           self.C_bind_store_b1_df,
                                           self.O_store_new_aors_b1_df))

        self.logger.info('Write model binding constraints to block 2 dfs')
        self.C_bind_trans_existing_b2_df, self.C_bind_trans_add_b2_df, self.C_bind_store_b2_df = (
            self.binding_constraints_to_df('b2', self.O_flows_b2_df,
                                           self.C_bind_trans_existing_b2_df,
                                           self.C_bind_trans_add_b2_df,
                                           self.C_bind_store_b2_df,
                                           self.O_store_new_aors_b2_df))


        # Write duals to df
        self.logger.info('Duals to DF')
        self.duals_to_df()

        # Write electricity demand to df
        self.logger.info('Electricity demand to DF')
        self.electricity_demand_to_df()

        # Runtime slack variable tests
        self.logger.info('Test Slack Variables')
        self.test_flows()

        # Save pipeline network for next model year
        self.logger.info('Save Pipeline Network for next model year')
        self.update_existing_pipeline_network()

        # Save storage volumes  for next model year
        self.logger.info('Save Storage Capacity Used for next model year')
        self.update_storage_volumes()

        # Write results to local restart file variables
        self.logger.info('Write results to local restart file variables')
        self.write_restart_file()

        # Write data to pytest for unit testing and result reconciliation
        self.logger.info('Copy Variables for Pytest')
        self.copy_variables_for_pytest()

        # Write Pickle
        if ((self.parent.integrated_switch == 1) & (self.parent.param_fcrl == 1)) | ((self.parent.integrated_switch == 1) & (self.parent.param_ncrl == 1)):
            self.logger.info('Write Local Variables to PKL')
            self.write_pkl()


        # Output postprocessor
        self.logger.info('Output Results from Postprocessor')
        if self.parent.debug_switch:
            self.results_to_csv()
            self.summarize_outputs()

        # Visualize results
        self.logger.info('Visualize Results')
        if self.parent.visualize_postproc_switch == True:
            self.visualize_results(self.O_flows_b0_df, 'b0')
            self.visualize_results(self.O_flows_b1_df, 'b1')
            self.visualize_results(self.O_flows_b2_df, 'b2')

        pass


    def variables_to_df(self, block, O_flows_df, O_pipe_bins_df, O_store_new_aors_df):
        '''Write pyomo results to DataFrames for use in postprocessors.

        Parameters
        ----------

        block: Pyomo Block
            Current Pyomo Block to be analyzed.

        O_flows_df: DataFrame
            Expected to be an empty DataFrame.

        O_pipe_bins_df : DataFrame
            Expected to be an empty DataFrame.

        O_store_new_aors_df : DataFrame
            Expected to be an empty DataFrame.


        Returns
        -------

        O_flows_df : DataFrame
            Flows indexed by arc (node i, node j), 45Q elgibility, and pipeline ID.

        O_pipe_bins_df : DataFrame
            Transportation binary investment decisions indexed by arc (node i, node j) and pipeline ID.

        O_store_new_aors_df : DataFrame
            Saline storage added (AORS) indexed by node.

        O_transport_cap_add_df : DataFrame
            Transportation capacity added indexed by arc (node i, node j) and segment.
        '''
        ### Create flow output df
        O_flows_df = com.unpack_pyomo(block.v_flow_by_policy, pyo.value, 3)
        O_flows_df = O_flows_df.rename(columns = {0:'node_i_id', 1:'node_j_id', 2:'eligibility_45q', 3:'pipeline_id', 'variable value':'co2_volume'})
        O_flows_df = O_flows_df.merge(self.preproc.pipeline_lookup_df,
                                                how = 'left',
                                                on = ['node_i_id','node_j_id'])
        O_flows_df = O_flows_df.drop(['index'], axis = 1)

        # Mask for tiny deviations from 0
        mask_neg1 = O_flows_df['co2_volume'] >= -1
        mask_pos1 = O_flows_df['co2_volume'] <= 1
        mask = mask_pos1 & mask_neg1
        temp_flows = O_flows_df[mask].copy()
        temp_flows['co2_volume'] = 0
        O_flows_df.update(temp_flows)


        ### Create pipe binary variable output df for MILP version of the model
        O_pipe_bins_df = com.unpack_pyomo(block.vb_transport_investment, pyo.value, 3)
        O_pipe_bins_df = O_pipe_bins_df.rename(columns={0: 'node_i_id', 1: 'node_j_id', 2: 'pipe_segment', 3: 'pipeline_id', 'variable value': 'pipe_bin'})
        O_pipe_bins_df = O_pipe_bins_df.merge(self.preproc.pipeline_lookup_df,
                                                            how='left',
                                                            on=['node_i_id', 'node_j_id', 'pipe_segment'])

        if self.parent.linear_model_switch == True:
            O_pipe_bins_df['pipe_bin'] = 1


        ### Create new storage AOR output df
        O_store_new_aors_df = com.unpack_pyomo(block.v_storage_investment, pyo.value, 1)
        O_store_new_aors_df = O_store_new_aors_df.rename(columns={0: 'node_id', 1: 'variable', 'variable value': 'new_aors'})

        ### Create pipeline transport capacity added output df
        O_transport_cap_add_df = com.unpack_pyomo(block.v_transport_cap_add, pyo.value, 3)
        O_transport_cap_add_df = O_transport_cap_add_df.rename(columns={0: 'node_i_id', 1: 'node_j_id', 2: 'segment', 'variable value': 'thruput_tonnes'})

        return O_flows_df, O_pipe_bins_df, O_store_new_aors_df, O_transport_cap_add_df


    def slacks_to_df(self, block, O_infeasible_supply, O_infeasible_ts_in, O_infeasible_ts_out, O_slack_demand, O_slack_storage):
        '''Write pyomo slack variables to DataFrames for use in postprocessors.

        Parameters
        ----------

        block : Pyomo Block
            Pyomo Block to be analyzed.

        O_infeasible_supply : DataFrame
            Empty DataFrame.

        O_infeasible_ts_in : DataFrame
            Empty DataFrame.

        O_infeasible_ts_out : DataFrame
            Empty DataFrame.

        O_slack_demand : DataFrame
            Empty DataFrame.

        O_slack_storage : DataFrame
            Empty DataFrame.


        Returns
        -------

        O_infeasible_supply : DataFrame
            Infeasible supply by node.

        O_infeasible_ts_in : DataFrame
            Infeasible flow into a trans-shipment node.

        O_infeasible_ts_out: DataFrame
            Infeasible flow out of a trans-shipment node.

        O_slack_demand : DataFrame
            Slack EOR demand by node.

        O_slack_storage : DataFrame
            Slack saline storage by node.
        '''
        ### Slack Variables
        # Infeasible supply
        O_infeasible_supply = com.unpack_pyomo(block.va_excess_supply, pyo.value, 1).drop(['index',1], axis = 1)
        O_infeasible_supply = O_infeasible_supply.rename(columns={0: 'node_id', 'variable value': 'infeasible_slack'})

        # Infeasible TS
        O_infeasible_ts_in = com.unpack_pyomo(block.va_excess_ts_in, pyo.value, 1).drop(['index',1], axis = 1)
        O_infeasible_ts_in = O_infeasible_ts_in.rename(columns={0: 'node_id', 'variable value': 'infeasible_slack'})
        O_infeasible_ts_out = com.unpack_pyomo(block.va_excess_ts_out, pyo.value, 1).drop(['index',1], axis = 1)
        O_infeasible_ts_out = O_infeasible_ts_out.rename(columns={0: 'node_id', 'variable value': 'infeasible_slack'})

        # Infeasible Demand
        try:
            O_slack_demand = com.unpack_pyomo(block.va_slack_demand_eor, pyo.value, 1).drop(['index',1], axis = 1)
            O_slack_demand = O_slack_demand.rename(columns={0: 'node_id', 'variable value': 'infeasible_slack'})
        except: # no CO2 EOR demand in the model
            pass

        # Infeasible Storage
        O_slack_storage = com.unpack_pyomo(block.va_slack_storage, pyo.value, 1).drop(['index',1], axis = 1)
        O_slack_storage = O_slack_storage.rename(columns={0: 'node_id', 'variable value': 'infeasible_slack'})

        return O_infeasible_supply, O_infeasible_ts_in, O_infeasible_ts_out, O_slack_demand, O_slack_storage


    def assign_flows_to_pipes(self, O_flows_df, O_pipe_bins_df, time_period):
        '''Assign flows to the correct pipeline segments.

        Parameters
        ----------

        O_flows_df : DataFrame
            Current block flows.

        O_pipe_bins_df : DataFrame
            Transportation investments.

        time_period : int
            Block number.


        Returns
        -------

        O_flows_df : DataFrame
            Current block flows assigned to pipelines.
        '''
        ### Adjust O_flows for new pipelines
        # Get existing pipelines
        temp_df = self.preproc.pipes_existing_df[['thruput_tonnes', 'node_i_id', 'node_j_id', 'pipe_segment']]
        temp_df = temp_df.rename(columns={'thruput_tonnes':'existing_thruput_tonnes'})

        # Merge existing pipelines to flow to get the difference in new pipelines
        temp_O_flows_df = O_flows_df.loc[O_flows_df['co2_volume'] > 0]
        temp_O_flows_df = temp_O_flows_df.reset_index().merge(temp_df[['existing_thruput_tonnes','node_i_id','node_j_id','pipe_segment']],
                                                how = 'left',
                                                on = ['node_i_id','node_j_id','pipe_segment']).set_index('index').fillna(0.0)
        O_flows_df.update(temp_O_flows_df)


        ### Assign flow to pipeline based on existing production
        # Get existing flow
        temp_O_flows_df['co2_volume_existing'] = temp_O_flows_df['co2_volume']
        temp_O_flows_df.loc[temp_O_flows_df['existing_thruput_tonnes'] <= 0, 'co2_volume_existing'] = 0
        temp_O_flows_df.loc[temp_O_flows_df['co2_volume'] >= temp_O_flows_df['existing_thruput_tonnes'], 'co2_volume_existing'] = temp_O_flows_df['existing_thruput_tonnes']

        if time_period == 0:
            temp_O_flows_df['co2_volume_new'] = 0
        else:
            temp_O_flows_df = temp_O_flows_df.reset_index().merge(O_pipe_bins_df[['node_i_id', 'node_j_id', 'pipe_segment', 'pipe_bin']],
                                                    how = 'left',
                                                    on = ['node_i_id','node_j_id','pipe_segment']).set_index('index')
            temp_O_flows_df['co2_volume_new'] = temp_O_flows_df['co2_volume'] * temp_O_flows_df['pipe_bin'] - temp_O_flows_df['co2_volume_existing']
            temp_O_flows_df.loc[temp_O_flows_df['co2_volume_new'] < 0, 'co2_volume_new'] = 0

        # Combine flows and update CO2 volume
        temp_O_flows_df['co2_volume'] = temp_O_flows_df['co2_volume_new'] + temp_O_flows_df['co2_volume_existing']
        O_flows_df.update(temp_O_flows_df)

        return O_flows_df


    def binding_constraints_to_df(self, block, O_flows_df, C_bind_trans_existing_df, C_bind_trans_add_df, C_bind_store_df, new_aors_df):
        '''Write Pyomo constraints to DataFrames for use in postprocessors.

        Parameters
        ----------

        block : Pyomo Block
            Pyomo block to be analyzed.

        O_flows_df: DataFrame
            Current block flows.

        C_bind_trans_existing_df : DataFrame
            Empty DataFrame.

        C_bind_trans_add_df : DataFrame
            Empty DataFrame.

        C_bind_store_df : DataFrame
            Empty DataFrame.

        new_aors_df : DataFrame
            Additional storage capacity (AORS) that has been added.


        Returns
        -------

        C_bind_trans_existing_df : DataFrame
            Binding existing transportation constraints for the current block.

        C_bind_trans_add_df : DataFrame
            Binding added transportation constraints for the current block.

        C_bind_store_df : DataFrame
            Binding storage constraints for the current block.
        '''
        ### Max capacity bind for existing pipelines
        C_bind_trans_existing_df = self.preproc.transport_add.reset_index().copy()
        C_bind_trans_existing_df = O_flows_df[['node_i_id','node_j_id','co2_volume']].merge(C_bind_trans_existing_df,
                                                                                             how = 'left',
                                                                                             on = ['node_i_id','node_j_id'])
        C_bind_trans_existing_df = C_bind_trans_existing_df.loc[C_bind_trans_existing_df['node_i_id'] != 'dummy']
        C_bind_trans_existing_df = C_bind_trans_existing_df.loc[C_bind_trans_existing_df['co2_volume'] >= (C_bind_trans_existing_df['thruput_tonnes'])]

        if C_bind_trans_existing_df.empty:
            self.logger.info('No binding constraints')
        elif self.parent.debug_switch:
            self.logger.info('Outputting binding constraints to debug folder')
            C_bind_trans_existing_df.to_csv(self.output_path + 'optimization_results//binding_constraints//' + 'max_cap_binding_trans_existing_' + str(block) + '_' + str(self.year_current) + '.csv')
        else:
            pass

        ### Max capacity bind for new pipeline additions
        C_bind_trans_add_df = self.preproc.transport_add.reset_index().copy()
        C_bind_trans_add_df = O_flows_df[['node_i_id','node_j_id','co2_volume']].merge(C_bind_trans_add_df,
                                                                                             how = 'left',
                                                                                             on = ['node_i_id','node_j_id'])
        C_bind_trans_add_df = C_bind_trans_add_df.loc[C_bind_trans_add_df['node_i_id'] != 'dummy']
        C_bind_trans_add_df = C_bind_trans_add_df.loc[C_bind_trans_add_df['co2_volume'] >= (C_bind_trans_add_df['thruput_tonnes'])]

        if C_bind_trans_add_df.empty:
            self.logger.info('No binding constraints')
        elif self.parent.debug_switch:
            self.logger.info('Outputting binding constraints to debug folder')
            C_bind_trans_add_df.to_csv(self.output_path + 'optimization_results//binding_constraints//' + 'max_cap_binding_trans_add_' + str(block) + '_' + str(self.year_current) + '.csv')
        else:
            pass

        ### Max capacity bind for storage
        C_bind_store_df = self.preproc.storage_existing_df[['node_id','inj_rate_project_tonnes_yr','storage_aors_existing']].reset_index().copy()
        C_bind_store_df = C_bind_store_df.merge(
            O_flows_df[['node_j_id', 'co2_volume']],
            how='left',
            left_on = 'node_id',
            right_on = 'node_j_id').drop(['index','node_j_id'], axis = 1)
        C_bind_store_df = C_bind_store_df.groupby(['node_id','inj_rate_project_tonnes_yr','storage_aors_existing']).sum().reset_index()
        C_bind_store_df = C_bind_store_df.merge(
            new_aors_df[['new_aors', 'node_id']],
            how='left',
            on = 'node_id')
        C_bind_store_df['total_store_aors'] = C_bind_store_df['storage_aors_existing'] + C_bind_store_df['new_aors']
        C_bind_store_df['total_store_inj'] = C_bind_store_df['total_store_aors'] * C_bind_store_df['inj_rate_project_tonnes_yr']
        C_bind_store_df = C_bind_store_df.loc[C_bind_store_df['co2_volume'] > 0]
        C_bind_store_df = C_bind_store_df.loc[C_bind_store_df['co2_volume'] >= (C_bind_store_df['total_store_inj'])]

        if C_bind_store_df.empty:
            self.logger.info('No binding constraints')
        elif self.parent.debug_switch:
            self.logger.info('Outputting binding constraints to debug folder')
            C_bind_store_df.to_csv(
                self.output_path + 'optimization_results//binding_constraints//' + 'max_cap_binding_store_' + str(
                    block) + '_' + str(self.year_current) + '.csv')
        else:
            pass


        # Return binding constraint dfs
        return C_bind_trans_existing_df, C_bind_trans_add_df, C_bind_store_df

    pass


    def duals_to_df(self):
        '''Translate dual variables to df.

        Parameters
        ----------

        None


        Returns
        -------

        self.duals_df : DataFrame
            DataFrame of dual variables generated from :class:`models.ccats_optimization.OptimizationModel`.

        self.shadow_price_df : DataFrame
            DataFrame of all shadow prices indexed by tax credit eligibility (policy_eligibility).

        self.shadow_price_div_df : DataFrame
            DataFrame of shadow prices at the census division level.

        self.shadow_price_reg_df : DataFrame
            DataFrame of shadow prices at the census region level.
        '''
        self.duals_df = pd.DataFrame(columns = ['dual_value', 'nodes', 'constraint'])

        #Instantiate duals
        duals = self.b0.component_objects(pyo.Constraint, active=True)

        self.logger.info('Get Duals')
        for c in duals:
            if c.local_name == 'c_flow_balance_supply':
                for index in c:
                    self.duals_df.loc[len(self.duals_df.index)] = [self.m.dual[c[index]], index, c]
            else:
                pass


        # Format dual constraint index
        self.duals_df['constraint'] = self.duals_df['constraint'].astype('str').str.replace('blocks[0].','')


        ### Get Shadow price
        # Instantiate shadow price df
        self.shadow_price_df = pd.DataFrame(data=0, index=list(range(1, 10)), columns=['shadow_price'])
        self.shadow_price_df['policy_eligibility'] = 0
        temp_shadow_price = self.shadow_price_df.copy()
        temp_shadow_price['policy_eligibility'] = 1
        self.shadow_price_df = pd.concat([self.shadow_price_df,temp_shadow_price],ignore_index = False)
        self.shadow_price_df = self.shadow_price_df.set_index('policy_eligibility', append = True)


        # Select only supply duals
        # Prices are calculated based on duals from supply nodes, not from demand nodes
        # Intuition is that the prices are used by CO2 suppliers in other models
        if (self.m.objective_value == 0.0) | (self.m.objective_value >= 999999999999) | (self.m.objective_value <= -999999999999) | (self.preproc.co2_supply_b0.sum() == 0):
            self.shadow_price_df = pd.DataFrame(columns = ['dual_value', 'nodes', 'constraint', 'policy_eligibility'])
        else:
            self.shadow_price_df = self.duals_df.loc[self.duals_df['constraint'] == 'c_flow_balance_supply'].copy()
            self.shadow_price_df['nodes'], self.shadow_price_df['policy_eligibility'] = zip(*self.shadow_price_df.nodes)

        ### Merge to supply set df
        self.shadow_price_df = self.shadow_price_df.merge(self.O_flows_b0_df,
                                         how = 'left',
                                         left_on = ['nodes','policy_eligibility'],
                                         right_on = ['node_i_id','eligibility_45q'])
        self.shadow_price_df = self.shadow_price_df[['dual_value','node_i_id','node_i_type','node_i_census_division', 'node_i_census_region','policy_eligibility','co2_volume']]
        self.shadow_price_df.columns = ['shadow_price','node_i_id','node_i_type','census_division','census_region','policy_eligibility','co2_volume']

		# Drop duplicates and nans
        self.shadow_price_df = self.shadow_price_df.drop_duplicates(subset = ['shadow_price','census_division','policy_eligibility','co2_volume'])
        self.shadow_price_df = self.shadow_price_df.dropna()
        self.shadow_price_df = self.shadow_price_df.loc[self.shadow_price_df['co2_volume'] > 0]

        # Drop infeasible supply prices
        self.shadow_price_df = self.shadow_price_df.loc[self.shadow_price_df['shadow_price'] > -1000]
        self.shadow_price_df = self.shadow_price_df.loc[self.shadow_price_df['shadow_price'] < 1000]

        # Drop source-to-sink prices
        mask = self.shadow_price_df['shadow_price'] != 0.0
        self.shadow_price_df = self.shadow_price_df[mask].copy()

        # Get prices
        # Switch for choosing between weighted average and marginal prices
        try:

            ### Weighted average price (default)
            if self.parent.price_average_switch:
                self.logger.info('Calculating CO2 Price - weighted average price.')

                # Census Division
                self.shadow_price_div_df = pd.DataFrame(self.shadow_price_df.groupby(['census_division', 'policy_eligibility']).apply(lambda x: np.average(x.shadow_price, weights=x.co2_volume))).copy()
                self.shadow_price_div_df.columns = ['shadow_price']

                # Census Region
                self.shadow_price_reg_df = pd.DataFrame(self.shadow_price_df.groupby(['census_region', 'policy_eligibility']).apply(lambda x: np.average(x.shadow_price, weights=x.co2_volume))).copy()
                self.shadow_price_reg_df.columns = ['shadow_price']

            ### Marginal price
            # lowest cost for transporting and storing CO2, lowest(min) selected so that all transactions would clear under this cost
            elif self.parent.price_marginal_switch:
                self.logger.info('Calculating CO2 Price - marginal price.')

                # Census Division
                self.shadow_price_div_df = pd.DataFrame(self.shadow_price_df.groupby(['census_division', 'policy_eligibility'])['shadow_price'].apply(max)).copy() # max b/c we take the inverse later

                # Census Region
                self.shadow_price_reg_df = pd.DataFrame(self.shadow_price_df.groupby(['census_region', 'policy_eligibility'])['shadow_price'].apply(max)).copy() # max b/c we take the inverse later

            else:
                self.logger.warning('No CO2 Price Method selected, check setup.csv')

            # Change signs for restart file
            self.shadow_price_div_df['shadow_price'] *= -1
            self.shadow_price_reg_df['shadow_price'] *= -1

        except:
            self.logger.warning('No CO2 Price Produced, check flows')

        # Drop 45Q ineligible shadow prices through steo years since hsm centroids don't have prices assigned to them
        if self.year_current <= self.parent.years_steo[-1]:
            try:
                self.shadow_price_div_df = self.shadow_price_div_df.loc[self.shadow_price_div_df.index.get_level_values(1) != 0]
                self.shadow_price_reg_df = self.shadow_price_reg_df.loc[self.shadow_price_reg_df.index.get_level_values(1) != 0]
            except:
                pass

        pass


    def electricity_demand_to_df(self):
        '''Calculate electricity demand and write to dedicated df by census division.

        Parameters
        ----------

        None


        Returns
        -------

        self.electric_demand : DataFrame
            Electricity consumed by census division.
        '''

        self.electric_demand = self.O_flows_b0_df.copy()
        self.electric_demand = self.electric_demand.loc[self.electric_demand['co2_volume'] > 0]
        self.electric_demand['electricity_demand'] = self.electric_demand['co2_volume'] * self.electric_demand['average_electricity']
        self.electric_demand = self.electric_demand[['electricity_demand','node_i_census_division']]
        self.electric_demand = self.electric_demand.groupby(['node_i_census_division']).sum().reset_index()

        pass




    def test_flows(self):
        '''Test if supply slack variables indicate that not all supply was allocated to demand or storage.

        If infeasibilities found:

            * :meth:`postprocessor.Postprocessor.results_to_csv`
            * :meth:`postprocessor.Postprocessor.summarize_outputs`

        Parameters
        ----------

        None


        If infeasibilities found:

            * :meth:`postprocessor.Postprocessor.results_to_csv`
            * :meth:`postprocessor.Postprocessor.summarize_outputs`

        Parameters
        ----------

        None


        Returns
        -------

        O_infeasible_supply : DataFrame
            DataFrame of supply slack variables from optimization.
        '''
        # Determine whether to log infeasibilities, and how to prefix the message
        if self.parent.scedes==None: # standalone
            log_infeasibilities = True
            warning_prefix = 'CCATS - Year ' + str(self.year_current) + ':'
        elif (int(self.parent.scedes['NRUNS']) == self.parent.cycle_current) and (self.parent.param_ncrl==1 or self.parent.param_fcrl==1): # last cycle and (reporting model iteration (NCRL) or last regular model iteration (FCRL) )
            log_infeasibilities = True
            warning_prefix = 'CCATS - Cycle==NRUNS( ' + str(self.parent.cycle_current) + '), Year ' + str(self.year_current) + ', NCRL or FCRL (Iter ' + str(self.parent.iteration_current) + '):'
        else:
            log_infeasibilities = False
            warning_prefix = ''

        # Determine infeasibility threshold based on Block 0 total CO2 supply
        temp_supply_flow_df = self.O_flows_b0_df.copy()
        temp_supply_flow_df = temp_supply_flow_df.loc[temp_supply_flow_df['pipeline_type'].isin(['source_to_storage',
                                                                                                'source_to_demand',
                                                                                                'source_to_ts_node',
                                                                                                'source_to_hsm_cent'])].copy()
        self.infeasibility_threshold = self.infeasibility_threshold_pct/100.0 * temp_supply_flow_df.co2_volume.sum()

        # Supply slack
        if (not self.O_infeasible_supply_b0_df.empty) & (self.O_infeasible_supply_b0_df['infeasible_slack'].sum() > 0.0) & (sum(self.O_infeasible_supply_b0_df['infeasible_slack'] > self.slack_threshold) > 0):
            temp_mask = self.O_infeasible_supply_b0_df['infeasible_slack'] >= self.slack_threshold
            O_supply_slack_b0_df = self.O_infeasible_supply_b0_df[temp_mask]
            self.results_to_csv()
            self.summarize_outputs()
            O_supply_slack_b0_df.to_csv(self.output_path + 'optimization_tests//' + 'infeasible_supply_b0_' + str(self.year_current) + '_' + str(self.parent.iteration_current) + '_' + str(self.parent.cycle_current) + '.csv')
            if log_infeasibilities and (self.O_infeasible_supply_b0_df['infeasible_slack'].sum() > self.infeasibility_threshold):
                self.logger.warning(warning_prefix + 'CO2 supply directed to supply slack variables in block 0. Model is infeasible. Check "optimization_tests" debug folder for slack variable results.')
        if (not self.O_infeasible_supply_b1_df.empty) & (self.O_infeasible_supply_b1_df['infeasible_slack'].sum() > 0.0) & (sum(self.O_infeasible_supply_b1_df['infeasible_slack'] > self.slack_threshold) > 0):
            mask = self.O_infeasible_supply_b1_df['infeasible_slack'] >= self.slack_threshold
            O_supply_slack_b1_df = self.O_infeasible_supply_b1_df[mask]
            self.results_to_csv()
            self.summarize_outputs()
            O_supply_slack_b1_df.to_csv(self.output_path + 'optimization_tests//' + 'infeasible_supply_b1_' + str(self.year_current) + '_' + str(self.parent.iteration_current) + '_' + str(self.parent.cycle_current) + '.csv')
            if log_infeasibilities and self.log_b1_infeasibilities and (self.O_infeasible_supply_b1_df['infeasible_slack'].sum() > self.infeasibility_threshold):
                self.logger.warning(warning_prefix + 'CO2 supply directed to supply slack variables in block 1. Model is infeasible. Check "optimization_tests" debug folder for slack variable results.')
        if (not self.O_infeasible_supply_b2_df.empty) & (self.O_infeasible_supply_b2_df['infeasible_slack'].sum() > 0.0) & (sum(self.O_infeasible_supply_b2_df['infeasible_slack'] > self.slack_threshold) > 0):
            mask = self.O_infeasible_supply_b2_df['infeasible_slack'] >= self.slack_threshold
            O_supply_slack_b2_df = self.O_infeasible_supply_b2_df[mask]
            self.results_to_csv()
            self.summarize_outputs()
            O_supply_slack_b2_df.to_csv(self.output_path + 'optimization_tests//' + 'infeasible_supply_b2_' + str(self.year_current) + '_' + str(self.parent.iteration_current) + '_' + str(self.parent.cycle_current) + '.csv')
            if log_infeasibilities and self.log_b2_infeasibilities and (self.O_infeasible_supply_b2_df['infeasible_slack'].sum() > self.infeasibility_threshold):
                self.logger.warning(warning_prefix + 'CO2 supply directed to supply slack variables in block 2. Model is infeasible. Check "optimization_tests" debug folder for slack variable results.')

        # TS in
        if (not self.O_infeasible_ts_in_b0_df.empty) & (self.O_infeasible_ts_in_b0_df['infeasible_slack'].sum() > 0.0) & (sum(self.O_infeasible_ts_in_b0_df['infeasible_slack'] > self.slack_threshold) > 0):
            temp_mask = self.O_infeasible_ts_in_b0_df['infeasible_slack'] >= self.slack_threshold
            O_supply_ts_in_b0_df = self.O_infeasible_ts_in_b0_df[temp_mask]
            self.results_to_csv()
            self.summarize_outputs()
            O_supply_ts_in_b0_df.to_csv(self.output_path + 'optimization_tests//' + 'infeasible_ts_in_b0_' + str(self.year_current) + '_' + str(self.parent.iteration_current) + '_' + str(self.parent.cycle_current) + '.csv')
            if log_infeasibilities and (self.O_infeasible_ts_in_b0_df['infeasible_slack'].sum() > self.infeasibility_threshold):
                self.logger.warning(warning_prefix + 'CO2 supply directed to ts in slack variables in block 0. Model is infeasible. Check "optimization_tests" debug folder for slack variable results.')
        if (not self.O_infeasible_ts_in_b1_df.empty) & (self.O_infeasible_ts_in_b1_df['infeasible_slack'].sum() > 0.0) & (sum(self.O_infeasible_ts_in_b1_df['infeasible_slack'] > self.slack_threshold) > 0):
            mask = self.O_infeasible_ts_in_b1_df['infeasible_slack'] >= self.slack_threshold
            O_infeasible_ts_in_b1_df = self.O_infeasible_ts_in_b1_df[mask]
            self.results_to_csv()
            self.summarize_outputs()
            O_infeasible_ts_in_b1_df.to_csv(self.output_path + 'optimization_tests//' + 'infeasible_ts_in_b1_' + str(self.year_current) + '_' + str(self.parent.iteration_current) + '_' + str(self.parent.cycle_current) + '.csv')
            if log_infeasibilities and self.log_b1_infeasibilities and (self.O_infeasible_ts_in_b1_df['infeasible_slack'].sum() > self.infeasibility_threshold):
                self.logger.warning(warning_prefix + 'CO2 supply directed to ts in slack variables in block 1. Model is infeasible. Check "optimization_tests" debug folder for slack variable results.')
        if (not self.O_infeasible_ts_in_b2_df.empty) & (self.O_infeasible_ts_in_b2_df['infeasible_slack'].sum() > 0.0) & (sum(self.O_infeasible_ts_in_b2_df['infeasible_slack'] > self.slack_threshold) > 0):
            mask = self.O_infeasible_ts_in_b2_df['infeasible_slack'] >= self.slack_threshold
            O_infeasible_ts_in_b2_df = self.O_infeasible_ts_in_b2_df[mask]
            self.results_to_csv()
            self.summarize_outputs()
            O_infeasible_ts_in_b2_df.to_csv(self.output_path + 'optimization_tests//' + 'infeasible_ts_in_b2_' + str(self.year_current) + '_' + str(self.parent.iteration_current) + '_' + str(self.parent.cycle_current) + '.csv')
            if log_infeasibilities and self.log_b2_infeasibilities and (self.O_infeasible_ts_in_b2_df['infeasible_slack'].sum() > self.infeasibility_threshold):
                self.logger.warning(warning_prefix + 'CO2 supply directed to ts in slack variables in block 2. Model is infeasible. Check "optimization_tests" debug folder for slack variable results.')

        # TS out
        if (not self.O_infeasible_ts_out_b0_df.empty) & (self.O_infeasible_ts_out_b0_df['infeasible_slack'].sum() > 0.0) & (sum(self.O_infeasible_ts_out_b0_df['infeasible_slack'] > self.slack_threshold) > 0):
            temp_mask = self.O_infeasible_ts_out_b0_df['infeasible_slack'] >= self.slack_threshold
            O_supply_ts_in_b0_df = self.O_infeasible_ts_out_b0_df[temp_mask]
            self.results_to_csv()
            self.summarize_outputs()
            O_supply_ts_in_b0_df.to_csv(self.output_path + 'optimization_tests//' + 'infeasible_ts_out_b0_' + str(self.year_current) + '_' + str(self.parent.iteration_current) + '_' + str(self.parent.cycle_current) + '.csv')
            if log_infeasibilities and (self.O_infeasible_ts_out_b0_df['infeasible_slack'].sum() > self.infeasibility_threshold):
                self.logger.warning(warning_prefix + 'CO2 supply directed to ts out slack variables in block 0. Model is infeasible. Check "optimization_tests" debug folder for slack variable results.')
        if (not self.O_infeasible_ts_out_b1_df.empty) & (self.O_infeasible_ts_out_b1_df['infeasible_slack'].sum() > 0.0) & (sum(self.O_infeasible_ts_out_b1_df['infeasible_slack'] > self.slack_threshold) > 0):
            mask = self.O_infeasible_ts_out_b1_df['infeasible_slack'] >= self.slack_threshold
            O_infeasible_ts_out_b1_df = self.O_infeasible_ts_out_b1_df[mask]
            self.results_to_csv()
            self.summarize_outputs()
            O_infeasible_ts_out_b1_df.to_csv(self.output_path + 'optimization_tests//' + 'infeasible_ts_out_b1_' + str(self.year_current) + '_' + str(self.parent.iteration_current) + '_' + str(self.parent.cycle_current) + '.csv')
            if log_infeasibilities and self.log_b1_infeasibilities and (self.O_infeasible_ts_out_b1_df['infeasible_slack'].sum() > self.infeasibility_threshold):
                self.logger.warning(warning_prefix + 'CO2 supply directed to ts out slack variables in block 1. Model is infeasible. Check "optimization_tests" debug folder for slack variable results.')
        if (not self.O_infeasible_ts_out_b2_df.empty) & (self.O_infeasible_ts_out_b2_df['infeasible_slack'].sum() > 0.0) & (sum(self.O_infeasible_ts_out_b2_df['infeasible_slack'] > self.slack_threshold) > 0):
            mask = self.O_infeasible_ts_out_b2_df['infeasible_slack'] >= self.slack_threshold
            O_infeasible_ts_out_b2_df = self.O_infeasible_ts_out_b2_df[mask]
            self.results_to_csv()
            self.summarize_outputs()
            O_infeasible_ts_out_b2_df.to_csv(self.output_path + 'optimization_tests//' + 'infeasible_ts_out_b2_' + str(self.year_current) + '_' + str(self.parent.iteration_current) + '_' + str(self.parent.cycle_current) + '.csv')
            if log_infeasibilities and self.log_b2_infeasibilities and (self.O_infeasible_ts_out_b2_df['infeasible_slack'].sum() > self.infeasibility_threshold):
                self.logger.warning(warning_prefix + 'CO2 supply directed to ts out slack variables in block 2. Model is infeasible. Check "optimization_tests" debug folder for slack variable results.')

        else:
            pass

        pass


    def update_existing_pipeline_network(self):
        '''Get capacity added for new pipelines and write to DataFrame to be passed between model years.

        * Use block 0, because those are the investments selected in the curent model year (first available to use in block 1)

        Parameters
        ----------

        None


        Returns
        -------

        self.parent.new_built_pipes_df : DataFrame
            DataFrame of pipeline capacity added.
        '''
        self.parent.new_built_pipes_df = self.O_transport_cap_add_b0_df.copy()
        self.parent.new_built_pipes_df = self.parent.new_built_pipes_df.loc[self.parent.new_built_pipes_df['thruput_tonnes'] > 0]

        pass


    def update_storage_volumes(self):
        '''Update realized storage volumes passed between model years.

        * Use block 0 since it is investment block.

        Parameters
        ----------

        None


        Returns
        -------
        
        self.parent.new_aors_df : DataFrame
            Additional storage capacity (AORs) added.
        
        self.parent.store_prev_b0_df : DataFrame
            Track amount of CO\ :sub:`2` stored.
        '''
        # Get new AORS
        new_aors_df = self.O_store_new_aors_b0_df[['node_id','new_aors']].copy()
        new_aors_df = new_aors_df.merge(self.preproc.storage_existing_df[['node_id','max_inj_projects','storage_aors_existing']],
                                        how = 'left',
                                        on = 'node_id')

        # If new + existing AORs greater than max AORs (optimization can produce results marginally outside of constraint), set as max AORs
        new_aors_df['total_aors'] = new_aors_df['new_aors'] + new_aors_df['storage_aors_existing']
        new_aors_df.loc[new_aors_df['total_aors'] > new_aors_df['max_inj_projects'], 'new_aors'] = new_aors_df['max_inj_projects'] - new_aors_df['storage_aors_existing']
        new_aors_df = new_aors_df.drop(['storage_aors_existing','total_aors'], axis = 1)
        self.parent.new_aors_df = new_aors_df.copy()

        # Get previous year storage volumes
        store_prev_df = self.O_flows_b0_df[['node_j_id','co2_volume','node_j_type']].copy()
        store_prev_df = store_prev_df.loc[store_prev_df['node_j_type'] == 'storage']
        store_prev_df = store_prev_df.rename(columns = {'node_j_id':'node_id', 'co2_volume':'co2_stored'})
        store_prev_df = store_prev_df.drop(['node_j_type'], axis = 1)
        store_prev_df = store_prev_df.groupby('node_id').sum().reset_index()
        self.parent.store_prev_b0_df = store_prev_df.copy()

        pass


    def write_restart_file(self):
        '''Write results to local restart variable.

        * Results are based on block 0

        Results use the following indices:

        * Supply

            * 1 = PP Coal
            * 2 = PP Natgas
            * 3 = BECCS
            * 4 = Natural Gas Processing
            * 5 = Cement
            * 6 = Ethanol
            * 7 = Hydrogen
            * 8 = Other
            * 9 = Total

        * Sequestration
            
            * 1 = CO\ :sub:`2` EOR
            * 2 = Storage

        Parameters
        ----------

        None


        Returns
        -------
        self.parent.rest_co2_sup_out : DataFrame
            Supplied CO\ :sub:`2` by census division

        self.parent.rest_co2_seq_out: DataFrame
            Sequestered CO\ :sub:`2` by census division

        self.parent.rest_co2_sup_out_r : DataFrame
            Supplied CO\ :sub:`2` by census region

        self.parent.rest_co2_seq_out_r : DataFrame
            Sequestered CO\ :sub:`2` by census region

        self.parent.rest_co2_prc_dis_45q : DataFrame
            Price of CO\ :sub:`2` 45Q eligible by census district

        self.parent.rest_co2_prc_dis_ntc : DataFrame
            Price of CO\ :sub:`2` 45Q ineligible by census district

        self.parent.rest_co2_prc_reg_45q : DataFrame
            Price of CO\ :sub:`2` 45Q eligible by census region

        self.parent.rest_co2_prc_reg_ntc : DataFrame
            Price of CO\ :sub:`2` 45Q ineligible by census region

        self.parent.rest_co2_elec : DataFrame
            Electricity consumption by census division
        '''
        ### CO2 Supply Output - Census Division
        temp_supply_flow_df = self.O_flows_b0_df.copy()
        temp_supply_flow_df = temp_supply_flow_df.loc[temp_supply_flow_df['pipeline_type'].isin(['source_to_storage',
                                                                                                'source_to_demand',
                                                                                                'source_to_ts_node',
                                                                                                'source_to_hsm_cent'])].copy()

        # Map supply types to restart file variable index
        temp_supply_flow_df['co2_sup_out_index'] = temp_supply_flow_df['node_i_type'].map(self.parent.co2_supply_index)
        
        # Group and format for restart file
        temp_supply_flow_df = temp_supply_flow_df[['co2_volume','co2_sup_out_index','node_i_census_division']]
        temp_supply_flow_df = temp_supply_flow_df.groupby(['node_i_census_division','co2_sup_out_index']).sum()
        temp_supply_flow_df['year'] = int(self.year_current)
        temp_supply_flow_df = temp_supply_flow_df.set_index('year', append = True)
        temp_supply_flow_df.columns = ['value']
        temp_supply_flow_df.index.names = self.parent.rest_co2_sup_out.index.names
        
        # Update restart file
        self.parent.rest_co2_sup_out.update(temp_supply_flow_df)
        
        # Get sum by type/census division
        temp = self.parent.rest_co2_sup_out.copy()
        temp = temp[temp.index.isin([1,2,3,4,5,6,7,8,9], level = 0)]
        temp = temp[temp.index.isin([int(self.year_current)], level = 2)]
        temp = temp.groupby(level = [1,2]).sum()
        temp['census_div'] = 11
        temp = temp.set_index('census_div', append = True)
        temp = temp.reorder_levels([2,0,1])
        temp.index.names = self.parent.rest_co2_sup_out.index.names
        self.parent.rest_co2_sup_out.update(temp)


        ### CO2 Sequestration Output - Census Division
        temp_seq_flow_df = self.O_flows_b0_df.copy()
        temp_seq_flow_df = temp_seq_flow_df.loc[temp_seq_flow_df['pipeline_type'].isin(['source_to_storage',
                                                                                        'source_to_demand',
                                                                                        'source_to_hsm_cent',
                                                                                        'ts_node_to_storage',
                                                                                        'ts_node_to_demand'])].copy()

        # Map supply types to restart file variable index
        temp_seq_flow_df['co2_seq_out_index'] = temp_seq_flow_df['node_j_type'].map(self.parent.co2_seq_index)

        # Group and format for restart file
        temp_seq_flow_df = temp_seq_flow_df[['co2_volume', 'co2_seq_out_index', 'node_j_census_division']]
        temp_seq_flow_df = temp_seq_flow_df.groupby(['node_j_census_division', 'co2_seq_out_index']).sum()
        temp_seq_flow_df['year'] = int(self.year_current)
        temp_seq_flow_df = temp_seq_flow_df.set_index('year', append=True)
        temp_seq_flow_df.columns = ['value']
        temp_seq_flow_df.index.names = self.parent.rest_co2_seq_out.index.names

        # Update restart file
        self.parent.rest_co2_seq_out.update(temp_seq_flow_df)

        # Get sum by type/census division
        temp = self.parent.rest_co2_seq_out.copy()
        temp = temp[temp.index.isin([1,2,3,4,5,6,7,8,9], level = 0)]
        temp = temp[temp.index.isin([int(self.year_current)], level = 2)]
        temp = temp.groupby(level = [1,2]).sum()
        temp['census_div'] = 11
        temp = temp.set_index('census_div', append = True)
        temp = temp.reorder_levels([2,0,1])
        temp.index.names = self.parent.rest_co2_seq_out.index.names
        self.parent.rest_co2_seq_out.update(temp)


        ### CO2 Supply Output - Census Region
        temp_supply_flow_df = self.O_flows_b0_df.copy()
        temp_supply_flow_df = temp_supply_flow_df.loc[temp_supply_flow_df['pipeline_type'].isin(['source_to_storage',
                                                                                                 'source_to_demand',
                                                                                                 'source_to_ts_node',
                                                                                                 'source_to_hsm_cent'])].copy()

        # Map supply types to restart file variable index
        temp_supply_flow_df['co2_sup_out_index'] = temp_supply_flow_df['node_i_type'].map(self.parent.co2_supply_index)

        # Group and format for restart file
        temp_supply_flow_df = temp_supply_flow_df[['co2_volume', 'co2_sup_out_index', 'node_i_census_region']]
        temp_supply_flow_df = temp_supply_flow_df.groupby(['node_i_census_region', 'co2_sup_out_index']).sum()
        temp_supply_flow_df['year'] = int(self.year_current)
        temp_supply_flow_df = temp_supply_flow_df.set_index('year', append=True)
        temp_supply_flow_df.columns = ['value']
        temp_supply_flow_df.index.names = self.parent.rest_co2_sup_out_r.index.names

        # Update restart file
        self.parent.rest_co2_sup_out_r.update(temp_supply_flow_df)

        # Get sum by type/census region
        temp = self.parent.rest_co2_sup_out_r.copy()
        temp = temp[temp.index.isin([1,2,3,4], level = 0)]
        temp = temp[temp.index.isin([int(self.year_current)], level = 2)]
        temp = temp.groupby(level = [1,2]).sum()
        temp['census_reg'] = 5
        temp = temp.set_index('census_reg', append = True)
        temp = temp.reorder_levels([2,0,1])
        temp.index.names = self.parent.rest_co2_sup_out_r.index.names
        self.parent.rest_co2_sup_out_r.update(temp)


        ### CO2 Sequestration Output - Census Region
        temp_seq_flow_df = self.O_flows_b0_df.copy()
        temp_seq_flow_df = temp_seq_flow_df.loc[temp_seq_flow_df['pipeline_type'].isin(['source_to_storage',
                                                                                        'source_to_demand',
                                                                                        'source_to_hsm_cent',
                                                                                        'ts_node_to_storage',
                                                                                        'ts_node_to_demand'])].copy()

        # Map supply types to restart file variable index
        temp_seq_flow_df['co2_seq_out_index'] = temp_seq_flow_df['node_j_type'].map(self.parent.co2_seq_index)

        # Group and format for restart file
        temp_seq_flow_df = temp_seq_flow_df[['co2_volume', 'co2_seq_out_index', 'node_j_census_region']]
        temp_seq_flow_df = temp_seq_flow_df.groupby(['node_j_census_region', 'co2_seq_out_index']).sum()
        temp_seq_flow_df['year'] = int(self.year_current)
        temp_seq_flow_df = temp_seq_flow_df.set_index('year', append=True)
        temp_seq_flow_df.columns = ['value']
        temp_seq_flow_df.index.names = self.parent.rest_co2_seq_out_r.index.names

        # Update restart file
        self.parent.rest_co2_seq_out_r.update(temp_seq_flow_df)

        # Get sum by type/census division
        temp = self.parent.rest_co2_seq_out_r.copy()
        temp = temp[temp.index.isin([1,2,3,4], level = 0)]
        temp = temp[temp.index.isin([int(self.year_current)], level = 2)]
        temp = temp.groupby(level = [1,2]).sum()
        temp['census_reg'] = 5
        temp = temp.set_index('census_reg', append = True)
        temp = temp.reorder_levels([2,0,1])
        temp.index.names = self.parent.rest_co2_seq_out_r.index.names
        self.parent.rest_co2_seq_out_r.update(temp)


        ### Check for problems with prices before writing to restart file
        if self.shadow_price_div_df.empty:
            self.logger.warning(str(self.year_current) + ': No shadow prices, not updating prices in restart file')
        
        elif (all(i > self.price_limit for i in self.shadow_price_div_df['shadow_price'].tolist())) | (all(i < -1*self.price_limit for i in self.shadow_price_div_df['shadow_price'].tolist())):
            self.logger.warning(str(self.year_current) + ': Shadow prices exceed limit (+/-'+ str(self.price_limit) + '), not updating prices in restart file. There may be an infeasibility in the model. Change limit with postproc_setup.price_limit')
            # Save self.shadow_price_div_df, to help identify the issue
            self.shadow_price_div_df.to_csv(self.output_path + 'optimization_tests//' + 'shadow_price_div_df_' + str(self.year_current) + '_' + str(self.parent.cycle_current) + '.csv')
        
        else:
            ### CO2 45Q Price Output - Census Division
            # Format for restart file
            temp_shadow_price = self.shadow_price_div_df.loc[self.shadow_price_div_df.index.get_level_values(1) == 1].copy()
            temp_shadow_price.index = temp_shadow_price.index.droplevel(1)
            temp_shadow_price = temp_shadow_price[['shadow_price']].copy()
            temp_shadow_price = temp_shadow_price.reindex(list(range(1, 10)), fill_value=np.nan)
            temp_shadow_price['year'] = int(self.year_current)
            temp_shadow_price = temp_shadow_price.set_index(['year'], append=True)
            temp_shadow_price.columns = ['value_new']
            temp_shadow_price.index.names = self.parent.rest_co2_prc_dis_45q.index.names

            # Take current year price as an average of the prior 3 model years
            if int(self.year_current) >= self.parent.years_steo[-1]:
                # Get averaging years
                years = list(range(self.year_current -2,self.year_current))
                temp_restart_price = self.parent.rest_co2_prc_dis_45q.loc[self.parent.rest_co2_prc_dis_45q.index.get_level_values(1).isin(years)].copy()
                temp_restart_price = temp_restart_price.unstack()
                temp_restart_price.columns = years
                temp_restart_price = temp_restart_price.loc[temp_restart_price.index <= 9]
                temp_restart_price.index = temp_shadow_price.index

                # Apply average
                temp_shadow_price[years] = temp_restart_price[years]
                temp_shadow_price = temp_shadow_price.dropna(axis = 0)
                temp_shadow_price['value_new'] = temp_shadow_price.mean(axis = 1)
                temp_shadow_price = temp_shadow_price[['value_new']]

            # Limit price movements to price peg
            temp_shadow_price = temp_shadow_price.merge(self.parent.rest_co2_prc_dis_45q, how = 'left',
                                                        left_index = True,
                                                        right_index = True)
            temp_shadow_price.loc[temp_shadow_price['value_new'] > (temp_shadow_price['value'] + self.price_peg), 'value_new'] = (temp_shadow_price['value'] + self.price_peg)
            temp_shadow_price.loc[temp_shadow_price['value_new'] < (temp_shadow_price['value'] - self.price_peg), 'value_new'] = (temp_shadow_price['value'] - self.price_peg)
            temp_shadow_price = temp_shadow_price.drop(['value'], axis = 1)
            temp_shadow_price.columns = self.parent.rest_co2_prc_dis_45q.columns


            # Update restart file
            self.parent.rest_co2_prc_dis_45q.update(temp_shadow_price)

            # Average price
            temp = self.parent.rest_co2_prc_dis_45q.copy()
            temp = temp[temp.index.isin([1, 2, 3, 4, 5, 6, 7, 8, 9], level=0)]
            temp = temp[temp.index.isin([int(self.year_current)], level=1)]
            temp = temp.groupby(level=[1]).mean()
            temp['census_div'] = 11
            temp = temp.set_index('census_div', append=True)
            temp = temp.reorder_levels([1, 0])
            temp.index.names = self.parent.rest_co2_prc_dis_45q.index.names
            self.parent.rest_co2_prc_dis_45q.update(temp)


            ### CO2 NTC Price Output - Census Division
            # Format for restart file
            temp_shadow_price = self.shadow_price_div_df.loc[self.shadow_price_div_df.index.get_level_values(1) == 0].copy()
            temp_shadow_price.index = temp_shadow_price.index.droplevel(1)
            temp_shadow_price = temp_shadow_price[['shadow_price']].copy()
            temp_shadow_price = temp_shadow_price.reindex(list(range(1, 10)), fill_value=np.nan)
            temp_shadow_price['year'] = int(self.year_current)
            temp_shadow_price = temp_shadow_price.set_index(['year'], append=True)
            temp_shadow_price.columns = ['value_new']
            temp_shadow_price.index.names = self.parent.rest_co2_prc_dis_ntc.index.names

            # Take current year price as an average of the prior 3 model years
            if int(self.year_current) >= self.parent.years_steo[-1]:
                # Get averaging years
                years = list(range(self.year_current -2,self.year_current))
                temp_restart_price = self.parent.rest_co2_prc_dis_ntc.loc[self.parent.rest_co2_prc_dis_ntc.index.get_level_values(1).isin(years)].copy()
                temp_restart_price = temp_restart_price.unstack()
                temp_restart_price.columns = years
                temp_restart_price = temp_restart_price.loc[temp_restart_price.index <= 9]
                temp_restart_price.index = temp_shadow_price.index

                # Apply average
                temp_shadow_price[years] = temp_restart_price[years]
                temp_shadow_price = temp_shadow_price.dropna(axis = 0)
                temp_shadow_price['value_new'] = temp_shadow_price.mean(axis = 1)
                temp_shadow_price = temp_shadow_price[['value_new']]

            # Limit price movements to $5
            temp_shadow_price = temp_shadow_price.merge(self.parent.rest_co2_prc_dis_ntc, how = 'left',
                                                        left_index = True,
                                                        right_index = True)
            temp_shadow_price.loc[temp_shadow_price['value_new'] > (temp_shadow_price['value'] + self.price_peg), 'value_new'] = (temp_shadow_price['value'] + self.price_peg)
            temp_shadow_price.loc[temp_shadow_price['value_new'] < (temp_shadow_price['value'] - self.price_peg), 'value_new'] = (temp_shadow_price['value'] - self.price_peg)
            temp_shadow_price = temp_shadow_price.drop(['value'], axis = 1)
            temp_shadow_price.columns = self.parent.rest_co2_prc_dis_ntc.columns

            # Update restart file
            self.parent.rest_co2_prc_dis_ntc.update(temp_shadow_price)

            # Average price
            temp = self.parent.rest_co2_prc_dis_ntc.copy()
            temp = temp[temp.index.isin([1, 2, 3, 4, 5, 6, 7, 8, 9], level=0)]
            temp = temp[temp.index.isin([int(self.year_current)], level=1)]
            temp = temp.groupby(level=[1]).mean()
            temp['census_div'] = 11
            temp = temp.set_index('census_div', append=True)
            temp = temp.reorder_levels([1, 0])
            temp.index.names = self.parent.rest_co2_prc_dis_ntc.index.names
            self.parent.rest_co2_prc_dis_ntc.update(temp)


            ### CO2 45Q Price Output - Census Region
            # Format for restart file
            temp_shadow_price = self.shadow_price_reg_df.loc[self.shadow_price_reg_df.index.get_level_values(1) == 1].copy()
            temp_shadow_price.index = temp_shadow_price.index.droplevel(1)
            temp_shadow_price = temp_shadow_price[['shadow_price']].copy()
            temp_shadow_price = temp_shadow_price.reindex(list(range(1, 5)), fill_value=np.nan)
            temp_shadow_price['year'] = int(self.year_current)
            temp_shadow_price = temp_shadow_price.set_index(['year'], append=True)
            temp_shadow_price.columns = ['value_new']
            temp_shadow_price.index.names = self.parent.rest_co2_prc_reg_45q.index.names

            # Take current year price as an average of the prior 3 model years
            if int(self.year_current) >= self.parent.years_steo[-1]:
                # Get averaging years
                years = list(range(self.year_current -2,self.year_current))
                temp_restart_price = self.parent.rest_co2_prc_reg_45q.loc[self.parent.rest_co2_prc_reg_45q.index.get_level_values(1).isin(years)].copy()
                temp_restart_price = temp_restart_price.unstack()
                temp_restart_price.columns = years
                temp_restart_price = temp_restart_price.loc[temp_restart_price.index <= 9]
                temp_restart_price.index = temp_shadow_price.index

                # Apply average
                temp_shadow_price[years] = temp_restart_price[years]
                temp_shadow_price = temp_shadow_price.dropna(axis = 0)
                temp_shadow_price['value_new'] = temp_shadow_price.mean(axis = 1)
                temp_shadow_price = temp_shadow_price[['value_new']]

            # Limit price movements to price peg
            temp_shadow_price = temp_shadow_price.merge(self.parent.rest_co2_prc_reg_45q, how = 'left',
                                                        left_index = True,
                                                        right_index = True)
            temp_shadow_price.loc[temp_shadow_price['value_new'] > (temp_shadow_price['value'] + self.price_peg), 'value_new'] = (temp_shadow_price['value'] + 5)
            temp_shadow_price.loc[temp_shadow_price['value_new'] < (temp_shadow_price['value'] - self.price_peg), 'value_new'] = (temp_shadow_price['value'] - 5)
            temp_shadow_price = temp_shadow_price.drop(['value'], axis = 1)
            temp_shadow_price.columns = self.parent.rest_co2_prc_reg_45q.columns

            # Update restart file
            self.parent.rest_co2_prc_reg_45q.update(temp_shadow_price)


            ### CO2 NTC Price Output - Census Region
            # Format for restart file
            temp_shadow_price = self.shadow_price_reg_df.loc[self.shadow_price_reg_df.index.get_level_values(1) == 0].copy()
            temp_shadow_price.index = temp_shadow_price.index.droplevel(1)
            temp_shadow_price = temp_shadow_price[['shadow_price']].copy()
            temp_shadow_price = temp_shadow_price.reindex(list(range(1, 5)), fill_value=np.nan)
            temp_shadow_price['year'] = int(self.year_current)
            temp_shadow_price = temp_shadow_price.set_index(['year'], append=True)
            temp_shadow_price.columns = ['value_new']
            temp_shadow_price.index.names = self.parent.rest_co2_prc_reg_ntc.index.names

            # Take current year price as an average of the prior 3 model years
            if int(self.year_current) >= self.parent.years_steo[-1]:
                # Get averaging years
                years = list(range(self.year_current -2,self.year_current))
                temp_restart_price = self.parent.rest_co2_prc_reg_ntc.loc[self.parent.rest_co2_prc_reg_ntc.index.get_level_values(1).isin(years)].copy()
                temp_restart_price = temp_restart_price.unstack()
                temp_restart_price.columns = years
                temp_restart_price = temp_restart_price.loc[temp_restart_price.index <= 9]
                temp_restart_price.index = temp_shadow_price.index

                # Apply average
                temp_shadow_price[years] = temp_restart_price[years]
                temp_shadow_price = temp_shadow_price.dropna(axis = 0)
                temp_shadow_price['value_new'] = temp_shadow_price.mean(axis = 1)
                temp_shadow_price = temp_shadow_price[['value_new']]

            # Limit price movements to price peg
            temp_shadow_price = temp_shadow_price.merge(self.parent.rest_co2_prc_reg_ntc, how = 'left',
                                                        left_index = True,
                                                        right_index = True)
            temp_shadow_price.loc[temp_shadow_price['value_new'] > (temp_shadow_price['value'] + self.price_peg), 'value_new'] = (temp_shadow_price['value'] + 5)
            temp_shadow_price.loc[temp_shadow_price['value_new'] < (temp_shadow_price['value'] - self.price_peg), 'value_new'] = (temp_shadow_price['value'] - 5)
            temp_shadow_price = temp_shadow_price.drop(['value'], axis = 1)
            temp_shadow_price.columns = self.parent.rest_co2_prc_reg_ntc.columns

            # Update restart file
            self.parent.rest_co2_prc_reg_ntc.update(temp_shadow_price)

        
        ### Fill in restart variables
        temp_electricity = self.electric_demand.copy()
        temp_electricity['year'] = int(self.year_current)
        temp_electricity = temp_electricity.set_index(['node_i_census_division','year'])
        temp_electricity.index.names = self.parent.rest_co2_elec.index.names
        temp_electricity.columns = ['value']
        self.parent.rest_co2_elec.update(temp_electricity)

        # Get sum by type/census division
        temp = self.parent.rest_co2_elec.copy()
        temp = temp[temp.index.isin([1,2,3,4,5,6,7,8,9], level = 0)]
        temp = temp[temp.index.isin([int(self.year_current)], level = 1)]
        temp = temp.groupby(level = [1]).sum()
        temp['census_div'] = 11
        temp = temp.set_index('census_div', append = True)
        temp = temp.reorder_levels([1,0])
        temp.index.names = self.parent.rest_co2_elec.index.names
        self.parent.rest_co2_elec.update(temp)

        if int(self.year_current) == self.parent.year_start:
            # Write stats output
            temp_electricity.to_csv(self.output_path + 'electricity_demand\\' + 'electricity_demand.csv', mode='w')
        else:
            # Write stats output
            temp_electricity.to_csv(self.output_path + 'electricity_demand\\' + 'electricity_demand.csv', mode='a', header = False)

        pass


    def copy_variables_for_pytest(self):
        '''Copy flow variables for Pytest.

        * Pytest is used for results testing (i.e. confirming flows in the model don't exceed flows reported via the restart variable)

        Parameters
        ----------

        None


        Returns
        -------

        self.parent.pytest.O_flows_b0_df : DataFrame
            Block 0 flows copied for pytest.

        self.parent.pytest.O_flows_b1_df : DataFrame
            Block 1 flows copied for pytest.

        self.parent.pytest.O_infeasible_supply_b0_df : DataFrame
            Copy of block 0 infeasible supply df for pytest.

        self.parent.pytest.O_infeasible_supply_b1_df : DataFrame
            Copy of block 1 infeasible supply df for pytest.

        self.parent.pytest.O_infeasible_supply_b2_df : DataFrame
            Copy of block 2 infeasible supply df for pytest.

        self.parent.pytest.transport_add : DataFrame
            Copy of transport capacity add df for pytest.

        self.parent.pytest.transport_existing : DataFrame
            Copy of transport capacity existing df for pytest.

        self.parent.pytest.year_start : int
            Copy of start year for pytest.

        self.parent.pytest.year_current : int
            Copy of current year for pytest.

        self.parent.pytest.iteration_current : int
            Copy of current iteration for pytest.

        self.parent.pytest.rest_co2_sup_out : DataFrame
            Supplied CO\ :sub:`2` by census division

        self.parent.pytest.rest_co2_sup_out_r : DataFrame
            Sequestered CO\ :sub:`2` by census division
        '''
        # Postprocessor Variables
        self.parent.pytest.O_flows_b0_df = self.O_flows_b0_df.copy()
        self.parent.pytest.O_flows_b1_df = self.O_flows_b1_df.copy()
        self.parent.pytest.O_flows_b2_df = self.O_flows_b2_df.copy()
        self.parent.pytest.O_infeasible_supply_b0_df = self.O_infeasible_supply_b0_df.copy()
        self.parent.pytest.O_infeasible_supply_b1_df = self.O_infeasible_supply_b1_df.copy()
        self.parent.pytest.O_infeasible_supply_b2_df = self.O_infeasible_supply_b1_df.copy()
        self.parent.pytest.transport_add = self.preproc.transport_add.copy()
        self.parent.pytest.transport_existing = self.preproc.transport_existing.copy()
        self.parent.pytest.year_start = self.parent.year_start
        self.parent.pytest.year_current = self.parent.year_current
        self.parent.pytest.iteration_current = self.parent.iteration_current

        # Restart Variables
        self.parent.pytest.rest_co2_sup_out = self.parent.rest_co2_sup_out
        self.parent.pytest.rest_co2_sup_out_r = self.parent.rest_co2_sup_out_r

        pass


    def write_pkl(self):
        '''Write local variables that need to be passed between model iterations to pickle.

        Parameters
        ----------

        None


        Returns
        -------

        self.parent.pkl.mod_new_aors_df : DataFrame
            AORS opened during Block 0.
        
        self.parent.pkl.mod_store_prev_b0_df : DataFrame
            Previous block 0 storage.
        
        self.parent.pkl.mod_new_built_pipes_df : DataFrame
            New built pipelines.
        '''
        self.parent.pkl.mod_new_aors_df             = self.parent.new_aors_df.copy()
        self.parent.pkl.mod_store_prev_b0_df        = self.parent.store_prev_b0_df.copy()
        self.parent.pkl.mod_new_built_pipes_df      = self.parent.new_built_pipes_df.copy()

        pass


    def visualize_results(self,O_flows_input,block):
        '''Visualize optimization results.

        Parameters
        ----------

        O_flows_input: DataFrame
            Flows from the current block to be visualized.
            
        block : String
            Name of block.

        
        Returns
        -------

        None

        '''
        # Get total node and arc options for block
        supply_nodes_total = len(self.preproc.nodes_supply)
        demand_nodes_total = len(self.preproc.nodes_demand)
        arcs_total         = len(self.preproc.arcs)
        postproc_data = {
            'year' : [self.year_current],
            '# of supply node options': [supply_nodes_total],
            '# of demand node options': [demand_nodes_total],
            'number of arc options': [arcs_total],
        }
        if not self.postproc_df.empty and block!='b2': 
            number_of_supply_nodes_selected = round(self.postproc_df[f'number of supply nodes selected {block}'][0], 2)
            number_of_demand_nodes_selected = round(self.postproc_df[f'number of demand nodes selected {block}'][0], 2)
            number_of_arcs_selected = round(self.postproc_df[f'number of arcs selected {block}'][0], 2)
            CO2_volume_at_supply_nodes = round(self.postproc_df[f'CO2 volume at supply nodes {block}'][0], 2)
            CO2_volume_at_demand_nodes = round(self.postproc_df[f'CO2 volume at demand nodes {block}'][0], 2)
            total_CO2_volume_transported = round(self.postproc_df[f'total CO2 volume transported {block}'][0], 2)
            total_cost =  round(self.m.objective_value,2)

            postproc_data[f'# supply nodes selected {block}'] =  number_of_supply_nodes_selected
            postproc_data[f'# demand nodes selected {block}'] = number_of_demand_nodes_selected
            postproc_data[f'# arcs selected {block}'] = number_of_arcs_selected
            postproc_data[f'CO2 volume at supply nodes {block}'] = CO2_volume_at_supply_nodes
            postproc_data[f'CO2 volume at demand nodes {block}'] = CO2_volume_at_demand_nodes
            postproc_data[f'total CO2 volume transported {block}'] = total_CO2_volume_transported
            postproc_data['total_cost']  = total_cost
        postproc_df = pd.DataFrame(postproc_data)
            
  
        # Visualize block
        vis_folder = self.output_path + 'visualization_results/' + self.m.name
        with open(vis_folder + '//map.run', 'w') as file:
            file.write('run folder:' + vis_folder)

        O_flows_df = O_flows_input[['node_i_id',
                            'node_i_type',
                            'i_longitude',
                            'i_latitude',
                            'node_j_id',
                            'node_j_type',
                            'j_longitude',
                            'j_latitude',
                            'co2_volume',
                            'route_id',
                            'pipe_segment']]
        transport_existing = self.preproc.transport_existing[self.preproc.transport_existing > 0]
        temp = pd.merge(O_flows_df, transport_existing, how='inner', on=['node_i_id', 'node_j_id'])
        existing_arcs_no_flow = temp[temp['co2_volume']<=1]
        layers = [O_flows_df, existing_arcs_no_flow]

        com_vis.create_folium_map(layers, vis_folder + '//', 'postproc_pipeline_map_' + str(self.year_current) + '_' + block +  '.html')
        datatable_outfile = vis_folder + '//datatable_postproc_pipeline_map_' + str(self.year_current) + '_' + block + '.html'
        com_vis.print_datatable_to_html(postproc_df, datatable_outfile)

        pass


    def results_to_csv(self):
        '''Write Pyomo results to csv.

        Parameters
        ----------

        None


        Returns
        -------

        None
        '''
        ### Output flow variables to CSV
        temp_flows_b0_df = self.O_flows_b0_df.loc[self.O_flows_b0_df['co2_volume'] > 0].copy()
        temp_flows_b0_df.to_csv(self.output_path + 'optimization_results//flows//' + str(self.m.name) + '_flows_b0_' + str(self.year_current) + '.csv')
        
        temp_flows_b1_df = self.O_flows_b1_df.loc[self.O_flows_b1_df['co2_volume'] > 0].copy()
        temp_flows_b1_df.to_csv(self.output_path + 'optimization_results//flows//' + str(self.m.name) + '_flows_b1_' + str(self.year_current) + '.csv')

        temp_flows_b2_df = self.O_flows_b2_df.loc[self.O_flows_b2_df['co2_volume'] > 0].copy()
        temp_flows_b2_df.to_csv(self.output_path + 'optimization_results//flows//' + str(self.m.name) + '_flows_b2_' + str(self.year_current) + '.csv')

        if self.parent.debug_switch == True:
            temp_flows_b0_df['block'] = 0
            temp_flows_b1_df['block'] = 1
            temp_flows_b2_df['block'] = 2
            
            temp_flows_b0_df['year'] = int(self.parent.year_current)
            temp_flows_b1_df['year'] = int(self.parent.year_current)
            temp_flows_b2_df['year'] = int(self.parent.year_current)
            
            # supply
            temp_flows_b0_df['supply'] = 0
            temp_flows_b1_df['supply'] = 0
            temp_flows_b2_df['supply'] = 0
            
            pipeline_types = ['source_to_demand', 'source_to_hsm_cent', 'source_to_storage', 'source_to_ts_node']

            for pipeline_type in pipeline_types:
                temp_flows_b0_df.loc[temp_flows_b0_df.loc[:,'pipeline_type']==pipeline_type, 'supply'] = temp_flows_b0_df.loc[temp_flows_b0_df.loc[:,'pipeline_type']==pipeline_type, 'co2_volume']
                temp_flows_b1_df.loc[temp_flows_b1_df.loc[:,'pipeline_type']==pipeline_type, 'supply'] = temp_flows_b1_df.loc[temp_flows_b1_df.loc[:,'pipeline_type']==pipeline_type, 'co2_volume']
                temp_flows_b2_df.loc[temp_flows_b2_df.loc[:,'pipeline_type']==pipeline_type, 'supply'] = temp_flows_b2_df.loc[temp_flows_b2_df.loc[:,'pipeline_type']==pipeline_type, 'co2_volume']
            
            # sequestration
            temp_flows_b0_df['sequestration'] = 0
            temp_flows_b1_df['sequestration'] = 0
            temp_flows_b2_df['sequestration'] = 0

            pipeline_types = ['source_to_demand', 'source_to_hsm_cent', 'source_to_storage', 'ts_node_to_demand', 'ts_node_to_storage']

            for pipeline_type in pipeline_types:

                temp_flows_b0_df.loc[temp_flows_b0_df.loc[:,'pipeline_type']==pipeline_type, 'sequestration'] = temp_flows_b0_df.loc[temp_flows_b0_df.loc[:,'pipeline_type']==pipeline_type, 'co2_volume']
                temp_flows_b1_df.loc[temp_flows_b1_df.loc[:,'pipeline_type']==pipeline_type, 'sequestration'] = temp_flows_b1_df.loc[temp_flows_b1_df.loc[:,'pipeline_type']==pipeline_type, 'co2_volume']
                temp_flows_b2_df.loc[temp_flows_b2_df.loc[:,'pipeline_type']==pipeline_type, 'sequestration'] = temp_flows_b2_df.loc[temp_flows_b2_df.loc[:,'pipeline_type']==pipeline_type, 'co2_volume']

            # save to CSV
            temp_flows_b0_df_filename = self.output_path + 'optimization_results//flows//' + 'b0_flows_all_years.csv'
            temp_flows_b1_df_filename = self.output_path + 'optimization_results//flows//' + 'b1_flows_all_years.csv'
            temp_flows_b2_df_filename = self.output_path + 'optimization_results//flows//' + 'b2_flows_all_years.csv'
            
            if self.year_current == self.parent.year_start:
                temp_flows_b0_df.to_csv(temp_flows_b0_df_filename)
                temp_flows_b1_df.to_csv(temp_flows_b1_df_filename)
                temp_flows_b2_df.to_csv(temp_flows_b2_df_filename)
            else:
                temp_flows_b0_df.to_csv(temp_flows_b0_df_filename, mode='a', header=False)
                temp_flows_b1_df.to_csv(temp_flows_b1_df_filename, mode='a', header=False)
                temp_flows_b2_df.to_csv(temp_flows_b2_df_filename, mode='a', header=False)


        ### Output investment variables to CSV
        # Pipes
        self.O_pipe_bins_b0_df.to_csv(self.output_path + 'optimization_results//investment_decisions//' + str(self.m.name) + '_bins_b0_' + str(self.year_current) + '.csv')
        self.O_pipe_bins_b1_df.to_csv(self.output_path + 'optimization_results//investment_decisions//' + str(self.m.name) + '_bins_b1_' + str(self.year_current) + '.csv')
        self.O_pipe_bins_b2_df.to_csv(self.output_path + 'optimization_results//investment_decisions//' + str(self.m.name) + '_bins_b2_' + str(self.year_current) + '.csv')

        # Pipeline capacity added
        temp_cap_b0_df = self.O_transport_cap_add_b0_df.loc[self.O_transport_cap_add_b0_df['thruput_tonnes'] > 0].copy()
        temp_cap_b1_df = self.O_transport_cap_add_b1_df.loc[self.O_transport_cap_add_b1_df['thruput_tonnes'] > 0].copy()
        temp_cap_b2_df = self.O_transport_cap_add_b2_df.loc[self.O_transport_cap_add_b2_df['thruput_tonnes'] > 0].copy()
        temp_cap_b0_df.to_csv(self.output_path + 'optimization_results//transport_cap_added//' + str(self.m.name) + '_cap_add_b0_' + str(self.year_current) + '.csv')
        temp_cap_b1_df.to_csv(self.output_path + 'optimization_results//transport_cap_added//' + str(self.m.name) + '_cap_add_b1_' + str(self.year_current) + '.csv')
        temp_cap_b2_df.to_csv(self.output_path + 'optimization_results//transport_cap_added//' + str(self.m.name) + '_cap_add_b2_' + str(self.year_current) + '.csv')

        # Storage
        temp_store_aors_b0_df = self.O_store_new_aors_b0_df.loc[self.O_store_new_aors_b0_df['new_aors'] > 0].copy()
        temp_store_aors_b1_df = self.O_store_new_aors_b1_df.loc[self.O_store_new_aors_b1_df['new_aors'] > 0].copy()
        temp_store_aors_b2_df = self.O_store_new_aors_b2_df.loc[self.O_store_new_aors_b2_df['new_aors'] > 0].copy()
        temp_store_aors_b0_df.to_csv(self.output_path + 'optimization_results//investment_decisions//' + str(self.m.name) + '_store_aors_b0_' + str(self.year_current) + '.csv')
        temp_store_aors_b1_df.to_csv(self.output_path + 'optimization_results//investment_decisions//' + str(self.m.name) + '_store_aors_b1_' + str(self.year_current) + '.csv')
        temp_store_aors_b2_df.to_csv(self.output_path + 'optimization_results//investment_decisions//' + str(self.m.name) + '_store_aors_b2_' + str(self.year_current) + '.csv')


        ### Output duals to CSV
        self.duals_df.to_csv(self.output_path + 'optimization_results//duals//' + str(self.m.name) + '_duals_'+ str(self.year_current) + '.csv')

        pass


    def summarize_outputs(self):
        '''Create high level summary of run and output to <model>_data_stats_postproc.csv.

        Parameters
        ----------

        None


        Returns
        -------
        
        None
        '''
        # Get total node and arc options
        supply_nodes_total = len(self.preproc.nodes_supply)
        demand_nodes_total = len(self.preproc.nodes_demand)
        arcs_total         = len(self.preproc.arcs)

        # Get selected nodes and arcs
        supply_nodes_selected_b0 = self.O_flows_b0_df.loc[self.O_flows_b0_df['pipeline_type'].isin(['source_to_storage','source_to_demand','source_to_ts_node', 'source_to_hsm_cent'])].copy()
        supply_nodes_selected_b0 = supply_nodes_selected_b0.loc[supply_nodes_selected_b0['co2_volume'] > 1]
        supply_nodes_selected_b0 = len(supply_nodes_selected_b0['node_i_id'].unique())
        
        supply_nodes_selected_b1 = self.O_flows_b1_df.loc[self.O_flows_b1_df['pipeline_type'].isin(['source_to_storage','source_to_demand','source_to_ts_node', 'source_to_hsm_cent'])].copy()
        supply_nodes_selected_b1 = supply_nodes_selected_b1.loc[supply_nodes_selected_b1['co2_volume'] > 1]
        supply_nodes_selected_b1 = len(supply_nodes_selected_b1['node_i_id'].unique())
        
        supply_nodes_selected_b2 = self.O_flows_b2_df.loc[self.O_flows_b2_df['pipeline_type'].isin(['source_to_storage','source_to_demand','source_to_ts_node', 'source_to_hsm_cent'])].copy()
        supply_nodes_selected_b2 = supply_nodes_selected_b2.loc[supply_nodes_selected_b2['co2_volume'] > 1]
        supply_nodes_selected_b2 = len(supply_nodes_selected_b2['node_i_id'].unique())

        demand_nodes_selected_b0 = self.O_flows_b0_df.loc[self.O_flows_b0_df['pipeline_type'].isin(['source_to_storage','source_to_demand','ts_node_to_demand','ts_node_to_storage', 'source_to_hsm_cent'])].copy()
        demand_nodes_selected_b0 = demand_nodes_selected_b0.loc[demand_nodes_selected_b0['co2_volume'] > 1]
        demand_nodes_selected_b0 = len(demand_nodes_selected_b0['node_j_id'].unique())
        
        demand_nodes_selected_b1 = self.O_flows_b1_df.loc[self.O_flows_b1_df['pipeline_type'].isin(['source_to_storage','source_to_demand','ts_node_to_demand','ts_node_to_storage', 'source_to_hsm_cent'])].copy()
        demand_nodes_selected_b1 = demand_nodes_selected_b1.loc[demand_nodes_selected_b1['co2_volume'] > 1]
        demand_nodes_selected_b1 = len(demand_nodes_selected_b1['node_j_id'].unique())

        arcs_selected_b0 = self.O_flows_b0_df.loc[self.O_flows_b0_df['co2_volume'] > 1].copy()
        arcs_selected_b0 = len(arcs_selected_b0['node_j_id'].unique())
        
        arcs_selected_b1 = self.O_flows_b1_df.loc[self.O_flows_b1_df['co2_volume'] > 1].copy()
        arcs_selected_b1 = len(arcs_selected_b1['node_j_id'].unique())

        co2_volume_supply_b0_nodes = self.O_flows_b0_df.loc[self.O_flows_b0_df['pipeline_type'].isin(['source_to_storage','source_to_demand','source_to_ts_node', 'source_to_hsm_cent'])].copy()
        co2_volume_supply_b0_nodes = co2_volume_supply_b0_nodes['co2_volume'].sum()
        
        co2_volume_supply_b1_nodes = self.O_flows_b1_df.loc[self.O_flows_b1_df['pipeline_type'].isin(['source_to_storage','source_to_demand','source_to_ts_node', 'source_to_hsm_cent'])].copy()
        co2_volume_supply_b1_nodes = co2_volume_supply_b1_nodes['co2_volume'].sum()
        
        co2_volume_supply_b2_nodes = self.O_flows_b2_df.loc[self.O_flows_b2_df['pipeline_type'].isin(['source_to_storage','source_to_demand','source_to_ts_node', 'source_to_hsm_cent'])].copy()
        co2_volume_supply_b2_nodes = co2_volume_supply_b2_nodes['co2_volume'].sum()

        co2_volume_demand_b0_nodes = self.O_flows_b0_df.loc[self.O_flows_b0_df['pipeline_type'].isin(['source_to_storage','source_to_demand','ts_node_to_demand','ts_node_to_storage', 'source_to_hsm_cent'])].copy()
        co2_volume_demand_b0_nodes = co2_volume_demand_b0_nodes['co2_volume'].sum()
        
        co2_volume_demand_b1_nodes = self.O_flows_b1_df.loc[self.O_flows_b1_df['pipeline_type'].isin(['source_to_storage','source_to_demand','ts_node_to_demand','ts_node_to_storage', 'source_to_hsm_cent'])].copy()
        co2_volume_demand_b1_nodes = co2_volume_demand_b1_nodes['co2_volume'].sum()


        postproc_data = {
            'year' : [self.year_current],
            'iteration': [self.parent.iteration_current],
            'cycle': [self.parent.cycle_current],
            'number of supply node options': [supply_nodes_total],
            'number of demand node options': [demand_nodes_total],
            'number of arc options': [arcs_total],
            'number of supply nodes selected b0': [supply_nodes_selected_b0],
            'number of supply nodes selected b1': [supply_nodes_selected_b1],
            'number of demand nodes selected b0': [demand_nodes_selected_b0],
            'number of demand nodes selected b1': [demand_nodes_selected_b1],
            'number of arcs selected b0': [arcs_selected_b0],
            'number of arcs selected b1': [arcs_selected_b1],
            'CO2 volume at supply nodes b0': [co2_volume_supply_b0_nodes],
            'CO2 volume at supply nodes b1': [co2_volume_supply_b1_nodes],
            'CO2 volume at demand nodes b0': [co2_volume_demand_b0_nodes],
            'CO2 volume at demand nodes b1': [co2_volume_demand_b1_nodes],
            'total CO2 volume transported b0': [self.O_flows_b0_df['co2_volume'].sum()],
            'total CO2 volume transported b1': [self.O_flows_b1_df['co2_volume'].sum()],
            'total cost b0 (actual)': self.m.blocks[0].e_sum_all_costs(),
            'total cost b1 (projected)': self.m.blocks[1].e_sum_all_costs(),
            'total cost b2 (projected)': self.m.blocks[2].e_sum_all_costs(),
            'total cost': self.m.objective_value
        }

        postproc_df = pd.DataFrame(postproc_data)
        self.postproc_df = postproc_df

        if self.year_current == self.parent.year_start:
            # Write stats output
            postproc_df.to_csv(self.output_path + str(self.m.name) + '_data_stats_postproc.csv', mode= 'w')
        else:
            # Write stats output
            postproc_df.to_csv(self.output_path + str(self.m.name) + '_data_stats_postproc.csv', mode= 'a', header=False)

        pass
