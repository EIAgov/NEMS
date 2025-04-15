"""Optimization Submodule.

CCATS Optimization: Summary
---------------------------

This submodule is the main optimization program for CCATS. 
CCATS treats the problem as either a Linear Program (LP) or Mixed Integer Linear Program (MILP) using Pyomo.
The optimization operates as follows:

    1. :meth:`~models.ccats_optimization.OptimizationModel.setup` is called from :ref:`module`.Optimization specific options are set based on inputs
       from *opt_setup.csv*.

    2. :meth:`~models.ccats_optimization.OptimizationModel.run` is called from :ref:`module`.

    3. Data is organized by time period in preparation for creating a Pyomo model.

    4. The Pyomo model is created using three blocks to represent three model time periods.

    5. Model is solved. If solved as a LP, duals are saved.

    6. If model was originally solved as MILP, it is re-solved as a LP (by fixing integer variables) to get duals.


CCATS Optimization: Input Files
-------------------------------

* opt_setup.csv - optimization setup file


CCATS Optimization: Model Functions and Class Methods
_____________________________________________________
    
* :meth:`~models.ccats_optimization.OptimizationModel.__init__` - Constructor to initialize Class (called by :meth:`module.Module.setup`)
* :meth:`~models.ccats_optimization.OptimizationModel.setup` - Setup function using *opt_setup.csv* (called by :meth:`module.Module.setup`)
* :meth:`~models.ccats_optimization.OptimizationModel.run` - runs main model functions in :class:`~models.ccats_optimization.OptimizationModel` (called by :meth:`module.Module.run`)
* :meth:`~models.ccats_optimization.OptimizationModel.instantiate_pyomo` - creates the pyomo model (called by :meth:`~models.ccats_optimization.OptimizationModel.run`)
* :meth:`~models.ccats_optimization.OptimizationModel.prepare_data_for_blocks` - creates dictionaries of data indexed by block (called by :meth:`~models.ccats_optimization.OptimizationModel.run`)
* :meth:`~models.ccats_optimization.OptimizationModel.instantiate_blocks` - calls most declare_* functions to set-up the optimization problem, assigning data by block (called by :meth:`~models.ccats_optimization.OptimizationModel.run`)
* :meth:`~models.ccats_optimization.OptimizationModel.declare_sets` - creates sets (called by :meth:`~models.ccats_optimization.OptimizationModel.instantiate_pyomo`)
* :meth:`~models.ccats_optimization.OptimizationModel.declare_parameters` - creates parameters (variables held fixed) in Pyomo (called by :meth:`~models.ccats_optimization.OptimizationModel.instantiate_pyomo`)
* :meth:`~models.ccats_optimization.OptimizationModel.declare_variables` - creates variables in Pyomo (called by :meth:`~models.ccats_optimization.OptimizationModel.instantiate_pyomo`)
* :meth:`~models.ccats_optimization.OptimizationModel.declare_constraints` - creates constraints in Pyomo (called by :meth:`~models.ccats_optimization.OptimizationModel.instantiate_pyomo`)
* :meth:`~models.ccats_optimization.OptimizationModel.declare_objective` - creates objective function for a single block (called by :meth:`~models.ccats_optimization.OptimizationModel.instantiate_pyomo`)
* :meth:`~models.ccats_optimization.OptimizationModel.declare_constraints_across_blocks` - creates constraints across blocks (called by :meth:`~models.ccats_optimization.OptimizationModel.run`)
* :meth:`~models.ccats_optimization.OptimizationModel.declare_objective_across_blocks` - creates multi-block objective (called by :meth:`~models.ccats_optimization.OptimizationModel.run`)
* :meth:`~models.ccats_optimization.OptimizationModel.solve_optimization` - solve the optimization problem (called by :meth:`~models.ccats_optimization.OptimizationModel.run`)
* :meth:`~models.ccats_optimization.OptimizationModel.solve_linearized_optimization` - re-solve the optimization as a linear problem to get duals, if originally solved non-linear (called by :meth:`~models.ccats_optimization.OptimizationModel.run`)

CCATS Optimization: Output Debug Files
______________________________________
    
* **main_opt_log.log** - Optimization log file
* debug\\model_file\\**ccats_model_20XX.mps** - MPS of 20XX optimization problem
* debug\\model_file\\**ccats_model_20XX.lp** - LP of 20XX optimization problem
* debug\\optimization_inputs\\data\\**data_b#_20XX.pkl** - Pickled data inputs for block # for 20XX


CCATS Optimization: Code
________________________
"""
import logging

import numpy as np
import pandas as pd
from pyomo.core import *
import pyomo.environ as pyo
from pyomo.opt import SolutionStatus, SolverStatus, TerminationCondition
from pyomo.common.timing import report_timing
from itertools import compress

import submodule_ccats as subccats
from ccats_common import common as com
from ccats_common import common_debug as com_bug


class OptimizationModel(subccats.Submodule):
    """
    Main Optimization Submodule for CCATS.
    """

    def __init__(self, parent):
        '''Initializes OptimizationModel object.

        Parameters
        ----------
        
        parent : str
            Module.Module (Pointer to parent module)

        Returns
        -------

        None

        '''
        super().__init__(parent, submodule_name='ccats_opt')

        ### Pyomo model objects
        # Note self.m variables (i.e. self.m.S_nodes) can't be declared until model is declared
        self.m          = None # Model object for main pyomo optimization
        self.m_linear   = None # Model object for linearized pyomo optimization

        pass


    def setup(self, setup_filename):
        """Setup Main Optimization Submodule for CCATS.

        Parameters
        ----------
            
        setup_filename : str
            Path to transport setup file.

        Returns
        -------
        
        self.opt_check_solver_status : bool
            If True, check the solver status

        self.troubleshoot_datatypes : bool
            If True, log datatypes to assist with troubleshooting

        self.debug_opt_log : Boolean
            If True, output solver status to nohup (integrated runs only) and main_opt_log.log (standalone & integrated runs)

        """
        ### General Setup
        # Read in setup file
        super().setup(setup_filename)

        # Read in switches
        self.opt_check_solver_status      = str(self.setup_table.at['opt_check_solver_status',      'value']).upper() == 'True'.upper() # boolean only true if setup file only contains booleans
        self.troubleshoot_datatypes       = str(self.setup_table.at['troubleshoot_datatypes',       'value']).upper() == 'True'.upper() # log datatypes to assist with troubleshooting
        self.debug_opt_log                = str(self.setup_table.at['debug_opt_log',                'value']).upper() == 'True'.upper() # Output solver status to nohup (integrated runs only) and main_opt_log.log (standalone & integrated runs)
        self.use_solver_xpress_persistent = str(self.setup_table.at['use_solver_xpress_persistent', 'value']).upper() == 'True'.upper() # Use the xpress persistent solver (NEMS default - note: the first solver selected is used)
        self.use_solver_xpress_direct     = str(self.setup_table.at['use_solver_xpress_direct',     'value']).upper() == 'True'.upper() # Use the xpress direct solver (note: the first solver selected is used)
        self.use_solver_xpress            = str(self.setup_table.at['use_solver_xpress',            'value']).upper() == 'True'.upper() # Use the default version of the xpress solver (note: the first solver selected is used)
        self.use_solver_highs             = str(self.setup_table.at['use_solver_highs',             'value']).upper() == 'True'.upper() # Use the HiGHS solver (note: the first solver selected is used)
        
        # Read in solver options
        self.xpress_path    = str(self.setup_table.at['xpress_path', 'value'])   # path to xpress' xpauth.xpr license file
        self.soltimelimit   = int(self.setup_table.at['soltimelimit', 'value'])  # solution time limit per iteration (seconds)
        self.threads        = self.parent.threads                                # number of threads available (from Module)
        self.maxmemorysoft  = int(self.setup_table.at['maxmemorysoft', 'value']) # MB available for solve

        ## set self.solver and import/configure solver
        # FICO XPRESS
        if (self.use_solver_xpress_persistent) or (self.use_solver_xpress_direct) or (self.use_solver_xpress):
            
            # set self.solver
            if self.use_solver_xpress_persistent:
                self.solver = 'xpress_persistent'
            elif self.use_solver_xpress_direct:
                self.solver = 'xpress_direct'
            elif self.use_solver_xpress:
                self.solver = 'xpress'
            
            # import and configure xpress
            try:
                import xpress
                xpress.init(self.xpress_path)
                self.logger.info("CCATS will use " + self.solver + ".")
            except:
                self.logger.error("Unable to import xpress, please specify xpress_path: path to xpress' xpauth.xpr license file.")

        # HiGHS
        elif self.use_solver_highs:
            # set self.solver
            self.solver = 'appsi_highs'
            
            # import and configure HiGHS
            try:
                import highspy
                from pyomo.contrib.appsi.solvers import Highs
                self.logger.info("CCATS will use HiGHS.")
            except:
                self.logger.error("Unable to import HiGHS. Please check that it is installed.")

        else:
            self.logger.error("No solver selected, please set one of the use_solver_<name> options to TRUE in opt_setup.csv.")

        pass


    def run(self):
        '''Run Main Optimization Submodule for CCATS.
        
        Parameters
        ----------

        None

        
        Returns
        -------
        
        None
        '''
        # Instantiate parent variables
        super().run()

        # Report timing
        if self.parent.debug_switch == True:
            report_timing()

        # Run annual methods
        if self.year_current >= self.parent.year_start:
            # Instantiate Pyomo
            self.logger.info('Instantiate Pyomo')
            self.instantiate_pyomo()

            # Prepare data for blocks
            self.logger.info('Prepare Data for Blocks')
            self.prepare_data_for_blocks()

            # Declare blocks
            self.logger.info('Declare Blocks')
            self.m.blocks = pyo.Block(self.m.s_time, rule=self.instantiate_blocks)

            # Declare Constraints and Objectives across Blocks
            self.logger.info('Declare Constraints Across Blocks')
            self.declare_constraints_across_blocks()
            self.logger.info('Declare Objective Across Blocks')
            self.declare_objective_across_blocks()

            # Solve Optimization
            self.logger.info('Solve Optimization')
            self.solve_optimization()

            # Solve Optimization - Linearized
            if self.parent.linear_model_switch == False: # Do not resolve if the model was already linearized
                self.logger.info('Solve Optimization - Linearized')
                self.solve_linearized_optimization()

        pass


    def instantiate_pyomo(self):
        '''Run Main Optimization Submodule for CCATS.

        Parameters
        ----------
        
        None

        
        Returns
        -------
        
        m : Pyomo ConcreteModel
            Instantiated Pyomo model
        
        '''
        #Check solver availability
        if self.opt_check_solver_status == True:
            self.logger.info('Checking Solver Availability')
            pyomo_solvers_list = pyo.SolverFactory.__dict__['_cls'].keys()
            solvers_filter = []
            for s in pyomo_solvers_list:
               try:
                   solvers_filter.append(pyo.SolverFactory(s).available())
               except (Exception, NameError, ImportError) as e:
                   solvers_filter.append(False)
            pyomo_solvers_list = list(compress(pyomo_solvers_list, solvers_filter))
            self.logger.info(pyomo_solvers_list)
        else:
            pass

        ### Turn off pyomo debug logging
        logging.getLogger('pyomo.core').setLevel(logging.ERROR)
        logging.getLogger('pyomo').setLevel(logging.ERROR)

        ### Declare main optimization object
        self.m = pyo.ConcreteModel(name='ccats_opt')

        pass

    def prepare_data_for_blocks(self):
        '''Produce block-level tables for optimization.

        Parameters
        ----------
        
        None

        Returns
        -------
        
        self.m.s_time : list
            List of number of time periods as model blocks

        self.m.duration : list
            List of duration of each block (years)

        self.m.co2_supply : dict
            Dictionary of CO\ :sub:`2` supply (metric tonnes) by CO\ :sub:`2` source by block

        self.m.co2_demand : dict
            Dictionary of CO\ :sub:`2` demand (metric tonnes) by CO\ :sub:`2` demand site by block

        self.m.co2_demand_cost_net : dict
            Dictionary of CO\ :sub:`2` demand net cost ($1987/tonne tonne) by CO\ :sub:`2` demand site by block

        self.m.storage_injectivity_existing : dict
            Dictionary of existing CO\ :sub:`2` storage site injectivity (tonnes/year) by block

        self.m.storage_injectivity_new : dict
            Dictionary of potential new CO\ :sub:`2` storage site injectivity (tonnes/year) by block

        self.m.storage_net_existing  : dict
            Dictionary of existing CO\ :sub:`2` storage site net storage capacity (tonnes) by block

        self.m.storage_net_adder : dict
            Dictionary of potential new CO\ :sub:`2` storage site net storage capacity (tonnes) by block

        self.m.storage_aors_available : dict
            Dictionary of remaining storage AORs available by block

        self.m.capex_transport_base : dict
            Dictionary of transportation CAPEX intercept (base) by block
        
        self.m.capex_transport_slope : dict
            Dictionary of transportation CAPEX slope by block

        self.m.opex_transport_elec : dict
            Dictionary of transportation electricity demand by block

        self.m.storage_capex : dict
            Dictionary of storage capex (1987$/tonne) by storage site by block

        self.m.storage_opex : dict
            Dictionary of storage opex (1987$/tonne) by storage site by block

        self.m.discount_invest_storage : dict
            Dictionary of storage investment discount rates by block

        self.m.discount_invest_transport : dict
            Dictionary of transportation discount rates by block

        self.m.discount_variable : dict
            Dictionary of variable discount rates by block

        self.m.discount_policy : dict
            Dictionary of policy discount rates by block

        '''
        # Set block time period and duration
        time = {0, 1, 2}       # index of each block
        duration = [self.preproc.duration_b0,
                    self.preproc.duration_b1,
                    self.preproc.duration_b2]  # duration of each block (in years)
        self.m.s_time = pyo.Set(initialize=time)
        self.m.duration = duration


        ### Set period model inputs to dicts
        ### Supply, demand and storage
        self.m.co2_supply = {0: self.preproc.co2_supply_b0.copy(),
                             1: self.preproc.co2_supply_b1.copy(),
                             2: self.preproc.co2_supply_b2.copy()}

        self.m.co2_demand = {0: self.preproc.co2_demand_b0.copy(),
                             1: self.preproc.co2_demand_b1.copy(),
                             2: self.preproc.co2_demand_b2.copy()}

        self.m.co2_demand_cost_net =   {0: self.preproc.co2_demand_cost_b0.copy(),
                                        1: self.preproc.co2_demand_cost_b1.copy(),
                                        2: self.preproc.co2_demand_cost_b2.copy()}

        self.m.storage_injectivity_existing = {0: self.preproc.co2_injectivity_existing_b0.copy(),
                                               1: self.preproc.co2_injectivity_existing_b1.copy(),
                                               2: self.preproc.co2_injectivity_existing_b2.copy()}

        self.m.storage_injectivity_new = {0: self.preproc.co2_injectivity_new_b0.copy(),
                                          1: self.preproc.co2_injectivity_new_b1.copy(),
                                          2: self.preproc.co2_injectivity_new_b2.copy()}

        self.m.storage_net_existing = {0: self.preproc.co2_store_net_existing_b0.copy(),
                                       1: self.preproc.co2_store_net_existing_b1.copy(),
                                       2: self.preproc.co2_store_net_existing_b2.copy()}

        self.m.storage_net_adder = {0: self.preproc.co2_store_net_adder_b0.copy(),
                                    1: self.preproc.co2_store_net_adder_b1.copy(),
                                    2: self.preproc.co2_store_net_adder_b2.copy()}

        self.m.storage_aors_available = {0: self.preproc.storage_aors_available_b0.copy(),
                                         1: self.preproc.storage_aors_available_b1.copy(),
                                         2: self.preproc.storage_aors_available_b2.copy()}

        ### Transport costs
        self.m.capex_transport_base = {0: self.preproc.capex_transport_base_b0.copy(),
                                       1: self.preproc.capex_transport_base_b1.copy(),
                                       2: self.preproc.capex_transport_base_b2.copy()}
        
        self.m.capex_transport_slope = {0: self.preproc.capex_transport_slope_b0.copy(),
                                        1: self.preproc.capex_transport_slope_b1.copy(),
                                        2: self.preproc.capex_transport_slope_b2.copy()}
        
        self.m.opex_transport_elec= {   0: self.preproc.opex_transport_elec_b0.copy(),
                                        1: self.preproc.opex_transport_elec_b1.copy(),
                                        2: self.preproc.opex_transport_elec_b2.copy()}


        ### Sequestration costs
        self.m.storage_capex = {0: self.preproc.storage_capex_b0.copy(),
                                1: self.preproc.storage_capex_b1.copy(),
                                2: self.preproc.storage_capex_b2.copy()}
        
        self.m.storage_opex = {0: self.preproc.storage_varom_b0.copy(),
                               1: self.preproc.storage_varom_b1.copy(),
                               2: self.preproc.storage_varom_b2.copy()}


        ### Discounting/Financing
        self.m.discount_invest_storage = {0: self.preproc.discount_invest_storage_b0,
                                          1: self.preproc.discount_invest_storage_b1,
                                          2: self.preproc.discount_invest_storage_b2}

        self.m.discount_invest_transport = {0: self.preproc.discount_invest_transport_b0,
                                            1: self.preproc.discount_invest_transport_b1,
                                            2: self.preproc.discount_invest_transport_b2}

        self.m.discount_variable = {0: self.preproc.discount_variable_b0,
                                    1: self.preproc.discount_variable_b1,
                                    2: self.preproc.discount_variable_b2}

        self.m.discount_policy = {0: self.preproc.discount_policy_b0,
                                  1: self.preproc.discount_policy_b1,
                                  2: self.preproc.discount_policy_b2}

        pass


    def instantiate_blocks(self, mb, t):
        '''Instantiate a single pyomo block for *m*.

        Parameters
        ----------

        mb : Pyomo Block
            Pyomo block to be populated

        t : int
            Time period

        Returns
        -------
        
        self.b : Pyomo Block
            Block populated with sets, parameters, variables, constraints, and objective function.

        '''
        self.b = mb  # block
        self.t = t  # time period

        # Assign input data from preproc to a local variable
        data = self.preproc

        ### Instantiate model input data by block
        # Supply
        data.co2_supply = self.m.co2_supply[self.t]

        # Demand
        data.co2_demand = self.m.co2_demand[self.t]

        # Demand Cost Net
        data.co2_demand_cost_net = self.m.co2_demand_cost_net[self.t]

        # Transport costs
        data.capex_transport_base = self.m.capex_transport_base[self.t]
        data.capex_transport_slope = self.m.capex_transport_slope[self.t]
        data.opex_transport_elec = self.m.opex_transport_elec[self.t]

        # Injectivity
        data.storage_injectivity_existing = self.m.storage_injectivity_existing[self.t]
        data.storage_injectivity_new = self.m.storage_injectivity_new[self.t]

        # Net Storage
        data.storage_net_existing = self.m.storage_net_existing[self.t]
        data.storage_net_adder = self.m.storage_net_adder[self.t]
        data.storage_aors_available = self.m.storage_aors_available[self.t]

        # Sequestration costs
        data.storage_capex = self.m.storage_capex[self.t]
        data.storage_opex = self.m.storage_opex[self.t]

        # Duration
        data.duration = self.m.duration[t]

        # Financing/Discounting
        data.discount_invest_storage = self.m.discount_invest_storage[t]
        data.discount_invest_transport = self.m.discount_invest_transport[t]
        data.discount_variable = self.m.discount_variable[t]
        data.discount_policy = self.m.discount_policy[t]

        # Setup Pyomo
        self.logger.info('Setup Block')
        self.logger.info('Declare Block Sets')
        self.declare_sets(data)
        self.logger.info('Declare Block Parameters')
        self.declare_parameters(data)
        self.logger.info('Declare Block Variables')
        self.declare_variables()
        self.logger.info('Declare Block Constraints')
        self.declare_constraints()
        self.logger.info('Declare Block Objective')
        self.declare_objective()

        ## Print out the data types and unique values for data to assist with troubleshooting
        if self.troubleshoot_datatypes:
            
            d = {}
            self.logger.info("Datatypes and unique values for time period " + str(self.t) + ".")
            
            attributes = ['co2_supply','co2_demand', 'co2_demand_cost_net',
                          'capex_transport_base', 'capex_transport_slope', 'opex_transport_elec',
                          'electricity_demand',
                          'storage_injectivity_existing', 'storage_injectivity_new', 'storage_net_existing',
                          'storage_net_adder', 'storage_aors_available', 'storage_capex', 'storage_opex', 'duration',
                          'discount_invest_storage', 'discount_invest_transport', 'discount_variable', 'discount_policy']
            for attr in attributes:
                try:
                    exec("self.logger.info(attr + ': \t' + str(data." + attr + ".dtypes))")
                    exec("self.logger.info(attr + ': \t' + str(data." + attr + ".unique()))")
                except:
                    self.logger.info(attr + ": ?")
                    
                d[attr] = eval("data." + attr + "")
            
            # pickle the data to review later
            import pickle
            pkl_filename = self.output_path + 'optimization_inputs\\data\\data_b' + str(t) + '_' + str(self.year_current) + '.pkl'
            with open(pkl_filename, 'wb') as f:
                pickle.dump(d, f)


        pass


    def declare_sets(self, data):
        '''Declare sets.

        Parameters
        ----------

        data : object
            Data for including within this block.

        Returns
        -------
        
        self.b.s_nodes_supply : Pyomo Set
            Pyomo set of supplu nodes
            
        self.b.s_nodes_trans_ship : Pyomo Set
            Pyomo set of trans-shipment nodes
            
        self.b.s_nodes_demand : Pyomo Set
            Pyomo set of demand nodes
            
        self.b.s_nodes_sequester : Pyomo Set
            Pyomo set of sequestration nodes (i.e. storage and demand)
            
        self.b.s_nodes_storage  : Pyomo Set
            Pyomo set of storage nodes
            
        self.b.s_nodes : Pyomo Set
            Pyomo set of all nodes

        self.b.s_arcs : Pyomo Set
            Pyomo set of arcs.

        self.b.s_arcs_out : Pyomo Set
            Pyomo set of arcs out

        self.b.s_arcs_in : Pyomo Set
            Pyomo set of arcs in
        
        self.b.s_transport_options : Pyomo Set
            Pyomo set of transportation options
        
        self.b.s_policy_options : Pyomo Set
            Pyomo set of policy options
            
        '''
        ### Nodes
        # Declare nodes
        self.b.s_nodes_supply = pyo.Set(initialize=data.nodes_supply)
        self.b.s_nodes_trans_ship = pyo.Set(initialize=data.nodes_trans_ship)
        self.b.s_nodes_demand = pyo.Set(initialize=data.nodes_demand)
        self.b.s_nodes_sequester = pyo.Set(initialize=data.nodes_sequester)
        self.b.s_nodes_storage = pyo.Set(initialize=data.nodes_storage)
        self.b.s_nodes = pyo.Set(initialize=data.nodes)

        ### Arcs
        # Declare arcs
        self.b.s_arcs = data.arcs.copy()
        self.b.s_arcs_out = {n: list() for n in data.nodes}
        self.b.s_arcs_in = {n: list() for n in data.nodes}

        # s_arcs_out and s_arcs_in are manually declared as Ordered Scalar Sets
        for a in data.arcs.copy():
            self.b.s_arcs_out[a[0]].append(a)
            self.b.s_arcs_in[a[1]].append(a)

        self.b.s_arcs = pyo.Set(initialize=self.b.s_arcs)

        # Declare transport options
        transport_options = data.pipeline_lookup_opt['pipe_segment'].unique().tolist()
        self.b.s_transport_options = pyo.Set(initialize=transport_options)

        # Declare policy options
        policy_options = [0,1]
        self.b.s_policy_options = pyo.Set(initialize=policy_options)

        pass


    def declare_parameters(self, data):
        '''Declare parameters.

        Parameters
        ----------

        data : object
            Data for including within this block.

        Returns
        -------
        self.b.p_transport_cap_existing : Pyomo Param
            Parameter for block existing transportation capacities.

        self.b.p_transport_cap_add_min : Pyomo Param
            Parameter for block transport capacity adder minimum values.

        self.b.p_transport_cap_add_max : Pyomo Param
            Parameter for block transport capacity adder maximum values.

        self.b.p_capex_transport_base : Pyomo Param
            Parameter for block transportation capex equation intercept.

        self.b.p_capex_transport_slope : Pyomo Param
            Parameter for block transportation capex equation slope.

        self.b.p_opex_transport : Pyomo Param
            Parameter for block transportation variable opex.

        self.b.p_electricity_demand : Pyomo Param
            Parameter for block transportation electricity demand.

        self.b.p_capex_storage : Pyomo Param
            Parameter for block storage capex.

        self.b.p_opex_storage : Pyomo Param
            Parameter for block storage variable opex.

        self.b.p_co2_demand_cost_net : Pyomo Param
            Parameter for block CO\ :sub:`2` demand net costs.

        self.b.p_policy_cost : Pyomo Param
            Parameter for block policy costs.

        self.b.p_co2_supply : Pyomo Param
            Parameter for block CO\ :sub:`2` supply.

        self.b.p_co2_demand : Pyomo Param
            Parameter for block CO\ :sub:`2` demand.

        self.b.p_storage_injectivity_existing : Pyomo Param
            Parameter for block existing CO\ :sub:`2` injection/year.

        self.b.p_storage_injectivity_new : Pyomo Param
            Parameter for block potential new CO\ :sub:`2` injection/year.

        self.b.p_storage_net_existing : Pyomo Param
            Parameter for block existing storage total capacity.

        self.b.p_storage_net_adder : Pyomo Param
            Parameter for block potential storage total capacity adder.

        self.b.p_storage_aors_available : Pyomo Param
            Parameter for block remaining storage injection sites available.

        self.b.p_duration : Pyomo Param
            Parameter for block duration in years.

        self.b.p_discount_invest_storage : Pyomo Param
            Parameter for storage investment discount rate.

        self.b.p_discount_invest_transport : Pyomo Param
            Parameter for transportation investment discount rate.

        self.b.p_discount_variable : Pyomo Param
            Parameter for variable cost discount rate.

        self.b.p_discount_policy : Pyomo Param
            Parameter for policy cost discount rate.

        self.b.transport_buffer : Pyomo Param
            Parameter for transportation build buffer.

        self.b.storage_buffer : Pyomo Param
            Parameter for storage build buffer.

        self.b.p_M_supply : Pyomo Param
            Parameter for supply bounds.

        self.b.p_M_storage : Pyomo Param
            Parameter for storage bounds.

        self.b.p_M_excess : Pyomo Param
            Parameter for excess (infeasibility) cost.
        '''
        ### Instantiate input parameters
        # Instantiate transportation capacities
        self.b.p_transport_cap_existing = pyo.Param(self.b.s_arcs, initialize=data.transport_existing, mutable=True, default=0.00)
        self.b.p_transport_cap_add_min = pyo.Param(self.b.s_arcs, self.b.s_transport_options, initialize=data.transport_add_min, mutable=True, default=0.00)
        self.b.p_transport_cap_add_max = pyo.Param(self.b.s_arcs, self.b.s_transport_options, initialize=data.transport_add, mutable=True, default=0.00)

        # Instantiate transportation capex
        self.b.p_capex_transport_base = pyo.Param(self.b.s_arcs, self.b.s_transport_options, initialize=data.capex_transport_base, mutable=True, default=9E14)
        self.b.p_capex_transport_slope = pyo.Param(self.b.s_arcs, self.b.s_transport_options, initialize=data.capex_transport_slope, mutable=True, default=9E14)

        # Instantiate transportation opex
        self.b.p_opex_transport = pyo.Param(self.b.s_arcs, initialize=data.opex_transport_elec, mutable=True, default=9E14)
        self.b.p_electricity_demand = pyo.Param(self.b.s_arcs, initialize=data.electricity_demand, mutable=True, default=9E14)

        # Instantiate demand node costs and incentives
        self.b.p_capex_storage          = pyo.Param(self.b.s_nodes_storage, initialize=data.storage_capex, mutable=True, default=9E14)
        self.b.p_opex_storage           = pyo.Param(self.b.s_nodes_storage, initialize=data.storage_opex, mutable=True, default=9E14)
        self.b.p_co2_demand_cost_net    = pyo.Param(self.b.s_nodes_demand, initialize = -1 * data.co2_demand_cost_net, mutable=True, default=0.0)
        self.b.p_policy_cost            = pyo.Param(self.b.s_nodes_sequester, self.b.s_policy_options, initialize= -1 * data.policy_cost, mutable=True, default=9E14)

        # Instantiate target CO2 Supply
        self.b.p_co2_supply = pyo.Param(self.b.s_nodes_supply, self.b.s_policy_options, initialize=data.co2_supply, mutable=True, default=0.00)

        # Instantiate max CO2 Demand
        self.b.p_co2_demand = pyo.Param(self.b.s_nodes_demand, initialize=data.co2_demand, mutable=True, default=0.00)

        # Instantiate CO2 Injectivity
        self.b.p_storage_injectivity_existing = pyo.Param(self.b.s_nodes_storage, initialize=data.storage_injectivity_existing, mutable=True, default=0.00)
        self.b.p_storage_injectivity_new = pyo.Param(self.b.s_nodes_storage, initialize=data.storage_injectivity_new, mutable=True, default=0.00)

        # Instantiate Net CO2 Storage
        self.b.p_storage_net_existing = pyo.Param(self.b.s_nodes_storage, initialize=data.storage_net_existing, mutable=True, default=0.00)
        self.b.p_storage_net_adder = pyo.Param(self.b.s_nodes_storage, initialize=data.storage_net_adder, mutable=True, default=0.00)

        # Instantiate Storage AORs Available
        self.b.p_storage_aors_available = pyo.Param(self.b.s_nodes_storage, initialize=data.storage_aors_available, mutable = True, default = 0.00)

        # Instantiate time period duration
        self.b.p_duration = pyo.Param( initialize=data.duration, mutable=True, default=0.00)

        # Instantiate financing/discounting
        self.b.p_discount_invest_storage = pyo.Param(initialize=data.discount_invest_storage, mutable=True, default=0.00)
        self.b.p_discount_invest_transport = pyo.Param(initialize=data.discount_invest_transport, mutable=True, default=0.00)
        self.b.p_discount_variable = pyo.Param(initialize=data.discount_variable, mutable=True, default=0.00)
        self.b.p_discount_policy = pyo.Param(initialize=data.discount_policy, mutable=True, default=0.00)

        # Instantiate transport and storage buffers
        self.b.transport_buffer = self.preproc.transport_buffer
        self.b.storage_buffer   = self.preproc.storage_buffer

        ### Instantiate big M parameters
        M_supply = 999999999
        self.b.p_M_supply = pyo.Param(initialize=M_supply)

        M_storage = 9999999999
        self.b.p_M_storage = pyo.Param(initialize=M_storage)

        M_infeasibility = 1000000
        self.b.p_M_excess = pyo.Param(initialize=M_infeasibility)

        pass


    def declare_variables(self):
        '''Declare variables.

        Parameters
        ----------
                
        None

        Returns
        -------
        
        self.b.v_flow : Pyomo Var
            Pyomo decision variable of block flow through arcs.

        self.b.v_flow_base : Pyomo Var
            Pyomo decision variable of block flow through existing arcs.

        self.b.v_flow_add : Pyomo Var
            Pyomo decision variable of block flow through arcs requiring investment.

        self.b.v_flow_by_policy : Pyomo Var
            Pyomo decision variable of block flow by policy type (i.e. 45Q or no tax credit).

        self.b.vb_transport_investment : Pyomo Var
            Pyomo binary variable for block new investment.

        self.b.v_transport_cap_add : Pyomo Var
            Pyomo decision variable of block transport capacity added.

        self.b.v_storage_investment : Pyomo Var
            Pyomo decision variable of block storage investment added.

        self.b.v_storage_injectivity  : Pyomo Var
            Pyomo decision variable of block storage injectivity.

        self.b.va_slack_storage : Pyomo Var
            Slack variable for storage capacity.

        self.b.va_slack_demand_eor : Pyomo Var
            Slack variable for demand capacity.

        self.b.va_excess_supply : Pyomo Var
            Slack variable for excess (infeasible) supply.

        self.b.va_excess_ts_in : Pyomo Var
            Slack variable for excess (infeasible) supply flowing into ts nodes.

        self.b.va_excess_ts_out : Pyomo Var
            Slack variable for excess (infeasible) supply flowing out of ts nodes.
        '''
        ### Define objective variables
        # Flow variable
        self.b.v_flow = pyo.Var(self.b.s_arcs, bounds=(0, self.b.p_M_supply))

        # Flow variable adjustments - *v_flow_add* will be non-zero if there is investment in the chosen pipeline
        self.b.v_flow_base = pyo.Var(self.b.s_arcs, bounds=(0, self.b.p_M_supply))
        self.b.v_flow_add = pyo.Var(self.b.s_arcs, bounds=(0, self.b.p_M_supply))

        # Flow by policy
        self.b.v_flow_by_policy = pyo.Var(self.b.s_arcs, self.b.s_policy_options, bounds=(0, self.b.p_M_supply))  # primary decision var

        # Binary investment decision variable
        # "parameter" cost of additional quantity
        if self.parent.linear_model_switch:
            self.b.vb_transport_investment = Var(self.b.s_arcs, self.b.s_transport_options, domain=pyo.NonNegativeReals, initialize=0, bounds=(0, self.b.p_M_supply))
        else:
            self.b.vb_transport_investment = Var(self.b.s_arcs, self.b.s_transport_options, domain=pyo.Binary, initialize=0)  

        # Added transportation capacity variable
        self.b.v_transport_cap_add = Var(self.b.s_arcs, self.b.s_transport_options, domain=pyo.NonNegativeReals, bounds=(0, self.b.p_M_supply))  # decision var for additional quantity

        # Storage investment decision variable
        self.b.v_storage_investment = Var(self.b.s_nodes_storage, domain=NonNegativeReals, bounds=(0, self.b.p_M_storage))  # "parameter" cost of additional quantity

        # Storage max injection capacity variable
        self.b.v_storage_injectivity = pyo.Var(self.b.s_nodes_storage, domain=NonNegativeReals, bounds=(0, self.b.p_M_supply))

        # Slack variable
        self.b.va_slack_storage = pyo.Var(self.b.s_nodes_storage, domain=NonNegativeReals, bounds=(0, self.b.p_M_supply))  # supporting decision var

        def slack_demand_rule(m, n):
            return (0, (m.p_co2_demand[n]))  # bounds have to be nonnegative; need the - sign in the rule
        
        self.b.va_slack_demand_eor = pyo.Var(self.b.s_nodes_demand, bounds=slack_demand_rule)


        ### Define additional variables to catch infeasibility on supply constraints
        # Infeasibility Supply
        self.b.va_excess_supply = pyo.Var(self.b.s_nodes_supply, self.b.s_policy_options, domain=NonNegativeReals, bounds=(0, self.b.p_M_supply))

        # Infeasibility Trans-shipment
        self.b.va_excess_ts_in  = pyo.Var(self.b.s_nodes_trans_ship, self.b.s_policy_options, domain=NonNegativeReals, bounds=(0, self.b.p_M_supply))
        self.b.va_excess_ts_out = pyo.Var(self.b.s_nodes_trans_ship, self.b.s_policy_options, domain=NonNegativeReals, bounds=(0, self.b.p_M_supply))

        pass


    def declare_constraints(self):
        '''Declare CCATS block-level constraints.

        Parameters
        ----------
        
        None

        Returns
        -------
        
        c_flow_total_rule : Pyomo Constraint
            Pyomo constraint asserting that total flow == flow from existing pipelines + flow from new pipelines.

        c_flow_by_policy : Pyomo Constraint
            Pyomo constraint asserting that flow by policy type == total flow.

        c_flow_balance_supply : Pyomo Constraint
            Pyomo constraint asserting that CO\ :sub:`2` out of CO\ :sub:`2` supply nodes == CO\ :sub:`2` supplied to the model.

        c_flow_balance_transshipment : Pyomo Constraint
            Pyomo constraint asserting that CO\ :sub:`2` out of trans-shipment nodes == CO\ :sub:`2` into trans-shipment nodes.

        c_flow_balance_demand_storage : Pyomo Constraint
            Pyomo constraint asserting that CO\ :sub:`2` flow to CO\ :sub:`2` storage doesn't exceed storage capacity.

        c_flow_balance_demand_eor : Pyomo Constraint
            Pyomo constraint asserting that CO\ :sub:`2` flow to CO\ :sub:`2` demand doesn't exceed demand.

        c_t0_flow_base : Pyomo Constraint
            Pyomo constraint asserting that no CO\ :sub:`2` transport capacity can be added in time period 1.

        c_transport_capacity_added_min : Pyomo Constraint
            Pyomo constraint asserting minimum CO\ :sub:`2` transportation capacity that can be added/arc.

        c_transport_capacity_added_max : Pyomo Constraint
            Pyomo constraint asserting maximum CO\ :sub:`2` transportation capacity that can be added/arc.

        c_transport_selection : Pyomo Constraint
            Pyomo constraint asserting only one transportation option allowed to be added/arc.

        c_vb_transport_investment_dummy : Pyomo Constraint
            Pyomo constraint to constrain vb_transport_investment when solved as a linear model.
        '''
        ### Flow balance
        # Enforce total flow
        @self.b.Constraint(self.b.s_arcs)
        def c_flow_total_rule(m, i, j):
            return m.v_flow[(i, j)] \
                == m.v_flow_base[(i, j)] \
                + m.v_flow_add[(i, j)]

        # Enforce flow by policy, the total flow in an arc is the sum of the policy flows
        def flow_by_policy_rule(m, i, j):
            return m.v_flow[i, j] == sum(m.v_flow_by_policy[i, j, p] for p in m.s_policy_options)
        self.b.c_flow_by_policy = pyo.Constraint(self.b.s_arcs, rule=flow_by_policy_rule)

        # Enforce flow balance
        @self.b.Constraint(self.b.s_nodes_supply, self.b.s_policy_options)
        def c_flow_balance_supply(m, n, p):
            return (sum(m.v_flow_by_policy[a, p] for a in m.s_arcs_in[n]) - sum(m.v_flow_by_policy[a, p] for a in m.s_arcs_out[n])
                    - m.va_excess_supply[n, p]
                    == - m.p_co2_supply[n, p])

        # Enforce flow balance at transshipment nodes - has to be zero
        @self.b.Constraint(self.b.s_nodes_trans_ship, self.b.s_policy_options)
        def c_flow_balance_transshipment(m, n, p):
            return (sum(m.v_flow_by_policy[a, p] for a in m.s_arcs_in[n]) - sum(m.v_flow_by_policy[a, p] for a in m.s_arcs_out[n])
                    + m.va_excess_ts_out[n, p] - m.va_excess_ts_in[n, p]
                    == 0)

        # Enforce flow balance at storage nodes - can have slack from inequality constraint
        # Sign for slack_demand has to match how the variable was declared above
        @self.b.Constraint(self.b.s_nodes_storage)
        def c_flow_balance_demand_storage(m, n):
            return (sum(m.v_flow[a] for a in m.s_arcs_in[n]) \
                    - sum(m.v_flow[a] for a in m.s_arcs_out[n]) \
                    + m.va_slack_storage[n]
                    == m.v_storage_injectivity[n])
                

        # Enforce flow balance at EOR nodes nodes - can have slack from inequality constraint
        # Sign for slack_demand has to match how the variable was declared above
        @self.b.Constraint(self.b.s_nodes_demand)
        def c_flow_balance_demand_eor(m, n):
            return (sum(m.v_flow[a] for a in m.s_arcs_in[n]) \
                   - sum(m.v_flow[a] for a in m.s_arcs_out[n]) \
                   + m.va_slack_demand_eor[n]
                   == m.p_co2_demand[n])

        # Enforce base flow  must be less than existing transportation capacity
        @self.b.Constraint(self.b.s_arcs)
        def c_t0_flow_base(m, i, j):
            return m.v_flow_base[i, j] <= m.p_transport_cap_existing[i, j]


        ### Investment Decision and Selection
        # Enforce capacity added is more than selected pipeline minimum capacity
        if self.parent.linear_model_switch == False: # Only applies to MILP version (Linear program lower bound is always 0)
            @self.b.Constraint(self.b.s_arcs, self.b.s_transport_options)
            def c_transport_capacity_added_min(m, i, j, d):
                return m.p_transport_cap_add_min[i, j, d] * m.vb_transport_investment[i, j, d] <= \
                    m.v_transport_cap_add[i, j, d]

        # Enforce capacity added is less than selected pipeline capacity
        if self.parent.linear_model_switch: # Linear model does not utilize binary investment variable
            @self.b.Constraint(self.b.s_arcs, self.b.s_transport_options)
            def c_transport_capacity_added_max(m, i, j, d):
                return m.v_transport_cap_add[i, j, d] <= \
                    m.p_transport_cap_add_max[i, j, d]
        else:
            @self.b.Constraint(self.b.s_arcs, self.b.s_transport_options)
            def c_transport_capacity_added_max(m, i, j, d):
                return m.v_transport_cap_add[i, j, d] <= \
                    m.p_transport_cap_add_max[i, j, d] * m.vb_transport_investment[i, j, d]

        # Enforce only one transport option per arc
        if self.parent.linear_model_switch == False: # Only applies to MILP version (linear model only has one transport option per arc)
            @self.b.Constraint(self.b.s_arcs)
            def c_transport_selection(m, i, j):
                return sum(m.vb_transport_investment[(i, j, d)] for d in m.s_transport_options) <= 1


        ### Dummy constraint (linear model only) - ensures that m.vb_transport_investment has a value
        if self.parent.linear_model_switch:
            @self.b.Constraint(self.b.s_arcs, self.b.s_transport_options)
            def vb_transport_investment_dummy(m, i, j, d):
                return m.v_transport_cap_add[i, j, d] == m.vb_transport_investment[i, j, d]

        pass


    def declare_objective(self):
        '''Declare objective function.

        Parameters
        ----------
        
        None

        Returns
        -------
        
        self.b.e_sum_flow_arcs_in : Pyomo Expression
            Sum of flows into each node.

        self.b.e_sum_policy : Pyomo Expression
            Sum of policy costs (including tax credits).

        self.b.e_sum_costs_investment : Pyomo Expression
            Sum of investment costs.

        self.b.e_sum_costs_variable : Pyomo Expression
            Sum of variable costs.
        
        self.b.e_sum_excess : Pyomo Expression
            Costs of slack (infeasibilities).
        
        self.b.e_sum_costs : Pyomo Expression
            Costs exclusive of slack (infeasibilities).

        self.b.e_sum_all_costs : Pyomo Expression
            Costs inclusive of slack (infeasibilities).
        
        self.b.objective : Pyomo Objective
            Pyomo block-level objective function minimizing total costs.
        '''
        # Summation of flow for arcs_in
        def sum_flow_arcs_in_rule(m, n):
            return sum(m.v_flow[a] for a in m.s_arcs_in[n])
        self.b.e_sum_flow_arcs_in = pyo.Expression(self.b.s_nodes, rule=sum_flow_arcs_in_rule)

        # Sum policy incentives in demand nodes
        def sum_policy_rule(m):
            return m.p_discount_policy * sum(m.p_policy_cost[n,p] * m.v_flow_by_policy[a,p]
                                             for n in m.s_nodes_sequester
                                             for a in m.s_arcs_in[n]
                                             for p in m.s_policy_options)
        self.b.e_sum_policy = pyo.Expression(rule=sum_policy_rule)

        # Sum Investment Costs
        if self.parent.linear_model_switch: # Linear Program - does not use vb_transport_investment/p_capex_transport_base
            def sum_costs_investment_rule(m):
                return (
                        m.p_discount_invest_transport * sum(m.p_capex_transport_slope[a, d] * m.v_transport_cap_add[a, d]
                                                            for a in m.s_arcs
                                                            for d in m.s_transport_options)
                        + m.p_discount_invest_storage * sum(m.p_capex_storage[n] * m.v_storage_investment[n]
                                                            for n in m.s_nodes_storage)
                        )
        else: # Mixed Integer Linear Program
            def sum_costs_investment_rule(m):
                return (
                        m.p_discount_invest_transport * sum(m.p_capex_transport_base[a, d] * m.vb_transport_investment[a, d] +
                                                            m.p_capex_transport_slope[a, d] * m.v_transport_cap_add[a, d]
                                                            for a in m.s_arcs
                                                            for d in m.s_transport_options)
                        + m.p_discount_invest_storage * sum(m.p_capex_storage[n] * m.v_storage_investment[n]
                                                            for n in m.s_nodes_storage)
                        )
        self.b.e_sum_costs_investment = pyo.Expression(rule=sum_costs_investment_rule)

        # Sum Variable Costs
        def sum_costs_variable_rule(m):
            return (m.p_discount_variable
                    * (  sum(m.p_opex_transport[a] * m.p_electricity_demand[a] * m.v_flow[a]
                             for a in m.s_arcs)
                       + sum(m.p_opex_storage[n] * m.e_sum_flow_arcs_in[n]
                             for n in m.s_nodes_storage)
                       + sum(m.p_co2_demand_cost_net[n] * m.e_sum_flow_arcs_in[n]
                             for n in m.s_nodes_demand)
                       )
                    )
        self.b.e_sum_costs_variable = pyo.Expression(rule=sum_costs_variable_rule)


        # Sum excesses (infeasibilities) to make them expensive
        def sum_excess_rule(m):
            return (sum(m.va_excess_supply[n,p] * m.p_M_excess
                       for n in m.s_nodes_supply
                       for p in m.s_policy_options)
                + sum((m.va_excess_ts_in[n,p] + m.va_excess_ts_out[n,p]) * m.p_M_excess
                      for n in m.s_nodes_trans_ship
                      for p in m.s_policy_options)
                    )
        self.b.e_sum_excess = pyo.Expression(rule=sum_excess_rule)

        # Sum costs rule (no slack)
        def sum_costs_rule(m):
            return (m.e_sum_costs_investment
                + m.e_sum_costs_variable
                + m.e_sum_policy)
        self.b.e_sum_costs = pyo.Expression(rule=sum_costs_rule)

        # Sum all costs inclusive of slack (objective function)
        def sum_all_costs_rule(m):
            return (m.e_sum_costs_investment
                + m.e_sum_costs_variable
                + m.e_sum_policy
                + m.e_sum_excess)
        self.b.e_sum_all_costs = pyo.Expression(rule=sum_all_costs_rule)


        ### Declare block-level objective function
        # minimize total costs
        def objective_rule(m):
            return m.e_sum_all_costs
        self.b.objective = pyo.Objective(rule=objective_rule, sense=pyo.minimize)

        # Deactivate block-level solve of objective function, block level objective functions to be solved simultaneously
        self.b.objective.deactivate()

        pass


    def declare_constraints_across_blocks(self):
        '''Declare constraints across blocks.

        Parameters
        ----------
        
        None

        Returns
        -------
        
        c_flow_added : Pyomo Constrtaint
            Pyomo constraint asserting that block flow added * a transport buffer <= block-level transport capacity added in previous time period

        c_storage_injectivity: Pyomo Constraint
            Pyomo constraint asserting that stored CO\ :sub:`2` <= new storage capacity + existing storage capacity.

        c_storage_cumulative_injection : Pyomo Constraint
            Pyomo constraint asserting that storage volumes <= exceed maximum formation capacity.

        c_storage_aors_available : Pyomo Constraint
            Pyomo constraint asserting that storage investment sites <= remaining storage sites available for investment.
        '''
        ### Constraints across blocks - use dynamic naming so that constraints are not overwritten
        
        for t in self.m.s_time:
            
            # Update transportation capacity based on investment in prior blocks
            def c_flow_added(m, i, j):
                return m.blocks[t].v_flow_add[i, j]  * self.b.transport_buffer <= 0.0 + sum(m.blocks[t_star].v_transport_cap_add[i, j, d]
                                                                 for d in m.blocks[t].s_transport_options
                                                                 for t_star in self.m.s_time if t_star <= t - 1)
            rule_name = "c_flow_added"
            index = "s_arcs"
            command = "self.m.c_" + rule_name + "_t%i = pyo.Constraint(self.m.blocks[%i]." + index + ", rule=" + rule_name + ")"
            exec(command % (t, t))

            # Update injectivity based on storage investments in prior blocks
            def c_storage_injectivity(m, n):
                return (m.blocks[t].v_storage_injectivity[n] == m.blocks[t].p_storage_injectivity_existing[n]
                        + m.blocks[t].p_storage_injectivity_new[n] * sum(m.blocks[t_star].v_storage_investment[n] / self.b.storage_buffer
                                                                         for t_star in self.m.s_time if
                                                                         t_star <= t - 1))
            rule_name = "c_storage_injectivity"
            index = "s_nodes_storage"
            command = "self.m.c_" + rule_name + "_t%i = pyo.Constraint(self.m.blocks[%i]." + index + ", rule=" + rule_name + ")"
            exec(command % (t, t))

            # Track cumulative injection into saline storage
            def c_storage_cumulative_injection(m, n):
                return (sum(m.blocks[t_star].e_sum_flow_arcs_in[n] * m.blocks[t_star].p_duration for t_star in self.m.s_time if t_star <= t) <=
                        m.blocks[t].p_storage_net_existing[n] +
                        m.blocks[t].p_storage_net_adder[n] *
                        sum(m.blocks[t_star].v_storage_investment[n]
                            for t_star in self.m.s_time if t_star <= t - 1))
            rule_name = "c_storage_cumulative_injection"
            index = "s_nodes_storage"
            command = "self.m.c_" + rule_name + "_t%i = pyo.Constraint(self.m.blocks[%i]." + index + ", rule=" + rule_name + ")"
            exec(command % (t, t))

            # Track AORs available for storage
            def c_storage_aors_available(m,n):
                return (sum(m.blocks[t_star].v_storage_investment[n] for t_star in self.m.s_time if t_star <= t)
                        <= m.blocks[t].p_storage_aors_available[n])
            rule_name = "c_storage_aors_available"
            index = "s_nodes_storage"
            command = "self.m.c_" + rule_name + "_t%i = pyo.Constraint(self.m.blocks[%i]." + index + ", rule=" + rule_name + ")"
            exec(command % (t, t))
        pass


    def declare_objective_across_blocks(self):
        '''Declare objective across blocks.

        Parameters
        ----------

        None

        Returns
        -------
        
        self.m.e_sum_blocks : Pyomo Expression
            Pyomo Expression summing all costs across blocks.
        
        self.m.objective_multiblock : Pyomo Objective
            Pyomo objective function minimizing total costs across blocks.
        '''

        ### Objective Rule
        def sum_blocks_rule(m):
            return sum(m.blocks[t].e_sum_all_costs for t in m.s_time)
        self.m.e_sum_blocks = pyo.Expression(rule=sum_blocks_rule)

        def objective_multiblock_rule(m):
            return m.e_sum_blocks
        self.m.objective_multiblock = pyo.Objective(rule=objective_multiblock_rule, sense=pyo.minimize)

        pass


    def solve_optimization(self):
        '''Solve optimization and commit *self.m* model to parent level variable for use by other CCATS submodules.

        Parameters
        ----------
        
        None

        Returns
        -------
        
        self.m.objective_value : Float
            Copy of multiblock objective result.

        self.parent.pyomo_model : Pyomo model
            Copy of self.m.

        self.parent.pyomo_block0 : Pyomo block
            Copy of self.m.blocks[0] for easier access in postprocessor.

        self.parent.pyomo_block1  : Pyomo block
            Copy of self.m.blocks[0] for easier access in postprocessor.

        self.parent.pyomo_block2  : Pyomo block
            Copy of self.m.blocks[0] for easier access in postprocessor.
        
        self.parent.model_name : String
            Name of optimization model ('ccats_opt').
        '''
        self.logger.info("Constructing pyomo model")

        # Add Duals
        if self.parent.linear_model_switch:
            self.m.dual = pyo.Suffix(direction=pyo.Suffix.IMPORT)

        # Declare Solver and set solver options
        opt = pyo.SolverFactory(self.solver)
        opt.options['SOLTIMELIMIT'] = self.soltimelimit
        opt.options['THREADS'] = self.threads
        opt.options['MAXMEMORYSOFT'] = self.maxmemorysoft

        if self.solver in ['xpress_persistent', 'appsi_highs']: # does not work with xpress or xpress_direct
            opt.set_instance(self.m)

        ### Write Model to .mps and .lp
        if self.parent.debug_switch == True:
            self.m.write(self.output_path + 'model_file\\' + 'ccats_model_' + str(self.year_current) + '.mps')
            self.m.write(self.output_path + 'model_file\\' + 'ccats_model_' + str(self.year_current) + '.lp')

        # Solve Model
        self.logger.info('Start solve.')
        try:
            if self.debug_opt_log == True:
                if self.solver in ['xpress', 'xpress_direct', 'xpress_persistent']:
                    results = opt.solve(self.m, tee=True, load_solutions=False, keepfiles=True, logfile="main_opt_log.log")
                else:
                    results = opt.solve(self.m, tee=True, load_solutions=False) # HiGHS
                pyo.check_optimal_termination(results)
                solver_exception = None
            else:
                if self.solver in ['xpress', 'xpress_direct', 'xpress_persistent']:
                    results = opt.solve(self.m, tee=False, load_solutions=False, keepfiles=True, logfile="main_opt_log.log")
                else:
                    results = opt.solve(self.m, tee=False, load_solutions=False) # HiGHS
                pyo.check_optimal_termination(results)
                solver_exception = None
        except Exception as e:
            results = None
            solver_exception = None

        # Check results and load model solutions
        # Check results for termination condition and solution status
        if com.check_results(results, TerminationCondition, SolutionStatus):
            name = "noclass!"
            self.logger.info(f"[{name}] Solve failed")
            com_bug.compute_infeasibility_explanation(self.m, solver=pyo.SolverFactory(self.solver))
            if results is not None:
                self.logger.info("status=" + str(results.solver.status))
                self.logger.info("TerminationCondition=" + str(results.solver.termination_condition))

        # If model solved, load model solutions into model, else exit
        try:
            if (results.solver.status == SolverStatus.ok) and (
                    results.solver.termination_condition == TerminationCondition.optimal):
                self.m.solutions.load_from(results)
            else:
                self.logger.warning('Solve Failed.')
                com_bug.compute_infeasibility_explanation(self.m, solver=pyo.SolverFactory(self.solver))
                exit()
        except:
            self.logger.warning('Solve Failed.')
            com_bug.compute_infeasibility_explanation(self.m, solver=pyo.SolverFactory(self.solver))
            exit()

        self.logger.info("End solve.")

        # Log and write results
        self.logger.info("Displaying solution...")
        self.m.objective_value = self.m.objective_multiblock().copy() # make copy of objective multiblock because it takes 0.5 seconds to call
        self.logger.info(f"m.objective(): {self.m.objective_value}")

        # Write optimization to parent variable
        self.parent.pyomo_model = self.m
        self.parent.pyomo_block0 = self.m.blocks[0]
        self.parent.pyomo_block1 = self.m.blocks[1]
        self.parent.pyomo_block2 = self.m.blocks[2]
        self.parent.model_name = 'ccats_opt'

        pass


    def solve_linearized_optimization(self):
        """Re-solve the optimization problem after linearizing and fixing investment variables.
        
        Parameters
        ----------

        None
        
        Returns
        -------

        self.parent.pyomo_model : Pyomo model
            Copy of self.m_linear

        self.parent.pyomo_block0 : Pyomo block
            Copy of self.m_linear.blocks[0] for easier access in postprocessor.

        self.parent.pyomo_block1  : Pyomo block
            Copy of self.m_linear.blocks[0] for easier access in postprocessor.

        self.parent.pyomo_block2  : Pyomo block
            Copy of self.m_linear.blocks[0] for easier access in postprocessor.
        
        self.parent.model_name : String
            Name of optimization model ('ccats_opt').
        """
        ### Linearize model and rerun
        # Get linear version of model
        self.logger.info('Transform Model to Linearized Form')
        xfrm = pyo.TransformationFactory('core.relax_integer_vars')
        xfrm.apply_to(self.m)
        self.m_linear = self.m

        # Fix binary variables
        # Round to nearest whole number
        self.logger.info('Round binary variables to nearest whole number')
        block_0_transport_inv = com.unpack_pyomo(self.m_linear.blocks[0].vb_transport_investment, pyo.value, 1)
        block_0_transport_inv['variable value'] = block_0_transport_inv['variable value'].abs()
        block_0_transport_inv['variable value'] = block_0_transport_inv['variable value'].round()
        block_0_transport_inv = block_0_transport_inv.set_index(0)

        block_1_transport_inv = com.unpack_pyomo(self.m_linear.blocks[1].vb_transport_investment, pyo.value, 1)
        block_1_transport_inv['variable value'] = block_1_transport_inv['variable value'].abs()
        block_1_transport_inv['variable value'] = block_1_transport_inv['variable value'].round()
        block_1_transport_inv = block_1_transport_inv.set_index(0)

        # Assign rounded values to binary variable
        for a in self.m.blocks[0].vb_transport_investment:
           self.m.blocks[0].vb_transport_investment[a] = block_0_transport_inv.at[a, 'variable value']

        for a in self.m.blocks[1].vb_transport_investment:
           self.m.blocks[1].vb_transport_investment[a] = block_1_transport_inv.at[a, 'variable value']

        # Fix binary variable
        self.logger.info('Fix binary variables')
        self.m_linear.blocks[0].vb_transport_investment.fix()
        self.m_linear.blocks[1].vb_transport_investment.fix()
        self.m_linear.blocks[2].vb_transport_investment.fix()


        ### Relax MIP
        # Declare new solver instance
        opt_linear = pyo.SolverFactory(self.solver)
        opt_linear.options['SOLTIMELIMIT'] = self.soltimelimit
        opt_linear.options['THREADS'] = self.threads
        opt_linear.options['MAXMEMORYSOFT'] = self.maxmemorysoft
 
        if self.solver in ['xpress_persistent', 'appsi_highs']: # does not work with xpress or xpress_direct
            opt_linear.set_instance(self.m_linear)

        ### Add Duals
        self.logger.info('Add Duals')
        self.m_linear.dual = pyo.Suffix(direction=pyo.Suffix.IMPORT)

        ### Solve Model
        self.logger.info('Start solve.')
        try:
            if self.debug_opt_log == True:
                if self.solver in ['xpress', 'xpress_direct', 'xpress_persistent']:
                    results = opt_linear.solve(self.m, tee=True, load_solutions=False, keepfiles=True, logfile="main_opt_log.log")
                else:
                    results = opt_linear.solve(self.m, tee=True, load_solutions=False) # HiGHS
                pyo.check_optimal_termination(results)
                solver_exception = None
            else:
                if self.solver in ['xpress', 'xpress_direct', 'xpress_persistent']:
                    results = opt_linear.solve(self.m, tee=False, load_solutions=False, keepfiles=True, logfile="main_opt_log.log")
                else:
                    results = opt_linear.solve(self.m, tee=True, load_solutions=False) # HiGHS
                pyo.check_optimal_termination(results)
                solver_exception = None
        except Exception as e:
            results = None
            solver_exception = None


        ### Check results and load model solutions
        # Check results for termination condition and solution status
        if com.check_results(results, TerminationCondition, SolutionStatus):
            name = "noclass!"
            self.logger.info(f"[{name}] Solve failed")
            com_bug.compute_infeasibility_explanation(self.m_linear, solver=pyo.SolverFactory(self.solver))
            if results is not None:
                self.logger.info("status=" + str(results.solver.status))
                self.logger.info("TerminationCondition=" + str(results.solver.termination_condition))

        # If model solved, load model solutions into model, else exit
        try:
            if (results.solver.status == SolverStatus.ok) and (
                    results.solver.termination_condition == TerminationCondition.optimal):
                self.m_linear.solutions.load_from(results)
            else:
                self.logger.warning('Solve Failed.')
                com_bug.compute_infeasibility_explanation(self.m_linear, solver=pyo.SolverFactory(self.solver))
                exit()
        except:
            self.logger.warning('Solve Failed.')
            com_bug.compute_infeasibility_explanation(self.m_linear, solver=pyo.SolverFactory(self.solver))
            exit()

        self.logger.info("End solve.")

        # Write optimization to parent variable
        self.parent.pyomo_model     = self.m_linear
        self.parent.pyomo_block0    = self.m_linear.blocks[0]
        self.parent.pyomo_block1    = self.m_linear.blocks[1]
        self.parent.pyomo_block2    = self.m_linear.blocks[2]
        self.parent.model_name      = 'ccats_opt'

        pass
