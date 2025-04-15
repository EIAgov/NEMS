"""Submodule for calculating offshore well production.

Summary
_______

This submodule provides a field-level micro-economic model for characterizing the oil and gas resource potential in the
United States Outer Continental Shelf.  The model currently handles the Western and Central Gulf of Mexico, Eastern Gulf of Mexico,
Atlantic OCS, and Pacific OCS. The module operates as follows:

     1. The module is initialized in the onshore class *__init__* method, with class dataframes and variables being declared here.

    2. The *setup* method is called, which reads in the necessary input files via .csv or .pkl (see **intermediate_var_pickle.py**) format.
       From *setup* the following methods are called to load in different input variable classes:

        a. *load_input_variables* - Reads variable data from self.setup_table
        b. *load_input_tables* - Reads Offshore input tables in as DataFrames
        c. *load_announced* - Performs one-time setup for announced fields
        d. *run_producing_resources* - Performs one-time calculation for producing field resources
        e. *run_producing_properties* - Performs one-time calculation for producing field properties
        f. *run_producing_production* - Performs one-time production calculation for producing fields
        g. *determine_undiscovered_fields* - Calculates distribution of undiscovered fields
        h. *calculate_drilling_capacity* - Determine model drilling capacity by rig type
        i. *side_case_adjustments* - Adjust U=undiscovered project EURs for Oil and Gas Supply Cases

    3. The *run* method is called, which kicks off the main model functions.

    4. Technically recoverable resources are calculated for discovered fields in *calculate_resources*.

    5. New field wildcats are calculated for each evaluation unit in *determine_nfws*.

    6. Newly discovered fields and their properties are calculated.

    7. In *load_fields* newly discovered fields and announced fields are added to the main self.fields DataFrame
       for processing.

    8. Calculate crude oil and natural gas by region based on price values from LFMM and NGMM in *load_prices*.

    9. For the loaded fields, calculate annual drilling in *calculate_drilling*.

    10. Calculate exploration wells and costs for loaded fields in *calculate_exploration_cost*.

    11. For loaded fields, calculate the number of delay years between drilling being discovered and being eligible for production
        in *calculate_delay_years*.

    12. Calculate various costs in *calculate_platform_costs*, *calculate_development_cost*, and *calculate_operating_cost*,
        and load these costs into the cashflow.

    13. Load cashflow in *load_cash_flow_properties* and *load_cash_flow_production*.

    14. Run the cashflow

    15. Rank fields based on profitability, apply drilling constraints and select fields in *rank_fields* and *select_fields*.

    16. Expand drilling constraints if they are exceeded in *expand_drilling_capacity*

    17. Calculate ngpl volumes as proportion of natural gas production

    18. Run *write_intermediate_variables* to store iterative calculation tables for the next model run year. These tables are written out to .pkl files in
        **intermediate_var_pickle.py**.

    19. *report_results_unf* is called from **module_unf.py** to report results to debug files and the restart file.


Input Files
___________
    * off_mapping.csv - Offshore Mapping
    * off_water_depth.csv - Water Depth Mapping
    * off_drill_depth.csv - Drill Depth Mapping
    * off_delineation_wells.csv - Delineation Drilling Mapping
    * off_exp_cost.csv - Exploration Cost
    * off_dev_cost.csv - Development Cost
    * off_development_wells.csv - Development Drilling Mapping
    * off_delays.csv - Delays Based On Depth And Wells
    * off_platform_delays.csv - Delays Based On Depth And Platform Type
    * off_platform_slots.csv - Slots By Water Depth And FSC
    * off_platform_structure_type.csv - Platform Type By Water Depth And FSC
    * off_platform_abandonment.csv - Abandonment Fraction By Facility Type
    * off_announced_fields.csv - Announced Fields
    * off_drill_per_year.csv - Drilling Per Year
    * off_platform_drill_per_year.csv - Drilling Per Year And Platform Type
    * off_field_size_classes.csv - Standard Field Size Class Resources
    * off_operating_cost.csv - Operating Costs
    * off_gas_ratio_and_cond.csv - Gas Ratios And Condensate Ratios
    * off_transportation.csv - Gas And Oil Transportation Costs/Unit
    * off_royalty - self.royalty_rates              = pd.DataFrame()  #: DataFrame of royalty rates
    * off_prod_fac_cost_frac.csv - Fraction Of Distributed Facility Costs
    * off_producing_fields.csv - Producing Fields
    * off_prod_profile.csv - Production Profile Params For Producing Fields
    * off_1990_fields.csv - Fields Discovered Before 1990
    * off_cumulative_nfws.csv - New Field Wildcats At Zero Year
    * off_undiscovered_fields.csv - Undiscovered Fields After 1990
    * off_field_availability.csv - Fields Availability By Evaluation Unit
    * off_nfw_coefficients.csv - NFW Drilling Rate Coefficients
    * off_discovery_coefficients.csv - Discovery Coefficients
    * off_undiscovered_production.csv - Undiscovered Production Parameters
    * off_exp_success_rate.csv - Exploration Success Rate


Model Functions and Class Methods
_________________________________

class Offshore(sub.Submodule) - Offshore submodule for HSM

    * __init__ - Constructor to initialize Offshore submodule
    * setup - Method to set up Offshore Submodule for HSM
    * load_input_variables - Method to read input variable data
    * load_input_tables - Method to read table data from input files
    * load_announced - Method to perform one-time setup for announced fields
    * run_producing_resources - Method to perform one-time calculation for producing field resources
    * run_producing_properties - Method to perform one-time calculation for producing field properties
    * run_producing_production - Method to perform one-time production calculation for producing fields
    * determine_undiscovered_fields - Method to calculate distribution of undiscovered fields
    * run - Method to run Offshore module for HSM
    * load_prices - Method to calculate average prices for Offshore regions
    * calculate_drilling_capacity - Method to determine drilling capacity by rig type
    * calculate_resources - Method to calculate technically recoverable resources based on available undiscovered fields
    * determine_nfws - Method to calculate number of new field wildcats drilling in each evaluation unit
    * calculate_new_fields - Method to calculate number of new fields discovered based on new field wildcat drilling
    * calculate_new_field_properties - Method to calculate properties (e.g. drilling depth, water depth) for discovered fields
    * load_fields - Method to load available announced and discovered fields for economic analysis
    * calculate_exploration_cost - Method, for the loaded fields, to calculate exploration cost
    * calculate_delay_years - Method, for the loaded fields, to calculate the number of delay years
    * calculate_platform_cost - Method, for the loaded fields, to calculate platform, flowline, and abandonment costs
    * calculate_drilling - Method, for the loaded fields, to calculate yearly drilling
    * calculate_development_cost - Method, for the loaded fields, to calculate development drilling costs
    * calculate_operating_cost - Method, for the loaded fields, to calculate operating costs
    * load_cash_flow_properties - Method, for the loaded fields, to load CashFlow properties in preparation for NPV/DCF calculation
    * load_cash_flow_production - Method, for the loaded fields, to load CashFlow object production in preparation for NPV/DCF calculation
    * run_cash_flow - Method, for the loaded fields, to run through CashFlow
    * rank_fields - Method to rank Fields by net present value
    * select_fields - Method to select fields based on rank, net present value and rig capacity
    * expand_drilling_capacity - Method to develop new drilling capacity based on rig utilization
    * calculate_ngpls - Method to calculate production of NGPLs
    * report_results_hdf/unf - Method to report results to restart variables


Output Debug Files
__________________

    * off_discovery_disc.csv - Offshore discovered field distribution by evaluation unit per year
    * fields.csv - Multiple resources and rates
    * off_drilling_rig_constraint.csv - Constraints by type of drilling rig
    * off_fields.csv - Discovered and producing fields by evaluation unit per year
    * off_fields_selected.csv - Multiple resources and rates
    * off_cumulative_nfws.csv - Offshore cumulative new field wildcats by evaluation unit per year


Output Restart Variables
________________________

REAL OGPRDOFF(6,2,MNUMYR)         ! Lower 48 offshore production split into Federal/State categories
REAL OGQCRREP(MNOGCRO,MNUMYR)   ! CRUDE PRODUCTION BY OIL CAT
REAL OGQNGREP(MNOGCAT,MNUMYR)   ! NG PRODUCTION BY GAS CAT
REAL OGNGPLBU(OGDIST,MNUMYR)      ! Butane production
REAL OGNGPLET(OGDIST,MNUMYR)      ! Ethane production
REAL OGNGPLIS(OGDIST,MNUMYR)      ! Isobutane production
REAL OGNGPLPP(OGDIST,MNUMYR)      ! Pentanes plus production
REAL OGNGPLPR(OGDIST,MNUMYR)      ! Propane production
REAL OGNGPLPRD(OGDIST,MNUMYR)     ! Natural gas plant output by NGPL region
REAL OGNOWELL(MNUMYR)           ! WELLS COMPLETED
REAL OGOILPRD(OGDIST,OILTYPES,MNUMYR)     ! crude oil production

Offshore Submodule Class Methods
________________________________
"""

import pandas as pd
import numpy as np
import names as nam
import submodule as sub
import common as com
import drilling_equations as drill_eq
import warnings
import cash_flow as cf
from pathlib import Path


class Offshore(sub.Submodule):
    """Offshore submodule for HSM.

    Parameters
    ----------
    parent : module_unf.Module
        Pointer to head module
    """

    def __init__(self, parent):

        super().__init__(parent, submodule_name='offshore')
        # input tables
        self.mapping                    = pd.DataFrame()  #: DataFrame of mapping
        self.water_depth                = pd.DataFrame()  #: DataFrame of water depth mapping
        self.drill_depth                = pd.DataFrame()  #: DataFrame of drill depth mapping
        self.delineation_wells          = pd.DataFrame()  #: DataFrame of delineation drilling mapping
        self.exp_cost                   = pd.DataFrame()  #: DataFrame of exploration cost
        self.dev_cost                   = pd.DataFrame()  #: DataFrame of development cost
        self.development_wells          = pd.DataFrame()  #: DataFrame of development drilling mapping
        self.delays                     = pd.DataFrame()  #: DataFrame of delays based on depth and wells
        self.platform_delays            = pd.DataFrame()  #: DataFrame of delays based on depth and platform type
        self.platform_slots             = pd.DataFrame()  #: DataFrame of slots by water depth and FSC
        self.platform_structure_type    = pd.DataFrame()  #: DataFrame of platform type by water depth and FSC
        self.platform_abandonment       = pd.DataFrame()  #: DataFrame of abandonment fraction by facility type
        self.announced_fields           = pd.DataFrame()  #: DataFrame of announced fields
        self.drill_per_year             = pd.DataFrame()  #: DataFrame of drilling per year
        self.platform_drill_per_year    = pd.DataFrame()  #: DataFrame of drilling per year and platform type
        self.drill_rig_constraint       = pd.DataFrame()  #: DataFrame of drilling rig constraints
        self.field_size_classes         = pd.DataFrame()  #: DataFrame of standard field size class resources
        self.operating_cost             = pd.DataFrame()  #: DataFrame of operating costs
        self.gas_ratio_and_cond         = pd.DataFrame()  #: DataFrame of gas ratios and condensate ratios
        self.transportation             = pd.DataFrame()  #: DataFrame of gas and oil transportation costs/unit
        self.royalty_rates              = pd.DataFrame()  #: DataFrame of royalty rates
        self.prod_fac_cost_frac         = pd.DataFrame()  #: DataFrame of fraction of distributed facility costs
        self.producing_fields           = pd.DataFrame()  #: DataFrame of producing fields
        self.prod_profile               = pd.DataFrame()  #: DataFrame of production profile params for producing fields
        self.off_1990_fields            = pd.DataFrame()  #: DataFrame of fields Discovered before 1990
        self.cumulative_nfws            = pd.DataFrame()  #: DataFrame of new field wildcats at zero year
        self.undiscovered_fields        = pd.DataFrame()  #: DataFrame of fields undiscovered fields after 1990
        self.field_availability         = pd.DataFrame()  #: DataFrame of fields availability by evaluation unit
        self.nfw_coefficients           = pd.DataFrame()  #: DataFrame of nfw drilling rate coefficients
        self.discovery_coefficients     = pd.DataFrame()  #: DataFrame of discovery coefficients
        self.undiscovered_production    = pd.DataFrame()  #: DataFrame of undiscovered production parameters
        self.exp_success_rate           = pd.DataFrame()  #: DataFrame of exploration success rate

        # tables for internal calculations
        self.off_reg_crude_price        = pd.DataFrame()  #: DataFrame of fixed model iteration 1 crude prices to stop oscillations in NEMS
        self.off_reg_natgas_price       = pd.DataFrame()  #: DataFrame of fixed model iteration 1 natural gas prices to stop oscillations in NEMS
        self.fields                     = pd.DataFrame()  #: DataFrame of active fields
        self.fields_schedule            = pd.DataFrame()  #: DataFrame of active fields
        self.total_fields               = pd.DataFrame()  #: DataFrame of total count of fields by fsc
        self.discovered_field_dist      = pd.DataFrame()  #: DataFrame of total count of discovered fields by fsc (distribution)
        self.discovered_fields          = pd.DataFrame()  #: DataFrame of discovered fields in queue for cash flow
        self.technically_recoverable    = pd.DataFrame()  #: DataFrame technically recoverable resources
        self.fields_selected            = pd.DataFrame()  #: DataFrame of selected fields
        self.crude_production           = pd.DataFrame()  #: DataFrame of all crude production
        self.natgas_production          = pd.DataFrame()  #: DataFrame of all natural gas production
        self.nagas_production           = pd.DataFrame()  #: DataFrame of non-associated natural gas production
        self.adgas_production           = pd.DataFrame()  #: DataFrame of associated-dissolved natural gas production
        self.field_crude_production     = pd.DataFrame()  #: DataFrame of developing field crude production
        self.field_natgas_production    = pd.DataFrame()  #: DataFrame of developing field natgas production
        self.wells                      = pd.DataFrame()  #: DataFrame of all producing wells
        self.exploratory_wells          = pd.DataFrame()  #: DataFrame of all exploratory wells
        self.ad_fraction                = pd.DataFrame()  #: DataFrame of ad gas fraction of total gas production
        self.ngpl_production            = pd.DataFrame()  #: DataFrame of all ngpl production
        self.ethane_production          = pd.DataFrame()  #: DataFrame of all ethane (ngpl) production
        self.propane_production         = pd.DataFrame()  #: DataFrame of all propane (ngpl) production
        self.butane_production          = pd.DataFrame()  #: DataFrame of all butane (ngpl) production
        self.isobutane_production       = pd.DataFrame()  #: DataFrame of all isobutane (ngpl) production
        self.proplus_production         = pd.DataFrame()  #: DataFrame of all natural gasoline (ngpl) production

        # variables for internal calculations
        self.zero_year                      = 0    # Zero year for offshore submodule (equal to AEO year - 1)
        self.averaging_years                = 0    # number of past year to average price
        self.wells_per_flowline             = 0    # maximum number of wells per flowline
        self.exp_success_improve_rate       = 0.0  # rate (percent per year) of exp success improvement
        self.exp_delay_improve_rate         = 0.0  # rate (percent per year) of exp delay improvement
        self.drill_cost_improve_rate        = 0.0  # rate (percent per year) of drilling cost reduction
        self.delay_improve_rate             = 0.0  # rate (percent per year) of platform delay reduction
        self.platform_cost_improve_rate     = 0.0  # rate (percent per year) of platform cost reduction
        self.operating_cost_improve_rate    = 0.0  # rate (percent per year) of operating cost reduction
        self.base_oil_prc                   = 0.0  # base oil price for nfw drilling
        self.base_gas_prc                   = 0.0  # base gas price for nfw drilling
        self.oil_price_cost_base            = 0.0  # base oil price for price-based cost adjustments
        self.platform_cost_year             = 0    # base year for platform costs
        self.drill_cost_base                = 0    # base year for exploratory costs
        self.drill_price_ratio              = 0.0  # fraction ratio between drilling and crude price
        self.pipeline_diameter              = 0.0  # diameter of pipelines in inches
        self.evaluation_years               = 0    # number of years to evaluate projects
        self.boe_to_mcf                     = 0.0  # conversion factor from boe to mcf for natural gas
        self.fed_tax_rate                   = 0.0  # federal tax rate (fraction)
        self.exp_tang_frac                  = 0.0  # exploratory fraction of tangible expenses
        self.dev_tang_frac                  = 0.0  # developmental fraction of tangible expenses
        self.kap_tang_frac                  = 0.0  # capital fraction of tangible expenses
        self.intang_amor_frac               = 0.0  # fraction of tangible amortized
        self.amor_schedule                  = ''   # amortization schedule
        self.deprec_schedule                = ''   # depreciation schedule
        self.discount_rate                  = 0.0  # offshore discount rate
        self.boem_assessment_year           = 0    # year resource assessment received from BOEM

        # Offshore CashFlow object
        self.cash_flow = cf.CashFlow()


    def setup(self, setup_filename):
        """
        Setup Offshore submodule for HSM.

        Parameters
        ----------
        setup_filename : str
            Path to offshore setup file.

        Returns
        -------
        None
        """
        super().setup(setup_filename)

        #Load variables and tables
        self.logger.info('Load Offshore')
        self.load_input_variables()
        self.load_input_tables()

        #If year 1 or standalone load announced projects, otherwise load intermediate variables
        if (self.rest_curcalyr == self.zero_year) | (self.parent.integrated_switch == False):
            self.logger.info('Load Offshore Announced Projects')
            self.load_announced()
        else:
            self.logger.info('Load Offshore Intermediate Variables')
            self.load_intermediate_variables()


        #One-time setup of announced, producing and undiscovered fields
        if (self.rest_curcalyr == self.zero_year) | (self.parent.integrated_switch == False):  # Only load projects in model year one, after we can just load projects lists from intermediate tables
            self.logger.info('Run Offshore Producing Fields')
            self.run_producing_resources()
            self.run_producing_properties()
            self.run_producing_production()

            self.logger.info('Determine Offshore Undiscovered Fields')
            self.determine_undiscovered_fields()

            self.logger.info('Determine Offshore Drilling Capacity')
            self.calculate_drilling_capacity()

            self.logger.info('Apply Offshore Side Case Adjustments')
            self.side_case_adjustments()

        pass


    def load_input_variables(self):
        """Reads variable data from self.setup_table (loaded in super().setup(setup_filename))

        Returns
        -------
        self.zero_year : int
            First model year

        self.averaging_years : int
            Number of averaging years used to produce crude oil and natural gas prices

        self.exp_success_improve_rate : float
            Technology improvement rate for exploration success rate

        self.exp_delay_improve_rate : float
            Technology improvement rate for exploration delay period

        self.drill_cost_improve_rate : float
            Technology improvement rate for drilling costs

        self.delay_improve_rate : float
            Technology improvement rate for nfw delays

        self.platform_cost_improve_rate : float
            Technology improvement rate for platform costs

        self.operating_cost_improve_rate : float
            Technology improvement rate for operating costs

        self.drill_price_ratio : float
            Price adjustment ratio for drill prices

        self.pipeline_diameter : float
            Assumption for pipeline diameter

        self.wells_per_flowline : int
            Assumption for wells/flowline

        self.evaluation_years : int
            Number of evaluation years (self.final_year - self.zero_year + 1)

        self.boe_to_mcf : float
            BOE to MCF conversion factor

        self.exp_tang_frac : float
            Fraction of well costs that are tangible

        self.dev_tang_frac : float
            Fraction of well costs that are tangible

        self.kap_tang_frac : float
            Fraction of capital costs that are tangible

        self.intang_amor_frac : float
            Fraction of costs to be amortized

        self.fed_tax_rate : float
            Federal tax rate

        self.amor_schedule : str
            Amortization schedule code (see **depreciation_schedules.csv**)

        self.deprec_schedule : str
            Depreciation schedule code (see **depreciation_schedules.csv**)

        self.discount_rate : float
            Discount rate assumption

        self.boem_assessment_year : int
            BOEM field size class assessment year (for determining discovered projects)

        self.platform_cost_year : int
            Basis year for platform costs

        self.drill_cost_base : int
            Drill cost base value used for cost adjustments

        self.oil_price_cost_base : float
            Oil price base value used for cost adjustments

        """
        # variables
        self.zero_year                   = int(  self.setup_table.at[nam.zero_year                      , nam.filename])
        self.averaging_years             = int(  self.setup_table.at[nam.off_averaging_years            , nam.filename])
        self.exp_success_improve_rate    = float(self.setup_table.at[nam.off_exp_success_improve_rate   , nam.filename])
        self.exp_delay_improve_rate      = float(self.setup_table.at[nam.off_exp_delay_improve_rate     , nam.filename])
        self.drill_cost_improve_rate     = float(self.setup_table.at[nam.off_drill_cost_improve_rate    , nam.filename])
        self.delay_improve_rate          = float(self.setup_table.at[nam.off_delay_improve_rate         , nam.filename])
        self.platform_cost_improve_rate  = float(self.setup_table.at[nam.off_platform_cost_improve_rate , nam.filename])
        self.operating_cost_improve_rate = float(self.setup_table.at[nam.off_operating_cost_improve_rate, nam.filename])
        self.base_oil_prc                = float(self.setup_table.at[nam.off_base_oil_prc               , nam.filename])
        self.base_gas_prc                = float(self.setup_table.at[nam.off_base_gas_prc               , nam.filename])
        self.drill_price_ratio           = float(self.setup_table.at[nam.off_drill_price_ratio          , nam.filename])
        self.pipeline_diameter           = float(self.setup_table.at[nam.off_pipeline_diameter          , nam.filename])
        self.wells_per_flowline          = int(  self.setup_table.at[nam.off_wells_per_flowline         , nam.filename])
        self.evaluation_years            = int(  self.setup_table.at[nam.off_evaluation_years           , nam.filename])
        self.boe_to_mcf                  = float(self.setup_table.at[nam.off_boe_to_mcf                 , nam.filename])
        self.exp_tang_frac               = float(self.setup_table.at[nam.exp_tang_frac                  , nam.filename])
        self.dev_tang_frac               = float(self.setup_table.at[nam.dev_tang_frac                  , nam.filename])
        self.kap_tang_frac               = float(self.setup_table.at[nam.kap_tang_frac                  , nam.filename])
        self.intang_amor_frac            = float(self.setup_table.at[nam.intang_amor_frac               , nam.filename])
        self.fed_tax_rate                = float(self.setup_table.at[nam.fed_tax_rate                   , nam.filename])
        self.amor_schedule               = str(  self.setup_table.at[nam.amor_schedule                  , nam.filename])
        self.deprec_schedule             = str(  self.setup_table.at[nam.deprec_schedule                , nam.filename])
        self.discount_rate               = float(self.setup_table.at[nam.discount_rate                  , nam.filename])
        self.boem_assessment_year        = int(  self.setup_table.at[nam.boem_assessment_year           , nam.filename])
        self.platform_cost_year          = int(  self.setup_table.at[nam.off_platform_cost_base         , nam.filename])
        self.drill_cost_base             = int(  self.setup_table.at[nam.off_drill_cost_base            , nam.filename])


        ### Variables that need price adjustment
        # Adjust base cost prices for inflation
        self.oil_price_cost_base         = float(self.setup_table.at[nam.off_oil_price_cost_base        , nam.filename])
        temp_cost_year                   = int(  self.setup_table.at[nam.off_oil_price_cost_base        , nam.price_year])
        self.oil_price_cost_base         *= com.calculate_inflation(self.parent.rest_mc_jpgdp,temp_cost_year)

        # Adjust base nfw prices for inflation
        self.base_oil_prc       *= com.calculate_inflation(self.parent.rest_mc_jpgdp, self.zero_year)
        self.base_gas_prc       *= com.calculate_inflation(self.parent.rest_mc_jpgdp, self.zero_year)

        # Adjust tech levers for side cases
        self.exp_success_improve_rate *= self.parent.side_case_adj
        self.exp_delay_improve_rate *= self.parent.side_case_adj
        self.drill_cost_improve_rate *= self.parent.side_case_adj
        self.delay_improve_rate *= self.parent.side_case_adj
        self.platform_cost_improve_rate *= self.parent.side_case_adj
        self.operating_cost_improve_rate *= self.parent.side_case_adj

        # warnings
        if self.parent.aeo_year - 1 != self.zero_year:
            # com.print_out(self.parent.aeo_year)
            # com.print_out(self.zero_year)
            warnings.warn('self.aeo_year - 2 != self.history_year', UserWarning)

        if self.parent.discount_rate != self.discount_rate:
            # com.print_out(self.parent.discount_rate)
            # com.print_out(self.discount_rate)
            warnings.warn('self.parent.discount_rate != self.discount_rate', UserWarning)

            pass


    def load_input_tables(self):
        """Reads Offshore input tables in as DataFrames.

        Returns
        -------
        self.water_depth : df
            DataFrame of water depth by evaluation unit and field size class

        self.drill_depth : df
            DataFrame of drill depth by evaluation unit and field size class

        self.development_wells : df
            DataFrame of development wells by water depth  and field size class

        self.delays : df
            DataFrame of delays between well discovery and drilling by water depth  and total number of wells

        self.platform_delays : df
            DataFrame of delays between project start and drilling based on platform type by platform type and water depth

        self.platform_slots : df
            DataFrame of platform slots per well by water depth and field size class

        self.platform_structure_type : df
            DataFrame of  well platform structure type by water depth and field size class

        self.platform_drill_per_year : df
            DataFrame of platform annual drilling by platform type and water depth

        self.exp_success_rate : df
            DataFrame of exploratory wells success rates by evaluation unit and field size class

        self.royalty_rates : df
            DataFrame of royalty rates by evaluation unit and field size class

        self.transportation : df
            DataFrame of transportation costs by evaluation unit

        self.mapping : df
            DataFrame of HSM mapping by evaluation unit

        self.prod_fac_cost_frac : df
            DataFrame of capital cost annualized distribution by delay years

        self.prod_profile : df
            DataFrame of well production profile by evaluation unit and field size class

        self.undiscovered_production : df
            DataFrame of undiscovered project production attributes by evaluation unit

        self.producing_fields : df
            DataFrame of producing offshore fields

        self.off_1990_fields : df
            DataFrame of fields discovered before 1990 by evaluation unit

        self.cumulative_nfws : df
            DataFrame of cumulative new field wildcats by evaluation unit

        self.undiscovered_fields : df
            DataFrame of number of undiscovered fields by evaluation unit

        self.field_availability : df
            DataFrame of when fields are available for drilling by evaluation unit

        self.nfw_coefficients : df
            DataFrame of new field wildcat drilling coefficients by evaluation unit

        self.discovery_coefficients : df
            DataFrame of discovery coefficients for discovery algorithim by evaluation unit

        self.dev_cost : df
            DataFrame of development cost equation coefficients by water depth and drill depth

        self.field_size_classes : df
            DataFrame of field size class characteristics

        self.operating_cost : df
            DataFrame of operating cost equation coefficients

        self.gas_ratio_and_cond : df
            DataFrame of gas to oil ratio for mixed wells by evaluation unit

        self.drill_rig_constraint : df
            DataFrame of drill rig constraint eq coefficients by rig type and water depth

        self.exp_cost : df
            DataFrame of exploration cost coefficients by rig type

        self.platform_abandonment : df
            DataFrame of platform abandonment captial cost ratios by rig type

        self.announced_fields : df
            DataFrame of announced projects
        """
        # input tables that need to be stacked (reshape so columns are rows
        self.water_depth                            = super()._load_dataframe(self.offshore_input_path, nam.off_water_depth, index_col=nam.evaluation_unit)
        self.water_depth.columns.name               = nam.field_size_class
        self.water_depth                            = pd.DataFrame({nam.water_depth_ft: self.water_depth.stack()})

        self.drill_depth                            = super()._load_dataframe(self.offshore_input_path, nam.off_drill_depth, index_col=nam.evaluation_unit)
        self.drill_depth.columns.name               = nam.field_size_class
        self.drill_depth                            = pd.DataFrame({nam.drill_depth: self.drill_depth.stack()})

        self.delineation_wells                      = super()._load_dataframe(self.offshore_input_path, nam.off_delineation_wells, index_col=nam.evaluation_unit)
        self.delineation_wells.columns.name         = nam.field_size_class
        self.delineation_wells                      = pd.DataFrame({nam.delineation_wells: self.delineation_wells.stack()})

        self.development_wells                      = super()._load_dataframe(self.offshore_input_path, nam.off_development_wells, index_col=nam.water_depth_m)
        self.development_wells.columns.name         = nam.field_size_class
        self.development_wells                      = pd.DataFrame({nam.development_wells: self.development_wells.stack()})

        self.delays                                 = super()._load_dataframe(self.offshore_input_path, nam.off_delays, index_col=nam.total_wells)
        self.delays.columns.name                    = nam.water_depth_ft
        self.delays                                 = pd.DataFrame({nam.delay_years: self.delays.stack()})

        self.platform_delays                        = super()._load_dataframe(self.offshore_input_path, nam.off_platform_delays, index_col=nam.platform_type)
        self.platform_delays.columns.name           = nam.water_depth_ft
        self.platform_delays                        = pd.DataFrame({nam.delay_years: self.platform_delays.stack()})

        self.platform_slots                         = super()._load_dataframe(self.offshore_input_path, nam.off_platform_slots, index_col=nam.water_depth_m)
        self.platform_slots.columns.name            = nam.field_size_class
        self.platform_slots                         = pd.DataFrame({nam.platform_slots: self.platform_slots.stack()})

        self.platform_structure_type                = super()._load_dataframe(self.offshore_input_path, nam.off_platform_structure_type, index_col=nam.water_depth_m)
        self.platform_structure_type.columns.name   = nam.field_size_class
        self.platform_structure_type                = pd.DataFrame({nam.platform_type: self.platform_structure_type.stack()})

        self.platform_drill_per_year                = super()._load_dataframe(self.offshore_input_path, nam.off_platform_drill_per_year, index_col=nam.platform_type)
        self.platform_drill_per_year.columns.name   = nam.water_depth_ft
        self.platform_drill_per_year                = pd.DataFrame({nam.drill_per_year: self.platform_drill_per_year.stack()})

        self.exp_success_rate                       = super()._load_dataframe(self.offshore_input_path, nam.off_exp_success_rate, index_col=nam.evaluation_unit)
        self.exp_success_rate.columns.name          = nam.field_size_class
        self.exp_success_rate                       = pd.DataFrame({nam.exp_success_rate: self.exp_success_rate.stack()})

        self.royalty_rates                          = super()._load_dataframe(self.offshore_input_path, nam.off_royalty, index_col=nam.evaluation_unit)
        self.royalty_rates.columns.name             = nam.field_size_class
        self.royalty_rates                          = pd.DataFrame({nam.royalty_rate: self.royalty_rates.stack()})

        #note, each transportation cost can have a different cost year
        self.transportation                         = super()._load_dataframe(self.offshore_input_path, nam.off_transportation, index_col=nam.evaluation_unit)
        temp_cost_year                              = self.transportation[nam.price_year].apply(lambda x: com.calculate_inflation(self.parent.rest_mc_jpgdp, x))
        self.transportation[nam.crude_trans_price]  *= temp_cost_year
        self.transportation[nam.natgas_trans_price] *= temp_cost_year

        # input tables that don't need to be stacked
        self.mapping                                = super()._load_dataframe(self.offshore_input_path, nam.off_mapping,                  index_col=nam.evaluation_unit)
        self.prod_fac_cost_frac                     = super()._load_dataframe(self.offshore_input_path, nam.off_prod_fac_cost_frac,       index_col=nam.delay_years)
        self.prod_profile                           = super()._load_dataframe(self.offshore_input_path, nam.off_prod_profile,             index_col=[nam.evaluation_unit, nam.field_size_class])
        self.undiscovered_production                = super()._load_dataframe(self.offshore_input_path, nam.off_undiscovered_production,  index_col=nam.evaluation_unit)
        self.producing_fields                       = super()._load_dataframe(self.offshore_input_path, nam.off_producing_fields,         index_col=None)
        self.off_1990_fields                        = super()._load_dataframe(self.offshore_input_path, nam.off_1990_fields,              index_col=nam.evaluation_unit)
        self.cumulative_nfws                        = super()._load_dataframe(self.offshore_input_path, nam.off_cumulative_nfws,          index_col=nam.evaluation_unit)
        self.undiscovered_fields                    = super()._load_dataframe(self.offshore_input_path, nam.off_undiscovered_fields,      index_col=nam.evaluation_unit)
        self.field_availability                     = super()._load_dataframe(self.offshore_input_path, nam.off_field_availability,       index_col=nam.evaluation_unit)
        self.nfw_coefficients                       = super()._load_dataframe(self.offshore_input_path, nam.off_nfw_coefficients,         index_col=nam.evaluation_unit)
        self.discovery_coefficients                 = super()._load_dataframe(self.offshore_input_path, nam.off_discovery_coefficients,   index_col=nam.evaluation_unit)
        self.dev_cost                               = super()._load_dataframe(self.offshore_input_path, nam.off_dev_cost,                 index_col=[nam.water_depth_m, nam.drill_depth])
        self.field_size_classes                     = super()._load_dataframe(self.offshore_input_path, nam.off_field_size_classes,       index_col=nam.field_size_class)
        self.operating_cost                         = super()._load_dataframe(self.offshore_input_path, nam.off_operating_cost,           index_col=None)
        self.gas_ratio_and_cond                     = super()._load_dataframe(self.offshore_input_path, nam.off_gas_ratio_and_cond,       index_col=nam.evaluation_unit)
        self.drill_rig_constraint                   = super()._load_dataframe(self.offshore_input_path, nam.drill_rig_constraint,         index_col=[nam.drill_rig, nam.water_depth_min])
        self.exp_cost                               = super()._load_dataframe(self.offshore_input_path, nam.off_exp_cost,                 index_col=nam.water_depth_m)
        self.platform_abandonment                   = super()._load_dataframe(self.offshore_input_path, nam.off_platform_abandonment,     index_col=nam.platform_type)
        self.drill_per_year                         = super()._load_dataframe(self.offshore_input_path, nam.off_drill_per_year,           index_col=nam.water_depth_ft)

        #Year 1 tables
        if (self.rest_curcalyr == self.zero_year) | (self.parent.integrated_switch == False):
            self.announced_fields = super()._load_dataframe(self.offshore_input_path, nam.off_announced_fields,index_col=None)


        # Load crude price
        self.logger.info('Load Offshore Fixed Crude Price')
        temp_natgas_file = Path(self.parent.hsm_var_output_path + 'off_reg_crude_price.pkl')
        if temp_natgas_file.is_file():
            self.off_reg_crude_price = pd.read_pickle(self.parent.hsm_var_output_path + 'off_reg_crude_price' + '.pkl')
        else:
            pass


        # Load natgas price
        self.logger.info('Load Offshore Fixed Natgas Price')
        temp_natgas_file = Path(self.parent.hsm_var_output_path + 'off_reg_natgas_price.pkl')
        if temp_natgas_file.is_file():
            self.off_reg_natgas_price = pd.read_pickle(self.parent.hsm_var_output_path + 'off_reg_natgas_price' + '.pkl')
        else:
            pass


        # warnings
        test = (self.gas_ratio_and_cond[nam.condensate_yield] * self.boe_to_mcf > 1.0).any()
        if test:
            warnings.warn('condensate_yield greater than 1', UserWarning)
        test = (self.gas_ratio_and_cond[nam.gas_oil_ratio] / self.boe_to_mcf > 1.0).any()
        if test:
            warnings.warn('gas oil ratio greater than 1', UserWarning)


    def load_announced(self):
        """Performs one-time setup for announced fields.

            * Merge to HSM mapping, drilling depths etc.
            * Convert water depth from feet to metres
            * Declare project type as "announced"

        Returns
        -------
        self.announced_fields : df
            DataFrame of announced projects
        """
        self.announced_fields.drop([nam.region_number, nam.region_name, nam.district_number], axis=1, errors='ignore')
        self.announced_fields = self.announced_fields.reset_index().merge(self.mapping[[nam.region_number, nam.region_name, nam.district_number]],
                                         left_on=nam.evaluation_unit,
                                         right_index=True,
                                         how='left').set_index('index')

        self.announced_fields = self.announced_fields.drop(nam.drill_depth, axis=1, errors='ignore')
        self.announced_fields = self.announced_fields.reset_index().merge(self.drill_depth[[nam.drill_depth]],
                                         left_on=[nam.evaluation_unit, nam.field_size_class],
                                         right_index=True,
                                         how='left').set_index('index')

        self.announced_fields[nam.water_depth_m] = (self.announced_fields[nam.water_depth_ft] / 3.28084).astype('int64')

        self.announced_fields[nam.discovery_type] = nam.announced

        pass


    def load_intermediate_variables(self):
        """Load intermediate variables for integrated runs from .pkl files.

        Returns
        -------
        self.announced_fields : df
            DataFrame of announced projects

        self.discovered_fields : df
            DataFrame of discovered fields

        self.discovered_field_dist : df
            DataFrane if discovered fields distribution across evaluation units

        self.undiscovered_fields : df
            DataFrame of number of undiscovered fields by evaluation unit

        self.cumulative_nfws : df
            DataFrame of cumulative new field wildcats by evaluation unit

        self.total_fields : df
            DataFrame of total fields by evaluation unit and field size class

        self.fields : df
            Main DataFrame of fields with field-level attributes

        self.fields_selected : df
            DataFrame of fields that have been selected to produce

        self.crude_production : df
            DataFrame of crude production by field

        self.natgas_production : df
            DataFrame of total natural gas production by field

        self.nagas_production : df
            DataFrame of non-associated natural gas production by field

        self.adgas_production : df
            DataFrame of associated dissolved natural gas production by field

        self.wells : df
            DataFrame of wells by field

        self.exploratory_wells : df
            DataFrame of exploratory wells only by field

        self.drill_rig_constraint : df
            DataFrame of rig constaints by rig type

        self.field_crude_production : df
            DataFrame of crude production by developing field

        self.field_natgas_production : df
            DataFrame of natural gas production by developing field
        """
        ###Load Intermediate Tables:
        self.logger.info('Loading Intermediate Offshore Variables')
        self.announced_fields           = self.parent.hsm_vars.off_announced_fields.copy()
        self.discovered_fields          = self.parent.hsm_vars.off_discovered_fields.copy()
        self.discovered_field_dist      = self.parent.hsm_vars.off_discovered_field_dist.copy()
        self.undiscovered_fields        = self.parent.hsm_vars.off_undiscovered_fields.copy()
        self.cumulative_nfws            = self.parent.hsm_vars.off_cumulative_nfws.copy()
        self.total_fields               = self.parent.hsm_vars.off_total_fields.copy()
        self.fields                     = self.parent.hsm_vars.off_fields.copy()
        self.fields_selected            = self.parent.hsm_vars.off_fields_selected.copy()
        self.crude_production           = self.parent.hsm_vars.off_crude_production.copy()
        self.natgas_production          = self.parent.hsm_vars.off_natgas_production.copy()
        self.nagas_production           = self.parent.hsm_vars.off_nagas_production.copy()
        self.adgas_production           = self.parent.hsm_vars.off_adgas_production.copy()
        self.wells                      = self.parent.hsm_vars.off_wells.copy()
        self.exploratory_wells          = self.parent.hsm_vars.off_exploratory_wells.copy()
        self.drill_rig_constraint       = self.parent.hsm_vars.off_drill_rig_constraint.copy()
        self.field_crude_production     = self.parent.hsm_vars.off_field_crude_production.copy()
        self.field_natgas_production    = self.parent.hsm_vars.off_field_natgas_production.copy()

        pass


    def run_producing_resources(self):
        """Performs one-time calculation for producing field resources.

            * Gets producing project max production and initial production based on history or field characteristics
            * Merge field attributes (i.e. evaluation unit, field size class resources)

        Returns
        -------
        self.producing_fields : df
            DataFrame of producing offshore fields
        """
        def max_prod(series):
            """
            Function to determine producing projects' max production

            * If decline_flag == 0 then set max prod as max historical production, since flag sets well as new again
            * If decline_flag != 0 set max prod to last positive value in a Series

            Series needs to run from 1990->self.zero_year - 1

            Examples
            --------
            df.apply(fun, axis=1)

            Parameters
            ----------
            series : pd.Series
                Row in pd.Dataframe

            Returns
            -------
            int
            """
            # function returns last non-zero value in series
            if series[nam.decline_flag] == 0:
                return series[list(range(self.parent.param_baseyr, (self.zero_year + 1)))].max()
            else:
                for i in range(self.zero_year, self.parent.param_baseyr - 1, -1):
                    if series[i] > 0:
                        return series[i]
                # if no positive values, return zero
                return 0

        def init_prod(series):
            """
            Function to determine producing projects' initial production

            -Set initial prod to last positive value in a Series

            Series needs to run from 1990->self.zero_year - 1

            Examples
            --------
            df.apply(fun, axis=1)

            Parameters
            ----------
            series : pd.Series
                Row in pd.Dataframe

            Returns
            -------
            int
            """
            # function returns last non-zero value in series
            for i in range(self.zero_year, self.parent.param_baseyr - 1, -1):
                if series[i] > 0:
                    return series[i]
            # if no positive values, return zero
            return 0

        #Convert input table values from MBBLs to BBls
        history_range = list(range(self.parent.param_baseyr, (self.parent.history_year + 1)))
        self.producing_fields[history_range] = self.producing_fields[history_range].mul(1000)

        #Determine max production rates
        self.producing_fields.loc[self.producing_fields[nam.fuel] == nam.oil, nam.max_oil_production_rate] = self.producing_fields.apply(max_prod, axis=1)
        self.producing_fields.loc[self.producing_fields[nam.fuel] == nam.gas, nam.max_gas_production_rate] = self.producing_fields.apply(max_prod, axis=1) / self.boe_to_mcf
        self.producing_fields[nam.max_oil_production_rate] = self.producing_fields[nam.max_oil_production_rate].fillna(0).astype(int)
        self.producing_fields[nam.max_gas_production_rate] = self.producing_fields[nam.max_gas_production_rate].fillna(0).astype(int)

        #Determine initial production rates
        self.producing_fields.loc[self.producing_fields[nam.fuel] == nam.oil, nam.initial_oil_prod_rate] = self.producing_fields.apply(init_prod, axis=1)
        self.producing_fields.loc[self.producing_fields[nam.fuel] == nam.gas, nam.initial_gas_prod_rate] = self.producing_fields.apply(init_prod, axis=1) / self.boe_to_mcf
        self.producing_fields[nam.initial_oil_prod_rate] = self.producing_fields[nam.initial_oil_prod_rate].fillna(0).astype(int)
        self.producing_fields[nam.initial_gas_prod_rate] = self.producing_fields[nam.initial_gas_prod_rate].fillna(0).astype(int)


        #Some fields have no past production, set max production as the initial production rate from undiscovered fields
        mask = (self.producing_fields[nam.max_oil_production_rate] == 0)
        self.producing_fields.loc[mask, nam.max_oil_production_rate] = self.producing_fields.merge(self.undiscovered_production,
                                                                                left_on=nam.evaluation_unit,
                                                                                right_index=True,
                                                                                how='left',
                                                                                suffixes=['','_und'])[nam.initial_oil_prod_rate_und]
        mask = (self.producing_fields[nam.max_gas_production_rate] == 0)
        self.producing_fields.loc[mask, nam.max_gas_production_rate] = self.producing_fields.merge(self.undiscovered_production,
                                                                                left_on=nam.evaluation_unit,
                                                                                right_index=True,
                                                                                how='left',
                                                                                suffixes=['','_und'])[nam.initial_gas_prod_rate_und]

        #Calculate past production
        self.producing_fields[nam.past_production] = self.producing_fields[list(range(1990, (self.zero_year - 1)))].sum(axis=1)

        #Convert from mcf to boe for gas production
        mask = self.producing_fields[nam.fuel] == nam.gas
        self.producing_fields.loc[mask, nam.past_production] = self.producing_fields.loc[mask, nam.past_production] / self.boe_to_mcf
        self.producing_fields[nam.past_production] = self.producing_fields[nam.past_production].astype('int64')

        #The field size class determined in the preprocessor is not always consistant with the historical production of the fields
        #Check that the past production doesn't exceed the average production
        self.producing_fields = pd.merge_asof(self.producing_fields.sort_values(nam.past_production),
                                              self.field_size_classes[[nam.mean_resources_boe]].reset_index().sort_values(nam.mean_resources_boe),
                                              left_on=nam.past_production,
                                              right_on=nam.mean_resources_boe,
                                              direction='forward',
                                              suffixes=['_in', '_calc'])
        self.producing_fields[nam.field_size_class + '_calc'] = self.producing_fields[nam.field_size_class + '_calc'].fillna(0)
        self.producing_fields[nam.field_size_class] = self.producing_fields[[nam.field_size_class + '_in', nam.field_size_class + '_calc']].max(axis=1)
        self.producing_fields[nam.field_size_class] = self.producing_fields[nam.field_size_class].astype('int64')

        #Now that the field size classes have been updated, determine the resources for each field
        self.producing_fields = self.producing_fields.drop(nam.mean_resources_boe, axis=1)
        temp_resources = self.producing_fields[[nam.field_size_class]].reset_index().merge(self.field_size_classes,
                                  left_on=nam.field_size_class,
                                  right_index=True,
                                  how='left').set_index('index')[nam.max_resources_boe]
        self.producing_fields.loc[self.producing_fields[nam.fuel] == nam.oil , nam.oil_resources] = temp_resources
        self.producing_fields.loc[self.producing_fields[nam.fuel] == nam.gas , nam.gas_resources] = temp_resources * self.boe_to_mcf
        self.producing_fields[nam.oil_resources] = self.producing_fields[nam.oil_resources].fillna(0)
        self.producing_fields[nam.gas_resources] = self.producing_fields[nam.gas_resources].fillna(0)

        pass


    def run_producing_properties(self):
        """Performs one-time calculation for producing field properties.

            * Merge producing projects to mapping, production properties, decline rates, etc.

        Returns
        -------
        self.producing_fields : df
            DataFrame of producing offshore fields
        """
        #Get district numbers
        self.producing_fields.drop([nam.district_number], axis=1, errors='ignore')
        self.producing_fields = self.producing_fields.reset_index().merge(self.mapping[[nam.district_number]],
                                         left_on=nam.evaluation_unit,
                                         right_index=True,
                                         how='left').set_index('index')

        #Get region numbers
        self.producing_fields = self.producing_fields.reset_index().merge(
                                         self.mapping[[nam.region_number]],
                                         left_on=nam.evaluation_unit,
                                         right_index=True,
                                         how='left').set_index('index')

        # Get District numbers for state-level production
        self.producing_fields.loc[self.producing_fields['NAME'] == 'STATEAL', nam.district_number] = 70
        self.producing_fields.loc[self.producing_fields['NAME'] == 'STATELA', nam.district_number] = 71
        self.producing_fields.loc[self.producing_fields['NAME'] == 'STATETX', nam.district_number] = 72
        self.producing_fields.loc[self.producing_fields['NAME'] == 'STATECA', nam.district_number] = 73


        #Determine oil properties (prod_years, peak_years, decline_rate)
        mask = self.prod_profile[nam.oil_or_gas] == nam.oil
        mask2 = self.producing_fields[nam.fuel] == nam.oil
        temp = com.df_interpolate_2(self.producing_fields.loc[mask2, [nam.evaluation_unit, nam.field_size_class]],
                                    self.prod_profile[mask],
                                    xcol=nam.evaluation_unit,
                                    ycol=nam.field_size_class,
                                    xmerge='merge')

        self.producing_fields[nam.prod_years] = temp[nam.ramp_up_years] + temp[nam.peak_years]
        self.producing_fields[nam.hyper_decline_coef] = temp[nam.hyper_decline_coef]
        self.producing_fields[nam.oil_decline_rate] = temp[nam.decline_rate]

        #Determine gas properties (prod_years, peak_years, decline_rate)
        mask = self.prod_profile[nam.oil_or_gas] == nam.gas
        mask2 = self.producing_fields[nam.fuel] == nam.gas
        temp = com.df_interpolate_2(self.producing_fields.loc[mask2, [nam.evaluation_unit, nam.field_size_class]],
                                    self.prod_profile[mask],
                                    xcol=nam.evaluation_unit,
                                    ycol=nam.field_size_class,
                                    xmerge='merge')
        self.producing_fields[nam.prod_years].update(temp[nam.ramp_up_years] + temp[nam.peak_years])
        self.producing_fields[nam.hyper_decline_coef].update(temp[nam.hyper_decline_coef])
        self.producing_fields[nam.gas_decline_rate] = temp[nam.decline_rate]

        self.producing_fields[nam.oil_decline_rate] = self.producing_fields[nam.oil_decline_rate].fillna(0.0)
        self.producing_fields[nam.gas_decline_rate] = self.producing_fields[nam.gas_decline_rate].fillna(0.0)

        #Set ramp up to zero
        self.producing_fields[nam.ramp_up_years] = 0

        #OGSM has a different production routine for producing fields (than announced/undiscovered fields)
        #To match the behavior of OGSM without having multiple production routines, back-calculate pre-decline production
        #(past_production + peak_production*peak_years)/resources
        self.producing_fields.loc[self.producing_fields[nam.fuel] == nam.oil, nam.predecline_prod_fraction_oil] = \
            ((self.producing_fields[nam.past_production] +
              self.producing_fields[nam.prod_years] * self.producing_fields[nam.max_oil_production_rate]) /
             self.producing_fields[nam.oil_resources])
        self.producing_fields.loc[self.producing_fields[nam.fuel] == nam.gas, nam.predecline_prod_fraction_gas] = \
            ((self.producing_fields[nam.past_production] +
              self.producing_fields[nam.prod_years] * self.producing_fields[nam.max_gas_production_rate]) /
             self.producing_fields[nam.gas_resources])
        self.producing_fields[nam.predecline_prod_fraction_oil] = self.producing_fields[nam.predecline_prod_fraction_oil].fillna(0.0)
        self.producing_fields[nam.predecline_prod_fraction_gas] = self.producing_fields[nam.predecline_prod_fraction_gas].fillna(0.0)

        #Calibrated to OGSM, adjust pre-decline to min of .0.05
        self.producing_fields.loc[self.producing_fields[nam.predecline_prod_fraction_gas] > 0.3, nam.predecline_prod_fraction_gas] = 0.3
        self.producing_fields.loc[self.producing_fields[nam.predecline_prod_fraction_oil] > 0.3, nam.predecline_prod_fraction_oil] = 0.3

        #Calibrated to OGSM, adjust pre-decline to in of 0.1
        self.producing_fields.loc[self.producing_fields[nam.predecline_prod_fraction_gas] < 0.1, nam.predecline_prod_fraction_gas] = 0.1
        self.producing_fields.loc[self.producing_fields[nam.predecline_prod_fraction_oil] < 0.1, nam.predecline_prod_fraction_oil] = 0.1

        pass


    def run_producing_production(self):
        """Performs one-time production calculations for producing fields.

            * Apply drilling equations to producing projects to project future production
            * Split production between oil and natural gas and write to self.crude_production, self.natgas_production
              self.adgas_production, and self.nagas_production

        Returns
        -------
        self.producing_fields : df
            DataFrame of producing offshore fields
        """
        ###Baseline Oil Production Profile
        self.producing_fields = self.producing_fields.reset_index().merge(
                                         self.gas_ratio_and_cond[[nam.gas_proportion, nam.gas_oil_ratio, nam.condensate_yield]],
                                         left_on=nam.evaluation_unit,
                                         right_index=True,
                                         how='left').set_index('index')

        #Start with baseline crude and gas production
        years = self.parent.final_aeo_year - self.zero_year + 1
        mask = self.producing_fields[nam.fuel] == nam.oil
        baseline_crude = self.producing_fields[mask].apply(drill_eq.off_production_profile,
                                                           evaluation_years=years,
                                                           hist_year=self.zero_year,
                                                           oil_or_gas=nam.oil,
                                                           past_prod_flag=True,
                                                           decline_flag=True,
                                                           axis=1)

        #Append zero year to dataframe
        if baseline_crude.empty:
            pass
        else:
            baseline_crude.columns = baseline_crude.columns + self.zero_year


        ###Baseline Natural Gas Production Profile
        mask = self.producing_fields[nam.fuel] == nam.gas
        baseline_natgas = self.producing_fields[mask].apply(drill_eq.off_production_profile,
                                                            evaluation_years=years,
                                                            hist_year=self.zero_year,
                                                            oil_or_gas=nam.gas,
                                                            past_prod_flag=True,
                                                            decline_flag=True,
                                                            axis=1)

        #Append zero year to dataframe
        if baseline_natgas.empty:
            pass
        else:
            baseline_natgas.columns = baseline_natgas.columns + self.zero_year


        ###Determine associated, non-associated, condensate and non-condensate production
        #Get type production volumes from baselines
        associated_gas_production     = baseline_crude.mul( self.producing_fields[nam.gas_oil_ratio], axis=0)
        non_condensate_production     = baseline_crude - associated_gas_production / self.boe_to_mcf
        condensate_production         = baseline_natgas.mul(self.producing_fields[nam.condensate_yield], axis=0)
        non_associated_gas_production = baseline_natgas - condensate_production * self.boe_to_mcf

        associated_gas_production     = associated_gas_production.fillna(0.0)
        non_condensate_production     = non_condensate_production.fillna(0.0)
        condensate_production         = condensate_production.fillna(0.0)
        non_associated_gas_production = non_associated_gas_production.fillna(0.0)

        #Combine condensate and associated production to calculate crude and natural gas production, respectively
        self.crude_production = (non_condensate_production + condensate_production)
        self.crude_production[nam.discovery_type] = nam.producing
        self.crude_production[nam.evaluation_unit]  = self.producing_fields[nam.evaluation_unit].copy()
        self.crude_production[nam.federal_land]  = self.producing_fields[nam.federal_land].copy()
        self.crude_production[nam.district_number] = self.producing_fields[nam.district_number].copy()
        self.crude_production[nam.region_number] = self.producing_fields[nam.region_number].copy()
        self.crude_production[nam.water_depth_ft] = self.producing_fields[nam.water_depth_ft].copy()

        self.natgas_production = (associated_gas_production + non_associated_gas_production)
        self.natgas_production[nam.discovery_type] = nam.producing
        self.natgas_production[nam.evaluation_unit] = self.producing_fields[nam.evaluation_unit].copy()
        self.natgas_production[nam.federal_land]  = self.producing_fields[nam.federal_land].copy()
        self.natgas_production[nam.district_number]  = self.producing_fields[nam.district_number].copy()
        self.natgas_production[nam.region_number]  = self.producing_fields[nam.region_number].copy()
        self.natgas_production[nam.water_depth_ft]  = self.producing_fields[nam.water_depth_ft].copy()


        self.nagas_production = non_associated_gas_production
        self.nagas_production[nam.discovery_type] = nam.producing
        self.nagas_production[nam.evaluation_unit] = self.producing_fields[nam.evaluation_unit].copy()
        self.nagas_production[nam.federal_land]  = self.producing_fields[nam.federal_land].copy()
        self.nagas_production[nam.district_number]  = self.producing_fields[nam.district_number].copy()
        self.nagas_production[nam.region_number]  = self.producing_fields[nam.region_number].copy()
        self.nagas_production[nam.water_depth_ft]  = self.producing_fields[nam.water_depth_ft].copy()


        self.adgas_production = associated_gas_production
        self.adgas_production[nam.discovery_type] = nam.producing
        self.adgas_production[nam.evaluation_unit] = self.producing_fields[nam.evaluation_unit].copy()
        self.adgas_production[nam.federal_land]  = self.producing_fields[nam.federal_land].copy()
        self.adgas_production[nam.district_number]  = self.producing_fields[nam.district_number].copy()
        self.adgas_production[nam.region_number]  = self.producing_fields[nam.region_number].copy()
        self.adgas_production[nam.water_depth_ft] = self.producing_fields[nam.water_depth_ft]

        pass


    def determine_undiscovered_fields(self):
        """Calculates distribution of undiscovered fields.

            * Accounts for discoveries before 1990, and after the BOEM assessment year by subtracting them from BOEM assessment
            * Appends all producing and announced wells to the relevant field dfs

        Returns
        -------
        self.undiscovered_fields : df
            DataFrame of number of undiscovered fields by evaluation unit

        self.total_fields : df
            DataFrame of total fields by evaluation unit and field size class

        self.discovered_field_dist : df
            DataFrame of discovered fields distribution across evaluation units
        """
        ###Subtract fields discovered after BOEM assessment or before 1990
        #Producing fields
        mask = self.producing_fields[nam.discovery_year] >= self.boem_assessment_year
        mask = mask & (self.producing_fields[nam.discovery_year] < self.zero_year)
        mask = mask & (self.producing_fields[nam.fuel] == nam.oil)
        temp = self.producing_fields[mask].groupby([nam.evaluation_unit, nam.field_size_class]).size()
        temp = temp.unstack(level=-1, fill_value=0)
        self.undiscovered_fields.update(self.undiscovered_fields - temp)

        #Subtract announced fields
        mask = self.announced_fields[nam.discovery_year] >= self.boem_assessment_year
        mask = mask & (self.announced_fields[nam.discovery_year] < self.zero_year)
        temp = self.announced_fields[mask].groupby([nam.evaluation_unit, nam.field_size_class]).size()
        temp = temp.unstack(level=-1, fill_value=0)
        self.undiscovered_fields.update(self.undiscovered_fields - temp)

        #Subtract volumes from evaluations units with buffer fraction
        self.undiscovered_fields = self.undiscovered_fields.mul(1.0 - self.field_availability[nam.buffer_fraction], axis=0)
        #Match OGSM for rounding routine
        self.undiscovered_fields = self.undiscovered_fields + 0.5
        self.undiscovered_fields = self.undiscovered_fields.astype(int)


        ###Get total fields; fields before 1990, after 1990 , and undiscovered
        self.total_fields = self.off_1990_fields + self.undiscovered_fields


        ###Get discovered fields; fields before 1990, after 1990
        self.discovered_field_dist = self.off_1990_fields.copy()


        ###Determine number of discovered fields after 1990
        #Producing fields
        mask = self.producing_fields[nam.discovery_year] >= 1990
        mask = mask & (self.producing_fields[nam.fuel] == nam.oil)
        temp = pd.DataFrame(0, index=self.mapping.index, columns=list(range(2, 18))).stack()
        temp.update(self.producing_fields[mask].groupby([nam.evaluation_unit, nam.field_size_class]).size())
        temp = temp.unstack(level=-1, fill_value=0)
        self.total_fields.update(self.total_fields + temp)
        self.discovered_field_dist.update(self.discovered_field_dist + temp)


        #Announced fields
        mask = self.announced_fields[nam.discovery_year] >= 1990
        temp = pd.DataFrame(0, index=self.mapping.index, columns=list(range(2, 18))).stack()
        temp.update(self.announced_fields[mask].groupby([nam.evaluation_unit, nam.field_size_class]).size())
        temp = temp.unstack(level=-1, fill_value=0)
        self.total_fields.update(self.total_fields + temp)
        self.discovered_field_dist.update(self.discovered_field_dist + temp)

        self.total_fields = self.total_fields.astype(int)
        self.discovered_field_dist = self.discovered_field_dist.astype(int)

        pass


    def calculate_drilling_capacity(self):
        """Determine model drilling capacity by rig type.

        Returns
        -------
        self.drill_rig_constraint : df
            DataFrame of rig constaints by rig type
        """
        #Create eq variables
        num_rigs            = self.drill_rig_constraint[nam.num_rigs            ]
        target_rig_util     = self.drill_rig_constraint[nam.target_rig_util     ]
        max_rig_build_rate  = self.drill_rig_constraint[nam.max_rig_build_rate  ]
        drill_rate          = self.drill_rig_constraint[nam.drill_rate          ]
        efficiency_scalar   = self.drill_rig_constraint[nam.efficiency_scalar   ]

        #Calculate well drilling capacity (Wells/Rig) based on target well utilization + max build rate
        self.drill_rig_constraint[nam.rig_well_cap_boy] = round(num_rigs * ((target_rig_util + max_rig_build_rate) / 100) * 365 / drill_rate)

        #Number of wells drilled (Wells/Rig)
        self.drill_rig_constraint[nam.rig_well_util] = round(num_rigs * efficiency_scalar)

        #Set rig utilization percent to 0
        self.drill_rig_constraint[nam.rig_well_util_per] = 0

        pass


    def side_case_adjustments(self):
        """Adjust undiscovered project EURs for Oil and Gas Supply Cases.

        Returns
        -------
        self.undiscovered_production : df
            DataFrame of undiscovered project production attributes by evaluation unit
        """
        self.undiscovered_production[nam.initial_oil_prod_rate] = self.undiscovered_production[nam.initial_oil_prod_rate] * self.parent.side_case_adj
        self.undiscovered_production[nam.initial_gas_prod_rate] = self.undiscovered_production[nam.initial_gas_prod_rate] * self.parent.side_case_adj

        pass


    def run(self):
        """Run Offshore Submodule for HSM.

        Returns
        -------
        None
        """
        com.print_out('running offshore submodule')
        super().run()

        #Fix crude oil price to iteration 1 prices to stop oscillations with NEMS
        temp_crude_file = Path(self.parent.hsm_var_output_path + 'off_reg_crude_price.pkl')
        if temp_crude_file.is_file():
            self.logger.info('Offshore Regional Crude Price Pickle File Exists')
            pass
        else:
            self.logger.info('Offshore Regional Crude Price Pickle File Does Not Exist, Reading in Regional Crude Prices')
            self.fix_prices()

        #Fix natural gas price to iteration 1 prices to stop oscillations with NEMS
        temp_natgas_file = Path(self.parent.hsm_var_output_path + 'off_reg_natgas_price.pkl')
        if temp_natgas_file.is_file():
            self.logger.info('Offshore Regional Natural Gas Price Pickle File Exists')
            pass
        else:
            self.logger.info('Offshore Regional Natural Gas Price Pickle File Does Not Exist, Reading in Regional Natural Gas Prices')
            self.fix_prices()

        #Only run if in or past zero year
        if self.rest_curcalyr >= self.zero_year:

            #Only run the discovery algorithm after the zero year
            if self.rest_curcalyr > self.zero_year:
                self.logger.info('Run Offshore Discovery Algorithm')
                self.calculate_resources()

                #Calculate possible exploration projects
                self.logger.info('Calculate Offshore New Fields')
                self.determine_nfws()
                self.calculate_new_fields()
                self.calculate_new_field_properties()

            #Load fields
            self.load_fields()

            #If there are no fields, don't run cash flow.
            if len(self.fields):

                #Load Crude Oil/Natural Gas Prices
                self.logger.info('Load Offshore Crude/Natural Gas Prices')
                self.load_prices()

                #Calculate Offshore Drilling
                self.logger.info('Calculate Offshore Drilling')
                self.calculate_drilling()

                #Calculate Exploration Costs
                self.logger.info('Calculate Offshore Exploration Costs')
                self.calculate_exploration_cost()
                self.calculate_delay_years()
                self.calculate_platform_cost()

                #Calculate Offshore Well Costs
                self.logger.info('Calculate Offshore Well Costs')
                self.calculate_development_cost()
                self.calculate_operating_cost()

                #Load and Run DCF
                self.logger.info('Load Offshore Cash Flow Properties')
                self.load_cash_flow_properties()

                self.logger.info('Load Offshore Cash Flow Production')
                self.load_cash_flow_production()

                self.logger.info('Run Offshore DCF')
                self.run_cash_flow()

                #Select Offshore Projects
                self.logger.info('Select Offshore Projects')
                self.rank_fields()
                self.select_fields()

                #Calculate Offshore NGPLs
                self.logger.info('Calculate Offshore NGPLs')
                self.calculate_ngpls()

            #Only develop drilling capacity after the zero year
            if self.rest_curcalyr > self.zero_year:
                self.logger.info('Develop Offshore Drilling Capacity')
                self.expand_drilling_capacity()

            # Debug Offshore Submodule
            if self.parent.debug_switch == True:
                self.logger.info('Debug Offshore Submodule')
                self.debug_offshore()

            #Write intermediate variables if FCRL = 1 or NCRL = 1
            if ((self.parent.integrated_switch == True) & (self.parent.param_fcrl == 1)) | ((self.parent.integrated_switch == True) & (self.parent.param_ncrl == 1)):
                self.logger.info('Offshore Write Intermediate Variables')
                self.write_intermediate_variables()

        pass


    def fix_prices(self):
        """Fix natural gas prices for the offshore submodule in the first model iteration to stop oscillations with NGMM

        Returns
        -------
        self.off_reg_natgas_price : df
            Fixed iteration 1 natural gas prices

        self.off_reg_crude_price : df
            Fixed iteration 1 crude prices

        """
        self.off_reg_crude_price = self.parent.reg_crude_price.copy()
        self.off_reg_natgas_price = self.parent.reg_natgas_price.copy()

        pass


    def calculate_resources(self):
        """Calculate technically recoverable resources based on available undiscovered fields.

            * Sum undiscovered project resource volumes by field area

        Returns
        -------
        self.technically_recoverable : df
            DataFrame of technically recoverable resources by field
        """
        #Determine volumes for each field size class
        temp_volumes = self.field_size_classes[nam.mean_resources_boe].T.copy()

        # Side case adjustment
        temp_volumes  *= self.parent.side_case_adj

        #Zero out fields that are not yet available
        mask = self.rest_curcalyr < self.field_availability[nam.year_available]
        available_fields = self.undiscovered_fields.copy()
        available_fields.loc[mask] = 0

        #Calculate resources for current year
        available_fields = available_fields.mul(temp_volumes, axis=1)
        available_fields = available_fields.merge(self.mapping[nam.planning_area],
                                    left_index=True,
                                    right_index=True,
                                    how='left')
        available_fields = available_fields.groupby(nam.planning_area).sum()
        available_fields = available_fields.sum(axis=1)

        #Technically Recoverable resources are equal to available fields resources
        self.technically_recoverable[int(self.rest_curcalyr)] = available_fields

        pass


    def determine_nfws(self):
        """Calculate number of new fields wildcats drilling in each evaluation unit.

        The new field wildcat drilling volume is based on the lagged prices of crude oil and natural gas.

        Returns
        -------
        self.cumulative_nfws : df
            DataFrame of cumulative new field wildcats by evaluation unit
        """
        #Determine region number and lag year for nfws
        nfws = self.nfw_coefficients.copy()
        nfws = nfws.merge(self.mapping[nam.region_number],
                        left_index=True,
                        right_index=True,
                        how='left')
        nfws[nam.lag_year] = self.rest_curcalyr - nfws[nam.lag_years]

        #Get lagged prices
        #Crude
        temp_price = self.off_reg_crude_price.copy()
        temp_price = temp_price.stack()

        nfws = nfws.reset_index().merge(temp_price.to_frame(name=nam.crude_price),
                        left_on=[nam.region_number, nam.lag_year],
                        right_index=True,
                        how='left').set_index('evaluation_unit') #index

        #Natural gas
        temp_price = self.off_reg_natgas_price.copy()
        temp_price = temp_price.stack()

        nfws = nfws.reset_index().merge(temp_price.to_frame(name=nam.natgas_price),
                        left_on=[nam.region_number, nam.lag_year],
                        right_index=True,
                        how='left').set_index('evaluation_unit') #index

        #Base prices
        nfws[nam.off_base_oil_prc] = self.base_oil_prc
        nfws[nam.off_base_gas_prc] = self.base_gas_prc

        #Calculate model year nfws
        #Equation: nfws = alpha + beta * (crude_price * natgas_price)
        #Added base oil and gas price to reduce scale of changes and reduce oscillation between iterations
        nfws[int(self.rest_curcalyr)] = nfws[nam.nfw_alpha] \
                                   + nfws[nam.nfw_beta] \
                                   * ((nfws[nam.crude_price] + nfws[nam.off_base_oil_prc] / 2) \
                                    * nfws[nam.natgas_price] + nfws[nam.off_base_gas_prc] / 2)

        #Apply technology improvement
        rate = (self.exp_success_improve_rate * (self.rest_curcalyr - self.zero_year + 1) / 100.0) * self.parent.side_case_adj
        nfws[int(self.rest_curcalyr)] = nfws[int(self.rest_curcalyr)] * (1 + rate)

        #Clear drilling in off-limits areas
        mask = self.rest_curcalyr < self.field_availability[nam.year_available]
        nfws.loc[mask, int(self.rest_curcalyr)] = 0.0

        #Move to self.cumulative_nfws and clean up typing
        nfws[int(self.rest_curcalyr)] = nfws[int(self.rest_curcalyr)].apply(np.ceil)
        self.cumulative_nfws[int(self.rest_curcalyr)] = nfws[int(self.rest_curcalyr)].copy().astype(int)

        pass


    def calculate_new_fields(self):
        """Calculate number of new fields discovered based on new field wildcat drilling.

        Using Arps-Roberts discovery algorithm. The discovery coefficient (self.discovery_coefficients) is scaled by the
        number of total fields (self.total_fields).

        Returns
        -------
        self.discovered_field_dist : df
            DataFrame of discovered fields distribution across evaluation units

        self.discovered_fields : df
            DataFrame of discovered fields
        """
        ###Calculate new discoveries
        cum_nfws = self.cumulative_nfws.sum(axis=1)
        total = self.total_fields * (1.0 - np.exp((-self.discovery_coefficients/self.total_fields).mul(cum_nfws, axis=0)))
        total = total.fillna(0.0)
        total = total.apply(np.floor)

        new_fields = total - self.discovered_field_dist
        mask = new_fields < 0
        new_fields[mask] = 0

        #Discovered fields distribution = past discovered fields + new discovered fields
        self.discovered_field_dist = self.discovered_field_dist + new_fields


        ###Load fields into discovered list
        #see here: https://stackoverflow.com/questions/63672289/how-do-i-expanding-rows-by-a-count-column-in-pandas?noredirect=1&lq=1
        #Reshape to make 1 line per field
        new_fields = new_fields.stack()
        new_fields = new_fields.loc[new_fields.index.repeat(new_fields)].reset_index()
        new_fields = new_fields.rename(columns={'level_1': nam.field_size_class})
        new_fields = new_fields.drop(0, axis=1)

        #Set discovery and available year
        new_fields[nam.discovery_year] = self.rest_curcalyr
        new_fields = new_fields.reset_index().merge(self.field_availability[[nam.lag_years]],
                              left_on=nam.evaluation_unit,
                              right_index=True,
                              how='left').set_index('index')

        #Apply technology improvement
        rate = (self.exp_delay_improve_rate * (self.rest_curcalyr - self.zero_year + 1) / 100.0) * self.parent.side_case_adj
        if rate > 1:  # make sure lag years not negative
            rate = 1
        new_fields[nam.lag_years] = new_fields[nam.lag_years] * (1 - rate)

        new_fields[nam.available_year] = round(self.rest_curcalyr + new_fields[nam.lag_years])
        new_fields[nam.available_year] = new_fields[nam.available_year].astype(int)

        #Add new discoveries to list of discovered fields
        self.discovered_fields = pd.concat([self.discovered_fields, new_fields], ignore_index=True)

        pass


    def calculate_new_field_properties(self):
        """Assigns properties (e.g. drilling depth, water depth) for discovered fields.

        Properties are assigned based upon field evaluation unit.

        Returns
        -------
        self.discovered_fields : df
            DataFrame of discovered fields
        """
        #Get region
        self.discovered_fields = self.discovered_fields.drop([nam.region_number, nam.region_name, nam.district_number], axis=1, errors='ignore')
        self.discovered_fields = self.discovered_fields.reset_index().merge(self.mapping[[nam.region_number, nam.region_name, nam.district_number]],
                                          left_on=nam.evaluation_unit,
                                          right_index=True,
                                          how='left').set_index('index')


        #Zero out fields that are in the Atlantic (no drilling in the Atlantic)
        region_mask = self.discovered_fields[nam.region_number] != 8
        self.discovered_fields = self.discovered_fields[region_mask]

        #Get drilling depth
        self.discovered_fields = self.discovered_fields.drop(nam.drill_depth, axis=1, errors='ignore')
        self.discovered_fields = self.discovered_fields.reset_index().merge(self.drill_depth,
                                          left_on=[nam.evaluation_unit, nam.field_size_class],
                                          right_index=True,
                                          how='left').set_index('index')

        #Get water depth
        self.discovered_fields = self.discovered_fields.drop(nam.water_depth_ft, axis=1, errors='ignore')
        self.discovered_fields = self.discovered_fields.reset_index().merge(self.water_depth,
                                          left_on=[nam.evaluation_unit, nam.field_size_class],
                                          right_index=True,
                                          how='left').set_index('index')

        self.discovered_fields[nam.water_depth_m] = (self.discovered_fields[nam.water_depth_ft] / 3.28084).astype('int64')
        self.discovered_fields[nam.field_size_class] = (self.discovered_fields[nam.field_size_class]).astype('int64')

        #Determine platform structure type
        self.discovered_fields[nam.platform_type] = com.df_interpolate_2(self.discovered_fields,
                                                                         self.platform_structure_type,
                                                                         nam.field_size_class,
                                                                         nam.water_depth_m,
                                                                         ydir='backward')

        #Set discovered fields to both oil and gas
        self.discovered_fields[nam.oil_or_gas] = nam.both

        #Get production properties
        self.discovered_fields = self.discovered_fields.drop([nam.ramp_up_years,
                                                              nam.predecline_prod_fraction_oil,
                                                              nam.initial_oil_prod_rate,
                                                              nam.oil_decline_rate,
                                                              nam.predecline_prod_fraction_gas,
                                                              nam.initial_gas_prod_rate,
                                                              nam.gas_decline_rate,
                                                              nam.hyper_decline_coef], axis=1, errors='ignore')
        self.discovered_fields = self.discovered_fields.reset_index().merge(self.undiscovered_production,
                                          left_on=nam.evaluation_unit,
                                          right_index=True,
                                          how='left').set_index('index')

        self.discovered_fields[nam.discovery_type] = nam.discovered

        #Set index relative to current fields index
        self.discovered_fields.index = self.discovered_fields.index + (self.announced_fields.index.max() + 1)

        pass


    def load_fields(self):
        """Load available announced and discovered fields for economic analysis.

        Returns
        -------
        self.fields : df
            Main DataFrame of fields with field-level attributes
        """
        year = self.rest_curcalyr
        mask = self.announced_fields[nam.year_production_start] == year
        #If there are no existing fields, need to join outer to get all the columns assigned correctly, otherwise, join inner
        #Announced fields
        if len(self.fields):
            self.fields = pd.concat([self.fields, self.announced_fields[mask]], join='inner', ignore_index=False)
        else:
            self.fields = self.announced_fields[mask].copy()

        #Discovered fields
        if not self.discovered_fields.empty:
            mask = self.discovered_fields[nam.available_year] == year
            self.fields = pd.concat([self.fields, self.discovered_fields[mask].copy()], ignore_index=False)

        pass


    def load_prices(self):
        """Calculate average prices for Offshore regions.

        Averages prices over the years set in averaging_years. Currently set to only use current year data.

        Returns
        -------
        self.fields : df
            Main DataFrame of fields with field-level attributes
        """
        avg_years = list(range(self.rest_curcalyr - self.averaging_years, self.rest_curcalyr + 1))

        #Get crude price
        temp_price = self.fields[nam.region_number].reset_index().merge(self.off_reg_crude_price[avg_years],
                              left_on=nam.region_number,
                              right_index=True,
                              how='left').set_index('index')
        temp_price[nam.crude_price] = temp_price[avg_years].mean(axis=1)
        self.fields[nam.crude_price] = temp_price[nam.crude_price].copy()

        #Get natural gas price
        temp_price = self.fields[nam.region_number].reset_index().merge(self.off_reg_natgas_price[avg_years],
                              left_on=nam.region_number,
                              right_index=True,
                              how='left').set_index('index')
        temp_price[nam.natgas_price] = temp_price[avg_years].mean(axis=1)
        self.fields[nam.natgas_price] = temp_price[nam.natgas_price].copy()

        pass


    def calculate_drilling(self):
        """For the loaded fields, calculate annual drilling.

            * Fields are merged to platform slots,  platform delays, and drilling/year
            * Drilling/year is multiplied by available platform slots

        Returns
        -------
        self.fields : df
            Main DataFrame of fields with field-level attributes
        """
        #Get number of platform slots per field
        self.fields[nam.platform_slots] = com.df_interpolate_2(self.fields,
                                                               self.platform_slots,
                                                               nam.field_size_class,
                                                               nam.water_depth_m,
                                                               ydir='backward')

        #Reset Fields to int
        self.fields = self.fields.astype({nam.water_depth_m: 'int64'})
        self.fields = self.fields.astype({nam.water_depth_ft: 'int64'})
        self.fields = self.fields.astype({nam.field_size_class: 'int64'})
        self.fields = self.fields.astype({nam.drill_depth: 'int64'})

        #Not all platforms are represented in self.platform_delays
        self.fields = self.fields.drop(nam.drill_per_year, axis=1, errors='ignore')
        self.fields[nam.drill_per_year] = com.df_interpolate_2(self.fields,
                                                               self.platform_drill_per_year,
                                                               nam.platform_type,
                                                               nam.water_depth_ft,
                                                               xmerge='merge')

        self.fields = self.fields.sort_values(nam.water_depth_ft)

        #Merge eligible wells to drilling per year
        mask = self.fields[nam.drill_per_year].isnull()
        self.fields.loc[mask, nam.drill_per_year] = pd.merge_asof(self.fields, self.drill_per_year,
                                                                  left_on=nam.water_depth_ft,
                                                                  right_index=True,
                                                                  suffixes=['_', None])
        self.fields.loc[mask, nam.drill_per_year] = (self.fields.loc[mask, nam.drill_per_year] * self.fields.loc[mask, nam.platform_slots])

        pass


    def calculate_exploration_cost(self):
        """Calculate exploration wells and associated costs.

            * Well-level exploration costs are calculated
            * Delineation wells and exploration success rates are merged to the cost calculation DataFrame
            * Number of exploratory wells is calculated
            * Exploration costs are distributed based on delineation and exploratory wells drilled

        Returns
        -------
        self.fields : df
            Main DataFrame of fields with field-level attributes
        """
        #Get cost parameters
        cost = pd.merge_asof(self.fields.sort_values(nam.water_depth_m),
                             self.exp_cost[['alpha', 'beta1', 'beta2', 'beta3', 'power']],
                             left_on=nam.water_depth_m,
                             right_index=True,
                             direction='backward')

        #Calculate base drilling cost
        #alpha + beta1 * water_depth + beta2 * (drill_depth + water_depth) + beta3 * water_depth * drill_depth ^ power
        cost[nam.exp_drill_cost] = (cost['alpha'] +
                                    cost['beta1'] * cost[nam.water_depth_ft] +
                                    cost['beta2'] * (cost[nam.drill_depth] + cost[nam.water_depth_ft]) +
                                    cost['beta3'] * cost[nam.water_depth_ft] *
                                    (cost[nam.drill_depth].pow(cost['power'])))

        #Convert cost back to 1987 dollars
        cost[nam.exp_drill_cost] = (cost[nam.exp_drill_cost] * com.calculate_inflation(self.parent.rest_mc_jpgdp, self.drill_cost_base))

        #Adjust price reduction due to tech improvement
        rate = (self.drill_cost_improve_rate * (self.rest_curcalyr - self.zero_year + 1) / 100.0) * self.parent.side_case_adj
        cost[nam.exp_drill_cost] = cost[nam.exp_drill_cost] * (1.0 - rate)

        #OGSM has an oil price adjustment to exploration cost
        temp_adj = (1. + (cost[nam.crude_price] / self.oil_price_cost_base - 1.0) * self.drill_price_ratio * 2.0)
        cost[nam.exp_drill_cost] = cost[nam.exp_drill_cost] * temp_adj

        #OGSM adjusts price up by 1.15
        cost[nam.exp_drill_cost] = cost[nam.exp_drill_cost] * 1.15

        #Get number of delineation and exploratory wells based on costs
        cost = cost.drop(nam.delineation_wells, axis=1, errors='ignore')
        cost = cost.reset_index().merge(self.delineation_wells,
                        left_on=[nam.evaluation_unit, nam.field_size_class],
                        right_index=True,
                        how='left').set_index('index').sort_index()

        cost = cost.drop(nam.exp_success_rate, axis=1, errors='ignore')
        cost = cost.reset_index().merge(self.exp_success_rate,
                        left_on=[nam.evaluation_unit, nam.field_size_class],
                        right_index=True,
                        how='left').set_index('index')
        cost[nam.exploratory_wells] = (1.0/cost[nam.exp_success_rate]).apply(np.ceil).astype('int')

        #Convert per well drill cost to total drill costs
        cost[nam.exp_drill_cost] = cost[nam.exp_drill_cost] * (cost[nam.delineation_wells] + cost[nam.exploratory_wells])

        self.fields = self.fields.drop([nam.exp_drill_cost, nam.delineation_wells, nam.exploratory_wells], axis=1, errors='ignore')
        self.fields = self.fields.merge(cost[[nam.exp_drill_cost, nam.delineation_wells, nam.exploratory_wells]],
                               right_index=True,
                               left_index=True,
                               how='left')

        pass


    def calculate_delay_years(self):
        """For the loaded fields, calculate the number of delay years between drilling being discovered and being eligible for production.

        Returns
        -------
        self.fields : df
            Main DataFrame of fields with field-level attributes
        """
        ###Reformat fields and get development wells
        #Force self.fields values back to integer, needed for the merge_asof in df_interpolate_2
        self.fields = self.fields.astype({nam.water_depth_m: 'int64'})
        self.fields = self.fields.astype({nam.water_depth_ft: 'int64'})
        self.fields = self.fields.astype({nam.field_size_class: 'int64'})
        self.fields = self.fields.astype({nam.drill_depth: 'int64'})

        self.fields = self.fields.drop(nam.development_wells, axis=1, errors='ignore')
        self.fields[nam.development_wells] = com.df_interpolate_2(self.fields,
                                                                  self.development_wells,
                                                                  nam.water_depth_m,
                                                                  nam.field_size_class)


        ###Determine number of years delay
        # not all platforms are represented in self.platform_delays
        self.fields[nam.delay_years] = com.df_interpolate_2(self.fields,
                                                            self.platform_delays,
                                                            nam.platform_type,
                                                            nam.water_depth_ft,
                                                            xmerge='merge')
        mask = self.fields[nam.delay_years].isnull()

        self.fields[nam.total_wells] = (self.fields[nam.development_wells] +
                                        self.fields[nam.delineation_wells] +
                                        self.fields[nam.exploratory_wells])
        self.fields[nam.total_wells] = self.fields[nam.total_wells].fillna(0.0).astype(np.int64)
        self.fields.loc[mask, nam.delay_years] = com.df_interpolate_2(self.fields,
                                                                      self.delays,
                                                                      nam.total_wells,
                                                                      nam.water_depth_ft)

        #Adjust delay for tech improvement
        rate = (self.delay_improve_rate * (self.rest_curcalyr - self.zero_year + 1) / 100.0) * self.parent.side_case_adj
        self.fields[nam.delay_years] = self.fields[nam.delay_years] * (1.0 - rate)
        self.fields[nam.delay_years] = self.fields[nam.delay_years].round()

        #Merging with NaN's forces type to float, force back to int
        self.fields = self.fields.astype({nam.delay_years: 'int64'})

        pass


    def calculate_platform_cost(self):
        """For the loaded fields, calculate platform, abandonment, and flowline costs.

        Cost equations transferred from hard-coded cost equations in OGSM.

        Returns
        -------
        self.fields : df
            Main DataFrame of fields with field-level attributes
        """
        mask = self.fields[nam.platform_type] == nam.off_fp
        self.fields[nam.platform_cost] = (2000000. + 9000. * self.fields.loc[mask, nam.platform_slots] +
                                          (1500. * self.fields.loc[mask, nam.water_depth_ft] *
                                           self.fields.loc[mask, nam.platform_slots]) +
                                          40. * self.fields.loc[mask, nam.water_depth_ft] ** 2)

        mask = self.fields[nam.platform_type] == nam.off_ct
        self.fields[nam.platform_cost].update((self.fields.loc[mask, nam.platform_slots] + 30.) *
                                              (1500000. + 2000. * (self.fields.loc[mask, nam.water_depth_ft]-1000.)))

        mask = self.fields[nam.platform_type] == nam.off_tlp
        self.fields[nam.platform_cost].update(2 * ((self.fields.loc[mask, nam.platform_slots] + 30.) *
                                                   (3000000. + 750. * (self.fields.loc[mask, nam.water_depth_ft]-1000.))))

        mask = self.fields[nam.platform_type] == nam.off_fps
        self.fields[nam.platform_cost].update((self.fields.loc[mask, nam.platform_slots] + 20.) *
                                              (7500000. + 250. * (self.fields.loc[mask, nam.water_depth_ft]-1000.)))

        mask = self.fields[nam.platform_type] == nam.off_spar
        self.fields[nam.platform_cost].update(100 * ((self.fields.loc[mask, nam.platform_slots] + 20.) *
                                                     (3000000. + 500. * (self.fields.loc[mask, nam.water_depth_ft]-1000.))))

        mask = self.fields[nam.platform_type] == nam.off_fpso
        self.fields[nam.platform_cost].update(4 * ((self.fields.loc[mask, nam.platform_slots] + 20.) *
                                                   (7500000. + 250. * (self.fields.loc[mask, nam.water_depth_ft]-1000.))))

        #ss not used, only flowline costs for ss
        mask = self.fields[nam.platform_type] == nam.off_ss
        #Deprecated ss code
        #self.fields[nam.platform_cost].update((self.fields.loc[mask, nam.platform_slots] + 20.) *
        #                                      (4000000. + 250. * (self.fields.loc[mask, nam.water_depth_ft]-1000.)))
        self.fields.loc[mask, nam.platform_cost] = 0.0

        #warnings
        if self.fields[nam.platform_cost].isnull().values.any():
            warnings.warn('Invalid production facility type', UserWarning)

        #Add cost adder for Eastern Gulf of Mexico
        egom_mask = self.fields[nam.district_number] == 79
        temp = self.fields[egom_mask].copy()
        temp[nam.platform_cost] = temp[nam.platform_cost] * 1.06
        self.fields.update(temp)

        rate = self.platform_cost_improve_rate * (self.rest_curcalyr - self.zero_year + 1) / 100.0
        self.fields[nam.platform_cost] = self.fields[nam.platform_cost] * (1.0 - rate)

        #inflation adjustment
        self.fields[nam.platform_cost] = (self.fields[nam.platform_cost] * com.calculate_inflation(self.parent.rest_mc_jpgdp, self.platform_cost_year))


        ###Abandonment costs
        temp_abandon_rate = self.fields[[nam.platform_type]].reset_index().merge(self.platform_abandonment,
                                     left_on=nam.platform_type,
                                     right_index=True,
                                     how='left').set_index('index')

        self.fields[nam.abandon_rate] = self.fields[nam.platform_cost] * temp_abandon_rate[nam.platform_abandonment]


        ###Flowline costs
        self.fields = self.fields.drop(nam.flowlines, axis=1, errors='ignore')
        self.fields[nam.flowlines] = (self.fields[nam.total_wells] / self.wells_per_flowline).apply(np.ceil)
        self.fields[nam.flowline_cost] = self.fields[nam.flowlines] * (0.042 * 1000000 * self.pipeline_diameter *
                                                                       (self.fields[nam.water_depth_ft] / 600.)**0.1)

        #inflation adjustment
        self.fields[nam.flowline_cost] = (self.fields[nam.flowline_cost] * com.calculate_inflation(self.parent.rest_mc_jpgdp, self.platform_cost_year))

        #price adjustment
        temp_adj = (1. + (self.fields[nam.crude_price] / self.oil_price_cost_base - 1.0) * self.drill_price_ratio)
        self.fields[nam.flowline_cost] = self.fields[nam.flowline_cost] * temp_adj

        pass


    def calculate_development_cost(self):
        """For the loaded fields, calculate development drilling costs.

        Returns
        -------
        self.fields : df
            Main DataFrame of fields with field-level attributes
        """
        #Force self.fields values back to integer, needed for the merge_asof in df_interpolate_2
        self.fields = self.fields.astype({nam.water_depth_m: 'int64'})
        self.fields = self.fields.astype({nam.water_depth_ft: 'int64'})
        self.fields = self.fields.astype({nam.field_size_class: 'int64'})
        self.fields = self.fields.astype({nam.drill_depth: 'int64'})

        #Get base cost df
        cost = com.df_interpolate_2(self.fields[[nam.water_depth_m,
                                                 nam.drill_depth]],
                                    self.dev_cost,
                                    nam.water_depth_m,
                                    nam.drill_depth,
                                    xdir='backward',
                                    ydir='backward')

        #Concat other values needed for cost calculation
        cost = pd.concat([cost, self.fields[[nam.water_depth_m,
                                             nam.water_depth_ft,
                                             nam.drill_depth,
                                             nam.crude_price]]], axis=1)

        #Calculate drilling costs
        #alpha + water_depth * (beta1 + beta2 * drill_depth) + (gamma1 * drill_depth + gamma2) * drill_depth)
        cost[nam.dev_drill_cost] = (cost['alpha'] +
                                    cost[nam.water_depth_ft] * (cost['beta1'] + cost['beta2'] * cost[nam.drill_depth]) +
                                    (cost['gamma1'] * cost[nam.drill_depth] + cost['gamma2']) * cost[nam.drill_depth])

        #Add completion cost
        cost[nam.dev_drill_cost] = cost[nam.dev_drill_cost] + cost[nam.completion_cost]

        #Adjust dev drill cost for inflation
        cost[nam.dev_drill_cost] = (cost[nam.dev_drill_cost] * com.calculate_inflation(self.parent.rest_mc_jpgdp, self.drill_cost_base))

        #Adjust price reduction due to tech improvement
        rate = (self.drill_cost_improve_rate * (self.rest_curcalyr - self.zero_year + 1) / 100.0) * self.parent.side_case_adj
        cost[nam.dev_drill_cost] = cost[nam.dev_drill_cost] * (1.0 - rate)

        #OGSM has an oil price adjustment to development cost
        temp_adj = (1. + (cost[nam.crude_price] / self.oil_price_cost_base - 1.0) * self.drill_price_ratio * 2.0)
        cost[nam.dev_drill_cost] = cost[nam.dev_drill_cost] * temp_adj

        #OGSM adjusts price up by 1.30
        cost[nam.dev_drill_cost] = cost[nam.dev_drill_cost] * 1.30
        self.fields[nam.dev_drill_cost] = cost[nam.dev_drill_cost]

        pass


    def calculate_operating_cost(self):
        """For the loaded fields, calculate operating costs.

        Returns
        -------
        self.fields : df
            Main DataFrame of fields with field-level attributes
        """
        #Collect required field values
        cost = self.fields[[nam.water_depth_ft, nam.platform_slots, nam.crude_price]].copy()

        #Set parameters across operating costs
        cost['alpha'] = self.operating_cost.at[0, 'alpha']
        cost['beta1'] = self.operating_cost.at[0, 'beta1']
        cost['beta2'] = self.operating_cost.at[0, 'beta2']

        #If > 1500 ft, use water_depth, otherwise use 1500 constant
        #alpha + beta1 * platform_slots + beta2 * platform_slots * water_depth ^ 2
        mask = cost[nam.water_depth_ft] <= 1500
        cost.loc[mask, nam.operating_cost] = (cost['alpha'] +
                                              cost['beta1'] * cost[nam.platform_slots] +
                                              cost['beta2'] * cost[nam.platform_slots] * cost[nam.water_depth_ft] ** 2)
        mask = cost[nam.water_depth_ft] > 1500
        cost.loc[mask, nam.operating_cost] = (cost['alpha'] +
                                              cost['beta1'] * cost[nam.platform_slots] +
                                              cost['beta2'] * cost[nam.platform_slots] * 1500. ** 2)

        #Adjust for inflation
        cost[nam.operating_cost] = (cost[nam.operating_cost] * com.calculate_inflation(self.parent.rest_mc_jpgdp, self.drill_cost_base))

        #OGSM matching, recalculate per slot
        cost[nam.operating_cost] = cost[nam.operating_cost]/cost[nam.platform_slots]

        #Apply tech improvement
        rate = (self.operating_cost_improve_rate * (self.rest_curcalyr - self.zero_year + 1) / 100.0) * self.parent.side_case_adj
        cost[nam.operating_cost] = cost[nam.operating_cost] * (1.0 - rate)

        #Apply price adjustment to operating costs
        temp_adj = (1. + (cost[nam.crude_price] / self.oil_price_cost_base - 1.0) * self.drill_price_ratio)
        cost[nam.operating_cost] = cost[nam.operating_cost] * temp_adj
        self.fields[nam.operating_cost] = cost[nam.operating_cost]

        pass


    def load_cash_flow_properties(self):
        """For the loaded fields, load CashFlow properties in preparation for NPV/DCF calculation.

        Returns
        -------
        self.cash_flow.properties : df
            DataFrame containing properties used in the cash flow (costs, tangible/intangible cost ratios, etc.)

        self.cash_flow.exp_drill_cost : df
            DataFrame of exploratory well drill costs

        self.cash_flow.dev_drill_cost : df
            DataFrame of development well drill costs

        self.cash_flow.operating_cost : df
            DataFrame of operating costs

        self.cash_flow.kap_cost : df
            DataFrame pf capital costs

        self.fields : df
            Main DataFrame of fields with field-level attributes
        """
        #Sort fields based on index
        #self.fields = self.fields.sort_index()

        ###Initiate properties
        #Grab 1:1 properties from the fields DataFrame
        self.cash_flow.properties = self.fields[[nam.field_size_class,
                                                 nam.evaluation_unit,
                                                 nam.crude_price,
                                                 nam.natgas_price,
                                                 nam.abandon_rate]].copy()

        #Set unused offshore properties to 0
        self.cash_flow.properties[[nam.crude_tariff_price, nam.natgas_tariff_price]] = 0.0

        #Set unused offshore DataFrames to 0
        self.cash_flow.general_admin_cost   = pd.DataFrame(index=self.fields.index, columns=list(range(self.evaluation_years))).fillna(0.0)
        self.cash_flow.depletion            = pd.DataFrame(index=self.fields.index, columns=list(range(self.evaluation_years))).fillna(0.0)
        self.cash_flow.dry_hole_cost        = pd.DataFrame(index=self.fields.index, columns=list(range(self.evaluation_years))).fillna(0.0)
        self.cash_flow.eor_tax_credit       = pd.DataFrame(index=self.fields.index, columns=list(range(self.evaluation_years))).fillna(0.0)
        self.cash_flow.invest_credit        = pd.DataFrame(index=self.fields.index, columns=list(range(self.evaluation_years))).fillna(0.0)
        self.cash_flow.geo_geo_and_lease_aq = pd.DataFrame(index=self.fields.index, columns=list(range(self.evaluation_years))).fillna(0.0)
        self.cash_flow.exp_dry_cost         = pd.DataFrame(index=self.fields.index, columns=list(range(self.evaluation_years))).fillna(0.0)
        self.cash_flow.dev_dry_cost         = pd.DataFrame(index=self.fields.index, columns=list(range(self.evaluation_years))).fillna(0.0)
        self.cash_flow.equip_cost           = pd.DataFrame(index=self.fields.index, columns=list(range(self.evaluation_years))).fillna(0.0)
        self.cash_flow.fed_credit           = pd.DataFrame(index=self.fields.index, columns=list(range(self.evaluation_years))).fillna(0.0)
        self.cash_flow.tangible_adjustment  = pd.DataFrame(index=self.fields.index, columns=list(range(self.evaluation_years))).fillna(0.0)
        self.cash_flow.gg_la_cost           = pd.DataFrame(index=self.fields.index, columns=list(range(self.evaluation_years))).fillna(0.0)
        self.cash_flow.ch4_emission_cost    = pd.DataFrame(index=self.fields.index, columns=list(range(self.evaluation_years))).fillna(0.0)
        self.cash_flow.ch4_emissions        = pd.DataFrame(index=self.fields.index, columns=list(range(self.evaluation_years))).fillna(0.0)

        #Constant properties
        self.cash_flow.properties[nam.exp_tang_frac     ] = self.exp_tang_frac
        self.cash_flow.properties[nam.dev_tang_frac     ] = self.dev_tang_frac
        self.cash_flow.properties[nam.kap_tang_frac     ] = self.kap_tang_frac
        self.cash_flow.properties[nam.intang_amor_frac  ] = self.intang_amor_frac
        self.cash_flow.properties[nam.amor_schedule     ] = self.amor_schedule
        self.cash_flow.properties[nam.deprec_schedule   ] = self.deprec_schedule
        self.cash_flow.properties[nam.fed_tax_rate      ] = self.fed_tax_rate

        #Set offshore "state"
        self.cash_flow.properties[nam.state] = nam.ocs

        #Load exploration cost, everything zero, except first year
        self.cash_flow.exp_drill_cost = pd.DataFrame(index=self.fields.index, columns=list(range(self.evaluation_years))).fillna(0.0)
        self.cash_flow.exp_drill_cost[0] = self.fields[nam.exp_drill_cost].copy()

        #Calculate drilling schedule, development cost and operating cost
        self.fields_schedule = self.fields.apply(drill_eq.off_drill_schedule, evaluation_years=self.evaluation_years, axis=1)
        self.cash_flow.dev_drill_cost = self.fields_schedule.mul(self.fields[nam.dev_drill_cost], axis=0)

        #Calculate operating costs
        temp_op_schedule = self.fields.apply(drill_eq.off_operating_schedule, evaluation_years=self.evaluation_years, axis=1)
        self.cash_flow.operating_cost = temp_op_schedule.mul(self.fields[nam.operating_cost], axis=0)

        #Distribute platform costs to kap costs
        self.cash_flow.kap_cost = pd.DataFrame(index=self.fields.index, columns=list(range(self.evaluation_years))).fillna(0.0)
        temp = self.fields[[nam.delay_years]].reset_index().merge(self.prod_fac_cost_frac,
                        left_on=nam.delay_years,
                        right_index=True,
                        how='left').set_index('index')
        temp = temp.mul(self.fields[nam.platform_cost] + self.fields[nam.flowline_cost], axis=0)
        self.cash_flow.kap_cost.update(temp)

        #Load transportation costs
        self.cash_flow.properties = self.cash_flow.properties.reset_index('index').merge(self.transportation,
                                             left_on=nam.evaluation_unit,
                                             right_index=True,
                                             how='left').set_index('index')
        #Load royalty rates
        self.cash_flow.properties = self.cash_flow.properties.reset_index().merge(self.royalty_rates,
                                             left_on=[nam.evaluation_unit, nam.field_size_class],
                                             right_index=True,
                                             how='left').set_index('index')

        #Apply Discount Rate Adjustment
        self.fields = self.fields.reset_index().merge(self.mapping[[nam.discount_rate_adjustment]],
                               left_on=nam.evaluation_unit,
                               right_index=True,
                               how='left').set_index('index')

        self.fields[nam.discount_rate] = self.fields[nam.discount_rate_adjustment] * self.discount_rate

        self.cash_flow.properties[nam.discount_rate] = self.fields[nam.discount_rate].copy()

        pass


    def load_cash_flow_production(self):
        """
        For the loaded fields, load CashFlow object production in preparation for NPV/DCF calculation.

        Returns
        -------

        self.cash_flow.crude_production
            DataFrame of onshore crude oil production

        self.cash_flow.natgas_production : df
            DataFrame of onshore natural gas production

        self.fields : df
            Main DataFrame of fields with field-level attributes
        """
        ###Calculate production
        #Calculate production based on types oil, gas, both
        temp_resources = self.fields[[nam.field_size_class]].reset_index().merge(self.field_size_classes[[nam.mean_resources_boe]],
                                  left_on=nam.field_size_class,
                                  right_index=True,
                                  how='left').set_index('index')[nam.mean_resources_boe]
        
        # Side case adjustments
        temp_resources *= self.parent.side_case_adj
        
        # Merge production ratios
        self.fields = self.fields.reset_index().merge(self.gas_ratio_and_cond[[nam.gas_proportion, nam.gas_oil_ratio, nam.condensate_yield]],
                               left_on=nam.evaluation_unit,
                               right_index=True,
                               how='left').set_index('index')
        self.fields.loc[self.fields[nam.oil_or_gas] == nam.oil  , nam.oil_resources] = temp_resources
        self.fields.loc[self.fields[nam.oil_or_gas] == nam.gas  , nam.gas_resources] = temp_resources * self.boe_to_mcf
        self.fields.loc[self.fields[nam.oil_or_gas] == nam.both , nam.oil_resources] = (temp_resources * (1.0 - self.fields[nam.gas_proportion]))
        self.fields.loc[self.fields[nam.oil_or_gas] == nam.both , nam.gas_resources] = (temp_resources * self.boe_to_mcf * self.fields[nam.gas_proportion])

        self.fields[[nam.oil_resources, nam.gas_resources]] = self.fields[[nam.oil_resources, nam.gas_resources]].fillna(0.0)


        ###Apply production profile for discovered fields
        mask = self.fields[nam.discovery_type] == nam.discovered
        self.fields.loc[mask, nam.max_oil_production_rate] = self.fields[nam.initial_oil_prod_rate] * self.fields[nam.drill_per_year]
        self.fields.loc[mask, nam.max_gas_production_rate] = self.fields[nam.initial_gas_prod_rate] * self.fields[nam.drill_per_year]

        #Get unprocessed fields to calculate crude production by field
        crude_index_mask  = self.field_crude_production.index
        natgas_index_mask = self.field_natgas_production.index
        joint_index_mask = list(set(crude_index_mask.append(natgas_index_mask)))
        unprocessed_fields = self.fields.loc[~self.fields.index.isin(joint_index_mask)].copy()


        ###Calculate crude production by field
        baseline_crude = unprocessed_fields.apply(drill_eq.off_production_profile,
                                           evaluation_years=self.evaluation_years,
                                           hist_year=(self.parent.history_year - 1),
                                           oil_or_gas=nam.oil,
                                           axis=1)
        baseline_natgas = unprocessed_fields.apply(drill_eq.off_production_profile,
                                            evaluation_years=self.evaluation_years,
                                            hist_year=(self.parent.history_year - 1),
                                            oil_or_gas=nam.gas,
                                            axis=1)


        ###Split Production into AD and NA Gas
        associated_gas_production           = baseline_crude.mul(unprocessed_fields[nam.gas_oil_ratio], axis=0)
        non_condensate_production           = baseline_crude - associated_gas_production / self.boe_to_mcf
        condensate_production               = baseline_natgas.mul(unprocessed_fields[nam.condensate_yield], axis=0)
        non_associated_gas_production       = baseline_natgas - condensate_production * self.boe_to_mcf


        ###Mask for remaining project crude production
        self.field_crude_production  =  self.field_crude_production[self.field_crude_production.index.isin(self.fields.index)]
        self.field_natgas_production = self.field_natgas_production[self.field_natgas_production.index.isin(self.fields.index)]


        ###Apply new fields to Crude Production
        crude_production                    = non_condensate_production + condensate_production
        natgas_production                   = associated_gas_production + non_associated_gas_production
        self.cash_flow.crude_production     = pd.concat([self.field_crude_production, crude_production], ignore_index=False)
        self.cash_flow.natgas_production    = pd.concat([self.field_natgas_production, natgas_production], ignore_index=False)

        #Get field crude and natgas production for local variables (used to transfer cashflow data from one year to the next to optimize code)
        self.field_crude_production     = self.cash_flow.crude_production.copy()
        self.field_natgas_production    = self.cash_flow.crude_production.copy()

        #Copy ad to na gas ratio so they can be split out after cashflow
        self.ad_fraction = associated_gas_production/(associated_gas_production + non_associated_gas_production)
        self.ad_fraction = self.ad_fraction.fillna(0)


        pass


    def run_cash_flow(self):
        """For the loaded fields, run through CashFlow.

        Returns
        -------
        self.fields : df
            Main DataFrame of fields with field-level attributes
        """
        #Run CashFlow
        self.cash_flow.calculate_revenue()
        self.cash_flow.calculate_royalty()
        self.cash_flow.calculate_severance()
        self.cash_flow.calculate_drill_cost()
        self.cash_flow.calculate_trans_cost()
        self.cash_flow.calculate_intangible_tangible()
        self.cash_flow.calculate_depreciation()
        self.cash_flow.calculate_econ_limit()
        self.cash_flow.calculate_abandonment()
        self.cash_flow.calculate_state_tax()
        self.cash_flow.calculate_fed_tax()
        self.cash_flow.calculate_cash_flow()

        #Copy npv to fields
        self.fields[nam.net_present_value] = self.cash_flow.properties[nam.net_present_value]

        pass


    def rank_fields(self):
        """Rank Fields by net present value.

        Returns
        -------
        self.fields : df
            Main DataFrame of fields with field-level attributes
        """
        self.fields = self.fields.sort_values([nam.discovery_type, nam.net_present_value], ascending=[True, False])
        ranks = range(len(self.fields))
        self.fields[nam.rank] = ranks

        pass


    def select_fields(self):
        """Select fields based on rank, net present value and rig capacity.

            * Run through drilling constraints to determine if there are sufficient rigs available to drill all profitable projects
            * Mask for projects that fall under rig constratints, and apply production volumes and well counts to relevant
              aggregate dfs (i.e. self.crude_production, self.natgas_production)

        Returns
        -------
        self.fields : df
            Main DataFrame of fields with field-level attributes

        self.crude_production : df
            DataFrame of crude production by field

        self.natgas_production : df
            DataFrame of onshore natural gas production

        self.adgas_production : df
            DataFrame of associated dissolved natural gas production by field

        self.nagas_production : df
            DataFrame of non-associated natural gas production by field

        self.wells : df
            DataFrame of wells by field

        self.fields_selected : df
            DataFrame of fields that have been selected to produce
        """
        #Ignore Fields availability and skip to rigs availability as a means to drill
        #Identify the rig type needed for each field and add to counts

        #Create "selected" column in year 1 to show if a given field was picked for drilling
        self.fields[nam.selected] = 0

        #Select positive npv fields
        mask = (self.fields[nam.net_present_value] > 0) | (self.fields[nam.discovery_type] == nam.announced)
        viable_fields = self.fields.copy().loc[mask]

        #Set "num_rigs" type to float
        self.drill_rig_constraint[nam.num_rigs] = self.drill_rig_constraint[nam.num_rigs].astype(float)

        #Create available rigs column to keep count of how many rigs are left for drilling
        self.drill_rig_constraint[nam.available_rigs] = self.drill_rig_constraint[nam.num_rigs]
        self.drill_rig_constraint[nam.rig_well_util] = 0.0

        #Set loop variables and counters
        sum_rigs = self.drill_rig_constraint[nam.num_rigs].sum()
        npv_rank_counter = 0

        #Set loop masks which assign water depths to specific rig types, which are ranked in order of preferred use
        mask_7500 = self.drill_rig_constraint[nam.use_rank] >= 7
        mask_5000 = self.drill_rig_constraint[nam.use_rank] >= 5
        mask_1500 = (self.drill_rig_constraint[nam.use_rank] >= 4) & (self.drill_rig_constraint[nam.use_rank] <= 6)
        mask_0    = self.drill_rig_constraint[nam.use_rank] <= 3

        #While there are still rigs or fields available in a given year, keep running the while loop
        while npv_rank_counter < len(viable_fields) and sum_rigs > 0:
            #Select first ranked field and continue down the list
            field_mask = viable_fields[nam.rank] == npv_rank_counter
            field = viable_fields[field_mask]

            #Select available rig type group by field
            if field.at[field.index[0], nam.water_depth_ft] > 7500:
                available_rigs = self.drill_rig_constraint.loc[mask_7500]
            elif field.at[field.index[0], nam.water_depth_ft] > 5000:
                available_rigs = self.drill_rig_constraint.loc[mask_5000]
            elif field.at[field.index[0], nam.water_depth_ft] > 1500:
                available_rigs = self.drill_rig_constraint.loc[mask_1500]
            else:
                available_rigs = self.drill_rig_constraint.loc[mask_0]

            #Set nested while loop parameters
            rig_rank_loop_counter = available_rigs[nam.use_rank].min()  # sets
            total_wells = field.at[field.index[0], nam.total_wells]
            rig_overhang = 0

            #Calculate rig use
            while rig_rank_loop_counter <= available_rigs[nam.use_rank].max() and total_wells > 0:

                #Loop Variables
                loop_mask = available_rigs[nam.use_rank] == rig_rank_loop_counter
                index = available_rigs.loc[loop_mask].index[0]
                util_conv = (self.drill_rig_constraint.at[index, nam.drill_rate] / 365)

                #Calculate rig utilization, if rig overhang > drilling, move to next rig
                if self.drill_rig_constraint.at[index, nam.available_rigs] - abs(rig_overhang - (total_wells * util_conv)) >= 0:

                    #Calculate available rigs and rig utilization
                    self.drill_rig_constraint.at[index, nam.available_rigs] = self.drill_rig_constraint.at[index, nam.available_rigs] - (total_wells * util_conv)
                    self.drill_rig_constraint.at[index, nam.rig_well_util]  = self.drill_rig_constraint.at[index, nam.rig_well_util ] + (total_wells * util_conv)

                    sum_rigs = sum_rigs - (field.at[field.index[0], nam.total_wells] * self.drill_rig_constraint.at[index, nam.drill_rate] / 365)

                    #Change selected_fields=1 and close loop
                    self.fields.at[field.index[0], nam.selected] = 1

                    total_wells = 0
                    rig_overhang = 0

                #If insufficient rigs are available subtract the remaining rigs then move to the next rig type available in "rank_loop"
                else:
                    #Number of rigs needed in excess of availability
                    rig_overhang = abs(self.drill_rig_constraint.at[index, nam.available_rigs] - total_wells * util_conv)

                    total_wells = rig_overhang

                    #Set utilization to max and availability to 0
                    self.drill_rig_constraint.at[index, nam.available_rigs] = 0
                    self.drill_rig_constraint.at[index, nam.rig_well_util] = self.drill_rig_constraint.at[index, nam.num_rigs]

                    #Set Warning
                    if rig_rank_loop_counter == available_rigs[nam.use_rank].max():
                        warnings.warn('WARNING: Not enough drilling capacity for Offshore wells', UserWarning)

                    #Increase rank loop var
                    rig_rank_loop_counter = rig_rank_loop_counter + 1

            #Move to next field
            npv_rank_counter = npv_rank_counter + 1

        #Get announced fields and set as selected
        mask = (self.fields[nam.discovery_type] == nam.announced)
        self.fields.loc[mask, nam.selected] = 1

        #Trigger selected fields boolean so that the same fields are not repeatedly picked
        selected_mask = self.fields[nam.selected] == 1

        ###Get production from cash_flow
        ###Crude oil
        temp = self.cash_flow.crude_production.loc[selected_mask].copy()
        temp.columns = temp.columns + self.rest_curcalyr
        # from https://stackoverflow.com/a/4587920
        labels = [i for i in temp.columns if i > self.parent.final_aeo_year]
        temp = temp.drop(labels, axis=1)
        temp[nam.evaluation_unit] = self.fields.loc[selected_mask, nam.evaluation_unit]
        temp[nam.discovery_type]  = self.fields.loc[selected_mask, nam.discovery_type]
        temp[nam.district_number]  = self.fields.loc[selected_mask, nam.district_number]
        temp[nam.region_number] = self.fields.loc[selected_mask, nam.region_number]
        temp[nam.water_depth_ft] = self.fields.loc[selected_mask, nam.water_depth_ft]
        temp[nam.federal_land]  = 'federal'
        self.crude_production = pd.concat([self.crude_production, temp], ignore_index=True)
        self.crude_production = self.crude_production.fillna(0)


        ###Natural gas
        #Create base tables
        temp = self.cash_flow.natgas_production.loc[selected_mask].copy()
        temp_ad = temp * self.ad_fraction
        temp_na = temp - temp_ad

        #Total Natural Gas
        temp.columns = temp.columns + self.rest_curcalyr
        labels = [i for i in temp.columns if i > self.parent.final_aeo_year]
        temp = temp.drop(labels, axis=1)
        temp[nam.evaluation_unit] = self.fields.loc[selected_mask, nam.evaluation_unit]
        temp[nam.discovery_type]  = self.fields.loc[selected_mask, nam.discovery_type]
        temp[nam.district_number]  = self.fields.loc[selected_mask, nam.district_number]
        temp[nam.region_number]  = self.fields.loc[selected_mask, nam.region_number]
        temp[nam.water_depth_ft] = self.fields.loc[selected_mask, nam.water_depth_ft]
        temp[nam.federal_land]  = 'federal'
        self.natgas_production = pd.concat([self.natgas_production, temp], ignore_index=True)
        self.natgas_production = self.natgas_production.fillna(0)

        #AD Natural Gas
        temp_ad.columns = temp_ad.columns + self.rest_curcalyr
        labels = [i for i in temp_ad.columns if i > self.parent.final_aeo_year]
        temp_ad = temp_ad.drop(labels, axis=1)
        temp_ad[nam.evaluation_unit] = self.fields.loc[selected_mask, nam.evaluation_unit]
        temp_ad[nam.discovery_type]  = self.fields.loc[selected_mask, nam.discovery_type]
        temp_ad[nam.district_number]  = self.fields.loc[selected_mask, nam.district_number]
        temp_ad[nam.region_number]  = self.fields.loc[selected_mask, nam.region_number]
        temp_ad[nam.water_depth_ft] = self.fields.loc[selected_mask, nam.water_depth_ft]
        temp_ad[nam.federal_land]  = 'federal'
        self.adgas_production = pd.concat([self.adgas_production, temp_ad], ignore_index=True)
        self.adgas_production = self.adgas_production.fillna(0)

        #NA Natural Gas
        temp_na.columns = temp_na.columns + self.rest_curcalyr
        labels = [i for i in temp_na.columns if i > self.parent.final_aeo_year]
        temp_na = temp_na.drop(labels, axis=1)
        temp_na[nam.evaluation_unit] = self.fields.loc[selected_mask, nam.evaluation_unit]
        temp_na[nam.discovery_type]  = self.fields.loc[selected_mask, nam.discovery_type]
        temp_na[nam.district_number]  = self.fields.loc[selected_mask, nam.district_number]
        temp_na[nam.region_number]  = self.fields.loc[selected_mask, nam.region_number]
        temp_na[nam.water_depth_ft] = self.fields.loc[selected_mask, nam.water_depth_ft]
        temp_na[nam.federal_land]  = 'federal'
        self.nagas_production = pd.concat([self.nagas_production, temp_na], ignore_index=True)
        self.nagas_production = self.nagas_production.fillna(0)


        ###Wells
        temp = self.fields_schedule.loc[selected_mask].copy()
        temp.columns = temp.columns + self.rest_curcalyr
        # from https://stackoverflow.com/a/4587920
        labels = [i for i in temp.columns if i > self.parent.final_aeo_year]
        temp = temp.drop(labels, axis=1)
        temp[nam.evaluation_unit] = self.fields.loc[selected_mask, nam.evaluation_unit]
        temp[nam.discovery_type]  = self.fields.loc[selected_mask, nam.discovery_type]
        temp[nam.district_number]  = self.fields.loc[selected_mask, nam.district_number]
        temp[nam.federal_land]  = 'federal'

        # join wells to evaluation units
        self.wells = pd.concat([self.wells, temp], join='outer', ignore_index=True)
        self.wells = self.wells.fillna(0)

        ###Exploratory Wells
        temp = self.fields[[nam.exploratory_wells]].copy()
        temp.columns = [int(self.rest_curcalyr)]
        # from https://stackoverflow.com/a/4587920
        labels = [i for i in temp.columns if i > self.parent.final_aeo_year]
        temp = temp.drop(labels, axis=1)
        temp[nam.evaluation_unit] = self.fields.loc[selected_mask, nam.evaluation_unit]
        temp[nam.discovery_type]  = self.fields.loc[selected_mask, nam.discovery_type]
        temp[nam.district_number]  = self.fields.loc[selected_mask, nam.district_number]
        temp[nam.federal_land]  = 'federal'

        self.exploratory_wells = pd.concat([self.exploratory_wells, temp], join='outer', ignore_index=True)
        self.exploratory_wells = self.exploratory_wells.fillna(0)


        #Remove selected fields from self.fields and add to selected_fields for debug
        self.fields_selected = pd.concat([self.fields[selected_mask].copy(), self.fields_selected], ignore_index=True)
        self.fields = self.fields.loc[~selected_mask]

        pass


    def expand_drilling_capacity(self):
        """Develop new drilling capacity based on rig utilization.

            * If profitable fields exceed rig constraints, add more rigs for next model year

        Returns
        -------
        self.drill_rig_constraint : df
            DataFrame of rig constaints by rig type
        """
        #Actual well drilling capacity (Wells/Rig)
        self.drill_rig_constraint[nam.rig_well_cap_eoy] = round(self.drill_rig_constraint[nam.num_rigs] * 365 / self.drill_rig_constraint[nam.drill_rate])

        #Calculate rig utilization based on target utilization (%)
        self.drill_rig_constraint[nam.rig_well_util_per] = self.drill_rig_constraint[nam.rig_well_util] / self.drill_rig_constraint[nam.num_rigs] * 100

        #If rig utilization is too high, then increase number of rigs
        #set mask
        mask = self.drill_rig_constraint[nam.rig_well_util_per] >= self.drill_rig_constraint[nam.target_rig_util]

        #Additional well capacity needed to reach utilization target
        self.drill_rig_constraint[nam.rig_well_cap_inc] = 0
        rig_well_util       = self.drill_rig_constraint.loc[mask, nam.rig_well_util   ]
        target_well_util    = self.drill_rig_constraint.loc[mask, nam.target_rig_util ]
        rig_well_cap_eoy    = self.drill_rig_constraint.loc[mask, nam.rig_well_cap_eoy]
        self.drill_rig_constraint.loc[mask, nam.rig_well_cap_inc] = (rig_well_util * (365 / self.drill_rig_constraint[nam.drill_rate]) /
                                                                     (target_well_util / 100) - rig_well_cap_eoy).apply(np.ceil)

        #Number of additional rigs needed to reach utilization target
        self.drill_rig_constraint[nam.rig_inc] = 0
        rig_well_cap_inc        = self.drill_rig_constraint.loc[mask, nam.rig_well_cap_inc]
        calculated_drilling_cap = self.drill_rig_constraint.loc[mask, nam.calculated_drilling_cap]
        self.drill_rig_constraint.loc[mask, nam.rig_inc] = ((rig_well_cap_inc / calculated_drilling_cap).apply(np.ceil))

        #calculate maximum rig increase based on well capacity and rig count
        self.drill_rig_constraint[nam.max_build] = 0
        max_rig_build_rate = self.drill_rig_constraint.loc[mask, nam.max_rig_build_rate]
        num_rigs = self.drill_rig_constraint.loc[mask, nam.num_rigs]
        self.drill_rig_constraint.loc[mask, nam.max_build] = ((max_rig_build_rate / 100) * num_rigs).apply(np.ceil)

        #Ensure new rigs aren't less than the minimum increment
        mask2 = self.drill_rig_constraint[nam.rig_inc] <= self.drill_rig_constraint[nam.min_rig_increment]  # set mask
        self.drill_rig_constraint.loc[mask2, nam.rig_inc] = self.drill_rig_constraint.loc[mask2, nam.min_rig_increment]

        #Ensure new rigs aren't greater than the maximum allowable amount
        mask3 = self.drill_rig_constraint[nam.rig_inc] >= self.drill_rig_constraint[nam.max_build]  # set mask
        self.drill_rig_constraint.loc[mask3, nam.rig_inc] = self.drill_rig_constraint[nam.max_build]

        #Add new rig count to existing rigs
        self.drill_rig_constraint[nam.num_rigs] = self.drill_rig_constraint[nam.num_rigs] + self.drill_rig_constraint[nam.rig_inc]

        #New well drilling capacity (Wells/Rig)
        self.drill_rig_constraint[nam.rig_well_cap_eoy] = round(self.drill_rig_constraint[nam.num_rigs] * 365 / self.drill_rig_constraint[nam.drill_rate])

        #New rig utilization based on target utilization (%)
        self.drill_rig_constraint[nam.rig_well_util_per] = self.drill_rig_constraint[nam.rig_well_util] / self.drill_rig_constraint[nam.num_rigs]

        #Set rig availability and utilization to ceiling values
        self.drill_rig_constraint[nam.rig_well_util]    = self.drill_rig_constraint[nam.rig_well_util ].apply(np.ceil)
        self.drill_rig_constraint[nam.available_rigs]   = self.drill_rig_constraint[nam.available_rigs].apply(np.ceil)

        pass


    def calculate_ngpls(self):
        """Calculate NGPL production volumes based on natural gas production factor.

        Returns
        -------
        self.ngpl_production : df
            DataFrame of onshore natural gas plant liquids production

        self.ngpl_ethane_production : df
            DataFrame of onshore ethane production

        self.ngpl_propane_production : df
            DataFrame of onshore propane production

        self.ngpl_butane_production : df
            DataFrame of onshore butane production

        self.ngpl_isobutane_production : df
            DataFrame of onshore isobutane production

        self.ngpl_proplus_production : df
            DataFrame of onshore pentanes production
        """

        ###Create NGPL DFs
        sim_years = list(range(self.zero_year, self.parent.final_aeo_year + 1))

        self.ngpl_production        = self.natgas_production.copy()
        self.ethane_production      = self.natgas_production.copy()
        self.propane_production     = self.natgas_production.copy()
        self.butane_production      = self.natgas_production.copy()
        self.isobutane_production   = self.natgas_production.copy()
        self.proplus_production     = self.natgas_production.copy()

        self.ngpl_production[sim_years]         = 0
        self.ethane_production[sim_years]       = 0
        self.propane_production[sim_years]      = 0
        self.butane_production[sim_years]       = 0
        self.isobutane_production[sim_years]    = 0
        self.proplus_production[sim_years]      = 0

        #Merge NGPL production to ngpl ratio
        self.ngpl_production = pd.merge(self.ngpl_production, self.mapping[[nam.ngpl_ratio]],
                                        left_on=nam.evaluation_unit, right_index=True, how='left')
        self.ethane_production = pd.merge(self.ethane_production, self.mapping[[nam.ethane_fraction]],
                                          left_on=nam.evaluation_unit, right_index=True, how='left')
        self.propane_production = pd.merge(self.propane_production, self.mapping[[nam.propane_fraction]],
                                           left_on=nam.evaluation_unit, right_index=True, how='left')
        self.butane_production = pd.merge(self.butane_production, self.mapping[[nam.butane_fraction]],
                                          left_on=nam.evaluation_unit, right_index=True, how='left')
        self.isobutane_production = pd.merge(self.isobutane_production, self.mapping[[nam.isobutane_fraction]],
                                             left_on=nam.evaluation_unit, right_index=True, how='left')
        self.proplus_production = pd.merge(self.proplus_production, self.mapping[[nam.natural_gasoline_fraction]],
                                               left_on=nam.evaluation_unit, right_index=True, how='left')

        #Calculate NGPL production
        self.ngpl_production[sim_years]         = self.natgas_production[sim_years].mul(self.ngpl_production[nam.ngpl_ratio], axis=0)
        self.ethane_production[sim_years]       = self.ngpl_production[sim_years].mul(self.ethane_production[nam.ethane_fraction], axis=0)
        self.propane_production[sim_years]      = self.ngpl_production[sim_years].mul(self.propane_production[nam.propane_fraction], axis=0)
        self.butane_production[sim_years]       = self.ngpl_production[sim_years].mul(self.butane_production[nam.butane_fraction], axis=0)
        self.isobutane_production[sim_years]    = self.ngpl_production[sim_years].mul(self.isobutane_production[nam.isobutane_fraction], axis=0)
        self.proplus_production[sim_years]      = self.ngpl_production[sim_years].mul(self.proplus_production[nam.natural_gasoline_fraction], axis=0)

        pass


    def debug_offshore(self):
        """Produce debug outputs for Offshore Submodule.

        Returns
        -------
        None

        """
        #Debug fields
        self.fields_selected.to_csv(self.output_path + 'projects_debug\\' + 'hsm_off_fields_selected_' +  str(self.rest_curcalyr) + '.csv')
        self.fields.to_csv(self.fields.to_csv(self.output_path + 'projects_debug\\' + 'hsm_off_fields_' + str(self.rest_curcalyr) + '.csv'))

        #Debug cashflow
        self.cash_flow.to_csv(self.output_path + 'cashflow_debug\\','off_cash_flow.csv', self.rest_curcalyr, self.parent.current_iteration, self.parent.current_cycle)

        #Debug results
        self.crude_production.to_csv(self.output_path + 'module_results_debug//' + 'hsm_off_crude_.csv')
        self.natgas_production.to_csv(self.output_path + 'module_results_debug//' + 'hsm_off_natgas_.csv')

        #Debug nfws
        temp = self.discovered_field_dist.copy()
        temp.insert(0, nam.year, self.rest_curcalyr)
        temp.insert(0, 'variable', 'discovered_field_dist')

        temp.to_csv(self.output_path + 'module_results_debug//' + 'hsm_off_discovery_dist.csv', mode='a')
        self.cumulative_nfws.to_csv(self.output_path + 'module_results_debug//' + 'hsm_off_cumulative_nfws.csv')


        pass


    def write_intermediate_variables(self):
        """
        Write local variables to restart file to be read back in each iteration.

        Returns
        -------
        self.parent.hsm_vars.off_announced_fields : df
            DataFrame of announced projects

        self.parent.hsm_vars.off_discovered_fields : df
            DataFrame of discovered fields

        self.parent.hsm_vars.off_discovered_field_dist : df
            DataFrame of discovered fields distribution across evaluation units

        self.parent.hsm_vars.off_undiscovered_fields : df
            DataFrame of number of undiscovered fields by evaluation unit

        self.parent.hsm_vars.off_cumulative_nfws : df
            DataFrame of cumulative new field wildcats by evaluation unit

        self.parent.hsm_vars.off_fields : df
            Main DataFrame of fields with field-level attributes

        self.parent.hsm_vars.off_fields_selected : df
            DataFrame of fields that have been selected to produce

        self.parent.hsm_vars.off_total_fields : df
            DataFrame of total fields by evaluation unit and field size class

        self.parent.hsm_vars.off_crude_production : df
            DataFrame of crude production by field

        self.parent.hsm_vars.off_natgas_production : df
            DataFrame of onshore natural gas production

        self.parent.hsm_vars.off_nagas_production : df
            DataFrame of non-associated natural gas production by field

        self.parent.hsm_vars.off_adgas_production : df
            DataFrame of associated dissolved natural gas production by field

        self.parent.hsm_vars.off_wells : df
            DataFrame of wells by field

        self.parent.hsm_vars.off_exploratory_wells : df
            DataFrame of exploratory wells only by field

        self.parent.hsm_vars.off_drill_rig_constraint : df
            DataFrame of rig constaints by rig type

        self.field_crude_production : df
            DataFrame of crude production by developing field

        self.field_natgas_production : df
            DataFrame of natural gas production by developing field
        """
        self.parent.hsm_vars.off_reg_crude_price            = self.off_reg_crude_price.copy()
        self.parent.hsm_vars.off_reg_natgas_price           = self.off_reg_natgas_price.copy()
        self.parent.hsm_vars.off_announced_fields           = self.announced_fields.copy()
        self.parent.hsm_vars.off_discovered_fields          = self.discovered_fields.copy()
        self.parent.hsm_vars.off_discovered_field_dist      = self.discovered_field_dist.copy()
        self.parent.hsm_vars.off_undiscovered_fields        = self.undiscovered_fields.copy()
        self.parent.hsm_vars.off_cumulative_nfws            = self.cumulative_nfws
        self.parent.hsm_vars.off_fields                     = self.fields.copy()
        self.parent.hsm_vars.off_fields_selected            = self.fields_selected.copy()
        self.parent.hsm_vars.off_total_fields               = self.total_fields.copy()
        self.parent.hsm_vars.off_crude_production           = self.crude_production.copy()
        self.parent.hsm_vars.off_natgas_production          = self.natgas_production.copy()
        self.parent.hsm_vars.off_nagas_production           = self.nagas_production.copy()
        self.parent.hsm_vars.off_adgas_production           = self.adgas_production.copy()
        self.parent.hsm_vars.off_wells                      = self.wells.copy()
        self.parent.hsm_vars.off_exploratory_wells          = self.exploratory_wells.copy()
        self.parent.hsm_vars.off_drill_rig_constraint       = self.drill_rig_constraint.copy()
        self.parent.hsm_vars.off_field_crude_production     = self.field_crude_production.copy()
        self.parent.hsm_vars.off_field_natgas_production    = self.field_natgas_production.copy()


        #Debug
        if self.parent.hsm_var_debug_switch == True:
            self.announced_fields.to_csv(self.parent.hsm_var_output_path + 'hsm_off_announced_fields.csv')
            self.discovered_fields.to_csv(self.parent.hsm_var_output_path + 'hsm_off_discovered_fields.csv')
            self.discovered_field_dist.to_csv(self.parent.hsm_var_output_path + 'hsm_off_discovered_field_dist.csv')
            self.undiscovered_fields.to_csv(self.parent.hsm_var_output_path + 'hsm_off_undiscovered_fields.csv')
            self.fields.to_csv(self.parent.hsm_var_output_path + 'hsm_off_fields.csv')
            self.fields_selected.to_csv(self.parent.hsm_var_output_path + 'hsm_off_fields_selected.csv')
            self.total_fields.to_csv(self.parent.hsm_var_output_path + 'hsm_off_total_fields.csv')
            self.cumulative_nfws.to_csv(self.parent.hsm_var_output_path + 'self.cumulative_nfws')
            self.crude_production.to_csv(self.parent.hsm_var_output_path + 'hsm_off_crude_production.csv')
            self.natgas_production.to_csv(self.parent.hsm_var_output_path + 'hsm_off_natgas_production.csv')
            self.nagas_production.to_csv(self.parent.hsm_var_output_path + 'hsm_off_nagas_production.csv')
            self.adgas_production.to_csv(self.parent.hsm_var_output_path + 'hsm_off_adgas_production.csv')
            self.exploratory_wells.to_csv(self.parent.hsm_var_output_path + 'hsm_off_exploratory_wells.csv')
            self.wells.to_csv(self.parent.hsm_var_output_path + 'hsm_off_wells.csv')
            self.drill_rig_constraint.to_csv(self.parent.hsm_var_output_path + 'hsm_off_drill_rig_constraint.csv')
            self.field_crude_production.to_csv(self.parent.hsm_var_output_path + 'hsm_off_field_crude_production.csv')
            self.field_natgas_production.to_csv(self.parent.hsm_var_output_path + 'hsm_off_field_natgas_production.csv')

    def report_results_unf(self):
        """Report results to restart variables.

        Returns
        -------
        self.restart.ogsmout_ogprdoff : df
            Lower 48 offshore production split into Federal/State categories

        self.restart.ogsmout_ogqcrrep : df
            Crude oil production by oil category

        self.restart.ogsmout_ogcrdprd : df
            Crude oil production by HSM region and crude type

        self.restart.pmmout_rfqtdcrd : df
            Total crude production by HSM region

        self.restart.pmmout_rfqdcrd : df
            Total crude oil production by HSM region (not including EOR)

        self.restart.ogsmout_ogcoprd : df
            Crude oil production by lower 48 region

        self.restart.ogsmout_ogcruderef : df
            Crude oil production by LFMM crude oil type and region

        self.restart.ogsmout_ogngprd : df
            Natural Gas production by natural gas category

        self.restart.ogsmout_ogqngrep : df
            Natural gas production by natural gas type

        self.restart.ogsmout_ogdngprd : df
            Dry natural gas production by state/district and gas type

        self.restart.ogsmout_ogenagprd : df
            Natural gas expected production by natural gas type and HSM district

        self.restart.ogsmout_ogrnagprd : df
            Natural gas realized production by natural gas type and HSM district

        self.restart.ogsmout_ogadgprd : df
            Natural gas associated dissolved production by oil type and HSM district

        self.restart.ogsmout_ogprdad : df
            Natural gas associated dissolved production by HSM region

        self.restart.ogsmout_ogprdadof : df
            Offshore AD natural gas production (BCF)

        self.restart.ogsmout_ogcoprdgom : df
            Crude oil production Gulf of Mexico

        self.restart.ogsmout_ogngprdgom : df
            Natural gas production Gulf of Mexico

        self.restart.ogsmout_ogcowhp : df
            Crude oil wellhead price by HSM region

        self.restart.ogsmout_ogngwhp : df
            Natural gas wellhead price by HSM region

        self.restart.ogsmout_ogngplprd : df
            NGPL production by HSM district

        self.restart.ogsmout_ogngplet : df
            Ethane production by HSM district

        self.restart.ogsmout_ogngplpr : df
            Propane production by HSM district

        self.restart.ogsmout_ogngplbu : df
            Butane production by HSM district

        self.restart.ogsmout_ogngplis : df
            Isobutane production by HSM district

        self.restart.ogsmout_ogngplpp : df
            Pentanes production by HSM district

        self.restart.ogsmout_ognowell : df
            Total completed wells

        self.restart.ogsmout_ogoilprd : df
            Crude oil production by oil type and HSM district
        """
        ###Get fed numbers for reporting to restart
        fed_num_df = pd.DataFrame({nam.federal_land: [nam.federal, nam.not_federal],
                                       nam.federal_land_number: [2, 1]})

        if self.rest_curcalyr >= self.parent.steo_years[0]:
            ###Crude production
            temp = self.crude_production.copy()
            temp = pd.merge(temp, self.mapping[[nam.region_number, nam.planning_area, nam.offshore_region_number]],
                            left_on=nam.evaluation_unit, right_index=True,
                            how='left')

            temp = pd.merge(temp,
                            fed_num_df,
                            left_on=nam.federal_land,
                            right_on=nam.federal_land,
                            how='left')

            temp = temp.groupby([nam.offshore_region_number, nam.federal_land_number]).sum()
            temp = temp.drop(2, level=1, axis=0)
            temp = temp.droplevel(1)
            temp[nam.oil_or_gas] = 1
            temp = temp.reset_index().set_index([nam.offshore_region_number, nam.oil_or_gas])
            temp = temp[[self.rest_curcalyr]].stack() * 365 / 1000000
            self.restart.ogsmout_ogprdoff['value'].update(temp)


            ###Crude production by oil category
            temp = (self.crude_production.copy()[self.rest_curcalyr].sum()) /1000000
            self.restart.ogsmout_ogqcrrep.at[(3,int(self.rest_curcalyr)), 'value'] = temp


            ###Crude production by HSM region and LFMM fuel type
            temp = self.crude_production.copy()[[int(self.rest_curcalyr), nam.region_number]]
            temp = temp.groupby([nam.region_number]).sum()
            #Write to restart
            try:
                self.restart.ogsmout_ogcrdprd.at[(8, 3, int(self.rest_curcalyr)), nam.value] = temp.at[8, int(self.rest_curcalyr)] / 1000000  #! ATLANTIC medium medium sour
            except:
                pass
            self.restart.ogsmout_ogcrdprd.at[(9, 3, int(self.rest_curcalyr)), nam.value] = temp.at[9, int(self.rest_curcalyr)] / 1000000  #! GOM medium medium sour
            self.restart.ogsmout_ogcrdprd.at[(10, 7, int(self.rest_curcalyr)), nam.value] = temp.at[10, int(self.rest_curcalyr)] / 1000000 #! california


            ###Domestic crude oil production by region for LFMM (including EOR)
            temp = self.crude_production.copy()[[int(self.rest_curcalyr), nam.region_number]]
            temp = temp.groupby(nam.region_number).sum()
            self.restart.pmmout_rfqtdcrd.at[(8, int(self.rest_curcalyr)), 'value'] = 0 #Set to 0 because currently no production in the Atlantic
            self.restart.pmmout_rfqtdcrd.at[(9, int(self.rest_curcalyr)), 'value'] = temp.at[(9, int(self.rest_curcalyr))] / 1000000 / 365
            self.restart.pmmout_rfqtdcrd.at[(10, int(self.rest_curcalyr)), 'value'] = temp.at[(10, int(self.rest_curcalyr))] / 1000000 / 365



            ###Domestic crude oil production by region for LFMM (not including EOR)
            temp = self.crude_production.copy()[[int(self.rest_curcalyr), nam.region_number]]
            temp = temp.groupby(nam.region_number).sum()
            self.restart.pmmout_rfqdcrd.at[(8, int(self.rest_curcalyr)), 'value'] = 0 #Set to 0 because currently no production in the Atlantic
            self.restart.pmmout_rfqdcrd.at[(9, int(self.rest_curcalyr)), 'value'] = temp.at[(9, int(self.rest_curcalyr))] / 1000000 / 365
            self.restart.pmmout_rfqdcrd.at[(10, int(self.rest_curcalyr)), 'value'] = temp.at[(10, int(self.rest_curcalyr))] / 1000000 / 365


            ###Crude production by HSM Region (offset from PMMOUT regions since they don't line up with OGSM regions)
            self.restart.ogsmout_ogcoprd.at[(8, int(self.rest_curcalyr)), 'value'] = self.restart.pmmout_rfqtdcrd.at[(9, int(self.rest_curcalyr)), 'value']
            self.restart.ogsmout_ogcoprd.at[(9, int(self.rest_curcalyr)), 'value'] = self.restart.pmmout_rfqtdcrd.at[(10, int(self.rest_curcalyr)), 'value']
            self.restart.ogsmout_ogcoprd.at[(10, int(self.rest_curcalyr)), 'value'] = self.restart.pmmout_rfqtdcrd.at[(8, int(self.rest_curcalyr)), 'value']


            ###Domestic oil crude production by LFMM padd region
            self.restart.ogsmout_ogcruderef.at[(1, 3, int(self.rest_curcalyr)), 'value'] = self.restart.ogsmout_ogcruderef.at[(1, 3, int(self.rest_curcalyr)), 'value'] + \
                                                                                      (self.restart.pmmout_rfqtdcrd.at[(8, int(self.rest_curcalyr)), 'value'] * 365)
            self.restart.ogsmout_ogcruderef.at[(4, 3, int(self.rest_curcalyr)), 'value'] = self.restart.ogsmout_ogcruderef.at[(4, 3, int(self.rest_curcalyr)), 'value'] + \
                                                                                      (self.restart.pmmout_rfqtdcrd.at[(9, int(self.rest_curcalyr)), 'value'] * 365)
            self.restart.ogsmout_ogcruderef.at[(7, 7, int(self.rest_curcalyr)), 'value'] = self.restart.ogsmout_ogcruderef.at[(7, 7, int(self.rest_curcalyr)), 'value'] +\
                                                                                      (self.restart.pmmout_rfqtdcrd.at[(10, int(self.rest_curcalyr)), 'value'] * 365)


            ### Crude Production Federal/Non-Federal
            # Federal Land
            temp = self.crude_production.copy()[[int(self.rest_curcalyr), nam.region_number, nam.federal_land]]
            temp = temp.loc[temp[nam.federal_land] == 'federal'].copy()
            temp = temp.drop([nam.federal_land], axis = 1)
            temp = temp.groupby([nam.region_number]).sum()

            #Write to restart
            try:
                self.restart.ogsmout_ogcoprd_fed.at[(8, int(self.rest_curcalyr)), nam.value] = temp.at[8, int(self.rest_curcalyr)] / 1000000 / 365
            except:
                pass
            self.restart.ogsmout_ogcoprd_fed.at[(9, int(self.rest_curcalyr)), nam.value] = temp.at[9, int(self.rest_curcalyr)] / 1000000  / 365
            self.restart.ogsmout_ogcoprd_fed.at[(10, int(self.rest_curcalyr)), nam.value] = temp.at[10, int(self.rest_curcalyr)] / 1000000  / 365

            # Non-Federal Land
            temp = self.crude_production.copy()[[int(self.rest_curcalyr), nam.region_number, nam.federal_land]]
            temp = temp.loc[temp[nam.federal_land] == 'non_federal'].copy()
            temp = temp.drop([nam.federal_land], axis = 1)
            temp = temp.groupby([nam.region_number]).sum()

            # Write to restart
            try:
                self.restart.ogsmout_ogcoprd_nonfed.at[(8, int(self.rest_curcalyr)), nam.value] = temp.at[8, int(self.rest_curcalyr)] / 1000000 / 365 # ! ATLANTIC medium medium sour
            except:
                pass
            self.restart.ogsmout_ogcoprd_nonfed.at[(9, int(self.rest_curcalyr)), nam.value] = temp.at[9, int(self.rest_curcalyr)] / 1000000 / 365
            self.restart.ogsmout_ogcoprd_nonfed.at[(10, int(self.rest_curcalyr)), nam.value] = temp.at[10, int(self.rest_curcalyr)] / 1000000 / 365


            ###Natural gas production
            temp = self.natgas_production.copy()
            temp = pd.merge(temp, self.mapping[[nam.region_number, nam.planning_area, nam.offshore_region_number]],
                            left_on=nam.evaluation_unit, right_index=True,
                            how='left')

            temp = pd.merge(temp,
                            fed_num_df,
                            left_on=nam.federal_land,
                            right_on=nam.federal_land,
                            how='left')

            temp = temp.groupby([nam.offshore_region_number, nam.federal_land_number]).sum()
            temp = temp.drop(2, level=1, axis=0)
            temp[nam.oil_or_gas] = 2
            temp = temp.reset_index().set_index([nam.offshore_region_number, nam.oil_or_gas])
            temp = temp[[int(self.rest_curcalyr)]].stack()/1000000
            self.restart.ogsmout_ogprdoff['value'].update(temp)


            ###Natural Gas production by region
            temp = self.natgas_production[[int(self.rest_curcalyr), nam.region_number]].copy()
            temp = temp.groupby([nam.region_number]).sum()
            temp.columns = [nam.value]

            self.restart.ogsmout_ogngprd.at[(8, int(self.rest_curcalyr)), nam.value] = temp.at[9, nam.value] / 1000000
            self.restart.ogsmout_ogngprd.at[(9, int(self.rest_curcalyr)), nam.value] = temp.at[10, nam.value] / 1000000
            self.restart.ogsmout_ogngprd.at[(10, int(self.rest_curcalyr)), nam.value] = 0


            ###Natural gas production by gas category
            temp = self.nagas_production.copy()[self.rest_curcalyr].sum()
            self.restart.ogsmout_ogqngrep.loc[(6, int(self.rest_curcalyr)), 'value'] = temp/1000000
            temp = self.adgas_production.copy()[self.rest_curcalyr].sum()
            self.restart.ogsmout_ogqngrep.loc[(7, int(self.rest_curcalyr)), 'value'] = temp/1000000


            ###Dry natural gas production by state and district type
            temp = self.natgas_production[[int(self.rest_curcalyr), nam.district_number]].copy()
            temp[int(self.rest_curcalyr)] = temp[int(self.rest_curcalyr)] / 1000000
            temp[nam.gas_type_number] = 1
            temp = temp.groupby([nam.district_number, nam.gas_type_number]).sum()
            self.restart.ogsmout_ogdngprd['value'].update(temp.stack())


            ###Non-associated natural gas production by state and district type
            temp = self.nagas_production[[int(self.rest_curcalyr), nam.district_number]].copy()
            temp[int(self.rest_curcalyr)] = temp[int(self.rest_curcalyr)] / 1000000
            temp[nam.gas_type_number] = 1
            temp = temp.groupby([nam.district_number, nam.gas_type_number]).sum()
            self.restart.ogsmout_ogenagprd['value'].update(temp.stack())
            #self.restart.ogsmout_ogrnagprd['value'].update(temp.stack())


            ###AD natural gas production by region
            temp = self.adgas_production[[int(self.rest_curcalyr), nam.district_number]].copy()
            temp[int(self.rest_curcalyr)] = temp[int(self.rest_curcalyr)] / 1000000
            temp = temp.groupby([nam.district_number]).sum()
            temp[nam.well_type] = 1
            temp[nam.year] = self.rest_curcalyr
            temp = temp.set_index([nam.well_type, nam.year], append = True)
            temp.index.names = self.restart.ogsmout_ogadgprd.index.names
            temp.columns = [nam.value]
            self.restart.ogsmout_ogadgprd.update(temp)


            ###AD natural gas production by district
            temp = self.adgas_production[[int(self.rest_curcalyr), nam.region_number]].copy()
            temp[int(self.rest_curcalyr)] = temp[int(self.rest_curcalyr)] / 1000000
            temp = temp.groupby([nam.region_number]).sum()
            self.restart.ogsmout_ogprdad['value'].update(temp.stack())


            ###AD natural gas production for offshore regions
            temp.rename(index={8:1,9:2,10:3})
            self.restart.ogsmout_ogprdadof['value'].update(temp.stack())


            ### Natural Gas Production Federal/Non-Federal
            # Federal Land
            temp = self.natgas_production.copy()[[int(self.rest_curcalyr), nam.region_number, nam.federal_land]]
            temp = temp.loc[temp[nam.federal_land] == 'federal'].copy()
            temp = temp.drop([nam.federal_land], axis = 1)
            temp = temp.groupby([nam.region_number]).sum()

            # Write to restart
            try:
                self.restart.ogsmout_ogngprd_fed.at[(8, int(self.rest_curcalyr)), nam.value] = temp.at[8, int(self.rest_curcalyr)] / 1000000000
            except:
                pass
            self.restart.ogsmout_ogngprd_fed.at[(9, int(self.rest_curcalyr)), nam.value] = temp.at[9, int(self.rest_curcalyr)] / 1000000000
            self.restart.ogsmout_ogngprd_fed.at[(10, int(self.rest_curcalyr)), nam.value] = temp.at[10, int(self.rest_curcalyr)] / 1000000000

            # Non-Federal Land
            temp = self.natgas_production.copy()[[int(self.rest_curcalyr), nam.region_number, nam.federal_land]]
            temp = temp.loc[temp[nam.federal_land] == 'non_federal'].copy()
            temp = temp.drop([nam.federal_land], axis = 1)
            temp = temp.groupby([nam.region_number]).sum()

            # Write to restart
            try:
                self.restart.ogsmout_ogngprd_nonfed.at[(8, int(self.rest_curcalyr)), nam.value] = temp.at[8, int(self.rest_curcalyr)] / 1000000000  # ! ATLANTIC medium medium sour
            except:
                pass
            self.restart.ogsmout_ogngprd_nonfed.at[(9, int(self.rest_curcalyr)), nam.value] = temp.at[9, int(self.rest_curcalyr)] / 1000000000
            self.restart.ogsmout_ogngprd_nonfed.at[(10, int(self.rest_curcalyr)), nam.value] = temp.at[10, int(self.rest_curcalyr)] / 1000000000


            ###State-level Oil and Natural Gas Production
            #Pacific and Atlantic
            temp_oil = self.crude_production[[int(self.rest_curcalyr), nam.region_number, nam.federal_land]].copy()
            temp_gas = self.natgas_production[[int(self.rest_curcalyr), nam.region_number, nam.federal_land]].copy()

            #Filter for federal land
            temp_oil = temp_oil.loc[temp_oil[nam.federal_land] == 'non_federal']
            temp_gas = temp_gas.loc[temp_gas[nam.federal_land] == 'non_federal']

            #groupby region
            temp_oil = temp_oil.groupby([nam.region_number]).sum()
            temp_gas = temp_gas.groupby([nam.region_number]).sum()

            #Atlantic Prod (try/except because no production in the atlantic currently)
            try:
                self.restart.ogsmout_ogprdoff.at[(1,1,int(self.rest_curcalyr)), nam.value] = temp_oil.at[8, int(self.rest_curcalyr)] / 1000000
                self.restart.ogsmout_ogprdoff.at[(1,2,int(self.rest_curcalyr)), nam.value] = temp_gas.at[8, int(self.rest_curcalyr)] / 1000000
            except:
                pass

            #Pacific Prod
            self.restart.ogsmout_ogprdoff.at[(2,1,int(self.rest_curcalyr)), nam.value] = temp_oil.at[10, int(self.rest_curcalyr)] / 1000000
            self.restart.ogsmout_ogprdoff.at[(2,2,int(self.rest_curcalyr)), nam.value] = temp_gas.at[10, int(self.rest_curcalyr)] / 1000000

            #Split Gulf Production into regions based on drill depth
            temp_oil = self.crude_production[[int(self.rest_curcalyr), nam.district_number, nam.water_depth_ft, nam.federal_land]]
            temp_gas = self.natgas_production[[int(self.rest_curcalyr), nam.district_number, nam.water_depth_ft, nam.federal_land]]
            temp_oil = temp_oil[temp_oil[nam.district_number].isin([80, 81])]
            temp_gas = temp_gas[temp_gas[nam.district_number].isin([80, 81])]

            #Filter for federal land
            temp_oil = temp_oil.loc[temp_oil[nam.federal_land] == 'non_federal']
            temp_gas = temp_gas.loc[temp_gas[nam.federal_land] == 'non_federal']

            #Gulf water depth
            temp_oil_shallow = temp_oil.loc[temp_oil[nam.water_depth_ft] < 400].copy()
            temp_oil_deep = temp_oil.loc[temp_oil[nam.water_depth_ft] >= 400].copy()
            temp_gas_shallow = temp_gas.loc[temp_gas[nam.water_depth_ft] < 400].copy()
            temp_gas_deep = temp_gas.loc[temp_gas[nam.water_depth_ft] >= 400].copy()

            #Sum by gulf water depth
            temp_oil_shallow = temp_oil_shallow[int(self.rest_curcalyr)].sum() / 1000000
            temp_oil_deep = temp_oil_deep[int(self.rest_curcalyr)].sum() / 1000000
            temp_gas_shallow = temp_gas_shallow[int(self.rest_curcalyr)].sum() / 1000000
            temp_gas_deep = temp_gas_deep[int(self.rest_curcalyr)].sum() / 1000000

            #Write to Regions
            self.restart.ogsmout_ogprdoff.at[(3,1,int(self.rest_curcalyr)), nam.value] = temp_oil_shallow
            self.restart.ogsmout_ogprdoff.at[(3,2,int(self.rest_curcalyr)), nam.value] = temp_gas_shallow
            self.restart.ogsmout_ogprdoff.at[(4,1,int(self.rest_curcalyr)), nam.value] = temp_oil_deep
            self.restart.ogsmout_ogprdoff.at[(4,2,int(self.rest_curcalyr)), nam.value] = temp_gas_deep

            #Eastern Gulf
            temp_oil = self.crude_production[[int(self.rest_curcalyr), nam.district_number, nam.federal_land]].copy()
            temp_gas = self.natgas_production[[int(self.rest_curcalyr), nam.district_number, nam.federal_land]].copy()

            #Filter for federal land
            temp_oil = temp_oil.loc[temp_oil[nam.federal_land] == 'non_federal']
            temp_gas = temp_gas.loc[temp_gas[nam.federal_land] == 'non_federal']

            #Groupby district
            temp_oil = temp_oil.groupby([nam.district_number]).sum()
            temp_gas = temp_gas.groupby([nam.district_number]).sum()

            #Write to Eastern Gulf District (try/except because no production in EGOM currently)
            try:
                self.restart.ogsmout_ogprdoff.at[(5, 1, int(self.rest_curcalyr)), nam.value] = temp_oil.at[81, int(self.rest_curcalyr)]/1000
                self.restart.ogsmout_ogprdoff.at[(5, 2, int(self.rest_curcalyr)), nam.value] = 0
            except:
                pass


            ###Crude production for Gulf of Mexico
            #Split Gulf Production into regions based on drill depth
            temp_oil = self.crude_production[[int(self.rest_curcalyr), nam.district_number, nam.water_depth_ft]]
            temp_oil = temp_oil[temp_oil[nam.district_number].isin([80, 81])]

            #Gulf water depth
            temp_oil_shallow = temp_oil.loc[temp_oil[nam.water_depth_ft] < 400].copy()
            temp_oil_deep = temp_oil.loc[temp_oil[nam.water_depth_ft] >= 400].copy()

            #Sum by gulf water depth
            temp_oil_shallow = temp_oil_shallow[int(self.rest_curcalyr)].sum() / 1000000 / 365
            temp_oil_deep = temp_oil_deep[int(self.rest_curcalyr)].sum() / 1000000 / 365

            #Write to Regions
            self.restart.ogsmout_ogcoprdgom.at[(1,int(self.rest_curcalyr)), nam.value] = temp_oil_shallow
            self.restart.ogsmout_ogcoprdgom.at[(2,int(self.rest_curcalyr)), nam.value] = temp_oil_deep


            ###Natural gas production for Gulf of Mexico
            #Split Gulf Production into regions based on drill depth
            temp_gas = self.nagas_production[[int(self.rest_curcalyr), nam.district_number, nam.water_depth_ft]]
            temp_gas = temp_gas[temp_gas[nam.district_number].isin([80, 81])]


            #Gulf water depth
            temp_gas_shallow = temp_gas.loc[temp_gas[nam.water_depth_ft] < 400].copy()
            temp_gas_deep = temp_gas.loc[temp_gas[nam.water_depth_ft] >= 400].copy()

            #Sum by gulf water depth
            temp_gas_shallow = temp_gas_shallow[int(self.rest_curcalyr)].sum() / 1000000 / 365
            temp_gas_deep = temp_gas_deep[int(self.rest_curcalyr)].sum() / 1000000 / 365

            #Write to Regions
            self.restart.ogsmout_ogngprdgom.at[(1,int(self.rest_curcalyr)), nam.value] = temp_gas_shallow
            self.restart.ogsmout_ogngprdgom.at[(2,int(self.rest_curcalyr)), nam.value] = temp_gas_deep


            ###Crude wellhead price
            self.restart.ogsmout_ogcowhp.at[(8, int(self.rest_curcalyr)), nam.value] = self.parent.rest_dcrdwhp.at[9, int(self.rest_curcalyr)].copy()
            self.restart.ogsmout_ogcowhp.at[(9, int(self.rest_curcalyr)), nam.value] = self.parent.rest_dcrdwhp.at[10, int(self.rest_curcalyr)].copy()
            self.restart.ogsmout_ogcowhp.at[(10, int(self.rest_curcalyr)), nam.value] = self.parent.rest_dcrdwhp.at[8, int(self.rest_curcalyr)].copy()


            ###Natgas wellhead price
            self.restart.ogsmout_ogngwhp.at[(8, int(self.rest_curcalyr)), nam.value] = self.parent.reg_natgas_price.at[9, int(self.rest_curcalyr)]
            self.restart.ogsmout_ogngwhp.at[(9, int(self.rest_curcalyr)), nam.value] = self.parent.reg_natgas_price.at[10, int(self.rest_curcalyr)]
            self.restart.ogsmout_ogngwhp.at[(10, int(self.rest_curcalyr)), nam.value] = self.parent.reg_natgas_price.at[8, int(self.rest_curcalyr)]
            self.restart.ogsmout_ogngwhp.at[(11, int(self.rest_curcalyr)), nam.value] = self.parent.rest_ogpngwhp.at[int(self.rest_curcalyr), nam.value]


            ###NGPLs
            #Write to restart for ogsmout_ogngplbu. transform to OGDIST,MNUMYR
            temp = self.butane_production.copy()[[int(self.rest_curcalyr), nam.district_number]].groupby(
                [nam.district_number]).sum()
            temp = temp.stack()/(365 * 1000000)
            self.restart.ogsmout_ogngplbu['value'].update(temp)

            #Write to restart for self.ogsmout_ogngplet
            temp = self.ethane_production.copy()[[int(self.rest_curcalyr), nam.district_number]].groupby(
                [nam.district_number]).sum()
            temp = temp.stack()/(365 * 1000000)
            self.restart.ogsmout_ogngplet['value'].update(temp)

            #Write to restart for self.ogsmout_ogngplis
            temp = self.isobutane_production.copy()[[int(self.rest_curcalyr), nam.district_number]].groupby(
                [nam.district_number]).sum()
            temp = temp.stack()/(365 * 1000000)
            self.restart.ogsmout_ogngplis['value'].update(temp)

            #Write to restart for self.ogsmout_ogngplpp
            temp = self.proplus_production.copy()[[int(self.rest_curcalyr), nam.district_number]].groupby(
                [nam.district_number]).sum()
            temp = temp.stack()/(365 * 1000000)
            self.restart.ogsmout_ogngplpp['value'].update(temp)

            #Write to restart for self.ogsmout_ogngplpr
            temp = self.propane_production.copy()[[int(self.rest_curcalyr), nam.district_number]].groupby(
                [nam.district_number]).sum()
            temp = temp.stack()/(365 * 1000000)
            self.restart.ogsmout_ogngplpr['value'].update(temp)

            #Write to restart for self.ogsmout_ogngplprd
            temp = self.ngpl_production.copy()[[int(self.rest_curcalyr), nam.district_number]].groupby(
                [nam.district_number]).sum()
            temp = temp.stack()/(365 * 1000000)
            self.restart.ogsmout_ogngplprd['value'].update(temp)

            #Write NGPL Debug
            temp_ngpl_debug = self.ngpl_production.copy()
            temp_ngpl_debug[list(range(self.zero_year, (self.parent.final_aeo_year + 1)))] = temp_ngpl_debug[list(range(self.zero_year, (self.parent.final_aeo_year + 1)))]/(365 * 1000000)
            temp_ngpl_debug = temp_ngpl_debug.astype('str')
            temp_ngpl_debug = temp_ngpl_debug.groupby([nam.district_number, nam.region_number, nam.evaluation_unit, nam.district_number,nam.water_depth_ft]).sum()

            #Add wells to self.ogsmout_ognowell
            temp_wells = self.wells.copy()
            temp_exp_wells = self.exploratory_wells.copy()
            if (not temp.empty) & (self.rest_curcalyr > (self.parent.history_year + 1)):
                self.restart.ogsmout_ognowell.loc[int(self.rest_curcalyr)] += temp_wells[[int(self.rest_curcalyr)]].sum().values[0]
                self.restart.ogsmout_ognowell.loc[int(self.rest_curcalyr)] += temp_exp_wells[[int(self.rest_curcalyr)]].sum().values[0]


            #add to self.ogsmout_ogoilprd
            #all offshore production falls in oiltype - 1 all other oil, add oiltype column and update restart var
            temp = self.crude_production.copy()
            temp['oiltype'] = 1
            temp = temp[[int(self.rest_curcalyr), nam.district_number, 'oiltype']].groupby(
                [nam.district_number, 'oiltype']).sum()
            temp = temp.stack()/(365 * 1000000)
            self.restart.ogsmout_ogoilprd['value'].update(temp)

            #sum across all oil types (1 in this case), and add to restart variable for oil type 5
            temp = self.crude_production.copy()
            temp['oiltype'] = 5
            temp = temp[[int(self.rest_curcalyr), nam.district_number, 'oiltype']].groupby(
                [nam.district_number, 'oiltype']).sum()
            temp = temp.stack() / (365 * 1000000)
            additive = self.restart.ogsmout_ogoilprd.loc[temp.index.get_level_values('district_number'), 5, int(self.rest_curcalyr)].copy()
            additive = additive['value'] + temp
            self.restart.ogsmout_ogoilprd['value'].update(additive)
        else:
            pass

        pass