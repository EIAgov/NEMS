"""Submodule for calculating onshore well production.

Summary
-------
This submodule projects production of crude oil and natural gas from the onshore Lower 48 states in response to price
data received from the LFMM and the NGMM. The module operates as follows:

    1. The module is initialized in the onshore class *__init__* method, with class dataframes and variables being declared here.

    2. The *setup* method is called, which reads in the necessary input files via .csv or .pkl (see **intermediate_var_pickle.py**) format.

    3. The *run* method is called, which kicks off the main model functions.

    4. Year 1 operations are called from the *run* method, these operations are listed below. All other operations are run every model
       year unless otherwise indicated:

        **Setup Operations**

        a. *set_up_projects* - Setup for base project table (i.e. declare project resids, merge projects with process codes, etc.).

        b. *set_up_projects_crude_type* - Assigns missing API numbers to projects tables for merge to LFMM crude types.

        c. *setup_output_tables* - Setup output tables and set baseline crude and natural gas production from producing projects.

        **Legacy Producing Project Operations**

        d. *producing_projects_load_prices* - Get oil and natural gas prices for legacy producing projects using base  international
           prices, so that cash flow can be calculated to determine producing projects economic life
           (i.e. when the project is no longer producing positive net revenue).

        e. *producing_projects_load_costs* - Load operating costs for legacy producing projects so that cash flow can be calculated.

        f. *producing_projects_calculate_drilling_capex* - Applies drilling cost equations for legacy producing projects so that cash flow can be calculated.

        g. *producing_projects_load_cashflow* - Load legacy producing projects into the cashflow.

        h. *run_producing_cash_flow* - Run cash flow for legacy producing projects to determine project economic life.

        i. *shut_down_unprofitable_legacy_production* - Shut down legacy producing project production in year when project net income becomes negative.

        j. *producing_projects_baseline_constraints* - Load baseline producing project rig and footage values for constraints.

        **Continuous, EOR and Undiscovered Projects Setup Operations**

        k. *load_continuous_projects* - Load continuous projects into the master self.projects dataframe for economic analysis.

        l. *load_eor_projects* - Load EOR/ASR projects into the master self.projects dataframe for economic analysis.

        m. *filter_undiscovered_projects* - Removes projects on restricted land from the undiscovered projects list and deletes duplicates.

        **Cost Setup Operations**

        n. *set_base_project_params* - Perform merges and calculations for base parameters commonly used in economic analysis.

        o. *load_costs_setup* - Loads production opex, transportation opex, sga opex and facility capex into projects.

        p. *drill_cost_eqs_assumptions* -  Adjusts self.drill_cost_eq_coefs so that statistically insignificant coefficients are set to 0
           (assumed to be indistinguishable from distribution).

        q. *calculate_drilling_capex_setup* - Applies drilling cost equations derived from historical drilling cost data to projects.

        r. *startup_cost_adjustments* - Reduces costs for projects with historical drilling to ensure they are selected for future drilling.

    5. Calculate crude oil, natural gas, and ngpl prices by region based on price values from LFMM and NGMM in *load_prices*.

    6. Calculate project constraints based on projected brent prices in *calculate_drilling_constraints* and *calculate capital constraint*.

    7. Load in discovered projects based on load order generated from a Monte Carlo simulation. Discovered projects are merged with the main projects
       table for calculations:

            a. *set_undiscovered_drilling_params* - Set base undiscovered drilling assumptions (i.e. max drill rate, available wells, etc.).

            b. *calculate_undiscovered_drilling* - Calculate drilling required to explore and locate an undiscovered project
               (Same methodology as regular drilling equation, but used to determine number of wells needed to be drilled to discover a project).

            c. *load_undiscovered_projects* - Applies rig constraint to load in undiscovered projects.

    8. Calculate production/well in *calculate_production* and apply technology improvement rate in *calculated_prod_tech_improvement*.

    9. Set drilling paramaters for drilling (i.e. max wells, remaining wells, etc.) in *set_drilling params*, then call the *on_next_wells* drilling equation
       from **drilling_equations.py** in *calculate drilling* to determine model year project drilling. Dryholes are also calculated here.

    10. Apply cost technology improvement rates in *apply_cost_tech_rate*.

    11. Calculate remaining annualized costs based on brent_price in *calculate_exploration_costs* and *calculate_ngpl_costs*.

    12. Setup CO2 EOR for integration with CTUS:

        a. *co2_eor_econ* - Calculates costs and tax credits related to CO2 EOR.

        b. *determine_co2_supply_prices* - Calculate Recycled CO2 supply and prices. Pull and sort CO2 supply and price curves from CTUS, LFMM, and EMM.
           Then merge CO2 supply and prices from all sources into a single table.

        c. *calculate_co2_project_costs* - Apply CO2 prices to CO2 required for each CO2 EOR project, using the lowest-cost prices first for all projects,
           and apply to the self.projects dataframe as a single weighted-average cost.

    13. Load and run cash flow in *load_cashflow* and *run_cashflow*. Rank projects by project net present value.

    14. Determine whether EOR projects are eligible to run (i.e. no other EOR project types have been selected for the same resid)
        in *determine_eor_eligibility*.

    15. Apply drilling constraints in *apply_rig_constraints* and *apply_footage_constraints*, so that project drilling does not exceed projected available
        capacity to drill.

    16. Run *select_projects* to mask for all drilling constraints and determine final project drilling. Assign production and well counts to the
        relevant output tables and update iterative calculation tables.

    17. Run *write_intermediate_variables* to store iterative calculation tables for the next model run year. These tables are written out to .pkl files in
        **intermediate_var_pickle.py**.

    18. *report_results_unf* and *adjust_onshore_shale_gas_plays_for_realized_prod* are called from **module_unf.py** to report results to
        debug files and the restart file.


Input Files
___________

    * on_projects_continuous - Continuous projects
    * on_projects_producing_gas - Producing gas projects
    * on_projects_producing_oil - Producing oil projects
    * on_projects_other_eor - Other enhanced oil recovery (eor) projects
    * on_projects_co2_eor - CO2 eor projects
    * on_projects_undiscovered - Undiscovered projects
    * on_process_codes - Process codes
    * on_old_process_code_conv - Convert OGSM proc codes to HSM codes
    * on_rig_constraint_eq - Rig constraint coefficients
    * on_footage_constraint_eq - Footage constraint coefficients
    * on_capital_constraint_eq - Coefficient: natural log of Brent price
    * on_dryhole_rate - Dryhole rate according to region number
    * on_cost_eqs - Cost equations


Model Functions and Class Methods
_________________________________

class Onshore(sub.Submodule) - Onshore submodule for HSM

    * __init__ - Constructor to initialize Onshore submodule
    * setup - Method to set up Onshore Submodule for HSM
    * load_input_variables - Method to read variable data
    * load_input_tables - Method to read table data from input files
    * set_up_projects - Method to prepare tables for processing
    * setup_output_tables - Method to set baseline crude and natural gas production from producing projects
    * load_continuous_projects - Method to load continuous projects for economic analysis
    * load_eor_projects - Method to load EOR projects for economic analysis
    * cost_eq_setup - Method to maps, format, and clean data for cost equations
    * run - Method to run Offshore Submodule for HSM
    * calculate_production - Calculates production from setup table and lists paths of input files
    * load_prices - Method to calculate average prices for Onshore regions; averages prices over the previous years set in averaging_years
    * calculate_drilling - Method to calculate yearly drilling for continuous formations
    * load_costs - Method to load operating expenses (Production and Transport) and assign to projects
    * calculate_drilling_capex - Method to apply capex equations derived from Rystad historical data to calculate project costs
    * co2_eor_econ - Method to shut down ineligible projects
    * calculate_geo_geo_and_lease_aq - Method N/A
    * load_cashflow - Method to load cash flow
    * run_cashflow - Method to run cash flow
    * calculate_constraints - Method to calculate rig, footage, and capital constraints
    * select_projects - Method to apply constraints and rank projects
    * report_results_unf - Method to report results to restart variables


Output Debug Files
__________________

    * on_crude_district_num.csv - Crude Oil Production By District Number
    * on_crude_federal_land.csv - Crude Oil Production Grouped By Federal vs. Nonfederal Land
    * on_crude_fields.csv - Crude Oil Production
    * on_crude_proc_code.csv - Crude Oil Production Grouped By Process Code
    * on_crude_region_num.csv - Crude Oil Production Grouped By Region Number
    * on_natgas_district_num.csv - Natural Gas Production By District Number
    * on_natgas_federal_land.csv - Natural Gas Production Grouped By Federal vs. Nonfederal Land
    * on_natgas_fields.csv - Natural Gas Production
    * on_natgas_proc_code.csv - Natural Gas Production Grouped By Process Code
    * on_natgas_region_num.csv - Natural Gas Production Grouped By Region Number
    * on_wells_district_num.csv - Wells By District Number
    * on_wells_fields.csv - Wells Grouped By Resid, Play, Process Code, Region Number, District Number, Well Type Number, Well Limit
    * on_wells_proc_code.csv - Wells Grouped By Process Code
    * on_wells_region_num.csv - Wells Grouped By Region Number


Output Restart Variables
________________________

**Crude Oil Restart Variables**
    * pmmout_rfqtdcrd - Total crude production by HSM region
    * pmmout_rfqdcrd - Total crude oil production by HSM region (not including EOR)
    * ogsmout_ogqcrrep - Crude oil production by oil category
    * ogsmout_ogcoprd - Crude oil production by lower 48 region
    * ogsmout_ogqshloil - Crude oil production by select tight oil play
    * ogsmout_ogoilprd - Crude oil production by oil type and HSM district
    * ogsmout_ogcrdprd - Crude oil production by HSM region and crude type
    * ogsmout_ogcruderef - Crude oil production by LFMM crude oil type and region
    * ogsmout_ogcrdheat - Heat rate by type of crude oil
    * ogsmout_ogeorprd - CO2 EOR crude oil production


**Natural Gas Restart Variables**
    * ogsmout_ogenagprd - Natural gas expected production by natural gas type and HSM district
    * ogsmout_ogrnagprd - Natural gas realized production by natural gas type and HSM district
    * ogsmout_ogadgprd - Natural gas associated dissolved production by oil type and HSM district
    * ogsmout_ogprdad - Natural gas associated dissolved production by HSM region
    * ogsmout_ogqshlgas - Natural gas production by select natural gas play
    * ogsmout_ogqngrep - Natural gas production by natural gas type
    * ogsmout_ogprdugr - Lower 48 unconventional natural gas production


**Crude Oil and Natural Gas Production Restart Variable**
    * ogsmout_ogregprd - Total crude oil and natural gas production by production type


**Crude Oil Price Restart Variables**
    * ogsmout_ogcowhp - Crude oil wellhead price by HSM region
    * ogsmout_ogpcrwhp -  Crude oil HSM average wellhead price


**Natural Gas Price Restart Variable**
    * ogsmout_ogngwhp - Natural gas wellhead price by HSM region


**Well Restart Variables**
    * ogsmout_ogogwells - Total wells
    * ogsmout_ognowell - Total completed wells
    * ogsmout_ogwellsl48 - Total lower 48 wells
    * ogsmout_ogsrl48 - Lower 48 drilling success rates


**NGPL Production Restart Variables**
    * ogsmout_ogngplprd - NGPL production by HSM district
    * ogsmout_ogngplet - Ethane production by HSM district
    * ogsmout_ogngplpr - Propane production by HSM district
    * ogsmout_ogngplbu - Butane production by HSM district
    * ogsmout_ogngplis - Isobutane production by HSM district
    * ogsmout_ogngplpp - Pentanes production by HSM district


**EOR Restart Variables**
    * ogsmout_ogco2rec - CO2 recycled by HSM region and CO2 type
    * ogsmout_ogco2inj - CO2 injected by HSM region and CO2 type
    * ogsmout_ogco2pur - CO2 purchased by HSM region and CO2 type
    * ogsmout_ogco2avl - CO2 available by HSM region and CO2 type
    * ogsmout_ogco2prc - CO2 price by HSM region and CO2 type


**Technology Improvement Rate Restart Variable**
    * ogsmout_ogtechon - HSM technology improvement rate


Onshore Submodule Class Methods
_______________________________
"""

import pandas as pd
import numpy as np
import names as nam
import submodule as sub
import common as com
import drilling_equations as drill_eq
import warnings
import cash_flow as cf


class Onshore(sub.Submodule):
    """Onshore submodule for HSM.

    Parameters
    ----------
    parent : str
        Module_unf.Module (Pointer to parent module)
    """


    def __init__(self, parent):
        super().__init__(parent, submodule_name='onshore')
        #Input tables
        self.projects_continuous    = pd.DataFrame()  # DataFrame of continuous projects
        self.projects_producing_gas = pd.DataFrame()  # DataFrame of producing gas projects
        self.projects_producing_oil = pd.DataFrame()  # DataFrame of producing oil projects
        self.projects_other_eor     = pd.DataFrame()  # DataFrame of other eor projects
        self.projects_co2_eor       = pd.DataFrame()  # DataFrame of CO2 eor projects
        self.projects_undiscovered  = pd.DataFrame()  # DataFrame of undiscovered projects
        self.projects_discovered    = pd.DataFrame()  # DataFrame of discovered projects that have not yet been loaded into the main projects df
        self.process_codes          = pd.DataFrame()  # DataFrame of process codes
        self.old_process_code_conv  = pd.DataFrame()  # DataFrame to convert OGSM proc codes to HSM codes
        self.discovery_order        = pd.DataFrame()  # DataFrame of discovery order for undiscovered projects
        self.capex_eq_coefs         = pd.DataFrame()  # DataFrame of cost equation coefficients
        self.basin_costs            = pd.DataFrame()  # DataFrame of average basin operating expenses
        self.region_costs           = pd.DataFrame()  # DataFrame of average region operating expenses
        self.rig_constraint_eq      = pd.DataFrame()  # DataFrame of rig constraint coefficients
        self.footage_constraint_eq  = pd.DataFrame()  # DataFrame of footage constraint coefficients
        self.capital_constraint_eq  = pd.DataFrame()  # DataFrame of capital constraint coefficients
        self.constraint_params      = pd.DataFrame()  # DataFrame of constraint parameters and factors
        self.tech_levers            = pd.DataFrame()  # DataFrame of technology levers
        self.gg_costs               = pd.DataFrame()  # DataFrame of Geological, geophysical and lease acquisition costs
        self.ngpl_costs             = pd.DataFrame()  # DataFrame of legacy ngpl costs
        self.eor_other_costs        = pd.DataFrame()  # DataFrame of other EOR cost variables (i.e. water, polymers, etc.)
        self.base_co2_supply_price  = pd.DataFrame()  # DataFrame of co2 supply and cost by region
        self.co2_legacy_costs       = pd.DataFrame()  # DataFrame of legacy co2 costs by source and region for use in cost modifiers
        self.co2_pipe_seg_costs     = pd.DataFrame()  # DataFrame of cost sof co2 transportation costs between different regions

        #Tables for internal calculations
        self.crude_production           = pd.DataFrame()  # DataFrame of crude production for onshore
        self.natgas_production          = pd.DataFrame()  # DataFrame of natgas production for onshore
        self.wells                      = pd.DataFrame()  # DataFrame of wells
        self.dryholes                   = pd.DataFrame()  # DataFrame of dryholes
        self.exploratory_wells          = pd.DataFrame()  # DataFrame of exploratory wells
        self.ngpl_production            = pd.DataFrame()  # DataFrame of ngpl production for onshore
        self.ngpl_ethane_production     = pd.DataFrame()  # DataFrame of ethane production for onshore
        self.ngpl_propane_production    = pd.DataFrame()  # DataFrame of propane production for onshore
        self.ngpl_butane_production     = pd.DataFrame()  # DataFrame of butane production for onshore
        self.ngpl_isobutane_production  = pd.DataFrame()  # DataFrame of isobutane production for onshore
        self.ngpl_proplus_production    = pd.DataFrame()  # DataFrame of pentanes production for onshore
        self.water_production           = pd.DataFrame()  # DataFrame of water production for onshore
        self.project_crude_production   = pd.DataFrame()  # DataFrame of crude production for projects
        self.project_natgas_production  = pd.DataFrame()  # DataFrame of natgas production for projects
        self.project_ngpl_production    = pd.DataFrame()  # DataFrame of ngpl production for projects
        self.project_water_production   = pd.DataFrame()  # DataFrame of water production for projects
        self.producing_footage          = pd.DataFrame()  # DataFrame of producing project footage by region
        self.producing_wells            = pd.DataFrame()  # DataFrame of producing project wells by region
        self.producing_projects         = pd.DataFrame()  # DataFrame of producing projects, concatenated
        self.projects                   = pd.DataFrame()  # DataFrame of all projects, concatenated
        self.project_drilling           = pd.DataFrame()  # DataFrame of drilling schedules by project
        self.project_dryholes           = pd.DataFrame()  # DataFrame of dryholes by project
        self.undiscovered_drilling      = pd.DataFrame()  # DataFrame of drilling required for undiscovered projects
        self.cost_eqs                   = pd.DataFrame()  # DataFrame of cost equations
        self.cost_outliers              = pd.DataFrame()  # DataFrame of cost outliers from cost_eqs
        self.footage_constraint         = pd.DataFrame()  # DataFrame of footage constraints
        self.rig_constraint             = pd.DataFrame()  # DataFrame of rig constraints
        self.co2_price                  = pd.DataFrame()  # DataFrame of co2 price by source and region
        self.project_co2_inj            = pd.DataFrame()  # DataFrane of CO2 injected for CO2 EOR projects
        self.project_co2_recy           = pd.DataFrame()  # DataFrame of recyled CO2 for EOR projects
        self.co2_used                   = pd.DataFrame()  # DataFrame of co2 used by source and region
        self.co2_purchased              = pd.DataFrame()  # DataFrame of co2 purchased by source and region
        self.co2_recycled               = pd.DataFrame()  # DataFrame of co2 recycled by source and region
        self.co2_injected               = pd.DataFrame()  # DataFrame of co2 injected by source and region
        self.co2_cost                   = pd.DataFrame()  # DataFrame of co2 cost by source and region
        self.co2_45q_eor_tax_credit     = pd.DataFrame()  # DataFrame of 45q tax credit values
        self.co2_eor_wells              = pd.DataFrame()  # DataFrame of CO2 EOR wells
        self.co2_net_cost               = pd.DataFrame()  # DataFrame of CO2 NPV


        # Switches
        self.play_map_switch = False # Switch for whether to produce a new play map file for CCATS/HSM relationship

        #Input variables for internal calculations
        self.zero_year              = 0    # Zero year for submodule (equal to AEO year - 1)
        self.final_year             = 0    # Final year for submodule
        self.evaluation_years       = 0    # Number of evaluation years between zero year and final year
        self.averaging_years        = 0    # Number of past years to average price
        self.royalty_rate           = 0.0  # Federal Royalty Rate for onshore drilling
        self.base_oil_prc           = 0.0  # Base oil price for drill schedule benchmarking
        self.base_gas_prc           = 0.0  # Base gas price for drill schedule benchmarking
        self.oil_exp_tang_frac      = 0.0  # Fraction of oil well exploration costs that are tangible
        self.oil_dev_tang_frac      = 0.0  # Fraction of oil well development costs that are tangible
        self.gas_exp_tang_frac      = 0.0  # Fraction of gas well exploration costs that are tangible
        self.gas_dev_tang_frac      = 0.0  # Fraction of gas well development costs that are tangible
        self.kap_tang_frac          = 0.0  # Fraction of well capital costs that are tangible
        self.amor_schedule          = 0.0  # Amortization schedule
        self.deprec_schedule        = 0.0  # Depreciation Schedule
        self.intang_amor_frac       = 0.0  # Fraction of costs to be amortized
        self.abandon_rate           = 0.0  # Well abandonment rate
        self.drill_ramp_up          = 0.0  # Drilling ram-up years
        self.drill_predecline       = 0.0  # Drilling predecline rate
        self.eor_tc_rate            = 0.0  # EOR tax credit rate
        self.eor_tc_phaseout        = 0.0  # EOR tax credit phaseout
        self.co2_trans_cost         = 0.0  # EOR transportation cost/transportation segment
        self.ch4_emission_cost      = []   # Array of CH4 Emission costs by year

        #Calculated Variables for Internal Calculations
        self.exp_const_ratio    = 0.0  # Ratio of drilling constraint capacity assigned to undiscovered production
        self.dev_const_ratio    = 0.0  # Ratio of drilling constraint capacity assigned to developing production

        #Onshore CashFlow object
        self.cash_flow = cf.CashFlow()



    def setup(self, setup_filename):
        """Setup Onshore submodule for HSM.

        Parameters
        ----------
        setup_filename : str
            Path to offshore setup file.

        Returns
        -------
        None
        """
        #Load in Setup File
        super().setup(setup_filename)

        #Run Setup Functions
        self.logger.info('Run Onshore Setup Functions')
        self.load_input_variables()
        self.load_input_tables()

        pass


    def load_input_variables(self):
        """Reads input variable data from self.setup_table (loaded in super().setup(setup_filename)) then adjusts base prices for inflation.

        Returns
        -------
        self.zero_year : int
            First model year

        self.final_year : int
            Final model year

        self.evaluation_years : int
            Number of evaluation years (self.final_year - self.zero_year + 1)

        self.averaging_years : int
            Number of averaging years used to produce crude oil and natural gas prices

        self.royalty_rate : float
            U.S. Federal royalty rate

        self.base_oil_prc : float
            Baseline oil price used to determine drilling responsiveness (i.e. if model year oil price > baseline oil price,
            then drilling is adjusted higher)

        self.base_gas_prc : float
            Baseline natgas price used to determine drilling responsiveness (i.e. if model year natgas price > baseline natgas price,
            then drilling is adjusted higher)

        self.oil_exp_tang_frac : float
            Fraction of oil well exploration costs that are tangible

        self.oil_dev_tang_frac : float
            Fraction of oil well development costs that are tangible

        self.gas_exp_tang_frac : float
            Fraction of gas well exploration costs that are tangible

        self.gas_dev_tang_frac : float
            Fraction of gas well development costs that are tangible

        self.kap_tang_frac : float
            Fraction of well capital costs that are tangible

        self.amor_schedule : str
            Amortization schedule code (see **depreciation_schedules.csv**)

        self.deprec_schedule : str
            Depreciation schedule code (see **depreciation_schedules.csv**)

        self.intang_amor_frac : float
            Fraction of costs to be amortized

        self.abandon_rate : float
            Well abandonment cost rate

        self.drill_ramp_up : int
            Drilling ram-up years

        self.drill_predecline : float
            Fraction of total available wells drilled at which point drilling decline starts (i.e. 70% of available wells)

        self.eor_tc_rate : float
            EOR tax credit rate

        self.eor_tc_phaseout : int
            EOR tax credit phaseout year

        self.co2_trans_cost : float
            CO2 transportation cost

        """
        self.play_map_switch    = str(self.setup_table.at[nam.play_map_switch, nam.filename]).upper() == 'True'.upper()
        self.zero_year          = int(  self.setup_table.at[nam.on_zero_year            , nam.filename])
        self.final_year         = int(  self.setup_table.at[nam.on_final_year           , nam.filename])
        self.evaluation_years   = self.final_year - self.zero_year + 1
        self.averaging_years    = int(  self.setup_table.at[nam.on_averaging_years      , nam.filename])
        self.royalty_rate       = float(self.setup_table.at[nam.on_royalty_rate         , nam.filename])
        self.base_oil_prc       = float(self.setup_table.at[nam.on_base_oil_prc         , nam.filename])
        self.base_gas_prc       = float(self.setup_table.at[nam.on_base_gas_prc         , nam.filename])
        self.oil_exp_tang_frac  = float(self.setup_table.at[nam.on_oil_exp_tang_frac    , nam.filename])
        self.oil_dev_tang_frac  = float(self.setup_table.at[nam.on_oil_dev_tang_frac    , nam.filename])
        self.gas_exp_tang_frac  = float(self.setup_table.at[nam.on_gas_exp_tang_frac    , nam.filename])
        self.gas_dev_tang_frac  = float(self.setup_table.at[nam.on_gas_dev_tang_frac    , nam.filename])
        self.kap_tang_frac      = float(self.setup_table.at[nam.on_kap_tang_frac        , nam.filename])
        self.amor_schedule      = str(self.setup_table.at[nam.on_amor_schedule          , nam.filename])
        self.deprec_schedule    = str(self.setup_table.at[nam.on_deprec_schedule        , nam.filename])
        self.intang_amor_frac   = float(self.setup_table.at[nam.on_intang_amor_frac     , nam.filename])
        self.abandon_rate       = float(self.setup_table.at[nam.on_abandon_rate         , nam.filename])
        self.drill_ramp_up      = float(self.setup_table.at[nam.on_drill_ramp_up        , nam.filename])
        self.drill_predecline   = float(self.setup_table.at[nam.on_drill_predecline     , nam.filename])
        self.eor_tc_rate        = float(self.setup_table.at[nam.on_eor_tc_rate          , nam.filename])
        self.eor_tc_phaseout    = float(self.setup_table.at[nam.on_eor_tc_phaseout      , nam.filename])
        self.co2_trans_cost     = float(self.setup_table.at[nam.on_co2_trans_cost       , nam.filename])
        self.max_co2_prc        = float(self.setup_table.at[nam.on_max_co2_prc          , nam.filename])

        #Adjust base prices for inflation
        self.base_oil_prc       *= com.calculate_inflation(self.parent.rest_mc_jpgdp, self.zero_year)
        self.base_gas_prc       *= com.calculate_inflation(self.parent.rest_mc_jpgdp, self.zero_year)

        pass


    def load_input_tables(self):
        """Reads the following table data from input files:

            * Process Codes
            * Undiscovered projects discovery order
            * Projects by type (i.e. continuous, producing oil, etc.)
            * Cost input
            * Constraint input
            * Select plays list for output tables
            * technology levers
            * CO2 supply and price input

        self.setup_table (loaded in super().setup(setup_filename)) lists paths of input files.

        Returns
        -------
        self.process_codes : df
            Table of process codes which describe projects (i.e. process code 0 = producing vertical oil project,
            process code 1 = producing vertical natural gas projects).

        self.old_process_code_conv : df
            Table that maps legacy OGSM process codes to HSM process codes.

        self.discovery_order : df
            Results of a Monte Carlo simulation to determine discovery order for undiscovered vertical projects in
            the *projects_undiscovered* df

        self.projects_continuous : df
            Table of horizontal projects

        self.projects_producing_gas : df
            Table of legacy producing vertical natural gas projects

        self.projects_producing_oil : df
            Table of legacy producing vertical oil projects

        self.projects_other_eor : df
            Table of secondary production projects that are not CO2 EOR

        self.projects_co2_eor : df
            Table of CO2 EOR projects

        self.projects_undiscovered : df
            Table of undiscovered projects

        self.drill_eq_constraints : df
            Table of drilling equation constraints (i.e. max available wells that can be drilled/year)

        self.drill_cost_eq_coefs : df
            Table of cost equations based on inputs from Rystad

        self.region_costs : df
            Table of average facility, operating, GA and transportation costs by HSM region based on inputs from Rystad

        self.basin_costs : df
            Table of average facility, operating, GA and transportation costs by USGS province based on inputs from Rystad

        self.dryhole_rate : df
            Table of dryhole rate by horizontal projects, undiscovered vertical projects, and discovered vertical projects

        self.ngpl_costs : df
            Table of legacy OGSM NGPL cost equations

        self.rig_constraint_eq : df
            Table of rig constraint equations by HSM region based on data from the DPR (Naser Ameen)

        self.footage_constraint_eq : df
            Table of footage constraint equations by HSM region based on data from the DPR (Naser Ameen)

        self.capital_constraint_eq : df
            Table of a capital constraint equation based on Evaluate Energy dataset (Jeff Barron)

        self.constraint_params : df
            Table containing rig constraint allocation to discovering vertical wells vs. drilling horizontal well

        self.wells_per_rig : df
            Table of wells/rig for constraint equations by HSM region based on data from the DPR (Naser Ameen)

        self.tech_levers : df
            Table of technology improvement rates

        self.eor_other_costs : df
            Table of legacy OGSM Other EOR project costs

        self.base_co2_supply_price : df
            Table of legacy OGSM CO2 EOR supply and price curves

        self.co2_legacy_costs : df
            Table of legacy CO2 EOR costs

        self.co2_pipe_seg_costs : df
            Table of legacy CO2 region-to-region transportation costs in OGSM

        HSM Pickle Files : df
            Collection of iterative intermediate model tables (i.e. tables the model updates and then needs again each year)
            that are input/output in **intermediate_var_pickle.py**
        """
        #Load process codes
        self.process_codes          = super()._load_dataframe(self.onshore_input_path, nam.on_process_codes, index_col=nam.process_code)

        #Load undiscovered projects discovery order
        self.discovery_order        = super()._load_dataframe(self.onshore_input_path, nam.on_discovery_order)

        #Load projects
        if (self.rest_curcalyr == self.zero_year) | (self.parent.integrated_switch == 0): #Only load projects in model year one, after we can just load projects lists from intermediate tables
            self.projects_continuous    = super()._load_dataframe(self.onshore_input_path, nam.on_projects_continuous)
            self.projects_producing_gas = super()._load_dataframe(self.onshore_input_path, nam.on_projects_producing_gas)
            self.projects_producing_oil = super()._load_dataframe(self.onshore_input_path, nam.on_projects_producing_oil)
            self.projects_other_eor     = super()._load_dataframe(self.onshore_input_path, nam.on_projects_other_eor)
            self.projects_co2_eor       = super()._load_dataframe(self.onshore_input_path, nam.on_projects_co2_eor)
            self.projects_undiscovered  = super()._load_dataframe(self.onshore_input_path, nam.on_projects_undiscovered)
        else:
            pass

        #Load drilling eq constraints
        self.drill_eq_constraints   = super()._load_dataframe(self.onshore_input_path, nam.on_drill_eq_constraints)

        #Load other well costs
        self.drill_cost_eq_coefs    = super()._load_dataframe(self.onshore_input_path, nam.on_drill_cost_eqs)
        self.region_costs           = super()._load_dataframe(self.onshore_input_path, nam.on_region_avg_cost)
        self.basin_costs            = super()._load_dataframe(self.onshore_input_path, nam.on_basin_avg_cost)
        self.dryhole_rate           = super()._load_dataframe(self.onshore_input_path, nam.on_dryhole_rate)

        #Load ngpl processing costs
        self.ngpl_costs             = super()._load_dataframe(self.onshore_input_path, nam.on_ngpl_costs)

        #Load constraint coefficients
        self.rig_constraint_eq      = super()._load_dataframe(self.onshore_input_path, nam.on_rig_constraint_eq, index_col= ['coef_name'])
        self.footage_constraint_eq  = super()._load_dataframe(self.onshore_input_path, nam.on_footage_constraint_eq, index_col= ['coef_name'])
        self.capital_constraint_eq  = super()._load_dataframe(self.onshore_input_path, nam.on_capital_constraint_eq, index_col= ['coef_name'])
        self.constraint_params      = super()._load_dataframe(self.onshore_input_path, nam.on_constraint_params, index_col= ['parameter'])
        self.wells_per_rig          = super()._load_dataframe(self.onshore_input_path, nam.on_wells_per_rig)

        #Load technology levers
        self.tech_levers            = super()._load_dataframe(self.onshore_input_path, nam.on_tech_levers, index_col = nam.well_type_number)
        self.tech_levers = self.tech_levers.mul(self.parent.side_case_adj, axis = 1) #Apply supply case adjusmtent for tech rates

        #Load EOR supply, prices and costs
        self.eor_other_costs        = super()._load_dataframe(self.onshore_input_path, nam.on_eor_other_costs, index_col=nam.cost_type)
        self.base_co2_supply_price  = super()._load_dataframe(self.onshore_input_path, nam.on_co2_supply_cost_temp)
        self.co2_legacy_costs       = super()._load_dataframe(self.onshore_input_path, nam.on_legacy_co2_costs)
        self.co2_pipe_seg_costs     = super()._load_dataframe(self.onshore_input_path, nam.on_co2_pipe_seg_costs)

        #Load CH4 Emissions Factors
        self.ch4_emission_factors_basin = super()._load_dataframe(self.onshore_input_path, nam.on_ch4_emission_factors_basin)


        ###Load Intermediate Tables:
        if (self.rest_curcalyr > self.zero_year) & (self.parent.integrated_switch == 1):
            self.projects                   = self.parent.hsm_vars.on_projects.copy()
            self.projects_undiscovered      = self.parent.hsm_vars.on_projects_undiscovered.copy()
            self.projects_discovered        = self.parent.hsm_vars.on_projects_discovered.copy()
            self.crude_production           = self.parent.hsm_vars.on_crude_production.copy()
            self.natgas_production          = self.parent.hsm_vars.on_natgas_production.copy()
            self.ngpl_production            = self.parent.hsm_vars.on_ngpl_production.copy()
            self.ngpl_ethane_production     = self.parent.hsm_vars.on_ngpl_ethane_production.copy()
            self.ngpl_propane_production    = self.parent.hsm_vars.on_ngpl_propane_production.copy()
            self.ngpl_butane_production     = self.parent.hsm_vars.on_ngpl_butane_production.copy()
            self.ngpl_isobutane_production  = self.parent.hsm_vars.on_ngpl_isobutane_production.copy()
            self.ngpl_proplus_production    = self.parent.hsm_vars.on_ngpl_proplus_production.copy()
            self.wells                      = self.parent.hsm_vars.on_wells.copy()
            self.producing_wells            = self.parent.hsm_vars.on_producing_wells.copy()
            self.producing_footage          = self.parent.hsm_vars.on_producing_footage.copy()
            self.dryholes                   = self.parent.hsm_vars.on_dryholes.copy()
            self.exploratory_wells          = self.parent.hsm_vars.on_exploratory_wells.copy()
            self.co2_injected               = self.parent.hsm_vars.on_co2_injected.copy()
            self.co2_recycled               = self.parent.hsm_vars.on_co2_recycled.copy()
            self.co2_eor_wells              = self.parent.hsm_vars.on_co2_eor_wells.copy()
            self.co2_net_cost               = self.parent.hsm_vars.on_co2_net_cost.copy()

        pass


    def run(self):
        """Run Onshore Submodule for HSM.

            1. In zero year setup project tables, run producing projects, and setup project costs
            2. In zero year onward run main model processes


        **Year 1 Setup Methods**

        * self.set_up_projects
        * set_up_projects_crude_type
        * self.setup_output_tables
        * self.load_continuous_projects
        * self.load_eor_projects
        * self.filter_undiscovered_projects
        * self.set_base_project_params


        **Year 1 Producing Project Methods**

        * self.producing_projects_load_prices
        * self.producing_projects_load_costs
        * self.producing_projects_calculate_drilling_capex
        * self.producing_projects_load_cashflow
        * self.run_producing_cash_flow
        * self.shut_down_unprofitable_legacy_production
        * self.producing_projects_baseline_constraints


        **Year 1 Cost Setup Methods**

        * self.load_costs_setup
        * self.drill_cost_eqs_assumptions
        * self.calculate_drilling_capex_setup
        * self.startup_cost_adjustments


        **Annual Model Methods**

        * self.load_prices
        * self.calculate_drilling_constraints
        * self.calculate_capital_constraint
        * self.set_undiscovered_drilling_params
        * self.calculate_undiscovered_drilling
        * self.load_undiscovered_projects
        * self.calculate_production
        * self.calculate_prod_tech_improvement
        * self.set_drilling_params
        * self.calculate_drilling
        * self.apply_cost_tech_rate
        * self.calculate_exploration_costs
        * self.calculate_ngpl_costs
        * self.co2_eor_econ
        * self.determine_co2_supply_prices
        * self.calculate_co2_project_costs
        * self.load_cashflow
        * self.run_cashflow
        * self.determine_eor_eligibility
        * self.apply_rig_constraints
        * self.apply_footage_constraints
        * self.select_projects
        * self.write_intermediate_variables

        Returns
        -------
        None
        """
        super().run()
        self.rest_curcalyr = int(self.rest_curcalyr)

        ###Year 1 Processes
        if self.rest_curcalyr == self.zero_year:
            #Run projects setup
            self.logger.info('Run Projects Setup')
            self.set_up_projects()
            if self.play_map_switch:
                self.logger.info('Setup Play Mapping between CCATS and HSM')
                self.setup_play_map()
            self.logger.info('Run Projects Crude Type Setup')
            self.set_up_projects_crude_type()

            #Run Producing Projects and Year 1 Onshore Developing Project Setup
            self.logger.info('Run Onshore Producing Projects')
            self.setup_output_tables()

            #Load Onshore Producing Projects Attributes
            self.logger.info('Load Onshore Producing Projects Attributes')
            self.producing_projects_load_prices()
            self.producing_projects_load_costs()
            self.producing_projects_load_ch4_costs()
            self.producing_projects_calculate_drilling_capex()

            #Run Onshore Producing Projects Cashflow
            self.logger.info('Run Onshore Producing Projects Cashflow')
            self.producing_projects_load_cashflow()
            self.run_producing_cash_flow()

            #Shut Down Onshore Producing Projects Unprofitable Legacy Production
            self.logger.info('Shut Down Onshore Producing Projects Unprofitable Legacy Production')
            self.shut_down_unprofitable_legacy_production()

            #Set Onshore Producing Projects Constraints
            self.logger.info('Set Onshore Producing Projects Constraints')
            self.producing_projects_baseline_constraints()

            #Load main projects table and setup base params
            self.logger.info('Year 1 Developing, Secondary Production and Undiscovered Projects Setup')
            self.load_continuous_projects()
            self.load_eor_projects()
            self.filter_undiscovered_projects()
            self.set_base_project_params()


        ###Main Cost Processes
        if self.rest_curcalyr <= (self.zero_year + 1): #Calculate in year 1 and 2 to undo startup adjustments for remainder of model
            self.logger.info('Load Average Rystad Costs')
            self.load_costs_setup()
            self.logger.info('Run Onshore Drilling Capex')
            self.drill_cost_eqs_assumptions()
            self.calculate_drilling_capex_setup()

        if self.rest_curcalyr == self.zero_year:
            self.logger.info('Startup Cost Adjustments')
            self.startup_cost_adjustments()


        ###Annual Processes
        if self.rest_curcalyr >= self.zero_year:
            #Load Prices
            self.logger.info('Run Onshore Load Prices')
            self.load_prices()

            #Calculate Constraints
            self.logger.info('Run Onshore Calculate Constraints')
            self.calculate_drilling_constraints()
            self.calculate_capital_constraint()

            #Calculate Undiscovered Drilling
            self.logger.info('Run Onshore Calculate Undiscovered Drilling')
            self.set_undiscovered_drilling_params()
            self.calculate_undiscovered_drilling()
            self.load_undiscovered_projects()

            #Annual Production Setup
            self.logger.info('Run Onshore Annual Production Setup')
            self.calculate_production()
            self.calculate_prod_tech_improvement()

            #Developing Project Drilling
            self.logger.info('Run Onshore Developing Project Drilling')
            self.set_drilling_params()
            self.calculate_drilling()

            #Calculate Costs
            self.logger.info('Apply Cost Tech Rate')
            self.apply_cost_tech_rate()
            self.logger.info('Run Onshore Exploration Costs')
            self.calculate_exploration_costs()
            self.logger.info('Run Onshore NGPL Costs')
            self.calculate_ngpl_costs()
            self.logger.info('Run CH4 Emissions Cost Calculation')
            self.load_ch4_emission_costs()

            #CO2 EOR Setup
            self.logger.info('Run Onshore CO2 EOR Setup')
            self.co2_eor_econ()
            self.determine_co2_supply_prices()
            self.calculate_co2_project_costs()

            #Cash Flow
            self.logger.info('Run Onshore DCF')
            self.load_cashflow()
            self.run_cashflow()

            # Apply Constraints
            self.logger.info('Run Onshore Apply EOR Constraints')
            self.determine_eor_eligibility()
            self.logger.info('Run Onshore Apply Rig Constraints')
            self.apply_rig_constraints()
            self.logger.info('Run Onshore Apply Footage Constraints')
            self.apply_footage_constraints()

            #Select Projects
            self.logger.info('Run Onshore Select Projects')
            self.select_projects()

            # Debug Onshore Submodule
            if self.parent.debug_switch == True:
                self.logger.info('Debug Onshore Submodule')
                self.debug_onshore()

            #Write Local Tables for Integrated Runs
            if ((self.parent.integrated_switch == 1) & (self.parent.param_fcrl == 1)) | ((self.parent.integrated_switch == 1) & (self.parent.param_ncrl == 1)):
                self.logger.info('Write Intermediate Variables')
                self.write_intermediate_variables()

        pass


    ###START OF FUNCTIONS ONLY RUN IN FIRST MODEL YEAR###

    def set_up_projects(self):
        """Prepares tables for processing by:

            * Creating a unique numerical id for each project
            * Merging projects with process codes
            * Creating id columns (i.e. state abbreviation and region numbers)

        Returns
        -------
        self.projects_continuous : df
            Table of horizontal projects

        self.projects_producing_gas : df
            Table of legacy producing vertical natural gas projects

        self.projects_producing_oil : df
            Table of legacy producing vertical oil projects

        self.projects_other_eor : df
            Table of secondary production projects that are not CO2 EOR

        self.projects_co2_eor : df
            Table of CO2 EOR projects

        self.projects_undiscovered : df
            Table of undiscovered projects
        """

         #Make table column
        self.projects_continuous[nam.table]     = nam.continuous
        self.projects_producing_gas[nam.table]  = nam.producing_gas
        self.projects_producing_oil[nam.table]  = nam.producing_oil
        self.projects_other_eor[nam.table]      = nam.other_eor
        self.projects_co2_eor[nam.table]        = nam.co2_eor
        self.projects_undiscovered[nam.table]   = nam.undiscovered

        #Set unique index id for each project
        self.projects_continuous[nam.hsm_index]     = self.projects_continuous.index.copy()
        self.projects_producing_gas[nam.hsm_index]  = self.projects_producing_gas.index.copy() + self.projects_continuous[nam.hsm_index].max()    + 1
        self.projects_producing_oil[nam.hsm_index]  = self.projects_producing_oil.index.copy() + self.projects_producing_gas[nam.hsm_index].max() + 1
        self.projects_other_eor[nam.hsm_index]      = self.projects_other_eor.index.copy()     + self.projects_producing_oil[nam.hsm_index].max() + 1
        self.projects_co2_eor[nam.hsm_index]        = self.projects_co2_eor.index.copy()       + self.projects_other_eor[nam.hsm_index].max()     + 1
        self.projects_undiscovered[nam.hsm_index]   = self.projects_undiscovered.index.copy()  + self.projects_co2_eor[nam.hsm_index].max()       + 1

        #Merge projects with process codes
        self.projects_continuous    = pd.merge(self.projects_continuous, self.process_codes,
                                               left_on=nam.process_code,
                                               right_index=True,
                                               how='left')
        self.projects_producing_gas = pd.merge(self.projects_producing_gas, self.process_codes,
                                               left_on=nam.process_code,
                                               right_index=True,
                                               how='left')
        self.projects_producing_oil = pd.merge(self.projects_producing_oil, self.process_codes,
                                               left_on=nam.process_code,
                                               right_index=True,
                                               how='left')
        self.projects_other_eor     = pd.merge(self.projects_other_eor, self.process_codes,
                                               left_on=nam.process_code,
                                               right_index=True,
                                               how='left')
        self.projects_co2_eor       = pd.merge(self.projects_co2_eor, self.process_codes,
                                               left_on=nam.process_code,
                                               right_index=True,
                                               how='left')
        self.projects_undiscovered  = pd.merge(self.projects_undiscovered, self.process_codes,
                                               left_on=nam.process_code,
                                               right_index=True,
                                               how='left')

        #Get state
        self.projects_continuous[nam.state]    = self.projects_continuous[nam.resid].str[2:4]
        self.projects_producing_gas[nam.state] = self.projects_producing_gas[nam.resid].str[2:4]
        self.projects_producing_oil[nam.state] = self.projects_producing_oil[nam.resid].str[2:4]
        self.projects_other_eor[nam.state]     = self.projects_other_eor[nam.resid].str[2:4]
        self.projects_co2_eor[nam.state]       = self.projects_co2_eor[nam.resid].str[2:4]
        self.projects_undiscovered[nam.state]  = self.projects_undiscovered[nam.resid].str[2:4]

        #Get region number
        self.projects_continuous    = pd.merge(self.projects_continuous, self.parent.mapping[[nam.district_number, nam.region_number]],
                                               on=nam.district_number,
                                               how='left')
        self.projects_producing_gas = pd.merge(self.projects_producing_gas, self.parent.mapping[[nam.district_number, nam.region_number]],
                                               on=nam.district_number,
                                               how='left')
        self.projects_producing_oil = pd.merge(self.projects_producing_oil, self.parent.mapping[[nam.district_number, nam.region_number]],
                                               on=nam.district_number,
                                               how='left')
        self.projects_other_eor     = pd.merge(self.projects_other_eor, self.parent.mapping[[nam.district_number, nam.region_number]],
                                               on=nam.district_number,
                                               how='left')
        self.projects_co2_eor       = pd.merge(self.projects_co2_eor, self.parent.mapping[[nam.district_number, nam.region_number,'census_division','census_region']],
                                               on=nam.district_number,
                                               how='left')
        self.projects_undiscovered  = pd.merge(self.projects_undiscovered, self.parent.mapping[[nam.district_number, nam.region_number]],
                                               on=nam.district_number,
                                               how='left')

        #Get LFMM mapping
        self.projects_continuous    = pd.merge(self.projects_continuous, self.parent.mapping[[nam.district_number, nam.lfmm_region_number, nam.padd]],
                                               on=nam.district_number,
                                               how='left')
        self.projects_producing_gas = pd.merge(self.projects_producing_gas, self.parent.mapping[[nam.district_number, nam.lfmm_region_number, nam.padd]],
                                               on=nam.district_number,
                                               how='left')
        self.projects_producing_oil = pd.merge(self.projects_producing_oil, self.parent.mapping[[nam.district_number, nam.lfmm_region_number, nam.padd]],
                                               on=nam.district_number,
                                               how='left')
        self.projects_other_eor     = pd.merge(self.projects_other_eor, self.parent.mapping[[nam.district_number, nam.lfmm_region_number, nam.padd]],
                                               on=nam.district_number,
                                               how='left')
        self.projects_co2_eor       = pd.merge(self.projects_co2_eor, self.parent.mapping[[nam.district_number, nam.lfmm_region_number, nam.padd]],
                                               on=nam.district_number,
                                               how='left')
        self.projects_undiscovered  = pd.merge(self.projects_undiscovered,
                                               self.parent.mapping[[nam.district_number, nam.lfmm_region_number, nam.padd]],
                                               on=nam.district_number,
                                               how='left')

        #Merge drilling equation constraints
        self.projects_continuous    = pd.merge(self.projects_continuous, self.drill_eq_constraints,
                                               on=nam.process_code,
                                               how='left')
        self.projects_producing_gas = pd.merge(self.projects_producing_gas, self.drill_eq_constraints,
                                               on=nam.process_code,
                                               how='left')
        self.projects_producing_oil = pd.merge(self.projects_producing_oil, self.drill_eq_constraints,
                                               on=nam.process_code,
                                               how='left')
        self.projects_other_eor     = pd.merge(self.projects_other_eor, self.drill_eq_constraints,
                                               on=nam.process_code,
                                               how='left')
        self.projects_co2_eor       = pd.merge(self.projects_co2_eor, self.drill_eq_constraints,
                                               on=nam.process_code,
                                               how='left')
        self.projects_undiscovered  = pd.merge(self.projects_undiscovered, self.drill_eq_constraints,
                                               on=nam.process_code,
                                               how='left')


        #Get production tier (type of production)
        self.projects_continuous[nam.prod_tier]    = self.projects_continuous[nam.resid].str[0:1]
        self.projects_producing_gas[nam.prod_tier] = self.projects_producing_gas[nam.resid].str[0:1]
        self.projects_producing_oil[nam.prod_tier] = self.projects_producing_oil[nam.resid].str[0:1]
        self.projects_other_eor[nam.prod_tier]     = self.projects_other_eor[nam.resid].str[0:1]
        self.projects_co2_eor[nam.prod_tier]       = self.projects_co2_eor[nam.resid].str[0:1]
        self.projects_undiscovered[nam.prod_tier]  = self.projects_undiscovered[nam.resid].str[0:1]

        #Get USGS province number
        self.projects_continuous[nam.usgs_province_num] = self.projects_continuous[nam.resid].str[4:6].astype(int)
        self.projects_producing_gas[nam.usgs_province_num] = self.projects_producing_gas[nam.resid].str[4:6].astype(int)
        self.projects_producing_oil[nam.usgs_province_num] = self.projects_producing_oil[nam.resid].str[4:6].astype(int)
        self.projects_other_eor[nam.usgs_province_num]     = self.projects_other_eor[nam.resid].str[4:6].astype(int)
        self.projects_co2_eor[nam.usgs_province_num]       = self.projects_co2_eor[nam.resid].str[4:6].astype(int)
        self.projects_undiscovered[nam.usgs_province_num]  = self.projects_undiscovered[nam.resid].str[4:6].astype(int)

        #Merge CH4 emissions factors
        self.projects_continuous        = pd.merge(self.projects_continuous,
                                                   self.ch4_emission_factors_basin[[nam.well_type_number, nam.usgs_province_num, nam.ch4_emission_factor]],
                                                                on = [nam.usgs_province_num, nam.well_type_number],
                                                                how='left').fillna(0.0).drop_duplicates()
        self.projects_producing_gas     = pd.merge(self.projects_producing_gas,
                                                   self.ch4_emission_factors_basin[[nam.well_type_number, nam.usgs_province_num, nam.ch4_emission_factor]],
                                                                on = [nam.usgs_province_num, nam.well_type_number],
                                                                how='left').fillna(0.0).drop_duplicates()
        self.projects_producing_oil     = pd.merge(self.projects_producing_oil,
                                                   self.ch4_emission_factors_basin[[nam.well_type_number, nam.usgs_province_num, nam.ch4_emission_factor]],
                                                                on = [nam.usgs_province_num, nam.well_type_number],
                                                                how='left').fillna(0.0).drop_duplicates()
        self.projects_other_eor         = pd.merge(self.projects_other_eor,
                                                   self.ch4_emission_factors_basin[[nam.well_type_number, nam.usgs_province_num, nam.ch4_emission_factor]],
                                                                on = [nam.usgs_province_num, nam.well_type_number],
                                                                how='left').fillna(0.0).drop_duplicates()
        self.projects_co2_eor           = pd.merge(self.projects_co2_eor,
                                                   self.ch4_emission_factors_basin[[nam.well_type_number, nam.usgs_province_num, nam.ch4_emission_factor]],
                                                                on = [nam.usgs_province_num, nam.well_type_number],
                                                                how='left').fillna(0.0).drop_duplicates()
        self.projects_undiscovered      = pd.merge(self.projects_undiscovered,
                                                   self.ch4_emission_factors_basin[[nam.well_type_number, nam.usgs_province_num, nam.ch4_emission_factor]],
                                                                on = [nam.usgs_province_num, nam.well_type_number],
                                                                how='left').fillna(0.0).drop_duplicates()


        #Set USGS Province Number for Cost Merge (Appalachian and East TX/LA salt flats are broken into sub-basin components)
        self.projects_continuous[nam.usgs_province_num_merge]     = self.projects_continuous[nam.usgs_province_num]
        self.projects_producing_gas[nam.usgs_province_num_merge]  = self.projects_producing_gas[nam.usgs_province_num]
        self.projects_producing_oil[nam.usgs_province_num_merge]  = self.projects_producing_oil[nam.usgs_province_num]
        self.projects_other_eor[nam.usgs_province_num_merge]      = self.projects_other_eor[nam.usgs_province_num]
        self.projects_co2_eor[nam.usgs_province_num_merge]        = self.projects_co2_eor[nam.usgs_province_num]
        self.projects_undiscovered[nam.usgs_province_num_merge]   = self.projects_undiscovered[nam.usgs_province_num]

        #Match usgs province numbers to cost eq match number (specific usgs provinces are split by play)
        def usgs_province_num_merge_match(projects_df):
            projects_df.loc[projects_df[nam.play].isin([4774, 4775]), nam.usgs_province_num_merge] = 471 #Haynesville
            projects_df.loc[projects_df[nam.play].isin([6761, 6776, 6777, 6778, 6779, 6780, 6781, 6782, 6783]), nam.usgs_province_num_merge] = 671 #Marcellus
            projects_df.loc[projects_df[nam.play].isin([6790, 6791, 6792, 6793]), nam.usgs_province_num_merge] = 672 #Utica
            projects_df[nam.usgs_province_num_merge] = projects_df[nam.usgs_province_num_merge].astype(str)

            return projects_df

        self.projects_continuous    = usgs_province_num_merge_match(self.projects_continuous)
        self.projects_producing_gas = usgs_province_num_merge_match(self.projects_producing_gas)
        self.projects_producing_oil = usgs_province_num_merge_match(self.projects_producing_oil)
        self.projects_other_eor     = usgs_province_num_merge_match(self.projects_other_eor)
        self.projects_co2_eor       = usgs_province_num_merge_match(self.projects_co2_eor)
        self.projects_undiscovered  = usgs_province_num_merge_match(self.projects_undiscovered)

        #Drop unused columns to reduce runtime
        prod_range      = list(range(self.parent.final_aeo_year - self.zero_year + 1))
        wi_range        = [('WI' + str(x + 1)) for x in prod_range]

        self.projects_continuous    = self.projects_continuous.drop(wi_range, axis = 1)
        self.projects_producing_gas = self.projects_producing_gas.drop(wi_range, axis = 1)
        self.projects_producing_oil = self.projects_producing_oil.drop(wi_range, axis = 1)
        self.projects_other_eor     = self.projects_other_eor.drop(wi_range, axis = 1)
        self.projects_co2_eor       = self.projects_co2_eor.drop(wi_range, axis = 1)
        self.projects_undiscovered  = self.projects_undiscovered.drop(wi_range, axis = 1)

        pass


    def setup_play_map(self):
        """Setup Restart file play map.

        Returns
        -------
        self.play_map : df
            DataFrame containing mapping of CO2 EOR plays
        """
        # Play Map
        self.play_map = self.projects_co2_eor['play'].unique()
        self.play_map = pd.DataFrame(self.play_map)
        self.play_map.index = self.play_map.index + 1

        pass


    def set_up_projects_crude_type(self):
        """Assigns LFMM crude type numbers to input tables.

        **LFMM API Numbers**

            1. Light Sweet
            2. Light Sour
            3. Medium Medium Sour
            4. Medium Sour
            5. Heavy Sweet
            6. Heavy Sour
            7. California
            8. Syncrude
            9. DilBit/SynBit
            10. Ultra Light Sweet
            11. Condensates

        **AVG API VALUES**
            * DEFAULT     LF_APIVOL(M,:) = 35.0
            * IF (M.eq.1) LF_APIVOL(m,:) = 39.7
            * IF (M.eq.2) LF_APIVOL(m,:) = 37.3
            * IF (M.eq.3) LF_APIVOL(m,:) = 32.7
            * IF (M.eq.4) LF_APIVOL(m,:) = 30.9
            * IF (M.eq.5) LF_APIVOL(m,:) = 22.5
            * IF (M.eq.6) LF_APIVOL(m,:) = 19.9
            * IF (M.eq.7) LF_APIVOL(m,:) = 17.4
            * IF (M.eq.8) LF_APIVOL(m,:) = 31.7
            * IF (M.eq.9) LF_APIVOL(m,:) = 20.6
            * IF (M.eq.10) LF_APIVOL(m,:) = 45.0
            * IF (M.eq.11) LF_APIVOL(m,:) = 55.0


        Returns
        -------
        self.projects_continuous : df
            Table of horizontal projects

        self.projects_producing_gas : df
            Table of legacy producing vertical natural gas projects

        self.projects_producing_oil : df
            Table of legacy producing vertical oil projects

        self.projects_other_eor : df
            Table of secondary production projects that are not CO2 EOR

        self.projects_co2_eor : df
            Table of CO2 EOR projects

        self.projects_undiscovered : df
            Table of undiscovered projects
        """

        project_table_list = [self.projects_continuous,
                        self.projects_producing_gas,
                        self.projects_producing_oil,
                        self.projects_other_eor,
                        self.projects_co2_eor,
                        self.projects_undiscovered]

        for table in project_table_list:
            table[nam.lfmm_crude_type] = 0
            table[nam.avg_api] = 0.0
            table[nam.crude_heat] = 0.0

            # Create Masks
            well_type_mask = (table[nam.well_type_number] == 1) | (table[nam.well_type_number] == 3)
            sulfur_mask = table[nam.sulfur] <= 0
            no_api_mask = table[nam.api] <= 0
            ca_mask = table[nam.state] == 'CA'
            sulf_1_1_mask = table[nam.sulfur] < 1.1
            sulf_0_5_mask = table[nam.sulfur] < 0.5
            api_mask_27 = table[nam.api] < 27.0
            api_mask_35 = table[nam.api] < 35.0
            api_mask_40 = table[nam.api] < 40.0
            api_mask_50 = table[nam.api] < 50.0

            ###Set Max API Volume
            table.loc[table[nam.api] > 60, nam.api] = 60

            ###Fix Sulfur Volumes
            #Fix Sulfur Volumes by State
            temp_df = table[well_type_mask & sulfur_mask].copy()
            temp_df.loc[temp_df[nam.state] == 'AL', nam.sulfur] = 2.41
            temp_df.loc[temp_df[nam.state] == 'CA', nam.sulfur] = 1.41
            temp_df.loc[temp_df[nam.state] == 'CO', nam.sulfur] = 0.16
            temp_df.loc[temp_df[nam.state] == 'KS', nam.sulfur] = 0.24
            temp_df.loc[temp_df[nam.state] == 'OK', nam.sulfur] = 0.30
            temp_df.loc[temp_df[nam.state] == 'TX', nam.sulfur] = 0.22
            table.update(temp_df)

            #Fix Sulfur Volumes by Province
            sulfur_mask = table[nam.sulfur] <= 0
            temp_df = table[well_type_mask & sulfur_mask].copy()
            temp_df = temp_df[temp_df[nam.usgs_province_num].isin([20,21,22,36,38,39,40,45,47,55,58,60,62])]
            temp_df[nam.sulfur] = 0.2
            table.update(temp_df)

            temp_df = table[well_type_mask & sulfur_mask].copy()
            temp_df = temp_df[temp_df[nam.usgs_province_num].isin([10,18,19,27,28,31,43,44,46,51,52,
                                                                   53,56,59,61,63,64,67])]
            temp_df[nam.sulfur] = 0.4
            table.update(temp_df)

            temp_df = table[well_type_mask & sulfur_mask].copy()
            temp_df = temp_df[temp_df[nam.usgs_province_num].isin([6,7,8,11,12,13,14,33,34,35,49,50,65])]
            temp_df[nam.sulfur] = 1.2
            table.update(temp_df)

            temp_df = table[well_type_mask & sulfur_mask].copy() #All other provinces
            temp_df = temp_df[~temp_df[nam.usgs_province_num].isin([20,21,22,36,38,39,40,45,47,55,58,
                                                                    60,62,10,18,19,27,28,31,43,44,46,
                                                                    51,52,53,56,59,61,63,64,67,6,7,8,
                                                                    11,12,13,14,33,34,35,49,50,65])]
            temp_df[nam.sulfur] = 0.2
            table.update(temp_df)


            ###Fix API Numbers and set crude heat content (BTU per barrel)
            temp_df = table[no_api_mask].copy()
            temp_df = temp_df[temp_df[nam.usgs_province_num].isin([6,13])]
            temp_df[nam.api] = 25
            table.update(temp_df)

            temp_df = table[no_api_mask].copy()
            temp_df = temp_df[temp_df[nam.usgs_province_num].isin([38,67])]
            temp_df[nam.api] = 35
            table.update(temp_df)

            temp_df = table[no_api_mask].copy()
            temp_df = temp_df[~temp_df[nam.usgs_province_num].isin([6,13,38,67])]
            temp_df[nam.api] = 45
            table.update(temp_df)

            table[nam.crude_heat] = 158886 * np.exp(-0.0043*table[nam.api]) * 42 * 0.000001


            ###Set LFMM crude type volumes and average api
            #MAC 20240918 - moved California since, I think, setting is overwritten by following assignments

            #Crude Type 5- API < 27, Low Sulfur
            mask = api_mask_27 & sulf_1_1_mask
            temp_df = table[mask].copy()
            temp_df[nam.lfmm_crude_type] = 5
            temp_df[nam.avg_api] = 22.5
            table.update(temp_df)

            #Crude Type 6- API < 27, High Sulfur
            mask = api_mask_27 & ~sulf_1_1_mask
            temp_df = table[mask].copy()
            temp_df[nam.lfmm_crude_type] = 6
            temp_df[nam.avg_api] = 19.9
            table.update(temp_df)

            #Crude type 3, 27 < API < 35, Low Sulfur
            mask = api_mask_35 & ~api_mask_27 & sulf_1_1_mask
            temp_df = table[mask].copy()
            temp_df[nam.lfmm_crude_type] = 3
            temp_df[nam.avg_api] = 32.7
            table.update(temp_df)

            #Crude type 4, 35 < API < 35, High Sulfur
            mask = api_mask_35 & ~api_mask_27 & ~sulf_1_1_mask
            temp_df = table[mask].copy()
            temp_df[nam.lfmm_crude_type] = 4
            temp_df[nam.avg_api] = 30.9
            table.update(temp_df)

            #Crude type 1, 35 < API < 40, Low Sulfur
            mask = api_mask_40 & ~api_mask_35 & sulf_0_5_mask
            temp_df = table[mask].copy()
            temp_df[nam.lfmm_crude_type] = 1
            temp_df[nam.avg_api] = 39.7
            table.update(temp_df)

            #Crude type 2, 35 < API < 40, High Sulfur
            mask = api_mask_40 & ~api_mask_35 & ~sulf_0_5_mask
            temp_df = table[mask].copy()
            temp_df[nam.lfmm_crude_type] = 2
            temp_df[nam.avg_api] = 37.3
            table.update(temp_df)

            #Crude type 10, 40 < API < 50, Low Sulfur
            mask = api_mask_50 & ~api_mask_40 & sulf_0_5_mask
            temp_df = table[mask].copy()
            temp_df[nam.lfmm_crude_type] = 10
            temp_df[nam.avg_api] = 45.0
            table.update(temp_df)

            #Crude type 2, 40 < API < 50, High Sulfur
            mask = api_mask_50 & ~api_mask_40 & ~sulf_0_5_mask
            temp_df = table[mask].copy()
            temp_df[nam.lfmm_crude_type] = 2
            temp_df[nam.avg_api] = 37.3
            table.update(temp_df)

            #Crude type 11, API > 50, Low Sulfur
            mask = ~api_mask_50 & sulf_0_5_mask
            temp_df = table[mask].copy()
            temp_df[nam.lfmm_crude_type] = 11
            temp_df[nam.avg_api] = 55.0
            table.update(temp_df)

            #Crude type 2, API > 50, High Sulfur
            mask = ~api_mask_50 & ~sulf_0_5_mask
            temp_df = table[mask].copy()
            temp_df[nam.lfmm_crude_type] = 2
            temp_df[nam.avg_api] = 37.3
            table.update(temp_df)

            # Crude Type - California
            temp_df = table[ca_mask].copy()
            temp_df[nam.lfmm_crude_type] = 7
            temp_df[nam.avg_api] = 17.4
            table.update(temp_df)

            #Set lfmm crude type as integer
            table[nam.lfmm_crude_type] = table[nam.lfmm_crude_type].astype(int)

        pass


    def setup_output_tables(self):
        """Setup output tables and set baseline production volumes and well counts from producing projects.

            * Only legacy producing vertical wells are counted, horizontal wells are not
            * These well counts are then adjusted based on oil price (higher oil price -> more wells)
            * Current well counts are based on 2018 annual estimates, which require some degree of analyst judgement in the
              *Set Well Ratio* section of the code to smooth

        Returns
        -------
        self.crude_production : df
            DataFrame of onshore crude oil production

        self.natgas_production : df
            DataFrame of onshore natural gas production

        self.ngpl_production : df
            DataFrame of onshore natural gas plant liquids production

        self.water_production : df
            DataFrame of onshore water production

        self.wells : df
            DataFrame of onshore wells

        """

        #Start with baseline crude and gas production
        years = self.parent.final_aeo_year - self.zero_year + 1

        #Combine producing oil and gas projects to reduce redundancy
        self.producing_projects = pd.concat([self.projects_producing_oil,
                                   self.projects_producing_gas],
                                  ignore_index=True)

        #Set production ranges
        prod_range      = list(range(years))
        crude_range     = [('OP' + str(x + 1)) for x in prod_range]
        natgas_range    = [('GP' + str(x + 1)) for x in prod_range]
        water_range     = [('WP' + str(x + 1)) for x in prod_range]
        well_range      = [('WL' + str(x + 1)) for x in prod_range]

        #Create production tables and convert production units
        self.crude_production[prod_range]  = self.producing_projects[crude_range] * 1000
        self.natgas_production[prod_range] = self.producing_projects[natgas_range] * 1000
        self.ngpl_production[prod_range]   = self.producing_projects[natgas_range].mul(self.producing_projects[nam.ngpl], axis = 'index').mul(0.000001, axis='index').div(365, axis='index') #mmb/d
        self.water_production[prod_range]  = self.producing_projects[water_range] * 1000

        #Set production table year column values
        self.crude_production.columns   = self.crude_production.columns + self.zero_year
        self.natgas_production.columns  = self.natgas_production.columns + self.zero_year
        self.ngpl_production.columns    = self.ngpl_production.columns + self.zero_year
        self.water_production.columns   = self.water_production.columns + self.zero_year

        #Apply crude production identifiers
        self.crude_production[nam.hsm_index]                = self.producing_projects[nam.hsm_index].copy()
        self.crude_production[nam.resid]                    = self.producing_projects[nam.resid].copy()
        self.crude_production[nam.process_code]             = self.producing_projects[nam.process_code].copy()
        self.crude_production[nam.district_number]          = self.producing_projects[nam.district_number].copy()
        self.crude_production[nam.region_number]            = self.producing_projects[nam.region_number].copy()
        self.crude_production[nam.federal_land]             = 'N'
        self.crude_production[nam.well_type_number]         = self.producing_projects[nam.well_type_number].copy()
        self.crude_production[nam.oil_type]                 = self.producing_projects[nam.oil_type].copy()
        self.crude_production[nam.oil_type_number]          = self.producing_projects[nam.oil_type_number].copy()
        self.crude_production[nam.gas_type]                 = self.producing_projects[nam.gas_type].copy()
        self.crude_production[nam.gas_type_number]          = self.producing_projects[nam.gas_type_number].copy()
        self.crude_production[nam.lfmm_crude_type]          = self.producing_projects[nam.lfmm_crude_type].copy()
        self.crude_production[nam.api]                      = self.producing_projects[nam.api].copy()
        self.crude_production[nam.avg_api]                  = self.producing_projects[nam.avg_api].copy()
        self.crude_production[nam.play]                     = self.producing_projects[nam.play].copy()
        self.crude_production[nam.year_production_start]    = self.rest_curcalyr

        #Apply natural gas production identifiers
        self.natgas_production[nam.hsm_index]               = self.producing_projects[nam.hsm_index].copy()
        self.natgas_production[nam.resid]                   = self.producing_projects[nam.resid].copy()
        self.natgas_production[nam.process_code]            = self.producing_projects[nam.process_code].copy()
        self.natgas_production[nam.district_number]         = self.producing_projects[nam.district_number].copy()
        self.natgas_production[nam.region_number]           = self.producing_projects[nam.region_number].copy()
        self.natgas_production[nam.federal_land]             = 'N'
        self.natgas_production[nam.well_type_number]        = self.producing_projects[nam.well_type_number].copy()
        self.natgas_production[nam.gas_type]                = self.producing_projects[nam.gas_type].copy()
        self.natgas_production[nam.gas_type_number]         = self.producing_projects[nam.gas_type_number].copy()
        self.natgas_production[nam.oil_type]                = self.producing_projects[nam.oil_type].copy()
        self.natgas_production[nam.oil_type_number]         = self.producing_projects[nam.oil_type_number].copy()
        self.natgas_production[nam.lfmm_crude_type]         = self.producing_projects[nam.lfmm_crude_type].copy()
        self.natgas_production[nam.api]                     = self.producing_projects[nam.api].copy()
        self.natgas_production[nam.avg_api]                 = self.producing_projects[nam.avg_api].copy()
        self.natgas_production[nam.play]                    = self.producing_projects[nam.play].copy()
        self.natgas_production[nam.year_production_start]   = self.rest_curcalyr

        #Apply ngpl production identifiers
        self.ngpl_production[nam.hsm_index]               = self.producing_projects[nam.hsm_index       ].copy()
        self.ngpl_production[nam.resid]                   = self.producing_projects[nam.resid           ].copy()
        self.ngpl_production[nam.process_code]            = self.producing_projects[nam.process_code    ].copy()
        self.ngpl_production[nam.district_number]         = self.producing_projects[nam.district_number ].copy()
        self.ngpl_production[nam.region_number]           = self.producing_projects[nam.region_number   ].copy()
        self.ngpl_production[nam.federal_land]            = 'N'
        self.ngpl_production[nam.play]                    = self.producing_projects[nam.play].copy()
        self.ngpl_production[nam.year_production_start]   = self.rest_curcalyr

        #Other NGPL Production
        self.ngpl_ethane_production = self.ngpl_production.copy()
        self.ngpl_propane_production = self.ngpl_production.copy()
        self.ngpl_butane_production = self.ngpl_production.copy()
        self.ngpl_isobutane_production = self.ngpl_production.copy()
        self.ngpl_proplus_production = self.ngpl_production.copy()

        #Apply Factors
        prod_range_ngpl = [year + self.zero_year for year in prod_range]
        self.ngpl_ethane_production[prod_range_ngpl] = self.ngpl_ethane_production[prod_range_ngpl].mul(self.producing_projects['NGPLET'], axis='index')
        self.ngpl_propane_production[prod_range_ngpl] = self.ngpl_propane_production[prod_range_ngpl].mul(self.producing_projects['NGPLPR'], axis='index')
        self.ngpl_butane_production[prod_range_ngpl] = self.ngpl_butane_production[prod_range_ngpl].mul(self.producing_projects['NGPLBU'], axis='index')
        self.ngpl_isobutane_production[prod_range_ngpl] = self.ngpl_isobutane_production[prod_range_ngpl].mul(self.producing_projects['NGPLIS'], axis='index')
        self.ngpl_proplus_production[prod_range_ngpl] = self.ngpl_proplus_production[prod_range_ngpl].mul(self.producing_projects['NGPLPP'], axis='index')

        #Setup wells table (just setup with zeros, use natgas as template) and apply well identifiers
        self.wells[nam.hsm_index]               = self.producing_projects[nam.hsm_index       ].copy()
        self.wells[nam.resid]                   = self.producing_projects[nam.resid           ].copy()
        self.wells[nam.process_code]            = self.producing_projects[nam.process_code    ].copy()
        self.wells[nam.district_number]         = self.producing_projects[nam.district_number ].copy()
        self.wells[nam.region_number]           = self.producing_projects[nam.region_number   ].copy()
        self.wells[nam.well_type_number]        = self.producing_projects[nam.well_type_number].copy()
        self.wells[nam.play]                    = self.producing_projects[nam.play].copy()
        self.wells['decline_factor']            = self.producing_projects['decline_factor']
        self.wells[nam.well_decline_limit]      = 0
        self.wells[nam.past_wells]              = 0
        self.wells[nam.year_production_start]   = self.rest_curcalyr
        prod_range_abs = [x + self.zero_year for x in prod_range]
        self.wells[prod_range_abs] = 0


        ###Only get wells for producing conventional projects
        well_mask = (self.wells[nam.well_type_number] == 1) #| (self.wells[nam.well_type_number] == 3)
        temp_wells = self.wells[well_mask].copy()
        project_mask = (self.producing_projects[nam.well_type_number] == 1) | (self.producing_projects[nam.well_type_number] == 3)
        temp_projects = self.producing_projects[project_mask].copy()

        #Apply well count to self.wells
        temp_wells[prod_range_abs] = temp_projects[well_range].copy()
        self.wells.update(temp_wells)


        ###Get producing wells/year
        #Mask for Well type (Oil and Natural Gas have different price adjustment eqs)
        well_type_number_mask = self.wells[nam.well_type_number] <= 2

        #Get Gas and Oil Producing Project Wells
        oil_wells_df = self.wells[well_type_number_mask].copy()
        gas_wells_df = self.wells[~well_type_number_mask].copy()

        #Get Oil Price Adjustment value based on regional crude prices
        year_price_adj = self.parent.rest_dcrdwhp.loc[[1,2,3,4,5,6,7]].copy()
        year_price_adj = year_price_adj.div(year_price_adj[self.zero_year], axis = 0)
        year_price_adj = year_price_adj ** 0.25

        price_adj_oil = oil_wells_df[[nam.region_number]].copy().reset_index().merge(year_price_adj,
                                                                                     how = 'left',
                                                                                     left_on = nam.region_number,
                                                                                     right_index = True).set_index('index')

        #Apply price_adj to oil wells and calculate producing oil wells by year
        eval_years = list(range(self.zero_year, (self.parent.final_aeo_year + 1)))
        oil_wells_df[eval_years] = 1.03 * oil_wells_df[eval_years]
        oil_wells_df[eval_years] = oil_wells_df[eval_years].mul(price_adj_oil[eval_years], axis = 1)


        #Get Gas Price Adjustment value based on district natural gas prices
        year_price_adj = self.parent.dist_natgas_price.copy()
        year_price_adj = year_price_adj.div(year_price_adj[self.zero_year], axis = 0)
        year_price_adj = year_price_adj ** 0.25

        price_adj_gas = gas_wells_df[[nam.district_number]].copy().reset_index().merge(year_price_adj,
                                                                                     how = 'left',
                                                                                     left_on = nam.district_number,
                                                                                     right_index = True).set_index('index')

        #Apply price_adj to gas wells and calculate producing oil wells by year
        eval_years = list(range(self.zero_year, (self.parent.final_aeo_year + 1)))
        gas_wells_df[eval_years] = 1.03 * gas_wells_df[eval_years]
        gas_wells_df[eval_years] = gas_wells_df[eval_years].mul(price_adj_gas[eval_years], axis = 1)

        #Update Self.wells with producing project wells
        self.wells.update(oil_wells_df)
        self.wells.update(gas_wells_df)


        ###Set Well Ratio
        #The goal here is to produce a  well count that matches STEO, and then to stabilize from there, we do not currently have good projections for producing wells
        self.wells[list(range(self.zero_year, (self.parent.steo_years[-1] + 3)))] = self.wells[
            list(range(self.zero_year, (self.parent.steo_years[-1] + 3)))].mul(0.3)
        self.wells[list(range((self.parent.steo_years[-1] + 3), (self.parent.final_aeo_year + 1)))] = self.wells[
            list(range((self.parent.steo_years[-1] + 3), (self.parent.final_aeo_year + 1)))].mul(0.3)

        #Set minimum wells to 1
        self.wells[list(range(self.zero_year, self.parent.final_aeo_year + 1))] = self.wells[list(range(self.zero_year, self.parent.final_aeo_year + 1))].apply(np.ceil)
        self.wells[list(range(self.zero_year, self.parent.final_aeo_year + 1))] = self.wells[list(range(self.zero_year, self.parent.final_aeo_year + 1))].replace(0, 1)

        pass


    def producing_projects_load_prices(self):
        """Get oil, natural gas and NGPL prices for legacy producing projects based on starting international oil price,
        regional natural gas wellhead prices, and regional NGPL prices, so that projects can be run through the cash flow
        to determine project economic life (i.e. when the project is no longer producing positive net revenue).

        Returns
        -------
        self.producing_price_df : df
            Table of crude oil and natural gas prices for producing projects
        self.producing_projects : df
            Table of legacy producing projects
        """

        #Set production years
        prod_range      = list(range(self.evaluation_years))
        self.producing_price_df = pd.DataFrame(columns = prod_range)

        #Calculate oil price curve for producing project DCF using Restart File starting oil price path
        start_series_price = self.parent.rest_start_price.at[self.rest_curcalyr, nam.value]
        end_series_price = self.parent.rest_start_price[nam.value].iloc[-1]
        growth_rate = (end_series_price / start_series_price) ** (1. / (self.parent.final_aeo_year - (self.rest_curcalyr - 1)))
        self.producing_price_df.loc[nam.crude_price] = start_series_price
        for year in prod_range:
            self.producing_price_df.at[nam.crude_price, year] = start_series_price * (growth_rate ** (year + 1))


        #Calculate gas price curve for producing project DCF using average rate of natural gas price change between zero year and final year
        start_series_price = self.parent.rest_ogpngwhp.at[self.rest_curcalyr, nam.value]
        end_series_price = self.parent.rest_ogpngwhp[nam.value].iloc[-1]
        growth_rate = (end_series_price / start_series_price) ** (1. / (self.parent.final_aeo_year - (self.rest_curcalyr - 1)))
        self.producing_price_df.loc[nam.natgas_price] = start_series_price
        for year in prod_range:
            self.producing_price_df.at[nam.natgas_price, year] = start_series_price * (growth_rate ** (year + 1))

        #NGPL Price
        self.producing_projects[nam.ngpl_price] = self.parent.rest_plginpf.at[(11, self.rest_curcalyr), 'value'] * \
                                        (self.parent.rest_cflgq.at[self.rest_curcalyr, 'value']) / 42

        pass


    def producing_projects_load_costs(self):
        """Apply operating and facility costs to legacy producing projects, so that they can be run through the cash flow to determine
        project economic life (i.e. when the project is no longer producing positive net income).

            * Costs are based on average historic costs derived from Rystad
            * Costs are mapped by production type and USGS Province (i.e. basin)
            * If there are no reported historic costs at the USGS Province level, then costs are assigned as the
              HSM region level average with a cost adder to indicate that there is limited drilling in these areas
            * Apply tech improvement rate (since cost average is derived from the past 5 years of data)

        Returns
        -------
        self.producing_projects : df
            Table of legacy producing projects
        """
        ###Set USGS Province numbers and region numbers to match projects format
        self.basin_costs[nam.usgs_province_num_merge] = self.basin_costs[nam.usgs_province_num_merge].astype(str)
        self.region_costs[nam.region_number] = self.region_costs[nam.region_number].astype(int)

        #Rename well_type_merge column
        self.basin_costs = self.basin_costs.rename(columns = {nam.well_type:nam.well_type_merge})
        self.region_costs = self.region_costs.rename(columns = {nam.well_type:nam.well_type_merge})

        #Create cost type list
        cost_type_list = [nam.production_opex_brl, nam.transport_opex_brl, nam.facility_capex_well, nam.sga_opex_well]

        #Create a merge variable for projects because Tight/Shale gas use the same opex values, but are different well types
        self.producing_projects[nam.well_type_merge] = self.producing_projects[nam.well_type]

        self.producing_projects.loc[(self.producing_projects[nam.process_code] <= 3), nam.well_type_merge] = 'Conventional'
        self.producing_projects.loc[(self.producing_projects[nam.process_code] == 4), nam.well_type_merge] = 'Tight Oil'
        self.producing_projects.loc[(self.producing_projects[nam.process_code] == 5) | (self.producing_projects[nam.process_code] == 7), nam.well_type_merge] = 'Shale Gas'
        self.producing_projects.loc[(self.producing_projects[nam.process_code] == 7), nam.well_type_merge] = 'Coalbed Methane'


        ###Merge costs by USGS basin
        self.producing_projects = self.producing_projects.reset_index().merge(self.basin_costs,
                                                                              how = 'left',
                                                                              on = [nam.usgs_province_num_merge,
                                                                                    nam.well_type_merge]).set_index('index')


        ###Merge projects that could not be matched to costs by basin to HSM region
        #Create new temp region cost df
        cost_mask = (np.isnan(self.producing_projects[nam.production_opex_brl])) | \
                (np.isnan(self.producing_projects[nam.transport_opex_brl])) | \
                (np.isnan(self.producing_projects[nam.facility_capex_well])) | \
                 (np.isnan(self.producing_projects[nam.sga_opex_well]))
        temp_region_cost_df = self.producing_projects[cost_mask].copy()
        temp_region_cost_df = temp_region_cost_df.drop([nam.production_opex_brl, nam.transport_opex_brl, nam.facility_capex_well, nam.sga_opex_well], axis = 1)

        #Merge Dfs
        temp_region_cost_df = temp_region_cost_df.reset_index().merge(self.region_costs,
                                                                              how='left',
                                                                              on=[nam.region_number,
                                                                                  nam.well_type_merge]).set_index('index')

        #Assign cost adder to regional matches assuming that these are not highly economical producing regions
        for cost_type in cost_type_list:
            temp_region_cost_df[cost_type] = temp_region_cost_df[cost_type].mul(1.50)

        #Update Projects with temp df
        self.producing_projects.update(temp_region_cost_df)


        ###Fill remaining NANs with total cost means with a significant multiplier since these projects are almost certainly not economical
        cost_mask = (np.isnan(self.producing_projects[nam.production_opex_brl])) | \
                (np.isnan(self.producing_projects[nam.transport_opex_brl])) | \
                (np.isnan(self.producing_projects[nam.facility_capex_well])) | \
                 (np.isnan(self.producing_projects[nam.sga_opex_well]))
        temp_national_cost_df = self.producing_projects[cost_mask].copy()
        temp_national_cost_df = temp_national_cost_df.drop([nam.production_opex_brl, nam.transport_opex_brl, nam.facility_capex_well, nam.sga_opex_well], axis = 1)


        cost_mean_df = self.region_costs.groupby([nam.well_type_merge]).mean()
        temp_national_cost_df = temp_national_cost_df.reset_index().merge(cost_mean_df,
                                                                              how='left',
                                                                              on=[nam.well_type_merge]).set_index('index')

        #Assign higher cost adder to national matches assuming that these are not highly economical producing regions
        for cost_type in cost_type_list:
            temp_national_cost_df[cost_type] = temp_national_cost_df[cost_type] * 2.00

        #Update Projects with temp df
        self.producing_projects.update(temp_national_cost_df)

        # Apply Year 1 & 2 Tech Rate Adjustment
        self.producing_projects =  self.producing_projects.reset_index().merge(self.tech_levers, how = 'left', on = nam.well_type_number).set_index('index')

        self.producing_projects[nam.production_opex_brl] = self.producing_projects[nam.production_opex_brl].mul(
            (1 - self.producing_projects[nam.cost_tech]) ** (self.rest_curcalyr - self.zero_year))
        self.producing_projects[nam.transport_opex_brl] = self.producing_projects[nam.transport_opex_brl].mul(
            (1 - self.producing_projects[nam.cost_tech]) ** (self.rest_curcalyr - self.zero_year))
        self.producing_projects[nam.facility_capex_well] = self.producing_projects[nam.facility_capex_well].mul(
            (1 - self.producing_projects[nam.cost_tech]) ** (self.rest_curcalyr - self.zero_year))

        #Mask projects by production (no facility cost for projects that are in terminal decline)
        prod_mask = (self.producing_projects['OP1'] < 50) | (self.producing_projects['GP1'] < 280)
        temp_projects = self.producing_projects[prod_mask].copy()
        temp_projects[nam.facility_capex_well] = 0
        self.producing_projects.update(temp_projects)

        pass

    def producing_projects_load_ch4_costs(self):
        """Load methane venting/flaring costs/ton of ch4 vented/flared.

        Returns
        -------
        self.ch4_emission_cost : df
            DataFrame containing CH4 emission cost bases (costs are not consistent across years)
        """

        def duplicate_list(list, dup_years):
            return [cost for cost in list for _ in dup_years]
        dup_list = duplicate_list([1500], list(range(2026, (self.final_year + 1))))


        if self.rest_curcalyr <= 2024:
            self.ch4_emission_cost = [900, 1200] + dup_list
        elif self.rest_curcalyr <= 2025:
            self.ch4_emission_cost = [1200] + dup_list
        else:
            self.ch4_emission_cost = dup_list

        #Append required 0 values
        zero_list = duplicate_list([0], range((len(self.ch4_emission_cost)), (self.evaluation_years)))
        self.ch4_emission_cost = zero_list + self.ch4_emission_cost
        self.ch4_emission_cost = np.array(self.ch4_emission_cost)

        pass


    def producing_projects_calculate_drilling_capex(self):
        """Applies drilling cost equations derived from historical drilling cost data.

        Notes
        -----
        1. Separate cost equations for oil and gas; however, they have the same processes:
            a. Set baseline cost to intercept value
            b. Apply basin and state coefficients, with missing data being filled in by an average of district, region, type production and a cost adder
            c. Apply lateral length and depth coefficients

        2. If the cost equation produces outliers (costs that exceed the top and bottom historical percentile of costs,
        with some additional room for change with time), a warning is generated

        3. Calculate for dry hole cost:
            a. Data source reports that 1/3 of capital costs are for drilling, 2/3 for completion
            b. Take drilling cost and multiply by dryhole rate to get dryhole costs

        Returns
        -------
        self.producing_projects : df
            Table of legacy producing projects
        """
        #Create drilling cost and dryhole cost columns
        self.producing_projects[nam.drill_cost] = 0
        self.producing_projects[nam.dry_hole_cost] = 0

        #Create cash flow properties dataframe
        self.cash_flow.properties = pd.DataFrame()

        #Load production
        self.cash_flow.crude_production = self.crude_production[list(range(self.zero_year,self.parent.final_aeo_year + 1))].copy()
        self.cash_flow.crude_production.columns = list(range(self.evaluation_years))
        self.cash_flow.natgas_production = self.natgas_production[list(range(self.zero_year,self.parent.final_aeo_year + 1))].copy()
        self.cash_flow.natgas_production.columns = list(range(self.evaluation_years))
        self.cash_flow.ch4_emissions     = self.cash_flow.natgas_production.copy().mul(self.producing_projects[nam.ch4_emission_factor], axis = 0)
        self.cash_flow.co2_use = pd.DataFrame().reindex_like(self.cash_flow.crude_production).fillna(0.0)

        #Mask projects by type
        oil_mask = self.producing_projects[nam.resource_type] == 'oil'
        gas_mask = self.producing_projects[nam.resource_type] == 'gas'

        #Mask projects by production (no cost  for projects that are in terminal decline)
        oil_prod_mask = self.producing_projects['OP1'] > 50
        gas_prod_mask = self.producing_projects['GP1'] > 280

        #Create temp cost dfs
        temp_oil_cost_df = self.producing_projects[oil_mask].copy()[oil_mask & oil_prod_mask]
        temp_gas_cost_df = self.producing_projects[gas_mask].copy()[gas_mask & gas_prod_mask]

        #Mask coefs by type
        oil_mask = self.drill_cost_eq_coefs[nam.resource_type] == 'oil'
        gas_mask = self.drill_cost_eq_coefs[nam.resource_type] == 'gas'

        #Create coef dfs
        oil_coef_df = self.drill_cost_eq_coefs.copy()[oil_mask].set_index(nam.coef_name)
        gas_coef_df = self.drill_cost_eq_coefs.copy()[gas_mask].set_index(nam.coef_name)

        #Load CH4 Emission penalties cost
        self.cash_flow.ch4_emission_cost = pd.DataFrame(index=self.producing_projects.index, columns=list(range(self.evaluation_years))).fillna(1.0)
        self.cash_flow.ch4_emission_cost = self.cash_flow.ch4_emission_cost.mul(self.ch4_emission_cost, axis = 1)

        #Set dryhole cost to 0
        self.cash_flow.dry_hole_cost = pd.DataFrame(index=self.producing_projects.index,columns=list(range(self.evaluation_years))).fillna(0.0)


        ###Oil Cost Equations
        #Get Intercept cost
        temp_oil_cost_df[nam.drill_cost] = oil_coef_df.at['Intercept', nam.coef]


        ###Apply USGS Province coef
        #Set merge value as string
        temp_oil_cost_df[nam.usgs_province_num_merge] = temp_oil_cost_df[nam.usgs_province_num_merge].astype(str)

        temp_oil_cost_df = temp_oil_cost_df.reset_index().merge(
            oil_coef_df[[nam.usgs_province_num_merge, nam.coef]],
            how='left',
            on=nam.usgs_province_num_merge).set_index('index')

        temp_oil_cost_df = temp_oil_cost_df.rename(columns={nam.coef: nam.usgs_province_coef})

        #Fill NA USGS Provinces with lowest level equivalent with multipliers where basin-level data is missing
        #Fill by district number
        temp_oil_cost_df[nam.usgs_province_coef] = temp_oil_cost_df[nam.usgs_province_coef].fillna(
            temp_oil_cost_df.groupby(nam.district_number)[nam.usgs_province_coef].transform('mean') * 1.50)

        #Fill by region number
        temp_oil_cost_df[nam.usgs_province_coef] = temp_oil_cost_df[nam.usgs_province_coef].fillna(
            temp_oil_cost_df.groupby(nam.region_number)[nam.usgs_province_coef].transform('mean') * 1.75)

        #Fill by resource type
        temp_oil_cost_df[nam.usgs_province_coef] = temp_oil_cost_df[nam.usgs_province_coef].fillna(
            temp_oil_cost_df.groupby(nam.resource_type)[nam.usgs_province_coef].transform('mean') * 2.0)

        #Calculate USGS Province coef cost
        temp_oil_cost_df[nam.drill_cost] = temp_oil_cost_df[nam.drill_cost] + temp_oil_cost_df[nam.usgs_province_coef]


        ###Apply State coef
        temp_oil_cost_df = temp_oil_cost_df.reset_index().merge(  #Perform USGS Province coef merge
            oil_coef_df[[nam.coef]],
            how = 'left',
            left_on = nam.state, right_index = True).set_index('index')

        temp_oil_cost_df = temp_oil_cost_df.rename(columns = {nam.coef: nam.state_coef})

        #Fill NA states with lowest level equivalent with multipliers where state-level data is missing
        #Fill by district number
        temp_oil_cost_df[nam.state_coef] = temp_oil_cost_df[nam.state_coef].fillna(
            temp_oil_cost_df.groupby(nam.district_number)[nam.state_coef].transform('mean') * 1.50)

        #Fill by region number
        temp_oil_cost_df[nam.state_coef] = temp_oil_cost_df[nam.state_coef].fillna(
            temp_oil_cost_df.groupby(nam.region_number)[nam.state_coef].transform('mean') * 1.75)

        #Fill by resource type
        temp_oil_cost_df[nam.state_coef] = temp_oil_cost_df[nam.state_coef].fillna(
            temp_oil_cost_df.groupby(nam.resource_type)[nam.state_coef].transform('mean') * 2.0)

        #Calculate USGS Province coef cost
        temp_oil_cost_df[nam.drill_cost] = temp_oil_cost_df[nam.drill_cost] + temp_oil_cost_df[nam.state_coef]


        ###Apply Depth coefficients
        #temp_oil_cost_df['tot_depth'] = temp_oil_cost_df[nam.drill_depth_ft] + temp_oil_cost_df[nam.lateral_length_ft]
        #temp_oil_cost_df[nam.drill_cost] = temp_oil_cost_df[nam.drill_cost] + \
        #                                   (temp_oil_cost_df['tot_depth'] * oil_coef_df.at['tot_depth', nam.coef])
        temp_oil_cost_df[nam.drill_cost] = temp_oil_cost_df[nam.drill_cost] + \
                                           (temp_oil_cost_df[nam.drill_depth_ft] * oil_coef_df.at[nam.num_vfeet, nam.coef])
        temp_oil_cost_df[nam.drill_cost] = temp_oil_cost_df[nam.drill_cost] + \
                                           (temp_oil_cost_df[nam.lateral_length_ft] * oil_coef_df.at[nam.num_latlen, nam.coef])

        #Convert Costs from Natural Log
        temp_oil_cost_df[nam.drill_cost] = np.exp(temp_oil_cost_df[nam.drill_cost])


        ###Track Outliers
        lower_bound = oil_coef_df.at[nam.hist_cost_quantile_01, nam.coef] * 0.5 #adjust for tech rate
        upper_bound = oil_coef_df.at[nam.hist_cost_quantile_99, nam.coef] * 1.5 #adjust for tech rate
        outlier_mask = (temp_oil_cost_df[nam.drill_cost] < lower_bound) | (temp_oil_cost_df[nam.drill_cost] > upper_bound)
        temp_outlier_df = temp_oil_cost_df.copy()[outlier_mask]  # get outliers
        # Purge outliers and trigger warning to indicate outliers have been removed from the dataset
        if temp_outlier_df.shape[0] > 0:
            warnings.warn('Outliers outside of the 1st-99th percentile range of historical cost data have been detected and removed',UserWarning)
        else:
            pass

        ###Gas Cost Equations
        # Get Intercept cost
        temp_gas_cost_df[nam.drill_cost] = gas_coef_df.at['Intercept', nam.coef]


        ###Apply USGS Province coef
        temp_gas_cost_df = temp_gas_cost_df.reset_index().merge(  # Perform USGS Province coef merge
            gas_coef_df[[nam.usgs_province_num_merge, nam.coef]],
            how='left',
            on=nam.usgs_province_num_merge).set_index('index')

        temp_gas_cost_df = temp_gas_cost_df.rename(columns={nam.coef: nam.usgs_province_coef})

        #Fill NA USGS Provinces with lowest level equivalent with multiplier where basin-level data is missing
        #Fill by district number
        temp_gas_cost_df[nam.usgs_province_coef] = temp_gas_cost_df[nam.usgs_province_coef].fillna(
            temp_gas_cost_df.groupby(nam.district_number)[nam.usgs_province_coef].transform('mean') * 1.50)

        #Fill by region number
        temp_gas_cost_df[nam.usgs_province_coef] = temp_gas_cost_df[nam.usgs_province_coef].fillna(
            temp_gas_cost_df.groupby(nam.region_number)[nam.usgs_province_coef].transform('mean') * 1.75)

        #Fill by resource type
        temp_gas_cost_df[nam.usgs_province_coef] = temp_gas_cost_df[nam.usgs_province_coef].fillna(
            temp_gas_cost_df.groupby(nam.resource_type)[nam.usgs_province_coef].transform('mean') * 2.0)

        #Calculate USGS Province coef cost
        temp_gas_cost_df[nam.drill_cost] = temp_gas_cost_df[nam.drill_cost] + temp_gas_cost_df[nam.usgs_province_coef]


        ###Apply State coef
        temp_gas_cost_df = temp_gas_cost_df.reset_index().merge(  # Perform USGS Province coef merge
            gas_coef_df[[nam.coef]],
            how='left',
            left_on=nam.state, right_index = True).set_index('index')

        temp_gas_cost_df = temp_gas_cost_df.rename(columns={nam.coef: nam.state_coef})

        # Fill NA states with lowest level equivalent with multiplier where state-level data is missing
        # Fill by district number
        temp_gas_cost_df[nam.state_coef] = temp_gas_cost_df[nam.state_coef].fillna(
            temp_gas_cost_df.groupby(nam.district_number)[nam.state_coef].transform('mean') * 1.50)

        # Fill by region number
        temp_gas_cost_df[nam.state_coef] = temp_gas_cost_df[nam.state_coef].fillna(
            temp_gas_cost_df.groupby(nam.region_number)[nam.state_coef].transform('mean') * 1.75)

        # Fill by resource type
        temp_gas_cost_df[nam.state_coef] = temp_gas_cost_df[nam.state_coef].fillna(
            temp_gas_cost_df.groupby(nam.resource_type)[nam.state_coef].transform('mean') * 2.0)

        # Calculate USGS Province coef cost
        temp_gas_cost_df[nam.drill_cost] = temp_gas_cost_df[nam.drill_cost] + temp_gas_cost_df[nam.state_coef]


        ###Apply Depth coefficients
        #temp_gas_cost_df['tot_depth'] = temp_gas_cost_df[nam.drill_depth_ft] + temp_gas_cost_df[nam.lateral_length_ft]
        #temp_gas_cost_df[nam.drill_cost] = temp_gas_cost_df[nam.drill_cost] + \
        #                                   (temp_gas_cost_df['tot_depth'] * gas_coef_df.at['tot_depth', nam.coef])
        temp_gas_cost_df[nam.drill_cost] = temp_gas_cost_df[nam.drill_cost] + \
                                           (temp_gas_cost_df[nam.drill_depth_ft] * gas_coef_df.at[nam.num_vfeet, nam.coef])
        temp_gas_cost_df[nam.drill_cost] = temp_gas_cost_df[nam.drill_cost] + \
                                           (temp_gas_cost_df[nam.lateral_length_ft] * gas_coef_df.at[nam.num_latlen, nam.coef])

        ###Convert Costs from Natural Log
        temp_gas_cost_df[nam.drill_cost] = np.exp(temp_gas_cost_df[nam.drill_cost])


        ###Track Outliers
        lower_bound = gas_coef_df.at[nam.hist_cost_quantile_01, nam.coef] * 0.5
        upper_bound = gas_coef_df.at[nam.hist_cost_quantile_99, nam.coef] * 1.5
        outlier_mask = (temp_gas_cost_df[nam.drill_cost] < lower_bound) | (
                    temp_gas_cost_df[nam.drill_cost] > upper_bound)
        temp_outlier_df = temp_gas_cost_df.copy()[outlier_mask]  # get outliers

        #Trigger warning to indicate outliers have been removed from the dataset
        if temp_outlier_df.shape[0] > 0:
            warnings.warn('Outliers outside of the 1st-99th percentile range of historical cost data have been detected and removed',UserWarning)
        else:
            pass


        ###Update master projects Df
        self.producing_projects.update(temp_oil_cost_df)
        self.producing_projects.update(temp_gas_cost_df)


        ###Get Dryhole Cost (Category 1 - Tight/Shale, Category 2 - exploration conventional, Category 3 - development conventional)
        #Shale & Tight Wells
        temp_df = self.producing_projects.copy()
        temp_dryhole_rate = self.dryhole_rate.loc[self.dryhole_rate[nam.drill_category] == 3]
        temp_df = temp_df.reset_index().merge(temp_dryhole_rate, on=[nam.region_number, nam.resource_type], how='left').set_index('index')  # Perform USGS Province coef merge
        temp_df[nam.dry_hole_cost] = temp_df[nam.drill_cost].mul(0.33334) #Rystad reports that 1/3 of capex is drill cost while 2/3 is completion costs 11/08/2022
        temp_df[nam.dry_hole_cost] = temp_df[nam.dry_hole_cost].mul(temp_df[nam.dryhole_rate]) #Dryhole rates from OGSM
        self.producing_projects.update(temp_df)

        # Apply Year 1 & 2 Tech Rate Adjustment
        self.producing_projects[nam.drill_cost] = self.producing_projects[nam.drill_cost].mul(
            (1 - self.producing_projects[nam.drill_tech]) ** (self.rest_curcalyr - self.zero_year))
        self.producing_projects[nam.dry_hole_cost] = self.producing_projects[nam.dry_hole_cost].mul(
            (1 - self.producing_projects[nam.drill_tech]) ** (self.rest_curcalyr - self.zero_year))

        pass


    def producing_projects_load_cashflow(self):
        """Load legacy producing projects into the cashflow.

        Returns
        -------
        self.cash_flow.properties : df
            DataFrame containing properties used in the cash flow (costs, tangible/intangible cost ratios, etc.)

        self.cash_flow.crude_production : df
            DataFrame of onshore crude oil production

        self.cash_flow.natgas_production : df
            DataFrame of onshore natural gas production

        self.cash_flow.crude_price : df
            DataFrame of crude prices

        self.cash_flow.natgas_price : df
            DataFrame of natural gas prices

        self.cash_flow.general_admin_cost : df
            DataFrame of GA costs

        self.cash_flow.kap_cost : df
            DataFrame pf capital costs
        """

        #Create cash flow properties dataframe
        self.cash_flow.properties = pd.DataFrame()

        #Load production (divide by well count for production/well)
        self.cash_flow.crude_production = self.crude_production[list(range(self.zero_year,self.parent.final_aeo_year + 1))].copy().div(
            self.wells[list(range(self.zero_year,self.parent.final_aeo_year + 1))], axis = 0)
        self.cash_flow.crude_production.columns = list(range(self.evaluation_years))

        self.cash_flow.natgas_production = self.natgas_production[list(range(self.zero_year,self.parent.final_aeo_year + 1))].copy().div(
            self.wells[list(range(self.zero_year,self.parent.final_aeo_year + 1))], axis = 0)
        self.cash_flow.natgas_production.columns = list(range(self.evaluation_years))

        self.cash_flow.co2_use = pd.DataFrame().reindex_like(self.cash_flow.crude_production).fillna(0.0) #Fill empty since no CO2

        #Load properties
        self.cash_flow.properties[nam.hsm_index] = self.producing_projects[nam.hsm_index].copy()
        self.cash_flow.crude_price = self.producing_price_df.loc[[nam.crude_price]].copy()
        self.cash_flow.natgas_price = self.producing_price_df.loc[[nam.natgas_price]].copy()
        self.cash_flow.properties[nam.ngpl_price] = self.producing_projects[nam.ngpl_price].copy()
        self.cash_flow.properties[nam.ngpl_volume] = self.producing_projects[nam.ngpl]
        self.cash_flow.properties[[nam.crude_tariff_price, nam.natgas_tariff_price]] = 0.0
        self.cash_flow.properties[nam.royalty_rate] = self.producing_projects[nam.project_royalty_multiplier].mul(self.royalty_rate)
        self.cash_flow.properties[nam.state] = self.producing_projects[nam.state].copy()

        #Adding resource type to cash_flow although not needed for cash flow calculation, this helps with masking later
        self.cash_flow.properties[nam.resource_type] = self.producing_projects[nam.resource_type].copy()

        #Set drilling costs to 0 since these are producing projects
        self.cash_flow.exp_drill_cost = pd.DataFrame(index=self.producing_projects.index,columns=list(range(self.evaluation_years))).fillna(0.0)
        self.cash_flow.dev_drill_cost = pd.DataFrame(index=self.producing_projects.index,columns=list(range(self.evaluation_years))).fillna(0.0)
        self.cash_flow.dev_drill_cost[0] = self.producing_projects[nam.drill_cost].copy().fillna(0.0)
        self.cash_flow.exp_dry_cost = pd.DataFrame(index=self.producing_projects.index,columns=list(range(self.evaluation_years))).fillna(0.0)
        self.cash_flow.dev_dry_cost = pd.DataFrame(index=self.producing_projects.index,columns=list(range(self.evaluation_years))).fillna(0.0)
        self.cash_flow.dev_dry_cost[0] = 0 #Fill as 0

        #Load NGPL Cost
        self.cash_flow.properties[nam.ngpl_cost] = 0.14 #Set to $0.14, on the higher end of the calculated NGPL price for continuous projects

        #Load SGA cost
        self.cash_flow.general_admin_cost   = pd.DataFrame(index=self.producing_projects.index, columns=list(range(self.evaluation_years))).fillna(1.0)
        self.cash_flow.properties[nam.sga_opex_well] = self.producing_projects[nam.sga_opex_well] * 0.10 #Set to 10% to be conservative (no new costs)

        #Set dryhole cost to 0
        self.cash_flow.dry_hole_cost    = pd.DataFrame(index=self.producing_projects.index,columns=list(range(self.evaluation_years))).fillna(0.0)

        #Load production opex
        self.cash_flow.properties[nam.production_opex_brl] = self.producing_projects[nam.production_opex_brl].copy().fillna(0.0) #Adjustment to BOE equiv in cashflow

        #Load transportation opex
        self.cash_flow.properties[nam.crude_trans_price] = self.producing_projects[nam.transport_opex_brl].copy().fillna(0.0)
        self.cash_flow.properties[nam.natgas_trans_price] = self.producing_projects[nam.transport_opex_brl].copy().fillna(0.0)  #Adjustment to BOE equiv in cashflow

        #Load capital cost
        self.cash_flow.kap_cost = pd.DataFrame(index=self.projects.index, columns=list(range(self.evaluation_years)))
        self.cash_flow.kap_cost[0] = (self.producing_projects[nam.facility_capex_well] * 0.50) #Assume some facilities are already built out
        self.cash_flow.kap_cost = self.cash_flow.kap_cost.fillna(0.0)

        #Load tangible cost fractions
        mask = self.cash_flow.properties[nam.resource_type] == nam.oil
        self.cash_flow.properties.loc[mask, nam.exp_tang_frac] = self.oil_exp_tang_frac
        self.cash_flow.properties.loc[mask, nam.dev_tang_frac] = self.oil_dev_tang_frac
        mask = self.cash_flow.properties[nam.resource_type] == nam.gas
        self.cash_flow.properties.loc[mask, nam.exp_tang_frac] = self.gas_exp_tang_frac
        self.cash_flow.properties.loc[mask, nam.dev_tang_frac] = self.gas_dev_tang_frac
        self.cash_flow.properties[nam.kap_tang_frac] = self.kap_tang_frac

        #Load amortization and depreciation schedules
        self.cash_flow.properties[nam.amor_schedule] = self.amor_schedule
        self.cash_flow.properties[nam.deprec_schedule] = self.deprec_schedule

        #Load tax rate
        self.cash_flow.properties[nam.fed_tax_rate] = self.parent.fed_tax_rate

        #Load intangible amortization fraction
        self.cash_flow.properties[nam.intang_amor_frac] = self.intang_amor_frac

        #Load Abandon Cost rate
        self.cash_flow.properties[nam.abandon_rate] = self.abandon_rate * self.producing_projects[nam.facility_capex_well] #Set abandon cost upfront for producing projects

        #Load discount rate
        self.cash_flow.properties[nam.process_code] = self.producing_projects[nam.process_code].fillna(0.0)
        self.cash_flow.properties[nam.discount_rate] = self.parent.discount_rate + 0.05  # cost of capital + required return over cost of capital
        
        #Set unused variables to 0
        self.cash_flow.gg_la_cost = pd.DataFrame(index=self.producing_projects.index, columns=list(range(self.evaluation_years))).fillna(0.0)
        self.cash_flow.invest_credit = pd.DataFrame(index=self.producing_projects.index,columns=list(range(self.evaluation_years))).fillna(0.0)
        self.cash_flow.fed_credit = pd.DataFrame(index=self.producing_projects.index,columns=list(range(self.evaluation_years))).fillna(0.0)
        self.cash_flow.eor_tax_credit = pd.DataFrame(index=self.producing_projects.index, columns=list(range(self.evaluation_years))).fillna(0.0)
        self.cash_flow.equip_cost = pd.DataFrame(index=self.producing_projects.index,columns=list(range(self.evaluation_years))).fillna(0.0)
        self.cash_flow.properties[nam.co2_cost] = 0

        pass


    def run_producing_cash_flow(self):
        """Run cash flow for legacy producing projects to determine project economic life.

            * Project economic life is used to determine when EOR/ASR projects can begin production
            * Secondary production can only turn on within 5 years of the primary production economic life
            * Only calculate to abandonment when project economic life is determined

        Returns
        -------
        self.producing_projects : df
            Table of legacy producing projects
        """
        self.cash_flow.calculate_revenue()
        self.cash_flow.calculate_trans_cost()
        self.cash_flow.calculate_royalty()
        self.cash_flow.calculate_severance()
        self.cash_flow.calculate_drill_cost()
        self.cash_flow.calculate_ngpl_operating_cost()
        self.cash_flow.calculate_operating_cost()
        self.cash_flow.calculate_intangible_tangible()
        self.cash_flow.calculate_depreciation()
        self.cash_flow.calculate_econ_limit()
        self.cash_flow.calculate_abandonment()

        #Return Economic life indicators
        self.producing_projects[nam.econ_life] = self.cash_flow.properties[nam.econ_life]
        self.producing_projects[nam.econ_life] = self.cash_flow.properties[nam.econ_life] + self.zero_year

        pass


    def shut_down_unprofitable_legacy_production(self):
        """Shut down legacy well production in the year when net income becomes negative.

        Returns
        -------
        self.crude_production : df
            DataFrame of onshore crude oil production

        self.natgas_production : df
            DataFrame of onshore natural gas production

        self.ngpl_production : df
            DataFrame of onshore natural gas plant liquids production

        self.water_production : df
            DataFrame of onshore water production

        self.wells : df
            DataFrame of onshore wells
        """

        #Create mask df for binary of whether projects are profitable or not
        mask_df = pd.DataFrame(index = self.crude_production.index, columns = self.crude_production.columns).fillna(1.0)

        #Set mask to 0 for any projects that are shut down
        self.producing_projects['max_econ_life'] = (self.parent.aeo_year + 30)
        self.producing_projects['shutdown_year'] =  self.producing_projects[[nam.econ_life, 'max_econ_life']].min(axis=1)

        temp_df = self.producing_projects.loc[self.producing_projects['shutdown_year'] <= self.final_year]

        for project in temp_df.itertuples():
            index = project.Index
            shutdown_year = project.shutdown_year
            if shutdown_year >= self.final_year:
                pass
            else:
                mask_range = list(range(shutdown_year, self.final_year + 1))
                mask_df.loc[index,mask_range] = 0.0

        #Get year columns
        years = list(range(self.zero_year, self.parent.final_aeo_year + 1))
        mask_df = mask_df[years]

        #Multiply relevant dataframes by mask
        self.crude_production[years]    = self.crude_production[years].mul(mask_df[years], axis = 1)
        self.natgas_production[years]   = self.natgas_production[years].mul(mask_df[years], axis = 1)
        self.ngpl_production[years]     = self.ngpl_production[years].mul(mask_df[years], axis = 1)
        self.water_production[years]    = self.water_production[years].mul(mask_df[years], axis = 1)
        self.wells[years]               = self.wells[years].mul(mask_df[years], axis = 1)

        pass


    def producing_projects_baseline_constraints(self):
        """Get baseline producing project rig and footage values for constraints.

        Returns
        -------
        self.producing_wells : df
            DataFrame of producing projects wells
        self.producing_footage : df
            DataFrame of producing projects cumulative footage
        """

        #Get producing project wells as baseline for constraint
        temp_producing_wells = self.wells.copy()
        temp_producing_wells = temp_producing_wells.drop(['resid'], axis = 1)
        self.producing_wells = temp_producing_wells.groupby(nam.region_number).sum()

        #Get producing project footage as baseline for constraint
        temp_producing_footage = self.producing_projects[[nam.lateral_length_ft, nam.drill_depth_ft]].copy()
        temp_producing_footage[nam.region_cum_footage] = temp_producing_footage[nam.lateral_length_ft].copy() + temp_producing_footage[nam.drill_depth_ft]
        temp_producing_footage = temp_producing_footage.reset_index().merge(self.wells, how = 'left', left_index = True, right_index = True).set_index('index')
        temp_producing_footage = temp_producing_footage.drop(['resid'], axis = 1)


        #Multiply well count by footage to get total well footage
        years = list(range(self.zero_year, self.parent.final_aeo_year + 1))
        temp_producing_footage[years] = temp_producing_footage[years].mul(temp_producing_footage[nam.region_cum_footage], axis = 'index')
        self.producing_footage = temp_producing_footage.groupby(nam.region_number).sum()

        pass


    def load_continuous_projects(self):
        """Load continuous projects into the master self.projects dataframe for economic analysis.

            * Assign last_year_drilling as NINJWELL value
            * Calculate past_wells for drilling eqs

        Returns
        -------
        self.projects : df
            Master DataFrame containing all projects to be processed in current model year
        """
        self.projects = self.projects_continuous.copy()
        self.projects[nam.last_year_drilling] = self.projects[nam.hist_year_wells].copy()
        self.projects[nam.past_wells] = ((self.projects[nam.total_pattern_size_acres] / self.projects[nam.std_pattern_size_acres]).round() - self.projects[nam.totpat])
        self.projects.loc[self.projects[nam.past_wells] < 0, nam.past_wells] = 0

        pass


    def load_eor_projects(self):
        """Load EOR/ASR projects into the master self.projects dataframe for economic analysis.

            * Combine other EOR and CO2 EOR projects
            * Pull out resource-restricted projects
            * Merge in economic life for producing projects to determine project eligibility (EOR projects can only move
              forward if associated producing project is approaching/just past end of economic life)

        Returns
        -------
        self.projects : df
            Master DataFrame containing all projects to be processed in current model year
        """
        #Join EOR and ASR dfs
        projects_eor = self.projects_co2_eor
        #Pull out access-restricted projects (resacc <=3)
        resacc_mask = projects_eor[nam.resacc] >= 3
        projects_eor = projects_eor[resacc_mask].copy()

        #Create co2 eor type and eligibility columns
        projects_eor[nam.eor_type] = projects_eor[nam.resid].str[-1:]
        projects_eor[nam.eor_type] = projects_eor[nam.eor_type].astype(int)
        projects_eor[nam.eligible] = 1

        # Assign project royalty multiplier to EOR projects
        projects_eor[nam.project_royalty_multiplier] = 1

        #CO2 EOR/ASR projects have resids that are identical to a matching conventional legacy production project minus the last two numbers
        #This is how projects will be identified to determine eligibility based on project economic life and other EOR/ASR activity
        projects_eor[nam.eor_resid] = projects_eor[nam.resid].str[:-2]
        self.producing_projects[nam.eor_resid] = self.producing_projects[nam.resid].str[:-2]

        #Load EOR projects into projects
        self.projects = pd.concat((self.projects, projects_eor), ignore_index = True)

        #Create shared projects index
        self.projects = self.projects.reset_index(drop=True)

        #Add Econ life to projects, ASR projects can only run within 5 years of project econ life
        self.projects = self.projects.reset_index().merge(self.producing_projects[[nam.eor_resid, nam.econ_life]],
                                            how='left', on=nam.eor_resid).set_index('index')
        self.projects[nam.econ_life] = self.projects[nam.econ_life].fillna(self.parent.aeo_year + 10)

        pass


    def filter_undiscovered_projects(self):
        """Removes projects on restricted land from the undiscovered projects list and deletes duplicates.

        Returns
        -------
        self.projects_undiscovered : df
            DataFrame of undiscovered projects
        """
        #Pull out access-restricted projects (resacc <=3)
        resacc_mask = self.projects_undiscovered[nam.resacc] >= 3
        self.projects_undiscovered = self.projects_undiscovered[resacc_mask].copy()

        pass


    def set_base_project_params(self):
        """Perform merges and calculations to self.projects for base parameters commonly used in economic analysis.

            * Apply Side case adjustments
            * In the Oil and Gas Supply Cases EURs and technology rates are adjusted by 50%
            * Sidecase switch is pulled in from Scedes
            * Merge technology improvement rates
            * Apply drilling restrictions based on resource access flag

        Returns
        -------
        self.projects : df
            Master DataFrame containing all projects to be processed in current model year
        """
        #Apply supply case adjusmtent for EURs
        prod_range = list(range(self.evaluation_years))
        crude_range = [('OP' + str(x + 1)) for x in prod_range]
        natgas_range = [('GP' + str(x + 1)) for x in prod_range]
        self.projects[crude_range] = self.projects[crude_range].mul(self.parent.side_case_adj, axis = 1)
        self.projects[natgas_range] = self.projects[natgas_range].mul(self.parent.side_case_adj, axis=1)

        #Fill missing latlen with 0.0
        self.projects[nam.lateral_length_ft] = self.projects[nam.lateral_length_ft].fillna(0.0)

        #Merge Tech rates
        self.projects = self.projects.reset_index().merge(self.tech_levers, how = 'left', on = nam.well_type_number).set_index('index')
        self.projects_undiscovered = self.projects_undiscovered.merge(self.tech_levers, how = 'left', on = nam.well_type_number)

        #Pull out access-restricted projects (resacc <=3)
        resacc_mask = self.projects[nam.resacc] >= 3
        self.projects = self.projects[resacc_mask].copy()


        ###Apply drilling limit on restricted federal lands
        #Some lands such as national forest may allow drilling, but it's restricted, so for these designations we cut drilling in half
        #Get resacc limited drilling mask (resacc <= 7) for discovered Projects
        resacc_mask = self.projects[nam.resacc] <= 7

        #Apply drilling restrictions
        temp_df = self.projects[resacc_mask].copy()
        temp_df[nam.total_pattern_size_acres] = temp_df[nam.total_pattern_size_acres] * 0.5
        temp_df[nam.totpat] = temp_df[nam.totpat] * 0.5
        self.projects_undiscovered.update(temp_df)

        #Get resacc limited drilling mask (resacc <= 7) for undiscovered projects
        resacc_mask = self.projects_undiscovered[nam.resacc] <= 7

        #Apply drilling restrictions
        temp_df = self.projects_undiscovered[resacc_mask].copy()
        temp_df[nam.total_pattern_size_acres] = temp_df[nam.total_pattern_size_acres] * 0.5
        self.projects.update(temp_df)

        pass


    def load_costs_setup(self):
        """Loads production opex, transportation opex, sga opex and facility capex into projects.

        * All Costs are $/BOE
        * Projects are each matched to cost by production type and basin
        * If no basin-level costs are available, default to region-level costs with a cost adder
        * if no region-level costs are available default to national-level average costs with a cost adder
        * Additional cost adder is assigned for Other EOR projects, replacing the low-impact, EOR-type specific methodolgy in OGSM

        Returns
        -------
        self.projects : df
            Master DataFrame containing all projects to be processed in current model year

        self.projects_undiscovered : df
            DataFrame of undiscovered projects
        """

        def load_costs(project_df):
            """Function for loading costs.

            Costs need to be applied to master projects df and undiscovered projects using same methodology, hence a shared function.

            Parameters
            ----------
            project_df : df
                DataFrame of projects

            Returns
            -------
            project_df : df
                DataFrame of projects
            """
           
            #Set USGS Province numbers to match projects format
            self.basin_costs[nam.usgs_province_num_merge] = self.basin_costs[nam.usgs_province_num_merge].astype(str)
            self.region_costs[nam.region_number] = self.region_costs[nam.region_number].astype(int)

            #Create a merge variable for projects because Tight/Shale gas use the same opex values, but are different well type, and because Conventional costs are merged
            project_df[nam.well_type_merge] = project_df[nam.well_type]
            project_df.loc[project_df[nam.well_type_merge] == 'Conventional (Oil)', nam.well_type_merge] == 'Conventional'
            project_df.loc[project_df[nam.well_type_merge] == 'Conventional (Gas)', nam.well_type_merge] == 'Conventional'

            project_df.loc[(project_df[nam.process_code] <= 11) |
                        (project_df[nam.process_code] == 16) |
                        (project_df[nam.process_code] == 17) |
                        (project_df[nam.process_code] == 18) |
                        (project_df[nam.process_code] == 19), nam.well_type_merge] = 'Conventional'

            project_df.loc[(project_df[nam.process_code] == 12) |
                        (project_df[nam.process_code] == 20), nam.well_type_merge] = 'Tight Oil'

            project_df.loc[(project_df[nam.process_code] == 13) |
                        (project_df[nam.process_code] == 14) |
                        (project_df[nam.process_code] == 21) |
                        (project_df[nam.process_code] == 22), nam.well_type_merge] = 'Shale Gas'

            project_df.loc[(project_df[nam.process_code] == 15) |
                        (project_df[nam.process_code] == 23), nam.well_type_merge] = 'Coalbed Methane'

            #Create cost type list
            cost_type_list = [nam.production_opex_brl, nam.transport_opex_brl, nam.facility_capex_well]

            #Drop year 1 basin costs if year 2
            try:
                project_df = project_df.drop([nam.production_opex_brl,nam.transport_opex_brl,nam.facility_capex_well,nam.sga_opex_well], axis = 1)
            except:
                pass

            #Merge Basin Costs
            project_df = project_df.reset_index().merge(self.basin_costs,
                                                  how='left',
                                                  on=[nam.usgs_province_num_merge,
                                                      nam.well_type_merge]).set_index('index')


            ###Merge projects that could not be matched to costs by basin to HSM region
            #Create new temp region cost df
            cost_mask = (np.isnan(project_df[nam.production_opex_brl])) | \
                        (np.isnan(project_df[nam.transport_opex_brl])) | \
                        (np.isnan(project_df[nam.facility_capex_well])) | \
                        (np.isnan(project_df[nam.sga_opex_well]))
            temp_region_cost_df = project_df[cost_mask].copy()
            temp_region_cost_df = temp_region_cost_df.drop(
                [nam.production_opex_brl, nam.transport_opex_brl, nam.facility_capex_well, nam.sga_opex_well], axis=1)

            #Merge Dfs
            temp_region_cost_df = temp_region_cost_df.reset_index().merge(self.region_costs, how='left',
                                                                          on=[nam.region_number,
                                                                              nam.well_type_merge]).set_index('index')

            #Assign cost adder to regional matches assuming that these are not highly economical producing regions
            for cost_type in cost_type_list:
                temp_region_cost_df[cost_type] = temp_region_cost_df[cost_type] * 1.50

            #Update Projects with temp df
            project_df.update(temp_region_cost_df)

            ###Fill remaining NANs with total cost means with a significant multiplier since these projects are almost certainly not economical
            cost_mask = (np.isnan(project_df[nam.production_opex_brl])) | \
                        (np.isnan(project_df[nam.transport_opex_brl])) | \
                        (np.isnan(project_df[nam.facility_capex_well])) | \
                         (np.isnan(project_df[nam.sga_opex_well]))
            temp_national_cost_df = project_df[cost_mask].copy()
            temp_national_cost_df = temp_national_cost_df.drop([nam.production_opex_brl, nam.transport_opex_brl, nam.facility_capex_well, nam.sga_opex_well], axis=1)

            cost_mean_df = self.region_costs.groupby([nam.well_type_merge]).mean()
            temp_national_cost_df = temp_national_cost_df.reset_index().merge(cost_mean_df,
                                                                              how='left',
                                                                              on=[nam.well_type_merge]).set_index('index')

            #Assign higher cost adder to national matches assuming that these are not highly economical producing regions
            for cost_type in cost_type_list:
                temp_national_cost_df[cost_type] = temp_national_cost_df[cost_type] * 2.00

            #Update Projects with temp df
            project_df.update(temp_national_cost_df)

            #Apply Year 1 & 2 Tech Rate Adjustment
            project_df[nam.production_opex_brl] = project_df[nam.production_opex_brl].mul((1 - project_df[nam.cost_tech]) ** (self.rest_curcalyr - self.zero_year))
            project_df[nam.transport_opex_brl] = project_df[nam.transport_opex_brl].mul((1 - project_df[nam.cost_tech]) ** (self.rest_curcalyr - self.zero_year))
            project_df[nam.facility_capex_well] = project_df[nam.facility_capex_well].mul((1 - project_df[nam.cost_tech]) ** (self.rest_curcalyr - self.zero_year))

            return project_df

        #Apply Function
        self.projects = load_costs(self.projects)
        self.projects_undiscovered = load_costs(self.projects_undiscovered)


        ###Simplified methodology for "Other EOR" Costs
        temp_df = self.projects.loc[self.projects[nam.process_code] == 11].copy()
        temp_df[nam.production_opex_brl] = temp_df[nam.production_opex_brl] * 1.5
        temp_df[nam.facility_capex_well] = temp_df[nam.facility_capex_well] * 2.0
        self.projects.update(temp_df)

        pass


    def drill_cost_eqs_assumptions(self):
        """Adjusts self.drill_cost_eq_coefs so that statistically insignificant coefficients are set to mean of coefficients (assumed to be indistinguishable from distribution).

        Returns
        -------
        self.drill_cost_eq_coefs : df
            Table of cost equations based on inputs from Rystad
        """
        p_score_mask = self.drill_cost_eq_coefs[nam.p_val] > 0.10
        temp_df = self.drill_cost_eq_coefs[p_score_mask].copy()
        temp_df[nam.coef] = 0
        self.drill_cost_eq_coefs.update(temp_df)

        pass


    def calculate_drilling_capex_setup(self):
        """Applies drilling cost equations derived from historical drilling cost data.

        Notes
        -----
        1. Separate cost equations for oil and gas; however, they have the same processes:
            a. Set baseline cost to intercept value
            b. Apply basin and state coefficients, with missing data being filled in by an average of district, region, type production and a cost adder
            c. Apply lateral length and depth coefficients

        2. If the cost equation produces outliers (costs that exceed the top and bottom historical percentile of costs,
        with some additional room for change with time), a warning is generated

        3. Calaculate for dry hole cost:
            a. Data provider reports that 1/3 of capital costs are for drilling, 2/3 for completion
            b. Take drilling cost and multiply by dryhole rate to get dryhole costs

        4. Additional cost adder is assigned for Other EOR projects, replacing the low-impact, EOR-type specific methodolgy in OGSM


        Returns
        -------
        self.projects : df
            Master DataFrame containing all projects to be processed in current model year

        self.projects_undiscovered : df
            DataFrame of undiscovered projects
        """
        def load_costs(project_df):
            """Function for loading costs.

            Costs need to be applied to master projects df and undiscovered projects using same methodology, hence a shared function.

            Parameters
            ----------
            project_df : df
                DataFrame of projects

            Returns
            -------
            project_df : df
                DataFrame of projects
            """
            #Create drilling cost and dryhole cost columns
            project_df[nam.drill_cost] = 0
            project_df[nam.dry_hole_cost] = 0

            #Mask projects by type
            oil_mask = project_df[nam.resource_type] == 'oil'
            gas_mask = project_df[nam.resource_type] == 'gas'

            #Create temp cost dfs
            temp_oil_cost_df = project_df[oil_mask].copy()[oil_mask]
            temp_gas_cost_df = project_df[gas_mask].copy()[gas_mask]
            temp_oil_cost_df[[nam.region_number,nam.district_number,nam.usgs_province_num_merge,nam.drill_depth_ft,nam.lateral_length_ft]]
            temp_gas_cost_df[[nam.region_number,nam.district_number,nam.usgs_province_num_merge,nam.drill_depth_ft,nam.lateral_length_ft]]

            #Mask coefs by type
            oil_mask = self.drill_cost_eq_coefs[nam.resource_type] == 'oil'
            gas_mask = self.drill_cost_eq_coefs[nam.resource_type] == 'gas'

            #Create coef dfs
            oil_coef_df = self.drill_cost_eq_coefs.copy()[oil_mask].set_index(nam.coef_name)
            gas_coef_df = self.drill_cost_eq_coefs.copy()[gas_mask].set_index(nam.coef_name)


            ###Oil Cost Equations
            # Get Intercept cost
            temp_oil_cost_df[nam.drill_cost] = oil_coef_df.at['Intercept', nam.coef]

            ###Apply USGS Province coef
            temp_oil_cost_df = temp_oil_cost_df.reset_index().merge(  # Perform USGS Province coef merge
                oil_coef_df[[nam.usgs_province_num_merge, nam.coef]],
                how='left',
                on=nam.usgs_province_num_merge).set_index('index')

            temp_oil_cost_df = temp_oil_cost_df.rename(columns={nam.coef: nam.usgs_province_coef})

            # Fill NA USGS Provinces with lowest level equivalent with multipliers where basin-level data is missing
            # Fill by district number
            temp_oil_cost_df[nam.usgs_province_coef] = temp_oil_cost_df[nam.usgs_province_coef].fillna(
                temp_oil_cost_df.groupby(nam.district_number)[nam.usgs_province_coef].transform('mean') * 1.50)

            # Fill by region number
            temp_oil_cost_df[nam.usgs_province_coef] = temp_oil_cost_df[nam.usgs_province_coef].fillna(
                temp_oil_cost_df.groupby(nam.region_number)[nam.usgs_province_coef].transform('mean') * 1.75)

            # Fill by resource type
            temp_oil_cost_df[nam.usgs_province_coef] = temp_oil_cost_df[nam.usgs_province_coef].fillna(
                temp_oil_cost_df.groupby(nam.resource_type)[nam.usgs_province_coef].transform('mean') * 2.0)

            # Calculate USGS Province coef cost
            temp_oil_cost_df[nam.drill_cost] = temp_oil_cost_df[nam.drill_cost] + temp_oil_cost_df[nam.usgs_province_coef]


            ###Apply State coef
            temp_oil_cost_df = temp_oil_cost_df.reset_index().merge(  #Perform USGS Province coef merge
                oil_coef_df[[nam.coef]],
                how = 'left',
                left_on = nam.state, right_index = True).set_index('index')

            temp_oil_cost_df = temp_oil_cost_df.rename(columns = {nam.coef: nam.state_coef})

            #Fill NA states with lowest level equivalent with multipliers where state-level data is missing
            #Fill by district number
            
            temp_oil_cost_df[nam.state_coef] = temp_oil_cost_df[nam.state_coef].fillna(
                temp_oil_cost_df.groupby(nam.district_number)[nam.state_coef].transform('mean') * 1.50)

            #Fill by region number
            temp_oil_cost_df[nam.state_coef] = temp_oil_cost_df[nam.state_coef].fillna(
                temp_oil_cost_df.groupby(nam.region_number)[nam.state_coef].transform('mean') * 1.75)

            #Fill by resource type
            temp_oil_cost_df[nam.state_coef] = temp_oil_cost_df[nam.state_coef].fillna(
                temp_oil_cost_df.groupby(nam.resource_type)[nam.state_coef].transform('mean') * 2.0)

            #Calculate USGS Province coef cost
            temp_oil_cost_df[nam.drill_cost] = temp_oil_cost_df[nam.drill_cost] + temp_oil_cost_df[nam.state_coef]


            ###Apply Depth coefficients
            #temp_oil_cost_df['tot_depth'] = temp_oil_cost_df[nam.drill_depth_ft] + temp_oil_cost_df[nam.lateral_length_ft]
            #temp_oil_cost_df[nam.drill_cost] = temp_oil_cost_df[nam.drill_cost] + \
            #                                   (temp_oil_cost_df['tot_depth'] * oil_coef_df.at['tot_depth', nam.coef])
            temp_oil_cost_df[nam.drill_cost] = temp_oil_cost_df[nam.drill_cost] + \
                                               (temp_oil_cost_df[nam.drill_depth_ft] * oil_coef_df.at[nam.num_vfeet, nam.coef])
            temp_oil_cost_df[nam.drill_cost] = temp_oil_cost_df[nam.drill_cost] + \
                                               (temp_oil_cost_df[nam.lateral_length_ft] * oil_coef_df.at[nam.num_latlen, nam.coef])


            ###Convert Costs from Natural Log
            temp_oil_cost_df[nam.drill_cost] = np.exp(temp_oil_cost_df[nam.drill_cost])


            ###Track Outliers
            lower_bound = oil_coef_df.at[nam.hist_cost_quantile_01, nam.coef] * 0.5 #adjust for tech rate
            upper_bound = oil_coef_df.at[nam.hist_cost_quantile_99, nam.coef] * 1.5 #adjust for tech rate
            outlier_mask = (temp_oil_cost_df[nam.drill_cost] < lower_bound) | (temp_oil_cost_df[nam.drill_cost] > upper_bound)
            temp_outlier_df = temp_oil_cost_df.copy()[outlier_mask]  # get outliers
            # Purge outliers and trigger warning to indicate outliers have been removed from the dataset
            if temp_outlier_df.shape[0] > 0:
                warnings.warn('Outliers outside of the 1st-99th percentile range of historical cost data have been detected and removed',UserWarning)
            else:
                pass

            ###Gas Cost Equations
            # Get Intercept cost
            temp_gas_cost_df[nam.drill_cost] = gas_coef_df.at['Intercept', nam.coef]


            ###Apply USGS Province coef
            temp_gas_cost_df = temp_gas_cost_df.reset_index().merge(  # Perform USGS Province coef merge
                gas_coef_df[[nam.usgs_province_num_merge, nam.coef]],
                how='left',
                on=nam.usgs_province_num_merge).set_index('index')

            temp_gas_cost_df = temp_gas_cost_df.rename(columns={nam.coef: nam.usgs_province_coef})

            #Fill NA USGS Provinces with lowest level equivalent with multiplier where basin-level data is missing
            #Fill by district number
            temp_gas_cost_df[nam.usgs_province_coef] = temp_gas_cost_df[nam.usgs_province_coef].fillna(
                temp_gas_cost_df.groupby(nam.district_number)[nam.usgs_province_coef].transform('mean') * 1.50)

            #Fill by region number
            temp_gas_cost_df[nam.usgs_province_coef] = temp_gas_cost_df[nam.usgs_province_coef].fillna(
                temp_gas_cost_df.groupby(nam.region_number)[nam.usgs_province_coef].transform('mean') * 1.75)

            #Fill by resource type
            temp_gas_cost_df[nam.usgs_province_coef] = temp_gas_cost_df[nam.usgs_province_coef].fillna(
                temp_gas_cost_df.groupby(nam.resource_type)[nam.usgs_province_coef].transform('mean') * 2.0)

            #Calculate USGS Province coef cost
            temp_gas_cost_df[nam.drill_cost] = temp_gas_cost_df[nam.drill_cost] + temp_gas_cost_df[nam.usgs_province_coef]


            ###Apply State coef
            temp_gas_cost_df = temp_gas_cost_df.reset_index().merge(  # Perform USGS Province coef merge
                gas_coef_df[[nam.coef]],
                how='left',
                left_on=nam.state, right_index = True).set_index('index')

            temp_gas_cost_df = temp_gas_cost_df.rename(columns={nam.coef: nam.state_coef})

            # Fill NA states with lowest level equivalent with multiplier where state-level data is missing
            # Fill by district number
            
            temp_gas_cost_df[nam.state_coef] = temp_gas_cost_df[nam.state_coef].fillna(
                temp_gas_cost_df.groupby(nam.district_number)[nam.state_coef].transform('mean') * 1.50)

            # Fill by region number
            temp_gas_cost_df[nam.state_coef] = temp_gas_cost_df[nam.state_coef].fillna(
                temp_gas_cost_df.groupby(nam.region_number)[nam.state_coef].transform('mean') * 1.75)

            # Fill by resource type
            temp_gas_cost_df[nam.state_coef] = temp_gas_cost_df[nam.state_coef].fillna(
                temp_gas_cost_df.groupby(nam.resource_type)[nam.state_coef].transform('mean') * 2.0)

            # Calculate USGS Province coef cost
            temp_gas_cost_df[nam.drill_cost] = temp_gas_cost_df[nam.drill_cost] + temp_gas_cost_df[nam.state_coef]


            ###Apply Depth coefficients
            #temp_gas_cost_df['tot_depth'] = temp_gas_cost_df[nam.drill_depth_ft] + temp_gas_cost_df[nam.lateral_length_ft]
            #temp_gas_cost_df[nam.drill_cost] = temp_gas_cost_df[nam.drill_cost] + \
            #                                   (temp_gas_cost_df['tot_depth'] * gas_coef_df.at['tot_depth', nam.coef])
            temp_gas_cost_df[nam.drill_cost] = temp_gas_cost_df[nam.drill_cost] + \
                                               (temp_gas_cost_df[nam.drill_depth_ft] * gas_coef_df.at[nam.num_vfeet, nam.coef])
            temp_gas_cost_df[nam.drill_cost] = temp_gas_cost_df[nam.drill_cost] + \
                                               (temp_gas_cost_df[nam.lateral_length_ft] * gas_coef_df.at[nam.num_latlen, nam.coef])


            ###Convert Costs from Natural Log
            temp_gas_cost_df[nam.drill_cost] = np.exp(temp_gas_cost_df[nam.drill_cost])


            ###Track Outliers
            lower_bound = gas_coef_df.at[nam.hist_cost_quantile_01, nam.coef] * 0.5
            upper_bound = gas_coef_df.at[nam.hist_cost_quantile_99, nam.coef] * 1.5
            outlier_mask = (temp_gas_cost_df[nam.drill_cost] < lower_bound) | (
                        temp_gas_cost_df[nam.drill_cost] > upper_bound)
            temp_na_df = temp_gas_cost_df[temp_gas_cost_df[nam.drill_cost].isna()].copy()  # get na values
            temp_outlier_df = temp_gas_cost_df.copy()[outlier_mask]  # get outliers
            #temp_outlier_df = temp_outlier_df.append(temp_na_df)

            #Trigger warning to indicate outliers have been removed from the dataset
            if temp_outlier_df.shape[0] > 0:
                warnings.warn('Outliers outside of the 1st-99th percentile range of historical cost data have been detected and removed',UserWarning)
            else:
                pass

            ###Update master projects df
            project_df.update(temp_oil_cost_df)
            project_df.update(temp_gas_cost_df)


            ###Get Dryhole Cost (Category 1 - Tight/Shale, Category 2 - exploration conventional, Category 3 - development conventional)
            #Mask based on well type and assign dryhole rates
            tight_shale_mask = ((project_df[nam.well_type_number] == 2) | (self.projects[nam.well_type_number] >= 4))
            undisc_mask = project_df[nam.process_code] >= 16
            past_wells_mask = project_df[nam.past_wells] > 10

            #Shale & Tight Wells
            temp_df = project_df[tight_shale_mask].copy()
            temp_dryhole_rate = self.dryhole_rate.loc[self.dryhole_rate[nam.drill_category] == 1]
            temp_df = temp_df.reset_index().merge(temp_dryhole_rate, on=[nam.region_number, nam.resource_type], how='left').set_index('index')  # Perform USGS Province coef merge
            temp_df[nam.dry_hole_cost] = temp_df[nam.drill_cost].mul(0.33334) #Rystad reports that 1/3 of capex is drill cost while 2/3 is completion costs 11/08/2022
            temp_df[nam.dry_hole_cost] = temp_df[nam.dry_hole_cost].mul(temp_df[nam.dryhole_rate]) #Dryhole rates from OGSM
            project_df.update(temp_df)

            #Undiscovered Exploration Wells
            temp_df = project_df[undisc_mask & ~past_wells_mask].copy()
            temp_dryhole_rate = self.dryhole_rate.loc[self.dryhole_rate[nam.drill_category] == 2]
            temp_df = temp_df.reset_index().merge(temp_dryhole_rate, on=[nam.region_number, nam.resource_type], how='left').set_index('index')  # Perform USGS Province coef merge
            temp_df[nam.dry_hole_cost] = temp_df[nam.drill_cost].mul(0.33334) #Rystad reports that 1/3 of capex is drill cost while 2/3 is completion costs 11/08/2022
            temp_df[nam.dry_hole_cost] = temp_df[nam.dry_hole_cost].mul(temp_df[nam.dryhole_rate]) #Dryhole rates from OGSM
            project_df.update(temp_df)

            #Undiscovered Development Wells
            temp_df = project_df[undisc_mask & past_wells_mask].copy()
            temp_dryhole_rate = self.dryhole_rate.loc[self.dryhole_rate[nam.drill_category] == 3]
            temp_df = temp_df.reset_index().merge(temp_dryhole_rate, on=[nam.region_number, nam.resource_type], how='left').set_index('index')  # Perform USGS Province coef merge
            temp_df[nam.dry_hole_cost] = temp_df[nam.drill_cost].mul(0.33334) #Rystad reports that 1/3 of capex is drill cost while 2/3 is completion costs 11/08/2022
            temp_df[nam.dry_hole_cost] = temp_df[nam.dry_hole_cost].mul(temp_df[nam.dryhole_rate]) #Dryhole rates from OGSM
            project_df.update(temp_df)

            #Apply Year 1 & 2 Tech Rate Adjustment
            project_df[nam.drill_cost] = project_df[nam.drill_cost].mul((1 - project_df[nam.drill_tech]) ** (self.rest_curcalyr - self.zero_year))
            project_df[nam.dry_hole_cost] = project_df[nam.dry_hole_cost].mul((1 - project_df[nam.drill_tech]) ** (self.rest_curcalyr - self.zero_year))

            return project_df

        #Load Costs
        self.projects = load_costs(self.projects)
        self.projects_undiscovered = load_costs(self.projects_undiscovered)


        ###Simplified methodology for "Other EOR" Costs
        temp_df = self.projects.loc[self.projects[nam.process_code] == 11].copy()
        temp_df[nam.drill_cost] = temp_df[nam.drill_cost] * 1.5
        self.projects.update(temp_df)

        pass


    def startup_cost_adjustments(self):
        '''Reduces costs for projects with historical drilling to ensure they are selected for future drilling.

        Returns
        -------
        self.projects : df
            Master DataFrame containing all projects to be processed in current model year
        '''

        ###Cost Adjustments for wells with recent production
        #Wells that are already drilling should continue drilling
        #However, there are some areas where only exploration wells have been drilled
        #Tiered cost adjustments based on number of last year wells

        ###Oil
        #Wells with 1 well or more of production
        tier_one_mask = (self.projects[nam.hist_year_wells] >= 1) & (self.projects[nam.hist_year_wells] < 10) & (self.projects[nam.resource_type] == 'oil')
        temp_df = self.projects[tier_one_mask].copy()
        cadj = 1.0
        temp_df[nam.drill_cost] = temp_df[nam.drill_cost] * cadj
        temp_df[nam.dry_hole_cost] = temp_df[nam.dry_hole_cost] * cadj
        temp_df[nam.facility_capex_well] = temp_df[nam.facility_capex_well] * cadj
        self.projects.update(temp_df)

        #Wells with 10 wells or more of production
        tier_two_mask = (self.projects[nam.hist_year_wells] >= 10) & (self.projects[nam.hist_year_wells] < 50) & (self.projects[nam.resource_type] == 'oil')
        temp_df = self.projects[tier_two_mask].copy()
        cadj = 0.6
        temp_df[nam.drill_cost] = temp_df[nam.drill_cost] * cadj
        temp_df[nam.dry_hole_cost] = temp_df[nam.dry_hole_cost] * cadj
        temp_df[nam.facility_capex_well] = temp_df[nam.facility_capex_well] * cadj
        self.projects.update(temp_df)

        #Wells with 50 wells or more of production
        tier_three_mask = (self.projects[nam.hist_year_wells] >= 50) & (self.projects[nam.resource_type] == 'oil')
        temp_df = self.projects[tier_three_mask].copy()
        cadj = 0.3
        temp_df[nam.drill_cost] = temp_df[nam.drill_cost] * cadj
        temp_df[nam.dry_hole_cost] = temp_df[nam.dry_hole_cost] * cadj
        temp_df[nam.facility_capex_well] = temp_df[nam.facility_capex_well] * cadj
        self.projects.update(temp_df)


        ###Natural Gas
        tier_one_mask = (self.projects[nam.hist_year_wells] >= 1) & (self.projects[nam.hist_year_wells] < 5) & (self.projects[nam.resource_type] == 'gas')
        temp_df = self.projects[tier_one_mask].copy()
        cadj = 1.0
        temp_df[nam.drill_cost] = temp_df[nam.drill_cost] * cadj
        temp_df[nam.dry_hole_cost] = temp_df[nam.dry_hole_cost] * cadj
        temp_df[nam.facility_capex_well] = temp_df[nam.facility_capex_well] * cadj
        self.projects.update(temp_df)

        #Wells with 10 wells or more of production
        tier_two_mask = (self.projects[nam.hist_year_wells] >= 10) & (self.projects[nam.hist_year_wells] < 50) & (self.projects[nam.resource_type] == 'gas')
        temp_df = self.projects[tier_two_mask].copy()
        cadj = 0.6
        temp_df[nam.drill_cost] = temp_df[nam.drill_cost] * cadj
        temp_df[nam.dry_hole_cost] = temp_df[nam.dry_hole_cost] * cadj
        temp_df[nam.facility_capex_well] = temp_df[nam.facility_capex_well] * cadj
        self.projects.update(temp_df)

        #Wells with 50 wells or more of production
        tier_three_mask = (self.projects[nam.hist_year_wells] >= 50) & (self.projects[nam.resource_type] == 'gas')
        temp_df = self.projects[tier_three_mask].copy()
        cadj = 0.3
        temp_df[nam.drill_cost] = temp_df[nam.drill_cost] * cadj
        temp_df[nam.dry_hole_cost] = temp_df[nam.dry_hole_cost] * cadj
        temp_df[nam.facility_capex_well] = temp_df[nam.facility_capex_well] * cadj
        self.projects.update(temp_df)

    pass


    ###START OF FUNCTIONS RUN EVERY YEAR###


    def load_prices(self):
        """Calculate average crude, natural gas, and ngpl prices for Onshore variables by region.

            * Crude prices are derived from LFMM regional wellhead prices
            * Natural gas prices are derived from NGMM regional wellhead prices
            * NGPL prices are derived from LFMM aggregate national price
        iteration now so no need to do this)

        Returns
        -------
        self.projects : df
            Master DataFrame containing all projects to be processed in current model year

        self.projects_undiscovered : df
            DataFrame of undiscovered projects
        """
        #Get average years
        avg_years = list(range(self.rest_curcalyr-self.averaging_years , self.rest_curcalyr + 1))


        ###Crude
        # Continuous and EOR/ASR projects
        temp_price = pd.merge(self.projects[nam.region_number],
                              self.parent.reg_crude_price[avg_years],
                              left_on=nam.region_number,
                              right_index=True,
                              how='left')
        temp_price[nam.crude_price] = temp_price[avg_years].mean(axis=1)
        self.projects[nam.crude_price] = temp_price[nam.crude_price].copy()
        self.projects[nam.crude_price_lag] = temp_price[avg_years[0]].copy()

        #Undiscovered projects
        temp_price = pd.merge(self.projects_undiscovered[nam.region_number],
                              self.parent.reg_crude_price[avg_years],
                              left_on=nam.region_number,
                              right_index=True,
                              how='left')
        temp_price[nam.crude_price] = temp_price[avg_years].mean(axis=1)
        self.projects_undiscovered[nam.crude_price] = temp_price[nam.crude_price].copy()
        self.projects_undiscovered[nam.crude_price_lag] = temp_price[avg_years[0]].copy()


        ###Natural Gas
        #Continuous and EOR/ASR projects
        temp_price = pd.merge(self.projects[nam.district_number],
                              self.parent.dist_natgas_price[avg_years],
                              left_on=nam.district_number,
                              right_index=True,
                              how='left')
        temp_price[nam.natgas_price] = temp_price[avg_years].mean(axis=1)

        #Apply regional prices if district price = 0
        temp_price_mask = temp_price[nam.natgas_price] == 0
        temp_reg_price = pd.merge(self.projects.loc[temp_price_mask, nam.region_number],
                              self.parent.reg_natgas_price[avg_years],
                              left_on=nam.region_number,
                              right_index=True,
                              how='left')
        temp_reg_price[nam.natgas_price] = temp_reg_price[avg_years].mean(axis=1)
        temp_price.update(temp_reg_price)

        #Apply Prices
        self.projects[nam.natgas_price] = temp_price[nam.natgas_price].copy()
        self.projects[nam.natgas_price_lag] = temp_price[avg_years[0]].copy()


        #Undiscovered projects
        temp_price = pd.merge(self.projects_undiscovered[nam.region_number],
                              self.parent.reg_natgas_price[avg_years],
                              left_on=nam.region_number,
                              right_index=True,
                              how='left')
        temp_price[nam.natgas_price] = temp_price[avg_years].mean(axis=1)

        #Apply regional prices if district price = 0
        temp_price_mask = temp_price[nam.natgas_price] == 0
        temp_reg_price = pd.merge(self.projects_undiscovered.loc[temp_price_mask, nam.region_number],
                              self.parent.reg_natgas_price[avg_years],
                              left_on=nam.region_number,
                              right_index=True,
                              how='left')
        temp_reg_price[nam.natgas_price] = temp_reg_price[avg_years].mean(axis=1)
        temp_price.update(temp_reg_price)

        #Apply Prices
        self.projects_undiscovered[nam.natgas_price] = temp_price[nam.natgas_price].copy()
        self.projects_undiscovered[nam.natgas_price_lag] = temp_price[avg_years[0]].copy()


        ###NGPLs
        #Get NGPL price from LFMM PMORE_PLGINPF U.S. (11)
        self.projects[nam.ngpl_price] = self.parent.rest_plginpf.at[(11, self.rest_curcalyr), 'value'] * \
                                        (self.parent.rest_cflgq.at[self.rest_curcalyr, 'value']) / 42
        self.projects_undiscovered[nam.ngpl_price] = self.parent.rest_plginpf.at[(11, self.rest_curcalyr), 'value'] * \
                                                    (self.parent.rest_cflgq.at[self.rest_curcalyr, 'value']) / 42

        pass


    def calculate_drilling_constraints(self):
        """Calculate rig and footage constraints by year based on oil price.

        Returns
        -------
        self.exp_const_ratio : float
            Ratio of drilling constraint capacity assigned to undiscovered production

        self.dev_const_ratio : float
            Ratio of drilling constraint capacity assigned to developing production

        self.rig_constraint : df
            DataFrame of regional rig constraints

        self.footage_constraint : df
            DataFrame of regional footage constraints
        """
        ###Get ratio of drilling constraints applied to undiscovered vs. developing projects
        self.exp_const_ratio = self.constraint_params.at[nam.exp_ratio, nam.value]
        self.dev_const_ratio = 1 - self.constraint_params.at[nam.exp_ratio, nam.value]


        ###Rig Constraint
        self.rig_constraint = pd.DataFrame(columns = [nam.region, nam.region_rig_constraint]).set_index(nam.region)

        #Apply rig constraint equation by region
        region_list = ['region_1', 'region_2', 'region_3', 'region_4', 'region_5', 'region_6', 'region_7']
        for region in region_list:

            #Set Intercept
            self.rig_constraint.loc[region] = self.rig_constraint_eq.at[('Intercept', nam.coef)]

            #Apply region coefficient
            self.rig_constraint.loc[region] = self.rig_constraint.loc[region] + self.rig_constraint_eq.at[(region, nam.coef)]

            # Don't allow low price case to constrain drilling rig availability to the same extent that Brent price is dropped
            if (self.parent.low_price_case) & (region == 'region_1'):
                brent_price = self.parent.rest_brent_price.at[self.rest_curcalyr, nam.value] * 1.55
            else:
                brent_price = self.parent.rest_brent_price.at[self.rest_curcalyr, nam.value]

            #Apply Brent coefficient
            ln_brent_price = np.log(brent_price)
            self.rig_constraint.loc[region] = self.rig_constraint.loc[region] + ln_brent_price * self.rig_constraint_eq.at[(nam.ln_brent_price, nam.coef)]

        #Set floor on number of rigs per region
        self.rig_constraint[self.rig_constraint < 5] = 5 #Set floor on number of rigs

        #Reformat region number column so that it matches other tables
        self.rig_constraint[nam.region_number] = self.rig_constraint.index
        self.rig_constraint[nam.region_number] = self.rig_constraint[nam.region_number].map(lambda x: x.replace('region_', '')).astype(int)


        ###Footage Constraint
        self.footage_constraint = pd.DataFrame(columns=[nam.region, nam.region_footage_constraint]).set_index(nam.region)

        #Apply footage constraint equation by region
        for region in region_list:
            self.footage_constraint.loc[region] = self.footage_constraint_eq.at[('Intercept', nam.coef)]

            #Apply region coefficient
            self.footage_constraint.loc[region] = self.footage_constraint.loc[region] + self.footage_constraint_eq.at[(region, nam.coef)]

            # Apply Brent coefficient
            if (self.parent.low_price_case) & (region == 'region_1'):
                brent_price = self.parent.rest_brent_price.at[self.rest_curcalyr, nam.value] * 1.55
            else:
                brent_price = self.parent.rest_brent_price.at[self.rest_curcalyr, nam.value]

            ln_brent_price = np.log(brent_price)
            self.footage_constraint.loc[region] = self.footage_constraint.loc[region] + (ln_brent_price * self.footage_constraint_eq.at[(nam.ln_brent_price, nam.coef)])

        #Apply multiplier to transform from monthly values to annual values
        self.footage_constraint = self.footage_constraint.mul(12)

        #Set floor on footage
        self.footage_constraint[self.footage_constraint < 1000000] = 1000000 #Set floor on footage

        #Reformat region number column so that it matches other tables
        self.footage_constraint[nam.region_number] = self.footage_constraint.index
        self.footage_constraint[nam.region_number] = self.footage_constraint[nam.region_number].map(lambda x: x.replace('region_', '')).astype(int)

        pass


    def calculate_capital_constraint(self):
        """Calculate capital constraint by year based on oil price.

            * Available dataset only accounts for public company capital investment, which is ~60% of total, so constraint value is
              adjusted to account for private company investment as well
            * Available dataset doesn't split Offshore and Alaska production from domestic production, Rystad reports
              Offshore investment is approx 15% of US total, Alaska is approx 5%

        Returns
        -------
        self.projects : df
            Master DataFrame containing all projects to be processed in current model year

        self.projects_undiscovered : df
            DataFrame of undiscovered projects
        """
        capital_constraint = 0

        # Apply Intercept
        capital_constraint = capital_constraint + self.capital_constraint_eq.at['Intercept', nam.coef]

        #Apply LN Brent price coefficient
        brent_price = self.parent.rest_brent_price.at[self.rest_curcalyr, nam.value]
        ln_brent_price = np.log(brent_price)
        capital_constraint = capital_constraint + ln_brent_price * self.capital_constraint_eq.at[nam.ln_brent_price, nam.coef]

        # Apply LN Brent price lagged coefficient
        brent_price_lagged = self.parent.rest_brent_price.at[(self.rest_curcalyr - 1), 'value']
        ln_brent_price_lagged = np.log(brent_price_lagged)
        capital_constraint = capital_constraint + ln_brent_price_lagged * self.capital_constraint_eq.at[nam.ln_brent_price_lagged, nam.coef]

        #Add Capital constraint and cumulative capital columns to master projects DataFrame and undiscovered projects DataFrame
        self.projects[nam.capital_constraint] = capital_constraint / 0.6 #Divide by 0.6 because constraint only includes public company investment
        self.projects[nam.capital_constraint] = capital_constraint * 0.8 #Offshore investment is approx 15% of US total, Alaska is approx 5% (Rystad)
        self.projects[nam.cumulative_capital] = 0.0

        self.projects_undiscovered[nam.capital_constraint] = capital_constraint / 0.6  # Divide by 0.6 because constraint only includes public company investment
        self.projects_undiscovered[nam.capital_constraint] = capital_constraint * 0.8 #Offshore investment is approx 15% of US total, Alaska is approx 5% (Rystad)
        self.projects_undiscovered[nam.cumulative_capital] = 0.0

        pass


    def set_undiscovered_drilling_params(self):
        """Set base undiscovered drilling assumptions.

        Returns
        -------
        self.projects_undiscovered : df
            DataFrame of undiscovered projects
        """
        #Drilling Assumptions
        #well_decline_limit is the basis for declining productivity, max_wells is the absolute maximum number of wells that can be drilled
        self.projects_undiscovered[nam.well_decline_limit] = (self.projects_undiscovered[nam.total_pattern_size_acres] /
                                                      self.projects_undiscovered[nam.std_pattern_size_acres]).round(decimals=0)

        self.projects_undiscovered[nam.max_lat_len_spacing] = self.projects_undiscovered[nam.lateral_length_ft] / 3000 * 40
        self.projects_undiscovered[nam.min_well_spacing] = self.projects_undiscovered[nam.max_lat_len_spacing].clip(lower = 40.0)
        self.projects_undiscovered[nam.max_wells] = (self.projects_undiscovered[nam.total_pattern_size_acres] /
                                                   self.projects_undiscovered[nam.min_well_spacing]).round(decimals=0)

        #Set Max Drill Rate
        self.projects_undiscovered[nam.max_drill_rate] = self.projects_undiscovered['max_annual_wells']
        temp_unconv_df_mask = self.projects_undiscovered[nam.process_code].isin([20,21,22]).copy()
        temp_unconv_df = self.projects_undiscovered[temp_unconv_df_mask].copy()
        temp_unconv_df[nam.max_drill_rate] = (temp_unconv_df[nam.total_pattern_size_acres] / 640 * 0.08)  # multipliers are assumptions
        self.projects_undiscovered.update(temp_unconv_df)

        #Get Total Patterns Remaining
        self.projects_undiscovered[nam.totpat] = self.projects_undiscovered[nam.well_decline_limit] - self.projects_undiscovered[nam.past_wells]

        #Set Drill EQ constraints
        self.projects_undiscovered[nam.min_drill_totpat_pct] = self.projects_undiscovered[nam.min_annual_wells] / self.projects_undiscovered[nam.totpat]
        self.projects_undiscovered[nam.max_year_drill_pct] = self.projects_undiscovered[[nam.max_annual_pct_dev, nam.min_drill_totpat_pct]].max(axis=1)
        self.projects_undiscovered.loc[self.projects_undiscovered[nam.max_year_drill_pct] > 0.3, nam.max_year_drill_pct] = 0.3
        self.projects_undiscovered[nam.max_year_drill_pct] = self.projects_undiscovered[nam.max_year_drill_pct].replace([np.inf, -np.inf, np.nan], 0.06)

        pass


    def calculate_undiscovered_drilling(self):
        """Calculate drilling required to explore and locate an undiscovered project.

        Same methodology as regular drilling equation, but used to determine number of wells needed to be drilled to discover a project.

        * Drilling limit is reduced in Colorado due to 2020 law increasing drilling setbacks
        * *on_next_wells* from **drilling_equations.py** is called to determine number of wells that need to be drilled for a well discovery
        * Calculate project dryhole rate for cost equations

        Returns
        -------
        self.projects_undiscovered : df
            DataFrame of undiscovered projects
        """
        #Table Setup
        prod_range = list(range(self.evaluation_years))
        self.undiscovered_drilling[prod_range] = 0

        #Adjust resources in Colorado to account for increase in setback reqs (2020)
        #https://coloradonewsline.com/2020/09/15/increased-setbacks-a-ban-on-drilling-a-new-study-shows-otherwise/
        co_mask = self.projects_undiscovered[nam.state] == 'CO'
        temp_df = self.projects_undiscovered[[nam.well_decline_limit,nam.max_wells]][co_mask].copy()
        temp_df[nam.well_decline_limit] = temp_df[nam.well_decline_limit].mul(0.9)
        temp_df[nam.max_wells] = temp_df[nam.max_wells].mul(0.9)
        self.projects_undiscovered.update(temp_df)

        # Legacy Lat-length test from Dana to test for lateral-length density
        self.projects_undiscovered[nam.lat_len_test] = (self.projects_undiscovered[nam.total_pattern_size_acres] / \
                                                      self.projects_undiscovered[nam.lateral_length_ft] * 870 / 43560) * 0.5

        #Calculate Drilling for Undiscovered Projects
        self.undiscovered_drilling = self.projects_undiscovered.copy().apply(lambda x:
                                                            drill_eq.on_next_wells(x[nam.well_decline_limit],
                                                                           x[nam.max_wells],
                                                                           0.0, #Set historical wells to 0
                                                                           0.0, #Set last year wells to 0
                                                                           x[nam.max_drill_rate],
                                                                           x[nam.max_drill_rate_frac],
                                                                           self.drill_predecline,
                                                                           self.drill_ramp_up,
                                                                           x[nam.crude_price],
                                                                           x[nam.natgas_price],
                                                                           x[nam.prime_fuel_type_number],
                                                                           self.base_oil_prc,
                                                                           self.base_gas_prc,
                                                                           x['OP1'],
                                                                           x['GP1'],
                                                                           x[nam.max_year_drill_pct],
                                                                           self.rest_curcalyr,
                                                                           self.parent.low_price_case,
                                                                           undiscovered_drill_flag = True), axis=1)

        # Get undiscovered project dryholes
        temp_dryholes_df = self.projects_undiscovered[[nam.region_number, nam.well_type, nam.resource_type]].copy()
        temp_dryhole_rate = self.dryhole_rate.loc[self.dryhole_rate[nam.drill_category] == 2]  # Category 2 = exploration undiscovered projects
        temp_dryholes_df = temp_dryholes_df.merge(temp_dryhole_rate, how='left', on=[nam.region_number, nam.resource_type])
        temp_dryholes_df[nam.wells] = self.undiscovered_drilling
        temp_dryholes_df[nam.dryholes] = (temp_dryholes_df[nam.wells].mul(temp_dryholes_df[nam.dryhole_rate])).round()

        #Apply drilling + dryholes to projected wells column
        self.projects_undiscovered[nam.projected_und_wells] = self.undiscovered_drilling.copy() + temp_dryholes_df[nam.dryholes]

        pass


    def load_undiscovered_projects(self):
        """Applies rig constraint to load in undiscovered projects.

        * Wells/rig and number of rigs available by region are calculated in the "onshore_constraints" preprocesser
        * Projected wells are then split amongst the regions and loaded into the master projects list in order based upon
          the annual rig constraint tied to brent price
        * A minimum of 15% of available rigs or 5 rigs is assigned to each region
        * Once projects are discovered they are added to the master projects list and removed from the undiscovered projects list
        * This function also reformats undiscovered projects to be added to the master projects list, and saves exploratory wells
          to track the cumulative annual well count

        Returns
        -------
        self.projects : df
            Master DataFrame containing all projects to be processed in current model year

        self.discovery_order : df
            Results of a Monte Carlo simulation to determine discovery order for undiscovered vertical projects in
            the *projects_undiscovered* df

        self.exploratory_wells : df
            DataFrame of exploratory wells

        self.parent.hsm_vars.on_projects_discovered : df
            DataFrame of discovered projectst that have not yet been assigned to the master project df (duplicates)

        """
        ###Calculate projects that can be discovered given exploration rig constraint
        #Merge to project discovery order (note that in the order some projects can be discovered multiple times, meaning we get duplicates
        temp_undiscovered = pd.merge(self.discovery_order[[nam.resid, nam.item]],
                                     self.projects_undiscovered,
                                     how = 'left',
                                     on = nam.resid)
        temp_undiscovered.index = self.discovery_order.index
        temp_undiscovered = temp_undiscovered.dropna()

        #Sort projects by item
        temp_undiscovered = temp_undiscovered.sort_values(nam.item)

        ###Get well counts by undiscovered projects
        temp_well = temp_undiscovered[[nam.region_number, nam.projected_und_wells]].copy()
        temp_well.columns = [nam.region_number, nam.region_cum_well_count]
        temp_well[nam.region_number] = temp_well[nam.region_number].astype(int)

        #Convert well counts to number of rigs
        temp_rig = temp_well.copy()
        temp_rig = temp_rig.reset_index().merge(self.wells_per_rig, how='left', left_on=[nam.region_number],
                                                  right_on=[nam.region]).set_index('index')  # Merge in number of wells drilled/year per rig by region
        temp_rig[nam.rigs] = temp_rig[nam.region_cum_well_count] * (1 / temp_rig['wells_per_rig']) # Apply rig efficiency rate

        #Get cumulative sum of rigs by region and apply to temp undiscovered df
        temp_undiscovered[nam.region_cum_rig_count] = temp_rig.groupby(nam.region_number)[nam.rigs].cumsum()

        #Create temp rig constraint table and apply undiscovered well constraint ratio as portion of total drilling (15% of rigs are assigned to undiscovered projects)
        temp_rig_constraint = self.rig_constraint.copy()
        temp_rig_constraint[nam.region_rig_constraint] = temp_rig_constraint[nam.region_rig_constraint].mul(self.exp_const_ratio)

        #Set rig constraint
        temp_undiscovered = temp_undiscovered.reset_index().merge(temp_rig_constraint, how = 'left', on = nam.region_number)
        temp_undiscovered[nam.region_rig_constraint] = np.maximum(temp_undiscovered[nam.region_rig_constraint], 3) #Set rig floor at 3 rigs


        ###Identify discovered projects and prepare discovered projects for merge to master projects list
        #Mask for discovered projects
        discovered_mask = (temp_undiscovered[nam.region_rig_constraint] - temp_undiscovered[nam.region_cum_rig_count]) >= 0
        discovered_df = temp_undiscovered[discovered_mask].copy()

        #Fill missing vertical depth with "Depth" column
        try:
            discovered_df[nam.drill_depth_ft] = discovered_df[nam.drill_depth_ft].fillna(discovered_df[nam.depth])
        except:
            pass

        #Fill missing latlen with 0.0
        discovered_df[nam.lateral_length_ft] = discovered_df[nam.lateral_length_ft].fillna(0.0)

        #Get USGS province number
        discovered_df[nam.usgs_province_num] = discovered_df[nam.resid].str[4:6]

        #Add Discovered Year
        discovered_df[nam.discovery_year] = self.rest_curcalyr

        #Drop unused columns
        discovered_df = discovered_df.drop([nam.index, nam.item, 'Latnum'], axis = 1)

        #Set maximum number of new projects that can be discovered/year at 300 (this is a runtime consideration)
        discovered_df = discovered_df.head(300)


        ###Add discovered projects to master projects list and pull from undiscovered projects
        #Add discovered projects to master projects list
        self.projects_discovered = pd.concat([self.projects_discovered, discovered_df])

        #Set index for merge to master projects list
        self.projects_discovered = self.projects_discovered.reset_index(drop = True)
        self.projects_discovered.index = self.projects_discovered.index + self.projects.index.max() + 1
        self.projects_discovered.index = list(self.projects_discovered.index) #convert from range type to int type index to avoid indexing issues

        #Drop duplicates for merge to projects
        temp_df = self.projects_discovered.copy().drop_duplicates(keep = 'first') #Don't load multiple duplicates
        temp_df = temp_df.drop_duplicates(nam.resid, keep = 'first') #Don't load multiple duplicates
        resid_mask = temp_df[nam.resid].isin(self.projects[nam.resid])

        #Only add unique discovered projects, this reduces runtime
        #Every cycle exhausted discovered projects are dropped from the project list
        unique_discovered_df = temp_df[~resid_mask]
        self.projects = pd.concat([self.projects, unique_discovered_df], ignore_index = False)

        if self.rest_curcalyr >= self.parent.history_year + 1:
            index_dif = self.projects[~self.projects.index.isin(self.project_co2_inj.index)].index
            self.project_co2_inj = pd.concat([self.project_co2_inj, pd.DataFrame(0, index=index_dif, columns = self.project_co2_inj.columns)], ignore_index=True)
            self.project_co2_recy = pd.concat([self.project_co2_recy, pd.DataFrame(0, index=index_dif, columns = self.project_co2_recy.columns)], ignore_index=True)

        #Remove discovered projects from the discovery order
        discovered_df[nam.selected] = 1
        discovered_df = discovered_df.reset_index(drop = True)
        discovered_df.index = discovered_df.index + self.discovery_order.index[0]
        self.discovery_order = self.discovery_order.reset_index().merge(discovered_df[[nam.selected]],
                                                           how = 'left',
                                                           left_index = True,
                                                           right_index = True).set_index('index')
        remove_mask = self.discovery_order[nam.selected] == 1
        self.discovery_order = self.discovery_order[~remove_mask].copy()
        self.discovery_order = self.discovery_order.drop(['selected'], axis = 1)

        #Remove discovered projects from discovered projects df
        remove_mask = resid_mask.loc[resid_mask == False]
        remove_mask = remove_mask.index
        self.projects_discovered = self.projects_discovered.loc[~self.projects_discovered.index.isin(remove_mask)]


        ###Assign exploration wells
        temp_exploratory_df = discovered_df[[nam.hsm_index,
                                             nam.resid,nam.process_code,
                                             nam.district_number,
                                             nam.region_number,
                                             nam.well_type_number,
                                             nam.play,
                                             nam.well_decline_limit,
                                             nam.past_wells]].copy()
        temp_exploratory_df[nam.year_production_start] = self.rest_curcalyr
        temp_exploratory_df[list(range(self.zero_year, self.parent.final_aeo_year + 1))] = 0
        temp_exploratory_df[self.rest_curcalyr] = discovered_df[nam.projected_und_wells]
        self.exploratory_wells = pd.concat([self.exploratory_wells, temp_exploratory_df], ignore_index = False)

        pass


    def calculate_production(self):
        """Assigns production of relevant resources for projects.

        * Adjusts production units from input file
        * Takes in input file production values and applies unit transformations and calculations for NGPLS and CO2 recycling
        * Reassigns production df indices every model year to match self.projects index

        Returns
        -------
        self.project_crude_production : df
            DataFrame of model year project crude oil production

        self.project_natgas_production : df
            DataFrame of model year project natural gas production

        self.project_ngpl_production : df
            DataFrame of model year project NGPL production

        self.project_water_production : df
            DataFrame of model year project water production

        self.project_co2_inj : df
            DataFrame of model year project CO2 injected
        """
        #Set production ranges
        prod_range = list(range(self.evaluation_years))
        crude_range = [('OP' + str(x + 1)) for x in prod_range]
        natgas_range = [('GP' + str(x + 1)) for x in prod_range]
        water_range = [('WP' + str(x + 1)) for x in prod_range]
        co2_range = [('II' + str(x + 1)) for x in prod_range]


        #Create tables (have to redo every year since new discovered projects are loaded in)
        self.project_crude_production   = pd.DataFrame()
        self.project_natgas_production  = pd.DataFrame()
        self.project_ngpl_production    = pd.DataFrame()
        self.project_water_production   = pd.DataFrame()
        self.project_co2_inj            = pd.DataFrame()
        self.project_co2_recy           = pd.DataFrame()

        # Convert production units
        self.project_crude_production[prod_range] = self.projects[crude_range].copy() * 1000
        self.project_natgas_production[prod_range] = self.projects[natgas_range].copy() * 1000
        self.project_ngpl_production[prod_range] = self.projects[natgas_range].copy().mul(self.projects[nam.ngpl], axis = 'index')
        self.project_water_production[prod_range] = self.projects[water_range].copy() * 1000
        self.project_co2_inj[prod_range] = self.projects[co2_range].copy()

        #Calculate CO2 Recycled for CO2 use
        self.project_co2_recy[prod_range] = self.projects[co2_range]
        for column in self.project_co2_recy.columns:
            recy_factor = (0.00003*((column+1)**3))+(-0.0025*((column+1)**2))+(0.0615*(column+1))
            self.project_co2_recy[column] = self.project_co2_recy[column] / 1.2 * recy_factor
        self.project_co2_recy[prod_range] = self.project_co2_recy[prod_range]

        #Set indices to match current index order following sorting by NPV/Profitability
        self.project_crude_production = self.project_crude_production.reindex(self.projects.index)
        self.project_natgas_production[prod_range] = self.project_natgas_production.reindex(self.projects.index)
        self.project_ngpl_production[prod_range] = self.project_ngpl_production.reindex(self.projects.index)
        self.project_water_production[prod_range] = self.project_water_production.reindex(self.projects.index)
        self.project_co2_inj[prod_range] = self.project_co2_inj.reindex(self.projects.index)
        self.project_co2_recy[prod_range] = self.project_co2_recy.reindex(self.projects.index)

        pass


    def calculate_prod_tech_improvement(self):
        """Calculates project production technology improvement.

        * Technology improvement rate is tiered with projects in areas without significant past production receiving a higher technology improvement rate
        * For the first 5 model years the tier 2 tech rate is doubled for projects that don't have past drilling
          (assumes it takes time to figure out where the best resources are)
        * Then assumed that productivity increases will be the same across project types
        * For highly productive regions (i.e. the Haynesville) there is a manual function for assigning the tier-1 tech rate to "emerging"
          type projects

        Returns
        -------
        self.project_crude_production : df
            DataFrame of model year project crude oil production

        self.project_natgas_production : df
            DataFrame of model year project natural gas production

        self.project_ngpl_production : df
            DataFrame of model year project NGPL production
        """

        ###Apply technology improvement
        #Tier 2 Mask
        prod_tier_mask = self.projects[nam.prod_tier] == 'E' #'E' prod tier signifies "Emerging"
        play_exempt_mask = self.projects[nam.play].isin([4774,4775]) #Pull out plays that are responding too aggresively
        tier_2_mask = prod_tier_mask & ~play_exempt_mask #& past_wells_mask

        #Create tech tier arrays
        tier_1_tech_df = self.projects[~tier_2_mask]
        tier_1_tech = tier_1_tech_df[nam.tier_1_eur_tech]

        tier_2_tech_df = self.projects[tier_2_mask]
        tier_2_tech = tier_2_tech_df[nam.tier_2_eur_tech]

        #Assign year multiplier
        year = self.rest_curcalyr - self.zero_year

        #Split tech rate into two tiers, tier-1 is the standard tech rate, tier-2 is the tech rate for undeveloped resources
        ###Apply tier 1 tech
        #Crude
        temp_df = self.project_crude_production[~tier_2_mask].copy()
        temp_df = temp_df.mul(((1 + tier_1_tech) ** year), axis = 0)
        self.project_crude_production.update(temp_df)

        #Natural Gas
        temp_df = self.project_natgas_production[~tier_2_mask].copy()
        temp_df = temp_df.mul(((1 + tier_1_tech) ** year), axis = 0)
        self.project_natgas_production.update(temp_df)

        #NGPLs
        temp_df = self.project_ngpl_production[~tier_2_mask].copy()
        temp_df = temp_df.mul(((1 + tier_1_tech) ** year), axis = 0)
        self.project_ngpl_production.update(temp_df)


        ###Apply tier 2 tech
        #Set year condition so that doubled tier 2 tech rate is only applied for first 5 model years (ramp-up time)
        if self.rest_curcalyr <= self.zero_year + 5:
            tier_2_tech = tier_2_tech * 2

            #Crude
            temp_df = self.project_crude_production[tier_2_mask].copy()
            temp_df = temp_df.mul(((1 + tier_2_tech) ** year), axis=0)
            self.project_crude_production.update(temp_df)

            #Natural Gas
            temp_df = self.project_natgas_production[tier_2_mask].copy()
            temp_df = temp_df.mul(((1 + tier_2_tech) ** year), axis=0)
            self.project_natgas_production.update(temp_df)

            #NGPLs
            temp_df = self.project_ngpl_production[tier_2_mask].copy()
            temp_df = temp_df.mul(((1 + tier_2_tech) ** year), axis=0)
            self.project_ngpl_production.update(temp_df)

        else:
            # Crude
            temp_df = self.project_crude_production[tier_2_mask].copy()
            temp_df = temp_df.mul(((1 + tier_2_tech * 2) ** 5), axis=0)
            temp_df = temp_df.mul(((1 + tier_2_tech) ** (year)), axis=0)
            self.project_crude_production.update(temp_df)

            # Natural Gas
            temp_df = self.project_natgas_production[tier_2_mask].copy()
            temp_df = temp_df.mul(((1 + tier_2_tech * 2) ** 5), axis=0)
            temp_df = temp_df.mul(((1 + tier_2_tech) ** (year)), axis=0)
            self.project_natgas_production.update(temp_df)

            # NGPLs
            temp_df = self.project_ngpl_production[tier_2_mask].copy()
            temp_df = temp_df.mul(((1 + tier_2_tech * 2) ** 5), axis=0)
            temp_df = temp_df.mul(((1 + tier_2_tech) ** (year)), axis=0)
            self.project_ngpl_production.update(temp_df)

        pass


    def set_drilling_params(self):
        """Set base drilling assumptions.

            * Set undiscovered projects last year of drilling to 0
            * Two drilling limits are calculated:
                1. well_decline_limit - A factor of total acreage/average well spacing, which determines when wells start to experience
                   productivity decline (70% of well decline limit)
                2. max_wells - The maximum wells that can be drilling which is a factor of total acreage/minimum well spacing, which is
                   the hard maximum on the number of wells that can be drilled in a project
            * The project maximum drilling rates are calculated as a maximum of the following two equations:
                1. Last year drilling * 1.3
                2. Total acreage / 640 * a production factor (this factor is rest every year as an analyst judgement of between 0.08 - 0.1)
            * A hard ceiling on drilling is set at 200 wells (which can then be adjusted upward in the drilling equation based on price)
            * The number of remaining wells to be drilled is calculated as a factor of max_wells / past_wells
            * A cap on the percentage of available wells that can be drilled is set based on a ceiling of 30%, and the maximum of the following two values:
                1. hist_year_wells / max_wells
                2. 3% - 6% depending on project type

        Returns
        -------
        self.projects : df
            Master DataFrame containing all projects to be processed in current model year
        """
        #Drilling Assumptions
        #Set undiscovered projects last year drilling to 0
        self.projects[nam.last_year_drilling] = self.projects[nam.last_year_drilling].fillna(0.0)

        #well_decline_limit is the basis for declining productivity, max_wells is the absolute maximum number of wells that can be drilled
        self.projects[nam.well_decline_limit] = (self.projects[nam.total_pattern_size_acres] / self.projects[nam.std_pattern_size_acres]).round(decimals=0)
        self.projects[nam.max_lat_len_spacing] = self.projects[nam.lateral_length_ft] / 3000 * 40
        self.projects[nam.min_well_spacing] = self.projects[nam.max_lat_len_spacing].clip(lower=40.0)
        self.projects[nam.max_wells] = (self.projects[nam.total_pattern_size_acres] / (self.projects[nam.min_well_spacing])).round(decimals=0)

        #Set Max Drill Rate
        self.projects[nam.max_drill_rate] = self.projects['max_annual_wells']
        temp_unconv_df_mask = self.projects[nam.process_code].isin([12,13,14,20,21,22]).copy()
        temp_unconv_df = self.projects[temp_unconv_df_mask].copy()
        temp_unconv_df[nam.max_drill_rate] = np.maximum((temp_unconv_df[nam.hist_year_wells] * 1.3), (
                temp_unconv_df[nam.total_pattern_size_acres] / 640 * 0.09))  # multipliers are assumptions
        self.projects.update(temp_unconv_df)

        # Adjust Permian production
        # Permian area plays want to continue growing


        #Get Total Patterns Remaining
        self.projects[nam.totpat] = self.projects[nam.well_decline_limit] - self.projects[nam.past_wells]

        #Get cumulative production
        self.projects[nam.cum_oil_prod] = self.project_crude_production.sum(axis = 1)
        self.projects[nam.cum_gas_prod] = self.project_natgas_production.sum(axis = 1)

        #Get total resources
        self.projects[nam.remaining_oil_resources] = self.projects[nam.cum_oil_prod].mul(self.projects[nam.totpat])
        self.projects[nam.remaining_gas_resources] = self.projects[nam.cum_gas_prod].mul(self.projects[nam.totpat])

        #Get Drilling EQ Constraint parameters
        self.projects[nam.min_drill_totpat_pct] = self.projects[nam.min_annual_wells] / self.projects[nam.max_wells]
        self.projects[nam.max_year_drill_pct] = self.projects[[nam.max_annual_pct_dev,nam.min_drill_totpat_pct]].max(axis = 1)
        self.projects[nam.hist_well_drill_pct] = (self.projects[nam.hist_year_wells] / self.projects[nam.max_wells])
        self.projects[nam.max_year_drill_pct] = self.projects[[nam.max_annual_pct_dev, nam.hist_well_drill_pct]].max(axis=1)

        #Set rules for maximum remaining % of wells that can be drilled/year and process-code-specific ratios
        self.projects.loc[self.projects[nam.max_year_drill_pct] > 0.3, nam.max_year_drill_pct] = 0.3
        self.projects.loc[self.projects[nam.process_code] == 12, nam.max_year_drill_pct] = self.projects[[nam.max_year_drill_pct, nam.hist_well_drill_pct]].max(axis = 1)
        self.projects.loc[self.projects[nam.process_code] == 13, nam.max_year_drill_pct] = self.projects[[nam.max_year_drill_pct, nam.hist_well_drill_pct]].max(axis = 1)
        self.projects.loc[self.projects[nam.process_code] == 14, nam.max_year_drill_pct] = self.projects[[nam.max_year_drill_pct, nam.hist_well_drill_pct]].max(axis = 1)
        self.projects.loc[self.projects[nam.process_code] == 20, nam.max_year_drill_pct] = self.projects[[nam.max_year_drill_pct, nam.hist_well_drill_pct]].max(axis = 1)
        self.projects.loc[self.projects[nam.process_code] == 21, nam.max_year_drill_pct] = self.projects[[nam.max_year_drill_pct, nam.hist_well_drill_pct]].max(axis = 1)
        self.projects.loc[self.projects[nam.process_code] == 22, nam.max_year_drill_pct] = self.projects[[nam.max_year_drill_pct, nam.hist_well_drill_pct]].max(axis = 1)

        #Replace Nans, infinities and negative infinities
        self.projects[nam.max_year_drill_pct] = self.projects[nam.max_year_drill_pct].replace([np.inf, -np.inf, np.nan], 0.06)

        pass


    def calculate_drilling(self):
        """Calculates model year drilling for continuous and discovered formations.

        * Contains code to increase Colorado well spacing per 2020 setback regulations
        * Runs projects through the drilling_eq.py nexwells function to get annual project drilling
        * Assigns dryhole values for projects

        Returns
        -------
        self.projects : df
            Master DataFrame containing all projects to be processed in current model year

        self.project_drilling : df
            DataFrame of model year project drilling

        self.project_dryholes : df
            DataFrame of model year project dryholes
        """
        # Adjust resources in Colorado to account for increase in setback reqs (2020)
        # https://coloradonewsline.com/2020/09/15/increased-setbacks-a-ban-on-drilling-a-new-study-shows-otherwise/
        co_mask = self.projects[nam.state] == 'CO'
        temp_df = self.projects[co_mask].copy()
        temp_df[nam.well_decline_limit] = temp_df[nam.well_decline_limit].mul(0.9)
        temp_df[nam.max_wells] = temp_df[nam.max_wells].mul(0.9)
        self.projects.update(temp_df)

        #Legacy Lat-length test from Dana to test for lateral-length density
        self.projects[nam.lat_len_test] = (self.projects[nam.total_pattern_size_acres] / self.projects[nam.lateral_length_ft] * 870 / 43560) * 0.5

        #Calculate project drilling
        continuous_mask = (self.projects[nam.table] != 'other_eor') & (self.projects[nam.table] != 'co2_eor')

        if self.rest_curcalyr <= (self.zero_year+1): #Get history and year-1 drilling from history
            self.project_drilling = self.projects.loc[continuous_mask, nam.hist_year_wells]

        else:
            self.project_drilling = self.projects[continuous_mask].copy().apply(lambda x: drill_eq.on_next_wells(x[nam.well_decline_limit],
                                                                                              x[nam.max_wells],
                                                                                              x[nam.past_wells],
                                                                                              x[nam.last_year_drilling],
                                                                                              x[nam.max_drill_rate],
                                                                                              x[nam.max_drill_rate_frac],
                                                                                              self.drill_predecline,
                                                                                              self.drill_ramp_up,
                                                                                              x[nam.crude_price],
                                                                                              x[nam.natgas_price],
                                                                                              x[nam.well_type_number],
                                                                                              self.base_oil_prc,
                                                                                              self.base_gas_prc,
                                                                                              x['OP1'],
                                                                                              x['GP1'],
                                                                                              x[nam.max_year_drill_pct],
                                                                                              self.rest_curcalyr,
                                                                                              self.parent.low_price_case,
                                                                                              undiscovered_drill_flag = False), axis=1)


        ###Produce drilling for fixed projects (i.e. EOR)
        fixed_mask = (self.projects[nam.table] == 'other_eor') | (self.projects[nam.table] == 'co2_eor')
        temp_fixed_drilling = self.projects[fixed_mask].copy()
        temp_fixed_drilling[nam.remaining_wells] = temp_fixed_drilling[nam.well_decline_limit] - temp_fixed_drilling[nam.past_wells]
        temp_fixed_drilling.loc[temp_fixed_drilling[nam.remaining_wells] < 0, 'remaining_wells'] = 0
        temp_fixed_drilling = temp_fixed_drilling[['WL1', nam.remaining_wells]].min(axis=1)
        self.project_drilling = pd.concat([self.project_drilling, temp_fixed_drilling], ignore_index = False)


        ###Get project dryholes
        temp_dryholes_df = self.projects[[nam.region_number, nam.well_type, nam.resource_type, nam.dev_type_number]].copy()
        temp_dryholes_df[nam.wells] = self.project_drilling

        #Mask for developing vs. discovered projects
        dev_dryholes_mask   = temp_dryholes_df[nam.dev_type_number] == 2
        disc_dryholes_mask  = temp_dryholes_df[nam.dev_type_number] == 3

        temp_dryhole_dev    = temp_dryholes_df[dev_dryholes_mask].copy()
        temp_dryhole_disc   = temp_dryholes_df[disc_dryholes_mask].copy()

        #Developing Projects dryholes
        temp_dev_dryhole_rate = self.dryhole_rate.loc[self.dryhole_rate[nam.drill_category] == 1] #Category 1 = developing projects
        temp_dryhole_dev = temp_dryhole_dev.reset_index().merge(temp_dev_dryhole_rate,
                                                                  how = 'left',
                                                                  on = [nam.region_number, nam.resource_type]).set_index('index')
        temp_dryhole_dev[nam.dryholes] = (temp_dryhole_dev[nam.wells].mul(temp_dryhole_dev[nam.dryhole_rate])).apply(np.ceil)

        #Discovered Projects dryholes
        temp_disc_dryhole_rate = self.dryhole_rate.loc[self.dryhole_rate[nam.drill_category] == 3] #Category 3 = discovered projects
        temp_dryhole_disc = temp_dryhole_disc.reset_index().merge(temp_disc_dryhole_rate,
                                                                  how = 'left',
                                                                  on = [nam.region_number, nam.resource_type]).set_index('index')
        temp_dryhole_disc[nam.dryholes] = (temp_dryhole_disc[nam.wells].mul(temp_dryhole_disc[nam.dryhole_rate])).apply(np.ceil)

        #Set project dryholes
        self.project_dryholes = pd.concat([temp_dryhole_dev[nam.dryholes], temp_dryhole_disc[nam.dryholes]], ignore_index = False)
        self.project_dryholes.name = 0

        pass


    def apply_cost_tech_rate(self):
        """Applies technology improvement rate to relevant costs.

        Returns
        -------
        self.projects : df
            Master DataFrame containing all projects to be processed in current model year
        """

        #Apply Technology Improvement
        self.projects[nam.production_opex_brl] = self.projects[nam.production_opex_brl].mul(1 - self.projects[nam.cost_tech])
        self.projects[nam.transport_opex_brl] = self.projects[nam.transport_opex_brl].mul(1 - self.projects[nam.cost_tech])
        self.projects[nam.facility_capex_well] = self.projects[nam.facility_capex_well].mul(1 - self.projects[nam.cost_tech])
        self.projects[nam.drill_cost] = self.projects[nam.drill_cost].mul(1 - self.projects[nam.drill_tech])
        self.projects[nam.dry_hole_cost] = self.projects[nam.dry_hole_cost].mul(1 - self.projects[nam.drill_tech])

        pass


    def calculate_exploration_costs(self):
        """Calculates geological, engineering and lease acquisition costs for discovered wells using legacy OGSM equations.

            * We only calculate geological and acquisition costs for undiscovered projects (vs. horizontal projects) because
              these costs are tied to the discovery

        Returns
        -------
        self.gg_costs : df
            DataFrame of geological, engineering and lease costs for discovered wells.
        """
        lbc_frac = 0.01 #lease bonus cost factor

        #Instantiate g&g lease capex for undiscovered projects
        prod_years = list(range(self.evaluation_years))
        self.gg_costs = pd.DataFrame(index=self.projects.index, columns=prod_years).fillna(0.0)

        #Mask for undiscovered projects and minimize DF size for loop
        undiscovered_mask = self.projects[nam.process_code] >= 16
        temp_df = self.projects[undiscovered_mask].copy()
        temp_df = temp_df[[nam.projected_und_wells,nam.process_code,nam.cum_gas_prod,
                           nam.cum_oil_prod,nam.natgas_price,nam.crude_price,nam.project_royalty_multiplier]]

        #Get temp past wells
        temp_df[nam.temp_total_patterns] = 0

        #mask for oil and gas projects
        oil_mask = (temp_df[nam.process_code] == 16) | \
                      (temp_df[nam.process_code] == 18) | \
                      (temp_df[nam.process_code] == 19) | \
                      (temp_df[nam.process_code] == 20)
        gas_mask = (temp_df[nam.process_code] == 17) | \
                      (temp_df[nam.process_code] == 21) | \
                      (temp_df[nam.process_code] == 22) | \
                      (temp_df[nam.process_code] == 23)

        temp_gas_df = temp_df[gas_mask].copy()
        temp_oil_df = temp_df[oil_mask].copy()

        #Calculate expensed G&G and Lease aquisition costs
        oil_lease_costs = pd.DataFrame(index = temp_oil_df.index, columns = prod_years).fillna(0.0)
        gas_lease_costs = pd.DataFrame(index = temp_gas_df.index, columns = prod_years).fillna(0.0)

        #Calculate Lease Bonus
        oil_lease_costs[0] = temp_oil_df[nam.cum_oil_prod] * lbc_frac * temp_oil_df[nam.crude_price] * (1 - temp_oil_df[nam.project_royalty_multiplier].mul(self.royalty_rate))
        gas_lease_costs[0] = temp_gas_df[nam.cum_gas_prod] * lbc_frac * temp_gas_df[nam.natgas_price] * (1 - temp_gas_df[nam.project_royalty_multiplier].mul(self.royalty_rate))


        ###Calculate Lease Costs
        for year in prod_years:
            #Iteratively add projected undiscovered wells each year
            temp_oil_df[nam.temp_total_patterns] = temp_oil_df[nam.temp_total_patterns] + temp_oil_df[nam.projected_und_wells]
            temp_gas_df[nam.temp_total_patterns] = temp_gas_df[nam.temp_total_patterns] + temp_gas_df[nam.projected_und_wells]

            #Oil
            oil_lease_costs[year] = oil_lease_costs[year] + (temp_oil_df[nam.cum_oil_prod] * 7.62 *
                                                             (temp_oil_df[nam.projected_und_wells] / temp_oil_df[nam.temp_total_patterns]))

            #Natural Gas
            #Get different term lengths for different gas types
            temp_gas_df[nam.term] = 0
            temp_gas_df[nam.term_mult] = 0
            conv_mask = temp_gas_df[nam.process_code] == 17
            shale_cbm_mask = (temp_gas_df[nam.process_code] == 21) | \
                             (temp_gas_df[nam.process_code] == 22) | \
                             (temp_gas_df[nam.process_code] == 23)

            #Apply Masks
            temp_conv_gas_df = temp_gas_df[conv_mask].copy()
            temp_shale_cbm_gas_df = temp_gas_df[shale_cbm_mask].copy()

            #Calculate Term and term mult
            temp_conv_gas_df[nam.term] = 1+ 0.7 *((temp_conv_gas_df[nam.natgas_price]) - 4.0) / 4.0
            temp_conv_gas_df[nam.term_mult] = 1
            temp_shale_cbm_gas_df[nam.term] = 1+ 0.4 *((temp_shale_cbm_gas_df[nam.natgas_price]) - 3.54) / 3.54
            temp_shale_cbm_gas_df[nam.term_mult] = 0.45

            #Update temp_gas_df
            temp_gas_df.update(temp_conv_gas_df)
            temp_gas_df.update(temp_shale_cbm_gas_df)

            gas_lease_costs[year] = gas_lease_costs[year] + (temp_gas_df[nam.cum_gas_prod] * 7.62 * 0.178 * temp_gas_df[nam.term_mult] * temp_gas_df[nam.term] \
                                    * (temp_gas_df[nam.projected_und_wells] / temp_gas_df[nam.temp_total_patterns]))

        #Assign GG costs
        self.gg_costs.update(oil_lease_costs)
        self.gg_costs.update(gas_lease_costs)

        #Multiply GG costs by 2 since equations are dated to 2014 and costs have grown
        self.gg_costs = self.gg_costs.mul(2, axis = 1)

        pass


    def calculate_ngpl_costs(self):
        """Calculates NGPL Processing Plant Costs using legacy OGSM equations.

        Returns
        -------
        self.projects : df
            Master DataFrame containing all projects to be processed in current model year
        """
        ###Setup NGPL costs
        #Create ngpl_cost variable
        self.projects[nam.ngpl_cost] = 0

        #Set floor of ngpl production at which costs need to be calculated
        ngl_lim = 6.7
        ngl_lim_mask = self.projects[nam.ngpl] > ngl_lim
        ngpl_df = self.projects[ngl_lim_mask].copy()
        ngpl_df = ngpl_df[[nam.region_number, nam.ngpl]]
        ngpl_df = ngpl_df.reset_index().merge(self.ngpl_costs, on = nam.region_number, how = 'left').set_index('index')

        #Get denominator value and apply to relevant input
        ngpl_df[nam.denom] =  (1 - (ngpl_df[nam.ngpl] * 1512.0) / (10 ** 6 + ngpl_df[nam.ngpl] * 1512.0))
        ngpl_df[nam.capacity_st] = ngpl_df[nam.capacity_st].div(ngpl_df[nam.denom])
        ngpl_df[nam.capacity_cr] = ngpl_df[nam.capacity_cr].div(ngpl_df[nam.denom])

        #Create cost type columns
        ngpl_df[nam.str_plant_capex]    = 0
        ngpl_df[nam.str_plant_opex]     = 0
        ngpl_df[nam.cry_plant_capex]    = 0
        ngpl_df[nam.cry_plant_opex]     = 0


        ###Straight Refrigeration Plant: Capital Costs
        #Calculate gallons per mcf (GPM) Value
        ngpl_df[nam.gpm_val] = ngpl_df[nam.thru] * ngpl_df[nam.ngpl] * 42.0 / 24.0 / 60.0

        #192 GPM C3+
        ngpl_df['gpm_192'] = 1.4396 * np.log(ngpl_df[nam.capacity_st] - 1.0789)

        #96 GPM C3+
        ngpl_df['gpm_96'] = 1.194 * np.log(ngpl_df[nam.capacity_st] - 0.8611)

        #48 GPM C3+
        ngpl_df['gpm_48'] = 0.9868 * np.log(ngpl_df[nam.capacity_st] - 0.7031)

        #24 GPM C3+
        ngpl_df['gpm_24'] = 0.8139 * np.log(ngpl_df[nam.capacity_st] - 0.6011)

        #12 GPM C3+
        ngpl_df['gpm_12'] = 0.6724 * np.log(ngpl_df[nam.capacity_st] - 0.5536)

        #6.0 GPM C3+  (Rich)
        ngpl_df['gpm_6'] = 0.5612 * np.log(ngpl_df[nam.capacity_st] - 0.5628)

        #3.0 GPM C3+ (Medium)
        ngpl_df['gpm_3'] = 0.481300 * np.log(ngpl_df[nam.capacity_st] - 0.6366)

        #1.5 GPM C3+ (Lean)
        ngpl_df['gpm_1.5'] = 0.437900 * np.log(ngpl_df[nam.capacity_st] - 0.7929)

        #Interpolate costs based on the GPM value
        #1.5 GPM
        mask = ngpl_df[nam.gpm_val] <= 1.5
        temp_df = ngpl_df[mask].copy()
        temp_df[nam.weight_b] = temp_df[nam.gpm_val]/1.5
        temp_df[nam.weight_a] = 1 - temp_df[nam.weight_b]
        temp_df[nam.str_plant_capex] = temp_df[nam.weight_a] * 0 + temp_df[nam.weight_b] * temp_df['gpm_1.5']
        ngpl_df.update(temp_df)

        #3.00 GPM
        mask = (ngpl_df[nam.gpm_val] > 1.5) & (ngpl_df[nam.gpm_val] <= 3.0)
        temp_df = ngpl_df[mask].copy()
        temp_df[nam.weight_b] = (temp_df[nam.gpm_val] - 1.5)/(3.0-1.5)
        temp_df[nam.weight_a] = 1 - temp_df[nam.weight_b]
        temp_df[nam.str_plant_capex] = temp_df[nam.weight_a] * temp_df['gpm_1.5'] + temp_df[nam.weight_b] * temp_df['gpm_3']
        ngpl_df.update(temp_df)

        #6.00 GPM
        mask = (ngpl_df[nam.gpm_val] > 3.0) & (ngpl_df[nam.gpm_val] <= 6.0)
        temp_df = ngpl_df[mask].copy()
        temp_df[nam.weight_b] = (temp_df[nam.gpm_val] - 3.0)/(6.0-3.0)
        temp_df[nam.weight_a] = 1 - temp_df[nam.weight_b]
        temp_df[nam.str_plant_capex] = temp_df[nam.weight_a] * temp_df['gpm_3'] + temp_df[nam.weight_b] * temp_df['gpm_6']
        ngpl_df.update(temp_df)

        #12.00 GPM
        mask = (ngpl_df[nam.gpm_val] > 6.0) & (ngpl_df[nam.gpm_val] <= 12.0)
        temp_df = ngpl_df[mask].copy()
        temp_df[nam.weight_b] = (temp_df[nam.gpm_val] - 6.0)/(12.0-6.0)
        temp_df[nam.weight_a] = 1 - temp_df[nam.weight_b]
        temp_df[nam.str_plant_capex] = temp_df[nam.weight_a] * temp_df['gpm_6'] + temp_df[nam.weight_b] * temp_df['gpm_12']
        ngpl_df.update(temp_df)

        #24.00 GPM
        mask = (ngpl_df[nam.gpm_val] > 12.0) & (ngpl_df[nam.gpm_val] <= 24.0)
        temp_df = ngpl_df[mask].copy()
        temp_df[nam.weight_b] = (temp_df[nam.gpm_val] - 12.0)/(24.0-12.0)
        temp_df[nam.weight_a] = 1 - temp_df[nam.weight_b]
        temp_df[nam.str_plant_capex] = temp_df[nam.weight_a] * temp_df['gpm_12'] + temp_df[nam.weight_b] * temp_df['gpm_24']
        ngpl_df.update(temp_df)

        #48.00 GPM
        mask = (ngpl_df[nam.gpm_val] > 24.0) & (ngpl_df[nam.gpm_val] <= 48.0)
        temp_df = ngpl_df[mask].copy()
        temp_df[nam.weight_b] = (temp_df[nam.gpm_val] - 24.0)/(48.0/24.0)
        temp_df[nam.weight_a] = 1 - temp_df[nam.weight_b]
        temp_df[nam.str_plant_capex] = temp_df[nam.weight_a] * temp_df['gpm_24'] + temp_df[nam.weight_b] * temp_df['gpm_48']
        ngpl_df.update(temp_df)

        #96.00 GPM
        mask = (ngpl_df[nam.gpm_val] > 48.0) & (ngpl_df[nam.gpm_val] <= 96.0)
        temp_df = ngpl_df[mask].copy()
        temp_df[nam.weight_b] = (temp_df[nam.gpm_val] - 48.0)/(96.0-48.0)
        temp_df[nam.weight_a] = 1 - temp_df[nam.weight_b]
        temp_df[nam.str_plant_capex] = temp_df[nam.weight_a] * temp_df['gpm_48'] + temp_df[nam.weight_b] * temp_df['gpm_96']
        ngpl_df.update(temp_df)

        #192.00 GPM
        mask = (ngpl_df[nam.gpm_val] > 96.0) & (ngpl_df[nam.gpm_val] <= 192.0)
        temp_df = ngpl_df[mask].copy()
        temp_df[nam.weight_b] = (temp_df[nam.gpm_val] - 96.0)/(192.0-96.0)
        temp_df[nam.weight_a] = 1 - temp_df[nam.weight_b]
        temp_df[nam.str_plant_capex] = temp_df[nam.weight_a] * temp_df['gpm_96'] + temp_df[nam.weight_b] * temp_df['gpm_192']
        ngpl_df.update(temp_df)

        # Max GPM
        mask = ngpl_df[nam.gpm_val] >= 192.0
        temp_df = ngpl_df[mask].copy()
        temp_df[nam.str_plant_capex] = temp_df['gpm_192']
        ngpl_df.update(temp_df)

        #Exponentiate Cost
        ngpl_df[nam.str_plant_capex] = np.exp(ngpl_df[nam.str_plant_capex])


        ###Cryogenic Expander Plant: Capital Costs
        #CAP CR < 100
        mask = ngpl_df[nam.capacity_cr] < 100
        temp_df = ngpl_df[mask].copy()
        temp_df[nam.cry_plant_capex] = np.exp(0.484200 * np.log(temp_df[nam.capacity_cr]) - 0.096200)
        ngpl_df.update(temp_df)

        #CAP CR >= 100
        mask = ngpl_df[nam.capacity_cr] >= 100
        temp_df = ngpl_df[mask].copy()
        temp_df[nam.cry_plant_capex] = np.exp(0.989300 * np.log(temp_df[nam.capacity_cr]) - 2.376900)
        ngpl_df.update(temp_df)


        ###Straight Refrigeration Plant: Operating Costs #$/MCF
        #Calculate GPM Value
        temp_df[nam.gpm_val] = temp_df[nam.thru] * temp_df[nam.ngpl] * 42.0 / 24.0 / 60.0

        #192 GPM C3+
        ngpl_df['gpm_192'] = 0.3667*ngpl_df[nam.capacity_st]**(-.5809)

        #96 GPM C3+
        ngpl_df['gpm_96'] = 0.2177 * ngpl_df[nam.capacity_st] ** (-.3134)

        #48 GPM C3+
        ngpl_df['gpm_48'] = 0.2038 * ngpl_df[nam.capacity_st] ** (-.3155)

        #24 GPM C3+
        ngpl_df['gpm_24'] = 0.1898 * ngpl_df[nam.capacity_st] ** (-.3179)

        #12 GPM C3+
        ngpl_df['gpm_12'] = 0.1759 * ngpl_df[nam.capacity_st] ** (-.3207)

        #6.0 GPM C3+  (Rich)
        ngpl_df['gpm_6'] = 0.162 * ngpl_df[nam.capacity_st] ** (-.3241)

        #3.0 GPM C3+ (Medium)
        ngpl_df['gpm_3'] = 0.1481 * ngpl_df[nam.capacity_st] ** (-.3282)

        #1.5 GPM C3+ (Lean)
        ngpl_df['gpm_1.5'] = 0.1293 * ngpl_df[nam.capacity_st] ** (-.3159)


        #Interpolate costs based on the GPM Value
        #1.5 GPM
        mask = ngpl_df[nam.gpm_val] <= 1.5
        temp_df = ngpl_df[mask].copy()
        temp_df[nam.weight_b] = temp_df[nam.gpm_val] / 1.5
        temp_df[nam.weight_a] = 1 - temp_df[nam.weight_b]
        temp_df[nam.str_plant_opex] = temp_df[nam.weight_a] * 0 + temp_df[nam.weight_b] * temp_df['gpm_1.5']
        ngpl_df.update(temp_df)

        #3.00 GPM
        mask = (ngpl_df[nam.gpm_val] > 1.5) & (ngpl_df[nam.gpm_val] <= 3.0)
        temp_df = ngpl_df[mask].copy()
        temp_df[nam.weight_b] = (temp_df[nam.gpm_val] - 1.5) / (3.0 - 1.5)
        temp_df[nam.weight_a] = 1 - temp_df[nam.weight_b]
        temp_df[nam.str_plant_opex] = temp_df[nam.weight_a] * temp_df['gpm_1.5'] + temp_df[nam.weight_b] * temp_df['gpm_3']
        ngpl_df.update(temp_df)

        #6.00 GPM
        mask = (ngpl_df[nam.gpm_val] > 3.0) & (ngpl_df[nam.gpm_val] <= 6.0)
        temp_df = ngpl_df[mask].copy()
        temp_df[nam.weight_b] = (temp_df[nam.gpm_val] - 3.0) / (6.0 - 3.0)
        temp_df[nam.weight_a] = 1 - temp_df[nam.weight_b]
        temp_df[nam.str_plant_opex] = temp_df[nam.weight_a] * temp_df['gpm_3'] + temp_df[nam.weight_b] * temp_df['gpm_6']
        ngpl_df.update(temp_df)

        #12.00 GPM
        mask = (ngpl_df[nam.gpm_val] > 6.0) & (ngpl_df[nam.gpm_val] <= 12.0)
        temp_df = ngpl_df[mask].copy()
        temp_df[nam.weight_b] = (temp_df[nam.gpm_val] - 6.0) / (12.0 - 6.0)
        temp_df[nam.weight_a] = 1 - temp_df[nam.weight_b]
        temp_df[nam.str_plant_opex] = temp_df[nam.weight_a] * temp_df['gpm_6'] + temp_df[nam.weight_b] * temp_df['gpm_12']
        ngpl_df.update(temp_df)

        #24.00 GPM
        mask = (ngpl_df[nam.gpm_val] > 12.0) & (ngpl_df[nam.gpm_val] <= 24.0)
        temp_df = ngpl_df[mask].copy()
        temp_df[nam.weight_b] = (temp_df[nam.gpm_val] - 12.0) / (24.0 - 12.0)
        temp_df[nam.weight_a] = 1 - temp_df[nam.weight_b]
        temp_df[nam.str_plant_opex] = temp_df[nam.weight_a] * temp_df['gpm_12'] + temp_df[nam.weight_b] * temp_df['gpm_24']
        ngpl_df.update(temp_df)

        #48.00 GPM
        mask = (ngpl_df[nam.gpm_val] > 24.0) & (ngpl_df[nam.gpm_val] <= 48.0)
        temp_df = ngpl_df[mask].copy()
        temp_df[nam.weight_b] = (temp_df[nam.gpm_val] - 24.0) / (48.0 / 24.0)
        temp_df[nam.weight_a] = 1 - temp_df[nam.weight_b]
        temp_df[nam.str_plant_opex] = temp_df[nam.weight_a] * temp_df['gpm_24'] + temp_df[nam.weight_b] * temp_df['gpm_48']
        ngpl_df.update(temp_df)

        #96.00 GPM
        mask = (ngpl_df[nam.gpm_val] > 48.0) & (ngpl_df[nam.gpm_val] <= 96.0)
        temp_df = ngpl_df[mask].copy()
        temp_df[nam.weight_b] = (temp_df[nam.gpm_val] - 48.0) / (96.0 - 48.0)
        temp_df[nam.weight_a] = 1 - temp_df[nam.weight_b]
        temp_df[nam.str_plant_opex] = temp_df[nam.weight_a] * temp_df['gpm_48'] + temp_df[nam.weight_b] * temp_df['gpm_96']
        ngpl_df.update(temp_df)

        #192.00 GPM
        mask = (ngpl_df[nam.gpm_val] > 96.0) & (ngpl_df[nam.gpm_val] <= 192.0)
        temp_df = ngpl_df[mask].copy()
        temp_df[nam.weight_b] = (temp_df[nam.gpm_val] - 96.0) / (192.0 - 96.0)
        temp_df[nam.weight_a] = 1 - temp_df[nam.weight_b]
        temp_df[nam.str_plant_opex] = temp_df[nam.weight_a] * temp_df['gpm_96'] + temp_df[nam.weight_b] * temp_df['gpm_192']
        ngpl_df.update(temp_df)

        #Max GPM
        mask = ngpl_df[nam.gpm_val] >= 192.0
        temp_df = ngpl_df[mask].copy()
        temp_df[nam.str_plant_opex] = temp_df['gpm_192']
        ngpl_df.update(temp_df)


        ###Legacy Cryogenic Extender Operating Costs for NPGLs #$/MCF
        #CAPCR < 30
        mask = ngpl_df[nam.capacity_cr] < 30
        temp_df = ngpl_df[mask].copy()
        temp_df[nam.cry_plant_opex] = np.exp(2.738000 * np.exp(-0.253400 * np.log(temp_df[nam.capacity_cr])))/100
        ngpl_df.update(temp_df)

        #CAPCR >= 30
        mask = ngpl_df[nam.capacity_cr] >= 30
        temp_df = ngpl_df[mask].copy()
        temp_df[nam.cry_plant_opex] = np.exp(4.341700 * np.exp(-0.246300 * np.log(temp_df[nam.capacity_cr])))/100
        ngpl_df.update(temp_df)


        ###Clean and merge NGPL Costs
        #Run through legacy unit conversion calculations for capex
        ngpl_df[nam.str_plant_capex] = com.leg_cost_conversion(ngpl_df[nam.str_plant_capex], ngpl_df[nam.capacity_st], 0.15, 15, 0.97)
        ngpl_df[nam.cry_plant_capex] = com.leg_cost_conversion(ngpl_df[nam.cry_plant_capex], ngpl_df[nam.capacity_cr], 0.15, 15, 0.97)

        #Fuel costs are approximately the same amount as operating costs, so double opex
        ngpl_df[nam.str_plant_opex] = ngpl_df[nam.str_plant_opex] * 2
        ngpl_df[nam.cry_plant_opex] = ngpl_df[nam.cry_plant_opex] * 2

        #Take Minimum NGPL Plant Cost
        ngpl_df[nam.ngpl_opex] = ngpl_df[[nam.str_plant_opex, nam.cry_plant_opex]].min(axis = 1)
        ngpl_df[nam.ngpl_capex] = ngpl_df[[nam.str_plant_capex, nam.cry_plant_capex]].min(axis=1)

        #Divide ngpl opex by denom
        ngpl_df[nam.ngpl_opex] = ngpl_df[nam.ngpl_opex].div(ngpl_df[nam.denom])

        #Create single cost
        ngpl_df[nam.ngpl_cost] = ngpl_df[nam.ngpl_opex] + ngpl_df[nam.ngpl_capex]

        #Update Project DF
        self.projects.update(ngpl_df)

        pass

    def load_ch4_emission_costs(self):
        """Load methane venting/flaring costs/ton of ch4 vented/flared.

        Returns
        -------
        self.ch4_emission_cost : df
            DataFrame containing CH4 emission cost bases (costs are not consistent across years)
        """

        def duplicate_list(list, dup_years):
            return [cost for cost in list for _ in dup_years]
        dup_list = duplicate_list([1500], list(range(max(2026, self.rest_curcalyr), (self.rest_curcalyr + self.evaluation_years))))

        if self.rest_curcalyr <= 2024:
            self.ch4_emission_cost = [900, 1200] + dup_list
        elif self.rest_curcalyr <= 2025:
            self.ch4_emission_cost = [1200] + dup_list
        else:
            self.ch4_emission_cost = dup_list

        if self.rest_curcalyr < 2024: #Don't apply emissions charges for 2022 or 2023
            zero_list = duplicate_list([0], range((len(self.ch4_emission_cost)), (self.evaluation_years)))
            self.ch4_emission_cost = zero_list + self.ch4_emission_cost

        self.ch4_emission_cost = np.array(self.ch4_emission_cost)

        pass


    def co2_eor_econ(self):
        """Calculates costs and tax credits related to CO2 EOR using legacy OGSM equations.

            * Calculate water handling plant cost
            * Calculate recycling plant cost
            * Get base EOR tax credit values including 45Q
        Returns
        -------
        self.projects : df
            Master DataFrame containing all projects to be processed in current model year

        self.co2_45q_eor_tax_credit : df
            DataFrame of 45q tax credit values
        """
        #Create relevant project columns
        self.projects[nam.water_plant_cost]     = 0.0
        self.projects[nam.co2_recy_plant_cost]  = 0.0
        self.projects[nam.eor_tc_rate]          = 0.0
        self.projects[nam.eor_tc_phaseout]      = 0.0

        #Create prod range for EOR/ASR cost equations
        prod_range = list(range(self.evaluation_years))

        #Mask for eligible projects
        co2_mask = (self.projects[nam.eor_type] > 0)
        eligible_mask = self.projects[nam.eligible] == 1
        eor_mask = co2_mask & eligible_mask
        eor_df = self.projects[eor_mask].copy()


        ###Calculate costs specific to EOR
        #Cost of produced water handling plant
        water_range = [('WP' + str(x + 1)) for x in prod_range]
        eor_df[nam.water_max] = eor_df[water_range].max(axis=1)
        eor_df[nam.water_plant_cost] = eor_df[nam.water_max].mul(1000).div(365).mul(self.eor_other_costs.at[nam.water_handling_plant, 'a'])
        eor_df = eor_df.drop([nam.water_max], axis=1) #Drop temp columns

        #Cost of CO2 Recycling Plant
        project_co2_recy = self.project_co2_recy.copy()
        recy_max = project_co2_recy.max(axis = 1)

        #Split Recycling Plant Costs into two volume categories and apply legacy facility cost eqs
        under_30_mask = (recy_max/365) <= 30
        temp_df = self.project_co2_recy[under_30_mask].copy()
        temp_df[nam.co2_recy_plant_cost] = 1200 * (recy_max/365)
        eor_df.update(temp_df)

        over_30_mask = (recy_max/365) > 30
        temp_df = self.project_co2_recy[over_30_mask].copy()
        temp_df[nam.co2_recy_plant_cost] = 36000.0 + 750.0 * (recy_max/365.0-30.0)
        eor_df.update(temp_df)

        #Convert Recycling Cost from 2016$
        eor_df[nam.co2_recy_plant_cost] = eor_df[nam.co2_recy_plant_cost].div(self.parent.rest_mc_jpgdp.at[2016, nam.value])

        #Apply CPI factors relative to 2008 to relevant costs
        cpi_2003 = 1.170
        eor_df[nam.water_plant_cost]    = eor_df[nam.water_plant_cost] * cpi_2003


        ###Calculate EOR tax credits
        #Set EOR tax credit rates and phaseout
        eor_df[nam.eor_tc_rate] = self.eor_tc_rate
        eor_df[nam.eor_tc_phaseout] = self.eor_tc_phaseout

        #Set EOR 45Q Rate and Phaseout
        program_phaseout_year = self.parent.rest_45q_lyr_ret
        duration = self.parent.rest_45q_duration

        if self.rest_curcalyr <= program_phaseout_year + duration:
            self.co2_45q_eor_tax_credit = self.parent.rest_ccs_eor_45q / (1 - self.parent.fed_tax_rate) * self.parent.rest_mc_jpgdp.at[2008, nam.value]
        else:
            self.co2_45q_eor_tax_credit = self.parent.rest_ccs_eor_45q.mul(0.0)


        ###Load EOR Costs into projects
        self.projects.update(eor_df)

        pass


    def determine_co2_supply_prices(self):
        """Calculate Recycled CO2 supply and prices.

            * Calculate CO2 price from natural sources based on crude oil price (CO2 supply is determined by NETL input file)
            * Apply Natural CO2 price as CO2 price for now,

        Returns
        -------
        self.co2_supply_price : df
            Table of legacy OGSM CO2 EOR supply and price curves

        self.co2_price : df
            DataFrame of co2 price by source and region
        """
        ###Calculate Recyclef CO2 prices
        recy_co2_price_df = pd.DataFrame(columns=[nam.region_number, nam.co2_type, nam.co2_price])

        #Get Benchmark Oil prices
        bench_oil_1 = self.parent.rest_dcrdwhp.copy()
        recy_co2_price_df[nam.bench_1] = bench_oil_1[self.rest_curcalyr]
        bench_oil_2 = (self.parent.rest_rfqtdcrd.copy().mul(self.parent.rest_dcrdwhp.copy(), axis='index')).div(self.parent.rest_rfqtdcrd.copy()).fillna(0)
        recy_co2_price_df[nam.bench_2] = bench_oil_2[[(self.rest_curcalyr - 1)]]

        #Calculate CO2 costs
        recy_co2_price_df[nam.bench_oil] = recy_co2_price_df[[nam.bench_1, nam.bench_2]].values.min(1)
        recy_co2_price_df[nam.co2_price] = self.eor_other_costs.at['natural_co2', 'a'] + recy_co2_price_df[nam.bench_oil].mul(self.eor_other_costs.at['natural_co2', 'b'])

        #Clean temp df and format df for merge to master CO2 price df
        recy_co2_price_df[nam.co2_type] = 4
        recy_co2_price_df[nam.bin] = 1
        recy_co2_price_df[nam.region_number] = recy_co2_price_df.index

        #Merge Recycled CO2 to legacy concrete CO2 prices to set ceiling on prices
        recy_co2_price_df = recy_co2_price_df.merge(self.co2_legacy_costs[[nam.region_number, 5]], how = 'left', on = nam.region_number)
        recy_co2_price_df[nam.co2_price] = recy_co2_price_df[[nam.co2_price, 5]].min(axis = 1)

        #Format recy_co2_price for merge with other co2 price types
        recy_co2_price_df = recy_co2_price_df.drop([nam.bench_1, nam.bench_2, nam.bench_oil, 5, nam.bin, nam.co2_type], axis=1)
        recy_co2_price_df.columns = [nam.region_number, nam.co2_price]

        # Get CO2 price df
        self.co2_price =  recy_co2_price_df #Update recycled CO2 prices with calculated CO2 prices

        pass


    def calculate_co2_project_costs(self):
        """Apply CO2 prices to CO2 required for each CO2 EOR project, using the lowest-cost prices first for all projects,
        and apply to the self.projects dataframe as a single weighted-average cost.

            * Get CO2 Supply and Costs
            * Apply transportation costs for CO2 to CO2 costs (not doing this for constraints because too granular and limited benefit)
            * Take only the two most economical CO2 sources by project and apply co2 costs as a weighted average to reduce runtime
              (we only use the most economical sources because there is no way to determine which CO2 projects will be selected before cash flow,
              and thus no selection order for CO2 supply)
            * We can default to the two lowest cost CO2 sources because CO2 required never exceeds top two sources of CO2 supply,
              but if this begins to happen with any frequency equation can be updated (at the cost of runtime as this calculation is slow)

        Returns
        -------
        self.projects : df
            Master DataFrame containing all projects to be processed in current model year
        """
        #Instantiate Projects CO2 cost
        self.projects[nam.co2_cost] = 0

        #Set masks for CO2 and eligibility
        eligibility_mask = self.projects[nam.eligible] == 1
        co2_mask = self.projects[nam.process_code] == 10
        mask = eligibility_mask & co2_mask

        #Get CO2 Supply and Price and subtract transportation cost from price
        co2_price_df = self.co2_price.copy()
        co2_price_df[nam.co2_price] = co2_price_df[nam.co2_price] - self.co2_trans_cost

        #Merge price to temp_df and update projects
        temp_df = self.projects[[nam.region_number, nam.process_code]].copy()
        temp_df = temp_df.loc[temp_df[nam.process_code] == 10]
        temp_df = temp_df.reset_index().merge(co2_price_df[[nam.region_number, nam.co2_price ]],
                                              how = 'left',
                                              on = nam.region_number).set_index('index')
        temp_df = temp_df.rename(columns = {'co2_price':'co2_cost'})

        #Update self.projects
        self.projects.update(temp_df)

        pass


    def load_cashflow(self):
        """Load developing projects into cash flow.

        Returns
        -------
        self.cash_flow.properties : df
            DataFrame containing properties used in the cash flow (costs, tangible/intangible cost ratios, etc.)

        self.cash_flow.crude_production : df
            DataFrame of onshore crude oil production

        self.cash_flow.natgas_production : df
            DataFrame of onshore natural gas production

        self.cash_flow.ngpl_production  : df
            DataFrame of onshore NGPL production

        self.cash_flow.co2_use : df
            DataFrame of onshore CO2 use

        self.cash_flow.crude_price : df
            DataFrame of crude prices

        self.cash_flow.natgas_price : df
            DataFrame of natural gas prices

        self.cash_flow.exp_drill_cost : df
            DataFrame of exploratory well drill costs

        self.cash_flow.dev_drill_cost : df
            DataFrame of development well drill costs

        self.cash_flow.exp_dry_cost : df
            DataFrame of exploratory well dryhole costs

        self.cash_flow.dev_dry_cost : df
            DataFrame of development well dryhole costs

        self.cash_flow.gg_la_cost : df
            DataFrame of geological, engineering and lease costs for undiscovered projects

        self.cash_flow.general_admin_cost : df
            DataFrame of GA costs

        self.cash_flow.kap_cost : df
            DataFrame pf capital costs

        """

        self.cash_flow.properties = pd.DataFrame()

        #Load Production
        self.cash_flow.crude_production  = self.project_crude_production.copy()
        self.cash_flow.natgas_production = self.project_natgas_production.copy()
        self.cash_flow.ngpl_production   = self.project_ngpl_production.copy()
        self.cash_flow.co2_use           = self.project_co2_recy.copy()
        self.cash_flow.ch4_emissions     = self.cash_flow.natgas_production.copy().mul(self.projects[nam.ch4_emission_factor], axis = 0)

        #Load Properties
        self.cash_flow.properties[nam.hsm_index]  = self.projects[nam.hsm_index].copy()
        self.cash_flow.properties[nam.crude_price]  = self.projects[nam.crude_price].copy()
        self.cash_flow.properties[nam.natgas_price] = self.projects[nam.natgas_price].copy()
        self.cash_flow.properties[nam.ngpl_price] = self.projects[nam.ngpl_price].copy()
        self.cash_flow.properties[nam.ngpl_volume] = self.projects[nam.ngpl].copy()
        self.cash_flow.properties[nam.remaining_oil_resources] = self.projects[nam.remaining_oil_resources]
        self.cash_flow.properties[nam.remaining_gas_resources] = self.projects[nam.remaining_gas_resources]
        self.cash_flow.properties[[nam.crude_tariff_price, nam.natgas_tariff_price]] = 0.0
        self.cash_flow.properties[nam.royalty_rate] = self.projects[nam.project_royalty_multiplier].fillna(1.0) * self.royalty_rate
        self.cash_flow.properties[nam.state] = self.projects[nam.state].copy()

        #Adding resource type to cash_flow although not needed for cash flow calculation, this helps with masking later
        self.cash_flow.properties[nam.resource_type] = self.projects[nam.resource_type].copy()

        #Load Exploration and Development Drill and Dryhole Costs
        self.cash_flow.exp_drill_cost       = pd.DataFrame(index=self.projects.index, columns=list(range(self.evaluation_years))).fillna(0.0)
        self.cash_flow.dev_drill_cost       = pd.DataFrame(index=self.projects.index, columns=list(range(self.evaluation_years))).fillna(0.0)
        self.cash_flow.dev_drill_cost[0]    = self.projects[nam.drill_cost].copy().fillna(0.0)

        self.cash_flow.exp_dry_cost         = pd.DataFrame(index=self.projects.index, columns=list(range(self.evaluation_years))).fillna(0.0)
        self.cash_flow.dev_dry_cost         = pd.DataFrame(index=self.projects.index, columns=list(range(self.evaluation_years))).fillna(0.0)
        self.cash_flow.dev_dry_cost[0]      = self.projects[nam.dry_hole_cost].copy().fillna(0.0)

        #Load empty dryhole cost table for use in cashflow
        self.cash_flow.dry_hole_cost        = pd.DataFrame(index=self.projects.index, columns=list(range(self.evaluation_years))).fillna(0.0)

        #Calculate geo_geo_and_lease_aq
        self.cash_flow.gg_la_cost = self.gg_costs.copy().fillna(0.0)

        #Calculate SGA Costs
        self.cash_flow.general_admin_cost   = pd.DataFrame(index=self.projects.index, columns=list(range(self.evaluation_years))).fillna(1.0)
        self.cash_flow.properties[nam.sga_opex_well] = self.projects[nam.sga_opex_well] * 0.50 #Set to 50% to be conservative

        #Load CH4 Emissions Cost
        self.cash_flow.ch4_emission_cost    = pd.DataFrame(index=self.projects.index, columns=list(range(self.evaluation_years))).fillna(1.0)
        self.cash_flow.ch4_emission_cost = self.cash_flow.ch4_emission_cost.mul(self.ch4_emission_cost, axis = 1)

        #Load Dry Hole Cost
        self.cash_flow.dry_hole_cost        = pd.DataFrame(index=self.projects.index, columns=list(range(self.evaluation_years))).fillna(0.0)

        #Load production opex
        self.cash_flow.properties[nam.production_opex_brl] = self.projects[nam.production_opex_brl].copy().fillna(0.0)

        #Load CO2 cost opex
        self.projects.loc[self.projects[nam.process_code] != 10, nam.co2_cost] = 0
        self.cash_flow.properties[nam.co2_cost] = self.projects[nam.co2_cost].copy().fillna(0.0)

        #Load transportation opex
        self.cash_flow.properties[nam.crude_trans_price] = self.projects[nam.transport_opex_brl].copy().fillna(0.0)
        self.cash_flow.properties[nam.natgas_trans_price] = self.projects[nam.transport_opex_brl].copy().fillna(0.0) #Adjustment to BOE equiv in cashflow

        #Calculate Onshore Capital Cost
        self.cash_flow.kap_cost       = pd.DataFrame(index=self.projects.index, columns=list(range(self.evaluation_years))).fillna(0.0)
        self.cash_flow.kap_cost[0]    = self.projects[nam.facility_capex_well].fillna(0.0).copy() + \
                                        self.projects[nam.water_plant_cost].fillna(0.0).copy() + \
                                        self.projects[nam.co2_recy_plant_cost].fillna(0.0).copy()

        #Load NGPL Cost
        self.cash_flow.properties[nam.ngpl_cost] = self.projects[nam.ngpl_cost].copy().fillna(0.0)

        #Calculate Equipment Cost
        self.cash_flow.equip_cost           = pd.DataFrame(index=self.projects.index, columns=list(range(self.evaluation_years))).fillna(0.0)

        #Load in fractions for tangible costs
        mask = self.cash_flow.properties[nam.resource_type] == nam.oil
        self.cash_flow.properties.loc[mask, nam.exp_tang_frac] = self.oil_exp_tang_frac
        self.cash_flow.properties.loc[mask, nam.dev_tang_frac] = self.oil_dev_tang_frac
        mask = self.cash_flow.properties[nam.resource_type] == nam.gas
        self.cash_flow.properties.loc[mask, nam.exp_tang_frac] = self.gas_exp_tang_frac
        self.cash_flow.properties.loc[mask, nam.dev_tang_frac] = self.gas_dev_tang_frac

        #Load in fraction for tangible capital costs
        self.cash_flow.properties[nam.kap_tang_frac] = self.kap_tang_frac

        #Load in amortization and depreciation schedules
        self.cash_flow.properties[nam.amor_schedule]   = self.amor_schedule
        self.cash_flow.properties[nam.deprec_schedule] = self.deprec_schedule

        #Load in Fed Tax Rate
        self.cash_flow.properties[nam.fed_tax_rate] = self.parent.fed_tax_rate

        #Load in intangible amortization fraction
        self.cash_flow.properties[nam.intang_amor_frac] = self.intang_amor_frac

        #Load in Abandon Cost rate
        self.cash_flow.properties[nam.abandon_rate] = self.abandon_rate * self.cash_flow.kap_cost[0]

        #Load in EOR Tax Credit
        self.cash_flow.properties[nam.eor_tc_rate]      = self.projects[nam.eor_tc_rate]
        self.cash_flow.properties[nam.eor_tc_phaseout]  = self.projects[nam.eor_tc_phaseout]

        #Load in Discount rate
        self.cash_flow.properties[nam.process_code] = self.projects[nam.process_code].fillna(0.0)
        self.cash_flow.properties[nam.discount_rate] = self.parent.discount_rate + 0.05 #cost of capital + required return over cost of capital

        # Set discount rate for CO2 EOR projects as 2% higher to account for profit over CO2 EOR costs
        temp_df = self.cash_flow.properties.loc[self.cash_flow.properties[nam.process_code] == 10]
        temp_df[nam.discount_rate] = temp_df[nam.discount_rate] + 0.02
        self.cash_flow.properties.update(temp_df)

        #Reorder Properties for DCF
        self.cash_flow.properties = self.cash_flow.properties.sort_index()

        #Set unused variables to 0
        self.cash_flow.eor_tax_credit       = pd.DataFrame(index=self.projects.index, columns=list(range(self.evaluation_years))).fillna(0.0) #Applied directly to CO2 EOR costs
        self.cash_flow.invest_credit        = pd.DataFrame(index=self.projects.index, columns=list(range(self.evaluation_years))).fillna(0.0) #Not Used
        self.cash_flow.fed_credit           = pd.DataFrame(index=self.projects.index,columns=list(range(self.evaluation_years))).fillna(0.0) #Not Used

        pass


    def run_cashflow(self):
        """Run main projects cash flow.

            * Run projects cashflow in **cash_flow.py**
            * Rank projects by projects that had drilling in the previous model (or history) year, then NPV
            * Make sure there are no duplicated undiscovered projects in the table

        Returns
        -------
        self.projects : df
            Master DataFrame containing all projects to be processed in current model year
        """
        self.cash_flow.calculate_revenue()
        self.cash_flow.calculate_royalty()
        self.cash_flow.calculate_severance()
        self.cash_flow.calculate_drill_cost()
        self.cash_flow.calculate_ngpl_operating_cost()
        self.cash_flow.calculate_operating_cost()
        self.cash_flow.calculate_trans_cost()
        self.cash_flow.calculate_intangible_tangible()
        self.cash_flow.calculate_depreciation()
        self.cash_flow.calculate_gg_la_depletion()
        self.cash_flow.calculate_econ_limit()
        self.cash_flow.calculate_abandonment()
        self.cash_flow.calculate_co2_eor_tax_credit()
        self.cash_flow.calculate_ch4_emission_penalties()
        self.cash_flow.calculate_state_tax()
        self.cash_flow.calculate_fed_tax()
        self.cash_flow.calculate_cash_flow()
        self.cash_flow.calculate_profitability()

        #Return profitability indicators
        self.projects[nam.net_present_value] = self.cash_flow.properties[nam.net_present_value]
        self.projects[nam.profitability] = self.cash_flow.properties[nam.profitability]
        self.projects[nam.capital_cost] = self.cash_flow.properties[nam.capital_cost]


        ###Rank Projects by last year drilling if eligible and NPV
        #Projects that had drilling in the previous year or the last history year get a bump in sorting as we assume some continuity
        self.projects[nam.last_year_drilling_sort] = 0
        past_drilling_mask  = (self.projects[nam.last_year_drilling] >= 10) | ((self.projects[nam.hist_year_wells] >= 10) & (self.rest_curcalyr <= self.parent.steo_years[-1]))
        max_wells_mask      = (self.projects[nam.max_wells] >= self.projects[nam.past_wells]) #Simulate well decline
        proc_code_mask      = (self.projects[nam.process_code] != 10) & (self.projects[nam.process_code] != 11) & (self.projects[nam.process_code] <= 15)
        past_drilling_mask  = past_drilling_mask & max_wells_mask & proc_code_mask
        temp_df = self.projects[past_drilling_mask].copy()
        temp_df[nam.last_year_drilling_sort] = 1
        self.projects.update(temp_df)


        ###Apply Sorting to Projects
        self.projects = self.projects.sort_values([nam.last_year_drilling_sort, nam.net_present_value], ascending=False)

        pass


    def determine_eor_eligibility(self):
        """Determine which EOR/ASR projects are eligible to be run in each iteration based on economic life of legacy projects and profitibility.

            * CO2 EOR can only run within 10 years of legacy project abandonment, while other EOR projects can only run within 6 years
            * Once we have a list of eligible projects based on economic life aggregate the most profitible, eligible, selected projects
            * Set all other EOR/ASR projects tied to the same producing project (excluding infill) inelligible
              (there can't be multiple EOR processes operating on the same well)

        Returns
        -------
        self.projects : df
            Master DataFrame containing all projects to be processed in current model year
        """
        ###No EOR Projects are eligible during STEO years

        ###Determine EOR/ASR project eligibility based on economic life
        #Set ineligible project npvs and profitibility to -1
        self.projects.loc[self.projects[nam.eligible] == 0, nam.net_present_value] = -1
        self.projects.loc[self.projects[nam.eligible] == 0, nam.profitability] = -1

        #Mask for econ life (what year the project hits negative net income)
        co2_ineligible_mask = ((self.projects[nam.econ_life] - 15) > self.rest_curcalyr) | \
                          ((self.projects[nam.econ_life] + 15) < self.rest_curcalyr)
        co2_proc_code_mask = self.projects[nam.process_code] == 10

        asr_ineligible_mask = ((self.projects[nam.econ_life] - 6) > self.rest_curcalyr) | \
                          ((self.projects[nam.econ_life] + 6) < self.rest_curcalyr)
        asr_proc_code_mask = self.projects[nam.process_code] == 11

        ineligible_mask = (co2_ineligible_mask & co2_proc_code_mask) | (asr_ineligible_mask & asr_proc_code_mask)

        #Set ASR project profitability to -1 for projects that are not within 5 years of econ life
        temp_df = self.projects[ineligible_mask].copy()
        temp_df[nam.net_present_value] = -1
        temp_df[nam.profitability] = -1
        self.projects.update(temp_df)


        ###Determine project eligibility based on profitability
        #Set Masks
        eor_mask = (self.projects[nam.process_code] == 10) | (self.projects[nam.process_code] == 11)
        npv_mask = self.projects[nam.net_present_value] > 0
        duplicated_mask = self.projects['eor_resid'].duplicated(keep = False)
        most_prof_mask = self.projects['eor_resid'].duplicated(keep = 'first')

        #Merge masks
        most_prof_mask = most_prof_mask & eor_mask & npv_mask

        #Temp df with all eligible EOR projects
        all_eor_temp_df = self.projects[duplicated_mask].copy() # Get duplicated projects

        #Create temp df with only most profitabale eor projects
        most_prof_temp_df = self.projects[most_prof_mask].copy()
        most_prof_temp_df[nam.eligible] = 0

        #Set all lower ranked projects as unprofitable
        less_prof_temp_df = all_eor_temp_df[~(most_prof_mask)].copy()
        less_prof_temp_df[nam.net_present_value] = -1
        less_prof_temp_df[nam.profitability] = -1

        #Pull out infill because infill can be paired with other types of EOR
        infill_mask = most_prof_temp_df[nam.eor_type] == 5
        temp_infill_df = most_prof_temp_df[infill_mask]

        #Merge selected projects with remaining unprofitable projects to see which are removed from eligibility
        most_prof_temp_df = most_prof_temp_df[~infill_mask]
        most_prof_temp_df['selected_flag'] = 1
        less_prof_temp_df = less_prof_temp_df.reset_index().merge(most_prof_temp_df[[nam.eor_resid,'selected_flag']], how='left', on=nam.eor_resid).set_index('index')

        #Set selected flag mask and set all relevant uneconomical projects as ineligible
        selected_mask = less_prof_temp_df['selected_flag'] == 1
        less_prof_temp_df = less_prof_temp_df[selected_mask].copy()
        less_prof_temp_df[nam.eligible] = 0

        #Update self.projects
        self.projects.update(temp_infill_df)
        self.projects.update(most_prof_temp_df)
        self.projects.update(less_prof_temp_df)

        pass


    def apply_rig_constraints(self):
        """Apply rig constraints to projects based on project npv rank.

            * Map project well counts against regional wells/rig calculated in preprocessors
            * Assign two rig constraints, total and regional
            * There's some flexibility in the regional constraints to represent rigs moving around, but no flexibility in the total constraint
            * Select projects until rig count > rig constraint
            * Only 85% of total rigs go to developing projects, the rest goes to exploration

        Returns
        -------
        self.projects : df
            Master DataFrame containing all projects to be processed in current model year
        """
        ###Apply regional rig constraints
        temp_well = pd.DataFrame({nam.wells:(self.project_drilling.copy() + self.project_dryholes.copy())})

        #Merge to projects for ranking
        temp_well = temp_well.merge(self.projects[[nam.region_number, nam.net_present_value, nam.last_year_drilling_sort]], left_index = True, right_index = True)
        temp_well = temp_well.sort_values([nam.last_year_drilling_sort, nam.net_present_value], ascending=False) #Sort by profitability

        #Calculate rigs required for each project based on well count
        temp_well_reg = temp_well.copy()
        temp_well_reg[nam.region_number] = temp_well_reg[nam.region_number].astype('int')
        temp_well_reg = temp_well_reg.reset_index().merge(self.wells_per_rig, how='left', left_on=[nam.region_number], right_on=[nam.region]).set_index('index')
        temp_well_reg[nam.rigs] = temp_well_reg[nam.wells] * (1 / temp_well_reg[nam.wells_per_rig])

        #Get cumulative sum of rigs required
        temp_well_reg[nam.region_cum_rig_count] = temp_well_reg.groupby(nam.region_number)[nam.rigs].cumsum()

        #Combine with rigs required for already producing wells
        temp_well_reg = temp_well_reg.reset_index().merge(self.producing_wells[[self.rest_curcalyr]], how = 'left', left_on = nam.region_number, right_index = True).set_index('index')
        temp_well_reg[nam.producing_rigs] = temp_well_reg[self.rest_curcalyr] * (1 / temp_well_reg[nam.wells_per_rig])
        temp_well_reg[nam.region_cum_rig_count] = temp_well_reg[nam.region_cum_rig_count] + temp_well_reg[nam.producing_rigs]

        #Solve for regional rig constraint and apply to self.projects df
        temp_well_reg = temp_well_reg.reset_index().merge(self.rig_constraint, how = 'left', on = [nam.region_number]).set_index('index')
        self.projects.update(temp_well_reg)
        #self.projects[nam.region_cum_rig_count] = temp_well_reg[nam.region_cum_rig_count]

        #Multiply regional rig constraint by rig use ratio (the remainder of rigs are being used for exploration)
        self.projects[nam.region_rig_constraint] = self.projects[nam.region_rig_constraint].mul(self.dev_const_ratio)

        #Provide some slack to regional rig constraints for rigs to move around (compensated for by national constraint so total rigs never exceed national limit)
        self.projects[nam.region_rig_constraint] = self.projects[nam.region_rig_constraint].mul(1.1)


        ###Calculate national rig constraint and cumulative values
        #Calculate national rig constraint and apply rig use ratio (the remainder of rigs are being used for exploration)
        self.projects[nam.national_rig_constraint] = self.rig_constraint[nam.region_rig_constraint].sum()
        self.projects[nam.national_rig_constraint] = self.projects[nam.national_rig_constraint].mul(self.dev_const_ratio)
        if self.rest_curcalyr <= self.parent.steo_years[-1]:
            self.projects[nam.national_rig_constraint] = self.projects[nam.national_rig_constraint].mul(1.1) #loosen constraint a bit in steo years to bump up production

        #Calculate rigs required for each project based on well count
        temp_well_nat = temp_well.copy()
        temp_well_nat[nam.region_number] = temp_well_nat[nam.region_number].astype('int')
        temp_well_nat = temp_well_nat.reset_index().merge(self.wells_per_rig, how='left', left_on=[nam.region_number], right_on=[nam.region]).set_index('index')
        temp_well_nat[nam.rigs] = temp_well_nat[nam.wells] * (1 / temp_well_nat[nam.wells_per_rig])

        #Get cumulative sum of rigs required
        temp_well_nat[nam.national_cum_rig_count] = temp_well_nat[nam.rigs].cumsum()

        #Calculate rigs required for already producing wells
        producing_wells = self.producing_wells[[self.rest_curcalyr]].copy()
        producing_wells = producing_wells.merge(self.wells_per_rig, how='left', left_index = True, right_on=[nam.region])
        producing_wells[nam.rigs] = producing_wells[self.rest_curcalyr] * (1 / producing_wells[nam.wells_per_rig])
        producing_rigs = producing_wells[nam.rigs].sum()

        #Add rigs required for already producing wells
        temp_well_nat[nam.producing_rigs] = producing_rigs
        temp_well_nat[nam.national_cum_rig_count] = temp_well_nat[nam.national_cum_rig_count] + temp_well_nat[nam.producing_rigs]

        #Apply national cumulative rig count to projects
        self.projects[nam.national_cum_rig_count] = temp_well_nat[nam.national_cum_rig_count]

        pass


    def apply_footage_constraints(self):
        """
        Apply footage constraints to projects based on project npv rank.

            * Assign two footage constraints, total and regional
            * There's some flexibility in the regional constraints to represent rigs moving around, but no flexibility in the total constraint
            * Select projects until footage > footage constraint
            * Only 85% of total rigs go to developing projects, the rest goes to exploration

        Returns
        -------
        self.projects : df
            Master DataFrame containing all projects to be processed in current model year
        """
        ###Apply footage constraints
        #Combine lat length and depth and multiply by number of wells
        temp_footage = pd.DataFrame( {nam.footage: (self.projects[nam.lateral_length_ft].copy() + self.projects[nam.drill_depth_ft].copy())})
        temp_footage[nam.footage] = temp_footage[nam.footage].mul((self.project_drilling.copy() + self.project_dryholes.copy()))  # Multiply by number of wells

        #Merge to projects for ranking
        temp_footage = temp_footage.merge(self.projects[[nam.region_number, nam.net_present_value, nam.last_year_drilling_sort]], left_index=True, right_index=True)
        temp_footage = temp_footage.sort_values([nam.last_year_drilling_sort, nam.net_present_value], ascending=False) #Sort by past drilling then profitability

        # Get cumulative sum by region and add producing wells
        temp_footage_reg = temp_footage.copy()
        temp_footage_reg[nam.region_cum_footage] = temp_footage_reg.groupby(nam.region_number)[nam.footage].cumsum()
        temp_footage_reg = temp_footage_reg.reset_index().merge(self.producing_footage[[self.rest_curcalyr]], how='left', left_on=nam.region_number, right_index = True).set_index('index')
        temp_footage_reg[nam.region_cum_footage] = temp_footage_reg[nam.region_cum_footage] + temp_footage_reg[self.rest_curcalyr]

        #Solve for footage constraint and apply to self.projects
        temp_footage_reg = temp_footage_reg.reset_index().merge(self.footage_constraint, how='left', on=[nam.region_number]).set_index('index')
        #self.projects.update(temp_footage_reg)
        self.projects[nam.region_cum_footage] = temp_footage_reg[nam.region_cum_footage]
        self.projects[nam.region_footage_constraint] = temp_footage_reg[nam.region_footage_constraint]

        #Multiply footage constraint by rig use ratio (the remainder of rigs are being used for exploration)
        self.projects[nam.region_footage_constraint] = self.projects[nam.region_footage_constraint].mul(self.dev_const_ratio)

        # Provide some slack to regional footage constraints for rigs to move around (compensated for by national constraint so total rigs never exceed national limit)
        self.projects[nam.region_footage_constraint] = self.projects[nam.region_footage_constraint].mul(1.1)


        ###Calculate national footage constraint and cumulative values
        #Calculate national footage constraint and apply rig use ratio (the remainder of rigs are being used for exploration)
        self.projects[nam.national_footage_constraint] = self.footage_constraint[nam.region_footage_constraint].sum()
        self.projects[nam.national_footage_constraint] = self.projects[nam.national_footage_constraint].mul(self.dev_const_ratio)
        if self.rest_curcalyr <= self.parent.steo_years[-1]:
            self.projects[nam.national_footage_constraint] = self.projects[nam.national_footage_constraint].mul(1.1)  # loosen constraint a bit in steo years to bump up production

        #Calculate national footage cumulative sum
        temp_footage_nat = temp_footage.copy()
        temp_footage_nat[nam.national_cum_footage] = temp_footage_reg[nam.footage].cumsum()

        #Add producing footage
        producing_footage = self.producing_footage[self.rest_curcalyr].sum()
        temp_footage_nat[nam.national_cum_footage] = temp_footage_nat[nam.national_cum_footage] + producing_footage

        #Apply national cumulative footage to projects
        self.projects[nam.national_cum_footage] = temp_footage_nat[nam.national_cum_footage]

        pass


    def select_projects(self):
        """Select projects that are economical produce in the current run iteration.

            1. Create all the relevant constraint and run masks:
                a. npv_mask: If a project has positive npv it is eligible to be selected
                b. past_drilling_mask: If a project started drilling, keep drilling
                c. max_wells_mask: If a project hits 70% of max wells, revert to calculating based on economics (overrides past_drilling_mask)
                d. proc_code_mask: Only apply past drilling mask to developing primary production projects
                e. reg_rig_mask: Regional rig constraint mask
                f. nat_rig_mask: National rig constraint mask
                g. reg_footage_mask: Regional footage constraint mask
                h. nat_footage_mask: National footage constraint mask

            2. Apply capital constraint and apply capital constraint mask
            3. Apply selected mask to relevant production to get production and well values
            4. Generate annual project output tables
            5. Update iterative values (i.e. past_wells)
            6. Remove projects that are no longer eligible from production (i.e. exhausted discovered vertical projects) from the master projects table

        Returns
        -------
        self.projects : df
            Master DataFrame containing all projects to be processed in current model year

        self.crude_production : df
            DataFrame of onshore crude oil production

        self.natgas_production : df
            DataFrame of onshore natural gas production

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

        self.wells : df
            DataFrame of onshore wells

        self.dryholes : df
            DataFrame of onshore wells
        """

        ###Prepare masks
        #Only select projects with positive NPV
        npv_mask            = self.projects[nam.net_present_value] > 0.0

        #For developing primary production projects, if drilling has started, keep going until cap is hit
        past_drilling_mask  =  (self.projects[nam.last_year_drilling] >= 10) | ((self.projects[nam.hist_year_wells] >= 10) & (self.rest_curcalyr <= self.parent.steo_years[-1]))
        max_wells_mask      = self.projects[nam.max_wells] >= self.projects[nam.past_wells] #Simulate well decline
        proc_code_mask      = (self.projects[nam.process_code] != 10) & (self.projects[nam.process_code] != 11) & (self.projects[nam.process_code] <= 15)
        past_drilling_mask  = past_drilling_mask & max_wells_mask & proc_code_mask

        #Apply rig constraints
        reg_rig_mask            = self.projects[nam.region_cum_rig_count]   <= self.projects[nam.region_rig_constraint]
        nat_rig_mask            = self.projects[nam.national_cum_rig_count] <= self.projects[nam.national_rig_constraint]

        #Apply footage constraints
        reg_footage_mask        = self.projects[nam.region_cum_footage] <= self.projects[nam.region_footage_constraint]
        nat_footage_mask        = self.projects[nam.national_cum_footage] <= self.projects[nam.national_cum_footage]

        #Start with all True
        selected_mask = True

        #Apply masks
        selected_mask = selected_mask & npv_mask

        if self.rest_curcalyr > self.parent.steo_years[0]: #Add drilling restraints for natural gas and oil plays
            selected_mask = selected_mask & reg_rig_mask & nat_rig_mask
            selected_mask = selected_mask & reg_footage_mask & nat_footage_mask
        else:#No drilling restraints for history year
            pass

        #In startoff year when we're coming off history drilling can only be from wells that we know are producing
        if self.rest_curcalyr == self.zero_year:
            selected_mask = selected_mask & past_drilling_mask
        else:
            selected_mask = selected_mask | past_drilling_mask


        ###Apply Capital Constraint
        #Create Cumulative Capital column in projects, only including projects within footage and rig constraints
        #Start by getting base capital expenditures for exploration
        undiscovered_drilling_mask = self.projects[nam.discovery_year] == self.rest_curcalyr
        undiscovered_drilling = self.projects.loc[undiscovered_drilling_mask, nam.projected_und_wells].fillna(0.0).copy()
        undiscovered_drilling_capital = undiscovered_drilling.mul(self.projects[nam.drill_cost])
        base_cap = undiscovered_drilling_capital.sum()

        #Get Project and dryhole drilling
        temp_drilling = self.project_drilling + self.project_dryholes
        temp_cap = temp_drilling.mul(self.projects[nam.capital_cost])
        temp_cap = temp_cap.reindex(self.projects.index)

        #Create capital expenditure cumulative sum for selected projects
        temp_cap = temp_cap[selected_mask]
        cum_cap = base_cap + temp_cap.cumsum(axis=0)

        #Apply cumulative capital column to Projects Df
        self.projects[nam.cumulative_capital].update(cum_cap)

        #Add capital constraint to selected mask
        capital_mask = self.projects[nam.cumulative_capital] <= self.projects[nam.capital_constraint]
        selected_mask = selected_mask & capital_mask


        ###Get production from cash_flow
        #Crude
        temp = self.cash_flow.crude_production.loc[selected_mask].mul(self.project_drilling[selected_mask], axis=0)
        temp.columns = temp.columns + self.rest_curcalyr
        labels = [i for i in temp.columns if i > self.parent.final_aeo_year]
        temp = temp.drop(labels, axis=1)
        temp[nam.process_code]                  = self.projects.loc[selected_mask, nam.process_code]
        temp[nam.resid]                         = self.projects.loc[selected_mask, nam.resid]
        temp[nam.district_number]               = self.projects.loc[selected_mask, nam.district_number]
        temp[nam.region_number]                 = self.projects.loc[selected_mask, nam.region_number]
        temp[nam.federal_land]                  = self.projects.loc[selected_mask, nam.federal_land]
        temp[nam.well_type_number]              = self.projects.loc[selected_mask, nam.well_type_number]
        temp[nam.oil_type_number]               = self.projects.loc[selected_mask, nam.oil_type_number]
        temp[nam.gas_type_number]               = self.projects.loc[selected_mask, nam.gas_type_number]
        temp[nam.lfmm_crude_type]               = self.projects.loc[selected_mask, nam.lfmm_crude_type]
        temp[nam.api]                           = self.projects.loc[selected_mask, nam.api]
        temp[nam.avg_api]                       = self.projects.loc[selected_mask, nam.avg_api]
        temp[nam.play]                          = self.projects.loc[selected_mask, nam.play]
        temp[nam.project_royalty_multiplier]    = self.projects.loc[selected_mask, nam.project_royalty_multiplier]
        temp[nam.year_production_start]         = self.rest_curcalyr
        self.crude_production = pd.concat([self.crude_production, temp], ignore_index=True)
        self.crude_production = self.crude_production.fillna(0)

        #Natural gas
        temp = self.cash_flow.natgas_production.loc[selected_mask].mul(self.project_drilling[selected_mask], axis=0)
        temp.columns = temp.columns + self.rest_curcalyr
        labels = [i for i in temp.columns if i > self.parent.final_aeo_year]
        temp = temp.drop(labels, axis=1)
        temp[nam.process_code]                  = self.projects.loc[selected_mask, nam.process_code]
        temp[nam.resid]                         = self.projects.loc[selected_mask, nam.resid]
        temp[nam.district_number]               = self.projects.loc[selected_mask, nam.district_number]
        temp[nam.region_number]                 = self.projects.loc[selected_mask, nam.region_number]
        temp[nam.federal_land]                  = self.projects.loc[selected_mask, nam.federal_land]
        temp[nam.well_type_number]              = self.projects.loc[selected_mask, nam.well_type_number]
        temp[nam.oil_type_number]               = self.projects.loc[selected_mask, nam.oil_type_number]
        temp[nam.gas_type_number]               = self.projects.loc[selected_mask, nam.gas_type_number]
        temp[nam.lfmm_crude_type]               = self.projects.loc[selected_mask, nam.lfmm_crude_type]
        temp[nam.api]                           = self.projects.loc[selected_mask, nam.api]
        temp[nam.avg_api]                       = self.projects.loc[selected_mask, nam.avg_api]
        temp[nam.play]                          = self.projects.loc[selected_mask, nam.play]
        temp[nam.project_royalty_multiplier]    = self.projects.loc[selected_mask, nam.project_royalty_multiplier]
        temp[nam.year_production_start]         = self.rest_curcalyr
        self.natgas_production = pd.concat([self.natgas_production, temp], ignore_index=True)
        self.natgas_production = self.natgas_production.fillna(0)

        #NGPLs
        temp_ngpl = self.cash_flow.ngpl_production.loc[selected_mask].mul(self.project_drilling[selected_mask], axis=0)

        #Apply Factors
        temp_ngpl = temp_ngpl.mul(0.000001, axis=1).div(365, axis=1) #convert to mmb/d
        temp_ethane_production = temp_ngpl.mul(self.projects.loc[selected_mask, 'NGPLET'], axis='index')
        temp_propane_production = temp_ngpl.mul(self.projects.loc[selected_mask, 'NGPLPR'], axis='index')
        temp_butane_production = temp_ngpl.mul(self.projects.loc[selected_mask, 'NGPLBU'], axis='index')
        temp_isobutane_production = temp_ngpl.mul(self.projects.loc[selected_mask, 'NGPLIS'], axis='index')
        temp_proplus_production = temp_ngpl.mul(self.projects.loc[selected_mask, 'NGPLPP'], axis='index')

        #Total NGPLs
        temp_ngpl.columns = temp_ngpl.columns + self.rest_curcalyr
        # from https://stackoverflow.com/a/4587920
        labels = [i for i in temp_ngpl.columns if i > self.parent.final_aeo_year]
        temp_ngpl = temp_ngpl.drop(labels, axis=1)
        temp_ngpl[nam.process_code]             = self.projects.loc[selected_mask, nam.process_code]
        temp_ngpl[nam.resid]                    = self.projects.loc[selected_mask, nam.resid]
        temp_ngpl[nam.district_number]          = self.projects.loc[selected_mask, nam.district_number]
        temp_ngpl[nam.region_number]            = self.projects.loc[selected_mask, nam.region_number]
        temp_ngpl[nam.federal_land]             = self.projects.loc[selected_mask, nam.federal_land]
        temp_ngpl[nam.play]                     = self.projects.loc[selected_mask, nam.play]
        temp_ngpl[nam.year_production_start]    = self.rest_curcalyr
        self.ngpl_production = pd.concat([self.ngpl_production, temp_ngpl], ignore_index=True)
        self.ngpl_production = self.ngpl_production.fillna(0)

        #Ethane
        temp_ethane_production.columns = temp_ethane_production.columns + self.rest_curcalyr
        # from https://stackoverflow.com/a/4587920
        labels = [i for i in temp_ethane_production.columns if i > self.parent.final_aeo_year]
        temp_ngpl = temp_ethane_production.drop(labels, axis=1)
        temp_ethane_production[nam.process_code]            = self.projects.loc[selected_mask, nam.process_code]
        temp_ethane_production[nam.resid]                   = self.projects.loc[selected_mask, nam.resid]
        temp_ethane_production[nam.district_number]         = self.projects.loc[selected_mask, nam.district_number]
        temp_ethane_production[nam.region_number]           = self.projects.loc[selected_mask, nam.region_number]
        temp_ethane_production[nam.federal_land]            = self.projects.loc[selected_mask, nam.federal_land]
        temp_ethane_production[nam.play]                    = self.projects.loc[selected_mask, nam.play]
        temp_ethane_production[nam.year_production_start]   = self.rest_curcalyr
        self.ngpl_ethane_production = pd.concat([self.ngpl_ethane_production, temp_ethane_production], ignore_index=True)
        self.ngpl_ethane_production = self.ngpl_ethane_production.fillna(0)

        #Propane
        temp_propane_production.columns = temp_propane_production.columns + self.rest_curcalyr
        # from https://stackoverflow.com/a/4587920
        labels = [i for i in temp_propane_production.columns if i > self.parent.final_aeo_year]
        temp_propane_production = temp_propane_production.drop(labels, axis=1)
        temp_propane_production[nam.process_code]           = self.projects.loc[selected_mask, nam.process_code]
        temp_propane_production[nam.resid]                  = self.projects.loc[selected_mask, nam.resid]
        temp_propane_production[nam.district_number]        = self.projects.loc[selected_mask, nam.district_number]
        temp_propane_production[nam.region_number]          = self.projects.loc[selected_mask, nam.region_number]
        temp_propane_production[nam.federal_land]           = self.projects.loc[selected_mask, nam.federal_land]
        temp_propane_production[nam.play]                   = self.projects.loc[selected_mask, nam.play]
        temp_propane_production[nam.year_production_start]  = self.rest_curcalyr
        self.ngpl_propane_production = pd.concat([self.ngpl_propane_production, temp_propane_production], ignore_index=True)
        self.ngpl_propane_production = self.ngpl_propane_production.fillna(0)

        #Butane
        temp_butane_production.columns = temp_butane_production.columns + self.rest_curcalyr
        # from https://stackoverflow.com/a/4587920
        labels = [i for i in temp_butane_production.columns if i > self.parent.final_aeo_year]
        temp_butane_production = temp_butane_production.drop(labels, axis=1)
        temp_butane_production[nam.process_code]            = self.projects.loc[selected_mask, nam.process_code]
        temp_butane_production[nam.resid]                   = self.projects.loc[selected_mask, nam.resid]
        temp_butane_production[nam.district_number]         = self.projects.loc[selected_mask, nam.district_number]
        temp_butane_production[nam.region_number]           = self.projects.loc[selected_mask, nam.region_number]
        temp_butane_production[nam.federal_land]            = self.projects.loc[selected_mask, nam.federal_land]
        temp_butane_production[nam.play]                    = self.projects.loc[selected_mask, nam.play]
        temp_butane_production[nam.year_production_start]   = self.rest_curcalyr
        self.ngpl_butane_production = pd.concat([self.ngpl_butane_production, temp_butane_production], ignore_index=True)
        self.ngpl_butane_production = self.ngpl_butane_production.fillna(0)

        #Isobutane
        temp_isobutane_production.columns = temp_isobutane_production.columns + self.rest_curcalyr
        # from https://stackoverflow.com/a/4587920
        labels = [i for i in temp_isobutane_production.columns if i > self.parent.final_aeo_year]
        temp_isobutane_production = temp_isobutane_production.drop(labels, axis=1)
        temp_isobutane_production[nam.process_code]             = self.projects.loc[selected_mask, nam.process_code]
        temp_isobutane_production[nam.resid]                    = self.projects.loc[selected_mask, nam.resid]
        temp_isobutane_production[nam.district_number]          = self.projects.loc[selected_mask, nam.district_number]
        temp_isobutane_production[nam.region_number]            = self.projects.loc[selected_mask, nam.region_number]
        temp_isobutane_production[nam.federal_land]             = self.projects.loc[selected_mask, nam.federal_land]
        temp_isobutane_production[nam.play]                     = self.projects.loc[selected_mask, nam.play]
        temp_isobutane_production[nam.year_production_start]    = self.rest_curcalyr
        self.ngpl_isobutane_production = pd.concat([self.ngpl_isobutane_production, temp_isobutane_production], ignore_index=True)
        self.ngpl_isobutane_production = self.ngpl_isobutane_production.fillna(0)

        #Pentanes
        temp_proplus_production.columns = temp_proplus_production.columns + self.rest_curcalyr
        # from https://stackoverflow.com/a/4587920
        labels = [i for i in temp_proplus_production.columns if i > self.parent.final_aeo_year]
        temp_proplus_production = temp_proplus_production.drop(labels, axis=1)
        temp_proplus_production[nam.process_code]           = self.projects.loc[selected_mask, nam.process_code]
        temp_proplus_production[nam.resid]                  = self.projects.loc[selected_mask, nam.resid]
        temp_proplus_production[nam.district_number]        = self.projects.loc[selected_mask, nam.district_number]
        temp_proplus_production[nam.region_number]          = self.projects.loc[selected_mask, nam.region_number]
        temp_proplus_production[nam.federal_land]           = self.projects.loc[selected_mask, nam.federal_land]
        temp_proplus_production[nam.play]                   = self.projects.loc[selected_mask, nam.play]
        temp_proplus_production[nam.year_production_start]  = self.rest_curcalyr
        self.ngpl_proplus_production = pd.concat([self.ngpl_proplus_production, temp_proplus_production], ignore_index=True)
        self.ngpl_proplus_production = self.ngpl_proplus_production.fillna(0)

        #Wells
        temp = pd.DataFrame(self.project_drilling.loc[selected_mask].copy())
        temp.columns = temp.columns + self.rest_curcalyr
        labels = [i for i in temp.columns if i > self.parent.final_aeo_year]
        temp = temp.drop(labels, axis=1)
        temp[nam.process_code]          = self.projects.loc[selected_mask, nam.process_code]
        temp[nam.resid]                 = self.projects.loc[selected_mask, nam.resid]
        temp[nam.district_number]       = self.projects.loc[selected_mask, nam.district_number]
        temp[nam.region_number]         = self.projects.loc[selected_mask, nam.region_number]
        temp[nam.well_type_number]      = self.projects.loc[selected_mask, nam.well_type_number]
        temp[nam.well_decline_limit]    = self.projects.loc[selected_mask, nam.well_decline_limit]
        temp[nam.past_wells]            = self.projects.loc[selected_mask, nam.past_wells]
        temp[nam.play]                  = self.projects.loc[selected_mask, nam.play]
        temp[nam.hsm_index]             = self.projects.loc[selected_mask, nam.hsm_index]
        temp[nam.year_production_start] = self.rest_curcalyr
        self.wells = pd.concat([self.wells, temp], ignore_index=True)
        self.wells = self.wells.fillna(0)

        #Update projects past drilling
        self.projects = self.projects.merge(temp[self.rest_curcalyr], how = 'left', left_index = True, right_index = True)
        self.projects[self.rest_curcalyr] = self.projects[self.rest_curcalyr].fillna(0)

        if self.rest_curcalyr > self.parent.history_year:
            self.projects[nam.past_wells] += self.projects[self.rest_curcalyr]
        self.projects = self.projects.drop([self.rest_curcalyr], axis = 1)

        #Dryholes
        temp = pd.DataFrame(self.project_dryholes.loc[selected_mask].copy())
        temp.columns = temp.columns + self.rest_curcalyr
        labels = [i for i in temp.columns if i > self.parent.final_aeo_year]
        temp = temp.drop(labels, axis=1)
        temp[nam.process_code]          = self.projects.loc[selected_mask, nam.process_code]
        temp[nam.resid]                 = self.projects.loc[selected_mask, nam.resid]
        temp[nam.district_number]       = self.projects.loc[selected_mask, nam.district_number]
        temp[nam.region_number]         = self.projects.loc[selected_mask, nam.region_number]
        temp[nam.well_type_number]      = self.projects.loc[selected_mask, nam.well_type_number]
        temp[nam.well_decline_limit]    = self.projects.loc[selected_mask, nam.well_decline_limit]
        temp[nam.past_wells]            = self.projects.loc[selected_mask, nam.past_wells]
        temp[nam.play]                  = self.projects.loc[selected_mask, nam.play]
        temp[nam.hsm_index]             = self.projects.loc[selected_mask, nam.hsm_index]
        temp[nam.year_production_start] = self.rest_curcalyr
        self.dryholes = pd.concat([self.dryholes, temp], ignore_index=True)
        self.dryholes = self.dryholes.fillna(0)


        #CO2 injected
        temp_co2_inj = pd.DataFrame(self.project_co2_inj.loc[selected_mask].copy())
        temp_co2_inj['wells'] = self.project_drilling[selected_mask].copy()
        temp_co2_inj[temp_co2_inj.columns] = temp_co2_inj[temp_co2_inj.columns].mul(temp_co2_inj['wells'], axis = 0)
        temp_co2_inj = temp_co2_inj.drop(['wells'], axis = 1)
        temp_co2_inj.columns = temp_co2_inj.columns + self.rest_curcalyr
        # from https://stackoverflow.com/a/4587920
        labels = [i for i in temp_co2_inj.columns if i > self.parent.final_aeo_year]
        temp_co2_inj = temp_co2_inj.drop(labels, axis=1)
        temp_co2_inj[nam.process_code]           = self.projects.loc[selected_mask, nam.process_code]
        temp_co2_inj[nam.resid]                  = self.projects.loc[selected_mask, nam.resid]
        temp_co2_inj[nam.district_number]        = self.projects.loc[selected_mask, nam.district_number]
        temp_co2_inj[nam.region_number]          = self.projects.loc[selected_mask, nam.region_number]
        temp_co2_inj[nam.federal_land]           = self.projects.loc[selected_mask, nam.federal_land]
        temp_co2_inj[nam.play]                   = self.projects.loc[selected_mask, nam.play]
        temp_co2_inj[nam.year_production_start]  = self.rest_curcalyr
        self.co2_injected = pd.concat([self.co2_injected, temp_co2_inj], ignore_index=True)
        self.co2_injected = self.co2_injected.fillna(0)


        #CO2 Recycled
        temp_co2_recy = pd.DataFrame(self.project_co2_recy.loc[selected_mask].copy())
        temp_co2_recy['wells'] = self.project_drilling[selected_mask].copy()
        temp_co2_recy[temp_co2_recy.columns] = temp_co2_recy[temp_co2_recy.columns].mul(temp_co2_recy['wells'], axis = 0)
        temp_co2_recy = temp_co2_recy.drop(['wells'], axis = 1)
        temp_co2_recy.columns = temp_co2_recy.columns + self.rest_curcalyr
        # from https://stackoverflow.com/a/4587920
        labels = [i for i in temp_co2_recy.columns if i > self.parent.final_aeo_year]
        temp_co2_recy = temp_co2_recy.drop(labels, axis=1)
        temp_co2_recy[nam.process_code]           = self.projects.loc[selected_mask, nam.process_code]
        temp_co2_recy[nam.resid]                  = self.projects.loc[selected_mask, nam.resid]
        temp_co2_recy[nam.district_number]        = self.projects.loc[selected_mask, nam.district_number]
        temp_co2_recy[nam.region_number]          = self.projects.loc[selected_mask, nam.region_number]
        temp_co2_recy[nam.federal_land]           = self.projects.loc[selected_mask, nam.federal_land]
        temp_co2_recy[nam.play]                   = self.projects.loc[selected_mask, nam.play]
        temp_co2_recy[nam.year_production_start]  = self.rest_curcalyr
        self.co2_recycled = pd.concat([self.co2_recycled, temp_co2_recy], ignore_index=True)
        self.co2_recycled = self.co2_recycled.fillna(0)


        ###Apply selected EOR wells to self.wells
        #Merge fixed well counts to wells output df
        eor_mask = (self.wells[nam.process_code] == 10) | (self.wells[nam.process_code] == 11)
        selected_eor_mask = eor_mask & selected_mask
        temp_wells = self.wells[selected_eor_mask].copy()

        #Get fixed project wells
        temp_projects = self.projects[selected_eor_mask].copy()
        prod_range = list(range(self.evaluation_years))
        well_range = [('WL' + str(x + 1)) for x in prod_range]
        temp_fixed_wells = temp_projects[well_range]

        #Get output range based on current model year
        output_range = list(range(self.rest_curcalyr, self.final_year + 1))
        output_length = len(output_range)

        #Set fixed wells dimensions equal to output range dimensions
        temp_fixed_wells = temp_fixed_wells.iloc[:, : output_length]

        #Replace existing well count with fixed wells and update master df
        temp_wells[output_range] = temp_fixed_wells[temp_fixed_wells.columns]
        self.wells.update(temp_wells)

        #Set last year drilling for non EOR projects
        self.projects.loc[selected_mask, nam.last_year_drilling] = self.project_drilling.loc[selected_mask]


        ###Create selected projects df for debug
        self.projects_selected = self.projects[selected_mask].copy()


        ###Remove depleted projects from the projects list
        projects_mask = (self.projects[nam.past_wells] > self.projects[nam.max_wells]) & (self.projects[nam.process_code] >= 16)
        self.projects = self.projects[~projects_mask]
        self.project_co2_inj = self.project_co2_inj[~projects_mask]
        self.project_co2_recy = self.project_co2_recy[~projects_mask]


        ###Remove EOR projects that are not eligible from the projects list and CO2 EOR lists
        eor_mask = (self.projects[nam.process_code] == 10) | (self.projects[nam.process_code] == 11)
        econ_life_mask = (self.projects[nam.econ_life] >= 2050) | (self.projects[nam.econ_life] + 11 < self.rest_curcalyr )
        eligible_mask = self.projects[nam.eligible] == 0
        eor_mask = (eor_mask & econ_life_mask) | (eor_mask & eligible_mask)

        self.projects = self.projects[~eor_mask]
        self.project_co2_inj = self.project_co2_inj[~eor_mask]
        self.project_co2_recy = self.project_co2_recy[~eor_mask]

        pass


    def debug_onshore(self):
        """Produce debug outputs for Onshore Submodule.

        Returns
        -------
        None

        """
        ### Debug Projects
        self.projects_selected.to_csv(self.output_path + 'projects_debug//' + 'hsm_on_selected_projects' + '_' + str(self.rest_curcalyr) + '_' + str(self.parent.current_iteration) + '.csv')
        self.projects.to_csv(self.output_path + 'projects_debug//' + 'hsm_on_projects' + '_' + str(self.rest_curcalyr) + '_' + str(self.parent.current_iteration) + '.csv')

        ### Debug production
        #Get debug_range
        debug_range = list(range(self.zero_year, self.parent.final_aeo_year + 1))
        self.crude_production[debug_range] = self.crude_production[debug_range].astype(float)
        self.natgas_production[debug_range] = self.natgas_production[debug_range].astype(float)
        self.ngpl_production[debug_range] = self.ngpl_production[debug_range].astype(float)
        self.wells[debug_range] = self.wells[debug_range].astype(float)

        #Debug Crude
        temp = self.crude_production.copy()
        temp[debug_range] = temp[debug_range].div(365)
        temp = temp.drop([nam.gas_type, nam.oil_type, nam.hsm_index,'API','avg_api', 'year_production_start','federal_land'], axis = 1)
        temp[[nam.resid, nam.play, nam.process_code, nam.region_number, nam.district_number, nam.well_type_number, nam.lfmm_crude_type, 'oil_type_number', 'gas_type_number']] \
            = temp[[nam.resid, nam.play, nam.process_code, nam.region_number, nam.district_number, nam.well_type_number, nam.lfmm_crude_type, 'oil_type_number', 'gas_type_number']].astype('str')
        temp.groupby([nam.resid, nam.play, nam.process_code, nam.region_number, nam.district_number, nam.well_type_number, nam.lfmm_crude_type, 'oil_type_number', 'gas_type_number']).sum().to_csv(self.output_path + 'module_results_debug//' + 'hsm_on_crude_fields.csv')
        temp[([nam.process_code] + debug_range)].groupby(nam.process_code).sum().to_csv(self.output_path + 'module_results_debug//' + 'hsm_on_crude_proc_code.csv')
        temp[([nam.region_number] + debug_range)].groupby(nam.region_number).sum().to_csv(self.output_path + 'module_results_debug//' + 'hsm_on_crude_region_num.csv')
        temp[([nam.district_number] + debug_range)].groupby(nam.district_number).sum().to_csv(self.output_path + 'module_results_debug//' + 'hsm_on_crude_district_num.csv')
        temp[([nam.play] + debug_range)].groupby(nam.play).sum().to_csv(self.output_path + 'module_results_debug//' + 'hsm_on_crude_play.csv')

        #Debug Natgas
        temp = self.natgas_production.copy()
        temp = temp.drop([nam.gas_type, nam.oil_type, nam.hsm_index,'API','avg_api', 'year_production_start','federal_land'], axis = 1)
        temp[[nam.resid, nam.play, nam.process_code, nam.region_number, nam.district_number, 'oil_type_number', 'gas_type_number']] \
            = temp[[nam.resid, nam.play, nam.process_code, nam.region_number, nam.district_number, 'oil_type_number', 'gas_type_number']].astype('str')
        temp.groupby([nam.resid, nam.play, nam.process_code, nam.region_number, nam.district_number, 'oil_type_number', 'gas_type_number']).sum().to_csv(self.output_path + 'module_results_debug//' + 'hsm_on_natgas_fields.csv')
        temp[([nam.process_code] + debug_range)].groupby(nam.process_code).sum().to_csv(self.output_path + 'module_results_debug//' + 'hsm_on_natgas_proc_code.csv' )
        temp[([nam.region_number] + debug_range)].groupby(nam.region_number).sum().to_csv(self.output_path + 'module_results_debug//' + 'hsm_on_natgas_region_num.csv' )
        temp[([nam.play] + debug_range)].groupby(nam.play).sum().to_csv(self.output_path + 'module_results_debug//' + 'hsm_on_natgas_play.csv' )

        #Debug NGPLs
        temp = self.ngpl_production.copy()
        temp = temp.drop(['federal_land'], axis = 1)
        temp[[nam.resid, nam.play, nam.process_code, nam.region_number, nam.district_number]] = temp[[nam.resid, nam.play, nam.process_code, nam.region_number, nam.district_number]].astype('str')
        temp.groupby([nam.resid, nam.play, nam.process_code, nam.region_number, nam.district_number]).sum().to_csv(self.output_path + 'module_results_debug//' + 'hsm_on_ngpls_fields.csv'    )
        temp[([nam.process_code] + debug_range)].groupby(nam.process_code).sum().to_csv(self.output_path + 'module_results_debug//' + 'hsm_on_ngpls_proc_code.csv' )
        temp[([nam.region_number] + debug_range)].groupby(nam.region_number).sum().to_csv(self.output_path + 'module_results_debug//' + 'hsm_on_ngpls_region_num.csv' )
        temp[([nam.district_number] + debug_range)].groupby(nam.district_number).sum().to_csv(self.output_path + 'module_results_debug//' + 'hsm_on_ngpls_district_num.csv' )
        temp[([nam.play] + debug_range)].groupby(nam.play).sum().to_csv(self.output_path + 'module_results_debug//' + 'hsm_on_ngpls_play.csv' )

        ##Debug Wells
        temp = self.wells.copy()
        temp = temp.drop([nam.year_production_start, nam.past_wells], axis = 1)
        temp[[nam.resid, nam.play, nam.process_code, nam.region_number, nam.district_number, 'well_type_number', nam.well_decline_limit]] = temp[[nam.resid, nam.play, nam.process_code, nam.region_number, nam.district_number, 'well_type_number', nam.well_decline_limit]].astype('str')
        temp.groupby([nam.resid, nam.play, nam.process_code, nam.region_number, nam.district_number, 'well_type_number', nam.well_decline_limit]).sum().to_csv(self.output_path + 'module_results_debug//' + 'hsm_on_wells_fields.csv')
        temp[([nam.process_code] + debug_range)].groupby(nam.process_code).sum().to_csv(self.output_path + 'module_results_debug//' + 'hsm_on_wells_proc_code.csv' )
        temp[([nam.region_number] + debug_range)].groupby(nam.region_number).sum().to_csv(self.output_path + 'module_results_debug//' + 'hsm_on_wells_region_num.csv' )
        temp[([nam.district_number] + debug_range)].groupby(nam.district_number).sum().to_csv(self.output_path + 'module_results_debug//' + 'hsm_on_wells_district_num.csv' )
        temp[([nam.play] + debug_range)].groupby(nam.play).sum().to_csv(self.output_path + 'module_results_debug//' + 'hsm_on_wells_play.csv')

        #CO2 Cost
        temp = self.projects_selected[[nam.play,nam.region_number,nam.net_present_value]].copy()
        temp = temp.groupby([nam.play,nam.region_number]).sum()
        temp.to_csv(self.output_path + 'module_results_debug//' + 'hsm_on_co2cost_play.csv')

        #CO2 Recycled
        temp = self.co2_recycled.copy()
        temp[[nam.play, nam.region_number]] = temp[[nam.play, nam.region_number]].astype(str)
        temp = temp.drop(['process_code','resid','district_number','federal_land', 'year_production_start'], axis = 1)
        temp[([nam.region_number] + debug_range)].groupby(nam.region_number).sum().to_csv(self.output_path + 'module_results_debug//' + 'hsm_on_co2rec_region.csv' )
        temp.groupby([nam.play, nam.region_number]).sum().to_csv(self.output_path + 'module_results_debug//' + 'hsm_on_co2rec_play.csv')

        #CO2 Injected
        temp = self.co2_injected.copy()
        temp[[nam.play, nam.region_number]] = temp[[nam.play, nam.region_number]].astype(str)
        temp = temp.drop(['process_code','resid','district_number','federal_land', 'year_production_start'], axis = 1)
        temp[([nam.region_number] + debug_range)].groupby(nam.region_number).sum().to_csv(self.output_path + 'module_results_debug//' + 'hsm_on_co2inj_region.csv' )
        temp.groupby([nam.play, nam.region_number]).sum().to_csv(self.output_path + 'module_results_debug//' + 'hsm_on_co2inj_play.csv')

        self.logger.info('Finish Debug')

        pass


    def write_intermediate_variables(self):
        """
        Write local variables to restart file to be read back in each iteration.

        Returns
        -------
        self.parent.hsm_vars.on_projects : df
            Master DataFrame containing all projects to be processed in current model year

        self.parent.hsm_vars.on_projects_undiscovered : df
            DataFrame of undiscovered projects

        self.parent.hsm_vars.on_projects_discovered : df
            DataFrame of discovered projects that have not yet been assigned to the master project df (duplicates)

        self.parent.hsm_vars.on_crude_production : df
            DataFrame of onshore crude oil production

        self.parent.hsm_vars.on_natgas_production : df
            DataFrame of onshore natural gas production

        self.parent.hsm_vars.on_ngpl_production : df
            DataFrame of onshore NGPL production

        self.parent.hsm_vars.on_ngpl_ethane_production : df
             DataFrame of onshore ethane production

        self.parent.hsm_vars.on_ngpl_propane_production : df
            DataFrame of onshore propane production

        self.parent.hsm_vars.on_ngpl_butane_production : df
            DataFrame of onshore butane production

        self.parent.hsm_vars.on_ngpl_isobutane_production : df
            DataFrame of onshore isobutane production

        self.parent.hsm_vars.on_ngpl_proplus_production : df
            DataFrame of onshore pentanes production

        self.parent.hsm_vars.on_wells : df
            DataFrame of onshore wells

        self.parent.hsm_vars.on_producing_wells : df
            DataFrame of onshore legacy producing project wells

        self.parent.hsm_vars.on_producing_footage : df
            DataFrame of osnhore legacy producing project footage

        self.parent.hsm_vars.on_dryholes : df
            DataFrame of onshore dryholes

        self.parent.hsm_vars.on_exploratory_wells : df
            DataFrame of onshore exploratory wells

        self.parent.hsm_vars.on_co2_injected : df
            DataFrame of onshore CO2 used

        self.parent.hsm_vars.on_co2_recycled : df
            DataFrame of onshore CO2 recycled

        """
        self.parent.hsm_vars.on_projects                   = self.projects.copy()
        self.parent.hsm_vars.on_projects_undiscovered      = self.projects_undiscovered.copy()
        self.parent.hsm_vars.on_projects_discovered        = self.projects_discovered.copy()
        self.parent.hsm_vars.on_crude_production           = self.crude_production.copy()
        self.parent.hsm_vars.on_natgas_production          = self.natgas_production.copy()
        self.parent.hsm_vars.on_ngpl_production            = self.ngpl_production.copy()
        self.parent.hsm_vars.on_ngpl_ethane_production     = self.ngpl_ethane_production.copy()
        self.parent.hsm_vars.on_ngpl_propane_production    = self.ngpl_propane_production.copy()
        self.parent.hsm_vars.on_ngpl_butane_production     = self.ngpl_butane_production.copy()
        self.parent.hsm_vars.on_ngpl_isobutane_production  = self.ngpl_isobutane_production.copy()
        self.parent.hsm_vars.on_ngpl_proplus_production    = self.ngpl_proplus_production.copy()
        self.parent.hsm_vars.on_wells                      = self.wells.copy()
        self.parent.hsm_vars.on_producing_wells            = self.producing_wells.copy()
        self.parent.hsm_vars.on_producing_footage          = self.producing_footage.copy()
        self.parent.hsm_vars.on_dryholes                   = self.dryholes.copy()
        self.parent.hsm_vars.on_exploratory_wells          = self.exploratory_wells.copy()
        self.parent.hsm_vars.on_co2_injected               = self.co2_injected.copy()
        self.parent.hsm_vars.on_co2_recycled               = self.co2_recycled.copy()
        self.parent.hsm_vars.on_co2_eor_wells              = self.co2_eor_wells.copy()
        self.parent.hsm_vars.on_co2_net_cost               = self.co2_net_cost.copy()

        #Debug
        if self.parent.hsm_var_debug_switch == True:
            self.projects.to_csv(self.parent.hsm_var_output_path + 'hsm_on_projects.csv')
            self.projects_undiscovered.to_csv(self.parent.hsm_var_output_path + 'hsm_on_projects_undiscovered.csv')
            self.crude_production.to_csv(self.parent.hsm_var_output_path + 'hsm_on_crude_production.csv')
            self.natgas_production.to_csv(self.parent.hsm_var_output_path + 'hsm_on_natgas_production.csv')
            self.ngpl_production.to_csv(self.parent.hsm_var_output_path + 'hsm_on_ngpl_production.csv')
            self.ngpl_ethane_production.to_csv(self.parent.hsm_var_output_path + 'hsm_on_ngpl_ethane_production.csv')
            self.ngpl_propane_production.to_csv(self.parent.hsm_var_output_path + 'hsm_on_ngpl_propane_production.csv')
            self.ngpl_butane_production.to_csv(self.parent.hsm_var_output_path + 'hsm_on_ngpl_butane_production.csv')
            self.ngpl_isobutane_production.to_csv(self.parent.hsm_var_output_path + 'hsm_on_ngpl_isobutane_production.csv')
            self.ngpl_proplus_production.to_csv(self.parent.hsm_var_output_path + 'hsm_on_ngpl_proplus_production.csv')
            self.wells.to_csv(self.parent.hsm_var_output_path + 'hsm_on_wells.csv')
            self.producing_wells.to_csv(self.parent.hsm_var_output_path + 'hsm_on_producing_wells.csv')
            self.producing_footage.to_csv(self.parent.hsm_var_output_path + 'hsm_on_producing_footage.csv')
            self.dryholes.to_csv(self.parent.hsm_var_output_path + 'hsm_on_dryholes.csv')
            self.exploratory_wells.to_csv(self.parent.hsm_var_output_path + 'hsm_on_exploratory_wells.csv')
            self.co2_purchased.to_csv(self.parent.hsm_var_output_path + 'hsm_on_co2_purchased.csv')
            self.co2_used.to_csv(self.parent.hsm_var_output_path + 'hsm_on_co2_used.csv')
            self.co2_injected.to_csv(self.parent.hsm_var_output_path + 'hsm_on_co2_injected.csv')
            self.co2_recycled.to_csv(self.parent.hsm_var_output_path + 'hsm_on_co2_recycled.csv')

        pass


    def report_results_unf(self):
        """Report results to restart variables and produce debug files.

        Returns
        -------
        self.restart.pmmout_rfqtdcrd : df
            Total crude production by HSM region

        self.restart.pmmout_rfqdcrd : df
            Total crude oil production by HSM region (not including EOR)

        self.restart.ogsmout_ogqcrrep : df
            Crude oil production by oil category

        self.restart.ogsmout_ogcoprd : df
            Crude oil production by lower 48 region

        self.restart.ogsmout_ogqshloil : df
            Crude oil production by select tight oil play

        self.restart.ogsmout_ogoilprd : df
            Crude oil production by oil type and HSM district

        self.restart.ogsmout_ogcrdprd : df
            Crude oil production by HSM region and crude type

        self.restart.ogsmout_ogcruderef : df
            Crude oil production by LFMM crude oil type and region

        self.restart.ogsmout_ogcrdheat : df
            Heat rate by type of crude oil

        self.restart.ogsmout_ogeorprd : df
            CO2 EOR crude oil production

        self.restart.ogsmout_ogenagprd : df
            Natural gas expected production by natural gas type and HSM district

        self.restart.ogsmout_ogrnagprd : df
             Natural gas realized production by natural gas type and HSM district

        self.restart.ogsmout_ogadgprd : df
            Natural gas associated dissolved production by oil type and HSM district

        self.restart.ogsmout_ogprdad : df
            Natural gas associated dissolved production by HSM region

        self.restart.ogsmout_ogqshlgas : df
            Natural gas production by select natural gas play

        self.restart.ogsmout_ogqngrep : df
            Natural gas production by natural gas type

        self.restart.ogsmout_ogprdugr : df
            Lower 48 unconventional natural gas production

        self.restart.ogsmout_ogregprd : df
            Total crude oil and natural gas production by production type

        self.restart.ogsmout_ogcowhp : df
            Crude oil wellhead price by HSM region

        self.restart.ogsmout_ogpcrwhp : df
            Crude oil HSM average wellhead price

        self.restart.ogsmout_ogngwhp : df
            Natural gas wellhead price by HSM region

        self.restart.ogsmout_ogogwells : df
            Total wells

        self.restart.ogsmout_ognowell : df
            Total completed wells

        self.restart.ogsmout_ogwellsl48 : df
            Total lower 48 wells

        self.restart.ogsmout_ogsrl48 : df
            Lower 48 drilling success rates

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

        self.restart.ogsmout_ogco2rec : df
            CO2 recycled by HSM region and CO2 type

        self.restart.ogsmout_ogco2inj : df
            CO2 injected by HSM region and CO2 type

        self.restart.ogsmout_ogco2pur : df
            CO2 purchased by HSM region and CO2 type

        self.restart.ogsmout_ogco2avl : df
            CO2 available by HSM region and CO2 type

        self.restart.ogsmout_ogco2prc : df
            CO2 price by HSM region and CO2 type

        self.restart.ogsmout_ogtechon : df
            HSM technology improvement rate

        """
        # ensure key columns have not changed type
        self.crude_production[nam.process_code] = self.crude_production[nam.process_code].astype(int)
        self.crude_production[nam.district_number] = self.crude_production[nam.district_number].astype(int)
        self.crude_production[nam.oil_type_number] = self.crude_production[nam.oil_type_number].astype(int)

        self.natgas_production[nam.process_code] = self.natgas_production[nam.process_code].astype(int)
        self.natgas_production[nam.district_number] = self.natgas_production[nam.district_number].astype(int)

        ### Update Play Map
        if (self.play_map_switch) & (self.rest_curcalyr == self.zero_year):
            temp_play_map = self.play_map.copy()
            temp_play_map = temp_play_map.reindex(list(range(1, 201))).reset_index(drop=True).fillna(0)
            temp_play_map.index = self.restart.ogsmout_play_map.index
            temp_play_map.columns = self.restart.ogsmout_play_map.columns
            temp_play_map.index.names = self.restart.ogsmout_play_map.index.names
            self.restart.ogsmout_play_map.update(temp_play_map)
        else:
            pass

        # Write Results
        if self.rest_curcalyr >= self.parent.steo_years[0]:
            ###Writing total crude production to restart (split by EOR)
            temp = self.crude_production[[self.rest_curcalyr, nam.oil_type_number]].copy()
            eor_mask = (temp[nam.oil_type_number] == 3) | (temp[nam.oil_type_number] == 4)

            #EOR
            temp_eor = temp[eor_mask].values.sum()
            self.restart.ogsmout_ogqcrrep.at[(1,self.rest_curcalyr), 'value'] = temp_eor/1000000

            #Conventional and Tight
            temp_oil = temp[~eor_mask].values.sum()
            self.restart.ogsmout_ogqcrrep.at[(2,self.rest_curcalyr), 'value'] = temp_oil/1000000


            ###Domestic Crude Oil Production by region for LFMM (including EOR)
            temp = self.crude_production.copy()[[nam.region_number, self.rest_curcalyr]]
            temp = temp.groupby(nam.region_number).sum()
            self.restart.pmmout_rfqtdcrd['value'].update(temp[[self.rest_curcalyr]].stack() / 1000000 / 365)

            #Switch regions 6 & 7 to match LFMM
            self.restart.pmmout_rfqtdcrd.at[(6, self.rest_curcalyr), 'value'] = temp.at[7, self.rest_curcalyr] / 1000000 / 365
            self.restart.pmmout_rfqtdcrd.at[(7, self.rest_curcalyr), 'value'] = temp.at[6, self.rest_curcalyr] / 1000000 / 365



            ###Crude Production by Lower 48 Region
            temp = self.restart.pmmout_rfqtdcrd.xs(self.rest_curcalyr, level = 1, drop_level = False).copy()
            temp = temp[temp.index.get_level_values(0).isin([1,2,3,4,5,6,7])].copy()
            self.restart.ogsmout_ogcoprd.update(temp)

            # Switch regions 6 & 7 to match LFMM
            self.restart.ogsmout_ogcoprd.at[(7, self.rest_curcalyr), 'value'] = temp.at[(6, self.rest_curcalyr), nam.value]
            self.restart.ogsmout_ogcoprd.at[(6, self.rest_curcalyr), 'value'] = temp.at[(7, self.rest_curcalyr), nam.value]


            ###Domestic Crude Oil Production by region for LFMM (not including EOR)
            temp = self.crude_production.copy()[[nam.region_number, nam.oil_type_number, self.rest_curcalyr]]
            temp = temp[temp[nam.oil_type_number].isin([1,2])]
            temp = temp.groupby(nam.region_number).sum()
            self.restart.pmmout_rfqdcrd['value'].update(temp[[self.rest_curcalyr]].stack() / 1000000 / 365)

            #Switch regions 6 & 7 to match LFMM
            self.restart.pmmout_rfqdcrd.at[(6, self.rest_curcalyr), 'value'] = temp.at[7, self.rest_curcalyr] / 1000000 / 365
            self.restart.pmmout_rfqdcrd.at[(7, self.rest_curcalyr), 'value'] = temp.at[6, self.rest_curcalyr] / 1000000 / 365


            ###Crude Oil production by select play
            temp = self.crude_production.copy()[[nam.play, nam.well_type_number, self.rest_curcalyr]]

            #Bakken
            bakken_temp = temp[temp[nam.play].isin([2804, 3110, 3111, 3112, 3113, 3114, 3115])].copy()
            self.restart.ogsmout_ogqshloil.at[(1, self.rest_curcalyr), 'value'] = bakken_temp[self.rest_curcalyr].sum() / 1000000 / 365

            #Eagle Ford
            eagle_ford_temp = temp[temp[nam.play].isin([4761, 4762, 4763])].copy()
            self.restart.ogsmout_ogqshloil.at[(2, self.rest_curcalyr), 'value'] = eagle_ford_temp[self.rest_curcalyr].sum() / 1000000 / 365

            #Woodford
            woodford_temp = temp[temp[nam.play].isin([5861, 5862, 5863, 5875, 99926039])].copy()
            self.restart.ogsmout_ogqshloil.at[(3, self.rest_curcalyr), 'value'] = woodford_temp[self.rest_curcalyr].sum() / 1000000 / 365

            #Austin Chalk
            ac_temp = temp[temp[nam.play].isin([4747, 4748, 4749])].copy()
            self.restart.ogsmout_ogqshloil.at[(4, self.rest_curcalyr), 'value'] = ac_temp[self.rest_curcalyr].sum() / 1000000 / 365

            #Spraberry
            spraberry_temp = temp[temp[nam.play].isin([4409])].copy()
            self.restart.ogsmout_ogqshloil.at[(5, self.rest_curcalyr), 'value'] = spraberry_temp[self.rest_curcalyr].sum() / 1000000 / 365

            #Niobrara
            niobrara_temp = temp[temp[nam.play].isin([3904, 3920, 99905037, 99943037, 99949033, 99949037])].copy()
            self.restart.ogsmout_ogqshloil.at[(6, self.rest_curcalyr), 'value'] = niobrara_temp[self.rest_curcalyr].sum() / 1000000 / 365

            #Avalon/Bonesprings
            ab_temp = temp[temp[nam.play].isin([4473])].copy()
            self.restart.ogsmout_ogqshloil.at[(7, self.rest_curcalyr), 'value'] = ab_temp[self.rest_curcalyr].sum() / 1000000 / 365

            #Monterey
            monterey_temp = temp[temp[nam.play].isin([1201, 99904010])].copy()
            self.restart.ogsmout_ogqshloil.at[(8, self.rest_curcalyr), 'value'] = monterey_temp[self.rest_curcalyr].sum() / 1000000 / 365

            #Wolfcamp
            wolfcamp_temp = temp[temp[nam.play].isin([4401])].copy()
            self.restart.ogsmout_ogqshloil.at[(9, self.rest_curcalyr), 'value'] = wolfcamp_temp[self.rest_curcalyr].sum() / 1000000 / 365

            #Utica
            utica_temp = temp[temp[nam.play].isin([6790, 6791, 6792, 6793])].copy()
            self.restart.ogsmout_ogqshloil.at[(10, self.rest_curcalyr), 'value'] = utica_temp[self.rest_curcalyr].sum() / 1000000 / 365

            #Other
            temp = temp[temp[nam.well_type_number].isin([2])] # Tight oil plays only. Excludes oil from other tight and shale gas plays
            other_temp = temp[~temp[nam.play].isin([2804, 3110, 3111, 3112, 3113, 3114, 3115, 4761, 4762, 4763, 5861, 5862, 5863, 5875, 99926039,
                                                    4747, 4748, 4749, 4409, 3904, 3920, 99905037, 99943037, 99949033, 99949037,
                                                    4473, 1201, 99904010, 4401, 6790, 6791, 6792, 6793])].copy()
            self.restart.ogsmout_ogqshloil.at[(15, self.rest_curcalyr), 'value'] = other_temp[self.rest_curcalyr].sum() / 1000000 / 365


            ###Crude Total Oil Production by oil type
            temp = self.crude_production.copy()
            temp = temp[[self.rest_curcalyr, nam.district_number, nam.oil_type_number]].groupby(
                [nam.district_number, nam.oil_type_number]).sum()
            temp = temp.stack() / (365 * 1000000)
            self.restart.ogsmout_ogoilprd['value'].update(temp)

            ###Oil production by LFMM crude type and HSM region, convert to thousand barrels per day
            temp = self.crude_production[[self.rest_curcalyr, nam.region_number, nam.lfmm_crude_type]].copy()
            temp = temp.groupby([nam.region_number, nam.lfmm_crude_type]).sum()
            temp = temp.stack() / (365 * 1000)
            temp.index = temp.index.set_levels([temp.index.levels[0].astype('int64'), #ensure data type of series indices matches restart file
                                                temp.index.levels[1].astype('int64'),
                                                temp.index.levels[2].astype('int64')])

            #Write to Restart
            self.restart.ogsmout_ogcrdprd['value'].update(temp)

            #Update Oil Shale Medium Medium Sour
            self.restart.ogsmout_ogcrdprd.at[(5,3,self.rest_curcalyr), nam.value] = self.restart.ogsmout_ogcrdprd.at[(5,3,self.rest_curcalyr), nam.value] + \
                self.restart.ogsmout_ogqcrrep.at[(1,self.rest_curcalyr), nam.value]


            ###Oil production by LFMM Region
            temp = self.crude_production.copy()[[nam.district_number, nam.lfmm_crude_type, self.rest_curcalyr]]
            temp = pd.merge(temp,
                            self.parent.mapping[[nam.district_number,nam.lfmm_region_number]],
                            on=nam.district_number,
                            how='left')
            temp = temp.drop([nam.district_number], axis = 1)
            temp[nam.lfmm_crude_type] = temp[nam.lfmm_crude_type].astype(int)
            temp = temp.groupby([nam.lfmm_region_number, nam.lfmm_crude_type]).sum()
            self.restart.ogsmout_ogcruderef['value'].update(temp[[self.rest_curcalyr]].stack() / 1000000)


            ###Heat rate by LFMM region crude oil type
            #Get crude production with associated api and crude type identifiers
            temp = self.crude_production.copy()[[nam.district_number, nam.lfmm_crude_type, nam.api, nam.avg_api, self.rest_curcalyr]]
            temp = temp.drop([nam.district_number], axis = 1)
            temp[nam.lfmm_crude_type] = temp[nam.lfmm_crude_type].astype(int)

            #Convert API gravity to Specific Gravity then get heat gravity
            temp['specific_gravity_prod'] = 1
            mask = temp[nam.api] > 0
            api_temp = temp[mask].copy()
            api_temp['specific_gravity_prod'] = temp[self.rest_curcalyr].copy() / 1000000 * (141.5 / (temp[nam.api] + 131.5)).copy().fillna(0.0)
            temp.update(api_temp)

            #Groupby and merge volumes
            national_temp = temp.groupby([nam.lfmm_crude_type, nam.avg_api], as_index=False)[[self.rest_curcalyr, 'specific_gravity_prod']].sum()
            national_temp = national_temp.set_index(nam.lfmm_crude_type)

            #Get default gravity value
            #national_temp['grav'] = 141.5/ national_temp[nam.avg_api] + 131.5
            #Set heat gravity value
            national_temp['grav'] = national_temp['specific_gravity_prod'].div(national_temp[self.rest_curcalyr] / 1000000)
            national_temp[self.rest_curcalyr] = national_temp['grav'] * (7.801769 - 1.3213 * national_temp['grav']**2)
            self.restart.ogsmout_ogcrdheat['value'].update(national_temp[[self.rest_curcalyr]].stack())


            ###CO2 EOR Crude Production
            # Create a mask for CO2 and Other EOR process codes
            eor_mask = self.crude_production[nam.process_code].isin([2, 10, 18])

            # Filter rows and select specific columns using .loc
            temp = self.crude_production.loc[eor_mask, [nam.district_number, self.rest_curcalyr]].copy()

            # put all EOR types in index 1, convert to thousand barrels per year, mbbl/yr
            self.restart.ogsmout_ogeorprd['value'].loc[(8, 1, self.rest_curcalyr)] = temp[self.rest_curcalyr].sum() / 1000 # convert to thousand bbls/yr

            ### Federal/Non-Federal land oil production
            # Federal Land
            temp = self.crude_production.copy()[[nam.region_number, self.rest_curcalyr, nam.federal_land, nam.project_royalty_multiplier]]
            temp = temp.loc[temp[nam.federal_land] == 'Y']

            # Split out mixed land from Federal land
            temp_fed = temp.copy()
            temp_fed[self.rest_curcalyr] = temp_fed[self.rest_curcalyr] * temp_fed[nam.project_royalty_multiplier]
            temp_fed = temp_fed.drop([nam.federal_land,nam.project_royalty_multiplier], axis = 1)
            temp_fed = temp_fed.groupby(nam.region_number).sum()
            self.restart.ogsmout_ogcoprd_fed['value'].update(temp_fed[[self.rest_curcalyr]].stack() / 1000000 / 365)

            temp_nofed = temp.copy()
            temp_nofed[self.rest_curcalyr] = temp_nofed[self.rest_curcalyr] * (1 - temp_nofed[nam.project_royalty_multiplier])
            temp_nofed = temp_nofed.drop([nam.federal_land,nam.project_royalty_multiplier], axis = 1)
            temp_nofed = temp_nofed.groupby(nam.region_number).sum()

            # Non-Federal Land
            temp = self.crude_production.copy()[[nam.region_number, self.rest_curcalyr, nam.federal_land, nam.project_royalty_multiplier]]
            temp = temp.loc[temp[nam.federal_land] == 'N']
            temp = temp.drop([nam.federal_land], axis = 1)
            temp = temp.groupby(nam.region_number).sum()
            temp = temp + temp_nofed
            self.restart.ogsmout_ogcoprd_nonfed['value'].update(temp[[self.rest_curcalyr]].stack() / 1000000 / 365)


            ###Non-Associated natural gas production
            temp_na = self.natgas_production[[nam.district_number, self.rest_curcalyr, nam.gas_type_number, nam.well_type_number]].copy()
            well_type_mask = temp_na[nam.well_type_number] >= 3
            temp_na = temp_na[well_type_mask].copy()
            temp_na = temp_na.drop([nam.well_type_number], axis = 1)
            temp_na[nam.gas_type_number] = temp_na[nam.gas_type_number].astype(int)
            temp_na = temp_na.groupby([nam.district_number, nam.gas_type_number]).sum()
            temp_na = temp_na.stack() / 1000000
            self.restart.ogsmout_ogenagprd['value'].update(temp_na)
            #self.restart.ogsmout_ogrnagprd['value'].update(temp)


            ###AD natgas production by District and Oil type
            temp_ad = self.natgas_production.copy()[[nam.district_number, nam.well_type_number, nam.oil_type_number, self.rest_curcalyr]]
            temp_ad[nam.well_type_number] = temp_ad[nam.well_type_number].astype(int)
            temp_ad[nam.oil_type_number] = temp_ad[nam.oil_type_number].astype(int)

            #mask for secondary gas production
            well_type_mask = temp_ad[nam.well_type_number] < 3
            temp_ad = temp_ad[well_type_mask].copy()

            #Format for Restart File
            temp_ad = temp_ad.drop([nam.well_type_number], axis = 1)
            temp_ad = temp_ad.groupby([nam.district_number, nam.oil_type_number]).sum()
            temp_ad = pd.DataFrame(temp_ad[[self.rest_curcalyr]].stack() / 1000000, columns = [nam.value])
            temp_ad.index.names = self.restart.ogsmout_ogadgprd.index.names

            #Write to Restart File
            self.restart.ogsmout_ogadgprd.update(temp_ad)


            ###Total prod by district
            #Assign to ogsmout_ogdngprd
            temp_na.index.names = temp_ad.index.names
            temp_na = pd.DataFrame(temp_na, index = temp_na.index, columns = temp_ad.columns)
            temp_prod = temp_ad.merge(temp_na,
                                      left_index = True,
                                      right_index = True,
                                      how = 'outer',
                                      suffixes = ['_na','_ad']).fillna(0.0)
            temp_prod['value'] = temp_prod['value_na'] + temp_prod['value_ad']
            self.restart.ogsmout_ogdngprd.update(temp_prod['value'])

            #Sum values
            temp = self.restart.ogsmout_ogdngprd.copy()
            temp = temp[temp.index.isin([1, 2, 3, 4], level=1)]
            temp = temp.groupby(level=[0, 2]).sum()
            temp[nam.well_type] = 5
            temp = temp.set_index([nam.well_type], append=True)
            temp = temp.reorder_levels([0, 2, 1])
            temp.index.names = self.restart.ogsmout_ogdngprd.index.names
            self.restart.ogsmout_ogdngprd.update(temp)


            ###AD natgas production by Region
            temp = self.natgas_production.copy()[[nam.region_number, nam.well_type_number, self.rest_curcalyr]]
            #mask for secondary gas production
            well_type_mask = temp[nam.well_type_number] < 3
            temp = temp[well_type_mask].copy()
            temp = temp.drop([nam.well_type_number], axis=1)

            # Merge and reformat data for restart variable
            temp = temp.groupby([nam.region_number]).sum()

            # Write to Restart File
            self.restart.ogsmout_ogprdad['value'].update(temp[[self.rest_curcalyr]].stack() / 1000000)

            if (self.parent.param_ncrl != 1) | (self.parent.integrated_switch == False): #During Reporting loop iteration only calculate after realized production adjustments
                ###Natural gas production by select play
                temp = self.natgas_production.copy()[[nam.play, nam.well_type_number, self.rest_curcalyr]]
                temp = temp[temp[nam.well_type_number].isin([2,5])]

                #Barnett
                barnett_temp = temp[temp[nam.play].isin([4561, 4562, 4563])].copy()
                self.restart.ogsmout_ogqshlgas.at[(1, self.rest_curcalyr), 'value'] = barnett_temp[self.rest_curcalyr].sum() / 1000000000

                #Haynesville
                haynesville_temp = temp[temp[nam.play].isin([4774, 4775])].copy()
                self.restart.ogsmout_ogqshlgas.at[(2, self.rest_curcalyr), 'value'] = haynesville_temp[self.rest_curcalyr].sum() / 1000000000

                #Fayetteville
                fayetteville_temp = temp[temp[nam.play].isin([6261, 6262])].copy()
                self.restart.ogsmout_ogqshlgas.at[(3, self.rest_curcalyr), 'value'] = fayetteville_temp[self.rest_curcalyr].sum() / 1000000000

                #Woodford
                woodford_temp = temp[temp[nam.play].isin([5861, 5862, 5863, 5875, 99926039])].copy()
                self.restart.ogsmout_ogqshlgas.at[(4, self.rest_curcalyr), 'value'] = woodford_temp[self.rest_curcalyr].sum() / 1000000000

                #Eagle Ford
                eagle_ford_temp = temp[temp[nam.play].isin([4761, 4762, 4763])].copy()
                self.restart.ogsmout_ogqshlgas.at[(5, self.rest_curcalyr), 'value'] = eagle_ford_temp[self.rest_curcalyr].sum() / 1000000000

                #Antrim
                antrim_temp = temp[temp[nam.play].isin([6361])].copy()
                self.restart.ogsmout_ogqshlgas.at[(6, self.rest_curcalyr), 'value'] = antrim_temp[self.rest_curcalyr].sum() / 1000000000

                #Marcellus
                marcellus_temp = temp[temp[nam.play].isin([6761, 6776, 6777, 6778, 6779, 6780, 6781, 6782, 6783])].copy()
                self.restart.ogsmout_ogqshlgas.at[(7, self.rest_curcalyr), 'value'] = marcellus_temp[self.rest_curcalyr].sum() / 1000000000

                #Bakken
                bakken_temp = temp[temp[nam.play].isin([2804, 3110, 3111, 3112, 3113, 3114, 3115])].copy()
                self.restart.ogsmout_ogqshlgas.at[(8, self.rest_curcalyr), 'value'] = bakken_temp[self.rest_curcalyr].sum() / 1000000000

                #Utica
                utica_temp = temp[temp[nam.play].isin([6790, 6791, 6792, 6793])].copy()
                self.restart.ogsmout_ogqshlgas.at[(9, self.rest_curcalyr), 'value'] = utica_temp[self.rest_curcalyr].sum() / 1000000000

                # Permian (excl. conv plays)
                permian_temp = temp[temp[nam.play].isin([4401, 4409, 4413, 4415, 4425, 4471, 4472, 4473, 4475, 4565])].copy()
                self.restart.ogsmout_ogqshlgas.at[(10, self.rest_curcalyr), 'value'] = permian_temp[
                                                                                           self.rest_curcalyr].sum() / 1000000000

                # Other
                other_temp = temp[~temp[nam.play].isin(
                    [4561, 4562, 4563, 4774, 4775, 6261, 6262, 5861, 5862, 5863, 5875, 99926039,
                     4761, 4762, 4763, 6361, 6761, 6776, 6777, 6778, 6779, 6780, 6781, 6782, 6783,
                     2804, 3110, 3111, 3112, 3113, 3114, 3115, 6790, 6791, 6792, 6793, 4401, 4409, 4413, 4415, 4425,
                     4471, 4472, 4473, 4475, 4565])].copy()
                self.restart.ogsmout_ogqshlgas.at[(15, self.rest_curcalyr), 'value'] = other_temp[
                                                                                           self.rest_curcalyr].sum() / 1000000000
            else:
                pass


            ###Natural gas production by gas category
            temp = self.natgas_production.copy()[[nam.region_number, nam.well_type_number, self.rest_curcalyr]]

            #Shale gas and tight oil associated dissolved gas
            shale_mask = temp[nam.well_type_number] == 5
            tight_oil_mask = temp[nam.well_type_number] == 2
            shale_tight_mask = shale_mask | tight_oil_mask
            shale_temp = temp[shale_tight_mask].copy()
            shale_temp = shale_temp[self.rest_curcalyr].sum()
            self.restart.ogsmout_ogqngrep.at[(1,self.rest_curcalyr), 'value'] = shale_temp / 1000000

            #Coalbed Methane
            cbm_mask = temp[nam.well_type_number] == 6
            cbm_temp = temp[cbm_mask].copy()
            cbm_temp = cbm_temp[self.rest_curcalyr].sum()
            self.restart.ogsmout_ogqngrep.at[(2,self.rest_curcalyr), 'value'] = cbm_temp / 1000000

            #Tight Gas
            tight_mask = temp[nam.well_type_number] == 4
            tight_temp = temp[tight_mask].copy()
            tight_temp = tight_temp[self.rest_curcalyr].sum()
            self.restart.ogsmout_ogqngrep.at[(3, self.rest_curcalyr), 'value'] = tight_temp / 1000000

            #Conventional Gas
            conv_mask = temp[nam.well_type_number] == 3
            conv_temp = temp[conv_mask].copy()
            conv_temp = conv_temp[self.rest_curcalyr].sum()
            self.restart.ogsmout_ogqngrep.at[(4, self.rest_curcalyr), 'value'] = conv_temp / 1000000

            #Onshore AD Gas from non-tight plays
            ad_mask = temp[nam.well_type_number] == 1
            ad_temp = temp[ad_mask].copy()
            ad_temp = ad_temp[self.rest_curcalyr].sum()
            self.restart.ogsmout_ogqngrep.at[(5, self.rest_curcalyr), 'value'] = ad_temp / 1000000

            #Ogqshlgas adjustment
            #Adjust 'other' to make sure total play-level section matches total on Table 14
            shale_sum = self.restart.ogsmout_ogqshlgas.xs(self.rest_curcalyr, level=1, drop_level=False).sum()
            tempscale = self.restart.ogsmout_ogqngrep.at[(1, self.rest_curcalyr), 'value'] - shale_sum * 1000
            # Use .loc[] for accessing the value, ensuring it's a scalar
            self.restart.ogsmout_ogqshlgas.loc[(15,self.rest_curcalyr)] = self.restart.ogsmout_ogqshlgas.loc[(15,self.rest_curcalyr)] + tempscale / 1000


            ###Unconventional Natural Gas by region
            temp = self.natgas_production.copy()[[nam.region_number, nam.well_type_number, self.rest_curcalyr]]
            #mask for process code with Tight Gas, Shale Gas or CBM production
            shale_mask = ((temp[nam.well_type_number] == 2) | \
                         (temp[nam.well_type_number] == 5))

            tight_mask = temp[nam.well_type_number] == 4

            cbm_mask = temp[nam.well_type_number] == 6

            pc_mask = shale_mask | tight_mask | cbm_mask

            temp = temp[pc_mask].copy()

            #Set fuel type numbers for Restart Variable
            temp['ugr_number'] = 0
            temp.loc[shale_mask, 'ugr_number'] = 2
            temp.loc[tight_mask, 'ugr_number'] = 1
            temp.loc[cbm_mask, 'ugr_number'] = 3

            #Sum shale production by type and region
            temp = temp.groupby([nam.region_number, 'ugr_number']).sum()

            #Write to Restart Variable
            self.restart.ogsmout_ogprdugr['value'].update(temp[[self.rest_curcalyr]].stack() / 1000000)


            ### Federal/Non-Federal land natural gas production
            # Federal Land
            temp = self.natgas_production.copy()[[nam.region_number, self.rest_curcalyr, nam.federal_land]]
            temp = temp.loc[temp[nam.federal_land] == 'Y']
            temp = temp.drop([nam.federal_land], axis=1)
            temp = temp.groupby(nam.region_number).sum()
            self.restart.ogsmout_ogngprd_fed['value'].update(temp[[self.rest_curcalyr]].stack() / 1000000000)

            # Non-Federal Land
            temp = self.natgas_production.copy()[[nam.region_number, self.rest_curcalyr, nam.federal_land]]
            temp = temp.loc[temp[nam.federal_land] == 'N']
            temp = temp.drop([nam.federal_land], axis=1)
            temp = temp.groupby(nam.region_number).sum()
            self.restart.ogsmout_ogngprd_nonfed['value'].update(temp[[self.rest_curcalyr]].stack() / 1000000000)


            ###Regional crude oil and natural gas production by type of production
            #Crude Oil (1 = Primary Crude Oil, 2 = Secondary Production)
            for region in list(range(1,8)):
                if region <= 4:
                    self.restart.ogsmout_ogregprd.at[(region,1,self.rest_curcalyr), nam.value] = self.restart.pmmout_rfqdcrd.at[(region, self.rest_curcalyr), 'value']
                    self.restart.ogsmout_ogregprd.at[(region,2,self.rest_curcalyr), nam.value] = self.restart.pmmout_rfqtdcrd.at[(region, self.rest_curcalyr), 'value'] - \
                        self.restart.pmmout_rfqdcrd.at[(region, self.rest_curcalyr), 'value']

                elif region == 5:
                    self.restart.ogsmout_ogregprd.at[(region, 1, self.rest_curcalyr), nam.value] = self.restart.pmmout_rfqdcrd.at[(region, self.rest_curcalyr), 'value'] + \
                        self.restart.ogsmout_ogqcrrep.at[(1, self.rest_curcalyr), 'value'] / 365 / 1000
                    self.restart.ogsmout_ogregprd.at[(region, 2, self.rest_curcalyr), nam.value] = self.restart.pmmout_rfqtdcrd.at[(region, self.rest_curcalyr), 'value'] - \
                        self.restart.pmmout_rfqdcrd.at[(region,self.rest_curcalyr), 'value'] - \
                        self.restart.ogsmout_ogqcrrep.at[(1, self.rest_curcalyr), 'value'] / 365 / 1000

                elif region == 6:
                    self.restart.ogsmout_ogregprd.at[(region, 1, self.rest_curcalyr), nam.value] = self.restart.pmmout_rfqdcrd.at[(7, self.rest_curcalyr), 'value']
                    self.restart.ogsmout_ogregprd.at[(region, 2, self.rest_curcalyr), nam.value] = self.restart.pmmout_rfqtdcrd.at[(7, self.rest_curcalyr), 'value'] - \
                        self.restart.pmmout_rfqdcrd.at[(7, self.rest_curcalyr), 'value']

                elif region == 7:
                    self.restart.ogsmout_ogregprd.at[(region, 1, self.rest_curcalyr), nam.value] = self.restart.pmmout_rfqdcrd.at[(6, self.rest_curcalyr), 'value']
                    self.restart.ogsmout_ogregprd.at[(region, 2, self.rest_curcalyr), nam.value] = self.restart.pmmout_rfqtdcrd.at[(6, self.rest_curcalyr), 'value'] - \
                        self.restart.pmmout_rfqdcrd.at[(6, self.rest_curcalyr), 'value']

            ###Apply adjusted realized natural gas volumes to ogsmout_ogregprd (4 = Conventional gas, 5 = Tight Gas, 6 = Shale Gas, 7 = CBM)
            if self.rest_curcalyr <= self.parent.steo_years[0]: #Get initial years as raw data for scaling to STEO, other year calculation against realized prod
                temp = self.natgas_production[[nam.well_type_number, nam.region_number, self.rest_curcalyr]]

                conv_temp = temp[temp[nam.well_type_number].isin([1, 3])]
                conv_temp = conv_temp.groupby(nam.region_number).sum()
                conv_temp[nam.gas_type] = 4
                conv_temp = conv_temp.set_index([nam.gas_type], append=True)
                self.restart.ogsmout_ogregprd['value'].update(conv_temp[[self.rest_curcalyr]].stack() / 1000000000)

                tight_temp = temp[temp[nam.well_type_number].isin([4])]
                tight_temp = tight_temp.groupby(nam.region_number).sum()
                tight_temp[nam.gas_type] = 5
                tight_temp = tight_temp.set_index([nam.gas_type], append=True)
                self.restart.ogsmout_ogregprd['value'].update(tight_temp[[self.rest_curcalyr]].stack() / 1000000000)

                shale_temp = temp[temp[nam.well_type_number].isin([2, 5])]
                shale_temp = shale_temp.groupby(nam.region_number).sum()
                shale_temp[nam.gas_type] = 6
                shale_temp = shale_temp.set_index([nam.gas_type], append=True)
                self.restart.ogsmout_ogregprd['value'].update(shale_temp[[self.rest_curcalyr]].stack() / 1000000000)

                cbm_temp = temp[temp[nam.well_type_number].isin([6])]
                cbm_temp = cbm_temp.groupby(nam.region_number).sum()
                cbm_temp[nam.gas_type] = 7
                cbm_temp = cbm_temp.set_index([nam.gas_type], append=True)
                self.restart.ogsmout_ogregprd['value'].update(cbm_temp[[self.rest_curcalyr]].stack() / 1000000000)


            ###Oil wellhead prices by region
            regions = list(self.crude_production[nam.region_number].unique())
            #Write to global HSM restart variables
            #Crude Wellhead Price by lower 48 region
            for region in regions:
                region = int(region)
                self.restart.ogsmout_ogcowhp.at[(region,self.rest_curcalyr), nam.value] = self.parent.rest_dcrdwhp.at[region, self.rest_curcalyr].copy()

            #Switch Pacific and Northern Great Plains for FTAB
            self.restart.ogsmout_ogcowhp.at[(6, self.rest_curcalyr), nam.value] = self.parent.rest_dcrdwhp.at[7, self.rest_curcalyr].copy()
            self.restart.ogsmout_ogcowhp.at[(7, self.rest_curcalyr), nam.value] = self.parent.rest_dcrdwhp.at[6, self.rest_curcalyr].copy()


            ###Weighted Averge Crude Oil Price
            temp_price = self.restart.ogsmout_ogcowhp.loc(axis=0)[pd.IndexSlice[:, self.rest_curcalyr]].copy()
            temp_prod = self.restart.ogsmout_ogcoprd.loc(axis=0)[pd.IndexSlice[:, self.rest_curcalyr]].copy()
            temp = temp_prod.at[(11,self.rest_curcalyr), nam.value]
            temp_prod_weight = temp_prod.copy()/temp
            temp_price = temp_price.mul(temp_prod_weight, axis = 1)
            temp_price = temp_price.drop(11, axis = 0) #drop sum value
            self.restart.ogsmout_ogpcrwhp.at[self.rest_curcalyr, nam.value] = temp_price[nam.value].sum()


            ###Gas wellhead prices
            regions = list(self.natgas_production[nam.region_number].unique())
            regions = [int(i) for i in regions]
            #Natgas Wellhead Price by lower 48 region
            for region in regions:
                self.restart.ogsmout_ogngwhp.at[(region, self.rest_curcalyr), nam.value] = self.parent.reg_natgas_price.at[region, self.rest_curcalyr]
            #Swap Regions 6 & 7
            self.restart.ogsmout_ogngwhp.at[(6, self.rest_curcalyr), nam.value] = self.parent.reg_natgas_price.at[7, self.rest_curcalyr]
            self.restart.ogsmout_ogngwhp.at[(7, self.rest_curcalyr), nam.value] = self.parent.reg_natgas_price.at[6, self.rest_curcalyr]


            if self.parent.param_ncrl != 1: #For Reporting loop year only calculate after pulling in realized prod
                ###Successful Wells and Dryholes
                self.wells[nam.district_number] = self.wells[nam.district_number].astype(int)
                self.wells[nam.well_type_number] = self.wells[nam.well_type_number].astype(int)
                temp = self.wells.groupby([nam.district_number, nam.well_type_number]).sum()[[self.rest_curcalyr]].stack()
                self.restart.ogsmout_ogogwells['value'].update(temp)

                #Add to self.ogsmout_ognowell
                temp = pd.concat([self.wells.copy(), self.dryholes.copy(), self.exploratory_wells.copy()], ignore_index = True)
                if (not temp.empty) & (self.rest_curcalyr > self.parent.steo_years[0]):
                    self.restart.ogsmout_ognowell.loc[self.rest_curcalyr] += temp[[self.rest_curcalyr]].sum().values[0]


                ###Successful Wells and Dryholes by Region
                temp_wells = self.wells[[nam.region_number, nam.well_type_number, self.rest_curcalyr]].copy()
                temp_total = pd.concat([self.wells.copy(), self.dryholes.copy()], ignore_index = True)
                temp_total = temp_total[[nam.region_number, nam.well_type_number, self.rest_curcalyr]]

                #Groupby well_type_number & region - Gas
                temp_total = temp_total.groupby([nam.region_number, nam.well_type_number]).sum()
                temp_wells = temp_wells.groupby([nam.region_number, nam.well_type_number]).sum()
                temp_sr = temp_wells.div(temp_total, axis = 1)

                #Apply total wells to restart variables (total wells/dryholes and success rates)
                self.restart.ogsmout_ogwellsl48['value'].update(temp_total.stack())
                self.restart.ogsmout_ogsrl48['value'].update(temp_sr.stack())
            else:
                pass


            ###Report NGPL Results
            #NGPL Total
            temp = self.ngpl_production[[nam.district_number, self.rest_curcalyr]].copy()
            temp[nam.district_number] = temp[nam.district_number].astype(int)
            temp = temp.groupby([nam.district_number]).sum()
            temp = temp.stack()
            self.restart.ogsmout_ogngplprd['value'].update(temp)

            #Ethane
            temp = self.ngpl_ethane_production[[nam.district_number, self.rest_curcalyr]].copy()
            temp[nam.district_number] = temp[nam.district_number].astype(int)
            temp = temp.groupby([nam.district_number]).sum()
            temp = temp.stack()
            self.restart.ogsmout_ogngplet['value'].update(temp)

            #Propane
            temp = self.ngpl_propane_production[[nam.district_number, self.rest_curcalyr]].copy()
            temp[nam.district_number] = temp[nam.district_number].astype(int)
            temp = temp.groupby([nam.district_number]).sum()
            temp = temp.stack()
            self.restart.ogsmout_ogngplpr['value'].update(temp)

            #Butane
            temp = self.ngpl_butane_production[[nam.district_number, self.rest_curcalyr]].copy()
            temp[nam.district_number] = temp[nam.district_number].astype(int)
            temp = temp.groupby([nam.district_number]).sum()
            temp = temp.stack()
            self.restart.ogsmout_ogngplbu['value'].update(temp)

            #Isobutane
            temp = self.ngpl_isobutane_production[[nam.district_number, self.rest_curcalyr]].copy()
            temp[nam.district_number] = temp[nam.district_number].astype(int)
            temp = temp.groupby([nam.district_number]).sum()
            temp = temp.stack()
            self.restart.ogsmout_ogngplis['value'].update(temp)

            #Pentanes
            temp = self.ngpl_proplus_production[[nam.district_number, self.rest_curcalyr]].copy()
            temp[nam.district_number] = temp[nam.district_number].astype(int)
            temp = temp.groupby([nam.district_number]).sum()
            temp = temp.stack()
            self.restart.ogsmout_ogngplpp['value'].update(temp)


            ###Report CO2 EOR Results
            # Get temp play_map
            temp_play_map = self.parent.restart.ogsmout_play_map.copy()
            temp_play_map = temp_play_map.reset_index(drop = True)
            temp_play_map.index = temp_play_map.index + 1

            ### Report CO2 demand to CCATS
            # Total CO2 Demand
            new_co2_demand_df = self.co2_injected.loc[self.co2_injected[nam.process_code] == 10].copy()
            
            # Total CO2 Demand net recycled CO2
            recycled_co2_df = self.co2_recycled.loc[self.co2_recycled[nam.process_code] == 10].copy()
            new_co2_demand_df[self.rest_curcalyr] = new_co2_demand_df[self.rest_curcalyr] - recycled_co2_df[self.rest_curcalyr]
            
            # Group volumes by play
            new_co2_demand_df = new_co2_demand_df[[nam.play,self.rest_curcalyr]]
            new_co2_demand_df = new_co2_demand_df.groupby(nam.play).sum()
            new_co2_demand_df = new_co2_demand_df.reset_index()

            # Reformat for Restart File
            new_co2_demand_df = temp_play_map.merge(new_co2_demand_df,
                                                        how = 'left',
                                                        left_on = nam.value,
                                                        right_on = nam.play).fillna(0.0)
            new_co2_demand_df.index = new_co2_demand_df.index + 1 # reindex for merge to restart file
            new_co2_demand_df[nam.year] = self.rest_curcalyr
            new_co2_demand_df = new_co2_demand_df.drop([nam.value],axis = 1)
            new_co2_demand_df = new_co2_demand_df.rename(columns = {self.rest_curcalyr:nam.value})
            new_co2_demand_df = new_co2_demand_df.set_index(nam.year, append = True)
            new_co2_demand_df[nam.value] = new_co2_demand_df[nam.value] * 55.10 # Convert mmcf to metric tonnes

            # Write to Restart File
            self.restart.ccatsdat_dem_eor.update(new_co2_demand_df)
                        

            ### Report CO2 purchase price to CCATS
            ### Proportion of injectant across years
            co2_eor_dem_ratio = self.projects_selected.loc[self.projects_selected[nam.process_code] == 10].copy()
            dem_ratio = co2_eor_dem_ratio[[('II' + str(x + 1)) for x in list(range(self.evaluation_years))]].copy()
            dem_ratio[dem_ratio.columns] = dem_ratio[dem_ratio.columns].div(dem_ratio[[('II' + str(x + 1)) for x in list(range(self.evaluation_years))]].sum(axis = 1), axis = 0)
            dem_ratio.columns = list(range(self.rest_curcalyr, (self.rest_curcalyr + self.evaluation_years)))
            dem_ratio = dem_ratio[list(range(self.rest_curcalyr, self.parent.final_aeo_year + 1))]


            ### Well count 
            co2_eor_wells_df = self.projects_selected.loc[self.projects_selected[nam.process_code] == 10].copy()
            co2_eor_wells = co2_eor_wells_df[[('WL' + str(x + 1)) for x in list(range(self.evaluation_years))]].copy()
            co2_eor_wells.columns = list(range(self.rest_curcalyr, (self.rest_curcalyr + self.evaluation_years)))
            co2_eor_wells = co2_eor_wells[list(range(self.rest_curcalyr, self.parent.final_aeo_year + 1))].copy()
            co2_eor_wells_df = co2_eor_wells_df[[nam.play]]
            co2_eor_wells_df[co2_eor_wells.columns] = co2_eor_wells[co2_eor_wells.columns].copy()
            
            if self.co2_eor_wells.empty:
                self.co2_eor_wells = co2_eor_wells_df.copy()
            else:
                self.co2_eor_wells = pd.concat([self.co2_eor_wells,co2_eor_wells_df], ignore_index= True)
                self.co2_eor_wells = self.co2_eor_wells.groupby([nam.play]).sum().reset_index()
            
            # Get CO2 EOR NPVs by year based on CO2 demand ratio
            new_co2_net_cost_df = self.projects_selected.loc[self.projects_selected[nam.process_code] == 10].copy()
            new_co2_net_cost = new_co2_net_cost_df[nam.net_present_value]
            new_co2_net_cost = dem_ratio.mul(new_co2_net_cost, axis = 0)

            # Multiply net costs by number of wells
            for year in list(range(self.rest_curcalyr, self.parent.final_aeo_year + 1)):
                new_co2_net_cost[year] = new_co2_net_cost[year] * co2_eor_wells_df[year]

            # Assign play number
            new_co2_net_cost_df = new_co2_net_cost_df[[nam.play]]
            new_co2_net_cost_df[new_co2_net_cost.columns] = new_co2_net_cost[new_co2_net_cost.columns].copy()

            # Sum Net costs
            if self.co2_net_cost.empty:
                self.co2_net_cost = new_co2_net_cost_df.copy()
            else:
                self.co2_net_cost = pd.concat([self.co2_net_cost, new_co2_net_cost_df], ignore_index = True)
                self.co2_net_cost = self.co2_net_cost.groupby(['play']).sum().reset_index()
                

            # Reformat for Restart File
            new_co2_net_cost_df = self.co2_net_cost[[nam.play, self.rest_curcalyr]]
            new_co2_net_cost_df = new_co2_net_cost_df.groupby([nam.play]).sum().reset_index()
            new_co2_net_cost_df = temp_play_map.merge(new_co2_net_cost_df,
                                                    how='left',
                                                    left_on=nam.value,
                                                    right_on=nam.play).fillna(0.0)
            new_co2_net_cost_df.index = new_co2_net_cost_df.index + 1 # reindex for merge to restart file
            new_co2_net_cost_df[nam.year] = self.rest_curcalyr
            new_co2_net_cost_df = new_co2_net_cost_df.drop([nam.value], axis=1)
            new_co2_net_cost_df = new_co2_net_cost_df.rename(columns={self.rest_curcalyr: nam.value})
            new_co2_net_cost_df = new_co2_net_cost_df.set_index(nam.year, append=True)
            new_co2_net_cost_df.index.names = self.restart.ccatsdat_cst_eor.index.names
                        
            # Divide CO2 NPV by volume to get purchase price/tonne of CO2
            new_co2_net_cost_df[nam.value] = new_co2_net_cost_df[nam.value] / new_co2_demand_df[nam.value]
            new_co2_net_cost_df[nam.value] = new_co2_net_cost_df[nam.value].fillna(0.0)

            # Set cap on CO2 EOR $s paid for CO2 at $9.52/tonne 1987$, which is 50% more than 2nd standard dev, max contract in the Permian ~$13/tonne 1987$ ,but this is outlier  https://doi.org/10.1080/15140326.2022.2065064
            # Adjust max CO2 price for oil price path change
            if (self.rest_curcalyr > self.parent.steo_years[-1]) & (self.parent.integrated_switch == True):
                multiplier = 1
                for year in list(range((self.parent.steo_years[-1] + 1), (self.rest_curcalyr + 1))):
                    multiplier *= (self.parent.rest_brent_price.at[(year,nam.value)]/ \
                                                       self.parent.rest_brent_price.loc[((year - 1),nam.value)])

                self.max_co2_prc = self.max_co2_prc * multiplier

            new_co2_net_cost_df.loc[new_co2_net_cost_df[nam.value] > self.max_co2_prc, nam.value] = self.max_co2_prc
            
            # Set as negative to represent revenue to CCATS
            new_co2_net_cost_df[nam.value] = new_co2_net_cost_df[nam.value] * -1
            new_co2_net_cost_df[nam.value] = new_co2_net_cost_df[nam.value].replace([np.inf, -np.inf, np.nan], 0.0)

            # Adjust max CO2 price for side cases
            if self.parent.side_case_adj > 1:
                self.max_co2_prc = self.max_co2_prc * self.parent.side_case_adj
            new_co2_net_cost_df.loc[new_co2_net_cost_df[nam.value] > self.max_co2_prc, nam.value] = self.max_co2_prc

            # Set as negative to represent revenue to CCATS
            new_co2_net_cost_df[nam.value] = new_co2_net_cost_df[nam.value] * -1
            new_co2_net_cost_df[nam.value] = new_co2_net_cost_df[nam.value].replace([np.inf, -np.inf, np.nan], 0.0)

            # Write to Restart File
            self.restart.ccatsdat_cst_eor.update(new_co2_net_cost_df)
            
            # Write to pickle
            self.parent.hsm_vars.on_co2_eor_wells = self.co2_eor_wells.copy()
            self.parent.hsm_vars.on_co2_net_cost = self.co2_net_cost.copy()


            ###Tech Rates
            #Format tech rates for restart file
            temp = self.tech_levers[[nam.drill_tech, nam.cost_tech, nam.tier_1_eur_tech]]
            temp.columns = [1,2,3]
            temp = temp.melt()
            temp.index = [1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6]
            temp = temp.set_index(['variable'], append = True)
            temp = temp.reorder_levels([1, 0])
            temp[nam.year] = self.rest_curcalyr
            temp = temp.set_index([nam.year], append = True)

            #Write to Restart Variable
            self.restart.ogsmout_ogtechon.update(temp)


        else:
            pass

        pass

    def adjust_onshore_shale_gas_plays_for_realized_prod(self):
        """Scales onshore natural gas play production to NGMM realized demand.

        Returns
        -------
        self.restart.ogsmout_ogqshlgas : df
            Natural gas production by select natural gas play

        self.restart.ogsmout_ogogwells : df
            Total wells

        self.restart.ogsmout_ognowell : df
            Total completed wells

        self.restart.ogsmout_ogwellsl48 : df
            Total lower 48 wells

        self.restart.ogsmout_ogsrl48 : df
            Lower 48 drilling success rates

        self.restart.ogsmout_ogregprd : df
            Total crude oil and natural gas production by production type

        self.projects : df
            Master DataFrame containing all projects to be processed in current model year

        self.wells : df
            DataFrame of onshore wells

        """
        ###Get temp dfs for expected prod, realized prod, wells, natgas prod, and projects
        #Realized Prod
        temp_realized_prod = self.restart.ogsmout_ogrnagprd.copy()
        temp_realized_prod = temp_realized_prod.xs(self.rest_curcalyr, level = 2)
        temp_realized_prod = temp_realized_prod.xs(5, level = 1)

        #Expected Prod
        temp_expected_prod = self.restart.ogsmout_ogenagprd.copy()
        temp_expected_prod = temp_expected_prod.xs(self.rest_curcalyr, level=2)
        temp_expected_prod = temp_expected_prod.xs(5, level = 1)

        #Project Natgas Prod
        temp_natgas_prod = self.natgas_production[[nam.district_number,nam.play, nam.well_type_number, nam.process_code, self.rest_curcalyr]].copy()
        temp_shale_tight_prod = temp_natgas_prod[temp_natgas_prod[nam.well_type_number].isin([4,5])].copy()

        #Project wells
        temp_shale_tight_wells = self.wells[[nam.district_number,nam.play, nam.well_type_number, self.rest_curcalyr]].copy()
        temp_shale_tight_wells = temp_shale_tight_wells[temp_shale_tight_wells[nam.well_type_number].isin([4,5])].copy()

        #Projects
        temp_projects = self.projects.copy()
        temp_projects = temp_projects.loc[temp_projects[nam.well_type_number].isin([4,5])]


        ###Get realized_production ratios
        temp_prod_ratio = temp_realized_prod.div(temp_expected_prod, axis = 0)
        temp_prod_ratio = temp_prod_ratio.replace([np.inf, -np.inf, np.nan], 1.0)
        temp_prod_ratio.columns = [nam.realized_prod_ratio]

        ###Apply realized production ratios to temp_shale_tight_prod
        temp_shale_tight_prod = temp_shale_tight_prod.reset_index().merge(temp_prod_ratio,
                                                                          how = 'left',
                                                                          left_on = nam.district_number,
                                                                          right_index = True).set_index('index')

        temp_shale_tight_prod[self.rest_curcalyr] = temp_shale_tight_prod[self.rest_curcalyr] * temp_shale_tight_prod[nam.realized_prod_ratio]

        #Update self.natgas_prod
        self.natgas_production.update(temp_shale_tight_prod[[self.rest_curcalyr]])

        #Concatenate Shale, Tight and AD gas
        temp_natgas_prod_adj = self.natgas_production.copy()
        temp_natgas_prod_adj = temp_natgas_prod_adj[temp_natgas_prod_adj[nam.well_type_number].isin([2,5])]

        ###Apply Production to Plays
        #Barnett
        barnett_temp = temp_natgas_prod_adj[temp_natgas_prod_adj[nam.play].isin([4561, 4562, 4563])].copy()
        self.restart.ogsmout_ogqshlgas.at[(1, self.rest_curcalyr), 'value'] = barnett_temp[self.rest_curcalyr].sum() / 1000000000

        #Haynesville
        haynesville_temp = temp_natgas_prod_adj[temp_natgas_prod_adj[nam.play].isin([4774, 4775])].copy()
        self.restart.ogsmout_ogqshlgas.at[(2, self.rest_curcalyr), 'value'] = haynesville_temp[self.rest_curcalyr].sum() / 1000000000

        #Fayetteville
        fayetteville_temp = temp_natgas_prod_adj[temp_natgas_prod_adj[nam.play].isin([6261, 6262])].copy()
        self.restart.ogsmout_ogqshlgas.at[(3, self.rest_curcalyr), 'value'] = fayetteville_temp[self.rest_curcalyr].sum() / 1000000000

        #Woodford
        woodford_temp = temp_natgas_prod_adj[temp_natgas_prod_adj[nam.play].isin([5861, 5862, 5863, 5875, 99926039])].copy()
        self.restart.ogsmout_ogqshlgas.at[(4, self.rest_curcalyr), 'value'] = woodford_temp[self.rest_curcalyr].sum() / 1000000000

        #Eagle Ford
        eagle_ford_temp = temp_natgas_prod_adj[temp_natgas_prod_adj[nam.play].isin([4761, 4762, 4763])].copy()
        self.restart.ogsmout_ogqshlgas.at[(5, self.rest_curcalyr), 'value'] = eagle_ford_temp[self.rest_curcalyr].sum() / 1000000000

        #Antrim - NOT INCLUDED IN DECLINE BECAUSE VOLUME IS SO SMALL
        antrim_temp = temp_natgas_prod_adj[temp_natgas_prod_adj[nam.play].isin([6361])].copy()
        self.restart.ogsmout_ogqshlgas.at[(6, self.rest_curcalyr), 'value'] = antrim_temp[self.rest_curcalyr].sum() / 1000000000

        #Marcellus
        marcellus_temp = temp_natgas_prod_adj[temp_natgas_prod_adj[nam.play].isin([6761, 6776, 6777, 6778, 6779, 6780, 6781, 6782, 6783])].copy()
        self.restart.ogsmout_ogqshlgas.at[(7, self.rest_curcalyr), 'value'] = marcellus_temp[self.rest_curcalyr].sum() / 1000000000

        #Bakken
        bakken_temp = temp_natgas_prod_adj[temp_natgas_prod_adj[nam.play].isin([2804, 3110, 3111, 3112, 3113, 3114, 3115])].copy()
        self.restart.ogsmout_ogqshlgas.at[(8, self.rest_curcalyr), 'value'] = bakken_temp[self.rest_curcalyr].sum() / 1000000000

        #Utica
        utica_temp = temp_natgas_prod_adj[temp_natgas_prod_adj[nam.play].isin([6790, 6791, 6792, 6793])].copy()
        self.restart.ogsmout_ogqshlgas.at[(9, self.rest_curcalyr), 'value'] = utica_temp[self.rest_curcalyr].sum() / 1000000000

        #Permian (excl. conv plays)
        permian_temp = temp_natgas_prod_adj[temp_natgas_prod_adj[nam.play]
                        .isin([4401, 4409, 4413, 4415, 4425, 4471, 4472, 4473, 4475, 4565])].copy()
        self.restart.ogsmout_ogqshlgas.at[(10, self.rest_curcalyr), 'value'] = permian_temp[
                                                                                  self.rest_curcalyr].sum() / 1000000000

        #Other
        other_temp = temp_natgas_prod_adj[~temp_natgas_prod_adj[nam.play].isin([4561, 4562, 4563, 4774, 4775, 6261, 6262, 5861, 5862, 5863, 5875, 99926039,
                                  4761, 4762, 4763, 6361, 6761, 6776, 6777, 6778, 6779, 6780, 6781, 6782, 6783,
                                  2804, 3110, 3111, 3112, 3113, 3114, 3115, 6790, 6791, 6792, 6793, 4401, 4409, 4413, 4415, 4425, 4471, 4472, 4473, 4475, 4565])].copy()
        self.restart.ogsmout_ogqshlgas.at[(15, self.rest_curcalyr), 'value'] = other_temp[self.rest_curcalyr].sum() / 1000000000

        # Ogqshlgas adjustment
        # Adjust 'other' to make sure total play-level section matches total on Table 14
        shale_sum = self.restart.ogsmout_ogqshlgas.xs(self.rest_curcalyr, level=1, drop_level=False).sum()
        tempscale = self.restart.ogsmout_ogqngrep.at[(1, self.rest_curcalyr), 'value'] - shale_sum * 1000
        # Use .loc[] for accessing the value, ensuring it's a scalar
        self.restart.ogsmout_ogqshlgas.loc[(15, self.rest_curcalyr)] = self.restart.ogsmout_ogqshlgas.loc[
                                                                           (15, self.rest_curcalyr)] + tempscale / 1000


        ###Apply realized production ratios to wells
        temp_shale_tight_wells = temp_shale_tight_wells.reset_index().merge(temp_prod_ratio,
                                                                          how = 'left',
                                                                          left_on = nam.district_number,
                                                                          right_index = True).set_index('index')

        temp_shale_tight_wells[self.rest_curcalyr] = temp_shale_tight_wells[self.rest_curcalyr] * temp_shale_tight_wells[nam.realized_prod_ratio]

        #Round well counts
        temp_shale_tight_wells[self.rest_curcalyr] = temp_shale_tight_wells[self.rest_curcalyr].apply(np.ceil)

        #Update self.wells
        self.wells.update(temp_shale_tight_wells[[self.rest_curcalyr]])


        ###Adjust Wells and Dryholes for restart file
        self.wells[nam.district_number] = self.wells[nam.district_number].astype(int)
        self.wells[nam.well_type_number] = self.wells[nam.well_type_number].astype(int)
        temp = self.wells.groupby([nam.district_number, nam.well_type_number]).sum()[[self.rest_curcalyr]].stack()
        self.restart.ogsmout_ogogwells['value'].update(temp)

        #Add to self.ogsmout_ognowell
        temp = pd.concat([self.wells.copy(), self.dryholes.copy(), self.exploratory_wells.copy()], ignore_index=True)
        if (not temp.empty) & (self.rest_curcalyr > self.parent.steo_years[0]):
            self.restart.ogsmout_ognowell.loc[self.rest_curcalyr] += temp[[self.rest_curcalyr]].sum().values[0]


        ###Successful Wells and Dryholes by Region
        temp_wells = self.wells[[nam.region_number, nam.well_type_number, self.rest_curcalyr]].copy()
        temp_total = pd.concat([self.wells.copy(), self.dryholes.copy()], ignore_index=True)
        temp_total = temp_total[[nam.region_number, nam.well_type_number, self.rest_curcalyr]]

        # Groupby well_type_number & region - Gas
        temp_total = temp_total.groupby([nam.region_number, nam.well_type_number]).sum()
        temp_wells = temp_wells.groupby([nam.region_number, nam.well_type_number]).sum()
        temp_sr = temp_wells.div(temp_total, axis=1)

        # Apply total wells to restart variables (total wells/dryholes and success rates)
        self.restart.ogsmout_ogwellsl48['value'].update(temp_total.stack())
        self.restart.ogsmout_ogsrl48['value'].update(temp_sr.stack())


        #Apply realized production ratios to temp_projects
        temp_projects = temp_projects.reset_index().merge(temp_prod_ratio,
                                                           how = 'left',
                                                           left_on = nam.district_number,
                                                           right_index = True).set_index('index')

        #Redo past_drilling
        temp_projects[nam.past_wells] = temp_projects[nam.past_wells] - temp_projects[nam.last_year_drilling]

        #Apply ratio to last year drilling
        temp_projects[nam.last_year_drilling] = temp_projects[nam.last_year_drilling] * temp_projects[nam.realized_prod_ratio]
        temp_projects[nam.last_year_drilling] = temp_projects[nam.last_year_drilling].apply(np.ceil)

        #Adjust Past Wells
        temp_projects[nam.past_wells] = temp_projects[nam.past_wells] + temp_projects[nam.last_year_drilling]

        #Update self.projects
        self.projects.update(temp_projects)


        ###Apply adjusted realized natural gas volumes to ogsmout_ogregprd (4 = Conventional gas, 5 = Tight Gas, 6 = Shale Gas, 7 = CBM)
        temp = self.natgas_production[[nam.well_type_number, nam.region_number, self.rest_curcalyr]]

        conv_temp = temp[temp[nam.well_type_number].isin([1, 3])]
        conv_temp = conv_temp.groupby(nam.region_number).sum()
        conv_temp[nam.gas_type] = 4
        conv_temp = conv_temp.set_index([nam.gas_type], append=True)
        self.restart.ogsmout_ogregprd['value'].update(conv_temp[[self.rest_curcalyr]].stack() / 1000000000)

        tight_temp = temp[temp[nam.well_type_number].isin([4])]
        tight_temp = tight_temp.groupby(nam.region_number).sum()
        tight_temp[nam.gas_type] = 5
        tight_temp = tight_temp.set_index([nam.gas_type], append=True)
        self.restart.ogsmout_ogregprd['value'].update(tight_temp[[self.rest_curcalyr]].stack() / 1000000000)

        shale_temp = temp[temp[nam.well_type_number].isin([2, 5])]
        shale_temp = shale_temp.groupby(nam.region_number).sum()
        shale_temp[nam.gas_type] = 6
        shale_temp = shale_temp.set_index([nam.gas_type], append=True)
        self.restart.ogsmout_ogregprd['value'].update(shale_temp[[self.rest_curcalyr]].stack() / 1000000000)

        cbm_temp = temp[temp[nam.well_type_number].isin([6])]
        cbm_temp = cbm_temp.groupby(nam.region_number).sum()
        cbm_temp[nam.gas_type] = 7
        cbm_temp = cbm_temp.set_index([nam.gas_type], append=True)
        self.restart.ogsmout_ogregprd['value'].update(cbm_temp[[self.rest_curcalyr]].stack() / 1000000000)


        pass