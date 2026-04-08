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

    18. *report_results_unf* is called from **module_unf.py** to report results to
        debug files and the restart file.


Input Files
___________

    * on_projects_continuous - Continuous projects
    * on_projects_producing_gas - Producing gas projects
    * on_projects_producing_oil - Producing oil projects
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
    * load_prices - Method to calculate average prices for Onshore regions; averages prices over years set in averaging_years (positive = forward-looking, negative = backward-looking)
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
import logging
import sys

# Opt-in to future pandas behavior to suppress FutureWarnings about downcasting
pd.set_option('future.no_silent_downcasting', True)


def _ensure_dtype_compatibility(target_df, source_df):
    """
    Ensure dtype compatibility between source and target DataFrames to avoid FutureWarnings.
    
    Parameters
    ----------
    target_df : pd.DataFrame
        The target DataFrame
    source_df : pd.DataFrame
        The source DataFrame to convert
    
    Returns
    -------
    pd.DataFrame
        Source DataFrame with compatible dtypes
    """
    source_df = source_df.copy()
    for col in source_df.columns:
        if col in target_df.columns:
            if pd.api.types.is_numeric_dtype(target_df[col].dtype) and pd.api.types.is_numeric_dtype(source_df[col].dtype):
                try:
                    source_df[col] = source_df[col].astype(target_df[col].dtype)
                except (ValueError, TypeError):
                    pass
    return source_df


def _cast_restart_scalar(restart_df, value):
    """Cast a scalar to match a restart DataFrame's value dtype."""
    if nam.value not in restart_df.columns:
        return value
    return np.asarray(value, dtype=restart_df[nam.value].dtype).item()


def _cast_restart_series(restart_df, series):
    """Cast a Series to match a restart DataFrame's value dtype."""
    if nam.value not in restart_df.columns:
        return series
    return series.astype(restart_df[nam.value].dtype)


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
        self.project_drilling_pre_steo  = pd.Series()    # Series to store pre-STEO drilling values for STEO year 0
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
        self.averaging_years        = 0    # Number of additional years to average price. Positive = future years (current + N future), negative = past years (N past + current)
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
        
        # Economic and conversion parameters
        self.boe_conversion                = 0.0  # BOE conversion factor for natural gas (mcf per barrel)
        self.barrels_per_gallon            = 0.0  # Gallons per barrel conversion
        self.ch4_to_metric_tons            = 0.0  # MCF to metric tons conversion for CH4 emissions
        self.ngpl_volume_divisor           = 0.0  # NGPL volume calculation divisor
        self.acres_per_section             = 0.0  # Acres per section for drilling calculations
        self.drilling_growth_multiplier    = 0.0  # Maximum annual drilling growth rate multiplier
        self.production_factor             = 0.0  # Production factor for max drill rate calculation
        self.undiscovered_unconv_production_factor = 0.0  # Production factor for undiscovered unconventional max drill rate
        self.max_drilling_pct_cap          = 0.0  # Maximum % of available wells drillable per year
        self.default_drilling_pct          = 0.0  # Default drilling percentage for projects without history
        self.min_well_spacing_floor        = 0.0  # Minimum well spacing floor in acres
        self.lateral_length_divisor        = 0.0  # Divisor for lateral length spacing calculation
        self.lateral_length_multiplier     = 0.0  # Multiplier for lateral length spacing calculation
        self.cost_adj_tier_one             = 0.0  # Cost adjustment for projects with 1-5 historical wells
        self.cost_adj_tier_two             = 0.0  # Cost adjustment for projects with 10-50 historical wells
        self.cost_adj_tier_three           = 0.0  # Cost adjustment for projects with 50+ historical wells
        self.min_years_before_abandon      = 0    # Minimum production years before abandonment eligible
        self.econ_life_default             = 0    # Default economic life when not hitting econ limit
        self.producing_wells_multiplier    = 0.0  # Multiplier applied to producing well counts
        self.price_elasticity_exponent     = 0.0  # Price adjustment elasticity exponent
        self.wells_steo_ratio              = 0.0  # Well ratio factor for matching STEO projections
        
        # Cost adjustment multipliers
        self.national_cost_multiplier      = 0.0  # Cost multiplier for national-level matches
        self.regional_cost_multiplier      = 0.0  # Cost multiplier for regional-level matches
        self.eor_other_opex_multiplier     = 0.0  # Opex multiplier for Other EOR projects
        self.eor_other_capex_multiplier    = 0.0  # Capex multiplier for Other EOR projects
        self.cost_fillna_district_mult     = 0.0  # Cost multiplier for district-level fill
        self.cost_fillna_region_mult       = 0.0  # Cost multiplier for region-level fill
        self.cost_fillna_resource_mult     = 0.0  # Cost multiplier for resource-type fill
        self.cost_outlier_lower_mult       = 0.0  # Lower bound multiplier for outlier detection
        self.cost_outlier_upper_mult       = 0.0  # Upper bound multiplier for outlier detection
        self.crude_heat_base               = 0.0  # Base value for crude heat calculation
        self.sga_conservative_factor       = 0.0  # SGA conservative factor for developing projects
        self.sga_producing_factor          = 0.0  # SGA conservative factor for producing projects
        self.facility_producing_factor     = 0.0  # Facility capex factor for producing projects
        self.tier_boundary_low             = 0    # Well count boundary between tier 1 and 2
        self.tier_boundary_high            = 0    # Well count boundary between tier 2 and 3

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
        self.vented_gas_royalty = str(self.setup_table.at[nam.on_vented_gas_royalty     , nam.filename]).upper() == 'True'
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
        self.steo_eor_annual_growth_rate = float(self.setup_table.at[nam.on_steo_eor_annual_growth_rate, nam.filename])
        
        # Load economic and conversion parameters
        self.boe_conversion             = float(self.setup_table.at[nam.on_boe_conversion              , nam.filename])
        self.barrels_per_gallon         = float(self.setup_table.at[nam.on_barrels_per_gallon          , nam.filename])
        self.ch4_to_metric_tons         = float(self.setup_table.at[nam.on_ch4_to_metric_tons          , nam.filename])
        self.ngpl_volume_divisor        = float(self.setup_table.at[nam.on_ngpl_volume_divisor         , nam.filename])
        self.acres_per_section          = float(self.setup_table.at[nam.on_acres_per_section           , nam.filename])
        self.drilling_growth_multiplier = float(self.setup_table.at[nam.on_drilling_growth_multiplier  , nam.filename])
        self.production_factor          = float(self.setup_table.at[nam.on_production_factor           , nam.filename])
        self.undiscovered_unconv_production_factor = float(self.setup_table.at[nam.on_undiscovered_unconv_production_factor, nam.filename])
        self.max_drilling_pct_cap       = float(self.setup_table.at[nam.on_max_drilling_pct_cap        , nam.filename])
        self.default_drilling_pct       = float(self.setup_table.at[nam.on_default_drilling_pct        , nam.filename])
        self.min_well_spacing_floor     = float(self.setup_table.at[nam.on_min_well_spacing_floor      , nam.filename])
        self.lateral_length_divisor     = float(self.setup_table.at[nam.on_lateral_length_divisor      , nam.filename])
        self.lateral_length_multiplier  = float(self.setup_table.at[nam.on_lateral_length_multiplier   , nam.filename])
        self.cost_adj_tier_one          = float(self.setup_table.at[nam.on_cost_adj_tier_one           , nam.filename])
        self.cost_adj_tier_two          = float(self.setup_table.at[nam.on_cost_adj_tier_two           , nam.filename])
        self.cost_adj_tier_three        = float(self.setup_table.at[nam.on_cost_adj_tier_three         , nam.filename])
        self.min_years_before_abandon   = int(  self.setup_table.at[nam.on_min_years_before_abandon    , nam.filename])
        self.econ_life_default          = int(  self.setup_table.at[nam.on_econ_life_default           , nam.filename])
        self.producing_wells_multiplier = float(self.setup_table.at[nam.on_producing_wells_multiplier  , nam.filename])
        self.price_elasticity_exponent  = float(self.setup_table.at[nam.on_price_elasticity_exponent   , nam.filename])
        self.wells_steo_ratio           = float(self.setup_table.at[nam.on_wells_steo_ratio            , nam.filename])
        
        # Load cost adjustment multipliers
        self.national_cost_multiplier   = float(self.setup_table.at[nam.on_national_cost_multiplier    , nam.filename])
        self.regional_cost_multiplier   = float(self.setup_table.at[nam.on_regional_cost_multiplier    , nam.filename])
        self.eor_other_opex_multiplier  = float(self.setup_table.at[nam.on_eor_other_opex_multiplier   , nam.filename])
        self.eor_other_capex_multiplier = float(self.setup_table.at[nam.on_eor_other_capex_multiplier  , nam.filename])
        self.cost_fillna_district_mult  = float(self.setup_table.at[nam.on_cost_fillna_district_mult   , nam.filename])
        self.cost_fillna_region_mult    = float(self.setup_table.at[nam.on_cost_fillna_region_mult     , nam.filename])
        self.cost_fillna_resource_mult  = float(self.setup_table.at[nam.on_cost_fillna_resource_mult   , nam.filename])
        self.cost_outlier_lower_mult    = float(self.setup_table.at[nam.on_cost_outlier_lower_mult     , nam.filename])
        self.cost_outlier_upper_mult    = float(self.setup_table.at[nam.on_cost_outlier_upper_mult     , nam.filename])
        self.crude_heat_base            = float(self.setup_table.at[nam.on_crude_heat_base             , nam.filename])
        self.sga_conservative_factor    = float(self.setup_table.at[nam.on_sga_conservative_factor     , nam.filename])
        self.sga_producing_factor       = float(self.setup_table.at[nam.on_sga_producing_factor        , nam.filename])
        self.facility_producing_factor  = float(self.setup_table.at[nam.on_facility_producing_factor   , nam.filename])
        self.tier_boundary_low          = int(  self.setup_table.at[nam.on_tier_boundary_low           , nam.filename])
        self.tier_boundary_high         = int(  self.setup_table.at[nam.on_tier_boundary_high          , nam.filename])
        self.past_drilling_threshold    = int(  self.setup_table.at[nam.on_past_drilling_threshold     , nam.filename])


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

        #Load play mapping files
        self.tight_oil_play_map = super()._load_dataframe(self.onshore_input_path, nam.on_tight_oil_play_map)
        self.shale_gas_play_map = super()._load_dataframe(self.onshore_input_path, nam.on_shale_gas_play_map)
        
        # Ensure play_number is integer type for proper comparison
        self.tight_oil_play_map['play_number'] = self.tight_oil_play_map['play_number'].astype(int)
        self.shale_gas_play_map['play_number'] = self.shale_gas_play_map['play_number'].astype(int)
        
        # Create dictionaries: {play_number: output_index} for each play type
        self.tight_oil_play_to_index = dict(zip(self.tight_oil_play_map['play_number'], self.tight_oil_play_map['output_index']))
        self.shale_gas_play_to_index = dict(zip(self.shale_gas_play_map['play_number'], self.shale_gas_play_map['output_index']))
        
        # Create dictionaries: {output_index: [play_numbers]} grouped by output index
        self.tight_oil_index_to_plays = self.tight_oil_play_map.groupby('output_index')['play_number'].apply(list).to_dict()
        self.shale_gas_index_to_plays = self.shale_gas_play_map.groupby('output_index')['play_number'].apply(list).to_dict()

        # Create reverse mappings from play names to play numbers for STEO benchmark matching
        # {play_name: [list of play_numbers]} for both tight oil and shale gas
        self.tight_oil_play_name_to_numbers = self.tight_oil_play_map.groupby('play_name')['play_number'].apply(list).to_dict()
        self.shale_gas_play_name_to_numbers = self.shale_gas_play_map.groupby('play_name')['play_number'].apply(list).to_dict()
        
        # Create mapping from play_number to play_name for SGTO play name lookup
        self.tight_oil_play_number_to_name = dict(zip(self.tight_oil_play_map['play_number'], self.tight_oil_play_map['play_name']))
        self.shale_gas_play_number_to_name = dict(zip(self.shale_gas_play_map['play_number'], self.shale_gas_play_map['play_name']))

        #Load STEO benchmark files
        steo_input_path = self.onshore_input_path.replace('onshore\\', 'steo\\')
        # Try to load via setup table, fallback to direct load if entry not found
        try:
            if nam.steo_togqshloil in self.setup_table.index:
                self.steo_tight_oil_benchmarks = super()._load_dataframe(steo_input_path, nam.steo_togqshloil)
            else:
                raise KeyError('steo_togqshloil not in setup table')
        except (KeyError, IndexError):
            # Fallback: load directly if not in setup table
            self.steo_tight_oil_benchmarks = com.read_dataframe(steo_input_path + 'steo_togqshloil.csv', skiprows=1).copy()
        
        try:
            if nam.steo_togqshlgas in self.setup_table.index:
                self.steo_shale_gas_benchmarks = super()._load_dataframe(steo_input_path, nam.steo_togqshlgas)
            else:
                raise KeyError('steo_togqshlgas not in setup table')
        except (KeyError, IndexError):
            # Fallback: load directly if not in setup table
            self.steo_shale_gas_benchmarks = com.read_dataframe(steo_input_path + 'steo_togqshlgas.csv', skiprows=1).copy()

        #Load base oil price by play
        self.base_oil_prc_by_play = super()._load_dataframe(self.onshore_input_path, nam.on_base_oil_prc_by_play)
        # Merge with tight_oil_play_map to get play_number
        # Note: multiple play_numbers can map to same play_name, so we keep all matches
        self.base_oil_prc_by_play = self.base_oil_prc_by_play.merge(
            self.tight_oil_play_map[['play_name', 'play_number']], 
            how='left', 
            on='play_name'
        )
        
        # Adjust base oil price by play for inflation (from zero_year to 1987, same as global base_oil_prc)
        inflation_mult = com.calculate_inflation(self.parent.rest_mc_jpgdp, self.zero_year)
        self.base_oil_prc_by_play['base_oil_prc'] = self.base_oil_prc_by_play['base_oil_prc'] * inflation_mult

        #Load decline rate configuration files for producing projects
        # Load play-level decline rates (plays only, no regions)
        self.producing_oil_decline_rates_df = super()._load_dataframe(self.onshore_input_path, nam.on_producing_oil_decline_rates)
        # Load region-level decline rates (regions with optional oil_type_number filtering)
        self.producing_oil_decline_rates_regions_df = super()._load_dataframe(self.onshore_input_path, nam.on_producing_oil_decline_rates_regions)
        self.producing_gas_decline_rates_df = super()._load_dataframe(self.onshore_input_path, nam.on_producing_gas_decline_rates_play)
        # Load region-level decline rates (regions with optional gas_type_number filtering)
        self.producing_gas_decline_rates_regions_df = super()._load_dataframe(self.onshore_input_path, nam.on_producing_gas_decline_rates_regions)
        
        # Convert year_1_decline and decline_rate columns to float for play-level rates
        self.producing_oil_decline_rates_df['year_1_decline'] = self.producing_oil_decline_rates_df['year_1_decline'].astype(float)
        self.producing_oil_decline_rates_df['decline_rate'] = self.producing_oil_decline_rates_df['decline_rate'].astype(float)
        self.producing_gas_decline_rates_df['year_1_decline'] = self.producing_gas_decline_rates_df['year_1_decline'].astype(float)
        self.producing_gas_decline_rates_df['decline_rate'] = self.producing_gas_decline_rates_df['decline_rate'].astype(float)

        # Process play-level decline rates
        # Merge play-level decline rates with play maps to get play_number
        oil_play_decline_rates_df = self.producing_oil_decline_rates_df.merge(self.tight_oil_play_map, how='left', on='play_name').fillna(0)
        
        # Ensure play_number is integer for proper comparison
        oil_play_decline_rates_df['play_number'] = oil_play_decline_rates_df['play_number'].astype(int)
        
        # Create dictionaries mapping play_number (as int) to year_1_decline and decline_rate for play-level entries
        self.producing_oil_year_1_decline_rates = {int(k): v for k, v in zip(oil_play_decline_rates_df['play_number'], oil_play_decline_rates_df['year_1_decline'])}
        self.producing_oil_decline_rates = {int(k): v for k, v in zip(oil_play_decline_rates_df['play_number'], oil_play_decline_rates_df['decline_rate'])}
        
        # Process region-level decline rates with oil_type_number filtering
        # Convert year_1_decline and decline_rate columns to float
        self.producing_oil_decline_rates_regions_df['year_1_decline'] = self.producing_oil_decline_rates_regions_df['year_1_decline'].astype(float)
        self.producing_oil_decline_rates_regions_df['decline_rate'] = self.producing_oil_decline_rates_regions_df['decline_rate'].astype(float)
        
        # Strip whitespace from region_name to ensure proper matching
        self.producing_oil_decline_rates_regions_df['region_name'] = self.producing_oil_decline_rates_regions_df['region_name'].astype(str).str.strip()
        
        # Convert oil_type_number: empty/NaN becomes None (wildcard), otherwise convert to int
        self.producing_oil_decline_rates_regions_df['oil_type_number'] = self.producing_oil_decline_rates_regions_df['oil_type_number'].replace('', np.nan)
        self.producing_oil_decline_rates_regions_df['oil_type_number'] = pd.to_numeric(self.producing_oil_decline_rates_regions_df['oil_type_number'], errors='coerce')
        # Convert NaN to None, and valid numbers to int
        self.producing_oil_decline_rates_regions_df['oil_type_number'] = self.producing_oil_decline_rates_regions_df['oil_type_number'].apply(
            lambda x: None if pd.isna(x) else int(x)
        )
        
        # Create nested dictionary structure: {(region_name, oil_type_number): (year_1_decline, decline_rate)}
        # Use tuple keys for efficient lookup, where None represents wildcard (all oil types)
        self.producing_oil_decline_rates_by_region = {}
        for _, row in self.producing_oil_decline_rates_regions_df.iterrows():
            region_name = row['region_name']
            oil_type_number = row['oil_type_number']
            key = (region_name, oil_type_number)
            self.producing_oil_decline_rates_by_region[key] = (row['year_1_decline'], row['decline_rate'])

        # Merge gas decline rates with play maps to get play_number (gas decline rates remain play-level only for now)
        self.producing_gas_decline_rates_df = self.producing_gas_decline_rates_df.merge(self.tight_oil_play_map, how='left', on='play_name').fillna(0)
        
        # Ensure play_number is integer for proper comparison (consistent with oil dictionaries)
        self.producing_gas_decline_rates_df['play_number'] = self.producing_gas_decline_rates_df['play_number'].astype(int)
        
        # Create dictionaries mapping play_number (as int) to year_1_decline and decline_rate for gas
        self.producing_gas_year_1_decline_rates = {int(k): v for k, v in zip(self.producing_gas_decline_rates_df['play_number'], self.producing_gas_decline_rates_df['year_1_decline'])}
        self.producing_gas_decline_rates = {int(k): v for k, v in zip(self.producing_gas_decline_rates_df['play_number'], self.producing_gas_decline_rates_df['decline_rate'])}
        
        # Process region-level decline rates with gas_type_number filtering
        # Convert year_1_decline and decline_rate columns to float
        self.producing_gas_decline_rates_regions_df['year_1_decline'] = self.producing_gas_decline_rates_regions_df['year_1_decline'].astype(float)
        self.producing_gas_decline_rates_regions_df['decline_rate'] = self.producing_gas_decline_rates_regions_df['decline_rate'].astype(float)
        
        # Strip whitespace and normalize to lowercase from region_name to ensure proper matching
        self.producing_gas_decline_rates_regions_df['region_name'] = self.producing_gas_decline_rates_regions_df['region_name'].astype(str).str.strip().str.lower()
        
        # Convert gas_type_number: empty/NaN becomes None (wildcard), otherwise convert to int
        self.producing_gas_decline_rates_regions_df['gas_type_number'] = self.producing_gas_decline_rates_regions_df['gas_type_number'].replace('', np.nan)
        self.producing_gas_decline_rates_regions_df['gas_type_number'] = pd.to_numeric(self.producing_gas_decline_rates_regions_df['gas_type_number'], errors='coerce')
        # Convert NaN to None, and valid numbers to int
        self.producing_gas_decline_rates_regions_df['gas_type_number'] = self.producing_gas_decline_rates_regions_df['gas_type_number'].apply(
            lambda x: None if pd.isna(x) else int(x)
        )
        
        # Create nested dictionary structure: {(region_name, gas_type_number): (year_1_decline, decline_rate)}
        # Use tuple keys for efficient lookup, where None represents wildcard (all gas types)
        self.producing_gas_decline_rates_by_region = {}
        for _, row in self.producing_gas_decline_rates_regions_df.iterrows():
            region_name = row['region_name']
            gas_type_number = row['gas_type_number']
            key = (region_name, gas_type_number)
            self.producing_gas_decline_rates_by_region[key] = (row['year_1_decline'], row['decline_rate'])

        #Load unified future wells configuration file (production multipliers)
        self.future_wells_config_df = super()._load_dataframe(self.onshore_input_path, 'on_future_wells_configuration')
        
        # Convert columns to appropriate types
        self.future_wells_config_df['production_multiplier'] = self.future_wells_config_df['production_multiplier'].astype(float)
        self.future_wells_config_df['multiplier_start_year'] = self.future_wells_config_df['multiplier_start_year'].astype(int)
        self.future_wells_config_df['multiplier_apply_every_year'] = self.future_wells_config_df['multiplier_apply_every_year'].astype(int)
        
        # Merge with play maps to get play_number(s)
        # Some plays have multiple play_numbers (e.g., Haynesville has 4774 and 4775)
        # We need to expand the config to include all play_numbers for each play_name
        
        # Combine both play maps to get all play_name -> play_number mappings
        combined_play_map = pd.concat([
            self.tight_oil_play_map[['play_name', 'play_number']],
            self.shale_gas_play_map[['play_name', 'play_number']]
        ]).drop_duplicates()
        
        # Merge to expand config: one row per (play_name, play_number) combination
        # This handles plays with multiple play_numbers correctly
        self.future_wells_config_df = self.future_wells_config_df.merge(
            combined_play_map,
            how='left',
            on='play_name'
        )
        
        # Filter out region_default rows (region-level multipliers are now in separate file)
        self.future_wells_config_df = self.future_wells_config_df[self.future_wells_config_df['play_name'] != 'region_default'].copy()
        
        # Handle region_name column - ensure it exists and clean values
        # This must be done BEFORE the split so both oil and gas dataframes have the cleaned column
        if 'region_name' not in self.future_wells_config_df.columns:
            self.future_wells_config_df['region_name'] = ''
        self.future_wells_config_df['region_name'] = self.future_wells_config_df['region_name'].fillna('').astype(str).str.strip()
        
        # Split by commodity_type and create separate dataframes
        self.future_wells_oil_config_df = self.future_wells_config_df[self.future_wells_config_df['commodity_type'] == 'oil'].copy()
        self.future_wells_gas_config_df = self.future_wells_config_df[self.future_wells_config_df['commodity_type'] == 'gas'].copy()
        
        # Create dictionaries: {play_number: {'production_multiplier': float, 'start_year': int, 'apply_every_year': int}}
        self.future_wells_oil_production_multipliers = {}
        for _, row in self.future_wells_oil_config_df.iterrows():
            # Skip rows where play_number is NaN (play not found in map)
            if pd.isna(row['play_number']):
                continue
            play_num = int(row['play_number'])
            if play_num not in self.future_wells_oil_production_multipliers:
                self.future_wells_oil_production_multipliers[play_num] = {
                    'production_multiplier': row['production_multiplier'],
                    'start_year': int(row['multiplier_start_year']),
                    'apply_every_year': int(row['multiplier_apply_every_year'])
                }
        
        self.future_wells_gas_production_multipliers = {}
        for _, row in self.future_wells_gas_config_df.iterrows():
            # Skip rows where play_number is NaN (play not found in map)
            if pd.isna(row['play_number']):
                continue
            play_num = int(row['play_number'])
            if play_num not in self.future_wells_gas_production_multipliers:
                self.future_wells_gas_production_multipliers[play_num] = {
                    'production_multiplier': row['production_multiplier'],
                    'start_year': int(row['multiplier_start_year']),
                    'apply_every_year': int(row['multiplier_apply_every_year'])
                }
        
        # Store default values for fallback (other_sgto_plays)
        default_oil_row = self.future_wells_oil_config_df[self.future_wells_oil_config_df['play_name'] == 'other_sgto_plays']
        if not default_oil_row.empty:
            self.future_wells_oil_production_multiplier_default = {
                'production_multiplier': float(default_oil_row.iloc[0]['production_multiplier']),
                'start_year': int(default_oil_row.iloc[0]['multiplier_start_year']),
                'apply_every_year': int(default_oil_row.iloc[0]['multiplier_apply_every_year'])
            }
        else:
            # Default: 1.0 for production_multiplier (no change), 9999 for start_year (never), 0 for apply_every_year
            self.future_wells_oil_production_multiplier_default = {'production_multiplier': 1.0, 'start_year': 9999, 'apply_every_year': 0}
        
        default_gas_row = self.future_wells_gas_config_df[self.future_wells_gas_config_df['play_name'] == 'other_sgto_plays']
        if not default_gas_row.empty:
            self.future_wells_gas_production_multiplier_default = {
                'production_multiplier': float(default_gas_row.iloc[0]['production_multiplier']),
                'start_year': int(default_gas_row.iloc[0]['multiplier_start_year']),
                'apply_every_year': int(default_gas_row.iloc[0]['multiplier_apply_every_year'])
            }
        else:
            # Default: 1.0 for production_multiplier (no change), 9999 for start_year (never), 0 for apply_every_year
            self.future_wells_gas_production_multiplier_default = {'production_multiplier': 1.0, 'start_year': 9999, 'apply_every_year': 0}

        #Load unified override switches
        self.override_switches_df = super()._load_dataframe(self.onshore_input_path, 'on_override_switches')
        self.override_switches_df.set_index('key', drop=True, inplace=True)
        
        #Filter unified file into separate dictionaries for backward compatibility
        #Decline rate overrides: keys containing 'decline'
        decline_keys = [key for key in self.override_switches_df.index if 'decline' in key.lower()]
        self.decline_rate_overrides = self.override_switches_df.loc[decline_keys].copy()
        
        #Production multiplier overrides: keys containing 'production_multiplier'
        multiplier_keys = [key for key in self.override_switches_df.index if 'production_multiplier' in key.lower()]
        self.production_multiplier_overrides = self.override_switches_df.loc[multiplier_keys].copy()
        
        #Totpat multiplier overrides: keys containing 'totpat'
        totpat_keys = [key for key in self.override_switches_df.index if 'totpat' in key.lower()]
        self.totpat_multiplier_overrides = self.override_switches_df.loc[totpat_keys].copy()
        
        #Load totpat multiplier configuration
        self.totpat_multipliers_df = super()._load_dataframe(self.onshore_input_path, nam.on_totpat_multipliers)
        
        # Convert columns to appropriate types
        self.totpat_multipliers_df['multiplier'] = self.totpat_multipliers_df['multiplier'].astype(float)
        self.totpat_multipliers_df['start_year'] = self.totpat_multipliers_df['start_year'].astype(int)
        self.totpat_multipliers_df['apply_every_year'] = self.totpat_multipliers_df['apply_every_year'].astype(int)
        
        # Merge with tight_oil_play_map to get play_number
        self.totpat_multipliers_df = self.totpat_multipliers_df.merge(
            self.tight_oil_play_map[['play_name', 'play_number']], 
            how='left', 
            on='play_name'
        )
        
        # Create dictionary: {play_number: {'multiplier': float, 'start_year': int, 'apply_every_year': int}}
        # Filter out rows with NaN play_number (plays not in the map)
        self.totpat_multipliers = {}
        for _, row in self.totpat_multipliers_df.iterrows():
            # Skip rows where play_number is NaN (play not found in map)
            if pd.isna(row['play_number']):
                continue
            play_num = int(row['play_number'])
            if play_num not in self.totpat_multipliers:
                self.totpat_multipliers[play_num] = {
                    'multiplier': row['multiplier'],
                    'start_year': row['start_year'],
                    'apply_every_year': row['apply_every_year']
                }
        
        #Load royalty multiplier overrides configuration
        # Load directly from file path since it's not in setup_table
        try:
            royalty_mult_file = self.onshore_input_path + 'configuration\\on_royalty_multiplier_overrides.csv'
            self.royalty_multiplier_overrides = com.read_dataframe(royalty_mult_file, skiprows=1)
            # Filter out any rows that are notes or empty
            if len(self.royalty_multiplier_overrides) > 0:
                # Remove any empty rows
                self.royalty_multiplier_overrides = self.royalty_multiplier_overrides.dropna(subset=['region_name', 'commodity_type'])
                # Convert columns to appropriate types
                self.royalty_multiplier_overrides['multiplier'] = pd.to_numeric(self.royalty_multiplier_overrides['multiplier'], errors='coerce')
                # Normalize region_name and commodity_type: strip whitespace and convert to lowercase for consistent matching
                self.royalty_multiplier_overrides['region_name'] = self.royalty_multiplier_overrides['region_name'].astype(str).str.strip().str.lower()
                self.royalty_multiplier_overrides['commodity_type'] = self.royalty_multiplier_overrides['commodity_type'].astype(str).str.strip().str.lower()
                # Create dictionary for efficient lookup: {(region_name, commodity_type): multiplier}
                # Keys are normalized to lowercase for consistent matching with project data
                self.royalty_multipliers_dict = {}
                for _, row in self.royalty_multiplier_overrides.iterrows():
                    if pd.notna(row['region_name']) and pd.notna(row['commodity_type']) and pd.notna(row['multiplier']):
                        key = (row['region_name'], row['commodity_type'])
                        self.royalty_multipliers_dict[key] = float(row['multiplier'])
                self.logger.info(f'Loaded {len(self.royalty_multipliers_dict)} royalty multiplier overrides')
            else:
                self.royalty_multiplier_overrides = pd.DataFrame()
                self.royalty_multipliers_dict = {}
        except Exception as e:
            self.logger.warning(f'Could not load royalty multiplier overrides: {e}. Continuing without overrides.')
            self.royalty_multiplier_overrides = pd.DataFrame()
            self.royalty_multipliers_dict = {}
        
        # Store default value for fallback (other_sgto_plays)
        default_row = self.totpat_multipliers_df[self.totpat_multipliers_df['play_name'] == 'other_sgto_plays']
        if not default_row.empty:
            self.totpat_multiplier_default = {
                'multiplier': float(default_row.iloc[0]['multiplier']),
                'start_year': int(default_row.iloc[0]['start_year']),
                'apply_every_year': int(default_row.iloc[0]['apply_every_year'])
            }
        else:
            # Default: 1.0 multiplier (no change), start_year 9999 (never), apply_every_year 0
            self.totpat_multiplier_default = {'multiplier': 1.0, 'start_year': 9999, 'apply_every_year': 0}

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

        self.projects_co2_eor : df
            Table of CO2 EOR projects

        self.projects_undiscovered : df
            Table of undiscovered projects
        """

         #Make table column
        self.projects_continuous[nam.table]     = nam.continuous
        self.projects_producing_gas[nam.table]  = nam.producing_gas
        self.projects_producing_oil[nam.table]  = nam.producing_oil
        self.projects_co2_eor[nam.table]        = nam.co2_eor
        self.projects_undiscovered[nam.table]   = nam.undiscovered

        #Set unique index id for each project
        self.projects_continuous[nam.hsm_index]     = self.projects_continuous.index.copy()
        self.projects_producing_gas[nam.hsm_index]  = self.projects_producing_gas.index.copy() + self.projects_continuous[nam.hsm_index].max()    + 1
        self.projects_producing_oil[nam.hsm_index]  = self.projects_producing_oil.index.copy() + self.projects_producing_gas[nam.hsm_index].max() + 1
        self.projects_co2_eor[nam.hsm_index]        = self.projects_co2_eor.index.copy()       + self.projects_producing_oil[nam.hsm_index].max()     + 1
        self.projects_undiscovered[nam.hsm_index]   = self.projects_undiscovered.index.copy()  + self.projects_co2_eor[nam.hsm_index].max()       + 1

        #Merge projects with process codes - optimized to merge once via concat
        project_dfs = [
            self.projects_continuous, self.projects_producing_gas, self.projects_producing_oil,
            self.projects_co2_eor, self.projects_undiscovered
        ]
        project_lengths = [len(df) for df in project_dfs]
        all_projects = pd.concat(project_dfs, ignore_index=True)
        all_projects = pd.merge(all_projects, self.process_codes, left_on=nam.process_code, right_index=True, how='left')
        
        # Split back into separate dataframes
        idx = 0
        self.projects_continuous = all_projects.iloc[idx:idx+project_lengths[0]].reset_index(drop=True)
        idx += project_lengths[0]
        self.projects_producing_gas = all_projects.iloc[idx:idx+project_lengths[1]].reset_index(drop=True)
        idx += project_lengths[1]
        self.projects_producing_oil = all_projects.iloc[idx:idx+project_lengths[2]].reset_index(drop=True)
        idx += project_lengths[2]
        self.projects_co2_eor = all_projects.iloc[idx:idx+project_lengths[3]].reset_index(drop=True)
        idx += project_lengths[3]
        self.projects_undiscovered = all_projects.iloc[idx:idx+project_lengths[4]].reset_index(drop=True)

        #Get state - optimized to extract once on concatenated data
        # Extract state code from resid (characters 2-4 of resid string)
        project_dfs_state = [self.projects_continuous, self.projects_producing_gas, self.projects_producing_oil,
                             self.projects_co2_eor, self.projects_undiscovered]
        project_lengths_state = [len(df) for df in project_dfs_state]
        all_resids = pd.concat([df[nam.resid] for df in project_dfs_state], ignore_index=True)
        all_states = all_resids.str[2:4]  # Extract once for all projects
        
        # Split back and assign
        idx = 0
        self.projects_continuous[nam.state] = all_states.iloc[idx:idx+project_lengths_state[0]].values
        idx += project_lengths_state[0]
        self.projects_producing_gas[nam.state] = all_states.iloc[idx:idx+project_lengths_state[1]].values
        idx += project_lengths_state[1]
        self.projects_producing_oil[nam.state] = all_states.iloc[idx:idx+project_lengths_state[2]].values
        idx += project_lengths_state[2]
        self.projects_co2_eor[nam.state] = all_states.iloc[idx:idx+project_lengths_state[3]].values
        idx += project_lengths_state[3]
        self.projects_undiscovered[nam.state] = all_states.iloc[idx:idx+project_lengths_state[4]].values

        #Get region number - optimized to merge once via concat
        # Most projects use basic mapping, co2_eor needs extra columns
        project_dfs_basic = [
            self.projects_continuous, self.projects_producing_gas, self.projects_producing_oil,
            self.projects_undiscovered
        ]
        project_lengths_basic = [len(df) for df in project_dfs_basic]
        all_projects_basic = pd.concat(project_dfs_basic, ignore_index=True)
        
        # Check if region_number already exists to avoid duplicate column issues
        merge_cols = [nam.district_number, nam.region_number]
        if nam.region_name not in all_projects_basic.columns:
            merge_cols.append(nam.region_name)
        all_projects_basic = pd.merge(all_projects_basic, self.parent.mapping[merge_cols], 
                                      on=nam.district_number, how='left')
        
        # Split back
        idx = 0
        self.projects_continuous = all_projects_basic.iloc[idx:idx+project_lengths_basic[0]].reset_index(drop=True)
        idx += project_lengths_basic[0]
        self.projects_producing_gas = all_projects_basic.iloc[idx:idx+project_lengths_basic[1]].reset_index(drop=True)
        idx += project_lengths_basic[1]
        self.projects_producing_oil = all_projects_basic.iloc[idx:idx+project_lengths_basic[2]].reset_index(drop=True)
        idx += project_lengths_basic[2]
        self.projects_undiscovered = all_projects_basic.iloc[idx:idx+project_lengths_basic[3]].reset_index(drop=True)
        
        # Handle co2_eor separately with extra columns
        self.projects_co2_eor = pd.merge(self.projects_co2_eor, 
                                        self.parent.mapping[[nam.district_number, nam.region_number,'census_division','census_region']],
                                        on=nam.district_number, how='left')

        #Get LFMM mapping - optimized to merge once via concat
        project_dfs_lfmm = [
            self.projects_continuous, self.projects_producing_gas, self.projects_producing_oil,
            self.projects_co2_eor, self.projects_undiscovered
        ]
        project_lengths_lfmm = [len(df) for df in project_dfs_lfmm]
        all_projects_lfmm = pd.concat(project_dfs_lfmm, ignore_index=True)
        all_projects_lfmm = pd.merge(all_projects_lfmm, 
                                     self.parent.mapping[[nam.district_number, nam.lfmm_region_number, nam.padd]],
                                     on=nam.district_number, how='left')
        
        # Split back
        idx = 0
        self.projects_continuous = all_projects_lfmm.iloc[idx:idx+project_lengths_lfmm[0]].reset_index(drop=True)
        idx += project_lengths_lfmm[0]
        self.projects_producing_gas = all_projects_lfmm.iloc[idx:idx+project_lengths_lfmm[1]].reset_index(drop=True)
        idx += project_lengths_lfmm[1]
        self.projects_producing_oil = all_projects_lfmm.iloc[idx:idx+project_lengths_lfmm[2]].reset_index(drop=True)
        idx += project_lengths_lfmm[2]
        self.projects_co2_eor = all_projects_lfmm.iloc[idx:idx+project_lengths_lfmm[3]].reset_index(drop=True)
        idx += project_lengths_lfmm[3]
        self.projects_undiscovered = all_projects_lfmm.iloc[idx:idx+project_lengths_lfmm[4]].reset_index(drop=True)

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
        self.projects_co2_eor[nam.prod_tier]       = self.projects_co2_eor[nam.resid].str[0:1]
        self.projects_undiscovered[nam.prod_tier]  = self.projects_undiscovered[nam.resid].str[0:1]

        #Get USGS province number
        self.projects_continuous[nam.usgs_province_num] = self.projects_continuous[nam.resid].str[4:6].astype(int)
        self.projects_producing_gas[nam.usgs_province_num] = self.projects_producing_gas[nam.resid].str[4:6].astype(int)
        self.projects_producing_oil[nam.usgs_province_num] = self.projects_producing_oil[nam.resid].str[4:6].astype(int)
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
        self.projects_co2_eor       = usgs_province_num_merge_match(self.projects_co2_eor)
        self.projects_undiscovered  = usgs_province_num_merge_match(self.projects_undiscovered)

        #Drop unused columns to reduce runtime
        prod_range      = list(range(self.parent.final_aeo_year - self.zero_year + 1))

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

        self.projects_co2_eor : df
            Table of CO2 EOR projects

        self.projects_undiscovered : df
            Table of undiscovered projects
        """

        project_table_list = [self.projects_continuous,
                        self.projects_producing_gas,
                        self.projects_producing_oil,
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
            #Fix Sulfur Volumes by State - vectorized using direct .loc assignments
            state_sulfur_mask = well_type_mask & sulfur_mask
            table.loc[state_sulfur_mask & (table[nam.state] == 'AL'), nam.sulfur] = 2.41
            table.loc[state_sulfur_mask & (table[nam.state] == 'CA'), nam.sulfur] = 1.41
            table.loc[state_sulfur_mask & (table[nam.state] == 'CO'), nam.sulfur] = 0.16
            table.loc[state_sulfur_mask & (table[nam.state] == 'KS'), nam.sulfur] = 0.24
            table.loc[state_sulfur_mask & (table[nam.state] == 'OK'), nam.sulfur] = 0.30
            table.loc[state_sulfur_mask & (table[nam.state] == 'TX'), nam.sulfur] = 0.22

            #Fix Sulfur Volumes by Province - vectorized using direct .loc assignments
            sulfur_mask = table[nam.sulfur] <= 0
            province_sulfur_mask = well_type_mask & sulfur_mask
            
            # Define province groups
            province_group_0_2 = [20,21,22,36,38,39,40,45,47,55,58,60,62]
            province_group_0_4 = [10,18,19,27,28,31,43,44,46,51,52,53,56,59,61,63,64,67]
            province_group_1_2 = [6,7,8,11,12,13,14,33,34,35,49,50,65]
            
            table.loc[province_sulfur_mask & table[nam.usgs_province_num].isin(province_group_0_2), nam.sulfur] = 0.2
            table.loc[province_sulfur_mask & table[nam.usgs_province_num].isin(province_group_0_4), nam.sulfur] = 0.4
            table.loc[province_sulfur_mask & table[nam.usgs_province_num].isin(province_group_1_2), nam.sulfur] = 1.2
            # All other provinces
            all_specified_provinces = province_group_0_2 + province_group_0_4 + province_group_1_2
            table.loc[province_sulfur_mask & ~table[nam.usgs_province_num].isin(all_specified_provinces), nam.sulfur] = 0.2

            ###Fix API Numbers and set crude heat content (BTU per barrel) - vectorized
            table.loc[no_api_mask & table[nam.usgs_province_num].isin([6, 13]), nam.api] = 25
            table.loc[no_api_mask & table[nam.usgs_province_num].isin([38, 67]), nam.api] = 35
            table.loc[no_api_mask & ~table[nam.usgs_province_num].isin([6, 13, 38, 67]), nam.api] = 45

            table[nam.crude_heat] = self.crude_heat_base * np.exp(-0.0043*table[nam.api]) * 42 * 0.000001

            ###Set LFMM crude type volumes and average api - vectorized using np.select
            # Note: California (type 7) is set last to overwrite previous assignments
            
            # Build conditions and values for crude type assignment
            conditions = [
                # Crude Type 5- API < 27, Low Sulfur
                api_mask_27 & sulf_1_1_mask & ~ca_mask,
                # Crude Type 6- API < 27, High Sulfur
                api_mask_27 & ~sulf_1_1_mask & ~ca_mask,
                # Crude type 3, 27 < API < 35, Low Sulfur
                api_mask_35 & ~api_mask_27 & sulf_1_1_mask & ~ca_mask,
                # Crude type 4, 27 < API < 35, High Sulfur
                api_mask_35 & ~api_mask_27 & ~sulf_1_1_mask & ~ca_mask,
                # Crude type 1, 35 < API < 40, Low Sulfur
                api_mask_40 & ~api_mask_35 & sulf_0_5_mask & ~ca_mask,
                # Crude type 2, 35 < API < 40, High Sulfur
                api_mask_40 & ~api_mask_35 & ~sulf_0_5_mask & ~ca_mask,
                # Crude type 10, 40 < API < 50, Low Sulfur
                api_mask_50 & ~api_mask_40 & sulf_0_5_mask & ~ca_mask,
                # Crude type 2, 40 < API < 50, High Sulfur
                api_mask_50 & ~api_mask_40 & ~sulf_0_5_mask & ~ca_mask,
                # Crude type 11, API > 50, Low Sulfur
                ~api_mask_50 & sulf_0_5_mask & ~ca_mask,
                # Crude type 2, API > 50, High Sulfur
                ~api_mask_50 & ~sulf_0_5_mask & ~ca_mask,
            ]
            
            crude_type_values = [5, 6, 3, 4, 1, 2, 10, 2, 11, 2]
            avg_api_values = [22.5, 19.9, 32.7, 30.9, 39.7, 37.3, 45.0, 37.3, 55.0, 37.3]
            
            # Assign crude types and avg_api using vectorized operations
            table[nam.lfmm_crude_type] = np.select(conditions, crude_type_values, default=0)
            table[nam.avg_api] = np.select(conditions, avg_api_values, default=0.0)
            
            # Crude Type - California (overwrites previous assignments)
            table.loc[ca_mask, nam.lfmm_crude_type] = 7
            table.loc[ca_mask, nam.avg_api] = 17.4

            #Set lfmm crude type as integer
            table[nam.lfmm_crude_type] = table[nam.lfmm_crude_type].astype(int)

        pass

    def _convert_decline_to_retention(self, decline_pct):
        """Convert decline percentage to retention percentage.
        
        Parameters
        ----------
        decline_pct : float
            Decline percentage (e.g., 0.05 for 5% decline)
            
        Returns
        -------
        float
            Retention percentage (e.g., 0.95 for 5% decline)
        """
        return 1.0 - decline_pct if decline_pct > 0 else 0.0

    def _get_oil_decline_rates(self, row, excluded_plays):
        """Get oil decline rates (play-level or region-level).
        
        Parameters
        ----------
        row : pd.Series
            Project row from producing_projects DataFrame
        excluded_plays : set
            Set of play numbers that use play-level rates
            
        Returns
        -------
        tuple
            (year_1_decline_raw, decline_rate_raw, rate_source)
        """
        # Get play number and convert to int for comparison
        play_number_raw = row.get(nam.play, None)
        try:
            play_number = int(play_number_raw) if pd.notna(play_number_raw) else None
        except (ValueError, TypeError):
            play_number = None
        
        # Check if play_number is in tight_oil_play_map (play-level adjustment)
        if play_number is not None and play_number in excluded_plays:
            # Use play-level decline rates
            year_1_decline_raw = self.producing_oil_year_1_decline_rates.get(play_number, self.producing_oil_year_1_decline_rates.get(0, 0.7))
            decline_rate_raw = self.producing_oil_decline_rates.get(play_number, self.producing_oil_decline_rates.get(0, 0.05))
            rate_source = 'play-level'
        else:
            # Use region-level decline rates (for tight oil plays not in tight_oil_play_map)
            region_name = row.get(nam.region_name, '')
            # Strip whitespace and ensure proper matching
            if isinstance(region_name, str):
                region_name = region_name.strip()
            
            # Get oil_type_number from row for filtering
            oil_type_number_raw = row.get(nam.oil_type_number, None)
            try:
                oil_type_number = int(oil_type_number_raw) if pd.notna(oil_type_number_raw) else None
            except (ValueError, TypeError):
                oil_type_number = None
            
            # Look up region-level rates with oil_type_number filtering
            # First try exact match on (region_name, oil_type_number)
            # Then try wildcard match on (region_name, None)
            year_1_decline_raw = 0
            decline_rate_raw = 0
            rate_source = 'none (region not found)'
            
            if region_name:
                # Try exact match first
                exact_key = (region_name, oil_type_number)
                if exact_key in self.producing_oil_decline_rates_by_region:
                    year_1_decline_raw, decline_rate_raw = self.producing_oil_decline_rates_by_region[exact_key]
                    rate_source = f'region-level ({region_name}, oil_type={oil_type_number})'
                else:
                    # Try wildcard match (None for oil_type_number means all types)
                    wildcard_key = (region_name, None)
                    if wildcard_key in self.producing_oil_decline_rates_by_region:
                        year_1_decline_raw, decline_rate_raw = self.producing_oil_decline_rates_by_region[wildcard_key]
                        rate_source = f'region-level ({region_name}, all oil types)'
        
        return (year_1_decline_raw, decline_rate_raw, rate_source)

    def _get_gas_decline_rates(self, row, excluded_plays):
        """Get gas decline rates (play-level first, then region-level).
        
        Parameters
        ----------
        row : pd.Series
            Project row from producing_projects DataFrame
        excluded_plays : set
            Set of play numbers that use play-level rates
            
        Returns
        -------
        tuple
            (year_1_decline_raw, decline_rate_raw, rate_source)
        """
        # Get region_name and gas_type_number for region-level lookup
        region_name = row.get(nam.region_name, '')
        # Strip whitespace and normalize to lowercase for matching
        if isinstance(region_name, str):
            region_name = region_name.strip().lower()
        else:
            region_name = ''
        
        # Get gas_type_number from row for filtering
        gas_type_number_raw = row.get(nam.gas_type_number, None)
        try:
            gas_type_number = int(gas_type_number_raw) if pd.notna(gas_type_number_raw) else None
        except (ValueError, TypeError):
            gas_type_number = None
        
        # Get play number and convert to int for comparison
        play_number_raw = row.get(nam.play, None)
        try:
            play_number = int(play_number_raw) if pd.notna(play_number_raw) else None
        except (ValueError, TypeError):
            play_number = None
        
        # Check if play_number is in excluded_plays (play-level adjustment)
        # If play is in excluded_plays, ALWAYS use play-level rates and NEVER check region-level
        if play_number is not None and play_number in excluded_plays:
            # Get play-level decline rates (use explicit entry if exists, otherwise use defaults)
            if play_number in self.producing_gas_year_1_decline_rates and play_number in self.producing_gas_decline_rates:
                # Use explicit play-level rates
                year_1_decline_play = self.producing_gas_year_1_decline_rates[play_number]
                decline_rate_play = self.producing_gas_decline_rates[play_number]
                return (year_1_decline_play, decline_rate_play, 'play-level')
            else:
                # Play is in map but no explicit entry - use defaults and exclude from region-level
                year_1_decline_play = self.producing_gas_year_1_decline_rates.get(0, 0.7)
                decline_rate_play = self.producing_gas_decline_rates.get(0, 0.05)
                return (year_1_decline_play, decline_rate_play, 'play-level (default)')
        
        # Only check region-level rates if play is NOT in excluded_plays
        year_1_decline_raw = 0
        decline_rate_raw = 0
        rate_source = 'none (region not found)'
        
        if region_name:
            # Try exact match first
            exact_key = (region_name, gas_type_number)
            if exact_key in self.producing_gas_decline_rates_by_region:
                year_1_decline_raw, decline_rate_raw = self.producing_gas_decline_rates_by_region[exact_key]
                rate_source = f'region-level ({region_name}, gas_type={gas_type_number})'
            else:
                # Try wildcard match (None for gas_type_number means all types)
                wildcard_key = (region_name, None)
                if wildcard_key in self.producing_gas_decline_rates_by_region:
                    year_1_decline_raw, decline_rate_raw = self.producing_gas_decline_rates_by_region[wildcard_key]
                    rate_source = f'region-level ({region_name}, all gas types)'
        
        return (year_1_decline_raw, decline_rate_raw, rate_source)

    def apply_royalty_multiplier_overrides(self, project_df):
        """Apply royalty multiplier overrides to projects based on region and commodity type.
        
        Parameters
        ----------
        project_df : pd.DataFrame
            DataFrame containing projects with columns: region_number, region_name, resource_type, project_royalty_multiplier
            
        Returns
        -------
        project_df : pd.DataFrame
            DataFrame with updated project_royalty_multiplier values
        """
        # Check if overrides are available
        if not hasattr(self, 'royalty_multipliers_dict') or not self.royalty_multipliers_dict:
            return project_df
        
        # Ensure project_royalty_multiplier exists and fill missing values with 1.0
        if nam.project_royalty_multiplier not in project_df.columns:
            project_df[nam.project_royalty_multiplier] = 1.0
        else:
            project_df[nam.project_royalty_multiplier] = project_df[nam.project_royalty_multiplier].fillna(1.0)
        
        # Filter to onshore projects only (region_number 1-7)
        onshore_mask = project_df[nam.region_number].isin([1, 2, 3, 4, 5, 6, 7])
        onshore_projects = project_df[onshore_mask].copy()
        
        if len(onshore_projects) == 0:
            return project_df
        
        # Ensure region_name exists and is string
        if nam.region_name not in onshore_projects.columns:
            self.logger.warning('region_name column not found in project_df. Cannot apply royalty multiplier overrides.')
            return project_df
        
        # Normalize region_name to lowercase and strip whitespace for consistent matching
        onshore_projects[nam.region_name] = onshore_projects[nam.region_name].astype(str).str.strip().str.lower()
        
        # Ensure resource_type exists to determine commodity_type
        if nam.resource_type not in onshore_projects.columns:
            # Try to infer from OP1/GP1 columns if resource_type is missing
            if 'OP1' in onshore_projects.columns and 'GP1' in onshore_projects.columns:
                # Determine resource_type based on production columns
                oil_mask = onshore_projects['OP1'].fillna(0) > 0
                gas_mask = onshore_projects['GP1'].fillna(0) > 0
                onshore_projects[nam.resource_type] = 'oil'  # Default
                onshore_projects.loc[gas_mask & ~oil_mask, nam.resource_type] = 'gas'
                onshore_projects.loc[oil_mask & gas_mask, nam.resource_type] = 'oil'  # Prefer oil if both
            else:
                self.logger.warning('resource_type column not found and cannot be inferred. Cannot apply royalty multiplier overrides.')
                return project_df
        else:
            # Ensure resource_type is string and handle any NaN values
            onshore_projects[nam.resource_type] = onshore_projects[nam.resource_type].astype(str).str.strip().str.lower()
            # Fill any NaN or empty values by inferring from OP1/GP1
            missing_resource_type = (onshore_projects[nam.resource_type] == '') | (onshore_projects[nam.resource_type] == 'nan')
            if missing_resource_type.any() and 'OP1' in onshore_projects.columns and 'GP1' in onshore_projects.columns:
                oil_mask = onshore_projects['OP1'].fillna(0) > 0
                gas_mask = onshore_projects['GP1'].fillna(0) > 0
                onshore_projects.loc[missing_resource_type & oil_mask, nam.resource_type] = 'oil'
                onshore_projects.loc[missing_resource_type & gas_mask & ~oil_mask, nam.resource_type] = 'gas'
                onshore_projects.loc[missing_resource_type & ~oil_mask & ~gas_mask, nam.resource_type] = 'oil'  # Default to oil if no production
        
        # Apply multipliers using vectorized operations for better performance
        # Create a series to store multipliers for each project
        multipliers_series = pd.Series(1.0, index=onshore_projects.index)
        
        for idx in onshore_projects.index:
            region_name = str(onshore_projects.loc[idx, nam.region_name]).strip().lower()
            resource_type = str(onshore_projects.loc[idx, nam.resource_type]).strip().lower()
            
            # Map resource_type to commodity_type
            commodity_type = 'oil' if resource_type == 'oil' else 'gas'
            
            # Look up multiplier - ensure region_name is normalized (lowercase, stripped)
            key = (region_name, commodity_type)
            if key in self.royalty_multipliers_dict:
                multiplier = self.royalty_multipliers_dict[key]
                multipliers_series.loc[idx] = multiplier
        
        # Ensure multipliers_series is properly aligned with onshore_projects index
        multipliers_series = multipliers_series.reindex(onshore_projects.index, fill_value=1.0)
        
        # Apply multipliers to project_royalty_multiplier (apply all, including 1.0 to ensure consistency)
        onshore_projects[nam.project_royalty_multiplier] = onshore_projects[nam.project_royalty_multiplier] * multipliers_series
        
        # Cap project_royalty_multiplier at 1.0 if it exceeds 1.0
        capped_count = (onshore_projects[nam.project_royalty_multiplier] > 1.0).sum()
        if capped_count > 0:
            onshore_projects.loc[onshore_projects[nam.project_royalty_multiplier] > 1.0, nam.project_royalty_multiplier] = 1.0
            self.logger.info(f'Capped {capped_count} project royalty multipliers at 1.0')
        
        # Verify the multiplication worked (check for NaN or unexpected values)
        if onshore_projects[nam.project_royalty_multiplier].isna().any():
            self.logger.warning(f'Warning: {onshore_projects[nam.project_royalty_multiplier].isna().sum()} NaN values created during multiplier application. This may indicate an index alignment issue.')
        
        # Update the original dataframe with modified onshore projects
        # Use explicit index alignment to ensure update succeeds
        try:
            project_df.loc[onshore_projects.index, nam.project_royalty_multiplier] = onshore_projects[nam.project_royalty_multiplier]
        except Exception as e:
            self.logger.error(f'Error updating project_df with multipliers: {e}. Attempting alternative update method.')
            # Fallback: use update method
            project_df.update(onshore_projects[[nam.project_royalty_multiplier]])
        
        return project_df

    def _apply_decline_curve(self, idx, year_1_retention, decline_rate, columns, base_value):
        """Apply decline curve to production columns.
        
        Parameters
        ----------
        idx : int
            Row index in producing_projects DataFrame
        year_1_retention : float
            Retention percentage for year 2 (already converted from decline percentage)
        decline_rate : float
            Annual decline rate as percentage (e.g., 0.05 for 5% decline)
        columns : list
            List of column names (e.g., ['OP1', 'OP2', ...] or ['GP1', 'GP2', ...])
        base_value : float
            Base production value (e.g., OP1 or GP1)
        """
        if year_1_retention > 0:
            # Column[1] (e.g., OP2 or GP2) = base_value * year_1_retention
            self.producing_projects.at[idx, columns[1]] = base_value * year_1_retention
            
            # Columns[2:] (e.g., OP3-OP40 or GP3-GP40): Apply exponential decline
            if decline_rate > 0:
                # Column_n = base_value * year_1_retention * (1 - decline_rate)^(n-2) for n >= 3
                for i, col in enumerate(columns[2:], start=3):  # Start from column 3
                    self.producing_projects.at[idx, col] = base_value * year_1_retention * ((1 - decline_rate) ** (i - 2))

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

        # Set resource_type based on source table (oil projects from producing_oil, gas from producing_gas)
        # This is needed for royalty multiplier overrides
        # After concat with ignore_index=True, index is 0, 1, 2, ...
        # First N rows are from producing_oil, rest are from producing_gas
        num_oil = len(self.projects_producing_oil)
        num_gas = len(self.projects_producing_gas)
        
        # Create resource_type array: first num_oil are 'oil', rest are 'gas'
        # Ensure it's explicitly a string type to avoid any type issues
        resource_type_array = ['oil'] * num_oil + ['gas'] * num_gas
        self.producing_projects[nam.resource_type] = pd.Series(resource_type_array, dtype=str, index=self.producing_projects.index)

        # Convert OP and GP columns to numeric to support comparison operations
        # OP1-OP40 and GP1-GP40 columns must be numeric for >, < comparisons
        op_columns = [f'OP{i}' for i in range(1, 41)]
        gp_columns = [f'GP{i}' for i in range(1, 41)]
        
        # Convert existing OP columns to numeric
        for col in op_columns:
            if col in self.producing_projects.columns:
                self.producing_projects[col] = pd.to_numeric(self.producing_projects[col], errors='coerce').fillna(0)
        
        # Convert existing GP columns to numeric
        for col in gp_columns:
            if col in self.producing_projects.columns:
                self.producing_projects[col] = pd.to_numeric(self.producing_projects[col], errors='coerce').fillna(0)

        # Apply exponential decline rate adjustments to OP1-OP40 and GP1-GP40 if enabled
        try:
            oil_override_enabled = self.decline_rate_overrides.loc['producing_oil_decline_override', 'value'] == 1
        except (KeyError, IndexError):
            oil_override_enabled = False
        
        try:
            gas_override_enabled = self.decline_rate_overrides.loc['producing_gas_decline_override', 'value'] == 1
        except (KeyError, IndexError):
            gas_override_enabled = False
        
        if oil_override_enabled or gas_override_enabled:
            # Get set of excluded play_numbers from tight_oil_play_map for efficient lookup
            excluded_plays = set(int(p) for p in self.tight_oil_play_map['play_number'].unique())
            # Get set of excluded play_numbers from shale_gas_play_map for gas
            excluded_plays_gas = set(int(p) for p in self.shale_gas_play_map['play_number'].unique())
            
            # For oil decline rates: apply to all oil projects (filtered by region and oil_type_number)
            # Only apply to legacy projects (process_code <= 7)
            # For gas decline rates: filter to only shale gas (5) projects
            if oil_override_enabled:
                # Filter to all oil projects (by resource_type or OP1 > 0) AND process_code <= 7 (legacy only)
                oil_mask = ((self.producing_projects[nam.resource_type] == 'oil') | (self.producing_projects['OP1'] > 0)) & (self.producing_projects[nam.process_code] <= 7)
                oil_indices = self.producing_projects[oil_mask].index
                
                # Apply oil decline rates to all oil projects
                for idx in oil_indices:
                    row = self.producing_projects.loc[idx]
                    
                    # Only apply if OP1 > 0 (has oil production)
                    if row['OP1'] > 0:
                        # Get decline rates (play-level or region-level)
                        year_1_decline_raw, decline_rate_raw, _ = self._get_oil_decline_rates(row, excluded_plays)
                        
                        # Only apply if we found a rate (non-zero or explicitly set)
                        if year_1_decline_raw != 0 or decline_rate_raw != 0:
                            # Convert decline percentage to retention percentage
                            year_1_retention = self._convert_decline_to_retention(year_1_decline_raw)
                            
                            # Apply decline curve to OP columns
                            self._apply_decline_curve(idx, year_1_retention, decline_rate_raw, op_columns, row['OP1'])
            
            # For gas decline rates: filter to only shale gas (5) projects
            if gas_override_enabled:
                # Filter to only tight oil (4) or shale gas (5) projects for gas
                tight_shale_mask = self.producing_projects[nam.process_code].isin([4, 5])
                filtered_indices = self.producing_projects[tight_shale_mask].index
                
                # Apply gas decline rates
                for idx in filtered_indices:
                    row = self.producing_projects.loc[idx]
                    
                    # Apply gas decline rate if GP1 > 0
                    if row['GP1'] > 0:
                        # Get decline rates (play-level or region-level)
                        year_1_decline_raw, decline_rate_raw, _ = self._get_gas_decline_rates(row, excluded_plays_gas)
                        
                        # Only apply if we found a rate (non-zero or explicitly set)
                        if year_1_decline_raw != 0 or decline_rate_raw != 0:
                            # Convert decline percentage to retention percentage
                            year_1_retention = self._convert_decline_to_retention(year_1_decline_raw)
                            
                            # Apply decline curve to GP columns
                            self._apply_decline_curve(idx, year_1_retention, decline_rate_raw, gp_columns, row['GP1'])

        # Apply royalty multiplier overrides to producing projects
        self.producing_projects = self.apply_royalty_multiplier_overrides(self.producing_projects)

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
        self.crude_production[nam.hsm_index]                = self.producing_projects[nam.hsm_index]
        self.crude_production[nam.resid]                    = self.producing_projects[nam.resid]
        self.crude_production[nam.process_code]             = self.producing_projects[nam.process_code]
        self.crude_production[nam.district_number]          = self.producing_projects[nam.district_number]
        self.crude_production[nam.region_number]            = self.producing_projects[nam.region_number]
        self.crude_production[nam.federal_land]             = self.producing_projects[nam.federal_land]
        self.crude_production[nam.well_type_number]         = self.producing_projects[nam.well_type_number]
        self.crude_production[nam.oil_type]                 = self.producing_projects[nam.oil_type]
        self.crude_production[nam.oil_type_number]          = self.producing_projects[nam.oil_type_number]
        self.crude_production[nam.gas_type]                 = self.producing_projects[nam.gas_type]
        self.crude_production[nam.gas_type_number]          = self.producing_projects[nam.gas_type_number]
        self.crude_production[nam.lfmm_crude_type]          = self.producing_projects[nam.lfmm_crude_type]
        self.crude_production[nam.api]                      = self.producing_projects[nam.api]
        self.crude_production[nam.avg_api]                  = self.producing_projects[nam.avg_api]
        self.crude_production[nam.play]                     = self.producing_projects[nam.play]
        self.crude_production[nam.project_royalty_multiplier] = self.producing_projects[nam.project_royalty_multiplier]
        self.crude_production[nam.year_production_start]    = self.rest_curcalyr
        
        # Merge process_codes to add description columns for legacy producing projects
        if not self.crude_production.empty and nam.process_code in self.crude_production.columns:
            process_code_cols = ['well_type', 'oil_type', 'gas_type', 'dev_type', 'resource_type', 'prime_fuel_type']
            available_cols = [col for col in process_code_cols if col in self.process_codes.columns]
            if available_cols:
                # Convert process_code to int for proper merge (process_codes index is int, but data may be float)
                process_code_int = pd.to_numeric(self.crude_production[nam.process_code], errors='coerce').astype('Int64')
                # Drop existing description columns if they exist (to avoid duplicates)
                cols_to_drop = [col for col in available_cols if col in self.crude_production.columns]
                if cols_to_drop:
                    self.crude_production = self.crude_production.drop(columns=cols_to_drop)
                # Merge with process_codes
                self.crude_production = self.crude_production.merge(
                    self.process_codes[available_cols], 
                    left_on=process_code_int, 
                    right_index=True, 
                    how='left'
                )
        
        # Add county name from producing projects
        if not self.crude_production.empty and nam.resid in self.crude_production.columns:
            county_name_col = 'cnty_name'
            # Try to get county name from producing_projects
            if county_name_col in self.producing_projects.columns:
                county_map = self.producing_projects[[nam.resid, county_name_col]].drop_duplicates(subset=nam.resid)
                self.crude_production = self.crude_production.merge(county_map, on=nam.resid, how='left')
        
        # Add SGTO play name column for legacy producing crude production
        def get_sgto_play_name_crude(row):
            play_number = row[nam.play]
            well_type = row[nam.well_type_number]
            
            # Handle NaN or None values
            if pd.isna(play_number) or pd.isna(well_type):
                if not pd.isna(well_type) and well_type in [2, 5]:
                    return "other"
                return ""
            
            # Convert play_number to int for dictionary lookup
            try:
                play_number_int = int(play_number)
            except (ValueError, TypeError):
                play_number_int = None
            
            if play_number_int is not None and play_number_int in self.tight_oil_play_number_to_name:
                return self.tight_oil_play_number_to_name[play_number_int]
            elif well_type in [2, 5]:
                return "other"
            else:
                return ""
        
        self.crude_production['sgto_play_name'] = self.crude_production.apply(get_sgto_play_name_crude, axis=1)

        #Apply natural gas production identifiers
        self.natgas_production[nam.hsm_index]               = self.producing_projects[nam.hsm_index]
        self.natgas_production[nam.resid]                   = self.producing_projects[nam.resid]
        self.natgas_production[nam.process_code]            = self.producing_projects[nam.process_code]
        self.natgas_production[nam.district_number]         = self.producing_projects[nam.district_number]
        self.natgas_production[nam.region_number]           = self.producing_projects[nam.region_number]
        self.natgas_production[nam.federal_land]             = self.producing_projects[nam.federal_land]
        self.natgas_production[nam.well_type_number]        = self.producing_projects[nam.well_type_number]
        self.natgas_production[nam.gas_type]                = self.producing_projects[nam.gas_type]
        self.natgas_production[nam.gas_type_number]         = self.producing_projects[nam.gas_type_number]
        self.natgas_production[nam.oil_type]                = self.producing_projects[nam.oil_type]
        self.natgas_production[nam.oil_type_number]         = self.producing_projects[nam.oil_type_number]
        self.natgas_production[nam.lfmm_crude_type]         = self.producing_projects[nam.lfmm_crude_type]
        self.natgas_production[nam.api]                     = self.producing_projects[nam.api]
        self.natgas_production[nam.avg_api]                 = self.producing_projects[nam.avg_api]
        self.natgas_production[nam.play]                    = self.producing_projects[nam.play]
        self.natgas_production[nam.project_royalty_multiplier] = self.producing_projects[
            nam.project_royalty_multiplier]
        self.natgas_production[nam.year_production_start]   = self.rest_curcalyr
        
        # Merge process_codes to add description columns for legacy producing projects
        if not self.natgas_production.empty and nam.process_code in self.natgas_production.columns:
            process_code_cols = ['well_type', 'oil_type', 'gas_type', 'dev_type', 'resource_type', 'prime_fuel_type']
            available_cols = [col for col in process_code_cols if col in self.process_codes.columns]
            if available_cols:
                # Convert process_code to int for proper merge (process_codes index is int, but data may be float)
                process_code_int = pd.to_numeric(self.natgas_production[nam.process_code], errors='coerce').astype('Int64')
                # Drop existing description columns if they exist (to avoid duplicates)
                cols_to_drop = [col for col in available_cols if col in self.natgas_production.columns]
                if cols_to_drop:
                    self.natgas_production = self.natgas_production.drop(columns=cols_to_drop)
                # Merge with process_codes
                self.natgas_production = self.natgas_production.merge(
                    self.process_codes[available_cols], 
                    left_on=process_code_int, 
                    right_index=True, 
                    how='left'
                )
        
        # Add county name from producing projects
        if not self.natgas_production.empty and nam.resid in self.natgas_production.columns:
            county_name_col = 'cnty_name'
            # Try to get county name from producing_projects
            if county_name_col in self.producing_projects.columns:
                county_map = self.producing_projects[[nam.resid, county_name_col]].drop_duplicates(subset=nam.resid)
                self.natgas_production = self.natgas_production.merge(county_map, on=nam.resid, how='left')
        
        # Add SGTO play name column for legacy producing natgas production
        def get_sgto_play_name_natgas(row):
            play_number = row[nam.play]
            well_type = row[nam.well_type_number]
            
            # Handle NaN or None values
            if pd.isna(play_number) or pd.isna(well_type):
                if not pd.isna(well_type) and well_type in [2, 5]:
                    return "other"
                return ""
            
            # Convert play_number to int for dictionary lookup
            try:
                play_number_int = int(play_number)
            except (ValueError, TypeError):
                play_number_int = None
            
            if play_number_int is not None and play_number_int in self.shale_gas_play_number_to_name:
                return self.shale_gas_play_number_to_name[play_number_int]
            elif well_type in [2, 5]:
                return "other"
            else:
                return ""
        
        self.natgas_production['sgto_play_name'] = self.natgas_production.apply(get_sgto_play_name_natgas, axis=1)

        #Apply ngpl production identifiers
        self.ngpl_production[nam.hsm_index]               = self.producing_projects[nam.hsm_index       ]
        self.ngpl_production[nam.resid]                   = self.producing_projects[nam.resid           ]
        self.ngpl_production[nam.process_code]            = self.producing_projects[nam.process_code    ]
        self.ngpl_production[nam.district_number]         = self.producing_projects[nam.district_number ]
        self.ngpl_production[nam.region_number]           = self.producing_projects[nam.region_number   ]
        self.ngpl_production[nam.federal_land]            = self.producing_projects[nam.federal_land]
        self.ngpl_production[nam.play]                    = self.producing_projects[nam.play]
        self.ngpl_production[nam.project_royalty_multiplier] = self.producing_projects[nam.project_royalty_multiplier]
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
        self.wells[nam.hsm_index]               = self.producing_projects[nam.hsm_index       ]
        self.wells[nam.resid]                   = self.producing_projects[nam.resid           ]
        self.wells[nam.process_code]            = self.producing_projects[nam.process_code    ]
        self.wells[nam.district_number]         = self.producing_projects[nam.district_number ]
        self.wells[nam.region_number]           = self.producing_projects[nam.region_number   ]
        self.wells[nam.well_type_number]        = self.producing_projects[nam.well_type_number]
        self.wells[nam.play]                    = self.producing_projects[nam.play]
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
        year_price_adj = self.parent.reg_crude_price.loc[[1,2,3,4,5,6,7]].copy()
        year_price_adj = year_price_adj.div(year_price_adj[self.zero_year], axis = 0)
        year_price_adj = year_price_adj ** self.price_elasticity_exponent

        price_adj_oil = oil_wells_df[[nam.region_number]].copy().reset_index().merge(year_price_adj,
                                                                                     how = 'left',
                                                                                     left_on = nam.region_number,
                                                                                     right_index = True).set_index('index')

        #Apply price_adj to oil wells and calculate producing oil wells by year
        eval_years = list(range(self.zero_year, (self.parent.final_aeo_year + 1)))
        oil_wells_df[eval_years] = self.producing_wells_multiplier * oil_wells_df[eval_years]
        oil_wells_df[eval_years] = oil_wells_df[eval_years].mul(price_adj_oil[eval_years], axis = 1)


        #Get Gas Price Adjustment value based on district natural gas prices
        year_price_adj = self.parent.dist_natgas_price.copy()
        year_price_adj = year_price_adj.div(year_price_adj[self.zero_year], axis = 0)
        year_price_adj = year_price_adj ** self.price_elasticity_exponent

        price_adj_gas = gas_wells_df[[nam.district_number]].copy().reset_index().merge(year_price_adj,
                                                                                     how = 'left',
                                                                                     left_on = nam.district_number,
                                                                                     right_index = True).set_index('index')

        #Apply price_adj to gas wells and calculate producing oil wells by year
        eval_years = list(range(self.zero_year, (self.parent.final_aeo_year + 1)))
        gas_wells_df[eval_years] = self.producing_wells_multiplier * gas_wells_df[eval_years]
        gas_wells_df[eval_years] = gas_wells_df[eval_years].mul(price_adj_gas[eval_years], axis = 1)

        #Update Self.wells with producing project wells
        self.wells.update(oil_wells_df)
        self.wells.update(gas_wells_df)


        ###Set Well Ratio
        #The goal here is to produce a  well count that matches STEO, and then to stabilize from there, we do not currently have good projections for producing wells
        self.wells[list(range(self.zero_year, (self.parent.steo_years[-1] + 3)))] = self.wells[
            list(range(self.zero_year, (self.parent.steo_years[-1] + 3)))].mul(self.wells_steo_ratio)
        self.wells[list(range((self.parent.steo_years[-1] + 3), (self.parent.final_aeo_year + 1)))] = self.wells[
            list(range((self.parent.steo_years[-1] + 3), (self.parent.final_aeo_year + 1)))].mul(self.wells_steo_ratio)

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
        # Ensure float64 dtype to avoid FutureWarnings about dtype incompatibility
        self.producing_price_df = self.producing_price_df.astype('float64')

        #Calculate oil price curve for producing project DCF using Restart File starting oil price path
        start_series_price = self.parent.rest_start_price.at[self.rest_curcalyr, nam.value]
        end_series_price = self.parent.rest_start_price[nam.value].iloc[-1]
        growth_rate = (end_series_price / start_series_price) ** (1. / (self.parent.final_aeo_year - (self.rest_curcalyr - 1)))
        self.producing_price_df.loc[nam.crude_price] = float(start_series_price)
        for year in prod_range:
            self.producing_price_df.at[nam.crude_price, year] = float(start_series_price * (growth_rate ** (year + 1)))


        #Calculate gas price curve for producing project DCF using average rate of natural gas price change between zero year and final year
        start_series_price = self.parent.rest_ogpngwhp.at[self.rest_curcalyr, nam.value]
        end_series_price = self.parent.rest_ogpngwhp[nam.value].iloc[-1]
        growth_rate = (end_series_price / start_series_price) ** (1. / (self.parent.final_aeo_year - (self.rest_curcalyr - 1)))
        self.producing_price_df.loc[nam.natgas_price] = float(start_series_price)
        for year in prod_range:
            self.producing_price_df.at[nam.natgas_price, year] = float(start_series_price * (growth_rate ** (year + 1)))

        #NGPL Price
        # Defragment before adding new columns to avoid PerformanceWarning
        self.producing_projects = self.producing_projects.copy()
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
        # Defragment before adding new columns to avoid PerformanceWarning
        self.producing_projects = self.producing_projects.copy()
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
        # Drop any unnamed columns that may have been created
        cost_mean_df = cost_mean_df.loc[:, ~cost_mean_df.columns.str.contains('^Unnamed', na=False)]
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
        self.producing_projects[nam.drill_cost] = 0.0
        self.producing_projects[nam.dry_hole_cost] = 0.0

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

        # Combine masks and filter in one step
        temp_oil_cost_df = self.producing_projects.loc[oil_mask & oil_prod_mask].copy()
        temp_gas_cost_df = self.producing_projects.loc[gas_mask & gas_prod_mask].copy()

        #Mask coefs by type
        oil_mask = self.drill_cost_eq_coefs[nam.resource_type] == 'oil'
        gas_mask = self.drill_cost_eq_coefs[nam.resource_type] == 'gas'

        #Create coef dfs
        oil_coef_df = self.drill_cost_eq_coefs.copy().loc[oil_mask].set_index(nam.coef_name)
        gas_coef_df = self.drill_cost_eq_coefs.copy().loc[gas_mask].set_index(nam.coef_name)

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
            temp_oil_cost_df.groupby(nam.district_number)[nam.usgs_province_coef].transform('mean') * self.cost_fillna_district_mult)

        #Fill by region number
        temp_oil_cost_df[nam.usgs_province_coef] = temp_oil_cost_df[nam.usgs_province_coef].fillna(
            temp_oil_cost_df.groupby(nam.region_number)[nam.usgs_province_coef].transform('mean') * self.cost_fillna_region_mult)

        #Fill by resource type
        temp_oil_cost_df[nam.usgs_province_coef] = temp_oil_cost_df[nam.usgs_province_coef].fillna(
            temp_oil_cost_df.groupby(nam.resource_type)[nam.usgs_province_coef].transform('mean') * self.cost_fillna_resource_mult)

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
            temp_oil_cost_df.groupby(nam.district_number)[nam.state_coef].transform('mean') * self.cost_fillna_district_mult)

        #Fill by region number
        temp_oil_cost_df[nam.state_coef] = temp_oil_cost_df[nam.state_coef].fillna(
            temp_oil_cost_df.groupby(nam.region_number)[nam.state_coef].transform('mean') * self.cost_fillna_region_mult)

        #Fill by resource type
        temp_oil_cost_df[nam.state_coef] = temp_oil_cost_df[nam.state_coef].fillna(
            temp_oil_cost_df.groupby(nam.resource_type)[nam.state_coef].transform('mean') * self.cost_fillna_resource_mult)

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
        lower_bound = oil_coef_df.at[nam.hist_cost_quantile_01, nam.coef] * self.cost_outlier_lower_mult #adjust for tech rate
        upper_bound = oil_coef_df.at[nam.hist_cost_quantile_99, nam.coef] * self.cost_outlier_upper_mult #adjust for tech rate
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
            temp_gas_cost_df.groupby(nam.district_number)[nam.state_coef].transform('mean') * self.cost_fillna_district_mult)

        # Fill by region number
        temp_gas_cost_df[nam.state_coef] = temp_gas_cost_df[nam.state_coef].fillna(
            temp_gas_cost_df.groupby(nam.region_number)[nam.state_coef].transform('mean') * self.cost_fillna_region_mult)

        # Fill by resource type
        temp_gas_cost_df[nam.state_coef] = temp_gas_cost_df[nam.state_coef].fillna(
            temp_gas_cost_df.groupby(nam.resource_type)[nam.state_coef].transform('mean') * self.cost_fillna_resource_mult)

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
        lower_bound = gas_coef_df.at[nam.hist_cost_quantile_01, nam.coef] * self.cost_outlier_lower_mult
        upper_bound = gas_coef_df.at[nam.hist_cost_quantile_99, nam.coef] * self.cost_outlier_upper_mult
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
        temp_df = temp_df.reset_index().merge(temp_dryhole_rate, on=[nam.region_number, nam.resource_type], how='left').set_index('index')
        # Perform USGS Province coef merge
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
        
        # Set conversion factors and economic parameters from onshore configuration
        self.cash_flow.boe_conversion = self.boe_conversion
        self.cash_flow.barrels_per_gallon = self.barrels_per_gallon
        self.cash_flow.ch4_to_metric_tons = self.ch4_to_metric_tons
        self.cash_flow.ngpl_volume_divisor = self.ngpl_volume_divisor
        self.cash_flow.min_years_before_abandon = self.min_years_before_abandon
        self.cash_flow.econ_life_default = self.econ_life_default

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
        self.cash_flow.properties[nam.sga_opex_well] = self.producing_projects[nam.sga_opex_well] * self.sga_producing_factor #Set to conservative factor (no new costs)

        #Set dryhole cost to 0
        self.cash_flow.dry_hole_cost    = pd.DataFrame(index=self.producing_projects.index,columns=list(range(self.evaluation_years))).fillna(0.0)

        #Load production opex
        self.cash_flow.properties[nam.production_opex_brl] = self.producing_projects[nam.production_opex_brl].copy().fillna(0.0) #Adjustment to BOE equiv in cashflow

        #Load transportation opex
        self.cash_flow.properties[nam.crude_trans_price] = self.producing_projects[nam.transport_opex_brl].copy().fillna(0.0)
        self.cash_flow.properties[nam.natgas_trans_price] = self.producing_projects[nam.transport_opex_brl].copy().fillna(0.0)  #Adjustment to BOE equiv in cashflow

        #Load capital cost
        self.cash_flow.kap_cost = pd.DataFrame(index=self.projects.index, columns=list(range(self.evaluation_years)))
        self.cash_flow.kap_cost[0] = (self.producing_projects[nam.facility_capex_well] * self.facility_producing_factor) #Assume some facilities are already built out
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


    def _validate_drilling_params(self, projects_df, stage_name=''):
        """Validate consistency between past_wells, totpat, well_decline_limit, and pattern sizes.
        
        Parameters
        ----------
        projects_df : pd.DataFrame
            DataFrame containing project data to validate
        stage_name : str, optional
            Name of the stage calling validation (for logging context)
        
        Notes
        -----
        This method checks:
        - past_wells >= 0 (auto-corrects if negative)
        - well_decline_limit matches calculated value from pattern sizes
        - totpat = well_decline_limit - past_wells (with ±1 tolerance for rounding)
        Logs warnings for inconsistencies but allows model to continue.
        """
        logger = logging.getLogger('onshore.py')
        stage_prefix = f"[{stage_name}] " if stage_name else ""
        
        # Ensure past_wells is non-negative
        negative_mask = projects_df[nam.past_wells] < 0
        if negative_mask.any():
            n_negative = negative_mask.sum()
            logger.warning(f"{stage_prefix}Found {n_negative} projects with negative past_wells. Setting to 0.")
            projects_df.loc[negative_mask, nam.past_wells] = 0
        
        # Check if well_decline_limit exists, if not calculate expected value
        if nam.well_decline_limit in projects_df.columns:
            # Calculate expected well_decline_limit from pattern sizes
            expected_wdl = (projects_df[nam.total_pattern_size_acres] / 
                           projects_df[nam.std_pattern_size_acres]).round(decimals=0)
            
            # Check for inconsistencies (allow ±1 for rounding differences)
            wdl_diff = (projects_df[nam.well_decline_limit] - expected_wdl).abs()
            inconsistent_mask = wdl_diff > 1
            
            if inconsistent_mask.any():
                n_inconsistent = inconsistent_mask.sum()
                max_diff = wdl_diff.max()
                logger.warning(
                    f"{stage_prefix}Found {n_inconsistent} projects where well_decline_limit differs from "
                    f"calculated value (total_pattern_size_acres / std_pattern_size_acres) by > 1. "
                    f"Max difference: {max_diff}"
                )
            
            # Check if past_wells > well_decline_limit (project may be depleted)
            depleted_mask = projects_df[nam.past_wells] > projects_df[nam.well_decline_limit]
            if depleted_mask.any():
                n_depleted = depleted_mask.sum()
                logger.warning(
                    f"{stage_prefix}Found {n_depleted} projects where past_wells > well_decline_limit. "
                    f"These projects may be depleted."
                )
            
            # Check totpat consistency if it exists
            if nam.totpat in projects_df.columns:
                expected_totpat = projects_df[nam.well_decline_limit] - projects_df[nam.past_wells]
                totpat_diff = (projects_df[nam.totpat] - expected_totpat).abs()
                inconsistent_totpat_mask = totpat_diff > 1
                
                if inconsistent_totpat_mask.any():
                    n_inconsistent = inconsistent_totpat_mask.sum()
                    max_diff = totpat_diff.max()
                    logger.warning(
                        f"{stage_prefix}Found {n_inconsistent} projects where totpat differs from "
                        f"calculated value (well_decline_limit - past_wells) by > 1. "
                        f"Max difference: {max_diff}"
                    )
                
                # Ensure totpat is non-negative
                negative_totpat_mask = projects_df[nam.totpat] < 0
                if negative_totpat_mask.any():
                    n_negative = negative_totpat_mask.sum()
                    logger.warning(f"{stage_prefix}Found {n_negative} projects with negative totpat. Setting to 0.")
                    projects_df.loc[negative_totpat_mask, nam.totpat] = 0


    def load_continuous_projects(self):
        """Load continuous projects into the master self.projects dataframe for economic analysis.

            * Assign last_year_drilling as NINJWELL value
            * Use past_wells from input file and validate consistency

        Returns
        -------
        self.projects : df
            Master DataFrame containing all projects to be processed in current model year
        """
        self.projects = self.projects_continuous.copy()
        self.projects[nam.last_year_drilling] = self.projects[nam.hist_year_wells]
        # Use past_wells directly from input file (no longer calculated from totpat)
        # Ensure past_wells is non-negative (safety check)
        self.projects.loc[self.projects[nam.past_wells] < 0, nam.past_wells] = 0
        
        # Validate consistency of drilling parameters
        self._validate_drilling_params(self.projects, 'load_continuous_projects')

        pass


    def load_eor_projects(self):
        """Load EOR/ASR projects into the master self.projects dataframe for economic analysis.

            * Load CO2 EOR projects
            * Pull out resource-restricted projects
            * Merge in economic life for producing projects to determine project eligibility (EOR projects can only move
              forward if associated producing project is approaching/just past end of economic life)

        Returns
        -------
        self.projects : df
            Master DataFrame containing all projects to be processed in current model year
        """
        #Join EOR and ASR dfs
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
        projects_eor[nam.federal_land] = 0
        # Set resource_type for EOR projects (typically oil)
        projects_eor[nam.resource_type] = 'oil'

        #CO2 EOR/ASR projects have resids that are identical to a matching conventional legacy production project minus the last two numbers
        #This is how projects will be identified to determine eligibility based on project economic life and other EOR/ASR activity
        projects_eor[nam.eor_resid] = projects_eor[nam.resid].str[:-2]
        self.producing_projects[nam.eor_resid] = self.producing_projects[nam.resid].str[:-2]

        #Load EOR projects into projects
        self.projects = pd.concat((self.projects, projects_eor), ignore_index = True)

        #Create shared projects index
        self.projects = self.projects.reset_index(drop=True)

        #Add Econ life to projects, ASR projects can only run within 5 years of project econ life
        self.projects = self.projects.reset_index().merge(self.producing_projects[[nam.eor_resid, nam.econ_life, nam.well_type]],
                                            how='left', on=[nam.well_type, nam.eor_resid]).set_index('index')
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

        # Set resource_type for continuous projects before applying royalty multiplier overrides
        # EOR projects already have resource_type set to 'oil' in load_eor_projects()
        # Continuous projects need resource_type set based on production columns (OP1/GP1)
        if nam.resource_type not in self.projects.columns:
            self.projects[nam.resource_type] = None
        
        # Determine resource_type for projects that don't have it set (continuous projects)
        # Use OP1/GP1 production columns to determine if project is oil or gas
        # Handle NaN, empty string, and string 'nan' values
        resource_type_str = self.projects[nam.resource_type].astype(str).str.lower().str.strip()
        missing_resource_type = (self.projects[nam.resource_type].isna() | 
                                (resource_type_str == '') | 
                                (resource_type_str == 'nan') |
                                (resource_type_str == 'none'))
        if missing_resource_type.any():
            # Check if OP1 and GP1 columns exist
            if 'OP1' in self.projects.columns and 'GP1' in self.projects.columns:
                # Determine resource_type based on production columns
                oil_mask = self.projects['OP1'].fillna(0) > 0
                gas_mask = self.projects['GP1'].fillna(0) > 0
                
                # Set resource_type: gas if only gas production, oil if oil production (or both)
                self.projects.loc[missing_resource_type & gas_mask & ~oil_mask, nam.resource_type] = 'gas'
                self.projects.loc[missing_resource_type & oil_mask, nam.resource_type] = 'oil'
                # Default to oil if no production data available
                still_missing = self.projects[nam.resource_type].isna() | (self.projects[nam.resource_type] == '')
                if still_missing.any():
                    self.projects.loc[still_missing, nam.resource_type] = 'oil'
            else:
                self.logger.warning('OP1/GP1 columns not found. Cannot set resource_type for continuous projects. Royalty multiplier overrides may not work correctly.')

        # Apply royalty multiplier overrides to continuous and EOR projects
        # resource_type should now be set for all projects
        self.projects = self.apply_royalty_multiplier_overrides(self.projects)

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
            # Use vectorized operations to avoid DataFrame fragmentation
            well_type_merge = project_df[nam.well_type].copy()
            well_type_merge.loc[well_type_merge == 'Conventional (Oil)'] = 'Conventional'
            well_type_merge.loc[well_type_merge == 'Conventional (Gas)'] = 'Conventional'
            well_type_merge.loc[(project_df[nam.process_code] <= 11) |
                        (project_df[nam.process_code] == 16) |
                        (project_df[nam.process_code] == 17) |
                        (project_df[nam.process_code] == 18) |
                        (project_df[nam.process_code] == 19)] = 'Conventional'
            well_type_merge.loc[(project_df[nam.process_code] == 12) |
                        (project_df[nam.process_code] == 20)] = 'Tight Oil'
            well_type_merge.loc[(project_df[nam.process_code] == 13) |
                        (project_df[nam.process_code] == 14) |
                        (project_df[nam.process_code] == 21) |
                        (project_df[nam.process_code] == 22)] = 'Shale Gas'
            well_type_merge.loc[(project_df[nam.process_code] == 15) |
                        (project_df[nam.process_code] == 23)] = 'Coalbed Methane'
            # Assign directly to avoid duplicate column issues
            project_df[nam.well_type_merge] = well_type_merge

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
                temp_region_cost_df[cost_type] = temp_region_cost_df[cost_type] * self.regional_cost_multiplier

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
            # Drop any unnamed columns that may have been created
            cost_mean_df = cost_mean_df.loc[:, ~cost_mean_df.columns.str.contains('^Unnamed', na=False)]
            temp_national_cost_df = temp_national_cost_df.reset_index().merge(cost_mean_df,
                                                                              how='left',
                                                                              on=[nam.well_type_merge]).set_index('index')

            #Assign higher cost adder to national matches assuming that these are not highly economical producing regions
            for cost_type in cost_type_list:
                temp_national_cost_df[cost_type] = temp_national_cost_df[cost_type] * self.national_cost_multiplier

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
        temp_df[nam.production_opex_brl] = temp_df[nam.production_opex_brl] * self.eor_other_opex_multiplier
        temp_df[nam.facility_capex_well] = temp_df[nam.facility_capex_well] * self.eor_other_capex_multiplier
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
            #Create drilling cost and dryhole cost columns (use float64 to avoid dtype warnings)
            project_df[nam.drill_cost] = 0.0
            project_df[nam.dry_hole_cost] = 0.0

            #Mask projects by type
            oil_mask = project_df[nam.resource_type] == 'oil'
            gas_mask = project_df[nam.resource_type] == 'gas'

            #Create temp cost dfs
            temp_oil_cost_df = project_df[oil_mask].copy()
            temp_gas_cost_df = project_df[gas_mask].copy()

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
                temp_oil_cost_df.groupby(nam.district_number)[nam.usgs_province_coef].transform('mean') * self.cost_fillna_district_mult)

            # Fill by region number
            temp_oil_cost_df[nam.usgs_province_coef] = temp_oil_cost_df[nam.usgs_province_coef].fillna(
                temp_oil_cost_df.groupby(nam.region_number)[nam.usgs_province_coef].transform('mean') * self.cost_fillna_region_mult)

            # Fill by resource type
            temp_oil_cost_df[nam.usgs_province_coef] = temp_oil_cost_df[nam.usgs_province_coef].fillna(
                temp_oil_cost_df.groupby(nam.resource_type)[nam.usgs_province_coef].transform('mean') * self.cost_fillna_resource_mult)

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
                temp_oil_cost_df.groupby(nam.district_number)[nam.state_coef].transform('mean') * self.cost_fillna_district_mult)

            #Fill by region number
            temp_oil_cost_df[nam.state_coef] = temp_oil_cost_df[nam.state_coef].fillna(
                temp_oil_cost_df.groupby(nam.region_number)[nam.state_coef].transform('mean') * self.cost_fillna_region_mult)

            #Fill by resource type
            temp_oil_cost_df[nam.state_coef] = temp_oil_cost_df[nam.state_coef].fillna(
                temp_oil_cost_df.groupby(nam.resource_type)[nam.state_coef].transform('mean') * self.cost_fillna_resource_mult)

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
            lower_bound = oil_coef_df.at[nam.hist_cost_quantile_01, nam.coef] * self.cost_outlier_lower_mult #adjust for tech rate
            upper_bound = oil_coef_df.at[nam.hist_cost_quantile_99, nam.coef] * self.cost_outlier_upper_mult #adjust for tech rate
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
                temp_gas_cost_df.groupby(nam.district_number)[nam.usgs_province_coef].transform('mean') * self.cost_fillna_district_mult)

            #Fill by region number
            temp_gas_cost_df[nam.usgs_province_coef] = temp_gas_cost_df[nam.usgs_province_coef].fillna(
                temp_gas_cost_df.groupby(nam.region_number)[nam.usgs_province_coef].transform('mean') * self.cost_fillna_region_mult)

            #Fill by resource type
            temp_gas_cost_df[nam.usgs_province_coef] = temp_gas_cost_df[nam.usgs_province_coef].fillna(
                temp_gas_cost_df.groupby(nam.resource_type)[nam.usgs_province_coef].transform('mean') * self.cost_fillna_resource_mult)

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
                temp_gas_cost_df.groupby(nam.district_number)[nam.state_coef].transform('mean') * self.cost_fillna_district_mult)

            # Fill by region number
            temp_gas_cost_df[nam.state_coef] = temp_gas_cost_df[nam.state_coef].fillna(
                temp_gas_cost_df.groupby(nam.region_number)[nam.state_coef].transform('mean') * self.cost_fillna_region_mult)

            # Fill by resource type
            temp_gas_cost_df[nam.state_coef] = temp_gas_cost_df[nam.state_coef].fillna(
                temp_gas_cost_df.groupby(nam.resource_type)[nam.state_coef].transform('mean') * self.cost_fillna_resource_mult)

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
            lower_bound = gas_coef_df.at[nam.hist_cost_quantile_01, nam.coef] * self.cost_outlier_lower_mult
            upper_bound = gas_coef_df.at[nam.hist_cost_quantile_99, nam.coef] * self.cost_outlier_upper_mult
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
            # Ensure columns are compatible dtype to avoid dtype warnings
            for col in temp_oil_cost_df.columns:
                if col in project_df.columns:
                    if project_df[col].dtype != temp_oil_cost_df[col].dtype:
                        project_df[col] = project_df[col].astype(temp_oil_cost_df[col].dtype)
            project_df.update(temp_oil_cost_df)
            
            for col in temp_gas_cost_df.columns:
                if col in project_df.columns:
                    if project_df[col].dtype != temp_gas_cost_df[col].dtype:
                        project_df[col] = project_df[col].astype(temp_gas_cost_df[col].dtype)
            project_df.update(temp_gas_cost_df)


            ###Get Dryhole Cost (Category 1 - Tight/Shale, Category 2 - exploration conventional, Category 3 - development conventional)
            #Mask based on well type and assign dryhole rates
            tight_shale_mask = ((project_df[nam.well_type_number] == 2) | (self.projects[nam.well_type_number] >= 4))
            undisc_mask = project_df[nam.process_code] >= 16
            past_wells_mask = project_df[nam.past_wells] > 10

            #Shale & Tight Wells
            temp_df = project_df.loc[tight_shale_mask].copy()
            temp_dryhole_rate = self.dryhole_rate.loc[self.dryhole_rate[nam.drill_category] == 1]
            temp_df = temp_df.reset_index().merge(temp_dryhole_rate, on=[nam.region_number, nam.resource_type], how='left').set_index('index')
            temp_df[nam.dry_hole_cost] = temp_df[nam.drill_cost].mul(0.33334) #Rystad reports that 1/3 of capex is drill cost while 2/3 is completion costs 11/08/2022
            temp_df[nam.dry_hole_cost] = temp_df[nam.dry_hole_cost].mul(temp_df[nam.dryhole_rate]) #Dryhole rates from OGSM
            # Ensure columns are float64 to avoid dtype warnings
            for col in temp_df.columns:
                if col in project_df.columns and project_df[col].dtype != temp_df[col].dtype:
                    project_df[col] = project_df[col].astype(temp_df[col].dtype)
            project_df.update(temp_df)

            #Undiscovered Exploration Wells
            temp_df = project_df[undisc_mask & ~past_wells_mask].copy()
            # Drop dryhole_rate and drill_category if they exist from previous merges to avoid suffix conflicts
            if nam.dryhole_rate in temp_df.columns:
                temp_df = temp_df.drop(columns=[nam.dryhole_rate])
            if nam.drill_category in temp_df.columns:
                temp_df = temp_df.drop(columns=[nam.drill_category])
            temp_dryhole_rate = self.dryhole_rate.loc[self.dryhole_rate[nam.drill_category] == 2]
            temp_df = temp_df.reset_index().merge(temp_dryhole_rate, on=[nam.region_number, nam.resource_type], how='left').set_index('index')
            temp_df[nam.dry_hole_cost] = temp_df[nam.drill_cost].mul(0.33334) #Rystad reports that 1/3 of capex is drill cost while 2/3 is completion costs 11/08/2022
            temp_df[nam.dry_hole_cost] = temp_df[nam.dry_hole_cost].mul(temp_df[nam.dryhole_rate]) #Dryhole rates from OGSM
            # Ensure columns are float64 to avoid dtype warnings
            for col in temp_df.columns:
                if col in project_df.columns and project_df[col].dtype != temp_df[col].dtype:
                    project_df[col] = project_df[col].astype(temp_df[col].dtype)
            project_df.update(temp_df)

            #Undiscovered Development Wells
            temp_df = project_df[undisc_mask & past_wells_mask].copy()
            temp_dryhole_rate = self.dryhole_rate.loc[self.dryhole_rate[nam.drill_category] == 3]
            temp_df = temp_df.reset_index().merge(temp_dryhole_rate, on=[nam.region_number, nam.resource_type], how='left').set_index('index')
            temp_df[nam.dry_hole_cost] = temp_df[nam.drill_cost].mul(0.33334) #Rystad reports that 1/3 of capex is drill cost while 2/3 is completion costs 11/08/2022
            temp_df[nam.dry_hole_cost] = temp_df[nam.dry_hole_cost].mul(temp_df[nam.dryhole_rate]) #Dryhole rates from OGSM
            # Ensure columns are float64 to avoid dtype warnings
            for col in temp_df.columns:
                if col in project_df.columns and project_df[col].dtype != temp_df[col].dtype:
                    project_df[col] = project_df[col].astype(temp_df[col].dtype)
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
        tier_one_mask = (self.projects[nam.hist_year_wells] >= 1) & (self.projects[nam.hist_year_wells] < self.tier_boundary_low) & (self.projects[nam.resource_type] == 'oil')
        temp_df = self.projects[tier_one_mask].copy()
        cadj = self.cost_adj_tier_one
        temp_df[nam.drill_cost] = temp_df[nam.drill_cost] * cadj
        temp_df[nam.dry_hole_cost] = temp_df[nam.dry_hole_cost] * cadj
        temp_df[nam.facility_capex_well] = temp_df[nam.facility_capex_well] * cadj
        self.projects.update(temp_df)

        #Wells with 10 wells or more of production
        tier_two_mask = (self.projects[nam.hist_year_wells] >= self.tier_boundary_low) & (self.projects[nam.hist_year_wells] < self.tier_boundary_high) & (self.projects[nam.resource_type] == 'oil')
        temp_df = self.projects[tier_two_mask].copy()
        cadj = self.cost_adj_tier_two
        temp_df[nam.drill_cost] = temp_df[nam.drill_cost] * cadj
        temp_df[nam.dry_hole_cost] = temp_df[nam.dry_hole_cost] * cadj
        temp_df[nam.facility_capex_well] = temp_df[nam.facility_capex_well] * cadj
        self.projects.update(temp_df)

        #Wells with 50 wells or more of production
        tier_three_mask = (self.projects[nam.hist_year_wells] >= self.tier_boundary_high) & (self.projects[nam.resource_type] == 'oil')
        temp_df = self.projects[tier_three_mask].copy()
        cadj = self.cost_adj_tier_three
        temp_df[nam.drill_cost] = temp_df[nam.drill_cost] * cadj
        temp_df[nam.dry_hole_cost] = temp_df[nam.dry_hole_cost] * cadj
        temp_df[nam.facility_capex_well] = temp_df[nam.facility_capex_well] * cadj
        self.projects.update(temp_df)


        ###Natural Gas
        tier_one_mask = (self.projects[nam.hist_year_wells] >= 1) & (self.projects[nam.hist_year_wells] < self.tier_boundary_low) & (self.projects[nam.resource_type] == 'gas')
        temp_df = self.projects[tier_one_mask].copy()
        cadj = self.cost_adj_tier_one
        temp_df[nam.drill_cost] = temp_df[nam.drill_cost] * cadj
        temp_df[nam.dry_hole_cost] = temp_df[nam.dry_hole_cost] * cadj
        temp_df[nam.facility_capex_well] = temp_df[nam.facility_capex_well] * cadj
        self.projects.update(temp_df)

        #Wells with 10 wells or more of production
        tier_two_mask = (self.projects[nam.hist_year_wells] >= self.tier_boundary_low) & (self.projects[nam.hist_year_wells] < self.tier_boundary_high) & (self.projects[nam.resource_type] == 'gas')
        temp_df = self.projects[tier_two_mask].copy()
        cadj = self.cost_adj_tier_two
        temp_df[nam.drill_cost] = temp_df[nam.drill_cost] * cadj
        temp_df[nam.dry_hole_cost] = temp_df[nam.dry_hole_cost] * cadj
        temp_df[nam.facility_capex_well] = temp_df[nam.facility_capex_well] * cadj
        self.projects.update(temp_df)

        #Wells with 50 wells or more of production
        tier_three_mask = (self.projects[nam.hist_year_wells] >= self.tier_boundary_high) & (self.projects[nam.resource_type] == 'gas')
        temp_df = self.projects[tier_three_mask].copy()
        cadj = self.cost_adj_tier_three
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

        Price averaging behavior:
            * Positive averaging_years: forward-looking (current year + N future years)
              Example: averaging_years=2 averages current year + 2 future years (3 years total)
            * Negative averaging_years: backward-looking (N past years + current year)
              Example: averaging_years=-2 averages 2 past years + current year (3 years total)

        Returns
        -------
        self.projects : df
            Master DataFrame containing all projects to be processed in current model year

        self.projects_undiscovered : df
            DataFrame of undiscovered projects
        """
        #Get average years
        # If averaging_years < 0: backward-looking (N past years + current)
        # If averaging_years >= 0: forward-looking (current + N future years)
        if self.averaging_years < 0:
            avg_years = list(range(self.rest_curcalyr + self.averaging_years, self.rest_curcalyr + 1))
        else:
            avg_years = list(range(self.rest_curcalyr, self.rest_curcalyr + self.averaging_years + 1))

        # Helper function to filter years to those available in price dataframe
        # For forward-looking, progressively fall back if future years don't exist
        def get_available_years(requested_years, price_df):
            """Filter requested years to those available in price_df, with fallback for forward-looking.
            
            For forward-looking (positive averaging_years):
            - First try: all requested years
            - If missing: try current + 1 future year
            - If still missing: fall back to just current year
            """
            available_years = [yr for yr in requested_years if yr in price_df.columns]
            
            # If forward-looking and some years are missing, try progressive fallback
            if self.averaging_years >= 0 and len(available_years) < len(requested_years):
                # Try current + 1 future year
                fallback_years = [self.rest_curcalyr, self.rest_curcalyr + 1]
                fallback_available = [yr for yr in fallback_years if yr in price_df.columns]
                if len(fallback_available) > 0:
                    return fallback_available
                # Fall back to just current year
                if self.rest_curcalyr in price_df.columns:
                    return [self.rest_curcalyr]
            
            # For backward-looking or if all requested years are available, return what we have
            return available_years if len(available_years) > 0 else [self.rest_curcalyr]

        ###Crude
        # Get available years for crude prices
        crude_avg_years = get_available_years(avg_years, self.parent.reg_crude_price)
        
        # Continuous and EOR/ASR projects
        temp_price = pd.merge(self.projects[nam.region_number],
                              self.parent.reg_crude_price[crude_avg_years],
                              left_on=nam.region_number,
                              right_index=True,
                              how='left')
        temp_price[nam.crude_price] = temp_price[crude_avg_years].mean(axis=1)
        crude_nan_count = temp_price[nam.crude_price].isna().sum()
        if crude_nan_count > 0:
            logging.getLogger('onshore.py').warning(f'Filling {crude_nan_count} NaN crude prices with base_oil_prc={self.base_oil_prc}')
        self.projects[nam.crude_price] = temp_price[nam.crude_price].fillna(self.base_oil_prc)
        self.projects[nam.crude_price_lag] = temp_price[crude_avg_years[0]] # these lag prices do not appear to be used

        #Undiscovered projects
        temp_price = pd.merge(self.projects_undiscovered[nam.region_number],
                              self.parent.reg_crude_price[crude_avg_years],
                              left_on=nam.region_number,
                              right_index=True,
                              how='left')
        temp_price[nam.crude_price] = temp_price[crude_avg_years].mean(axis=1)
        # Before fillna, check for and log NaN values
        crude_nan_count = temp_price[nam.crude_price].isna().sum()
        if crude_nan_count > 0:
            logging.getLogger('onshore.py').warning(f'Filling {crude_nan_count} NaN crude prices with base_oil_prc={self.base_oil_prc}')
        self.projects_undiscovered[nam.crude_price] = temp_price[nam.crude_price].fillna(self.base_oil_prc).copy()
        self.projects_undiscovered[nam.crude_price_lag] = temp_price[crude_avg_years[0]].copy() # these lag prices do not appear to be used


        ###Natural Gas
        # Get available years for natural gas prices (use district prices to check availability)
        natgas_avg_years = get_available_years(avg_years, self.parent.dist_natgas_price)
        
        #Continuous and EOR/ASR projects
        temp_price = pd.merge(self.projects[nam.district_number],
                              self.parent.dist_natgas_price[natgas_avg_years],
                              left_on=nam.district_number,
                              right_index=True,
                              how='left')
        temp_price[nam.natgas_price] = temp_price[natgas_avg_years].mean(axis=1)

        #Apply regional prices if district price = 0
        temp_price_mask = temp_price[nam.natgas_price] == 0
        temp_reg_price = pd.merge(self.projects.loc[temp_price_mask, nam.region_number],
                              self.parent.reg_natgas_price[natgas_avg_years],
                              left_on=nam.region_number,
                              right_index=True,
                              how='left')
        temp_reg_price[nam.natgas_price] = temp_reg_price[natgas_avg_years].mean(axis=1)
        temp_price.update(temp_reg_price)

        #Apply Prices
        natgas_nan_count = temp_price[nam.natgas_price].isna().sum()
        if natgas_nan_count > 0:
            logging.getLogger('onshore.py').warning(f'Filling {natgas_nan_count} NaN natgas prices with base_gas_prc={self.base_gas_prc}')
        self.projects[nam.natgas_price] = temp_price[nam.natgas_price].fillna(self.base_gas_prc).copy()
        self.projects[nam.natgas_price_lag] = temp_price[natgas_avg_years[0]].copy()# these lag prices do not appear to be used


        #Undiscovered projects
        temp_price = pd.merge(self.projects_undiscovered[nam.region_number],
                              self.parent.reg_natgas_price[natgas_avg_years],
                              left_on=nam.region_number,
                              right_index=True,
                              how='left')
        temp_price[nam.natgas_price] = temp_price[natgas_avg_years].mean(axis=1)

        #Apply regional prices if district price = 0
        temp_price_mask = temp_price[nam.natgas_price] == 0
        temp_reg_price = pd.merge(self.projects_undiscovered.loc[temp_price_mask, nam.region_number],
                              self.parent.reg_natgas_price[natgas_avg_years],
                              left_on=nam.region_number,
                              right_index=True,
                              how='left')
        temp_reg_price[nam.natgas_price] = temp_reg_price[natgas_avg_years].mean(axis=1)
        temp_price.update(temp_reg_price)

        #Apply Prices
        natgas_nan_count = temp_price[nam.natgas_price].isna().sum()
        if natgas_nan_count > 0:
            logging.getLogger('onshore.py').warning(f'Filling {natgas_nan_count} NaN natgas prices with base_gas_prc={self.base_gas_prc}')
        self.projects_undiscovered[nam.natgas_price] = temp_price[nam.natgas_price].fillna(self.base_gas_prc).copy()
        self.projects_undiscovered[nam.natgas_price_lag] = temp_price[natgas_avg_years[0]].copy() # these lag prices do not appear to be used


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


            # Apply Brent Coeffiicent
            # If brent prices are too low, then it can constrain the Eastern region way too much, which doesn't make too much
            # sense because the eastern region is mostly gas-directed drilling, and should not be so sensitive to brent price.
            # If this constraint is hit due to low brent prices, then alot of profitable projects are left on the table in the region.
            # To resolve this for now: We multiply Brent by a factor (currently 1.35) to alleviate the constraint just for the Eastern Region.
            # This is something that was previously only done for the low price case, but if prices are too low in the Reference case, needs
            # to be done in the Reference (and all other) cases as well.
            if region == 'region_1' and self.override_switches_df.loc['drilling_cons_override', 'value'] == 1:
                brent_price = self.parent.rest_brent_price.at[self.rest_curcalyr, nam.value] * self.override_switches_df.loc['drilling_cons_multiplier', 'value'] # Factor to multiply brent by so it does not unduly constraints East region (analyst judgement)
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
        # Ensure float dtype to avoid FutureWarning when assigning float values
        self.footage_constraint[nam.region_footage_constraint] = self.footage_constraint[nam.region_footage_constraint].astype(float)

        #Apply footage constraint equation by region
        for region in region_list:
            self.footage_constraint.at[region, nam.region_footage_constraint] = float(
                self.footage_constraint_eq.at[('Intercept', nam.coef)]
            )

            #Apply region coefficient
            self.footage_constraint.at[region, nam.region_footage_constraint] = (
                self.footage_constraint.at[region, nam.region_footage_constraint]
                + float(self.footage_constraint_eq.at[(region, nam.coef)])
            )

            # Apply Brent coefficient
            # If brent prices are too low, then it can constrain the Eastern region way too much, which doesn't make too much
            # sense because the eastern region is mostly gas-directed drilling, and should not be so sensitive to brent price.
            # If this constraint is hit due to low brent prices, then alot of profitable projects are left on the table in the region.
            # To resolve this for now: We multiply Brent by a factor (currently 1.35) to alleviate the constraint just for the Eastern Region.
            # This is something that was previously only done for the low price case, but if prices are too low in the Reference case, needs
            # to be done in the Reference (and all other) cases as well.
            if region == 'region_1' and self.override_switches_df.loc['drilling_cons_override', 'value'] == 1:
                brent_price = self.parent.rest_brent_price.at[self.rest_curcalyr, nam.value] * self.override_switches_df.loc['drilling_cons_multiplier', 'value'] # Factor to multiply brent by so it does not unduly constraints East region (analyst judgement)
            else:
                brent_price = self.parent.rest_brent_price.at[self.rest_curcalyr, nam.value]

            ln_brent_price = np.log(brent_price)
            self.footage_constraint.at[region, nam.region_footage_constraint] = (
                self.footage_constraint.at[region, nam.region_footage_constraint]
                + (ln_brent_price * self.footage_constraint_eq.at[(nam.ln_brent_price, nam.coef)])
            )

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

        Calculates key drilling parameters for undiscovered projects:
            * well_decline_limit - Factor of total acreage/average well spacing that determines when wells experience productivity decline
            * max_wells - Maximum wells that can be drilled (total acreage/minimum well spacing)
            * max_drill_rate - Maximum annual drilling rate, calculated differently for unconventional vs conventional projects:
                - Conventional projects: Use max_annual_wells from input data
                - Unconventional projects (process codes 20-22: Tight Oil, Shale Gas, Tight Gas): 
                  Use (total_acres / acres_per_section) * undiscovered_unconv_production_factor to limit drilling intensity
            * totpat - Total patterns remaining (well_decline_limit - past_wells)
            * Drilling constraint percentages based on min_annual_wells and max_annual_pct_dev

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
        self.projects_undiscovered[nam.max_drill_rate] = self.projects_undiscovered[nam.max_drill_rate].astype(float)
        temp_unconv_df_mask = self.projects_undiscovered[nam.process_code].isin([20,21,22]).copy()
        temp_unconv_df = self.projects_undiscovered[temp_unconv_df_mask].copy()
        # Calculate max drill rate for undiscovered unconventional projects (process codes 20-22: Tight Oil, Shale Gas, Tight Gas)
        # Formula: (total_acres / acres_per_section) * production_factor = wells per year
        # The production_factor represents maximum wells that can be drilled per square mile annually
        temp_unconv_df[nam.max_drill_rate] = (temp_unconv_df[nam.total_pattern_size_acres] / self.acres_per_section * self.undiscovered_unconv_production_factor)
        temp_unconv_df = _ensure_dtype_compatibility(self.projects_undiscovered, temp_unconv_df)
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
        df_und = self.projects_undiscovered.copy()

        # Merge base_oil_prc by play
        # Create merge dataframe with play_number and base_oil_prc
        # Drop duplicates on play_number since multiple play_numbers per play_name have same base_oil_prc
        base_oil_merge = self.base_oil_prc_by_play[['play_number', 'base_oil_prc']].drop_duplicates(subset='play_number').copy()
        base_oil_merge.columns = ['play_number', 'base_oil_prc_play']
        
        # Store original index before merge to preserve alignment
        original_index = df_und.index.copy()
        
        # Merge with df_und on play_number = play, preserving left index
        df_und = df_und.merge(base_oil_merge, how='left', left_on='play', right_on='play_number')
        
        # Restore the original index to preserve alignment
        df_und.index = original_index
        
        # Fill missing values with global base_oil_prc
        df_und['base_oil_prc_play'] = df_und['base_oil_prc_play'].fillna(self.base_oil_prc)

        wdl               = df_und[nam.well_decline_limit].to_numpy()
        mxw               = df_und[nam.max_wells].to_numpy()
        max_rate          = df_und[nam.max_drill_rate].to_numpy()
        max_rate_frac     = df_und[nam.max_drill_rate_frac].to_numpy()
        crude             = df_und[nam.crude_price].to_numpy()
        gas               = df_und[nam.natgas_price].to_numpy()
        wt_num            = df_und[nam.prime_fuel_type_number].to_numpy()
        op1               = df_und['OP1'].to_numpy()
        gp1               = df_und['GP1'].to_numpy()
        max_year_pct      = df_und[nam.max_year_drill_pct].to_numpy()
        base_oil_play     = df_und['base_oil_prc_play'].to_numpy()

        n = len(df_und)
        # Use a numeric dtype to avoid propagating object dtype downstream
        out = np.empty(n, dtype='float64')

        # Local bindings for faster attribute access inside the loop
        drill_fn = drill_eq.on_next_wells
        drill_predecline = self.drill_predecline
        drill_ramp_up = self.drill_ramp_up
        base_gas = self.base_gas_prc
        cur_year = self.rest_curcalyr
        low_price = self.parent.low_price_case

        # Past drilling and last year drilling are 0.0 for undiscovered projects
        past_const = 0.0
        last_const = 0.0

        for i in range(n):
            out[i] = drill_fn(
                wdl[i],
                mxw[i],
                past_const,
                last_const,
                max_rate[i],
                max_rate_frac[i],
                drill_predecline,
                drill_ramp_up,
                crude[i],
                gas[i],
                wt_num[i],
                base_oil_play[i],
                base_gas,
                op1[i],
                gp1[i],
                max_year_pct[i],
                cur_year,
                low_price,
                undiscovered_drill_flag=True,
            )
        
        # Drop temporary column
        df_und = df_und.drop(columns=['base_oil_prc_play', 'play_number'], errors='ignore')

        # Preserve original index alignment and ensure numeric dtype
        self.undiscovered_drilling = pd.Series(out, index=df_und.index, dtype='float64')

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
        # Reset index for merge to preserve it as a column, then merge
        temp_undiscovered = pd.merge(self.discovery_order[[nam.resid, nam.item]].reset_index(),
                                     self.projects_undiscovered,
                                     how = 'left',
                                     on = nam.resid)
        # Set the index back from the preserved 'index' column
        temp_undiscovered = temp_undiscovered.set_index('index')
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
            * totpat (total patterns remaining) is calculated as well_decline_limit - past_wells
            * The project maximum drilling rates are calculated as a maximum of the following two equations:
                1. Last year drilling * 1.3
                2. Total acreage / 640 * a production factor (this factor is reset every year as an analyst judgement of between 0.08 - 0.1)
            * A hard ceiling on drilling is set at 200 wells (which can then be adjusted upward in the drilling equation based on price)
            * The number of remaining wells to be drilled is calculated as a factor of max_wells / past_wells
            * A cap on the percentage of available wells that can be drilled is set based on a ceiling of 30%, and the maximum of the following two values:
                # 1. hist_year_wells / max_wells
                2. 3% - 6% depending on project type
            * Validates consistency of drilling parameters (past_wells, totpat, well_decline_limit, pattern sizes)

        Returns
        -------
        self.projects : df
            Master DataFrame containing all projects to be processed in current model year
        """
        # Constants used in function
        UNCONVENTIONAL_PROCESS_CODES = [12, 13, 14, 20, 21, 22]
        DRILLING_GROWTH_MULTIPLIER = self.drilling_growth_multiplier
        PRODUCTION_FACTOR = self.production_factor #adjusted each year for analyst judgement between 0.08 - 0.1
        ACRES_PER_SECTION = self.acres_per_section
        MAX_DRILLING_PCT_CAP = self.max_drilling_pct_cap
        DEFAULT_DRILLING_PCT = self.default_drilling_pct
        MIN_WELL_SPACING_FLOOR = self.min_well_spacing_floor
        LATERAL_LENGTH_DIVISOR = self.lateral_length_divisor
        LATERAL_LENGTH_MULTIPLIER = self.lateral_length_multiplier

        # Defragment DataFrame to improve performance and eliminate warnings
        self.projects = self.projects.copy()

        # Drilling Assumptions
        # Set undiscovered projects last year drilling to 0
        self.projects[nam.last_year_drilling] = self.projects[nam.last_year_drilling].fillna(0.0)

        # Apply production multiplier overrides for future wells (developing and undiscovered) if enabled
        # Applies based on per-play start_year and apply_every_year configuration to projects with process_code > 7
        # Helper function to apply production multiplier to entire production curve
        # Priority: play-level > default (other_sgto_plays)
        def apply_production_multiplier(production_df, play_multipliers_dict, multiplier_default, is_oil=True):
            production_columns = [col for col in production_df.columns if isinstance(col, (int, np.integer))]
            if not production_columns:
                return
            
            # Get play numbers and process codes, filter to projects with process_code > 7
            play_series = self.projects['play'].reindex(production_df.index).astype(int).fillna(0)
            process_code_series = self.projects[nam.process_code].reindex(production_df.index)
            future_wells_mask = process_code_series > 7
            
            # Helper to check if a config is active for current year
            def is_config_active(config):
                if config['production_multiplier'] == 1.0:
                    return False
                if self.rest_curcalyr < config['start_year']:
                    return False
                if config['apply_every_year'] == 1:
                    return True
                return self.rest_curcalyr == config['start_year']
            
            # Determine final multiplier config for each project using priority: play > default
            def get_effective_config(play_num):
                # 1. Check play-level first
                play_config = play_multipliers_dict.get(int(play_num))
                if play_config and is_config_active(play_config):
                    return play_config
                
                # 2. Fall back to default
                return multiplier_default
            
            # Vectorized mapping of effective configs
            effective_configs = pd.Series(
                [get_effective_config(p) for p in play_series],
                index=production_df.index
            )
            
            production_multiplier_series = effective_configs.map(lambda x: x['production_multiplier'])
            start_year_series = effective_configs.map(lambda x: x['start_year'])
            apply_every_year_series = effective_configs.map(lambda x: x['apply_every_year'])
            
            # Check if current year meets start_year and apply_every_year conditions
            # Vectorized check: current year >= start_year AND (apply_every_year == 1 OR (apply_every_year == 0 AND current year == start_year))
            year_condition_mask = (
                (self.rest_curcalyr >= start_year_series) & 
                (
                    (apply_every_year_series == 1) | 
                    ((apply_every_year_series == 0) & (self.rest_curcalyr == start_year_series))
                )
            )
            
            # Apply only to future wells with multiplier != 1.0 that meet year conditions
            apply_mask = (production_multiplier_series != 1.0) & future_wells_mask & year_condition_mask
            if not apply_mask.any():
                return
            
            # Apply multiplier to all production columns (all years)
            result_df = production_df[production_columns].copy()
            for year_idx in production_columns:
                result_df.loc[apply_mask, year_idx] = result_df.loc[apply_mask, year_idx] * production_multiplier_series.loc[apply_mask]
            
            production_df[production_columns] = result_df
        
        # Apply oil production multiplier if enabled
        try:
            if self.production_multiplier_overrides.loc['future_wells_oil_production_multiplier_override', 'value'] == 1:
                apply_production_multiplier(
                    self.project_crude_production, 
                    self.future_wells_oil_production_multipliers, 
                    self.future_wells_oil_production_multiplier_default,
                    is_oil=True
                )
        except (KeyError, IndexError):
            pass
        
        # Apply gas production multiplier if enabled
        try:
            if self.production_multiplier_overrides.loc['future_wells_gas_production_multiplier_override', 'value'] == 1:
                apply_production_multiplier(
                    self.project_natgas_production, 
                    self.future_wells_gas_production_multipliers, 
                    self.future_wells_gas_production_multiplier_default,
                    is_oil=False
                )
        except (KeyError, IndexError):
            pass 
        # well_decline_limit is the basis for declining productivity, max_wells is the absolute maximum number of wells that can be drilled
        self.projects[nam.well_decline_limit] = (
                    self.projects[nam.total_pattern_size_acres] / self.projects[nam.std_pattern_size_acres]).round(
            decimals=0)
        self.projects[nam.max_lat_len_spacing] = self.projects[
                                                     nam.lateral_length_ft] / LATERAL_LENGTH_DIVISOR * LATERAL_LENGTH_MULTIPLIER
        self.projects[nam.min_well_spacing] = self.projects[nam.max_lat_len_spacing].clip(lower=MIN_WELL_SPACING_FLOOR)
        self.projects[nam.max_wells] = (
                    self.projects[nam.total_pattern_size_acres] / (self.projects[nam.min_well_spacing])).round(
            decimals=0)

        # For LOGS side case, limit gas projects in Region 6 (Northern Great Plains) to prevent unrealistic production spikes
        if self.parent.side_case_adj == 0.5 and nam.resource_type in self.projects.columns:
            ngp_gas_mask = (self.projects[nam.region_number] == 6) & (self.projects[nam.resource_type] == 'gas')
            self.projects.loc[ngp_gas_mask, nam.max_wells] = 0
            self.projects.loc[ngp_gas_mask, nam.well_decline_limit] = 0

        # Set Max Drill Rate
        self.projects[nam.max_drill_rate] = self.projects['max_annual_wells']
        temp_unconv_df_mask = self.projects[nam.process_code].isin(UNCONVENTIONAL_PROCESS_CODES)
        # Use .loc directly instead of creating intermediate DataFrames
        self.projects.loc[temp_unconv_df_mask, nam.max_drill_rate] = np.maximum(
            (self.projects.loc[temp_unconv_df_mask, nam.hist_year_wells] * DRILLING_GROWTH_MULTIPLIER),
            (self.projects.loc[
                 temp_unconv_df_mask, nam.total_pattern_size_acres] / ACRES_PER_SECTION * PRODUCTION_FACTOR)
        )

        # Adjust Permian production
        # Permian area plays want to continue growing

        # Apply totpat multiplier override if enabled
        try:
            if self.totpat_multiplier_overrides.loc['totpat_multiplier_override', 'value'] == 1:
                # Filter to continuous projects only
                continuous_mask = self.projects[nam.process_code].isin(UNCONVENTIONAL_PROCESS_CODES)
                
                if continuous_mask.any():
                    # Get play numbers for continuous projects
                    play_numbers = self.projects.loc[continuous_mask, 'play'].astype(int).fillna(0)
                    
                    # Apply multiplier to well_decline_limit for matching projects
                    for idx in self.projects[continuous_mask].index:
                        play_num = int(play_numbers.loc[idx])
                        multiplier_config = self.totpat_multipliers.get(play_num, self.totpat_multiplier_default)
                        
                        # Check if current year >= start_year
                        if self.rest_curcalyr >= multiplier_config['start_year']:
                            # Check apply_every_year logic
                            should_apply = False
                            if multiplier_config['apply_every_year'] == 0:
                                # Only apply in start year
                                if self.rest_curcalyr == multiplier_config['start_year']:
                                    should_apply = True
                            else:
                                # Apply every year from start year onwards
                                should_apply = True
                            
                            # Apply multiplier if conditions are met
                            if should_apply:
                                multiplier = multiplier_config['multiplier']
                                self.projects.loc[idx, nam.well_decline_limit] = (
                                    self.projects.loc[idx, nam.well_decline_limit] * multiplier
                                ).round(decimals=0)
        except (KeyError, IndexError):
            pass

        # Get Total Patterns Remaining
        # totpat is calculated from well_decline_limit and past_wells (which comes from input file)
        self.projects[nam.totpat] = self.projects[nam.well_decline_limit] - self.projects[nam.past_wells]
        
        # Validate consistency of drilling parameters
        self._validate_drilling_params(self.projects, 'set_drilling_params')

        # Get cumulative production
        self.projects[nam.cum_oil_prod] = self.project_crude_production.sum(axis=1)
        self.projects[nam.cum_gas_prod] = self.project_natgas_production.sum(axis=1)

        # Get total resources
        self.projects[nam.remaining_oil_resources] = self.projects[nam.cum_oil_prod].mul(self.projects[nam.totpat])
        self.projects[nam.remaining_gas_resources] = self.projects[nam.cum_gas_prod].mul(self.projects[nam.totpat])

        # Get Drilling EQ Constraint parameters
        self.projects[nam.min_drill_totpat_pct] = self.projects[nam.min_annual_wells] / self.projects[nam.max_wells]
        self.projects[nam.hist_well_drill_pct] = (self.projects[nam.hist_year_wells] / self.projects[nam.max_wells])
        # Avoid inf/nan where max_wells is 0 (e.g. LOGS NGP gas cap)
        zero_max_wells_mask = self.projects[nam.max_wells] == 0
        self.projects.loc[zero_max_wells_mask, nam.min_drill_totpat_pct] = 0
        self.projects.loc[zero_max_wells_mask, nam.hist_well_drill_pct] = 0

        # Calculate max_year_drill_pct (the first assignment in original code was overwritten, so we skip it)
        self.projects[nam.max_year_drill_pct] = self.projects[[nam.max_annual_pct_dev, nam.hist_well_drill_pct]].max(
            axis=1)

        # Set rules for maximum remaining % of wells that can be drilled/year and process-code-specific ratios
        self.projects.loc[
            self.projects[nam.max_year_drill_pct] > MAX_DRILLING_PCT_CAP, nam.max_year_drill_pct] = MAX_DRILLING_PCT_CAP

        # Simplify repetitive process code assignments
        for code in UNCONVENTIONAL_PROCESS_CODES:
            mask = self.projects[nam.process_code] == code
            self.projects.loc[mask, nam.max_year_drill_pct] = self.projects.loc[
                mask, [nam.max_year_drill_pct, nam.hist_well_drill_pct]].max(axis=1)

        # Replace Nans, infinities and negative infinities
        self.projects[nam.max_year_drill_pct] = self.projects[nam.max_year_drill_pct].replace([np.inf, -np.inf, np.nan],
                                                                                              DEFAULT_DRILLING_PCT)

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
        continuous_mask = (self.projects[nam.table] != 'co2_eor')

        if self.rest_curcalyr <= (self.zero_year+1): #Get history and year-1 drilling from history
            self.project_drilling = self.projects.loc[continuous_mask, nam.hist_year_wells]

        else:
            # avoid pandas apply(axis=1) overhead by iterating over NumPy arrays
            df = self.projects.loc[continuous_mask].copy()

            # Merge base_oil_prc by play
            # Create merge dataframe with play_number and base_oil_prc
            # Drop duplicates on play_number since multiple play_numbers per play_name have same base_oil_prc
            base_oil_merge = self.base_oil_prc_by_play[['play_number', 'base_oil_prc']].drop_duplicates(subset='play_number').copy()
            base_oil_merge.columns = ['play_number', 'base_oil_prc_play']
            
            # Store original index before merge to preserve alignment
            original_index = df.index.copy()
            
            # Merge with df on play_number = play
            df = df.merge(base_oil_merge, how='left', left_on='play', right_on='play_number')
            
            # Restore the original index to preserve alignment
            df.index = original_index
            
            # Fill missing values with global base_oil_prc
            df['base_oil_prc_play'] = df['base_oil_prc_play'].fillna(self.base_oil_prc)

            wdl               = df[nam.well_decline_limit].to_numpy()
            mxw               = df[nam.max_wells].to_numpy()
            past              = df[nam.past_wells].to_numpy()
            last              = df[nam.last_year_drilling].to_numpy()
            max_rate          = df[nam.max_drill_rate].to_numpy()
            max_rate_frac     = df[nam.max_drill_rate_frac].to_numpy()
            crude             = df[nam.crude_price].to_numpy()
            gas               = df[nam.natgas_price].to_numpy()
            wt_num            = df[nam.well_type_number].to_numpy()
            op1               = df['OP1'].to_numpy()
            gp1               = df['GP1'].to_numpy()
            max_year_pct      = df[nam.max_year_drill_pct].to_numpy()
            base_oil_play     = df['base_oil_prc_play'].to_numpy()

            n = len(df)
            # Use a numeric dtype to avoid propagating object dtype downstream,
            # which can cause Python-level ops (e.g., ZeroDivisionError on DataFrame.div)
            out = np.empty(n, dtype='float64')

            # Local bindings for faster attribute access inside the loop
            drill_fn = drill_eq.on_next_wells
            drill_predecline = self.drill_predecline
            drill_ramp_up = self.drill_ramp_up
            base_gas = self.base_gas_prc
            cur_year = self.rest_curcalyr
            low_price = self.parent.low_price_case

            for i in range(n):
                out[i] = drill_fn(
                    wdl[i],
                    mxw[i],
                    past[i],
                    last[i],
                    max_rate[i],
                    max_rate_frac[i],
                    drill_predecline,
                    drill_ramp_up,
                    crude[i],
                    gas[i],
                    wt_num[i],
                    base_oil_play[i],
                    base_gas,
                    op1[i],
                    gp1[i],
                    max_year_pct[i],
                    cur_year,
                    low_price,
                    undiscovered_drill_flag=False,
                )
            
            # Drop temporary column (don't update self.projects with it)
            df = df.drop(columns=['base_oil_prc_play', 'play_number'], errors='ignore')

            # Preserve original index alignment and ensure numeric dtype
            self.project_drilling = pd.Series(out, index=df.index, dtype='float64')


        ###Produce drilling for fixed projects (i.e. EOR)
        fixed_mask = (self.projects[nam.table] == 'co2_eor')
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
            # explicitly cast all cols to float64 first
            # Cast to float64 for consistent calculations
            for col in temp_gas_df.columns:
                if temp_gas_df[col].dtype != 'float64':
                    temp_gas_df[col] = temp_gas_df[col].astype(np.float64)

            for col in temp_conv_gas_df.columns:
                if temp_conv_gas_df[col].dtype != 'float64':
                    temp_conv_gas_df[col] = temp_conv_gas_df[col].astype(np.float64)
            temp_gas_df.update(temp_conv_gas_df)
            
            for col in temp_shale_cbm_gas_df.columns:
                if  temp_shale_cbm_gas_df[col].dtype != 'float64':
                    temp_shale_cbm_gas_df[col] = temp_shale_cbm_gas_df[col].astype(np.float64)
            temp_gas_df.update(temp_shale_cbm_gas_df)

            gas_lease_costs[year] = gas_lease_costs[year] + (temp_gas_df[nam.cum_gas_prod] * 7.62 * 0.178 * temp_gas_df[nam.term_mult] * temp_gas_df[nam.term] \
                                    * (temp_gas_df[nam.projected_und_wells] / temp_gas_df[nam.temp_total_patterns]))

        #Assign GG costs
        self.gg_costs.update(oil_lease_costs)
        self.gg_costs.update(gas_lease_costs)

        #Multiply GG costs by 2 since equations are dated to 2014 and costs have grown
        self.gg_costs = self.gg_costs.mul(2, axis = 1)

        pass


    def _calculate_gpm_capex_values(self, ngpl_df, capacity_col):
        """Calculate GPM capex values for all breakpoints using log formulas.
        
        Parameters
        ----------
        ngpl_df : pd.DataFrame
            DataFrame containing capacity values
        capacity_col : str
            Column name for capacity values
            
        Returns
        -------
        dict
            Dictionary mapping GPM breakpoint names to calculated values
        """
        # GPM breakpoints and their corresponding log formula coefficients and offsets
        gpm_capex_formulas = {
            'gpm_1.5': (0.437900, 0.7929),
            'gpm_3': (0.481300, 0.6366),
            'gpm_6': (0.5612, 0.5628),
            'gpm_12': (0.6724, 0.5536),
            'gpm_24': (0.8139, 0.6011),
            'gpm_48': (0.9868, 0.7031),
            'gpm_96': (1.194, 0.8611),
            'gpm_192': (1.4396, 1.0789)
        }
        
        gpm_values = {}
        for gpm_name, (coef, offset) in gpm_capex_formulas.items():
            gpm_values[gpm_name] = coef * np.log(ngpl_df[capacity_col] - offset)
        
        return gpm_values
    
    def _calculate_gpm_opex_values(self, ngpl_df, capacity_col):
        """Calculate GPM opex values for all breakpoints using power formulas.
        
        Parameters
        ----------
        ngpl_df : pd.DataFrame
            DataFrame containing capacity values
        capacity_col : str
            Column name for capacity values
            
        Returns
        -------
        dict
            Dictionary mapping GPM breakpoint names to calculated values
        """
        # GPM breakpoints and their corresponding power formula coefficients and exponents
        gpm_opex_formulas = {
            'gpm_1.5': (0.1293, -0.3159),
            'gpm_3': (0.1481, -0.3282),
            'gpm_6': (0.162, -0.3241),
            'gpm_12': (0.1759, -0.3207),
            'gpm_24': (0.1898, -0.3179),
            'gpm_48': (0.2038, -0.3155),
            'gpm_96': (0.2177, -0.3134),
            'gpm_192': (0.3667, -0.5809)
        }
        
        gpm_values = {}
        for gpm_name, (coef, exp) in gpm_opex_formulas.items():
            gpm_values[gpm_name] = coef * ngpl_df[capacity_col] ** exp
        
        return gpm_values
    
    def _interpolate_gpm_costs(self, ngpl_df, gpm_val_col, gpm_breakpoints, gpm_value_cols, output_col):
        """Interpolate GPM costs between breakpoints using linear interpolation.
        
        Parameters
        ----------
        ngpl_df : pd.DataFrame
            DataFrame containing GPM values and calculated breakpoint values
        gpm_val_col : str
            Column name for actual GPM values to interpolate
        gpm_breakpoints : list
            List of GPM breakpoint values in ascending order
        gpm_value_cols : dict
            Dictionary mapping breakpoint values to column names containing calculated values
        output_col : str
            Column name to store interpolated results
        """
        # Handle values at or below first breakpoint
        first_breakpoint = gpm_breakpoints[0]
        mask = ngpl_df[gpm_val_col] <= first_breakpoint
        if mask.any():
            temp_df = ngpl_df[mask].copy()
            temp_df[nam.weight_b] = temp_df[gpm_val_col] / first_breakpoint
            temp_df[nam.weight_a] = 1 - temp_df[nam.weight_b]
            temp_df[output_col] = temp_df[nam.weight_a] * 0 + temp_df[nam.weight_b] * temp_df[gpm_value_cols[first_breakpoint]]
            ngpl_df.update(temp_df)
        
        # Handle values between breakpoints
        for i in range(len(gpm_breakpoints) - 1):
            lower_breakpoint = gpm_breakpoints[i]
            upper_breakpoint = gpm_breakpoints[i + 1]
            mask = (ngpl_df[gpm_val_col] > lower_breakpoint) & (ngpl_df[gpm_val_col] <= upper_breakpoint)
            if mask.any():
                temp_df = ngpl_df[mask].copy()
                temp_df[nam.weight_b] = (temp_df[gpm_val_col] - lower_breakpoint) / (upper_breakpoint - lower_breakpoint)
                temp_df[nam.weight_a] = 1 - temp_df[nam.weight_b]
                temp_df[output_col] = (temp_df[nam.weight_a] * temp_df[gpm_value_cols[lower_breakpoint]] + 
                                      temp_df[nam.weight_b] * temp_df[gpm_value_cols[upper_breakpoint]])
                ngpl_df.update(temp_df)
        
        # Handle values above last breakpoint
        last_breakpoint = gpm_breakpoints[-1]
        mask = ngpl_df[gpm_val_col] > last_breakpoint
        if mask.any():
            temp_df = ngpl_df[mask].copy()
            temp_df[output_col] = temp_df[gpm_value_cols[last_breakpoint]]
            ngpl_df.update(temp_df)

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

        #Create cost type columns (ensure float dtype to avoid FutureWarning)
        ngpl_df[nam.str_plant_capex]    = 0.0
        ngpl_df[nam.str_plant_opex]     = 0.0
        ngpl_df[nam.cry_plant_capex]    = 0.0
        ngpl_df[nam.cry_plant_opex]     = 0.0

        ###Straight Refrigeration Plant: Capital Costs
        #Calculate gallons per mcf (GPM) Value
        ngpl_df[nam.gpm_val] = ngpl_df[nam.thru] * ngpl_df[nam.ngpl] * 42.0 / 24.0 / 60.0

        #Calculate GPM capex values for all breakpoints
        gpm_capex_values = self._calculate_gpm_capex_values(ngpl_df, nam.capacity_st)
        for gpm_name, gpm_series in gpm_capex_values.items():
            ngpl_df[gpm_name] = gpm_series

        #Interpolate costs based on the GPM value
        gpm_breakpoints = [1.5, 3.0, 6.0, 12.0, 24.0, 48.0, 96.0, 192.0]
        gpm_value_cols = {
            1.5: 'gpm_1.5',
            3.0: 'gpm_3',
            6.0: 'gpm_6',
            12.0: 'gpm_12',
            24.0: 'gpm_24',
            48.0: 'gpm_48',
            96.0: 'gpm_96',
            192.0: 'gpm_192'
        }
        self._interpolate_gpm_costs(ngpl_df, nam.gpm_val, gpm_breakpoints, gpm_value_cols, nam.str_plant_capex)

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
        #Recalculate GPM Value (ensure it's up to date)
        ngpl_df[nam.gpm_val] = ngpl_df[nam.thru] * ngpl_df[nam.ngpl] * 42.0 / 24.0 / 60.0

        #Calculate GPM opex values for all breakpoints
        gpm_opex_values = self._calculate_gpm_opex_values(ngpl_df, nam.capacity_st)
        for gpm_name, gpm_series in gpm_opex_values.items():
            ngpl_df[gpm_name] = gpm_series

        #Interpolate costs based on the GPM Value
        # Reuse the same gpm_value_cols mapping for opex
        self._interpolate_gpm_costs(ngpl_df, nam.gpm_val, gpm_breakpoints, gpm_value_cols, nam.str_plant_opex)

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
        # Ensure columns exist and are float type to avoid FutureWarning
        for col in ngpl_df.columns:
            if col not in self.projects.columns:
                self.projects[col] = 0.0
            elif self.projects[col].dtype == 'int64':
                self.projects[col] = self.projects[col].astype(float)
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
            * Apply Natural CO2 price as CO2 price

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
        bench_oil_1 = self.parent.reg_crude_price.copy()
        recy_co2_price_df[nam.bench_1] = bench_oil_1[self.rest_curcalyr]
        bench_oil_2 = (self.parent.rest_rfqtdcrd.copy().mul(self.parent.reg_crude_price.copy(), axis='index')).div(self.parent.rest_rfqtdcrd.copy()).fillna(0)
        recy_co2_price_df[nam.bench_2] = bench_oil_2[[(self.rest_curcalyr - 1)]]

        #Calculate CO2 costs
        recy_co2_price_df[nam.bench_oil] = recy_co2_price_df[[nam.bench_1, nam.bench_2]].values.min(1)
        recy_co2_price_df[nam.co2_price] = self.eor_other_costs.at['natural_co2', 'a'] + recy_co2_price_df[nam.bench_oil].mul(self.eor_other_costs.at['natural_co2', 'b'])

        #Clean temp df and format df for merge to master CO2 price df
        recy_co2_price_df[nam.co2_type] = 4
        recy_co2_price_df[nam.bin] = 1
        #Drop the existing region_number column (has NaN values) and reset index to convert index to column
        recy_co2_price_df = recy_co2_price_df.drop(columns=[nam.region_number], errors='ignore')
        recy_co2_price_df = recy_co2_price_df.reset_index()
        recy_co2_price_df = recy_co2_price_df.rename(columns={'index': nam.region_number})

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
        # Drop duplicates on index to avoid duplicate index labels error
        # temp_df = temp_df[~temp_df.index.duplicated(keep='first')]
        # Ensure columns exist and are float type to avoid FutureWarning
        for col in temp_df.columns:
            if col not in self.projects.columns:
                self.projects[col] = 0.0
            elif self.projects[col].dtype == 'int64':
                self.projects[col] = self.projects[col].astype(float)
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
        
        # Set conversion factors and economic parameters from onshore configuration
        self.cash_flow.boe_conversion = self.boe_conversion
        self.cash_flow.barrels_per_gallon = self.barrels_per_gallon
        self.cash_flow.ch4_to_metric_tons = self.ch4_to_metric_tons
        self.cash_flow.ngpl_volume_divisor = self.ngpl_volume_divisor
        self.cash_flow.min_years_before_abandon = self.min_years_before_abandon
        self.cash_flow.econ_life_default = self.econ_life_default

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
        self.cash_flow.properties[nam.sga_opex_well] = self.projects[nam.sga_opex_well] * self.sga_conservative_factor #Set to conservative factor

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
        mask = self.cash_flow.properties[nam.process_code] == 10
        self.cash_flow.properties.loc[mask, nam.discount_rate] += 0.02

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
        past_drilling_mask  = (self.projects[nam.last_year_drilling] >= self.past_drilling_threshold) | ((self.projects[nam.hist_year_wells] >= self.past_drilling_threshold) & (self.rest_curcalyr <= self.parent.steo_years[-1]))
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
        """Determine which EOR/ASR projects are eligible to be run in each iteration based on economic life of legacy projects and profitability.

            * CO2 EOR can only run within 10 years of legacy project abandonment, while other EOR projects can only run within 6 years
            * Once we have a list of eligible projects based on economic life aggregate the most profitable, eligible, selected projects
            * Set all other EOR/ASR projects tied to the same producing project (excluding infill) ineligible
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

        #Create temp df with only most profitable eor projects
        most_prof_temp_df = self.projects[most_prof_mask].copy()
        most_prof_temp_df[nam.eligible] = 0

        #Set all lower ranked projects as unprofitable
        less_prof_temp_df = all_eor_temp_df.loc[~(most_prof_mask)].copy()
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

        #Ensure 'nam.rigs' is numeric before cumsum
        temp_well_reg[nam.rigs] = temp_well_reg[nam.rigs].astype(float)
        #Get cumulative sum of rigs required
        temp_well_reg[nam.region_cum_rig_count] = temp_well_reg.groupby(nam.region_number)[nam.rigs].cumsum()


        #Combine with rigs required for already producing wells
        temp_well_reg = temp_well_reg.reset_index().merge(self.producing_wells[[self.rest_curcalyr]], how = 'left', left_on = nam.region_number, right_index = True).set_index('index')
        temp_well_reg[nam.producing_rigs] = temp_well_reg[self.rest_curcalyr] * (1 / temp_well_reg[nam.wells_per_rig])
        temp_well_reg[nam.region_cum_rig_count] = temp_well_reg[nam.region_cum_rig_count] + temp_well_reg[nam.producing_rigs]

        #Solve for regional rig constraint and apply to self.projects df
        temp_well_reg = temp_well_reg.reset_index().merge(self.rig_constraint, how = 'left', on = [nam.region_number]).set_index('index')
        temp_well_reg = _ensure_dtype_compatibility(self.projects, temp_well_reg)
        temp_well_reg = temp_well_reg.astype(
            {col: self.projects[col].dtype for col in temp_well_reg.columns if col in self.projects.columns}
        )
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
        temp_footage_reg[nam.footage] = temp_footage_reg[nam.footage].astype(float)
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


    def _apply_steo_benchmark(self, selected_mask, temp_crude, temp_natgas):
        """Apply STEO benchmark override logic after production is computed.
        
        This function adjusts project_drilling values for selected projects to meet 
        STEO production targets for tight oil and shale gas plays. It calculates
        adjustment factors per play and applies them proportionally to project_drilling
        values, then recomputes production.
        
        Parameters
        ----------
        selected_mask : pd.Series
            Boolean mask of selected projects (not modified)
        temp_crude : pd.DataFrame
            Computed crude production dataframe with year columns shifted by rest_curcalyr
        temp_natgas : pd.DataFrame
            Computed natgas production dataframe with year columns shifted by rest_curcalyr
            
        Returns
        -------
        adjusted_crude_production : pd.DataFrame
            Updated crude production dataframe for adjusted plays
        adjusted_natgas_production : pd.DataFrame
            Updated natgas production dataframe for adjusted plays
        adjusted_plays : set
            Set of play numbers that were adjusted
        """
        # Apply benchmark override only in first STEO year
        if self.rest_curcalyr != self.parent.steo_years[0]:
            return pd.DataFrame(), pd.DataFrame(), set()
        
        steo_year = self.parent.steo_years[0]
        adjusted_plays = set()
        adjusted_crude_production = pd.DataFrame()
        adjusted_natgas_production = pd.DataFrame()
        
        # Determine if this is a side case (HOGS/LOGS) - use new methodology to prevent double-adjustment
        is_side_case = self.parent.scedes.get('OGTECH') in ['23', '30']
        
        # Ensure selected_mask is a Series with proper index
        if not isinstance(selected_mask, pd.Series):
            selected_mask = pd.Series(selected_mask, index=self.projects.index)
        elif selected_mask.index is not self.projects.index:
            selected_mask = pd.Series(selected_mask.values, index=self.projects.index)
        
        # Helper function to recompute production for a specific play
        def _recompute_production_for_play(play_numbers, play_mask, is_crude=True):
            """Recompute production for a specific play using current project_drilling values."""
            selected_play_mask = selected_mask & play_mask
            if not selected_play_mask.any():
                return pd.DataFrame()
            
            # Filter to only projects that exist in cash_flow and project_drilling
            valid_mask = (selected_play_mask & 
                         (selected_play_mask.index.isin(self.cash_flow.crude_production.index if is_crude else self.cash_flow.natgas_production.index)) &
                         (selected_play_mask.index.isin(self.project_drilling.index)))
            
            if not valid_mask.any():
                return pd.DataFrame()
            
            if is_crude:
                temp = self.cash_flow.crude_production.loc[valid_mask].mul(
                    self.project_drilling[valid_mask], axis=0)
            else:
                temp = self.cash_flow.natgas_production.loc[valid_mask].mul(
                    self.project_drilling[valid_mask], axis=0)
            
            temp.columns = temp.columns + self.rest_curcalyr
            labels = [i for i in temp.columns if i > self.parent.final_aeo_year]
            temp = temp.drop(labels, axis=1)
            
            # Add metadata columns
            temp[nam.hsm_index] = self.projects.loc[valid_mask, nam.hsm_index]
            temp[nam.process_code] = self.projects.loc[valid_mask, nam.process_code]
            temp[nam.resid] = self.projects.loc[valid_mask, nam.resid]
            temp[nam.district_number] = self.projects.loc[valid_mask, nam.district_number]
            temp[nam.region_number] = self.projects.loc[valid_mask, nam.region_number]
            temp[nam.federal_land] = self.projects.loc[valid_mask, nam.federal_land]
            temp[nam.well_type_number] = self.projects.loc[valid_mask, nam.well_type_number]
            temp[nam.oil_type_number] = self.projects.loc[valid_mask, nam.oil_type_number]
            temp[nam.gas_type_number] = self.projects.loc[valid_mask, nam.gas_type_number]
            temp[nam.lfmm_crude_type] = self.projects.loc[valid_mask, nam.lfmm_crude_type]
            temp[nam.api] = self.projects.loc[valid_mask, nam.api]
            temp[nam.avg_api] = self.projects.loc[valid_mask, nam.avg_api]
            temp[nam.play] = self.projects.loc[valid_mask, nam.play]
            temp[nam.project_royalty_multiplier] = self.projects.loc[valid_mask, nam.project_royalty_multiplier]
            temp[nam.year_production_start] = self.rest_curcalyr
            
            # Add SGTO play name for both crude and natgas production
            def get_sgto_play_name(row):
                play_number = row[nam.play]
                well_type = row[nam.well_type_number]
                
                # Handle NaN or None values
                if pd.isna(play_number) or pd.isna(well_type):
                    # If well_type is valid SGTO type, still label as "other" even if play_number is missing
                    if not pd.isna(well_type) and well_type in [2, 5]:
                        return "other"
                    return ""
                
                # Convert play_number to int for dictionary lookup (handles float values)
                try:
                    play_number_int = int(play_number)
                except (ValueError, TypeError):
                    play_number_int = None
                
                # Use appropriate mapping based on production type
                if is_crude:
                    # Check if play_number is in tight_oil_play_map
                    if play_number_int is not None and play_number_int in self.tight_oil_play_number_to_name:
                        return self.tight_oil_play_number_to_name[play_number_int]
                else:
                    # Check if play_number is in shale_gas_play_map
                    if play_number_int is not None and play_number_int in self.shale_gas_play_number_to_name:
                        return self.shale_gas_play_number_to_name[play_number_int]
                
                # If not in map but is SGTO (well_type 2 = Tight Oil, 5 = Shale Gas), label as "other"
                if well_type in [2, 5]:
                    return "other"
                # Otherwise, not an SGTO play
                else:
                    return ""
            
            temp['sgto_play_name'] = temp.apply(get_sgto_play_name, axis=1)
            
            return temp
        
        # Process tight oil benchmarks
        for _, row in self.steo_tight_oil_benchmarks.iterrows():
            play_name = row.get('play', '')
            if pd.isna(play_name) or play_name == '' or play_name == 'play':
                continue
            
            # Get target production for steo_years[0]
            year_col = steo_year
            if year_col not in row.index:
                year_col = str(steo_year)
            if year_col not in row.index:
                continue
            
            target_prod_mmbbl_d = row[year_col]
            if pd.isna(target_prod_mmbbl_d) or target_prod_mmbbl_d == 0:
                continue
            
            # Convert MMBBL/D to barrels per year
            target_prod_bbl_yr = float(target_prod_mmbbl_d) * 1000000 * 365
            
            # Handle "Other" play specially
            if play_name == "Other":
                # For "Other": filter well_type_number in [2, 5] and exclude plays in tight_oil_play_map
                well_type_mask = self.projects[nam.well_type_number].isin([2, 5])
                excluded_play_numbers = set(self.tight_oil_play_map['play_number'].unique())
                play_exclusion_mask = ~self.projects[nam.play].isin(excluded_play_numbers)
                play_mask = well_type_mask & play_exclusion_mask
                selected_play_mask = selected_mask & play_mask
                # For "Other", we'll use the actual play numbers of the selected projects for tracking
                if selected_play_mask.any():
                    play_numbers = self.projects.loc[selected_play_mask, nam.play].unique().tolist()
                else:
                    play_numbers = []
            else:
                # Get all play numbers for this play name
                play_numbers = self.tight_oil_play_name_to_numbers.get(play_name, [])
                if not play_numbers:
                    continue
                
                play_mask = self.projects[nam.play].isin(play_numbers)
                selected_play_mask = selected_mask & play_mask
            
            # Skip if no selected projects for this play
            if not selected_play_mask.any():
                continue
            
            # Filter to only projects that exist in cash_flow and project_drilling
            valid_mask = (selected_play_mask & 
                         (selected_play_mask.index.isin(self.cash_flow.crude_production.index)) &
                         (selected_play_mask.index.isin(self.project_drilling.index)))
            
            # For side cases, only adjust Tight Oil projects (well_type=2) to prevent double-adjustment
            if is_side_case:
                valid_mask = valid_mask & (self.projects[nam.well_type_number] == 2)
            
            if not valid_mask.any():
                continue
            
            # Calculate existing production from legacy wells (self.crude_production)
            existing_prod_from_self = 0.0
            if not self.crude_production.empty and nam.play in self.crude_production.columns and \
               steo_year in self.crude_production.columns:
                if play_name == "Other":
                    # For "Other": filter by play numbers AND well_type_number in [2, 5]
                    existing_mask = (self.crude_production[nam.play].isin(play_numbers) &
                                   self.crude_production[nam.well_type_number].isin([2, 5]))
                else:
                    existing_mask = self.crude_production[nam.play].isin(play_numbers)
                if existing_mask.any():
                    existing_prod_from_self = self.crude_production.loc[existing_mask, steo_year].sum()
            
            # Calculate temp production for this play (from new wells)
            temp_prod_from_play = 0.0
            if not temp_crude.empty and nam.play in temp_crude.columns and steo_year in temp_crude.columns:
                if play_name == "Other":
                    # For "Other": filter by play numbers AND well_type_number in [2, 5]
                    play_mask_temp = (temp_crude[nam.play].isin(play_numbers) &
                                    temp_crude[nam.well_type_number].isin([2, 5]))
                else:
                    play_mask_temp = temp_crude[nam.play].isin(play_numbers)
                if play_mask_temp.any():
                    temp_prod_from_play = temp_crude.loc[play_mask_temp, steo_year].sum()
            
            # Calculate total current production
            current_total_prod = existing_prod_from_self + temp_prod_from_play
            
            # Calculate difference between target and current
            production_diff = target_prod_bbl_yr - current_total_prod
            
            # Check if adjustment is needed (use small tolerance to avoid unnecessary adjustments)
            if abs(production_diff) < 1.0:  # Less than 1 barrel difference, skip
                continue
            
            # Calculate adjustment factor: difference / temp production
            # If temp production is zero or very small, cannot adjust
            if temp_prod_from_play <= 0:
                continue
            
            factor = production_diff / temp_prod_from_play
            
            # Get current project_drilling values for this play
            current_drilling = self.project_drilling[valid_mask].copy()
            current_total_drilling = current_drilling.sum()
            
            if current_total_drilling <= 0:
                continue
            
            # Calculate new total drilling needed: current_total * (1 + factor)
            new_total_drilling = current_total_drilling * (1 + factor)
            
            # Ensure new total is at least 0
            if new_total_drilling < 0:
                new_total_drilling = 0
            
            # Distribute proportionally
            if current_total_drilling > 0:
                new_drilling = current_drilling * (new_total_drilling / current_total_drilling)
            else:
                new_drilling = current_drilling.copy()
            
            # Round up to ensure whole numbers
            new_drilling = np.ceil(new_drilling)
            
            # Update project_drilling for this play
            self.project_drilling.loc[valid_mask] = new_drilling
            
            # Mark play as adjusted
            adjusted_plays.update(play_numbers)
            
            # Recompute production for this play with adjusted project_drilling
            temp_play = _recompute_production_for_play(play_numbers, play_mask, is_crude=True)
            if not temp_play.empty:
                if adjusted_crude_production.empty:
                    adjusted_crude_production = temp_play
                else:
                    adjusted_crude_production = pd.concat([adjusted_crude_production, temp_play], ignore_index=True)
        
        # Process shale gas benchmarks
        for _, row in self.steo_shale_gas_benchmarks.iterrows():
            play_name = row.get('play', '')
            if pd.isna(play_name) or play_name == '' or play_name == 'play':
                continue
            
            # Get target production for steo_years[0]
            year_col = steo_year
            if year_col not in row.index:
                year_col = str(steo_year)
            if year_col not in row.index:
                continue
            
            target_prod_tcf = row[year_col]
            if pd.isna(target_prod_tcf) or target_prod_tcf == 0:
                continue
            
            # Convert TCF to CF per year
            target_prod_cf_yr = float(target_prod_tcf) * 1_000_000_000
            
            # Handle "Other" play specially
            if play_name == "Other":
                # For "Other": filter well_type_number in [2, 5] and exclude plays in shale_gas_play_map
                well_type_mask = self.projects[nam.well_type_number].isin([2, 5])
                excluded_play_numbers = set(self.shale_gas_play_map['play_number'].unique())
                play_exclusion_mask = ~self.projects[nam.play].isin(excluded_play_numbers)
                play_mask = well_type_mask & play_exclusion_mask
                selected_play_mask = selected_mask & play_mask
                # For "Other", we'll use the actual play numbers of the selected projects for tracking
                if selected_play_mask.any():
                    play_numbers = self.projects.loc[selected_play_mask, nam.play].unique().tolist()
                else:
                    play_numbers = []
            else:
                # Get all play numbers for this play name
                play_numbers = self.shale_gas_play_name_to_numbers.get(play_name, [])
                if not play_numbers:
                    continue
                
                play_mask = self.projects[nam.play].isin(play_numbers)
                selected_play_mask = selected_mask & play_mask
            
            # Skip if no selected projects for this play
            if not selected_play_mask.any():
                continue
            
            # Filter to only projects that exist in cash_flow and project_drilling
            valid_mask = (selected_play_mask & 
                         (selected_play_mask.index.isin(self.cash_flow.natgas_production.index)) &
                         (selected_play_mask.index.isin(self.project_drilling.index)))
            
            # For side cases, only adjust Shale Gas projects (well_type=5) to prevent double-adjustment
            if is_side_case:
                valid_mask = valid_mask & (self.projects[nam.well_type_number] == 5)
            
            if not valid_mask.any():
                continue
            
            # Calculate existing production from legacy wells (self.natgas_production)
            existing_prod_from_self = 0.0
            if not self.natgas_production.empty and nam.play in self.natgas_production.columns and \
               steo_year in self.natgas_production.columns:
                if play_name == "Other":
                    # For "Other": filter by play numbers AND well_type_number in [2, 5]
                    existing_mask = (self.natgas_production[nam.play].isin(play_numbers) &
                                   self.natgas_production[nam.well_type_number].isin([2, 5]))
                else:
                    existing_mask = self.natgas_production[nam.play].isin(play_numbers)
                if existing_mask.any():
                    existing_prod_from_self = self.natgas_production.loc[existing_mask, steo_year].sum()
            
            # Calculate temp production for this play (from new wells)
            temp_prod_from_play = 0.0
            if not temp_natgas.empty and nam.play in temp_natgas.columns and steo_year in temp_natgas.columns:
                if play_name == "Other":
                    # For "Other": filter by play numbers AND well_type_number in [2, 5]
                    play_mask_temp = (temp_natgas[nam.play].isin(play_numbers) &
                                    temp_natgas[nam.well_type_number].isin([2, 5]))
                else:
                    play_mask_temp = temp_natgas[nam.play].isin(play_numbers)
                if play_mask_temp.any():
                    temp_prod_from_play = temp_natgas.loc[play_mask_temp, steo_year].sum()
            
            # Calculate total current production
            current_total_prod = existing_prod_from_self + temp_prod_from_play
            
            # Calculate difference between target and current
            production_diff = target_prod_cf_yr - current_total_prod
            
            # Check if adjustment is needed (use small tolerance to avoid unnecessary adjustments)
            if abs(production_diff) < 1.0:  # Less than 1 CF difference, skip
                continue
            
            # Calculate adjustment factor: difference / temp production
            # If temp production is zero or very small, cannot adjust
            if temp_prod_from_play <= 0:
                continue
            
            factor = production_diff / temp_prod_from_play
            
            # Get current project_drilling values for this play
            current_drilling = self.project_drilling[valid_mask].copy()
            current_total_drilling = current_drilling.sum()
            # Ensure scalar (project_drilling may be DataFrame -> .sum() can return Series)
            current_total_drilling = float(np.asarray(current_total_drilling).ravel()[0])
            
            if current_total_drilling <= 0:
                continue
            
            # Calculate new total drilling needed: current_total * (1 + factor)
            new_total_drilling = float(current_total_drilling * (1 + factor))
            
            # Ensure new total is at least 0
            if new_total_drilling < 0:
                new_total_drilling = 0
            
            # Distribute proportionally
            if current_total_drilling > 0:
                new_drilling = current_drilling * (new_total_drilling / current_total_drilling)
            else:
                new_drilling = current_drilling.copy()
            
            # Round up to ensure whole numbers
            new_drilling = np.ceil(new_drilling)
            
            # Update project_drilling for this play
            self.project_drilling.loc[valid_mask] = new_drilling
            
            # Mark play as adjusted
            adjusted_plays.update(play_numbers)
            
            # Recompute production for this play with adjusted project_drilling
            temp_play = _recompute_production_for_play(play_numbers, play_mask, is_crude=False)
            if not temp_play.empty:
                if adjusted_natgas_production.empty:
                    adjusted_natgas_production = temp_play
                else:
                    adjusted_natgas_production = pd.concat([adjusted_natgas_production, temp_play], ignore_index=True)
        
        return adjusted_crude_production, adjusted_natgas_production, adjusted_plays

    def _add_metadata_columns(self, df, selected_mask):
        """Add common metadata columns to a production dataframe.
        
        Parameters
        ----------
        df : pd.DataFrame
            DataFrame to add columns to
        selected_mask : pd.Series
            Boolean mask of selected projects
            
        Returns
        -------
        pd.DataFrame
            DataFrame with metadata columns added
        """
        df[nam.hsm_index] = self.projects.loc[selected_mask, nam.hsm_index]
        df[nam.process_code] = self.projects.loc[selected_mask, nam.process_code]
        df[nam.resid] = self.projects.loc[selected_mask, nam.resid]
        df[nam.district_number] = self.projects.loc[selected_mask, nam.district_number]
        df[nam.region_number] = self.projects.loc[selected_mask, nam.region_number]
        df[nam.federal_land] = self.projects.loc[selected_mask, nam.federal_land]
        df[nam.well_type_number] = self.projects.loc[selected_mask, nam.well_type_number]
        df[nam.oil_type_number] = self.projects.loc[selected_mask, nam.oil_type_number]
        df[nam.gas_type_number] = self.projects.loc[selected_mask, nam.gas_type_number]
        df[nam.lfmm_crude_type] = self.projects.loc[selected_mask, nam.lfmm_crude_type]
        df[nam.api] = self.projects.loc[selected_mask, nam.api]
        df[nam.avg_api] = self.projects.loc[selected_mask, nam.avg_api]
        df[nam.play] = self.projects.loc[selected_mask, nam.play]
        df[nam.project_royalty_multiplier] = self.projects.loc[selected_mask, nam.project_royalty_multiplier]
        df[nam.year_production_start] = self.rest_curcalyr
        return df

    def _add_sgto_play_name(self, df, is_oil=True):
        """Add SGTO play name column to production dataframe.
        
        Parameters
        ----------
        df : pd.DataFrame
            DataFrame to add SGTO play name to
        is_oil : bool
            True for crude oil, False for natural gas
            
        Returns
        -------
        pd.DataFrame
            DataFrame with sgto_play_name column added
        """
        def get_sgto_play_name(row):
            play_number = row[nam.play]
            well_type = row[nam.well_type_number]
            
            # Handle NaN or None values
            if pd.isna(play_number) or pd.isna(well_type):
                # If well_type is valid SGTO type, still label as "other" even if play_number is missing
                if not pd.isna(well_type) and well_type in [2, 5]:
                    return "other"
                return ""
            
            # Convert play_number to int for dictionary lookup (handles float values)
            try:
                play_number_int = int(play_number)
            except (ValueError, TypeError):
                play_number_int = None
            
            # Use appropriate mapping based on production type
            if is_oil:
                # Check if play_number is in tight_oil_play_map
                if play_number_int is not None and play_number_int in self.tight_oil_play_number_to_name:
                    return self.tight_oil_play_number_to_name[play_number_int]
            else:
                # Check if play_number is in shale_gas_play_map
                if play_number_int is not None and play_number_int in self.shale_gas_play_number_to_name:
                    return self.shale_gas_play_number_to_name[play_number_int]
            
            # If not in map but is SGTO (well_type 2 = Tight Oil, 5 = Shale Gas), label as "other"
            if well_type in [2, 5]:
                return "other"
            # Otherwise, not an SGTO play
            else:
                return ""
        
        df['sgto_play_name'] = df.apply(get_sgto_play_name, axis=1)
        return df

    def _add_process_code_descriptions(self, df):
        """Add process code description columns to dataframe.
        
        Parameters
        ----------
        df : pd.DataFrame
            DataFrame to add descriptions to
            
        Returns
        -------
        pd.DataFrame
            DataFrame with process code descriptions added
        """
        if df.empty or nam.process_code not in df.columns:
            return df
        
        process_code_cols = ['well_type', 'oil_type', 'gas_type', 'dev_type', 'resource_type', 'prime_fuel_type']
        available_cols = [col for col in process_code_cols if col in self.process_codes.columns]
        if available_cols:
            # Convert process_code to int for proper merge (process_codes index is int, but data may be float)
            df = df.reset_index(drop=True)
            process_code_int = pd.to_numeric(df[nam.process_code], errors='coerce').astype('Int64')
            # Drop existing description columns if they exist (to avoid duplicates)
            cols_to_drop = [col for col in available_cols if col in df.columns]
            if cols_to_drop:
                df = df.drop(columns=cols_to_drop)
            # Merge with process_codes
            df = df.merge(
                self.process_codes[available_cols], 
                left_on=process_code_int, 
                right_index=True, 
                how='left'
            )
        return df

    def _add_county_name(self, df):
        """Add county name column to dataframe.
        
        Parameters
        ----------
        df : pd.DataFrame
            DataFrame to add county name to
            
        Returns
        -------
        pd.DataFrame
            DataFrame with county name added
        """
        if df.empty or nam.resid not in df.columns:
            return df
        
        county_name_col = 'cnty_name'
        # Try to get county name from self.projects first
        if county_name_col in self.projects.columns:
            county_map = self.projects[[nam.resid, county_name_col]].drop_duplicates(subset=nam.resid)
            df = df.merge(county_map, on=nam.resid, how='left')
        else:
            # Try source dataframes
            county_sources = []
            if hasattr(self, 'projects_continuous') and county_name_col in self.projects_continuous.columns:
                county_sources.append(self.projects_continuous[[nam.resid, county_name_col]])
            if hasattr(self, 'projects_producing_oil') and county_name_col in self.projects_producing_oil.columns:
                county_sources.append(self.projects_producing_oil[[nam.resid, county_name_col]])
            if hasattr(self, 'projects_producing_gas') and county_name_col in self.projects_producing_gas.columns:
                county_sources.append(self.projects_producing_gas[[nam.resid, county_name_col]])
            if county_sources:
                county_map = pd.concat(county_sources, ignore_index=True).drop_duplicates(subset=nam.resid)
                df = df.merge(county_map, on=nam.resid, how='left')
        return df

    def _create_production_dataframe(self, selected_mask, production_type='crude'):
        """Create production dataframe with all metadata columns.
        
        Parameters
        ----------
        selected_mask : pd.Series
            Boolean mask of selected projects
        production_type : str
            'crude' or 'natgas'
            
        Returns
        -------
        pd.DataFrame
            Production dataframe with metadata columns
        """
        is_oil = (production_type == 'crude')
        production_df = self.cash_flow.crude_production if is_oil else self.cash_flow.natgas_production
        
        # Get production data
        temp = production_df.loc[selected_mask].mul(self.project_drilling[selected_mask], axis=0)
        temp.columns = temp.columns + self.rest_curcalyr
        labels = [i for i in temp.columns if i > self.parent.final_aeo_year]
        temp = temp.drop(labels, axis=1)
        
        # Add metadata columns
        temp = self._add_metadata_columns(temp, selected_mask)
        
        # Add SGTO play name
        temp = self._add_sgto_play_name(temp, is_oil=is_oil)
        
        # Add process code descriptions
        temp = self._add_process_code_descriptions(temp)
        
        # Add county name
        temp = self._add_county_name(temp)
        
        return temp

    def _process_ngpl_production(self, selected_mask):
        """Process all NGPL production types and add to output dataframes.
        
        Parameters
        ----------
        selected_mask : pd.Series
            Boolean mask of selected projects
        """
        # Get base NGPL production
        temp_ngpl = self.cash_flow.ngpl_production.loc[selected_mask].mul(self.project_drilling[selected_mask], axis=0)
        
        # Apply factors to convert to mmb/d
        temp_ngpl = temp_ngpl.mul(0.000001, axis=1).div(365, axis=1)
        
        # Calculate individual NGPL type productions
        temp_ethane_production = temp_ngpl.mul(self.projects.loc[selected_mask, 'NGPLET'], axis='index')
        temp_propane_production = temp_ngpl.mul(self.projects.loc[selected_mask, 'NGPLPR'], axis='index')
        temp_butane_production = temp_ngpl.mul(self.projects.loc[selected_mask, 'NGPLBU'], axis='index')
        temp_isobutane_production = temp_ngpl.mul(self.projects.loc[selected_mask, 'NGPLIS'], axis='index')
        temp_proplus_production = temp_ngpl.mul(self.projects.loc[selected_mask, 'NGPLPP'], axis='index')
        
        # Define NGPL types and their dataframes
        ngpl_types = {
            'total': (temp_ngpl, 'ngpl_production'),
            'ethane': (temp_ethane_production, 'ngpl_ethane_production'),
            'propane': (temp_propane_production, 'ngpl_propane_production'),
            'butane': (temp_butane_production, 'ngpl_butane_production'),
            'isobutane': (temp_isobutane_production, 'ngpl_isobutane_production'),
            'pentanes': (temp_proplus_production, 'ngpl_proplus_production')
        }
        
        # Process each NGPL type
        for ngpl_type, (temp_production, output_attr) in ngpl_types.items():
            # Shift columns by current year
            temp_production.columns = temp_production.columns + self.rest_curcalyr
            labels = [i for i in temp_production.columns if i > self.parent.final_aeo_year]
            temp_production = temp_production.drop(labels, axis=1)
            
            # Add common metadata columns
            temp_production[nam.process_code] = self.projects.loc[selected_mask, nam.process_code]
            temp_production[nam.resid] = self.projects.loc[selected_mask, nam.resid]
            temp_production[nam.district_number] = self.projects.loc[selected_mask, nam.district_number]
            temp_production[nam.region_number] = self.projects.loc[selected_mask, nam.region_number]
            temp_production[nam.federal_land] = self.projects.loc[selected_mask, nam.federal_land]
            temp_production[nam.play] = self.projects.loc[selected_mask, nam.play]
            temp_production[nam.year_production_start] = self.rest_curcalyr
            
            # Add to output dataframe
            output_df = getattr(self, output_attr)
            output_df = pd.concat([output_df, temp_production], ignore_index=True)
            output_df = output_df.fillna(0)
            setattr(self, output_attr, output_df)

    def _create_output_dataframe(self, data_source, selected_mask, metadata_cols):
        """Create output dataframe (wells, dryholes, CO2) with metadata columns.
        
        Parameters
        ----------
        data_source : pd.DataFrame
            Source data (project_drilling, project_dryholes, project_co2_inj, etc.)
        selected_mask : pd.Series
            Boolean mask of selected projects
        metadata_cols : list
            List of metadata column names to add
            
        Returns
        -------
        pd.DataFrame
            Output dataframe with metadata columns
        """
        temp = pd.DataFrame(data_source.loc[selected_mask].copy())
        temp.columns = temp.columns + self.rest_curcalyr
        labels = [i for i in temp.columns if i > self.parent.final_aeo_year]
        temp = temp.drop(labels, axis=1)
        
        # Add metadata columns
        for col in metadata_cols:
            if col in self.projects.columns:
                temp[col] = self.projects.loc[selected_mask, col]
        
        temp[nam.year_production_start] = self.rest_curcalyr
        return temp

    def _write_production_debug_files(self, temp_crude, temp_natgas):
        """Write production debug files for crude and natural gas, preserving project_royalty_multiplier logic.
        
        Parameters
        ----------
        temp_crude : pd.DataFrame
            Crude production dataframe
        temp_natgas : pd.DataFrame
            Natural gas production dataframe
        """
        # Helper function to write debug file for a production type
        def write_debug_file(production_df, production_type):
            if production_df.empty:
                return
            
            filename = f'hsm_on_{production_type}_production_{self.rest_curcalyr}_{self.parent.current_iteration}.csv'
            
            # Reorder columns: ID parameters first, then year columns
            cols = production_df.columns.tolist()
            # Exclude critical columns from CSV output only (keep in dataframe for restart variables)
            cols_to_exclude_from_csv = [nam.lfmm_crude_type, nam.avg_api, nam.api]
            # Separate ID columns (non-year) from year columns, excluding critical columns from CSV
            id_cols = [col for col in cols if col not in cols_to_exclude_from_csv and not (isinstance(col, (int, float)) and 2000 <= col <= 2100)]
            year_cols = [col for col in cols if isinstance(col, (int, float)) and 2000 <= col <= 2100]
            year_cols.sort()
            
            # Ensure project_royalty_multiplier is included in id_cols (it should be, but verify)
            if nam.project_royalty_multiplier in cols:
                if nam.project_royalty_multiplier not in id_cols:
                    id_cols.append(nam.project_royalty_multiplier)
            else:
                # Column is missing - this should not happen, log a warning
                self.logger.warning(f'project_royalty_multiplier column missing from {production_type}_production dataframe. This may indicate an issue with data flow.')
            
            # Reorder: ID columns first, then year columns
            reordered_cols = id_cols + year_cols
            # Ensure project_royalty_multiplier is in the final column list before selecting
            if nam.project_royalty_multiplier not in reordered_cols and nam.project_royalty_multiplier in cols:
                # Insert after play column if it exists, otherwise at the end of id_cols
                if nam.play in reordered_cols:
                    play_idx = reordered_cols.index(nam.play)
                    reordered_cols.insert(play_idx + 1, nam.project_royalty_multiplier)
                else:
                    reordered_cols.insert(len(id_cols), nam.project_royalty_multiplier)
            
            production_reordered = production_df[reordered_cols].copy()
            
            # Add state column extracted from resid (characters 2-4 of resid string)
            if nam.resid in production_reordered.columns:
                production_reordered[nam.state] = production_reordered[nam.resid].str[2:4]
                # Insert state column right after resid in the column order
                id_cols_with_state = id_cols.copy()
                if nam.resid in id_cols_with_state:
                    resid_idx = id_cols_with_state.index(nam.resid)
                    id_cols_with_state.insert(resid_idx + 1, nam.state)
                else:
                    id_cols_with_state.insert(0, nam.state)
                
                # Add county name after state if available
                county_name_col = 'cnty_name'
                if county_name_col in production_reordered.columns and nam.state in id_cols_with_state:
                    state_idx = id_cols_with_state.index(nam.state)
                    if county_name_col not in id_cols_with_state:
                        id_cols_with_state.insert(state_idx + 1, county_name_col)
                reordered_cols = id_cols_with_state + year_cols
                
                # Ensure project_royalty_multiplier is still in reordered_cols after state column addition
                if nam.project_royalty_multiplier not in reordered_cols and nam.project_royalty_multiplier in production_reordered.columns:
                    # Insert after play column if it exists, otherwise after state
                    if nam.play in reordered_cols:
                        play_idx = reordered_cols.index(nam.play)
                        reordered_cols.insert(play_idx + 1, nam.project_royalty_multiplier)
                    elif nam.state in reordered_cols:
                        state_idx = reordered_cols.index(nam.state)
                        reordered_cols.insert(state_idx + 1, nam.project_royalty_multiplier)
                    else:
                        reordered_cols.insert(len(id_cols_with_state), nam.project_royalty_multiplier)
                
                # Reorder description columns to be after their numeric counterparts
                desc_mappings = [
                    (nam.well_type_number, 'well_type'),
                    (nam.oil_type_number, 'oil_type'),
                    (nam.gas_type_number, 'gas_type'),
                    (nam.process_code, 'prime_fuel_type'),
                    (nam.process_code, 'dev_type'),
                    (nam.process_code, 'resource_type')
                ]
                for num_col, desc_col in desc_mappings:
                    if num_col in reordered_cols and desc_col in production_reordered.columns:
                        num_idx = reordered_cols.index(num_col)
                        if desc_col not in reordered_cols:
                            reordered_cols.insert(num_idx + 1, desc_col)
                        elif reordered_cols.index(desc_col) != num_idx + 1:
                            # Move description column to right after numeric column
                            reordered_cols.remove(desc_col)
                            reordered_cols.insert(num_idx + 1, desc_col)
                production_reordered = production_reordered[reordered_cols]
            
            # Convert year columns to appropriate units
            if production_type == 'crude':
                # Convert from bbls/yr to Mbbl/d (thousand barrels per day)
                production_reordered[year_cols] = production_reordered[year_cols] / 365000
                output_path = self.output_path + 'projects_debug//crude//' + filename
                # Log total crude production in million barrels per day when year is 2026
                if self.rest_curcalyr == 2026:
                    crude_2026_prod = production_df[2026].sum()/365000000
                    logging.info(f"Total crude production in {self.rest_curcalyr}: {crude_2026_prod:.2f} million barrels per day")
            else:  # natgas
                # Convert from cf/yr to tcf/yr (trillion cubic feet per year)
                production_reordered[year_cols] = production_reordered[year_cols] / 1e9
                output_path = self.output_path + 'projects_debug//natgas//' + filename
            
            # Final verification: ensure project_royalty_multiplier is in the dataframe before writing
            if nam.project_royalty_multiplier not in production_reordered.columns:
                self.logger.error(f'project_royalty_multiplier column missing from {production_type}_production_reordered before writing debug file. Available columns: {list(production_reordered.columns)[:10]}...')
            
            # Add actual wells drilled from self.project_drilling (current model year per project)
            if (nam.hsm_index in production_reordered.columns and nam.hsm_index in self.projects.columns
                    and not self.project_drilling.empty):
                drill_vals = self.project_drilling.squeeze() if isinstance(self.project_drilling, pd.DataFrame) else self.project_drilling
                drill_df = pd.DataFrame({
                    nam.hsm_index: self.projects.loc[self.project_drilling.index, nam.hsm_index].values,
                    'wells_drilled': drill_vals.values
                })
                drill_df = drill_df.drop_duplicates(subset=nam.hsm_index, keep='first')
                production_reordered = production_reordered.merge(drill_df, on=nam.hsm_index, how='left')
            
            # Add pre-STEO wells drilled from self.project_drilling_pre_steo when present
            if (nam.hsm_index in production_reordered.columns and nam.hsm_index in self.projects.columns
                    and not self.project_drilling_pre_steo.empty):
                pre_steo_vals = self.project_drilling_pre_steo.squeeze() if isinstance(self.project_drilling_pre_steo, pd.DataFrame) else self.project_drilling_pre_steo
                # Resolve hsm_index for pre_steo rows: index may not be in self.projects after later ops in select_projects
                try:
                    pre_steo_hsm = self.projects.loc[self.project_drilling_pre_steo.index, nam.hsm_index].values
                except KeyError:
                    pre_steo_hsm = np.asarray(self.project_drilling_pre_steo.index)
                pre_steo_df = pd.DataFrame({
                    nam.hsm_index: pre_steo_hsm,
                    'wells_drilled_pre_steo': pre_steo_vals.values
                })
                pre_steo_df = pre_steo_df.drop_duplicates(subset=nam.hsm_index, keep='first')
                production_reordered = production_reordered.merge(pre_steo_df, on=nam.hsm_index, how='left')
            
            # Convert to long format: one row per (identifier, year), values in a single column
            id_vars = [c for c in production_reordered.columns if c not in year_cols]
            production_long = production_reordered.melt(
                id_vars=id_vars,
                value_vars=year_cols,
                var_name='year',
                value_name='production'
            )
            production_long.to_csv(output_path, index=False)
        
        # Write debug files for both production types
        write_debug_file(self.crude_production, 'crude')
        write_debug_file(self.natgas_production, 'natgas')

    def select_projects(self):
        """Select projects that are economical to produce in the current run iteration.


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
        past_drilling_mask  =  (self.projects[nam.last_year_drilling] >= self.past_drilling_threshold) | ((self.projects[nam.hist_year_wells] >= self.past_drilling_threshold) & (self.rest_curcalyr <= self.parent.steo_years[-1]))
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
            # Exclude continuous and undiscovered projects from zero_year production
            # These project types should start production in zero_year + 1 at earliest
            continuous_undiscovered_mask = (self.projects[nam.table] != nam.continuous) & (self.projects[nam.table] != nam.undiscovered)
            selected_mask = selected_mask & past_drilling_mask & continuous_undiscovered_mask
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
        # Ensure numeric dtype to avoid pandas FutureWarning on assignment
        temp_cap = pd.to_numeric(temp_cap, errors='coerce').astype('float64')
        cum_cap = (base_cap + temp_cap.cumsum(axis=0)).astype('float64')

        #Apply cumulative capital column to Projects Df
        self.projects.loc[cum_cap.index, nam.cumulative_capital] = cum_cap.values

        #Add capital constraint to selected mask
        capital_mask = self.projects[nam.cumulative_capital] <= self.projects[nam.capital_constraint]
        selected_mask = selected_mask & capital_mask

        ###Get production from cash_flow
        #Crude
        temp_crude = self._create_production_dataframe(selected_mask, production_type='crude')
        
        #Natural gas
        temp_natgas = self._create_production_dataframe(selected_mask, production_type='natgas')

        ###Store pre-STEO drilling values for STEO year 0
        if self.rest_curcalyr == self.parent.steo_years[0]:
            self.project_drilling_pre_steo = self.project_drilling.loc[selected_mask].copy()

        ###STEO Benchmark Override Logic (after production computation)
        adjusted_crude_production, adjusted_natgas_production, adjusted_plays = \
            self._apply_steo_benchmark(selected_mask, temp_crude, temp_natgas)

        # Update production dataframes: remove old entries for adjusted plays, add new ones
        if adjusted_plays:
            # Remove old entries for adjusted plays from current year
            if not self.crude_production.empty and nam.play in self.crude_production.columns:
                crude_mask = ~(self.crude_production[nam.play].isin(adjusted_plays) & 
                              (self.crude_production[nam.year_production_start] == self.rest_curcalyr))
                self.crude_production = self.crude_production[crude_mask].copy()
            
            if not self.natgas_production.empty and nam.play in self.natgas_production.columns:
                natgas_mask = ~(self.natgas_production[nam.play].isin(adjusted_plays) & 
                               (self.natgas_production[nam.year_production_start] == self.rest_curcalyr))
                self.natgas_production = self.natgas_production[natgas_mask].copy()
            
            # Remove adjusted plays from temp dataframes before concatenation
            if not temp_crude.empty and nam.play in temp_crude.columns:
                temp_crude = temp_crude[~temp_crude[nam.play].isin(adjusted_plays)].copy()
            if not temp_natgas.empty and nam.play in temp_natgas.columns:
                temp_natgas = temp_natgas[~temp_natgas[nam.play].isin(adjusted_plays)].copy()
        
        # Concatenate production dataframes
        if not temp_crude.empty:
            self.crude_production = pd.concat([self.crude_production, temp_crude], ignore_index=True)
        if not adjusted_crude_production.empty:
            self.crude_production = pd.concat([self.crude_production, adjusted_crude_production], ignore_index=True)
        # Fill NaN with 0 for numeric columns only, preserve string columns like sgto_play_name
        numeric_cols = self.crude_production.select_dtypes(include=[np.number]).columns
        self.crude_production[numeric_cols] = self.crude_production[numeric_cols].fillna(0)

        if not temp_natgas.empty:
            self.natgas_production = pd.concat([self.natgas_production, temp_natgas], ignore_index=True)
        if not adjusted_natgas_production.empty:
            self.natgas_production = pd.concat([self.natgas_production, adjusted_natgas_production], ignore_index=True)
        # Fill NaN with 0 for numeric columns only, preserve string columns like sgto_play_name
        numeric_cols_natgas = self.natgas_production.select_dtypes(include=[np.number]).columns
        self.natgas_production[numeric_cols_natgas] = self.natgas_production[numeric_cols_natgas].fillna(0)

        # Debug: Save production dataframes to projects_debug folder (one file per rest_curcalyr and iteration)
        self._write_production_debug_files(temp_crude, temp_natgas)

        #NGPLs
        self._process_ngpl_production(selected_mask)

        #Wells
        wells_metadata_cols = [nam.process_code, nam.resid, nam.district_number, nam.region_number, 
                              nam.well_type_number, nam.well_decline_limit, nam.past_wells, nam.play, nam.hsm_index]
        temp = self._create_output_dataframe(self.project_drilling, selected_mask, wells_metadata_cols)
        self.wells = pd.concat([self.wells, temp], ignore_index=True)
        self.wells = self.wells.fillna(0)

        #Update projects past drilling
        self.projects = self.projects.merge(temp[self.rest_curcalyr], how = 'left', left_index = True, right_index = True)
        self.projects[self.rest_curcalyr] = self.projects[self.rest_curcalyr].fillna(0)

        if self.rest_curcalyr > self.parent.history_year:
            self.projects[nam.past_wells] += self.projects[self.rest_curcalyr]
        self.projects = self.projects.drop([self.rest_curcalyr], axis = 1)

        #Dryholes
        temp = self._create_output_dataframe(self.project_dryholes, selected_mask, wells_metadata_cols)
        self.dryholes = pd.concat([self.dryholes, temp], ignore_index=True)
        self.dryholes = self.dryholes.fillna(0)


        #CO2 injected
        temp_co2_inj = pd.DataFrame(self.project_co2_inj.loc[selected_mask].copy())
        temp_co2_inj['wells'] = self.project_drilling[selected_mask].copy()
        temp_co2_inj[temp_co2_inj.columns] = temp_co2_inj[temp_co2_inj.columns].mul(temp_co2_inj['wells'], axis = 0)
        temp_co2_inj = temp_co2_inj.drop(['wells'], axis = 1)
        co2_metadata_cols = [nam.process_code, nam.resid, nam.district_number, nam.region_number, nam.federal_land, nam.play]
        # Shift columns and drop future years
        temp_co2_inj.columns = temp_co2_inj.columns + self.rest_curcalyr
        labels = [i for i in temp_co2_inj.columns if i > self.parent.final_aeo_year]
        temp_co2_inj = temp_co2_inj.drop(labels, axis=1)
        # Add metadata columns
        for col in co2_metadata_cols:
            if col in self.projects.columns:
                temp_co2_inj[col] = self.projects.loc[selected_mask, col]
        temp_co2_inj[nam.year_production_start] = self.rest_curcalyr
        self.co2_injected = pd.concat([self.co2_injected, temp_co2_inj], ignore_index=True)
        self.co2_injected = self.co2_injected.fillna(0)

        #CO2 Recycled
        temp_co2_recy = pd.DataFrame(self.project_co2_recy.loc[selected_mask].copy())
        temp_co2_recy['wells'] = self.project_drilling[selected_mask].copy()
        temp_co2_recy[temp_co2_recy.columns] = temp_co2_recy[temp_co2_recy.columns].mul(temp_co2_recy['wells'], axis = 0)
        temp_co2_recy = temp_co2_recy.drop(['wells'], axis = 1)
        # Shift columns and drop future years
        temp_co2_recy.columns = temp_co2_recy.columns + self.rest_curcalyr
        labels = [i for i in temp_co2_recy.columns if i > self.parent.final_aeo_year]
        temp_co2_recy = temp_co2_recy.drop(labels, axis=1)
        # Add metadata columns
        for col in co2_metadata_cols:
            if col in self.projects.columns:
                temp_co2_recy[col] = self.projects.loc[selected_mask, col]
        temp_co2_recy[nam.year_production_start] = self.rest_curcalyr
        self.co2_recycled = pd.concat([self.co2_recycled, temp_co2_recy], ignore_index=True)
        self.co2_recycled = self.co2_recycled.fillna(0)


        ###Apply selected EOR wells to self.wells
        #Merge fixed well counts to wells output df
        eor_mask = (self.wells[nam.process_code] == 10) | (self.wells[nam.process_code] == 11)
        selected_eor_mask = eor_mask & selected_mask
        temp_wells = self.wells.loc[selected_eor_mask].copy()

        #Get fixed project wells
        temp_projects = self.projects.loc[selected_eor_mask].copy()
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
        # In STEO year 0, use pre-STEO values for non-EOR projects
        if self.rest_curcalyr == self.parent.steo_years[0] and not self.project_drilling_pre_steo.empty:
            # Filter pre-STEO values to only non-EOR projects (process_code != 10 and != 11)
            # project_drilling_pre_steo has indices from selected_mask, so filter those indices
            pre_steo_indices = self.project_drilling_pre_steo.index
            non_eor_pre_steo_mask = (self.projects.loc[pre_steo_indices, nam.process_code] != 10) & \
                                     (self.projects.loc[pre_steo_indices, nam.process_code] != 11)
            non_eor_pre_steo_indices = pre_steo_indices[non_eor_pre_steo_mask]
            # Use pre-STEO values for non-EOR projects
            if len(non_eor_pre_steo_indices) > 0:
                self.projects.loc[non_eor_pre_steo_indices, nam.last_year_drilling] = \
                    self.project_drilling_pre_steo.loc[non_eor_pre_steo_indices]
            # Use STEO-adjusted values for EOR projects
            eor_mask = selected_mask & ((self.projects[nam.process_code] == 10) | (self.projects[nam.process_code] == 11))
            if eor_mask.any():
                self.projects.loc[eor_mask, nam.last_year_drilling] = self.project_drilling.loc[eor_mask]
        else:
            # Normal case: use current project_drilling values
            self.projects.loc[selected_mask, nam.last_year_drilling] = self.project_drilling.loc[selected_mask]


        ###Create selected projects df for debug
        self.projects_selected = self.projects[selected_mask].copy()


        ###Remove depleted projects from the projects list
        projects_mask = (self.projects[nam.past_wells] > self.projects[nam.max_wells]) & (self.projects[nam.process_code] >= 16)
        self.projects = self.projects.loc[~projects_mask]
        self.project_co2_inj = self.project_co2_inj.loc[~projects_mask]
        self.project_co2_recy = self.project_co2_recy.loc[~projects_mask]


        ###Remove EOR projects that are not eligible from the projects list and CO2 EOR lists
        eor_mask = (self.projects[nam.process_code] == 10) | (self.projects[nam.process_code] == 11)
        econ_life_mask = (self.projects[nam.econ_life] >= 2050) | (self.projects[nam.econ_life] + 11 < self.rest_curcalyr )
        eligible_mask = self.projects[nam.eligible] == 0
        eor_mask = (eor_mask & econ_life_mask) | (eor_mask & eligible_mask)

        self.projects = self.projects.loc[~eor_mask]
        self.project_co2_inj = self.project_co2_inj.loc[~eor_mask]
        self.project_co2_recy = self.project_co2_recy.loc[~eor_mask]

        pass


    def debug_onshore(self):
        """Produce debug outputs for Onshore Submodule.

        Returns
        -------
        None

        """
        ### Debug Projects
        # Note: project_royalty_multiplier is included in these debug files (all columns are written by default)
        self.projects_selected.to_csv(self.output_path + 'projects_debug//selected//' + 'hsm_on_selected_projects' + '_' + str(self.rest_curcalyr) + '_' + str(self.parent.current_iteration) + '.csv')
        self.projects.to_csv(self.output_path + 'projects_debug//all//' + 'hsm_on_projects' + '_' + str(self.rest_curcalyr) + '_' + str(self.parent.current_iteration) + '.csv')

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
        temp.groupby([nam.resid, nam.play, nam.process_code, nam.region_number, nam.district_number, nam.well_type_number, nam.lfmm_crude_type, 'oil_type_number', 'gas_type_number']).sum().to_csv(self.output_path + 'module_results_debug//crude//' + 'hsm_on_crude_fields.csv')
        temp[([nam.process_code] + debug_range)].groupby(nam.process_code).sum().to_csv(self.output_path + 'module_results_debug//crude//' + 'hsm_on_crude_proc_code.csv')
        temp[([nam.region_number] + debug_range)].groupby(nam.region_number).sum().to_csv(self.output_path + 'module_results_debug//crude//' + 'hsm_on_crude_region_num.csv')
        temp[([nam.district_number] + debug_range)].groupby(nam.district_number).sum().to_csv(self.output_path + 'module_results_debug//crude//' + 'hsm_on_crude_district_num.csv')
        temp[([nam.play] + debug_range)].groupby(nam.play).sum().to_csv(self.output_path + 'module_results_debug//crude//' + 'hsm_on_crude_play.csv')

        #Debug Natgas
        temp = self.natgas_production.copy()
        temp = temp.drop([nam.gas_type, nam.oil_type, nam.hsm_index,'API','avg_api', 'year_production_start','federal_land'], axis = 1)
        temp[[nam.resid, nam.play, nam.process_code, nam.region_number, nam.district_number, 'oil_type_number', 'gas_type_number']] \
            = temp[[nam.resid, nam.play, nam.process_code, nam.region_number, nam.district_number, 'oil_type_number', 'gas_type_number']].astype('str')
        temp.groupby([nam.resid, nam.play, nam.process_code, nam.region_number, nam.district_number, 'oil_type_number', 'gas_type_number']).sum().to_csv(self.output_path + 'module_results_debug//natgas//' + 'hsm_on_natgas_fields.csv')
        temp[([nam.process_code] + debug_range)].groupby(nam.process_code).sum().to_csv(self.output_path + 'module_results_debug//natgas//' + 'hsm_on_natgas_proc_code.csv' )
        temp[([nam.region_number] + debug_range)].groupby(nam.region_number).sum().to_csv(self.output_path + 'module_results_debug//natgas//' + 'hsm_on_natgas_region_num.csv' )
        temp[([nam.play] + debug_range)].groupby(nam.play).sum().to_csv(self.output_path + 'module_results_debug//natgas//' + 'hsm_on_natgas_play.csv' )

        #Debug NGPLs
        temp = self.ngpl_production.copy()
        temp = temp.drop(['federal_land'], axis = 1)
        temp[[nam.resid, nam.play, nam.process_code, nam.region_number, nam.district_number]] = temp[[nam.resid, nam.play, nam.process_code, nam.region_number, nam.district_number]].astype('str')
        temp.groupby([nam.resid, nam.play, nam.process_code, nam.region_number, nam.district_number]).sum().to_csv(self.output_path + 'module_results_debug//ngpls//' + 'hsm_on_ngpls_fields.csv'    )
        temp[([nam.process_code] + debug_range)].groupby(nam.process_code).sum().to_csv(self.output_path + 'module_results_debug//ngpls//' + 'hsm_on_ngpls_proc_code.csv' )
        temp[([nam.region_number] + debug_range)].groupby(nam.region_number).sum().to_csv(self.output_path + 'module_results_debug//ngpls//' + 'hsm_on_ngpls_region_num.csv' )
        temp[([nam.district_number] + debug_range)].groupby(nam.district_number).sum().to_csv(self.output_path + 'module_results_debug//ngpls//' + 'hsm_on_ngpls_district_num.csv' )
        temp[([nam.play] + debug_range)].groupby(nam.play).sum().to_csv(self.output_path + 'module_results_debug//ngpls//' + 'hsm_on_ngpls_play.csv' )

        ##Debug Wells
        temp = self.wells.copy()
        temp = temp.drop([nam.year_production_start, nam.past_wells], axis = 1)
        temp[[nam.resid, nam.play, nam.process_code, nam.region_number, nam.district_number, 'well_type_number', nam.well_decline_limit]] = temp[[nam.resid, nam.play, nam.process_code, nam.region_number, nam.district_number, 'well_type_number', nam.well_decline_limit]].astype('str')
        temp.groupby([nam.resid, nam.play, nam.process_code, nam.region_number, nam.district_number, 'well_type_number', nam.well_decline_limit]).sum().to_csv(self.output_path + 'module_results_debug//wells//' + 'hsm_on_wells_fields.csv')
        temp[([nam.process_code] + debug_range)].groupby(nam.process_code).sum().to_csv(self.output_path + 'module_results_debug//wells//' + 'hsm_on_wells_proc_code.csv' )
        temp[([nam.region_number] + debug_range)].groupby(nam.region_number).sum().to_csv(self.output_path + 'module_results_debug//wells//' + 'hsm_on_wells_region_num.csv' )
        temp[([nam.district_number] + debug_range)].groupby(nam.district_number).sum().to_csv(self.output_path + 'module_results_debug//wells//' + 'hsm_on_wells_district_num.csv' )
        temp[([nam.play] + debug_range)].groupby(nam.play).sum().to_csv(self.output_path + 'module_results_debug//wells//' + 'hsm_on_wells_play.csv')

        #CO2 Cost
        temp = self.projects_selected[[nam.play,nam.region_number,nam.net_present_value]].copy()
        temp = temp.groupby([nam.play,nam.region_number]).sum()
        temp.to_csv(self.output_path + 'module_results_debug//co2//' + 'hsm_on_co2cost_play.csv')

        #CO2 Recycled
        temp = self.co2_recycled.copy()
        temp[[nam.play, nam.region_number]] = temp[[nam.play, nam.region_number]].astype(str)
        temp = temp.drop(['process_code','resid','district_number','federal_land', 'year_production_start'], axis = 1)
        temp[([nam.region_number] + debug_range)].groupby(nam.region_number).sum().to_csv(self.output_path + 'module_results_debug//co2//' + 'hsm_on_co2rec_region.csv' )
        temp.groupby([nam.play, nam.region_number]).sum().to_csv(self.output_path + 'module_results_debug//co2//' + 'hsm_on_co2rec_play.csv')

        #CO2 Injected
        temp = self.co2_injected.copy()
        temp[[nam.play, nam.region_number]] = temp[[nam.play, nam.region_number]].astype(str)
        temp = temp.drop(['process_code','resid','district_number','federal_land', 'year_production_start'], axis = 1)
        temp[([nam.region_number] + debug_range)].groupby(nam.region_number).sum().to_csv(self.output_path + 'module_results_debug//co2//' + 'hsm_on_co2inj_region.csv' )
        temp.groupby([nam.play, nam.region_number]).sum().to_csv(self.output_path + 'module_results_debug//co2//' + 'hsm_on_co2inj_play.csv')

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
        # Note: project_royalty_multiplier is included in all debug files (all columns are written by default)
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


    def _get_tight_oil_play_mapping(self):
        """Get mapping of tight oil play names to play numbers and restart variable indices.
        
        Returns
        -------
        dict
            Dictionary mapping play names to (play_numbers, restart_index) tuples
        """
        return {
            'Bakken': ([2804, 3110, 3111, 3112, 3113, 3114, 3115], 1),
            'Eagle_Ford': ([4761, 4762, 4763], 2),
            'Woodford': ([5861, 5862, 5863, 5875, 99926039], 3),
            'Austin_Chalk': ([4747, 4748, 4749], 4),
            'Spraberry': ([4409], 5),
            'Niobrara': ([3904, 3920, 99905037, 99943037, 99949033, 99949037], 6),
            'Avalon_Bone_Spring': ([4473], 7),
            'Monterey': ([], 8),  # No longer classified, set to zero
            'Wolfcamp': ([4401], 9),
            'Utica': ([6790, 6791, 6792, 6793], 10),
            'Other': ([], 15)  # Will be calculated as all tight oil plays not in above list
        }
    
    def _get_shale_gas_play_mapping(self):
        """Get mapping of shale gas play names to play numbers and restart variable indices.
        
        Returns
        -------
        dict
            Dictionary mapping play names to (play_numbers, restart_index) tuples
        """
        return {
            'Barnett': ([4561, 4562, 4563], 1),
            'Haynesville': ([4774, 4775], 2),
            'Fayetteville': ([6261, 6262], 3),
            'Woodford': ([5861, 5862, 5863, 5875, 99926039], 4),
            'Eagle_Ford': ([4761, 4762, 4763], 5),
            'Antrim': ([6361], 6),
            'Marcellus': ([6761, 6776, 6777, 6778, 6779, 6780, 6781, 6782, 6783], 7),
            'Bakken': ([2804, 3110, 3111, 3112, 3113, 3114, 3115], 8),
            'Utica': ([6790, 6791, 6792, 6793], 9),
            'Permian': ([4401, 4409, 4413, 4415, 4425, 4471, 4472, 4473, 4475, 4565], 10),
            'Other': ([], 15)  # Will be calculated as all shale/tight plays not in above list
        }
    
    def _calculate_play_production(self, production_df, play_mapping, year_col, conversion_factor, restart_obj, restart_var_name, well_type_filter=None, exclude_plays=None):
        """Calculate production for multiple plays and write to restart variable.
        
        Parameters
        ----------
        production_df : pd.DataFrame
            Production dataframe
        play_mapping : dict
            Dictionary mapping play names to (play_numbers, restart_index) tuples
        year_col : int
            Year column to aggregate
        conversion_factor : float
            Factor to convert production units (e.g., / 1000000 / 365)
        restart_obj : object
            Restart object containing restart variables
        restart_var_name : str
            Name of restart variable attribute (e.g., 'ogsmout_ogqshloil')
        well_type_filter : list, optional
            List of well_type_numbers to filter by
        exclude_plays : list, optional
            List of play numbers to exclude when calculating "Other"
        """
        # Start with play and year columns, add well_type_number if filtering is needed
        cols_to_copy = [nam.play, year_col]
        if well_type_filter is not None and nam.well_type_number in production_df.columns:
            cols_to_copy.append(nam.well_type_number)
        temp = production_df.copy()[cols_to_copy]
        
        # Apply well_type filter if specified
        if well_type_filter is not None and nam.well_type_number in temp.columns:
            temp = temp[temp[nam.well_type_number].isin(well_type_filter)]
            temp = temp.drop([nam.well_type_number], axis=1)
        
        all_play_numbers = []
        for play_name, (play_numbers, restart_idx) in play_mapping.items():
            if play_name == 'Other':
                continue  # Handle "Other" separately
            if not play_numbers:
                # Set to zero for plays that are no longer classified
                restart_df = getattr(restart_obj, restart_var_name)
                restart_df.at[(restart_idx, year_col), 'value'] = _cast_restart_scalar(restart_df, 0)
                continue
            
            all_play_numbers.extend(play_numbers)
            play_temp = temp[temp[nam.play].isin(play_numbers)].copy()
            if not play_temp.empty:
                production_sum = play_temp[year_col].sum() * conversion_factor
                restart_df = getattr(restart_obj, restart_var_name)
                restart_df.at[(restart_idx, year_col), 'value'] = _cast_restart_scalar(restart_df, production_sum)
            else:
                restart_df = getattr(restart_obj, restart_var_name)
                restart_df.at[(restart_idx, year_col), 'value'] = _cast_restart_scalar(restart_df, 0)
        
        # Handle "Other" play
        if 'Other' in play_mapping:
            _, other_restart_idx = play_mapping['Other']
            if exclude_plays is not None:
                all_play_numbers.extend(exclude_plays)
            other_temp = temp[~temp[nam.play].isin(all_play_numbers)].copy()
            if not other_temp.empty:
                production_sum = other_temp[year_col].sum() * conversion_factor
                restart_df = getattr(restart_obj, restart_var_name)
                restart_df.at[(other_restart_idx, year_col), 'value'] = _cast_restart_scalar(restart_df, production_sum)
            else:
                restart_df = getattr(restart_obj, restart_var_name)
                restart_df.at[(other_restart_idx, year_col), 'value'] = _cast_restart_scalar(restart_df, 0)

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
        # Handle NaN values before converting to int to avoid ValueError
        self.crude_production[nam.process_code] = pd.to_numeric(self.crude_production[nam.process_code], errors='coerce').fillna(0).astype(int)
        self.crude_production[nam.district_number] = pd.to_numeric(self.crude_production[nam.district_number], errors='coerce').fillna(0).astype(int)
        self.crude_production[nam.oil_type_number] = pd.to_numeric(self.crude_production[nam.oil_type_number], errors='coerce').fillna(0).astype(int)

        self.natgas_production[nam.process_code] = pd.to_numeric(self.natgas_production[nam.process_code], errors='coerce').fillna(0).astype(int)
        self.natgas_production[nam.district_number] = pd.to_numeric(self.natgas_production[nam.district_number], errors='coerce').fillna(0).astype(int)

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
            self.restart.ogsmout_ogqcrrep.at[(1, self.rest_curcalyr), 'value'] = _cast_restart_scalar(
                self.restart.ogsmout_ogqcrrep,
                temp_eor / 1000000
            )

            #Conventional and Tight
            temp_oil = temp[~eor_mask].values.sum()
            self.restart.ogsmout_ogqcrrep.at[(2, self.rest_curcalyr), 'value'] = _cast_restart_scalar(
                self.restart.ogsmout_ogqcrrep,
                temp_oil / 1000000
            )


            ###Domestic Crude Oil Production by region for LFMM (including EOR)
            temp = self.crude_production.copy()[[nam.region_number, self.rest_curcalyr]]
            temp = temp.groupby(nam.region_number).sum()
            update_values = temp[[self.rest_curcalyr]].stack() / 1000000 / 365
            update_values = _cast_restart_series(self.restart.pmmout_rfqtdcrd, update_values)
            self.restart.pmmout_rfqtdcrd.loc[update_values.index, 'value'] = update_values.values

            ###Crude Production by Lower 48 Region
            temp = self.restart.pmmout_rfqtdcrd.xs(self.rest_curcalyr, level = 1, drop_level = False).copy()
            temp = temp[temp.index.get_level_values(0).isin([1,2,3,4,5,6,7])].copy()
            temp = _ensure_dtype_compatibility(self.restart.ogsmout_ogcoprd, temp)
            self.restart.ogsmout_ogcoprd.update(temp)

            ###Domestic Crude Oil Production by region for LFMM (not including EOR)
            temp = self.crude_production.copy()[[nam.region_number, nam.oil_type_number, self.rest_curcalyr]]
            temp = temp[temp[nam.oil_type_number].isin([1,2])]
            temp = temp.groupby(nam.region_number).sum()
            update_values = temp[[self.rest_curcalyr]].stack() / 1000000 / 365
            update_values = _cast_restart_series(self.restart.pmmout_rfqdcrd, update_values)
            self.restart.pmmout_rfqdcrd.loc[update_values.index, 'value'] = update_values.values


            ###Crude Oil production by select play
            tight_oil_play_mapping = self._get_tight_oil_play_mapping()
            # Collect all play numbers for "Other" calculation
            all_tight_oil_plays = []
            for play_name, (play_numbers, _) in tight_oil_play_mapping.items():
                if play_name != 'Other':
                    all_tight_oil_plays.extend(play_numbers)
            
            self._calculate_play_production(
                self.crude_production,
                tight_oil_play_mapping,
                self.rest_curcalyr,
                1.0 / 1000000 / 365,  # Convert to million barrels per day
                self.restart,
                'ogsmout_ogqshloil',
                well_type_filter=[2, 5],  # Tight oil plays only
                exclude_plays=all_tight_oil_plays
            )
 

            ###Crude Total Oil Production by oil type
            temp = self.crude_production.copy()
            temp = temp[[self.rest_curcalyr, nam.district_number, nam.oil_type_number]].groupby(
                [nam.district_number, nam.oil_type_number]).sum()
            temp = temp.stack() / (365 * 1000000)
            temp = _cast_restart_series(self.restart.ogsmout_ogoilprd, temp)
            self.restart.ogsmout_ogoilprd.loc[temp.index, 'value'] = temp.values

            ###Oil production by LFMM crude type and HSM region, convert to thousand barrels per day
            temp = self.crude_production[[self.rest_curcalyr, nam.region_number, nam.lfmm_crude_type]].copy()
            temp = temp.groupby([nam.region_number, nam.lfmm_crude_type]).sum()
            temp = temp.stack() / (365 * 1000)
            temp.index = temp.index.set_levels([temp.index.levels[0].astype('int64'), #ensure data type of series indices matches restart file
                                                temp.index.levels[1].astype('int64'),
                                                temp.index.levels[2].astype('int64')])

            #Write to Restart
            temp = _cast_restart_series(self.restart.ogsmout_ogcrdprd, temp)
            self.restart.ogsmout_ogcrdprd.loc[temp.index, 'value'] = temp.values

            #Update Oil Shale Medium Medium Sour
            ogcrd_value = self.restart.ogsmout_ogcrdprd.at[(5, 3, self.rest_curcalyr), nam.value]
            ogqcr_value = self.restart.ogsmout_ogqcrrep.at[(1, self.rest_curcalyr), nam.value]
            self.restart.ogsmout_ogcrdprd.at[(5, 3, self.rest_curcalyr), nam.value] = _cast_restart_scalar(
                self.restart.ogsmout_ogcrdprd,
                ogcrd_value + ogqcr_value
            )


            ###Oil production by LFMM Region
            temp = self.crude_production.copy()[[nam.district_number, nam.lfmm_crude_type, self.rest_curcalyr]]
            temp = pd.merge(temp,
                            self.parent.mapping[[nam.district_number,nam.lfmm_region_number]],
                            on=nam.district_number,
                            how='left')
            temp = temp.drop([nam.district_number], axis = 1)
            temp[nam.lfmm_crude_type] = temp[nam.lfmm_crude_type].astype(int)
            temp = temp.groupby([nam.lfmm_region_number, nam.lfmm_crude_type]).sum()
            update_values = temp[[self.rest_curcalyr]].stack() / 1000000
            update_values = _cast_restart_series(self.restart.ogsmout_ogcruderef, update_values)
            self.restart.ogsmout_ogcruderef.loc[update_values.index, 'value'] = update_values.values


            ###Heat rate by LFMM region crude oil type
            #Get crude production with associated api and crude type identifiers
            temp = self.crude_production.copy()[[nam.district_number, nam.lfmm_crude_type, nam.api, nam.avg_api, self.rest_curcalyr]]
            temp = temp.drop([nam.district_number], axis = 1)
            temp[nam.lfmm_crude_type] = temp[nam.lfmm_crude_type].astype(int)

            #Convert API gravity to Specific Gravity then get heat gravity
            temp['specific_gravity_prod'] = 1.0
            temp['specific_gravity_prod'] = temp['specific_gravity_prod'].astype(float)
            mask = temp[nam.api] > 0
            temp.loc[mask, 'specific_gravity_prod'] = (
                temp.loc[mask, self.rest_curcalyr] / 1000000
                * (141.5 / (temp.loc[mask, nam.api] + 131.5)).fillna(0.0)
            ).astype(float)

            #Groupby and merge volumes
            national_temp = temp.groupby([nam.lfmm_crude_type, nam.avg_api], as_index=False)[[self.rest_curcalyr, 'specific_gravity_prod']].sum()
            national_temp = national_temp.set_index(nam.lfmm_crude_type)

            #Get default gravity value
            #Set heat gravity value
            national_temp['grav'] = national_temp['specific_gravity_prod'].div(national_temp[self.rest_curcalyr] / 1000000)
            national_temp[self.rest_curcalyr] = national_temp['grav'] * (7.801769 - 1.3213 * national_temp['grav']**2)
            update_values = national_temp[[self.rest_curcalyr]].stack()
            update_values = _cast_restart_series(self.restart.ogsmout_ogcrdheat, update_values)
            self.restart.ogsmout_ogcrdheat.loc[update_values.index, 'value'] = update_values.values


            ###EOR Crude Production by Region and Type
            # Create masks for different EOR process codes
            thermal_mask = self.crude_production[nam.process_code].isin([3, 11, 19])  # Thermal EOR (Other EOR)
            co2_mask = self.crude_production[nam.process_code].isin([10, 2, 18])  # CO2 EOR
            
            # Initialize OGEORPRD values for this year to zero
            for region in range(1, 9):  # Regions 1-8
                for eor_type in range(1, 4):  # EOR types 1-3
                    self.restart.ogsmout_ogeorprd.loc[(region, eor_type, self.rest_curcalyr), 'value'] = _cast_restart_scalar(
                        self.restart.ogsmout_ogeorprd,
                        0
                    )
            
            # Process Thermal EOR (eor_type 1)
            if thermal_mask.any():
                temp_thermal = self.crude_production.loc[thermal_mask, [nam.region_number, self.rest_curcalyr]].copy()
                thermal_by_region = temp_thermal.groupby(nam.region_number)[self.rest_curcalyr].sum() / 1000  # Convert to MBbl/yr
                
                # Populate regions 1-7 for Thermal EOR
                for region in range(1, 8):
                    if region in thermal_by_region.index:
                        self.restart.ogsmout_ogeorprd.loc[(region, 1, self.rest_curcalyr), 'value'] = _cast_restart_scalar(
                            self.restart.ogsmout_ogeorprd,
                            thermal_by_region.loc[region]
                        )
                
                # Calculate region 8 (Total) for Thermal EOR
                thermal_total = thermal_by_region.sum()
                self.restart.ogsmout_ogeorprd.loc[(8, 1, self.rest_curcalyr), 'value'] = _cast_restart_scalar(
                    self.restart.ogsmout_ogeorprd,
                    thermal_total
                )
            
            # Process CO2 EOR (eor_type 2)
            if co2_mask.any():
                temp_co2 = self.crude_production.loc[co2_mask, [nam.region_number, self.rest_curcalyr]].copy()
                co2_by_region = temp_co2.groupby(nam.region_number)[self.rest_curcalyr].sum() / 1000  # Convert to MBbl/yr
                
                # Populate regions 1-7 for CO2 EOR
                for region in range(1, 8):
                    if region in co2_by_region.index:
                        self.restart.ogsmout_ogeorprd.loc[(region, 2, self.rest_curcalyr), 'value'] = _cast_restart_scalar(
                            self.restart.ogsmout_ogeorprd,
                            co2_by_region.loc[region]
                        )
                
                # Calculate region 8 (Total) for CO2 EOR
                co2_total = co2_by_region.sum()
                self.restart.ogsmout_ogeorprd.loc[(8, 2, self.rest_curcalyr), 'value'] = _cast_restart_scalar(
                    self.restart.ogsmout_ogeorprd,
                    co2_total
                )
            
            # Calculate Total EOR (eor_type 3) for each region
            for region in range(1, 9):
                thermal_val = self.restart.ogsmout_ogeorprd.loc[(region, 1, self.rest_curcalyr), 'value']
                co2_val = self.restart.ogsmout_ogeorprd.loc[(region, 2, self.rest_curcalyr), 'value']
                self.restart.ogsmout_ogeorprd.loc[(region, 3, self.rest_curcalyr), 'value'] = _cast_restart_scalar(
                    self.restart.ogsmout_ogeorprd,
                    thermal_val + co2_val
                )

            ### Federal/Non-Federal land oil production
            # Federal Land
            temp = self.crude_production.copy()[[nam.region_number, self.rest_curcalyr, nam.federal_land, nam.project_royalty_multiplier]]
            temp = temp.loc[(temp[nam.federal_land] == 1)]

            # Split out mixed land from Federal land
            temp_fed = temp.copy()
            temp_fed[self.rest_curcalyr] = temp_fed[self.rest_curcalyr] * temp_fed[nam.project_royalty_multiplier]
            temp_fed = temp_fed.drop([nam.federal_land,nam.project_royalty_multiplier], axis = 1)
            temp_fed = temp_fed.groupby(nam.region_number).sum()
            update_values = temp_fed[[self.rest_curcalyr]].stack() / 1000000 / 365
            update_values = _cast_restart_series(self.restart.ogsmout_ogcoprd_fed, update_values)
            self.restart.ogsmout_ogcoprd_fed.loc[update_values.index, 'value'] = update_values.values

            temp_nofed = temp.copy()
            temp_nofed[self.rest_curcalyr] = temp_nofed[self.rest_curcalyr] * (1 - temp_nofed[nam.project_royalty_multiplier])
            temp_nofed = temp_nofed.drop([nam.federal_land,nam.project_royalty_multiplier], axis = 1)
            temp_nofed = temp_nofed.groupby(nam.region_number).sum()

            # Non-Federal Land
            temp = self.crude_production.copy()[[nam.region_number, self.rest_curcalyr, nam.federal_land]]
            temp = temp.loc[(temp[nam.federal_land] == 0)]
            temp = temp.drop([nam.federal_land], axis = 1)
            temp = temp.groupby(nam.region_number).sum()
            temp = temp[self.rest_curcalyr].add(temp_nofed[self.rest_curcalyr], fill_value=0).to_frame()
            update_values = temp[[self.rest_curcalyr]].stack() / 1000000 / 365
            update_values = _cast_restart_series(self.restart.ogsmout_ogcoprd_nonfed, update_values)
            self.restart.ogsmout_ogcoprd_nonfed.loc[update_values.index, 'value'] = update_values.values


            ###Non-Associated natural gas production
            temp_na = self.natgas_production[[nam.district_number, self.rest_curcalyr, nam.gas_type_number, nam.well_type_number]].copy()
            well_type_mask = temp_na[nam.well_type_number] >= 3 # this excludes AD gas
            temp_na = temp_na[well_type_mask].copy()
            temp_na = temp_na.drop([nam.well_type_number], axis = 1)
            temp_na[nam.gas_type_number] = temp_na[nam.gas_type_number].astype(int)
            temp_na = temp_na.groupby([nam.district_number, nam.gas_type_number]).sum()
            temp_na = temp_na.stack() / 1000000
            temp_na = _cast_restart_series(self.restart.ogsmout_ogenagprd, temp_na)
            self.restart.ogsmout_ogenagprd.loc[temp_na.index, 'value'] = temp_na.values


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
            temp_ad = _ensure_dtype_compatibility(self.restart.ogsmout_ogadgprd, temp_ad)
            temp_ad[nam.value] = temp_ad[nam.value].astype(self.restart.ogsmout_ogadgprd[nam.value].dtype)
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
            temp_prod['value'] = _cast_restart_series(self.restart.ogsmout_ogdngprd, temp_prod['value'])
            self.restart.ogsmout_ogdngprd.update(temp_prod['value'])

            #Sum values
            temp = self.restart.ogsmout_ogdngprd.copy()
            temp = temp[temp.index.isin([1, 2, 3, 4], level=1)]
            temp = temp.groupby(level=[0, 2]).sum()
            temp[nam.well_type] = 5
            temp = temp.set_index([nam.well_type], append=True)
            temp = temp.reorder_levels([0, 2, 1])
            temp.index.names = self.restart.ogsmout_ogdngprd.index.names
            temp = _ensure_dtype_compatibility(self.restart.ogsmout_ogdngprd, temp)
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
            update_values = temp[[self.rest_curcalyr]].stack() / 1000000
            update_values = _cast_restart_series(self.restart.ogsmout_ogprdad, update_values)
            self.restart.ogsmout_ogprdad.loc[update_values.index, 'value'] = update_values.values

            if (self.parent.param_ncrl != 1) | (self.parent.integrated_switch == False): #During Reporting loop iteration only calculate after realized production adjustments
                ###Natural gas production by select play
                shale_gas_play_mapping = self._get_shale_gas_play_mapping()
                # Collect all play numbers for "Other" calculation
                all_shale_gas_plays = []
                for play_name, (play_numbers, _) in shale_gas_play_mapping.items():
                    if play_name != 'Other':
                        all_shale_gas_plays.extend(play_numbers)
                
                self._calculate_play_production(
                    self.natgas_production,
                    shale_gas_play_mapping,
                    self.rest_curcalyr,
                    1.0 / 1000000000,  # Convert to TCF
                    self.restart,
                    'ogsmout_ogqshlgas',
                    well_type_filter=[2, 5],  # Shale gas and tight oil plays
                    exclude_plays=all_shale_gas_plays
                )
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
            self.restart.ogsmout_ogqngrep.at[(1, self.rest_curcalyr), 'value'] = _cast_restart_scalar(
                self.restart.ogsmout_ogqngrep,
                shale_temp / 1000000
            )

            #Coalbed Methane
            cbm_mask = temp[nam.well_type_number] == 6
            cbm_temp = temp[cbm_mask].copy()
            cbm_temp = cbm_temp[self.rest_curcalyr].sum()
            self.restart.ogsmout_ogqngrep.at[(2, self.rest_curcalyr), 'value'] = _cast_restart_scalar(
                self.restart.ogsmout_ogqngrep,
                cbm_temp / 1000000
            )

            #Tight Gas
            tight_mask = temp[nam.well_type_number] == 4
            tight_temp = temp[tight_mask].copy()
            tight_temp = tight_temp[self.rest_curcalyr].sum()
            self.restart.ogsmout_ogqngrep.at[(3, self.rest_curcalyr), 'value'] = _cast_restart_scalar(
                self.restart.ogsmout_ogqngrep,
                tight_temp / 1000000
            )

            #Conventional Gas
            conv_mask = temp[nam.well_type_number] == 3
            conv_temp = temp[conv_mask].copy()
            conv_temp = conv_temp[self.rest_curcalyr].sum()
            self.restart.ogsmout_ogqngrep.at[(4, self.rest_curcalyr), 'value'] = _cast_restart_scalar(
                self.restart.ogsmout_ogqngrep,
                conv_temp / 1000000
            )

            #Onshore AD Gas from non-tight plays
            ad_mask = temp[nam.well_type_number] == 1
            ad_temp = temp[ad_mask].copy()
            ad_temp = ad_temp[self.rest_curcalyr].sum()
            self.restart.ogsmout_ogqngrep.at[(5, self.rest_curcalyr), 'value'] = _cast_restart_scalar(
                self.restart.ogsmout_ogqngrep,
                ad_temp / 1000000
            )

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
            update_values = temp[[self.rest_curcalyr]].stack() / 1000000
            update_values = _cast_restart_series(self.restart.ogsmout_ogprdugr, update_values)
            self.restart.ogsmout_ogprdugr.loc[update_values.index, 'value'] = update_values.values


            ### Federal/Non-Federal land natural gas production
            # Federal Land
            temp = self.natgas_production.copy()[[nam.region_number, self.rest_curcalyr, nam.federal_land, nam.project_royalty_multiplier]]
            temp = temp.loc[temp[nam.federal_land] == 1]

            # Split out mixed land from Federal land
            temp_fed = temp.copy()
            temp_fed[self.rest_curcalyr] = temp_fed[self.rest_curcalyr] * temp_fed[nam.project_royalty_multiplier]
            temp_fed = temp_fed.drop([nam.federal_land, nam.project_royalty_multiplier], axis=1)
            temp_fed = temp_fed.groupby(nam.region_number).sum()
            update_values = temp_fed[[self.rest_curcalyr]].stack() / 1000000000
            update_values = _cast_restart_series(self.restart.ogsmout_ogngprd_fed, update_values)
            self.restart.ogsmout_ogngprd_fed.loc[update_values.index, 'value'] = update_values.values

            temp_nofed = temp.copy()
            temp_nofed[self.rest_curcalyr] = temp_nofed[self.rest_curcalyr] * (1 - temp_nofed[nam.project_royalty_multiplier])
            temp_nofed = temp_nofed.drop([nam.federal_land, nam.project_royalty_multiplier], axis=1)
            temp_nofed = temp_nofed.groupby(nam.region_number).sum()

            # Non-Federal Land
            temp = self.natgas_production.copy()[[nam.region_number, self.rest_curcalyr, nam.federal_land]]
            temp = temp.loc[(temp[nam.federal_land] == 0)]
            temp = temp.drop([nam.federal_land], axis=1)
            temp = temp.groupby(nam.region_number).sum()
            temp = temp[self.rest_curcalyr].add(temp_nofed[self.rest_curcalyr], fill_value=0).to_frame()
            update_values = temp[[self.rest_curcalyr]].stack() / 1000000000
            update_values = _cast_restart_series(self.restart.ogsmout_ogngprd_nonfed, update_values)
            self.restart.ogsmout_ogngprd_nonfed.loc[update_values.index, 'value'] = update_values.values


            ###Regional crude oil and natural gas production by type of production
            #Crude Oil (1 = Primary Crude Oil, 2 = Tertiary Crude Oil aka EOR)
            for region in list(range(1,8)):
                self.restart.ogsmout_ogregprd.at[(region, 1, self.rest_curcalyr), nam.value] = _cast_restart_scalar(
                    self.restart.ogsmout_ogregprd,
                    self.restart.pmmout_rfqdcrd.at[(region, self.rest_curcalyr), 'value']
                )
                self.restart.ogsmout_ogregprd.at[(region, 2, self.rest_curcalyr), nam.value] = _cast_restart_scalar(
                    self.restart.ogsmout_ogregprd,
                    self.restart.pmmout_rfqtdcrd.at[(region, self.rest_curcalyr), 'value']
                    - self.restart.pmmout_rfqdcrd.at[(region, self.rest_curcalyr), 'value']
                )


            ###Apply adjusted realized natural gas volumes to ogsmout_ogregprd (4 = Conventional gas, 5 = Tight Gas, 6 = Shale Gas, 7 = CBM)
            if self.rest_curcalyr <= self.parent.steo_years[0]: #Get initial years as raw data for scaling to STEO, other year calculation against realized prod
                temp = self.natgas_production[[nam.well_type_number, nam.region_number, self.rest_curcalyr]]

                conv_temp = temp[temp[nam.well_type_number].isin([1, 3])]
                conv_temp = conv_temp.groupby(nam.region_number).sum()
                conv_temp[nam.gas_type] = 4
                conv_temp = conv_temp.set_index([nam.gas_type], append=True)
                update_values = conv_temp[[self.rest_curcalyr]].stack() / 1000000000
                update_values = _cast_restart_series(self.restart.ogsmout_ogregprd, update_values)
                self.restart.ogsmout_ogregprd.loc[update_values.index, 'value'] = update_values.values

                tight_temp = temp[temp[nam.well_type_number].isin([4])]
                tight_temp = tight_temp.groupby(nam.region_number).sum()
                tight_temp[nam.gas_type] = 6  # Recategorize well type 4 (tight gas) to shale gas (index 6)
                tight_temp = tight_temp.set_index([nam.gas_type], append=True)
                update_values = tight_temp[[self.rest_curcalyr]].stack() / 1000000000
                update_values = _cast_restart_series(self.restart.ogsmout_ogregprd, update_values)
                self.restart.ogsmout_ogregprd.loc[update_values.index, 'value'] = update_values.values

                shale_temp = temp[temp[nam.well_type_number].isin([2, 5])]
                shale_temp = shale_temp.groupby(nam.region_number).sum()
                shale_temp[nam.gas_type] = 6
                shale_temp = shale_temp.set_index([nam.gas_type], append=True)
                update_values = shale_temp[[self.rest_curcalyr]].stack() / 1000000000
                update_values = _cast_restart_series(self.restart.ogsmout_ogregprd, update_values)
                self.restart.ogsmout_ogregprd.loc[update_values.index, 'value'] = update_values.values

                cbm_temp = temp[temp[nam.well_type_number].isin([6])]
                cbm_temp = cbm_temp.groupby(nam.region_number).sum()
                cbm_temp[nam.gas_type] = 7
                cbm_temp = cbm_temp.set_index([nam.gas_type], append=True)
                update_values = cbm_temp[[self.rest_curcalyr]].stack() / 1000000000
                update_values = _cast_restart_series(self.restart.ogsmout_ogregprd, update_values)
                self.restart.ogsmout_ogregprd.loc[update_values.index, 'value'] = update_values.values

            # Zero out tight gas (index 5) for all projection years after routing well type 4 to shale gas
            # This ensures no tight gas values persist in projection years
            if self.rest_curcalyr >= self.parent.history_year + 1:
                projection_year_mask = self.restart.ogsmout_ogregprd.index.get_level_values(2) == self.rest_curcalyr
                tight_gas_mask = self.restart.ogsmout_ogregprd.index.get_level_values(1) == 5
                tight_gas_projection_mask = projection_year_mask & tight_gas_mask
                self.restart.ogsmout_ogregprd.loc[tight_gas_projection_mask, 'value'] = 0

            ###Oil wellhead prices by region
            #Get onshore regions (1-7) that have production
            onshore_regions = [int(r) for r in self.crude_production[nam.region_number].unique() if 1 <= int(r) <= 7]
            
            #Update ogsmout_ogcowhp for onshore regions using vectorized operation
            if onshore_regions:
                #Extract prices for onshore regions and create Series with proper MultiIndex
                prices = self.parent.reg_crude_price.loc[onshore_regions, self.rest_curcalyr]
                #Create MultiIndex tuples for (region, year) pairs
                price_index = [(region, self.rest_curcalyr) for region in onshore_regions]
                #Update restart variable using .loc with list of tuples
                self.restart.ogsmout_ogcowhp.loc[price_index, nam.value] = prices.values

            ###Weighted Averge Crude Oil Price
            # Regions 1-10 hold the active Lower 48 (incl. offshore) areas; slice both price and production tables once.
            region_slice = pd.IndexSlice[range(1, 11), self.rest_curcalyr]
            regional_prices = self.restart.ogsmout_ogcowhp.loc(axis=0)[region_slice][nam.value]
            regional_production = self.restart.ogsmout_ogcoprd.loc(axis=0)[region_slice][nam.value]

            # Derive weights strictly from regions 1-10 so we never depend on pre-computed totals.
            total_production = regional_production.sum()
            weighted_price = (regional_prices * (regional_production / total_production)).sum() if total_production else 0

            # Mirror the weighted price into both the synthetic region 11 slot and the report output. These are both reporting variables
            self.restart.ogsmout_ogcowhp.at[(11, self.rest_curcalyr), nam.value] = _cast_restart_scalar(
                self.restart.ogsmout_ogcowhp,
                weighted_price
            )
            self.restart.ogsmout_ogpcrwhp.at[self.rest_curcalyr, nam.value] = _cast_restart_scalar(
                self.restart.ogsmout_ogpcrwhp,
                weighted_price
            )


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
                temp = _cast_restart_series(self.restart.ogsmout_ogogwells, temp)
                self.restart.ogsmout_ogogwells.loc[temp.index, 'value'] = temp.values

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
                # Replace zeros with NaN to avoid division by zero
                temp_total = temp_total.replace(0, np.nan)
                temp_sr = temp_wells.div(temp_total, axis = 1)

                #Apply total wells to restart variables (total wells/dryholes and success rates)
                update_values = temp_total.stack()
                update_values = _cast_restart_series(self.restart.ogsmout_ogwellsl48, update_values)
                self.restart.ogsmout_ogwellsl48.loc[update_values.index, 'value'] = update_values.values
                update_values = temp_sr.stack()
                update_values = _cast_restart_series(self.restart.ogsmout_ogsrl48, update_values)
                self.restart.ogsmout_ogsrl48.loc[update_values.index, 'value'] = update_values.values
            else:
                pass


            ###Report NGPL Results
            #NGPL Total
            temp = self.ngpl_production[[nam.district_number, self.rest_curcalyr]].copy()
            temp[nam.district_number] = temp[nam.district_number].astype(int)
            temp = temp.groupby([nam.district_number]).sum()
            temp = temp.stack()
            temp = _cast_restart_series(self.restart.ogsmout_ogngplprd, temp)
            self.restart.ogsmout_ogngplprd.loc[temp.index, 'value'] = temp.values

            #Ethane
            temp = self.ngpl_ethane_production[[nam.district_number, self.rest_curcalyr]].copy()
            temp[nam.district_number] = temp[nam.district_number].astype(int)
            temp = temp.groupby([nam.district_number]).sum()
            temp = temp.stack()
            temp = _cast_restart_series(self.restart.ogsmout_ogngplet, temp)
            self.restart.ogsmout_ogngplet.loc[temp.index, 'value'] = temp.values

            #Propane
            temp = self.ngpl_propane_production[[nam.district_number, self.rest_curcalyr]].copy()
            temp[nam.district_number] = temp[nam.district_number].astype(int)
            temp = temp.groupby([nam.district_number]).sum()
            temp = temp.stack()
            temp = _cast_restart_series(self.restart.ogsmout_ogngplpr, temp)
            self.restart.ogsmout_ogngplpr.loc[temp.index, 'value'] = temp.values

            #Butane
            temp = self.ngpl_butane_production[[nam.district_number, self.rest_curcalyr]].copy()
            temp[nam.district_number] = temp[nam.district_number].astype(int)
            temp = temp.groupby([nam.district_number]).sum()
            temp = temp.stack()
            temp = _cast_restart_series(self.restart.ogsmout_ogngplbu, temp)
            self.restart.ogsmout_ogngplbu.loc[temp.index, 'value'] = temp.values

            #Isobutane
            temp = self.ngpl_isobutane_production[[nam.district_number, self.rest_curcalyr]].copy()
            temp[nam.district_number] = temp[nam.district_number].astype(int)
            temp = temp.groupby([nam.district_number]).sum()
            temp = temp.stack()
            temp = _cast_restart_series(self.restart.ogsmout_ogngplis, temp)
            self.restart.ogsmout_ogngplis.loc[temp.index, 'value'] = temp.values

            #Pentanes
            temp = self.ngpl_proplus_production[[nam.district_number, self.rest_curcalyr]].copy()
            temp[nam.district_number] = temp[nam.district_number].astype(int)
            temp = temp.groupby([nam.district_number]).sum()
            temp = temp.stack()
            temp = _cast_restart_series(self.restart.ogsmout_ogngplpp, temp)
            self.restart.ogsmout_ogngplpp.loc[temp.index, 'value'] = temp.values


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
            new_co2_demand_df = _ensure_dtype_compatibility(self.restart.ccatsdat_dem_eor, new_co2_demand_df)
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
            # Create mask for zero denominators
            demand_mask = new_co2_demand_df[nam.value] == 0
            # Replace zeros with 1 to avoid division error
            demand_safe = new_co2_demand_df[nam.value].replace(0, 1)
            # Perform division
            new_co2_net_cost_df[nam.value] = new_co2_net_cost_df[nam.value] / demand_safe
            # Set result to 0 where demand was originally zero
            new_co2_net_cost_df.loc[demand_mask, nam.value] = 0.0

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
            new_co2_net_cost_df = _ensure_dtype_compatibility(self.restart.ccatsdat_cst_eor, new_co2_net_cost_df)
            new_co2_net_cost_df[nam.value] = new_co2_net_cost_df[nam.value].astype(
                self.restart.ccatsdat_cst_eor[nam.value].dtype
            )
            self.restart.ccatsdat_cst_eor.update(new_co2_net_cost_df)
            
            # Write to pickle
            # Write to parent HSM variables
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
            temp = _ensure_dtype_compatibility(self.restart.ogsmout_ogtechon, temp)
            self.restart.ogsmout_ogtechon.update(temp)


        else:
            pass

        pass